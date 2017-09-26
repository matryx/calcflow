;; Author: Barton Willis with help from Richard Fateman

#|
To simplify a sum with n terms, the standard simplus function calls
great O(n^2) times. By using sorting more effectively, this code
reduces the calls to great to O(n log_2(n)).

Also, this code tries to be "infinity correct" for addition. By this I
mean inf + inf --> inf, inf + number --> inf, and inf + minf --> und.
Since -1 * inf doesn't simplify to minf, this code doesn't simplify
inf - inf to und; consequently, this portion of the code is largely
untested. There are other problems too.  For one, this code does
f(inf) - f(inf) --> 0 (comment from Stavros Macrakis). I don't know
how far we can go with such things without making a mess. You could
argue that Maxima should do

   f(x) - f(x) --> if finitep(f(x)) then 0 else und 

instead of f(x) - f(x) --> 0.

There is a great deal more that could be done. We could tag each
infinity with a unique identifier (idea from RJF). That way we could
have x : inf, x - x --> 0 and x : inf, y : inf, x - y --> und, for
example.

In short, this code is highly experimental; you should not use it for
anything that is important. I place it in /share/contrib because
others encouraged me too; also I thought it might be useful to others
who would like to experiment with similar code. Since a great deal of
work has gone into the current simplus code, I'm not sure that a total
re-write is the best route.

Maybe the special case dispatch part of this code makes the task of
extending simplus (to intervals, for example) easier. The basic design
of this code is due to Barton Willis.
|#

#| Fixed bugs:

(1) ceiling(asin(-107) -42) <--- bug! Gets stuck. I think -1.0 * (complex) should 
    expand, but it doesn't. I fixed this by changing the condition for a "do-over" from 
   (and (or (equalp cf 1) (equalp cf -1)) (mplusp x) ...)  to (and (or (eq cf 1) (eq cf -1)) (mplusp x) ...) 

(2) rat(x) + taylor(x^42,x,0,1) --> error. Fixed by adding taylor terms separately from mrat terms.

Maxima 5.17.0 bugs: I think the rtest16 bugs 74 and 121 are related to the fact that
-1 * inf doesn't simplify to minf. Fixing this requires a new simptimes, I think.

 Errors found in rtest15.mac, problems: (189 222) <-- correct, but differ from expected
 Errors found in rtest16.mac, problems: (74 121)  <-- wrong and do asksign
 Error found in rtestsum.mac, problem: (226)  <-- not wrong but differs from expected
 Errors found in rtest_expintegral.mac, problems: (133 134) <-- small differences in big float

Unfixed:

 (1) sqrt(3) + sqrt(3) + sqrt(3) --> 3^(3/2), but altsimp does 3 * sqrt(3). I'm not so sure
     we want sqrt(3) + sqrt(3) + sqrt(3) --> 3^(3/2).

|#

(in-package :maxima)
(declaim (optimize (speed 3)(safety 0)))

(define-modify-macro mincf (&optional (i 1)) addk)

(defmacro opcons (op &rest args)
  `(simplify (list (list ,op) ,@args)))

(defmacro opapply (op args)
  `(simplify (cons (list ,op) ,args)))

(defun mzerop (z)
  (and (mnump z)
       (or (and (numberp z)(= z 0))
	   (and (bigfloatp z)(= (cadr z) 0))))) ;bigfloat zeros may be diff precisions

(defun convert-to-coeff-form (x)  
  (let ((c))
    (cond ((mnump x) (cons 1 x))
	  ((mtimesp x) 
	   (pop x)  ;remove (car x) which is (mtimes ..)
	   (cond ((mnump (setf c (car x))) ;set c to numeric coeff.
		  (pop x) ; remove numeric coeff.
		  (if (null (cdr x));; if only one more item, that's it.
		      (cons  (car x) c)
		    (cons  `((mtimes simp) ,@x) c)))
		 (t (cons  `((mtimes simp) ,@x) 1))))
	  (t (cons x 1)))))

;; The expression e must be simplified (ok)
;;   (a) 1 * x --> x,
;;   (b) 0 * x --> 0, 0.0 * x --> 0.0, 0.0b0 * x --> 0.0b0
;;   (c) cf * e --> timesk(ck,e) when e is a maxima number,
;;   (d) -1 * (a + b) --> -a - b,
;;   (e) cf * (* a b c) --> (* (* cf a) b c ...) when a is a number; otherwise (* cf a b ...)
;;   (f) (* cf e) (default)

(defun number-times-expr (cf e)
  (cond ((eq cf 1) e)
	((mzerop cf) cf)
	((mnump e) (timesk cf e)) ; didn't think this should happen
	((and (onep1 (neg cf)) (mplusp e))
	 (opapply 'mplus (mapcar 'neg (cdr e))))
	((mtimesp e) 
	 (if (mnump (cadr e))
	     `((mtimes simp) ,@(cons (timesk cf (cadr e)) (cddr e)))
	   `((mtimes simp) ,@(cons cf (cdr e)))))
	(t  `((mtimes simp) ,cf ,e))))

;; Add an expression x to a list of equalities l.

(defun add-expr-mequal (x l)
  (setq l (mapcar 'cdr l))
  (push (list x x) l)
  (setq l (list (reduce 'add (mapcar 'first l)) (reduce 'add (mapcar 'second l))))
  (simplifya (cons '(mequal) l) t))
  
(defun add-expr-mrat (x l)
  (ratf (cons '(mplus) (cons (ratf x) l))))

(defun add-expr-taylor (x l)
  ($taylor (cons '(mplus) (cons x l))))

(defun add-expr-mlist (x l)
  (setq l (if (cdr l) (reduce 'addmx l) (car l)))
  (opapply 'mlist (mapcar #'(lambda (s) (add x s)) (cdr l))))

;; Simple demo showing how to define addition for a new object.
;; We could append simplification rules for intervals:

;;  (a) interval(a,a) --> a,
;;  (b) if p > q then interval(p,q) --> standardized empty interval?

(defun add-expr-interval (x l)
  (setq l (mapcar #'(lambda (s) `((mlist) ,@(cdr s))) l))
  (setq l (if (cdr l) (reduce 'addmx l) (car l)))
  (opapply '$interval (mapcar #'(lambda (s) (add x s)) (cdr l))))

;; Add an expression x to a list of matrices l.

(defun add-expr-matrix (x l)
  (mxplusc x (if (cdr l) (reduce 'addmx l) (car l))))

;; Return a + b, where a, b in {minf, inf, ind, und, infinity}. I should
;; extend this to allow zeroa and zerob (but I'm not sure zeroa and zerob
;; are supposed to be allowed outside the limit code).

(defun add-extended-real (a b)
  (cond ((eq a '$minf) 
	 (cond ((memq b '($minf $ind)) '$minf)
	       ((memq b '($und $inf)) '$und)
	       ((eq b '$infinity) '$infinity)))
	((eq a '$ind)
	 (cond ((eq b '$minf) '$minf)
	       ((eq b '$ind) '$ind)
	       ((eq b '$und) '$und)
	       ((eq b '$inf) '$inf)
	       ((eq b '$infinity) '$infinity)))
	((eq a '$und) '$und)
	((eq a '$inf)
	 (cond ((memq b '($minf $und)) '$und)
	       ((memq b '($inf $ind)) '$inf)
	       ((eq b '$infinity) '$infinity)))
	((eq a '$infinity) (if (eq b '$und) '$und '$infinity))))

;; Add an expression x to a list of infinities.

(defun add-expr-infinities (x l) 
  (setq l (if l (reduce 'add-extended-real l) (car l)))
  (if (mnump x) l `((mplus simp) ,x ,l)))
	
;; I assumed that if a list of distinct members is sorted using great,
;; then it's still sorted after multiplying each list member by a nonzero
;; maxima number. I'm not sure this is true.

;; If l has n summands, simplus calls great O(n log_2(n)) times. All
;; other spendy functions are called O(n) times. The standard simplus
;; function calls great O(n^2) times, I think.

;(defvar *calls-to-simplus* 0)
;(defvar *simplus-length* 0)
;(defvar *its-an-atom* 0)
;(defvar *not-an-atom* 0)


(defun simplus (l w z)
  (declare (ignore w))
  ;;(incf *calls-to-simplus*)
  ;;(if (> 8 (length l)) (incf *simplus-length*))
  (let ((acc nil) (cf) (x) (num-sum 0) (do-over nil) (mequal-terms nil) (mrat-terms nil) 
	(inf-terms nil) (matrix-terms nil) (mlist-terms nil) (taylor-terms nil) (interval-terms nil) (op)
	(atom-hash (make-hash-table :test #'eq :size 8)))

    (setq l (margs l))

    ;; simplfy and flatten
    (let (($%enumer $numer)) ;; convert %e --> 2.718...Why not %pi too? See simpcheck in simp.lisp.
      (dolist (li l)
	(setq li (simplifya li z))
	(if (mplusp li) (setq acc (append acc (cdr li))) (push li acc))))
    (setq l acc)
    (setq acc nil)
    (dolist (li l)
      ;;(if (atom li) (incf *its-an-atom*) (incf *not-an-atom*))
      (cond ((mnump li) (mincf num-sum li))
	    ;; factor out infrequent cases.
	    ((and (consp li) (consp (car li)) (memq (caar li) '(mequal mrat $matrix mlist $interval)))
	     (setq op (caar li))
	     (cond ((eq op 'mequal)
		    (push li mequal-terms))
		   (($taylorp li)
		    (push li taylor-terms))
		   ((eq op 'mrat)
		    (push li mrat-terms))
		   ((eq op '$matrix)
		    (push li matrix-terms))
		   ((eq op '$interval)
		    (push li interval-terms))
		   ((eq op 'mlist)
		    (if $listarith (push li mlist-terms) (push (convert-to-coeff-form li) acc)))))

	    ;; Put non-infinite atoms into a hashtable; push infinite atoms into inf-terms.
	    ((atom li)
	     (if (memq li '($minf $inf $infinity $und $ind))
		 (push li inf-terms)
	       (progn
		 (setq cf (gethash li atom-hash))
		 (setf (gethash li atom-hash) (if cf (1+ cf) 1)))))

	    (t (push (convert-to-coeff-form li) acc))))

     ;; push atoms in the hashtable into the accumulator acc; sort acc.
    (maphash #'(lambda (cf a) (push (cons cf a) acc)) atom-hash)
    (setq l (sort acc 'great :key 'car))
 
    ;; common term crunch: when the new coefficient is -1 or 1 (for example, 5*a - 4*a),
    ;; set the "do-over" flag to true. In this case, the sum needs to be re-simplified.
    ;; Without the do over flag, a + 5*a - 4*a --> a + a. Last I checked, the testsuite
    ;; does not test the do-over scheme.

    (setq acc nil)
    (while l
      (setq x (pop l))
      (setq cf (cdr x))
      (setq x (car x))
      (while (and l (like x (caar l)))
	(mincf cf (cdr (pop l))))
      (if (and (or (eq cf 1) (eq cf -1)) (mplusp x)) (setq do-over t))
      (setq x (number-times-expr cf x))
      (cond ((mnump x) (mincf num-sum x))
	    ((not (mzerop x)) (push x acc))))

    ;;(setq acc (sort acc '$orderlessp))   ;;<-- not sure this is needed.

    ;; I think we want x + 0.0 --> x + 0.0, not x + 0.0 --> x.
    ;; If float and bfloat were simplifying functions we could do 
    ;; x + 0.0 --> float(x) and 0.0b0 + x --> bfloat(x). Changing this
    ;; test from mzerop to (eq 0 num-sum) causes problems with the test suite.
    ;; For example, if x + 0.0 --> x + 0.0, we get an asksign for 
    ;; tlimit((x*atan(x))/(1+x),x,inf). That's due to the (bogus) floating point
    ;; calculations done by the limit code.
  
    ;;(if (not (eq 0 num-sum)) (push num-sum acc))
    (if (not (mzerop num-sum)) (push num-sum acc))
   
    ;;(if do-over (incf *do-over*)) ;; never happens for testsuite!
    (setq acc
	  (cond (do-over (simplifya `((mplus) ,@acc) nil))
		((null acc) num-sum)
		((null (cdr acc)) (car acc))
		(t (cons '(mplus simp) acc))))
    
    ;; special case dispatch
    (if mequal-terms
	(setq acc (add-expr-mequal acc mequal-terms)))
    (if taylor-terms
	(setq acc (add-expr-taylor acc taylor-terms)))
    (if mrat-terms
	(setq acc (add-expr-mrat acc mrat-terms)))
    (if mlist-terms
	(setq acc (add-expr-mlist acc mlist-terms)))
    (if interval-terms
	(setq acc (add-expr-interval acc interval-terms)))
    (if matrix-terms
	(setq acc (add-expr-matrix acc matrix-terms)))
    (if inf-terms
	(setq acc (add-expr-infinities acc inf-terms)))   
 
    acc))


