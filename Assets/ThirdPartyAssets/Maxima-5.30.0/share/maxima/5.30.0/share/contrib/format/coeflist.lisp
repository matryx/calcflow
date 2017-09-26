;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIMAX; Base: 10 -*-
;;;>******************************************************************************************
;;;>   Software developed by Bruce R. Miller
;;;>   using Symbolics Common Lisp (system 425.111, ivory revision 4)
;;;>   at NIST - Computing and Applied Mathematics Laboratory
;;;>   a part of the U.S. Government; it is therefore not subject to copyright.
;;;>******************************************************************************************

;(in-package 'climax)

;;; To run in Schelter's Maxima comment the above and uncomment these:
(in-package :maxima)
(defmacro mexp-lookup (item alist) `(assolike ,item ,alist))
(defmacro mlist* (arg1 &rest more-args) `(list* '(mlist simp) ,arg1 ,@more-args))

;;;;******************************************************************************************
;;;; Needed, but unrelated, stuff.  Possibly useful in its own right
;;;;******************************************************************************************
;;; Returns an mlist of all subexpressions of expr which `match' predicate.
;;; Predicate(expr,args,...) returns non-nil if the expression matches.
;;; [eg. a function constructed by $defmatch]
(defun $matching_parts (expr predicate &rest args)
  (let ((matches nil))
    (labels ((srch (expr)
	       (when (specrepp expr)
		 (setq expr (specdisrep expr)))
	       (when (apply #'mfuncall predicate expr args)
		 (pushnew expr matches :test #'alike1))
	       (unless (atom expr)
		 (mapc #'srch (cdr expr)))))
      (srch expr)
      (mlist* matches))))

;;; Return an  mlist of all unique function calls of the function(s) FCNS in EXPR.
;;; (FCNS can also be a single function)
(defun $function_calls (expr &rest functions)
  ;; Coerce fcns to a list of function names.
  (let ((fcns (mapcar #'(lambda (x)(or (get (setq x (getopr x)) 'verb) x)) functions)))
    ($matching_parts expr #'(lambda (p)(and (listp p)(member (caar p) fcns))))))

;;; Return an mlist of all unique arguments used by FCNS in EXPR.
(defun $function_arguments (expr &rest functions)
  (mlist* (remove-duplicates (cdr (map1 #'$args (apply #'$function_calls expr functions)))
			     :test #'alike1)))

;;; $totaldisrep only `disrep's CRE (mrat), but not POIS!
(defun totalspecdisrep (expr)
  (cond ((atom expr) expr)
	((not (or (among 'mrat expr)(among 'mpois expr))) expr)
	((eq (caar expr) 'mrat)(ratdisrep expr))
	((eq (caar expr) 'mpois) ($outofpois expr))
	(t (cons (remove 'ratsimp (car expr))(mapcar 'totalspecdisrep (cdr expr))))))

;;;;******************************************************************************************
;;;; Variable Lists
;;; A variable list consists of a list of variables, simple expressions and specs like
;;;   OPERATOR(fcn) or OPERATOR(fcn,...) represents ALL calls to fcn in the expression.
;;;   MATCH(fcn,arg..) represents subexpressions of expression which pass FCN(subexpr,args..)
;;; Instanciating the variable list involves replacing those special cases with those
;;; subexpressions of the relevant expression which pass the test.
;;;;******************************************************************************************

(defun instanciate-variable-list (vars expr caller &optional max-vars)
  (let ((ivars (mapcan #'(lambda (var)
			   (setq var (totalspecdisrep var))
			   (case (and (listp var)(caar var))
			     ($operator (cdr (apply #'$function_calls expr (cdr var))))
			     ($match    (cdr (apply #'$matching_parts expr (cdr var))))
			     (t (list var))))
		       vars)))
    (when (and max-vars (> (length ivars) max-vars))
      (merror "Too many variables for ~M: ~M" caller (mlist* ivars)))
    ivars))

;;;;******************************************************************************************
;;;; Helpers
;;;;******************************************************************************************

;;; Similar to lisp:reduce with the :key keyword.
;;; Apparently, the Lisp underneath the Sun version doesn't support it. Ugh.
;  (defmacro cl-reduce (function list key) `(lisp:reduce ,function ,list :key ,key))

(defun cl-reduce (function list key)
  (if (null list) nil
      (let ((result (funcall key (car list))))
	(dolist (item (cdr list))
	  (setq result (funcall function result (funcall key item))))
	result)))

(defun map-mlist (list) (mapcar #'(lambda (e)(mlist* e)) list))

;;;******************************************************************************************
;;; Coefficient List = Pseudo-polynomial : as list of ( (coefficient power(s) ...) ...)
;;;     coefficient: the coefficient of the monomial (anything algebraic thing)
;;;     power(s) : the power(s) of the variable(s) in the monomial (any algebraic thing)
;;; Pairs are sorted into increasing order of the power(s).   
;;; 0 is represented by NIL.
;;;******************************************************************************************
;;; NOTE on ordering of terms.  The Macsyma predicate GREAT (& friends lessthan, etc)
;;; define a total ordering, but if non-numeric elements are allowed, the ordering is not
;;; robust under addition, eg L=[1,2,m] is in order, but L+m=[m,m+2,2*m] is not.
;;; We define the ordering of A & B by determining the `sign' of A-B, where the sign is
;;; the sign of the coefficient of the leading (highest degree) term.  We can use SIGNUM1 
;;; for this. 
;;;******************************************************************************************

;;;******************************************************************************************
;;; CLIST Arithmetic

;;; Add two CLISTs
(defun clist-add (l1 l2)
  (do ((result nil))
      ((not (and l1 l2)) (if (or l1 l2)(nconc (nreverse result)(or l1 l2)) (nreverse result)))
    (do ((p1 (cdar l1) (cdr p1))
	 (p2 (cdar l2) (cdr p2)))
	((or (null p1) (not (alike1 (car p1)(car p2))))
	 (if p1
	     (push (if (plusp (signum1 (sub (car p1)(car p2))))(pop l2)(pop l1)) result)
	     (let ((c3 (add (caar l1)(caar l2))))	;If power is same, combine
	       (unless (zerop1 c3)		;And accumulate result, unless zero.
		 (push (cons c3 (cdar l1)) result))
	       (pop l1)(pop l2)))))))

;;; Multiply two CLISTs
;;; Optional ORDER is for use by series arithmetic (single variable): truncates powers>order
(defun clist-mul (l1 l2 &optional order)
  (when (and l1 l2)
    (when (> (length l1)(length l2))		; make l1 be shortest
      (psetq l1 l2 l2 l1))
    (let ((rl2 (reverse l2)))
      (flet ((mul1 (pair1)
	       (let ((c1 (car pair1)) (p1 (cdr pair1)) result)
		 (dolist (i2 rl2)
		   (let ((p (mapcar #'add p1 (cdr i2))))
		     (unless (and order (great (car p) order))
		       (push (cons (mul c1 (car i2)) p) result))))
		 result)))
 	(cl-reduce #'clist-add l1 #'mul1)))))

;;; Take the Nth power of a CLIST, using "binary expansion of the exponent" method.
;;; Built-in code to handle P^2, instead of P*P.
(defun clist-pow (l n)				; Assumes n>0
  (cond ((null l) nil)				; l=0 -> 0 (nil)
	((null (cdr l))				; single term, trivial
	 `((,(power (caar l) n) ,@(mapcar #'(lambda (p)($expand (mul p n)))(cdar l)))))
	(t (let ((l^i l) (l^n (if (logtest n 1) l)))
	     (do ((bits (ash n -1)(ash bits -1)))
		 ((zerop bits) l^n)
	       (do ((sq nil)			; Square l^i
		    (ll (reverse l^i) (cdr ll)))
		   ((null ll) (setq l^i sq))
		 (let* ((c1 (caar ll)) (2c1 (mul 2 c1))(p1 (cdar ll))
			(psq (list (cons (power c1 2)(mapcar #'add p1 p1)))))
		   (dolist (lll (cdr ll))
		     (push (cons (mul 2c1 (car lll))(mapcar #'add p1 (cdr lll))) psq))
		   (setq sq (if sq (clist-add sq psq) psq))))
	       (if (logtest bits 1) (setq l^n (if l^n (clist-mul l^n l^i) l^i))))))))

;;; An MBAG includes lists, arrays and equations.
;;; Given the list of {list|array|equation} elements which have been converted to CLIST's, 
;;; this function combines them into a single clist whose coefficients 
;;; are {list|array|equation}s

(defun clist-mbag (op clists)
  (let ((z (if (eq op '$matrix)			; the `zero' of a matrix is an mlist of 0's!!!
	       (mlist* (make-list (length (cdaaar clists)) :initial-element 0))
	       0)))
    (flet ((keylessp (l1 l2)			; does key l1 precede l2?
	     (do ((l1 l1 (cdr l1))
		  (l2 l2 (cdr l2)))
		 ((or (null l1)(not (alike1 (car l1)(car l2))))
		  (and l1 (minusp (signum1 (sub (car l1)(car l2)))))))))
      (mapcar #'(lambda (p)
		  `(((,op)
		     ,@(mapcar #'(lambda (l)(or (car (rassoc p l :test #'alike)) z)) clists))
		    ,@p))
	      (sort (cl-reduce #'union1 clists #'(lambda (e)(mapcar #'cdr e))) #'keylessp)))))

;;;;******************************************************************************************
;;;; Transform an expression into its polynomial coefficient list form.

(defun $coeffs (expr &rest vars)
  (setq expr (totalspecdisrep expr))
  (let* ((vs (instanciate-variable-list vars expr '$coeffs))
	 (zeros (make-list (length vs) :initial-element 0))
	 (cache nil))
    (dolist (v vs)				; preload the cache w/ encoded variables
      (let ((u (copy-list zeros)))
	(setf (nth (position v vs) u) 1)
	(push (cons v (list (cons 1 u))) cache)))
    (labels ((gcf (expr)			; Get coefficients.
	       (or (mexp-lookup expr cache)	; reuse cached value
		   (cdar (push (cons expr (gcf1 expr)) cache))))	; or compute & store
	     (gcf1 (expr)
	       (let ((op (and (listp expr)(caar expr))) x y)
		 (cond ((mbagp expr)    (clist-mbag op (mapcar #'gcf (cdr expr))))
		       ((or (null op)(not (dependsall expr vs))) `((,expr . ,zeros)))
		       ((eq op 'mplus)  (cl-reduce #'clist-add (cdr expr) #'gcf))
		       ((eq op 'mtimes) (cl-reduce #'clist-mul (cdr expr) #'gcf))
		       ((and (eq op 'mexpt)	; Check that we can actually compute X^Y:
			     (setq x (gcf (second expr)) y (third expr))
			     (or (and (integerp y)(plusp y))	; Either integer y > 0
				 (and (null (cdr x))	; or x is a single monomial
				      (not (dependsall y vs))	; w/ y must be free of vars
				      (or (eql $radexpand '$all) ; & dont care about cuts
					  (integerp y)	; or y is an integer
					  (every #'(lambda (p)(or (zerop1 p)(onep p)))
						 (cdar x))))))	; or x is linear in vars.
			(clist-pow x y))	; OK, so we CAN compute x^y (whew).
		       (t `((,expr . ,zeros)))))))
      (mlist* (mlist* '$%poly vs)(map-mlist (gcf expr))))))

; Inverse of above: make an expression out of clist.
;;; Actually works for SERIES & Taylor too.
(defun unclist (clist vars)
  (addn (mapcar #'(lambda (e)(mul (cadr e)(muln (mapcar #'power vars (cddr e)) t))) clist) t))

;;;********************************************************************************
;;; TRIG SERIES
;;; Given an expression and a list of variables, v_i, construct the list of sine & cosine
;;; coefficients & multiples of the variables in the expression:
;;;  [[%trig, v_1, ...] sine_list, cosine_list]
;;;     sine_list: [[c,m_1,...],[c',m_1',...]....]
;;;     cosine_list: " "
;;; This version carries out `trig arithmetic' on coefficient lists (does NOT use the 
;;; enhanced poisson package (pois2m)
;;;********************************************************************************

;;;;******************************************************************************************
;;;; TLIST Arithmetic.

;;; Is L1 < (0 0 0 ...)? ie. is first non-zero element negative?
(defun list-negp (l1)
  (dolist (i1 l1)
    (unless (zerop1 i1)
      (return-from list-negp (eq -1 (signum1 i1))))))

(defun tlist-add (l1 l2) (mapcar #'clist-add l1 l2))

; multiply a cos or sin list by another.  c1p is T if L1 is cosine list, c2p ditto.
(defun tlist-mul1 (l1 l2 c1p c2p)
  (when (> (length l2)(length l1))		; Swap so that L2 is the shortest.
    (psetq l1 l2 l2 l1)
    (psetq c1p c2p c2p c1p))
  (when l2
    (let ((s1 (if (and c1p (not c2p)) -1 +1))	; cos * sines -> -1, else +1
	  (s2 (if (or c1p c2p) +1 -1))		; either are cosines -> +1 else -1
	  (s3 (if (xor c1p c2p) -1 +1))		; result is sines -> -1 else +1
	  (rl1 (reverse l1)))
      (flet ((mul1 (pr2)
	       (let ((c2 (car pr2))(m2 (cdr pr2)))
		 (if (every #'zerop1 m2)
		     (when c2p
		       (mapcar #'(lambda (pr) (cons (mul c2 (car pr))(cdr pr))) l1))
		     (let ((t1 nil)(t2 nil)(t3 nil))
		       (dolist (i1 rl1)
			 (let* ((c1 (car i1))(m1 (cdr i1))
				(cc (div (mul c1 c2) 2)))
			   (push (cons (mul s1 cc)(mapcar #'sub m1 m2)) t1)
			   (push (cons (mul s2 cc)(mapcar #'add m1 m2)) t2)))
		       (do () ((not (and t1 (list-negp (cdar t1)))))
			 (push (cons (mul s3 (caar t1))(mapcar #'neg (cdar t1))) t3)
			 (pop t1))
		       (when (and (minusp s3)(every #'zerop1 (cdar t1)))	; sin(0) ?
			 (pop t1))		; remove it.
		       (clist-add (clist-add t1 t3) t2))))))
	(cl-reduce #'clist-add l2 #'mul1)))))

(defun tlist-mul (l1 l2)
  (let ((sin1 (first l1))(cos1 (second l1))
	(sin2 (first l2))(cos2 (second l2)))
    (list (clist-add (tlist-mul1 cos1 sin2 t nil)
		     (tlist-mul1 sin1 cos2 nil t))
	  (clist-add (tlist-mul1 cos1 cos2 t t)
		     (tlist-mul1 sin1 sin2 nil nil)))))

(defun tlist-pow (l n zeros)
  (let ((sin (first l))(cos (second l)))
    (flet ((pow1 (coef m sinp)			; single {cos or sin}^n: use explicit formula
	     (let* ((s (if sinp -1 +1))		; by this point, n better be a fixnum!
		    (c (mul (if (and sinp (oddp (floor n 2))) -1 +1)
			    (div (power coef n) (power 2 (1- n)))))
		    (pow (do ((result nil)
			      (k 0)(kk n (- kk 2)))
			     ((not (plusp kk)) result)
			   (push (cons c (mapcar #'(lambda (mm)(mul kk mm)) m)) result)
			   (setq c (mul c (div (* s (- n k))(setq k (+ k 1))))))))
	       (cond ((evenp n)(list nil (cons (cons (div c 2) zeros) pow)))
		     (sinp (list pow nil))
		     (t (list nil pow))))))
      (cond ((and (null cos)(null sin))       (list nil nil))	; 0^n
	    ((zerop1 n) `( nil ((1 ,@ zeros))))
	    ((and (null (cdr cos))(null sin)) (pow1 (caar cos)(cdar cos) nil))	; cos^n
	    ((and (null (cdr sin))(null cos)) (pow1 (caar sin)(cdar sin) t))	; sin^n
	    (t (let (l^i l^n)			; Compute using "binary expansion" method.
		 (do ((bits n (ash bits -1)))
		     ((zerop bits) l^n)
		   (setq l^i (if l^i (tlist-mul l^i l^i) l))
		   (when (oddp bits)
		     (setq l^n (if l^n (tlist-mul l^n l^i) l^i))))))))))

;;;;******************************************************************************************
;;;; Extracting Trigonometric sum coefficients.

;;; Encode a call to %sin or %cos
(defun encode-tlist (expr vs)
  (let* ((arg ($expand (cadr expr)))		; trig(arg)
	 (m (mapcar #'(lambda (v)
			(let ((mm ($coeff arg v)))
			  (setq arg (sub arg (mul mm v)))
			  mm))
		    vs))
	 (sign +1))
    (when (list-negp m)				; Make sure multiples are normalized
      (setq m (mapcar #'neg m) sign -1))
    (if (eql (caar expr) '%cos)
	(if (zerop1 arg) `(()((1 . ,m)))
	    `(((,(mul (- sign) ($sin arg)) . ,m)) ((,($cos arg) . ,m))))
	(if (zerop1 arg) `(((,sign . ,m))())
	    `(((,(mul sign ($cos arg)) . ,m)) ((,($sin arg) . ,m)))))))

;;; Transform an expression into its trigonometric coefficient list form.
(defun $trig_coeffs (expr &rest vars)
  (setq expr (totalspecdisrep expr))
  (let* ((vars (instanciate-variable-list vars expr '$alt_trig_coeffs))
	 (zeros (make-list (length vars) :initial-element 0))
	 (cache nil))
    (labels  ((gcf (expr)
		(or (mexp-lookup expr cache)
		    (cdar (push (cons expr (gcf1 expr)) cache))))
	      (gcf1 (expr)
		(let ((op (and (listp expr)(caar expr))) x y)
		  (cond ((mbagp expr) (let ((elements (mapcar #'gcf (cdr expr))))
					(list (clist-mbag op (mapcar #'car elements))
					      (clist-mbag op (mapcar #'cadr elements)))))
			((or (null op)(not (dependsall expr vars))) `(()((,expr . ,zeros))))
			((member op '(%sin %cos) :test #'eq)  (encode-tlist expr vars))
			((eq op 'mplus)  (cl-reduce #'tlist-add (cdr expr) #'gcf))
			((eq op 'mtimes) (cl-reduce #'tlist-mul (cdr expr) #'gcf))
			((and (eq op 'mexpt)	; x^y Check that we can actually compute:
			      (setq x (gcf (second expr)) y (third expr))
			      (and (integerp y)(plusp y)))	; need int y >0
			 (tlist-pow x y zeros))
			(t `(()((,expr . ,zeros))))))))
      (mlist* (mlist* '$%trig vars)(map-mlist (mapcar #'map-mlist (gcf expr)))))))

(defun untlist (tlist vars)
  (flet ((un1 (list trig)
	   (flet ((un2 (e)(mul (cadr e)(cons-exp trig (multl (cddr e) vars)))))
	     (addn (mapcar #'un2 list) t))))
    (addn (mapcar #'un1 tlist '(%sin %cos)) t)))

;;;********************************************************************************
;;; SERIES & TAYLOR
;;; Given an expression, a variable and an order, compute the coefficients of the
;;; expansion of the expression about variable=0 to order ORDER.
;;;  -> [[%series,variable,order],[c,p],[c',p'],...]
;;;  or [[%taylor,variable,order],[c,p],[c',p'],...]
;;; The difference is that TAYLOR computes the Taylor expansion, whereas
;;; SERIES only carries out the expansion over arithmetic functions (+,*,exp) and thus 
;;; is significantly faster.
;;;********************************************************************************

(defun $taylor_coeffs (expr var order)
  (setq expr (totalspecdisrep expr))
  (let ((var (car (instanciate-variable-list (list var) expr '$taylor_coeffs 1))))
    (labels ((make1 (expr)
	       (cond ((mbagp expr)      (clist-mbag (caar expr) (mapcar #'make1 (cdr expr))))
		     ((freeof var expr) (list (list expr 0)))
		     (t (let* ((r ($taylor expr var 0 order))
			       (ohdr (car r))
			       (hdr (list (first ohdr)(second ohdr)(third ohdr)(fourth ohdr))))
			  (if (eq (second r) 'ps)
			      (mapcar #'(lambda (p)
					  (list (specdisrep (cons hdr (cdr p)))
						(cons-exp 'rat (caar p)(cdar p))))
				      (cddddr r))
			      (list (list (specdisrep (cons hdr (cdr r))) 0))))))))
      (mlist* (mlist* '$%taylor var order nil)(map-mlist (make1 expr))))))

;;;;******************************************************************************************
;;;; SLIST Arithmetic.
;;; The addition & multiplication of polynomial arithmetic are used.

;;; compute the N-th power of S through ORDER.
(defun slist-pow (s n order)
  (when s
    (let* ((m (cadar s))
	   (nm (mul n m))
	   (s_m (caar s))
	   (p (list (list (power s_m n) nm))))	; 1st term of result
      (if (null (cdr s))			; Single term
	  (or (great nm order) p)		; then trivial single term (unless high order)
	  (let* ((g (cl-reduce #'$gcd s #'(lambda (x)(sub (cadr x) m))))
		 (kmax (div (sub order nm) g)))
	    (do ((k 1 (1+ k)))
		((great k kmax) (nreverse p))
	      (let ((ff (div (add 1 n) k))
		    (trms nil))
		(dolist (s (cdr s))
		  (let ((i (div (sub (cadr s) m) g)))
		    (when (lessthan k i)(return))
		    (let ((e (member (add nm (mul (sub k i) g)) p :key #'cadr :test #'like)))
		      (when e			; multthru limits expression depth
 			(push ($multthru (mul (sub (mul i ff) 1) (car s) (caar e))) trms)))))
		(let ((pk ($multthru (div (addn trms t) s_m))))
		  (unless (zerop1 pk)
		    (push (list pk (add nm (mul k g))) p))))))))))

;;;;******************************************************************************************
;;;; Extracting Series Coefficients.

(defun $series_coeffs (expr var order)
  (setq expr (totalspecdisrep expr))
  (let ((v (car (instanciate-variable-list (list var) expr '$series_coeffs 1))))
    (setq v ($ratdisrep v))
    (labels ((mino (expr)			; find minumum power of v in expr (for mult)
	       (let ((op (and (listp expr)(caar expr))))
		 (cond ((like expr v)   1)	; Trivial case: expr is V itself
		       ((or ($atom expr)(freeof v expr)) 0)	; `constant' case
		       ((member op '(mplus mlist mequal $matrix))
			(cl-reduce #'(lambda (u v) ($lmin `((mlist simp) ,u ,v))) (cdr expr) #'mino))
		       ((eq op 'mtimes)  (cl-reduce #'add (cdr expr) #'mino))
		       ((and (eq op 'mexpt)($numberp (third expr)))	; can we compute?
			(mul (mino (second expr)) (third expr)))
		       (t 0))))			; oh, well, Treat it as constant.
	     (gcf (expr order)
	       (let ((op (and (listp expr)(caar expr))))
		 (cond ((like expr v)   `((1 1)))	; Trivial case: expr is V itself
		       ((or ($atom expr)(freeof v expr)) `((,expr 0)))	; `constant' case
		       ((mbagp expr)
			(clist-mbag op (mapcar #'(lambda (el)(gcf el order))(cdr expr))))
		       ((eq op 'mplus)
			(cl-reduce #'clist-add (cdr expr) #'(lambda (el)(gcf el order))))
		       ((eq op 'mtimes)
			(let* ((ms (mapcar #'mino (cdr expr)))
			       (mtot (addn ms t))
			       (prod '((1 0))))
			  (unless (great mtot order)
			    (do ((terms (cdr expr)(cdr terms))
				 (m ms (cdr m)))
				((null terms) prod)
			      (let ((term (gcf (car terms) (sub (add order (car m)) mtot))))
				(setq prod (clist-mul term prod order)))))))
		       ((and (eq op 'mexpt)($numberp (third expr)))	; can we compute?
			(slist-pow (gcf (second expr) order)(third expr) order))
		       (t `((,expr 0)))))))	; just treat it as constant.
      (mlist* (mlist* '$%series v order nil)(map-mlist (gcf expr order))))))

(defun unslist (clist vars)
  ($trunc (unclist clist vars)))

;;;********************************************************************************
;;; Find the coefficient associated with keys (powers or multiples) in the 
;;; coefficient list clist.
(defun $get_coef (clist &rest keys)
  (let ((sublist (case (and ($listp clist)($listp (cadr clist))(cadr (cadr clist)))
		   (($%poly $%series $%taylor) (cddr clist))
		   ($%trig (case (car keys)
			     (($sin %sin) (cdr (third clist)))
			     (($cos %cos) (cdr (fourth clist)))
			     (otherwise (merror "First KEY must be SIN or COS"))))
		   (otherwise (merror "Unknown coefficient list type: ~M" clist)))))
    (or (cadar (member keys sublist :test #'alike :key #'cddr)) 0)))

;;; Reconstruct a macsyma expression from a coefficient list.
(defun $uncoef (cl)
  (let ((spec (and ($listp cl)(second cl))))
    (case (and ($listp spec)(second spec))
      ($%poly  (unclist (cddr cl) (cddr spec)))
      (($%series $%taylor) (unslist (cddr cl) (cddr spec)))
      ($%trig   (untlist (mapcar #'cdr (cddr cl)) (cddr spec)))
      (otherwise (merror "UNCOEF: Unrecognized COEFFS form: ~M" cl)))))

;;;********************************************************************************
;;; Partition a polynomial, trig series or series into those terms whose 
;;; powers (or multiples) pass a certain test, and those who dont.
;;; Returns the pair [passed, failed].
;;; The TEST is applied to the exponents or multiples of each term.

(defun partition-clist (list test)
  (cond ((null test) (values nil list))
	((eq test t) (values list nil))
	(t  (let ((pass nil)(fail nil))
	      (dolist (item list)
		(if (is-boole-check (mapply test (cddr item) '$partition_test))
		    (push item pass)
		    (push item fail)))
	      (values (nreverse pass)(nreverse fail))))))

(defun $partition_poly (expr test &rest vars)
  (let* ((clist (apply #'$coeffs expr vars))
	 (vars (cddr (second clist))))
    (multiple-value-bind (p f)(partition-clist (cddr clist) test)
      (mlist* (unclist p vars)(unclist f vars) nil))))

(defun $partition_trig (expr sintest costest &rest vars)
  (let* ((tlist (apply #'$trig_coeffs expr vars))
	 (vars (cddr (second tlist))))
    (multiple-value-bind (sp sf)(partition-clist (cdr (third tlist)) sintest)
      (multiple-value-bind (cp cf)(partition-clist (cdr (fourth tlist)) costest)
	(mlist* (untlist (list sp cp) vars) (untlist (list sf cf) vars) nil)))))

(defun $partition_series (expr test var order)
  (let* ((clist ($series_coeffs expr var order))
	 (var (caddr (second clist))))
    (multiple-value-bind (p f)(partition-clist (cddr clist) test)
      (mlist* (unslist p var)(unslist f var) nil))))

(defun $partition_taylor (expr test var order)
  (let* ((clist ($taylor_coeffs expr var order))
	 (var (caddr (second clist))))
    (multiple-value-bind (p f)(partition-clist (cddr clist) test)
      (mlist* (unslist p var)(unslist f var) nil))))
