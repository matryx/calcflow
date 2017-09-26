;;  Copyright 2009 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(in-package :maxima)

(if (not (mget '$hypergeometric_simp 'mexpr)) ($load "hypergeometric_mac.mac"))
(if (not (mget '$abramowitz_id 'mexpr)) ($load "abramowitz_id.mac"))
(if (not (functionp 'simp-nfloat)) ($load "nfloat"))

;; mea culpa---for numerical evaluation of the hypergeometric functions, the
;; method uses a running error. When the error is too large, the value of fpprec
;; is increased and the evaluation is redone with the larger value of fpprec. 
;; The option variable max_fpprec is the largest value for fpprec Maxima will try.

(defmvar $max_fpprec 1000)

(setf (get '$max_fpprec 'assign)
      #'(lambda (a b) 
	  (declare (ignore a))
	  (if (not (and (atom b) (integerp b)))
	      (progn
		(mtell "The value of `max_fpprec' must be an integer.")
		'munbindp))))

(defmvar $expand_hypergeometric nil)

(setf (get '$expand_hypergeometric 'assign)
      #'(lambda (a b)
	  (declare (ignore a))
	  (if (not (or (eq b nil) (eq b t)))
	      (progn
		(mtell "The value of `expand_hypergeometric' must be either true or false.")
		'munbindp))))

;; If the length of l is n, return true; otherwise signal wna-err = (wrong number of arguments, by the way).

(defun argument-length-check (l n)
  (if (and (consp l) (consp (first l)) (equal n (length (margs l)))) t (wna-err (caar l))))

;; When multiple_value_return is nil, multiple_values(e1,e2,...) --> e1; otherwise
;; multiple_values(e1,e2,...) --> multiple_values(e1,e2,...).

(setf (get '$multiple_values 'operators) 'simp-multiple-values)

(defmvar $multiple_value_return nil)

(defun simp-multiple-values (e yy z)
  (declare (ignore yy))
  (if $multiple_value_return
      `(($multiple_values simp) ,@(mapcar #'(lambda (s) (simpcheck s z)) (cdr e)))
    (simpcheck (cadr e) z)))
	 
;; Detect undefined and polynomial cases. 

(defun classify-hypergeometric (a b x)
  (let ((ah nil) (bh nil))
  
    ;; Let bh = the least member of b that is a negative integer. If there is
    ;; no such member, set bh = nil.
    
    (dolist (bk b)
      (if (and (integerp bk) (<= bk 0) (or (eq bh nil) (< bk bh))) (setq bh bk)))
	  
    ;; Let ah = the greatest member of a that is a negative integer. If there is
    ;; no such member, set ah = nil.
    
    (dolist (ak a)
      (if (and (integerp ak) (<= ak 0) (or (eq ah nil) (> ak ah))) (setq ah ak)))

    ;; Undefined when either (1) ah is nil and bh is non-nil or (2) ah and bh are
    ;; non-nil and ah >= bh, and each member of a and b are numbers. (We don't
    ;; want hypergeometric([a],[-3],x) to be undefined, do we?). I suppose this
    ;; function could look for declared integers...
    
    (cond ((and (every '$numberp a)
		(every '$numberp b)
		(or (and (not ah) bh) (and ah bh (>= bh ah)))) 'undefined)

	  ((or ah (zerop1 ($ratdisrep x))
	       (and ($taylorp x) (eq 0 ($second ($first ($taylorinfo x))))
		    (integerp ($third ($first ($taylorinfo x))))))
	   'polynomial)

	  (t 'nonpolynomial))))

;; The function simpcheck changes taylor polynomials to general form--that messes
;; it harder to taylorize hypergeometrics (things like hypergeometric([5],[], taylor(x,x,0,3)) -->
;; a taylor polynomial. So use tsimpcheck: if e is a taylor polynomial, simplify; otherwise, simpcheck.

(defun tsimpcheck (e z)
  (if (or ($taylorp e) ($ratp e)) (simplifya e z) (simpcheck e z)))

;; We don't want realpart and imagpart to think that hypergeometric functions are 
;; real valued. So declare hypergeometric to be complex.

(eval-when
    #+gcl (load eval)
    #-gcl (:load-toplevel :execute)
    (let (($context '$global) (context '$global))
      (meval '(($declare) $hypergeometric $complex))))

(setf (get '$hypergeometric 'conjugate-function) 'conjugate-hypergeometric)

;; hypergeometric(a,b,x) is entire (commutes with conjugate) when length(a) < length(b) + 1. Also
;; hypergeometric(a,b,x) is analytic inside the unit disk. Outside the unit disk, we need to be careful;
;; for now, conjugate gives a nounform in this case. I suppose we could check for declared negative integer
;; parameter in the list a...I'll wait for a user to request this feature :)

(defun conjugate-hypergeometric (l)
  (let ((a (first l)) (b (second l)) (x (third l)))
    (cond ((or (< ($length a) (+ 1 ($length b))) (eq t (mgrp 1 (take '(mabs) x))))
	   (take '($hypergeometric) (take '($conjugate) a) (take '($conjugate) b) (take '($conjugate) x)))
	  (t `(($conjugate simp) (($hypergeometric) ,a ,b ,x))))))

(defun lenient-complex-p (e)
  (and ($freeof '$infinity '$und '$ind '$inf '$minf '$false '$true t nil e) ;; what else?
       (not (mbagp e))
       (not ($featurep e '$nonscalarp))
       (not (mrelationp e))
       (not ($member e $arrays))))

(defprop $hypergeometric simp-hypergeometric operators)

;; Do noncontroversial simplifications on the hypergeometric function. A user that
;; wants additional simplifications can use $hypergeometric_simp. The simplifications are

;;     (a) hypergeometric([], [], x) --> exp(x),

;;     (b) hypergeometric([a], [], x) --> 1 / (1 - x)^a,

;;     (c) hypergeometric([a1,...], [b1, ...], 0) --> 1,

;;     (d) hypergeometric([-n,...], [b1, ...], x) --> polynomial.

;;     (d) sort and delete common parameters; for example
;;         hypergeometric([p,b,a],[c,b],x) --> hypergeometric([a,p],[c],x).

;;     (e) hypergeometric([0,a1, ... ], [b1, ...], x) --> 1.

;;  Why does this code do (take '(mlist) a) instead of (cons '(mlist) a)? Because 
;; (cons '(mlist) a) messes up tellsimp rules. Say tellsimp([a], a]). Then 
;; (take (mlist) a) --> a, but (cons '(mlist) a) --> ((mlist) a). And that's not correct.

(defun simp-hypergeometric (e yy z)
  (declare (ignore yy))
  (argument-length-check e 3)
  (let ((a (second e)) (b (third e)) (x (fourth e)) (l nil) (a-len) (b-len) 
	(hg-type nil) (dig) (return-type) ($domain '$complex))
    
    (cond ((or (not ($listp a)) (not ($listp b))) 
	   (mtell "warning: The first two arguments to 'hypergeometric' must be lists.")
	   `(($hypergeometric simp) ,(tsimpcheck a z) ,(tsimpcheck b z) ,(tsimpcheck x z)))
	  (t
	   
	   (setq a (mapcar #'(lambda (s) (tsimpcheck s z)) (margs a))
		 b (mapcar #'(lambda (s) (tsimpcheck s z)) (margs b))
		 x (tsimpcheck x z))

	   ;; Delete common members of a and b. This code is taken from hyp.lisp.

	   (setq l (zl-intersection a b))
	   (setq a (del l a)
	   	 b (del l b))
	  
	   ;; Check for undefined cases
	   (setq hg-type (classify-hypergeometric a b x))
	 	    
	   (setq a-len (length a))
	   (setq b-len (length b))

	   ;; Sort a and b and reconvert to Maxima lists.

	   (setq a (sort a '$orderlessp))
	   (setq b (sort b '$orderlessp))
	   (setq a (simplify (cons '(mlist) a)))
	   (setq b (simplify (cons '(mlist) b)))
	   
	   ;; If constantp(x), apply rectform to x. For now, multiplication and division
	   ;; of complex numbers doesn't always return a number in rectangular form. Let's
	   ;; apply rectform to constants.

	   (if ($constantp x) (setq x ($rectform x)))
	   (cond 
	    
	    ;; Catch undefined cases and return a nounform. 
	    ((or (eq hg-type 'undefined) 
		 (member-if #'(lambda(s) (not (lenient-complex-p s))) (cdr a))
		 (member-if #'(lambda(s) (not (lenient-complex-p s))) (cdr b))
		 (not (lenient-complex-p x)))
	     `(($hypergeometric simp) ,a ,b ,x))
	     
	    ;; pFq([a1,...,ap], [b1,...,bq], 0) --> 1 + 0
	    ((zerop1 x) (add 1 x))

	    ;; pFq([0,a1,...,ap], [b1,...,bq], x) --> 1
	    ((member-if 'zerop1 (margs a)) 1)
	    	       
	    ;; Do hypergeometric([],[],x) --> exp(x). All numerical evaluation is funneled through
	    ;; the same entry point; the function hypergeometric-0f0 doesn't do numerical evaluation.
	    ((and (= 0 a-len) (= 0 b-len) (hypergeometric-0f0 x)))

	    ;; Do hypergeometric([a],[],x) --> 1 / (1-x)^a. 
	    ((and (= a-len 1) (= 0 b-len) (hypergeometric-1f0 (second a) x)))
		 		 		 
	    ;; Try reflection identity for 1F1.
	    ((and (= a-len 1) (= b-len 1) (hypergeometric-1f1 (second a) (second b) x hg-type)))
		 
	    ;; For 2F1, value at 1--nothing else.
	    ((and (= a-len 2) (= b-len 1) (hypergeometric-2f1 (second a) (third a) (second b) x)))
		 	
	    ;; Try numerical evaluation; return nil on failure. This should handle IEEE float, 
	    ;; IEEE complex float, bigfloat, and complex big float cases.
	    ((and (setq return-type (use-float-hypergeometric-numerical-eval (margs a) (margs b) x))
		  (setq dig (ceiling (* (if (eq return-type 'float) (float-digits 1.0) fpprec) 
					#.(/ (log 2) (log 10)))))
		  (hypergeometric-float-eval (margs a) (margs b) x dig return-type)))
	    
	    ;; Try rational number numerical evaluation; return nil on failure. This should handle
	    ;; rational and complex rational numerical evaluation.
	    ((use-rational-hypergeometric-numerical-eval (margs a) (margs b) x)
	     (rational-hypergeometric-numerical-eval (margs a) (margs b) x))

	    ;; Handle all other polynomial cases; this includes the case that
	    ;; x is a Taylor polynomial centered at zero.
	    ((hypergeometric-poly-case (margs a) (margs b) x))
	
	    ;; Return a nounform.
	    (t `(($hypergeometric simp) ,a ,b ,x)))))))

;; When x isn't a float, do 0F0([],[],x) --> exp(x).
(defun hypergeometric-0f0 (x)
  (if (use-float-hypergeometric-numerical-eval nil nil x) nil (take '(mexpt) '$%e x)))

;; When a or x aren't floats, do 1F0([a],[],x) --> 1/(1-x)^a. 
(defun hypergeometric-1f0 (a x)
  (cond ((use-float-hypergeometric-numerical-eval (list a) nil x) nil)
	((onep x)
	 (if (eq t (mgrp 0 a)) 0 nil))
	(t  (div 1 (take '(mexpt) (sub 1 x) a)))))
  
;; Apply the Kummer reflection identity when b-a is a negative integer and we know that
;; the hypergeometric function is not already known to be a polynomial (that is a is not a
;; negative integer) or when (great (neg x) x); otherwise, return nil. This function 
;; doesn't do floating point evaluation.

(defun hypergeometric-1f1 (a b x hg-type)
  (cond ((use-float-hypergeometric-numerical-eval (list a) (list b) x) nil)
	((or (and (not (eq hg-type 'polynomial)) (great (neg x) x))
	     (and (not (eq hg-type 'polynomial)) (integerp (sub b a)) (< (sub b a) 0)))
	 (mul (take '(mexpt) '$%e x) 
	      (take '($hypergeometric) (take '(mlist) (sub b a)) (take '(mlist) b) (neg x))))
	(t nil)))

;; Coerce x to the number type of z. The Maxima function safe_float returns a bigfloat when
;; conversion to a float fails (overflow, for example).
(defun number-coerce (x z)
  (cond ((complex-number-p z '$bfloatp)
	 ($bfloat x))
	((complex-number-p z 'floatp)
	 (mfuncall '$safe_float x))
	(t x)))

;; 2F1(a,b;c, x) -->  gamma(c) gamma(c - a - b) / (gamma(c-a) gamma (c-b)) 
;; (Chu-Vandermonde identity, A & S 15.1.20) provided real_part(c-a-b) > 0 and c # 0,-1,-2, ...
;; The c = 0, -1, -2, ... case should be caught previously. If we wanted to be super careful, we'd
;; demand explicitly that c isn't a negative integer.

(defun hypergeometric-2f1 (a b c x)
  (let ((z)) 
    (setq z (sub c (add a b)))
    (cond ((and (onep1 x) (complex-number-p z '$numberp) (eq t (mgrp ($realpart z) 0)))
	   (number-coerce 
	    (div
	     (mul (take '(%gamma) c) (take '(%gamma) z))
	     (mul (take '(%gamma) (sub c a)) (take '(%gamma) (sub c b))))
	    x))
	  (t nil))))


		     
;; For numerical evaluation of a general hypergeometric function, there aren't many 
;; alternatives to power series summation.

;; Pursuant to well-established Maxima coding practices :), bigfloat
;; functions receive bigfloat arguments and return bigfloat values.

(in-package #-gcl #:bigfloat #+gcl "BIGFLOAT")

;(import 'maxima::while) ;; <--- broken Why?

(defmacro while (cond &rest body)
  `(do ()
       ((not ,cond))
     ,@body))

(defun 0f0-numeric (x)
  (exp x))

(defun 1f0-numeric (a x)
  (/ 1 (expt (- 1 x) a)))
   	  
(defun gamma (x)
  (bigfloat (maxima::$bfloat (maxima::take '(maxima::%gamma) (maxima::to x)))))

;; This is DLMF: http://dlmf.nist.gov/15.15#E1 with zo = 1/2. Also here is Maxima code that
;; sums the first n+1 terms of the sum. The CL function 2f1-numeric-alt uses a running
;; error and it sums until three consectutive partial sums have a modified relative difference
;; that is bounded by the machine epsilon.

#|
ff(a,b,c,x,n) := block([f, f0 : 1, f1 : 1- 2 * b / c,s : 1,k : 1, cf : a / (1-2/x), z],
  b : c - 2 * b,
  z : 1 - 2 / x,
  while k <= n do (    
    s : s + cf * f1,
    cf : cf * (a + k) / ((k + 1) * z),    
    f : (k * f0 + b * f1)/(k+c),
    f0 : f1,
    f1 : f,
    k : k + 1),
  s / (1-x/2)^a)$
|#

(defun 2f1-numeric-alt (a b c x)
  (let ((f) (f0 1) (f1 (- 1 (/ (* 2 b) c))) (s 1) (ds 1) (k 1) (cf (/ a (- 1 (/ 2 x)))) (z) (se 0) 
	(eps (epsilon x)) (done 0))
    (setq b (- c (* 2 b)))
    (setq z (- 1 (/ 2 x)))
    (while (< done 3)
      (setq ds (* cf f1))
      (setq s (+ s ds))
      (setq done (if (< (abs ds) (* eps (max 1 (abs s)))) (+ done 1) 0))
      (setq se (+ se (abs s) (abs ds)))
      (setq cf (/ (* cf (+ a k)) (* (+ 1 k) z)))
      (setq f (/ (+ (* k f0) (* b f1)) (+ k c)))
      (setq f0 f1)
      (setq f1 f)
      (setq k (+ k 1)))
    (values (/ s (expt (- 1 (/ x 2)) a)) (* se (epsilon x)))))

;; hypergeometric([ma,mb],[mc],mx); prefix m means Maxima expression.

(defun 2f1-numeric (ma mb mc mx digits)
  (let* ((region) (f) (ff) (er) (local-fpprec digits) (eps) (mma) (mmb) (mmc) (mmx)
	 (x (bigfloat::to mx))
	 (d (list (list "none" (abs x)) ;; region I, inside unit disk
		  (list "15.3.4" (if (= x 1) nil (abs (/ x (- x 1)))))
		  (list "15.3.6" (abs (- 1 x)))
		  (list "15.3.7" (if (zerop x) nil (abs (/ 1 x))))
		  (list "15.3.8" (if (= x 1) nil (abs (/ 1 (- 1 x)))))
		  (list "15.3.9" (if (zerop x) nil (abs (- 1 (/ 1 x))))))))
     
    (setq d (delete-if #'(lambda(s) (null (second s))) d))
    ;; Sort d from least to greatest magnitude.
    ;;(print `(d = ,d))
    (setq d (sort d #'(lambda (a b) (< (second a) (second b)))))
    (setq region (first (first d)))
    ;;(print `(region = ,region))
    ;;(print `(d = ,(second (first d))))
    
    (cond 
     ;; When x = 0, return 1. 
     ((zerop x) 1)
     
     ;; Use the alternative numerical method when |x| > 0.9; this happens when x is near exp(+/- %i %pi / 3).

     ((> (second (first d)) 0.9)
      (setq eps (epsilon (bigfloat::to mx)))
      (setq er 1)
      (setq f 1)

      (while (> (abs er) (* eps (max (abs f) 1)))
	(maxima::bind-fpprec local-fpprec
			     (setq mma (maxima::nfloat ma `((maxima::mlist)) local-fpprec maxima::$max_fpprec))
			     (setq mmb (maxima::nfloat mb `((maxima::mlist)) local-fpprec maxima::$max_fpprec))
			     (setq mmc (maxima::nfloat mc `((maxima::mlist)) local-fpprec maxima::$max_fpprec))
			     (setq mmx (maxima::nfloat mx `((maxima::mlist)) local-fpprec maxima::$max_fpprec))
			     (multiple-value-setq (f er) 
			       (2f1-numeric-alt 
				(bigfloat::to mma) (bigfloat::to mmb) (bigfloat:to mmc) (bigfloat::to mmx)))
			     (setq local-fpprec (* 2 local-fpprec))))
      
      (values f er))
     ;; ma or mb negative integers--that causes trouble for most of the A&S 15.3.4--15.3.9 
     ;; transformations--let's quickly dispatch hypergeometric-float-eval; also dispatch
     ;; hypergeometric-float-eval when the tranformation is "none" (with adjust-parameters
     ;; is false!

     ((or (equal region "none") (and (integerp ma) (<= ma 0)) (and (integerp mb) (<= mb 0))
	  (< (abs x) 0.5))
      (hypergeometric-float-eval (list ma mb) (list mc) mx digits nil))
 
     ;; The case of a,b, and c integers causes trouble; let's dispatch hgfred on it.
     ((and (integerp ma) (integerp mb) (integerp mc))
      (setq f (maxima::$hgfred (maxima::take '(maxima::mlist) ma mb)
			       (maxima::take '(maxima::mlist) mc) 'maxima::z))
      (setq f (maxima::$horner f 'maxima::z))
      (let ((d))
	(multiple-value-setq (f d) 
	  (maxima::nfloat f `((maxima::mlist) ((maxima::mequal) maxima::z ,mx)) digits maxima::$max_fpprec))
	(values (bigfloat f) (bigfloat d))))

     (t
      (let ((maxima::$multiple_value_return t))
	(setq ff  `((maxima::$hypergeometric maxima::simp)
		    ((maxima::mlist maxima::simp) ,ma ,mb) 
		    ((maxima::mlist maxima::simp) ,mc) maxima::z))
	(setq f nil)
	(while d 
	  (setq f (if (equal region "none")
		      `((maxima::multiple_values) ,ff t)
		    (maxima::mfuncall 'maxima::$abramowitz_id ff region)))
	  (if (maxima::$second f)
	      (setq d nil f (maxima::$first f)) (setq region (first (pop d)))))
	
	;;(maxima::displa f)
	;;(maxima::displa `((maxima::mequal) maxima::z ,mx))
	(setq f (multiple-value-list
		 (maxima::nfloat f `((maxima::mlist) ((maxima::mequal) maxima::z ,mx)) 
				 digits maxima::$max_fpprec)))
	(values (bigfloat::to (first f)) (bigfloat::to (second f))))))))

;; Let a = (a1, a2, ..., am) and b = (b1, b2, ..., bn). Approximate sum(c(k) x^k / k!,k,1,inf),
;; where c(k + 1) / c(k) = (a1 + k) (a2 + k) ... (am + k) / (b1 + k) (b2 + k) ... (bn + k).
   
(defun hypergeometric-by-series (a b x)
  ;; es = running error for e and ez running error for z.

  (let ((s 0) (s0 1) (k 0) (z 1) (es 0) (ez 1) (n) (p) (q) (stop 20000) (dig))
    (setq n (* 2 (+ (length a) (length b) 1)))
    (while (and (< k stop) (/= s s0)) ;; (not (= s s0)))
      (setq s s0)
      (setq p (reduce #'* (mapcar #'(lambda (s) (+ s k)) a))) ;; p adds and p-1 multiplications
      (setq q (reduce #'* (mapcar #'(lambda (s) (+ s k)) b))) ;; q adds and q-1 multiplications
      (incf k)
      (setq z (* z (/ (* p x) (* q k))))
      ;;(setq ez (+ (* n (abs z)) ez))
      (setq ez (+ (* (abs (/ (* x p) (* q k))) ez) (* (abs z) n)))
      (setq s0 (+ s z))
      (setq es (+ es ez (abs s0))))
   
    ;;(print `(k = ,k))
    (if (>= k stop) (values nil nil) 
      (progn
	;; estimate number of correct digits:

	(setq dig (floor 
		   (*
		    (- (log (max (abs s) (epsilon x))) (log (* es (epsilon x))))
		    #.(/ (log 2) (log 10)))))

	;;(print "-----------")
	;;(maxima::displa `((maxima::mequal) k ,k))
	;;(maxima::displa `((maxima::mequal) xxx ,(maxima::to (epsilon x))))
	;;(maxima::displa `((maxima::mequal) es ,(maxima::$float (maxima::to es))))
	;;(maxima::displa `((maxima::mequal) s ,(maxima::$float (maxima::to s))))
	;;(maxima::displa `((maxima::mequal) dig ,(maxima::$float (maxima::to dig))))
	(values s dig)))))
 
(defun hypergeometric-poly-case (a b x)
  (let ((z 1) (s 1) (k 0) (p) (q))
    (while (not (zerop z))
      (setq p (reduce #'* (mapcar #'(lambda (s) (+ s k)) a)))
      (setq q (reduce #'* (mapcar #'(lambda (s) (+ s k)) b)))
      (incf k)
      (setq z (/ (* p x z) (* q k)))
      (setq s (+ z s)))
    s))

;; This function numerically evaluates pFq([a1,...,ap], [b1,....bq], x), where all the arguments
;; are Maxima expressions, not bigfloat objects.

(defun hypergeometric-float-eval (ma mb mx digits &optional (adjust-params t))
  (let ((a-len (length ma)) (b-len (length mb)) (f nil) (local-fpprec maxima::$fpprec) (d) (a) (b) (x))

    ;(maxima::displa `((maxima::mlist) ,@ma))
    ;(maxima::displa `((maxima::mlist) ,@mb))
    ;(maxima::displa `((maxima::mlist) ,mx))
    (setq a (mapcar #'bigfloat::to ma))
    (setq b (mapcar #'bigfloat::to mb))
    (setq x (bigfloat::to mx))
       
    ;; Special case 0f0, 1f0, 2f1 for |x| > 1, and pfq for |x| > 1 and p >= q + 1.
    ;; For a general hypergeometric, I don't know how to analytically continue, so in the last case, 
    ;; return false. 

    ;; In the general case, sum the hypergeometric series using a running error, recursing
    ;; on local-fpprec; bailout when local-fpprec exceeds 1000.

    (cond ((and (eq a-len 0) (eq b-len 0)) ;; special case 0f0
	   (values (0f0-numeric x) digits))

	  ((and (eq a-len 1) (eq b-len 0)) ;; special case 1f0
	   (values (1f0-numeric (first a) x) digits))

	  ((and (eq a-len 1) (integerp (first a)) (< (first a) 0) (eq b-len 1)) ;; special case 1f1
	   (maxima::bind-fpprec local-fpprec
	  			(multiple-value-setq (f d) (1f1-downward-recursion (first a) (first b) x)))
	   (values f d))
						
	  ;; Optionally do Kummer transformation--when is the Kummer transformation advantageous?
	  ;; I think the sum is ill-conditioned when realpart(x) < 0. Since x is a float, realpart
	  ;; should be quick.

	  ;; The adjust-params argument should prevent an infinite loop (transform --> untransform ...)
	  ;; In this case, an infinite loop shouldn't happen even without the adjust-param scheme.

 
	  ((and adjust-params 
		(eq a-len 1) 
		(eq b-len 1)
		(< (realpart x) 0))
	   (let ((f) (d))
	     (multiple-value-setq (f d) (hypergeometric-float-eval 
					 (list (maxima::sub (car mb) (car ma))) 
					 mb (maxima::neg mx) digits nil))
	     (values (* (exp x) f) d)))
	  	  
	  ;; analytic continuation for 2f1;
	  ((and (eq a-len 2) (eq b-len 1) adjust-params)
	   (2f1-numeric (car ma) (cadr ma) (car mb) mx digits))
	  
	  ((or (< a-len (+ b-len 1)) (in-unit-circle-p x) (eq 'maxima::polynomial 
							      (maxima::classify-hypergeometric ma mb mx)))

	   ;; recurse on local-fpprec; bailout when local-fpprec exceeds $max_fpprec.
	 
	   (while (and (or (null f) (< d digits)) (< local-fpprec maxima::$max_fpprec))
	     (maxima::bind-fpprec local-fpprec
				  (multiple-value-setq (f d) (hypergeometric-by-series a b x))
				  (setq a (mapcar #'(lambda (s) (bigfloat::to (maxima::$bfloat s))) ma))
				  (setq b (mapcar #'(lambda (s) (bigfloat::to (maxima::$bfloat s))) mb))
				  (setq x (bigfloat::to (maxima::$bfloat mx)))
				  ;(print "----------")
				  ;(print `(fpprec = ,local-fpprec))
				  ;(print `(d = ,d))
				  ;(print `(digits = ,digits))
				  ;(incf local-fpprec (+ (- digits d) 10))))
				  (setq local-fpprec (* 2 local-fpprec))))
	   
	   (if (>= local-fpprec maxima::$max_fpprec) 
	       (progn
		 (maxima::mtell "Exceeded maximum allowed fpprec.")
		 (values nil nil))
	     (values f d))))))

(defun in-unit-circle-p (x)
  (< (abs x) 1))

;; Compute f11(a,b,x) using downward recursion (A&S 13.4.1). The first argument must be a negative integer:
;;
;;    f <-- (k * fo + (2 * k + x) * fm1)/(b-k)
;;

;; I think this is faster than the power series summation--it might be useful for orthogonal polynomials.
(defun 1f1-downward-recursion (a b x)
  (let ((fo 1) (fm1 (- 1 (/ x b))) (f) (k -1) (efo 0) (efm1 0) (ef 0))
    (declare (type fixnum k))
    (setq k -1)
    (cond ((eq a 0) (values fo 0))
	  ((eq a -1) (values fm1 0))
	  (t
	   (setq x (- x b))
	   (while (>= k a)
	     (setq f (/ (- (* k fo) (* (+ (* 2 k) x) fm1)) (- b k)))
	     (setq ef 
		   (+
		    (/ (+ (* k efo) 
			  (* (abs (+ (* 2 k) x))) (+ efm1 (* 2 fm1))
			  (* k fo))		
		       (abs (- b k)))
		    (* 3 (abs f))))

	     (setq fo fm1)
	     (setq efo efm1)
	     (setq fm1 f)
	     (setq efm1 ef)
	     (decf k))
	   (values fo efo)))))
	 
(in-package :maxima)

(defun float-or-bigfloat-p (x)
  (or (floatp x) ($bfloatp x)))

(defun float-or-rational-p (x)
  (or (floatp x) ($ratnump x)))

;; Return true iff it is possible to evaluate hypergeometric(a,b,x) using (exact)
;; rational arithmetic. Thus (1) x and every member of a and b (Common Lisp lists) must be
;; a $ratnump and (2) some member of a must be an explicit negative integer. When $numer
;; is true, never do exact rational evaluation? (Likely when $numer is true, we'll never
;; get here anyway?)

(defun use-rational-hypergeometric-numerical-eval (a b x)
  (and (not $numer)
       (complex-number-p x '$ratnump)
       (every #'(lambda (s) (complex-number-p s '$ratnump)) a)
       (every #'(lambda (s) (complex-number-p s '$ratnump)) b)
       (some #'(lambda (s) (and (integerp s) (< s 0))) a)))

;; Evaluate hypergeometric(a,b,x) using (exact) rational arithmetic. Here a 
;; and b are Common Lisp lists. Don't call this function without first 
;; checking that use-rational-hypergeometric-numerical-eval returns true.
;; These are all polynomial cases, so we don't need any analytic continuations.

(defun rational-hypergeometric-numerical-eval (a b x)
  (setq a (mapcar #'(lambda (s) (bigfloat::to s)) a))
  (setq b (mapcar #'(lambda (s) (bigfloat::to s)) b))
  (setq x (bigfloat::to x))
  (maxima::to (bigfloat::hypergeometric-poly-case a b x)))

;; Return float if hypergeometric(a,b,x) should evaluate to a double float (real or 
;; complex; return bigfloat if it should evaluate to a bigfloat (real or complex); otherwise
;; return false.

(defun use-float-hypergeometric-numerical-eval (a b x)

  ;; float, complex float, bigfloat, and complex bigfloat; this is a great deal of 
  ;; stuff to check. When $numer is true, everybody must be a $numberp for numerical
  ;; evaluation; when numer is false, everybody must be a $numberp and somebody must 
  ;;be a float.

  (if (and (every #'(lambda (s) (complex-number-p s '$numberp)) a)
	   (every #'(lambda (s) (complex-number-p s '$numberp)) b)
	   (complex-number-p x '$numberp)
	   (or
	    $numer
	    (not (every #'(lambda (s) (complex-number-p s '$ratnump)) a))
	    (not (every #'(lambda (s) (complex-number-p s '$ratnump)) b))
	    (not (complex-number-p x '$ratnump))))

       ;; When everybody is a float or rational, the return type is float; otherwise bigfloat.
      (if (and
	   (every #'(lambda (s) (complex-number-p s 'float-or-rational-p)) a)
	   (every #'(lambda (s) (complex-number-p s 'float-or-rational-p)) b)
	   (complex-number-p x 'float-or-rational-p))
	  'float 'bigfloat) nil))
   
;; Evaluate pFq(a,b,x) using floating point arithmetic. Coerce the returned value 
;; to the type described by return-type.

;; When there is a double float overflow, ignore-errors should return nil. After that, we'll
;; try again with a bigfloat.
			 
(defun hypergeometric-float-eval (a b z digits return-type)
  (let ((d) (x))
    (multiple-value-setq (x d) (ignore-errors (bigfloat::hypergeometric-float-eval a b z digits)))

    (cond ((and (null x) (eq return-type 'float))
	   (number-coerce 
	    (hypergeometric-float-eval (mapcar '$bfloat a) 
				       (mapcar '$bfloat b) 
				       ($bfloat z) 
				       digits
				       'bigfloat) 1.0))

	  ((or (null x) (null d)) nil)
	  
	  ((eq return-type 'float)
	   ($float (maxima::to x)))

	  ((eq return-type 'bigfloat)
	   ($bfloat (maxima::to x)))

	  ;; Unused hypergeometric-float-eval doesn't return rational
	  ;; ((eq return-type 'rational)
	  ;;  ($rationalize (maxima::to x)))
	  
	  ;; This should not happen.
	  (t (maxima::to x)))))

(defun hypergeometric-poly-case (a b x)
  (let ((n nil) (z 1) (s 1) (p) (q) (cf 1))
    
    ;; Determine how many terms to sum
    (cond ((and ($taylorp x) (eq 0 ($second ($first ($taylorinfo x))))
		(integerp ($third ($first ($taylorinfo x)))))
	   (setq n ($third ($first ($taylorinfo x)))))
	  
	  ((some #'(lambda (s) (and (integerp s) (<= s 0))) a)
	   (dolist (ak a)
	     (if (and (integerp ak) (< ak 0)) (setq n (if (null n) ak (max n ak)))))
	   (setq n (- n)))
	  (t (setq n nil)))

    (if ($ratp x) (setq s ($rat 1) z ($rat 1)))

    ;; Expand to a polynomial when n is an integer and either 
    ;;  (1) x and each member of a and b are complex numbers, 
    ;;  (2) n < $expop or $expand_hypergeometric 
    ;;  (3) x is a CRE expression.
        
    (if (and (integerp n) (or (and (complex-number-p x '$numberp) 
				   (every #'(lambda (s) (complex-number-p s '$numberp)) a)
				   (every #'(lambda (s) (complex-number-p s '$numberp)) b))
			      (or $expand_hypergeometric (< n $expop))
			      ($ratp x)))
	(dotimes (k n s)
	  (setq p (reduce #'mul (mapcar #'(lambda (s) (add s k)) a)))
	  (setq q (reduce #'mul (mapcar #'(lambda (s) (add s k)) b)))

	  ;; sigh..Maxima should (I think) return a rectangular form for
	  ;; complex number multiplication and division. But it doesn't. If 
	  ;; that changes, delete the next two lines.

	  (setq cf (mul cf (div p (mul q (+ k 1)))))
	  (if ($constantp cf) (setq cf ($rectform cf)))
	  (setq z (mul z x))
	  (setq s (add s (mul cf z))))
	 
      nil)))

(defun diff-hypergeometric (a b z x)
  (cond ((and ($freeof x a) ($freeof x b))
	 (setq a (margs a))
	 (setq b (margs b))
	 (let ((p (reduce #'mul a))
	       (q (reduce #'mul b)))
	   (setq a (simplify (cons '(mlist) (mapcar #'(lambda (s) (add 1 s)) a))))
	   (setq b (simplify (cons '(mlist) (mapcar #'(lambda (s) (add 1 s)) b))))
	   (mul ($diff z x) p (div 1 q) (take '($hypergeometric) a b z))))
	(t (merror "Maxima does not know the derivatives of the hypergeometric functions with respect to the parameters"))))


;; TeX hypergeometric([a],[b,c],x) as $$F\left( \begin{bmatrix}a\\b\;\,c\end{bmatrix} ,x\right)$$
;; For no good reason, I'm not so fond of pFq notation. Some newer references don't use
;; the pFq notation.

(defprop $hypergeometric tex-hypergeometric tex)

(defun tex-hypergeometric (x l r)
  (let ((p) (q) (wide-space ",\\;"))
    (setq p (tex-list (margs (cadr x)) nil nil wide-space))
    (setq q (tex-list (margs (caddr x)) nil nil wide-space))
    (setq p `(,@l "F\\left( \\left. \\begin{array}{c}" ,@p "\\\\" ,@q "\\end{array} \\right |,"))
    (tex (fourth x) p `("\\right)" ,@r) 'mparen 'mparen)))

;; Integral of hypergeometric(a,b,z)
;; 
;; Integrals and Series: Volume 3, More Special Functions
;; Prudnikov, A. P., Brychkov, Yu A., Gould, G. G., Marichev, O.I.
;; 
;;  /
;; [
;; I pFq((a_p);(b_q);c z) dz
;; ]
;; /
;;
;;     = z (p+1)F(q+1)((a_p),1;(b_q),2;c z)                 1.16.1.2
;;
;;        product((b_q - 1))
;;     =  ------------------ pFq((a_p)-1; (b_q)-1; c z)     1.16.1.3
;;        product((a_p - 1))

(defun hyp-integral-3 (a b z)
  "Integral of hypergeometric(a,b,z) wrt z"
  (let* (($listarith t)
	 (a-1 (add a -1))
         (b-1 (add b -1))
         (prod_b-1 (reduce #'mul (margs b-1)))
         (prod_a-1 (reduce #'mul (margs a-1))))
    (if (eq prod_a-1 0)
      (mul z (take '($hypergeometric) (append a '(1)) (append b '(2)) z))
      (mul prod_b-1 (inv prod_a-1) (take '($hypergeometric) a-1 b-1 z)))))

(putprop '$hypergeometric `((a b z) nil nil ,#'hyp-integral-3) 'integral)
