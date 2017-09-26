;;  Copyright 2009 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;; The last time I tried, the file "hypergeometric.lisp" must be loaded before compiling nfloat; so

;(eval-when (compile) 
;  ($load "hypergeometric.lisp"))

(in-package :maxima)


(in-package #-gcl #:bigfloat #+gcl "BIGFLOAT")

;; Each member of the CL list l is a two member list (running error form).
(defun running-error-plus (l)
  (let ((acc 0) (err 0))
    (dolist (lk l)
      (setq acc (+ acc (first lk)))
      (setq err (+ err (second lk) (abs acc))))
    (list acc err)))

;;(%i20) ah * lh * (1 + eps);
;;(%o20) ah*(eps+1)*lh
;;(%i21) %-a*l;
;;(%o21) ah*(eps+1)*lh-a*l
;;(%i22) subst([a = ah-e,l = lh-w],%);
;;(%o22) ah*(eps+1)*lh-(ah-e)*(lh-w)
;;(%i23) expand(%);
;;(%o23) -e*w+ah*w+ah*eps*lh+e*lh

(defun running-error-prod (l)
  (let ((acc 1) (err 0) (z))
    (dolist (lk l)
      (setq z acc)
      (setq acc (* acc (first lk)))
      (setq err (+ (* err (abs (first lk))) (* (abs z) (abs (second lk))))))
    (list acc err)))

;; (%i1) (a+ae*eps)*(1+eps)/(b+be*eps)-a/b$
;; (%i2) expand(limit(%/eps,eps,0))$
;; (%i3) expand(ratsubst(Q,a/b,%))$
;; (%i4) map('abs,%)$
;; (%i5) facsum(%,abs(Q));
;; (%o5) ((abs(be)+abs(b))*abs(Q)+abs(ae))/abs(b)
	
(defun running-error-quotient (l)
  (let* ((a (first l)) (b (second l)) (s))
    (setq s (/ (first a) (first b)))
    (list s (+ (* (abs s) (+ 1 (abs (/ (second b) (first b))))) (abs (/ (second a) (first b)))))))

;; unary negation.	 
(defun running-error-negate (x)
  (setq x (first x))
  (list (- (first x)) (second x)))

;;(%i46) (x*(1+ex))^(n *(1+en));
;;(%o46) ((ex+1)*x)^((en+1)*n)
;;(%i47) taylor(%,[ex,en],0,1);
;;(%o47) x^n+(x^n*n*ex+x^n*n*log(x)*en)+...
;;(%i48) factor(%);
;;(%o48) x^n*(en*n*log(x)+ex*n+1)

(defun running-error-expt (l)
  (let* ((s) (x (first l)) (n (second l)) (ex) (en))
    (setq ex (second x))
    (setq en (second n))
    (setq x (first x))
    (setq n (first n))
    (setq s (bigfloat::expt x n))
    (list s (+ (abs (* s en n (log x))) (abs (* s ex n))))))

;; sqrt(x + ex) = sqrt(x)+(sqrt(x)*ex)/(2*x)+... = sqrt(x) + ex / (2 * sqrt(x)) + ...
(defun running-error-sqrt (x)
  (setq x (first x))
  (let ((s (sqrt (first x))))
    (list s (abs (/ (second x) (* 2 s))))))

;; log(x + ex) = log(x) + ex / x + ... 
(defun running-error-log (x)
  (setq x (first x))
  (list (log (first x)) (abs (/ (second x) (first x)))))

(defun psi0 (x)
  (bigfloat (maxima::$rectform (maxima::mfuncall 'maxima::$bfpsi0 (maxima::$bfloat (maxima::to x))
						 maxima::$fpprec))))

;; gamma(x + ex) = gamma(x) + ex * gamma(x) * psi[0](x) + ..
(defun running-error-gamma (x)
  (setq x (first x))
  (let ((s (gamma (first x))))
    (list s (abs (* s (second x) (psi0 (first x)))))))

(defun running-error-hypergeometric (a b x subs bits)
  (let ((dig) (d) (f))
    
    ;; To do this correctly, we'd need the partial derivatives of the hypergeometric functions
    ;; with respect the the parameters. Ouch!

    (setq a (mapcar #'(lambda (s) (car (running-error-eval s subs bits))) (maxima::margs a)))
    (setq b (mapcar #'(lambda (s) (car (running-error-eval s subs bits))) (maxima::margs b)))
    (setq x (car (running-error-eval x subs bits)))
    (cond ((< (abs x) 0.99)
	   (multiple-value-setq (f d) (hypergeometric-by-series a b x))
	   (list f (* (abs f) (expt 10 (- d)))))
	  (t
	   (setq dig (ceiling (* bits #.(/ (log 2.0) (log 10.0)))))
	   (setq a (mapcar 'maxima::to a))
	   (setq b (mapcar 'maxima::to b))
	   (setq x (maxima::to x))
	   (multiple-value-setq (f d) (hypergeometric-float-eval a b x dig t))
	   (list f (* (abs f) (expt 10 (- d))))))))

(defun running-error-sum (l subs bits)
  (let ((sumand (first l))
	(v (second l))
	(lo (third l))
	(hi (fourth l))
	(acc 0) (err 0) (x) (q))

    (cond ((and (integerp lo) (integerp hi))
	   (maxima::while (<= lo hi)
	     (setq q (maxima::$sublis `((maxima::mlist) ((maxima::mequal) ,v ,lo)) sumand))
	     (setq q (maxima::simplify q))
	     (setq x (running-error-eval q subs bits))
	     (incf lo)
	     (setq acc (+ acc (first x)))
	     (setq err (+ err (second x) (abs acc))))
	   (list acc err))
	  (t (throw 'maxima::nfloat-nounform-return 'return-nounform)))))

(defun running-error-product (l subs bits)
  (let ((prodand (first l)) ;; a sum has a summand, so a product has a ...
	(v (second l))
	(lo (third l))
	(hi (fourth l))
	(acc 1) (err 0) (x))

    (cond ((and (integerp lo) (integerp hi))
	   (maxima::while (<= lo hi)
	     (setq x (maxima::$sublis `((maxima::mlist) ((maxima::mequal) ,v ,lo)) prodand))
	     (setq x (maxima::simplify x))
	     (setq x (running-error-eval x subs bits))
	     (incf lo)
	     (setq acc (* acc (first x)))
	     (setq err (+ err (second x) (abs acc))))
	   (list acc err))
	  (t (throw 'maxima::nfloat-nounform-return 'return-nounform)))))

(defun running-error-abs (l)
  (setq l (first l))
  (list (abs (first l)) (second l)))

(defun running-error-conjugate (l)
  (setq l (first l))
  (list (conjugate (first l)) (second l)))

;; untested!!!!!
(defun running-error-factorial (l)
  (setq l (first l))
  (if (integerp l)
      (list (maxima::take (list 'maxima::mfactorial) l) 0)
    (running-error-gamma (list (list (+ 1 (first l)) (second l))))))
	 
(defun running-error-atan2 (l)
  (let* ((y (first l))
	 (x (second l))
	 (d (/ 1 (+ (* (first x) (first x)) (* (first y) (first y))))))
    (list (atan (first y) (first x))
	  (* (+ (* (abs (second y)) (first x)) (* (abs (second x)) (first y))) d))))
    
(defun running-error-realpart (l)
  (setq l (first l))
  (list (realpart (first l)) (second l)))

(defun running-error-imagpart (l)
  (setq l (first l))
  (list (imagpart (first l)) (second l)))


;; For a similar hashtable mechanism, see trig.lisp.
(defvar *running-error-op* (make-hash-table :size 16)
  "Hash table mapping a maxima function to a corresponding Lisp
  function to evaluate the maxima function numerically using a running error.")

(setf (gethash 'maxima::mplus *running-error-op*) #'running-error-plus)
(setf (gethash 'maxima::mtimes *running-error-op*)  #'running-error-prod)
(setf (gethash 'maxima::mquotient *running-error-op*) #'running-error-quotient)
(setf (gethash 'maxima::mminus *running-error-op*) #'running-error-negate)
(setf (gethash 'maxima::mexpt *running-error-op*) #'running-error-expt)
(setf (gethash 'maxima::%sqrt *running-error-op*) #'running-error-sqrt)
(setf (gethash 'maxima::%log *running-error-op*) #'running-error-log)
(setf (gethash 'maxima::%gamma *running-error-op*) #'running-error-gamma)
(setf (gethash 'maxima::mabs *running-error-op*) #'running-error-abs)
(setf (gethash 'maxima::$cabs *running-error-op*) #'running-error-abs)
(setf (gethash 'maxima::$conjugate *running-error-op*) #'running-error-conjugate)
(setf (gethash 'maxima::mfactorial *running-error-op*) #'running-error-factorial)
(setf (gethash 'maxima::$atan2 *running-error-op*) #'running-error-atan2)
(setf (gethash 'maxima::$realpart *running-error-op*) #'running-error-realpart)
(setf (gethash 'maxima::$imagpart *running-error-op*) #'running-error-imagpart)

(defun running-error-eval (e subs bits)
  (let ((f))
   
    (cond ((eq e 'maxima::$%i) 
	   (setq e (bigfloat::to (if (> bits #.(float-digits 1.0e0)) (maxima::$bfloat 1) (maxima::$float 1))))
	   (list (bigfloat::to 0 e) (abs e)))
	 
	  ((maxima::complex-number-p e #'(lambda (s) (or (maxima::$ratnump s) (maxima::$numberp s))))
	   (setq e (bigfloat::to (if (> bits #.(float-digits 1.0e0)) (maxima::$bfloat e) (maxima::$float e))))
	   (list e (abs e)))
	  
	  ((and (atom e) (maxima::mget e '$numer))
	   (running-error-eval (maxima::mget e 'maxima::$numer) '((mlist)) bits))

	  ((and (atom e) (get e 'maxima::sysconst))
	   (running-error-eval (maxima::$bfloat e) '((mlist)) bits))
	    
	  ((atom e) 
	   (setq e (maxima::$sublis subs e))
	   (if (maxima::complex-number-p e 'maxima::bigfloat-or-number-p)
	       (running-error-eval e nil bits)
	     (throw 'maxima::nfloat-nounform-return 'return-nounform)))

	  ;; Return a nounform for expressions (arrays, CRE expressions) that don't
	  ;; appear to be Maxima expressions of the form ((op) a1 a2 ...).
	  ((not (and (consp e) (consp (car e))))
	   (throw 'maxima::nfloat-nounform-return 'return-nounform))
	   
	  ;; Special case exp(x) (more efficient & accurate than sending this through mexpt).
	  ((and (eq 'maxima::mexpt (caar e)) (eq (second e) 'maxima::$%e))
	   (setq e (running-error-eval (third e) subs bits))
	   (let ((z (exp (first e))))
	     (list z (abs (* (second e) z)))))

	  ;; Special case x^n, where n is an integer. For this case, we do not want to 
	  ;; convert the integer to a float. This prevents some, but not all, semi-spurious
	  ;; nonzero imaginary parts for (negative real)^integer.
	  ((and (eq 'maxima::mexpt (caar e)) (integerp (third e)))
	   (running-error-expt (list (running-error-eval (second e) subs bits) (list (third e) 0))))
	 
	  ;; main function dispatch.
	  ((setq f (gethash (maxima::mop e) *running-error-op*))
	   ;(print `(e = ,e mop = ,(maxima::mop e)))
	   (setq e (mapcar #'(lambda (s) (running-error-eval s subs bits)) (maxima::margs e)))
	   (funcall f e))

	  ;; f(x + ex) = f(x) + ex * f'(x) + ... Functions without bigfloat 
	  ;; evaluation, for example the Bessel functions, need to be excluded.
	  ;; For now, this code rejects functions of two or more variables.
	  ((and (get (caar e) 'maxima::grad) (null (cdr (maxima::margs e)))
		(or (gethash (caar e) maxima::*big-float-op*) (maxima::trigp (caar e))
		    (maxima::arcp (caar e))))

	   (let ((x (running-error-eval (cadr e) subs bits)) (f) (df))
	     (setq f (maxima::take (list (caar e)) (maxima::to (first x))))
	     (setq df (get (caar e) 'maxima::grad))
	     (setq df (maxima::$rectform (maxima::$substitute f (caar df) (cadr df))))
	     (setq df (bigfloat::to df))
	     (list (bigfloat::to f) (* (second x) (abs df)))))

	  ;; special case hypergeometric
	  ((eq (caar e) 'maxima::$hypergeometric)
	   (running-error-hypergeometric (second e) (third e) (fourth e) subs bits))
	   
	  ;; special case sum.
	  ((or (eq (caar e) 'maxima::%sum) (eq (caar e) 'maxima::$sum))
	   (running-error-sum (cdr e) subs bits))

	  ;; special case product
	  ((or (eq (caar e) 'maxima::$product) (eq (caar e) 'maxima::%product))
	   (running-error-product (cdr e) subs bits))
	  
	  ;; special case assignment
	  ((eq (caar e) 'maxima::msetq)
	   (maxima::mset (car e) (car (running-error-eval (cadr e) subs bits))))
	  
	  ;; Yes, via nformat, this can happen. Try, for example, nfloat('(a,b),[a=3,b=7]).
	  ((eq (caar e) 'maxima::mprogn)
	   (let ((q))
	     (setq e (cdr e))
	     (dolist (ek e q)
	       (setq q (running-error-eval ek subs bits)))))
	  
	  (t (throw 'maxima::nfloat-nounform-return 'return-nounform)))))

;; d * eps is a upper bound for how much e differs from its true value, where eps is
;; the machine epsilon.

;; First (log = natural log)
;;
;;    log10(x) = log(x) / log(10) and log2(x) = log(x) / log(2).
;; So
;;    log10(x) = log2(x) * (log(2) / log(10)).
;;
;; Second
;;     -log10(abs(d * eps / e)) = log10(abs(e)) - log10(abs(d)) - log10(eps),
;;                        = (log2(abs(e)) - log2(abs(d)) - log2(eps)) * (log(2) / log(10).

;; For log2 we use the binary exponent of the number. Common Lisp gives
;; (decode-float 0.0) --> 0.0 0 1.0, by the way.

(defun log10-relative-error (d e)
 
  (if (rationalp d) (setq d (bigfloat (maxima::$bfloat (maxima::to d)))))
  (if (rationalp e) (setq e (bigfloat (maxima::$bfloat (maxima::to e)))))

  (floor (* 
	  (-
	   (second (multiple-value-list (decode-float (abs e))))
	   (+
	    (second (multiple-value-list (decode-float (abs d))))
	    (second (multiple-value-list (decode-float (epsilon (abs d)))))))
	  #.(/ (log 2.0) (log 10.0)))))
    
(defun not-done (err f eps machine-eps)
  (> (* machine-eps err) (* eps (max (abs f) 1))))

;;(defmethod epsilon ((x integer)) 0)

(in-package :maxima)
		
(defun nfloat (e subs digits max-digits)
  (let ((z (list nil nil)) (dig digits) (eps) (machine-epsilon nil))
    (cond ((or (mbagp e) (mrelationp e) ($setp e))
	   (simplify (cons (list (caar e))
			   (mapcar #'(lambda (s) (nfloat s subs digits max-digits)) (margs e)))))
	  
	  (t
	   (catch 'nfloat-nounform-return
	     (setq e (nformat e))
	     (setq eps (expt 10.0 (- digits)))
	     (setq eps (/ eps (- 1 eps)))
	     (while (and (or (null (first z)) (bigfloat::not-done (second z) (first z) eps machine-epsilon))
			 (< digits max-digits))
	       (bind-fpprec digits 
			    (setq z (bigfloat::running-error-eval e subs fpprec))
			    (setq machine-epsilon 
				  (cond ((not (second z)) nil)
					((integerp (second z)) 0)
					(t (bigfloat::epsilon (second z)))))
					
			    (setq digits (* 2 digits))))
	     
	     (if (or (null (first z)) (>= digits max-digits))
		 (merror "Unable to evaluate to requested number of digits")
	       (maxima::bind-fpprec dig (values (maxima::to (first z)) (maxima::to (second z))))))))))

(setf (get '$nfloat 'operators) 'simp-nfloat)

(defun simp-nfloat (x yy z)
  (declare (ignore yy))
  (declare (special $max_fpprec))
  (pop x) ;; remove ($nfloat)
  (let*  ((e (if x (simpcheck (pop x) z) (wna-err '$nfloat)))
	  (subs (if x (simpcheck (pop x) z) (take '(mlist))))
	  (digits (if x (simpcheck (pop x) z) $fpprec))
	  (max-digits (if x (simpcheck (pop x) z) $max_fpprec))
	  (f))

    (cond ((and ($listp subs)
		(every #'(lambda (s) (and (mequalp s) (symbolp ($lhs s)) (complex-number-p ($rhs s) 'mnump)))
		       (cdr subs)))
	   (cond ((or (mbagp e) (mrelationp e) ($setp e))
		  (simplify (cons (list (caar e))
				  (mapcar #'(lambda (s) (take '($nfloat) s subs digits max-digits))
					  (margs e)))))
		 (t
		  (setq f (nfloat e subs digits max-digits))
		  (if (complex-number-p f 'bigfloat-or-number-p) f 
		    `(($nfloat simp) ,e ,subs ,digits ,$max_fpprec)))))
	  (t  `(($nfloat simp) ,e ,subs ,digits ,$max_fpprec))))) 