;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module trigi)

(load-macsyma-macros mrgmac)

(declare-top (special varlist errorsw $demoivre 1//2 -1//2))

(defmvar $%piargs t)
(defmvar $%iargs t)
(defmvar $triginverses t)
(defmvar $trigexpand nil)
(defmvar $trigexpandplus t)
(defmvar $trigexpandtimes t)
(defmvar $trigsign t)
(defmvar $exponentialize nil)
(defmvar $logarc nil)
(defmvar $halfangles nil)

;; Simplified shortcuts for constant expressions.
(defvar %pi//4 '((mtimes simp) ((rat simp) 1 4.) $%pi))
(defvar %pi//2 '((mtimes simp) ((rat simp) 1 2) $%pi))
(defvar sqrt3//2 '((mtimes simp)
                   ((rat simp) 1 2) 
                   ((mexpt simp) 3 ((rat simp) 1 2))))
(defvar -sqrt3//2 '((mtimes simp)
                    ((rat simp) -1 2)
                    ((mexpt simp) 3 ((rat simp) 1 2))))

;;; Arithmetic utilities.

(defmfun sqrt1-x^2 (x)
  (power (sub 1 (power x 2)) 1//2))

(defmfun sqrt1+x^2 (x)
  (power (add 1 (power x 2)) 1//2))

(defmfun sqrtx^2-1 (x)
  (power (add (power x 2) -1) 1//2))

(defmfun sq-sumsq (x y)
  (power (add (power x 2) (power y 2)) 1//2))

(defmfun trigp (func)
  (member func '(%sin %cos %tan %csc %sec %cot %sinh %cosh %tanh %csch %sech %coth)
	  :test #'eq))

(defmfun arcp (func)
  (member func '(%asin %acos %atan %acsc %asec %acot %asinh %acosh %atanh %acsch %asech %acoth)
	  :test #'eq))

(defprop %sin simp-%sin operators)
(defprop %cos simp-%cos operators)
(defprop %tan simp-%tan operators)
(defprop %cot simp-%cot operators)
(defprop %csc simp-%csc operators)
(defprop %sec simp-%sec operators)
(defprop %sinh simp-%sinh operators)
(defprop %cosh simp-%cosh operators)
(defprop %tanh simp-%tanh operators)
(defprop %coth simp-%coth operators)
(defprop %csch simp-%csch operators)
(defprop %sech simp-%sech operators)
(defprop %asin simp-%asin operators)
(defprop %acos simp-%acos operators)
(defprop %atan simp-%atan operators)
(defprop %acot simp-%acot operators)
(defprop %acsc simp-%acsc operators)
(defprop %asec simp-%asec operators)
(defprop %asinh simp-%asinh operators)
(defprop %acosh simp-%acosh operators)
(defprop %atanh simp-%atanh operators)
(defprop %acoth simp-%acoth operators)
(defprop %acsch simp-%acsch operators)
(defprop %asech simp-%asech operators)

;;; The trigonometric functions distribute of lists, matrices and equations.

(dolist (x '(%sin   %cos   %tan   %cot   %csc   %sec
             %sinh  %cosh  %tanh  %coth  %csch  %sech
             %asin  %acos  %atan  %acot  %acsc  %asec
             %asinh %acosh %atanh %acoth %acsch %asech))
  (setf (get x 'distribute_over) '(mlist $matrix mequal)))

(defun domain-error (x f)
  (merror (intl:gettext "~A: argument ~:M isn't in the domain of ~A.") f (complexify x) f))

;; Build a hash table 'cl-flonum-op' that maps Maxima function names 
;; to their CL equivalents. 

(defvar *flonum-op* (make-hash-table :size 64)
  "Hash table mapping a maxima function to a corresponding Lisp
  function to evaluate the maxima function numerically with
  flonum precision.")

(defvar *big-float-op* (make-hash-table)
  "Hash table mapping a maxima function to a corresponding Lisp
  function to evaluate the maxima function numerically with
  big-float precision.")
  
;; Some Lisp implementations goof up branch cuts for ASIN, ACOS, and/or ATANH.
;; Here are definitions which have the right branch cuts
;; (assuming LOG, PHASE, and SQRT have the right branch cuts).
;; Don't bother trying to sort out which implementations get it right or wrong;
;; we'll make all implementations use these functions.

;; Apply formula from CLHS if X falls on a branch cut.
;; Otherwise punt to CL:ASIN.
(defun maxima-branch-asin (x)
  ;; Test for (IMAGPART X) is EQUAL because signed zero is EQUAL to zero.
  (if (and (> (abs (realpart x)) 1.0) (equal (imagpart x) 0.0))
      ;; The formula from CLHS is asin(x) = -%i*log(%i*x+sqrt(1-x^2)).
      ;; This has problems with overflow for large x.
      ;;
      ;; Let's rewrite it, where abs(x)>1
      ;;
      ;; asin(x) = -%i*log(%i*x+abs(x)*sqrt(1-1/x^2))
      ;;         = -%i*log(%i*x*(1+abs(x)/x*sqrt(1-1/x^2)))
      ;;         = -%i*[log(abs(x)*abs(1+abs(x)/x*sqrt(1-1/x^2)))
      ;;                 + %i*arg(%i*x*(1+abs(x)/x*sqrt(1-1/x^2)))]
      ;;         = -%i*[log(abs(x)*(1+abs(x)/x*sqrt(1-1/x^2)))
      ;;                 + %i*%pi/2*sign(x)]
      ;;         = %pi/2*sign(x) - %i*[log(abs(x)*(1+abs(x)/x*sqrt(1-1/x^2))]
      ;;
      ;; Now, look at log part.  If x > 0, we have
      ;;
      ;;    log(x*(1+sqrt(1-1/x^2)))
      ;;
      ;; which is just fine.  For x < 0, we have
      ;;
      ;;    log(abs(x)*(1-sqrt(1-1/x^2))).
      ;;
      ;; But
      ;;    1-sqrt(1-1/x^2) = (1-sqrt(1-1/x^2))*(1+sqrt(1-1/x^2))/(1+sqrt(1-1/x^2))
      ;;                    = (1-(1-1/x^2))/(1+sqrt(1-1/x^2))
      ;;                    = 1/x^2/(1+sqrt(1-1/x^2))
      ;;
      ;; So
      ;;
      ;;    log(abs(x)*(1-sqrt(1-1/x^2)))
      ;;        = log(abs(x)/x^2/(1+sqrt(1-1/x^2)))
      ;;        = -log(x^2/abs(x)*(1+sqrt(1-1/x^2))
      ;;        = -log(abs(x)*(1+sqrt(1-1/x^2)))
      ;;
      ;; Thus, for x < 0,
      ;;
      ;; asin(x) = -%pi/2+%i*log(abs(x)*(1+sqrt(1-1/x^2)))
      ;;         = -asin(-x)
      ;;
      ;; If we had an accurate f(x) = log(1+x) function, we should
      ;; probably evaluate log(1+sqrt(1-1/x^2)) via f(x) instead of
      ;; log.  One other accuracy change is to evaluate sqrt(1-1/x^2)
      ;; as sqrt(1-1/x)*sqrt(1+1/x), because 1/x^2 won't underflow as
      ;; soon as 1/x.
      (let* ((absx (abs x))
	     (recip (/ absx))
	     (result (complex (/ #.(float pi) 2)
			      (- (log (* absx
					 (1+ (* (sqrt (+ 1 recip))
						(sqrt (- 1 recip))))))))))
	(if (minusp x)
	    (- result)
	    result))
      (cl:asin x)))

;; Apply formula from CLHS if X falls on a branch cut.
;; Otherwise punt to CL:ACOS.
(defun maxima-branch-acos (x)
  ; Test for (IMAGPART X) is EQUAL because signed zero is EQUAL to zero.
  (if (and (> (abs (realpart x)) 1.0) (equal (imagpart x) 0.0))
    (- #.(/ (float pi) 2) (maxima-branch-asin x))
    (cl:acos x)))

(defun maxima-branch-acot (x)
  ;; Allow 0.0 in domain of acot, otherwise use atan(1/x)
  (if (and (equal (realpart x) 0.0) (equal (imagpart x) 0.0))
    #.(/ (float pi) 2)
    (cl:atan (/ 1 x))))

;; Apply formula from CLHS if X falls on a branch cut.
;; Otherwise punt to CL:ATANH.
(defun maxima-branch-atanh (x)
  ; Test for (IMAGPART X) is EQUAL because signed zero is EQUAL to zero.
  (if (and (> (abs (realpart x)) 1.0) (equal (imagpart x) 0.0))
    (/ (- (cl:log (+ 1 x)) (cl:log (- 1 x))) 2)
    (cl:atanh x)))

;; Fill the hash table.
(macrolet ((frob (mfun dfun) `(setf (gethash ',mfun *flonum-op*) ,dfun)))
  (frob mplus #'+)
  (frob mtimes #'*)
  (frob mquotient #'/)
  (frob mminus #'-)

  (frob %cos #'cl:cos)
  (frob %sin #'cl:sin)
  (frob %tan #'cl:tan)

  (frob %sec #'(lambda (x)
		 (let ((y (ignore-errors (/ 1 (cl:cos x)))))
		   (if y y (domain-error x 'sec)))))
				   
  (frob %csc #'(lambda (x)
		 (let ((y (ignore-errors (/ 1 (cl:sin x)))))
		   (if y y (domain-error x 'csc)))))

  (frob %cot #'(lambda (x)
		 (let ((y (ignore-errors (/ 1 (cl:tan x)))))
		   (if y y (domain-error x 'cot)))))

  (frob %acos #'maxima-branch-acos)
  (frob %asin #'maxima-branch-asin)

  (frob %atan #'cl:atan)

  (frob %asec #'(lambda (x)
		  (let ((y (ignore-errors (maxima-branch-acos (/ 1 x))))) 
		    (if y y (domain-error x 'asec)))))

  (frob %acsc #'(lambda (x)
		  (let ((y (ignore-errors (maxima-branch-asin (/ 1 x)))))
		    (if y y (domain-error x 'acsc)))))

  (frob %acot #'(lambda (x)
		  (let ((y (ignore-errors (maxima-branch-acot x))))
		    (if y y (domain-error x 'acot)))))

  (frob %cosh #'cl:cosh)
  (frob %sinh #'cl:sinh)
  (frob %tanh #'cl:tanh)

  (frob %sech #'(lambda (x)
		  (let ((y (ignore-errors (/ 1 (cl:cosh x)))))
		    (if y y (domain-error x 'sech)))))

  (frob %csch #'(lambda (x)
		  (let ((y (ignore-errors (/ 1 (cl:sinh x)))))
		    (if y y (domain-error x 'csch)))))

  (frob %coth #'(lambda (x)
		  (let ((y (ignore-errors (/ 1 (cl:tanh x)))))
		    (if y y (domain-error x 'coth)))))

  (frob %acosh #'cl:acosh)
  (frob %asinh #'cl:asinh)
  
  (frob %atanh #'maxima-branch-atanh)

  (frob %asech #'(lambda (x)
		   (let ((y (ignore-errors (cl:acosh (/ 1 x)))))
		     (if y y (domain-error x 'asech)))))

  (frob %acsch #'(lambda (x)
		   (let ((y (ignore-errors (cl:asinh (/ 1 x)))))
		     (if y y (domain-error x 'acsch)))))

  (frob %acoth #'(lambda (x)
		   (let ((y (ignore-errors (maxima-branch-atanh (/ 1 x))))) 
		     (if y y (domain-error x 'acoth)))))

  (frob mabs #'cl:abs)
  (frob %exp #'cl:exp)
  (frob mexpt #'cl:expt)
  (frob %sqrt #'cl:sqrt)
  (frob %log #'(lambda (x)
		 (let ((y (ignore-errors (cl:log x))))
		   (if y y (domain-error x 'log)))))

  (frob %plog #'(lambda (x)
		  (let ((y (ignore-errors (cl:log x))))
		    (if y y (domain-error x 'log)))))

  (frob $conjugate #'cl:conjugate)
  (frob $floor #'cl:ffloor)
  (frob $ceiling #'cl:fceiling)
  (frob $realpart #'cl:realpart)
  (frob $imagpart #'cl:imagpart)
  (frob $max #'cl:max)
  (frob $min #'cl:min)
  (frob %signum #'cl:signum)
  (frob $atan2 #'cl:atan)
  (frob %log #'(lambda (x)
		 (let ((y (ignore-errors (cl:log x))))
		   (if y y (domain-error x 'log)))))
  (frob %sqrt #'cl:sqrt))

(macrolet ((frob (mfun dfun) `(setf (gethash ',mfun *big-float-op*) ,dfun)))
  ;; All big-float implementation functions MUST support a required x
  ;; arg and an optional y arg for the real and imaginary parts.  The
  ;; imaginary part does not have to be given.
  (frob %asin #'big-float-asin)
  (frob %sinh #'big-float-sinh)
  (frob %asinh #'big-float-asinh)
  (frob %tanh #'big-float-tanh)
  (frob %atanh #'big-float-atanh)
  (frob %acos 'big-float-acos)
  (frob %log 'big-float-log)
  (frob %sqrt 'big-float-sqrt))

;; Here is a general scheme for defining and applying reflection rules. A 
;; reflection rule is something like f(-x) --> f(x), or  f(-x) --> %pi - f(x). 

;; We define functions for the two most common reflection rules; these
;; are the odd function rule (f(-x) --> -f(x)) and the even function rule
;; (f(-x) --> f(x)). A reflection rule takes two arguments (the operator and 
;; the operand).

(defun odd-function-reflect (op x)
  (neg (take (list op) (neg x))))

(defun even-function-reflect (op x)
  (take (list op) (neg x)))

;; Put the reflection rule on the property list of the exponential-like
;; functions.

(setf (get '%cos 'reflection-rule) #'even-function-reflect)
(setf (get '%sin 'reflection-rule) #'odd-function-reflect)
(setf (get '%tan 'reflection-rule) #'odd-function-reflect)
(setf (get '%sec 'reflection-rule) #'even-function-reflect)
(setf (get '%csc 'reflection-rule) #'odd-function-reflect)
(setf (get '%cot 'reflection-rule) #'odd-function-reflect)

;; See A&S 4.4.14--4.4.19

(setf (get '%acos 'reflection-rule) #'(lambda (op x) (sub '$%pi (take (list op) (neg x)))))
(setf (get '%asin 'reflection-rule) #'odd-function-reflect)
(setf (get '%atan 'reflection-rule) #'odd-function-reflect)
(setf (get '%asec 'reflection-rule) #'(lambda (op x) (sub '$%pi (take (list op) (neg x)))))
(setf (get '%acsc 'reflection-rule) #'odd-function-reflect)
(setf (get '%acot 'reflection-rule) #'odd-function-reflect)

(setf (get '%cosh 'reflection-rule) #'even-function-reflect)
(setf (get '%sinh 'reflection-rule) #'odd-function-reflect)
(setf (get '%tanh 'reflection-rule) #'odd-function-reflect)
(setf (get '%sech 'reflection-rule) #'even-function-reflect)
(setf (get '%csch 'reflection-rule) #'odd-function-reflect)
(setf (get '%coth 'reflection-rule) #'odd-function-reflect)

(setf (get '%asinh 'reflection-rule) #'odd-function-reflect)
(setf (get '%atanh 'reflection-rule) #'odd-function-reflect)
(setf (get '%asech 'reflection-rule) #'even-function-reflect)
(setf (get '%acsch 'reflection-rule) #'odd-function-reflect)
(setf (get '%acoth 'reflection-rule) #'even-function-reflect)

;; When b is nil, do not apply the reflection rule. For trigonometric like
;; functions, b is $trigsign.  This function uses 'great' to decide when to
;; apply the rule.  Another possibility is to apply the rule when (mminusp* x)
;; evaluates to true. Maxima <= 5.9.3 uses this scheme; with this method, we have 
;; assume(z < 0), cos(z) --> cos(-z). I (Barton Willis) think this goofy.

;; The function 'great' is non-transitive. I don't think this bug will cause
;; trouble for this function. If there is an expression such that both
;; (great (neg x) x) and (great x (neg x)) evaluate to true, this function
;; could cause an infinite loop. I could protect against this possibility with 
;; (and b f (great (neg x) x) (not (great x (neg x))).

(defun apply-reflection-simp (op x &optional (b t))
  (let ((f (get op 'reflection-rule)))
    (if (and b f (great (neg x) x)) (funcall f op x) nil)))
  
(defun taylorize (op x)
  (if ($taylorp x)
      (mfuncall '$apply '$taylor `((mlist) ((,op) ,($ratdisrep x)) ,@(cdr ($taylorinfo x)))) nil))

(defun float-or-rational-p (x)
  (or (floatp x) ($ratnump x)))

(defun bigfloat-or-number-p (x)
  (or ($bfloatp x) (numberp x) ($ratnump x)))

;; When z is a Maxima complex float or when 'numer' is true and z is a
;; Maxima complex number, evaluate (op z) by applying the mapping from
;; the Maxima operator 'op' to the operator in the hash table
;; 'flonum-op'. When z isn't a Maxima complex number, return
;; nil.

(defun flonum-eval (op z)
  (let ((op (gethash op *flonum-op*)))
    (when (and op (complex-number-p z 'float-or-rational-p))
      (let ((x ($realpart z)) (y ($imagpart z)))
	(when (or $numer (floatp x) (floatp y))
	  (setq x ($float x))
	  (setq y ($float y))
	  (complexify (funcall op (if (zerop y) x (complex x y)))))))))

;; For now, big float evaluation of trig-like functions for complex
;; big floats uses rectform.  I suspect that for some functions (not
;; all of them) rectform generates expressions that are poorly suited
;; for numerical evaluation. For better accuracy, these functions
;; (maybe acosh, for one) may need to be special cased.  If they are
;; special-cased, the *big-float-op* hash table contains the special
;; cases.

(defun big-float-eval (op z)
  (when (complex-number-p z 'bigfloat-or-number-p)
    (let ((x ($realpart z))
	  (y ($imagpart z))
	  (bop (gethash op *big-float-op*)))
      ;; If bop is non-NIL, we want to try that first.  If bop
      ;; declines (by returning NIL), we silently give up and use the
      ;; rectform version.
      (cond ((and ($bfloatp x) (like 0 y))
	     (or (and bop (funcall bop x))
		 ($bfloat `((,op simp) ,x))))
	    ((or ($bfloatp x) ($bfloatp y))
	     (or (and bop (funcall bop ($bfloat x) ($bfloat y)))
		 (let ((z (add ($bfloat x) (mul '$%i ($bfloat y)))))
		   (setq z ($rectform `((,op simp) ,z)))
		   ($bfloat z))))))))
	 
;; For complex big float evaluation, it's important to check the 
;; simp flag -- otherwise Maxima can get stuck in an infinite loop:
;; asin(1.23b0 + %i * 4.56b0) ---> (simp-%asin ((%asin) ...) -->
;; (big-float-eval ((%asin) ...) --> (risplit ((%asin simp) ...) -->
;; (simp-%asin ((%asin simp) ...). If the simp flag is ignored, we've
;; got trouble.

(defmfun simp-%sin (form y z) 
  (oneargcheck form)
  (setq y (simpcheck (cadr form) z))
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (cond ((zerop1 y) 0)
			     ((has-const-or-int-term y '$%pi) (%piargs-sin/cos y)))))
	((and $%iargs (multiplep y '$%i)) (mul '$%i (cons-exp '%sinh (coeff y '$%i 1))))
	((and $triginverses (not (atom y))
	      (cond ((eq '%asin (setq z (caar y))) (cadr y))
		    ((eq '%acos z) (sqrt1-x^2 (cadr y)))
		    ((eq '%atan z) (div (cadr y) (sqrt1+x^2 (cadr y))))
		    ((eq '%acot z) (div 1 (sqrt1+x^2 (cadr y))))
		    ((eq '%asec z) (div (sqrtx^2-1 (cadr y)) (cadr y)))
		    ((eq '%acsc z) (div 1 (cadr y)))
		    ((eq '$atan2 z) (div (cadr y) (sq-sumsq (cadr y) (caddr y)))))))
	((and $trigexpand (trigexpand '%sin y)))
	($exponentialize (exponentialize '%sin y))
	((and $halfangles (halfangle '%sin y)))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (cons-exp '%sin (neg y))))
	(t (eqtest (list '(%sin) y) form))))

(defmfun simp-%cos (form y z) 
  (oneargcheck form)
  (setq y (simpcheck (cadr form) z))
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (cond ((zerop1 y) 1)
			     ((has-const-or-int-term y '$%pi)
			      (%piargs-sin/cos (add %pi//2 y))))))
	((and $%iargs (multiplep y '$%i)) (cons-exp '%cosh (coeff y '$%i 1)))
	((and $triginverses (not (atom y))
	      (cond ((eq '%acos (setq z (caar y))) (cadr y))
		    ((eq '%asin z) (sqrt1-x^2 (cadr y)))
		    ((eq '%atan z) (div 1 (sqrt1+x^2 (cadr y))))
		    ((eq '%acot z) (div (cadr y) (sqrt1+x^2 (cadr y))))
		    ((eq '%asec z) (div 1 (cadr y)))
		    ((eq '%acsc z) (div (sqrtx^2-1 (cadr y)) (cadr y)))
		    ((eq '$atan2 z) (div (caddr y) (sq-sumsq (cadr y) (caddr y)))))))
	((and $trigexpand (trigexpand '%cos y)))
	($exponentialize (exponentialize '%cos y))
	((and $halfangles (halfangle '%cos y)))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (cons-exp '%cos (neg y)))
	(t (eqtest (list '(%cos) y) form))))

(defun %piargs-sin/cos (x)
  (let ($float coeff ratcoeff zl-rem)
    (setq ratcoeff (get-const-or-int-terms x '$%pi)
	  coeff (linearize ratcoeff)
	  zl-rem (get-not-const-or-int-terms x '$%pi))
    (cond ((zerop1 zl-rem) (%piargs coeff ratcoeff))
	  ((not (mevenp (car coeff))) nil)
	  ((equal 0 (setq x (mmod (cdr coeff) 2))) (cons-exp '%sin zl-rem))
	  ((equal 1 x) (neg (cons-exp '%sin zl-rem)))
	  ((alike1 1//2 x) (cons-exp '%cos zl-rem))
	  ((alike1 '((rat) 3 2) x) (neg (cons-exp '%cos zl-rem))))))


(defun filter-sum (pred form simp-flag)
  "Takes form to be a sum and a sum of the summands for which pred is
   true. Passes simp-flag through to addn if there is more than one
   term in the sum."
  (if (mplusp form)
      (addn (mapcan
	     #'(lambda (term)
		 (when (funcall pred term) (list term))) (cdr form))
	    simp-flag)
    (if (funcall pred form) form 0)))

;; collect terms of form A*var where A is a constant or integer.
;; returns sum of all such A.
;; does not expand form, so does not find constant term in (x+1)*var.
;; thus we cannot simplify sin(2*%pi*(1+x)) => sin(2*%pi*x) unless
;;  the user calls expand.  this could be extended to look a little
;;  more deeply into the expression, but we don't want to call expand
;;  in the core simplifier for reasons of speed and predictability.
(defun get-const-or-int-terms (form var)
  (coeff 
   (filter-sum (lambda (term)
		 (let ((coeff (coeff term var 1)))
		   (and (not (zerop1 coeff))
			(or ($constantp coeff)
			    (maxima-integerp coeff)))))
	       form
	       0)
   var 1))

;; collect terms skipped by get-const-or-int-terms
(defun get-not-const-or-int-terms (form var)
  (filter-sum (lambda (term)
		(let ((coeff (coeff term var 1)))
		  (not (and (not (zerop1 coeff))
			    (or ($constantp coeff)
				(maxima-integerp coeff))))))
	      form
	      0))

(defun has-const-or-int-term (form var)
  "Tests whether form has at least some term of the form a*var where a
  is constant or integer"
  (not (zerop1 (get-const-or-int-terms form var))))

(defmfun simp-%tan (form y z)
  (oneargcheck form)
  (setq y (simpcheck (cadr form) z))
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (cond ((zerop1 y) 0)
			     ((has-const-or-int-term y '$%pi) (%piargs-tan/cot y)))))
	((and $%iargs (multiplep y '$%i)) (mul '$%i (cons-exp '%tanh (coeff y '$%i 1))))
	((and $triginverses (not (atom y))
	      (cond ((eq '%atan (setq z (caar y))) (cadr y))
		    ((eq '%asin z) (div (cadr y) (sqrt1-x^2 (cadr y))))
		    ((eq '%acos z) (div (sqrt1-x^2 (cadr y)) (cadr y)))
		    ((eq '%acot z) (div 1 (cadr y)))
		    ((eq '%asec z) (sqrtx^2-1 (cadr y)))
		    ((eq '%acsc z) (div 1 (sqrtx^2-1 (cadr y))))
		    ((eq '$atan2 z) (div (cadr y) (caddr y))))))
	((and $trigexpand (trigexpand '%tan y)))
	($exponentialize (exponentialize '%tan y))
	((and $halfangles (halfangle '%tan y)))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (cons-exp '%tan (neg y))))
	(t (eqtest (list '(%tan) y) form))))

(defmfun simp-%cot (form y z)
  (oneargcheck form)
  (setq y (simpcheck (cadr form) z))
  
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (cond ((zerop1 y) (domain-error y 'cot))
			     ((and (has-const-or-int-term y '$%pi)
				   (setq z (%piargs-tan/cot (add %pi//2 y))))
			      (neg z)))))
	((and $%iargs (multiplep y '$%i)) (mul -1 '$%i (cons-exp '%coth (coeff y '$%i 1))))
	((and $triginverses (not (atom y))
	      (cond ((eq '%acot (setq z (caar y))) (cadr y))
		    ((eq '%asin z) (div (sqrt1-x^2 (cadr y)) (cadr y)))
		    ((eq '%acos z) (div (cadr y) (sqrt1-x^2 (cadr y))))
		    ((eq '%atan z) (div 1 (cadr y)))
		    ((eq '%asec z) (div 1 (sqrtx^2-1 (cadr y))))
		    ((eq '%acsc z) (sqrtx^2-1 (cadr y)))
		    ((eq '$atan2 z) (div (caddr y) (cadr y))))))
	((and $trigexpand (trigexpand '%cot y)))
	($exponentialize (exponentialize '%cot y))
	((and $halfangles (halfangle '%cot y)))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (cons-exp '%cot (neg y))))
	(t (eqtest (list '(%cot) y) form))))

(defun %piargs-tan/cot (x)
  "If x is of the form tan(u) where u has a nonzero constant linear
   term in %pi, then %piargs-tan/cot returns a simplified version of x
   without this constant term."
  ;; Set coeff to be the coefficient of $%pi collecting terms with no
  ;; other atoms, so given %pi(x+1/2), coeff = 1/2. Let zl-rem be the
  ;; remainder (TODO: computing zl-rem could probably be prettier.)
  (let* ((nice-terms (get-const-or-int-terms x '$%pi))
	 (coeff (linearize nice-terms))
	 (zl-rem (get-not-const-or-int-terms x '$%pi))
	 (sin-of-coeff-pi)
	 (cos-of-coeff-pi))
    (cond
     ;; sin-of-coeff-pi and cos-of-coeff-pi are only non-nil if they
     ;; are constants that %piargs-offset could compute, and we just
     ;; checked that cos-of-coeff-pi was nonzero. Thus we can just
     ;; return their quotient.
     ((and (zerop1 zl-rem)
	   (setq sin-of-coeff-pi
		 (%piargs coeff nil)))
      (setq cos-of-coeff-pi
	    (%piargs (cons (car coeff)
			   (rplus 1//2 (cdr coeff))) nil))
      (cond ((zerop1 sin-of-coeff-pi) 
	     0)		;; tan(integer*%pi)
	    ((zerop1 cos-of-coeff-pi)
	     (merror (intl:gettext "tan: ~M isn't in the domain of tan.") x))
	    (cos-of-coeff-pi
	     (div sin-of-coeff-pi cos-of-coeff-pi))))
       
     ;; Need period of 2*%pi to continue
     ((not (mevenp (car coeff))) nil)
 
     ;; This expression sets x to the coeff of %pi (mod 2) as a side
     ;; effect and then, if this is an integer, returns tan of the
     ;; rest.
     ((integerp (setq x (mmod (cdr coeff) 2)))
      (cons-exp '%tan zl-rem))
 
     ;; Similarly, if x = 1/2 or 3/2 then return -cot(x).
     ((or (alike1 1//2 x)
	  (alike1 '((rat) 3 2) x))
        (neg (cons-exp '%cot zl-rem))))))

(defmfun simp-%csc (form y z)
  (oneargcheck form)
  (setq y (simpcheck (cadr form) z))
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (cond ((zerop1 y) (domain-error y 'csc))
			     ((has-const-or-int-term y '$%pi) (%piargs-csc/sec y)))))
	((and $%iargs (multiplep y '$%i)) (mul -1 '$%i (cons-exp '%csch (coeff y '$%i 1))))
	((and $triginverses (not (atom y))
	      (cond ((eq '%acsc (setq z (caar y))) (cadr y))
		    ((eq '%asin z) (div 1 (cadr y)))
		    ((eq '%acos z) (div 1 (sqrt1-x^2 (cadr y))))
		    ((eq '%atan z) (div (sqrt1+x^2 (cadr y)) (cadr y)))
		    ((eq '%acot z) (sqrt1+x^2 (cadr y)))
		    ((eq '%asec z) (div (cadr y) (sqrtx^2-1 (cadr y))))
		    ((eq '$atan2 z) (div (sq-sumsq (cadr y) (caddr y)) (cadr y))))))
	((and $trigexpand (trigexpand '%csc y)))
	($exponentialize (exponentialize '%csc y))
	((and $halfangles (halfangle '%csc y)))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (neg (cons-exp '%csc (neg y))))

	(t (eqtest (list '(%csc) y) form))))

(defmfun simp-%sec (form y z)
  (oneargcheck form)
  (setq y (simpcheck (cadr form) z))
  (cond ((flonum-eval (mop form) y))
	((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
	((taylorize (mop form) (second form)))
	((and $%piargs (cond ((zerop1 y) 1)
			     ((has-const-or-int-term y '$%pi) (%piargs-csc/sec (add %pi//2 y))))))
	((and $%iargs (multiplep y '$%i)) (cons-exp '%sech (coeff y '$%i 1)))
	((and $triginverses (not (atom y))
	      (cond ((eq '%asec (setq z (caar y))) (cadr y))
		    ((eq '%asin z) (div 1 (sqrt1-x^2 (cadr y))))
		    ((eq '%acos z) (div 1 (cadr y)))
		    ((eq '%atan z) (sqrt1+x^2 (cadr y)))
		    ((eq '%acot z) (div (sqrt1+x^2 (cadr y)) (cadr y)))
		    ((eq '%acsc z) (div (cadr y) (sqrtx^2-1 (cadr y))))
		    ((eq '$atan2 z) (div (sq-sumsq (cadr y) (caddr y)) (caddr y))))))
	((and $trigexpand (trigexpand '%sec y)))
	($exponentialize (exponentialize '%sec y))
	((and $halfangles (halfangle '%sec y)))
	((apply-reflection-simp (mop form) y $trigsign))
	;((and $trigsign (mminusp* y)) (cons-exp '%sec (neg y)))
	
	(t (eqtest (list '(%sec) y) form))))

(defun %piargs-csc/sec (x)
  (prog ($float coeff ratcoeff zl-rem)
     (setq ratcoeff (get-const-or-int-terms x '$%pi)
	   coeff (linearize ratcoeff)
	   zl-rem (get-not-const-or-int-terms x '$%pi))
     (return (cond ((and (zerop1 zl-rem) (setq zl-rem (%piargs coeff nil))) (div 1 zl-rem))
		   ((not (mevenp (car coeff))) nil)
		   ((equal 0 (setq x (mmod (cdr coeff) 2))) (cons-exp '%csc zl-rem))
		   ((equal 1 x) (neg (cons-exp '%csc zl-rem)))
		   ((alike1 1//2 x) (cons-exp '%sec zl-rem))
		   ((alike1 '((rat) 3 2) x) (neg (cons-exp '%sec zl-rem)))))))

(defun simp-%atan (form y z)
  (oneargcheck form)
  (setq y (simpcheck (cadr form) z))
  (cond ((flonum-eval (mop form) y))
        ((and (not (member 'simp (car form))) (big-float-eval (mop form) y)))
        ((taylorize (mop form) (second form)))
        ;; Simplification for special values
        ((zerop1 y) y)
        ((or (eq y '$inf) (alike1 y '((mtimes) -1 $minf)))
         (div '$%pi 2))
        ((or (eq y '$minf) (alike1 y '((mtimes) -1 $inf)))
         (div '$%pi -2))
        ((and $%piargs
              ;; Recognize more special values
              (cond ((equal 1 y) (div '$%pi 4))
                    ((equal -1 y) (div '$%pi -4))
                    ;; sqrt(3)
                    ((alike1 y '((mexpt) 3 ((rat) 1 2)))
                     (div '$%pi 3))
                    ;; -sqrt(3)
                    ((alike1 y '((mtimes) -1 ((mexpt) 3 ((rat) 1 2))))
                     (div '$%pi -3))
                    ;; 1/sqrt(3)
                    ((alike1 y '((mexpt) 3 ((rat) -1 2)))
                     (div '$%pi 6))
                    ;; -1/sqrt(3)
                    ((alike1 y '((mtimes) -1 ((mexpt) 3 ((rat) -1 2))))
                     (div '$%pi -6))
                    ((alike1 y '((mplus) -1 ((mexpt) 2 ((rat) 1 2))))
                     (div '$%pi 8))
                    ((alike1 y '((mplus) 1 ((mexpt) 2 ((rat) 1 2))))
                     (mul 3 (div '$%pi 8))))))
        ((and $%iargs (multiplep y '$%i))
         ;; atan(%i*y) -> %i*atanh(y)
         (mul '$%i (take '(%atanh) (coeff y '$%i 1))))
	((and (not (atom y)) (member (caar y) '(%cot %tan))
	      (if ($constantp (cadr y))
		  (let ((y-val (mfuncall '$mod 
					 (if (eq (caar y) '%tan)
					     (cadr y)
					     (sub %pi//2 (cadr y)))
					 '$%pi)))
		    (cond ((eq (mlsp y-val %pi//2) t) y-val)
			  ((eq (mlsp y-val '$%pi) t) (sub y-val '$%pi)))))))
	((and (eq $triginverses '$all) (not (atom y))
	      (if (eq (caar y) '%tan) (cadr y))))
	((and (eq $triginverses t) (not (atom y)) (eq (caar y) '%tan)
	      ;; Check if y in [-%pi/2, %pi/2]
	      (if (and (member (csign (sub (cadr y) %pi//2)) '($nz $neg) :test #'eq)
		       (member (csign (add (cadr y) %pi//2)) '($pz $pos) :test #'eq))
		  (cadr y))))
	($logarc (logarc '%atan y))
	((apply-reflection-simp (mop form) y $trigsign))
	(t (eqtest (list '(%atan) y) form))))

(defun %piargs (x ratcoeff)
  (let (offset-result)
    (cond ((and (integerp (car x)) (integerp (cdr x))) 0)
	  ((not (mevenp (car x))) 
	   (cond ((null ratcoeff) nil)
		 ((and (integerp (car x)) 
		       (setq offset-result (%piargs-offset (cdr x))))
		  (mul (power -1 (sub ratcoeff (cdr x)))
		       offset-result))))
	  ((%piargs-offset (mmod (cdr x) 2))))))

; simplifies sin(%pi * x) where x is between 0 and 1 
; returns nil if can't simplify
(defun %piargs-offset (x)
  (cond ((or (alike1 '((rat) 1 6) x) (alike1 '((rat) 5 6) x)) 1//2)
	((or (alike1 '((rat) 1 4) x) (alike1 '((rat) 3 4) x)) (div (power 2 1//2) 2))
	((or (alike1 '((rat) 1 3) x) (alike1 '((rat) 2 3) x)) (div (power 3 1//2) 2))
	((alike1 1//2 x) 1)
	((or (alike1 '((rat) 7 6) x) (alike1 '((rat) 11 6) x)) -1//2)
	((or (alike1 '((rat) 4 3) x) (alike1 '((rat) 5 3) x)) (div (power 3 1//2) -2))
	((or (alike1 '((rat) 5 4) x) (alike1 '((rat) 7 4) x)) (mul -1//2 (power 2 1//2)))
	((alike1 '((rat) 3 2) x) -1)))

;; identifies integer part of form
;; returns (X . Y) if form can be written as X*some_integer + Y
;; returns nil otherwise
(defun linearize (form)
  (cond ((integerp form) (cons 0 form))
	((numberp form) nil)
	((atom form)
	 (let (dum)
	   (cond ((setq dum (evod form))
		  (if (eq '$even dum) '(2 . 0) '(2 . 1)))
		 ((maxima-integerp form) '(1 . 0)))))
	((eq 'rat (caar form)) (cons 0 form))
	((eq 'mplus (caar form)) (lin-mplus form))
	((eq 'mtimes (caar form)) (lin-mtimes form))
	((eq 'mexpt (caar form)) (lin-mexpt form))))

(defun lin-mplus (form)
  (do ((tl (cdr form) (cdr tl)) (dummy) (coeff 0) (zl-rem 0))
      ((null tl) (cons coeff (mmod zl-rem coeff)))
    (setq dummy (linearize (car tl)))
    (if (null dummy) (return nil)
	(setq coeff (rgcd (car dummy) coeff) zl-rem (rplus (cdr dummy) zl-rem)))))

(defun lin-mtimes (form)
  (do ((fl (cdr form) (cdr fl)) (dummy) (coeff 0) (zl-rem 1))
      ((null fl) (cons coeff (mmod zl-rem coeff)))
    (setq dummy (linearize (car fl)))
    (cond ((null dummy) (return nil))
	  (t (setq coeff (rgcd (rtimes coeff (car dummy))
			       (rgcd (rtimes coeff (cdr dummy)) (rtimes zl-rem (car dummy))))
		   zl-rem (rtimes (cdr dummy) zl-rem))))))

(defun lin-mexpt (form)
  (prog (dummy)
     (cond ((and (integerp (caddr form)) (not (minusp (caddr form)))
		 (not (null (setq dummy (linearize (cadr form))))))
	    (return (cons (car dummy) (mmod (cdr dummy) (caddr form))))))))

(defun rgcd (x y)
  (cond ((integerp x)
	 (cond ((integerp y) (gcd x y))
	       (t (list '(rat) (gcd x (cadr y)) (caddr y)))))
	((integerp y) (list '(rat) (gcd (cadr x) y) (caddr x)))
	(t (list '(rat) (gcd (cadr x) (cadr y)) (lcm (caddr x) (caddr y))))))

(defun maxima-reduce (x y)
  (prog (gcd)
     (setq gcd (gcd x y) x (truncate x gcd) y (truncate y gcd))
     (if (minusp y) (setq x (- x) y (- y)))
     (return (if (eql y 1) x (list '(rat simp) x y)))))

;; The following four functions are generated in code by TRANSL. - JPG 2/1/81

(defmfun rplus (x y) (addk x y))

(defmfun rdifference (x y) (addk x (timesk -1 y)))

(defmfun rtimes (x y) (timesk x y))

(defmfun rremainder (x y)
  (cond ((equal 0 y) (dbz-err))
	((integerp x)
	 (cond ((integerp y) (maxima-reduce x y))
	       (t (maxima-reduce (* x (caddr y)) (cadr y)))))
	((integerp y) (maxima-reduce (cadr x) (* (caddr x) y)))
	(t (maxima-reduce (* (cadr x) (caddr y)) (* (caddr x) (cadr y))))))

(defmfun $exponentialize (exp)
  (let ($demoivre)
    (cond ((atom exp) exp)
	  ((trigp (caar exp))
	   (exponentialize (caar exp) ($exponentialize (cadr exp))))
	  (t (recur-apply #'$exponentialize exp)))))

(defmfun exponentialize (op arg)
  (cond ((eq '%sin op)
	 (div (sub (power '$%e (mul '$%i arg)) (power '$%e (mul -1 '$%i arg)))
	      (mul 2 '$%i)))
	((eq '%cos op)
	 (div (add (power '$%e (mul '$%i arg)) (power '$%e (mul -1 '$%i arg))) 2))
	((eq '%tan op)
	 (div (sub (power '$%e (mul '$%i arg)) (power '$%e (mul -1 '$%i arg)))
	      (mul '$%i (add (power '$%e (mul '$%i arg))
			     (power '$%e (mul -1 '$%i arg))))))
	((eq '%cot op)
	 (div (mul '$%i (add (power '$%e (mul '$%i arg))
			     (power '$%e (mul -1 '$%i arg))))
	      (sub (power '$%e (mul '$%i arg)) (power '$%e (mul -1 '$%i arg)))))
	((eq '%csc op)
	 (div (mul 2 '$%i)
	      (sub (power '$%e (mul '$%i arg)) (power '$%e (mul -1 '$%i arg)))))
	((eq '%sec op)
	 (div 2 (add (power '$%e (mul '$%i arg)) (power '$%e (mul -1 '$%i arg)))))
	((eq '%sinh op)
	 (div (sub (power '$%e arg) (power '$%e (neg arg))) 2))
	((eq '%cosh op)
	 (div (add (power '$%e arg) (power '$%e (mul -1 arg))) 2))
	((eq '%tanh op)
	 (div (sub (power '$%e arg) (power '$%e (neg arg)))
	      (add (power '$%e arg) (power '$%e (mul -1 arg)))))
	((eq '%coth op)
	 (div (add (power '$%e arg) (power '$%e (mul -1 arg)))
	      (sub (power '$%e arg) (power '$%e (neg arg)))))
	((eq '%csch op)
	 (div 2 (sub (power '$%e arg) (power '$%e (neg arg)))))
	((eq '%sech op)
	 (div 2 (add (power '$%e arg) (power '$%e (mul -1 arg)))))))

(defun coefficient (exp var pow)
  (coeff exp var pow))

(defun mmod (x mod)
  (cond ((and (integerp x) (integerp mod))
	 (if (minusp (if (zerop mod) x (setq x (- x (* mod (truncate x mod))))))
	     (+ x mod)
	     x))
        ((and ($ratnump x) ($ratnump mod))
	 (let
	     ((d (lcm ($denom x) ($denom mod))))
	   (setq x (mul* d x))
	   (setq mod (mul* d mod))
	   (div (mod x mod) d)))
	(t nil)))

(defun multiplep (exp var)
  (and (not (zerop1 exp)) (zerop1 (sub exp (mul var (coeff exp var 1))))))

(defun linearp (exp var)
  (and (setq exp (islinear exp var)) (not (equal (car exp) 0))))

(defmfun mminusp (x)
  (= -1 (signum1 x)))

(defmfun mminusp* (x)
  (let (sign)
    (setq sign (csign x))
    (or (member sign '($neg $nz) :test #'eq)
	(and (mminusp x) (not (member sign '($pos $pz) :test #'eq))))))

;; This should give more information somehow.

(defun dbz-err ()
  (cond ((not errorsw) (merror (intl:gettext "Division by zero attempted.")))
	(t (throw 'errorsw t))))

(defun dbz-err1 (func)
  (cond ((not errorsw) (merror (intl:gettext "~A: division by zero attempted.") func))
	(t (throw 'errorsw t))))
