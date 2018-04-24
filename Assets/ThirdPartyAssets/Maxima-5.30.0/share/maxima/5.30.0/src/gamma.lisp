;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Double Factorial, Incomplete Gamma function, ...
;;;
;;; This file will be extended with further functions related to the 
;;; Factorial and Gamma functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains the following Maxima User functions:
;;;
;;;   double_factorial(z)
;;;
;;;   gamma_incomplete(a,z)
;;;   gamma_incomplete_generalized(a,z1,z2)
;;;   gamma_incomplete_regularized(a,z)
;;;
;;;   log_gamma(z)
;;;
;;;   erf(z)
;;;   erfc(z)
;;;   erfi(z)
;;;   erf_generalized(z1,z2)
;;;
;;;   inverse_erf(z)
;;;   inverse_erfc(z)
;;;
;;;   fresnel_s(z)
;;;   fresnel_c(z)
;;;
;;;   beta_incomplete(a,b,z)
;;;   beta_incomplete_generalized(a,b,z1,z2)
;;;   beta_incomplete_regularized(a,b,z)
;;;
;;; Maxima User variable:
;;;
;;;   $factorial_expand    - Allows argument simplificaton for expressions like
;;;                          factorial_double(n-1) and factorial_double(2*k+n)
;;;   $beta_expand         - Switch on further expansions for the Beta functions
;;;
;;;   $erf_representation  - When T erfc, erfi, erf_generalized, fresnel_s 
;;;                          and fresnel_c are transformed to erf.
;;;   $erf_%iargs          - Enable simplification of Erf and Erfi for
;;;                          imaginary arguments
;;;   $hypergeometric_representation
;;;                        - Enables transformation to a Hypergeometric
;;;                          representation for fresnel_s and fresnel_c
;;;
;;; Maxima User variable (not definied in this file):
;;;
;;;   $factlim             - biggest integer for numerically evaluation
;;;                          of the Double factorial
;;;   $gamma_expand        - Expansion of the Gamma und Incomplete Gamma
;;;                          function for some special cases
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This library is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by the 
;;; Free Software Foundation; either version 2 of the License, or (at
;;; your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along 
;;; with this library; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Copyright (C) 2008 Dieter Kaiser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(declare-top (special $factlim $gamma_expand))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmvar $factorial_expand nil)
(defmvar $beta_expand nil)

(defmvar $erf_representation nil
  "When T erfc, erfi and erf_generalized are transformed to erf.")

(defmvar $erf_%iargs nil
  "When T erf and erfi simplifies for an imaginary argument.")

(defmvar $hypergeometric_representation nil
  "When T a transformation to a hypergeometric representation is done.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The following functions test if numerical evaluation has to be done.
;;; The functions should help to test for numerical evaluation more consitent
;;; and without complicated conditional tests including more than one or two
;;; arguments.
;;;
;;; The functions take a list of arguments. All arguments have to be a CL or
;;; Maxima number. If all arguments are numbers we have two cases:
;;; 1. $numer is T we return T. The function has to be evaluated numerically.
;;; 2. One of the args is a float or a bigfloat. Evaluate numerically.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Test for numerically evaluation in float precision

(defun float-numerical-eval-p (&rest args)
  (let ((flag nil))
    (dolist (ll args)
      (when (not (float-or-rational-p ll)) 
        (return-from float-numerical-eval-p nil))
      (when (floatp ll) (setq flag t)))
    (if (or $numer flag) t nil)))

;;; Test for numerically evaluation in complex float precision

(defun complex-float-numerical-eval-p (&rest args)
  (let ((flag nil))
    (dolist (ll args)
      (when (not (complex-number-p ll 'float-or-rational-p)) 
        (return-from complex-float-numerical-eval-p nil))
      (when (or (floatp ($realpart ll)) (floatp ($imagpart ll)))
        (setq flag t)))
    (if (or $numer flag) t nil)))

;;; Test for numerically evaluation in bigfloat precision

(defun bigfloat-numerical-eval-p (&rest args)
  (let ((flag nil))
    (dolist (ll args)
      (when (not (bigfloat-or-number-p ll)) 
        (return-from bigfloat-numerical-eval-p nil))
      (when ($bfloatp ll) (setq flag t)))
    (if (or $numer flag) t nil)))

;;; Test for numerically evaluation in complex bigfloat precision

(defun complex-bigfloat-numerical-eval-p (&rest args)
  (let ((flag nil))
    (dolist (ll args)
      (when (not (complex-number-p ll 'bigfloat-or-number-p)) 
        (return-from complex-bigfloat-numerical-eval-p nil))
      (when (or ($bfloatp ($realpart ll)) ($bfloatp ($imagpart ll)))
        (setq flag t)))
    (if (or $numer flag) t nil)))

;;; Test for numerical evaluation in any precision, real or complex.
(defun numerical-eval-p (&rest args)
  (or (apply 'float-numerical-eval-p args)
      (apply 'complex-float-numerical-eval-p args)
      (apply 'bigfloat-numerical-eval-p args)
      (apply 'complex-bigfloat-numerical-eval-p args)))

;;; Check for an integer or a float or bigfloat representation. When we
;;; have a float or bigfloat representation return the integer value.

(defun integer-representation-p (x)
  (let ((val nil))
    (cond ((integerp x) x)
          ((and (floatp x) (= 0 (nth-value 1 (truncate x))))
           (nth-value 0 (truncate x)))
          ((and ($bfloatp x) 
                (eq ($sign (sub (setq val ($truncate x)) x)) '$zero))
           val)
          (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The changes to the parser to connect the operator !! to double_factorial(z)

;(def-mheader |$!!| (%double_factorial))

;(def-led (|$!!| 160.) (op left)
;  (list '$expr
;	(mheader '$!!)
;	(convert left '$expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The implementation of the function Double factorial
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $double_factorial (z)
  (simplify (list '(%double_factorial) z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set properties to give full support to the parser and display

(defprop $double_factorial %double_factorial alias)
(defprop $double_factorial %double_factorial verb)

(defprop %double_factorial $double_factorial reversealias)
(defprop %double_factorial $double_factorial noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Double factorial is a simplifying function

(defprop %double_factorial simp-double-factorial operators)

;;; Double factorial distributes over bags

(defprop %double_factorial (mlist $matrix mequal) distribute_over)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Double factorial has mirror symmetry

(defprop %double_factorial t commutes-with-conjugate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Differentiation of Double factorial

(defprop %double_factorial
  ((z)
   ((mtimes) 
      ((rat) 1 2)
      ((%double_factorial) z)
      ((mplus) 
         ((%log) 2)
         ((mqapply) 
            (($psi array) 0)
            ((mplus) 1 ((mtimes) ((rat) 1 2) z)))
         ((mtimes) 
            ((rat) 1 2) $%pi
            ((%log) ((mtimes) 2 ((mexpt) $%pi -1)))
            ((%sin) ((mtimes) $%pi z))))))
  grad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-double-factorial (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond    
    ((and (fixnump z) (> z -1) (or (minusp $factlim) (< z $factlim)))
     ;; Positive Integer less then $factlim or $factlim is -1. Call gfact.
     (gfact z (floor (/ z 2)) 2))

    ((and (mnump z)
          (eq ($sign z) '$neg)          
          (zerop1 (sub (simplify (list '(%truncate) (div z 2))) (div z 2))))
     ;; Even negative integer or real representation. Not defined.
     (simp-domain-error 
       (intl:gettext 
         "double_factorial: double_factorial(~:M) is undefined.") z))

    ((or (integerp z)   ; at this point odd negative integer. Evaluate.
         (complex-float-numerical-eval-p z))
     (cond
       ((and (integerp z) (= z -1))  1)  ; Special cases -1 and -3 
       ((and (integerp z) (= z -3)) -1)
       (t
        ;; Odd negative integer, float or complex float.
        (complexify 
          (double-factorial 
            (complex ($float ($realpart z)) ($float ($imagpart z))))))))
  
    ((and (not (ratnump z))
          (complex-bigfloat-numerical-eval-p z))
     ;; bigfloat or complex bigfloat.
     (bfloat-double-factorial 
       (add ($bfloat ($realpart z)) (mul '$%i ($bfloat ($imagpart z))))))

    ;; double_factorial(inf) -> inf
    ((eq z '$inf) '$inf)

    ((and $factorial_expand
          (mplusp z)
          (integerp (cadr z)))
     (let ((k (cadr z))
           (n (simplify (cons '(mplus) (cddr z)))))
       (cond
         ((= k -1)
          ;; Special case double_factorial(n-1)
          ;; Not sure if this simplification is useful.
          (div (simplify (list '(mfactorial) n)) 
               (simplify (list '(%double_factorial) n))))
         ((= k (* 2 (truncate (/ k 2))))
          ;; Special case double_factorial(2*k+n), k integer
          (setq k (/ k 2))
          ($factor   ; we get more simple expression when factoring
            (mul
              (power 2 k)
              (simplify (list '($pochhammer) (add (div n 2) 1) k))
              (simplify (list '(%double_factorial) n)))))
         (t
           (eqtest (list '(%double_factorial) z) expr)))))

    (t
      (eqtest (list '(%double_factorial) z) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Double factorial for a complex float argument. The result is a CL complex.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun double-factorial (z)
  (let ((pival (float pi)))
    (*
     (expt
      (/ 2 pival)
      (/ (- 1 (cos (* pival z))) 4))
     (expt 2e0 (/ z 2))
     (gamma-lanczos (+ 1 (/ z 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Double factorial for a bigfloat or complex bigfloat argument
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bfloat-double-factorial (z)
  (let* ((pival ($bfloat '$%pi))
         (bigfloat1 ($bfloat bigfloatone))
         (bigfloat2 (add bigfloat1 bigfloat1))
         (bigfloat4 (add bigfloat2 bigfloat2))
         ($ratprint nil))
    (cmul
      (cpower
        (cdiv bigfloat2 pival)
        (cdiv (sub bigfloat1 
                   (simplify (list '(%cos) (cmul pival z)))) bigfloat4))
      (cmul
        (cpower bigfloat2 (cdiv z bigfloat2))
        (simplify (list '(%gamma) (add bigfloat1 (cdiv z bigfloat2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The implementation of the Incomplete Gamma function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *debug-gamma* nil)

(defun $gamma_incomplete (a z)
  (simplify (list '(%gamma_incomplete) a z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set properties to give full support to the parser and display

(defprop $gamma_incomplete %gamma_incomplete alias)
(defprop $gamma_incomplete %gamma_incomplete verb)

(defprop %gamma_incomplete $gamma_incomplete reversealias)
(defprop %gamma_incomplete $gamma_incomplete noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Incomplete Gamma function is a simplifying function

(defprop %gamma_incomplete simp-gamma-incomplete operators)

;;; Incomplete Gamma distributes over bags

(defprop %gamma_incomplete (mlist $matrix mequal) distribute_over)

;;; Incomplete Gamma function has not mirror symmetry for z on the negative
;;; real axis. We support a conjugate-function which test this case.

(defprop %gamma_incomplete conjugate-gamma-incomplete conjugate-function)

(defun conjugate-gamma-incomplete (args)
  (let ((a (first args)) (z (second args)))
    (cond ((off-negative-real-axisp z)
           ;; Definitly not on the negative real axis for z. Mirror symmetry.
	   (simplify
             (list
              '(%gamma_incomplete)
               (simplify (list '($conjugate) a))
               (simplify (list '($conjugate) z)))))
	  (t
           ;; On the negative real axis or no information. Unsimplified.
           (list
            '($conjugate simp)
             (simplify (list '(%gamma_incomplete) a z)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Derivative of the Incomplete Gamma function

(putprop '%gamma_incomplete
  `((a z)
    ,(lambda (a z)
       (cond ((member ($sign a) '($pos $pz))
              ;; The derivative wrt a in terms of hypergeometric_regularized 2F2
              ;; function and the Generalized Incomplete Gamma function 
              ;; (functions.wolfram.com), only for a>0.
              '((mplus)
                 ((mtimes)
                   ((mexpt) ((%gamma) a) 2)
                   ((mexpt) z a)
                   (($hypergeometric_regularized)
                      ((mlist) a a)
                      ((mlist) ((mplus) 1 a) ((mplus) 1 a))
                      ((mtimes) -1 z)))
                 ((mtimes) -1
                   ((%gamma_incomplete_generalized) a 0 z)
                   ((%log) z))
                 ((mtimes)
                   ((%gamma) a)
                   ((mqapply) (($psi array) 0) a))))
             (t
              ;; No derivative. Maxima generates a noun form.  
              nil)))
    ;; The derivative wrt z
    ((mtimes) -1
      ((mexpt) $%e ((mtimes) -1 z))
      ((mexpt) z ((mplus) -1 a))))
  'grad)

;;; Integral of the Incomplete Gamma function

(defprop %gamma_incomplete
  ((a z)
   nil
   ((mplus)
      ((mtimes) -1 ((%gamma_incomplete) ((mplus) 1 a) z))
      ((mtimes) ((%gamma_incomplete) a z) z)))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We support a simplim%function. The function is looked up in simplimit and 
;;; handles specific values of the function.

(defprop %gamma_incomplete simplim%gamma_incomplete simplim%function)

(defun simplim%gamma_incomplete (expr var val)
  ;; Look for the limit of the arguments.
  (let ((a (limit (cadr expr) var val 'think))
        (z (limit (caddr expr) var val 'think)))
  (cond

   ((eq z '$infinity)			;; http://dlmf.nist.gov/8.11#i
    (cond ((and (zerop1 ($realpart (caddr expr)))
		(eq ($csign (m+ -1 (cadr expr))) '$neg))
	   0)
	  (t (throw 'limit t))))

    ;; Handle an argument 0 at this place.
    ((or (zerop1 z)
         (eq z '$zeroa)
         (eq z '$zerob))
     (let ((sgn ($sign ($realpart a))))
       (cond ((zerop1 a) '$inf)
             ((member sgn '($neg $nz)) '$infinity)
             ((eq sgn '($pos)) ($gamma a))
             ;; Call the simplifier of the function.
             (t (simplify (list '(%gamma_incomplete) a z))))))
    (t
     ;; All other cases are handled by the simplifier of the function.
     (simplify (list '(%gamma_incomplete) a z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-gamma-incomplete (expr ignored simpflag)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((a (simpcheck (cadr expr) simpflag))
        (z (simpcheck (caddr expr) simpflag))
        (ratorder))
    (cond

      ;; Check for specific values

      ((zerop1 z)
       ;; gamma_incomplete(v,0) is gamma(v) only if the realpart(v) >
       ;; 0.  If realpart(v) <= 0, gamma_incomplete is undefined.  For
       ;; all other cases, return the noun form.
       (let ((sgn ($sign ($realpart a))))
         (cond ((member sgn '($neg $zero))
                (simp-domain-error 
                  (intl:gettext 
                    "gamma_incomplete: gamma_incomplete(~:M,~:M) is undefined.")
                    a z))
               ((member sgn '($pos $pz)) ($gamma a))
               (t (eqtest (list '(%gamma_incomplete) a z) expr)))))
              
      ((eq z '$inf) 0)
      ((and (eq z '$minf) 
	    (eq a 0))
       '$infinity)

      ;; Check for numerical evaluation in Float or Bigfloat precision

      ((float-numerical-eval-p a z)
       (when *debug-gamma* 
         (format t "~&SIMP-GAMMA-INCOMPLETE in float-numerical-eval-p~%"))
       ;; a and z are Maxima numbers, at least one has a float value
       (let ((a ($float a))
             (z ($float z)))
         (cond
           ((or (= a 0.0)
                (and (= 0 (- a (truncate a)))
                     (< a 0.0)))
            ;; a is zero or a negative float representing an integer.
            ;; For these cases the numerical routines of gamma-incomplete
            ;; do not work. Call the numerical routine for the Exponential 
            ;; Integral E(n,z). The routine is called with a positive integer!.
            (setq a (truncate a))
            (complexify (* (expt z a) (expintegral-e (- 1 a) z))))
           (t
             (complexify (gamma-incomplete a z))))))

      ((complex-float-numerical-eval-p a z)
       (when *debug-gamma* 
         (format t 
           "~&SIMP-GAMMA-INCOMPLETE in complex-float-numerical-eval-p~%"))
       ;; a and z are Maxima numbers, at least one is a complex value and
       ;; we have at least one float part
       (let ((ca (complex ($float ($realpart a)) ($float ($imagpart a))))
             (cz (complex ($float ($realpart z)) ($float ($imagpart z)))))
         (cond
           ((and (= (imagpart ca) 0.0)
                 (or (= (realpart ca) 0.0)
                     (and (= 0 (- (realpart ca) (truncate (realpart ca))))
                          (< (realpart ca) 0.0))))
            ;; Call expintegral-e. See comment above.
            (setq ca (truncate (realpart ca)))
            (complexify (* (expt cz ca) (expintegral-e (- 1 ca) cz))))
           (t
            (complexify (gamma-incomplete ca cz))))))
           
      ((bigfloat-numerical-eval-p a z)
       (when *debug-gamma* 
         (format t "~&SIMP-GAMMA-INCOMPLETE in bigfloat-numerical-eval-p~%"))
       (let ((a ($bfloat a))
             (z ($bfloat z)))
         (cond         
           ((or (eq ($sign a) '$zero)
                (and (eq ($sign (sub a ($truncate a))) '$zero)
                     (eq ($sign a) '$neg)))
            ;; Call bfloat-expintegral-e. See comment above.
            (setq a ($truncate a))
            ($rectform (mul (power z a) (bfloat-expintegral-e (- 1 a) z))))
           (t
             (bfloat-gamma-incomplete a z)))))

      ((complex-bigfloat-numerical-eval-p a z)
       (when *debug-gamma* 
         (format t 
           "~&SIMP-GAMMA-INCOMPLETE in complex-bigfloat-numerical-eval-p~%"))
       (let ((ca (add ($bfloat ($realpart a))
                      (mul '$%i ($bfloat ($imagpart a)))))
             (cz (add ($bfloat ($realpart z)) 
                      (mul '$%i ($bfloat ($imagpart z))))))
         (cond         
           ((and (eq ($sign ($imagpart ca)) '$zero)
                 (or (eq ($sign ($realpart ca)) '$zero)
                     (and (eq ($sign (sub ($realpart ca)
                                          ($truncate ($realpart ca))))
                              '$zero)
                          (eq ($sign ($realpart ca)) '$neg))))
            ;; Call bfloat-expintegral-e. See comment above.
            (when *debug-gamma* 
               (format t 
                 "~& bigfloat-numerical-eval-p calls bfloat-expintegral-e~%"))
            (setq a ($truncate ($realpart a)))
            ($rectform (mul (power cz a)
                            (bfloat-expintegral-e (- 1 a) cz))))
           (t
            (complex-bfloat-gamma-incomplete ca cz)))))

      ;; Check for transformations and argument simplification

      ((and $gamma_expand (integerp a))
       ;; Integer or a symbol declared to be an integer. Expand in a series.
       (let ((sgn ($sign a)))
         (cond
           ((eq sgn '$zero)
            (add
              (mul -1
                (simplify (list '(%expintegral_ei) (mul -1 z))))
              (mul
                '((rat simp) 1 2)
                (sub
                  (simplify (list '(%log) (mul -1 z)))
                  (simplify (list '(%log) (div -1 z)))))
              (mul -1 (simplify (list '(%log) z)))))
           ((member sgn '($pos $pz))
            (mul
              (simplify (list '(%gamma) a))
              (power '$%e (mul -1 z))
              (let ((index (gensumindex)))
                (dosum
                  (div 
                    (power z index)
                    (let (($gamma_expand nil))
                      ;; Simplify gamma, but do not expand to avoid division 
                      ;; by zero.
                      (simplify (list '(%gamma) (add index 1)))))
                  index 0 (sub a 1) t))))
           ((member sgn '($neg $nz))
            (sub
              (mul
                (div
                  (power -1 (add (mul -1 a) -1))
                  (simplify (list '(%gamma) (add (mul -1 a) 1))))
                (add
                  (simplify (list '(%expintegral_ei) (mul -1 z)))
                  (mul
                    '((rat simp) -1 2)
                    (sub
                      (simplify (list '(%log) (mul -1 z)))
                      (simplify (list '(%log) (div -1 z)))))
                  (simplify (list '(%log) z))))
              (mul
                (power '$%e (mul -1 z))
                (let ((index (gensumindex)))
                  (dosum
                    (div
                      (power z (add index a -1))
                      (simplify (list '($pochhammer) a index)))
                    index 1 (mul -1 a) t)))))
           (t (eqtest (list '(%gamma_incomplete) a z) expr)))))

      ((and $gamma_expand (setq ratorder (max-numeric-ratio-p a 2)))
       ;; We have a half integral order and $gamma_expand is not NIL.
       ;; We expand in a series with the Erfc function
       (setq ratorder (- ratorder (/ 1 2)))
       (cond
         ((equal ratorder 0)
          (mul 
            (power '$%pi '((rat simp) 1 2))
            (simplify (list '(%erfc) (power z '((rat simp) 1 2))))))
         ((> ratorder 0)
          (sub
            (mul
              (simplify (list '(%gamma) a))
              (simplify (list '(%erfc) (power z '((rat simp) 1 2)))))
            (mul
              (power -1 (sub ratorder 1))
              (power '$%e (mul -1 z))
              (power z '((rat simp) 1 2))
              (let ((index (gensumindex)))
                (dosum
                  (mul -1                      ; we get more simple results
                    (simplify                  ; when multiplying with -1  
                      (list 
                       '($pochhammer)
                        (sub '((rat simp) 1 2) ratorder)
                        (sub ratorder (add index 1))))
                    (power (mul -1 z) index))
                  index 0 (sub ratorder 1) t)))))
         ((< ratorder 0)
          (setq ratorder (- ratorder))
          (sub
            (div
              (mul
                (power -1 ratorder)
                (power '$%pi '((rat simp) 1 2))
                (simplify (list '(%erfc) (power z '((rat simp) 1 2)))))
              (simplify (list '($pochhammer) '((rat simp) 1 2) ratorder)))
            (mul 
              (power z (sub '((rat simp) 1 2) ratorder))
              (power '$%e (mul -1 z))
              (let ((index (gensumindex)))
                (dosum
                  (div
                    (power z index)
                    (simplify 
                      (list 
                       '($pochhammer) 
                        (sub '((rat simp) 1 2) ratorder)  
                        (add index 1))))
                  index 0 (sub ratorder 1) t)))))))

      ((and $gamma_expand (mplusp a) (integerp (cadr a)))
       (let ((n (cadr a))
             (a (simplify (cons '(mplus) (cddr a)))))
         (cond
           ((> n 0)
            (add
              (mul
                (simplify (list '($pochhammer) a n))
                (simplify (list '(%gamma_incomplete) a z)))
              (mul
                (power '$%e (mul -1 z))
                (power z (add a n -1))
                (let ((index (gensumindex)))
                  (dosum
                    (mul
                      (simplify 
                        (list 
                         '($pochhammer) (add 1 (mul -1 a) (mul -1 n)) index))
                      (power (mul -1 z) (mul -1 index)))
                    index 0 (add n -1) t)))))
           ((< n 0)
            (setq n (- n))
            (sub
              (div
                (mul
                  (power -1 n)
                  (simplify (list '(%gamma_incomplete) a z)))
                (simplify (list '($pochhammer) (sub 1 a) n)))
              (mul
                (power '$%e (mul -1 z))
                (power z (sub a n))
                (let ((index (gensumindex)))
                  (dosum
                    (div
                      (power z index)
                      (simplify (list '($pochhammer) (sub a n) (add index 1))))
                    index 0 (sub n 1) t))))))))

      (t (eqtest (list '(%gamma_incomplete) a z) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numerical evaluation of the Incomplete Gamma function
;;;
;;;  gamma-incomplete (a,z)                - real and complex double float
;;;  bfloat-gamma-incomplete (a z)         - bigfloat
;;;  complex-bfloat-gamma-incomplete (a z) - complex bigfloat
;;;
;;;  Expansion in a power series for abs(x) < R, where R is
;;;  *gamma-radius* + real(a) if real(a) > 0 or *gamma-radius*
;;;  otherwise.
;;;
;;;  (A&S 6.5.29):
;;;
;;;                            inf
;;;                            ===    
;;;                            \      gamma(a)
;;;  gamma(a,z) = exp(-x)*z^a * >   ------------ * z^n
;;;                            /    gamma(a+1+n)
;;;                            ===
;;;                            n=0
;;;
;;; This expansion does not work for an integer a<=0, because the Gamma function
;;; in the denominator is not defined for a=0 and negative integers. For this
;;; case we use the Exponential Integral E for numerically evaluation. The
;;; Incomplete Gamma function and the Exponential integral are connected by
;;;
;;; gamma(a,z) = z^a * expintegral_e(1-a,z)
;;;
;;; When the series is not used, two forms of the continued fraction
;;; are used.  When z is not near the negative real axis use the
;;; continued fractions (A&S 6.5.31):
;;;
;;;                              1   1-a   1   2-a   2
;;;  gamma(a,z) = exp(-z) z^a *( --  ---  ---  ---  --- ... )  
;;;                              z+  1+   z+   1+   z+ 
;;;
;;; The accuracy is controlled by *gamma-incomplete-eps* for double float
;;; precision. For bigfloat precision epsilon is 10^(-$fpprec). The expansions
;;; in a power series or continued fractions stops if *gamma-incomplete-maxit*
;;; is exceeded and an Maxima error is thrown.
;;;
;;; The above fraction does not converge on the negative real axis and
;;; converges very slowly near the axis.  In this case, use the
;;; relationship
;;;
;;;  gamma(a,z) = gamma(a) - gamma_lower(a,z)
;;;
;;; The continued fraction for gamma_incomplete_lower(a,z) is
;;; (http://functions.wolfram.com/06.06.10.0009.01):
;;;
;;;  gamma_lower(a,z) = exp(-z) * z^a / cf(a,z)
;;;
;;; where
;;;
;;;                -a*z   z    (a+1)*z   2*z  (a+2)*z
;;;  cf(a,z) = a + ----  ----  -------  ----  -------
;;;                a+1+  a+2-  a+3+     a+4-   a+5+
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *gamma-incomplete-maxit* 10000)
(defvar *gamma-incomplete-eps* (* 2 flonum-epsilon))
(defvar *gamma-incomplete-min* 1.0e-32)

(defvar *gamma-radius* 1.0
  "Controls the radius within a series expansion is done.")
(defvar *gamma-imag* 1.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The numerical evaluation for CL float or complex values a and x
;;; When the flag regularized is T, the result is divided by gamma(a) and
;;; Maxima returns the numercial result for gamma_incomplete_regularized

(defun gamma-incomplete (a x &optional (regularized nil))
  (let ((factor
	 ;; Compute the factor needed to scale the series or continued
	 ;; fraction.  This is x^a*exp(-x) or x^a*exp(-x)/gamma(a)
	 ;; depending on whether we want a non-regularized or
	 ;; regularized form.  We want to compute the factor carefully
	 ;; to avoid unnecessary overflow if possible.
	 (cond (regularized
		(or (try-float-computation
		     #'(lambda ()
			 ;; gammafloat is more accurate for real
			 ;; values of a.
			 (cond ((complexp a)
				(/ (* (expt x a) (exp (- x)))
				   (gamma-lanczos a)))
			       (t
				(/ (* (expt x a) (exp (- x)))
				   (gammafloat a))))))
		    ;; Easy way failed.  Use logs to compute the
		    ;; result.  This loses some precision, especially
		    ;; for large values of a and/or x because we use
		    ;; many bits to hold the exponent part.
		    (exp (- (* a (log x))
			    x
			    (log-gamma-lanczos (if (complexp a)
						   a
						   (complex a)))))))
	       (t
		(or (try-float-computation
		     #'(lambda ()
			 (* (expt x a) (exp (- x)))))
		    ;; Easy way failed, so use the log form.
		    (exp (- (* a (log x))
			    x)))))))
    (multiple-value-bind (result lower-incomplete-tail-p)
	(%gamma-incomplete a x)
      (cond (lower-incomplete-tail-p
	     ;; %gamma-incomplete compute the lower incomplete gamma
	     ;; function, so we need to substract that from gamma(a),
	     ;; more or less.
	     (cond (regularized
		    (- 1 (* result factor)))
		   ((complexp a)
		    (- (gamma-lanczos a) (* result factor)))
		   (t
		    (- (gammafloat a) (* result factor)))))
	    (t
	     ;; Continued fraction used.  Just multiply by the factor
	     ;; to get the final result.
	     (* factor result))))))

;; Compute the key part of the gamma incomplete function using either
;; a series expression or a continued fraction expression.  Two values
;; are returned: the value itself and a boolean, indicating what the
;; computed value is.  If the boolean non-NIL, then the computed value
;; is the lower incomplete gamma function.
(defun %gamma-incomplete (a x)
  (let ((gm-maxit *gamma-incomplete-maxit*)
        (gm-eps   *gamma-incomplete-eps*)
        (gm-min   *gamma-incomplete-min*))
    (when *debug-gamma*
      (format t "~&GAMMA-INCOMPLETE with ~A and ~A~%" a x))
    (cond
      ;; The series expansion is done for x within a circle of radius
      ;; R, where R = *gamma-radius*+(realpart(a)) for realpart(a) > 0
      ;; and R = *gamma-radisu* for realpart(a) < 0.  Otherwise a
      ;; continued fraction is used.
      ((and (> (abs x) (+ *gamma-radius*
                          (if (> (realpart a) 0.0) (realpart a) 0.0))))
       (cond ((and (< (realpart x) 0)
		   (< (abs (imagpart x))
		      (* *gamma-imag* (abs (realpart x)))))
	      ;; For x near the negative real axis, use the
	      ;; relationship gamma_incomplete(a,z) = gamma(a) -
	      ;; gamma_incomplete_lower(a,z), where
	      ;; gamma_incomplete_lower(a,z) is the lower poart of the
	      ;; incomplete gamma function.  We can evaluate that
	      ;; using a continued fraction from
	      ;; http://functions.wolfram.com/06.06.10.0009.01.  (Note
	      ;; that the alternative fraction,
	      ;; http://functions.wolfram.com/06.06.10.0007.01,
	      ;; appears to be less accurate.)
	      ;;
	      ;; Also note that this appears to be valid for all
	      ;; values of x (real or complex), but we don't want to
	      ;; use this everywhere for gamma_incomplete.  Consider
	      ;; what happens for large real x.  gamma_incomplete(a,x)
	      ;; is small, but gamma_incomplete(a,x) = gamma(x) - cf
	      ;; will have large roundoff errors.
	      (when *debug-gamma*
		(format t "~&GAMMA-INCOMPLETE in continued fractions for lower integral~%"))
	      (let ((a (bigfloat:to a))
		    (x (bigfloat:to x))
		    (bigfloat::*debug-cf-eval* *debug-gamma*)
		    (bigfloat::*max-cf-iterations* *gamma-incomplete-maxit*))
		(values (/ (bigfloat::lentz #'(lambda (n)
						(+ n a))
					    #'(lambda (n)
						(if (evenp n)
						    (* (ash n -1) x)
						    (- (* (+ a (ash n -1)) x))))))
			t)))
	     (t
	      ;; Expansion in continued fractions for gamma_incomplete.
	      (when *debug-gamma* 
		(format t "~&GAMMA-INCOMPLETE in continued fractions~%"))
	      (do* ((i 1 (+ i 1))
		    (an (- a 1.0) (* i (- a i)))
		    (b (+ 3.0 x (- a)) (+ b 2.0))
		    (c (/ 1.0 gm-min))
		    (d (/ 1.0 (- b 2.0)))
		    (h d)
		    (del 0.0))
		   ((> i gm-maxit)
		    (merror (intl:gettext "gamma_incomplete: continued fractions failed for gamma_incomplete(~:M, ~:M).") a x))
		(setq d (+ (* an d) b))
		(when (< (abs d) gm-min) (setq d gm-min))
		(setq c (+ b (/ an c)))
		(when (< (abs c) gm-min) (setq c gm-min))
		(setq d (/ 1.0 d))
		(setq del (* d c))
		(setq h (* h del))
		(when (< (abs (- del 1.0)) gm-eps)
		  ;; Return nil to indicate we used the continued fraction.
		  (return (values h nil)))))))
      (t
       ;; Expansion in a series
       (when *debug-gamma* 
	 (format t "~&GAMMA-INCOMPLETE in series~%"))
       (do* ((i 1 (+ i 1))
	     (ap a (+ ap 1.0))
	     (del (/ 1.0 a) (* del (/ x ap)))
	     (sum del (+ sum del)))
	    ((> i gm-maxit)
	     (merror (intl:gettext "gamma_incomplete: series expansion failed for gamma_incomplete(~:M, ~:M).") a x))
	 (when (< (abs del) (* (abs sum) gm-eps))
	   (when *debug-gamma* (format t "~&Series converged.~%"))
	   ;; Return T to indicate we used the series and the series
	   ;; is for the integral from 0 to x, not x to inf.
	   (return (values sum t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This function is called for a and x real

(defun bfloat-gamma-incomplete (a x)
  (let* ((gm-maxit *gamma-incomplete-maxit*)
         (gm-eps (power ($bfloat 10.0) (- $fpprec)))
         (gm-min (mul gm-eps gm-eps))
         ($ratprint nil))
    (cond
      ;; The series expansion is done for x within a circle of radius
      ;; R, where R = *gamma-radius*+(realpart(a)) for realpart(a) > 0
      ;; and R = *gamma-radisu* for realpart(a) < 0.  Otherwise a
      ;; continued fraction is used.
      ((eq ($sign (sub (simplify (list '(mabs) x))
		       (add *gamma-radius*
			    (if (eq ($sign a) '$pos) a 0.0))))
	   '$pos)
       (cond
	 ((and (eq ($sign x) '$pos))
	  ;; Expansion in continued fractions of the Incomplete Gamma function
	  (do* ((i 1 (+ i 1))
		(an (sub a 1.0) (mul i (sub a i)))
		(b (add 3.0 x (mul -1 a)) (add b 2.0))
		(c (div 1.0 gm-min))
		(d (div 1.0 (sub b 2.0)))
		(h d)
		(del 0.0))
	       ((> i gm-maxit)
		(merror (intl:gettext "gamma_incomplete: continued fractions failed for gamma_incomplete(~:M, ~:M).") a x))
	    (when *debug-gamma* 
	      (format t "~&in coninued fractions:~%")
	      (mformat t "~&   : i = ~M~%" i)
	      (mformat t "~&   : h = ~M~%" h))
	    (setq d (add (mul an d) b))
	    (when (eq ($sign (sub (simplify (list '(mabs) d)) gm-min)) '$neg)
	      (setq d gm-min))
	    (setq c (add b (div an c)))
	    (when (eq ($sign (sub (simplify (list '(mabs) c)) gm-min)) '$neg)
	      (setq c gm-min))
	    (setq d (div 1.0 d))
	    (setq del (mul d c))
	    (setq h (mul h del))
	    (when (eq ($sign (sub (simplify (list '(mabs) (sub del 1.0))) gm-eps))
		      '$neg)
	      (return 
		(mul h
		     (power x a) 
		     (power ($bfloat '$%e) (mul -1 x)))))))
	 (t
	  ;; Expand to multiply everything out.
	  ($expand
	   ;; Expansion in continued fraction for the lower incomplete gamma.
	   (sub (simplify (list '(%gamma) a))
		;; NOTE: We want (power x a) instead of bigfloat:expt
		;; because this preserves how maxima computes x^a when
		;; x is negative and a is rational.  For, example
		;; (-8)^(1/2) is -2.  bigfloat:expt returns the
		;; principal value.
		(mul (power x a)
		     (power ($bfloat '$%e) (mul -1 x))
		     (let ((a (bigfloat:to a))
			   (x (bigfloat:to x)))
		       (to (bigfloat:/
			    (bigfloat:lentz
			     #'(lambda (n)
				 (bigfloat:+ n a))
			     #'(lambda (n)
				 (if (evenp n)
				     (bigfloat:* (ash n -1) x)
				     (bigfloat:- (bigfloat:* (bigfloat:+ a (ash n -1))
							     x))))))))))))))

      (t
       ;; Series expansion of the Incomplete Gamma function
       (do* ((i 1 (+ i 1))
             (ap a (add ap 1.0))
             (del (div 1.0 a) (mul del (div x ap)))
             (sum del (add sum del)))
            ((> i gm-maxit)
             (merror (intl:gettext "gamma_incomplete: series expansion failed for gamma_incomplete(~:M, ~:M).") a x))
         (when *debug-gamma* 
           (format t "~&GAMMA-INCOMPLETE in series:~%")
           (mformat t "~&   : i    = ~M~%" i)
           (mformat t "~&   : ap   = ~M~%" ap)
           (mformat t "~&   : x/ap = ~M~%" (div x ap))
           (mformat t "~&   : del  = ~M~%" del)
           (mformat t "~&   : sum  = ~M~%" sum))
         (when (eq ($sign (sub (simplify (list '(mabs) del)) 
                               (mul (simplify (list '(mabs) sum)) gm-eps)))
                   '$neg)
           (when *debug-gamma* (mformat t "~&Series converged to ~M.~%" sum))
           (return 
             (sub (simplify (list '(%gamma) a))
                  ($rectform
                    (mul sum
                         (power x a)
                         (power ($bfloat '$%e) (mul -1 x))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun complex-bfloat-gamma-incomplete (a x)
  (let* ((gm-maxit *gamma-incomplete-maxit*)
         (gm-eps (power ($bfloat 10.0) (- $fpprec)))
         (gm-min (mul gm-eps gm-eps))
         ($ratprint nil))
    (when *debug-gamma*
      (format t "~&COMPLEX-BFLOAT-GAMMA-INCOMPLETE~%")
      (format t "   : a = ~A~%" a)
      (format t "   : x = ~A~%" x))
    (cond
      ;; The series expansion is done for x within a circle of radius
      ;; R, where R = *gamma-radius*+(realpart(a)) for realpart(a) > 0
      ;; and R = *gamma-radisu* for realpart(a) < 0.  Otherwise a
      ;; continued fraction is used.
      ((and (eq ($sign (sub (simplify (list '(mabs) x))
                            (add *gamma-radius*
                                 (if (eq ($sign ($realpart a)) '$pos)
                                     ($realpart a)
                                     0.0))))
                '$pos))
       (cond
	 ((not (and (eq ($sign ($realpart x)) '$neg)
		    (eq ($sign (sub (simplify (list '(mabs) ($imagpart x)))
				    (simplify (list '(mabs) ($realpart x)))))
			'$neg)))
	  ;; Expansion in continued fractions of the Incomplete Gamma function
	  (when *debug-gamma* 
	    (format t "~&in continued fractions:~%"))
	  (do* ((i 1 (+ i 1))
		(an (sub a 1.0) (mul i (sub a i)))
		(b (add 3.0 x (mul -1 a)) (add b 2.0))
		(c (cdiv 1.0 gm-min))
		(d (cdiv 1.0 (sub b 2.0)))
		(h d)
		(del 0.0))
	       ((> i gm-maxit)
		(merror (intl:gettext "gamma_incomplete: continued fractions failed for gamma_incomplete(~:M, ~:M).") a x))
	    (setq d (add (cmul an d) b))
	    (when (eq ($sign (sub (simplify (list '(mabs) d)) gm-min)) '$neg)
	      (setq d gm-min))
	    (setq c (add b (cdiv an c)))
	    (when (eq ($sign (sub (simplify (list '(mabs) c)) gm-min)) '$neg)
	      (setq c gm-min))
	    (setq d (cdiv 1.0 d))
	    (setq del (cmul d c))
	    (setq h (cmul h del))
	    (when (eq ($sign (sub (simplify (list '(mabs) (sub del 1.0))) 
				  gm-eps))
		      '$neg)
	      (return
		($bfloat ; force evaluation of expressions with sin or cos
		 (cmul h
		       (cmul
			(cpower x a)
			(cpower ($bfloat '$%e) ($bfloat (mul -1 x))))))))))
	 (t
	  ;; Expand to multiply everything out.
	  ($expand
	   ;; Expansion in continued fraction for the lower incomplete gamma.
	   (sub ($rectform (simplify (list '(%gamma) a)))
		;; NOTE: We want (power x a) instead of bigfloat:expt
		;; because this preserves how maxima computes x^a when
		;; x is negative and a is rational.  For, example
		;; (-8)^(1/2) is -2.  bigfloat:expt returns the
		;; principal value.
		(mul ($rectform (power x a))
		     ($rectform (power ($bfloat '$%e) (mul -1 x)))
		     (let ((a (bigfloat:to a))
			   (x (bigfloat:to x)))
		       (to (bigfloat:/
			    (bigfloat:lentz
			     #'(lambda (n)
				 (bigfloat:+ n a))
			     #'(lambda (n)
				 (if (evenp n)
				     (bigfloat:* (ash n -1) x)
				     (bigfloat:- (bigfloat:* (bigfloat:+ a (ash n -1))
							     x))))))))))))))
      (t
       ;; Series expansion of the Incomplete Gamma function
       (when *debug-gamma*
           (format t "~&GAMMA-INCOMPLETE in series:~%"))
       (do* ((i 1 (+ i 1))
             (ap a (add ap 1.0))
             (del (cdiv 1.0 a) (cmul del (cdiv x ap)))
             (sum del (add sum del)))
            ((> i gm-maxit)
             (merror (intl:gettext "gamma_incomplete: series expansion failed for gamma_incomplete(~:M, ~:M).") a x))
         (when (eq ($sign (sub (simplify (list '(mabs) del)) 
                               (mul (simplify (list '(mabs) sum)) gm-eps)))
                   '$neg)
           (when *debug-gamma* (format t "~&Series converged.~%"))
           (return
             ($bfloat ; force evaluation of expressions with sin or cos
               (sub (simplify (list '(%gamma) a))
                    (cmul sum
                      (cmul
                        (cpower x a)
                        (cpower ($bfloat '$%e) (mul -1 x)))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Generalized Incomplete Gamma function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $gamma_incomplete_generalized (a z1 z2)
  (simplify (list '(%gamma_incomplete_generalized) a z1 z2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set the properties alias, reversealias, noun and verb

(defprop $gamma_incomplete_generalized %gamma_incomplete_generalized alias)
(defprop $gamma_incomplete_generalized %gamma_incomplete_generalized verb)

(defprop %gamma_incomplete_generalized 
         $gamma_incomplete_generalized reversealias)
(defprop %gamma_incomplete_generalized 
         $gamma_incomplete_generalized noun)

;;; Generalized Incomplete Gamma function has not mirror symmetry for z1 or z2 
;;; on the negative real axis. 
;;; We support a conjugate-function which test this case.

(defprop %gamma_incomplete_generalized 
         conjugate-gamma-incomplete-generalized conjugate-function)

(defun conjugate-gamma-incomplete-generalized (args)
  (let ((a (first args)) (z1 (second args)) (z2 (third args)))
    (cond ((and (off-negative-real-axisp z1) (off-negative-real-axisp z2))
           ;; z1 and z2 definitly not on the negative real axis. 
           ;; Mirror symmetry.
	   (simplify
             (list
              '(%gamma_incomplete_generalized)
               (simplify (list '($conjugate) a))
               (simplify (list '($conjugate) z1))
               (simplify (list '($conjugate) z2)))))
	  (t
           ;; On the negative real axis or no information. Unsimplified.
           (list
            '($conjugate simp)
             (simplify (list '(%gamma_incomplete_generalized) a z1 z2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generalized Incomplete Gamma function is a simplifying function

(defprop %gamma_incomplete_generalized 
         simp-gamma-incomplete-generalized operators)

;;; Generalized Incomplete Gamma distributes over bags

(defprop %gamma_incomplete_generalized (mlist $matrix mequal) distribute_over)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Differentiation of Generalized Incomplete Gamma function

(defprop %gamma_incomplete_generalized
  ((a z1 z2)
   ;; The derivative wrt a in terms of hypergeometric_regularized 2F2 function
   ;; and the Generalized Incomplete Gamma function (functions.wolfram.com)
   ((mplus)
      ((mtimes)
         ((mexpt) ((%gamma) a) 2)
         ((mexpt) z1 a)
         (($hypergeometric_regularized)
            ((mlist) a a)
            ((mlist) ((mplus) 1 a) ((mplus) 1 a))
            ((mtimes) -1 z1)))
      ((mtimes) -1
         ((mexpt) ((%gamma) a) 2)
         ((mexpt) z2 a)
         (($hypergeometric_regularized)
            ((mlist) a a)
            ((mlist) ((mplus) 1 a) ((mplus) 1 a))
            ((mtimes) -1 z2)))
      ((mtimes) -1
         ((%gamma_incomplete_generalized) a 0 z1)
         ((%log) z1))
      ((mtimes)
         ((%gamma_incomplete_generalized) a 0 z2)
         ((%log) z2)))
   ;; The derivative wrt z1
   ((mtimes) -1
      ((mexpt) $%e ((mtimes) -1 z1))
      ((mexpt) z1 ((mplus) -1 a)))
   ;; The derivative wrt z2
   ((mtimes)
      ((mexpt) $%e ((mtimes) -1 z2))
      ((mexpt) z2 ((mplus) -1 a))))
  grad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-gamma-incomplete-generalized (expr ignored simpflag)
  (declare (ignore ignored))
  (if (not (= (length expr) 4)) (wna-err '$gamma_incomplete_generalized))
  (let ((a  (simpcheck (cadr expr)   simpflag))
        (z1 (simpcheck (caddr expr)  simpflag))
        (z2 (simpcheck (cadddr expr) simpflag)))

    (cond

      ;; Check for specific values

      ((zerop1 z2)
       (let ((sgn ($sign ($realpart a))))
         (cond 
           ((member sgn '($pos $pz))
            (sub
              (simplify (list '(%gamma_incomplete) a z1))
              (simplify (list '(%gamma) a))))
           (t 
            (eqtest (list '(%gamma_incomplete_generalized) a z1 z2) expr)))))

      ((zerop1 z1)
       (let ((sgn ($sign ($realpart a))))
         (cond 
           ((member sgn '($pos $pz))
            (sub
              (simplify (list '(%gamma) a))
              (simplify (list '(%gamma_incomplete) a z2))))
           (t 
            (eqtest (list '(%gamma_incomplete_generalized) a z1 z2) expr)))))

      ((zerop1 (sub z1 z2)) 0)

      ((eq z2 '$inf) (simplify (list '(%gamma_incomplete) a z1)))
      ((eq z1 '$inf) (mul -1 (simplify (list '(%gamma_incomplete) a z2))))

      ;; Check for numerical evaluation in Float or Bigfloat precision
      ;; Use the numerical routines of the Incomplete Gamma function

      ((float-numerical-eval-p a z1 z2)
       (complexify 
         (- (gamma-incomplete ($float a) ($float z1)) 
            (gamma-incomplete ($float a) ($float z2)))))

      ((complex-float-numerical-eval-p a z1 z2)
       (let ((ca  (complex ($float ($realpart a))  ($float ($imagpart a))))
             (cz1 (complex ($float ($realpart z1)) ($float ($imagpart z1))))
             (cz2 (complex ($float ($realpart z2)) ($float ($imagpart z2)))))
         (complexify (- (gamma-incomplete ca cz1) (gamma-incomplete ca cz2)))))
           
      ((bigfloat-numerical-eval-p a z1 z2)
       (sub (bfloat-gamma-incomplete ($bfloat a) ($bfloat z1)) 
            (bfloat-gamma-incomplete ($bfloat a) ($bfloat z2))))

      ((complex-bigfloat-numerical-eval-p a z1 z2)
       (let ((ca  (add ($bfloat ($realpart a)) 
                       (mul '$%i ($bfloat ($imagpart a)))))
             (cz1 (add ($bfloat ($realpart z1))
                       (mul '$%i ($bfloat ($imagpart z1)))))
             (cz2 (add ($bfloat ($realpart z2))
                       (mul '$%i ($bfloat ($imagpart z2))))))
       (sub (complex-bfloat-gamma-incomplete ca cz1)
            (complex-bfloat-gamma-incomplete ca cz2))))

      ;; Check for transformations and argument simplification

      ((and $gamma_expand (mplusp a) (integerp (cadr a)))
       ;; Expand gamma_incomplete_generalized(a+n,z1,z2) with n an integer
       (let ((n (cadr a))
             (a (simplify (cons '(mplus) (cddr a)))))
         (cond
           ((> n 0)
            (mul
              (simplify (list '($pochhammer) a n))
              (add
                (simplify (list '(%gamma_incomplete_generalized) a z1 z2))
                (mul
                  (power '$%e (mul -1 z1))
                  (let ((index (gensumindex)))
                    (dosum
                      (div
                        (power z1 (add a index -1))
                        (simplify (list '($pochhammer) a index)))
                      index 1 n t)))
                (mul -1
                  (power '$%e (mul -1 z2))
                  (let ((index (gensumindex)))
                    (dosum
                      (div
                        (power z2 (add a index -1))
                        (simplify (list '($pochhammer) a index)))
                      index 1 n t))))))

           ((< n 0)
            (setq n (- n))
            (add
              (mul
                (div
                  (power -1 n)
                  ($factor (simplify (list '($pochhammer) (sub 1 a) n))))
                (simplify (list '(%gamma_incomplete_generalized) a z1 z2)))
              (mul -1
                (power '$%e (mul -1 z2))
                (let ((index (gensumindex)))
                  (dosum
                    (div
                      (power z1 (add a index (- n) -1))
                      (simplify (list '($pochhammer) (sub a n) index)))
                    index 1 n t)))
              (mul
                (power '$%e (mul -1 z2))
                (let ((index (gensumindex)))
                  (dosum
                    (div
                      (power z2 (add a index (- n) -1))
                      (simplify (list '($pochhammer) (sub a n) index)))
                    index 1 n t))))))))

      (t (eqtest (list '(%gamma_incomplete_generalized) a z1 z2) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Regularized Incomplete Gamma function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $gamma_incomplete_regularized (a z)
  (simplify (list '(%gamma_incomplete_regularized) a z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop $gamma_incomplete_regularized %gamma_incomplete_regularized alias)
(defprop $gamma_incomplete_regularized %gamma_incomplete_regularized verb)

(defprop %gamma_incomplete_regularized 
         $gamma_incomplete_regularized reversealias)
(defprop %gamma_incomplete_regularized 
         $gamma_incomplete_regularized noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generalized Incomplete Gamma function has not mirror symmetry for z1 or z2 
;;; on the negative real axis. 
;;; We support a conjugate-function which test this case.

(defprop %gamma_incomplete_regularized
         conjugate-gamma-incomplete-regularized conjugate-function)

(defun conjugate-gamma-incomplete-regularized (args)
  (let ((a (first args)) (z (second args)))
    (cond ((off-negative-real-axisp z)
           ;; z definitly not on the negative real axis. Mirror symmetry.
	   (simplify
             (list
              '(%gamma_incomplete_regularized)
               (simplify (list '($conjugate) a))
               (simplify (list '($conjugate) z)))))
	  (t
           ;; On the negative real axis or no information. Unsimplified.
           (list
            '($conjugate simp)
             (simplify (list '(%gamma_incomplete_regularized) a z)))))))

;;; Regularized Incomplete Gamma function is a simplifying function

(defprop %gamma_incomplete_regularized 
         simp-gamma-incomplete-regularized operators)

;;; Regularized Incomplete Gamma distributes over bags

(defprop %gamma_incomplete_regularized (mlist $matrix mequal) distribute_over)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Differentiation of Regularized Incomplete Gamma function

(defprop %gamma_incomplete_regularized
  ((a z)
   ;; The derivative wrt a in terms of hypergeometric_regularized 2F2 function
   ;; and the Regularized Generalized Incomplete Gamma function 
   ;; (functions.wolfram.com)
   ((mplus)
      ((mtimes)
         ((%gamma) a)
         ((mexpt) z a)
         (($hypergeometric_regularized)
            ((mlist) a a)
            ((mlist) ((mplus) 1 a) ((mplus) 1 a))
            ((mtimes) -1 z)))
      ((mtimes)
         ((%gamma_incomplete_generalized_regularized) a z 0)
         ((mplus)
            ((%log) z)
            ((mtimes) -1 ((mqapply) (($psi array) 0) a)))))
   ;; The derivative wrt z
   ((mtimes)
      ((mexpt) $%e ((mtimes) -1 z))
      ((mexpt) z ((mplus) -1 a))
      ((mexpt) ((%gamma) a) -1)))
  grad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-gamma-incomplete-regularized (expr ignored simpflag)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((a (simpcheck (cadr expr)  simpflag))
        (z (simpcheck (caddr expr) simpflag))
        (ratorder 0))

    (cond

      ;; Check for specific values

      ((zerop1 z)
       (let ((sgn ($sign ($realpart a))))
         (cond ((member sgn '($neg $zero))
                (simp-domain-error 
                  (intl:gettext 
                    "gamma_incomplete_regularized: gamma_incomplete_regularized(~:M,~:M) is undefined.")
                    a z))
               ((member sgn '($pos $pz)) 1)
               (t (eqtest (list '(%gamma_incomplete_regularized) a z) expr)))))  

      ((zerop1 a) 0)
      ((eq z '$inf) 0)

      ;; Check for numerical evaluation in Float or Bigfloat precision

      ((float-numerical-eval-p a z)
       (complexify
       ;; gamma_incomplete returns a regularized result
         (gamma-incomplete ($float a) ($float z) t)))

      ((complex-float-numerical-eval-p a z)
       (let ((ca (complex ($float ($realpart a)) ($float ($imagpart a))))
             (cz (complex ($float ($realpart z)) ($float ($imagpart z)))))
         ;; gamma_incomplete returns a regularized result
         (complexify (gamma-incomplete ca cz t))))
           
      ((bigfloat-numerical-eval-p a z)
       (div (bfloat-gamma-incomplete ($bfloat a) ($bfloat z)) 
            (simplify (list '(%gamma) ($bfloat a)))))

      ((complex-bigfloat-numerical-eval-p a z)
       (let ((ca (add ($bfloat ($realpart a)) 
                      (mul '$%i ($bfloat ($imagpart a)))))
             (cz (add ($bfloat ($realpart z)) 
                 (mul '$%i ($bfloat ($imagpart z))))))
       ($rectform
         (div
           (complex-bfloat-gamma-incomplete ca cz)
           (simplify (list '(%gamma) ca))))))

      ;; Check for transformations and argument simplification

      ((and $gamma_expand (integerp a))
       ;; An integer. Expand the expression.
       (let ((sgn ($sign a)))
         (cond
           ((member sgn '($pos $pz))
            (mul
              (power '$%e (mul -1 z))
              (let ((index (gensumindex)))
                (dosum
                  (div 
                    (power z index)
                    (let (($gamma_expand nil))
                      (simplify (list '(%gamma) (add index 1)))))
                  index 0 (sub a 1) t))))
           ((member sgn '($neg $nz)) 0)
           (t (eqtest (list '(%gamma_incomplete_regularized) a z) expr)))))

      ((and $gamma_expand (setq ratorder (max-numeric-ratio-p a 2)))
       ;; We have a half integral order and $gamma_expand is not NIL.
       ;; We expand in a series with the Erfc function
       (setq ratorder (- ratorder (/ 1 2)))
       (when *debug-gamma*
         (format t "~&SIMP-GAMMA-INCOMPLETE-REGULARIZED in RATORDER~%")
         (format t "~&   : a        = ~A~%" a)
         (format t "~&   : ratorder = ~A~%" ratorder))
       (cond
         ((equal ratorder 0)
          (simplify (list '(%erfc) (power z '((rat simp) 1 2)))))

         ((> ratorder 0)
          (add                               
            (simplify (list '(%erfc) (power z '((rat simp) 1 2))))
            (mul
              (power -1 (sub ratorder 1))
              (power '$%e (mul -1 z))
              (power z '((rat simp) 1 2))
              (div 1 (simplify (list '(%gamma) a)))             
              (let ((index (gensumindex)))
                (dosum
                  (mul
                    (power (mul -1 z) index)
                    (simplify (list '($pochhammer) 
                                    (sub '((rat simp) 1 2) ratorder)
                                    (sub ratorder (add index 1)))))
                  index 0 (sub ratorder 1) t)))))

         ((< ratorder 0)
          (setq ratorder (- ratorder))
          (add
            (simplify (list '(%erfc) (power z '((rat simp) 1 2))))
            (mul -1
              (power '$%e (mul -1 z))
              (power z (sub '((rat simp) 1 2) ratorder))
              (inv (simplify (list '(%gamma) (sub '((rat simp) 1 2) ratorder))))
              (let ((index (gensumindex)))
                (dosum
                  (div
                    (power z index)
                    (simplify (list '($pochhammer) 
                                    (sub '((rat simp) 1 2) ratorder) 
                                    (add index 1))))
                  index 0 (sub ratorder 1) t)))))))

      ((and $gamma_expand (mplusp a) (integerp (cadr a)))
       (when *debug-gamma* 
         (format t "~&SIMP-GAMMA-INCOMPLETE-REGULARIZED in COND (mplusp)~%"))
       (let ((n (cadr a))
             (a (simplify (cons '(mplus) (cddr a)))))
         (cond
           ((> n 0)
            (add
              (simplify (list '(%gamma_incomplete_regularized) a z))
              ;; We factor the second summand. 
              ;; Some factors vanish and the result is more readable.
              ($factor
                (mul
                  (power '$%e (mul -1 z))
                  (power z (add a -1))
                  (div 1 (simplify (list '(%gamma) a)))
                  (let ((index (gensumindex)))
                    (dosum
                      (div
                        (power z index)
                        (simplify (list '($pochhammer) a index)))
                      index 1 n t))))))
           ((< n 0)
            (setq n (- n))
            (add
              (simplify (list '(%gamma_incomplete_regularized) a z))
              ;; We factor the second summand.
              ($factor
                (mul -1
                  (power '$%e (mul -1 z))
                  (power z (sub a (add n 1)))
                  (div 1 (simplify (list '(%gamma) (add a (- n)))))
                  (let ((index (gensumindex)))
                    (dosum
                      (div
                        (power z index)
                        (simplify (list '($pochhammer) (add a (- n)) index)))
                      index 1 n t)))))))))
      
      (t (eqtest (list '(%gamma_incomplete_regularized) a z) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Logarithm of the Gamma function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $log_gamma (z)
  (simplify (list '(%log_gamma) z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop $log_gamma %log_gamma alias)
(defprop $log_gamma %log_gamma verb)

(defprop %log_gamma $log_gamma reversealias)
(defprop %log_gamma $log_gamma noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %log_gamma simp-log-gamma operators)

;;; Logarithm of the Gamma function distributes over bags

(defprop %log_gamma (mlist $matrix mequal) distribute_over)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %log_gamma
  ((z)
   ((mqapply) (($psi array) 0) z))
  grad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-log-gamma (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond

    ;; Check for specific values

    ((and (mnump z)
          (or (zerop1 z)
              (and (eq ($sign z) '$neg)
                   (zerop1 (sub z ($truncate z))))))
     ;; We have zero, a negative integer or a float or bigfloat representation.
     (simp-domain-error 
       (intl:gettext "log_gamma: log_gamma(~:M) is undefined.") z))

    ((eq z '$inf) '$inf)

    ;; Check for numerical evaluation

    ((float-numerical-eval-p z)
     (complexify (log-gamma-lanczos (complex ($float z) 0))))

    ((complex-float-numerical-eval-p z)
     (complexify 
       (log-gamma-lanczos 
         (complex ($float ($realpart z)) ($float ($imagpart z))))))

    ((bigfloat-numerical-eval-p z) 
     (bfloat-log-gamma ($bfloat z)))

    ((complex-bigfloat-numerical-eval-p z)
     (complex-bfloat-log-gamma 
       (add ($bfloat ($realpart z)) (mul '$%i ($bfloat ($imagpart z))))))

    ;; Transform to Logarithm of Factorial for integer values
    ;; At this point the integer value is positive and not zero.

    ((integerp z)
     (simplify (list '(%log) (simplify (list '(mfactorial) (- z 1))))))

    (t (eqtest (list '(%log_gamma) z) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The functions log-gamma-lanczos, bfloat-log-gamma and 
;;; complex-bfloat-log-gamma are modified versions of the related functions
;;; gamma-lanczos, bffac and cbffac. The functions return the Logarithm of
;;; the Gamma function. If we have to calculate the quotient of Gamma functions,
;;; e. g. for the Beta function, it is much more appropriate to use the
;;; logarithmic versions to avoid overflow.
;;;
;;; Be careful log(gamma(z)) is only for realpart(z) positive equal to 
;;; log_gamma(z). For a negative realpart(z) log_gamma differ by multiple of 
;;; %pi from log(gamma(z)). But we always have exp(log_gamma(z))= gamma(z).
;;; The terms to get the transformation for log_gamma(-z) are taken from
;;; functions.wolfram.com.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun log-gamma-lanczos (z)
  (declare (type (complex flonum) z)
           (optimize (safety 3)))
  (let ((c (make-array 15 :element-type 'flonum
                       :initial-contents
                       '(0.99999999999999709182
                         57.156235665862923517
                         -59.597960355475491248
                         14.136097974741747174
                         -0.49191381609762019978
                         .33994649984811888699e-4
                         .46523628927048575665e-4
                         -.98374475304879564677e-4
                         .15808870322491248884e-3
                         -.21026444172410488319e-3
                         .21743961811521264320e-3
                         -.16431810653676389022e-3
                         .84418223983852743293e-4
                         -.26190838401581408670e-4
                         .36899182659531622704e-5))))
    (declare (type (simple-array flonum (15)) c))
    (if (minusp (realpart z))
        (let ((z (- z)))
          (-
            (+
              (*
                (- (float pi))
                (complex 0 1)
                (abs (floor (realpart z)))
                (- 1 (abs (signum (imagpart z)))))
              (log (float pi))
              (- (log (- z)))
              (- (log (sin (* (float pi) (- z (floor (realpart z)))))))
              (* 
                (float pi)
                (complex 0 1)
                (floor (realpart z))
                (signum (imagpart z))))
            (log-gamma-lanczos z)))
        (let* ((z (- z 1))
               (zh (+ z 1/2))
               (zgh (+ zh 607/128))
               (lnzp (* (/ zh 2) (log zgh)))
               (ss 
                (do ((sum 0.0)
                     (pp (1- (length c)) (1- pp)))
                    ((< pp 1)
                     sum)
                  (incf sum (/ (aref c pp) (+ z pp))))))
          (+ (log (sqrt (float (* 2 pi))))
             (log (+ ss (aref c 0)))
             (+ (- zgh) (* 2 lnzp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bfloat-log-gamma (z)
  (let (($ratprint nil)
        (bigfloat%pi  ($bfloat '$%pi)))
  (cond
    ((eq ($sign z) '$neg)
     (let ((z (mul -1 z)))
       (sub
         (add
           (mul -1 bigfloat%pi '$%i
             (simplify (list '(mabs) (simplify (list '($floor) ($realpart z)))))
             (sub 1
               (simplify 
                 (list '(mabs) (simplify (list '(%signum) ($imagpart z)))))))
           (simplify (list '(%log) bigfloat%pi))
           (mul -1 (simplify (list '(%log) (mul -1 z))))
           (mul -1 
             (simplify (list '(%log) 
               (simplify (list '(%sin) 
                 (mul 
                   bigfloat%pi 
                   (sub z (simplify (list '($floor) ($realpart z))))))))))
           (mul
             bigfloat%pi '$%i
             (simplify (list '($floor) ($realpart z)))
             (simplify (list '(%signum) ($imagpart z)))))
         (bfloat-log-gamma z))))
    (t
     (let* ((k (* 2 (+ 1 ($entier (* 0.41 $fpprec)))))
            (m ($bfloat bigfloatone))
            (z+k (add z k -1))
            (y (power z+k 2))
            (x ($bfloat bigfloatzero))
            (ii))
       (dotimes (i (/ k 2))
         (setq ii (* 2 (+ i 1)))
         (setq m (mul m (add z ii -2) (add z ii -1)))
         (setq x (div
                   (add x
                        (div ($bern (+ k (- ii) 2))
                             (* (+ k (- ii) 1) (+ k (- ii) 2))))
                   y)))
       (add
         (div (simplify (list '(%log) (mul 2 bigfloat%pi z+k))) 2)
         (mul z+k (add (simplify (list '(%log) z+k)) x -1))
         (mul -1 (simplify (list '(%log) m)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun complex-bfloat-log-gamma (z)
  (let (($ratprint nil)
        (bigfloat%pi ($bfloat '$%pi)))
  (cond
    ((eq ($sign ($realpart z)) '$neg)
     (let ((z (mul -1 z)))
       (sub
         (add
           (mul -1 bigfloat%pi '$%i
             (simplify (list '(mabs) (simplify (list '($floor) ($realpart z)))))
             (sub 1
               (simplify 
                 (list '(mabs) (simplify (list '(%signum) ($imagpart z)))))))
           (simplify (list '(%log) bigfloat%pi))
           (mul -1 (simplify (list '(%log) (mul -1 z))))
           (mul -1 
             (simplify (list '(%log) 
               (simplify (list '(%sin) 
                 (mul 
                   bigfloat%pi 
                   (sub z (simplify (list '($floor) ($realpart z))))))))))
           (mul
             bigfloat%pi '$%i
             (simplify (list '($floor) ($realpart z)))
             (simplify (list '(%signum) ($imagpart z)))))
         (complex-bfloat-log-gamma z))))
    (t
     (let* ((k (* 2 (+ 1 ($entier (* 0.41 $fpprec)))))
            (m ($bfloat bigfloatone))
            (z+k (add z k -1))
            (y ($rectform (power z+k 2)))
            (x ($bfloat bigfloatzero))
            (ii))
       (dotimes (i (/ k 2))
         (setq ii (* 2 (+ i 1)))
         (setq m ($rectform (mul m (add z ii -2) (add z ii -1))))
         (setq x ($rectform
                   (div
                     (add x 
                       (div ($bern (+ k (- ii) 2))
                            (* (+ k (- ii) 1) (+ k (- ii) 2))))
                   y))))
       ($rectform
         (add
           (div (simplify (list '(%log) (mul 2 bigfloat%pi z+k))) 2)
           (mul z+k (add (simplify (list '(%log) z+k)) x -1))
           (mul -1 (simplify (list '(%log) m))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Error function Erf(z)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $erf (z)
  (simplify (list '(%erf) z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop $erf %erf alias)
(defprop $erf %erf verb)

(defprop %erf $erf reversealias)
(defprop %erf $erf noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; erf has mirror symmetry

(defprop %erf t commutes-with-conjugate)

;;; erf is an odd function

(defprop %erf odd-function-reflect reflection-rule)

;;; erf is a simplifying function

(defprop %erf simp-erf operators)

;;; erf distributes over bags

(defprop %erf (mlist $matrix mequal) distribute_over)

;;; Derivative of the Error function erf

(defprop %erf 
  ((z)
   ((mtimes) 2 
      ((mexpt) $%pi ((rat) -1 2))
      ((mexpt) $%e ((mtimes) -1 ((mexpt) z 2)))))
  grad)

;;; Integral of the Error function erf

(defprop %erf
  ((z)
   ((mplus)
     ((mtimes) 
        ((mexpt) $%pi ((rat) -1 2))
        ((mexpt) $%e ((mtimes) -1 ((mexpt) z 2))))
     ((mtimes) z ((%erf) z))))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-erf (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond

    ;; Check for specific values

    ((zerop1 z) z)
    ((eq z '$inf) 1)
    ((eq z '$minf) -1)

    ;; Check for numerical evaluation

    ((float-numerical-eval-p z)
     (bigfloat::bf-erf ($float z)))
    ((complex-float-numerical-eval-p z)
     (complexify 
       (bigfloat::bf-erf (complex ($float ($realpart z)) ($float ($imagpart z))))))
    ((bigfloat-numerical-eval-p z)
     (to (bigfloat::bf-erf (bigfloat:to ($bfloat z)))))
    ((complex-bigfloat-numerical-eval-p z)
     (to (bigfloat::bf-erf
	  (bigfloat:to (add ($bfloat ($realpart z)) (mul '$%i ($bfloat ($imagpart z))))))))

    ;; Argument simplification
    
    ((taylorize (mop expr) (second expr)))
    ((and $erf_%iargs 
          (not $erf_representation)
          (multiplep z '$%i))
     (mul '$%i (simplify (list '(%erfi) (coeff z '$%i 1)))))
    ((apply-reflection-simp (mop expr) z $trigsign))
    
    ;; Representation through equivalent functions
    
    ($hypergeometric_representation
      (mul 2 z 
           (power '$%pi '((rat simp) 1 2))
           (list '(%hypergeometric simp)
                 (list '(mlist simp) '((rat simp) 1 2))
                 (list '(mlist simp) '((rat simp) 3 2))
                 (mul -1 (power z 2)))))
    
    ;; Transformation to Erfc or Erfi
    
    ((and $erf_representation
          (not (eq $erf_representation '$erf)))
     (case $erf_representation
       (%erfc
        (sub 1 (take '(%erfc) z)))
       (%erfi
        (mul -1 '$%i (take '(%erfi) (mul '$%i z))))
       (t
        (eqtest (list '(%erf) z) expr))))

    (t
     (eqtest (list '(%erf) z) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erf (z)
  ;; We use the slatec routine for float values.
  (slatec:derf (float z)))

;; Compute erf(z) using the relationship
;;
;;   erf(z) = sqrt(z^2)/z*(1 - gamma_incomplete(1/2,z^2)/sqrt(%pi))
;;
;; When z is real sqrt(z^2)/z is signum(z).  For complex z,
;; sqrt(z^2)/z = 1 if -%pi/2 < arg(z) <= %pi/2 and -1 otherwise.
;;
;; This relationship has serious round-off issues when z is small
;; because gamma_incomplete(1/2,z^2)/sqrt(%pi) is near 1.
;;
;; complex-erf is for (lisp) complex numbers; bfloat-erf is for real
;; bfloats, and complex-bfloat-erf is for complex bfloats.  Care is
;; taken to return real results for real arguments and imaginary
;; results for imaginary arguments
;;
;; Pure imaginary z with Im(z) < 0 causes trouble for Lisp implementations
;; which recognize signed zero, so just avoid Im(z) < 0 altogether.

(defun complex-erf (z)
  (if (< (imagpart z) 0.0)
    (conjugate (complex-erf-upper-half-plane (conjugate z)))
    (complex-erf-upper-half-plane z)))

(defun complex-erf-upper-half-plane (z)
  (let ((result
          (*
	    (if (< (realpart z) 0.0) ;; only test needed in upper half plane
		-1
	      1)
            (- 1.0 
              ;; GAMMA-INCOMPLETE returns conjugate when z is pure imaginary
              ;; with Im(z) < 0 and Lisp implementation recognizes signed zero.
              ;; Good thing we are in the upper half plane.
              (* (/ (sqrt (float pi))) (gamma-incomplete 0.5 (* z z)))))))
    (cond
      ((= (imagpart z) 0.0)
       ;; Pure real argument, the result is real
       (complex (realpart result) 0.0))
      ((= (realpart z) 0.0)
       ;; Pure imaginary argument, the result is pure imaginary
       (complex 0.0 (imagpart result)))
      (t
        result))))

(defun bfloat-erf (z)
  ;; Warning!  This has round-off problems when abs(z) is very small.
  (let ((1//2 ($bfloat '((rat simp) 1 2))))
  ;; The argument is real, the result is real too
    ($realpart
      (mul
        (simplify (list '(%signum) z))
        (sub 1
          (mul 
            (div 1 (power ($bfloat '$%pi) 1//2))
            (bfloat-gamma-incomplete 1//2 ($bfloat (power z 2)))))))))

(defun complex-bfloat-erf (z)
  ;; Warning!  This has round-off problems when abs(z) is very small.
  (let* (($ratprint nil)
         (1//2 ($bfloat '((rat simp) 1 2)))
         (result
           (cmul
             (cdiv (cpower (cpower z 2) 1//2) z)
             (sub 1
               (cmul 
                 (div 1 (power ($bfloat '$%pi) 1//2))
                 (complex-bfloat-gamma-incomplete 
                   1//2
                   ($bfloat (cpower z 2))))))))
    (cond
      ((zerop1 ($imagpart z))
       ;; Pure real argument, the result is real
       ($realpart result))
      ((zerop1 ($realpart z))
        ;; Pure imaginary argument, the result is pure imaginary
        (mul '$%i ($imagpart result)))
      (t
       ;; A general complex result
       result))))

(in-package :bigfloat)

;; Erf(z) for all z.  Z must be a CL real or complex number or a
;; BIGFLOAT or COMPLEX-BIGFLOAT object.  The result will be of the
;; same type as Z.
(defun bf-erf (z)
  (cond ((typep z 'cl:real)
	 ;; Use Slatec derf, which should be faster than the series.
	 (maxima::erf z))
	((<= (abs z) 0.476936)
	 ;; Use the series A&S 7.1.5 for small x:
	 ;; 
	 ;; erf(z) = 2*z/sqrt(%pi) * sum((-1)^n*z^(2*n)/n!/(2*n+1), n, 0, inf)
	 ;;
	 ;; The threshold is approximately erf(x) = 0.5.  (Doesn't
	 ;; have to be super accurate.)  This gives max accuracy when
	 ;; using the identity  erf(x) = 1 - erfc(x).
	 (let ((z^2 (* z z)))
	   (/ (* 2 z (sum-power-series z^2
				       #'(lambda (k)
					   (let ((2k (+ k k)))
					     (- (/ (- 2k 1)
						   k
						   (+ 2k 1)))))))
	      (sqrt (%pi z)))))
	(t
	 ;; The general case.
	 (etypecase z
	   (cl:real (maxima::erf z))
	   (cl:complex (maxima::complex-erf z))
	   (bigfloat
	    (bigfloat (maxima::$bfloat (maxima::$expand (maxima::bfloat-erf (maxima::to z))))))
	   (complex-bigfloat
	    (bigfloat (maxima::$bfloat (maxima::$expand (maxima::complex-bfloat-erf (maxima::to z))))))))))

(defun bf-erfc (z)
  ;; Compute erfc(z) via 1 - erf(z) is not very accurate if erf(z) is
  ;; near 1.  Wolfram says
  ;;
  ;; erfc(z) = 1 - sqrt(z^2)/z * (1 - 1/sqrt(pi)*gamma_incomplete_tail(1/2, z^2))
  ;;
  ;; For real(z) > 0, sqrt(z^2)/z is 1 so
  ;;
  ;; erfc(z) = 1 - (1 - 1/sqrt(pi)*gamma_incomplete_tail(1/2,z^2))
  ;;         = 1/sqrt(pi)*gamma_incomplete_tail(1/2,z^2)
  ;;
  ;; For real(z) < 0, sqrt(z^2)/z is -1 so
  ;;
  ;; erfc(z) = 1 + (1 - 1/sqrt(pi)*gamma_incomplete_tail(1/2,z^2))
  ;;         = 2 - 1/sqrt(pi)*gamma_incomplete(1/2,z^2)
  (flet ((gamma-inc (z)
	   (etypecase z
	     (cl:number
	      (maxima::gamma-incomplete 0.5 z))
	     (bigfloat
	      (bigfloat:to (maxima::$bfloat
			    (maxima::bfloat-gamma-incomplete (maxima::$bfloat maxima::1//2)
							     (maxima::to z)))))
	     (complex-bigfloat
	      (bigfloat:to (maxima::$bfloat
			    (maxima::complex-bfloat-gamma-incomplete (maxima::$bfloat maxima::1//2)
								     (maxima::to z))))))))
  (if (>= (realpart z) 0)
      (/ (gamma-inc (* z z))
	 (sqrt (%pi z)))
      (- 2
	 (/ (gamma-inc (* z z))
	    (sqrt (%pi z)))))))

(in-package :maxima)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Generalized Error function Erf(z1,z2)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $erf_generalized (z1 z2)
  (simplify (list '(%erf_generalized) z1 z2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop $erf_generalized %erf_generalized alias)
(defprop $erf_generalized %erf_generalized verb)

(defprop %erf_generalized $erf_generalized reversealias)
(defprop %erf_generalized $erf_generalized noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generalized Erf has mirror symmetry

(defprop %erf_generalized t commutes-with-conjugate)

;;; Generalized Erf distributes over bags

(defprop %erf_generalized (mlist $matrix mequal) distribute_over)

;;; Generalized Erf is antisymmetric Erf(z1,z2) = - Erf(z2,z1)

(eval-when
    #+gcl (load eval)
    #-gcl (:load-toplevel :execute)
    (let (($context '$global) (context '$global))
      (meval '(($declare) %erf_generalized $antisymmetric))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %erf_generalized simp-erf-generalized operators)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %erf_generalized 
  ((z1 z2)
   ;; derivative wrt z1
   ((mtimes) -2 
      ((mexpt) $%pi ((rat) -1 2))
      ((mexpt) $%e ((mtimes) -1 ((mexpt) z1 2))))
   ;; derviative wrt z2
   ((mtimes) 2 
      ((mexpt) $%pi ((rat) -1 2))
      ((mexpt) $%e ((mtimes) -1 ((mexpt) z2 2)))))
  grad)

;;; ----------------------------------------------------------------------------

(defprop %erf_generalized simplim%erf_generalized simplim%function)

(defun simplim%erf_generalized (expr var val)
  ;; Look for the limit of the arguments.
  (let ((z1 (limit (cadr expr) var val 'think))
        (z2 (limit (caddr expr) var val 'think)))
    (cond
      ;; Handle infinities at this place.
      ((or (eq z2 '$inf)
           (alike1 z2 '((mtimes) -1 $minf)))
       (sub 1 (take '(%erf) z1)))
      ((or (eq z2 '$minf)
           (alike1 z2 '((mtimes) -1 $inf)))
       (sub (mul -1 (take '(%erf) z1)) 1))
      ((or (eq z1 '$inf)
           (alike1 z1 '((mtimes) -1 $minf)))
       (sub (take '(%erf) z2) 1))
      ((or (eq z1 '$minf)
           (alike1 z1 '((mtimes) -1 $inf)))
       (add (take '(%erf) z2) 1))
      (t
       ;; All other cases are handled by the simplifier of the function.
       (simplify (list '(%erf_generalized) z1 z2))))))

;;; ----------------------------------------------------------------------------

(defun simp-erf-generalized (expr ignored simpflag)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((z1 (simpcheck (cadr expr) simpflag))
        (z2 (simpcheck (caddr expr) simpflag)))
    (cond
      
      ;; Check for specific values
      
      ((and (zerop1 z1) (zerop1 z2)) 0)
      ((zerop1 z1) (take '(%erf) z2))
      ((zerop1 z2) (mul -1 (take '(%erf) z1)))
      ((or (eq z2 '$inf)
           (alike1 z2 '((mtimes) -1 $minf)))
       (sub 1 (take '(%erf) z1)))
      ((or (eq z2 '$minf)
           (alike1 z2 '((mtimes) -1 $inf)))
       (sub (mul -1 (take '(%erf) z1)) 1))
      ((or (eq z1 '$inf)
           (alike1 z1 '((mtimes) -1 $minf)))
       (sub (take '(%erf) z2) 1))
      ((or (eq z1 '$minf)
           (alike1 z1 '((mtimes) -1 $inf)))
       (add (take '(%erf) z2) 1))

      ;; Check for numerical evaluation. Use erf(z1,z2) = erf(z2)-erf(z1)

      ((float-numerical-eval-p z1 z2)
       (- (bigfloat::bf-erf ($float z2))
	  (bigfloat::bf-erf ($float z1))))
      ((complex-float-numerical-eval-p z1 z2)
       (complexify 
         (- 
           (bigfloat::bf-erf 
             (complex ($float ($realpart z2)) ($float ($imagpart z2))))
           (bigfloat::bf-erf 
             (complex ($float ($realpart z1)) ($float ($imagpart z1)))))))
      ((bigfloat-numerical-eval-p z1 z2)
       (to (bigfloat:-
	    (bigfloat::bf-erf (bigfloat:to ($bfloat z2)))
	    (bigfloat::bf-erf (bigfloat:to ($bfloat z1))))))
      ((complex-bigfloat-numerical-eval-p z1 z2)
       (to (bigfloat:-
	    (bigfloat::bf-erf 
	     (bigfloat:to (add ($bfloat ($realpart z2)) (mul '$%i ($bfloat ($imagpart z2))))))
	    (bigfloat::bf-erf 
	     (bigfloat:to (add ($bfloat ($realpart z1)) (mul '$%i ($bfloat ($imagpart z1)))))))))

      ;; Argument simplification
      
      ((and $trigsign (great (mul -1 z1) z1) (great (mul -1 z2) z2))
       (mul -1 (simplify (list '(%erf_generalized) (mul -1 z1) (mul -1 z2)))))

      ;; Transformation to Erf

      ($erf_representation
       (sub (simplify (list '(%erf) z2)) (simplify (list '(%erf) z1))))

      (t
       (eqtest (list '(%erf_generalized) z1 z2) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Complementary Error function Erfc(z)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $erfc (z)
  (simplify (list '(%erfc) z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop $erfc %erfc alias)
(defprop $erfc %erfc verb)

(defprop %erfc $erfc reversealias)
(defprop %erfc $erfc noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %erfc t commutes-with-conjugate)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %erfc simp-erfc operators)

;;; Complementary Error function distributes over bags

(defprop %erfc (mlist $matrix mequal) distribute_over)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %erfc 
  ((z)
   ((mtimes) -2 
      ((mexpt) $%pi ((rat) -1 2))
      ((mexpt) $%e ((mtimes) -1 ((mexpt) z 2)))))
  grad)

;;; Integral of the Error function erfc

(defprop %erfc
  ((z)
   ((mplus)
     ((mtimes) -1
        ((mexpt) $%pi ((rat) -1 2))
        ((mexpt) $%e ((mtimes) -1 ((mexpt) z 2))))
     ((mtimes) z ((%erfc) z))))
  integral)

;;; ----------------------------------------------------------------------------

(defprop %erfc simplim%erfc simplim%function)

(defun simplim%erfc (expr var val)
  ;; Look for the limit of the arguments.
  (let ((z (limit (cadr expr) var val 'think)))
    (cond
      ;; Handle infinities at this place.
      ((eq z '$inf) 0)
      ((eq z '$minf) 2)
      (t
       ;; All other cases are handled by the simplifier of the function.
       (simplify (list '(%erfc) z))))))

;;; ----------------------------------------------------------------------------

(defun simp-erfc (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond

    ;; Check for specific values

    ((zerop1 z) 1)
    ((eq z '$inf) 0)
    ((eq z '$minf) 2)

    ;; Check for numerical evaluation.

    ((numerical-eval-p z)
     (to (bigfloat::bf-erfc (bigfloat:to z))))

    ;; Argument simplification

    ((taylorize (mop expr) (second expr)))
    ((and $trigsign (great (mul -1 z) z))
     (sub 2 (simplify (list  '(%erfc) (mul -1 z)))))
    
    ;; Representation through equivalent functions
    
    ($hypergeometric_representation
      (sub 1
        (mul 2 z 
             (power '$%pi '((rat simp) 1 2))
             (list '(%hypergeometric simp)
                   (list '(mlist simp) '((rat simp) 1 2))
                   (list '(mlist simp) '((rat simp) 3 2))
                   (mul -1 (power z 2))))))
    
    ;; Transformation to Erf or Erfi

    ((and $erf_representation
          (not (eq $erf_representation '$erfc)))
     (case $erf_representation
       (%erf
        (sub 1 (take '(%erf) z)))
       (%erfi
        (add 1 (mul '$%i (take '(%erfi) (mul '$%i z)))))
       (t
        (eqtest (list '(%erfc) z) expr))))
    
    (t
     (eqtest (list '(%erfc) z) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Imaginary Error function Erfi(z)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $erfi (z)
  (simplify (list '(%erfi) z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop $erfi %erfi alias)
(defprop $erfi %erfi verb)

(defprop %erfi $erfi reversealias)
(defprop %erfi $erfi noun)

;;; erfi has mirror symmetry

(defprop %erfi t commutes-with-conjugate)

;;; erfi is an odd function

(defprop %erfi odd-function-reflect reflection-rule)

;;; erfi is an simplifying function

(defprop %erfi simp-erfi operators)

;;; erfi distributes over bags

(defprop %erfi (mlist $matrix mequal) distribute_over)

;;; Derivative of the Error function erfi

(defprop %erfi
  ((z)
   ((mtimes) 2 
      ((mexpt) $%pi ((rat) -1 2))
      ((mexpt) $%e ((mexpt) z 2))))
  grad)

;;; Integral of the Error function erfi

(defprop %erfi
  ((z)
   ((mplus)
     ((mtimes) -1
        ((mexpt) $%pi ((rat) -1 2))
        ((mexpt) $%e ((mexpt) z 2)))
     ((mtimes) z ((%erfi) z))))
  integral)

;;; ----------------------------------------------------------------------------

(defprop %erfi simplim%erfi simplim%function)

(defun simplim%erfi (expr var val)
  ;; Look for the limit of the arguments.
  (let ((z (limit (cadr expr) var val 'think)))
    (cond
      ;; Handle infinities at this place.
      ((eq z '$inf) '$inf)
      ((eq z '$minf) '$minf)
      (t
       ;; All other cases are handled by the simplifier of the function.
       (simplify (list '(%erfi) z))))))

;;; ----------------------------------------------------------------------------

(in-package :bigfloat)
(defun bf-erfi (z)
  (flet ((erfi (z)
	   ;; Use the relationship erfi(z) = -%i*erf(%i*z)
	   (let* ((iz (complex (- (imagpart z)) (realpart z))) ; %i*z
		  (result (bf-erf iz)))
	     (complex (imagpart result) (- (realpart result))))))
    ;; Take care to return real results when the argument is real.
    (if (realp z)
	(if (minusp z)
	    (- (bf-erfi (- z)))
	    (realpart (erfi z)))
	(erfi z))))

(in-package :maxima)

(defun simp-erfi (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond

    ;; Check for specific values

    ((zerop1 z) z)
    ((eq z '$inf) '$inf)
    ((eq z '$minf) '$minf)

    ;; Check for numerical evaluation. Use erfi(z) = -%i*erf(%i*z).

    ((numerical-eval-p z)
     (to (bigfloat::bf-erfi (bigfloat:to z))))

    ;; Argument simplification

    ((taylorize (mop expr) (second expr)))
    ((and $erf_%iargs
          (multiplep z '$%i))
     (mul '$%i (simplify (list '(%erf) (coeff z '$%i 1)))))
    ((apply-reflection-simp (mop expr) z $trigsign))

    ;; Representation through equivalent functions
    
    ($hypergeometric_representation
      (mul 2 z
        (power '$%pi '((rat simp) 1 2))
               (list '(%hypergeometric simp)
                     (list '(mlist simp) '((rat simp) 1 2))
                     (list '(mlist simp) '((rat simp) 3 2))
                     (power z 2))))
    
    ;; Transformation to Erf or Erfc
    
    ((and $erf_representation
          (not (eq $erf_representation '$erfi)))
     (case $erf_representation
       (%erf
        (mul -1 '$%i (take '(%erf) (mul '$%i z))))
       (%erfc
        (sub (mul '$%i (take '(%erfc) (mul '$%i z))) '$%i))
       (t
        (eqtest (list '(%erfi) z) expr))))
    
    (t
     (eqtest (list '(%erfi) z) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The implementation of the Inverse Error function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $inverse_erf (z)
  (simplify (list '(%inverse_erf) z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set properties to give full support to the parser and display

(defprop $inverse_erf %inverse_erf alias)
(defprop $inverse_erf %inverse_erf verb)

(defprop %inverse_erf $inverse_erf reversealias)
(defprop %inverse_erf $inverse_erf noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The Inverse Error function is a simplifying function

(defprop %inverse_erf simp-inverse-erf operators)

;;; The Inverse Error function distributes over bags

(defprop %inverse_erf (mlist $matrix mequal) distribute_over)

;;; inverse_erf is the inverse of the erf function

(defprop %inverse_erf %erf $inverse)
(defprop %erf %inverse_erf $inverse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Differentiation of the Inverse Error function

(defprop %inverse_erf
  ((z)
   ((mtimes) 
      ((rat) 1 2) 
      ((mexpt) $%pi ((rat) 1 2))
      ((mexpt) $%e ((mexpt) ((%inverse_erf) z) 2))))
  grad)

;;; Integral of the Inverse Error function

(defprop %inverse_erf
    ((z)
     ((mtimes) -1 
        ((mexpt) $%pi ((rat) -1 2))
        ((mexpt) $%e ((mtimes) -1 ((mexpt) ((%inverse_erf) z) 2)))))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We support a simplim%function. The function is looked up in simplimit and 
;;; handles specific values of the function.

(defprop %inverse_erf simplim%inverse_erf simplim%function)

(defun simplim%inverse_erf (expr var val)
  ;; Look for the limit of the argument.
  (let ((z (limit (cadr expr) var val 'think)))
  (cond
    ;; Handle an argument 0 at this place
    ((onep1 z) '$inf)
    ((onep1 (mul -1 z)) '$minf)
    (t
     ;; All other cases are handled by the simplifier of the function.
     (simplify (list '(%inverse_erf) z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
(defun simp-inverse-erf (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond
    ((or (onep1 z)
         (onep1 (mul -1 z)))
     (simp-domain-error 
       (intl:gettext "inverse_erf: inverse_erf(~:M) is undefined.") z))
    ((zerop1 z) z)
    ((numerical-eval-p z)
     (to (bigfloat::bf-inverse-erf (bigfloat:to z))))
    ((taylorize (mop expr) (cadr expr)))
    (t
     (eqtest (list '(%inverse_erf) z) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The implementation of the Inverse Complementary Error function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $inverse_erfc (z)
  (simplify (list '(%inverse_erfc) z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set properties to give full support to the parser and display

(defprop $inverse_erfc %inverse_erfc alias)
(defprop $inverse_erfc %inverse_erfc verb)

(defprop %inverse_erfc $inverse_erfc reversealias)
(defprop %inverse_erfc $inverse_erfc noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Inverse Complementary Error function is a simplifying function

(defprop %inverse_erfc simp-inverse-erfc operators)

;;; Inverse Complementary Error function distributes over bags

(defprop %inverse_erfc (mlist $matrix mequal) distribute_over)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; inverse_erfc is the inverse of the erfc function

(defprop %inverse_erfc %erfc $inverse)
(defprop %erfc %inverse_erfc $inverse)


;;; Differentiation of the Inverse Complementary Error function

(defprop %inverse_erfc
  ((z)
   ((mtimes) 
      ((rat) -1 2) 
      ((mexpt) $%pi ((rat) 1 2))
      ((mexpt) $%e ((mexpt) ((%inverse_erfc) z) 2))))
  grad)

;;; Integral of the Inverse Complementary Error function

(defprop %inverse_erfc
    ((z)
     ((mtimes)
        ((mexpt) $%pi ((rat) -1 2))     
        ((mexpt) $%e
         ((mtimes) -1 ((mexpt) ((%inverse_erfc) z) 2)))))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We support a simplim%function. The function is looked up in simplimit and 
;;; handles specific values of the function.

(defprop %inverse_erfc simplim%inverse_erfc simplim%function)

(defun simplim%inverse_erfc (expr var val)
  ;; Look for the limit of the argument.
  (let ((z (limit (cadr expr) var val 'think)))
  (cond
    ;; Handle an argument 0 at this place
    ((or (zerop1 z) 
         (eq z '$zeroa)
         (eq z '$zerob)) 
     '$inf)
    ((zerop1 (add z -2)) '$minf)
    (t
     ;; All other cases are handled by the simplifier of the function.
     (simplify (list '(%inverse_erfc) z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
(defun simp-inverse-erfc (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond
    ((or (zerop1 z)
         (zerop1 (add z -2)))
     (simp-domain-error 
       (intl:gettext "inverse_erfc: inverse_erfc(~:M) is undefined.") z))
    ((onep1 z) 0)
    ((numerical-eval-p z)
     (to (bigfloat::bf-inverse-erfc (bigfloat:to z))))
    ((taylorize (mop expr) (cadr expr)))
    (t
     (eqtest (list '(%inverse_erfc) z) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Implementation of the Newton algorithm to calculate numerical values
;;; of the Inverse Error functions in float or bigfloat precision.
;;; The algorithm is a modified version of the routine in newton1.mac.

(defvar *debug-newton* nil)
(defvar *newton-maxcount* 1000)
(defvar *newton-epsilon-factor* 50)

(defun float-newton (expr var x0 eps)
  (do ((s (sdiff expr var))
       (xn x0)
       (sn)
       (count 0 (+ count 1)))
      ((> count *newton-maxcount*)
       (merror 
         (intl:gettext "float-newton: Newton does not converge for ~:M") expr))
    (setq sn ($float (maxima-substitute xn var expr)))
    (when (< (abs sn) eps) (return xn))
    (when *debug-newton* (format t "~&xn = ~A~%" xn))
    (setq xn ($float (sub xn (div sn (maxima-substitute xn var s)))))))

(defun bfloat-newton (expr var x0 eps)
  (do ((s (sdiff expr var))
       (xn x0)
       (sn)
       (count 0 (+ count 1)))
      ((> count *newton-maxcount*)
       (merror 
         (intl:gettext "bfloat-newton: Newton does not converge for ~:M") expr))
    (setq sn ($bfloat (maxima-substitute xn var expr)))
    (when (eq ($sign (sub (simplify (list '(mabs) sn)) eps)) '$neg)
      (return xn))
    (when *debug-newton* (format t "~&xn = ~A~%" xn))
    (setq xn ($bfloat (sub xn (div sn (maxima-substitute xn var s)))))))


(in-package :bigfloat)

;; Compute inverse_erf(z) for z a real or complex number, including
;; bigfloat objects.  The value is computing using a Newton iteration
;; to solve erf(x) = z.
(defun bf-inverse-erf (z)
  (cond ((zerop z)
	 z)
	((= (abs z) 1)
	 (maxima::merror
	  (intl:gettext "bf-inverse-erf: inverse_erf(~M) is undefined")
	  z))
	((minusp (realpart z))
	 ;; inverse_erf is odd because erf is.
	 (- (bf-inverse-erf (- z))))
	(t
	 (labels
	     ((approx (z)
		;; Find an approximate solution for x = inverse_erf(z).
		(let ((result
			(cond ((<= (abs z) 1)
			       ;; For small z, inverse_erf(z) = z*sqrt(%pi)/2
			       ;; + O(z^3).  Thus, x = z*sqrt(%pi)/2 is our
			       ;; initial starting point.
			       (* z (sqrt (%pi z)) 1/2))
			      (t
			       ;; For |z| > 1 and realpart(z) >= 0, we have
			       ;; the asymptotic series z = erf(x) = 1 -
			       ;; exp(-x^2)/x/sqrt(%pi).
			       ;;
			       ;; Then
			       ;;   x = sqrt(-log(x*sqrt(%pi)*(1-z))
			       ;;
			       ;; We can use this as a fixed-point iteration
			       ;; to find x, and we can start the iteration at
			       ;; x = 1.  Just do one more iteration.  I (RLT)
			       ;; think that's close enough to get the Newton
			       ;; algorithm to converge.
			       (let* ((sp (sqrt (%pi z)))
				      (a (sqrt (- (log (* sp (- 1 z)))))))
				 (setf a (sqrt (- (log (* a sp (- 1 z))))))
				 (setf a (sqrt (- (log (* a sp (- 1 z)))))))))))
		  (when maxima::*debug-newton*
		    (format t "approx = ~S~%" result))
		  result)))
	   (let ((two/sqrt-pi (/ 2 (sqrt (%pi z))))
		 (eps
		   ;; Try to pick a reasonable epsilon value for the
		   ;; Newton iteration.
		   (cond ((<= (abs z) 1)
			  (typecase z
			    (cl:real (* 2 maxima::flonum-epsilon))
			    (t (* maxima::*newton-epsilon-factor* (epsilon z)))))
			 (t
			  (* maxima::*newton-epsilon-factor* (epsilon z))))))
	     (when maxima::*debug-newton*
	       (format t "eps = ~S~%" eps))
	     (flet ((diff (x)
		      ;; Derivative of erf(x)
		      (* two/sqrt-pi (exp (- (* x x))))))
	       (bf-newton #'bf-erf
			  #'diff
			  z
			  (approx z)
			  eps)))))))

(defun bf-inverse-erfc (z)
  (cond ((zerop z)
	 (maxima::merror
	  (intl:gettext "bf-inverse-erf: inverse_erf(~M) is undefined")
	  z))
	((= z 1)
	 (float 0 z))
	(t
	 (flet
	     ((approx (z)
		;; Find an approximate solution for x =
		;; inverse_erfc(z).  We have inverse_erfc(z) =
		;; inverse_erf(1-z), so that's a good starting point.
		;; We don't need full precision, so a float value is
		;; good enough.  But if 1-z is 1, inverse_erf is
		;; undefined, so we need to do something else.
		(let ((result
			(let ((1-z (float (- 1 z) 0.0)))
			  (cond ((= 1 1-z)
				 (if (minusp (realpart z))
				     (bf-inverse-erf (+ 1 (* 5 maxima::flonum-epsilon)))
				     (bf-inverse-erf (- 1 (* 5 maxima::flonum-epsilon)))))
				(t
				 (bf-inverse-erf 1-z))))))
		  (when maxima::*debug-newton*
		    (format t "approx = ~S~%" result))
		  result)))
	   (let ((-two/sqrt-pi (/ -2 (sqrt (%pi z))))
		 (eps (* maxima::*newton-epsilon-factor* (epsilon z))))
	     (when maxima::*debug-newton*
	       (format t "eps = ~S~%" eps))
	     (flet ((diff (x)
		      ;; Derivative of erfc(x)
		      (* -two/sqrt-pi (exp (- (* x x))))))
	       (bf-newton #'bf-erfc
			  #'diff
			  z
			  (approx z)
			  eps)))))))

;; Newton iteration for solving f(x) = z, given f and the derivative
;; of f.
(defun bf-newton (f df z start eps)
  (do ((x start)
       (delta (/ (- (funcall f start) z)
		 (funcall df start))
	      (/ (- (funcall f x) z)
		 (funcall df x)))
       (count 0 (1+ count)))
      ((or (< (abs delta) (* (abs x) eps))
	   (> count maxima::*newton-maxcount*))
       (if (> count maxima::*newton-maxcount*)
	   (maxima::merror 
	    (intl:gettext "bf-newton: failed to converge after ~M iterations: delta = ~S,  x = ~S")
	    count delta x)
	   x))
    (when maxima::*debug-newton*
      (format t "x = ~S, abs(delta) = ~S relerr = ~S~%"
	      x (abs delta) (/ (abs delta) (abs x))))
    (setf x (- x delta))))

(in-package :maxima)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Fresnel Integral S(z)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $fresnel_s (z)
  (simplify (list '(%fresnel_s) z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set properties to give full support to the parser and display

(defprop $fresnel_s %fresnel_s alias)
(defprop $fresnel_s %fresnel_s verb)

(defprop %fresnel_s $fresnel_s reversealias)
(defprop %fresnel_s $fresnel_s noun)

;;; fresnel_s is a simplifying function

(defprop %fresnel_s simp-fresnel-s operators)

;;; fresnel_s distributes over bags

(defprop %fresnel_s (mlist $matrix mequal) distribute_over)

;;; fresnel_s has mirror symmetry

(defprop %fresnel_s t commutes-with-conjugate)

;;; fresnel_s is an odd function
;;;
;;; Maxima has two mechanism to define a reflection property
;;; 1. Declare the feature oddfun or evenfun
;;; 2. Put a reflection rule on the property list
;;;
;;; The second way is used for the trig functions. We use it here too.

;;; This would be the first way to give the property of an odd function.
;(eval-when
;    #+gcl (load eval)
;    #-gcl (:load-toplevel :execute)
;    (let (($context '$global) (context '$global))
;      (meval '(($declare) %fresnel_s $oddfun))))

(defprop %fresnel_s odd-function-reflect reflection-rule)

;;; Differentiation of the Fresnel Integral S

(defprop %fresnel_s
  ((z)
   ((%sin) ((mtimes) ((rat) 1 2) $%pi ((mexpt) z 2))))
  grad)

;;; Integration of the Fresnel Integral S

(defprop %fresnel_s
  ((z)
   ((mplus) 
      ((mtimes) z ((%fresnel_s) z))
      ((mtimes) 
         ((mexpt) $%pi -1)
         ((%cos) ((mtimes) ((rat) 1 2) $%pi ((mexpt) z 2))))))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *fresnel-maxit* 1000)
(defvar *fresnel-eps*   (* 2 flonum-epsilon))
(defvar *fresnel-min*   1e-32)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :bigfloat)

(defun bf-fresnel (z)
  (let* ((eps (epsilon z))
         (maxit maxima::*fresnel-maxit*)
         (xmin 1.5)
	 (bf-pi (%pi z))
	 ;; For very small x, we have
	 ;;  fresnel_s(x) = %pi/6*z^3
	 ;;  fresnel_c(x) = x
         (s (* (/ bf-pi 6) z z z))
         (c z))
    (when (> (abs z) eps)
      (cond
        ((< (abs z) xmin)
         (when maxima::*debug-gamma*
           (format t "~& in FRESNEL series expansion.~%"))
         (let ((sums 0) (sumc z))
           (do ((sum 0)
                (sign 1)
                (fact (* (/ bf-pi 2) (* z z)))
                (term z)
                (odd t (not odd))
                (test 0)
                (n 3 (+ n 2))
                (k 1 (+ k 1)))
               ((> k maxit)
		(maxima::merror (intl:gettext "fresnel: series expansion failed for (COMPLEX-BFLOAT-FRESNEL ~:M).") z))
             (setq term (* term (/ fact k)))
             (setq sum (+ sum (/ (* sign term) n)))
             (setq test (* (abs sum) eps))
             (if odd
		 (progn
		   (setq sign (- sign))
		   (setq sums sum)
		   (setq sum sumc))
		 (progn
		   (setq sumc sum)
		   (setq sum sums)))
             (if (< (abs term) test)
		 (return)))
           (setq s sums)
           (setq c sumc)))
        (t
         (let* ((sqrt-pi (sqrt bf-pi))
		(z+ (* (complex 1/2 1/2)
		       (* sqrt-pi
			  z)))
                (z- (* (complex 1/2 -1/2)
		       (* sqrt-pi
			  z)))
                (erf+ (bf-erf z+))
                (erf- (bf-erf z-)))
           (setq s (* (complex 1/4 1/4)
		      (+ erf+ (* (complex 0 -1) erf-))))
           (setq c (* (complex 1/4 -1/4)
		      (+ erf+ (* (complex 0 1) erf-))))))))
    (values s c)))

(defun bf-fresnel-s (z)
  (if (and (complexp z) (zerop (realpart z)))
      ;; A pure imaginary argument. Use fresnel_s(%i*x)=-%i*fresnel_s(x).
      (complex 0
	       (- (bf-fresnel-s (imagpart z))))
      (let ((fs (bf-fresnel z)))
	(if (realp z)
	    (realpart fs)
	    fs))))

(defun bf-fresnel-c (z)
  (if (and (complexp z) (zerop (realpart z)))
      ;; A pure imaginary argument. Use fresnel_c(%i*x)=%i*fresnel_c(x).
      (complex 0
	       (bf-fresnel-c (imagpart z)))
      (let ((fc (nth-value 1 (bf-fresnel z))))
	(if (realp z)
	    (realpart fc)
	    fc))))

(in-package :maxima)
(defun simp-fresnel-s (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond

    ;; Check for specific values

    ((zerop1 z) z)
    ((eq z '$inf)  '((rat simp) 1 2))
    ((eq z '$minf) '((rat simp) -1 2))
    
    ;; Check for numerical evaluation
    ((numerical-eval-p z)
     (to (bigfloat::bf-fresnel-s (bigfloat::to z))))

    ;; Check for argument simplification

    ((taylorize (mop expr) (second expr)))
    ((and $%iargs (multiplep z '$%i))
     (mul -1 '$%i (simplify (list '(%fresnel_s) (coeff z '$%i 1)))))
    ((apply-reflection-simp (mop expr) z $trigsign))

    ;; Representation through equivalent functions

    ($erf_representation
      (mul
        (div (add 1 '$%i) 4)
        (add
          (simplify 
            (list 
              '(%erf) 
              (mul (div (add 1 '$%i) 2) (power '$%pi '((rat simp) 1 2)) z)))
          (mul -1 '$%i
            (simplify 
              (list 
                '(%erf) 
                (mul (div (sub 1 '$%i) 2) 
                     (power '$%pi '((rat simp) 1 2)) z)))))))

    ($hypergeometric_representation
      (mul (div (mul '$%pi (power z 3)) 6)
           (take '($hypergeometric)
                 (list '(mlist) (div 3 4))
                 (list '(mlist) (div 3 2) (div 7 4))
                 (mul -1 (div (mul (power '$%pi 2) (power z 4)) 16)))))

    (t
     (eqtest (list '(%fresnel_s) z) expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Fresnel Integral C(z)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $fresnel_c (z)
  (simplify (list '(%fresnel_c) z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Set properties to give full support to the parser and display

(defprop $fresnel_c %fresnel_c alias)
(defprop $fresnel_c %fresnel_c verb)

(defprop %fresnel_c $fresnel_c reversealias)
(defprop %fresnel_c $fresnel_c noun)

;;; fresnel_c is a simplifying function

(defprop %fresnel_c simp-fresnel-c operators)

;;; fresnel_c distributes over bags

(defprop %fresnel_c (mlist $matrix mequal) distribute_over)

;;; fresnel_c has mirror symmetry

(defprop %fresnel_c t commutes-with-conjugate)

;;; fresnel_c is an odd function
;;; Maxima has two mechanism to define a reflection property
;;; 1. Declare the feature oddfun or evenfun
;;; 2. Put a reflection rule on the property list
;;;
;;; The second way is used for the trig functions. We use it here too.

(defprop %fresnel_c odd-function-reflect reflection-rule)

;;; Differentiation of the Fresnel Integral C

(defprop %fresnel_c
  ((z)
   ((%cos) ((mtimes) ((rat) 1 2) $%pi ((mexpt) z 2))))
  grad)

;;; Integration of the Fresnel Integral C

(defprop %fresnel_c
  ((z)
   ((mplus) 
      ((mtimes) z ((%fresnel_c) z))
      ((mtimes) -1 
         ((mexpt) $%pi -1)
         ((%sin) ((mtimes) ((rat) 1 2) $%pi ((mexpt) z 2))))))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-fresnel-c (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (cond

    ;; Check for specific values

    ((zerop1 z) z)
    ((eq z '$inf)  '((rat simp) 1 2))
    ((eq z '$minf) '((rat simp) -1 2))
    
    ;; Check for numerical evaluation
    ((numerical-eval-p z)
     (to (bigfloat::bf-fresnel-c (bigfloat::to z))))


    ;; Check for argument simplification

    ((taylorize (mop expr) (second expr)))
    ((and $%iargs (multiplep z '$%i))
     (mul '$%i (simplify (list '(%fresnel_c) (coeff z '$%i 1)))))
    ((apply-reflection-simp (mop expr) z $trigsign))

    ;; Representation through equivalent functions

    ($erf_representation
      (mul
        (div (sub 1 '$%i) 4)
        (add
          (simplify 
            (list 
              '(%erf) 
              (mul (div (add 1 '$%i) 2) (power '$%pi '((rat simp) 1 2)) z)))
          (mul '$%i
            (simplify 
              (list 
                '(%erf) 
                (mul (div (sub 1 '$%i) 2) 
                     (power '$%pi '((rat simp) 1 2)) z)))))))

    ($hypergeometric_representation
      (mul z
           (take '($hypergeometric)
                 (list '(mlist) (div 1 4))
                 (list '(mlist) (div 1 2) (div 5 4))
                 (mul -1 (div (mul (power '$%pi 2) (power z 4)) 16)))))

    (t
      (eqtest (list '(%fresnel_c) z) expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Beta function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The code for the implementation of the beta function is in the files
;;; csimp2.lisp, simp.lisp and mactex.lisp.
;;; At this place we only implement the operator property SYMMETRIC.

;;; Beta is symmetric beta(a,b) = beta(b,a)

(eval-when
    #+gcl (load eval)
    #-gcl (:load-toplevel :execute)
    (let (($context '$global) (context '$global))
      (meval '(($declare) $beta $symmetric))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Incomplete Beta function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *beta-incomplete-maxit* 10000)
(defvar *beta-incomplete-eps* 1.0e-15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $beta_incomplete (a b z)
  (simplify (list '(%beta_incomplete) a b z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop $beta_incomplete %beta_incomplete alias)
(defprop $beta_incomplete %beta_incomplete verb)

(defprop %beta_incomplete $beta_incomplete reversealias)
(defprop %beta_incomplete $beta_incomplete noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %beta_incomplete simp-beta-incomplete operators)

;;; beta_incomplete distributes over bags

(defprop %beta_incomplete (mlist $matrix mequal) distribute_over)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %beta_incomplete
  ((a b z)
   ;; Derivative wrt a
   ((mplus) 
      ((%beta_incomplete) a b z)
      ((mtimes) -1 
         ((mexpt) ((%gamma) a) 2)
         (($hypergeometric_regularized)
           ((mlist) a a ((mplus) 1 ((mtimes) -1 b)))
           ((mlist) ((mplus) 1 a) ((mplus) 1 a)) 
           z)
         ((mexpt) z a)))
   ;; Derivative wrt b
   ((mplus)
      ((mtimes) 
         (($beta) a b)
         ((mplus) 
            ((mqapply) 
               (($psi array 0) b)
               ((mtimes) -1 ((mqapply) (($psi array) 0) ((mplus) a b)))))
         ((mtimes) -1
            ((%beta_incomplete) b a ((mplus) 1 ((mtimes) -1 z)))
            ((%log) ((mplus) 1 ((mtimes) -1 z))))
         ((mtimes) 
            ((mexpt) ((%gamma) b) 2)
            (($hypergeometric_regularized)
               ((mlist) b b ((mplus) 1 ((mtimes) -1 a)))
               ((mlist) ((mplus) 1 b) ((mplus) 1 b))
               ((mplus) 1 ((mtimes) -1 z)))
            ((mexpt) ((mplus) 1 ((mtimes) -1 z)) b))))
   ;; The derivative wrt z
   ((mtimes)
      ((mexpt) ((mplus) 1 ((mtimes) -1 z)) ((mplus) -1 b))
      ((mexpt) z ((mplus) -1 a))))
  grad)

;;; Integral of the Incomplete Beta function

(defprop %beta_incomplete
  ((a b z)
   nil 
   nil
   ((mplus)
      ((mtimes) -1 ((%beta_incomplete) ((mplus) 1 a) b z))
      ((mtimes) ((%beta_incomplete) a b z) z)))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-beta-incomplete (expr ignored simpflag)
  (declare (ignore ignored))
  (if (not (= (length expr) 4)) (wna-err '$beta_incomplete))
  (let ((a (simpcheck (cadr expr)   simpflag))
        (b (simpcheck (caddr expr)  simpflag))
        (z (simpcheck (cadddr expr) simpflag)))
    (when *debug-gamma* 
         (format t "~&SIMP-BETA-INCOMPLETE:~%")
         (format t "~&   : a = ~A~%" a)
         (format t "~&   : b = ~A~%" b)
         (format t "~&   : z = ~A~%" z))
    (cond

      ;; Check for specific values

      ((zerop1 z)
       (let ((sgn ($sign ($realpart a))))
         (cond ((member sgn '($neg $zero))
                (simp-domain-error 
                  (intl:gettext 
                    "beta_incomplete: beta_incomplete(~:M,~:M,~:M) is undefined.") 
                    a b z))
               ((member sgn '($pos $pz)) 
                z)
               (t 
                (eqtest (list '(%beta_incomplete) a b z) expr)))))

      ((and (onep1 z) (eq ($sign ($realpart b)) '$pos))
       ;; z=1 and realpart(b)>0. Simplify to a Beta function.
       ;; If we have to evaluate numerically preserve the type.
       (cond
         ((complex-float-numerical-eval-p a b z)
          (simplify (list '($beta) ($float a) ($float b))))
         ((complex-bigfloat-numerical-eval-p a b z)
          (simplify (list '($beta) ($bfloat a) ($bfloat b))))
         (t
          (simplify (list '($beta) a b)))))
      
      ((or (zerop1 a)
           (and (integer-representation-p a)
                (eq ($sign a) '$neg)
                (or (and (mnump b) 
                         (not (integer-representation-p b)))
                    (eq ($sign (add a b)) '$pos))))
       ;; The argument a is zero or a is negative and the argument b is
       ;; not in a valid range. Beta incomplete is undefined.
       ;; It would be more correct to return Complex infinity.
       (simp-domain-error 
         (intl:gettext 
           "beta_incomplete: beta_incomplete(~:M,~:M,~:M) is undefined.") a b z))

      ;; Check for numerical evaluation in Float or Bigfloat precision

      ((complex-float-numerical-eval-p a b z)
       (cond
         ((not (and (integer-representation-p a) (< a 0.0)))
          (let ((*beta-incomplete-eps* (bigfloat:epsilon ($float 1.0))))
            (beta-incomplete ($float a) ($float b) ($float z))))
         (t
           ;; Negative integer a and b is in a valid range. Expand.
           ($rectform 
             (beta-incomplete-expand-negative-integer 
               (truncate a) ($float b) ($float z))))))
           
      ((complex-bigfloat-numerical-eval-p a b z)
       (cond
         ((not (and (integer-representation-p a) (eq ($sign a) '$neg)))
          (let ((*beta-incomplete-eps*
                  (bigfloat:epsilon (bigfloat:bigfloat 1.0))))
            (beta-incomplete ($bfloat a) ($bfloat b) ($bfloat z))))
         (t
           ;; Negative integer a and b is in a valid range. Expand.          
           ($rectform
             (beta-incomplete-expand-negative-integer
               ($truncate a) ($bfloat b) ($bfloat z))))))

      ;; Argument simplifications and transformations
      
      ((and (integerp b) 
            (plusp b)
            (or (not (integerp a))
                (plusp a)))
       ;; Expand for b a positive integer and a not a negative integer.
       (mul
         (simplify (list '($beta) a b))
         (power z a)
         (let ((index (gensumindex)))
           (dosum
             (div
               (mul
                 (simplify (list '($pochhammer) a index))
                 (power (sub 1 z) index))
              (simplify (list '(mfactorial) index)))
             index 0 (sub b 1) t))))
      
      ((and (integerp a) (plusp a))
       ;; Expand for a a positive integer.
       (mul
         (simplify (list '($beta) a b))
         (sub 1
           (mul
             (power (sub 1 z) b)
             (let ((index (gensumindex)))
               (dosum 
                 (div
                   (mul
                     (simplify (list '($pochhammer) b index))
                     (power z index))
                 (simplify (list '(mfactorial) index)))
               index 0 (sub a 1) t))))))
      
      ((and (integerp a) (minusp a) (integerp b) (plusp b) (<= b (- a)))
       ;; Expand for a a negative integer and b an integer with b <= -a.
       (mul
         (power z a)
         (let ((index (gensumindex)))
           (dosum
             (div
               (mul (simplify (list '($pochhammer) (sub 1 b) index))
                    (power z index))
               (mul (add index a)
                    (simplify (list '(mfactorial) index))))
             index 0 (sub b 1) t))))

      ((and $beta_expand (mplusp a) (integerp (cadr a)) (plusp (cadr a)))
       (let ((n (cadr a))
             (a (simplify (cons '(mplus) (cddr a)))))
         (sub
           (mul
             (div
               (simplify (list '($pochhammer) a n))
               (simplify (list '($pochhammer) (add a b) n)))
             ($beta_incomplete a b z))
           (mul
             (power (add a b n -1) -1)
             (let ((index (gensumindex)))
               (dosum
                 (mul
                   (div
                     (simplify (list '($pochhammer) 
                                     (add 1 (mul -1 a) (mul -1 n))
                                     index))
                     (simplify (list '($pochhammer)
                                     (add 2 (mul -1 a) (mul -1 b) (mul -1 n))
                                     index)))
                   (mul (power (sub 1 z) b)
                        (power z (add a n (mul -1 index) -1))))
                index 0 (sub n 1) t))))))

      ((and $beta_expand (mplusp a) (integerp (cadr a)) (minusp (cadr a)))
       (let ((n (- (cadr a)))
             (a (simplify (cons '(mplus) (cddr a)))))
         (sub
           (mul
             (div
               (simplify (list '($pochhammer) (add 1 (mul -1 a) (mul -1 b)) n))
               (simplify (list '($pochhammer) (sub 1 a) n)))
             ($beta_incomplete a b z))
           (mul
             (div
               (simplify 
                 (list '($pochhammer) (add 2 (mul -1 a) (mul -1 b)) (sub n 1)))
               (simplify (list '($pochhammer) (sub 1 a) n)))
             (let ((index (gensumindex)))
               (dosum
                 (mul
                   (div
                     (simplify (list '($pochhammer) (sub 1 a) index))
                     (simplify (list '($pochhammer)
                                     (add 2 (mul -1 a) (mul -1 b))
                                     index)))
                   (mul (power (sub 1 z) b)
                        (power z (add a (mul -1 index) -1))))
                index 0 (sub n 1) t))))))
      
      (t
       (eqtest (list '(%beta_incomplete) a b z) expr)))))

(defun beta-incomplete-expand-negative-integer (a b z)
  (mul
    (power z a)
    (let ((index (gensumindex)))
      (dosum
        (div
          (mul (simplify (list '($pochhammer) (sub 1 b) index)) 
               (power z index))
          (mul (add index a) (simplify (list '(mfactorial) index))))
        index 0 (sub b 1) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun beta-incomplete (a b z)
  (cond
    ((eq ($sign (sub (mul ($realpart z) ($realpart (add a b 2)))
                     ($realpart (add a 1))))
         '$pos)
     ($rectform
       (sub
         (simplify (list '($beta) a b))
         (to (numeric-beta-incomplete b a (sub 1.0 z))))))
    (t
      (to (numeric-beta-incomplete a b z)))))

(defun numeric-beta-incomplete (a b z)
  (when *debug-gamma*
    (format t "~&NUMERIC-BETA-INCOMPLETE enters continued fractions~%"))
  (let ((a (bigfloat:to a))
        (b (bigfloat:to b))
        (z (bigfloat:to z)))
  (do* ((beta-maxit *beta-incomplete-maxit*)
        (beta-eps   *beta-incomplete-eps*)
        (beta-min   (bigfloat:* beta-eps beta-eps))
        (ab (bigfloat:+ a b))
        (ap (bigfloat:+ a 1.0))
        (am (bigfloat:- a 1.0))
        (c  1.0)
        (d  (bigfloat:- 1.0 (bigfloat:/ (bigfloat:* z ab) ap)))
        (d  (if (bigfloat:< (bigfloat:abs d) beta-min) beta-min d))
        (d  (bigfloat:/ 1.0 d))
        (h  d)
        (aa 0.0)
        (de 0.0)
        (m2 0)
        (m 1 (+ m 1)))
       ((> m beta-maxit)
        (merror (intl:gettext "beta_incomplete: continued fractions failed for beta_incomplete(~:M, ~:M, ~:M).") a b z))
    (setq m2 (+ m m))
    (setq aa (bigfloat:/ (bigfloat:* m z (bigfloat:- b m))
                         (bigfloat:* (bigfloat:+ am m2)
                                     (bigfloat:+ a m2))))
    (setq d  (bigfloat:+ 1.0 (bigfloat:* aa d)))
    (when (bigfloat:< (bigfloat:abs d) beta-min) (setq d beta-min))
    (setq c (bigfloat:+ 1.0 (bigfloat:/ aa c)))
    (when (bigfloat:< (bigfloat:abs c) beta-min) (setq c beta-min))
    (setq d (bigfloat:/ 1.0 d))
    (setq h (bigfloat:* h d c))
    (setq aa (bigfloat:/ (bigfloat:* -1 
                                     (bigfloat:+ a m) 
                                     (bigfloat:+ ab m) z) 
                         (bigfloat:* (bigfloat:+ a m2) 
                                     (bigfloat:+ ap m2))))
    (setq d (bigfloat:+ 1.0 (bigfloat:* aa d)))
    (when (bigfloat:< (bigfloat:abs d) beta-min) (setq d beta-min))
    (setq c (bigfloat:+ 1.0 (bigfloat:/ aa c)))
    (when (bigfloat:< (bigfloat:abs c) beta-min) (setq c beta-min))
    (setq d (bigfloat:/ 1.0 d))
    (setq de (bigfloat:* d c))
    (setq h (bigfloat:* h de))
    (when (bigfloat:< (bigfloat:abs (bigfloat:- de 1.0)) beta-eps)
      (when *debug-gamma* 
        (format t "~&Continued fractions finished.~%")
        (format t "~&  z = ~A~%" z)
        (format t "~&  a = ~A~%" a)
        (format t "~&  b = ~A~%" b)
        (format t "~&  h = ~A~%" h))
      (return
        (let ((result
          (bigfloat:/ 
            (bigfloat:* h 
                        (bigfloat:expt z a)
                        (bigfloat:expt (bigfloat:- 1.0 z) b)) a)))
          (when *debug-gamma*
            (format t "~& result = ~A~%" result))
          result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Generalized Incomplete Beta function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $beta_incomplete_generalized (a b z1 z2)
  (simplify (list '(%beta_incomplete_generalized) a b z1 z2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop $beta_incomplete_generalized %beta_incomplete_generalized alias)
(defprop $beta_incomplete_generalized %beta_incomplete_generalized verb)

(defprop %beta_incomplete_generalized $beta_incomplete_generalized reversealias)
(defprop %beta_incomplete_generalized $beta_incomplete_generalized noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %beta_incomplete_generalized 
         simp-beta-incomplete-generalized operators)

;;; beta_incomplete_generalized distributes over bags

(defprop %beta_incomplete_generalized (mlist $matrix mequal) distribute_over)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Generalized Incomplete Gamma function has not mirror symmetry for z1 or z2 
;;; but not on the negative real axis and for z1 or z2 real and > 1.
;;; We support a conjugate-function which test these cases.

(defprop %beta_incomplete_generalized
         conjugate-beta-incomplete-generalized conjugate-function)

(defun conjugate-beta-incomplete-generalized (args)
  (let ((a  (first args))
        (b  (second args))
        (z1 (third args))
        (z2 (fourth args)))
    (cond ((and (off-negative-real-axisp z1)
                (not (and (zerop1 ($imagpart z1))
                          (eq ($sign (sub ($realpart z1) 1)) '$pos)))
                (off-negative-real-axisp z2)
                (not (and (zerop1 ($imagpart z2))
                          (eq ($sign (sub ($realpart z2) 1)) '$pos))))
           (simplify
             (list
              '(%beta_incomplete_generalized)
               (simplify (list '($conjugate) a))
               (simplify (list '($conjugate) b))
               (simplify (list '($conjugate) z1))
               (simplify (list '($conjugate) z2)))))
          (t
           (list
            '($conjugate simp)
             (simplify (list '(%beta_incomplete_generalized)
                             a b z1 z2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %beta_incomplete_generalized
  ((a b z1 z2)
   ;; Derivative wrt a
   ((mplus)
      ((mtimes) -1 
         ((%beta_incomplete) a b z1)
         ((%log) z1))
      ((mtimes) 
         ((mexpt) ((%gamma) a) 2)
         ((mplus)
            ((mtimes)
               (($hypergeometric_regularized)
                  ((mlist) a a ((mplus) 1 ((mtimes) -1 b)))
                  ((mlist) ((mplus) 1 a) ((mplus) 1 a)) 
                  z1)
               ((mexpt) z1 1))
            ((mtimes) -1
               (($hypergeometric_regularized)
                  ((mlist) a a ((mplus) 1 ((mtimes) -1 b)))
                  ((mlist) ((mplus) 1 a) ((mplus) 1 a)) 
                  z2)
               ((mexpt) z2 a))))
      ((mtimes) ((%beta_incomplete) a b z2) ((%log) z2)))
   ;; Derivative wrt b
   ((mplus)
      ((mtimes)
         ((%beta_incomplete) b a ((mplus) 1 ((mtimes) -1 z1)))
         ((%log) ((mplus) 1 ((mtimes) -1 z1))))
      ((mtimes) -1
         ((%beta_incomplete) b a ((mplus) 1 ((mtimes) -1 z2)))
         ((%log) ((mplus) 1 ((mtimes) -1 z2))))
      ((mtimes) -1 
         ((mexpt) ((%gamma) b) 2)
         ((mplus)
            ((mtimes)
               (($hypergeometric_regularized)
                  ((mlist) b b ((mplus) 1 ((mtimes) -1 a)))
                  ((mlist) ((mplus) 1 b) ((mplus) 1 b))
                  ((mplus) 1 ((mtimes) -1 z1)))
               ((mexpt) ((mplus) 1 ((mtimes) -1 z1)) b))
            ((mtimes) -1
               (($hypergeometric_regularized)
                  ((mlist) b b ((mplus) 1 ((mtimes) -1 a)))
                  ((mlist) ((mplus) 1 b) ((mplus) 1 b))
                  ((mplus) 1 ((mtimes) -1 z2)))
               ((mexpt) ((mplus) 1 ((mtimes) -1 z2)) b)))))
   ;; The derivative wrt z1
   ((mtimes) -1
      ((mexpt) 
         ((mplus) 1 ((mtimes) -1 z1))
         ((mplus) -1 b))
      ((mexpt) z1 ((mplus) -1 a)))
   ;; The derivative wrt z2
   ((mtimes)
      ((mexpt) 
         ((mplus) 1 ((mtimes) -1 z2))
         ((mplus) -1 b))
      ((mexpt) z2 ((mplus) -1 a))))
  grad)

;;; Integral of the Incomplete Beta function

(defprop %beta_incomplete_generalized
  ((a b z1 z2)
   nil 
   nil
   ;; Integral wrt z1
   ((mplus) 
      ((%beta_incomplete) ((mplus) 1 a) b z1)
      ((mtimes) ((%beta_incomplete_generalized) a b z1 z2) z1))
   ;; Integral wrt z2
   ((mplus)
      ((mtimes) -1
         ((%beta_incomplete) ((mplus) 1 a) b z2))
      ((mtimes) ((%beta_incomplete_generalized) a b z1 z2) z2)))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-beta-incomplete-generalized (expr ignored simpflag)
  (declare (ignore ignored))
  (if (not (= (length expr) 5)) (wna-err '$beta_incomplete_generalized))
  (let ((a  (simpcheck (second expr) simpflag))
        (b  (simpcheck (third expr)  simpflag))
        (z1 (simpcheck (fourth expr) simpflag))
        (z2 (simpcheck (fifth expr)  simpflag)))
    (cond

      ;; Check for specific values

      ((zerop1 z2)
       (let ((sgn ($sign ($realpart a))))
         (cond ((eq sgn '$neg)
                (simp-domain-error 
                  (intl:gettext 
                    "beta_incomplete_generalized: beta_incomplete_generalized(~:M,~:M,~:M,~:M) is undefined.") 
                    a b z1 z2))
               ((member sgn '($pos $pz)) 
                (mul -1 ($beta_incomplete a b z1)))
               (t 
                (eqtest 
                  (list '(%beta_incomplete_generalized) a b z1 z2) expr)))))

      ((zerop1 z1)
       (let ((sgn ($sign ($realpart a))))
         (cond ((eq sgn '$neg)
                (simp-domain-error 
                  (intl:gettext 
                    "beta_incomplete_generalized: beta_incomplete_generalized(~:M,~:M,~:M,~:M) is undefined.") 
                    a b z1 z2))
               ((member sgn '($pos $pz)) 
                (mul -1 ($beta_incomplete a b z2)))
               (t 
                (eqtest 
                  (list '(%beta_incomplete_generalized) a b z1 z2) expr)))))

      ((and (onep1 z2) (or (not (mnump a)) (not (mnump b)) (not (mnump z1))))
       (let ((sgn ($sign ($realpart b))))
         (cond ((member sgn '($pos $pz)) 
                (sub (simplify (list '($beta) a b))
                     ($beta_incomplete a b z1)))
               (t 
                (eqtest 
                  (list '(%beta_incomplete_generalized) a b z1 z2) expr)))))

      ((and (onep1 z1) (or (not (mnump a)) (not (mnump b)) (not (mnump z2))))
       (let ((sgn ($sign ($realpart b))))
         (cond ((member sgn '($pos $pz)) 
                (sub ($beta_incomplete a b z2) 
                     (simplify (list '($beta) a b))))
               (t 
                (eqtest 
                  (list '(%beta_incomplete_generalized) a b z1 z2) expr)))))

      ;; Check for numerical evaluation in Float or Bigfloat precision

      ((complex-float-numerical-eval-p a b z1 z2)
       (let ((*beta-incomplete-eps* (bigfloat:epsilon ($float 1.0))))
         (sub (beta-incomplete ($float a) ($float b) ($float z2))
              (beta-incomplete ($float a) ($float b) ($float z1)))))
           
      ((complex-bigfloat-numerical-eval-p a b z1 z2)
       (let ((*beta-incomplete-eps*
               (bigfloat:epsilon (bigfloat:bigfloat 1.0))))
         (sub (beta-incomplete ($bfloat a) ($bfloat b) ($bfloat z2))
              (beta-incomplete ($bfloat a) ($bfloat b) ($bfloat z1)))))

      ;; Check for argument simplifications and transformations

      ((and (integerp a) (plusp a))
       (mul
         (simplify (list '($beta) a b))
         (sub
           (mul
             (power (sub 1 z1) b)
             (let ((index (gensumindex)))
               (dosum
                 (div
                   (mul
                     (simplify (list '($pochhammer) b index))
                     (power z1 index))
                   (simplify (list '(mfactorial) index)))
                index 0 (sub a 1) t)))
           (mul
             (power (sub 1 z2) b)
             (let ((index (gensumindex)))
               (dosum
                 (div
                   (mul 
                     (simplify (list '($pochhammer) b index))
                     (power z2 index))
                   (simplify (list '(mfactorial) index)))
                 index 0 (sub a 1) t))))))

      ((and (integerp b) (plusp b))
       (mul
         (simplify (list '($beta) a b))
         (sub
           (mul
             (power z2 a)
             (let ((index (gensumindex)))
               (dosum
                 (div
                   (mul
                     (simplify (list '($pochhammer) a index))
                     (power (sub 1 z2) index))
                   (simplify (list '(mfactorial) index)))
                index 0 (sub b 1) t)))
           (mul
             (power z1 a)
             (let ((index (gensumindex)))
               (dosum
                 (div
                   (mul 
                     (simplify (list '($pochhammer) a index))
                     (power (sub 1 z1) index))
                   (simplify (list '(mfactorial) index)))
                 index 0 (sub b 1) t))))))
      
      ((and $beta_expand (mplusp a) (integerp (cadr a)) (plusp (cadr a)))
       (let ((n (cadr a))
             (a (simplify (cons '(mplus) (cddr a)))))
         (add
           (mul
             (div
               (simplify (list '($pochhammer) a n))
               (simplify (list '($pochhammer) (add a b) n)))
             ($beta_incomplete_generalized a b z1 z2))
           (mul
             (power (add a b n -1) -1)
             (let ((index (gensumindex)))
               (dosum
                 (mul
                   (div
                     (simplify (list '($pochhammer) 
                                     (add 1 (mul -1 a) (mul -1 n))
                                     index))
                     (simplify (list '($pochhammer)
                                     (add 2 (mul -1 a) (mul -1 b) (mul -1 n))
                                     index)))
                   (sub
                     (mul (power (sub 1 z1) b)
                          (power z1 (add a n (mul -1 index) -1)))
                     (mul (power (sub 1 z2) b)
                          (power z2 (add a n (mul -1 index) -1)))))
                index 0 (sub n 1) t))))))

      ((and $beta_expand (mplusp a) (integerp (cadr a)) (minusp (cadr a)))
       (let ((n (- (cadr a)))
             (a (simplify (cons '(mplus) (cddr a)))))
         (sub
           (mul
             (div
               (simplify (list '($pochhammer) (add 1 (mul -1 a) (mul -1 b)) n))
               (simplify (list '($pochhammer) (sub 1 a) n)))
             ($beta_incomplete_generalized a b z1 z2))
           (mul
             (div
               (simplify 
                 (list '($pochhammer) (add 2 (mul -1 a) (mul -1 b)) (sub n 1)))
               (simplify (list '($pochhammer) (sub 1 a) n)))
             (let ((index (gensumindex)))
               (dosum
                 (mul
                   (div
                     (simplify (list '($pochhammer) (sub 1 a) index))
                     (simplify (list '($pochhammer)
                                     (add 2 (mul -1 a) (mul -1 b))
                                     index)))
                   (sub
                     (mul (power (sub 1 z2) b)
                          (power z2 (add a (mul -1 index) -1)))
                     (mul (power (sub 1 z1) b)
                          (power z1 (add a (mul -1 index) -1)))))
                index 0 (sub n 1) t))))))
      
      (t
       (eqtest (list '(%beta_incomplete_generalized) a b z1 z2) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Regularized Incomplete Beta function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $beta_incomplete_regularized (a b z)
  (simplify (list '(%beta_incomplete_regularized) a b z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop $beta_incomplete_regularized %beta_incomplete_regularized alias)
(defprop $beta_incomplete_regularized %beta_incomplete_regularized verb)

(defprop %beta_incomplete_regularized $beta_incomplete_regularized reversealias)
(defprop %beta_incomplete_regularized $beta_incomplete_regularized noun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %beta_incomplete_regularized
         simp-beta-incomplete-regularized operators)

;;; beta_incomplete_regularized distributes over bags

(defprop %beta_incomplete_regularized (mlist $matrix mequal) distribute_over)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprop %beta_incomplete_regularized
  ((a b z)
   ;; Derivative wrt a
   ((mplus)
      ((mtimes) -1 
         ((%gamma) a)
         (($hypergeometric_regularized)
            ((mlist) a a ((mplus) 1 ((mtimes) -1 b)))
            ((mlist) ((mplus) 1 a) ((mplus) 2 a)) z)
         ((mexpt) ((%gamma) b) -1) 
         ((%gamma) ((mplus) a b))
         ((mexpt) z a))
      ((mtimes) 
         ((%beta_incomplete_regularized) a b z)
         ((mplus) 
            ((mtimes) -1 ((mqapply) (($psi array) 0) a))
            ((mqapply) (($psi array) 0) ((mplus) a b))
            ((%log) z))))
   ;; Derivative wrt b
   ((mplus)
      ((mtimes)
         ((%beta_incomplete_regularized) b a ((mplus) 1 ((mtimes) -1 z)))
         ((mplus) 
            ((mqapply) (($psi array) 0) b)
            ((mtimes) -1 ((mqapply) (($psi array) 0) ((mplus) a b)))
            ((mtimes) -1 ((%log) ((mplus) 1 ((mtimes) -1 z))))))
      ((mtimes) 
         ((mexpt) ((%gamma) a) -1) 
         ((%gamma) b)
         ((%gamma) ((mplus) a b))
         (($hypergeometric_regularized)
            ((mlist) b b ((mplus) 1 ((mtimes) -1 a)))
            ((mlist) ((mplus) 1 b) ((mplus) 1 b))
            ((mplus) 1 ((mtimes) -1 z)))
         ((mexpt) ((mplus) 1 ((mtimes) -1 z)) b)))
   ;; The derivative wrt z
   ((mtimes) 
      ((mexpt) (($beta) a b) -1)
      ((mexpt) ((mplus) 1 ((mtimes) -1 z)) ((mplus) -1 b))
      ((mexpt) z ((mplus) -1 a))))
  grad)

;;; Integral of the Generalized Incomplete Beta function

(defprop %beta_incomplete_regularized
  ((a b z)
   nil 
   nil
   ;; Integral wrt z
   ((mplus)
      ((mtimes) -1 a
         ((%beta_incomplete_regularized) ((mplus) 1 a) b z)
         ((mexpt) ((mplus) a b) -1))
      ((mtimes) ((%beta_incomplete_regularized) a b z) z)))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-beta-incomplete-regularized (expr ignored simpflag)
  (declare (ignore ignored))
  (if (not (= (length expr) 4)) (wna-err '$beta_incomplete_regularized))
  (let ((a (simpcheck (second expr) simpflag))
        (b (simpcheck (third expr)  simpflag))
        (z (simpcheck (fourth expr) simpflag)))
    (cond

      ;; Check for specific values

      ((zerop1 z)
       (let ((sgn ($sign ($realpart a))))
         (cond ((eq sgn '$neg)
                (simp-domain-error 
                  (intl:gettext 
                    "beta_incomplete_regularized: beta_incomplete_regularized(~:M,~:M,~:M) is undefined.") 
                    a b z))
               ((member sgn '($pos $pz)) 
                0)
               (t 
                (eqtest 
                  (list '(%beta_incomplete_regularized) a b z) expr)))))

      ((and (onep1 z) 
            (or (not (mnump a)) 
                (not (mnump b)) 
                (not (mnump z))))
       (let ((sgn ($sign ($realpart b))))
         (cond ((member sgn '($pos $pz)) 
                1)
               (t 
                (eqtest 
                  (list '(%beta_incomplete_regularized) a b z) expr)))))

      ((and (integer-representation-p b) (minusp b))
       ;; Problem: for b a negative integer the Regularized Incomplete 
       ;; Beta function is defined to be zero. BUT: When we calculate
       ;; e.g. beta_incomplete(1.0,-2.0,1/2)/beta(1.0,-2.0) we get the 
       ;; result -3.0, because beta_incomplete and beta are defined for
       ;; for this case. How do we get a consistent behaviour?
       0)

      ((and (integer-representation-p a) (minusp a))
       (cond
         ((and (integer-representation-p b) (<= b (- a)))
          (div ($beta_incomplete a b z)
               (simplify (list '($beta) a b))))
         (t 
          1)))

      ;; Check for numerical evaluation in Float or Bigfloat precision

      ((complex-float-numerical-eval-p a b z)
       (let ((*beta-incomplete-eps* (bigfloat:epsilon ($float 1.0))))
         ($rectform 
           (div (beta-incomplete ($float a) ($float b) ($float z))
                (simplify (list '($beta) ($float a) ($float b)))))))
           
      ((complex-bigfloat-numerical-eval-p a b z)
       (let ((*beta-incomplete-eps*
               (bigfloat:epsilon (bigfloat:bigfloat 1.0))))
         ($rectform 
           (div (beta-incomplete ($bfloat a) ($bfloat b) ($bfloat z))
                (simplify (list '($beta) ($float a) ($float b)))))))

      ;; Check for argument simplifications and transformations

      ((and (integerp b) (plusp b))
       (div ($beta_incomplete a b z)
            (simplify (list '($beta) a b))))

      ((and (integerp a) (plusp a))
       (div ($beta_incomplete a b z)
            (simplify (list '($beta) a b))))

      ((and $beta_expand (mplusp a) (integerp (cadr a)) (plusp (cadr a)))
       (let ((n (cadr a))
             (a (simplify (cons '(mplus) (cddr a)))))
         (sub
           ($beta_incomplete_regularized a b z)
           (mul
             (power (add a b n -1) -1)
             (power (simplify (list '($beta) (add a n) b)) -1)
             (let ((index (gensumindex)))
               (dosum
                 (mul
                   (div
                     (simplify (list '($pochhammer) 
                                     (add 1 (mul -1 a) (mul -1 n))
                                     index))
                     (simplify (list '($pochhammer)
                                     (add 2 (mul -1 a) (mul -1 b) (mul -1 n))
                                     index)))
                   (power (sub 1 z) b)
                   (power z (add a n (mul -1 index) -1)))
                index 0 (sub n 1) t))))))

      ((and $beta_expand (mplusp a) (integerp (cadr a)) (minusp (cadr a)))
       (let ((n (- (cadr a)))
             (a (simplify (cons '(mplus) (cddr a)))))
         (sub
           ($beta_incomplete_regularized a b z)
           (mul
             (power (add a b -1) -1)
             (power (simplify (list '($beta) a b)) -1)
             (let ((index (gensumindex)))
               (dosum
                 (mul
                   (div
                     (simplify (list '($pochhammer) (sub 1 a) index))
                     (simplify (list '($pochhammer)
                                     (add 2 (mul -1 a) (mul -1 b))
                                     index)))
                   (power (sub 1 z) b)
                   (power z (add a (mul -1 index) -1)))
                index 0 (sub n 1) t))))))

      (t
       (eqtest (list '(%beta_incomplete_regularized) a b z) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
