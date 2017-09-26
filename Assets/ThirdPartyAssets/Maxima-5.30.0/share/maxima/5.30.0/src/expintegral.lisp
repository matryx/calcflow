;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exponential Integrals
;;;
;;; This file contains the following Maxima User functions:
;;; 
;;;  $expintegral_e (n,z) - Exponential Integral En(z)
;;;  $expintegral_e1 (z)  - Exponential Integral E1(z)
;;;  $expintegral_ei (z)  - Exponential Integral Ei(z)
;;; 
;;;  $expintegral_li (z)  - Logarithmic Integral Li(z)
;;;
;;;  $expintegral_si (z)  - Exponential Integral Si(z)
;;;  $expintegral_ci (z)  - Exponential Integral Ci(z)
;;;
;;;  $expintegral_shi (z) - Exponential Integral Shi(z)
;;;  $expintegral_chi (z) - Exponential Integral Chi(z)
;;;
;;;  $expint (x)          - Exponential Integral E1(x) (depreciated)
;;;
;;;  Global variables for the Maxima User:
;;;
;;;  $expintrep    - Change the representation of the Exponential Integral to
;;;                  gamma_incomplete, expintegral_e1, expintegral_ei, 
;;;                  expintegral_li, expintegral_trig, expintegral_hyp
;;;
;;;  $expintexpand - Expand the Exponential Integral E[n](z)
;;;                  for half integral values in terms of Erfc or Erf and
;;;                  for positive integers in terms of Ei 
;;;
;;; The following features are implemented:
;;;
;;; 1. Numerical evaluation for complex Flonum and Bigfloat numbers 
;;;    using an expansion in a power series or continued fractions.
;;;    The numerical support is fully implemented for the E[n](z) function.
;;;    All other functions call E[n](z) for numerical evaluation.
;;;
;;; 2. For a negative integer parameter E[n](z) is automatically expanded in
;;;    a finite series in terms of powers and the Exponential function.
;;; 
;;; 3. When $expintexpand is set to TRUE or ERF E[n](z) expands
;;;    a) for n a half integral number in terms of Erfc (TRUE) or Erf (ERF)
;;;    b) for n a positive integer number in terms of Ei
;;;
;;; 3. Simplifications for special values: Ev(0), E[0](z), Li(0), Li(1),...
;;;
;;; 4. Derivatives of the Exponential Integrals
;;;
;;; 5. Change the representation of every Exponential Integral through other
;;;    Exponential Integrals or the Incomplete Gamma function.
;;;
;;; 6. Mirror symmetry for all functions and reflection symmetry for
;;;    the Exponential Inegral Si and Shi are implemented.
;;;
;;; 7. Handling of taylor expansions as argument.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals to help debugging the code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *debug-expintegral* nil
  "When enabled print debug information.")

(defvar *debug-expint-maxit* 0
  "When in debug mode count the maximum of iterations needed by the algorithm.")

(defvar *debug-expint-fracmaxit* 0
  "When in debug mode count the maximum of iterations needed by the algorithm.")

(defvar *debug-expint-bfloatmaxit* 0
  "When in debug mode count the maximum of iterations needed by the algorithm.")

(defvar *debug-expint-fracbfloatmaxit* 0
  "When in debug mode count the maximum of iterations needed by the algorithm.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals for the Maxima Users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar $expintexpand nil
  "When not nil we expand for a half integral parameter of the Exponetial 
   Integral in a series in terms of the Erfc or Erf function and for positive 
   integer in terms of the Ei function.")

(defvar $expintrep nil
  "Change the representation of the Exponential Integral. 
   Values are: gamma_incomplete, expintegral_e1, expintegral_ei, 
   expintegral_li, expintegral_trig, expintegral_hyp.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global to this file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *expintflag* '(%expintegral_e1   %expintegral_ei  %expintegral_li
                       $expintegral_trig $expintegral_hyp %gamma_incomplete)
  "Allowed flags to transform the Exponential Integral.")

(defun simp-domain-error (&rest args)
  (if errorsw
      (throw 'errorsw t)
      (apply #'merror args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 1: The implementation of the Exponential Integral En
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $expintegral_e (v z)
  (simplify (list '(%expintegral_e) v z)))

;;; Set properties to give full support to the parser and display

(defprop $expintegral_e %expintegral_e alias)
(defprop $expintegral_e %expintegral_e verb)

(defprop %expintegral_e $expintegral_e reversealias)
(defprop %expintegral_e $expintegral_e noun)

;;; Exponential Integral E is a simplifying function

(defprop %expintegral_e simp-expintegral-e operators)

;;; Exponential Integral E distributes over bags

(defprop %expintegral_e (mlist $matrix mequal) distribute_over)

;;; Exponential Integral E has mirror symmetry, 
;;; but not on the real negative axis.

(defprop %expintegral_e conjugate-expintegral-e conjugate-function)

(defun conjugate-expintegral-e (args)
  (let ((n (first args))
        (z (second args)))
    (cond ((off-negative-real-axisp z)
           ;; Definitly not on the negative real axis for z. Mirror symmetry.
           (take '(%expintegral_e)
                 (take '($conjugate) n) 
                 (take '($conjugate) z)))
          (t
            ;; On the negative real axis or no information. Unsimplified.
            (list '($conjugate simp)
                  (take '(%expintegral_e) n z))))))

;;; Differentiation of Exponential Integral E

(defprop %expintegral_e 
  ((n z)
    ;; The derivative wrt the parameter n is expressed in terms of the
    ;; Regularized Hypergeometric function 2F2 (see functions.wolfram.com)
    ((mplus)
       ((mtimes) -1
          (($hypergeometric_regularized)
             ((mlist) 
               ((mplus) 1 ((mtimes) -1 n))
               ((mplus) 1 ((mtimes) -1 n)))
             ((mlist) 
               ((mplus) 2 ((mtimes) -1 n))
               ((mplus) 2 ((mtimes) -1 n)))
             ((mtimes) -1 z))
          ((mexpt) 
             ((%gamma) ((mplus) 1 ((mtimes) -1 n))) 2))
       ((mtimes) 
          ((%gamma) ((mplus) 1 ((mtimes) -1 n)))
          ((mexpt) z ((mplus) -1 n))
          ((mplus)
             ((mtimes) -1
                ((mqapply) 
                   (($psi array) 0)
                   ((mplus) 1 ((mtimes) -1 n))))
             ((%log) z))))

   ;; The derivative wrt the argument of the function
   ((mtimes) -1 ((%expintegral_e) ((mplus) -1 n) z)))
  grad)

;;; Integral of Exponential Integral E

(defprop %expintegral_e
  ((n z)
   nil
   ((mtimes) -1 ((%expintegral_e) ((mplus) 1 n) z)))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We support a simplim%function. The function is looked up in simplimit and 
;;; handles specific values of the function.

(defprop %expintegral_e simplim%expintegral_e simplim%function)

(defun simplim%expintegral_e (expr var val)
  ;; Look for the limit of the arguments.
  (let ((a (limit (cadr expr) var val 'think))
        (z (limit (caddr expr) var val 'think)))
  (cond ((and (onep1 a)
              (or (eq z '$zeroa)
                  (eq z '$zerob)
                  (zerop1 z)))
         ;; Special case order a=1
         '$inf)
         
        ((member ($sign (add ($realpart a) -1)) '($neg $nz $zero))
         ; realpart of order < 1
         (cond ((eq z '$zeroa)
                ;; from above, always inf
                '$inf)
               ((eq z '$zerob)
                ;; this can be further improved to give a directed infinity
                '$infinity)
               ((zerop1 z)
                ;; no direction, return infinity
                '$infinity)
               (t
                ($expintegral_e a z))))
        (t
         ;; All other cases are handled by the simplifier of the function.
         ($expintegral_e a z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-expintegral-e (expr ignored z)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((order (simpcheck (cadr expr) z))
        (arg   (simpcheck (caddr expr) z))
        (ratorder))
    (cond
      ;; Check for special values
      ((or (eq arg '$inf)
           (alike1 arg '((mtimes) -1 $minf)))
       ;; arg is inf or -minf, return zero
       0)
      
      ((zerop1 arg)
       (let ((sgn ($sign (add ($realpart order) -1))))
         (cond 
           ((eq sgn '$pos)
            ;; we handle the special case E[v](0) = 1/(v-1), for realpart(v)>1
            (inv (add order -1)))
           ((member sgn '($neg $nz $zero))
            (simp-domain-error 
              (intl:gettext 
                "expintegral_e: expintegral_e(~:M,~:M) is undefined.")
                order arg))
           (t (eqtest (list '(%expintegral_e) order arg) expr)))))
      
      ((or (and (symbolp order) (member order infinities))
           (and (symbolp arg) (member arg infinities)))
       ;; order or arg is one of the infinities, we return a noun form,
       ;; but we have already handled the special value inf for arg.
       (eqtest (list '(%expintegral_e) order arg) expr))
      
      ((and (numberp order) (integerp order))
       ;; The parameter of the Exponential integral is an integer. For this 
       ;; case we can do further simplifications or numerical evaluation.
       (cond
         ((= order 0)
          ;; Special case E[0](z) = %e^(-z)/z, z<>0
          ;; The case z=0 is already handled.
          (div (power '$%e (mul -1 arg)) arg))
         
         ((= order -1)
          ;; Special case E[-1](0) = ((z+1)*%e^(-z))/z^2, z<>0
          ;; The case z=0 is already handled.
          (div (mul (power '$%e (mul -1 arg)) (add arg 1)) (mul arg arg)))
         
         ((< order -1)
          ;; We expand in a series, z<>0
          (mul
            (factorial (- order))
            (power arg (+ order -1))
            (power '$%e (mul -1 arg))
            (let ((index (gensumindex)))
              (dosum 
                (div (power arg index)
                     (take '(mfactorial) index))
                index 0 (- order) t))))
         
         ((and (> order 0) 
               (complex-float-numerical-eval-p arg))
          ;; Numerical evaluation for double float real or complex arg
          ;; order is an integer > 0 and arg <> 0 for order < 2
          (let ((carg (complex ($float ($realpart arg))
                               ($float ($imagpart arg)))))
            (complexify (expintegral-e order carg))))
         
         ((and (> order 0) 
               (complex-bigfloat-numerical-eval-p arg))
          ;; Numerical evaluation for Bigfloat real or complex arg.
          (let* (($ratprint nil)
                 (carg (add ($bfloat ($realpart arg)) 
                            (mul '$%i ($bfloat ($imagpart arg)))))
                 (result (bfloat-expintegral-e order carg)))
            (add ($realpart result) (mul '$%i ($imagpart result)))))
         
         ((and $expintexpand (> order 0))
          ;; We only expand in terms of the Exponential Integral Ei
          ;; if the expand flag is set.
          (sub (mul -1
                    (power (mul -1 arg) (- order 1))
                    (inv (factorial (- order 1)))
                    (add (take '(%expintegral_ei) (mul -1 arg))
                         (mul (inv 2)
                              (sub (take '(%log) (mul -1 (inv arg)))
                                   (take '(%log) (mul -1 arg))))
                         (take '(%log) arg)))
               (mul (power '$%e (mul -1 arg))
                    (let ((index (gensumindex)))
                      (dosum 
                        (div (power arg (add index -1))
                             (take '($pochhammer) (- 1 order) index))
                        index 1 (- order 1) t)))))
         
         ((eq $expintrep '%gamma_incomplete)
          ;; We transform to the Incomplete Gamma function.
          (mul (power arg (- order 1))
               (take '(%gamma_incomplete) (- 1 order) arg)))
         
         (t
          (eqtest (list '(%expintegral_e) order arg) expr))))
      
      ((complex-float-numerical-eval-p order arg)
       (cond
         ((and (setq z (integer-representation-p order))
               (plusp z))
          ;; We have a pure real positive order and the realpart is a float 
          ;; representation of an integer value.
          ;; We call the routine for an integer order.
          (let ((carg (complex ($float ($realpart arg)) 
                               ($float ($imagpart arg)))))
            (complexify (expintegral-e z carg))))
         (t
          ;; The general case, order and arg are complex or real.
          (let ((corder (complex ($float ($realpart order)) 
                                 ($float ($imagpart order))))
                (carg (complex ($float ($realpart arg)) 
                               ($float ($imagpart arg)))))
            (complexify (frac-expintegral-e corder carg))))))
      
      ((complex-bigfloat-numerical-eval-p order arg)
       (cond
         ((and (setq z (integer-representation-p order))
               (plusp z))
          ;; We have a real positive order and the realpart is a Float or 
          ;; Bigfloat representation of an integer value.
          ;; We call the routine for an integer order.
          (let* (($ratprint nil)
                 (carg (add ($bfloat ($realpart arg)) 
                            (mul '$%i ($bfloat ($imagpart arg)))))
                 (result (bfloat-expintegral-e z carg)))
            (add ($realpart result) 
                 (mul '$%i ($imagpart result)))))
         (t
          ;; the general case, order and arg are bigfloat or complex bigfloat
          (let* (($ratprint nil)
                 (corder (add ($bfloat ($realpart order))
                              (mul '$%i ($bfloat ($imagpart order)))))
                 (carg (add ($bfloat ($realpart arg))
                            (mul '$%i ($bfloat ($imagpart arg)))))
                 (result (frac-bfloat-expintegral-e corder carg)))
            (add ($realpart result)
                 (mul '$%i ($imagpart result)))))))
      
      ((and $expintexpand
            (setq ratorder (max-numeric-ratio-p order 2)))
       ;; We have a half integral order and $expintexpand is not NIL. 
       ;; We expand in a series in terms of the Erfc or Erf function.
       (let ((func (cond ((eq $expintexpand '%erf)
                          (sub 1 ($erf (power arg '((rat simp) 1 2)))))
                         (t
                          ($erfc (power arg '((rat simp) 1 2)))))))
         (cond
           ((= ratorder 1/2)
            (mul (power '$%pi '((rat simp) 1 2))
                 (power arg '((rat simp) -1 2))
                 func))
           ((= ratorder -1/2)
            (add
              (mul
                (power '$%pi '((rat simp) 1 2))
                (inv (mul 2 (power arg '((rat simp) 3 2))))
                func)
              (div (power '$%e (mul -1 arg)) arg)))
           (t
            (let ((n (- ratorder 1/2)))
              (mul
                (power arg (sub n '((rat simp) 1 2)))
                (add
                  (mul func (take '(%gamma) (sub '((rat simp) 1 2) n)))
                  (mul
                    (power '$%e (mul -1 arg))
                    (let ((index (gensumindex)))
                      (dosum
                        (div
                          (power arg (add index '((rat simp) 1 2)))
                          (take '($pochhammer)
                                (sub '((rat simp) 1 2) n)
                                (add index n 1)))
                        index 0 (mul -1 (add n 1)) t)))
                  (mul -1
                    (power '$%e (mul -1 arg))
                    (let ((index (gensumindex)))
                      (dosum
                        (div
                          (power arg (add index '((rat simp) 1 2)))
                          (take '($pochhammer)
                                (sub '((rat simp) 1 2) n)
                                (add index n 1)))
                        index (- n) -1 t))))))))))
      
      ((eq $expintrep '%gamma_incomplete)
       ;; We transform to the Incomplete Gamma function.
       (mul (power arg (sub order 1))
            (take '(%gamma_incomplete) (sub 1 order) arg)))
      
      (t 
       (eqtest (list '(%expintegral_e) order arg) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numerical evaluation of the Exponential Integral En(z)
;;;
;;; The following numerical routines are implemented:
;;;
;;; expintegral-e             - n positive integer, z real or complex
;;; frac-expintegral-e        - n,z real or complex; n not a positive integer
;;; bfloat-expintegral-e      - n positive integer, 
;;;                             z Bigfloat or Complex Bigfloat
;;; frac-bfloat-expintegral-e - n Bigfloat, z Bigfloat or Complex Bigfloat
;;;
;;; The algorithm are implemented for full support of Flonum and Bigfloat real 
;;; or Complex parameter and argument of the Exponential Integral.
;;; Because we have no support for Complex Bigfloat arguments of the Gamma
;;; function the evaluation for a Complex Bigfloat parameter don't give
;;; the desiered accuracy.
;;;
;;; The flonum versions return a CL complex number. The Bigfloat versions
;;; a Maxima Complex Bigfloat number. It is assumed that the calling routine 
;;; check the values. We don't handle any special case. This has to be done by 
;;; the calling routine.
;;;
;;; The evaluation uses an expansion in continued fractions for arguments with
;;; realpart(z) > 0 and abs(z)> 1.0 (A&S 5.1.22). This expansion works for
;;; every Real or Complex numbers including Bigfloat numbers for the parameter n
;;; and the argument z:
;;;
;;;                       1   n   1  n+1  2
;;;   En(z) = e^(-z) * ( --- --- --- --- --- ... )
;;;                      z+  1+  z+  1+  z+
;;;
;;; The continued fraction is evaluated by the modified Lentz's method
;;; for the more rapidly converging even form.
;;;
;;; For the parameter n an positive integer we do an expansion in a power series
;;; (A&S 5.1.12):
;;;                                           inf
;;;                                           ===
;;;            (-z)^(n-1)                     \     (-z)^m
;;;   En(z) =  --------- * (-log(z)+psi(n)) *  >  ---------- ; n an integer
;;;               (n-1)!                      /   (m-n+1)*m!
;;;                                           ===
;;;                                           m=0 (m <> n-1)
;;;
;;; For an parameter n not an integer we expand in the following series
;;; (functions.wolfram.com ):
;;;                                    inf
;;;                                    ===
;;;                                    \    (-1)^m * z^m
;;;   Ev(z) = gamma(1-v) * z^(v-1) *    >   -------------  ; n not an integer
;;;                                    /     (m-v+1)*m!
;;;                                    ===
;;;                                    m=0
;;;
;;; The evaluation stops if an accuracy better than *expint-eps* is achived.
;;; If the expansion don't converge within *expint-maxit* steps a Maxima
;;; Error is thrown.
;;;
;;; The algorithm is based on technics desribed in Numerical Recipes, 2nd Ed. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Constants to terminate the numerical evaluation
;;;
;;; The accuracy *expint-eps* is fixed to 1.0e-15 for double float calculations.
;;; The variable is declared global, so we can later give the Maxima User access
;;; to the variable. The routine for Bigfloat numerical evaluation change this
;;; value to the desired precision of the global $fpprec.
;;; The maximum number of iterations is arbitrary set to 1000. For Bigfloat
;;; evaluation this number is for very Big numbers too small.
;;;
;;; The maximum iterations counted for the test file rtest-expintegral.mac are
;;; 101 for Complex Flonum and 1672 for Complex Bigfloat evaluation.

(defvar *expint-eps*   1.0e-15)
(defvar *expint-maxit* 1000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expintegral-e (n z)
  (declare (type integer n))
  (let ((*expint-eps*   *expint-eps*)
        (*expint-maxit* *expint-maxit*)
	;; Add (complex) 0 to get rid of any signed zeroes, and make z
	;; be a complex number.
	(z (+ (coerce 0 '(complex flonum)) z)))
    (declare (type (complex flonum) z))

    (when *debug-expintegral*
      (format t "~&EXPINTEGRAL-E called with:~%")
      (format t "~&   : n = ~A~%" n)
      (format t "~&   : z = ~A~%" z))

    (cond
      ((or (and (> (abs z) 2.0) (< (abs (phase z)) (* pi 0.9)))
	   ;; (abs z)>2.0 is necessary since there is a point
	   ;; -1.700598-0.612828*%i which 1<(abs z)<2, phase z < 0.9pi and
	   ;; still c-f expansion does not converge.
	   (and (>= (realpart z) 0) (> (abs z) 1.0)))
       ;; We expand in continued fractions.
       (when *debug-expintegral*
         (format t "~&We expand in continued fractions.~%"))
       (let* ((b  (+ z n))
              (c  (/ 1.0 (* *expint-eps* *expint-eps*)))
              (d  (/ 1.0 b))
              (n1 (- n 1))
              (h  d)
              (e  0.0))
         (do* ((i 1 (+ i 1))
               (a (* -1 n) (* (- i) (+ n1 i))))
              ((> i *expint-maxit*)
               (merror 
                 (intl:gettext "expintegral_e: continued fractions failed.")))

           (setq b (+ b 2.0))
           (setq d (/ 1.0 (+ (* a d) b)))
           (setq c (+ b (/ a c)))
           (setq e (* c d))
           (setq h (* h e))
            
           (when (< (abs (- e 1.0)) *expint-eps*)
             (when *debug-expintegral*
               (setq *debug-expint-maxit* (max *debug-expint-maxit* i)))
             (return (* h (exp (- z))))))))
      (t
       ;; We expand in a power series.
       (when *debug-expintegral*
         (format t "~&We expand in a power series.~%"))
       (let* ((n1 (- n 1))
              (euler (mget '$%gamma '$numer))
              (r (if (= n1 0) (- (- euler) (log z)) (/ 1.0 n1)))
              (f 1.0)
              (e 0.0))
         (do ((i 1 (+ i 1)))
             ((> i *expint-maxit*)
              (merror (intl:gettext "expintegral_e: series failed.")))
           (setq f (* -1 f (/ z i)))
           (cond 
             ((= i n1)
              (let ((psi (- euler)))              
                (dotimes (ii n1)
                  (setq psi (+ psi (/ 1.0 (+ ii 1)))))
                (setq e (* f (- psi (log z))))))
             (t 
              (setq e (/ (- f) (- i n1)))))
           (setq r (+ r e))
           (when (< (abs e) (* (abs r) *expint-eps*))
             (when *debug-expintegral*
               (setq *debug-expint-maxit* (max *debug-expint-maxit* i)))
             (return r))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numerical evaluation for a real or complex parameter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun frac-expintegral-e (n z)
  (declare (type (complex flonum) n)
           (type (complex flonum) z))
                    
  (let ((*expint-eps*   *expint-eps*)
        (*expint-maxit* *expint-maxit*))

    (when *debug-expintegral*
      (format t "~&FRAC-EXPINTEGRAL-E called with:~%")
      (format t "~&   : n = ~A~%" n)
      (format t "~&   : z = ~A~%" z))

    (cond
      ((and (> (realpart z) 0) (> (abs z) 1.0))
       ;; We expand in continued fractions.
       (when *debug-expintegral*
         (format t "~&We expand in continued fractions.~%"))
       (let* ((b  (+ z n))
              (c  (/ 1.0 (* *expint-eps* *expint-eps*)))
              (d  (/ 1.0 b))
              (n1 (- n 1))
              (h  d)
              (e  0.0))
         (do* ((i 1 (+ i 1))
               (a (* -1 n) (* (- i) (+ n1 i))))
              ((> i *expint-maxit*)
               (merror 
                 (intl:gettext "expintegral_e: continued fractions failed.")))

           (setq b (+ b 2.0))
           (setq d (/ 1.0 (+ (* a d) b)))
           (setq c (+ b (/ a c)))
           (setq e (* c d))
           (setq h (* h e))
            
           (when (< (abs (- e 1.0)) *expint-eps*)
             (when *debug-expintegral*
               (setq *debug-expint-fracmaxit* (max *debug-expint-fracmaxit* i)))
             (return (* h (exp (- z))))))))

      ((and (= (imagpart n) 0) 
            (> (realpart n) 0)
            (= (nth-value 1 (truncate (realpart n))) 0))
       ;; We have a positive integer n or an float representation of an 
       ;; integer. We call expintegral-e which does this calculation.
       (when *debug-expintegral*
         (format t "~&We call expintegral-e.~%"))
       (expintegral-e (truncate (realpart n)) z))

      (t
       ;; At this point the parameter n is a real (not an float representation
       ;; of an integer) or complex. We expand in a power series.
       (when *debug-expintegral*
         (format t "~&We expand in a power series.~%"))
       (let* ((n1 (- n 1))
              ;; It would be possible to call the numerical implementation 
              ;; gamm-lanczos directly. But then the code would depend on the
              ;; details of the implementation.
              (gm (let ((tmp (take '(%gamma) (complexify (- 1 n)))))
                    (complex ($realpart tmp) ($imagpart tmp))))
              (r (- (* (expt z n1) gm) (/ 1.0 (- 1 n))))
              (f 1.0)
              (e 0.0))
         (do ((i 1 (+ i 1)))
             ((> i *expint-maxit*)
              (merror (intl:gettext "expintegral_e: series failed.")))
           (setq f (* -1 f (/ z (float i))))
           (setq e (/ (- f) (- (float i) n1)))
           (setq r (+ r e))
           (when (< (abs e) (* (abs r) *expint-eps*))
             (when *debug-expintegral*
               (setq *debug-expint-fracmaxit* (max *debug-expint-fracmaxit* i)))
             (return r))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions for Bigfloat numerical evaluation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cmul (x y) ($rectform (mul x y)))

(defun cdiv (x y) ($rectform (div x y)))

(defun cpower (x y) ($rectform (power x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We have not changed the above algorithm, but generalized it to handle
;;; complex and real Bigfloat numbers. By carefully examination of the
;;; algorithm some of the additional calls to $rectform can be eliminated.
;;; But the algorithm works and so we leave the extra calls for later work 
;;; in the code. 
;;; The accuracy of the result is determined by *expint-eps*. The value is
;;; chosen to correspond to the value of $fpprec. We don't give any extra
;;; digits to fpprec, so we loose 1 to 2 digits of precision.
;;; One problem is to chose a sufficient big *expint-maxit*. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bfloat-expintegral-e (n z)
  (let ((*expint-eps* (power ($bfloat 10.0) (- $fpprec)))
        (*expint-maxit* 5000) ; arbitrarily chosen, we need a better choice
        (bigfloattwo (add bigfloatone bigfloatone))
        (bigfloat%e ($bfloat '$%e))
        (bigfloat%gamma ($bfloat '$%gamma))
	(flz (complex ($float ($realpart z)) ($float ($imagpart z)))))

    (when *debug-expintegral*
      (format t "~&BFLOAT-EXPINTEGRAL-E called with:~%")
      (format t "~&   : n = ~A~%" n)
      (format t "~&   : z = ~A~%" flz))

    (cond
      ((or (and (> (abs flz) 2) (< (abs (phase flz)) (* pi 0.9)))
	   ;; The same condition as you see in expintegral-e()
	   (and (>= (realpart flz) 0) (> (abs flz) 1.0)))
       ;; We expand in continued fractions.
       (when *debug-expintegral*
         (format t "~&We expand in continued fractions.~%"))
       (let* ((b  (add z n))
              (c  (div bigfloatone (mul *expint-eps* *expint-eps*)))
              (d  (cdiv bigfloatone b))
              (n1 (- n 1))
              (h  d)
              (e  0.0))
         (do* ((i 1 (+ i 1))
               (a (* -1 n) (* (- i) (+ n1 i))))
              ((> i *expint-maxit*)
               (merror 
                 (intl:gettext "expintegral_e: continued fractions failed.")))

           (setq b (add b bigfloattwo))
           (setq d (cdiv bigfloatone (add (mul a d) b)))
           (setq c (add b (cdiv a c)))
           (setq e (cmul c d))
           (setq h (cmul h e))
            
           (when (eq ($sign (sub (cabs (sub e bigfloatone)) *expint-eps*)) 
                     '$neg)
             (when *debug-expintegral*
               (setq *debug-expint-bfloatmaxit*
                     (max *debug-expint-bfloatmaxit* i)))
             (return (cmul h (cpower bigfloat%e (mul -1 z))))))))
      (t
       ;; We expand in a power series.
       (when *debug-expintegral*
         (format t "~&We expand in a power series.~%"))
       (let* ((n1 (- n 1))
              (meuler (mul -1 bigfloat%gamma))
              (r (if (= n1 0) (sub meuler ($log z)) (div bigfloatone n1)))
              (f bigfloatone)
              (e bigfloatzero))
         (do* ((i 1 (+ i 1)))
              ((> i *expint-maxit*)
               (merror (intl:gettext "expintegral_e: series failed.")))
           (setq f (mul -1 (cmul f (cdiv z i))))
           (cond
             ((= i n1)
              (let ((psi meuler))                
                (dotimes (ii n1)
                  (setq psi (add psi (cdiv bigfloatone (+ ii 1)))))
                (setq e (cmul f (sub psi ($log z))))))
             (t 
              (setq e (cdiv (mul -1 f) (- i n1)))))
           (setq r (add r e))
           (when (eq ($sign (sub (cabs e) (cmul (cabs r) *expint-eps*)))
                     '$neg)
             (when *debug-expintegral*
               (setq *debug-expint-bfloatmaxit*
                     (max *debug-expint-bfloatmaxit* i)))
             (return r))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numerical Bigfloat evaluation for a real (Bigfloat) parameter.
;;; The algorithm would work for a Complex Bigfloat paramter too. But we
;;; need the values of Gamma for Complex Bigfloats. This is at this time (2008)
;;; not implemented in Maxima.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun frac-bfloat-expintegral-e (n z)
  (let ((*expint-eps* (power ($bfloat 10.0) (- $fpprec)))
        (*expint-maxit* 5000) ; arbitrarily chosen, we need a better choice
        (bigfloattwo (add bigfloatone bigfloatone))
        (bigfloat%e ($bfloat '$%e))
        (bigfloat%gamma ($bfloat '$%gamma)))

    (when *debug-expintegral*
      (format t "~&FRAC-BFLOAT-EXPINTEGRAL-E called with:~%")
      (format t "~&   : n = ~A~%" n)
      (format t "~&   : z = ~A~%" z))

    (cond
      ((and (or (eq ($sign ($realpart z)) '$pos)
		(eq ($sign ($realpart z)) '$zero))
            (eq ($sign (sub (cabs z) bigfloatone)) '$pos))
       ;; We expand in continued fractions.
       (when *debug-expintegral*
             (format t "We expand in continued fractions.~%"))
       (let* ((b  (add z n))
              (c  (div bigfloatone (mul *expint-eps* *expint-eps*)))
              (d  (cdiv bigfloatone b))
              (n1 (sub n 1))
              (h  d)
              (e  0.0))
         (do* ((i 1 (+ i 1))
               (a (mul -1 n) (cmul (- i) (add n1 i))))
              ((> i *expint-maxit*)
               (merror 
                 (intl:gettext "expintegral_e: continued fractions failed.")))

           (setq b (add b bigfloattwo))
           (setq d (cdiv bigfloatone (add (mul a d) b)))
           (setq c (add b (cdiv a c)))
           (setq e (cmul c d))
           (setq h (cmul h e))
            
           (when (eq ($sign (sub (cabs (sub e bigfloatone)) *expint-eps*))
                  '$neg)
             (when *debug-expintegral*
               (setq *debug-expint-fracbfloatmaxit*
                     (max *debug-expint-fracbfloatmaxit* i)))   
             (return (cmul h (cpower bigfloat%e (mul -1 z))))))))

       ((or (and (numberp n)
                   (= ($imagpart n) 0)
                   (> ($realpart n) 0)
                   (= (nth-value 1 (truncate ($realpart n))) 0))
              (and ($bfloatp n)
                   (eq ($sign n) '$pos)
                   (equal (sub (mul 2 ($fix n)) (mul 2 n))
                          bigfloatzero)))
       ;; We have a Float or Bigfloat representation of positive integer.
       ;; We call bfloat-expintegral-e.
       (when *debug-expintegral*
         (format t "frac-Bigfloat with integer ~A~%" n))
       (bfloat-expintegral-e ($fix ($realpart n)) z))

      (t
       ;; At this point the parameter n is a real (not a float representation
       ;; of an integer) or complex. We expand in a power series.
       (when *debug-expintegral*
             (format t "We expand in a power series.~%"))       
       (let* ((n1 (sub n bigfloatone))
              (n2 (sub bigfloatone n))
              (gm (take '(%gamma) n2))
              (r (sub (cmul (cpower z n1) gm) (cdiv bigfloatone n2)))
              (f bigfloatone)
              (e bigfloatzero))
         (do ((i 1 (+ i 1)))
             ((> i *expint-maxit*)
              (merror (intl:gettext "expintegral_e: series failed.")))
           (setq f (cmul (mul -1 bigfloatone) (cmul f (cdiv z i))))
           (setq e (cdiv (mul -1 f) (sub i n1)))
           (setq r (add r e))
           (when (eq ($sign (sub (cabs e) (cmul (cabs r) *expint-eps*)))
                     '$neg)
             (when *debug-expintegral*
               (setq *debug-expint-fracbfloatmaxit*
                     (max *debug-expint-fracbfloatmaxit* i)))
             (return r))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 2: The implementation of the Exponential Integral E1
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $expintegral_e1 (z)
  (simplify (list '(%expintegral_e1) z)))

;;; Set properties to give full support to the parser and display

(defprop $expintegral_e1 %expintegral_e1 alias)
(defprop $expintegral_e1 %expintegral_e1 verb)

(defprop %expintegral_e1 $expintegral_e1 reversealias)
(defprop %expintegral_e1 $expintegral_e1 noun)

;;; Exponential Integral E1 is a simplifying function

(defprop %expintegral_e1 simp-expintegral_e1 operators)

;;; Exponential Integral E1 distributes over bags

(defprop %expintegral_e1 (mlist $matrix mequal) distribute_over)

;;; Exponential Integral E1 has mirror symmetry, 
;;; but not on the real negative axis.

(defprop %expintegral_e1 conjugate-expintegral-e1 conjugate-function)

(defun conjugate-expintegral-e1 (args)
  (let ((z (first args)))
    (cond ((off-negative-real-axisp z)
           ;; Definitly not on the negative real axis for z. Mirror symmetry.
           (take '(%expintegral_e1) (take '($conjugate) z)))
          (t
           ;; On the negative real axis or no information. Unsimplified.
           (list '($conjugate simp) (take '(%expintegral_e1) z))))))

;;; Differentiation of Exponential Integral E1

(defprop %expintegral_e1
  ((x)
   ((mtimes) -1
    ((mexpt) x -1)
    ((mexpt) $%e ((mtimes) -1 x))))
  grad)

;;; Integral of Exponential Integral E1

(defprop %expintegral_e1
  ((z)
   ((mtimes) -1 ((%expintegral_e) 2 z)))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We support a simplim%function. The function is looked up in simplimit and 
;;; handles specific values of the function.

(defprop %expintegral_e1 simplim%expintegral_e1 simplim%function)

(defun simplim%expintegral_e1 (expr var val)
  ;; Look for the limit of the argument.
  (let ((z (limit (cadr expr) var val 'think)))
  (cond
    ;; Handle an argument 0 at this place
    ((or (zerop1 z)
         (eq z '$zeroa)
         (eq z '$zerob))
     ;; limit is inf from both sides
     '$inf)
    (t
     ;; All other cases are handled by the simplifier of the function.
     (take '(%expintegral_e1) z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-expintegral_e1 (expr ignored z)
  (declare (ignore ignored))
  (oneargcheck expr)
  (let ((arg (simpcheck (cadr expr) z)))
    (cond
      ;; Check for special values
      ((or (eq arg '$inf)
           (alike1 arg '((mtimes) -1 $minf)))
       0)
      ((zerop1 arg)
       (simp-domain-error
        (intl:gettext "expintegral_e1: expintegral_e1(~:M) is undefined.") arg))

      ;; Check for numerical evaluation
      ((complex-float-numerical-eval-p arg)
       ;; For E1 we call En(z) with n=1 directly.
       (let ((carg (complex ($float ($realpart arg)) ($float ($imagpart arg)))))
         (complexify (expintegral-e 1 carg))))

      ((complex-bigfloat-numerical-eval-p arg)
       ;; For E1 we call En(z) with n=1 directly.
       (let* (($ratprint nil)
              (carg (add ($bfloat ($realpart arg))
                         (mul '$%i ($bfloat ($imagpart arg)))))
              (result (bfloat-expintegral-e 1 carg)))
         (add ($realpart result)
              (mul '$%i ($imagpart result)))))

      ;; Check argument simplifications and transformations
      ((taylorize (mop expr) (second expr)))

      ((and $expintrep
            (member $expintrep *expintflag* :test #'eq)
            (not (eq $expintrep '%expintegral_e1)))
       (case $expintrep
         (%gamma_incomplete
          (take '(%gamma_incomplete) 0 arg))
         (%expintegral_ei
          (add (mul -1 (take '(%expintegral_ei) (mul -1 arg)))
               (mul (inv 2)
                    (sub (take '(%log) (mul -1 arg))
                         (take '(%log) (mul -1 (inv arg)))))
              (mul -1 (take '(%log) arg))))
         (%expintegral_li
          (add (mul -1 (take '(%expintegral_li) (power '$%e (mul -1 arg))))
               (mul -1 (take '(%log) arg))
               (mul (inv 2)
                    (sub (take '(%log) (mul -1 arg))
                         (take '(%log) (mul -1 (inv arg)))))))
         ($expintegral_trig
          (add (mul -1 '$%i (take '(%expintegral_si) (mul '$%i arg)))
               (mul -1 (take '(%expintegral_ci) (mul '$%i arg)))
               (take '(%log) (mul '$%i arg))
               (mul -1 (take '(%log) arg))))
         ($expintegral_hyp
          (sub (take '(%expintegral_shi) arg)
               (take '(%expintegral_chi) arg)))
         (t 
          (eqtest (list '(%expintegral_e1) arg) expr))))
      
      (t
       (eqtest (list '(%expintegral_e1) arg) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 3: The implementation of the Exponential Integral Ei
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $expintegral_ei (z)
  (simplify (list '(%expintegral_ei) z)))

;;; Set properties to give full support to the parser and display

(defprop $expintegral_ei %expintegral_ei alias)
(defprop $expintegral_ei %expintegral_ei verb)

(defprop %expintegral_ei $expintegral_ei reversealias)
(defprop %expintegral_ei $expintegral_ei noun)

;;; Exponential Integral Ei is a simplifying function

(defprop %expintegral_ei simp-expintegral-ei operators)

;;; Exponential Integral Ei distributes over bags

(defprop %expintegral_ei (mlist $matrix mequal) distribute_over)

;;; Exponential Integral Ei has mirror symmetry

(defprop %expintegral_ei t commutes-with-conjugate)

;;; Differentiation of Exponential Integral Ei

(defprop %expintegral_ei
  ((x)
   ((mtimes) ((mexpt) x -1) ((mexpt) $%e x)))
  grad)

;;; Integral of Exponential Ei

(defprop %expintegral_ei
  ((x)
   ((mplus) 
      ((mtimes) -1 ((mexpt) $%e x))
      ((mtimes) x ((%expintegral_ei) x))))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We support a simplim%function. The function is looked up in simplimit and 
;;; handles specific values of the function.

(defprop %expintegral_ei simplim%expintegral_ei simplim%function)

(defun simplim%expintegral_ei (expr var val)
  ;; Look for the limit of the arguments.
  (let ((z (limit (cadr expr) var val 'think)))
  (cond
    ;; Handle an argument 0 at this place
    ((or (zerop1 z)
         (eq z '$zeroa)
         (eq z '$zerob))
     '$minf)
    (t
     ;; All other cases are handled by the simplifier of the function.
     (take '(%expintegral_ei) z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-expintegral-ei (expr ignored z)
  (declare (ignore ignored))
  (oneargcheck expr)
  (let ((arg (simpcheck (cadr expr) z)))
    (cond
      ;; Check special values
      ((zerop1 arg) 
       (simp-domain-error 
         (intl:gettext "expintegral_ei: expintegral_ei(~:M) is undefined.") 
         arg))
      ((eq arg '$inf) '$inf)
      ((or (eq arg '$minf) (alike1 arg '((mtimes) -1 $inf))) 0)
      ((alike1 arg '((mtimes) $%i $inf)) (mul '$%i '$%pi))
      ((alike1 arg '((mtimes) -1 $%i $inf)) (mul -1 '$%i '$%pi))

      ;; Check numerical evaluation
      ((complex-float-numerical-eval-p arg)
       (let ((carg (complex ($float ($realpart arg)) ($float ($imagpart arg)))))
         (complexify (expintegral-ei carg))))

      ((complex-bigfloat-numerical-eval-p arg)
       (let* (($ratprint nil)
              (carg (add ($bfloat ($realpart arg))
                         (mul '$%i ($bfloat ($imagpart arg)))))
              (result (bfloat-expintegral-ei carg)))
         (add ($realpart result)
              (mul '$%i ($imagpart result)))))

      ;; Check argument simplifications and transformations
      ((taylorize (mop expr) (second expr)))

      ((and $expintrep
            (member $expintrep *expintflag*)
            (not (eq $expintrep '%expintegral_ei)))
       (case $expintrep
         (%gamma_incomplete
           (add (mul -1 (take '(%gamma_incomplete) 0 (mul -1 arg)))
                (mul (inv 2)
                     (sub (take '(%log) arg)
                          (take '(%log) (inv arg))))
                (mul -1 (take '(%log) (mul -1 arg)))))
         (%expintegral_e1
           (add (mul -1 (take '(%expintegral_e1) (mul -1 arg)))
                (mul (inv 2)
                     (sub (take '(%log) arg)
                          (take '(%log) (inv arg))))
                (mul -1 (take '(%log) (mul -1 arg)))))
         (%expintegral_li
           (take '(%expintegral_li) (power '$%e arg)))
         ($expintegral_trig
           (add (take '(%expintegral_ci) (mul '$%i arg))
                (mul -1 '$%i (take '(%expintegral_si) (mul '$%i arg)))
                (mul (inv -2)
                     (sub (take '(%log) (inv arg))
                          (take '(%log) arg)))
                (mul -1 (take '(%log) (mul '$%i arg)))))
         ($expintegral_hyp
           (add (take '(%expintegral_chi) arg)
                (take '(%expintegral_shi) arg)
                (mul (inv -2)
                     (add (take '(%log) (inv arg))
                          (take '(%log) arg)))))))
      
      (t
       (eqtest (list '(%expintegral_ei) arg) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numerical evaluation of the Exponential Integral Ei(z):
;;;
;;; We use the following representation (see functions.wolfram.com):
;;;
;;;   Ei(z) = -E1(-z) + 0.5*(log(z)-log(1/z))-log(-z)
;;;
;;; z is a CL Complex number. Because we evaluate for Complex values we have to 
;;; take into account the complete Complex phase factors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expintegral-ei (z)
  (+ (- (expintegral-e 1 (- z)))
     ;; Carefully compute 1/2*(log(z)-log(1/z))-log(-z), using the
     ;; branch cuts that we want, not the one that Lisp wants.
     ;; (Mostly an issue with Lisps that support signed zeroes.)
     (cond 
       ((> (imagpart z) 0)
	;; Positive imaginary part. Add phase %i*%pi.
	(complex 0 (float pi)))
       ((< (imagpart z) 0)
	;; Negative imaginary part. Add phase -%i*%pi.
	(complex 0 (- (float pi))))
       ((> (realpart z) 0)
	;; Positive real value. Add phase -%i*pi.
	(complex 0 (- (float pi))))
       ;; Negative real value. No phase factor.
       (t 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We have not modified the algorithm for Bigfloat numbers. It is only
;;; generalized for Bigfloats. The calcualtion of the complex phase factor
;;; can be simplified to conditions about the sign of the realpart and 
;;; imagpart. We leave this for further work to optimize the speed of the
;;; calculation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bfloat-expintegral-ei (z)
  (let ((mz (mul -1 z)))
    (add (cmul (mul -1 bigfloatone) 
               (bfloat-expintegral-e 1 mz))
         (sub (cmul (div bigfloatone 2)
                    (sub (take '(%log) z)
                         (take '(%log) (cdiv bigfloatone z))))
              (take '(%log) mz)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 4: The implementation of the Logarithmic integral li(z)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $expintegral_li (z)
  (simplify (list '(%expintegral_li) z)))

;;; Set properties to give full support to the parser and display

(defprop $expintegral_li %expintegral_li alias)
(defprop $expintegral_li %expintegral_li verb)

(defprop %expintegral_li $expintegral_li reversealias)
(defprop %expintegral_li $expintegral_li noun)

;;; Exponential Integral Li is a simplifying function

(defprop %expintegral_li simp-expintegral-li operators)

;;; Exponential Integral Li distributes over bags

(defprop %expintegral_li (mlist $matrix mequal) distribute_over)

;;; Exponential Integral Li has mirror symmetry, 
;;; but not on the real negative axis.

(defprop %expintegral_li conjugate-expintegral-li conjugate-function)

(defun conjugate-expintegral-li (args)
  (let ((z (first args)))
    (cond ((off-negative-real-axisp z)
           ;; Definitly not on the negative real axis for z. Mirror symmetry.
           (take '(%expintegral_li) (take '($conjugate) z)))
          (t
            ;; On the negative real axis or no information. Unsimplified.
            (list '($conjugate simp) (take '(%expintegral_li) z))))))

;;; Differentiation of Exponential Integral Li 

(defprop %expintegral_li
  ((x)
   ((mtimes) ((mexpt) ((%log) x) -1)))
  grad)

;;; Integral of Exponential Li

(defprop %expintegral_li
  ((x)
   ((mplus)
      ((mtimes) x ((%expintegral_li) x))
      ((mtimes) -1  ((%expintegral_ei) ((mtimes) 2 ((%log) x))))))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We support a simplim%function. The function is looked up in simplimit and 
;;; handles specific values of the function.

(defprop %expintegral_li simplim%expintegral_li simplim%function)

(defun simplim%expintegral_li (expr var val)
  ;; Look for the limit of the argument.
  (let ((z (limit (cadr expr) var val 'think)))
  (cond
    ;; Handle an argument 1 at this place
    ((onep1 z) '$minf)
    (t
     ;; All other cases are handled by the simplifier of the function.
     (take '(%expintegral_li) z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-expintegral-li (expr ignored z)
  (declare (ignore ignored))
  (oneargcheck expr)
  (let ((arg (simpcheck (cadr expr) z)))
    (cond
      ((zerop1 arg) arg)
      ((onep1 arg)
       (simp-domain-error
	(intl:gettext "expintegral_li: expintegral_li(~:M) is undefined.") arg))
      ((eq arg '$inf) '$inf)
      ((eq arg '$infinity) '$infinity)

      ((complex-float-numerical-eval-p arg)
       (let ((carg (complex ($float ($realpart arg)) ($float ($imagpart arg)))))
         (complexify (expintegral-li carg))))

      ((complex-bigfloat-numerical-eval-p arg)
       (let* (($ratprint nil)
              (carg (add ($bfloat ($realpart arg))
                         (mul '$%i ($bfloat ($imagpart arg)))))
              (result (bfloat-expintegral-li carg)))
         (add (mul '$%i ($imagpart result))
              ($realpart result))))

      ;; Check for argument simplifications and transformations
      ((taylorize (mop expr) (second expr)))

      ((and $expintrep
            (member $expintrep *expintflag*)
            (not (eq $expintrep '%expintegral_li)))
       (let ((logarg (take '(%log) arg)))
         (case $expintrep
           (%gamma_incomplete
             (add (mul -1 (take '(%gamma_incomplete) 0 (mul -1 logarg)))
                  (mul (inv 2)
                       (sub (take '(%log) logarg)
                            (take '(%log) (inv logarg))))
                  (mul -1 (take '(%log) (mul -1 logarg)))))
           (%expintegral_e1
             (add (mul -1 (take '(%expintegral_e1) (mul -1 logarg)))
                  (mul (inv 2)
                       (sub (take '(%log) logarg)
                            (take '(%log) (inv logarg))))
                  (mul -1 (take '(%log) (mul -1 logarg)))))
           (%expintegral_ei
             ($expintegral_ei logarg))
           ($expintegral_trig
             (add (take '(%expintegral_ci) (mul '$%i logarg))
                  (mul -1 '$%i (take '(%expintegral_si) (mul '$%i logarg)))
                  (mul (inv -2)
                       (sub (take '(%log) (inv logarg))
                            (take '(%log) logarg)))
                  (mul -1 (take '(%log) (mul '$%i logarg)))))
           ($expintegral_hyp
             (add (take '(%expintegral_chi) logarg)
                  (take '(%expintegral_shi) logarg)
                  (mul (inv -2)
                       (add (take '(%log) (inv logarg))
                            (take '(%log) logarg))))))))
      
      (t
       (eqtest (list '(%expintegral_li) arg) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numerical evaluation of the Expintegral Li
;;;
;;; We use the representation:
;;;
;;;   Li(z) = Ei(log(z))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expintegral-li (z)
  (expintegral-ei (log z)))

(defun bfloat-expintegral-li (z)
  (bfloat-expintegral-ei ($log z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 5: The implementation of the Exponential Integral Si
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $expintegral_si (z)
  (simplify (list '(%expintegral_si) z)))

;;; Set properties to give full support to the parser and display

(defprop $expintegral_si %expintegral_si alias)
(defprop $expintegral_si %expintegral_si verb)

(defprop %expintegral_si $expintegral_si reversealias)
(defprop %expintegral_si $expintegral_si noun)

;;; Exponential Integral Si is a simplifying function

(defprop %expintegral_si simp-expintegral-si operators)

;;; Exponential Integral Si distributes over bags

(defprop %expintegral_si (mlist $matrix mequal) distribute_over)

;;; Exponential Integral Si has mirror symmetry

(defprop %expintegral_si t commutes-with-conjugate)

;;; Exponential Integral Si is a odd function

(defprop %expintegral_si odd-function-reflect reflection-rule)

;;; Differentiation of Exponential Integral Si

(defprop %expintegral_si
  ((x)
   ((mtimes) ((%sin) x) ((mexpt) x -1)))
  grad)

;;; Integral of Exponential Si

(defprop %expintegral_si
  ((x)
   ((mplus)
      ((%cos) x)
      ((mtimes) x ((%expintegral_si) x))))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-expintegral-si (expr ignored z)
  (declare (ignore ignored))
  (oneargcheck expr)
  (let ((arg (simpcheck (cadr expr) z)))
    (cond
      ;; Check for special values
      ((zerop1 arg) arg)
      ((eq arg '$inf) (div '$%pi 2))
      ((eq arg '$minf) (mul -1 (div '$%pi 2)))
      ((alike1 arg '((mtimes) -1 $inf)) (mul -1 (div '$%pi 2)))

      ;; Check for numerical evaluation     
      ((complex-float-numerical-eval-p arg)
       (let ((carg (complex ($float ($realpart arg)) ($float ($imagpart arg)))))
         (complexify (expintegral-si carg))))

      ((complex-bigfloat-numerical-eval-p arg)
       (let* (($ratprint nil)
              (carg (add ($bfloat ($realpart arg))
                         (mul '$%i ($bfloat ($imagpart arg)))))
              (result (bfloat-expintegral-si carg)))
         (add (mul '$%i ($imagpart result))
              ($realpart result))))

      ;; Check for argument simplifications and transformations
      ((taylorize (mop expr) (second expr)))
      ((apply-reflection-simp (mop expr) arg $trigsign))

      ((and $expintrep
            (member $expintrep *expintflag*)
            (not (eq $expintrep '$expintegral_trig)))
       (case $expintrep
         (%gamma_incomplete
           (mul (div '$%i 2)
                (add (take '(%gamma_incomplete) 0 (mul -1 '$%i arg))
                     (mul -1 (take '(%gamma_incomplete) 0 (mul '$%i arg)))
                     (take '(%log) (mul -1 '$%i arg))
                     (mul -1 (take '(%log) (mul '$%i arg))))))
         (%expintegral_e1
           (mul (div '$%i 2)
                (add (take '(%expintegral_e1) (mul -1 '$%i arg))
                     (mul -1 (take '(%expintegral_e1) (mul '$%i arg)))
                     (take '(%log) (mul -1 '$%i arg))
                     (mul -1 (take '(%log) (mul '$%i arg))))))
         (%expintegral_ei
           (mul (div '$%i 4)
                (add (mul 2
                          (sub (take '(%expintegral_ei) (mul -1 '$%i arg))
                               (take '(%expintegral_ei) (mul '$%i arg))))
                     (take '(%log) (div '$%i arg))
                     (mul -1 (take '(%log) (mul -1 (div '$%i arg))))
                     (mul -1 (take '(%log) (mul -1 '$%i arg)))
                     (take '(%log) (mul '$%i arg)))))
         (%expintegral_li
           (mul (inv (mul 2 '$%i))
                (add (take '(%expintegral_li) (power '$%e (mul '$%i arg)))
                     (mul -1
                          (take '(%expintegral_li)
                                (power '$%e (mul -1 '$%e arg))))
                     (mul (div '$%pi -2)
                          (take '(%signum) ($realpart arg))))))
         ($expintegral_hyp
           (mul -1 '$%i (take '(%expintegral_shi) (mul '$%i arg))))))
      
      (t
       (eqtest (list '(%expintegral_si) arg) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numerical evaluation of the Exponential Integral Si
;;;
;;; We use the representation:
;;;
;;;   Si(z) = %i/2 * (E1(-%i*z) - E1(*%i*z) + log(%i*z) - log(-%i*z))
;;;
;;; For the Sin, Cos, Sinh and Cosh Exponential Integrals we have to call the
;;; numerical evaluation twice. In principle we could use a direct expansion
;;; in a power series or continued fractions to optimize the speed of the code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expintegral-si (z)
  (let ((z (coerce z '(complex flonum))))
    (* (complex 0 0.5)
       (+ (expintegral-e 1 (* (complex 0 -1) z))
	  (- (expintegral-e 1 (* (complex 0 1) z)))
	  (log (* (complex 0 -1) z))
	  (- (log (* (complex 0 1) z)))))))

(defun bfloat-expintegral-si (z)
  (let ((z*%i (cmul '$%i z))
        (mz*%i (cmul (mul -1 '$%i) z)))        
  (cmul
    (mul 0.5 '$%i)
    (add
      (bfloat-expintegral-e 1 mz*%i)
      (mul -1 (bfloat-expintegral-e 1 z*%i))
      ($log mz*%i)
      (mul -1 ($log z*%i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 6: The implementation of the Exponential Integral Shi
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $expintegral_shi (z)
  (simplify (list '(%expintegral_shi) z)))

;;; Set properties to give full support to the parser and display

(defprop $expintegral_shi %expintegral_shi alias)
(defprop $expintegral_shi %expintegral_shi verb)

(defprop %expintegral_shi $expintegral_shi reversealias)
(defprop %expintegral_shi $expintegral_shi noun)

;;; Exponential Integral Shi is a simplifying function

(defprop %expintegral_shi simp-expintegral-shi operators)

;;; Exponential Integral Shi distributes over bags

(defprop %expintegral_shi (mlist $matrix mequal) distribute_over)

;;; Exponential Integral Shi has mirror symmetry

(defprop %expintegral_si t commutes-with-conjugate)

;;; Exponential Integral Shi is a odd function

(defprop %expintegral_si odd-function-reflect reflection-rule)

;;; Differentiation of Exponential Integral Shi

(defprop %expintegral_shi
  ((x)
   ((mtimes) ((%sinh) x) ((mexpt) x -1)))
  grad)

;;; Integral of Exponential Shi

(defprop %expintegral_shi
  ((x)
   ((mplus)
      ((mtimes) -1 ((%cosh) x))
      ((mtimes) x ((%expintegral_shi) x))))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-expintegral-shi (expr ignored z)
  (declare (ignore ignored))
  (oneargcheck expr)
  (let ((arg (simpcheck (cadr expr) z)))
    (cond
      ;; Check for special values
      ((zerop1 arg) arg)
      ((alike1 arg '((mtimes) '$%i $inf)) (div (mul '$%i '$%pi) 2))
      ((alike1 arg '((mtimes) -1 '$%i $inf)) (div (mul -1 '$%i '$%pi) 2))

      ;; Check for numrical evaluation
      ((float-numerical-eval-p arg)
       (realpart (expintegral-shi arg)))
      
      ((complex-float-numerical-eval-p arg)
       (let ((carg (complex ($float ($realpart arg)) ($float ($imagpart arg)))))
         (complexify (expintegral-shi carg))))

      ((complex-bigfloat-numerical-eval-p arg)
       (let* (($ratprint nil)
              (carg (add ($bfloat ($realpart arg))
                         (mul '$%i ($bfloat ($imagpart arg)))))
              (result (bfloat-expintegral-shi carg)))
         (add (mul '$%i ($imagpart result))
              ($realpart result))))

      ;; Check for argument simplifications and transformations
      ((taylorize (mop expr) (second expr)))
      ((apply-reflection-simp (mop expr) arg $trigsign))

      ((and $expintrep
            (member $expintrep *expintflag*)
            (not (eq $expintrep '$expintegral_hyp)))
       (case $expintrep
         (%gamma_incomplete
           (mul (inv 2)
                (add (take '(%gamma_incomplete) 0 arg)
                     (mul -1 (take '(%gamma_incomplete) 0 (mul -1 arg)))
                     (mul -1 (take '(%log) (mul -1 arg)))
                     (take '(%log) arg))))
         (%expintegral_e1
           (mul (inv 2)
                (add (take '(%expintegral_e1) arg)
                     (mul -1 (take '(%expintegral_e1) (mul -1 arg)))
                     (mul -1 (take '(%log) (mul -1 arg)))
                     (take '(%log) arg))))
         (%expintegral_ei
           (mul (inv 4)
                (add (mul 2
                          (sub (take '(%expintegral_ei) arg)
                               (take '(%expintegral_ei) (mul -1 arg))))
                     (take '(%log) (inv arg))
                     (mul -1 (take '(%log) (mul -1 (inv arg))))
                     (take '(%log) (mul -1 arg))
                     (mul -1 (take '(%log) arg)))))
         (%expintegral_li
           (add (mul (inv 2)
                     (sub (take '(%expintegral_li) (power '$%e arg))
                          (take '(%expintegral_li) (power '$%e (mul -1 arg)))))
                (mul (div (mul '$%i '$%pi) -2)
                     (take '(%signum) ($imagpart arg)))))
         ($expintegral_trig
           (mul -1 '$%i (take '(%expintegral_si) (mul '$%i arg))))))
      
      (t
       (eqtest (list '(%expintegral_shi) arg) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numerical evaluation of the Exponential Integral Shi
;;;
;;; We use the representation:
;;;
;;;   Shi(z) = 1/2 * (E1(z) - E1(-z) - log(-z) + log(z))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expintegral-shi (z)
  (* 
    0.5
    (+
      (expintegral-e 1 z)
      (- (expintegral-e 1 (- z)))
      (- (log (- z)))
      (log z))))

(defun bfloat-expintegral-shi (z)
  (let ((mz (mul -1 z)))
    (mul 
      0.5
      (add
        (bfloat-expintegral-e 1 z)
        (mul -1 (bfloat-expintegral-e 1 mz))
        (mul -1 ($log mz))
        ($log z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 7: The implementation of the Exponential Integral Ci
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $expintegral_ci (z)
  (simplify (list '(%expintegral_ci) z)))

;;; Set properties to give full support to the parser and display

(defprop $expintegral_ci %expintegral_ci alias)
(defprop $expintegral_ci %expintegral_ci verb)

(defprop %expintegral_ci $expintegral_ci reversealias)
(defprop %expintegral_ci $expintegral_ci noun)

;;; Exponential Integral Ci is a simplifying function

(defprop %expintegral_ci simp-expintegral-ci operators)

;;; Exponential Integral Ci distributes over bags

(defprop %expintegral_ci (mlist $matrix mequal) distribute_over)

;;; Exponential Integral Ci has mirror symmetry, 
;;; but not on the real negative axis.

(defprop %expintegral_ci conjugate-expintegral-ci conjugate-function)

(defun conjugate-expintegral-ci (args)
  (let ((z (first args)))
    (cond ((off-negative-real-axisp z)
           ;; Definitly not on the negative real axis for z. Mirror symmetry.
           (take '(%expintegral_ci) (take '($conjugate) z)))
          (t
           ;; On the negative real axis or no information. Unsimplified.
           (list '($conjugate simp) (take '(%expintegral_ci) z))))))

;;; Differentiation of Exponential Integral Ci

(defprop %expintegral_ci
  ((x)
   ((mtimes) ((%cos) x) ((mexpt) x -1)))
  grad)

;;; Integral of Exponential Ci

(defprop %expintegral_ci
  ((x)
   ((mplus)
      ((mtimes) x ((%expintegral_ci) x))
      ((mtimes) -1 ((%sin) x))))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We support a simplim%function. The function is looked up in simplimit and 
;;; handles specific values of the function.

(defprop %expintegral_ci simplim%expintegral_ci simplim%function)

(defun simplim%expintegral_ci (expr var val)
  ;; Look for the limit of the argument.
  (let ((z (limit (cadr expr) var val 'think)))
  (cond
    ;; Handle an argument 0 at this place
    ((or (zerop1 z)
         (eq z '$zeroa)
         (eq z '$zerob))
     '$minf)
    (t
     ;; All other cases are handled by the simplifier of the function.
     (take '(%expintegral_ci) z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-expintegral-ci (expr ignored z)
  (declare (ignore ignored))
  (oneargcheck expr)
  (let ((arg (simpcheck (cadr expr) z)))
    (cond
      ;; Check for special values
      ((zerop1 arg)
       (simp-domain-error
	(intl:gettext "expintegral_ci: expintegral_ci(~:M) is undefined.") arg))
      ((eq arg '$inf) 0)
      ((eq arg '$minf) (mul '$%i '$%pi))
      ((alike1 arg '((mtimes) -1 $inf)) (mul '$%pi '$%pi))

      ;; Check for numerical evaluation
      ((complex-float-numerical-eval-p arg)
       (let ((carg (complex ($float ($realpart arg)) ($float ($imagpart arg)))))
         (complexify (expintegral-ci carg))))

      ((complex-bigfloat-numerical-eval-p arg)
       (let* (($ratprint nil)
              (carg (add ($bfloat ($realpart arg))
                         (mul '$%i ($bfloat ($imagpart arg)))))
              (result (bfloat-expintegral-ci carg)))
         (add (mul '$%i ($imagpart result))
              ($realpart result))))

      ;; Check for argument simplifications and transformations
      ((taylorize (mop expr) (second expr)))

      ((and $expintrep
            (member $expintrep *expintflag*)
            (not (eq $expintrep '$expintegral_trig)))
       (case $expintrep
         (%gamma_incomplete
           (sub (take '(%log) arg)
                (mul (inv 2)
                     (add (take '(%gamma_incomplete) 0 (mul -1 '$%i arg))
                          (take '(%gamma_incomplete) 0 (mul '$%i arg))
                          (take '(%log) (mul -1 '$%i arg))
                          (take '(%log) (mul '$%i arg))))))
         (%expintegral_e1
           (add (mul (inv -2)
                     (add (take '(%expintegral_e1) (mul -1 '$%i arg))
                          (take '(%expintegral_e1) (mul '$%i arg)))
                     (take '(%log) (mul -1 '$%i arg))
                     (take '(%log) (mul '$%i arg)))
                (take '(%log) arg)))
         (%expintegral_ei
           (add (mul (inv 4)
                     (add (mul 2
                               (add (take '(%expintegral_ei) (mul -1 '$%i arg))
                                    (take '(%expintegral_ei) (mul '$%i arg))))
                          (take '(%log) (div '$%i arg))
                          (take '(%log) (mul -1 '$%i (inv arg)))
                          (mul -1 (take '(%log) (mul -1 '$%i arg)))
                          (mul -1 (take '(%log) (mul '$%i arg)))))
                (take '(%log) arg)))
         (%expintegral_li
           (add (mul (inv 2)
                     (add (take '(%expintegral_li)
                                (power '$%e (mul -1 '$%i arg)))
                          (take '(%expintegral_li)
                                (power '$%e (mul '$%i arg)))))
                (mul (div (mul '$%i '$%pi) 2)
                     (take '(%signum) ($imagpart arg)))
                (sub 1 (take '(%signum) ($realpart arg)))))
         ($expintegral_hyp
           (add (take '(%expintegral_chi) (mul '$%i arg))
                (mul -1 (take '(%log) (mul '$%i arg)))
                (take '(%log) arg)))))
      
      (t
       (eqtest (list '(%expintegral_ci) arg) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numerical evaluation of the Exponential Integral Ci
;;;
;;; We use the representation:
;;;
;;;   Ci(z) = -1/2 * (E1(-%i*z) + E1(%i*z) + log(-%i*z) + log(%i*z)) + log(z)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expintegral-ci (z)
  (let ((z (coerce z '(complex flonum))))
    (+ (* -0.5
	  (+ (expintegral-e 1 (* (complex 0 -1) z))
	     (expintegral-e 1 (* (complex 0 1) z))
	     (log (* (complex 0 -1) z))
	     (log (* (complex 0 1) z))))
       (log z))))

(defun bfloat-expintegral-ci (z)
  (let ((z*%i (cmul '$%i z))
        (mz*%i (cmul (mul -1 '$%i) z)))
  (add
    (cmul 
      -0.5
      (add
        (bfloat-expintegral-e 1 mz*%i)
        (bfloat-expintegral-e 1 z*%i)
        ($log mz*%i)
        ($log z*%i)))
    ($log z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part 8: The implementation of the Exponential Integral Chi
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $expintegral_chi (z)
  (simplify (list '(%expintegral_chi) z)))

;;; Set properties to give full support to the parser and display

(defprop $expintegral_chi %expintegral_chi alias)
(defprop $expintegral_chi %expintegral_chi verb)

(defprop %expintegral_chi $expintegral_chi reversealias)
(defprop %expintegral_chi $expintegral_chi noun)

;;; Exponential Integral Chi is a simplifying function

(defprop %expintegral_chi simp-expintegral-chi operators)

;;; Exponential Integral Chi distributes over bags

(defprop %expintegral_chi (mlist $matrix mequal) distribute_over)

;;; Exponential Integral Chi has mirror symmetry, 
;;; but not on the real negative axis.

(defprop %expintegral_chi conjugate-expintegral-chi conjugate-function)

(defun conjugate-expintegral-chi (args)
  (let ((z (first args)))
    (cond ((off-negative-real-axisp z)
           ;; Definitly not on the negative real axis for z. Mirror symmetry.
           (take '(%expintegral_chi) (take '($conjugate) z)))
          (t
           ;; On the negative real axis or no information. Unsimplified.
           (list '($conjugate simp) (take '(%expintegral_chi) z))))))

;;; Differentiation of Exponential Integral Chi

(defprop %expintegral_chi
  ((x)
   ((mtimes) ((%cosh) x) ((mexpt) x -1)))
  grad)

;;; Integral of Exponential Chi

(defprop %expintegral_chi
  ((x)
   ((mplus)
      ((mtimes) x ((%expintegral_chi) x))
      ((mtimes) -1 ((%sinh) x))))
  integral)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We support a simplim%function. The function is looked up in simplimit and 
;;; handles specific values of the function.

(defprop %expintegral_chi simplim%expintegral_chi simplim%function)

(defun simplim%expintegral_chi (expr var val)
  ;; Look for the limit of the argument.
  (let ((z (limit (cadr expr) var val 'think)))
  (cond
    ;; Handle an argument 0 at this place
    ((or (zerop1 z)
         (eq z '$zeroa)
         (eq z '$zerob))
     '$minf)
    (t
     ;; All other cases are handled by the simplifier of the function.
     (take '(%expintegral_chi) z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-expintegral-chi (expr ignored z)
  (declare (ignore ignored))
  (oneargcheck expr)
  (let ((arg (simpcheck (cadr expr) z)))
    (cond
      ;; Check for special values
      ((zerop1 arg) 
       ;; First check for zero argument. Throw Maxima error.
       (simp-domain-error
	(intl:gettext "expintegral_chi: expintegral_chi(~:M) is undefined.") 
        arg))
      ((alike1 arg '((mtimes) $%i $inf)) (div (mul '$%pi '$%i) 2))
      ((alike1 arg '((mtimes) -1 $%i $inf)) (div (mul -1 '$%pi '$%i) 2))

      ;; Check for numerical evaluation
      ((complex-float-numerical-eval-p arg)
       (let ((carg (complex ($float ($realpart arg)) ($float ($imagpart arg)))))
         (complexify (expintegral-chi carg))))

      ((complex-bigfloat-numerical-eval-p arg)
       (let* (($ratprint nil)
              (carg (add ($bfloat ($realpart arg))
                         (mul '$%i ($bfloat ($imagpart arg)))))
              (result (bfloat-expintegral-chi carg)))
         (add (mul '$%i ($imagpart result))
              ($realpart result))))
      
      ;; Check for argument simplifications and transformations
      ((taylorize (mop expr) (second expr)))

      ((and $expintrep
            (member $expintrep *expintflag*)
            (not (eq $expintrep '$expintegral_hyp)))
       (case $expintrep
         (%gamma_incomplete
           (mul (inv -2)
                (add (take '(%gamma_incomplete) 0 (mul -1 arg))
                     (take '(%gamma_incomplete) 0 arg)
                     (take '(%log) (mul -1 arg))
                     (mul -1 (take '(%log) arg)))))
         (%expintegral_e1
           (mul (inv -2)
                (add (take '(%expintegral_e1) (mul -1 arg))
                     (take '(%expintegral_e1) arg)
                     (take '(%log) (mul -1 arg))
                     (mul -1 (take '(%log) arg)))))
         (%expintegral_ei
           (mul (inv 4)
                (add (mul 2
                          (add (take '(%expintegral_ei) (mul -1 arg))
                               (take '(%expintegral_ei) arg)))
                     (take '(%log) (inv arg))
                     (take '(%log) (mul -1 (inv arg)))
                     (mul -1 (take '(%log) (mul -1 arg)))
                     (mul 3 (take '(%log) arg)))))
         (%expintegral_li
           (add (mul (inv 2)
                     (add (take '(%expintegral_li) (power '$%e (mul -1 arg)))
                          (take '(%expintegral_li) (power '$%e arg))))
                (mul (div (mul '$%i '$%pi) 2)
                     (take '(%signum) ($imagpart arg)))
                (mul (inv 2)
                     (add (take '(%log) (inv arg))
                          (take '(%log) arg)))))
         ($expintegral_trig
           (add (take '(%expintegral_ci) (mul '$%i arg))
                (take '(%log) arg)
                (mul -1 (take '(%log) (mul '$%i arg)))))))
      
      (t
       (eqtest (list '(%expintegral_chi) arg) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numerical evaluation of the Exponential Integral Ci
;;;
;;; We use the representation:
;;;
;;;   Chi(z) = -1/2 * (E1(-z) + E1(z) + log(-z) - log(z))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expintegral-chi (z)
  (* 
    -0.5
    (+
      (expintegral-e 1 z)
      (expintegral-e 1 (- z))
      (log (- z))
      (- (log z)))))

(defun bfloat-expintegral-chi (z)
  (let ((mz (mul -1 z)))
    (mul 
      -0.5
      (add
        (bfloat-expintegral-e 1 z)
        (bfloat-expintegral-e 1 mz)
        ($log mz)
        (mul -1 ($log z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moved from bessel.lisp 2008-12-11.  Consider deleting it.
;;
;; Exponential integral E1(x).  The Cauchy principal value is used for
;; negative x.
(defun $expint (x)
  (cond ((numberp x)
	 (values (slatec:de1 (float x))))
	(t
	 (list '($expint simp) x))))
