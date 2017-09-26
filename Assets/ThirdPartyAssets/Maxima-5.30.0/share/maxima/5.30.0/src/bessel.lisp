;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;; When non-NIL, the Bessel functions of half-integral order are
;; expanded in terms of elementary functions.

(defmvar $besselexpand nil)

;; When T Bessel functions with an integer order are reduced to order 0 and 1
(defmvar $bessel_reduce nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Helper functions for this file

;; If E is a maxima ratio with a denominator of DEN, return the ratio
;; as a Lisp rational.  Otherwise NIL.
(defun max-numeric-ratio-p (e den)
  (if (and (listp e)
           (eq 'rat (caar e))
           (= den (third e))
           (integerp (second e)))
      (/ (second e) (third e))
      nil))

(defun bessel-numerical-eval-p (order arg)
  ;; Return non-NIL if we should numerically evaluate a bessel
  ;; function.  Basically, both args have to be numbers.  If both args
  ;; are integers, we don't evaluate unless $numer is true.
  (or (and (numberp order) (complex-number-p arg)
           (or (floatp order) 
               (floatp ($realpart arg)) 
               (floatp ($imagpart arg))))
      (and $numer (numberp order)
           (complex-number-p arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Bessel J function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun $bessel_j (v z)
  (simplify (list '(%bessel_j) v z)))

(defprop $bessel_j %bessel_j alias)
(defprop $bessel_j %bessel_j verb)
(defprop %bessel_j $bessel_j reversealias)
(defprop %bessel_j $bessel_j noun)

;; Bessel J is a simplifying function.

(defprop %bessel_j simp-bessel-j operators)

;; Bessel J distributes over lists, matrices, and equations

(defprop %bessel_j (mlist $matrix mequal) distribute_over)

;; Derivatives of the Bessel function.
(defprop %bessel_j
    ((n x)
     ;; Derivative wrt to order n.  A&S 9.1.64.  Do we really want to
     ;; do this?  It's quite messy.
     ;;
     ;; J[n](x)*log(x/2) 
     ;;       - (x/2)^n*sum((-1)^k*psi(n+k+1)/gamma(n+k+1)*(z^2/4)^k/k!,k,0,inf)
     ((mplus)
      ((mtimes)
       ((%bessel_j) n x)
       ((%log) ((mtimes) ((rat) 1 2) x)))
      ((mtimes) -1
       ((mexpt) ((mtimes) x ((rat) 1 2)) n)
       ((%sum)
        ((mtimes) ((mexpt) -1 $%k)
         ((mexpt) ((mfactorial) $%k) -1)
         ((mqapply) (($psi array) 0) ((mplus) 1 $%k n))
         ((mexpt) ((%gamma) ((mplus) 1 $%k n)) -1)
         ((mexpt) ((mtimes) x x ((rat) 1 4)) $%k))
        $%k 0 $inf)))
      
     ;; Derivative wrt to arg x.  
     ;; A&S 9.1.27; changed from 9.1.30 so that taylor works on Bessel functions
     ((mtimes) 
      ((mplus) 
       ((%bessel_j) ((mplus) -1 n) x) 
       ((mtimes) -1 ((%bessel_j) ((mplus) 1 n) x))) 
      ((rat) 1 2)))
  grad)

;; Integral of the Bessel function wrt z
(defun bessel-j-integral-2 (v z)
  (case v
    (0 
     ;; integrate(bessel_j(0,z)
     ;; = (1/2)*z*(%pi*bessel_j(1,z)*struve_h(0,z)
     ;;            +bessel_j(0,z)*(2-%pi*struve_h(1,z)))
     `((mtimes) ((rat) 1 2) ,z
       ((mplus)
	((mtimes) $%pi 
	 ((%bessel_j) 1 ,z)
	 ((%struve_h) 0 ,z))
	((mtimes) 
	 ((%bessel_j) 0 ,z)
	 ((mplus) 2 ((mtimes) -1 $%pi ((%struve_h) 1 ,z)))))))
    (1
     ;; integrate(bessel_j(1,z) = -bessel_j(0,z)
     `((mtimes) -1 ((%bessel_j) 0 ,z)))
    (otherwise
     ;; http://functions.wolfram.com/03.01.21.0002.01
     ;; integrate(bessel_j(v,z)
     ;;  = 2^(-v-1)*z^(v+1)*gamma(v/2+1/2)
     ;;   * hypergeometric_regularized([v/2+1/2],[v+1,v/2+3/2],-z^2/4)
     ;;  = 2^(-v-1)*z^(v+1)*hypergeometric([v/2+1/2],[v+1,v/2+3/2],-z^2/4)
     ;;   / ((v/2+1/2)*gamma(v+1))
     `((mtimes)
       (($hypergeometric)
	((mlist)
	 ((mplus) ((rat) 1 2) ((mtimes) ((rat) 1 2) ,v)))
	((mlist)
	 ((mplus) ((rat) 3 2) ((mtimes) ((rat) 1 2) ,v))
	 ((mplus) 1 ,v))
	((mtimes) ((rat) -1 4) ((mexpt) ,z 2)))
       ((mexpt) ((mplus) ((rat) 1 2) ((mtimes) ((rat) 1 2) ,v)) -1)
       ((mexpt) 2 ((mplus) -1 ((mtimes) -1 ,v)))
       ((mexpt) ((%gamma) ((mplus) 1 ,v)) -1) 
       ((mexpt) z ((mplus ) 1 ,v))))))

(putprop '%bessel_j `((v z) nil ,#'bessel-j-integral-2) 'integral)

;; Support a simplim%bessel_j function to handle specific limits

(defprop %bessel_j simplim%bessel_j simplim%function)

(defun simplim%bessel_j (expr var val)
  ;; Look for the limit of the arguments.
  (let ((v (limit (cadr expr) var val 'think))
        (z (limit (caddr expr) var val 'think)))
  (cond
    ;; Handle an argument 0 at this place.
    ((or (zerop1 z)
         (eq z '$zeroa)
         (eq z '$zerob))
     (let ((sgn ($sign ($realpart v))))
       (cond ((and (eq sgn '$neg)
                   (not (maxima-integerp v)))
              ;; bessel_j(v,0), Re(v)<0 and v not an integer
              '$infinity)
             ((and (eq sgn '$zero)
                   (not (zerop1 v)))
              ;; bessel_j(v,0), Re(v)=0 and v #0
              '$und)
             ;; Call the simplifier of the function.
             (t (simplify (list '(%bessel_j) v z))))))
    ((or (eq z '$inf)
         (eq z '$minf))
     ;; bessel_j(v,inf) or bessel_j(v,minf)
     0)
    (t
     ;; All other cases are handled by the simplifier of the function.
     (simplify (list '(%bessel_j) v z))))))

(defun simp-bessel-j (expr ignored z)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((order (simpcheck (cadr expr) z))
        (arg   (simpcheck (caddr expr) z))
        (rat-order nil))
    (cond
      ((zerop1 arg)
       ;; We handle the different case for zero arg carefully.
       (let ((sgn ($sign ($realpart order))))
         (cond ((and (eq sgn '$zero)
                     (zerop1 ($imagpart order)))
                ;; bessel_j(0,0) = 1
                (cond ((or ($bfloatp order) ($bfloatp arg)) ($bfloat 1))
                      ((or (floatp order) (floatp arg)) 1.0)
                      (t 1)))
               ((or (eq sgn '$pos)
                    (maxima-integerp order))
                ;; bessel_j(v,0) and Re(v)>0 or v an integer
                (cond ((or ($bfloatp order) ($bfloatp arg)) ($bfloat 0))
                      ((or (floatp order) (floatp arg)) 0.0)
                      (t 0)))
               ((and (eq sgn '$neg)
                     (not (maxima-integerp order)))
                ;; bessel_j(v,0) and Re(v)<0 and v not an integer
                (simp-domain-error
                  (intl:gettext "bessel_j: bessel_j(~:M,~:M) is undefined.")
                  order arg))
               ((and (eq sgn '$zero)
                     (not (zerop1 ($imagpart order))))
                ;; bessel_j(v,0) and Re(v)=0 and v # 0
                (simp-domain-error
                  (intl:gettext "bessel_j: bessel_j(~:M,~:M) is undefined.")
                  order arg))
               (t
                ;; No information about the sign of the order
                (eqtest (list '(%bessel_j) order arg) expr)))))
      
      ((complex-float-numerical-eval-p order arg)
       ;; We have numeric order and arg and $numer is true, or we have either 
       ;; the order or arg being floating-point, so let's evaluate it 
       ;; numerically.
       ;; The numerical routine bessel-j returns a CL number, so we have
       ;; to add the conversion to a Maxima-complex-number.
       (cond ((= 0 ($imagpart order))
              ;; order is real, arg is real or complex
              (let* ((order ($float order))
                     (arg (complex ($float ($realpart arg))
                                   ($float ($imagpart arg))))
                     (result (bessel-j order arg)))
                (add (mul '$%i (imagpart result))
                     (realpart result))))
             (t
              ;; order is complex, arg is real or complex
              ;; Use 1/gamma(v+1)*(z/2)^v*0F1(;v+1;-z^2/4)
              (let (($numer t)
                    ($float t)
                    (order ($float order))
                    (arg ($float arg)))
                ($float
                  ($rectform
                    (mul (inv (take '(%gamma) (add order 1.0)))
                         (power (div arg 2.0) order)
                         (take '($hypergeometric)
                               (list '(mlist))
                               (list '(mlist) (add order 1.0))
                               (neg (div (mul arg arg) 4.0))))))))))
      
      ((and (integerp order) (minusp order))
       ;; Some special cases when the order is an integer.
       ;; A&S 9.1.5: J[-n](x) = (-1)^n*J[n](x)
       (if (evenp order)
           (take '(%bessel_j) (- order) arg)
           (mul -1 (take '(%bessel_j) (- order) arg))))
      
      ((and $besselexpand 
            (setq rat-order (max-numeric-ratio-p order 2)))
       ;; When order is a fraction with a denominator of 2, we
       ;; can express the result in terms of elementary functions.
       (bessel-j-half-order rat-order arg))
      
      ((and $bessel_reduce
            (and (integerp order)
                 (plusp order)
                 (> order 1)))
       ;; Reduce a bessel function of order > 2 to order 1 and 0:
       ;; bessel_j(v,z) -> 2*(v-1)/z*bessel_j(v-1,z)-bessel_i(v-2,z)
       (sub (mul 2
                 (- order 1)
                 (inv arg) 
                 (take '(%bessel_j) (- order 1) arg))
            (take '(%bessel_j) (- order 2) arg)))

      ((and $%iargs (multiplep arg '$%i))
       ;; bessel_j(v, %i*x) = (%i*x)^v/(x^v) * bessel_i(v, x)
       ;; (From http://functions.wolfram.com/03.01.27.0002.01)
       (let ((x (coeff arg '$%i 1)))
	 (mul (power (mul '$%i x) order)
	      (inv (power x order))
	      (take '(%bessel_i) order x))))

      ($hypergeometric_representation
        ;; Return Hypergeometric representation of bessel_j
        (mul (inv (take '(%gamma) (add order 1)))
             (power (div arg 2) order)
             (take '($hypergeometric)
                   (list '(mlist))
                   (list '(mlist) (add order 1))
                   (neg (div (mul arg arg) 4)))))
      
      (t
       (eqtest (list '(%bessel_j) order arg) expr)))))

;; Compute value of Bessel function of the first kind of order ORDER.
(defun bessel-j (order arg)
  (cond 
    ((zerop (imagpart arg))
     ;; We have numeric args and the arg is purely real. 
     ;; Call the real-valued Bessel function when possible.
     (let ((arg (realpart arg)))
       (cond
         ((= order 0)
          (slatec:dbesj0 (float arg)))
         ((= order 1)
          (slatec:dbesj1 (float arg)))
         ((minusp order)
          (cond ((zerop (nth-value 1 (truncate order)))
                 ;; The order is a negative integer.
                 ;; We use J[n](z)=(-1)^n*J[n](z) and not the Hankel functions.
                 (if (evenp (floor order)) 
                     (bessel-j (- order) arg)
                     (- (bessel-j (- order) arg))))
                (t
                 ;; Bessel function of negative order.  We use the Hankel
                 ;; functions to compute this: J(v,z)= 0.5*(H1(v,x) +
                 ;; H2(v,x)).  This works for negative and positive arg
                 ;; and handles special cases correctly.
                 (let ((result (* 0.5 (+ (hankel-1 order arg) (hankel-2 order arg)))))
                   (cond ((= (nth-value 1 (floor order)) 1/2)
                          ;; ORDER is a half-integral-value or a float
                          ;; representation, thereof.
                          (if (minusp arg)
                              ;; arg is negative, the result is purely imaginary
                              (complex 0 (imagpart result))
                              ;; arg is positive, the result is purely real
                              (realpart result)))
                         ;; in all other cases general complex result
                         (t result))))))
         (t
          ;; We have a real arg and order > 0 and order not 0 or 1
          ;; for this case we can call the function dbesj
          (multiple-value-bind (n alpha) (floor (float order))
            (let ((jvals (make-array (1+ n) :element-type 'flonum)))
              (slatec:dbesj (abs (float arg)) alpha (1+ n) jvals 0)
              (cond ((>= arg 0) 
                     (aref jvals n))
                    (t
                     ;; Use analytic continuation formula A&S 9.1.35:
                     ;; %j[v](z*exp(m*%pi*%i)) = exp(m*%pi*%i*v)*%j[v](z)
                     ;; for an integer m.  In particular, for m = 1:
                     ;; %j[v](-x) = exp(v*%pi*%i)*%j[v](x)
                     ;; and handle special cases
                     (cond ((zerop (nth-value 1 (truncate order)))
                            ;; order is an integer
                            (if (evenp (floor order))
                                (aref jvals n)
                                (- (aref jvals n))))
                           ((= (nth-value 1 (floor order)) 1/2)
                            ;; Order is a half-integral-value and we know that 
                            ;; arg < 0, so the result is purely imginary.
                            (if (evenp (floor order))
                                (complex 0 (aref jvals n))
                                (complex 0 (- (aref jvals n)))))
                           ;; In all other cases a general complex result
                           (t
                            (* (cis (* order pi))
                               (aref jvals n))))))))))))
    (t
     ;; The arg is complex. Use the complex-valued Bessel function.
     (cond ((mminusp order)
            ;; Bessel function of negative order. We use the Hankel function to 
            ;; compute this, because A&S 9.1.3 says H1(v,z)=J(v,z) + i * Y(v,z),
            ;; and H2(v,z) = J(v,z) - i * Y(v,z).    
            ;; Thus, J(v,z) = (H1(v,z) + H2(v,z))/2. Not the most efficient way,
            ;; but perhaps good enough for maxima.
            (* 0.5 (+ (hankel-1 order arg) (hankel-2 order arg))))
           (t
            (multiple-value-bind (n alpha) (floor (float order))
              (let ((cyr (make-array (1+ n) :element-type 'flonum))
                    (cyi (make-array (1+ n) :element-type 'flonum)))
                (multiple-value-bind (v-zr v-zi v-fnu v-kode v-n
                                           v-cyr v-cyi v-nz v-ierr)
                   (slatec:zbesj (float (realpart arg))
                                 (float (imagpart arg))
                                 alpha 1 (1+ n) cyr cyi 0 0)
                  (declare (ignore v-zr v-zi v-fnu v-kode v-n v-cyr v-cyi v-nz))
                  
                  ;; Should check the return status in v-ierr of this routine.
                  (when (plusp v-ierr)
                    (format t "zbesj ierr = ~A~%" v-ierr))
                  (complex (aref cyr n) (aref cyi n))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Bessel Y function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun $bessel_y (v z)
  (simplify (list '(%bessel_y) v z)))

(defprop $bessel_y %bessel_y alias)
(defprop $bessel_y %bessel_y verb)
(defprop %bessel_y $bessel_y reversealias)
(defprop %bessel_y $bessel_y noun)

(defprop %bessel_y simp-bessel-y operators)

;; Bessel Y distributes over lists, matrices, and equations

(defprop %bessel_y (mlist $matrix mequal) distribute_over)

(defprop %bessel_y
    ((n x)
     ;; A&S 9.1.65
     ;;
     ;; cot(n*%pi)*[diff(bessel_j(n,x),n)-%pi*bessel_y(n,x)]
     ;;  - csc(n*%pi)*diff(bessel_j(-n,x),n)-%pi*bessel_j(n,x)
     ((mplus)
      ((mtimes) $%pi ((%bessel_j) n x))
      ((mtimes)
       -1
       ((%csc) ((mtimes) $%pi n))
       ((%derivative) ((%bessel_j) ((mtimes) -1 n) x) x 1))
      ((mtimes)
       ((%cot) ((mtimes) $%pi n))
       ((mplus)
        ((mtimes) -1 $%pi ((%bessel_y) n x))
        ((%derivative) ((%bessel_j) n x) n 1))))

     ;; Derivative wrt to arg x.  A&S 9.1.27; changed from A&S 9.1.30
     ;; to be consistent with bessel_j.
     ((mtimes) 
      ((mplus) 
       ((%bessel_y)((mplus) -1 n) x) 
       ((mtimes) -1 ((%bessel_y) ((mplus) 1 n) x))) 
      ((rat) 1 2)))
    grad)

;; Integral of the Bessel Y function wrt z
;; http://functions.wolfram.com/Bessel-TypeFunctions/BesselY/21/01/01/
(defun bessel-y-integral-2 (n z)
  ;;(declare (ignore unused))
  (cond 
   ((and ($integerp n) (<= 0 n))
    (cond
     (($oddp n)
      ;; integrate(bessel_y(2*N+1,z)) , N > 0 
      ;; = -bessel_y(0,z) - 2 * sum(bessel_y(2*k,z),k,1,N)
      (let* ((k (gensym))
	     (answer `((mplus) ((mtimes) -1 ((%bessel_y) 0 ,z))
                       ((mtimes) -2
                        ((%sum) ((%bessel_y) ((mtimes) 2 ,k) ,z) ,k 1
                         ((mtimes) ((rat) 1 2) ((mplus) -1 ,n)))))))
	;; Expand out the sum if n < 10.  Otherwise fix up the indices
	(if (< n 10) 
            (meval `(($ev) ,answer $sum))   ; Is there a better way?
	  (simplify ($niceindices answer)))))
     (($evenp n)
      ;; integrate(bessel_y(2*N,z)) , N > 0
      ;; = (1/2)*%pi*z*(bessel_y(0,z)*struve_h(-1,z)
      ;;               +bessel_y(1,z)*struve_h(0,z))
      ;;    - 2 * sum(bessel_y(2*k+1,z),k,0,N-1)
      (let* 
	  ((k (gensym))
	   (answer `((mplus)
		     ((mtimes) -2
		      ((%sum)
		       ((%bessel_y) ((mplus) 1 ((mtimes) 2 ,k)) ,z)
		       ,k 0
		       ((mplus)
		        -1
		        ((mtimes) ((rat) 1 2) ,n))))
		     ((mtimes) ((rat) 1 2) $%pi ,z
		      ((mplus)
		       ((mtimes)
			((%bessel_y) 0 ,z)
			((%struve_h) -1 ,z))
		       ((mtimes)
			((%bessel_y) 1 ,z)
			((%struve_h) 0 ,z)))))))
	;; Expand out the sum if n < 10.  Otherwise fix up the indices
	(if (< n 10) 
            (meval `(($ev) ,answer $sum))  ; Is there a better way?
	  (simplify ($niceindices answer)))))))
   (t nil)))

(putprop '%bessel_y `((n z) nil ,#'bessel-y-integral-2) 'integral)

;; Support a simplim%bessel_y function to handle specific limits

(defprop %bessel_y simplim%bessel_y simplim%function)

(defun simplim%bessel_y (expr var val)
  ;; Look for the limit of the arguments.
  (let ((v (limit (cadr expr) var val 'think))
        (z (limit (caddr expr) var val 'think)))
  (cond
    ;; Handle an argument 0 at this place.
    ((or (zerop1 z)
         (eq z '$zeroa)
         (eq z '$zerob))
     (cond ((zerop1 v)
            ;; bessel_y(0,0)
            '$minf)
           ((integerp v)
            ;; bessel_y(n,0), n an integer
            (cond ((evenp v) '$minf)
                  (t (cond ((eq z '$zeroa) '$minf)
                           ((eq z '$zerob) '$inf)
                           (t '$infinity)))))
           ((not (zerop1 ($realpart v)))
            ;; bessel_y(v,0), Re(v)#0
            '$infinity)
           ((and (zerop1 ($realpart v))
                 (not (zerop1 v)))
            ;; bessel_y(v,0), Re(v)=0 and v#0
            '$und)
           ;; Call the simplifier of the function.
           (t (simplify (list '(%bessel_y) v z)))))
    ((or (eq z '$inf)
         (eq z '$minf))
     ;; bessel_y(v,inf) or bessel_y(v,minf)
     0)
    (t
     ;; All other cases are handled by the simplifier of the function.
     (simplify (list '(%bessel_y) v z))))))

(defun simp-bessel-y (expr ignored z)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((order (simpcheck (cadr expr) z))
        (arg (simpcheck (caddr expr) z))
        (rat-order nil))
    (cond
      ((zerop1 arg)
       ;; Domain error for a zero argument.
       (simp-domain-error
         (intl:gettext "bessel_y: bessel_y(~:M,~:M) is undefined.") order arg))
      
      ((complex-float-numerical-eval-p order arg)
       ;; We have numeric order and arg and $numer is true, or
       ;; we have either the order or arg being floating-point,
       ;; so let's evaluate it numerically.
       (cond ((= 0 ($imagpart order))
              (let* ((order ($float order))
                     (arg (complex ($float ($realpart arg))
                                   ($float ($imagpart arg))))
                     (result (bessel-y order arg)))
                (add (mul '$%i (imagpart result))
                     (realpart result))))
             (t
              ;; order is complex, arg is real or complex
              (let (($numer t)
                    ($float t)
                    (order ($float order))
                    (arg ($float arg))
                    (dpi (coerce pi 'flonum)))
                ($float
                  ($rectform
                    (add
                      (mul -1.0
                           (power 2.0 order)
                           (inv (power arg order))
                           (take '(%gamma) order)
                           (inv dpi)
                           (take '($hypergeometric)
                                 (list '(mlist))
                                 (list '(mlist) (sub 1.0 order))
                                 (neg (div (mul arg arg) 4.0))))
                     (mul -1.0
                          (inv (power 2.0 order))
                          (power arg order)
                          (take '(%cos) (mul order dpi))
                          (take '(%gamma) (neg order))
                          (inv dpi)
                          (take '($hypergeometric)
                                (list '(mlist))
                                (list '(mlist) (add order 1.0))
                                (neg (div (mul arg arg) 4.0)))))))))))
      
      ((and (integerp order) (minusp order))
       ;; Special case when the order is an integer.
       ;; A&S 9.1.5: Y[-n](x) = (-1)^n*Y[n](x)
       (if (evenp order)
           (take '(%bessel_y) (- order) arg)
           (mul -1 (take '(%bessel_y) (- order) arg))))
      
      ((and $besselexpand 
            (setq rat-order (max-numeric-ratio-p order 2)))
       ;; When order is a fraction with a denominator of 2, we
       ;; can express the result in terms of elementary functions.
       ;;
       ;; Y[1/2](z) = -J[1/2](z) is a function of sin.
       ;; Y[-1/2](z) = -J[-1/2](z) is a function of cos.
       (bessel-y-half-order rat-order arg))
      
      ((and $bessel_reduce
            (and (integerp order)
                 (plusp order)
                 (> order 1)))
       ;; Reduce a bessel function of order > 2 to order 1 and 0:
       ;; bessel_y(v,z) -> 2*(v-1)/z*bessel_y(v-1,z)-bessel_y(v-2,z)
       (sub (mul 2
                 (- order 1)
                 (inv arg) 
                 (take '(%bessel_y) (- order 1) arg))
            (take '(%bessel_y) (- order 2) arg)))
      
      ($hypergeometric_representation
        ;; Return Hypergeometric representation of bessel_y
        (add (mul -1
                  (power 2 order)
                  (inv (power arg order))
                  (take '(%gamma) order)
                  (inv '$%pi)
                  (take '($hypergeometric)
                        (list '(mlist))
                        (list '(mlist) (sub 1 order))
                        (mul -1 (div (mul arg arg) 4))))
             (mul -1
                  (inv (power 2 order))
                  (power arg order)
                  (take '(%cos) (mul order '$%pi))
                  (take '(%gamma) (neg order))
                  (inv '$%pi)
                  (take '($hypergeometric)
                        (list '(mlist))
                        (list '(mlist) (add 1 order))
                        (mul -1 (div (mul arg arg) 4))))))
      
      (t
       (eqtest (list '(%bessel_y) order arg) expr)))))

;; Bessel function of the second kind, Y[n](z), for real or complex z
(defun bessel-y (order arg)
  (cond 
    ((zerop (imagpart arg))
     ;; We have numeric args and the first arg is purely
     ;; real. Call the real-valued Bessel function.
     ;;
     ;; For negative values, use the analytic continuation formula
     ;; A&S 9.1.36:
     ;;
     ;; %y[v](z*exp(m*%pi*%i)) = exp(-v*m*%pi*%i) * %y[v](z)
     ;;       + 2*%i*sin(m*v*%pi)*cot(v*%pi)*%j[v](z)
     ;;
     ;; In particular for m = 1:
     ;; %y[v](-z) = exp(-v*%pi*%i) * %y[v](z) + 2*%i*cos(v*%pi)*%j[v](z)
     (let ((arg (realpart arg)))
       (cond 
         ((zerop order)
          (cond ((>= arg 0)
                 (slatec:dbesy0 (float arg)))
                (t
                 ;; For v = 0, this simplifies to
                 ;; %y[0](-z) = %y[0](z) + 2*%i*%j[0](z)
                 ;; the return value has to be a CL number
                 (+ (slatec:dbesy0 (float (- arg)))
                    (complex 0 (* 2 (slatec:dbesj0 (float (- arg)))))))))
         ((= order 1)
          (cond ((>= arg 0)
                 (slatec:dbesy1 (float arg)))
                (t
                 ;; For v = 1, this simplifies to
                 ;; %y[1](-z) = -%y[1](z) - 2*%i*%j[1](v)
                 ;; the return value has to be a CL number
                 (+ (- (slatec:dbesy1 (float (- arg))))
                    (complex 0 (* -2 (slatec:dbesj1 (float (- arg)))))))))
         ((minusp order)
          (cond ((zerop (nth-value 1 (truncate order)))
                 ;; Order is a negative integeger or float representation.
                 ;; We use Y[-n](z)=(-1)^n*Y[n](z).
                 (if (evenp (floor order))
                     (bessel-y (- order) arg)
                     (- (bessel-y (- order) arg))))
                (t
                 ;; Bessel function of negative order. We use the Hankel 
                 ;; function to compute this, because A&S 9.1.3 says 
                 ;; H1(v,z) = J(v,z) + i * Y(v,z) and H2(v,z) = J(v,z) -i *
                 ;; Y(v,z), we know that Y(v,z) = 0.5/%i * (H1(v,z) - H2(v,z))
                 (let ((result (/ (- (hankel-1 order arg)
                                     (hankel-2 order arg))
                                  (complex 0 2))))
                   (cond ((= (nth-value 1 (floor order)) 1/2)
                          ;; ORDER is half-integral-value or a float
                          ;; representation thereof.
                          (if (minusp arg)
                              ;; arg is negative, the result is purely imaginary
                              (complex 0 (imagpart result))
                              ;; arg is positive, the result is purely real
                              (realpart result)))
                         ;; in all other cases general complex result
                         (t result))))))
         (t
          (multiple-value-bind (n alpha) (floor (float order))
            (let ((jvals (make-array (1+ n) :element-type 'flonum)))
              ;; First we do the calculation for an positive argument.
              (slatec:dbesy (abs (float arg)) alpha (1+ n) jvals)
              
              ;; Now we look at the sign of the argument
              (cond ((>= arg 0)                
                     (aref jvals n))
                    (t
                     (let* ((dpi (coerce pi 'flonum))
                            (s1 (cis (- (* order dpi))))
                            (s2 (* #c(0 2) (cos (* order dpi)))))
                       (let ((result (+ (* s1 (aref jvals n)) 
                                        (* s2 (bessel-j order (- arg))))))
                         (cond ((zerop (nth-value 1 (truncate order)))
                                ;; ORDER is an integer or a float representation
                                ;; of an integer, and the arg is positive the 
                                ;; result is general complex.
                                result)
                               ((= (nth-value 1 (floor order)) 1/2)
                                ;; ORDER is a half-integral-value or an float
                                ;; representation and we have arg < 0. The 
                                ;; result is purely imaginary.
                                (complex 0 (imagpart result)))
                               ;; in all other cases general complex result
                               (t result))))))))))))
    (t
     (cond ((minusp order)
            ;; Bessel function of negative order. We use the Hankel function to
            ;; compute this, because A&S 9.1.3 says H1(v,z)=J(v,z) + i * Y(v,z) 
            ;; and H2(v,z) = J(v,z) -i * Y(v,z), we now that
            ;; Y(v,z) = 1/(2*%i) * (H1(v,z) - H2(v,z))
            (/ (- (hankel-1 order arg) (hankel-2 order arg))
               (complex 0 2)))
           (t
            (multiple-value-bind (n alpha) (floor (float order))
              (let ((cyr (make-array (1+ n) :element-type 'flonum))
                    (cyi (make-array (1+ n) :element-type 'flonum))
                    (cwrkr (make-array (1+ n) :element-type 'flonum))
                    (cwrki (make-array (1+ n) :element-type 'flonum)))
                (multiple-value-bind (v-zr v-zi v-fnu v-kode v-n v-cyr v-cyi 
                                           v-nz v-cwrkr v-cwrki v-ierr)
                   (slatec::zbesy (float (realpart arg))
                                  (float (imagpart arg))
                                  alpha 1 (1+ n) cyr cyi 0 cwrkr cwrki 0)
                  (declare (ignore v-zr v-zi v-fnu v-kode v-n
                                   v-cyr v-cyi v-cwrkr v-cwrki v-nz))
                  
                  ;; We should check for errors based on the value of v-ierr.
                  (when (plusp v-ierr)
                    (format t "zbesy ierr = ~A~%" v-ierr))
                  
                  (complex (aref cyr n) (aref cyi n))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Bessel I function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun $bessel_i (v z)
  (simplify (list '(%bessel_i) v z)))

(defprop $bessel_i %bessel_i alias)
(defprop $bessel_i %bessel_i verb)
(defprop %bessel_i $bessel_i reversealias)
(defprop %bessel_i $bessel_i noun)

(defprop %bessel_i simp-bessel-i operators)

;; Bessel I distributes over lists, matrices, and equations

(defprop %bessel_i (mlist $matrix mequal) distribute_over)

(defprop %bessel_i
    ((n x)
     ;; A&S 9.6.42
     ;;
     ;; I[n](x)*log(x/2) 
     ;;   - (x/2)^n*sum(psi(n+k+1)/gamma(n+k+1)*(z^2/4)^k/k!,k,0,inf)
     ((mplus)
      ((mtimes)
       ((%bessel_i) n x)
       ((%log) ((mtimes) ((rat) 1 2) x)))
      ((mtimes) -1
       ((mexpt) ((mtimes) x ((rat) 1 2)) n)
       ((%sum)
        ((mtimes)
         ((mexpt) ((mfactorial) $%k) -1)
         ((mqapply) (($psi array) 0) ((mplus) 1 $%k n))
         ((mexpt) ((%gamma) ((mplus) 1 $%k n)) -1)
         ((mexpt) ((mtimes) x x ((rat) 1 4)) $%k))
        $%k 0 $inf)))
     ;; Derivative wrt to x.  A&S 9.6.29.
     ((mtimes)
      ((mplus) ((%bessel_i) ((mplus) -1 n) x)
               ((%bessel_i) ((mplus) 1 n) x))
      ((rat) 1 2)))
  grad)

;; Integral of the Bessel I function wrt z
;; http://functions.wolfram.com/Bessel-TypeFunctions/BesselI/21/01/01/
(defun bessel-i-integral-2 (n z)
  (case n
	(0
	 ;; integrate(bessel_i(0,z)
	 ;; = (1/2)*z*(bessel_i(0,z)*(%pi*struve_l(1,z)+2)
	 ;;            -%pi*bessel_i(1,z)*struve_l(0,z))
	 `((mtimes) ((rat) 1 2) ,z
	   ((mplus)
	    ((mtimes) -1 $%pi
	     ((%bessel_i) 1 ,z)
	     ((%struve_l) 0 ,z))
	    ((mtimes)
	     ((%bessel_i) 0 ,z)
	     ((mplus) 2
	      ((mtimes) $%pi ((%struve_l) 1 ,z)))))))
	(1
	 ;; integrate(bessel_i(1,z) = bessel_i(0,z)
	 `((%bessel_i) 0 ,z))
	(otherwise nil)))

(putprop '%bessel_i `((n z) nil ,#'bessel-i-integral-2) 'integral)

;; Support a simplim%bessel_i function to handle specific limits

(defprop %bessel_i simplim%bessel_i simplim%function)

(defun simplim%bessel_i (expr var val)
  ;; Look for the limit of the arguments.
  (let ((v (limit (cadr expr) var val 'think))
        (z (limit (caddr expr) var val 'think)))
  (cond
    ;; Handle an argument 0 at this place.
    ((or (zerop1 z)
         (eq z '$zeroa)
         (eq z '$zerob))
     (let ((sgn ($sign ($realpart v))))
       (cond ((and (eq sgn '$neg)
                   (not (maxima-integerp v)))
              ;; bessel_i(v,0), Re(v)<0 and v not an integer
              '$infinity)
             ((and (eq sgn '$zero)
                   (not (zerop1 v)))
              ;; bessel_i(v,0), Re(v)=0 and v #0
              '$und)
             ;; Call the simplifier of the function.
             (t (simplify (list '(%bessel_i) v z))))))
    ((eq z '$inf)
     ;; bessel_i(v,inf)
     '$inf)
    ((eq z '$minf)
     ;; bessel_i(v,minf)
     '$infinity)
    (t
     ;; All other cases are handled by the simplifier of the function.
     (simplify (list '(%bessel_i) v z))))))

(defun simp-bessel-i (expr ignored z)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((order (simpcheck (cadr expr) z))
        (arg (simpcheck (caddr expr) z))
        (rat-order nil))
    (cond
      ((zerop1 arg)
       ;; We handle the different case for zero arg carefully.
       (let ((sgn ($sign ($realpart order))))
         (cond ((and (eq sgn '$zero)
                     (zerop1 ($imagpart order)))
                ;; bessel_i(0,0) = 1
                (cond ((or ($bfloatp order) ($bfloatp arg)) ($bfloat 1))
                      ((or (floatp order) (floatp arg)) 1.0)
                      (t 1)))
               ((or (eq sgn '$pos)
                    (maxima-integerp order))
                ;; bessel_i(v,0) and Re(v)>0 or v an integer
                (cond ((or ($bfloatp order) ($bfloatp arg)) ($bfloat 0))
                      ((or (floatp order) (floatp arg)) 0.0)
                      (t 0)))
               ((and (eq sgn '$neg)
                     (not (maxima-integerp order)))
                ;; bessel_i(v,0) and Re(v)<0 and v not an integer
                (simp-domain-error
                  (intl:gettext "bessel_i: bessel_i(~:M,~:M) is undefined.")
                  order arg))
               ((and (eq sgn '$zero)
                     (not (zerop1 ($imagpart order))))
                ;; bessel_i(v,0) and Re(v)=0 and v # 0
                (simp-domain-error
                  (intl:gettext "bessel_i: bessel_i(~:M,~:M) is undefined.")
                  order arg))
               (t
                ;; No information about the sign of the order
                (eqtest (list '(%bessel_i) order arg) expr)))))
      
      ((complex-float-numerical-eval-p order arg)
       (cond ((= 0 ($imagpart order))
              (let* ((order ($float order))
                     (arg (complex ($float ($realpart arg))
                                   ($float ($imagpart arg))))
                     (result (bessel-i order arg)))
                (add (mul '$%i (imagpart result))
                     (realpart result))))
             (t
              ;; order is complex, arg is real or complex
              ;; Use 1/gamma(v+1)*(z/2)^v*0F1(;v+1;z^2/4)
              (let (($numer t)
                    ($float t)
                    (order ($float order))
                    (arg ($float arg)))
                ($float
                  ($rectform
                    (mul (inv (take '(%gamma) (add order 1.0)))
                         (power (div arg 2.0) order)
                         (take '($hypergeometric)
                               (list '(mlist))
                               (list '(mlist) (add order 1.0))
                               (div (mul arg arg) 4.0)))))))))
      
      ((and (integerp order) (minusp order))
       ;; Some special cases when the order is an integer
       ;; A&S 9.6.6: I[-n](x) = I[n](x)
       (take '(%bessel_i) (- order) arg))
      
      ((and $besselexpand (setq rat-order (max-numeric-ratio-p order 2)))
       ;; When order is a fraction with a denominator of 2, we
       ;; can express the result in terms of elementary functions.
       ;;
       ;; I[1/2](z) = sqrt(2/%pi/z)*sinh(z)
       ;; I[-1/2](z) = sqrt(2/%pi/z)*cosh(z)
       (bessel-i-half-order rat-order arg))
      
      ((and $bessel_reduce
            (and (integerp order)
                 (plusp order)
                 (> order 1)))
       ;; Reduce a bessel function of order > 2 to order 1 and 0:
       ;; bessel_i(v,z) -> -2*(v-1)/z*bessel_i(v-1,z)+bessel_i(v-2,z)
       (add (mul -2
                 (- order 1)
                 (inv arg) 
                 (take '(%bessel_i) (- order 1) arg))
            (take '(%bessel_i) (- order 2) arg)))

      ((and $%iargs (multiplep arg '$%i))
       ;; bessel_i(v, %i*x) = (%i*x)^v/(x^v) * bessel_j(v, x)
       ;; (From http://functions.wolfram.com/03.02.27.0002.01)
       (let ((x (coeff arg '$%i 1)))
	 (mul (power (mul '$%i x) order)
	      (inv (power x order))
	      (take '(%bessel_j) order x))))
      ($hypergeometric_representation
        ;; Return Hypergeometric representation of bessel_i
        (mul (inv (take '(%gamma) (add order 1)))
             (power (div arg 2) order)
             (take '($hypergeometric)
                   (list '(mlist))
                   (list '(mlist) (add order 1))
                   (div (mul arg arg) 4))))
      
      (t
       (eqtest (list '(%bessel_i) order arg) expr)))))

;; Compute value of Modified Bessel function of the first kind of order n
(defun bessel-i (order arg)
  (cond 
    ((zerop (imagpart arg))
     ;; We have numeric args and the first arg is purely
     ;; real. Call the real-valued Bessel function.  Use special
     ;; routines for order 0 and 1, when possible
     (let ((arg (realpart arg)))
       (cond 
         ((zerop order)
          (slatec:dbesi0 (float arg)))
         ((= order 1)
          (slatec:dbesi1 (float arg)))
         ((or (minusp order) (< arg 0))
          (multiple-value-bind (order-int order-frac) (floor order)
            (cond ((zerop order-frac)
                   ;; order is an integer. We have I[-n](z)=I[n](z) and
                   ;; I[n](-z)=(-1)^n*I[n](z)
                   (if (< arg 0)
                       (if (evenp order-int)
                           (bessel-i (abs order) (abs arg))
                           (- (bessel-i (abs order) (abs arg))))
                       (bessel-i (abs order) arg)))
                  (t
                   ;; Order or arg is negative and order is not an integer, use 
                   ;; the bessel-j function for calculation.  We know from
                   ;; the definition I[v](x) = z^v*%i^(-v)*J[v](%i*x).
                   (let* ((arg (float arg))
                          (result (* (expt arg order)
                                     (expt (complex 0 arg) (- order))
                                     (bessel-j order (complex 0 arg)))))
                     ;; Try to clean up result if we know the result is
                     ;; purely real or purely imaginary.
                     (cond ((>= arg 0)
                            ;; Result is purely real for arg >= 0
                            (realpart result))
                           ((zerop order-frac)
                            ;; Order is an integer or a float representation of
                            ;; an integer, the result is purely real.
                            (realpart result))
                           ((= order-frac 1/2)
                            ;; Order is half-integral-value or a float
                            ;; representation and arg < 0, the result
                            ;; is purely imaginary.
                            (complex 0 (imagpart result)))
                           (t result)))))))
         (t
          ;; Now the case order > 0 and arg >= 0
          (multiple-value-bind (n alpha) (floor (float order))
            (let ((jvals (make-array (1+ n) :element-type 'flonum)))
              (slatec:dbesi (float (realpart arg)) alpha 1 (1+ n) jvals 0)
              (aref jvals n)))))))

    ((and (zerop (realpart arg))
	  (zerop (rem order 1)))
     ;; Handle the case for a pure imaginary arg and integer order.
     ;; In this case, the result is purely real or purely imaginary,
     ;; so we want to make sure that happens.
     ;;
     ;; bessel_i(n, %i*x) = (%i*x)^n/x^n * bessel_j(n,x)
     ;;    = %i*n * bessel_j(n,x)
     (let* ((n (floor order))
	    (result (bessel-j (float n) (imagpart arg))))
       (cond ((evenp n)
	      ;; %i^(2*m) = (-1)^m, where n=2*m
	      (if (evenp (/ n 2))
		  result
		  (- result)))
	     ((oddp n)
	      ;; %i^(2*m+1) = %i*(-1)^m, where n = 2*m+1
	      (if (evenp (floor n 2))
		  (complex 0 result)
		  (complex 0 (- result)))))))

    (t
     ;; The arg is complex.  Use the complex-valued Bessel function.
     (multiple-value-bind (n alpha) (floor (abs (float order)))
       ;; Evaluate the function for positive order and fixup the result later.
       (let ((cyr (make-array (1+ n) :element-type 'flonum))
             (cyi (make-array (1+ n) :element-type 'flonum)))
         (multiple-value-bind (v-zr v-zi v-fnu v-kode v-n
                                    v-cyr v-cyi v-nz v-ierr)
            (slatec::zbesi (float (realpart arg))
                           (float (imagpart arg))
                           alpha 1 (1+ n) cyr cyi 0 0)
           (declare (ignore v-zr v-zi v-fnu v-kode v-n v-cyr v-cyi v-nz))
           ;; We should check for errors here based on the value of v-ierr.
           (when (plusp v-ierr)
             (format t "zbesi ierr = ~A~%" v-ierr))
           
           ;; We have evaluated I(abs(order), arg), now we look at
           ;; the the sign of the order.
           (cond ((minusp order)
                  ;;  I(-a,z) = I(a,z) + (2/pi)*sin(pi*a)*K(a,z)
                  (+ (complex (aref cyr n) (aref cyi n))
                     (let ((dpi (coerce pi 'flonum)))
                       (* (/ 2.0 dpi)
                          (sin (* dpi (- order))) 
                          (bessel-k (- order) arg)))))
                 (t
                  (complex (aref cyr n) (aref cyi n))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Bessel K function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun $bessel_k (v z)
  (simplify (list '(%bessel_k) v z)))

(defprop $bessel_k %bessel_k alias)
(defprop $bessel_k %bessel_k verb)
(defprop %bessel_k $bessel_k reversealias)
(defprop %bessel_k $bessel_k noun)

(defprop %bessel_k simp-bessel-k operators)

;; Bessel K distributes over lists, matrices, and equations

(defprop %bessel_k (mlist $matrix mequal) distribute_over)

(defprop %bessel_k
    ((n x)
     ;; A&S 9.6.43
     ;;
     ;; %pi/2*csc(n*%pi)*['diff(bessel_i(-n,x),n)-'diff(bessel_i(n,x),n)]
     ;;    - %pi*cot(n*%pi)*bessel_k(n,x)
     ((mplus)
      ((mtimes) -1 $%pi
       ((%bessel_k) n x)
       ((%cot) ((mtimes) $%pi n)))
      ((mtimes)
       ((rat) 1 2)
       $%pi
       ((%csc) ((mtimes) $%pi n))
       ((mplus)
        ((%derivative) ((%bessel_i) ((mtimes) -1 n) x) n 1)
        ((mtimes) -1
         ((%derivative) ((%bessel_i) n x) n 1)))))
     ;; Derivative wrt to x.  A&S 9.6.29.
     ((mtimes)
      -1
      ((mplus) ((%bessel_k) ((mplus) -1 n) x)
               ((%bessel_k) ((mplus) 1 n) x))
      ((rat) 1 2)))
  grad)

;; Integral of the Bessel K function wrt z
;; http://functions.wolfram.com/Bessel-TypeFunctions/BesselK/21/01/01/
(defun bessel-k-integral-2 (n z)
  (cond 
   ((and ($integerp n) (<= 0 n))
    (cond
     (($oddp n)
      ;; integrate(bessel_y(2*N+1,z)) , N > 0
      ;; = -(-1)^((n-1)/2)*bessel_k(0,z) 
      ;;   + 2*sum((-1)^(k+(n-1)/2-1)*bessel_k(2*k,z),k,1,(n-1)/2)
      (let* ((k (gensym))
	     (answer `((mplus)
		       ((mtimes) -1 ((%bessel_k) 0 ,z)
			((mexpt) -1
			 ((mtimes) ((rat) 1 2) ((mplus) -1 ,n))))
		       ((mtimes) 2
			((%sum)
			 ((mtimes) ((%bessel_k) ((mtimes) 2 ,k) ,z)
			  ((mexpt) -1
			   ((mplus) -1 ,k
			    ((mtimes) ((rat) 1 2) ((mplus) -1 ,n)))))
			 ,k 1 ((mtimes) ((rat) 1 2) ((mplus) -1 ,n)))))))
	;; Expand out the sum if n < 10.  Otherwise fix up the indices
	(if (< n 10) 
            (meval `(($ev) ,answer $sum))   ; Is there a better way?
	  (simplify ($niceindices answer)))))
     (($evenp n)
      ;; integrate(bessel_k(2*N,z)) , N > 0
      ;; = (1/2)*(-1)^(n/2)*%pi*z*(bessel_k(0,z)*struve_l(-1,z)
      ;;               +bessel_k(1,z)*struve_l(0,z))
      ;;    + 2 * sum((-1)^(k+n/2)*bessel_k(2*k+1,z),k,0,n/2-1)
      (let* 
	  ((k (gensym))
	   (answer `((mplus)
		     ((mtimes) 2
		      ((%sum)
		       ((mtimes)
			((%bessel_k) ((mplus) 1 ((mtimes) 2 ,k)) ,z)
			((mexpt) -1
			 ((mplus) ,k ((mtimes) ((rat) 1 2) ,n))))
		       ,k 0 ((mplus) -1 ((mtimes) ((rat) 1 2) ,n))))
	             ((mtimes)
	              ((rat) 1 2)
	              $%pi
	              ((mexpt) -1 ((mtimes) ((rat) 1 2) ,n))
	              z
		      ((mplus)
		       ((mtimes)
		        ((%bessel_k) 0 ,z)
			((%struve_l) -1 ,z))
		       ((mtimes)
		        ((%bessel_k) 1 ,z)
			((%struve_l) 0 ,z)))))))
	;; expand out the sum if n < 10.  Otherwise fix up the indices
	(if (< n 10) 
            (meval `(($ev) ,answer $sum))  ; Is there a better way?
	  (simplify ($niceindices answer)))))))
   (t nil)))

(putprop '%bessel_k `((n z) nil ,#'bessel-k-integral-2) 'integral)

;; Support a simplim%bessel_k function to handle specific limits

(defprop %bessel_k simplim%bessel_k simplim%function)

(defun simplim%bessel_k (expr var val)
  ;; Look for the limit of the arguments.
  (let ((v (limit (cadr expr) var val 'think))
        (z (limit (caddr expr) var val 'think)))
  (cond
    ;; Handle an argument 0 at this place.
    ((or (zerop1 z)
         (eq z '$zeroa)
         (eq z '$zerob))
     (cond ((zerop1 v)
            ;; bessel_k(0,0)
            '$inf)
           ((integerp v)
            ;; bessel_k(n,0), n an integer
            (cond ((evenp v) '$inf)
                  (t (cond ((eq z '$zeroa) '$inf)
                           ((eq z '$zerob) '$minf)
                           (t '$infinity)))))
           ((not (zerop1 ($realpart v)))
            ;; bessel_k(v,0), Re(v)#0
            '$infinity)
           ((and (zerop1 ($realpart v))
                 (not (zerop1 v)))
            ;; bessel_k(v,0), Re(v)=0 and v#0
            '$und)
           ;; Call the simplifier of the function.
           (t (simplify (list '(%bessel_k) v z)))))
    ((eq z '$inf)
     ;; bessel_k(v,inf)
     0)
    ((eq z '$minf)
     ;; bessel_k(v,minf)
     '$infinity)
    (t
     ;; All other cases are handled by the simplifier of the function.
     (simplify (list '(%bessel_k) v z))))))

(defun simp-bessel-k (expr ignored z)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((order (simpcheck (cadr expr) z))
        (arg (simpcheck (caddr expr) z))
        (rat-order nil))
    (cond
      ((zerop1 arg)
       ;; Domain error for a zero argument.
       (simp-domain-error
         (intl:gettext "bessel_k: bessel_k(~:M,~:M) is undefined.") order arg))
      
      ((complex-float-numerical-eval-p order arg)
       (cond ((= 0 ($imagpart order))
              ;; A&S 9.6.6: K[-v](x) = K[v](x)
              (let* ((order ($float order))
                     (arg (complex ($float ($realpart arg))
                                  ($float ($imagpart arg))))
                     (result (bessel-k order arg)))
                (add (mul '$%i (imagpart result))
                     (realpart result))))
             (t
              ;; order is complex, arg is real or complex
              (let (($numer t)
                    ($float t)
                    (order ($float order))
                    (arg ($float arg)))
                ($float
                  ($rectform
                    (add
                      (mul (power 2.0 (sub order 1.0))
                           (take '(%gamma) order)
                           (inv (power arg order))
                           (take '($hypergeometric)
                                 (list '(mlist))
                                 (list '(mlist) (sub 1.0 order))
                                 (div (mul arg arg) 4.0)))
                     (mul (inv (power 2.0 (add order 1.0)))
                          (power arg order)
                          (take '(%gamma) (neg order))
                          (take '($hypergeometric)
                                (list '(mlist))
                                (list '(mlist) (add order 1.0))
                                (div (mul arg arg) 4.0))))))))))
      
      ((mminusp order)
       ;; A&S 9.6.6: K[-v](x) = K[v](x)
       (take '(%bessel_k) (mul -1 order) arg))
      
      ((and $besselexpand 
            (setq rat-order (max-numeric-ratio-p order 2)))
       ;; When order is a fraction with a denominator of 2, we
       ;; can express the result in terms of elementary
       ;; functions.
       ;;
       ;; K[1/2](z) = sqrt(2/%pi/z)*exp(-z) = K[1/2](z)
       (bessel-k-half-order rat-order arg))
      
      ((and $bessel_reduce
            (and (integerp order)
                 (plusp order)
                 (> order 1)))
       ;; Reduce a bessel function of order > 2 to order 1 and 0:
       ;; bessel_k(v,z) -> 2*(v-1)/z*bessel_k(v-1,z)+bessel_k(v-2,z)
       (add (mul 2
                 (- order 1)
                 (inv arg) 
                 (take '(%bessel_k) (- order 1) arg))
            (take '(%bessel_k) (- order 2) arg)))
      
      ($hypergeometric_representation
        ;; Return Hypergeometric representation of bessel_k
        (add (mul (power 2 (sub order 1))
                  (take '(%gamma) order)
                  (inv (power arg order))
                  (take '($hypergeometric)
                        (list '(mlist))
                        (list '(mlist) (sub 1 order))
                        (div (mul arg arg) 4)))
             (mul (inv (power 2 (add order 1)))
                   (power arg order)
                   (take '(%gamma) (neg order))
                   (take '($hypergeometric)
                         (list '(mlist))
                         (list '(mlist) (add order 1))
                         (div (mul arg arg) 4)))))
      
      (t
       (eqtest (list '(%bessel_k) order arg) expr)))))

;; Compute value of Modified Bessel function of the second kind of order n
(defun bessel-k (order arg)
  (cond 
    ((zerop (imagpart arg))
     ;; We have numeric args and the first arg is purely real. Call the 
     ;; real-valued Bessel function. Handle orders 0 and 1 specially, when 
     ;; possible.
     (let ((arg (realpart arg)))
       (cond 
         ((< arg 0)
          ;; This is the extension for negative arg.
          ;; We use the following formula for evaluation:
          ;; K[v](-z) = exp(-i*pi*v) * K[n][z]-i * pi *I[n](z)
          (let* ((dpi (coerce pi 'flonum))
                 (s1 (cis (* dpi (- (abs order)))))
                 (s2 (* (complex 0 -1) dpi))
                 (result (+ (* s1 (bessel-k (abs order) (- arg)))
                            (* s2 (bessel-i (abs order) (- arg)))))
                 (rem (nth-value 1 (floor order))))
            (cond ((zerop rem)
                   ;; order is an integer or a float representation of an 
                   ;; integer, the result is a general complex
                   result)
                  ((= rem 1/2)
                   ;; order is half-integral-value or an float representation 
                   ;; and arg  < 0, the result is pure imaginary
                   (complex 0 (imagpart result)))
                  ;; in all other cases general complex result
                  (t result))))
         ((= order 0)
          (slatec:dbesk0 (float arg)))
         ((= order 1)
          (slatec:dbesk1 (float arg)))
         (t
          ;; From A&S 9.6.6, K(-v,z) = K(v,z), so take the
          ;; absolute value of the order.
          (multiple-value-bind (n alpha) (floor (abs (float order)))
            (let ((jvals (make-array (1+ n) :element-type 'flonum)))
              (slatec:dbesk (float arg) alpha 1 (1+ n) jvals 0)
              (aref jvals n)))))))
    (t
     ;; The arg is complex.  Use the complex-valued Bessel function. From 
     ;; A&S 9.6.6, K(-v,z) = K(v,z), so take the absolute value of the order.
     (multiple-value-bind (n alpha) (floor (abs (float order)))
       (let ((cyr (make-array (1+ n) :element-type 'flonum))
             (cyi (make-array (1+ n) :element-type 'flonum)))
         (multiple-value-bind (v-zr v-zi v-fnu v-kode v-n
                                    v-cyr v-cyi v-nz v-ierr)
            (slatec::zbesk (float (realpart arg))
                           (float (imagpart arg))
                           alpha 1 (1+ n) cyr cyi 0 0)
           (declare (ignore v-zr v-zi v-fnu v-kode v-n v-cyr v-cyi v-nz))
           
           ;; We should check for errors here based on the value of v-ierr.
           (when (plusp v-ierr)
             (format t "zbesk ierr = ~A~%" v-ierr))
           (complex (aref cyr n) (aref cyi n))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expansion of Bessel J and Y functions of half-integral order.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; From A&S 10.1.1, we have
;;
;; J[n+1/2](z) = sqrt(2*z/pi)*j[n](z)
;; Y[n+1/2](z) = sqrt(2*z/pi)*y[n](z)
;;
;; where j[n](z) is the spherical bessel function of the first kind
;; and y[n](z) is the spherical bessel function of the second kind.
;;
;; A&S 10.1.8 and 10.1.9 give
;;
;; j[n](z) = 1/z*[P(n+1/2,z)*sin(z-n*pi/2) + Q(n+1/2)*cos(z-n*pi/2)]
;;
;; y[n](z) = (-1)^(n+1)*1/z*[P(n+1/2,z)*cos(z+n*pi/2) - Q(n+1/2)*sin(z+n*pi/2)]
;;

;; A&S 10.1.10
;;
;; j[n](z) = f[n](z)*sin(z) + (-1)^n*f[-n-1](z)*cos(z)
;;
;; f[0](z) = 1/z, f[1](z) = 1/z^2
;;
;; f[n-1](z) + f[n+1](z) = (2*n+1)/z*f[n](z)
;;
(defun f-fun (n z)
  (cond ((= n 0)
         (div 1 z))
        ((= n 1)
         (div 1 (mul z z)))
        ((= n -1)
         0)
        ((>= n 2)
         ;; f[n+1](z) = (2*n+1)/z*f[n](z) - f[n-1](z) or
         ;; f[n](z) = (2*n-1)/z*f[n-1](z) - f[n-2](z)
         (sub (mul (div (+ n n -1) z)
                   (f-fun (1- n) z))
              (f-fun (- n 2) z)))
        (t
         ;; Negative n
         ;;
         ;; f[n-1](z) = (2*n+1)/z*f[n](z) - f[n+1](z) or
         ;; f[n](z) = (2*n+3)/z*f[n+1](z) - f[n+2](z)
         (sub (mul (div (+ n n 3) z)
                   (f-fun (1+ n) z))
              (f-fun (+ n 2) z)))))

;; Compute sqrt(2*z/%pi)
(defun root-2z/pi (z)
  (power (mul 2 z (inv '$%pi)) '((rat simp) 1 2)))

(defun bessel-j-half-order (order arg)
  "Compute J[n+1/2](z)"
  (let* ((n (floor order))
         (sign (if (oddp n) -1 1))
         (jn (sub (mul ($expand (f-fun n arg))
                       (take '(%sin) arg))
                  (mul sign
                       ($expand (f-fun (- (- n) 1) arg))
                       (take '(%cos) arg)))))
    (mul (root-2z/pi arg)
         jn)))

(defun bessel-y-half-order (order arg)
  "Compute Y[n+1/2](z)"
  ;; A&S 10.1.1:
  ;; Y[n+1/2](z) = sqrt(2*z/%pi)*y[n](z)
  ;;
  ;; A&S 10.1.15:
  ;; y[n](z) = (-1)^(n+1)*j[-n-1](z)
  ;;
  ;; So
  ;; Y[n+1/2](z) = sqrt(2*z/%pi)*(-1)^(n+1)*j[-n-1](z)
  ;;             = (-1)^(n+1)*sqrt(2*z/%pi)*j[-n-1](z)
  ;;             = (-1)^(n+1)*J[-n-1/2](z)
  (let* ((n (floor order))
         (jn (bessel-j-half-order (- (- order) 1/2) arg)))
    (if (evenp n)
        (mul -1 jn)
        jn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Expansion of Bessel I and K functions of half-integral order.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See A&S 10.2.12
;;
;; I[n+1/2](z) = sqrt(2*z/%pi)*[g[n](z)*sinh(z) + g[n-1](z)*cosh(z)]
;;
;; g[0](z) = 1/z, g[1](z) = -1/z^2
;;
;; g[n-1](z) - g[n+1](z) = (2*n+1)/z*g[n](z)
;;
;;
(defun g-fun (n z)
  (declare (type integer n))
  (cond ((= n 0)
         (div 1 z))
        ((= n 1)
         (div -1 (mul z z)))
        ((>= n 2)
         ;; g[n](z) = g[n-2](z) - (2*n-1)/z*g[n-1](z)
         (sub (g-fun (- n 2) z)
              (mul (div (+ n n -1) z)
                   (g-fun (- n 1) z))))
        (t
         ;; n is negative
         ;;
         ;; g[n](z) = (2*n+3)/z*g[n+1](z) + g[n+2](z)
         (add (mul (div (+ n n 3) z)
                   (g-fun (+ n 1) z))
              (g-fun (+ n 2) z)))))

;; See A&S 10.2.12
;;
;; I[n+1/2](z) = sqrt(2*z/%pi)*[g[n](z)*sinh(z) + g[-n-1](z)*cosh(z)]

(defun bessel-i-half-order (order arg)
  (let ((order (floor order)))
    (mul (root-2z/pi arg)
         (add (mul ($expand (g-fun order arg))
                   (take '(%sinh) arg))
              (mul ($expand (g-fun (- (+ order 1)) arg))
                   (take '(%cosh) arg))))))

;; See A&S 10.2.15
;;
;; sqrt(%pi/2/z)*K[n+1/2](z) = (%pi/2/z)*exp(-z)*sum (n+1/2,k)/(2*z)^k
;;
;; or
;;                                            n
;; K[n+1/2](z) = sqrt(%pi/2)/sqrt(z)*exp(-z) sum (n+1/2,k)/(2*z)^k
;;                                           k=0
;;
;; where (A&S 10.1.9)
;;
;; (n+1/2,k) = (n+k)!/k!/(n-k)!

(defun k-fun (n z)
  (declare (type unsigned-byte n))
  ;; Computes the sum above
  (let ((sum 1)
        (term 1))
    (loop for k from 0 upto n do
          (setf term (mul term
                          (div (div (* (- n k) (+ n k 1))
                                    (+ k 1))
                               (mul 2 z))))
          (setf sum (add sum term)))
    sum))

(defun bessel-k-half-order (order arg)
  (let ((order (truncate (abs order))))
    (mul (mul (power (div '$%pi 2) '((rat simp) 1 2))
              (power arg '((rat simp) -1 2))
              (power '$%e (neg arg)))
         (k-fun (abs order) arg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Realpart und imagpart of Bessel functions
;;;
;;; We handle the special cases when we know that the Bessel function
;;; is pure real or pure imaginary. In all other cases Maxima generates 
;;; a general noun form as result.
;;;
;;; To get the complex sign of the order an argument of the Bessel function
;;; the function $csign is used which calls $sign in a complex mode.
;;;
;;; This is an extension of of the algorithm of the function risplit in 
;;; the file rpart.lisp. risplit looks for a risplit-function on the 
;;; property list and call it if available.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *debug-bessel* nil)

;;; Put the risplit-function for Bessel J and Bessel I on the property list

(defprop %bessel_j risplit-bessel-j-or-i risplit-function)
(defprop %bessel_i risplit-bessel-j-or-i risplit-function)

;;; realpart und imagpart for Bessel J and Bessel I function

(defun risplit-bessel-j-or-i (expr)
  (when *debug-bessel*
    (format t "~&RISPLIT-BESSEL-J with ~A~%" expr))
  (let ((order (cadr  expr))
        (arg   (caddr expr))
        (sign-order ($csign (cadr  expr)))
        (sign-arg   ($csign (caddr expr))))
    
    (when *debug-bessel*
      (format t "~&   : order      = ~A~%" order)
      (format t "~&   : arg        = ~A~%" arg)
      (format t "~&   : sign-order = ~A~%" sign-order)
      (format t "~&   : sign-arg   = ~A~%" sign-arg))
    
    (cond
      ((or (member sign-order '($complex $imaginary))
           (eq sign-arg '$complex))
       ;; order or arg are complex, return general noun-form
       (risplit-noun expr))
      ((eq sign-arg '$imaginary)
       ;; arg is pure imaginary
       (cond 
         ((or ($oddp  order)
              ($featurep order '$odd)) 
          ;; order is an odd integer, pure imaginary noun-form
          (cons 0 (mul -1 '$%i expr)))
         ((or ($evenp order)
              ($featurep order '$even))
           ;; order is an even integer, real noun-form
           (cons expr 0))
         (t
          ;; order is not an odd or even integer, or Maxima can not
          ;; determine it, return general noun-form 
          (risplit-noun expr))))
      ;; At this point order and arg are real.
      ;; We have to look for some special cases involing a negative arg
      ((or (maxima-integerp order)
           (member sign-arg '($pos $pz)))
       ;; arg is positive or order an integer, real noun-form
       (cons expr 0))
      ;; At this point we know that arg is negative or the sign is not known
      ((zerop1 (sub ($truncate ($multthru 2 order)) ($multthru 2 order)))
       ;; order is half integral
       (cond
          ((eq sign-arg '$neg)
           ;; order is half integral and arg negative, imaginary noun-form
           (cons 0 (mul -1 '$%i expr)))
          (t
            ;; the sign of arg or order is not known
            (risplit-noun expr))))
      (t
        ;; the sign of arg or order is not known
        (risplit-noun expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Put the risplit-function for Bessel K and Bessel Y on the property list

(defprop %bessel_k risplit-bessel-k-or-y risplit-function)
(defprop %bessel_y risplit-bessel-k-or-y risplit-function)

;;; realpart und imagpart for Bessel K and Bessel Y function

(defun risplit-bessel-k-or-y (expr)
  (when *debug-bessel*
    (format t "~&RISPLIT-BESSEL-K with ~A~%" expr))
  (let ((order (cadr  expr))
        (arg   (caddr expr))
        (sign-order ($csign (cadr  expr)))
        (sign-arg   ($csign (caddr expr))))
    
    (when *debug-bessel*
      (format t "~&   : order      = ~A~%" order)
      (format t "~&   : arg        = ~A~%" arg)
      (format t "~&   : sign-order = ~A~%" sign-order)
      (format t "~&   : sign-arg   = ~A~%" sign-arg))
    
    (cond
      ((or (member sign-order '($complex $imaginary))
           (member sign-arg '($complex '$imaginary)))
       (risplit-noun expr))
      ;; At this point order and arg are real valued.
      ;; We have to look for some special cases involing a negative arg
      ((member sign-arg '($pos $pz))
       ;; arg is positive
       (cons expr 0))
      ;; At this point we know that arg is negative or the sign is not known
      ((and (not (maxima-integerp order))
            (zerop1 (sub ($truncate ($multthru 2 order)) ($multthru 2 order))))
       ;; order is half integral
       (cond
          ((eq sign-arg '$neg)
           (cons 0 (mul -1 '$%i expr)))
          (t
            ;; the sign of arg is not known
            (risplit-noun expr))))
      (t
        ;; the sign of arg is not known
        (risplit-noun expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of scaled Bessel I functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: The following scaled functions need work.  They should be
;; extended to match bessel_i, but still carefully compute the scaled
;; value.

;; I think g0(x) = exp(-x)*I[0](x), g1(x) = exp(-x)*I[1](x), and
;; gn(x,n) = exp(-x)*I[n](x), based on some simple numerical
;; evaluations.

(defun $scaled_bessel_i0 ($x)
  (cond ((mnump $x)
	 ;; XXX Should we return noun forms if $x is rational?
	 (slatec:dbsi0e ($float $x)))
	(t
	 (mul (power '$%e (neg (simplifya `((mabs) ,$x) nil)))
	      `((%bessel_i) 0 ,$x)))))

(defun $scaled_bessel_i1 ($x)
  (cond ((mnump $x)
	 ;; XXX Should we return noun forms if $x is rational?
	 (slatec:dbsi1e ($float $x)))
	(t
	 (mul (power '$%e (neg (simplifya `((mabs) ,$x) nil)))
	      `((%bessel_i) 1 ,$x)))))

(defun $scaled_bessel_i ($n $x)
  (cond ((and (mnump $x) (mnump $n))
	 ;; XXX Should we return noun forms if $n and $x are rational?
	 (multiple-value-bind (n alpha) (floor ($float $n))
	   (let ((iarray (make-array (1+ n) :element-type 'flonum)))
	   (slatec:dbesi ($float $x) alpha 2 (1+ n) iarray 0)
	   (aref iarray n))))
	(t
	 (mul (power '$%e (neg (simplifya `((mabs) ,$x) nil)))
	      ($bessel_i $n $x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Hankel 1 function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $hankel_1 (v z)
  (simplify (list '(%hankel_1) v z)))

(defprop $hankel_1 %hankel_1 alias)
(defprop $hankel_1 %hankel_1 verb)
(defprop %hankel_1 $hankel_1 reversealias)
(defprop %hankel_1 $hankel_1 noun)

;; hankel_1 distributes over lists, matrices, and equations

(defprop %hankel_1 (mlist $matrix mequal) distribute_over)

(defprop %hankel_1 simp-hankel-1 operators)

(defprop %hankel_1
    ((n x)
     nil
     ((mtimes)
       ((mplus)
         ((%hankel_1) ((mplus) -1 n) x)
         ((mtimes) -1 ((%hankel_1) ((mplus) 1 n) x)))
       ((rat) 1 2)))
    grad)

(defun simp-hankel-1 (expr ignored z)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((order (simpcheck (cadr expr) z))
	(arg   (simpcheck (caddr expr) z))
        rat-order)
    (cond
      ((zerop1 arg)
       (simp-domain-error
         (intl:gettext "hankel_1: hankel_1(~:M,~:M) is undefined.")
         order arg))
      ((and (complex-float-numerical-eval-p order arg)
            (mnump order))
       (let* ((order ($float order))
              (arg (complex ($float ($realpart arg))
                            ($float ($imagpart arg))))
              (result (hankel-1 order arg)))
         (add (mul '$%i (imagpart result)) (realpart result))))
      ((and $besselexpand
            (setq rat-order (max-numeric-ratio-p order 2)))
       ;; When order is a fraction with a denominator of 2, we can express 
       ;; the result in terms of elementary functions.
       ;; Use the defintion hankel_1(v,z) = bessel_j(v,z)+%i*bessel_y(v,z)
       (sratsimp
         (add (bessel-j-half-order rat-order arg)
              (mul '$%i
                   (bessel-y-half-order rat-order arg)))))
      (t (eqtest (list '(%hankel_1) order arg) expr)))))

;; Numerically compute H1(v, z).
;;
;; A&S 9.1.3 says H1(v,z) = J(v,z) + i * Y(v,z)
;;
(defun hankel-1 (v z)
  (let ((v (float v))
	(z (coerce z '(complex flonum))))
    (cond ((minusp v)
	   ;; A&S 9.1.6:
	   ;;
	   ;; H1(-v,z) = exp(v*pi*i)*H1(v,z)
	   ;;
	   ;; or
	   ;;
	   ;; H1(v,z) = exp(-v*pi*i)*H1(-v,z)
	   
	   (* (cis (* (float pi) (- v))) (hankel-1 (- v) z)))
	  (t
	   (multiple-value-bind (n fnu) (floor v)
	     (let ((zr (realpart z))
	           (zi (imagpart z))
	           (cyr (make-array (1+ n) :element-type 'flonum))
	           (cyi (make-array (1+ n) :element-type 'flonum)))
	       (multiple-value-bind (dzr dzi df dk dm dn dcyr dcyi nz ierr)
	          (slatec::zbesh zr zi fnu 1 1 (1+ n) cyr cyi 0 0)
	         (declare (ignore dzr dzi df dk dm dn dcyr dcyi nz ierr))
	         (complex (aref cyr n) (aref cyi n)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of the Hankel 2 function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $hankel_2 (v z)
  (simplify (list '(%hankel_2) v z)))

(defprop $hankel_2 %hankel_2 alias)
(defprop $hankel_2 %hankel_2 verb)
(defprop %hankel_2 $hankel_2 reversealias)
(defprop %hankel_2 $hankel_2 noun)

;; hankel_2 distributes over lists, matrices, and equations

(defprop %hankel_2 (mlist $matrix mequal) distribute_over)

(defprop %hankel_2 simp-hankel-2 operators)

(defprop %hankel_2
    ((n x)
     nil
     ((mtimes)
       ((mplus) 
         ((%hankel_2) ((mplus) -1 n) x)
         ((mtimes) -1 ((%hankel_2) ((mplus) 1 n) x)))
       ((rat) 1 2)))
    grad)

(defun simp-hankel-2 (expr ignored z)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((order (simpcheck (cadr expr) z))
	(arg   (simpcheck (caddr expr) z))
        rat-order)
    (cond
      ((zerop1 arg)
       (simp-domain-error
         (intl:gettext "hankel_2: hankel_2(~:M,~:M) is undefined.")
         order arg))
      ((and (complex-float-numerical-eval-p order arg)
            (mnump order))
       (let* ((order ($float order))
              (arg (complex ($float ($realpart arg))
                            ($float ($imagpart arg))))
              (result (hankel-2 order arg)))
         (add (mul '$%i (imagpart result)) (realpart result))))
      ((and $besselexpand
            (setq rat-order (max-numeric-ratio-p order 2)))
       ;; When order is a fraction with a denominator of 2, we can express 
       ;; the result in terms of elementary functions.
       ;; Use the defintion hankel_1(v,z) = bessel_j(v,z)-%i*bessel_y(v,z)
       (sratsimp
         (sub (bessel-j-half-order rat-order arg)
              (mul '$%i
                   (bessel-y-half-order rat-order arg)))))
      (t (eqtest (list '(%hankel_2) order arg) expr)))))

;; Numerically compute H2(v, z).
;;
;; A&S 9.1.4 says H2(v,z) = J(v,z) - i * Y(v,z)
;;
(defun hankel-2 (v z)
  (let ((v (float v))
	(z (coerce z '(complex flonum))))
    (cond ((minusp v)
	   ;; A&S 9.1.6:
	   ;;
	   ;; H2(-v,z) = exp(-v*pi*i)*H1(v,z)
	   ;;
	   ;; or
	   ;;
	   ;; H2(v,z) = exp(v*pi*i)*H1(-v,z)
	   
	   (* (cis (* (float pi) v)) (hankel-2 (- v) z)))
	  (t
	   (multiple-value-bind (n fnu) (floor v)
	     (let ((zr (realpart z))
	           (zi (imagpart z))
	           (cyr (make-array (1+ n) :element-type 'flonum))
	           (cyi (make-array (1+ n) :element-type 'flonum)))
	       (multiple-value-bind (dzr dzi df dk dm dn dcyr dcyi nz ierr)
	          (slatec::zbesh zr zi fnu 1 2 (1+ n) cyr cyi 0 0)
	         (declare (ignore dzr dzi df dk dm dn dcyr dcyi nz ierr))
	         (complex (aref cyr n) (aref cyi n)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of Struve H function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $struve_h (v z)
  (simplify (list '(%struve_h) v z)))

(defprop $struve_h %struve_h alias)
(defprop $struve_h %struve_h verb)
(defprop %struve_h $struve_h reversealias)
(defprop %struve_h $struve_h noun)

;; struve_h distributes over lists, matrices, and equations

(defprop %struve_h (mlist $matrix mequal) distribute_over)

(defprop %struve_h simp-struve-h operators)

(defprop %struve_h
  ((v z)
   nil
   ((mtimes) 
    ((rat) 1 2)
    ((mplus) 
     ((%struve_h) ((mplus) -1 v) z)
     ((mtimes) -1 ((%struve_h) ((mplus) 1 v) z))
     ((mtimes) 
      ((mexpt) $%pi ((rat) -1 2))
      ((mexpt) 2 ((mtimes) -1 v))
      ((mexpt) ((%gamma) ((mplus) ((rat) 3 2) v)) -1)
      ((mexpt) z v)))))
  grad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-struve-h (expr ignored z)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((order (simpcheck (cadr expr) z))
        (arg   (simpcheck (caddr expr) z)))
    (cond
      
      ;; Check for special values
      
      ((zerop1 arg)
       (cond ((eq ($csign (add order 1)) '$zero)
              (cond ((or ($bfloatp order)
                         ($bfloatp arg))
                     ($bfloat (div 2 '$%pi)))
                    ((or (floatp order)
                         (floatp arg))
                     ($float (div 2 '$%pi)))
                    (t
                     (div 2 '$%pi))))
             ((eq ($sign (add ($realpart order) 1)) '$pos)
              arg)
             ((member ($sign (add ($realpart order) 1)) '($zero $neg $nz))
              (simp-domain-error 
                (intl:gettext "struve_h: struve_h(~:M,~:M) is undefined.")
                order arg))
             (t
              (eqtest (list '(%struve_h) order arg) expr))))
      
      ;; Check for numerical evaluation
      
      ((complex-float-numerical-eval-p order arg)
       (let (($numer t) ($float t))
         ($rectform
           ($float
             (mul
               ($rectform (power arg (add order 1.0)))
               ($rectform (inv (power 2.0 order)))
               (inv (power ($float '$%pi) 0.5))
               (inv (simplify (list '(%gamma) (add order 1.5))))
               (simplify (list '($hypergeometric)
                               (list '(mlist) 1)
                               (list '(mlist) '((rat simp) 3 2)
                                     (add order '((rat simp) 3 2)))
                               (div (mul arg arg) -4.0))))))))
      
      ((complex-bigfloat-numerical-eval-p order arg)
       (let (($ratprint nil)
             (arg ($bfloat arg))
             (order ($bfloat order)))
         ($rectform
           ($bfloat
             (mul
               ($rectform (power arg (add order 1)))
               ($rectform (inv (power 2 order)))
               (inv (power ($bfloat '$%pi) ($bfloat '((rat simp) 1 2))))
               (inv (simplify (list '(%gamma) 
                                    (add order ($bfloat '((rat simp) 3 2))))))
               (simplify (list '($hypergeometric)
                               (list '(mlist) 1)
                               (list '(mlist) '((rat simp) 3 2)
                                     (add order '((rat simp) 3 2)))
                               (div (mul arg arg) ($bfloat -4)))))))))
      
      ;; Transformations and argument simplifications
      
      ((and $besselexpand
            (ratnump order)
            (integerp (mul 2 order)))
       (cond 
         ((eq ($sign order) '$pos)
          ;; Expansion of Struve H for a positive half integral order.
          (sratsimp
            (add
              (mul
                (inv (simplify (list '(mfactorial) (sub order 
                                                        '((rat simp) 1 2)))))
                (inv (power '$%pi '((rat simp) 1 2 )))
                (power (div arg 2) (add order -1))
                (let ((index (gensumindex)))
                  (dosum
                    (mul
                      (simplify (list '($pochhammer) '((rat simp) 1 2) index))
                      (simplify (list '($pochhammer)
                                      (sub '((rat simp) 1 2) order)
                                      index))
                      (power (mul -1 arg arg (inv 4)) (mul -1 index)))
                    index 0 (sub order '((rat simp) 1 2)) t)))
              (mul
                (power (div 2 '$%pi) '((rat simp) 1 2))
                (power -1 (add order '((rat simp) 1 2)))
                (inv (power arg '((rat simp) 1 2)))
                (add
                  (mul
                    (simplify 
                      (list '(%sin)
                            (add (mul '((rat simp) 1 2)
                                      '$%pi
                                      (add order '((rat simp) 1 2)))
                                 arg)))
                    (let ((index (gensumindex)))
                      (dosum
                        (mul
                          (power -1 index)
                          (simplify (list '(mfactorial) 
                                          (add (mul 2 index) 
                                               order 
                                               '((rat simp) -1 2))))
                          (inv (simplify (list '(mfactorial) (mul 2 index))))
                          (inv (simplify (list '(mfactorial)
                                               (add (mul -2 index)
                                                    order
                                                    '((rat simp) -1 2)))))
                          (inv (power (mul 2 arg) (mul 2 index))))
                        index 0 
                        (simplify (list '($floor) 
                                        (div (sub (mul 2 order) 1) 4)))
                        t)))
                  (mul
                    (simplify (list '(%cos)
                                    (add (mul '((rat simp) 1 2)
                                              '$%pi
                                              (add order '((rat simp) 1 2)))
                                         arg)))
                    (let ((index (gensumindex)))
                      (dosum
                        (mul
                          (power -1 index)
                          (simplify (list '(mfactorial) 
                                          (add (mul 2 index) 
                                               order 
                                               '((rat simp) 1 2))))
                          (power (mul 2 arg) (mul -1 (add (mul 2 index) 1)))
                          (inv (simplify (list '(mfactorial) 
                                               (add (mul 2 index) 1))))
                          (inv (simplify (list '(mfactorial)
                                               (add (mul -2 index)
                                                    order
                                                    '((rat simp) -3 2))))))
                        index 0 
                        (simplify (list '($floor) 
                                        (div (sub (mul 2 order) 3) 4)))
                        t))))))))
         
         ((eq ($sign order) '$neg)
          ;; Expansion of Struve H for a negative half integral order.
          (sratsimp
            (add
              (mul
                (power (div 2 '$%pi) '((rat simp) 1 2))
                (power -1 (add order '((rat simp) 1 2)))
                (inv (power arg '((rat simp) 1 2)))
                (add
                  (mul
                    (simplify (list '(%sin)
                                    (add
                                      (mul
                                        '((rat simp) 1 2)
                                        '$%pi
                                        (add order '((rat simp) 1 2)))
                                      arg)))
                    (let ((index (gensumindex)))
                      (dosum
                        (mul
                          (power -1 index)
                          (simplify (list '(mfactorial)
                                          (add (mul 2 index) 
                                               (neg order) 
                                               '((rat simp) -1 2))))
                          (inv (simplify (list '(mfactorial) (mul 2 index))))
                          (inv (simplify (list '(mfactorial)
                                               (add (mul -2 index)
                                                    (neg order)
                                                    '((rat simp) -1 2)))))
                          (inv (power (mul 2 arg) (mul 2 index))))
                        index 0
                        (simplify (list '($floor) 
                                        (div (add (mul 2 order) 1) -4)))
                        t)))
                  (mul
                    (simplify (list '(%cos)
                                    (add
                                      (mul
                                        '((rat simp) 1 2)
                                        '$%pi
                                        (add order '((rat simp) 1 2)))
                                      arg)))
                    (let ((index (gensumindex)))
                      (dosum
                        (mul
                          (power -1 index)
                          (simplify (list '(mfactorial) 
                                          (add (mul 2 index)
                                               (neg order) 
                                               '((rat simp) 1 2))))
                          (power (mul 2 arg) (mul -1 (add (mul 2 index) 1)))
                          (inv (simplify (list '(mfactorial) 
                                               (add (mul 2 index) 1))))
                          (inv (simplify (list '(mfactorial)
                                               (add (mul -2 index)
                                                    (neg order)
                                                    '((rat simp) -3 2))))))
                        index 0 
                        (simplify (list '($floor) 
                                        (div (add (mul 2 order) 3) -4)))
                        t))))))))))
      (t 
       (eqtest (list '(%struve_h) order arg) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of Struve L function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $struve_l (v z)
  (simplify (list '(%struve_l) v z)))

(defprop $struve_l %struve_l alias)
(defprop $struve_l %struve_l verb)
(defprop %struve_l $struve_l reversealias)
(defprop %struve_l $struve_l noun)

(defprop %struve_l simp-struve-l operators)

;; struve_l distributes over lists, matrices, and equations

(defprop %struve_l (mlist $matrix mequal) distribute_over)

(defprop %struve_l
  ((v z)
   nil
   ((mtimes) 
    ((rat) 1 2)
    ((mplus) 
     ((%struve_l) ((mplus) -1 v) z)
     ((%struve_l) ((mplus) 1 v) z)
     ((mtimes) 
      ((mexpt) $%pi ((rat) -1 2))
      ((mexpt) 2 ((mtimes) -1 v))
      ((mexpt) ((%gamma) ((mplus) ((rat) 3 2) v)) -1)
      ((mexpt) z v)))))
  grad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-struve-l (expr ignored z)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((order (simpcheck (cadr expr) z))
        (arg   (simpcheck (caddr expr) z)))
    (cond
      
      ;; Check for special values
      
      ((zerop1 arg)
       (cond ((eq ($csign (add order 1)) '$zero)
              (cond ((or ($bfloatp order)
                         ($bfloatp arg))
                     ($bfloat (div 2 '$%pi)))
                    ((or (floatp order)
                         (floatp arg))
                     ($float (div 2 '$%pi)))
                    (t
                     (div 2 '$%pi))))
             ((eq ($sign (add ($realpart order) 1)) '$pos)
              arg)
             ((member ($sign (add ($realpart order) 1)) '($zero $neg $nz))
              (simp-domain-error 
                (intl:gettext "struve_l: struve_l(~:M,~:M) is undefined.")
                order arg))
             (t
              (eqtest (list '(%struve_l) order arg) expr))))
      
      ;; Check for numerical evaluation
      
      ((complex-float-numerical-eval-p order arg)
       (let (($numer t) ($float t))
         ($rectform
           ($float
             (mul
               ($rectform (power arg (add order 1.0)))
               ($rectform (inv (power 2.0 order)))
               (inv (power ($float '$%pi) 0.5))
               (inv (simplify (list '(%gamma) (add order 1.5))))
               (simplify (list '($hypergeometric)
                               (list '(mlist) 1)
                               (list '(mlist) '((rat simp) 3 2) 
                                     (add order '((rat simp) 3 2)))
                               (div (mul arg arg) 4.0))))))))
      
      ((complex-bigfloat-numerical-eval-p order arg)
       (let (($ratprint nil))
         ($rectform
           ($bfloat
             (mul
               ($rectform (power arg (add order 1)))
               ($rectform (inv (power 2 order)))
               (inv (power ($bfloat '$%pi) ($bfloat '((rat simp) 1 2))))
               (inv (simplify (list '(%gamma) (add order '((rat simp) 3 2)))))
               (simplify (list '($hypergeometric)
                               (list '(mlist) 1)
                               (list '(mlist) '((rat simp) 3 2) 
                                     (add order '((rat simp) 3 2)))
                               (div (mul arg arg) 4))))))))
      
      ;; Transformations and argument simplifications
      
      ((and $besselexpand
            (ratnump order)
            (integerp (mul 2 order)))
       (cond 
         ((eq ($sign order) '$pos)
          ;; Expansion of Struve L for a positive half integral order.
          (sratsimp
            (add
              (mul -1
                (power 2 (sub 1 order))
                (power arg (sub order 1))
                (inv (power '$%pi '((rat simp) 1 2)))
                (inv (simplify (list '(mfactorial) (sub order 
                                                        '((rat simp) 1 2)))))
                (let ((index (gensumindex)))
                  (dosum
                    (mul
                      (simplify (list '($pochhammer) '((rat simp) 1 2) index))
                      (simplify (list '($pochhammer) 
                                      (sub '((rat simp) 1 2) order) 
                                      index))
                      (power (mul arg arg (inv 4)) (mul -1 index)))
                    index 0 (sub order '((rat simp) 1 2)) t)))
              (mul -1
                (power (div 2 '$%pi) '((rat simp) 1 2))
                (inv (power arg '((rat simp) 1 2)))
                ($exp (div (mul '$%pi '$%i (add order '((rat simp) 1 2))) 2))
                (add
                  (mul
                    (let (($trigexpand t)) 
                      (simplify (list '(%sinh) 
                                      (sub (mul '((rat simp) 1 2) 
                                                '$%pi 
                                                '$%i 
                                                (add order '((rat simp) 1 2))) 
                                           arg))))
                    (let ((index (gensumindex)))
                      (dosum
                        (mul
                          (simplify (list '(mfactorial)
                                          (add (mul 2 index)
                                               (simplify (list '(mabs) order))
                                               '((rat simp) -1 2))))
                          (inv (simplify (list '(mfactorial) (mul 2 index))))
                          (inv (simplify (list '(mfactorial) 
                                               (add (simplify (list '(mabs) 
                                                                    order))
                                                    (mul -2 index)
                                                    '((rat simp) -1 2)))))
                          (inv (power (mul 2 arg) (mul 2 index))))
                        index 0
                        (simplify (list '($floor)
                                        (div (sub (mul 2 
                                                       (simplify (list '(mabs) 
                                                                       order))) 
                                                  1) 
                                             4)))
                        t)))
                  (mul
                    (let (($trigexpand t)) 
                      (simplify (list '(%cosh) 
                                      (sub (mul '((rat simp) 1 2) 
                                                '$%pi 
                                                '$%i 
                                                (add order '((rat simp) 1 2))) 
                                           arg))))
                    (let ((index (gensumindex)))
                      (dosum
                        (mul
                          (simplify (list '(mfactorial) 
                                          (add (mul 2 index)
                                               (simplify (list '(mabs) order))
                                               '((rat simp) 1 2))))
                          (power (mul 2 arg) (neg (add (mul 2 index) 1)))
                          (inv (simplify (list '(mfactorial)
                                               (add (mul 2 index) 1))))
                          (inv (simplify (list '(mfactorial)
                                               (add (simplify (list '(mabs) 
                                                                    order))
                                                    (mul -2 index)
                                                    '((rat simp) -3 2))))))                    
                        index 0
                        (simplify (list '($floor) 
                                        (div (sub (mul 2 
                                                       (simplify (list '(mabs) 
                                                                       order)))
                                                       3)
                                             4)))
                        t))))))))
         ((eq ($sign order) '$neg)
          ;; Expansion of Struve L for a negative half integral order.
          (sratsimp
            (add
              (mul -1
                (power (div 2 '$%pi) '((rat simp) 1 2))
                (inv (power arg '((rat simp) 1 2)))
                ($exp (div (mul '$%pi '$%i (add order '((rat simp) 1 2))) 2))
                (add
                  (mul
                    (let (($trigexpand t)) 
                      (simplify (list '(%sinh) 
                                      (sub (mul '((rat simp) 1 2) 
                                                '$%pi 
                                                '$%i 
                                                (add order '((rat simp) 1 2))) 
                                           arg))))
                    (let ((index (gensumindex)))
                      (dosum
                        (mul
                          (simplify (list '(mfactorial)
                                          (add (mul 2 index)
                                               (neg order)
                                               '((rat simp) -1 2))))
                          (inv (simplify (list '(mfactorial) (mul 2 index))))
                          (inv (simplify (list '(mfactorial) 
                                               (add (neg order)
                                                    (mul -2 index)
                                                    '((rat simp) -1 2)))))
                          (inv (power (mul 2 arg) (mul 2 index))))
                        index 0
                        (simplify (list '($floor)
                                        (div (add (mul 2 order) 1) -4)))
                        t)))
                  (mul
                    (let (($trigexpand t)) 
                      (simplify (list '(%cosh) 
                                      (sub (mul '((rat simp) 1 2) 
                                                '$%pi 
                                                '$%i 
                                                (add order '((rat simp) 1 2))) 
                                           arg))))
                    (let ((index (gensumindex)))
                      (dosum
                        (mul
                          (simplify (list '(mfactorial)
                                          (add (mul 2 index)
                                               (neg order)
                                               '((rat simp) 1 2))))
                          (power (mul 2 arg) (neg (add (mul 2 index) 1)))
                          (inv (simplify (list '(mfactorial)
                                               (add (mul 2 index) 1))))
                          (inv (simplify (list '(mfactorial)
                                               (add (neg order)
                                                    (mul -2 index)
                                                    '((rat simp) -3 2))))))                    
                        index 0
                        (simplify (list '($floor) 
                                        (div (add (mul 2 order) 3) -4)))
                   t))))))))))
      (t 
       (eqtest (list '(%struve_l) order arg) expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmspec $gauss (form)
  (format t
"NOTE: The gauss function is superseded by random_normal in the `distrib' package.
Perhaps you meant to enter `~a'.~%"
    (print-invert-case (implode (mstring `(($random_normal) ,@ (cdr form))))))
  '$done)
