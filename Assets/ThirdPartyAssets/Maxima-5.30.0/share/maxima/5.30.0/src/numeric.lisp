;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10-*- ;;;;

;;; This package contains a numeric class for use with Maxima.  The
;;; purpose is to allow users to write numerical algorithms that
;;; support double-float, (complex double-float) and Maxima bfloat and
;;; complex bfloat arithmetic, without having to write separate
;;; versions for each.  Of course, specially written versions for
;;; double-float and (complex double-float) will probably be much
;;; faster, but this allows users to write just one routine that can
;;; handle all of the data types in a more "natural" Lisp style.

#+cmu
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf lisp::*enable-package-locked-errors* nil))

(in-package #-gcl #:bigfloat #+gcl "BIGFLOAT")

(defun intofp (re)
  ;; Kind of like Maxima's INTOFP, but we only handle numeric types.
  ;; We should return a Maxima bigfloat object (list of bigfloat
  ;; marker, mantissa, and exponent).
  (cond ((floatp re)
	 (maxima::bcons (maxima::floattofp re)))
	((eql re 0)
	 (maxima::bcons '(0 0)))
	((integerp re)
	 (maxima::bcons (list (maxima::fpround re) (cl:+ maxima::*m maxima::fpprec))))
	((typep re 'ratio)
	 ;; Should we do something better than converting the
	 ;; numerator and denominator to floats and dividing?
	 (maxima::bcons (maxima::fpquotient (cdr (intofp (numerator re)))
					    (cdr (intofp (denominator re))))))
	((maxima::$bfloatp re)
	 ;; Call bigfloatp to make sure we round/scale the bigfloat to
	 ;; the correct precision!
	 (maxima::bigfloatp re))
	(t
	 (error "Don't know how to convert ~S to a BIGFLOAT" re))))

(defclass numeric ()
  ()
  (:documentation "Basic class, like CL's NUMBER type"))

(defclass bigfloat (numeric)
  ;; We store the Maxima internal bigfloat format because we also need
  ;; the precision in case we have mixed size bigfloat operations.
  ;; (We could recompute it from the size of the mantissa part, but
  ;; why bother?
  ((real :initform (intofp 0)
	 :initarg :real
	 :documentation "A Maxima bigfloat.  This contains the full
	 Maxima bigfloat object including the mantissa, the exponent
	 and the bigfloat marker and precision." ))
  (:documentation "Big float, equivalent to a Maxima bfloat object"))

;; Extract the internal representation of a bigfloat, and adjust the
;; precision to the current value of fpprec.
(defmethod real-value ((b bigfloat))
  (maxima::bigfloatp (slot-value b 'real)))

(defclass complex-bigfloat (numeric)
  ;; Currently, the real and imaginary parts contain a Maxima bigfloat
  ;; including the bigfloat marker and the mantissa and exponent.
  ;; Should they be BIGFLOAT objects instead?
  ((real :initform (intofp 0)
	 :initarg :real
	 :documentation "Real part of a complex bigfloat")
   (imag :initform (intofp 0)
	 :initarg :imag
	 :documentation "Imaginary part of a complex bigfloat"))
  (:documentation "Complex bigfloat"))

;; Extract the internal representation of the real part of a
;; complex-bigfloat, and adjust the precision to the current value of
;; fpprec.
(defmethod real-value ((b complex-bigfloat))
  (maxima::bigfloatp (slot-value b 'real)))

;; Extract the internal representation of the imaginary part of a
;; complex-bigfloat, and adjust the precision to the current value of
;; fpprec.
(defmethod imag-value ((b complex-bigfloat))
  (maxima::bigfloatp (slot-value b 'imag)))

(defmethod make-load-form ((x bigfloat) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(class-of x)
		  :real ',(real-value x)))

;;; BIGFLOAT - External
;;;
;;;    BIGFLOAT converts a number to a BIGFLOAT or COMPLEX-BIGFLOAT.
;;; This is intended to convert CL numbers or Maxima (internal)
;;; numbers to a bigfloat object.
(defun bigfloat (re &optional im)
  "Convert RE to a BIGFLOAT.  If IM is given, return a COMPLEX-BIGFLOAT"
  (cond (im
	 (make-instance 'complex-bigfloat
			:real (intofp re)
			:imag (intofp im)))
	((cl:realp re)
	 (make-instance 'bigfloat :real (intofp re)))
	((cl:complexp re)
	 (make-instance 'complex-bigfloat
			:real (intofp (cl:realpart re))
			:imag (intofp (cl:imagpart re))))
	((maxima::$bfloatp re)
	 (make-instance 'bigfloat :real (intofp re)))
	((maxima::complex-number-p re 'maxima::bigfloat-or-number-p)
	 (make-instance 'complex-bigfloat
			:real (intofp (maxima::$realpart re))
			:imag (intofp (maxima::$imagpart re))))
	((typep re 'bigfloat)
	 ;; Done this way so the new bigfloat updates the precision of
	 ;; the given bigfloat, if necessary.
	 (make-instance 'bigfloat :real (real-value re)))
	((typep re 'complex-bigfloat)
	 ;; Done this way so the new bigfloat updates the precision of
	 ;; the given bigfloat, if necessary.
	 (make-instance 'complex-bigfloat
			:real (real-value re)
			:imag (imag-value re)))
	(t
	 (make-instance 'bigfloat :real (intofp re)))))


;;; MAXIMA::TO - External
;;;
;;;    Convert a CL number, a BIGFLOAT, or a COMPLEX-BIGFLOAT to
;;; Maxima's internal representation of the number.
(defmethod maxima::to ((z cl:float))
  z)

(defmethod maxima::to ((z cl:rational))
  (if (typep z 'ratio)
      (list '(maxima::rat maxima::simp) (numerator z) (denominator z))
      z))

(defmethod maxima::to ((z cl:complex))
  (maxima::add (maxima::to (cl:realpart z))
	       (maxima::mul 'maxima::$%i
			    (maxima::to (cl:imagpart z)))))

(defmethod maxima::to ((z bigfloat))
  "Convert BIGFLOAT  object to a maxima number"
  (real-value z))

(defmethod maxima::to ((z complex-bigfloat))
  "Convert COMPLEX-BIGFLOAT  object to a maxima number"
  (maxima::add (real-value z)
	       (maxima::mul 'maxima::$%i
			    (imag-value z))))

(defmethod maxima::to ((z t))
  z)

;; MAX-EXPONENT roughly computes the log2(|x|).  If x is real and x =
;; 2^n*f, with |f| < 1, MAX-EXPONENT returns |n|.  For complex
;; numbers, we return one more than the max of the exponent of the
;; real and imaginary parts.
(defmethod max-exponent ((x bigfloat))
  (cl:abs (third (slot-value x 'real))))

(defmethod max-exponent ((x complex-bigfloat))
  (cl:1+ (cl:max (cl:abs (third (slot-value x 'real)))
		 (cl:abs (third (slot-value x 'imag))))))

(defmethod max-exponent ((x cl:float))
  (cl:abs (nth-value 1 (cl:decode-float x))))

(defmethod max-exponent ((x cl:rational))
  (cl:ceiling (cl:log (cl:abs x) 2)))

(defmethod max-exponent ((x cl:complex))
  (cl:1+ (cl:max (max-exponent (cl:realpart x))
		 (max-exponent (cl:imagpart x)))))

;; When computing x^a using exp(a*log(x)), we need extra bits because
;; the integer part of a*log(x) doesn't contribute to the accuracy of
;; the result.  The number of extra bits needed is basically the
;; "size" of a plus the number of bits for ceiling(log(x)).  We need
;; ceiling(log(x)) extra bits because that's how many bits are taken
;; up by the log(x).  The "size" of a is, basically, the exponent of
;; a. If a = 2^n*f where |f| < 1, then the size is abs(n) because
;; that's how many extra bits are added to the integer part of
;; a*log(x).
(defun expt-extra-bits (x a)
  (max 1 (+ (integer-length (max-exponent x))
	    (max-exponent a))))

;;; WITH-EXTRA-PRECISION - Internal
;;;
;;;   Executes the body BODY with extra precision.  The precision is
;;; increased by EXTRA, and the list of variables given in VARLIST have
;;; the precision increased.  The precision of the first value of the
;;; body is then reduced back to the normal precision.
(defmacro with-extra-precision ((extra (&rest varlist)) &body body)
  (let ((result (gensym))
	(old-fpprec (gensym)))
    `(let ((,result
	     (let ((,old-fpprec maxima::fpprec))
	       (unwind-protect
		    (let ((maxima::fpprec (cl:+ maxima::fpprec ,extra)))
		      (let ,(mapcar #'(lambda (v)
					;; Could probably do this in a faster
					;; way, but conversion to a maxima
					;; form automatically increases the
					;; precision of the bigfloat to the
					;; new precision.  Conversion of that
					;; to a bigfloat object preserves the
					;; precision.
					`(,v (bigfloat:to (maxima::to ,v))))
			     varlist)
			,@body))
		 (setf maxima::fpprec ,old-fpprec)))))
       ;; Conversion of the result to a maxima number adjusts the
       ;; precision appropriately.
       (bigfloat:to (maxima::to ,result)))))
     
;;; REALP

;; GCL doesn't have the REAL class!  But since a real is a rational or
;; float, we can fake it by defining methods on rationals and floats
;; for gcl.
#-gcl
(defmethod realp ((x cl:real))
  t)

#+gcl
(progn
  (defmethod realp ((x cl:rational))
    t)
  (defmethod realp ((x cl:float))
    t)
  )
  

(defmethod realp ((x bigfloat))
  t)

(defmethod realp ((x t))
  nil)

;;; COMPLEXP
(defmethod complexp ((x cl:complex))
  t)

(defmethod complexp ((x complex-bigfloat))
  t)

(defmethod complexp ((x t))
  nil)

;;; NUMBERP
(defmethod numberp ((x cl:number))
  t)

(defmethod numberp ((x bigfloat))
  t)

(defmethod numberp ((x complex-bigfloat))
  t)

(defmethod numberp ((x t))
  nil)

(defmethod make-load-form ((x complex-bigfloat) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(class-of x)
		  :real ',(real-value x)
		  :imag ',(imag-value x)))

;; The print-object and describe-object methods are mostly for
;; debugging purposes.  Maxima itself shouldn't ever see such objects.
(defmethod print-object ((x bigfloat) stream)
  (let ((r (cdr (real-value x))))
    (multiple-value-bind (sign output-list)
	(if (cl:minusp (first r))
	    (values "-" (maxima::fpformat (maxima::bcons (list (cl:- (first r)) (second r)))))
	    (values "+" (maxima::fpformat (maxima::bcons r))))
      (format stream "~A~{~D~}" sign output-list))))

(defmethod print-object ((x complex-bigfloat) stream)
  (format stream "~A~A*%i" (realpart x) (imagpart x)))

(defmethod describe-object ((x bigfloat) stream)
  (let ((r (slot-value x 'real)))
    (format stream "~&~S is a ~D-bit BIGFLOAT with mantissa ~D and exponent ~D~%"
	    x (third (first r)) (second r) (third r))))

(defmethod describe-object ((x complex-bigfloat) stream)
  (format stream "~S is a COMPLEX-BIGFLOAT~%" x)
  (describe-object (make-instance 'bigfloat :real (slot-value x 'real)) stream)
  (describe-object (make-instance 'bigfloat :real (slot-value x 'imag)) stream))


(defgeneric add1 (a)
  (:documentation "Add 1"))

(defgeneric sub1 (a)
  (:documentation "Subtract 1"))


(defgeneric two-arg-+ (a b)
  (:documentation "A + B"))

(defgeneric two-arg-- (a b)
  (:documentation "A - B"))

(defgeneric two-arg-* (a b)
  (:documentation "A * B"))

(defgeneric two-arg-/ (a b)
  (:documentation "A / B"))

(defgeneric two-arg-< (a b)
  (:documentation "A < B"))

(defgeneric two-arg-> (a b)
  (:documentation "A > B"))

(defgeneric two-arg-<= (a b)
  (:documentation "A <= B"))

(defgeneric two-arg->= (a b)
  (:documentation "A >= B"))

(defgeneric two-arg-= (a b)
  (:documentation "A = B?"))


(defgeneric unary-minus (a)
  (:documentation "-A"))

(defgeneric unary-divide (a)
  (:documentation "1 / A"))


;;; Basic arithmetic operations

;;; 1+ and 1-

(defmethod add1 ((a number))
  (cl::1+ a))

(defmethod add1 ((a bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpplus (cdr (real-value a))
					(maxima::fpone)))))

(defmethod add1 ((a complex-bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::bcons
			(maxima::fpplus (cdr (real-value a))
					(maxima::fpone)))
		 :imag (imag-value a)))

(defmethod sub1 ((a number))
  (cl::1- a))

(defmethod sub1 ((a bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpdifference (cdr (real-value a))
					      (maxima::fpone)))))

(defmethod sub1 ((a complex-bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::bcons
			(maxima::fpdifference (cdr (real-value a))
					      (maxima::fpone)))
		 :imag (imag-value a)))

(declaim (inline 1+ 1-))

(defun 1+ (x)
  (add1 x))

(defun 1- (x)
  (sub1 x))

;; Add two numbers
(defmethod two-arg-+ ((a cl:number) (b cl:number))
  (cl:+ a b))

(defmethod two-arg-+ ((a bigfloat) (b bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpplus (cdr (real-value a))
					(cdr (real-value b))))))

(defmethod two-arg-+ ((a complex-bigfloat) (b complex-bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::bcons
			(maxima::fpplus (cdr (real-value a))
					(cdr (real-value b))))
		 :imag (maxima::bcons
			(maxima::fpplus (cdr (imag-value a))
					(cdr (imag-value b))))))

;; Handle contagion for two-arg-+
(defmethod two-arg-+ ((a bigfloat) (b cl:float))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpplus (cdr (real-value a))
					(cdr (intofp b))))))

(defmethod two-arg-+ ((a bigfloat) (b cl:rational))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpplus (cdr (real-value a))
					(cdr (intofp b))))))

(defmethod two-arg-+ ((a bigfloat) (b cl:complex))
  (make-instance 'complex-bigfloat
		 :real (maxima::bcons
			(maxima::fpplus (cdr (real-value a))
					(cdr (intofp (realpart b)))))
		 :imag (intofp (imagpart b))))

(defmethod two-arg-+ ((a cl:number) (b bigfloat))
  (two-arg-+ b a))

(defmethod two-arg-+ ((a complex-bigfloat) (b bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::bcons
			(maxima::fpplus (cdr (real-value a))
					(cdr (real-value b))))
		 :imag (imag-value a)))

(defmethod two-arg-+ ((a complex-bigfloat) (b number))
  (make-instance 'complex-bigfloat
		 :real (maxima::bcons
			(maxima::fpplus (cdr (real-value a))
					(cdr (intofp (cl:realpart b)))))
		 :imag (maxima::bcons
			(maxima::fpplus (cdr (imag-value a))
					(cdr (intofp (cl:imagpart b)))))))

(defmethod two-arg-+ ((a bigfloat) (b complex-bigfloat))
  (two-arg-+ b a))

(defmethod two-arg-+ ((a number) (b complex-bigfloat))
  (two-arg-+ b a))

(defun + (&rest args)
  (if (null args)
      0
      (do ((args (cdr args) (cdr args))
	   (res (car args)
		(two-arg-+ res (car args))))
	  ((null args) res))))

;; Negate a number
(defmethod unary-minus ((a number))
  (cl:- a))

(defmethod unary-minus ((a bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpminus (cdr (real-value a))))))

(defmethod unary-minus ((a complex-bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::bcons
			(maxima::fpminus (cdr (real-value a))))
		 :imag (maxima::bcons
			(maxima::fpminus (cdr (imag-value a))))))

;;; Subtract two numbers
(defmethod two-arg-- ((a number) (b number))
  (cl:- a b))

(defmethod two-arg-- ((a bigfloat) (b bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpdifference (cdr (real-value a))
					      (cdr (real-value b))))))

(defmethod two-arg-- ((a complex-bigfloat) (b complex-bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::bcons
			(maxima::fpdifference (cdr (real-value a))
					      (cdr (real-value b))))
		 :imag (maxima::bcons
			(maxima::fpdifference (cdr (imag-value a))
					      (cdr (imag-value b))))))

;; Handle contagion for two-arg--
(defmethod two-arg-- ((a bigfloat) (b cl:float))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpdifference (cdr (real-value a))
					      (cdr (intofp b))))))

(defmethod two-arg-- ((a bigfloat) (b cl:rational))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpdifference (cdr (real-value a))
					      (cdr (intofp b))))))

(defmethod two-arg-- ((a bigfloat) (b cl:complex))
  (make-instance 'complex-bigfloat
		 :real (maxima::bcons
			(maxima::fpdifference (cdr (real-value a))
					      (cdr (intofp (realpart b)))))
		 :imag (maxima::bcons (maxima::fpminus (cdr (intofp (imagpart b)))))))

(defmethod two-arg-- ((a cl:float) (b bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpdifference (cdr (intofp a))
					      (cdr (real-value b))))))

(defmethod two-arg-- ((a cl:rational) (b bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpdifference (cdr (intofp a))
					      (cdr (real-value b))))))

(defmethod two-arg-- ((a cl:complex) (b bigfloat))
  (two-arg-- (bigfloat (cl:realpart a) (cl:imagpart a)) b))

(defmethod two-arg-- ((a complex-bigfloat) (b bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::bcons
			(maxima::fpdifference (cdr (real-value a))
					      (cdr (real-value b))))
		 :imag (imag-value a)))

(defmethod two-arg-- ((a complex-bigfloat) (b number))
  (if (cl:complexp b)
      (two-arg-- a (bigfloat (cl:realpart b) (cl:imagpart b)))
      (two-arg-- a (bigfloat b))))

(defmethod two-arg-- ((a bigfloat) (b complex-bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::bcons
			(maxima::fpdifference (cdr (real-value a))
					      (cdr (real-value b))))
		 :imag (maxima::bcons (maxima::fpminus (cdr (imag-value b))))))

(defmethod two-arg-- ((a number) (b complex-bigfloat))
  (if (cl:complexp a)
      (two-arg-- (bigfloat (cl:realpart a) (cl:imagpart a))
		 b)
      (two-arg-- (bigfloat a) b)))

(defun - (number &rest more-numbers)
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result (two-arg-- result (car nlist))))
      (unary-minus number)))

;;; Multiply two numbers
(defmethod two-arg-* ((a number) (b number))
  (cl:* a b))

(defmethod two-arg-* ((a bigfloat) (b bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fptimes* (cdr (real-value a))
					  (cdr (real-value b))))))

(defmethod two-arg-* ((a complex-bigfloat) (b complex-bigfloat))
  (let ((a-re (cdr (real-value a)))
	(a-im (cdr (imag-value a)))
	(b-re (cdr (real-value b)))
	(b-im (cdr (imag-value b))))
    (make-instance 'complex-bigfloat
		   :real (maxima::bcons
			  (maxima::fpdifference (maxima::fptimes* a-re b-re)
						(maxima::fptimes* a-im b-im)))
		   :imag (maxima::bcons
			  (maxima::fpplus (maxima::fptimes* a-re b-im)
					  (maxima::fptimes* a-im b-re))))))

;; Handle contagion for two-arg-*
(defmethod two-arg-* ((a bigfloat) (b cl:float))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fptimes* (cdr (real-value a))
					  (cdr (intofp b))))))

(defmethod two-arg-* ((a bigfloat) (b cl:rational))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fptimes* (cdr (real-value a))
					  (cdr (intofp b))))))

(defmethod two-arg-* ((a bigfloat) (b cl:complex))
  (make-instance 'complex-bigfloat
		 :real (maxima::bcons
			(maxima::fptimes* (cdr (real-value a))
					  (cdr (intofp (realpart b)))))
		 :imag (maxima::bcons
			(maxima::fptimes* (cdr (real-value a))
					  (cdr (intofp (imagpart b)))))))

(defmethod two-arg-* ((a cl:number) (b bigfloat))
  (two-arg-* b a))

(defmethod two-arg-* ((a complex-bigfloat) (b bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::bcons
			(maxima::fptimes* (cdr (real-value a))
					  (cdr (real-value b))))
		 :imag (maxima::bcons
			(maxima::fptimes* (cdr (imag-value a))
					  (cdr (real-value b))))))

(defmethod two-arg-* ((a complex-bigfloat) (b number))
  (if (cl:complexp b)
      (two-arg-* a (bigfloat (cl:realpart b) (cl:imagpart b)))
      (two-arg-* a (bigfloat b))))

(defmethod two-arg-* ((a bigfloat) (b complex-bigfloat))
  (two-arg-* b a))

(defmethod two-arg-* ((a number) (b complex-bigfloat))
  (two-arg-* b a))

(defun * (&rest args)
  (if (null args)
      1
      (do ((args (cdr args) (cdr args))
	   (res (car args)
		(two-arg-* res (car args))))
	  ((null args) res))))

;;; Reciprocal of a number
(defmethod unary-divide ((a number))
  (cl:/ a))

(defmethod unary-divide ((a bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpquotient (maxima::fpone)
					    (cdr (real-value a))))))

(defmethod unary-divide ((b complex-bigfloat))
  ;; Could just call two-arg-/, but let's optimize this a little
  (let ((a-re (maxima::fpone))
	(b-re (cdr (real-value b)))
	(b-im (cdr (imag-value b))))
    (if (maxima::fpgreaterp (maxima::fpabs b-re) (maxima::fpabs b-im))
	(let* ((r (maxima::fpquotient b-im b-re))
	       (dn (maxima::fpplus b-re (maxima::fptimes* r b-im))))
	  (make-instance 'complex-bigfloat
			 :real (maxima::bcons (maxima::fpquotient a-re dn))
			 :imag (maxima::bcons
				(maxima::fpquotient (maxima::fpminus r)
						    dn))))
	(let* ((r (maxima::fpquotient b-re b-im))
	       (dn (maxima::fpplus b-im (maxima::fptimes* r b-re))))
	  (make-instance 'complex-bigfloat
			 :real (maxima::bcons (maxima::fpquotient r dn))
			 :imag (maxima::bcons
				(maxima::fpquotient (maxima::fpminus a-re)
						    dn)))))))
;;; Divide two numbers
(defmethod two-arg-/ ((a number) (b number))
  (cl:/ a b))

(defmethod two-arg-/ ((a bigfloat) (b bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpquotient (cdr (real-value a))
					    (cdr (real-value b))))))

(defmethod two-arg-/ ((a complex-bigfloat) (b complex-bigfloat))
  (let ((a-re (cdr (real-value a)))
	(a-im (cdr (imag-value a)))
	(b-re (cdr (real-value b)))
	(b-im (cdr (imag-value b))))
    (if (maxima::fpgreaterp (maxima::fpabs b-re) (maxima::fpabs b-im))
	(let* ((r (maxima::fpquotient b-im b-re))
	       (dn (maxima::fpplus b-re (maxima::fptimes* r b-im))))
	  (make-instance 'complex-bigfloat
			 :real (maxima::bcons
				(maxima::fpquotient
				 (maxima::fpplus a-re
						 (maxima::fptimes* a-im r))
				 dn))
			 :imag (maxima::bcons
				(maxima::fpquotient
				 (maxima::fpdifference a-im
						       (maxima::fptimes* a-re r))
				 dn))))
	(let* ((r (maxima::fpquotient b-re b-im))
	       (dn (maxima::fpplus b-im (maxima::fptimes* r b-re))))
	  (make-instance 'complex-bigfloat
			 :real (maxima::bcons
				(maxima::fpquotient
				 (maxima::fpplus (maxima::fptimes* a-re r)
						 a-im)
				 dn))
			 :imag (maxima::bcons
				(maxima::fpquotient (maxima::fpdifference
						     (maxima::fptimes* a-im r)
						     a-re)
						    dn)))))))
;; Handle contagion for two-arg-/
(defmethod two-arg-/ ((a bigfloat) (b cl:float))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpquotient (cdr (real-value a))
					    (cdr (intofp b))))))

(defmethod two-arg-/ ((a bigfloat) (b cl:rational))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpquotient (cdr (real-value a))
					    (cdr (intofp b))))))

(defmethod two-arg-/ ((a bigfloat) (b cl:complex))
  (two-arg-/ (complex a) b))

(defmethod two-arg-/ ((a cl:float) (b bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpquotient (cdr (intofp a))
					    (cdr (real-value b))))))

(defmethod two-arg-/ ((a cl:rational) (b bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpquotient (cdr (intofp a))
					    (cdr (real-value b))))))

(defmethod two-arg-/ ((a cl:complex) (b bigfloat))
  (two-arg-/ (bigfloat a) b))


(defmethod two-arg-/ ((a complex-bigfloat) (b bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::bcons
			(maxima::fpquotient (cdr (real-value a))
					    (cdr (real-value b))))
		 :imag (maxima::bcons
			(maxima::fpquotient (cdr (imag-value a))
					    (cdr (real-value b))))))

(defmethod two-arg-/ ((a complex-bigfloat) (b number))
  (if (cl:complexp b)
      (two-arg-/ a (bigfloat (cl:realpart b) (cl:imagpart b)))
      (two-arg-/ a (bigfloat b))))

(defmethod two-arg-/ ((a bigfloat) (b complex-bigfloat))
  (two-arg-/ (make-instance 'complex-bigfloat :real (real-value a))
	     b))

(defmethod two-arg-/ ((a number) (b complex-bigfloat))
  (if (cl:complexp a)
      (two-arg-/ (bigfloat (cl:realpart a) (cl:imagpart a))
		 b)
      (two-arg-/ (bigfloat a) b)))


(defun / (number &rest more-numbers)
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result (two-arg-/ result (car nlist))))
      (unary-divide number)))

;;; Compare against zero (zerop, minusp, plusp)
(macrolet
    ((frob (name)
       (let ((cl-name (intern (symbol-name name) :cl)))
	 `(progn
	    (defmethod ,name ((x cl:float))
	      (,cl-name x))
	    (defmethod ,name ((x cl:rational))
	      (,cl-name x))))))
  (frob plusp)
  (frob minusp))

(defmethod zerop ((x number))
  (cl:zerop x))

(defmethod zerop ((x bigfloat))
  (let ((r (cdr (real-value x))))
    (and (zerop (first r))
	 (zerop (second r)))))

(defmethod zerop ((a complex-bigfloat))
  (and (equal (cdr (real-value a)) '(0 0))
       (equal (cdr (imag-value a)) '(0 0))))

(defmethod plusp ((x bigfloat))
  (cl:plusp (first (cdr (real-value x)))))

(defmethod minusp ((x bigfloat))
  (cl:minusp (first (cdr (real-value x)))))



;;; Equality 
(defmethod two-arg-= ((a number) (b number))
  (cl:= a b))

(defmethod two-arg-= ((a bigfloat) (b bigfloat))
  (zerop (two-arg-- a b)))

(defmethod two-arg-= ((a complex-bigfloat) (b complex-bigfloat))
  (zerop (two-arg-- a b)))

;; Handle contagion for two-arg-=.  This needs some work.  CL says
;; floats and rationals are compared by converting the float to a
;; rational before converting.
(defmethod two-arg-= ((a bigfloat) (b number))
  (zerop (two-arg-- a b)))

(defmethod two-arg-= ((a number) (b bigfloat))
  (two-arg-= b a))

(defmethod two-arg-= ((a complex-bigfloat) (b number))
  (zerop (two-arg-- a b)))

(defmethod two-arg-= ((a number) (b complex-bigfloat))
  (two-arg-= b a))

(defun = (number &rest more-numbers)
  "Returns T if all of its arguments are numerically equal, NIL otherwise."
  (declare (optimize (safety 2))
	   #-gcl (dynamic-extent more-numbers))
  (do ((nlist more-numbers (cdr nlist)))
      ((atom nlist) t)
    (declare (list nlist))
    (if (not (two-arg-= (car nlist) number))
	(return nil))))

(defun /= (number &rest more-numbers)
  "Returns T if no two of its arguments are numerically equal, NIL otherwise."
  (declare (optimize (safety 2))
	   #-gcl (dynamic-extent more-numbers))
  (do* ((head number (car nlist))
	(nlist more-numbers (cdr nlist)))
       ((atom nlist) t)
    (declare (list nlist))
    (unless (do* ((nl nlist (cdr nl)))
		 ((atom nl) t)
	      (declare (list nl))
	      (if (two-arg-= head (car nl))
		  (return nil)))
      (return nil))))

;;; Comparison operations
(macrolet
    ((frob (op)
       (let ((method (intern (concatenate 'string
					  (string '#:two-arg-)
					  (symbol-name op))))
	     (cl-fun (find-symbol (symbol-name op) :cl)))
	 `(progn
	    (defmethod ,method ((a cl:float) (b cl:float))
	      (,cl-fun a b))
	    (defmethod ,method ((a cl:float) (b cl:rational))
	      (,cl-fun a b))
	    (defmethod ,method ((a cl:rational) (b cl:float))
	      (,cl-fun a b))
	    (defmethod ,method ((a cl:rational) (b cl:rational))
	      (,cl-fun a b))
	    (defun ,op (number &rest more-numbers)
	      "Returns T if its arguments are in strictly increasing order, NIL otherwise."
	      (declare (optimize (safety 2))
		       #-gcl (dynamic-extent more-numbers))
	      (do* ((n number (car nlist))
		    (nlist more-numbers (cdr nlist)))
		   ((atom nlist) t)
		(declare (list nlist))
		(if (not (,method n (car nlist))) (return nil))))))))
  (frob <)
  (frob >)
  (frob <=)
  (frob >=))

(defmethod two-arg-< ((x bigfloat) (y bigfloat))
  (maxima::fplessp (cdr (real-value x)) (cdr (real-value y))))

(defmethod two-arg-< ((x bigfloat) (y cl:float))
  (maxima::fplessp (cdr (real-value x)) (cdr (intofp y))))

(defmethod two-arg-< ((x bigfloat) (y cl:rational))
  (maxima::fplessp (cdr (real-value x)) (cdr (intofp y))))

(defmethod two-arg-< ((x cl:float) (y bigfloat))
  (maxima::fplessp (cdr (intofp x)) (cdr (real-value y))))

(defmethod two-arg-< ((x cl:rational) (y bigfloat))
  (maxima::fplessp (cdr (intofp x)) (cdr (real-value y))))

(defmethod two-arg-> ((x bigfloat) (y bigfloat))
  (maxima::fpgreaterp (cdr (real-value x)) (cdr (real-value y))))

(defmethod two-arg-> ((x bigfloat) (y cl:float))
  (maxima::fpgreaterp (cdr (real-value x)) (cdr (intofp y))))

(defmethod two-arg-> ((x bigfloat) (y cl:rational))
  (maxima::fpgreaterp (cdr (real-value x)) (cdr (intofp y))))

(defmethod two-arg-> ((x cl:float) (y bigfloat))
  (maxima::fpgreaterp (cdr (intofp x)) (cdr (real-value y))))

(defmethod two-arg-> ((x cl:rational) (y bigfloat))
  (maxima::fpgreaterp (cdr (intofp x)) (cdr (real-value y))))

(defmethod two-arg-<= ((x bigfloat) (y bigfloat))
  (or (zerop (two-arg-- x y))
      (maxima::fplessp (cdr (real-value x)) (cdr (real-value y)))))

(defmethod two-arg-<= ((x bigfloat) (y cl:float))
  (or (zerop (two-arg-- x y))
      (maxima::fplessp (cdr (real-value x)) (cdr (intofp y)))))

(defmethod two-arg-<= ((x bigfloat) (y cl:rational))
  (or (zerop (two-arg-- x y))
      (maxima::fplessp (cdr (real-value x)) (cdr (intofp y)))))

(defmethod two-arg-<= ((x cl:float) (y bigfloat))
  (or (zerop (two-arg-- x y))
      (maxima::fplessp (cdr (intofp x)) (cdr (real-value y)))))

(defmethod two-arg-<= ((x cl:rational) (y bigfloat))
  (or (zerop (two-arg-- x y))
      (maxima::fplessp (cdr (intofp x)) (cdr (real-value y)))))

(defmethod two-arg->= ((x bigfloat) (y bigfloat))
  (or (zerop (two-arg-- x y))
      (maxima::fpgreaterp (cdr (real-value x)) (cdr (real-value y)))))

(defmethod two-arg->= ((x bigfloat) (y cl:float))
  (or (zerop (two-arg-- x y))
      (maxima::fpgreaterp (cdr (real-value x)) (cdr (intofp y)))))

(defmethod two-arg->= ((x bigfloat) (y cl:rational))
  (or (zerop (two-arg-- x y))
      (maxima::fpgreaterp (cdr (real-value x)) (cdr (intofp y)))))

(defmethod two-arg->= ((x cl:float) (y bigfloat))
  (or (zerop (two-arg-- x y))
      (maxima::fpgreaterp (cdr (intofp x)) (cdr (real-value y)))))

(defmethod two-arg->= ((x cl:rational) (y bigfloat))
  (or (zerop (two-arg-- x y))
      (maxima::fpgreaterp (cdr (intofp x)) (cdr (real-value y)))))

;; Need to define incf and decf to call our generic +/- methods.
(defmacro incf (place &optional (delta 1) &environment env)
  "The first argument is some location holding a number. This number is
  incremented by the second argument, DELTA, which defaults to 1."
  (multiple-value-bind (dummies vals newval setter getter)
      (#-gcl get-setf-expansion #+gcl get-setf-method
	     place env)
    (let ((d (gensym)))
      `(let* (,@(mapcar #'list dummies vals)
              (,d ,delta)
              (,(car newval) (+ ,getter ,d)))
         ,setter))))

(defmacro decf (place &optional (delta 1) &environment env)
  "The first argument is some location holding a number. This number is
  decremented by the second argument, DELTA, which defaults to 1."
  (multiple-value-bind (dummies vals newval setter getter)
      (#-gcl get-setf-expansion #+gcl get-setf-method
	     place env)
    (let ((d (gensym)))
      `(let* (,@(mapcar #'list dummies vals)
              (,d ,delta)
              (,(car newval) (- ,getter ,d)))
         ,setter))))

    

;;; Special functions for real-valued arguments
(macrolet
    ((frob (name)
       (let ((cl-name (intern (symbol-name name) :cl)))
	 `(progn
	    (defmethod ,name ((x number))
	      (,cl-name x))))))
  (frob sqrt)
  (frob abs)
  (frob exp)
  (frob sin)
  (frob cos)
  (frob tan)
  (frob asin)
  (frob acos)
  (frob sinh)
  (frob cosh)
  (frob tanh)
  (frob asinh)
  (frob acosh)
  (frob atanh)
  )

(defmethod abs ((x bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons (maxima::fpabs (cdr (real-value x))))))

(defmethod abs ((z complex-bigfloat))
  (let ((x (make-instance 'bigfloat :real (real-value z)))
	(y (make-instance 'bigfloat :real (imag-value z))))
    ;; Bigfloats don't overflow, so we don't need anything special.
    (sqrt (+ (* x x) (* y y)))))

(defmethod exp ((x bigfloat))
  (make-instance 'bigfloat 
		 :real (maxima::bcons (maxima::fpexp (cdr (real-value x))))))

(defmethod sin ((x bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons (maxima::fpsin (cdr (real-value x)) t))))

(defmethod cos ((x bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons (maxima::fpsin (cdr (real-value x)) nil))))

(defmethod tan ((x bigfloat))
  (let ((r (cdr (real-value x))))
    (make-instance 'bigfloat
		   :real (maxima::bcons
			  (maxima::fpquotient (maxima::fpsin r t)
					      (maxima::fpsin r nil))))))

(defmethod asin ((x bigfloat))
  (bigfloat (maxima::fpasin (real-value x))))

(defmethod acos ((x bigfloat))
  (bigfloat (maxima::fpacos (real-value x))))


(defmethod sqrt ((x bigfloat))
  (if (minusp x)
      (make-instance 'complex-bigfloat
		     :real (intofp 0)
		     :imag (maxima::bcons
			    (maxima::fproot (maxima::bcons (maxima::fpabs (cdr (real-value x)))) 2)))
      (make-instance 'bigfloat
		     :real (maxima::bcons
			    (maxima::fproot (real-value x) 2)))))

(defmethod sqrt ((z complex-bigfloat))
  (multiple-value-bind (rx ry)
      (maxima::complex-sqrt (real-value z)
			    (imag-value z))
    (make-instance 'complex-bigfloat
		   :real (maxima::bcons rx)
		   :imag (maxima::bcons ry))))

(defmethod one-arg-log ((a number))
  (cl:log a))

(defmethod one-arg-log ((a bigfloat))
  (if (minusp a)
      (make-instance 'complex-bigfloat
		     :real (maxima::bcons
			    (maxima::fplog (maxima::fpabs (cdr (real-value a)))))
		     :imag (maxima::bcons (maxima::fppi)))
      (make-instance 'bigfloat
		     :real (maxima::bcons (maxima::fplog (cdr (real-value a)))))))

(defmethod one-arg-log ((a complex-bigfloat))
  (let* ((res (maxima::big-float-log (real-value a)
				     (imag-value a))))
    (bigfloat res)))

(defmethod two-arg-log ((a number) (b number))
  (cl:log a b))

(defmethod two-arg-log ((a numeric) (b numeric))
  (two-arg-/ (one-arg-log a) (one-arg-log b)))

(defmethod two-arg-log ((a numeric) (b cl:number))
  (two-arg-/ (one-arg-log a) (one-arg-log (bigfloat b))))

(defmethod two-arg-log ((a cl:number) (b numeric))
  (two-arg-/ (one-arg-log (bigfloat a)) (one-arg-log b)))

(defun log (a &optional b)
  (if b
      (two-arg-log a b)
      (one-arg-log a)))

(defmethod sinh ((x bigfloat))
  (let ((r (real-value x)))
    (make-instance 'bigfloat :real (maxima::fpsinh r))))

(defmethod cosh ((x bigfloat))
  (let ((r (real-value x)))
    (make-instance 'bigfloat :real (maxima::$bfloat `((maxima::%cosh) ,r)))))

(defmethod tanh ((x bigfloat))
  (let ((r (real-value x)))
    (make-instance 'bigfloat :real (maxima::fptanh r))))

(defmethod asinh ((x bigfloat))
  (let ((r (real-value x)))
    (make-instance 'bigfloat :real (maxima::fpasinh r))))

(defmethod atanh ((x bigfloat))
  (let ((r (maxima::big-float-atanh (real-value x))))
    (if (maxima::bigfloatp r)
	(make-instance 'bigfloat :real r)
	(make-instance 'complex-bigfloat
		       :real (maxima::$realpart r)
		       :imag (maxima::$imagpart r)))))

(defmethod acosh ((x bigfloat))
  (let* ((r (real-value x))
	 (value (maxima::mevalp `((maxima::%acosh maxima::simp)
				  ,r))))
    (if (maxima::bigfloatp value)
	(make-instance 'bigfloat :real value)
	(make-instance 'complex-bigfloat
		       :real (maxima::$realpart value)
		       :imag (maxima::$imagpart value)))))

;;; Complex arguments

;;; Special functions for complex args
(macrolet
    ((frob (name &optional big-float-op-p)
       (if big-float-op-p
	   (let ((big-op (intern (concatenate 'string
					      (string '#:big-float-)
					      (string name))
				 '#:maxima)))
	     `(defmethod ,name ((a complex-bigfloat))
		(let ((res (,big-op (real-value a)
				    (imag-value a))))
		  (to res))))
	   (let ((max-op (intern (concatenate 'string "$" (string name)) '#:maxima)))
	     `(defmethod ,name ((a complex-bigfloat))
		;; We should do something better than calling mevalp
		(let* ((arg (maxima::add (real-value a)
					 (maxima::mul 'maxima::$%i (imag-value a))))
		       (result (maxima::mevalp `((,',max-op maxima::simp) ,arg))))
		  (to result)))))))
  (frob exp)
  (frob sin)
  (frob cos)
  (frob tan)
  (frob asin t)
  (frob acos t)
  (frob sinh)
  (frob cosh)
  (frob tanh t)
  (frob asinh t)
  (frob acosh)
  (frob atanh t))

(defmethod one-arg-atan ((a number))
  (cl:atan a))

(defmethod one-arg-atan ((a bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons (maxima::fpatan (cdr (real-value a))))))

(defmethod one-arg-atan ((a complex-bigfloat))
  ;; We should do something better than calling mevalp
  (let* ((arg (maxima::add (real-value a)
			   (maxima::mul 'maxima::$%i (imag-value a))))
	 (result (maxima::mevalp `((maxima::%atan maxima::simp) ,arg))))
    (make-instance 'complex-bigfloat
		   :real (maxima::$realpart result)
		   :imag (maxima::$imagpart result))))

;; Really want type real, but gcl doesn't like that.  Define methods for rational and float
#-gcl
(defmethod two-arg-atan ((a real) (b real))
  (cl:atan a b))

#+gcl
(progn
  (defmethod two-arg-atan ((a cl:float) (b cl:float))
    (cl:atan a b))
  (defmethod two-arg-atan ((a cl:rational) (b cl:rational))
    (cl:atan a b))
  (defmethod two-arg-atan ((a cl:float) (b cl:rational))
    (cl:atan a b))
  (defmethod two-arg-atan ((a cl:rational) (b cl:float))
    (cl:atan a b))
  )

(defmethod two-arg-atan ((a bigfloat) (b bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons
			(maxima::fpatan2 (cdr (real-value a))
					 (cdr (real-value b))))))

(defmethod two-arg-atan ((a bigfloat) (b cl:float))
  (make-instance 'bigfloat
		 :real (maxima::bcons (maxima::fpatan2 (cdr (real-value a))
						       (cdr (intofp b))))))

(defmethod two-arg-atan ((a bigfloat) (b cl:rational))
  (make-instance 'bigfloat
		 :real (maxima::bcons (maxima::fpatan2 (cdr (real-value a))
						       (cdr (intofp b))))))

(defmethod two-arg-atan ((a cl:float) (b bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons (maxima::fpatan2 (cdr (intofp a))
						       (cdr (real-value b))))))

(defmethod two-arg-atan ((a cl:rational) (b bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons (maxima::fpatan2 (cdr (intofp a))
						       (cdr (real-value b))))))

(defun atan (a &optional b)
  (if b
      (two-arg-atan a b)
      (one-arg-atan a)))
      
(defmethod scale-float ((a cl:float) (n integer))
  (cl:scale-float a n))

(defmethod scale-float ((a bigfloat) (n integer))
  (if (cl:zerop (second (real-value a)))
      (make-instance 'bigfloat :real (maxima::bcons (list 0 0)))
      (destructuring-bind (marker mantissa exp)
	  (real-value a)
	(declare (ignore marker))
	(make-instance 'bigfloat :real (maxima::bcons (list mantissa (+ exp n)))))))

(macrolet
    ((frob (name)
       (let ((cl-name (intern (string name) '#:cl)))
	 `(defmethod ,name ((a number))
	    (,cl-name a)))))
  (frob realpart)
  (frob imagpart)
  (frob conjugate)
  (frob phase))

(macrolet
    ((frob (name)
       (let ((cl-name (intern (string name) '#:cl)))
	 `(defmethod ,name ((a number) &optional (divisor 1))
	    (,cl-name a divisor)))))
  (frob floor)
  (frob ffloor)
  (frob ceiling)
  (frob fceiling)
  (frob truncate)
  (frob ftruncate)
  (frob round)
  (frob fround))
  
  
(defmethod realpart ((a bigfloat))
  (make-instance 'bigfloat :real (real-value a)))

(defmethod realpart ((a complex-bigfloat))
  (make-instance 'bigfloat :real (real-value a)))

(defmethod imagpart ((a bigfloat))
  (make-instance 'bigfloat :real (intofp 0)))

(defmethod imagpart ((a complex-bigfloat))
  (make-instance 'bigfloat :real (imag-value a)))

(defmethod conjugate ((a bigfloat))
  (make-instance 'bigfloat :real (real-value a)))

(defmethod conjugate ((a complex-bigfloat))
  (make-instance 'complex-bigfloat
		 :real (real-value a)
		 :imag (maxima::bcons (maxima::fpminus (cdr (imag-value a))))))

(defmethod cis ((a cl:float))
  (cl:cis a))

(defmethod cis ((a cl:rational))
  (cl:cis a))

(defmethod cis ((a bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::bcons (maxima::fpsin (cdr (real-value a)) t))
		 :imag (maxima::bcons (maxima::fpsin (cdr (real-value a)) nil))))

(defmethod phase ((a bigfloat))
  (let ((r (cdr (real-value a))))
    (if (cl:>= (car r) 0)
	(make-instance 'bigfloat :real (maxima::bcons (list 0 0)))
	(make-instance 'bigfloat :real (maxima::bcons (maxima::fppi))))))

(defmethod phase ((a complex-bigfloat))
  (make-instance 'bigfloat
		 :real (maxima::bcons (maxima::fpatan2 (cdr (imag-value a))
						       (cdr (real-value a))))))

(defun max (number &rest more-numbers)
  "Returns the greatest of its arguments."
  (declare (optimize (safety 2)) (type (or real bigfloat) number)
	   #-gcl (dynamic-extent more-numbers))
  (dolist (real more-numbers)
    (when (> real number)
      (setq number real)))
  number)

(defun min (number &rest more-numbers)
  "Returns the least of its arguments."
  (declare (optimize (safety 2)) (type (or real bigfloat) number)
	   #-gcl (dynamic-extent more-numbers))
  (do ((nlist more-numbers (cdr nlist))
       (result (the (or real bigfloat) number)))
      ((null nlist) (return result))
    (declare (list nlist))
    (if (< (car nlist) result)
	(setq result (car nlist)))))

;; We really want a real type, but gcl doesn't like it, so use number
;; instead.
#-gcl
(defmethod one-arg-complex ((a real))
  (cl:complex a))

#+gcl
(progn
(defmethod one-arg-complex ((a cl:float))
  (cl:complex a))
(defmethod one-arg-complex ((a cl:rational))
  (cl:complex a))
)

(defmethod one-arg-complex ((a bigfloat))
  (make-instance 'complex-bigfloat
		 :real (real-value a)
		 :imag (intofp 0)))

#-gcl
(defmethod two-arg-complex ((a real) (b real))
  (cl:complex a b))

#+gcl
(progn
(defmethod two-arg-complex ((a cl:float) (b cl:float))
  (cl:complex a b))
(defmethod two-arg-complex ((a cl:rational) (b cl:rational))
  (cl:complex a b))
(defmethod two-arg-complex ((a cl:float) (b cl:rational))
  (cl:complex a b))
(defmethod two-arg-complex ((a cl:rational) (b cl:float))
  (cl:complex a b))
)

(defmethod two-arg-complex ((a bigfloat) (b bigfloat))
  (make-instance 'complex-bigfloat
		 :real (real-value a)
		 :imag (real-value b)))

(defmethod two-arg-complex ((a cl:float) (b bigfloat))
  (make-instance 'complex-bigfloat
		 :real (intofp a)
		 :imag (real-value b)))

(defmethod two-arg-complex ((a cl:rational) (b bigfloat))
  (make-instance 'complex-bigfloat
		 :real (intofp a)
		 :imag (real-value b)))

(defmethod two-arg-complex ((a bigfloat) (b cl:float))
  (make-instance 'complex-bigfloat
		 :real (real-value a)
		 :imag (intofp b)))

(defmethod two-arg-complex ((a bigfloat) (b cl:rational))
  (make-instance 'complex-bigfloat
		 :real (real-value a)
		 :imag (intofp b)))

(defun complex (a &optional b)
  (if b
      (two-arg-complex a b)
      (one-arg-complex a)))

(defmethod unary-floor ((a bigfloat))
  ;; fpentier truncates to zero, so adjust for negative numbers.
  (if (minusp a)
      (maxima::fpentier (real-value (- a 1)))
      (maxima::fpentier (real-value a))))

(defmethod unary-ffloor ((a bigfloat))
  ;; We can probably do better than converting to an integer and
  ;; converting back to a float.
  (make-instance 'bigfloat :real (intofp (unary-floor a))))

(defmethod floor ((a bigfloat) &optional (divisor 1))
  (if (= divisor 1)
      (let ((int (unary-floor a)))
	(values int (- a int)))
      (let ((q (unary-floor (/ a divisor))))
	(values q (- a (* q divisor))))))

(defmethod ffloor ((a bigfloat) &optional (divisor 1))
  (if (= divisor 1)
      (let ((int (unary-ffloor a)))
	(values int (- a int)))
      (let ((q (unary-ffloor (/ a divisor))))
	(values q (- a (* q divisor))))))

(defmethod unary-truncate ((a bigfloat))
  (maxima::fpentier (real-value a)))

(defmethod unary-ftruncate ((a bigfloat))
  ;; We can probably do better than converting to an integer and
  ;; converting back to a float.
  (make-instance 'bigfloat :real (intofp (unary-truncate a))))

(defmethod truncate ((a bigfloat) &optional (divisor 1))
  (if (eql divisor 1)
      (let ((int (unary-truncate a)))
	(values int (- a int)))
      (let ((q (unary-truncate (/ a divisor))))
	(values q (- a (* q divisor))))))

(defmethod ftruncate ((a bigfloat) &optional (divisor 1))
  (if (eql divisor 1)
      (let ((int (unary-ftruncate a)))
	(values int (- a int)))
      (let ((q (unary-ftruncate (/ a divisor))))
	(values q (- a (* q divisor))))))

(defmethod unary-ceiling ((a bigfloat))
  ;; fpentier truncates to zero, so adjust for positive numbers.
  (if (minusp a)
      (maxima::fpentier (real-value a))
      (maxima::fpentier (real-value (+ a 1)))))

(defmethod unary-fceiling ((a bigfloat))
  ;; We can probably do better than converting to an integer and
  ;; converting back to a float.
  (make-instance 'bigfloat :real (intofp (unary-ceiling a))))

(defmethod ceiling ((a bigfloat) &optional (divisor 1))
  (if (eql divisor 1)
      (let ((int (unary-ceiling a)))
	(values int (- a int)))
      (let ((q (unary-ceiling (/ a divisor))))
	(values q (- a (* q divisor))))))

(defmethod fceiling ((a bigfloat) &optional (divisor 1))
  (if (eql divisor 1)
      (let ((int (unary-fceiling a)))
	(values int (- a int)))
      (let ((q (unary-fceiling (/ a divisor))))
	(values q (- a (* q divisor))))))

;; Stolen from CMUCL.
(defmethod round ((a bigfloat) &optional (divisor 1))
  (multiple-value-bind (tru rem)
      (truncate a divisor)
    (if (zerop rem)
	(values tru rem)
	(let ((thresh (/ (abs divisor) 2)))
	  (cond ((or (> rem thresh)
		     (and (= rem thresh) (oddp tru)))
		 (if (minusp divisor)
		     (values (- tru 1) (+ rem divisor))
		     (values (+ tru 1) (- rem divisor))))
		((let ((-thresh (- thresh)))
		   (or (< rem -thresh)
		       (and (= rem -thresh) (oddp tru))))
		 (if (minusp divisor)
		     (values (+ tru 1) (- rem divisor))
		     (values (- tru 1) (+ rem divisor))))
		(t (values tru rem)))))))

(defmethod fround ((number bigfloat) &optional (divisor 1))
  "Same as ROUND, but returns first value as a float."
  (multiple-value-bind (res rem)
      (round number divisor)
    (values (bigfloat res) rem)))

(defmethod expt ((a number) (b number))
  (cl:expt a b))

;; This needs more work
(defmethod expt ((a numeric) (b numeric))
  (if (zerop b)
      ;; CLHS says if the power is 0, the answer is 1 of the appropriate type.
      (if (or (typep a 'complex-bigfloat)
	      (typep b 'complex-bigfloat))
	  (complex (bigfloat 1))
	  (bigfloat 1))
      (cond ((and (zerop a) (plusp (realpart b)))
	     (* a b))
	    ((and (typep b 'bigfloat) (= b (truncate b)))
	     ;; Use the numeric^number method because it can be much
	     ;; more accurate when b is an integer.
	     (expt a (truncate b)))
	    (t
	     (with-extra-precision ((expt-extra-bits a b)
				    (a b))
	       (exp (* b (log a))))))))

(defmethod expt ((a cl:number) (b numeric))
  (if (zerop b)
      ;; CLHS says if the power is 0, the answer is 1 of the appropriate type.
      (if (or (typep a 'cl:complex)
	      (typep b 'complex-bigfloat))
	  (complex (bigfloat 1))
	  (bigfloat 1))
      (cond ((and (zerop a) (plusp (realpart b)))
	     (* a b))
	    ((= b (truncate b))
	     (with-extra-precision ((expt-extra-bits a b)
				    (a b))
	       (expt a (truncate b))))
	    (t
	     (with-extra-precision ((expt-extra-bits a b)
				    (a b))
	       (exp (* b (log (bigfloat a)))))))))

(defmethod expt ((a numeric) (b cl:number))
  (if (zerop b)
      ;; CLHS says if the power is 0, the answer is 1 of the appropriate type.
      (if (or (typep a 'complex-bigfloat)
	      (typep b 'cl:complex))
	  (complex (bigfloat 1))
	  (bigfloat 1))
      (if (and (zerop a) (plusp (realpart b)))
	  (* a b)
	  ;; Handle a few special cases using multiplication.
	  (cond ((= b 1)
		 a)
		((= b -1)
		 (/ a))
		((= b 2)
		 (* a a))
		((= b -2)
		 (/ (* a a)))
		((= b 3) (* a a a))
		((= b -3) (/ (* a a a)))
		((= b 4)
		 (let ((a2 (* a a)))
		   (* a2 a2)))
		((= b -4)
		 (let ((a2 (* a a)))
		   (/ (* a2 a2))))
		(t
		 (with-extra-precision ((expt-extra-bits a b)
					(a b))
		   (exp (* (bigfloat b) (log a)))))))))

;; Handle a^b a little more carefully because the result is known to
;; be real when a is real and b is an integer.
(defmethod expt ((a bigfloat) (b integer))
  (cond ((zerop b)
	 (bigfloat 1))
	((and (zerop a) (plusp b))
	 ;; 0^b, for positive b
	 (* a b))
	;; Handle a few special cases using multiplication.
	((eql b 1) a)
	((eql b -1) (/ a))
	((eql b 2) (* a a))
	((eql b -2) (/ (* a a)))
	((eql b 3) (* a a a))
	((eql b -3) (/ (* a a a)))
	((eql b 4)
	 (let ((a2 (* a a)))
	   (* a2 a2)))
	((eql b -4)
	 (let ((a2 (* a a)))
	   (/ (* a2 a2))))
	((minusp a)
	 ;; a^b = exp(b*log(|a|) + %i*%pi*b)
	 ;;     = exp(b*log(|a|))*exp(%i*%pi*b)
	 ;;     = (-1)^b*exp(b*log(|a|))
	 (with-extra-precision ((expt-extra-bits a b)
				(a b))
	   (* (exp (* b (log (abs a))))
	      (if (oddp b) -1 1))))
	(t
	 (with-extra-precision ((expt-extra-bits a b)
				(a b))
	   (exp (* b (log a)))))))

;;; TO - External
;;;
;;;    TO takes a maxima number and converts it.  Floats remain
;;; floats, maxima cl:rationals are converted to CL cl:rationals.  Maxima
;;; bigfloats are convert to BIGFLOATS.  Maxima complex numbers are
;;; converted to CL complex numbers or to COMPLEX-BIGFLOAT's.
(defun to (maxima-num &optional imag)
  (let ((result (ignore-errors (%to maxima-num imag))))
    (or result
	(maxima::merror (intl:gettext "BIGFLOAT: unable to convert ~M to a CL or BIGFLOAT number.")
			(if imag
			    (maxima::add maxima-num (maxima::mul imag 'maxima::$%i))
			    maxima-num)))))

;;; MAYBE-TO - External
;;;
;;;   Like TO, but if the maxima number can't be converted to a CL
;;; number or BIGFLOAT, just return the maxima number.
(defun maybe-to (maxima-num &optional imag)
  (let ((result (ignore-errors (%to maxima-num imag))))
    (or result
	(if imag
	    (maxima::add maxima-num imag)
	    maxima-num))))
	
(defun %to (maxima-num &optional imag)
  (cond (imag
	 ;; Clisp has a "feature" that (complex rat float) does not
	 ;; make the both components of the complex number a float.
	 ;; Sometimes this is nice, but other times it's annoying
	 ;; because it is non-ANSI behavior.  For our code, we really
	 ;; want both components to be a float.
	 #-clisp
	 (complex (to maxima-num) (to imag))
	 #+clisp
	 (let ((re (to maxima-num))
	       (im (to imag)))
	   (cond ((and (rationalp re) (floatp im))
		  (setf re (float re im)))
		 ((and (rational im) (floatp re))
		  (setf im (float im re))))
	   (complex re im)))
	(t
	 (cond ((cl:numberp maxima-num)
		maxima-num)
	       ((eq maxima-num 'maxima::$%i)
		;; Convert %i to an exact complex cl:rational.
		#c(0 1))
	       ((consp maxima-num)
		;; Some kind of maxima number
		(cond ((maxima::ratnump maxima-num)
		       ;; Maxima cl:rational ((mrat ...) num den)
		       (/ (second maxima-num) (third maxima-num)))
		      ((maxima::$bfloatp maxima-num)
		       (bigfloat maxima-num))
		      ((maxima::complex-number-p maxima-num #'(lambda (x)
								(or (cl:realp x)
								    (maxima::$bfloatp x)
								    (and (consp x)
									 (eq (caar x) 'maxima::rat)))))
		       ;; We have some kind of complex number whose
		       ;; parts are a cl:real, a bfloat, or a Maxima
		       ;; cl:rational.
		       (let ((re (maxima::$realpart maxima-num))
			     (im (maxima::$imagpart maxima-num)))
			 (to re im)))))
	       ((or (typep maxima-num 'bigfloat)
		    (typep maxima-num 'complex-bigfloat))
		maxima-num)
	       (t
		(error "BIGFLOAT: unable to convert to a CL or BIGFLOAT number."))))))

;;; EPSILON - External
;;;
;;;   Return the float epsilon value for the given float type.
(defmethod epsilon ((x cl:float))
  (etypecase x
    (short-float short-float-epsilon)
    (single-float single-float-epsilon)
    (double-float double-float-epsilon)
    (long-float long-float-epsilon)))

(defmethod epsilon ((x cl:complex))
  (epsilon (cl:realpart x)))

(defmethod epsilon ((x bigfloat))
  ;; epsilon is just above 2^(-fpprec).
  (make-instance 'bigfloat
		 :real (maxima::bcons (list (1+ (ash 1 (1- maxima::fpprec)))
					    (- (1- maxima::fpprec))))))

(defmethod epsilon ((x complex-bigfloat))
  (epsilon (realpart x)))

      

;; Compiler macros to convert + to multiple calls to two-arg-+.  Same
;; for -, *, and /.
(define-compiler-macro + (&whole form &rest args)
  (declare (ignore form))
  (if (null args)
      0
      (do ((args (cdr args) (cdr args))
	   (res (car args)
		`(two-arg-+ ,res ,(car args))))
	  ((null args) res))))

(define-compiler-macro - (&whole form number &rest more-numbers)
  (declare (ignore form))
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result `(two-arg-- ,result ,(car nlist))))
      `(unary-minus ,number)))

(define-compiler-macro * (&whole form &rest args)
  (declare (ignore form))
  (if (null args)
      1
      (do ((args (cdr args) (cdr args))
	   (res (car args)
		`(two-arg-* ,res ,(car args))))
	  ((null args) res))))

(define-compiler-macro / (number &rest more-numbers)
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result `(two-arg-/ ,result ,(car nlist))))
      `(unary-divide ,number)))

(define-compiler-macro /= (&whole form number &rest more-numbers)
  ;; Convert (/= x y) to (not (two-arg-= x y)).  Should we try to
  ;; handle a few more cases?
  (if (cdr more-numbers)
      form
      `(not (two-arg-= ,number ,(car more-numbers)))))

;; Compiler macros to convert <, >, <=, and >= into multiple calls of
;; the corresponding two-arg-<foo> function.
(macrolet
    ((frob (op)
       (let ((method (intern (concatenate 'string
					  (string '#:two-arg-)
					  (symbol-name op)))))
	 `(define-compiler-macro ,op (number &rest more-numbers)
	    (do* ((n number (car nlist))
		  (nlist more-numbers (cdr nlist))
		  (res nil))
		 ((atom nlist) 
		  `(and ,@(nreverse res)))
	      (push `(,',method ,n ,(car nlist)) res))))))
  (frob <)
  (frob >)
  (frob <=)
  (frob >=))

(defmethod integer-decode-float ((x cl:float))
  (cl:integer-decode-float x))

(defmethod integer-decode-float ((x bigfloat))
  (let ((r (real-value x)))
    (values (abs (second r))
	    (- (third r) (third (first r)))
	    (signum (second r)))))

(defmethod decode-float ((x cl:float))
  (cl:decode-float x))

(defmethod decode-float ((x bigfloat))
  (let ((r (real-value x)))
    (values (make-instance 'bigfloat
			   :real (maxima::bcons (list (abs (second r)) 0)))
	    (third r)
	    (bigfloat (signum (second r))))))

;; GCL doesn't have a REAL class!
#+gcl
(progn
(defmethod float ((x cl:float) (y cl:float))
  (cl:float x y))

(defmethod float ((x cl:rational) (y cl:float))
  (cl:float x y))

(defmethod float ((x cl:float) (y bigfloat))
  (bigfloat x))

(defmethod float ((x cl:rational) (y bigfloat))
  (bigfloat x))
)

#-gcl
(progn
(defmethod float ((x real) (y cl:float))
  (cl:float x y))

(defmethod float ((x real) (y bigfloat))
  (bigfloat x))
)

;; Like Maxima's fp2flo, but for single-float numbers.
(defun fp2single (l)
  (let ((precision (caddar l))
	(mantissa (cadr l))
	(exponent (caddr l))
	(fpprec (float-digits 1f0))
	(maxima::*m 0))
    ;; Round the mantissa to the number of bits of precision of the
    ;; machine, and then convert it to a floating point fraction.  We
    ;; have 0.5 <= mantissa < 1
    (setq mantissa (maxima::quotient (maxima::fpround mantissa)
				     (expt 2f0 fpprec)))
    ;; Multiply the mantissa by the exponent portion.  I'm not sure
    ;; why the exponent computation is so complicated.
    ;;
    ;; GCL doesn't signal overflow from scale-float if the number
    ;; would overflow.  We have to do it this way.  0.5 <= mantissa <
    ;; 1.  The largest double-float is .999999 * 2^128.  So if the
    ;; exponent is 128 or higher, we have an overflow.
    (let ((e (+ exponent (- precision) maxima::*m fpprec)))
      (if (>= (abs e) 129)
	  (maxima::merror (intl:gettext "FP2SINGLE: floating point overflow converting ~:M to float.") l)
	  (cl:scale-float mantissa e)))))


(defmethod float ((x bigfloat) (y cl:float))
  (if (typep y 'maxima::flonum)
      (maxima::fp2flo (real-value x))
      (fp2single (real-value x))))

(defmethod random ((x cl:float) &optional (state cl:*random-state*))
  (cl:random x state))
(defmethod random ((x integer) &optional (state cl:*random-state*))
  (cl:random x state))

(defmethod random ((x bigfloat) &optional (state cl:*random-state*))
  ;; Generate an integer with fpprec bits, and convert to a bigfloat
  ;; by making the exponent 0.  Then multiply by the arg to get the
  ;; correct range.
  (if (plusp x)
      (let ((int (cl:random (ash 1 maxima::fpprec) state)))
	(* x (bigfloat (maxima::bcons (list int 0)))))
      (error "Argument is not a positive bigfloat: ~A~%" x)))

(defmethod signum ((x number))
  (cl:signum x))

(defmethod signum ((x bigfloat))
  (cond ((minusp x)
	 (bigfloat -1))
	((plusp x)
	 (bigfloat 1))
	(t
	 x)))

(defmethod signum ((x complex-bigfloat))
  (/ x (abs x)))

(defmethod float-sign ((x cl:float))
  (cl:float-sign x))

(defmethod float-sign ((x bigfloat))
  (if (minusp x)
      (bigfloat -1)
      (bigfloat 1)))

(defmethod float-digits ((x cl:float))
  (cl:float-digits x))

(defmethod float-digits ((x bigfloat))
  ;; Should we just return fpprec or should we get the actual number
  ;; of bits in the bigfloat number?  We choose the latter in case the
  ;; number and fpprec don't match.
  (let ((r (slot-value x 'real)))
    (third (first r))))

#-gcl
(defmethod rational ((x real))
  (cl:rational x))

#+gcl
(progn
(defmethod rational ((x cl:float))
  (cl:rational x))
(defmethod rational ((x cl:rational))
  (cl:rational x))
)

(defmethod rational ((x bigfloat))
  (destructuring-bind ((marker simp prec) mantissa exp)
      (real-value x)
    (declare (ignore marker simp))
    (* mantissa (expt 2 (- exp prec)))))

#-gcl
(defmethod rationalize ((x real))
  (cl:rationalize x))

#+gcl
(progn
(defmethod rationalize ((x cl:float))
  (cl:rationalize x))
(defmethod rationalize ((x cl:rational))
  (cl:rationalize x))
)


;;; This routine taken from CMUCL, which, in turn is a routine from
;;; CLISP, which is GPL.
;;;
;;; I (rtoy) have modified it from CMUCL so that it only handles bigfloats.
;;;
;;; RATIONALIZE  --  Public
;;;
;;; The algorithm here is the method described in CLISP.  Bruno Haible has
;;; graciously given permission to use this algorithm.  He says, "You can use
;;; it, if you present the following explanation of the algorithm."
;;;
;;; Algorithm (recursively presented):
;;;   If x is a rational number, return x.
;;;   If x = 0.0, return 0.
;;;   If x < 0.0, return (- (rationalize (- x))).
;;;   If x > 0.0:
;;;     Call (integer-decode-float x). It returns a m,e,s=1 (mantissa,
;;;     exponent, sign).
;;;     If m = 0 or e >= 0: return x = m*2^e.
;;;     Search a rational number between a = (m-1/2)*2^e and b = (m+1/2)*2^e
;;;     with smallest possible numerator and denominator.
;;;     Note 1: If m is a power of 2, we ought to take a = (m-1/4)*2^e.
;;;       But in this case the result will be x itself anyway, regardless of
;;;       the choice of a. Therefore we can simply ignore this case.
;;;     Note 2: At first, we need to consider the closed interval [a,b].
;;;       but since a and b have the denominator 2^(|e|+1) whereas x itself
;;;       has a denominator <= 2^|e|, we can restrict the seach to the open
;;;       interval (a,b).
;;;     So, for given a and b (0 < a < b) we are searching a rational number
;;;     y with a <= y <= b.
;;;     Recursive algorithm fraction_between(a,b):
;;;       c := (ceiling a)
;;;       if c < b
;;;         then return c       ; because a <= c < b, c integer
;;;         else
;;;           ; a is not integer (otherwise we would have had c = a < b)
;;;           k := c-1          ; k = floor(a), k < a < b <= k+1
;;;           return y = k + 1/fraction_between(1/(b-k), 1/(a-k))
;;;                             ; note 1 <= 1/(b-k) < 1/(a-k)
;;;
;;; You can see that we are actually computing a continued fraction expansion.
;;;
;;; Algorithm (iterative):
;;;   If x is rational, return x.
;;;   Call (integer-decode-float x). It returns a m,e,s (mantissa,
;;;     exponent, sign).
;;;   If m = 0 or e >= 0, return m*2^e*s. (This includes the case x = 0.0.)
;;;   Create rational numbers a := (2*m-1)*2^(e-1) and b := (2*m+1)*2^(e-1)
;;;   (positive and already in lowest terms because the denominator is a
;;;   power of two and the numerator is odd).
;;;   Start a continued fraction expansion
;;;     p[-1] := 0, p[0] := 1, q[-1] := 1, q[0] := 0, i := 0.
;;;   Loop
;;;     c := (ceiling a)
;;;     if c >= b
;;;       then k := c-1, partial_quotient(k), (a,b) := (1/(b-k),1/(a-k)),
;;;            goto Loop
;;;   finally partial_quotient(c).
;;;   Here partial_quotient(c) denotes the iteration
;;;     i := i+1, p[i] := c*p[i-1]+p[i-2], q[i] := c*q[i-1]+q[i-2].
;;;   At the end, return s * (p[i]/q[i]).
;;;   This rational number is already in lowest terms because
;;;   p[i]*q[i-1]-p[i-1]*q[i] = (-1)^i.
;;;
(defmethod rationalize ((x bigfloat))
  (multiple-value-bind (frac expo sign)
      (integer-decode-float x)
    (cond ((or (zerop frac) (>= expo 0))
	   (if (minusp sign)
	       (- (ash frac expo))
	       (ash frac expo)))
	  (t
	   ;; expo < 0 and (2*m-1) and (2*m+1) are coprime to 2^(1-e),
	   ;; so build the fraction up immediately, without having to do
	   ;; a gcd.
	   (let ((a (/ (- (* 2 frac) 1) (ash 1 (- 1 expo))))
		 (b (/ (+ (* 2 frac) 1) (ash 1 (- 1 expo))))
		 (p0 0)
		 (q0 1)
		 (p1 1)
		 (q1 0))
	     (do ((c (ceiling a) (ceiling a)))
		 ((< c b)
		  (let ((top (+ (* c p1) p0))
			(bot (+ (* c q1) q0)))
		    (/ (if (minusp sign)
			   (- top)
			   top)
		       bot)))
	       (let* ((k (- c 1))
		      (p2 (+ (* k p1) p0))
		      (q2 (+ (* k q1) q0)))
		 (psetf a (/ (- b k))
			b (/ (- a k)))
		 (setf p0 p1
		       q0 q1
		       p1 p2
		       q1 q2))))))))

(defun coerce (obj type)
  (flet ((coerce-error ()
	   (error "Cannot coerce ~A to type ~S" obj type)))
    (cond ((typep obj type)
	   obj)
	  ((subtypep type 'bigfloat)
	   ;; (coerce foo 'bigfloat).  Foo has to be a real
	   (cond ((typep obj 'real)
		  (bigfloat obj))
		 (t
		  (coerce-error))))
	  ((subtypep type 'complex-bigfloat)
	   ;; (coerce foo 'complex-bigfloat).  Foo has to be a real or complex
	   (cond ((typep obj 'real)
		  (bigfloat obj 0))
		 ((typep obj 'cl:complex)
		  (bigfloat obj))
		 ((typep obj 'bigfloat)
		  (bigfloat obj 0))
		 (t
		  (coerce-error))))
	  ((typep obj 'bigfloat)
	   ;; (coerce bigfloat foo)
	   (cond ((subtypep type 'cl:float)
		  (float obj (cl:coerce 0 type)))
		 ((subtypep type '(cl:complex long-float))
		  (cl:complex (float (realpart obj) 1l0)
			      (float (imagpart obj) 1l0)))
		 ((subtypep type '(cl:complex double-float))
		  (cl:complex (float (realpart obj) 1d0)
			      (float (imagpart obj) 1d0)))
		 ((subtypep type '(cl:complex single-float))
		  (cl:complex (float (realpart obj) 1f0)
			      (float (imagpart obj) 1f0)))
		 ((subtypep type 'cl:complex)
		  ;; What should we do here?  Return a
		  ;; complex-bigfloat?  A complex double-float?
		  ;; complex single-float?  I arbitrarily select
		  ;; complex maxima:flonum for now.
		  (cl:complex (float (realpart obj) 1.0)
			      (float (imagpart obj) 1.0)))
		 (t
		  (coerce-error))))
	  ((typep obj 'complex-bigfloat)
	   ;; (coerce complex-bigfloat foo)
	   (cond ((subtypep type 'complex-bigfloat)
		  obj)
		 ((subtypep type '(cl:complex long-float))
		  (cl:complex (float (realpart obj) 1l0)
			      (float (imagpart obj) 1l0)))
		 ((subtypep type '(cl:complex double-float))
		  (cl:complex (float (realpart obj) 1d0)
			      (float (imagpart obj) 1d0)))
		 ((subtypep type '(cl:complex single-float))
		  (cl:complex (float (realpart obj) 1f0)
			      (float (imagpart obj) 1f0)))
		 (t
		  (coerce-error))))
	  (t
	   (cl:coerce obj type)))))

;;; %PI - External
;;;
;;;   Return a value of pi with the same precision as the argument.
;;; For rationals, we return a single-float approximation.
(defmethod %pi ((x cl:rational))
  (declare (ignore x))
  (cl:coerce cl:pi 'single-float))

(defmethod %pi ((x cl:float))
  (cl:float cl:pi x))

(defmethod %pi ((x bigfloat))
  (declare (ignore x))
  (to (maxima::bcons (maxima::fppi))))

(defmethod %pi ((x cl:complex))
  (cl:float cl:pi (realpart x)))

(defmethod %pi ((x complex-bigfloat))
  (declare (ignore x))
  (to (maxima::bcons (maxima::fppi))))

;;; %e - External
;;;
;;;   Return a value of e with the same precision as the argument.
;;;   For rationals, we return a single-float approximation.
(defmethod %e ((x cl:rational))
  (declare (ignore x))
  (cl:coerce maxima::%e-val 'single-float))

(defmethod %e ((x cl:float))
  (cl:float maxima::%e-val x))

(defmethod %e ((x bigfloat))
  (declare (ignore x))
  (to (maxima::bcons (maxima::fpe))))

(defmethod %e ((x cl:complex))
  (cl:float maxima::%e-val (realpart x)))

(defmethod %e ((x complex-bigfloat))
  (declare (ignore x))
  (to (maxima::bcons (maxima::fpe))))

;;;; Useful routines

;;; Evaluation of continued fractions

(defvar *debug-cf-eval*
  nil
  "When true, enable some debugging prints when evaluating a
  continued fraction.")

;; Max number of iterations allowed when evaluating the continued
;; fraction.  When this is reached, we assume that the continued
;; fraction did not converge.
(defvar *max-cf-iterations*
  10000
  "Max number of iterations allowed when evaluating the continued
  fraction.  When this is reached, we assume that the continued
  fraction did not converge.")

;;; LENTZ - External
;;;
;;; Lentz's algorithm for evaluating continued fractions.
;;;
;;; Let the continued fraction be:
;;;
;;;      a1    a2    a3
;;; b0 + ----  ----  ----
;;;      b1 +  b2 +  b3 +
;;;
;;;
;;; Then LENTZ expects two functions, each taking a single fixnum
;;; index.  The first returns the b term and the second returns the a
;;; terms as above for a give n.
(defun lentz (bf af)
  (let ((tiny-value-count 0))
    (flet ((value-or-tiny (v)
	     ;; If v is zero, return a "tiny" number.
	     (if (zerop v)
		 (progn
		   (incf tiny-value-count)
		   (etypecase v
		     ((or double-float cl:complex)
		      (sqrt least-positive-normalized-double-float))
		     ((or bigfloat complex-bigfloat)
		      ;; What is a "tiny" bigfloat?  Bigfloats have
		      ;; unbounded exponents, so we need something
		      ;; small, but not zero.  Arbitrarily choose an
		      ;; exponent of 50 times the precision.
		      (expt 10 (- (* 50 maxima::$fpprec))))))
		 v)))
      (let* ((f (value-or-tiny (funcall bf 0)))
	     (c f)
	     (d 0)
	     (eps (epsilon f)))
	(loop
	   for j from 1 upto *max-cf-iterations*
	   for an = (funcall af j)
	   for bn = (funcall bf j)
	   do (progn
		(setf d (value-or-tiny (+ bn (* an d))))
		(setf c (value-or-tiny (+ bn (/ an c))))
		(when *debug-cf-eval*
		  (format t "~&j = ~d~%" j)
		  (format t "  an = ~s~%" an)
		  (format t "  bn = ~s~%" bn)
		  (format t "  c  = ~s~%" c)
		  (format t "  d  = ~s~%" d))
		(let ((delta (/ c d)))
		  (setf d (/ d))
		  (setf f (* f delta))
		  (when *debug-cf-eval*
		    (format t "  dl= ~S (|dl - 1| = ~S)~%" delta (abs (1- delta)))
		    (format t "  f = ~S~%" f))
		  (when (<= (abs (- delta 1)) eps)
		    (return-from lentz (values f j tiny-value-count)))))
	   finally
	     (error 'simple-error
		    :format-control "~<Continued fraction failed to converge after ~D iterations.~%    Delta = ~S~>"
		    :format-arguments (list *max-cf-iterations* (/ c d))))))))

;;; SUM-POWER-SERIES - External
;;;
;;;   SUM-POWER-SERIES sums the given power series, adding terms until
;;; the next term would not change the sum.
;;;
;;; The series to be summed is
;;;
;;;   S = 1 + sum(c[k]*x^k, k, 1, inf)
;;;     = 1 + sum(prod(f[n]*x, n, 1, k), k, 1, inf)
;;;
;;; where f[n] = c[n]/c[n-1].
;;;
(defun sum-power-series (x f)
  (let ((eps (epsilon x)))
    (do* ((k 1 (+ 1 k))
	  (sum 1 (+ sum term))
	  (term (* x (funcall f 1))
		(* term x (funcall f k))))
	 ((< (abs term) (* eps (abs sum)))
	  sum)
      #+nil
      (format t "~4d: ~S ~S ~S~%" k sum term (funcall f k)))))
