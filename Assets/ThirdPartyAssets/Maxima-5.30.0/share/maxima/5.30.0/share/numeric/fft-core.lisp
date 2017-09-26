;; -*- Lisp -*-

(in-package :maxima)

(defun mgetarray (marg) 
"Return the lisp array which is somehow attached to MARG."
  (or (and (symbolp marg) (symbol-array (mget marg 'array)))
      (and ($listp marg) (make-array ($length marg) :initial-contents (rest marg)))
      (and (arrayp marg) marg)))

(defun fft-arg-check (user-fcn-name ary)
  ;; I don't check here if this is really a floating point array.  For maxima
  ;; arrays which are symbols this would be no problem since the type is on
  ;; the property list.  On the other hand, for "fast" arrays (i.e. lisp
  ;; arrays), using ARRAY-ELEMENT-TYPE might not be too useful.
  (or (mgetarray ary)
      (merror "~M: argument must a list or array; instead found ~:M" user-fcn-name ary)))

(defun make-empty-copy (a)
  (cond
    (($listp a)
     (cons '(mlist) (make-list ($length a) :initial-element 0e0)))
    ((arrayp a)
     (make-array (length a) :initial-element 0e0))
    ((symbolp a)
     (meval
       `(($array)
         ,(intern (symbol-name (gensym "$G")))
         $float
         ,(1- (length (mgetarray a))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DATA CONVERSION FUNCTIONS for fft
;;
;; These convert various possible arguments for $fft:
;; 1. maxima list
;; 2. lisp array
;; 3. 'maxima array'
;; into two 'flonum' lisp arrays ('fft-arrays') holding
;; real and imaginary parts. After fft is done, these
;; two arrays are converted back into the same data type
;; the $fft function was given.
;;
;; Real and imaginary parts are extracted with:
;; (risplit ($float `maxima-expression'))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mlist->fft-arrays (mlist)
  "Converts a maxima list into two Lisp flonum
arrays - to be used by FFT algorithm"
  (let* ((lst (rest mlist))
         (N (length lst))
         (realparts (make-array N :element-type 'flonum))
         (imagparts (make-array N :element-type 'flonum)))
        (do ((element lst (rest element))
             (index 0 (1+ index)))
          ;; iteration end:
          ((>= index N) (list realparts imagparts)) ;; return arrays
          ;; do this every iteration
          (let ((fl (risplit ($float (first element)))))
            (setf (aref realparts index) (coerce (car fl) 'flonum))
            (setf (aref imagparts index) (coerce (cdr fl) 'flonum))))))

(defun lisp-array->fft-arrays (arr)
  (let* ((N (length arr))
         (realparts (make-array N :element-type 'flonum))
         (imagparts (make-array N :element-type 'flonum)))
    (dotimes (index N)
      (let ((fl (risplit ($float (aref arr index)))))
        (setf (aref realparts index) (coerce (car fl) 'flonum))
        (setf (aref imagparts index) (coerce (cdr fl) 'flonum))))
    (list realparts imagparts)))

;; Backwards data conversion (from fft arrays)

(defun fft-arrays->mlist (realparts imagparts)
  "Takes two Lisp arrays with real and imaginary
parts and returns a Maxima list of complex numbers
in Maxima's 'format'."
  (let (ans)
    (dotimes (i (length realparts))
      (push (add (aref realparts i) (mul (aref imagparts i) '$%i))
            ans))
    (cons '(mlist simp) (nreverse ans))))

(defun fft-arrays->lisp-array (realparts imagparts)
  "Outputs a Lisp array of Maxima expressions."
  (let ((ans (make-array (length realparts))))
    (dotimes (i (length realparts))
      (setf (aref ans i)
            (add (aref realparts i) (mul (aref imagparts i) '$%i))))
    ans))

(defun fft-arrays->maxima-symbol-array (realparts imagparts)
  "Outputs a Maxima array as does Maxima's 'array()' function."
  (let ((lisp-array (fft-arrays->lisp-array realparts imagparts))
        (maxima-symbol (meval `(($array)
                                ,(intern (symbol-name (gensym "$G")))
                                $float
                                ,(1- (length realparts))))))
    (setf (symbol-array (mget maxima-symbol 'array)) lisp-array)
    maxima-symbol))

;;
;; main function used by both fft() and inverse_fft()
;;
(defun fft+ifft-common (input lisp-function-to-call maxima-function-name)
  "This function checks the type of input argument,
does the apropriate conversion to `fft arrays', calls
the list function given and converts the result back
into the original datatype of `input'"
  (multiple-value-bind (convert reverse-convert)
      ;; set the conversion functions
      (cond (($listp input)
	     (values #'mlist->fft-arrays
		     #'fft-arrays->mlist))
	    ((arrayp input)
	     (values  #'lisp-array->fft-arrays
		      #'fft-arrays->lisp-array))
	    ((and (symbolp input) (symbol-array (mget input 'array)))
	     (values #'(lambda (x)
			 (lisp-array->fft-arrays
			  (symbol-array (mget x 'array))))
		     #'fft-arrays->maxima-symbol-array))
	    (t
	     (merror "~A: input is not a list or an array." maxima-function-name)))
    (multiple-value-bind (realparts imagparts)
	;; perform fft or inverse fft
	(apply lisp-function-to-call (funcall convert input))
      ;; return the same data type as was the input
      (funcall reverse-convert realparts imagparts))))

;;
;; Maxima functions fft() and inverse_fft()
;;

(defun $fft (input)
  (fft+ifft-common input #'forward-fft "fft"))

(defun $inverse_fft (input)
  (fft+ifft-common input #'inverse-fft "inverse_fft"))

;; These functions perhaps need revision if they are
;; needed at all. Output ignores the type of input.
(defun $recttopolar (rary iary)
  (let ((fast-rary (fft-arg-check '$recttopolar rary))
	(fast-iary (fft-arg-check '$recttopolar iary)))
    (complex-to-polar fast-rary fast-iary)
    (list '(mlist) rary iary)))

(defun $polartorect (rary iary)
  (let ((fast-rary (fft-arg-check '$polartorect rary))
	(fast-iary (fft-arg-check '$polartorect iary)))
    (polar-to-complex fast-rary fast-iary)
    (list '(mlist) rary iary)))

(defun complex-to-polar (re im)
  (dotimes (k (min (length re) (length im)))
    (let ((rp (aref re k))
	  (ip (aref im k)))
      (setf (aref re k) (abs (complex rp ip)))
      (setf (aref im k) (atan ip rp)))))

(defun polar-to-complex (mag phase)
  (dotimes (k (min (length mag) (length phase)))
    (let ((r (aref mag k))
	  (p (aref phase k)))
      (setf (aref mag k) (* r (cos p)))
      (setf (aref phase k) (* r (sin p))))))

;;; FFT written by Raymond Toy, based on the Fortran version in
;;; Oppenheim and Schaffer.  The original version used an array of
;;; complex numbers, but this causes quite a bit of consing if your
;;; Lisp doesn't handle them well.  (CMUCL does handle complex numbers
;;; well.)  So, take two separate arrays, one for the real part and
;;; one for the imaginary part.

(defun log-base2 (n)
  (declare (type (and fixnum (integer 1)) n)
	   (optimize (speed 3)))
  ;; Just find m such that 2^m <= n < 2^(m+1).  It's up to the caller
  ;; to make sure that n is a power of two.  (The previous
  ;; implementation using (/ (log n) (log 2)) has roundoff errors.
  ;; This doesn't.)
  (1- (integer-length n)))

(defvar *sincos-tables*
  (make-hash-table) 
  "Hash table mapping log2 of the FFT size to an
  array of exp(2*pi*i/N), where N is the FFT size.")

(defun sincos-table (m)
  (cond ((gethash m *sincos-tables*))
	(t
	 ;; Need to create the sincos table.  Only need to have a half
	 ;; period.
	 (let* ((n (ash 1 (1- m)))
		(p (/ #+(and cmu flonum-double-double) kernel:dd-pi
		      #-flonum-double-double (coerce pi 'double-float)
		      n))
		(table (make-array n :element-type
				   #+(and cmu flonum-double-double) '(complex double-double-float)
				   #-flonum-double-double '(complex double-float))))
	   (dotimes (k n)
	     (setf (aref table k) (cis (* k p))))
	   ;; Make the half point exactly correct
	   (when (> n 1)
	     (setf (aref table (ash n -1))
		   (coerce #c(0 1) #+(and cmu flonum-double-double) '(complex double-double-float)
			   #-flonum-double-double '(complex double-float))))
	   (setf (gethash m *sincos-tables*) table)
	   table))))

;; Warning: What this routine thinks is the foward or inverse
;; direction is the "engineering" definition used in Oppenheim and
;; Schafer.  This is usually the opposite of the definitions used in
;; math, and is the opposite used by maxima.
;;
;; Scaling and bit-reversed ordering is not done by this routine.
(defun fft-dif-internal (vec-r vec-i &optional direction)
  "Internal FFT routine for decimation-in-frequency FFT
fft-dif-internal (vec &optional direction)

	vec		-- simple-array of elements.
	direction	-- NIL for forward, non-NIL for inverse
			   Default is forward.

The result is returned in vec."
  (declare (type (simple-array flonum (*)) vec-r vec-i)
	   (optimize speed))
  (let* ((size (length vec-r))
	 (le size)
	 (m (log-base2 le))
	 (sincos (sincos-table m))
	 (dir (if direction 1 -1)))
    (declare (fixnum size le)
	     (type (simple-array #+(and cmu flonum-double-double) (complex double-double-float)
				 #-flonum-double-double (complex double-float)
				 (*))
		   sincos))
    (unless (= size (ash 1 m))
      (merror "fft: size of array must be a power of 2; found: ~:M" size))
    (loop for level of-type fixnum from 0 below m
	  for repetition of-type fixnum = 1 then (ash repetition 1)
	  do
	  (let* ((le1 (truncate le 2)))
	    (declare (type fixnum le1))
	    (loop for j of-type fixnum from 0 below le1
	       for phase of-type fixnum from 0 by repetition
	       do
	       (let* ((u (aref sincos phase))
		      (u-r (realpart u))
		      (u-i (* dir (imagpart u))))
		 (declare (type #+(and cmu flonum-double-double) (complex double-double-float)
				#-flonum-double-double (complex double-float)
				u)
			  (type flonum u-r u-i))
		 (loop for k of-type fixnum from j below size by le
		    do
		    (let* ((kp (+ k le1))
			   (tmp-r (+ (aref vec-r k) (aref vec-r kp)))
			   (tmp-i (+ (aref vec-i k) (aref vec-i kp)))
			   (diff-r (- (aref vec-r k) (aref vec-r kp)))
			   (diff-i (- (aref vec-i k) (aref vec-i kp))))
		      (declare (fixnum kp)
			       (type flonum tmp-r tmp-i))
		      (psetf (aref vec-r kp) (- (* u-r diff-r) (* u-i diff-i))
			     (aref vec-i kp) (+ (* u-r diff-i) (* u-i diff-r)))
		      (setf (aref vec-r k) tmp-r)
		      (setf (aref vec-i k) tmp-i)))))
	    (setq le le1))))
  (values vec-r vec-i))

(defun fft-bit-reverse (vec-r vec-i)
  "fft-bit-reverse (vec)

Reorder vec in bit-reversed order.  The length of vec
must be a power of 2."
  (declare (type (simple-array flonum (*)) vec-r vec-i)
	   (optimize speed))
  (let* ((size (length vec-r))
	 (n/2 (/ size 2))
	 (j 0)
	 (k 0))
    (declare (type fixnum size n/2 j k))
    (dotimes (i (- size 1))
      (declare (fixnum i))
      (when (< i j)
	(rotatef (aref vec-r i) (aref vec-r j))
	(rotatef (aref vec-i i) (aref vec-i j)))
      (setq k n/2)
      (do* ()
	  ((> k j))
	(setq j (- j k))
	(setq k (ash k -1)))
      (setq j (+ j k)))))

(defun forward-fft (x-real x-imag)
  "forward-fft
Takes two lisp arrays, one for real parts
and the other for imaginary parts.
Returns transformed real and imaginary arrays.
A normalisation is performed."
  (let ((size (length x-real)))
    (when (> size 1)
      (fft-dif-internal x-real x-imag t)
      (fft-bit-reverse x-real x-imag)
      (let ((1/N (/ (coerce 1 'flonum) size)))
        (dotimes (k size) (setf (aref x-real k) (* (aref x-real k) 1/N)))
        (dotimes (k size) (setf (aref x-imag k) (* (aref x-imag k) 1/N)))))
    (values x-real x-imag)))

(defun inverse-fft (x-real x-imag)
  "inverse-fft
Takes two lisp arrays, one for real parts
and the other for imaginary parts.
Returns transformed real and imaginary arrays."
  (let ((size (length x-real)))
    (when (> size 1)
      (fft-dif-internal x-real x-imag nil)
      (fft-bit-reverse x-real x-imag)))
  (values x-real x-imag))
