;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defun memq (x lis)
  (member x lis :test #'eq))

;;this will make operators which declare the type and result of numerical operations
(eval-when (:compile-toplevel :load-toplevel)

  (defmacro def-op (name arg-type op &optional return-type)
    `(setf (macro-function ',name)
           (make-operation ',arg-type ',op ',return-type)))

  ;;make very sure .type .op and .return are not special!!
  (defun make-operation (.type .op .return)
    (or .return (setf .return .type))
    #'(lambda (bod env)
        (declare (ignore env))
        (loop for v in (cdr bod)
           when (eq t .type) collect v into body
           else
           collect `(the , .type ,v) into body
           finally (setq body `(, .op ,@body))
	     (return
	       (if (eq t .return)
		   body
		   `(the , .return ,body))))))

  ;; these allow running of code and they print out where the error occurred
  #+fix-debug
  (progn
    (defvar *dbreak* t)

    (defun chk-type (lis na typ sho)
      (unless (every #'(lambda (v) (typep v typ)) lis)
        (format t "~%Bad call ~a types:~a" (cons na sho) (mapcar #'type-of lis))
        (when *dbreak*
          (break "hi"))))

    (defmacro def-op (name arg-type old)
      `(defmacro ,name (&rest l)
         `(progn
            (chk-type (list ,@l) ',',name ',',arg-type ',l)
            (,',old ,@l)))))

  (def-op f+ fixnum +)
  (def-op f* fixnum *)
  (def-op f- fixnum -)
  (def-op f1- fixnum 1-)
  (def-op f1+ fixnum 1+)
  (def-op quotient t quot))

;;this is essentially what the quotient is supposed to do.

(declaim (inline quot))
(defun quot (a b)
  (if (and (integerp a) (integerp b))
      (truncate a b)
      (/ a b)))

(defmacro status (option &optional item)
  (cond ((equal (symbol-name option) (symbol-name '#:feature))
	 `(member ,(intern (string item) (find-package 'keyword)) *features*))
	((equal option 'gctime) 0)))

#+(or scl allegro)
(defun string<$ (str1 str2)
  "Compare string, but flip the case for maxima variable names to maintain
  the same order irrespective of the lisp case mode."
  (declare (string str1 str2))
  (cond (#+scl (eq ext:*case-mode* :lower)
	 #+allegro (eq excl:*current-case-mode* :case-sensitive-lower)
	 (let ((str1l (length str1))
	       (str2l (length str2)))
	   (cond ((and (> str1l 1) (char= (aref str1 0) #\$)
		       (> str2l 1) (char= (aref str2 0) #\$))
		  (flet ((case-flip (str)
			   (let ((some-upper nil)
				 (some-lower nil))
			     (dotimes (i (length str))
			       (let ((ch (schar str i)))
				 (when (lower-case-p ch)
				   (setf some-lower t))
				 (when (upper-case-p ch)
				   (setf some-upper t))))
			     (cond ((and some-upper some-lower)
				    nil)
				   (some-upper
				    :downcase)
				   (some-lower
				    :upcase)))))
		    (let ((flip1 (case-flip str1))
			  (flip2 (case-flip str2)))
		      (do ((index 1 (1+ index)))
			  ((or (>= index str1l) (>= index str2l))
			   (if (= index str1l) index nil))
			(let ((ch1 (aref str1 index))
			      (ch2 (aref str2 index)))
			  (cond ((and (eq flip1 :downcase) (both-case-p ch1))
				 (setf ch1 (char-downcase ch1)))
				((and (eq flip1 :upcase) (both-case-p ch1))
				 (setf ch1 (char-upcase ch1))))
			  (cond ((and (eq flip2 :downcase) (both-case-p ch2))
				 (setf ch2 (char-downcase ch2)))
				((and (eq flip2 :upcase) (both-case-p ch2))
				 (setf ch2 (char-upcase ch2))))
			  (unless (char= ch1 ch2)
			    (return (if (char< ch1 ch2)
					index
					nil))))))))
		 (t
		  (string< str1 str2)))))
	(t
	 (string< str1 str2))))
;;;
#-(or scl allegro)
(defun string<$ (str1 str2)
  (string< str1 str2))

;;numbers<strings<symbols<lists<?
(defun alphalessp (x y)
  (cond ((numberp x)
	 (if (numberp y) (< x y) t))
	((stringp x)
	 (cond ((numberp y) nil)
	       ((stringp y)
		(string< x y))
	       (t t)))
	((symbolp x)
	 (cond ((or (numberp y) (stringp y)) nil)
	       ((symbolp y)
		(let ((nx (symbol-name x))
		      (ny (symbol-name y)))
		  (declare (string nx ny))
		  (cond ((string<$ nx ny)
			 t)
			((string= nx ny)
			 (cond ((eq nx ny) nil)
			       ((null (symbol-package x)) nil)
			       ((null (symbol-package y)) nil)
			       (t (string<
				   (package-name (symbol-package x))
				   (package-name (symbol-package y))))))
			(t nil))))
	       ((consp y) t)))
	((listp x)
	 (cond ((or (numberp y) (stringp y)(symbolp y )) nil)
	       ((listp y)
		(or (alphalessp (car x) (car y))
		    (and (equal (car x) (car y))
			 (alphalessp (cdr x) (cdr y)))))
	       (t nil)))
	((or (numberp y) (stringp y) (symbolp y)(consp y))
	 nil)
	(t				;neither is of known type:
	 (alphalessp (format nil "~s" x)(format nil "~s" y)))))

(defmacro symbol-array (sym)
  `(get ,sym 'array))

(defun arraydims (ar)
  (when (symbolp ar)
    (setq ar (symbol-array ar)))
  (cons (array-element-type ar) (array-dimensions ar)))

(defun firstn (n lis)
  (subseq lis 0 n))

(defun fixnump (n)
  (typep n 'fixnum))

(defun  bignump (x)
  (typep x 'bignum))

;;actually this was for lists too.

(defun putprop (sym val  indic)
  (if (consp sym)
      (setf (getf (cdr sym) indic) val)
      (setf (get sym indic) val)))

(defmacro defprop (sym val indic)
  (if (eq indic 'expr)
      `(setf (symbol-function ',sym) #',val)
      `(setf (get ',sym ',indic) ',val)))

;; Find the N most significant or least significant bits of the
;; absolute value of X.  If N is positive, take the most significant;
;; otherwise, the least significant.
(defun haipart (x n)
  (let ((x (abs x)))
    (if (< n 0)
	;; If the desired number of bits is larger than the actual
	;; number, just return the number.  (Prevents gratuitously
	;; generating a huge bignum if n is very large, as can happen
	;; with bigfloats.)
	(if (< (integer-length x) (- n))
	    x
	    (logand x (1- (ash 1 (- n)))))
	(ash x (min (- n (integer-length x)) 0)))))

;; also correct but slower.
;;(defun haipart (integer count)
;;  (let ((x (abs integer)))
;;    (if (minusp count)
;;      (ldb (byte (- count) 0) x)
;;      (ldb (byte count (max 0 (- (integer-length x) count))) x))))

;;used in translation
(defun fset (sym val)
  (setf (symbol-function sym) val))

(defun oldget (plist indic)
  (cond ((symbolp plist)
	 (setq plist (symbol-plist plist)))
	((consp plist) (setq plist (cdr plist)))
	(t (return-from oldget nil)))
  (loop for tail on plist by #'cddr
	 when (eq (car tail) indic)
	 do (return (second tail))))

(defun safe-get (sym prop)
  (and (symbolp sym) (get sym prop)))

(defmacro safe-getl (sym prop)
  `(and (symbolp ,sym) (getl ,sym ,prop)))

(defun getl (plist indicator-list )
  (cond ((symbolp plist)
	 (setq plist (symbol-plist plist)))
	((consp plist) (setq plist (cdr plist)))
	(t (return-from getl nil)))
  (loop for tail on plist by #'cddr
	 when (member (car tail) indicator-list :test #'eq)
	 do (return tail)))

(defmacro ncons (x)
  `(cons ,x nil)) ;;can one optimize this??

(defvar *acursor* (make-array 11 :element-type 'fixnum :initial-element 0))

;; Format of *acursor*.
;; 0                 1  2  3  4  5    6  7  8  9  10
;; dim               i1 i2 i3 i4 i5   d1 d2 d3 d4 d5
;; array dimension   current index    maximal index

(defun set-up-cursor (ar)
  (let ((lis (array-dimensions ar)))
    (setf (aref *acursor* 0) (length lis))
    (loop for v in lis for i from 6 do (setf (aref *acursor* i) (1- v)))
    (loop for i from 1 to (length lis) do (setf (aref *acursor* i) 0))))

(defun aset-by-cursor (ar  val)
  (let ((curs  *acursor*))
    (declare (type (simple-array fixnum (11)) curs))
    (ecase (aref curs 0)
      (1 (setf (aref ar (aref curs 1)) val))
      (2 (setf (aref ar (aref curs 1) (aref curs 2)) val))
      (3 (setf (aref ar (aref curs 1) (aref curs 2) (aref curs 3)) val))
      (4 (setf (aref ar (aref curs 1) (aref curs 2) (aref curs 3)
		     (aref curs 4)) val))
      (5 (setf (aref ar (aref curs 1) (aref curs 2) (aref curs 3)
		     (aref curs 4) (aref curs 5)) val)))
    ;; set the index (`cursor') for the next call to ASET-BY-CURSOR
    (loop for j downfrom (aref curs 0)
	   do (cond ((< (aref curs j) (aref curs (+ 5 j)))
		     (setf (aref curs j) (+  (aref curs j) 1))
		     (return-from aset-by-cursor t))
		    (t (setf (aref curs j) 0)))
	   (cond ((eql j 0) (return-from aset-by-cursor nil))))))

(defun fillarray (ar x)
  (when (symbolp ar)
    (setq ar (get ar 'array)))
  (when (/= (array-rank ar) 1)
    (setq ar (make-array (array-total-size ar) :displaced-to ar)))
  (setq x (cond ((null x)
		 (ecase (array-element-type ar)
		   (fixnum '(0))
		   (float '(0.0))
		   ((t) '(nil))))
		((arrayp x)(listarray x))
		((atom x) (list x))
		(t x)))
  (when (> (length ar) 0)
    (set-up-cursor ar)
    (loop while (aset-by-cursor ar (car x))
       do (and (cdr x) (setq x (cdr x))))))

(defun listarray (x)
  (when (symbolp x)
    (setq x (get x 'array)))
  (if (eql (array-rank x) 1)
      (coerce x 'list)
      (coerce (make-array (apply '* (array-dimensions x)) :displaced-to x
			  :element-type (array-element-type x))
	      'list)))

(defmacro check-arg (place pred &rest res)
  (when (atom pred)
    (setq pred (list pred place)))
  `(assert ,pred (,place) ,@res))

(defmacro deff (fun val)
  `(setf (symbol-function ',fun) ,val))

(defmacro xcons (x y)
  (cond ((atom x) `(cons ,y,x))
	(t (let ((g (gensym)))
	     `(let ((,g ,x))
	       (cons ,y ,g))))))

(defun make-equal-hash-table (not-dim1)
  (let ((table (make-hash-table :test 'equal)))
    (or not-dim1 (setf (gethash 'dim1 table) t))
    table))

;;; Range of atan should be [0,2*pi]
(defun atan (y x)
  (let ((tem (cl:atan y x)))
    (if (>= tem 0)
	tem
	(+ tem (* 2 pi)))))

;;; Range of atan2 should be (-pi,pi]
;;; CL manual says that's what lisp::atan is supposed to have.
(deff atan2 #'cl:atan)

;;; exp is shadowed to save trouble for other packages--its declared special
(deff exp #'cl:exp)

#+clisp
(progn
  ;; This used to be enabled, but
  ;; http://clisp.cons.org/impnotes/num-dict.html seems to indicate
  ;; that the result of float, coerce, sqrt, etc., on a rational will
  ;; return a float of the specified type.  But ANSI CL says we must
  ;; return a single-float.  I (rtoy) am commenting this out for now.

  ;; (setq custom:*default-float-format* 'double-float)

  ;; We currently don't want any warnings about floating-point contagion.
  (setq custom::*warn-on-floating-point-contagion* nil)

  ;; We definitely want ANSI-style floating-point contagion.
  (setq custom:*floating-point-contagion-ansi* t)

  ;; Set custom:*floating-point-rational-contagion-ansi* so that
  ;; contagion is done as per the ANSI CL standard. Has an effect only
  ;; in those few cases when the mathematical result is exact although
  ;; one of the arguments is a floating-point number, such as (* 0
  ;; 1.618), (/ 0 1.618), (atan 0 1.0), (expt 2.0 0)
  (setq custom:*floating-point-rational-contagion-ansi* t)

  ;; When building maxima using with 'flonum being a 'long-float it may be
  ;; useful to adjust the number of bits of precision that CLISP uses for
  ;; long-floats.
  #+nil
  (setf (ext:long-float-digits) 128)

  ;; We want underflows not to signal errors.
  (ext:without-package-lock ()
    (setq sys::*inhibit-floating-point-underflow* t))
  )

#+abcl
(progn
  ;; We want underflows not to signal errors
  (when (fboundp (find-symbol "FLOAT-UNDERFLOW-MODE" "SYS"))
    (funcall (find-symbol "FLOAT-UNDERFLOW-MODE" "SYS") nil))
  )

;; Make the maximum exponent larger for CMUCL.  Without this, cmucl
;; will generate a continuable error when raising an integer to a
;; power greater than this.
#+cmu
(setf ext::*intexp-maximum-exponent* 100000)
;;;; Setup the mapping from the Maxima 'flonum float type to a CL float type.
;;;;
;;;; Add :flonum-long to *features* if you want flonum to be a
;;;; long-float.  Or add :flonum-double-double if you want flonum to
;;;; be a double-double (currently only for CMUCL).  Otherwise, you
;;;; get double-float as the flonum type.
;;;;
;;;; Default double-float flonum.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *read-default-float-format* 'double-float))

#-(or flonum-long flonum-double-double)
(progn
;; Tell Lisp the float type for a 'flonum.
#-clisp
(deftype flonum (&optional low high)
  (cond (high
	 `(double-float ,low ,high))
	(low
	 `(double-float ,low))
	(t
	 'double-float)))

;; Some versions of clisp appear to be buggy: (coerce 1 'flonum)
;; signals an error.  So does (coerce 1 '(double-float 0d0)).  But
;; (coerce 1 'double-float) returns 1d0 as expected.  So for now, make
;; flonum be exactly the same as double-float, without bounds.
#+clisp
(deftype flonum (&optional low high)
  (declare (ignorable low high))
  'double-float)

(defconstant most-positive-flonum most-positive-double-float)
(defconstant most-negative-flonum most-negative-double-float)
(defconstant least-positive-flonum least-positive-double-float)
(defconstant least-negative-flonum least-negative-double-float)
(defconstant flonum-epsilon double-float-epsilon)
(defconstant least-positive-normalized-flonum least-positive-normalized-double-float)

(defconstant flonum-exponent-marker #\D)
)

#+flonum-long
(progn
;;;; The Maxima 'flonum can be a CL 'long-float on the Scieneer CL or CLISP,
;;;; but should be the same as 'double-float on other CL implementations.

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *read-default-float-format* 'long-float))

;; Tell Lisp the float type for a 'flonum.
(deftype flonum (&optional low high)
  (cond (high
	 `(long-float ,low ,high))
	(low
	 `(long-float ,low))
	(t
	 'long-float)))

(defconstant most-positive-flonum most-positive-long-float)
(defconstant most-negative-flonum most-negative-long-float)
(defconstant least-positive-flonum least-positive-long-float)
(defconstant least-negative-flonum least-negative-long-float)
(defconstant flonum-epsilon long-float-epsilon)
(defconstant least-positive-normalized-flonum least-positive-normalized-long-float)

(defconstant flonum-exponent-marker #\L)

)

#+flonum-double-double
(progn

;;;; The Maxima 'flonum can be a 'kernel:double-double-float on the CMU CL.

  (eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *read-default-float-format* 'kernel:double-double-float))

;; Tell Lisp the float type for a 'flonum.
(deftype flonum (&optional low high)
  (cond (high
	 `(kernel:double-double-float ,low ,high))
	(low
	 `(kernel:double-double-float ,low))
	(t
	 'kernel:double-double-float)))

;; While double-double can represent number as up to
;; most-positive-double-float, it can't really do operations on them
;; due to the way multiplication and division are implemented.  (I
;; don't think there's any workaround for that.)
;;
;; So, the largest number that can be used is the float just less than
;; 2^1024/(1+2^27).  This is the number given here.
(defconstant most-positive-double-double-hi
  (scale-float (cl:float (1- 9007199187632128) 1d0) 944))

(defconstant most-positive-flonum (cl:float most-positive-double-double-hi 1w0))
(defconstant most-negative-flonum (cl:float (- most-positive-double-double-hi 1w0)))
(defconstant least-positive-flonum (cl:float least-positive-double-float 1w0))
(defconstant least-negative-flonum (cl:float least-negative-double-float 1w0))
;; This is an approximation to a double-double epsilon.  Due to the
;; way double-doubles are represented, epsilon is actually zero
;; because 1+x = 1 only when x is zero.  But double-doubles only have
;; 106 bits of precision, so we use that as epsilon.
(defconstant flonum-epsilon (scale-float 1w0 -106))
(defconstant least-positive-normalized-flonum (cl:float least-positive-normalized-double-float 1w0))

(defconstant flonum-exponent-marker #\W)

)

;;;;
(defmacro float (x &optional (y 1e0))
  `(cl:float ,x ,y))

(defmacro with-collector (collector-sym &body forms)
  (let ((acc (gensym)))
    `(let ((,acc))
       (flet ((,collector-sym (x) (push x ,acc)))
         ,@forms
         (nreverse ,acc)))))

;; DO-MERGE-ASYM moved here from nset.lisp so that it is defined before
;; it is referenced in compar.lisp.
(defmacro do-merge-symm (list1 list2 eqfun lessfun bothfun onefun)
  ;; Like do-merge-asym, but calls onefun if an element appears in one but
  ;; not the other list, regardless of which list it appears in.
  `(do-merge-asym ,list1 ,list2 ,eqfun ,lessfun ,bothfun ,onefun ,onefun))

(defmacro do-merge-asym
  (list1 list2 eqfun lessfun bothfun only1fun only2fun)
  ;; Takes two lists.
  ;; The element equality function is eqfun, and they must be sorted by lessfun.
  ;; Calls bothfun on each element that is shared by the two lists;
  ;; calls only1fun on each element that appears only in the first list;
  ;; calls only2fun on each element that appears only in the second list.
  ;; If both/only1/only2 fun are nil, treat as no-op.
  (let ((l1var (gensym))
	(l2var (gensym)))
    `(do ((,l1var ,list1)
	  (,l2var ,list2))
	 ((cond ((null ,l1var)
		 (if ,only2fun
		     (while ,l2var
		       (funcall ,only2fun (car ,l2var))
		       (setq ,l2var (cdr ,l2var))))
		 t)
		((null ,l2var)
		 (if ,only1fun
		     (while ,l1var
		       (funcall ,only1fun (car ,l1var))
		       (setq ,l1var (cdr ,l1var))))
		 t)
		((funcall ,eqfun (car ,l1var) (car ,l2var))
		 (if ,bothfun (funcall ,bothfun (car ,l1var)))
		 (setq ,l1var (cdr ,l1var) ,l2var (cdr ,l2var))
		 nil)
		((funcall ,lessfun (car ,l1var) (car ,l2var))
		 (if ,only1fun (funcall ,only1fun (car ,l1var)))
		 (setq ,l1var (cdr ,l1var))
		 nil)
		(t
		 (if ,only2fun (funcall ,only2fun (car ,l2var)))
		 (setq ,l2var (cdr ,l2var))
		 nil))))))

;;; Test
; (do-merge-asym '(a a a b c g h k l)
; 	       '(a b b c c h i j k k)
; 	       'eq
; 	       'string<
; 	       '(lambda (x) (prin0 'both x))
; 	       '(lambda (x) (prin0 'one1 x))
; 	       '(lambda (x) (prin0 'one2 x)))
; both a
; one1 a
; one1 a
; both b
; one2 b
; both c
; one2 c
; one1 g
; both h
; one2 i
; one2 j
; both k
; one2 k
; one1 l
; nil
