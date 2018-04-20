;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module ar)

(declare-top (special evarrp munbound flounbound fixunbound $use_fast_arrays))

(defstruct (mgenarray (:conc-name mgenarray-))
  aref
  aset
  type
  null
  generator
  content)

(defun marray-type (x)
  (cond ((arrayp x) 'array)
        ((hash-table-p x) 'hash-table)
        ((eq (type-of x) 'mgenarray) (mgenarray-type x))))

(defun $make_array (type &rest diml)
  (let ((ltype (assoc type '(($float . flonum)
                             ($flonum . flonum)
                             ($fixnum . fixnum)))))
    ;; Check the dimensions. No check for number of dimensions.
    (when (member nil
                  (mapcar #'(lambda (u) (fixnump u))
                          ;; For a functional array the list of dimensions
                          ;; starts at the third element of the list diml
                          (if (eq type '$functional)
                              (cddr diml)
                              diml))
                  :test #'eq)
      (merror (intl:gettext "make_array: dimensions must be integers; found ~M")
              `((mlist) ,@diml)))
    (if (not ltype)
	(case type
	  ($any
	   (make-array diml :initial-element nil))
          ($hashed
           (make-equal-hash-table (cdr diml)))
	  ($functional
           ;; MAKE_ARRAY('FUNCTIONAL, LAMBDA(...), 'ARRAY_TYPE, ...)
           ;; This is a memoizing array.
	   (unless (> (length diml) 1)
	     (merror (intl:gettext "make_array: not enough arguments for functional array specification.")))
	   (let ((ar (apply #'$make_array (cadr diml) (cddr diml)))
	         (the-null))
	     (case (cadr diml)
	       ($fixnum
	         (fillarray ar (list (setq the-null fixunbound))))
	       (($flonum $float)
	        (fillarray ar (list (setq the-null flounbound))))
	       ($any
	        (fillarray ar (list (setq the-null munbound))))
	       (t
	        ;; Nothing to do for hashed arrays. Is FUNCTIONAL here an error?
	        ;; No, it is the most useful case for a FUNCTIONAL array.
	        (setq the-null nil)))
	     (make-mgenarray :type '$functional
	                     :content ar
	                     :generator (car diml)
	                     :null the-null)))
	  (t
	   (merror (intl:gettext "make_array: array type ~M not recognized.")
	           type)))
	(make-array diml :initial-element (case (cdr ltype)
					    (fixnum 0)
					    (flonum 0.0)
					    (otherwise nil))))))

(defun dimension-array-object (form result)
  (let ((mtype (marray-type form)))
    (if (eq mtype '$functional)
        (dimension-array-object (mgenarray-content form) result)
        (dimension-atom (format nil "{Lisp Array: ~A}" form) result))))

(defun msize-array-object (x l r)
  (let ((mtype (marray-type x)))
    (if (eq mtype '$functional)
        (msize-array-object (mgenarray-content x) l r)
        (msize-atom (format nil "{Lisp Array: ~A}" x) l r))))

(defun marray-check (a)
  (if (eq (ml-typep a) 'array)
      (case (marray-type a)
	(($fixnum $float art-q) a)
	(($any) (mgenarray-content a))
	(($hashed $functional)
	 ;; BUG: It does have a number of dimensions! Gosh. -GJC
	 (merror (intl:gettext "MARRAY-CHECK: hashed array ~M has no dimension data.") a))
	(t
	 (marray-type-unknown a)))
      (merror (intl:gettext "MARRAY-CHECK: not an array: ~M") a)))

(defmfun $array_dimension_n (n a)
  (array-dimension (marray-check a) n))

(defun marray-type-unknown (x)
  (merror (intl:gettext "MARRAY-TYPE-UNKNOWN: array type ~S not recognized.")
          x))

(defun marrayref-gensub (aarray ind1 inds)
  (case (marray-type aarray)
    ;; We are using a CASE on the TYPE instead of a FUNCALL, (or SUBRCALL)
    ;; because we are losers. All this stuff uses too many functions from
    ;; the "MLISP" modual, which are not really suitable for the kind of
    ;; speed and simplicity we want anyway. Ah me. Also, passing the single
    ;; unconsed index IND1 around is a dubious optimization, which causes
    ;; extra consing in the case of hashed arrays.
    ((array) (apply #'aref aarray ind1 inds))
    ((hash-table) (gethash (if inds (cons ind1 inds) ind1) aarray))
    (($functional)
     (let ((value (let ((evarrp t))
		    ;; special variable changes behavior of hashed-array
		    ;; referencing functions in case of not finding an element.
		    (catch 'evarrp (marrayref-gensub
		                    (mgenarray-content aarray) ind1 inds)))))
       (if (equal value (mgenarray-null aarray))
	   (marrayset-gensub  (apply #'mfuncall
				     (mgenarray-generator aarray)
				     ;; the first argument we pass the
				     ;; function is a SELF variable.
;				     aarray
				     ;; extra consing here! LEXPR madness.
				     ind1
				     inds)
			      (mgenarray-content aarray) ind1 inds)
	   value)))
    (t
     (marray-type-unknown aarray))))

(defun marrayset-gensub (val aarray ind1 inds)
  (case (marray-type aarray)
    ((array) (setf (apply #'aref aarray ind1 inds) val))
    ((hash-table) (setf (gethash (if inds (cons ind1 inds) ind1) aarray) val))
    (($functional)
     (marrayset-gensub val (mgenarray-content aarray) ind1 inds))
    (t
     (marray-type-unknown aarray))))

;; Extensions to MEVAL.

(defmfun meval1-extend (form)
  (let ((l (mevalargs (cdr form))))
    (marrayref-gensub (caar form) (car l) (cdr l))))

(defmfun arrstore-extend (a l r)
  (marrayset-gensub r a (car l) (cdr l)))
