;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module array)

;;; Macsyma User array utilities originally due to CFFK.

;;; Note that on the lisp level we regard as an array either
;;;   (1) a symbol whose ARRAY property is a common lisp array
;;;       [i.e., (symbol-array 'symbol)
;;;               == (get 'symbol 'array) => some array] or
;;;   (2) a common lisp array.
;;; On the maxima level a declared array not of type HASH or FUNCTIONAL 
;;; is either
;;;   (1m) a symbol whose ARRAY mproperty is of type (1)
;;;        [i.e., (symbol-array (mget 'symbol 'array)) => some array] or
;;;   (2m) it is of type (2) (and then called a `fast' array).
;;; Such an array is of type (1m) iff it was created with ARRAY 
;;; with USE_FAST_ARRAYS being set to FALSE.
;;;
;;; Curiously enough, ARRAY(...,TYPE,...) (which currently can only be
;;; used for USE_FAST_ARRAYS:FALSE) results in an array which is
;;; simultaneously of type (1) and (1m).

(defun $listarray (ary)
  (cons '(mlist)
	(cond ((mget ary 'hashar)
	       (mapcar #'(lambda (subs) ($arrayapply ary subs))
		       (cdddr (meval (list '($arrayinfo) ary)))))
	      ((mget ary 'array) (listarray (mget ary 'array)))
              ((arrayp ary)
               (if (eql (array-rank ary) 1)
                   (coerce ary 'list)
                   (coerce (make-array (apply '* (array-dimensions ary))
                                       :displaced-to ary
                                       :element-type (array-element-type ary))
                           'list)))
	      ((hash-table-p ary)
	       (let (vals (tab ary))
                 (maphash #'(lambda (x &rest l) l 
                              (unless (eq x 'dim1) (push (gethash x tab) vals)))
                            ary)
	         (reverse vals)))
	      ((eq (marray-type ary) '$functional)
	       (cdr ($listarray (mgenarray-content ary))))
	      (t 
	       (merror (intl:gettext "listarray: argument must be an array; found: ~M")
	               ary)))))

(defmfun $fillarray (ary1 ary2)
  (let ((ary
	 (or (mget ary1 'array)
	     (and (arrayp ary1) ary1)
	     (merror (intl:gettext "fillarray: first argument must be a declared array; found: ~M") ary1))))
    (fillarray ary
	       (cond (($listp ary2) (cdr ary2))
		     ((get (mget ary2 'array) 'array))
		     ((arrayp ary2) ary2)
		     (t
		      (merror (intl:gettext "fillarray: second argument must be an array or list; found: ~M") ary2))))
    ary1))

(defun getvalue (sym)
  (and (symbolp sym) (boundp sym) (symbol-value sym)))

(defmspec $rearray (l)
  (setq l (cdr l))
  (let ((ar (car l))
	(dims (mapcar #'meval (cdr l))))
    (cond ($use_fast_arrays
	   (setf (symbol-value ar) (rearray-aux ar (getvalue ar) dims)))
	  (t
	   (rearray-aux ar (getvalue ar) dims)))))

(defun rearray-aux (ar val dims &aux marray-sym)
  (cond ((arrayp val)
	 (apply 'lispm-rearray val dims))
	((arrayp (get ar 'array))
	 (setf (get ar 'array) (apply 'lispm-rearray (get ar 'array) dims)))
	((setq marray-sym (mget ar 'array))
	 (rearray-aux marray-sym nil dims)
	 ar)
	(t (merror (intl:gettext "rearray: argument is not an array: ~A") ar))))

(defun lispm-rearray (ar &rest dims)
  (cond ((eql (array-rank ar) (length dims))
	 (adjust-array ar (mapcar #'1+ (copy-list dims)) :element-type (array-element-type ar)  ))
	(t (merror (intl:gettext "rearray: arrays must have the same number of subscripts.")))))
