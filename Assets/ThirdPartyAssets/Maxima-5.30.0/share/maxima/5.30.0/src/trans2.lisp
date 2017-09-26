;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;            Please do not modify this file.         See GJC           ;;;
;;;       (c) Copyright 1980 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;; TRANSLATION PROPERTIES FOR MACSYMA OPERATORS AND FUNCTIONS.

;;; This file is for list and array manipulation optimizations.

(macsyma-module trans2)


(def%tr $random (form) `($fixnum . ($random ,@(tr-args (cdr form)))))

(def%tr mequal (form)
  `($any . (simplify (list '(mequal) ,@(tr-args (cdr form))))))

(def%tr mcall (form)
  (setq form (cdr form))
  (let ((mode (cond ((atom (car form))
		     (function-mode (car form)))
		    (t '$any))))
    (setq form (tr-args form))
    (let ((op (car form)))
      (call-and-simp mode 'mcall `(,op . ,(cdr form))))))

;;; Meaning of the mode properties: most names are historical.
;;; (GETL X '(ARRAY-MODE)) means it is an array callable by the
;;; old maclisp style. This is unfortunately still useful to
;;; avoid indirection through the property list to get to the
;;; array.

(defvar $translate_fast_arrays nil)
;;When $translate_fast_arrays and $use_fast_arrays are true
;;there should only be two types of arrays and they should be stored on
;;the value cell of the symbol.  These should be the equivalent of the
;;zetalisp art-q and the si:equal-hash-table. Note that maxima lists
;;and maxima $matrices are also allowed for setting.  Note also that
;;because of some hokey things like mqapply etc, if you want
;;fast referenceing use a[i], or b[i]:..., ie use variables,
;;since if you try something complicated it may not translate as
;;simply.
;;Idea of these is for the lispm to store the array in the value cell
;;to use equal-hash-tables, and to clean up the local variable
;;in translated code for an array.
;;txx(i,j):=block([hl],hl[i]:j,hl[i]); should leave hl unbound, after creating
;;a  hash table for hl, There should be a resource of these.

(defun tr-maset (ar val  inds)
  ;; Top-level forms need to define the variable first.
  (if *macexpr-top-level-form-p* 
      `(nil progn (defvar ,ar ',ar) (maset ,val ,ar  ,@ inds))
      `(nil maset ,val ,ar  ,@ inds)))

(defun maset1 ( val ar  &rest inds &aux  )
  (cond
    ((and (typep ar 'cl:array)
	  (= (length inds) (cl:array-rank ar)))
     (setf (apply #'aref ar inds)  val))
    ((typep ar 'cl:hash-table)
     (setf (gethash (if (cdr inds) (copy-list inds) (car inds))
		    ar)
	   val))
    ((symbolp ar)
     (error "MASET1: first argument must not be a symbol; found: ~M" ar))
    ((and (= (length inds) 1)
	  (or ($listp ar) ($matrixp ar)))
     (setf (nth (car inds) ar) val) val)
    ((and ($matrixp ar)
	  (= (length inds) 2))
     (setf (nth (second inds) (nth  (car inds) ar)) val) val)
    (t (error "MASET1: invalid array reference: ~A" ar))))


;;apply is too expensive for a simple array reference.  The time
;;is increased by a factor of 6.  Note we use the locf form to get at
;;the local variable of the function calling maset in order to be able
;;to store a hash-table there in the case that the variable was not an
;;array

;;COULD USE THE FOLLOWING TO handle fast_arrays:true.
;;(defun set-up-hash-table (&optional val key &aux tab)
;;  (setq tab (make-hash-table :test 'equal)) ;alike?
;;  (setf (gethash key tab) val) tab)
;;
;;(defun maset-help1 ( val ar  &rest inds &aux  )
;;  "returns t if it set and nil if what went in could not be set but is a variable that
;;    should be set to hash array"
;;  (cond ((hash-table-p ar)
;;	 (setf (gethash (car inds) ar) val))
;;	((symbolp ar) nil)
;;	(($listp ar)
;;	 (setf (nth (car inds) ar) val)  t)
;;	(($matrixp ar) (setf (nth (second inds) (nth  (car inds) ar)) val) t)
;;	(t (error "not valid place ~A to put an array" ar))))
;;
;;
;;;;doesn't prevent multiple evaluation of inds val and ar.. but doesn't use locf
;;(defmacro maset (val ar &rest  inds )
;;  `(cond
;;     ((arrayp ar) (setf (aref ar ,@ inds) ,val))
;;     ((maset-help1 ,val ,ar ,@ inds) ,val)
;;      (t (setf ,ar (set-up-hash-table ,val (car ,ind))),val)))
;;
;;(defmacro maref ( ar &rest inds)
;;  `(cond ((arrayp ,ar) (aref ,ar ,@ inds))
;;	 ((hash-table-p ,ar) (gethash ,ar (car ,inds)))
;;	 ((symbolp ,ar)`((,ar ,@ (copy-list ,inds))))))

;;in maref in transl now

(defun tr-maref (ar inds)
  `(nil maref , ar ,@ (copy-list inds)))

(defun maref1 (ar  &rest inds &aux )
  (cond
    ((and (typep ar 'cl:array)
	  (= (length inds) (cl:array-rank ar)))
     (apply #'aref ar inds))
    ((typep ar 'cl:hash-table)
     (gethash (if (cdr inds) inds (car inds)) ar))
    ((symbolp ar)
     (cond ((mget ar 'hashar)
	    (harrfind `((,ar array) ,@(copy-list inds))))
	   (t
	    `((,ar array) ,@(copy-list inds)))))
    ((and (= (length inds) 1)
	  (or ($listp ar) ($matrixp ar)))
     (nth (first inds) ar))
    ((and ($matrixp ar) (= (length inds) 2))
     (nth (second inds) (nth (first inds) ar)))
    (t
     (merror (intl:gettext "Wrong number of array indices: ~M") (cons '(mlist) inds)))))


(defun tr-arraycall (form &aux all-inds)
  (cond
	($translate_fast_arrays (setq all-inds (mapcar 'dtranslate (cdr form)))
				;;not apply changed 'tr-maref
				(funcall 'tr-maref (cdr (translate (caar form)))   all-inds))
	(t
	 (translate `((marrayref)
		      ,(if $tr_array_as_ref (caar form)
			   `((mquote) ,(caar form)))     
		      ,@(cdr form))))))



(defun tr-arraysetq (array-ref value)
  ;; actually an array SETF, but it comes from A[X]:FOO
  ;; which is ((MSETQ) ... ...)
  (cond
	($translate_fast_arrays 
	 (funcall 'tr-maset (caar array-ref) (dtranslate value)
		  (mapcar 'dtranslate (copy-list (cdr array-ref)))))
	(t
	 ;; oops. Hey, I switch around order of evaluation
	 ;; here. no need to either man. gee.
	 (translate `((marrayset) ,value
		      ,(if $tr_array_as_ref (caar array-ref)
			   `((mquote) ,(caar array-ref)))
		      ,@(cdr array-ref))))))
 

(def%tr marrayref (form)
  (setq form (cdr form))
  (let ((mode (cond ((atom (car form))
		     (get (car form) 'array-mode)))))
    (cond ((null mode) (setq mode '$any)))
    (setq form (tr-args form))
    (let ((op (car form)))
      `(,mode . (,(if (and (= (length form) 2)
			   (eq mode '$float))
		      (progn (push-autoload-def 'marrayref '(marrayref1$))
			     'marrayref1$)
		      'marrayref)
		 ,op . ,(cdr form))))))

(def%tr marrayset (form)
  (setq form (cdr form))
  (let ((mode (cond ((atom (cadr form))
		     (get (cadr form) 'array-mode)))))
    (when (null mode) (setq mode '$any))
    (setq form (tr-args form))
    (destructuring-let (((val aarray . inds) form))
      `(,mode . (,(if (and (= (length inds) 1)
			   (eq mode '$float))
		      (progn
			(push-autoload-def 'marrayset '(marrayset1$))
			'marrayset1$)
		      'marrayset)
		  ,val ,aarray . ,inds)))))

(def%tr mlist (form)
  (if (null (cdr form)) ;;; []
      '($any . '((mlist)))
      `($any . (list '(mlist) . ,(tr-args (cdr form))))))

(def%tr $first (form)
  (setq form (translate (cadr form)))
  (call-and-simp '$any (if (eq '$list (car form))
			   'cadr
			   '$first)
		 (list (cdr form))))

;; Local Modes:
;; Mode: LISP
;; Comment Col: 40
;; END:
