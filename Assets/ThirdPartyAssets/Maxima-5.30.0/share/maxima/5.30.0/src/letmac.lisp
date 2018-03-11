;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
;; Destructuring DEFUN must be added to this at some point.

(defvar *let-macro-vals* nil)

;; Kludge to avoid warning that a different file is redefining
;; LET and LET*.  SI has LET and LET* externed, so there is no
;; "illegally defining" warning.

(defmacro destructuring-let (pairs &body body)
  (do ((pairs pairs (cdr pairs))
       (vars nil)
       (*let-macro-vals* nil)
       (tem))
      ((null pairs)
       (cond ((not (null vars))
	      `(cl:let ,(nreverse (loop for v in vars
				     for w in *let-macro-vals*
				     collect (list v w)))
		,@body))
	     ((null (cdr body))
	      (car body))
	     (t `(progn . ,body))))
    (cond ((atom (car pairs))
	   (or (symbolp (car pairs))
	       (error "Garbage found in `let' pattern: ~S" (car pairs)))
	   (setq vars (cons (car pairs) vars))
	   (setq *let-macro-vals* (cons nil *let-macro-vals*)))
	  (t
	   (setq tem vars)
	   (setq vars (let-macro-get-vars (caar pairs) vars))
	   (or (eq tem vars)
	       (setq body (nconc (let-macro-hair (caar pairs)
						 (cadar pairs)
						 *let-macro-vals*)
				 body)))))))

(defun let-macro-get-vars (pattern vars)
  (cond ((null pattern) vars)
	((atom pattern)
	 (or (symbolp pattern)
	     (error "Garbage found in `let' pattern: ~S" pattern))
	 (setq *let-macro-vals* (cons nil *let-macro-vals*))
	 (cons pattern vars))
	(t (let-macro-get-vars (cdr pattern)
			       (let-macro-get-vars (car pattern) vars)))))

(defmacro desetq (&rest p)
  (do ((p p (cddr p))
       (body nil)
       (tem))
      ((null p)
       `(progn . ,body))
    (cond ((atom (cdr p))
	   (error "Odd number of args to `desetq': ~S" p))
	  ((atom (car p))
	   (or (symbolp (car p))
	       (error "Garbage found in `desetq' pattern: ~S" (car p)))
	   (and (null (car p))
		(error "Bad `desetq' pattern: ~S" (car p)))
	   (setq body (nconc body `((setq ,(car p) ,(cadr p))))))
	  (t
	   (setq tem (cons nil nil))
	   (setq body (nconc body
			     `((setq ,(let-macro-get-last-var (car p))
				. ,tem)
			       . ,(let-macro-hair (car p) (cadr p) tem))))))))


(defun let-macro-get-last-var (pattern)
  (cond ((atom pattern) pattern)
	(t
	 (or (let-macro-get-last-var (cdr pattern))
	     (let-macro-get-last-var (car pattern))))))

(defun let-macro-hair (pattern code cell)
  (cond ((null pattern) nil)
	((atom pattern)
	 (rplaca cell code)
	 nil)
	(t
	 (let ((avar (let-macro-get-last-var (car pattern)))
	       (dvar (let-macro-get-last-var (cdr pattern))))
	   (cond ((null avar)
		  (if (null dvar)
		      nil
		      (let-macro-hair (cdr pattern) `(cdr ,code) cell)))
		 ((null dvar)
		  (let-macro-hair (car pattern) `(car ,code) cell))
		 (t
		  (rplaca cell code)
		  (let ((acell (cons nil nil))
			(dcell (cons nil nil)))
		    (cons `(setq ,avar . ,acell)
			  (nconc (let-macro-hair (car pattern) `(car ,dvar) acell)
				 (cons `(setq ,dvar . ,dcell)
				       (let-macro-hair (cdr pattern) `(cdr ,dvar) dcell)))))))))))

(defmacro destructuring-let* (pairs &body body)
  (cond ((loop for v in pairs
		always (or (symbolp v) (and (consp v) (symbolp (car v)))))
	 `(cl:let* ,pairs ,@body))
	(t
	 (do ((a (reverse pairs) (cdr a))
	      (b body `((destructuring-let (,(car a)) . ,b))))
	     ((null a)
	      (cond ((null (cdr b)) (car b))
		    (t `(progn . ,b))))))))

