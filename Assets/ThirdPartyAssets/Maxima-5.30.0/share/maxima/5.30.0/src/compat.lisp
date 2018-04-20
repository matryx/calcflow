;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;; Maclisp compatibility definitions.
;; This file is for Lisp differences only.  No knowledge of Macsyma should be
;; contained in this file.

;; Run time stuff

(defun symbolconc (&rest syms)
  (intern (apply #'concatenate 'string
		 (mapcar #'(lambda (sym)
			     (cond ((floatp sym)
				    (format nil "~S" sym))
				   ((integerp sym)
				    (format nil "~D" sym))
				   ((symbolp sym)
				    (symbol-name sym))
				   (t sym)))
			 syms))))

;; make a symbol out of the printed representations of all args
(defun concat (&rest args)
  (intern (format nil "~{~A~^~}" args)))

