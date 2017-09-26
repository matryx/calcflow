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

(macsyma-module pois2)

(declare-top (special poisvals poishift poistsm poissiz poists $poisz $pois1))

(defmspec mpois (x) x)

(defun poislim1 (uu n)
  (declare (ignore uu))
  (unless  (fixnump n)
    (merror (intl:gettext "poislim: argument must be an integer; found: ~M") n))
  (setq poisvals nil)
  (setq poists (ash 1 n))
  (dotimes (j 6)
    (push (expt poists j) poisvals))
  (setq poissiz n
	poistsm (expt 2 (1- n))
	poishift (let ((sum 0))
		    (dotimes (i 6 sum)
		      (incf sum (* poistsm (expt poists i)))))
	$poisz '((mpois simp) nil nil)
	$pois1 (list '(mpois simp) nil (list poishift 1)))
  n)

(defun nonperiod (p)
  (and (null (cadr p))
       (= (caaddr p) poishift)
       (null (cddr (caddr p)))))

(poislim1 nil 5)
