;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;  
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module zero)

(declare-top (special $numer $listconstvars varlist genvar))

(defmfun $zeroequiv (exp var)
  (declare (special var ))
  (prog (r s v varlist genvar)
     (declare (special s v))
     (setq exp (specrepcheck exp))
     (setq r (let ($listconstvars) ($listofvars exp)))
     (if (and (cdr r) (or (cddr r) (not (alike1 (cadr r) var))))
	 (return '$dontknow))
     (setq exp ($exponentialize exp))
     (setq r (sdiff exp var))
     (if (isinop r '%derivative) (return '$dontknow))
     ($rat r)
     (setq r ($rat exp))
     (setq s (car r))
     (setq v (ratnumerator (cdr r)))
     (return (zeroequiv1 v))))

(defun zeroequiv1 (v)
  (declare (special var v s))
  (prog (v1 v2 coeff deg)
     (declare (special v1 v2))
     (if (atom v) (return (equal v 0)))
     coeffloop (if (null (cdr v)) (return t))
     (setq deg (cadr v))
     (if (equal deg 0) (return (zeroequiv1 (caddr v))))
     (setq coeff (caddr v))
     (when (zeroequiv1 coeff)
       (setq v (cons (car v) (cdddr v)))
       (go coeffloop))
     (setq v1 ($rat (sdiff (ratdisrep (cons s (cons v (caddr v)))) var)))
     (setq v2 (cadr ($rat (ratdisrep v1))))
     (if (equal (pdegree v2 (car v)) (cadr v))
	 (return (zeroequiv2 v)))
     (if (< (pdegree v2 (car v)) (cadr v))
	 (return (if (zeroequiv1 v2) (zeroequiv2 v))))
     (return '$dontknow)))

(defun zeroequiv2 (v)
  (declare (special var v s))
  (prog (r r1 r2)
     (declare (special r1 r2))
     (setq r (sin (* 1e-3 (random 1000.))))
     (setq v (maxima-substitute r var (ratdisrep (cons s (cons v 1)))))
     (setq v (meval '(($ev) v $numer)))
     (cond ((and (numberp v) (< (abs v) (* r 1e-2)))
	    (return t))
	   ((numberp v) (return nil)))
     (if (and (free v '$%i) (not (isinop v '%log)))
	 (return '$dontknow))
     (setq r1 ($realpart v))
     (setq r1 (meval '(($ev) r1 $numer)))
     (if (not (numberp r1)) (return '$dontknow))
     (setq r2 ($imagpart v))
     (setq r2 (meval '(($ev) r2 $numer)))
     (if (not (numberp r2)) (return '$dontknow))
     (cond ((and (< (abs r1) (* r 1e-2))
		 (< (abs r2) (* r 1e-2)))
	    (return t))
	   (t (return nil)))))
