;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       (c) Copyright 1980 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
;;; TRANSLATION PROPERTIES FOR MACSYMA OPERATORS AND FUNCTIONS.

;;; This file is for list and array manipulation optimizations.

(macsyma-module transf)


;;; some floating point translations. with tricks.

(def%tr %log (form)
  (let   (arg)
    (setq arg (translate (cadr form)))
    (cond ((and (eq (car arg) '$float) (get (caar form) 'lisp-function-to-use))
	   `($float ,(get (caar form) 'lisp-function-to-use) ,(cdr arg)))
	  (t `($any simplify (list ',(list (caar form)) ,(cdr arg)))))))

(def-same%tr %sin %log)
(def-same%tr %cos %log)
(def-same%tr %tan %log)
(def-same%tr %cot %log)
(def-same%tr %csc %log)
(def-same%tr %sec %log)
(def-same%tr %acot %log)
(def-same%tr %sinh %log)
(def-same%tr %cosh %log)
(def-same%tr %tanh %log)
(def-same%tr %coth %log)
(def-same%tr %csch %log)
(def-same%tr %sech %log)
(def-same%tr %asinh %log)
(def-same%tr %acsch %log)
(def-same%tr %erf %log)

(defmvar $tr_float_can_branch_complex t
  "States wether the arc functions might return complex
	 results. The arc functions are SQRT,LOG,ACOS, etc.
	 e.g. When it is TRUE then ACOS(X) will be of mode ANY even if X is
	 of mode FLOAT. When FALSE then ACOS(X) will be of mode FLOAT
	 if and only if X is of mode FLOAT.")

(def%tr %acos (form)
  (let ((arg (translate (cadr form))))
    (cond ((and (eq (car arg) '$float)
		(get (caar form) 'lisp-function-to-use))
	   `(,(cond ($tr_float_can_branch_complex
		     '$any)
		    (t '$float))
	     . (,(get (caar form) 'lisp-function-to-use)
		,(cdr arg))))
	  (t
	   `($any . (simplify (list '(,(caar form)) ,(cdr arg))))))))

(def-same%tr %asin %acos)
(def-same%tr %asec %acos)
(def-same%tr %asec %acos)
(def-same%tr %acsc %acos)
