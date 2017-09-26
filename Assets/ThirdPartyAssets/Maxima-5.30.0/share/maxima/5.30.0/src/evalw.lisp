;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       (c) Copyright 1981 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module evalw)

;;; Assuming that this will only be a top-level form, it will
;;; only be seen by MEVAL when a file is batched.

;;; EVAL_WHEN(TRANSLATE,FOO(ME),BAZ())$
;;; EVAL_WHEN([LOADFILE,BATCH],INITIALIZE())$

;; Gosh. Seems it was really stupid to have EVAL_WHEN for BATCH and DEMO,
;; people use it for the most random things. -gjc

(defmspec $eval_when (argl)
  (setq argl (cdr argl))
  (when (or (< (length argl) 2)
	    (not (or (atom (car argl))
		     ($listp (car argl)))))
    (merror (intl:gettext "eval_when: incorrect arguments; found: ~M") (car argl)))
  (if (member '$batch (if ($listp (car argl))
			  (cdar argl)
			  (list (car argl))))
      `(($evaluated_when) ,@(mapcar #'meval (cdr argl)))
      '$not_evaluated_when))

