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

(macsyma-module displm macro)

(declare-top
 ;; evaluate for declarations
 (special
  ^w			;If T, then no output goes to the console.
  linel			;Width of screen.
  ttyheight		;Height of screen.

  width height depth maxht maxdp level size lop rop break right
  bkpt bkptwd bkptht bkptdp bkptlevel bkptout lines 
  oldrow oldcol display-file in-p
  mratp $aliases))

;;; macros for the DISPLA package.

;; (PUSH-STRING "foo" RESULT) --> (SETQ RESULT (APPEND '(#/o #/o #/f) RESULT))

(defmacro push-string (string symbol)
  (check-arg string stringp "a string")
  (check-arg symbol symbolp "a symbol")
  `(setq ,symbol (list* ,@(nreverse (exploden string)) ,symbol)))

;; Macros for setting up dispatch table.
;; Don't call this DEF-DISPLA, since it shouldn't be annotated by
;; TAGS and @.  Syntax is:
;; (DISPLA-DEF [<operator>] [<dissym> | <l-dissym> <r-dissym>] [<lbp>] [<rbp>])
;; If only one integer appears in the form, then it is taken to be an RBP.

;; This should be modified to use GJC's dispatch scheme where the subr
;; object is placed directly on the symbol's property list and subrcall
;; is used when dispatching.

(defmacro displa-def (operator dim-function &rest rest &aux l-dissym r-dissym lbp rbp)
  (dolist (x rest)
    (cond ((stringp x)
	   (if l-dissym (setq r-dissym x) (setq l-dissym x)))
	  ((integerp x)
	   (if rbp (setq lbp rbp))
	   (setq rbp x))
	  (t (merror "DISPLA-DEF: unrecognized object: ~a" x))))
  (when l-dissym
    (setq l-dissym (if r-dissym
		       (cons (exploden l-dissym) (exploden r-dissym))
		       (exploden l-dissym))))
  `(progn 
    (defprop ,operator ,dim-function dimension)
    ,(when l-dissym  `(defprop ,operator ,l-dissym dissym))
    ,(when lbp       `(defprop ,operator ,lbp lbp))
    ,(when rbp       `(defprop ,operator ,rbp rbp))))
