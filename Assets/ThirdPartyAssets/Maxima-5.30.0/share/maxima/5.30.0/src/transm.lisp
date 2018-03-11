;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                Macros for TRANSL source compilation.                 ;;;
;;;       (c) Copyright 1980 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module transm macro)

(defprop dcl maxdoc fasl-dir)

(defmacro def%tr (name lambda-list &body body &aux definition)
  (setq definition
	(if (and (null body) (symbolp lambda-list))
	    `(def-same%tr ,name ,lambda-list)
	    `(defun-prop (,name translate) ,lambda-list ,@body)))
  `(eval-when (:compile-toplevel :execute :load-toplevel)
    ,definition))

(defmacro def-same%tr (name same-as)
  ;; right now MUST be used in the SAME file.
  `(putprop ',name
    (or (get ',same-as 'translate)
     (maxima-error "DEF-SAME%TR: ~a has no TRANSLATE property, so I can't make an alias." ',same-as))
    'translate))

;;; declarations for the TRANSL PACKAGE.

(declare-top (special *transl-sources*)
	     ;; The warning and error subsystem.
	     (special tr-abort	    ; set this T if you want to abort.
		      *translation-msgs-files*)	; the stream to print messages to.
	     ;; State variables.
	     (special pre-transl-forms*	; push onto this, gets output first into the transl file.
		      *warned-un-declared-vars*
		      *warned-fexprs*
		      *warned-mode-vars*
		      *warned-undefined-vars*
		      warned-undefined-variables
		      tr-abort
		      transl-file
		      *in-compfile*
		      *in-translate-file*
		      *in-translate*
		      *pre-transl-forms*
		      *new-autoload-entries* ; new entries created by TRANSL.
		      *untranslated-functions-called*)


	     ;; these special declarations are for before DEFMVAR
	     (special $errexp $loadprint $numer $savedef $nolabels $functions $props
		      munbound $values $transrun
		      st oldst  $version
		      rephrase $packagefile
		      dskfnp))

(defmacro bind-transl-state (&rest forms)
  ;; this binds all transl state variables to NIL.
  ;; and binds user-settable variables to themselves.
  ;; $TRANSCOMPILE for example can be set to TRUE while translating
  ;; a file, yet will only affect that file.
  ;; Called in 3 places, for compactness maybe this should be a PROGV
  ;; which references a list of variables?
  `(let (*warned-un-declared-vars*
	 *warned-fexprs*
	 *warned-mode-vars*
	 *warned-undefined-vars*
	 warned-undefined-variables
	 tr-abort
	 transl-file
	 *in-compfile*
	 *in-translate-file*
	 *in-translate*
	 *pre-transl-forms*
	 *new-autoload-entries*
	 ($tr_semicompile $tr_semicompile)
	 (arrays nil)
	 (exprs nil)
	 (lexprs nil)
	 (fexprs nil)
	 (specials nil)
	 (declares nil)
	 ($transcompile $transcompile)
	 ($tr_numer $tr_numer)
	 defined_variables)
    ,@forms))

(defun tr-format (sstring &rest argl &aux strs)
  (if (consp *translation-msgs-files*)
      (setq strs *translation-msgs-files*)
      (setq strs (list *translation-msgs-files*)))
  (loop for v in strs
	do (apply #'mformat v sstring argl)))

;; to use in mixing maxima and lisp
;; (tr #$$f(x):=x+2$)
(defmacro tr (u)
  (and (consp u)
       (eq (car u) 'quote)
       (bind-transl-state (translate-macexpr-toplevel (second u)))))

(defmacro maset (val ar &rest inds)
  `(progn
    (when (symbolp ,ar)
      (setf ,ar (make-equal-hash-table ,(if (cdr inds) t nil))))
    (maset1 ,val ,ar ,@inds)))

(defmacro maref (ar &rest inds)
  (cond ((or (eql ar 'mqapply)(and (consp ar) (member 'mqapply ar :test #'eq)))
         `(marrayref ,(first inds) ,@(cdr inds)))
	((consp ar)`(marrayref ,ar ,(first inds) ,@(cdr inds)))
	(t
	 `(maref1 ,ar ,@inds))))
