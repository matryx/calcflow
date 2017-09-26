;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module mmacro)

;; Exported functions are MDEFMACRO, $MACROEXPAND, $MACROEXPAND1, MMACRO-APPLY
;;                        MMACROEXPANDED, MMACROEXPAND and MMACROEXPAND1


(declare-top (special $macros $functions $transrun $translate))

 
;; $MACROS declared in jpg;mlisp >


(defmvar $macroexpansion ()
  "Governs the expansion of Maxima Macros.  The following settings are
available:  FALSE means to re-expand the macro every time it gets called.
EXPAND means to remember the expansion for each individual call do that it 
won't have to be re-expanded every time the form is evaluated.  The form will 
still grind and display as if the expansion had not taken place.  DISPLACE
means to completely replace the form with the expansion.  This is more space
efficient than EXPAND but grinds and displays the expansion instead of the
call."
  modified-commands '($macroexpand)
  setting-list      '( () $expand $displace ) )


;;; LOCAL MACRO ;;;

(defmacro copy1cons (name) `(cons (car ,name) (cdr ,name)))

;;; DEFINING A MACRO ;;;

(defmspec mdefmacro (form) (setq form (cdr form))
	  (cond ((or (null (cdr form)) (cdddr form))
		 (merror (intl:gettext "macro definition: must have exactly two arguments; found: ~M")
			 `((mdefmacro) ,@form))
		 )
		(t (mdefmacro1 (car form) (cadr form)))))


(defun mdefmacro1 (fun body)
  (let ((name) (args))
    (cond ((or (atom fun)
	       (not (atom (caar fun)))                
	       (member 'array (cdar fun) :test #'eq)              
	       (mopp (setq name ($verbify (caar fun))))
	       (member name '($all $% $%% mqapply) :test #'eq))
	   (merror (intl:gettext "macro definition: illegal definition: ~M") ;ferret out all the
		   fun))		;  illegal forms
	  ((not (eq name (caar fun)))	;efficiency hack I guess
	   (rplaca (car fun) name)))	;  done in jpg;mlisp
    (setq args (cdr fun))		;  (in MDEFINE).
    (mredef-check name)
    (do ((a args (cdr a)) (mlexprp))
	((null a)
	 (remove1 (ncons name) 'mexpr t $functions t) ;do all arg checking,
	 (cond (mlexprp (mputprop name t 'mlexprp)) ; then remove MEXPR defn
	       (t nil)))
      (cond ((mdefparam (car a)))
	    ((and (mdeflistp a)
		  (mdefparam (cadr (car a))))
	     (setq mlexprp t))
	    (t 
	     (merror (intl:gettext "macro definition: bad argument: ~M")
		     (car a)))))
    (remove-transl-fun-props name)
    (add2lnc `((,name) ,@args) $macros)
    (mputprop name (mdefine1 args body) 'mmacro)
     
    (cond ($translate (translate-and-eval-macsyma-expression
		       `((mdefmacro) ,fun ,body))))
    `((mdefmacro simp) ,fun ,body)))




;;; EVALUATING A MACRO CALL ;;;


(defmfun mmacro-apply (defn form)
  (mmacroexpansion-check form
			 (if (and (atom defn)
				  (not (symbolp defn)))
			     ;; added this clause for NIL. MAPPLY
			     ;; doesn't really handle applying interpreter
			     ;; closures and subrs very well.
			     (apply defn (cdr form))
			     (mapply1 defn (cdr form) (caar form) form))))




;;; MACROEXPANSION HACKERY ;;;


;; does any reformatting necessary according to the current setting of
;; $MACROEXPANSION.  Note that it always returns the expansion returned
;; by displace, for future displacing.

(defun mmacroexpansion-check (form expansion)
  (case $macroexpansion
    (( () )
     (cond ((eq (caar form) 'mmacroexpanded)
	    (mmacro-displace form expansion))
	   (t expansion)))
    (($expand)
     (cond ((not (eq (caar form) 'mmacroexpanded))
	    (displace form `((mmacroexpanded) 
			     ,expansion
			     ,(copy1cons form)))))
     expansion)
    (($displace)
     (mmacro-displace form expansion))
    (t (mtell (intl:gettext "warning: unrecognized value of 'macroexpansion'.")))))


(defun mmacro-displace (form expansion)
  (displace form (cond ((atom expansion) `((mprogn) ,expansion))
		       (t expansion))))


;; Handles memo-ized forms.  Reformats them if $MACROEXPANSION has changed.
;; Format is ((MMACROEXPANDED) <expansion> <original form>)

(defmspec mmacroexpanded (form)
  (meval (mmacroexpansion-check form (cadr form))))


;;; MACROEXPANDING FUNCTIONS ;;;


(defmspec $macroexpand (form) (setq form (cdr form))
	  (cond ((or (null form) (cdr form))
		 (merror (intl:gettext "macroexpand: must have exactly one argument; found: ~M")
			 `(($macroexpand) ,@form)))
		(t (mmacroexpand (car form)))))

(defmspec $macroexpand1 (form) (setq form (cdr form))
	  (cond ((or (null form) (cdr form))
		 (merror (intl:gettext "macroexpand1: must have exactly one argument; found: ~M")
			 `(($macroexpand1) ,@form)))
		(t (mmacroexpand1 (car form)))))


;; Expands the top-level form repeatedly until it is no longer a macro
;; form.  Has to copy the form each time because if macros are displacing
;; the form given to mmacroexpand1 will get bashed each time.  Recursion
;; is used instead of iteration so the user gets a pdl overflow error
;; if he tries to expand recursive macro definitions that never terminate.

(defun mmacroexpand (form)
  (let ((test-form (if (atom form) form (copy1cons form)))
	(expansion (mmacroexpand1 form)))
    (cond ((equal expansion test-form)
	   expansion)
	  (t (mmacroexpand expansion)))))


;; only expands the form once.  If the form is not a valid macro
;; form it just gets returned (eq'ness is preserved).  Note that if the
;; macros are displacing, the returned form is also eq to the given
;; form (which has been bashed).

(defun mmacroexpand1 (form)
  (let ((funname) (macro-defn))
    (cond ((or (atom form)
	       (atom (car form))
	       (member 'array (cdar form) :test #'eq)
	       (not (symbolp (setq funname (mop form)))))
	   form)
	  ((eq funname 'mmacroexpanded)
	   (mmacroexpansion-check form (cadr form)))
	  ((setq macro-defn
		 (or (and $transrun 
			  (get (caar form) 'translated-mmacro))
		     (mget (caar form) 'mmacro)))
	   (mmacro-apply macro-defn form))
	  (t form))))

;;; SIMPLIFICATION ;;;

(defprop mdefmacro simpmdefmacro operators)

;; emulating simpmdef (for mdefine) in jm;simp
(defmfun simpmdefmacro (x *ignored* simp-flag)
  (declare (ignore *ignored* simp-flag))
  (cons '(mdefmacro simp) (cdr x)))

(defun displace (x y)
  (setf (car x) (car y))
  (setf (cdr x) (cdr y))
  x)
