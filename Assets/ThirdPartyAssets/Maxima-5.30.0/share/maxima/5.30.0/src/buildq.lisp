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

(macsyma-module buildq)

;; Exported functions are $BUILDQ and MBUILDQ-SUBST
;; TRANSLATION property for $BUILDQ in MAXSRC;TRANS5 >

;;**************************************************************************
;;******                                                              ******
;;******      BUILDQ:  A backquote-like construct for Macsyma         ******
;;******                                                              ******
;;**************************************************************************

;;DESCRIPTION:


;; Syntax:

;; BUILDQ([<varlist>],<expression>);

;; <expression> is any single macsyma expression
;; <varlist> is a list of elements of the form <atom> or <atom>:<value>


;; Semantics:

;; the <value>s in the <varlist> are evaluated left to right (the syntax
;; <atom> is equivalent to <atom>:<atom>).  then these values are substituted
;; into <expression> in parallel.  If any <atom> appears as a single
;; argument to the special form SPLICE (i.e. SPLICE(<atom>) ) inside
;; <expression>, then the value associated with that <atom> must be a macsyma
;; list, and it is spliced into <expression> instead of substituted.

;;SIMPLIFICATION:


;; the arguments to $BUILDQ need to be protected from simplification until
;; the substitutions have been carried out.  This code should affect that.

(defprop $buildq simpbuildq operators)
(defprop %buildq simpbuildq operators)

;; This is modeled after SIMPMDEF, SIMPLAMBDA etc. in JM;SIMP >

(defun simpbuildq (x *ignore* simp-flags)
  (declare (ignore *ignore* simp-flags))
  (cons '($buildq simp) (cdr x)))

;; Note that supression of simplification is very important to the semantics
;; of BUILDQ.  Consider BUILDQ([A:'[B,C,D]],SPLICE(A)+SPLICE(A));

;; If no simplification takes place, $BUILDQ returns B+C+D+B+C+D.
;; If the expression is simplified into 2*SPLICE(A), then 2*B*C*D results.



;;INTERPRETIVE CODE:


(defmspec $buildq (form) (setq form (cdr form))
	  (cond ((or (null (cdr form))
		     (cddr form))
		 (merror (intl:gettext "buildq: expected exactly two arguments; found ~M") `(($buildq) ,@form)))
		(t (mbuildq (car form) (cadr form)))))

;; this macro definition is NOT equivalent because of the way lisp macros
;; are currently handled in the macsyma interpreter.  When the subr form
;; is returned the arguments get MEVAL'd (and hence simplified) before
;; we get ahold of them.

;; Lisp MACROS, and Lisp FEXPR's are meaningless to the macsyma evaluator
;; and should be ignored, the proper things to use are MFEXPR* and
;; MMACRO properties.  -GJC

;;(DEFMACRO ($BUILDQ DEFMACRO-FOR-COMPILING T)
;;          (VARLIST . EXPRESSIONS)
;;   (COND ((OR (NULL VARLIST)
;;	       (NULL EXPRESSIONS)
;;	       (CDR EXPRESSIONS))
;;	   (DISPLA `(($BUILDQ) ,VARLIST ,@EXPRESSIONS))
;;	   (MERROR "`buildq' takes 2 args"))
;;	  (T `(MBUILDQ ',VARLIST ',(CAR EXPRESSIONS)))))


(defun mbuildq (substitutions expression)
  (cond ((not ($listp substitutions))
	 (merror (intl:gettext "buildq: first argument must be a list; found ~M") substitutions)))
  (mbuildq-subst
   (mapcar #'(lambda (form)             ; make a variable/value alist
	       (cond ((symbolp form)
		      (cons form (meval form)))
		     ((and (eq (caar form) 'msetq)
			   (symbolp (cadr form)))
		      (cons (cadr form) (meval (caddr form))))
		     (t
		      (merror (intl:gettext "buildq: variable must be a symbol or an assignment to a symbol; found ~M")
			      form
			      ))))
	   (cdr substitutions))
   expression))


;; this performs the substitutions for the variables in the expressions.
;; it tries to be smart and only copy what list structure it has to.
;; the first arg is an alist of pairs:  (<variable> . <value>)
;; the second arg is the macsyma expression to substitute into.

(defmfun mbuildq-subst (alist expression)
  (prog (new-car)
     (cond ((atom expression)
	    (return (mbuildq-associate expression alist)))
	   ((atom (car expression))
	    (setq new-car (mbuildq-associate (car expression) alist)))
	   ((mbuildq-splice-associate expression alist)
					; if the expression is a legal SPLICE, this clause is taken.
					; a SPLICE should never occur here.  It corresponds to `,@form

	    (merror (intl:gettext "splice: encountered 'splice' in an unexpected place: ~M") expression))
	   ((atom (caar expression))
	    (setq new-car (mbuildq-associate (caar expression) alist))
	    (cond ((eq new-car (caar expression))
		   (setq new-car (car expression)))
		  ((atom new-car)
		   (setq new-car (cons new-car (cdar expression))))
		  (t (return
		       `(,(cons 'mqapply (cdar expression))
			 ,new-car
			 ,@(mbuildq-subst alist (cdr expression)))))))
	   ((setq new-car
		  (mbuildq-splice-associate (car expression) alist))
	    (return (append (cdr new-car)
			    (mbuildq-subst alist (cdr expression)))))
	   (t (setq new-car (mbuildq-subst alist (car expression)))))
     (return
       (let ((new-cdr (mbuildq-subst alist (cdr expression))))
	 (cond ((and (eq new-car (car expression))
		     (eq new-cdr (cdr expression)))
		expression)
	       (t (cons new-car new-cdr)))))))

;; this function returns the appropriate thing to substitute for an atom
;; appearing inside a backquote.  If it's not in the varlist, it's the
;; atom itself.

(defun mbuildq-associate (atom alist)
  (let ((form))
    (cond ((not (symbolp atom))
	   atom)
	  ((setq form (assoc atom alist :test #'eq))
	   (cdr form))
	  ((setq form (assoc ($verbify atom) alist :test #'eq))
					;trying to match a nounified substitution variable
	   (cond ((atom (cdr form))
		  ($nounify (cdr form)))
		 ((member (caar (cdr form))
			'(mquote mlist mprog mprogn lambda) :test #'eq)
					;list gotten from the parser.
		  `((mquote) ,(cdr form)))
		 (t `( (,($nounify (caar (cdr form)))
			,@(cdar (cdr form)))
		      ,@(cdr (cdr form))))))
	  ;; ((<verb> ...) ...)  ==>  ((<noun> ...) ...)
	  (t atom))))

;; this function decides whether the SPLICE is one of ours or not.
;; the basic philosophy is that the SPLICE is ours if it has exactly
;; one symbolic argument and that arg appears in the current varlist.
;; if it's one of ours, this function returns the list it's bound to.
;; otherwise it returns nil.  Notice that the list returned is an
;; MLIST and hence the cdr of the return value is what gets spliced in.

(defun mbuildq-splice-associate (expression alist)
  (and (eq (caar expression) '$splice)
       (cdr expression)
       (null (cddr expression))
       (let ((match (assoc (cadr expression) alist :test #'eq)))
	 (cond ((null match) () )
	       ((not ($listp (cdr match)))
		(merror (intl:gettext "buildq: 'splice' must return a list, but ~M returned: ~M~%")
			expression (cdr match)))
	       (t (cdr match))))))
