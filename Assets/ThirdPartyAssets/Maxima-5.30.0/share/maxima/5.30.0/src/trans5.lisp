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

(macsyma-module trans5)

;;; these are TRANSLATE properies for the FSUBRS in JPG;COMM >

;;; LDISPLAY is one of the most beastly of all macsyma idiot
;;; constructs. First of all it makes a variable name and sets it,
;;; but it evaluates its argument such that
;;; x:10, LDISPLAY(F(X)) gives  (E1)   F(10)= ...
;;; LDISPLAY(X) gives X=10 of course. Sometimes it evaluates to get
;;; the left hand side, and sometimes it doesn't. It has its own
;;; private fucking version of the macsyma evaluator.
;;; To see multiple evaluation lossage in the interperter, try
;;; these: LDISPLAY(F(PRINT("FOOBAR")))$


(def%tr $disp (form)
  `($any . (display-for-tr ,(eq (caar form) '$ldisp)
	    nil				; equationsp
	    ,@(tr-args (cdr form)))))

(def-same%tr $ldisp $disp)

(def%tr $display (form)
  `($any . (display-for-tr ,(eq (caar form) '$ldisplay)
	    t
	    ,@(mapcar #'tr-exp-to-display (cdr form)))))

(def-same%tr $ldisplay $display)

;;; DISPLAY(F(X,Y,FOO()))
;;; (F X Y (FOO)) => (LET ((&G1 (FOO))) (list '(mequal) (LIST '(F) X Y &G1)
;;;						              (F X Y &G1)))
;;; DISPLAY(X) => (LIST '(MEQUAL) '$X $X)
;;; DISPLAY(Q[I]) => (LIST '(MEQUAL) (LIST '(Q ARRAY) $I) ...)

;;; Ask me why I did this at lisp level, this should be able
;;; to be hacked as a macsyma macro. the brain damage I get
;;; into sometimes...

;;; This walks the translated code attempting to come up
;;; with a reasonable object for display, expressions which
;;; might have to get evaluated twice are pushed on the
;;; VALUE-ALIST (<expression> . <gensym>)
;;; This is incompatible with the interpreter which evaluates
;;; arguments to functions twice. Here I only evaluate non-atomic
;;; things once, and make sure that the order of evaluation is
;;; pretty much correct. I say "pretty much" because MAKE-VALUES
;;; does the optmization of not generating a temporary for a variable.
;;; DISPLAY(FOO(Z,Z:35)) will loose because the second argument will
;;; be evaluated first. I don't seriously expect anyone to find this
;;; bug.

(defvar value-alist nil)

(defun make-values (expr-args)
  (mapcar #'(lambda (arg)
	      (cond ((or (atom arg)
			 (member (car arg) '(trd-msymeval quote) :test #'eq))
		     arg)
		    (t
		     (let ((sym (gensym)))
		       (push (cons arg sym) value-alist)
		       sym))))
	  expr-args))

(defstruct (disp-hack-ob (:conc-name nil) (:type list))
  left-ob right-ob)

(defun object-for-display-hack (exp)
  (if (atom exp)
      (make-disp-hack-ob :left-ob `',exp :right-ob exp)
      (case (car exp)
	(simplify
	 (let ((v (object-for-display-hack (cadr exp))))
	   (make-disp-hack-ob :left-ob (left-ob v)
			      :right-ob `(simplify ,(right-ob v)))))
	(marrayref
	 (let ((vals (make-values (cdr exp))))
	   (make-disp-hack-ob :left-ob `(list (list* ,(car vals) '(array)) ,@(cdr vals))
			      :right-ob `(marrayref ,@vals))))
	(mfunction-call
	 ;; assume evaluation of arguments.
	 (let ((vals (make-values (cddr exp))))
	   (make-disp-hack-ob :left-ob `(list '(,(cadr exp)) ,@vals)
			      :right-ob `(mfunction-call ,(cadr exp) ,@vals))))
	(list
	 (let ((obs (mapcar #'object-for-display-hack (cdr exp))))
	   (make-disp-hack-ob :left-ob `(list ,@(mapcar #'(lambda (u) (left-ob u)) obs))
			      :right-ob `(list ,@(mapcar #'(lambda (u) (right-ob u)) obs)))))
	(quote (make-disp-hack-ob :left-ob exp :right-ob exp))
	(trd-msymeval
	 (make-disp-hack-ob :left-ob `',(cadr exp) :right-ob exp))
	(t
	 (cond ((or (not (atom (car exp)))
		    (getl (car exp) '(fsubr fexpr macro)))
		(make-disp-hack-ob :left-ob `',exp :right-ob exp))
	       (t
		(let ((vals (make-values (cdr exp))))
		  (make-disp-hack-ob :left-ob `(list '(,(untrans-op (car exp))) ,@vals)
				     :right-ob `(,(car exp) ,@vals)))))))))

(defun tr-exp-to-display (exp)
  (let* ((lisp-exp (dtranslate exp))
	 (value-alist nil)
	 (ob (object-for-display-hack lisp-exp))
	 (disp `(list '(mequal) ,(left-ob ob) ,(right-ob ob))))
    (setq value-alist (nreverse value-alist))
    (if value-alist
	`((lambda ,(mapcar #'cdr value-alist) ,disp)
	  ,@(mapcar #'car value-alist))
	disp)))

(defun untrans-op (op)
  (or (cdr (assoc op '((add* . mplus)
		      (sub* . mminus)
		      (mul* . mtimes)
		      (div* . mquotient)
		      (power* . mexpt)) :test #'equal))
      op))


;;; From COMBIN >

(def%tr $cf (form)
  (setq form (car (tr-args (cdr form))))
  (push-autoload-def '$cf '(cfeval))
  `($any . (let (($listarith nil))
	     (cfeval ,form))))

;;; from TRGRED >

(def%tr $apply1 (form &aux (expr (tr-gensym)) (rules (tr-gensym)))
  (push-autoload-def '$apply1 '(apply1))
  `($any  . (do ((,expr ,(dtranslate (cadr form))
			(apply1 ,expr (car ,rules) 0))
		 (,rules ',(cddr form) (cdr ,rules)))
		((null ,rules) ,expr))))

(def%tr $apply2 (form)
  `($any . ((lambda (*rulelist)
	      (declare (special *rulelist))
	      (apply2 ,(dtranslate (cadr form)) 0))
	    ',(cddr form))))

(def%tr $applyb1 (form &aux (expr (tr-gensym)) (rules (tr-gensym)))
  (push-autoload-def '$applyb1 '(apply1hack))
  `($any . (do ((,expr ,(dtranslate (cadr form))
		       (car (apply1hack ,expr (car ,rules))))
		(,rules ',(cddr form) (cdr ,rules)))
	       ((null ,rules) ,expr))))

(def%tr $applyb2 (form)
  (push-autoload-def '$applyb2 '(apply2hack))
  `($any . ((lambda (*rulelist)
	      (declare (special *rulelist))
	      (apply2hack ,(dtranslate (cadr form))))
	    ',(cddr form))))

;;; this nice translation property written by REH.
;;; He is the first macsyma system program to ever
;;; write the translation property for his own special form!

(def%tr $buildq (form)
  (let ((alist				;would be nice to output
	 (mapcar		       ;backquote instead of list/cons
	  #'(lambda (var)	       ;but I'm not sure if things get
	      (cond ((atom var)		;macroexpanded.  -REH
					; Well, any macros are o.k. They
					; get expanded "at the right time". -gjc

		     `(cons ',var ,(dtranslate var)))
		    ((eq (caar var) 'msetq)
		     `(cons ',(cadr var) ,(dtranslate (caddr var))))
		    (t (setq tr-abort t)
		       (tr-format (intl:gettext "error: found unhandled variable ~:M in 'buildq'.~%") var))))
					;right thing to do here??
					;how much error checking does transl do now?
					; Yes. Not as much as it should! -GJC

	  (cdr (cadr form)))))
    (cond ((null alist)
	   `($any quote ,(caddr form)))
	  (t `($any mbuildq-subst (list ,@alist) ',(caddr form))))))
