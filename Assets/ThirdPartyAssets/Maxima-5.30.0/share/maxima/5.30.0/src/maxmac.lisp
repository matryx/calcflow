;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  (c) Copyright 1976, 1983 Massachusetts Institute of Technology      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module maxmac macro)

;; This file contains miscellaneous macros used in Macsyma source files.

;; General purpose macros which are used in Lisp code, but not widely enough
;; accepted to be a part of Lisp systems.

;; 'ttyoff' is a system independent way of expressing the Maclisp ^W.

(defvar ttyoff    '^w)

;; Like PUSH, but works at the other end.

(defmacro tuchus (list object)
  `(setf ,list (nconc ,list (ncons ,object))))

;; (EXCH A B) exchanges the bindings of A and B
;; Maybe it should turn into (PSETF A B B A)?

(defmacro exch (x y)
  `(setf ,x (prog1 ,y (setf ,y ,x))))

;; The following macros pertain only to Macsyma.

;; Except on the Lisp Machine, load the specified macro files.
;; On the Lisp Machine, the DEFSYSTEM facility is used for loading
;; macro files, so just check that the file is loaded. This is
;; a useful error check that has saved a lot of time since Defsystem
;; is far from fool-proof. 

(defun load-macsyma-macros-at-runtime (&rest l)
  (mapcar #'(lambda (x) (unless (get x 'macsyma-module)
			  (error  "Missing Maxima macro file -- ~A" x)))
	  l))

(defmacro load-macsyma-macros (&rest macro-files)
  (apply #'load-macsyma-macros-at-runtime macro-files)
  (values))

(defmacro with-new-context (sub-context &rest forms)
  `(let ((my-context (gensym "$CTXT")))
     (mfuncall '$supcontext my-context ,@sub-context)
     (unwind-protect
       (prog1 ,@forms)
       ($killcontext my-context))))

;; For creating a macsyma evaluator variable binding context.
;; (MBINDING (VARIABLES &OPTIONAL VALUES FUNCTION-NAME)
;;    ... BODY ...)

(defmacro mbinding (variable-specification &rest body &aux (temp (gensym)))
  `(let ((,temp ,(car variable-specification)))
     ;; Don't optimize out this temporary, even if (CAR VARIABLE-SPECICIATION)
     ;; is an ATOM. We don't want to risk side-effects.
     ,(case (length variable-specification)
	    ((1)
	     `(mbinding-sub ,temp ,temp nil ,@body))
	    ((2)
	     `(mbinding-sub ,temp ,(cadr variable-specification) nil ,@body))
	    ((3)
	     `(mbinding-sub ,temp ,(cadr variable-specification)
			    ,(caddr variable-specification)
			    ,@body))
	    (t
	     (maxima-error "Bad variable specification: ~a" variable-specification)))))

(defmacro mbinding-sub (variables values function-name &rest body &aux (win (gensym)))
  `(let ((,win nil))
     (unwind-protect
	  (progn
	    (mbind ,variables ,values ,function-name)
	    (setq ,win t)
	    ,@body)
       (if ,win (munbind ,variables)))))

;; How About MTYPEP like (MTYPEP EXP 'ATAN) or (MTYPEP EXP '*) - Jim.
;; Better, (EQ (MTYPEP EXP) 'ATAN).

(defmacro matanp (x)
  `(let ((thing ,x))
     (and (not (atom thing)) (eq (caar thing) '%atan))))

;; Macros used in LIMIT, DEFINT, RESIDU.
;; If we get a lot of these, they can be split off into a separate macro
;; package.

(defmacro real-infinityp (x)
  `(member ,x real-infinities :test #'eq))

(defmacro infinityp (x)
  `(member ,x infinities :test #'eq))

(defmacro real-epsilonp (x)
  `(member ,x infinitesimals :test #'eq))

(defmacro free-epsilonp (x)
  `(not (amongl infinitesimals ,x)))

(defmacro free-infp (x)
  `(not (amongl infinities ,x)))

(defmacro inf-typep (x)
  `(car (amongl infinities ,x)))

(defmacro hot-coef (p)
  `(pdis (caddr (cadr (rat-no-ratfac ,p)))))

(defmacro defmspec (function . rest)
  `(progn
     (defun-prop (,function mfexpr*) ,@rest)))

;; Setf hacking.

(defmfun mget (atom ind)
  (let ((props (and (symbolp atom) (get atom 'mprops))))
    (and props (getf (cdr props) ind))))

(defsetf mget (sym tag) (value)
  `(mputprop ,sym ,value ,tag))

(defmacro old-get (plist tag)
  `(getf (cdr ,plist) ,tag))

(defmfun $get (atom ind)
  (prop1 '$get atom nil ind))

(defsetf $get (sym tag) (value)
  `($put ,sym ,value ,tag))

(defmacro  mdefprop (sym val indicator)
  `(mputprop ',sym ',val ',indicator))

(defmfun mputprop (atom val ind)
  (let ((props (get atom 'mprops)))
    (if (null props) (putprop atom (setq props (ncons nil)) 'mprops))
    (putprop props val ind)))
