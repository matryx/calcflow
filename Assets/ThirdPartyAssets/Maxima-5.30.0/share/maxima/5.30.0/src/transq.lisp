;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Compilation environment for TRANSLATED MACSYMA code.        ;;;
;;;       (c) Copyright 1980 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;; this are COMPILE-TIME macros for TRANSLATE MACSYMA code.

(macsyma-module transq macro)

(load-macsyma-macros transm)

(defmacro def-mtrvar (v a &optional (priority 1))
  (declare (ignore priority))
  ;; ignored variable around for TRANSLATED files pre
  ;; 3:03pm  Thursday, 11 March 1982 -gjc
  `(progn
    (declare-top (special ,v))

    (if (or (not (boundp ',v))
	    ;; a SYMBOL SET to ITSELF is considered to be
	    ;; UNBOUND for our purposes in Macsyma.
	    (eq ,v ',v))
	(setq ,v ,a))))

(define-compiler-macro mfunction-call (f &rest l &aux l1)
  (setq l1 l)
  (cond ((or (fboundp f)
	     (get f 'once-translated)
	     (get f 'translated))
	 (cons f l1))
	(t `(lispm-mfunction-call-aux ',f ', l1 (list ,@ l1) nil))))


;;; macros for compiled environments.

;;; (FUNGEN&ENV-for-meval <eval vars list> <late eval vars list> .  <EXP>)
;;; will define a function globally with a unique name
;;; (defun <name> <list of variables> <exp>). And return
;;; `((<name>) ,@<eval>> . <late eval>). The resulting expression may
;;; then be passed to a function which will bind variables from
;;; the <late eval vars list> and possibly other variables free in
;;; <exp> and then call MEVAL on the expression.
;;; the expression was translated using TR-LAMBDA.

(defvar *infile-name-key* '||
  "This is a key gotten from the infile name, in the interpreter
  other completely hackish things with FSUBRS will go on.")

(defvar forms-to-compile-queue ())

(defun emit-defun (exp)
  (if $tr_semicompile (setq exp `(progn ,exp)))
  (let nil
    (setq forms-to-compile-queue (nconc forms-to-compile-queue (list (copy-tree exp))))))

(defmacro pop-declare-statement (l)
  `(and (not (atom (car ,l)))
    (eq (caar ,l) 'declare)
    (pop ,l)))


;;; Lambda expressions emitted by the translator.

;; lambda([u,...],...) where any free unquoted variable in the body is
;; either unbound or globally bound or locally bound in some
;; non-enclosing block.  At this point, BODY has already the correct
;; special declarations for elements of ARGL.
(defmacro m-tlambda (argl &body body)
  `(function
    (lambda ,argl
     ,@body)))

;; lambda([u,...,[v]],...) with the same condition as above.
(defmacro m-tlambda& (argl &rest body)
  `(function (lambda (,@(reverse (cdr (reverse argl)))
		      &rest ,@(last argl))
     ,(pop-declare-statement body)
     (setq ,(car (last argl))
	   (cons '(mlist) ,(car (last argl))))
     ,@ body)))

;; lambda([u,...],...) with free unquoted variables in the body which
;; have a local binding in some enclosing block, but no global one,
;; i.e, the complement of the condition for m-tlambda above.
(defmacro m-tlambda&env ((reg-argl env-argl) &body body)
  (declare (ignore env-argl))
  `(function
    (lambda ,reg-argl
     ;;(,@(or (pop-declare-statement body) '(declare)) (special ,@env-argl))
     ,@body)))

;; lambda([u,...,[v]],...) with the same condition as above.
(defmacro m-tlambda&env& ((reg-argl env-argl) &body body)
  (declare (ignore env-argl))
  (let ((last-arg (car (last reg-argl))))
    `(function
      (lambda (,@(butlast reg-argl) &rest ,last-arg)
       ;;(,@(or (pop-declare-statement body) '(declare)) (special ,@env-argl))
       ,(pop-declare-statement body)
       (setq ,last-arg (cons '(mlist) ,last-arg))
       ,@body))))


;; Problem: You can pass a lambda expression around in macsyma
;; because macsyma "general-rep" has a CAR which is a list.
;; Solution: Just as well anyway.


;;the lexical scoping  handles the environment in most cases
;;and it is messy to queue things

;;; this is the important case for numerical hackery.


;;; This is not optimal code.
;;; I.E. IT SUCKS ROCKS.

(defmacro set-vals-into-list (argl var)
  (do ((j 0 (1+ j))
       (argl argl (cdr argl))
       (l nil `((setf (nth ,j ,var) ,(car argl)) ,@l)))
      ((null argl) `(progn ,@l))))
