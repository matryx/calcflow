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
(macsyma-module strmac macro)

;; Data Representation macros.

;; Hand coded macros for manipulating data structures in a simple
;; way, yet still preserving some abstraction.  Replacement for the mode
;; package.  We no longer know the type of things at run-time, so the names
;; of each macro must reflect the type of its operand, e.g.
;; RAT-NUMER versus MRAT-NUMER.

(defmacro make-g-rep (operator . operands)
  `(list (list ,operator) . ,operands))
(defmacro make-g-rep-simp (operator . operands)
  `(list (list ,operator) . ,operands))

(defmacro g-rep-operator (exp) `(caar ,exp))
(defmacro g-rep-operands (exp) `(cdr ,exp))
(defmacro g-rep-first-operand (exp)
  `(cadr ,exp))

(defmacro make-mplus (&rest args) `(list '(mplus) . ,args))
(defmacro make-mplus-l (llist) `(cons '(mplus) ,llist))
(defmacro make-mplus-simp (&rest args) `(list '(mplus simp) . ,args))



(defmacro make-mtimes (&rest args) `(list '(mtimes) . ,args))
(defmacro make-mtimes-l (llist) `(cons '(mtimes) ,llist))
(defmacro make-mtimes-simp (&rest args) `(list '(mtimes simp) . ,args))

;; losing MACLISP doesn't like BASE as a variable name !!
(defmacro make-mexpt (thing-being-raised-to-power expt)
  `(list '(mexpt) ,thing-being-raised-to-power ,expt))
(defmacro make-mexpt-l (llist) `(cons '(mexpt) ,llist))
(defmacro make-mexpt-simp (thing-being-raised-to-power expt)
  `(list '(mexpt simp) ,thing-being-raised-to-power ,expt))

(defmacro mexpt-base (mexpt) `(cadr ,mexpt))
(defmacro mexpt-expt (mexpt) `(caddr ,mexpt))

(defmacro make-mequal (lhs rhs) `(list '(mequal) ,lhs ,rhs))
(defmacro make-mequal-l (llist) `(cons '(mequal) ,llist))
(defmacro make-mequal-simp (lhs rhs) `(list '(mequal simp) ,lhs ,rhs))

(defmacro mequal-lhs (mequal) `(cadr ,mequal))
(defmacro mequal-rhs (mequal) `(caddr ,mequal))

(defmacro make-mlist (&rest args) `(list '(mlist) . ,args))
(defmacro make-mlist-l (llist) `(cons '(mlist) ,llist))
(defmacro make-mlist-simp (&rest args) `(list '(mlist simp) . ,args))

(defmacro make-mtext (&rest args) `(list '(mtext) . ,args))

(defmacro make-rat (&rest args) `(list '(rat) . ,args))
(defmacro make-rat-simp (&rest args) `(list '(rat simp) . ,args))
(defmacro make-rat-body (numer denom) `(cons ,numer ,denom))
(defmacro rat-numer (rat) `(cadr ,rat))
(defmacro rat-denom (rat) `(caddr ,rat))

;; Schematic of MRAT form:
;;  ((MRAT SIMP <varlist> <genvars>) <numer> . <denom>)

;; Schematic of <numer> and <denom>:
;;  (<genvar> <exponent 1> <coefficient 1> ...)

;; Representation for X^2+1:
;;  ((MRAT SIMP ($X) (G0001)) (G0001 2 1 0 1) . 1)

;; Representation for X+Y:
;;  ((MRAT SIMP ($X $Y) (G0001 G0002)) (G0001 1 1 0 (G0002 1 1)) . 1)

(defmacro mrat-body  (mrat) `(cdr ,mrat))
(defmacro mrat-numer (mrat) `(cadr ,mrat))
(defmacro mrat-denom (mrat) `(cddr ,mrat))

(defmacro make-mrat (varlist genvars numer denom)
  `((mrat ,varlist ,genvars) ,numer . ,denom))

(defmacro make-mrat-body (numer denom) `(cons ,numer ,denom))

;; Data structures used only in this file.

(defmacro trig-cannon (operator) `(get ,operator 'trig-cannon))

;; Linear equation -- cons of linear term and constant term.

(defmacro make-lineq (linear constant) `(cons ,linear ,constant))
(defmacro lineq-linear   (lineq) `(car ,lineq))
(defmacro lineq-constant (lineq) `(cdr ,lineq))

;; Solutions -- a pair of polynomial/multiplicity lists

(defmacro make-solution (wins losses) `(cons ,wins ,losses))
(defmacro solution-wins   (solution) `(car ,solution))
(defmacro solution-losses (solution) `(cdr ,solution))

;; Polynomials -- these appear in the numerator or denominator
;; of MRAT forms.  This doesn't handle the case of a coefficient
;; polynomial.

(defmacro make-mrat-poly (var terms) `(cons ,var ,terms))
(defmacro poly-var   (poly) `(car ,poly))
(defmacro poly-terms (poly) `(cdr ,poly))



