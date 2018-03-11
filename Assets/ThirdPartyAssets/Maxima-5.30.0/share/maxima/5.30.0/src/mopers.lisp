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

(macsyma-module mopers macro)

;; This file is the compile-time half of the OPERS package, an interface to the
;; Maxima general representaton simplifier.  When new expressions are being
;; created, the macros in this file or the functions in NOPERS should be called
;; rather than the entrypoints in SIMP such as SIMPLIFYA or SIMPLUS.

;; The basic functions are ADD, SUB, MUL, DIV, POWER, NCMUL, NCPOWER, INV.
;; Each of these functions assume that their arguments are simplified.  Some
;; functions will have a "*" adjoined to the end of the name (as in ADD*).
;; These do not assume that their arguments are simplified.  The above
;; functions are the only entrypoints to this package.

;; The functions ADD2, MUL2, and MUL3 are for use internal to this package
;; and should not be called externally.

;; I have added the macro DEFGRAD as an interface to the $DERIVATIVE function
;; for use by macsyma programers who want to do a bit of lisp programming. -GJC

(defmacro =0 (x) `(equal ,x 0))
(defmacro =1 (x) `(equal ,x 1))

;; Addition -- call ADD with simplified operands,
;;             ADD* with unsimplified operands.

(defun add (&rest terms)
  (if (= (length terms) 2)
      (apply #'add2 terms)
      (apply #'addn `(,terms t))))

(define-compiler-macro add (&rest terms)
  (if (= (length terms) 2)
      `(add2 ,@terms)
      `(addn (list ,@terms) t)))

(defun add* (&rest terms)
  (if (= (length terms) 2)
      (apply #'add2* terms)
      (apply #'addn `(,terms nil))))

(define-compiler-macro add* (&rest terms)
  (if (= (length terms) 2)
      `(add2* ,@terms)
      `(addn (list ,@terms) nil)))

;; Multiplication -- call MUL or NCMUL with simplified operands,
;;                        MUL* or NCMUL* with unsimplified operands.

(defun mul (&rest factors)
  (cond ((= (length factors) 2) (apply #'mul2 factors))
        ((= (length factors) 3) (apply #'mul3 factors))
        (t (apply #'muln `(,factors t)))))

(define-compiler-macro mul (&rest factors)
  (cond ((= (length factors) 2) `(mul2 ,@factors))
	((= (length factors) 3) `(mul3 ,@factors))
	(t `(muln (list ,@factors) t))))

(defun mul* (&rest factors)
  (if (= (length factors) 2)
      (apply #'mul2* factors)
      (apply #'muln `(,factors nil))))

(define-compiler-macro mul* (&rest factors)
  (if (= (length factors) 2)
      `(mul2* ,@factors)
      `(muln (list ,@factors) nil)))

(defmacro inv (x)
  `(power ,x -1))

(defmacro inv* (x)
  `(power* ,x -1))

(defmacro ncmul (&rest factors)
  (if (= (length factors) 2)
      `(ncmul2 ,@factors)
      `(ncmuln (list ,@factors) t)))

;; (TAKE '(%TAN) X) = tan(x)
;; This syntax really loses.  Not only does this syntax lose, but this macro
;; has to look like a subr.  Otherwise, the definition would look like
;; (DEFMACRO TAKE ((NIL (OPERATOR)) . ARGS) ...)

;; (TAKE A B) --> (SIMPLIFYA (LIST A B) T)
;; (TAKE '(%SIN) A) --> (SIMP-%SIN (LIST '(%SIN) A) 1 T)

(defmacro take (operator &rest args)
; Cutting out the code which bypasses the simplifier.
;  (let ((simplifier (and (not (atom operator))
;			 (eq (car operator) 'quote)
;			 (cdr (assoc (caadr operator) '((%atan  . simp-%atan)
;							(%tan   . simp-%tan)
;							(%log   . simpln)
;							(mabs   . simpabs)
;							(%sin   . simp-%sin)
;							(%cos   . simp-%cos)
;							($atan2 . simpatan2)) :test #'eq)))))
;    (if simplifier
;	`(,simplifier (list ,operator ,@args) 1 t)
	`(simplifya (list ,operator ,@args) t))

;; take* does not assume that the arguments are simplified.
(defmacro take* (operator &rest args)
  `(simplifya (list ,operator ,@args) nil))

(defmacro min%i () ''((mtimes simp) -1 $%i)) ;-%I
(defmacro 1//2 () ''((rat simp) 1 2))	;1/2
(defmacro half () ''((rat simp) 1 2))	;1/2
(defmacro i//2 () ''((mtimes simp) ((rat simp) 1 2) $%i)) ;%I/2

(declaim (inline simplify))
(defun simplify (x)
  (simplifya x nil))

;; A hand-made DEFSTRUCT for dealing with the Maxima MDO structure.
;; Used in GRAM, etc. for storing/retrieving from DO structures.

(defmacro make-mdo () '(list (list 'mdo) nil nil nil nil nil nil nil))

(defmacro mdo-op (x)     `(car (car ,x)))

(defmacro mdo-for (x)    `(second ,x))
(defmacro mdo-from (x)   `(third ,x))
(defmacro mdo-step (x)   `(fourth ,x))
(defmacro mdo-next (x)   `(fifth ,x))
(defmacro mdo-thru (x)   `(sixth ,x))
(defmacro mdo-unless (x) `(seventh ,x))
(defmacro mdo-body (x)	 `(eighth ,x))

(defmacro defgrad (name arguments &body body)
  `(defprop ,name (,arguments ,@body) grad))
