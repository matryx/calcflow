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

(macsyma-module rzmac macro)

;;;   *****************************************************************
;;;   ***** MACROS ******* ASSORTED MACROS FOR GENERAL REPRESENTATION *
;;;   *****************************************************************

(defmacro *bind* (bindings &body body)
  (nconc (list 'do (mapcar #'(lambda (q)
			       (cond ((atom q)
				      (list q))
				     ((eq (cadr q) '|<-|)
				      (list (car q) (caddr q)))
				     (t q)))
			   bindings)
	       '(nil))
	 (maplist #'(lambda (x)
		      (cond ((null (cdr x))
			     (cons 'return x))
			    ((car x))))
		  body)))

;; Returns the negation of VALUE if PREDICATE is true.  Otherwise, just
;; returns VALUE.

(defmacro negate-if (predicate value &aux (temp (gensym)))
  `(let ((,temp ,predicate))
    (if ,temp
	(neg ,value)
	,value)))

;; Setq's the first variable to VALUE if SWITCH is true, and sets the second
;; variable otherwise.

(defmacro set-either (first-var second-var switch value &aux (temp (gensym)))
  `(let ((,temp ,value))
    (if ,switch
	(setq ,first-var ,temp)
	(setq ,second-var ,temp))))

;; symbolic arithmetic macros

(defmacro m+ (&rest body) `(add* . ,body))

(defmacro m* (&rest body) `(mul* . ,body))

(defmacro m1+ (x) `(add* 1 ,x))

(defmacro m1- (x) `(add* -1 ,x))

(defmacro m// (a1 &optional (a2 nil 2args))
  (if 2args
      `(div* ,a1 ,a2)
      `(inv* ,a1)))

(defmacro m- (a1 &optional (a2 nil 2args))
  (if 2args
      `(sub* ,a1 ,a2)
      `(mul* -1 ,a1)))

(defmacro m^ (b e) `(power* ,b ,e))

(defmacro m+l (l) `(addn ,l nil))

(defmacro m*l (l) `(muln ,l nil))

(defmacro m+t (&rest body) `(add . ,body))

(defmacro m*t (&rest body) `(mul . ,body))

(defmacro m1+t (x) `(add 1 ,x))

(defmacro m1-t (x) `(add -1 ,x))

(defmacro m//t (a1 &optional (a2 nil 2args))
  (if 2args
      `(div ,a1 ,a2)
      `(inv ,a1)))

(defmacro m-t (a1 &optional (a2 nil 2args))
  (if 2args
      `(sub ,a1 ,a2)
      `(neg ,a1)))

(defmacro m^t (b e) `(power ,b ,e))

(defmacro m+lt (l) `(addn ,l ,t))

(defmacro m*lt (l) `(muln ,l ,t))
