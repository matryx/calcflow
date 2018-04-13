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
(macsyma-module logarc)

;;;  Logarc and Halfangles

(defmfun $logarc (exp)
  (cond ((atom exp) exp)
	((arcp (caar exp)) (logarc (caar exp) ($logarc (cadr exp))))
	((eq (caar exp) '$atan2)
	 (logarc '%atan2 (list ($logarc (second exp)) ($logarc (third exp)))))
	(t (recur-apply #'$logarc exp))))

(defmfun logarc (f x)
  ;; Gives the logarithmic form of arc trig and hyperbolic functions
  (cond ((eq f '%acos)
	 ;; -%i * log(x + %i*sqrt(1-x^2))
	 (mul -1 '$%i (take '(%log) (add x (mul '$%i (root (sub 1 (power x 2)) 2))))))
	((eq f '%asin)
	 ;; -%i * log(sqrt(1-x^2)+%i*x)
	 (mul -1 '$%i (take '(%log) (add (mul '$%i x) (root (sub 1 (power x 2)) 2)))))
	((eq f '%atan)
	 ;; (log(1 + %i*x) - log(1 - %i*x)) /(2 %i)
	 (div (sub (take '(%log) (add 1 (mul '$%i x))) (take '(%log) (sub 1 (mul '$%i x))))
	      (mul 2 '$%i)))
	((eq f '%atan2)
	 ;; atan2(y,x) = -%i*log((x + %i*y)/sqrt(x^2+y^2))
	 (destructuring-bind (y x)
	     x
	   (mul -1 '$%i
	        (take '(%log) (div (add x (mul '$%i y))
	                           (root (add (mul x x) (mul y y)) 2))))))
    	((eq f '%asinh)
	 ;; log(sqrt(x^2+1)+x)
	 (take '(%log) (add x (root (add 1 (power x 2)) 2))))
	((eq f '%acosh)
         ;; log(x+sqrt(x-1)*sqrt(x+1))
         (take '(%log) (add x (mul (root (add x -1) 2) (root (add x 1) 2)))))
    	((eq f '%atanh)
	 ;; (log(x+1)-log(1-x))/2
	 (div (sub (take '(%log) (add 1 x)) (take '(%log) (sub 1 x))) 2))
    	((member f '(%asec %acsc %acot %asech %acsch %acoth) :test #'eq)
	 ;; asec(x) = acos(1/x), and etc.
	 (logarc (oldget (oldget (get f '$inverse) 'recip) '$inverse) (inv x)))
	(t (merror "LOGARC: unrecognized argument: ~M" f))))

(defmfun halfangle (f a)
  (and (mtimesp a)
       (ratnump (cadr a))
       (equal (caddr (cadr a)) 2)
       (halfangleaux f (mul 2 a))))

(defun halfangleaux (f a) ;; f=function; a=twice argument
  (let ((sw (member f '(%cos %cot %coth %cosh) :test #'eq)))
    (cond ((member f '(%sin %cos) :test #'eq)
           (mul (halfangleaux-factor f a)
                (power (div (add 1 (porm sw (take '(%cos) a))) 2) (1//2))))
          ((member f '(%tan %cot) :test #'eq)
           (div (add 1 (porm sw (take '(%cos) a))) (take '(%sin) a)))
          ((member f '(%sinh %cosh) :test #'eq)
           (mul (halfangleaux-factor f a)
                (power (div (add (take '(%cosh) a) (porm sw 1)) 2) (1//2))))
	  ((member f '(%tanh %coth) :test #'eq)
	   (div (add (take '(%cosh) a) (porm sw 1)) (take '(%sinh) a)))
	  ((member f '(%sec %csc %sech %csch) :test #'eq)
	   (inv (halfangleaux (get f 'recip) a))))))

(defun halfangleaux-factor (f a)
  (cond 
    ((member f '(%sin %cos))
     (let ((arg (div (if (eq f '%sin) 
                         ($realpart a)
                         (add ($realpart a) '$%pi)) 
                     (mul 2 '$%pi))))
       (mul 
         (power -1 (simplify (list '($floor) arg)))
         (sub 1
           (mul 
             (add 1
               (power -1 (add (simplify (list '($floor) arg))
                              (simplify (list '($floor) (mul -1 arg))))))
               (simplify (list '($unit_step) (mul -1 ($imagpart a)))))))))
    ((member f '(%sinh %cosh))
     (let ((arg (div (add ($imagpart a) '$%pi) (mul 2 '$%pi)))
           (fac (if (eq f '%sinh)
                    (div (power (power a 2) (div 1 2)) a)
                    1)))
       (mul fac
         (power -1 (simplify (list '($floor) arg)))
         (sub 1
           (mul 
             (add 1
               (power -1 (add (simplify (list '($floor) arg))
                              (simplify (list '($floor) (mul -1 arg))))))
               (simplify (list '($unit_step) ($realpart a))))))))
    (t 1)))
