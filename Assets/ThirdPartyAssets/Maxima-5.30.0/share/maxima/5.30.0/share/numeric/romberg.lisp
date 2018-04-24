;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;; Original code by CFFK.  Modified to interface correctly with TRANSL  ;;;
;;; and the rest of macsyma by GJC                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module rombrg)

(load-macsyma-macros transm)

(declare-top(special user-timesofar))

(defmvar  $rombergit 11 "the maximum number of iterations")
(defmvar  $rombergmin 0 "the minimum number of iterations")
(defmvar  $rombergtol 1e-4 "the relative tolerance of error")	
(defmvar  $rombergabs 0.0 "the absolute tolerance of error")
(defmvar  $rombergit_used 0 "the number of iterations actually used.")

(defun romberg-subr (f left right)
   (let (a b
	 (x 0.0)
	 (tt (make-array  $rombergit :element-type 'flonum))
	 (rr  (make-array  $rombergit :element-type 'flonum)))
     (let (($numer t) ($%enumer t))
       (setq
         a ($float left)
         b ($float right)))
     (if (not (and (numberp a) (numberp b)))
       (return-from romberg-subr (values nil a b)))
     (setq x (- b a))
     (let
       ((fa (funcall f a))
        (fb (funcall f b))
        (fab (funcall f (* (+ b a) 0.5))))
       (if
         (or
           (not (numberp fa))
           (not (numberp fb))
           (not (numberp fab)))
         (return-from romberg-subr (values nil a b))
         (progn
           (setf (aref tt 0) (* x (+ fb fa) 0.5))
           (setf (aref rr 0) (* x fab)))))

     (do ((l 1 (1+ l))
	  (m 4 (* m 2))
	  (y 0.0)
	  (z 0.0)
	  (cerr 0.0))
	 ((= l $rombergit)
	  (merror "`romberg' failed to converge"))
       (declare	(fixnum l m))
       (setq y (float m) z (/ x y))
       (setf (aref tt l) (* (+ (aref tt (1- l))
			       (aref rr (1- l))) 0.5))
       (setf (aref rr l) 0.0)
       (do ((i 1 (+ i 2)))
	   ((> i m))
       (let
         ((fzi+a (funcall f (+ (* z (float i)) a))))
         (if (not (numberp fzi+a))
           (return-from romberg-subr (values nil a b))
           (setf (aref rr l) (+ fzi+a (aref rr l))))))

       (setf (aref rr l) (* z (aref rr l) 2))
       (setq y 0.0)
       (do ((k l (1- k))) ((= k 0))
	 (declare (fixnum k))
	 (setq y (+ (* y 4) 3))
	 (setf (aref  tt (1- k))
	       (+ (/ (- (aref tt k)
			(aref tt (1- k))) y)
		  (aref tt k)))
	 (setf (aref rr (1- k))
	       (+ (/ (- (aref rr k) (aref rr (1- k))) y)
		  (aref rr k))))
       (setq y (* (+ (aref tt 0) (aref rr 0)) 0.5))
;;; this is the WIN condition test.
       (cond ((and (or (not (< $rombergabs
			       (setq cerr (abs (- (aref tt 0) (aref rr 0))))))
		       (not (< $rombergtol
			       ;; cerr = "calculated error"; used for ^]
			       (setq cerr (/ cerr (if (zerop y) 1.0 (abs y)))))))
		   (> l $rombergmin))
	      (setq $rombergit_used l)
	      (return-from romberg-subr (values y nil nil)))))))

(defun $romberg (&rest args)
  (cond
    ((= (length args) 3)
     (multiple-value-bind
       (result left right)
       ;; BIND EVIL SPECIAL VARIABLE *PLOT-REALPART* HERE ...
       (let (($numer t) ($%enumer t) (*plot-realpart* nil))
         (romberg-subr
           (coerce-float-fun (first args))
           (second args)
           (third args)))
       (if (numberp result)
         result
         `(($romberg) ,(first args) ,left ,right))))
    ((= (length args) 4)
     (multiple-value-bind
       (result left right)
       ;; BIND EVIL SPECIAL VARIABLE *PLOT-REALPART* HERE ...
       (let (($numer t) ($%enumer t) (*plot-realpart* nil))
         (romberg-subr
           (coerce-float-fun (first args) `((mlist) ,(second args)))
           (third args)
           (fourth args)))
       (if (numberp result)
         result
         `(($romberg) ,(first args) ,(second args) ,left ,right))))
    (t
      (wna-err '$romberg))))
