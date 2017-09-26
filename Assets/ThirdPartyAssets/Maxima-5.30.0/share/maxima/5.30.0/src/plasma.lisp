;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;; Plasma Dispersion Function, NZETA(Z).
;;
;; This function is related to the complex error function by
;;
;;    NZETA(Z) = %I*SQRT(%PI)*EXP(-Z^2)*(1-ERF(-%I*Z))
;;
;; NZETA(Z)  returns the complex value of the Plasma Dispersion Function 
;; for complex Z. 
;; 
;; NZETAR(Z)  returns REALPART(NZETA(Z)).
;;
;; NZETAI(Z)  returns IMAGPART(NZETA(Z)).

(defun z-function (x y)
  (let ((xs (if (> 0.0 x) -1.0 1.0))
	(ys (if (> 0.0 y) -1.0 1.0))
	(capn 0) (nu 0)
	(bool nil)
	(h 0.0) (h2 0.0) (lamb 0.0) (r1 0.0) (r2 0.0) (s 0.0)
	(s1 0.0) (s2 0.0) (t1 0.0) (t2 0.0) (c 0.0)
	(re 0.0) (im 0.0))
    (setq x (abs x) y (abs y))
    (cond ((and (> 4.29 y) (> 5.33 x))
	   (setq s (* (1+ (* -0.23310023 y))
		      (sqrt (1+ (* -0.035198873 x x)))))
	   (setq h (* 1.6 s) h2 (* 2.0 h) capn (+ 6 (floor (* 23.0 s))))
	   (setq nu (+ 9 (floor (* 21.0 s)))))
	  (t (setq h 0.0) (setq capn 0) (setq nu 8)))
    (when (> h 0.0) (setq lamb (expt h2 capn)))
    (setq bool (or (zerop h) (zerop lamb)))
    (do ((n nu (1- n)))
	((> 0 n))
      (setq t1 (+ h (* (float (1+ n)) r1) y))
      (setq t2 (- x (* (float (1+ n)) r2)))
      (setq c (/ 0.5 (+ (* t1 t1) (* t2 t2))))
      (setq r1 (* c t1) r2 (* c t2))
      (cond ((and (> h 0.0) (not (< capn n)))
	     (setq t1 (+ s1 lamb) s1 (- (* r1 t1) (* r2 s2)))
	     (setq s2 (+ (* r1 s2) (* r2 t1)) lamb (/ lamb h2)))))
    (setq im (if (zerop y)
		 (* 1.77245384 (exp (- (* x x))))
		 (* 2.0 (if bool r1 s1))))
    (setq re (* -2.0 (if bool r2 s2)))
    (cond ((> ys 0.0) (setq re (* re xs)))
	  (t (setq r1 (* 3.5449077 (exp (- (* y y) (* x x)))))
	     (setq r2 (* 2.0 x y))
	     (setq re (* (- re (* r1 (sin r2))) xs))
	     (setq im (- (* r1 (cos r2)) im))))
    `((mlist simp) ,re ,im)))

(defun $nzeta (z)
  (let ((x ($realpart z))
	(y ($imagpart z)))
     (if (and (numberp x) (numberp y))
	 (let ((w (z-function (float x) (float y))))
	   (simplify `((mplus) ,(second w) ,(simplify `((mtimes) $%i ,(third w))))))
	 `(($nzeta simp) ,z))))

(defun $nzetar (z)
  (let ((x ($realpart z))
	(y ($imagpart z)))
     (if (and (numberp x) (numberp y))
	 (second (z-function (float x) (float y)))
	 `(($nzetar simp) ,z))))

(defun $nzetai (z)
  (let ((x ($realpart z))
	(y ($imagpart z)))
     (if (and (numberp x) (numberp y))
	 (third (z-function (float x) (float y)))
	 `(($nzetai simp) ,z))))
