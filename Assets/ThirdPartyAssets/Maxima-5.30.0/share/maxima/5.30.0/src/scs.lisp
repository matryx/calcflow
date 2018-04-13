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

(macsyma-module scs)

(defmfun $scsimp (expr &rest rules)
  (scs expr (mapcar #'meqhk rules)))

(defun scs (x zrs)
  (do ((flag t) (sz (conssize x)) (nx) (nsz))
      ((not flag) x)
    (do ((l zrs (cdr l)))
	((null l) (setq flag nil))
      (setq nx (subscs 0 (car l) x) nsz (conssize nx))
      (if (< nsz sz) (return (setq x nx sz nsz))))))

(defun subscs (a b c)
  (cond ((atom b) (subsc a b c))
	((eq 'mplus (caar b))
	 (do ((l (cdr b) (cdr l)) (sz (conssize c)) (nl) (nc) (nsz)) ((null l) c)
	   (setq nc (subscs (sub a (addn (revappend nl (cdr l)) t)) (car l) c)
		 nsz (conssize nc) nl (cons (car l) nl))
	   (if (< nsz sz) (setq c nc sz nsz))))
	(t (subsc a b c))))

(defun subsc (a b c)
  ($expand ($ratsubst a b c)))

(defun dstrb (x l nl)
  (revappend (mapcar #'(lambda (u) (mul x u)) l) nl))

(defmfun $distrib (exp)
  (cond ((or (mnump exp) (symbolp exp)) exp)
	((eq 'mtimes (caar exp))
	 (setq exp (mapcar '$distrib (cdr exp)))
	 (do ((l (cdr exp) (cdr l))
	      (nl (if (mplusp (car exp)) (cdar exp) (list (car exp)))))
	     ((null l) (addn nl t))
	   (if (mplusp (car l))
	       (do ((m (cdar l) (cdr m)) (ml)) ((null m) (setq nl ml))
		 (setq ml (dstrb (car m) nl ml)))
	       (setq nl (dstrb (car l) nl nil)))))
	((eq 'mequal (caar exp))
	 (list '(mequal) ($distrib (cadr exp)) ($distrib (caddr exp))))
	((eq 'mrat (caar exp)) ($distrib (ratdisrep exp)))
	(t exp)))

(defmfun $facout (x y)
  (if (mplusp y)
      (mul x (addn (mapcar #'(lambda (l) (div l x)) (cdr y)) t))
      y))
