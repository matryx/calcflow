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
(macsyma-module ufact)

(declare-top (special modulus coef-type))

(load-macsyma-macros ratmac rzmac)

;; Dense Polynomial Representation

(defun dprep (p)
  (do ((n (car p))
       (e (car p) (f1- e))
       (l))
      ((< e 0) (cons n (nreverse l)))
    (cond ((equal e (car p))
	   (push (cadr p) l)
	   (setq p (cddr p)))
	  (t (push 0 l)))))

(defun dpdisrep (l)
  (cond ((zerop (car l)) (cadr l))
	((do ((l (nreverse (cdr l)) (cdr l))
	      (n 0 (f1+ n))
	      (ll))
	     ((null l) ll)
	   (or (= (car l) 0)
	       (setq ll (cons n (cons (car l) ll))))))))

;; not currently called
;;(DEFUN PGCDU* (P Q)
;;       (COND ((OR (PCOEFP P) (PCOEFP Q))  1)
;;	     ((NULL MODULUS)
;;	      (merror  "Illegal CALL TO PGCDU"))
;;	     ((> (CADR P) (CADR Q)) 
;;	      (PSIMP (CAR P) (DPDISREP (DPGCD (DPREP (CDR P)) (DPREP (CDR Q))))))
;;	     ((PSIMP (CAR P) (DPDISREP (DPGCD (DPREP (CDR Q)) (DPREP (CDR P))))))))
;;
;;(DEFUN PMODSQFRU (P)
;;       (DO ((DPL (DPSQFR (DPREP (CDR P))) (CDR DPL))
;;	    (PL NIL (CONS (PSIMP (CAR P) (DPDISREP (CDAR DPL))) (CONS (CAAR DPL) PL))))
;;	   ((NULL DPL) PL)))

(defun dpgcd (p q)
  (if (< (car p) (car q)) (exch p q))
  (do ((p (copy-list p) q)
       (q (copy-list q) (dpremquo p q nil)))
      ((= (car q) 0)
       (if (= (cadr q) 0) p '(0 1)))))

(defun dpdif (p q)
  (cond ((> (car p) (car q))
	 (do ((i (car p) (f1- i))
	      (pl (cdr p) (cdr pl))
	      (l nil (cons (car pl) l)))
	     ((= i (car q)) (dpdif1 pl (cdr q) l)) ))
	((< (car p) (car q))
	 (do ((i (car q) (f1- i))
	      (ql (cdr q) (cdr ql))
	      (l nil (cons (cminus (car ql)) l)))
	     ((= i (car p)) (dpdif1 (cdr p) ql l))))
	(t (dpdif1 (cdr p) (cdr q) nil))))

(defun dpdif1 (p1 q1 l)
  (do ((pl p1 (cdr pl))
       (ql q1 (cdr ql))
       (ll l (cons (cdifference (car pl) (car ql)) ll)))
      ((null pl) (dpsimp (nreverse ll)))))

(defun dpsimp (pl) (setq pl (ufact-strip-zeroes pl))
       (cond ((null pl) '(0 0))
	     (t (cons (f1- (length pl)) pl))))

(defun dpderiv (p)
  (cond ((= 0 (car p)) '(0 0))
	(t (do ((l (cdr p) (cdr l))
		(i (car p) (f1- i))
		(dp nil (cons (ctimes i (car l)) dp)))
	       ((= i 0) (cons (f1- (car p)) (nreverse dp)))))))

(defun dpsqfr (q)			;ASSUMES MOD > DEGREE
  (do ((c q (dpmodquo c p))
       (d (dpderiv q) (dpmodquo d p))
       (i 0 (f1+ i))
       (p)
       (pl))
      ((= 0 (car c)) pl)
    (cond (p (setq d (dpdif d (dpderiv c))
		   p (dpgcd c d))
	     (and (> (car p) 0)
		  (setq pl (cons (cons i p) pl))))
	  (t (setq p (dpgcd c d))
	     (cond ((= (car p) 0) (return (ncons (cons 1 c)))))))))



(defun dpmodrem (p q)
  (cond ((< (car p) (car q)) p)
	((= (car q) 0) '(0 0))
	((dpremquo (copy-list p) (copy-list q) nil))))
  
(defun dpmodquo (p q)
  (cond ((< (car p) (car q)) '(0 0))
	((= (car q) 0)
	 (cond ((equal (cadr q) 1) p)
	       (t (cons (car p)
			(mapcar #'(lambda (c) (cquotient c (cadr q))) (cdr p))
			))))
	((dpremquo (copy-list p) (copy-list q) t))))
  
;; If FLAG is T, return quotient.  Otherwise return remainder.

(defun dpremquo (p q flag)
  (prog (lp lq l alpha)
     (cond ((= (cadr q) 1)
	    (setq alpha 1))
	   (t (setq alpha (crecip (cadr q)))
	      (do ((l (cddr q) (cdr l)))
		  ((null l)
		   (rplaca (cdr q) 1))
		(rplaca l (ctimes (car l) alpha)))))
     a    (and flag (setq l (cons (ctimes (cadr p) alpha) l)))
     (setq lp (cddr p) lq (cddr q))
     b    (rplaca lp (cdifference (car lp) (ctimes (car lq) (cadr p))))
     (cond ((null (setq lq (cdr lq)))
	    (do ((e (f1- (car p)) (f1- e))
		 (pp (cddr p) (cdr pp)))
		((null pp) (setq p '(0 0)))
	      (cond ((signp e (car pp))
		     (and flag (not (< e (car q)))
			  (setq l (cons 0 l))))
		    ((return (setq p (cons e pp))))))
	    (cond ((< (car p) (car q))
		   (return (cond (flag (dpsimp (nreverse l)));GET EXP?
				 (p))))
		  ((go a))))
	   (t (setq lp (cdr lp))
	      (go b)))))

(defun ufact-strip-zeroes (l)
  (do ((l l (cdr l)))
      ((null (czerop (car l))) l)))

(defun cpres1 (a b)
  (prog (res (v 0) a3) (declare (fixnum v))
	(setq  a (dprep a) b (dprep b))
	(setq res 1)
	again (setq a3 (dpmodrem a b))
	(setq v (boole boole-xor v (logand 1 (car a) (car b) )))
	(setq res (ctimes res (cexpt (cadr b)
				     (f- (car a) (car a3)))))
	(cond ((= 0 (car a3))
	       (setq res (ctimes res (cexpt (cadr a3) (car b))))
	       (return (cond ((oddp v) (cminus res))
			     (t res))) ))
	(setq a b)
	(setq b a3)
	(go again) ))
