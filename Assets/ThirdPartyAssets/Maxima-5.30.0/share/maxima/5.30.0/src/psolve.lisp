;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(in-package :maxima)

(macsyma-module psolve)

(declare-top (special mult *roots *failures $solvefactors))

(defmvar flag4 nil)

(defmfun solvecubic (x) 
  (prog (s1 a0 a1 a2 discr lcoef adiv3 omega^2 pdiv3 qdiv-2
	 omega y1 u y2) 
     (setq x (cdr x))
     (setq lcoef (cadr x))
     (setq adiv3
	   (list '(mtimes)
		 '((rat) -1 3)
		 (rdis (setq a2 (ratreduce (pterm x 2)
					   lcoef)))))
     (setq a1 (ratreduce (pterm x 1) lcoef))
     (setq a0 (ratreduce (pterm x 0) lcoef))
     (setq s1 '((mtimes)
		((rat) 1 2)
		$%i
		((mexpt) 3 ((rat) 1 2))))
     (setq omega (list '(mplus)
		       '((rat) -1 2)
		       s1) 
	   omega^2 (list '(mplus)
			 '((rat) -1 2)
			 (list '(mtimes) -1 s1)))
     (setq pdiv3
	   (meval* (rdis (ratplus (rattimes a1 '(1 . 3) t)
				  (rattimes (ratexpt a2 2)
					    '(-1 . 9)
					    t)))))
     (and (not (equal pdiv3 0)) (go harder))
     (setq y1
	   (simptimes
	    (list
	     '(mtimes)
	     '((rat) 1 3)
	     (list '(mplus)
		   (simpnrt (rdis (setq y2 (ratplus (ratexpt a2 3)
						    (rattimes '(-27 . 1) a0 t))))
			    3)
		   (list '(mtimes) -1 (rdis a2))))
	    1
	    nil))
     (and flag4 (return (solve3 y1 mult)))
     (setq y2 (simpnrt (rdis (rattimes  y2 '(1 . 27) t)) 3))
     (return (mapc #'(lambda (j) (solve3 j mult))
		   (list y1
			 (list '(mplus)
			       (list '(mtimes) omega y2)
			       adiv3)
			 (list '(mplus)
			       (list '(mtimes) omega^2 y2)
			       adiv3))))
     harder
     (setq qdiv-2
	   (rdis (ratplus (rattimes (ratplus (rattimes a1 a2 t)
					     (rattimes '(-3 . 1) a0 t))
				    '(1 . 6)
				    t)
			  (rattimes (ratexpt a2 3)
				    '(-1 . 27)
				    t))))
     (cond ((equal qdiv-2 0)
	    (setq u (simpnrt pdiv3 2))
	    (setq y1 adiv3))
	   (t (setq discr (simplus (list '(mplus)
					 (list '(mexpt) pdiv3 3)
					 (list '(mexpt) qdiv-2 2))
				   1
				   nil))
	      (cond ((equal discr 0)
		     (setq u (simpnrt qdiv-2 3)))
		    (t (setq discr (simpnrt discr 2))
		       (and (complicated discr)
			    (setq discr (adispline discr)))
		       (setq u (simpexpt (list '(mexpt)
					       (list '(mplus)
						     qdiv-2
						     discr)
					       '((rat) 1 3)) 1 nil))
		       (and (complicated u)
			    (setq u (adispline u)))))))
     (if (equal u 0) (merror (intl:gettext "SOLVECUBIC: arithmetic overflow.")))
     (or y1
	 (setq y1 (simplus (list '(mplus)
				 adiv3
				 u
				 (list '(mtimes)
				       -1
				       pdiv3
				       (list '(mexpt) u -1)))
			   1
			   nil)))
     (return
       (cond (flag4 (solve3 y1 mult))
	     (t (mapc 
		 #'(lambda (j) (solve3 j mult))
		 (list y1
		       (list '(mplus)
			     adiv3
			     (list '(mtimes) omega u)
			     (list '(mtimes)
				   -1
				   pdiv3
				   omega^2
				   (list '(mexpt) u -1)))
		       (list '(mplus)
			     adiv3
			     (list '(mtimes) omega^2 u)
			     (list '(mtimes)
				   -1
				   pdiv3
				   omega
				   (list '(mexpt) u -1))))))))))

(defmfun solvequartic (x) 
  (prog (a0 a1 a2 b1 b2 b3 b0 lcoef z1 r tr1 tr2 d d1 e sqb3) 
     (setq x (cdr x) lcoef (cadr x))
     (setq b3 (ratreduce (pterm x 3) lcoef))
     (setq b2 (ratreduce (pterm x 2) lcoef))
     (setq b1 (ratreduce (pterm x 1) lcoef))
     (setq b0 (ratreduce (pterm x 0) lcoef))
     (setq a2 (ratminus b2))
     (setq a1 (ratdif (rattimes b1 b3 t)
		      (setq a0 (rattimes b0 '(4 . 1) t))))
     (setq a0
	   (ratdif (ratdif (rattimes b2 a0 t)
			   (rattimes (setq sqb3
					   (ratexpt b3 2))
				     b0
				     t))
		   (ratexpt b1 2.)))
     (setq tr2
	   (simplify (rdis
		      (rattimes
		       '(1 . 4)
		       (ratdif (ratdif (rattimes b3
						 (rattimes b2 '(4 . 1) t)
						 t)
				       (rattimes '(8 . 1) b1 t))
			       (rattimes sqb3 b3 nil))
		       t))))
     (setq z1 (resolvent a2 a1 a0))
     (setq r
	   (simplus (list '(mplus)
			  z1
			  (rdis (ratdif (rattimes sqb3
						  '(1. . 4.)
						  t)
					b2)))
		    1
		    nil))
     (setq r (simpnrt r 2))
     (and (equal r 0) (go l0))
     (and (complicated r) (setq r (adispline r)))
     (and (complicated tr2) (setq tr2 (adispline tr2)))
     (setq tr1
	   (simplus (list '(mplus)
			  (rdis (ratdif (rattimes sqb3 '(1 . 2) t)
					b2))
			  (list '(mtimes) -1 z1))
		    1
		    nil))
     (and (complicated tr1) (setq tr1 (adispline tr1)))
     (setq tr2 (div* tr2 r))
     (go lb1)
     l0   (setq d1
		(simpnrt (simplify (list '(mplus)
					 (list '(mexpt) z1 2)
					 (list '(mtimes)
					       -4
					       (rdis b0))))
			 2))
     (setq tr2 (simplify (list '(mtimes) 2. d1)))
     (and (complicated tr2) (setq tr2 (adispline tr2)))
     (setq tr1
	   (simplify (rdis (ratdif (rattimes sqb3 '(3 . 4) t)
				   (rattimes b2 '(2 . 1) t)))))
     (and (complicated tr1) (setq tr1 (adispline tr1)))
  lb1
     (setq d (div (power (add tr1 tr2) '((rat simp) 1 2)) 2))
     (setq e (div (power (sub tr1 tr2) '((rat simp) 1 2)) 2))
     (and (complicated d) (setq d (adispline d)))
     (and (complicated e) (setq e (adispline e)))
     (setq a2 (rdis (rattimes b3 '(-1 . 4) t)))
     (setq a1 (div* r 2))
     (setq z1
	   (list (list '(mplus) a2 a1 d)
		 (list '(mplus)
		       a2
		       a1
		       (list '(mtimes) -1 d))
		 (list '(mplus)
		       a2
		       (list '(mtimes) -1 a1)
		       e)
		 (list '(mplus)
		       a2
		       (list '(mtimes) -1 a1)
		       (list '(mtimes) -1 e))))
     (return (mapc #'(lambda (j) (solve3 j mult)) z1))))

;;; SOLVES RESOLVENT CUBIC EQUATION
;;; GENERATED FROM QUARTIC

(defun resolvent (a2 a1 a0) 
  (prog (*roots flag4 *failures $solvefactors) ;undoes binding in
     (setq flag4 t $solvefactors t)	       ;algsys
     (solve (simplus (list '(mplus)
			   (list '(mexpt) 'yy 3)
			   (list '(mtimes)
				 (rdis a2)
				 (list '(mexpt) 'yy 2))
			   (list '(mtimes)
				 (rdis a1)
				 'yy)
			   (rdis a0))
		     1
		     nil)
	    'yy
	    1)
     (when (member 0 *roots :test #'equal) (return 0))
     (return (caddar (cdr (reverse *roots))))))

(declare-top (unspecial mult))
