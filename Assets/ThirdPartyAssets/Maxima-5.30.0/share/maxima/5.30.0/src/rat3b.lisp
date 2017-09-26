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

(macsyma-module rat3b)

;;	THIS IS THE NEW RATIONAL FUNCTION PACKAGE PART 2.
;;	IT INCLUDES RATIONAL FUNCTIONS ONLY.

(declare-top (special $algebraic $ratfac $keepfloat $float))

(load-macsyma-macros ratmac)

(defmvar $ratwtlvl nil) 
(defmvar $ratalgdenom t)       ;If T then denominator is rationalized.

(defun ralgp (r) (or (palgp (car r)) (palgp (cdr r))))

(defun palgp (poly)
  (cond ((pcoefp poly) nil)
	((alg poly) t)
	(t (do ((p (cdr poly) (cddr p)))
	       ((null p))
	     (and (palgp (cadr p)) (return t))))))


(defun ratdx (e *x*)
  (declare (special *x*))
  (prog (varlist flag v* genvar *a a trunclist)
     (declare (special v* *a flag trunclist))
     (and (member 'trunc (car e) :test #'eq) (setq trunclist (cadddr (cdar e))))
     (cond ((not (eq (caar e) (quote mrat))) (setq e (ratf e))))
     (setq varlist (caddar e))
     (setq genvar (car (cdddar e)))
     ;; Next cond could be flushed if genvar would shrink with varlist
     (cond ((> (length genvar) (length varlist))
	    ;; Presumably this produces a copy of GENVAR which has the
	    ;; same length as VARLIST.  Why not rplacd?
	    (setq genvar (mapcar #'(lambda (a b) (declare (ignore a)) b)
				 varlist genvar))))
     (setq *x* (fullratsimp *x*))
     (newvar *x*) 
     (setq a (mapcan #'(lambda (z)
			 (prog (ff)
			    (newvar 
			     (setq ff (fullratsimp (sdiff z *x*))))
			    (orderpointer varlist)
			    (return (list z ff)))) varlist))
     (setq *a (cons nil a))
     (mapc #'(lambda(z b)
	       (cond ((null (old-get *a z))(putprop b (rzero) 'diff))
		     ((and(putprop b(cdr (ratf (old-get *a z))) 'diff)
			  (alike1 z *x*))
		      (setq v*  b))
		     (t (setq flag t)))) varlist genvar)

     ;;; causing lisp error - [ 2010843 ] diff of Taylor poly
     ;;(cond ((and (signp n (cdr (old-get trunclist v*)))
     ;;		 (car (old-get trunclist v*))) (return 0)))	     

     (and trunclist
	  (return (cons (list 'mrat 'simp varlist genvar trunclist 'trunc)
			(cond (flag (psdp (cdr e)))
			      (t (psderivative (cdr e) v*))))))
     (return (cons (list 'mrat 'simp varlist genvar)
		   (cond (flag (ratdx1 (cadr e) (cddr e)))
			 (t (ratderivative (cdr e) v*)))))))

(defun ratdx1 (u v)
  (ratquotient (ratdif (rattimes (cons v 1) (ratdp u) t)
		       (rattimes (cons u 1) (ratdp v) t))
	       (cons (pexpt v 2) 1)))

(defun ratdp (p)
  (cond ((pcoefp p) (rzero))
	((rzerop (get (car p) 'diff))
	 (ratdp1 (cons (list (car p) 'foo 1) 1) (cdr p)))
	(t (ratdp2 (cons (list (car p) 'foo 1) 1)
		   (get (car p) 'diff)
		   (cdr p)))))

(defun ratdp1 (x v)
  (cond ((null v) (rzero))
	((eqn (car v) 0) (ratdp (cadr v)))
	(t (ratplus (rattimes (subst (car v) 'foo x) (ratdp (cadr v)) t)
		    (ratdp1 x (cddr v))))))

(defun ratdp2 (x dx v)
  (cond ((null v) (rzero))
	((eqn (car v) 0) (ratdp (cadr v)))
	((eqn (car v) 1)
	 (ratplus (ratdp2 x dx (cddr v))
		  (ratplus (rattimes dx (cons (cadr v) 1) t)
			   (rattimes (subst 1 'foo x)
				     (ratdp (cadr v)) t))))
	(t (ratplus (ratdp2 x dx (cddr v))
		    (ratplus (rattimes dx
				       (rattimes (subst (1- (car v))
							'foo
							x)
						 (cons (ptimes (car v)
							       (cadr v))
						       1)
						 t)
				       t)
			     (rattimes (ratdp (cadr v))
				       (subst (car v) (quote foo) x)
				       t))))))

(defmfun ratderivative (rat  var)
  (let ((num (car rat))
	(denom (cdr rat)))
    (cond ((eqn 1 denom) (cons (pderivative num var) 1))
	  (t (setq denom (pgcdcofacts denom (pderivative denom var)))
	     (setq num (ratreduce (pdifference (ptimes (cadr denom)
						       (pderivative num var))
					       (ptimes num (caddr denom)))
					;RATREDUCE ONLY NEEDS TO BE DONE WITH CONTENT OF GCD WRT VAR.
				  (car denom)))
	     (cond ((pzerop (car num)) num)
		   (t (rplacd num (ptimes (cdr num)
					  (pexpt (cadr denom) 2)))))))))

(defmfun ratdif (x y)
  (ratplus x (ratminus y))) 

(defmfun ratfact (x fn)
  (cond ((and $keepfloat (or (pfloatp (car x)) (pfloatp (cdr x)))
	      (setq fn 'floatfact) nil))
	((not (equal (cdr x) 1))
	 (nconc (funcall fn (car x)) (fixmult (funcall fn (cdr x)) -1)))
	(t (funcall fn (car x)))))
	 
(defun floatfact (p)
  (destructuring-let (((cont primp) (ptermcont p)))
    (setq cont (monom->facl cont))
    (cond ((equal primp 1) cont)
	  (t (append cont (list primp 1))))))

(defun ratinvert (y)
  (ratalgdenom
   (cond ((pzerop (car y)) (errrjf "`quotient' by `zero'"))
	 ((and modulus (pcoefp (car y)))
	  (cons (pctimes (crecip (car y)) (cdr y)) 1))
	 ((and $keepfloat (floatp (car y)))
	  (cons (pctimes (/ (car y)) (cdr y)) 1))
	 ((pminusp (car y)) (cons (pminus (cdr y)) (pminus (car y))))
	 (t (cons (cdr y) (car y))))))

(defmfun ratminus (x)
  (cons (pminus (car x)) (cdr x)))
	 
(defun ratalgdenom (x)
  (cond ((not $ratalgdenom) x)
	((pcoefp (cdr x)) x)
	((and (alg (cdr x))
	      (let ((errrjfflag t))
		(catch 'raterr
		  (rattimes (cons (car x) 1)
			    (rainv (cdr x)) t)))))
	(t x)))

(defmfun ratreduce (x y &aux b)
  (cond ((pzerop y) (errrjf "`quotient' by `zero'"))
	((pzerop x) (rzero))
	((eqn y 1) (cons x 1))
	((and $keepfloat (pcoefp y) (or $float (floatp y) (pfloatp x)))
	 (cons (pctimes (quotient 1.0 y) x) 1))
	(t (setq b (pgcdcofacts x y))
	   (setq b (ratalgdenom (rplacd (cdr b) (caddr b))))
	   (cond ((and modulus (pcoefp (cdr b)))
		  (cons (pctimes (crecip (cdr b)) (car b)) 1))
		 ((pminusp (cdr b))
		  (cons (pminus (car b)) (pminus (cdr b))))
		 (t b)))))

(defun ptimes* (p q)
  (cond ($ratwtlvl (wtptimes p q 0))
	(t (ptimes p q))))

(defmfun rattimes (x y gcdsw)
  (cond ($ratfac (facrtimes x y gcdsw))
	((and $algebraic gcdsw (ralgp x) (ralgp y))
	 (let ((w  (rattimes x y nil)))
	   (ratreduce (car w) (cdr w))))
	((eqn 1 (cdr x))
	 (cond ((eqn 1 (cdr y)) (cons (ptimes* (car x) (car y)) 1))
	       (t (cond (gcdsw (rattimes (ratreduce (car x) (cdr y))
					 (cons (car y) 1) nil))
			(t (cons (ptimes* (car x) (car y)) (cdr y)))))))
	((eqn 1 (cdr y)) (rattimes y x gcdsw))
	(t (cond (gcdsw (rattimes (ratreduce (car x) (cdr y))
				  (ratreduce (car y) (cdr x)) nil))
		 (t (cons (ptimes* (car x) (car y))
			  (ptimes* (cdr x) (cdr y))))))))
	  
(defmfun ratexpt (x n)
  (cond ((equal n 0) '(1 . 1))
	((equal n 1) x)
	((minusp n) (ratinvert (ratexpt x (- n))))
	($ratwtlvl (ratreduce (wtpexpt (car x) n) (wtpexpt (cdr x) n)))
	($algebraic (ratreduce (pexpt (car x) n) (pexpt (cdr x) n)))
	(t (cons (pexpt (car x) n) (pexpt (cdr x) n)))))

(defmfun ratplus (x y &aux q n)
  (cond ($ratfac (facrplus x y))
	($ratwtlvl
	 (ratreduce
	  (pplus (wtptimes (car x) (cdr y) 0)
		 (wtptimes (car y) (cdr x) 0))
	  (wtptimes (cdr x) (cdr y) 0)))
	((and $algebraic (ralgp x) (ralgp y))
	 (ratreduce
	  (pplus (ptimeschk (car x) (cdr y))
		 (ptimeschk (car y) (cdr x)))
	  (ptimeschk (cdr x) (cdr y))))
	((eqn 1 (cdr x))
	 (cond ((eqn 0 (car x)) y)
	       ((eqn 1 (cdr y)) (cons (pplus (car x) (car y)) 1))
	       (t (cons (pplus (ptimes (car x) (cdr y)) (car y)) (cdr y)))))
	((eqn 1 (cdr y))
	 (cond ((eqn 0 (car y)) x)
	       (t (cons (pplus (ptimes (car y) (cdr x)) (car x)) (cdr x)))))
	(t (setq q (pgcdcofacts (cdr x) (cdr y)))
	   (setq n (pplus (ptimes (car x)(caddr q))
			  (ptimes (car y)(cadr q))))
	   (if (cadddr q)		; denom factor from algebraic gcd
	       (setq n (ptimes n (cadddr q))))
	   (ratreduce n 
		      (ptimes (car q)
			      (ptimes (cadr q) (caddr q)))))))

(defmfun ratquotient (x y)
  (rattimes x (ratinvert y) t)) 

;;	THIS IS THE END OF THE NEW RATIONAL FUNCTION PACKAGE PART 2.
;;	IT INCLUDES RATIONAL FUNCTIONS ONLY.
