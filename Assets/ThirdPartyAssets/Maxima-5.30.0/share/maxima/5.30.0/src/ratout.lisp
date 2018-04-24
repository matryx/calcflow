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

(macsyma-module ratout)

;; THIS IS THE OUT-OF-CORE SEGMENT OF THE RATIONAL FUNCTION PACKAGE.

(declare-top (special $algebraic errrjfflag varlist ss *y* f $factorflag modulus hmodulus
		      genvar *a* *alpha *var* *x* *p *max *var *res *chk *l $intfaclim
		      $ratfac u* $ratwtlvl *ratweights $ratweights $keepfloat))

(load-macsyma-macros ratmac)

(declare-top (special $gcd xv bigf1 bigf2 nonlindeg $linhack
		      $intfaclim bigf1tilde bigf2tilde
		      gcd $factorflag *gcdl* last-good-prime))

;;	NEWGCD (X,Y) RETURNS A LIST OF THREE ITEMS,
;;	(GCD, X/GCD, Y/GCD)

(defun newgcd (x y modulus &aux hmodulus)
  (setqmodulus modulus)
  (let ((a (cond ((pcoefp x)
		  (cond ((zerop x) y)
			((pcoefp y) (cgcd x y))
			(t (pcontent1 (cdr y) x))))
		 ((pcoefp y) (cond ((zerop y) x) (t (pcontent1 (cdr x) y))))
		 ((pointergp (p-var x) (p-var y)) (oldcontent1 (cdr x) y))
		 ((pointergp (p-var y) (p-var x)) (oldcontent1 (cdr y) x))
		 (t nil))))
    (cond (a (list a (pquotient x a) (pquotient y a)))
	  (modulus (pgcdp x y modulus))
	  (t (pgcdm x y)))))

;;;***	PMODCONTENT COMPUTES CONTENT OF
;;;	P IN
;;	Z [X ] [X , X , ..., X   ]
;;        P  V    1   2        V-1

;;	PMODCONTENT OF 3*A*X IS A, IF MAINVAR IS X (=X )
;;						      V

(defun pmodcontent (p)
  (prog (*var *chk *res *max gcd)
     (setq *chk (car p))
     (setq *max 0)
     (setq *var (pnext (cdr p) nil))
     (cond ((pointergp xv *chk) (go ret1))
	   ((null *var) (return (list p 1))))
     (pgath1 (cdr p))
     a    (setq *res 0)
     (pgath3 (cdr p))
     a2   (cond ((pcoefp *res) (cond ((pzerop *res) nil)(t(go ret1))))
		((not (eq (car *res) *chk)) (go ret1))
		((not (univar (cdr *res)))
		 (setq *res (car (pmodcontent *res)))
		 (go a2))
		(gcd (setq gcd (pgcdu gcd *res)))
		(t (setq gcd *res)))
     (cond ((pcoefp gcd) (go ret1))
	   ((minusp (setq *max (1- *max)))
	    (return (list gcd (pquotient p gcd)))))
     (go a)
     ret1 (return (list 1 p))))

(defun pgathercoef (p *chk *res)
  (if (not (eq (car p) *chk)) 1 (pgath2 (cdr p) nil)))

(defun pgath1 (p)
  (prog nil
     (cond ((null p) (return *max))
	   ((pcoefp (cadr p)) nil)
	   ((eq (caadr p) *var) (setq *max (max *max (cadadr p)))))
     (return (pgath1 (cddr p)))))

(defun pgath2 (p vmax)
  (prog (v2)
     (cond ((null p) (return *res))
	   ((pcoefp (cadr p)) nil)
	   ((vgreat (setq v2 (pdegreer (cadr p))) vmax)
	    (setq *res (psimp *chk
			      (list (car p) (leadcoefficient (cadr p)))))
	    (setq vmax v2))
	   ((equal vmax v2)
	    (setq *res
		  (pplus *res
			 (psimp *chk
				(list (car p) (leadcoefficient (cadr p))))))))
     (return (pgath2 (cddr p) vmax))))

(defun pgath3 (p)
  (prog (zz)
     (cond ((null p) (return *res))
	   ((pcoefp (cadr p))
	    (cond ((eqn *max 0) (setq zz (cadr p)) (go add)) (t (go ret))))
	   ((eq (caadr p) *var) (setq zz (pterm (cdadr p) *max)) (go add)))
     (cond ((eqn *max 0) (setq zz (cadr p))) (t (go ret)))
     add  (cond ((eqn zz 0) (go ret)))
     (setq *res (pplus *res (psimp *chk (list (car p) zz))))
     ret  (return (pgath3 (cddr p)))))

(defun pnext (x *l)
  (pnext1 x)
  (cond ((null *l) nil)
	(t (car (sort *l #'pointergp)))))

(defun pnext1 (x)
  (prog nil
     (cond ((null x) (return *l))
	   ((or (pcoefp (cadr x)) (member (caadr x) *l :test #'eq)) nil)
	   (t (setq *l (cons (caadr x) *l))))
     (return (pnext1 (cddr x)))))

(defun vgreat (x y)
  (cond ((null x) nil)
	((null y) t)
	((pointergp (car x)(car y))t)
	((not (eq (car x)(car y)))nil)
	((> (cadr x)(cadr y)) t)
	((eqn (cadr x)(cadr y))(vgreat (cddr x)(cddr y)))
	(t nil)))

(defun pdegreer (x)
  (if (pcoefp x) () (cons (car x) (cons (cadr x) (pdegreer (caddr x))))))

;;***	PGCDP CORRESPONDS TO BROWN'S ALGORITHM P

(defun pgcdp (bigf1 bigf2 modulus)
  (prog (c c1		c2		n		q
	 h1tilde	h2tilde		gstar		h1star
	 h2star		xv		e		b
	 gbar		nubar		nu1bar		nu2bar
	 gtilde		f1tilde		f2tilde		biggtilde
	 degree		f1		f1f2		hmodulus)
     (setqmodulus modulus)
     (cond ((and (univar (cdr bigf1)) (univar (cdr bigf2)))
	    (setq q (pgcdu bigf1 bigf2))
	    (return (list q (pquotient bigf1 q) (pquotient bigf2 q)))))
     (setq xv (car bigf1))
     (setq bigf1 (pmodcontent bigf1))
     (setq bigf2 (pmodcontent bigf2))
     (setq c (pgcdu (setq c1 (car bigf1)) (setq c2 (car bigf2))))
     (setq bigf1 (cadr bigf1))
     (setq bigf2 (cadr bigf2))
     (setq n 0)
     (setq e (pdegreer bigf2))
     (setq degree (pdegreer bigf1))
     (cond ((vgreat e degree) (setq e degree)))
     (setq b (ash modulus -1))
     (setq gbar
	   (pgcdu (setq f1 (pgathercoef bigf1 xv 0))
		  (setq f1f2
			(pgathercoef bigf2 xv 0))))
     (cond ((equal 0 f1f2) (go step15a)))
     (setq nubar (pdegree gbar xv))
     (setq nu1bar (+ nubar (pdegree bigf1 xv)))
     (setq nu2bar (+ nubar (pdegree bigf2 xv)))
     (setq f1f2 (ptimes f1 f1f2))
     (setq nubar (max nu1bar nu2bar))
     step6(setq b (cplus b 1))
     (cond ((equal (pcsubst f1f2 b xv) 0) (go step6)))
     ;; Step 7
     (setq gtilde (pcsubst gbar b xv))
     (setq f1tilde (pcsubst bigf1 b xv))
     (setq f2tilde (pcsubst bigf2 b xv))
     (setq biggtilde
	   (ptimeschk gtilde
		      (car (setq h2tilde (newgcd f1tilde f2tilde modulus)))))
     (cond ((pcoefp biggtilde) (go step15a)))
     (setq h1tilde (cadr h2tilde))
     (setq h2tilde (caddr h2tilde))
     (setq degree (pdegreer biggtilde))
     (cond ((vgreat degree e) (go step6))
	   ((vgreat e degree) (setq n 0) (setq e degree)))
     (setq n (1+ n))
     (cond ((equal n 1) (setq q (list xv 1 1 0 (cminus b)))
	    (setq gstar biggtilde)
	    (setq h1star h1tilde)
	    (setq h2star h2tilde))
	   (t (setq gstar (lagrange33 gstar biggtilde q b))
	      (setq h1star (lagrange33 h1star h1tilde q b))
	      (setq h2star (lagrange33 h2star h2tilde q b))
	      (setq q (ptimes q (list xv 1 1 0 (cminus b))))))
     ;; Step 12
     (cond ((not (> n nubar)) (go step6)))
     ;; Step 13
     (cond ((or (not (= nu1bar (+ (setq degree (pdegree gstar xv))
				   (pdegree h1star xv))))
		(not (= nu2bar (+ degree (pdegree h2star xv)))))
	    (setq n 0)
	    (go step6)))
     (setq gstar (cadr (pmodcontent gstar)))
     ;; Step 15
     (setq q (pgathercoef gstar xv 0))
     (return (monicgcd  (ptimeschk c gstar)
			(ptimeschk (pquotient c1 c) (pquotientchk h1star q))
			(ptimeschk (pquotient c2 c) (pquotientchk h2star q))
			(leadcoefficient gstar)))
     step15a
     (return (list c
		   (ptimeschk (pquotient c1 c) bigf1)
		   (ptimeschk (pquotient c2 c) bigf2))) ))


(defun monicgcd (gcd x y lcf)
  (cond ((eqn lcf 1) (list gcd x y))
	(t (list	(ptimes (crecip lcf) gcd)
			(ptimes lcf x)
			(ptimes lcf y) )) ))

;;***	PGCDM CORRESPONDS TO BROWN'S ALGORITHM M


(defun pgcdm
    (bigf1 bigf2)
  (prog (c c1		c2		f1		f2	n
	 e		degree		mubar		p
	 nonlindeg	gtilde		h1tilde		h2tilde
	 modulus	hmodulus	bigf1tilde	bigf2tilde
	 biggtilde	q		h1star		h2star
	 gstar		xv              gbar)
     (setq p *alpha)
     (setq xv (car bigf1))
     ;; Step 1
     (setq f1 (pcontent bigf1))
     (setq f2 (pcontent bigf2))
     (setq c (cgcd (setq c1 (car f1)) (setq c2 (car f2))))
     (setq bigf1 (cadr f1))
     (setq bigf2 (cadr f2))
     ;; Step 3
     (setq f1 (leadcoefficient bigf1))
     (setq f2 (leadcoefficient bigf2))
     (setq gbar (cgcd f1 f2))
     ;; Step 4
     (setq n 0)
     (setq degree (pdegreer bigf1))
     (setq e (pdegreer bigf2))
     (cond ((vgreat e degree) (setq e degree)))
     ;; Step 5
     (setq mubar
	   (* 2 gbar (max (maxcoefficient bigf1)
			      (maxcoefficient bigf2))))
     (go step6a)
     step6(setq p (newprime p))
     step6a
     (cond ((or (zerop (rem f1 p)) (zerop (rem f2 p)))
	    (go step6)))
     (setqmodulus p)
     ;; Step 7
     (setq gtilde (pmod gbar))
     ;; Step 8
     (setq biggtilde
	   (ptimeschk gtilde
		      (car (setq h2tilde
				 (newgcd (pmod bigf1) (pmod bigf2)
					 modulus)))))
     (cond ((pcoefp biggtilde) (setq modulus nil)
	    (setq gstar 1)
	    (setq h1star bigf1)
	    (setq h2star bigf2)
	    (go step15)))
     (cond ((null (cdr h2tilde))
	    (setq h1tilde (pquotient (pmod bigf1) (car h2tilde)))
	    (setq h2tilde (pquotient (pmod bigf2) (car h2tilde))))
	   (t (setq h1tilde (cadr h2tilde))
	      (setq h2tilde (caddr h2tilde))))
     (setq degree (pdegreer biggtilde))
     (cond ((vgreat degree e) (go step6))
	   ((vgreat e degree) (setq n 0) (setq e degree)))
     (setq n (1+ n))
     ;; Step 11
     (setqmodulus nil)
     (cond ((equal n 1) (setq q p)
	    (setq gstar biggtilde)
	    (setq h1star h1tilde)
	    (setq h2star h2tilde))
	   (t (setq gstar (lagrange3 gstar biggtilde p q))
	      (setq h1star (lagrange3 h1star h1tilde p q))
	      (setq h2star (lagrange3 h2star h2tilde p q))
	      (setq q (* p q))))
     ;; Step 12
     (cond ((> mubar q) (go step6)))
     (cond ((> (* 2 (max (* (setq gtilde (norm gstar)) (maxcoefficient h1star))
			 (* gtilde (maxcoefficient h2star))))
	       q)
	    (go step6)))
     (setqmodulus nil)
     (setq gstar (cadr (pcontent gstar)))
     step15
     (setq last-good-prime p)
     (setq q (leadcoefficient gstar))
     (return (list (ptimeschk c gstar)
		   (ptimeschk (cquotient c1 c) (pquotientchk h1star q))
		   (ptimeschk (cquotient c2 c) (pquotientchk h2star q))))))

;;	THE FUNCTIONS ON THIS PAGE ARE USED BY KRONECKER FACTORING

(defun pkroneck (p)
  (prog (maxexp i l *p factors factor errrjfflag)
     (setq maxexp (quotient (cadr p) 2))
     (setq i 1)
     a    (when (> i maxexp) (return (cons p factors)))
     (setq l (p1 (reverse (let ((p p) (i i) ($factorflag t))
			    (pfactor2 p i)))))
     b    (when (null l) (go d))
     (setq *l (car l))
     (setq *p (car p))
     (setq errrjfflag t)
     (setq factor (errset (pinterpolate *l *p) nil))
     (setq errrjfflag nil)
     (setq l (cdr l))
     (if (atom factor)
	 (go b)
	 (setq factor (car factor)))
     (when (or (pcoefp factor)
	       (not (eqn (car p) (car factor)))
	       (not (pzerop (prem p factor))))
       (go b))
     (cond (modulus (pmonicize (cdr factor)))
	   ((pminusp factor) (setq factor (pminus factor))))
     (setq p (pquotient p factor))
     (setq maxexp (quotient (cadr p) 2))
     (setq factors (cons factor factors))
     (when (or (eqn p 1) (eqn p -1)) (return factors))
     (go a)
     d    (incf i)
     (go a)))

(defun pfactor2 (p i)
  (cond ((< i 0) nil)
	(t (cons (pfactor (pcsubst p i (car p)))
		 (pfactor2 p (1- i))))))

(defun rpowerset (x n)
  (cond ((null x) (quote (1 nil)))
	((equal x 1) (quote (1)))
	(t (cons 1 (ptts1 x n x)))))


(defun allprods (x y)
  (cond ((null x) nil)
	((null y) nil)
	(t (nconc (ap1 (car x) y) (allprods (cdr x) y)))))

(defun al1 (f r len)
  (prog (ss)
     (cond
       ((equal len 1)
	(return (mapcar #'(lambda (*y*) (cons *y* nil)) f)))
       ((null r) (return nil))
       (t
	(mapc #'(lambda (*y*)
		  (setq ss
			(nconc ss
			       (mapcar #'(lambda (z) (cons z *y*))
				       f))))
	      (al1 (car r) (cdr r) (1- len)))
	(return ss)))))


(defun ap1 (x l)
  (cond ((null l) nil)
	(t (cons (ptimes x (car l)) (ap1 x (cdr l))))))

(defun ptts1 (x n y)
  (cond ((eqn n 1) (list y))
	(t (cons y (ptts1 x (1- n) (ptimes x y))))))

(defun p1 (l)
  (prog (a)
     (setq a (mapcar #'p11 l))
     (return (cond ((null l) nil)
		   (t (cdr (al1 (car a)
				(cdr a)
				(length a))))))))

(defun p11 (ele)
  (cond ((null (cddr ele)) (rpowerset (car ele) (cadr ele)))
	(t (allprods (rpowerset (car ele) (cadr ele))
		     (p11 (cddr ele))))))

(defun pinterpolate (l var)
  (psimp var (pinterpolate1 (pinterpolate2 l 1)
			    (- (length l) 2))))

(defun pinterpolate1 (x n)
  (pinterpolate4 (pinterpolate5 (reverse x) 1 n n) (1+ n)))

(defun pinterpolate2 (x n)
  (cond ((null (cdr x)) x)
	(t (cons (car x)
		 (pinterpolate2 (pinterpolate3 x n) (1+ n))))))

(defun pinterpolate3 (x n)
  (cond ((null (cdr x)) nil)
	(t (cons (pquotient (pdifference (cadr x) (car x)) n)
		 (pinterpolate3 (cdr x) n)))))

(defun pinterpolate4 (x n)
  (cond ((null x) nil)
	((pzerop (car x)) (pinterpolate4 (cdr x) (1- n)))
	(t (cons n (cons (car x)
			 (pinterpolate4 (cdr x) (1- n)))))))

(defun pinterpolate5 (x i j n)
  (cond ((> i n) x)
	(t (pinterpolate5 (cons (car x) (pinterpolate6 x i j))
			  (1+ i)
			  (1- j)
			  n))))

(defun pinterpolate6 (x i j)
  (cond ((zerop i) (cdr x))
	(t (cons (pdifference (cadr x) (pctimes j (car x)))
		 (pinterpolate6 (cdr x) (1- i) j)))))

;; THE N**(1.585) MULTIPLICATION SCHEME
;;FOLLOWS.  IT SHOULD BE USED ONLY WHEN BOTH INPUTS ARE MULTIVARIATE,
;;DENSE, AND OF NEARLY THE SAME SIZE.  OR ABSOLUTELY TREMENDOUS.
;;(THE CLASSICAL MULTIPLICATION SCHEME IS N**2 WHERE N IS SIZE OF
;;POLYNOMIAL   (OR N*M FOR DIFFERENT SIZES).  FOR THIS
;;CASE, N IS APPX. THE SIZE OF LARGER.

(defmfun $fasttimes (x y)
  (cond ((and (not (atom x)) (not (atom y))
	      (equal (car x) (car y)) (equal (caar x) 'mrat)
	      (equal (cddr x) 1) (equal (cddr y) 1))
	 (cons (car x)(cons (fptimes (cadr x)(cadr y))1)))
	(t (merror (intl:gettext "fasttimes: arguments must be CRE polynomials with same variables.")))))

(defun fptimes (x y)
  (cond ((or (pzerop x) (pzerop y)) (pzero))
	((pcoefp x) (pctimes x y))
	((pcoefp y) (pctimes y x))
	((eq (car x) (car y))
	 (cond((or(univar(cdr x))(univar(cdr y)))
	       (cons (car x) (ptimes1 (cdr x) (cdr y))))
	      (t(cons (car x) (fptimes1 (cdr x)(cdr y))))))
	((pointergp (car x) (car y))
	 (cons (car x) (pctimes1 y (cdr x))))
	(t (cons (car y) (pctimes1 x (cdr y))))))

(defun fptimes1 (f g)
  (prog (a b c d)
     (cond ((or (null f) (null g)) (return nil))
	   ((null (cddr f))
	    (return (lsft (pctimes1 (cadr f) g) (car f))))
	   ((null (cddr g))
	    (return (lsft (pctimes1 (cadr g) f) (car g)))))
     (setq d (ash (1+ (max (car f) (car g))) -1))
     (setq f (halfsplit f d) g (halfsplit g d))
     (setq a (fptimes1 (car f) (car g)))
     (setq b
	   (fptimes1 (pplus1 (car f) (cdr f)) (pplus1 (car g) (cdr g))))
     (setq c (fptimes1 (cdr f) (cdr g)))
     (setq b (pdiffer1 (pdiffer1 b a) c))
     (return (pplus1 (lsft a (ash d 1)) (pplus1 (lsft b d) c)))))

(defun halfsplit (p d)
  (do ((a) (p p (cddr p)))
      ((or (null p) (< (car p) d)) (cons (nreverse a) p))
    (setq a (cons (cadr p) (cons (- (car p) d) a)))))

(defun lsft (p n)
  (do ((q p (cddr (rplaca q (+ (car q) n)))))
      ((null q)))
  p)

(declare-top (special wtsofar xweight $ratwtlvl v *x* *i*))

;;; TO TRUNCATE ON E, DO RATWEIGHT(E,1);
;;;THEN DO RATWTLVL:N.  ALL POWERS >N GO TO 0.

(defmfun $ratweight (&rest args)
  (when (oddp (length args))
    (merror (intl:gettext "ratweight: number of arguments must be a multiple of 2.")))
  (do ((l args (cddr l)))
      ((null l))
    (rplacd (or (assoc (first l) *ratweights :test #'equal)
		(car (push (list (first l)) *ratweights)))
	    (second l)))
  (setq $ratweights (cons '(mlist simp) (dot2l *ratweights)))
  (if (null args)
      $ratweights
      (cons '(mlist) args)))

(defun pweight (x)
  (or (get x '$ratweight) 0))

(defun wtptimes (x y wtsofar)
  (cond ((or (pzerop x) (pzerop y) (> wtsofar $ratwtlvl))
	 (pzero))
	((pcoefp x) (wtpctimes x y))
	((pcoefp y) (wtpctimes y x))
	((eq (car x) (car y))
	 (palgsimp (car x)
		   (wtptimes1 (cdr x)
			      (cdr y)
			      (pweight (car x)))
		   (alg x)))
	((pointergp (car x) (car y))
	 (psimp (car x)
		(wtpctimes1 y (cdr x) (pweight (car x)))))
	(t (psimp (car y)
		  (wtpctimes1 x (cdr y) (pweight (car y)))))))

(defun wtptimes1 (*x* y xweight)
  (prog (u* v)
     (declare (special v))
     (setq v (setq u* (wtptimes2 y)))
     a    (setq *x* (cddr *x*))
     (cond ((null *x*) (return u*)))
     (wtptimes3 y)
     (go a)))


(defun wtptimes2 (y)
  (if (null y)
      nil
      (let ((ii (+ (* xweight (+ (car *x*) (car y))) wtsofar)))
	(if (> ii $ratwtlvl)
	    (wtptimes2 (cddr y))
	    (pcoefadd (+ (car *x*) (car y))
		      (wtptimes (cadr *x*) (cadr y) ii)
		      (wtptimes2 (cddr y)))))))

(defun wtptimes3 (y)
  (prog ((e 0) u c)
     (declare (special v))
     a1   (cond ((null y) (return nil)))
     (setq e (+ (car *x*) (car y)))
     (setq c (wtptimes (cadr y) (cadr *x*) (+ wtsofar (* xweight e))))
     (cond ((pzerop c) (setq y (cddr y)) (go a1))
	   ((or (null v) (> e (car v))) (setq u* (setq v (pplus1 u* (list e c)))) (setq y (cddr y)) (go a1))
	   ((eqn e (car v))
	    (setq c (pplus c (cadr v)))
	    (cond ((pzerop c) (setq u* (setq v (pdiffer1 u* (list (car v) (cadr v)))))) (t (rplaca (cdr v) c)))
	    (setq y (cddr y))
	    (go a1)))
     a    (cond ((and (cddr v) (> (caddr v) e)) (setq v (cddr v)) (go a)))
     (setq u (cdr v))
     b    (cond ((or (null (cdr u)) (< (cadr u) e)) (rplacd u (cons e (cons c (cdr u)))) (go e)))
     (cond ((pzerop (setq c (pplus (caddr u) c))) (rplacd u (cdddr u)) (go d)) (t (rplaca (cddr u) c)))
     e    (setq u (cddr u))
     d    (setq y (cddr y))
     (cond ((null y) (return nil))
	   ((pzerop
	     (setq c (wtptimes (cadr *x*) (cadr y)
			       (+ wtsofar (* xweight
					       (setq e (+ (car *x*) (car y))))))))
	    (go d)))
     c    (cond ((and (cdr u) (> (cadr u) e)) (setq u (cddr u)) (go c)))
     (go b)))


(defun wtpctimes (c p)
  (cond ((pcoefp p) (ctimes c p))
	(t (psimp (car p) (wtpctimes1 c (cdr p) (pweight (car p)))))))

(defun wtpctimes1 (c x xwt)
  (prog (cc)
     (return
       (cond ((null x) nil)
	     (t (setq cc (wtptimes c
				   (cadr x)
				   (+ wtsofar (* xwt (car x)))))
		(cond ((pzerop cc) (wtpctimes1 c (cddr x) xwt))
		      (t (cons (car x)
			       (cons cc
				     (wtpctimes1 c
						 (cddr x)
						 xwt))))))))))

(defun wtpexpt (x n)
  (cond ((= n 0) 1)
	((= n 1) x)
	((evenp n)
	 (let ((xn2 (wtpexpt x (/ n 2))))
	   (wtptimes xn2 xn2 0)))
	(t (wtptimes x (wtpexpt x (1- n)) 0))))

(defmfun $horner (e &rest l)
  (let (($ratfac nil)
	(varlist (cdr $ratvars))
	genvar
	(x nil)
	(arg1 (taychk2rat e)))
    (cond ((mbagp arg1)
	   (cons (car arg1)
		 (mapcar #'(lambda (u) (apply '$horner (cons u l))) (cdr arg1))))
	  (t
	   (setq x (apply #'$rat (cons arg1 l)))
	   (mapc #'(lambda (y z) (putprop y z 'disrep)) (cadddr (car x)) (caddar x))
	   (div* (hornrep (cadr x)) (hornrep (cddr x)))))))

(defun hornrep (p)
  (if (pcoefp p)
      p
      (horn+ (cdr p) (get (car p) 'disrep))))

(defun horn+ (l var)
  (prog (ans last)
     (setq ans (hornrep (cadr l)))
     a (setq last (car l) l (cddr l))
     (cond ((null l)
	    (return (cond ((equal last 0) ans)
			  (t (list '(mtimes)
				   (list '(mexpt) var last) ans)))))
	   (t (setq ans (list '(mplus)
			      (hornrep (cadr l))
			      (list '(mtimes)
				    (list '(mexpt) var (- last (car l)))
				    ans)))))
     (go a)))

(declare-top (special y genvar $savefactors checkfactors w
		      exp var x $factorflag $ratfac
		      $keepfloat ratform rootfactor
		      wholepart parnumer varlist n))

(defmfun $partfrac (exp var)
  (cond ((and (not (atom exp)) (member (caar exp) '(mequal mlist $matrix) :test #'eq))
	 (cons (car exp) (mapcar #'(lambda (u) ($partfrac u var)) (cdr exp))))
	((and (atom var) (not (among var exp))) exp)
	(t (let (($savefactors t) (checkfactors ()) (varlist (list var))
		 $ratfac $algebraic ratform genvar)
	     (desetq (ratform . exp) (taychk2rat exp))
	     (setq var (caadr (ratf var)))
	     (setq exp (partfrac exp var))
	     (setq exp (cons (car exp)	;FULL DECOMP?
			     (mapcan #'partfraca (cdr exp))))
	     (add2* (disrep (car exp))
		    (cons '(mplus)
			  (mapcar #'(lambda (l)
				      (destructuring-let (((coef poly exp) l))
							 (list '(mtimes)
							       (disrep  coef)
							       (list '(mexpt)
								     (disrep poly)
								     (- exp)))))
				  (cdr exp))))))))

(defun partfraca (llist)
  (destructuring-let (((coef poly exp) llist))
    (do ((nc (ratdivide coef poly) (ratdivide (car nc) poly))
	 (n exp (1- n))
	 (ans))
	((rzerop (car nc)) (cons (list (cdr nc) poly n) ans))
      (push (list (cdr nc) poly n) ans))))

(defun partfrac (rat var &optional facdenom)
  (destructuring-let* (((wholepart frpart) (pdivide (car rat) (cdr rat)))
		       ((num . denom) (ratqu frpart (cdr rat))))
    (cond ((pzerop num) (cons wholepart nil))
	  ((or (pcoefp denom) (pointergp var (car denom))) (cons rat nil))
	  (t (destructuring-let (((content bpart) (oldcontent denom)))
	       (do ((factor (or facdenom (pfactor bpart)) (cddr factor))
		    (apart) (y) (parnumer))
		   ((null factor) (cons wholepart parnumer))
		 (cond
		   ((zerop (pdegree (car factor) var)))
		   (t (setq apart (pexpt (car factor) (cadr factor))
			    bpart (pquotient bpart apart)
			    y (bprog apart bpart)
			    frpart (cdr (ratdivide (ratti num (cdr y) t)
						   apart)))
		      (push (list (ratqu frpart content)
				  (car factor)
				  (cadr factor))
			    parnumer)
		      (desetq (num . content)
			      (cdr (ratdivide (ratqu (ratti num (car y) t)
						     content)
					      bpart)))))))))))

(declare-top (unspecial exp f n ss v var w xv y *a* *chk *l *max *p
			*res u* *var* *x* *y*))

;; $RATDIFF TAKES DERIVATIVES FAST.  IT ASSUMES THAT THE
;; ONLY ENTITY WHICH DEPENDS ON X IS X ITSELF.
;; THAT IS, DEPENDENCIES DECLARED EXPLICITLY OR IMPLICITLY ARE
;; TOTALLY IGNORED.  RATDIFF(F(X),X) IS 0.  RATDIFF(Y,X) IS 0.
;; ANY OTHER USAGE MUST GO THROUGH $DIFF.
;; FURTHERMORE, X IS ASSUMED TO BE AN ATOM OR A SINGLE ITEM ON
;; VARLIST.  E.G. X MIGHT BE SIN(U), BUT NOT 2*SIN(U).

(declare-top (special varlist genvar x))

(defmfun $ratdiff (p x)
  (if ($ratp p)
      (setq p (minimize-varlist
	       (if (member 'trunc (cdar p) :test #'eq) ($taytorat p) p))))
  (let ((formflag ($ratp p)) (varlist) (genvar))
    (newvar x) (newvar p)
    (or (every #'(lambda (exp)
		     (or (alike1 x exp) (free exp x)))
		 varlist)
	(merror (intl:gettext "ratdiff: first argument must be a polynomial in ~M; found: ~M") x p))
    (setq p (ratf p))
    (setq x (caadr (ratf x)))
    (setq p (cons (car p) (ratderivative (cdr p) x)))
    (if formflag p ($ratdisrep p))))

(declare-top (unspecial x))

(declare-top (special $pfeformat varlist $factorflag m v dosimp))

(defmfun $pfet (m)
  (prog (listov $pfeformat varlist $factorflag)
     (setq $pfeformat t)
     (newvar m)
     (setq listov varlist)
     (mapc #'(lambda (r) (setq m (pfet1 m r)))
	   listov)
     (setq m (simplify m))
     (setq m (cond ((atom m) m)
		   ((eq (caar m) 'mplus)
		    (cons '(mplus)
			  (mapcar #'$ratexpand (cdr m))))
		   (t ($ratexpand m))))
     (return (cond ((atom m) m)
		   ((eq (caar m) 'mplus)
		    (cons '(mplus)
			  (mapcar #'sssqfr (cdr m))))
		   (t (sssqfr m))))))

(defun sssqfr (x)
  (let ((dosimp t)) (simplify ($sqfr x))))

(defun pfet1 (m v)
  (cond ((atom m) m)
	((eq (caar m) 'mplus)
	 (cons '(mplus)
	       (mapcar #'(lambda (s) ($partfrac s v))
		       (cdr m))))
	(t ($partfrac m v))))

(declare-top (unspecial m v))
