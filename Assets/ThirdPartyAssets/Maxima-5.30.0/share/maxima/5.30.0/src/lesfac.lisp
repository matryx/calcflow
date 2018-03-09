;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module lesfac)

(load-macsyma-macros rzmac ratmac)

(defun newsym2 (p e &aux (g (gensym)))
  (putprop g e 'disrep)
  (valput g (1- (valget (car genvar))))
  (setq genvar (cons g genvar))
  (setq varlist (cons e varlist))
  (putprop g p 'unhacked)
  g)

(defun getunhack (gen) (or (get gen 'unhacked) (pget gen)))

(defmacro getdis (x) `(get ,x 'disrep))

(defmacro cons1 (x) `(cons ,x 1))

(defun frpoly? (r) (equal 1 (cdr r)))

(defmacro setcall (&rest l)
  (setq l (cons 'setcall l))
  (sublis (list (cons 'fncall (cdr l))
		(cons 'a (caddr l))
		(cons 'b (cadddr l)))
	  '(prog1 (car (setq a fncall)) (setq b (caddr a) a (cadr a)))))

(defun pquocof (p q)
  (let ((qq (testdivide p q)))
    (cond (qq (list q qq 1))
	  ((list 1 p q)))))

(defun polyst (a)
  (cond ((pcoefp a) (list a))
	(t (cons (cons (car a) (cadr a)) (polyst (caddr a))))))

(defun cdinf (a b both)
  (cond ((or (pcoefp a) (pcoefp b)) (list 1 a b))
	(t (setq a (ncons (copy-tree a))
		 b (ncons (cond (both (copy-tree b))(t b))))
	   (list (cd1 a b both) (car a) (car b)))))

(defun cd1 (a b both)
  (cond ((or (pcoefp (car a)) (pcoefp (car b))) 1)
	((eq (caar a) (caar b))
	 (ptimes (pexpt (pget (caar a))	;CHECK FOR ALG. TRUNC.
			(prog1 (cond (both (+ (cadar a) (cadar b))) (t (cadar a)))
			  (rplaca a (caddar a))
			  (cond (both (rplaca b (caddar b)))
				(t (setq b (cddar b))))))
		 (cd1 a b both)))
	((pointergp (caar a) (caar b)) (cd1 (cddar a) b both))
	(t (cd1 a (cddar b) both))))

(defun lmake (p l)
  (cond ((pcoefp p) (cons p l))
	((get (car p) 'unhacked)
	 (lmake (caddr p) (cons (cons (car p) (cadr p)) l)))
	(t (setq l (lmake (caddr p) l))
	   (rplaca l (list (car p) (cadr p) (car l))))))

(defun lmake2 (p l)
  (setq l (lmake p l))
  (mapc #'(lambda (x) (rplaca x (getunhack (car x))))
	(cdr l))
  (cond ((equal (car l) 1) (cdr l))
	(t (rplaca l (cons (car l) 1)))))


(defun pmake (l)
  (cond ((null l) 1)
	((= 0 (cdar l)) (pmake (cdr l)))
	((numberp (caar l))	     ;CLAUSE SHOULD BE ELIMINATED ASAP
	 (ptimes (cexpt (caar l) (cdar l)) (pmake (cdr l))))
	(t (ptimes (list (caar l) (cdar l) 1) (pmake (cdr l))))))

(defun facmgcd (pl)            ;GCD OF POLY LIST FOR EZGCD WITH RATFAC
  (do ((l (cdr pl) (cdr l))
       (ans nil (cons (caddr gcd) ans))
       (gcd (car pl) (car gcd)))
      ((null l) (cons gcd (nreverse ans)))
    (setq gcd (fpgcdco gcd (car l)))
    (cond ((equal (car gcd) 1) (return (cons 1 pl)))
	  ((null ans) (setq ans (list (cadr gcd))))
	  ((not (equal (cadr gcd) 1))
	   (do ((l2 ans (cdr l2))) ((null l2))
	     (rplaca l2 (ptimes (cadr gcd) (car l2))))))))


(defun fpgcdco (p q)
  (let ($ratfac gcdl)			;FACTORED PGCDCOFACTS
    (cond ((or (pcoefp p) (pcoefp q)) (pgcdcofacts p q))
	  (t (list (ptimeschk
		    (setcall pgcdcofacts p q)
		    (car (setq p (lmake p nil)
			       q (lmake q nil)
			       gcdl (mapcar 'pmake (lgcd1 (cdr p) (cdr q)) ))))
		   (ptimeschk (car p) (cadr gcdl))
		   (ptimeschk (car q) (caddr gcdl)))))))

;;	NOTE: ITEMS ON VARLIST ARE POS. NORMAL
;;	INTEGER COEF GCD=1 AND LEADCOEF. IS POS.

(defun lgcd1 (a b)
  (prog (ptlist g bj c t1 d1 d2)
     (setq ptlist (mapcar #'(lambda (ig) (declare (ignore ig)) b) a))
     (do ((a a (cdr a))
	  (ptlist ptlist (cdr ptlist)))
	 ((null a))
       (do ((ai (getunhack (caar a)))
	    (b (car ptlist) (cdr b)))
	   ((null b))
	 (and (zerop (cdar b)) (go nextb))
	 (setq d1 1 d2 1)
	 (setq bj (getunhack (caar b)))
	 (setq c (cond ((pirredp (caar a))
			(cond ((pirredp (caar b)) 1)
			      (t (setcall pquocof bj ai))))
		       ((pirredp (caar b)) (setcall pquocof ai bj))
		       (t (setcall pgcdcofacts ai bj))))
	 (cond ((equal c 1) (go nextb))
	       ((equal ai 1) (go bloop)))
	aloop
	 (cond ((setq t1 (testdivide ai c))
		(setq ai t1 d1 (1+ d1))
		(go aloop)))
	bloop
	 (and (= d1 1)
	      (not (equal bj 1))
	      (do ((t1
		    (testdivide bj c)
		    (testdivide bj c)))
		  ((null t1))
		(setq bj t1 d2 (1+ d2))))
	 (setq g (cons (cons (makprodg c t)
			     (min (setq d1 (* d1 (cdar a)))
				  (setq d2 (* d2 (cdar b)))))
		       g))
	 (cond ((> d1 (cdar g))
		(rplacd (last a)
			(ncons (cons (caar g) (- d1 (cdar g)))))
		(rplacd (last ptlist) (ncons (cdr b)))))
	 (cond ((> d2 (cdar g))
		(rplacd (last b)
			(ncons (cons (caar g) (- d2 (cdar g)))))))
	 (rplaca (car a) (makprodg ai t))
	 (rplaca (car b) (makprodg bj t))
	 (and (equal bj 1) (rplacd (car b) 0))
	 (and (equal ai 1) (rplacd (car a) 0) (return nil))
	nextb))
     (return (list g a b))))

(defun makprodg (p sw)
  (cond ((pcoefp p) p)
	(t (car (makprod p sw)))))

(defun dopgcdcofacts (x y)
  (let (($gcd
	 $gcd)( $ratfac nil)) (or (member $gcd *gcdl* :test #'eq) (setq $gcd '$ez))
	 (pgcdcofacts x y)))

(defun facrplus (x y)
  (let ((a (car x))
	(b (cdr x))
	(c (car y))
	(d (cdr y)))
    (setq x (setcall dopgcdcofacts a c)
	  y (setcall fpgcdco b d))
    (setq a (makprod
	     (pplus (pflatten (ptimeschk a d))
		    (pflatten (ptimeschk b c))) nil))
    (setq b (ptimeschk b d))
    (cond ($algebraic (setq y (ptimeschk y b))
		      (setcall fpgcdco y a) ;for unexpected gcd
		      (cons (ptimes x a) y))
	  (t (setq c (setcall cdinf y b nil))
	     (setcall fpgcdco y a)
	     (cons (ptimes x a) (ptimeschk y (ptimeschk c b)))))))

(defun mfacpplus (l)
  (let (($gcd (or $gcd '$ez))
	($ratfac nil)
	(g nil))
    (setq g (oldcontent2 (sort (copy-list l) 'contodr) 0))
    (cond ((pzerop g) g)
	  ((do ((a (pflatten (pquotient (car l) g))
		   (pplus a (pflatten (pquotient (car ll) g))))
		(ll (cdr l) (cdr ll)))
	       ((null ll) (ptimes g (makprod a nil))))))))

(defun  facrtimes (x y gcdsw)
  (cond ((not gcdsw)
	 (cons (ptimes (car x) (car y)) (ptimeschk (cdr x) (cdr y))))
	(t (let ((g (cdinf (car x) (car y) t))
		 (h (cdinf (cdr x) (cdr y) t)))
	     (setq x (fpgcdco (cadr g) (caddr h)))
	     (setq y (fpgcdco (caddr g) (cadr h)))
	     (cons (ptimes (car g) (ptimes (cadr x) (cadr y)))
		   (ptimeschk (car h) (ptimeschk (caddr x) (caddr y))))))))

(defun pfacprod (poly) 			;FOR RAT3D
  (cond ((pcoefp poly) (cfactor poly))
	(t (nconc (pfacprod (caddr poly))
		  (list (pget (car poly)) (cadr poly))))))

(defun fpcontent (poly)
  (let (($ratfac
	 nil))				;algebraic uses
    (setq poly (oldcontent poly))	;rattimes?
    (let ((a (lowdeg (cdadr poly))))	;main var. content
      (cond ((> a 0) (setq a (list (caadr poly) a 1))
	     (setq poly
		   (list (ptimes (car poly) a)
			 (pquotient (cadr poly) a))))))
    (if (pminusp (cadr poly))
	(list (pminus (car poly)) (pminus (cadr poly)))
	poly)))

;; LOWDEG written to compute the lowest degree of a polynomial. - RZ

(defmfun lowdeg (p)
  (do ((l p (cddr l)))
      ((null (cddr l)) (car l))))

(defun makprod (poly contswitch)
  (cond ((pureprod poly) poly)
	((null (cdddr poly))
	 (ptimes (list (car poly) (cadr poly) 1)
		 (makprod (caddr poly) contswitch)))
	(contswitch (makprod1 poly))
	(t (setq poly (fpcontent poly))
	   (ptimes (makprod (car poly) contswitch) (makprod1 (cadr poly))))))

(defun makprod1 (poly)
  (do ((v varlist (cdr v))
       (g genvar (cdr g))
       (p (pdis poly)))
      ((null v) (maksymp poly))
    (and (alike1 p (car v)) (return (pget (car g))))))

(defun maksym (p)
  (newsym2 p (pdis p)))

(defun maksymp (p)
  (cond ((atom p) p)
	(t (pget (maksym p)))))

(defun pflatten (h)
  (prog (m)
     (setq m (listovars h))
     checkmore
     (cond ((null m) (return h))
	   ((not (let ((p (getunhack (car m))))
		   (or (null p) (eq (car m) (car p)))))
	    (go redo))
	   (t (setq m (cdr m)) (go checkmore)))
     redo (return (let ($ratfac) (pflat1 h)))))

(defun pflat1 (p)
  (cond ((pcoefp p) p)
	((null (cdddr p))
	 (ptimes (pexpt (getunhack (car p)) (cadr p)) (pflat1 (caddr p))))
	(t (do ((val (getunhack (car p)))
		(ld (cadr p) (car a))
		(a (cdddr p) (cddr a))
		(ans (pflat1 (caddr p))))
	       ((null a) (ptimes ans (pexpt val ld)))
	     (setq ans
		   (pplus (ptimes ans
				  (pexpt val (- ld (car a))))
			  (pflat1 (cadr a))))))))

(defun pirredp (x)
  (and (setq x (getdis x))
       (or (atom x) (member 'irreducible (cdar x) :test #'eq))))

(defun knownfactors (d)
  (prog (h)
     (cond ((pcoefp d) (return d)))
     (setq h (getdis (car d)))
     (return (cond ((or (atom h) (not (eq (caar h) 'mtimes)))
		    (ptimes (knownfactors (caddr d))
			    (list (car d) (cadr d) 1)))
		   (t (setq h (getunhack (car d)))
		      (ptimes (knownfactors (caddr d))
			      (pexpt (knownfactors h) (cadr d))))))))
