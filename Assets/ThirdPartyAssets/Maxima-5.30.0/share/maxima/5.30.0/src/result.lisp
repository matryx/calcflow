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

(macsyma-module result)

(declare-top (special varlist genvar $ratfac $keepfloat modulus *alpha xv))

(load-macsyma-macros ratmac)

(defmfun $poly_discriminant (poly var)
  (let* ((varlist (list var))
	 ($ratfac nil)
	 (genvar ())
	 (rform (rform poly))
	 (rvar (car (last genvar)))
	 (n (pdegree (setq poly (car rform)) rvar)))

    (cond ((= n 1) 1)
	  ((or (= n 0) (not (atom  (cdr rform))))
	   (merror (intl:gettext "poly_discriminant: first argument must be a polynomial in ~:M; found: ~M") var poly))
	  (t (pdis (presign
		    (ash (* n (1- n)) -1)
		    (pquotient (resultant poly (pderivative poly rvar))
			       (p-lc poly))))))))

(defmfun $resultant (a b mainvar)
  (prog (varlist formflag $ratfac res ans genvar $keepfloat)
     (setq varlist (list mainvar) $ratfac t ans 1)
     (and ($ratp a)(setq formflag t)(setq a ($ratdisrep a)))
     (and ($ratp b)(setq formflag t)(setq b ($ratdisrep b)))
     (newvar a)
     (newvar b)
     (setq a (lmake2 (cadr (ratrep* a)) nil))
     (setq b (lmake2 (cadr (ratrep* b)) nil))
     (setq mainvar (caadr (ratrep* mainvar)))
     (do ((l1 a (cdr l1))) ((null l1))
       (do ((l2 b (cdr l2))) ((null l2))
	 (setq res (result1 (caar l1) (caar l2) mainvar))
	 (setq ans (ptimes ans (pexpt
				(cond ((zerop (caddr res)) (car res))
				      (t (ptimeschk (car res)
						    (pexpt (makprod (cadr res) nil)
							   (caddr res)))))
				(* (cdar l1) (cdar l2)))))))
     (return (cond (formflag (pdis* ans)) (t (pdis ans))))))

(defun result1 (p1 p2 var)
  (cond ((or (pcoefp p1) (pointergp var (car p1)))
	 (list 1 p1 (pdegree p2 var)))
	((or (pcoefp p2) (pointergp var (car p2)))
	 (list 1 p2 (pdegree p1 var)))
	((null (cdddr p1))
	 (cond ((null (cdddr p2)) (list 0 0 1))
	       (t (list (pexpt (caddr p1) (cadr p2))
			(pcsubsty 0 var p2)
			(cadr p1)))))
	((null (cdddr p2))
	 (list (pexpt (caddr p2) (cadr p1))
	       (pcsubsty 0 var p1)
	       (cadr p2)))
	((> (setq var (gcd (pgcdexpon p1) (pgcdexpon p2))) 1)
	 (list 1 (resultant (pexpon*// p1 var nil)
			    (pexpon*// p2 var nil)) var))
	(t (list 1 (resultant p1 p2) 1))))

(defmvar $resultant '$subres "Designates which resultant algorithm")

(defvar *resultlist '($subres $mod $red))

(defmfun resultant (p1 p2)		;assumes same main var
  (if (> (p-le p2) (p-le p1))
      (presign (* (p-le p1) (p-le p2)) (resultant p2 p1))
      (case $resultant
	($subres (subresult p1 p2))
	#+broken ($mod (modresult p1 p2))
	($red (redresult p1 p2))
	(t (merror (intl:gettext "resultant: no such algorithm: ~M") $resultant)))))

(defun presign (n p)
  (if (oddp n) (pminus p) p))

;;computes resultant using subresultant p.r.s. TOMS Sept. 1978

(defun subresult (p q)
  (loop for g = 1 then (p-lc p)
	 for h = 1 then (pquotient (pexpt g d) h^1-d)
	 for degq = (pdegree q (p-var p))
	 for d = (- (p-le p) degq)
	 for h^1-d = (if (equal h 1) 1 (pexpt h (1- d)))
	 if (zerop degq) return (if (pzerop q) q (pquotient (pexpt q d) h^1-d))
	 do (psetq p q
		   q (presign (1+ d) (pquotient (prem p q)
						 (ptimes g (ptimes h h^1-d)))))))

;;	PACKAGE FOR CALCULATING MULTIVARIATE POLYNOMIAL RESULTANTS
;;	USING MODIFIED REDUCED P.R.S.

(defun redresult (u v)
  (prog (a r sigma c)
     (setq a 1)
     (setq sigma 0)
     (setq c 1)
     a    (if (pzerop (setq r (prem u v))) (return (pzero)))
     (setq c (ptimeschk c (pexpt (p-lc v)
				 (* (- (p-le u) (p-le v))
				     (- (p-le v) (pdegree r (p-var u))
					 1)))))
     (setq sigma (+ sigma (* (p-le u) (p-le v))))
     (if (zerop (pdegree r (p-var u)))
	 (return
	   (presign sigma
		    (pquotient (pexpt (pquotientchk r a) (p-le v)) c))))
     (psetq u v
	    v (pquotientchk r a)
	    a (pexpt (p-lc v) (+ (p-le u) 1 (- (p-le v)))))
     (go a)))


;;	PACKAGE FOR CALCULATING MULTIVARIATE POLYNOMIAL RESULTANTS
;;	USING MODULAR AND EVALUATION HOMOMORPHISMS.
;; modresultant fails on the following example
;;RESULTANT(((-4)*Z)^4+(Y+8*Z)^4+(X-5*Z)^4-1,
;;	       ((-4)*Z)^4-(X-5*Z)^3*((-4)*Z)^3+(Y+8*Z)^3*((-4)*Z)^2
;;			 +(-2)*(Y+8*Z)^4+((-4)*Z)^4+1,Z)

#+broken
(progn
  (defun modresult (a b)
    (modresult1 a b (sort (union* (listovars a) (listovars b))
			  (function pointergp))))

  (defun modresult1 (x y varl)
    (cond ((null modulus) (pres x y (car varl) (cdr varl)))
	  (t (cpres x y (car varl) (cdr varl))) ))

  (defun pres (a b xr1 varl)
    (prog (m n f a* b* c* p q c modulus hmodulus)
       (setq m (cadr a))
       (setq n (cadr b))
       (setq f (coefbound m n (maxnorm (cdr a)) (maxnorm (cdr b)) ))
       (setq q 1)
       (setq c 0)
       (setq p *alpha)
       (go step3)
       step2	(setq p (newprime p))
       step3	(setqmodulus p)
       (setq a* (pmod a))
       (setq b* (pmod b))
       (cond ((or (reject a* m xr1) (reject b* n xr1)) (go step2)))
       (setq c* (cpres a* b* xr1 varl))
       (setqmodulus nil)
       (setq c (lagrange3 c c* p q))
       (setq q (* p q))
       (cond ((> q f) (return c))
	     (t (go step2)) ) ))

  (defun reject (a m xv)
    (not (eqn (pdegree a xv) m)))

  (defun coefbound (m n d e)
    (* 2 (expt (1+ m) (ash n -1))
	   (expt (1+ n) (ash m -1))
	   (cond ((oddp n) (1+ ($isqrt (1+ m))))
		 (t 1))
	   (cond ((oddp m) (1+ ($isqrt (1+ n))))
		 (t 1))
	   ;; (FACTORIAL (PLUS M N)) USED TO REPLACE PREV. 4 LINES. KNU II P. 375
	   (expt d n)
	   (expt e m) ))

  (defun main2 (a var exp tot)
    (cond ((null a) (cons exp tot))
	  (t (main2 (cddr a) var
		    (max (setq var (pdegree (cadr a) var)) exp)
		    (max (+ (car a) var) tot))) ))

  (defun cpres (a b xr1 varl)		;XR1 IS MAIN VAR WHICH
    (cond ((null varl) (cpres1 (cdr a) (cdr b))) ;RESULTANT ELIMINATES
	  (t	(prog (  m2 		  ( m1 (cadr a))
		       ( n1 (cadr b))  n2 (k 0) c d a* b* c* bp xv) ;XV IS INTERPOLATED VAR
		   (declare (fixnum m1 n1 k))

		   step2
		   (setq xv (car varl))
		   (setq varl (cdr varl))
		   (setq m2 (main2 (cdr a) xv 0 0)) ;<XV DEG . TOTAL DEG>
		   (setq n2 (main2 (cdr b) xv 0 0))
		   (cond ((zerop (+ (car m2) (car n2)))
			  (cond ((null varl) (return (cpres1 (cdr a) (cdr b))))
				(t (go step2)) ) ))
		   (setq k (1+ (min (+ (* m1 (car n2)) (* n1 (car m2)))
				     (+ (* m1 (cdr n2)) (* n1 (cdr m2))
					 (- (* m1 n1))) )))
		   (setq c 0)
		   (setq d 1)
		   (setq m2 (car m2) n2 (car n2))
		   (setq bp (- 1))
		   step3
		   (cond ((equal (setq bp (1+ bp)) modulus)
			  (merror "CPRES: resultant primes too small."))
			 ((zerop m2) (setq a* a))
			 (t (setq a* (pcsubst a bp xv))
			    (cond ((reject a* m1 xr1)(go step3)) )) )
		   (cond ((zerop n2) (setq b* b))
			 (t (setq b* (pcsubst b bp xv))
			    (cond ((reject b* n1 xr1) (go step3))) ))
		   (setq c* (cpres a* b* xr1 varl))
		   (setq c (lagrange33 c c* d bp))
		   (setq d (ptimeschk d (list xv 1 1 0 (cminus bp))))
		   (cond ((> (cadr d) k) (return c))
			 (t (go step3))))))))


;; *** NOTE THAT MATRIX PRODUCED IS ALWAYS SYMETRIC
;; *** ABOUT THE MINOR DIAGONAL.

(defmfun $bezout (p q var)
  (let ((varlist (list var)) genvar)
    (newvar p)
    (newvar q)
    (setq p (cadr (ratrep* p))
	  q (cadr (ratrep* q)))
    (setq p (cond ((> (cadr q) (cadr p)) (bezout q p))
		  (t (bezout p q))))
    (cons '($matrix)
	  (mapcar #'(lambda (l) (cons '(mlist) (mapcar 'pdis l)))
		  p))))

(defun vmake (poly n *l)
  (do ((i (1- n) (1- i))) ((minusp i))
    (cond ((or (null poly) (< (car poly) i))
	   (setq *l (cons 0 *l)))
	  (t (setq *l (cons (cadr poly) *l))
	     (setq poly (cddr poly)))))
  (nreverse *l))

(defun bezout (p q)
  (let* ((n (1+ (p-le p)))
	 (n2 (- n (p-le q)))
	 (a (vmake (p-terms p) n nil))
	 (b (vmake (p-terms q) n nil))
	 (ar (reverse (nthcdr n2 a)))
	 (br (reverse (nthcdr n2 b)))
	 (l (make-list n :initial-element 0)))
    (rplacd (nthcdr (1- (p-le p)) a) nil)
    (rplacd (nthcdr (1- (p-le p)) b) nil)
    (nconc
     (mapcar
      #'(lambda (ar br)
	  (setq l (mapcar #'(lambda (a b l)
			      (ppluschk l (pdifference
					   (ptimes br a) (ptimes ar b))))
			  a b (cons 0 l))))
      ar br)
     (and (pzerop (car b))
	  (do ((b (vmake (cdr q) (cadr p) nil) (rot* b))
	       (m nil (cons b m)))
	      ((not (pzerop (car b))) (cons b m))))) ))

(defun rot* (b)
  (setq b (copy-list b))
  (prog2
      (nconc b b)
      (cdr b)
    (rplacd b nil)))


(defun ppluschk (p q)
  (cond ((pzerop p) q)
	(t (pplus p q))))
