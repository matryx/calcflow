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

(macsyma-module rat3c)

;;	THIS IS THE NEW RATIONAL FUNCTION PACKAGE PART 3.
;;	IT INCLUDES THE GCD ROUTINES AND THEIR SUPPORTING FUNCTIONS

(load-macsyma-macros ratmac)

(declare-top (special $float $keepfloat $algebraic $ratfac genvar))

;; List of GCD algorithms.  Default one is first.
(defmvar *gcdl* '($spmod $subres $ez $red $mod $algebraic))

(defmvar $gcd (car *gcdl*))		;Sparse Modular

(defun cgcd (a b)
  (cond (modulus 1)
	((and $keepfloat (or (floatp a) (floatp b))) 1)
	(t (gcd a b))))

(defmfun pquotientchk (a b)
  (if (eqn b 1) a (pquotient a b)))

;; divides polynomial x by polynomial y
;; avoids error "quotient by polynomial of higher degree"
;;  (returns nil in this case)
(defun pquotientchk-safe (x y)
  (let ((errrjfflag t))
    (catch 'raterr (pquotientchk x y))))

(defun ptimeschk (a b)
  (cond ((eqn a 1) b)
	((eqn b 1) a)
	(t (ptimes a b))))

(defun pfloatp (x)
  (catch 'float (if (pcoefp x) (floatp x) (pfloatp1 x))))

(defun pfloatp1 (x)
  (mapc #'(lambda (q) (cond ((pcoefp q) (when (floatp q) (throw 'float t)))
			    ((pfloatp1 q))))
	(cdr x))
  nil)

(defmfun pgcd (x y)
  (setq x (car (pgcda x y nil)))
  (cond ((pminusp x) (pminus x))
	(modulus (monize x))
	(t x)))

(defmfun plcm (x y)
  (setq x (pgcdcofacts x y))
  (ptimes (car x) (ptimes (cadr x) (caddr x))))

(defun plcmcofacts (x y)
  (setq x (pgcdcofacts x y))
  (list (ptimes (car x) (ptimes (cadr x) (caddr x)))
	(caddr x) (cadr x)))

; returns list (gcd xx yy alg)
; where x * y = gcd^2 * xx * yy / alg^2
; and alg is non-nil only when $algebraic is true
(defun pgcdcofacts (x y)
  (let ((a (pgcda x y t)))
    (cond ((cdr a) a)
	  ((equal (setq a (car a)) 1) (list 1 x y))
	  ((and $algebraic (not (pcoefp a)))
	   (cons a (prog2 (setq x (rquotient x a)
				y (rquotient y a)
				a (pgcdcofacts (cdr x) (cdr y)))
		       (list (ptimes (car x) (caddr a))
			     (ptimes (car y) (cadr a))
			     (ptimes (cadr a) (cdr y))))))
	  ((eq a x) (list x 1 (pquotient y x)))
	  ((eq a y) (list a (pquotient x y) 1))
	  (t (list a (pquotient x a) (pquotient y a))))))

(defun pgcda (x y cofac? &aux a c)
  (cond ((not $gcd) (list 1 x y))
	((and $keepfloat (or (pfloatp x) (pfloatp y)))
	 (cond ((or (pcoefp x) (pcoefp y)
		    (pcoefp (setq a (car (ptermcont x))))
		    (pcoefp (setq a (pgcd a (car (ptermcont y))))))
		(list 1 x y))
	       (t (list a))))
	((pcoefp x)
	 (cond ((pcoefp y)
		(cons (setq a (cgcd x y))
		      (and cofac?
			   (list (cquotient x a) ;(CQUOTIENT 0 0) = 0
				 (cquotient y a)))))
	       ((zerop x) (list y x 1))
	       (t (list (pcontent1 (cdr y) x)))))
	((pcoefp y) (cond ((zerop y) (list x 1 y))
			  (t (list (pcontent1 (cdr x) y)))))
	((equal x y) (list x 1 1))
	($ratfac (fpgcdco x y))
	((not (eq (p-var x) (p-var y)))
	 (list (if (pointergp (p-var x) (p-var y))
		   (oldcontent1 (p-terms x) y)
		   (oldcontent1 (p-terms y) x))))
	((progn (desetq (a x) (ptermcont x))
		(desetq (c y) (ptermcont y))
		(not (and (equal a 1) (equal c 1))))
	 (mapcar #'ptimes (monomgcdco a c cofac?) (pgcda x y cofac?)))
	((and (not $algebraic) (not modulus)
	      (desetq (a . c) (lin-var-find (nreverse (pdegreevector x))
					    (nreverse (pdegreevector y))
					    (reverse genvar))))
	 (cond ((= a 1) (linhack x y (car c) (cadr c) cofac?))
	       (t (setq a (linhack y x a (cadr c) cofac?))
		  (if (cdr a) (rplacd a (nreverse (cdr a))))
		  a)))
	((eq $gcd '$spmod) (list (zgcd x y)))
	((eq $gcd '$subres) (list (oldgcd x y)))
	((eq $gcd '$algebraic)
	 (if (or (palgp x) (palgp y))
	     (let (($gcd '$subres)) (list (oldgcd x y)))
	     (let (($gcd '$spmod)) (list (zgcd x y)))))
	((eq $gcd '$ez) (ezgcd2 x y))
	((eq $gcd '$red) (list (oldgcd x y)))
	((eq $gcd '$mod) (newgcd x y modulus))
	((not (member $gcd *gcdl* :test #'eq))
	 (merror (intl:gettext "gcd: 'gcd' variable must be one of ~M; found: ~M") *gcdl* $gcd))
	(t (list 1 x y))))

(defun monomgcdco (p q cofac?)
  (let ((gcd (monomgcd p q)))
    (cons gcd (if cofac? (list (pquotient p gcd) (pquotient q gcd)) ()))))

(defun monomgcd (p q)
  (cond ((or (pcoefp p) (pcoefp q)) 1)
	((eq (p-var p) (p-var q))
	 (make-poly (p-var p) (min (p-le p) (p-le q))
		    (monomgcd (p-lc p) (p-lc q))))
	((pointergp (car p) (car q)) (monomgcd (p-lc p) q))
	(t (monomgcd p (p-lc q)))))

(defun linhack (pol1 pol2 nonlindeg var cofac?)
  (prog (coeff11 coeff12 gcdab rpol1 rpol2 gcdcd gcdcoef)
     (desetq (coeff11 . coeff12) (bothprodcoef (make-poly var) pol1))
     (setq gcdab (if (pzerop coeff12) coeff11
		     (pgcd coeff11 coeff12)))
     (cond ((equal gcdab 1)
	    (cond ((setq coeff11 (testdivide pol2 pol1))
		   (return (list pol1 1 coeff11)))
		  (t (return (list 1 pol1 pol2))))))
     (setq rpol1 (pquotient pol1 gcdab))
     (desetq (gcdcd rpol2) (linhackcontent var pol2 nonlindeg))
     (cond ((equal gcdcd 1)
	    (cond ((setq coeff12 (testdivide rpol2 rpol1))
		   (return (list rpol1 gcdab coeff12)))
		  (t (return (list 1 pol1 pol2))))))
     (cond (cofac? (desetq (gcdcoef coeff11 coeff12)
			   (pgcdcofacts gcdab gcdcd))
		   (cond ((setq gcdcd (testdivide rpol2 rpol1))
			  (return (list (ptimes gcdcoef rpol1)
					coeff11
					(ptimes coeff12 gcdcd))))
			 (t (return (list gcdcoef
					  (ptimes coeff11 rpol1)
					  (ptimes coeff12 rpol2))))))
	   (t (setq gcdcoef (pgcd gcdcd gcdab))
	      (cond ((testdivide rpol2 rpol1)
		     (return (list (ptimes gcdcoef rpol1))))
		    (t (return (list gcdcoef))))))))

(defun lin-var-find (a b c)
  (do ((varl c (cdr varl))
       (degl1 a (cdr degl1))
       (degl2 b (cdr degl2)))
      ((or (null degl1) (null degl2)) nil)
    (if (equal (min (car degl1) (car degl2)) 1)
	(return (list (car degl1) (car degl2) (car varl))))))

(defun linhackcontent (var pol nonlindeg &aux (npol pol) coef gcd)
  (do ((i nonlindeg (1- i)))
      ((= i 0) (list (setq gcd (pgcd gcd npol)) (pquotient pol gcd)))
    (desetq (coef . npol) (bothprodcoef (make-poly var i 1) npol))
    (unless (pzerop coef)
      (setq gcd (if (null gcd) coef (pgcd coef gcd)))
      (if (equal gcd 1) (return (list 1 pol))))))

;;*** THIS IS THE REDUCED POLYNOMIAL REMAINDER SEQUENCE GCD (COLLINS')

(defun oldgcd (x y &aux u v s egcd)	;only called from pgcda
  (desetq (x  u) (oldcontent x))
  (desetq (y  v) (oldcontent y))
  (setq egcd (gcd (pgcdexpon u) (pgcdexpon v)))
  (if (> egcd 1)
      (setq u (pexpon*// u egcd nil)
	    v (pexpon*// v egcd nil)))
  (if (> (p-le v) (p-le u)) (exch u v))
  (setq s (case $gcd
	    ($red (redgcd u v))
	    ($subres (subresgcd u v))
	    (t (merror "OLDGCD: found gcd = ~M; how did that happen?" $gcd))))
  (let ((errrjfflag t))			;; check for gcd that simplifies to 0
    (if (not (catch 'raterr (rainv s))) ;; sourceforge bugs 831445 and 1313987
	(setq s 1)))
  (unless (equal s 1)
    (setq s (pexpon*// (primpart
			(if $algebraic s
			    (pquotient s (pquotient (p-lc s)
						    (pgcd (p-lc u) (p-lc v))))))
		       egcd t)))
  (setq s (ptimeschk s (pgcd x y)))
  (and $algebraic (not (pcoefp (setq u (leadalgcoef s))))
       (not (equal u s)) (setq s (algnormal s)))
  (cond (modulus (monize s))
	((pminusp s) (pminus s))
	(t s)))

(defun pgcdexpon (p)
  (if (pcoefp p) 0
      (do ((d (cadr p) (gcd d (car l)))
	   (l (cdddr p) (cddr l)))
	  ((or (null l) (= d 1)) d))))

(defun pexpon*// (p n *?)
  (if (or (pcoefp p) (= n 1)) p
      (do ((ans (list (car p))
		(cons (cadr l)
		      (cons (if *? (* (car l) n)
				(truncate (car l) n))
			    ans)))
	   (l (cdr p) (cddr l)))
	  ((null l) (nreverse ans)))))

;;polynomial gcd using reduced prs

(defun redgcd (p q &aux (d 0))
  (loop until (zerop (pdegree q (p-var p)))
	 do (psetq p q
		   q (pquotientchk-safe (prem p q) (pexpt (p-lc p) d))
		   d (+ (p-le p) 1 (- (p-le q))))
	 (if (< d 1) (return 1))
	 finally (return (if (pzerop q) p 1))))

;;computes gcd's using subresultant prs
;;ACM Transactions On Mathematical Software Sept. 1978

(defun subresgcd (p q)
  (loop for g = 1 then (p-lc p)
	 for h = 1 then (pquotientchk-safe (pexpt g d) h^1-d)
	 for d = (- (p-le p) (p-le q))
	 for h^1-d = 1 then (if (< d 1)
				(return 1)
			      (pexpt h (1- d)))
	 do (psetq p q
		   q (pquotientchk-safe (prem p q) (ptimes g (ptimes h h^1-d))))
	 if (zerop (pdegree q (p-var p))) return (if (pzerop q) p 1)))

;;*** THIS COMPUTES PSEUDO REMAINDERS

(defun psquorem1 (u v quop)
  (prog (k (m 0) lcu lcv quo lc)
     (declare (special lcu lcv))
     (setq lcv (pt-lc v))
     (setq k (- (pt-le u) (pt-le v)))
     (cond ((minusp k) (return (list 1 '(0 0) u))))
     (if quop (setq lc (pexpt (pt-lc v) (1+ k))))
     a     (setq lcu (pminus (pt-lc u)))
     (if quop (setq quo (cons (ptimes (pt-lc u) (pexpt (pt-lc v) k))
			      (cons k quo))))
     (cond ((null (setq u (pgcd2 (pt-red u) (pt-red v) k)))
	    (return (list lc (nreverse quo) '(0 0))))
	   ((minusp (setq m (- (pt-le u) (pt-le v))))
	    (setq u (cond ((zerop k) u)
			  (t (pctimes1 (pexpt lcv k) u))))
	    (return (list lc (nreverse quo) u)))
	   ((> (1- k) m)
	    (setq u (pctimes1 (pexpt lcv (- (1- k) m)) u))))
     (setq k m)
     (go a)))

(defun prem (p q)
  (cond ((pcoefp p) (if (pcoefp q) (cremainder p q) p))
	((pcoefp q) (pzero))
	(t (psimp (p-var p) (pgcd1 (p-terms p) (p-terms q))))))

(defmfun pgcd1 (u v) (caddr (psquorem1 u v nil)))

(defun pgcd2 (u v k &aux (i 0))
  (declare (special lcu lcv) (fixnum k i))
  (cond ((null u) (pcetimes1 v k lcu))
	((null v) (pctimes1 lcv u))
	((zerop (setq i (+ (pt-le u) (- k) (- (car v)))))
	 (pcoefadd (pt-le u) (pplus (ptimes lcv (pt-lc u))
				    (ptimes lcu (pt-lc v)))
                   (pgcd2 (pt-red u) (pt-red v) k)))
	((minusp i)
         (list* (+ (pt-le v) k) (ptimes lcu (pt-lc v)) (pgcd2 u (pt-red v) k)))
        (t (list* (pt-le u) (ptimes lcv (pt-lc u)) (pgcd2 (pt-red u) v k)))))

;;;*** OLDCONTENT REMOVES ALL BUT MAIN VARIABLE AND PUTS THAT IN CONTENT
;;;***  OLDCONTENT OF 3*A*X IS 3*A (WITH MAINVAR=X)

(defun rcontent (p)			;RETURNS RAT-FORMS
  (let ((q (oldcontenta p)))
    (list (cons q 1) (cond ($algebraic (rquotient p q))
			   (t (cons (pquotient p q) 1))))))

(defun oldcontenta (x)
  (cond ((pcoefp x) x)
	(t (setq x (contsort (cdr x)))
	   (oldcontent2 (cdr x) (car x)))))

(defmfun oldcontent (x)
  (cond ((pcoefp x) (list x 1))
	((null (p-red x))
	 (list (p-lc x) (make-poly (p-var x) (p-le x) 1)))
	(t (let ((u (contsort (cdr x))) v)
	     (setq u (oldcontent2 (cdr u) (car u))
		   v (cond ($algebraic (car (rquotient x u)))
			   (t (pcquotient x u))))
	     (cond ((pminusp v) (list (pminus u) (pminus v)))
		   (t (list u v)))))))

(defun oldcontent1 (x gcd)
  (cond ((equal gcd 1) 1)
	((null x) gcd)
	(t (oldcontent2 (contsort x) gcd))))

(defun oldcontent2 (x gcd)
  (do ((x x (cdr x))
       (gcd gcd (pgcd (car x) gcd)))
      ((or (null x) (equal gcd 1)) gcd)))

(defun contsort (x)
  (setq x (coefl x))
  (cond ((member 1 x) '(1))
	((null (cdr x)) x)
	(t (sort x #'contodr))))

(defun coefl (x)
  (do ((x x (cddr x))
       (ans nil (cons (cadr x) ans)))
      ((null x) ans)))

(defun contodr (a b)
  (cond ((pcoefp a) t)
	((pcoefp b) nil)
	((eq (car a) (car b)) (not (> (cadr a) (cadr b))))
	(t (pointergp (car b)(car a)))))

;;;*** PCONTENT COMPUTES INTEGER CONTENT
;;;*** PCONTENT OF 3*A*X IS 3 IF MODULUS = NIL  1 OTHERWISE

(defun pcontent (x)
  (cond ((pcoefp x) (list x 1))
	(t (let ((u (pcontentz x)))
	     (if (eqn u 1) (list 1 x)
		 (list u (pcquotient x u)))))))

(defun pcontent1 (x gcd)
  (do ((x x (cddr x))
       (gcd gcd (cgcd gcd (pcontentz (cadr x)))))
      ((or (null x) (equal gcd 1)) gcd)))

(defun pcontentz (p)
  (cond ((pcoefp p) p)
	(t (pcontent1 (p-red p) (pcontentz (p-lc p))))))

(defun ucontent (p)			;CONTENT OF UNIV. POLY
  (cond ((pcoefp p) (abs p))
	(t (setq p (mapcar #'abs (coefl (cdr p))))
	   (let ((m (apply #'min p)))
	     (oldcontent2 (delete m p :test #'equal) m)))))

;;***	PGCDU CORRESPONDS TO BROWN'S ALGORITHM U

;;;PGCDU IS NOT NOW IN RAT;UFACT >

(defmfun pgcdu (p q)
  (do () ((pzerop q) (monize p))
    (psetq p q q (pmodrem p q))))

(defun pmodrem (x y)
  (cond ((null modulus)
	 (merror "PMODREM: null modulus; how did that happen?"))
	((pacoefp y) (if (pzerop y) x 0))
	((pacoefp x) x)
	((eq (p-var x) (p-var y))
	 (psimp (car x) (pgcdu1 (p-terms x) (p-terms y) nil)))
	(t (merror "PMODREM: I can't handle this; x = ~M, y = ~M" x y))))

(defun pmodquo (u v &aux quo)
  (declare (special quo))
  (cond ((null modulus)
	 (merror "PMODQUO: null modulus; how did that happen?"))
	((pcoefp v) (cons (ptimes (crecip v) u) 0))
	((alg v) (cons (ptimes (painvmod v) u) 0))
	((pacoefp u) (cons 0 u))
	((not (eq (p-var u) (p-var v)))
	 (merror "PMODQUO: arguments have different variables; how did that happen?"))
	(t (xcons (psimp (car u) (pgcdu1 (cdr u) (cdr v) t))
		  (psimp (car u) quo)))))


(defun pgcdu1 (u v pquo*)
  (let ((invv (painvmod (pt-lc v))) (k 0) q*)
    (declare (special k quo q*) (fixnum k))
    (loop until (minusp (setq k (- (pt-le u) (pt-le v))))
	   do (setq q* (ptimes invv (pt-lc u)))
	   if pquo* do (setq quo (nconc quo (list k q*)))
	   when (ptzerop (setq u (pquotient2 (pt-red u) (pt-red v))))
	   return (ptzero)
	   finally (return u))))

(defun rtzerl2 (n)
  (cond ((zerop n) 0)
	(t (do ((n n (ash n -2)))
	       ((not (zerop (haipart n -2))) n)))))

(defmfun $jacobi (p q)
  (cond ((null (and (integerp p) (integerp q)))
	 (list '($jacobi) p q))
	((zerop q) (merror (intl:gettext "jacobi: zero denominator.")))
	((minusp q) ($jacobi p (- q)))
	((and (evenp (setq q (rtzerl2 q)))
	      (setq q (ash q -1))
	      (evenp p)) 0)
	((equal q 1) 1)
	((minusp (setq p (rem p q)))
	 (jacobi (rtzerl2 (+ p q)) q))
	(t (jacobi (rtzerl2 p) q))))

(defun jacobi (p q)
  (do ((r1 p (rtzerl2 (rem r2 r1)))
       (r2 q r1)
       (bit2 (haipart q -2))
       (odd 0 (boole boole-xor odd (boole boole-and bit2 (setq bit2 (haipart r1 -2))))))
      ((zerop r1) 0)
    (cond ((evenp r1)
	   (setq r1 (ash r1 -1))
	   (setq odd (boole boole-xor odd (ash (expt (haipart r2 -4) 2) -2)))))
    (and (equal r1 1) (return (expt -1 (boole  boole-and 1 (ash odd -1)))))))

;; it is convenient to have the *bigprimes* be actually less than
;; half the size of the most positive fixnum, so that arithmetic is easier

(defvar *bigprimes* (loop with p = (ash most-positive-fixnum -1) repeat 20 do
			 (setq p (next-prime (1- p) -1))
		       collect p))

(defmvar *alpha (car *bigprimes*))

(defun newprime (p)
  (do ((pl *bigprimes* (cdr pl)))
      ((null pl)
       (setq p (next-prime (1- p) -1))
       (setq *bigprimes* (nconc *bigprimes* (list p)))
       p)
    (when (< (car pl) p)
      (return (car pl)))))

(defun leadcoefficient (p)
  (if (pcoefp p) p (leadcoefficient (caddr p))))

(defun maxcoefficient (p)
  (if (pcoefp p) (abs p) (maxcoef1 (cdr p))))

(defun maxcoef1 (p)
  (if (null p) 0 (max (maxcoefficient (cadr p)) (maxcoef1 (cddr p)))))

(defun maxnorm (poly)
  (if (null poly) 0 (max (norm (cadr poly)) (maxnorm (cddr poly)))))

(defun norm (poly)
  (cond ((null poly) 0)
	((pcoefp poly) (abs poly))
	(t (+ (norm (caddr poly)) (norm1 (cdddr poly)) )) ))

(defun norm1 (poly)
  (if (null poly) 0 (+ (norm (cadr poly)) (norm1 (cddr poly)) )) )

(defmfun pdegree (p var)
  (cond ((pcoefp p) 0)
	((eq var (p-var p)) (p-le p))
	((pointergp var (p-var p)) 0)
	(t (do ((l (p-red p) (pt-red l))
		(e (pdegree (p-lc p) var) (max e (pdegree (pt-lc l) var))))
	       ((null l) e)))))

(defun poly-in-var (p v)
  (cond ((or (pcoefp p) (pointergp v (p-var p))) (list 0 p))
	((eq (p-var p) v) (p-terms p))
	((loop with ans
		for (exp coef) on (p-terms p) by #'cddr
		do (setq ans (pplus1 ans
				     (everysubst2 (poly-in-var coef v)
						  (list (p-var p) exp 1))))
		finally (return ans)))))

(defun univar (x)
  (or (null x) (and (pcoefp (pt-lc x)) (univar (pt-red x)))))

;;**THE CHINESE REMAINDER ALGORITHM IS A SPECIAL CASE OF LAGRANGE INTERPOLATION

(defun lagrange3 (u uk p qk)
  (setqmodulus p)
  (setq uk (pdifference uk (pmod u)))
  (cond ((pzerop uk) (setq modulus nil) u)
	(t (setq uk (pctimes (crecip (cmod qk)) uk))
	   (setq modulus nil)
	   (pplus u (pctimes qk uk)))))


(defun lagrange33 (u uk qk xk)
  (declare (special xv))
  (setq uk (pdifference uk (pcsubst u xk xv)))
  (cond ((pzerop uk) u)
	(t (pplus u (ptimes
		     (pctimes (crecip (pcsubst qk xk xv)) uk)
		     qk)))))


;;;*************************************************************

;;	THIS IS THE END OF THE NEW RATIONAL FUNCTION PACKAGE PART 3.
;;	IT INCLUDES THE GCD ROUTINES AND THEIR SUPPORTING FUNCTIONS
