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

(macsyma-module rat3d)

(load-macsyma-macros ratmac)


;;	THIS IS THE NEW RATIONAL FUNCTION PACKAGE PART 4.
;;	IT INCLUDES THE POLYNOMIAL FACTORING ROUTINES.

(declare-top (special *min* *mx* *odr* nn* scanmapp *checkagain adn*))

(declare-top (special $factorflag $intfaclim $dontfactor $algebraic $ratfac errrjfflag))

;;There really do seem to be two such variables...
(declare-top (special alpha *alpha gauss genvar minpoly*))

(defmvar *irreds nil)
(defmvar algfac* nil)
(defmvar low* nil)

(defmvar $intfaclim t)
(defmvar $berlefact t)

(defmfun listovars (q)
  (cond ((pcoefp q) nil)
	(t (let ((ans nil))
	     (declare (special ans))
	     (listovars0 q)))))

(defun listovars0 (q)
  (declare (special ans))
  (cond ((pcoefp q) ans)
	((member (car q) ans :test #'eq) (listovars1 (cdr q)))
	(t (push (car q) ans)
	   (listovars1 (cdr q)))))

(defun listovars1 (ql)
  (declare (special ans))
  (cond ((null ql) ans)
	(t (listovars0 (cadr ql)) (listovars1 (cddr ql)))))

(defun dontfactor (y)
  (cond ((or (null $dontfactor) (equal $dontfactor '((mlist)))) nil)
	((memalike (pdis (make-poly y)) $dontfactor) t)))

(defun removealg (l)
  (loop for var in l
	 unless (algv var) collect var))

(defun degvecdisrep (degl)
  (do ((l degl (cdr l))
       (gv genvar (cdr gv))
       (ans 1))
      ((null l) ans)
    (and (> (car l) 0)
	 (setq ans (list (car gv) (car l) ans)))))

(defun ptermcont (p)
  (let ((tcont (degvecdisrep (pmindegvec p)))
	($algebraic))
    (list tcont (pquotient p tcont))))

(defun pmindegvec (p)
  (minlist (let ((*odr* (putodr (reverse genvar)))
		 (nn* (1+ (length genvar)))
		 (*min* t))
	     (degvector nil 1 p))))

(defun pdegreevector (p)
  (maxlist (let ((*odr* (putodr (reverse genvar)))
		 (nn* (1+ (length genvar)))
		 (*mx* t))
	     (degvector nil 1 p))))

(defun maxlist(l) (maxminl l t))

(defun minlist(l) (maxminl l nil))

(defun maxminl (l switch)
  (do ((l1 (copy-list (car l)))
       (ll (cdr l) (cdr ll)))
      ((null ll) l1)
    (do ((v1 l1 (cdr v1))
	 (v2 (car ll) (cdr v2)))
	((null v1))
      (cond (switch
	     (cond ((> (car v2) (car v1))
		    (rplaca v1 (car v2)))))
	    (t (cond ((< (car v2) (car v1))
		      (rplaca v1 (car v2)))))))))

(defun quick-sqfr-check (p var)
  (let ((gv (delete var (listovars p) :test #'equal))
	(modulus (or modulus *alpha))
	(l) (p0))
    (if $algebraic (setq gv (removealg gv)))
    (and gv
	 (not (pzerop (pcsubsty (setq l (rand (length gv) modulus))
				gv (pmod (p-lc p)))))
	 (not (pcoefp (setq p0 (pcsubsty l gv (pmod p)))))
	 (pcoefp (pgcd p0 (pderivative p0 (car p0))))
	 (list l gv p0))))

(defun monom->facl (p)
  (cond ((pcoefp p) (if (equal p 1) nil (list p 1)))
	(t (list* (pget (car p)) (cadr p) (monom->facl (caddr p))))))

(defun psqfr (p)
  (prog (r varl var mult factors)
     (cond ((pcoefp p) (return (cfactor p)))
	   ((pminusp p) (return (cons -1 (cons 1 (psqfr (pminus p)))))))
     (desetq (factors p) (ptermcont p))
     (setq factors (monom->facl factors))
     (cond ((pcoefp p) (go end)))
     (setq varl (sort (listovars p) 'pointergp))
     setvar
     (setq var (car varl) varl (cdr varl) mult 0)
     (cond ((pointergp var (car p)) (go nextvar))
	   ((dontfactor var)
	    (setq factors (cons p (cons 1 factors))
		  p 1)
	    (go end)))
     (cond ((quick-sqfr-check p var)	;QUICK SQFR CHECK BY SUBST.
	    (setq r (oldcontent p))
	    (setq p (car r) factors (cons (cadr r)
					  (cons 1 factors)))
	    (go nextvar)))
     (setq r (pderivative p var))
     (cond ((pzerop r) (go nextvar)))
     (cond ((and modulus (not (pcoefp r))) (pmonicize (cdr r))))
     (setq p (pgcdcofacts p r))
     (and algfac* (cadddr p) (setq adn* (ptimes adn* (cadddr p))))
     (setq r (cadr p)			; PRODUCT OF P[I]
	   p (car p))
     a (setq r (pgcdcofacts r p)
	     p (caddr r)
	     mult (1+ mult))
     (and algfac* (cadddr r) (setq adn* (ptimes adn* (cadddr r))))
     (cond ((not (pcoefp (cadr r)))
	    (setq factors
		  (cons (cadr r)
			(cons mult factors)))))
     (cond ((not (pcoefp (setq r (car r)))) (go a)))
     nextvar
     (cond ((pcoefp p) (go end))
	   (varl (go setvar))
	   (modulus (setq factors (append (fixmult (psqfr (pmodroot p))
						   modulus)
					  factors))
		    (setq p 1)))
     end  (setq p (cond ((equal 1 p) nil)
			(t (cfactor p))))
     (return (append p factors))))

(defun fixmult (l n)
  (do ((l l (cddr l)))
      ((null l))
    (rplaca (cdr l) (* n (cadr l))))
  l)

(defun pmodroot (p)
  (cond ((pcoefp p) p)
	((alg p) (pexpt p (expt modulus (1- (car (alg p))))))
	(t (cons (car p) (pmodroot1 (cdr p))))))

(defun pmodroot1 (x)
  (cond ((null x) x)
	(t (cons (truncate (car x) modulus)
		 (cons (pmodroot (cadr x))
		       (pmodroot1 (cddr x)))))))

(defmvar $savefactors nil "If t factors of ratreped forms will be saved")

(defvar checkfactors () "List of saved factors")

(defun savefactors (l)
  (when $savefactors
    (savefactor1 (car l))
    (savefactor1 (cdr l)))
  l)

(defun savefactor1 (p)
  (unless (or (pcoefp p)
	      (ptzerop (p-red p))
	      (member p checkfactors :test #'equal))
    (push p checkfactors)))

(defun heurtrial1 (poly facs)
  (prog (h j)
     (setq h (pdegreevector poly))
     (cond ((or (member 1 h :test #'equal) (member 2 h :test #'equal)) (return (list poly))))
     (cond ((null facs) (return (list poly))))
     (setq h (pgcd poly (car facs)))
     (return (cond ((pcoefp h) (heurtrial1 poly (cdr facs)))
		   ((pcoefp (setq j (pquotient poly h)))
		    (heurtrial1 poly (cdr facs)))
		   (t (heurtrial (list h j) (cdr facs)))))))

(defun heurtrial (x facs)
  (cond ((null x) nil)
	(t (nconc (heurtrial1 (car x) facs)
		  (heurtrial (cdr x) facs)))))


(defun pfactorquad (p)
  (prog (a b c d $dontfactor l v)
     (cond((or (onevarp p)(equal modulus 2))(return (list p))))
     (setq l (pdegreevector p))
     (cond ((not (member 2 l :test #'equal)) (return (list p))))
     (setq l (nreverse l) v (reverse genvar)) ;FIND MOST MAIN VAR
     loop (cond ((eqn (car l) 2) (setq v (car v)))
		(t (setq l (cdr l)) (setq v (cdr v)) (go loop)))
     (desetq (a . c) (bothprodcoef (make-poly v 2 1) p))
     (desetq (b . c) (bothprodcoef (make-poly v 1 1) c))
     (setq d (pgcd (pgcd a b) c))
     (cond ((pcoefp d) nil)
	   (t (setq *irreds (nconc *irreds (pfactor1 d)))
	      (return (pfactorquad (pquotient p d)))))
     (setq d (pplus (pexpt b 2) (ptimes -4 (ptimes a c))))
     (return
       (cond ((setq c (pnthrootp d 2))
	      (setq d (ratreduce (pplus b c) (ptimes 2 a)))
	      (setq d (pabs (pplus (ptimes (make-poly v) (cdr d))
				   (car d))))
	      (setq *irreds (nconc *irreds (list d (pquotient p d))))
	      nil)
	     (modulus (list p))    ;NEED TO TAKE SQRT(INT. MOD P) LCF.
	     (t (setq *irreds (nconc *irreds (list p)))nil)))))

(defmfun $isqrt (x) ($inrt x 2))

(defmfun $inrt (x n)
  (cond ((not (integerp (setq x (mratcheck x))))
	 (cond ((equal n 2) (list '($isqrt) x)) (t (list '($inrt) x n))))
	((zerop x) x)
	((not (integerp (setq n (mratcheck n)))) (list '($inrt) x n))
	(t (car (iroot (abs x) n)))))

(defun iroot (a n)   ; computes a^(1/n)  see Fitch, SIGSAM Bull Nov 74
  (cond ((< (integer-length a) n) (list 1 (1- a)))
	(t				;assumes integer a>0 n>=2
	 (do ((x (expt 2 (1+ (truncate (integer-length a) n)))
		 (- x (truncate (+ n1 bk) n)))
	      (n1 (1- n)) (xn) (bk))
	     (nil)
	   (cond ((signp le (setq bk (- x (truncate a (setq xn (expt x n1))))))
		  (return (list x (- a (* x xn))))))))))

(defmfun $nthroot (p n)
  (if (and (integerp n) (> n 0))
      (let ((k (pnthrootp (cadr ($rat p)) n)))
	(if k (pdis k) (merror (intl:gettext "nthroot: ~M is not a ~M power") p (format nil "~:r" n))))
    (merror (intl::gettext "nthroot: ~M is not a positive integer") n)))

(defun pnthrootp (p n)
  (let ((errrjfflag t))
    (catch 'raterr (pnthroot p n))))

(defun pnthroot (poly n)
  (cond ((equal n 1) poly)
	((pcoefp poly) (cnthroot poly n))
	(t (let* ((var (p-var poly))
		  (ans (make-poly var (cquotient (p-le poly) n)
				  (pnthroot (p-lc poly) n)))
		  (ae (p-terms (pquotient (pctimes n (leadterm poly)) ans))))
	     (do ((p (psimp var (p-red poly))
		     (pdifference poly (pexpt ans n))))
		 ((pzerop p) ans)
	       (cond ((or (pcoefp p) (not (eq (p-var p) var))
			  (> (car ae) (p-le p)))
		      (throw 'raterr nil)))
	       (setq ans (nconc ans (pquotient1 (cdr (leadterm p)) ae)))
	       )))))

(defun cnthroot(c n)
  (cond ((minusp c)
	 (cond ((oddp n) (- (cnthroot (- c) n)))
	       (t (throw 'raterr nil))))
	((zerop c) c)
	((zerop (cadr (setq c (iroot c n)))) (car c))
	(t (throw 'raterr nil))))


(defmfun pabs (x) (cond ((pminusp x) (pminus x)) (t x)))

(defun pfactorlin (p l)
  (do ((degl l (cdr degl))
       (v genvar (cdr v))
       (a)(b))
      ((null degl) nil)
    (cond ((and (= (car degl) 1)
		(not (algv (car v))))
	   (desetq (a . b) (bothprodcoef (make-poly (car v)) p))
	   (setq a (pgcd a b))
	   (return (cons (pquotientchk p a)
			 (cond ((equal a 1) nil)
			       (t (pfactor1 a)))))))))


(defun ffactor (l fn &aux (alpha alpha))
  ;;  (declare (special varlist))		;i suppose...
  (prog (q)
     (cond ((and (null $factorflag) (mnump l)) (return l))
	   ((or (atom l) algfac* modulus) nil)
	   ((and (not gauss)(member 'irreducible (cdar l) :test #'eq))(return l))
	   ((and gauss (member 'irreducibleg (cdar l) :test #'eq)) (return l))
	   ((and (not gauss)(member 'factored (cdar l) :test #'eq))(return l))
	   ((and gauss (member 'gfactored (cdar l) :test #'eq)) (return l)))
     (newvar l)
     (if algfac* (setq varlist (cons alpha (remove alpha varlist :test #'equal))))
     (setq q (ratrep* l))
     (when algfac*
       (setq alpha (cadr (ratrep* alpha)))
       (setq minpoly* (subst (car (last genvar))
			     (car minpoly*)
			     minpoly*)))
     (mapc #'(lambda (y z) (putprop y z (quote disrep)))
	   genvar
	   varlist)
     (return (retfactor (cdr q) fn l))))

(defun factorout1 (l p)
  (do ((gv genvar (cdr gv))
       (dl l (cdr dl))
       (ans))
      ((null dl) (list ans p))
    (cond ((zerop (car dl)))
	  (t (setq ans (cons (pget (car gv)) (cons (car dl) ans))
		   p (pquotient p (list (car gv) (car dl) 1)))))))

(defun factorout (p)
  (cond ((and (pcoefp (pterm (cdr p) 0))
	      (not (zerop (pterm (cdr p) 0))))
	 (list nil p))
	(t (factorout1 (pmindegvec p) p))))

(defmfun pfactor (p &aux ($algebraic algfac*))
  (cond ((pcoefp p) (cfactor p))
	($ratfac (pfacprod p))
	(t (setq p (factorout p))
	   (cond ((equal (cadr p) 1) (car p))
		 ((numberp (cadr p)) (append (cfactor (cadr p)) (car p)))
		 (t (let ((cont (cond (modulus (list (leadalgcoef (cadr p)) (monize (cadr p))))
				      (algfac* (algcontent (cadr p)))
				      (t (pcontent (cadr p))))))
		      (nconc
		       (cond ((equal (car cont) 1) nil)
			     (algfac*
			      (cond (modulus (list (car cont) 1))
				    ((equal (car cont) '(1 . 1)) nil)
				    ((equal (cdar cont) 1) (list (caar cont) 1))
				    (t (list (caar cont) 1 (cdar cont) -1))))
			     (t (cfactor (car cont))))
		       (pfactor11 (psqfr (cadr cont)))
		       (car p))))))))

(defun pfactor11 (p)
  (cond ((null p) nil)
	((numberp (car p))
	 (cons (car p) (cons (cadr p) (pfactor11 (cddr p)))))
	(t (let* ((adn* 1)
		  (f (pfactor1 (car p))))
	     (nconc (if (equal adn* 1) nil
			(list adn* (- (cadr p))))
		    (do ((l f (cdr l))
			 (ans nil (cons (car l) (cons (cadr p) ans))))
			((null l) ans))
		    (pfactor11 (cddr p)))))))

(defun pfactor1 (p)			;ASSUMES P SQFR
  (prog (factors *irreds *checkagain)
     (cond ((dontfactor (car p)) (return (list p)))
	   ((onevarp p)
	    (cond ((setq factors (factxn+-1 p))
		   (if (and (not modulus)
			    (or gauss (not algfac*)))
		       (setq *irreds factors
			     factors nil))
		   (go out))
		  ((and (not algfac*) (not modulus)
			(not (equal (cadr p) 2)) (estcheck (cdr p)))
		   (return (list p))))))
     (and (setq factors (pfactorlin p (pdegreevector p)))
	  (return factors))
     (setq factors(if (or algfac* modulus) (list p) ;SQRT(NUM. CONT OF DISC)
		      (pfactorquad p)))
     (cond ((null factors)(go out)))
     (when checkfactors
       (setq factors (heurtrial factors checkfactors))
       (setq *checkagain (cdr factors)))
     out (return (nconc *irreds (mapcan (function pfactorany) factors)))))

(defmvar $homog_hack nil)  ; If T tries to eliminate homogeneous vars.

(declare-top (special *hvar *hmat))

(defun pfactorany (p)
  (cond (*checkagain (let (checkfactors) (pfactor1 p)))
	((and $homog_hack (not algfac*) (not (onevarp p)))
	 (let ($homog_hack *hvar *hmat)
	   (mapcar #'hexpand (pfactor (hreduce p)))))
	($berlefact (factor1972 p))
	(t (pkroneck p))))


(defun retfactor (x fn l &aux (a (ratfact x fn)))
  (prog ()
   b    (cond ((null (cddr a))
	       (setq a (retfactor1 (car a) (cadr a)))
	       (return (cond ((and scanmapp (not (atom a)) (not (atom l))
				   (eq (caar a) (caar l)))
			      (tagirr l))
			     (t a))))
	      ((equal (car a) 1) (setq a (cddr a)) (go b))
	      (t (setq a (map2c #'retfactor1 a))
		 (return (cond ((member 0 a :test #'eq) 0)
			       (t (setq a (let (($expop 0) ($expon 0)
						$negdistrib)
					    (muln (sortgreat a) t)))
				  (cond ((not (mtimesp a)) a)
					(t (cons '(mtimes simp factored)
						 (cdr a)))))))))))

;;; FOR LISTS OF ARBITRARY EXPRESSIONS
(defun retfactor1 (p e)
  (power (tagirr (simplify (pdisrep p))) e))

(defun tagirr (x)
  (cond ((or (atom x) (member 'irreducible (cdar x) :test #'eq)) x)
	(t (cons (append (car x) '(irreducible)) (cdr x)))))

(defun revsign (x)
  (cond ((null x) nil)
	(t (cons (car x)
		 (cons (- (cadr x)) (revsign (cddr x)))))))

;;	THIS IS THE END OF THE NEW RATIONAL FUNCTION PACKAGE PART 4
