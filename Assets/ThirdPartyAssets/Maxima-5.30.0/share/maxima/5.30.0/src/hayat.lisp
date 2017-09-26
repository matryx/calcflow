;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   **************************************************************
;;;   ***** HAYAT ******* Finite Power Series Routines *************
;;;   **************************************************************
;;;   ** (c) Copyright 1982 Massachusetts Institute of Technology **
;;;   ****** This is a read-only file! (All writes reserved) *******
;;;   **************************************************************

(in-package :maxima)

;;;		TOP LEVEL STRUCTURE

;;;	Power series have the following format when seen outside the power
;;; series package:
;;;
;;;    ((MRAT SIMP <varlist> <genvar> <tlist> trunc) <poly-form>)
;;;
;;; This is the form of the output of the expressions, to
;;; be displayed they are RATDISREPed and passed to DISPLA.

;;; The <poly-forms> consist of a header and list of exponent-coefficient
;;; pairs as shown below.  The PS is used to distinguish power series
;;; from their coefficients which have a similar representation.
;;;
;;;   (PS (<var> . <ord-num>) (<trunc-lvl>)
;;;	  (<exponent> . <coeff>) (<exponent> . <coeff>) . . .)
;;;
;;; The <var> component of the power series is a gensym which represents the
;;; kernel of the power series.  If the package is called with the arguments:
;;; Taylor(<expr>, x, a, n)  then the kernel will be (x - a).
;;; The <ord-num> is a relative ordering for the various kernels in a
;;; multivariate expansion.
;;; <trunc-lvl> is the highest degree of the variable <var> which is retained
;;; in the current power series.
;;; The terms in the list of exponent-coefficient pairs are ordered by
;;; increasing degree.

;;; Problem: fix expansion of logs so that taylor(log(1+exp(-1/x)),x,0,3)
;;; works. Done.
;;;
;;; Problem: taylor(log(1+exp(-1/x)),x,0,5) loses because, while
;;; taylor_simplify_recurse'ing exp(-3/x) get trunc level = -3. FIxed.
;;;
;;; Problem: Need to fix things so that asymptotic kernels aren't put onto
;;; tvars via tlist merge etc. in taylor1. Done.
;;;
;;; Problem: get-series returns 0 for taylor(log(1+exp(-1/x)),x,0,5) and
;;; need to make log(exp(1/x)) -> 1/x. Fixed.
;;;
;;; Problem: Fix psexpt-fn so that it doesn't lose via the invert-var
;;; scheme, e.g. try taylor(exp(exp(-1/x)+x),x,0,5). Note that if it did
;;; just that scheme. Done.
;;;
;;; Problem: fix adjoin-tvar so that the new tvars are ordered correctly
;;; according to their strength. This is necessary in order to read the limit
;;; directly from the leading term. E.g. see the misordered:
;;; taylor(subst(1/x,x,part(screw2,1)),x,0,2) from ALJABR;SCREW2 LIMIT.
;;; Note that the answer given for this appear to be incorrect when the
;;; truncation on x is < 4. Is this due to the misordering?
;;; Also taylor(screwa,x,0,4)+taylor(screwb,x,0,8) doesn't agree with
;;; taylor(screw,x,0,8) where it should (here screwa = part(screw,1),
;;; screwb = part(screw, 2); is this a truncation problem on the
;;; gvar exp(1/x)?).
;;;
;;; Problem: new gvars have to be intro'd for logs just as for exp's instead
;;; of treating them like constants as currently done. For example,
;;; taylor(log(1+1/log(x)),x,0,2) currently doesn't expand. Done.
;;;
;;; Problem: The display routines need pieces of the taylor environment
;;; (tvar-limits, tvars, tlist, etc.) to figure out how to order terms.
;;; This means we'll probably have to store some of this on the local tlist.
;;; When this is done the commented out code in psdisrep and psdisrep2 can
;;; be re-inserted. Psdisrep2expand will also need to be modified.
;;; I just fixed srdisrep to get the local env it needs; psdisrep2expand
;;; still needs to be updated. The display order problem is still around
;;; however: try taylor(exp(exp(-1/x)+x),x,0,3). After more investigation,
;;; it seems that the term reversal always occurs for ps's that are coeff's
;;; of terms whose expt is < 0. Probably the psdisrep routines should reverse
;;; these terms to account for this (the bug is somewhere in the DISPLA
;;; routines, possible DIM-MPLUS).
;;;
;;; Problem: Since gvar's like exp(-1/x) can't be put on genvar, they have
;;; to be saved somewhere locally and restored by everyone who needs to setup
;;; disrep info etc. Done for re-taylor.
;;;
;;; Problem: All new code needs to be checked to ensure it does the correct
;;; thing when the expansion point is infinite (e.g. see the code in
;;; TSEXPT-RED which handles this case).
;;;
;;; Perhaps the code for exp's and log's which pushes trunc degrees
;;; can be done by first computing exp(c0) or log(c0) first and see
;;; how much to push by looking at this series. Done for exp in tsexpt-red.
;;;
;;; Problems: taylor(part(screwa,2)-2/x,x,0,1) shouldn't be exact.
;;; taylor(screwa,x,0,-2) misses the degree -2 term. This part is now fixed.
;;;
;;; Tvar-limits should be stored locally so that psdisrep need not recompute
;;; each gvar limit when disrepping.

(macsyma-module hayat)

(defmvar tlist nil)

(defvar *within-srf?* nil)

(load-macsyma-macros mhayat rzmac ratmac)

;;;		 Subtitle Special Stuff for Compiling

(declare-top
 (special vlist
	  varlist		;List of all the variables occuring in a power
				;series, the power series variables at the end
	  genvar		;The list of gensyms corresponding to varlist
	  modulus		;
	  *a*			;Temporary special
	  sub-exprs		;
	  silent-taylor-flag	;If true indicates that errors will be
				;returned via a throw to TAY-ERR
	  tlist			;An association list which contains the
				;relevant information for the expansion which
				;is passed in at toplevel invocation.
	  $float		;Indicates whether to convert rational numbers
				;to floating point numbers.
	  $keepfloat		;When true retains floatin point numbers
				;internal to Taylor.
	  $radexpand		;
	  log-1			;What log(-1) should be log(-1) or pi*i.
	  log%i			;Similarly for log(i)
	  exact-poly		;Inicates whether polynomials are to be
				;considered exact or not.  True within SRF,
				;false within TAYLOR.
	  ngps			;
	  num-syms		;
	  loc-gensym		;
	  syms			;
	  tvars			;
	  pssyms		;
	  half%pi		;Has pi/2 to save space.
	  const-funs		;
	  const-exp-funs	;
	  tay-const-expand	;For rediculousness like csch(log(x))
	  $exponentialize	;which we do by exponentiation.
	  tay-pole-expand	;
	  trigdisp		;
	  last-exp		;last-expression through taylor2
	  $taylordepth		;
	  $ratexpand		;
	  genpairs		;List of dotted pairs
	  ps-bmt-disrep		;
	  ivars			;Pairlist if gensym and disreped version
	  key-vars		;Pairlist of gensym and key var (for searching
				;TLIST)
	  $algebraic		;
	  *psacirc		;
	  *pscirc		;
	  full-log		;
	  $logarc		;
	  trunclist		;
	  *within-srf?*		;flag for in srf
	  mainvar-datum		;
	  least_term?		; If non-null then the addition routines
				; are adding or subtracting coeff's of the
				; least term of a sum so they should do
				; zero checking on it if it is desired.
	  taylor_simplifier	; This is set by taylor1 to the object
				; which will be funcalled whenever
				; coefficient simplification is desired.
	  zerolist		; A list of constant expressions which have
				; been verified to be zero by a call to
				; $TAYLOR_SIMPLIFIER in taylor2. It is used to
				; suppress the message that TAYLOR is assumming
				; an expression to be zero.
	; 0p-funord lexp-non0	; referenced only in commented-out code, so comment out here too
	$zerobern $simp)
 )				;Don't want to see closed compilation notes.

(defmvar $psexpand ()
 "When TRUE extended rational function expressions will be displayed fully
  expanded. (RATEXPAND will also cause this.) If FALSE, multivariate
  expressions will be displayed just as in the rational function package.
  If PSEXPAND:MULTI, then terms with the same total degree in the variables
  are grouped together.")

(defmvar $maxtayorder t
 "When true TAYLOR retains as many terms as are certain to be correct
  during power series arithmetic. Otherwise, truncation is controlled
  by the arguments specified to TAYLOR.")

(defmvar $taylor_truncate_polynomials t
 "When FALSE polynomials input to TAYLOR are considered to have infinite
  precison; otherwise (the default) they are truncated based upon the input
  truncation levels.")

(defmvar $taylor_logexpand t
 "Unless FALSE log's of products will be expanded fully in TAYLOR (the default)
  to avoid identically-zero constant terms which involve log's. When FALSE,
  only expansions necessary to produce a formal series will be executed.")

;Note!  The value of this must be a symbol, because it is checked with
; FBOUNDP.
(defmvar $taylor_simplifier 'simplify
 "A function of one argument which TAYLOR uses to simplify coefficients
  of power series.")

(defvar taylor_simplifier nil)

;;;		 Subtitle General Macsyma Free Predicates

(defun zfree (e x)
    (cond ((equal e x) () )
	  ((atom e) 't)
	  ((eq (caar e) 'mrat)
	   (null (member x (cdr ($listofvars e)) :test #'equal)))
	  ('t (do ((l (cdr e) (cdr l))) ((null l) 't)
		 (or (zfree (car l) x) (return () ))))))

(defun mfree (exp varl)
  (declare (special dummy-variable-operators))
   (cond ((atom exp) (not (member exp varl :test #'eq)))
	 ((eq (caar exp) 'mrat)
	  (do ((l (mrat-varlist exp) (cdr l)))
	      ((null l) 't)
	     (unless (mfree (car l) varl) (return () ))))
	 ((or (member (caar exp) dummy-variable-operators :test #'eq)
	      (member 'array (cdar exp) :test #'eq))
	  (do ((vars varl (cdr vars)))
	      ((null vars) 't)
	     (unless (freeof (car vars) exp) (return () ))))
	 ('t (and (mfree (caar exp) varl) (mfreel (cdr exp) varl)))))

(defun mfreel (l varl)
  (or (null l) (and (mfree (car l) varl) (mfreel (cdr l) varl))))

;;; Subtitle Coefficient Arithmetic

(defun rcexpt (x y)
       (cond ((equal x (rcone)) (rcone))
	     ((rczerop y) (rcone))
	     ((and (equal (cdr y) 1) (fixnump (car y)))
	      (ratexpt x (car y)))
	     ((and $radexpand (numberp (car y)) (numberp (cdr y)))
	      (if (floatp (car y))
		  (setq y (maxima-rationalize (quot (car y) (cdr y)))))
	      (ratexpt (rcquo (rcexpt1 (car x) (cdr y))
			      (rcexpt1 (cdr x) (cdr y)))
		       (car y)))
	     (t (let ($keepfloat)
		     (prep1 (m^ (rcdisrep x) (rcdisrep y)))))))

(defun rcexpt1 (p n)
   (cond ((equal p 1) (rcone))
	 ((pcoefp p) (prep1 (m^ (pdis p) (*red 1 n))))
	 ;; psfr does a square-free decom on p yielding (p1 e1 p2 e2 ... pn en)
	 ;; where p = p1^e1 p2^e2 ... pn^en, the pi being square-free
	 (t (do ((l (psqfr p) (cddr l))
		 (ans (rcone)))
		((null l) ans)
	       (if (not (equal (rem (cadr l) n) 0))
		   (setq ans (rctimes ans (prep1 (m^ (pdis (car l))
						     (*red (cadr l) n)))))
		  ;; If pi<0, n=2m and n|ei then ei=2e and
		  ;;	    (pi^ei)^(1/(2m)) = (-pi)^(e/m)
		   (progn
		     (when (and (evenp n) (eq ($sign (pdis (car l))) '$neg))
		       (rplaca l (pminus (car l))))
		     (setq ans (rctimes ans (ratexpt (cons (car l) 1)
						   (truncate (cadr l) n))))))))))

(defun rccoefp (e)		;a sure check, but expensive
       (and (null (atom e))
	    (or (atom (car e))
		(member (caar e) genvar :test #'eq))
	    (or (atom (cdr e))
		(member (cadr e) genvar :test #'eq))))

;;;		 Subtitle Exponent arithmetic

(defun ezerop (x)
  (and (not (infp x)) (signp e (car x))))

(defun e+ (x y)
    (cond ((or (infp x) (infp y)) (inf))
	  ((and (equal (cdr x) 1) (equal (cdr y) 1))
	   (cons (+ (car x) (car y)) 1))
	  (t (ereduce (+ (* (car x) (cdr y)) (* (cdr x) (car y)))
		      (* (cdr x) (cdr y))))))

(defun ediff (x y)
    (cond ((infp x) (inf))
	  ((and (equal (cdr x) 1) (equal (cdr y) 1))
	   (cons (- (car x) (car y)) 1))
	  (t (ereduce (- (* (car x) (cdr y)) (* (cdr x) (car y)))
		      (* (cdr x) (cdr y))))))

(defun emin (x y)
    (cond ((infp x) y)
	  ((infp y) x)
	  ((equal (cdr x) (cdr y)) (cons (min (car x) (car y)) (cdr x)))
	  ((< (* (car x) (cdr y)) (* (cdr x) (car y))) x)
	  (t y)))

(defun emax (x y)
    (cond ((or (infp x) (infp y)) (inf))
	  ((equal (cdr x) (cdr y)) (cons (max (car x) (car y)) (cdr x)))
	  ((> (* (car x) (cdr y)) (* (cdr x) (car y))) x)
	  (t y)))

(defun e* (x y)
    (cond ((or (infp x) (infp y)) (inf))
	  ((and (equal (cdr x) 1) (equal (cdr y) 1))
	   (cons (* (car x) (car y)) 1))
	  (t (ereduce (* (car x) (car y)) (* (cdr x) (cdr y))))))

(defun erecip (e)
       (if (minusp (car e))
	   (cons (- (cdr e)) (- (car e)))
	   (cons (cdr e) (car e))))

(defun equo (x y)
       (cond ((infp x) (inf))
	     ((infp y) (rczero))
	     (t (ereduce (* (car x) (cdr y))
			 (* (cdr x) (car y))))))

(defun e1+ (x)
    (cond ((infp x) (inf))
	  ((= (cdr x) 1) (cons (1+ (car x)) 1))
	  (t (cons (+ (cdr x) (car x)) (cdr x)))))

(defun e1- (x)
    (cond ((infp x) (inf))
	  ((equal (cdr x) 1) (cons (1- (car x)) 1))
	  (t (cons (- (car x) (cdr x)) (cdr x)))))

(defun e> (x y)
    (cond ((infp x) t)
	  ((infp y) ())
	  ((equal (cdr x) (cdr y)) (> (car x) (car y)))
	  (t (> (* (car x) (cdr y)) (* (car y) (cdr x))))))

(defun e= (e1 e2)
	  (cond ((eq e1 e2) t)
		((or (null e1) (null e2)) ())
		(t (and (equal (car e1) (car e2))
			(equal (cdr e1) (cdr e2))))))

(defun ereduce (n d)
       (if (signp l d) (setq d (- d) n (- n)))
       (if (zerop n) (rczero)
	   (let ((gcd (gcd n d)))
		(cons (/ n gcd) (/ d gcd)))))

(defun egcd (x y)
       (let ((xn (abs (car x))) (xd (cdr x))
	     (yn (abs (car y))) (yd (cdr y)))
	    (cons (gcd xn yn) (* xd (/ yd (gcd xd yd))))))

;;;		 Subtitle polynomial arithmetic

(declare-top (special vars))

(defun ord-vector (p)
  (let ((vars (mapcar #'(lambda (datum) (list (int-gvar datum))) tlist)))
    (declare (special vars))
    (cond ((not (cdr vars)) (ncons (ps-le* p)))
	  (t (ord-vect1 p) (mapcar #'(lambda (x) (or (cdr x) (rczero))) vars)))))

(defun ord-vect1 (p)
  (declare (special vars))
  (unless (pscoefp p)
     (let ((data (assoc (gvar p) vars :test #'eq))
	   (le (ps-le p)))
	(rplacd data (cond ((not (cdr data)) le)
			   (t (emin (cdr data) le))))
	(mapl #'(lambda (l) (ord-vect1 (lc l))) (terms p)))))

(defun trunc-vector (p min?)
   (let ((vars (mapcar #'(lambda (datum) (list (int-gvar datum))) tlist)))
     (declare (special vars))
      (if (null (cdr vars)) (ncons (if (psp p) (trunc-lvl p) () ))
	 (progn
	   (trunc-vect1 p min?)
	   (mapcar 'cdr vars)))))

(defun trunc-vect1 (p min?)
  (declare (special vars))
   (unless (pscoefp p)
      (let ((data (assoc (gvar p) vars :test #'eq))
	    (trunc (trunc-lvl p)))
	 (when trunc
	    (rplacd data (if (null (cdr data)) trunc
			   (if min? (emin (cdr data) trunc)
			      (emax (cdr data) trunc))))))
      (dolist (term (terms p))
	(trunc-vect1 (c term) min?))))

(declare-top (unspecial vars))

(defun psplus (x y)
   (cond ((pscoefp x)
	  (cond ((pscoefp y) (rcplus x y))
		((rczerop x) y)
		(t (pscplus x y))))
	 ((pscoefp y) (if (rczerop y) x (pscplus y x)))
	 ((eqgvar (gvar-o x) (gvar-o y)) (psplus1 x y))
	 ((pointerp (gvar-o x) (gvar-o y)) (pscplus y x))
	 (t (pscplus x y))))

(defun rcplus! (x y)
   (if (not (and least_term? taylor_simplifier)) (rcplus x y)
      (prep1 (funcall taylor_simplifier (m+ (rcdisrep x) (rcdisrep y))))))

(defun psdiff (x y)
   (cond ((pscoefp x) (cond ((pscoefp y) (rcdiff x y))
			    ((rczerop x) (pstimes (rcmone) y))
			    (t (pscdiff x y () ))))
	 ((pscoefp y) (if (rczerop y) x (pscdiff y x t)))
	 ((eqgvar (gvar-o x) (gvar-o y)) (psdiff1 x y))
	 ((pointerp (gvar-o x) (gvar-o y)) (pscdiff y x t))
	 (t (pscdiff x y () ))))

(defun rcdiff! (x y)
   (if (not (and least_term? taylor_simplifier)) (rcdiff x y)
      (prep1 (funcall taylor_simplifier (m- (rcdisrep x) (rcdisrep y))))))

(defun psplus1 (x y)
   (let ((ans (cons () () )))
      (psplus2 (gvar-o x) (emin (trunc-lvl x) (trunc-lvl y))
	       (cons 0 (terms x)) (cons 0 (terms y)) ans ans)))

(defun pscplus (c p)
   (if (e> (rczero) (trunc-lvl p)) p
      (pscheck (gvar-o p) (poly-data p) (pscplus1 c (terms p)))))

(defun pscdiff (c p fl)
   (if (e> (rczero) (trunc-lvl p))
       (if fl p (psminus p))
       (pscheck (gvar-o p) (poly-data p)
		(cond ((not fl) (pscplus1 c (psminus-terms (terms p))))
		      (t (pscplus1 (psminus c) (terms p)))))))

(defun strip-zeroes (terms ps?)
   (cond ((or (null terms) (null taylor_simplifier)) terms)
	 ((null ps?)
	  (do ((terms terms (n-term terms)))
	      ((null terms) () )
	     (change-lc terms (strip-zeroes (lc terms) 't))
	     (unless (rczerop (lc terms)) (return terms))))
	 ((pscoefp terms)
	  (if (null taylor_simplifier) terms
	     (let ((exp (rcdisrep terms)))
		;; If a pscoeff is not free of tvars then the ps is a
		;; multivar series and we can't handle a recursive
		;; call to taylor (as opposed to a call to prep1, as below)
		;; because this would be circuler (e.g. try
		;; taylor(x/ (x^2+1),[x],%i,-1) ). Besides, in this case
		;; the pscoeff contains a tvar hence should not be 0.
		(if (not (mfree exp tvars)) terms
		   (prep1 (funcall taylor_simplifier exp))))))
	 (t (pscheck (gvar-o terms) (poly-data terms)
		     (strip-zeroes (terms terms) () )))))

(defun pscplus1 (c l)
   (cond ((null l) (list (term (rczero) c)))
	 ((rczerop (le l)) (setq c (psplus c (lc l)))
	  (if (rczerop c) (strip-zeroes (n-term l) () )
	     (cons (term (rczero) c) (n-term l))))
	 ((e> (le l) (rczero)) (cons (term (rczero) c) l))
	 (t (cons (lt l) (let ((least_term?)) (pscplus1 c (n-term l)))))))

;;; Both here and in psdiff2 xx and yy point one before where one
;;; might think they should point so that extensions will be retained.

(defun psplus2 (varh trunc xx yy ans a)
  (prog (c)
   a	(cond ((mono-term? xx)
	       (if (mono-term? yy) (go end) (go null)))
	      ((mono-term? yy) (setq yy xx) (go null)))
	(cond ((equal (le (n-term xx)) (le (n-term yy)))
	       (setq xx (n-term xx) yy (n-term yy))
	       (setq c (let ((least_term? (null (n-term ans))))
			  (psplus (lc xx) (lc yy))))
	       (if (rczerop c) (go a) (add-term a (le xx) c)))
	      ((e> (le (n-term xx)) (le (n-term yy)))
	       (setq yy (n-term yy))
	       (add-term a (lt yy)))
	      (t (setq xx (n-term xx))
		 (add-term a (lt xx))))
	(setq a (n-term a))
	(go a)
   null (if (or (mono-term? yy) (e> (le (n-term yy)) trunc))
	    (go end)
	    (progn
	      (setq yy (n-term yy))
	      (add-term-&-pop a (lt yy))
	      (go null)))
   end  (return (pscheck varh (list trunc) (cdr ans)))))

(defun psdiff1 (x y)
   (let ((ans (cons () () )))
      (psdiff2 (gvar-o x) (emin (trunc-lvl x) (trunc-lvl y))
	       (cons 0 (terms x)) (cons 0 (terms y)) ans ans)))

(defun psdiff2 (varh trunc xx yy ans a)
  (prog (c)
   a	(cond ((mono-term? xx)
	       (if (mono-term? yy)
		   (go end)
		   (progn
		     (setq yy
			   (cons 0 (mapcar #'(lambda (q)
					       (term (e q) (psminus (c q))))
					   (cdr yy))))
		     (go null))))
	      ((mono-term? yy)
	       (setq yy xx) (go null)))
	(cond ((equal (le (n-term xx)) (le (n-term yy)))
	       (setq xx (n-term xx) yy (n-term yy))
	       (setq c (let ((least_term? (null (n-term ans))))
			  (psdiff (lc xx) (lc yy))))
	       (if (rczerop c) (go a)
		   (add-term a (le xx) c)))
	      ((e> (le (n-term xx)) (le (n-term yy)))
	       (setq yy (n-term yy))
	       (add-term a (le yy) (psminus (lc yy))))
	      (t (setq xx (n-term xx))
		 (add-term a (lt xx))))
	(setq a (n-term a))
	(go a)
   null (if (or (mono-term? yy) (e> (le (n-term yy)) trunc))
	    (go end)
	    (progn
	      (setq yy (n-term yy))
	      (add-term-&-pop a (le yy) (lc yy))
	    (go null)))
   end	(return (pscheck varh (list trunc) (cdr ans)))))

(defun psminus (x)
   (if (psp x) (make-ps x (psminus-terms (terms x)))
      (rcminus x)))

(defun psminus-terms (terms)
   (let ((ans (cons () () )))
      (do ((q terms (n-term q))
	   (a ans (cdr a)))
	  ((null q) (cdr ans))
	 (add-term a (le q) (psminus (lc q))))))

(defun pscheck (a b terms)
   (cond ((null terms) (rczero))
	 ((and (mono-term? terms) (rczerop (le terms)))
	  (lc terms))
	 (t (make-ps a b terms))))

(defun pstrim-terms (terms e)
   (do () (())
      (cond ((null terms) (return () ))
	    ((null (e> e (le terms))) (return terms))
	    (t (setq terms (n-term terms))))))

(defun psterm (terms e)
   (psterm1 (pstrim-terms terms e) e))

(defun psterm1 (l e)
   (cond ((null l) (rczero))
	 ((e= (le l) e) (lc l))
	 (t (rczero))))

(defun pscoeff1 (a b c)		;a is an mrat!!!
   (let ((tlist (mrat-tlist a)))
      (cons (nconc (list 'mrat 'simp (mrat-varlist a) (mrat-genvar a))
		   (do ((l (mrat-tlist a) (cdr l))
			(ans () (cons (car l) ans)))
		       ((null l) ans)
		      (when (alike1 (caar l) b)
			 (return
			  (and (or ans (cdr l))
			       (list (nreconc ans (cdr l)) 'trunc))))))
	    (pscoef (mrat-ps a) (int-gvar (get-datum b)) (prep1 c)))))

(defun pscoef (a b c)
   (cond ((pscoefp a) (if (rczerop c) a (rczero)))
	 ((eq b (gvar a)) (psterm (terms a) c))
	 (t (do ((gvar-o (gvar-o a))
		 (poly-data (poly-data a))
		 (ans (rczero))
		 (terms (terms a) (n-term terms))
		 (temp))
		((null terms) ans)
	       (unless (rczerop (setq temp (pscoef (lc terms) b c)))
		  (setq ans (psplus ans
				    (make-ps gvar-o poly-data
					     (ncons (term (le terms)
							  temp))))))))))

(defun psdisextend (p)
  (cond ((not (psp p)) p)
	(t (make-ps p (mapcar #'(lambda (q) (cons (car q) (psdisextend (cdr q))))
			      (terms p))))))

(defun psfloat (p)
   (if (psp p) (psfloat1 p (trunc-lvl p) (terms p) (ncons 0))
      (rctimes (rcfone) p)))

(defun psfloat1 (p trunc l ans)
   (do (($float 't)
	(a (last ans) (n-term a)))
       ((or (null l) (e> (le l) trunc))
	(pscheck (gvar-o p) (poly-data p) (cdr ans)))
      (add-term a (le l) (psfloat (lc l)))
      (setq l (n-term l))))

(defun pstrunc (p)
  (pstrunc1 p (mapcar #'(lambda (q) (cons (int-gvar q) (current-trunc q)))
		      tlist)))

(defun pstrunc1 (p trlist)
  (cond ((not (psp p))
	 p)
	(t
	 (let ((trnc (cdr (assoc (gvar p) trlist :test #'eq))) (trunc-ps) (a nil))
	   (do ((l (terms p) (n-term l)))
	       ((null l) (pscheck (gvar-o p) (ncons (trunc-lvl p)) (nreverse a)))
	     (when (e> (le l) trnc)
	       (return (pscheck (gvar-o p) (ncons trnc) (nreverse a))))
	     (unless (rczerop (setq trunc-ps (pstrunc1 (lc l) trlist)))
	       (push (term (le l) trunc-ps) a)))))))

(defun pstimes (x y)
   (cond ((or (rczerop x) (rczerop y)) (rczero))
	 ((pscoefp x) (cond ((pscoefp y) (rctimes x y))
			    ((equal x (rcone)) y)
			    (t (psctimes* x y))))
	 ((pscoefp y) (if (equal y (rcone)) x (psctimes* y x)))
	 ((eqgvar (gvar-o x) (gvar-o y)) (pstimes*1 x y))
	 ((pointerp (gvar-o x) (gvar-o y)) (psctimes* y x))
	 (t (psctimes* x y))))

(defun psctimes* (c p)
  (make-ps p (maplist #'(lambda (l)
			   (term (le l) (pstimes c (lc l))))
		      (terms p))))

(defun pstimes*1 (xa ya)
   (let ((ans (cons () () ))
	 (trunc (let ((lex (ps-le xa)) (ley (ps-le ya)))
		   (e+ (emin (e- (trunc-lvl xa) lex) (e- (trunc-lvl ya) ley))
		       (e+ lex ley)))))
      (unless $maxtayorder
	 (setq trunc (emin trunc (t-o-var (gvar xa)))))
      (pstimes*2 xa ya trunc ans)))

(defun pstimes*2 (xa ya trunc ans)
   (prog (a c e x y yy)
	 (setq x (terms xa) y (setq yy (terms ya)) a ans)
    a	 (cond ((or (null y) (e> (setq e (e+ (le x) (le y))) trunc))
		(go b))
	       ((not (rczerop (setq c (pstimes (lc x) (lc y)))))
		(add-term-&-pop a e c)))
	 (setq y (n-term y))
	 (go a)
    b	 (unless (setq x (n-term x))
	    (return (pscheck (gvar-o xa) (list trunc) (cdr ans))))
	 (setq y yy a ans)
    c	 (when (or (null y) (e> (setq e (e+ (le x) (le y))) trunc))
	    (go b))
	 (setq c (pstimes (lc x) (lc y)))
    d	 (cond ((or (mono-term? a) (e> (le (n-term a)) e))
		(add-term-&-pop a e c))
	       ((e> e (le (n-term a)))
		(setq a (n-term a))
		(go d))
	       (t (setq c (psplus c (lc (n-term a))))
		  (if (rczerop c)
		      (rplacd a (n-term (n-term a)))
		      (progn
			(change-lc (n-term a) c)
			(setq a (n-term a))))))
	 (setq y (n-term y))
	 (go c)))

(defun pscsubst (c v p)
  (cond ((pscoefp p) p)
	((eq v (gvar p)) (pscsubst1 c p))
	((pointerp v (gvar p)) p)
	(t (make-ps p (maplist
		       #'(lambda (q) (term (le q)
					   (pscsubst c v (lc q))))
		       (terms p))))))

(defun pscsubst1 (v u)
   (do ((a (rczero))
	(ul (terms u) (n-term ul)))
       ((null ul) a)
      (setq a (psplus a (pstimes (lc ul) (psexpt v (le ul)))))))

(defun get-series (func trunc var e c)
   (let ((pw (e// trunc e)))
      (setq e (if (and (equal e (rcone)) (equal c (rcone)))
		  (getexp-fun func var pw)
		 (psmonsubst (getexp-fun func var pw) trunc e c)))
      (if (and $float $keepfloat) (psfloat e) e)))

(defun psmonsubst (p trunc e c)
  (if (psp p)
      (psmonsubst1 p trunc e c
		   `(() . ,(terms p)) (ncons () ) (rcone) (rczero))
    p))


(defun psmonsubst1 (p trunc e c l ans cc el)
   ;; We set $MAXTAYORDER to () here so that the calls to psexpt below
   ;; won't do needless extra work, e.g. see rwg's complaint of 9/7/82.
   (prog (a ee varh $maxtayorder)
	 (setq a ans varh (gvar-o p))
    a    (cond ((or (mono-term? l)
		    (e> (setq ee (e* e (le (n-term l)))) trunc))
		(go end))
	       ((rczerop (setq cc
			       (pstimes cc
					(psexpt c (e- (le (setq l (n-term l)))
						      el))))))
	       ((mono-term? a)
		(add-term a ee (pstimes cc (lc l)))))
	 (setq a (n-term a) el (le l))
	 (go a)
    end  (return (pscheck varh (list trunc) (cdr ans)))))

(defun psexpon-gcd (terms)
   (do ((gcd (le terms) (egcd (le l) gcd))
	(l (n-term terms) (n-term l)))
       ((null l) gcd)))

(defun psfind-s (p)
   (if (psp p) (psfind-s (psterm (terms p) (rczero)))
      (psfind-s1 p)))

(defun psfind-s1 (r)
   (cond ((null (atom (cdr r))) (rczero))
	 ((atom (car r)) r)
	 (t (do ((p (pterm (cdar r) 0) (pterm (cdr p) 0)))
		((atom p) (cons p (cdr r)))))))

(defun psexpt (p n)
    (cond ((rczerop n)			;; p^0
	   (if (rczerop p)		;; 0^0
	       (merror (intl:gettext "taylor: 0^0 is undefined."))
	      (rcone)))			;; Otherwise can let p^0 = 1
	  ((or (equal n (rcone)) (equal n (rcfone))) p)	;; p^1 cases
	  ((pscoefp p) (rcexpt p n))
	  ((mono-term? (terms p))	;; A monomial to a power
	   (let ((s (psfind-s n)) (n-s) (x) (l (terms p)))
	      ;; s is the numeric part of the exponent
	      (if (floatp (car s)) ;; Perhaps we souldn't
		  ;; rationalize if $keepfloat is true?
		  (setq s (maxima-rationalize (quot (car s) (cdr s)))))
	      (setq n-s (psdiff n s)	;; the non-numeric part of exponent
		    x   (e* s (le l)))	;; the degree of the lowest term
	      (setq x (if (and (null $maxtayorder) ;; if not getting all terms
			       (e> x (t-o-var (gvar p))))
			  ;; and result is of high order
			  (rczero)	;; then zero is enough
			 (pscheck (gvar-o p)	;; otherwise
				  (ncons (e+ (trunc-lvl p) ;; new trunc-level
					     (e- x (le l)))) ;; kick exponent
				  (ncons (term x (psexpt (lc l) n))))))
	      ;; x is now p^s
	      (if (or (rczerop n-s) (rczerop x))	;; is that good enough?
		  x			;; yes! The rest is bletcherous.
		 (pstimes x (psexpt (prep1 (m^ (get-inverse (gvar p))
					       (rcdisrep n-s)))
				    (ps-le p))))))
	  (t (prog (l lc le inc trunc s lt mr lim lcinv ans)
		   (setq lc (lc (setq l (terms p)))
			 le (le l) lt (lt l) trunc (trunc-lvl p)
			 inc (psexpon-gcd l) s (psfind-s n))
		   (when (floatp (car s))
		      (setq s (maxima-rationalize (quot (car s) (cdr s)))))
		   (setq ans (psexpt (setq lt (pscheck (gvar-o p) (list trunc)
						       (list lt))) n)
			 lcinv (psexpt lc (rcmone))
			 mr (e+ inc (e* s le))
			 lim (if (and (infp trunc) (not (e> s (rczero))))
				 (t-o-var (gvar p))
				;; See the comment in PSEXPT1 below which tells
				;; why we don't allow inf. trunc's here.
				(e+ (if (and (infp trunc) (not (rcintegerp s)))
					(if (infp (setq lim (t-o-var (gvar p))))
					    (infin-ord-err)
					   lim)
				       trunc)
				    (e* (e1- s) le)))
			 ans
			 (if (or (pscoefp ans) (null (eq (gvar p) (gvar ans))))
			     (list 0 (term (rczero) ans))
			    (cons 0 (terms ans))))
		   (and (null $maxtayorder)
			(or (not (infp lim))
			    (not (rcintegerp s))
			    (e> (e* s (le (last l))) (t-o-var (gvar p))))
			(setq lim (emin lim (t-o-var (gvar p)))))
		   ;;(and (infp lim) (n-term l) (e> (rczero) n)
		   ;;	  (infin-ord-err))
		   (return (psexpt1 (gvar-o p)
				    lim l n s inc 1 mr ans le lcinv))))))

(defun psexpt1 (varh trunc l n s inc m mr ans r linv)
   ;; n is the power we are raising the series to
   ;; inc is the exponent increment
   ;; mr is the current exponent
   ;; tr is the truncation level desired
   ;; m is the term index
   (declare (fixnum m))
   ;; s ;Ignored <- not true, see below. Who wrote this?
   (prog (a (k 0) ak cm-k c ma0 sum kr tr)
	 (declare (fixnum k))
	 ;; truly unfortunate that we need so many variables in this hack
	 (setq a (last ans) tr trunc)
	 ;; I don't see what's wrong with truncating exact series when
	 ;; raising them to fractional powers so we'll allow it for now.
	 ;; This is accomplished above in PSEXPT (see the comment). Thus,
	 ;; presumably, this check should never be needed anymore.
	 ;; Bugs catching this clause were sqrt(1-x)*taylor(f1,x,0,0)
	 ;; and sqrt(taylor(x+x^2,x,0,2)),taylor_truncate_polynomials=false.
	 (when (infp tr)
	    (if (rcintegerp s)
		(setq tr (e* s (le (last l))))
	       (merror (intl:gettext "taylor: expected an integer, instead found: ~:M") s)))
	 (when (infp tr) (setq tr (t-o-var (car varh))))
	 b (and (e> mr tr) (go end))
	   (setq kr inc ak l ma0 (pstimes (cons 1 m) linv)
		 k 1 sum (rczero))
	 a (if (or (> k m) (null (setq cm-k (psterm (cdr ans) (e- mr kr)))))
	       (go add-term))
	   (setq ak (or (pstrim-terms ak (e+ kr r)) (go add-term))
		 c (pstimes (psdiff (pstimes (cons k 1) n)
				    (cons (- m k) 1))
			    (pstimes (if (e= (e+ kr r) (le ak))
					 (lc ak)
					 (rczero))
				     cm-k)))
	   (setq sum (psplus sum c)
		 k (1+ k) kr (e+ kr inc))
	   (go a)
	 add-term
	  (and (null (rczerop sum))
	       (add-term-&-pop a mr (pstimes ma0 sum)))
	  (setq m (1+ m) mr (e+ mr inc))
	 (go b)
	 end (return (pscheck varh (list trunc) (cdr ans)))))

(defun psderivative (p v)
   (cond ((pscoefp p) (rcderiv p v))
	 ((eq v (gvar p))
	  (if (prog1 (rczerop (ps-le p))
		     (setq p (psderiv1 (gvar-o p)
				(trunc-lvl p) (cons 0 (terms p)) (list 0))))
	      (strip-zeroes p 't) p))
	 (t (psderiv2 (gvar-o p)
		      (trunc-lvl p) v (cons 0 (terms p)) (list 0)))))

(defun psderiv1 (varh trunc l ans)
       (do ((a (last ans)))
	   ((or (mono-term? l) (e> (le (n-term l)) trunc))
	    (pscheck varh (list (e1- trunc)) (cdr ans)))
	   (setq l (n-term l))
	   (when (not (rczerop (le l)))
	      (add-term-&-pop a (e1- (le l)) (pstimes (le l) (lc l))))))

(defun psderiv2 (varh trunc v l ans)
       (do ((a (last ans) (n-term a)) (c))
	   ((or (mono-term? l) (e> (le (n-term l)) trunc))
	    (pscheck varh (list trunc) (cdr ans)))
	   (setq l (n-term l))
	   (or (rczerop (setq c (psderivative (lc l) v)))
	       (add-term a (le l) c))))

(defun psdp (p)
  (let (temp temp2)
   (cond ((pscoefp p) (rcderivx p))
	 ((or (rczerop (setq temp (getdiff (gvar-o p))))
	      (eq (car temp) 'multi))
	  (setq temp2 (psdp2 (gvar-o p) (trunc-lvl p)
			     (cons 0 (terms p)) (list 0)))
	  (if (eq (car temp) 'multi)
	      (pstimes temp2
		       (make-ps (gvar-o p) (ncons (inf))
				(list (term (cdr temp) (rcone)))))
	      temp2))
	 (t (psdp1 (gvar-o p)
		   (trunc-lvl p) (cons 0 (terms p))
		   (list 0) temp)))))

(defun psdp1 (varh trunc l ans dx)
       (do ((a (last ans)) (c (rczero)))
	   ((or (mono-term? l) (e> (le (n-term l)) trunc))
	    (psplus c (pscheck varh (list (e1- trunc)) (cdr ans))))
	   (setq l (n-term l))
	   (if (rczerop (le l)) (setq c (psdp (lc l)))
	       (add-term-&-pop
		a (e1- (le l)) (pstimes (le l) (pstimes dx (lc l)))))))

(defun psdp2 (varh trunc l ans)
       (do ((a (last ans)) (c))
	   ((or (mono-term? l) (e> (le (n-term l)) trunc))
	    (pscheck varh (list trunc) (cdr ans)))
	   (setq l (n-term l))
	   (when (null (rczerop (setq c (psdp (lc l)))))
		 (add-term-&-pop a (le l) c))))

;;; Currently unused
;;;
;;; (defun psintegrate (p v)
;;;    (cond ((rczerop p) (rczero))
;;;	  ((pscoefp p)
;;;	   (pstimes p (taylor2 (get-inverse (car v)))))
;;;	  ((eqgvar v (gvar-o p))
;;;	   (psinteg1 (gvar-o p)
;;;		     (trunc-lvl p) (cons 0 (terms p)) (list 0)))
;;;	  (t (psinteg2 (gvar-o p)
;;;		       (trunc-lvl p) v (cons 0 (terms p)) (list 0)))))
;;;
;;; (defun psinteg1 (varh trunc l ans)
;;;       (prog (a)
;;;	     (setq a (last ans))
;;;	a    (if (or (null (n-term l)) (e> (le (n-term l)) trunc))
;;;		 (go end)
;;;		 (add-term a (e1+ (le (setq l (n-term l))))
;;;			   (pstimes (le l)
;;;				    (if (e= (le l) (rcmone))
;;;					(prep1 (list '(%LOG)
;;;						     (get-inverse
;;;						      (car varh))))
;;;					(lc l))))
;;;		 (setq a (n-term a)))
;;;	     (go a)
;;;        end  (return (pscheck varh (list (e1+ trunc)) (cdr ans)))))

;;; (defun psinteg2 (varh trunc v l ans)
;;;        (prog (a)
;;;	     (setq a (last ans))
;;;     a    (if (or (null (n-term l)) (e> (le (n-term l)) trunc))
;;;		 (go end)
;;;		 (add-term a (le l)
;;;			   (psintegrate (lc (setq l (n-term l))) v))
;;;		 (setq a (n-term a)))
;;;	     (go a)
;;;	end  (return (pscheck varh (list trunc) (cdr ans)))))

(defun psexpt-log-ord (p)
   (cond ((null $maxtayorder) (emin (trunc-lvl p) (t-o-var (gvar p))))
	 ((infp (trunc-lvl p)) (t-o-var (gvar p)))
	 (t (trunc-lvl p))))

;(defun ps-infp (p)
;   (if (pscoefp p) ()
;      (get-) "..."))

(defun psexpt-fn (p)
  (let (ans ord<0?)
   (cond ((pscoefp p) (psexpt-fn2 (rcdisrep p)))
	 ((ps-lim-infp p) (psexpt-fn-sing p))
	 ((prog2 (setq ord<0? (e> (rczero) (ps-le p)))
		 (null (n-term (terms p))))
	  (setq ans (get-series '%ex (psexpt-log-ord p) (gvar-o p)
				(if ord<0? (e- (ps-le p)) (ps-le p))
				(ps-lc p)))
	  (if ord<0? (ps-invert-var ans) ans))
	 ((if ord<0?
	      (when (e= (rczero) (e (setq ans (ps-gt p))))
		 (pstimes (psexpt-fn (pscheck (gvar-o p) (list (trunc-lvl p))
					      (delete ans (terms p) :test #'eq)))
			  (psexpt-fn2 (srdis (c ans)))))
	     (when (e= (rczero) (ps-le p))
		(pstimes (psexpt-fn2 (srdis (lc (terms p))))
			 (psexpt-fn (pscheck (gvar-o p) (list (trunc-lvl p))
					     (n-term (terms p))))))) )
	 (t (prog (l inc trunc ea0 ans)
	       (setq l (terms p))
	       (when ord<0?
		  ;(return (ps-invert-var (psexpt-fn (ps-invert-var p))))
		  (setq l (invert-terms l)))
	       (setq trunc (trunc-lvl p)
		     inc (psexpon-gcd l) ea0 (rcone))
	       (unless (e> (le l) (rczero))
		  ;; MEANING OF FOLLOWING MESSAGE IS OBSCURE
		  (merror "PSEXPT-FN: unreachable point."))
	       (setq ans
		     (if (or (pscoefp ea0) (null (eq (gvar p) (gvar ea0))))
			 (list 0 (term (rczero) ea0))
			(cons 0 (terms ea0))))
	       (unless $maxtayorder
		  (setq trunc (emin trunc (t-o-var (gvar p)))))
	       (when (infp trunc) (setq trunc (t-o-var (gvar p))))
	       (setq ans (psexpt-fn1 (gvar-o p) trunc l inc 1 inc ans))
	       (return (if ord<0? (ps-invert-var ans) ans)))))))

(defun psexpt-fn-sing (p)
   (let ((inf-var? (member (gvar-lim (gvar p)) '($inf $minf) :test #'eq))
	 (c*logs (c*logs (lt-poly p))) c strongest-term)
      ;; Must pull out out logs here: exp(ci*log(ui)+x) -> ui^ci*exp(x)
      ;; since its much harder for adjoin-tvar to do this transformation
      ;; below after things have been disrepped.
      (setq c (exp-c*logs c*logs) p (psdiff p (sum-c*logs c*logs)))
      (if (not (ps-lim-infp p))
	  ;; Here we just subtracted the only infinite term, e.g.
	  ;; p = 1/2*log(x)+1/log(x)+...
	  (pstimes c (psexpt-fn p))
	  (progn
	    (setq strongest-term (if inf-var? (ps-gt p) (ps-lt p)))
	    ;; If the strongest term has degree 0 in the mainvar then the singular
	    ;; terms occur in some other weaker var. There may be terms in this
	    ;; coef which arent singular (e.g. 1 in (1/x+1+...)+exp(-1/x)+...) so
	    ;; we must recursively psexpt-fn this term to get only what we need.
	    (if (rczerop (e strongest-term))
		(setq c (pstimes c (psexpt-fn (c strongest-term))))
		(dolist (exp (expand-and-disrep strongest-term p))
		  (setq c (pstimes c (adjoin-tvar (m^ '$%e exp))))))
	    (pstimes c (psexpt-fn (pscheck (gvar-o p) (list (trunc-lvl p))
					   (if inf-var?
					       (delete strongest-term (terms p) :test #'eq)
					       (n-term (terms p))))))))))

(defun gvar-logp (gvar)
   (let ((var (get-inverse gvar)))
      (and (consp var) (eq (caar var) 'mexpt) (equal (caddr var) -1)
	   (consp (setq var (cadr var))) (eq (caar var) '%log)
	   var)))

(defun c*logs (p)
   (if (pscoefp p) ()
      (let ((log (gvar-logp (gvar p))) c)
	 (if (not log)
	     ()
	     (progn
	       (setq c (psconst (psterm (terms p) (rcmone))))
	       ;; We don't want e.g. exp(x^a*log(x)) -> x^x^a
	       (if (not (mfree (rcdisrep c) tvars)) ()
		   (cons (cons c (cons log p))
			 (c*logs (psterm (terms p) (rczero))))))))))

(defun psconst (p)
   (if (pscoefp p) p (psconst (psterm (terms p) (rczero)))))

(defun exp-c*logs (c*logs)
   (if (null c*logs) (rcone)
      (pstimes (taylor2 `((mexpt) ,(cadr (cadr (car c*logs)))
				  ,(rcdisrep (caar c*logs))))
	       (exp-c*logs (cdr c*logs)))))

(defun sum-c*logs (c*logs)
   (if (null c*logs) (rczero)
      (let ((ps (cddr (car c*logs))))
	 (psplus (make-ps ps (ncons (term (ps-le ps) (caar c*logs))))
		 (sum-c*logs (cdr c*logs))))))

;; Calculatest the limit of a series at the expansion point. Returns one of
;; {$zeroa, $zerob, $pos, $neg, $inf, $minf}.

(defvar tvar-limits ()
   "A list of the form ((gvar . limit(gvar)) ...)")

(defun ps-lim-infp (ps)
   (if (pscoefp ps) ()
      ;; Assume taylor vars at 0+ for now. Should handle the cases when
      ;; the expansion point is INF, MINF,etc.
      (let* ((lim (gvar-lim (gvar ps)))
	     (strongest-term
	      (if (member lim '($inf $minf) :test #'eq) (ps-gt ps) (ps-lt ps))))
	 (if (ezerop (e strongest-term))
	     (ps-lim-infp (c strongest-term))
	     (progn
	       (setq lim (lim-power lim (e strongest-term)))
	       (and (lim-infp lim) (not (eq lim '$infinity))))))))

(defun lim-zerop (lim)
  (member lim '($zeroa $zerob $zeroim) :test #'eq))

(defun lim-plusp (lim)
  (member lim '($zeroa $pos $inf $finite) :test #'eq))

(defun lim-finitep (lim)
  (member lim '($pos $neg $im $finite) :test #'eq))

(defun lim-infp (lim)
  (member lim '($inf $minf $infinity) :test #'eq))

(defun lim-imagp (lim)
  (member lim '($im $infinity) :test #'eq))

(defun lim-minus (lim)
  (cdr (assoc lim '(($zeroa . $zerob) ($zerob . $zeroa) ($pos . $neg) ($zero . $zero)
		    ($neg . $pos) ($inf . $minf) ($minf . $inf)
		    ($im . $im) ($infinity . $infinity) ($finite . $finite)) :test #'eq)))
(defun lim-abs (lim)
   (or (cdr (assoc lim '(($zerob . $zeroa) ($neg . $pos) ($minf . $inf)) :test #'eq))
       lim))

(defun lim-times (lim1 lim2)
  (let (lim)
   (cond ((or (eq lim1 '$zero) (eq lim2 '$zero)) (setq lim '$zero))
	 ((and (lim-infp lim1) (lim-infp lim2)) (setq lim '$inf))
	 ((and (lim-zerop lim1) (lim-zerop lim2)) (setq lim '$pos))
	 ((or (when (lim-finitep lim2) (exch lim1 lim2) 't)
	      (lim-finitep lim1))
	  (when (and (eq lim1 '$finite) (lim-infp lim1))
	     (break "Undefined finite*inf in lim-times"))
	  (setq lim (lim-abs lim2)))
	 (t (break "Undefined limit product ~A * ~A in lim-times" lim1 lim2)))
   (if (or (lim-imagp lim1) (lim-imagp lim2))
       (if (lim-infp lim) '$infinity '$im)
      (if (and (lim-plusp lim1) (lim-plusp lim2)) lim (lim-minus lim)))))

(defun lim-power (lim power)
   (cond ((ezerop power) '$pos)
	 ((e> (rczero) power) (lim-recip (lim-power lim (e- power))))
	 ((not (oddp (car power)))
	  (if (lim-plusp lim) lim (lim-minus lim)))
	 (t lim)))

(defun lim-recip (lim)
   (or (cdr (assoc lim '(($zeroa . $inf) ($zerob . $minf)
			 ($inf . $zeroa) ($minf . $zerob)) :test #'eq))
       (if (eq lim '$finite) (break "inverting $finite?")
	  lim)))

(defun lim-exp (lim)
   (case lim
      (($zeroa $zerob $zero $pos $neg $minf) '$zeroa)
      (($inf $finite) lim)
      ($infinity '$infinity) ; actually only if Re lim = inf
      (t (break "Unhandled limit in lim-exp"))))

(defun lim-log (lim)
   (case lim
      ($zeroa '$minf)
      ($inf '$inf)
      ($minf '$infinity)
      ($zerob '$infinity)
      (t (break "Unhandled limit in lim-log"))))

(defun expand-and-disrep (term p)
   (let ((x^n (list '(mexpt) (get-inverse (gvar p)) (edisrep (e term))))
	 (a (c term)))
      (if (pscoefp a) (ncons (m* (srdis a) x^n))
	 (mapcar #'(lambda (subterm)
		      (m* (cons '(mtimes) (expand-and-disrep subterm a)) x^n))
		 (terms a)))))

(defun adjoin-sing-datum (d)
   (let ((r (prep1 (datum-var d))) (g (gensym)) (kernel (datum-var d))
	 (no (1+ (cdr (int-var (car (last tlist)))))))
      (unless (and (equal (car r) 1) (equal (cddr r) '(1 1)))
	 (break "bad singular datum"))
      (putprop g kernel 'disrep)
      (rplacd (cdddr d) (cons g no))
      (adjoin-datum d)
      (push (cons (cadr r) kernel) key-vars)
      (push (cons g kernel) key-vars)
      (push (car key-vars) ivars)
      ;(push (cons kernel (cons (pget g) 1)) genpairs)
      (push (cons g (exp-pt d)) tvar-limits)))

(defun adjoin-tvar (exp) (rat->ps (prep1 exp)))

(defun rat->ps (rat)
   (pstimes (poly->ps (car rat))
	    (psexpt (poly->ps (cdr rat)) (rcmone))))

(defun poly->ps (poly)
  (if (or (pcoefp poly) (mfree (pdis poly) tvars)) (prep1 poly)
      (let ((g (p-var poly)) datum (pow (rcone)))
	(if (setq datum (key-var-pow g)) (desetq (g . pow) datum)
	    (desetq (g . pow) (adjoin-pvar g)))
	(if (and (not (atom g)) (psp g))
	    g
	    (progn
	      (setq datum (gvar-data g))
	      (do ((po-terms (p-terms poly) (p-red po-terms))
		   (ps-terms ()
			     (push (term (e* pow (prep1 (pt-le po-terms)))
					 (poly->ps (pt-lc po-terms)))
				   ps-terms)))
		  ((null po-terms)
	   ;; This must be exact so that when we invert in rat-ps above
	   ;; we dont lose any terms. E.g. try
	   ;; taylor(log(1+exp(-1/x)),x,0,5). When taylor2'ing exp(-1/x),
	   ;; if you used current trunc here this would return exp(1/x)...5
	   ;; which would then be trunc'd to degree 3 by psexpt.
		   (make-ps (int-var datum)
			    (ncons (current-trunc datum))
			    (if (eq g (data-gvar datum)) ps-terms
				(invert-terms ps-terms))))))))))

(defun key-var-pow (g)
   (let ((var (get-key-var g)) datum)
      (when var
	 (setq datum (get-datum var))
	 (if (eq g (setq g (data-gvar datum))) (cons g (rcone))
	    (cons g (rcmone))))))

(defun adjoin-pvar (g)
  (let ((kernel (get g 'disrep)) g* lim datum ans
	(no (1+ (cdr (int-var (car (last tlist)))))) (pow (rcone)) expt)
    (when (assol kernel tlist) (break "bad1"))
    (if (and (eq (caar kernel) 'mexpt) (eq (cadr kernel) '$%e)
	     (not (atom (setq expt (caddr kernel))))
	     (eq (caar expt) 'mtimes) (not (mfree expt (ncons '$%i))))
	(destructuring-let (((rpart . ipart) (trisplit expt)))
	   (cons (pstimes (prep1 (m^ '$%e rpart))
			  (psplus (adjoin-tvar `((%cos) ,ipart))
				  (pstimes (prep1 '$%i)
					   (adjoin-tvar `((%sin) ,ipart)))))
			  pow))
	(progn
	  (when (eq (caar kernel) 'mexpt)
	    (when (and (not (atom (setq expt (caddr kernel))))
		       (eq (caar expt) 'mtimes)
		       ($ratnump (cadr expt)))
	      (setq pow (cadr expt) kernel (m^ kernel (m// pow))
		    g (prep1 kernel) pow (prep1 pow))
	      (unless (and (equal (cdr g) 1) (equal (cdar g) '(1 1)))
		(break "Illegal kernel in `adjoin-pvar'"))
	      (setq g (caar g) kernel (get g 'disrep))))
	  (if (setq ans (key-var-pow g))
	      (cons (car ans) (e* pow (cdr ans)))
	      (progn
		(when (lim-infp (or lim (setq lim (tvar-lim kernel))))
		  (setq g* g g (gensym) kernel (m// kernel)
			lim (lim-recip lim) pow (e* (rcmone) pow))
		  (putprop g kernel 'disrep)
					;(push g genvar) (push kernel varlist)
		  (push (cons g* kernel) key-vars))
		(when (assol kernel tlist) (break "bad2"))
		(setq datum (list* kernel
					;(mapcar #'(lambda (e) (emax e (rczero)))
					;	    (trunc-stack (car tlist)))
				   (copy-list (trunc-stack (car tlist)))
				   lim () g no))
					;(setq tlist (nconc tlist (ncons datum)))
		(adjoin-datum datum)
		(push (cons g kernel) key-vars)
		(push (car key-vars) ivars)
					;(push (cons kernel (cons (pget g) 1)) genpairs)
		(push (cons g lim) tvar-limits)
		(cons g pow)))))))

(defun adjoin-datum (datum)
   (do ((tlist* tlist (cdr tlist*))
	(tlist** () tlist*))
       ((null tlist*) (setq tlist (nconc tlist (ncons datum))))
      (when (stronger-var? (datum-var (car tlist*)) (datum-var datum))
	 (return (if (null tlist**)
		     (progn
		       (push datum tlist)
		       (renumber-tlist tlist))
		     (progn
		       (rplacd tlist** (cons datum tlist*))
		       (renumber-tlist (cdr tlist**))))))))

;; Maybe this should just permute the numbering in case it isn't sequential?

(defun renumber-tlist (tlist)
   (rplacd (data-gvar-o (car tlist)) (cdr (data-gvar-o (cadr tlist))))
   (do ((tlist* (cdr tlist) (cdr tlist*)))
       ((null tlist*))
      (rplacd (data-gvar-o (car tlist*))
	      (1+ (cdr (data-gvar-o (car tlist*)))))))

(defun tvar? (var) (or (atom var) (member 'array (cdar var) :test #'eq)))

;; Needs to be extended to handle singular tvars in > 1 var's.

(defun stronger-var? (v1 v2)
  (let ((e1 (rcone)) (e2 (rcone)) reverse? ans)
    (when (alike1 v1 v2)
      (tay-err (intl:gettext "taylor: stronger-var? called on equal vars.")))
    (when (and (mexptp v1) ($ratnump (caddr v1)))
      (setq e1 (prep1 (caddr v1)) v1 (cadr v1)))
    (when (and (mexptp v2) ($ratnump (caddr v2)))
      (setq e2 (prep1 (caddr v2)) v2 (cadr v2)))
    (if (alike1 v1 v2)
	(if (equal e1 e2)
            (tay-err 
              (intl:gettext "taylor: stronger-var? called on equal vars."))
	    (e> e1 e2))
	(progn
	  (when (eq (tvar-lim v2) '$finite)
	    (exch v1 v2) (exch e1 e2) (setq reverse? (not reverse?)))
	  (if (eq (tvar-lim v1) '$finite)
	      (if (eq (tvar-lim v2) '$finite)
		  (great v1 v2) reverse?)
	      (progn
		(when (mtimesp v2)
		  (exch v1 v2) (exch e1 e2) (setq reverse? (not reverse?)))
		(setq ans
		      (if (mtimesp v1)
			  (stronger-vars? (order-vars-by-strength (cdr v1))
					  (order-vars-by-strength (if (mtimesp v2) (cdr v2)
								      (ncons (m^ v2 (edisrep e2))))))
			  (progn
			    (when (tvar? v2)
			      (exch v1 v2) (exch e1 e2) (setq reverse? (not reverse?)))
			    (if (tvar? v1)
				(cond ((tvar? v2)
				       (let ((n1 (cdr (data-gvar-o (get-datum v1 t))))
					     (n2 (cdr (data-gvar-o (get-datum v2 t)))))
					 (> n1 n2)))
				      ((mfree v2 (ncons v1))
				       (tay-err 
				         (intl:gettext "taylor: Unhandled multivar datum comparison.")))
				      ((eq (caar v2) '%log) 't)
				      ((and (eq (caar v2) 'mexpt) (eq (cadr v2) '$%e))
				       (stronger-var? `((%log) ,v1) (caddr v2)))
				      (t (tay-err (intl:gettext "taylor: Unhandled var in stronger-var?."))))
				(progn
				  (when (eq (caar v2) '%log)
				    (exch v1 v2) (exch e1 e2) (setq reverse? (not reverse?)))
				  (if (eq (caar v1) '%log)
				      (cond ((eq (caar v2) '%log)
					     (stronger-var? (cadr v1) (cadr v2)))
					    ((and (eq (caar v2) 'mexpt) (eq (cadr v2) '$%e))
					     (stronger-var? `((%log) ,v1) (caddr v2)))
					    (t (tay-err (intl:gettext "taylor: Unhandled var in stronger-var?"))))
				      (if (and (eq (caar v1) 'mexpt) (eq (cadr v1) '$%e)
					       (eq (caar v2) 'mexpt) (eq (cadr v2) '$%e))
					  (stronger-var? (caddr v1) (caddr v2))
					  (tay-err (intl:gettext "taylor: Unhandled var in stronger-var?")))))))))
		(if reverse? (not ans) ans)))))))

(defun neg-monom? (exp)
   (and (mtimesp exp) (equal (cadr exp) -1) (null (cdddr exp))
	(caddr exp)))

(defun order-vars-by-strength (vars)
   (do ((vars* vars (cdr vars*)) (ordvars () ))
       ((null vars*) ordvars)
      (unless (mfree (car vars*) tvars) ; ignore constants
	 (do ((ordvars* ordvars (cdr ordvars*)))
	     ((null ordvars*)
	      (if (null ordvars) (setq ordvars (ncons (car vars*)))
		 (rplacd (last ordvars) (ncons (car vars*)))))
	    (when (stronger-var? (car vars*) (car ordvars*))
	       (rplacd ordvars* (cons (car ordvars*) (cdr ordvars*)))
	       (rplaca ordvars* (car vars*))
	       (return () ))))))

(defun stronger-vars? (vars1 vars2)
   (do ((vars1* vars1 (cdr vars1*))
	(vars2* vars2 (cdr vars2*)))
       (())
      (cond ((null vars1*)
	     (if (null vars2*)
		 ;; two equal vars generated
		 (return 't)
		(let ((lim (tvar-lim (car vars2*))))
		   (return
		    (cond ((lim-infp lim) ())
			  ((lim-zerop lim) 't)
			  (t (break "var with non-zero finite lim?")))))))
	    ((null vars2*)
	     (let ((lim (tvar-lim (car vars1*))))
	       (return
		 (cond ((lim-infp lim) 't)
		       ((lim-zerop lim) ())
		       (t (break "var with non-zero finite lim?"))))))
	    ((alike1 (car vars1*) (car vars2*)) )
	    ((return (stronger-var? (car vars1*) (car vars2*)))))))

(defun stronger-datum? (d1 d2)
   (setq d1 (datum-var d1) d2 (datum-var d2))
   (do ((end-flag) (answer))
       (end-flag (member answer '($yes $y) :test #'eq))
      (setq answer (retrieve `((mtext) |Is  | ,d1 | stronger than | ,d2 |?|)
			     nil))
      (if (member answer '($yes $y $no $n) :test #'eq) (setq end-flag 't)
	 (mtell "~%Acceptable answers are: yes, y, no, n~%"))))

(defun datum-lim (datum)
   (if (not (tvar? (datum-var datum)))
       (exp-pt datum)
      (let ((pt (exp-pt datum)))
	 (if (member pt '($inf $minf) :test #'eq) pt '$zeroa))))

(defun tvar-lim (kernel)
  (if (mfree kernel tvars) (coef-sign kernel)
    (let ((datum (get-datum kernel t)) lim)
      (or (and datum (datum-lim datum))
	  (and (setq datum (get-datum (m// kernel) t))
	       (setq lim (datum-lim datum))
	       (lim-recip lim))
	  (progn
	   (setq lim
		 (cond ((eq (caar kernel) 'mexpt)
			(cond ((and (setq datum (get-datum (cadr kernel) t))
				    ($ratnump (caddr kernel)))
			       (lim-power (datum-lim datum)
					  (prep1 (caddr kernel))))
			      (($ratnump (caddr kernel))
			       (lim-power (tvar-lim (cadr kernel))
					  (prep1 (caddr kernel))))
			      ((eq (cadr kernel) '$%e)
			       (lim-exp (tvar-lim (caddr kernel))))
			      (t (break "Unhandled case in tvar-lim"))))
		       ((eq (caar kernel) 'mtimes)
			(do ((ans (tvar-lim (cadr kernel))
				  (lim-times ans (tvar-lim (car facs))))
			     (facs (cddr kernel) (cdr facs)))
			    ((null facs) ans)))
		       ((eq (caar kernel) '%log)
			(lim-log (datum-lim (get-datum (cadr kernel) t))))
		       ((member (caar kernel) '(%sin %cos) :test #'eq)
		        (unless (lim-infp (tvar-lim (cadr kernel)))
		          (tay-error "Invalid trig kernel in tvar-lim" kernel))
			'$finite)
		       (t (tay-error "Unhandled kernel in tvar-lim" kernel))))
	  lim)))))

(defun coef-sign (coef)
   (if (not ($freeof '$%i ($rectform coef)))
       '$im
     ($asksign coef)))

(defun gvar-lim (gvar)
   (or (cdr (assoc gvar tvar-limits :test #'eq))
       (if (member (gvar->var gvar) tvars :test #'eq) '$zeroa ; user tvars assumed 0+ now
	  (break "Invalid gvar"))))

(defun psexpt-fn1 (varh trunc l inc m mr ans)
       (declare (fixnum m ))
       (prog (a (k 0) ak cm-k c sum kr lim)
	     (declare (fixnum k ))
	   ;; truly unfortunate that we need so many variables in this hack
	   (setq a (last ans))
	 b (and (e> mr trunc) (go end))
	   (setq kr inc ak l k 1 sum (rczero) lim m)
	 a (cond ((or (> k lim)
		      (null (setq cm-k (psterm (cdr ans) (e- mr kr)))))
		  (go add-term)))
	   (setq ak (or (pstrim-terms ak kr)
			(go add-term))
		 c (pstimes (ereduce k m)
			    (pstimes (psterm1 ak kr) cm-k))
		 sum (psplus sum c))
	   (setq k (1+ k) kr (e+ kr inc))
	   (go a)
	 add-term
	   (unless (rczerop sum) (add-term-&-pop a mr sum))
	   (setq m (1+ m) mr (e+ mr inc))
	   (go b)
	 end
	   (return (pscheck varh (list trunc) (cdr ans)))))

;;; PSEXPT-FN2 and RED-MONO-LOG are needed to reduce exponentials of logs.

(defun psexpt-fn2 (p)
  (cond ((atom p) (if (get-datum p)
		      (psexpt-fn (taylor2 p))
		      (prep1 `((mexpt) $%e ,p))))
	((eq (caar p) '%log)
	 (if (get-datum (cadr p)) (taylor2 (cadr p)) (prep1 (cadr p))))
	((or (eq (caar p) 'mplus) (eq (caar p) 'mtimes))
	 (let ((e ($ratexpand p)) temp)
	   (cond ((not (and (consp e) (member (caar e) '(mplus mtimes) :test #'eq)))
		  (psexpt-fn2 e))
		 (t
		  (if (eq (caar e) 'mplus)
		      (do ((sumnds (cdr e) (cdr sumnds)) (log-facs) (l))
			  ((null sumnds)
			   (cond ((not log-facs) (tsexpt '$%e p))
				 (t (tstimes (cons (m^t '$%e (m+l l)) log-facs)))))
			(if (setq temp (red-mono-log (car sumnds)))
			    (push temp log-facs)
			    (push (car sumnds) l)))
		      (progn
			(setq temp (red-mono-log e))
			(if temp
			    (taylor2 temp)
			    (prep1 (power '$%e p)))))))))
	(t (prep1 (power '$%e p)))))

(defun red-mono-log (e)
   (cond ((atom e) ())
	 ((eq (caar e) '%log) (cadr e))
	 ((mtimesp e)
	  (do ((facs (cdr e) (cdr facs)) (log-term))
	      ((null facs)
	       (when log-term
		     (m^t (cadr log-term) (m*l (remove log-term (cdr e) :test #'eq)))))
	      (if (and (null (atom (car facs))) (eq (caaar facs) '%log))
		  (if log-term (return ()) (setq log-term (car facs)))
		  (unless (mfree (car facs) tvars) (return nil)))))
	 (t nil )))

(defun pslog (p)
   (if (pscoefp p) (pslog2 (rcdisrep p))
       (let ((terms (terms p)))
	  (cond ((mono-term? terms) ; log(c x^n) = log(c) + n log(x)
		 ;; do this always for now
		 (if 't ;$TAYLOR_LOGEXPAND
		     ;(psplus (pslog (lc terms))
		     ;	     (pstimes (le terms) (pslog-of-gvar (gvar p))))
		     (pslog-monom p)
		     ;(prep1 `((%LOG) ,(term-disrep (lt terms) p)))
		     ))
		;; expand log(1+ax^n) directly by series substitution
		((not (or (n-term (setq terms (terms (psplus p (rcmone)))))
			  ;(e> (rczero) (le terms))
			  (ps-lim-infp p)))
		 (setq p (get-series '%log (psexpt-log-ord p) (gvar-o p)
			    (if (e> (rczero) (le terms)) (e- (le terms))
			       (le terms))
			    (lc terms)))
		 (if (e> (rczero) (le terms)) (ps-invert-var p) p))
		(t (prog (l inc trunc lt ans lterm $maxtayorder gvar-lim gt)
		    ;; log(lt+y) = log(lt) + log(1 + y/lt) = lterm + p
		    (setq trunc (trunc-lvl p))
		    (if (not (member (setq gvar-lim (gvar-lim (gvar p)))
				   '($zeroa $zerob $inf $minf) :test #'eq))
			(break "bad gvar lim")
		       (if (member gvar-lim '($inf $minf) :test #'eq)
			   (setq lt (ps-gt p) gt lt)
			  (setq lt (ps-lt p) gt () )))
		    (setq lterm (pslog
				 (setq lt (pscheck (gvar-o p)
						   (ncons trunc)
						   (ncons lt))))
			  p (pstimes p (let (($maxtayorder 't))
					  (psexpt lt (rcmone)))))
		    (when (and (member gvar-lim '($inf $minf) :test #'eq)
			       (e> (le terms) (rczero)))
		       (return (psplus lterm (pslog p))))
		    (when (pscoefp p)
		       (unless (equal p (rcone))
			  (merror "PSLOG: internal error."))
		       (return lterm))
		    (setq l (terms p) inc (psexpon-gcd l))
		    (if gt (setq l (delete (last l) l :test #'equal))
		       (setq l (n-term l)))
		    (setq ans (ncons 0))
		    (unless $maxtayorder
		       (setq trunc (emin trunc (t-o-var (gvar p)))))
		    ;; When we've divided by the greatest term, all terms
		    ;; have non-positive exponents and we must perform the
		    ;; transformation x -> 1/x befor calling pslog1 and then
		    ;; perform the inverse afterwards.
		    (when gt (setq l (invert-terms l)))
		    (when (e> (rczero) inc) (setq inc (e- inc)))
		    (setq ans (psplus lterm
				 (pslog1 (gvar-o p) trunc l inc 1 inc ans)))
		    (return
		     (if (and gt (psp ans) (eq (gvar ans) (gvar p)))
			 (ps-invert-var ans)
			ans))))))))

(defun invert-terms (terms)
   (nreverse (mapc #'(lambda (x) (rplaca x (e- (e x)))) terms)))

(defun ps-invert-var (ps)
   (when (psp ps) (rplacd (cddr ps) (invert-terms (terms ps))))
   ps)

(defun ps-gt (ps)
   (if (pscoefp ps) (term (rczero) ps)
      (lt (last (terms ps)))))

(defun pslog1  (varh trunc l inc m mr ans)
       (declare (fixnum m ))
       (prog (a (k 0) ak cm-k c sum kr m-kr)
	     (declare (fixnum k ))
	   ;; truly unfortunate that we need so many variables in this hack
	   ;;
	   (setq a (last ans))
	 b (and (e> mr trunc) (go end))
	   (setq kr inc ak l k 1 sum (rczero))
	 a (cond ((or (= k m)
		      (null (setq cm-k (psterm (cdr ans)
					       (setq m-kr (e- mr kr))))))
		  (go add-term)))
	   (setq ak (or (pstrim-terms ak kr)
			(go add-term))
		 c (pstimes m-kr (pstimes (psterm1 ak kr) cm-k))
		 sum (psplus sum c)
		 k (1+ k) kr (e+ kr inc))
	   (go a)
	 add-term
	   (cond ((setq c (pstrim-terms ak mr))
		  (setq c (psterm1 c mr)))
		 ((setq c (rczero))))
	   (setq sum (psdiff c (pstimes sum (e// mr))))
	   (unless (rczerop sum) (add-term-&-pop a mr sum))
	   (setq m (1+ m) mr (e+ mr inc))
	   (go b)
	 end
	   (return (pscheck varh (list trunc) (cdr ans)))))

;; Computes log(monom), where monom = c x^n. Is extra careful trying to keep
;; singular logs going to INF and not generating log(-1)'s unless it is
;; necessary to transform a log at MINF to INF.

(defun pslog-monom (monom)
  (let* ((gvar (gvar monom))
	 (datum (gvar-data gvar)) var pt logvar c)
    (if (switch 'multivar datum)
	(pslog (ps-lc monom))
	(progn
	  (setq var (datum-var datum))
	  (if (tvar? var)
	      (if (not (member (setq pt (exp-pt datum)) '($inf $minf) :test #'eq))
		  (setq logvar (adjoin-tvar `((%log) ,(m- var pt))))
		  (progn
		    ;; At x = inf: log(c (1/x)^n) -> log(c) - n log(x)
		    ;; At x = minf: log(c (-1/x)^n) -> log(c (-1)^n) - n log(x)
		    (setq logvar (psminus (adjoin-tvar `((%log) ,var))))
		    (when (eq pt '$minf)
		      (setq c (rcexpt (rcmone) (ps-le monom))))))
	      (if (eq (caar var) 'mexpt)
		  (if (equal (caddr var) -1);; var must be 1/log(y)
		      ;; Try to keep inf's real. Here we want
		      ;; log(c (1/log(x))^n) -> log(c (-1)^n) - n log(-log(x))
		      (if (equal (tvar-lim (cadr var)) '$minf)
			  (setq c (rcexpt (rcmone) (ps-le monom))
				logvar
				(psminus (adjoin-tvar
					  `((%log) ,(m- (cadr var))))))
			  (setq logvar (psminus
					(adjoin-tvar `((%log) ,(cadr var))))))
		      (if (equal (cadr var) '$%e)
			  (setq logvar (taylor2 (caddr var)))
			  (break "Unhandled gvar in `pslog-of-gvar'")))))
	  (psplus (pslog (if c (pstimes c (ps-lc monom)) (ps-lc monom)))
		  (pstimes (ps-le monom) logvar))))))

;; Computes log(p), where p is an rcdisrep'd pscoef.

(defun pslog2 (p) (let ($logarc) (pslog3 p)))

(defun pslog3 (p)
   (cond ((atom p)
	  (prep1 (cond ((equal p 1) 0)
		       ((equal p -1) log-1)
		       ((eq p '$%i) log%i)
		       ((eq p '$%e) 1)
		       ((equal p 0)
			(merror (intl:gettext "taylor: log(0) encountered while processing ~:M") last-exp))
		       (t `((%log) ,p)))))
	 ((eq (caar p) 'rat)
	  (prep1 (cond ((not $taylor_logexpand) `((%log) ,p))
		       (t (m- `((%log) ,(cadr p)) `((%log) ,(caddr p)))))))
	 ((and full-log (not (free p '$%i)))
	  (let ((full-log () )) (pslog3 ($polarform p))))
	 ((eq (caar p) 'mexpt)
	  ;; Must handle things like x^a, %e^(a*x), etc. which are pscoef's.
	  (pstimes (taylor2 (caddr p)) (pslog (taylor2 (cadr p)))))
	 ((and (eq (caar p) 'mtimes) $taylor_logexpand)
	  (do ((l (cddr p) (cdr l))
	       (ans (pslog3 (cadr p)) (psplus ans (pslog3 (car l)))))
	      ((null l) ans)))
	 (t (prep1 `((%log) ,p)))))

;;;		 Subtitle Extending Routines

(defun getfun-lt (fun)
   (let ((exp-datum (get (oper-name fun) 'exp-form)))
	(cond (exp-datum
		   ;; Info not needed yet.
		   ;; (or (atom (car exp-datum))
		   ;;     (setq 0p-funord (copy-tree (cdar exp-datum))))
	       (exp-datum-lt fun exp-datum))
	      ((setq exp-datum (get (oper-name fun) 'sp2))
	       (setq exp-datum (get-lexp (subst (dummy-var) 'sp2var exp-datum)
					 (rcone) ()))
		   ;; Info not needed yet; need to bind lexp-non0 to T when
		   ;; this is used though so n-term will be there.
		   ;; (and (rczerop (le exp-datum))
		   ;;      (setq 0p-funord (le (n-term exp-datum))))
	       (if (psp exp-datum) (ps-lt exp-datum)
		   (term (rczero) exp-datum)))
	      (t (merror "GETFUN-LT: unknown function ~A" fun)))))

(declare-top (special var))

(defun getexp-fun (fun var pw)
  (declare (special var))
  (let ((exp-datum (copy-tree (get (oper-name fun) 'exp-form))))
    (cond ((infp pw) (infin-ord-err))
	  ((null exp-datum)
	   (if (null (setq exp-datum
			   (get-ps-form (if (atom fun) fun (caar fun)))))
	       (merror (intl:gettext "taylor: power series unavailable for function ~A") fun)
	       (progn
		 (unless (atom fun)
		   (do ((subvals (cdr fun) (cdr subvals))
			(subs (safe-get (caar fun) 'sp2subs) (cdr subs)))
		       ((or (null subvals) (null subs))
			(when (or subvals subs)
			  (merror (intl:gettext "taylor: incorrect number of subscripts to the deftaylor'd function ~A") (caar fun))))
		     (setq exp-datum (maxima-substitute (car subvals) (car subs)
							exp-datum))))
		 (ts-formula exp-datum var pw))))
	  ((e> (exp-datum-le fun exp-datum) pw) (pszero var pw))
	  ((setq exp-datum
		 (apply (exp-fun exp-datum)
			(if (atom fun) (cons pw (cdr exp-datum))
			    (cons pw (cons (cdr fun) (cdr exp-datum))))))
	   (cond ((null exp-datum) (pszero var pw))
		 ((psp exp-datum) exp-datum)
		 (t (make-ps var (ncons pw) exp-datum)))))))

(declare-top (unspecial var))

(defun expexp-funs (pw l sign chng inc)
       (prog (e lt-l)
	     (setq e (e l) lt-l (setq l (ncons l)))
	a    (cond ((e> (setq e (e+ e inc)) pw) (return l))
		   (t (add-term-&-pop
		       lt-l
		       e
		       (rctimes (e// sign
				     (cond ((e= inc (rcone)) e)
					   ((e* e (e1- e)))))
				(cons 1 (cdr (lc lt-l)))))
		      (setq sign (e* sign chng))))
	     (go a)))

(defun explog-funs (pw l sign chng inc)
       (prog (e lt-l)
	     (setq e (e l) lt-l (setq l (ncons l)))
	a    (cond ((e> (setq e (e+ e inc)) pw) (return l))
		   (t (add-term lt-l e (e// sign e))
		      (setq lt-l (n-term lt-l)
			    sign (e* sign chng))))
	     (go a)))

(defun exptan-funs (pw l chng)
       (prog (e lt-l sign fact pow)
	     (setq e (e l) lt-l (setq l (ncons l))
		   sign (rcone) fact '(1 . 2) pow '(4 . 1))
	a    (cond ((e> (setq e (e+ (rctwo) e)) pw) (return l))
		   (t (setq fact (e// fact (e* e (e1+ e)))
			    pow (e* '(4 . 1) pow)
			    sign (e* chng sign))
		      (add-term lt-l e (e* (e* sign fact)
					   (e* (prep1
						($bern (rcdisrep (e1+ e))))
					       (e* pow (e1- pow)))))
		      (setq lt-l (n-term lt-l))))
	     (go a)))

(defun expcot-funs (pw l sign chng plus)
       (prog (e lt-l fact pow)
	     (setq e (e l) lt-l (setq l (ncons l))
		   fact (rcone) pow (rcone))
	a    (cond ((e> (setq e (e+ (rctwo) e)) pw) (return l))
		   (t (setq fact (e// fact (e* e (e1+ e)))
			    pow (e* '(4 . 1) pow)
			    sign (e* chng sign))
		      (add-term lt-l e (e* (e* sign fact)
					   (e* (prep1
						($bern (rcdisrep (e1+ e))))
					       (e+ pow plus))))
		      (setq lt-l (n-term lt-l))))
	     (go a)))

(defun expsec-funs (pw l chng)
       (prog (e lt-l sign fact)
	     (setq e (e l) lt-l (setq l (ncons l))
		   sign (rcone)  fact (rcone))
	a    (cond ((e> (setq e (e+ (rctwo) e)) pw) (return l))
		   (t (setq fact (e// fact (e* e (e1- e)))
			    sign (e* chng sign))
		      (add-term lt-l e (e* (e* sign fact)
					   (prep1 ($euler (rcdisrep e)))))
		      (setq lt-l (n-term lt-l))))
	     (go a)))

(defun expasin-funs (pw l chng)
  (prog (e lt-l sign n d)
     (setq e (e l) lt-l (setq l (ncons l)) sign 1 n 1 d 1)
     a    (cond ((e> (setq e (e+ (rctwo) e)) pw) (return l))
		(t (setq n (* n (car (e- e (rctwo))))
			 d (* d (car (e1- e)))
			 sign (* sign chng))
		   (add-term lt-l e ; need to reduce here ? - check this.
			     (let ((x (*red (* n sign)
					    (* d (car e)))))
			       (if (atom x) x
				   (cons (cadr x) (caddr x)))))
		   (setq lt-l (n-term lt-l))))
     (go a)))

;;; This is the table of expansion data for known functions.
;;; The format of the EXP-FORM property is as follows:
;;;	(<name of the expanding routine for the function or
;;;	  (name . le of n-term) if expansion is of order 0>
;;;      <first term in the expansion or the name of a routine which
;;;	  computes the order when it may depend on parameters (e.g subsripts)>
;;;      <data for the expanding routine>)


(loop for (fun exp) on
 '(%ex    ((expexp-funs 1 . 1) ((0 . 1) 1 . 1) (1 . 1) (1 . 1) (1 . 1))
  %sin   (expexp-funs ((1 . 1) 1 . 1) (-1 . 1) (-1 . 1) (2 . 1))
  %cos   ((expexp-funs 2 . 1) ((0 . 1) 1 . 1) (-1 . 1) (-1 . 1) (2 . 1))
  %sinh  (expexp-funs ((1 . 1) 1 . 1) (1 . 1) (1 . 1) (2 . 1))
  %cosh  ((expexp-funs 2 . 1) ((0 . 1) 1 . 1) (1 . 1) (1 . 1) (2 . 1))
  %log   (explog-funs ((1 . 1) 1 . 1) (-1 . 1) (-1 . 1) (1 . 1))
  %atan  (explog-funs ((1 . 1) 1 . 1) (-1 . 1) (-1 . 1) (2 . 1))
  %atanh (explog-funs ((1 . 1) 1 . 1) (1 . 1) (1 . 1) (2 . 1))
  %cot   (expcot-funs ((-1 . 1) 1 . 1) (1 . 1) (-1 . 1) (0 . 1))
  %csc   (expcot-funs ((-1 . 1) 1 . 1) (-1 . 1) (-1 . 1) (-2 . 1))
  %csch  (expcot-funs ((-1 . 1) 1 . 1) (-1 . 1) (1 . 1) (-2 . 1))
  %coth  (expcot-funs ((-1 . 1) 1 . 1) (1 . 1) (1 . 1) (0 . 1))
  %tan   (exptan-funs ((1 . 1) 1 . 1) (-1 . 1))
  %tanh  (exptan-funs ((1 . 1) 1 . 1) (1 . 1))
  %sec   ((expsec-funs 2 . 1) ((0 . 1) 1 . 1) (-1 . 1))
  %sech  ((expsec-funs 2 . 1) ((0 . 1) 1 . 1) (1 . 1))
  %asin  (expasin-funs ((1 . 1) 1 . 1) 1)
  %asinh (expasin-funs ((1 . 1) 1 . 1) -1)
  %gamma (expgam-fun ((-1 . 1) 1 . 1))
  $li    (exp$li-fun li-ord)
  $psi   (expplygam-funs plygam-ord))
  by #'cddr
  do  (putprop fun exp 'exp-form))


(defun known-ps (fun)
  (getl fun '(exp-form sp2)))

;;;	         Autoload Properties

;;;		 Taylor series expansion routines

;;; SRF is only called externally; by RATF and SIMPEXPT.

(defun srf (x)
   (let ((exact-poly t) (tlist) (*within-srf?* 't))
      (setq x (taylor1 x ()) tlist (mrat-tlist x))
      ;; Set trunc levels in the local tlist to correspond to the maximum
      ;; level occuring in any series.
      (do ((data tlist (cdr data))
	   (truncs (trunc-vector (mrat-ps x) () )))
	  ((null data))
	 (when (and (car truncs) (e> (car truncs) (current-trunc (car data))))
	    (setf (current-trunc (car data)) (car truncs))))
      x))

;;; [var, pt, order, asymp]

(defmfun $taylor (e &rest args)
  (when (not ($ratp e))
    ;; Not a mrat expression. Remove the special representation.
    (setq e (specrepcheck e)))
  (taylor* e args))

(defun taylor* (arg l)
   ;; We must bind $MAXTAYORDER to () below because of the problem of constants
   ;; not retaining their truncation level. This means that when we add a
   ;; series which has more terms than the user-specified truncation to a
   ;; constant we must truncate the series with more terms down to the user
   ;; specified level because, in the worst case, the constant could be a
   ;; series no better than to the user-specified level. Hence $MAXTAYORDER
   ;; is essentially useless until the constant problem is fixed. If one
   ;; decides to not bind $MAXTAYORDER below then the sum routines must
   ;; be updated to truncate series with more terms than the user-specified
   ;; level down to that level---taylor(sin(x)^2-cos(x)^2-1,x,0,1) would
   ;; give x^2+... in this case if the sum routines weren't updated.
   ;; Also, batch(mquery,160,aljabr) for another truncation bug which crops
   ;; up when $maxtayorder isn't bound here. Similarly, loadfile(taybad,rl,
   ;; aljabr) and see tomh's bug note of 4/15/81.
   (let ((tlist () ) ($maxtayorder () ) (*within-srf?* () )
	 (exact-poly (if l (not $taylor_truncate_polynomials) 'user-specified)))
     (declare (special *within-srf?*))

      (parse-tay-args l)
      (taylor1 arg (ncons tlist))))

(defun tay-order (n)
       (let (($float) (modulus))
	  (cond ((eq n '$inf) (ncons (inf)))
		((null n) (wna-err '$taylor))
		((null (mnump n))
		 (merror (intl:gettext "taylor: expansion order must be a number; found: ~:M") n))
		(t (ncons (prep1 n))))))

(defun re-erat (head exp)
       (taylor1 exp (list (cadddr (cdr head)))))

(defun parse-tay-args (l)
   (cond ((null l) )
	 ((numberp (car l))
	  (merror (intl:gettext "taylor: variable of expansion cannot be a number: ~M") (car l)))
	 ((or (symbolp (car l)) (not (eq (caaar l) 'mlist)))
	  (parse-tay-args1 (list (car l) ($ratdisrep (cadr l)) (caddr l)))
	  (parse-tay-args (cdddr l)))
	 ((do ((l (cddar l) (cdr l)))
	      ((null l) () )
	     (and (or (mnump (car l)) (eq (car l) '$inf))
		  (return 't)))
	  (parse-tay-args1 (cdar l))
	  (parse-tay-args (cdr l)))
	 (t (parse-tay-args2 (list (car l) (cadr l) (caddr l)))
	    (parse-tay-args (cdddr l)))))

(defun parse-tay-args1 (l)
   (if ($listp (car l)) (parse-tay-args2 l)
      (let ((v (car l))
	    (pt ($ratdisrep (cadr l)))
	    (ord (tay-order (caddr l)))
	    (switches (make-switch-list (cdddr l))))
	 (push (list v ord pt switches) tlist))))

(defun parse-tay-args2 (l)
  (let ((label (gensym))
	(vs (cdar l))
	(pts (make-long-list (if ($listp (cadr l))
				 (copy-list (cdadr l))
				 (ncons (ratdisrep (cadr l))))))
	(ord (caddr l))
	(switches (make-switch-list (cdddr l)))
	(lcm 1)
	(max 0))
    (if (atom ord)
	(setq lcm ord max ord ord (make-long-list (ncons ord)))
	(do ((a vs (cdr a))
	     (l (cdr ord) (cdr l)))
	    ((null a) (setq ord (cdr ord)))
	  (cond ((not l) (merror "PARSE-TAY-ARGS2: ran out of truncation levels."))
		(t (setq lcm (lcm lcm (car l)) max (max max (car l)))))))
    (push (list label (tay-order max) 0
		(ncons (list 'multivar lcm vs)))
	  tlist)
    (do ((vl vs (cdr vl))
	 (ordl ord (cdr ordl))
	 (ptl pts (cdr ptl)))
	((null vl) )
      (cond ((not ptl) (merror "PARSE-TAY-ARGS2: ran out of matching points of expansion."))
	    (t
	     (push
	      (list (car vl) (tay-order (car ordl)) (car ptl)
		    (cons (list 'multi label (timesk lcm (expta (car ordl) -1))) switches))
	      tlist))))))

(defun make-switch-list (l)
  (mapcar #'(lambda (q) (cons q t)) l))

(defun make-long-list (q)
  (nconc q q))

;;; This checks to ensure that there isn't more than one set of multi-
;;; dependent variables with different orders of expansion, e.g.
;;; taylor(u+v+x+y,[u,v],0,[2,3],[x,y],0,[5,7]) is one.

(defun ratwtsetup (l)
   (do ((l l (cdr l)) (a) (sw))
       ((null l) )
      (when (setq a (switch 'multivar (car l)))
	 (do ((ll (cadr a) (cdr ll)))
	     ((null ll) )
	    (cond ((equal (cadr (switch 'multi (get-datum (car ll)))) 1) )
		  (sw (merror (intl:gettext "taylor: multiple dependent variables must all have the same order of expansion.")))
		  ('t (setq sw 't) (return 't)))))))

(defmvar $taylor_order_coefficients t
 "When `true', coefficients of taylor series will be ordered canonically.")

(defun taylor1 (e tlist)
  (declare (special *within-srf?* ))
  (setq tlist (tlist-merge (nconc (find-tlists e) tlist)))
  (prog ($zerobern $simp $algebraic genpairs varlist tvars sing-tvars
	 log-1 log%i ivars key-vars ans full-log last-exp
	 mainvar-datum zerolist taylor_simplifier least_term? tvar-limits 
         genvar)
	(setq tlist (mapcan #'(lambda (d)
				(if (tvar? (datum-var d))
				    (ncons d)
				    (progn
				      (push d sing-tvars)
				      () )))
			    tlist))
	(setq $zerobern t $simp t $algebraic t last-exp e least_term? 't
	      log-1 '((%log simp) -1) log%i '((%log simp) $%i)
	      tvars (mapcar 'car tlist) varlist (copy-list tvars))
	 (when $taylor_simplifier
	    (setq taylor_simplifier
		  (if (fboundp $taylor_simplifier) $taylor_simplifier
		     'taylor_simplifier_caller)))
	;; Ensure that the expansion points don't depend on the expansion vars.
	;; This could cause an infinite loop, e.g. taylor(x,x,x,1).
	(do ((tl tlist (cdr tl)))
	    ((null tl) )
	   (unless (mfree (exp-pt (car tl)) tvars)
	      (merror (intl:gettext "taylor: attempt to expand ~M at a point depending on ~M.") e (caar tl))))
	;; This drastic initialization ensures that ALGEBRAIC, TELLRAT, DISREP,
	;; etc. prop's are removed from our gensyms. RATSETUP does not appear
	;; to do this correctly, e.g. see ASB's bug of 1/10/83 (MQUERY 17).
	(mapc #'(lambda (g) (setf (symbol-plist g) nil)) genvar)
	(ratsetup varlist genvar)
	(when (and $taylor_order_coefficients (not *within-srf?*)) (newvar e))
	(orderpointer varlist)
	(maplist #'(lambda (q g)
		     (push (cons (car g) (car q)) key-vars)
		     (let ((data (get-datum (car q))))
			(rplaca q (transform-tvar (car q) data))
			(push (cons (car g) (car q)) ivars)
			;(setf (data-gvar-o data)
			;      (cons (car g) (valget (car g))))
			(rplacd (cdddr data)
				(cons (car g) (valget (car g))))))
		 (do ((v varlist (cdr v)))
		     ((eq (car v) (car tvars)) v))
		 (do ((v varlist (cdr v))
		      (g genvar (cdr g)))
		     ((eq (car v) (car tvars)) g)))
	(setq genpairs (mapcar #'(lambda (y z)
				   (putprop z y 'disrep)
				   (cons y (cons (pget z) 1)))
			       varlist genvar))
	(ratwtsetup tlist)
	(setup-multivar-disrep () )
	(setq mainvar-datum (car (last tlist)))
	(mapc #'(lambda (d) (adjoin-sing-datum d)) sing-tvars)
	(setq ans (catch 'tay-err (taylor3 e)))
	(return
	 (if (atom (car ans)) (tay-error (car ans) (cadr ans)) ans))))

(defun transform-tvar (var data)
   (if (not (tvar? var)) var
      (cond ((and (signp e (exp-pt data)) (null (switch '$asymp data)))
	     var)	;Simple case
	    ((member (exp-pt data) '($inf infinity) :test #'eq)
	     (m^ var -1))
	    ((eq (exp-pt data) '$minf)
	     (m- (m^ var -1)))
	    ((let ((temp (m- var (exp-pt data))))
		(if (switch '$asymp data) (m^ temp -1) temp))))))

(defun taylor_simplifier_caller (exp)
   (mcall $taylor_simplifier exp))

(defun taylor_simplify_recurse (ps)
   (if (pscoefp ps) (taylor2 (funcall taylor_simplifier (rcdisrep ps)))
      (let ((datum (ps-data ps)) (var () ))
	 ;; We must treat multivars like 1, since they'll reappear again
	 ;; when we call taylor2 on their disrep'd coeff's.
	 (if (switch 'multivar datum)
	     (setq datum '())
	     (progn
	       (setq var (getdisrep (gvar-o ps)))
	       ;; Don't push pw's < 0, else constants will be truncated
	       (push-pw datum (emax (trunc-lvl ps) (rczero)))))
	 (do ((terms (terms ps) (n-term terms))
	      (ans (rczero) (psplus (if (null datum)
					 (taylor_simplify_recurse (lc terms))
				       (pstimes (taylor_simplify_recurse
						 (lc terms))
	 ;; Don't do
	 ;;    (taylor2 (funcall taylor_simplifier
	 ;;			  (m^ var (edisrep (le terms)))))
	 ;; causes terms to be lost when inverting. E.g.
	 ;; taylor(log(1+exp(-1/x)),x,0,5) calls psexpt(<exp(1/x)^3>...3,-1)
	 ;; which must return a series good to 3+3(-1-1)=-3 which, when added
	 ;; to other terms will truncate them to degree -3 also.
					  (if (ezerop (le terms)) (rcone)
					     (make-ps ps
						(ncons
						 (term (le terms) (rcone)))))))

				    ans)))
	     ((null terms)
	      (when datum (pop-pw datum))
	      ans)))))

(defun push-pw (datum pw)
   (push pw (trunc-stack datum))
   ;; When changing the truncation on an internal multivar we must also
   ;; propagate the change to all var's which depend upon it. See WGD's
   ;; bug report of 9/15/82 which exhibits the necessity of this.
   (when (setq datum (switch 'multivar datum))
      (do ((vars (cadr datum) (cdr vars)))
	  ((null vars) )
	 (push pw (trunc-stack (get-datum (car vars)))))))

(defun pop-pw (datum)
   (pop (trunc-stack datum))
   ;; See the comment above in push-pw; here we must undo the propagation.
   (when (setq datum (switch 'multivar datum))
      (do ((vars (cadr datum) (cdr vars)))
	  ((null vars) )
	 (pop (trunc-stack (get-datum (car vars)))))))

(defun setup-multivar-disrep (mrat?)
   (let ((varlist varlist) (genvar genvar) (multivars () ))
      (when mrat?
	 (setq varlist (mrat-varlist mrat?) genvar (mrat-genvar mrat?)))
      (mapc #'(lambda (datum)
		 (when (switch 'multivar datum)
		    (push (car datum) multivars)
		    ;; All genvar's corresponding to internally generated
		    ;; multivars must "disappear" when disrep'd. If this
		    ;; were not done then disrep'ing gx*gt would give x*t
		    ;; which, upon, re-tayloring would give (gx*gt)*gt,
		    ;; where t is the internal multivar for x, and gt, gx
		    ;; are their genvars. An example where this would occur is
		    ;; taylor(sin(x+y),[x],0,f1,[y],0,1).
		    (putprop (int-gvar datum) 1 'disrep)))
	    (if mrat? (mrat-tlist mrat?) tlist))
      ;; Here we must substitute 1 for any genvars which depend on multivars.
      ;; For example, taylor(x^n,[x],0,0) generates a multivar^n.
      (when multivars
	 (do ((expl varlist (cdr expl))
	      (gvarl genvar (cdr gvarl)))
	     ((null expl) )
	    (unless (mfree (car expl) multivars)
	       (putprop (car gvarl) 1 'disrep))))))

;; An example showing why this flag is need is given by
;; taylor(-exp(exp(-1/x)+2/x),x,0,-1). Without it, tstimes and
;; taylor_simplify_recurse end up trunc'ing the -1.

(defvar trunc-constants? 't)

(defun taylor3 (e)
   (cond ((mbagp e) (cons (car e) (mapcar #'taylor3 (cdr e))))
	 ((and (null tlist) (not (eq exact-poly 'user-specified)))
	  (xcons (prep1 e)
		 (list 'mrat 'simp varlist genvar)))
	 (t (xcons (if (null taylor_simplifier)
		       (taylor2 e)
		       (progn
			 (setq e (taylor2 e))
			 (let ((exact-poly () ) (trunc-constants? () ))
			   (taylor_simplify_recurse e))))
		   (list 'mrat 'simp varlist genvar tlist 'trunc)))))

(defun find-tlists (e) (let (*a*) (findtl1 e) *a*))

(defun findtl1 (e)
  (cond ((or (atom e) (mnump e)) )
	((eq (caar e) 'mrat)
	 (when (member 'trunc (car e) :test #'eq)
	    (push (mapcar #'copy-tree (mrat-tlist e)) *a*)))
	(t (mapc #'findtl1 (cdr e)))))

(defun tlist-merge (tlists)
  (do ((tlists tlists (cdr tlists)) (tlist () ))
      ((null tlists) (nreverse tlist))
    (do ((a_tlist (car tlists) (cdr a_tlist)) (temp nil))
	((null a_tlist) )
      (if (null (setq temp (get-datum (datum-var (car a_tlist)) t)))
	  (if (prog2 (setq temp (car a_tlist))
		  (or (tvar? (datum-var temp))
		      (member (caar (datum-var temp)) '(mexpt %log) :test #'eq)))
	      (push (list (datum-var temp) (trunc-stack temp)
			  (exp-pt temp) (switches temp))
		    tlist)
	      (merror (intl:gettext "taylor: ~M cannot be a variable.") (datum-var temp)))
	  (progn
	    (if $maxtayorder
		;; We must take the max truncation level when $maxtayorder
		;; is T, cf. JPG's bug of 9/15/82.
		(when (e> (current-trunc (car a_tlist)) (current-trunc temp))
		  (setf (current-trunc temp) (current-trunc (car a_tlist))))
		(unless (e> (current-trunc (car a_tlist)) (current-trunc temp))
		  (setf (current-trunc temp) (current-trunc (car a_tlist)))))
	    (unless (alike1 (exp-pt temp) (exp-pt (car a_tlist)))
	      (merror (intl:gettext "taylor: cannot combine expressions expanded at different points.")))
	    (setf (switches temp)
		  (union* (switches temp) (switches (car a_tlist)))))))))

(defun compattlist (list)
   (do ((l list (cdr l)))
       ((null l) t)
      (or (alike1 (exp-pt (get-datum (datum-var (car l)))) (exp-pt (car l)))
	  (return () ))))

(defun taylor2  (e)
 (let ((last-exp e))	    ;; lexp-non0 should be bound here when needed
  (cond ((assolike e tlist) (var-expand e 1 () ))
	((or (mnump e) (atom e) (mfree e tvars))
	 (if (or (e> (rczero) (current-trunc mainvar-datum))
		 (lim-zerop e))
	     (pszero (data-gvar-o mainvar-datum)
		     (current-trunc mainvar-datum))
	    (if (and taylor_simplifier (not (atom e)))
		(let ((e-simp (prep1 (funcall taylor_simplifier e))))
		   (when (and (rczerop e-simp) (not (member e-simp zerolist :test #'eq)))
		      (push e zerolist))
		   e-simp)
	       (prep1 e))))
	((null (atom (caar e))) (merror "TAYLOR2: internal error."))
	(($taylorp e)
	 (if (and (compatvarlist varlist (mrat-varlist e)
				 genvar (mrat-genvar e))
		  (compattlist (mrat-tlist e)))
	     (pstrunc (cdr e))
	    (let ((exact-poly () )) (re-taylor e))))
	((eq (caar e) 'mplus) (tsplus (cdr e)))
	((eq (caar e) 'mtimes) (tstimes (cdr e)))
	((eq (caar e) 'mexpt) (tsexpt (cadr e) (caddr e)))
	((eq (caar e) '%log) (tslog (cadr e)))
	((and (or (known-ps (caar e)) (get (caar e) 'tay-trans))
	      (not (member 'array (cdar e) :test #'eq))
	      (try-expansion (if (cddr e) (cdr e) (cadr e))
			     (caar e))) )
	((and (mqapplyp e)
	      (cond ((get (subfunname e) 'spec-trans)
		     (funcall (get (subfunname e) 'spec-trans) e))
		    ((known-ps (subfunname e))
		     (try-expansion (caddr e) (cadr e))))) )
	((and (member (caar e) '(%sum %product) :test #'eq)
	      (mfreel (cddr e) tvars)) 
	 (tsprsum (cadr e) (cddr e) (caar e)))
	((eq (caar e) '%derivative) (tsdiff (cadr e) (cddr e) e))
	((or (eq (caar e) '%at)
	     (do ((l (mapcar 'car tlist) (cdr l)))
		 ((null l) t)
		 (or (free e (car l)) (return ()))))
	 (newsym e))
	(t (let ((exact-poly () ))	; Taylor series aren't exact
	      (taylor2 (diff-expand e tlist)))))))

(defun compatvarlist (a b c d)
   (cond ((null a) t)
	 ((or (null b) (null c) (null d)) () )
	 ((alike1 (car a) (car b))
	  (if (not (eq (car c) (car d))) ()
	     (compatvarlist (cdr a) (cdr b) (cdr c) (cdr d))))
	 (t (compatvarlist a (cdr b) c (cdr d)))))


(defun re-taylor (mrat)
   (let ((old-tlist (mrat-tlist mrat)) (old-varlist (mrat-varlist mrat))
	 (old-genvar (mrat-genvar mrat)) old-ivars)
     (declare (special old-tlist old-ivars))
      ;; Put back the old disrpes so rcdisrep's will work correctly.
      (mapc #'(lambda (g v) (putprop g v 'disrep)) old-genvar old-varlist)
      (setup-multivar-disrep mrat)
      (setq old-ivars (mapcar #'(lambda (g v) (cons g v))
			      old-genvar old-varlist))
      (prog1 (re-taylor-recurse (mrat-ps mrat))
	     ;; Restore the correct disreps.
	     (mapc #'(lambda (g v) (putprop g v 'disrep)) genvar varlist)
	     (setup-multivar-disrep () ))))

(defun re-taylor-recurse (ps)
  (declare (special old-tlist old-ivars))
   (if (not (psp ps)) (taylor2 (rcdisrep ps))
      (let (var (datum () ))
	 (setq var (cdr (assoc (gvar ps) old-ivars :test #'eq)))
	 ;; We must treat multivars like 1, since they'll reappear again
	 ;; when we call taylor2 or var-expand below.
	 (if (switch 'multivar (assoc var old-tlist :test #'equal))
	     (setq var () )
	    (when (setq datum (var-data var))
	       (push-pw datum (trunc-lvl ps))))
	 (prog1
	  (do ((terms (terms ps) (n-term terms))
	       (ans (rczero)
		    (psplus (if (null var) (re-taylor-recurse (lc terms))
			       (pstimes (re-taylor-recurse (lc terms))
					(if datum
					    (var-expand (car datum)
							(edisrep (le terms))
							() )
					   (taylor2
					    (m^t var (edisrep (le terms)))))))
			    ans)))
	      ((null terms) ans))
	  (when datum (pop-pw datum))))))

(defun var-expand (var exp dont-truncate?)
  (let (($keepfloat) ($float) (modulus))
     (setq exp (prep1 exp)))		;; exp must be a rational integer
  (let ((temp (get-datum var 't)))
     (cond ((null temp) (merror "VAR-EXPAND: invalid call."))
	   ((member (exp-pt temp) '($inf $minf $infinity) :test #'eq)
	    (cond ((switch '$asymp temp)
		     (merror (intl:gettext "taylor: cannot create an asymptotic expansion at infinity.")))
		    ((e> (setq exp (rcminus exp)) (current-trunc temp))
		     (rczero))
		    (t (make-ps (int-var temp)
				(ncons (if exact-poly (inf) (current-trunc temp)))
				(ncons (term exp
					     (if (eq (exp-pt temp) '$minf)
						 (rcmone)
					       (rcone))))))))
	   ;; multivar expansion does not work at infinity, so
	   ;; expansion at infinity is handled by above clause even if doing multivar.
	   ((switch 'multi temp)	;; multivar expansion
	    (psexpt (psplus
		     ;; The reason we call var-expand below instead of taylor2
		     ;; is that we must be sure the call is not truncated to
		     ;; 0 which would cause an error in psexpt if exp < 0.
		     ;; For example, this occured in TAYLOR(X^2/Y,[X,Y],0,2).
		     (pstimes
		      ;; Must ensure that we get back a series truncated
		      ;; to at least what is specified by tlist. This means
		      ;; we'll have to push-pw unless exp>0 since psexpt'n
		      ;; kills (exp-1) terms. The bug that discovered this
		      ;; is taylor(li[2](x+1/2)/x,[x],0,0) missing 2*log(2).
		      (if (not (e> exp (rczero)))
			  (let-pw (get-datum (car (switch 'multi temp)))
				  (e+ (current-trunc temp) (e- (e1- exp)))
			     (var-expand (car (switch 'multi temp)) 1 't))
			 (var-expand (car (switch 'multi temp)) 1 't))
		      (cons (list (int-gvar temp) 1 1) 1))
		     (taylor2 (exp-pt temp)))
		    exp))
	   ((signp e (exp-pt temp))
	    (let ((exp>trunc? () ))
	       (if (and (e> exp (current-trunc temp)) (setq exp>trunc? 't)
			(not dont-truncate?))
		   (rczero)
		  (make-ps (int-var temp)
			   (ncons (if exact-poly (inf)
				     (if exp>trunc? exp (current-trunc temp))))
			   (ncons (term (if (switch '$asymp temp) (rcminus exp)
					   exp)
					(rcone)))))))
	   (t (psexpt (psplus
			 (make-ps (int-var temp)
				  (ncons (if exact-poly (inf) (current-trunc temp)))
				  (ncons (term (if (switch '$asymp temp)
						   (rcmone)
						   (rcone))
					       (rcone))))
			 (taylor2 (exp-pt temp)))
			exp)))))

(defun expand (arg func)
   (or (try-expansion arg func) (exp-pt-err)))

(defun try-expansion (arg func)
  (prog (funame funord fun-lc argord psarg arg-trunc temp exact-poly)
     ;; We bind exact-poly to () since we dont want psexpt retaining
     ;; higher order terms when subst'ing into series (which aren't exact).
     ;; Try diff-expanding unknown subsripted functions.
     (unless (or (atom func) (known-ps (caar func)))
       (taylor2 (diff-expand `((mqapply) ,func ,arg) tlist)))
     (when (setq temp (get (setq funame (oper-name func)) 'tay-trans))
       (return (funcall temp arg func)))
     (let ((lterm (getfun-lt func)))
       (setq funord (e lterm) fun-lc (c lterm)))
     begin-expansion
     (when (rczerop (or psarg (setq psarg (get-lexp arg (rcone) () ))))
       (if (e> (rczero) funord)
	   (if (rczerop (setq psarg (get-lexp arg (rcone) 't)))
	       (tay-depth-err)
	       (go begin-expansion))
	   (return (cond ((setq temp (assoc funame tay-pole-expand :test #'eq))
			  (funcall (cdr temp) arg psarg func))
			 ((rczerop funord) fun-lc)
			 (t (rczero))))))
     (when (pscoefp psarg) (setq psarg (taylor2 arg)))
     (when (pscoefp psarg)
       (return
	 (cond ((null (mfree (rdis psarg) tvars))
		(symbolic-expand arg psarg func))
	       ((setq temp (assoc funame tay-pole-expand :test #'eq))
		(funcall (cdr temp) arg psarg func))
	       (t (prep1 (simplify
			  (if (atom func) `((,func) ,(rcdisrep psarg))
			      `((mqapply) ,func ,(rcdisrep psarg)))))))))
     (when (e> (rczero) (setq argord (ps-le psarg)))
       (cond ((not (member funame '(%atan %asin %asinh %atanh) :test #'eq))
	      (if (e> (rczero) (ps-le* (setq psarg (get-lexp arg (rcone) 't))))
		  (essen-sing-err)
		  (go begin-expansion)))
	     (t
	      (if (and (eq funame '%atan)
		       (eq (coef-sign arg) '$neg))
		  (return (psplus (atrigh arg func) (taylor2 (m- '$%pi))))
		  (return (atrigh arg func))))))
     (setq temp (t-o-var (gvar psarg)))
     (when (e> (e* funord argord) temp) (return (rczero)))
     ;; the following form need not be executed if psarg is really exact.
     ;; The constant problem does not allow one to determine this now,
     ;; so we always have to execute this currently.
     ;; This really should be
     ;; (unless (infp (trunc-lvl psarg)) ... )
     ;; Likewise, the infp checks shouldn't be there; have to assume
     ;; nothing is exact until constant problem is fixed.
     (setq arg-trunc (if (and (not (infp (trunc-lvl psarg)))
			      (e= funord (rcone)))
			 temp
			 (e- temp (e* (e1- funord) argord)))
	   psarg (let-pw (get-datum (get-key-var (gvar psarg)))
			 arg-trunc
			 (if (or (infp (trunc-lvl psarg))
				 (e> arg-trunc (trunc-lvl psarg)))
			     (taylor2 arg)
			     (pstrunc psarg)))
	   ;; We must recalculate argord since pstrunc may have "picked"
	   ;; a coeff out of a constant monomial; e.g. this occurs in
	   ;; taylor(sin(x+y),x,0,0,y,0,1) where psarg is (Y+...)*X^0+...
	   ;; which truncates to Y+... of order 1.
	   argord (ps-le* psarg))
     (if (rczerop argord)
	 (cond ((member funame '(%atan %asin %asinh %atanh) :test #'eq)
		(return (atrigh arg func)))
	       ((setq temp (assoc funame const-exp-funs :test #'eq))
		(return (funcall (cdr temp) arg psarg func)))
	       ((rczerop (ps-le* (setq psarg (get-lexp arg (rcone) 't))))
		(return () ))		; Don't know an addition formula
	       (t (go begin-expansion)))
	 (return
	   (if (mono-term? (terms psarg))
	       (get-series func (current-trunc
				 (get-datum (get-key-var (gvar psarg))))
			   (gvar-o psarg) (ps-le psarg) (ps-lc psarg))
	       (progn
		 (setq temp (get-series func
					(e// temp argord) (gvar-o psarg)
					(rcone) (rcone)))
		 (cond ((not (psp temp)) temp)
		       (t (pscsubst1 psarg temp)))))))))

(defun symbolic-expand (ign psarg func) ; should be much stronger
  (declare (ignore ign))
  (prep1 (simplifya (if (atom func)
			`((,func) ,(rcdisrep psarg))
			`((mqapply) ,func ,(rcdisrep psarg)))
		    () )))

(defun expand-sing-trig? (arg func)
   (cond ((member func *pscirc :test #'eq) (tay-exponentialize arg func))
	 ((member func *psacirc :test #'eq) (atrigh arg func))
	 (t (essen-sing-err))))

(defun trig-const (a arg func)
       (let ((const (ps-lc* arg)) (temp (cdr (assoc func trigdisp :test #'eq))))
	    (cond ((and (pscoefp const)
			(member func '(%tan %cot) :test #'eq)
			(multiple-%pi a (srdis const) func)))
		  (temp (funcall temp (setq const (psdisrep const))
				 (m- a const)))
		  (t (tsexpt `((,(get func 'recip)) ,(srdis arg)) -1)))))

(defun multiple-%pi (a const func)
  (let (coef)
    (and (equal ($hipow const '$%pi) 1)
	 ($ratnump (setq coef ($ratcoef const '$%pi 1)))
	 (cond ((numberp coef) (expand (m- a const) func))
	       ((equal (caddr coef) 2)
		(psminus (expand (m- a const)
				 (cond ((eq func '%tan) '%cot)
				       ((eq func '%cot) '%tan)
				       (t (merror "MULTIPLE-%PI: internal error in Taylor expansion."))))))))))

(setq *pscirc '(%cot %tan %csc %sin %sec %cos %coth
		%tanh %csch %sinh %sech %cosh)

      *psacirc '(%acot %atan %acsc %asin %asec %acos %acoth
		       %atanh %acsch %asinh %asech %acosh))

(setq const-exp-funs
      `((%gamma . gam-const) ($psi . plygam-const)
	. ,(mapcar #'(lambda (q) (cons q 'trig-const)) *pscirc))

      trigdisp '((%sin . psina+b) (%cos . pscosa+b) (%tan . pstana+b)
		 (%sinh . psinha+b) (%cosh . pscosha+b) (%tanh . pstanha+b))

      tay-pole-expand '((%gamma . plygam-pole) ($psi . plygam-pole))

      tay-const-expand ; !these should be handled by symbolic-expand
		       ; be sure to change this with tay-exponentialize!
      (append (mapcar #'(lambda (q) (cons q 'tay-exponentialize)) *pscirc)
	      (mapcar #'(lambda (q) (cons q 'tay-exponentialize)) *psacirc)))

(mapc #'(lambda (q) (putprop q 'atrig-trans 'tay-trans))
      '(%acos %acot %asec %acsc %acosh %acoth %asech %acsch))

(defprop mfactorial factorial-trans tay-trans)

(defun factorial-trans (arg func)
  (declare (ignore func))
  (taylor2 `((%gamma) ,(m1+ arg))))

;;; Not done properly yet
;;;
;;; (defprop $BETA BETA-TRANS TAY-TRANS)

(defun psina+b (a b)
	(psplus (pstimes (expand a '%sin) (expand b '%cos))
		(pstimes (expand a '%cos) (expand b '%sin))))

(defun pscosa+b (a b)
	(psdiff (pstimes (expand a '%cos) (expand b '%cos))
		(pstimes (expand a '%sin) (expand b '%sin))))

(defun pstana+b (a b)
	(setq a (expand a '%tan) b (expand b '%tan))
	(pstimes (psplus a b)
		 (psexpt (psdiff (rcone) (pstimes a b))
			 (rcmone))))

(defun psinha+b (a b)
	(psplus (pstimes (expand a '%sinh) (expand b '%cosh))
		(pstimes (expand a '%cosh) (expand b '%sinh))))

(defun pscosha+b (a b)
	(psplus (pstimes (expand a '%cosh) (expand b '%cosh))
		(pstimes (expand a '%sinh) (expand b '%sinh))))

(defun pstanha+b (a b)
	(setq a (expand a '%tanh) b (expand b '%tanh))
	(pstimes (psplus a b)
		 (psexpt (psplus (rcone) (pstimes a b))
			 (rcmone))))

(defun atrig-trans (arg func)
  (taylor2
   (cond ((eq func '%acos)
	  `((mplus) ,half%pi ((mtimes) -1 ((%asin) ,arg))))

	 ((eq func '%acosh)
	  `((mtimes) -1 $%i ((mplus) ,half%pi ((mtimes) -1 ((%asin) ,arg)))))

	 (t
	  `((,(cdr (assoc func '((%acsc . %asin) (%asec . %acos)
				(%acot . %atan) (%acsch . %asinh)
				(%asech . %acosh) (%acoth . %atanh)) :test #'eq)))
	    ,(m^ arg -1))))))

(defun atrigh (arg func)
       (let ((full-log t) ($logarc t) (log-1 '((mtimes) $%i $%pi))
	     (log%i '((mtimes) ((rat) 1 2) $%i $%pi)))
	    (taylor2 (simplify `((,func) ,arg)))))

(defun tay-exponentialize (arg fun) ; !this should be in symbolic-expand!
       (let (($exponentialize t) ($logarc t))
	     (setq arg (meval `((,fun) ,arg))))
       (taylor2 arg))

(defun tsplus (l)
       (do ((l (cdr l) (cdr l))
	    (ans (taylor2 (car l))
		 (psplus ans (taylor2 (car l)))))
	   ((null l) ans)))

(defun ts-formula (form var pw)
   (let ((datum (get-datum (get-key-var (car var)))))
      (let-pw datum pw
	 (taylor2 (subst (get-inverse (car var)) 'sp2var form)))))

(defmacro next-series (l) `(cdadr ,l))

(defun tstimes-get-pw (l pw)
   (do ((l l (cdr l)) (vect))
       ((null l) pw)
      (setq pw (mapcar #'(lambda (pwq ple) (e+ pwq ple))
		       pw (setq vect (ord-vector (cdar l)))))
      (rplacd (car l) (cons (cdar l) vect))))

(defun tstimes-l-mult (a)
   (do ((l (cdr a) (cdr l)) ($maxtayorder t)
	(a (car a) (pstimes a (car l))))
       ((null l) a)))

(defun mzfree (e l)
   (do ((l l (cdr l)))
       ((null l) 't)
      (or (zfree e (car l)) (return () ))))

;;; The lists posl, negl and  zerl have the following format:
;;;
;;;   ( (<expression> <expansion> <order vector>) . . . )

(defun tstimes (l)
  (*bind* ((funl) (expl) (negl) (zerl) (posl)
	   (pw) (negfl) (temp) (fixl (rcone)))
    (dolist (fun l)			;; find the exponentials
       (if (mexptp fun)
	   (push (if (free (caddr fun) (car tvars)) fun
		    `((mexpt) $%e ,(m* (caddr fun)
				       `((%log) ,(cadr fun)))))
		 expl)
	  (push fun funl)))
    (when expl
       (setq expl (tsexp-comb expl))		;; simplify exps
       (setq expl (tsbase-comb expl)))		;; and again
    (setq l (nconc expl funl))			;; now try expanding
    (let ((trunc-constants? () ))
       (setq expl (cons 0 (mapcar #'(lambda (exp)
				       (cons exp (taylor2 exp)))
				  l))) )
    ;; EXPL is now of the form (0 ( <form> . <taylor2(form)> ) ...)
    ;; L points behind the cons considered for destructive updating.
    (do ((l expl) (tem))
	((null (cdr l)) )
       (cond ((rczerop (cdadr l))
	      ;; Consider taylor((a+1/x)*1/x,x,0,-2). Each factor will be on
	      ;; zerl. Each factor will also appear to have le = 0 since its
	      ;; series is 0, which would fool the get-pw routines below if
	      ;; they tried to handle this case. The easiest fix for now
	      ;; appears to be to always call get-lexp here, killing this:
	      (cond ;((null $maxtayorder)
		    ; (setq zerl (cons (cadr l) zerl))
		    ; (rplacd l (cddr l)))
		    ((rczerop (setq tem (get-lexp (caadr l) (rcone) ())))
		     (return (setq zerl 0)))
		    ('t (setq posl (cons (cons (caadr l) tem) posl))
			(rplacd l (cddr l)))))
	     ((pscoefp (cdadr l))
	      (cond ((mzfree (caadr l) tvars) ;must be zfree to permit ratfuns
		     (setq fixl (pstimes (cdadr l) fixl))
		     (rplacd l (cddr l)))
		    ((setq zerl (cons (cadr l) zerl))
		     (rplacd l (cddr l)))))
	     ((rczerop (ps-le (cdadr l)))
	      (setq zerl (cons (cadr l) zerl))
	      (rplacd l (cddr l)))
	     ((e> (ps-le (cdadr l)) (rczero))
	      (setq posl (cons (cadr l) posl))
	      (rplacd l (cddr l)))
	     ('t (setq l (cdr l)))))
    (when (equal zerl 0) (return (rczero)))
    (setq negl (cdr expl) temp (ord-vector fixl))
    (mapcar #'(lambda (x) (and (e> (rczero) x) (setq negfl t))) temp)
    (tstimes-get-pw zerl temp)
    (setq pw (tstimes-get-pw posl (tstimes-get-pw negl temp)))
    (if (or negl negfl)
	(setq posl
	      (mapcar #'(lambda (x)
			   (prog2 (mapcar #'(lambda (datum lel pwl)
					       (push-pw datum
						  (e+ (current-trunc datum)
						      (e- lel pwl))))
					  tlist (cddr x) pw)
				  (taylor2 (car x))
				  (mapcar #'(lambda (datum) (pop-pw datum))
					  tlist)))
		      (nconc posl zerl negl)))
       (setq posl (nconc (mapcar 'cadr posl) (mapcar 'cadr zerl)
			 (mapcar 'cadr negl))))
    (setq posl (tstimes-l-mult posl))
    (let ((ans (cond ((null fixl) posl)
		     ((null posl) fixl)
		     ('t (pstimes fixl posl)))))
       (if $maxtayorder ans (pstrunc ans)))))

;;; This routine transforms a list of exponentials as follows:
;;;
;;;	a^c*b^(n*c) ===> (a*b^n)^c,   where n is free of var.
;;;
;;; This transformation is only applicable when c is not free of var.

(defun tsexp-comb (l)	;; ***** clobbers l *****
   (setq l (cons '* l))
   (do ((a l) (e))	;; updated by a rplacd or cdr.
       ((null (cddr a)) (cdr l))	;; get rid of the *
      (rplaca (cddadr a) (setq e ($ratsimp (caddr (cadr a)))))
      ;; Must delete e^0 lest we divide by the 0 below. RWG's byzero bug
      ;; of 3/1/78 used to cause this.
      (if (equal e 0) (rplacd a (cddr a))
	 (if (mfree (caddr (cadr a)) tvars) (pop a)
	    (do ((b (cddr a) (cdr b)) (n))
		((null b) (setq a (cdr a)))
	       (when (mfree (setq n ($ratsimp (m// (caddar b)
						   (caddr (cadr a)))))
			    tvars)
		  (rplaca b (list '(mexpt simp)
				  (m* (cadadr a)
				      (m^ (cadar b) n))	;; b^n
				  (caddr (cadr a))))
		  (rplacd a (cddr a))			;; delete a^c
		  (return () )))))))

;;; This routine transforms a list of exponentials as follows:
;;;
;;;	a^b*a^c ===> a^(b+c),
;;;
;;; this is only necessary when b and c depend on "var."

(defun tsbase-comb (l)		;;; *******clobbers l********
   (setq l (cons '* l))
   (do ((a l))			;;; updated by a rplacd or cdr
       ((null (cddr a)) (cdr l))
      (do ((b (cddr a) (cdr b)))
	  ((null b) (pop a))	;;; did not return early so pop.
	 (when (alike1 (cadar b) (cadadr a))
	    (rplaca b (m^ (cadar b) (m+ (caddar b) (caddr (cadr a)))))
	    (rplacd a (cddr a))
	    (return 't)))))

(defun tsexpt (b e)
   (cond ((and (atom b) (mnump e)
	       (get-datum b)
	       (not (eq (exp-pt (get-datum b)) '$minf)))
	  ;; one could remove this clause and let this case be handled by tsexpt1
	  (var-expand b e () ))
	 ((mfree e tvars) (tsexpt1 b e))
	 ((eq '$%e b) (tsexpt-red (list e)))
	 (t (tsexpt-red (list (list '(%log) b) e)))))

(defun tsexpt-red (l)
   (*bind* ((free) (nfree) (full-log) ($logarc t) (expt) (ps) (e)
	    (log-1 '((mtimes) $%i $%pi))
	    (log%i '((mtimes) ((rat) 1 2) $%i $%pi)))
	   (declare (special e))
    a  (do ((l l (cdr l)))
	   ((null l) )
	  (cond ((mtimesp (car l)) (setq l (append l (cdar l))))
		((mfree (car l) tvars) (push (car l) free))
		(t (push (car l) nfree))))
       (cond ((or (cdr nfree) (atom (car nfree))) )
	     ((eq (caaar nfree) '%log)
	      (return (tsexpt1 (cadar nfree) (m*l free))))
	     ((member (caaar nfree) *psacirc :test #'eq)
	      (setq l (ncons (simplifya	 ;; simplify after removing simp flag
			      (cons (ncons (caaar nfree)) (cdar nfree))
			      () ))
		    nfree (cdr nfree))
	      (go a)))
       ;; Must have truncs > 0 so that logs in the expt aren't trunc'd.
       ;; E.g, consider taylor(x^(x-1),x,0,-1).
       (tlist-mapc d (push-pw d (emax (current-trunc d) (rcone))))
       (setq ps (taylor2 (setq expt (m*l (append nfree free)))))
       (tlist-mapc d (pop-pw d))
       ;; Here we must account for the truncation gain or lossage that
       ;; is encountered in exp(c*log(x)+y) -> x^c*exp(y).
       (let ((c0 (if (pscoefp ps) ps (psterm (terms ps) (rczero))))
	     e^c0 ord-e^c0)
	  (unless (rczerop c0)
	     (setq ord-e^c0 (ord-vector (setq e^c0 (psexpt-fn c0))))
	     ;; Must emax with 0 so that new singular kernals won't be trunc'd
	     ;; e.g exp(1/x+...) to degree -2 should be exp(-1/x)+...
	     ;; Also try taylor(screwa,x,0,-2).
	     (mapc #'(lambda (d o) (push-pw d (emax (e- (current-trunc d) o)
						   (rczero))))
		   tlist ord-e^c0)
	     (setq ps (psdiff (taylor2 expt) c0)))
	  (setq ps (psexpt-fn ps))
	  (when e^c0
	     (tlist-mapc d (pop-pw d))
	     (setq ps (pstimes e^c0 ps)))
	  (pstrunc ps))))

;; Taylor's b^e, where e is independent of tvars.

(defun tsexpt1 (b e)
  (prog (s le pw tb)
     (setq e (let ((modulus () )) ; Don't mod exponents! See WGM's bug
	       (prog2 (mapcar		;  of 3/6/83 for an example.
		       #'(lambda (datum)
			   (push-pw datum
				    (emax (current-trunc datum) (rczero))))
		       tlist)
		   (taylor2 e)
		 (mapcar #'(lambda (datum) (pop-pw datum)) tlist)))
	   s (psfind-s e)
	   tb (taylor2 b)
	   pw (if (psp tb) (current-trunc (get-datum
					   (get-key-var (gvar tb))))
		  ;; Constant problem kludge.
		  (if (rczerop tb) (current-trunc (car tlist)) (rczero))))
     (if (floatp (car s))
	 (setq s (maxima-rationalize (quot (car s) (cdr s)))))
     ;; We must ensure that the lc is non-zero since it will be inverted in
     ;; psexpt.
     (setq tb (strip-zeroes tb 't))
     (cond ((rczerop tb)
	    (when (or ;; When 1 > s we need more terms since -le*(s-1) > 0.
		   (e> (rcone) s)
		   (and (e> (rczero) pw) (e> s (rcone))))
	      (setq tb (get-lexp b () 't)))
	    (setq le (ps-le* tb)))
	   ((psp tb) (setq le (ps-le tb)))
	   (t (return (rcexpt tb e))))
     (and (e> (e* s le) pw) (null $maxtayorder) (return (rczero)))
     (setq s (e- pw (e* le (e1- s))))
					;(setq le (increment-truncs tb))
     (return
       (psexpt
	(if (e> pw s)
	    (if $maxtayorder tb
		(pstrunc1 tb (list (cons (gvar tb) s))))
	    ;; because of constants not retaining info, have to
	    ;; just keep the constant here
	    (cond ((not (psp tb)) tb)
		  (t (let-pw (get-datum (get-key-var (gvar tb))) s (strip-zeroes (taylor2 b) 't)))))
	e))))

;;; the method of calculating truncation levels below is incorrect.
;;; (i.e. increment-truncs & decrement-truncs, also used above)
;;; Examples which exhibit this incorrectness are:
;;; taylor(log(sin(y)+x),x,0,2,y,0,1) is missing a y/6*x and -1/6*x^2
;;; taylor(log(sin(z)+sin(y)+x),x,0,f1,y,0,3,z,0,5) misses a z^5*y^3 term.

;;; TSLOG must find the lowest degree term in the expansion of the
;;; log arg, then expand with the orders of all var's in this low term
;;; incremented by their order in this low term. Note that this is
;;; only necessary for var's with ord > 0, since otherwise we have
;;; already expanded to a higher ord than required. Also we must
;;; not do this for var's with trunc < 0, since this may incorrectly
;;; truncate terms which should end up as logs.

(defun increment-truncs (ps)
   (do ((ps ps (ps-lc ps)) (trunc (t-o-var (gvar ps))) (data () ))
       ((pscoefp ps) data)
      (when (e> (ps-le ps) (rczero))
	 (push (assoc (get-key-var (gvar ps)) tlist :test #'eq) data)
	 (push-pw (car data) (e+ (e* (e+ trunc (rctwo)) (ps-le ps))
				 (current-trunc (car data))))
	 (setq trunc (e+ trunc (current-trunc (car data))))
	 )))

(defun decrement-truncs (data)
   (mapc #'(lambda (data) (pop-pw data)) data))

(defun tslog (arg)
  (let ((psarg (taylor2 arg)) datum)
   (when (rczerop psarg) (setq psarg (get-lexp arg () 't)))
   ;; We must ensure that the lc is non-zero since it will be inverted in pslog
   (setq psarg (strip-zeroes psarg 't))
   (do ((ps psarg (ps-lc ps)) (shift (rcone) (e* shift (rctwo))))
       ((pscoefp ps)
	(when datum
	   (when (rczerop (setq psarg (taylor2 arg)))
	      (setq psarg (get-lexp arg () 't)))
	   (mapc #'(lambda (data) (pop-pw data)) datum))
	(pslog psarg))
      (push (get-datum (get-key-var (gvar ps))) datum)
      (if (and (e> (ps-le ps) (rczero))
	       (e> (current-trunc (car datum)) (rczero)))
	  (push-pw (car datum) (e+ (e* shift (ps-le ps))
				   (current-trunc (car datum))))
	 (pop datum)))))

;; When e-start is non-null we start expanding at order e-start, ... , 2^m,
;; then 2^m*pow, instead of the normal sequence pow, ... , 2^m*pow
;; (where m = $taylordepth, pow = ord of var). This is done because it is
;; usually much more efficient for large, non-trivial expansions when we only
;; want the lowest order term.

(defun get-lexp (exp e-start zerocheck?)
  (if (equal exp 0)
      (if zerocheck?
	  (tay-depth-err)
	  (rczero))
      (progn
	(tlist-mapc d (push-pw d (or e-start (emax (orig-trunc d) (rcone)))))
	(do ((psexp) (i (1+ $taylordepth) (1- i)))
	    ((signp e i)
	     (tlist-mapc d (pop-pw d))
	     (if zerocheck?
		 (tay-depth-err)
	         (progn
		   (unless silent-taylor-flag (zero-warn exp))
		   (rczero))))
	  (declare (fixnum i))
	  (cond ((and (rczerop (setq psexp (if zerocheck?
					       (strip-zeroes (taylor2 exp) 't)
					       (taylor2 exp))))
		      (not (member exp zerolist :test #'eq))) )
		;; Info not needed yet.
		;; ((and lexp-non0 (rczerop (le (terms psexp)))
		;;       (mono-term? (terms psexp))))
		(t (tlist-mapc d (pop-pw d))
		   (return psexp)))
	  (cond ((and (= i 1) e-start)
		 (setq e-start () i 2)
		 (tlist-mapc d (push-pw d (prog1 (e* (orig-trunc d) (current-trunc d))
					    (pop-pw d)))))
		(t (tlist-mapc d (push-pw d (prog1 (e* (rctwo) (current-trunc d))
					      (pop-pw d))))))))))

(defun 1p (x)
  (or (equal x 1) (equal x 1.0)))

(defun [max-trunc] ()
   (do ((l tlist (cdr l)) (emax (rczero)))
       ((null l) (1+ (truncate (car emax) (cdr emax))))
      (when (e> (current-trunc (car l)) emax)
	 (setq emax (orig-trunc (car l))))))

(defun tsprsum (f l type)
  (if (mfree f tvars) (newsym f)
      (let ((li (ncons (car l))) (hi (caddr l)) (lv (ncons (cadr l))) a aa
	    ($maxtayorder () ));; needed to determine when terms are 0
	(if (and (numberp (car lv)) (numberp hi) (> (car lv) hi))
	    (if (eq type '%sum) (taylor2 0) (taylor2 1))
	    (progn
	      (if (eq type '%sum) (setq type '()))
	      (do ((m (* ([max-trunc]) (ash 1 $taylordepth)))
		   (k 0 (1+ k))
		   (ans (taylor2 (maxima-substitute (car lv) (car li) f))))
		  ((equal hi (car lv)) ans)
		(rplaca lv (m1+ (car lv)))
		;; A cheap heuristic to catch infinite recursion when
		;; possible, should be improved in the future
		(if (> k m) (exp-pt-err)
		    (setq a		;(mlet li lv (taylor2 (setq aa (meval f))))
			  (taylor2 (maxima-substitute (car lv) (car li) f))))
		(if type
		    (if (and (1p (car a)) (1p (cdr a)) (not (1p aa)))
			(return ans)
			(setq ans (pstimes a ans)))
		    (if (and (rczerop a) (not (signp e aa)))
			(return ans)
			(setq ans (psplus ans a))))))))))

(defun tsdiff (e l check)
	(*bind* ((n) (v) (u))
	      (do ((l l (cddr l)))
		  ((null l))
		  (if (and (atom (car l)) (numberp (cadr l))
			   (assoc (car l) tlist :test #'eq))
		      (setq n (cons (cadr l) n) v (cons (car l) v))
		      (setq u (cons (car l) (cons (cadr l) u)))))
	      (or n (return (prep1 check)))
	      (if u (setq e (meval (cons '($diff) (cons e l)))))
	      (setq l (mapcar #'(lambda (x) (get-datum x)) v))
	      (mapcar #'(lambda (datum pw)
			  (push-pw datum (e+ (current-trunc datum) (prep1 pw))))
		      l n)
	      (setq e (taylor2 e))
	      (mapc #'(lambda (datum) (pop-pw datum)) l)
	      (do ((vl v (cdr vl))
		   (nl n (cdr nl)))
		  ((null vl ) e)
		  (do ((i 1 (1+ i)))
		      ((> i (car nl)) )
		      (mapc #'(lambda (a b)
				(putprop a (prep1 (sdiff b (car v)))
					 'diff))
			    genvar varlist)
		      (setq e (psdp e))))))


(defun no-sing-err (x)			;; try to catch all singularities
  (let ((errorsw t))
    (declare (special errorsw))
    (let ((ans (catch 'errorsw (eval x))))
      (if (eq ans t) (unfam-sing-err) ans))))

;; evaluate deriv at location var=pt
;; if this results in division by zero, use unevaluated form of deriv 
;; in order to get series expansions such as
;; taylor(gamma_incomplete(1/2, x), x, 0, 5) ->
;; sqrt(%pi)+97*sqrt(x)/512+113*x^(3/2)/512-2207*x^(5/2)/5120
;;               +997*x^(7/2)/3072-5845*x^(9/2)/36864
(defun eval-deriv (deriv var pt)
  (let ((errorsw t))
    (declare (special errorsw))
    (let ((ans (catch 'errorsw (eval `(meval '(($at) ,deriv ((mequal) ,var ,pt)))))))
      (if (eq ans t) 
	  deriv
	ans))))

(defun check-inf-sing (pt-list) ; don't know behavior of random fun's @ inf
       (and (or (member '$inf pt-list :test #'eq) (member '$minf pt-list :test #'eq))
	    (unfam-sing-err)))

(defun diff-expand (exp l)		;l is tlist
  (check-inf-sing (mapcar (function caddr) l))
  (cond ((not l) exp)
	(t
	 (setq exp (diff-expand exp (cdr l)))
	 (do ((deriv (sdiff exp (caar l)) (sdiff deriv var))
	      (var (caar l))
	      (coef 1 (* coef (1+ cnt)))
	      (cnt 1 (1+ cnt))
	      (pt (exp-pt (car l)))
	      (lim (rcdisrep (current-trunc (car l))))
	      (ans (list (no-sing-err `(meval '(($at) ,exp ((mequal) ,(caar l) ,(exp-pt (car l)))))))
		   (cons `((mtimes) ((rat simp) 1 ,coef)
			   ,(eval-deriv deriv var pt)
			   ((mexpt) ,(sub* var pt) ,cnt))
			 ans)))
	     ((or (great cnt lim) (equal deriv 0)) (cons '(mplus) ans))))))

;;;		  subtitle disreping routines

(defun edisrep (e)
       (if (= (cdr e) 1) (car e) (list '(rat) (car e) (cdr e))))

(defun striptimes (a)
       (if (mtimesp a) (cdr a) (ncons a)))

(defun srdis (x)
   (let (($psexpand () )) ; Called only internally, no need to expand.
      ($ratdisrep
       (cons (list 'mrat 'simp varlist genvar tlist 'trunc)
	     x))))

(defun srdisrep (r)
   (let ((varlist (mrat-varlist r)) (genvar (mrat-genvar r)))
      (mapc #'(lambda (exp genv) (putprop genv exp 'disrep))
	    varlist genvar)
      (setup-multivar-disrep r)
      ;; This used to return 0 if psdisrep returned () but this is wrong
      ;; since taylor(false,x,0,0) would lose. If psdisrep really wants to
      ;; return () for 0 then we will probably find out soon.
      (if (eq $psexpand '$multi) (psdisexpand (cdr r))
	 (psdisrep (cdr r)))))

(defun psdisrep (p)
   (if (psp p)
       (psdisrep+ (psdisrep2 (terms p) (getdisrep (gvar-o p)) (trunc-lvl p))
		  (if (or $psexpand (trunc-lvl p)) '(mplus trunc)
		     '(mplus exact)))
      (rcdisrep p)))

(defun psdisrep^ (n var)
   ;; If var = () then it is an internal var generated in a multivariate
   ;; expansion so it shouldn't be displayed. If var = 1 then it probably
   ;; resulted from the substitution in srdisrep, so it depends on an
   ;; internal var and likewise shouldn't be displayed.
   (cond ((or (rczerop n) (null var) (equal var 1)) 1)
	 ((equal n (rcone)) var)
	 ((and ps-bmt-disrep (mexptp var) (equal (caddr var) -1))
	  (psdisrep^ (e- n) (cadr var)))
	 ('t `((mexpt ratsimp) ,var ,(edisrep n)))))

;;; There used to be a hack below that would print a series consisting
;;; of merely one term as exact polynomial (i.e. no trailing "..."'s).
;;; This is, of course, wrong but the problem with the fix is that
;;; now exact things like taylor(y*x,x,0,f1,y,0,1) will display like
;;; (y+...) x+... because of the problem with $MAXTAYORDER being internally
;;; bound to ()---which causes exact things to look inexact, such as
;;; x and y above. See the comment above taylor* for the $MAXTAYORDER problem.

(defun psdisrep+ (p plush &aux lowest-degree-term)
  (if;; An exact sum of one arg is just that arg.
   (and (null (cdr p)) (eq (cadr plush) 'exact))
   (car p)
   (progn
     ;; Since the DISPLAY package prints trunc'd sum's arguments
     ;; from right to left we must put the terms of any constant term
     ;; in decreasing order. Note that only a constant (wrt to the
     ;; mainvar) term can be a term which is a sum.
     (when (mplusp (setq lowest-degree-term (car (last p))))
       (rplacd lowest-degree-term (nreverse (cdr lowest-degree-term))))
     (cons plush p))))

(defun psdisrep* (a b)
	 (cond ((equal a 1) b)
	       ((equal b 1) a)
	       (t (cons '(mtimes ratsimp)
			(nconc (striptimes a) (striptimes b))))))

(defun psdisrep2 (p var trunc)
   (if (or $ratexpand $psexpand) (psdisrep2expand p var)
      (do ((a () (cons (psdisrep* (psdisrep (lc p)) (psdisrep^ (le p) var))
		       a))
	   (p p (cdr p)))
	  ((or (null p) (e> (le p) trunc)) a))))

(defun psdisrep2expand (p var)
   (do ((p p (cdr p))
	(l () (nconc (psdisrep*expand (psdisrep (lc p)) (psdisrep^ (le p) var))
		     l)))
       ((null p) l)))

(defun psdisrep*expand (a b)
  (cond ((equal a 1) (list b))
	((equal b 1) (list a))
	((null (mplusp a))
	 (list (cons '(mtimes ratimes) (nconc (striptimes a) (striptimes b)))))
	('t (mapcar #'(lambda (z) (psdisrep* z b))
		    (cdr a)))))


(defun psdisexpand (p)
  (let ((ans (ncons ())))
    (declare (special ans))		;used in pans-add
    (psdisexcnt p () (rczero))
    (setq ans
	  (nreverse
	   (mapcar #'(lambda (x) (cond ((not (cddr x)) (cadr x))
				       (t (cons '(mplus trunc) (cdr x)))))
		   (cdr ans))))
    (cond ((not (cdr ans)) (car ans))
	  (t (cons '(mplus trunc) ans)))))

(defun psdisexcnt (p l n)
  (if (psp p)
      (do ((var (getdisrep (gvar-o p))) (ll (terms p) (n-term ll)))
	  ((null ll) ())
	(if (rczerop (le ll)) (psdisexcnt (lc ll) l n)
	    (psdisexcnt (lc ll)
			(cons (psdisrep^ (le ll) var) l)
			(e+ (le ll) n))))
      (psans-add (cond ((not l) (rcdisrep p))
		       (t (psdisrep* (rcdisrep p)
				     (cond ((not (cdr l)) (car l))
					   (t (cons '(mtimes trunc) l))))))
		 n)))

(defun psans-add (exp n)
  (declare (special ans))	;bound in psdisexpand
  (do ((l ans (cdr l)))
      ((cond ((null (cdr l)) (rplacd l (ncons (list n exp))))
	     ((e= (caadr l) n) (rplacd (cadr l) (cons exp (cdadr l))))
	     ((e> (caadr l) n) (rplacd l (cons (list n exp) (cdr l))))))))

(defun srconvert (r)
  (cond ((not (atom (caadr (cdddar r))))
	 (cons (car r) (psdisextend (cdr r))))
	(t
	 (*bind* ((trunclist (cadr (cdddar r)))
		  (tlist)
		  (gps)
		  (temp)
		  (vs (caddar r))
		  (gens (cadddr (car r))))
		 (setq gps (mapcar #'cons gens vs))
		 (do ((tl (cdr trunclist) (cddr tl)))
		     ((null tl) (cons (list 'mrat 'simp vs gens tlist 'trunc) (srconvert1 (cdr r))))
		   (setq temp (cdr (assoc (car tl) gps :test #'eq)))
		   (cond ((null (member (car tl) (cdr trunclist) :test #'eq)))
			 ((mplusp temp) (merror "SRCONVERT: internal error."))
			 (t
			  (setq tlist
				(cons (list* temp (tay-order (cadr tl)) 0 nil
					     (cons (car tl) (symbol-value (car tl))))
				      tlist)))))))))

(defun srconvert1 (p)
  (cond ((not (member (car p) genvar :test #'eq)) p)
	(t
	 (do ((l (cdr p) (cddr l))
	      (a nil (cons (term (prep1 (car l)) (srconvert1 (cadr l))) a)))
	     ((null l)
	      (make-ps (cons (car p) (symbol-value (car p)))
		       (tay-order (oldget trunclist (car p))) a))))))

;;;		 subtitle error handling

(defun tay-error (msg exp)
  (if silent-taylor-flag (throw 'taylor-catch ())
      (if exp
	  (merror "taylor: ~A~%~M" msg exp)
	  (merror "taylor: ~A" msg))))

(defun exp-pt-err ()
       (tay-err (intl:gettext "unable to expand at a point specified in:")))

(defun essen-sing-err ()
       (tay-err (intl:gettext "encountered an essential singularity in:")))

(defun unfam-sing-err ()
       (tay-err (intl:gettext "encountered an unfamiliar singularity in:")))

(defun infin-ord-err ()
       (tay-err (intl:gettext "expansion to infinite order?")))

(defun tay-depth-err ()
       (tay-err (intl:gettext "'taylordepth' exceeded while expanding:")))

;;;		 Subtitle TAYLORINFO

(defun taylor-trunc (q)
  (setq q (current-trunc q))
  (cond ((null q) '$inf)
	((equal (cdr q) 1) (car q))
	(t `((rat) ,(car q) ,(cdr q)))))

(defun taylor-info (q)
  (let ((acc-var nil) (acc-pt nil) (acc-ord nil) (qk) (acc))
    (cond ((null q) nil)
	  (t
	   (setq qk (pop q))
	   (cond ((and (fourth qk) (consp (fourth qk)) (eq (caar (fourth qk)) 'multivar)) nil)
		 ((and (fourth qk) (consp (fourth qk)) (eq (caar (fourth qk)) 'multi))
		  (while (and (fourth qk) (consp (fourth qk)) (eq (caar (fourth qk)) 'multi))
		    (setq acc nil)
		    (push (taylor-trunc qk) acc-ord)
		    (push (exp-pt qk) acc-pt)
		    (push (datum-var qk) acc-var)
		    (setq qk (pop q)))
		  (push '(mlist) acc-ord)
		  (push '(mlist) acc-pt)
		  (push '(mlist) acc-var)
		  (setq q (taylor-info q))
		  (if (null q) (list acc-var acc-pt acc-ord) (append q (list acc-var acc-pt acc-ord))))

		 (t
		  (setq acc (if (and (fourth qk) (consp (fourth qk)) (eq '$asympt (caar (fourth qk))))
				(list '$asympt) nil))
		  (push (taylor-trunc qk) acc)
		  (push (exp-pt qk) acc)
		  (push (datum-var qk) acc)
		  (push '(mlist) acc)
		  (setq q (taylor-info q))
		  (if (null q) (list acc) (append q (list acc)))))))))

(defun $taylorinfo (x)
  (if (and (consp x) (member 'trunc (first x) :test #'eq))
      (cons '(mlist) (taylor-info (mrat-tlist x)))
      nil))


;;; Local Modes:
;;; Lisp let-pw Indent:2
;;; End:
