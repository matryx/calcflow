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

(macsyma-module algsys)

(load-macsyma-macros ratmac)

;;This is the algsys package.

;;It solves systems of polynomial equations by straight-forward
;;resultant hackery.  Other possible methods seem worse:
;;the Buchberger-Spear canonical ideal basis algorithm is slow,
;;and the "resolvent" method (see van der Waerden, section 79)
;;blows up in time and space.  The "resultant"
;;method (see the following sections of van der Waerden and
;;Macaulay's book - Algebraic Theory of Modular Systems) looks
;;good, but it requires the evaluation of large determinants.
;;Unless some hack (such as prs's for evaluating resultants of
;;two polynomials) is developed for multi-polynomial resultants,
;;this method will remain impractical.

;;Some other possible ideas:  Keeping the total number of equations constant,
;;in an effort to reduce extraneous solutions, or Reducing to a linear
;;equation before taking resultants.

(declare-top (special $algdelta $ratepsilon $algepsilon $keepfloat
		     varlist genvar *roots *failures $ratprint $numer $ratfac
		     $rnum $solvefactors $dispflag $breakup $rootsquad
		     *tvarxlist* errorsw $programmode *ivar* errset $polyfactor
		     bindlist loclist $float $infeval))

;;note if $algepsilon is too large you may lose some roots.

(defmvar $algdelta 1e-5 )

(defmvar $%rnum_list '((mlist))
  "Upon exit from ALGSYS this is bound to a list of the %RNUMS
	 which where introduced into the expression. Useful for mapping
	 over and using as an argument to SUBST.")

(defmvar $realonly nil "If t only real solutions are returned.")

(defmvar realonlyratnum nil
  "A REALROOTS hack for RWG.  Causes ALGSYS to retain rational numbers
  returned by REALROOTS when REALONLY is TRUE."
  in-core)

(defmvar $algexact nil "If t ALGSYS always calls SOLVE to try to MAXIMA-FIND exact
			solutions.")

(defmvar algnotexact nil
  "A hack for RWG for univariate polys.  Causes SOLVE not to get called
  so that sqrts and cube roots will not be generated."
  in-core)

(defmacro merrset (l)
  `(let ((errset 'errbreak1) (unbind (cons bindlist loclist)) val)
     (setq val (errset ,l nil))
     (when (null val) (errlfun1 unbind))
     val))

(defmfun $algsys (lhslist varxlist &aux varlist genvar)
  ;;  (declare (special varxlist)) ;;??
  (setq $%rnum_list (list '(mlist)))
  (cond ((not ($listp lhslist))
	 (merror (intl:gettext "algsys: first argument must be a list; found ~M") lhslist))
	((not ($listp varxlist))
	 (merror (intl:gettext "algsys: second argument must be a list; found ~M") varxlist)))
  (let ((tlhslist nil) (*tvarxlist* nil) (solnlist nil) ($ratprint nil)
        ;; GCL seems to read 1e-7 as zero, but only when compiling. Incantations
        ;; based on 1d-7, 1l-7 etc. don't seem to make any difference.
	($ratepsilon #-gcl 1e-7
	             #+gcl (float 1/10000000))
	($keepfloat nil)
	(varlist (reverse (cdr varxlist)))
	(genvar nil) ($ratfac nil) ($breakup nil)
	($solvefactors nil) (*roots nil) (*failures nil)
	(*ivar* nil) ($polyfactor nil) (varxl nil)
	($infeval nil) ($numer nil) ($float nil)
	(numerflg $numer))
    (dolist (var (cdr ($listofvars (list '(mlist simp) lhslist varxlist))))
      (if (and (symbolp var) (not (constant var)))
	  (setq varxl (cons var varxl))))
    (orderpointer varlist)
    (setq tlhslist
	  (mapcar #'(lambda (q) (cadr (ratf (meqhk q))))
		  (cdr lhslist)))
    (setq *ivar* (caadr (ratf '$%i)))
    (setq *tvarxlist*
	  (mapcar #'(lambda (q)
		      (if (mnump q)
			  (merror (intl:gettext "algsys: variable cannot be a number; found ~M") q)
			  (caadr (ratf q))))
		  (cdr varxlist)))
    (putorder *tvarxlist*)
    (mbinding (varxl varxl)
	      (setq solnlist
		    (mapcar #'(lambda (q)
				(addmlist
				 (bbsorteqns
				  (addparam (roundroots1 q) varxlist))))
			    (algsys tlhslist))))
    (remorder *tvarxlist*)
    (setq solnlist (addmlist solnlist))
    (if numerflg
	(let (($numer t) ($float t))
	  (resimplify solnlist))
	solnlist)))

(defun condensesolnl (tempsolnl)
  (let (solnl)
    (mapl #'(lambda (q) (or (subsetl (cdr q) (car q))
			    (setq solnl (cons (car q) solnl))))
	  (sort tempsolnl #'(lambda (a b) (> (length a) (length b)))))
    solnl))

(defun subsetl (l1 s2)
  (or (equal s2 (list nil))
      (do ((l l1 (cdr l)))
	  ((null l) nil)
	(when (m-subset (car l) s2) (return t)))))

(defun m-subset (s1 s2)
  (do ((s s1 (cdr s)))
      ((null s) t)
    (unless (memalike (car s) s2) (return nil))))

(defun algsys (tlhslist &aux answ)
  (setq answ
	(condensesolnl (apply #'append
			      (mapcar #' algsys0
					 (distrep (mapcar  #'lofactors tlhslist))))))
  ;;     (displa  (cons '(mlist)  (loop for v in answ collecting
  ;;				  (cons '(mlist) v))))
  answ)

(defun algsys0 (tlhslist)
  (cond ((null tlhslist) (list nil))
	((equal tlhslist (list nil)) nil)
	(t (algsys1 tlhslist))))

(defun algsys1 (tlhslist)
  (let ((resulteq (findleastvar tlhslist))
	(vartorid nil)
	(nlhslist nil))
    (setq vartorid (cdr resulteq)
	  resulteq (car resulteq)
	  nlhslist (mapcar #'(lambda (q)
			       (if (among vartorid q)
				   (presultant q resulteq vartorid)
				   q))
			   (delete resulteq (copy-list tlhslist) :test #'equal)))
    (bakalevel (algsys nlhslist) tlhslist vartorid)))

(defun addmlist (l)
  (cons '(mlist) l))

(defmacro what-the-$ev (&rest l)
  ;; macro for calling $EV when you are not really
  ;; sure why you are calling it, but you want the
  ;; features of multiple evaluations and unpredictabiltiy
  ;; anyway.
  `(meval (list '($ev) ,@l)))

(defun rootsp (asolnset eqn)		;eqn is ((MLIST) eq deriv)
  (let (rr ($keepfloat t) ($numer t) ($float t))
    (setq rr (what-the-$ev eqn asolnset)) ; ratsimp?
    (cond ((and (complexnump (cadr rr)) (complexnump (caddr rr)))
	   (< (cabs (cadr rr))
		  (* $algdelta (max 1 (cabs (caddr rr))))))
	  (t nil))))

(defun round1 (a)
  (cond ((floatp a)
	 (setq a (maxima-rationalize a))
	 (fpcofrat1 (car a) (cdr a)))
	(t a)))

(defun roundrhs (eqn)
  (list (car eqn) (cadr eqn) (round1 (caddr eqn))))

(defun roundroots1 (lsoln)
  (mapcar #'roundrhs lsoln))

(defun bbsorteqns (l)
  (sort (copy-list l) #'orderlessp))

(defun putorder (tempvarl)
  (do ((n 1 (1+ n))
       (tempvarl tempvarl (cdr tempvarl)))
      ((null tempvarl) nil)
    (putprop (car tempvarl) n 'varorder)))

(defun remorder (gvarl)
  (mapc #'(lambda (x) (remprop x 'varorder)) gvarl))


(defun orderlessp (eqn1 eqn2)
  (< (get (caadr (ratf (cadr eqn1))) 'varorder)
     (get (caadr (ratf (cadr eqn2))) 'varorder)))

(defun addparam (asolnsetl varxlist)
  (cond ((= (length asolnsetl) (length *tvarxlist*))
	 asolnsetl)
	(t
	 (do ((tvarxl (cdr varxlist) (cdr tvarxl))
	      (defvar (mapcar #'cadr asolnsetl))
	      (var) (param))
	     ((null tvarxl) asolnsetl)
	   (setq var (car tvarxl))
	   (cond ((memalike var defvar) nil)
		 (t (setq param (make-param)
			  asolnsetl (cons (list '(mequal) var param)
					  (cdr (maxima-substitute
						param var
						(addmlist asolnsetl)))))))))))

(declare-top (special *vardegs*))

(defun findleastvar (lhsl)
  (do ((tlhsl lhsl (cdr tlhsl))
       (teq) (*vardegs*) (tdeg)
       ;; Largest possible fixnum.  The actual degree of any polynomial
       ;; is supposed to be less than this number.
       (leastdeg  most-positive-fixnum)
       (leasteq) (leastvar))
      ((null tlhsl) (cons leasteq leastvar))
    (declare (special *vardegs*))
    (setq teq (car tlhsl))
    (setq *vardegs* (getvardegs teq))
    (setq tdeg (killvardegsc teq))
    (mapc #'(lambda (q) (cond ((not (> (cdr q) leastdeg))
			      (setq leastdeg (cdr q)
				    leasteq teq
				    leastvar (car q)))))
	   *vardegs*)
    (cond ((< tdeg leastdeg) (setq leastdeg tdeg
				   leasteq teq
				   leastvar (car teq))))))

(defun killvardegsc (poly)
  (cond ((pconstp poly) 0)
	(t (do ((poly (cdr poly) (cddr poly))
		(tdeg 0 (max tdeg (+ (car  poly)
				      (cond ((= (car poly) 0)
					     (killvardegsc (cadr poly)))
					    (t (killvardegsn (cadr poly))))))))
	       ((null poly) tdeg)))))

(defun killvardegsn (poly)
  (declare (special *vardegs*))
  (cond ((pconstp poly)
	 0)
	(t
	 (let ((x (assoc (car poly) *vardegs* :test #'eq)))
	   (and x
		(not (> (cdr x) (cadr poly)))
		(setq *vardegs* (delete x *vardegs* :test #'equal))))
	 (do ((poly (cdr poly) (cddr poly))
	      (tdeg 0 (max tdeg (+ (car poly) (killvardegsn (cadr poly))))))
	     ((null poly) tdeg)))))

(defun getvardegs (poly)
  (cond ((pconstp poly) nil)
	((pconstp (caddr poly))
	 (cons (cons (car poly) (cadr poly))
	       (getvardegs (pterm (cdr poly) 0))))
	(t (getvardegs (pterm (cdr poly) 0)))))

(declare-top (unspecial *vardegs*))

(defun pconstp (poly)
  (or (atom poly) (not (member (car poly) *tvarxlist* :test #'eq))))

(defun pfreeofmainvarsp (poly)
  (cond ((atom poly) poly)
	((null (member (car poly) *tvarxlist* :test #'eq))
	 ($radcan (pdis poly)))
	(t poly)))

(defun lofactors (poly)
  (setq poly (pfreeofmainvarsp poly))
  (cond ((pzerop poly)			;(signp e poly)
	 (list 0))
	((or (atom poly) (not (atom (car poly))))  nil)
	(t (do ((tfactors (pfactor poly) (cddr tfactors))
		(lfactors))
	       ((null tfactors) lfactors)
	     (setq poly (pfreeofmainvarsp (car tfactors)))
	     (cond ((pzerop poly)	;(signp e poly)
		    (return (list 0)))
		   ((and (not (atom poly)) (atom (car poly)))
		    (setq lfactors (cons (pabs poly) lfactors))))))))

(defun combiney (listofl)
  (cond ((member nil listofl :test #'eq) nil)
	(t (combiney1 (delete '(0) listofl :test #'equal)))))

(defun combiney1 (listofl)
  (cond ((null listofl) (list nil))
	(t (mapcan #'(lambda (r)
		       (if (intersection (car listofl) r :test #'equal)
			   (list r)
			   (mapcar #'(lambda (q) (cons q r)) (car listofl))))
		   (combiney1 (cdr listofl))))))

(defun midpnt (l)
  (rhalf (rplus* (car l) (cadr l))))

(defun rflot (l)
  (let ((rr (midpnt l)))
    (if realonlyratnum (list '(rat) (car rr) (cdr rr))
	(/ (+ 0.0 (car rr)) (cdr rr)))))

(defun memberroot (a x eps)
  (cond ((null x) nil)
	((< (abs (- a (car x)))
		(/ (+ 0.0 (car eps)) (cdr eps)))
	 t)
	(t (memberroot a (cdr x) eps))))

(defun commonroots (eps solnl1 solnl2)
  (cond ((null solnl1) nil)
	((memberroot (car solnl1) solnl2 eps)
	 (cons (car solnl1) (commonroots eps (cdr solnl1) solnl2)))
	(t (commonroots eps (cdr solnl1) solnl2))))

(defun deletmult (l)
  (and l (cons (car l) (deletmult (cddr l)))))

(defun punivarp (poly)
  ;; Check if called with the number zero, return nil. 
  ;; Related bugs: SF[609466], SF[1430379], SF[1663399]
  (when (and (numberp poly) (= poly 0)) (return-from punivarp nil))
  (do ((l (cdr poly) (cddr l)))
      ((null l) t)
    (or (numberp (cadr l))
	(and (eq (caadr l) *ivar*)
	     (punivarp (cadr l)))
	(return nil))))

(defun realonly (rootsl)
  (cond ((null rootsl) nil)
	((equal 0 (sratsimp ($imagpart (caddr (car rootsl)))))
	 (nconc (list (car rootsl)) (realonly (cdr rootsl))))
	(t (realonly (cdr rootsl)))))


(defun presultant (p1 p2 var)
  (cadr (ratf ($resultant (pdis p1) (pdis p2) (pdis (list var 1 1))))))

(defun ptimeftrs (l)
  (prog (ll)
     (setq ll (cddr l))
     (cond ((null ll) (return (car l)))
	   (t (return (ptimes (car l) (ptimeftrs ll)))))))

(defun ebaksubst (solnl lhsl)
  (mapcar #'(lambda (q) (cadr (ratf (what-the-$ev (pdis q)
						  (cons '(mlist) solnl)
						  '$radcan))))
	  lhsl))

(defun baksubst (solnl lhsl)
  (setq lhsl (delete 't (mapcar #'(lambda (q) (car (merrset (baksubst1 solnl q))))
				lhsl)
		     :test #'eq))	;catches arith. ovfl
  (if (member nil lhsl :test #'eq)
      (list nil)
      lhsl))

(defun baksubst1 (solnl poly)
  (let* (($keepfloat (not $realonly))	;sturm1 needs poly with
	 (poly1				;integer coefs
	  (cdr (ratf (what-the-$ev (pdis poly)
				   (cons '(mlist) solnl)
				   '$numer)))))
    (cond ((and (complexnump (pdis (car poly1)))
		(numberp (cdr poly1)))
	   (rootsp (cons '(mlist) solnl)
		   (list '(mlist) (pdis poly) (tayapprox poly))))
	  (t (car poly1)))))

(defun complexnump (p)
  (let ((p (cadr (ratf ($ratsimp p)))))
    (or (numberp p)
	(eq (pdis (pget (car p))) '$%i))))

(defun bakalevel (solnl lhsl var)
;;(apply #'append (mapcar #'(lambda (q) (bakalevel1 q lhsl var)) solnl))
  (loop for q in solnl append (bakalevel1 q lhsl var)))

(defun bakalevel1 (solnl lhsl var)
  (cond ((exactonly solnl)
	 (cond (solnl (mergesoln solnl (algsys (ebaksubst solnl lhsl))))
	       ((cdr lhsl)
		(bakalevel (callsolve (setq solnl (findleastvar lhsl)))
			   (remove (car solnl) lhsl :test #'equal) var))
	       (t (callsolve (cons (car lhsl) var)))))
	(t (mergesoln solnl (apprsys (baksubst solnl lhsl))))))

(defun exactonly (solnl)
  (cond ((atom solnl)
	 (and (not (floatp solnl))
	      (or (null realonlyratnum) (not (eq solnl 'rat)))))
	(t (and (exactonly (car solnl)) (exactonly (cdr solnl))))))

(defun mergesoln (asoln solnl)
  (let ((errorsw t) s (unbind (cons bindlist loclist)))
    (mapcan #'(lambda (q)
		(setq s (catch 'errorsw
			  (append
			   (mapcar #'(lambda (r)
				       (what-the-$ev r (cons '(mlist) q)))
				   asoln)
			   q)))
		(cond ((eq s t)
		       (errlfun1 unbind)
		       nil)
		      (t
		       (list s))))
	    solnl)))

(defun callsolve (pv)
  (let ((poly (car pv))
	(var (cdr pv))
	(varlist varlist)
	(genvar genvar)
	(*roots nil)
	(*failures nil)
	($programmode t))
    (cond ((or $algexact (not (punivarp  poly))
	       (biquadraticp poly))
	   (solve (pdis poly) (pdis (list var 1 1)) 1)
	   (cond ((null (or *roots *failures))
		  (list nil))
		 (t
		  (append (mapcan #'(lambda (q) (callapprs (cadr (ratf (meqhk q))))) (deletmult *failures))
			  (mapcar #'list
				  (if $realonly
				      (realonly (deletmult *roots))
				      (deletmult *roots)))))))
	  (t (callapprs poly)))))

(defun biquadraticp (poly)
  (or (atom poly)
      (if algnotexact
	  (< (cadr poly) 2)
	  (or (< (cadr poly) 3)
	      (and (= (cadr poly) 4) (biquadp1 (cdddr poly)))))))

(defun biquadp1 (l)
  (or (null l)
      (and (or (= (car l) 2) (= (car l) 0))
	   (biquadp1 (cddr l)))))

(defun callapprs (poly)
  (or (punivarp poly)
      (merror (intl:gettext "algsys: tried and failed to reduce system to a polynomial in one variable; give up.")))
  (let ($rootsquad $dispflag)
    (cond ($realonly
	   (mapcar #'(lambda (q)
		       (list (list '(mequal)
				   (pdis (list (car poly) 1 1))
				   (rflot q))))
		   (sturm1 poly (cons 1 $algepsilon))))
	  (t (mapcar #'list
		     (let (($programmode t) l)
		       (setq l (cdr ($allroots (pdis poly))))
		       (cond ((not (eq (caaar l) 'mequal)) (cdr l))
			     (t l))))))))

(defun apprsys (lhsl)
  (cond ((null lhsl) (list nil))
	(t
	 (do ((tlhsl lhsl (cdr tlhsl))) (nil)
	   (cond ((null tlhsl)
          ;; SHOULD TRY TO BE MORE SPECIFIC: "TOO COMPLICATED" IN WHAT SENSE??
		  (merror (intl:gettext "algsys: system too complicated; give up.")))
		 ((pconstp (car tlhsl)) (return nil))
		 ((punivarp (car tlhsl))
		  (return (bakalevel (callapprs (car tlhsl))
				     lhsl nil))))))))

(defun tayapprox (p)
  (cons '(mplus)
	(mapcar #'(lambda (x)
		    (list '(mycabs) (pdis (ptimes (list x 1 1)
						  (pderivative p x)))))
		(listovars p))))

(defmfun mycabs (x)
  (and (complexnump x) (cabs x)))

(defun distrep (lol)
  (condensesolnl (condensesublist (combiney lol))))

(defun condensey (l)
  (let ((result nil))
    (mapl #'(lambda (q)
	      (or (memalike (car q) (cdr q)) (push (car q) result)))
	  l)
    result))

(defun condensesublist (lol)
  (mapcar #'condensey lol))

(defun exclude (l1 l2)
  (cond ((null l2)
	 nil)
	((member (car l2) l1 :test #'equal)
	 (exclude l1 (cdr l2)))
	(t
	 (cons (car l2) (exclude l1 (cdr l2))))))
