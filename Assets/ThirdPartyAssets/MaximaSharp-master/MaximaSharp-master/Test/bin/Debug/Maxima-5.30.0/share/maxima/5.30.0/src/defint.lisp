;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) copyright 1982 massachusetts institute of technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module defint)

;;;          this is the definite integration package.
;;	defint does definite integration by trying to find an
;;appropriate method for the integral in question.  the first thing that
;;is looked at is the endpoints of the problem.
;;
;;	i(grand,var,a,b) will be used for integrate(grand,var,a,b)

;; References are to "Evaluation of Definite Integrals by Symbolic
;; Manipulation", by Paul S. Wang,
;; (http://www.lcs.mit.edu/publications/pubs/pdf/MIT-LCS-TR-092.pdf)
;;
;;	nointegrate is a macsyma level flag which inhibits indefinite
;;integration.
;;	abconv is a macsyma level flag which inhibits the absolute
;;convergence test.
;;
;;	$defint is the top level function that takes the user input
;;and does minor changes to make the integrand ready for the package.
;;
;;	next comes defint, which is the function that does the
;;integration.  it is often called recursively from the bowels of the
;;package.  defint does some of the easy cases and dispatches to:
;;
;;	dintegrate.  this program first sees if the limits of
;;integration are 0,inf or minf,inf.  if so it sends the problem to
;;ztoinf or mtoinf, respectivly.
;;	else, dintegrate tries:
;;
;;	intsc1 - does integrals of sin's or cos's or exp(%i var)'s
;;		 when the interval is 0,2 %pi or 0,%pi.
;;		 method is conversion to rational function and find
;;		 residues in the unit circle. [wang, pp 107-109]
;;
;;	ratfnt - does rational functions over finite interval by
;;		 doing polynomial part directly, and converting
;;		 the rational part to an integral on 0,inf and finding
;;		 the answer by residues.
;;
;;	zto1   - i(x^(k-1)*(1-x)^(l-1),x,0,1) = beta(k,l)  or
;;		 i(log(x)*x^(x-1)*(1-x)^(l-1),x,0,1) = psi...
;;		 [wang, pp 116,117]
;;
;;	dintrad- i(x^m/(a*x^2+b*x+c)^(n+3/2),x,0,inf) [wang, p 74]
;;
;;	dintlog- i(log(g(x))*f(x),x,0,inf) = 0 (by symmetry) or
;;		 tries an integration by parts.  (only routine to
;;		 try integration by parts) [wang, pp 93-95]
;;
;;	dintexp- i(f(exp(k*x)),x,a,inf) = i(f(x+1)/(x+1),x,0,inf)
;;               or i(f(x)/x,x,0,inf)/k. First case hold for a=0;
;;               the second for a=minf. [wang 96-97]
;;
;;dintegrate also tries indefinite integration based on certain
;;predicates (such as abconv) and tries breaking up the integrand
;;over a sum or tries a change of variable.
;;
;;	ztoinf is the routine for doing integrals over the range 0,inf.
;;          it goes over a series of routines and sees if any will work:
;;
;;	   scaxn  - sc(b*x^n) (sc stands for sin or cos) [wang, pp 81-83]
;;
;;	   ssp    - a*sc^n(r*x)/x^m  [wang, pp 86,87]
;;
;;	   zmtorat- rational function. done by multiplication by plog(-x)
;;		    and finding the residues over the keyhole contour
;;		    [wang, pp 59-61]
;;
;;	   log*rat- r(x)*log^n(x) [wang, pp 89-92]
;;
;;	   logquad0 log(x)/(a*x^2+b*x+c) uses formula
;;		    i(log(x)/(x^2+2*x*a*cos(t)+a^2),x,0,inf) =
;;		    t*log(a)/sin(t).  a better formula might be
;;		    i(log(x)/(x+b)/(x+c),x,0,inf) =
;;		    (log^2(b)-log^2(c))/(2*(b-c))
;;
;;	   batapp - x^(p-1)/(b*x^n+a)^m uses formula related to the beta
;;		    function [wang, p 71]
;;		    there is also a special case when m=1 and a*b<0
;;		    see [wang, p 65]
;;
;;          sinnu  - x^-a*n(x)/d(x) [wang, pp 69-70]
;;
;;	   ggr    - x^r*exp(a*x^n+b)
;;
;;	   dintexp- see dintegrate
;;
;;     ztoinf also tries 1/2*mtoinf if the integrand is an even function
;;
;; mtoinf is the routine for doing integrals on minf,inf.
;;        it too tries a series of routines and sees if any succeed.
;;
;;	 scaxn  - when the integrand is an even function, see ztoinf
;;
;;	 mtosc  - exp(%i*m*x)*r(x) by residues on either the upper half
;;		  plane or the lower half plane, depending on whether
;;		  m is positive or negative.
;;
;;	 zmtorat- does rational function by finding residues in upper
;;	          half plane
;;
;;	 dintexp- see dintegrate
;;
;;	 rectzto%pi2 - poly(x)*rat(exp(x)) by finding residues in
;;		       rectangle [wang, pp98-100]
;;
;;	 ggrm   - x^r*exp((x+a)^n+b)
;;
;;   mtoinf also tries 2*ztoinf if the integrand is an even function.

(load-macsyma-macros rzmac)

(declare-top (special *def2* pcprntd *mtoinf* rsn*
		      sn* sd* leadcoef checkfactors
		      *nodiverg rd* exp1
		      *ul1* *ll1* *dflag bptu bptd plm* zn
		      *updn ul ll exp pe* pl* rl* pl*1 rl*1
		      loopstop* var nn* nd* dn* p*
		      ind* factors rlm*
		      $trigexpandplus $trigexpandtimes
		      plogabs *scflag*
		      *sin-cos-recur* *rad-poly-recur* *dintlog-recur*
		      *dintexp-recur* defintdebug *defint-assumptions*
		      *current-assumptions*
		      *global-defint-assumptions*)
;;;rsn* is in comdenom. does a ratsimp of numerator.
					;expvar
	     (special $intanalysis $abconvtest $noprincipal $nointegrate)
					;impvar
	     (special $solveradcan $solvetrigwarn *roots *failures
		      $logabs $tlimswitch $maxposex $maxnegex
		      $trigsign $savefactors $radexpand $breakup $%emode
		      $float $exptsubst dosimp context rp-polylogp
		      %p%i half%pi %pi2 half%pi3 varlist genvar
		      $domain $m1pbranch errorsw errrjfflag raterr
		      limitp $algebraic
		      ;;LIMITP T Causes $ASKSIGN to do special things
		      ;;For DEFINT like eliminate epsilon look for prin-inf
		      ;;take realpart and imagpart.
		      integer-info
		      ;;If LIMITP is non-null ask-integer conses
		      ;;its assumptions onto this list.
		      generate-atan2))
					;If this switch is () then RPART returns ATAN's
					;instead of ATAN2's

(declare-top (special infinities real-infinities infinitesimals))

;;These are really defined in LIMIT but DEFINT uses them also.
(cond ((not (boundp 'infinities))
       (setq infinities '($inf $minf $infinity))
       (setq real-infinities '($inf $minf))
       (setq infinitesimals '($zeroa $zerob))))

(defmvar $intanalysis t
  "When @code{true}, definite integration tries to find poles in the integrand 
in the interval of integration.")

(defmvar defintdebug () "If true Defint prints out debugging information")

(defmvar integerl nil
  "An integer-list for non-atoms found out to be `integer's")

(defmvar nonintegerl nil
  "A non-integer-list for non-atoms found out to be `noninteger's")

;; Not really sure what this is meant to do, but it's used by MTORAT,
;; KEYHOLE, and POLELIST.
(defvar *semirat* nil)

(defun $defint (exp var ll ul)
  (let ((*global-defint-assumptions* ())
	(integer-info ()) (integerl integerl) (nonintegerl nonintegerl))
    (with-new-context (context)
      (unwind-protect
	   (let ((*defint-assumptions* ())  (*def2* ())  (*rad-poly-recur* ())
		 (*sin-cos-recur* ())  (*dintexp-recur* ())  (*dintlog-recur* 0.)
		 (ans nil)  (orig-exp exp)  (orig-var var)
		 (orig-ll ll)  (orig-ul ul)
		 (pcprntd nil)  (*nodiverg nil)  ($logabs t)  ; (limitp t)
		 (rp-polylogp ())
                 ($%edispflag nil) ; to get internal representation
		 ($m1pbranch ())) ;Try this out.

	     (make-global-assumptions) ;sets *global-defint-assumptions*
	     (setq exp (ratdisrep exp))
	     (setq var (ratdisrep var))
	     (setq ll (ratdisrep ll))
	     (setq ul (ratdisrep ul))
	     (cond (($constantp var)
		    (merror (intl:gettext "defint: variable of integration cannot be a constant; found ~M") var))
		   (($subvarp var)  (setq var (stripdollar (caar var)))
		    (setq exp ($substitute var orig-var exp))))
	     (cond ((not (atom var))
		    (merror (intl:gettext "defint: variable of integration must be a simple or subscripted variable.~%defint: found ~M") var))
		   ((or (among var ul)
			(among var ll))
		    (setq var (stripdollar var))
		    (setq exp ($substitute var orig-var exp))))
	     (cond ((not (equal (sratsimp ($imagpart ll)) 0))
		    (merror (intl:gettext "defint: lower limit of integration must be real; found ~M") ll))
		   ((not (equal (sratsimp ($imagpart ul)) 0))
		    (merror (intl:gettext "defint: upper limit of integration must be real; found ~M") ul)))
	     ;; Distribute $defint over equations, lists, and matrices.
	     (cond ((mbagp exp)
	            (return-from $defint
	              (simplify
	                (cons (car exp)
	                      (mapcar #'(lambda (e)
	                                  (simplify ($defint e var ll ul)))
	                              (cdr exp)))))))
	     (cond ((setq ans (defint exp var ll ul))
		    (setq ans (subst orig-var var ans))
		    (cond ((atom ans)  ans)
			  ((and (free ans '%limit)
				(free ans '%integrate)
				(or (not (free ans '$inf))
				    (not (free ans '$minf))
				    (not (free ans '$infinity))))
			   (diverg))
			  ((not (free ans '$und))
			   `((%integrate) ,orig-exp ,orig-var ,orig-ll ,orig-ul))
			  (t ans)))
		   (t `((%integrate) ,orig-exp ,orig-var ,orig-ll ,orig-ul))))
	(forget-global-assumptions)))))

(defun eezz (exp ll ul)
  (cond ((or (polyinx exp var nil)
	     (catch 'pin%ex (pin%ex exp)))
	 (setq exp (antideriv exp))
	 ;; If antideriv can't do it, returns nil
	 ;; use limit to evaluate every answer returned by antideriv.
	 (cond ((null exp) nil)
	       (t (intsubs exp ll ul))))))
;;;Hack the expression up for exponentials.

(defun sinintp (expr var)
  ;; Is this expr a candidate for SININT ?
  (let ((expr (factor expr))
	(numer nil)
	(denom nil))
    (setq numer ($num expr))
    (setq denom ($denom expr))
    (cond ((polyinx numer var nil)
	   (cond ((and (polyinx denom var nil)
		       (deg-lessp denom var 2))
		  t)))
	  ;;ERF type things go here.
	  ((let ((exponent (%einvolve numer)))
	     (and (polyinx exponent var nil)
		  (deg-lessp exponent var 2)))
	   (cond ((free denom var)
		  t))))))

(defun deg-lessp (expr var power)
  (cond  ((or (atom expr)
	      (mnump expr)) t)
	 ((or (mtimesp expr)
	      (mplusp expr))
	  (do ((ops (cdr expr) (cdr ops)))
	      ((null ops) t)
	    (cond ((not (deg-lessp (car ops) var power))
		   (return ())))))
	 ((mexptp expr)
	  (and (or (not (alike1 (cadr expr) var))
		   (and (numberp (caddr expr))
			(not (eq (asksign (m+ power (m- (caddr expr))))
				 '$negative))))
	       (deg-lessp (cadr expr) var power)))
	 ((and (consp expr)
	       (member 'array (car expr))
	       (not (eq var (caar expr))))
	  ;; We have some subscripted variable that's not our variable
	  ;; (I think), so it's deg-lessp.
	  ;;
	  ;; FIXME: Is this the best way to handle this?  Are there
	  ;; other cases we're mising here?
	  t)))

(defun antideriv (a)
  (let ((limitp ())
	(ans ())
	(generate-atan2 ()))
    (setq ans (sinint a var))
    (cond ((among '%integrate ans)  nil)
	  (t (simplify ans)))))

;; This routine tries to take a limit a couple of ways.
(defmfun get-limit (exp var val &optional (dir '$plus dir?))
  (let ((ans (if dir?
		 (funcall #'limit-no-err exp var val dir)
		 (funcall #'limit-no-err exp var val))))
    (if (and ans (not (among '%limit ans)))
	ans
	(when (member val '($inf $minf) :test #'eq)
	  (setq ans (limit-no-err (maxima-substitute (m^t var -1) var exp)
				  var
				  0
				  (if (eq val '$inf) '$plus '$minus)))
	  (if (among '%limit ans) nil ans)))))

(defun limit-no-err (&rest argvec)
  (declare (special errorsw))
  (let ((errorsw t) (ans nil))
    (setq ans (catch 'errorsw (apply #'$limit argvec)))
    (if (eq ans t) nil ans)))

;; test whether fun2 is inverse of fun1 at val
(defun test-inverse (fun1 var1 fun2 var2 val)
  (let* ((out1 (let ((var var1))
		 (no-err-sub val fun1)))
	 (out2 (let ((var var2))
		 (no-err-sub out1 fun2))))
    (alike1 val out2)))

;; integration change of variable
(defun intcv (nv flag)
  (let ((d (bx**n+a nv))
	(*roots ())  (*failures ())  ($breakup ()))
    (cond ((and (eq ul '$inf)
		(equal ll 0)
		(equal (cadr d) 1)) ())
	  ((eq var 'yx)		; new var cannot be same as old var
	   ())
	  (t
	   ;; This is a hack!  If nv is of the form b*x^n+a, we can
	   ;; solve the equation manually instead of using solve.
	   ;; Why?  Because solve asks us for the sign of yx and
	   ;; that's bogus.
	   (cond (d
		  ;; Solve yx = b*x^n+a, for x.  Any root will do.  So we
		  ;; have x = ((yx-a)/b)^(1/n).
		  (destructuring-bind (a n b)
		      d
		    (let ((root (power* (div (sub 'yx a) b) (inv n))))
		      (cond (t
			     (setq d root)
			     (cond (flag (intcv2 d nv))
				   (t (intcv1 d nv))))
			    ))))
		 (t
		  (putprop 'yx t 'internal);; keep var from appearing in questions to user
		  (solve (m+t 'yx (m*t -1 nv)) var 1.)
		  (cond ((setq d	;; look for root that is inverse of nv
			       (do* ((roots *roots (cddr roots))
				     (root (caddar roots) (caddar roots)))
				    ((null root) nil)
				    (if (and (or (real-infinityp ll)
						 (test-inverse nv var root 'yx ll))
					     (or (real-infinityp ul)
						 (test-inverse nv var root 'yx ul)))
					(return root))))
			 (cond (flag (intcv2 d nv))
			       (t (intcv1 d nv))))
			(t ()))))))))

;; d: original variable (var) as a function of 'yx
;; ind: boolean flag
;; nv: new variable ('yx) as a function of original variable (var)
(defun intcv1 (d nv)
  (cond ((and (intcv2 d nv)
	      (equal ($imagpart *ll1*) 0)
	      (equal ($imagpart *ul1*) 0)
	      (not (alike1 *ll1* *ul1*)))
	 (let ((*def2* t))
	   (defint exp1 'yx *ll1* *ul1*)))))

;; converts limits of integration to values for new variable 'yx
(defun intcv2 (d nv)
  (intcv3 d nv)
  (and (cond ((and (zerop1 (m+ ll ul))
		   (evenfn nv var))
	      (setq exp1 (m* 2 exp1)
		    *ll1* (limcp nv var 0 '$plus)))
	     (t (setq *ll1* (limcp nv var ll '$plus))))
       (setq *ul1* (limcp nv var ul '$minus))))

;; wrapper around limit, returns nil if 
;; limit not found (nounform returned), or undefined ($und or $ind)
(defun limcp (a b c d)
  (let ((ans ($limit a b c d)))
    (cond ((not (or (null ans)
		    (among '%limit ans)
		    (among '$ind ans)
		    (among '$und ans)))
	   ans))))

;; rewrites exp, the integrand in terms of var,
;; into exp1, the integrand in terms of 'yx.
(defun intcv3 (d nv)
  (setq exp1 (m* (sdiff d 'yx)
		 (subst d var (subst 'yx nv exp))))
  (setq exp1 (sratsimp exp1)))

(defun integrand-changevar (d newvar exp var)
  (m* (sdiff d newvar)
      (subst d var exp)))
  
(defun defint (exp var ll ul)
  (let ((old-assumptions *defint-assumptions*)  
        (*current-assumptions* ())
        (limitp t))
    (unwind-protect
	 (prog ()
	    (setq *current-assumptions* (make-defint-assumptions 'noask))
	    (let ((exp (resimplify exp))
		  (var (resimplify var))
		  ($exptsubst t)
		  (loopstop* 0)
		  ;; D (not used? -- cwh)
		  ans nn* dn* nd* $noprincipal)
	      (cond ((setq ans (defint-list exp var ll ul))
		     (return ans))
		    ((or (zerop1 exp)
			 (alike1 ul ll))
		     (return 0.))
		    ((not (among var exp))
		     (cond ((or (member ul '($inf $minf) :test #'eq)
				(member ll '($inf $minf) :test #'eq))
			    (diverg))
			   (t (setq ans (m* exp (m+ ul (m- ll))))
			      (return ans))))
                    ;; Look for integrals which involve log and exp functions.
                    ;; Maxima has a special algorithm to get general results.
                    ((and (setq ans (defint-log-exp exp var ll ul)))
                     (return ans)))
	      (let* ((exp (rmconst1 exp))
		     (c (car exp))
		     (exp (%i-out-of-denom (cdr exp))))
		(cond ((and (not $nointegrate)
			    (not (atom exp))
			    (or (among 'mqapply exp)
				(not (member (caar exp)
					   '(mexpt mplus mtimes %sin %cos
					     %tan %sinh %cosh %tanh
					     %log %asin %acos %atan
					     %cot %acot %sec
					     %asec %csc %acsc
					     %derivative) :test #'eq))))
		       (cond ((setq ans (antideriv exp))
			      (setq ans (intsubs ans ll ul))
			      (return (m* c ans)))
			     (t (return nil)))))
		(setq exp (tansc exp))
		(cond ((setq  ans (initial-analysis exp var ll ul))
		       (return (m* c ans))))
		(return nil))))
      (restore-defint-assumptions old-assumptions *current-assumptions*))))

(defun defint-list (exp var ll ul)
  (cond ((and (not (atom exp))
	      (member (caar exp)
		    '(mequal mlist $matrix) :test #'eq))
	 (let ((ans (cons (car exp)
			  (mapcar
			   #'(lambda (sub-exp)
			       (defint sub-exp var ll ul))
			   (cdr exp)))))
	   (cond (ans (simplify ans))
		 (t nil))))
	(t nil)))

(defun initial-analysis (exp var ll ul)
  (let ((pole (cond ((not $intanalysis)
		     '$no)		;don't do any checking.
		    (t (poles-in-interval exp var ll ul)))))
    (cond ((eq pole '$no)
	   (cond ((and (oddfn exp var)
		       (or (and (eq ll '$minf)
				(eq ul '$inf))
			   (eq ($sign (m+ ll ul))
			       '$zero)))  0)
		 (t (parse-integrand exp var ll ul))))
	  ((eq pole '$unknown)  ())
	  (t (principal-value-integral exp var ll ul pole)))))

(defun parse-integrand (exp var ll ul)
  (let (ans)
    (cond ((setq ans (eezz exp ll ul))  ans)
	  ((and (ratp exp var)
		(setq ans (method-by-limits exp var ll ul)))  ans)
	  ((and (mplusp exp)
		(setq ans (intbyterm exp t)))  ans)
	  ((setq ans (method-by-limits exp var ll ul))  ans)
	  (t ()))))

(defun rmconst1 (e)
  (cond ((not (freeof var e))
	 (partition e var 1))
	(t (cons e 1))))


(defun method-by-limits (exp var ll ul)
  (let ((old-assumptions *defint-assumptions*))
    (setq *current-assumptions* (make-defint-assumptions 'noask))
    ;;Should be a PROG inside of unwind-protect, but Multics has a compiler
    ;;bug wrt. and I want to test this code now.
    (unwind-protect
	 (cond ((and (and (eq ul '$inf)
			  (eq ll '$minf))
		     (mtoinf exp var)))
	       ((and (and (eq ul '$inf)
			  (equal ll 0.))
		     (ztoinf exp var)))
;;;This seems((and (and (eq ul '$inf)
;;;fairly losing	(setq exp (subin (m+ ll var) exp))
;;;			(setq ll 0.))
;;;		   (ztoinf exp var)))
	       ((and (equal ll 0.)
		     (freeof var ul)
		     (eq ($asksign ul) '$pos)
		     (zto1 exp)))
	       ;;	     ((and (and (equal ul 1.)
	       ;;			(equal ll 0.))  (zto1 exp)))
	       (t (dintegrate exp var ll ul)))
      (restore-defint-assumptions old-assumptions *defint-assumptions*))))


(defun dintegrate (exp var ll ul)
  (let ((ans nil) (arg nil) (*scflag* nil)
	(*dflag nil) ($%emode t))
;;;NOT COMPLETE for sin's and cos's.
    (cond ((and (not *sin-cos-recur*)
		(oscip exp)
		(setq *scflag* t)
		(intsc1 ll ul exp)))
	  ((and (not *rad-poly-recur*)
		(notinvolve exp '(%log))
		(not (%einvolve exp))
		(method-radical-poly exp var ll ul)))
	  ((and (not (equal *dintlog-recur* 2.))
		(setq arg (involve exp '(%log)))
		(dintlog exp arg)))
	  ((and (not *dintexp-recur*)
		(setq arg (%einvolve exp))
		(dintexp exp var)))
	  ((and (not (ratp exp var))
		(setq ans (let (($trigexpandtimes nil)
				($trigexpandplus t))
			    ($trigexpand exp)))
		(setq ans ($expand ans))
		(not (alike1 ans exp))
		(intbyterm ans t)))
	  ((setq ans (antideriv exp))
	   (intsubs ans ll ul))
	  (t nil))))

(defun method-radical-poly (exp var ll ul)
;;;Recursion stopper
  (let ((*rad-poly-recur* t)		;recursion stopper
	(result ()))
    (cond ((and (sinintp exp var)
		(setq result (antideriv exp))
		(intsubs result ll ul)))
	  ((and (ratp exp var)
		(setq result (ratfnt exp))))
	  ((and (not *scflag*)
		(not (eq ul '$inf))
		(radic exp var)
		(kindp34)
		(setq result (cv exp))))
	  (t ()))))

(defun principal-value-integral (exp var ll ul poles)
  (let ((anti-deriv ()))
    (cond ((not (null (setq anti-deriv (antideriv exp))))
	   (cond ((not (null poles))
		  (order-limits 'ask)
		  (cond ((take-principal anti-deriv ll ul poles))
			(t ()))))))))

;; adds up integrals of ranges between each pair of poles.
;; checks if whole thing is divergent as limits of integration approach poles.
(defun take-principal (anti-deriv ll ul poles &aux ans merged-list)
  ;;; calling $logcontract causes antiderivative of 1/(1-x^5) to blow up
  ;;  (setq anti-deriv (cond ((involve anti-deriv '(%log))
  ;;			  ($logcontract anti-deriv))
  ;;			 (t anti-deriv)))
  (setq ans 0.)
  (setq merged-list (interval-list poles ll ul))
  (do ((current-pole (cdr merged-list) (cdr current-pole))
       (previous-pole merged-list (cdr previous-pole)))
      ((null current-pole)  t)
    (setq ans (m+ ans
		  (intsubs anti-deriv (m+ (caar previous-pole) 'epsilon)
			   (m+ (caar current-pole) (m- 'epsilon))))))

  (setq ans (get-limit (get-limit ans 'epsilon 0 '$plus) 'prin-inf '$inf))
  ;;Return section.
  (cond ((or (null ans)
	     (not (free ans '$infinity))
	     (not (free ans '$ind)))  ())
	((or (among '$minf ans)
	     (among '$inf ans)
	     (among '$und ans))
	 (diverg))
	(t (principal) ans)))

(defun interval-list (pole-list ll ul)
  (let ((first (car (first pole-list)))
	(last (caar (last pole-list))))
    (cond ((eq ul last)
	   (if (eq ul '$inf)
	       (setq pole-list (subst 'prin-inf '$inf pole-list))))
	  (t (if (eq ul '$inf)
		 (setq ul 'prin-inf))
	     (setq pole-list (append pole-list (list (cons ul 'ignored))))))
    (cond ((eq ll first)
	   (if (eq ll '$minf)
	       (setq pole-list (subst (m- 'prin-inf) '$minf pole-list))))
	  (t (if (eq ll '$minf)
		 (setq ll (m- 'prin-inf)))
	     (setq pole-list (append (list (cons ll 'ignored)) pole-list)))))
  pole-list)

;; Assumes EXP is a rational expression with no polynomial part and
;; converts the finite integration to integration over a half-infinite
;; interval.  The substitution y = (x-a)/(b-x) is used.  Equivalently,
;; x = (b*y+a)/(y+1).
;;
;; (I'm guessing CV means Change Variable here.)
(defun cv (exp)
  (if (not (or (real-infinityp ll) (real-infinityp ul)))
      ;; FIXME!  This is a hack.  We apply the transformation with
      ;; symbolic limits and then substitute the actual limits later.
      ;; That way method-by-limits (usually?) sees a simpler
      ;; integrand.
      ;;
      ;; See Bugs 938235 and 941457.  These fail because $FACTOR is
      ;; unable to factor the transformed result.  This needs more
      ;; work (in other places).
      (let ((trans (integrand-changevar (m// (m+t 'll (m*t 'ul 'yx))
					     (m+t 1. 'yx))
					'yx exp var)))
	;; If the limit is a number, use $substitute so we simplify
	;; the result.  Do we really want to do this?
	(setf trans (if (mnump ll)
			($substitute ll 'll trans)
			(subst ll 'll trans)))
	(setf trans (if (mnump ul)
			($substitute ul 'ul trans)
			(subst ul 'ul trans)))
	(method-by-limits trans 'yx 0. '$inf))
      ()))

;; Integrate rational functions over a finite interval by doing the
;; polynomial part directly, and converting the rational part to an
;; integral from 0 to inf.  This is evaluated via residues.
(defun ratfnt (exp)
  (let ((e (pqr exp)))
    ;; PQR divides the rational expression and returns the quotient
    ;; and remainder
    (flet ((try-antideriv (e lo hi)
	     (let ((ans (antideriv e)))
	       (when ans
		 (intsubs ans lo hi)))))

      (cond ((equal 0. (car e))
	     ;; No polynomial part
	     (let ((ans (try-antideriv exp ll ul)))
	       (if ans
		   ans
		   (cv exp))))
	    ((equal 0. (cdr e))
	     ;; Only polynomial part
	     (eezz (car e) ll ul))
	    (t
	     ;; A non-zero quotient and remainder.  Combine the results
	     ;; together.
	     (let ((ans (try-antideriv (m// (cdr e) dn*) ll ul)))
	       (cond (ans
		      (m+t (eezz (car e) ll ul)
			   ans))
		     (t
		      (m+t (eezz (car e) ll ul)
			   (cv (m// (cdr e) dn*)))))))))))

;; I think this takes a rational expression E, and finds the
;; polynomial part.  A cons is returned.  The car is the quotient and
;; the cdr is the remainder.
(defun pqr (e)
  (let ((varlist (list var)))
    (newvar e)
    (setq e (cdr (ratrep* e)))
    (setq dn* (pdis (ratdenominator e)))
    (setq e (pdivide (ratnumerator e) (ratdenominator e)))
    (cons (simplify (rdis (car e))) (simplify (rdis (cadr e))))))


(defun intbyterm (exp *nodiverg)
  (let ((saved-exp exp))
    (cond ((mplusp exp)
	   (let ((ans (catch 'divergent
			(andmapcar #'(lambda (new-exp)
				       (let ((*def2* t))
					 (defint new-exp var ll ul)))
				   (cdr exp)))))
	     (cond ((null ans) nil)
		   ((eq ans 'divergent)
		    (let ((*nodiverg nil))
		      (cond ((setq ans (antideriv saved-exp))
			     (intsubs ans ll ul))
			    (t nil))))
		   (t (sratsimp (m+l ans))))))
;;;If leadop isn't plus don't do anything.
	  (t nil))))

(defun kindp34 nil
  (numden exp)
  (let* ((d dn*)
	 (a (cond ((and (zerop1 ($limit d var ll '$plus))
			(eq (limit-pole (m+ exp (m+ (m- ll) var))
					var ll '$plus)
			    '$yes))
		   t)
		  (t nil)))
	 (b (cond ((and (zerop1 ($limit d var ul '$minus))
			(eq (limit-pole (m+ exp (m+ ul (m- var)))
					var ul '$minus)
			    '$yes))
		   t)
		  (t nil))))
    (or a b)))

(defun diverg nil
  (cond (*nodiverg (throw 'divergent 'divergent))
	(t (merror (intl:gettext "defint: integral is divergent.")))))

(defun make-defint-assumptions (ask-or-not)
  (cond ((null (order-limits ask-or-not))  ())
	(t (mapc 'forget *defint-assumptions*)
	   (setq *defint-assumptions* ())
	   (let ((sign-ll (cond ((eq ll '$inf)  '$pos)
				((eq ll '$minf) '$neg)
				(t ($sign ($limit ll)))))
		 (sign-ul (cond ((eq ul '$inf)  '$pos)
				((eq ul '$minf)  '$neg)
				(t ($sign ($limit ul)))))
		 (sign-ul-ll (cond ((and (eq ul '$inf)
					 (not (eq ll '$inf)))  '$pos)
				   ((and (eq ul '$minf)
					 (not (eq ll '$minf)))  '$neg)
				   (t ($sign ($limit (m+ ul (m- ll))))))))
	     (cond ((eq sign-ul-ll '$pos)
		    (setq *defint-assumptions*
			  `(,(assume `((mgreaterp) ,var ,ll))
			    ,(assume `((mgreaterp) ,ul ,var)))))
		   ((eq sign-ul-ll '$neg)
		    (setq *defint-assumptions*
			  `(,(assume `((mgreaterp) ,var ,ul))
			    ,(assume `((mgreaterp) ,ll ,var))))))
	     (cond ((and (eq sign-ll '$pos)
			 (eq sign-ul '$pos))
		    (setq *defint-assumptions*
			  `(,(assume `((mgreaterp) ,var 0))
			    ,@*defint-assumptions*)))
		   ((and (eq sign-ll '$neg)
			 (eq sign-ul '$neg))
		    (setq *defint-assumptions*
			  `(,(assume `((mgreaterp) 0 ,var))
			    ,@*defint-assumptions*)))
		   (t *defint-assumptions*))))))

(defun restore-defint-assumptions (old-assumptions assumptions)
  (do ((llist assumptions (cdr llist)))
      ((null llist) t)
    (forget (car llist)))
  (do ((llist old-assumptions (cdr llist)))
      ((null llist) t)
    (assume (car llist))))

(defun make-global-assumptions ()
  (setq *global-defint-assumptions*
	(cons (assume '((mgreaterp) *z* 0.))
	      *global-defint-assumptions*))
  ;; *Z* is a "zero parameter" for this package.
  ;; Its also used to transform.
  ;;  limit(exp,var,val,dir) -- limit(exp,tvar,0,dir)
  (setq *global-defint-assumptions*
	(cons (assume '((mgreaterp) epsilon 0.))
	      *global-defint-assumptions*))
  (setq *global-defint-assumptions*
	(cons (assume '((mlessp) epsilon 1.0e-8))
	      *global-defint-assumptions*))
  ;; EPSILON is used in principal value code to denote the familiar
  ;; mathematical entity.
  (setq *global-defint-assumptions*
	(cons (assume '((mgreaterp) prin-inf 1.0e+8))
	      *global-defint-assumptions*)))

;;; PRIN-INF Is a special symbol in the principal value code used to
;;; denote an end-point which is proceeding to infinity.

(defun forget-global-assumptions ()
  (do ((llist *global-defint-assumptions* (cdr llist)))
      ((null llist) t)
    (forget (car llist)))
  (cond ((not (null integer-info))
	 (do ((llist integer-info (cdr llist)))
	     ((null llist) t)
	   (i-$remove `(,(cadar llist) ,(caddar llist)))))))

(defun order-limits (ask-or-not)
  (cond ((or (not (equal ($imagpart ll) 0))
	     (not (equal ($imagpart ul) 0)))  ())
	(t (cond ((alike1 ll (m*t -1 '$inf))
		  (setq ll '$minf)))
	   (cond ((alike1 ul (m*t -1 '$inf))
		  (setq ul '$minf)))
	   (cond ((alike1 ll (m*t -1 '$minf))
		  (setq ll '$inf)))
	   (cond ((alike1 ul (m*t -1 '$minf))
		  (setq ul '$inf)))
	   (cond ((eq ul '$inf) nil)
		 ((eq ll '$minf)
		  (setq exp (subin (m- var) exp))
		  (setq ll (m- ul))
		  (setq ul '$inf))
		 ((eq ll '$inf)
		  (setq ll ul)
		  (setq ul '$inf)
		  (setq exp (m- exp))))
	   ;;Fix limits so that ll < ul.
	   (let ((d (complm ask-or-not)))
	     (cond ((equal d -1)
		    (setq exp (m- exp))
		    (setq d ll)
		    (setq ll ul)
		    (setq ul d))
		   (t t))))))

(defun complm (ask-or-not)
  (let ((askflag (cond ((eq ask-or-not 'ask)  t)
		       (t nil)))
	(a ()))
    (cond ((alike1 ul ll)  0.)
	  ((eq (setq a (cond (askflag ($asksign ($limit (m+t ul (m- ll)))))
			     (t ($sign ($limit (m+t ul (m- ll)))))))
	       '$pos)
	   1.)
	  ((eq a '$neg)  -1)
	  (t 1.))))

;; Substitute a and b into integral e
;;
;; Looks for discontinuties in integral, and works around them.
;; For example, in  
;;
;; integrate(x^(2*n)*exp(-(x)^2),x)    ==>
;; -gamma_incomplete((2*n+1)/2,x^2)*x^(2*n+1)*abs(x)^(-2*n-1)/2
;; 
;; the integral has a discontinuity at x=0.
;;
(defun intsubs (e a b)
  (let ((edges (cond ((not $intanalysis)
		      '$no)		;don't do any checking.
		    (t (discontinuities-in-interval 
			(let (($algebraic t)) 
			  (sratsimp e))
			var a b)))))

    (cond ((or (eq edges '$no)
	       (eq edges '$unknown))
	   (whole-intsubs e a b))
	  (t
	   (do* ((l edges (cdr l))
		 (total nil)
		 (a1 (car l) (car l))
		 (b1 (cadr l) (cadr l)))
		((null (cdr l)) (if (every (lambda (x) x) total)
				    (m+l total)))
		(push
		 (whole-intsubs e a1 b1)
		 total))))))

;; look for terms with a negative exponent
(defun discontinuities-denom (exp)
  (cond ((atom exp) 1)
	((eq (caar exp) 'mtimes)
	 (m*l (mapcar #'discontinuities-denom (cdr exp))))
	((and (eq (caar exp) 'mexpt)
	      (eq ($sign (caddr exp)) '$neg))
	 (m^ (cadr exp) (m- (caddr exp))))
	(t 1)))

;; returns list of places where exp might be discontinuous in var.
;; list begins with ll and ends with ul, and include any values between
;; ll and ul.
;; return '$no or '$unknown if no discontinuities found.
(defun discontinuities-in-interval (exp var ll ul)
  (let* ((denom (discontinuities-denom exp))
	 (roots (real-roots denom var)))
    (cond ((eq roots '$failure)
	   '$unknown)
	  ((eq roots '$no)
	   '$no)
	  (t (do ((dummy roots (cdr dummy))
		  (pole-list nil))
		 ((null dummy)
		  (cond (pole-list
			 (append (list ll)
				 (sortgreat pole-list)
				 (list ul)))
			(t '$no)))
		 (let ((soltn (caar dummy)))
		   ;; (multiplicity (cdar dummy)) ;; not used
		   (if (strictly-in-interval soltn ll ul)
		       (push soltn pole-list))))))))


;; Carefully substitute the integration limits A and B into the
;; expression E.
(defun whole-intsubs (e a b)
  (cond ((easy-subs e a b))
	(t (setq *current-assumptions*
		 (make-defint-assumptions 'ask)) ;get forceful!
	   (let (($algebraic t))
	     (setq e (sratsimp e))
	     (cond ((limit-subs e a b))
		   (t (same-sheet-subs e a b)))))))

;; Try easy substitutions.  Return NIL if we can't.
(defun easy-subs (e ll ul)
  (cond ((or (infinityp ll) (infinityp ul))
	 ;; Infinite limits aren't easy
	 nil)
	(t
	 (cond ((or (polyinx e var ())
		    (and (not (involve e '(%log %asin %acos %atan %asinh %acosh %atanh %atan2
						%gamma_incomplete %expintegral_ei)))
			 (free ($denom e) var)))
		;; It's easy if we have a polynomial.  I (rtoy) think
		;; it's also easy if the denominator is free of the
		;; integration variable and also if the expression
		;; doesn't involve inverse functions.
		;;
		;; gamma_incomplete and expintegral_ie
		;; included because of discontinuity in
		;; gamma_incomplete(0, exp(%i*x)) and 
		;; expintegral_ei(exp(%i*x))
		;;
		;; XXX:  Are there other cases we've forgotten about?
		;;
		;; So just try to substitute the limits into the
		;; expression.  If no errors are produced, we're done.
		(let ((ll-val (no-err-sub ll e))
		      (ul-val (no-err-sub ul e)))
		  (cond ((or (eq ll-val t)
                             (eq ul-val t))
                         ;; no-err-sub has returned T. An error was catched.
                         nil)
                        ((and ll-val ul-val)
			 (m- ul-val ll-val))
			(t nil))))
	       (t nil)))))

(defun limit-subs (e ll ul)
  (cond ((involve e '(%atan %gamma_incomplete %expintegral_ei))
	 ())	; functions with discontinuities
	(t (setq e ($multthru e))
	   (let ((a1 ($limit e var ll '$plus))
		 (a2 ($limit e var ul '$minus)))
	     (cond ((member a1 '($inf $minf $infinity ) :test #'eq)
		    (cond ((member a2 '($inf $minf $infinity) :test #'eq)
			   (cond ((eq a2 a1)  ())
				 (t (diverg))))
			  (t (diverg))))
		   ((member a2 '($inf $minf $infinity) :test #'eq)  (diverg))
		   ((or (member a1 '($und $ind) :test #'eq)
			(member a2 '($und $ind) :test #'eq))  ())
		   (t (m- a2 a1)))))))

;;;This function works only on things with ATAN's in them now.
(defun same-sheet-subs (exp ll ul &aux ll-ans ul-ans)
  ;; POLES-IN-INTERVAL doesn't know about the poles of tan(x).  Call
  ;; trigsimp to convert tan into sin/cos, which POLES-IN-INTERVAL
  ;; knows how to handle.
  ;;
  ;; XXX Should we fix POLES-IN-INTERVAL instead?
  ;;
  ;; XXX Is calling trigsimp too much?  Should we just only try to
  ;; substitute sin/cos for tan?
  ;;
  ;; XXX Should the result try to convert sin/cos back into tan?  (A
  ;; call to trigreduce would do it, among other things.)
  (let* ((exp (mfuncall '$trigsimp exp))
	 (poles (atan-poles exp ll ul)))
    ;;POLES -> ((mlist) ((mequal) ((%atan) foo) replacement) ......)
    ;;We can then use $SUBSTITUTE
    (setq ll-ans (limcp exp var ll '$plus))
    (setq exp (sratsimp ($substitute poles exp)))
    (setq ul-ans (limcp exp var ul '$minus))
    (if (and ll-ans 
	     ul-ans
	     (not (member ll-ans infinities))
	     (not (member ul-ans infinities)))
	(m- ul-ans ll-ans)
      nil)))

(defun atan-poles (exp ll ul)
  `((mlist) ,@(atan-pole1 exp ll ul)))

(defun atan-pole1 (exp ll ul &aux ipart)
  (cond
    ((mapatom exp)  ())
    ((matanp exp)	 ;neglect multiplicity and '$unknowns for now.
     (desetq (exp . ipart) (trisplit exp))
     (cond
       ((not (equal (sratsimp ipart) 0))  ())
       (t (let ((pole (poles-in-interval (let (($algebraic t))
					   (sratsimp (cadr exp)))
					 var ll ul)))
	    (cond ((and pole (not (or (eq pole '$unknown)
				      (eq pole '$no))))
		   (do ((l pole (cdr l)) (llist ()))
		       ((null l)  llist)
		     (cond
		       ((zerop1 (m- (caar l) ll)) t)  ; don't worry about discontinuity
 		       ((zerop1 (m- (caar l) ul)) t)  ;  at boundary of integration
		       (t (let ((low-lim ($limit (cadr exp) var (caar l) '$minus))
				(up-lim ($limit (cadr exp) var (caar l) '$plus)))
			    (cond ((and (not (eq low-lim up-lim))
					(real-infinityp low-lim)
					(real-infinityp up-lim))
				   (let ((change (if (eq low-lim '$minf)
						     (m- '$%pi)
						     '$%pi)))
				     (setq llist (cons `((mequal simp) ,exp  ,(m+ exp change))
						       llist)))))))))))))))
    (t (do ((l (cdr exp) (cdr l))
	    (llist ()))
	   ((null l)  llist)
	 (setq llist (append llist (atan-pole1 (car l) ll ul)))))))

(defun difapply (n d s fn1)
  (prog (k m r $noprincipal)
     (cond ((eq ($asksign (m+ (deg d) (m- s) (m- 2.)))  '$neg)
	    (return nil)))
     (setq $noprincipal t)
     (cond ((or (not (mexptp d))
		(not (numberp (setq r (caddr d)))))
	    (return nil))
	   ((and (equal n 1.)
		 (eq fn1 'mtorat)
		 (equal 1. (deg (cadr d))))
	    (return 0.)))
     (setq m (deg (setq d (cadr d))))
     (setq k (m// (m+ s 2.) m))
     (cond ((eq (ask-integer (m// (m+ s 2.) m) '$any)  '$yes)
	    nil)
	   (t (setq k (m+ 1 k))))
     (cond ((eq ($sign (m+ r (m- k))) '$pos)
	    (return (diffhk fn1 n d k (m+ r (m- k))))))))

(defun diffhk (fn1 n d r m)
  (prog (d1 *dflag)
     (setq *dflag t)
     (setq d1 (funcall fn1 n
		       (m^ (m+t '*z* d) r)
		       (m* r (deg d))))
     (cond (d1 (return (difap1 d1 r '*z* m 0.))))))

(defun principal nil
  (cond ($noprincipal (diverg))
	((not pcprntd)
	 (format t "Principal Value~%")
	 (setq pcprntd t))))

;; e is of form poly(x)*exp(m*%i*x)
;; s is degree of denominator
;; adds e to bptu or bptd according to sign of m
(defun rib (e s)
  (let (*updn c)
    (cond ((or (mnump e) (constant e))
	   (setq bptu (cons e bptu)))
	  (t (setq e (rmconst1 e))
	     (setq c (car e))
	     (setq nn* (cdr e))
	     (setq nd* s)
	     (setq e (catch 'ptimes%e (ptimes%e nn* nd*)))
	     (cond ((null e) nil)
		   (t (setq e (m* c e))
		      (cond (*updn (setq bptu (cons e bptu)))
			    (t (setq bptd (cons e bptd))))))))))

;; check term is of form poly(x)*exp(m*%i*x)
;; n is degree of denominator
(defun ptimes%e (term n)
  (cond ((and (mexptp term)
	      (eq (cadr term) '$%e)
	      (polyinx (caddr term) var nil)
	      (eq ($sign (m+ (deg ($realpart (caddr term))) -1))
		  '$neg)
	      (eq ($sign (m+ (deg (setq nn* ($imagpart (caddr term))))
			     -2.))
		  '$neg))
	 (cond ((eq ($asksign (ratdisrep (ratcoef nn* var))) '$pos)
		(setq *updn t))
	       (t (setq *updn nil)))
	 term)
	((and (mtimesp term)
	      (setq nn* (polfactors term))
	      (or (null (car nn*))
		  (eq ($sign (m+ n (m- (deg (car nn*)))))
		      '$pos))
	      (ptimes%e (cadr nn*) n)
	      term))
	(t (throw 'ptimes%e nil))))

(defun csemidown (n d var)
  (let ((pcprntd t)) ;Not sure what to do about PRINCIPAL values here.
    (princip (res n d #'lowerhalf #'(lambda (x)
				      (cond ((equal ($imagpart x) 0)  t)
					    (t ())))))))

(defun lowerhalf (j)
  (eq ($asksign ($imagpart j)) '$neg))

(defun upperhalf (j)
  (eq ($asksign ($imagpart j)) '$pos))


(defun csemiup (n d var)
  (let ((pcprntd t)) ;I'm not sure what to do about PRINCIPAL values here.
    (princip (res n d #'upperhalf #'(lambda (x)
				      (cond ((equal ($imagpart x) 0)  t)
					    (t ())))))))

(defun princip (n)
  (cond ((null n) nil)
	(t (m*t '$%i ($rectform (m+ (cond ((car n)
					   (m*t 2. (car n)))
					  (t 0.))
				    (cond ((cadr n)
					   (principal)
					   (cadr n))
					  (t 0.))))))))

;; exponentialize sin and cos
(defun sconvert (e)
  (cond ((atom e) e)
	((polyinx e var nil) e)
	((eq (caar e) '%sin)
	 (m* '((rat) -1 2)
	     '$%i
	     (m+t (m^t '$%e (m*t '$%i (cadr e)))
		  (m- (m^t '$%e (m*t (m- '$%i) (cadr e)))))))
	((eq (caar e) '%cos)
	 (mul* '((rat) 1. 2.)
	       (m+t (m^t '$%e (m*t '$%i (cadr e)))
		    (m^t '$%e (m*t (m- '$%i) (cadr e))))))
	(t (simplify
	    (cons (list (caar e)) (mapcar #'sconvert (cdr e)))))))

(defun polfactors (exp)
  (let (poly rest)
    (cond ((mplusp exp)  nil)
	  (t (cond ((mtimesp exp)
		    (setq exp (reverse (cdr exp))))
		   (t (setq exp (list exp))))
	     (mapc #'(lambda (term)
		       (cond ((polyinx term var nil)
			      (push term poly))
			     (t (push term rest))))
		   exp)
	     (list (m*l poly) (m*l rest))))))

(defun esap (e)
  (prog (d)
     (cond ((atom e) (return e))
	   ((not (among '$%e e)) (return e))
	   ((and (mexptp e)
		 (eq (cadr e) '$%e))
	    (setq d ($imagpart (caddr e)))
	    (return (m* (m^t '$%e ($realpart (caddr e)))
			(m+ `((%cos) ,d)
			    (m*t '$%i `((%sin) ,d))))))
	   (t (return (simplify (cons (list (caar e))
				      (mapcar #'esap (cdr e)))))))))

;; computes integral from minf to inf for expressions of the form
;; exp(%i*m*x)*r(x) by residues on either the upper half
;;		  plane or the lower half plane, depending on whether
;;		  m is positive or negative.  [wang p. 77]
;;
;; exponentializes sin and cos before applying residue method.
;; can handle some expressions with poles on real line, such as
;; sin(x)*cos(x)/x.
(defun mtosc (grand)
  (numden grand)
  (let ((n nn*)
	(d dn*)
	ratterms ratans
	plf bptu bptd s upans downans)
    (cond ((not (or (polyinx d var nil)
		    (and (setq grand (%einvolve d))
			 (among '$%i grand)
			 (polyinx (setq d (sratsimp (m// d (m^t '$%e grand))))
				  var
				  nil)
			 (setq n (m// n (m^t '$%e grand))))))  nil)
	  ((equal (setq s (deg d)) 0)  nil)
;;;Above tests for applicability of this method.
	  ((and (or (setq plf (polfactors n))  t)
		(setq n ($expand (cond ((car plf)
					(m*t 'x* (sconvert (cadr plf))))
				       (t (sconvert n)))))
		(cond ((mplusp n)  (setq n (cdr n)))
		      (t (setq n (list n))))
		(dolist (term n t)
		  (cond ((polyinx term var nil)
			 ;; call to $expand can create rational terms
			 ;; with no exp(m*%i*x)
			 (setq ratterms (cons term ratterms)))
			((rib term s))
			(t (return nil))))
;;;Function RIB sets up the values of BPTU and BPTD
		(cond ((car plf)
		       (setq bptu (subst (car plf) 'x* bptu))
		       (setq bptd (subst (car plf) 'x* bptd))
		       (setq ratterms (subst (car plf) 'x* ratterms))
		       t)	 ;CROCK, CROCK. This is TERRIBLE code.
		      (t t))
;;;If there is BPTU then CSEMIUP must succeed.
;;;Likewise for BPTD.
		(setq ratans
		      (if ratterms
			  (let (($intanalysis nil))
			    ;; The original integrand was already
			    ;; determined to have no poles by initial-analysis.
			    ;; If individual terms of the expansion have poles, the poles 
			    ;; must cancel each other out, so we can ignore them.
			    (try-defint (m// (m+l ratterms) d) var '$minf '$inf))
			0))
		;; if integral of ratterms is divergent, ratans is nil, 
		;; and mtosc returns nil

		(cond (bptu (setq upans (csemiup (m+l bptu) d var)))
		      (t (setq upans 0)))
		(cond (bptd (setq downans (csemidown (m+l bptd) d var)))
		      (t (setq downans 0))))
	   
	   (sratsimp (m+ ratans
			 (m* '$%pi (m+ upans (m- downans)))))))))


(defun evenfn (e var)
  (let ((temp (m+ (m- e)
		  (cond ((atom var)
			 ($substitute (m- var) var e))
			(t ($ratsubst (m- var) var e))))))
    (cond ((zerop1 temp)
	   t)
	  ((zerop1 (sratsimp temp))
	   t)
	  (t nil))))

(defun oddfn (e var)
  (let ((temp (m+ e (cond ((atom var)
			   ($substitute (m- var) var e))
			  (t ($ratsubst (m- var) var e))))))
    (cond ((zerop1 temp)
	   t)
	  ((zerop1 (sratsimp temp))
	   t)
	  (t nil))))

(defun ztoinf (grand var)
  (prog (n d sn* sd* varlist
	 s nc dc
	 ans r $savefactors checkfactors temp test-var)
     (setq $savefactors t sn* (setq sd* (list 1.)))
     (cond ((eq ($sign (m+ loopstop* -1))
		'$pos)
	    (return nil))
	   ((setq temp (or (scaxn grand)
			   (ssp grand)))
	    (return temp))
	   ((involve grand '(%sin %cos %tan))
	    (setq grand (sconvert grand))
	    (go on)))

     (cond ((polyinx grand var nil)
	    (diverg))
	   ((and (ratp grand var)
		 (mtimesp grand)
		 (andmapcar #'snumden (cdr grand)))
	    (setq nn* (m*l sn*)
		  sn* nil)
	    (setq dn* (m*l sd*)
		  sd* nil))
	   (t (numden grand)))
;;;
;;;New section.
     (setq n (rmconst1 nn*))
     (setq d (rmconst1 dn*))
     (setq nc (car n))
     (setq n (cdr n))
     (setq dc (car d))
     (setq d (cdr d))
     (cond ((polyinx d var nil)
	    (setq s (deg d)))
	   (t (go findout)))
     (cond ((and (setq r (findp n))
		 (eq (ask-integer r '$integer) '$yes)
		 (setq test-var (bxm d s))
		 (setq ans (apply 'fan (cons (m+ 1. r) test-var))))
	    (return (m* (m// nc dc) (sratsimp ans))))
	   ((and (ratp grand var)
		 (setq ans (zmtorat n (cond ((mtimesp d) d)
					    (t ($sqfr d)))
				    s #'ztorat)))
		   (return (m* (m// nc dc) ans)))
	   ((and (evenfn d var)
		 (setq nn* (p*lognxp n s)))
	    (setq ans (log*rat (car nn*) d (cadr nn*)))
	    (return (m* (m// nc dc) ans)))
	   ((involve grand '(%log))
	    (cond ((setq ans (logquad0 grand))
		   (return (m* (m// nc dc) ans)))
		  (t (return nil)))))
     findout
     (cond ((setq temp (batapp grand))
	    (return temp))
	   (t nil))
     on
     (cond ((let ((*mtoinf* nil))
	      (setq temp (ggr grand t)))
	    (return temp))
	   ((mplusp grand)
	    (cond ((let ((*nodiverg t))
		     (setq ans (catch 'divergent
				 (andmapcar #'(lambda (g)
						(ztoinf g var))
					    (cdr grand)))))
		   (cond ((eq ans 'divergent) nil)
			 (t (return (sratsimp (m+l ans)))))))))

     (cond ((and (evenfn grand var)
		 (setq loopstop* (m+ 1 loopstop*))
		 (setq ans (method-by-limits grand var '$minf '$inf)))
	    (return (m*t '((rat) 1. 2.) ans)))
	   (t (return nil)))))

(defun ztorat (n d s)
  (cond ((and (null *dflag)
	      (setq s (difapply n d s #'ztorat)))
	 s)
	((setq n (let ((plogabs ()))
		   (keyhole (m* `((%plog) ,(m- var)) n) d var)))
	 (m- n))
	(t
	 ;; Let's not signal an error here.  Return nil so that we
	 ;; eventually return a noun form if no other algorithm gives
	 ;; a result.
	 #+(or)
	 (merror (intl:gettext "defint: keyhole integration failed.~%"))
	 nil)))

(setq *dflag nil)

(defun logquad0 (exp)
  (let ((a ()) (b ())  (c ()))
    (cond ((setq exp (logquad exp))
	   (setq a (car exp) b (cadr exp) c (caddr exp))
	   ($asksign b)	  ;let the data base know about the sign of B.
	   (cond ((eq ($asksign c) '$pos)
		  (setq c (m^ (m// c a) '((rat) 1. 2.)))
		  (setq b (simplify
			   `((%acos) ,(add* 'epsilon (m// b (mul* 2. a c))))))
		  (setq a (m// (m* b `((%log) ,c))
			       (mul* a (simplify `((%sin) ,b)) c)))
		  (get-limit a 'epsilon 0 '$plus))))
	  (t ()))))

(defun logquad (exp)
  (let ((varlist (list var)))
    (newvar exp)
    (setq exp (cdr (ratrep* exp)))
    (cond ((and (alike1 (pdis (car exp))
			`((%log) ,var))
		(not (atom (cdr exp)))
		(equal (cadr (cdr exp)) 2.)
		(not (equal (pterm (cddr exp) 0.) 0.)))
	   (setq exp (mapcar 'pdis (cdr (oddelm (cdr exp)))))))))

(defun mtoinf (grand var)
  (prog (ans sd* sn* p* pe* n d s nc dc $savefactors checkfactors temp)
     (setq $savefactors t)
     (setq sn* (setq sd* (list 1.)))
     (cond ((eq ($sign (m+ loopstop* -1)) '$pos)
	    (return nil))
	   ((involve grand '(%sin %cos))
	    (cond ((and (evenfn grand var)
			(or (setq temp (scaxn grand))
			    (setq temp (ssp grand))))
		   (return (m*t 2. temp)))
		  ((setq temp (mtosc grand))
		   (return temp))
		  (t (go en))))
	   ((among '$%i (%einvolve grand))
	    (cond ((setq temp (mtosc grand))
		   (return temp))
		  (t (go en)))))
     (setq grand ($exponentialize grand))	; exponentializing before numden 
     (cond ((polyinx grand var nil)		;  avoids losing multiplicities [ 1309432 ]
	    (diverg))
	   ((and (ratp grand var)
		 (mtimesp grand)
		 (andmapcar #'snumden (cdr grand)))
	    (setq nn* (m*l sn*) sn* nil)
	    (setq dn* (m*l sd*) sd* nil))
	   (t (numden grand)))
     (setq n (rmconst1 nn*))
     (setq d (rmconst1 dn*))
     (setq nc (car n))
     (setq n (cdr n))
     (setq dc (car d))
     (setq d (cdr d))
     (cond ((polyinx d var nil)
	    (setq s (deg d))))
     (cond ((and (not (%einvolve grand))
		 (notinvolve exp '(%sinh %cosh %tanh))
		 (setq p* (findp n))
		 (eq (ask-integer p* '$integer) '$yes)
		 (setq pe* (bxm d s)))
	    (cond ((and (eq (ask-integer (caddr pe*) '$even) '$yes)
			(eq (ask-integer p* '$even) '$yes))
		   (cond ((setq ans (apply 'fan (cons (m+ 1. p*) pe*)))
			  (setq ans (m*t 2. ans))
			  (return (m* (m// nc dc) ans)))))
		  ((equal (car pe*) 1.)
		   (cond ((and (setq ans (apply 'fan (cons (m+ 1. p*) pe*)))
			       (setq nn* (fan (m+ 1. p*)
					      (car pe*)
					      (m* -1 (cadr pe*))
					      (caddr pe*)
					      (cadddr pe*))))
			  (setq ans (m+ ans (m*t (m^ -1 p*) nn*)))
			  (return (m* (m// nc dc) ans))))))))
     (cond ((ratp grand var)
	    (setq ans (m*t '$%pi (zmtorat n (cond ((mtimesp d) d)
						  (t ($sqfr d)))
					  s
					  #'mtorat)))
	    (return (m* (m// nc dc) ans)))
	   ((and (or (%einvolve grand)
		     (involve grand '(%sinh %cosh %tanh)))
		 (p*pin%ex n)	      ;setq's P* and PE*...Barf again.
		 (setq ans (catch 'pin%ex (pin%ex d))))
	    ;; We have an integral of the form p(x)*F(exp(x)), where
	    ;; p(x) is a polynomial.
	    (cond ((null p*)
		   ;; No polynomial
		   (return (dintexp grand var)))
		  ((not (and (zerop1 (get-limit grand var '$inf))
			     (zerop1 (get-limit grand var '$minf))))
		   ;; These limits must exist for the integral to converge.
		   (diverg))
		  ((setq ans (rectzto%pi2 (m*l p*) (m*l pe*) d))
		   ;; This only handles the case when the F(z) is a
		   ;; rational function.
		   (return (m* (m// nc dc) ans)))
		  ((setq ans (log-transform (m*l p*) (m*l pe*) d))
		   ;; If we get here, F(z) is not a rational function.
		   ;; We transform it using the substitution x=log(y)
		   ;; which gives us an integral of the form
		   ;; p(log(y))*F(y)/y, which maxima should be able to
		   ;; handle.
		   (return (m* (m// nc dc) ans)))
		  (t
		   ;; Give up.  We don't know how to handle this.
		   (return nil)))))
     en
     (cond ((setq ans (ggrm grand))
	    (return ans))
	   ((and (evenfn grand var)
		 (setq loopstop* (m+ 1 loopstop*))
		 (setq ans (method-by-limits grand var 0 '$inf)))
	    (return (m*t 2. ans)))
	   (t (return nil)))))

(defun linpower0 (exp var)
  (cond ((and (setq exp (linpower exp var))
	      (eq (ask-integer (caddr exp) '$even)
		  '$yes)
	      (ratgreaterp 0. (car exp)))
	 exp)))

;;; given (b*x+a)^n+c returns  (a b n c)
(defun linpower (exp var)
  (let (linpart deg lc c varlist)
    (cond ((not (polyp exp))   nil)
	  (t (let ((varlist (list var)))
	       (newvar exp)
	       (setq linpart (cadr (ratrep* exp)))
	       (cond ((atom linpart)
		      nil)
		     (t (setq deg (cadr linpart))
;;;get high degree of poly
			(setq linpart ($diff exp var (m+ deg -1)))
;;;diff down to linear.
			(setq lc (sdiff linpart var))
;;;all the way to constant.
			(setq linpart (sratsimp (m// linpart lc)))
			(setq lc (sratsimp (m// lc `((mfactorial) ,deg))))
;;;get rid of factorial from differentiation.
			(setq c (sratsimp (m+ exp (m* (m- lc)
						      (m^ linpart deg)))))))
;;;Sees if can be expressed as (a*x+b)^n + part freeof x.
	       (cond ((not (among var c))
		      `(,lc ,linpart ,deg ,c))
		     (t nil)))))))

(defun mtorat (n d s)
  (let ((*semirat* t))
    (cond ((and (null *dflag)
		(setq s (difapply n d s #'mtorat)))
	   s)
	  (t (csemiup n d var)))))

(defun zmtorat (n d s fn1)
  (prog (c)
     (cond ((eq ($sign (m+ s (m+ 1 (setq nn* (deg n)))))
		'$neg)
	    (diverg))
	   ((eq ($sign (m+ s -4))
		'$neg)
	    (go on)))
     (setq d ($factor d))
     (setq c (rmconst1 d))
     (setq d (cdr c))
     (setq c (car c))
     (cond
       ((mtimesp d)
	(setq d (cdr d))
	(setq n (partnum n d))
	(let ((rsn* t))
	  (setq n ($xthru (m+l
			   (mapcar #'(lambda (a b)
				       (m// (funcall fn1 (car a) b (deg b))
					    (cadr a)))
				   n
				   d)))))
	(return (cond (c (m// n c))
		      (t n)))))
     on

     (setq n (funcall fn1 n d s))
     (return  (sratsimp (cond (c  (m// n c))
			      (t n))))))

(defun pfrnum (f g n n2 var)
  (let ((varlist (list var))  genvar)
    (setq f (polyform f)
	  g (polyform g)
	  n (polyform n)
	  n2 (polyform n2))
    (setq var (caadr (ratrep* var)))
    (setq f (resprog0 f g n n2))
    (list (list (pdis (cadr f)) (pdis (cddr f)))
	  (list (pdis (caar f)) (pdis (cdar f))))))

(defun polyform (e)
  (prog (f d)
     (newvar e)
     (setq f (ratrep* e))
     (and (equal (cddr f) 1)
	  (return (cadr f)))
     (and (equal (length (setq d (cddr f))) 3)
	  (not (among (car d)
		      (cadr f)))
	  (return (list (car d)
			(- (cadr d))
			(ptimes (cadr f) (caddr d)))))
     (merror "defint: bug from PFRNUM in RESIDU.")))

(defun partnum (n dl)
  (let ((n2 1)  ans nl)
    (do ((dl dl (cdr dl)))
	((null (cdr dl))
	 (nconc ans (ncons (list n n2))))
      (setq nl (pfrnum (car dl) (m*l (cdr dl)) n n2 var))
      (setq ans (nconc ans (ncons (car nl))))
      (setq n2 (cadadr nl) n (caadr nl) nl nil))))

(defun ggrm (e)
  (prog (poly expo *mtoinf* mb  varlist  genvar l c gvar)
     (setq varlist (list var))
     (setq *mtoinf* t)
     (cond ((and (setq expo (%einvolve e))
		 (polyp (setq poly (sratsimp (m// e (m^t '$%e expo)))))
		 (setq l (catch 'ggrm (ggr (m^t '$%e expo) nil))))
	    (setq *mtoinf* nil)
	    (setq mb (m- (subin 0. (cadr l))))
	    (setq poly (m+ (subin (m+t mb var) poly)
			   (subin (m+t mb (m*t -1 var)) poly))))
	   (t (return nil)))
     (setq expo (caddr l)
	   c (cadddr l)
	   l (m* -1 (car l))
	   e nil)
     (newvar poly)
     (setq poly (cdr (ratrep* poly)))
     (setq mb (m^ (pdis (cdr poly)) -1)
	   poly (car poly))
     (setq gvar (caadr (ratrep* var)))
     (cond ((or (atom poly)
		(pointergp gvar (car poly)))
	    (setq poly (list 0. poly)))
	   (t (setq poly (cdr poly))))
     (return (do ((poly poly (cddr poly)))
		 ((null poly)
		  (mul* (m^t '$%e c) (m^t expo -1) mb (m+l e)))
	       (setq e (cons (ggrm1 (car poly) (pdis (cadr poly)) l expo)
			     e))))))

(defun ggrm1 (d k a b)
  (setq b (m// (m+t 1. d) b))
  (m* k `((%gamma) ,b) (m^ a (m- b))))

(defun radic (e v)
  ;;If rd* is t the m^ts must just be free of var.
  ;;If rd* is () the m^ts must be mnump's.
  (let ((rd* ()))
    (radicalp e v)))

;; Compute the integral(n/d,x,0,inf) by computing the negative of the
;; sum of residues of log(-x)*n/d over the poles of n/d inside the
;; keyhole contour.  This contour is basically an disk with a slit
;; along the positive real axis.  n/d must be a rational function.
(defun keyhole (n d var)
  (let* ((*semirat* ())
	 (res (res n d
		   #'(lambda (j)
		       ;; Ok if not on the positive real axis.
		       (or (not (equal ($imagpart j) 0))
			   (eq ($asksign j) '$neg)))
		   #'(lambda (j)
		       (cond ((eq ($asksign j) '$pos)
			      t)
			     (t (diverg)))))))
    (when res
      (let ((rsn* t))
	($rectform ($multthru (m+ (cond ((car res)
					 (car res))
					(t 0.))
				  (cond ((cadr res)
					 (cadr res))
					(t 0.)))))))))

;; Look at an expression e of the form sin(r*x)^k, where k is an
;; integer.  Return the list (1 r k).  (Not sure if the first element
;; of the list is always 1 because I'm not sure what partition is
;; trying to do here.)
(defun skr (e)
  (prog (m r k)
     (cond ((atom e) (return nil)))
     (setq e (partition e var 1))
     (setq m (car e))
     (setq e (cdr e))
     (cond ((setq r (sinrx e))
	    (return (list m r 1)))
	   ((and (mexptp e)
		 (eq (ask-integer (setq k (caddr e)) '$integer) '$yes)
		 (setq r (sinrx (cadr e))))
	    (return (list m r k))))))

;; Look at an expression e of the form sin(r*x) and return r.
(defun sinrx (e)
  (cond ((and (consp e) (eq (caar e) '%sin))
	 (cond ((eq (cadr e) var)
		1.)
	       ((and (setq e (partition (cadr e) var 1))
		     (eq (cdr e) var))
		(car e))))))



;; integrate(a*sc(r*x)^k/x^n,x,0,inf).
(defun ssp (exp)
  (prog (u n c arg)
     ;; Get the argument of the involved trig function.
     (when (null (setq arg (involve exp '(%sin %cos))))
       (return nil))
     ;; I don't think this needs to be special.
     #+nil
     (declare (special n))
     ;; Replace (1-cos(arg)^2) with sin(arg)^2.
     (setq exp ($substitute ;(m^t `((%sin) ,var) 2.)
                            ;(m+t 1. (m- (m^t `((%cos) ,var) 2.)))
                            ;; The code from above generates expressions with
                            ;; a missing simp flag. Furthermore, the 
                            ;; substitution has to be done for the complete
                            ;; argument of the trig function. (DK 02/2010)
                            `((mexpt simp) ((%sin simp) ,arg) 2)
                            `((mplus) 1 ((mtimes) -1 ((mexpt) ((%cos) ,arg) 2)))
                            exp))
     (numden exp)
     (setq u nn*)
     (cond ((and (setq n (findp dn*))
		 (eq (ask-integer n '$integer) '$yes))
	    ;; n is the power of the denominator.
	    (cond ((setq c (skr u))
		   ;; The simple case.
		   (return (scmp c n)))
		  ((and (mplusp u)
			(setq c (andmapcar #'skr (cdr u))))
		   ;; Do this for a sum of such terms.
		   (return (m+l (mapcar #'(lambda (j) (scmp j n))
					c)))))))))

;; We have an integral of the form sin(r*x)^k/x^n.  C is the list (1 r k).
;;
;; The substitution y=r*x converts this integral to
;;
;;   r^(n-1)*integral(sin(y)^k/y^n,y,0,inf)
;;
;; (If r is negative, we need to negate the result.)
;;
;; The recursion Wang gives on p. 87 has a typo.  The second term
;; should be subtracted from the first.  This formula is given in G&R,
;; 3.82, formula 12.
;;
;; integrate(sin(x)^r/x^s,x) =
;;    r*(r-1)/(s-1)/(s-2)*integrate(sin(x)^(r-2)/x^(s-2),x)
;;    - r^2/(s-1)/(s-2)*integrate(sin(x)^r/x^(s-2),x)
;;
;; (Limits are assumed to be 0 to inf.)
;;
;; This recursion ends up with integrals with s = 1 or 2 and
;;
;; integrate(sin(x)^p/x,x,0,inf) = integrate(sin(x)^(p-1),x,0,%pi/2)
;;
;; with p > 0 and odd.  This latter integral is known to maxima, and
;; it's value is beta(p/2,1/2)/2.
;;
;; integrate(sin(x)^2/x^2,x,0,inf) = %pi/2*binomial(q-3/2,q-1)
;;
;; where q >= 2.
;;
(defun scmp (c n)
  ;; Compute sign(r)*r^(n-1)*integrate(sin(y)^k/y^n,y,0,inf)
  (destructuring-bind (mult r k)
      c
    (let ((recursion (sinsp k n)))
      (if recursion
	  (m* mult
	      (m^ r (m+ n -1))
	      `((%signum) ,r)
	      recursion)
          ;; Recursion failed.  Return the integrand
          ;; The following code generates expressions with a missing simp flag 
          ;; for the sin function. Use better simplifying code. (DK 02/2010)
;	  (let ((integrand (div (pow `((%sin) ,(m* r var))
;				     k)
;				(pow var n))))
          (let ((integrand (div (power (take '(%sin) (mul r var))
                                       k)
                                (power var n))))
	    (m* mult
		`((%integrate) ,integrand ,var ,ll ,ul)))))))

;; integrate(sin(x)^n/x^2,x,0,inf) = pi/2*binomial(n-3/2,n-1).
;; Express in terms of Gamma functions, though.
(defun sevn (n)
  (m* half%pi ($makegamma `((%binomial) ,(m+t (m+ n -1) '((rat) -1 2))
			    ,(m+ n -1)))))


;; integrate(sin(x)^n/x,x,0,inf) = beta((n+1)/2,1/2)/2, for n odd and
;; n > 0.
(defun sforx (n)
  (cond ((equal n 1.)
	 half%pi)
	(t (bygamma (m+ n -1) 0.))))

;; This implements the recursion for computing
;; integrate(sin(y)^l/y^k,y,0,inf).  (Note the change in notation from
;; the above!)
(defun sinsp (l k)
  (let ((i ())
	(j ()))
    (cond ((eq ($sign (m+ l (m- (m+ k -1))))
	       '$neg)
	   ;; Integral diverges if l-(k-1) < 0.
	   (diverg))
	  ((not (even1 (m+ l k)))
	   ;; If l + k is not even, return NIL.  (Is this the right
	   ;; thing to do?)
	   nil)
	  ((equal k 2.)
	   ;; We have integrate(sin(y)^l/y^2).  Use sevn to evaluate.
	   (sevn (m// l 2.)))
	  ((equal k 1.)
	   ;; We have integrate(sin(y)^l/y,y)
	   (sforx l))
	  ((eq ($sign  (m+ k -2.))
	       '$pos)
	   (setq i (m* (m+ k -1)
		       (setq j (m+ k -2.))))
	   ;; j = k-2, i = (k-1)*(k-2)
	   ;;
	   ;;
	   ;; The main recursion:
	   ;;
	   ;; i(sin(y)^l/y^k)
	   ;;    = l*(l-1)/(k-1)/(k-2)*i(sin(y)^(l-2)/y^k)
	   ;;      - l^2/(k-1)/(k-1)*i(sin(y)^l/y^(k-2))
	   (m+ (m* l (m+ l -1)
		   (m^t i -1)
		   (sinsp (m+ l -2.) j))
	       (m* (m- (m^ l 2))
		   (m^t i -1)
		   (sinsp l j)))))))

;; Returns the fractional part of a?
(defun fpart (a)
  (cond ((null a) 0.)
	((numberp a)
	 ;; Why do we return 0 if a is a number?  Perhaps we really
	 ;; mean integer?
	 0.)
	((mnump a)
	 ;; If we're here, this basically assumes a is a rational.
	 ;; Compute the remainder and return the result.
	 (list (car a) (rem (cadr a) (caddr a)) (caddr a)))
	((and (atom a) (abless1 a)) a)
	((and (mplusp a)
	      (null (cdddr a))
	      (abless1 (caddr a)))
	 (caddr a))))

(defun thrad (e)
  (cond ((polyinx e var nil) 0.)
	((and (mexptp e)
	      (eq (cadr e) var)
	      (mnump (caddr e)))
	 (fpart (caddr e)))
	((mtimesp e)
	 (m+l (mapcar #'thrad e)))))


;;; THE FOLLOWING FUNCTION IS FOR TRIG FUNCTIONS OF THE FOLLOWING TYPE:
;;; LOWER LIMIT=0 B A MULTIPLE OF %PI SCA FUNCTION OF SIN (X) COS (X)
;;; B<=%PI2

(defun period (p e var)
  (and (alike1 (no-err-sub var e) (setq e (no-err-sub (m+ p var) e)))
       ;; means there was no error
       (not (eq e t))))

; returns cons of (integer_part . fractional_part) of a
(defun infr (a)
  ;; I think we really want to compute how many full periods are in a
  ;; and the remainder.
  (let* ((q (igprt (div a (mul 2 '$%pi))))
	 (r (add a (mul -1 (mul q 2 '$%pi)))))
    (cons q r)))


;; Return the integer part of r.
(defun igprt (r)
  ;; r - fpart(r)
  (pretty-good-floor-or-ceiling r '$floor))


;;;Try making exp(%i*var) --> yy, if result is rational then do integral
;;;around unit circle. Make corrections for limits of integration if possible.
(defun scrat (sc b)
  (let* ((exp-form (sconvert sc))	;Exponentialize
	 (rat-form (maxima-substitute 'yy (m^t '$%e (m*t '$%i var))
				      exp-form))) ;Try to make Rational fun.
    (cond ((and (ratp rat-form 'yy)
		(not (among var rat-form)))
	   (cond ((alike1 b %pi2)
		  (let ((ans (zto%pi2 rat-form 'yy)))
		    (cond (ans ans)
			  (t nil))))
		 ((and (eq b '$%pi)
		       (evenfn exp-form var))
		  (let ((ans (zto%pi2 rat-form 'yy)))
		    (cond (ans (m*t '((rat) 1. 2.) ans))
			  (t nil))))
		 ((and (alike1 b half%pi)
		       (evenfn exp-form var)
		       (alike1 rat-form
			       (no-err-sub (m+t '$%pi (m*t -1 var))
					   rat-form)))
		  (let ((ans (zto%pi2 rat-form 'yy)))
		    (cond (ans (m*t '((rat) 1. 4.) ans))
			  (t nil)))))))))

;;; Do integrals of sin and cos. this routine makes sure lower limit
;;; is zero.
(defun intsc1 (a b e)
  ;; integrate(e,var,a,b)
  (let ((trigarg (find-first-trigarg e))
	(var var)
	($%emode t)
	($trigsign t)
	(*sin-cos-recur* t))		;recursion stopper
    (prog (ans d nzp2 l int-zero-to-d int-nzp2 int-zero-to-c limit-diff)
       (let* ((arg (simple-trig-arg trigarg))	;; pattern match sin(cc*x + bb)
	      (cc (cdras 'c arg))
	      (bb (cdras 'b arg))
	      (new-var (gensym "NEW-VAR-")))
	 (when (or (not arg)
		   (not (every-trigarg-alike e trigarg)))
	   (return nil))
	 (when (not (and (equal cc 1) (equal bb 0)))
	   (setq e (div (maxima-substitute (div (sub new-var bb) cc)
					   var e)
			cc))
	   (setq var new-var)	;; change of variables to get sin(new-var)
	   (setq a (add bb (mul a cc)))
	   (setq b (add bb (mul b cc)))))
       (setq limit-diff (m+ b (m* -1 a)))
       (when (or (not (period %pi2 e var))
		 (not (and ($constantp a)
			   ($constantp b))))
	 ;; Exit if b or a is not a constant or if the integrand
	 ;; doesn't appear to have a period of 2 pi.
	 (return nil))
       
       ;; Multiples of 2*%pi in limits.
       (cond ((integerp (setq d (let (($float nil))
				 (m// limit-diff %pi2))))
	      (cond ((setq ans (intsc e %pi2 var))
		     (return (m* d ans)))
		    (t (return nil)))))
       
       ;; The integral is not over a full period (2*%pi) or multiple
       ;; of a full period.  

       ;; Wang p. 111: The integral integrate(f(x),x,a,b) can be
       ;; written as:
       ;;
       ;;   n * integrate(f,x,0,2*%pi) + integrate(f,x,0,c)
       ;;     - integrate(f,x,0,d)
       ;;
       ;; for some integer n and d >= 0, c < 2*%pi because there exist
       ;; integers p and q such that a = 2 * p *%pi + d and b = 2 * q
       ;; * %pi + c.  Then n = q - p.

       ;; Compute q and c for the upper limit b.
       (setq b (infr b))
       (setq l a)
       (cond ((null l)
	      (setq nzp2 (car b))
	      (setq int-zero-to-d 0.)
	      (go out)))
       ;; Compute p and d for the lower limit a.
       (setq l (infr l))
       ;; Compute -integrate(f,x,0,d)
       (setq int-zero-to-d
	     (cond ((setq ans (try-intsc e (cdr l) var))
		    (m*t -1 ans))
		   (t  nil)))
       ;; Compute n = q - p (stored in nzp2)
       (setq nzp2 (m+ (car b) (m- (car l))))
       out
       ;; Compute n*integrate(f,x,0,2*%pi)
       (setq int-nzp2 (cond ((zerop1 nzp2)
			      ;; n = 0
			      0.)
			     ((setq ans (try-intsc e %pi2 var))
			      ;; n is not zero, so compute
			      ;; integrate(f,x,0,2*%pi)
			      (m*t nzp2 ans))
			     ;; Unable to compute integrate(f,x,0,2*%pi)
			     (t nil)))
       ;; Compute integrate(f,x,0,c)
       (setq int-zero-to-c (try-intsc e (cdr b) var))

       (return (cond ((and int-zero-to-d int-nzp2 int-zero-to-c)
		      ;; All three pieces succeeded.
		      (add* int-zero-to-d int-nzp2 int-zero-to-c))
		     ((ratgreaterp %pi2 limit-diff)
		      ;; Less than 1 full period, so intsc can integrate it.
		      ;; Apply the substitution to make the lower limit 0.
		      ;; This is last resort because substitution often causes intsc to fail.
		      (intsc (maxima-substitute (m+ a var) var e)
			     limit-diff var))
		     ;; nothing worked
		     (t nil))))))

;; integrate(sc, var, 0, b), where sc is f(sin(x), cos(x)).
;; calls intsc with a wrapper to just return nil if integral is divergent,
;;  rather than generating an error.
(defun try-intsc (sc b var)
  (let* ((*nodiverg t)
	 (ans (catch 'divergent (intsc sc b var))))
    (if (eq ans 'divergent)
	nil
      ans)))

;; integrate(sc, var, 0, b), where sc is f(sin(x), cos(x)).  I (rtoy)
;; think this expects b to be less than 2*%pi.
(defun intsc (sc b var)
  (if (zerop1 b)
      0
      (multiple-value-bind (b sc)
	  (cond ((eq ($sign b) '$neg)
		 (values (m*t -1 b)
			 (m* -1 (subin (m*t -1 var) sc))))
		(t
		 (values b sc)))
	;; Partition the integrand SC into the factors that do not
	;; contain VAR (the car part) and the parts that do (the cdr
	;; part).
	(setq sc (partition sc var 1))
	(cond ((setq b (intsc0 (cdr sc) b var))
	       (m* (resimplify (car sc)) b))))))

;; integrate(sc, var, 0, b), where sc is f(sin(x), cos(x)).
(defun intsc0 (sc b var)
  ;; Determine if sc is a product of sin's and cos's.
  (let ((nn* (scprod sc))
	(dn* ()))
    (cond (nn*
	   ;; We have a product of sin's and cos's.  We handle some
	   ;; special cases here.
	   (cond ((alike1 b half%pi)
		  ;; Wang p. 110, formula (1):
		  ;; integrate(sin(x)^m*cos(x)^n, x, 0, %pi/2) =
		  ;;   gamma((m+1)/2)*gamma((n+1)/2)/2/gamma((n+m+2)/2)
		  (bygamma (car nn*) (cadr nn*)))
		 ((eq b '$%pi)
		  ;; Wang p. 110, near the bottom, says
		  ;;
		  ;; int(f(sin(x),cos(x)), x, 0, %pi) =
		  ;;   int(f(sin(x),cos(x)) + f(sin(x),-cos(x)),x,0,%pi/2)
		  (cond ((eq (real-branch (cadr nn*) -1) '$yes)
			 (m* (m+ 1. (m^ -1 (cadr nn*)))
			     (bygamma (car nn*) (cadr nn*))))))
		 ((alike1 b %pi2)
		  (cond ((or (and (eq (ask-integer (car nn*) '$even)
				      '$yes)
				  (eq (ask-integer (cadr nn*) '$even)
				      '$yes))
			     (and (ratnump (car nn*))
				  (eq (real-branch (car nn*) -1)
				      '$yes)
				  (ratnump (cadr nn*))
				  (eq (real-branch (cadr nn*) -1)
				      '$yes)))
			 (m* 4.	(bygamma (car nn*) (cadr nn*))))
			((or (eq (ask-integer (car nn*) '$odd) '$yes)
			     (eq (ask-integer (cadr nn*) '$odd) '$yes))
			 0.)
			(t nil)))
		 ((alike1 b half%pi3)
		  ;; Wang, p. 111 says
		  ;;
		  ;; int(f(sin(x),cos(x)),x,0,3*%pi/2) =
		  ;;   int(f(sin(x),cos(x)),x,0,%pi)
		  ;;   + int(f(-sin(x),-cos(x)),x,0,%pi/2)
		  (m* (m+ 1. (m^ -1 (cadr nn*)) (m^ -1 (m+l nn*)))
		      (bygamma (car nn*) (cadr nn*))))))
	  (t
	   ;; We don't have a product of sin's and cos's.
	   (cond ((and (or (eq b '$%pi)
			   (alike1 b %pi2)
			   (alike1 b half%pi))
		       (setq dn* (scrat sc b)))
		  dn*)
		 ((setq nn* (antideriv sc))
		  (sin-cos-intsubs nn* var 0. b))
		 (t ()))))))

;;;Is careful about substitution of limits where the denominator may be zero
;;;because of various assumptions made.
(defun sin-cos-intsubs (exp var ll ul)
  (cond ((mplusp exp)
	 (let ((l (mapcar #'sin-cos-intsubs1 (cdr exp))))
	   (if (not (some #'null l))
	       (m+l l))))
	(t (sin-cos-intsubs1 exp))))

(defun sin-cos-intsubs1 (exp)
  (let* ((rat-exp ($rat exp))
	 (denom (pdis (cddr rat-exp))))
    (cond ((equal ($csign denom) '$zero)
	   '$undefined)
	  (t (try-intsubs exp ll ul)))))

(defun try-intsubs (exp ll ul)
  (let* ((*nodiverg t)
	 (ans (catch 'divergent (intsubs exp ll ul))))
    (if (eq ans 'divergent)
	nil
      ans)))

(defun try-defint (exp var ll ul)
  (let* ((*nodiverg t)
	 (ans (catch 'divergent (defint exp var ll ul))))
    (if (eq ans 'divergent)
	nil
      ans)))

;; Determine whether E is of the form sin(x)^m*cos(x)^n and return the
;; list (m n).
(defun scprod (e)
  (let ((great-minus-1 #'(lambda (temp)
			   (ratgreaterp temp -1)))
	m n)
    (cond
      ((setq m (powerofx e `((%sin) ,var) great-minus-1 var))
       (list m 0.))
      ((setq n (powerofx e `((%cos) ,var) great-minus-1 var))
       (setq m 0.)
       (list 0. n))
      ((and (mtimesp e)
	    (or (setq m (powerofx (cadr e) `((%sin) ,var) great-minus-1 var))
		(setq n (powerofx (cadr e) `((%cos) ,var) great-minus-1 var)))
	    (cond
	      ((null m)
	       (setq m (powerofx (caddr e) `((%sin) ,var) great-minus-1 var)))
	      (t (setq n (powerofx (caddr e) `((%cos) ,var) great-minus-1 var))))
	    (null (cdddr e)))
       (list m n))
      (t ()))))

(defun real-branch (exponent value)
  ;; Says wether (m^t value exponent) has at least one real branch.
  ;; Only works for values of 1 and -1 now.  Returns $yes $no
  ;; $unknown.
  (cond ((equal value 1.)
	 '$yes)
	((eq (ask-integer exponent '$integer) '$yes)
	 '$yes)
	((ratnump exponent)
	 (cond ((eq ($oddp (caddr exponent)) t)
		'$yes)
	       (t '$no)))
	(t '$unknown)))

;; Compute beta((m+1)/2,(n+1)/2)/2.
(defun bygamma (m n)
  (let ((one-half (m//t 1. 2.)))
    (m* one-half `(($beta) ,(m* one-half (m+t 1. m))
		   ,(m* one-half (m+t 1. n))))))

;;Seems like Guys who call this don't agree on what it should return.
(defun powerofx (e x p var)
  (setq e (cond ((not (among var e)) nil)
		((alike1 e x) 1.)
		((atom e) nil)
		((and (mexptp e)
		      (alike1 (cadr e) x)
		      (not (among var (caddr e))))
		 (caddr e))))
  (cond ((null e) nil)
	((funcall p e) e)))


;; Check e for an expression of the form x^kk*(b*x^n+a)^l.  If it
;; matches, Return the two values kk and (list l a n b).
(defun bata0 (e)
  (let (k c)
    (cond ((atom e) nil)
	  ((mexptp e)
	   ;; We have f(x)^y.  Look to see if f(x) has the desired
	   ;; form.  Then f(x)^y has the desired form too, with
	   ;; suitably modified values.
	   ;;
	   ;; XXX: Should we ask for the sign of f(x) if y is not an
	   ;; integer?  This transformation we're going to do requires
	   ;; that f(x)^y be real.
	   (destructuring-bind (mexp base power)
	       e
	     (declare (ignore mexp))
	     (multiple-value-bind (kk cc)
		 (bata0 base)
	       (when kk
		 ;; Got a match.  Adjust kk and cc appropriately.
		 (destructuring-bind (l a n b)
		     cc
		   (values (mul kk power)
			   (list (mul l power) a n b)))))))
	  ((and (mtimesp e)
		(null (cdddr e))
		(or (and (setq k (findp (cadr e)))
			 (setq c (bxm (caddr e) (polyinx (caddr e) var nil))))
		    (and (setq k (findp (caddr e)))
			 (setq c (bxm (cadr e) (polyinx (cadr e) var nil))))))
	   (values k c))
	  ((setq c (bxm e (polyinx e var nil)))
	   (setq k 0.)
	   (values k c)))))


;;(DEFUN BATAP (E)
;;  (PROG (K C L)
;;    (COND ((NOT (BATA0 E)) (RETURN NIL))
;;	  ((AND (EQUAL -1. (CADDDR C))
;;		(EQ ($askSIGN (SETQ K (m+ 1. K)))
;;		    '$pos)
;;		(EQ ($askSIGN (SETQ L (m+ 1. (CAR C))))
;;		    '$pos)
;;		(ALIKE1 (CADR C)
;;			(m^ UL (CADDR C)))
;;		(SETQ E (CADR C))
;;		(EQ ($askSIGN (SETQ C (CADDR C))) '$pos))
;;	   (RETURN (M// (m* (m^ UL (m+t K (m* C (m+t -1. L))))
;;			    `(($BETA) ,(SETQ NN* (M// K C))
;;				      ,(SETQ DN* L)))
;;			C))))))


;; Integrals of the form i(log(x)^m*x^k*(a+b*x^n)^l,x,0,ul).  There
;; are two cases to consider: One case has ul>0, b<0, a=-b*ul^n, k>-1,
;; l>-1, n>0, m a nonnegative integer.  The second case has ul=inf, l < 0.
;;
;; These integrals are essentially partial derivatives of the Beta
;; function (i.e. the Eulerian integral of the first kind).  Note
;; that, currently, with the default setting intanalysis:true, this
;; function might not even be called for some of these integrals.
;; However, this can be palliated by setting intanalysis:false.

(defun zto1 (e)
  (when (or (mtimesp e) (mexptp e))
    (let ((m 0)
	  (log (list '(%log) var)))
      (flet ((set-m (p)
	       (setq m p)))
	(find-if #'(lambda (fac)
		     (powerofx fac log #'set-m var))
		 (cdr e)))
      (when (and (freeof var m)
		 (eq (ask-integer m '$integer) '$yes)
		 (not (eq ($asksign m) '$neg)))
	(setq e (m//t e (list '(mexpt) log m)))
	(cond
	  ((eq ul '$inf)
	   (multiple-value-bind (kk s d r cc)
	       (batap-inf e)
	     ;; We have i(x^kk/(d+cc*x^r)^s,x,0,inf) =
	     ;; beta(aa,bb)/(cc^aa*d^bb*r).  Compute this, and then
	     ;; differentiate it m times to get the log term
	     ;; incorporated.
	     (when kk
	       (let* ((aa (div (add 1 var) r))
		      (bb (sub s aa))
		      (m (if (eq ($asksign m) '$zero)
			     0
			     m)))
	       (let ((res (div `(($beta) ,aa ,bb)
			       (mul (m^t cc aa)
				    (m^t d bb)
				    r))))
		 ($at ($diff res var m)
		      (list '(mequal) var kk)))))))
	  (t
	   (multiple-value-bind
		 (k/n l n b) (batap-new e)
	     (when k/n
	       (let ((beta (simplify (list '($beta) k/n l)))
		     (m (if (eq ($asksign m) '$zero) 0 m)))
		 ;; The result looks like B(k/n,l) ( ... ).
		 ;; Perhaps, we should just $factor, instead of
		 ;; pulling out beta like this.
		 (m*t
		  beta
		  ($fullratsimp
		   (m//t
		    (m*t
		     (m^t (m-t b) (m1-t l))
		     (m^t ul (m*t n (m1-t l)))
		     (m^t n (m-t (m1+t m)))
		     ($at ($diff (m*t (m^t ul (m*t n var))
				      (list '($beta) var l))
				 var m)
			  (list '(mequal) var k/n)))
		    beta))))))))))))


;;; If e is of the form given below, make the obvious change
;;; of variables (substituting ul*x^(1/n) for x) in order to reduce
;;; e to the usual form of the integrand in the Eulerian
;;; integral of the first kind.
;;; N. B: The old version of ZTO1 completely ignored this
;;; substitution; the log(x)s were just thrown in, which,
;;; of course would give wrong results.

(defun batap-new (e)
  ;; Parse e
  (multiple-value-bind (k c)
      (bata0 e)
    (when k
      ;; e=x^k*(a+b*x^n)^l
      (destructuring-bind (l a n b)
	  c
	(when (and (freeof var k)
		   (freeof var n)
		   (freeof var l)
		   (alike1 a (m-t (m*t b (m^t ul n))))
		   (eq ($asksign b) '$neg)
		   (eq ($asksign (setq k (m1+t k))) '$pos)
		   (eq ($asksign (setq l (m1+t l))) '$pos)
		   (eq ($asksign n) '$pos))
	  (values (m//t k n) l n b))))))


;; Wang p. 71 gives the following formula for a beta function:
;;
;; integrate(x^(k-1)/(c*x^r+d)^s,x,0,inf)
;;   = beta(a,b)/(c^a*d^b*r)
;;
;; where a = k/r > 0, b = s - a > 0, s > k > 0, r > 0, c*d > 0.
;;
;; This function matches this and returns k-1, d, r, c, a, b.  And
;; also checks that all the conditions hold.  If not, NIL is returned.
;;
(defun batap-inf (e)
  (multiple-value-bind (k c)
      (bata0 e)
    (when k
      (destructuring-bind (l d r cc)
	  c
	(let* ((s (mul -1 l))
	       (kk (add k 1))
	       (a (div kk r))
	       (b (sub s a)))
	  (when (and (freeof var k)
		     (freeof var r)
		     (freeof var l)
		     (eq ($asksign kk) '$pos)
		     (eq ($asksign a) '$pos)
		     (eq ($asksign b) '$pos)
		     (eq ($asksign (sub s k)) '$pos)
		     (eq ($asksign r) '$pos)
		     (eq ($asksign (mul cc d)) '$pos))
	    (values k s d r cc)))))))


;; Handles beta integrals.
(defun batapp (e)
  (cond ((not (or (equal ll 0)
		  (eq ll '$minf)))
	 (setq e (subin (m+ ll var) e))))
  (multiple-value-bind (k c)
      (bata0 e)
    (cond ((null k)
	   nil)
	  (t
	   (destructuring-bind (l d al c)
	       c
	     ;; e = x^k*(d+c*x^al)^l.
	     (let ((new-k (m// (m+ 1 k) al)))
	       (when (and (ratgreaterp al 0.)
			  (eq ($asksign new-k) '$pos)
			  (ratgreaterp (setq l (m* -1 l))
				       new-k)
			  (eq ($asksign (m* d c))
			      '$pos))
		 (setq l (m+ l (m*t -1 new-k)))
		 (m// `(($beta) ,new-k ,l)
		      (mul* al (m^ c new-k) (m^ d l))))))))))


;; Compute exp(d)*gamma((c+1)/b)/b/a^((c+1)/b).  In essence, this is
;; the value of integrate(x^c*exp(d-a*x^b),x,0,inf).
(defun gamma1 (c a b d)
  (m* (m^t '$%e d)
      (m^ (m* b (m^ a (setq c (m// (m+t c 1) b)))) -1)
      `((%gamma) ,c)))

(defun zto%pi2 (grand var)
  (let ((result (unitcir (sratsimp (m// grand var)) var)))
    (cond (result (sratsimp (m* (m- '$%i) result)))
	  (t nil))))

;; Evaluates the contour integral of GRAND around the unit circle
;; using residues.
(defun unitcir (grand var)
  (numden grand)
  (let* ((sgn nil)
	 (result (princip (res nn* dn* 
			       #'(lambda (pt)
				   ;; Is pt stricly inside the unit circle?
				   (setq sgn (let ((limitp nil))
					       ($asksign (m+ -1 (cabs pt)))))
				   (eq sgn '$neg))
			       #'(lambda (pt)
				   (declare (ignore pt))
				   ;; Is pt on the unit circle?  (Use
				   ;; the cached value computed
				   ;; above.)
				   (prog1
				       (eq sgn '$zero)
				       (setq sgn nil)))))))
    (when result
      (m* '$%pi result))))


(defun logx1 (exp ll ul)
  (let ((arg nil))
    (cond
      ((and (notinvolve exp '(%sin %cos %tan %atan %asin %acos))
	    (setq arg (involve exp '(%log))))
       (cond ((eq arg var)
	      (cond ((ratgreaterp 1. ll)
		     (cond ((not (eq ul '$inf))
			    (intcv1 (m^t '$%e (m- 'yx)) (m- `((%log) ,var))))
			   (t (intcv1 (m^t '$%e 'yx) `((%log) ,var)))))))
	     (t (intcv arg nil)))))))


;; Wang 81-83.  Unfortunately, the pdf version has page 82 as all
;; black, so here is, as best as I can tell, what Wang is doing.
;; Fortunately, p. 81 has the necessary hints.
;;
;; First consider integrate(exp(%i*k*x^n),x) around the closed contour
;; consisting of the real axis from 0 to R, the arc from the angle 0
;; to %pi/(2*n) and the ray from the arc back to the origin.
;;
;; There are no poles in this region, so the integral must be zero.
;; But consider the integral on the three parts.  The real axis is the
;; integral we want.  The return ray is
;;
;;   exp(%i*%pi/2/n) * integrate(exp(%i*k*(t*exp(%i*%pi/2/n))^n),t,R,0)
;;     = exp(%i*%pi/2/n) * integrate(exp(%i*k*t^n*exp(%i*%pi/2)),t,R,0)
;;     = -exp(%i*%pi/2/n) * integrate(exp(-k*t^n),t,0,R)
;;
;; As R -> infinity, this last integral is gamma(1/n)/k^(1/n)/n.
;;
;; We assume the integral on the circular arc approaches 0 as R ->
;; infinity.  (Need to prove this.)
;;
;; Thus, we have
;;
;;   integrate(exp(%i*k*t^n),t,0,inf)
;;     = exp(%i*%pi/2/n) * gamma(1/n)/k^(1/n)/n.
;;
;; Equating real and imaginary parts gives us the desired results:
;;
;; integrate(cos(k*t^n),t,0,inf) = G * cos(%pi/2/n)
;; integrate(sin(k*t^n),t,0,inf) = G * sin(%pi/2/n)
;;
;; where G = gamma(1/n)/k^(1/n)/n.
;;
(defun scaxn (e)
  (let (ind s g)
    (cond ((atom e)  nil)
	  ((and (or (eq (caar e) '%sin)
		    (eq (caar e) '%cos))
		(setq ind (caar e))
		(setq e (bx**n (cadr e))))
	   ;; Ok, we have cos(b*x^n) or sin(b*x^n), and we set e = (n
	   ;; b)
	   (cond ((equal (car e) 1.)
		  ;; n = 1.  Give up.  (Why not divergent?)
		  nil)
		 ((zerop (setq s (let ((sign ($asksign (cadr e))))
				   (cond ((eq sign '$pos) 1)
					 ((eq sign '$neg) -1)
					 ((eq sign '$zero) 0)))))
		  ;; s is the sign of b.  Give up if it's zero.
		  nil)
		 ((not (eq ($asksign (m+ -1 (car e)))  '$pos))
		  ;; Give up if n-1 <= 0.  (Why give up?  Isn't the
		  ;; integral divergent?)
		  nil)
		 (t
		  ;; We can apply our formula now.  g = gamma(1/n)/n/b^(1/n)
		  (setq g (gamma1 0. (m* s (cadr e)) (car e) 0.))
		  (setq e (m* g `((,ind) ,(m// half%pi (car e)))))
		  (m* (cond ((and (eq ind '%sin)
				  (equal s -1))
			     -1)
			    (t 1))
		      e)))))))


;; this is the second part of the definite integral package

(declare-top(special var plm* pl* rl* pl*1 rl*1))

(defun p*lognxp (a s)
  (let (b)
    (cond ((not (among '%log a))
	   ())
	  ((and (polyinx (setq b (maxima-substitute 1. `((%log) ,var) a))
			 var t)
		(eq ($sign (m+ s (m+ 1 (deg b))))
		    '$pos)
		(evenfn b var)
		(setq a (lognxp (sratsimp (m// a b)))))
	   (list b a)))))

(defun lognxp (a)
  (cond ((atom a) nil)
	((and (eq (caar a) '%log)
	      (eq (cadr a) var)) 1.)
	((and (mexptp a)
	      (numberp (caddr a))
	      (lognxp (cadr a)))
	 (caddr a))))

(defun logcpi0 (n d)
  (prog (pl dp)
     (setq pl (polelist d #'upperhalf #'(lambda (j)
					  (cond ((zerop1 j) nil)
						((equal ($imagpart j) 0)
						 t)))))
     (cond ((null pl)
	    (return nil)))
     (setq factors (car pl)
	   pl (cdr pl))
     (cond ((or (cadr pl)
		(caddr pl))
	    (setq dp (sdiff d var))))
     (cond ((setq plm* (car pl))
	    (setq rlm* (residue n (cond (leadcoef factors)
					(t d))
				plm*))))
     (cond ((setq pl* (cadr pl))
	    (setq rl* (res1 n dp pl*))))
     (cond ((setq pl*1 (caddr pl))
	    (setq rl*1 (res1 n dp pl*1))))
     (return (m*t (m//t 1. 2.)
		  (m*t '$%pi
		       (princip
			(list (cond ((setq nn* (append rl* rlm*))
				     (m+l nn*)))
			      (cond (rl*1 (m+l rl*1))))))))))

(defun lognx2 (nn dn pl rl)
  (do ((pl pl (cdr pl))
       (rl rl (cdr rl))
       (ans ()))
      ((or (null pl)
	   (null rl))  ans)
    (setq ans (cons (m* dn (car rl) (m^ `((%plog) ,(car pl)) nn))
		    ans))))

(defun logcpj (n d i)
  (setq n (append
	   (if plm*
	       (list (mul* (m*t '$%i %pi2)
			   (m+l
			    (residue (m* (m^ `((%plog) ,var) i)	 n)
				     d
				     plm*)))))
	   (lognx2 i (m*t '$%i %pi2) pl* rl*)
	   (lognx2 i %p%i pl*1 rl*1)))
  (if (null n)
      0
      (simplify (m+l n))))

;; Handle integral(n(x)/d(x)*log(x)^m,x,0,inf).  n and d are
;; polynomials.
(defun log*rat (n d m)
  (declare (special *i* *j*))
  (setq *i* (make-array (1+ m)))
  (setq *j* (make-array (1+ m)))
  (setf (aref *j* 0) 0)
  (prog (leadcoef factors plm* pl* rl* pl*1 rl*1 rlm*)
     (dotimes (c m (return (logcpi n d m)))
       (setf (aref *i* c) (logcpi n d c))
       (setf (aref *j* c) (logcpj n factors c)))))

(defun logcpi (n d c)
  (declare (special *j*))
  (if (zerop c)
      (logcpi0 n d)
      (m* '((rat) 1 2) (m+ (aref *j* c) (m* -1 (sumi c))))))

(defun sumi (c)
  (declare (special *i*))
  (do ((k 1 (1+ k))
       (ans ()))
      ((= k c)
       (m+l ans))
    (push (mul* ($makegamma `((%binomial) ,c ,k))
		(m^t '$%pi k)
		(m^t '$%i k)
		(aref *i* (- c k)))
	  ans)))

(defun fan (p m a n b)
  (let ((povern (m// p n))
	(ab (m// a b)))
    (cond
      ((or (eq (ask-integer povern '$integer) '$yes)
	   (not (equal ($imagpart ab) 0)))  ())
      (t (let ((ind ($asksign ab)))
	   (cond ((eq ind '$zero) nil)
		 ((eq ind '$neg) nil)
		 ((not (ratgreaterp m povern)) nil)
		 (t (m// (m* '$%pi
			     ($makegamma `((%binomial) ,(m+ -1 m (m- povern))
					   ,(m+t -1 m)))
			     `((mabs) ,(m^ a (m+ povern (m- m)))))
			 (m* (m^ b povern)
			     n
			     `((%sin) ,(m*t '$%pi povern)))))))))))


;;Makes a new poly such that np(x)-np(x+2*%i*%pi)=p(x).
;;Constructs general POLY of degree one higher than P with
;;arbitrary coeff. and then solves for coeffs by equating like powers
;;of the varibale of integration.
;;Can probably be made simpler now.

(defun makpoly (p)
  (let ((n (deg p))  (ans ())  (varlist ())  (gp ())  (cl ())  (zz ()))
    (setq ans (genpoly (m+ 1 n))) ;Make poly with gensyms of 1 higher deg.
    (setq cl (cdr ans))			;Coefficient list
    (setq varlist (append cl (list var))) ;Make VAR most important.
    (setq gp (car ans))		 ;This is the poly with gensym coeffs.
;;;Now, poly(x)-poly(x+2*%i*%pi)=p(x), P is the original poly.
    (setq ans (m+ gp (subin (m+t (m*t '$%i %pi2) var) (m- gp)) (m- p)))
    (newvar ans)
    (setq ans (ratrep* ans))	       ;Rational rep with VAR leading.
    (setq zz (coefsolve n cl (cond ((not (eq (caadr ans) ;What is Lead Var.
					     (genfind (car ans) var)))
				    (list 0 (cadr ans))) ;No VAR in ans.
				   ((cdadr ans))))) ;The real Poly.
    (if (or (null zz) (null gp))
	-1
	($substitute zz gp))))	       ;Substitute Values for gensyms.

(defun coefsolve (n cl e)
  (do (($breakup)
       (eql (ncons (pdis (pterm e n))) (cons (pdis (pterm e m)) eql))
       (m (m+ n -1) (m+ m -1)))
      ((signp l m) (solvex eql cl nil nil))))

;; Integrate(p(x)*f(exp(x))/g(exp(x)),x,minf,inf) by applying the
;; transformation y = exp(x) to get
;; integrate(p(log(y))*f(y)/g(y)/y,y,0,inf).  This should be handled
;; by dintlog.
(defun log-transform (p pe d)
  (let ((new-p (subst (list '(%log) var) var p))
	(new-pe (subst var 'z* (catch 'pin%ex (pin%ex pe))))
	(new-d (subst var 'z* (catch 'pin%ex (pin%ex d)))))
    (defint (div (div (mul new-p new-pe) new-d) var) var 0 ul)))

;; This implements Wang's algorithm in Chapter 5.2, pp. 98-100.
;;
;; This is a very brief description of the algorithm.  Basically, we
;; have integrate(R(exp(x))*p(x),x,minf,inf), where R(x) is a rational
;; function and p(x) is a polynomial.
;;
;; We find a polynomial q(x) such that q(x) - q(x+2*%i*%pi) = p(x).
;; Then consider a contour integral of R(exp(z))*q(z) over a
;; rectangular contour.  Opposite corners of the rectangle are (-R,
;; 2*%i*%pi) and (R, 0).
;;
;; Wang shows that this contour integral, in the limit, is the
;; integral of R(exp(x))*q(x)-R(exp(x))*q(x+2*%i*%pi), which is
;; exactly the integral we're looking for.
;;
;; Thus, to find the value of the contour integral, we just need the
;; residues of R(exp(z))*q(z).  The only tricky part is that we want
;; the log function to have an imaginary part between 0 and 2*%pi
;; instead of -%pi to %pi.
(defun rectzto%pi2 (p pe d)
  ;; We have R(exp(x))*p(x) represented as p(x)*pe(exp(x))/d(exp(x)).
  (prog (dp n pl a b c denom-exponential)
     (if (not (and (setq denom-exponential (catch 'pin%ex (pin%ex d)))
		   (%e-integer-coeff pe)
		   (%e-integer-coeff d)))
	 (return ()))
     ;; At this point denom-exponential has converted d(exp(x)) to the
     ;; polynomial d(z), where z = exp(x).
     (setq n (m* (cond ((null p) -1)
		       (t ($expand (m*t '$%i %pi2 (makpoly p)))))
		 pe))
     (let ((var 'z*)
	   (leadcoef ()))
       ;; Find the poles of the denominator.  denom-exponential is the
       ;; denominator of R(x).
       ;;
       ;; It seems as if polelist returns a list of several items.
       ;; The first element is a list consisting of the pole and (z -
       ;; pole).  We don't care about this, so we take the rest of the
       ;; result.
       (setq pl (cdr (polelist denom-exponential
			       #'(lambda (j)
				   ;; The imaginary part is nonzero,
				   ;; or the realpart is negative.
				   (or (not (equal ($imagpart j) 0))
				       (eq ($asksign ($realpart j)) '$neg)))
			       #'(lambda (j)
				   ;; The realpart is not zero.
				   (not (eq ($asksign ($realpart j)) '$zero)))))))
     ;; Not sure what this does.
     (cond ((null pl)
	    ;; No roots at all, so return
	    (return nil))
	   ((or (cadr pl)
		(caddr pl))
	    ;; We have simple roots or roots in REGION1
	    (setq dp (sdiff d var))))
     (cond ((cadr pl)
	    ;; The cadr of pl is the list of the simple poles of
	    ;; denom-exponential.  Take the log of them to find the
	    ;; poles of the original expression.  Then compute the
	    ;; residues at each of these poles and sum them up and put
	    ;; the result in B.  (If no simple poles set B to 0.)
	    (setq b (mapcar #'log-imag-0-2%pi (cadr pl)))
	    (setq b (res1 n dp b))
	    (setq b (m+l b)))
	   (t (setq b 0.)))
     (cond ((caddr pl)
	    ;; I think this handles the case of poles outside the
	    ;; regions.  The sum of these residues are placed in C.
	    (let ((temp (mapcar #'log-imag-0-2%pi (caddr pl))))
	      (setq c (append temp (mapcar #'(lambda (j)
					       (m+ (m*t '$%i %pi2) j))
					   temp)))
	      (setq c (res1 n dp c))
	      (setq c (m+l c))))
	   (t (setq c 0.)))
     (cond ((car pl)
	    ;; We have the repeated poles of deonom-exponential, so we
	    ;; need to convert them to the actual pole values for
	    ;; R(exp(x)), by taking the log of the value of poles.
	    (let ((poles (mapcar #'(lambda (p)
				     (log-imag-0-2%pi (car p)))
				 (car pl)))
		  (exp (m// n (subst (m^t '$%e var) 'z* denom-exponential))))
	      ;; Compute the residues at all of these poles and sum
	      ;; them up.
	      (setq a (mapcar #'(lambda (j)
				  ($residue exp var j))
			      poles))
	      (setq a (m+l a))))
	   (t (setq a 0.)))
     (return (sratsimp (m+ a b (m* '((rat) 1. 2.) c))))))

(defun genpoly (i)
  (do ((i i (m+ i -1))
       (c (gensym) (gensym))
       (cl ())
       (ans ()))
      ((zerop i)
       (cons (m+l ans) cl))
    (setq ans (cons (m* c (m^t var i)) ans))
    (setq cl (cons c cl))))

;; Check to see if each term in exp that is of the form exp(k*x) has
;; an integer value for k.
(defun %e-integer-coeff (exp)
  (cond ((mapatom exp) t)
	((and (mexptp exp)
	      (eq (cadr exp) '$%e))
	 (eq (ask-integer ($coeff (caddr exp) var) '$integer)
	     '$yes))
	(t (every '%e-integer-coeff (cdr exp)))))

(defun wlinearpoly (e var)
  (cond ((and (setq e (polyinx e var t))
	      (equal (deg e) 1))
	 (subin 1 e))))

;; Test to see if exp is of the form f(exp(x)), and if so, replace
;; exp(x) with 'z*.  That is, basically return f(z*).
(defun pin%ex (exp)
  (declare (special $exponentialize))
  (pin%ex0 (cond ((notinvolve exp '(%sinh %cosh %tanh))
		  exp)
		 (t
		  (let (($exponentialize t))
		    (setq exp ($expand exp)))))))

(defun pin%ex0 (e)
  ;; Does e really need to be special here?  Seems to be ok without
  ;; it; testsuite works.
  #+nil
  (declare (special e))
  (cond ((not (among var e))
	 e)
	((atom e)
	 (throw 'pin%ex nil))
	((and (mexptp e)
	      (eq (cadr e)  '$%e))
	 (cond ((eq (caddr e) var)
		'z*)
	       ((let ((linterm (wlinearpoly (caddr e) var)))
		  (and linterm
		       (m* (subin 0 e) (m^t 'z* linterm)))))
	       (t
		(throw 'pin%ex nil))))
	((mtimesp e)
	 (m*l (mapcar #'pin%ex0 (cdr e))))
	((mplusp e)
	 (m+l (mapcar #'pin%ex0 (cdr e))))
	(t
	 (throw 'pin%ex nil))))

;; Test to see if exp is of the form p(x)*f(exp(x)).  If so, set p* to
;; be p(x) and set pe* to f(exp(x)).
(defun p*pin%ex (nd*)
  (setq nd* ($factor nd*))
  (cond ((polyinx nd* var nil)
	 (setq p* (cons nd* p*)) t)
	((catch 'pin%ex (pin%ex nd*))
	 (setq pe* (cons nd* pe*)) t)
	((mtimesp nd*)
	 (andmapcar #'p*pin%ex (cdr nd*)))))

(defun findsub (p)
  (cond ((findp p) nil)
	((setq nd* (bx**n p))
	 (m^t var (car nd*)))
	((setq p (bx**n+a p))
	 (m* (caddr p) (m^t var (cadr p))))))

;; I think this is looking at f(exp(x)) and tries to find some
;; rational function R and some number k such that f(exp(x)) =
;; R(exp(k*x)).
(defun funclogor%e (e)
  (prog (ans arg nvar r)
     (cond ((or (ratp e var)
		(involve e '(%sin %cos %tan))
		(not (setq arg (xor (and (setq arg (involve e '(%log)))
					 (setq r '%log))
				    (%einvolve e)))))
	    (return nil)))
     ag (setq nvar (cond ((eq r '%log) `((%log) ,arg))
			 (t (m^t '$%e arg))))
     (setq ans (maxima-substitute (m^t 'yx -1) (m^t nvar -1) (maxima-substitute 'yx nvar e)))
     (cond ((not (among var ans))  (return (list (subst var 'yx ans) nvar)))
	   ((and (null r)
		 (setq arg (findsub arg)))
	    (go ag)))))

;; Integration by parts.
;;
;; integrate(u(x)*diff(v(x),x),x,a,b)
;;              |b
;;   = u(x)*v(x)| - integrate(v(x)*diff(u(x),x))
;;              |a
;;
(defun dintbypart (u v a b)
;;;SINCE ONLY CALLED FROM DINTLOG TO get RID OF LOGS - IF LOG REMAINS, QUIT
  (let ((ad (antideriv v)))
    (cond ((or (null ad)
	       (involve ad '(%log)))
	   nil)
	  (t (let ((p1 (m* u ad))
		   (p2 (m* ad (sdiff u var))))
	       (let ((p1-part1 (get-limit p1 var b '$minus))
		     (p1-part2 (get-limit p1 var a '$plus)))
		 (cond ((or (null p1-part1)
			    (null p1-part2))
			nil)
		       (t (let ((p2 (let ((*def2* t))
				      (defint p2 var a b))))
			    (cond (p2 (add* p1-part1
					    (m- p1-part2)
					    (m- p2)))
				  (t nil)))))))))))

;; integrate(f(exp(k*x)),x,a,b), where f(z) is rational.
;;
;; See Wang p. 96-97.
;;
;; If the limits are minf to inf, we use the substitution y=exp(k*x)
;; to get integrate(f(y)/y,y,0,inf)/k.  If the limits are 0 to inf,
;; use the substitution s+1=exp(k*x) to get
;; integrate(f(s+1)/(s+1),s,0,inf).
(defun dintexp (exp arg &aux ans)
  (let ((*dintexp-recur* t))		;recursion stopper
    (cond ((and (sinintp exp var)     ;To be moved higher in the code.
		(setq ans (antideriv exp))
		(setq ans (intsubs ans ll ul)))
	   ;; If we can integrate it directly, do so and take the
	   ;; appropriate limits.
	   )
	  ((setq ans (funclogor%e exp))
	   ;; ans is the list (f(x) exp(k*x)).
	   (cond ((and (equal ll 0.)
		       (eq ul '$inf))
		  ;; Use the substitution s + 1 = exp(k*x).  The
		  ;; integral becomes integrate(f(s+1)/(s+1),s,0,inf)
		  (setq ans (m+t -1 (cadr ans))))
		 (t
		  ;; Use the substitution y=exp(k*x) because the
		  ;; limits are minf to inf.
		  (setq ans (cadr ans))))
	   ;; Apply the substitution and integrate it.
	   (intcv ans nil)))))

;; integrate(log(g(x))*f(x),x,0,inf)
(defun dintlog (exp arg)
  (let ((*dintlog-recur* (1+ *dintlog-recur*))) ;recursion stopper
    (prog (ans d)
       (cond ((and (eq ul '$inf)
		   (equal ll 0.)
		   (eq arg var)
		   (equal 1 (sratsimp (m// exp (m* (m- (subin (m^t var -1)
							       exp))
						    (m^t var -2))))))
	      ;; Make the substitution y=1/x.  If the integrand has
	      ;; exactly the same form, the answer has to be 0.
	      (return 0.))
             ((and (setq ans (let (($gamma_expand t)) (logx1 exp ll ul)))
		   (free ans '%limit))
	      (return ans))
	     ((setq ans (antideriv exp))
	      ;; It's easy if we have the antiderivative.
	      ;; but intsubs sometimes gives results containing %limit
	      (return (intsubs ans ll ul))))
       ;; Ok, the easy cases didn't work.  We now try integration by
       ;; parts.  Set ANS to f(x).
       (setq ans (m// exp `((%log) ,arg)))
       (cond ((involve ans '(%log))
	      ;; Bad. f(x) contains a log term, so we give up.
	      (return nil))
	     ((and (eq arg var)
		   (equal 0. (no-err-sub 0. ans))
		   (setq d (let ((*def2* t))
			     (defint (m* ans (m^t var '*z*))
				 var ll ul))))
	      ;; The arg of the log function is the same as the
	      ;; integration variable.  We can do something a little
	      ;; simpler than integration by parts.  We have something
	      ;; like f(x)*log(x).  Consider f(x)*x^z.  If we
	      ;; differentiate this wrt to z, the integrand becomes
	      ;; f(x)*log(x)*x^z.  When we evaluate this at z = 0, we
	      ;; get the desired integrand.
	      ;;
	      ;; So we need f(0) to be 0 at 0.  If we can integrate
	      ;; f(x)*x^z, then we differentiate the result and
	      ;; evaluate it at z = 0.
	      (return (derivat '*z* 1. d 0.)))
	     ((setq ans (dintbypart `((%log) ,arg) ans ll ul))
	      ;; Try integration by parts.
	      (return ans))))))

;; Compute diff(e,var,n) at the point pt.
(defun derivat (var n e pt)
  (subin pt (apply '$diff (list e var n))))

;;; GGR and friends

;; MAYBPC returns (COEF EXPO CONST)
;;
;; This basically picks off b*x^n+a and returns the list
;; (b n a).  It may also set the global *zd*.
(defun maybpc (e var)
  (declare (special *zd*))
  (cond (*mtoinf* (throw 'ggrm (linpower0 e var)))
	((and (not *mtoinf*)
	      (null (setq e (bx**n+a e)))) ;bx**n+a --> (a n b) or nil.
	 nil)				;with var being x.
	;; At this point, e is of the form (a n b)
	((and (among '$%i (caddr e))
	      (zerop1 ($realpart (caddr e)))
	      (setq zn ($imagpart (caddr e)))
	      (eq ($asksign (cadr e)) '$pos))
	 ;; If we're here, b is complex, and n > 0.  zn = imagpart(b).
	 ;;
	 ;; Set var to the same sign as zn.
	 (cond ((eq ($asksign zn) '$neg)
		(setq var -1)
		(setq zn (m- zn)))
	       (t (setq var 1)))
	 ;; zd = exp(var*%i*%pi*(1+nd)/(2*n). (ZD is special!)
	 (setq *zd* (m^t '$%e (m// (mul* var '$%i '$%pi (m+t 1 nd*))
				   (m*t 2 (cadr e)))))
	 ;; Return zn, n, a.
	 `(,(caddr e) ,(cadr e) ,(car e)))
	((and (or (eq (setq var ($asksign ($realpart (caddr e)))) '$neg)
		  (equal var '$zero))
	      (equal ($imagpart (cadr e)) 0)
	      (ratgreaterp (cadr e) 0.))
	 ;; We're here if realpart(b) <= 0, and n >= 0.  Then return -b, n, a.
	 `(,(caddr e) ,(cadr e) ,(car e)))))

;; Integrate x^m*exp(b*x^n+a), with realpart(m) > -1.
;;
;; See Wang, pp. 84-85.
;;
;; I believe the formula Wang gives is incorrect.  The derivation is
;; correct except for the last step.
;;
;; Let J = integrate(x^m*exp(%i*k*x^n),x,0,inf), with real k.
;;
;; Consider the case for k < 0.  Take a sector of a circle bounded by
;; the real line and the angle -%pi/(2*n), and by the radii, r and R.
;; Since there are no poles inside this contour, the integral
;;
;; integrate(z^m*exp(%i*k*z^n),z) = 0
;;
;; Then J = exp(-%pi*%i*(m+1)/(2*n))*integrate(R^m*exp(k*R^n),R,0,inf)
;;
;; because the integral along the boundary is zero except for the part
;; on the real axis.  (Proof?)
;;
;; Wang seems to say this last integral is gamma(s/n/(-k)^s) where s =
;; (m+1)/n.  But that seems wrong.  If we use the substitution R =
;; (y/(-k))^(1/n), we end up with the result:
;;
;;   integrate(y^((m+1)/n-1)*exp(-y),y,0,inf)/(n*k^((m+1)/n).
;;
;; or gamma((m+1)/n)/k^((m+1)/n)/n.
;;
;; Note that this also handles the case of
;;
;;   integrate(x^m*exp(-k*x^n),x,0,inf);
;;
;; where k is positive real number.  A simple change of variables,
;; y=k*x^n, gives
;;
;;   integrate(y^((m+1)/n-1)*exp(-y),y,0,inf)/(n*k^((m+1)/n))
;;
;; which is the same form above.
(defun ggr (e ind)
  (prog (c *zd* zn nn* dn* nd* dosimp $%emode)
     (declare (special *zd*))
     (setq nd* 0.)
     (cond (ind (setq e ($expand e))
		(cond ((and (mplusp e)
			    (let ((*nodiverg t))
			      (setq e (catch 'divergent
					(andmapcar
					 #'(lambda (j)
					     (ggr j nil))
					 (cdr e))))))
		       (cond ((eq e 'divergent) nil)
			     (t (return (sratsimp (cons '(mplus) e)))))))))
     (setq e (rmconst1 e))
     (setq c (car e))
     (setq e (cdr e))
     (cond ((setq e (ggr1 e var))
	    ;; e = (m b n a).  That is, the integral is of the form
	    ;; x^m*exp(b*x^n+a).  I think we want to compute
	    ;; gamma((m+1)/n)/b^((m+1)/n)/n.
	    ;;
	    ;; FIXME: If n > m + 1, the integral converges.  We need
	    ;; to check for this.
	    (destructuring-bind (m b n a)
		e
	      (when (and (not (zerop1 ($realpart b)))
			 (not (zerop1 ($imagpart b))))
		;; The derivation only holds if b is purely real or
		;; purely imaginary.  Give up if it's not.
		(return nil))
	      ;; Check for convergence.  If b is complex, we need n -
	      ;; m > 1.  If b is real, we need b < 0.
	      (when (and (zerop1 ($imagpart b))
			 (not (eq ($asksign b) '$neg)))
		(diverg))
	      (when (and (not (zerop1 ($imagpart b)))
			 (not (eq ($asksign (sub n (add m 1))) '$pos)))
		(diverg))

	      (setq e (gamma1 m (cond ((zerop1 ($imagpart b))
				       ;; If we're here, b must be negative.
				       (neg b))
				      (t
				       ;; Complex b.  Take the imaginary part
				       `((mabs) ,($imagpart b))))
			      n a))
	      ;; NOTE: *zd* (Ick!) is special and might be set by maybpc.
	      (when *zd*
		;; FIXME: Why do we set %emode here?  Shouldn't we just
		;; bind it?  And why do we want it bound to T anyway?
		;; Shouldn't the user control that?  The same goes for
		;; dosimp.
		;;(setq $%emode t)
		(setq dosimp t)
		(setq e (m* *zd* e))))))
     (cond (e (return (m* c e))))))


;; Match x^m*exp(b*x^n+a).  If it does, return (list m b n a).
(defun ggr1 (e var)
  (cond ((atom e) nil)
	((and (mexptp e)
	      (eq (cadr e) '$%e))
	 ;; We're looking at something like exp(f(var)).  See if it's
	 ;; of the form b*x^n+a, and return (list 0 b n a).  (The 0 is
	 ;; so we can graft something onto it if needed.)
	 (cond ((setq e (maybpc (caddr e) var))
		(cons 0. e))))
	((and (mtimesp e)
	      ;; E should be the product of exactly 2 terms
	      (null (cdddr e))
	      ;; Check to see if one of the terms is of the form
	      ;; var^p.  If so, make sure the realpart of p > -1.  If
	      ;; so, check the other term has the right form via
	      ;; another call to ggr1.
	      (or (and (setq dn* (xtorterm (cadr e) var))
		       (ratgreaterp (setq nd* ($realpart dn*))
				    -1.)
		       (setq nn* (ggr1 (caddr e) var)))
		  (and (setq dn* (xtorterm (caddr e) var))
		       (ratgreaterp (setq nd* ($realpart dn*))
				    -1.)
		       (setq nn* (ggr1 (cadr e) var)))))
	 ;; Both terms have the right form and nn* contains the arg of
	 ;; the exponential term.  Put dn* as the car of nn*.  The
	 ;; result is something like (m b n a) when we have the
	 ;; expression x^m*exp(b*x^n+a).
	 (rplaca nn* dn*))))


;; Match b*x^n+a.  If a match is found, return the list (a n b).
;; Otherwise, return NIL
(defun bx**n+a (e)
  (cond ((eq e var)
	 (list 0 1 1))
	((or (atom e)
	     (mnump e)) ())
	(t (let ((a (no-err-sub 0. e)))
	     (cond ((null a)  ())
		   (t (setq e (m+ e (m*t -1 a)))
		      (cond ((setq e (bx**n e))
			     (cons a e))
			    (t ()))))))))

;; Match b*x^n.  Return the list (n b) if found or NIL if not.
(defun bx**n (e)
  (let ((n ()))
    (and (setq n (xexponget e var))
	 (not (among var
		     (setq e (let (($maxposex 1)
				   ($maxnegex 1))
			       ($expand (m// e (m^t var n)))))))
	 (list n e))))

(defun xexponget (e nn*)
  (cond ((atom e) (cond ((eq e var) 1.)))
	((mnump e) nil)
	((and (mexptp e)
	      (eq (cadr e) nn*)
	      (not (among nn* (caddr e))))
	 (caddr e))
	(t (some #'(lambda (j) (xexponget j nn*)) (cdr e)))))


;;; given (b*x^n+a)^m returns (m a n b)
(defun bxm (e ind)
  (let (m r)
    (cond ((or (atom e)
	       (mnump e)
	       (involve e '(%log %sin %cos %tan))
	       (%einvolve e))  nil)
	  ((mtimesp e)  nil)
	  ((mexptp e)  (cond ((among var (caddr e))  nil)
			     ((setq r (bx**n+a (cadr e)))
			      (cons (caddr e) r))))
	  ((setq r (bx**n+a e))  (cons 1. r))
	  ((not (null ind))
;;;Catches Unfactored forms.
	   (setq m (m// (sdiff e var) e))
	   (numden m)
	   (setq m nn*)
	   (setq r dn*)
	   (cond
	     ((and (setq r (bx**n+a (sratsimp r)))
		   (not (among var (setq m (m// m (m* (cadr r) (caddr r)
						      (m^t var (m+t -1 (cadr r))))))))
		   (setq e (m// (subin 0. e) (m^t (car r) m))))
	      (cond ((equal e 1.)
		     (cons m r))
		    (t (setq e (m^ e (m// 1. m)))
		       (list m (m* e (car r)) (cadr r)
			     (m* e (caddr r))))))))
	  (t ()))))

;;;Is E = VAR raised to some power? If so return power or 0.
(defun findp (e)
  (cond ((not (among var e)) 0.)
	(t (xtorterm e var))))

(defun xtorterm (e var1)
;;;Is E = VAR1 raised to some power? If so return power.
  (cond ((alike1 e var1) 1.)
	((atom e) nil)
	((and (mexptp e)
	      (alike1 (cadr e) var1)
	      (not (among var (caddr e))))
	 (caddr e))))

(defun tbf (l)
  (m^ (m* (m^ (caddr l) '((rat) 1 2))
	  (m+ (cadr l) (m^ (m* (car l) (caddr l))
			   '((rat) 1 2))))
      -1))

(defun radbyterm (d l)
  (do ((l l (cdr l))
       (ans ()))
      ((null l)
       (m+l ans))
    (destructuring-let (((const . integrand) (rmconst1 (car l))))
      (setq ans (cons (m* const (dintrad0 integrand d))
		      ans)))))

(defun sqdtc (e ind)
  (prog (a b c varlist)
     (setq varlist (list var))
     (newvar e)
     (setq e (cdadr (ratrep* e)))
     (setq c (pdis (pterm e 0)))
     (setq b (m*t (m//t 1 2) (pdis (pterm e 1))))
     (setq a (pdis (pterm e 2)))
     (cond ((and (eq ($asksign (m+ b (m^ (m* a c)
					 '((rat) 1 2))))
		     '$pos)
		 (or (and ind
			  (not (eq ($asksign a) '$neg))
			  (eq ($asksign c) '$pos))
		     (and (eq ($asksign a) '$pos)
			  (not (eq ($asksign c) '$neg)))))
	    (return (list a b c))))))

(defun difap1 (e pwr var m pt)
  (m// (mul* (cond ((eq (ask-integer m '$even) '$yes)
		    1)
		   (t -1))
	     `((%gamma) ,pwr)
	     (derivat var m e pt))
       `((%gamma) ,(m+ pwr m))))

(defun sqrtinvolve (e)
  (cond ((atom e) nil)
	((mnump e) nil)
	((and (mexptp e)
	      (and (mnump (caddr e))
		   (not (numberp (caddr e)))
		   (equal (caddr (caddr e)) 2.))
	      (among var (cadr e)))
	 (cadr e))
	(t (some #'sqrtinvolve (cdr e)))))

(defun bydif (r s d)
  (let ((b 1)  p)
    (setq d (m+ (m*t '*z* var) d))
    (cond ((or (zerop1 (setq p (m+ s (m*t -1 r))))
	       (and (zerop1 (m+ 1 p))
		    (setq b var)))
	   (difap1 (dintrad0 b (m^ d '((rat) 3 2)))
		   '((rat) 3 2) '*z* r 0))
	  ((eq ($asksign p) '$pos)
	   (difap1 (difap1 (dintrad0 1 (m^ (m+t 'z** d)
					    '((rat) 3 2)))
			   '((rat) 3 2) '*z* r 0)
		   '((rat) 3 2) 'z** p 0)))))

(defun dintrad0 (n d)
  (let (l r s)
    (cond ((and (mexptp d)
		(equal (deg (cadr d)) 2.))
	   (cond ((alike1 (caddr d) '((rat) 3. 2.))
		  (cond ((and (equal n 1.)
			      (setq l (sqdtc (cadr d) t)))
			 (tbf l))
			((and (eq n var)
			      (setq l (sqdtc (cadr d) nil)))
			 (tbf (reverse l)))))
		 ((and (setq r (findp n))
		       (or (eq ($asksign (m+ -1. (m-  r) (m*t 2.
							      (caddr d))))
			       '$pos)
			   (diverg))
		       (setq s (m+ '((rat) -3. 2.) (caddr d)))
		       (eq ($asksign s) '$pos)
		       (eq (ask-integer s '$integer) '$yes))
		  (bydif r s (cadr d)))
		 ((polyinx n var nil)
		  (radbyterm d (cdr n))))))))


;;;Looks at the IMAGINARY part of a log and puts it in the interval 0 2*%pi.
(defun log-imag-0-2%pi (x)
  (let ((plog (simplify ($rectform `((%plog) ,x)))))
    ;; We take the $rectform above to make sure that the log is
    ;; expanded out for the situations where simplifying plog itself
    ;; doesn't do it.  This should probably be considered a bug in the
    ;; plog simplifier and should be fixed there.
    (cond ((not (free plog '%plog))
	   (subst '%log '%plog plog))
	  (t
	   (destructuring-let (((real . imag) (trisplit plog)))
	     (cond ((eq ($asksign imag) '$neg)
		    (setq imag (m+ imag %pi2)))
		   ((eq ($asksign (m- imag %pi2)) '$pos)
		    (setq imag (m- imag %pi2)))
		   (t t))
	     (m+ real (m* '$%i imag)))))))


;;; Temporary fix for a lacking in taylor, which loses with %i in denom.
;;; Besides doesn't seem like a bad thing to do in general.
(defun %i-out-of-denom (exp)
  (let ((denom ($denom exp)))
    (cond ((among '$%i denom)
	   ;; Multiply the denominator by it's conjugate to get rid of
	   ;; %i.
	   (let* ((den-conj (maxima-substitute (m- '$%i) '$%i denom))
		  (num ($num exp))
		  (new-denom (sratsimp (m* denom den-conj))))
	     ;; If the new denominator still contains %i, just give
	     ;; up.  Otherwise, multiply the numerator by the
	     ;; conjugate and divide by the new denominator.
	     (if (among '$%i new-denom)
		 exp
		 (setq exp (m// (m* num den-conj) new-denom)))))
	  (t exp))))

;;; LL and UL must be real otherwise this routine return $UNKNOWN.
;;; Returns $no $unknown or a list of poles in the interval (ll ul)
;;; for exp w.r.t. var.
;;; Form of list ((pole . multiplicity) (pole1 . multiplicity) ....)
(defun poles-in-interval (exp var ll ul)
  (let* ((denom (cond ((mplusp exp)
		       ($denom (sratsimp exp)))
		      ((and (mexptp exp)
			    (free (caddr exp) var)
			    (eq ($asksign (caddr exp)) '$neg))
		       (m^ (cadr exp) (m- (caddr exp))))
		      (t ($denom exp))))
	 (roots (real-roots denom var))
	 (ll-pole (limit-pole exp var ll '$plus))
	 (ul-pole (limit-pole exp var ul '$minus)))
    (cond ((or (eq roots '$failure)
	       (null ll-pole)
	       (null ul-pole))   '$unknown)
	  ((and (eq roots '$no)
		(eq ll-pole '$no)
		(eq ul-pole '$no))  '$no)
	  (t (cond ((equal roots '$no)
		    (setq roots ())))
	     (do ((dummy roots (cdr dummy))
		  (pole-list (cond ((not (eq ll-pole '$no))
				    `((,ll . 1)))
				   (t nil))))
		 ((null dummy)
		  (cond ((not (eq ul-pole '$no))
			 (sort-poles (push `(,ul . 1) pole-list)))
			((not (null pole-list))
			 (sort-poles pole-list))
			(t '$no)))
	       (let* ((soltn (caar dummy))
		      ;; (multiplicity (cdar dummy)) (not used? -- cwh)
		      (root-in-ll-ul (in-interval soltn ll ul)))
		 (cond ((eq root-in-ll-ul '$no) '$no)
		       ((eq root-in-ll-ul '$yes)
			(let ((lim-ans (is-a-pole exp soltn)))
			  (cond ((null lim-ans)
				 (return '$unknown))
				((equal lim-ans 0)
				 '$no)
				(t (push (car dummy)
					 pole-list))))))))))))


;;;Returns $YES if there is no pole and $NO if there is one.
(defun limit-pole (exp var limit direction)
  (let ((ans (cond ((member limit '($minf $inf) :test #'eq)
		    (cond ((eq (special-convergent-formp exp limit) '$yes)
			   '$no)
			  (t (get-limit (m* exp var) var limit direction))))
		   (t '$no))))
    (cond ((eq ans '$no)   '$no)
	  ((null ans)   nil)
	  ((eq ans '$und) '$no)
	  ((equal ans 0.)   '$no)
	  (t '$yes))))

;;;Takes care of forms that the ratio test fails on.
(defun special-convergent-formp (exp limit)
  (cond ((not (oscip exp))  '$no)
	((or (eq (sc-converg-form exp limit) '$yes)
	     (eq (exp-converg-form exp limit) '$yes))
	 '$yes)
	(t  '$no)))

(defun exp-converg-form (exp limit)
  (let (exparg)
    (setq exparg (%einvolve exp))
    (cond ((or (null exparg)
	       (freeof '$%i exparg))
	   '$no)
	  (t (cond
	       ((and (freeof '$%i
			     (%einvolve
			      (setq exp
				    (sratsimp (m// exp (m^t '$%e exparg))))))
		     (equal (get-limit exp var limit)  0))
		'$yes)
	       (t '$no))))))

(defun sc-converg-form (exp limit)
  (prog (scarg trigpow)
     (setq exp ($expand exp))
     (setq scarg (involve (sin-sq-cos-sq-sub exp) '(%sin %cos)))
     (cond ((null scarg) (return '$no))
	   ((and (polyinx scarg var ())
		 (eq ($asksign (m- ($hipow scarg var) 1)) '$pos)) (return '$yes))
	   ((not (freeof var (sdiff scarg var)))
	    (return '$no))
	   ((and (setq trigpow ($hipow exp `((%sin) ,scarg)))
		 (eq (ask-integer trigpow '$odd) '$yes)
		 (equal (get-limit (m// exp `((%sin) ,scarg)) var limit)
			0))
	    (return '$yes))
	   ((and (setq trigpow ($hipow exp `((%cos) ,scarg)))
		 (eq (ask-integer trigpow '$odd) '$yes)
		 (equal (get-limit (m// exp `((%cos) ,scarg)) var limit)
			0))
	    (return '$yes))
	   (t (return '$no)))))

(defun is-a-pole (exp soltn)
  (get-limit ($radcan
	      (m* (maxima-substitute (m+ 'epsilon soltn) var exp)
		  'epsilon))
	     'epsilon 0 '$plus))

(defun in-interval (place ll ul)
  ;; real values for ll and ul; place can be imaginary.
  (let ((order (ask-greateq ul ll)))
    (cond ((eq order '$yes))
	  ((eq order '$no) (let ((temp ul)) (setq ul ll ll temp)))
	  (t (merror (intl:gettext "defint: failed to order limits of integration:~%~M")
		     (list '(mlist simp) ll ul)))))
  (if (not (equal ($imagpart place) 0))
      '$no
      (let ((lesseq-ul (ask-greateq ul place))
	    (greateq-ll (ask-greateq place ll)))
	(if (and (eq lesseq-ul '$yes) (eq greateq-ll '$yes)) '$yes '$no))))

;; returns true or nil
(defun strictly-in-interval (place ll ul)
  ;; real values for ll and ul; place can be imaginary.
  (and (equal ($imagpart place) 0)
       (or (eq ul '$inf) 
	   (eq ($asksign (m+ ul (m- place))) '$pos))
       (or (eq ll '$minf) 
	   (eq ($asksign (m+ place (m- ll))) '$pos))))

(defun real-roots (exp var)
  (let (($solvetrigwarn (cond (defintdebug t) ;Rest of the code for
			      (t ())))	;TRIGS in denom needed.
	($solveradcan (cond ((or (among '$%i exp)
				 (among '$%e exp)) t)
			    (t nil)))
	*roots *failures)		;special vars for solve.
    (cond ((not (among var exp))   '$no)
	  (t (solve exp var 1)
	     (cond (*failures '$failure)
		   (t (do ((dummy *roots (cddr dummy))
			   (rootlist))
			  ((null dummy)
			   (cond ((not (null rootlist))
				  rootlist)
				 (t '$no)))
			(cond ((equal ($imagpart (caddar dummy)) 0)
			       (setq rootlist
				     (cons (cons
					    ($rectform (caddar dummy))
					    (cadr dummy))
					   rootlist)))))))))))

(defun ask-greateq (x y)
;;; Is x > y. X or Y can be $MINF or $INF, zeroA or zeroB.
  (let ((x (cond ((among 'zeroa x)
		  (subst 0 'zeroa x))
		 ((among 'zerob x)
		  (subst 0 'zerob x))
		 ((among 'epsilon x)
		  (subst 0 'epsilon x))
		 ((or (among '$inf x)
		      (among '$minf x))
		  ($limit x))
		 (t x)))
	(y (cond ((among 'zeroa y)
		  (subst 0 'zeroa y))
		 ((among 'zerob y)
		  (subst 0 'zerob y))
		 ((among 'epsilon y)
		  (subst 0 'epsilon y))
		 ((or (among '$inf y)
		      (among '$minf y))
		  ($limit y))
		 (t y))))
    (cond ((eq x '$inf)
	   '$yes)
	  ((eq x '$minf)
	   '$no)
	  ((eq y '$inf)
	   '$no)
	  ((eq y '$minf)
	   '$yes)
	  (t (let ((ans ($asksign (m+ x (m- y)))))
	       (cond ((member ans '($zero $pos) :test #'eq)
		      '$yes)
		     ((eq ans '$neg)
		      '$no)
		     (t '$unknown)))))))

(defun sort-poles (pole-list)
  (sort pole-list #'(lambda (x y)
		      (cond ((eq (ask-greateq (car x) (car y))
				 '$yes)
			     nil)
			    (t t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Integrate Definite Integrals involving log and exp functions. The algorithm
;;;  are taken from the paper "Evaluation of CLasses of Definite Integrals ..."
;;;  by K.O.Geddes et. al.
;;;
;;;  1. CASE: Integrals generated by the Gamma funtion.
;;;
;;;    inf
;;;   /
;;;   [     w    m            s        - m - 1
;;;   I    t  log (t) expt(- t ) dt = s        signum(s)
;;;   ]
;;;   /
;;;    0
;;;                                                                 !
;;;                                                    m            !
;;;                                                   d             !
;;;                                                  (--- (gamma(z))!         )
;;;                                                     m           !
;;;                                                   dz            !    w + 1
;;;                                                                 !z = -----
;;;                                                                        s
;;;
;;;  The integral converges for: 
;;;  s # 0, m = 0, 1, 2, ... and realpart((w+1)/s) > 0.
;;;  
;;;  2. CASE: Integrals generated by the Incomplete Gamma function.
;;;
;;;    inf                                                         !
;;;   /                                m                           !
;;;   [     w    m           s        d                         s  !
;;;   I    t  log (t) exp(- t ) dt = (--- (gamma_incomplete(a, x ))!         )
;;;   ]                                 m                          !
;;;   /                               da                           !    w + 1
;;;    x                                                           !z = -----
;;;                                                                       s
;;;                                                           - m - 1
;;;                                                          s        signum(s)
;;;
;;;  The integral converges for:
;;;  s # 0, m = 0, 1, 2, ... and realpart((w+1)/s) > 0.
;;;  The shown solution is valid for s>0. For s<0 gamma_incomplete has to be 
;;;  replaced by gamma(a) - gamma_incomplete(a,x^s).
;;;
;;;  3. CASE: Integrals generated by the beta function.
;;;
;;;    1
;;;   /
;;;   [     m               s  r    n
;;;   I  log (1 - t) (1 - t)  t  log (t) dt = 
;;;   ]
;;;   /
;;;    0
;;;                                                                  !
;;;                                                       !          !
;;;                                   n    m              !          !
;;;                                  d    d               !          !
;;;                                  --- (--- (beta(z, w))!         )!
;;;                                    n    m             !          !
;;;                                  dz   dw              !          !
;;;                                                       !w = s + 1 !
;;;                                                                  !z = r + 1
;;;
;;;  The integral converges for:
;;;  n, m = 0, 1, 2, ..., s > -1 and r > -1.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *debug-defint-log* nil)

;;; Recognize c*z^w*log(z)^m*exp(-t^s)

(defun m2-log-exp-1 (expr)
  (when *debug-defint-log*
    (format t "~&M2-LOG-EXP-1 with ~A~%" expr))
  (m2 expr
    '((mtimes)
        (c freevar)
        ((mexpt) (z varp) (w freevar))
        ((mexpt) $%e ((mtimes) -1 ((mexpt) (z varp) (s freevar0))))
        ((mexpt) ((%log) (z varp)) (m freevar)))))

;;; Recognize c*z^r*log(z)^n*(1-z)^s*log(1-z)^m

(defun m2-log-exp-2 (expr)
  (when *debug-defint-log*
    (format t "~&M2-LOG-EXP-2 with ~A~%" expr))
  (m2 expr
    '((mtimes)
        (c freevar)
        ((mexpt) (z varp) (r freevar))
        ((mexpt) ((%log) (z varp)) (n freevar))
        ((mexpt) ((mplus) 1 ((mtimes) -1 (z varp))) (s freevar))
        ((mexpt) ((%log) ((mplus) 1 ((mtimes)-1 (z varp)))) (m freevar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun defint-log-exp (expr var ll ul)
  (let ((x nil)
        (result nil)
        (var1 (gensym)))
    
    ;; var1 is used as a parameter for differentiation. Add var1>0 to the 
    ;; database, to get the desired simplification of the differentiation of 
    ;; the gamma_incomplete function.
    (setq *global-defint-assumptions*
          (cons (assume `((mgreaterp) ,var1 0))
                *global-defint-assumptions*))

    (cond
      ((and (eq ul '$inf)
            (setq x (m2-log-exp-1 expr)))
       ;; The integrand matches the cases 1 and 2.
       (let ((c (cdras 'c x))
             (w (cdras 'w x))
             (m (cdras 'm x))
             (s (cdras 's x))
             ($gamma_expand nil)) ; No expansion of Gamma functions.

         (when *debug-defint-log*
           (format t "~&DEFINT-LOG-EXP-1:~%")
           (format t "~&   : c = ~A~%" c)
           (format t "~&   : w = ~A~%" w)
           (format t "~&   : m = ~A~%" m)
           (format t "~&   : s = ~A~%" s))

         (cond ((and (zerop1 ll)
                     (integerp m)
                     (>= m 0)
                     (not (eq ($sign s) '$zero))
                     (eq ($sign (div (add w 1) s)) '$pos))
                ;; Case 1: Generated by the Gamma function.
                (setq result 
                     (mul c
                          (simplify (list '(%signum) s))
                          (power s (mul -1 (add m 1)))
                          ($at ($diff (list '(%gamma) var1) var1 m)
                               (list '(mequal)
                                     var1
                                     (div (add w 1) s))))))
             ((and (member ($sign ll) '($pos $pz))
                   (integerp m)
                   (or (= m 0) (= m 1))	; Exclude m>1, because Maxima can not
                                        ; derivate the involved hypergeometric
                                        ; functions.
                   (or (and (eq ($sign s) '$neg)
                            (eq ($sign (div (add 1 w) s)) '$pos))
                       (and (eq ($sign s) '$pos)
                            (eq ($sign (div (add 1 w) s)) '$pos))))
              ;; Case 2: Generated by the Incomplete Gamma function.
	      (let ((f (if (eq ($sign s) '$pos)
			   (list '(%gamma_incomplete) var1 (power ll s))
			   (sub (list '(%gamma) var1)
				(list '(%gamma_incomplete) var1 (power ll s))))))
		(setq result 
		      (mul c
			   (simplify (list '(%signum) s))
			   (power s (mul -1 (add m 1)))
			   ($at ($diff f var1 m)
				(list '(mequal) var1 (div (add 1 w) s)))))))
               (t 
                (setq result nil)))))
      ((and (zerop1 ll)
            (onep1 ul)
            (setq x (m2-log-exp-2 expr)))
       ;; Case 3: Generated by the Beta function.
       (let ((c (cdras 'c x))
             (r (cdras 'r x))
             (n (cdras 'n x))
             (s (cdras 's x))
             (m (cdras 'm x))
             (var1 (gensym))
             (var2 (gensym)))

         (when *debug-defint-log*
           (format t "~&DEFINT-LOG-EXP-2:~%")
           (format t "~&   : c = ~A~%" c)
           (format t "~&   : r = ~A~%" r)
           (format t "~&   : n = ~A~%" n)
           (format t "~&   : s = ~A~%" s)
           (format t "~&   : m = ~A~%" m))

         (cond ((and (integerp m)
                     (>= m 0)
                     (integerp n)
                     (>= n 0)
                     (eq ($sign (add 1 r)) '$pos)
                     (eq ($sign (add 1 s)) '$pos))
                (setq result 
                      (mul c
                           ($at ($diff ($at ($diff (list '($beta) var1 var2) 
                                                   var2 m)
                                            (list '(mequal) var2 (add 1 s)))
                                       var1 n)
                                (list '(mequal) var1 (add 1 r))))))
              (t 
               (setq result nil)))))
      (t 
       (setq result nil)))
    ;; Simplify result and set $gamma_expand to global value
    (let (($gamma_expand $gamma_expand)) (sratsimp result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
