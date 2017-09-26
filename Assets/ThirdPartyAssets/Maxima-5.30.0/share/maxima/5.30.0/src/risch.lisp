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

(macsyma-module risch)

(load-macsyma-macros rzmac ratmac)

(declare-top (special prob rootfac parnumer pardenom logptdx wholepart
                      $ratalgdenom expexpflag $logsimp switch1 degree cary
                      $ratfac $logexpand ratform genvar *var var rootfactor
                      expint $keepfloat trigint operator $exponentialize $gcd
                      $logarc changevp klth r s beta gamma b mainvar expflag
                      expstuff liflag intvar switch varlist nogood genvar
                      $erfflag $liflag rischp $factorflag alphar m simp
                      genpairs hypertrigint *mosesflag yyy *exp y $algebraic
                      implicit-real errrjfflag $%e_to_numlog generate-atan2
                      context bigfloatzero rp-polylogp *in-risch-p*))

(defmvar $liflag t "Controls whether `risch' generates polylogs")

(defmvar $erfflag t "Controls whether `risch' generates `erfs'")

(defvar changevp t "When nil prevents changevar hack")

(defmacro pair (al bl) `(mapcar #'cons ,al ,bl))

;; internal representation of risch expressions: list with canonical rational
;; expression (CRE) as first element, standard maxima expressions as remaining
;; elements.  risch expression is sum of CRE and remaining elements.
(defmacro rischzero () ''((0 . 1) 0))

(defun rischnoun (exp1 &optional (exp2 exp1 exp2p))
  (unless exp2p (setq exp1 (rzero)))
  `(,exp1 ((%integrate) ,(disrep exp2) ,intvar)))

(defun getrischvar ()
  (do ((vl varlist (cdr vl))
       (gl genvar (cdr gl)))
      ((null (cdr vl)) (car gl))))

;; test whether CRE p is constant with respect to variable of integration.
;; requires variables in varlist and genvar
;; to be ordered as by intsetup, with var of integration ordered before
;; any other expressions that contain it.
(defun risch-pconstp (p)
  (or (pcoefp p) (pointergp mainvar (car p))))

(defun risch-constp (r)
  (setq r (ratfix r))
  (and (risch-pconstp (car r)) (risch-pconstp (cdr r))))

;; adds two risch expressions (defined above).
(defun rischadd (x y)
  (destructuring-let (((a . b) x) ((c . d) y))
    (cons (r+ a c) (append b d))))

(defmfun $risch (exp var)
  (let ((*integrator-level* 0))
    (declare (special *integrator-level*))
    (with-new-context (context)
      (rischint exp var))))

(defun spderivative (p var)
  (cond ((pcoefp p) '(0 . 1))
	((null (cdr p)) '(0 . 1))
	((or (not (atom (car p))) (numberp (car p))) ;P IS A RATFORM
	 (let ((denprime (spderivative (cdr p) var)))
	   (cond ((rzerop denprime)
		  (ratqu (spderivative (car p) var) (cdr p)))
		 (t (ratqu (r- (r* (spderivative (car p) var)
				   (cdr p))
			       (r* (car p) denprime))
			   (r* (cdr p) (cdr p)))))))
	(t (r+ (spderivative1 (car p)
			      (cadr p)
			      (caddr p)
			      var)
	       (spderivative (cons (car p) (cdddr p))
			     var)))))

(defun spderivative1 (var1 deg coeff var)
  (cond ((eq var1 var)
	 (r* (ratexpt (cons (list var 1 1) 1) (1- deg))
	     (pctimes deg coeff)))
	((pointergp var var1) '(0 . 1))
	((equal deg 0) (spderivative coeff var))
	(t (r+ (r* (ratexpt (cons (list var1 1 1) 1) deg)
		   (spderivative coeff var))
	       (r* (cond ((equal deg 1) coeff)
			 (t (r* deg
				coeff
				(ratexpt (cons (list var1 1 1) 1)
					 (1- deg)))))
		   (get var1 'rischdiff) )))))

(defun polylogp (exp &optional sub)
  (and (mqapplyp exp) (eq (subfunname exp) '$li)
       (or (null sub) (equal sub (car (subfunsubs exp))))))

(defun rischint (exp intvar &aux ($logarc nil) ($exponentialize nil)
                     ($gcd '$algebraic) ($algebraic t) (implicit-real t)
                     ;; The risch integrator expects $logexpand T. Otherwise,
                     ;; the integrator hangs for special types of integrals
                     ;; (See bug report ID:3039452)
                     ($logexpand t))
  (prog ($%e_to_numlog $logsimp trigint operator y z var ratform liflag
	 mainvar varlist genvar hypertrigint $ratfac $ratalgdenom )
     (if (specrepp exp) (setq exp (specdisrep exp)))
     (if (specrepp intvar) (setq intvar (specdisrep intvar)))
     (if (mnump intvar)
	 (merror (intl:gettext "risch: attempt to integrate wrt a number: ~:M") intvar))
     (if (and (atom intvar) (isinop exp intvar)) (go noun))
     (rischform exp)
     (cond (trigint (return (trigin1 exp intvar)))
	   (hypertrigint (return (hypertrigint1 exp intvar t)))
	   (operator (go noun)))
     (setq y (intsetup exp intvar))
     (if operator (go noun))
     (setq ratform (car y))
     (setq varlist (caddr ratform))
     (setq mainvar (caadr (ratf intvar)))
     (setq genvar (cadddr ratform))
     (unless (some #'algpget varlist)
       (setq $algebraic nil)
       (setq $gcd (car *gcdl*)))
     (setq var (getrischvar))
     (setq z (tryrisch (cdr y) mainvar))
     (setf (caddr ratform) varlist)
     (setf (cadddr ratform) genvar)
     (return (cond ((atom (cdr z)) (disrep (car z)))
		   (t (let (($logsimp t) ($%e_to_numlog t))
			(simplify (list* '(mplus)
					 (disrep (car z))
					 (cdr z)))))))
     noun (return (list '(%integrate) exp intvar))))

(defun rischform (l)
  (cond ((or (atom l) (alike1 intvar l) (freeof intvar l)) nil)
	((polylogp l)
	 (if (and (integerp (car (subfunsubs l)))
		  (signp g (car (subfunsubs l))))
	     (rischform (car (subfunargs l)))
	     (setq operator t)))
	((atom (caar l))
	 (case (caar l)
	   ((%sin %cos %tan %cot %sec %csc)
	    (setq trigint t $exponentialize t)
	    (rischform (cadr l)))
	   ((%asin %acos %atan %acot %asec %acsc)
	    (setq trigint t $logarc t)
	    (rischform (cadr l)))
	   ((%sinh %cosh %tanh %coth %sech %csch)
	    (setq hypertrigint t $exponentialize t)
	    (rischform (cadr l)))
	   ((%asinh %acosh %atanh %acoth %asech %acsch)
	    (setq hypertrigint t $logarc t)
	    (rischform (cadr l)))
	   ((mtimes mplus mexpt rat %erf %log)
	    (mapc #'rischform (cdr l)))
	   (t (setq operator (caar l)))))
	(t (setq operator (caar l)))))

(defun hypertrigint1 (exp var hyperfunc)
  (let ((result (if hyperfunc
                    (integrator (resimplify exp) var)
                    (rischint (resimplify exp) var))))
    ;; The result can contain solveable integrals. Look for this case.
    (if (isinop result '%integrate)
        ;; Found an integral. Evaluate the result again.
        ;; Set the flag *in-risch-p* to make sure that we do not call
        ;; rischint again from the integrator. This avoids endless loops.
        (let ((*in-risch-p* t)) 
          (meval (list '($ev) result '$nouns)))
        result)))

(defun trigin1 (*exp var)
  (let ((yyy (hypertrigint1 *exp var nil)))
    (setq yyy (div ($expand ($num yyy))
		   ($expand ($denom yyy))))
    (let ((rischp var) (rp-polylogp t) $logarc $exponentialize result)
      (setq result (sratsimp (if (and (freeof '$%i *exp) (freeof '$li yyy))
                                 ($realpart yyy)
                                 ($rectform yyy))))
      ;; The result can contain solveable integrals. Look for this case.
      (if (isinop result '%integrate)
          ;; Found an integral. Evaluate the result again.
          ;; Set the flag *in-risch-p* to make sure that we do not call
          ;; rischint again from the integrator. This avoids endless loops.
          (let ((*in-risch-p* t)) 
            (meval (list '($ev) result '$nouns)))
          result))))

(defun tryrisch (exp mainvar)
  (prog (wholepart rootfactor parnumer pardenom
	 switch1 logptdx expflag expstuff expint y)
     (setq expstuff '(0 . 1))
     (cond ((eq mainvar var)
	    (return (rischfprog exp)))
	   ((eq (get var 'leadop)
		'mexpt)
	    (setq expflag t)))
     (setq y (rischlogdprog exp))
     (dolist (rat logptdx)
       (setq y (rischadd (rischlogeprog rat) y)))
     (setq y (rischadd (tryrisch1 expstuff mainvar) y))
     (return (if expint (rischadd (rischexppoly expint var) y)
		 y))))

(defun tryrisch1 (exp mainvar)
  (let* ((varlist (reverse (cdr (reverse varlist))))
	 (var (getrischvar)))
    (tryrisch exp mainvar)))

(defun rischfprog (rat)
  (let (rootfactor pardenom parnumer logptdx wholepart switch1)
    (cons (cdr (ratrep* (dprog rat)))
	  (let ((varlist varlist)
		(genvar (subseq genvar 0 (length varlist))))
	    (mapcar #'eprog logptdx)))))

(defun rischlogdprog (ratarg)
  (prog (klth arootf deriv thebpg thetop thebot prod1 prod2 ans)
     (setq ans '(0 . 1))
     (cond ((or (pcoefp (cdr ratarg))
		(pointergp var (cadr ratarg)))
	    (return (rischlogpoly ratarg))))
     (aprog (ratdenominator ratarg))
     (cprog (ratnumerator ratarg) (ratdenominator ratarg))
     (do ((rootfactor (reverse rootfactor) (cdr rootfactor))
	  (parnumer (reverse parnumer) (cdr parnumer))
	  (klth (length rootfactor) (1- klth)))
	 ((= klth 1))
       (setq arootf (car rootfactor))
       (cond
	 ((pcoefp arootf))
	 ((and (eq (get (car arootf) 'leadop) 'mexpt)
	       (null (cdddr arootf)))
	  (setq
	   expint
	   (append
	    (cond ((and (not (atom (car parnumer)))
			(not (atom (caar parnumer)))
			(eq (caaar parnumer) (car arootf)))
		   (gennegs arootf (cdaar parnumer) (cdar parnumer)))
		  (t (list
		      (list 'neg (car parnumer)
			    (car arootf) klth (cadr arootf)))))
	    expint)))
	 ((not (zerop (pdegree arootf var)))
	  (setq deriv (spderivative arootf mainvar))
	  (setq thebpg (bprog arootf (ratnumerator deriv)))
	  (setq thetop (car parnumer))
	  (do ((kx (1- klth) (1- kx))) ((= kx 0))
	    (setq prod1 (r* thetop (car thebpg)))
	    (setq prod2 (r* thetop (cdr thebpg) (ratdenominator deriv)))
	    (setq thebot (pexpt arootf kx))
	    (setq ans (r+ ans (ratqu (r- prod2) (r* kx thebot))))
	    (setq thetop
		  (r+ prod1 (ratqu (spderivative prod2 mainvar) kx)))
	    (setq thetop (cdr (ratdivide thetop thebot))))
	  (push (ratqu thetop arootf) logptdx))))
     (push (ratqu (car parnumer) (car rootfactor)) logptdx)
     (cond ((or (pzerop ans) (pzerop (car ans)))
	    (return (rischlogpoly wholepart))))
     (setq thetop (cadr (pdivide (ratnumerator ans)
				 (ratdenominator ans))))
     (return (rischadd (ncons (ratqu thetop (ratdenominator ans)))
		       (rischlogpoly wholepart)))))

(defun gennegs (denom num numdenom)
  (cond ((null num) nil)
	(t (cons (list 'neg (cadr num)
		       (car denom)
		       (- klth (car num))
		       (r* numdenom (caddr denom) ))
		 (gennegs denom (cddr num) numdenom)))))

(defun rischlogeprog (p)
  (prog (p1e p2e p2deriv logcoef ncc dcc allcc expcoef)
     (if (or (pzerop p) (pzerop (car p))) (return (rischzero)))
     (setq p1e (ratnumerator p))
     (desetq (dcc p2e) (oldcontent (ratdenominator p)))
     (cond ((and (not switch1)
		 (cdr (setq pardenom (intfactor p2e))))
	    (setq parnumer nil)
	    (setq switch1 t)
	    (desetq (ncc p1e) (oldcontent p1e))
	    (cprog p1e p2e)
	    (setq allcc (ratqu ncc dcc))
	    (return (do ((pnum parnumer (cdr pnum))
			 (pden pardenom (cdr pden))
			 (ans (rischzero)))
			((or (null pnum) (null pden))
			 (setq switch1 nil) ans)
		      (setq ans (rischadd
				 (rischlogeprog
				  (r* allcc (ratqu (car pnum) (car pden))))
				 ans))))))
     (when (and expflag (null (p-red p2e)))
       (push (cons 'neg p) expint)
       (return (rischzero)))
     (if expflag (setq expcoef (r* (p-le p2e) (ratqu (get var 'rischdiff)
						     (make-poly var)))))
     (setq p1e (ratqu p1e (ptimes dcc (p-lc p2e)))
	   p2e (ratqu p2e (p-lc p2e)))	;MAKE DENOM MONIC
     (setq p2deriv (spderivative p2e mainvar))
     (setq logcoef (ratqu p1e
			  (if expflag (r- p2deriv (r* p2e expcoef))
			      p2deriv)))
     (when (risch-constp logcoef)
       (if expflag
	   (setq expstuff (r- expstuff (r* expcoef logcoef))))
       (return
	 (list
	  '(0 . 1)
	  (list '(mtimes)
		(disrep logcoef)
		(logmabs (disrep p2e))))))
     (if (and expflag $liflag changevp)
	 (let* ((newvar (gensym))
		(new-int ($changevar
			  `((%integrate) ,(simplify (disrep p)) ,intvar)
			  (sub newvar (get var 'rischexpr))
			  newvar intvar))
		(changevp nil))		;prevents recursive changevar
	   (if (and (freeof intvar new-int)
		    (freeof '%integrate
			    (setq new-int (rischint (sdiff new-int newvar)
						    newvar))))
	       (return
		 (list (rzero)
		       (maxima-substitute (get var 'rischexpr) newvar new-int))))))
     (return (rischnoun p))))


(defun findint (exp)
  (cond ((atom exp) nil)
	((atom (car exp)) (findint (cdr exp)))
	((eq (caaar exp) '%integrate) t)
	(t (findint (cdr exp)))))

(defun logequiv (fn1 fn2)
  (freeof intvar ($ratsimp (div* (remabs (leadarg fn1))
				 (remabs (leadarg fn2))))))

(defun remabs (exp)
  (cond ((atom exp) exp)
	((eq (caar exp) 'mabs) (cadr exp))
	(t exp)))

(declare-top (special vlist lians degree))

(defun getfnsplit (l &aux coef fn)
  (mapc #'(lambda (x) (if (free x intvar) (push x coef) (push x fn))) l)
  (cons (muln coef nil) (muln fn nil)))

(defun getfncoeff (a form)
  (cond ((null a) 0)
	((equal (car a) 0) (getfncoeff (cdr a) form))
	((and (listp (car a))
	      (eq (caaar a) 'mplus) (ratpl (getfncoeff (cdar a) form)
					   (getfncoeff (cdr a) form))))
	((and (listp (car a))
	      (eq (caaar a) 'mtimes))
	 (destructuring-let (((coef . newfn) (getfnsplit (cdar a))))
           ;; (car a) is a mtimes expression. We insert coef and newfn as the
           ;; new arguments to the mtimes expression. This causes problems if
           ;;   (1) coef is a mtimes expression too and
           ;;   (2) (car a) has already a simp flag
           ;; We get a nested mtimes expression, which does not simplify.
	   ;; We comment out the following code (DK 09/2009):
           ;; (setf (cdar a) (list coef newfn))
	   
	   ;; Insert a complete mtimes expression without simpflag.
           ;; Nested mtimes expressions simplify further.
           (setf (car a) (list '(mtimes) coef newfn))
	   
	   (setf (cdar a) (list coef newfn))
	   (cond ((zerop1 coef) (getfncoeff (cdr a) form))
		 ((and (matanp newfn) (member '$%i varlist :test #'eq))
		  (let (($logarc t) ($logexpand '$all))
		    (rplaca a ($expand (resimplify (car a)))))
		  (getfncoeff a form))
		 ((and (alike1 (leadop newfn) (leadop form))
		       (or (alike1 (leadarg newfn) (leadarg form))
			   (and (mlogp newfn)
				(logequiv form newfn))))
		  (ratpl (rform coef)
			 (prog2 (rplaca a 0)
			     (getfncoeff (cdr a) form))))
		 ((do ((vl varlist (cdr vl))) ((null vl))
		    (and (not (atom (car vl)))
			 (alike1 (leadop (car vl)) (leadop newfn))
			 (if (mlogp newfn)
			     (logequiv (car vl) newfn)
			     (alike1 (car vl) newfn))
			 (rplaca (cddar a) (car vl))
			 (return nil))))
		 ((let (vlist) (newvar1 (car a)) (null vlist))
		  (setq cary
			(ratpl (cdr (ratrep* (car a)))
			       cary))
		  (rplaca a 0)
		  (getfncoeff (cdr a) form))
		 ((and liflag
		       (mlogp form)
		       (mlogp newfn))
		  (push (dilog (cons (car a) form)) lians)
		  (rplaca a 0)
		  (getfncoeff (cdr a) form))
		 ((and liflag
		       (polylogp form)
		       (mlogp newfn)
		       (logequiv form newfn))
		  (push (mul* (cadar a) (make-li (1+ (car (subfunsubs form)))
						 (leadarg form)))
			lians)
		  (rplaca a 0)
		  (getfncoeff (cdr a) form))
		 (t (setq nogood t) 0))))
	(t (rplaca a (list '(mtimes) 1 (car a)))
	   (getfncoeff a form))))


(defun rischlogpoly (exp)
  (cond ((equal exp '(0 . 1)) (rischzero))
	(expflag (push (cons 'poly exp) expint)
		 (rischzero))
	((not (among var exp)) (tryrisch1 exp mainvar))
	(t (do ((degree (pdegree (car exp) var) (1- degree))
		(p (car exp))
		(den (cdr exp))
		(lians ())
		(sum (rzero))
		(cary (rzero))
		(y) (z) (ak) (nogood) (lbkpl1))
	       ((minusp degree) (cons sum (append lians (cdr y))))
	     (setq ak (r- (ratqu (polcoef p degree) den)
			  (r* (cons (1+ degree) 1)
			      cary
			      (get var 'rischdiff))))
	     (if (not (pzerop (polcoef p degree)))
		 (setq p (if (pcoefp p) (pzero) (psimp var (p-red p)))))
	     (setq y (tryrisch1 ak mainvar))
	     (setq cary (car y))
	     (and (> degree 0) (setq liflag $liflag))
	     (setq z (getfncoeff (cdr y) (get var 'rischexpr)))
	     (setq liflag nil)
	     (cond ((and (> degree 0)
			 (or nogood (findint (cdr y))))
		    (return (rischnoun sum (r+ (r* ak
						   (make-poly var degree 1))
					       (ratqu p den))))))
	     (setq lbkpl1 (ratqu z (cons (1+ degree) 1)))
	     (setq sum (r+ (r* lbkpl1 (make-poly var (1+ degree) 1))
			   (r* cary (if (zerop degree) 1
					(make-poly var degree 1)))
			   sum))))))

(defun make-li (sub arg)
  (subfunmake '$li (ncons sub) (ncons arg)))

;;integrates log(ro)^degree*log(rn)' in terms of polylogs
;;finds constants c,d and integers j,k such that
;;c*ro^j+d=rn^k  If ro and rn are poly's then can assume either j=1 or k=1
(defun dilog (l)
  (destructuring-let* ((((nil coef nlog) . olog) l)
		       (narg (remabs (cadr nlog)))
		       (varlist varlist)
		       (genvar genvar)
		       (rn (rform narg))	;; can add new vars to varlist
		       (ro (rform (cadr olog)))
		       (var (caar ro))
		       ((j . k) (ratreduce (pdegree (car rn) var) (pdegree (car ro) var)))
		       (idx (gensym))
		       (rc) (rd))
    (cond ((and (= j 1) (> k 1))
	   (setq rn (ratexpt rn k)
		 coef (div coef k)
		 narg (rdis rn)))
	  ((and (= k 1) (> j 1))
	   (setq ro (ratexpt ro j)
		 coef (div coef (f* j degree))
		 olog (mul j olog))))
    (desetq (rc . rd) (ratdivide rn ro))
    (cond ((and (freeof intvar (rdis rc))	;; can't use risch-constp because varlist
		(freeof intvar (rdis rd)))	;; is not set up with vars in correct order.
	   (setq narg ($ratsimp (sub 1 (div narg (rdis rd)))))
	   (mul* coef (power -1 (1+ degree))
		 `((mfactorial) ,degree)
		 (dosum (mul* (power -1 idx)
			      (div* (power olog idx)
				    `((mfactorial) ,idx))
			      (make-li (add degree (neg idx) 1) narg))
			idx 0 degree t)))
	  (t (setq nogood t) 0))))

(defun exppolycontrol (flag f a expg n)
  (let (y l var (varlist varlist) (genvar genvar))
    (setq varlist (reverse (cdr (reverse varlist))))
    (setq var (getrischvar))
    (setq y (get var 'leadop))
    (cond ((and (not (pzerop (ratnumerator f)))
		(risch-constp (setq l (ratqu a f))))
	   (cond (flag		;; multiply in expg^n - n may be negative
		  (list (r* l (ratexpt (cons (list expg 1 1) 1) n))
			0))
		 (t l)))
	  ((eq y intvar)
	   (rischexpvar nil flag (list f a expg n)))
	  (t (rischexplog (eq y 'mexpt) flag f a
			  (list expg n (get var 'rischarg)
				var (get var 'rischdiff)))))))

(defun rischexppoly (expint var)
  (let (y w num denom type (ans (rischzero))
	  (expdiff (ratqu (get var 'rischdiff) (list var 1 1))))
    (do ((expint expint (cdr expint)))
	((null expint) ans)
      (desetq (type . y) (car expint))
      (desetq (num . denom) (ratfix y))
      (cond ((eq type 'neg)
	     (setq w (exppolycontrol t
				     (r* (- (cadr denom))
					 expdiff)
				     (ratqu num (caddr denom))
				     var
				     (- (cadr denom)))))
	    ((or (numberp num) (not (eq (car num) var)))
	     (setq w (tryrisch1 y mainvar)))
	    (t (setq w (rischzero))
	       (do ((num (cdr num) (cddr num))) ((null num))
		 (cond ((equal (car num) 0)
			(setq w (rischadd
				 (tryrisch1 (ratqu (cadr num) denom) mainvar)
				 w)))
		       (t (setq w (rischadd (exppolycontrol
					     t
					     (r* (car num) expdiff)
					     (ratqu (cadr num) denom)
					     var
					     (car num))
					    w)))))))
      (setq ans (rischadd w ans)))))

(defun rischexpvar (expexpflag flag l)
  (prog (lcm y m p alphar beta gamma delta r s
	 tt denom k wl wv i ytemp ttemp yalpha f a expg n yn yd)
     (desetq (f a expg n) l)
     (cond ((or (pzerop a) (pzerop (car a)))
	    (return (cond ((null flag) (rzero))
			  (t (rischzero))))))
     (setq denom (ratdenominator f))
     (setq p (findpr (cdr (partfrac a mainvar))
		     (cdr (partfrac f mainvar))))
     (setq lcm (plcm (ratdenominator a) p))
     (setq y (ratpl (spderivative (cons 1 p) mainvar)
		    (ratqu f p)))
     (setq lcm (plcm lcm (ratdenominator y)))
     (setq r (car (ratqu lcm p)))
     (setq s (car (r* lcm y)))
     (setq tt (car (r* a lcm)))
     (setq beta (pdegree r mainvar))
     (setq gamma (pdegree s mainvar))
     (setq delta (pdegree tt mainvar))
     (setq alphar (max (- (1+ delta) beta)
		       (- delta gamma)))
     (setq m 0)
     (cond ((equal (1- beta) gamma)
	    (setq y (r* -1
			(ratqu (polcoef s gamma)
			       (polcoef r beta))))
	    (and (equal (cdr y) 1)
		 (numberp (car y))
		 (setq m (car y)))))
     (setq alphar (max alphar m))
     (if (minusp alphar)
	 (return (if flag (cxerfarg (rzero) expg n a) nil)))
     (cond ((not (and (equal alphar m) (not (zerop m))))
	    (go down2)))
     (setq k (+ alphar beta -2))
     (setq wl nil)
     l2   (setq wv (list (cons (polcoef tt k) 1)))
     (setq i alphar)
     l1   (setq wv
		(cons (r+ (r* (cons i 1)
			      (polcoef r (+ k 1 (- i))))
			  (cons (polcoef s (+ k (- i))) 1))
		      wv))
     (decf i)
     (cond ((> i -1) (go l1)))
     (setq wl (cons wv wl))
     (decf k)
     (cond ((> k -1) (go l2)))
     (setq y (lsa wl))
     (if (or (eq y 'singular) (eq y 'inconsistent))
	 (cond ((null flag) (return nil))
	       (t (return (cxerfarg (rzero) expg n a)))))
     (setq k 0)
     (setq lcm 0)
     (setq y (cdr y))
     l3   (setq lcm
		(r+ (r* (car y) (pexpt (list mainvar 1 1) k))
		    lcm))
     (incf k)
     (setq y (cdr y))
     (cond ((null y)
	    (return (cond ((null flag) (ratqu lcm p))
			  (t (list (r* (ratqu lcm p)
				       (cons (list expg n 1) 1))
				   0))))))
     (go l3)
     down2 (cond ((> (1- beta) gamma)
		 (setq k (+ alphar (1- beta)))
		 (setq denom '(ratti alphar (polcoef r beta) t)))
		((< (1- beta) gamma)
		 (setq k (+ alphar gamma))
		 (setq denom '(polcoef s gamma)))
		(t (setq k (+ alphar gamma))
		   (setq denom
			 '(ratpl (ratti alphar (polcoef r beta) t)
			   (polcoef s gamma)))))
     (setq y 0)
     loop (setq yn (polcoef (ratnumerator tt) k)
		yd (r* (ratdenominator tt) ;DENOM MAY BE 0
		       (cond ((zerop alphar) (polcoef s gamma))
			     (t (eval denom))) ))
     (cond ((rzerop yd)
	    (cond ((pzerop yn) (setq k (1- k) alphar (1- alphar))
		   (go loop))		;need more constraints?
		  (t (cond
		       ((null flag) (return nil))
		       (t (return (cxerfarg (rzero) expg n a)))))))
	   (t (setq yalpha (ratqu yn yd))))
     (setq ytemp (r+ y (r* yalpha
			   (cons (list mainvar alphar 1) 1) )))
     (setq ttemp (r- tt (r* yalpha
			    (r+ (r* s (cons (list mainvar alphar 1) 1))
				(r* r alphar
				    (list mainvar (1- alphar) 1))))))
     (decf k)
     (decf alphar)
     (cond ((< alphar 0)
	    (cond
	      ((rzerop ttemp)
	       (cond
		 ((null flag) (return (ratqu ytemp p)))
		 (t (return (list (ratqu (r* ytemp (cons (list expg n 1) 1))
					 p)
				  0)))))
	      ((null flag) (return nil))
	      ((and (risch-constp (setq ttemp (ratqu ttemp lcm)))
		    $erfflag
		    (equal (pdegree (car (get expg 'rischarg)) mainvar) 2)
		    (equal (pdegree (cdr (get expg 'rischarg)) mainvar) 0))
	       (return (list (ratqu (r* ytemp (cons (list expg n 1) 1)) p)
			     (erfarg2 (r* n (get expg 'rischarg)) ttemp))))
	      (t (return
		   (cxerfarg
		    (ratqu (r* y (cons (list expg n 1) 1)) p)
		    expg
		    n
		    (ratqu tt lcm)))))))
     (setq y ytemp)
     (setq tt ttemp)
     (go loop)))


;; *JM should be declared as an array, although it is not created
;; by this file. -- cwh

(defun lsa (mm)
  (prog (d *mosesflag m m2)
     (setq d (length (car mm)))
     ;; MTOA stands for MATRIX-TO-ARRAY.  An array is created and
     ;; associated functionally with the symbol *JM.  The elements
     ;; of the array are initialized from the matrix MM.
     (mtoa '*jm* (length mm) d mm)
     (setq m (tfgeli '*jm*  (length mm) d))
     (cond ((or (and (null (car m)) (null (cadr m)))
		(and (car m)
		     (> (length (car m)) (- (length mm) (1- d)))))
	    (return 'singular))
	   ((cadr m) (return 'inconsistent)))
     (setq *mosesflag t)
     (ptorat '*jm* (1- d) d)
     (setq m2 (xrutout '*jm* (1- d) d nil nil))
     (setq m2 (lsafix (cdr m2) (caddr m)))
     (return m2)))

(defun lsafix (l n)
  (declare (special *jm*))
  (do ((n n (cdr n))
       (l l (cdr l)))
      ((null l))
    (setf (aref *jm* 1 (car n)) (car l)))
  (do ((s (length l) (1- s))
       (ans))
      ((= s 0) (cons '(list) ans))
    (setq ans (cons (aref *jm* 1 s) ans))))


(defun findpr (alist flist &aux (p 1) alphar fterm)
  (do ((alist alist (cdr alist))) ((null alist))
    (setq fterm (findflist (cadar alist) flist))
    (if fterm (setq flist (remove y flist :count 1 :test #'eq)))
    (setq alphar
	  (cond ((null fterm) (caddar alist))
		((equal (caddr fterm) 1)
		 (fpr-dif (car flist) (caddar alist)))
		(t (max (- (caddar alist) (caddr fterm)) 0))))
    (if (not (zerop alphar))
	(setq p (ptimes p (pexpt (cadar alist) alphar)))))
  (do ((flist flist (cdr flist)))
      ((null flist))
    (when (equal (caddar flist) 1)
      (setq alphar (fpr-dif (car flist) 0))
      (setq p (ptimes p (pexpt (cadar flist) alphar)))))
  p)

(defun fpr-dif (fterm alpha)
  (destructuring-let* (((num den mult) fterm)
		       (m (spderivative den mainvar))
		       (n))
    (cond ((rzerop m) alpha)
	  (t (setq n (ratqu (cdr (ratdivide num den))
			    m))
	     (if (and (equal (cdr n) 1) (numberp (car n)))
		 (max (car n) alpha)
		 alpha)))))

(defun findflist (a llist)
  (cond ((null llist) nil)
	((equal (cadar llist) a) (car llist))
	(t (findflist a (cdr llist)))))


(defun rischexplog (expexpflag flag f a l)
  (declare (special var))
  (prog (lcm y yy m p alphar beta gamma delta
	 mu r s tt denom ymu rbeta expg n eta logeta logdiff
	 temp cary nogood vector aarray rmu rrmu rarray)
     (desetq (expg n eta logeta logdiff) l)
     (cond ((or (pzerop a) (pzerop (car a)))
	    (return (cond ((null flag) (rzero))
			  (t (rischzero))))))
     (setq p (findpr (cdr (partfrac a var)) (cdr (partfrac f var))))
     (setq lcm (plcm (ratdenominator a) p))
     (setq y (ratpl (spderivative (cons 1 p) mainvar)
		    (ratqu f p)))
     (setq lcm (plcm lcm (ratdenominator y)))
     (setq r (car (ratqu lcm p)))
     (setq s (car (r* lcm y)))
     (setq tt (car (r* a lcm)))
     (setq beta (pdegree r var))
     (setq gamma (pdegree s var))
     (setq delta (pdegree tt var))
     (cond (expexpflag (setq mu (max (- delta beta)
				     (- delta gamma)))
		       (go expcase)))
     (setq mu (max (- (1+ delta) beta)
		   (- (1+ delta) gamma)))
     (cond ((< beta gamma) (go back))
	   ((= (1- beta) gamma) (go down1)))
     (setq y (tryrisch1 (ratqu (r- (r* (polcoef r (1- beta))
				       (polcoef s gamma))
				   (r* (polcoef r beta)
				       (polcoef s (1- gamma))))
			       (r* (polcoef r beta)
				   (polcoef r beta) ))
			mainvar))
     (setq cary (car y))
     (setq yy (getfncoeff (cdr y) (get var 'rischexpr)))
     (cond ((and (not (findint (cdr y)))
		 (not nogood)
		 (not (atom yy))
		 (equal (cdr yy) 1)
		 (numberp (car yy))
		 (> (car yy) mu))
	    (setq mu (car yy))))
     (go back)
     expcase
     (cond ((not (equal beta gamma)) (go back)))
     (setq y (tryrisch1 (ratqu (polcoef s gamma) (polcoef r beta))
			mainvar))
     (cond ((findint (cdr y)) (go back)))
     (setq yy (ratqu (r* -1 (car y)) eta))
     (cond ((and (equal (cdr yy) 1)
		 (numberp (car yy))
		 (> (car yy) mu))
	    (setq mu (car yy))))
     (go back)
     down1(setq y (tryrisch1 (ratqu (polcoef s gamma) (polcoef r beta))
			     mainvar))
     (setq cary (car y))
     (setq yy (getfncoeff (cdr y) (get var 'rischexpr)))
     (cond ((and (not (findint (cdr y)))
		 (not nogood)
		 (equal (cdr yy) 1)
		 (numberp (car yy))
		 (> (- (car yy)) mu))
	    (setq mu (- (car yy)))))
     back (if (minusp mu)
	      (return (if flag (cxerfarg (rzero) expg n a) nil)))
     (cond ((> beta gamma)(go lsacall))
	   ((= beta gamma)
	    (go recurse)))
     (setq denom (polcoef s gamma))
     (setq y '(0 . 1))
     linearloop
     (setq ymu (ratqu (polcoef (ratnumerator tt) (+ mu gamma))
		      (r* (ratdenominator tt) denom)))
     (setq y (r+ y (setq ymu (r* ymu (pexpt (list logeta 1 1) mu) ))))
     (setq tt (r- tt
		  (r* s ymu)
		  (r* r (spderivative ymu mainvar))))
     (decf mu)
     (cond  ((not (< mu 0)) (go linearloop))
	    ((not flag) (return (if (rzerop tt) (ratqu y p) nil)))
	    ((rzerop tt)
	     (return (cons (ratqu (r* y (cons (list expg n 1) 1)) p) '(0))))
	    (t (return (cxerfarg (ratqu (r* y (cons (list expg n 1) 1)) p)
				 expg
				 n
				 (ratqu tt lcm)))))
     recurse
     (setq rbeta (polcoef r beta))
     (setq y '(0 . 1))
     recurseloop
     (setq f (r+ (ratqu (polcoef s gamma) rbeta)
		 (if expexpflag
		     (r* mu (spderivative eta mainvar))
		     0)))
     (setq ymu (exppolycontrol nil
			       f
			       (ratqu (polcoef (ratnumerator tt)
					       (+ beta mu))
				      (r* (ratdenominator tt) rbeta))
			       expg n))
     (when (null ymu)
       (return (cond ((null flag) nil)
		     (t (return (cxerfarg (ratqu (r* y (cons (list expg n 1) 1)) p)
					  expg n (ratqu tt lcm)))))))
     (setq y (r+ y (setq ymu (r* ymu (pexpt (list logeta 1 1) mu)))))
     (setq tt (r- tt
		  (r* s ymu)
		  (r* r (spderivative ymu mainvar))))
     (decf mu)
     (cond
       ((not (< mu 0)) (go recurseloop))
       ((not flag)
	(return (cond ((rzerop tt) (ratqu y p)) (t nil))))
       ((rzerop tt)
	(return (cons (ratqu (r* y (cons (list expg n 1) 1)) p) '(0))))
       (t (return (cxerfarg (ratqu (r* y (cons (list expg n 1) 1)) p)
			    expg
			    n
			    (ratqu tt lcm)))))
     lsacall
     (setq rrmu mu)
     muloop
     (setq temp (r* (ratexpt (cons (list logeta 1 1) 1) (1- mu))
		    (r+ (r* s (cons (list logeta 1 1) 1))
			(r* mu r logdiff ))))
     mu1  (setq vector nil)
     (setq rmu (+ rrmu beta))
     rmuloop
     (setq vector (cons (ratqu (polcoef (ratnumerator temp) rmu)
			       (ratdenominator temp)) vector))
     (decf rmu)
     (unless (< rmu 0) (go rmuloop))
     (decf mu)
     (setq aarray (append aarray (list (reverse vector))))
     (cond ((not (< mu 0)) (go muloop))
	   ((equal mu -2) (go skipmu)))
     (setq temp tt)
     (go mu1)
     skipmu
     (setq rarray nil)
     arrayloop
     (setq vector nil)
     (setq vector (mapcar 'car aarray))
     (setq aarray (mapcar 'cdr aarray))
     (setq rarray (append rarray (list vector)))
     (unless (null (car aarray)) (go arrayloop))
     (setq rmu (1+ rrmu))
     (setq vector nil)
     array1loop
     (setq vector (cons '(0 . 1) vector))
     (decf rmu)
     (unless (< rmu 0) (go array1loop))
     (setq aarray nil)
     array2loop
     (cond ((equal (car rarray) vector) nil)
	   (t (setq aarray (cons (car rarray) aarray))))
     (setq rarray (cdr rarray))
     (when rarray (go array2loop))
     (setq rarray (reverse aarray))
     (setq temp (lsa rarray))
     (when (or (eq temp 'singular) (eq temp 'inconsistent))
       (return (if (null flag) nil (cxerfarg (rzero) expg n a))))
     (setq temp (reverse  (cdr temp)))
     (setq rmu 0)
     (setq y 0)
     l3   (setq y (r+ y (r* (car temp) (pexpt (list logeta 1 1) rmu))))
     (setq temp (cdr temp))
     (incf rmu)
     (unless (> rmu rrmu) (go l3))
     (return (if (null flag)
		 (ratqu y p)
		 (cons (r* (list expg n 1) (ratqu y p)) '(0))))))


(defun erfarg (exparg coef)
  (prog (num denom erfarg)
     (setq exparg (r- exparg))
     (unless (and (setq num (pnthrootp (ratnumerator exparg) 2))
		  (setq denom (pnthrootp (ratdenominator exparg) 2)))
       (return nil))
     (setq erfarg (cons num denom))
     (if (risch-constp
	  (setq coef (ratqu coef (spderivative erfarg mainvar))))
	 (return (simplify `((mtimes) ((rat) 1 2)
			     ((mexpt) $%pi ((rat) 1 2))
			     ,(disrep coef)
			     ((%erf) ,(disrep erfarg))))))))

(defun erfarg2 (exparg coeff &aux (var mainvar) a b c d)
  (when (and (= (pdegree (car exparg) var) 2)
	     (eq (caar exparg) var)
	     (risch-pconstp (cdr exparg))
	     (risch-constp coeff))
    (setq a (ratqu (r* -1 (caddar exparg))
		   (cdr exparg)))
    (setq b (disrep (ratqu (r* -1 (polcoef (car exparg) 1))
			   (cdr exparg))))
    (setq c (disrep (ratqu (r* (polcoef (car exparg) 0))
			   (cdr exparg))))
    (setq d (ratsqrt a))
    (setq a (disrep a))
    (simplify `((mtimes)
		((mtimes)
		 ((mexpt) $%e ((mplus) ,c
			       ((mquotient) ((mexpt) ,b 2)
				((mtimes) 4 ,a))))
		 ((rat) 1 2)
		 ,(disrep coeff)
		 ((mexpt) ,d -1)
		 ((mexpt) $%pi ((rat) 1 2)))
		((%erf) ((mplus)
			 ((mtimes) ,d ,intvar)
			 ((mtimes) ,b ((rat) 1 2) ((mexpt) ,d -1))))))))


(defun cxerfarg (ans expg n numdenom &aux (arg (r* n (get expg 'rischarg)))
		 (fails 0))
  (prog (denom erfans num nerf)
     (desetq (num . denom) numdenom)
     (unless $erfflag (setq fails num) (go lose))
     (if (setq erfans (erfarg arg numdenom))
	 (return (list ans erfans)))
     again	(when (and (not (pcoefp denom))
			   (null (p-red denom))
			   (eq (get (car denom) 'leadop) 'mexpt))
		  (setq arg (r+ arg (r* (- (p-le denom))
					(get (p-var denom) 'rischarg)))
			denom (p-lc denom))
		  (go again))
     (loop for (coef exparg exppoly) in (explist num arg 1)
	    do (setq coef (ratqu coef denom)
		     nerf (or (erfarg2 exparg coef) (erfarg exparg coef)))
	    (if nerf (push nerf erfans) (setq fails
					      (pplus fails exppoly))))
     lose (return
	    (if (pzerop fails) (cons ans erfans)
		(rischadd (cons ans erfans)
			  (rischnoun (r* (ratexpt (cons (make-poly expg) 1) n)
					 (ratqu fails (cdr numdenom)))))))))

(defun explist (p oarg exps)
  (cond ((or (pcoefp p) (not (eq 'mexpt (get (p-var p) 'leadop))))
	 (list (list p oarg (ptimes p exps))))
	(t (loop with narg = (get (p-var p) 'rischarg)
		  for (exp coef) on (p-terms p) by #'cddr
		  nconc (explist coef
				 (r+ oarg (r* exp narg))
				 (ptimes exps
					 (make-poly (p-var p) exp 1)))))))


(declare-top (special *fnewvarsw))

(defun intsetup (exp *var)
  (prog (varlist clist $factorflag dlist genpairs old y z $ratfac $keepfloat
	 *fnewvarsw)
   y    (setq exp (radcan1 exp))
   (fnewvar exp)
   (setq *fnewvarsw t)
   a    (setq clist nil)
   (setq dlist nil)
   (setq z varlist)
   up   (setq y (pop z))
   (cond ((freeof *var y) (push y clist))
	 ((eq y *var) nil)
	 ((and (mexptp y)
	       (not (eq (cadr y) '$%e)))
	  (cond ((not (freeof *var (caddr y)))
		 (setq dlist `((mexpt simp)
			       $%e
			       ,(mul2* (caddr y)
				       `((%log) ,(cadr y)))))
		 (setq exp (maxima-substitute dlist y exp))
		 (setq varlist nil)  (go y))
		((atom (caddr y))
		 (cond ((numberp (caddr y)) (push y dlist))
		       (t (setq operator t)(return nil))))
		(t (push y dlist))))
	 (t (push y dlist)))
   (if z (go up))
   (if (member '$%i clist :test #'eq) (setq clist (cons '$%i (delete '$%i clist :test #'equal))))
   (setq varlist (append clist
			 (cons *var
			       (nreverse (sort (append dlist nil) #'intgreat)))))
   (orderpointer varlist)
   (setq old varlist)
   (mapc #'intset1 (cons *var dlist))
   (cond ((alike old varlist) (return (ratrep* exp)))
	 (t (go a)))))

(defun leadop (exp)
  (cond ((atom exp) exp)
	((mqapplyp exp) (cadr exp))
	(t (caar exp))))

(defun leadarg (exp)
  (cond ((atom exp) 0)
	((and (mexptp exp) (eq (cadr exp) '$%e)) (caddr exp))
	((mqapplyp exp) (car (subfunargs exp)))
	(t (cadr exp))))

(defun intset1 (b)
  (let (e c d)
    (fnewvar
     (setq d (if (mexptp b)		;needed for radicals
		 `((mtimes simp)
		   ,b
		   ,(radcan1 (sdiff (simplify (caddr b)) *var)))
		 (radcan1 (sdiff (simplify b) *var)))))
    (setq d (ratrep* d))
    (setq c (ratrep* (leadarg b)))
    (setq e (cdr (assoc b (pair varlist genvar) :test #'equal)))
    (putprop e (leadop b) 'leadop)
    (putprop e b 'rischexpr)
    (putprop e (cdr d) 'rischdiff)
    (putprop e (cdr c) 'rischarg)))

;; order of expressions for risch.
;; expressions containing erf and li last.
;; then order by size of expression to guarantee that
;; any subexpressions are considered smaller.
;; this relation should be transitive, since it is called by sort. 
(defun intgreat (a b)
  (cond ((and (not (atom a)) (not (atom b)))
	 (cond ((and (not (freeof '%erf a)) (freeof '%erf b)) t)
	       ((and (not (freeof '$li a)) (freeof '$li b)) t)
	       ((and (freeof '$li a) (not (freeof '$li b))) nil)
	       ((and (freeof '%erf a) (not (freeof '%erf b))) nil)
	       ((> (conssize a) (conssize b)) t)
	       ((< (conssize a) (conssize b)) nil)
	       (t (great (resimplify (fixintgreat a))
			 (resimplify (fixintgreat b))))))
	(t (great (resimplify (fixintgreat a))
		  (resimplify (fixintgreat b))))))

(defun fixintgreat (a)
  (subst '/_101x *var a))

(declare-top (unspecial b beta cary context *exp degree gamma
			klth liflag m nogood operator prob
			r s simp switch switch1 *var var  y yyy))
