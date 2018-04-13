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

(macsyma-module nrat4)

(declare-top (special $ratsimpexpons *exp *exp2 *radsubst *loglist $radsubstflag
		      $radexpand $logsimp *v *var fr-factor radcanp ratsubvl))

(load-macsyma-macros rzmac ratmac)

(defmvar $radsubstflag nil
  "`radsubstflag' `t' makes `ratsubs' call `radcan' when it appears useful")


(defmfun pdis (x) ($ratdisrep (pdis* x)))

(defun pdis* (x) `((mrat simp ,varlist ,genvar) ,x . 1))

(defun rdis (x) ($ratdisrep (rdis* x)))

(defun rdis* (x) `((mrat simp ,varlist ,genvar) . ,x))

(defun rform (x) (cdr (ratf x)))

(setq radcanp nil)

(defmfun $ratcoef (e x &optional (n 1))
  (ratcoeff e x n)) ; The spelling "ratcoeff" is nicer.

(defmfun ratcoeff (a b c)
  (let* ((formflag ($ratp a))
	 (taylorform (and formflag (member 'trunc (cdar a) :test #'eq))))
    (cond ((zerop1 b) (improper-arg-err b '$ratcoeff))
	  ((mbagp a) (cons (car a)
			   (mapcar #'(lambda (a) (ratcoeff a b c))
				   (cdr a))))
	  ((and taylorform (mnump c) (assolike b (cadddr (cdar a))))
	   (pscoeff1 a b c))
	  ((and taylorform (mexptp b) (mnump c) (mnump (caddr b))
		(assolike (cadr b) (cadddr (cdar a))))
	   (pscoeff1 a (cadr b) (mul2 c (caddr b))))
	  ((and taylorform (equal c 0)) a)
	  (t (if taylorform (setq a (ratdisrep a)))
	     (setq a (let ($ratwtlvl)
		       (if (equal c 0)
			   (ratcoef (mul2* a b) b)
			   (ratcoef a (if (equal c 1) b (list '(mexpt) b c))))))
	     (if (and formflag (not taylorform))
		 (minimize-varlist a)
		 (ratdisrep a))))))

(defun minimize-varlist (ratfun)
  (if (not ($ratp ratfun)) (setq ratfun (ratf ratfun)))
  (minvarlist-mrat (caddr (car ratfun)) (cadddr (car ratfun))
		   (cdr ratfun)))

(defun minvarlist-mrat (vars gens ratform)
  (let ((newgens (union* (listovars (car ratform))
			 (listovars (cdr ratform)))))
    (do ((lv vars (cdr lv))
	 (lg gens (cdr lg))
	 (nlv ())
	 (nlg ()))
	((null lg)
	 (cons (list 'mrat 'simp (nreverse nlv) (nreverse nlg))
	       ratform))
      (cond ((member (car lg) newgens :test #'eq)
	     (push (car lg) nlg)
	     (push (car lv) nlv))))))

(defun ratcoef (exp var)
  (prog (varlist genvar $ratfac $algebraic $ratwtlvl bas minvar)
     (setq var (ratdisrep var))
     (setq bas (if (and (mexptp var) (mnump (caddr var))) (cadr var) var))
     (newvar var)
     (newvar bas)
     (setq minvar (car varlist))
     (newvar exp)
     (setq exp (cdr (ratrep* exp)))
     (setq var (cdr (ratrep* var)))
     (setq bas (cadr (ratrep* bas)))
     (if (and (onep1 (cdr exp)) (onep1 (cdr var)) (pureprod (car var)))
	 (return (pdis* (prodcoef (car var) (car exp)))))
     (setq exp (ratquotient exp var))
     (if (null minvar) (return (pdis* (prodcoef (cdr exp) (car exp)))))
     (setq minvar (caadr (ratrep* minvar)))
     loop	(if (or (pcoefp (cdr exp)) (pointergp minvar (cadr exp)))
		    (return (rdis* (cdr (ratdivide exp bas)))))
     (setq exp (ratcoef1 (car exp) (cdr exp)))
     (go loop)))

(defun ratcoef1 (num den)
  (cond ((pcoefp num) (rzero))
	((eq (car num) (car den)) (car (pdivide num den)))
	((pointergp (car den) (car num)) (rzero))
	(t (ratcoef1 (constcoef (cdr num)) den))))

(defun constcoef (p)
  (cond ((null p) 0)
	((zerop (car p)) (cadr p))
	(t (constcoef (cddr p)))))

(setq *radsubst nil ratsubvl t)         ; SUBST ON VARLIST

(defmfun $ratsubst (a b c)              ; NEEDS CODE FOR FAC. FORM
  (prog (varlist newvarlist dontdisrepit $ratfac genvar $keepfloat)
     ;; hard to maintain user ordering info.
     (if ($ratp c) (setq dontdisrepit t))
     (when (and $radsubstflag
		(prog2 (newvar b) (some #'mexptp varlist)))
       (let (($factorflag t) *exp *exp2 *radsubst)
	 (setq b (fullratsimp b))
	 (setq c (fullratsimp c))
	 (setq varlist nil)
	 (fnewvar b)
	 (fnewvar c)
	 (setq *exp (cdr (ratrep* b)))
	 (setq *exp2 (cdr (ratrep* c)))
	 ;; since *radsubst is t, both *exp and *exp2 will be radcan simplified
	 (setq *radsubst t)
	 (spc0)
	 (setq b (rdis *exp) c (rdis *exp2))
	 (setq varlist nil)))
     (setq a ($ratdisrep a) b ($ratdisrep b) c ($ratdisrep c))
     (cond ((integerp b) (setq c (ratf (maxima-substitute a b c)))
	    (return (cond (dontdisrepit c) (t ($ratdisrep c))))))
     (newvar c)
     (setq
      newvarlist
      (if ratsubvl
	  (mapcar
	   #'(lambda (z)
	       (cond ((atom z) z)
		     (t (resimplify
			 (cons (car z)
			       (mapcar #'(lambda (zz)
					   (cond ((alike1 zz b) a)
						 ((atom zz) zz)
						 (t ($ratdisrep
						     ($ratsubst a b zz)))))
				       (cdr z)))))))
	   varlist)
	  varlist))
     (newvar a) (newvar b)
     (setq newvarlist (reverse (pairoff (reverse varlist)
					(reverse newvarlist))))
     (setq a (cdr (ratrep* a)))
     (setq b (cdr (ratrep* b)))
     (setq c (cdr (ratrep* c)))
     (when (pminusp (car b))
       (setq b (ratminus b))
       (setq a (ratminus a)))
     (when (and (eqn 1 (car b)) (not (eqn 1 (cdr b)))(not (eqn (car a) 0)))
       (setq a (ratinvert a))
       (setq b (ratinvert b)))
     (cond ((not (eqn 1 (cdr b)))
	    (setq a (rattimes a (cons (cdr b) 1) t))
	    (setq b (cons (car b) 1))))
     (setq c
	   (cond ((member (car b) '(0 1) :test #'equal)
		  (ratf (maxima-substitute (rdis a) b (rdis c))))
		 (t (cons (list 'mrat 'simp varlist genvar)
			  (if (eqn (cdr a) 1)
			      (ratreduce (everysubst0 (car a) (car b) (car c))
					 (everysubst0 (car a) (car b) (cdr c)))
			      (allsubst00 a b c))))))
     (unless (alike newvarlist varlist)
       (setq varlist newvarlist
	     c (rdis (cdr c))
	     varlist nil
	     c (ratf c)))
     (return (cond (dontdisrepit c) (t ($ratdisrep c))))))

(defun xptimes (x y) (if $ratwtlvl (wtptimes x y 0) (ptimes x y)))

(defun allsubst00 (a b c)
  (cond ((equal a b) c)
	(t (ratquotient (everysubst00 a (car b) (car c))
			(everysubst00 a (car b) (cdr c))))))

(defun everysubst00 (x i z)
  (loop with ans = (rzero)
	 for (exp coef) on (everysubst i z *alpha) by #'cddr
	 do (setq ans (ratplus ans (rattimes (cons coef 1) (ratexpt x exp) t)))
	 finally (return ans)))

(defun everysubst0 (x i z)
  (loop with ans = (pzero)
	 for (exp coef) on (everysubst i z *alpha) by #'cddr
	 do (setq ans (pplus ans (xptimes coef (pexpt x exp))))
	 finally (return ans)))

(defun everysubst1 (a b maxpow)
  (loop for (exp coef) on (p-terms b) by #'cddr
	 for part = (everysubst a coef maxpow)
	 nconc (if (= 0 exp) part
		   (everysubst2 part (make-poly (p-var b) exp 1)))))

(defun everysubst2 (l h)
  (do ((ptr l (cddr ptr)))
      ((null ptr) l)
    (setf (cadr ptr) (ptimes h (cadr ptr)))))


(defun pairoff (l m)
  (cond ((null m) l) (t (cons (car m) (pairoff (cdr l) (cdr m))))))

;;(DEFUN PAIROFF (L M)
;;  ;(COND ((NULL M) L) (T (CONS (CAR M) (PAIROFF (CDR L) (CDR M)))))
;;  (let ((ans nil))
;;    (dolist (x m (nreconc ans l))
;;      (push x ans) (setq l (cdr l)))))

(defun everysubst (a b maxpow)
  (cond ((pcoefp a)
	 (cond ((eqn a 1) (list maxpow b))
	       ((pcoefp b)
		(list (setq maxpow
			    (do ((b b (quotient b a))
				 (ans 0 (1+ ans)))
				((or (> (abs a) (abs b))
				     (eqn maxpow ans))
				 ans)))
		      (quotient b (setq maxpow (expt a maxpow)))
		      0
		      (rem b maxpow)))
	       (t (everysubst1 a b maxpow))))
	((or (pcoefp b) (pointergp (car a) (car b))) (list 0 b))
	((eq (car a) (car b))
	 (cond ((null (cdddr a)) (everypterms b (caddr a) (cadr a) maxpow))
	       (t (substforsum a b maxpow))))
	(t (everysubst1 a b maxpow))))

(defun everypterms (x p n maxpow)
  (if (< (cadr x) n)
      (list 0 x)
      (prog (k ans q part)
	 (setq k (car x))
	 (setq x (cdr x))
	 l    (setq q (min maxpow (quotient (car x) n)))
	 m    (when (eqn q 0)
		(return (if (null x)
			    ans
			    (cons 0 (cons (psimp k x) ans)))))
	 (setq part (everysubst p (cadr x) q))
	 (setq ans (nconc (everypterms1 part k n (car x)) ans))
	 (setq x (cddr x))
	 (when (null x)
	   (setq q 0)
	   (go m))
	 (go l))))

(defun everypterms1 (l k n j)
  (do ((ptr l (cddr ptr)))
      ((null ptr) l)
    (setf (cadr ptr)
	  (ptimes (psimp k (list (- j (* n (car ptr))) 1))
		  (cadr ptr)))))

(defun substforsum (a b maxpow)
  (do ((pow 0 (1+ pow))
       (quot) (zl-rem) (ans))
      ((not (< pow maxpow)) (list* maxpow b ans))
    (desetq (quot zl-rem) (pdivide b a))
    (unless (and (eqn (cdr quot) 1)
		 (not (pzerop (car quot)))
		 (eqn (cdr zl-rem) 1))
      (return (cons pow (cons b ans))))
    (unless (pzerop (car zl-rem))
      (setq ans (cons pow (cons (car zl-rem) ans))))
    (setq b (car quot))))

(defun prodcoef (a b)
  (cond ((pcoefp a)
	 (cond ((pcoefp b) (quotient b a)) (t (prodcoef1 a b))))
	((pcoefp b) (pzero))
	((pointergp (car a) (car b)) (pzero))
	((eq (car a) (car b))
	 (cond ((null (cdddr a))
		(prodcoef (caddr a) (pterm (cdr b) (cadr a))))
	       (t (sumcoef a b))))
	(t (prodcoef1 a b))))

(defun sumcoef (a b)
  (desetq (a b) (pdivide b a))
  (if (and (equal (cdr a) 1) (equal (cdr b) 1))
      (car a)
      (pzero)))

(defun prodcoef1 (a b)
  (loop with ans = (pzero)
	 for (bexp bcoef) on (p-terms b) by #'cddr
	 for part = (prodcoef a bcoef)
	 unless (pzerop part)
	 do (setq ans (pplus ans (psimp (p-var b) (list bexp part))))
	 finally (return ans)))

(defun pureprod (x)
  (or (atom x)
      (and (not (atom (cdr x)))
	   (null (cdddr x))
	   (pureprod (caddr x)))))

(defmfun $bothcoef (r var)
  (prog (*var h varlist genvar $ratfac)
     (unless ($ratp r)
       (return `((mlist)
		 ,(setq h (coeff r var 1.))
		 ((mplus) ,r ((mtimes) -1 ,h ,var)))))
     (newvar var)
     (setq h (and varlist (car varlist)))
     (newvar r)
     (setq var (cdr (ratrep* var)))
     (setq r (cdr (ratrep* r)))
     (and h (setq h (caadr (ratrep* h))))
     (cond ((and h (or (pcoefp (cdr r)) (pointergp h (cadr r)))
		 (equal 1 (cdr var)))
	    (setq var (bothprodcoef (car var) (car r)))
	    (return (list '(mlist)
			  (rdis* (ratreduce (car var) (cdr r)))
			  (rdis* (ratreduce (cdr var) (cdr r))))))
	   (t
             ;; CAN'T TELL WHAT BROUGHT US TO THIS POINT, SORRY
             (merror (intl:gettext "bothcoef: invalid arguments."))))))

;;COEFF OF A IN B

(defun bothprodcoef (a b)
  (let ((c (prodcoef a b)))
    (if (pzerop c) (cons (pzero) b) (cons c (pdifference b (ptimes c a))))))

(defvar argsfreeofp nil)

(defmfun argsfreeof (var e)
  (let ((argsfreeofp t)) (freeof var e)))

;;; This is a version of freeof for a list first argument
(defmfun $lfreeof (l e) "`freeof' for a list first argument"
	 (unless ($listp l)
           (merror (intl:gettext "lfreeof: first argument must be a list; found: ~M") l))
	 (let ((exp ($totaldisrep e)))
	   (dolist (var (margs l) t)
	     (unless (freeof ($totaldisrep var) exp) (return nil)))))

(defmfun $freeof (&rest args)
  (prog (l e)
     (setq l (mapcar #'$totaldisrep (nreverse args))
	   e (car l))
     loop (or (setq l (cdr l)) (return t))
     (if (freeof (getopr (car l)) e) (go loop))
     (return nil)))

(defun freeof (var e)
  (cond ((alike1 var e) nil)
	((atom e) t)
	((and (not argsfreeofp)
              (or (alike1 var ($verbify (caar e)))
                  (alike1 var ($nounify (caar e)))))
         nil)
	((and (or (member (caar e) '(%product %sum %laplace) :test #'eq)
		  (and (eq (caar e) '%integrate) (cdddr e))
		  (and (eq (caar e) '%limit) (cddr e)))
	      (alike1 var (caddr e)))
	 (freeofl var (cdddr e)))
	((eq (caar e) '%at)
	 (cond ((not (freeofl var (hand-side (caddr e) 'r))) nil)
	       ((not (freeofl var (hand-side (caddr e) 'l))) t)
	       (t (freeof var (cadr e)))))
	((and (eq (caar e) 'lambda) (member var (cdadr e) :test #'eq)) t)
        ;; Check for a local variable in a block.
        ((and (eq (caar e) 'mprog) (member var (cdadr e) :test #'eq)) t)
        ;; Check for a loop variable.
        ((and (eq (caar e) 'mdo) (alike1 var (cadr e))) t)
	(argsfreeofp (freeofl var (margs e)))
	(t (freeofl var (cdr e)))))

(defun freeofl (var l) (loop for x in l always (freeof var x)))

(defmfun hand-side (e flag)
  (setq e (if (eq (caar e) 'mequal) (ncons e) (cdr e)))
  (mapcar #'(lambda (u) (if (eq flag 'l) (cadr u) (caddr u))) e))

;; subtitle radcan

(defmfun $radcan (exp)
  (cond ((mbagp exp) (cons (car exp) (mapcar '$radcan (cdr exp))))
	(t (let (($ratsimpexpons t))
	     (simplify (let (($expop 0) ($expon 0))
			 (radcan1 (fr1 exp nil))))))))

(defun radcan1 (*exp)
  (cond ((atom *exp) *exp)
	(t (let (($factorflag t) varlist genvar $ratfac $norepeat
		 ($gcd (or $gcd (car *gcdl*)))
		 (radcanp t))
	     (newvar *exp)
	     (setq *exp (cdr (ratrep* *exp)))
	     (setq varlist
		   (mapcar
		    #'(lambda (x) (cond
				    ((atom x) x)
				    (t (cons (car x)
					     (mapcar 'radcan1 (cdr x))))))
		    varlist))
	     (spc0)
	     (fr1 (rdis *exp) nil)))))

(defun spc0 ()
  (prog (*v *loglist)
     (if (allatoms varlist) (return nil))
     (setq varlist (mapcar #'spc1 varlist)) ;make list of logs
     (setq *loglist (factorlogs *loglist))
     (mapc #'spc2 *loglist)		   ;subst log factorizations
     (mapc #'spc3 varlist genvar)	   ;expand exponents
     (mapc #'spc4 varlist)		   ;make exponent list
     (desetq (varlist . genvar) (spc5 *v varlist genvar))
					;find expon dependencies
     (setq varlist (mapcar #'rjfsimp varlist)) ;restore radicals
     (mapc #'spc7 varlist)))		       ;simplify radicals

(defun allatoms (l)
  (loop for x in l always (atom x)))

(defun rjfsimp (x &aux expon)
  (cond ((and *radsubst $radsubstflag) x)
	((not (m$exp? (setq x (let ($logsimp) (resimplify x))))) x)
	((mlogp (setq expon (caddr x))) (cadr expon))
	((not (and (mtimesp expon) (or $logsimp *var))) x)
	(t (do ((rischflag (and *var (not $logsimp) (not (freeof *var x))))
		(power (cdr expon) (cdr power))) ;POWER IS A PRODUCT
	       ((null power) x)
	     (cond ((numberp (car power)))
		   ((mlogp (car power))
		    (and rischflag (cdr power) (return x))
		    (return
		      `((mexpt) ,(cadar power)
			,(muln	(remove (car power) (cdr expon) :count 1 :test #'equal)
				nil))))
		   (rischflag (return x)))))))

(defun dsubsta (x y zl)
  (cond ((null zl) zl)
	(t (cond ((alike1 y (car zl)) (rplaca zl x))
		 ((not (atom (car zl))) (dsubsta x y (cdar zl))))
	   (dsubsta x y (cdr zl))
	   zl)))

(defun radsubst (a b)
  (setq *exp (allsubst00 a b *exp))
  (if *radsubst (setq *exp2 (allsubst00 a b *exp2))))

(setq *var nil)

(defun spc1 (x)
  (cond ((mlogp x) (putonloglist x))
	((and (mexptp x) (not (eq (cadr x) '$%e)))
         ($exp-form (list '(mtimes)
                          (caddr x)
                          (putonloglist (list '(%log simp ratsimp)
                                              (cadr x))))))
	(t x)))

(defun putonloglist (l)
  (unless (memalike l *loglist) (push l *loglist))
  l)

(defun spc2 (p)
  (radsubst (rform (cdr p)) (rform (car p)))
  (dsubsta (cdr p) (car p) varlist))

(defun spc2a (x)			;CONVERTS FACTORED
  (let ((sum (mapcar #'spc2b x)))	;RFORM LOGAND TO SUM
    (if (cdr sum)		        ;OF LOGS
	(cons '(mplus) sum)
	(car sum))))

(defun spc2b (x)
  (let ((log `((%log simp ratsimp irreducible) ,(pdis (car x)))))
    (if (equal 1 (cdr x)) log
	(list '(mtimes) (cdr x) log))))

(defun spc3 (x v &aux y)
  (when (and (m$exp? x)
	     (not (atom (setq y (caddr x))))
	     (mplusp (setq y (expand1 (if *var ($partfrac y *var) y) 10 10))))
    (setq y (cons '(mtimes) 
                  (mapcar #'(lambda (z) ($ratsimp ($exp-form z))) (cdr y))))
    (radsubst (rform y) (rget v))
    (dsubsta y x varlist)))

(defun spc4 (x)
  (if (and (m$exp? x)
	   (not (memalike (caddr x) *v)))
      (push (caddr x) *v)))

(defun rzcontent (r)
  (destructuring-let (((c1 p) (pcontent (car r)))
		      ((c2 q) (pcontent (cdr r))))
    (if (pminusp p) (setq p (pminus p) c1 (cminus c1)))
    (cons (cons c1 c2) (cons p q))))

;;The GCDLIST looks like (( GCM1pair occurrencepair11 occurrencepair12 ...) ...
;;(GCMnpair occurrencepairn1 occurrencepairn2 ...))
;;where GCMpairs are lists of ratforms and prefix forms for the greatest common
;;multiple of the occurrencepairs.  Each of these pairs is a list of a ratform
;;and a prefix form.  The prefix form is a pointer into the varlist.
;;The occurrences are exponents of the base %E.

(defun spc5 (vl oldvarlist oldgenvar &aux gcdlist varlist genvar)
  (dolist (v vl)
    (destructuring-let* ((((c1 . c) . r) (rzcontent (rform v)))
			 (g (assoc r gcdlist :test #'equal)))
      (cond (g (setf (cadr g) (plcm c (cadr g)))
               (push (list ($exp-form (div* v c1)) c) (cddr g)))
            (t (push (list r c (list ($exp-form (div* v c1)) c)) gcdlist)))))
  (dolist (g gcdlist)
    (let ((rd (rdis (car g))))
      (when (and (mlogp rd) (memalike (cadr rd) oldvarlist))
	(push (list (cadr rd) 1) (cddr g)))
      (rplaca g ($exp-form (div rd (cadr g))))))
  (spc5b gcdlist oldvarlist oldgenvar))

;;(DEFUN SPC5B (V VARLIST GENVAR)
;;  (DOLIST (L V)
;;     (DOLIST (X (CDDR L))
;;	     (UNLESS (EQUAL (CADR L) (CADR X))
;;		     (RADSUBST (RATEXPT (RFORM (CAR L))
;;					(CAR (QUOTIENT (CADR X) (CADR L))))
;;				      (RFORM (CAR X))))))
;;  (CONS VARLIST GENVAR))


(defun spc5b (v varlist genvar)
  (dolist (l v)
    (dolist (x (cddr l))
      (unless (equal (cadr l) (cadr x))
	(radsubst (ratexpt (rform (car l))
			   (quotient (cadr l) (cadr x)))
		  (rform (car x))))))
  (cons varlist genvar))

(defun spc7 (x)
  (if (eq x '$%i) (setq x '((mexpt) -1 ((rat) 1 2))))
  (when (and (mexptp x)
	     (ratnump (caddr x)))
    (let ((rad (rform x))
	  (rbase (rform (cadr x)))
	  (expon (caddr x)))
      (radsubst (ratexpt rbase (cadr expon))
		(ratexpt rad (caddr expon))))))


(defun goodform (l) ;;bad -> good
  (loop for (exp coef) on l by #'cddr
	 collect (cons exp coef)))

(defun factorlogs (l)
  (prog (negl posl maxpl maxnl maxn)
     (dolist (log l)
       (setq log
	     (cons log (goodform
			(ratfact (rform (radcan1 (cadr log)))
				 #'pfactor))))
       (cond ((equal (caadr log) -1) (push log negl))
	     (t (push log posl))))
     (setq negl (flsort negl) posl (flsort posl) l (append negl posl))
     (setq negl (mapcar #'cdr negl)
	   posl (mapcar #'cdr posl))
     a     (setq negl (delete '((-1 . 1)) negl :test #'equal))
     (or negl
	 (return (mapc #'(lambda (x) (rplacd x (spc2a (cdr x)))) l)))
     (setq maxnl (flmaxl negl)
	   maxn (caaar maxnl))
     b     (setq maxpl (flmaxl posl))
     (cond ((and maxpl (flgreat (caaar maxpl) maxn))
	    (setq posl (flred posl (caaar maxpl)))
	    (go b))
	   ((and maxpl
		 (not (equal (caaar maxpl) maxn)))
	    (setq maxpl nil)))
     (cond ((and (flevenp maxpl) (not (flevenp maxnl)))
	    (mapc #'(lambda (fp) (rplaca (car fp) (pminus (caar fp)))
			    (cond ((oddp (cdar fp))
				   (setq fp (delete '(-1 . 1) fp :test #'equal))
				   (setq negl (delete fp negl :test #'equal))
				   (and (cdr fp) (push (cdr fp) posl)))))
		  maxnl)
	    (go a))
	   (t (setq posl (flred posl maxn)
		    negl (flred negl maxn))
	      (go a)))))

(defun flevenp (pl)
  (loop for l in pl never (oddp (cdar l))))

(defun flred (pl p)
  (mapl #'(lambda (x) (if (equal p (caaar x))
			  (rplaca x (cdar x))))
	pl)
  (delete nil pl :test #'equal))

(defun flmaxl (fpl)			;lists of fac. polys
  (cond ((null fpl) nil)
	(t (do ((maxl (list (car fpl))
		      (cond ((equal (caaar maxl) (caaar ll))
			     (cons (car ll) maxl))
			    ((flgreat (caaar maxl) (caaar ll)) maxl)
			    (t (list (car ll)))))
		(ll (cdr fpl) (cdr ll)))
	       ((null ll) maxl)))))

(defun flsort (fpl)
  (mapc #'(lambda (x) (rplacd x (sort (cdr x) #'flgreat :key #'car)))
	fpl))

(defun nmt (p any)
  (cond ((pcoefp p)
	 (if (or any (cminusp p)) 1 0))
	(t (loop for lp on (p-terms p) by #'cddr
		  sum (nmt (cadr lp) any)))))

(defun nmterms (p)
  (cond ((equal p -1) (cons 0 0))
	(t (cons (nmt p nil) (nmt p t)))))

(defun flgreat (p q)
  (let ((pn (nmterms p)) (qn (nmterms q)))
    (cond ((> (car pn) (car qn)) t)
	  ((< (car pn) (car qn)) nil)
	  ((> (cdr pn) (cdr qn)) t)
	  ((< (cdr pn) (cdr qn)) nil)
	  (t (flgreat1 p q)))))

(defun flgreat1 (p q)
  (cond ((numberp p)
	 (cond ((numberp q) (> p q))
	       (t nil)))
	((numberp q) t)
	((pointergp (car p) (car q)) t)
	((pointergp (car q) (car p)) nil)
	((> (cadr p) (cadr q)) t)
	((< (cadr p) (cadr q)) nil)
	(t (flgreat1 (caddr p) (caddr q)))))
