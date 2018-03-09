;;; -*-  mode: lisp; package: cl-maxima; syntax: common-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(declare-top (unspecial p y))

;;   These functions can be used to keep an alphabetical masterlist in
;;*genvar* and *varlist* and use them.  I think *genpairs* is now
;;redundant second genpairs is much smaller than *genpairs* would be and
;;just keeps the pairs needed for the current form.  *varlist* and
;;*genvar* are still the global ones.


;;(ratsetup varlist genvar) does ratsetup1 and ratsetup2.  Which map the
;;above over varlist but also do things all the way down the list.
;;could do (ratsetup *varlist* *genvar*) if you want to fix them up.  to
;;get latest tellrat info and ratweight level info etc.

;;if new-newvar has been called on x and varlist is *varlist* then
;;new-prep1 should have all the variables it wants in genpairs and so we
;;could use the old prep1.  In fact new-newvar must be called first
;;because the newvarmexpt function which handles exponentiation does not
;;have a new- analogue and so will call (newsym) not (add-newvar)

;;    IDEAS NOT YET IMPLEMENTED:      Change the gensym so that instead
;;of allocating a symbol one uses a number (between 1 and 2^16 say).
;;Instead of using the value cell to record the ordering, this is done
;;in an array : so the function for POINTERGP would look like (> (aref
;;genvar x) (aref genvar y)) the functions VALGET and VALPUT would just
;;need changing to (aref genvar x) etc.

;;   Another idea would be to change PTIMES and PPLUS etc. so that their
;;internal calls to themselves would involve another function say
;;NPTIMES which would take as its arguments and values a reusable type
;;of polynomial like a an array etc.  Then one would only need the
;;functions to change would be the functions which change the
;;NPOLYNOMIALS back to the polynomials and vice versa.

;;the following are faster than the previous ones in the ratmac

(defun safe-putprop ( sym value indicator)
  (putprop sym value indicator))

;;(defun POINTERGP (A B) (> (VALGET A) (VALGET B)))
;;as a subst it is faster any problems 'wfs

(defun new-prep1 (x &aux temp)
       (cond ((floatp x)
	      (cond ($keepfloat (cons x 1.0)) ((prepfloat x))))
	     ((integerp x) (cons (cmod x) 1))
     	     ((typep x 'rational)
	      (cond ((null modulus)(cons
				    (numerator x) (denominator x)))
		    (t (cquotient (numerator x) (denominator x)))))

	     ((atom x)(cond ((assolike x genpairs))
			    (t(format t "***In new-prep1**")
					      (add-newvar-to-genpairs x ))))
	     ((and $ratfac (assolike x genpairs)))
	     ((eq (caar x) 'mplus)
	      (cond ($ratfac
		     (setq x (mapcar #'new-prep1 (cdr x)))
		     (cond ((every #'frpoly? x)
			    (cons (mfacpplus (mapl #'(lambda (x)
						      (rplaca x (caar x)))
						  x))
				  1))
			   (t (do ((a (car x) (facrplus a (car l)))
				   (l (cdr x) (cdr l)))
				  ((null l) a)))))
		    (t (do ((a (new-prep1 (cadr x)) (ratplus a (new-prep1 (car l))))
			    (l (cddr x) (cdr l)))
			   ((null l) a)))))
	     ((eq (caar x) 'mtimes)
	      (do ((a (savefactors (new-prep1 (cadr x)))
		      (rattimes a (savefactors (new-prep1 (car l))) sw))
		   (l (cddr x) (cdr l))
		   (sw (not (and $norepeat (member 'ratsimp (cdar x) :test #'eq)))))
		  ((null l) a)))
	     ((eq (caar x) 'mexpt)
	      (newvarmexpt x (caddr x) t))
	     ((eq (caar x) 'mquotient)
	      (ratquotient (savefactors (new-prep1 (cadr x)))
			   (savefactors (new-prep1 (caddr x)))))
	     ((eq (caar x) 'mminus)
	      (ratminus (new-prep1 (cadr x))))
	     ((eq (caar x) 'rat)
	      (cond (modulus (cons (cquotient (cmod (cadr x)) (cmod (caddr x))) 1))
		    (t (cons (cadr x) (caddr x)))))
	     ((eq (caar x) 'bigfloat)(bigfloat2rat x))
	     ((eq (caar x) 'mrat)
	      (cond ((and *withinratf* (member 'trunc (car x) :test #'eq))
		     (throw 'ratf nil))
		    ((catch 'compatvl
		       (progn (setq temp (compatvarl (caddar x)
						     varlist
						     (cadddr (car x))
						     genvar))
			      t))
		     (cond ((member 'trunc (car x) :test #'eq)
			    (cdr ($taytorat x)))
			   ((and (not $keepfloat)
				 (or (pfloatp (cadr x)) (pfloatp (cddr x))))
			    (cdr (ratrep* ($ratdisrep x))))
			   ((sublis temp (cdr x)))))
		    (t (cdr (ratrep* ($ratdisrep x))))))
	     ((assolike x genpairs))
	     (t (setq x (littlefr1 x))
		(cond ((assolike x genpairs))
		      (t (format t "%%in new-prep1")
			 (add-newvar-to-genpairs  x))))))

;;because symbolics will assign a common lisp print name only when the symbol is referred to
(defun safe-string (symb)
  (let ()
    (string symb)))

(defun new-ratf (l &aux  genpairs)
    (prog (u *withinratf*)
	  (setq *withinratf* t)
	  (when (eq '%% (catch 'ratf (new-newvar l))) ;;get the new variables onto *varlist*
	    (setq *withinratf* nil) (return (srf l)))	;new-prep1 should not have to add any.
  (let ((varlist *varlist*)(genvar *genvar*))

	  (setq u (catch 'ratf (new-ratrep* l)))	; for truncation routines
	  (return (or u (prog2 (setq *withinratf* nil) (srf l)))))))



(defun new-newvar (l  )
;  (let (( vlist varlist))
  (my-newvar1 l))
;  (setq varlist (sortgreat vlist))
 ; vlist))
 ; (setq varlist (nconc (sortgreat vlist) varlist)))


(defun new-ratrep* (x)
  ;;the ratsetup is done in my-newvar1
    (xcons (new-prep1 x)
	   (list* 'mrat 'simp *varlist* *genvar*
		  		  (if (and (not (atom x)) (member 'irreducible (cdar x) :test #'eq))
		      '(irreducible)))))

(defun new-rat (x &aux genpairs)
  (cond
    ((polynomialp x) (cons x 1))
    ((rational-functionp x) x)
    ((and (listp x) (eq (caar x) 'mrat))
	 (cond ((member (car (num (cdr x))) *genvar* :test #'eq)
		(cdr x))
	       (t (format t "~%disrepping")(new-rat  ($totaldisrep x)))))
	(t

  (prog (u *withinratf*)
	(setq *withinratf* t)
	(cond ((mbagp x)(return (cons (car x) (mapcar 'new-rat (cdr x)))))
	      (t
	(when (eq '%% (catch 'ratf (new-newvar x)))
	  (setq *withinratf* nil)(return (srf x)))
	(let ((varlist *varlist*)(genvar *genvar*))
	  (setq u (catch 'ratf (new-prep1 x)))  ;;truncations
	  (return (or u (prog2 (setq *withinratf* nil) (srf x)))))))))))


(defun my-newvar1 (x)
       (cond ((numberp x) nil)
	     ((assolike x genpairs) nil)
	    ;;; ((memalike x varlist))we 're using *varlist*
;	;     ((memalike x vlist) nil)
	     ((atom x) (add-newvar-to-genpairs x )nil)
	     ((member (caar x)
		    '(mplus mtimes rat mdifference
			    mquotient mminus bigfloat) :test #'eq)
	      (mapc #'my-newvar1 (cdr x)))

	     ((eq (caar x) 'mexpt)
	       (my-newvar1 (second  x) ))
	     ;; ;(newvarmexpt x (caddr x) nil))
	     ((eq (caar x) 'mrat) (merror " how did you get here Bill?")
	      (and *withinratf* (member 'trunc (cdddar x) :test #'eq) (throw 'ratf '%%))
	      (cond ($ratfac (mapc 'newvar3 (caddar x)))
		    (t (mapc #'my-newvar1 (reverse (caddar x))))))
	     ((eq (caar x) 'mnctimes)(add-newvar-to-genpairs x ))
	     (t (merror "What is x like ? ~A" x))))

;;need this?
;	      (cond (*fnewvarsw (setq x (littlefr1 x))
;				  (mapc (function newvar1)
;					(cdr x))
;				  (or (memalike x vlist)
;				      (memalike x varlist)
;;				      (putonvlist x)))
;;		      (t (putonvlist x))))))

(defun add-newvar-to-genpairs (va &aux the-gensym)
  (cond ((assolike va nil) genpairs)
	(t (setq the-gensym (add-newvar va))
	   (push (cons va (rget the-gensym)) genpairs)
	   (rat-setup1 va the-gensym)(rat-setup2 va the-gensym)))
  nil)


;;might be worthwhile to keep a resource or list of gensyms so that when
;;you reset-vgp then you don't just discard them you reuse them via the gensym call

(defvar *genvar-resemble* t)

(defun add-newvar ( va &optional (use-*genpairs* t)&aux  the-gensym)
  "If va is not in varlist ADD-NEWVAR splices va into the varlist and a new gensym
into genvar ordering and adds to genpairs"
 (declare (special $order_function))
   use-*genpairs*  ;;don't use it
  (cond ((and (symbolp va) (not (eql (aref  (safe-string va) 0) #\$))) (merror "doesn't begin with $")))
  (let ()
   (multiple-value-bind (after there)
       (find-in-ordered-list va *varlist* $order_function)
     (cond ((not there)
	    (setq the-gensym (gensym-readable va))
;	    (cond ((and (symbolp va) *genvar-resemble*)
;                   (setq the-gensym (make-symbol (string-trim "$" (safe-string va)))))
;		  (t
;		   (setq the-gensym (gensym))))

	    (safe-putprop the-gensym va 'disrep)
;	    (cond (use-*genpairs* (push (cons va (rget the-gensym)) *genpairs*)))
;	    (rat-setup1 va the-gensym)(rat-setup2 va the-gensym)
	    (setq *genvar* (nsplice-in after the-gensym *genvar*))
	    (setq *varlist* (nsplice-in after va  *varlist*))
    	    (when  *check-order*
;		   (check-repeats *varlist*)
	      (check-order *varlist*))
	    (loop for v in (nthcdr  (max 0 after) *genvar*)
		  for i from  (1+ after)
		  do (setf (symbol-value v) i)))
	   (there
	    (setq the-gensym (nth after *genvar*))
	    (cond ((not (nc-equal (get the-gensym 'disrep) va))
		   (fsignal "bad-correspondence" )))))
  (values the-gensym (not there)))))

(defun rat-setup1 (v g)
  (and $ratwtlvl
       (setq v (assolike v *ratweights))
       (if v (safe-putprop g v '$ratweight) (remprop g '$ratweight))))



(defun rat-setup2 (v g)
  (when $algebraic
    (cond ((setq v (algpget  v))
	   (let ()
	     (safe-putprop  g  v 'tellrat)))
	  (t (remprop  g 'tellrat)))))



(defun te (f g)
    (let* ((genvar (nreverse (sort (union1 (listovars f) (listovars g)) #'pointergp)))
	   (varlist (loop for v in genvar collecting (get v 'disrep))))
      (break t)
     (ratreduce  f g)))

;;

(defun new-pfactor (poly)
  "returns an alternating list: factor1 expt1 factor2 expt2 ..."
  (let ((genvar (nreverse (sort (listovars poly) #'pointergp))))
    (pfactor poly)))

(defun multiply-factors-with-multiplicity (a-list &aux ( answer 1))
  (loop for v in a-list by #'cddr
	for w in (cdr a-list) by #'cddr
	do (loop while (> w 0)
		 do (setq answer (n* answer v))
		 (setq w (1- w))))
  answer)

(defun copy-vgp ()
  (setq *varlist* (copy-list *varlist*))
  (setq *genvar* (copy-list *genvar*)) nil)


(defun q-var (f)(cond ((atom f) nil)
		      (t (aref f 0))))

(defun ar-last (aray)
  (aref aray (1- (length (the cl:array aray)))))
(defun ar-second-last (aray)
  (aref aray (- (length (the cl:array aray)) 2)))

(defun set-fill-pointer (aray n)(setf (fill-pointer aray ) n) aray)
(defun constant-term-in-main-variable (f)
     (cond ((czerop (ar-second-last f))
	    (ar-last f))
	   (t 0)))

#+debug
(progn
  (defmfun pplus (x y)
    (cond ((pcoefp x) (pcplus x y))
	  ((pcoefp y) (pcplus y x))
	  ((eq (p-var x) (p-var y))
	   (psimp (p-var x) (pplus1 (p-terms y) (p-terms x))))
	  ((pointergp (p-var x) (p-var y))
	   (psimp (p-var x) (pcplus1 y (p-terms x))))
	  (t (psimp (p-var y) (pcplus1 x (p-terms y))))))

  (defmfun ptimes (x y)
    (cond ((pcoefp x) (if (pzerop x) 0 (pctimes x y)))
	  ((pcoefp y) (if (pzerop y) 0 (pctimes y x)))
	  ((eq (p-var x) (p-var y))
	   (palgsimp (p-var x) (ptimes1 (p-terms x) (p-terms y)) (alg x)))
	  ((pointergp (p-var x) (p-var y))
	   (psimp (p-var x) (pctimes1 y (p-terms x))))
	  (t (psimp (p-var y) (pctimes1 x (p-terms y))))))
  (defun ptimes (x y)
    (cond ((atom x)
	   (cond ((and (numberp x)
		       (zerop x))
		  0)
		 (t (pctimes x y))))
	  ((atom y)
	   (cond ((and (numberp y)
		       (zerop y))
		  0)
		 (t (pctimes y x))))
	  ((eq (car x) (car y))
	   (palgsimp (car x) (ptimes1 (cdr x) (cdr y)) (alg x)))
	  ((> (symbol-value (car x)) (symbol-value (car y)))
	   (psimp (car x) (pctimes1 y (cdr x))))
	  (t (psimp (car y) (pctimes1 x (cdr y))))))

  (defmfun pdifference (x y)
    (cond ((pcoefp x) (pcdiffer x y))
	  ((pcoefp y) (pcplus (cminus y) x))
	  ((eq (p-var x) (p-var y))
	   (psimp (p-var x) (pdiffer1 (p-terms x) (p-terms y))))
	  ((pointergp (p-var x) (p-var y))
	   (psimp (p-var x) (pcdiffer2 (p-terms x) y)))
	  (t (psimp (p-var y) (pcdiffer1 x (p-terms y))))))


  (defun pfactor (p &aux ($algebraic algfac*))
    (cond ((pcoefp p) (cfactor p))
	  ($ratfac (pfacprod p))
	  (t (setq p (factorout p))
	     (cond ((equal (cadr p) 1) (car p))
		   ((numberp (cadr p)) (append (cfactor (cadr p)) (car p)))
		   (t ((lambda (cont)
			 (nconc
			  (cond ((equal (car cont) 1) nil)
				(algfac*
				 (cond (modulus (list (car cont) 1))
				       ((equal (car cont) '(1 . 1)) nil)
				       ((equal (cdar cont) 1)
					(list (caar cont) 1))
				       (t (list (caar cont) 1 (cdar cont) -1))))
				(t (cfactor (car cont))))
			  (pfactor11 (psqfr (cadr cont)))
			  (car p)))
		       (cond (modulus (list (leadalgcoef (cadr p))
					    (monize (cadr p))))
			     (algfac* (algcontent (cadr p)))

			     (t (pcontent (cadr p))))))))))


  (defun fullratsimp (l)
    (let (($expop 0) ($expon 0) (inratsimp t) $ratsimpexpons)
      (setq l ($totaldisrep l)) (fr1 l varlist))))


;;the following works but is slow see projective
(defmfun $gcdlist (&rest fns)
  (cond ((and (eq (length fns) 1)
	      ($listp (car fns))
	      (setq fns (cdr (car fns))))))
  (let (varlist  gcd-denom gcd-num rat-fns )
    (cond ((eq (length fns) 1) (car fns))
	  (t
	   (loop for v in fns
	      do (newvar v))
	   (setq rat-fns (loop for v in fns	collecting (cdr (ratrep* v))))
	   (setq gcd-num (num (car rat-fns)))
	   (loop for w in (cdr rat-fns)
	      do
	      (setq gcd-num (pgcd gcd-num (num  w))))
	   (setq gcd-denom (denom (car rat-fns)))
	   (loop for w in (cdr rat-fns)
	      do (setq gcd-denom (pgcd gcd-denom (denom w))))
	   (ratdisrep (cons (list 'mrat 'simp varlist genvar)
			    (cons gcd-num gcd-denom)))))))

;;;;the following works but seems slower than factoring
;(defun $projective ( vector)
;  (check-arg vector '$listp nil)
;  (let  ( VARLIST  (fns (cdr vector))
;			answer gcd-num factor lcm-denom  rat-fns )
;	       (loop for v in fns
;		     do (newvar v))
;	      (setq rat-fns (loop for v in fns
;		     collecting (cdr (ratrep* v))))
;	      (setq gcd-num (num (car rat-fns)))
;	      (loop for w in (cdr rat-fns)
;		    do
;		    (setq gcd-num (pgcd gcd-num (num  w))))
;	      (setq lcm-denom (denom (car rat-fns)))
;	      (loop for w in (cdr rat-fns)
;		    do (setq lcm-denom (plcm lcm-denom (denom w))))
;	      (setq factor (cons lcm-denom gcd-num))
;	      (setq answer (loop for v in rat-fns
;		    collecting (rattimes v factor t)))
;	      (setq header (list 'mrat 'simp varlist genvar))
;	      (loop for v in answer
;		    collecting (ratdisrep (cons header v)) into tem
;		    finally (return (cons '(mlist) tem)))))

(defun factoredp (poly)
  (cond ((atom poly) t)
	(t (member 'factored (car poly) :test #'eq))))

(defun exponent (expr prod)
  (cond ((atom prod) 0)
	((eq (caar prod) 'mexpt)(cond ((eq (second prod) expr)(third prod))
				      (t 0)))
	(t(check-arg prod '$productp nil)
	  (loop for v in (cdr prod) do
	       (cond
		 ((equal expr v) (return 1))
		 ((numberp v))
		 ((atom v))
		 ((and (equal (caar v) 'mexpt)
		       (equal (second v) expr))
		  (return (third v))))
	     finally (return 0)))))

(defun $projective (vector &aux factors first-one factored-vector expon lcm-denom tem fac where proj)
  (setq factored-vector (loop for v in (cdr vector)
			   when (factoredp v) collecting v
			   else collecting ($factor v)))
  (loop for v in factored-vector
     for i from 0
     when (not ($zerop v))
     do (setq first-one v)(setq where i) (return 'done))
  (cond ((null where) 'image_not_in_projective_space)
	(t
	 (setq factored-vector (delete first-one factored-vector :count 1 :test #'equal))
	 (setq proj (loop for w in  factored-vector collecting (div* w first-one)))
	 (loop for term in proj
	    when (not (numberp term) )
	    do
	    (cond ((atom term)(setq fac term))
		  (t
		   (loop for v in (cdr term) do
			(cond ((atom v) (setq fac v))
			      ((eq (caar v) 'mexpt) (setq fac (second v)))
			      ((eq (caar v) 'mplus) (setq fac v)))
			(cond ((not (member fac factors :test #'equal)) (push fac factors)))))))
	 (loop for w in factors
	    do (setq expon 0)
	    (setq expon (loop for v in proj
			   when (< (setq tem (exponent w v)) 0)
			   minimize tem))
	    (cond ((not (eql expon 0))
		   (push  `((mexpt simp) ,w ,expon) lcm-denom))))
	 (cond (lcm-denom (push '(mtimes simp) lcm-denom))
	       (t (setq lcm-denom 1)))
	 (loop for v in proj
	    collecting (div* v lcm-denom) into tem
	    finally (return
		      (cons '(mlist)  (nsplice-in (1- where)
						  (div* 1 lcm-denom) tem)))))))
(defun $zeta3_ratsimp (expr &aux answer)
  (setq answer (new-rat expr))
  (setq answer (rationalize-denom-zeta3 answer))
  (new-disrep answer))

(defun rationalize-denom-zeta3 (expr &aux the-denom the-num the-gen)
  (setq the-gen (add-newvar '$%zeta3))
  (cond ((polynomialp expr) expr)
	((variable-in-polyp (denom expr) the-gen)
	 (setq the-denom  (denom expr))
	 (setq the-num (num expr))
	 (setq the-denom (conj-zeta3 the-denom the-gen))
	 (ratreduce  (ptimes the-num the-denom) (ptimes the-denom (denom expr))))
	(t expr)))

(defun conj-zeta3 (expr the-gen &aux answer)
  (cond ((atom expr) expr)
	((eq (car expr) the-gen)
	 (setq expr (copy-list expr))
	 (setf (second expr) 2)
	 (palgsimp the-gen  (cdr expr) (alg expr)))
	(t (setq answer (copy-list expr))
	   (do ((r (cddr answer)  (cddr r)))
	       ((not (consp r)) answer)
	     (rplaca r (conj-zeta3 (car r) the-gen))))))

(defun variable-in-polyp (poly gen)
  (catch 'its-in
    (variable-in-polyp1 poly gen)))
(defun variable-in-polyp1 (poly gen)
  (cond ((atom poly) nil)
	((eq (car poly) gen) (throw 'its-in t))
	(t
	 (do ((r (cddr poly) (cddr r)))
	     ((not (consp  r)) nil)
	   (variable-in-polyp1 (car r) gen)))))

(defun $zeta3_factor (poly)
  ($factor poly `((mplus) ((mexpt) $%zeta3 2) $%zeta3 1))) ; %zeta3^2+%zeta3+1

(defun new-newvarmexpt (x e flag)
  (declare (special radlist expsumsplit vlist))
       ;; when flag is t, call returns ratform
       (prog (topexp)
	     (cond ((and (fixp e) (not flag))
		    (return (newvar1 (cadr x)))))
	     (setq topexp 1)
	top  (cond

	      ;; x=b^n for n a number
	      ((fixp e)
	       (setq topexp (* topexp e))
	       (setq x (cadr x)))
	      ((atom e) nil)

	      ;; x=b^(p/q) for p and q integers
	      ((eq (caar e) 'rat)
	       (cond ((or (minusp (cadr e)) (greaterp (cadr e) 1))
		      (setq topexp (* topexp (cadr e)))
		      (setq x (list '(mexpt)
				    (cadr x)
				    (list '(rat) 1 (caddr e))))))
	       (cond ((or flag (numberp (cadr x)) ))
		     (*ratsimp*
		      (cond ((memalike x radlist) (return nil))
			    (t (setq radlist (cons x radlist))
			       (return (newvar1 (cadr x))))) )
		     ($algebraic (newvar1 (cadr x)))))
	      ;; x=b^(a*c)
	      ((eq (caar e) 'mtimes)
	       (cond
		((or

		     ;; x=b^(n *c)
		     (and (atom (cadr e))
			  (fixp (cadr e))
			  (setq topexp (* topexp (cadr e)))
			  (setq e (cddr e)))

		     ;; x=b^(p/q *c)
		     (and (not (atom (cadr e)))
			  (eq (caaadr e) 'rat)
			  (not (equal 1 (cadadr e)))
			  (setq topexp (* topexp (cadadr e)))
			  (setq e (cons (list '(rat)
					      1
					      (caddr (cadr e)))
					(cddr e)))))
		 (setq x
		       (list '(mexpt)
			     (cadr x)
			     (setq e (simplify (cons '(mtimes)
						      e)))))
		 (go top))))

	      ;; x=b^(a+c)
	      ((and (eq (caar e) 'mplus) expsumsplit)	;switch controls
	       (setq					;splitting exponent
		x					;sums
		(cons
		 '(mtimes)
		 (mapcar
		  #'(lambda (ll)
		      (list '(mexpt)
			    (cadr x)
			    (simplify (list '(mtimes)
					    topexp
					    ll))))
		  (cdr e))))
	       (cond (flag (return (new-prep1 x)))
		     (t (return (newvar1 x))))))
	     (cond (flag nil)
		   ((equal 1 topexp)
		    (cond ((or (atom x)
			       (not (eq (caar x) 'mexpt)))
			   (newvar1 x))
			  ((or (memalike x varlist) (memalike x vlist))
			   nil)
			  (t (cond ((or (atom x) (null *fnewvarsw))
				    (putonvlist x))
				   (t (setq x (littlefr1 x))
				      (mapc #'newvar1 (cdr x))
				     (or (memalike x vlist)
					 (memalike x varlist)
					 (putonvlist x)))))))
		   (t (newvar1 x)))
	     (return
	      (cond
	       ((null flag) nil)
	       ((equal 1 topexp)
		(cond
		 ((and (not (atom x)) (eq (caar x) 'mexpt))
		  (cond ((assolike x genpairs))
; *** should only get here if called from fr1. *fnewvarsw=nil
			(t (setq x (littlefr1 x))
			 (cond ((assolike x genpairs))
			       (t (new-newsym x))))))
		 (t (new-prep1 x))))
	       (t (ratexpt (new-prep1 x) topexp))))))


(defun new-newsym (e)
  (prog (g p)
	(cond ((setq g (assolike e genpairs))
	       (return g)))
	(setq g (gensym))
	(putprop g e 'disrep)
	(add-newvar e)
;	(push e varlist)
;	(push (cons e (rget g)) genpairs)
;	(valput g (if genvar (1- (valget (car genvar))) 1))
;	(push g genvar)
	(cond ((setq p (and $algebraic (algpget e)))
;	       (algordset p genvar)
	       (putprop g p 'tellrat)))
	(return (rget g))))



;; the tellrat must be compatible with *genvar*

(defun tellrat1 (x &aux varlist genvar $algebraic $ratfac algvar)
  (setq x ($totaldisrep x))
  (and (not (atom x)) (eq (caar x) 'mequal)
       (newvar (cadr x)))
  (newvar (setq x (meqhk x)))
  (or varlist (merror "Improper polynomial"))
  (setq x (primpart (cadr ($new_rat x))))
  (setq algvar (if (symbolp (car x)) (get (car x) 'disrep)))
  (setq x (p-terms x))
  (if (not (equal (pt-lc x) 1)) (merror "Minimal polynomial must be monic"))
  (do ((p (pt-red x) (pt-red p))) ((ptzerop p)) (setf (pt-lc p) (pdis (pt-lc p))))
  (setq algvar (cons algvar x))
  (if (setq x (assol (car algvar) tellratlist))
      (setq tellratlist (remove x tellratlist :test #'equal)))
  (push algvar tellratlist))
