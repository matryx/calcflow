;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
;;	** (c) Copyright 1982 Massachusetts Institute of Technology **

(macsyma-module comm2)

;;;; DIFF2

(declare-top (special $props $dotdistrib))

(defmfun diffint (e x)
  (let (a)
    (cond ((null (cdddr e))
	   (cond ((alike1 x (caddr e)) (cadr e))
		 ((and (not (atom (caddr e))) (atom x) (not (free (caddr e) x)))
		  (mul2 (cadr e) (sdiff (caddr e) x)))
		 ((or ($constantp (setq a (sdiff (cadr e) x)))
		      (and (atom (caddr e)) (free a (caddr e))))
		  (mul2 a (caddr e)))
		 (t (simplifya (list '(%integrate) a (caddr e)) t))))
	  ((alike1 x (caddr e)) (addn (diffint1 (cdr e) x x) t))
	  (t (addn (cons (if (equal (setq a (sdiff (cadr e) x)) 0)
			     0
			     (simplifya (list '(%integrate) a (caddr e)
					      (cadddr e) (car (cddddr e)))
					t))
			 (diffint1 (cdr e) x (caddr e)))
		   t)))))

(defun diffint1 (e x y)
  (let ((u (sdiff (cadddr e) x)) (v (sdiff (caddr e) x)))
    (list (if (pzerop u) 0 (mul2 u (maxima-substitute (cadddr e) y (car e))))
	  (if (pzerop v) 0 (mul3 v (maxima-substitute (caddr e) y (car e)) -1)))))

(defmfun diffsumprod (e x)
  (cond ((or (not ($mapatom x)) (not (free (cadddr e) x)) (not (free (car (cddddr e)) x)))
	 (diff%deriv (list e x 1)))
	((eq (caddr e) x) 0)
	(t (let ((u (sdiff (cadr e) x)))
	     (setq u (simplifya (list '(%sum)
				      (if (eq (caar e) '%sum) u (div u (cadr e)))
				      (caddr e) (cadddr e) (car (cddddr e)))
				t))
	     (if (eq (caar e) '%sum) u (mul2 e u))))))

(defmfun difflaplace (e x)
  (cond ((or (not (atom x)) (eq (cadddr e) x)) (diff%deriv (list e x 1)))
	((eq (caddr e) x) 0)
	(t ($laplace (sdiff (cadr e) x) (caddr e) (cadddr e)))))

(defmfun diff-%at (e x)
  (cond ((freeof x e) 0)
	((not (freeofl x (hand-side (caddr e) 'r))) (diff%deriv (list e x 1)))
	(t ($at (sdiff (cadr e) x) (caddr e)))))

(defmfun diffncexpt (e x)
  (let ((base* (cadr e))
	(pow (caddr e)))
    (cond ((and (mnump pow) (or (not (eq (ml-typep pow) 'fixnum)) (< pow 0))) ; POW cannot be 0
	   (diff%deriv (list e x 1)))
	  ((and (atom base*) (eq base* x) (free pow base*))
	   (mul2* pow (list '(mncexpt) base* (add2 pow -1))))
	  ((ml-typep pow 'fixnum)
	   (let ((deriv (sdiff base* x))
		 (ans nil))
	     (do ((i 0 (1+ i))) ((= i pow))
	       (push (list '(mnctimes) (list '(mncexpt) base* i)
			   (list '(mnctimes) deriv
				 (list '(mncexpt) base* (- pow 1 i))))
		     ans))
	     (addn ans nil)))
	  ((and (not (depends pow x)) (or (atom pow) (and (atom base*) (free pow base*))))
	   (let ((deriv (sdiff base* x))
		 (index (gensumindex)))
	     (simplifya
	      (list '(%sum)
		    (list '(mnctimes) (list '(mncexpt) base* index)
			  (list '(mnctimes) deriv
				(list '(mncexpt) base*
				      (list '(mplus) pow -1 (list '(mtimes) -1 index)))))
		    index 0 (list '(mplus) pow -1)) nil)))
	  (t (diff%deriv (list e x 1))))))

(defmfun stotaldiff (e)
  (cond ((or (mnump e) (constant e)) 0)
	((or (atom e) (member 'array (cdar e) :test #'eq))
	 (let ((w (mget (if (atom e) e (caar e)) 'depends)))
	   (if w (cons '(mplus)
		       (mapcar #'(lambda (x)
				   (list '(mtimes) (chainrule e x) (list '(%del) x)))
			       w))
	       (list '(%del) e))))
	((specrepp e) (stotaldiff (specdisrep e)))
	((eq (caar e) 'mnctimes)
	 (let (($dotdistrib t))
	   (add2 (ncmuln (cons (stotaldiff (cadr e)) (cddr e)) t)
		 (ncmul2 (cadr e) (stotaldiff (ncmuln (cddr e) t))))))
	((eq (caar e) 'mncexpt)
	 (if (and (ml-typep (caddr e) 'fixnum) (> (caddr e) 0))
	     (stotaldiff (list '(mnctimes) (cadr e)
			       (ncpower (cadr e) (1- (caddr e)))))
	     (list '(%derivative) e)))
	(t (addn (cons 0 (mapcar #'(lambda (x)
				     (mul2 (sdiff e x) (list '(%del simp) x)))
				 (extractvars (margs e))))
		 t))))

(defun extractvars (e &aux vars)
  (cond ((null e) nil)
	((atom (car e))
	 (cond ((not (maxima-constantp (car e)))
	        (cond ((setq vars (mget (car e) 'depends))
	               ;; The symbol has dependencies. Put the dependencies on
	               ;; the list of extracted vars.
	               (union* vars (extractvars (cdr e))))
	              (t
	               ;; Put the symbol on the list of extracted vars.
	               (union* (ncons (car e)) (extractvars (cdr e))))))
	     (t (extractvars (cdr e)))))
	((member 'array (cdaar e) :test #'eq)
	 (union* (ncons (car e)) (extractvars (cdr e))))
	(t (union* (extractvars (cdar e)) (extractvars (cdr e))))))

;;;; AT

;;dummy-variable-operators is defined in COMM, which uses it inside of SUBST1.
(declare-top (special atvars *atp* munbound dummy-variable-operators))

(defmfun $atvalue (exp eqs val)
  (let (dl vl fun)
    (cond ((notloreq eqs) (improper-arg-err eqs '$atvalue))
	  ((or (atom exp) (and (eq (caar exp) '%derivative) (atom (cadr exp))))
	   (improper-arg-err exp '$atvalue)))
    (cond ((not (eq (caar exp) '%derivative))
	   (setq fun (caar exp) vl (cdr exp) dl (listof0s vl)))
	  (t (setq fun (caaadr exp) vl (cdadr exp))
	     (dolist (v vl)
	       (setq dl (nconc dl (ncons (or (getf (cddr exp) v) 0)))))))
    (if (or (mopp fun) (eq fun 'mqapply)) (improper-arg-err exp '$atvalue))
    (atvarschk vl)
    (do ((vl1 vl (cdr vl1)) (l atvars (cdr l))) ((null vl1))
      (if (and (symbolp (car vl1)) (not (kindp (car vl1) '$constant)))
	  (setq val (maxima-substitute (car l) (car vl1) val))
	  (improper-arg-err (cons '(mlist) vl) '$atvalue)))
    (setq eqs (if (eq (caar eqs) 'mequal) (list eqs) (cdr eqs)))
    (setq eqs (do ((eqs eqs (cdr eqs)) (l)) ((null eqs) l)
		(if (not (member (cadar eqs) vl :test #'eq))
		    (improper-arg-err (car eqs) '$atvalue))
		(setq l (nconc l (ncons (cons (cadar eqs) (caddar eqs)))))))
    (setq vl (do ((vl vl (cdr vl)) (l)) ((null vl) l)
	       (setq l (nconc l (ncons (cdr (or (assoc (car vl) eqs :test #'eq)
						(cons nil munbound))))))))
    (do ((atvalues (mget fun 'atvalues) (cdr atvalues)))
	((null atvalues)
	 (mputprop fun (cons (list dl vl val) (mget fun 'atvalues)) 'atvalues))
      (when (and (equal (caar atvalues) dl) (equal (cadar atvalues) vl))
	(rplaca (cddar atvalues) val) (return nil)))
    (add2lnc fun $props)
    val))

(defprop %at simp-%at operators)

(defun simp-%at (expr ignored simp-flag)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let* ((arg (simpcheck (cadr expr) simp-flag))
        (e (resimplify (caddr expr)))
        (eqn (if ($listp e)
                 (cons '(mlist simp) (cdr ($sort e)))
                 e)))
    (cond (($constantp arg) arg)
          ((alike1 eqn '((mlist))) arg)
          (t (eqtest (list '(%at) arg eqn) expr)))))

(defmfun $at (expr ateqs)
  (if (notloreq ateqs) (improper-arg-err ateqs '$at))
  (atscan (let ((*atp* t)) ($psubstitute ateqs expr)) ateqs))

(defun atscan (expr ateqs)
  (cond ((or (atom expr)
             (eq (caar expr) 'mrat)
             (like ateqs '((mlist))))
         expr)
        ((eq (caar expr) '%derivative)
         (or (and (not (atom (cadr expr)))
                  (let ((vl (cdadr expr)) dl)
                    (dolist (v vl)
                      (setq dl (nconc dl (ncons (or (getf (cddr expr) v) 0)))))
                    (atfind (caaadr expr)
                            (cdr ($psubstitute ateqs (cons '(mlist) vl)))
                            dl)))
             (list '(%at) expr ateqs)))
        ((member (caar expr) dummy-variable-operators :test #'eq)
         (list '(%at) expr ateqs))
        ((at1 expr))
        (t (recur-apply #'(lambda (x) (atscan x ateqs)) expr))))

(defun at1 (expr)
  (atfind (caar expr) (cdr expr) (listof0s (cdr expr))))

(defun atfind (fun vl dl)
  (do ((atvalues (mget fun 'atvalues) (cdr atvalues)))
      ((null atvalues))
    (and (equal (caar atvalues) dl)
	 (do ((l (cadar atvalues) (cdr l)) (vl vl (cdr vl)))
	     ((null l) t)
	   (if (and (not (equal (car l) (car vl)))
		    (not (eq (car l) munbound)))
	       (return nil)))
         (return (prog2
                    (atvarschk vl)
                    (substitutel vl atvars (caddar atvalues)))))))

(defun listof0s (llist)
  (do ((llist llist (cdr llist)) (l nil (cons 0 l)))
      ((null llist) l)))

(declare-top (special $ratfac genvar varlist $keepfloat *e*))

(defmvar $logconcoeffp nil)
(defmvar superlogcon t)
(defmvar $superlogcon t)

(defmfun $logcontract (e)
  (lgccheck (logcon e))) ; E is assumed to be simplified.

(defun logcon (e)
  (cond ((atom e) e)
	((member (caar e) '(mplus mtimes) :test #'eq)
	 (if (and $superlogcon (not (lgcsimplep e))) (setq e (lgcsort e)))
         (cond ((mplusp e) (lgcplus e))
               ((mtimesp e) (lgctimes e))
               (t (logcon e))))
	(t (recur-apply #'logcon e))))

(defun lgcplus (e)
  (do ((x (cdr e) (cdr x)) (log) (notlogs) (y))
      ((null x)
       (cond ((null log) (subst0 (cons '(mplus) (nreverse notlogs)) e))
             (t
              (setq log (let (($ratfac t)) (sratsimp (muln log t))))
              (addn (cons (lgcsimp log) notlogs) t))))
    (cond ((atom (car x)) (setq notlogs (cons (car x) notlogs)))
	  ((eq (caaar x) '%log) (setq log (cons (logcon (cadar x)) log)))
	  ((eq (caaar x) 'mtimes)
	   (setq y (lgctimes (car x)))
	   (cond ((or (atom y) (not (eq (caar y) '%log)))
		  (setq notlogs (cons y notlogs)))
		 (t (setq log (cons (cadr y) log)))))
	  (t (setq notlogs (cons (logcon (car x)) notlogs))))))

(defun lgctimes (e)
  (setq e (subst0 (cons '(mtimes) (mapcar 'logcon (cdr e))) e))
  (cond ((not (mtimesp e)) e)
	(t (do ((x (cdr e) (cdr x)) (log) (notlogs) (decints))
	       ((null x)
		(cond ((or (null log) (null decints)) e)
		      (t (muln (cons (lgcsimp (power log (muln decints t)))
				     notlogs)
			       t))))
	     (cond ((and (null log) (not (atom (car x)))
			 (eq (caaar x) '%log) (not (equal (cadar x) -1)))
		    (setq log (cadar x)))
		   ((logconcoeffp (car x)) (setq decints (cons (car x) decints)))
		   (t (setq notlogs (cons (car x) notlogs))))))))

(defun lgcsimp (e)
  (cond ((atom e)
         ;; e.g. log(1) -> 0, or log(%e) -> 1
         (simplify (list '(%log) e)))
        ((and (mexptp e) (eq (cadr e) '$%e))
         ;; log(%e^expr) -> expr
         (simplify (list '(%log) e)))
        (t
         (list '(%log simp) e))))

(defun lgcsimplep (e)
  (and (eq (caar e) 'mplus)
       (not (do ((l (cdr e) (cdr l))) ((null l))
	      (cond ((not (or (atom (car l))
			      (not (isinop (car l) '%log))
			      (eq (caaar l) '%log)
			      (and (eq (caaar l) 'mtimes)
				   (null (cdddar l))
				   (mnump (cadar l))
				   (not (atom (caddar l)))
				   (eq (caar (caddar l)) '%log))))
		     (return t)))))))

(defun lgcsort (e)
  (let (genvar varlist ($keepfloat t) vl e1)
    (newvar e)
    (setq vl (do ((vl varlist (cdr vl)) (logs) (notlogs) (decints))
		 ((null vl)
		  (setq logs (sort logs #'great))
		  (nreconc decints (nconc logs (nreverse notlogs))))
	       (cond ((and (not (atom (car vl))) (eq (caaar vl) '%log))
		      (setq logs (cons (car vl) logs)))
		     ((logconcoeffp (car vl))
		      (setq decints (cons (car vl) decints)))
		     (t (setq notlogs (cons (car vl) notlogs))))))
    (setq e1 (ratdisrep (ratrep e vl)))
    (if (alike1 e e1) e e1)))

(defun lgccheck (e)
  (let (num denom)
    (cond ((atom e) e)
	  ((and (eq (caar e) '%log)
		(setq num (member ($num (cadr e)) '(1 -1) :test #'equal))
		(not (equal (setq denom ($denom (cadr e))) 1)))
	   (list '(mtimes simp) -1
		 (list '(%log simp) (if (= (car num) 1) denom (neg denom)))))
	  (t (recur-apply #'lgccheck e)))))

(defun logconcoeffp (e)
  (if $logconcoeffp (let ((*e* e)) (is '(($logconcoeffp) *e*))) (maxima-integerp e)))

;;;; RTCON

(declare-top (special $radexpand $domain))

(defmvar $rootsconmode t)

(defun $rootscontract (e)	       ; E is assumed to be simplified
  (let ((radpe (and $radexpand (not (eq $radexpand '$all)) (eq $domain '$real)))
	($radexpand nil))
    (rtcon e radpe)))

(defun rtcon (e radpe)
  (cond ((atom e) e)
	((eq (caar e) 'mtimes)
	 (do ((x (cdr e) (cdr x)) (roots) (notroots) (y))
	     ((null x)
	      (cond ((null roots) (subst0 (cons '(mtimes) (nreverse notroots)) e))
		    (t (if $rootsconmode
			   (destructuring-let (((min gcd lcm) (rtc-getinfo roots)))
			     (cond ((and (= min gcd) (not (= gcd 1))
					 (not (= min lcm))
					 (not (eq $rootsconmode '$all)))
				    (setq roots
					  (rt-separ
					   (list gcd
						 (rtcon
						  (rtc-fixitup
						   (rtc-divide-by-gcd roots gcd)
						   nil) radpe)
						 1)
					   nil)))
				   ((eq $rootsconmode '$all)
				    (setq roots
					  (rt-separ (simp-roots lcm roots)
						    nil))))))
		       (rtc-fixitup roots notroots))))
	   (cond ((atom (car x))
		  (cond ((eq (car x) '$%i) (setq roots (rt-separ (list 2 -1) roots)))
			(t (setq notroots (cons (car x) notroots)))))
		 ((and (eq (caaar x) 'mexpt) (ratnump (setq y (caddar x))))
		  (setq roots (rt-separ (list (caddr y)
					      (list '(mexpt)
						    (rtcon (cadar x) radpe) (cadr y)))
					roots)))

		 ((and radpe (eq (caaar x) 'mabs))
		  (setq roots (rt-separ (list 2 `((mexpt) ,(rtcon (cadar x) radpe) 2) 1)
					roots)))
		 (t (setq notroots (cons (rtcon (car x) radpe) notroots))))))
	((and radpe (eq (caar e) 'mabs))
	 (power (power (rtcon (cadr e) radpe) 2) '((rat simp) 1 2)))
	(t (recur-apply #'(lambda (x) (rtcon x radpe)) e))))

;; RT-SEPAR separates like roots into their appropriate "buckets",
;; where a bucket looks like:
;; ((<denom of power> (<term to be raised> <numer of power>)
;;		     (<term> <numer>)) etc)

(defun rt-separ (a roots)
  (let ((u (assoc (car a) roots :test #'equal)))
    (cond (u (nconc u (cdr a))) (t (setq roots (cons a roots)))))
  roots)

(defun simp-roots (lcm root-list)
  (let (root1)
    (do ((x root-list (cdr x)))
	((null x) (push lcm root1))
      (push (list '(mexpt) (muln (cdar x) nil) (quotient lcm (caar x)))
	    root1))))

(defun rtc-getinfo (llist)
  (let ((m (caar llist)) (g (caar llist)) (l (caar llist)))
    (do ((x (cdr llist) (cdr x)))
	((null x) (list m g l))
      (setq m (min m (caar x)) g (gcd g (caar x)) l (lcm l (caar x))))))

(defun rtc-fixitup (roots notroots)
  (mapcar #'(lambda (x) (rplacd x (list (sratsimp (muln (cdr x) (not $rootsconmode))))))
	  roots)
  (muln (nconc (mapcar #'(lambda (x) (power* (cadr x) `((rat) 1 ,(car x))))
		       roots)
	       notroots)
	(not $rootsconmode)))

(defun rtc-divide-by-gcd (llist gcd)
  (mapcar #'(lambda (x) (rplaca x (quotient (car x) gcd))) llist)
  llist)

(defmfun $nterms (e)
  (cond ((zerop1 e) 0)
	((atom e) 1)
	((eq (caar e) 'mtimes)
	 (if (equal -1 (cadr e)) (setq e (cdr e)))
	 (do ((l (cdr e) (cdr l)) (c 1 (* c ($nterms (car l)))))
	     ((null l) c)))
	((eq (caar e) 'mplus)
	 (do ((l (cdr e) (cdr l)) (c 0 (+ c ($nterms (car l)))))
	     ((null l) c)))
	((and (eq (caar e) 'mexpt) (integerp (caddr e)) (plusp (caddr e)))
	 ($binomial (+ (caddr e) ($nterms (cadr e)) -1) (caddr e)))
	((specrepp e) ($nterms (specdisrep e)))
	(t 1)))

;;;; ATAN2

(declare-top (special $numer $logarc $trigsign))

;; atan2 distributes over lists, matrices, and equations
(defprop $atan2 (mlist $matrix mequal) distribute_over)

(defun simpatan2 (expr vestigial z)     ; atan2(y,x) ~ atan(y/x)
  (declare (ignore vestigial))
  (twoargcheck expr)
  (let (y x signy signx)
    (setq y (simpcheck (cadr expr) z)
          x (simpcheck (caddr expr) z))
    (cond ((and (zerop1 y) (zerop1 x))
           (merror (intl:gettext "atan2: atan2(0,0) is undefined.")))
          ( ;; float contagion
           (and (or (numberp x) (ratnump x))       ; both numbers
                (or (numberp y) (ratnump y))       ; ... but not bigfloats
                (or $numer (floatp x) (floatp y))) ; at least one float
           (atan2 ($float y) ($float x)))
          ( ;; bfloat contagion
           (and (mnump x)
                (mnump y)
                (or ($bfloatp x) ($bfloatp y)))    ; at least one bfloat
           (setq x ($bfloat x)
                 y ($bfloat y))
           (*fpatan y (list x)))
          ;; Simplifify infinities
          ((or (eq x '$inf)
               (alike1 x '((mtimes) -1 $minf)))
           ;; Simplify atan2(y,inf) -> 0
           0)
          ((or (eq x '$minf)
               (alike1 x '((mtimes) -1 $inf)))
           ;; Simplify atan2(y,minf) -> %pi for realpart(y)>=0 or 
           ;; -%pi for realpart(y)<0. When sign of y unknwon, return noun form.
           (cond ((member (setq signy ($sign ($realpart x))) '($pos $pz $zero)) 
                  '$%pi)
                 ((eq signy '$neg) (mul -1 '$%pi))
                 (t (eqtest (list '($atan2) y x) expr))))
          ((or (eq y '$inf)
               (alike1 y '((mtimes) -1 $minf)))
           ;; Simplify atan2(inf,x) -> %pi/2
           (div '$%pi 2))
          ((or (eq y '$minf)
               (alike1 y '((mtimes -1 $inf))))
           ;; Simplify atan2(minf,x) -> -%pi/2
           (div '$%pi -2))
          ((and (free x '$%i) (setq signx ($sign x))
                (free y '$%i) (setq signy ($sign y))
                (cond ((zerop1 y)
                       (cond ((eq signx '$neg) '$%pi)
                             ((member signx '($pos $pz)) 0)))
                      ((zerop1 x)
                       (cond ((eq signy '$neg) (div '$%pi -2))
                             ((member signy '($pos $pz)) (div '$%pi 2))))
                      ((alike1 y x)
                       (cond ((eq signx '$neg) (mul -3 (div '$%pi 4)))
                             ((member signx '($pos $pz)) (div '$%pi 4))))
                      ((alike1 y (mul -1 x))
                       (cond ((eq signx '$neg) (mul 3 (div '$%pi 4)))
                             ((member signx '($pos $pz)) (div '$%pi -4)))))))
          ($logarc
           (logarc '%atan2 (list ($logarc y) ($logarc x))))
          ((and $trigsign (eq t (mminusp* y)))
           (neg (take '($atan2) (neg y) x)))
          ;; atan2(y,x) = atan(y/x) + pi sign(y) (1-sign(x))/2
          ((eq signx '$pos)
           (take '(%atan) (div y x)))
          ((and (eq signx '$neg)
                (member (setq signy ($csign y)) '($pos $neg) :test #'eq))
           (add (take '(%atan) (div y x))
                (porm (eq signy '$pos) '$%pi)))
          ((and (eq signx '$zero) (eq signy '$zero))
           ;; Unfortunately, we'll rarely get here.  For example,
           ;; assume(equal(x,0)) atan2(x,x) simplifies via the alike1 case above
           (merror (intl:gettext "atan2: atan2(0,0) is undefined.")))
          (t (eqtest (list '($atan2) y x) expr)))))

;;;; ARITHF

(defmfun $fibtophi (e &optional (lnorecurse nil))
  (cond ((atom e) e)
	((eq (caar e) '$fib)
	 (setq e (cond (lnorecurse (cadr e)) (t ($fibtophi (cadr e) lnorecurse))))
	 (let ((phi (meval '$%phi)))
	   (div (add2 (power phi e) (neg (power (add2 1 (neg phi)) e)))
		(add2 -1 (mul2 2 phi)))))
	(t (recur-apply #'(lambda (x) ($fibtophi x lnorecurse)) e))))

(defmspec $numerval (l) (setq l (cdr l))
	  (do ((l l (cddr l)) (x (ncons '(mlist simp)))) ((null l) x)
	    (cond ((null (cdr l)) (merror (intl:gettext "numerval: expected an even number of arguments.")))
		  ((not (symbolp (car l)))
		   (merror (intl:gettext "numerval: expected a symbol; found ~M") (car l)))
		  ((boundp (car l))
		   (merror (intl:gettext "numerval: cannot declare a value because ~M is bound.") (car l))))
	    (mputprop (car l) (cadr l) '$numer)
	    (add2lnc (car l) $props)
	    (nconc x (ncons (car l)))))

(let (my-powers)
  (declare (special my-powers))

  (defmfun $derivdegree (e depvar var)
    (let (my-powers) (declare (special my-powers)) (derivdeg1 e depvar var) (if (null my-powers) 0 (maximin my-powers '$max))))

  (defun derivdeg1 (e depvar var)
    (cond ((or (atom e) (specrepp e)))
	  ((eq (caar e) '%derivative)
	   (cond ((alike1 (cadr e) depvar)
		  (do ((l (cddr e) (cddr l))) ((null l))
		    (cond ((alike1 (car l) var)
			   (return (setq my-powers (cons (cadr l) my-powers)))))))))
	  (t (mapc #'(lambda (x) (derivdeg1 x depvar var)) (cdr e))))))

;;;; BOX

;; Set the the property reversealias
(defprop mbox $box reversealias)
(defprop mlabox $box reversealias)

(defmfun $dpart (&rest args)
  (mpart args nil t nil '$dpart))

(defmfun $lpart (e &rest args)
  (mpart args nil (list e) nil '$lpart))

(defmfun $box (e &optional (l nil l?))
  (if l?
      (list '(mlabox) e (box-label l))
      (list '(mbox) e)))

(defmfun box (e label)
  (if (eq label t)
      (list '(mbox) e)
      ($box e (car label))))

(defun box-label (x)
  (if (atom x)
      x
      (coerce (mstring x) 'string)))

(defmfun $rembox (e &optional (l nil l?))
  (let ((label (if l? (box-label l) '(nil))))
    (rembox1 e label)))

(defun rembox1 (e label)
  (cond ((atom e) e)
	((or (and (eq (caar e) 'mbox)
		  (or (equal label '(nil)) (member label '($unlabelled $unlabeled) :test #'eq)))
	     (and (eq (caar e) 'mlabox)
		  (or (equal label '(nil)) (equal label (caddr e)))))
	 (rembox1 (cadr e) label))
	(t (recur-apply #'(lambda (x) (rembox1 x label)) e))))

;;;; MAPF

(declare-top (special scanmapp))

(defmspec $scanmap (l)
  (let ((scanmapp t))
    (resimplify (apply #'scanmap1 (mmapev l)))))

(defmfun scanmap1 (func e &optional (flag nil flag?))
  (let ((arg2 (specrepcheck e)) newarg2)
    (cond ((eq func '$rat)
	   (merror (intl:gettext "scanmap: cannot apply 'rat'.")))
	  (flag?
	   (unless (eq flag '$bottomup)
	     (merror (intl:gettext "scanmap: third argument must be 'bottomup', if present; found ~M") flag))
	   (if (mapatom arg2)
	       (funcer func (ncons arg2))
	       (subst0 (funcer func
			       (ncons (mcons-op-args (mop arg2)
						     (mapcar #'(lambda (u)
								 (scanmap1 func u '$bottomup))
							     (margs arg2)))))
		       arg2)))
	  ((mapatom arg2)
	   (funcer func (ncons arg2)))
	  (t
	   (setq newarg2 (specrepcheck (funcer func (ncons arg2))))
	     (cond ((mapatom newarg2)
		    newarg2)
		   ((and (alike1 (cadr newarg2) arg2) (null (cddr newarg2)))
		    (subst0 (cons (ncons (caar newarg2))
				  (ncons (subst0
					  (mcons-op-args (mop arg2)
							 (mapcar #'(lambda (u) (scanmap1 func u))
								 (margs arg2)))
					  arg2)))
			    newarg2))
		   (t
		    (subst0 (mcons-op-args (mop newarg2)
					   (mapcar #'(lambda (u) (scanmap1 func u))
						   (margs newarg2)))
			    newarg2)))))))

(defun subgen (form)	   ; This function does mapping of subscripts.
  (do ((ds (if (eq (caar form) 'mqapply) (list (car form) (cadr form))
	       (ncons (car form)))
	   (outermap1 #'dsfunc1 (simplify (car sub)) ds))
       (sub (reverse (or (and (eq 'mqapply (caar form)) (cddr form))
			 (cdr form)))
	    (cdr sub)))
      ((null sub) ds)))

(defun dsfunc1 (dsn dso)
  (cond ((or (atom dso) (atom (car dso))) dso)
	((member 'array (car dso) :test #'eq)
	 (cond ((eq 'mqapply (caar dso))
		(nconc (list (car dso) (cadr dso) dsn) (cddr dso)))
	       (t (nconc (list (car dso) dsn) (cdr dso)))))
	(t (mapcar #'(lambda (d) (dsfunc1 dsn d)) dso))))

;;;; GENMAT

(defmfun $genmatrix (a i2 &optional (j2 i2) (i1 1) (j1 i1))
  (let ((f) (l (ncons '($matrix))))
    (setq f (if (or (symbolp a) (hash-table-p a) (arrayp a))
		#'(lambda (i j) (meval (list (list a 'array) i j)))
	      #'(lambda (i j) (mfuncall a i j))))
    
    (if (notevery #'fixnump (list i2 j2 i1 j1))
      (merror (intl:gettext "genmatrix: bounds must be integers; found ~M, ~M, ~M, ~M") i2 j2 i1 j1))
 	 
    (if (or (> i1 i2) (> j1 j2))
      (merror (intl:gettext "genmatrix: upper bounds must be greater than or equal to lower bounds; found ~M, ~M, ~M, ~M") i2 j2 i1 j1))
 	 
    (dotimes (i (1+ (- i2 i1)))
      (nconc l (ncons (ncons '(mlist)))))
    (do ((i i1 (1+ i))
	 (l (cdr l) (cdr l)))
	((> i i2))
      (do ((j j1 (1+ j)))
	  ((> j j2))
	(nconc (car l) (ncons (funcall f i j)))))
    l))

; Execute deep copy for copymatrix and copylist.
; Resolves SF bug report [ 1224960 ] sideeffect with copylist.
; An optimization would be to call COPY-TREE only on mutable expressions.

(defmfun $copymatrix (x)
  (unless ($matrixp x)
    (merror (intl:gettext "copymatrix: argument must be a matrix; found ~M") x))
  (copy-tree x))

(defmfun $copylist (x)
  (unless ($listp x)
    (merror (intl:gettext "copylist: argument must be a list; found ~M") x))
  (copy-tree x))

(defmfun $copy (x)
  (copy-tree x))

;;;; ADDROW

(defmfun $addrow (m &rest rows)
  (declare (dynamic-extent rows))
  (cond ((not ($matrixp m))
         (merror
           (intl:gettext "addrow: first argument must be a matrix; found ~M")
          m))
        ((null rows) m)
        (t
         (let ((m (copy-tree m)))
           (dolist (r rows m)
             (setq m (addrow m r)))))))

(defmfun $addcol (m &rest cols)
  (declare (dynamic-extent cols))
  (cond ((not ($matrixp m)) (merror (intl:gettext "addcol: first argument must be a matrix; found ~M") m))
	((null cols) m)
	(t (let ((m ($transpose m)))
	     (dolist (c cols ($transpose m))
	       (setq m (addrow m ($transpose c))))))))

(defun addrow (m r)
  (cond ((not (mxorlistp r)) (merror (intl:gettext "addrow or addcol: argument must be a matrix or list; found ~M") r))
	((and (cdr m)
	      (or (and (eq (caar r) 'mlist) (not (= (length (cadr m)) (length r))))
		  (and (eq (caar r) '$matrix)
		       (not (= (length (cadr m)) (length (cadr r))))
		       (prog2 (setq r ($transpose r))
			   (not (= (length (cadr m)) (length (cadr r))))))))
	 (merror (intl:gettext "addrow or addcol: incompatible structure."))))
  (append m (if (eq (caar r) '$matrix) (cdr r) (ncons r))))

;;;; ARRAYF

(defmfun $arraymake (ary subs)
  (cond ((or (not ($listp subs)) (null (cdr subs)))
	 (merror (intl:gettext "arraymake: second argument must be a list of one or more elements; found ~M") subs))
	((eq (ml-typep ary) 'symbol)
	 (cons (cons (getopr ary) '(array)) (cdr subs)))
	(t (cons '(mqapply array) (cons ary (cdr subs))))))

(defmspec $arrayinfo (ary)
  (setq ary (cdr ary))
  (arrayinfo-aux (car ary) (getvalue (car ary))))

(defun arrayinfo-aux (sym val)
  (prog (arra ary)
     (setq arra val)
     (setq ary sym)
     (if (and arra
              (or (hash-table-p arra)
                  (arrayp arra)
                  (eq (marray-type arra) '$functional)))
	 (cond ((hash-table-p arra)
		(let ((dim1 (gethash 'dim1 arra)))
		  (return (list* '(mlist) '$hash_table (if dim1 1 t)
				 (loop for u being the hash-keys in arra using (hash-value v)
				    unless (eq u 'dim1)
				    collect
				    (if (progn v dim1) ;;ignore v
					u
					(cons '(mlist simp) u)))))))
	       ((arrayp arra)
		(return (let ((dims (array-dimensions arra)))
			  (list '(mlist) '$declared
				;; they don't want more info (array-type arra)
			        (length dims)
			        (cons '(mlist) (mapcar #'1- dims))))))
	       ((eq (marray-type arra) '$functional)
	        (return (arrayinfo-aux sym (mgenarray-content arra)))))
	 (let ((gen (safe-mgetl sym '(hashar array))) ary1)
           (when (null gen)
             (merror (intl:gettext "arrayinfo: ~M is not an array.") ary))
	   (setq ary1 (cadr gen))
	   (cond ((eq (car gen) 'hashar)
		  (setq ary1 (symbol-array ary1))
		  (return (append '((mlist simp) $hashed)
				  (cons (aref ary1 2)
					(do ((i 3 (1+ i)) (l)
					     (n (cadr (arraydims ary1))))
					    ((= i n) (sort l #'(lambda (x y) (great y x))))
					  (do ((l1 (aref ary1 i) (cdr l1)))
					      ((null l1))
					    (push (cons '(mlist simp) (caar l1)) l)))))))
		 (t (setq ary1 (arraydims ary1))
		    (return (list '(mlist simp)
				  (cond ((safe-get ary 'array)
					 (cdr (assoc (car ary1)
						     '((t . $complete) (fixnum . $integer)
						       (flonum . $float)) :test #'eq)))
					(t '$declared))
				  (length (cdr ary1))
				  (cons '(mlist simp) (mapcar #'1- (cdr ary1)))))))))))

;;;; ALIAS

(declare-top (special greatorder lessorder))

(defmspec $ordergreat (l)
  (if greatorder (merror (intl:gettext "ordergreat: reordering is not allowed.")))
  (makorder (setq greatorder (reverse (cdr l))) '_))

(defmspec $orderless (l)
  (if lessorder (merror (intl:gettext "orderless: reordering is not allowed.")))
  (makorder (setq lessorder (cdr l)) '|#|))

(defun makorder (l char)
  (do ((l l (cdr l))
       (n 101 (1+ n)))
      ((null l) '$done)
    (alias (car l)
	   (implode (nconc (ncons char) (mexploden n)
			   (exploden (stripdollar (car l))))))))

(defmfun $unorder ()
  (let ((l (delete nil
		 (cons '(mlist simp)
		       (nconc (mapcar #'(lambda (x) (remalias (getalias x))) lessorder)
			      (mapcar #'(lambda (x) (remalias (getalias x))) greatorder)))
		 :test #'eq)))
    (setq lessorder nil greatorder nil)
    l))

;;;; CONCAT

(defun $concat (&rest l)
  (when (null l)
    (merror (intl:gettext "concat: there must be at least one argument.")))
  (let ((result-is-a-string (or (numberp (car l)) (stringp (car l)))))
    (setq l (mapcan #'(lambda (x) (unless (atom x) (merror (intl:gettext "concat: argument must be an atom; found ~M") x)) (string* x)) l))
    (if result-is-a-string
      (coerce l 'string)
      (getalias (implode (cons '#\$ l))))))
