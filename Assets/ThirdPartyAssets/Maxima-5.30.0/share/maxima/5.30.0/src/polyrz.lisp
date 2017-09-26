;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module polyrz)

(declare-top (special errrjfflag $programmode varlist
		      $ratepsilon $ratprint $factorflag genvar
		      equations $keepfloat $ratfac $rootsepsilon
		      $multiplicities))

(load-macsyma-macros ratmac)

;;	PACKAGE FOR FINDING REAL ZEROS OF UNIVARIATE POLYNOMIALS
;;	WITH INTEGER COEFFICIENTS USING STURM SEQUENCES.

(defmfun $realroots (exp &optional (eps $rootsepsilon))
  (setq exp (meqhk exp))
  (when ($ratp exp)
    (setq exp ($ratdisrep exp)))
  (when (or (not (mnump eps)) (mnegp eps) (equal eps 0))
    (merror (intl:gettext "realroots: second argument must be a positive number; found: ~M") eps))
  (let (($keepfloat nil))
    (sturmseq exp eps)))

(defun unipoly (exp)
  (setq exp (cadr (ratf exp)))
  (cond ((and (not (atom exp))
	      (loop for v in (cdr exp)
		     when (not (atom v))
		     do (return nil)
		     finally (return t)))
	 ;;(EVERY #'ATOM (CDR EXP)))
	 exp)
	(t (merror (intl:gettext "UNIPOLY: argument must be a univariate polynomial; found: ~M") exp))))

(defun makrat (pt)
  (cond ((floatp pt) (maxima-rationalize pt))
	((numberp pt) (cons pt 1))
	(($bfloatp pt) (bigfloat2rat pt))
	((atom pt) (merror (intl:gettext "MAKRAT: argument must be a number; found: ~M") pt))
	((equal (caar pt) 'rat) (cons (cadr pt) (caddr pt)))
	(t (merror (intl:gettext "MAKRAT: argument must be a number; found: ~M") pt))))

(declare-top (special equations))

(defun sturmseq (exp eps)
  (let (varlist equations $factorflag $ratprint $ratfac)
    (cond ($programmode
	   (cons '(mlist)
		 (multout (findroots (psqfr (pabs (unipoly exp)))
				     (makrat eps)))))
	  (t (solve2 (findroots (psqfr (pabs (unipoly exp)))
				(makrat eps)))
	     (cons '(mlist) equations)))))

(declare-top (unspecial equations))

(defmfun sturm1 (poly eps &aux b llist)
  (setq b (cons (root-bound (cdr poly)) 1))
  (setq llist (isolat poly (cons (- (car b)) (cdr b)) b))
  (mapcar #'(lambda (int) (refine poly (car int) (cdr int) eps)) llist))

(defun root-bound (p)
  (prog (n lcf loglcf coef logb)
     (setq n (car p))
     (setq lcf (abs (cadr p)))
     (setq loglcf (- (integer-length lcf) 2))
     (setq logb 1)
     loop (cond ((null (setq p (cddr p))) (return (expt 2 logb)))
		((< (setq coef (abs (cadr p))) lcf) (go loop)))
     (setq logb (max logb (1+ (ceil (- (integer-length coef) loglcf 1) (- n (car p))))))
     (go loop)))

(defun ceil (a b)
  (+ (quotient a b)			;CEILING FOR POS A,B
     (signum (rem a b))))

(defun sturmapc (fn llist multiplicity)
  (cond ((null llist) nil)
	(t  (cons (funcall fn (car llist))
		  (cons  multiplicity
			 (sturmapc fn (cdr llist) multiplicity)))) ))

(defun findroots (l eps)
  (cond ((null l) nil)
	((numberp (car l)) (findroots (cddr l) eps))
	(t (append (sturmapc 'sturmout (sturm1 (car l) eps)(cadr l))
		   (findroots (cddr l) eps) )) ))

(defun sturmout (int)
  (list '(mequal simp) (car varlist)
	(midout (rhalf (rplus* (car int) (cadr int)))) ))

(defun midout (pt)
  (cond ((equal (cdr pt) 1) (car pt))
	($float (fpcofrat1 (car pt) (cdr pt)))
	(t (list '(rat simp) (car pt) (cdr pt))) ))

(defun uprimitive (p)
  (pquotient p (ucontent p)))		;PRIMITIVE UNIVAR. POLY

(defun sturm (p)
  (prog (p1 p2 seq r)
     (setq p1 (uprimitive  p))
     (setq p2 (uprimitive (pderivative p1 (car p1))))
     (setq seq (list p2 p1))
     a    (setq r (prem p1 (pabs p2)))
     (cond ((pzerop r) (return (reverse seq))))
     (setq p1 p2)
     (setq p2 (pminus (uprimitive r)))
     (push p2 seq)
     (go a) ))

(defun signum(x)
  (cond ((zerop x) 0)
	((minusp x) -1)
	(t 1)))

;;	IVAR COUNTS SIGN CHANGES IN A STURM SEQUENCE

(defun ivar (seq pt)
  (prog (v s ls)
     (setq v 0)
     (setq ls 0)
     a    (cond ((null seq)(return v)))
     (setq s (reval (car seq) pt))
     (setq seq (cdr seq))
     (cond ((minusp (* s  ls))(setq v (1+ v)))
	   ((not (zerop ls))(go a)))
     (setq ls s)
     (go a) ))

(defun ivar2 (seq pt)
  (cond ((not (atom pt)) (ivar seq pt))
	(t (setq seq (mapcar (function leadterm) seq))
	   (ivar seq (cons pt 1)) )))

;;	OUTPUT SIGN(P(R)) , R RATIONAL (A.B)

(defun reval (p r)
  (cond ((pcoefp p) (signum p))
	((zerop (car r)) (signum (pterm (cdr p) 0)))
	(t (prog (a b bi v m c)
	      (setq bi 1)
	      (setq v 0)
	      (setq p (cdr p))
	      (setq m (car p))
	      (setq a (car r))
	      (setq b (cdr r))
	      a    (cond ((equal m (car p)) (setq c (cadr p))
			  (setq p (cddr p)))
			 (t (setq c 0)))
	      (cond ((zerop m) (return (signum (+ v (* bi c))))))
	      (setq v (* a (+ v (* bi c))))
	      (setq bi (* bi b))
	      (setq m (1- m))
	      (go a) ))))

(defun makpoint (pt)
  (cond ((eq pt '$inf) 1)
	((eq pt '$minf) -1)
	(t (makrat (let (($numer t))
		     (meval pt))))))

(defmfun $nroots (exp &optional (l '$minf) (r '$inf))
  (let (varlist $keepfloat $ratfac)
    (nroots (unipoly (meqhk exp)) (makpoint l) (makpoint r))))

(defun nroots (p l r)
  (rootaddup (psqfr p) l r))

(defun rootaddup (llist l r)
  (cond ((null llist) 0)
	((numberp (car llist)) (rootaddup (cddr llist) l r))
	(t (+ (rootaddup (cddr llist) l r)
		 (* (cadr llist) (nroot1 (car llist) l r)))) ))

(defun nroot1 (p l r)
  (let ((seq (sturm p)))
    (- (ivar2 seq l) (ivar2 seq r))))

;;	RETURNS ROOT IN INTERVAL OF FORM (A,B])

(defun isolat (p l r)
  (prog (seq lv rv mid midv tlist islist rts)
     (setq seq (sturm p))
     (setq lv (ivar seq l))
     (setq rv (ivar seq r))
     (setq tlist (setq islist nil))
     (cond ((equal lv rv) (return nil)))
     a	(cond ((> (setq rts (- lv rv)) 1)(go b))
	      ((equal rts 1)(setq islist (cons (cons l r) islist))))
     (cond ((null tlist) (return islist)))
     (setq lv (car tlist))
     (setq rv (cadr tlist))
     (setq l (caddr tlist))
     (setq r (cadddr tlist))
     (setq tlist (cddddr tlist))
     (go a)
     b	(setq mid (rhalf (rplus* l r)))
     (setq midv (ivar seq mid))
     (cond ((not (equal lv midv))
	    (setq tlist (append (list lv midv l mid) tlist))))
     (setq l mid)
     (setq lv midv)
     (go a)))

(defun refine (p l r eps)
  (prog (sr mid smid)
     (cond ((zerop (setq sr (reval p r)))
	    (return (list r r))) )
     a	(cond ((rlessp (rdifference* r l) eps)
	       (return (list l r))) )
     (setq mid (rhalf (rplus* l r)))
     (setq smid (reval p mid))
     (cond ((zerop smid)(return (list mid mid)))
	   ((equal smid sr)(setq r mid))
	   (t (setq l mid)) )
     (go a)))

(defun rhalf (r) (rreduce (car r) (* 2 (cdr r))))

(defun rreduce (a b)
  (let ((g (abs (gcd a b))))
    (cons (truncate a g) (truncate b g))) )

(defun rplus* (a b)
  (cons (+ (* (car a) (cdr b)) (* (car b) (cdr a)))
	(* (cdr a) (cdr b))))

(defun rdifference* (a b)
  (rplus* a (cons (- (car b)) (cdr b))) )

(defun rlessp (a b)
  (< (* (car a) (cdr b))
     (* (car b) (cdr a)) ))


;;; This next function is to do what SOLVE2 should do in programmode
(defun multout (rootlist)
  (progn
    (setq rootlist (do ((rtlst)
			(multlst)
			(lunch rootlist))
		       ((null lunch) (cons (reverse rtlst)
					   (reverse multlst)))
		     (setq rtlst (cons (car lunch) rtlst))
		     (setq multlst (cons (cadr lunch) multlst))
		     (setq lunch (cddr lunch))))
    (setq $multiplicities (cons '(mlist)  (cdr rootlist)))
    (car rootlist)))

(declare-top (unspecial equations))
