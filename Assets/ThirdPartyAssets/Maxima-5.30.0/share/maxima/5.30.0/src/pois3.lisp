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

(macsyma-module pois3)

;; GENERAL POISSON SERIES

(declare-top (special *argc *coef poisvals b* a* *a ss cc h* poishift
		      poistsm poists $poisz $pois1))

(defvar trim nil)

;;; THESE ARE THE ONLY COEFFICIENT DEPENDENT ROUTINES.

;;; POISCDECODE DECODES A COEFFICIENT
(defun poiscdecode (x) x)

;;; INTOPOISCO PUTS AN EXPRESSION INTO POISSON COEFFICIENT FORM
(defun intopoisco (x) (simplifya x nil))

;;; POISCO+ ADDS 2 COEFFICIENTS
(defun poisco+ (r s) (simplifya (list '(mplus) r s) nil))

;;; POISCO* MULTIPLIES 2 COEFFICIENTS
(defun poisco* (r s) (simplifya (list '(mtimes) r s) nil))

;;; HALVE DIVIDES A COEFFICIENT BY 2
(defun halve (r)
  (simplifya (list '(mtimes) '((rat) 1 2) r) nil))

;;; POISSUBSTCO SUBSTITUTES AN EXPRESSION FOR A VARIABLE IN A COEFFICIENT.
(defun poissubstco (a b c)
  (maxima-substitute a b c))

;;; THIS DIFFERENTIATES A COEFFICIENT
(defun poiscodif (h var)
  ($diff h var))

;;; THIS INTEGRATES A COEFFICIENT
(defun poiscointeg (h var)
  (intopoisco($integrate (poiscdecode h) var)))

;;; TEST FOR ZERO
(defun poispzero (x) (zerop1 x))

(defun fumcheck (x)
  (not (and (atom x) (integerp x) (< (abs x) poistsm))))

(defun checkencode(r)
  (prog(q)
     (setq q ($coeff r '$u))
     (cond ((fumcheck q) (return nil))
	   (t (setq r (simplifya (list '(mplus) r (list '(mtimes) -1  '$u q)) nil))))
     (setq q ($coeff r '$v))
     (cond ((fumcheck q)(return nil))
	   (t (setq r (simplifya (list '(mplus) r (list '(mtimes) -1  '$v q)) nil))))
     (setq q ($coeff r '$w))
     (cond ((fumcheck q)(return nil))
	   (t (setq r (simplifya (list '(mplus) r (list '(mtimes) -1  '$w q)) nil))))
     (setq q ($coeff r '$x))
     (cond ((fumcheck q)(return nil))
	   (t (setq r (simplifya (list '(mplus) r (list '(mtimes) -1  '$x q)) nil))))
     (setq q ($coeff r '$y))
     (cond ((fumcheck q)(return nil))
	   (t (setq r (simplifya (list '(mplus) r (list '(mtimes) -1  '$y q)) nil))))
     (setq q ($coeff r '$z))
     (cond ((fumcheck q)(return nil))
	   (t (setq r (simplifya (list '(mplus) r (list '(mtimes) -1  '$z q)) nil))))
     (cond ((equal r 0)(return t))
	   (t (return nil)))))

(defmfun $poissimp (x)
  (if (mbagp x)
      (cons (car x) (mapcar #'$poissimp (cdr x)))
      ($outofpois x)))

;;;********

;; ABOVE ASSUMES POISLIM(5) OR LESS ALSO REDEFINE ORDER< AND ORDER= TO BE < AND =

;;; THIS TELLS THE EVALUATOR TO KEEP OUT OF POISSON $SERIES.

(defprop mpois (lambda (x) x) mfexpr*)

(defmfun $poisplus (a b)
  (setq a (intopois a) b (intopois b))
  (list '(mpois simp) (poismerge22 (cadr a) (cadr b)) (poismerge22 (caddr a) (caddr b))))

(declare-top (special *b *fn))

(defmfun $poismap (p sinfn cosfn)
  (prog (*b *fn)
     (setq p (intopois p))
     (setq *fn (list sinfn))
     (return (list (car p)
		   (poismap (cadr p))
		   (prog2 (setq *fn (list cosfn)) (poismap (caddr p)))))))

(defun poismap (y)
  (cond ((null y) nil)
	(t (setq *b (meval (list *fn (poiscdecode (cadr y)) (poisdecodec (car y)))))
	   (tcons3(car y) (intopoisco  *b) (poismap (cddr y))))))

(defun poismerge22 (r s)
  (cond ((null r) s)
	((null s) r)
	((equal (car r) (car s))
	 (prog (tt)
	    (setq tt (poisco+ (cadr r) (cadr s)))
	    (return (cond ((poispzero tt) (poismerge22 (cddr r) (cddr s)))
			  (t (cons (car s) (cons tt (poismerge22 (cddr r) (cddr s)))))))))
	((< (car r) (car s)) (cons (car r) (cons (cadr r) (poismerge22 (cddr r) s))))
	(t (cons (car s) (cons (cadr s) (poismerge22 (cddr s) r))))))

(defun poiscosine (m)
  (setq m (poisencode m))
  (cond ((poisnegpred m) (setq m (poischangesign m))))
  (list '(mpois simp) nil (list m 1)))

(defun poissine (m)
  (setq m (poisencode m))
  (cond ((poisnegpred m) (list '(mpois simp) (list (poischangesign m) -1) nil))
	(t (list '(mpois simp) (list m 1) nil))))

(defmfun $intopois (x)
  (let (*a)
    (intopois x)))

(defun intopois (a)
  (cond ((atom a)
	 (cond ((equal a 0) $poisz) (t (list '(mpois simp) nil (list poishift (intopoisco a))))))
	((eq (caar a) 'mpois) a)
	((eq (caar a) '%sin) (poissine (cadr a)))
	((eq (caar a) '%cos) (poiscosine (cadr a)))
	((and (eq (caar a) 'mexpt) (numberp (caddr a)) (> (caddr a) 0.))
	 ($poisexpt (intopois (cadr a)) (caddr a)))
	((eq (caar a) 'mplus)
	 (setq *a (intopois (cadr a)))
	 (mapc (function (lambda (z) (setq *a ($poisplus *a (intopois z))))) (cddr a))
	 *a)
	((eq (caar a) 'mtimes)
	 (setq *a (intopois (cadr a)))
	 (mapc (function (lambda (z) (setq *a ($poistimes *a (intopois z))))) (cddr a))
	 *a)
	((eq (caar a) 'mrat) (intopois (ratdisrep a)))
	(t (list '(mpois simp) nil (list poishift (intopoisco a))))))

(defun tcons (r s)
  (if (poispzero (car s))
      (cdr s)
      (cons r s)))

(defun poisnegpred ($n)
  (prog ($r)
   $loop (cond ((equal $n 0) (return nil))
	       (t nil))
   (setq $r (- (rem $n poists) poistsm))
   (cond ((> $r 0) (return nil))
	 ((> 0 $r) (return t))
	 (t (setq $n (quotient $n poists))))
   (go $loop)))

(defun poischangesign ($n)
  (- (* poishift 2) $n))

(declare-top (special $u $v $w $x $y $z))

(defun poisencode (h*)
  (unless (checkencode h*)
    ;; NOT CLEAR WHAT IS ILLEGAL HERE
    (merror (intl:gettext "poissimp: illegal argument: ~M") h*))
  (apply #'(lambda ($z $y $x $w $v $u)
	     (declare (special $u $v $w $x $y $z))
	     (setq h* (meval h*))
	     ;; NOT CLEAR WHAT IS ILLEGAL HERE EITHER
	     (unless (integerp h*) (merror  (intl:gettext "poisson: illegal trigonometric argument.")))
	     (+ poishift  h*))
	 poisvals))

(let ((n 5))
   (setq poists (expt 2 n)
	 poisvals (loop for i from 5 downto 0 collect (expt poists i))
	 poistsm (expt 2 (1- n))
	 poishift (loop for i from 0 to 5 sum (* poistsm (expt poists i)))
	 $poisz '((mpois simp) nil nil)
	 $pois1 (list '(mpois simp) nil (list poishift 1)))
   n)

(defun poisdecodec (m)
  (prog (arg h)
     (setq h m)
     (setq arg (list '(mtimes) (- (rem h poists) poistsm) '$u))
     (setq h (quotient h poists))
     (setq arg
	   (list '(mplus)
		 arg
		 (list '(mtimes) (- (rem h poists) poistsm) '$v)))
     (setq h (quotient h poists))
     (setq arg
	   (list '(mplus)
		 arg
		 (list '(mtimes) (- (rem h poists) poistsm) '$w)))
     (setq h (quotient h poists))
     (setq arg
	   (list '(mplus)
		 arg
		 (list '(mtimes) (- (rem h poists) poistsm) '$x)))
     (setq h (quotient h poists))
     (setq arg
	   (list '(mplus)
		 arg
		 (list '(mtimes) (- (rem h poists) poistsm) '$y)))
     (setq h (quotient h poists))
     (setq arg
	   (list '(mplus)
		 arg
		 (list '(mtimes) (- (rem h poists) poistsm) '$z)))
     (return (simplifya arg nil))))


;;; THIS PROGRAM MULTIPLIES A POISSON SERIES P BY A NON-SERIES, C,
;;; WHICH IS FREE OF SINES AND COSINES .

(defmfun $poisctimes (c p)
  (list '(mpois simp) (poisctimes1 (setq c (intopoisco c)) (cadr p)) (poisctimes1 c (caddr p))))

(defmfun $outofpois (p)
  (prog (ans)
     (cond ((or (atom p) (not (eq (caar p) 'mpois))) (setq p (intopois p))))

     ;; DO SINES
     (do ((m
	   (cadr p)
	   (cddr m)))(
		      (null m))
       (setq ans (cons (list '(mtimes)
			     (poiscdecode (cadr m))
			     (list '(%sin) (poisdecodec (car m))))
		       ans)))

     ;; DO COSINES
     (do ((m
	   (caddr p)
	   (cddr m)))(
		      (null m))
       (setq ans (cons (list '(mtimes)
			     (poiscdecode (cadr m))
			     (cond ((equal (car m) poishift) 1)
				   (t (list '(%cos) (poisdecodec (car m))))))
		       ans)))
     (return (cond ((null ans) 0.) (t (simplifya (cons '(mplus) ans) nil))))))

(defmfun $printpois (p)
  (prog nil
     (setq p (intopois p))

     ;; DO SINES
     (do ((m
	   (cadr p)
	   (cddr m)))(
		      (null m))
       (displa (simplifya (list '(mtimes)
				(poiscdecode (cadr m))
				(list '(%sin) (poisdecodec (car m))))
			  t))
       (terpri))

     ;; DO COSINES
     (do ((m
	   (caddr p)
	   (cddr m)))(
		      (null m))
       (displa (simplifya (list '(mtimes)
				(poiscdecode (cadr m))
				(cond ((equal (car m) poishift) 1.)
				      (t (list '(%cos) (poisdecodec (car m))))))
			  t))
       (terpri))
     (return '$done)))


;;; $POISDIFF DIFFERENTIATES A POISSON SERIES WRT X, Y, Z, U, V, W, OR A COEFF VAR.


(defmfun $poisdiff (p m)
  (declare (special m))
  (cond ((member m '($u $v $w $x $y $z) :test #'eq)
	 (list (car p) (cosdif (caddr p) m) (sindif (cadr p) m)))
	(t (list (car p) (poisdif4(cadr p)) (poisdif4 (caddr p))))))


(defun poisdif4 (y)
  (declare (special m))
  (cond ((null y) nil)
	(t (tcons3 (car y)(poiscodif (cadr y) m) (poisdif4 (cddr y))))))


;;; COSDIF DIFFERENTIATES COSINES TO GET SINES

(defun cosdif (h m)
  (cond ((null h) nil)
	(t (tcons (car h)
		  (cons (poisco* (intopoisco (- (poisxcoef (car h) m))) (cadr h))
			(cosdif (cddr h) m))))))

(defun sindif (h m)
  (cond ((null h) nil)
	(t (tcons (car h)
		  (cons (poisco* (intopoisco (poisxcoef (car h) m)) (cadr h)) (sindif (cddr h) m))))))

(defun poisxcoef (h m)
  (- (rem (quotient h
		    (expt poists
			  (cadr (member m '($u 0 $v 1 $w 2 $x 3 $y 4 $z 5) :test #'eq))))
	  poists)
     poistsm))


;;; AVL BALANCED TREE SEARCH AND INSERTION.
;;; NODE LOOKS LIKE (KEY (LLINK .  RLKINK) BALANCEFACTOR .  RECORD)
;;; PROGRAM FOLLOWS ALGORITHM GIVEN IN KNUTH VOL. 3 455-57

(declare-top (special ans))


;; MACROS TO EXTRACT FIELDS FROM NODE

(defmacro key  (&rest l) (cons 'car l))

(defmacro llink  (&rest l) (cons 'caadr l))

(defmacro rlink  (&rest l) (cons 'cdadr l))

(defmacro bp  (&rest l) (cons 'caddr l))

(defmacro rec  (&rest l) (cons 'cdddr l))


;;  FOR ORDERING KEYS

(defmacro order<  (&rest l) (cons '<  l))
(defmacro order=  (&rest l) (cons '=  l))

;; MACROS TO SET FIELDS IN NODE

(defmacro setrlink  (&rest l) (setq l (cons nil l))
	  (list 'rplacd (list 'cadr (cadr l)) (caddr l)))

(defmacro setllink  (&rest l) (setq l (cons nil l))
	  (list 'rplaca (list 'cadr (cadr l)) (caddr l)))

(defmacro setbp  (&rest l) (setq l (cons nil l))
	  (list 'rplaca (list 'cddr (cadr l)) (caddr l)))

(defmacro setrec  (&rest l)(setq l (cons nil l))
	  (list 'rplacd (list 'cddr (cadr l)) (caddr l)))


(defun insert-it (pp newrec) (setrec pp (poisco+ (rec pp) newrec)))

(defun avlinsert (k newrec head)
  (prog (qq tt ss pp rr)
     (setq tt head)
     (setq ss (setq pp (rlink head)))
     a2   (cond ((order< k (key pp)) (go a3))
		((order< (key pp) k) (go a4))
		(t (insert-it pp newrec) (return head)))
     a3   (setq qq (llink pp))
     (cond ((null qq) (setllink pp (cons k (cons (cons nil nil) (cons 0. newrec)))) (go a6))
	   ((order= 0. (bp qq)) nil)
	   (t (setq tt pp ss qq)))
     (setq pp qq)
     (go a2)
     a4   (setq qq (rlink pp))
     (cond ((null qq) (setrlink pp (cons k (cons (cons nil nil) (cons 0. newrec)))) (go a6))
	   ((order= 0 (bp qq)) nil)
	   (t (setq tt pp ss qq)))
     (setq pp qq)
     (go a2)
     a6   (cond ((order< k (key ss)) (setq rr (setq pp (llink ss)))) (t (setq rr (setq pp (rlink ss)))))
     a6loop
     (cond ((order< k (key pp)) (setbp pp -1) (setq pp (llink pp)))
	   ((order< (key pp) k) (setbp pp 1) (setq pp (rlink pp)))
	   ((order= k (key pp)) (go a7)))
     (go a6loop)
     a7   (cond ((order< k (key ss)) (go a7l)) (t (go a7r)))
     a7l  (cond ((order= 0. (bp ss)) (setbp ss -1) (setllink head (1+ (llink head))) (return head))
		((order= (bp ss) 1) (setbp ss 0) (return head)))
     (cond ((order= (bp rr) -1) nil) (t (go a9l)))
     (setq pp rr)
     (setllink ss (rlink rr))
     (setrlink rr ss)
     (setbp ss 0)
     (setbp rr 0)
     (go a10)
     a9l  (setq pp (rlink rr))
     (setrlink rr (llink pp))
     (setllink pp rr)
     (setllink ss (rlink pp))
     (setrlink pp ss)
     (cond ((order= (bp pp) -1.) (setbp ss 1.) (setbp rr 0.))
	   ((order= (bp pp) 0.) (setbp ss 0.) (setbp rr 0.))
	   ((order= (bp pp) 1.) (setbp ss 0.) (setbp rr -1.)))
     (setbp pp 0.)
     (go a10)
     a7r  (cond ((order= 0. (bp ss)) (setbp ss 1.) (setllink head (1+ (llink head))) (return head))
		((order= (bp ss) -1.) (setbp ss 0.) (return head)))
     (cond ((order= (bp rr) 1.) nil) (t (go a9r)))
     (setq pp rr)
     (setrlink ss (llink rr))
     (setllink rr ss)
     (setbp ss 0.)
     (setbp rr 0.)
     (go a10)
     a9r  (setq pp (llink rr))
     (setllink rr (rlink pp))
     (setrlink pp rr)
     (setrlink ss (llink pp))
     (setllink pp ss)
     (cond ((order= (bp pp) 1.) (setbp ss -1.) (setbp rr 0.))
	   ((order= (bp pp) 0.) (setbp ss 0.) (setbp rr 0.))
	   ((order= (bp pp) -1.) (setbp ss 0.) (setbp rr 1.)))
     (setbp pp 0.)
     a10  (cond ((eq ss (rlink tt)) (setrlink tt pp)) (t (setllink tt pp)))
     (return head)))

(defun avlinit (key rec)
  (cons 'top (cons (cons 0. (cons key (cons (cons nil nil) (cons 0. rec)))) (cons 0. nil))))


;; UNTREE CONVERTS THE TREE TO A LIST WHICH LOOKS LIKE ( SmALLEST-KEY RECORD NEXT-SMALLEST-KEY RECORD ....  LARGEST-KEY
;;RECORD)

(defun untree (h) (prog (ans) (untree1 (rlink h)) (return ans)))

(defun untree1 (h)
  (cond ((null h) ans)
	((null (rlink h)) (setq ans (tcons3 (key h) (rec h) ans)) (untree1 (llink h)))
	(t (setq ans (tcons3 (key h) (rec h) (untree1 (rlink h)))) (untree1 (llink h)))))

(defun tcons3 (r s tt) (cond ((poispzero s) tt) (t (cons r (cons s tt)))))


(defun poismerges (a ae l)
  (cond ((equal poishift ae) l)		; SINE(0) IS 0
	((poisnegpred ae) (poismerge (poisco* -1 a) (poischangesign ae) l))
	(t (poismerge a ae l))))

(defun poismergec (a ae l)
  (cond ((poisnegpred ae) (poismerge a (poischangesign ae) l)) (t (poismerge a ae l))))

(defun poismerge (a ae l) (cond ((poispzero a) nil) (t (merge11 a ae l))))

(defun poismerge2 (r s)
  (cond ((null r) s)
	((null s) r)
	(t (prog (m n tt)
	      (setq m (setq n (cons 0. r)))
	      a    (cond ((null r) (rplacd m s) (return (cdr n)))
			 ((null s) (return (cdr n)))
			 ((equal (car r) (car s))
			  (setq tt (poisco+ (cadr r) (cadr s)))
			  (cond ((poispzero tt) (rplacd m (cddr r)) (setq r (cddr r) s (cddr s)))
				(t (rplaca (cdr r) tt) (setq s (cddr s) r (cddr r) m (cddr m)))))
			 ((> (car r) (car s))
			  (rplacd m s)
			  (setq s (cddr s))
			  (rplacd (cddr m) r)
			  (setq m (cddr m)))
			 (t (setq r (cddr r)) (setq m (cddr m))))
	      (go a)))))

(defun merge11 (a ae l)
  (poismerge2 (list ae a) l))

(defun poismergesx (a ae l)
  (cond ((equal poishift ae) l)		; SINE(0) IS 0
	((poisnegpred ae) (avlinsert (poischangesign ae) (poisco* -1 a) l))
	(t (avlinsert ae a l))))

(defun poismergecx (a ae l)
  (cond ((poisnegpred ae) (avlinsert (poischangesign ae) a l)) (t (avlinsert ae a l))))

(defun poisctimes1 (c h)
  (cond ((null h) nil)
	((and trim (trimf (car h))) (poisctimes1 c (cddr h)))
	(t (tcons (car h) (cons (poisco* c (cadr h)) (poisctimes1 c (cddr h)))))))

(defun trimf (m)
  (meval (list '($poistrim)
	       (poisxcoef m '$u)
	       (poisxcoef m '$v)
	       (poisxcoef m '$w)
	       (poisxcoef m '$x)
	       (poisxcoef m '$y)
	       (poisxcoef m '$z))))

(defmfun $poistimes (a b)
  (prog (slc clc temp ae aa zero trim t1 t2 f1 f2)
     (setq a (intopois a) b (intopois b))
     (cond ((or (getl-lm-fcn-prop '$poistrim '(expr subr))
		(mget '$poistrim 'mexpr))
	    (setq trim t)))
     (cond ((nonperiod a) (return ($poisctimes (cadr (caddr a)) b)))
	   ((nonperiod b) (return ($poisctimes (cadr (caddr b)) a))))
     (setq slc (avlinit poishift (setq zero (intopoisco 0.))) clc (avlinit poishift zero))
     ;; PROCEED THROUGH ALL THE SINES IN ARGUMENT A
     (do ((sla
	   (cadr a)
	   (cddr sla)))(
			(null sla))
       (setq aa (halve (cadr sla)) ae (car sla))
       ;; SINE(U)*SINE(V) ==> (-COSINE(U+V) + COSINE(U-V))/2
       (do ((slb
	     (cadr b)
	     (cddr slb)))(
			  (null slb))
	 (setq t1 (+ ae poishift (- (car slb))) t2 (+ ae (- poishift) (car slb)))
	 (cond(trim(setq f1(trimf t1) f2 (trimf t2)))
	      (t (setq f1 nil f2 nil)))
	 (setq temp (poisco* aa (cadr slb)))
	 (cond ((poispzero temp) nil)
	       (t (or f1 (poismergecx temp t1 clc))
		  (or f2 (poismergecx (poisco* -1 temp) t2 clc)))))
       ;; SINE*COSINE ==> SINE + SINE
       (do ((clb
	     (caddr b)
	     (cddr clb)))(
			  (null clb))
	 (setq t1 (+ ae poishift (- (car clb))) t2 (+ ae (- poishift) (car clb)))
	 (cond(trim(setq f1(trimf t1) f2 (trimf t2)))
	      (t (setq f1 nil f2 nil)))
	 (setq temp (poisco* aa (cadr clb)))
	 (cond ((poispzero temp) nil)
	       (t (or f1 (poismergesx temp t1 slc)) (or f2 (poismergesx temp t2 slc))))))
     ;; PROCEED THROUGH ALL THE COSINES IN ARGUMENT A
     (do ((cla
	   (caddr a)
	   (cddr cla)))(
			(null cla))
       (setq aa (halve (cadr cla)) ae (car cla))
       ;; COSINE*SINE ==> SINE - SINE
       (do ((slb
	     (cadr b)
	     (cddr slb)))(
			  (null slb))
	 (setq t1 (+ ae poishift (- (car slb)))
	       t2 (+ ae (- poishift) (car slb)))
	 (cond (trim (setq f1 (trimf t1) f2 (trimf t2)))
	       (t (setq f1 nil f2 nil)))
	 (cond (t (setq temp (poisco* aa (cadr slb)))
		  (cond ((poispzero temp) nil)
			(t (or f1 (poismergesx (poisco* -1 temp) t1 slc))
			   (or f2 (poismergesx temp t2 slc)))))))
       ;; COSINE*COSINE ==> COSINE + COSINE
       (do ((clb (caddr b) (cddr clb)))
	   ((null clb))
	 (setq t1 (+ ae poishift (- (car clb)))
	       t2 (+ ae (- poishift) (car clb)))
	 (cond (trim (setq f1 (trimf t1) f2 (trimf t2)))
	       (t (setq f1 nil f2 nil)))
	 (cond
	   (t (setq temp (poisco* aa (cadr clb)))
	      (cond ((poispzero temp) nil)
		    (t (or f1 (poismergecx temp t1 clc))
		       (or f2 (poismergecx temp t2 clc))))))))
     (return (list '(mpois simp) (untree slc) (untree clc)))))

(defmfun $poisexpt (p n)
  (prog (u h)
     (cond ((oddp n) (setq u p)) (t (setq u (setq h (intopois 1.)))))
     a    (setq n (ash n -1))
     (cond ((zerop n) (return u)))
     (setq p ($poistimes p p))
     (cond ((oddp n) (setq u (cond ((equal u h) p) (t ($poistimes u p))))))
     (go a)))

(defmfun $poissquare (a) ($poisexpt a 2))

;;; $POISINT INTEGRATES A POISSON SERIES WRT X,Y, Z, U, V, W.  THE VARIABLE OF
;;; INTEGRATION MUST OCCUR ONLY IN THE ARGUMENTS OF SIN OR COS,
;;; OR ONLY IN THE COEFFICIENTS.  POISCOINTEG IS CALLED TO INTEGRATE COEFFS.

;;; NON-PERIODIC TERMS ARE REMOVED.

(defmfun $poisint (p m)
  (declare (special m))
  (prog (b*)
     (setq p (intopois p))
     (cond ((member m '($u $v $w $x $y $z) :test #'eq)
	    (return (list (car p)
			  (cosint* (caddr p) m)
			  (sinint* (cadr p) m))))
	   (t (return (list (car p)
			    (poisint4 (cadr p))
			    (poisint4 (caddr p))))))))

(defun poisint4 (y)
  (declare (special m))
  (cond ((null y) nil)
	(t (tcons3 (car y)(poiscointeg (cadr y) m) (poisint4 (cddr y))))))

;;;COSINT* INTEGRATES COSINES TO GET SINES

(defun cosint* (h m)
  (cond ((null h) nil)
	((equal 0 (setq b* (poisxcoef (car h) m))) (cosint* (cddr h) m))
	(t (tcons (car h)
		  (cons (poisco* (intopoisco (list '(mexpt) b* -1)) (cadr h))
			(cosint* (cddr h) m))))))

(defun sinint* (h m)
  (cond ((null h) nil)
	((equal 0 (setq b* (poisxcoef (car h) m))) (sinint* (cddr h) m))
	(t (tcons (car h)
		  (cons (poisco* (intopoisco (list '(mexpt) (- (poisxcoef (car h) m)) -1))
				 (cadr h))
			(sinint* (cddr h) m))))))


;;; $POISSUBST SUBSTITUTES AN EXPRESSION FOR A VARIABLE IN ARGUMENT OF TRIG FUNCTIONS OR
;;; COEFFICIENTS.

(defun poissubsta (a b* c)
  (prog (ss cc)
     (setq h* (- (poisencode (list '(mplus) a (list '(mtimes) -1 b*))) poishift))
     (poissubst1s (cadr c))
     (poissubst1c (caddr c))
     (return (list (car c) ss cc))))

(defun poissubst1s (c)
  (cond ((null c) nil)
	(t (setq ss (poismerges (cadr c) (argsubst (car c)) ss))
	   (poissubst1s (cddr c)))))

(defun poissubst1c (c)
  (cond ((null c) nil)
	(t (setq cc (poismergec (cadr c) (argsubst (car c)) cc))
	   (poissubst1c (cddr c)))))

(defun argsubst (c)
  (+ c (* h* (poisxcoef c b*))))

(defmfun $poissubst (aa bb cc &optional dd nn)
  (if (and dd nn)
      (fancypoissubst aa bb (intopois cc) (intopois dd) nn)
      (let ((a* aa) (b* bb) (c (intopois cc)))
	(if (member b* '($u $v $w $x $y $z) :test #'eq)
	    (poissubsta a* b* c)
	    (list (car c) (poissubstco1 (cadr c)) (poissubstco1 (caddr c)))))))

(declare-top (unspecial $u $v $w $x $y $z))

(defun poissubstco1 (c)
  (if (null c)
      nil
      (tcons (car c) (cons (poissubstco a* b* (cadr c)) (poissubstco1 (cddr c))))))

(declare-top (special dc ds *ans))

(defun fancypoissubst (a b* c d n)
  ;;SUBSTITUTES A+D FOR B IN C, WHERE D IS EXPANDED IN POWERSERIES TO ORDER N
  (prog (h* dc ds *ans)
     (setq *ans (list '(mpois simp) nil nil) d (intopois d) dc (intopois 1) ds (intopois 0))
     (when (equal n 0) (return ($poissubst a b* c)))
     (fancypois1s d 1 1 n)
     (setq h* (- (poisencode (list '(mplus) a (list '(mtimes) -1 b*))) poishift))
     (fancypas (cadr c))
     (fancypac (caddr c))
     (return *ans)))

(defun fancypois1s (d dp n lim)	; DP IS LAST POWER: D^(N-1), LIM IS HIGHEST TO
  (cond ((> n lim) nil)		;GO
	(t (setq ds ($poisplus ds
			       ($poisctimes (list '(rat)
						  (expt -1 (ash (1- n) -1))
						  (factorial n))
					    (setq dp ($poistimes dp d)))))
	   (fancypois1c d dp (1+ n) lim))))

(defun fancypois1c (d dp n lim)	; DP IS LAST POWER: D^(N-1), LIM IS HIGHEST TO
  (cond ((> n lim) nil)		;GO
	(t (setq dc
		 ($poisplus dc
			    ($poisctimes (list '(rat) (expt -1 (ash n -1)) (factorial n))
					 (setq dp ($poistimes dp d)))))
	   (fancypois1s d dp (1+ n) lim))))

;;; COS(R+K*B) ==> K*COS(R+K*A)*DC - K*SIN(R+K*A)*DS
;;; SIN(R+K*B) ==> K*COS(R+K*A)*DS + K*SIN(R+K*A)*DC

(defun fancypac (c)
  (prog nil
     (cond ((null c) (return nil)))
     (setq *coef (poisxcoef (car c) b*))
     (cond ((equal *coef 0)
	    (setq *ans ($poisplus *ans (list '(mpois simp) nil (list (car c) (cadr c)))))
	    (go end)))
     (cond ((poispzero (setq *coef (poisco* (cadr c) (intopoisco *coef)))) (go end)))
     (setq *argc (argsubst (car c)))
     (setq *ans
	   ($poisplus *ans
		      ($poisplus ($poistimes (list '(mpois simp)
						   nil
						   (poismergec *coef *argc nil))
					     dc)
				 ($poistimes (list '(mpois simp)
						   (poismerges (poisco* -1 *coef) *argc nil)
						   nil)
					     ds))))
     end  (fancypac (cddr c))))

(defun fancypas (c)
  (prog nil
     (cond ((null c) (return nil)))
     (setq *coef (poisxcoef (car c) b*))
     (cond ((equal *coef 0.)
	    (setq *ans ($poisplus *ans (list '(mpois simp) (list (car c) (cadr c)) nil)))
	    (go end)))
     (cond ((poispzero (setq *coef (poisco* (cadr c) (intopoisco *coef)))) (go end)))
     (setq *argc (argsubst (car c)))
     (setq *ans ($poisplus *ans
			   ($poisplus ($poistimes (list '(mpois simp)
							nil
							(poismergec *coef *argc nil))
						  ds)
				      ($poistimes (list '(mpois simp)
							(poismerges *coef *argc nil)
							nil)
						  dc))))
     end  (fancypas (cddr c))))
