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

(macsyma-module asum)

(load-macsyma-macros rzmac)

(declare-top (special opers *a *n $factlim sum msump *i *opers-list opers-list $ratsimpexpons makef $factorial_expand))

(loop for (x y) on '(%cot %tan %csc %sin %sec %cos %coth %tanh %csch %sinh %sech %cosh)
   by #'cddr do (putprop x y 'recip) (putprop y x 'recip))

(defmvar $zeta%pi t)

;; polynomial predicates and other such things

(defun poly? (exp var)
  (cond ((or (atom exp) (free exp var)))
	((member (caar exp) '(mtimes mplus) :test #'eq)
	 (do ((exp (cdr exp) (cdr exp)))
	     ((null exp) t)
	   (and (null (poly? (car exp) var)) (return nil))))
	((and (eq (caar exp) 'mexpt)
	      (integerp (caddr exp))
	      (> (caddr exp) 0))
	 (poly? (cadr exp) var))))

(defun smono (x var)
  (smonogen x var t))

(defun smonop (x var)
  (smonogen x var nil))

(defun smonogen (x var fl)	; fl indicates whether to return *a *n
  (cond ((free x var) (and fl (setq *n 0 *a x)) t)
	((atom x) (and fl (setq *n (setq *a 1))) t)
	((and (listp (car x))
	      (eq (caar x) 'mtimes))
	 (do ((x (cdr x) (cdr x))
	      (a '(1)) (n '(0)))
	     ((null x)
	      (and fl (setq *n (addn n nil) *a (muln a nil))) t)
	   (let (*a *n)
	     (if (smonogen (car x) var fl)
		 (and fl (setq a (cons *a a) n (cons *n n)))
		 (return nil)))))
	((and (listp (car x))
	      (eq (caar x) 'mexpt))
	 (cond ((and (free (caddr x) var) (eq (cadr x) var))
		(and fl (setq *n (caddr x) *a 1)) t)))))

;; factorial stuff

(setq $factlim 100000 ; set to a big integer which will work (not -1)
      makef nil)

(defmfun $genfact (&rest l)
  (cons '(%genfact) l))

(defun gfact (n %m i)
  (cond ((minusp %m) (improper-arg-err %m '$genfact))
	((= %m 0) 1)
	(t (prog (ans)
	      (setq ans n)
	      a (if (= %m 1) (return ans))
	      (setq n (m- n i) %m (1- %m) ans (m* ans n))
	      (go a)))))

;; From Richard Fateman's paper, "Comments on Factorial Programs",
;; http://www.cs.berkeley.edu/~fateman/papers/factorial.pdf
;;
;; k(n,m) = n*(n-m)*(n-2*m)*...
;;
;; (k n 1) is n!
;;
;; This is much faster (3-4 times) than the original factorial
;; function.

(defun k (n m) 
  (if (<= n m)
      n
      (* (k n (* 2 m))
	 (k (- n m) (* 2 m)))))

(defun factorial (n)
  (if (zerop n)
      1
      (k n 1)))

;;; Factorial has mirror symmetry

(defprop mfactorial t commutes-with-conjugate)

(defmfun simpfact (x y z)
  (oneargcheck x)
  (setq y (simpcheck (cadr x) z))
  (cond ((and (mnump y)
              (eq ($sign y) '$neg)
              (zerop1 (sub (simplify (list '(%truncate) y)) y)))
         ;; Negative integer or a real representation of a negative integer.
         (merror (intl:gettext "factorial: factorial of negative integer ~:M not defined.") y))
        ((or (floatp y)             
             ($bfloatp y)
             (and (not (integerp y))
                  (not (ratnump y))
                  (or (and (complex-number-p y 'float-or-rational-p)
                           (or $numer 
                               (floatp ($realpart y)) 
                               (floatp ($imagpart y))))
                      (and (complex-number-p y 'bigfloat-or-number-p)
                           (or $numer
                               ($bfloatp ($realpart y))
                               ($bfloatp ($imagpart y))))))
             (and (not makef) (ratnump y) (equal (caddr y) 2)))
         ;; Numerically evaluate for real or complex argument in float or
         ;; bigfloat precision using the Gamma function
	 (simplify (list '(%gamma) (add 1 y))))
        ((eq y '$inf) '$inf)
        ((and $factorial_expand
              (mplusp y)
              (integerp (cadr y)))
         ;; factorial(n+m) and m integer. Expand.
         (let ((m (cadr y))
               (n (simplify (cons '(mplus) (cddr y)))))
           (cond ((>= m 0)
                  (mul 
                    (simplify (list '($pochhammer) (add n 1) m))
                    (simplify (list '(mfactorial) n))))
                 ((< m 0)
                  (setq m (- m))
                  (div
                    (mul (power -1 m) (simplify (list '(mfactorial) n)))
                    ;; We factor to get the ordering (n-1)*(n-2)*...
                    ($factor
                      (simplify (list '($pochhammer) (mul -1 n) m))))))))
	((or (not (fixnump y)) (not (> y -1)))
	 (eqtest (list '(mfactorial) y) x))
	((or (minusp $factlim) (not (> y $factlim)))
	 (factorial y))
	(t (eqtest (list '(mfactorial) y) x))))

(defun makegamma1 (e)
  (cond ((atom e) e)
	((eq (caar e) 'mfactorial)
	 (list '(%gamma) (list '(mplus) 1 (makegamma1 (cadr e)))))

	;; Begin code copied from orthopoly/orthopoly-init.lisp
	;; Do pochhammer(x,n) ==> gamma(x+n)/gamma(x).

	((eq (caar e) '$pochhammer)
	 (let ((x (makegamma1 (nth 1 e)))
	       (n (makegamma1 (nth 2 e))))
	   (div (take '(%gamma) (add x n)) (take '(%gamma) x))))
	 
	;; (gamma(x/z+1)*z^floor(y))/gamma(x/z-floor(y)+1)

	((eq (caar e) '%genfact)
	 (let ((x (makegamma1 (nth 1 e)))
	       (y (makegamma1 (nth 2 e)))
	       (z (makegamma1 (nth 3 e))))
	   (setq y (take '($floor) y))
	   (div 
	    (mul
	     (take '(%gamma) (add (div x z) 1))
	     (power z y))
	    (take '(%gamma) (sub (add (div x z) 1) y)))))
	;; End code copied from orthopoly/orthopoly-init.lisp

        ;; Double factorial

        ((eq (caar e) '%factorial_double)
         (let ((x (makegamma1 (nth 1 e))))
           (mul
             (power
               (div 2 '$%pi)
               (mul
                 (div 1 4)
                 (sub 1 (simplify (list '(%cos) (mul '$%pi x))))))
             (power 2 (div x 2))
             (simplify (list '(%gamma) (add 1 (div x 2)))))))

	((eq (caar e) '%elliptic_kc)
	 ;; Complete elliptic integral of the first kind
	 (cond ((alike1 (cadr e) '((rat simp) 1 2))
		;; K(1/2) = gamma(1/4)/4/sqrt(pi)
		'((mtimes simp) ((rat simp) 1 4)
		  ((mexpt simp) $%pi ((rat simp) -1 2))
		  ((mexpt simp) ((%gamma simp) ((rat simp) 1 4)) 2)))
	       ((or (alike1 (cadr e)
			    '((mtimes simp) ((rat simp) 1 4)
			      ((mplus simp) 2
			       ((mexpt simp) 3 ((rat simp) 1 2)))))
		    (alike1 (cadr e)
			    '((mplus simp) ((rat simp) 1 2)
			      ((mtimes simp) ((rat simp) 1 4)
			       ((mexpt simp) 3 ((rat simp) 1 2)))))
		    (alike1 (cadr e)
			    ;; 1/(8-4*sqrt(3))
			    '((mexpt simp)
			      ((mplus simp) 8
			       ((mtimes simp) -4
				((mexpt simp) 3 ((rat simp) 1 2))))
			      -1)))
		;; K((2+sqrt(3)/4))
		'((mtimes simp) ((rat simp) 1 4)
		  ((mexpt simp) 3 ((rat simp) 1 4))
		  ((mexpt simp) $%pi ((rat simp) -1 2))
		  ((%gamma simp) ((rat simp) 1 6))
		  ((%gamma simp) ((rat simp) 1 3))))
	       ((or (alike1 (cadr e)
			    ;; (2-sqrt(3))/4
			    '((mtimes simp) ((rat simp) 1 4)
			      ((mplus simp) 2
			       ((mtimes simp) -1
				((mexpt simp) 3 ((rat simp) 1 2))))))
		    (alike1 (cadr e)
			    ;; 1/2-sqrt(3)/4
			    '((mplus simp) ((rat simp) 1 2)
			      ((mtimes simp) ((rat simp) -1 4)
			       ((mexpt simp) 3 ((rat simp) 1 2)))))
		    (alike (cadr e)
			   ;; 1/(4*sqrt(3)+8)
			   '((mexpt simp)
			     ((mplus simp) 8
			      ((mtimes simp) 4
			       ((mexpt simp) 3 ((rat simp) 1 2))))
			     -1)))
		;; K((2-sqrt(3))/4)
		'((mtimes simp) ((rat simp) 1 4)
		  ((mexpt simp) 3 ((rat simp) -1 4))
		  ((mexpt simp) $%pi ((rat simp) -1 2))
		  ((%gamma simp) ((rat simp) 1 6))
		  ((%gamma simp) ((rat simp) 1 3))))
	       ((or
		 ;; (3-2*sqrt(2))/(3+2*sqrt(2))
		 (alike1 (cadr e)
			 '((mtimes simp)
			   ((mplus simp) 3 
			    ((mtimes simp) -2 
			     ((mexpt simp) 2 ((rat simp) 1 2))))
			   ((mexpt simp)
			    ((mplus simp) 3 
			     ((mtimes simp) 2
			      ((mexpt simp) 2 ((rat simp) 1 2)))) -1)))
		 ;; 17 - 12*sqrt(2)
		 (alike1 (cadr e)
			 '((mplus simp) 17 
			   ((mtimes simp) -12
			    ((mexpt simp) 2 ((rat simp) 1 2)))))
		 ;;   (2*SQRT(2) - 3)/(2*SQRT(2) + 3)
		 (alike1 (cadr e)
			 '((mtimes simp) -1
			   ((mplus simp) -3
			    ((mtimes simp) 2 
			     ((mexpt simp) 2 ((rat simp) 1 2))))
			   ((mexpt simp)
			    ((mplus simp) 3
			     ((mtimes simp) 2
			      ((mexpt simp) 2 ((rat simp) 1 2))))
			    -1))))
		'((mtimes simp) ((rat simp) 1 8)
		  ((mexpt simp) 2 ((rat simp) -1 2))
		  ((mplus simp) 1 ((mexpt simp) 2 ((rat simp) 1 2)))
		  ((mexpt simp) $%pi ((rat simp) -1 2))
		  ((mexpt simp) ((%gamma simp) ((rat simp) 1 4)) 2)))
	       (t
		;; Give up
		e)))
	((eq (caar e) '%elliptic_ec)
	 ;; Complete elliptic integral of the second kind
	 (cond ((alike1 (cadr e) '((rat simp) 1 2))
		;; 2*E(1/2) - K(1/2) = 2*%pi^(3/2)*gamma(1/4)^(-2)
		'((mplus simp)
		  ((mtimes simp) ((mexpt simp) $%pi ((rat simp) 3 2))
		   ((mexpt simp) 
		    ((%gamma simp irreducible) ((rat simp) 1 4)) -2))
		  ((mtimes simp) ((rat simp) 1 8)
		   ((mexpt simp) $%pi ((rat simp) -1 2))
		   ((mexpt simp) ((%gamma simp) ((rat simp) 1 4)) 2))))
	       ((or (alike1 (cadr e)
			    '((mtimes simp) ((rat simp) 1 4)
			      ((mplus simp) 2
			       ((mtimes simp) -1
				((mexpt simp) 3 ((rat simp) 1 2))))))
		    (alike1 (cadr e)
			    '((mplus simp) ((rat simp) 1 2)
			      ((mtimes simp) ((rat simp) -1 4)
			       ((mexpt simp) 3 ((rat simp) 1 2))))))
		;; E((2-sqrt(3))/4)
		;;
		;; %pi/4/sqrt(3) = K*(E-(sqrt(3)+1)/2/sqrt(3)*K)
		'((mplus simp)
		  ((mtimes simp) ((mexpt simp) 3 ((rat simp) -1 4))
		   ((mexpt simp) $%pi ((rat simp) 3 2))
		   ((mexpt simp) ((%gamma simp) ((rat simp) 1 6)) -1)
		   ((mexpt simp) ((%gamma simp) ((rat simp) 1 3)) -1))
		  ((mtimes simp) ((rat simp) 1 8)
		   ((mexpt simp) 3 ((rat simp) -3 4))
		   ((mexpt simp) $%pi ((rat simp) -1 2))
		   ((%gamma simp) ((rat simp) 1 6))
		   ((%gamma simp) ((rat simp) 1 3)))
		  ((mtimes simp) ((rat simp) 1 8)
		   ((mexpt simp) 3 ((rat simp) -1 4))
		   ((mexpt simp) $%pi ((rat simp) -1 2))
		   ((%gamma simp) ((rat simp) 1 6))
		   ((%gamma simp) ((rat simp) 1 3)))))
	       ((or (alike1 (cadr e)
			    '((mtimes simp) ((rat simp) 1 4)
			      ((mplus simp) 2
			       ((mexpt simp) 3 ((rat simp) 1 2)))))
		    (alike1 (cadr e)
			    '((mplus simp) ((rat simp) 1 2)
			      ((mtimes simp) ((rat simp) 1 4)
			       ((mexpt simp) 3 ((rat simp) 1 2))))))
		;; E((2+sqrt(3))/4)
		;;
		;; %pi*sqrt(3)/4 = K1*(E1-(sqrt(3)-1)/2/sqrt(3)*K1)
		'((mplus simp)
		  ((mtimes simp) 3 ((mexpt simp) 3 ((rat simp) -3 4))
		   ((mexpt simp) $%pi ((rat simp) 3 2))
		   ((mexpt simp) ((%gamma simp) ((rat simp) 1 6)) -1)
		   ((mexpt simp) ((%gamma simp) ((rat simp) 1 3)) -1))
		  ((mtimes simp) ((rat simp) 3 8)
		   ((mexpt simp) 3 ((rat simp) -3 4))
		   ((mexpt simp) $%pi ((rat simp) -1 2))
		   ((%gamma simp) ((rat simp) 1 6))
		   ((%gamma simp) ((rat simp) 1 3)))
		  ((mtimes simp) ((rat simp) -1 8)
		   ((mexpt simp) 3 ((rat simp) -1 4))
		   ((mexpt simp) $%pi ((rat simp) -1 2))
		   ((%gamma simp) ((rat simp) 1 6))
		   ((%gamma simp) ((rat simp) 1 3)))))
	       (t
		e)))
	(t (recur-apply #'makegamma1 e))))

(defmfun simpgfact (x vestigial z)
  (declare (ignore vestigial))
  (if (not (= (length x) 4)) (wna-err '$genfact))
  (setq z (mapcar #'(lambda (q) (simpcheck q z)) (cdr x)))
  (let ((a (car z)) (b (take '($floor) (cadr z))) (c (caddr z)))
    (cond ((and (fixnump a)
                (fixnump b)
                (fixnump c))
           (if (and (> a -1)
                    (> b -1) 
                    (or (<= c a) (= b 0))
                    (<= b (/ a c)))
             (gfact a b c)
             (merror (intl:gettext "genfact: generalized factorial not defined for given arguments."))))
	  (t (eqtest (list '(%genfact) a
			   (if (and (not (atom b))
				    (eq (caar b) '$floor))
			       (cadr b)
			     b)
			   c)
		     x)))))

;; sum begins

(defmvar $cauchysum nil
  "When multiplying together sums with INF as their upper limit, 
causes the Cauchy product to be used rather than the usual product.
In the Cauchy product the index of the inner summation is a function of 
the index of the outer one rather than varying independently."
  modified-commands '$sum)

(defmvar $gensumnum 0
  "The numeric suffix used to generate the next variable of
summation.  If it is set to FALSE then the index will consist only of
GENINDEX with no numeric suffix."
  modified-commands '$sum
  setting-predicate #'(lambda (x) (or (null x) (integerp x))))

(defmvar $genindex '$i
  "The alphabetic prefix used to generate the next variable of
summation when necessary."
  modified-commands '$sum
  setting-predicate #'symbolp)

(defmvar $zerobern t)
(defmvar $simpsum nil)
(defmvar $simpproduct nil)

(defvar *infsumsimp t)

;; These variables should be initialized where they belong.

(setq $wtlevel nil $cflength 1
      $weightlevels '((mlist)) *trunclist nil $taylordepth 3
      $maxtaydiff 4 $verbose nil $psexpand nil ps-bmt-disrep t
      silent-taylor-flag nil)

(defmacro sum-arg (sum)
  `(cadr ,sum))

(defmacro sum-index (sum)
  `(caddr ,sum))

(defmacro sum-lower (sum)
  `(cadddr ,sum))

(defmacro sum-upper (sum)
  `(cadr (cdddr ,sum)))

(defmspec $sum (l)
  (setq l (cdr l))
  (if (= (length l) 4)
      (dosum (car l) (cadr l) (meval (caddr l)) (meval (cadddr l)) t :evaluate-summand t)
      (wna-err '$sum)))

(defmspec $lsum (l)
  (setq l (cdr l))
  (or (= (length l) 3) (wna-err '$lsum))
  (let ((form (car l))
	(ind (cadr l))
	(lis (meval (caddr l)))
	(ans 0))
    (or (symbolp ind) (merror (intl:gettext "lsum: second argument must be a variable; found ~M") ind))
    (cond (($listp lis)
	   (loop for v in (cdr lis)
	      with lind = (cons ind nil)
	      for w = (cons v nil)
	      do
	      (setq ans (add* ans  (mbinding (lind w) (meval form)))))
	   ans)
	  (t `((%lsum) ,form ,ind ,lis)))))
    
(defmfun simpsum (x y z)
  (let (($ratsimpexpons t))
    (setq y (simplifya (sum-arg x) z)))
  (simpsum1 y (sum-index x) (simplifya (sum-lower x) z)
	    (simplifya (sum-upper x) z)))

; This function was SIMPSUM1 until the sum/product code was revised Nov 2005.
; The revised code punts back to this function since this code knows
; some simplifications not handled by the revised code. -- Robert Dodier

(defun simpsum1-save (exp i lo hi)
  (cond ((not (symbolp i)) (merror (intl:gettext "sum: index must be a symbol; found ~M") i))
	((equal lo hi) (mbinding ((list i) (list hi)) (meval exp)))
	((and (atom exp)
	      (not (eq exp i))
	      (getl '%sum '($outative $linear)))
	 (freesum exp lo hi 1))
	((null $simpsum) (list (get '%sum 'msimpind) exp i lo hi))
	((and (or (eq lo '$minf)
		  (alike1 lo '((mtimes simp) -1 $inf)))
	      (equal hi '$inf))
	 (let ((pos-part (simpsum2 exp i 0 '$inf))
	       (neg-part (simpsum2 (maxima-substitute (m- i) i exp) i 1 '$inf)))
	   (cond
	     ((or (eq neg-part '$und)
		  (eq pos-part '$und))
	      '$und)
	     ((eq pos-part '$inf)
	      (if (eq neg-part '$minf) '$und '$inf))
	     ((eq pos-part '$minf)
	      (if (eq neg-part '$inf) '$und '$minf))
	     ((or (eq neg-part '$inf) (eq neg-part '$minf))
	      neg-part)
	     (t (m+ neg-part pos-part)))))
	((or (eq lo '$minf)
	     (alike1 lo '((mtimes simp) -1 '$inf)))
	 (simpsum2 (maxima-substitute (m- i) i exp) i (m- hi) '$inf))
	(t (simpsum2 exp i lo hi))))

;; DOSUM, MEVALSUMARG, DO%SUM -- general principles

;;  - evaluate the summand/productand
;;  - substitute a gensym for the index variable and make assertions (via assume) about the gensym index
;;  - return 0/1 for empty sum/product. sumhack/prodhack are ignored
;;  - distribute sum/product over mbags when listarith = true

(defun dosum (expr ind low hi sump &key (evaluate-summand t))
  (setq low (ratdisrep low) hi (ratdisrep hi)) ;; UGH, GAG WITH ME A SPOON
  (if (not (symbolp ind))
      (merror (intl:gettext "~:M: index must be a symbol; found ~M") (if sump '$sum '$product) ind))
  (unwind-protect
       (prog (u *i lind l*i *hl)
	  (setq lind (cons ind nil))
	  (cond
	    ((not (fixnump (setq *hl (mfuncall '$floor (m- hi low)))))
	     (if evaluate-summand (setq expr (mevalsumarg expr ind low hi)))
	     (return (cons (if sump '(%sum) '(%product))
			   (list expr ind low hi))))
	    ((signp l *hl)
	     (return (if sump 0 1))))
	  (setq *i low l*i (list *i) u (if sump 0 1))
	  lo (setq u
		   (if sump
		       (add u (resimplify (let* ((foo (mbinding (lind l*i) (meval expr)))
						 (bar (subst-if-not-freeof *i ind foo)))
					    bar)))
		       (mul u (resimplify (let* ((foo (mbinding (lind l*i) (meval expr)))
						 (bar (subst-if-not-freeof *i ind foo)))
					    bar)))))
	  (when (zerop *hl) (return u))
	  (setq *hl (1- *hl))
	  (setq *i (car (rplaca l*i (m+ *i 1))))
	  (go lo))))

(defun subst-if-not-freeof (x y expr)
  (if ($freeof y expr)
      expr
      (if (atom expr)
	  x
	  (let* ((args (cdr expr))
		 (L (eval `(mapcar (lambda (a) (subst-if-not-freeof ',x ',y a)) ',args))))
	    (cons (car expr) L)))))

(defun mevalsumarg (expr ind low hi)
  (if (let (($prederror nil))
	(eq (mevalp `((mlessp) ,hi ,low)) t))
      0)

  (let ((gensym-ind (gensym)))
    (if (apparently-integer low) 
	(meval `(($declare) ,gensym-ind $integer)))
    (assume (list '(mgeqp) gensym-ind low))
    (if (not (eq hi '$inf))
	(assume (list '(mgeqp) hi gensym-ind)))
    (let ((msump t) (foo) (summand))
      (setq summand
            (if (and (not (atom expr)) (get (caar expr) 'mevalsumarg-macro))
		(funcall (get (caar expr) 'mevalsumarg-macro) expr)
		expr))
      (let (($simp nil))
        (setq summand ($substitute gensym-ind ind summand)))
      (setq foo (mbinding ((list gensym-ind) (list gensym-ind))
                          (resimplify (meval summand))))
      ;; At this point we do not switch off simplification to preserve
      ;; the achieved simplification of the summand (DK 02/2010).
      (let (($simp t))
	(setq foo ($substitute ind gensym-ind foo)))
      (if (not (eq hi '$inf))
	  (forget (list '(mgeqp) hi gensym-ind)))
      (forget (list '(mgeqp) gensym-ind low))
      (if (apparently-integer low)
	  (meval `(($remove) ,gensym-ind $integer)))
      foo)))

(defun apparently-integer (x)
  (or ($integerp x) ($featurep x '$integer)))

(defun do%sum (l op)
  (if (not (= (length l) 4)) (wna-err op))
  (let ((ind (cadr l)))
    (if (mquotep ind) (setq ind (cadr ind)))
    (if (not (symbolp ind))
      (merror (intl:gettext "~:M: index must be a symbol; found ~M") op ind))
    (let ((low (caddr l))
	  (hi (cadddr l)))
      (list (mevalsumarg (car l) ind low hi)
            ind (meval (caddr l)) (meval (cadddr l))))))

(defun simpsum1 (e k lo hi)
  (let ((fact1) (fact2) (acc 0) (n) (sgn) ($prederror nil) (i (gensym)) (ex))
    (setq lo ($ratdisrep lo))
    (setq hi ($ratdisrep hi))
   
    (setq n ($limit (add 1 (sub hi lo))))
    (setq sgn ($sign n))
  
    (setq fact1 `((mgeqp) ,i ,lo))
    (setq fact2 `((mgeqp) ,hi ,i))
    
    (if (not (eq t (csign lo))) (mfuncall '$assume fact1))
    (if (not (eq t (csign hi))) (mfuncall '$assume fact2))
    
    (setq ex (subst i k e))
    (setq ex (subst i k ex))

    (setq acc
	  (cond ((and (eq n '$inf) ($freeof i ex))
		 (setq sgn (csign ex))
		 (cond ((eq sgn '$pos) '$inf)
		       ((eq sgn '$neg) '$minf)
		       ((eq sgn '$zero) 0)
		       (t `((%sum simp) ,ex ,i ,lo ,hi))))

		((and (mbagp e) $listarith)
		 (simplifya
		  `((,(caar e)) ,@(mapcar #'(lambda (s) (mfuncall '$sum s k lo hi)) (margs e))) t))
		 		
		((or (eq sgn '$neg) (eq sgn '$zero) (eq sgn '$nz)) 0)
		
		((like ex 0) 0)
			
		(($freeof i ex) (mult n ex))
			
		((and (integerp n) (eq sgn '$pos) $simpsum)
		 (unwind-protect 
		      (dotimes (j n acc)
			(setq acc (add acc (resimplify (subst (add j lo) i ex)))))
		   (mfuncall '$forget fact1)
		   (mfuncall '$forget fact2)))
		(t 
		 (setq ex (subst '%sum '$sum ex))
		 `((%sum simp) ,(subst k i ex) ,k ,lo ,hi))))
	      
    (setq acc (subst k i acc))
    
    ;; If expression is still a summation,
    ;; punt to previous simplification code.

    (if (and $simpsum (op-equalp acc '$sum '%sum))
	(let* ((args (cdr acc)) (e (first args)) (i (second args)) (i0 (third args)) (i1 (fourth args)))
	  (setq acc (simpsum1-save e i i0 i1))))

    (mfuncall '$forget fact1)
    (mfuncall '$forget fact2)

    acc))

(defun simpprod1 (e k lo hi)
  (let ((fact1) (fact2) (acc 1) (n) (sgn) ($prederror nil) (i (gensym)) (ex) (ex-mag) (realp))

    (setq lo ($ratdisrep lo))
    (setq hi ($ratdisrep hi))
    (setq n ($limit (add 1 (sub hi lo))))
    (setq sgn ($sign n))
    (setq fact1 `((mgeqp) ,i ,lo))
    (setq fact2 `((mgeqp) ,hi ,i))
    
    (if (not (eq t (csign lo))) (mfuncall '$assume fact1))
    (if (not (eq t (csign hi))) (mfuncall '$assume fact2))

    (setq ex (subst i k e))
    (setq ex (subst i k ex))

    (setq acc
          (cond
            ((like ex 1) 1)

            ((and (eq n '$inf) ($freeof i ex))
             (setq ex-mag (mfuncall '$cabs ex))
             (setq realp (mfuncall '$imagpart ex))
             (setq realp (mevalp `((mequal) 0 ,realp)))
             
             (cond ((eq t (mevalp `((mlessp) ,ex-mag 1))) 0)
                   ((and (eq realp t) (eq t (mevalp `((mgreaterp) ,ex 1)))) '$inf)
                   ((eq t (mevalp `((mgreaterp) ,ex-mag 1))) '$infinity)
                   ((eq t (mevalp `((mequal) 1 ,ex-mag))) '$und)
                   (t `((%product) ,e ,k ,lo ,hi))))
            
            ((or (eq sgn '$neg) (eq sgn '$zero) (eq sgn '$nz))
             1)

            ((and (mbagp e) $listarith)
             (simplifya
	      `((,(caar e)) ,@(mapcar #'(lambda (s) (mfuncall '$product s k lo hi)) (margs e))) t))
            
            (($freeof i ex) (power ex n))
            
            ((and (integerp n) (eq sgn '$pos) $simpproduct)
             (unwind-protect
		  (dotimes (j n acc)
		    (setq acc (mult acc (resimplify (subst (add j lo) i ex)))))
               
               (mfuncall '$forget fact1)
               (mfuncall '$forget fact2)))
            
            (t
	     (setq ex (subst '%product '$product ex))
	     `((%product simp) ,(subst k i ex) ,k ,lo ,hi))))

    ;; Hmm, this is curious... don't call existing product simplifications
    ;; if index range is infinite -- what's up with that??

    (if (and $simpproduct (op-equalp acc '$product '%product) (not (like n '$inf)))
	(let* ((args (cdr acc)) (e (first args)) (i (second args)) (i0 (third args)) (i1 (fourth args)))
	  (setq acc (simpprod1-save e i i0 i1))))

    (setq acc (subst k i acc))
    (setq acc (subst '%product '$product acc))

    (mfuncall '$forget fact1)
    (mfuncall '$forget fact2)

    acc))

; This function was SIMPPROD1 until the sum/product code was revised Nov 2005.
; The revised code punts back to this function since this code knows
; some simplifications not handled by the revised code. -- Robert Dodier

(defun simpprod1-save (exp i lo hi)
  (let (u)
    (cond ((not (symbolp i)) (merror (intl:gettext "product: index must be a symbol; found ~M") i))
	  ((alike1 lo hi)
	   (let ((valist (list i)))
	     (mbinding (valist (list hi))
		       (meval exp))))
	  ((eq ($sign (setq u (m- hi lo))) '$neg)
	   (cond ((eq ($sign (add2 u 1)) '$zero) 1)
		 (t (merror (intl:gettext "product: lower bound ~M greater than upper bound ~M") lo hi))))
	  ((atom exp)
	   (cond ((null (eq exp i))
		  (power* exp (list '(mplus) hi 1 (list '(mtimes) -1 lo))))
		 ((let ((lot (asksign lo)))
		    (cond ((equal lot '$zero) 0)
			  ((eq lot '$positive)
			   (m// (list '(mfactorial) hi)
				(list '(mfactorial) (list '(mplus) lo -1))))
			  ((m* (list '(mfactorial)
				     (list '(mabs) lo))
			       (cond ((member (asksign hi) '($zero $positive) :test #'eq)
				      0)
				     (t (prog2 0
					    (m^ -1 (m+ hi lo 1))
					  (setq hi (list '(mabs) hi)))))
			       (list '(mfactorial) hi))))))))
	  ((list '(%product simp) exp i lo hi)))))


;; multiplication of sums

(defun gensumindex ()
  (intern (format nil "~S~D" $genindex (incf $gensumnum))))

(defun sumtimes (x y)
  (cond ((null x) y)
	((null y) x)
	((or (safe-zerop  x) (safe-zerop y)) 0)
	((or (atom x) (not (eq (caar x) '%sum))) (sumultin x y))
	((or (atom y) (not (eq (caar y) '%sum))) (sumultin y x))
	(t (let (u v i j)
	     (if (great (sum-arg x) (sum-arg y)) (setq u y v x) (setq u x v y))
	     (setq i (let ((ind (gensumindex)))
		       (setq u (subst ind (sum-index u) u)) ind))
	     (setq j (let ((ind (gensumindex)))
		       (setq v (subst ind (sum-index v) v)) ind))
	     (if (and $cauchysum (eq (sum-upper u) '$inf)
		      (eq (sum-upper v) '$inf))
		 (list '(%sum)
		       (list '(%sum)
			     (sumtimes (maxima-substitute j i (sum-arg u))
				       (maxima-substitute (m- i j) j (sum-arg v)))
			     j (sum-lower u) (m- i (sum-lower v)))
		       i (m+ (sum-lower u) (sum-lower v)) '$inf)
		 (list '(%sum)
		       (list '(%sum) (sumtimes (sum-arg u) (sum-arg v))
			     j (sum-lower v) (sum-upper v))
		       i (sum-lower u) (sum-upper u)))))))

(defun sumultin (x s)	  ; Multiplies x into a sum adjusting indices.
  (cond ((or (atom s) (not (eq (caar s) '%sum))) (m* x s))
	((free x (sum-index s))
	 (list* (car s) (sumultin x (sum-arg s)) (cddr s)))
	(t (let ((ind (gensumindex)))
	     (list* (car s)
		    (sumultin x (subst ind (sum-index s) (sum-arg s)))
		    ind
		    (cdddr s))))))

;; addition of sums

(defun sumpls (sum out)
  (prog (l)
     (if (null out) (return (cons sum nil)))
     (setq out (setq l (cons nil out)))
     a 	(if (null (cdr out)) (return (cons sum (cdr l))))
     (and (not (atom (cadr out)))
	  (consp (caadr out))
	  (eq (caar (cadr out)) '%sum)
	  (alike1 (sum-arg (cadr out)) (sum-arg sum))
	  (alike1 (sum-index (cadr out)) (sum-index sum))
	  (cond ((onediff (sum-upper (cadr out)) (sum-lower sum))
		 (setq sum (list (car sum)
				 (sum-arg sum)
				 (sum-index sum)
				 (sum-lower (cadr out))
				 (sum-upper sum)))
		 (rplacd out (cddr out))
		 (go a))
		((onediff (sum-upper sum) (sum-lower (cadr out)))
		 (setq sum (list (car sum)
				 (sum-arg sum)
				 (sum-index sum)
				 (sum-lower sum)
				 (sum-upper (cadr out))))
		 (rplacd out (cddr out))
		 (go a))))
     (setq out (cdr out))
     (go a)))

(defun onediff (x y)
  (equal 1 (m- y x)))

(defun freesum (e b a q)
  (m* e q (m- (m+ a 1) b)))

;; linear operator stuff

(defparameter *opers-list '(($linear . linearize1)))
(defparameter  opers (list '$linear))

(defun oper-apply (e z)
  (cond ((null opers-list)
	 (let ((w (get (caar e) 'operators)))
	   (if w (funcall w e 1 z) (simpargs e z))))
	((get (caar e) (caar opers-list))
	 (let ((opers-list (cdr opers-list))
	       (fun (cdar opers-list)))
	   (funcall fun e z)))
	(t (let ((opers-list (cdr opers-list)))
	     (oper-apply e z)))))

(defun linearize1 (e z)		; z = t means args already simplified.
  (linearize2 (cons (car e) (mapcar #'(lambda (q) (simpcheck q z)) (cdr e)))
	      nil))

(defun opident (op)
  (cond ((eq op 'mplus) 0)
	((eq op 'mtimes) 1)))

(defun rem-const (e)			;removes constantp stuff
  (do ((l (cdr e) (cdr l))
       (a (list (opident (caar e))))
       (b (list (opident (caar e)))))
      ((null l)
       (cons (simplifya (cons (list (caar e)) a) nil)
	     (simplifya (cons (list (caar e)) b) nil)))
    (if ($constantp (car l))
	(setq a (cons (car l) a))
	(setq b (cons (car l) b)))))

(defun linearize2 (e times)
  (cond ((linearconst e))
	((atom (cadr e)) (oper-apply e t))
	((eq (caar (cadr e)) 'mplus)
	 (addn (mapcar #'(lambda (q)
			   (linearize2 (list* (car e) q (cddr e)) nil))
		       (cdr (cadr e)))
	       t))
	((and (eq (caar (cadr e)) 'mtimes) (null times))
	 (let ((z (if (and (cddr e)
			   (or (atom (caddr e))
			       ($subvarp (caddr e))))
		      (partition (cadr e) (caddr e) 1)
		      (rem-const (cadr e))))
	       (w))
	   (setq w (linearize2 (list* (car e)
				      (simplifya (cdr z) t)
				      (cddr e))
			       t))
	   (linearize3 w e (car z))))
	(t (oper-apply e t))))

(defun linearconst (e)
  (if (or (mnump (cadr e))
	  (constant (cadr e))
	  (and (cddr e)
	       (or (atom (caddr e)) (member 'array (cdar (caddr e)) :test #'eq))
	       (free (cadr e) (caddr e))))
      (if (or (zerop1 (cadr e))
	      (and (member (caar e) '(%sum %integrate) :test #'eq)
		   (= (length e) 5)
		   (or (eq (cadddr e) '$minf)
		       (member (car (cddddr e)) '($inf $infinity) :test #'eq))
		   (eq ($asksign (cadr e)) '$zero)))
	  0
	  (let ((w (oper-apply (list* (car e) 1 (cddr e)) t)))
	    (linearize3 w e (cadr e))))))

(defun linearize3 (w e x)
  (let (w1)
    (if (and (member w '($inf $minf $infinity) :test #'eq) (safe-zerop x))
	(merror (intl:gettext "LINEARIZE3: undefined form 0*inf: ~M") e))
    (setq w (mul2 (simplifya x t) w))
    (cond ((or (atom w) (getl (caar w) '($outative $linear))) (setq w1 1))
	  ((eq (caar w) 'mtimes)
	   (setq w1 (cons '(mtimes) nil))
	   (do ((w2 (cdr w) (cdr w2)))
	       ((null w2) (setq w1 (nreverse w1)))
	     (if (or (atom (car w2))
		     (not (getl (caaar w2) '($outative $linear))))
		 (setq w1 (cons (car w2) w1)))))
	  (t (setq w1 w)))
    (if (and (not (atom w1)) (or (among '$inf w1) (among '$minf w1)))
	(infsimp w)
	w)))

(setq opers (cons '$additive opers)
      *opers-list (cons '($additive . additive) *opers-list))

(defun rem-opers-p (p)
  (cond ((eq (caar opers-list) p)
	 (setq opers-list (cdr p)))
	((do ((l opers-list (cdr l)))
	     ((null l))
	   (if (eq (caar (cdr l)) p)
	       (return (rplacd l (cddr l))))))))

(defun additive (e z)
  (cond ((get (caar e) '$outative)  ; Really a linearize!
         (setq opers-list (copy-list opers-list))
         (rem-opers-p '$outative)
         (linearize1 e z))
        ((mplusp (cadr e))
         (addn (mapcar #'(lambda (q)
                           (let ((opers-list *opers-list))
                             (oper-apply (list* (car e) q (cddr e)) z)))
                       (cdr (cadr e)))
               z))
        (t (oper-apply e z))))

(setq opers (cons '$multiplicative opers)
      *opers-list (cons '($multiplicative . multiplicative) *opers-list))

(defun multiplicative (e z)
  (cond ((mtimesp (cadr e))
         (muln (mapcar #'(lambda (q)
                           (let ((opers-list *opers-list))
                             (oper-apply (list* (car e) q (cddr e)) z)))
                       (cdr (cadr e)))
               z))
        (t (oper-apply e z))))

(setq opers (cons '$outative opers)
      *opers-list (cons '($outative . outative) *opers-list))

(defun outative (e z)
  (setq e (cons (car e) (mapcar #'(lambda (q) (simpcheck q z)) (cdr e))))
  (cond ((get (caar e) '$additive)
	 (setq opers-list (copy-list opers-list ))
	 (rem-opers-p '$additive)
	 (linearize1 e t))
	((linearconst e))
	((mtimesp (cadr e))
	 (let ((u (if (and (cddr e)
			   (or (atom (caddr e))
			       ($subvarp (caddr e))))
		      (partition (cadr e) (caddr e) 1)
		      (rem-const (cadr e))))
	       (w))
	   (setq w (oper-apply (list* (car e)
				      (simplifya (cdr u) t)
				      (cddr e))
			       t))
	   (linearize3 w e (car u))))
	(t (oper-apply e t))))

(defprop %sum t $outative)
(defprop %sum t opers)
(defprop %integrate t $outative)
(defprop %integrate t opers)
(defprop %limit t $outative)
(defprop %limit t opers)

(setq opers (cons '$evenfun opers)
      *opers-list (cons '($evenfun . evenfun) *opers-list))

(setq opers (cons '$oddfun opers)
      *opers-list (cons '($oddfun . oddfun) *opers-list))

(defmfun evenfun (e z)
  (if (or (null (cdr e)) (cddr e))
      (merror (intl:gettext "Function declared 'even' takes exactly one argument; found ~M") e))
  (let ((arg (simpcheck (cadr e) z)))
    (oper-apply (list (car e) (if (mminusp arg) (neg arg) arg)) t)))

(defmfun oddfun (e z)
  (if (or (null (cdr e)) (cddr e))
      (merror (intl:gettext "Function declared 'odd' takes exactly one argument; found ~M") e))
  (let ((arg (simpcheck (cadr e) z)))
    (if (mminusp arg) (neg (oper-apply (list (car e) (neg arg)) t))
	(oper-apply (list (car e) arg) t))))

(setq opers (cons '$commutative opers)
      *opers-list (cons '($commutative . commutative1) *opers-list))

(setq opers (cons '$symmetric opers)
      *opers-list (cons '($symmetric . commutative1) *opers-list))

(defmfun commutative1 (e z)
  (oper-apply (cons (car e)
		    (reverse
		     (sort (mapcar #'(lambda (q) (simpcheck q z))
				   (cdr e))
			   'great)))
	      t))

(setq opers (cons '$antisymmetric opers)
      *opers-list (cons '($antisymmetric . antisym) *opers-list))

(declare-top (special antisym-sign))

(defmfun antisym (e z)
  (let ((l (mapcar #'(lambda (q) (simpcheck q z)) (cdr e))))
    (let (antisym-sign) (if (or (not (eq (caar e) 'mnctimes)) (freel l 'mnctimes))
		    (setq l (bbsort1 l)))
	 (cond ((equal l 0) 0)
	       ((prog1 (null antisym-sign) (setq e (oper-apply (cons (car e) l) t)))
		e)
	       (t (neg e))))))

(defun bbsort1 (l)
  (prog (sl sl1)
     (if (or (null l) (null (cdr l))) (return l))
     (setq antisym-sign nil sl (list nil (car l)))
     loop (setq l (cdr l))
     (if (null l) (return (nreverse (cdr sl))))
     (setq sl1 sl)
     loop1(cond ((null (cdr sl1)) (rplacd sl1 (cons (car l) nil)))
		((alike1 (car l) (cadr sl1)) (return 0))
		((great (car l) (cadr sl1)) (rplacd sl1 (cons (car l) (cdr sl1))))
		(t (setq antisym-sign (not antisym-sign) sl1 (cdr sl1)) (go loop1)))
     (go loop)))

(setq opers (cons '$nary opers)
      *opers-list (cons '($nary . nary1) *opers-list))

(defun nary1 (e z)
  (do
    ((l (cdr e) (cdr l)) (ans) (some-change))

    ((null l)
     (if some-change
       (nary1 (cons (car e) (nreverse ans)) z)
       (let ((w (get (caar e) 'operators)))
         (if w (funcall w e 1 z) (simpargs e z)))))

    (setq
      ans (if (and (not (atom (car l))) (eq (caaar l) (caar e)))
            (progn
              (setq some-change t)
              (nconc (reverse (cdar l)) ans))
            (cons (car l) ans)))))

(setq opers (cons '$lassociative opers)
      *opers-list (cons '($lassociative . lassociative) *opers-list))

(defmfun lassociative (e z)
  (let*
    ((ans0 (oper-apply (cons (car e) (total-nary e)) z))
     (ans (cdr ans0)))
    (cond ((or (null (cddr ans)) (not (eq (caar ans0) (caar e)))) ans0)
          ((do ((newans (list (car e) (car ans) (cadr ans))
                        (list (car e) newans (car ans)))
                (ans (cddr ans) (cdr ans)))
               ((null ans) newans))))))

(setq opers (cons '$rassociative opers)
      *opers-list (cons '($rassociative . rassociative) *opers-list))

(defmfun rassociative (e z)
  (let*
    ((ans0 (oper-apply (cons (car e) (total-nary e)) z))
     (ans (cdr ans0)))
    (cond ((or (null (cddr ans)) (not (eq (caar ans0) (caar e)))) ans0)
	  (t (setq ans (nreverse ans))
	     (do ((newans (list (car e) (cadr ans) (car ans))
			  (list (car e) (car ans) newans))
		  (ans (cddr ans) (cdr ans)))
		 ((null ans) newans))))))

(defmfun total-nary (e)
  (do ((l (cdr e) (cdr l)) (ans))
      ((null l) (nreverse ans))
    (setq ans (if (and (not (atom (car l))) (eq (caaar l) (caar e)))
		  (nconc (reverse (total-nary (car l))) ans)
		  (cons (car l) ans)))))

(defparameter $opproperties (cons '(mlist simp) (reverse opers)))
