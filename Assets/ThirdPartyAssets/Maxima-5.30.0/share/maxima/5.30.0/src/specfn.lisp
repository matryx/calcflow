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

(macsyma-module specfn)

;;*********************************************************************
;;****************                                   ******************
;;**************** Macsyma Special Function Routines ******************
;;****************                                   ******************
;;*********************************************************************

(load-macsyma-macros rzmac)
(load-macsyma-macros mhayat)

(defmacro mnumericalp (arg)
  `(or (floatp ,arg) (and (or $numer $float) (integerp ,arg))))

;; subtitle polylogarithm routines

(declare-top (special $zerobern ivars key-vars tlist %e-val))

(defun lisimp (expr vestigial z)
  (declare (ignore vestigial))
  (let ((s (simpcheck (car (subfunsubs expr)) z))
        ($zerobern t)
        (a))
    (subargcheck expr 1 1 '$li)
    (setq a (simpcheck (car (subfunargs expr)) z))
    (or (cond ((zerop1 a) a)
              ((not (integerp s)) ())
              ((= s 1)
               (if (onep1 a)
                   (simp-domain-error
                     (intl:gettext "li: li[~:M](~:M) is undefined.") s a)
                   (neg (take '(%log) (sub 1 a)))))
              ((= s 0) (div a (sub 1 a)))
              ((< s 0) (lisimp-negative-integer s a))
              ((and (integerp a) (> s 1)
                    (cond ((= a 1) (take '(%zeta) s))
                          ((= a -1)
                           (mul (add -1 (inv (expt 2 (- s 1))))
                                (take '(%zeta) s))))))
              ((= s 2) (li2simp a))
              ((= s 3) (li3simp a)))
        (eqtest (subfunmakes '$li (ncons s) (ncons a))
                expr))))

;; Expand the Polylogarithm li[s](z) for a negative integer parameter s.
(defun lisimp-negative-integer (s z)
  (let ((n (- s)))
    (mul (inv (power (sub 1 z) (+ n 1)))
         (let ((index1 (gensumindex))
               ($simpsum t))
           (dosum
             (mul (power z index1)
                  (let ((index2 (gensumindex)))
                    (dosum
                      (mul (power -1 (add index2 1))
                           (take '(%binomial) (+ n 1) (sub index2 1))
                           (power (add 1 (sub index1 index2)) n))
                      index2 1 index1 t)))
             index1 1 n t)))))

(defun li2simp (arg)
  (cond ((mnumericalp arg) (li2numer (float arg)))
        ((alike1 arg '((rat) 1 2))
         (add (div (take '(%zeta) 2) 2)
              (mul '((rat simp) -1 2)
                   (power (take '(%log) 2) 2))))))

(defun li3simp (arg)
  (cond ((mnumericalp arg) (li3numer (float arg)))
        ((alike1 arg '((rat) 1 2))
         (add (mul '((rat simp) 7 8) (take '(%zeta) 3))
              (mul (div (take '(%zeta) 2) -2) (take '(%log) 2))
              (mul '((rat simp) 1 6) (power (take '(%log) 2) 3))))))

;; exponent in first term of taylor expansion of $li is one
(defun li-ord (subl)
  (ncons (rcone)))

;; taylor expansion of $li is its definition:
;; x + x^2/2^s + x^3/3^s + ...
(defun exp$li-fun (pw subl l)	; l is a irrelevant here
  (setq subl (car subl))	; subl is subscript of li
  (prog ((e 0) 			; e is exponent of current term
	 npw)			; npw is exponent of last term needed
	(declare (fixnum e))
	(setq npw (/ (float (car pw)) (float (cdr pw))))
	(setq
	 l (cons '((0 . 1) 0 . 1)
		 nil))
	a (setq e (1+ e))
	(if (> e npw) (return l)
	  (rplacd (last l)
		  `(((,e . 1)
		     . ,(prep1 (m^ e (m- subl)))))))
	(go a)))


;; computes first pw terms of asymptotic expansion of $li[s](z)
;;
;; pw should be < (1/2)*s or gamma term is undefined
;;
;; Wood, D.C. (June 1992). The Computation of Polylogarithms. Technical Report 15-92
;; University of Kent Computing Laboratory.
;; http://www.cs.kent.ac.uk/pubs/1992/110
;; equation 11.1
(defun li-asymptotic-expansion (pw s z)
  (m+l (loop for k from 0 to pw collect
	     (m* (m^ -1 k)
		 (m- 1 (m^ 2 (m- 1 (m* 2 k))))
		 (m^ (m* 2 '$%pi) (m* 2 k))
		 (m// ($bern (m* 2 k))
		      `((mfactorial) ,(m* 2 k)))
		 (m// (m^ `((%log) ,(m- z)) (m- 2 (m* 2 k))) 
		      ($gamma (m+ s 1 (m* -2 k))))))))

;; Numerical evaluation for Chebyschev expansions of the first kind

(defun cheby (x chebarr)
  (let ((bn+2 0.0) (bn+1 0.0))
    (do ((i (floor (aref chebarr 0)) (1- i)))
	((< i 1) (- bn+1 (* bn+2 x)))
     (setq bn+2
	    (prog1 bn+1 (setq bn+1 (+ (aref chebarr i)
				      (- (* 2.0 x bn+1) bn+2))))))))

(defun cheby-prime (x chebarr)
  (- (cheby x chebarr)
      (* (aref chebarr 1) 0.5)))

;; These should really be calculated with minimax rational approximations.
;; Someone has done LI[2] already, and this should be updated; I haven't
;; seen any results for LI[3] yet.

(defun li2numer (y)
  ;; Spence's function can be used to compute li[2] for 0 <= x <= 1.
  ;; To compute the rest, we need the following identities:
  ;;
  ;; li[2](x) = -li[2](1/x)-log(-x)^2/2-%pi^2/6
  ;; li[2](x) = li[2](1/(1-x)) + log(1-x)*log((1-x)/x^2)/2 - %pi^2/6
  ;;
  ;; The first tells us how to compute li[2] for x > 1.  The result is complex.
  ;; For x < 0, the second can be used, and the result is real.
  ;;
  ;; (See http://functions.wolfram.com/ZetaFunctionsandPolylogarithms/PolyLog2/17/01/01/)
  (labels ((li2 (x)
	     (cond ((< x 0)
		    (+ (li2 (/ (- 1 x)))
		       (* 0.5 (log (- 1 x)) (log (/ (- 1 x) (* x x))))
		       (- (/ (cl:expt (float pi) 2) 6))))
		   ((< x 1)
		    (slatec:dspenc x))
		   ((= x 1)
		    (/ (cl:expt (float pi) 2) 6))
		   (t
		    ;; li[2](x) = -li[2](1/x)-log(-x)^2/2-%pi^2/6
		    (- (+ (li2 (/ x))
			  (/ (cl:expt (cl:log (- x)) 2) 2)
			  (/ (cl:expt (float pi) 2) 6)))))))
    (complexify (li2 y))))


(defun li3numer (x)
  (cond ((= x 0.0) 0.0)
	((= x 1.0) 1.20205690)
	((< x -1.0)
	 (- (chebyli3 (/ x)) (* 1.64493407 (log (- x)))
	     (/ (expt (log (- x)) 3) 6.0)))
	((not (> x 0.5)) (chebyli3 x))
	((not (> x 2.0))
	 (let ((fac (* (expt (log x) 2) 0.5)))
	   (m+t (+ 1.20205690
		    (- (* (log x)
			    (- 1.64493407 (chebyli2 (- 1.0 x))))
			(chebys12 (- 1.0 x))
			(* fac
			    (log (cond ((< x 1.0) (- 1.0 x))
				       ((1- x)))))))
		(cond ((< x 1.0) 0)
		      ((m*t (* fac -3.14159265) '$%i))))))
	(t (m+t (+ (chebyli3 (/ x)) (* 3.28986813 (log x))
		    (/ (expt (log x) 3) -6.0))
		(m*t (* -1.57079633 (expt (log x) 2)) '$%i)))))

(defvar *li2* (make-array 15. :initial-contents '(14.0 1.93506430 .166073033 2.48793229e-2
						  4.68636196e-3 1.0016275e-3 2.32002196e-4
						  5.68178227e-5 1.44963006e-5 3.81632946e-6
						  1.02990426e-6 2.83575385e-7 7.9387055e-8
						  2.2536705e-8 6.474338e-9)
			  :element-type 'flonum))


(defvar *li3* (make-array 15. :initial-contents '(14.0 1.95841721 8.51881315e-2 8.55985222e-3
						  1.21177214e-3 2.07227685e-4 3.99695869e-5
						  8.38064066e-6 1.86848945e-6 4.36660867e-7
						  1.05917334e-7 2.6478920e-8 6.787e-9
						  1.776536e-9 4.73417e-10)
			  :element-type 'flonum))

(defvar *s12* (make-array 18. :initial-contents '(17.0 1.90361778 .431311318 .100022507
						  2.44241560e-2 6.22512464e-3 1.64078831e-3
						  4.44079203e-4 1.22774942e-4 3.45398128e-5
						  9.85869565e-6 2.84856995e-6 8.31708473e-7
						  2.45039499e-7 7.2764962e-8 2.1758023e-8 6.546158e-9
						  1.980328e-9)
			  :element-type 'flonum))

(defun chebyli2 (x)
  (* x (cheby-prime (/ (1+ (* x 4)) 3) *li2*)))

(defun chebyli3 (x)
  (* x (cheby-prime (/ (1+ (* 4 x)) 3) *li3*)))

(defun chebys12 (x)
  (* (/ (expt x 2) 4)
      (cheby-prime (/ (1+ (* 4 x)) 3) *s12*)))

;; subtitle polygamma routines

;; gross efficiency hack, exp is a function of *k*, *k* should be mbind'ed

(defun msum (exp lo hi)
  (if (< hi lo)
      0
      (let ((sum 0))
	(do ((*k* lo (1+ *k*)))
	    ((> *k* hi) sum)
	  (declare (special *k*))
	  (setq sum (add2 sum (meval exp)))))))


(defun pole-err (exp)
  (declare (special errorsw))
  (cond (errorsw (throw 'errorsw t))
	(t (merror (intl:gettext "Pole encountered in: ~M") exp))))


(declare-top (special $maxpsiposint $maxpsinegint $maxpsifracnum $maxpsifracdenom))

(defprop $psi psisimp specsimp)

;; Integral of psi function psi[n](x)
(putprop '$psi
  `((n x)
   nil
   ,(lambda (n x)
     (cond 
      ((and ($integerp n) (>= n 0))
       (cond 
	((= n 0) '((%log_gamma) x))
	(t '((mqapply) (($psi array) ((mplus) -1 n)) x))))
      (t nil))))
     'integral)

(mapcar #'(lambda (var val)
	    (and (not (boundp var)) (setf (symbol-value var) val)))
	'($maxpsiposint $maxpsinegint $maxpsifracnum $maxpsifracdenom)
	'(20. -10. 6 6))

(defun psisimp (expr a z)
  (let ((s (simpcheck (car (subfunsubs expr)) z)))
    (subargcheck expr 1 1 '$psi)
    (setq a (simpcheck (car (subfunargs expr)) z))
    (and (setq z (integer-representation-p a))
         (< z 1)
         (pole-err expr))
    (eqtest (psisimp1 s a) expr)))

;; This gets pretty hairy now.

(defun psisimp1 (s a)
  (let ((*k*))
    (declare (special *k*))
    (or
     (and (integerp s) (>= s 0) (mnumericalp a)
	  (let (($float2bf t)) ($float (mfuncall '$bfpsi s a 18))))
     (and (integerp s) (>= s 0) ($bfloatp a)
	  (mfuncall '$bfpsi s a $fpprec))
     (and (not $numer) (not $float) (integerp s) (> s -1)
	  (cond
	    ((integerp a)
	     (and (not (> a $maxpsiposint)) ; integer values
		  (m*t (expt -1 s) (factorial s)
		       (m- (msum (inv (m^t '*k* (1+ s))) 1 (1- a))
			   (cond ((zerop s) '$%gamma)
				 (($zeta (1+ s))))))))
	    ((or (not (ratnump a)) (ratgreaterp a $maxpsiposint)) ())
	    ((ratgreaterp a 0)
	     (cond
	       ((ratgreaterp a 1)
		(let* ((int ($entier a)) ; reduction to fractional values
		       (frac (m-t a int)))
		  (m+t
		   (psisimp1 s frac)
		   (if (> int $maxpsiposint)
		       (subfunmakes '$psi (ncons s) (ncons int))
		       (m*t (expt -1 s) (factorial s)
			    (msum (m^t (m+t (m-t a int) '*k*)
				       (1- (- s)))
				  0 (1- int)))))))
	       ((= s 0)
		(let ((p (cadr a)) (q (caddr a)))
		  (cond
		    ((or (> p $maxpsifracnum)
			 (> q $maxpsifracdenom) (bignump p) (bignump q)) ())
		    ((and (= p 1)
			  (cond ((= q 2)
				 (m+ (m* -2 '((%log) 2)) (m- '$%gamma)))
				((= q 3)
				 (m+ (m* '((rat simp) -1 2)
					 (m^t 3 '((rat simp) -1 2)) '$%pi)
				     (m* '((rat simp) -3 2) '((%log) 3))
				     (m- '$%gamma)))
				((= q 4)
				 (m+ (m* '((rat simp) -1 2) '$%pi)
				     (m* -3 '((%log) 2)) (m- '$%gamma)))
				((= q 6)
				 (m- (m+ (m* '((rat simp) 3 2) '((%log) 3))
					 (m* 2 '((%log) 2))
					 (m* '((rat simp) 1 2) '$%pi
					     (m^t 3 '((rat simp) 1 2)))
					 '$%gamma))))))
		    ((and (= p 2) (= q 3))
		     (m+ (m* '((rat simp) 1 2)
			     (m^t 3 '((rat simp) -1 2)) '$%pi)
			 (m* '((rat simp) -3 2) '((%log) 3))
			 (m- '$%gamma)))
		    ((and (= p 3) (= q 4))
		     (m+ (m* '((rat simp) 1 2) '$%pi)
			 (m* -3 '((%log) 2)) (m- '$%gamma)))
		    ((and (= p 5) (= q 6))
		     (m- (m* '((rat simp) 1 2) '$%pi
			     (m^t 3 '((rat simp) 1 2)))
			 (m+ (m* '((rat simp) 3 2) '((%log) 3))
			     (m* 2 '((%log) 2))
			     '$%gamma)))
		    ;; Gauss's Formula
		    ((let ((f (m* `((%cos) ,(m* 2 a '$%pi '*k*))
				  `((%log) ,(m-t 2 (m* 2 `((%cos)
							   ,(m//t (m* 2 '$%pi '*k*)
								  q))))))))
		       (m+t (msum f 1 (1- (truncate q 2)))
			    (let ((*k* (truncate q 2)))
			      (declare (special *k*))
			      (m*t (meval f)
				   (cond ((oddp q) 1)
					 ('((rat simp) 1 2)))))
			    (m-t (m+ (m* '$%pi '((rat simp) 1 2)
					 `((%cot) ((mtimes simp) ,a $%pi)))
				     `((%log) ,q)
				     '$%gamma))))))))
	       ((alike1 a '((rat) 1 2))
		(m*t (expt -1 (1+ s)) (factorial s)
		     (1- (expt 2 (1+ s))) (simplify ($zeta (1+ s)))))
	       ((and (ratgreaterp a '((rat) 1 2))
		     (ratgreaterp 1 a))
		(m*t
		 (expt -1 s)
		 (m+t (psisimp1 s (m- 1 a))
		      (let ((dif (m* '$%pi
				     ($diff `((%cot) ,(m* '$%pi '$z)) '$z s)))
			    ($z (m-t a)))
			(declare (special $z))
			(meval dif)))))))
	    ((ratgreaterp a $maxpsinegint)  ;;; Reflection Formula
	     (m*t
	      (expt -1 s)
	      (m+t (m+t (psisimp1 s (m- a))
			(let ((dif (m* '$%pi
				       ($diff `((%cot) ,(m* '$%pi '$z)) '$z s)))
			      ($z (m-t a)))
			  (declare (special $z))
			  (meval dif)))
		   (m*t (factorial s) (m^t (m-t a) (1- (- s)))))))))
     (subfunmakes '$psi (ncons s) (ncons a)))))


;; subtitle polygamma tayloring routines

;; These routines are specially coded to be as fast as possible given the
;; current $TAYLOR; too bad they have to be so ugly.

(declare-top (special var subl *last* sign last-exp))

(defun expgam-fun (pw temp)
  (setq temp (get-datum (get-key-var (car var))))
  (let-pw temp pw
	  (pstimes
	   (let-pw temp (e1+ pw)
		   (psexpt-fn (getexp-fun '(($psi) -1) var (e1+ pw))))
	   (make-ps var (ncons pw) '(((-1 . 1) 1 . 1))))))

(defun expplygam-funs (pw subl l)	; l is a irrelevant here
  (setq subl (car subl))
  (if (or (not (integerp subl)) (< subl -1))
      (tay-err "Unable to expand at a subscript in")
      (prog ((e 0) (sign 0) npw)
	 (declare (fixnum e) (fixnum sign))
	 (setq npw (/ (float (car pw)) (float (cdr pw))))
	 (setq
	  l (cond ((= subl -1)
		   `(((1 . 1) . ,(prep1 '((mtimes) -1 $%gamma)))))
		  ((= subl 0)
		   (cons '((-1 . 1) -1 . 1)
			 (if (> 0.0 npw) ()
			     `(((0 . 1)
				. ,(prep1 '((mtimes) -1 $%gamma)))))))
		  (t (setq *last* (factorial subl))
		     `(((,(- (1+ subl)) . 1)
			,(* (expt -1 (1+ subl))
				(factorial subl)) . 1))))
	  e (if (< subl 1) (- subl) -1)
	  sign (if (< subl 1) -1 (expt -1 subl)))
	 a (setq e (1+ e) sign (- sign))
	 (if (> e npw) (return l)
	     (rplacd (last l)
		     `(((,e . 1)
			. ,(rctimes (rcplygam e)
				    (prep1 ($zeta (+ (1+ subl) e))))))))
	 (go a))))

(defun rcplygam (k)
  (declare (fixnum k) )
  (cond ((= subl -1) (cons sign k))
	((= subl 0) (cons sign 1))
	(t (prog1
	       (cons (* sign *last*) 1)
	     (setq *last*
		   (quot (* *last* (+ subl (1+ k)))
			 (1+ k)))))))

(defun plygam-ord (subl)
  (if (equal (car subl) -1) (ncons (rcone))
      `((,(m- (m1+ (car subl))) . 1))))

(defun plygam-pole (a c func)
  (if (rcmintegerp c)
      (let ((ps (get-lexp (m- a (rcdisrep c)) () t)))
	(rplacd (cddr ps) (cons `((0 . 1) . ,c) (cdddr ps)))
	(if (atom func) (gam-const a ps func)
	    (plygam-const a ps func)))
      (prep1 (simplifya
	      (if (atom func) `((%gamma) ,(rcdisrep c))
		  `((mqapply) ,func ,(rcdisrep c)))
	      () ))))

(defun gam-const (a arg func)
  (let ((const (ps-lc* arg)) (arg-c))
    (cond ((not (rcintegerp const))
	   (taylor2 (diff-expand `((%gamma) ,a) tlist)))
	  (t
	   (setq const (car const))
	   (if (pscoefp arg) (setq arg-c (get-lexp (m+t a (- const)) (rcone) (signp le const))))
	   (if (and arg-c (not (psp arg-c)))
	       (taylor2 (simplify `((%gamma) ,const)))
	       (let ((datum (get-datum (get-key-var (gvar (or arg-c arg)))))
		     (ord (if arg-c (le (terms arg-c)) (le (n-term (terms arg))))))
		 (setq func (current-trunc datum))
		 (if (> const 0)
		     (pstimes (let-pw datum (e- func ord) (expand (m+t a (- const)) '%gamma))
			      (let-pw datum (e+ func ord)
				      (tsprsum (m+t a (m-t '%%taylor-index%%))
					       `(%%taylor-index%% 1 ,const) '%product)))
		     (pstimes (expand (m+t a (- const)) '%gamma)
			      (let-pw datum (e+ func ord)
				      (psexpt (tsprsum (m+t a '%%taylor-index%%)
						       `(%%taylor-index%% 0 ,(- (1+ const))) '%product)
					      (rcmone)))))))))))

(defun plygam-const (a arg func)
  (let ((const (ps-lc* arg)) (sub (cadr func)))
    (cond
      ((or (not (integerp sub)) (< sub -1))
       (tay-err "Unable to expand at a subscript in"))
      ((not (rcintegerp const))
       (taylor2 (diff-expand `((mqapply) ,func ,a) tlist)))
      (t (setq const (car const))
	 (psplus
	  (expand (m+t a (- const)) func)
	  (if (> const 0)
	      (pstimes
	       (cons (* (expt -1 sub) (factorial sub)) 1)
	       (tsprsum `((mexpt) ,(m+t a (m-t '%%taylor-index%%)) ,(- (1+ sub)))
			`(%%taylor-index%% 1 ,const) '%sum))
	      (pstimes
	       (cons (* (expt -1 (1+ sub)) (factorial sub)) 1)
	       (tsprsum `((mexpt) ,(m+t a '%%taylor-index%%) ,(- (1+ sub)))
			`(%%taylor-index%% 0 ,(- (1+ const))) '%sum))))))))

(declare-top (unspecial var subl *last* sign last-exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lambert W function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; References
;;
;; Corless, R. M., Gonnet, D. E. G., Jeffrey, D. J., Knuth, D. E. (1996). 
;; "On the Lambert W function". Advances in Computational Mathematics 5: 
;; pp 329-359
;; 
;;    http://www.apmaths.uwo.ca/~djeffrey/Offprints/W-adv-cm.pdf.
;; or http://www.apmaths.uwo.ca/~rcorless/frames/PAPERS/LambertW/
;;
;; D. J. Jeffrey, D. E. G. Hare, R. M. Corless
;; Unwinding the branches of the Lambert W function
;; The Mathematical Scientist, 21, pp 1-7, (1996)
;; http://www.apmaths.uwo.ca/~djeffrey/Offprints/wbranch.pdf
;;
;; Winitzki, S. Uniform Approximations for Transcendental Functions. 
;; In Part 1 of Computational Science and its Applications - ICCSA 2003, 
;; Lecture Notes in Computer Science, Vol. 2667, Springer-Verlag, 
;; Berlin, 2003, 780-789. DOI 10.1007/3-540-44839-X_82
;; http://homepages.physik.uni-muenchen.de/~Winitzki/papers/
;;
;; Darko Verebic, 
;; Having Fun with Lambert W(x) Function
;; arXiv:1003.1628v1, March 2010, http://arxiv.org/abs/1003.1628
;;
;; See also http://en.wikipedia.org/wiki/Lambert's_W_function

(defun $lambert_w (z)
  (simplify (list '(%lambert_w) (resimplify z))))

;;; Set properties to give full support to the parser and display
(defprop $lambert_w %lambert_w alias)
(defprop $lambert_w %lambert_w verb)
(defprop %lambert_w $lambert_w reversealias)
(defprop %lambert_w $lambert_w noun)

;;; lambert_w is a simplifying function
(defprop %lambert_w simp-lambertw operators)

;;; Derivative of lambert_w
(defprop %lambert_w
  ((x) 
   ((mtimes)
    ((mexpt) $%e ((mtimes ) -1 ((%lambert_w) x)))
    ((mexpt) ((mplus) 1 ((%lambert_w) x)) -1)))
  grad)

;;; Integral of lambert_w
;;; integrate(W(x),x) := x*(W(x)^2-W(x)+1)/W(x)
(defprop %lambert_w
  ((x)
   ((mtimes)
    x
    ((mplus) 
     ((mexpt) ((%lambert_w) x) 2) 
     ((mtimes) -1 ((%lambert_w) x))
     1)
    ((mexpt) ((%lambert_w) x) -1)))
  integral)

(defun simp-lambertw (x y z)
  (oneargcheck x)
  (setq x (simpcheck (cadr x) z))
  (cond ((equal x 0) 0)
	((equal x 0.0) 0.0)
	((zerop1 x) ($bfloat 0))	;bfloat case
	((alike1 x '$%e)
	 ;; W(%e) = 1
	 1)
	((alike1 x '((mtimes simp) ((rat simp) -1 2) ((%log simp) 2)))
	 ;; W(-log(2)/2) = -log(2)
	 '((mtimes simp) -1 ((%log simp) 2)))
	((alike1 x '((mtimes simp) -1 ((mexpt simp) $%e -1)))
	 ;; W(-1/e) = -1
	 -1)
	((alike1 x '((mtimes) ((rat) -1 2) $%pi))
	 ;; W(-%pi/2) = %i*%pi/2
	 '((mtimes simp) ((rat simp) 1 2) $%i $%pi))
        ;; numerical evaluation
	((complex-float-numerical-eval-p x)
          ;; x may be an integer.  eg "lambert_w(1),numer;"
	  (if (integerp x)
	    (to (bigfloat::lambert-w-k 0 (bigfloat:to ($float x))))
	    (to (bigfloat::lambert-w-k 0 (bigfloat:to x)))))
	((complex-bigfloat-numerical-eval-p x)
	 (to (bigfloat::lambert-w-k 0 (bigfloat:to x))))
	(t (list '(%lambert_w simp) x))))

;; An approximation of the k-branch of generalized Lambert W function
;;   k integer
;;   z real or complex lisp float
;; Used as initial guess for Halley's iteration. 
;; When W(z) is real, ensure that guess is real.
(defun init-lambert-w-k (k z)
  (let ( ; parameters for k = +/- 1 near branch pont z=-1/%e
        (branch-eps 0.2e0)
	(branch-point (/ -1 %e-val))) ; branch pont z=-1/%e
    (cond 
      ; For principal branch k=0, use expression by Winitzki
      ((= k 0) (init-lambert-w-0 z))
      ; For k=1 branch, near branch point z=-1/%e with im(z) <  0
      ((and (= k 1)
	    (< (imagpart z) 0)
	    (< (abs (- branch-point z)) branch-eps))
        (bigfloat::lambert-branch-approx z))
      ; For k=-1 branch, z real with -1/%e < z < 0
      ; W(z) is real in this range
      ((and (= k -1) (realp z) (> z branch-point) (< z 0))
        (init-lambert-w-minus1 z))
      ; For k=-1 branch, near branch point z=-1/%e with im(z) >= 0
      ((and (= k -1)
	    (>= (imagpart z) 0)
	    (< (abs (- branch-point z)) branch-eps))
        (bigfloat::lambert-branch-approx z))
      ; Default to asymptotic expansion Corless et al (4.20)
      ; W_k(z) = log(z) + 2.pi.i.k - log(log(z)+2.pi.i.k)
      (t (let ((two-pi-i-k (complex 0.0e0 (* 2 pi k))))
		 (+ (log z) 
		    two-pi-i-k 
		    (* -1 (log (+ (log z) two-pi-i-k )))))))))

;; Complex value of the principal branch of Lambert's W function in 
;; the entire complex plane with relative error less than 1%, given 
;; standard branch cuts for sqrt(z) and log(z).
;; Winitzki (2003)
(defun init-lambert-w-0 (z)
  (let ((a 2.344e0) (b 0.8842e0) (c 0.9294e0) (d 0.5106e0) (e -1.213e0)
     (y (sqrt (+ (* 2 %e-val z ) 2)) ) )   ; y=sqrt(2*%e*z+2) 
    ; w = (2*log(1+B*y)-log(1+C*log(1+D*y))+E)/(1+1/(2*log(1+B*y)+2*A)
     (/ 
      (+ (* 2 (log (+ 1 (* b y))))
	 (* -1 (log (+ 1 (* c (log (+ 1 (* d y)))))))
	 e)
      (+ 1
	 (/ 1 (+ (* 2 (log (+ 1 (* b y)))) (* 2 a)))))))

;; Approximate k=-1 branch of Lambert's W function over -1/e < z < 0. 
;; W(z) is real, so we ensure the starting guess for Halley iteration 
;; is also real.
;; Verebic (2010)
(defun init-lambert-w-minus1 (z)
  (cond 
    ((not (realp z)) 
      (merror "z not real in init-lambert-w-minus1"))
    ((or (< z (/ -1 %e-val)) (plusp z))
      (merror "z outside range of approximation in init-lambert-w-minus1"))
    ;; In the region where W(z) is real
    ;; -1/e < z < C, use power series about branch point -1/e ~ -0.36787
    ;; C = -0.3 seems a reasonable crossover point
    ((< z -0.3)
      (bigfloat::lambert-branch-approx z))
    ;; otherwise C <= z < 0, use iteration W(z) ~ ln(-z)-ln(-W(z))
    ;; nine iterations are sufficient over -0.3 <= z < 0 
    (t (let* ( (ln-z (log (- z))) (maxiter 9) (w ln-z) k)
	 (dotimes (k maxiter w)
            (setq w (- ln-z (log (- w)))))))))

(in-package #-gcl #:bigfloat #+gcl "BIGFLOAT")

;; Approximate Lambert W(k,z) for k=1 and k=-1 near branch point z=-1/%e
;; using power series in y=-sqrt(2*%e*z+2)
;;   for im(z) < 0,  approximates k=1 branch
;;   for im(z) >= 0, approximates k=-1  branch
;;
;; Corless et al (1996) (4.22)
;; Verebic (2010)
;;
;; z is a real or complex bigfloat: 
(defun lambert-branch-approx (z)
  (let ((y (- (sqrt (+ (* 2 (%e z) z ) 2)))) ; y=-sqrt(2*%e*z+2)
    (b0 -1) (b1 1) (b2 -1/3) (b3 11/72))
    (+ b0 (* y (+ b1 (* y (+ b2 (* b3 y))))))))

;; Algorithm based in part on Corless et al (1996).
;;
;; It is Halley's iteration applied to w*exp(w).
;;
;;
;;                               w[j] exp(w[j]) - z 
;; w[j+1] = w[j] - -------------------------------------------------
;;                                       (w[j]+2)(w[j] exp(w[j]) -z)
;;                  exp(w[j])(w[j]+1) -  ---------------------------
;;                                               2 w[j] + 2
;;
;; The algorithm has cubic convergence.  Once convergence begins, the 
;; number of digits correct at step k is roughly 3 times the number 
;; which were correct at step k-1.
;;
;; Convergence can stall using convergence test abs(w[j+1]-w[j]) < prec,
;; as happens for generalized_lambert_w(-1,z) near branch point z = -1/%e
;; Therefore also stop iterating if abs(w[j]*exp(w[j]) - z) << abs(z)
(defun lambert-w-k (k z &key (maxiter 50))
  (let ((w (init-lambert-w-k k z)) we w1e delta (prec (* 4 (epsilon z))))
    (dotimes (i maxiter (maxima::merror "lambert-w-k did not converge"))
      (setq we (* w (exp w)))
      (when (<= (abs (- z we)) (* 4 (epsilon z) (abs z))) (return))
      (setq w1e (* (1+ w) (exp w)))
      (setq delta (/ (- we z)
		     (- w1e (/ (* (+ w 2) (- we z)) (+ 2 (* 2 w))))))
      (decf w delta)
      (when (<= (abs (/ delta w)) prec) (return)))
    ;; Check iteration converged to correct branch
    (check-lambert-w-k k w z)
    w))

(defmethod init-lambert-w-k ((k integer) (z number))
  (maxima::init-lambert-w-k k z))

(defmethod init-lambert-w-k ((k integer) (z bigfloat))
  (bfloat-init-lambert-w-k k z))

(defmethod init-lambert-w-k ((k integer) (z complex-bigfloat))
  (bfloat-init-lambert-w-k k z))

(defun bfloat-init-lambert-w-k (k z)
  "Approximate generalized_lambert_w(k,z) for bigfloat: z as initial guess"
  (let ((branch-point -0.36787944117144)) ; branch point -1/%e
    (cond
       ;; if k=-1, z very close to -1/%e and imag(z)>=0, use power series
       ((and (= k -1)
	     (or (zerop (imagpart z))
		 (plusp (imagpart z)))
	     (< (abs (- z branch-point)) 1e-10))
	 (lambert-branch-approx z))
       ;; if k=1, z very close to -1/%e and imag(z)<0, use power series
       ((and (= k 1)
	     (minusp (imagpart z))
	     (< (abs (- z branch-point)) 1e-10))
	 (lambert-branch-approx z))
       ;; Initialize using float value if z is representable as a float
       ((< (abs z) 1.0e100)
	 (if (complexp z)
	     (bigfloat (lambert-w-k k (cl:complex (float (realpart z) 1.0)
						  (float (imagpart z) 1.0))))
	     (bigfloat (lambert-w-k k (float z 1.0)))))
       ;; For large z, use Corless et al (4.20)
       ;;              W_k(z) ~ log(z) + 2.pi.i.k - log(log(z)+2.pi.i.k)
       (t
	(let ((log-z (log z)))
	  (if (= k 0)
	    (- log-z (log log-z))
	    (let* ((i (make-instance 'complex-bigfloat :imag (intofp 1)))
		  (two-pi-i-k (* 2 (%pi z) i k)))
	      (- (+ log-z two-pi-i-k) 
		 (log (+ log-z two-pi-i-k))))))))))

;; Check Lambert W iteration converged to the correct branch
;; W_k(z) + ln W_k(z) = ln z, for k = -1 and z in [-1/e,0)
;;                    = ln z + 2 pi i k, otherwise
;; See Jeffrey, Hare, Corless (1996), eq (12)
;; k integer
;; z, w bigfloat: numbers
(defun check-lambert-w-k (k w z)
  (let ((tolerance #-gcl 1.0e-6
                   #+gcl (cl:float 1/1000000)))
  (if
     (cond 
       ;; k=-1 branch with z and w real.
      ((and (= k -1) (realp z) (minusp z) (>= z (/ -1 (%e z))))
       (if (and (realp w) 
		(<= w -1)
		(< (abs (+ w (log w) (- (log z)))) tolerance))
	   t
	   nil))
       (t
         ; i k =  (W_k(z) + ln W_k(z) - ln(z)) / 2 pi
        (let (ik)
	  (setq ik (/ (+ w (log w) (- (log z))) (* 2 (%pi z))))
	  (if (and (< (realpart ik) tolerance)
		   (< (abs (- k (imagpart ik))) tolerance))
	    t
	    nil))))
      t
      (maxima::merror "Lambert W iteration converged to wrong branch"))))

(in-package :maxima)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generalized Lambert W function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun $generalized_lambert_w (k z)
  (simplify (list '(%generalized_lambert_w) (resimplify k) (resimplify z))))

;;; Set properties to give full support to the parser and display
(defprop $generalized_lambert_w %generalized_lambert_w alias)
(defprop $generalized_lambert_w %generalized_lambert_w verb)
(defprop %generalized_lambert_w $generalized_lambert_w reversealias)
(defprop %generalized_lambert_w $generalized_lambert_w noun)

;;; lambert_w is a simplifying function
(defprop %generalized_lambert_w simp-generalized-lambertw operators)

;;; Derivative of lambert_w
(defprop %generalized_lambert_w
  ((k x)
   nil
   ((mtimes)
    ((mexpt) $%e ((mtimes ) -1 ((%generalized_lambert_w) k x)))
    ((mexpt) ((mplus) 1 ((%generalized_lambert_w) k x)) -1)))
  grad)

;;; Integral of lambert_w
;;; integrate(W(k,x),x) := x*(W(k,x)^2-W(k,x)+1)/W(k,x)
(defprop %generalized_lambert_w
  ((k x)
   nil
   ((mtimes)
    x
    ((mplus) 
     ((mexpt) ((%generalized_lambert_w) k x) 2) 
     ((mtimes) -1 ((%generalized_lambert_w) k x))
     1)
    ((mexpt) ((%generalized_lambert_w) k x) -1)))
  integral)

(defun simp-generalized-lambertw (expr ignored z)
  (declare (ignore ignored))
  (twoargcheck expr)
  (let ((k (simpcheck (cadr expr) z))
        (x (simpcheck (caddr expr) z)))
    (cond
     ;; Numerical evaluation for real or complex x
     ((and (integerp k) (complex-float-numerical-eval-p x))
       ;; x may be an integer.  eg "generalized_lambert_w(0,1),numer;"
       (if (integerp x) 
	   (to (bigfloat::lambert-w-k k (bigfloat:to ($float x))))
	   (to (bigfloat::lambert-w-k k (bigfloat:to x)))))
     ;; Numerical evaluation for real or complex bigfloat x
     ((and (integerp k) (complex-bigfloat-numerical-eval-p x))
      (to (bigfloat::lambert-w-k k (bigfloat:to x))))
     (t (list '(%generalized_lambert_w simp) k x)))))
