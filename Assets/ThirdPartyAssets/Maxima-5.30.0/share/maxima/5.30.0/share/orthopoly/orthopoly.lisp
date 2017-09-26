;; 8/8 | 5/5 
;; Copyright (C) 2000, 2001, 2003, 2008, 2009 Barton Willis

#|
  This is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License,
  http://www.gnu.org/copyleft/gpl.html.

 This software has NO WARRANTY, not even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Maxima code for evaluating orthogonal polynomials listed in Chapter 22 of Abramowitz and Stegun (A & S). 
|#


(in-package :maxima)
($put '$orthopoly 1.0 '$version)

;; If the input can be evaluated to a floating point number (either
;; real or complex), convert the input to a Common Lisp complex number.
;; When the input doesn't evaluate to a float, return the input.

(defun maxima-to-lisp-complex-float (a)
  (let* (($ratprint nil)
	 (b ($rectform a))
	 (br ($float ($realpart b)))
	 (bi ($float ($imagpart b))))
    (cond ((and (numberp br) (numberp bi))
	   (if (= bi 0) (float br)
	     (complex (float br) 
		      (float bi))))
	  (t a))))

;; Return true iff a is a float or a complex number with either a
;; real or imaginary part that is a float.

(defun xcomplexp (a)
  (or (floatp a)
      (and (complexp a) (or (floatp (realpart a)) (floatp (imagpart a))))))

;; Convert a Lisp complex number into a Maxima complex number. When the
;; input isn't a Lisp complex number, return the argument.

(defun lisp-complex-to-maxima (x)
  (if (complexp x) (add (realpart x) (mul '$%i (imagpart x))) x))
   
;; When orthopoly_returns_intervals is true, floating point evaluation 
;; returns an interval using the form (($interval) c r), where c is the 
;; center of the interval and r is its radius.  We don't provide the user
;; with any tools for working with intervals; if a user wants

(defmvar $orthopoly_returns_intervals t)

(defun orthopoly-return-handler (d f e)
  (cond ((or (floatp f) (complexp f))
	 (let ((df (maxima-to-lisp-complex-float d))
	       (ef))
	   (setq f (lisp-complex-to-maxima
		    (if (numberp df) (* df f) (mul d f))))
	   (setq ef (if (and (numberp df) (numberp e)) (abs (* df e)) nil))
	   (cond ((and $orthopoly_returns_intervals (floatp ef))
		  `(($interval simp) ,f ,ef))
		 (t f))))
	(t (mul d f))))
		    	     
;; When a user requests the derivative of an a function in this package
;; with respect to the order or some other parameter, return a form 
;; ((unk) input from user). We "simplify" this form by printing an error.

(defprop unk simp-unk operators)
(defun simp-unk (x y z)
  (declare (ignore y z))
  (merror "Maxima doesn't know the derivative of ~:M with respect the ~:M argument" (nth 2 x) (nth 1 x)))

;; A left continuous unit step function; thus 
;;
;;       unit_step(x) = 0 for x <= 0 and 1 for x > 0.  
;;
;; This function differs from (1 + signum(x))/2 which isn't left or right
;; continuous at 0.

(defprop $unit_step "\\Theta" texword)

(defun simp-unit-step (a y z)
  (oneargcheck a)
  (setq y (simpcheck (cadr a) z))
  (let ((s (csign y)))
    (cond ((or (eq s '$nz) (eq s '$zero) (eq s '$neg)) 0)
	  ((eq s '$pos) 1)
	  (t `(($unit_step simp) ,y)))))

;; We barely support intervals; these functions aren't for user-level use.
;; The function intervalp returns true iff its argument is an interval.

(defun $intervalp (a)
  (and (consp a) (consp (car a)) (eq (caar a) '$interval)))

;; Multiply x by a, where x isn't an interval and a might be an 
;; interval.  When a isn't an interval or x is an interval, return
;; x * a.  The optional argument dx is an upper bound for the 
;; relative error in x.

(defun interval-mult (x a &optional dx)
  (if (null dx) (setq dx 0))
  (cond ((and ($intervalp a) (not ($intervalp x)))
	 (let ((p (nth 1 a))
	       (q (nth 2 a)))
	   (if (or (floatp p) (floatp q)) 
	       (setq x ($float ($rectform x))))
	   (setq p ($expand (mult x p)))
	   (setq q ($expand (mult x (add q (simplify `((mabs) ,(mul p dx)))))))
	   (setq q (simplify `((mabs) ,q)))
	   `(($interval) ,p ,q)))
	(t (mult x a))))
	   
;; TeX a function with subscripts and superscripts.  The string fn is the
;; function name, the list sub holds the positions of the subscripts, the list
;; sup holds the positions of the superscripts, and i is the position of the 
;; function argument.  When b1 is true, the subscript is surrounded by parens;
;; when b2 is true, the superscript is surrounded by parens.  The lists sub and
;; sup may be nil, but the function must have at least on argument.

(defun tex-sub-and-super-scripted-function (fn sub b1 sup b2 i x l r)
  (setq x (cdr x))
  (let ((lo) (hi) (s1) (s2))
    (setq s1 (if b1 `("\\left(")  nil))
    (setq s2 (if b1 `("\\right)") nil))
    (dolist (i sub)
      (setq lo (cons (nth i x) lo)))
    (setq lo (if lo (tex-list (nreverse lo) s1 s2 ",") nil))
    (setq s1 (if b2 `("\\left(")  nil))
    (setq s2 (if b2 `("\\right)") nil))
    (dolist (i sup)
      (setq hi (cons (nth i x) hi)))
    (setq hi (if hi (tex-list (nreverse hi) s1 s2 ",") nil))
    (append l `(,fn)
	    (if lo `("_{",@lo "}") nil)
	    (if hi `("^{" ,@hi "}") nil)	   
	    `(,@(tex-list (nthcdr i x) `("\\left(") `("\\right)") ","))
	    r)))
	    
(defun dimension-sub-and-super-scripted-function (fn sub sup b2 k x)
  (let ((lo) (hi) (form))
    (dolist (i sub)
      (setq lo (cons (nth i x) lo)))
    (setq lo (nreverse lo))
    (dolist (i sup)
      (setq hi (cons (nth i x) hi)))
    (setq hi (nreverse hi))
    (cond ((null hi)
	   (setq form `((,fn simp array) ,@lo)))
	  (b2
	   (setq form `((mexpt) ((,fn simp array) ,@lo) (("") ,@hi))))
	  (t
	   (setq form `((mexpt) ((,fn simp array) ,@lo) ,@hi))))
    `((,form simp) ,@(nthcdr k x))))
 
;; Return true iff 
;;   (1) each member of the list a evaluates to a floating
;;       point number (using $float) and 
;;   (2) at least one member of a has a real or imaginary part that is a 
;;       floating point number *or* a bigfloat.
;; When we find an member of a that doesn't evaluate to a float, 
;; immediately bail out and return nil.

(defun use-float (&rest a)
  (let ((xr) (xi) (float-found nil) (okay t))
    (dolist (x a)
      (setq x ($rectform x))
      (setq xr ($realpart x))
      (setq xi ($imagpart x))
      (setq float-found (or float-found (floatp xr) (floatp xi)
			    ($bfloatp xr) ($bfloatp xi)))
      (setq okay (and (floatp ($float xr)) (floatp ($float xi))))
      (if (not okay) (return)))
    (and okay float-found)))
	 
;; When n is a nonnegative integer, return 1F1(a,b,x); that is return
;; sum(pochhammer(a,k) * x^k /pochhammer(b,k),k,0,n). Regardless of the
;; value of n,  when x = 0, immediately return 1.

;; Any orthopoly function that calls hypergeo should check that n is an
;; integer with n > -1; thus the summation code shouldn't get called for
;; any orthopoly function.  It wouldn't be terrible if it happened -- I
;; think the summation form isn't useful and a user can get into trouble
;; with things like subst(0,x,sum(x^k,k,0,n)).

(defun $hypergeo11 (a b x n)
  (cond ((like x 0) 1)
	((and (integerp n) (> n -1))
	 (cond ((and (like a (- n)) (use-float b x))
		(let ((f) (e))
		  (multiple-value-setq (f e)
		    (hypergeo11-float (- n) (maxima-to-lisp-complex-float b)
				      (maxima-to-lisp-complex-float x)))
		  (values f e)))
	       (t
		(let ((sum 1) (cf 1))
		  (dotimes (i n sum)
		    (setq cf (div (mul cf (add i a) x) (mul (add i b) (+ 1 i))))
		    (setq sum (add cf sum)))))))
	(t
; The following is replaced with simplifying code.
;	 `((%sum )
;	   ((mtimes) ((mexpt) ((mfactorial) ,$genindex) -1)
;	    (($pochhammer) ,a ,$genindex)
;	    ((mexpt) (($pochhammer ) ,b ,$genindex) -1) 
;	    ((mexpt) ,x ,$genindex)) ,$genindex 0 ,n))))
         (let ((index (gensumindex)))
           (simplify
             (list '(%sum)
                   (mul (inv (take '(mfactorial) index))
                        (take '($pochhammer) a index)
                        (inv (take '($pochhammer) b index))
                        (power x index))
                   index 0 n))))))

;; return the F[2,1] hypergeometic sum from 0 to n. Use genindex as the 
;; sum index; genindex is defined in asum.lisp

(defun $hypergeo21 (a b c x n)
  (cond ((like x 0) 1)
	((and (integerp n) (> n -1))
	 (cond ((and (like a (- n)) (use-float b c x))
		(let ((f) (e))
		  (multiple-value-setq (f e)
		    (hypergeo21-float (- n) 
				      (maxima-to-lisp-complex-float b)
				      (maxima-to-lisp-complex-float c)
				      (maxima-to-lisp-complex-float x)))
		  (values f e)))
	       (t
		(let ((sum 1) (cf 1))
		  (dotimes (i n sum)
		    (setq cf (div (mul cf (add i a) (add i b) x) (mul (+ 1 i) 
								      (add i c))))
		    (setq sum (add cf sum)))))))
	
	(t
; The following is replaced with simplifying code.
;	 `((%sum)
;	   ((mtimes) (($pochhammer) ,a ,$genindex) (($pochhammer) ,b ,$genindex)
;	    ((mexpt) (($pochhammer) ,c ,$genindex) -1)
;	    ((mexpt) ((mfactorial) ,$genindex) -1)
;	    ((mexpt) ,x ,$genindex)) 
;	   ,$genindex 0 ,n))))
	 (let ((index (gensumindex)))
           (simplify
             (list '(%sum)
                   (mul (take '($pochhammer) a index)
                        (take '($pochhammer) b index)
                        (inv (take '($pochhammer) c index))
                        (inv (take '(mfactorial) index))
                        (power x index))
                   index 0 n))))))

(eval-when
    #+gcl (load eval)
    #-gcl (:load-toplevel :execute)
    (let (($context '$global) (context '$global))
      (meval '(($declare) $pochhammer $complex))))

(defmvar $pochhammer_max_index 100)

;; This disallows noninteger assignments to $max_pochhammer_index.

(setf (get '$pochhammer_max_index 'assign)
      #'(lambda (a b) 
	  (declare (ignore a))
	  (if (not (and (atom b) (integerp b)))
	      (progn
		(mtell "The value of `max_pochhammer_index' must be an integer.")
		'munbindp))))

(defun $pochhammer (x n)
  (take '($pochhammer) x n))

(in-package #-gcl #:bigfloat #+gcl "BIGFLOAT")

;; Numerical evaluation of pochhammer using the bigfloat package.
(defun pochhammer (x n)
  (let ((acc 1))
    (if (< n 0) (/ 1 (pochhammer (+ x n) (- n)))
      (dotimes (k n acc)
	(setq acc (* acc (+ k x)))))))

(in-package :maxima)

(defun simp-pochhammer (e y z)
  (declare (ignore y))
  (let ((x) (n) (return-a-rat))
    (twoargcheck e)
    (setq return-a-rat ($ratp (second e)))
    (setq x (simplifya (specrepcheck (second e)) z))
    (setq n (simplifya (specrepcheck (third e)) z))
 
    (cond ((or ($taylorp (second e)) ($taylorp (third e)))
	   (setq x (simplifya (second e) z))
	   (setq n (simplifya (third e) z))
	   ($taylor (div (take '(%gamma) (add x n)) (take '(%gamma) x))))
	   
	  ((eq n 0) 1)
	  
	  ;; pochhammer(1,n) = n! (factorial is faster than bigfloat::pochhammer.)
	  ((eq x 1) (take '(mfactorial) n))
     
	  ;; pure numeric evaluation--use numeric package.
	  ((and (integerp n) (complex-number-p x '$numberp))
	   (maxima::to (bigfloat::pochhammer (bigfloat::to x) (bigfloat::to n))))

	  ((and (integerp (mul 2 n)) (integerp (mul 2 x)) (> (mul 2 n) 0) (> (mul 2 x) 0))
	   (div (take '(%gamma) (add x n)) (take '(%gamma) x)))

	  ;; Use a reflection identity when (great (neg n) n) is true. Specifically,
	  ;; use pochhammer(x,-n) * pochhammer(x-n,n) = 1; thus pochhammer(x,n) = 1/pochhammer(x+n,-n).
	  ((great (neg n) n)
	   (div 1 (take '($pochhammer) (add x n) (neg n))))
	  
	  ;; Expand when n is an integer and either abs(n) < $expop or abs(n) < $pochhammer_max_index.
	  ;; Let's give $expand a bit of early help.
	  ((and (integerp n) (or (< (abs n) $expop) (< (abs n) $pochhammer_max_index)))
	   (if (< n 0) (div 1 (take '($pochhammer) (add x n) (neg n)))
	     (let ((acc 1))
	       (if (or (< (abs n) $expop) return-a-rat) (setq acc ($rat acc) x ($rat x)))
	       (dotimes (k n (if return-a-rat acc ($ratdisrep acc)))
		 (setq acc (mul acc (add k x)))))))
	   
	  ;; return a nounform.
	  (t `(($pochhammer simp) ,x ,n)))))

(putprop '$pochhammer
	 '((x n)
	   ((mtimes) (($pochhammer) x n)
	    ((mplus) ((mtimes) -1 ((mqapply) (($psi array) 0) x))
	     ((mqapply) (($psi array) 0) ((mplus) n x)))) 
	   ((mtimes) (($pochhammer) x n)
	    ((mqapply) (($psi array) 0) ((mplus) n x))))
	 'grad)

(defprop $pochhammer tex-pochhammer tex)

(defun tex-pochhammer (x l r)
  (setq x (mapcar #'(lambda (s) (tex s nil nil nil nil)) (cdr x)))
  (append l 
	  `("\\left(")
	  (nth 0 x)
	  `("\\right)_{")
	  (nth 1 x)
	  `("}")
	  r))

(setf (get '$pochhammer 'dimension) 'dimension-pochhammer)

(defun dimension-pochhammer (form result)
  (setq form `(( (("") ,(nth 1 form)) simp array) ,(nth 2 form)))
  (dimension-array form result))

;; pochhammer-quotient(a b x n) returns pochhammer( a,n) / pochhammer(b,n).  
;; Only when one of a, b, or x  is a floating point number and the others are 
;; numeric types (that is when (use-float a b x) evaluates to true) does this 
;; function differ from using (div ($pochhammer a n) ($pochhammer b n)).  
;; In the floating point case, we arrange the operations to reduce rounding 
;; errors and to reduce over and underflows.

(defun pochhammer-quotient (a b x n)
  (cond ((mminusp n)
	 (pochhammer-quotient b a x (neg n)))
	((and (integerp n) (use-float a b x))
	 (let ((acc 1.0))
	   (setq a (maxima-to-lisp-complex-float a))
	   (setq b (maxima-to-lisp-complex-float b))
	   (dotimes (i n (lisp-float-to-maxima-float acc))
	     (setq acc (* acc (/ (+ i a) (+ i b)))))))
	(t (div ($pochhammer a n) ($pochhammer b n)))))
  
(defun use-hypergeo (n x)
  (declare (ignore x))
  (or (and (integerp n) (> n -1))
      ($featurep n '$integer)))
;      (and ($featurep n '$integer) ($ratnump x))))

;; See A&S 22.5.42, page 779. For integer n, use the identity
;;     binomial(n+a,n) = pochhammer(a+1,n)/pochhammer(1,n)

;; The condition number of the pochhammer function is 
;;     |1 + x/(x+1) + x/(x+2) + ... + x/(x+n-1)| <= n.
;; The relative floating point error in computing the pochhammer 
;; function is bounded by 3 n eps.  Putting these errors together,
;; the error in d is bounded 4 n |d| eps.

;; We're looking at (d +|- 4 n eps |d|)(f +|- e) = d (f +|- (e + 4 n eps |f|)) + 
;; O(eps^2). 

(defun $jacobi_p (n a b x)
  (cond ((use-hypergeo n x)
	 (let ((f) (d) (e))
	   ;(setq d (div ($pochhammer (add a 1) n) ($pochhammer 1 n)))
	   (setq d (pochhammer-quotient (add a 1) 1 x n))
	   (multiple-value-setq (f e)
	     ($hypergeo21 (mul -1 n) (add n a b 1) (add a 1)
			  (div (add 1 (mul -1 x)) 2) n))
	   (setq e (if e (+ e (* 4 n (abs f) flonum-epsilon)) nil))
	   (orthopoly-return-handler d f e)))
	(t `(($jacobi_p simp) ,n ,a ,b ,x))))

(putprop '$jacobi_p
	 '((n a b x)
	   ((unk) "$first" "$jacobi_p")
	   ((unk) "$second" "$jacobi_p")
	   ((unk) "$third" "$jacobi_p")

	   ((mtimes)
	    ((mexpt) ((mplus ) a b ((mtimes ) 2 n)) -1)
	    ((mplus)
	     ((mtimes) 2
	      (($unit_step) n)
	      ((mplus) a n) ((mplus) b n)
	      (($jacobi_p) ((mplus) -1 n) a b x))
	     ((mtimes) n (($jacobi_p) n a b x)
	      ((mplus) a ((mtimes ) -1 b)
	       ((mtimes)
		((mplus) ((mtimes) -1 a) ((mtimes ) -1 b)
		 ((mtimes) -2 n)) x))))
	    ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt ) x 2))) -1)))
	 'grad)
 	   
(defprop $jacobi_p tex-jacobi-poly tex)

(defun tex-jacobi-poly (x l r)
  (tex-sub-and-super-scripted-function "P" `(0) nil `(1 2) t 3 x l r))

(setf (get '$jacobi_p 'dimension) 'dimension-jacobi-p)

(defun dimension-jacobi-p (form result)
  (dimension-function
   (dimension-sub-and-super-scripted-function "P" `(1) `(2 3) t 4 form)
   result))
     	  
;; See A&S 22.5.46, page 779.

(defun $ultraspherical (n a x)
  (cond ((use-hypergeo n x)
	 (let ((f) (d) (e))
	   ;(setq d (div ($pochhammer (mul 2 a) n) ($pochhammer 1 n)))
	   (setq d (pochhammer-quotient (mul 2 a) 1 x n))
	   (multiple-value-setq (f e)
	     ($hypergeo21 (mul -1 n) (add n (mul 2 a)) (add a (div 1 2))
			  (div (add 1 (mul -1 x)) 2) n))
	   (setq e (if e (+ e (* 4 n (abs f) flonum-epsilon)) nil))
	   (orthopoly-return-handler d f e)))
	(t `(($ultraspherical simp) ,n ,a ,x))))

(putprop '$ultraspherical 
	 '((n a x)
	   ((unk) "$first" "$ultraspherical")
	   ((unk) "$second" "$ultrapsherical")
	   ((mtimes)
	    ((mplus)
	     ((mtimes)
	      (($unit_step) n)
	      ((mplus) -1 ((mtimes) 2 a) n)
	      (($ultraspherical) ((mplus) -1 n) a x))
	     ((mtimes) -1 n (($ultraspherical) n a x) x))
	    ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) x 2))) -1)))
	 'grad) 	   

(defprop $ultraspherical tex-ultraspherical tex)

(defun tex-ultraspherical (x l r)
  (tex-sub-and-super-scripted-function "C" `(0) nil `(1) t 2 x l r))

(setf (get '$ultraspherical 'dimension) 'dimension-ultraspherical)

(defun dimension-ultraspherical (form result)
    (dimension-function
     (dimension-sub-and-super-scripted-function "C" `(1) `(2) t 3 form)
     result))
  
(defun $chebyshev_t (n x)
  (cond ((use-hypergeo n x)
	 (let ((f) (e))
	   (multiple-value-setq (f e)
	     ($hypergeo21 (mul -1 n) n (rat 1 2) (div (add 1 (mul -1 x)) 2) n))
	   (orthopoly-return-handler 1 f e)))
	(t `(($chebyshev_t simp) ,n ,x))))

(putprop '$chebyshev_t 
	 '((n x)
	   ((unk) "$first" "$chebyshev_t")
	   ((mtimes)
	    ((mplus)
	     ((mtimes) n (($chebyshev_t) ((mplus ) -1 n) x))
	     ((mtimes ) -1 n (($chebyshev_t) n x) x))
	    ((mexpt) ((mplus ) 1 ((mtimes) -1 ((mexpt) x 2))) -1)))
	   'grad)

(defprop $chebyshev_t tex-chebyshev-t tex)

(defun tex-chebyshev-t (x l r)
  (tex-sub-and-super-scripted-function "T" `(0) nil nil nil 1 x l r))

(setf (get '$chebyshev_t 'dimension) 'dimension-chebyshev-t)

(defun dimension-chebyshev-t (form result)
  (dimension-function
   (dimension-sub-and-super-scripted-function "T" `(1) nil nil 2 form)
   result))

;; See A & S 22.5.48, page 779.

(defun $chebyshev_u (n x)
  (cond ((use-hypergeo n x)
	 (let ((f) (d) (e))
	   (setq d (add 1 n)) 
	   (multiple-value-setq (f e)
	     ($hypergeo21 (mul -1 n) (add 2 n) (rat 3 2)
			  (div (add 1 (mul -1 x)) 2) n))
	   (orthopoly-return-handler d f e)))
	(t `(($chebyshev_u simp) ,n ,x))))

(putprop '$chebyshev_u
	 '((n x)
	   ((unk) "$first" "$chebyshev_u")
	   ((mtimes)
	    ((mplus)
	     ((mtimes)
	      (($unit_step) n)
	      ((mplus) 1 n) (($chebyshev_u) ((mplus) -1 n) x))
	     ((mtimes) -1 n (($chebyshev_u) n x) x))
	    ((mexpt) ((mplus ) 1 ((mtimes) -1 ((mexpt) x 2))) -1)))
	 'grad) 

(defprop $chebyshev_u tex-chebyshev-u tex)

(defun tex-chebyshev-u (x l r)
  (tex-sub-and-super-scripted-function "U" `(0) nil nil nil 1 x l r))

(setf (get '$chebyshev_u 'dimension) 'dimension-chebyshev-u)

(defun dimension-chebyshev-u (form result)
  (dimension-function
   (dimension-sub-and-super-scripted-function "U" `(1) nil nil 2 form)
   result))

;; See A&S 8.2.1 page 333 and 22.5.35 page 779.  We evaluate the legendre
;; polynomials as jacobi_p(n,0,0,x).  Eat less exercise more.

(defun $legendre_p (n x)
  (cond ((use-hypergeo n x) 
	 (if (and (integerp n) (< n 0))
	   ($legendre_p (- (abs n) 1) x)
	   ($jacobi_p n 0 0 x)))
	(t `(($legendre_p simp) ,n ,x))))

(putprop '$legendre_p 
	 '((n x) 
	   ((unk) "$first" "$legendre_p")
	   ((mtimes)
	     ((mplus)
	      ((mtimes) n (($legendre_p) ((mplus) -1 n) x))
	      ((mtimes) -1 n (($legendre_p) n x) x))
	     ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) x 2))) -1)))
	 'grad)

(defprop $legendre_p tex-legendre-p tex)

(defun tex-legendre-p (x l r)
  (tex-sub-and-super-scripted-function "P" `(0) nil nil nil 1 x l r))

(setf (get '$legendre_p 'dimension) 'dimension-legendre-p)

(defun dimension-legendre-p (form result)
  (dimension-function
   (dimension-sub-and-super-scripted-function "P" `(1) nil nil 2 form)
   result))
  
(defun $legendre_q (n x)
  (if (and (integerp n) (> n -1)) 
      ($assoc_legendre_q n 0 x)
    `(($legendre_q simp) ,n ,x)))

(putprop '$legendre_q 
	 '((n x) 
	   ((unk) "$first" "$legendre_p")
	   ((mplus)
	    ((mtimes) -1 ((%kron_delta) 0 n)
	     ((mexpt) ((mplus) -1 ((mexpt) x 2)) -1)) 
	    ((mtimes)
	     ((mplus)
	      ((mtimes) n (($legendre_q) ((mplus) -1 n) x))
	      ((mtimes) -1 n (($legendre_q) n x) x))
	     ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) x 2))) -1))))
	 'grad)

(defprop $legendre_q tex-legendre-q tex)

(defun tex-legendre-q (x l r)
  (tex-sub-and-super-scripted-function "Q" `(0) nil nil nil 1 x l r))

(setf (get '$legendre_q 'dimension) 'dimension-legendre-q)

(defun dimension-legendre-q (form result)
 (dimension-function
   (dimension-sub-and-super-scripted-function "Q" `(1) nil nil 2 form)
   result))
   	 
;; See A & S 8.6.7 and 8.2.6 pages 333 and 334. I chose the 
;; definition that is real valued on (-1,1).  

;; For negative m, A & S 8.2.6 page 333 and  G & R 8.706 page 1000
;; disagree; the factor of exp(i m pi) in A & S 8.1.6 suggests to me that
;; A & S  8.2.6 is bogus.  As further evidence, Macsyma 2.4 seems to 
;; agree with G & R 8.706. I'll use G & R.

;; Return assoc_legendre(0,m,x). This isn't a user-level function; we 
;; don't check that m is a positive integer.

(defun q0m (m x)
  (cond ((< m 0)
	 (merror "function q0m called with negative order. File a bug report"))
   	((= m 0)
	 (div (simplify `((%log) ,(div (add 1 x) (sub 1 x)))) 2))
	(t
	 (mul (factorial (- m 1)) `((rat simp) 1 2)
	      (if (oddp m) -1 1) (power (sub 1 (mult x x)) (div m 2))
	      (add 
	       (mul (if (oddp m) 1 -1) (power (add 1 x) (neg m)))
	       (power (sub 1 x) (neg m)))))))
  
;; Return assoc_legendre(1,m,x).  This isn't a user-level function; we 
;; don't check that m is a positive integer; we don't check that m is 
;; a positive integer.

(defun q1m (m x)
  (cond ((< m 0)
	 (merror "function q1m called with negative order. File a bug report"))
	((= m 0)
	 (sub (mul x (q0m 0 x)) 1))
	((= m 1)
	 (mul -1 (power (sub 1 (mult x x)) `((rat simp) 1 2))
	      (sub (q0m 0 x) (div x (sub (mul x x) 1)))))
	(t
	 (mul (if (oddp m) -1 1) (power (sub 1 (mult x x)) (div m 2))
	      (add
	       (mul (factorial (- m 2)) `((rat simp) 1 2) (if (oddp m) -1 1)
		    (sub (power (add x 1) (sub 1 m))
			 (power (sub x 1) (sub 1 m))))
	       
	       (mul (factorial (- m 1)) `((rat simp) 1 2) (if (oddp m) -1 1)
		    (add (power (add x 1) (neg m)) 
			 (power (sub x 1) (neg m)))))))))

;; Return assoc_legendre_q(n,n,x). I don't have a reference that gives
;; a formula for assoc_legendre_q(n,n,x). To figure one out,  I used
;; A&S 8.2.1 and a formula for assoc_legendre_p(n,n,x).  

;; After finishing the while loop, q = int(1/(1-t^2)^(n+1),t,0,x). 

(defun assoc-legendre-q-nn (n x)
  (let ((q) 
	(z (sub 1 (mul x x))) 
	(i 1))
    (setq q (div (simplify `((%log) ,(div (add 1 x) (sub 1 x)))) 2))
    (while (<= i n)
      (setq q (add (mul (sub 1 `((rat) 1 ,(* 2 i))) q)
		   (div x (mul 2 i (power z i)))))
      (incf i))
    (mul (expt -2 n) (factorial n) (power z (div n 2)) q)))

;; Use degree recursion to find the assoc_legendre_q function. 
;; See A&S 8.5.3. When i = m in the while loop, we have a special
;; case.  For negative order, see A&S 8.2.6.

(defun $assoc_legendre_q (n m x)
  (cond ((and (integerp n) (> n -1) (integerp m) (<= (abs m) n))
	 (cond ((< m 0)
		(mul (div (factorial (+ n m)) (factorial (- n m)))
		     ($assoc_legendre_q n (- m) x)))
	       (t
		(if (not (or (floatp x) ($bfloatp x))) (setq x ($rat x)))
		(let* ((q0 (q0m m x))
		       (q1 (if (= n 0) q0 (q1m m x)))
		       (q) (i 2)
		       (use-rat (or ($ratp x) (floatp x) ($bfloatp x))))
		  
		  (while (<= i n)
		    (setq q (if (= i m) (assoc-legendre-q-nn i x) 
			      (div (sub (mul (- (* 2 i) 1) x q1) 
					(mul (+ i -1 m) q0)) (- i m))))
		    (setq q0 q1)
		    (setq q1 q)
		    (incf i))
		  (if use-rat q1 ($ratsimp q1))))))
	(t `(($assoc_legendre_q simp) ,n ,m ,x))))

;; See G & R, 8.733, page 1005 first equation.

(putprop '$assoc_legendre_q
	 '((n m x)
	   ((unk) "$first" "$assoc_legendre_q")
	   ((unk) "$second" "$assoc_legendre_q")
	   
	   ((mplus)
	    ((mtimes)
	     ((mplus)
	      ((mtimes) -1 ((mplus) 1 ((mtimes) -1 m) n)
	       (($assoc_legendre_q ) ((mplus ) 1 n) m x))
	      ((mtimes) ((mplus) 1 n)
	       (($assoc_legendre_q ) n m x) x))
	     ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) x 2))) -1))))
	 'grad) 
	    
(defprop $assoc_legendre_q tex-assoc-legendre-q tex)

(defun tex-assoc-legendre-q (x l r)
  (tex-sub-and-super-scripted-function "Q" `(0) nil `(1) nil 2 x l r))

(setf (get '$assoc_legendre_q 'dimension) 'dimension-assoc-legendre-q)

(defun dimension-assoc-legendre-q (form result)
 (dimension-function
   (dimension-sub-and-super-scripted-function "Q" `(1) `(2) nil 3 form)
   result))

;; See A & S 22.5.37 page 779, A & S 8.6.6 (second equation) page 334, and 
;; A & S 8.2.5 page 333.  For n < 0, see A&S 8.2.1 page 333.

(defun $assoc_legendre_p (n m x)
  (let ((f) (d) (dx 0))
    (cond ((and (integerp n) (integerp m))
	   (cond ((< n 0)
		  (setq f ($assoc_legendre_p (- (abs n) 1) m x))
		  (setq d 1)
		  (setq dx 1))
		 ((> (abs m) n)
		  (setq f 0)
		  (setq d 1))
		 ((< m 0)
		  (setq f ($assoc_legendre_p n (neg m) x))
		  ;; Adding a factor (-1)^m to the transformation to get the
		  ;; expected results for odd negative integers. DK 09/2009
		  (setq d (mul (power -1 m)
		               (div (factorial (+ n m)) (factorial (- n m)))))
		  (setq dx 1))
		 (t
		  (cond ((eq m 0)
			 (setq d 1))
			(t
			 (setq d (simplify  
				  `((%genfact) ,(- (* 2 m) 1) ,(- m 1) 2)))
			 (setq d (mul d (if (oddp m) -1 1)))
			 (setq d (mul d (power (sub 1 (mul x x)) (div m 2))))))
		  (setq dx 4)
		  (setq f 
			($ultraspherical (- n m) (add m (rat 1 2)) x)))))
	  (t
	   (setq d 1)
	   (setq f `(($assoc_legendre_p simp) ,n ,m ,x))))
    (interval-mult d f (* flonum-epsilon dx))))


;; For the derivative of the associated legendre p function, see
;; A & S 8.5.4 page 334.

(putprop `$assoc_legendre_p
	 '((n m x)
	   ((unk) "$first" "$assoc_legendre_p")
	   ((unk) "$second" "$assoc_legendre_p")
	   ((mtimes simp)
	    ((mplus simp)
	     ((mtimes simp) -1 ((mplus simp) m n) (($unit_step) n)
	      (($assoc_legendre_p simp) ((mplus simp) -1 n) m x))
	     ((mtimes simp) n (($assoc_legendre_p simp) n m x) x))
	    ((mexpt simp) ((mplus simp) -1 ((mexpt simp) x 2)) -1))) 
	   'grad) 
	   
(defprop $assoc_legendre_p tex-assoc-legendre-p tex)

(defun tex-assoc-legendre-p (x l r)
  (tex-sub-and-super-scripted-function "P" `(0) nil `(1) nil 2 x l r))

(setf (get '$assoc_legendre_p 'dimension) 'dimension-assoc-legendre-p)

(defun dimension-assoc-legendre-p (form result)
  (dimension-function
   (dimension-sub-and-super-scripted-function "P" `(1) `(2) nil 3 form)
   result))
		  		
;; See A&S 22.5.55 and 22.5.56, page 780.

(defun $hermite (n x)
  (cond ((integerp n)
	 (let ((f) (d) (e))
	   (cond (($evenp n)
		  (setq n (/ n 2))
		  (multiple-value-setq (f e)  
		    ($hypergeo11 (- n) (rat 1 2) (mul x x) n))
		  (setq d (mul (if (oddp n) -1 1) (factorial (* 2 n))
			       (div 1 (factorial n)))))
		 (($oddp n)
		  (setq n (/ (- n 1) 2))
		  (multiple-value-setq (f e)
		    ($hypergeo11 (- n) (rat 3 2) (mul x x) n))
		  (setq d (mul (if (oddp n) -1 1) (factorial (+ 1 (* 2 n))) 2 x
			       (div 1 (factorial n))))))
	   (orthopoly-return-handler d f e))) 
	(t `(($hermite) ,n ,x))))

(putprop '$hermite
	 '((n x)
	   ((unk) "$first" "$hermite")
	   ((mtimes) 2 n (($hermite) ((mplus) -1 n) x)))
	 'grad)

(defprop $hermite tex-hermite tex)

(defun tex-hermite (x l r)
  (tex-sub-and-super-scripted-function "H" `(0) nil nil nil 1 x l r))

(setf (get '$hermite 'dimension) 'dimension-hermite)

(defun dimension-hermite (form result)
 (dimension-function
   (dimension-sub-and-super-scripted-function "H" `(1) nil nil 2 form)
   result))

;; See A & S 22.5.54, page 780.  For integer n, use the identity
;;     binomial(n+a,n) = pochhammer(a+1,n)/pochhammer(1,n)

(defun $gen_laguerre (n a x)
  (cond ((use-hypergeo n x)
	 (let ((f) (d) (e))
	   ;(setq d (div ($pochhammer (add a 1) n) ($pochhammer 1 n)))
	   (setq d (pochhammer-quotient (add a 1) 1 x n))
	   (multiple-value-setq (f e)
	     ($hypergeo11 (mul -1 n) (add 1 a) x n))
	   (setq e (if e (+ e (* 4 (abs f) flonum-epsilon n)) nil))
	   (orthopoly-return-handler d f e)))
	(t
	 `(($gen_laguerre) ,n ,a ,x))))

(putprop '$gen_laguerre
	 '((n a x)
	   ((unk) "$first" "$gen_laguerre")
	   ((unk) "$second" "$gen_laguerre")
	   ((mtimes)
	    ((mplus)
	     ((mtimes) -1 ((mplus) a n)
	      (($unit_step) n) (($gen_laguerre) ((mplus) -1 n) a x))
	     ((mtimes) n (($gen_laguerre) n a x)))
	    ((mexpt) x -1)))
	 'grad)
	 	 
(defprop $gen_laguerre tex-gen-laguerre tex)

(defun tex-gen-laguerre (x l r)
  (tex-sub-and-super-scripted-function "L" `(0) nil `(1) t 1 x l r))

(setf (get '$gen_laguerre 'dimension) 'dimension-gen-laguerre)

(defun dimension-gen-laguerre (form result)
  (dimension-function
   (dimension-sub-and-super-scripted-function "L" `(1) `(2) t 3 form)
   result))

;; See A & S 22.5.16, page 778.

(defun $laguerre (n x)
  (cond ((use-hypergeo n x)
	 (let ((f) (e))
	   (multiple-value-setq (f e) ($hypergeo11 (mul -1 n) 1 x n))
	   (orthopoly-return-handler 1 f e)))
	(t
	 `(($laguerre) ,n ,x))))

(putprop '$laguerre
	 '((n x)
	   ((unk) "$first" "$laguerre")
	   ((mtimes)
	    ((mplus)
	     ((mtimes) -1 n (($laguerre) ((mplus) -1 n) x))
	     ((mtimes) n (($laguerre) n x)))
	    ((mexpt) x -1)))
	 'grad)
	 
(defprop $laguerre tex-laguerre tex)

(defun tex-laguerre (x l r)
  (tex-sub-and-super-scripted-function "L" `(0) nil nil nil 1 x l r))

(setf (get '$laguerre 'dimension) 'dimension-laguerre)

(defun dimension-laguerre (form result)
  (dimension-function
   (dimension-sub-and-super-scripted-function "L" `(1) nil nil 2 form)
   result))

(defun $spherical_hankel1 (n x)
  (let ((f) (d) (e))
    (cond ((and (integerp n) (< n 0))
	   (setq d (mul '$%i (if (oddp n) 1 -1)))
	   (multiple-value-setq (f e)
	     ($spherical_hankel1 (add -1 (mul -1 n)) x))
	   (orthopoly-return-handler d f e))
	  ((use-hypergeo n x)
	   (multiple-value-setq (f e)
	     ($hypergeo11 (mul -1 n) (mul -2 n) (mul -2 '$%i x) n))
	   (setq d (mul '$%i (if (= 0 n) 1 
			       (simplify `((%genfact) ,(add (mul 2 n) -1) 
					   ,(add n (rat -1 2)) 2)))
			(power '$%e (mul '$%i x)) (div -1 (power x (add 1 n)))))
	   (orthopoly-return-handler d f e))
	  (t 
	   `(($spherical_hankel1) ,n ,x)))))

(putprop '$spherical_hankel1
	 '((n x)
	   ((unk) "$first" "$spherical_hankel1")
	   ((mplus simp) (($spherical_hankel1) ((mplus) -1 n) x)
	    ((mtimes simp) -1 ((mplus) 1 n)
	     (($spherical_hankel1) n x) ((mexpt) x -1))))
	 'grad)

(defprop $spherical_hankel1 tex-spherical-hankel-1 tex)

(defun tex-spherical-hankel-1 (x l r)
  (tex-sub-and-super-scripted-function "h^{(1)}" `(0) nil nil nil 1 x l r))

(setf (get '$spherical_hankel1 'dimension) 'dimension-spherical-hankel-1)

(defun dimension-spherical-hankel-1 (form result)
  (let ((form1 `((mexpt) (($\h simp array) ,(nth 1 form)) 
		 (1))))
    (dimension-function `((,form1 simp) ,(nth 2 form)) result)))

;; See A & S 10.1.36.

(defun $spherical_hankel2 (n x)
  (cond ((integerp n)
	 (setq x (mul x (power '$%e (mul '$%i '$%pi (add (mul 2 n) 1)))))
	 (let ((f))
	   (setq f ($spherical_hankel1 n x))
	   (if (oddp n) (interval-mult -1 f) f)))
	(t `(($spherical_hankel2) ,n ,x))))

(putprop '$spherical_hankel2
	 '((n x)
	   ((unk) "$first" "$spherical_hankel2")
	   ((mplus simp) (($spherical_hankel2) ((mplus) -1 n) x)
	    ((mtimes simp) -1 ((mplus) 1 n)
	     (($spherical_hankel2) n x) ((mexpt) x -1))))
	 'grad)

(defprop $spherical_hankel2 tex-spherical-hankel-2 tex)

(defun tex-spherical-hankel-2 (x l r)
  (tex-sub-and-super-scripted-function "h^{(2)}" `(0) nil nil nil 1 x l r))

(setf (get '$spherical_hankel2 'dimension) 'dimension-spherical-hankel-2)

(defun dimension-spherical-hankel-2 (form result)
  (let ((form1 `((mexpt) (($\h simp array) ,(nth 1 form))  (2))))
    (dimension-function `((,form1 simp) ,(nth 2 form)) result)))
  
;;---------------------------------------------------------------------
;; The spherical_bessel functions use the functions p-fun and q-fun.
;; See A&S 10.1.8 and 10.1.9 page 437.

(defun p-fun (n x)
  (let ((s 1) (w 1) 
	(n1 (floor (/ n 2)))
	(x2 (mul x x)) (m2))
    (dotimes (m n1 s)
      (setq m2 (* 2 m))
      (setq w (div (mul w `((rat) ,(* -1 (+ n m2 2) (+ n m2 1) 
				      (- n m2) (- n (+ m2 1)))
			    ,(* 4 (+ m2 1) (+ m2 2)))) x2))
      (setq s (add s w)))))

(defun q-fun (n x)
  (let ((s (if (= 0 n) 0 1))
	(w 1) (m2) (x2 (mul x x))
	(n1 (floor (/ (- n 1) 2))))
    (dotimes (m n1 (div (mul n (+ n 1) s) (mul 2 x)))
      (setq m2 (* 2 m))
      (setq w (div (mul w `((rat) ,(* -1 (+ n m2 3) (+ n m2 2) 
				      (- n (+ m2 1)) (- n (+ m2 2)))
			    ,(* 4 (+ m2 3) (+ m2 2)))) x2))
      (setq s (add s w)))))


;; See A&S 10.1.8 page 437 and A&S 10.1.15 page 439.  When the order
;; is an integer and x is a float or a bigfloat, use the slatec code
;; for numerical evaluation.  Yes, we coerce bigfloats to floats and
;; return a float.

;; For numerical evaluation, we do our own analytic continuation -- otherwise
;; we get factors exp(%i n %pi / 2) that should evaluate to 1,%i,-1,-%, but
;; numerically have "fuzz" in their values.  The fuzz can cause the spherical
;; bessel functions to have nonzero (but small) imaginary values on the
;; negative real axis. See A&S 10.1.34

(defun $spherical_bessel_j (n x)
  (cond ((and (eq '$zero (csign ($ratdisrep x)))
	      (or (integerp n) ($featurep n '$integer)))
	 `((%kron_delta) 0 ,n))

	((and (use-float x) (integerp n))
	 (let ((d 1) (xr) (xi) (z))
	   (setq x ($rectform ($float x)))
	   (setq xr ($realpart x))
	   (setq xi ($imagpart x))
	   (setq z (complex xr xi))
	   (cond ((< xr 0.0)
		  (setq d (if (oddp n) -1 1))
		  (setq x (mul -1 x))
		  (setq z (* -1 z))))
	   (setq n (+ 0.5 ($float n)))
	   (setq d (* d (sqrt (/ pi (* 2 z)))))
	   (setq d (lisp-float-to-maxima-float d))
	   ($expand (mul ($rectform d) ($bessel_j n x)))))

	((and (integerp n) (> n -1))
	 (let ((xt (sub x (div (mul n '$%pi) 2))))
	   (div (add
		 (mul (p-fun n x) (simplify `((%sin) ,xt)))
		 (mul (q-fun n x) (simplify `((%cos) ,xt)))) x)))

	((integerp n)
	 (mul (if (oddp n) -1 1) ($spherical_bessel_y (- (+ n 1)) x)))

	(t 
	 `(($spherical_bessel_j) ,n ,x))))
	 
(putprop '$spherical_bessel_j
	 '((n x)
	   ((unk) "$first" "$spherical_bessel_j")
	   ((mtimes) ((mexpt) ((mplus) 1 ((mtimes) 2 n)) -1)
	    ((mplus)
	     ((mtimes) n (($spherical_bessel_j) ((mplus) -1 n) x))
	     ((mtimes) -1 ((mplus) 1 n)
	      (($spherical_bessel_j) ((mplus) 1 n) x)))))
	 'grad)
 
(defprop $spherical_bessel_j tex-spherical-bessel-j tex)

(defun tex-spherical-bessel-j (x l r)
  (tex-sub-and-super-scripted-function "j^{(2)}" `(0) nil nil nil 1 x l r))

(setf (get '$spherical_bessel_j 'dimension) 'dimension-spherical-bessel-j)

(defun dimension-spherical-bessel-j (form result)
  (let ((form1 `(($\j simp array) ,(nth 1 form)))) 
    (dimension-function `((,form1 simp) ,(nth 2 form)) result)))

;; For analytic continuation, see A&S 10.1.35.
 
(defun $spherical_bessel_y (n x)
  (cond ((and (use-float x) (integerp n))
	 (let ((d 1) (xr) (xi) (z))
	   (setq x ($rectform ($float x)))
	   (setq xr ($realpart x))
	   (setq xi ($imagpart x))
	   (setq z (complex xr xi))
	   (cond ((< xr 0.0)
		  (setq d (if (oddp n) 1 -1))
		  (setq x (mul -1 x))
		  (setq z (* -1 z))))
	   (setq n (+ 0.5 ($float n)))
	   (setq d (* d (sqrt (/ pi (* 2 z)))))
	   (setq d (lisp-float-to-maxima-float d))
	   ($expand (mul ($rectform d) ($bessel_y n x)))))

	((and (integerp n) (> n -1))
	 (let ((xt (add x (div (mul n '$%pi) 2))))
	   (mul (if (oddp n) 1 -1)
		(div (sub
		      (mul (p-fun n x) (simplify `((%cos) ,xt)))
		      (mul (q-fun n x) (simplify `((%sin) ,xt)))) x))))

	((integerp n)
	 (mul (if (oddp n) 1 -1) ($spherical_bessel_j (- (+ n 1)) x)))
	(t  `(($spherical_bessel_y) ,n ,x))))

(putprop '$spherical_bessel_y
	 '((n x)
	   ((unk) "$first" "$spherical_bessel_y")
	   ((mtimes) ((mexpt) ((mplus) 1 ((mtimes) 2 n)) -1)
	    ((mplus)
	     ((mtimes) n (($spherical_bessel_y) ((mplus) -1 n) x))
	     ((mtimes) -1 ((mplus) 1 n)
	      (($spherical_bessel_y) ((mplus) 1 n) x)))))
	 'grad)
 
(defprop $spherical_bessel_y tex-spherical-bessel-y tex)

(defun tex-spherical-bessel-y (x l r)
  (tex-sub-and-super-scripted-function "y^{(2)}" `(0) nil nil nil 1 x l r))

 (setf (get '$spherical_bessel_y 'dimension) 'dimension-spherical-bessel-y)

(defun dimension-spherical-bessel-y (form result)
  (let ((form1 `(($\y simp array) ,(nth 1 form)))) 
    (dimension-function `((,form1 simp) ,(nth 2 form)) result)))

;; Compute P_n^m(cos(theta)).  See Merzbacher, 9.59 page 184
;; and 9.64 page 185, and A & S 22.5.37 page 779.  This function
;; lacks error checking; it should only be called by spherical_harmonic.

;; We need to be careful -- for the spherical harmonics we can't use
;; assoc_legendre_p(n,m,cos(x)).  If we did, we'd get factors 
;; (1 - cos^2(x))^(m/2) that simplify to |sin(x)|^m but we want them
;; to simplify to sin^m(x).  Oh my!

(defun assoc-leg-cos (n m x)
  (interval-mult
   (if (= m 0) 1 (mul (take '(%genfact) (sub (mul 2 m) 1) (sub m (div 1 2)) 2) (power (take '(%sin) x) m)))
   ($ultraspherical (sub n m) (add m (div 1 2)) (take  '(%cos) x))))

(defun $spherical_harmonic (n m th p)
  (cond ((and (integerp n) (integerp m) (> n -1))
	 (cond ((> (abs m) n)
		0)
	       ((< m 0)
		(interval-mult (if (oddp m) -1 1) 
			       ($spherical_harmonic n (- m) th (mul -1 p))))
	       (t
		(interval-mult
		 (mul ($exp (mul '$%i m p))
		      (power (div (* (+ (* 2 n) 1) (factorial (- n m)))
				  (mul '$%pi (* 4 (factorial (+ n m))))) 
			     `((rat) 1 2)))
		 (assoc-leg-cos n m th)))))
	(t
	 `(($spherical_harmonic) ,n ,m ,th ,p))))

(defprop $spherical_harmonic tex-spherical-harmonic tex)

(defun tex-spherical-harmonic (x l r)
  (tex-sub-and-super-scripted-function "Y" `(0) nil `(1) nil 2 x l r))

(setf (get '$spherical_harmonic 'dimension) 'dimension-spherical-harmonic)

(defun dimension-spherical-harmonic (form result)
 (dimension-function
  (dimension-sub-and-super-scripted-function "Y" `(1) `(2) nil 3 form)
  result))

(putprop '$spherical_harmonic
	 '((n m theta phi)
	   ((unk) "$first" "$spherical_harmonic")
	   ((unk) "$second" "$spherical_harmonic")
	   ((mplus)
	    ((mtimes) ((rat ) -1 2)
	     ((mexpt)
	      ((mtimes) ((mplus) ((mtimes) -1 m) n)
	       ((mplus) 1 m n))
	      ((rat) 1 2))
	     (($spherical_harmonic) n ((mplus) 1 m) theta phi)
	     ((mexpt) $%e ((mtimes) -1 $%i phi)))
	    ((mtimes) ((rat) 1 2)
	     ((mexpt)
	      ((mtimes) ((mplus) 1 ((mtimes) -1 m) n)
	       ((mplus) m n))
	      ((rat) 1 2))
	     (($spherical_harmonic) n ((mplus) -1 m) theta phi)
	     ((mexpt) $%e ((mtimes) $%i phi)))) 
	   
	   ((mtimes) $%i m (($spherical_harmonic) n m theta phi)))
	 'grad)
	  	   	  	 				 	
(defun maxima-float-to-lisp-float (y)
  (let* ((x ($rectform y))
	 (xr ($realpart x))
	 (xi ($imagpart x)))
    (cond ((or (floatp xr) (floatp xi))
	   (setq xr (float xr)
		 xi (float xi))
	   (if (= 0.0 xi) xr (complex xr xi)))
	  (t
	   y))))

(defun lisp-float-to-maxima-float (x)
  (if (complexp x)
      (add (realpart x) (mul '$%i (imagpart x)))
    x))
     
(defun hypergeo11-float (n b x)
  (let ((f0) (fm1) (f) (i 0) (k) (dk) (ak) (bk) (err)
	(as (make-array (- 1 n) 
			:initial-element 0.0))
	(bs (make-array (- 1 n) 
			:initial-element 0.0))
	(fs (make-array (- 1 n)
			:initial-element 0.0))
	(u) (u0) (um1))

    (setq f0 1.0)
    (setq fm1 0.0)
    (setq x (- b x))
    (setq n (- n))
    (while (< i n)
      (setf (aref fs i) f0)
      (setq dk (+ b i))
      (setq ak (/ (+ (* 2 i) x) dk))
      (setq bk (/ (- i) dk))
      (setq f (+ (* ak f0) (* bk fm1)))
      (setf (aref as i) ak)
      (setf (aref bs i) bk)
      (setq fm1 f0)
      (setq f0 f)
      (incf i))
    (setf (aref fs i) f0)
    (setq i 1)
    (setq err 1.0)
    (setq u0 1.0)
    (setq um1 0.0)
    (while (< i n)
      (setq k (- n i))
      (setq u (+ (* (aref as k) u0)
		 (* (aref bs (+ 1 k)) um1)))
      (setq um1 u0)
      (setq u0 u)
      (setq err (+ err (abs (* u0 (aref fs k)))))
      (incf i))
    (values f0 (* 12 flonum-epsilon err))))
    
(defun hypergeo21-float (n b c x)
  (let ((f0) (fm1) (f) (i 0) (k) (dk) (ak) (bk) (err)
	(as (make-array (- 1 n) 
			:initial-element 0.0))
	(bs (make-array (- 1 n) 
			:initial-element 0.0))
	(fs (make-array (- 1 n)
			:initial-element 0.0))
	(u) (u0) (um1))

    (setq f0 1.0)
    (setq fm1 0.0)
    (setq n (- n))
    (while (< i n)
      (setf (aref fs i) f0)
      (setq dk (+ c i))
      (setq ak (/ (+ (* 2 i) c (- (* x (+ b i)))) dk))
      (setq bk (/ (* i (- x 1)) dk))
      (setq f (+ (* ak f0) (* bk fm1)))
      (setf (aref as i) ak)
      (setf (aref bs i) bk)
      (setq fm1 f0)
      (setq f0 f)
      (incf i))
    (setf (aref fs i) f0)
    (setq i 1)
    (setq err 1.0)
    (setq u0 1.0)
    (setq um1 0.0)
    (while (< i n)
      (setq k (- n i))
      (setq u (+ (* (aref as k) u0)
		 (* (aref bs (+ 1 k)) um1)))
      (setq um1 u0)
      (setq u0 u)
      (incf i)
      (setq err (+ err (abs (* (aref fs k) u0)))))
    (values f0 (* 12 flonum-epsilon err))))
    
;; For recursion relations, see A & S 22.7 page 782. 

;; legendre_p(n+1,x) = ((2*n+1)*legendre_p(n,x)*x-n*legendre_p(n-1,x))/(n+1)

;;  jacobi_p(n+1,a,b,x) = (((2*n+a+b+1)*(a^2-b^2) + 
;;    x*pochhammer(2*n+a+b,3)) * jacobi_p(n,a,b,x) - 
;;    2*(n+a)*(n+b)*(2*n+a+b+2)*jacobi_p(n-1,a,b,x))/(2*(n+1)*(n+a+b+1)*(2*n+a+b))

;; ultraspherical(n+1,a,x) = (2*(n+a)*x * ultraspherical(n,a,x) - 
;;    (n+2*a-1)*ultraspherical(n-1,a,x))/(n+1)

;; chebyshev_t(n+1,x) = 2*x*chebyshev_t(n,x) -chebyshev_t(n-1,x)

;; chebyshev_u(n+1,x) = 2*x*chebyshev_u(n,x) -chebyshev_u(n-1,x)

;;  laguerre(n+1,x) = (((2*n+1) - x)*laguerre(n,x)  -(n)*laguerre(n-1,x))/(n+1)

;; gen_laguerre(n+1,a,x) = (((2*n+a+1) - x)*gen_laguerre(n,a,x)  
;;  -(n+a)*gen_laguerre(n-1,a,x))/(n+1)

;; hermite(n+1,x) = 2*x*hermite(n,x) -2*n*hermite(n-1,x)

;; See G & R 8.733.2; A & S 22.7.11 might be wrong -- or maybe I need
;; reading glasses.

;; (2*n+1)*x*assoc_legendre_p(n,m,x) = (n-m+1)*assoc_legendre_p(n+1,m,x) 
;; + (n+m)*assoc_legendre_p(n-1,m,x)

;; For the half-integer bessel functions, See A & S 10.1.19

;; fn(n-1,x) + fn(n+1,x) = (2*n+1)*fn(n,x)/x;

(defun check-arg-length (fn n m)
  (if (not (= n m))
      (merror "Function ~:M needs ~:M arguments, instead it received ~:M"
	      fn n m)))

(defun $orthopoly_recur (fn arg)
  (if (not ($listp arg)) 
      (merror "The second argument to orthopoly_recur must be a list"))
  (cond ((eq fn '$jacobi_p)
	 (check-arg-length fn 4 (- (length arg) 1))
	 (let ((n (nth 1 arg))
	       (a (nth 2 arg))
	       (b (nth 3 arg))
	       (x (nth 4 arg)))
	   (simplify
	    `((mequal) (($jacobi_p ) ((mplus) 1 ,n) ,a ,b ,x)
	      ((mtimes) ((rat) 1 2) ((mexpt) ((mplus) 1 ,n) -1)
	       ((mexpt) ((mplus) 1 ,a ,b ,n) -1)
	       ((mexpt) ((mplus) ,a ,b ((mtimes) 2 ,n)) -1)
	       ((mplus)
		((mtimes) -2 ((mplus) ,a ,n) ((mplus) ,b ,n)
		 ((mplus) 2 ,a ,b ((mtimes) 2 ,n))
		 (($jacobi_p ) ((mplus) -1 ,n) ,a ,b ,x))
		((mtimes) (($jacobi_p ) ,n ,a ,b ,x)
		 ((mplus)
		 ((mtimes)
		  ((mplus) ((mexpt) ,a 2)
		   ((mtimes) -1 ((mexpt) ,b 2)))
		  ((mplus) 1 ,a ,b ((mtimes) 2 ,n)))
		 ((mtimes) ((mplus) ,a ,b ((mtimes) 2 ,n))
		  ((mplus) 1 ,a ,b ((mtimes) 2 ,n))
		  ((mplus) 2 ,a ,b ((mtimes) 2 ,n)) ,x)))))))))

	((eq fn '$ultraspherical)
	 (check-arg-length fn 3 (- (length arg) 1))
	 (let ((n (nth 1 arg))
	       (a (nth 2 arg))
	       (x (nth 3 arg)))
	   (simplify
	    `((mequal) (($ultraspherical) ((mplus) 1 ,n) ,a ,x)
	     ((mtimes) ((mexpt) ((mplus) 1 ,n) -1)
	      ((mplus)
	       ((mtimes) -1 ((mplus) -1 ((mtimes) 2 ,a) ,n)
		(($ultraspherical) ((mplus) -1 ,n) ,a ,x))
	       ((mtimes) 2 ((mplus) ,a ,n)
		(($ultraspherical) ,n ,a ,x) ,x)))))))

	((member fn  `($chebyshev_t $chebyshev_u) :test 'eq)
	 (check-arg-length fn 2 (- (length arg) 1))
	 (let ((n (nth 1 arg))
	       (x (nth 2 arg)))
	  (simplify
	   `((mequal ) ((,fn) ((mplus ) 1 ,n) ,x)
	    ((mplus )
	     ((mtimes ) -1 ((,fn) ((mplus ) -1 ,n) ,x))
	     ((mtimes ) 2 ((,fn) ,n ,x) ,x))))))

	((member fn '($legendre_p $legendre_q) :test 'eq)
	 (check-arg-length fn 2 (- (length arg) 1))
	 (let* ((n (nth 1 arg))
	       (x (nth 2 arg))
	       (z (if (eq fn '$legendre_q) 
		      `((mtimes) -1 ((%kron_delta) ,n 0)) 0))) 
	   (simplify
	     `((mequal) ((,fn) ((mplus) 1 ,n) ,x)
	       ((mplus)
		((mtimes) ((mexpt) ((mplus) 1 ,n) -1)
		 ((mplus)
		  ((mtimes) ((mtimes) -1 ,n)
		   ((,fn) ((mplus) -1 ,n) ,x))
		  ((mtimes) ((mplus) 1 ((mtimes) 2 ,n))
		   ((,fn) ,n ,x) ,x)))
		,z)))))

	((member fn '($assoc_legendre_p $assoc_legendre_q) :test 'eq)
	 (check-arg-length fn 3 (- (length arg) 1))
	 (let ((n (nth 1 arg))
	       (m (nth 2 arg))
	       (x (nth 3 arg)))
	   (simplify
	    `((mequal) ((,fn) ((mplus) 1 ,n) ,m ,x)
	      ((mtimes)
	       ((mexpt) ((mplus) 1 ((mtimes) -1 ,m) ,n) -1)
	       ((mplus)
		((mtimes)
		 ((mplus) ((mtimes) -1 ,m)
		  ((mtimes) -1 ,n))
		 ((,fn) ((mplus) -1 ,n) ,m ,x))
		((mtimes) ((mplus) 1 ((mtimes) 2 ,n))
		 ((,fn) ,n ,m ,x) ,x))))))) 
	
	((eq fn '$laguerre)
	 (check-arg-length fn 2 (- (length arg) 1))
	 (let ((n (nth 1 arg))
	       (x (nth 2 arg)))
	   (simplify
	    `((mequal ) (($laguerre ) ((mplus ) 1 ,n) ,x)
	      ((mtimes ) ((mexpt ) ((mplus ) 1 ,n) -1)
	       ((mplus )
		((mtimes ) -1 ,n (($laguerre ) ((mplus ) -1 ,n) ,x))
		((mtimes ) (($laguerre ) ,n ,x)
		 ((mplus ) 1 ((mtimes ) 2 ,n) ((mtimes ) -1 ,x))))))))) 

	((eq fn '$gen_laguerre)
	 (check-arg-length fn 3 (- (length arg) 1))
	 (let ((n (nth 1 arg))
	       (a (nth 2 arg))
	       (x (nth 3 arg)))
	   (simplify
	    `((mequal) (($gen_laguerre) ((mplus) 1 ,n) ,a ,x)
	      ((mtimes) ((mexpt ) ((mplus) 1 ,n) -1)
	       ((mplus)
		((mtimes) -1 ((mplus) ,a ,n)
		 (($gen_laguerre) ((mplus) -1 ,n) ,a ,x))
		((mtimes) (($gen_laguerre) ,n ,a ,x)
		 ((mplus) 1 ,a ((mtimes) 2 ,n) ((mtimes ) -1 ,x))))))))) 

	((eq fn '$hermite)
	 (check-arg-length fn 2 (- (length arg) 1))
	 (let ((n (nth 1 arg))
	       (x (nth 2 arg)))
	   (simplify
	    `((mequal) (($hermite) ((mplus) 1 ,n) ,x)
	      ((mplus)
	       ((mtimes) -2 ,n (($hermite) ((mplus) -1 ,n) ,x))
	       ((mtimes) 2 (($hermite) ,n ,x) ,x))))))

	((member fn `($spherical_bessel_j $spherical_bessel_y
					  $spherical_hankel1 $spherical_hankel2)
		 :test 'eq)
	 (check-arg-length fn 2 (- (length arg) 1))
	 (let ((n (nth 1 arg))
	       (x (nth 2 arg)))
	   (simplify
	    `((mequal) ((,fn) ((mplus) 1 ,n) ,x)
	      ((mplus) ((mtimes) -1 ((,fn ) ((mplus) -1 ,n) ,x))
	       ((mtimes) ((,fn) ,n ,x) ((mexpt) ,x -1))
	       ((mtimes) 2 ,n ((,fn ) ,n ,x) ((mexpt) ,x -1)))))))
	 
	(t (merror "A recursion relation for ~:M isn't known to Maxima" fn))))
    
;; See A & S Table 22.2, page 774.

(defun $orthopoly_weight (fn arg)
  (if (not ($listp arg)) 
      (merror "The second argument to orthopoly_weight must be a list"))

  (if (not (or ($symbolp (car (last arg))) ($subvarp (car (last arg)))))
      (merror "The last element of the second argument to orthopoly_weight must
be a symbol or a subscripted symbol, instead found ~:M" (car (last arg))))

  (if (not (every #'(lambda (s) 
		      ($freeof (car (last arg)) s)) (butlast (cdr arg))))
      (merror "Only the last element of ~:M may depend on the integration
variable ~:M" arg (car (last arg))))

  (cond ((eq fn '$jacobi_p)
	 (check-arg-length fn 4 (- (length arg) 1))
	 (let ((a (nth 2 arg))
	       (b (nth 3 arg))
	       (x (nth 4 arg)))
	   (simplify
	    `((mlist)
	      ((mtimes) ((mexpt) ((mplus) 1 ((mtimes) -1 ,x)) ,a)
	       ((mexpt) ((mplus ) 1 ,x) ,b))
	      -1 1))))
	
	((eq fn '$ultraspherical)
	 (check-arg-length fn 3 (- (length arg) 1))
	 (let ((a (nth 2 arg))
	       (x (nth 3 arg)))
	   (simplify
	    `((mlist)
	      ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) ,x 2)))
	       ((mplus) ((rat) -1 2) ,a)) -1 1))))

	((eq fn '$chebyshev_t)
	 (check-arg-length fn 2 (- (length arg) 1))
	 (let ((x (nth 2 arg)))
	   (simplify
	    `((mlist)
	      ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) ,x 2)))
	       ((rat) -1 2)) -1 1)))) 
	  
	((eq fn '$chebyshev_u)
	 (check-arg-length fn 2 (- (length arg) 1))
	 (let ((x (nth 2 arg)))
	   (simplify
	    `((mlist)
	      ((mexpt) ((mplus) 1  ((mtimes) -1 ((mexpt) ,x 2)))
	       ((rat) 1 2)) -1 1))))

	((eq fn '$legendre_p)
	 (check-arg-length fn 2 (- (length arg) 1))
	 `((mlist) 1 -1 1))

	; This is for a fixed order.  There is also an orthogonality
	; condition for fixed degree with weight function 1/(1-x^2).
	; See A & S 8.14.11 and 8.14.12.
	((eq fn '$assoc_legendre_p)
	 (check-arg-length fn 3 (- (length arg) 1))
	 `((mlist) 1 -1 1))

	((eq fn '$laguerre)
	 (check-arg-length fn 2 (- (length arg) 1))
	 (let ((x (nth 2 arg)))
	   (simplify
	    `((mlist) ((mexpt) $%e ((mtimes) -1 ,x)) 0 $inf))))

	((eq fn '$gen_laguerre)
	 (check-arg-length fn 3 (- (length arg) 1))
	 (let ((a (nth 2 arg))
	       (x (nth 3 arg)))
	   (simplify
	    `((mlist)
	      ((mtimes) ((mexpt) ,x ,a)
	       ((mexpt) $%e ((mtimes) -1 ,x))) 0 $inf))))

	((eq fn '$hermite)
	 (check-arg-length fn 2 (- (length arg) 1))
	 (let ((x (nth 2 arg)))
	   (simplify
	    `((mlist) ((mexpt) $%e ((mtimes) -1 ((mexpt) ,x 2)))
	      ((mtimes ) -1 $inf) $inf))))

	(t (merror "A weight for ~:M isn't known to Maxima" fn))))
    

    



