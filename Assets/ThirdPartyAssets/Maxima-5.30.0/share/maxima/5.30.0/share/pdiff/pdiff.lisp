;; A positional derivative package for Maxima.
;; Copyright (C) 2002, 2008, Barton Willis 

;;  Maxima pdiff is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,

(in-package :maxima)
($put '$pdiff 1.4 '$version)

;; Required for wxMaxima display--I switched from ((%pderivop) ...) to (($pderivop) ...).
(setf (get '$pderivop 'wxxml) 'wxxml-pderivop)

;; When use_pdiff is true, use positional derivatives for unknown
;; non-subscripted functions. By unknown function, I mean a function that
;; is not bound to a formula and that has a derivative that is not known
;; to Maxima. Let's default $use_pdiff to false.

(defmvar $use_pdiff nil)
(setf $use_pdiff t)

(defmvar $commute_partial_derivatives t)

;; Return the number of arguments (arity) for f; when the arity is
;; unknown or the arity is variable (for example addition), return false.
;; To attempt to find the arity, we check if f is a user-defined function
;; (examine the $functions list), check if f has a gradef property, check
;; if f is a trig, inverse trig, log or abs function. 

;; Subscripted functions aren't members of $functions; for this and 
;; other reasons, we disallow subscripted functions from being 
;; positionally differentiated. 

(defun get-number-args (f)
  (let ((z (cdar (member f (cdr $functions) :key #'caar))))
    (cond ((and z (every #'symbolp z)) (length z))
	  ((and (atom f) (or (trigp f) (arcp f) (memq f '(%log %gamma %erf %sqrt mabs)))) 1)
	  ((memq f '($bessel_i $bessel_j $bessel_k $bessel_y)) 2) 
	  ((or (equal f "^") (equal f ".") (equal f "^^")) 2)
	  ((and (op-equalp f 'lambda) (every #'symbolp (margs (second f))))
	   ($length (second f)))
	  ((and (symbolp f) (get f 'grad)) (length (first (get f 'grad))))
	  ((eq t ($constantp f)) 'constant-function)
	  (t nil))))
 
;; pderivop(f,n1,n2,...np) returns the function whose formula is
;; (x1,x2,...,xn) --> diff(f(x1,x2,..xn),x1,n1,x2,n2,...). The 
;; pderivop function allows the user to do things like
;;  
;;  (c1) tellsimpafter(pderivop(f,1)(a),1);
;;  (c2) tellsimpafter(pderivop(f,2)(a),1);
;;  (c3) diff(f(x),x,2) + diff(f(x),x);
;;
;;  (d3) 			       f   (x) + f   (x)
;;  				        (2)	  (1)
;;  (c4) subst(a,x,%);
;;  (d4)      2 
 
;; And
;;   (c5) g(x) := x^2;
;;   (c6) pderivop(g,1);
;;   (c7) ev(%);
;;   (d7) lambda([i54924], 2 i54924)
;;   (c8) apply(%,[z]);
;;
;;   (d8)     2 z

(defprop $pderivop simp-pderivop operators)

(defun simp-pderivop (x yy z)
  (declare (ignore yy))
  (let ((f (simplifya (cadr x) z))
	(n (mapcar #'(lambda (s) (simplifya s z)) (cddr x)))
	(gen-args nil)
	(nbr-args))
    (setq nbr-args (get-number-args f))
    (cond ((eq nbr-args 'constant-function)
	   (cond ((some #'(lambda (s) (and (integerp s) (> s 0))) n)
		  `((lambda simp) ((mlist) ((mlist) $x)) 0))

		 ((every #'(lambda (s) (and (integerp s) (= s 0))) n)
		  `((lambda simp) ((mlist) ((mlist) $x)) ,f))

		 (t `(($pderivop simp) ,f ,@n))))
	  
	  ;; When every derivative is 0 (this includes the case that there are no derivatives),
	  ;; return f (even when f is not a function---for example pderivop(a < b) --> a < b).
	  ((every #'(lambda (s) (equal 0 s)) n) f)
	  
	  ((integerp nbr-args)
	  
	   (if (not (= nbr-args (length n)))
	       (merror "~:M expected ~:M derivative argument(s), but it received ~:M" f nbr-args (length n))
	     (progn
	       (dotimes (i nbr-args) 
		 (push (gensym) gen-args))
	       (push '(mlist) gen-args)
	       (setq f ($apply f gen-args))
	       (setq gen-args (cdr gen-args))
	       (setq f (apply '$diff (cons f (mapcan #'list gen-args n))))
	       `((lambda) ((mlist) ,@gen-args) ,f))))
	  
	  ;; pderivop(pderivop(m1,m2,...mk),n1,n2,...,nk) --> pderivop(m1+n1,m2+n2,...mk+nk),
	  ;; provided all the m1..nk are explicit nonnegative integers. I could allow declared
	  ;; integers, I suppose. Also, I could give an error when the number of positional 
	  ;; derivatives aren't equal--for example pderivop(pderivop(f,1,2),3).

	  ((and (op-equalp f '$pderivop)
		$commute_partial_derivatives
		(every #'(lambda (s) (and (integerp s) (> s -1))) n)
		(every #'(lambda (s) (and (integerp s) (> s -1))) (cdr (margs f)))
		(= (length n) (- (length (margs f)) 1)))
	   `(($pderivop simp) ,(cadr f) ,@(mapcar #'(lambda (a b) (add a b)) (cddr f) n)))
	  
	  (t `(($pderivop simp) ,f ,@n)))))
    	 
;; Extension to mapply1:
  
(setf (get '$pderivop 'mapply1-extension) 'mapply-pdiff)

(defun mapply-pdiff (fn args fnname form)
  (declare (ignore fnname form))
  (if (and $use_pdiff (eq (mop fn) '$pderivop))
      (cond ((equal (length (cddr fn)) (length args))
	     `((mqapply simp) ,fn ,@args))
	    (t
	     (merror "The function ~:M expected ~:M argument(s), but it received ~:M" 
		     (cadr fn) (length (cddr fn)) (length args))))
    nil))


;; Extension to sdiffgrad

(defun sdiffgrad-pdiff (e x)
  (let ((args) (fun (caar e)) (de) (n) (d-order))
    (labels ((pderivop (f x n) (simplify `((mqapply) (($pderivop) ,f ,@n) ,@x)))
	     (incf-ith (i e)
		       (let ((k (nth i e)) (q (copy-list e)))
			 (setf (nth i q) (add 1 k))
			 q))
	     (i-list (i n) (incf-ith i (make-list n :initial-element 0))))

      (cond ((and $use_pdiff (eq fun 'mqapply) (eq (caaadr e) '$pderivop))
	     (setq args (cddr e))
	     (setq fun (cadadr e))
	     (setq de 0)
	     (setq n (length args))
	     (setq d-order (cddadr e))
	     (dotimes (i n de)
	       (setq de (add de (mul ($diff (nth i args) x) (pderivop fun args (incf-ith i d-order)))))))
	  
	    ;; We disallow positional derivatives of subscripted functions and lambda forms.
    
	    ((and $use_pdiff (null (oldget fun 'grad)) (not ($subvarp (cadr e)))
		  (not (memq fun '(mlessp mleqp mequal mgeqp mgreaterp mcond lambda))))
	     (setq args (cdr e))
	     (setq fun (caar e))
	     (setq de 0)
	     (setq n (length args))
	     (dotimes (i n de)
	       (setq de (add de (mul ($diff (nth i args) x) (pderivop fun args (i-list i n)))))))
	    (t nil)))))

;; Display support for positional derivatives. Indicate the derivative order 
;; with a subscript surrounded by parenthesis.

(setf (get '$pderivop 'dimension) 'dimension-pderiv)

(defun dimension-pderiv (form result)
  (setq form (cdr form))
  (setq form `(( ,(car form) simp array) (("") ,@(cdr form))))
  (dimension-array form result))

;; Extend tex to handle positional derivatives.  Depending on the values of 
;; the option variables $tex_uses_prime_for_derivatives
;; and $tex_uses_named_subscripts_for_derivatives, derivatives can
;; tex as superscripted primes, subscripted variable names, or
;; parenthesis surrounded subscripts that indicate the derivative order. 

(defmvar $tex_uses_prime_for_derivatives nil)
(defmvar $tex_prime_limit 3)
(defmvar $tex_uses_named_subscripts_for_derivatives nil)
(defmvar $tex_diff_var_names (list '(mlist) '$x '$y '$z))

(setf (get '$pderivop 'tex) 'tex-pderivop)

;; Examples 

;; 1. $tex_uses_prime_for_derivatives is true, 

;;    (c1) tex(diff(f(x),x));
;;           $$f^{\prime}(x)$$
;;    (c2) tex(diff(f(x),x,2));
;;           $$f^{\prime\prime}(x)$$
;;    (c4) tex(diff(f(x),x,4));
;;           $$f^{(4)}(x)$$

;; In the last example, the derivative order exceeds $tex_prime_limit, 
;; so the derivative is indicated as shown in (c4).

;; 2. $tex_uses_named subscripts is true 
;;  (c1) tex_uses_prime_for_derivatives : false;
;;  (c2) tex(diff(f(x),x));
;;       $$f_{x}(x)$$
;;  (c3) tex(diff(f(x),x,2));
;;       $$f_{xx}(x)$$
;;  (c4) tex(diff(f(y),y,2));
;;       $$f_{xx}(y)$$

;; Although the function argument in (c4) is y, the derivative with
;; respect to the first argument is indicated by a subscript that is
;; the first element of tex_diff_var_names. A further example

;;  (c5) tex_diff_var_names : [\a,\b,\c]$
;;  (c6) tex(diff(f(x,y,z),x,1,y,1,z,1));
;;       $$f_{abc}(x,y,z)$$ 

;; When the derivative order exceeds tex_prime_limit, we don't use named
;; subscripts for derivatives; otherwise, we could get ridiculously long
;; subscripts.

;;   (c43) tex_prime_limit : 3;
;;   (c44) tex(diff(f(x,y),x,1,y,1));
;;         $$f_{xy}(x,y)$$
;;   (c45) tex(diff(f(x,y),x,1066,y,1776));
;;         $$f_{\left(1066,1776\right)}(x,y)$$

;; Finally, setting tex_uses_named subscripts and tex_uses_prime_for_derivatives
;; to false, derivatives are indicated with parenthesis surrounded 
;; subscripts.  There is one subscript for each function argument; when
;; the derivative order is zero, the subscript is zero.

;;  (c11) tex_uses_prime_for_derivatives : false;
;;  (c12) tex_uses_named_subscripts_for_derivatives : false;
;;  (c13) tex(diff(f(a,b),a,2,b,1));
;;            $$f_{\left(2,1\right)}(a,b)$$
;;  (c14) tex(diff(f(a,b),a,0,b,1));
;;            $$f_{\left(0,1\right)}(a,b)$$
;;  (c15) tex(diff(f(x,y),x,0,y,1));
;;            $$f_{\left(0,1\right)}(x,y)$$

(defun tex-pderivop (x l r)
  ;(print `(lop = ,lop rop = ,rop x = ,x r = ,r l = ,l))
  (cond ((and $tex_uses_prime_for_derivatives (eq 3 (length x)))
	 (let* ((n (car (last x)))
		(p))
	   
	   (cond ((<= n $tex_prime_limit)
		  (setq p (make-list n :initial-element "\\prime")))
		 (t
		  (setq p (list "(" n ")"))))

	   ;; We need to avoid double tex superscripts; when rop is mexpt, use parens.

	   (cond ((eq rop 'mexpt)
		  (append l (list "\\left(") (tex (cadr x) nil nil lop rop) 
			  (list "^{") p (list "}") (list "\\right)") r))
		 (t
		  (append l  (tex (cadr x) nil nil lop rop) 
			  (list "^{") p (list "}")  r)))))
		  
	((and $tex_uses_named_subscripts_for_derivatives 
	      (< (apply #'+ (cddr x)) $tex_prime_limit))
	 (let ((n (cddr x))
	       (v (mapcar #'stripdollar (cdr $tex_diff_var_names)))
	       (p))
	   
	   (cond ((> (length n) (length v))
		  (merror "Not enough elements in tex_diff_var_names to tex the expression")))
	   (dotimes (i (length n))
	     (setq p (append p (make-list (nth i n) :initial-element (nth i v)))))
	   (append l (tex (cadr x) nil nil lop rop) (list "_{") p (list "}") r)))
	   
	(t
	 (append l (tex (cadr x) nil nil lop rop)  (list "_{") 
		 (tex-matchfix (cons '(mprogn) 
				     (cddr x)) nil nil) (list "}") r))))

;; Convert from positional derivatives to non-positional derivatives.

(defun $convert_to_diff (e)
  (setq e ($totaldisrep e))
  (cond (($mapatom e) e)
	((and (consp e) (eq (caar e) 'mqapply) (eq (caaadr e) '$pderivop))
	 (let ((f (second (second e)))
	       (n (rest (rest (second e))))
	       (v (rest (rest e)))
	       (g nil)
	       ($use_pdiff nil)
	       (vk) (gk)
	       (at-list nil))
	   (dolist (vs v) (declare (ignore vs)) (push (gensym) g))
	   (setq e (mapply f g nil))
	   (setq e (apply '$diff (cons e (mapcan #'list g n))))
	   (while v
	     (setq vk (pop v))
	     (setq gk (pop g))
	     (if (symbolp vk) (setq e ($substitute vk gk e)) (push (take '(mequal) gk vk) at-list)))
	   (if at-list ($at e (simplify (cons '(mlist) at-list))) e)))
	(t (mapply (mop e) (mapcar #'$convert_to_diff (margs e)) nil))))

