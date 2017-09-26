;; Maxima code for extracting powers, finding leading and trailing 
;; coefficients, and finding the degree of polynomials.

;; Author Barton Willis, University of Nebraska at Kearney (aka UNK)
;; December 2001, December 2002

;; License: GPL
;; The user of this code assumes all risk for its use. It has no warranty.
;; If you don't know the meaning of "no warranty," don't use this code. :)

(in-package :maxima)
($put '$powers 1 '$version)

;; Acknowledgement: Dan Stanger helped find and correct bugs.  He
;; also wrote user documentation and a test routine. 

;; posintp(x) returns true iff x is a positive integer or if x has been declared 
;; to be an integer and has been assumed to be greater than zero. Thus

;;  (C1) declare(m, integer)$
;;  (C2) assume(m > 0)$
;;  (C3) posintp(m);
;;  (D3)        TRUE 
;; posintp isn't used by any functions in powers; it could be expunged.

(defun $posintp (x)
  (and (or ($integerp x) ($featurep x '$integer)) (mgrp x 0)))

;; Set ratfac to nil, return rat(e,x), and reset ratfac to
;; its previous value.

(defun myrat (e &rest x)
  (let ((save-ratfac $ratfac))
    (setq $ratfac nil)
    (unwind-protect
	 (apply '$rat `(,e ,@x))
      (setq $ratfac save-ratfac))))
            
;; If x list a Maxima list of symbols, return true iff the expression p
;; doesn't depend on any member of x.  If x is a symbol, return true
;; iff p doesn't depend on x.  This function is similar to $freeof, but
;; it maybe somewhat more efficient when p is a CRE expression. Finally,
;; if x (any member of x when x is a Maxima list) isn't a symbol signal 
;; an error.

(defun $ratfreeof (x p)
  (setq x (require-list-of-symbols x "$ratfreeof" 2))
  (let ((p-vars (cdr ($showratvars p))))
    (cond ((every #'(lambda (z) (or ($symbolp z) ($subvarp z))) p-vars)
	   (every #'(lambda (z) (null (member z p-vars :test #'like))) x))
	  (t 
	   (setq p ($totaldisrep p))
	   (every #'(lambda(z) ($freeof ($totaldisrep z) p)) x)))))

;; variablep(e) evaluates to true if and only if e is a non-constant symbol 
;; or a subscripted symbol. Because symbolp(pi) evaluates to true, we need to 
;; check whether cd mae is constant.

(defun $variablep (e)
  (and (or ($symbolp e) ($subvarp e)) (not ($constantp e))))

;; ordinal_string(i) returns the ordinal name of the integer i.  When 
;; i > 10, i < 1, or i isn't an integer, give up and return i-th.

(defun $ordinal_string (i)
  (case i
    (1 "first")
    (2 "second")
    (3 "third")
    (4 "fourth")
    (5 "fifth")
    (6 "sixth")
    (7 "seventh")
    (8 "eighth")
    (9 "ninth")
    (10 "tenth")
    (otherwise
      (format nil "~A-th" (mfuncall '$string i)))))
    
;; If variablep(v) evaluates to false, signal an error saying that
;; the i-th argument of the function f requires a symbol; otherwise, 
;; return true.

(defun require-symbol (v f i)
  (if (not ($variablep v))
      (merror "The ~A argument of ~:M must be a symbol, instead found ~:M" 
	      ($ordinal_string i) f v) t))

;; If v is a Maxima list and each element of v is a symbol, return the
;; cdr of v.  When v isn't a list, but is a symbol, return the Lisp list
;; (list v). Otherwise signal an error saying that the i-th argument of the
;; function f requires a symbol or a list of symbols.

(defun require-list-of-symbols (v f i)
  (let ((x))
    (if ($listp v) (setq x (cdr v)) (setq x (list v)))
    (if (every #'$variablep x) x
	(merror "The ~A argument of ~:M must be a symbol or a list of symbols, instead found ~:M" ($ordinal_string i) f v))))
      
(defun require-poly (p v f i)
  (setq p (myrat p v))
  (if ($polynomialp p v) p
      (merror "The ~A argument of ~:M requires a polynomial, instead found ~:M" ($ordinal_string i) f p)))

(defun require-nonlist (e f i)
  (if ($listp e)
      (merror "The ~A argument of ~:M requires a nonlist, instead found ~:M" 
	      ($ordinal_string i) f e)))

;; Return a Maxima list of the non-constant rat variables in e.
   
(defun non-constant-ratvars (e)
  (let ((v (cdr ($showratvars e)))
	(acc))
    (dolist (vi v `((mlist simp) ,@acc))
      (if (not ($constantp vi)) (push vi acc)))))

;; If e is a list, map $powers over the list.  If e is a sum of powers 
;; of powers of x, return a list of the exponents.

(defun $powers (e x)
  (require-symbol x "$powers" 2)
  (cond (($listp e)
	 (cons '(mlist simp) (mapcar #'(lambda (p) ($powers p x)) (cdr e))))
	(t
	 (setq e (require-poly (myrat e x) x "$powers" 1))
	 (cond (($ratfreeof x e)
		`((mlist simp) 0))
	       (t
		(cons '(mlist simp) (odds (cadr e) 0)))))))
	       
;; odds is defined in mactex.lisp. Here is its definition.

(defun odds (n c)
  ;; if c = 1, get the odd terms  (first, third...)
  (cond ((null n) nil)
	((= c 1) (cons (car n) (odds (cdr n) 0)))
	((= c 0) (odds (cdr n) 1))))

;; Return the highest power of the polynomial e in the variable x.

(defun $hipower (e x)
  (require-symbol x "$hipower" 2)
  (setq e (require-poly (myrat e x) x "$hipower" 1))
  (if (or ($constantp e) ($ratfreeof x e)) 0 (cadadr e)))

;; Return the lowest power of the polynomial e in the variable x.

(defun $lowpower (e x)
  (require-symbol x "$lowpower" 2)
  (setq e (require-poly (myrat e x) x "$lowpower" 1))
  (if (or ($constantp e) ($ratfreeof x e)) 0 (nth 1 (reverse (cadr e)))))

;; Flatten a Maxima list.

(defun flatten-list (e)
  (cond (($listp e)
	 (let ((acc))
	   (dolist (ei (cdr e) (cons '(mlist simp) (nreverse acc)))
	     (setq acc (if ($listp ei) (nconc (cdr (flatten-list ei)) acc)
			   (cons ei acc))))))
	(t e))) 

;; If e is a sum of powers of x, return a list of the coefficients
;; of powers of x.  When e isn't a sum of powers, return false.  This 
;; function is based on a Macsyma function written by A.D. Kennedy and 
;; referenced in "Mathematics and System Reference Manual," 16th edition, 
;; 1996.

(defun $allcoeffs (e x)
  (flatten-list (allcoeffs e x)))

(defun allcoeffs (e x)
  (cond (($listp e)
	 (cons '(mlist simp) (mapcar #'(lambda (s) (allcoeffs s x)) (cdr e))))
	(($listp x)
	 (cond ((= 0 ($length x)) e)
	       (t (allcoeffs (allcoeffs e ($first x)) ($rest x)))))
	(t 
	 (require-symbol x "$allcoeffs" 2)
	 (setq e (myrat e x))
	 (let ((p ($powers e x)))
	   (cons '(mlist simp) 
		 (mapcar #'(lambda (n) ($ratcoef e x n)) (cdr p)))))))

;; Return the coefficient of the term of the polynomial e that
;; contains the highest power of x. When x = [x1,x2,...,xn], return 
;; lcoeff(lcoeff( ... (lcoeff(e,x1),x2),...,xn)...)

(defun $lcoeff (e &optional v)
  (require-nonlist e "$lcoeff" 1)
  (if (null v) (setq v (non-constant-ratvars e)))
  (lcoeff (require-poly (myrat e) v "$lcoeff" 1) 
	  (require-list-of-symbols v "$lcoeff" 2)))
  
(defun lcoeff (e x)
  (if (null x) e (lcoeff ($ratcoef e (car x) ($hipower e (car x))) (cdr x))))

;; Return the coefficient of the term of the polynomial e that
;; contains the least power of x. When x = [x1,x2,...,xn], return 
;; lcoeff(lcoeff( ... (lcoeff(e,x1),x2),...,xn)...)

(defun $tcoeff (e &optional v)
  (require-nonlist e "$tcoeff" 1)
  (if (null v) (setq v (non-constant-ratvars e)))
  (tcoeff (require-poly (myrat e) v "$tcoeff" 1) 
	  (require-list-of-symbols v "$tcoeff" 2)))
  
(defun tcoeff (e x)
  (if (null x) e (tcoeff ($ratcoef e (car x) ($lowpower e (car x))) (cdr x))))

;; Return the degree of the symbol x in the polynomial p.  When
;; x is a list, degree(p, [x1,x2,...,xn]) returns
;;       degree(p,x1) + degree(lcoeff(p, x1),[x2,...xn]).  
;; Finally, degree(p,[]) returns 0.

(defun $degree (p x)
  (degree (require-poly (myrat p) x "$degree" 1) 
	  (require-list-of-symbols x "$degree" 2)))

(defun degree (p x)
  (if (null x) 0
      (add ($hipower p (car x)) (degree (lcoeff p `(,(car x))) (cdr x)))))

;; Return the total degree of the polynomial.  Four cases:
;;   (a) total_degree(p) returns the total degree of the polynomial
;;       in the variables listofvars(p).
;;   (b) total_degree(p,x), where x isn't a list returns the 
;;       total_degree of p in the variable x.
;;   (c) total_degree(p,[x1,x2,...,xn]), where x = [x1,x2,...,xn] 
;;       returns the total_degree of p in the variables x1 thru xn.
;;   (d) total_degree(p,x1,x2,...xn), where the x's are symbols
;;       returns the total_degree of p in the variables x1 thru xn.

(defun $total_degree (p &optional v)
  (if (null v) (setq v (non-constant-ratvars p)))
  (setq v (require-list-of-symbols v "$total_degree" 2))
  (total-degree (cadr (apply 'myrat `(,p ,@v)))))

(defun total-degree (e)
  (cond ((consp (nth 2 e))
	 (+ (nth 1 e) (total-degree (nth 2 e))))
	(t
	 (nth 1 e))))
	 
(defun $lcm (p q)
  (nth 1 ($divide (mul p q) ($gcd p q))))

;; Compute the s-polynomial of f and g.  For a definition of the 
;; s-polynomial, see Davenport, Siret, and  Tournier, "Computer Algebra," 
;; 1988, page 100.
 
(defun $spoly (f g v)
  (setq v (cons '(mlist simp) (require-list-of-symbols v "$spoly" 3)))
  (setq f (myrat f))
  (setq g (myrat g))
  (let ((fp ($lterm f v))
	(gp ($lterm g v)))
    (mul ($lcm fp gp) (add (div f fp) (mul -1 (div g gp))))))

(defun $lterm (p &optional v)
  (if (null v) (setq v (non-constant-ratvars p)))
  (lterm (require-poly (myrat p) v "$lterm" 1) 
	 (require-list-of-symbols v "$lterm" 2)))

(defun lterm (p v)
  (cond ((null v) p)
	(t
	 (let* ((vo (car v))
		(n ($hipower p vo)))
	   (lterm (mult ($ratcoef p vo n) (power vo n)) (cdr v))))))

(defun $get_exponents (p x)
  (setq x (require-list-of-symbols x "$get_exponents" 2))
  (let ((acc))
    (setq p (myrat p))
    (require-poly p (cons '(mlist simp) x) "$get_exponents" 1)
    (dolist (xi x (cons '(mlist simp) (nreverse acc)))
      (push ($hipower p xi) acc)
      (setq p ($lcoeff p xi)))))

;; Return true iff and only if e is a polynomial in the variables var.

(defun $polynomialp (e &optional vars cp ep)
  (declare (ignore cp ep))
  (if (null vars) (setq vars (non-constant-ratvars e)))
  (setq vars (require-list-of-symbols vars "$polynomialp" 2))
  (setq vars `((mlist simp) ,@vars))
  (and (every #'(lambda (x) (or ($variablep x) ($ratfreeof vars x) 
				($constantp x))) 
	      (cdr ($showratvars e)))
       (not ($taylorp e)) ($ratfreeof vars ($ratdenom e))))


     
  
  
