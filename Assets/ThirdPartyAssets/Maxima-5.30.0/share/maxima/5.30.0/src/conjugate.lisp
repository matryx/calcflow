;;  Copyright 2005, 2006 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(in-package :maxima)

(macsyma-module conjugate)

($put '$conjugate 1 '$version)

(defprop $conjugate tex-postfix tex)
(defprop $conjugate ("^\\star") texsym)
(defprop $conjugate 160. tex-lbp)
(defprop $conjugate simp-conjugate operators)

(eval-when
    #+gcl (load eval)
    #-gcl (:load-toplevel :execute)
    (let (($context '$global) (context '$global))
      (meval '(($declare) $conjugate $complex))))

;; When a function commutes with the conjugate, give the function the
;; commutes-with-conjugate property. The log function commutes with
;; the conjugate on all of C except on the negative real axis. Thus
;; log does not get the commutes-with-conjugate property.  Instead,
;; log gets the conjugate-function property.

;; What importation functions have I missed?

;; (1) Arithmetic operators

(setf (get 'mplus 'commutes-with-conjugate) t)
(setf (get 'mtimes 'commutes-with-conjugate) t)
;(setf (get 'mnctimes 'commutes-with-conjugate) t) ;; generally I think users will want this

;; Trig-like functions and other such functions

(setf (get '%cosh 'commutes-with-conjugate) t)
(setf (get '%sinh 'commutes-with-conjugate) t)
(setf (get '%tanh 'commutes-with-conjugate) t)
(setf (get '%sech 'commutes-with-conjugate) t)
(setf (get '%csch 'commutes-with-conjugate) t)
(setf (get '%coth 'commutes-with-conjugate) t)
(setf (get '%cos 'commutes-with-conjugate) t)
(setf (get '%sin 'commutes-with-conjugate) t)
(setf (get '%tan 'commutes-with-conjugate) t)
(setf (get '%sec 'commutes-with-conjugate) t)
(setf (get '%csc 'commutes-with-conjugate) t)
(setf (get '%cot 'commutes-with-conjugate) t)
(setf (get '$atan2 'commutes-with-conjugate) t)

(setf (get '%jacobi_cn 'commutes-with-conjugate) t)
(setf (get '%jacobi_sn 'commutes-with-conjugate) t)
(setf (get '%jacobi_dn 'commutes-with-conjugate) t)

(setf (get '%gamma 'commutes-with-conjugate) t)
(setf (get '$pochhammer 'commutes-with-conjugate) t)

;; Collections

(setf (get '$matrix 'commutes-with-conjugate) t)
(setf (get 'mlist 'commutes-with-conjugate) t)
(setf (get '$set 'commutes-with-conjugate) t)

;; Relations

(setf (get 'mequal 'commutes-with-conjugate) t)
(setf (get 'mnotequal 'commutes-with-conjugate) t)
(setf (get '%transpose 'commutes-with-conjugate) t)

;; Oddball functions

(setf (get '$max 'commutes-with-conjugate) t)
(setf (get '$min 'commutes-with-conjugate) t)

;; When a function has the conjugate-function property,
;; use a non-generic function to conjugate it. Not done:
;; conjugate-functions for all the inverse trigonometric
;; functions.

;; Trig like and hypergeometric like functions

(setf (get '%log 'conjugate-function) 'conjugate-log)
(setf (get 'mexpt 'conjugate-function) 'conjugate-mexpt)
(setf (get '%asin 'conjugate-function) 'conjugate-asin)
(setf (get '%acos 'conjugate-function) 'conjugate-acos)
(setf (get '%atan 'conjugate-function) 'conjugate-atan)
(setf (get '%atanh 'conjugate-function) 'conjugate-atanh)

;;(setf (get '$asec 'conjugate-function) 'conjugate-asec)
;;(setf (get '$acsc 'conjugate-function) 'conjugate-acsc)
(setf (get '%bessel_j 'conjugate-function) 'conjugate-bessel-j)
(setf (get '%bessel_y 'conjugate-function) 'conjugate-bessel-y)
(setf (get '%bessel_i 'conjugate-function) 'conjugate-bessel-i)
(setf (get '%bessel_k 'conjugate-function) 'conjugate-bessel-k)

;; Other things:

(setf (get '%sum 'conjugate-function) 'conjugate-sum)
(setf (get '%product 'conjugate-function) 'conjugate-product)

;; Return true iff Maxima can prove that z is not on the
;; negative real axis.

(defun off-negative-real-axisp (z)
  (setq z (trisplit z))	          ; split into real and imaginary
  (or (eq t (mnqp (cdr z) 0))     ; y #  0
      (eq t (mgqp (car z) 0))))   ; x >= 0

(defun on-negative-real-axisp (z)
  (setq z (trisplit z))
  (and (eq t (meqp (cdr z) 0))
       (eq t (mgrp 0 (car z)))))

(defun in-domain-of-asin (z)
  (setq z (trisplit z))
  (let ((x (car z)) (y (cdr z)))
    (or (eq t (mgrp y 0))
	(eq t (mgrp 0 y))
	(and
	 (eq t (mgrp x -1))
	 (eq t (mgrp 1 x))))))

;; Return conjugate(log(x)). Actually, x is a lisp list (x).

(defun conjugate-log (x)
  (setq x (car x))
  (cond ((off-negative-real-axisp x)
	 (take '(%log) (take '($conjugate) x)))
	((on-negative-real-axisp x)
	 (add (take '(%log) (neg x)) (mul -1 '$%i '$%pi)))
	(t `(($conjugate simp) ((%log simp) ,x)))))

;; Return conjugate(x^p), where e = (x, p). Suppose x isn't on the negative real axis.
;; Then conjugate(x^p) == conjugate(exp(p * log(x))) == exp(conjugate(p) * conjugate(log(x)))
;; == exp(conjugate(p) * log(conjugate(x)) = conjugate(x)^conjugate(p). Thus, when
;; x is off the negative real axis, commute the conjugate with ^. Also if p is an integer
;; ^ commutes with the conjugate.

(defun conjugate-mexpt (e)
  (let ((x (first e)) (p (second e)))
    (if (or (off-negative-real-axisp x) ($featurep p '$integer))
	(power (take '($conjugate) x) (take '($conjugate) p))
      `(($conjugate simp) ,(power x p)))))

(defun conjugate-sum (e)
  (take '(%sum) (take '($conjugate) (first e)) (second e) (third e) (fourth e)))

(defun conjugate-product (e)
  (take '(%product) (take '($conjugate) (first e)) (second e) (third e) (fourth e)))

(defun conjugate-asin (x)
  (setq x (car x))
  (if (in-domain-of-asin x) (take '(%asin) (take '($conjugate) x))
    `(($conjugate simp) ((%asin) ,x))))

(defun conjugate-acos (x)
  (setq x (car x))
  (if (in-domain-of-asin x) (take '(%acos) (take '($conjugate) x))
    `(($conjugate simp) ((%acos) ,x))))

(defun conjugate-atan (x)
  (let ((xx))
    (setq x (car x))
    (setq xx (mul '$%i x))
    (if (in-domain-of-asin xx)
        (take '(%atan) (take '($conjugate) x))
        `(($conjugate simp) ((%atan) ,x)))))

;; atanh and asin are entire on the same set; see A&S Fig. 4.4 and 4.7.

(defun conjugate-atanh (x)
  (setq x (car x))
  (if (in-domain-of-asin x) (take '(%atanh) (take '($conjugate) x))
    `(($conjugate simp) ((%atanh) ,x))))

;; Integer order Bessel functions are entire; thus they commute with the
;; conjugate (Schwartz refection principle).  But non-integer order Bessel
;; functions are not analytic along the negative real axis. Notice that A&S
;; 9.1.40 isn't correct -- it says that the real order Bessel functions
;; commute with the conjugate. Not true.

(defun conjugate-bessel-j (z)
  (let ((n (first z)) (x (second z)))
    (if (off-negative-real-axisp x)
        (take '(%bessel_j) (take '($conjugate) n) (take '($conjugate) x))
       `(($conjugate simp) ((%bessel_j simp) ,@z)))))

(defun conjugate-bessel-y (z)
  (let ((n (first z)) (x (second z)))
    (if (off-negative-real-axisp x)
        (take '(%bessel_y) (take '($conjugate) n) (take '($conjugate) x))
       `(($conjugate simp) ((%bessel_y simp) ,@z)))))

(defun conjugate-bessel-i (z)
  (let ((n (first z)) (x (second z)))
    (if (off-negative-real-axisp x)
        (take '(%bessel_i) (take '($conjugate) n) (take '($conjugate) x))
       `(($conjugate simp) ((%bessel_i simp) ,@z)))))

(defun conjugate-bessel-k (z)
  (let ((n (first z)) (x (second z)))
    (if (off-negative-real-axisp x)
        (take '(%bessel_k) (take '($conjugate) n) (take '($conjugate) x))
       `(($conjugate simp) ((%bessel_k simp) ,@z)))))

;; When a function maps "everything" into the reals, put real-valued on the
;; property list of the function name. This duplicates some knowledge that
;; $rectform has. So it goes. The functions floor, ceiling, carg, and signum
;; aren't defined off the real-axis. I suppose these functions could be given the
;; real-valued property.

(setf (get '%imagpart 'real-valued) t)
(setf (get 'mabs 'real-valued) t)
(setf (get '%realpart 'real-valued) t)
(setf (get '%signum 'real-valued) t)

;; manifestly-real-p isn't a great name, but it's OK. Since (manifestly-real-p '$inf) --> true
;; it might be called manifestly-extended-real-p. A nonscalar isn't real.

;; There might be some advantage to requiring that the subscripts to a $subvarp
;; all be real.  Why? Well li[n] maps reals to reals when n is real, but li[n] does
;; not map the reals to reals when n is nonreal.

(defun manifestly-real-p (e)
  (let (($inflag t))
    (and ($mapatom e)
	 (not (manifestly-pure-imaginary-p e))
	 (not (manifestly-complex-p e))
	 (not (manifestly-nonreal-p e))
	 (or
	  ($numberp e)
	  (symbolp e)
	  (and ($subvarp e) (manifestly-real-p ($op e)))))))

(defun manifestly-pure-imaginary-p (e)
  (let (($inflag t))
    (or 
     (and ($mapatom e)
	  (or
	   (eq e '$%i)
	   (and (symbolp e) (kindp e '$imaginary) (not ($nonscalarp e)))
	   (and ($subvarp e) (manifestly-pure-imaginary-p ($op e)))))
     ;; For now, let's use $csign on constant expressions only; once $csign improves,
     ;; the ban on nonconstant expressions can be removed
     (and ($constantp e) (eq '$imaginary ($csign e))))))

;; Don't use (kindp e '$complex)!

(defun manifestly-complex-p (e)
  (let (($inflag t))
    (or (and (symbolp e) (decl-complexp e) (not ($nonscalarp e)))
	(eq e '$infinity)
	(and ($subvarp e) (manifestly-complex-p ($op e))
	     (not ($nonscalarp e))))))

(defun manifestly-nonreal-p (e)
  (and (symbolp e) (or (member e `($und $ind $zeroa $zerob t nil)) ($nonscalarp e))))

;; For a subscripted function, conjugate always returns the conjugate noun-form.
;; This could be repaired. For now, we don't have a scheme for conjugate(li[m](x)).

;; We could make commutes_with_conjugate and maps_to_reals features. But I
;; doubt it would get much use.

(defun simp-conjugate (e f z)
  (oneargcheck e)
  (setq e (simpcheck (cadr e) z))	; simp and disrep if necessary
  (cond ((complexp e) (conjugate e))    ; never happens, but might someday.
	((manifestly-real-p e) e)
	((manifestly-pure-imaginary-p e) (mul -1 e))
	((manifestly-nonreal-p e) `(($conjugate simp) ,e))
	(($mapatom e) `(($conjugate simp) ,e))
	((op-equalp e '$conjugate) (car (margs e)))

	((and (symbolp (mop e)) (get (mop e) 'real-valued)) e)

	((and (symbolp (mop e)) (get (mop e) 'commutes-with-conjugate))
	 (simplify (cons (list (mop e)) (mapcar #'(lambda (s) (take '($conjugate) s)) (margs e)))))

	((setq f (and (symbolp (mop e)) (get (mop e) 'conjugate-function)))
	 (funcall f (margs e)))

	(t `(($conjugate simp) ,e))))
