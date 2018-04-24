;;  Author Barton Willis
;;  University of Nebraska at Kearney
;;  Copyright (C) 2011 Barton Willis

;;  This program is free software; you can redistribute it and/or modify 
;;  it under the terms of the GNU General Public License as published by	 
;;  the Free Software Foundation; either version 2 of the License, or		 
;;  (at your option) any later version.					 
 		       								 
;;  This program is distributed in the hope that it will be useful,		 
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of		 
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the		 
;;  GNU General Public License for more details.

(in-package :maxima)

($load "bernstein_utilities.mac")

;; When bernstein_explicit is non-nil, bernstein_poly(k,n,x) simplifies to
;; binomial(n,k) * x^k (1-x)^(n-k) regardless of the values of k or n;

(defmvar $bernstein_explicit nil)

;; numerical (complex rational, float, or big float) evaluation of bernstein polynomials
(in-package #-gcl #:bigfloat #+gcl "BIGFLOAT")

(defun bernstein-poly (k n x)
  (* (to (maxima::opcons 'maxima::%binomial n k)) (expt x k) (expt (- 1 x) (- n k))))

(in-package :maxima)

(defun $bernstein_poly (k n x) (simplify (list '(%bernstein_poly) k n x)))
(defprop $bernstein_poly %bernstein_poly alias)
(defprop $bernstein_poly %bernstein_poly verb)
(defprop %bernstein_poly $bernstein_poly reversealias)
(defprop %bernstein_poly $bernstein_poly noun)

(setf (get '%bernstein_poly 'conjugate-function) 
      #'(lambda (e)
	  (let ((k (car e))
		(n (cadr e))
		(x (caddr e)))
	    (if (and ($featurep k '$integer) ($featurep n '$integer))
		(opcons '%bernstein_poly k n (opcons '$conjugate x))
	      `(($conjugate simp) ((%bernstein_poly simp) ,k ,n ,x))))))

;; integrate(bernstein_poly(k,n,x),x) = hypergeometric([k+1,k-n],[k+2],x)*x^(k+1)/(k+1)

(defun bernstein-integral (k n x)
  (div
   (mul 
    (opcons '%binomial n k)
    (opcons 'mexpt x (add 1 k))
    (opcons '$hypergeometric 
	    (opcons 'mlist (add 1 k) (sub k n))
	    (opcons 'mlist (add 2 k))
	    x))
   (add 1 k)))
	  
(putprop '%bernstein_poly `((k n x) nil nil ,#'bernstein-integral) 'integral)
(putprop '$bernstein_poly `((k n x) nil nil ,#'bernstein-integral) 'integral)

(defun bernstein-poly-simp (e y z)
  (declare (ignore y))
  (let* ((fn (car (pop e)))
	 (k (if (consp e) (simpcheck (pop e) z) (wna-err fn)))
	 (n (if (consp e) (simpcheck (pop e) z) (wna-err fn)))
	 (x (if (consp e) (simpcheck (pop e) z) (wna-err fn))))
    (if (consp e) (wna-err fn))
    (cond ((and (integerp k) (integerp n) (>= k 0) (>= n k)
		(complex-number-p x #'(lambda (s) (or (integerp s) ($ratnump s) (floatp s) ($bfloatp s)))))
	   (maxima::to (bigfloat::bernstein-poly (bigfloat::to k)  (bigfloat::to n) (bigfloat::to x))))

	  ((zerop1 x) (opcons '%kron_delta k 0))
	  
	  ((onep1 x) (opcons '%kron_delta k n))

	  ((or $bernstein_explicit (and (integerp k) (integerp n)))
	   (if (and (integerp k) (integerp n) (or (< k 0) (> k n))) (mul 0 x)
	     (mul (opcons '%binomial n k) (opcons 'mexpt x k) (opcons 'mexpt (sub 1 x) (sub n k)))))

	  (t (list (list fn 'simp) k n x)))))
	    
(setf (get '%bernstein_poly 'operators) #'bernstein-poly-simp)

(defprop %bernstein_poly
  ((k n x)

   ((mtimes) ((%bernstein_poly) k n x)
    ((mplus)
     ((mtimes) -1
      ((mqapply) (($psi array) 0) ((mplus) 1 k)))
     ((mqapply) (($psi array) 0)
      ((mplus) 1 ((mtimes) -1 k) n))
     ((mtimes) -1
      ((%log) ((mplus) 1 ((mtimes) -1 x))))
     ((%log) x)))

   ((mtimes)((%bernstein_poly) k n x)
    ((mplus)
     ((mqapply) (($psi array) 0) ((mplus) 1 n))
     ((mtimes) -1
      ((mqapply) (($psi array) 0)
       ((mplus) 1 ((mtimes) -1 k) n)))
     ((%log) ((mplus) 1 ((mtimes) -1 x))))) 
   
   ((mtimes)
    ((mplus)
     ((%bernstein_poly) ((mplus) -1 k) ((mplus) -1 n) x)
     ((mtimes) -1
      ((%bernstein_poly) k ((mplus) -1 n) x))) n))
  grad)
 
(defun $bernstein_approx (e vars n)
  (if (not ($listp vars)) (merror "The second argument to bernstein_approx must be a list"))

  (setq vars (margs vars))
  (if (some #'(lambda (s) (not ($mapatom s))) vars) 
      (merror "The second argument to bernstein_approx must be a list of atoms"))

  (if (or (not (integerp n)) (< n 1)) (merror "The third argument to bernstein_approx must be a positive integer"))

  (let* ((k (length vars))
	 (d (make-list k :initial-element 0))
	 (nn (make-list k :initial-element n))
	 (carry) (acc 0) (m) (x))
   
    (setq m (expt (+ n 1) k))
    (setq nn (cons '(mlist) nn))
    (dotimes (i m)
      (setq acc (add acc 
		     (mul
		      (opcons '%multibernstein_poly (cons '(mlist) d) nn (cons '(mlist) vars))
		      ($substitute (cons '(mlist) (mapcar #'(lambda (s x) (opcons 'mequal x (div s n))) d vars)) e))))
      (setq carry 1)
      (dotimes (j k)
	(setq x (+ carry (nth j d)))
	(if (> x n) (setq x 0 carry 1) (setq carry 0))
	(setf (nth j d) x)))
    acc))
  
(defun $multibernstein_poly (k n x) (simplify (list '(%multibernstein_poly) k n x)))
(defprop $multibernstein_poly %multibernstein_poly alias)
(defprop $multibernstein_poly %multibernstein_poly verb)
(defprop %multibernstein_poly $multibernstein_poly reversealias)
(defprop %multibernstein_poly $multibernstein_poly noun)

(defun multi-bernstein-poly-simp (e y z)
  (declare (ignore y))
  (let* 
      ((fn (car (pop e)))
       (k (if (consp e) (simpcheck (pop e) z) (wna-err fn)))
       (n (if (consp e) (simpcheck (pop e) z) (wna-err fn)))
       (x (if (consp e) (simpcheck (pop e) z) (wna-err fn))))
    (if (consp e) (wna-err fn))

    (if (or (not (and ($listp k) ($listp n) ($listp x))) (/= ($length k) ($length n)) (/= ($length n) ($length x)))
	(merror "Each argument to multibernstein_poly must be an equal length list"))

    (muln (mapcar #'(lambda (a b z) (opcons '%bernstein_poly a b z)) (margs k) (margs n) (margs x)) t)))

(setf (get '%multibernstein_poly 'operators) #'multi-bernstein-poly-simp)
      
