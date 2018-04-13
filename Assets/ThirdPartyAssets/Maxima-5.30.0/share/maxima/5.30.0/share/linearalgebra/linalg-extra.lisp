;; Copyright 2006 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

($put '$linalgextra 1 '$version)

(defun $circulant (lst)
  ($require_list lst "$first" "$circulant")
  (let ((q) (n ($length lst)))
    (setq lst (rest lst))
    (setq q (list lst))
    (decf n)
    (dotimes (i n)
      (setq lst `(,@(rest lst) ,(car lst)))
      (push lst q))
    (setq q (mapcar #'(lambda(s) (cons '(mlist) s)) q))
    (push '($matrix) q)))

(defun $cauchy_matrix (p &optional q)
  ($require_list p "$first" "$cauchy_matrix")
  (if q ($require_list q "$second" "$cauchy_matrix") (setq q p))
  (let ((row) (mat))
    (setq p (margs p))
    (setq q (margs q))
    (dolist (pj p)
      (setq row nil)
      (dolist (qj q)
	(push (div 1 (add pj qj)) row))
      (setq row (nreverse row))
      (push '(mlist) row)
      (push row mat))
    (setq mat (nreverse mat))
    (push '($matrix) mat)))

(defun $hessian (e vars)
  (cond (($listp vars)
	 (let ((z) (mat nil))
	   (setq vars (margs vars))
	   (dolist (vi vars)
	     (setq z ($diff e vi))
	     (push (cons '(mlist) (mapcar #'(lambda (s) ($diff z s)) vars)) mat))
	   (cons '($matrix) (reverse mat))))
	(t  `(($hessian) ,e ,vars))))
	    
(defun $jacobian (e vars)
  (cond ((and ($listp vars) ($listp e))
	 (setq e (margs e))
	 (setq vars (margs vars))
	 (let ((mat nil))
	   (dolist (ei e)
	     (push (cons '(mlist) (mapcar #'(lambda (s) ($diff ei s)) vars)) mat))
	   (cons '($matrix) (reverse mat))))
	(t `(($jacobian) ,e ,vars))))

(defun $vandermonde_matrix (l)
  (let ((x) (q) (row) (acc nil) (n))
    (setq l (require-list l "$vandermonde_matrix"))
    (setq n (- (length l) 1))
    (while l
      (setq q 1)
      (setq x (pop l)) 
      (setq row (list 1))
      (dotimes (j n)
	(setq q (mul q x))
	(push q row))
      (setq row (cons '(mlist) (nreverse row)))
      (push row acc))
    (simplify (cons '($matrix) (nreverse acc)))))
		    
;; Use Sylvester's criterion to decide if the self-adjoint part of a matrix is 
;; negative definite (neg) or positive definite (pos). For all other cases, return 
;; pnz. This algorithm is unable to determine if a matrix is negative semidefinite 
;; or positive semidefinite. The argument to this function must be a selfadjoint
;; matrix.

(defun matrix-sign-sylvester (m)
  (let ((n) (det) (p-sgn nil) (n-sgn nil))
    (setq n ($first ($matrix_size m)))
    
    (loop for i from 1 to n do
      (setq det (ratdisrep (newdet m i nil)))
      (push (csign det) p-sgn)
      (push (csign (mul (power -1 i) det)) n-sgn))
    
    (cond ((every #'(lambda (s) (eq s '$pos)) p-sgn) '$pos)
	  ((every #'(lambda (s) (eq s '$pos)) n-sgn) '$neg)
	  (t '$pnz))))

(defun order-of-root (p x pt)
  (let ((order 0))
    (setq p ($expand p))
    (while (and (alike 0 ($substitute pt x p)) (not (alike 0 p)))
      (incf order)
      (setq p ($expand ($diff p x))))
    order))
	
;; Let M be the self-adjoint part of mat. By the self-adjoint part 
;; of a matrix M, I mean (M + M^*) / 2, where ^* is the conjugate transpose
;; Return 

;;  (a) neg if M is negative definite,
;;  (b) nz if M is negative semidefinite,
;;  (c) pz if M is positive semidefinite,
;;  (d) pos if M is positive definite,
;;  (e) pnz if M isn't a--d or if Maxima isn't able determine the matrix sign.

;; When M is a matrix of rational numbers, look at the zeros of the characteristic polynomial;
;; otherwise, use Sylvester's criterion.

(defun $matrix_sign (mat)
  (let ((z (gensym)) (p) (n) (nbr-zero-roots) (nbr-neg-roots) (nbr-pos-roots))

    ($require_square_matrix mat '$first '$matrix_sign)
    (setq mat (div (add mat ($ctranspose mat)) 2))
    (cond (($some '((lambda) ((mlist) $s) ((mnot) (($ratnump) $s))) mat)
	   (matrix-sign-sylvester mat))
	  (t
	   (setq n ($first ($matrix_size mat)))
	   (cond (($zeromatrixp mat) '$zero)
		 (t
		  (setq p ($charpoly mat z))
		  
		  ;; number of roots of characteristic poly in {0}
		  (setq nbr-zero-roots (order-of-root p z 0))
		  
		  ;; number of roots of characteristic poly in (-inf,0)
		  (setq nbr-neg-roots (- ($nroots p '$minf 0) nbr-zero-roots))

		  ;; number of roots of characteristic poly in (0,inf)
		  (setq nbr-pos-roots ($nroots p 0 '$inf))
		  
		  (cond ((= n nbr-neg-roots) '$neg)
			((= 0 nbr-pos-roots) '$nz)
			((= n nbr-pos-roots) '$pos)
			((= n (+ nbr-pos-roots nbr-zero-roots)) '$pz)
			(t '$pnz))))))))

(defun $sylvester_matrix (p q z)
  (let ((p-coeff nil) (q-coeff nil) (mat nil) (p-deg) (q-deg))
    (if (or (not ($polynomialp p `((mlist) ,z) `((lambda) ((mlist) c) (($freeof) ,z c))))
	    (not ($polynomialp q `((mlist) ,z) `((lambda) ((mlist) c) (($freeof) ,z c)))))
	(merror "The first two arguments to 'sylvester_matrix' must be polynomials"))

    (setq p ($ratexpand p))
    (setq q ($ratexpand q))
   
    (setq p-deg ($hipow p z))
    (setq q-deg ($hipow q z))

    (dotimes (k (- q-deg 1))
      (push 0 p-coeff))
    
    (dotimes (k (+ p-deg 1))
      (push ($ratcoef p z k) p-coeff))

    (dotimes (k (- p-deg 1))
      (push 0 q-coeff))
    
    (dotimes (k (+ 1 q-deg))
      (push ($ratcoef q z k) q-coeff))
    
    (dotimes (k q-deg)
      (push (cons '(mlist) p-coeff) mat)
      (setq p-coeff (butlast p-coeff))
      (push 0 p-coeff))

    (dotimes (k p-deg)
      (push (cons '(mlist) q-coeff) mat)
      (setq q-coeff (butlast q-coeff))
      (push 0 q-coeff))
    
    (simplify `(($matrix) ,@(reverse mat)))))

(defun $krylov_matrix (v mat &optional (n 'no-value))
  ($require_matrix mat '$second '$krylov_matrix)
  (if (eq n 'no-value) (setq n ($first ($matrix_size mat))))
  ($require_posinteger n  '$third '$krylov_matrix)
  (let ((acc v))
    (decf n 1)
    (dotimes (k n)
      (setq v (ncmul mat v))
      (setq acc ($addcol acc v)))
    acc))
