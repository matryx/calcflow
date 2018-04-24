;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;

(in-package :maxima)

(defmfun $adjoint (mat)
  (let* ((n ($length mat))
	 (adj (simplify ($ident n))))
    (unless (like n 1)
      (do ((i 1 (1+ i)))
	  ((> i n))
	(do ((j 1 (1+ j)))
	    ((> j n))
	  (maset (mul* (power -1 (+ i j))
		       (simplify ($determinant (simplify ($minor mat j i)))))
		 adj i j))))
    adj))

(add2lnc '$adjoint $props)

(defun $invert (m &optional (field-name (if $ratmx '$crering '$generalring)))
  (declare (special $ratmx $detout))
  ;; Call functions from package linearalgebra via MFUNCALL to autoload them if necessary.
  (if $detout
    (let*
      ((field (mfuncall '$require_ring field-name "$second" "$invert"))
       (d-i (invert-by-lu-with-determinant m field-name))
       (d (first d-i))
       (i (second d-i))
       (d-times-i (multiply-matrix-elements d (mring-mult field) i))
       (d^-1 (funcall (mring-reciprocal field) d)))
      (list '(mtimes) d^-1 d-times-i))
    (mfuncall '$invert_by_lu m field-name)))

;; I wonder if this function already exists somewhere. Oh well.
(defun multiply-matrix-elements (a multiply m)
  (cons (car m) (mapcar #'(lambda (row) (cons (car row) (mapcar #'(lambda (x) (funcall multiply a x)) (cdr row)))) (cdr m))))
