;;  Copyright 2005 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

($put '$linalgutilities 2 '$version)

(defun inform (level pck msg &rest arg)
  (if (member level (member ($get '$infolevel pck) `($debug $verbose $silent)))
      (apply 'mtell `(,msg ,@arg))))

(defun $require_nonempty_matrix (m pos fun)
  (if (not (and ($matrixp m) (> ($length m) 0) (> ($length ($first m)) 0)))
      (merror "The ~:M argument of the function ~:M must be a nonempty matrix" pos fun)))

;; Why both some and every? Because we want blockmatrixp(matrix()) --> false.

(defun $blockmatrixp (m) 
  (and ($matrixp m) ($some '$matrixp m) ($every '$matrixp m)))

(defun $require_matrix (m pos fun)
  (if (not ($matrixp m))
      (merror "The ~:M argument of the function ~:M must be a matrix" pos fun)))

(defun $require_unblockedmatrix (m pos fun)
  (if (or (not ($matrixp m)) ($blockmatrixp m))
      (merror "The ~:M argument of the function ~:M must be an unblocked matrix" pos fun)))

(defun $require_square_matrix (m pos fun)
  (if (not (and ($matrixp m) (= ($length m) ($length ($first m)))))
      (merror "The ~:M argument of the function ~:M must be a square matrix" pos fun)))

(defun array-elem (m i j)
  (nth j (nth i m)))

(defun $require_symmetric_matrix (m pos fun)
  (if (not ($matrixp m)) (merror "The ~:M argument to ~:M must be a matrix" pos fun))
  (let ((n ($matrix_size m)))
    (if (not (= ($first n) ($second n)))
	(merror "The ~:M argument to ~:M must be a square matrix" pos fun))
    (if (not ($zeromatrixp (sub m ($transpose m))))
	(merror "The ~:M argument to ~:M must be a symmetric matrix" pos fun)))
  '$done)

(defun $require_real_symmetric_matrix (m pos fun)
  (if (not ($matrixp m)) (merror "The ~:M argument to ~:M must be a matrix" pos fun))
  (let ((n ($matrix_size m)))
    (if (not (= ($first n) ($second n)))
	(merror "The ~:M argument to ~:M must be a square matrix" pos fun))
    (if (and ($zeromatrixp (sub m ($transpose m))) ($zeromatrixp (sub m (take '($conjugate) m)))) '$done
      (merror "The ~:M argument to ~:M must be a real symmetric matrix" pos fun))))
 
(defun $require_selfadjoint_matrix (m fun pos)
  (if (not ($matrixp m)) (merror "The ~:M argument to ~:M must be a matrix" pos fun))
  (let ((n ($matrix_size m)))
    (if (not (= ($first n) ($second n)))
	(merror "The ~:M argument to ~:M must be a square matrix" pos fun))
    (if (not ($zeromatrixp (sub m ($ctranspose m))))
	(merror "The ~:M argument to ~:M must be a selfadjoint (hermitian) matrix" pos fun)))
  '$done)

;; matrix() is a 0 x 0 matrix, and matrix([]) is a 1 x 0 matrix.
;; There is no representation for a 0 x 1 matrix. Currently,
;; transpose(matrix([])) => matrix(). And that's a bug. 

(defun $matrix_size(m)
  ($require_matrix m "$first" "$matrix_size")
  `((mlist) ,($length ($args m)) ,(if ($emptyp ($args m)) 0 ($length ($first ($args m))))))
  
(defun $require_list (lst pos fun)
  (if (not ($listp lst))
      (merror "The ~:M argument of the function ~:M must be a list" pos fun)))

(defun $require_posinteger(i pos fun)
  (if (not (and (integerp i) (> i 0)))
      (merror "The ~:M argument of the function ~:M must be a positive integer" pos fun)))

(defun matrix-map (f mat)
  (setq mat (mapcar 'cdr (cdr mat)))
  (setq mat (mapcar #'(lambda (s) (mapcar f s)) mat))
  (setq mat (mapcar #'(lambda (s) (cons `(mlist simp) s)) mat))
  `(($matrix simp) ,@mat))  

;; Map the lisp function fn over the matrix m. This function is block matrix friendly.

(defun full-matrix-map (fn m)
  (if (or ($listp m) ($matrixp m))
      (cons (car m) (mapcar #'(lambda (s) (full-matrix-map fn s)) (cdr m)))
    (funcall fn m)))

(defun $zerofor (mat &optional (fld-name '$generalring))
  (let* ((fld ($require_ring fld-name "$second" "$zerofor"))
	(add-id (funcall (mring-mring-to-maxima fld) (funcall (mring-add-id fld)))))
    (zerofor mat add-id)))

(defun zerofor (mat zero)
  (if (or ($matrixp mat) ($listp mat))
      (cons (car mat) (mapcar #'(lambda (s) (zerofor s zero)) (cdr mat)))
    zero))

;; Return an identity matrix that has the same shape as the matrix
;; mat. The first argument 'mat' should be a square Maxima matrix or a 
;; non-matrix. When 'mat' is a matrix, each entry of 'mat' can be a
;; square matrix -- thus 'mat' can be a blocked Maxima matrix. The
;; matrix can be blocked to any (finite) depth.

(defun $identfor (mat &optional (fld-name '$generalring))
  (let* ((fld ($require_ring fld-name "$second" "$zerofor"))
	 (add-id (funcall (mring-mring-to-maxima fld) (funcall (mring-add-id fld))))
	 (mult-id (funcall (mring-mring-to-maxima fld) (funcall (mring-mult-id fld)))))
    (if ($matrixp mat) (identfor mat add-id mult-id) mult-id)))

(defun identfor (mat zero one)
  (let ((i) (acc) (j) (new-mat))
    (setq mat (rest mat))
    (setq i 0)
    (dolist (row mat)
      (setq row (rest row))
      (setq acc nil)
      (setq j 0)
      (dolist (aij row)
	(push (cond (($matrixp aij)
		     (if (= i j) (identfor aij zero one) (zerofor aij zero)))
		    ((= i j) one)
		    (t zero)) acc)
	(incf j))
      (incf i)
      (push '(mlist) acc)
      (push acc new-mat))
    (push '($matrix) new-mat)))

(defun $ctranspose (m)
  (mfuncall '$transpose (full-matrix-map #'(lambda (s) (simplifya `(($conjugate) ,s) nil)) m)))
 
(defun $zeromatrixp (m)
  (if (or ($matrixp m) ($listp m)) (every '$zeromatrixp (cdr m))
    (eq '$zero (csign ($rectform m)))))
	
(defun array-to-row-list (mat &optional (fn 'identity))
  (let ((acc) (r (array-dimensions mat)) (row) (c))
    (setq c (second r))
    (setq r (first r))
    (dotimes (i r)
      (setq row nil)
      (dotimes (j c)
	(push (funcall fn (aref mat i j)) row))
      (setq row (reverse row))
      (push row acc))
    (reverse acc)))
  
(defun array-to-maxima-matrix (mat &optional (fn 'identity))
  (cons '($matrix) (mapcar #'(lambda (s) (cons '(mlist) s)) (array-to-row-list mat fn))))

(defun array-to-maxima-list (ar &optional (fn 'identity))
  (cons '(mlist) (mapcar fn (coerce ar 'list))))
  
(defun maxima-to-array (mat &optional (fn 'identity) typ)
  (let ((r ($matrix_size mat)) (c))
    (setq c ($second r))
    (setq r ($first r))
    (setq mat (mapcar #'cdr (cdr mat)))
    (setq mat (mapcar #'(lambda (s) (mapcar #'(lambda (w) (funcall fn w)) s)) mat))
    (if typ (make-array (list r c) :element-type typ :initial-contents mat)
      (make-array (list r c) :initial-contents mat))))
