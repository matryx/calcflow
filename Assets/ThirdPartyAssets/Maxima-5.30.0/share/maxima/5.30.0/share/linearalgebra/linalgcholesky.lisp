;;  Copyright 2005, 2006 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

($put '$cholesky 1 '$version)
	(defun $cholesky (m &optional (fld-name '$generalring))
  ($require_nonempty_matrix m "$first" "$cholesky")
  ($require_selfadjoint_matrix m "$first" "$cholesky")

  (let* ((n ($first ($matrix_size m))) (lii) (lii-inv) (l) (acc) (mat) (row)
	 (fld ($require_ring fld-name "$second" "$cholesky")))
	 ;;(zero (funcall (mring-add-id fld))))

    (flet
	((fzerop (a) (funcall (mring-fzerop fld) a))
	 (fpsqrt (a) (funcall (mring-psqrt fld) a))
	 (fsub (a b) (funcall (mring-sub fld) a b))
	 (fmult (a b) (funcall (mring-mult fld) a b))
	 (fadjoint (a) (funcall (mring-adjoint fld) a))
	 (freciprocal (a) (funcall (mring-reciprocal fld) a))
	 (frevert (a) (funcall (mring-mring-to-maxima fld) a))
	 (fconvert (a) (funcall (mring-maxima-to-mring fld) a)))
      
      (setq l ($zerofor m))
      (setq l (make-array (list n n) :initial-contents (mapcar #'rest (margs l))))
      (setq mat (make-array (list n n) :initial-contents (mapcar #'rest (margs m))))
      
      (decf n)
      
      ;; Convert each entry of mat to a ring member.

      (loop for i from 0 to n do
	(loop for j from 0 to n do
	  (setf (aref mat i j) (fconvert (aref mat i j)))))

	 
      (loop for i from 0 to n do 
	(setq acc (aref mat i i))
	(loop for k from 0 to (- i 1) do 
	  (setq acc (fsub acc (fmult (aref l i k) (fadjoint (aref l i k))))))
     
	(setq lii (if ($matrixp acc) ($cholesky acc fld-name) (fpsqrt acc)))
	(if (null lii) (merror "Unable to find the Cholesky factorization"))
	(setf (aref l i i) lii)
	(cond ((<= (+ i 1) n)
	       (if (fzerop lii) (merror "Unable to find the Cholesky factorization"))
	       (setq lii-inv (fadjoint (freciprocal lii)))))
	(loop for j from (+ i 1) to n do
	  (setq acc (aref mat j i))
	  (loop for k from 0 to (- i 1) do
	    (setq acc (fsub acc (fmult (aref l j k) (fadjoint (aref l i k))))))
	  (setf (aref l j i) (fmult acc lii-inv))))

      (setq mat nil)
      (loop for i from 0 to n do 
	(setq row nil)
	(loop for j from 0 to n do
	  (push (full-matrix-map  #'frevert (aref l i j)) row))
	(setq row (reverse row))
	(push '(mlist) row)
	(push row mat))
      (setq mat (reverse mat))
      (push '($matrix) mat))))
 	
	
