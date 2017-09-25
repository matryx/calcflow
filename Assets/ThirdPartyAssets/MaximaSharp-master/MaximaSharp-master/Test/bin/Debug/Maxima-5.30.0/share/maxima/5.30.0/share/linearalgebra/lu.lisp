;;  Copyright 2005, 2006 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

($put '$lu 2 '$version)
	
;; Return the i,j entry of the Maxima matrix m. The rows of m have been permuted according
;; to the Maxima list p.

(defun m-elem (m p i j)
  (if (arrayp m) (aref m (aref p i) j) (nth j (nth (nth i p) m))))

;; Set the i j entry of the Maxima matrix or an array to x.

(defun setmatelem (m x i j) 
  (if (arrayp m) (setf (aref m i j) x) (setf (nth j (nth i m)) x)))

;; Return m[perm[i], k] - sum(m[perm[i],s] * m[perm[s],k],s,0,n)

(defun partial-matrix-prod (m p i k n fadd fsub fmult add-id fname)
  (cond ((eq fname '$floatfield)
	 (partial-matrix-prod-float m p i k n))
	((eq fname '$complexfield)
	 (partial-matrix-prod-complex-float m p i k n))
	(t
	 (let ((l (aref p i)))
	   (loop for s from 0 to n do
	     (setq add-id (funcall fadd add-id (funcall fmult (aref m l s) (aref m (aref p s) k)))))
	   (setf (aref m l k) (funcall fsub (aref m l k) add-id))))))

(defun partial-matrix-prod-float (m p i k n)
  (let ((add-id 0.0))
    (declare (type flonum add-id))
    (let ((l (aref p i)))
      (loop for s from 0 to n do
	(setq add-id (+ add-id (* (aref m l s) (aref m (aref p s) k)))))
      (setf (aref m l k) (- (aref m l k) add-id)))))

(defun partial-matrix-prod-complex-float (m p i k n)
  (let ((add-id 0.0))
    (let ((l (aref p i)))
      (loop for s from 0 to n do
	(setq add-id (+ add-id (* (aref m l s) (aref m (aref p s) k)))))
      (setf (aref m l k) (- (aref m l k) add-id)))))

;; Return the infinity norm (the largest row sum) of the r by c array mat. The function
;; fn coerces matrix elements into flonum floats. The argument 'mat' is a Maxima
;; style matrix; thus mat = (($matrix) ((mlist) a b c) etc).

(defun array-infinity-norm (mat fn)
  (reduce #'max (mapcar #'(lambda (s) (reduce #'+ s)) 
			(array-to-row-list mat #'(lambda (s) (abs (funcall fn s)))))))

(defun $lu_factor (mat &optional (fld '$generalring))
  ($require_square_matrix mat "$first" "$lu_factor")
  ($require_nonempty_matrix mat "$first" "$lu_factor")
  (setq fld ($require_ring fld "$second" "$lu_factor"))
  
  (let* ((c ($length mat)) (perm (make-array c)) (cnd) (fn))
    ;(setq mat (full-matrix-map (mring-maxima-to-mring fld) mat))
    (setq mat (maxima-to-array mat (mring-maxima-to-mring fld)))
    
    (decf c)
    (loop for i from 0 to c do (setf (aref perm i) i))   
    
    ;; When the matrix elements can be converted to CL floats, find
    ;; the infinity norm of m. The norm is used to bound the condition
    ;; number.

    (cond ((not (eq nil (mring-coerce-to-lisp-float fld)))
	   (setq fn (mring-coerce-to-lisp-float fld))
	   (setq cnd (array-infinity-norm mat fn)))
	  (t (setq cnd 0.0)))
    (lu-factor mat perm c fld cnd)))

(defun $get_lu_factors (x)
  (let ((mat ($first x)) (mp) (p ($second x)) (perm) (r) (c) (id) (lower) (upper) (zero))
 
    (setq r ($matrix_size mat))
    (setq c ($second r))
    (setq r ($first r))
    (setq id ($args ($identfor mat)))
    (loop for i from 1 to c do
      (push (nth (nth i p) id) perm)
      (push (nth (nth i p) mat) mp))
    (setq perm (reverse perm))
    (setq mp (reverse mp))
    (push '($matrix) perm)
    (push '($matrix) mp)

    (setq lower (copy-tree mp))
    (setq upper (copy-tree mp))
    (setq id ($identfor ($first ($first mat))))
    (setq zero ($zerofor ($first ($first mat))))
    (loop for i from 1 to r do
      (loop for j from 1 to c do
	(if (= i j) (setmatelem lower id i j))
	(if (> i j) (setmatelem upper zero i j))
	(if (< i j) (setmatelem lower zero i j))))	
    `((mlist) ,($transpose perm) ,lower ,upper)))
        
(defun lu-factor (m perm c fld &optional (cnd 1.0))
  (let ((pos) (kp1) (mx) (lb) (ub) (save) (fname (mring-name fld)) 
	(add-id (funcall (mring-add-id fld))))
    (flet
	((fzerop (a) (funcall (mring-fzerop fld) a))
	 (fadd (a b) (funcall (mring-add fld) a b))
	 (fsub (a b) (funcall (mring-sub fld) a b))
	 (fabs (a) (funcall (mring-abs fld) a))
	 (fmult (a b) (funcall (mring-mult fld) a b))
	 (fdiv (a b) (funcall (mring-div fld) a b))
	 (fgreat (a b) (funcall (mring-great fld) a b)))
      
      (loop for k from 0 to c do 
	(loop for i from k to c  do (partial-matrix-prod m perm i k (- k 1) 
							 #'fadd #'fsub #'fmult add-id fname))
	(setq mx (fabs (m-elem m perm k k)))
	(setq pos k)
	(loop for s from k to c do
	  (if (fgreat (fabs (m-elem m perm s k)) mx) (setq pos s mx (fabs (m-elem m perm s k)))))

	(setq save (aref perm k))
	(setf (aref perm k) (aref perm pos))
	(setf (aref perm pos) save)

	(setq kp1 (+ 1 k))
	(loop for i from kp1 to c do
	  (if (fzerop (m-elem m perm k k)) (merror "Unable to compute the LU factorization"))
	  (setmatelem m (fdiv (m-elem m perm i k) (m-elem m perm k k)) (aref perm i) k)
	  (partial-matrix-prod m perm k i (- k 1) #'fadd #'fsub #'fmult add-id fname)))
      
      (cond ((not (eq nil (mring-coerce-to-lisp-float fld)))
	     (multiple-value-setq (lb ub) (mat-cond-by-lu m perm c (mring-coerce-to-lisp-float fld)))
	     (setq m (array-to-maxima-matrix m (mring-mring-to-maxima fld)))
	     (setq lb ($limit (mul lb cnd)))
	     (setq ub ($limit (mul ub cnd)))
	     (setq perm (array-to-maxima-list perm #'(lambda (s) (+ s 1))))
	     `((mlist) ,m ,perm ,(mring-name fld) ,lb ,ub))
	    (t 
	     (setq perm (array-to-maxima-list perm #'(lambda (s) (+ s 1))))
	     (setq m (array-to-maxima-matrix m (mring-mring-to-maxima fld)))
	     `((mlist) ,m ,perm ,(mring-name fld)))))))
      
;; The first argument should be a matrix in packed LU form. The Maxima list perm
;; specifies the true row ordering. When r is false, reflect the matrix horizontally
;; and vertically.

(defun m-elem-reflect (mat perm n r i j)
  (cond ((and r (= i j)) 1)
	;;(r (m-elem mat perm (+ n (- i) 1) (+ n (- j) 1)))
	(r (m-elem mat perm (+ n (- i)) (+ n (- j))))
	(t (m-elem mat perm i j))))

;; The first argument mat should be a matrix in the packed LU format. 
;; When l-or-u is lower, return the i,j element of the lower triangular
;; factor; when l-or-u is upper, return the j, i element of upper triangular
;; factor. The first argument mat should be a matrix in the packed LU format. 

(defun mat-cond-by-lu (mat perm n fn)
  (let ((lb0) (ub0) (lb1) (ub1))
    (multiple-value-setq (lb0 ub0) (triangular-mat-cond mat perm n fn t))
    (multiple-value-setq (lb1 ub1) (triangular-mat-cond mat perm n fn nil))
    (values ($limit (mul lb0 lb1)) ($limit (mul ub0 ub1)))))

;; Return lower and upper bounds for the infinity norm condition number of the lower or
;; upper triangular part of the matrix mat. The function fn coerces the matrix
;; elements to flonum floats.  When the matrix is singular, return infinity.
;; This code is based on pseudo-code (algorithm 2.1) in ``Survey of condition 
;; number estimation,'' by Nicholas J. Higham, SIAM Review, Vol. 29, No. 4, December,
;; 1987.  The lower and upper bounds can differ from the true value by arbitrarily 
;; large factors. 

(defun triangular-mat-cond (mat perm n fn r)
  (let ((z) (d-max 0.0) (z-max 0.0) (s) (d))
    (setq z (make-array (+ 1 n)))
   
    (catch 'singular
      (loop for i from n downto 0 do
	(setq d (abs (funcall fn (m-elem-reflect mat perm n r i i))))
	(if (= 0.0 d) (throw 'singular (values '$inf '$inf)) (setq d (/ 1 d)))
	(setq d-max (max d-max (abs (funcall fn d))))
	(setq s 1.0)
	(loop for j from (+ 1 i) to n do
	  (incf s (* (abs (funcall fn (m-elem-reflect mat perm n r i j))) (aref z j))))
	(setf (aref z i) (* d s))
	(setq z-max (max z-max (aref z i))))
      (values d-max z-max))))
      
(defun $lu_backsub(m b1)
  ($require_list m "$first" "$lu_backsub")
  (if (< ($length m) 3) (merror "The first argument to 'lu_backsub' must be a list with at least 3 members"))
  
  (let* ((mat) (n) (r) (c) (bb) (acc) (perm) (id-perm) (b) 
	 (fld (get (mfuncall '$ev (fourth m)) 'ring)) (cc) 
	 (fadd (mring-add fld))
	 (fsub (mring-sub fld))
	 (fmult (mring-mult fld))
	 (fdiv (mring-rdiv fld))
	 (add-id (funcall (mring-add-id fld))))
    
    (setq mat (copy-tree ($first m)))
    (setq perm ($second m))
    (setq n ($matrix_size mat))
    (setq r ($first n))
    (setq c ($second n))
    
    (setq mat (matrix-map (mring-maxima-to-mring fld) mat))
    (setq b (copy-tree b1))
    (setq c ($second ($matrix_size mat)))
    
    (setq cc ($second ($matrix_size b)))
    (setq b (matrix-map (mring-maxima-to-mring fld) b))

    (setq bb (copy-tree b))
    (loop for i from 1 to r do
      (loop for j from 1 to cc do
	(setmatelem bb (m-elem b perm i j) i j)))
    
    (setq id-perm (mfuncall '$makelist 'i 'i 1 r))

    (loop for q from 1 to cc do 
      (loop for i from 2 to c do
	(setq acc add-id)
	(loop for j from 1 to (- i 1) do
	  (setq acc (funcall fadd acc (funcall fmult (m-elem mat perm i j) (m-elem bb id-perm j q)))))
	(setmatelem bb (funcall fsub (m-elem bb id-perm i q) acc) i q)))
    
    (loop for q from 1 to cc do
      (loop for i from r downto 1 do
	(setq acc (m-elem bb id-perm i q))
	(loop for j from (+ 1 i) to c do
	  (setq acc (funcall fsub acc (funcall fmult (m-elem mat perm i j) (m-elem bb id-perm j q)))))
	(setmatelem bb (funcall fdiv acc (m-elem mat perm i i)) i q)))
    
    (setq bb (matrix-map (mring-mring-to-maxima fld) bb))
    bb))

(defun $invert_by_lu (m  &optional (fld '$generalring))
  ($require_square_matrix m "$first" "$invert_by_lu")
  ($lu_backsub ($lu_factor m fld) ($identfor m)))
  					
;; Return a Lisp list of two elements, the determinant, and the inverse of M.
(defun invert-by-lu-with-determinant (m fld-name)
  (let*
   ((i ($identfor m))
    (m1 ($lu_factor m fld-name))
    (fld (get fld-name 'ring))
    (d (determinant-by-lu-factors m1 fld)))
   (list d ($lu_backsub m1 i))))
  					
(defun $determinant_by_lu (m &optional (fld-name '$generalring))
  ($require_square_matrix m "$first" "$determinant_by_lu")
 
  (let ((fld ($require_ring fld-name "$second" "$determinant_by_lu")))
    (setq m ($lu_factor m fld-name))
    (determinant-by-lu-factors m fld)))

;; Assume that M has already been factored by $LU_FACTOR
;; and FLD is some field (not a field name).
(defun determinant-by-lu-factors (m fld)
 
  (let* ((acc (funcall (mring-mult-id fld)))
	 (fmult (mring-mult fld))
	 (fconvert (mring-maxima-to-mring fld))
	 (n ($first ($matrix_size ($first m))))
	 (perm) (d))

    (setq perm ($second m))
   
    (setq m ($first m))
    (loop for i from 1 to n do
      (setq d (funcall fconvert (m-elem m perm i i)))
      ;;(if ($matrixp d) (setq d ($determinant_by_lu d fld)))
      (setq acc (funcall fmult acc d)))
    (bbsort1 (cdr perm))
    (funcall (mring-mring-to-maxima fld) (if sign (funcall (mring-negate fld) acc) acc))))

(defun $mat_cond (m p)
  ($require_square_matrix m "$first" "$mat_cond")
  (mul (mfuncall '$mat_norm m p) (mfuncall '$mat_norm ($invert_by_lu m) p)))

(defun $linsolve_by_lu (m b &optional (fld '$generalring))
  ($require_square_matrix m "$first" "$linsolve_by_lu")
  (setq b (if ($listp b) ($transpose b) b))
  ($require_matrix b "$second" "$linsolve_by_lu")
  ($require_ring fld "$third" "$linsolve_by_lu")
  (if (= ($second ($matrix_size m)) ($first ($matrix_size b))) t
    (merror "Incompatible matrix sizes"))
  
  (setq m ($lu_factor m fld))
  `((mlist) ,($lu_backsub m b) ,(if (floatp ($last m)) ($last m) nil)))

