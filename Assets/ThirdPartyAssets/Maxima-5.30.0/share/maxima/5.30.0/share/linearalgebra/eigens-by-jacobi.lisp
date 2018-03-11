;;  Copyright 2006 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ($put '$eigensbyjacobi 1 '$version)) ;; Let's have version numbers 1,2,3,...
		 
;; One sweep zeros each member of the matrix; for a n x n matrix, this requires n(n-1)/2
;; Jacobi rotations. 

;; For flonum floats, eps is the machine epsilon; for bigfloats, it is 1/2^fpprec.

;; The variable 'change' tracks the greatest percent change in a diagonal entry in
;; a sweep.  When the diagonal entry is less than eps, the percent change set to zero.
;; The iteration stops when 'change' is less than eps (numerical convergence).

;; The matrix entries are computed with numerically friendly formulae--they
;; have the form new value <-- old value + correction.  In general, the 
;; correction is 'small.' These formulae are well-known; I used the reference
;; "Numerical Recipes in Fortran," by Press et.al.

(defun $eigens_by_jacobi (mm &optional (fld-name '$floatfield))
  (if (not (member fld-name `($floatfield $bigfloatfield)))
      (merror "The field must either be 'floatfield' or 'bigfloatfield'"))
  
  (setq mm (mfuncall '$mat_fullunblocker mm))
  ($require_real_symmetric_matrix mm "$first" "$eigens_by_jacobi")
     
  (let* ((mat) (g) (h) (sweeps 0) (rotations 0) (eps) (change)
	 (theta) (mpq) (c) (s)  (tee) (tau) (d) (v) (x) (row)
	 (n ($first ($matrix_size mm))) (continue (> n 1))
	 (fld ($require_ring fld-name "$second" "$eigens_by_jacobi"))
	 (one (funcall (mring-mult-id fld)))
	 (zero (funcall (mring-add-id fld))))
	
    (flet
	((fzerop (a) (funcall (mring-fzerop fld) a))
	 (fabs (a) (funcall (mring-abs fld) a))
	 (fnegate (a) (funcall (mring-negate fld) a))
	 (fpsqrt (a) (funcall (mring-psqrt fld) a))
	 (fadd (a b) (funcall (mring-add fld) a b))
	 (fsub (a b) (funcall (mring-sub fld) a b))
	 (fmult (a b) (funcall (mring-mult fld) a b))
	 (fdiv (a b) (funcall (mring-div fld) a b))
	 (fgreat (a b) (funcall (mring-great fld) a b))
	 (fmax (a b) (if (funcall (mring-great fld) a b) a b))
	 (fconvert (a) (funcall (mring-maxima-to-mring fld) a)))

      (setq mat (make-array (list n n) :initial-contents (mapcar #'rest 
								 (margs (matrix-map #'fconvert mm)))))
      (setq v (make-array (list n n) :initial-element zero))
      (setq d (make-array n))
									 
      (setq eps (if (eq fld-name '$floatfield) flonum-epsilon ($bfloat (div 1 (power 2 fpprec)))))
             
      (decf n)
      (loop for i from 0 to n do 
	(setf (aref v i i) one)
	(setf (aref d i) (aref mat i i)))

      (while continue
	(if (> sweeps 50) (merror "Exceeded maximum allowable number of Jacobi sweeps"))
	(incf sweeps)
	(loop for p from 0 to n do 
	  (loop for q from (+ p 1) to n do
	    (setq mpq (aref mat p q))
	    (cond ((not (fzerop mpq))
		   (incf rotations)
		   (setq theta (fdiv (fsub (aref mat q q) (aref mat p p))(fmult 2 mpq)))
		   (setq tee (fdiv one (fadd (fabs theta) (fpsqrt (fadd one (fmult theta theta))))))
		   (if (fgreat 0 theta) (setq tee (fnegate tee)))
		   (setq c (fdiv one (fpsqrt (fadd one (fmult tee tee)))))
		   (setq s (fmult tee c))
		   (setq tau (fdiv s (fadd one c)))
		   (setf (aref mat p q) zero)
		   
		   (loop for k from 0 to (- p 1) do
		     (setq g (aref mat k p))
		     (setq h (aref mat k q))
		     (setf (aref mat k p) (fsub g (fmult s (fadd h (fmult g tau)))))
		     (setf (aref mat k q) (fadd h (fmult s (fsub g (fmult h tau))))))
		   
		   (loop for k from (+ p 1) to (- q 1) do
		     (setq g (aref mat p k))
		     (setq h (aref mat k q))
		     (setf (aref mat p k) (fsub g (fmult s (fadd h (fmult g tau)))))
		     (setf (aref mat k q) (fadd h (fmult s (fsub g (fmult h tau))))))
		   
		   (loop for k from (+ q 1) to n do
		     (setq g (aref mat p k))
		     (setq h (aref mat q k))
		     (setf (aref mat p k) (fsub g (fmult s (fadd h (fmult g tau)))))
		     (setf (aref mat q k) (fadd h (fmult s (fsub g (fmult h tau))))))
		   
		   (setf (aref mat p p) (fsub (aref mat p p) (fmult tee mpq)))
		   (setf (aref mat q q) (fadd (aref mat q q) (fmult tee mpq)))
		   (loop for k from 0 to n do
		     (setq g (aref v k p))
		     (setq h (aref v k q))
		     (setf (aref v k p) (fsub g (fmult s (fadd h (fmult g tau)))))
		     (setf (aref v k q)(fadd h (fmult s (fsub g (fmult h tau))))))))))
		
	(setq change zero)
	(loop for i from 0 to n do
	  (setq x (aref mat i i))
	  (setq change (fmax change (if (fgreat (fabs x) eps) (fabs (fdiv (fsub (aref d i) x) x)) zero)))
	  (setf (aref d i) x))
	
	(inform '$debug '$linearalgebra "The largest percent change was ~:M~%" change)
	(setq continue (fgreat change eps)))
	
      (inform '$verbose '$linearalgebra "number of sweeps: ~:M~%" sweeps)
      (inform '$verbose '$linearalgebra "number of rotations: ~:M~%" rotations)
      
      (setq mm nil)
      (loop for i from 0 to n do
	(setq row nil)
	(loop for j from 0 to n do
	  (push (aref v i j) row))
	(setq row (reverse row))
	(push '(mlist) row)
	(push row mm))
      (setq mm (reverse mm))
      (push '($matrix) mm)
      (setq d `((mlist) ,@(coerce d 'list)))
      `((mlist) ,d ,mm))))

      
				 
		     
		   
		   
   
		     
		     
		   
		   
		 
  
		
