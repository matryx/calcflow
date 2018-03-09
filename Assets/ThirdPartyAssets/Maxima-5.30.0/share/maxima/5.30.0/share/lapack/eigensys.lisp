(in-package :maxima)

(defun maxima-matrix-dims (a)
  (let ((row (second a)))
    ;; Should we check that all rows have the same length?
    (values (length (cdr a)) (length (cdr row)))))

(defun complex-maxima-matrix-p (a)
  (dolist (row (cdr a))
    (dolist (col (cdr row))
      (unless (eql ($imagpart col) 0)
	(return-from complex-maxima-matrix-p t))))
  nil)

(defun lapack-lispify-matrix (a nrow ncol)
  "Convert a Maxima matrix A of dimension NROW and NCOL to Lisp matrix
  suitable for use with LAPACK"
  (setq a ($float a))
  (let* ((array-type (if (complex-maxima-matrix-p a)
			 '(complex flonum)
			 'flonum))
	 (mat (make-array (* nrow ncol)
			  :element-type array-type))
	 (mat-2d (make-array (list ncol nrow)
			     :element-type array-type
			     :displaced-to mat))
	 (r 0))
    (dolist (row (cdr a))
      (let ((c 0))
	(dolist (col (cdr row))
	  ;; Fortran matrices are in column-major order!
	  (setf (aref mat-2d c r) (if (eql array-type 'flonum)
				      (coerce col 'flonum)
				      (coerce (complex ($realpart col) ($imagpart col))
					      '(complex flonum))
				      ))
	  (incf c)))
      (incf r))
    mat))

(defun lapack-maxify-matrix (nrow ncol a)
  "Convert an LAPACK matrix of dimensions NROW and NCOL into a Maxima
matrix (list of lists)"
  (let ((2d (make-array (list ncol nrow) :element-type (array-element-type a)
			:displaced-to a)))
    (let (res)
      (dotimes (r nrow)
	(let (row)
	  (dotimes (c ncol)
	    ;; Fortran arrays are column-major order!
	    (let ((v (aref 2d c r)))
	      (push (add (realpart v) (mul '$%i (imagpart v))) row)))
	  (push `((mlist) ,@(nreverse row)) res)))
      `(($matrix) ,@(nreverse res)))))

(defun $dgeev (a &optional right-vec-p left-vec-p)
  "
DGEEV computes for an N-by-N real nonsymmetric matrix A, the
eigenvalues and, optionally, the left and/or right eigenvectors.

The right eigenvector v(j) of A satisfies
                 A * v(j) = lambda(j) * v(j)
where lambda(j) is its eigenvalue.
The left eigenvector u(j) of A satisfies
              u(j)**H * A = lambda(j) * u(j)**H
where u(j)**H denotes the conjugate transpose of u(j).

The computed eigenvectors are normalized to have Euclidean norm
equal to 1 and largest component real.

A list of three items is returned.  The first item is a list of the
eigenvectors.  The second item is false or the matrix of right
eigenvectors.  The last itme is false or the matrix of left
eigenvectors."
  (flet ((make-eigval (wr wi)
	   `((mlist) ,@(map 'list #'(lambda (r i)
				      (add r (mul '$%i i)))
			    wr wi)))
	 (make-eigvec (n vr wi)
	   ;; dgeev stores the eigen vectors in a special way.  Undo
	   ;; that.  For simplicity, we create a 2-D matrix and store
	   ;; the eigenvectors there.  Then we convert that matrix
	   ;; into a form that maxima wants.  Somewhat inefficient.
	   (let ((evec (make-array (list n n))))
	     (do ((col 0 (incf col))
		  (posn 0))
		 ((>= col n))
	       (cond ((zerop (aref wi col))
		      (dotimes (row n)
			(setf (aref evec row col) (aref vr posn))
			(incf posn)))
		     (t
		      (dotimes (row n)
			(let* ((next-posn (+ posn n))
			       (val+ (add (aref vr posn)
					  (mul '$%i (aref vr next-posn))))
			       (val- (sub (aref vr posn)
					  (mul '$%i (aref vr next-posn)))))
			  (setf (aref evec row col) val+)
			  (setf (aref evec row (1+ col)) val-)
			  (incf posn)))
		      ;; Skip over the next column, which we've already used
		      (incf col)
		      (incf posn n))))
	     ;; Now convert this 2-D Lisp matrix into a maxima matrix
	     (let (res)
	       (dotimes (r n)
		 (let (row)
		   (dotimes (c n)
		     (push (aref evec r c) row))
		   (push `((mlist) ,@(nreverse row)) res)))
	       `(($matrix) ,@(nreverse res)))
	     )))
    
    (let* ((n (maxima-matrix-dims a))
	   (a-mat (lapack-lispify-matrix a n n))
	   (wr (make-array n :element-type 'flonum))
	   (wi (make-array n :element-type 'flonum))
	   (vl (make-array (if left-vec-p (* n n) 0)
			   :element-type 'flonum))
	   (vr (make-array (if right-vec-p (* n n) 0)
			   :element-type 'flonum)))
      ;; XXX: FIXME: We need to do more error checking in the calls to
      ;; dgeev!
      (multiple-value-bind (z-jobvl z-jobvr z-n z-a z-lda z-wr z-wi z-vl
				    z-ldvl z-vr z-ldvr z-work z-lwork info)
	  ;; Figure out how much space we need in the work array.
	  (lapack:dgeev (if left-vec-p "V" "N")
			(if right-vec-p "V" "N")
			n a-mat n wr wi vl n vr n wr -1 0)
	(declare (ignore z-jobvl z-jobvr z-n z-a z-lda z-wr z-wi z-vl
			 z-ldvl z-vr z-ldvr z-work z-lwork info))
	(let* ((opt-lwork (truncate (aref wr 0)))
	       (work (make-array opt-lwork :element-type 'flonum)))
	  ;; Now do the work with the optimum size of the work space.
	  (multiple-value-bind (z-jobvl z-jobvr z-n z-a z-lda z-wr z-wi z-vl
					z-ldvl z-vr z-ldvr z-work z-lwork info)
	      (lapack:dgeev (if left-vec-p "V" "N")
			    (if right-vec-p "V" "N")
			    n a-mat n wr wi vl n vr n work opt-lwork 0)
	    (declare (ignore z-jobvl z-jobvr z-n z-a z-lda z-wr z-wi z-vl
			     z-ldvl z-vr z-ldvr z-work z-lwork))
	    (cond ((< info 0)
		   (merror "DGEEV: invalid arguments: ~D" info))
		  ((> info 0)
		   (merror "DGEEV: failed to converge: ~D" info)))
	    ;; Convert wr+%i*wi to maxima form
	    #+nil
	    (progn
	      (format t "info = ~A~%" info)
	      (format t "lwork = ~A~%" (aref work 0))
	      (format t "vr = ~A~%" vr))
	    (let ((e-val (make-eigval wr wi))
		  (e-vec-right (if right-vec-p
				   (make-eigvec n vr wi)
				   nil))
		  (e-vec-left (if left-vec-p
				  (make-eigvec n vl wi)
				  nil)))
	      `((mlist) ,e-val ,e-vec-right ,e-vec-left))))))))

(defun $dgesvd (a &optional jobu jobvt)
  "
DGESVD computes the singular value decomposition (SVD) of a real
M-by-N matrix A, optionally computing the left and/or right singular
vectors. The SVD is written

     A = U * SIGMA * transpose(V)

where SIGMA is an M-by-N matrix which is zero except for its
min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
are the singular values of A; they are real and non-negative, and
are returned in descending order.  The first min(m,n) columns of
U and V are the left and right singular vectors of A.

Note that the routine returns V**T, not V.

A list of three items is returned.  The first is a list containing the
non-zero elements of SIGMA.  If jobu is not false, The second element
is the matrix U.  Otherwise it is false.  Similarly, the third element
is V**T or false, depending on jobvt."
  
  (flet ((maxify-vector (v)
	   `((mlist) ,@(coerce v 'list)))
	 (fixup-jobu (arg)
	   (if arg "All columns of U" "No columns of U"))
	 (fixup-jobvt (arg)
	   (if arg "All columns of V^T" "No columns of V^T")))
    (multiple-value-bind (nrow ncol)
	(maxima-matrix-dims a)  
      (let* ((a-mat (lapack-lispify-matrix a nrow ncol))
	     (s (make-array (min nrow ncol) :element-type 'flonum))
	     (u (make-array (* nrow nrow) :element-type 'flonum))
	     (u1 (make-array (list nrow nrow) :element-type 'flonum
			     :displaced-to u))
	     (vt (make-array (* ncol ncol)
			     :element-type 'flonum))
	     (vt1 (make-array (list ncol ncol) :element-type 'flonum
			      :displaced-to vt))
	     (wr (make-array 1 :element-type 'flonum)))
	;; XXX: FIXME: We need to do more error checking in the calls to
	;; dgesvd!
	(multiple-value-bind (z-jobu z-jobvt z-m z-n z-a z-lda z-s z-u z-ldu
				     z-vt z-ldvt z-work z-lwork info)
	    ;; Figure out the optimum size for the work array
	    (lapack::dgesvd (fixup-jobu jobu)
			    (fixup-jobvt jobvt)
			    nrow ncol a-mat nrow
			    s u nrow
			    vt ncol
			    wr -1
			    0)
	  (declare (ignore z-jobu z-jobvt z-m z-n z-a z-lda z-s z-u z-ldu
			   z-vt z-ldvt z-work z-lwork info))
	  (let* ((opt-lwork (truncate (aref wr 0)))
		 (work (make-array opt-lwork :element-type 'flonum)))
	    ;; Allocate the optimum work array and do the requested
	    ;; computation.
	    (multiple-value-bind (z-jobu z-jobvt z-m z-n z-a z-lda z-s z-u
					 z-ldu z-vt z-ldvt z-work z-lwork info)
		(lapack::dgesvd (fixup-jobu jobu)
				(fixup-jobvt jobvt)
				nrow ncol a-mat nrow
				s u nrow
				vt ncol
				work opt-lwork
				0)
	      (declare (ignore z-jobu z-jobvt z-m z-n z-a z-lda z-s z-u z-ldu
			       z-vt z-ldvt z-work z-lwork))
	      (cond ((< info 0)
		     (merror "DGESVD: invalid arguments: ~D" info))
		    ((> info 0)
		     (merror "DGESVD: failed to converge: ~D" info)))
	      (let ((u-max (if jobu
			       (lapack-maxify-matrix nrow nrow u1)
			       nil))
		    (vt-max (if jobvt
				(lapack-maxify-matrix ncol ncol vt1)
				nil))
		    (s-max (maxify-vector s)))
		`((mlist) ,s-max ,u-max ,vt-max)))))))))



(defun $dlange (norm a)
  "
DLANGE returns the value

   DLANGE = ( max(abs(A(i,j))), NORM = '$max
            (
            ( norm1(A),         NORM = '$one_norm
            (
            ( normI(A),         NORM = '$inf_norm
            (
            ( normF(A),         NORM = '$frobenius

where  norm1  denotes the  one norm of a matrix (maximum column sum),
normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
normF  denotes the  Frobenius norm of a matrix (square root of sum of
squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm."

  ;; Norm should be '$max, '$one_norm, '$inf_norm, '$frobenius
  (multiple-value-bind (nrows ncols)
      (maxima-matrix-dims a)
    (let* ((a-mat (lapack-lispify-matrix a nrows ncols))
	   (norm-type (ecase norm
			($max "M")
			($one_norm "O")
			($inf_norm "I")
			($frobenius "F")))
	   (work (make-array (if (equal norm-type "I") nrows 0)
			     :element-type 'flonum)))
      (lapack::dlange norm-type nrows ncols a-mat nrows work))))


(defun $zlange (norm a)
  "
DLANGE returns the value

   DLANGE = ( max(abs(A(i,j))), NORM = '$max
            (
            ( norm1(A),         NORM = '$one_norm
            (
            ( normI(A),         NORM = '$inf_norm
            (
            ( normF(A),         NORM = '$frobenius

where  norm1  denotes the  one norm of a matrix (maximum column sum),
normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
normF  denotes the  Frobenius norm of a matrix (square root of sum of
squares).  Note that  max(abs(A(i,j)))  is not a  matrix norm."

  ;; Norm should be '$max, '$one_norm, '$inf_norm, '$frobenius
  (multiple-value-bind (nrows ncols)
      (maxima-matrix-dims a)
    (let* ((a-mat (lapack-lispify-matrix a nrows ncols))
	   (norm-type (ecase norm
			($max "M")
			($one_norm "O")
			($inf_norm "I")
			($frobenius "F")))
	   (work (make-array (if (equal norm-type "I") nrows 0)
			     :element-type 'flonum)))
      (lapack::zlange norm-type nrows ncols a-mat nrows work))))
      
(defun $zgeev (a &optional right-vec-p left-vec-p)
  "
ZGEEV computes for an N-by-N complex nonsymmetric matrix A, the
eigenvalues and, optionally, the left and/or right eigenvectors.

The right eigenvector v(j) of A satisfies
                 A * v(j) = lambda(j) * v(j)
where lambda(j) is its eigenvalue.
The left eigenvector u(j) of A satisfies
              u(j)**H * A = lambda(j) * u(j)**H
where u(j)**H denotes the conjugate transpose of u(j).

The computed eigenvectors are normalized to have Euclidean norm
equal to 1 and largest component real.

A list of three items is returned.  The first item is a list of the
eigenvectors.  The second item is false or the matrix of right
eigenvectors.  The last itme is false or the matrix of left
eigenvectors."
  (flet ((make-eigval (w)
	   `((mlist) ,@(map 'list #'(lambda (z)
				      (add (realpart z) (mul '$%i (imagpart z))))
			    w))))
    
    (let* ((n (maxima-matrix-dims a))
	   (a-mat (lapack-lispify-matrix a n n))
	   (w (make-array n :element-type '(complex flonum)))
	   (vl (make-array (if left-vec-p (* n n) 0)
			   :element-type '(complex flonum)))
	   (vr (make-array (if right-vec-p (* n n) 0)
			   :element-type '(complex flonum)))
	   (rwork (make-array 1 :element-type 'flonum)))
      ;; XXX: FIXME: We need to do more error checking in the calls to
      ;; zgeev!
      (multiple-value-bind (z-jobvl z-jobvr z-n z-a z-lda z-wr z-wi z-vl
				    z-ldvl z-vr z-ldvr z-work z-lwork info)
	  ;; Figure out how much space we need in the work array.
	  (lapack:zgeev (if left-vec-p "V" "N")
			(if right-vec-p "V" "N")
			n a-mat n w vl n vr n w -1 rwork 0)
	(declare (ignore z-jobvl z-jobvr z-n z-a z-lda z-wr z-wi z-vl
			 z-ldvl z-vr z-ldvr z-work z-lwork info))
	(let* ((opt-lwork (truncate (realpart (aref w 0))))
	       (work (make-array opt-lwork :element-type '(complex flonum)))
	       (rwork (make-array opt-lwork :element-type 'flonum)))
	  ;; Now do the work with the optimum size of the work space.
	  (multiple-value-bind (z-jobvl z-jobvr z-n z-a z-lda z-wr z-wi z-vl
					z-ldvl z-vr z-ldvr z-work z-lwork info)
	      (lapack:zgeev (if left-vec-p "V" "N")
			    (if right-vec-p "V" "N")
			    n a-mat n w vl n vr n work opt-lwork rwork 0)
	    (declare (ignore z-jobvl z-jobvr z-n z-a z-lda z-wr z-wi z-vl
			     z-ldvl z-vr z-ldvr z-work z-lwork))
	    (cond ((< info 0)
		   (merror "ZGEEV: invalid arguments: ~D" info))
		  ((> info 0)
		   (merror "ZGEEV: failed to converge: ~D" info)))
	    (let ((e-val (make-eigval w))
		  (e-vec-right (if right-vec-p
				   (lapack-maxify-matrix n n vr)
				   nil))
		  (e-vec-left (if left-vec-p
				  (lapack-maxify-matrix n n vl)
				  nil)))
	      `((mlist) ,e-val ,e-vec-right ,e-vec-left))))))))