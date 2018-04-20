;;; Simple raw interface to LAPACK dgemm, general real matrix
;;; multiplication.
(in-package :maxima)

(defun %%dgemm (a b &key (c nil cp) transpose_a transpose_b (alpha 1e0) (beta 0e0 betap))
  (flet ((maybe-transpose-dims (transp row col)
	   (if transp
	       (values col row)
	       (values row col))))
    (multiple-value-bind (a-nrows a-ncols)
	(maxima-matrix-dims a)
      (multiple-value-bind (at-nrows at-ncols)
	  (maybe-transpose-dims transpose_a a-nrows a-ncols)
	(multiple-value-bind (b-nrows b-ncols)
	    (maxima-matrix-dims b)
	  (multiple-value-bind (bt-nrows bt-ncols)
	      (maybe-transpose-dims transpose_b b-nrows b-ncols)
	    ;; Compute the dimensions of C.  If C is not given, we
	    ;; need to create a space of the correct dimension to hold
	    ;; the result.
	    (multiple-value-bind (c-nrows c-ncols)
		(if c
		    (maxima-matrix-dims c)
		    (values at-nrows bt-ncols))
	      ;; Do something nice if C is given but beta is not.  We
	      ;; take this to mean that we just want to add C,
	      ;; implying that beta = 1.
	      (when (and cp (not betap))
		(setf beta 0e0))
	      ;; Now for some error checking.  This is a bit redundant
	      ;; since dgemm does some error checking, but I think we
	      ;; prefer not to see messages from dgemm.  It's better
	      ;; to have the messages come from maxima.

	      (unless (= at-ncols bt-nrows)
		(merror "Cannot multiply a ~m by ~m matrix by a ~m by ~m matrix"
			at-nrows at-ncols bt-nrows bt-ncols))
	      (when (and c
			 (or (/= at-nrows c-nrows)
			     (/= bt-ncols c-ncols)))
		(merror "Cannot add a ~m by ~m matrix to a ~m by ~m C matrix"
			at-nrows bt-ncols c-nrows c-ncols))
		
	      ;; But it doesn't make sense to supply beta without C.
	      ;; (Well, we could assume that C is zero, but then why
	      ;; bother specifying beta?)
	      (when (and betap (not cp))
		(merror "beta given, but no C matrix supplied?"))
	      (let ((alpha ($float alpha))
		    (beta ($float beta))
		    (matrix-a (lapack-lispify-matrix a a-nrows a-ncols))
		    (matrix-b (lapack-lispify-matrix b b-nrows b-ncols))
		    (matrix-c (cond ((and c (not (zerop beta)))
				     (lapack-lispify-matrix c a-nrows b-ncols))
				    (t
				     ;; No C matrix given, or beta is zero.
				     ;; Force beta to be zero to tell LAPACK
				     ;; not to add C.  But we still need to
				     ;; create a matrix.
				     (setf beta 0e0)
				     (make-array (* c-nrows c-ncols) :element-type 'flonum))))
		    (trans-a (if transpose_a "t" "n"))
		    (trans-b (if transpose_b "t" "n")))
		(blas::dgemm trans-a trans-b
			     at-nrows bt-ncols at-ncols
			     alpha
			     matrix-a a-nrows
			     matrix-b b-nrows
			     beta
			     matrix-c c-nrows)
		;; matrix-c contains the desired result.
		(lapack-maxify-matrix c-nrows c-ncols matrix-c)))))))))

;; Main interface from maxima to Lapack routine dgemm.  Just parses
;; args and calls %%dgemm to do the dirty work.
(defun $%dgemm (a b options)
  (let* ((args (lispify-maxima-keyword-options
		(cdr options)
		'($c $transpose_a $transpose_b $alpha $beta))))
    (apply #'%%dgemm a b args)))
    