;;; -*- Mode: lisp -*-

;;; Simple Maxima interface to COBYLA, Constrained Optimization BY
;;; Linear Approximations.

(in-package #-gcl #:maxima #+gcl "MAXIMA")

;; Variable that will hold the function that calculates the function
;; value and the constraints.
(defvar *calcfc*)

(in-package #-gcl #:bigfloat #+gcl "BIGFLOAT")

;; COBYLA always calls CALCFC to compute the function value and the
;; constraint equations.  But we want to be able to specify different
;; versions.  So, COBYLA calls CALCFC, which then calls *CALCFC* to
;; do the real compuation.
(cl:defun calcfc (n m x f con)
  (cl:declare (cl:ignore f))
  (cl:funcall maxima::*calcfc* n m x con))

(cl:in-package #-gcl #:maxima #+gcl "MAXIMA")

;; The actual interface to COBYLA.  Take the inputs from maxima,
;; massage them into a suitable Lisp form, and call COBYLA to find the
;; answer.
(defun %bf-cobyla (vars init-x f 
		   &key (ineq (list '(mlist))) eq (rhobeg .5d0)
		        (rhoend (bigfloat:sqrt (expt (bigfloat:to 10) (- $fpprec))))
		        (iprint 0)
		        (maxfun 1000))
  (when (and eq (cdr eq))
    ;; If there are equality constraints, append them to the list of
    ;; inequality constraints, and then append the negative of the
    ;; equality constraints too.  That is, if the equality constraint is
    ;; h(X) = 0, then we add inequality constraints h(X) >= 0 and -h(X)
    ;; >= 0.
    (setf ineq (append ineq
		       (cdr eq)
		       (mapcar #'(lambda (e)
				   (mul -1 e))
			       (cdr eq)))))
  (let* ((n (length (cdr vars)))
	 (m (length (cdr ineq)))
	 (x (make-array n
			:initial-contents (mapcar #'(lambda (z)
						      (let ((r (bigfloat:to ($bfloat z))))
							r))
						  (cdr init-x))))
	 ;; Real work array for cobyla.
	 (w (make-array (+ (* n (+ (* 3 n)
				   (* 2 m)
				   11))
			   (+ (* 4 m) 6)
			   6)))
	 ;; Integer work array for cobyla.
	 (iact (make-array (+ m 1) :element-type 'f2cl-lib::integer4))
	 (fv (coerce-bfloat-fun f vars))
	 (cv (coerce-bfloat-fun ineq vars))
	 (*calcfc* #'(lambda (nn mm xval cval)
		       ;; Compute the function and the constraints at
		       ;; the given xval.  The function value is
		       ;; returned is returned, and the constraint
		       ;; values are stored in cval.
		       (declare (fixnum nn mm)
				(type (cl:array t (*)) xval cval))
		       (let* ((x-list (map 'list #'to xval))
			      (f (bigfloat:maybe-to (apply fv x-list)))
			      (c (apply cv x-list)))
			 ;; Do we really need these checks?
			 #+nil
			 (progn
			   (format t "xval = ~S~%" xval)
			   (format t "f = ~A~%" f)
			   (format t "c = ~A~%" c))
			 (setf c (mapcar #'bigfloat:maybe-to (cdr c)))
			 ;;(format t "c = ~A~%" c)
			 (unless (or (floatp f) ($bfloatp f) (typep f 'bigfloat:bigfloat))
			   (merror "At the point ~M:~%The objective function did not evaluate to a number: ~M"
				   (list* '(mlist) x-list)
				   f))
			 (let ((bad-cons (loop for cval in c
					    for k from 1
					    unless (or (floatp cval)
						       ($bfloatp cval)
						       (typep cval 'bigfloat:bigfloat))
					    collect k)))
			   (when bad-cons
			     ;; List the constraints that did not
			     ;; evaluate to a number to make it easier
			     ;; for the user to figure out which
			     ;; constraints were bad.
			     (mformat t "At the point ~M:~%" (list* '(mlist) x-list))
			     (merror
			      (with-output-to-string (msg)
				(loop for index in bad-cons
				   do
				   (mformat msg "Constraint ~M did not evaluate to a real: ~M~%"
					    index (elt ineq index)))))))
			 (replace cval c)
			 ;; This is the f2cl calling convention for
			 ;; CALCFC.  For some reason, f2cl thinks n
			 ;; and m are modified, so they must be
			 ;; returned.
			 (values nn mm nil
				 f nil)))))
    (multiple-value-bind (null-0 null-1 null-2 null-3 null-4 null-5 neval null-6 null-7 ierr)
	(bigfloat::cobyla n m x (bigfloat:to rhobeg) (bigfloat:to rhoend) iprint maxfun w iact 0)
      (declare (ignore null-0 null-1 null-2 null-3 null-4 null-5 null-6 null-7))
      ;; Should we put a check here if the number of function
      ;; evaluations equals maxfun?  When iprint is not 0, the output
      ;; from COBYLA makes it clear that something bad happened.
      (let ((x-list (map 'list #'to x)))
	;; Return the optimum function value, the point that gives the
	;; optimum, the value of the constraints, and the number of
	;; function evaluations.  For convenience.  Only the point and
	;; the number of evaluations is really needed.
	(values (apply fv x-list)
		x
		neval
		ierr)))))

;; Interface.  See fmin_cobyla.mac for documentation.
(defun $%bf_fmin_cobyla (f vars init-x options)
  (let* ((args (lispify-maxima-keyword-options (cdr options) '($ineq $eq $rhobeg $rhoend $iprint $maxfun))))
    ;; Some rudimentary checks.
    (unless (= (length (cdr vars))
	       (length (cdr init-x)))
      (merror "Number of initial values (~M) does not match the number of variables ~M~%"
	      (length (cdr init-x))
	      (length (cdr vars))))
    (multiple-value-bind (fmin xopt neval ierr)
	(apply #'%bf-cobyla vars init-x f args)
      (list '(mlist)
	    (list* '(mlist) (mapcar #'(lambda (var val)
					`((mequal) ,var ,(to val)))
				    (cdr vars)
				    (coerce xopt 'list)))
	    fmin
	    neval
	    ierr))))
