;;; -*- Mode: lisp; Package: CL-USER -*-

(in-package :maxima)

;; Raw interface to colnew where essentially all of the parameters for
;; colnew are exposed.  The f, df, g, dg, and init-guess parameters
;; must be functions.
(defun $colnew_expert (ncomp m aleft aright zeta ipar ltol tol fixpnt ispace fspace
		       iflag f df g dg init-guess)
  (flet ((convert-to-array (mlist atype)
	   ;; Convert the Maxima lists to arrays
	   (make-array (length (cdr mlist)) :element-type atype
		       :initial-contents (cdr mlist)))
	 (convert-to-mlist (v)
	   (list* '(mlist) (coerce v 'list))))
    (let ((a-m (convert-to-array m 'f2cl-lib:integer4))
	  (a-zeta (convert-to-array zeta 'double-float))
	  (a-ipar (convert-to-array ipar 'f2cl-lib:integer4))
	  (a-ltol (convert-to-array ltol 'f2cl-lib:integer4))
	  (a-tol (convert-to-array tol 'double-float))
	  (a-fixpnt (convert-to-array fixpnt 'double-float))
	  (a-ispace (convert-to-array ispace 'f2cl-lib:integer4))
	  (a-fspace (convert-to-array fspace 'double-float))
	  (f (coerce-float-fun f))
	  (df (coerce-float-fun df))
	  (g (coerce-float-fun g))
	  (dg (coerce-float-fun dg))
	  (nz (reduce #'+ (cdr m))))
      (flet ((fsub (x z f-array)
	       (declare (type (cl:array double-float (*)) z f-array)
			(double-float x))
	       (let ((res (apply f x (coerce (subseq z 0 nz) 'list))))
		 (when (listp res)
		   (loop for k from 0
		      for ff in (cdr res)
		      do
		      (setf (aref f-array k) ($float ff)))))
	       (values nil nil f-array))
	     (dfsub (x z df-array)
	       (declare (type (cl:array double-float (*)) z df-array)
			(double-float x))
	       (let ((res (apply df x (coerce (subseq z 0 nz) 'list))))
		 ;; res is a Maxima matrix.
		 (when (listp res)
		   (loop for row from 1
		      for row-list in (cdr res)
		      do
		      (loop for col from 1
			 for element in (cdr row-list)
			 do
			 (setf (f2cl-lib::fref df-array (row col) ((1 ncomp) (1 *))) ($float element))))))
	       (values nil nil df-array))
	     (gsub (i z dummy)
	       (declare (type (cl:array double-float (*)) z)
			(type f2cl-lib:integer4 i))
	       (declare (ignore dummy))
	       (let ((res (apply g i (coerce (subseq z 0 nz) 'list))))
		 (values nil nil res)))
	     (dgsub (i z dg-array)
	       (declare (type (cl:array double-float (*)) z dg-array)
			(type f2cl-lib:integer4 i))
	       (let ((res (apply dg i (coerce (subseq z 0 nz) 'list))))
		 (loop for k from 0
		    for ff in (cdr res)
		    do
		    (setf (aref dg-array k) ($float ff))))
	       (values nil nil dg-array))
	     (guess (x z dmval)
	       (declare (double-float x)
			(type (cl:array double-float (*)) z dmval))
	       (let* ((res ($float (mcall init-guess x)))
		      (new-z (second res))
		      (new-d (third res)))
		 (loop for k from 0
		    for ff in (cdr new-d)
		    do (setf (aref dmval k) ($float ff)))
		 (loop for k from 0
		    for ff in (cdr new-z)
		    do (setf (aref z k) ($float ff))))
	       (values nil z dmval)))
	(multiple-value-bind (z-ncomp z-m z-aleft z-aright z-zeta z-ipar z-ltol z-tol
				      z-fixpnt z-ispace z-fspace o-iflag)
	    (colnew:colnew ncomp
			   a-m
			   ($float aleft)
			   ($float aright)
			   a-zeta
			   a-ipar
			   a-ltol
			   a-tol
			   a-fixpnt
			   a-ispace
			   a-fspace
			   iflag
			   #'fsub
			   #'dfsub
			   #'gsub
			   #'dgsub
			   #'guess)
	  (declare (ignore z-ncomp z-m z-aleft z-aright z-zeta z-ipar z-ltol z-tol
			   z-fixpnt z-ispace z-fspace))
	  (list '(mlist)
		o-iflag
		(convert-to-mlist a-fspace)
		(convert-to-mlist a-ispace)))))))

;; This is a slight extension of the actual APPSLN function.  The X
;; must be a list.  The output is then a list of the z values for each
;; x.  The z values themselves are lists.
(defun $colnew_appsln (x zlen fspace ispace)
  (flet ((convert-to-array (mlist atype)
	   (make-array (length (cdr mlist)) :element-type atype
		       :initial-contents (cdr mlist)))
	 (convert-to-mlist (v)
	   (list* '(mlist) (coerce v 'list))))
    (let ((a-fspace (convert-to-array fspace 'double-float))
	  (a-ispace (convert-to-array ispace 'f2cl-lib:integer4))
	  (z (make-array zlen :element-type 'double-float))
	  (result nil))
      (dolist (pnt (cdr x))
	(colnew:appsln ($float pnt) z a-fspace a-ispace)
	(push (convert-to-mlist z) result))
      (list* '(mlist) (nreverse result)))))
