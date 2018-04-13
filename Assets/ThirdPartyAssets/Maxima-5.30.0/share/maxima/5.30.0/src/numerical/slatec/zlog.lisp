;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 46c1f6a93b0d 2012/05/03 04:40:28 toy $"
;;;  "f2cl2.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 46c1f6a93b0d 2012/05/03 04:40:28 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v fceac530ef0c 2011/11/26 04:02:26 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2012-04 (20C Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((dpi 3.141592653589793) (dhpi 1.5707963267948966))
  (declare (type (double-float) dpi dhpi))
  (defun zlog (ar ai br bi ierr)
    (declare (type (f2cl-lib:integer4) ierr) (type (double-float) bi br ai ar))
    (prog ((zm 0.0) (dtheta 0.0))
      (declare (type (double-float) dtheta zm))
      (setf ierr 0)
      (if (= ar 0.0) (go label10))
      (if (= ai 0.0) (go label20))
      (setf dtheta (f2cl-lib:datan (/ ai ar)))
      (if (<= dtheta 0.0) (go label40))
      (if (< ar 0.0) (setf dtheta (- dtheta dpi)))
      (go label50)
     label10
      (if (= ai 0.0) (go label60))
      (setf bi dhpi)
      (setf br (f2cl-lib:flog (abs ai)))
      (if (< ai 0.0) (setf bi (- bi)))
      (go end_label)
     label20
      (if (> ar 0.0) (go label30))
      (setf br (f2cl-lib:flog (abs ar)))
      (setf bi dpi)
      (go end_label)
     label30
      (setf br (f2cl-lib:flog ar))
      (setf bi 0.0)
      (go end_label)
     label40
      (if (< ar 0.0) (setf dtheta (+ dtheta dpi)))
     label50
      (setf zm (coerce (realpart (zabs ar ai)) 'double-float))
      (setf br (f2cl-lib:flog zm))
      (setf bi dtheta)
      (go end_label)
     label60
      (setf ierr 1)
      (go end_label)
     end_label
      (return (values nil nil br bi ierr)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zlog fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float) (fortran-to-lisp::integer4))
           :return-values '(nil nil fortran-to-lisp::br fortran-to-lisp::bi
                            fortran-to-lisp::ierr)
           :calls '(fortran-to-lisp::zabs))))

