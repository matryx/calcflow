;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 2edcbd958861 2012/05/30 03:34:52 toy $"
;;;  "f2cl2.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 3fe93de3be82 2012/05/06 02:17:14 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 3fe93de3be82 2012/05/06 02:17:14 toy $")

;;; Using Lisp CMU Common Lisp 20d (20D Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :lapack)


(defun zladiv (x y)
  (declare (type (f2cl-lib:complex16) y x))
  (prog ((zi 0.0) (zr 0.0) (zladiv #C(0.0 0.0)) (dble$ 0.0f0) (dimag$ 0.0f0))
    (declare (type (single-float) dimag$ dble$)
             (type (f2cl-lib:complex16) zladiv)
             (type (double-float) zr zi))
    (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
        (dladiv (f2cl-lib:dble x) (f2cl-lib:dimag x) (f2cl-lib:dble y)
         (f2cl-lib:dimag y) zr zi)
      (declare (ignore var-0 var-1 var-2 var-3))
      (setf zr var-4)
      (setf zi var-5))
    (setf zladiv (f2cl-lib:dcmplx zr zi))
    (go end_label)
   end_label
    (return (values zladiv nil nil))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zladiv
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::complex16)
                        (fortran-to-lisp::complex16))
           :return-values '(nil nil)
           :calls '(fortran-to-lisp::dladiv))))

