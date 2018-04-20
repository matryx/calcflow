;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.222 2010/10/08 03:05:30 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.204 2010/02/23 05:21:30 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.114 2010/05/17 01:42:14 rtoy Exp $")

;;; Using Lisp CMU Common Lisp CVS Head 2010-09-27 22:45:24 (20B Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :cobyla)


(defun calcfc (n m x f con)
  (declare (type double-float f)
           (type (array double-float (*)) con x)
           (type (f2cl-lib:integer4) m n))
  (f2cl-lib:with-multi-array-data
      ((x double-float x-%data% x-%offset%)
       (con double-float con-%data% con-%offset%))
    (prog ()
      (declare)
      '""
      '"    test problem 2 (2d unit circle calculation)"
      '""
      (setf f
              (* (f2cl-lib:fref x-%data% (1) ((1 *)) x-%offset%)
                 (f2cl-lib:fref x-%data% (2) ((1 *)) x-%offset%)))
      (setf (f2cl-lib:fref con-%data% (1) ((1 *)) con-%offset%)
              (- 1.0
                 (expt (f2cl-lib:fref x-%data% (1) ((1 *)) x-%offset%) 2)
                 (expt (f2cl-lib:fref x-%data% (2) ((1 *)) x-%offset%) 2)))
      (go end_label)
     end_label
      (return (values nil nil nil f nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::calcfc
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) double-float
                        (array double-float (*)))
           :return-values '(nil nil nil fortran-to-lisp::f nil)
           :calls 'nil)))

