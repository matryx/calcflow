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


(let* ((zero (f2cl-lib:cmplx 0.0 0.0)))
  (declare (type (f2cl-lib:complex16) zero) (ignorable zero))
  (defun ilazlc (m n a lda)
    (declare (type (array f2cl-lib:complex16 (*)) a)
             (type (f2cl-lib:integer4) lda n m))
    (f2cl-lib:with-multi-array-data
        ((a f2cl-lib:complex16 a-%data% a-%offset%))
      (prog ((i 0) (ilazlc 0))
        (declare (type (f2cl-lib:integer4) i ilazlc))
        (cond
          ((= n 0)
           (setf ilazlc n))
          ((or (/= (f2cl-lib:fref a (1 n) ((1 lda) (1 *))) zero)
               (/= (f2cl-lib:fref a (m n) ((1 lda) (1 *))) zero))
           (setf ilazlc n))
          (t
           (f2cl-lib:fdo (ilazlc n
                          (f2cl-lib:int-add ilazlc (f2cl-lib:int-sub 1)))
                         ((> ilazlc 1) nil)
             (tagbody
               (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                             ((> i m) nil)
                 (tagbody
                   (if
                    (/=
                     (f2cl-lib:fref a-%data%
                                    (i ilazlc)
                                    ((1 lda) (1 *))
                                    a-%offset%)
                     zero)
                    (go end_label))
                  label100001))
              label100000))))
        (go end_label)
       end_label
        (return (values ilazlc nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::ilazlc
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil)
           :calls 'nil)))

