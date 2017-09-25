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

(in-package :blas)


(let* ((zero (f2cl-lib:cmplx 0.0 0.0)))
  (declare (type (f2cl-lib:complex16) zero) (ignorable zero))
  (defun zher (uplo n alpha x incx a lda)
    (declare (type (array f2cl-lib:complex16 (*)) a x)
             (type (double-float) alpha)
             (type (f2cl-lib:integer4) lda incx n)
             (type (simple-string *) uplo))
    (f2cl-lib:with-multi-array-data
        ((uplo character uplo-%data% uplo-%offset%)
         (x f2cl-lib:complex16 x-%data% x-%offset%)
         (a f2cl-lib:complex16 a-%data% a-%offset%))
      (prog ((i 0) (info 0) (ix 0) (j 0) (jx 0) (kx 0) (temp #C(0.0 0.0)))
        (declare (type (f2cl-lib:integer4) i info ix j jx kx)
                 (type (f2cl-lib:complex16) temp))
        (setf info 0)
        (cond
          ((and (not (lsame uplo "U")) (not (lsame uplo "L")))
           (setf info 1))
          ((< n 0)
           (setf info 2))
          ((= incx 0)
           (setf info 5))
          ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
           (setf info 7)))
        (cond
          ((/= info 0)
           (xerbla "ZHER  " info)
           (go end_label)))
        (if (or (= n 0) (= alpha (f2cl-lib:dble zero))) (go end_label))
        (cond
          ((<= incx 0)
           (setf kx
                   (f2cl-lib:int-sub 1
                                     (f2cl-lib:int-mul (f2cl-lib:int-sub n 1)
                                                       incx))))
          ((/= incx 1)
           (setf kx 1)))
        (cond
          ((lsame uplo "U")
           (cond
             ((= incx 1)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (cond
                    ((/= (f2cl-lib:fref x (j) ((1 *))) zero)
                     (setf temp
                             (coerce
                              (* alpha
                                 (f2cl-lib:dconjg
                                  (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)))
                              'f2cl-lib:complex16))
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i
                                       (f2cl-lib:int-add j
                                                         (f2cl-lib:int-sub 1)))
                                    nil)
                       (tagbody
                         (setf (f2cl-lib:fref a-%data%
                                              (i j)
                                              ((1 lda) (1 *))
                                              a-%offset%)
                                 (+
                                  (f2cl-lib:fref a-%data%
                                                 (i j)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                  (*
                                   (f2cl-lib:fref x-%data%
                                                  (i)
                                                  ((1 *))
                                                  x-%offset%)
                                   temp)))
                        label10))
                     (setf (f2cl-lib:fref a-%data%
                                          (j j)
                                          ((1 lda) (1 *))
                                          a-%offset%)
                             (coerce
                              (+
                               (f2cl-lib:dble
                                (f2cl-lib:fref a-%data%
                                               (j j)
                                               ((1 lda) (1 *))
                                               a-%offset%))
                               (f2cl-lib:dble
                                (*
                                 (f2cl-lib:fref x-%data%
                                                (j)
                                                ((1 *))
                                                x-%offset%)
                                 temp)))
                              'f2cl-lib:complex16)))
                    (t
                     (setf (f2cl-lib:fref a-%data%
                                          (j j)
                                          ((1 lda) (1 *))
                                          a-%offset%)
                             (coerce
                              (f2cl-lib:dble
                               (f2cl-lib:fref a-%data%
                                              (j j)
                                              ((1 lda) (1 *))
                                              a-%offset%))
                              'f2cl-lib:complex16))))
                 label20)))
             (t
              (setf jx kx)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (cond
                    ((/= (f2cl-lib:fref x (jx) ((1 *))) zero)
                     (setf temp
                             (coerce
                              (* alpha
                                 (f2cl-lib:dconjg
                                  (f2cl-lib:fref x-%data%
                                                 (jx)
                                                 ((1 *))
                                                 x-%offset%)))
                              'f2cl-lib:complex16))
                     (setf ix kx)
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i
                                       (f2cl-lib:int-add j
                                                         (f2cl-lib:int-sub 1)))
                                    nil)
                       (tagbody
                         (setf (f2cl-lib:fref a-%data%
                                              (i j)
                                              ((1 lda) (1 *))
                                              a-%offset%)
                                 (+
                                  (f2cl-lib:fref a-%data%
                                                 (i j)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                  (*
                                   (f2cl-lib:fref x-%data%
                                                  (ix)
                                                  ((1 *))
                                                  x-%offset%)
                                   temp)))
                         (setf ix (f2cl-lib:int-add ix incx))
                        label30))
                     (setf (f2cl-lib:fref a-%data%
                                          (j j)
                                          ((1 lda) (1 *))
                                          a-%offset%)
                             (coerce
                              (+
                               (f2cl-lib:dble
                                (f2cl-lib:fref a-%data%
                                               (j j)
                                               ((1 lda) (1 *))
                                               a-%offset%))
                               (f2cl-lib:dble
                                (*
                                 (f2cl-lib:fref x-%data%
                                                (jx)
                                                ((1 *))
                                                x-%offset%)
                                 temp)))
                              'f2cl-lib:complex16)))
                    (t
                     (setf (f2cl-lib:fref a-%data%
                                          (j j)
                                          ((1 lda) (1 *))
                                          a-%offset%)
                             (coerce
                              (f2cl-lib:dble
                               (f2cl-lib:fref a-%data%
                                              (j j)
                                              ((1 lda) (1 *))
                                              a-%offset%))
                              'f2cl-lib:complex16))))
                  (setf jx (f2cl-lib:int-add jx incx))
                 label40)))))
          (t
           (cond
             ((= incx 1)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (cond
                    ((/= (f2cl-lib:fref x (j) ((1 *))) zero)
                     (setf temp
                             (coerce
                              (* alpha
                                 (f2cl-lib:dconjg
                                  (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)))
                              'f2cl-lib:complex16))
                     (setf (f2cl-lib:fref a-%data%
                                          (j j)
                                          ((1 lda) (1 *))
                                          a-%offset%)
                             (coerce
                              (+
                               (f2cl-lib:dble
                                (f2cl-lib:fref a-%data%
                                               (j j)
                                               ((1 lda) (1 *))
                                               a-%offset%))
                               (f2cl-lib:dble
                                (* temp
                                   (f2cl-lib:fref x-%data%
                                                  (j)
                                                  ((1 *))
                                                  x-%offset%))))
                              'f2cl-lib:complex16))
                     (f2cl-lib:fdo (i (f2cl-lib:int-add j 1)
                                    (f2cl-lib:int-add i 1))
                                   ((> i n) nil)
                       (tagbody
                         (setf (f2cl-lib:fref a-%data%
                                              (i j)
                                              ((1 lda) (1 *))
                                              a-%offset%)
                                 (+
                                  (f2cl-lib:fref a-%data%
                                                 (i j)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                  (*
                                   (f2cl-lib:fref x-%data%
                                                  (i)
                                                  ((1 *))
                                                  x-%offset%)
                                   temp)))
                        label50)))
                    (t
                     (setf (f2cl-lib:fref a-%data%
                                          (j j)
                                          ((1 lda) (1 *))
                                          a-%offset%)
                             (coerce
                              (f2cl-lib:dble
                               (f2cl-lib:fref a-%data%
                                              (j j)
                                              ((1 lda) (1 *))
                                              a-%offset%))
                              'f2cl-lib:complex16))))
                 label60)))
             (t
              (setf jx kx)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (cond
                    ((/= (f2cl-lib:fref x (jx) ((1 *))) zero)
                     (setf temp
                             (coerce
                              (* alpha
                                 (f2cl-lib:dconjg
                                  (f2cl-lib:fref x-%data%
                                                 (jx)
                                                 ((1 *))
                                                 x-%offset%)))
                              'f2cl-lib:complex16))
                     (setf (f2cl-lib:fref a-%data%
                                          (j j)
                                          ((1 lda) (1 *))
                                          a-%offset%)
                             (coerce
                              (+
                               (f2cl-lib:dble
                                (f2cl-lib:fref a-%data%
                                               (j j)
                                               ((1 lda) (1 *))
                                               a-%offset%))
                               (f2cl-lib:dble
                                (* temp
                                   (f2cl-lib:fref x-%data%
                                                  (jx)
                                                  ((1 *))
                                                  x-%offset%))))
                              'f2cl-lib:complex16))
                     (setf ix jx)
                     (f2cl-lib:fdo (i (f2cl-lib:int-add j 1)
                                    (f2cl-lib:int-add i 1))
                                   ((> i n) nil)
                       (tagbody
                         (setf ix (f2cl-lib:int-add ix incx))
                         (setf (f2cl-lib:fref a-%data%
                                              (i j)
                                              ((1 lda) (1 *))
                                              a-%offset%)
                                 (+
                                  (f2cl-lib:fref a-%data%
                                                 (i j)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                  (*
                                   (f2cl-lib:fref x-%data%
                                                  (ix)
                                                  ((1 *))
                                                  x-%offset%)
                                   temp)))
                        label70)))
                    (t
                     (setf (f2cl-lib:fref a-%data%
                                          (j j)
                                          ((1 lda) (1 *))
                                          a-%offset%)
                             (coerce
                              (f2cl-lib:dble
                               (f2cl-lib:fref a-%data%
                                              (j j)
                                              ((1 lda) (1 *))
                                              a-%offset%))
                              'f2cl-lib:complex16))))
                  (setf jx (f2cl-lib:int-add jx incx))
                 label80))))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zher fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (fortran-to-lisp::integer4)
                        (double-float) (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::xerbla fortran-to-lisp::lsame))))

