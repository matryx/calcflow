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


(let* ((zero 0.0))
  (declare (type (double-float 0.0 0.0) zero) (ignorable zero))
  (defun dger (m n alpha x incx y incy a lda)
    (declare (type (array double-float (*)) a y x)
             (type (double-float) alpha)
             (type (f2cl-lib:integer4) lda incy incx n m))
    (f2cl-lib:with-multi-array-data
        ((x double-float x-%data% x-%offset%)
         (y double-float y-%data% y-%offset%)
         (a double-float a-%data% a-%offset%))
      (prog ((i 0) (info 0) (ix 0) (j 0) (jy 0) (kx 0) (temp 0.0))
        (declare (type (f2cl-lib:integer4) i info ix j jy kx)
                 (type (double-float) temp))
        (setf info 0)
        (cond
          ((< m 0)
           (setf info 1))
          ((< n 0)
           (setf info 2))
          ((= incx 0)
           (setf info 5))
          ((= incy 0)
           (setf info 7))
          ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
           (setf info 9)))
        (cond
          ((/= info 0)
           (xerbla "DGER  " info)
           (go end_label)))
        (if (or (= m 0) (= n 0) (= alpha zero)) (go end_label))
        (cond
          ((> incy 0)
           (setf jy 1))
          (t
           (setf jy
                   (f2cl-lib:int-sub 1
                                     (f2cl-lib:int-mul (f2cl-lib:int-sub n 1)
                                                       incy)))))
        (cond
          ((= incx 1)
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (cond
                 ((/= (f2cl-lib:fref y (jy) ((1 *))) zero)
                  (setf temp
                          (* alpha
                             (f2cl-lib:fref y-%data% (jy) ((1 *)) y-%offset%)))
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
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
                                (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%)
                                temp)))
                     label10))))
               (setf jy (f2cl-lib:int-add jy incy))
              label20)))
          (t
           (cond
             ((> incx 0)
              (setf kx 1))
             (t
              (setf kx
                      (f2cl-lib:int-sub 1
                                        (f2cl-lib:int-mul
                                         (f2cl-lib:int-sub m 1)
                                         incx)))))
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (cond
                 ((/= (f2cl-lib:fref y (jy) ((1 *))) zero)
                  (setf temp
                          (* alpha
                             (f2cl-lib:fref y-%data% (jy) ((1 *)) y-%offset%)))
                  (setf ix kx)
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
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
                     label30))))
               (setf jy (f2cl-lib:int-add jy incy))
              label40))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dger fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (double-float) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::xerbla))))

