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


(let* ((one (f2cl-lib:cmplx 1.0 0.0)) (zero (f2cl-lib:cmplx 0.0 0.0)))
  (declare (type (f2cl-lib:complex16) one)
           (type (f2cl-lib:complex16) zero)
           (ignorable one zero))
  (defun zhemv (uplo n alpha a lda x incx beta y incy)
    (declare (type (array f2cl-lib:complex16 (*)) y x a)
             (type (f2cl-lib:complex16) beta alpha)
             (type (f2cl-lib:integer4) incy incx lda n)
             (type (simple-string *) uplo))
    (f2cl-lib:with-multi-array-data
        ((uplo character uplo-%data% uplo-%offset%)
         (a f2cl-lib:complex16 a-%data% a-%offset%)
         (x f2cl-lib:complex16 x-%data% x-%offset%)
         (y f2cl-lib:complex16 y-%data% y-%offset%))
      (prog ((i 0) (info 0) (ix 0) (iy 0) (j 0) (jx 0) (jy 0) (kx 0) (ky 0)
             (temp1 #C(0.0 0.0)) (temp2 #C(0.0 0.0)))
        (declare (type (f2cl-lib:integer4) i info ix iy j jx jy kx ky)
                 (type (f2cl-lib:complex16) temp1 temp2))
        (setf info 0)
        (cond
          ((and (not (lsame uplo "U")) (not (lsame uplo "L")))
           (setf info 1))
          ((< n 0)
           (setf info 2))
          ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
           (setf info 5))
          ((= incx 0)
           (setf info 7))
          ((= incy 0)
           (setf info 10)))
        (cond
          ((/= info 0)
           (xerbla "ZHEMV " info)
           (go end_label)))
        (if (or (= n 0) (and (= alpha zero) (= beta one))) (go end_label))
        (cond
          ((> incx 0)
           (setf kx 1))
          (t
           (setf kx
                   (f2cl-lib:int-sub 1
                                     (f2cl-lib:int-mul (f2cl-lib:int-sub n 1)
                                                       incx)))))
        (cond
          ((> incy 0)
           (setf ky 1))
          (t
           (setf ky
                   (f2cl-lib:int-sub 1
                                     (f2cl-lib:int-mul (f2cl-lib:int-sub n 1)
                                                       incy)))))
        (cond
          ((/= beta one)
           (cond
             ((= incy 1)
              (cond
                ((= beta zero)
                 (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                               ((> i n) nil)
                   (tagbody
                     (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                             zero)
                    label10)))
                (t
                 (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                               ((> i n) nil)
                   (tagbody
                     (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                             (* beta
                                (f2cl-lib:fref y-%data%
                                               (i)
                                               ((1 *))
                                               y-%offset%)))
                    label20)))))
             (t
              (setf iy ky)
              (cond
                ((= beta zero)
                 (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                               ((> i n) nil)
                   (tagbody
                     (setf (f2cl-lib:fref y-%data% (iy) ((1 *)) y-%offset%)
                             zero)
                     (setf iy (f2cl-lib:int-add iy incy))
                    label30)))
                (t
                 (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                               ((> i n) nil)
                   (tagbody
                     (setf (f2cl-lib:fref y-%data% (iy) ((1 *)) y-%offset%)
                             (* beta
                                (f2cl-lib:fref y-%data%
                                               (iy)
                                               ((1 *))
                                               y-%offset%)))
                     (setf iy (f2cl-lib:int-add iy incy))
                    label40))))))))
        (if (= alpha zero) (go end_label))
        (cond
          ((lsame uplo "U")
           (cond
             ((and (= incx 1) (= incy 1))
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (setf temp1
                          (* alpha
                             (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)))
                  (setf temp2 zero)
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i
                                    (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                                 nil)
                    (tagbody
                      (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                              (+
                               (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                               (* temp1
                                  (f2cl-lib:fref a-%data%
                                                 (i j)
                                                 ((1 lda) (1 *))
                                                 a-%offset%))))
                      (setf temp2
                              (+ temp2
                                 (*
                                  (f2cl-lib:dconjg
                                   (f2cl-lib:fref a-%data%
                                                  (i j)
                                                  ((1 lda) (1 *))
                                                  a-%offset%))
                                  (f2cl-lib:fref x-%data%
                                                 (i)
                                                 ((1 *))
                                                 x-%offset%))))
                     label50))
                  (setf (f2cl-lib:fref y-%data% (j) ((1 *)) y-%offset%)
                          (+ (f2cl-lib:fref y-%data% (j) ((1 *)) y-%offset%)
                             (* temp1
                                (f2cl-lib:dble
                                 (f2cl-lib:fref a-%data%
                                                (j j)
                                                ((1 lda) (1 *))
                                                a-%offset%)))
                             (* alpha temp2)))
                 label60)))
             (t
              (setf jx kx)
              (setf jy ky)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (setf temp1
                          (* alpha
                             (f2cl-lib:fref x-%data% (jx) ((1 *)) x-%offset%)))
                  (setf temp2 zero)
                  (setf ix kx)
                  (setf iy ky)
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i
                                    (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                                 nil)
                    (tagbody
                      (setf (f2cl-lib:fref y-%data% (iy) ((1 *)) y-%offset%)
                              (+
                               (f2cl-lib:fref y-%data% (iy) ((1 *)) y-%offset%)
                               (* temp1
                                  (f2cl-lib:fref a-%data%
                                                 (i j)
                                                 ((1 lda) (1 *))
                                                 a-%offset%))))
                      (setf temp2
                              (+ temp2
                                 (*
                                  (f2cl-lib:dconjg
                                   (f2cl-lib:fref a-%data%
                                                  (i j)
                                                  ((1 lda) (1 *))
                                                  a-%offset%))
                                  (f2cl-lib:fref x-%data%
                                                 (ix)
                                                 ((1 *))
                                                 x-%offset%))))
                      (setf ix (f2cl-lib:int-add ix incx))
                      (setf iy (f2cl-lib:int-add iy incy))
                     label70))
                  (setf (f2cl-lib:fref y-%data% (jy) ((1 *)) y-%offset%)
                          (+ (f2cl-lib:fref y-%data% (jy) ((1 *)) y-%offset%)
                             (* temp1
                                (f2cl-lib:dble
                                 (f2cl-lib:fref a-%data%
                                                (j j)
                                                ((1 lda) (1 *))
                                                a-%offset%)))
                             (* alpha temp2)))
                  (setf jx (f2cl-lib:int-add jx incx))
                  (setf jy (f2cl-lib:int-add jy incy))
                 label80)))))
          (t
           (cond
             ((and (= incx 1) (= incy 1))
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (setf temp1
                          (* alpha
                             (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)))
                  (setf temp2 zero)
                  (setf (f2cl-lib:fref y-%data% (j) ((1 *)) y-%offset%)
                          (+ (f2cl-lib:fref y-%data% (j) ((1 *)) y-%offset%)
                             (* temp1
                                (f2cl-lib:dble
                                 (f2cl-lib:fref a-%data%
                                                (j j)
                                                ((1 lda) (1 *))
                                                a-%offset%)))))
                  (f2cl-lib:fdo (i (f2cl-lib:int-add j 1)
                                 (f2cl-lib:int-add i 1))
                                ((> i n) nil)
                    (tagbody
                      (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                              (+
                               (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                               (* temp1
                                  (f2cl-lib:fref a-%data%
                                                 (i j)
                                                 ((1 lda) (1 *))
                                                 a-%offset%))))
                      (setf temp2
                              (+ temp2
                                 (*
                                  (f2cl-lib:dconjg
                                   (f2cl-lib:fref a-%data%
                                                  (i j)
                                                  ((1 lda) (1 *))
                                                  a-%offset%))
                                  (f2cl-lib:fref x-%data%
                                                 (i)
                                                 ((1 *))
                                                 x-%offset%))))
                     label90))
                  (setf (f2cl-lib:fref y-%data% (j) ((1 *)) y-%offset%)
                          (+ (f2cl-lib:fref y-%data% (j) ((1 *)) y-%offset%)
                             (* alpha temp2)))
                 label100)))
             (t
              (setf jx kx)
              (setf jy ky)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (setf temp1
                          (* alpha
                             (f2cl-lib:fref x-%data% (jx) ((1 *)) x-%offset%)))
                  (setf temp2 zero)
                  (setf (f2cl-lib:fref y-%data% (jy) ((1 *)) y-%offset%)
                          (+ (f2cl-lib:fref y-%data% (jy) ((1 *)) y-%offset%)
                             (* temp1
                                (f2cl-lib:dble
                                 (f2cl-lib:fref a-%data%
                                                (j j)
                                                ((1 lda) (1 *))
                                                a-%offset%)))))
                  (setf ix jx)
                  (setf iy jy)
                  (f2cl-lib:fdo (i (f2cl-lib:int-add j 1)
                                 (f2cl-lib:int-add i 1))
                                ((> i n) nil)
                    (tagbody
                      (setf ix (f2cl-lib:int-add ix incx))
                      (setf iy (f2cl-lib:int-add iy incy))
                      (setf (f2cl-lib:fref y-%data% (iy) ((1 *)) y-%offset%)
                              (+
                               (f2cl-lib:fref y-%data% (iy) ((1 *)) y-%offset%)
                               (* temp1
                                  (f2cl-lib:fref a-%data%
                                                 (i j)
                                                 ((1 lda) (1 *))
                                                 a-%offset%))))
                      (setf temp2
                              (+ temp2
                                 (*
                                  (f2cl-lib:dconjg
                                   (f2cl-lib:fref a-%data%
                                                  (i j)
                                                  ((1 lda) (1 *))
                                                  a-%offset%))
                                  (f2cl-lib:fref x-%data%
                                                 (ix)
                                                 ((1 *))
                                                 x-%offset%))))
                     label110))
                  (setf (f2cl-lib:fref y-%data% (jy) ((1 *)) y-%offset%)
                          (+ (f2cl-lib:fref y-%data% (jy) ((1 *)) y-%offset%)
                             (* alpha temp2)))
                  (setf jx (f2cl-lib:int-add jx incx))
                  (setf jy (f2cl-lib:int-add jy incy))
                 label120))))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zhemv fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::complex16)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::complex16)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::xerbla fortran-to-lisp::lsame))))

