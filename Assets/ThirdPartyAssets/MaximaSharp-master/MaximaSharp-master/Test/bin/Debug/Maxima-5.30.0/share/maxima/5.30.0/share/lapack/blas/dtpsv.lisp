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
  (defun dtpsv (uplo trans diag n ap x incx)
    (declare (type (array double-float (*)) x ap)
             (type (f2cl-lib:integer4) incx n)
             (type (simple-string *) diag trans uplo))
    (f2cl-lib:with-multi-array-data
        ((uplo character uplo-%data% uplo-%offset%)
         (trans character trans-%data% trans-%offset%)
         (diag character diag-%data% diag-%offset%)
         (ap double-float ap-%data% ap-%offset%)
         (x double-float x-%data% x-%offset%))
      (prog ((nounit nil) (i 0) (info 0) (ix 0) (j 0) (jx 0) (k 0) (kk 0)
             (kx 0) (temp 0.0))
        (declare (type f2cl-lib:logical nounit)
                 (type (f2cl-lib:integer4) i info ix j jx k kk kx)
                 (type (double-float) temp))
        (setf info 0)
        (cond
          ((and (not (lsame uplo "U")) (not (lsame uplo "L")))
           (setf info 1))
          ((and (not (lsame trans "N"))
                (not (lsame trans "T"))
                (not (lsame trans "C")))
           (setf info 2))
          ((and (not (lsame diag "U")) (not (lsame diag "N")))
           (setf info 3))
          ((< n 0)
           (setf info 4))
          ((= incx 0)
           (setf info 7)))
        (cond
          ((/= info 0)
           (xerbla "DTPSV " info)
           (go end_label)))
        (if (= n 0) (go end_label))
        (setf nounit (lsame diag "N"))
        (cond
          ((<= incx 0)
           (setf kx
                   (f2cl-lib:int-sub 1
                                     (f2cl-lib:int-mul (f2cl-lib:int-sub n 1)
                                                       incx))))
          ((/= incx 1)
           (setf kx 1)))
        (cond
          ((lsame trans "N")
           (cond
             ((lsame uplo "U")
              (setf kk (the f2cl-lib:integer4 (truncate (* n (+ n 1)) 2)))
              (cond
                ((= incx 1)
                 (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                               ((> j 1) nil)
                   (tagbody
                     (cond
                       ((/= (f2cl-lib:fref x (j) ((1 *))) zero)
                        (if nounit
                            (setf (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)
                                    (/
                                     (f2cl-lib:fref x-%data%
                                                    (j)
                                                    ((1 *))
                                                    x-%offset%)
                                     (f2cl-lib:fref ap-%data%
                                                    (kk)
                                                    ((1 *))
                                                    ap-%offset%))))
                        (setf temp
                                (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%))
                        (setf k (f2cl-lib:int-sub kk 1))
                        (f2cl-lib:fdo (i
                                       (f2cl-lib:int-add j
                                                         (f2cl-lib:int-sub 1))
                                       (f2cl-lib:int-add i
                                                         (f2cl-lib:int-sub 1)))
                                      ((> i 1) nil)
                          (tagbody
                            (setf (f2cl-lib:fref x-%data%
                                                 (i)
                                                 ((1 *))
                                                 x-%offset%)
                                    (-
                                     (f2cl-lib:fref x-%data%
                                                    (i)
                                                    ((1 *))
                                                    x-%offset%)
                                     (* temp
                                        (f2cl-lib:fref ap-%data%
                                                       (k)
                                                       ((1 *))
                                                       ap-%offset%))))
                            (setf k (f2cl-lib:int-sub k 1))
                           label10))))
                     (setf kk (f2cl-lib:int-sub kk j))
                    label20)))
                (t
                 (setf jx
                         (f2cl-lib:int-add kx
                                           (f2cl-lib:int-mul
                                            (f2cl-lib:int-sub n 1)
                                            incx)))
                 (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                               ((> j 1) nil)
                   (tagbody
                     (cond
                       ((/= (f2cl-lib:fref x (jx) ((1 *))) zero)
                        (if nounit
                            (setf (f2cl-lib:fref x-%data%
                                                 (jx)
                                                 ((1 *))
                                                 x-%offset%)
                                    (/
                                     (f2cl-lib:fref x-%data%
                                                    (jx)
                                                    ((1 *))
                                                    x-%offset%)
                                     (f2cl-lib:fref ap-%data%
                                                    (kk)
                                                    ((1 *))
                                                    ap-%offset%))))
                        (setf temp
                                (f2cl-lib:fref x-%data%
                                               (jx)
                                               ((1 *))
                                               x-%offset%))
                        (setf ix jx)
                        (f2cl-lib:fdo (k
                                       (f2cl-lib:int-add kk
                                                         (f2cl-lib:int-sub 1))
                                       (f2cl-lib:int-add k
                                                         (f2cl-lib:int-sub 1)))
                                      ((> k
                                          (f2cl-lib:int-add kk
                                                            (f2cl-lib:int-sub
                                                             j)
                                                            1))
                                       nil)
                          (tagbody
                            (setf ix (f2cl-lib:int-sub ix incx))
                            (setf (f2cl-lib:fref x-%data%
                                                 (ix)
                                                 ((1 *))
                                                 x-%offset%)
                                    (-
                                     (f2cl-lib:fref x-%data%
                                                    (ix)
                                                    ((1 *))
                                                    x-%offset%)
                                     (* temp
                                        (f2cl-lib:fref ap-%data%
                                                       (k)
                                                       ((1 *))
                                                       ap-%offset%))))
                           label30))))
                     (setf jx (f2cl-lib:int-sub jx incx))
                     (setf kk (f2cl-lib:int-sub kk j))
                    label40)))))
             (t
              (setf kk 1)
              (cond
                ((= incx 1)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j n) nil)
                   (tagbody
                     (cond
                       ((/= (f2cl-lib:fref x (j) ((1 *))) zero)
                        (if nounit
                            (setf (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)
                                    (/
                                     (f2cl-lib:fref x-%data%
                                                    (j)
                                                    ((1 *))
                                                    x-%offset%)
                                     (f2cl-lib:fref ap-%data%
                                                    (kk)
                                                    ((1 *))
                                                    ap-%offset%))))
                        (setf temp
                                (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%))
                        (setf k (f2cl-lib:int-add kk 1))
                        (f2cl-lib:fdo (i (f2cl-lib:int-add j 1)
                                       (f2cl-lib:int-add i 1))
                                      ((> i n) nil)
                          (tagbody
                            (setf (f2cl-lib:fref x-%data%
                                                 (i)
                                                 ((1 *))
                                                 x-%offset%)
                                    (-
                                     (f2cl-lib:fref x-%data%
                                                    (i)
                                                    ((1 *))
                                                    x-%offset%)
                                     (* temp
                                        (f2cl-lib:fref ap-%data%
                                                       (k)
                                                       ((1 *))
                                                       ap-%offset%))))
                            (setf k (f2cl-lib:int-add k 1))
                           label50))))
                     (setf kk
                             (f2cl-lib:int-add kk
                                               (f2cl-lib:int-add
                                                (f2cl-lib:int-sub n j)
                                                1)))
                    label60)))
                (t
                 (setf jx kx)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j n) nil)
                   (tagbody
                     (cond
                       ((/= (f2cl-lib:fref x (jx) ((1 *))) zero)
                        (if nounit
                            (setf (f2cl-lib:fref x-%data%
                                                 (jx)
                                                 ((1 *))
                                                 x-%offset%)
                                    (/
                                     (f2cl-lib:fref x-%data%
                                                    (jx)
                                                    ((1 *))
                                                    x-%offset%)
                                     (f2cl-lib:fref ap-%data%
                                                    (kk)
                                                    ((1 *))
                                                    ap-%offset%))))
                        (setf temp
                                (f2cl-lib:fref x-%data%
                                               (jx)
                                               ((1 *))
                                               x-%offset%))
                        (setf ix jx)
                        (f2cl-lib:fdo (k (f2cl-lib:int-add kk 1)
                                       (f2cl-lib:int-add k 1))
                                      ((> k
                                          (f2cl-lib:int-add kk
                                                            n
                                                            (f2cl-lib:int-sub
                                                             j)))
                                       nil)
                          (tagbody
                            (setf ix (f2cl-lib:int-add ix incx))
                            (setf (f2cl-lib:fref x-%data%
                                                 (ix)
                                                 ((1 *))
                                                 x-%offset%)
                                    (-
                                     (f2cl-lib:fref x-%data%
                                                    (ix)
                                                    ((1 *))
                                                    x-%offset%)
                                     (* temp
                                        (f2cl-lib:fref ap-%data%
                                                       (k)
                                                       ((1 *))
                                                       ap-%offset%))))
                           label70))))
                     (setf jx (f2cl-lib:int-add jx incx))
                     (setf kk
                             (f2cl-lib:int-add kk
                                               (f2cl-lib:int-add
                                                (f2cl-lib:int-sub n j)
                                                1)))
                    label80)))))))
          (t
           (cond
             ((lsame uplo "U")
              (setf kk 1)
              (cond
                ((= incx 1)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j n) nil)
                   (tagbody
                     (setf temp
                             (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%))
                     (setf k kk)
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i
                                       (f2cl-lib:int-add j
                                                         (f2cl-lib:int-sub 1)))
                                    nil)
                       (tagbody
                         (setf temp
                                 (- temp
                                    (*
                                     (f2cl-lib:fref ap-%data%
                                                    (k)
                                                    ((1 *))
                                                    ap-%offset%)
                                     (f2cl-lib:fref x-%data%
                                                    (i)
                                                    ((1 *))
                                                    x-%offset%))))
                         (setf k (f2cl-lib:int-add k 1))
                        label90))
                     (if nounit
                         (setf temp
                                 (/ temp
                                    (f2cl-lib:fref ap-%data%
                                                   ((f2cl-lib:int-sub
                                                     (f2cl-lib:int-add kk j)
                                                     1))
                                                   ((1 *))
                                                   ap-%offset%))))
                     (setf (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)
                             temp)
                     (setf kk (f2cl-lib:int-add kk j))
                    label100)))
                (t
                 (setf jx kx)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j n) nil)
                   (tagbody
                     (setf temp
                             (f2cl-lib:fref x-%data% (jx) ((1 *)) x-%offset%))
                     (setf ix kx)
                     (f2cl-lib:fdo (k kk (f2cl-lib:int-add k 1))
                                   ((> k
                                       (f2cl-lib:int-add kk
                                                         j
                                                         (f2cl-lib:int-sub 2)))
                                    nil)
                       (tagbody
                         (setf temp
                                 (- temp
                                    (*
                                     (f2cl-lib:fref ap-%data%
                                                    (k)
                                                    ((1 *))
                                                    ap-%offset%)
                                     (f2cl-lib:fref x-%data%
                                                    (ix)
                                                    ((1 *))
                                                    x-%offset%))))
                         (setf ix (f2cl-lib:int-add ix incx))
                        label110))
                     (if nounit
                         (setf temp
                                 (/ temp
                                    (f2cl-lib:fref ap-%data%
                                                   ((f2cl-lib:int-sub
                                                     (f2cl-lib:int-add kk j)
                                                     1))
                                                   ((1 *))
                                                   ap-%offset%))))
                     (setf (f2cl-lib:fref x-%data% (jx) ((1 *)) x-%offset%)
                             temp)
                     (setf jx (f2cl-lib:int-add jx incx))
                     (setf kk (f2cl-lib:int-add kk j))
                    label120)))))
             (t
              (setf kk (the f2cl-lib:integer4 (truncate (* n (+ n 1)) 2)))
              (cond
                ((= incx 1)
                 (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                               ((> j 1) nil)
                   (tagbody
                     (setf temp
                             (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%))
                     (setf k kk)
                     (f2cl-lib:fdo (i n
                                    (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                                   ((> i (f2cl-lib:int-add j 1)) nil)
                       (tagbody
                         (setf temp
                                 (- temp
                                    (*
                                     (f2cl-lib:fref ap-%data%
                                                    (k)
                                                    ((1 *))
                                                    ap-%offset%)
                                     (f2cl-lib:fref x-%data%
                                                    (i)
                                                    ((1 *))
                                                    x-%offset%))))
                         (setf k (f2cl-lib:int-sub k 1))
                        label130))
                     (if nounit
                         (setf temp
                                 (/ temp
                                    (f2cl-lib:fref ap-%data%
                                                   ((f2cl-lib:int-add
                                                     (f2cl-lib:int-sub kk n)
                                                     j))
                                                   ((1 *))
                                                   ap-%offset%))))
                     (setf (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)
                             temp)
                     (setf kk
                             (f2cl-lib:int-sub kk
                                               (f2cl-lib:int-add
                                                (f2cl-lib:int-sub n j)
                                                1)))
                    label140)))
                (t
                 (setf kx
                         (f2cl-lib:int-add kx
                                           (f2cl-lib:int-mul
                                            (f2cl-lib:int-sub n 1)
                                            incx)))
                 (setf jx kx)
                 (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                               ((> j 1) nil)
                   (tagbody
                     (setf temp
                             (f2cl-lib:fref x-%data% (jx) ((1 *)) x-%offset%))
                     (setf ix kx)
                     (f2cl-lib:fdo (k kk
                                    (f2cl-lib:int-add k (f2cl-lib:int-sub 1)))
                                   ((> k
                                       (f2cl-lib:int-add kk
                                                         (f2cl-lib:int-sub
                                                          (f2cl-lib:int-add n
                                                                            (f2cl-lib:int-sub
                                                                             (f2cl-lib:int-add
                                                                              j
                                                                              1))))))
                                    nil)
                       (tagbody
                         (setf temp
                                 (- temp
                                    (*
                                     (f2cl-lib:fref ap-%data%
                                                    (k)
                                                    ((1 *))
                                                    ap-%offset%)
                                     (f2cl-lib:fref x-%data%
                                                    (ix)
                                                    ((1 *))
                                                    x-%offset%))))
                         (setf ix (f2cl-lib:int-sub ix incx))
                        label150))
                     (if nounit
                         (setf temp
                                 (/ temp
                                    (f2cl-lib:fref ap-%data%
                                                   ((f2cl-lib:int-add
                                                     (f2cl-lib:int-sub kk n)
                                                     j))
                                                   ((1 *))
                                                   ap-%offset%))))
                     (setf (f2cl-lib:fref x-%data% (jx) ((1 *)) x-%offset%)
                             temp)
                     (setf jx (f2cl-lib:int-sub jx incx))
                     (setf kk
                             (f2cl-lib:int-sub kk
                                               (f2cl-lib:int-add
                                                (f2cl-lib:int-sub n j)
                                                1)))
                    label160))))))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dtpsv fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string) (simple-string)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::xerbla fortran-to-lisp::lsame))))

