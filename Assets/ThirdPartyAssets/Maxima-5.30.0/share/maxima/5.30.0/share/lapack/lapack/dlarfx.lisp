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


(let* ((zero 0.0) (one 1.0))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (ignorable zero one))
  (defun dlarfx (side m n v tau c ldc work)
    (declare (type (double-float) tau)
             (type (array double-float (*)) work c v)
             (type (f2cl-lib:integer4) ldc n m)
             (type (simple-string *) side))
    (f2cl-lib:with-multi-array-data
        ((side character side-%data% side-%offset%)
         (v double-float v-%data% v-%offset%)
         (c double-float c-%data% c-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((sum 0.0) (t1 0.0) (t10 0.0) (t2 0.0) (t3 0.0) (t4 0.0) (t5 0.0)
             (t6 0.0) (t7 0.0) (t8 0.0) (t9 0.0) (v1 0.0) (v10 0.0) (v2 0.0)
             (v3 0.0) (v4 0.0) (v5 0.0) (v6 0.0) (v7 0.0) (v8 0.0) (v9 0.0)
             (j 0))
        (declare (type (double-float) sum t1 t10 t2 t3 t4 t5 t6 t7 t8 t9 v1 v10
                                      v2 v3 v4 v5 v6 v7 v8 v9)
                 (type (f2cl-lib:integer4) j))
        (if (= tau zero) (go end_label))
        (cond
          ((lsame side "L")
           (tagbody
             (f2cl-lib:computed-goto
              (label10 label30 label50 label70 label90 label110 label130
               label150 label170 label190)
              m)
             (dgemv "Transpose" m n one c ldc v 1 zero work 1)
             (dger m n (- tau) v 1 work 1 c ldc)
             (go label410)
            label10
             (setf t1
                     (+ one
                        (* (- tau)
                           (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%)
                           (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j n) nil)
               (tagbody
                 (setf (f2cl-lib:fref c-%data%
                                      (1 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (* t1
                            (f2cl-lib:fref c-%data%
                                           (1 j)
                                           ((1 ldc) (1 *))
                                           c-%offset%)))
                label20))
             (go label410)
            label30
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j n) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (1 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (2 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (1 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (1 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (2 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (2 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                label40))
             (go label410)
            label50
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (setf v3 (f2cl-lib:fref v-%data% (3) ((1 *)) v-%offset%))
             (setf t3 (* tau v3))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j n) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (1 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (2 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v3
                             (f2cl-lib:fref c-%data%
                                            (3 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (1 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (1 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (2 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (2 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                 (setf (f2cl-lib:fref c-%data%
                                      (3 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (3 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t3)))
                label60))
             (go label410)
            label70
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (setf v3 (f2cl-lib:fref v-%data% (3) ((1 *)) v-%offset%))
             (setf t3 (* tau v3))
             (setf v4 (f2cl-lib:fref v-%data% (4) ((1 *)) v-%offset%))
             (setf t4 (* tau v4))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j n) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (1 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (2 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v3
                             (f2cl-lib:fref c-%data%
                                            (3 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v4
                             (f2cl-lib:fref c-%data%
                                            (4 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (1 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (1 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (2 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (2 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                 (setf (f2cl-lib:fref c-%data%
                                      (3 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (3 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t3)))
                 (setf (f2cl-lib:fref c-%data%
                                      (4 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (4 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t4)))
                label80))
             (go label410)
            label90
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (setf v3 (f2cl-lib:fref v-%data% (3) ((1 *)) v-%offset%))
             (setf t3 (* tau v3))
             (setf v4 (f2cl-lib:fref v-%data% (4) ((1 *)) v-%offset%))
             (setf t4 (* tau v4))
             (setf v5 (f2cl-lib:fref v-%data% (5) ((1 *)) v-%offset%))
             (setf t5 (* tau v5))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j n) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (1 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (2 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v3
                             (f2cl-lib:fref c-%data%
                                            (3 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v4
                             (f2cl-lib:fref c-%data%
                                            (4 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v5
                             (f2cl-lib:fref c-%data%
                                            (5 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (1 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (1 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (2 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (2 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                 (setf (f2cl-lib:fref c-%data%
                                      (3 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (3 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t3)))
                 (setf (f2cl-lib:fref c-%data%
                                      (4 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (4 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t4)))
                 (setf (f2cl-lib:fref c-%data%
                                      (5 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (5 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t5)))
                label100))
             (go label410)
            label110
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (setf v3 (f2cl-lib:fref v-%data% (3) ((1 *)) v-%offset%))
             (setf t3 (* tau v3))
             (setf v4 (f2cl-lib:fref v-%data% (4) ((1 *)) v-%offset%))
             (setf t4 (* tau v4))
             (setf v5 (f2cl-lib:fref v-%data% (5) ((1 *)) v-%offset%))
             (setf t5 (* tau v5))
             (setf v6 (f2cl-lib:fref v-%data% (6) ((1 *)) v-%offset%))
             (setf t6 (* tau v6))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j n) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (1 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (2 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v3
                             (f2cl-lib:fref c-%data%
                                            (3 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v4
                             (f2cl-lib:fref c-%data%
                                            (4 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v5
                             (f2cl-lib:fref c-%data%
                                            (5 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v6
                             (f2cl-lib:fref c-%data%
                                            (6 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (1 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (1 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (2 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (2 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                 (setf (f2cl-lib:fref c-%data%
                                      (3 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (3 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t3)))
                 (setf (f2cl-lib:fref c-%data%
                                      (4 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (4 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t4)))
                 (setf (f2cl-lib:fref c-%data%
                                      (5 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (5 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t5)))
                 (setf (f2cl-lib:fref c-%data%
                                      (6 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (6 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t6)))
                label120))
             (go label410)
            label130
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (setf v3 (f2cl-lib:fref v-%data% (3) ((1 *)) v-%offset%))
             (setf t3 (* tau v3))
             (setf v4 (f2cl-lib:fref v-%data% (4) ((1 *)) v-%offset%))
             (setf t4 (* tau v4))
             (setf v5 (f2cl-lib:fref v-%data% (5) ((1 *)) v-%offset%))
             (setf t5 (* tau v5))
             (setf v6 (f2cl-lib:fref v-%data% (6) ((1 *)) v-%offset%))
             (setf t6 (* tau v6))
             (setf v7 (f2cl-lib:fref v-%data% (7) ((1 *)) v-%offset%))
             (setf t7 (* tau v7))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j n) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (1 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (2 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v3
                             (f2cl-lib:fref c-%data%
                                            (3 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v4
                             (f2cl-lib:fref c-%data%
                                            (4 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v5
                             (f2cl-lib:fref c-%data%
                                            (5 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v6
                             (f2cl-lib:fref c-%data%
                                            (6 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v7
                             (f2cl-lib:fref c-%data%
                                            (7 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (1 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (1 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (2 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (2 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                 (setf (f2cl-lib:fref c-%data%
                                      (3 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (3 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t3)))
                 (setf (f2cl-lib:fref c-%data%
                                      (4 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (4 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t4)))
                 (setf (f2cl-lib:fref c-%data%
                                      (5 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (5 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t5)))
                 (setf (f2cl-lib:fref c-%data%
                                      (6 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (6 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t6)))
                 (setf (f2cl-lib:fref c-%data%
                                      (7 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (7 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t7)))
                label140))
             (go label410)
            label150
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (setf v3 (f2cl-lib:fref v-%data% (3) ((1 *)) v-%offset%))
             (setf t3 (* tau v3))
             (setf v4 (f2cl-lib:fref v-%data% (4) ((1 *)) v-%offset%))
             (setf t4 (* tau v4))
             (setf v5 (f2cl-lib:fref v-%data% (5) ((1 *)) v-%offset%))
             (setf t5 (* tau v5))
             (setf v6 (f2cl-lib:fref v-%data% (6) ((1 *)) v-%offset%))
             (setf t6 (* tau v6))
             (setf v7 (f2cl-lib:fref v-%data% (7) ((1 *)) v-%offset%))
             (setf t7 (* tau v7))
             (setf v8 (f2cl-lib:fref v-%data% (8) ((1 *)) v-%offset%))
             (setf t8 (* tau v8))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j n) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (1 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (2 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v3
                             (f2cl-lib:fref c-%data%
                                            (3 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v4
                             (f2cl-lib:fref c-%data%
                                            (4 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v5
                             (f2cl-lib:fref c-%data%
                                            (5 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v6
                             (f2cl-lib:fref c-%data%
                                            (6 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v7
                             (f2cl-lib:fref c-%data%
                                            (7 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v8
                             (f2cl-lib:fref c-%data%
                                            (8 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (1 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (1 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (2 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (2 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                 (setf (f2cl-lib:fref c-%data%
                                      (3 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (3 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t3)))
                 (setf (f2cl-lib:fref c-%data%
                                      (4 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (4 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t4)))
                 (setf (f2cl-lib:fref c-%data%
                                      (5 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (5 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t5)))
                 (setf (f2cl-lib:fref c-%data%
                                      (6 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (6 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t6)))
                 (setf (f2cl-lib:fref c-%data%
                                      (7 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (7 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t7)))
                 (setf (f2cl-lib:fref c-%data%
                                      (8 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (8 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t8)))
                label160))
             (go label410)
            label170
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (setf v3 (f2cl-lib:fref v-%data% (3) ((1 *)) v-%offset%))
             (setf t3 (* tau v3))
             (setf v4 (f2cl-lib:fref v-%data% (4) ((1 *)) v-%offset%))
             (setf t4 (* tau v4))
             (setf v5 (f2cl-lib:fref v-%data% (5) ((1 *)) v-%offset%))
             (setf t5 (* tau v5))
             (setf v6 (f2cl-lib:fref v-%data% (6) ((1 *)) v-%offset%))
             (setf t6 (* tau v6))
             (setf v7 (f2cl-lib:fref v-%data% (7) ((1 *)) v-%offset%))
             (setf t7 (* tau v7))
             (setf v8 (f2cl-lib:fref v-%data% (8) ((1 *)) v-%offset%))
             (setf t8 (* tau v8))
             (setf v9 (f2cl-lib:fref v-%data% (9) ((1 *)) v-%offset%))
             (setf t9 (* tau v9))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j n) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (1 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (2 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v3
                             (f2cl-lib:fref c-%data%
                                            (3 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v4
                             (f2cl-lib:fref c-%data%
                                            (4 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v5
                             (f2cl-lib:fref c-%data%
                                            (5 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v6
                             (f2cl-lib:fref c-%data%
                                            (6 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v7
                             (f2cl-lib:fref c-%data%
                                            (7 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v8
                             (f2cl-lib:fref c-%data%
                                            (8 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v9
                             (f2cl-lib:fref c-%data%
                                            (9 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (1 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (1 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (2 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (2 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                 (setf (f2cl-lib:fref c-%data%
                                      (3 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (3 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t3)))
                 (setf (f2cl-lib:fref c-%data%
                                      (4 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (4 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t4)))
                 (setf (f2cl-lib:fref c-%data%
                                      (5 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (5 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t5)))
                 (setf (f2cl-lib:fref c-%data%
                                      (6 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (6 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t6)))
                 (setf (f2cl-lib:fref c-%data%
                                      (7 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (7 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t7)))
                 (setf (f2cl-lib:fref c-%data%
                                      (8 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (8 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t8)))
                 (setf (f2cl-lib:fref c-%data%
                                      (9 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (9 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t9)))
                label180))
             (go label410)
            label190
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (setf v3 (f2cl-lib:fref v-%data% (3) ((1 *)) v-%offset%))
             (setf t3 (* tau v3))
             (setf v4 (f2cl-lib:fref v-%data% (4) ((1 *)) v-%offset%))
             (setf t4 (* tau v4))
             (setf v5 (f2cl-lib:fref v-%data% (5) ((1 *)) v-%offset%))
             (setf t5 (* tau v5))
             (setf v6 (f2cl-lib:fref v-%data% (6) ((1 *)) v-%offset%))
             (setf t6 (* tau v6))
             (setf v7 (f2cl-lib:fref v-%data% (7) ((1 *)) v-%offset%))
             (setf t7 (* tau v7))
             (setf v8 (f2cl-lib:fref v-%data% (8) ((1 *)) v-%offset%))
             (setf t8 (* tau v8))
             (setf v9 (f2cl-lib:fref v-%data% (9) ((1 *)) v-%offset%))
             (setf t9 (* tau v9))
             (setf v10 (f2cl-lib:fref v-%data% (10) ((1 *)) v-%offset%))
             (setf t10 (* tau v10))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j n) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (1 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (2 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v3
                             (f2cl-lib:fref c-%data%
                                            (3 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v4
                             (f2cl-lib:fref c-%data%
                                            (4 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v5
                             (f2cl-lib:fref c-%data%
                                            (5 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v6
                             (f2cl-lib:fref c-%data%
                                            (6 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v7
                             (f2cl-lib:fref c-%data%
                                            (7 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v8
                             (f2cl-lib:fref c-%data%
                                            (8 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v9
                             (f2cl-lib:fref c-%data%
                                            (9 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v10
                             (f2cl-lib:fref c-%data%
                                            (10 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (1 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (1 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (2 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (2 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                 (setf (f2cl-lib:fref c-%data%
                                      (3 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (3 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t3)))
                 (setf (f2cl-lib:fref c-%data%
                                      (4 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (4 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t4)))
                 (setf (f2cl-lib:fref c-%data%
                                      (5 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (5 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t5)))
                 (setf (f2cl-lib:fref c-%data%
                                      (6 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (6 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t6)))
                 (setf (f2cl-lib:fref c-%data%
                                      (7 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (7 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t7)))
                 (setf (f2cl-lib:fref c-%data%
                                      (8 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (8 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t8)))
                 (setf (f2cl-lib:fref c-%data%
                                      (9 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (9 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t9)))
                 (setf (f2cl-lib:fref c-%data%
                                      (10 j)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (10 j)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t10)))
                label200))
             (go label410)))
          (t
           (tagbody
             (f2cl-lib:computed-goto
              (label210 label230 label250 label270 label290 label310 label330
               label350 label370 label390)
              n)
             (dgemv "No transpose" m n one c ldc v 1 zero work 1)
             (dger m n (- tau) work 1 v 1 c ldc)
             (go label410)
            label210
             (setf t1
                     (+ one
                        (* (- tau)
                           (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%)
                           (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j m) nil)
               (tagbody
                 (setf (f2cl-lib:fref c-%data%
                                      (j 1)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (* t1
                            (f2cl-lib:fref c-%data%
                                           (j 1)
                                           ((1 ldc) (1 *))
                                           c-%offset%)))
                label220))
             (go label410)
            label230
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j m) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (j 1)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (j 2)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 1)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 1)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 2)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 2)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                label240))
             (go label410)
            label250
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (setf v3 (f2cl-lib:fref v-%data% (3) ((1 *)) v-%offset%))
             (setf t3 (* tau v3))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j m) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (j 1)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (j 2)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v3
                             (f2cl-lib:fref c-%data%
                                            (j 3)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 1)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 1)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 2)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 2)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 3)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 3)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t3)))
                label260))
             (go label410)
            label270
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (setf v3 (f2cl-lib:fref v-%data% (3) ((1 *)) v-%offset%))
             (setf t3 (* tau v3))
             (setf v4 (f2cl-lib:fref v-%data% (4) ((1 *)) v-%offset%))
             (setf t4 (* tau v4))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j m) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (j 1)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (j 2)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v3
                             (f2cl-lib:fref c-%data%
                                            (j 3)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v4
                             (f2cl-lib:fref c-%data%
                                            (j 4)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 1)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 1)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 2)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 2)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 3)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 3)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t3)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 4)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 4)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t4)))
                label280))
             (go label410)
            label290
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (setf v3 (f2cl-lib:fref v-%data% (3) ((1 *)) v-%offset%))
             (setf t3 (* tau v3))
             (setf v4 (f2cl-lib:fref v-%data% (4) ((1 *)) v-%offset%))
             (setf t4 (* tau v4))
             (setf v5 (f2cl-lib:fref v-%data% (5) ((1 *)) v-%offset%))
             (setf t5 (* tau v5))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j m) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (j 1)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (j 2)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v3
                             (f2cl-lib:fref c-%data%
                                            (j 3)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v4
                             (f2cl-lib:fref c-%data%
                                            (j 4)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v5
                             (f2cl-lib:fref c-%data%
                                            (j 5)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 1)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 1)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 2)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 2)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 3)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 3)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t3)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 4)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 4)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t4)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 5)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 5)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t5)))
                label300))
             (go label410)
            label310
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (setf v3 (f2cl-lib:fref v-%data% (3) ((1 *)) v-%offset%))
             (setf t3 (* tau v3))
             (setf v4 (f2cl-lib:fref v-%data% (4) ((1 *)) v-%offset%))
             (setf t4 (* tau v4))
             (setf v5 (f2cl-lib:fref v-%data% (5) ((1 *)) v-%offset%))
             (setf t5 (* tau v5))
             (setf v6 (f2cl-lib:fref v-%data% (6) ((1 *)) v-%offset%))
             (setf t6 (* tau v6))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j m) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (j 1)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (j 2)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v3
                             (f2cl-lib:fref c-%data%
                                            (j 3)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v4
                             (f2cl-lib:fref c-%data%
                                            (j 4)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v5
                             (f2cl-lib:fref c-%data%
                                            (j 5)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v6
                             (f2cl-lib:fref c-%data%
                                            (j 6)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 1)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 1)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 2)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 2)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 3)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 3)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t3)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 4)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 4)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t4)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 5)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 5)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t5)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 6)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 6)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t6)))
                label320))
             (go label410)
            label330
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (setf v3 (f2cl-lib:fref v-%data% (3) ((1 *)) v-%offset%))
             (setf t3 (* tau v3))
             (setf v4 (f2cl-lib:fref v-%data% (4) ((1 *)) v-%offset%))
             (setf t4 (* tau v4))
             (setf v5 (f2cl-lib:fref v-%data% (5) ((1 *)) v-%offset%))
             (setf t5 (* tau v5))
             (setf v6 (f2cl-lib:fref v-%data% (6) ((1 *)) v-%offset%))
             (setf t6 (* tau v6))
             (setf v7 (f2cl-lib:fref v-%data% (7) ((1 *)) v-%offset%))
             (setf t7 (* tau v7))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j m) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (j 1)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (j 2)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v3
                             (f2cl-lib:fref c-%data%
                                            (j 3)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v4
                             (f2cl-lib:fref c-%data%
                                            (j 4)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v5
                             (f2cl-lib:fref c-%data%
                                            (j 5)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v6
                             (f2cl-lib:fref c-%data%
                                            (j 6)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v7
                             (f2cl-lib:fref c-%data%
                                            (j 7)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 1)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 1)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 2)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 2)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 3)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 3)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t3)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 4)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 4)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t4)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 5)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 5)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t5)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 6)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 6)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t6)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 7)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 7)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t7)))
                label340))
             (go label410)
            label350
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (setf v3 (f2cl-lib:fref v-%data% (3) ((1 *)) v-%offset%))
             (setf t3 (* tau v3))
             (setf v4 (f2cl-lib:fref v-%data% (4) ((1 *)) v-%offset%))
             (setf t4 (* tau v4))
             (setf v5 (f2cl-lib:fref v-%data% (5) ((1 *)) v-%offset%))
             (setf t5 (* tau v5))
             (setf v6 (f2cl-lib:fref v-%data% (6) ((1 *)) v-%offset%))
             (setf t6 (* tau v6))
             (setf v7 (f2cl-lib:fref v-%data% (7) ((1 *)) v-%offset%))
             (setf t7 (* tau v7))
             (setf v8 (f2cl-lib:fref v-%data% (8) ((1 *)) v-%offset%))
             (setf t8 (* tau v8))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j m) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (j 1)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (j 2)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v3
                             (f2cl-lib:fref c-%data%
                                            (j 3)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v4
                             (f2cl-lib:fref c-%data%
                                            (j 4)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v5
                             (f2cl-lib:fref c-%data%
                                            (j 5)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v6
                             (f2cl-lib:fref c-%data%
                                            (j 6)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v7
                             (f2cl-lib:fref c-%data%
                                            (j 7)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v8
                             (f2cl-lib:fref c-%data%
                                            (j 8)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 1)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 1)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 2)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 2)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 3)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 3)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t3)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 4)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 4)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t4)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 5)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 5)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t5)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 6)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 6)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t6)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 7)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 7)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t7)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 8)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 8)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t8)))
                label360))
             (go label410)
            label370
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (setf v3 (f2cl-lib:fref v-%data% (3) ((1 *)) v-%offset%))
             (setf t3 (* tau v3))
             (setf v4 (f2cl-lib:fref v-%data% (4) ((1 *)) v-%offset%))
             (setf t4 (* tau v4))
             (setf v5 (f2cl-lib:fref v-%data% (5) ((1 *)) v-%offset%))
             (setf t5 (* tau v5))
             (setf v6 (f2cl-lib:fref v-%data% (6) ((1 *)) v-%offset%))
             (setf t6 (* tau v6))
             (setf v7 (f2cl-lib:fref v-%data% (7) ((1 *)) v-%offset%))
             (setf t7 (* tau v7))
             (setf v8 (f2cl-lib:fref v-%data% (8) ((1 *)) v-%offset%))
             (setf t8 (* tau v8))
             (setf v9 (f2cl-lib:fref v-%data% (9) ((1 *)) v-%offset%))
             (setf t9 (* tau v9))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j m) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (j 1)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (j 2)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v3
                             (f2cl-lib:fref c-%data%
                                            (j 3)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v4
                             (f2cl-lib:fref c-%data%
                                            (j 4)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v5
                             (f2cl-lib:fref c-%data%
                                            (j 5)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v6
                             (f2cl-lib:fref c-%data%
                                            (j 6)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v7
                             (f2cl-lib:fref c-%data%
                                            (j 7)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v8
                             (f2cl-lib:fref c-%data%
                                            (j 8)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v9
                             (f2cl-lib:fref c-%data%
                                            (j 9)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 1)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 1)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 2)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 2)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 3)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 3)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t3)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 4)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 4)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t4)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 5)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 5)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t5)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 6)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 6)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t6)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 7)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 7)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t7)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 8)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 8)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t8)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 9)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 9)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t9)))
                label380))
             (go label410)
            label390
             (setf v1 (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%))
             (setf t1 (* tau v1))
             (setf v2 (f2cl-lib:fref v-%data% (2) ((1 *)) v-%offset%))
             (setf t2 (* tau v2))
             (setf v3 (f2cl-lib:fref v-%data% (3) ((1 *)) v-%offset%))
             (setf t3 (* tau v3))
             (setf v4 (f2cl-lib:fref v-%data% (4) ((1 *)) v-%offset%))
             (setf t4 (* tau v4))
             (setf v5 (f2cl-lib:fref v-%data% (5) ((1 *)) v-%offset%))
             (setf t5 (* tau v5))
             (setf v6 (f2cl-lib:fref v-%data% (6) ((1 *)) v-%offset%))
             (setf t6 (* tau v6))
             (setf v7 (f2cl-lib:fref v-%data% (7) ((1 *)) v-%offset%))
             (setf t7 (* tau v7))
             (setf v8 (f2cl-lib:fref v-%data% (8) ((1 *)) v-%offset%))
             (setf t8 (* tau v8))
             (setf v9 (f2cl-lib:fref v-%data% (9) ((1 *)) v-%offset%))
             (setf t9 (* tau v9))
             (setf v10 (f2cl-lib:fref v-%data% (10) ((1 *)) v-%offset%))
             (setf t10 (* tau v10))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j m) nil)
               (tagbody
                 (setf sum
                         (+
                          (* v1
                             (f2cl-lib:fref c-%data%
                                            (j 1)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v2
                             (f2cl-lib:fref c-%data%
                                            (j 2)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v3
                             (f2cl-lib:fref c-%data%
                                            (j 3)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v4
                             (f2cl-lib:fref c-%data%
                                            (j 4)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v5
                             (f2cl-lib:fref c-%data%
                                            (j 5)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v6
                             (f2cl-lib:fref c-%data%
                                            (j 6)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v7
                             (f2cl-lib:fref c-%data%
                                            (j 7)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v8
                             (f2cl-lib:fref c-%data%
                                            (j 8)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v9
                             (f2cl-lib:fref c-%data%
                                            (j 9)
                                            ((1 ldc) (1 *))
                                            c-%offset%))
                          (* v10
                             (f2cl-lib:fref c-%data%
                                            (j 10)
                                            ((1 ldc) (1 *))
                                            c-%offset%))))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 1)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 1)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t1)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 2)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 2)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t2)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 3)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 3)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t3)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 4)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 4)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t4)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 5)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 5)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t5)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 6)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 6)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t6)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 7)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 7)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t7)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 8)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 8)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t8)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 9)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 9)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t9)))
                 (setf (f2cl-lib:fref c-%data%
                                      (j 10)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                         (-
                          (f2cl-lib:fref c-%data%
                                         (j 10)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                          (* sum t10)))
                label400))
             (go label410))))
       label410
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlarfx
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (double-float) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::dger fortran-to-lisp::dgemv
                    fortran-to-lisp::lsame))))

