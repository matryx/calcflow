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


(let* ((one 1.0))
  (declare (type (double-float 1.0 1.0) one) (ignorable one))
  (defun dlarfb (side trans direct storev m n k v ldv t$ ldt c ldc work ldwork)
    (declare (type (array double-float (*)) work c t$ v)
             (type (f2cl-lib:integer4) ldwork ldc ldt ldv k n m)
             (type (simple-string *) storev direct trans side))
    (f2cl-lib:with-multi-array-data
        ((side character side-%data% side-%offset%)
         (trans character trans-%data% trans-%offset%)
         (direct character direct-%data% direct-%offset%)
         (storev character storev-%data% storev-%offset%)
         (v double-float v-%data% v-%offset%)
         (t$ double-float t$-%data% t$-%offset%)
         (c double-float c-%data% c-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((i 0) (j 0)
             (transt
              (make-array '(1) :element-type 'character :initial-element #\ )))
        (declare (type (f2cl-lib:integer4) i j)
                 (type (simple-string 1) transt))
        (if (or (<= m 0) (<= n 0)) (go end_label))
        (cond
          ((lsame trans "N")
           (f2cl-lib:f2cl-set-string transt "T" (string 1)))
          (t
           (f2cl-lib:f2cl-set-string transt "N" (string 1))))
        (cond
          ((lsame storev "C")
           (cond
             ((lsame direct "F")
              (cond
                ((lsame side "L")
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j k) nil)
                   (tagbody
                     (dcopy n
                      (f2cl-lib:array-slice c-%data%
                                            double-float
                                            (j 1)
                                            ((1 ldc) (1 *))
                                            c-%offset%)
                      ldc
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (1 j)
                                            ((1 ldwork) (1 *))
                                            work-%offset%)
                      1)
                    label10))
                 (dtrmm "Right" "Lower" "No transpose" "Unit" n k one v ldv
                  work ldwork)
                 (cond
                   ((> m k)
                    (dgemm "Transpose" "No transpose" n k
                     (f2cl-lib:int-sub m k) one
                     (f2cl-lib:array-slice c-%data%
                                           double-float
                                           ((+ k 1) 1)
                                           ((1 ldc) (1 *))
                                           c-%offset%)
                     ldc
                     (f2cl-lib:array-slice v-%data%
                                           double-float
                                           ((+ k 1) 1)
                                           ((1 ldv) (1 *))
                                           v-%offset%)
                     ldv one work ldwork)))
                 (dtrmm "Right" "Upper" transt "Non-unit" n k one t$ ldt work
                  ldwork)
                 (cond
                   ((> m k)
                    (dgemm "No transpose" "Transpose" (f2cl-lib:int-sub m k) n
                     k (- one)
                     (f2cl-lib:array-slice v-%data%
                                           double-float
                                           ((+ k 1) 1)
                                           ((1 ldv) (1 *))
                                           v-%offset%)
                     ldv work ldwork one
                     (f2cl-lib:array-slice c-%data%
                                           double-float
                                           ((+ k 1) 1)
                                           ((1 ldc) (1 *))
                                           c-%offset%)
                     ldc)))
                 (dtrmm "Right" "Lower" "Transpose" "Unit" n k one v ldv work
                  ldwork)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j k) nil)
                   (tagbody
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i n) nil)
                       (tagbody
                         (setf (f2cl-lib:fref c-%data%
                                              (j i)
                                              ((1 ldc) (1 *))
                                              c-%offset%)
                                 (-
                                  (f2cl-lib:fref c-%data%
                                                 (j i)
                                                 ((1 ldc) (1 *))
                                                 c-%offset%)
                                  (f2cl-lib:fref work-%data%
                                                 (i j)
                                                 ((1 ldwork) (1 *))
                                                 work-%offset%)))
                        label20))
                    label30)))
                ((lsame side "R")
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j k) nil)
                   (tagbody
                     (dcopy m
                      (f2cl-lib:array-slice c-%data%
                                            double-float
                                            (1 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%)
                      1
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (1 j)
                                            ((1 ldwork) (1 *))
                                            work-%offset%)
                      1)
                    label40))
                 (dtrmm "Right" "Lower" "No transpose" "Unit" m k one v ldv
                  work ldwork)
                 (cond
                   ((> n k)
                    (dgemm "No transpose" "No transpose" m k
                     (f2cl-lib:int-sub n k) one
                     (f2cl-lib:array-slice c-%data%
                                           double-float
                                           (1 (f2cl-lib:int-add k 1))
                                           ((1 ldc) (1 *))
                                           c-%offset%)
                     ldc
                     (f2cl-lib:array-slice v-%data%
                                           double-float
                                           ((+ k 1) 1)
                                           ((1 ldv) (1 *))
                                           v-%offset%)
                     ldv one work ldwork)))
                 (dtrmm "Right" "Upper" trans "Non-unit" m k one t$ ldt work
                  ldwork)
                 (cond
                   ((> n k)
                    (dgemm "No transpose" "Transpose" m (f2cl-lib:int-sub n k)
                     k (- one) work ldwork
                     (f2cl-lib:array-slice v-%data%
                                           double-float
                                           ((+ k 1) 1)
                                           ((1 ldv) (1 *))
                                           v-%offset%)
                     ldv one
                     (f2cl-lib:array-slice c-%data%
                                           double-float
                                           (1 (f2cl-lib:int-add k 1))
                                           ((1 ldc) (1 *))
                                           c-%offset%)
                     ldc)))
                 (dtrmm "Right" "Lower" "Transpose" "Unit" m k one v ldv work
                  ldwork)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j k) nil)
                   (tagbody
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i m) nil)
                       (tagbody
                         (setf (f2cl-lib:fref c-%data%
                                              (i j)
                                              ((1 ldc) (1 *))
                                              c-%offset%)
                                 (-
                                  (f2cl-lib:fref c-%data%
                                                 (i j)
                                                 ((1 ldc) (1 *))
                                                 c-%offset%)
                                  (f2cl-lib:fref work-%data%
                                                 (i j)
                                                 ((1 ldwork) (1 *))
                                                 work-%offset%)))
                        label50))
                    label60)))))
             (t
              (cond
                ((lsame side "L")
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j k) nil)
                   (tagbody
                     (dcopy n
                      (f2cl-lib:array-slice c-%data%
                                            double-float
                                            ((+ m (f2cl-lib:int-sub k) j) 1)
                                            ((1 ldc) (1 *))
                                            c-%offset%)
                      ldc
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (1 j)
                                            ((1 ldwork) (1 *))
                                            work-%offset%)
                      1)
                    label70))
                 (dtrmm "Right" "Upper" "No transpose" "Unit" n k one
                  (f2cl-lib:array-slice v-%data%
                                        double-float
                                        ((+ m (f2cl-lib:int-sub k) 1) 1)
                                        ((1 ldv) (1 *))
                                        v-%offset%)
                  ldv work ldwork)
                 (cond
                   ((> m k)
                    (dgemm "Transpose" "No transpose" n k
                     (f2cl-lib:int-sub m k) one c ldc v ldv one work ldwork)))
                 (dtrmm "Right" "Lower" transt "Non-unit" n k one t$ ldt work
                  ldwork)
                 (cond
                   ((> m k)
                    (dgemm "No transpose" "Transpose" (f2cl-lib:int-sub m k) n
                     k (- one) v ldv work ldwork one c ldc)))
                 (dtrmm "Right" "Upper" "Transpose" "Unit" n k one
                  (f2cl-lib:array-slice v-%data%
                                        double-float
                                        ((+ m (f2cl-lib:int-sub k) 1) 1)
                                        ((1 ldv) (1 *))
                                        v-%offset%)
                  ldv work ldwork)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j k) nil)
                   (tagbody
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i n) nil)
                       (tagbody
                         (setf (f2cl-lib:fref c-%data%
                                              ((f2cl-lib:int-add
                                                (f2cl-lib:int-sub m k)
                                                j)
                                               i)
                                              ((1 ldc) (1 *))
                                              c-%offset%)
                                 (-
                                  (f2cl-lib:fref c-%data%
                                                 ((f2cl-lib:int-add
                                                   (f2cl-lib:int-sub m k)
                                                   j)
                                                  i)
                                                 ((1 ldc) (1 *))
                                                 c-%offset%)
                                  (f2cl-lib:fref work-%data%
                                                 (i j)
                                                 ((1 ldwork) (1 *))
                                                 work-%offset%)))
                        label80))
                    label90)))
                ((lsame side "R")
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j k) nil)
                   (tagbody
                     (dcopy m
                      (f2cl-lib:array-slice c-%data%
                                            double-float
                                            (1
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-sub n k)
                                              j))
                                            ((1 ldc) (1 *))
                                            c-%offset%)
                      1
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (1 j)
                                            ((1 ldwork) (1 *))
                                            work-%offset%)
                      1)
                    label100))
                 (dtrmm "Right" "Upper" "No transpose" "Unit" m k one
                  (f2cl-lib:array-slice v-%data%
                                        double-float
                                        ((+ n (f2cl-lib:int-sub k) 1) 1)
                                        ((1 ldv) (1 *))
                                        v-%offset%)
                  ldv work ldwork)
                 (cond
                   ((> n k)
                    (dgemm "No transpose" "No transpose" m k
                     (f2cl-lib:int-sub n k) one c ldc v ldv one work ldwork)))
                 (dtrmm "Right" "Lower" trans "Non-unit" m k one t$ ldt work
                  ldwork)
                 (cond
                   ((> n k)
                    (dgemm "No transpose" "Transpose" m (f2cl-lib:int-sub n k)
                     k (- one) work ldwork v ldv one c ldc)))
                 (dtrmm "Right" "Upper" "Transpose" "Unit" m k one
                  (f2cl-lib:array-slice v-%data%
                                        double-float
                                        ((+ n (f2cl-lib:int-sub k) 1) 1)
                                        ((1 ldv) (1 *))
                                        v-%offset%)
                  ldv work ldwork)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j k) nil)
                   (tagbody
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i m) nil)
                       (tagbody
                         (setf (f2cl-lib:fref c-%data%
                                              (i
                                               (f2cl-lib:int-add
                                                (f2cl-lib:int-sub n k)
                                                j))
                                              ((1 ldc) (1 *))
                                              c-%offset%)
                                 (-
                                  (f2cl-lib:fref c-%data%
                                                 (i
                                                  (f2cl-lib:int-add
                                                   (f2cl-lib:int-sub n k)
                                                   j))
                                                 ((1 ldc) (1 *))
                                                 c-%offset%)
                                  (f2cl-lib:fref work-%data%
                                                 (i j)
                                                 ((1 ldwork) (1 *))
                                                 work-%offset%)))
                        label110))
                    label120)))))))
          ((lsame storev "R")
           (cond
             ((lsame direct "F")
              (cond
                ((lsame side "L")
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j k) nil)
                   (tagbody
                     (dcopy n
                      (f2cl-lib:array-slice c-%data%
                                            double-float
                                            (j 1)
                                            ((1 ldc) (1 *))
                                            c-%offset%)
                      ldc
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (1 j)
                                            ((1 ldwork) (1 *))
                                            work-%offset%)
                      1)
                    label130))
                 (dtrmm "Right" "Upper" "Transpose" "Unit" n k one v ldv work
                  ldwork)
                 (cond
                   ((> m k)
                    (dgemm "Transpose" "Transpose" n k (f2cl-lib:int-sub m k)
                     one
                     (f2cl-lib:array-slice c-%data%
                                           double-float
                                           ((+ k 1) 1)
                                           ((1 ldc) (1 *))
                                           c-%offset%)
                     ldc
                     (f2cl-lib:array-slice v-%data%
                                           double-float
                                           (1 (f2cl-lib:int-add k 1))
                                           ((1 ldv) (1 *))
                                           v-%offset%)
                     ldv one work ldwork)))
                 (dtrmm "Right" "Upper" transt "Non-unit" n k one t$ ldt work
                  ldwork)
                 (cond
                   ((> m k)
                    (dgemm "Transpose" "Transpose" (f2cl-lib:int-sub m k) n k
                     (- one)
                     (f2cl-lib:array-slice v-%data%
                                           double-float
                                           (1 (f2cl-lib:int-add k 1))
                                           ((1 ldv) (1 *))
                                           v-%offset%)
                     ldv work ldwork one
                     (f2cl-lib:array-slice c-%data%
                                           double-float
                                           ((+ k 1) 1)
                                           ((1 ldc) (1 *))
                                           c-%offset%)
                     ldc)))
                 (dtrmm "Right" "Upper" "No transpose" "Unit" n k one v ldv
                  work ldwork)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j k) nil)
                   (tagbody
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i n) nil)
                       (tagbody
                         (setf (f2cl-lib:fref c-%data%
                                              (j i)
                                              ((1 ldc) (1 *))
                                              c-%offset%)
                                 (-
                                  (f2cl-lib:fref c-%data%
                                                 (j i)
                                                 ((1 ldc) (1 *))
                                                 c-%offset%)
                                  (f2cl-lib:fref work-%data%
                                                 (i j)
                                                 ((1 ldwork) (1 *))
                                                 work-%offset%)))
                        label140))
                    label150)))
                ((lsame side "R")
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j k) nil)
                   (tagbody
                     (dcopy m
                      (f2cl-lib:array-slice c-%data%
                                            double-float
                                            (1 j)
                                            ((1 ldc) (1 *))
                                            c-%offset%)
                      1
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (1 j)
                                            ((1 ldwork) (1 *))
                                            work-%offset%)
                      1)
                    label160))
                 (dtrmm "Right" "Upper" "Transpose" "Unit" m k one v ldv work
                  ldwork)
                 (cond
                   ((> n k)
                    (dgemm "No transpose" "Transpose" m k
                     (f2cl-lib:int-sub n k) one
                     (f2cl-lib:array-slice c-%data%
                                           double-float
                                           (1 (f2cl-lib:int-add k 1))
                                           ((1 ldc) (1 *))
                                           c-%offset%)
                     ldc
                     (f2cl-lib:array-slice v-%data%
                                           double-float
                                           (1 (f2cl-lib:int-add k 1))
                                           ((1 ldv) (1 *))
                                           v-%offset%)
                     ldv one work ldwork)))
                 (dtrmm "Right" "Upper" trans "Non-unit" m k one t$ ldt work
                  ldwork)
                 (cond
                   ((> n k)
                    (dgemm "No transpose" "No transpose" m
                     (f2cl-lib:int-sub n k) k (- one) work ldwork
                     (f2cl-lib:array-slice v-%data%
                                           double-float
                                           (1 (f2cl-lib:int-add k 1))
                                           ((1 ldv) (1 *))
                                           v-%offset%)
                     ldv one
                     (f2cl-lib:array-slice c-%data%
                                           double-float
                                           (1 (f2cl-lib:int-add k 1))
                                           ((1 ldc) (1 *))
                                           c-%offset%)
                     ldc)))
                 (dtrmm "Right" "Upper" "No transpose" "Unit" m k one v ldv
                  work ldwork)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j k) nil)
                   (tagbody
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i m) nil)
                       (tagbody
                         (setf (f2cl-lib:fref c-%data%
                                              (i j)
                                              ((1 ldc) (1 *))
                                              c-%offset%)
                                 (-
                                  (f2cl-lib:fref c-%data%
                                                 (i j)
                                                 ((1 ldc) (1 *))
                                                 c-%offset%)
                                  (f2cl-lib:fref work-%data%
                                                 (i j)
                                                 ((1 ldwork) (1 *))
                                                 work-%offset%)))
                        label170))
                    label180)))))
             (t
              (cond
                ((lsame side "L")
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j k) nil)
                   (tagbody
                     (dcopy n
                      (f2cl-lib:array-slice c-%data%
                                            double-float
                                            ((+ m (f2cl-lib:int-sub k) j) 1)
                                            ((1 ldc) (1 *))
                                            c-%offset%)
                      ldc
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (1 j)
                                            ((1 ldwork) (1 *))
                                            work-%offset%)
                      1)
                    label190))
                 (dtrmm "Right" "Lower" "Transpose" "Unit" n k one
                  (f2cl-lib:array-slice v-%data%
                                        double-float
                                        (1
                                         (f2cl-lib:int-add
                                          (f2cl-lib:int-sub m k)
                                          1))
                                        ((1 ldv) (1 *))
                                        v-%offset%)
                  ldv work ldwork)
                 (cond
                   ((> m k)
                    (dgemm "Transpose" "Transpose" n k (f2cl-lib:int-sub m k)
                     one c ldc v ldv one work ldwork)))
                 (dtrmm "Right" "Lower" transt "Non-unit" n k one t$ ldt work
                  ldwork)
                 (cond
                   ((> m k)
                    (dgemm "Transpose" "Transpose" (f2cl-lib:int-sub m k) n k
                     (- one) v ldv work ldwork one c ldc)))
                 (dtrmm "Right" "Lower" "No transpose" "Unit" n k one
                  (f2cl-lib:array-slice v-%data%
                                        double-float
                                        (1
                                         (f2cl-lib:int-add
                                          (f2cl-lib:int-sub m k)
                                          1))
                                        ((1 ldv) (1 *))
                                        v-%offset%)
                  ldv work ldwork)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j k) nil)
                   (tagbody
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i n) nil)
                       (tagbody
                         (setf (f2cl-lib:fref c-%data%
                                              ((f2cl-lib:int-add
                                                (f2cl-lib:int-sub m k)
                                                j)
                                               i)
                                              ((1 ldc) (1 *))
                                              c-%offset%)
                                 (-
                                  (f2cl-lib:fref c-%data%
                                                 ((f2cl-lib:int-add
                                                   (f2cl-lib:int-sub m k)
                                                   j)
                                                  i)
                                                 ((1 ldc) (1 *))
                                                 c-%offset%)
                                  (f2cl-lib:fref work-%data%
                                                 (i j)
                                                 ((1 ldwork) (1 *))
                                                 work-%offset%)))
                        label200))
                    label210)))
                ((lsame side "R")
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j k) nil)
                   (tagbody
                     (dcopy m
                      (f2cl-lib:array-slice c-%data%
                                            double-float
                                            (1
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-sub n k)
                                              j))
                                            ((1 ldc) (1 *))
                                            c-%offset%)
                      1
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (1 j)
                                            ((1 ldwork) (1 *))
                                            work-%offset%)
                      1)
                    label220))
                 (dtrmm "Right" "Lower" "Transpose" "Unit" m k one
                  (f2cl-lib:array-slice v-%data%
                                        double-float
                                        (1
                                         (f2cl-lib:int-add
                                          (f2cl-lib:int-sub n k)
                                          1))
                                        ((1 ldv) (1 *))
                                        v-%offset%)
                  ldv work ldwork)
                 (cond
                   ((> n k)
                    (dgemm "No transpose" "Transpose" m k
                     (f2cl-lib:int-sub n k) one c ldc v ldv one work ldwork)))
                 (dtrmm "Right" "Lower" trans "Non-unit" m k one t$ ldt work
                  ldwork)
                 (cond
                   ((> n k)
                    (dgemm "No transpose" "No transpose" m
                     (f2cl-lib:int-sub n k) k (- one) work ldwork v ldv one c
                     ldc)))
                 (dtrmm "Right" "Lower" "No transpose" "Unit" m k one
                  (f2cl-lib:array-slice v-%data%
                                        double-float
                                        (1
                                         (f2cl-lib:int-add
                                          (f2cl-lib:int-sub n k)
                                          1))
                                        ((1 ldv) (1 *))
                                        v-%offset%)
                  ldv work ldwork)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j k) nil)
                   (tagbody
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i m) nil)
                       (tagbody
                         (setf (f2cl-lib:fref c-%data%
                                              (i
                                               (f2cl-lib:int-add
                                                (f2cl-lib:int-sub n k)
                                                j))
                                              ((1 ldc) (1 *))
                                              c-%offset%)
                                 (-
                                  (f2cl-lib:fref c-%data%
                                                 (i
                                                  (f2cl-lib:int-add
                                                   (f2cl-lib:int-sub n k)
                                                   j))
                                                 ((1 ldc) (1 *))
                                                 c-%offset%)
                                  (f2cl-lib:fref work-%data%
                                                 (i j)
                                                 ((1 ldwork) (1 *))
                                                 work-%offset%)))
                        label230))
                    label240))))))))
        (go end_label)
       end_label
        (return
         (values nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlarfb
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string) (simple-string)
                        (simple-string) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil)
           :calls '(fortran-to-lisp::dgemm fortran-to-lisp::dtrmm
                    fortran-to-lisp::dcopy fortran-to-lisp::lsame))))

