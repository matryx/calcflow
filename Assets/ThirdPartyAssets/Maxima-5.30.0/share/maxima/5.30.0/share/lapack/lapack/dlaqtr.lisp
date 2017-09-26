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
  (defun dlaqtr (ltran lreal n t$ ldt b w scale x work info)
    (declare (type (double-float) scale w)
             (type (array double-float (*)) work x b t$)
             (type (f2cl-lib:integer4) info ldt n)
             (type f2cl-lib:logical lreal ltran))
    (f2cl-lib:with-multi-array-data
        ((t$ double-float t$-%data% t$-%offset%)
         (b double-float b-%data% b-%offset%)
         (x double-float x-%data% x-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((d (make-array 4 :element-type 'double-float))
             (v (make-array 4 :element-type 'double-float)) (bignum 0.0)
             (eps 0.0) (rec 0.0) (scaloc 0.0) (si 0.0) (smin 0.0) (sminw 0.0)
             (smlnum 0.0) (sr 0.0) (tjj 0.0) (tmp 0.0) (xj 0.0) (xmax 0.0)
             (xnorm 0.0) (z 0.0) (i 0) (ierr 0) (j 0) (j1 0) (j2 0) (jnext 0)
             (k 0) (n1 0) (n2 0) (notran nil))
        (declare (type (array double-float (4)) d v)
                 (type (double-float) bignum eps rec scaloc si smin sminw
                                      smlnum sr tjj tmp xj xmax xnorm z)
                 (type (f2cl-lib:integer4) i ierr j j1 j2 jnext k n1 n2)
                 (type f2cl-lib:logical notran))
        (setf notran (not ltran))
        (setf info 0)
        (if (= n 0) (go end_label))
        (setf eps (dlamch "P"))
        (setf smlnum (/ (dlamch "S") eps))
        (setf bignum (/ one smlnum))
        (setf xnorm (dlange "M" n n t$ ldt d))
        (if (not lreal)
            (setf xnorm (max xnorm (abs w) (dlange "M" n 1 b n d))))
        (setf smin (max smlnum (* eps xnorm)))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%) zero)
        (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)
                    (dasum (f2cl-lib:int-sub j 1)
                     (f2cl-lib:array-slice t$-%data%
                                           double-float
                                           (1 j)
                                           ((1 ldt) (1 *))
                                           t$-%offset%)
                     1))
           label10))
        (cond
          ((not lreal)
           (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                         ((> i n) nil)
             (tagbody
               (setf (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%)
                       (+ (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%)
                          (abs
                           (f2cl-lib:fref b-%data% (i) ((1 *)) b-%offset%))))
              label20))))
        (setf n2 (f2cl-lib:int-mul 2 n))
        (setf n1 n)
        (if (not lreal) (setf n1 n2))
        (setf k (idamax n1 x 1))
        (setf xmax (abs (f2cl-lib:fref x-%data% (k) ((1 *)) x-%offset%)))
        (setf scale one)
        (cond
          ((> xmax bignum)
           (setf scale (/ bignum xmax))
           (dscal n1 scale x 1)
           (setf xmax bignum)))
        (cond
          (lreal
           (cond
             (notran
              (setf jnext n)
              (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                            ((> j 1) nil)
                (tagbody
                  (if (> j jnext) (go label30))
                  (setf j1 j)
                  (setf j2 j)
                  (setf jnext (f2cl-lib:int-sub j 1))
                  (cond
                    ((> j 1)
                     (cond
                       ((/=
                         (f2cl-lib:fref t$
                                        (j
                                         (f2cl-lib:int-add j
                                                           (f2cl-lib:int-sub
                                                            1)))
                                        ((1 ldt) (1 *)))
                         zero)
                        (setf j1 (f2cl-lib:int-sub j 1))
                        (setf jnext (f2cl-lib:int-sub j 2))))))
                  (cond
                    ((= j1 j2)
                     (setf xj
                             (abs
                              (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)))
                     (setf tjj
                             (abs
                              (f2cl-lib:fref t$-%data%
                                             (j1 j1)
                                             ((1 ldt) (1 *))
                                             t$-%offset%)))
                     (setf tmp
                             (f2cl-lib:fref t$-%data%
                                            (j1 j1)
                                            ((1 ldt) (1 *))
                                            t$-%offset%))
                     (cond
                       ((< tjj smin)
                        (setf tmp smin)
                        (setf tjj smin)
                        (setf info 1)))
                     (if (= xj zero) (go label30))
                     (cond
                       ((< tjj one)
                        (cond
                          ((> xj (* bignum tjj))
                           (setf rec (/ one xj))
                           (dscal n rec x 1)
                           (setf scale (* scale rec))
                           (setf xmax (* xmax rec))))))
                     (setf (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                             (/
                              (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                              tmp))
                     (setf xj
                             (abs
                              (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)))
                     (cond
                       ((> xj one)
                        (setf rec (/ one xj))
                        (cond
                          ((> (f2cl-lib:fref work (j1) ((1 *)))
                              (* (+ bignum (- xmax)) rec))
                           (dscal n rec x 1)
                           (setf scale (* scale rec))))))
                     (cond
                       ((> j1 1)
                        (daxpy (f2cl-lib:int-sub j1 1)
                         (- (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%))
                         (f2cl-lib:array-slice t$-%data%
                                               double-float
                                               (1 j1)
                                               ((1 ldt) (1 *))
                                               t$-%offset%)
                         1 x 1)
                        (setf k (idamax (f2cl-lib:int-sub j1 1) x 1))
                        (setf xmax
                                (abs
                                 (f2cl-lib:fref x-%data%
                                                (k)
                                                ((1 *))
                                                x-%offset%))))))
                    (t
                     (setf (f2cl-lib:fref d (1 1) ((1 2) (1 2)))
                             (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%))
                     (setf (f2cl-lib:fref d (2 1) ((1 2) (1 2)))
                             (f2cl-lib:fref x-%data% (j2) ((1 *)) x-%offset%))
                     (multiple-value-bind
                           (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                            var-8 var-9 var-10 var-11 var-12 var-13 var-14
                            var-15 var-16 var-17)
                         (dlaln2 f2cl-lib:%false% 2 1 smin one
                          (f2cl-lib:array-slice t$-%data%
                                                double-float
                                                (j1 j1)
                                                ((1 ldt) (1 *))
                                                t$-%offset%)
                          ldt one one d 2 zero zero v 2 scaloc xnorm ierr)
                       (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                        var-6 var-7 var-8 var-9 var-10 var-11
                                        var-12 var-13 var-14))
                       (setf scaloc var-15)
                       (setf xnorm var-16)
                       (setf ierr var-17))
                     (if (/= ierr 0) (setf info 2))
                     (cond
                       ((/= scaloc one)
                        (dscal n scaloc x 1)
                        (setf scale (* scale scaloc))))
                     (setf (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                             (f2cl-lib:fref v (1 1) ((1 2) (1 2))))
                     (setf (f2cl-lib:fref x-%data% (j2) ((1 *)) x-%offset%)
                             (f2cl-lib:fref v (2 1) ((1 2) (1 2))))
                     (setf xj
                             (max (abs (f2cl-lib:fref v (1 1) ((1 2) (1 2))))
                                  (abs (f2cl-lib:fref v (2 1) ((1 2) (1 2))))))
                     (cond
                       ((> xj one)
                        (setf rec (/ one xj))
                        (cond
                          ((>
                            (max (f2cl-lib:fref work (j1) ((1 *)))
                                 (f2cl-lib:fref work (j2) ((1 *))))
                            (* (+ bignum (- xmax)) rec))
                           (dscal n rec x 1)
                           (setf scale (* scale rec))))))
                     (cond
                       ((> j1 1)
                        (daxpy (f2cl-lib:int-sub j1 1)
                         (- (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%))
                         (f2cl-lib:array-slice t$-%data%
                                               double-float
                                               (1 j1)
                                               ((1 ldt) (1 *))
                                               t$-%offset%)
                         1 x 1)
                        (daxpy (f2cl-lib:int-sub j1 1)
                         (- (f2cl-lib:fref x-%data% (j2) ((1 *)) x-%offset%))
                         (f2cl-lib:array-slice t$-%data%
                                               double-float
                                               (1 j2)
                                               ((1 ldt) (1 *))
                                               t$-%offset%)
                         1 x 1)
                        (setf k (idamax (f2cl-lib:int-sub j1 1) x 1))
                        (setf xmax
                                (abs
                                 (f2cl-lib:fref x-%data%
                                                (k)
                                                ((1 *))
                                                x-%offset%)))))))
                 label30)))
             (t
              (setf jnext 1)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (if (< j jnext) (go label40))
                  (setf j1 j)
                  (setf j2 j)
                  (setf jnext (f2cl-lib:int-add j 1))
                  (cond
                    ((< j n)
                     (cond
                       ((/=
                         (f2cl-lib:fref t$
                                        ((f2cl-lib:int-add j 1) j)
                                        ((1 ldt) (1 *)))
                         zero)
                        (setf j2 (f2cl-lib:int-add j 1))
                        (setf jnext (f2cl-lib:int-add j 2))))))
                  (cond
                    ((= j1 j2)
                     (setf xj
                             (abs
                              (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)))
                     (cond
                       ((> xmax one)
                        (setf rec (/ one xmax))
                        (cond
                          ((> (f2cl-lib:fref work (j1) ((1 *)))
                              (* (+ bignum (- xj)) rec))
                           (dscal n rec x 1)
                           (setf scale (* scale rec))
                           (setf xmax (* xmax rec))))))
                     (setf (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                             (-
                              (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                              (ddot (f2cl-lib:int-sub j1 1)
                               (f2cl-lib:array-slice t$-%data%
                                                     double-float
                                                     (1 j1)
                                                     ((1 ldt) (1 *))
                                                     t$-%offset%)
                               1 x 1)))
                     (setf xj
                             (abs
                              (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)))
                     (setf tjj
                             (abs
                              (f2cl-lib:fref t$-%data%
                                             (j1 j1)
                                             ((1 ldt) (1 *))
                                             t$-%offset%)))
                     (setf tmp
                             (f2cl-lib:fref t$-%data%
                                            (j1 j1)
                                            ((1 ldt) (1 *))
                                            t$-%offset%))
                     (cond
                       ((< tjj smin)
                        (setf tmp smin)
                        (setf tjj smin)
                        (setf info 1)))
                     (cond
                       ((< tjj one)
                        (cond
                          ((> xj (* bignum tjj))
                           (setf rec (/ one xj))
                           (dscal n rec x 1)
                           (setf scale (* scale rec))
                           (setf xmax (* xmax rec))))))
                     (setf (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                             (/
                              (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                              tmp))
                     (setf xmax
                             (max xmax
                                  (abs
                                   (f2cl-lib:fref x-%data%
                                                  (j1)
                                                  ((1 *))
                                                  x-%offset%)))))
                    (t
                     (setf xj
                             (max
                              (abs
                               (f2cl-lib:fref x-%data%
                                              (j1)
                                              ((1 *))
                                              x-%offset%))
                              (abs
                               (f2cl-lib:fref x-%data%
                                              (j2)
                                              ((1 *))
                                              x-%offset%))))
                     (cond
                       ((> xmax one)
                        (setf rec (/ one xmax))
                        (cond
                          ((>
                            (max (f2cl-lib:fref work (j2) ((1 *)))
                                 (f2cl-lib:fref work (j1) ((1 *))))
                            (* (+ bignum (- xj)) rec))
                           (dscal n rec x 1)
                           (setf scale (* scale rec))
                           (setf xmax (* xmax rec))))))
                     (setf (f2cl-lib:fref d (1 1) ((1 2) (1 2)))
                             (-
                              (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                              (ddot (f2cl-lib:int-sub j1 1)
                               (f2cl-lib:array-slice t$-%data%
                                                     double-float
                                                     (1 j1)
                                                     ((1 ldt) (1 *))
                                                     t$-%offset%)
                               1 x 1)))
                     (setf (f2cl-lib:fref d (2 1) ((1 2) (1 2)))
                             (-
                              (f2cl-lib:fref x-%data% (j2) ((1 *)) x-%offset%)
                              (ddot (f2cl-lib:int-sub j1 1)
                               (f2cl-lib:array-slice t$-%data%
                                                     double-float
                                                     (1 j2)
                                                     ((1 ldt) (1 *))
                                                     t$-%offset%)
                               1 x 1)))
                     (multiple-value-bind
                           (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                            var-8 var-9 var-10 var-11 var-12 var-13 var-14
                            var-15 var-16 var-17)
                         (dlaln2 f2cl-lib:%true% 2 1 smin one
                          (f2cl-lib:array-slice t$-%data%
                                                double-float
                                                (j1 j1)
                                                ((1 ldt) (1 *))
                                                t$-%offset%)
                          ldt one one d 2 zero zero v 2 scaloc xnorm ierr)
                       (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                        var-6 var-7 var-8 var-9 var-10 var-11
                                        var-12 var-13 var-14))
                       (setf scaloc var-15)
                       (setf xnorm var-16)
                       (setf ierr var-17))
                     (if (/= ierr 0) (setf info 2))
                     (cond
                       ((/= scaloc one)
                        (dscal n scaloc x 1)
                        (setf scale (* scale scaloc))))
                     (setf (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                             (f2cl-lib:fref v (1 1) ((1 2) (1 2))))
                     (setf (f2cl-lib:fref x-%data% (j2) ((1 *)) x-%offset%)
                             (f2cl-lib:fref v (2 1) ((1 2) (1 2))))
                     (setf xmax
                             (max
                              (abs
                               (f2cl-lib:fref x-%data%
                                              (j1)
                                              ((1 *))
                                              x-%offset%))
                              (abs
                               (f2cl-lib:fref x-%data%
                                              (j2)
                                              ((1 *))
                                              x-%offset%))
                              xmax))))
                 label40)))))
          (t
           (setf sminw (max (* eps (abs w)) smin))
           (cond
             (notran
              (setf jnext n)
              (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                            ((> j 1) nil)
                (tagbody
                  (if (> j jnext) (go label70))
                  (setf j1 j)
                  (setf j2 j)
                  (setf jnext (f2cl-lib:int-sub j 1))
                  (cond
                    ((> j 1)
                     (cond
                       ((/=
                         (f2cl-lib:fref t$
                                        (j
                                         (f2cl-lib:int-add j
                                                           (f2cl-lib:int-sub
                                                            1)))
                                        ((1 ldt) (1 *)))
                         zero)
                        (setf j1 (f2cl-lib:int-sub j 1))
                        (setf jnext (f2cl-lib:int-sub j 2))))))
                  (cond
                    ((= j1 j2)
                     (setf z w)
                     (if (= j1 1)
                         (setf z
                                 (f2cl-lib:fref b-%data%
                                                (1)
                                                ((1 *))
                                                b-%offset%)))
                     (setf xj
                             (+
                              (abs
                               (f2cl-lib:fref x-%data%
                                              (j1)
                                              ((1 *))
                                              x-%offset%))
                              (abs
                               (f2cl-lib:fref x-%data%
                                              ((f2cl-lib:int-add n j1))
                                              ((1 *))
                                              x-%offset%))))
                     (setf tjj
                             (+
                              (abs
                               (f2cl-lib:fref t$-%data%
                                              (j1 j1)
                                              ((1 ldt) (1 *))
                                              t$-%offset%))
                              (abs z)))
                     (setf tmp
                             (f2cl-lib:fref t$-%data%
                                            (j1 j1)
                                            ((1 ldt) (1 *))
                                            t$-%offset%))
                     (cond
                       ((< tjj sminw)
                        (setf tmp sminw)
                        (setf tjj sminw)
                        (setf info 1)))
                     (if (= xj zero) (go label70))
                     (cond
                       ((< tjj one)
                        (cond
                          ((> xj (* bignum tjj))
                           (setf rec (/ one xj))
                           (dscal n2 rec x 1)
                           (setf scale (* scale rec))
                           (setf xmax (* xmax rec))))))
                     (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                         (dladiv
                          (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                          (f2cl-lib:fref x-%data%
                                         ((f2cl-lib:int-add n j1))
                                         ((1 *))
                                         x-%offset%)
                          tmp z sr si)
                       (declare (ignore var-0 var-1 var-2 var-3))
                       (setf sr var-4)
                       (setf si var-5))
                     (setf (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%) sr)
                     (setf (f2cl-lib:fref x-%data%
                                          ((f2cl-lib:int-add n j1))
                                          ((1 *))
                                          x-%offset%)
                             si)
                     (setf xj
                             (+
                              (abs
                               (f2cl-lib:fref x-%data%
                                              (j1)
                                              ((1 *))
                                              x-%offset%))
                              (abs
                               (f2cl-lib:fref x-%data%
                                              ((f2cl-lib:int-add n j1))
                                              ((1 *))
                                              x-%offset%))))
                     (cond
                       ((> xj one)
                        (setf rec (/ one xj))
                        (cond
                          ((> (f2cl-lib:fref work (j1) ((1 *)))
                              (* (+ bignum (- xmax)) rec))
                           (dscal n2 rec x 1)
                           (setf scale (* scale rec))))))
                     (cond
                       ((> j1 1)
                        (daxpy (f2cl-lib:int-sub j1 1)
                         (- (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%))
                         (f2cl-lib:array-slice t$-%data%
                                               double-float
                                               (1 j1)
                                               ((1 ldt) (1 *))
                                               t$-%offset%)
                         1 x 1)
                        (daxpy (f2cl-lib:int-sub j1 1)
                         (-
                          (f2cl-lib:fref x-%data%
                                         ((f2cl-lib:int-add n j1))
                                         ((1 *))
                                         x-%offset%))
                         (f2cl-lib:array-slice t$-%data%
                                               double-float
                                               (1 j1)
                                               ((1 ldt) (1 *))
                                               t$-%offset%)
                         1
                         (f2cl-lib:array-slice x-%data%
                                               double-float
                                               ((+ n 1))
                                               ((1 *))
                                               x-%offset%)
                         1)
                        (setf (f2cl-lib:fref x-%data% (1) ((1 *)) x-%offset%)
                                (+
                                 (f2cl-lib:fref x-%data%
                                                (1)
                                                ((1 *))
                                                x-%offset%)
                                 (*
                                  (f2cl-lib:fref b-%data%
                                                 (j1)
                                                 ((1 *))
                                                 b-%offset%)
                                  (f2cl-lib:fref x-%data%
                                                 ((f2cl-lib:int-add n j1))
                                                 ((1 *))
                                                 x-%offset%))))
                        (setf (f2cl-lib:fref x-%data%
                                             ((f2cl-lib:int-add n 1))
                                             ((1 *))
                                             x-%offset%)
                                (-
                                 (f2cl-lib:fref x-%data%
                                                ((f2cl-lib:int-add n 1))
                                                ((1 *))
                                                x-%offset%)
                                 (*
                                  (f2cl-lib:fref b-%data%
                                                 (j1)
                                                 ((1 *))
                                                 b-%offset%)
                                  (f2cl-lib:fref x-%data%
                                                 (j1)
                                                 ((1 *))
                                                 x-%offset%))))
                        (setf xmax zero)
                        (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                      ((> k
                                          (f2cl-lib:int-add j1
                                                            (f2cl-lib:int-sub
                                                             1)))
                                       nil)
                          (tagbody
                            (setf xmax
                                    (max xmax
                                         (+
                                          (abs
                                           (f2cl-lib:fref x-%data%
                                                          (k)
                                                          ((1 *))
                                                          x-%offset%))
                                          (abs
                                           (f2cl-lib:fref x-%data%
                                                          ((f2cl-lib:int-add k
                                                                             n))
                                                          ((1 *))
                                                          x-%offset%)))))
                           label50)))))
                    (t
                     (setf (f2cl-lib:fref d (1 1) ((1 2) (1 2)))
                             (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%))
                     (setf (f2cl-lib:fref d (2 1) ((1 2) (1 2)))
                             (f2cl-lib:fref x-%data% (j2) ((1 *)) x-%offset%))
                     (setf (f2cl-lib:fref d (1 2) ((1 2) (1 2)))
                             (f2cl-lib:fref x-%data%
                                            ((f2cl-lib:int-add n j1))
                                            ((1 *))
                                            x-%offset%))
                     (setf (f2cl-lib:fref d (2 2) ((1 2) (1 2)))
                             (f2cl-lib:fref x-%data%
                                            ((f2cl-lib:int-add n j2))
                                            ((1 *))
                                            x-%offset%))
                     (multiple-value-bind
                           (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                            var-8 var-9 var-10 var-11 var-12 var-13 var-14
                            var-15 var-16 var-17)
                         (dlaln2 f2cl-lib:%false% 2 2 sminw one
                          (f2cl-lib:array-slice t$-%data%
                                                double-float
                                                (j1 j1)
                                                ((1 ldt) (1 *))
                                                t$-%offset%)
                          ldt one one d 2 zero (- w) v 2 scaloc xnorm ierr)
                       (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                        var-6 var-7 var-8 var-9 var-10 var-11
                                        var-12 var-13 var-14))
                       (setf scaloc var-15)
                       (setf xnorm var-16)
                       (setf ierr var-17))
                     (if (/= ierr 0) (setf info 2))
                     (cond
                       ((/= scaloc one)
                        (dscal (f2cl-lib:int-mul 2 n) scaloc x 1)
                        (setf scale (* scaloc scale))))
                     (setf (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                             (f2cl-lib:fref v (1 1) ((1 2) (1 2))))
                     (setf (f2cl-lib:fref x-%data% (j2) ((1 *)) x-%offset%)
                             (f2cl-lib:fref v (2 1) ((1 2) (1 2))))
                     (setf (f2cl-lib:fref x-%data%
                                          ((f2cl-lib:int-add n j1))
                                          ((1 *))
                                          x-%offset%)
                             (f2cl-lib:fref v (1 2) ((1 2) (1 2))))
                     (setf (f2cl-lib:fref x-%data%
                                          ((f2cl-lib:int-add n j2))
                                          ((1 *))
                                          x-%offset%)
                             (f2cl-lib:fref v (2 2) ((1 2) (1 2))))
                     (setf xj
                             (max
                              (+ (abs (f2cl-lib:fref v (1 1) ((1 2) (1 2))))
                                 (abs (f2cl-lib:fref v (1 2) ((1 2) (1 2)))))
                              (+ (abs (f2cl-lib:fref v (2 1) ((1 2) (1 2))))
                                 (abs (f2cl-lib:fref v (2 2) ((1 2) (1 2)))))))
                     (cond
                       ((> xj one)
                        (setf rec (/ one xj))
                        (cond
                          ((>
                            (max (f2cl-lib:fref work (j1) ((1 *)))
                                 (f2cl-lib:fref work (j2) ((1 *))))
                            (* (+ bignum (- xmax)) rec))
                           (dscal n2 rec x 1)
                           (setf scale (* scale rec))))))
                     (cond
                       ((> j1 1)
                        (daxpy (f2cl-lib:int-sub j1 1)
                         (- (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%))
                         (f2cl-lib:array-slice t$-%data%
                                               double-float
                                               (1 j1)
                                               ((1 ldt) (1 *))
                                               t$-%offset%)
                         1 x 1)
                        (daxpy (f2cl-lib:int-sub j1 1)
                         (- (f2cl-lib:fref x-%data% (j2) ((1 *)) x-%offset%))
                         (f2cl-lib:array-slice t$-%data%
                                               double-float
                                               (1 j2)
                                               ((1 ldt) (1 *))
                                               t$-%offset%)
                         1 x 1)
                        (daxpy (f2cl-lib:int-sub j1 1)
                         (-
                          (f2cl-lib:fref x-%data%
                                         ((f2cl-lib:int-add n j1))
                                         ((1 *))
                                         x-%offset%))
                         (f2cl-lib:array-slice t$-%data%
                                               double-float
                                               (1 j1)
                                               ((1 ldt) (1 *))
                                               t$-%offset%)
                         1
                         (f2cl-lib:array-slice x-%data%
                                               double-float
                                               ((+ n 1))
                                               ((1 *))
                                               x-%offset%)
                         1)
                        (daxpy (f2cl-lib:int-sub j1 1)
                         (-
                          (f2cl-lib:fref x-%data%
                                         ((f2cl-lib:int-add n j2))
                                         ((1 *))
                                         x-%offset%))
                         (f2cl-lib:array-slice t$-%data%
                                               double-float
                                               (1 j2)
                                               ((1 ldt) (1 *))
                                               t$-%offset%)
                         1
                         (f2cl-lib:array-slice x-%data%
                                               double-float
                                               ((+ n 1))
                                               ((1 *))
                                               x-%offset%)
                         1)
                        (setf (f2cl-lib:fref x-%data% (1) ((1 *)) x-%offset%)
                                (+
                                 (f2cl-lib:fref x-%data%
                                                (1)
                                                ((1 *))
                                                x-%offset%)
                                 (*
                                  (f2cl-lib:fref b-%data%
                                                 (j1)
                                                 ((1 *))
                                                 b-%offset%)
                                  (f2cl-lib:fref x-%data%
                                                 ((f2cl-lib:int-add n j1))
                                                 ((1 *))
                                                 x-%offset%))
                                 (*
                                  (f2cl-lib:fref b-%data%
                                                 (j2)
                                                 ((1 *))
                                                 b-%offset%)
                                  (f2cl-lib:fref x-%data%
                                                 ((f2cl-lib:int-add n j2))
                                                 ((1 *))
                                                 x-%offset%))))
                        (setf (f2cl-lib:fref x-%data%
                                             ((f2cl-lib:int-add n 1))
                                             ((1 *))
                                             x-%offset%)
                                (-
                                 (f2cl-lib:fref x-%data%
                                                ((f2cl-lib:int-add n 1))
                                                ((1 *))
                                                x-%offset%)
                                 (*
                                  (f2cl-lib:fref b-%data%
                                                 (j1)
                                                 ((1 *))
                                                 b-%offset%)
                                  (f2cl-lib:fref x-%data%
                                                 (j1)
                                                 ((1 *))
                                                 x-%offset%))
                                 (*
                                  (f2cl-lib:fref b-%data%
                                                 (j2)
                                                 ((1 *))
                                                 b-%offset%)
                                  (f2cl-lib:fref x-%data%
                                                 (j2)
                                                 ((1 *))
                                                 x-%offset%))))
                        (setf xmax zero)
                        (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                      ((> k
                                          (f2cl-lib:int-add j1
                                                            (f2cl-lib:int-sub
                                                             1)))
                                       nil)
                          (tagbody
                            (setf xmax
                                    (max
                                     (+
                                      (abs
                                       (f2cl-lib:fref x-%data%
                                                      (k)
                                                      ((1 *))
                                                      x-%offset%))
                                      (abs
                                       (f2cl-lib:fref x-%data%
                                                      ((f2cl-lib:int-add k n))
                                                      ((1 *))
                                                      x-%offset%)))
                                     xmax))
                           label60))))))
                 label70)))
             (t
              (setf jnext 1)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (if (< j jnext) (go label80))
                  (setf j1 j)
                  (setf j2 j)
                  (setf jnext (f2cl-lib:int-add j 1))
                  (cond
                    ((< j n)
                     (cond
                       ((/=
                         (f2cl-lib:fref t$
                                        ((f2cl-lib:int-add j 1) j)
                                        ((1 ldt) (1 *)))
                         zero)
                        (setf j2 (f2cl-lib:int-add j 1))
                        (setf jnext (f2cl-lib:int-add j 2))))))
                  (cond
                    ((= j1 j2)
                     (setf xj
                             (+
                              (abs
                               (f2cl-lib:fref x-%data%
                                              (j1)
                                              ((1 *))
                                              x-%offset%))
                              (abs
                               (f2cl-lib:fref x-%data%
                                              ((f2cl-lib:int-add j1 n))
                                              ((1 *))
                                              x-%offset%))))
                     (cond
                       ((> xmax one)
                        (setf rec (/ one xmax))
                        (cond
                          ((> (f2cl-lib:fref work (j1) ((1 *)))
                              (* (+ bignum (- xj)) rec))
                           (dscal n2 rec x 1)
                           (setf scale (* scale rec))
                           (setf xmax (* xmax rec))))))
                     (setf (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                             (-
                              (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                              (ddot (f2cl-lib:int-sub j1 1)
                               (f2cl-lib:array-slice t$-%data%
                                                     double-float
                                                     (1 j1)
                                                     ((1 ldt) (1 *))
                                                     t$-%offset%)
                               1 x 1)))
                     (setf (f2cl-lib:fref x-%data%
                                          ((f2cl-lib:int-add n j1))
                                          ((1 *))
                                          x-%offset%)
                             (-
                              (f2cl-lib:fref x-%data%
                                             ((f2cl-lib:int-add n j1))
                                             ((1 *))
                                             x-%offset%)
                              (ddot (f2cl-lib:int-sub j1 1)
                               (f2cl-lib:array-slice t$-%data%
                                                     double-float
                                                     (1 j1)
                                                     ((1 ldt) (1 *))
                                                     t$-%offset%)
                               1
                               (f2cl-lib:array-slice x-%data%
                                                     double-float
                                                     ((+ n 1))
                                                     ((1 *))
                                                     x-%offset%)
                               1)))
                     (cond
                       ((> j1 1)
                        (setf (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                                (-
                                 (f2cl-lib:fref x-%data%
                                                (j1)
                                                ((1 *))
                                                x-%offset%)
                                 (*
                                  (f2cl-lib:fref b-%data%
                                                 (j1)
                                                 ((1 *))
                                                 b-%offset%)
                                  (f2cl-lib:fref x-%data%
                                                 ((f2cl-lib:int-add n 1))
                                                 ((1 *))
                                                 x-%offset%))))
                        (setf (f2cl-lib:fref x-%data%
                                             ((f2cl-lib:int-add n j1))
                                             ((1 *))
                                             x-%offset%)
                                (+
                                 (f2cl-lib:fref x-%data%
                                                ((f2cl-lib:int-add n j1))
                                                ((1 *))
                                                x-%offset%)
                                 (*
                                  (f2cl-lib:fref b-%data%
                                                 (j1)
                                                 ((1 *))
                                                 b-%offset%)
                                  (f2cl-lib:fref x-%data%
                                                 (1)
                                                 ((1 *))
                                                 x-%offset%))))))
                     (setf xj
                             (+
                              (abs
                               (f2cl-lib:fref x-%data%
                                              (j1)
                                              ((1 *))
                                              x-%offset%))
                              (abs
                               (f2cl-lib:fref x-%data%
                                              ((f2cl-lib:int-add j1 n))
                                              ((1 *))
                                              x-%offset%))))
                     (setf z w)
                     (if (= j1 1)
                         (setf z
                                 (f2cl-lib:fref b-%data%
                                                (1)
                                                ((1 *))
                                                b-%offset%)))
                     (setf tjj
                             (+
                              (abs
                               (f2cl-lib:fref t$-%data%
                                              (j1 j1)
                                              ((1 ldt) (1 *))
                                              t$-%offset%))
                              (abs z)))
                     (setf tmp
                             (f2cl-lib:fref t$-%data%
                                            (j1 j1)
                                            ((1 ldt) (1 *))
                                            t$-%offset%))
                     (cond
                       ((< tjj sminw)
                        (setf tmp sminw)
                        (setf tjj sminw)
                        (setf info 1)))
                     (cond
                       ((< tjj one)
                        (cond
                          ((> xj (* bignum tjj))
                           (setf rec (/ one xj))
                           (dscal n2 rec x 1)
                           (setf scale (* scale rec))
                           (setf xmax (* xmax rec))))))
                     (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                         (dladiv
                          (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                          (f2cl-lib:fref x-%data%
                                         ((f2cl-lib:int-add n j1))
                                         ((1 *))
                                         x-%offset%)
                          tmp (- z) sr si)
                       (declare (ignore var-0 var-1 var-2 var-3))
                       (setf sr var-4)
                       (setf si var-5))
                     (setf (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%) sr)
                     (setf (f2cl-lib:fref x-%data%
                                          ((f2cl-lib:int-add j1 n))
                                          ((1 *))
                                          x-%offset%)
                             si)
                     (setf xmax
                             (max
                              (+
                               (abs
                                (f2cl-lib:fref x-%data%
                                               (j1)
                                               ((1 *))
                                               x-%offset%))
                               (abs
                                (f2cl-lib:fref x-%data%
                                               ((f2cl-lib:int-add j1 n))
                                               ((1 *))
                                               x-%offset%)))
                              xmax)))
                    (t
                     (setf xj
                             (max
                              (+
                               (abs
                                (f2cl-lib:fref x-%data%
                                               (j1)
                                               ((1 *))
                                               x-%offset%))
                               (abs
                                (f2cl-lib:fref x-%data%
                                               ((f2cl-lib:int-add n j1))
                                               ((1 *))
                                               x-%offset%)))
                              (+
                               (abs
                                (f2cl-lib:fref x-%data%
                                               (j2)
                                               ((1 *))
                                               x-%offset%))
                               (abs
                                (f2cl-lib:fref x-%data%
                                               ((f2cl-lib:int-add n j2))
                                               ((1 *))
                                               x-%offset%)))))
                     (cond
                       ((> xmax one)
                        (setf rec (/ one xmax))
                        (cond
                          ((>
                            (max (f2cl-lib:fref work (j1) ((1 *)))
                                 (f2cl-lib:fref work (j2) ((1 *))))
                            (f2cl-lib:f2cl/ (+ bignum (- xj)) xmax))
                           (dscal n2 rec x 1)
                           (setf scale (* scale rec))
                           (setf xmax (* xmax rec))))))
                     (setf (f2cl-lib:fref d (1 1) ((1 2) (1 2)))
                             (-
                              (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                              (ddot (f2cl-lib:int-sub j1 1)
                               (f2cl-lib:array-slice t$-%data%
                                                     double-float
                                                     (1 j1)
                                                     ((1 ldt) (1 *))
                                                     t$-%offset%)
                               1 x 1)))
                     (setf (f2cl-lib:fref d (2 1) ((1 2) (1 2)))
                             (-
                              (f2cl-lib:fref x-%data% (j2) ((1 *)) x-%offset%)
                              (ddot (f2cl-lib:int-sub j1 1)
                               (f2cl-lib:array-slice t$-%data%
                                                     double-float
                                                     (1 j2)
                                                     ((1 ldt) (1 *))
                                                     t$-%offset%)
                               1 x 1)))
                     (setf (f2cl-lib:fref d (1 2) ((1 2) (1 2)))
                             (-
                              (f2cl-lib:fref x-%data%
                                             ((f2cl-lib:int-add n j1))
                                             ((1 *))
                                             x-%offset%)
                              (ddot (f2cl-lib:int-sub j1 1)
                               (f2cl-lib:array-slice t$-%data%
                                                     double-float
                                                     (1 j1)
                                                     ((1 ldt) (1 *))
                                                     t$-%offset%)
                               1
                               (f2cl-lib:array-slice x-%data%
                                                     double-float
                                                     ((+ n 1))
                                                     ((1 *))
                                                     x-%offset%)
                               1)))
                     (setf (f2cl-lib:fref d (2 2) ((1 2) (1 2)))
                             (-
                              (f2cl-lib:fref x-%data%
                                             ((f2cl-lib:int-add n j2))
                                             ((1 *))
                                             x-%offset%)
                              (ddot (f2cl-lib:int-sub j1 1)
                               (f2cl-lib:array-slice t$-%data%
                                                     double-float
                                                     (1 j2)
                                                     ((1 ldt) (1 *))
                                                     t$-%offset%)
                               1
                               (f2cl-lib:array-slice x-%data%
                                                     double-float
                                                     ((+ n 1))
                                                     ((1 *))
                                                     x-%offset%)
                               1)))
                     (setf (f2cl-lib:fref d (1 1) ((1 2) (1 2)))
                             (- (f2cl-lib:fref d (1 1) ((1 2) (1 2)))
                                (*
                                 (f2cl-lib:fref b-%data%
                                                (j1)
                                                ((1 *))
                                                b-%offset%)
                                 (f2cl-lib:fref x-%data%
                                                ((f2cl-lib:int-add n 1))
                                                ((1 *))
                                                x-%offset%))))
                     (setf (f2cl-lib:fref d (2 1) ((1 2) (1 2)))
                             (- (f2cl-lib:fref d (2 1) ((1 2) (1 2)))
                                (*
                                 (f2cl-lib:fref b-%data%
                                                (j2)
                                                ((1 *))
                                                b-%offset%)
                                 (f2cl-lib:fref x-%data%
                                                ((f2cl-lib:int-add n 1))
                                                ((1 *))
                                                x-%offset%))))
                     (setf (f2cl-lib:fref d (1 2) ((1 2) (1 2)))
                             (+ (f2cl-lib:fref d (1 2) ((1 2) (1 2)))
                                (*
                                 (f2cl-lib:fref b-%data%
                                                (j1)
                                                ((1 *))
                                                b-%offset%)
                                 (f2cl-lib:fref x-%data%
                                                (1)
                                                ((1 *))
                                                x-%offset%))))
                     (setf (f2cl-lib:fref d (2 2) ((1 2) (1 2)))
                             (+ (f2cl-lib:fref d (2 2) ((1 2) (1 2)))
                                (*
                                 (f2cl-lib:fref b-%data%
                                                (j2)
                                                ((1 *))
                                                b-%offset%)
                                 (f2cl-lib:fref x-%data%
                                                (1)
                                                ((1 *))
                                                x-%offset%))))
                     (multiple-value-bind
                           (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                            var-8 var-9 var-10 var-11 var-12 var-13 var-14
                            var-15 var-16 var-17)
                         (dlaln2 f2cl-lib:%true% 2 2 sminw one
                          (f2cl-lib:array-slice t$-%data%
                                                double-float
                                                (j1 j1)
                                                ((1 ldt) (1 *))
                                                t$-%offset%)
                          ldt one one d 2 zero w v 2 scaloc xnorm ierr)
                       (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                        var-6 var-7 var-8 var-9 var-10 var-11
                                        var-12 var-13 var-14))
                       (setf scaloc var-15)
                       (setf xnorm var-16)
                       (setf ierr var-17))
                     (if (/= ierr 0) (setf info 2))
                     (cond
                       ((/= scaloc one)
                        (dscal n2 scaloc x 1)
                        (setf scale (* scaloc scale))))
                     (setf (f2cl-lib:fref x-%data% (j1) ((1 *)) x-%offset%)
                             (f2cl-lib:fref v (1 1) ((1 2) (1 2))))
                     (setf (f2cl-lib:fref x-%data% (j2) ((1 *)) x-%offset%)
                             (f2cl-lib:fref v (2 1) ((1 2) (1 2))))
                     (setf (f2cl-lib:fref x-%data%
                                          ((f2cl-lib:int-add n j1))
                                          ((1 *))
                                          x-%offset%)
                             (f2cl-lib:fref v (1 2) ((1 2) (1 2))))
                     (setf (f2cl-lib:fref x-%data%
                                          ((f2cl-lib:int-add n j2))
                                          ((1 *))
                                          x-%offset%)
                             (f2cl-lib:fref v (2 2) ((1 2) (1 2))))
                     (setf xmax
                             (max
                              (+
                               (abs
                                (f2cl-lib:fref x-%data%
                                               (j1)
                                               ((1 *))
                                               x-%offset%))
                               (abs
                                (f2cl-lib:fref x-%data%
                                               ((f2cl-lib:int-add n j1))
                                               ((1 *))
                                               x-%offset%)))
                              (+
                               (abs
                                (f2cl-lib:fref x-%data%
                                               (j2)
                                               ((1 *))
                                               x-%offset%))
                               (abs
                                (f2cl-lib:fref x-%data%
                                               ((f2cl-lib:int-add n j2))
                                               ((1 *))
                                               x-%offset%)))
                              xmax))))
                 label80))))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil scale nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlaqtr
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(fortran-to-lisp::logical fortran-to-lisp::logical
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (double-float) (double-float) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil fortran-to-lisp::scale
                            nil nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dladiv fortran-to-lisp::ddot
                    fortran-to-lisp::dlaln2 fortran-to-lisp::daxpy
                    fortran-to-lisp::dscal fortran-to-lisp::idamax
                    fortran-to-lisp::dasum fortran-to-lisp::dlange
                    fortran-to-lisp::dlamch))))

