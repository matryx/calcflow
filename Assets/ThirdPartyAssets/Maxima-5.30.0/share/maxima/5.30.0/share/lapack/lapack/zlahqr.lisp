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


(let* ((itmax 30)
       (zero (f2cl-lib:cmplx 0.0 0.0))
       (one (f2cl-lib:cmplx 1.0 0.0))
       (rzero 0.0)
       (rone 1.0)
       (half 0.5)
       (dat1 (f2cl-lib:f2cl/ 3.0 4.0)))
  (declare (type (f2cl-lib:integer4 30 30) itmax)
           (type (f2cl-lib:complex16) zero)
           (type (f2cl-lib:complex16) one)
           (type (double-float 0.0 0.0) rzero)
           (type (double-float 1.0 1.0) rone)
           (type (double-float 0.5 0.5) half)
           (type (double-float) dat1)
           (ignorable itmax zero one rzero rone half dat1))
  (defun zlahqr (wantt wantz n ilo ihi h ldh w iloz ihiz z ldz info)
    (declare (type (array f2cl-lib:complex16 (*)) z w h)
             (type (f2cl-lib:integer4) info ldz ihiz iloz ldh ihi ilo n)
             (type f2cl-lib:logical wantz wantt))
    (f2cl-lib:with-multi-array-data
        ((h f2cl-lib:complex16 h-%data% h-%offset%)
         (w f2cl-lib:complex16 w-%data% w-%offset%)
         (z f2cl-lib:complex16 z-%data% z-%offset%))
      (labels ((cabs1 (cdum)
                 (+ (abs (f2cl-lib:dble cdum)) (abs (f2cl-lib:dimag cdum)))))
        (declare (ftype (function (f2cl-lib:complex16)
                         (values double-float &rest t))
                        cabs1))
        (prog ((v (make-array 2 :element-type 'f2cl-lib:complex16)) (i 0)
               (i1 0) (i2 0) (its 0) (j 0) (jhi 0) (jlo 0) (k 0) (l 0) (m 0)
               (nh 0) (nz 0) (aa 0.0) (ab 0.0) (ba 0.0) (bb 0.0) (h10 0.0)
               (h21 0.0) (rtemp 0.0) (s 0.0) (safmax 0.0) (safmin 0.0)
               (smlnum 0.0) (sx 0.0) (t2 0.0) (tst 0.0) (ulp 0.0)
               (cdum #C(0.0 0.0)) (h11 #C(0.0 0.0)) (h11s #C(0.0 0.0))
               (h22 #C(0.0 0.0)) (sc #C(0.0 0.0)) (sum #C(0.0 0.0))
               (t$ #C(0.0 0.0)) (t1 #C(0.0 0.0)) (temp #C(0.0 0.0))
               (u #C(0.0 0.0)) (v2 #C(0.0 0.0)) (x #C(0.0 0.0)) (y #C(0.0 0.0))
               (dconjg$ 0.0f0))
          (declare (type (single-float) dconjg$)
                   (type (array f2cl-lib:complex16 (2)) v)
                   (type (f2cl-lib:integer4) i i1 i2 its j jhi jlo k l m nh nz)
                   (type (double-float) aa ab ba bb h10 h21 rtemp s safmax
                                        safmin smlnum sx t2 tst ulp)
                   (type (f2cl-lib:complex16) cdum h11 h11s h22 sc sum t$ t1
                                              temp u v2 x y))
          (setf info 0)
          (if (= n 0) (go end_label))
          (cond
            ((= ilo ihi)
             (setf (f2cl-lib:fref w-%data% (ilo) ((1 *)) w-%offset%)
                     (f2cl-lib:fref h-%data%
                                    (ilo ilo)
                                    ((1 ldh) (1 *))
                                    h-%offset%))
             (go end_label)))
          (f2cl-lib:fdo (j ilo (f2cl-lib:int-add j 1))
                        ((> j (f2cl-lib:int-add ihi (f2cl-lib:int-sub 3))) nil)
            (tagbody
              (setf (f2cl-lib:fref h-%data%
                                   ((f2cl-lib:int-add j 2) j)
                                   ((1 ldh) (1 *))
                                   h-%offset%)
                      zero)
              (setf (f2cl-lib:fref h-%data%
                                   ((f2cl-lib:int-add j 3) j)
                                   ((1 ldh) (1 *))
                                   h-%offset%)
                      zero)
             label10))
          (if (<= ilo (f2cl-lib:int-sub ihi 2))
              (setf (f2cl-lib:fref h-%data%
                                   (ihi (f2cl-lib:int-sub ihi 2))
                                   ((1 ldh) (1 *))
                                   h-%offset%)
                      zero))
          (cond
            (wantt
             (setf jlo 1)
             (setf jhi n))
            (t
             (setf jlo ilo)
             (setf jhi ihi)))
          (f2cl-lib:fdo (i (f2cl-lib:int-add ilo 1) (f2cl-lib:int-add i 1))
                        ((> i ihi) nil)
            (tagbody
              (cond
                ((/=
                  (f2cl-lib:dimag
                   (f2cl-lib:fref h
                                  (i (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                                  ((1 ldh) (1 *))))
                  rzero)
                 (setf sc
                         (/
                          (f2cl-lib:fref h-%data%
                                         (i (f2cl-lib:int-sub i 1))
                                         ((1 ldh) (1 *))
                                         h-%offset%)
                          (cabs1
                           (f2cl-lib:fref h-%data%
                                          (i (f2cl-lib:int-sub i 1))
                                          ((1 ldh) (1 *))
                                          h-%offset%))))
                 (setf sc
                         (coerce (/ (f2cl-lib:dconjg sc) (abs sc))
                                 'f2cl-lib:complex16))
                 (setf (f2cl-lib:fref h-%data%
                                      (i (f2cl-lib:int-sub i 1))
                                      ((1 ldh) (1 *))
                                      h-%offset%)
                         (coerce
                          (abs
                           (f2cl-lib:fref h-%data%
                                          (i (f2cl-lib:int-sub i 1))
                                          ((1 ldh) (1 *))
                                          h-%offset%))
                          'f2cl-lib:complex16))
                 (zscal (f2cl-lib:int-add (f2cl-lib:int-sub jhi i) 1) sc
                  (f2cl-lib:array-slice h-%data%
                                        f2cl-lib:complex16
                                        (i i)
                                        ((1 ldh) (1 *))
                                        h-%offset%)
                  ldh)
                 (zscal
                  (f2cl-lib:int-add
                   (f2cl-lib:int-sub
                    (min (the f2cl-lib:integer4 jhi)
                         (the f2cl-lib:integer4 (f2cl-lib:int-add i 1)))
                    jlo)
                   1)
                  (f2cl-lib:dconjg sc)
                  (f2cl-lib:array-slice h-%data%
                                        f2cl-lib:complex16
                                        (jlo i)
                                        ((1 ldh) (1 *))
                                        h-%offset%)
                  1)
                 (if wantz
                     (zscal (f2cl-lib:int-add (f2cl-lib:int-sub ihiz iloz) 1)
                      (f2cl-lib:dconjg sc)
                      (f2cl-lib:array-slice z-%data%
                                            f2cl-lib:complex16
                                            (iloz i)
                                            ((1 ldz) (1 *))
                                            z-%offset%)
                      1))))
             label20))
          (setf nh (f2cl-lib:int-add (f2cl-lib:int-sub ihi ilo) 1))
          (setf nz (f2cl-lib:int-add (f2cl-lib:int-sub ihiz iloz) 1))
          (setf safmin (dlamch "SAFE MINIMUM"))
          (setf safmax (/ rone safmin))
          (multiple-value-bind (var-0 var-1)
              (dlabad safmin safmax)
            (declare (ignore))
            (setf safmin var-0)
            (setf safmax var-1))
          (setf ulp (dlamch "PRECISION"))
          (setf smlnum (* safmin (/ (f2cl-lib:dble nh) ulp)))
          (cond
            (wantt
             (setf i1 1)
             (setf i2 n)))
          (setf i ihi)
         label30
          (if (< i ilo) (go label150))
          (setf l ilo)
          (f2cl-lib:fdo (its 0 (f2cl-lib:int-add its 1))
                        ((> its itmax) nil)
            (tagbody
              (f2cl-lib:fdo (k i (f2cl-lib:int-add k (f2cl-lib:int-sub 1)))
                            ((> k (f2cl-lib:int-add l 1)) nil)
                (tagbody
                  (if
                   (<=
                    (cabs1
                     (f2cl-lib:fref h-%data%
                                    (k (f2cl-lib:int-sub k 1))
                                    ((1 ldh) (1 *))
                                    h-%offset%))
                    smlnum)
                   (go label50))
                  (setf tst
                          (+
                           (cabs1
                            (f2cl-lib:fref h-%data%
                                           ((f2cl-lib:int-sub k 1)
                                            (f2cl-lib:int-sub k 1))
                                           ((1 ldh) (1 *))
                                           h-%offset%))
                           (cabs1
                            (f2cl-lib:fref h-%data%
                                           (k k)
                                           ((1 ldh) (1 *))
                                           h-%offset%))))
                  (cond
                    ((= tst zero)
                     (if (>= (f2cl-lib:int-sub k 2) ilo)
                         (setf tst
                                 (+ tst
                                    (abs
                                     (f2cl-lib:dble
                                      (f2cl-lib:fref h-%data%
                                                     ((f2cl-lib:int-sub k 1)
                                                      (f2cl-lib:int-sub k 2))
                                                     ((1 ldh) (1 *))
                                                     h-%offset%))))))
                     (if (<= (f2cl-lib:int-add k 1) ihi)
                         (setf tst
                                 (+ tst
                                    (abs
                                     (f2cl-lib:dble
                                      (f2cl-lib:fref h-%data%
                                                     ((f2cl-lib:int-add k 1) k)
                                                     ((1 ldh) (1 *))
                                                     h-%offset%))))))))
                  (cond
                    ((<=
                      (abs
                       (f2cl-lib:dble
                        (f2cl-lib:fref h
                                       (k
                                        (f2cl-lib:int-add k
                                                          (f2cl-lib:int-sub
                                                           1)))
                                       ((1 ldh) (1 *)))))
                      (* ulp tst))
                     (setf ab
                             (max
                              (cabs1
                               (f2cl-lib:fref h-%data%
                                              (k (f2cl-lib:int-sub k 1))
                                              ((1 ldh) (1 *))
                                              h-%offset%))
                              (cabs1
                               (f2cl-lib:fref h-%data%
                                              ((f2cl-lib:int-sub k 1) k)
                                              ((1 ldh) (1 *))
                                              h-%offset%))))
                     (setf ba
                             (min
                              (cabs1
                               (f2cl-lib:fref h-%data%
                                              (k (f2cl-lib:int-sub k 1))
                                              ((1 ldh) (1 *))
                                              h-%offset%))
                              (cabs1
                               (f2cl-lib:fref h-%data%
                                              ((f2cl-lib:int-sub k 1) k)
                                              ((1 ldh) (1 *))
                                              h-%offset%))))
                     (setf aa
                             (max
                              (cabs1
                               (f2cl-lib:fref h-%data%
                                              (k k)
                                              ((1 ldh) (1 *))
                                              h-%offset%))
                              (cabs1
                               (-
                                (f2cl-lib:fref h-%data%
                                               ((f2cl-lib:int-sub k 1)
                                                (f2cl-lib:int-sub k 1))
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                                (f2cl-lib:fref h-%data%
                                               (k k)
                                               ((1 ldh) (1 *))
                                               h-%offset%)))))
                     (setf bb
                             (min
                              (cabs1
                               (f2cl-lib:fref h-%data%
                                              (k k)
                                              ((1 ldh) (1 *))
                                              h-%offset%))
                              (cabs1
                               (-
                                (f2cl-lib:fref h-%data%
                                               ((f2cl-lib:int-sub k 1)
                                                (f2cl-lib:int-sub k 1))
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                                (f2cl-lib:fref h-%data%
                                               (k k)
                                               ((1 ldh) (1 *))
                                               h-%offset%)))))
                     (setf s (+ aa ab))
                     (if
                      (<= (* ba (/ ab s)) (max smlnum (* ulp (* bb (/ aa s)))))
                      (go label50))))
                 label40))
             label50
              (setf l k)
              (cond
                ((> l ilo)
                 (setf (f2cl-lib:fref h-%data%
                                      (l (f2cl-lib:int-sub l 1))
                                      ((1 ldh) (1 *))
                                      h-%offset%)
                         zero)))
              (if (>= l i) (go label140))
              (cond
                ((not wantt)
                 (setf i1 l)
                 (setf i2 i)))
              (cond
                ((= its 10)
                 (setf s
                         (* dat1
                            (abs
                             (f2cl-lib:dble
                              (f2cl-lib:fref h-%data%
                                             ((f2cl-lib:int-add l 1) l)
                                             ((1 ldh) (1 *))
                                             h-%offset%)))))
                 (setf t$
                         (+ s
                            (f2cl-lib:fref h-%data%
                                           (l l)
                                           ((1 ldh) (1 *))
                                           h-%offset%))))
                ((= its 20)
                 (setf s
                         (* dat1
                            (abs
                             (f2cl-lib:dble
                              (f2cl-lib:fref h-%data%
                                             (i (f2cl-lib:int-sub i 1))
                                             ((1 ldh) (1 *))
                                             h-%offset%)))))
                 (setf t$
                         (+ s
                            (f2cl-lib:fref h-%data%
                                           (i i)
                                           ((1 ldh) (1 *))
                                           h-%offset%))))
                (t
                 (setf t$
                         (f2cl-lib:fref h-%data%
                                        (i i)
                                        ((1 ldh) (1 *))
                                        h-%offset%))
                 (setf u
                         (*
                          (f2cl-lib:fsqrt
                           (f2cl-lib:fref h-%data%
                                          ((f2cl-lib:int-sub i 1) i)
                                          ((1 ldh) (1 *))
                                          h-%offset%))
                          (f2cl-lib:fsqrt
                           (f2cl-lib:fref h-%data%
                                          (i (f2cl-lib:int-sub i 1))
                                          ((1 ldh) (1 *))
                                          h-%offset%))))
                 (setf s (cabs1 u))
                 (cond
                   ((/= s rzero)
                    (setf x
                            (* half
                               (-
                                (f2cl-lib:fref h-%data%
                                               ((f2cl-lib:int-sub i 1)
                                                (f2cl-lib:int-sub i 1))
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                                t$)))
                    (setf sx (cabs1 x))
                    (setf s (max s (cabs1 x)))
                    (setf y
                            (* s
                               (f2cl-lib:fsqrt
                                (+ (expt (/ x s) 2) (expt (/ u s) 2)))))
                    (cond
                      ((> sx rzero)
                       (if
                        (<
                         (+ (* (f2cl-lib:dble (/ x sx)) (f2cl-lib:dble y))
                            (* (f2cl-lib:dimag (/ x sx)) (f2cl-lib:dimag y)))
                         rzero)
                        (setf y (- y)))))
                    (setf t$ (- t$ (* u (zladiv u (+ x y)))))))))
              (f2cl-lib:fdo (m (f2cl-lib:int-add i (f2cl-lib:int-sub 1))
                             (f2cl-lib:int-add m (f2cl-lib:int-sub 1)))
                            ((> m (f2cl-lib:int-add l 1)) nil)
                (tagbody
                  (setf h11
                          (f2cl-lib:fref h-%data%
                                         (m m)
                                         ((1 ldh) (1 *))
                                         h-%offset%))
                  (setf h22
                          (f2cl-lib:fref h-%data%
                                         ((f2cl-lib:int-add m 1)
                                          (f2cl-lib:int-add m 1))
                                         ((1 ldh) (1 *))
                                         h-%offset%))
                  (setf h11s (- h11 t$))
                  (setf h21
                          (f2cl-lib:dble
                           (f2cl-lib:fref h-%data%
                                          ((f2cl-lib:int-add m 1) m)
                                          ((1 ldh) (1 *))
                                          h-%offset%)))
                  (setf s (+ (cabs1 h11s) (abs h21)))
                  (setf h11s (/ h11s s))
                  (setf h21 (/ h21 s))
                  (setf (f2cl-lib:fref v (1) ((1 2))) h11s)
                  (setf (f2cl-lib:fref v (2) ((1 2)))
                          (coerce h21 'f2cl-lib:complex16))
                  (setf h10
                          (f2cl-lib:dble
                           (f2cl-lib:fref h-%data%
                                          (m (f2cl-lib:int-sub m 1))
                                          ((1 ldh) (1 *))
                                          h-%offset%)))
                  (if
                   (<= (* (abs h10) (abs h21))
                       (* ulp (* (cabs1 h11s) (+ (cabs1 h11) (cabs1 h22)))))
                   (go label70))
                 label60))
              (setf h11
                      (f2cl-lib:fref h-%data%
                                     (l l)
                                     ((1 ldh) (1 *))
                                     h-%offset%))
              (setf h22
                      (f2cl-lib:fref h-%data%
                                     ((f2cl-lib:int-add l 1)
                                      (f2cl-lib:int-add l 1))
                                     ((1 ldh) (1 *))
                                     h-%offset%))
              (setf h11s (- h11 t$))
              (setf h21
                      (f2cl-lib:dble
                       (f2cl-lib:fref h-%data%
                                      ((f2cl-lib:int-add l 1) l)
                                      ((1 ldh) (1 *))
                                      h-%offset%)))
              (setf s (+ (cabs1 h11s) (abs h21)))
              (setf h11s (/ h11s s))
              (setf h21 (/ h21 s))
              (setf (f2cl-lib:fref v (1) ((1 2))) h11s)
              (setf (f2cl-lib:fref v (2) ((1 2)))
                      (coerce h21 'f2cl-lib:complex16))
             label70
              (f2cl-lib:fdo (k m (f2cl-lib:int-add k 1))
                            ((> k (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                             nil)
                (tagbody
                  (if (> k m)
                      (zcopy 2
                       (f2cl-lib:array-slice h-%data%
                                             f2cl-lib:complex16
                                             (k (f2cl-lib:int-sub k 1))
                                             ((1 ldh) (1 *))
                                             h-%offset%)
                       1 v 1))
                  (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                      (zlarfg 2 (f2cl-lib:fref v (1) ((1 2)))
                       (f2cl-lib:array-slice v f2cl-lib:complex16 (2) ((1 2)))
                       1 t1)
                    (declare (ignore var-0 var-2 var-3))
                    (setf (f2cl-lib:fref v (1) ((1 2))) var-1)
                    (setf t1 var-4))
                  (cond
                    ((> k m)
                     (setf (f2cl-lib:fref h-%data%
                                          (k (f2cl-lib:int-sub k 1))
                                          ((1 ldh) (1 *))
                                          h-%offset%)
                             (f2cl-lib:fref v (1) ((1 2))))
                     (setf (f2cl-lib:fref h-%data%
                                          ((f2cl-lib:int-add k 1)
                                           (f2cl-lib:int-sub k 1))
                                          ((1 ldh) (1 *))
                                          h-%offset%)
                             zero)))
                  (setf v2 (f2cl-lib:fref v (2) ((1 2))))
                  (setf t2 (f2cl-lib:dble (* t1 v2)))
                  (f2cl-lib:fdo (j k (f2cl-lib:int-add j 1))
                                ((> j i2) nil)
                    (tagbody
                      (setf sum
                              (+
                               (* (f2cl-lib:dconjg t1)
                                  (f2cl-lib:fref h-%data%
                                                 (k j)
                                                 ((1 ldh) (1 *))
                                                 h-%offset%))
                               (* t2
                                  (f2cl-lib:fref h-%data%
                                                 ((f2cl-lib:int-add k 1) j)
                                                 ((1 ldh) (1 *))
                                                 h-%offset%))))
                      (setf (f2cl-lib:fref h-%data%
                                           (k j)
                                           ((1 ldh) (1 *))
                                           h-%offset%)
                              (-
                               (f2cl-lib:fref h-%data%
                                              (k j)
                                              ((1 ldh) (1 *))
                                              h-%offset%)
                               sum))
                      (setf (f2cl-lib:fref h-%data%
                                           ((f2cl-lib:int-add k 1) j)
                                           ((1 ldh) (1 *))
                                           h-%offset%)
                              (-
                               (f2cl-lib:fref h-%data%
                                              ((f2cl-lib:int-add k 1) j)
                                              ((1 ldh) (1 *))
                                              h-%offset%)
                               (* sum v2)))
                     label80))
                  (f2cl-lib:fdo (j i1 (f2cl-lib:int-add j 1))
                                ((> j
                                    (min
                                     (the f2cl-lib:integer4
                                          (f2cl-lib:int-add k 2))
                                     (the f2cl-lib:integer4 i)))
                                 nil)
                    (tagbody
                      (setf sum
                              (+
                               (* t1
                                  (f2cl-lib:fref h-%data%
                                                 (j k)
                                                 ((1 ldh) (1 *))
                                                 h-%offset%))
                               (* t2
                                  (f2cl-lib:fref h-%data%
                                                 (j (f2cl-lib:int-add k 1))
                                                 ((1 ldh) (1 *))
                                                 h-%offset%))))
                      (setf (f2cl-lib:fref h-%data%
                                           (j k)
                                           ((1 ldh) (1 *))
                                           h-%offset%)
                              (-
                               (f2cl-lib:fref h-%data%
                                              (j k)
                                              ((1 ldh) (1 *))
                                              h-%offset%)
                               sum))
                      (setf (f2cl-lib:fref h-%data%
                                           (j (f2cl-lib:int-add k 1))
                                           ((1 ldh) (1 *))
                                           h-%offset%)
                              (-
                               (f2cl-lib:fref h-%data%
                                              (j (f2cl-lib:int-add k 1))
                                              ((1 ldh) (1 *))
                                              h-%offset%)
                               (* sum (f2cl-lib:dconjg v2))))
                     label90))
                  (cond
                    (wantz
                     (f2cl-lib:fdo (j iloz (f2cl-lib:int-add j 1))
                                   ((> j ihiz) nil)
                       (tagbody
                         (setf sum
                                 (+
                                  (* t1
                                     (f2cl-lib:fref z-%data%
                                                    (j k)
                                                    ((1 ldz) (1 *))
                                                    z-%offset%))
                                  (* t2
                                     (f2cl-lib:fref z-%data%
                                                    (j (f2cl-lib:int-add k 1))
                                                    ((1 ldz) (1 *))
                                                    z-%offset%))))
                         (setf (f2cl-lib:fref z-%data%
                                              (j k)
                                              ((1 ldz) (1 *))
                                              z-%offset%)
                                 (-
                                  (f2cl-lib:fref z-%data%
                                                 (j k)
                                                 ((1 ldz) (1 *))
                                                 z-%offset%)
                                  sum))
                         (setf (f2cl-lib:fref z-%data%
                                              (j (f2cl-lib:int-add k 1))
                                              ((1 ldz) (1 *))
                                              z-%offset%)
                                 (-
                                  (f2cl-lib:fref z-%data%
                                                 (j (f2cl-lib:int-add k 1))
                                                 ((1 ldz) (1 *))
                                                 z-%offset%)
                                  (* sum (f2cl-lib:dconjg v2))))
                        label100))))
                  (cond
                    ((and (= k m) (> m l))
                     (setf temp (- one t1))
                     (setf temp (/ temp (abs temp)))
                     (setf (f2cl-lib:fref h-%data%
                                          ((f2cl-lib:int-add m 1) m)
                                          ((1 ldh) (1 *))
                                          h-%offset%)
                             (*
                              (f2cl-lib:fref h-%data%
                                             ((f2cl-lib:int-add m 1) m)
                                             ((1 ldh) (1 *))
                                             h-%offset%)
                              (f2cl-lib:dconjg temp)))
                     (if (<= (f2cl-lib:int-add m 2) i)
                         (setf (f2cl-lib:fref h-%data%
                                              ((f2cl-lib:int-add m 2)
                                               (f2cl-lib:int-add m 1))
                                              ((1 ldh) (1 *))
                                              h-%offset%)
                                 (*
                                  (f2cl-lib:fref h-%data%
                                                 ((f2cl-lib:int-add m 2)
                                                  (f2cl-lib:int-add m 1))
                                                 ((1 ldh) (1 *))
                                                 h-%offset%)
                                  temp)))
                     (f2cl-lib:fdo (j m (f2cl-lib:int-add j 1))
                                   ((> j i) nil)
                       (tagbody
                         (cond
                           ((/= j (f2cl-lib:int-add m 1))
                            (if (> i2 j)
                                (zscal (f2cl-lib:int-sub i2 j) temp
                                 (f2cl-lib:array-slice h-%data%
                                                       f2cl-lib:complex16
                                                       (j
                                                        (f2cl-lib:int-add j 1))
                                                       ((1 ldh) (1 *))
                                                       h-%offset%)
                                 ldh))
                            (zscal (f2cl-lib:int-sub j i1)
                             (f2cl-lib:dconjg temp)
                             (f2cl-lib:array-slice h-%data%
                                                   f2cl-lib:complex16
                                                   (i1 j)
                                                   ((1 ldh) (1 *))
                                                   h-%offset%)
                             1)
                            (cond
                              (wantz
                               (zscal nz (f2cl-lib:dconjg temp)
                                (f2cl-lib:array-slice z-%data%
                                                      f2cl-lib:complex16
                                                      (iloz j)
                                                      ((1 ldz) (1 *))
                                                      z-%offset%)
                                1)))))
                        label110))))
                 label120))
              (setf temp
                      (f2cl-lib:fref h-%data%
                                     (i (f2cl-lib:int-sub i 1))
                                     ((1 ldh) (1 *))
                                     h-%offset%))
              (cond
                ((/= (f2cl-lib:dimag temp) rzero)
                 (setf rtemp (abs temp))
                 (setf (f2cl-lib:fref h-%data%
                                      (i (f2cl-lib:int-sub i 1))
                                      ((1 ldh) (1 *))
                                      h-%offset%)
                         (coerce rtemp 'f2cl-lib:complex16))
                 (setf temp (/ temp rtemp))
                 (if (> i2 i)
                     (zscal (f2cl-lib:int-sub i2 i) (f2cl-lib:dconjg temp)
                      (f2cl-lib:array-slice h-%data%
                                            f2cl-lib:complex16
                                            (i (f2cl-lib:int-add i 1))
                                            ((1 ldh) (1 *))
                                            h-%offset%)
                      ldh))
                 (zscal (f2cl-lib:int-sub i i1) temp
                  (f2cl-lib:array-slice h-%data%
                                        f2cl-lib:complex16
                                        (i1 i)
                                        ((1 ldh) (1 *))
                                        h-%offset%)
                  1)
                 (cond
                   (wantz
                    (zscal nz temp
                     (f2cl-lib:array-slice z-%data%
                                           f2cl-lib:complex16
                                           (iloz i)
                                           ((1 ldz) (1 *))
                                           z-%offset%)
                     1)))))
             label130))
          (setf info i)
          (go end_label)
         label140
          (setf (f2cl-lib:fref w-%data% (i) ((1 *)) w-%offset%)
                  (f2cl-lib:fref h-%data% (i i) ((1 ldh) (1 *)) h-%offset%))
          (setf i (f2cl-lib:int-sub l 1))
          (go label30)
         label150
          (go end_label)
         end_label
          (return
           (values nil nil nil nil nil nil nil nil nil nil nil nil info)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zlahqr
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(fortran-to-lisp::logical fortran-to-lisp::logical
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::zlarfg fortran-to-lisp::zcopy
                    fortran-to-lisp::zladiv fortran-to-lisp::dlabad
                    fortran-to-lisp::dlamch fortran-to-lisp::zscal))))

