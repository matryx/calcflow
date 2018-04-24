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


(let* ((zero 0.0) (one 1.0) (half 0.5) (dat1 0.75) (dat2 (- 0.4375)))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (type (double-float 0.5 0.5) half)
           (type (double-float 0.75 0.75) dat1)
           (type (double-float) dat2)
           (ignorable zero one half dat1 dat2))
  (defun dlahqr (wantt wantz n ilo ihi h ldh wr wi iloz ihiz z ldz info)
    (declare (type (array double-float (*)) z wi wr h)
             (type (f2cl-lib:integer4) info ldz ihiz iloz ldh ihi ilo n)
             (type f2cl-lib:logical wantz wantt))
    (f2cl-lib:with-multi-array-data
        ((h double-float h-%data% h-%offset%)
         (wr double-float wr-%data% wr-%offset%)
         (wi double-float wi-%data% wi-%offset%)
         (z double-float z-%data% z-%offset%))
      (prog ((v (make-array 3 :element-type 'double-float))
             (work (make-array 1 :element-type 'double-float)) (ave 0.0)
             (cs 0.0) (disc 0.0) (h00 0.0) (h10 0.0) (h11 0.0) (h12 0.0)
             (h21 0.0) (h22 0.0) (h33 0.0) (h33s 0.0) (h43h34 0.0) (h44 0.0)
             (h44s 0.0) (ovfl 0.0) (s 0.0) (smlnum 0.0) (sn 0.0) (sum 0.0)
             (t1 0.0) (t2 0.0) (t3 0.0) (tst1 0.0) (ulp 0.0) (unfl 0.0)
             (v1 0.0) (v2 0.0) (v3 0.0) (i 0) (i1 0) (i2 0) (itn 0) (its 0)
             (j 0) (k 0) (l 0) (m 0) (nh 0) (nr 0) (nz 0))
        (declare (type (array double-float (3)) v)
                 (type (array double-float (1)) work)
                 (type (double-float) ave cs disc h00 h10 h11 h12 h21 h22 h33
                                      h33s h43h34 h44 h44s ovfl s smlnum sn sum
                                      t1 t2 t3 tst1 ulp unfl v1 v2 v3)
                 (type (f2cl-lib:integer4) i i1 i2 itn its j k l m nh nr nz))
        (setf info 0)
        (if (= n 0) (go end_label))
        (cond
          ((= ilo ihi)
           (setf (f2cl-lib:fref wr-%data% (ilo) ((1 *)) wr-%offset%)
                   (f2cl-lib:fref h-%data%
                                  (ilo ilo)
                                  ((1 ldh) (1 *))
                                  h-%offset%))
           (setf (f2cl-lib:fref wi-%data% (ilo) ((1 *)) wi-%offset%) zero)
           (go end_label)))
        (setf nh (f2cl-lib:int-add (f2cl-lib:int-sub ihi ilo) 1))
        (setf nz (f2cl-lib:int-add (f2cl-lib:int-sub ihiz iloz) 1))
        (setf unfl (dlamch "Safe minimum"))
        (setf ovfl (/ one unfl))
        (multiple-value-bind (var-0 var-1)
            (dlabad unfl ovfl)
          (declare (ignore))
          (setf unfl var-0)
          (setf ovfl var-1))
        (setf ulp (dlamch "Precision"))
        (setf smlnum (* unfl (/ nh ulp)))
        (cond
          (wantt
           (setf i1 1)
           (setf i2 n)))
        (setf itn (f2cl-lib:int-mul 30 nh))
        (setf i ihi)
       label10
        (setf l ilo)
        (if (< i ilo) (go label150))
        (f2cl-lib:fdo (its 0 (f2cl-lib:int-add its 1))
                      ((> its itn) nil)
          (tagbody
            (f2cl-lib:fdo (k i (f2cl-lib:int-add k (f2cl-lib:int-sub 1)))
                          ((> k (f2cl-lib:int-add l 1)) nil)
              (tagbody
                (setf tst1
                        (+
                         (abs
                          (f2cl-lib:fref h-%data%
                                         ((f2cl-lib:int-sub k 1)
                                          (f2cl-lib:int-sub k 1))
                                         ((1 ldh) (1 *))
                                         h-%offset%))
                         (abs
                          (f2cl-lib:fref h-%data%
                                         (k k)
                                         ((1 ldh) (1 *))
                                         h-%offset%))))
                (if (= tst1 zero)
                    (setf tst1
                            (dlanhs "1"
                             (f2cl-lib:int-add (f2cl-lib:int-sub i l) 1)
                             (f2cl-lib:array-slice h-%data%
                                                   double-float
                                                   (l l)
                                                   ((1 ldh) (1 *))
                                                   h-%offset%)
                             ldh work)))
                (if
                 (<=
                  (abs
                   (f2cl-lib:fref h-%data%
                                  (k (f2cl-lib:int-sub k 1))
                                  ((1 ldh) (1 *))
                                  h-%offset%))
                  (max (* ulp tst1) smlnum))
                 (go label30))
               label20))
           label30
            (setf l k)
            (cond
              ((> l ilo)
               (setf (f2cl-lib:fref h-%data%
                                    (l (f2cl-lib:int-sub l 1))
                                    ((1 ldh) (1 *))
                                    h-%offset%)
                       zero)))
            (if (>= l (f2cl-lib:int-sub i 1)) (go label140))
            (cond
              ((not wantt)
               (setf i1 l)
               (setf i2 i)))
            (cond
              ((or (= its 10) (= its 20))
               (setf s
                       (+
                        (abs
                         (f2cl-lib:fref h-%data%
                                        (i (f2cl-lib:int-sub i 1))
                                        ((1 ldh) (1 *))
                                        h-%offset%))
                        (abs
                         (f2cl-lib:fref h-%data%
                                        ((f2cl-lib:int-sub i 1)
                                         (f2cl-lib:int-sub i 2))
                                        ((1 ldh) (1 *))
                                        h-%offset%))))
               (setf h44
                       (+ (* dat1 s)
                          (f2cl-lib:fref h-%data%
                                         (i i)
                                         ((1 ldh) (1 *))
                                         h-%offset%)))
               (setf h33 h44)
               (setf h43h34 (* dat2 s s)))
              (t
               (setf h44
                       (f2cl-lib:fref h-%data%
                                      (i i)
                                      ((1 ldh) (1 *))
                                      h-%offset%))
               (setf h33
                       (f2cl-lib:fref h-%data%
                                      ((f2cl-lib:int-sub i 1)
                                       (f2cl-lib:int-sub i 1))
                                      ((1 ldh) (1 *))
                                      h-%offset%))
               (setf h43h34
                       (*
                        (f2cl-lib:fref h-%data%
                                       (i (f2cl-lib:int-sub i 1))
                                       ((1 ldh) (1 *))
                                       h-%offset%)
                        (f2cl-lib:fref h-%data%
                                       ((f2cl-lib:int-sub i 1) i)
                                       ((1 ldh) (1 *))
                                       h-%offset%)))
               (setf s
                       (*
                        (f2cl-lib:fref h-%data%
                                       ((f2cl-lib:int-sub i 1)
                                        (f2cl-lib:int-sub i 2))
                                       ((1 ldh) (1 *))
                                       h-%offset%)
                        (f2cl-lib:fref h-%data%
                                       ((f2cl-lib:int-sub i 1)
                                        (f2cl-lib:int-sub i 2))
                                       ((1 ldh) (1 *))
                                       h-%offset%)))
               (setf disc (* (- h33 h44) half))
               (setf disc (+ (* disc disc) h43h34))
               (cond
                 ((> disc zero)
                  (setf disc (f2cl-lib:fsqrt disc))
                  (setf ave (* half (+ h33 h44)))
                  (cond
                    ((> (+ (abs h33) (- (abs h44))) zero)
                     (setf h33 (- (* h33 h44) h43h34))
                     (setf h44 (/ h33 (+ (f2cl-lib:sign disc ave) ave))))
                    (t
                     (setf h44 (+ (f2cl-lib:sign disc ave) ave))))
                  (setf h33 h44)
                  (setf h43h34 zero)))))
            (f2cl-lib:fdo (m (f2cl-lib:int-add i (f2cl-lib:int-sub 2))
                           (f2cl-lib:int-add m (f2cl-lib:int-sub 1)))
                          ((> m l) nil)
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
                (setf h21
                        (f2cl-lib:fref h-%data%
                                       ((f2cl-lib:int-add m 1) m)
                                       ((1 ldh) (1 *))
                                       h-%offset%))
                (setf h12
                        (f2cl-lib:fref h-%data%
                                       (m (f2cl-lib:int-add m 1))
                                       ((1 ldh) (1 *))
                                       h-%offset%))
                (setf h44s (- h44 h11))
                (setf h33s (- h33 h11))
                (setf v1 (+ (/ (- (* h33s h44s) h43h34) h21) h12))
                (setf v2 (- h22 h11 h33s h44s))
                (setf v3
                        (f2cl-lib:fref h-%data%
                                       ((f2cl-lib:int-add m 2)
                                        (f2cl-lib:int-add m 1))
                                       ((1 ldh) (1 *))
                                       h-%offset%))
                (setf s (+ (abs v1) (abs v2) (abs v3)))
                (setf v1 (/ v1 s))
                (setf v2 (/ v2 s))
                (setf v3 (/ v3 s))
                (setf (f2cl-lib:fref v (1) ((1 3))) v1)
                (setf (f2cl-lib:fref v (2) ((1 3))) v2)
                (setf (f2cl-lib:fref v (3) ((1 3))) v3)
                (if (= m l) (go label50))
                (setf h00
                        (f2cl-lib:fref h-%data%
                                       ((f2cl-lib:int-sub m 1)
                                        (f2cl-lib:int-sub m 1))
                                       ((1 ldh) (1 *))
                                       h-%offset%))
                (setf h10
                        (f2cl-lib:fref h-%data%
                                       (m (f2cl-lib:int-sub m 1))
                                       ((1 ldh) (1 *))
                                       h-%offset%))
                (setf tst1 (* (abs v1) (+ (abs h00) (abs h11) (abs h22))))
                (if (<= (* (abs h10) (+ (abs v2) (abs v3))) (* ulp tst1))
                    (go label50))
               label40))
           label50
            (f2cl-lib:fdo (k m (f2cl-lib:int-add k 1))
                          ((> k (f2cl-lib:int-add i (f2cl-lib:int-sub 1))) nil)
              (tagbody
                (setf nr
                        (min (the f2cl-lib:integer4 3)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-sub i k)
                                                    1))))
                (if (> k m)
                    (dcopy nr
                     (f2cl-lib:array-slice h-%data%
                                           double-float
                                           (k (f2cl-lib:int-sub k 1))
                                           ((1 ldh) (1 *))
                                           h-%offset%)
                     1 v 1))
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                    (dlarfg nr (f2cl-lib:fref v (1) ((1 3)))
                     (f2cl-lib:array-slice v double-float (2) ((1 3))) 1 t1)
                  (declare (ignore var-0 var-2 var-3))
                  (setf (f2cl-lib:fref v (1) ((1 3))) var-1)
                  (setf t1 var-4))
                (cond
                  ((> k m)
                   (setf (f2cl-lib:fref h-%data%
                                        (k (f2cl-lib:int-sub k 1))
                                        ((1 ldh) (1 *))
                                        h-%offset%)
                           (f2cl-lib:fref v (1) ((1 3))))
                   (setf (f2cl-lib:fref h-%data%
                                        ((f2cl-lib:int-add k 1)
                                         (f2cl-lib:int-sub k 1))
                                        ((1 ldh) (1 *))
                                        h-%offset%)
                           zero)
                   (if (< k (f2cl-lib:int-sub i 1))
                       (setf (f2cl-lib:fref h-%data%
                                            ((f2cl-lib:int-add k 2)
                                             (f2cl-lib:int-sub k 1))
                                            ((1 ldh) (1 *))
                                            h-%offset%)
                               zero)))
                  ((> m l)
                   (setf (f2cl-lib:fref h-%data%
                                        (k (f2cl-lib:int-sub k 1))
                                        ((1 ldh) (1 *))
                                        h-%offset%)
                           (-
                            (f2cl-lib:fref h-%data%
                                           (k (f2cl-lib:int-sub k 1))
                                           ((1 ldh) (1 *))
                                           h-%offset%)))))
                (setf v2 (f2cl-lib:fref v (2) ((1 3))))
                (setf t2 (* t1 v2))
                (cond
                  ((= nr 3)
                   (setf v3 (f2cl-lib:fref v (3) ((1 3))))
                   (setf t3 (* t1 v3))
                   (f2cl-lib:fdo (j k (f2cl-lib:int-add j 1))
                                 ((> j i2) nil)
                     (tagbody
                       (setf sum
                               (+
                                (f2cl-lib:fref h-%data%
                                               (k j)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                                (* v2
                                   (f2cl-lib:fref h-%data%
                                                  ((f2cl-lib:int-add k 1) j)
                                                  ((1 ldh) (1 *))
                                                  h-%offset%))
                                (* v3
                                   (f2cl-lib:fref h-%data%
                                                  ((f2cl-lib:int-add k 2) j)
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
                                (* sum t1)))
                       (setf (f2cl-lib:fref h-%data%
                                            ((f2cl-lib:int-add k 1) j)
                                            ((1 ldh) (1 *))
                                            h-%offset%)
                               (-
                                (f2cl-lib:fref h-%data%
                                               ((f2cl-lib:int-add k 1) j)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                                (* sum t2)))
                       (setf (f2cl-lib:fref h-%data%
                                            ((f2cl-lib:int-add k 2) j)
                                            ((1 ldh) (1 *))
                                            h-%offset%)
                               (-
                                (f2cl-lib:fref h-%data%
                                               ((f2cl-lib:int-add k 2) j)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                                (* sum t3)))
                      label60))
                   (f2cl-lib:fdo (j i1 (f2cl-lib:int-add j 1))
                                 ((> j
                                     (min
                                      (the f2cl-lib:integer4
                                           (f2cl-lib:int-add k 3))
                                      (the f2cl-lib:integer4 i)))
                                  nil)
                     (tagbody
                       (setf sum
                               (+
                                (f2cl-lib:fref h-%data%
                                               (j k)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                                (* v2
                                   (f2cl-lib:fref h-%data%
                                                  (j (f2cl-lib:int-add k 1))
                                                  ((1 ldh) (1 *))
                                                  h-%offset%))
                                (* v3
                                   (f2cl-lib:fref h-%data%
                                                  (j (f2cl-lib:int-add k 2))
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
                                (* sum t1)))
                       (setf (f2cl-lib:fref h-%data%
                                            (j (f2cl-lib:int-add k 1))
                                            ((1 ldh) (1 *))
                                            h-%offset%)
                               (-
                                (f2cl-lib:fref h-%data%
                                               (j (f2cl-lib:int-add k 1))
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                                (* sum t2)))
                       (setf (f2cl-lib:fref h-%data%
                                            (j (f2cl-lib:int-add k 2))
                                            ((1 ldh) (1 *))
                                            h-%offset%)
                               (-
                                (f2cl-lib:fref h-%data%
                                               (j (f2cl-lib:int-add k 2))
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                                (* sum t3)))
                      label70))
                   (cond
                     (wantz
                      (f2cl-lib:fdo (j iloz (f2cl-lib:int-add j 1))
                                    ((> j ihiz) nil)
                        (tagbody
                          (setf sum
                                  (+
                                   (f2cl-lib:fref z-%data%
                                                  (j k)
                                                  ((1 ldz) (1 *))
                                                  z-%offset%)
                                   (* v2
                                      (f2cl-lib:fref z-%data%
                                                     (j (f2cl-lib:int-add k 1))
                                                     ((1 ldz) (1 *))
                                                     z-%offset%))
                                   (* v3
                                      (f2cl-lib:fref z-%data%
                                                     (j (f2cl-lib:int-add k 2))
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
                                   (* sum t1)))
                          (setf (f2cl-lib:fref z-%data%
                                               (j (f2cl-lib:int-add k 1))
                                               ((1 ldz) (1 *))
                                               z-%offset%)
                                  (-
                                   (f2cl-lib:fref z-%data%
                                                  (j (f2cl-lib:int-add k 1))
                                                  ((1 ldz) (1 *))
                                                  z-%offset%)
                                   (* sum t2)))
                          (setf (f2cl-lib:fref z-%data%
                                               (j (f2cl-lib:int-add k 2))
                                               ((1 ldz) (1 *))
                                               z-%offset%)
                                  (-
                                   (f2cl-lib:fref z-%data%
                                                  (j (f2cl-lib:int-add k 2))
                                                  ((1 ldz) (1 *))
                                                  z-%offset%)
                                   (* sum t3)))
                         label80)))))
                  ((= nr 2)
                   (f2cl-lib:fdo (j k (f2cl-lib:int-add j 1))
                                 ((> j i2) nil)
                     (tagbody
                       (setf sum
                               (+
                                (f2cl-lib:fref h-%data%
                                               (k j)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                                (* v2
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
                                (* sum t1)))
                       (setf (f2cl-lib:fref h-%data%
                                            ((f2cl-lib:int-add k 1) j)
                                            ((1 ldh) (1 *))
                                            h-%offset%)
                               (-
                                (f2cl-lib:fref h-%data%
                                               ((f2cl-lib:int-add k 1) j)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                                (* sum t2)))
                      label90))
                   (f2cl-lib:fdo (j i1 (f2cl-lib:int-add j 1))
                                 ((> j i) nil)
                     (tagbody
                       (setf sum
                               (+
                                (f2cl-lib:fref h-%data%
                                               (j k)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                                (* v2
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
                                (* sum t1)))
                       (setf (f2cl-lib:fref h-%data%
                                            (j (f2cl-lib:int-add k 1))
                                            ((1 ldh) (1 *))
                                            h-%offset%)
                               (-
                                (f2cl-lib:fref h-%data%
                                               (j (f2cl-lib:int-add k 1))
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                                (* sum t2)))
                      label100))
                   (cond
                     (wantz
                      (f2cl-lib:fdo (j iloz (f2cl-lib:int-add j 1))
                                    ((> j ihiz) nil)
                        (tagbody
                          (setf sum
                                  (+
                                   (f2cl-lib:fref z-%data%
                                                  (j k)
                                                  ((1 ldz) (1 *))
                                                  z-%offset%)
                                   (* v2
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
                                   (* sum t1)))
                          (setf (f2cl-lib:fref z-%data%
                                               (j (f2cl-lib:int-add k 1))
                                               ((1 ldz) (1 *))
                                               z-%offset%)
                                  (-
                                   (f2cl-lib:fref z-%data%
                                                  (j (f2cl-lib:int-add k 1))
                                                  ((1 ldz) (1 *))
                                                  z-%offset%)
                                   (* sum t2)))
                         label110))))))
               label120))
           label130))
        (setf info i)
        (go end_label)
       label140
        (cond
          ((= l i)
           (setf (f2cl-lib:fref wr-%data% (i) ((1 *)) wr-%offset%)
                   (f2cl-lib:fref h-%data% (i i) ((1 ldh) (1 *)) h-%offset%))
           (setf (f2cl-lib:fref wi-%data% (i) ((1 *)) wi-%offset%) zero))
          ((= l (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
               (dlanv2
                (f2cl-lib:fref h-%data%
                               ((f2cl-lib:int-sub i 1) (f2cl-lib:int-sub i 1))
                               ((1 ldh) (1 *))
                               h-%offset%)
                (f2cl-lib:fref h-%data%
                               ((f2cl-lib:int-sub i 1) i)
                               ((1 ldh) (1 *))
                               h-%offset%)
                (f2cl-lib:fref h-%data%
                               (i (f2cl-lib:int-sub i 1))
                               ((1 ldh) (1 *))
                               h-%offset%)
                (f2cl-lib:fref h-%data% (i i) ((1 ldh) (1 *)) h-%offset%)
                (f2cl-lib:fref wr-%data%
                               ((f2cl-lib:int-sub i 1))
                               ((1 *))
                               wr-%offset%)
                (f2cl-lib:fref wi-%data%
                               ((f2cl-lib:int-sub i 1))
                               ((1 *))
                               wi-%offset%)
                (f2cl-lib:fref wr-%data% (i) ((1 *)) wr-%offset%)
                (f2cl-lib:fref wi-%data% (i) ((1 *)) wi-%offset%) cs sn)
             (declare (ignore))
             (setf (f2cl-lib:fref h-%data%
                                  ((f2cl-lib:int-sub i 1)
                                   (f2cl-lib:int-sub i 1))
                                  ((1 ldh) (1 *))
                                  h-%offset%)
                     var-0)
             (setf (f2cl-lib:fref h-%data%
                                  ((f2cl-lib:int-sub i 1) i)
                                  ((1 ldh) (1 *))
                                  h-%offset%)
                     var-1)
             (setf (f2cl-lib:fref h-%data%
                                  (i (f2cl-lib:int-sub i 1))
                                  ((1 ldh) (1 *))
                                  h-%offset%)
                     var-2)
             (setf (f2cl-lib:fref h-%data% (i i) ((1 ldh) (1 *)) h-%offset%)
                     var-3)
             (setf (f2cl-lib:fref wr-%data%
                                  ((f2cl-lib:int-sub i 1))
                                  ((1 *))
                                  wr-%offset%)
                     var-4)
             (setf (f2cl-lib:fref wi-%data%
                                  ((f2cl-lib:int-sub i 1))
                                  ((1 *))
                                  wi-%offset%)
                     var-5)
             (setf (f2cl-lib:fref wr-%data% (i) ((1 *)) wr-%offset%) var-6)
             (setf (f2cl-lib:fref wi-%data% (i) ((1 *)) wi-%offset%) var-7)
             (setf cs var-8)
             (setf sn var-9))
           (cond
             (wantt
              (if (> i2 i)
                  (drot (f2cl-lib:int-sub i2 i)
                   (f2cl-lib:array-slice h-%data%
                                         double-float
                                         ((+ i (f2cl-lib:int-sub 1))
                                          (f2cl-lib:int-add i 1))
                                         ((1 ldh) (1 *))
                                         h-%offset%)
                   ldh
                   (f2cl-lib:array-slice h-%data%
                                         double-float
                                         (i (f2cl-lib:int-add i 1))
                                         ((1 ldh) (1 *))
                                         h-%offset%)
                   ldh cs sn))
              (drot (f2cl-lib:int-sub i i1 1)
               (f2cl-lib:array-slice h-%data%
                                     double-float
                                     (i1 (f2cl-lib:int-sub i 1))
                                     ((1 ldh) (1 *))
                                     h-%offset%)
               1
               (f2cl-lib:array-slice h-%data%
                                     double-float
                                     (i1 i)
                                     ((1 ldh) (1 *))
                                     h-%offset%)
               1 cs sn)))
           (cond
             (wantz
              (drot nz
               (f2cl-lib:array-slice z-%data%
                                     double-float
                                     (iloz (f2cl-lib:int-sub i 1))
                                     ((1 ldz) (1 *))
                                     z-%offset%)
               1
               (f2cl-lib:array-slice z-%data%
                                     double-float
                                     (iloz i)
                                     ((1 ldz) (1 *))
                                     z-%offset%)
               1 cs sn)))))
        (setf itn (f2cl-lib:int-sub itn its))
        (setf i (f2cl-lib:int-sub l 1))
        (go label10)
       label150
        (go end_label)
       end_label
        (return
         (values nil nil nil nil nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlahqr
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(fortran-to-lisp::logical fortran-to-lisp::logical
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::drot fortran-to-lisp::dlanv2
                    fortran-to-lisp::dlarfg fortran-to-lisp::dcopy
                    fortran-to-lisp::dlanhs fortran-to-lisp::dlabad
                    fortran-to-lisp::dlamch))))

