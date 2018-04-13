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


(let* ((zero (f2cl-lib:cmplx 0.0 0.0))
       (one (f2cl-lib:cmplx 1.0 0.0))
       (rzero 0.0)
       (rone 1.0))
  (declare (type (f2cl-lib:complex16) zero)
           (type (f2cl-lib:complex16) one)
           (type (double-float 0.0 0.0) rzero)
           (type (double-float 1.0 1.0) rone)
           (ignorable zero one rzero rone))
  (defun zlaqr5
         (wantt wantz kacc22 n ktop kbot nshfts s h ldh iloz ihiz z ldz v ldv u
          ldu nv wv ldwv nh wh ldwh)
    (declare (type (array f2cl-lib:complex16 (*)) wh wv u v z h s)
             (type (f2cl-lib:integer4) ldwh nh ldwv nv ldu ldv ldz ihiz iloz
                                       ldh nshfts kbot ktop n kacc22)
             (type f2cl-lib:logical wantz wantt))
    (f2cl-lib:with-multi-array-data
        ((s f2cl-lib:complex16 s-%data% s-%offset%)
         (h f2cl-lib:complex16 h-%data% h-%offset%)
         (z f2cl-lib:complex16 z-%data% z-%offset%)
         (v f2cl-lib:complex16 v-%data% v-%offset%)
         (u f2cl-lib:complex16 u-%data% u-%offset%)
         (wv f2cl-lib:complex16 wv-%data% wv-%offset%)
         (wh f2cl-lib:complex16 wh-%data% wh-%offset%))
      (labels ((cabs1 (cdum)
                 (+ (abs (f2cl-lib:dble cdum)) (abs (f2cl-lib:dimag cdum)))))
        (declare (ftype (function (f2cl-lib:complex16)
                         (values double-float &rest t))
                        cabs1))
        (prog ((vt (make-array 3 :element-type 'f2cl-lib:complex16))
               (accum nil) (blk22 nil) (bmp22 nil) (i2 0) (i4 0) (incol 0)
               (j 0) (j2 0) (j4 0) (jbot 0) (jcol 0) (jlen 0) (jrow 0) (jtop 0)
               (k 0) (k1 0) (kdu 0) (kms 0) (knz 0) (krcol 0) (kzs 0) (m 0)
               (m22 0) (mbot 0) (mend 0) (mstart 0) (mtop 0) (nbmps 0)
               (ndcol 0) (ns 0) (nu 0) (h11 0.0) (h12 0.0) (h21 0.0) (h22 0.0)
               (safmax 0.0) (safmin 0.0) (scl 0.0) (smlnum 0.0) (tst1 0.0)
               (tst2 0.0) (ulp 0.0) (alpha #C(0.0 0.0)) (beta #C(0.0 0.0))
               (cdum #C(0.0 0.0)) (refsum #C(0.0 0.0)))
          (declare (type (array f2cl-lib:complex16 (3)) vt)
                   (type f2cl-lib:logical accum blk22 bmp22)
                   (type (f2cl-lib:integer4) i2 i4 incol j j2 j4 jbot jcol jlen
                                             jrow jtop k k1 kdu kms knz krcol
                                             kzs m m22 mbot mend mstart mtop
                                             nbmps ndcol ns nu)
                   (type (double-float) h11 h12 h21 h22 safmax safmin scl
                                        smlnum tst1 tst2 ulp)
                   (type (f2cl-lib:complex16) alpha beta cdum refsum))
          (if (< nshfts 2) (go end_label))
          (if (>= ktop kbot) (go end_label))
          (setf ns (f2cl-lib:int-sub nshfts (mod nshfts 2)))
          (setf safmin (dlamch "SAFE MINIMUM"))
          (setf safmax (/ rone safmin))
          (multiple-value-bind (var-0 var-1)
              (dlabad safmin safmax)
            (declare (ignore))
            (setf safmin var-0)
            (setf safmax var-1))
          (setf ulp (dlamch "PRECISION"))
          (setf smlnum (* safmin (/ (f2cl-lib:dble n) ulp)))
          (setf accum (or (= kacc22 1) (= kacc22 2)))
          (setf blk22 (and (> ns 2) (= kacc22 2)))
          (if (<= (f2cl-lib:int-add ktop 2) kbot)
              (setf (f2cl-lib:fref h-%data%
                                   ((f2cl-lib:int-add ktop 2) ktop)
                                   ((1 ldh) (1 *))
                                   h-%offset%)
                      zero))
          (setf nbmps (the f2cl-lib:integer4 (truncate ns 2)))
          (setf kdu (f2cl-lib:int-sub (f2cl-lib:int-mul 6 nbmps) 3))
          (f2cl-lib:fdo (incol
                         (f2cl-lib:int-add
                          (f2cl-lib:int-mul 3
                                            (f2cl-lib:int-add 1
                                                              (f2cl-lib:int-sub
                                                               nbmps)))
                          ktop
                          (f2cl-lib:int-sub 1))
                         (f2cl-lib:int-add incol
                                           (f2cl-lib:int-add
                                            (f2cl-lib:int-mul 3 nbmps)
                                            (f2cl-lib:int-sub 2))))
                        ((> incol (f2cl-lib:int-add kbot (f2cl-lib:int-sub 2)))
                         nil)
            (tagbody
              (setf ndcol (f2cl-lib:int-add incol kdu))
              (if accum (zlaset "ALL" kdu kdu zero one u ldu))
              (f2cl-lib:fdo (krcol incol (f2cl-lib:int-add krcol 1))
                            ((> krcol
                                (min
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add incol
                                                        (f2cl-lib:int-mul 3
                                                                          nbmps)
                                                        (f2cl-lib:int-sub 3)))
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add kbot
                                                        (f2cl-lib:int-sub
                                                         2)))))
                             nil)
                (tagbody
                  (setf mtop
                          (max 1
                               (+
                                (the f2cl-lib:integer4
                                     (truncate (+ (- ktop 1 krcol) 2) 3))
                                1)))
                  (setf mbot
                          (min nbmps
                               (the f2cl-lib:integer4
                                    (truncate (- kbot krcol) 3))))
                  (setf m22 (f2cl-lib:int-add mbot 1))
                  (setf bmp22
                          (and (< mbot nbmps)
                               (=
                                (f2cl-lib:int-add krcol
                                                  (f2cl-lib:int-mul 3
                                                                    (f2cl-lib:int-sub
                                                                     m22
                                                                     1)))
                                (f2cl-lib:int-sub kbot 2))))
                  (f2cl-lib:fdo (m mtop (f2cl-lib:int-add m 1))
                                ((> m mbot) nil)
                    (tagbody
                      (setf k
                              (f2cl-lib:int-add krcol
                                                (f2cl-lib:int-mul 3
                                                                  (f2cl-lib:int-sub
                                                                   m
                                                                   1))))
                      (cond
                        ((= k (f2cl-lib:int-add ktop (f2cl-lib:int-sub 1)))
                         (zlaqr1 3
                          (f2cl-lib:array-slice h-%data%
                                                f2cl-lib:complex16
                                                (ktop ktop)
                                                ((1 ldh) (1 *))
                                                h-%offset%)
                          ldh
                          (f2cl-lib:fref s-%data%
                                         ((f2cl-lib:int-sub
                                           (f2cl-lib:int-mul 2 m)
                                           1))
                                         ((1 *))
                                         s-%offset%)
                          (f2cl-lib:fref s-%data%
                                         ((f2cl-lib:int-mul 2 m))
                                         ((1 *))
                                         s-%offset%)
                          (f2cl-lib:array-slice v-%data%
                                                f2cl-lib:complex16
                                                (1 m)
                                                ((1 ldv) (1 *))
                                                v-%offset%))
                         (setf alpha
                                 (f2cl-lib:fref v-%data%
                                                (1 m)
                                                ((1 ldv) (1 *))
                                                v-%offset%))
                         (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                             (zlarfg 3 alpha
                              (f2cl-lib:array-slice v-%data%
                                                    f2cl-lib:complex16
                                                    (2 m)
                                                    ((1 ldv) (1 *))
                                                    v-%offset%)
                              1
                              (f2cl-lib:fref v-%data%
                                             (1 m)
                                             ((1 ldv) (1 *))
                                             v-%offset%))
                           (declare (ignore var-0 var-2 var-3))
                           (setf alpha var-1)
                           (setf (f2cl-lib:fref v-%data%
                                                (1 m)
                                                ((1 ldv) (1 *))
                                                v-%offset%)
                                   var-4)))
                        (t
                         (setf beta
                                 (f2cl-lib:fref h-%data%
                                                ((f2cl-lib:int-add k 1) k)
                                                ((1 ldh) (1 *))
                                                h-%offset%))
                         (setf (f2cl-lib:fref v-%data%
                                              (2 m)
                                              ((1 ldv) (1 *))
                                              v-%offset%)
                                 (f2cl-lib:fref h-%data%
                                                ((f2cl-lib:int-add k 2) k)
                                                ((1 ldh) (1 *))
                                                h-%offset%))
                         (setf (f2cl-lib:fref v-%data%
                                              (3 m)
                                              ((1 ldv) (1 *))
                                              v-%offset%)
                                 (f2cl-lib:fref h-%data%
                                                ((f2cl-lib:int-add k 3) k)
                                                ((1 ldh) (1 *))
                                                h-%offset%))
                         (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                             (zlarfg 3 beta
                              (f2cl-lib:array-slice v-%data%
                                                    f2cl-lib:complex16
                                                    (2 m)
                                                    ((1 ldv) (1 *))
                                                    v-%offset%)
                              1
                              (f2cl-lib:fref v-%data%
                                             (1 m)
                                             ((1 ldv) (1 *))
                                             v-%offset%))
                           (declare (ignore var-0 var-2 var-3))
                           (setf beta var-1)
                           (setf (f2cl-lib:fref v-%data%
                                                (1 m)
                                                ((1 ldv) (1 *))
                                                v-%offset%)
                                   var-4))
                         (cond
                           ((or
                             (/=
                              (f2cl-lib:fref h
                                             ((f2cl-lib:int-add k 3) k)
                                             ((1 ldh) (1 *)))
                              zero)
                             (/=
                              (f2cl-lib:fref h
                                             ((f2cl-lib:int-add k 3)
                                              (f2cl-lib:int-add k 1))
                                             ((1 ldh) (1 *)))
                              zero)
                             (=
                              (f2cl-lib:fref h
                                             ((f2cl-lib:int-add k 3)
                                              (f2cl-lib:int-add k 2))
                                             ((1 ldh) (1 *)))
                              zero))
                            (setf (f2cl-lib:fref h-%data%
                                                 ((f2cl-lib:int-add k 1) k)
                                                 ((1 ldh) (1 *))
                                                 h-%offset%)
                                    beta)
                            (setf (f2cl-lib:fref h-%data%
                                                 ((f2cl-lib:int-add k 2) k)
                                                 ((1 ldh) (1 *))
                                                 h-%offset%)
                                    zero)
                            (setf (f2cl-lib:fref h-%data%
                                                 ((f2cl-lib:int-add k 3) k)
                                                 ((1 ldh) (1 *))
                                                 h-%offset%)
                                    zero))
                           (t
                            (zlaqr1 3
                             (f2cl-lib:array-slice h-%data%
                                                   f2cl-lib:complex16
                                                   ((+ k 1)
                                                    (f2cl-lib:int-add k 1))
                                                   ((1 ldh) (1 *))
                                                   h-%offset%)
                             ldh
                             (f2cl-lib:fref s-%data%
                                            ((f2cl-lib:int-sub
                                              (f2cl-lib:int-mul 2 m)
                                              1))
                                            ((1 *))
                                            s-%offset%)
                             (f2cl-lib:fref s-%data%
                                            ((f2cl-lib:int-mul 2 m))
                                            ((1 *))
                                            s-%offset%)
                             vt)
                            (setf alpha (f2cl-lib:fref vt (1) ((1 3))))
                            (multiple-value-bind
                                  (var-0 var-1 var-2 var-3 var-4)
                                (zlarfg 3 alpha
                                 (f2cl-lib:array-slice vt
                                                       f2cl-lib:complex16
                                                       (2)
                                                       ((1 3)))
                                 1 (f2cl-lib:fref vt (1) ((1 3))))
                              (declare (ignore var-0 var-2 var-3))
                              (setf alpha var-1)
                              (setf (f2cl-lib:fref vt (1) ((1 3))) var-4))
                            (setf refsum
                                    (*
                                     (f2cl-lib:dconjg
                                      (f2cl-lib:fref vt (1) ((1 3))))
                                     (+
                                      (f2cl-lib:fref h-%data%
                                                     ((f2cl-lib:int-add k 1) k)
                                                     ((1 ldh) (1 *))
                                                     h-%offset%)
                                      (*
                                       (f2cl-lib:dconjg
                                        (f2cl-lib:fref vt (2) ((1 3))))
                                       (f2cl-lib:fref h-%data%
                                                      ((f2cl-lib:int-add k 2)
                                                       k)
                                                      ((1 ldh) (1 *))
                                                      h-%offset%)))))
                            (cond
                              ((>
                                (+
                                 (cabs1
                                  (+
                                   (f2cl-lib:fref h
                                                  ((f2cl-lib:int-add k 2) k)
                                                  ((1 ldh) (1 *)))
                                   (* -1
                                      refsum
                                      (f2cl-lib:fref vt (2) ((1 3))))))
                                 (cabs1
                                  (* refsum (f2cl-lib:fref vt (3) ((1 3))))))
                                (* ulp
                                   (+
                                    (cabs1
                                     (f2cl-lib:fref h (k k) ((1 ldh) (1 *))))
                                    (cabs1
                                     (f2cl-lib:fref h
                                                    ((f2cl-lib:int-add k 1)
                                                     (f2cl-lib:int-add k 1))
                                                    ((1 ldh) (1 *))))
                                    (cabs1
                                     (f2cl-lib:fref h
                                                    ((f2cl-lib:int-add k 2)
                                                     (f2cl-lib:int-add k 2))
                                                    ((1 ldh) (1 *)))))))
                               (setf (f2cl-lib:fref h-%data%
                                                    ((f2cl-lib:int-add k 1) k)
                                                    ((1 ldh) (1 *))
                                                    h-%offset%)
                                       beta)
                               (setf (f2cl-lib:fref h-%data%
                                                    ((f2cl-lib:int-add k 2) k)
                                                    ((1 ldh) (1 *))
                                                    h-%offset%)
                                       zero)
                               (setf (f2cl-lib:fref h-%data%
                                                    ((f2cl-lib:int-add k 3) k)
                                                    ((1 ldh) (1 *))
                                                    h-%offset%)
                                       zero))
                              (t
                               (setf (f2cl-lib:fref h-%data%
                                                    ((f2cl-lib:int-add k 1) k)
                                                    ((1 ldh) (1 *))
                                                    h-%offset%)
                                       (-
                                        (f2cl-lib:fref h-%data%
                                                       ((f2cl-lib:int-add k 1)
                                                        k)
                                                       ((1 ldh) (1 *))
                                                       h-%offset%)
                                        refsum))
                               (setf (f2cl-lib:fref h-%data%
                                                    ((f2cl-lib:int-add k 2) k)
                                                    ((1 ldh) (1 *))
                                                    h-%offset%)
                                       zero)
                               (setf (f2cl-lib:fref h-%data%
                                                    ((f2cl-lib:int-add k 3) k)
                                                    ((1 ldh) (1 *))
                                                    h-%offset%)
                                       zero)
                               (setf (f2cl-lib:fref v-%data%
                                                    (1 m)
                                                    ((1 ldv) (1 *))
                                                    v-%offset%)
                                       (f2cl-lib:fref vt (1) ((1 3))))
                               (setf (f2cl-lib:fref v-%data%
                                                    (2 m)
                                                    ((1 ldv) (1 *))
                                                    v-%offset%)
                                       (f2cl-lib:fref vt (2) ((1 3))))
                               (setf (f2cl-lib:fref v-%data%
                                                    (3 m)
                                                    ((1 ldv) (1 *))
                                                    v-%offset%)
                                       (f2cl-lib:fref vt (3) ((1 3))))))))))
                     label10))
                  (setf k
                          (f2cl-lib:int-add krcol
                                            (f2cl-lib:int-mul 3
                                                              (f2cl-lib:int-sub
                                                               m22
                                                               1))))
                  (cond
                    (bmp22
                     (cond
                       ((= k (f2cl-lib:int-add ktop (f2cl-lib:int-sub 1)))
                        (zlaqr1 2
                         (f2cl-lib:array-slice h-%data%
                                               f2cl-lib:complex16
                                               ((+ k 1) (f2cl-lib:int-add k 1))
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                         ldh
                         (f2cl-lib:fref s-%data%
                                        ((f2cl-lib:int-sub
                                          (f2cl-lib:int-mul 2 m22)
                                          1))
                                        ((1 *))
                                        s-%offset%)
                         (f2cl-lib:fref s-%data%
                                        ((f2cl-lib:int-mul 2 m22))
                                        ((1 *))
                                        s-%offset%)
                         (f2cl-lib:array-slice v-%data%
                                               f2cl-lib:complex16
                                               (1 m22)
                                               ((1 ldv) (1 *))
                                               v-%offset%))
                        (setf beta
                                (f2cl-lib:fref v-%data%
                                               (1 m22)
                                               ((1 ldv) (1 *))
                                               v-%offset%))
                        (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                            (zlarfg 2 beta
                             (f2cl-lib:array-slice v-%data%
                                                   f2cl-lib:complex16
                                                   (2 m22)
                                                   ((1 ldv) (1 *))
                                                   v-%offset%)
                             1
                             (f2cl-lib:fref v-%data%
                                            (1 m22)
                                            ((1 ldv) (1 *))
                                            v-%offset%))
                          (declare (ignore var-0 var-2 var-3))
                          (setf beta var-1)
                          (setf (f2cl-lib:fref v-%data%
                                               (1 m22)
                                               ((1 ldv) (1 *))
                                               v-%offset%)
                                  var-4)))
                       (t
                        (setf beta
                                (f2cl-lib:fref h-%data%
                                               ((f2cl-lib:int-add k 1) k)
                                               ((1 ldh) (1 *))
                                               h-%offset%))
                        (setf (f2cl-lib:fref v-%data%
                                             (2 m22)
                                             ((1 ldv) (1 *))
                                             v-%offset%)
                                (f2cl-lib:fref h-%data%
                                               ((f2cl-lib:int-add k 2) k)
                                               ((1 ldh) (1 *))
                                               h-%offset%))
                        (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                            (zlarfg 2 beta
                             (f2cl-lib:array-slice v-%data%
                                                   f2cl-lib:complex16
                                                   (2 m22)
                                                   ((1 ldv) (1 *))
                                                   v-%offset%)
                             1
                             (f2cl-lib:fref v-%data%
                                            (1 m22)
                                            ((1 ldv) (1 *))
                                            v-%offset%))
                          (declare (ignore var-0 var-2 var-3))
                          (setf beta var-1)
                          (setf (f2cl-lib:fref v-%data%
                                               (1 m22)
                                               ((1 ldv) (1 *))
                                               v-%offset%)
                                  var-4))
                        (setf (f2cl-lib:fref h-%data%
                                             ((f2cl-lib:int-add k 1) k)
                                             ((1 ldh) (1 *))
                                             h-%offset%)
                                beta)
                        (setf (f2cl-lib:fref h-%data%
                                             ((f2cl-lib:int-add k 2) k)
                                             ((1 ldh) (1 *))
                                             h-%offset%)
                                zero)))))
                  (cond
                    (accum
                     (setf jbot
                             (min (the f2cl-lib:integer4 ndcol)
                                  (the f2cl-lib:integer4 kbot))))
                    (wantt
                     (setf jbot n))
                    (t
                     (setf jbot kbot)))
                  (f2cl-lib:fdo (j
                                 (max (the f2cl-lib:integer4 ktop)
                                      (the f2cl-lib:integer4 krcol))
                                 (f2cl-lib:int-add j 1))
                                ((> j jbot) nil)
                    (tagbody
                      (setf mend
                              (min mbot
                                   (the f2cl-lib:integer4
                                        (truncate (+ (- j krcol) 2) 3))))
                      (f2cl-lib:fdo (m mtop (f2cl-lib:int-add m 1))
                                    ((> m mend) nil)
                        (tagbody
                          (setf k
                                  (f2cl-lib:int-add krcol
                                                    (f2cl-lib:int-mul 3
                                                                      (f2cl-lib:int-sub
                                                                       m
                                                                       1))))
                          (setf refsum
                                  (*
                                   (f2cl-lib:dconjg
                                    (f2cl-lib:fref v-%data%
                                                   (1 m)
                                                   ((1 ldv) (1 *))
                                                   v-%offset%))
                                   (+
                                    (f2cl-lib:fref h-%data%
                                                   ((f2cl-lib:int-add k 1) j)
                                                   ((1 ldh) (1 *))
                                                   h-%offset%)
                                    (*
                                     (f2cl-lib:dconjg
                                      (f2cl-lib:fref v-%data%
                                                     (2 m)
                                                     ((1 ldv) (1 *))
                                                     v-%offset%))
                                     (f2cl-lib:fref h-%data%
                                                    ((f2cl-lib:int-add k 2) j)
                                                    ((1 ldh) (1 *))
                                                    h-%offset%))
                                    (*
                                     (f2cl-lib:dconjg
                                      (f2cl-lib:fref v-%data%
                                                     (3 m)
                                                     ((1 ldv) (1 *))
                                                     v-%offset%))
                                     (f2cl-lib:fref h-%data%
                                                    ((f2cl-lib:int-add k 3) j)
                                                    ((1 ldh) (1 *))
                                                    h-%offset%)))))
                          (setf (f2cl-lib:fref h-%data%
                                               ((f2cl-lib:int-add k 1) j)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                                  (-
                                   (f2cl-lib:fref h-%data%
                                                  ((f2cl-lib:int-add k 1) j)
                                                  ((1 ldh) (1 *))
                                                  h-%offset%)
                                   refsum))
                          (setf (f2cl-lib:fref h-%data%
                                               ((f2cl-lib:int-add k 2) j)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                                  (-
                                   (f2cl-lib:fref h-%data%
                                                  ((f2cl-lib:int-add k 2) j)
                                                  ((1 ldh) (1 *))
                                                  h-%offset%)
                                   (* refsum
                                      (f2cl-lib:fref v-%data%
                                                     (2 m)
                                                     ((1 ldv) (1 *))
                                                     v-%offset%))))
                          (setf (f2cl-lib:fref h-%data%
                                               ((f2cl-lib:int-add k 3) j)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                                  (-
                                   (f2cl-lib:fref h-%data%
                                                  ((f2cl-lib:int-add k 3) j)
                                                  ((1 ldh) (1 *))
                                                  h-%offset%)
                                   (* refsum
                                      (f2cl-lib:fref v-%data%
                                                     (3 m)
                                                     ((1 ldv) (1 *))
                                                     v-%offset%))))
                         label20))
                     label30))
                  (cond
                    (bmp22
                     (setf k
                             (f2cl-lib:int-add krcol
                                               (f2cl-lib:int-mul 3
                                                                 (f2cl-lib:int-sub
                                                                  m22
                                                                  1))))
                     (f2cl-lib:fdo (j
                                    (max
                                     (the f2cl-lib:integer4
                                          (f2cl-lib:int-add k 1))
                                     (the f2cl-lib:integer4 ktop))
                                    (f2cl-lib:int-add j 1))
                                   ((> j jbot) nil)
                       (tagbody
                         (setf refsum
                                 (*
                                  (f2cl-lib:dconjg
                                   (f2cl-lib:fref v-%data%
                                                  (1 m22)
                                                  ((1 ldv) (1 *))
                                                  v-%offset%))
                                  (+
                                   (f2cl-lib:fref h-%data%
                                                  ((f2cl-lib:int-add k 1) j)
                                                  ((1 ldh) (1 *))
                                                  h-%offset%)
                                   (*
                                    (f2cl-lib:dconjg
                                     (f2cl-lib:fref v-%data%
                                                    (2 m22)
                                                    ((1 ldv) (1 *))
                                                    v-%offset%))
                                    (f2cl-lib:fref h-%data%
                                                   ((f2cl-lib:int-add k 2) j)
                                                   ((1 ldh) (1 *))
                                                   h-%offset%)))))
                         (setf (f2cl-lib:fref h-%data%
                                              ((f2cl-lib:int-add k 1) j)
                                              ((1 ldh) (1 *))
                                              h-%offset%)
                                 (-
                                  (f2cl-lib:fref h-%data%
                                                 ((f2cl-lib:int-add k 1) j)
                                                 ((1 ldh) (1 *))
                                                 h-%offset%)
                                  refsum))
                         (setf (f2cl-lib:fref h-%data%
                                              ((f2cl-lib:int-add k 2) j)
                                              ((1 ldh) (1 *))
                                              h-%offset%)
                                 (-
                                  (f2cl-lib:fref h-%data%
                                                 ((f2cl-lib:int-add k 2) j)
                                                 ((1 ldh) (1 *))
                                                 h-%offset%)
                                  (* refsum
                                     (f2cl-lib:fref v-%data%
                                                    (2 m22)
                                                    ((1 ldv) (1 *))
                                                    v-%offset%))))
                        label40))))
                  (cond
                    (accum
                     (setf jtop
                             (max (the f2cl-lib:integer4 ktop)
                                  (the f2cl-lib:integer4 incol))))
                    (wantt
                     (setf jtop 1))
                    (t
                     (setf jtop ktop)))
                  (f2cl-lib:fdo (m mtop (f2cl-lib:int-add m 1))
                                ((> m mbot) nil)
                    (tagbody
                      (cond
                        ((/= (f2cl-lib:fref v (1 m) ((1 ldv) (1 *))) zero)
                         (setf k
                                 (f2cl-lib:int-add krcol
                                                   (f2cl-lib:int-mul 3
                                                                     (f2cl-lib:int-sub
                                                                      m
                                                                      1))))
                         (f2cl-lib:fdo (j jtop (f2cl-lib:int-add j 1))
                                       ((> j
                                           (min (the f2cl-lib:integer4 kbot)
                                                (the f2cl-lib:integer4
                                                     (f2cl-lib:int-add k 3))))
                                        nil)
                           (tagbody
                             (setf refsum
                                     (*
                                      (f2cl-lib:fref v-%data%
                                                     (1 m)
                                                     ((1 ldv) (1 *))
                                                     v-%offset%)
                                      (+
                                       (f2cl-lib:fref h-%data%
                                                      (j
                                                       (f2cl-lib:int-add k 1))
                                                      ((1 ldh) (1 *))
                                                      h-%offset%)
                                       (*
                                        (f2cl-lib:fref v-%data%
                                                       (2 m)
                                                       ((1 ldv) (1 *))
                                                       v-%offset%)
                                        (f2cl-lib:fref h-%data%
                                                       (j
                                                        (f2cl-lib:int-add k 2))
                                                       ((1 ldh) (1 *))
                                                       h-%offset%))
                                       (*
                                        (f2cl-lib:fref v-%data%
                                                       (3 m)
                                                       ((1 ldv) (1 *))
                                                       v-%offset%)
                                        (f2cl-lib:fref h-%data%
                                                       (j
                                                        (f2cl-lib:int-add k 3))
                                                       ((1 ldh) (1 *))
                                                       h-%offset%)))))
                             (setf (f2cl-lib:fref h-%data%
                                                  (j (f2cl-lib:int-add k 1))
                                                  ((1 ldh) (1 *))
                                                  h-%offset%)
                                     (-
                                      (f2cl-lib:fref h-%data%
                                                     (j (f2cl-lib:int-add k 1))
                                                     ((1 ldh) (1 *))
                                                     h-%offset%)
                                      refsum))
                             (setf (f2cl-lib:fref h-%data%
                                                  (j (f2cl-lib:int-add k 2))
                                                  ((1 ldh) (1 *))
                                                  h-%offset%)
                                     (-
                                      (f2cl-lib:fref h-%data%
                                                     (j (f2cl-lib:int-add k 2))
                                                     ((1 ldh) (1 *))
                                                     h-%offset%)
                                      (* refsum
                                         (f2cl-lib:dconjg
                                          (f2cl-lib:fref v-%data%
                                                         (2 m)
                                                         ((1 ldv) (1 *))
                                                         v-%offset%)))))
                             (setf (f2cl-lib:fref h-%data%
                                                  (j (f2cl-lib:int-add k 3))
                                                  ((1 ldh) (1 *))
                                                  h-%offset%)
                                     (-
                                      (f2cl-lib:fref h-%data%
                                                     (j (f2cl-lib:int-add k 3))
                                                     ((1 ldh) (1 *))
                                                     h-%offset%)
                                      (* refsum
                                         (f2cl-lib:dconjg
                                          (f2cl-lib:fref v-%data%
                                                         (3 m)
                                                         ((1 ldv) (1 *))
                                                         v-%offset%)))))
                            label50))
                         (cond
                           (accum
                            (setf kms (f2cl-lib:int-sub k incol))
                            (f2cl-lib:fdo (j
                                           (max (the f2cl-lib:integer4 1)
                                                (the f2cl-lib:integer4
                                                     (f2cl-lib:int-add ktop
                                                                       (f2cl-lib:int-sub
                                                                        incol))))
                                           (f2cl-lib:int-add j 1))
                                          ((> j kdu) nil)
                              (tagbody
                                (setf refsum
                                        (*
                                         (f2cl-lib:fref v-%data%
                                                        (1 m)
                                                        ((1 ldv) (1 *))
                                                        v-%offset%)
                                         (+
                                          (f2cl-lib:fref u-%data%
                                                         (j
                                                          (f2cl-lib:int-add kms
                                                                            1))
                                                         ((1 ldu) (1 *))
                                                         u-%offset%)
                                          (*
                                           (f2cl-lib:fref v-%data%
                                                          (2 m)
                                                          ((1 ldv) (1 *))
                                                          v-%offset%)
                                           (f2cl-lib:fref u-%data%
                                                          (j
                                                           (f2cl-lib:int-add
                                                            kms
                                                            2))
                                                          ((1 ldu) (1 *))
                                                          u-%offset%))
                                          (*
                                           (f2cl-lib:fref v-%data%
                                                          (3 m)
                                                          ((1 ldv) (1 *))
                                                          v-%offset%)
                                           (f2cl-lib:fref u-%data%
                                                          (j
                                                           (f2cl-lib:int-add
                                                            kms
                                                            3))
                                                          ((1 ldu) (1 *))
                                                          u-%offset%)))))
                                (setf (f2cl-lib:fref u-%data%
                                                     (j
                                                      (f2cl-lib:int-add kms 1))
                                                     ((1 ldu) (1 *))
                                                     u-%offset%)
                                        (-
                                         (f2cl-lib:fref u-%data%
                                                        (j
                                                         (f2cl-lib:int-add kms
                                                                           1))
                                                        ((1 ldu) (1 *))
                                                        u-%offset%)
                                         refsum))
                                (setf (f2cl-lib:fref u-%data%
                                                     (j
                                                      (f2cl-lib:int-add kms 2))
                                                     ((1 ldu) (1 *))
                                                     u-%offset%)
                                        (-
                                         (f2cl-lib:fref u-%data%
                                                        (j
                                                         (f2cl-lib:int-add kms
                                                                           2))
                                                        ((1 ldu) (1 *))
                                                        u-%offset%)
                                         (* refsum
                                            (f2cl-lib:dconjg
                                             (f2cl-lib:fref v-%data%
                                                            (2 m)
                                                            ((1 ldv) (1 *))
                                                            v-%offset%)))))
                                (setf (f2cl-lib:fref u-%data%
                                                     (j
                                                      (f2cl-lib:int-add kms 3))
                                                     ((1 ldu) (1 *))
                                                     u-%offset%)
                                        (-
                                         (f2cl-lib:fref u-%data%
                                                        (j
                                                         (f2cl-lib:int-add kms
                                                                           3))
                                                        ((1 ldu) (1 *))
                                                        u-%offset%)
                                         (* refsum
                                            (f2cl-lib:dconjg
                                             (f2cl-lib:fref v-%data%
                                                            (3 m)
                                                            ((1 ldv) (1 *))
                                                            v-%offset%)))))
                               label60)))
                           (wantz
                            (f2cl-lib:fdo (j iloz (f2cl-lib:int-add j 1))
                                          ((> j ihiz) nil)
                              (tagbody
                                (setf refsum
                                        (*
                                         (f2cl-lib:fref v-%data%
                                                        (1 m)
                                                        ((1 ldv) (1 *))
                                                        v-%offset%)
                                         (+
                                          (f2cl-lib:fref z-%data%
                                                         (j
                                                          (f2cl-lib:int-add k
                                                                            1))
                                                         ((1 ldz) (1 *))
                                                         z-%offset%)
                                          (*
                                           (f2cl-lib:fref v-%data%
                                                          (2 m)
                                                          ((1 ldv) (1 *))
                                                          v-%offset%)
                                           (f2cl-lib:fref z-%data%
                                                          (j
                                                           (f2cl-lib:int-add k
                                                                             2))
                                                          ((1 ldz) (1 *))
                                                          z-%offset%))
                                          (*
                                           (f2cl-lib:fref v-%data%
                                                          (3 m)
                                                          ((1 ldv) (1 *))
                                                          v-%offset%)
                                           (f2cl-lib:fref z-%data%
                                                          (j
                                                           (f2cl-lib:int-add k
                                                                             3))
                                                          ((1 ldz) (1 *))
                                                          z-%offset%)))))
                                (setf (f2cl-lib:fref z-%data%
                                                     (j (f2cl-lib:int-add k 1))
                                                     ((1 ldz) (1 *))
                                                     z-%offset%)
                                        (-
                                         (f2cl-lib:fref z-%data%
                                                        (j
                                                         (f2cl-lib:int-add k
                                                                           1))
                                                        ((1 ldz) (1 *))
                                                        z-%offset%)
                                         refsum))
                                (setf (f2cl-lib:fref z-%data%
                                                     (j (f2cl-lib:int-add k 2))
                                                     ((1 ldz) (1 *))
                                                     z-%offset%)
                                        (-
                                         (f2cl-lib:fref z-%data%
                                                        (j
                                                         (f2cl-lib:int-add k
                                                                           2))
                                                        ((1 ldz) (1 *))
                                                        z-%offset%)
                                         (* refsum
                                            (f2cl-lib:dconjg
                                             (f2cl-lib:fref v-%data%
                                                            (2 m)
                                                            ((1 ldv) (1 *))
                                                            v-%offset%)))))
                                (setf (f2cl-lib:fref z-%data%
                                                     (j (f2cl-lib:int-add k 3))
                                                     ((1 ldz) (1 *))
                                                     z-%offset%)
                                        (-
                                         (f2cl-lib:fref z-%data%
                                                        (j
                                                         (f2cl-lib:int-add k
                                                                           3))
                                                        ((1 ldz) (1 *))
                                                        z-%offset%)
                                         (* refsum
                                            (f2cl-lib:dconjg
                                             (f2cl-lib:fref v-%data%
                                                            (3 m)
                                                            ((1 ldv) (1 *))
                                                            v-%offset%)))))
                               label70))))))
                     label80))
                  (setf k
                          (f2cl-lib:int-add krcol
                                            (f2cl-lib:int-mul 3
                                                              (f2cl-lib:int-sub
                                                               m22
                                                               1))))
                  (cond
                    (bmp22
                     (cond
                       ((/= (f2cl-lib:fref v (1 m22) ((1 ldv) (1 *))) zero)
                        (f2cl-lib:fdo (j jtop (f2cl-lib:int-add j 1))
                                      ((> j
                                          (min (the f2cl-lib:integer4 kbot)
                                               (the f2cl-lib:integer4
                                                    (f2cl-lib:int-add k 3))))
                                       nil)
                          (tagbody
                            (setf refsum
                                    (*
                                     (f2cl-lib:fref v-%data%
                                                    (1 m22)
                                                    ((1 ldv) (1 *))
                                                    v-%offset%)
                                     (+
                                      (f2cl-lib:fref h-%data%
                                                     (j (f2cl-lib:int-add k 1))
                                                     ((1 ldh) (1 *))
                                                     h-%offset%)
                                      (*
                                       (f2cl-lib:fref v-%data%
                                                      (2 m22)
                                                      ((1 ldv) (1 *))
                                                      v-%offset%)
                                       (f2cl-lib:fref h-%data%
                                                      (j
                                                       (f2cl-lib:int-add k 2))
                                                      ((1 ldh) (1 *))
                                                      h-%offset%)))))
                            (setf (f2cl-lib:fref h-%data%
                                                 (j (f2cl-lib:int-add k 1))
                                                 ((1 ldh) (1 *))
                                                 h-%offset%)
                                    (-
                                     (f2cl-lib:fref h-%data%
                                                    (j (f2cl-lib:int-add k 1))
                                                    ((1 ldh) (1 *))
                                                    h-%offset%)
                                     refsum))
                            (setf (f2cl-lib:fref h-%data%
                                                 (j (f2cl-lib:int-add k 2))
                                                 ((1 ldh) (1 *))
                                                 h-%offset%)
                                    (-
                                     (f2cl-lib:fref h-%data%
                                                    (j (f2cl-lib:int-add k 2))
                                                    ((1 ldh) (1 *))
                                                    h-%offset%)
                                     (* refsum
                                        (f2cl-lib:dconjg
                                         (f2cl-lib:fref v-%data%
                                                        (2 m22)
                                                        ((1 ldv) (1 *))
                                                        v-%offset%)))))
                           label90))
                        (cond
                          (accum
                           (setf kms (f2cl-lib:int-sub k incol))
                           (f2cl-lib:fdo (j
                                          (max (the f2cl-lib:integer4 1)
                                               (the f2cl-lib:integer4
                                                    (f2cl-lib:int-add ktop
                                                                      (f2cl-lib:int-sub
                                                                       incol))))
                                          (f2cl-lib:int-add j 1))
                                         ((> j kdu) nil)
                             (tagbody
                               (setf refsum
                                       (*
                                        (f2cl-lib:fref v-%data%
                                                       (1 m22)
                                                       ((1 ldv) (1 *))
                                                       v-%offset%)
                                        (+
                                         (f2cl-lib:fref u-%data%
                                                        (j
                                                         (f2cl-lib:int-add kms
                                                                           1))
                                                        ((1 ldu) (1 *))
                                                        u-%offset%)
                                         (*
                                          (f2cl-lib:fref v-%data%
                                                         (2 m22)
                                                         ((1 ldv) (1 *))
                                                         v-%offset%)
                                          (f2cl-lib:fref u-%data%
                                                         (j
                                                          (f2cl-lib:int-add kms
                                                                            2))
                                                         ((1 ldu) (1 *))
                                                         u-%offset%)))))
                               (setf (f2cl-lib:fref u-%data%
                                                    (j
                                                     (f2cl-lib:int-add kms 1))
                                                    ((1 ldu) (1 *))
                                                    u-%offset%)
                                       (-
                                        (f2cl-lib:fref u-%data%
                                                       (j
                                                        (f2cl-lib:int-add kms
                                                                          1))
                                                       ((1 ldu) (1 *))
                                                       u-%offset%)
                                        refsum))
                               (setf (f2cl-lib:fref u-%data%
                                                    (j
                                                     (f2cl-lib:int-add kms 2))
                                                    ((1 ldu) (1 *))
                                                    u-%offset%)
                                       (-
                                        (f2cl-lib:fref u-%data%
                                                       (j
                                                        (f2cl-lib:int-add kms
                                                                          2))
                                                       ((1 ldu) (1 *))
                                                       u-%offset%)
                                        (* refsum
                                           (f2cl-lib:dconjg
                                            (f2cl-lib:fref v-%data%
                                                           (2 m22)
                                                           ((1 ldv) (1 *))
                                                           v-%offset%)))))
                              label100)))
                          (wantz
                           (f2cl-lib:fdo (j iloz (f2cl-lib:int-add j 1))
                                         ((> j ihiz) nil)
                             (tagbody
                               (setf refsum
                                       (*
                                        (f2cl-lib:fref v-%data%
                                                       (1 m22)
                                                       ((1 ldv) (1 *))
                                                       v-%offset%)
                                        (+
                                         (f2cl-lib:fref z-%data%
                                                        (j
                                                         (f2cl-lib:int-add k
                                                                           1))
                                                        ((1 ldz) (1 *))
                                                        z-%offset%)
                                         (*
                                          (f2cl-lib:fref v-%data%
                                                         (2 m22)
                                                         ((1 ldv) (1 *))
                                                         v-%offset%)
                                          (f2cl-lib:fref z-%data%
                                                         (j
                                                          (f2cl-lib:int-add k
                                                                            2))
                                                         ((1 ldz) (1 *))
                                                         z-%offset%)))))
                               (setf (f2cl-lib:fref z-%data%
                                                    (j (f2cl-lib:int-add k 1))
                                                    ((1 ldz) (1 *))
                                                    z-%offset%)
                                       (-
                                        (f2cl-lib:fref z-%data%
                                                       (j
                                                        (f2cl-lib:int-add k 1))
                                                       ((1 ldz) (1 *))
                                                       z-%offset%)
                                        refsum))
                               (setf (f2cl-lib:fref z-%data%
                                                    (j (f2cl-lib:int-add k 2))
                                                    ((1 ldz) (1 *))
                                                    z-%offset%)
                                       (-
                                        (f2cl-lib:fref z-%data%
                                                       (j
                                                        (f2cl-lib:int-add k 2))
                                                       ((1 ldz) (1 *))
                                                       z-%offset%)
                                        (* refsum
                                           (f2cl-lib:dconjg
                                            (f2cl-lib:fref v-%data%
                                                           (2 m22)
                                                           ((1 ldv) (1 *))
                                                           v-%offset%)))))
                              label110))))))))
                  (setf mstart mtop)
                  (if
                   (<
                    (f2cl-lib:int-add krcol
                                      (f2cl-lib:int-mul 3
                                                        (f2cl-lib:int-sub
                                                         mstart
                                                         1)))
                    ktop)
                   (setf mstart (f2cl-lib:int-add mstart 1)))
                  (setf mend mbot)
                  (if bmp22 (setf mend (f2cl-lib:int-add mend 1)))
                  (if (= krcol (f2cl-lib:int-sub kbot 2))
                      (setf mend (f2cl-lib:int-add mend 1)))
                  (f2cl-lib:fdo (m mstart (f2cl-lib:int-add m 1))
                                ((> m mend) nil)
                    (tagbody
                      (setf k
                              (min
                               (the f2cl-lib:integer4
                                    (f2cl-lib:int-sub kbot 1))
                               (the f2cl-lib:integer4
                                    (f2cl-lib:int-add krcol
                                                      (f2cl-lib:int-mul 3
                                                                        (f2cl-lib:int-sub
                                                                         m
                                                                         1))))))
                      (cond
                        ((/=
                          (f2cl-lib:fref h
                                         ((f2cl-lib:int-add k 1) k)
                                         ((1 ldh) (1 *)))
                          zero)
                         (setf tst1
                                 (+
                                  (cabs1
                                   (f2cl-lib:fref h-%data%
                                                  (k k)
                                                  ((1 ldh) (1 *))
                                                  h-%offset%))
                                  (cabs1
                                   (f2cl-lib:fref h-%data%
                                                  ((f2cl-lib:int-add k 1)
                                                   (f2cl-lib:int-add k 1))
                                                  ((1 ldh) (1 *))
                                                  h-%offset%))))
                         (cond
                           ((= tst1 rzero)
                            (if (>= k (f2cl-lib:int-add ktop 1))
                                (setf tst1
                                        (+ tst1
                                           (cabs1
                                            (f2cl-lib:fref h-%data%
                                                           (k
                                                            (f2cl-lib:int-sub k
                                                                              1))
                                                           ((1 ldh) (1 *))
                                                           h-%offset%)))))
                            (if (>= k (f2cl-lib:int-add ktop 2))
                                (setf tst1
                                        (+ tst1
                                           (cabs1
                                            (f2cl-lib:fref h-%data%
                                                           (k
                                                            (f2cl-lib:int-sub k
                                                                              2))
                                                           ((1 ldh) (1 *))
                                                           h-%offset%)))))
                            (if (>= k (f2cl-lib:int-add ktop 3))
                                (setf tst1
                                        (+ tst1
                                           (cabs1
                                            (f2cl-lib:fref h-%data%
                                                           (k
                                                            (f2cl-lib:int-sub k
                                                                              3))
                                                           ((1 ldh) (1 *))
                                                           h-%offset%)))))
                            (if (<= k (f2cl-lib:int-sub kbot 2))
                                (setf tst1
                                        (+ tst1
                                           (cabs1
                                            (f2cl-lib:fref h-%data%
                                                           ((f2cl-lib:int-add k
                                                                              2)
                                                            (f2cl-lib:int-add k
                                                                              1))
                                                           ((1 ldh) (1 *))
                                                           h-%offset%)))))
                            (if (<= k (f2cl-lib:int-sub kbot 3))
                                (setf tst1
                                        (+ tst1
                                           (cabs1
                                            (f2cl-lib:fref h-%data%
                                                           ((f2cl-lib:int-add k
                                                                              3)
                                                            (f2cl-lib:int-add k
                                                                              1))
                                                           ((1 ldh) (1 *))
                                                           h-%offset%)))))
                            (if (<= k (f2cl-lib:int-sub kbot 4))
                                (setf tst1
                                        (+ tst1
                                           (cabs1
                                            (f2cl-lib:fref h-%data%
                                                           ((f2cl-lib:int-add k
                                                                              4)
                                                            (f2cl-lib:int-add k
                                                                              1))
                                                           ((1 ldh) (1 *))
                                                           h-%offset%)))))))
                         (cond
                           ((<=
                             (cabs1
                              (f2cl-lib:fref h
                                             ((f2cl-lib:int-add k 1) k)
                                             ((1 ldh) (1 *))))
                             (max smlnum (* ulp tst1)))
                            (setf h12
                                    (max
                                     (cabs1
                                      (f2cl-lib:fref h-%data%
                                                     ((f2cl-lib:int-add k 1) k)
                                                     ((1 ldh) (1 *))
                                                     h-%offset%))
                                     (cabs1
                                      (f2cl-lib:fref h-%data%
                                                     (k (f2cl-lib:int-add k 1))
                                                     ((1 ldh) (1 *))
                                                     h-%offset%))))
                            (setf h21
                                    (min
                                     (cabs1
                                      (f2cl-lib:fref h-%data%
                                                     ((f2cl-lib:int-add k 1) k)
                                                     ((1 ldh) (1 *))
                                                     h-%offset%))
                                     (cabs1
                                      (f2cl-lib:fref h-%data%
                                                     (k (f2cl-lib:int-add k 1))
                                                     ((1 ldh) (1 *))
                                                     h-%offset%))))
                            (setf h11
                                    (max
                                     (cabs1
                                      (f2cl-lib:fref h-%data%
                                                     ((f2cl-lib:int-add k 1)
                                                      (f2cl-lib:int-add k 1))
                                                     ((1 ldh) (1 *))
                                                     h-%offset%))
                                     (cabs1
                                      (-
                                       (f2cl-lib:fref h-%data%
                                                      (k k)
                                                      ((1 ldh) (1 *))
                                                      h-%offset%)
                                       (f2cl-lib:fref h-%data%
                                                      ((f2cl-lib:int-add k 1)
                                                       (f2cl-lib:int-add k 1))
                                                      ((1 ldh) (1 *))
                                                      h-%offset%)))))
                            (setf h22
                                    (min
                                     (cabs1
                                      (f2cl-lib:fref h-%data%
                                                     ((f2cl-lib:int-add k 1)
                                                      (f2cl-lib:int-add k 1))
                                                     ((1 ldh) (1 *))
                                                     h-%offset%))
                                     (cabs1
                                      (-
                                       (f2cl-lib:fref h-%data%
                                                      (k k)
                                                      ((1 ldh) (1 *))
                                                      h-%offset%)
                                       (f2cl-lib:fref h-%data%
                                                      ((f2cl-lib:int-add k 1)
                                                       (f2cl-lib:int-add k 1))
                                                      ((1 ldh) (1 *))
                                                      h-%offset%)))))
                            (setf scl (+ h11 h12))
                            (setf tst2 (* h22 (/ h11 scl)))
                            (if
                             (or (= tst2 rzero)
                                 (<= (* h21 (/ h12 scl))
                                     (max smlnum (* ulp tst2))))
                             (setf (f2cl-lib:fref h-%data%
                                                  ((f2cl-lib:int-add k 1) k)
                                                  ((1 ldh) (1 *))
                                                  h-%offset%)
                                     zero))))))
                     label120))
                  (setf mend
                          (min nbmps
                               (the f2cl-lib:integer4
                                    (truncate (- kbot krcol 1) 3))))
                  (f2cl-lib:fdo (m mtop (f2cl-lib:int-add m 1))
                                ((> m mend) nil)
                    (tagbody
                      (setf k
                              (f2cl-lib:int-add krcol
                                                (f2cl-lib:int-mul 3
                                                                  (f2cl-lib:int-sub
                                                                   m
                                                                   1))))
                      (setf refsum
                              (*
                               (f2cl-lib:fref v-%data%
                                              (1 m)
                                              ((1 ldv) (1 *))
                                              v-%offset%)
                               (f2cl-lib:fref v-%data%
                                              (3 m)
                                              ((1 ldv) (1 *))
                                              v-%offset%)
                               (f2cl-lib:fref h-%data%
                                              ((f2cl-lib:int-add k 4)
                                               (f2cl-lib:int-add k 3))
                                              ((1 ldh) (1 *))
                                              h-%offset%)))
                      (setf (f2cl-lib:fref h-%data%
                                           ((f2cl-lib:int-add k 4)
                                            (f2cl-lib:int-add k 1))
                                           ((1 ldh) (1 *))
                                           h-%offset%)
                              (- refsum))
                      (setf (f2cl-lib:fref h-%data%
                                           ((f2cl-lib:int-add k 4)
                                            (f2cl-lib:int-add k 2))
                                           ((1 ldh) (1 *))
                                           h-%offset%)
                              (* (- refsum)
                                 (f2cl-lib:dconjg
                                  (f2cl-lib:fref v-%data%
                                                 (2 m)
                                                 ((1 ldv) (1 *))
                                                 v-%offset%))))
                      (setf (f2cl-lib:fref h-%data%
                                           ((f2cl-lib:int-add k 4)
                                            (f2cl-lib:int-add k 3))
                                           ((1 ldh) (1 *))
                                           h-%offset%)
                              (-
                               (f2cl-lib:fref h-%data%
                                              ((f2cl-lib:int-add k 4)
                                               (f2cl-lib:int-add k 3))
                                              ((1 ldh) (1 *))
                                              h-%offset%)
                               (* refsum
                                  (f2cl-lib:dconjg
                                   (f2cl-lib:fref v-%data%
                                                  (3 m)
                                                  ((1 ldv) (1 *))
                                                  v-%offset%)))))
                     label130))
                 label140))
              (cond
                (accum
                 (cond
                   (wantt
                    (setf jtop 1)
                    (setf jbot n))
                   (t
                    (setf jtop ktop)
                    (setf jbot kbot)))
                 (cond
                   ((or (not blk22) (< incol ktop) (> ndcol kbot) (<= ns 2))
                    (setf k1
                            (max (the f2cl-lib:integer4 1)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-sub ktop incol))))
                    (setf nu
                            (f2cl-lib:int-add
                             (f2cl-lib:int-sub kdu
                                               (max (the f2cl-lib:integer4 0)
                                                    (the f2cl-lib:integer4
                                                         (f2cl-lib:int-sub
                                                          ndcol
                                                          kbot)))
                                               k1)
                             1))
                    (f2cl-lib:fdo (jcol
                                   (f2cl-lib:int-add
                                    (min (the f2cl-lib:integer4 ndcol)
                                         (the f2cl-lib:integer4 kbot))
                                    1)
                                   (f2cl-lib:int-add jcol nh))
                                  ((> jcol jbot) nil)
                      (tagbody
                        (setf jlen
                                (min (the f2cl-lib:integer4 nh)
                                     (the f2cl-lib:integer4
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-sub jbot jcol)
                                           1))))
                        (zgemm "C" "N" nu jlen nu one
                         (f2cl-lib:array-slice u-%data%
                                               f2cl-lib:complex16
                                               (k1 k1)
                                               ((1 ldu) (1 *))
                                               u-%offset%)
                         ldu
                         (f2cl-lib:array-slice h-%data%
                                               f2cl-lib:complex16
                                               ((+ incol k1) jcol)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                         ldh zero wh ldwh)
                        (zlacpy "ALL" nu jlen wh ldwh
                         (f2cl-lib:array-slice h-%data%
                                               f2cl-lib:complex16
                                               ((+ incol k1) jcol)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                         ldh)
                       label150))
                    (f2cl-lib:fdo (jrow jtop (f2cl-lib:int-add jrow nv))
                                  ((> jrow
                                      (f2cl-lib:int-add
                                       (max (the f2cl-lib:integer4 ktop)
                                            (the f2cl-lib:integer4 incol))
                                       (f2cl-lib:int-sub 1)))
                                   nil)
                      (tagbody
                        (setf jlen
                                (min (the f2cl-lib:integer4 nv)
                                     (the f2cl-lib:integer4
                                          (f2cl-lib:int-sub
                                           (max (the f2cl-lib:integer4 ktop)
                                                (the f2cl-lib:integer4 incol))
                                           jrow))))
                        (zgemm "N" "N" jlen nu nu one
                         (f2cl-lib:array-slice h-%data%
                                               f2cl-lib:complex16
                                               (jrow
                                                (f2cl-lib:int-add incol k1))
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                         ldh
                         (f2cl-lib:array-slice u-%data%
                                               f2cl-lib:complex16
                                               (k1 k1)
                                               ((1 ldu) (1 *))
                                               u-%offset%)
                         ldu zero wv ldwv)
                        (zlacpy "ALL" jlen nu wv ldwv
                         (f2cl-lib:array-slice h-%data%
                                               f2cl-lib:complex16
                                               (jrow
                                                (f2cl-lib:int-add incol k1))
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                         ldh)
                       label160))
                    (cond
                      (wantz
                       (f2cl-lib:fdo (jrow iloz (f2cl-lib:int-add jrow nv))
                                     ((> jrow ihiz) nil)
                         (tagbody
                           (setf jlen
                                   (min (the f2cl-lib:integer4 nv)
                                        (the f2cl-lib:integer4
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-sub ihiz jrow)
                                              1))))
                           (zgemm "N" "N" jlen nu nu one
                            (f2cl-lib:array-slice z-%data%
                                                  f2cl-lib:complex16
                                                  (jrow
                                                   (f2cl-lib:int-add incol k1))
                                                  ((1 ldz) (1 *))
                                                  z-%offset%)
                            ldz
                            (f2cl-lib:array-slice u-%data%
                                                  f2cl-lib:complex16
                                                  (k1 k1)
                                                  ((1 ldu) (1 *))
                                                  u-%offset%)
                            ldu zero wv ldwv)
                           (zlacpy "ALL" jlen nu wv ldwv
                            (f2cl-lib:array-slice z-%data%
                                                  f2cl-lib:complex16
                                                  (jrow
                                                   (f2cl-lib:int-add incol k1))
                                                  ((1 ldz) (1 *))
                                                  z-%offset%)
                            ldz)
                          label170)))))
                   (t
                    (setf i2 (the f2cl-lib:integer4 (truncate (+ kdu 1) 2)))
                    (setf i4 kdu)
                    (setf j2 (f2cl-lib:int-sub i4 i2))
                    (setf j4 kdu)
                    (setf kzs (f2cl-lib:int-sub j4 j2 (f2cl-lib:int-add ns 1)))
                    (setf knz (f2cl-lib:int-add ns 1))
                    (f2cl-lib:fdo (jcol
                                   (f2cl-lib:int-add
                                    (min (the f2cl-lib:integer4 ndcol)
                                         (the f2cl-lib:integer4 kbot))
                                    1)
                                   (f2cl-lib:int-add jcol nh))
                                  ((> jcol jbot) nil)
                      (tagbody
                        (setf jlen
                                (min (the f2cl-lib:integer4 nh)
                                     (the f2cl-lib:integer4
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-sub jbot jcol)
                                           1))))
                        (zlacpy "ALL" knz jlen
                         (f2cl-lib:array-slice h-%data%
                                               f2cl-lib:complex16
                                               ((+ incol 1 j2) jcol)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                         ldh
                         (f2cl-lib:array-slice wh-%data%
                                               f2cl-lib:complex16
                                               ((+ kzs 1) 1)
                                               ((1 ldwh) (1 *))
                                               wh-%offset%)
                         ldwh)
                        (zlaset "ALL" kzs jlen zero zero wh ldwh)
                        (ztrmm "L" "U" "C" "N" knz jlen one
                         (f2cl-lib:array-slice u-%data%
                                               f2cl-lib:complex16
                                               ((+ j2 1)
                                                (f2cl-lib:int-add 1 kzs))
                                               ((1 ldu) (1 *))
                                               u-%offset%)
                         ldu
                         (f2cl-lib:array-slice wh-%data%
                                               f2cl-lib:complex16
                                               ((+ kzs 1) 1)
                                               ((1 ldwh) (1 *))
                                               wh-%offset%)
                         ldwh)
                        (zgemm "C" "N" i2 jlen j2 one u ldu
                         (f2cl-lib:array-slice h-%data%
                                               f2cl-lib:complex16
                                               ((+ incol 1) jcol)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                         ldh one wh ldwh)
                        (zlacpy "ALL" j2 jlen
                         (f2cl-lib:array-slice h-%data%
                                               f2cl-lib:complex16
                                               ((+ incol 1) jcol)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                         ldh
                         (f2cl-lib:array-slice wh-%data%
                                               f2cl-lib:complex16
                                               ((+ i2 1) 1)
                                               ((1 ldwh) (1 *))
                                               wh-%offset%)
                         ldwh)
                        (ztrmm "L" "L" "C" "N" j2 jlen one
                         (f2cl-lib:array-slice u-%data%
                                               f2cl-lib:complex16
                                               (1 (f2cl-lib:int-add i2 1))
                                               ((1 ldu) (1 *))
                                               u-%offset%)
                         ldu
                         (f2cl-lib:array-slice wh-%data%
                                               f2cl-lib:complex16
                                               ((+ i2 1) 1)
                                               ((1 ldwh) (1 *))
                                               wh-%offset%)
                         ldwh)
                        (zgemm "C" "N" (f2cl-lib:int-sub i4 i2) jlen
                         (f2cl-lib:int-sub j4 j2) one
                         (f2cl-lib:array-slice u-%data%
                                               f2cl-lib:complex16
                                               ((+ j2 1)
                                                (f2cl-lib:int-add i2 1))
                                               ((1 ldu) (1 *))
                                               u-%offset%)
                         ldu
                         (f2cl-lib:array-slice h-%data%
                                               f2cl-lib:complex16
                                               ((+ incol 1 j2) jcol)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                         ldh one
                         (f2cl-lib:array-slice wh-%data%
                                               f2cl-lib:complex16
                                               ((+ i2 1) 1)
                                               ((1 ldwh) (1 *))
                                               wh-%offset%)
                         ldwh)
                        (zlacpy "ALL" kdu jlen wh ldwh
                         (f2cl-lib:array-slice h-%data%
                                               f2cl-lib:complex16
                                               ((+ incol 1) jcol)
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                         ldh)
                       label180))
                    (f2cl-lib:fdo (jrow jtop (f2cl-lib:int-add jrow nv))
                                  ((> jrow
                                      (f2cl-lib:int-add
                                       (max (the f2cl-lib:integer4 incol)
                                            (the f2cl-lib:integer4 ktop))
                                       (f2cl-lib:int-sub 1)))
                                   nil)
                      (tagbody
                        (setf jlen
                                (min (the f2cl-lib:integer4 nv)
                                     (the f2cl-lib:integer4
                                          (f2cl-lib:int-sub
                                           (max (the f2cl-lib:integer4 incol)
                                                (the f2cl-lib:integer4 ktop))
                                           jrow))))
                        (zlacpy "ALL" jlen knz
                         (f2cl-lib:array-slice h-%data%
                                               f2cl-lib:complex16
                                               (jrow
                                                (f2cl-lib:int-add incol 1 j2))
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                         ldh
                         (f2cl-lib:array-slice wv-%data%
                                               f2cl-lib:complex16
                                               (1 (f2cl-lib:int-add 1 kzs))
                                               ((1 ldwv) (1 *))
                                               wv-%offset%)
                         ldwv)
                        (zlaset "ALL" jlen kzs zero zero wv ldwv)
                        (ztrmm "R" "U" "N" "N" jlen knz one
                         (f2cl-lib:array-slice u-%data%
                                               f2cl-lib:complex16
                                               ((+ j2 1)
                                                (f2cl-lib:int-add 1 kzs))
                                               ((1 ldu) (1 *))
                                               u-%offset%)
                         ldu
                         (f2cl-lib:array-slice wv-%data%
                                               f2cl-lib:complex16
                                               (1 (f2cl-lib:int-add 1 kzs))
                                               ((1 ldwv) (1 *))
                                               wv-%offset%)
                         ldwv)
                        (zgemm "N" "N" jlen i2 j2 one
                         (f2cl-lib:array-slice h-%data%
                                               f2cl-lib:complex16
                                               (jrow
                                                (f2cl-lib:int-add incol 1))
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                         ldh u ldu one wv ldwv)
                        (zlacpy "ALL" jlen j2
                         (f2cl-lib:array-slice h-%data%
                                               f2cl-lib:complex16
                                               (jrow
                                                (f2cl-lib:int-add incol 1))
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                         ldh
                         (f2cl-lib:array-slice wv-%data%
                                               f2cl-lib:complex16
                                               (1 (f2cl-lib:int-add 1 i2))
                                               ((1 ldwv) (1 *))
                                               wv-%offset%)
                         ldwv)
                        (ztrmm "R" "L" "N" "N" jlen (f2cl-lib:int-sub i4 i2)
                         one
                         (f2cl-lib:array-slice u-%data%
                                               f2cl-lib:complex16
                                               (1 (f2cl-lib:int-add i2 1))
                                               ((1 ldu) (1 *))
                                               u-%offset%)
                         ldu
                         (f2cl-lib:array-slice wv-%data%
                                               f2cl-lib:complex16
                                               (1 (f2cl-lib:int-add 1 i2))
                                               ((1 ldwv) (1 *))
                                               wv-%offset%)
                         ldwv)
                        (zgemm "N" "N" jlen (f2cl-lib:int-sub i4 i2)
                         (f2cl-lib:int-sub j4 j2) one
                         (f2cl-lib:array-slice h-%data%
                                               f2cl-lib:complex16
                                               (jrow
                                                (f2cl-lib:int-add incol 1 j2))
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                         ldh
                         (f2cl-lib:array-slice u-%data%
                                               f2cl-lib:complex16
                                               ((+ j2 1)
                                                (f2cl-lib:int-add i2 1))
                                               ((1 ldu) (1 *))
                                               u-%offset%)
                         ldu one
                         (f2cl-lib:array-slice wv-%data%
                                               f2cl-lib:complex16
                                               (1 (f2cl-lib:int-add 1 i2))
                                               ((1 ldwv) (1 *))
                                               wv-%offset%)
                         ldwv)
                        (zlacpy "ALL" jlen kdu wv ldwv
                         (f2cl-lib:array-slice h-%data%
                                               f2cl-lib:complex16
                                               (jrow
                                                (f2cl-lib:int-add incol 1))
                                               ((1 ldh) (1 *))
                                               h-%offset%)
                         ldh)
                       label190))
                    (cond
                      (wantz
                       (f2cl-lib:fdo (jrow iloz (f2cl-lib:int-add jrow nv))
                                     ((> jrow ihiz) nil)
                         (tagbody
                           (setf jlen
                                   (min (the f2cl-lib:integer4 nv)
                                        (the f2cl-lib:integer4
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-sub ihiz jrow)
                                              1))))
                           (zlacpy "ALL" jlen knz
                            (f2cl-lib:array-slice z-%data%
                                                  f2cl-lib:complex16
                                                  (jrow
                                                   (f2cl-lib:int-add incol
                                                                     1
                                                                     j2))
                                                  ((1 ldz) (1 *))
                                                  z-%offset%)
                            ldz
                            (f2cl-lib:array-slice wv-%data%
                                                  f2cl-lib:complex16
                                                  (1 (f2cl-lib:int-add 1 kzs))
                                                  ((1 ldwv) (1 *))
                                                  wv-%offset%)
                            ldwv)
                           (zlaset "ALL" jlen kzs zero zero wv ldwv)
                           (ztrmm "R" "U" "N" "N" jlen knz one
                            (f2cl-lib:array-slice u-%data%
                                                  f2cl-lib:complex16
                                                  ((+ j2 1)
                                                   (f2cl-lib:int-add 1 kzs))
                                                  ((1 ldu) (1 *))
                                                  u-%offset%)
                            ldu
                            (f2cl-lib:array-slice wv-%data%
                                                  f2cl-lib:complex16
                                                  (1 (f2cl-lib:int-add 1 kzs))
                                                  ((1 ldwv) (1 *))
                                                  wv-%offset%)
                            ldwv)
                           (zgemm "N" "N" jlen i2 j2 one
                            (f2cl-lib:array-slice z-%data%
                                                  f2cl-lib:complex16
                                                  (jrow
                                                   (f2cl-lib:int-add incol 1))
                                                  ((1 ldz) (1 *))
                                                  z-%offset%)
                            ldz u ldu one wv ldwv)
                           (zlacpy "ALL" jlen j2
                            (f2cl-lib:array-slice z-%data%
                                                  f2cl-lib:complex16
                                                  (jrow
                                                   (f2cl-lib:int-add incol 1))
                                                  ((1 ldz) (1 *))
                                                  z-%offset%)
                            ldz
                            (f2cl-lib:array-slice wv-%data%
                                                  f2cl-lib:complex16
                                                  (1 (f2cl-lib:int-add 1 i2))
                                                  ((1 ldwv) (1 *))
                                                  wv-%offset%)
                            ldwv)
                           (ztrmm "R" "L" "N" "N" jlen (f2cl-lib:int-sub i4 i2)
                            one
                            (f2cl-lib:array-slice u-%data%
                                                  f2cl-lib:complex16
                                                  (1 (f2cl-lib:int-add i2 1))
                                                  ((1 ldu) (1 *))
                                                  u-%offset%)
                            ldu
                            (f2cl-lib:array-slice wv-%data%
                                                  f2cl-lib:complex16
                                                  (1 (f2cl-lib:int-add 1 i2))
                                                  ((1 ldwv) (1 *))
                                                  wv-%offset%)
                            ldwv)
                           (zgemm "N" "N" jlen (f2cl-lib:int-sub i4 i2)
                            (f2cl-lib:int-sub j4 j2) one
                            (f2cl-lib:array-slice z-%data%
                                                  f2cl-lib:complex16
                                                  (jrow
                                                   (f2cl-lib:int-add incol
                                                                     1
                                                                     j2))
                                                  ((1 ldz) (1 *))
                                                  z-%offset%)
                            ldz
                            (f2cl-lib:array-slice u-%data%
                                                  f2cl-lib:complex16
                                                  ((+ j2 1)
                                                   (f2cl-lib:int-add i2 1))
                                                  ((1 ldu) (1 *))
                                                  u-%offset%)
                            ldu one
                            (f2cl-lib:array-slice wv-%data%
                                                  f2cl-lib:complex16
                                                  (1 (f2cl-lib:int-add 1 i2))
                                                  ((1 ldwv) (1 *))
                                                  wv-%offset%)
                            ldwv)
                           (zlacpy "ALL" jlen kdu wv ldwv
                            (f2cl-lib:array-slice z-%data%
                                                  f2cl-lib:complex16
                                                  (jrow
                                                   (f2cl-lib:int-add incol 1))
                                                  ((1 ldz) (1 *))
                                                  z-%offset%)
                            ldz)
                          label200))))))))
             label210))
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
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zlaqr5
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(fortran-to-lisp::logical fortran-to-lisp::logical
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::ztrmm fortran-to-lisp::zlacpy
                    fortran-to-lisp::zgemm fortran-to-lisp::zlarfg
                    fortran-to-lisp::zlaqr1 fortran-to-lisp::zlaset
                    fortran-to-lisp::dlabad fortran-to-lisp::dlamch))))

