;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 46c1f6a93b0d 2012/05/03 04:40:28 toy $"
;;;  "f2cl2.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 46c1f6a93b0d 2012/05/03 04:40:28 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v fceac530ef0c 2011/11/26 04:02:26 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2012-04 (20C Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((gln
       (make-array 100
                   :element-type 'double-float
                   :initial-contents '(0.0 0.0 0.6931471805599453
                                       1.791759469228055 3.1780538303479458
                                       4.787491742782046 6.579251212010101
                                       8.525161361065415 10.60460290274525
                                       12.801827480081469 15.104412573075516
                                       17.502307845873887 19.987214495661885
                                       22.552163853123425 25.19122118273868
                                       27.89927138384089 30.671860106080672
                                       33.50507345013689 36.39544520803305
                                       39.339884187199495 42.335616460753485
                                       45.38013889847691 48.47118135183523
                                       51.60667556776438 54.78472939811232
                                       58.00360522298052 61.261701761002
                                       64.55753862700634 67.88974313718154
                                       71.25703896716801 74.65823634883016
                                       78.0922235533153 81.55795945611504
                                       85.05446701758152 88.58082754219768
                                       92.1361756036871 95.7196945421432
                                       99.33061245478743 102.96819861451381
                                       106.63176026064346 110.32063971475739
                                       114.0342117814617 117.77188139974507
                                       121.53308151543864 125.3172711493569
                                       129.12393363912722 132.95257503561632
                                       136.80272263732635 140.67392364823425
                                       144.5657439463449 148.47776695177302
                                       152.40959258449735 156.3608363030788
                                       160.3311282166309 164.32011226319517
                                       168.32744544842765 172.3527971391628
                                       176.39584840699735 180.45629141754378
                                       184.53382886144948 188.6281734236716
                                       192.7390472878449 196.86618167289
                                       201.00931639928152 205.1681994826412
                                       209.34258675253685 213.53224149456327
                                       217.73693411395422 221.95644181913033
                                       226.1905483237276 230.43904356577696
                                       234.70172344281826 238.97838956183432
                                       243.2688490029827 247.57291409618688
                                       251.8904022097232 256.22113555000954
                                       260.5649409718632 264.9216497985528
                                       269.2910976510198 273.6731242856937
                                       278.0675734403661 282.4742926876304
                                       286.893133295427 291.3239500942703
                                       295.76660135076065 300.22094864701415
                                       304.6868567656687 309.1641935801469
                                       313.65282994987905 318.1526396202093
                                       322.66349912672615 327.1852877037752
                                       331.7178871969285 336.26118197919845
                                       340.815058870799 345.37940706226686
                                       349.95411804077025 354.5390855194408
                                       359.1342053695754)))
      (cf
       (make-array 22
                   :element-type 'double-float
                   :initial-contents '(0.08333333333333333
                                       -0.002777777777777778
                                       7.936507936507937e-4
                                       -5.952380952380953e-4
                                       8.417508417508417e-4
                                       -0.0019175269175269176
                                       0.00641025641025641
                                       -0.029550653594771242
                                       0.17964437236883057 -1.3924322169059011
                                       13.402864044168393 -156.84828462600203
                                       2193.1033333333335 -36108.77125372499
                                       691472.268851313 -1.5238221539407415e7
                                       3.8290075139141417e8
                                       -1.0882266035784391e10
                                       3.4732028376500226e11
                                       -1.2369602142269275e13
                                       4.887880647930793e14
                                       -2.1320333960919372e16)))
      (con 1.8378770664093456))
  (declare (type (simple-array double-float (100)) gln)
           (type (simple-array double-float (22)) cf)
           (type (double-float) con))
  (defun dgamln (z ierr)
    (declare (type (f2cl-lib:integer4) ierr) (type (double-float) z))
    (prog ((i 0) (i1m 0) (k 0) (mz 0) (nz 0) (fln 0.0) (fz 0.0) (rln 0.0)
           (s 0.0) (tlg 0.0) (trm 0.0) (tst 0.0) (t1 0.0) (wdtol 0.0)
           (zdmy 0.0) (zinc 0.0) (zm 0.0) (zmin 0.0) (zp 0.0) (zsq 0.0)
           (dgamln 0.0))
      (declare (type (double-float) dgamln zsq zp zmin zm zinc zdmy wdtol t1
                                    tst trm tlg s rln fz fln)
               (type (f2cl-lib:integer4) nz mz k i1m i))
      (setf ierr 0)
      (if (<= z 0.0) (go label70))
      (if (> z 101.0) (go label10))
      (setf nz (f2cl-lib:int z))
      (setf fz (- z nz))
      (if (> fz 0.0) (go label10))
      (if (> nz 100) (go label10))
      (setf dgamln (f2cl-lib:fref gln (nz) ((1 100))))
      (go end_label)
     label10
      (setf wdtol (f2cl-lib:d1mach 4))
      (setf wdtol (max wdtol 5.0e-19))
      (setf i1m (f2cl-lib:i1mach 14))
      (setf rln (* (f2cl-lib:d1mach 5) i1m))
      (setf fln (min rln 20.0))
      (setf fln (max fln 3.0))
      (setf fln (- fln 3.0))
      (setf zm (+ 1.8 (* 0.3875 fln)))
      (setf mz (f2cl-lib:int (+ zm 1)))
      (setf zmin (coerce (the f2cl-lib:integer4 mz) 'double-float))
      (setf zdmy z)
      (setf zinc 0.0)
      (if (>= z zmin) (go label20))
      (setf zinc (- zmin nz))
      (setf zdmy (+ z zinc))
     label20
      (setf zp (/ 1.0 zdmy))
      (setf t1 (* (f2cl-lib:fref cf (1) ((1 22))) zp))
      (setf s t1)
      (if (< zp wdtol) (go label40))
      (setf zsq (* zp zp))
      (setf tst (* t1 wdtol))
      (f2cl-lib:fdo (k 2 (f2cl-lib:int-add k 1))
                    ((> k 22) nil)
        (tagbody
          (setf zp (* zp zsq))
          (setf trm (* (f2cl-lib:fref cf (k) ((1 22))) zp))
          (if (< (abs trm) tst) (go label40))
          (setf s (+ s trm))
         label30))
     label40
      (if (/= zinc 0.0) (go label50))
      (setf tlg (f2cl-lib:flog z))
      (setf dgamln (+ (* z (- tlg 1.0)) (* 0.5 (- con tlg)) s))
      (go end_label)
     label50
      (setf zp 1.0)
      (setf nz (f2cl-lib:int zinc))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i nz) nil)
        (tagbody (setf zp (* zp (+ z (f2cl-lib:int-sub i 1)))) label60))
      (setf tlg (f2cl-lib:flog zdmy))
      (setf dgamln
              (+ (- (* zdmy (- tlg 1.0)) (f2cl-lib:flog zp))
                 (* 0.5 (- con tlg))
                 s))
      (go end_label)
     label70
      (setf dgamln (f2cl-lib:d1mach 2))
      (setf ierr 1)
      (go end_label)
     end_label
      (return (values dgamln nil ierr)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgamln
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (fortran-to-lisp::integer4))
           :return-values '(nil fortran-to-lisp::ierr)
           :calls '(fortran-to-lisp::i1mach fortran-to-lisp::d1mach))))

