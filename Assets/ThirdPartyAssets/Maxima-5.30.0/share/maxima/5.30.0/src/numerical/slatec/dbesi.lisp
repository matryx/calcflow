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
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((rttpi 0.398942280401433) (inlim 80))
  (declare (type (double-float) rttpi) (type (f2cl-lib:integer4) inlim))
  (defun dbesi (x alpha kode n y nz)
    (declare (type (array double-float (*)) y)
             (type (f2cl-lib:integer4) nz n kode)
             (type (double-float) alpha x))
    (f2cl-lib:with-multi-array-data
        ((y double-float y-%data% y-%offset%))
      (prog ((temp (make-array 3 :element-type 'double-float)) (ain 0.0)
             (ak 0.0) (akm 0.0) (ans 0.0) (ap 0.0) (arg 0.0) (atol 0.0)
             (tolln 0.0) (dfn 0.0) (dtm 0.0) (dx 0.0) (earg 0.0) (elim 0.0)
             (etx 0.0) (flgik 0.0) (fn 0.0) (fnf 0.0) (fni 0.0) (fnp1 0.0)
             (fnu 0.0) (gln 0.0) (ra 0.0) (s 0.0) (sx 0.0) (sxo2 0.0) (s1 0.0)
             (s2 0.0) (t$ 0.0) (ta 0.0) (tb 0.0) (tfn 0.0) (tm 0.0) (tol 0.0)
             (trx 0.0) (t2 0.0) (xo2 0.0) (xo2l 0.0) (z 0.0) (i 0) (ialp 0)
             (in 0) (is 0) (i1 0) (k 0) (kk 0) (km 0) (kt 0) (nn 0) (ns 0))
        (declare (type (f2cl-lib:integer4) ns nn kt km kk k i1 is in ialp i)
                 (type (array double-float (3)) temp)
                 (type (double-float) z xo2l xo2 t2 trx tol tm tfn tb ta t$ s2
                                      s1 sxo2 sx s ra gln fnu fnp1 fni fnf fn
                                      flgik etx elim earg dx dtm dfn tolln atol
                                      arg ap ans akm ak ain))
        (setf nz 0)
        (setf kt 1)
        (setf ra (f2cl-lib:d1mach 3))
        (setf tol (max ra 1.0e-15))
        (setf i1 (f2cl-lib:int-sub (f2cl-lib:i1mach 15)))
        (setf gln (f2cl-lib:d1mach 5))
        (setf elim (* 2.303 (- (* i1 gln) 3.0)))
        (setf i1 (f2cl-lib:int-add (f2cl-lib:i1mach 14) 1))
        (setf tolln (* 2.303 gln i1))
        (setf tolln (min tolln 34.5388))
        (f2cl-lib:arithmetic-if (f2cl-lib:int-sub n 1)
                                (go label590)
                                (go label10)
                                (go label20))
       label10
        (setf kt 2)
       label20
        (setf nn n)
        (if (or (< kode 1) (> kode 2)) (go label570))
        (f2cl-lib:arithmetic-if x (go label600) (go label30) (go label80))
       label30
        (f2cl-lib:arithmetic-if alpha (go label580) (go label40) (go label50))
       label40
        (setf (f2cl-lib:fref y-%data% (1) ((1 *)) y-%offset%) 1.0)
        (if (= n 1) (go end_label))
        (setf i1 2)
        (go label60)
       label50
        (setf i1 1)
       label60
        (f2cl-lib:fdo (i i1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%) 0.0)
           label70))
        (go end_label)
       label80
        (if (< alpha 0.0) (go label580))
        (setf ialp (f2cl-lib:int alpha))
        (setf fni
                (coerce
                 (the f2cl-lib:integer4
                      (f2cl-lib:int-sub (f2cl-lib:int-add ialp n) 1))
                 'double-float))
        (setf fnf (- alpha ialp))
        (setf dfn (+ fni fnf))
        (setf fnu dfn)
        (setf in 0)
        (setf xo2 (* x 0.5))
        (setf sxo2 (* xo2 xo2))
        (setf etx
                (coerce (the f2cl-lib:integer4 (f2cl-lib:int-sub kode 1))
                        'double-float))
        (setf sx (* etx x))
        (if (<= sxo2 (+ fnu 1.0)) (go label90))
        (if (<= x 12.0) (go label110))
        (setf fn (* 0.55 fnu fnu))
        (setf fn (max 17.0 fn))
        (if (>= x fn) (go label430))
        (setf ans (max (- 36.0 fnu) 0.0))
        (setf ns (f2cl-lib:int ans))
        (setf fni (+ fni ns))
        (setf dfn (+ fni fnf))
        (setf fn dfn)
        (setf is kt)
        (setf km (f2cl-lib:int-add (f2cl-lib:int-sub n 1) ns))
        (if (> km 0) (setf is 3))
        (go label120)
       label90
        (setf fn fnu)
        (setf fnp1 (+ fn 1.0))
        (setf xo2l (f2cl-lib:flog xo2))
        (setf is kt)
        (if (<= x 0.5) (go label230))
        (setf ns 0)
       label100
        (setf fni (+ fni ns))
        (setf dfn (+ fni fnf))
        (setf fn dfn)
        (setf fnp1 (+ fn 1.0))
        (setf is kt)
        (if (> (f2cl-lib:int-add (f2cl-lib:int-sub n 1) ns) 0) (setf is 3))
        (go label230)
       label110
        (setf xo2l (f2cl-lib:flog xo2))
        (setf ns (f2cl-lib:int (- sxo2 fnu)))
        (go label100)
       label120
        (if (= kode 2) (go label130))
        (if (< alpha 1.0) (go label150))
        (setf z (/ x alpha))
        (setf ra (f2cl-lib:fsqrt (+ 1.0 (* z z))))
        (setf gln (f2cl-lib:flog (/ (+ 1.0 ra) z)))
        (setf t$ (+ (* ra (- 1.0 etx)) (/ etx (+ z ra))))
        (setf arg (* alpha (- t$ gln)))
        (if (> arg elim) (go label610))
        (if (= km 0) (go label140))
       label130
        (setf z (/ x fn))
        (setf ra (f2cl-lib:fsqrt (+ 1.0 (* z z))))
        (setf gln (f2cl-lib:flog (/ (+ 1.0 ra) z)))
        (setf t$ (+ (* ra (- 1.0 etx)) (/ etx (+ z ra))))
        (setf arg (* fn (- t$ gln)))
       label140
        (if (< arg (- elim)) (go label280))
        (go label190)
       label150
        (if (> x elim) (go label610))
        (go label130)
       label160
        (if (/= km 0) (go label170))
        (setf (f2cl-lib:fref y-%data% (1) ((1 *)) y-%offset%)
                (f2cl-lib:fref temp (3) ((1 3))))
        (go end_label)
       label170
        (setf (f2cl-lib:fref temp (1) ((1 3)))
                (f2cl-lib:fref temp (3) ((1 3))))
        (setf in ns)
        (setf kt 1)
        (setf i1 0)
       label180
        (setf is 2)
        (setf fni (- fni 1.0))
        (setf dfn (+ fni fnf))
        (setf fn dfn)
        (if (= i1 2) (go label350))
        (setf z (/ x fn))
        (setf ra (f2cl-lib:fsqrt (+ 1.0 (* z z))))
        (setf gln (f2cl-lib:flog (/ (+ 1.0 ra) z)))
        (setf t$ (+ (* ra (- 1.0 etx)) (/ etx (+ z ra))))
        (setf arg (* fn (- t$ gln)))
       label190
        (setf i1 (abs (f2cl-lib:int-sub 3 is)))
        (setf i1 (max (the f2cl-lib:integer4 i1) (the f2cl-lib:integer4 1)))
        (setf flgik 1.0)
        (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
            (dasyik x fn kode flgik ra arg i1
             (f2cl-lib:array-slice temp double-float (is) ((1 3))))
          (declare (ignore var-0 var-1 var-2 var-3 var-6 var-7))
          (setf ra var-4)
          (setf arg var-5))
        (f2cl-lib:computed-goto (label180 label350 label510) is)
       label230
        (setf gln (dlngam fnp1))
        (setf arg (- (* fn xo2l) gln sx))
        (if (< arg (- elim)) (go label300))
        (setf earg (exp arg))
       label240
        (setf s 1.0)
        (if (< x tol) (go label260))
        (setf ak 3.0)
        (setf t2 1.0)
        (setf t$ 1.0)
        (setf s1 fn)
        (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                      ((> k 17) nil)
          (tagbody
            (setf s2 (+ t2 s1))
            (setf t$ (/ (* t$ sxo2) s2))
            (setf s (+ s t$))
            (if (< (abs t$) tol) (go label260))
            (setf t2 (+ t2 ak))
            (setf ak (+ ak 2.0))
            (setf s1 (+ s1 fn))
           label250))
       label260
        (setf (f2cl-lib:fref temp (is) ((1 3))) (* s earg))
        (f2cl-lib:computed-goto (label270 label350 label500) is)
       label270
        (setf earg (/ (* earg fn) xo2))
        (setf fni (- fni 1.0))
        (setf dfn (+ fni fnf))
        (setf fn dfn)
        (setf is 2)
        (go label240)
       label280
        (setf (f2cl-lib:fref y-%data% (nn) ((1 *)) y-%offset%) 0.0)
        (setf nn (f2cl-lib:int-sub nn 1))
        (setf fni (- fni 1.0))
        (setf dfn (+ fni fnf))
        (setf fn dfn)
        (f2cl-lib:arithmetic-if (f2cl-lib:int-sub nn 1)
                                (go label340)
                                (go label290)
                                (go label130))
       label290
        (setf kt 2)
        (setf is 2)
        (go label130)
       label300
        (setf (f2cl-lib:fref y-%data% (nn) ((1 *)) y-%offset%) 0.0)
        (setf nn (f2cl-lib:int-sub nn 1))
        (setf fnp1 fn)
        (setf fni (- fni 1.0))
        (setf dfn (+ fni fnf))
        (setf fn dfn)
        (f2cl-lib:arithmetic-if (f2cl-lib:int-sub nn 1)
                                (go label340)
                                (go label310)
                                (go label320))
       label310
        (setf kt 2)
        (setf is 2)
       label320
        (if (<= sxo2 fnp1) (go label330))
        (go label130)
       label330
        (setf arg (+ (- arg xo2l) (f2cl-lib:flog fnp1)))
        (if (< arg (- elim)) (go label300))
        (go label230)
       label340
        (setf nz (f2cl-lib:int-sub n nn))
        (go end_label)
       label350
        (setf nz (f2cl-lib:int-sub n nn))
       label360
        (if (= kt 2) (go label420))
        (setf s1 (f2cl-lib:fref temp (1) ((1 3))))
        (setf s2 (f2cl-lib:fref temp (2) ((1 3))))
        (setf trx (/ 2.0 x))
        (setf dtm fni)
        (setf tm (* (+ dtm fnf) trx))
        (if (= in 0) (go label390))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i in) nil)
          (tagbody
            (setf s s2)
            (setf s2 (+ (* tm s2) s1))
            (setf s1 s)
            (setf dtm (- dtm 1.0))
            (setf tm (* (+ dtm fnf) trx))
           label380))
        (setf (f2cl-lib:fref y-%data% (nn) ((1 *)) y-%offset%) s1)
        (if (= nn 1) (go end_label))
        (setf (f2cl-lib:fref y-%data%
                             ((f2cl-lib:int-sub nn 1))
                             ((1 *))
                             y-%offset%)
                s2)
        (if (= nn 2) (go end_label))
        (go label400)
       label390
        (setf (f2cl-lib:fref y-%data% (nn) ((1 *)) y-%offset%) s1)
        (setf (f2cl-lib:fref y-%data%
                             ((f2cl-lib:int-sub nn 1))
                             ((1 *))
                             y-%offset%)
                s2)
        (if (= nn 2) (go end_label))
       label400
        (setf k (f2cl-lib:int-add nn 1))
        (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 1))
                      ((> i nn) nil)
          (tagbody
            (setf k (f2cl-lib:int-sub k 1))
            (setf (f2cl-lib:fref y-%data%
                                 ((f2cl-lib:int-sub k 2))
                                 ((1 *))
                                 y-%offset%)
                    (+
                     (* tm
                        (f2cl-lib:fref y-%data%
                                       ((f2cl-lib:int-sub k 1))
                                       ((1 *))
                                       y-%offset%))
                     (f2cl-lib:fref y-%data% (k) ((1 *)) y-%offset%)))
            (setf dtm (- dtm 1.0))
            (setf tm (* (+ dtm fnf) trx))
           label410))
        (go end_label)
       label420
        (setf (f2cl-lib:fref y-%data% (1) ((1 *)) y-%offset%)
                (f2cl-lib:fref temp (2) ((1 3))))
        (go end_label)
       label430
        (setf earg (/ rttpi (f2cl-lib:fsqrt x)))
        (if (= kode 2) (go label440))
        (if (> x elim) (go label610))
        (setf earg (* earg (exp x)))
       label440
        (setf etx (* 8.0 x))
        (setf is kt)
        (setf in 0)
        (setf fn fnu)
       label450
        (setf dx (+ fni fni))
        (setf tm 0.0)
        (if (and (= fni 0.0) (< (abs fnf) tol)) (go label460))
        (setf tm (* 4.0 fnf (+ fni fni fnf)))
       label460
        (setf dtm (* dx dx))
        (setf s1 etx)
        (setf trx (- dtm 1.0))
        (setf dx (/ (- (+ trx tm)) etx))
        (setf t$ dx)
        (setf s (+ 1.0 dx))
        (setf atol (* tol (abs s)))
        (setf s2 1.0)
        (setf ak 8.0)
        (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                      ((> k 25) nil)
          (tagbody
            (setf s1 (+ s1 etx))
            (setf s2 (+ s2 ak))
            (setf dx (- dtm s2))
            (setf ap (+ dx tm))
            (setf t$ (/ (* (- t$) ap) s1))
            (setf s (+ s t$))
            (if (<= (abs t$) atol) (go label480))
            (setf ak (+ ak 8.0))
           label470))
       label480
        (setf (f2cl-lib:fref temp (is) ((1 3))) (* s earg))
        (if (= is 2) (go label360))
        (setf is 2)
        (setf fni (- fni 1.0))
        (setf dfn (+ fni fnf))
        (setf fn dfn)
        (go label450)
       label500
        (setf akm (max (- 3.0 fn) 0.0))
        (setf km (f2cl-lib:int akm))
        (setf tfn (+ fn km))
        (setf ta
                (/ (+ (- (+ gln tfn) 0.9189385332) (/ -0.0833333333 tfn))
                   (+ tfn 0.5)))
        (setf ta (- xo2l ta))
        (setf tb (/ (- (+ 1.0 (/ (* -1 1.0) tfn))) tfn))
        (setf ain
                (+ (/ tolln (- (f2cl-lib:fsqrt (- (* ta ta) (* tolln tb))) ta))
                   1.5))
        (setf in (f2cl-lib:int ain))
        (setf in (f2cl-lib:int-add in km))
        (go label520)
       label510
        (setf t$ (/ 1.0 (* fn ra)))
        (setf ain
                (+
                 (/ tolln
                    (+ gln (f2cl-lib:fsqrt (+ (* gln gln) (* t$ tolln)))))
                 1.5))
        (setf in (f2cl-lib:int ain))
        (if (> in inlim) (go label160))
       label520
        (setf trx (/ 2.0 x))
        (setf dtm (+ fni in))
        (setf tm (* (+ dtm fnf) trx))
        (setf ta 0.0)
        (setf tb tol)
        (setf kk 1)
       label530
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i in) nil)
          (tagbody
            (setf s tb)
            (setf tb (+ (* tm tb) ta))
            (setf ta s)
            (setf dtm (- dtm 1.0))
            (setf tm (* (+ dtm fnf) trx))
           label540))
        (if (/= kk 1) (go label550))
        (setf ta (* (/ ta tb) (f2cl-lib:fref temp (3) ((1 3)))))
        (setf tb (f2cl-lib:fref temp (3) ((1 3))))
        (setf kk 2)
        (setf in ns)
        (if (/= ns 0) (go label530))
       label550
        (setf (f2cl-lib:fref y-%data% (nn) ((1 *)) y-%offset%) tb)
        (setf nz (f2cl-lib:int-sub n nn))
        (if (= nn 1) (go end_label))
        (setf tb (+ (* tm tb) ta))
        (setf k (f2cl-lib:int-sub nn 1))
        (setf (f2cl-lib:fref y-%data% (k) ((1 *)) y-%offset%) tb)
        (if (= nn 2) (go end_label))
        (setf dtm (- dtm 1.0))
        (setf tm (* (+ dtm fnf) trx))
        (setf km (f2cl-lib:int-sub k 1))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i km) nil)
          (tagbody
            (setf (f2cl-lib:fref y-%data%
                                 ((f2cl-lib:int-sub k 1))
                                 ((1 *))
                                 y-%offset%)
                    (+ (* tm (f2cl-lib:fref y-%data% (k) ((1 *)) y-%offset%))
                       (f2cl-lib:fref y-%data%
                                      ((f2cl-lib:int-add k 1))
                                      ((1 *))
                                      y-%offset%)))
            (setf dtm (- dtm 1.0))
            (setf tm (* (+ dtm fnf) trx))
            (setf k (f2cl-lib:int-sub k 1))
           label560))
        (go end_label)
       label570
        (xermsg "SLATEC" "DBESI" "SCALING OPTION, KODE, NOT 1 OR 2." 2 1)
        (go end_label)
       label580
        (xermsg "SLATEC" "DBESI" "ORDER, ALPHA, LESS THAN ZERO." 2 1)
        (go end_label)
       label590
        (xermsg "SLATEC" "DBESI" "N LESS THAN ONE." 2 1)
        (go end_label)
       label600
        (xermsg "SLATEC" "DBESI" "X LESS THAN ZERO." 2 1)
        (go end_label)
       label610
        (xermsg "SLATEC" "DBESI" "OVERFLOW, X TOO LARGE FOR KODE = 1." 6 1)
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nz))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dbesi fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil fortran-to-lisp::nz)
           :calls '(fortran-to-lisp::xermsg fortran-to-lisp::dlngam
                    fortran-to-lisp::dasyik fortran-to-lisp::i1mach
                    fortran-to-lisp::d1mach))))

