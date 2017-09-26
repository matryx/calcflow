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
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((rtwo 1.34839972492648)
      (pdf 0.785398163397448)
      (rttp 0.797884560802865)
      (pidt 1.5707963267949)
      (pp
       (make-array 4
                   :element-type 'double-float
                   :initial-contents '(8.72909153935547 0.26569393226503
                                       0.124578576865586 7.70133747430388e-4)))
      (inlim 150)
      (fnulim
       (make-array 2
                   :element-type 'double-float
                   :initial-contents '(100.0 60.0))))
  (declare (type (double-float) rtwo pdf rttp pidt)
           (type (simple-array double-float (4)) pp)
           (type (f2cl-lib:integer4) inlim)
           (type (simple-array double-float (2)) fnulim))
  (defun dbesj (x alpha n y nz)
    (declare (type (array double-float (*)) y)
             (type (f2cl-lib:integer4) nz n)
             (type (double-float) alpha x))
    (prog ((temp (make-array 3 :element-type 'double-float))
           (wk (make-array 7 :element-type 'double-float)) (ak 0.0) (akm 0.0)
           (ans 0.0) (ap 0.0) (arg 0.0) (coef 0.0) (dalpha 0.0) (dfn 0.0)
           (dtm 0.0) (earg 0.0) (elim1 0.0) (etx 0.0) (fidal 0.0) (flgjy 0.0)
           (fn 0.0) (fnf 0.0) (fni 0.0) (fnp1 0.0) (fnu 0.0) (gln 0.0)
           (rden 0.0) (relb 0.0) (rtx 0.0) (rzden 0.0) (s 0.0) (sa 0.0)
           (sb 0.0) (sxo2 0.0) (s1 0.0) (s2 0.0) (t$ 0.0) (ta 0.0) (tau 0.0)
           (tb 0.0) (tfn 0.0) (tm 0.0) (tol 0.0) (tolln 0.0) (trx 0.0) (tx 0.0)
           (t1 0.0) (t2 0.0) (xo2 0.0) (xo2l 0.0) (slim 0.0) (rtol 0.0) (i 0)
           (ialp 0) (idalp 0) (iflw 0) (in 0) (is 0) (i1 0) (i2 0) (k 0) (kk 0)
           (km 0) (kt 0) (nn 0) (ns 0))
      (declare (type (f2cl-lib:integer4) ns nn kt km kk k i2 i1 is in iflw
                                         idalp ialp i)
               (type (simple-array double-float (7)) wk)
               (type (simple-array double-float (3)) temp)
               (type (double-float) rtol slim xo2l xo2 t2 t1 tx trx tolln tol
                                    tm tfn tb tau ta t$ s2 s1 sxo2 sb sa s
                                    rzden rtx relb rden gln fnu fnp1 fni fnf fn
                                    flgjy fidal etx elim1 earg dtm dfn dalpha
                                    coef arg ap ans akm ak))
      (setf nz 0)
      (setf kt 1)
      (setf ns 0)
      (setf ta (f2cl-lib:d1mach 3))
      (setf tol (max ta 1.0e-15))
      (setf i1 (f2cl-lib:int-add (f2cl-lib:i1mach 14) 1))
      (setf i2 (f2cl-lib:i1mach 15))
      (setf tb (f2cl-lib:d1mach 5))
      (setf elim1 (* -2.303 (+ (* i2 tb) 3.0)))
      (setf rtol (/ 1.0 tol))
      (setf slim (* (f2cl-lib:d1mach 1) rtol 1000.0))
      (setf tolln (* 2.303 tb i1))
      (setf tolln (min tolln 34.5388))
      (f2cl-lib:arithmetic-if (f2cl-lib:int-sub n 1)
                              (go label720)
                              (go label10)
                              (go label20))
     label10
      (setf kt 2)
     label20
      (setf nn n)
      (f2cl-lib:arithmetic-if x (go label730) (go label30) (go label80))
     label30
      (f2cl-lib:arithmetic-if alpha (go label710) (go label40) (go label50))
     label40
      (setf (f2cl-lib:fref y (1) ((1 *))) 1.0)
      (if (= n 1) (go end_label))
      (setf i1 2)
      (go label60)
     label50
      (setf i1 1)
     label60
      (f2cl-lib:fdo (i i1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody (setf (f2cl-lib:fref y (i) ((1 *))) 0.0) label70))
      (go end_label)
     label80
      (if (< alpha 0.0) (go label710))
      (setf ialp (f2cl-lib:int alpha))
      (setf fni
              (coerce
               (the f2cl-lib:integer4
                    (f2cl-lib:int-sub (f2cl-lib:int-add ialp n) 1))
               'double-float))
      (setf fnf (- alpha ialp))
      (setf dfn (+ fni fnf))
      (setf fnu dfn)
      (setf xo2 (* x 0.5))
      (setf sxo2 (* xo2 xo2))
      (if (<= sxo2 (+ fnu 1.0)) (go label90))
      (setf ta (max 20.0 fnu))
      (if (> x ta) (go label120))
      (if (> x 12.0) (go label110))
      (setf xo2l (f2cl-lib:flog xo2))
      (setf ns (f2cl-lib:int-add (f2cl-lib:int (- sxo2 fnu)) 1))
      (go label100)
     label90
      (setf fn fnu)
      (setf fnp1 (+ fn 1.0))
      (setf xo2l (f2cl-lib:flog xo2))
      (setf is kt)
      (if (<= x 0.5) (go label330))
      (setf ns 0)
     label100
      (setf fni (+ fni ns))
      (setf dfn (+ fni fnf))
      (setf fn dfn)
      (setf fnp1 (+ fn 1.0))
      (setf is kt)
      (if (> (f2cl-lib:int-add (f2cl-lib:int-sub n 1) ns) 0) (setf is 3))
      (go label330)
     label110
      (setf ans (max (- 36.0 fnu) 0.0))
      (setf ns (f2cl-lib:int ans))
      (setf fni (+ fni ns))
      (setf dfn (+ fni fnf))
      (setf fn dfn)
      (setf is kt)
      (if (> (f2cl-lib:int-add (f2cl-lib:int-sub n 1) ns) 0) (setf is 3))
      (go label130)
     label120
      (setf rtx (f2cl-lib:fsqrt x))
      (setf tau (* rtwo rtx))
      (setf ta (+ tau (f2cl-lib:fref fnulim (kt) ((1 2)))))
      (if (<= fnu ta) (go label480))
      (setf fn fnu)
      (setf is kt)
     label130
      (setf i1 (abs (f2cl-lib:int-sub 3 is)))
      (setf i1 (max (the f2cl-lib:integer4 i1) (the f2cl-lib:integer4 1)))
      (setf flgjy 1.0)
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
          (dasyjy #'djairy x fn flgjy i1
           (f2cl-lib:array-slice temp double-float (is) ((1 3))) wk iflw)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
        (setf iflw var-7))
      (if (/= iflw 0) (go label380))
      (f2cl-lib:computed-goto (label320 label450 label620) is)
     label310
      (setf (f2cl-lib:fref temp (1) ((1 3))) (f2cl-lib:fref temp (3) ((1 3))))
      (setf kt 1)
     label320
      (setf is 2)
      (setf fni (- fni 1.0))
      (setf dfn (+ fni fnf))
      (setf fn dfn)
      (if (= i1 2) (go label450))
      (go label130)
     label330
      (setf gln (dlngam fnp1))
      (setf arg (- (* fn xo2l) gln))
      (if (< arg (- elim1)) (go label400))
      (setf earg (exp arg))
     label340
      (setf s 1.0)
      (if (< x tol) (go label360))
      (setf ak 3.0)
      (setf t2 1.0)
      (setf t$ 1.0)
      (setf s1 fn)
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k 17) nil)
        (tagbody
          (setf s2 (+ t2 s1))
          (setf t$ (/ (* (- t$) sxo2) s2))
          (setf s (+ s t$))
          (if (< (abs t$) tol) (go label360))
          (setf t2 (+ t2 ak))
          (setf ak (+ ak 2.0))
          (setf s1 (+ s1 fn))
         label350))
     label360
      (setf (f2cl-lib:fref temp (is) ((1 3))) (* s earg))
      (f2cl-lib:computed-goto (label370 label450 label610) is)
     label370
      (setf earg (/ (* earg fn) xo2))
      (setf fni (- fni 1.0))
      (setf dfn (+ fni fnf))
      (setf fn dfn)
      (setf is 2)
      (go label340)
     label380
      (setf (f2cl-lib:fref y (nn) ((1 *))) 0.0)
      (setf nn (f2cl-lib:int-sub nn 1))
      (setf fni (- fni 1.0))
      (setf dfn (+ fni fnf))
      (setf fn dfn)
      (f2cl-lib:arithmetic-if (f2cl-lib:int-sub nn 1)
                              (go label440)
                              (go label390)
                              (go label130))
     label390
      (setf kt 2)
      (setf is 2)
      (go label130)
     label400
      (setf (f2cl-lib:fref y (nn) ((1 *))) 0.0)
      (setf nn (f2cl-lib:int-sub nn 1))
      (setf fnp1 fn)
      (setf fni (- fni 1.0))
      (setf dfn (+ fni fnf))
      (setf fn dfn)
      (f2cl-lib:arithmetic-if (f2cl-lib:int-sub nn 1)
                              (go label440)
                              (go label410)
                              (go label420))
     label410
      (setf kt 2)
      (setf is 2)
     label420
      (if (<= sxo2 fnp1) (go label430))
      (go label130)
     label430
      (setf arg (+ (- arg xo2l) (f2cl-lib:flog fnp1)))
      (if (< arg (- elim1)) (go label400))
      (go label330)
     label440
      (setf nz (f2cl-lib:int-sub n nn))
      (go end_label)
     label450
      (if (/= ns 0) (go label451))
      (setf nz (f2cl-lib:int-sub n nn))
      (if (= kt 2) (go label470))
      (setf (f2cl-lib:fref y (nn) ((1 *))) (f2cl-lib:fref temp (1) ((1 3))))
      (setf (f2cl-lib:fref y ((f2cl-lib:int-sub nn 1)) ((1 *)))
              (f2cl-lib:fref temp (2) ((1 3))))
      (if (= nn 2) (go end_label))
     label451
      (setf trx (/ 2.0 x))
      (setf dtm fni)
      (setf tm (* (+ dtm fnf) trx))
      (setf ak 1.0)
      (setf ta (f2cl-lib:fref temp (1) ((1 3))))
      (setf tb (f2cl-lib:fref temp (2) ((1 3))))
      (if (> (abs ta) slim) (go label455))
      (setf ta (* ta rtol))
      (setf tb (* tb rtol))
      (setf ak tol)
     label455
      (setf kk 2)
      (setf in (f2cl-lib:int-sub ns 1))
      (if (= in 0) (go label690))
      (if (/= ns 0) (go label670))
      (setf k (f2cl-lib:int-sub nn 2))
      (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 1))
                    ((> i nn) nil)
        (tagbody
          (setf s tb)
          (setf tb (- (* tm tb) ta))
          (setf ta s)
          (setf (f2cl-lib:fref y (k) ((1 *))) (* tb ak))
          (setf dtm (- dtm 1.0))
          (setf tm (* (+ dtm fnf) trx))
          (setf k (f2cl-lib:int-sub k 1))
         label460))
      (go end_label)
     label470
      (setf (f2cl-lib:fref y (1) ((1 *))) (f2cl-lib:fref temp (2) ((1 3))))
      (go end_label)
     label480
      (setf in (f2cl-lib:int (+ (- alpha tau) 2.0)))
      (if (<= in 0) (go label490))
      (setf idalp (f2cl-lib:int-sub ialp in 1))
      (setf kt 1)
      (go label500)
     label490
      (setf idalp ialp)
      (setf in 0)
     label500
      (setf is kt)
      (setf fidal (coerce (the f2cl-lib:integer4 idalp) 'double-float))
      (setf dalpha (+ fidal fnf))
      (setf arg (- x (* pidt dalpha) pdf))
      (setf sa (sin arg))
      (setf sb (cos arg))
      (setf coef (/ rttp rtx))
      (setf etx (* 8.0 x))
     label510
      (setf dtm (+ fidal fidal))
      (setf dtm (* dtm dtm))
      (setf tm 0.0)
      (if (and (= fidal 0.0) (< (abs fnf) tol)) (go label520))
      (setf tm (* 4.0 fnf (+ fidal fidal fnf)))
     label520
      (setf trx (- dtm 1.0))
      (setf t2 (/ (+ trx tm) etx))
      (setf s2 t2)
      (setf relb (* tol (abs t2)))
      (setf t1 etx)
      (setf s1 1.0)
      (setf fn 1.0)
      (setf ak 8.0)
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k 13) nil)
        (tagbody
          (setf t1 (+ t1 etx))
          (setf fn (+ fn ak))
          (setf trx (- dtm fn))
          (setf ap (+ trx tm))
          (setf t2 (/ (* (- t2) ap) t1))
          (setf s1 (+ s1 t2))
          (setf t1 (+ t1 etx))
          (setf ak (+ ak 8.0))
          (setf fn (+ fn ak))
          (setf trx (- dtm fn))
          (setf ap (+ trx tm))
          (setf t2 (/ (* t2 ap) t1))
          (setf s2 (+ s2 t2))
          (if (<= (abs t2) relb) (go label540))
          (setf ak (+ ak 8.0))
         label530))
     label540
      (setf (f2cl-lib:fref temp (is) ((1 3))) (* coef (- (* s1 sb) (* s2 sa))))
      (if (= is 2) (go label560))
      (setf fidal (+ fidal 1.0))
      (setf dalpha (+ fidal fnf))
      (setf is 2)
      (setf tb sa)
      (setf sa (- sb))
      (setf sb tb)
      (go label510)
     label560
      (if (= kt 2) (go label470))
      (setf s1 (f2cl-lib:fref temp (1) ((1 3))))
      (setf s2 (f2cl-lib:fref temp (2) ((1 3))))
      (setf tx (/ 2.0 x))
      (setf tm (* dalpha tx))
      (if (= in 0) (go label580))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i in) nil)
        (tagbody
          (setf s s2)
          (setf s2 (- (* tm s2) s1))
          (setf tm (+ tm tx))
          (setf s1 s)
         label570))
      (if (= nn 1) (go label600))
      (setf s s2)
      (setf s2 (- (* tm s2) s1))
      (setf tm (+ tm tx))
      (setf s1 s)
     label580
      (setf (f2cl-lib:fref y (1) ((1 *))) s1)
      (setf (f2cl-lib:fref y (2) ((1 *))) s2)
      (if (= nn 2) (go end_label))
      (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 1))
                    ((> i nn) nil)
        (tagbody
          (setf (f2cl-lib:fref y (i) ((1 *)))
                  (- (* tm (f2cl-lib:fref y ((f2cl-lib:int-sub i 1)) ((1 *))))
                     (f2cl-lib:fref y ((f2cl-lib:int-sub i 2)) ((1 *)))))
          (setf tm (+ tm tx))
         label590))
      (go end_label)
     label600
      (setf (f2cl-lib:fref y (1) ((1 *))) s2)
      (go end_label)
     label610
      (setf akm (max (- 3.0 fn) 0.0))
      (setf km (f2cl-lib:int akm))
      (setf tfn (+ fn km))
      (setf ta
              (/ (+ (- (+ gln tfn) 0.9189385332) (/ -0.0833333333 tfn))
                 (+ tfn 0.5)))
      (setf ta (- xo2l ta))
      (setf tb (/ (- (+ 1.0 (/ (* -1 1.5) tfn))) tfn))
      (setf akm
              (+ (/ tolln (- (f2cl-lib:fsqrt (- (* ta ta) (* tolln tb))) ta))
                 1.5))
      (setf in (f2cl-lib:int-add km (f2cl-lib:int akm)))
      (go label660)
     label620
      (setf gln
              (+ (f2cl-lib:fref wk (3) ((1 7)))
                 (f2cl-lib:fref wk (2) ((1 7)))))
      (if (> (f2cl-lib:fref wk (6) ((1 7))) 30.0) (go label640))
      (setf rden
              (+
               (*
                (+
                 (* (f2cl-lib:fref pp (4) ((1 4)))
                    (f2cl-lib:fref wk (6) ((1 7))))
                 (f2cl-lib:fref pp (3) ((1 4))))
                (f2cl-lib:fref wk (6) ((1 7))))
               1.0))
      (setf rzden
              (+ (f2cl-lib:fref pp (1) ((1 4)))
                 (* (f2cl-lib:fref pp (2) ((1 4)))
                    (f2cl-lib:fref wk (6) ((1 7))))))
      (setf ta (/ rzden rden))
      (if (< (f2cl-lib:fref wk (1) ((1 7))) 0.1) (go label630))
      (setf tb (/ gln (f2cl-lib:fref wk (5) ((1 7)))))
      (go label650)
     label630
      (setf tb
              (/
               (+ 1.259921049
                  (*
                   (+ 0.167989473
                      (* 0.0887944358 (f2cl-lib:fref wk (1) ((1 7)))))
                   (f2cl-lib:fref wk (1) ((1 7)))))
               (f2cl-lib:fref wk (7) ((1 7)))))
      (go label650)
     label640
      (setf ta (/ (* 0.5 tolln) (f2cl-lib:fref wk (4) ((1 7)))))
      (setf ta
              (* (+ (* (- (* 0.049382716 ta) 0.1111111111) ta) 0.6666666667)
                 ta
                 (f2cl-lib:fref wk (6) ((1 7)))))
      (if (< (f2cl-lib:fref wk (1) ((1 7))) 0.1) (go label630))
      (setf tb (/ gln (f2cl-lib:fref wk (5) ((1 7)))))
     label650
      (setf in (f2cl-lib:int (+ (/ ta tb) 1.5)))
      (if (> in inlim) (go label310))
     label660
      (setf dtm (+ fni in))
      (setf trx (/ 2.0 x))
      (setf tm (* (+ dtm fnf) trx))
      (setf ta 0.0)
      (setf tb tol)
      (setf kk 1)
      (setf ak 1.0)
     label670
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i in) nil)
        (tagbody
          (setf s tb)
          (setf tb (- (* tm tb) ta))
          (setf ta s)
          (setf dtm (- dtm 1.0))
          (setf tm (* (+ dtm fnf) trx))
         label680))
      (if (/= kk 1) (go label690))
      (setf s (f2cl-lib:fref temp (3) ((1 3))))
      (setf sa (/ ta tb))
      (setf ta s)
      (setf tb s)
      (if (> (abs s) slim) (go label685))
      (setf ta (* ta rtol))
      (setf tb (* tb rtol))
      (setf ak tol)
     label685
      (setf ta (* ta sa))
      (setf kk 2)
      (setf in ns)
      (if (/= ns 0) (go label670))
     label690
      (setf (f2cl-lib:fref y (nn) ((1 *))) (* tb ak))
      (setf nz (f2cl-lib:int-sub n nn))
      (if (= nn 1) (go end_label))
      (setf k (f2cl-lib:int-sub nn 1))
      (setf s tb)
      (setf tb (- (* tm tb) ta))
      (setf ta s)
      (setf (f2cl-lib:fref y (k) ((1 *))) (* tb ak))
      (if (= nn 2) (go end_label))
      (setf dtm (- dtm 1.0))
      (setf tm (* (+ dtm fnf) trx))
      (setf k (f2cl-lib:int-sub nn 2))
      (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 1))
                    ((> i nn) nil)
        (tagbody
          (setf s tb)
          (setf tb (- (* tm tb) ta))
          (setf ta s)
          (setf (f2cl-lib:fref y (k) ((1 *))) (* tb ak))
          (setf dtm (- dtm 1.0))
          (setf tm (* (+ dtm fnf) trx))
          (setf k (f2cl-lib:int-sub k 1))
         label700))
      (go end_label)
     label710
      (xermsg "SLATEC" "DBESJ" "ORDER, ALPHA, LESS THAN ZERO." 2 1)
      (go end_label)
     label720
      (xermsg "SLATEC" "DBESJ" "N LESS THAN ONE." 2 1)
      (go end_label)
     label730
      (xermsg "SLATEC" "DBESJ" "X LESS THAN ZERO." 2 1)
      (go end_label)
     end_label
      (return (values nil nil nil nil nz)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dbesj fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil fortran-to-lisp::nz)
           :calls '(fortran-to-lisp::xermsg fortran-to-lisp::dlngam
                    fortran-to-lisp::dasyjy fortran-to-lisp::i1mach
                    fortran-to-lisp::d1mach))))

