;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.221 2010/05/26 19:25:52 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.204 2010/02/23 05:21:30 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.114 2010/05/17 01:42:14 rtoy Exp $")

;;; Using Lisp CMU Common Lisp CVS Head 2010-05-25 18:21:07 (20A Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :colnew)


(defun contrl
       (xi xiold z dmz rhs delz deldmz dqz dqdmz g w v valstr slope scale
        dscale accum ipvtg integs ipvtw nfxpnt fixpnt iflag fsub dfsub gsub
        dgsub guess)
  (declare (type (f2cl-lib:integer4) iflag nfxpnt)
           (type (array f2cl-lib:integer4 (*)) ipvtw integs ipvtg)
           (type (array double-float (*)) fixpnt accum dscale scale slope
                                          valstr v w g dqdmz dqz deldmz delz
                                          rhs dmz z xiold xi))
  (let ((colest-tolin
         (make-array 40
                     :element-type 'double-float
                     :displaced-to (colest-part-0 *colest-common-block*)
                     :displaced-index-offset 120))
        (colest-ltol
         (make-array 40
                     :element-type 'f2cl-lib:integer4
                     :displaced-to (colest-part-1 *colest-common-block*)
                     :displaced-index-offset 40)))
    (symbol-macrolet ((precis (aref (colout-part-0 *colout-common-block*) 0))
                      (iout (aref (colout-part-1 *colout-common-block*) 0))
                      (iprint (aref (colout-part-1 *colout-common-block*) 1))
                      (mstar (aref (colord-part-0 *colord-common-block*) 2))
                      (kd (aref (colord-part-0 *colord-common-block*) 3))
                      (n (aref (colapr-part-0 *colapr-common-block*) 0))
                      (nold (aref (colapr-part-0 *colapr-common-block*) 1))
                      (nmax (aref (colapr-part-0 *colapr-common-block*) 2))
                      (nz (aref (colapr-part-0 *colapr-common-block*) 3))
                      (ndmz (aref (colapr-part-0 *colapr-common-block*) 4))
                      (mshnum (aref (colmsh-part-0 *colmsh-common-block*) 1))
                      (mshlmt (aref (colmsh-part-0 *colmsh-common-block*) 2))
                      (mshalt (aref (colmsh-part-0 *colmsh-common-block*) 3))
                      (nonlin (aref (colnln-part-0 *colnln-common-block*) 0))
                      (iter (aref (colnln-part-0 *colnln-common-block*) 1))
                      (limit (aref (colnln-part-0 *colnln-common-block*) 2))
                      (icare (aref (colnln-part-0 *colnln-common-block*) 3))
                      (iguess (aref (colnln-part-0 *colnln-common-block*) 4))
                      (tolin colest-tolin)
                      (ltol colest-ltol)
                      (ntol (aref (colest-part-1 *colest-common-block*) 80)))
      (f2cl-lib:with-multi-array-data
          ((xi double-float xi-%data% xi-%offset%)
           (xiold double-float xiold-%data% xiold-%offset%)
           (z double-float z-%data% z-%offset%)
           (dmz double-float dmz-%data% dmz-%offset%)
           (rhs double-float rhs-%data% rhs-%offset%)
           (delz double-float delz-%data% delz-%offset%)
           (deldmz double-float deldmz-%data% deldmz-%offset%)
           (dqz double-float dqz-%data% dqz-%offset%)
           (dqdmz double-float dqdmz-%data% dqdmz-%offset%)
           (g double-float g-%data% g-%offset%)
           (w double-float w-%data% w-%offset%)
           (v double-float v-%data% v-%offset%)
           (valstr double-float valstr-%data% valstr-%offset%)
           (slope double-float slope-%data% slope-%offset%)
           (scale double-float scale-%data% scale-%offset%)
           (dscale double-float dscale-%data% dscale-%offset%)
           (accum double-float accum-%data% accum-%offset%)
           (fixpnt double-float fixpnt-%data% fixpnt-%offset%)
           (ipvtg f2cl-lib:integer4 ipvtg-%data% ipvtg-%offset%)
           (integs f2cl-lib:integer4 integs-%data% integs-%offset%)
           (ipvtw f2cl-lib:integer4 ipvtw-%data% ipvtw-%offset%))
        (prog ((ifin 0) (lj 0) (j 0) (fact 0.0) (factor 0.0) (arg 0.0)
               (anfix 0.0) (anorm 0.0) (ipred 0) (rlxold 0.0) (andif 0.0)
               (anscl 0.0) (np1 0) (iz 0) (inz 0) (it 0) (ifrz 0) (rnold 0.0)
               (ifreez 0) (relax 0.0) (rnorm 0.0) (msing 0) (noconv 0) (icor 0)
               (iconv 0) (imesh 0) (i 0) (check 0.0) (lmtfrz 0) (rstart 0.0)
               (relmin 0.0) (dummy (make-array 1 :element-type 'double-float)))
          (declare (type (array double-float (1)) dummy)
                   (type double-float relmin rstart check rnorm relax rnold
                                      anscl andif rlxold anorm anfix arg factor
                                      fact)
                   (type (f2cl-lib:integer4) lmtfrz i imesh iconv icor noconv
                                             msing ifreez ifrz it inz iz np1
                                             ipred j lj ifin))
          (setf relmin 0.001)
          (setf rstart 0.01)
          (setf lmtfrz 4)
          (setf check 0.0)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ntol) nil)
            (tagbody
             label10
              (setf check
                      (f2cl-lib:dmax1 (f2cl-lib:fref tolin (i) ((1 40)))
                                      check))))
          (setf imesh 1)
          (setf iconv 0)
          (if (= nonlin 0) (setf iconv 1))
          (setf icor 0)
          (setf noconv 0)
          (setf msing 0)
         label20
          (setf iter 0)
          (if (> nonlin 0) (go label50))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
                 var-19 var-20 var-21)
              (lsyslv msing xi xiold dummy dummy z dmz g w v rhs dummy integs
               ipvtg ipvtw rnorm 0 fsub dfsub gsub dgsub guess)
            (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                             var-9 var-10 var-11 var-12 var-13 var-14 var-16
                             var-17 var-18 var-19 var-20 var-21))
            (setf msing var-0)
            (setf rnorm var-15))
          (if (= msing 0) (go label400))
         label30
          (if (< msing 0) (go label40))
          (if (< iprint 1)
              (f2cl-lib:fformat iout
                                ("~%" "~%"
                                 " A LOCAL ELIMINATION MATRIX IS SINGULAR "
                                 "~%")))
          (go label460)
         label40
          (if (< iprint 1)
              (f2cl-lib:fformat iout
                                ("~%" "~%"
                                 " THE GLOBAL BVP-MATRIX IS SINGULAR " "~%")))
          (setf iflag 0)
          (go end_label)
         label50
          (setf relax 1.0)
          (if (or (= icare 1) (= icare -1)) (setf relax rstart))
          (if (= iconv 0) (go label160))
          (setf ifreez 0)
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
                 var-19 var-20 var-21)
              (lsyslv msing xi xiold z dmz delz deldmz g w v rhs dqdmz integs
               ipvtg ipvtw rnold 1 fsub dfsub gsub dgsub guess)
            (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                             var-9 var-10 var-11 var-12 var-13 var-14 var-16
                             var-17 var-18 var-19 var-20 var-21))
            (setf msing var-0)
            (setf rnold var-15))
          (if (< iprint 0)
              (f2cl-lib:fformat iout
                                ("~%" " FIXED JACOBIAN ITERATIONS," "~%")))
          (if (< iprint 0)
              (f2cl-lib:fformat iout
                                (" ITERATION = " 1 (("~3D")) "  NORM (RHS) = "
                                 1 (("~10,2,2,0,'*,,'DE")) "~%")
                                iter
                                rnold))
          (go label70)
         label60
          (if (< iprint 0)
              (f2cl-lib:fformat iout
                                (" ITERATION = " 1 (("~3D")) "  NORM (RHS) = "
                                 1 (("~10,2,2,0,'*,,'DE")) "~%")
                                iter
                                rnorm))
          (setf rnold rnorm)
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
                 var-19 var-20 var-21)
              (lsyslv msing xi xiold z dmz delz deldmz g w v rhs dummy integs
               ipvtg ipvtw rnorm (f2cl-lib:int-add 3 ifreez) fsub dfsub gsub
               dgsub guess)
            (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                             var-9 var-10 var-11 var-12 var-13 var-14 var-16
                             var-17 var-18 var-19 var-20 var-21))
            (setf msing var-0)
            (setf rnorm var-15))
         label70
          (if (/= msing 0) (go label30))
          (if (= ifreez 1) (go label80))
          (setf iter (f2cl-lib:int-add iter 1))
          (setf ifrz 0)
         label80
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i nz) nil)
            (tagbody
              (setf (f2cl-lib:fref z-%data% (i) ((1 1)) z-%offset%)
                      (+ (f2cl-lib:fref z-%data% (i) ((1 1)) z-%offset%)
                         (f2cl-lib:fref delz-%data%
                                        (i)
                                        ((1 1))
                                        delz-%offset%)))
             label90))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ndmz) nil)
            (tagbody
              (setf (f2cl-lib:fref dmz-%data% (i) ((1 1)) dmz-%offset%)
                      (+ (f2cl-lib:fref dmz-%data% (i) ((1 1)) dmz-%offset%)
                         (f2cl-lib:fref deldmz-%data%
                                        (i)
                                        ((1 1))
                                        deldmz-%offset%)))
             label100))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
                 var-19 var-20 var-21)
              (lsyslv msing xi xiold z dmz delz deldmz g w v rhs dummy integs
               ipvtg ipvtw rnorm 2 fsub dfsub gsub dgsub guess)
            (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                             var-9 var-10 var-11 var-12 var-13 var-14 var-16
                             var-17 var-18 var-19 var-20 var-21))
            (setf msing var-0)
            (setf rnorm var-15))
          (if (< rnorm precis) (go label390))
          (if (> rnorm rnold) (go label130))
          (if (= ifreez 1) (go label110))
          (setf ifreez 1)
          (go label60)
         label110
          (setf ifrz (f2cl-lib:int-add ifrz 1))
          (if (>= ifrz lmtfrz) (setf ifreez 0))
          (if (< rnold (* 4.0 rnorm)) (setf ifreez 0))
          (f2cl-lib:fdo (it 1 (f2cl-lib:int-add it 1))
                        ((> it ntol) nil)
            (tagbody
              (setf inz (f2cl-lib:fref ltol (it) ((1 40))))
              (f2cl-lib:fdo (iz inz (f2cl-lib:int-add iz mstar))
                            ((> iz nz) nil)
                (tagbody
                  (if
                   (>
                    (f2cl-lib:dabs
                     (f2cl-lib:fref delz-%data% (iz) ((1 1)) delz-%offset%))
                    (* (f2cl-lib:fref tolin (it) ((1 40)))
                       (+
                        (f2cl-lib:dabs
                         (f2cl-lib:fref z-%data% (iz) ((1 1)) z-%offset%))
                        1.0)))
                   (go label60))
                 label120))))
         label120
          (if (< iprint 1)
              (f2cl-lib:fformat iout
                                ("~%" " CONVERGENCE AFTER" 1 (("~3D"))
                                 " ITERATIONS" "~%" "~%")
                                iter))
          (go label400)
         label130
          (if (< iprint 0)
              (f2cl-lib:fformat iout
                                (" ITERATION = " 1 (("~3D")) "  NORM (RHS) = "
                                 1 (("~10,2,2,0,'*,,'DE")) "~%")
                                iter
                                rnorm))
          (if (< iprint 0)
              (f2cl-lib:fformat iout
                                ("~%" " SWITCH TO DAMPED NEWTON ITERATION,"
                                 "~%")))
          (setf iconv 0)
          (setf relax rstart)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i nz) nil)
            (tagbody
              (setf (f2cl-lib:fref z-%data% (i) ((1 1)) z-%offset%)
                      (- (f2cl-lib:fref z-%data% (i) ((1 1)) z-%offset%)
                         (f2cl-lib:fref delz-%data%
                                        (i)
                                        ((1 1))
                                        delz-%offset%)))
             label140))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ndmz) nil)
            (tagbody
              (setf (f2cl-lib:fref dmz-%data% (i) ((1 1)) dmz-%offset%)
                      (- (f2cl-lib:fref dmz-%data% (i) ((1 1)) dmz-%offset%)
                         (f2cl-lib:fref deldmz-%data%
                                        (i)
                                        ((1 1))
                                        deldmz-%offset%)))
             label150))
          (setf np1 (f2cl-lib:int-add n 1))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i np1) nil)
            (tagbody
             label155
              (setf (f2cl-lib:fref xiold-%data% (i) ((1 1)) xiold-%offset%)
                      (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%))))
          (setf nold n)
          (setf iter 0)
         label160
          (if (< iprint 0)
              (f2cl-lib:fformat iout
                                ("~%" " FULL DAMPED NEWTON ITERATION," "~%")))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
                 var-19 var-20 var-21)
              (lsyslv msing xi xiold z dmz delz deldmz g w v rhs dqdmz integs
               ipvtg ipvtw rnold 1 fsub dfsub gsub dgsub guess)
            (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                             var-9 var-10 var-11 var-12 var-13 var-14 var-16
                             var-17 var-18 var-19 var-20 var-21))
            (setf msing var-0)
            (setf rnold var-15))
          (if (/= msing 0) (go label30))
          (if (= iguess 1) (setf iguess 0))
          (skale n mstar kd z xi scale dscale)
          (go label220)
         label170
          (setf rnold rnorm)
          (if (>= iter limit) (go label430))
          (skale n mstar kd z xi scale dscale)
          (setf anscl 0.0)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i nz) nil)
            (tagbody
              (setf anscl
                      (+ anscl
                         (expt
                          (*
                           (f2cl-lib:fref delz-%data%
                                          (i)
                                          ((1 1))
                                          delz-%offset%)
                           (f2cl-lib:fref scale-%data%
                                          (i)
                                          ((1 1))
                                          scale-%offset%))
                          2)))
             label180))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ndmz) nil)
            (tagbody
              (setf anscl
                      (+ anscl
                         (expt
                          (*
                           (f2cl-lib:fref deldmz-%data%
                                          (i)
                                          ((1 1))
                                          deldmz-%offset%)
                           (f2cl-lib:fref dscale-%data%
                                          (i)
                                          ((1 1))
                                          dscale-%offset%))
                          2)))
             label190))
          (setf anscl
                  (f2cl-lib:dsqrt
                   (/ anscl (f2cl-lib:dfloat (f2cl-lib:int-add nz ndmz)))))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
                 var-19 var-20 var-21)
              (lsyslv msing xi xiold z dmz delz deldmz g w v rhs dummy integs
               ipvtg ipvtw rnorm 3 fsub dfsub gsub dgsub guess)
            (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                             var-9 var-10 var-11 var-12 var-13 var-14 var-16
                             var-17 var-18 var-19 var-20 var-21))
            (setf msing var-0)
            (setf rnorm var-15))
          (if (/= msing 0) (go label30))
          (setf andif 0.0)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i nz) nil)
            (tagbody
              (setf andif
                      (+ andif
                         (expt
                          (*
                           (-
                            (f2cl-lib:fref dqz-%data% (i) ((1 1)) dqz-%offset%)
                            (f2cl-lib:fref delz-%data%
                                           (i)
                                           ((1 1))
                                           delz-%offset%))
                           (f2cl-lib:fref scale-%data%
                                          (i)
                                          ((1 1))
                                          scale-%offset%))
                          2)))
             label200))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ndmz) nil)
            (tagbody
              (setf andif
                      (+ andif
                         (expt
                          (*
                           (-
                            (f2cl-lib:fref dqdmz-%data%
                                           (i)
                                           ((1 1))
                                           dqdmz-%offset%)
                            (f2cl-lib:fref deldmz-%data%
                                           (i)
                                           ((1 1))
                                           deldmz-%offset%))
                           (f2cl-lib:fref dscale-%data%
                                          (i)
                                          ((1 1))
                                          dscale-%offset%))
                          2)))
             label210))
          (setf andif
                  (f2cl-lib:dsqrt
                   (+ (/ andif (f2cl-lib:dfloat (f2cl-lib:int-add nz ndmz)))
                      precis)))
          (setf relax (/ (* relax anscl) andif))
          (if (> relax 1.0) (setf relax 1.0))
         label220
          (setf rlxold relax)
          (setf ipred 1)
          (setf iter (f2cl-lib:int-add iter 1))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i nz) nil)
            (tagbody
              (setf (f2cl-lib:fref z-%data% (i) ((1 1)) z-%offset%)
                      (+ (f2cl-lib:fref z-%data% (i) ((1 1)) z-%offset%)
                         (* relax
                            (f2cl-lib:fref delz-%data%
                                           (i)
                                           ((1 1))
                                           delz-%offset%))))
             label230))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ndmz) nil)
            (tagbody
              (setf (f2cl-lib:fref dmz-%data% (i) ((1 1)) dmz-%offset%)
                      (+ (f2cl-lib:fref dmz-%data% (i) ((1 1)) dmz-%offset%)
                         (* relax
                            (f2cl-lib:fref deldmz-%data%
                                           (i)
                                           ((1 1))
                                           deldmz-%offset%))))
             label240))
         label250
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
                 var-19 var-20 var-21)
              (lsyslv msing xi xiold z dmz dqz dqdmz g w v rhs dummy integs
               ipvtg ipvtw rnorm 2 fsub dfsub gsub dgsub guess)
            (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                             var-9 var-10 var-11 var-12 var-13 var-14 var-16
                             var-17 var-18 var-19 var-20 var-21))
            (setf msing var-0)
            (setf rnorm var-15))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
                 var-19 var-20 var-21)
              (lsyslv msing xi xiold z dmz dqz dqdmz g w v rhs dummy integs
               ipvtg ipvtw rnorm 4 fsub dfsub gsub dgsub guess)
            (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                             var-9 var-10 var-11 var-12 var-13 var-14 var-16
                             var-17 var-18 var-19 var-20 var-21))
            (setf msing var-0)
            (setf rnorm var-15))
          (setf anorm 0.0)
          (setf anfix 0.0)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i nz) nil)
            (tagbody
              (setf anorm
                      (+ anorm
                         (expt
                          (*
                           (f2cl-lib:fref delz-%data%
                                          (i)
                                          ((1 1))
                                          delz-%offset%)
                           (f2cl-lib:fref scale-%data%
                                          (i)
                                          ((1 1))
                                          scale-%offset%))
                          2)))
              (setf anfix
                      (+ anfix
                         (expt
                          (*
                           (f2cl-lib:fref dqz-%data% (i) ((1 1)) dqz-%offset%)
                           (f2cl-lib:fref scale-%data%
                                          (i)
                                          ((1 1))
                                          scale-%offset%))
                          2)))
             label260))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ndmz) nil)
            (tagbody
              (setf anorm
                      (+ anorm
                         (expt
                          (*
                           (f2cl-lib:fref deldmz-%data%
                                          (i)
                                          ((1 1))
                                          deldmz-%offset%)
                           (f2cl-lib:fref dscale-%data%
                                          (i)
                                          ((1 1))
                                          dscale-%offset%))
                          2)))
              (setf anfix
                      (+ anfix
                         (expt
                          (*
                           (f2cl-lib:fref dqdmz-%data%
                                          (i)
                                          ((1 1))
                                          dqdmz-%offset%)
                           (f2cl-lib:fref dscale-%data%
                                          (i)
                                          ((1 1))
                                          dscale-%offset%))
                          2)))
             label270))
          (setf anorm
                  (f2cl-lib:dsqrt
                   (/ anorm (f2cl-lib:dfloat (f2cl-lib:int-add nz ndmz)))))
          (setf anfix
                  (f2cl-lib:dsqrt
                   (/ anfix (f2cl-lib:dfloat (f2cl-lib:int-add nz ndmz)))))
          (if (= icor 1) (go label280))
          (if (< iprint 0)
              (f2cl-lib:fformat iout
                                (" ITERATION = " 1 (("~3D"))
                                 "  RELAXATION FACTOR = " 1
                                 (("~10,2,2,0,'*,,'DE")) "~%"
                                 " NORM OF SCALED RHS CHANGES FROM " 1
                                 (("~10,2,2,0,'*,,'DE")) " TO" 1
                                 (("~10,2,2,0,'*,,'DE")) "~%"
                                 " NORM   OF   RHS  CHANGES  FROM  " 1
                                 (("~10,2,2,0,'*,,'DE")) " TO" 1
                                 (("~10,2,2,0,'*,,'DE")) 1
                                 (("~10,2,2,0,'*,,'DE")) "~%")
                                iter
                                relax
                                anorm
                                anfix
                                rnold
                                rnorm))
          (go label290)
         label280
          (if (< iprint 0)
              (f2cl-lib:fformat iout
                                (" RELAXATION FACTOR CORRECTED TO RELAX = " 1
                                 (("~10,2,2,0,'*,,'DE")) "~%"
                                 " NORM OF SCALED RHS CHANGES FROM " 1
                                 (("~10,2,2,0,'*,,'DE")) " TO" 1
                                 (("~10,2,2,0,'*,,'DE")) "~%"
                                 " NORM   OF   RHS  CHANGES  FROM  " 1
                                 (("~10,2,2,0,'*,,'DE")) " TO" 1
                                 (("~10,2,2,0,'*,,'DE")) 1
                                 (("~10,2,2,0,'*,,'DE")) "~%")
                                relax
                                anorm
                                anfix
                                rnold
                                rnorm))
         label290
          (setf icor 0)
          (if (or (< anfix precis) (< rnorm precis)) (go label390))
          (if (> anfix anorm) (go label300))
          (if (<= anfix check) (go label350))
          (if (/= ipred 1) (go label170))
         label300
          (if (>= iter limit) (go label430))
          (setf ipred 0)
          (setf arg (+ (/ (- (/ anfix anorm) 1.0) relax) 1.0))
          (if (< arg 0.0) (go label170))
          (if (<= arg (+ (* 0.25 relax) (* 0.125 (expt relax 2))))
              (go label310))
          (setf factor (- (f2cl-lib:dsqrt (+ 1.0 (* 8.0 arg))) 1.0))
          (if (< (f2cl-lib:dabs (- factor 1.0)) (* 0.1 factor)) (go label170))
          (if (< factor 0.5) (setf factor 0.5))
          (setf relax (/ relax factor))
          (go label320)
         label310
          (if (>= relax 0.9) (go label170))
          (setf relax 1.0)
         label320
          (setf icor 1)
          (if (< relax relmin) (go label440))
          (setf fact (- relax rlxold))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i nz) nil)
            (tagbody
              (setf (f2cl-lib:fref z-%data% (i) ((1 1)) z-%offset%)
                      (+ (f2cl-lib:fref z-%data% (i) ((1 1)) z-%offset%)
                         (* fact
                            (f2cl-lib:fref delz-%data%
                                           (i)
                                           ((1 1))
                                           delz-%offset%))))
             label330))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ndmz) nil)
            (tagbody
              (setf (f2cl-lib:fref dmz-%data% (i) ((1 1)) dmz-%offset%)
                      (+ (f2cl-lib:fref dmz-%data% (i) ((1 1)) dmz-%offset%)
                         (* fact
                            (f2cl-lib:fref deldmz-%data%
                                           (i)
                                           ((1 1))
                                           deldmz-%offset%))))
             label340))
          (setf rlxold relax)
          (go label250)
         label350
          (f2cl-lib:fdo (it 1 (f2cl-lib:int-add it 1))
                        ((> it ntol) nil)
            (tagbody
              (setf inz (f2cl-lib:fref ltol (it) ((1 40))))
              (f2cl-lib:fdo (iz inz (f2cl-lib:int-add iz mstar))
                            ((> iz nz) nil)
                (tagbody
                  (if
                   (>
                    (f2cl-lib:dabs
                     (f2cl-lib:fref dqz-%data% (iz) ((1 1)) dqz-%offset%))
                    (* (f2cl-lib:fref tolin (it) ((1 40)))
                       (+
                        (f2cl-lib:dabs
                         (f2cl-lib:fref z-%data% (iz) ((1 1)) z-%offset%))
                        1.0)))
                   (go label170))
                 label360))))
         label360
          (if (< iprint 1)
              (f2cl-lib:fformat iout
                                ("~%" " CONVERGENCE AFTER" 1 (("~3D"))
                                 " ITERATIONS" "~%" "~%")
                                iter))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i nz) nil)
            (tagbody
              (setf (f2cl-lib:fref z-%data% (i) ((1 1)) z-%offset%)
                      (+ (f2cl-lib:fref z-%data% (i) ((1 1)) z-%offset%)
                         (f2cl-lib:fref dqz-%data% (i) ((1 1)) dqz-%offset%)))
             label370))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ndmz) nil)
            (tagbody
              (setf (f2cl-lib:fref dmz-%data% (i) ((1 1)) dmz-%offset%)
                      (+ (f2cl-lib:fref dmz-%data% (i) ((1 1)) dmz-%offset%)
                         (f2cl-lib:fref dqdmz-%data%
                                        (i)
                                        ((1 1))
                                        dqdmz-%offset%)))
             label380))
         label390
          (if (and (or (< anfix precis) (< rnorm precis)) (< iprint 1))
              (f2cl-lib:fformat iout
                                ("~%" " CONVERGENCE AFTER" 1 (("~3D"))
                                 " ITERATIONS" "~%" "~%")
                                iter))
          (setf iconv 1)
          (if (= icare -1) (setf icare 0))
         label400
          (if (>= iprint 0) (go label420))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j mstar) nil)
            (tagbody
              (f2cl-lib:fformat iout
                                (" MESH VALUES FOR Z(" 1 (("~2D")) ")," "~%")
                                j)
             label410
              (f2cl-lib:fformat iout
                                (" " 8 (("~15,7,2,0,'*,,'DE")) "~%")
                                (do ((lj j (f2cl-lib:int-add lj mstar))
                                     (%ret nil))
                                    ((> lj nz) (nreverse %ret))
                                  (declare (type f2cl-lib:integer4 lj))
                                  (push
                                   (f2cl-lib:fref z-%data%
                                                  (lj)
                                                  ((1 1))
                                                  z-%offset%)
                                   %ret)))))
         label420
          (setf ifin 1)
          (if (= imesh 2)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (errchk xi z dmz valstr ifin)
                (declare (ignore var-0 var-1 var-2 var-3))
                (setf ifin var-4)))
          (if (or (= imesh 1) (and (= ifin 0) (/= icare 2))) (go label460))
          (setf iflag 1)
          (go end_label)
         label430
          (if (< iprint 1)
              (f2cl-lib:fformat iout
                                ("~%" " NO CONVERGENCE AFTER " 1 (("~3D"))
                                 " ITERATIONS" "~%" "~%")
                                iter))
          (go label450)
         label440
          (if (< iprint 1)
              (f2cl-lib:fformat iout
                                ("~%" " NO CONVERGENCE.  RELAXATION FACTOR =" 1
                                 (("~10,3,2,0,'*,,'DE"))
                                 " IS TOO SMALL (LESS THAN" 1
                                 (("~10,3,2,0,'*,,'DE")) ")" "~%" "~%")
                                relax
                                relmin))
         label450
          (setf iflag -2)
          (setf noconv (f2cl-lib:int-add noconv 1))
          (if (and (= icare 2) (> noconv 1)) (go end_label))
          (if (= icare 0) (setf icare -1))
         label460
          (setf np1 (f2cl-lib:int-add n 1))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i np1) nil)
            (tagbody
             label470
              (setf (f2cl-lib:fref xiold-%data% (i) ((1 1)) xiold-%offset%)
                      (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%))))
          (setf nold n)
          (setf imesh 1)
          (if (or (= iconv 0) (>= mshnum mshlmt) (>= mshalt mshlmt))
              (setf imesh 2))
          (if (and (>= mshalt mshlmt) (< mshnum mshlmt)) (setf mshalt 1))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (newmsh imesh xi xiold z dmz valstr slope accum nfxpnt fixpnt)
            (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                             var-9))
            (setf imesh var-0))
          (if (<= n nmax) (go label480))
          (setf n (the f2cl-lib:integer4 (truncate n 2)))
          (setf iflag -1)
          (if (and (= iconv 0) (< iprint 1))
              (f2cl-lib:fformat iout ("  (NO CONVERGENCE)" "~%")))
          (if (and (= iconv 1) (< iprint 1))
              (f2cl-lib:fformat iout
                                ("  (PROBABLY TOLERANCES TOO STRINGENT, OR NMAX TOO "
                                 "SMALL)" "~%")))
          (go end_label)
         label480
          (if (= iconv 0) (setf imesh 1))
          (if (= icare 1) (setf iconv 0))
          (go label20)
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
                   iflag
                   nil
                   nil
                   nil
                   nil
                   nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::contrl
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (1)) (array double-float (1))
                        (array double-float (1)) (array double-float (1))
                        (array double-float (1)) (array double-float (1))
                        (array double-float (1)) (array double-float (1))
                        (array double-float (1)) (array double-float (1))
                        (array double-float (1)) (array double-float (1))
                        (array double-float (1)) (array double-float (1))
                        (array double-float (1)) (array double-float (1))
                        (array double-float (1))
                        (array fortran-to-lisp::integer4 (1))
                        (array fortran-to-lisp::integer4 (1))
                        (array fortran-to-lisp::integer4 (1))
                        (fortran-to-lisp::integer4) (array double-float (1))
                        (fortran-to-lisp::integer4) t t t t t)
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::iflag nil nil nil nil nil)
           :calls '(fortran-to-lisp::newmsh fortran-to-lisp::errchk
                    fortran-to-lisp::skale fortran-to-lisp::lsyslv))))

