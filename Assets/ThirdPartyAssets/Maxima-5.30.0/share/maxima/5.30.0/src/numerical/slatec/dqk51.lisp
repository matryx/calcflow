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


(let ((wg
       (make-array 13
                   :element-type 'double-float
                   :initial-contents '(0.011393798501026288
                                       0.026354986615032137
                                       0.040939156701306316
                                       0.054904695975835194 0.06803833381235691
                                       0.08014070033500102 0.09102826198296365
                                       0.10053594906705064 0.10851962447426365
                                       0.11485825914571164 0.11945576353578477
                                       0.12224244299031004
                                       0.12317605372671545)))
      (xgk
       (make-array 26
                   :element-type 'double-float
                   :initial-contents '(0.9992621049926098 0.9955569697904981
                                       0.9880357945340772 0.9766639214595175
                                       0.9616149864258425 0.9429745712289743
                                       0.9207471152817016 0.8949919978782753
                                       0.8658470652932756 0.833442628760834
                                       0.7978737979985001 0.7592592630373576
                                       0.7177664068130843 0.6735663684734684
                                       0.6268100990103174 0.577662930241223
                                       0.5263252843347191 0.473002731445715
                                       0.4178853821930377 0.36117230580938786
                                       0.30308953893110785 0.24386688372098844
                                       0.1837189394210489 0.1228646926107104
                                       0.06154448300568508 0.0)))
      (wgk
       (make-array 26
                   :element-type 'double-float
                   :initial-contents '(0.001987383892330316
                                       0.005561932135356714
                                       0.009473973386174152
                                       0.013236229195571676 0.0168478177091283
                                       0.020435371145882834
                                       0.024009945606953215 0.02747531758785174
                                       0.030792300167387487
                                       0.034002130274329335 0.03711627148341554
                                       0.04008382550403238 0.04287284502017005
                                       0.04550291304992179 0.04798253713883671
                                       0.05027767908071567 0.05236288580640747
                                       0.05425112988854549 0.055950811220412316
                                       0.057437116361567835
                                       0.058689680022394206 0.05972034032417406
                                       0.06053945537604586 0.061128509717053046
                                       0.061471189871425316
                                       0.061580818067832936))))
  (declare (type (array double-float (13)) wg)
           (type (array double-float (26)) xgk wgk))
  (defun dqk51 (f a b result abserr resabs resasc)
    (declare (type (double-float) resasc resabs abserr result b a))
    (prog ((fv1 (make-array 25 :element-type 'double-float))
           (fv2 (make-array 25 :element-type 'double-float)) (j 0) (jtw 0)
           (jtwm1 0) (absc 0.0) (centr 0.0) (dhlgth 0.0) (epmach 0.0) (fc 0.0)
           (fsum 0.0) (fval1 0.0) (fval2 0.0) (hlgth 0.0) (resg 0.0) (resk 0.0)
           (reskh 0.0) (uflow 0.0))
      (declare (type (array double-float (25)) fv2 fv1)
               (type (double-float) uflow reskh resk resg hlgth fval2 fval1
                                    fsum fc epmach dhlgth centr absc)
               (type (f2cl-lib:integer4) jtwm1 jtw j))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf uflow (f2cl-lib:d1mach 1))
      (setf centr (* 0.5 (+ a b)))
      (setf hlgth (* 0.5 (- b a)))
      (setf dhlgth (abs hlgth))
      (setf fc
              (multiple-value-bind (ret-val var-0)
                  (funcall f centr)
                (declare (ignore))
                (when var-0
                  (setf centr var-0))
                ret-val))
      (setf resg (* (f2cl-lib:fref wg (13) ((1 13))) fc))
      (setf resk (* (f2cl-lib:fref wgk (26) ((1 26))) fc))
      (setf resabs (abs resk))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 12) nil)
        (tagbody
          (setf jtw (f2cl-lib:int-mul j 2))
          (setf absc (* hlgth (f2cl-lib:fref xgk (jtw) ((1 26)))))
          (setf fval1 (funcall f (- centr absc)))
          (setf fval2 (funcall f (+ centr absc)))
          (setf (f2cl-lib:fref fv1 (jtw) ((1 25))) fval1)
          (setf (f2cl-lib:fref fv2 (jtw) ((1 25))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resg (+ resg (* (f2cl-lib:fref wg (j) ((1 13))) fsum)))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (jtw) ((1 26))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (jtw) ((1 26)))
                        (+ (abs fval1) (abs fval2)))))
         label10))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 13) nil)
        (tagbody
          (setf jtwm1 (f2cl-lib:int-sub (f2cl-lib:int-mul j 2) 1))
          (setf absc (* hlgth (f2cl-lib:fref xgk (jtwm1) ((1 26)))))
          (setf fval1 (funcall f (- centr absc)))
          (setf fval2 (funcall f (+ centr absc)))
          (setf (f2cl-lib:fref fv1 (jtwm1) ((1 25))) fval1)
          (setf (f2cl-lib:fref fv2 (jtwm1) ((1 25))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (jtwm1) ((1 26))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (jtwm1) ((1 26)))
                        (+ (abs fval1) (abs fval2)))))
         label15))
      (setf reskh (* resk 0.5))
      (setf resasc (* (f2cl-lib:fref wgk (26) ((1 26))) (abs (- fc reskh))))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 25) nil)
        (tagbody
          (setf resasc
                  (+ resasc
                     (* (f2cl-lib:fref wgk (j) ((1 26)))
                        (+ (abs (- (f2cl-lib:fref fv1 (j) ((1 25))) reskh))
                           (abs (- (f2cl-lib:fref fv2 (j) ((1 25))) reskh))))))
         label20))
      (setf result (* resk hlgth))
      (setf resabs (* resabs dhlgth))
      (setf resasc (* resasc dhlgth))
      (setf abserr (abs (* (- resk resg) hlgth)))
      (if (and (/= resasc 0.0) (/= abserr 0.0))
          (setf abserr
                  (* resasc (min 1.0 (expt (/ (* 200.0 abserr) resasc) 1.5)))))
      (if (> resabs (/ uflow (* 50.0 epmach)))
          (setf abserr (max (* epmach 50.0 resabs) abserr)))
      (go end_label)
     end_label
      (return (values nil nil nil result abserr resabs resasc)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dqk51 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float))
           :return-values '(nil nil nil fortran-to-lisp::result
                            fortran-to-lisp::abserr fortran-to-lisp::resabs
                            fortran-to-lisp::resasc)
           :calls '(fortran-to-lisp::d1mach))))

