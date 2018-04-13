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
       (make-array 8
                   :element-type 'double-float
                   :initial-contents '(0.03075324199611727 0.07036604748810812
                                       0.10715922046717194 0.13957067792615432
                                       0.16626920581699392 0.1861610000155622
                                       0.19843148532711158
                                       0.2025782419255613)))
      (xgk
       (make-array 16
                   :element-type 'double-float
                   :initial-contents '(0.9980022986933971 0.9879925180204854
                                       0.9677390756791391 0.937273392400706
                                       0.8972645323440819 0.8482065834104272
                                       0.790418501442466 0.7244177313601701
                                       0.650996741297417 0.5709721726085388
                                       0.4850818636402397 0.3941513470775634
                                       0.29918000715316884 0.20119409399743451
                                       0.1011420669187175 0.0)))
      (wgk
       (make-array 16
                   :element-type 'double-float
                   :initial-contents '(0.005377479872923349
                                       0.015007947329316122 0.02546084732671532
                                       0.03534636079137585 0.04458975132476488
                                       0.05348152469092809 0.06200956780067064
                                       0.06985412131872826 0.07684968075772038
                                       0.08308050282313302 0.08856444305621176
                                       0.09312659817082532 0.09664272698362368
                                       0.09917359872179196 0.10076984552387559
                                       0.10133000701479154))))
  (declare (type (array double-float (8)) wg)
           (type (array double-float (16)) xgk wgk))
  (defun dqk31 (f a b result abserr resabs resasc)
    (declare (type (double-float) resasc resabs abserr result b a))
    (prog ((fv1 (make-array 15 :element-type 'double-float))
           (fv2 (make-array 15 :element-type 'double-float)) (j 0) (jtw 0)
           (jtwm1 0) (absc 0.0) (centr 0.0) (dhlgth 0.0) (epmach 0.0) (fc 0.0)
           (fsum 0.0) (fval1 0.0) (fval2 0.0) (hlgth 0.0) (resg 0.0) (resk 0.0)
           (reskh 0.0) (uflow 0.0))
      (declare (type (array double-float (15)) fv2 fv1)
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
      (setf resg (* (f2cl-lib:fref wg (8) ((1 8))) fc))
      (setf resk (* (f2cl-lib:fref wgk (16) ((1 16))) fc))
      (setf resabs (abs resk))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 7) nil)
        (tagbody
          (setf jtw (f2cl-lib:int-mul j 2))
          (setf absc (* hlgth (f2cl-lib:fref xgk (jtw) ((1 16)))))
          (setf fval1 (funcall f (- centr absc)))
          (setf fval2 (funcall f (+ centr absc)))
          (setf (f2cl-lib:fref fv1 (jtw) ((1 15))) fval1)
          (setf (f2cl-lib:fref fv2 (jtw) ((1 15))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resg (+ resg (* (f2cl-lib:fref wg (j) ((1 8))) fsum)))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (jtw) ((1 16))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (jtw) ((1 16)))
                        (+ (abs fval1) (abs fval2)))))
         label10))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 8) nil)
        (tagbody
          (setf jtwm1 (f2cl-lib:int-sub (f2cl-lib:int-mul j 2) 1))
          (setf absc (* hlgth (f2cl-lib:fref xgk (jtwm1) ((1 16)))))
          (setf fval1 (funcall f (- centr absc)))
          (setf fval2 (funcall f (+ centr absc)))
          (setf (f2cl-lib:fref fv1 (jtwm1) ((1 15))) fval1)
          (setf (f2cl-lib:fref fv2 (jtwm1) ((1 15))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (jtwm1) ((1 16))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (jtwm1) ((1 16)))
                        (+ (abs fval1) (abs fval2)))))
         label15))
      (setf reskh (* resk 0.5))
      (setf resasc (* (f2cl-lib:fref wgk (16) ((1 16))) (abs (- fc reskh))))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 15) nil)
        (tagbody
          (setf resasc
                  (+ resasc
                     (* (f2cl-lib:fref wgk (j) ((1 16)))
                        (+ (abs (- (f2cl-lib:fref fv1 (j) ((1 15))) reskh))
                           (abs (- (f2cl-lib:fref fv2 (j) ((1 15))) reskh))))))
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
  (setf (gethash 'fortran-to-lisp::dqk31 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float))
           :return-values '(nil nil nil fortran-to-lisp::result
                            fortran-to-lisp::abserr fortran-to-lisp::resabs
                            fortran-to-lisp::resasc)
           :calls '(fortran-to-lisp::d1mach))))

