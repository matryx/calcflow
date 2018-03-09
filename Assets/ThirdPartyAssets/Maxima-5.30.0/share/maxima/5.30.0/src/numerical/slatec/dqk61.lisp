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
       (make-array 15
                   :element-type 'double-float
                   :initial-contents '(0.007968192496166605 0.01846646831109096
                                       0.02878470788332337 0.03879919256962705
                                       0.04840267283059405 0.057493156217619065
                                       0.06597422988218049 0.0737559747377052
                                       0.08075589522942021 0.08689978720108298
                                       0.09212252223778612 0.09636873717464425
                                       0.09959342058679527 0.1017623897484055
                                       0.10285265289355884)))
      (xgk
       (make-array 31
                   :element-type 'double-float
                   :initial-contents '(0.9994844100504906 0.9968934840746495
                                       0.9916309968704046 0.9836681232797472
                                       0.9731163225011262 0.9600218649683075
                                       0.94437444474856 0.9262000474292743
                                       0.9055733076999078 0.8825605357920527
                                       0.8572052335460612 0.8295657623827684
                                       0.799727835821839 0.7677774321048262
                                       0.7337900624532268 0.6978504947933158
                                       0.6600610641266269 0.6205261829892429
                                       0.5793452358263617 0.5366241481420199
                                       0.49248046786177857 0.44703376953808915
                                       0.4004012548303944 0.3527047255308781
                                       0.30407320227362505 0.25463692616788985
                                       0.20452511668230988 0.15386991360858354
                                       0.10280693796673702 0.0514718425553177
                                       0.0)))
      (wgk
       (make-array 31
                   :element-type 'double-float
                   :initial-contents '(0.0013890136986770077
                                       0.003890461127099884
                                       0.0066307039159312926
                                       0.009273279659517764
                                       0.011823015253496341
                                       0.014369729507045804 0.01692088918905327
                                       0.019414141193942382
                                       0.021828035821609193 0.0241911620780806
                                       0.0265099548823331 0.02875404876504129
                                       0.030907257562387762 0.03298144705748372
                                       0.034979338028060025 0.03688236465182123
                                       0.038678945624727595
                                       0.040374538951535956
                                       0.041969810215164244 0.04345253970135607
                                       0.04481480013316266 0.04605923827100699
                                       0.04718554656929915 0.04818586175708713
                                       0.04905543455502978 0.04979568342707421
                                       0.05040592140278235 0.05088179589874961
                                       0.051221547849258774 0.05142612853745902
                                       0.05149472942945157))))
  (declare (type (array double-float (15)) wg)
           (type (array double-float (31)) xgk wgk))
  (defun dqk61 (f a b result abserr resabs resasc)
    (declare (type (double-float) resasc resabs abserr result b a))
    (prog ((fv1 (make-array 30 :element-type 'double-float))
           (fv2 (make-array 30 :element-type 'double-float)) (j 0) (jtw 0)
           (jtwm1 0) (dabsc 0.0) (centr 0.0) (dhlgth 0.0) (epmach 0.0) (fc 0.0)
           (fsum 0.0) (fval1 0.0) (fval2 0.0) (hlgth 0.0) (resg 0.0) (resk 0.0)
           (reskh 0.0) (uflow 0.0))
      (declare (type (array double-float (30)) fv2 fv1)
               (type (double-float) uflow reskh resk resg hlgth fval2 fval1
                                    fsum fc epmach dhlgth centr dabsc)
               (type (f2cl-lib:integer4) jtwm1 jtw j))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf uflow (f2cl-lib:d1mach 1))
      (setf centr (* 0.5 (+ b a)))
      (setf hlgth (* 0.5 (- b a)))
      (setf dhlgth (abs hlgth))
      (setf resg 0.0)
      (setf fc
              (multiple-value-bind (ret-val var-0)
                  (funcall f centr)
                (declare (ignore))
                (when var-0
                  (setf centr var-0))
                ret-val))
      (setf resk (* (f2cl-lib:fref wgk (31) ((1 31))) fc))
      (setf resabs (abs resk))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 15) nil)
        (tagbody
          (setf jtw (f2cl-lib:int-mul j 2))
          (setf dabsc (* hlgth (f2cl-lib:fref xgk (jtw) ((1 31)))))
          (setf fval1 (funcall f (- centr dabsc)))
          (setf fval2 (funcall f (+ centr dabsc)))
          (setf (f2cl-lib:fref fv1 (jtw) ((1 30))) fval1)
          (setf (f2cl-lib:fref fv2 (jtw) ((1 30))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resg (+ resg (* (f2cl-lib:fref wg (j) ((1 15))) fsum)))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (jtw) ((1 31))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (jtw) ((1 31)))
                        (+ (abs fval1) (abs fval2)))))
         label10))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 15) nil)
        (tagbody
          (setf jtwm1 (f2cl-lib:int-sub (f2cl-lib:int-mul j 2) 1))
          (setf dabsc (* hlgth (f2cl-lib:fref xgk (jtwm1) ((1 31)))))
          (setf fval1 (funcall f (- centr dabsc)))
          (setf fval2 (funcall f (+ centr dabsc)))
          (setf (f2cl-lib:fref fv1 (jtwm1) ((1 30))) fval1)
          (setf (f2cl-lib:fref fv2 (jtwm1) ((1 30))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (jtwm1) ((1 31))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (jtwm1) ((1 31)))
                        (+ (abs fval1) (abs fval2)))))
         label15))
      (setf reskh (* resk 0.5))
      (setf resasc (* (f2cl-lib:fref wgk (31) ((1 31))) (abs (- fc reskh))))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 30) nil)
        (tagbody
          (setf resasc
                  (+ resasc
                     (* (f2cl-lib:fref wgk (j) ((1 31)))
                        (+ (abs (- (f2cl-lib:fref fv1 (j) ((1 30))) reskh))
                           (abs (- (f2cl-lib:fref fv2 (j) ((1 30))) reskh))))))
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
  (setf (gethash 'fortran-to-lisp::dqk61 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float))
           :return-values '(nil nil nil fortran-to-lisp::result
                            fortran-to-lisp::abserr fortran-to-lisp::resabs
                            fortran-to-lisp::resasc)
           :calls '(fortran-to-lisp::d1mach))))

