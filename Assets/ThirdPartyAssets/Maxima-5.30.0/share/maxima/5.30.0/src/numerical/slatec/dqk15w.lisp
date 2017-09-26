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


(let ((xgk
       (make-array 8
                   :element-type 'double-float
                   :initial-contents '(0.9914553711208126 0.9491079123427585
                                       0.8648644233597691 0.7415311855993943
                                       0.5860872354676911 0.4058451513773972
                                       0.2077849550078985 0.0)))
      (wgk
       (make-array 8
                   :element-type 'double-float
                   :initial-contents '(0.02293532201052922 0.06309209262997854
                                       0.1047900103222502 0.1406532597155259
                                       0.1690047266392679 0.1903505780647854
                                       0.2044329400752989 0.2094821410847278)))
      (wg
       (make-array 4
                   :element-type 'double-float
                   :initial-contents '(0.1294849661688697 0.2797053914892767
                                       0.3818300505051889 0.4179591836734694))))
  (declare (type (array double-float (8)) xgk wgk)
           (type (array double-float (4)) wg))
  (defun dqk15w (f w p1 p2 p3 p4 kp a b result abserr resabs resasc)
    (declare (type (f2cl-lib:integer4) kp)
             (type (double-float) resasc resabs abserr result b a p4 p3 p2 p1))
    (prog ((fv1 (make-array 7 :element-type 'double-float))
           (fv2 (make-array 7 :element-type 'double-float)) (j 0) (jtw 0)
           (jtwm1 0) (absc 0.0) (absc1 0.0) (absc2 0.0) (centr 0.0)
           (dhlgth 0.0) (epmach 0.0) (fc 0.0) (fsum 0.0) (fval1 0.0)
           (fval2 0.0) (hlgth 0.0) (resg 0.0) (resk 0.0) (reskh 0.0)
           (uflow 0.0))
      (declare (type (array double-float (7)) fv2 fv1)
               (type (double-float) uflow reskh resk resg hlgth fval2 fval1
                                    fsum fc epmach dhlgth centr absc2 absc1
                                    absc)
               (type (f2cl-lib:integer4) jtwm1 jtw j))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf uflow (f2cl-lib:d1mach 1))
      (setf centr (* 0.5 (+ a b)))
      (setf hlgth (* 0.5 (- b a)))
      (setf dhlgth (abs hlgth))
      (setf fc
              (*
               (multiple-value-bind (ret-val var-0)
                   (funcall f centr)
                 (declare (ignore))
                 (when var-0
                   (setf centr var-0))
                 ret-val)
               (multiple-value-bind
                     (ret-val var-0 var-1 var-2 var-3 var-4 var-5)
                   (funcall w centr p1 p2 p3 p4 kp)
                 (declare (ignore))
                 (when var-0
                   (setf centr var-0))
                 (when var-1
                   (setf p1 var-1))
                 (when var-2
                   (setf p2 var-2))
                 (when var-3
                   (setf p3 var-3))
                 (when var-4
                   (setf p4 var-4))
                 (when var-5
                   (setf kp var-5))
                 ret-val)))
      (setf resg (* (f2cl-lib:fref wg (4) ((1 4))) fc))
      (setf resk (* (f2cl-lib:fref wgk (8) ((1 8))) fc))
      (setf resabs (abs resk))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 3) nil)
        (tagbody
          (setf jtw (f2cl-lib:int-mul j 2))
          (setf absc (* hlgth (f2cl-lib:fref xgk (jtw) ((1 8)))))
          (setf absc1 (- centr absc))
          (setf absc2 (+ centr absc))
          (setf fval1
                  (*
                   (multiple-value-bind (ret-val var-0)
                       (funcall f absc1)
                     (declare (ignore))
                     (when var-0
                       (setf absc1 var-0))
                     ret-val)
                   (multiple-value-bind
                         (ret-val var-0 var-1 var-2 var-3 var-4 var-5)
                       (funcall w absc1 p1 p2 p3 p4 kp)
                     (declare (ignore))
                     (when var-0
                       (setf absc1 var-0))
                     (when var-1
                       (setf p1 var-1))
                     (when var-2
                       (setf p2 var-2))
                     (when var-3
                       (setf p3 var-3))
                     (when var-4
                       (setf p4 var-4))
                     (when var-5
                       (setf kp var-5))
                     ret-val)))
          (setf fval2
                  (*
                   (multiple-value-bind (ret-val var-0)
                       (funcall f absc2)
                     (declare (ignore))
                     (when var-0
                       (setf absc2 var-0))
                     ret-val)
                   (multiple-value-bind
                         (ret-val var-0 var-1 var-2 var-3 var-4 var-5)
                       (funcall w absc2 p1 p2 p3 p4 kp)
                     (declare (ignore))
                     (when var-0
                       (setf absc2 var-0))
                     (when var-1
                       (setf p1 var-1))
                     (when var-2
                       (setf p2 var-2))
                     (when var-3
                       (setf p3 var-3))
                     (when var-4
                       (setf p4 var-4))
                     (when var-5
                       (setf kp var-5))
                     ret-val)))
          (setf (f2cl-lib:fref fv1 (jtw) ((1 7))) fval1)
          (setf (f2cl-lib:fref fv2 (jtw) ((1 7))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resg (+ resg (* (f2cl-lib:fref wg (j) ((1 4))) fsum)))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (jtw) ((1 8))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (jtw) ((1 8)))
                        (+ (abs fval1) (abs fval2)))))
         label10))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 4) nil)
        (tagbody
          (setf jtwm1 (f2cl-lib:int-sub (f2cl-lib:int-mul j 2) 1))
          (setf absc (* hlgth (f2cl-lib:fref xgk (jtwm1) ((1 8)))))
          (setf absc1 (- centr absc))
          (setf absc2 (+ centr absc))
          (setf fval1
                  (*
                   (multiple-value-bind (ret-val var-0)
                       (funcall f absc1)
                     (declare (ignore))
                     (when var-0
                       (setf absc1 var-0))
                     ret-val)
                   (multiple-value-bind
                         (ret-val var-0 var-1 var-2 var-3 var-4 var-5)
                       (funcall w absc1 p1 p2 p3 p4 kp)
                     (declare (ignore))
                     (when var-0
                       (setf absc1 var-0))
                     (when var-1
                       (setf p1 var-1))
                     (when var-2
                       (setf p2 var-2))
                     (when var-3
                       (setf p3 var-3))
                     (when var-4
                       (setf p4 var-4))
                     (when var-5
                       (setf kp var-5))
                     ret-val)))
          (setf fval2
                  (*
                   (multiple-value-bind (ret-val var-0)
                       (funcall f absc2)
                     (declare (ignore))
                     (when var-0
                       (setf absc2 var-0))
                     ret-val)
                   (multiple-value-bind
                         (ret-val var-0 var-1 var-2 var-3 var-4 var-5)
                       (funcall w absc2 p1 p2 p3 p4 kp)
                     (declare (ignore))
                     (when var-0
                       (setf absc2 var-0))
                     (when var-1
                       (setf p1 var-1))
                     (when var-2
                       (setf p2 var-2))
                     (when var-3
                       (setf p3 var-3))
                     (when var-4
                       (setf p4 var-4))
                     (when var-5
                       (setf kp var-5))
                     ret-val)))
          (setf (f2cl-lib:fref fv1 (jtwm1) ((1 7))) fval1)
          (setf (f2cl-lib:fref fv2 (jtwm1) ((1 7))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (jtwm1) ((1 8))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (jtwm1) ((1 8)))
                        (+ (abs fval1) (abs fval2)))))
         label15))
      (setf reskh (* resk 0.5))
      (setf resasc (* (f2cl-lib:fref wgk (8) ((1 8))) (abs (- fc reskh))))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 7) nil)
        (tagbody
          (setf resasc
                  (+ resasc
                     (* (f2cl-lib:fref wgk (j) ((1 8)))
                        (+ (abs (- (f2cl-lib:fref fv1 (j) ((1 7))) reskh))
                           (abs (- (f2cl-lib:fref fv2 (j) ((1 7))) reskh))))))
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
      (return
       (values nil nil p1 p2 p3 p4 kp nil nil result abserr resabs resasc)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dqk15w
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t t (double-float) (double-float) (double-float)
                        (double-float) (fortran-to-lisp::integer4)
                        (double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float))
           :return-values '(nil nil fortran-to-lisp::p1 fortran-to-lisp::p2
                            fortran-to-lisp::p3 fortran-to-lisp::p4
                            fortran-to-lisp::kp nil nil fortran-to-lisp::result
                            fortran-to-lisp::abserr fortran-to-lisp::resabs
                            fortran-to-lisp::resasc)
           :calls '(fortran-to-lisp::d1mach))))

