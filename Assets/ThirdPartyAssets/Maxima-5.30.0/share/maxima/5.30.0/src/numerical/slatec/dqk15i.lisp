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
                   :initial-contents '(0.0 0.1294849661688697 0.0
                                       0.27970539148927664 0.0
                                       0.3818300505051189 0.0
                                       0.4179591836734694)))
      (xgk
       (make-array 8
                   :element-type 'double-float
                   :initial-contents '(0.9914553711208126 0.9491079123427585
                                       0.8648644233597691 0.7415311855993945
                                       0.5860872354676911 0.4058451513773972
                                       0.20778495500789848 0.0)))
      (wgk
       (make-array 8
                   :element-type 'double-float
                   :initial-contents '(0.022935322010529224 0.06309209262997856
                                       0.10479001032225019 0.14065325971552592
                                       0.1690047266392679 0.19035057806478542
                                       0.20443294007529889
                                       0.20948214108472782))))
  (declare (type (array double-float (8)) wg xgk wgk))
  (defun dqk15i (f boun inf a b result abserr resabs resasc)
    (declare (type (f2cl-lib:integer4) inf)
             (type (double-float) resasc resabs abserr result b a boun))
    (prog ((fv1 (make-array 7 :element-type 'double-float))
           (fv2 (make-array 7 :element-type 'double-float)) (j 0) (absc 0.0)
           (absc1 0.0) (absc2 0.0) (centr 0.0) (dinf 0.0) (epmach 0.0) (fc 0.0)
           (fsum 0.0) (fval1 0.0) (fval2 0.0) (hlgth 0.0) (resg 0.0) (resk 0.0)
           (reskh 0.0) (tabsc1 0.0) (tabsc2 0.0) (uflow 0.0))
      (declare (type (array double-float (7)) fv2 fv1)
               (type (double-float) uflow tabsc2 tabsc1 reskh resk resg hlgth
                                    fval2 fval1 fsum fc epmach dinf centr absc2
                                    absc1 absc)
               (type (f2cl-lib:integer4) j))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf uflow (f2cl-lib:d1mach 1))
      (setf dinf
              (coerce
               (the f2cl-lib:integer4
                    (min (the f2cl-lib:integer4 1)
                         (the f2cl-lib:integer4 inf)))
               'double-float))
      (setf centr (* 0.5 (+ a b)))
      (setf hlgth (* 0.5 (- b a)))
      (setf tabsc1 (+ boun (/ (* dinf (- 1.0 centr)) centr)))
      (setf fval1
              (multiple-value-bind (ret-val var-0)
                  (funcall f tabsc1)
                (declare (ignore))
                (when var-0
                  (setf tabsc1 var-0))
                ret-val))
      (if (= inf 2) (setf fval1 (+ fval1 (funcall f (- tabsc1)))))
      (setf fc (/ (/ fval1 centr) centr))
      (setf resg (* (f2cl-lib:fref wg (8) ((1 8))) fc))
      (setf resk (* (f2cl-lib:fref wgk (8) ((1 8))) fc))
      (setf resabs (abs resk))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j 7) nil)
        (tagbody
          (setf absc (* hlgth (f2cl-lib:fref xgk (j) ((1 8)))))
          (setf absc1 (- centr absc))
          (setf absc2 (+ centr absc))
          (setf tabsc1 (+ boun (/ (* dinf (- 1.0 absc1)) absc1)))
          (setf tabsc2 (+ boun (/ (* dinf (- 1.0 absc2)) absc2)))
          (setf fval1
                  (multiple-value-bind (ret-val var-0)
                      (funcall f tabsc1)
                    (declare (ignore))
                    (when var-0
                      (setf tabsc1 var-0))
                    ret-val))
          (setf fval2
                  (multiple-value-bind (ret-val var-0)
                      (funcall f tabsc2)
                    (declare (ignore))
                    (when var-0
                      (setf tabsc2 var-0))
                    ret-val))
          (if (= inf 2) (setf fval1 (+ fval1 (funcall f (- tabsc1)))))
          (if (= inf 2) (setf fval2 (+ fval2 (funcall f (- tabsc2)))))
          (setf fval1 (/ (/ fval1 absc1) absc1))
          (setf fval2 (/ (/ fval2 absc2) absc2))
          (setf (f2cl-lib:fref fv1 (j) ((1 7))) fval1)
          (setf (f2cl-lib:fref fv2 (j) ((1 7))) fval2)
          (setf fsum (+ fval1 fval2))
          (setf resg (+ resg (* (f2cl-lib:fref wg (j) ((1 8))) fsum)))
          (setf resk (+ resk (* (f2cl-lib:fref wgk (j) ((1 8))) fsum)))
          (setf resabs
                  (+ resabs
                     (* (f2cl-lib:fref wgk (j) ((1 8)))
                        (+ (abs fval1) (abs fval2)))))
         label10))
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
      (setf resasc (* resasc hlgth))
      (setf resabs (* resabs hlgth))
      (setf abserr (abs (* (- resk resg) hlgth)))
      (if (and (/= resasc 0.0) (/= abserr 0.0))
          (setf abserr
                  (* resasc (min 1.0 (expt (/ (* 200.0 abserr) resasc) 1.5)))))
      (if (> resabs (/ uflow (* 50.0 epmach)))
          (setf abserr (max (* epmach 50.0 resabs) abserr)))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil result abserr resabs resasc)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dqk15i
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (double-float) (fortran-to-lisp::integer4)
                        (double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float))
           :return-values '(nil nil nil nil nil fortran-to-lisp::result
                            fortran-to-lisp::abserr fortran-to-lisp::resabs
                            fortran-to-lisp::resasc)
           :calls '(fortran-to-lisp::d1mach))))

