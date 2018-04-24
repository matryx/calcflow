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


(let ((ntk1 0)
      (ntak1 0)
      (ntak12 0)
      (xmin 0.0)
      (xsml 0.0)
      (bk1cs
       (make-array 16
                   :element-type 'double-float
                   :initial-contents '(0.02530022733894777 -0.3531559607765449
                                       -0.12261118082265715
                                       -0.006975723859639864
                                       -1.730288957513052e-4
                                       -2.4334061415659684e-6
                                       -2.213387630734726e-8
                                       -1.4114883926335278e-10
                                       -6.666901694199329e-13
                                       -2.427449850519366e-15
                                       -7.023863479386288e-18
                                       -1.6543275155100994e-20
                                       -3.233834745994449e-23
                                       -5.331275052926527e-26
                                       -7.513040716215723e-29
                                       -9.155085717654187e-32)))
      (ak1cs
       (make-array 38
                   :element-type 'double-float
                   :initial-contents '(0.2744313406973883 0.07571989953199368
                                       -0.0014410515564754062
                                       6.650116955125748e-5
                                       -4.369984709520141e-6
                                       3.5402774997630525e-7
                                       -3.311163779293292e-8
                                       3.4459775819010535e-9
                                       -3.898932347475427e-10
                                       4.720819750465836e-11
                                       -6.047835662875356e-12
                                       8.128494874865875e-13
                                       -1.138694574714789e-13
                                       1.654035840846228e-14
                                       -2.4809025677068848e-15
                                       3.8292378907024097e-16
                                       -6.064734104001242e-17
                                       9.832425623264862e-18
                                       -1.628416873828438e-18
                                       2.750153649675262e-19
                                       -4.728966646395325e-20
                                       8.268150002810994e-21
                                       -1.4681405136624957e-21
                                       2.6447639269208245e-22
                                       -4.829015756485639e-23
                                       8.929302074361012e-24
                                       -1.6708397168972516e-24
                                       3.1616456034040695e-25
                                       -6.046205531227498e-26
                                       1.1678798942042733e-26
                                       -2.2773741582653997e-27
                                       4.481109730077368e-28
                                       -8.893288476902019e-29
                                       1.7794680018850274e-29
                                       -3.58845559673291e-30
                                       7.290629049269426e-31
                                       -1.4918449845546228e-31
                                       3.0736573872934276e-32)))
      (ak12cs
       (make-array 33
                   :element-type 'double-float
                   :initial-contents '(0.06379308343739001 0.02832887813049721
                                       -2.4753706739052506e-4
                                       5.771972451607249e-6
                                       -2.0689392195365484e-7
                                       9.739983441381804e-9
                                       -5.585336140380625e-10
                                       3.7329966340461855e-11
                                       -2.8250519610232256e-12
                                       2.372019002484144e-13
                                       -2.176677387991754e-14
                                       2.1579141616160325e-15
                                       -2.290196930718269e-16
                                       2.582885729823275e-17
                                       -3.076752641268463e-18
                                       3.8514877212804914e-19
                                       -5.044794897641529e-20
                                       6.888673850418544e-21
                                       -9.775041541950119e-22
                                       1.4374162185238365e-22
                                       -2.1850594973443474e-23
                                       3.426245621809221e-24
                                       -5.531064394246408e-25
                                       9.176601505685995e-26
                                       -1.562287203618025e-26
                                       2.725419375484333e-27
                                       -4.865674910074828e-28
                                       8.879388552723502e-29
                                       -1.6545859180392576e-29
                                       3.1451113213578485e-30
                                       -6.092998312193127e-31
                                       1.2020219393698158e-31
                                       -2.412930801459409e-32)))
      (first$ nil))
  (declare (type (f2cl-lib:integer4) ntk1 ntak1 ntak12)
           (type (double-float) xmin xsml)
           (type (simple-array double-float (16)) bk1cs)
           (type (simple-array double-float (38)) ak1cs)
           (type (simple-array double-float (33)) ak12cs)
           (type f2cl-lib:logical first$))
  (setq first$ f2cl-lib:%true%)
  (defun dbsk1e (x)
    (declare (type (double-float) x))
    (prog ((y 0.0) (dbsk1e 0.0) (eta 0.0f0))
      (declare (type (single-float) eta) (type (double-float) dbsk1e y))
      (cond
        (first$
         (setf eta (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3))))
         (setf ntk1 (initds bk1cs 16 eta))
         (setf ntak1 (initds ak1cs 38 eta))
         (setf ntak12 (initds ak12cs 33 eta))
         (setf xmin
                 (exp
                  (+
                   (max (f2cl-lib:flog (f2cl-lib:d1mach 1))
                        (- (f2cl-lib:flog (f2cl-lib:d1mach 2))))
                   0.01)))
         (setf xsml (f2cl-lib:fsqrt (* 4.0 (f2cl-lib:d1mach 3))))))
      (setf first$ f2cl-lib:%false%)
      (if (<= x 0.0) (xermsg "SLATEC" "DBSK1E" "X IS ZERO OR NEGATIVE" 2 2))
      (if (> x 2.0) (go label20))
      (if (< x xmin) (xermsg "SLATEC" "DBSK1E" "X SO SMALL K1 OVERFLOWS" 3 2))
      (setf y 0.0)
      (if (> x xsml) (setf y (* x x)))
      (setf dbsk1e
              (* (exp x)
                 (+ (* (f2cl-lib:flog (* 0.5 x)) (dbesi1 x))
                    (/ (+ 0.75 (dcsevl (- (* 0.5 y) 1.0) bk1cs ntk1)) x))))
      (go end_label)
     label20
      (if (<= x 8.0)
          (setf dbsk1e
                  (/ (+ 1.25 (dcsevl (/ (- (/ 16.0 x) 5.0) 3.0) ak1cs ntak1))
                     (f2cl-lib:fsqrt x))))
      (if (> x 8.0)
          (setf dbsk1e
                  (/ (+ 1.25 (dcsevl (- (/ 16.0 x) 1.0) ak12cs ntak12))
                     (f2cl-lib:fsqrt x))))
      (go end_label)
     end_label
      (return (values dbsk1e nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dbsk1e
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::dcsevl
                                                     fortran-to-lisp::dbesi1
                                                     fortran-to-lisp::xermsg
                                                     fortran-to-lisp::initds
                                                     fortran-to-lisp::d1mach))))

