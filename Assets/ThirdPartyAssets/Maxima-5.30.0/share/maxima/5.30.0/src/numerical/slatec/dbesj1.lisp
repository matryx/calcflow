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


(let ((ntj1 0)
      (xsml 0.0)
      (xmin 0.0)
      (bj1cs
       (make-array 19
                   :element-type 'double-float
                   :initial-contents '(-0.11726141513332787 -0.2536152183079064
                                       0.050127080984469566
                                       -0.004631514809625082
                                       2.47996229415914e-4
                                       -8.678948686278825e-6
                                       2.1429391714379368e-7
                                       -3.93609307918318e-9
                                       5.59118231794688e-11
                                       -6.327616404661393e-13
                                       5.840991610857247e-15
                                       -4.4825338187012584e-17
                                       2.9053844926250247e-19
                                       -1.6117321978414417e-21
                                       7.739478819392746e-24
                                       -3.2486937821119987e-26
                                       1.2022376772274103e-28
                                       -3.952012212651349e-31
                                       1.1616780822664534e-33)))
      (first$ nil))
  (declare (type (f2cl-lib:integer4) ntj1)
           (type (double-float) xsml xmin)
           (type (simple-array double-float (19)) bj1cs)
           (type f2cl-lib:logical first$))
  (setq first$ f2cl-lib:%true%)
  (defun dbesj1 (x)
    (declare (type (double-float) x))
    (prog ((ampl 0.0) (theta 0.0) (y 0.0) (dbesj1 0.0))
      (declare (type (double-float) dbesj1 y theta ampl))
      (cond
        (first$
         (setf ntj1
                 (initds bj1cs 19
                  (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
         (setf xsml (f2cl-lib:fsqrt (* 8.0 (f2cl-lib:d1mach 3))))
         (setf xmin (* 2.0 (f2cl-lib:d1mach 1)))))
      (setf first$ f2cl-lib:%false%)
      (setf y (abs x))
      (if (> y 4.0) (go label20))
      (setf dbesj1 0.0)
      (if (= y 0.0) (go end_label))
      (if (<= y xmin)
          (xermsg "SLATEC" "DBESJ1" "ABS(X) SO SMALL J1 UNDERFLOWS" 1 1))
      (if (> y xmin) (setf dbesj1 (* 0.5 x)))
      (if (> y xsml)
          (setf dbesj1
                  (* x (+ 0.25 (dcsevl (- (* 0.125 y y) 1.0) bj1cs ntj1)))))
      (go end_label)
     label20
      (multiple-value-bind (var-0 var-1 var-2)
          (d9b1mp y ampl theta)
        (declare (ignore var-0))
        (setf ampl var-1)
        (setf theta var-2))
      (setf dbesj1 (* (f2cl-lib:sign ampl x) (cos theta)))
      (go end_label)
     end_label
      (return (values dbesj1 nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dbesj1
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::d9b1mp
                                                     fortran-to-lisp::dcsevl
                                                     fortran-to-lisp::xermsg
                                                     fortran-to-lisp::initds
                                                     fortran-to-lisp::d1mach))))

