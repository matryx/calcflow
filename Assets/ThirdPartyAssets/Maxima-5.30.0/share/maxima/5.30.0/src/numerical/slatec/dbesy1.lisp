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


(let ((nty1 0)
      (xmin 0.0)
      (xsml 0.0)
      (by1cs
       (make-array 20
                   :element-type 'double-float
                   :initial-contents '(0.032080471006119084 1.2627078974335004
                                       0.006499961899923175
                                       -0.08936164528860505
                                       0.013250881221757096
                                       -8.979059119648352e-4
                                       3.647361487958307e-5
                                       -1.0013743816660006e-6
                                       1.994539657390174e-8
                                       -3.023065601803382e-10
                                       3.609878156947812e-12
                                       -3.4874882972875824e-14
                                       2.7838789715591767e-16
                                       -1.8678709686194878e-18
                                       1.0685315339116827e-20
                                       -5.274721956684482e-23
                                       2.2701994031556643e-25
                                       -8.595390353945232e-28
                                       2.8854043798337947e-30
                                       -8.647541138937173e-33)))
      (twodpi 0.6366197723675814)
      (first$ nil))
  (declare (type (f2cl-lib:integer4) nty1)
           (type (double-float) xmin xsml twodpi)
           (type (simple-array double-float (20)) by1cs)
           (type f2cl-lib:logical first$))
  (setq first$ f2cl-lib:%true%)
  (defun dbesy1 (x)
    (declare (type (double-float) x))
    (prog ((ampl 0.0) (theta 0.0) (y 0.0) (dbesy1 0.0))
      (declare (type (double-float) dbesy1 y theta ampl))
      (cond
        (first$
         (setf nty1
                 (initds by1cs 20
                  (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
         (setf xmin
                 (* 1.571
                    (exp
                     (+
                      (max (f2cl-lib:flog (f2cl-lib:d1mach 1))
                           (- (f2cl-lib:flog (f2cl-lib:d1mach 2))))
                      0.01))))
         (setf xsml (f2cl-lib:fsqrt (* 4.0 (f2cl-lib:d1mach 3))))))
      (setf first$ f2cl-lib:%false%)
      (if (<= x 0.0) (xermsg "SLATEC" "DBESY1" "X IS ZERO OR NEGATIVE" 1 2))
      (if (> x 4.0) (go label20))
      (if (< x xmin) (xermsg "SLATEC" "DBESY1" "X SO SMALL Y1 OVERFLOWS" 3 2))
      (setf y 0.0)
      (if (> x xsml) (setf y (* x x)))
      (setf dbesy1
              (+ (* twodpi (f2cl-lib:flog (* 0.5 x)) (dbesj1 x))
                 (/ (+ 0.5 (dcsevl (- (* 0.125 y) 1.0) by1cs nty1)) x)))
      (go end_label)
     label20
      (multiple-value-bind (var-0 var-1 var-2)
          (d9b1mp x ampl theta)
        (declare (ignore var-0))
        (setf ampl var-1)
        (setf theta var-2))
      (setf dbesy1 (* ampl (sin theta)))
      (go end_label)
     end_label
      (return (values dbesy1 nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dbesy1
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::d9b1mp
                                                     fortran-to-lisp::dcsevl
                                                     fortran-to-lisp::dbesj1
                                                     fortran-to-lisp::xermsg
                                                     fortran-to-lisp::initds
                                                     fortran-to-lisp::d1mach))))

