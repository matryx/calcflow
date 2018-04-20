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
      (xmin 0.0)
      (xsml 0.0)
      (xmax 0.0)
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
      (first$ nil))
  (declare (type (f2cl-lib:integer4) ntk1)
           (type (double-float) xmin xsml xmax)
           (type (simple-array double-float (16)) bk1cs)
           (type f2cl-lib:logical first$))
  (setq first$ f2cl-lib:%true%)
  (defun dbesk1 (x)
    (declare (type (double-float) x))
    (prog ((xmaxt 0.0) (y 0.0) (dbesk1 0.0))
      (declare (type (double-float) dbesk1 y xmaxt))
      (cond
        (first$
         (setf ntk1
                 (initds bk1cs 16
                  (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
         (setf xmin
                 (exp
                  (+
                   (max (f2cl-lib:flog (f2cl-lib:d1mach 1))
                        (- (f2cl-lib:flog (f2cl-lib:d1mach 2))))
                   0.01)))
         (setf xsml (f2cl-lib:fsqrt (* 4.0 (f2cl-lib:d1mach 3))))
         (setf xmaxt (- (f2cl-lib:flog (f2cl-lib:d1mach 1))))
         (setf xmax
                 (+ xmaxt
                    (/ (* -0.5 xmaxt (f2cl-lib:flog xmaxt)) (+ xmaxt 0.5))))))
      (setf first$ f2cl-lib:%false%)
      (if (<= x 0.0) (xermsg "SLATEC" "DBESK1" "X IS ZERO OR NEGATIVE" 2 2))
      (if (> x 2.0) (go label20))
      (if (< x xmin) (xermsg "SLATEC" "DBESK1" "X SO SMALL K1 OVERFLOWS" 3 2))
      (setf y 0.0)
      (if (> x xsml) (setf y (* x x)))
      (setf dbesk1
              (+ (* (f2cl-lib:flog (* 0.5 x)) (dbesi1 x))
                 (/ (+ 0.75 (dcsevl (- (* 0.5 y) 1.0) bk1cs ntk1)) x)))
      (go end_label)
     label20
      (setf dbesk1 0.0)
      (if (> x xmax) (xermsg "SLATEC" "DBESK1" "X SO BIG K1 UNDERFLOWS" 1 1))
      (if (> x xmax) (go end_label))
      (setf dbesk1 (* (exp (- x)) (dbsk1e x)))
      (go end_label)
     end_label
      (return (values dbesk1 nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dbesk1
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::dbsk1e
                                                     fortran-to-lisp::dcsevl
                                                     fortran-to-lisp::dbesi1
                                                     fortran-to-lisp::xermsg
                                                     fortran-to-lisp::initds
                                                     fortran-to-lisp::d1mach))))

