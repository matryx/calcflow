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


(let ((ntk0 0)
      (xsml 0.0)
      (xmax 0.0)
      (bk0cs
       (make-array 16
                   :element-type 'double-float
                   :initial-contents '(-0.03532739323390277 0.3442898999246285
                                       0.0359799365153615 0.001264615411446926
                                       2.286212103119452e-5
                                       2.5347910790261494e-7
                                       1.904516377220209e-9
                                       1.0349695257633625e-11
                                       4.2598161427910826e-14
                                       1.3744654358807508e-16
                                       3.5708965285083736e-19
                                       7.631643660116437e-22
                                       1.365424988440782e-24
                                       2.075275266906668e-27
                                       2.7128142180729857e-30
                                       3.0825938879146666e-33)))
      (first$ nil))
  (declare (type (f2cl-lib:integer4) ntk0)
           (type (double-float) xsml xmax)
           (type (simple-array double-float (16)) bk0cs)
           (type f2cl-lib:logical first$))
  (setq first$ f2cl-lib:%true%)
  (defun dbesk0 (x)
    (declare (type (double-float) x))
    (prog ((xmaxt 0.0) (y 0.0) (dbesk0 0.0))
      (declare (type (double-float) dbesk0 y xmaxt))
      (cond
        (first$
         (setf ntk0
                 (initds bk0cs 16
                  (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
         (setf xsml (f2cl-lib:fsqrt (* 4.0 (f2cl-lib:d1mach 3))))
         (setf xmaxt (- (f2cl-lib:flog (f2cl-lib:d1mach 1))))
         (setf xmax
                 (+ xmaxt
                    (/ (* -0.5 xmaxt (f2cl-lib:flog xmaxt)) (+ xmaxt 0.5))))))
      (setf first$ f2cl-lib:%false%)
      (if (<= x 0.0) (xermsg "SLATEC" "DBESK0" "X IS ZERO OR NEGATIVE" 2 2))
      (if (> x 2.0) (go label20))
      (setf y 0.0)
      (if (> x xsml) (setf y (* x x)))
      (setf dbesk0
              (+ (- (* (- (f2cl-lib:flog (* 0.5 x))) (dbesi0 x)) 0.25)
                 (dcsevl (- (* 0.5 y) 1.0) bk0cs ntk0)))
      (go end_label)
     label20
      (setf dbesk0 0.0)
      (if (> x xmax) (xermsg "SLATEC" "DBESK0" "X SO BIG K0 UNDERFLOWS" 1 1))
      (if (> x xmax) (go end_label))
      (setf dbesk0 (* (exp (- x)) (dbsk0e x)))
      (go end_label)
     end_label
      (return (values dbesk0 nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dbesk0
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::dbsk0e
                                                     fortran-to-lisp::dcsevl
                                                     fortran-to-lisp::dbesi0
                                                     fortran-to-lisp::xermsg
                                                     fortran-to-lisp::initds
                                                     fortran-to-lisp::d1mach))))

