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


(let ((nbif 0)
      (nbig 0)
      (nbif2 0)
      (nbig2 0)
      (x3sml 0.0)
      (xmax 0.0)
      (bifcs
       (make-array 13
                   :element-type 'double-float
                   :initial-contents '(-0.01673021647198665 0.10252335834249446
                                       0.0017083092507381517
                                       1.1862545467744682e-5
                                       4.4932907017792135e-8
                                       1.0698207143387889e-10
                                       1.7480643399771825e-13
                                       2.081023107176171e-16
                                       1.8849814695665417e-19
                                       1.3425779173097804e-22
                                       7.715959342965888e-26
                                       3.653387961747857e-29
                                       1.4497565927953065e-32)))
      (bigcs
       (make-array 13
                   :element-type 'double-float
                   :initial-contents '(0.022466223248574523 0.03736477545301955
                                       4.4476218957212283e-4
                                       2.4708075636329383e-6
                                       7.919135339514964e-9
                                       1.649807985182778e-11
                                       2.4119906664835456e-14
                                       2.6103736236091437e-17
                                       2.1753082977160324e-20
                                       1.4386946400390432e-23
                                       7.734912561208347e-27
                                       3.4469292033849e-30
                                       1.2938919273216e-33)))
      (bif2cs
       (make-array 15
                   :element-type 'double-float
                   :initial-contents '(0.0998457269381604 0.47862497786300556
                                       0.02515521196043301 5.820693885232646e-4
                                       7.499765964437787e-6
                                       6.134602870349384e-8
                                       3.462753885148063e-10
                                       1.4288910080270254e-12
                                       4.496270429833464e-15
                                       1.1142323065833012e-17
                                       2.2304791066175003e-20
                                       3.6815778736393144e-23
                                       5.096086844933826e-26
                                       6.000338692628856e-29
                                       6.082749744657067e-32)))
      (big2cs
       (make-array 15
                   :element-type 'double-float
                   :initial-contents '(0.03330566214551434 0.16130921512319707
                                       0.006319007309613428
                                       1.1879045681625174e-4
                                       1.3045345886200265e-6
                                       9.374125995535217e-9
                                       4.745801886747251e-11
                                       1.783107265094814e-13
                                       5.167591927849581e-16
                                       1.1900450838682712e-18
                                       2.229828806664035e-21
                                       3.465519230276894e-24
                                       4.539263363205045e-27
                                       5.078849965135223e-30
                                       4.910206746965333e-33)))
      (first$ nil))
  (declare (type (f2cl-lib:integer4) nbif nbig nbif2 nbig2)
           (type (double-float) x3sml xmax)
           (type (simple-array double-float (13)) bifcs bigcs)
           (type (simple-array double-float (15)) bif2cs big2cs)
           (type f2cl-lib:logical first$))
  (setq first$ f2cl-lib:%true%)
  (defun dbi (x)
    (declare (type (double-float) x))
    (prog ((theta 0.0) (xm 0.0) (z 0.0) (dbi 0.0) (eta 0.0f0))
      (declare (type (single-float) eta) (type (double-float) dbi z xm theta))
      (cond
        (first$
         (setf eta (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3))))
         (setf nbif (initds bifcs 13 eta))
         (setf nbig (initds bigcs 13 eta))
         (setf nbif2 (initds bif2cs 15 eta))
         (setf nbig2 (initds big2cs 15 eta))
         (setf x3sml (coerce (expt eta 0.3333f0) 'double-float))
         (setf xmax
                 (expt (* 1.5f0 (f2cl-lib:flog (f2cl-lib:d1mach 2))) 0.6666))))
      (setf first$ f2cl-lib:%false%)
      (if (>= x -1.0) (go label20))
      (multiple-value-bind (var-0 var-1 var-2)
          (d9aimp x xm theta)
        (declare (ignore var-0))
        (setf xm var-1)
        (setf theta var-2))
      (setf dbi (* xm (sin theta)))
      (go end_label)
     label20
      (if (> x 1.0) (go label30))
      (setf z 0.0)
      (if (> (abs x) x3sml) (setf z (expt x 3)))
      (setf dbi
              (+ 0.625
                 (dcsevl z bifcs nbif)
                 (* x (+ 0.4375 (dcsevl z bigcs nbig)))))
      (go end_label)
     label30
      (if (> x 2.0) (go label40))
      (setf z (/ (- (* 2.0 (expt x 3)) 9.0) 7.0))
      (setf dbi
              (+ 1.125
                 (dcsevl z bif2cs nbif2)
                 (* x (+ 0.625 (dcsevl z big2cs nbig2)))))
      (go end_label)
     label40
      (if (> x xmax) (xermsg "SLATEC" "DBI" "X SO BIG THAT BI OVERFLOWS" 1 2))
      (setf dbi (* (dbie x) (exp (/ (* 2.0 x (f2cl-lib:fsqrt x)) 3.0))))
      (go end_label)
     end_label
      (return (values dbi nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dbi fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::dbie
                                                     fortran-to-lisp::xermsg
                                                     fortran-to-lisp::dcsevl
                                                     fortran-to-lisp::d9aimp
                                                     fortran-to-lisp::initds
                                                     fortran-to-lisp::d1mach))))

