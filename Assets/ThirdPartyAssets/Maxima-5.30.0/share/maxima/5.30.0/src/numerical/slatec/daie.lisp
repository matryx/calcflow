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


(let ((naif 0)
      (naig 0)
      (naip1 0)
      (naip2 0)
      (x3sml 0.0)
      (x32sml 0.0)
      (xbig 0.0)
      (aifcs
       (make-array 13
                   :element-type 'double-float
                   :initial-contents '(-0.03797135849667 0.05919188853726364
                                       9.862928057727998e-4
                                       6.848843819076567e-6
                                       2.5942025962194713e-8
                                       6.176612774081375e-11
                                       1.0092454172466118e-13
                                       1.2014792511179938e-16
                                       1.0882945588716992e-19
                                       7.751377219668488e-23
                                       4.4548112037175636e-26
                                       2.1092845231692343e-29
                                       8.370173591074134e-33)))
      (aigcs
       (make-array 13
                   :element-type 'double-float
                   :initial-contents '(0.018152365581161272
                                       0.021572563166010757
                                       2.567835698748325e-4
                                       1.4265214119792405e-6
                                       4.572114920018043e-9
                                       9.52517084356471e-12
                                       1.3925634605771398e-14
                                       1.5070999142762378e-17
                                       1.2559148312567778e-20
                                       8.306307377082133e-24
                                       4.465753849371857e-27
                                       1.9900855034518868e-30
                                       7.4702885256533335e-34)))
      (aip1cs
       (make-array 57
                   :element-type 'double-float
                   :initial-contents '(-0.021469518589105386
                                       -0.0075353825350433015
                                       5.971527949026381e-4
                                       -7.28325125420761e-5
                                       1.1102971307392997e-5
                                       -1.950386152284406e-6
                                       3.786973885159515e-7
                                       -7.929675297350979e-8
                                       1.762247638674256e-8
                                       -4.110767539667195e-9
                                       9.984770057857892e-10
                                       -2.5100932513871223e-10
                                       6.500501929860696e-11
                                       -1.7278184053936166e-11
                                       4.6993788428245126e-12
                                       -1.304675656297744e-12
                                       3.6896984784626787e-13
                                       -1.0610872066468062e-13
                                       3.0984143848781874e-14
                                       -9.17490807982414e-15
                                       2.7520491403472108e-15
                                       -8.353750115922047e-16
                                       2.563931129357935e-16
                                       -7.950633762598855e-17
                                       2.48928363460307e-17
                                       -7.864326933928736e-18
                                       2.5056873114399757e-18
                                       -8.047420364163909e-19
                                       2.604097118952054e-19
                                       -8.486954164056412e-20
                                       2.784706882142338e-20
                                       -9.195858953498614e-21
                                       3.055304318374239e-21
                                       -1.0210354554794778e-21
                                       3.431118190743758e-22
                                       -1.1591293417977495e-22
                                       3.935772844200256e-23
                                       -1.3428809802967176e-23
                                       4.6032878835200026e-24
                                       -1.5850439270040642e-24
                                       5.481275667729676e-25
                                       -1.9033493718550473e-25
                                       6.635682302374009e-26
                                       -2.3223116500263143e-26
                                       8.157640113429179e-27
                                       -2.8758242406329004e-27
                                       1.0173294509429014e-27
                                       -3.6108791087422165e-28
                                       1.2857885403639934e-28
                                       -4.5929010373785476e-29
                                       1.6455970338207138e-29
                                       -5.913421299843502e-30
                                       2.131057006604993e-30
                                       -7.701158157787598e-31
                                       2.7905333079689304e-31
                                       -1.013807715111284e-31
                                       3.692580158719624e-32)))
      (aip2cs
       (make-array 37
                   :element-type 'double-float
                   :initial-contents '(-0.0017431449692937551
                                       -0.0016789385432554166
                                       3.5965340335216605e-5
                                       -1.3808186027392284e-6
                                       7.411228077315053e-8
                                       -5.00238203900133e-9
                                       4.0069391741718425e-10
                                       -3.6733124279590504e-11
                                       3.760344395923738e-12
                                       -4.2232133271874755e-13
                                       5.135094540336571e-14
                                       -6.690958503904776e-15
                                       9.266675456412906e-16
                                       -1.3551438241607058e-16
                                       2.0811549631283098e-17
                                       -3.3411649915917686e-18
                                       5.5857858458592435e-19
                                       -9.692190401523652e-20
                                       1.740457001288932e-20
                                       -3.226409797311304e-21
                                       6.160744711066252e-22
                                       -1.2093634798249005e-22
                                       2.436327633101381e-23
                                       -5.029142214974575e-24
                                       1.062241755436357e-24
                                       -2.2928428489598924e-25
                                       5.051817339295037e-26
                                       -1.134981237144124e-26
                                       2.5976556598560697e-27
                                       -6.051246215429395e-28
                                       1.4335977796677281e-28
                                       -3.4514775706089996e-29
                                       8.438751902136468e-30
                                       -2.0939614229818816e-30
                                       5.270088734789455e-31
                                       -1.3445743301455338e-31
                                       3.475709645266011e-32)))
      (first$ nil))
  (declare (type (f2cl-lib:integer4) naif naig naip1 naip2)
           (type (double-float) x3sml x32sml xbig)
           (type (simple-array double-float (13)) aifcs aigcs)
           (type (simple-array double-float (57)) aip1cs)
           (type (simple-array double-float (37)) aip2cs)
           (type f2cl-lib:logical first$))
  (setq first$ f2cl-lib:%true%)
  (defun daie (x)
    (declare (type (double-float) x))
    (prog ((sqrtx 0.0) (theta 0.0) (xm 0.0) (z 0.0) (daie 0.0) (eta 0.0f0))
      (declare (type (single-float) eta)
               (type (double-float) daie z xm theta sqrtx))
      (cond
        (first$
         (setf eta (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3))))
         (setf naif (initds aifcs 13 eta))
         (setf naig (initds aigcs 13 eta))
         (setf naip1 (initds aip1cs 57 eta))
         (setf naip2 (initds aip2cs 37 eta))
         (setf x3sml (coerce (expt eta 0.3333f0) 'double-float))
         (setf x32sml (* 1.3104 (expt x3sml 2)))
         (setf xbig (expt (f2cl-lib:d1mach 2) 0.6666))))
      (setf first$ f2cl-lib:%false%)
      (if (>= x -1.0) (go label20))
      (multiple-value-bind (var-0 var-1 var-2)
          (d9aimp x xm theta)
        (declare (ignore var-0))
        (setf xm var-1)
        (setf theta var-2))
      (setf daie (* xm (cos theta)))
      (go end_label)
     label20
      (if (> x 1.0) (go label30))
      (setf z 0.0)
      (if (> (abs x) x3sml) (setf z (expt x 3)))
      (setf daie
              (+ 0.375
                 (- (dcsevl z aifcs naif)
                    (* x (+ 0.25 (dcsevl z aigcs naig))))))
      (if (> x x32sml)
          (setf daie (* daie (exp (/ (* 2.0 x (f2cl-lib:fsqrt x)) 3.0)))))
      (go end_label)
     label30
      (if (> x 4.0) (go label40))
      (setf sqrtx (f2cl-lib:fsqrt x))
      (setf z (/ (- (/ 16.0 (* x sqrtx)) 9.0) 7.0))
      (setf daie
              (/ (+ 0.28125 (dcsevl z aip1cs naip1)) (f2cl-lib:fsqrt sqrtx)))
      (go end_label)
     label40
      (setf sqrtx (f2cl-lib:fsqrt x))
      (setf z -1.0)
      (if (< x xbig) (setf z (- (/ 16.0 (* x sqrtx)) 1.0)))
      (setf daie
              (/ (+ 0.28125 (dcsevl z aip2cs naip2)) (f2cl-lib:fsqrt sqrtx)))
      (go end_label)
     end_label
      (return (values daie nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::daie fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::dcsevl
                                                     fortran-to-lisp::d9aimp
                                                     fortran-to-lisp::initds
                                                     fortran-to-lisp::d1mach))))

