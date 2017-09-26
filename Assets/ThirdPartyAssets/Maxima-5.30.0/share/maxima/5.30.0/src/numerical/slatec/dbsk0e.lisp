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
      (ntak0 0)
      (ntak02 0)
      (xsml 0.0)
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
      (ak0cs
       (make-array 38
                   :element-type 'double-float
                   :initial-contents '(-0.07643947903327941
                                       -0.02235652605699819
                                       7.734181154693858e-4
                                       -4.281006688886099e-5
                                       3.0817001738629747e-6
                                       -2.639367222009665e-7
                                       2.563713036403469e-8
                                       -2.7427055499002012e-9
                                       3.1694296580974997e-10
                                       -3.902353286962184e-11
                                       5.068040698188575e-12
                                       -6.889574741007871e-13
                                       9.744978497825918e-14
                                       -1.4273328418845485e-14
                                       2.156412571021463e-15
                                       -3.3496542551495625e-16
                                       5.3352602169529114e-17
                                       -8.693669980890753e-18
                                       1.4464043478622123e-18
                                       -2.4528898255001297e-19
                                       4.2337545262321717e-20
                                       -7.427946526454465e-21
                                       1.3231505293926669e-21
                                       -2.3905871647396495e-22
                                       4.376827585923226e-23
                                       -8.113700607345117e-24
                                       1.521819913832173e-24
                                       -2.886041941483398e-25
                                       5.530620667054718e-26
                                       -1.0703773292498988e-26
                                       2.0910868931423843e-27
                                       -4.121713723646204e-28
                                       8.193483971121308e-29
                                       -1.6420002754592977e-29
                                       3.3161432814802274e-30
                                       -6.746863644145296e-31
                                       1.3824291463184248e-31
                                       -2.8518741673598326e-32)))
      (ak02cs
       (make-array 33
                   :element-type 'double-float
                   :initial-contents '(-0.012018698263075922
                                       -0.009174852691025696
                                       1.4445509317750059e-4
                                       -4.01361417543571e-6
                                       1.5678318108523108e-7
                                       -7.770110438521738e-9
                                       4.6111825761797177e-10
                                       -3.158592997860566e-11
                                       2.435018039365041e-12
                                       -2.0743313873983479e-13
                                       1.925787280589917e-14
                                       -1.927554805838956e-15
                                       2.0621980291978182e-16
                                       -2.3416851175792425e-17
                                       2.8059028106430423e-18
                                       -3.530507631161808e-19
                                       4.645295422935108e-20
                                       -6.368625941344267e-21
                                       9.069521310986516e-22
                                       -1.3379747854236907e-22
                                       2.0398360218599522e-23
                                       -3.2070274813678404e-24
                                       5.18974441366231e-25
                                       -8.629501497540573e-26
                                       1.47216118310256e-26
                                       -2.5730690238670112e-27
                                       4.601774086643516e-28
                                       -8.411555324201094e-29
                                       1.569806306635369e-29
                                       -2.988226453005758e-30
                                       5.7968313752168365e-31
                                       -1.1450359943476814e-31
                                       2.3012665942496828e-32)))
      (first$ nil))
  (declare (type (f2cl-lib:integer4) ntk0 ntak0 ntak02)
           (type (double-float) xsml)
           (type (simple-array double-float (16)) bk0cs)
           (type (simple-array double-float (38)) ak0cs)
           (type (simple-array double-float (33)) ak02cs)
           (type f2cl-lib:logical first$))
  (setq first$ f2cl-lib:%true%)
  (defun dbsk0e (x)
    (declare (type (double-float) x))
    (prog ((y 0.0) (dbsk0e 0.0) (eta 0.0f0))
      (declare (type (single-float) eta) (type (double-float) dbsk0e y))
      (cond
        (first$
         (setf eta (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3))))
         (setf ntk0 (initds bk0cs 16 eta))
         (setf ntak0 (initds ak0cs 38 eta))
         (setf ntak02 (initds ak02cs 33 eta))
         (setf xsml (f2cl-lib:fsqrt (* 4.0 (f2cl-lib:d1mach 3))))))
      (setf first$ f2cl-lib:%false%)
      (if (<= x 0.0) (xermsg "SLATEC" "DBSK0E" "X IS ZERO OR NEGATIVE" 2 2))
      (if (> x 2.0) (go label20))
      (setf y 0.0)
      (if (> x xsml) (setf y (* x x)))
      (setf dbsk0e
              (* (exp x)
                 (+ (- (* (- (f2cl-lib:flog (* 0.5 x))) (dbesi0 x)) 0.25)
                    (dcsevl (- (* 0.5 y) 1.0) bk0cs ntk0))))
      (go end_label)
     label20
      (if (<= x 8.0)
          (setf dbsk0e
                  (/ (+ 1.25 (dcsevl (/ (- (/ 16.0 x) 5.0) 3.0) ak0cs ntak0))
                     (f2cl-lib:fsqrt x))))
      (if (> x 8.0)
          (setf dbsk0e
                  (/ (+ 1.25 (dcsevl (- (/ 16.0 x) 1.0) ak02cs ntak02))
                     (f2cl-lib:fsqrt x))))
      (go end_label)
     end_label
      (return (values dbsk0e nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dbsk0e
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::dcsevl
                                                     fortran-to-lisp::dbesi0
                                                     fortran-to-lisp::xermsg
                                                     fortran-to-lisp::initds
                                                     fortran-to-lisp::d1mach))))

