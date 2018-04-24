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


(let ((spencs
       (make-array 38
                   :element-type 'double-float
                   :initial-contents '(0.1527365598892406 0.08169658058051014
                                       0.005814157140778731
                                       5.371619814541527e-4
                                       5.724704675185826e-5
                                       6.6745461216493364e-6
                                       8.276467339715677e-7
                                       1.073315673030679e-7
                                       1.4400772943032394e-8
                                       1.9844420299659065e-9
                                       2.7940058221636385e-10
                                       4.0039913108833116e-11
                                       5.8234628920446385e-12
                                       8.576708692638689e-13
                                       1.276862586280193e-13
                                       1.918826209042517e-14
                                       2.907319206977138e-15
                                       4.4371126852767803e-16
                                       6.8157277874146e-17
                                       1.0530173860155745e-17
                                       1.635389806752377e-18
                                       2.551852874940464e-19
                                       3.99902062199936e-20
                                       6.291501645216812e-21
                                       9.933827435675679e-22
                                       1.5736795707499649e-22
                                       2.500595316849476e-23
                                       3.984740918383811e-24
                                       6.366473210082844e-25
                                       1.0196742872396784e-25
                                       1.636881058913519e-26
                                       2.6333104394176502e-27
                                       4.244811560123977e-28
                                       6.855411983680052e-29
                                       1.1091224334380564e-29
                                       1.7974313049998914e-30
                                       2.917505845976095e-31
                                       4.742646808928671e-32)))
      (pi26 1.6449340668482264)
      (nspenc 0)
      (xbig 0.0))
  (declare (type (simple-array double-float (38)) spencs)
           (type (double-float) pi26 xbig)
           (type (f2cl-lib:integer4) nspenc))
  (defun dspenc (x)
    (declare (type (double-float) x))
    (prog ((aln 0.0) (dspenc 0.0))
      (declare (type (double-float) dspenc aln))
      (if (/= nspenc 0) (go label10))
      (setf nspenc
              (initds spencs 38 (* 0.1f0 (f2cl-lib:sngl (f2cl-lib:d1mach 3)))))
      (setf xbig (/ 1.0 (f2cl-lib:d1mach 3)))
     label10
      (if (> x 2.0) (go label60))
      (if (> x 1.0) (go label50))
      (if (> x 0.5) (go label40))
      (if (>= x 0.0) (go label30))
      (if (> x -1.0) (go label20))
      (setf aln (f2cl-lib:dlog (- 1.0 x)))
      (setf dspenc (- (* -0.5 aln (- (* 2.0 (f2cl-lib:dlog (- x))) aln)) pi26))
      (if (> x (- xbig))
          (setf dspenc
                  (+ dspenc
                     (/
                      (+ 1.0 (dcsevl (- (/ 4.0 (- 1.0 x)) 1.0) spencs nspenc))
                      (- 1.0 x)))))
      (go end_label)
     label20
      (setf dspenc
              (+ (* -0.5 (expt (f2cl-lib:dlog (- 1.0 x)) 2))
                 (/
                  (* (- x)
                     (+ 1.0
                        (dcsevl (- (/ (* 4.0 x) (- x 1.0)) 1.0) spencs
                         nspenc)))
                  (- x 1.0))))
      (go end_label)
     label30
      (setf dspenc (* x (+ 1.0 (dcsevl (- (* 4.0 x) 1.0) spencs nspenc))))
      (go end_label)
     label40
      (setf dspenc pi26)
      (if (/= x 1.0)
          (setf dspenc
                  (- pi26
                     (* (f2cl-lib:dlog x) (f2cl-lib:dlog (- 1.0 x)))
                     (* (- 1.0 x)
                        (+ 1.0
                           (dcsevl (- (* 4.0 (- 1.0 x)) 1.0) spencs
                            nspenc))))))
      (go end_label)
     label50
      (setf dspenc
              (+ pi26
                 (* -0.5
                    (f2cl-lib:dlog x)
                    (f2cl-lib:dlog (/ (expt (- x 1.0) 2) x)))
                 (/
                  (* (- x 1.0)
                     (+ 1.0
                        (dcsevl (- (/ (* 4.0 (- x 1.0)) x) 1.0) spencs
                         nspenc)))
                  x)))
      (go end_label)
     label60
      (setf dspenc (- (* 2.0 pi26) (* 0.5 (expt (f2cl-lib:dlog x) 2))))
      (if (< x xbig)
          (setf dspenc
                  (+ dspenc
                     (/ (- (+ 1.0 (dcsevl (- (/ 4.0 x) 1.0) spencs nspenc)))
                        x))))
      (go end_label)
     end_label
      (return (values dspenc nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dspenc
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::dcsevl
                                                     fortran-to-lisp::initds
                                                     fortran-to-lisp::d1mach))))

