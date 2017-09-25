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


(let ((nterf 0)
      (nterfc 0)
      (nterc2 0)
      (xsml 0.0)
      (xmax 0.0)
      (sqeps 0.0)
      (erfcs
       (make-array 21
                   :element-type 'double-float
                   :initial-contents '(-0.049046121234691806
                                       -0.14226120510371365
                                       0.010035582187599796
                                       -5.768764699767485e-4
                                       2.741993125219606e-5
                                       -1.1043175507344507e-6
                                       3.8488755420345036e-8
                                       -1.1808582533875466e-9
                                       3.2334215826050907e-11
                                       -7.991015947004549e-13
                                       1.7990725113961456e-14
                                       -3.718635487818693e-16
                                       7.103599003714253e-18
                                       -1.2612455119155226e-19
                                       2.0916406941769294e-21
                                       -3.2539731029314073e-23
                                       4.766867209797675e-25
                                       -6.598012078285134e-27
                                       8.655011469963763e-29
                                       -1.0788925177498064e-30
                                       1.2811883993017003e-32)))
      (erc2cs
       (make-array 49
                   :element-type 'double-float
                   :initial-contents '(-0.0696013466023095 -0.04110133936262089
                                       0.003914495866689627
                                       -4.906395650548979e-4
                                       7.157479001377036e-5
                                       -1.1530716341312328e-5
                                       1.9946705902019974e-6
                                       -3.642666471599223e-7
                                       6.944372610005012e-8
                                       -1.371220902104366e-8
                                       2.7883896610071373e-9
                                       -5.814164724331161e-10
                                       1.2389204917527532e-10
                                       -2.6906391453067435e-11
                                       5.942614350847911e-12
                                       -1.3323867357581197e-12
                                       3.0280468061771323e-13
                                       -6.966648814941033e-14
                                       1.620854541053923e-14
                                       -3.809934465250492e-15
                                       9.040487815978831e-16
                                       -2.1640061950896072e-16
                                       5.222102233995855e-17
                                       -1.2697296023645554e-17
                                       3.1091455042761977e-18
                                       -7.663762920320386e-19
                                       1.9008192513627452e-19
                                       -4.7422072790690393e-20
                                       1.1896492000765284e-20
                                       -3.0000355903257804e-21
                                       7.602993453043246e-22
                                       -1.9359094476068728e-22
                                       4.951399124773338e-23
                                       -1.2718074813363719e-23
                                       3.2800496004695132e-24
                                       -8.492320176822897e-25
                                       2.2069178928075603e-25
                                       -5.755617245696529e-26
                                       1.5061915336392342e-26
                                       -3.954502959018797e-27
                                       1.0415297041515009e-27
                                       -2.751487795278765e-28
                                       7.290058205497557e-29
                                       -1.936939645915948e-29
                                       5.1603571120514875e-30
                                       -1.3784193221930942e-30
                                       3.691326793107069e-31
                                       -9.909389590624365e-32
                                       2.666491705195388e-32)))
      (erfccs
       (make-array 59
                   :element-type 'double-float
                   :initial-contents '(0.07151793102029248
                                       -0.026532434337606717
                                       0.0017111539779208558
                                       -1.6375166345851787e-4
                                       1.9871293500552038e-5
                                       -2.843712412766555e-6
                                       4.6061613089631305e-7
                                       -8.227753025879209e-8
                                       1.5921418727709012e-8
                                       -3.295071362252843e-9
                                       7.223439760400556e-10
                                       -1.6648558133987297e-10
                                       4.010392588237665e-11
                                       -1.004816214425731e-11
                                       2.608275913300334e-12
                                       -6.991110560404025e-13
                                       1.9294923332617072e-13
                                       -5.470131188754331e-14
                                       1.5896633097626975e-14
                                       -4.726893980197555e-15
                                       1.4358733767849847e-15
                                       -4.449510561817358e-16
                                       1.4048108847682335e-16
                                       -4.5138183877642106e-17
                                       1.474521541045133e-17
                                       -4.8926214069457765e-18
                                       1.6476121414106467e-18
                                       -5.626817176329408e-19
                                       1.9474433822320786e-19
                                       -6.82630564294842e-20
                                       2.4219888872986492e-20
                                       -8.693414133503071e-21
                                       3.1551803462280855e-21
                                       -1.1573723240496087e-21
                                       4.288947161605654e-22
                                       -1.6050307420576167e-22
                                       6.063298757453803e-23
                                       -2.3114042516979585e-23
                                       8.888778540661885e-24
                                       -3.447260576651376e-24
                                       1.347865460206965e-24
                                       -5.311794071125021e-25
                                       2.109341058619783e-25
                                       -8.438365587923789e-26
                                       3.399982524945209e-26
                                       -1.3794523880732422e-26
                                       5.6344903118332525e-27
                                       -2.3164904344770655e-27
                                       9.58446284460181e-28
                                       -3.9907228803301096e-28
                                       1.6721292259444773e-28
                                       -7.045991522766014e-29
                                       2.9797684028642063e-29
                                       -1.2625224664606192e-29
                                       5.395438704542488e-30
                                       -2.380992882531459e-30
                                       1.0990528301027615e-30
                                       -4.867713741644966e-31
                                       1.5258772641103575e-31)))
      (sqrtpi 1.772453850905516)
      (first$ nil))
  (declare (type (f2cl-lib:integer4) nterf nterfc nterc2)
           (type (double-float) xsml xmax sqeps sqrtpi)
           (type (simple-array double-float (21)) erfcs)
           (type (simple-array double-float (49)) erc2cs)
           (type (simple-array double-float (59)) erfccs)
           (type f2cl-lib:logical first$))
  (setq first$ f2cl-lib:%true%)
  (defun derfc (x)
    (declare (type (double-float) x))
    (prog ((txmax 0.0) (y 0.0) (derfc 0.0) (eta 0.0f0))
      (declare (type (single-float) eta) (type (double-float) derfc y txmax))
      (cond
        (first$
         (setf eta (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3))))
         (setf nterf (initds erfcs 21 eta))
         (setf nterfc (initds erfccs 59 eta))
         (setf nterc2 (initds erc2cs 49 eta))
         (setf xsml
                 (-
                  (f2cl-lib:fsqrt
                   (- (f2cl-lib:flog (* sqrtpi (f2cl-lib:d1mach 3)))))))
         (setf txmax
                 (f2cl-lib:fsqrt
                  (- (f2cl-lib:flog (* sqrtpi (f2cl-lib:d1mach 1))))))
         (setf xmax (- (+ txmax (/ (* -0.5 (f2cl-lib:flog txmax)) txmax)) 0.01))
         (setf sqeps (f2cl-lib:fsqrt (* 2.0 (f2cl-lib:d1mach 3))))))
      (setf first$ f2cl-lib:%false%)
      (if (> x xsml) (go label20))
      (setf derfc 2.0)
      (go end_label)
     label20
      (if (> x xmax) (go label40))
      (setf y (abs x))
      (if (> y 1.0) (go label30))
      (if (< y sqeps) (setf derfc (+ 1.0 (/ (* -2.0 x) sqrtpi))))
      (if (>= y sqeps)
          (setf derfc
                  (- 1.0
                     (* x (+ 1.0 (dcsevl (- (* 2.0 x x) 1.0) erfcs nterf))))))
      (go end_label)
     label30
      (setf y (* y y))
      (if (<= y 4.0)
          (setf derfc
                  (* (/ (exp (- y)) (abs x))
                     (+ 0.5
                        (dcsevl (/ (- (/ 8.0 y) 5.0) 3.0) erc2cs nterc2)))))
      (if (> y 4.0)
          (setf derfc
                  (* (/ (exp (- y)) (abs x))
                     (+ 0.5 (dcsevl (- (/ 8.0 y) 1.0) erfccs nterfc)))))
      (if (< x 0.0) (setf derfc (- 2.0 derfc)))
      (go end_label)
     label40
      (xermsg "SLATEC" "DERFC" "X SO BIG ERFC UNDERFLOWS" 1 1)
      (setf derfc 0.0)
      (go end_label)
     end_label
      (return (values derfc nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::derfc fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::xermsg
                                                     fortran-to-lisp::dcsevl
                                                     fortran-to-lisp::initds
                                                     fortran-to-lisp::d1mach))))

