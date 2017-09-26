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


(let ((ngam 0)
      (xmin 0.0)
      (xmax 0.0)
      (dxrel 0.0)
      (gamcs
       (make-array 42
                   :element-type 'double-float
                   :initial-contents '(0.00857119559098933 0.004415381324841007
                                       0.05685043681599363 -0.00421983539641856
                                       0.0013268081812124603
                                       -1.8930245297988805e-4
                                       3.606925327441245e-5
                                       -6.056761904460864e-6
                                       1.0558295463022833e-6
                                       -1.811967365542384e-7
                                       3.117724964715322e-8
                                       -5.354219639019687e-9
                                       9.193275519859589e-10
                                       -1.5779412802883398e-10
                                       2.7079806229349544e-11
                                       -4.64681865382573e-12
                                       7.97335019200742e-13
                                       -1.368078209830916e-13
                                       2.3473194865638007e-14
                                       -4.027432614949067e-15
                                       6.910051747372101e-16
                                       -1.185584500221993e-16
                                       2.034148542496374e-17
                                       -3.490054341717406e-18
                                       5.987993856485306e-19
                                       -1.027378057872228e-19
                                       1.7627028160605298e-20
                                       -3.024320653735306e-21
                                       5.188914660218398e-22
                                       -8.902770842456576e-23
                                       1.5274740684933426e-23
                                       -2.620731256187363e-24
                                       4.496464047830539e-25
                                       -7.714712731336878e-26
                                       1.323635453126044e-26
                                       -2.2709994129429287e-27
                                       3.8964189980039913e-28
                                       -6.685198115125953e-29
                                       1.1469986631400244e-29
                                       -1.9679385863451348e-30
                                       3.376448816585338e-31
                                       -5.793070335782136e-32)))
      (pi$ 3.141592653589793)
      (sq2pil 0.9189385332046728)
      (first$ nil))
  (declare (type (f2cl-lib:integer4) ngam)
           (type (double-float) xmin xmax dxrel pi$ sq2pil)
           (type (simple-array double-float (42)) gamcs)
           (type f2cl-lib:logical first$))
  (setq first$ f2cl-lib:%true%)
  (defun dgamma (x)
    (declare (type (double-float) x))
    (prog ((sinpiy 0.0) (y 0.0) (dgamma 0.0) (i 0) (n 0))
      (declare (type (f2cl-lib:integer4) n i)
               (type (double-float) dgamma y sinpiy))
      (cond
        (first$
         (setf ngam
                 (initds gamcs 42
                  (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
         (multiple-value-bind (var-0 var-1)
             (dgamlm xmin xmax)
           (declare (ignore))
           (setf xmin var-0)
           (setf xmax var-1))
         (setf dxrel (f2cl-lib:fsqrt (f2cl-lib:d1mach 4)))))
      (setf first$ f2cl-lib:%false%)
      (setf y (abs x))
      (if (> y 10.0) (go label50))
      (setf n (f2cl-lib:int x))
      (if (< x 0.0) (setf n (f2cl-lib:int-sub n 1)))
      (setf y (- x n))
      (setf n (f2cl-lib:int-sub n 1))
      (setf dgamma (+ 0.9375 (dcsevl (- (* 2.0 y) 1.0) gamcs ngam)))
      (if (= n 0) (go end_label))
      (if (> n 0) (go label30))
      (setf n (f2cl-lib:int-sub n))
      (if (= x 0.0) (xermsg "SLATEC" "DGAMMA" "X IS 0" 4 2))
      (if (and (< x 0.0f0) (= (- (+ x n) 2) 0.0))
          (xermsg "SLATEC" "DGAMMA" "X IS A NEGATIVE INTEGER" 4 2))
      (if
       (and (< x -0.5) (< (abs (/ (- x (f2cl-lib:aint (- x 0.5))) x)) dxrel))
       (xermsg "SLATEC" "DGAMMA"
        "ANSWER LT HALF PRECISION BECAUSE X TOO NEAR NEGATIVE INTEGER" 1 1))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody (setf dgamma (/ dgamma (- (+ x i) 1))) label20))
      (go end_label)
     label30
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody (setf dgamma (* (+ y i) dgamma)) label40))
      (go end_label)
     label50
      (if (> x xmax) (xermsg "SLATEC" "DGAMMA" "X SO BIG GAMMA OVERFLOWS" 3 2))
      (setf dgamma 0.0)
      (if (< x xmin)
          (xermsg "SLATEC" "DGAMMA" "X SO SMALL GAMMA UNDERFLOWS" 2 1))
      (if (< x xmin) (go end_label))
      (setf dgamma
              (exp
               (+ (- (* (- y 0.5) (f2cl-lib:flog y)) y) sq2pil (d9lgmc y))))
      (if (> x 0.0) (go end_label))
      (if (< (abs (/ (- x (f2cl-lib:aint (- x 0.5))) x)) dxrel)
          (xermsg "SLATEC" "DGAMMA"
           "ANSWER LT HALF PRECISION, X TOO NEAR NEGATIVE INTEGER" 1 1))
      (setf sinpiy (sin (* pi$ y)))
      (if (= sinpiy 0.0)
          (xermsg "SLATEC" "DGAMMA" "X IS A NEGATIVE INTEGER" 4 2))
      (setf dgamma (/ (- pi$) (* y sinpiy dgamma)))
      (go end_label)
     end_label
      (return (values dgamma nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgamma
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::d9lgmc
                                                     fortran-to-lisp::xermsg
                                                     fortran-to-lisp::dcsevl
                                                     fortran-to-lisp::dgamlm
                                                     fortran-to-lisp::initds
                                                     fortran-to-lisp::d1mach))))

