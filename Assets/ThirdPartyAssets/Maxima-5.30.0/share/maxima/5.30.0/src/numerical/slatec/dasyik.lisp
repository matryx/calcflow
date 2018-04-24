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
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((con
       (make-array 2
                   :element-type 'double-float
                   :initial-contents '(0.3989422804014327 1.2533141373155003)))
      (c
       (make-array 65
                   :element-type 'double-float
                   :initial-contents '(-0.208333333333333 0.125
                                       0.334201388888889 -0.401041666666667
                                       0.0703125 -1.02581259645062
                                       1.84646267361111 -0.8912109375
                                       0.0732421875 4.66958442342625
                                       -11.207002616223 8.78912353515625
                                       -2.3640869140625 0.112152099609375
                                       -28.2120725582002 84.6362176746007
                                       -91.81824154324 42.5349987453885
                                       -7.36879435947963 0.227108001708984
                                       212.570130039217 -765.252468141182
                                       1059.990452528 -699.579627376133
                                       218.190511744212 -26.4914304869516
                                       0.572501420974731 -1919.45766231841
                                       8061.72218173731 -13586.5500064341
                                       11655.3933368645 -5305.6469786134
                                       1200.90291321635 -108.090919788395
                                       1.72772750258446 20204.2913309661
                                       -96980.5983886375 192547.001232532
                                       -203400.177280416 122200.464983017
                                       -41192.6549688976 7109.51430248936
                                       -493.915304773088 6.07404200127348
                                       -242919.187900551 1311763.61466298
                                       -2998015.91853811 3763271.2976564
                                       -2813563.22658653 1268365.27332162
                                       -331645.172484564 45218.7689813627
                                       -2499.83048181121 24.3805296995561
                                       3284469.85307204 -1.97068191184322e7
                                       5.09526024926646e7 -7.41051482115327e7
                                       6.6344512274729e7 -3.75671766607634e7
                                       1.32887671664218e7 -2785618.12808645
                                       308186.404612662 -13886.089753717
                                       110.017140269247))))
  (declare (type (array double-float (2)) con)
           (type (array double-float (65)) c))
  (defun dasyik (x fnu kode flgik ra arg in y)
    (declare (type (array double-float (*)) y)
             (type (f2cl-lib:integer4) in kode)
             (type (double-float) arg ra flgik fnu x))
    (f2cl-lib:with-multi-array-data
        ((y double-float y-%data% y-%offset%))
      (prog ((ak 0.0) (ap 0.0) (coef 0.0) (etx 0.0) (fn 0.0) (gln 0.0) (s1 0.0)
             (s2 0.0) (t$ 0.0) (tol 0.0) (t2 0.0) (z 0.0) (j 0) (jn 0) (k 0)
             (kk 0) (l 0))
        (declare (type (f2cl-lib:integer4) l kk k jn j)
                 (type (double-float) z t2 tol t$ s2 s1 gln fn etx coef ap ak))
        (setf tol (f2cl-lib:d1mach 3))
        (setf tol (max tol 1.0e-15))
        (setf fn fnu)
        (setf z (/ (- 3.0 flgik) 2.0))
        (setf kk (f2cl-lib:int z))
        (f2cl-lib:fdo (jn 1 (f2cl-lib:int-add jn 1))
                      ((> jn in) nil)
          (tagbody
            (if (= jn 1) (go label10))
            (setf fn (- fn flgik))
            (setf z (/ x fn))
            (setf ra (f2cl-lib:fsqrt (+ 1.0 (* z z))))
            (setf gln (f2cl-lib:flog (/ (+ 1.0 ra) z)))
            (setf etx
                    (coerce (the f2cl-lib:integer4 (f2cl-lib:int-sub kode 1))
                            'double-float))
            (setf t$ (+ (* ra (- 1.0 etx)) (/ etx (+ z ra))))
            (setf arg (* fn (- t$ gln) flgik))
           label10
            (setf coef (exp arg))
            (setf t$ (/ 1.0 ra))
            (setf t2 (* t$ t$))
            (setf t$ (/ t$ fn))
            (setf t$ (f2cl-lib:sign t$ flgik))
            (setf s2 1.0)
            (setf ap 1.0)
            (setf l 0)
            (f2cl-lib:fdo (k 2 (f2cl-lib:int-add k 1))
                          ((> k 11) nil)
              (tagbody
                (setf l (f2cl-lib:int-add l 1))
                (setf s1 (f2cl-lib:fref c (l) ((1 65))))
                (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                              ((> j k) nil)
                  (tagbody
                    (setf l (f2cl-lib:int-add l 1))
                    (setf s1 (+ (* s1 t2) (f2cl-lib:fref c (l) ((1 65)))))
                   label20))
                (setf ap (* ap t$))
                (setf ak (* ap s1))
                (setf s2 (+ s2 ak))
                (if (< (max (abs ak) (abs ap)) tol) (go label40))
               label30))
           label40
            (setf t$ (abs t$))
            (setf (f2cl-lib:fref y-%data% (jn) ((1 *)) y-%offset%)
                    (* s2
                       coef
                       (f2cl-lib:fsqrt t$)
                       (f2cl-lib:fref con (kk) ((1 2)))))
           label50))
        (go end_label)
       end_label
        (return (values nil nil nil nil ra arg nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dasyik
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float)
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (double-float)
                        (fortran-to-lisp::integer4) (array double-float (*)))
           :return-values '(nil nil nil nil fortran-to-lisp::ra
                            fortran-to-lisp::arg nil nil)
           :calls '(fortran-to-lisp::d1mach))))

