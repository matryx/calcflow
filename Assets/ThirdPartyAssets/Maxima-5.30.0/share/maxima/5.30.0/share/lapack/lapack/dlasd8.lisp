;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 2edcbd958861 2012/05/30 03:34:52 toy $"
;;;  "f2cl2.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 3fe93de3be82 2012/05/06 02:17:14 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 3fe93de3be82 2012/05/06 02:17:14 toy $")

;;; Using Lisp CMU Common Lisp 20d (20D Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :lapack)


(let* ((one 1.0))
  (declare (type (double-float 1.0 1.0) one) (ignorable one))
  (defun dlasd8 (icompq k d z vf vl difl difr lddifr dsigma work info)
    (declare (type (array double-float (*)) work dsigma difr difl vl vf z d)
             (type (f2cl-lib:integer4) info lddifr k icompq))
    (f2cl-lib:with-multi-array-data
        ((d double-float d-%data% d-%offset%)
         (z double-float z-%data% z-%offset%)
         (vf double-float vf-%data% vf-%offset%)
         (vl double-float vl-%data% vl-%offset%)
         (difl double-float difl-%data% difl-%offset%)
         (difr double-float difr-%data% difr-%offset%)
         (dsigma double-float dsigma-%data% dsigma-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((diflj 0.0) (difrj 0.0) (dj 0.0) (dsigj 0.0) (dsigjp 0.0)
             (rho 0.0) (temp 0.0) (i 0) (iwk1 0) (iwk2 0) (iwk2i 0) (iwk3 0)
             (iwk3i 0) (j 0))
        (declare (type (double-float) diflj difrj dj dsigj dsigjp rho temp)
                 (type (f2cl-lib:integer4) i iwk1 iwk2 iwk2i iwk3 iwk3i j))
        (setf info 0)
        (cond
          ((or (< icompq 0) (> icompq 1))
           (setf info -1))
          ((< k 1)
           (setf info -2))
          ((< lddifr k)
           (setf info -9)))
        (cond
          ((/= info 0)
           (xerbla "DLASD8" (f2cl-lib:int-sub info))
           (go end_label)))
        (cond
          ((= k 1)
           (setf (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%)
                   (abs (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%)))
           (setf (f2cl-lib:fref difl-%data% (1) ((1 *)) difl-%offset%)
                   (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%))
           (cond
             ((= icompq 1)
              (setf (f2cl-lib:fref difl-%data% (2) ((1 *)) difl-%offset%) one)
              (setf (f2cl-lib:fref difr-%data%
                                   (1 2)
                                   ((1 lddifr) (1 *))
                                   difr-%offset%)
                      one)))
           (go end_label)))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i k) nil)
          (tagbody
            (setf (f2cl-lib:fref dsigma-%data% (i) ((1 *)) dsigma-%offset%)
                    (-
                     (dlamc3
                      (f2cl-lib:fref dsigma-%data% (i) ((1 *)) dsigma-%offset%)
                      (f2cl-lib:fref dsigma-%data%
                                     (i)
                                     ((1 *))
                                     dsigma-%offset%))
                     (f2cl-lib:fref dsigma-%data%
                                    (i)
                                    ((1 *))
                                    dsigma-%offset%)))
           label10))
        (setf iwk1 1)
        (setf iwk2 (f2cl-lib:int-add iwk1 k))
        (setf iwk3 (f2cl-lib:int-add iwk2 k))
        (setf iwk2i (f2cl-lib:int-sub iwk2 1))
        (setf iwk3i (f2cl-lib:int-sub iwk3 1))
        (setf rho (dnrm2 k z 1))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
            (dlascl "G" 0 0 rho one k 1 z k info)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8))
          (setf info var-9))
        (setf rho (* rho rho))
        (dlaset "A" k 1 one one
         (f2cl-lib:array-slice work-%data%
                               double-float
                               (iwk3)
                               ((1 *))
                               work-%offset%)
         k)
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j k) nil)
          (tagbody
            (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                (dlasd4 k j dsigma z
                 (f2cl-lib:array-slice work-%data%
                                       double-float
                                       (iwk1)
                                       ((1 *))
                                       work-%offset%)
                 rho (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                 (f2cl-lib:array-slice work-%data%
                                       double-float
                                       (iwk2)
                                       ((1 *))
                                       work-%offset%)
                 info)
              (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-7))
              (setf (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%) var-6)
              (setf info var-8))
            (cond
              ((/= info 0)
               (go end_label)))
            (setf (f2cl-lib:fref work-%data%
                                 ((f2cl-lib:int-add iwk3i j))
                                 ((1 *))
                                 work-%offset%)
                    (*
                     (f2cl-lib:fref work-%data%
                                    ((f2cl-lib:int-add iwk3i j))
                                    ((1 *))
                                    work-%offset%)
                     (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)
                     (f2cl-lib:fref work-%data%
                                    ((f2cl-lib:int-add iwk2i j))
                                    ((1 *))
                                    work-%offset%)))
            (setf (f2cl-lib:fref difl-%data% (j) ((1 *)) difl-%offset%)
                    (- (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)))
            (setf (f2cl-lib:fref difr-%data%
                                 (j 1)
                                 ((1 lddifr) (1 *))
                                 difr-%offset%)
                    (-
                     (f2cl-lib:fref work-%data%
                                    ((f2cl-lib:int-add j 1))
                                    ((1 *))
                                    work-%offset%)))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i (f2cl-lib:int-add j (f2cl-lib:int-sub 1))) nil)
              (tagbody
                (setf (f2cl-lib:fref work-%data%
                                     ((f2cl-lib:int-add iwk3i i))
                                     ((1 *))
                                     work-%offset%)
                        (/
                         (/
                          (*
                           (f2cl-lib:fref work-%data%
                                          ((f2cl-lib:int-add iwk3i i))
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref work-%data%
                                          (i)
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref work-%data%
                                          ((f2cl-lib:int-add iwk2i i))
                                          ((1 *))
                                          work-%offset%))
                          (-
                           (f2cl-lib:fref dsigma-%data%
                                          (i)
                                          ((1 *))
                                          dsigma-%offset%)
                           (f2cl-lib:fref dsigma-%data%
                                          (j)
                                          ((1 *))
                                          dsigma-%offset%)))
                         (+
                          (f2cl-lib:fref dsigma-%data%
                                         (i)
                                         ((1 *))
                                         dsigma-%offset%)
                          (f2cl-lib:fref dsigma-%data%
                                         (j)
                                         ((1 *))
                                         dsigma-%offset%))))
               label20))
            (f2cl-lib:fdo (i (f2cl-lib:int-add j 1) (f2cl-lib:int-add i 1))
                          ((> i k) nil)
              (tagbody
                (setf (f2cl-lib:fref work-%data%
                                     ((f2cl-lib:int-add iwk3i i))
                                     ((1 *))
                                     work-%offset%)
                        (/
                         (/
                          (*
                           (f2cl-lib:fref work-%data%
                                          ((f2cl-lib:int-add iwk3i i))
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref work-%data%
                                          (i)
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref work-%data%
                                          ((f2cl-lib:int-add iwk2i i))
                                          ((1 *))
                                          work-%offset%))
                          (-
                           (f2cl-lib:fref dsigma-%data%
                                          (i)
                                          ((1 *))
                                          dsigma-%offset%)
                           (f2cl-lib:fref dsigma-%data%
                                          (j)
                                          ((1 *))
                                          dsigma-%offset%)))
                         (+
                          (f2cl-lib:fref dsigma-%data%
                                         (i)
                                         ((1 *))
                                         dsigma-%offset%)
                          (f2cl-lib:fref dsigma-%data%
                                         (j)
                                         ((1 *))
                                         dsigma-%offset%))))
               label30))
           label40))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i k) nil)
          (tagbody
            (setf (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                    (f2cl-lib:sign
                     (f2cl-lib:fsqrt
                      (abs
                       (f2cl-lib:fref work-%data%
                                      ((f2cl-lib:int-add iwk3i i))
                                      ((1 *))
                                      work-%offset%)))
                     (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)))
           label50))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j k) nil)
          (tagbody
            (setf diflj (f2cl-lib:fref difl-%data% (j) ((1 *)) difl-%offset%))
            (setf dj (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%))
            (setf dsigj
                    (-
                     (f2cl-lib:fref dsigma-%data%
                                    (j)
                                    ((1 *))
                                    dsigma-%offset%)))
            (cond
              ((< j k)
               (setf difrj
                       (-
                        (f2cl-lib:fref difr-%data%
                                       (j 1)
                                       ((1 lddifr) (1 *))
                                       difr-%offset%)))
               (setf dsigjp
                       (-
                        (f2cl-lib:fref dsigma-%data%
                                       ((f2cl-lib:int-add j 1))
                                       ((1 *))
                                       dsigma-%offset%)))))
            (setf (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)
                    (/
                     (/ (- (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%))
                        diflj)
                     (+
                      (f2cl-lib:fref dsigma-%data% (j) ((1 *)) dsigma-%offset%)
                      dj)))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i (f2cl-lib:int-add j (f2cl-lib:int-sub 1))) nil)
              (tagbody
                (setf (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%)
                        (/
                         (/ (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                            (-
                             (dlamc3
                              (f2cl-lib:fref dsigma-%data%
                                             (i)
                                             ((1 *))
                                             dsigma-%offset%)
                              dsigj)
                             diflj))
                         (+
                          (f2cl-lib:fref dsigma-%data%
                                         (i)
                                         ((1 *))
                                         dsigma-%offset%)
                          dj)))
               label60))
            (f2cl-lib:fdo (i (f2cl-lib:int-add j 1) (f2cl-lib:int-add i 1))
                          ((> i k) nil)
              (tagbody
                (setf (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%)
                        (/
                         (/ (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                            (+
                             (dlamc3
                              (f2cl-lib:fref dsigma-%data%
                                             (i)
                                             ((1 *))
                                             dsigma-%offset%)
                              dsigjp)
                             difrj))
                         (+
                          (f2cl-lib:fref dsigma-%data%
                                         (i)
                                         ((1 *))
                                         dsigma-%offset%)
                          dj)))
               label70))
            (setf temp (dnrm2 k work 1))
            (setf (f2cl-lib:fref work-%data%
                                 ((f2cl-lib:int-add iwk2i j))
                                 ((1 *))
                                 work-%offset%)
                    (/ (ddot k work 1 vf 1) temp))
            (setf (f2cl-lib:fref work-%data%
                                 ((f2cl-lib:int-add iwk3i j))
                                 ((1 *))
                                 work-%offset%)
                    (/ (ddot k work 1 vl 1) temp))
            (cond
              ((= icompq 1)
               (setf (f2cl-lib:fref difr-%data%
                                    (j 2)
                                    ((1 lddifr) (1 *))
                                    difr-%offset%)
                       temp)))
           label80))
        (dcopy k
         (f2cl-lib:array-slice work-%data%
                               double-float
                               (iwk2)
                               ((1 *))
                               work-%offset%)
         1 vf 1)
        (dcopy k
         (f2cl-lib:array-slice work-%data%
                               double-float
                               (iwk3)
                               ((1 *))
                               work-%offset%)
         1 vl 1)
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasd8
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dcopy fortran-to-lisp::ddot
                    fortran-to-lisp::dlasd4 fortran-to-lisp::dlaset
                    fortran-to-lisp::dlascl fortran-to-lisp::dnrm2
                    fortran-to-lisp::dlamc3 fortran-to-lisp::xerbla))))

