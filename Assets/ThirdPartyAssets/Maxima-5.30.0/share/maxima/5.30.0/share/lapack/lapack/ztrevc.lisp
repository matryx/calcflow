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


(let* ((zero 0.0)
       (one 1.0)
       (cmzero (f2cl-lib:cmplx 0.0 0.0))
       (cmone (f2cl-lib:cmplx 1.0 0.0)))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (type (f2cl-lib:complex16) cmzero)
           (type (f2cl-lib:complex16) cmone)
           (ignorable zero one cmzero cmone))
  (defun ztrevc
         (side howmny select n t$ ldt vl ldvl vr ldvr mm m work rwork info)
    (declare (type (array double-float (*)) rwork)
             (type (array f2cl-lib:complex16 (*)) work vr vl t$)
             (type (f2cl-lib:integer4) info m mm ldvr ldvl ldt n)
             (type (array f2cl-lib:logical (*)) select)
             (type (simple-string *) howmny side))
    (f2cl-lib:with-multi-array-data
        ((side character side-%data% side-%offset%)
         (howmny character howmny-%data% howmny-%offset%)
         (select f2cl-lib:logical select-%data% select-%offset%)
         (t$ f2cl-lib:complex16 t$-%data% t$-%offset%)
         (vl f2cl-lib:complex16 vl-%data% vl-%offset%)
         (vr f2cl-lib:complex16 vr-%data% vr-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%)
         (rwork double-float rwork-%data% rwork-%offset%))
      (labels ((cabs1 (cdum)
                 (+ (abs (f2cl-lib:dble cdum)) (abs (f2cl-lib:dimag cdum)))))
        (declare (ftype (function (f2cl-lib:complex16)
                         (values double-float &rest t))
                        cabs1))
        (prog ((cdum #C(0.0 0.0)) (ovfl 0.0) (remax 0.0) (scale 0.0) (smin 0.0)
               (smlnum 0.0) (ulp 0.0) (unfl 0.0) (i 0) (ii 0) (is 0) (j 0)
               (k 0) (ki 0) (allv nil) (bothv nil) (leftv nil) (over nil)
               (rightv nil) (somev nil) (dcmplx$ 0.0f0))
          (declare (type (single-float) dcmplx$)
                   (type (f2cl-lib:complex16) cdum)
                   (type (double-float) ovfl remax scale smin smlnum ulp unfl)
                   (type (f2cl-lib:integer4) i ii is j k ki)
                   (type f2cl-lib:logical allv bothv leftv over rightv somev))
          (setf bothv (lsame side "B"))
          (setf rightv (or (lsame side "R") bothv))
          (setf leftv (or (lsame side "L") bothv))
          (setf allv (lsame howmny "A"))
          (setf over (lsame howmny "B"))
          (setf somev (lsame howmny "S"))
          (cond
            (somev
             (setf m 0)
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j n) nil)
               (tagbody
                 (if (f2cl-lib:fref select-%data% (j) ((1 *)) select-%offset%)
                     (setf m (f2cl-lib:int-add m 1)))
                label10)))
            (t
             (setf m n)))
          (setf info 0)
          (cond
            ((and (not rightv) (not leftv))
             (setf info -1))
            ((and (not allv) (not over) (not somev))
             (setf info -2))
            ((< n 0)
             (setf info -4))
            ((< ldt (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
             (setf info -6))
            ((or (< ldvl 1) (and leftv (< ldvl n)))
             (setf info -8))
            ((or (< ldvr 1) (and rightv (< ldvr n)))
             (setf info -10))
            ((< mm m)
             (setf info -11)))
          (cond
            ((/= info 0)
             (xerbla "ZTREVC" (f2cl-lib:int-sub info))
             (go end_label)))
          (if (= n 0) (go end_label))
          (setf unfl (dlamch "Safe minimum"))
          (setf ovfl (/ one unfl))
          (multiple-value-bind (var-0 var-1)
              (dlabad unfl ovfl)
            (declare (ignore))
            (setf unfl var-0)
            (setf ovfl var-1))
          (setf ulp (dlamch "Precision"))
          (setf smlnum (* unfl (/ n ulp)))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf (f2cl-lib:fref work-%data%
                                   ((f2cl-lib:int-add i n))
                                   ((1 *))
                                   work-%offset%)
                      (f2cl-lib:fref t$-%data%
                                     (i i)
                                     ((1 ldt) (1 *))
                                     t$-%offset%))
             label20))
          (setf (f2cl-lib:fref rwork-%data% (1) ((1 *)) rwork-%offset%) zero)
          (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (setf (f2cl-lib:fref rwork-%data% (j) ((1 *)) rwork-%offset%)
                      (dzasum (f2cl-lib:int-sub j 1)
                       (f2cl-lib:array-slice t$-%data%
                                             f2cl-lib:complex16
                                             (1 j)
                                             ((1 ldt) (1 *))
                                             t$-%offset%)
                       1))
             label30))
          (cond
            (rightv
             (setf is m)
             (f2cl-lib:fdo (ki n (f2cl-lib:int-add ki (f2cl-lib:int-sub 1)))
                           ((> ki 1) nil)
               (tagbody
                 (cond
                   (somev
                    (if
                     (not
                      (f2cl-lib:fref select-%data%
                                     (ki)
                                     ((1 *))
                                     select-%offset%))
                     (go label80))))
                 (setf smin
                         (max
                          (* ulp
                             (cabs1
                              (f2cl-lib:fref t$-%data%
                                             (ki ki)
                                             ((1 ldt) (1 *))
                                             t$-%offset%)))
                          smlnum))
                 (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                         cmone)
                 (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                               ((> k
                                   (f2cl-lib:int-add ki (f2cl-lib:int-sub 1)))
                                nil)
                   (tagbody
                     (setf (f2cl-lib:fref work-%data%
                                          (k)
                                          ((1 *))
                                          work-%offset%)
                             (-
                              (f2cl-lib:fref t$-%data%
                                             (k ki)
                                             ((1 ldt) (1 *))
                                             t$-%offset%)))
                    label40))
                 (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                               ((> k
                                   (f2cl-lib:int-add ki (f2cl-lib:int-sub 1)))
                                nil)
                   (tagbody
                     (setf (f2cl-lib:fref t$-%data%
                                          (k k)
                                          ((1 ldt) (1 *))
                                          t$-%offset%)
                             (-
                              (f2cl-lib:fref t$-%data%
                                             (k k)
                                             ((1 ldt) (1 *))
                                             t$-%offset%)
                              (f2cl-lib:fref t$-%data%
                                             (ki ki)
                                             ((1 ldt) (1 *))
                                             t$-%offset%)))
                     (if
                      (<
                       (cabs1
                        (f2cl-lib:fref t$-%data%
                                       (k k)
                                       ((1 ldt) (1 *))
                                       t$-%offset%))
                       smin)
                      (setf (f2cl-lib:fref t$-%data%
                                           (k k)
                                           ((1 ldt) (1 *))
                                           t$-%offset%)
                              (coerce smin 'f2cl-lib:complex16)))
                    label50))
                 (cond
                   ((> ki 1)
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10)
                        (zlatrs "Upper" "No transpose" "Non-unit" "Y"
                         (f2cl-lib:int-sub ki 1) t$ ldt
                         (f2cl-lib:array-slice work-%data%
                                               f2cl-lib:complex16
                                               (1)
                                               ((1 *))
                                               work-%offset%)
                         scale rwork info)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-9))
                      (setf scale var-8)
                      (setf info var-10))
                    (setf (f2cl-lib:fref work-%data%
                                         (ki)
                                         ((1 *))
                                         work-%offset%)
                            (coerce scale 'f2cl-lib:complex16))))
                 (cond
                   ((not over)
                    (zcopy ki
                     (f2cl-lib:array-slice work-%data%
                                           f2cl-lib:complex16
                                           (1)
                                           ((1 *))
                                           work-%offset%)
                     1
                     (f2cl-lib:array-slice vr-%data%
                                           f2cl-lib:complex16
                                           (1 is)
                                           ((1 ldvr) (1 *))
                                           vr-%offset%)
                     1)
                    (setf ii
                            (izamax ki
                             (f2cl-lib:array-slice vr-%data%
                                                   f2cl-lib:complex16
                                                   (1 is)
                                                   ((1 ldvr) (1 *))
                                                   vr-%offset%)
                             1))
                    (setf remax
                            (/ one
                               (cabs1
                                (f2cl-lib:fref vr-%data%
                                               (ii is)
                                               ((1 ldvr) (1 *))
                                               vr-%offset%))))
                    (zdscal ki remax
                     (f2cl-lib:array-slice vr-%data%
                                           f2cl-lib:complex16
                                           (1 is)
                                           ((1 ldvr) (1 *))
                                           vr-%offset%)
                     1)
                    (f2cl-lib:fdo (k (f2cl-lib:int-add ki 1)
                                   (f2cl-lib:int-add k 1))
                                  ((> k n) nil)
                      (tagbody
                        (setf (f2cl-lib:fref vr-%data%
                                             (k is)
                                             ((1 ldvr) (1 *))
                                             vr-%offset%)
                                cmzero)
                       label60)))
                   (t
                    (if (> ki 1)
                        (zgemv "N" n (f2cl-lib:int-sub ki 1) cmone vr ldvr
                         (f2cl-lib:array-slice work-%data%
                                               f2cl-lib:complex16
                                               (1)
                                               ((1 *))
                                               work-%offset%)
                         1 (f2cl-lib:dcmplx scale)
                         (f2cl-lib:array-slice vr-%data%
                                               f2cl-lib:complex16
                                               (1 ki)
                                               ((1 ldvr) (1 *))
                                               vr-%offset%)
                         1))
                    (setf ii
                            (izamax n
                             (f2cl-lib:array-slice vr-%data%
                                                   f2cl-lib:complex16
                                                   (1 ki)
                                                   ((1 ldvr) (1 *))
                                                   vr-%offset%)
                             1))
                    (setf remax
                            (/ one
                               (cabs1
                                (f2cl-lib:fref vr-%data%
                                               (ii ki)
                                               ((1 ldvr) (1 *))
                                               vr-%offset%))))
                    (zdscal n remax
                     (f2cl-lib:array-slice vr-%data%
                                           f2cl-lib:complex16
                                           (1 ki)
                                           ((1 ldvr) (1 *))
                                           vr-%offset%)
                     1)))
                 (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                               ((> k
                                   (f2cl-lib:int-add ki (f2cl-lib:int-sub 1)))
                                nil)
                   (tagbody
                     (setf (f2cl-lib:fref t$-%data%
                                          (k k)
                                          ((1 ldt) (1 *))
                                          t$-%offset%)
                             (f2cl-lib:fref work-%data%
                                            ((f2cl-lib:int-add k n))
                                            ((1 *))
                                            work-%offset%))
                    label70))
                 (setf is (f2cl-lib:int-sub is 1))
                label80))))
          (cond
            (leftv
             (setf is 1)
             (f2cl-lib:fdo (ki 1 (f2cl-lib:int-add ki 1))
                           ((> ki n) nil)
               (tagbody
                 (cond
                   (somev
                    (if
                     (not
                      (f2cl-lib:fref select-%data%
                                     (ki)
                                     ((1 *))
                                     select-%offset%))
                     (go label130))))
                 (setf smin
                         (max
                          (* ulp
                             (cabs1
                              (f2cl-lib:fref t$-%data%
                                             (ki ki)
                                             ((1 ldt) (1 *))
                                             t$-%offset%)))
                          smlnum))
                 (setf (f2cl-lib:fref work-%data% (n) ((1 *)) work-%offset%)
                         cmone)
                 (f2cl-lib:fdo (k (f2cl-lib:int-add ki 1)
                                (f2cl-lib:int-add k 1))
                               ((> k n) nil)
                   (tagbody
                     (setf (f2cl-lib:fref work-%data%
                                          (k)
                                          ((1 *))
                                          work-%offset%)
                             (coerce
                              (-
                               (f2cl-lib:dconjg
                                (f2cl-lib:fref t$-%data%
                                               (ki k)
                                               ((1 ldt) (1 *))
                                               t$-%offset%)))
                              'f2cl-lib:complex16))
                    label90))
                 (f2cl-lib:fdo (k (f2cl-lib:int-add ki 1)
                                (f2cl-lib:int-add k 1))
                               ((> k n) nil)
                   (tagbody
                     (setf (f2cl-lib:fref t$-%data%
                                          (k k)
                                          ((1 ldt) (1 *))
                                          t$-%offset%)
                             (-
                              (f2cl-lib:fref t$-%data%
                                             (k k)
                                             ((1 ldt) (1 *))
                                             t$-%offset%)
                              (f2cl-lib:fref t$-%data%
                                             (ki ki)
                                             ((1 ldt) (1 *))
                                             t$-%offset%)))
                     (if
                      (<
                       (cabs1
                        (f2cl-lib:fref t$-%data%
                                       (k k)
                                       ((1 ldt) (1 *))
                                       t$-%offset%))
                       smin)
                      (setf (f2cl-lib:fref t$-%data%
                                           (k k)
                                           ((1 ldt) (1 *))
                                           t$-%offset%)
                              (coerce smin 'f2cl-lib:complex16)))
                    label100))
                 (cond
                   ((< ki n)
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10)
                        (zlatrs "Upper" "Conjugate transpose" "Non-unit" "Y"
                         (f2cl-lib:int-sub n ki)
                         (f2cl-lib:array-slice t$-%data%
                                               f2cl-lib:complex16
                                               ((+ ki 1)
                                                (f2cl-lib:int-add ki 1))
                                               ((1 ldt) (1 *))
                                               t$-%offset%)
                         ldt
                         (f2cl-lib:array-slice work-%data%
                                               f2cl-lib:complex16
                                               ((+ ki 1))
                                               ((1 *))
                                               work-%offset%)
                         scale rwork info)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-9))
                      (setf scale var-8)
                      (setf info var-10))
                    (setf (f2cl-lib:fref work-%data%
                                         (ki)
                                         ((1 *))
                                         work-%offset%)
                            (coerce scale 'f2cl-lib:complex16))))
                 (cond
                   ((not over)
                    (zcopy (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1)
                     (f2cl-lib:array-slice work-%data%
                                           f2cl-lib:complex16
                                           (ki)
                                           ((1 *))
                                           work-%offset%)
                     1
                     (f2cl-lib:array-slice vl-%data%
                                           f2cl-lib:complex16
                                           (ki is)
                                           ((1 ldvl) (1 *))
                                           vl-%offset%)
                     1)
                    (setf ii
                            (f2cl-lib:int-sub
                             (f2cl-lib:int-add
                              (izamax
                               (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1)
                               (f2cl-lib:array-slice vl-%data%
                                                     f2cl-lib:complex16
                                                     (ki is)
                                                     ((1 ldvl) (1 *))
                                                     vl-%offset%)
                               1)
                              ki)
                             1))
                    (setf remax
                            (/ one
                               (cabs1
                                (f2cl-lib:fref vl-%data%
                                               (ii is)
                                               ((1 ldvl) (1 *))
                                               vl-%offset%))))
                    (zdscal (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1) remax
                     (f2cl-lib:array-slice vl-%data%
                                           f2cl-lib:complex16
                                           (ki is)
                                           ((1 ldvl) (1 *))
                                           vl-%offset%)
                     1)
                    (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                  ((> k
                                      (f2cl-lib:int-add ki
                                                        (f2cl-lib:int-sub 1)))
                                   nil)
                      (tagbody
                        (setf (f2cl-lib:fref vl-%data%
                                             (k is)
                                             ((1 ldvl) (1 *))
                                             vl-%offset%)
                                cmzero)
                       label110)))
                   (t
                    (if (< ki n)
                        (zgemv "N" n (f2cl-lib:int-sub n ki) cmone
                         (f2cl-lib:array-slice vl-%data%
                                               f2cl-lib:complex16
                                               (1 (f2cl-lib:int-add ki 1))
                                               ((1 ldvl) (1 *))
                                               vl-%offset%)
                         ldvl
                         (f2cl-lib:array-slice work-%data%
                                               f2cl-lib:complex16
                                               ((+ ki 1))
                                               ((1 *))
                                               work-%offset%)
                         1 (f2cl-lib:dcmplx scale)
                         (f2cl-lib:array-slice vl-%data%
                                               f2cl-lib:complex16
                                               (1 ki)
                                               ((1 ldvl) (1 *))
                                               vl-%offset%)
                         1))
                    (setf ii
                            (izamax n
                             (f2cl-lib:array-slice vl-%data%
                                                   f2cl-lib:complex16
                                                   (1 ki)
                                                   ((1 ldvl) (1 *))
                                                   vl-%offset%)
                             1))
                    (setf remax
                            (/ one
                               (cabs1
                                (f2cl-lib:fref vl-%data%
                                               (ii ki)
                                               ((1 ldvl) (1 *))
                                               vl-%offset%))))
                    (zdscal n remax
                     (f2cl-lib:array-slice vl-%data%
                                           f2cl-lib:complex16
                                           (1 ki)
                                           ((1 ldvl) (1 *))
                                           vl-%offset%)
                     1)))
                 (f2cl-lib:fdo (k (f2cl-lib:int-add ki 1)
                                (f2cl-lib:int-add k 1))
                               ((> k n) nil)
                   (tagbody
                     (setf (f2cl-lib:fref t$-%data%
                                          (k k)
                                          ((1 ldt) (1 *))
                                          t$-%offset%)
                             (f2cl-lib:fref work-%data%
                                            ((f2cl-lib:int-add k n))
                                            ((1 *))
                                            work-%offset%))
                    label120))
                 (setf is (f2cl-lib:int-add is 1))
                label130))))
          (go end_label)
         end_label
          (return
           (values nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   m
                   nil
                   nil
                   info)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::ztrevc
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string)
                        (array fortran-to-lisp::logical (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::m nil nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::zgemv fortran-to-lisp::zdscal
                    fortran-to-lisp::izamax fortran-to-lisp::zcopy
                    fortran-to-lisp::zlatrs fortran-to-lisp::dzasum
                    fortran-to-lisp::dlabad fortran-to-lisp::dlamch
                    fortran-to-lisp::xerbla fortran-to-lisp::lsame))))

