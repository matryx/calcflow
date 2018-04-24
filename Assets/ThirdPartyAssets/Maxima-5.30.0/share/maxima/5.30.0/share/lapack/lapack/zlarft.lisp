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


(let* ((one (f2cl-lib:cmplx 1.0 0.0)) (zero (f2cl-lib:cmplx 0.0 0.0)))
  (declare (type (f2cl-lib:complex16) one)
           (type (f2cl-lib:complex16) zero)
           (ignorable one zero))
  (defun zlarft (direct storev n k v ldv tau t$ ldt)
    (declare (type (array f2cl-lib:complex16 (*)) t$ tau v)
             (type (f2cl-lib:integer4) ldt ldv k n)
             (type (simple-string *) storev direct))
    (f2cl-lib:with-multi-array-data
        ((direct character direct-%data% direct-%offset%)
         (storev character storev-%data% storev-%offset%)
         (v f2cl-lib:complex16 v-%data% v-%offset%)
         (tau f2cl-lib:complex16 tau-%data% tau-%offset%)
         (t$ f2cl-lib:complex16 t$-%data% t$-%offset%))
      (prog ((vii #C(0.0 0.0)) (i 0) (j 0) (prevlastv 0) (lastv 0))
        (declare (type (f2cl-lib:complex16) vii)
                 (type (f2cl-lib:integer4) i j prevlastv lastv))
        (if (= n 0) (go end_label))
        (cond
          ((lsame direct "F")
           (setf prevlastv n)
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i k) nil)
             (tagbody
               (setf prevlastv
                       (max (the f2cl-lib:integer4 prevlastv)
                            (the f2cl-lib:integer4 i)))
               (cond
                 ((= (f2cl-lib:fref tau (i) ((1 *))) zero)
                  (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                ((> j i) nil)
                    (tagbody
                      (setf (f2cl-lib:fref t$-%data%
                                           (j i)
                                           ((1 ldt) (1 *))
                                           t$-%offset%)
                              zero)
                     label10)))
                 (t
                  (setf vii
                          (f2cl-lib:fref v-%data%
                                         (i i)
                                         ((1 ldv) (1 *))
                                         v-%offset%))
                  (setf (f2cl-lib:fref v-%data%
                                       (i i)
                                       ((1 ldv) (1 *))
                                       v-%offset%)
                          one)
                  (cond
                    ((lsame storev "C")
                     (f2cl-lib:fdo (lastv n
                                    (f2cl-lib:int-add lastv
                                                      (f2cl-lib:int-sub 1)))
                                   ((> lastv (f2cl-lib:int-add i 1)) nil)
                       (tagbody
                         (if
                          (/=
                           (f2cl-lib:fref v-%data%
                                          (lastv i)
                                          ((1 ldv) (1 *))
                                          v-%offset%)
                           zero)
                          (go f2cl-lib::exit))
                        label100000))
                     (setf j
                             (min (the f2cl-lib:integer4 lastv)
                                  (the f2cl-lib:integer4 prevlastv)))
                     (zgemv "Conjugate transpose"
                      (f2cl-lib:int-add (f2cl-lib:int-sub j i) 1)
                      (f2cl-lib:int-sub i 1)
                      (- (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%))
                      (f2cl-lib:array-slice v-%data%
                                            f2cl-lib:complex16
                                            (i 1)
                                            ((1 ldv) (1 *))
                                            v-%offset%)
                      ldv
                      (f2cl-lib:array-slice v-%data%
                                            f2cl-lib:complex16
                                            (i i)
                                            ((1 ldv) (1 *))
                                            v-%offset%)
                      1 zero
                      (f2cl-lib:array-slice t$-%data%
                                            f2cl-lib:complex16
                                            (1 i)
                                            ((1 ldt) (1 *))
                                            t$-%offset%)
                      1))
                    (t
                     (f2cl-lib:fdo (lastv n
                                    (f2cl-lib:int-add lastv
                                                      (f2cl-lib:int-sub 1)))
                                   ((> lastv (f2cl-lib:int-add i 1)) nil)
                       (tagbody
                         (if
                          (/=
                           (f2cl-lib:fref v-%data%
                                          (i lastv)
                                          ((1 ldv) (1 *))
                                          v-%offset%)
                           zero)
                          (go f2cl-lib::exit))
                        label100001))
                     (setf j
                             (min (the f2cl-lib:integer4 lastv)
                                  (the f2cl-lib:integer4 prevlastv)))
                     (if (< i j)
                         (zlacgv (f2cl-lib:int-sub j i)
                          (f2cl-lib:array-slice v-%data%
                                                f2cl-lib:complex16
                                                (i (f2cl-lib:int-add i 1))
                                                ((1 ldv) (1 *))
                                                v-%offset%)
                          ldv))
                     (zgemv "No transpose" (f2cl-lib:int-sub i 1)
                      (f2cl-lib:int-add (f2cl-lib:int-sub j i) 1)
                      (- (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%))
                      (f2cl-lib:array-slice v-%data%
                                            f2cl-lib:complex16
                                            (1 i)
                                            ((1 ldv) (1 *))
                                            v-%offset%)
                      ldv
                      (f2cl-lib:array-slice v-%data%
                                            f2cl-lib:complex16
                                            (i i)
                                            ((1 ldv) (1 *))
                                            v-%offset%)
                      ldv zero
                      (f2cl-lib:array-slice t$-%data%
                                            f2cl-lib:complex16
                                            (1 i)
                                            ((1 ldt) (1 *))
                                            t$-%offset%)
                      1)
                     (if (< i j)
                         (zlacgv (f2cl-lib:int-sub j i)
                          (f2cl-lib:array-slice v-%data%
                                                f2cl-lib:complex16
                                                (i (f2cl-lib:int-add i 1))
                                                ((1 ldv) (1 *))
                                                v-%offset%)
                          ldv))))
                  (setf (f2cl-lib:fref v-%data%
                                       (i i)
                                       ((1 ldv) (1 *))
                                       v-%offset%)
                          vii)
                  (ztrmv "Upper" "No transpose" "Non-unit"
                   (f2cl-lib:int-sub i 1) t$ ldt
                   (f2cl-lib:array-slice t$-%data%
                                         f2cl-lib:complex16
                                         (1 i)
                                         ((1 ldt) (1 *))
                                         t$-%offset%)
                   1)
                  (setf (f2cl-lib:fref t$-%data%
                                       (i i)
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                          (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%))
                  (cond
                    ((> i 1)
                     (setf prevlastv
                             (max (the f2cl-lib:integer4 prevlastv)
                                  (the f2cl-lib:integer4 lastv))))
                    (t
                     (setf prevlastv lastv)))))
              label20)))
          (t
           (setf prevlastv 1)
           (f2cl-lib:fdo (i k (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                         ((> i 1) nil)
             (tagbody
               (cond
                 ((= (f2cl-lib:fref tau (i) ((1 *))) zero)
                  (f2cl-lib:fdo (j i (f2cl-lib:int-add j 1))
                                ((> j k) nil)
                    (tagbody
                      (setf (f2cl-lib:fref t$-%data%
                                           (j i)
                                           ((1 ldt) (1 *))
                                           t$-%offset%)
                              zero)
                     label30)))
                 (t
                  (cond
                    ((< i k)
                     (cond
                       ((lsame storev "C")
                        (setf vii
                                (f2cl-lib:fref v-%data%
                                               ((f2cl-lib:int-add
                                                 (f2cl-lib:int-sub n k)
                                                 i)
                                                i)
                                               ((1 ldv) (1 *))
                                               v-%offset%))
                        (setf (f2cl-lib:fref v-%data%
                                             ((f2cl-lib:int-add
                                               (f2cl-lib:int-sub n k)
                                               i)
                                              i)
                                             ((1 ldv) (1 *))
                                             v-%offset%)
                                one)
                        (f2cl-lib:fdo (lastv 1 (f2cl-lib:int-add lastv 1))
                                      ((> lastv
                                          (f2cl-lib:int-add i
                                                            (f2cl-lib:int-sub
                                                             1)))
                                       nil)
                          (tagbody
                            (if
                             (/=
                              (f2cl-lib:fref v-%data%
                                             (lastv i)
                                             ((1 ldv) (1 *))
                                             v-%offset%)
                              zero)
                             (go f2cl-lib::exit))
                           label100002))
                        (setf j
                                (max (the f2cl-lib:integer4 lastv)
                                     (the f2cl-lib:integer4 prevlastv)))
                        (zgemv "Conjugate transpose"
                         (f2cl-lib:int-add
                          (f2cl-lib:int-sub
                           (f2cl-lib:int-add (f2cl-lib:int-sub n k) i)
                           j)
                          1)
                         (f2cl-lib:int-sub k i)
                         (-
                          (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%))
                         (f2cl-lib:array-slice v-%data%
                                               f2cl-lib:complex16
                                               (j (f2cl-lib:int-add i 1))
                                               ((1 ldv) (1 *))
                                               v-%offset%)
                         ldv
                         (f2cl-lib:array-slice v-%data%
                                               f2cl-lib:complex16
                                               (j i)
                                               ((1 ldv) (1 *))
                                               v-%offset%)
                         1 zero
                         (f2cl-lib:array-slice t$-%data%
                                               f2cl-lib:complex16
                                               ((+ i 1) i)
                                               ((1 ldt) (1 *))
                                               t$-%offset%)
                         1)
                        (setf (f2cl-lib:fref v-%data%
                                             ((f2cl-lib:int-add
                                               (f2cl-lib:int-sub n k)
                                               i)
                                              i)
                                             ((1 ldv) (1 *))
                                             v-%offset%)
                                vii))
                       (t
                        (setf vii
                                (f2cl-lib:fref v-%data%
                                               (i
                                                (f2cl-lib:int-add
                                                 (f2cl-lib:int-sub n k)
                                                 i))
                                               ((1 ldv) (1 *))
                                               v-%offset%))
                        (setf (f2cl-lib:fref v-%data%
                                             (i
                                              (f2cl-lib:int-add
                                               (f2cl-lib:int-sub n k)
                                               i))
                                             ((1 ldv) (1 *))
                                             v-%offset%)
                                one)
                        (f2cl-lib:fdo (lastv 1 (f2cl-lib:int-add lastv 1))
                                      ((> lastv
                                          (f2cl-lib:int-add i
                                                            (f2cl-lib:int-sub
                                                             1)))
                                       nil)
                          (tagbody
                            (if
                             (/=
                              (f2cl-lib:fref v-%data%
                                             (i lastv)
                                             ((1 ldv) (1 *))
                                             v-%offset%)
                              zero)
                             (go f2cl-lib::exit))
                           label100003))
                        (setf j
                                (max (the f2cl-lib:integer4 lastv)
                                     (the f2cl-lib:integer4 prevlastv)))
                        (zlacgv
                         (f2cl-lib:int-add
                          (f2cl-lib:int-sub
                           (f2cl-lib:int-add (f2cl-lib:int-sub n k) i)
                           1
                           j)
                          1)
                         (f2cl-lib:array-slice v-%data%
                                               f2cl-lib:complex16
                                               (i j)
                                               ((1 ldv) (1 *))
                                               v-%offset%)
                         ldv)
                        (zgemv "No transpose" (f2cl-lib:int-sub k i)
                         (f2cl-lib:int-add
                          (f2cl-lib:int-sub
                           (f2cl-lib:int-add (f2cl-lib:int-sub n k) i)
                           j)
                          1)
                         (-
                          (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%))
                         (f2cl-lib:array-slice v-%data%
                                               f2cl-lib:complex16
                                               ((+ i 1) j)
                                               ((1 ldv) (1 *))
                                               v-%offset%)
                         ldv
                         (f2cl-lib:array-slice v-%data%
                                               f2cl-lib:complex16
                                               (i j)
                                               ((1 ldv) (1 *))
                                               v-%offset%)
                         ldv zero
                         (f2cl-lib:array-slice t$-%data%
                                               f2cl-lib:complex16
                                               ((+ i 1) i)
                                               ((1 ldt) (1 *))
                                               t$-%offset%)
                         1)
                        (zlacgv
                         (f2cl-lib:int-add
                          (f2cl-lib:int-sub
                           (f2cl-lib:int-add (f2cl-lib:int-sub n k) i)
                           1
                           j)
                          1)
                         (f2cl-lib:array-slice v-%data%
                                               f2cl-lib:complex16
                                               (i j)
                                               ((1 ldv) (1 *))
                                               v-%offset%)
                         ldv)
                        (setf (f2cl-lib:fref v-%data%
                                             (i
                                              (f2cl-lib:int-add
                                               (f2cl-lib:int-sub n k)
                                               i))
                                             ((1 ldv) (1 *))
                                             v-%offset%)
                                vii)))
                     (ztrmv "Lower" "No transpose" "Non-unit"
                      (f2cl-lib:int-sub k i)
                      (f2cl-lib:array-slice t$-%data%
                                            f2cl-lib:complex16
                                            ((+ i 1) (f2cl-lib:int-add i 1))
                                            ((1 ldt) (1 *))
                                            t$-%offset%)
                      ldt
                      (f2cl-lib:array-slice t$-%data%
                                            f2cl-lib:complex16
                                            ((+ i 1) i)
                                            ((1 ldt) (1 *))
                                            t$-%offset%)
                      1)
                     (cond
                       ((> i 1)
                        (setf prevlastv
                                (min (the f2cl-lib:integer4 prevlastv)
                                     (the f2cl-lib:integer4 lastv))))
                       (t
                        (setf prevlastv lastv)))))
                  (setf (f2cl-lib:fref t$-%data%
                                       (i i)
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                          (f2cl-lib:fref tau-%data%
                                         (i)
                                         ((1 *))
                                         tau-%offset%))))
              label40))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zlarft
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::ztrmv fortran-to-lisp::zlacgv
                    fortran-to-lisp::zgemv fortran-to-lisp::lsame))))

