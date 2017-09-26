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


(defun zunmhr (side trans m n ilo ihi a lda tau c ldc work lwork info)
  (declare (type (array f2cl-lib:complex16 (*)) work c tau a)
           (type (f2cl-lib:integer4) info lwork ldc lda ihi ilo n m)
           (type (simple-string *) trans side))
  (f2cl-lib:with-multi-array-data
      ((side character side-%data% side-%offset%)
       (trans character trans-%data% trans-%offset%)
       (a f2cl-lib:complex16 a-%data% a-%offset%)
       (tau f2cl-lib:complex16 tau-%data% tau-%offset%)
       (c f2cl-lib:complex16 c-%data% c-%offset%)
       (work f2cl-lib:complex16 work-%data% work-%offset%))
    (prog ((i1 0) (i2 0) (iinfo 0) (lwkopt 0) (mi 0) (nb 0) (nh 0) (ni 0)
           (nq 0) (nw 0) (left nil) (lquery nil))
      (declare (type f2cl-lib:logical lquery left)
               (type (f2cl-lib:integer4) nw nq ni nh nb mi lwkopt iinfo i2 i1))
      (setf info 0)
      (setf nh (f2cl-lib:int-sub ihi ilo))
      (setf left (lsame side "L"))
      (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
      (cond
        (left
         (setf nq m)
         (setf nw n))
        (t
         (setf nq n)
         (setf nw m)))
      (cond
        ((and (not left) (not (lsame side "R")))
         (setf info -1))
        ((and (not (lsame trans "N")) (not (lsame trans "C")))
         (setf info -2))
        ((< m 0)
         (setf info -3))
        ((< n 0)
         (setf info -4))
        ((or (< ilo 1)
             (> ilo (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 nq))))
         (setf info -5))
        ((or
          (< ihi (min (the f2cl-lib:integer4 ilo) (the f2cl-lib:integer4 nq)))
          (> ihi nq))
         (setf info -6))
        ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 nq)))
         (setf info -8))
        ((< ldc (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
         (setf info -11))
        ((and
          (< lwork (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 nw)))
          (not lquery))
         (setf info -13)))
      (cond
        ((= info 0)
         (cond
           (left
            (setf nb
                    (ilaenv 1 "ZUNMQR" (f2cl-lib:f2cl-// side trans) nh n nh
                     -1)))
           (t
            (setf nb
                    (ilaenv 1 "ZUNMQR" (f2cl-lib:f2cl-// side trans) m nh nh
                     -1))))
         (setf lwkopt
                 (f2cl-lib:int-mul
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 nw))
                  nb))
         (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                 (coerce lwkopt 'f2cl-lib:complex16))))
      (cond
        ((/= info 0)
         (xerbla "ZUNMHR" (f2cl-lib:int-sub info))
         (go end_label))
        (lquery
         (go end_label)))
      (cond
        ((or (= m 0) (= n 0) (= nh 0))
         (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                 (coerce 1 'f2cl-lib:complex16))
         (go end_label)))
      (cond
        (left
         (setf mi nh)
         (setf ni n)
         (setf i1 (f2cl-lib:int-add ilo 1))
         (setf i2 1))
        (t
         (setf mi m)
         (setf ni nh)
         (setf i1 1)
         (setf i2 (f2cl-lib:int-add ilo 1))))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12)
          (zunmqr side trans mi ni nh
           (f2cl-lib:array-slice a-%data%
                                 f2cl-lib:complex16
                                 ((+ ilo 1) ilo)
                                 ((1 lda) (1 *))
                                 a-%offset%)
           lda
           (f2cl-lib:array-slice tau-%data%
                                 f2cl-lib:complex16
                                 (ilo)
                                 ((1 *))
                                 tau-%offset%)
           (f2cl-lib:array-slice c-%data%
                                 f2cl-lib:complex16
                                 (i1 i2)
                                 ((1 ldc) (1 *))
                                 c-%offset%)
           ldc work lwork iinfo)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                         var-9 var-10 var-11))
        (setf iinfo var-12))
      (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
              (coerce lwkopt 'f2cl-lib:complex16))
      (go end_label)
     end_label
      (return
       (values nil nil nil nil nil nil nil nil nil nil nil nil nil info)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zunmhr
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::zunmqr fortran-to-lisp::xerbla
                    fortran-to-lisp::ilaenv fortran-to-lisp::lsame))))

