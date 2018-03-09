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


(let* ((zero (f2cl-lib:cmplx 0.0 0.0)) (one (f2cl-lib:cmplx 1.0 0.0)))
  (declare (type (f2cl-lib:complex16) zero)
           (type (f2cl-lib:complex16) one)
           (ignorable zero one))
  (defun zunghr (n ilo ihi a lda tau work lwork info)
    (declare (type (array f2cl-lib:complex16 (*)) work tau a)
             (type (f2cl-lib:integer4) info lwork lda ihi ilo n))
    (f2cl-lib:with-multi-array-data
        ((a f2cl-lib:complex16 a-%data% a-%offset%)
         (tau f2cl-lib:complex16 tau-%data% tau-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%))
      (prog ((i 0) (iinfo 0) (j 0) (lwkopt 0) (nb 0) (nh 0) (lquery nil))
        (declare (type (f2cl-lib:integer4) i iinfo j lwkopt nb nh)
                 (type f2cl-lib:logical lquery))
        (setf info 0)
        (setf nh (f2cl-lib:int-sub ihi ilo))
        (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
        (cond
          ((< n 0)
           (setf info -1))
          ((or (< ilo 1)
               (> ilo
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n))))
           (setf info -2))
          ((or
            (< ihi (min (the f2cl-lib:integer4 ilo) (the f2cl-lib:integer4 n)))
            (> ihi n))
           (setf info -3))
          ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
           (setf info -5))
          ((and
            (< lwork
               (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 nh)))
            (not lquery))
           (setf info -8)))
        (cond
          ((= info 0)
           (setf nb (ilaenv 1 "ZUNGQR" " " nh nh nh -1))
           (setf lwkopt
                   (f2cl-lib:int-mul
                    (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 nh))
                    nb))
           (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                   (coerce lwkopt 'f2cl-lib:complex16))))
        (cond
          ((/= info 0)
           (xerbla "ZUNGHR" (f2cl-lib:int-sub info))
           (go end_label))
          (lquery
           (go end_label)))
        (cond
          ((= n 0)
           (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                   (coerce 1 'f2cl-lib:complex16))
           (go end_label)))
        (f2cl-lib:fdo (j ihi (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                      ((> j (f2cl-lib:int-add ilo 1)) nil)
          (tagbody
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i (f2cl-lib:int-add j (f2cl-lib:int-sub 1))) nil)
              (tagbody
                (setf (f2cl-lib:fref a-%data% (i j) ((1 lda) (1 *)) a-%offset%)
                        zero)
               label10))
            (f2cl-lib:fdo (i (f2cl-lib:int-add j 1) (f2cl-lib:int-add i 1))
                          ((> i ihi) nil)
              (tagbody
                (setf (f2cl-lib:fref a-%data% (i j) ((1 lda) (1 *)) a-%offset%)
                        (f2cl-lib:fref a-%data%
                                       (i (f2cl-lib:int-sub j 1))
                                       ((1 lda) (1 *))
                                       a-%offset%))
               label20))
            (f2cl-lib:fdo (i (f2cl-lib:int-add ihi 1) (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf (f2cl-lib:fref a-%data% (i j) ((1 lda) (1 *)) a-%offset%)
                        zero)
               label30))
           label40))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j ilo) nil)
          (tagbody
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf (f2cl-lib:fref a-%data% (i j) ((1 lda) (1 *)) a-%offset%)
                        zero)
               label50))
            (setf (f2cl-lib:fref a-%data% (j j) ((1 lda) (1 *)) a-%offset%)
                    one)
           label60))
        (f2cl-lib:fdo (j (f2cl-lib:int-add ihi 1) (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf (f2cl-lib:fref a-%data% (i j) ((1 lda) (1 *)) a-%offset%)
                        zero)
               label70))
            (setf (f2cl-lib:fref a-%data% (j j) ((1 lda) (1 *)) a-%offset%)
                    one)
           label80))
        (cond
          ((> nh 0)
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
               (zungqr nh nh nh
                (f2cl-lib:array-slice a-%data%
                                      f2cl-lib:complex16
                                      ((+ ilo 1) (f2cl-lib:int-add ilo 1))
                                      ((1 lda) (1 *))
                                      a-%offset%)
                lda
                (f2cl-lib:array-slice tau-%data%
                                      f2cl-lib:complex16
                                      (ilo)
                                      ((1 *))
                                      tau-%offset%)
                work lwork iinfo)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
             (setf iinfo var-8))))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce lwkopt 'f2cl-lib:complex16))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zunghr
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::zungqr fortran-to-lisp::xerbla
                    fortran-to-lisp::ilaenv))))

