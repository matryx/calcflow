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

(in-package :blas)


(let* ((one (f2cl-lib:cmplx 1.0 0.0)) (zero (f2cl-lib:cmplx 0.0 0.0)))
  (declare (type (f2cl-lib:complex16) one)
           (type (f2cl-lib:complex16) zero)
           (ignorable one zero))
  (defun zsymm (side uplo m n alpha a lda b ldb$ beta c ldc)
    (declare (type (array f2cl-lib:complex16 (*)) c b a)
             (type (f2cl-lib:complex16) beta alpha)
             (type (f2cl-lib:integer4) ldc ldb$ lda n m)
             (type (simple-string *) uplo side))
    (f2cl-lib:with-multi-array-data
        ((side character side-%data% side-%offset%)
         (uplo character uplo-%data% uplo-%offset%)
         (a f2cl-lib:complex16 a-%data% a-%offset%)
         (b f2cl-lib:complex16 b-%data% b-%offset%)
         (c f2cl-lib:complex16 c-%data% c-%offset%))
      (prog ((temp1 #C(0.0 0.0)) (temp2 #C(0.0 0.0)) (i 0) (info 0) (j 0) (k 0)
             (nrowa 0) (upper nil))
        (declare (type (f2cl-lib:complex16) temp1 temp2)
                 (type (f2cl-lib:integer4) i info j k nrowa)
                 (type f2cl-lib:logical upper))
        (cond
          ((lsame side "L")
           (setf nrowa m))
          (t
           (setf nrowa n)))
        (setf upper (lsame uplo "U"))
        (setf info 0)
        (cond
          ((and (not (lsame side "L")) (not (lsame side "R")))
           (setf info 1))
          ((and (not upper) (not (lsame uplo "L")))
           (setf info 2))
          ((< m 0)
           (setf info 3))
          ((< n 0)
           (setf info 4))
          ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 nrowa)))
           (setf info 7))
          ((< ldb$ (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
           (setf info 9))
          ((< ldc (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
           (setf info 12)))
        (cond
          ((/= info 0)
           (xerbla "ZSYMM " info)
           (go end_label)))
        (if (or (= m 0) (= n 0) (and (= alpha zero) (= beta one)))
            (go end_label))
        (cond
          ((= alpha zero)
           (cond
             ((= beta zero)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf (f2cl-lib:fref c-%data%
                                           (i j)
                                           ((1 ldc) (1 *))
                                           c-%offset%)
                              zero)
                     label10))
                 label20)))
             (t
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf (f2cl-lib:fref c-%data%
                                           (i j)
                                           ((1 ldc) (1 *))
                                           c-%offset%)
                              (* beta
                                 (f2cl-lib:fref c-%data%
                                                (i j)
                                                ((1 ldc) (1 *))
                                                c-%offset%)))
                     label30))
                 label40))))
           (go end_label)))
        (cond
          ((lsame side "L")
           (cond
             (upper
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf temp1
                              (* alpha
                                 (f2cl-lib:fref b-%data%
                                                (i j)
                                                ((1 ldb$) (1 *))
                                                b-%offset%)))
                      (setf temp2 zero)
                      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                    ((> k
                                        (f2cl-lib:int-add i
                                                          (f2cl-lib:int-sub
                                                           1)))
                                     nil)
                        (tagbody
                          (setf (f2cl-lib:fref c-%data%
                                               (k j)
                                               ((1 ldc) (1 *))
                                               c-%offset%)
                                  (+
                                   (f2cl-lib:fref c-%data%
                                                  (k j)
                                                  ((1 ldc) (1 *))
                                                  c-%offset%)
                                   (* temp1
                                      (f2cl-lib:fref a-%data%
                                                     (k i)
                                                     ((1 lda) (1 *))
                                                     a-%offset%))))
                          (setf temp2
                                  (+ temp2
                                     (*
                                      (f2cl-lib:fref b-%data%
                                                     (k j)
                                                     ((1 ldb$) (1 *))
                                                     b-%offset%)
                                      (f2cl-lib:fref a-%data%
                                                     (k i)
                                                     ((1 lda) (1 *))
                                                     a-%offset%))))
                         label50))
                      (cond
                        ((= beta zero)
                         (setf (f2cl-lib:fref c-%data%
                                              (i j)
                                              ((1 ldc) (1 *))
                                              c-%offset%)
                                 (+
                                  (* temp1
                                     (f2cl-lib:fref a-%data%
                                                    (i i)
                                                    ((1 lda) (1 *))
                                                    a-%offset%))
                                  (* alpha temp2))))
                        (t
                         (setf (f2cl-lib:fref c-%data%
                                              (i j)
                                              ((1 ldc) (1 *))
                                              c-%offset%)
                                 (+
                                  (* beta
                                     (f2cl-lib:fref c-%data%
                                                    (i j)
                                                    ((1 ldc) (1 *))
                                                    c-%offset%))
                                  (* temp1
                                     (f2cl-lib:fref a-%data%
                                                    (i i)
                                                    ((1 lda) (1 *))
                                                    a-%offset%))
                                  (* alpha temp2)))))
                     label60))
                 label70)))
             (t
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (f2cl-lib:fdo (i m (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                                ((> i 1) nil)
                    (tagbody
                      (setf temp1
                              (* alpha
                                 (f2cl-lib:fref b-%data%
                                                (i j)
                                                ((1 ldb$) (1 *))
                                                b-%offset%)))
                      (setf temp2 zero)
                      (f2cl-lib:fdo (k (f2cl-lib:int-add i 1)
                                     (f2cl-lib:int-add k 1))
                                    ((> k m) nil)
                        (tagbody
                          (setf (f2cl-lib:fref c-%data%
                                               (k j)
                                               ((1 ldc) (1 *))
                                               c-%offset%)
                                  (+
                                   (f2cl-lib:fref c-%data%
                                                  (k j)
                                                  ((1 ldc) (1 *))
                                                  c-%offset%)
                                   (* temp1
                                      (f2cl-lib:fref a-%data%
                                                     (k i)
                                                     ((1 lda) (1 *))
                                                     a-%offset%))))
                          (setf temp2
                                  (+ temp2
                                     (*
                                      (f2cl-lib:fref b-%data%
                                                     (k j)
                                                     ((1 ldb$) (1 *))
                                                     b-%offset%)
                                      (f2cl-lib:fref a-%data%
                                                     (k i)
                                                     ((1 lda) (1 *))
                                                     a-%offset%))))
                         label80))
                      (cond
                        ((= beta zero)
                         (setf (f2cl-lib:fref c-%data%
                                              (i j)
                                              ((1 ldc) (1 *))
                                              c-%offset%)
                                 (+
                                  (* temp1
                                     (f2cl-lib:fref a-%data%
                                                    (i i)
                                                    ((1 lda) (1 *))
                                                    a-%offset%))
                                  (* alpha temp2))))
                        (t
                         (setf (f2cl-lib:fref c-%data%
                                              (i j)
                                              ((1 ldc) (1 *))
                                              c-%offset%)
                                 (+
                                  (* beta
                                     (f2cl-lib:fref c-%data%
                                                    (i j)
                                                    ((1 ldc) (1 *))
                                                    c-%offset%))
                                  (* temp1
                                     (f2cl-lib:fref a-%data%
                                                    (i i)
                                                    ((1 lda) (1 *))
                                                    a-%offset%))
                                  (* alpha temp2)))))
                     label90))
                 label100)))))
          (t
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (setf temp1
                       (* alpha
                          (f2cl-lib:fref a-%data%
                                         (j j)
                                         ((1 lda) (1 *))
                                         a-%offset%)))
               (cond
                 ((= beta zero)
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf (f2cl-lib:fref c-%data%
                                           (i j)
                                           ((1 ldc) (1 *))
                                           c-%offset%)
                              (* temp1
                                 (f2cl-lib:fref b-%data%
                                                (i j)
                                                ((1 ldb$) (1 *))
                                                b-%offset%)))
                     label110)))
                 (t
                  (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf (f2cl-lib:fref c-%data%
                                           (i j)
                                           ((1 ldc) (1 *))
                                           c-%offset%)
                              (+
                               (* beta
                                  (f2cl-lib:fref c-%data%
                                                 (i j)
                                                 ((1 ldc) (1 *))
                                                 c-%offset%))
                               (* temp1
                                  (f2cl-lib:fref b-%data%
                                                 (i j)
                                                 ((1 ldb$) (1 *))
                                                 b-%offset%))))
                     label120))))
               (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                             ((> k (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                              nil)
                 (tagbody
                   (cond
                     (upper
                      (setf temp1
                              (* alpha
                                 (f2cl-lib:fref a-%data%
                                                (k j)
                                                ((1 lda) (1 *))
                                                a-%offset%))))
                     (t
                      (setf temp1
                              (* alpha
                                 (f2cl-lib:fref a-%data%
                                                (j k)
                                                ((1 lda) (1 *))
                                                a-%offset%)))))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i m) nil)
                     (tagbody
                       (setf (f2cl-lib:fref c-%data%
                                            (i j)
                                            ((1 ldc) (1 *))
                                            c-%offset%)
                               (+
                                (f2cl-lib:fref c-%data%
                                               (i j)
                                               ((1 ldc) (1 *))
                                               c-%offset%)
                                (* temp1
                                   (f2cl-lib:fref b-%data%
                                                  (i k)
                                                  ((1 ldb$) (1 *))
                                                  b-%offset%))))
                      label130))
                  label140))
               (f2cl-lib:fdo (k (f2cl-lib:int-add j 1) (f2cl-lib:int-add k 1))
                             ((> k n) nil)
                 (tagbody
                   (cond
                     (upper
                      (setf temp1
                              (* alpha
                                 (f2cl-lib:fref a-%data%
                                                (j k)
                                                ((1 lda) (1 *))
                                                a-%offset%))))
                     (t
                      (setf temp1
                              (* alpha
                                 (f2cl-lib:fref a-%data%
                                                (k j)
                                                ((1 lda) (1 *))
                                                a-%offset%)))))
                   (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                 ((> i m) nil)
                     (tagbody
                       (setf (f2cl-lib:fref c-%data%
                                            (i j)
                                            ((1 ldc) (1 *))
                                            c-%offset%)
                               (+
                                (f2cl-lib:fref c-%data%
                                               (i j)
                                               ((1 ldc) (1 *))
                                               c-%offset%)
                                (* temp1
                                   (f2cl-lib:fref b-%data%
                                                  (i k)
                                                  ((1 ldb$) (1 *))
                                                  b-%offset%))))
                      label150))
                  label160))
              label170))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zsymm fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::complex16)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::complex16)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::xerbla fortran-to-lisp::lsame))))

