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


(let* ((zero 0.0) (one 1.0))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (ignorable zero one))
  (defun dorgbr (vect m n k a lda tau work lwork info)
    (declare (type (array double-float (*)) work tau a)
             (type (f2cl-lib:integer4) info lwork lda k n m)
             (type (simple-string *) vect))
    (f2cl-lib:with-multi-array-data
        ((vect character vect-%data% vect-%offset%)
         (a double-float a-%data% a-%offset%)
         (tau double-float tau-%data% tau-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((i 0) (iinfo 0) (j 0) (lwkopt 0) (mn 0) (nb 0) (lquery nil)
             (wantq nil))
        (declare (type (f2cl-lib:integer4) i iinfo j lwkopt mn nb)
                 (type f2cl-lib:logical lquery wantq))
        (setf info 0)
        (setf wantq (lsame vect "Q"))
        (setf mn (min (the f2cl-lib:integer4 m) (the f2cl-lib:integer4 n)))
        (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
        (cond
          ((and (not wantq) (not (lsame vect "P")))
           (setf info -1))
          ((< m 0)
           (setf info -2))
          ((or (< n 0)
               (and wantq
                    (or (> n m)
                        (< n
                           (min (the f2cl-lib:integer4 m)
                                (the f2cl-lib:integer4 k)))))
               (and (not wantq)
                    (or (> m n)
                        (< m
                           (min (the f2cl-lib:integer4 n)
                                (the f2cl-lib:integer4 k))))))
           (setf info -3))
          ((< k 0)
           (setf info -4))
          ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
           (setf info -6))
          ((and
            (< lwork
               (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 mn)))
            (not lquery))
           (setf info -9)))
        (cond
          ((= info 0)
           (cond
             (wantq
              (setf nb (ilaenv 1 "DORGQR" " " m n k -1)))
             (t
              (setf nb (ilaenv 1 "DORGLQ" " " m n k -1))))
           (setf lwkopt
                   (f2cl-lib:int-mul
                    (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 mn))
                    nb))
           (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                   (coerce (the f2cl-lib:integer4 lwkopt) 'double-float))))
        (cond
          ((/= info 0)
           (xerbla "DORGBR" (f2cl-lib:int-sub info))
           (go end_label))
          (lquery
           (go end_label)))
        (cond
          ((or (= m 0) (= n 0))
           (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                   (coerce (the f2cl-lib:integer4 1) 'double-float))
           (go end_label)))
        (cond
          (wantq
           (cond
             ((>= m k)
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                  (dorgqr m n k a lda tau work lwork iinfo)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7))
                (setf iinfo var-8)))
             (t
              (f2cl-lib:fdo (j m (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                            ((> j 2) nil)
                (tagbody
                  (setf (f2cl-lib:fref a-%data%
                                       (1 j)
                                       ((1 lda) (1 *))
                                       a-%offset%)
                          zero)
                  (f2cl-lib:fdo (i (f2cl-lib:int-add j 1)
                                 (f2cl-lib:int-add i 1))
                                ((> i m) nil)
                    (tagbody
                      (setf (f2cl-lib:fref a-%data%
                                           (i j)
                                           ((1 lda) (1 *))
                                           a-%offset%)
                              (f2cl-lib:fref a-%data%
                                             (i (f2cl-lib:int-sub j 1))
                                             ((1 lda) (1 *))
                                             a-%offset%))
                     label10))
                 label20))
              (setf (f2cl-lib:fref a-%data% (1 1) ((1 lda) (1 *)) a-%offset%)
                      one)
              (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                            ((> i m) nil)
                (tagbody
                  (setf (f2cl-lib:fref a-%data%
                                       (i 1)
                                       ((1 lda) (1 *))
                                       a-%offset%)
                          zero)
                 label30))
              (cond
                ((> m 1)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                     (dorgqr (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                      (f2cl-lib:int-sub m 1)
                      (f2cl-lib:array-slice a-%data%
                                            double-float
                                            (2 2)
                                            ((1 lda) (1 *))
                                            a-%offset%)
                      lda tau work lwork iinfo)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7))
                   (setf iinfo var-8)))))))
          (t
           (cond
             ((< k n)
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                  (dorglq m n k a lda tau work lwork iinfo)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7))
                (setf iinfo var-8)))
             (t
              (setf (f2cl-lib:fref a-%data% (1 1) ((1 lda) (1 *)) a-%offset%)
                      one)
              (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                            ((> i n) nil)
                (tagbody
                  (setf (f2cl-lib:fref a-%data%
                                       (i 1)
                                       ((1 lda) (1 *))
                                       a-%offset%)
                          zero)
                 label40))
              (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (f2cl-lib:fdo (i (f2cl-lib:int-add j (f2cl-lib:int-sub 1))
                                 (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                                ((> i 2) nil)
                    (tagbody
                      (setf (f2cl-lib:fref a-%data%
                                           (i j)
                                           ((1 lda) (1 *))
                                           a-%offset%)
                              (f2cl-lib:fref a-%data%
                                             ((f2cl-lib:int-sub i 1) j)
                                             ((1 lda) (1 *))
                                             a-%offset%))
                     label50))
                  (setf (f2cl-lib:fref a-%data%
                                       (1 j)
                                       ((1 lda) (1 *))
                                       a-%offset%)
                          zero)
                 label60))
              (cond
                ((> n 1)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                     (dorglq (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                      (f2cl-lib:int-sub n 1)
                      (f2cl-lib:array-slice a-%data%
                                            double-float
                                            (2 2)
                                            ((1 lda) (1 *))
                                            a-%offset%)
                      lda tau work lwork iinfo)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7))
                   (setf iinfo var-8))))))))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce (the f2cl-lib:integer4 lwkopt) 'double-float))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dorgbr
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dorglq fortran-to-lisp::dorgqr
                    fortran-to-lisp::xerbla fortran-to-lisp::ilaenv
                    fortran-to-lisp::lsame))))

