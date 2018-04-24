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


(defun dlaswp (n a lda k1 k2 ipiv incx)
  (declare (type (array f2cl-lib:integer4 (*)) ipiv)
           (type (array double-float (*)) a)
           (type (f2cl-lib:integer4) incx k2 k1 lda n))
  (f2cl-lib:with-multi-array-data
      ((a double-float a-%data% a-%offset%)
       (ipiv f2cl-lib:integer4 ipiv-%data% ipiv-%offset%))
    (prog ((temp 0.0) (i 0) (i1 0) (i2 0) (inc 0) (ip 0) (ix 0) (ix0 0) (j 0)
           (k 0) (n32 0))
      (declare (type (f2cl-lib:integer4) n32 k j ix0 ix ip inc i2 i1 i)
               (type (double-float) temp))
      (cond
        ((> incx 0)
         (setf ix0 k1)
         (setf i1 k1)
         (setf i2 k2)
         (setf inc 1))
        ((< incx 0)
         (setf ix0
                 (f2cl-lib:int-add 1
                                   (f2cl-lib:int-mul (f2cl-lib:int-sub 1 k2)
                                                     incx)))
         (setf i1 k2)
         (setf i2 k1)
         (setf inc -1))
        (t
         (go end_label)))
      (setf n32 (* (the f2cl-lib:integer4 (truncate n 32)) 32))
      (cond
        ((/= n32 0)
         (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 32))
                       ((> j n32) nil)
           (tagbody
             (setf ix ix0)
             (f2cl-lib:fdo (i i1 (f2cl-lib:int-add i inc))
                           ((> i i2) nil)
               (tagbody
                 (setf ip
                         (f2cl-lib:fref ipiv-%data%
                                        (ix)
                                        ((1 *))
                                        ipiv-%offset%))
                 (cond
                   ((/= ip i)
                    (f2cl-lib:fdo (k j (f2cl-lib:int-add k 1))
                                  ((> k (f2cl-lib:int-add j 31)) nil)
                      (tagbody
                        (setf temp
                                (f2cl-lib:fref a-%data%
                                               (i k)
                                               ((1 lda) (1 *))
                                               a-%offset%))
                        (setf (f2cl-lib:fref a-%data%
                                             (i k)
                                             ((1 lda) (1 *))
                                             a-%offset%)
                                (f2cl-lib:fref a-%data%
                                               (ip k)
                                               ((1 lda) (1 *))
                                               a-%offset%))
                        (setf (f2cl-lib:fref a-%data%
                                             (ip k)
                                             ((1 lda) (1 *))
                                             a-%offset%)
                                temp)
                       label10))))
                 (setf ix (f2cl-lib:int-add ix incx))
                label20))
            label30))))
      (cond
        ((/= n32 n)
         (setf n32 (f2cl-lib:int-add n32 1))
         (setf ix ix0)
         (f2cl-lib:fdo (i i1 (f2cl-lib:int-add i inc))
                       ((> i i2) nil)
           (tagbody
             (setf ip (f2cl-lib:fref ipiv-%data% (ix) ((1 *)) ipiv-%offset%))
             (cond
               ((/= ip i)
                (f2cl-lib:fdo (k n32 (f2cl-lib:int-add k 1))
                              ((> k n) nil)
                  (tagbody
                    (setf temp
                            (f2cl-lib:fref a-%data%
                                           (i k)
                                           ((1 lda) (1 *))
                                           a-%offset%))
                    (setf (f2cl-lib:fref a-%data%
                                         (i k)
                                         ((1 lda) (1 *))
                                         a-%offset%)
                            (f2cl-lib:fref a-%data%
                                           (ip k)
                                           ((1 lda) (1 *))
                                           a-%offset%))
                    (setf (f2cl-lib:fref a-%data%
                                         (ip k)
                                         ((1 lda) (1 *))
                                         a-%offset%)
                            temp)
                   label40))))
             (setf ix (f2cl-lib:int-add ix incx))
            label50))))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlaswp
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil)
           :calls 'nil)))

