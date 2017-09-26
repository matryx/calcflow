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


(defun dgeqrf (m n a lda tau work lwork info)
  (declare (type (array double-float (*)) work tau a)
           (type (f2cl-lib:integer4) info lwork lda n m))
  (f2cl-lib:with-multi-array-data
      ((a double-float a-%data% a-%offset%)
       (tau double-float tau-%data% tau-%offset%)
       (work double-float work-%data% work-%offset%))
    (prog ((i 0) (ib 0) (iinfo 0) (iws 0) (k 0) (ldwork 0) (lwkopt 0) (nb 0)
           (nbmin 0) (nx 0) (lquery nil))
      (declare (type f2cl-lib:logical lquery)
               (type (f2cl-lib:integer4) nx nbmin nb lwkopt ldwork k iws iinfo
                                         ib i))
      (setf info 0)
      (setf nb (ilaenv 1 "DGEQRF" " " m n -1 -1))
      (setf lwkopt (f2cl-lib:int-mul n nb))
      (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
              (coerce (the f2cl-lib:integer4 lwkopt) 'double-float))
      (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
      (cond
        ((< m 0)
         (setf info -1))
        ((< n 0)
         (setf info -2))
        ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
         (setf info -4))
        ((and
          (< lwork (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
          (not lquery))
         (setf info -7)))
      (cond
        ((/= info 0)
         (xerbla "DGEQRF" (f2cl-lib:int-sub info))
         (go end_label))
        (lquery
         (go end_label)))
      (setf k (min (the f2cl-lib:integer4 m) (the f2cl-lib:integer4 n)))
      (cond
        ((= k 0)
         (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                 (coerce (the f2cl-lib:integer4 1) 'double-float))
         (go end_label)))
      (setf nbmin 2)
      (setf nx 0)
      (setf iws n)
      (cond
        ((and (> nb 1) (< nb k))
         (setf nx
                 (max (the f2cl-lib:integer4 0)
                      (the f2cl-lib:integer4
                           (ilaenv 3 "DGEQRF" " " m n -1 -1))))
         (cond
           ((< nx k)
            (setf ldwork n)
            (setf iws (f2cl-lib:int-mul ldwork nb))
            (cond
              ((< lwork iws)
               (setf nb (the f2cl-lib:integer4 (truncate lwork ldwork)))
               (setf nbmin
                       (max (the f2cl-lib:integer4 2)
                            (the f2cl-lib:integer4
                                 (ilaenv 2 "DGEQRF" " " m n -1 -1))))))))))
      (cond
        ((and (>= nb nbmin) (< nb k) (< nx k))
         (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i nb))
                       ((> i (f2cl-lib:int-add k (f2cl-lib:int-sub nx))) nil)
           (tagbody
             (setf ib
                     (min
                      (the f2cl-lib:integer4
                           (f2cl-lib:int-add (f2cl-lib:int-sub k i) 1))
                      (the f2cl-lib:integer4 nb)))
             (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                 (dgeqr2 (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1) ib
                  (f2cl-lib:array-slice a-%data%
                                        double-float
                                        (i i)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                  lda
                  (f2cl-lib:array-slice tau-%data%
                                        double-float
                                        (i)
                                        ((1 *))
                                        tau-%offset%)
                  work iinfo)
               (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5))
               (setf iinfo var-6))
             (cond
               ((<= (f2cl-lib:int-add i ib) n)
                (dlarft "Forward" "Columnwise"
                 (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1) ib
                 (f2cl-lib:array-slice a-%data%
                                       double-float
                                       (i i)
                                       ((1 lda) (1 *))
                                       a-%offset%)
                 lda
                 (f2cl-lib:array-slice tau-%data%
                                       double-float
                                       (i)
                                       ((1 *))
                                       tau-%offset%)
                 work ldwork)
                (dlarfb "Left" "Transpose" "Forward" "Columnwise"
                 (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1)
                 (f2cl-lib:int-add (f2cl-lib:int-sub n i ib) 1) ib
                 (f2cl-lib:array-slice a-%data%
                                       double-float
                                       (i i)
                                       ((1 lda) (1 *))
                                       a-%offset%)
                 lda work ldwork
                 (f2cl-lib:array-slice a-%data%
                                       double-float
                                       (i (f2cl-lib:int-add i ib))
                                       ((1 lda) (1 *))
                                       a-%offset%)
                 lda
                 (f2cl-lib:array-slice work-%data%
                                       double-float
                                       ((+ ib 1))
                                       ((1 *))
                                       work-%offset%)
                 ldwork)))
            label10)))
        (t
         (setf i 1)))
      (if (<= i k)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dgeqr2 (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1)
               (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1)
               (f2cl-lib:array-slice a-%data%
                                     double-float
                                     (i i)
                                     ((1 lda) (1 *))
                                     a-%offset%)
               lda
               (f2cl-lib:array-slice tau-%data%
                                     double-float
                                     (i)
                                     ((1 *))
                                     tau-%offset%)
               work iinfo)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5))
            (setf iinfo var-6)))
      (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
              (coerce (the f2cl-lib:integer4 iws) 'double-float))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil info)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgeqrf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlarfb fortran-to-lisp::dlarft
                    fortran-to-lisp::dgeqr2 fortran-to-lisp::xerbla
                    fortran-to-lisp::ilaenv))))

