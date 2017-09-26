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


(let* ((zero 0.0))
  (declare (type (double-float 0.0 0.0) zero) (ignorable zero))
  (defun ddisna (job m n d sep info)
    (declare (type (array double-float (*)) sep d)
             (type (f2cl-lib:integer4) info n m)
             (type (simple-string *) job))
    (f2cl-lib:with-multi-array-data
        ((job character job-%data% job-%offset%)
         (d double-float d-%data% d-%offset%)
         (sep double-float sep-%data% sep-%offset%))
      (prog ((anorm 0.0) (eps 0.0) (newgap 0.0) (oldgap 0.0) (safmin 0.0)
             (thresh 0.0) (i 0) (k 0) (decr nil) (eigen nil) (incr nil)
             (left nil) (right nil) (sing nil))
        (declare (type (double-float) anorm eps newgap oldgap safmin thresh)
                 (type (f2cl-lib:integer4) i k)
                 (type f2cl-lib:logical decr eigen incr left right sing))
        (setf info 0)
        (setf eigen (lsame job "E"))
        (setf left (lsame job "L"))
        (setf right (lsame job "R"))
        (setf sing (or left right))
        (cond
          (eigen
           (setf k m))
          (sing
           (setf k (min (the f2cl-lib:integer4 m) (the f2cl-lib:integer4 n)))))
        (cond
          ((and (not eigen) (not sing))
           (setf info -1))
          ((< m 0)
           (setf info -2))
          ((< k 0)
           (setf info -3))
          (t
           (setf incr f2cl-lib:%true%)
           (setf decr f2cl-lib:%true%)
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i (f2cl-lib:int-add k (f2cl-lib:int-sub 1))) nil)
             (tagbody
               (if incr
                   (setf incr
                           (and incr
                                (<=
                                 (f2cl-lib:fref d-%data%
                                                (i)
                                                ((1 *))
                                                d-%offset%)
                                 (f2cl-lib:fref d-%data%
                                                ((f2cl-lib:int-add i 1))
                                                ((1 *))
                                                d-%offset%)))))
               (if decr
                   (setf decr
                           (and decr
                                (>=
                                 (f2cl-lib:fref d-%data%
                                                (i)
                                                ((1 *))
                                                d-%offset%)
                                 (f2cl-lib:fref d-%data%
                                                ((f2cl-lib:int-add i 1))
                                                ((1 *))
                                                d-%offset%)))))
              label10))
           (cond
             ((and sing (> k 0))
              (if incr
                  (setf incr
                          (and incr
                               (<= zero
                                   (f2cl-lib:fref d-%data%
                                                  (1)
                                                  ((1 *))
                                                  d-%offset%)))))
              (if decr
                  (setf decr
                          (and decr
                               (>=
                                (f2cl-lib:fref d-%data% (k) ((1 *)) d-%offset%)
                                zero))))))
           (if (not (or incr decr)) (setf info -4))))
        (cond
          ((/= info 0)
           (xerbla "DDISNA" (f2cl-lib:int-sub info))
           (go end_label)))
        (if (= k 0) (go end_label))
        (cond
          ((= k 1)
           (setf (f2cl-lib:fref sep-%data% (1) ((1 *)) sep-%offset%)
                   (dlamch "O")))
          (t
           (setf oldgap
                   (abs
                    (- (f2cl-lib:fref d-%data% (2) ((1 *)) d-%offset%)
                       (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%))))
           (setf (f2cl-lib:fref sep-%data% (1) ((1 *)) sep-%offset%) oldgap)
           (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                         ((> i (f2cl-lib:int-add k (f2cl-lib:int-sub 1))) nil)
             (tagbody
               (setf newgap
                       (abs
                        (-
                         (f2cl-lib:fref d-%data%
                                        ((f2cl-lib:int-add i 1))
                                        ((1 *))
                                        d-%offset%)
                         (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))))
               (setf (f2cl-lib:fref sep-%data% (i) ((1 *)) sep-%offset%)
                       (min oldgap newgap))
               (setf oldgap newgap)
              label20))
           (setf (f2cl-lib:fref sep-%data% (k) ((1 *)) sep-%offset%) oldgap)))
        (cond
          (sing
           (cond
             ((or (and left (> m n)) (and right (< m n)))
              (if incr
                  (setf (f2cl-lib:fref sep-%data% (1) ((1 *)) sep-%offset%)
                          (min
                           (f2cl-lib:fref sep-%data% (1) ((1 *)) sep-%offset%)
                           (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%))))
              (if decr
                  (setf (f2cl-lib:fref sep-%data% (k) ((1 *)) sep-%offset%)
                          (min
                           (f2cl-lib:fref sep-%data% (k) ((1 *)) sep-%offset%)
                           (f2cl-lib:fref d-%data%
                                          (k)
                                          ((1 *))
                                          d-%offset%))))))))
        (setf eps (dlamch "E"))
        (setf safmin (dlamch "S"))
        (setf anorm
                (max (abs (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%))
                     (abs (f2cl-lib:fref d-%data% (k) ((1 *)) d-%offset%))))
        (cond
          ((= anorm zero)
           (setf thresh eps))
          (t
           (setf thresh (max (* eps anorm) safmin))))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i k) nil)
          (tagbody
            (setf (f2cl-lib:fref sep-%data% (i) ((1 *)) sep-%offset%)
                    (max (f2cl-lib:fref sep-%data% (i) ((1 *)) sep-%offset%)
                         thresh))
           label30))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::ddisna
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlamch fortran-to-lisp::xerbla
                    fortran-to-lisp::lsame))))

