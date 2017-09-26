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


(let* ((nbmax 64)
       (ldt (+ nbmax 1))
       (zero (f2cl-lib:cmplx 0.0 0.0))
       (one (f2cl-lib:cmplx 1.0 0.0)))
  (declare (type (f2cl-lib:integer4 64 64) nbmax)
           (type (f2cl-lib:integer4) ldt)
           (type (f2cl-lib:complex16) zero)
           (type (f2cl-lib:complex16) one)
           (ignorable nbmax ldt zero one))
  (defun zgehrd (n ilo ihi a lda tau work lwork info)
    (declare (type (array f2cl-lib:complex16 (*)) work tau a)
             (type (f2cl-lib:integer4) info lwork lda ihi ilo n))
    (f2cl-lib:with-multi-array-data
        ((a f2cl-lib:complex16 a-%data% a-%offset%)
         (tau f2cl-lib:complex16 tau-%data% tau-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%))
      (prog ((ei #C(0.0 0.0)) (i 0) (ib 0) (iinfo 0) (iws 0) (j 0) (ldwork 0)
             (lwkopt 0) (nb 0) (nbmin 0) (nh 0) (nx 0) (lquery nil)
             (t$
              (make-array (the fixnum (reduce #'* (list ldt nbmax)))
                          :element-type 'f2cl-lib:complex16)))
        (declare (type (array f2cl-lib:complex16 (*)) t$)
                 (type (f2cl-lib:complex16) ei)
                 (type (f2cl-lib:integer4) i ib iinfo iws j ldwork lwkopt nb
                                           nbmin nh nx)
                 (type f2cl-lib:logical lquery))
        (setf info 0)
        (setf nb
                (min (the f2cl-lib:integer4 nbmax)
                     (the f2cl-lib:integer4
                          (ilaenv 1 "ZGEHRD" " " n ilo ihi -1))))
        (setf lwkopt (f2cl-lib:int-mul n nb))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce lwkopt 'f2cl-lib:complex16))
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
            (< lwork (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
            (not lquery))
           (setf info -8)))
        (cond
          ((/= info 0)
           (xerbla "ZGEHRD" (f2cl-lib:int-sub info))
           (go end_label))
          (lquery
           (go end_label)))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i (f2cl-lib:int-add ilo (f2cl-lib:int-sub 1))) nil)
          (tagbody
            (setf (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%) zero)
           label10))
        (f2cl-lib:fdo (i
                       (max (the f2cl-lib:integer4 1)
                            (the f2cl-lib:integer4 ihi))
                       (f2cl-lib:int-add i 1))
                      ((> i (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
          (tagbody
            (setf (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%) zero)
           label20))
        (setf nh (f2cl-lib:int-add (f2cl-lib:int-sub ihi ilo) 1))
        (cond
          ((<= nh 1)
           (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                   (coerce 1 'f2cl-lib:complex16))
           (go end_label)))
        (setf nb
                (min (the f2cl-lib:integer4 nbmax)
                     (the f2cl-lib:integer4
                          (ilaenv 1 "ZGEHRD" " " n ilo ihi -1))))
        (setf nbmin 2)
        (setf iws 1)
        (cond
          ((and (> nb 1) (< nb nh))
           (setf nx
                   (max (the f2cl-lib:integer4 nb)
                        (the f2cl-lib:integer4
                             (ilaenv 3 "ZGEHRD" " " n ilo ihi -1))))
           (cond
             ((< nx nh)
              (setf iws (f2cl-lib:int-mul n nb))
              (cond
                ((< lwork iws)
                 (setf nbmin
                         (max (the f2cl-lib:integer4 2)
                              (the f2cl-lib:integer4
                                   (ilaenv 2 "ZGEHRD" " " n ilo ihi -1))))
                 (cond
                   ((>= lwork (f2cl-lib:int-mul n nbmin))
                    (setf nb (the f2cl-lib:integer4 (truncate lwork n))))
                   (t
                    (setf nb 1)))))))))
        (setf ldwork n)
        (cond
          ((or (< nb nbmin) (>= nb nh))
           (setf i ilo))
          (t
           (f2cl-lib:fdo (i ilo (f2cl-lib:int-add i nb))
                         ((> i
                             (f2cl-lib:int-add ihi
                                               (f2cl-lib:int-sub 1)
                                               (f2cl-lib:int-sub nx)))
                          nil)
             (tagbody
               (setf ib
                       (min (the f2cl-lib:integer4 nb)
                            (the f2cl-lib:integer4 (f2cl-lib:int-sub ihi i))))
               (zlahr2 ihi i ib
                (f2cl-lib:array-slice a-%data%
                                      f2cl-lib:complex16
                                      (1 i)
                                      ((1 lda) (1 *))
                                      a-%offset%)
                lda
                (f2cl-lib:array-slice tau-%data%
                                      f2cl-lib:complex16
                                      (i)
                                      ((1 *))
                                      tau-%offset%)
                t$ ldt work ldwork)
               (setf ei
                       (f2cl-lib:fref a-%data%
                                      ((f2cl-lib:int-add i ib)
                                       (f2cl-lib:int-sub
                                        (f2cl-lib:int-add i ib)
                                        1))
                                      ((1 lda) (1 *))
                                      a-%offset%))
               (setf (f2cl-lib:fref a-%data%
                                    ((f2cl-lib:int-add i ib)
                                     (f2cl-lib:int-sub (f2cl-lib:int-add i ib)
                                                       1))
                                    ((1 lda) (1 *))
                                    a-%offset%)
                       one)
               (zgemm "No transpose" "Conjugate transpose" ihi
                (f2cl-lib:int-add (f2cl-lib:int-sub ihi i ib) 1) ib (- one)
                work ldwork
                (f2cl-lib:array-slice a-%data%
                                      f2cl-lib:complex16
                                      ((+ i ib) i)
                                      ((1 lda) (1 *))
                                      a-%offset%)
                lda one
                (f2cl-lib:array-slice a-%data%
                                      f2cl-lib:complex16
                                      (1 (f2cl-lib:int-add i ib))
                                      ((1 lda) (1 *))
                                      a-%offset%)
                lda)
               (setf (f2cl-lib:fref a-%data%
                                    ((f2cl-lib:int-add i ib)
                                     (f2cl-lib:int-sub (f2cl-lib:int-add i ib)
                                                       1))
                                    ((1 lda) (1 *))
                                    a-%offset%)
                       ei)
               (ztrmm "Right" "Lower" "Conjugate transpose" "Unit" i
                (f2cl-lib:int-sub ib 1) one
                (f2cl-lib:array-slice a-%data%
                                      f2cl-lib:complex16
                                      ((+ i 1) i)
                                      ((1 lda) (1 *))
                                      a-%offset%)
                lda work ldwork)
               (f2cl-lib:fdo (j 0 (f2cl-lib:int-add j 1))
                             ((> j (f2cl-lib:int-add ib (f2cl-lib:int-sub 2)))
                              nil)
                 (tagbody
                   (zaxpy i (- one)
                    (f2cl-lib:array-slice work-%data%
                                          f2cl-lib:complex16
                                          ((+ (f2cl-lib:int-mul ldwork j) 1))
                                          ((1 *))
                                          work-%offset%)
                    1
                    (f2cl-lib:array-slice a-%data%
                                          f2cl-lib:complex16
                                          (1 (f2cl-lib:int-add i j 1))
                                          ((1 lda) (1 *))
                                          a-%offset%)
                    1)
                  label30))
               (zlarfb "Left" "Conjugate transpose" "Forward" "Columnwise"
                (f2cl-lib:int-sub ihi i)
                (f2cl-lib:int-add (f2cl-lib:int-sub n i ib) 1) ib
                (f2cl-lib:array-slice a-%data%
                                      f2cl-lib:complex16
                                      ((+ i 1) i)
                                      ((1 lda) (1 *))
                                      a-%offset%)
                lda t$ ldt
                (f2cl-lib:array-slice a-%data%
                                      f2cl-lib:complex16
                                      ((+ i 1) (f2cl-lib:int-add i ib))
                                      ((1 lda) (1 *))
                                      a-%offset%)
                lda work ldwork)
              label40))))
        (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
            (zgehd2 n i ihi a lda tau work iinfo)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
          (setf iinfo var-7))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce iws 'f2cl-lib:complex16))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zgehrd
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
           :calls '(fortran-to-lisp::zgehd2 fortran-to-lisp::zlarfb
                    fortran-to-lisp::zaxpy fortran-to-lisp::ztrmm
                    fortran-to-lisp::zgemm fortran-to-lisp::zlahr2
                    fortran-to-lisp::xerbla fortran-to-lisp::ilaenv))))

