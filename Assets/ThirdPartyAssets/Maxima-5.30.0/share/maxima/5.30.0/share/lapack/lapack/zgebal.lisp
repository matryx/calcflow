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


(let* ((zero 0.0) (one 1.0) (sclfac 2.0) (factor 0.95))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (type (double-float 2.0 2.0) sclfac)
           (type (double-float 0.95 0.95) factor)
           (ignorable zero one sclfac factor))
  (defun zgebal (job n a lda ilo ihi scale info)
    (declare (type (array double-float (*)) scale)
             (type (array f2cl-lib:complex16 (*)) a)
             (type (f2cl-lib:integer4) info ihi ilo lda n)
             (type (simple-string *) job))
    (f2cl-lib:with-multi-array-data
        ((job character job-%data% job-%offset%)
         (a f2cl-lib:complex16 a-%data% a-%offset%)
         (scale double-float scale-%data% scale-%offset%))
      (labels ((cabs1 (cdum)
                 (+ (abs (f2cl-lib:dble cdum)) (abs (f2cl-lib:dimag cdum)))))
        (declare (ftype (function (f2cl-lib:complex16)
                         (values double-float &rest t))
                        cabs1))
        (prog ((cdum #C(0.0 0.0)) (c 0.0) (ca 0.0) (f 0.0) (g 0.0) (r 0.0)
               (ra 0.0) (s 0.0) (sfmax1 0.0) (sfmax2 0.0) (sfmin1 0.0)
               (sfmin2 0.0) (i 0) (ica 0) (iexc 0) (ira 0) (j 0) (k 0) (l 0)
               (m 0) (noconv nil))
          (declare (type (f2cl-lib:complex16) cdum)
                   (type (double-float) c ca f g r ra s sfmax1 sfmax2 sfmin1
                                        sfmin2)
                   (type (f2cl-lib:integer4) i ica iexc ira j k l m)
                   (type f2cl-lib:logical noconv))
          (setf info 0)
          (cond
            ((and (not (lsame job "N"))
                  (not (lsame job "P"))
                  (not (lsame job "S"))
                  (not (lsame job "B")))
             (setf info -1))
            ((< n 0)
             (setf info -2))
            ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
             (setf info -4)))
          (cond
            ((/= info 0)
             (xerbla "ZGEBAL" (f2cl-lib:int-sub info))
             (go end_label)))
          (setf k 1)
          (setf l n)
          (if (= n 0) (go label210))
          (cond
            ((lsame job "N")
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i n) nil)
               (tagbody
                 (setf (f2cl-lib:fref scale-%data% (i) ((1 *)) scale-%offset%)
                         one)
                label10))
             (go label210)))
          (if (lsame job "S") (go label120))
          (go label50)
         label20
          (setf (f2cl-lib:fref scale-%data% (m) ((1 *)) scale-%offset%)
                  (coerce (the f2cl-lib:integer4 j) 'double-float))
          (if (= j m) (go label30))
          (zswap l
           (f2cl-lib:array-slice a-%data%
                                 f2cl-lib:complex16
                                 (1 j)
                                 ((1 lda) (1 *))
                                 a-%offset%)
           1
           (f2cl-lib:array-slice a-%data%
                                 f2cl-lib:complex16
                                 (1 m)
                                 ((1 lda) (1 *))
                                 a-%offset%)
           1)
          (zswap (f2cl-lib:int-add (f2cl-lib:int-sub n k) 1)
           (f2cl-lib:array-slice a-%data%
                                 f2cl-lib:complex16
                                 (j k)
                                 ((1 lda) (1 *))
                                 a-%offset%)
           lda
           (f2cl-lib:array-slice a-%data%
                                 f2cl-lib:complex16
                                 (m k)
                                 ((1 lda) (1 *))
                                 a-%offset%)
           lda)
         label30
          (f2cl-lib:computed-goto (label40 label80) iexc)
         label40
          (if (= l 1) (go label210))
          (setf l (f2cl-lib:int-sub l 1))
         label50
          (f2cl-lib:fdo (j l (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                        ((> j 1) nil)
            (tagbody
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i l) nil)
                (tagbody
                  (if (= i j) (go label60))
                  (if
                   (or
                    (/=
                     (f2cl-lib:dble
                      (f2cl-lib:fref a-%data%
                                     (j i)
                                     ((1 lda) (1 *))
                                     a-%offset%))
                     zero)
                    (/=
                     (f2cl-lib:dimag
                      (f2cl-lib:fref a-%data%
                                     (j i)
                                     ((1 lda) (1 *))
                                     a-%offset%))
                     zero))
                   (go label70))
                 label60))
              (setf m l)
              (setf iexc 1)
              (go label20)
             label70))
          (go label90)
         label80
          (setf k (f2cl-lib:int-add k 1))
         label90
          (f2cl-lib:fdo (j k (f2cl-lib:int-add j 1))
                        ((> j l) nil)
            (tagbody
              (f2cl-lib:fdo (i k (f2cl-lib:int-add i 1))
                            ((> i l) nil)
                (tagbody
                  (if (= i j) (go label100))
                  (if
                   (or
                    (/=
                     (f2cl-lib:dble
                      (f2cl-lib:fref a-%data%
                                     (i j)
                                     ((1 lda) (1 *))
                                     a-%offset%))
                     zero)
                    (/=
                     (f2cl-lib:dimag
                      (f2cl-lib:fref a-%data%
                                     (i j)
                                     ((1 lda) (1 *))
                                     a-%offset%))
                     zero))
                   (go label110))
                 label100))
              (setf m k)
              (setf iexc 2)
              (go label20)
             label110))
         label120
          (f2cl-lib:fdo (i k (f2cl-lib:int-add i 1))
                        ((> i l) nil)
            (tagbody
              (setf (f2cl-lib:fref scale-%data% (i) ((1 *)) scale-%offset%)
                      one)
             label130))
          (if (lsame job "P") (go label210))
          (setf sfmin1 (/ (dlamch "S") (dlamch "P")))
          (setf sfmax1 (/ one sfmin1))
          (setf sfmin2 (* sfmin1 sclfac))
          (setf sfmax2 (/ one sfmin2))
         label140
          (setf noconv f2cl-lib:%false%)
          (f2cl-lib:fdo (i k (f2cl-lib:int-add i 1))
                        ((> i l) nil)
            (tagbody
              (setf c zero)
              (setf r zero)
              (f2cl-lib:fdo (j k (f2cl-lib:int-add j 1))
                            ((> j l) nil)
                (tagbody
                  (if (= j i) (go label150))
                  (setf c
                          (+ c
                             (cabs1
                              (f2cl-lib:fref a-%data%
                                             (j i)
                                             ((1 lda) (1 *))
                                             a-%offset%))))
                  (setf r
                          (+ r
                             (cabs1
                              (f2cl-lib:fref a-%data%
                                             (i j)
                                             ((1 lda) (1 *))
                                             a-%offset%))))
                 label150))
              (setf ica
                      (izamax l
                       (f2cl-lib:array-slice a-%data%
                                             f2cl-lib:complex16
                                             (1 i)
                                             ((1 lda) (1 *))
                                             a-%offset%)
                       1))
              (setf ca
                      (abs
                       (f2cl-lib:fref a-%data%
                                      (ica i)
                                      ((1 lda) (1 *))
                                      a-%offset%)))
              (setf ira
                      (izamax (f2cl-lib:int-add (f2cl-lib:int-sub n k) 1)
                       (f2cl-lib:array-slice a-%data%
                                             f2cl-lib:complex16
                                             (i k)
                                             ((1 lda) (1 *))
                                             a-%offset%)
                       lda))
              (setf ra
                      (abs
                       (f2cl-lib:fref a-%data%
                                      (i
                                       (f2cl-lib:int-sub
                                        (f2cl-lib:int-add ira k)
                                        1))
                                      ((1 lda) (1 *))
                                      a-%offset%)))
              (if (or (= c zero) (= r zero)) (go label200))
              (setf g (/ r sclfac))
              (setf f one)
              (setf s (+ c r))
             label160
              (if
               (or (>= c g) (>= (max f c ca) sfmax2) (<= (min r g ra) sfmin2))
               (go label170))
              (cond
                ((disnan (+ c f ca r g ra))
                 (setf info -3)
                 (xerbla "ZGEBAL" (f2cl-lib:int-sub info))
                 (go end_label)))
              (setf f (* f sclfac))
              (setf c (* c sclfac))
              (setf ca (* ca sclfac))
              (setf r (/ r sclfac))
              (setf g (/ g sclfac))
              (setf ra (/ ra sclfac))
              (go label160)
             label170
              (setf g (/ c sclfac))
             label180
              (if
               (or (< g r) (>= (max r ra) sfmax2) (<= (min f c g ca) sfmin2))
               (go label190))
              (setf f (/ f sclfac))
              (setf c (/ c sclfac))
              (setf g (/ g sclfac))
              (setf ca (/ ca sclfac))
              (setf r (* r sclfac))
              (setf ra (* ra sclfac))
              (go label180)
             label190
              (if (>= (+ c r) (* factor s)) (go label200))
              (cond
                ((and (< f one) (< (f2cl-lib:fref scale (i) ((1 *))) one))
                 (if
                  (<=
                   (* f
                      (f2cl-lib:fref scale-%data% (i) ((1 *)) scale-%offset%))
                   sfmin1)
                  (go label200))))
              (cond
                ((and (> f one) (> (f2cl-lib:fref scale (i) ((1 *))) one))
                 (if
                  (>= (f2cl-lib:fref scale-%data% (i) ((1 *)) scale-%offset%)
                      (/ sfmax1 f))
                  (go label200))))
              (setf g (/ one f))
              (setf (f2cl-lib:fref scale-%data% (i) ((1 *)) scale-%offset%)
                      (*
                       (f2cl-lib:fref scale-%data% (i) ((1 *)) scale-%offset%)
                       f))
              (setf noconv f2cl-lib:%true%)
              (zdscal (f2cl-lib:int-add (f2cl-lib:int-sub n k) 1) g
               (f2cl-lib:array-slice a-%data%
                                     f2cl-lib:complex16
                                     (i k)
                                     ((1 lda) (1 *))
                                     a-%offset%)
               lda)
              (zdscal l f
               (f2cl-lib:array-slice a-%data%
                                     f2cl-lib:complex16
                                     (1 i)
                                     ((1 lda) (1 *))
                                     a-%offset%)
               1)
             label200))
          (if noconv (go label140))
         label210
          (setf ilo k)
          (setf ihi l)
          (go end_label)
         end_label
          (return (values nil nil nil nil ilo ihi nil info)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zgebal
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil fortran-to-lisp::ilo
                            fortran-to-lisp::ihi nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::zdscal fortran-to-lisp::disnan
                    fortran-to-lisp::izamax fortran-to-lisp::dlamch
                    fortran-to-lisp::zswap fortran-to-lisp::xerbla
                    fortran-to-lisp::lsame))))

