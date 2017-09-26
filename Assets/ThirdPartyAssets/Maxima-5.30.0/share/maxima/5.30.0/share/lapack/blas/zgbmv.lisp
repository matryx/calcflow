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
  (defun zgbmv (trans m n kl ku alpha a lda x incx beta y incy)
    (declare (type (array f2cl-lib:complex16 (*)) y x a)
             (type (f2cl-lib:complex16) beta alpha)
             (type (f2cl-lib:integer4) incy incx lda ku kl n m)
             (type (simple-string *) trans))
    (f2cl-lib:with-multi-array-data
        ((trans character trans-%data% trans-%offset%)
         (a f2cl-lib:complex16 a-%data% a-%offset%)
         (x f2cl-lib:complex16 x-%data% x-%offset%)
         (y f2cl-lib:complex16 y-%data% y-%offset%))
      (prog ((noconj nil) (i 0) (info 0) (ix 0) (iy 0) (j 0) (jx 0) (jy 0)
             (k 0) (kup1 0) (kx 0) (ky 0) (lenx 0) (leny 0) (temp #C(0.0 0.0)))
        (declare (type f2cl-lib:logical noconj)
                 (type (f2cl-lib:integer4) i info ix iy j jx jy k kup1 kx ky
                                           lenx leny)
                 (type (f2cl-lib:complex16) temp))
        (setf info 0)
        (cond
          ((and (not (lsame trans "N"))
                (not (lsame trans "T"))
                (not (lsame trans "C")))
           (setf info 1))
          ((< m 0)
           (setf info 2))
          ((< n 0)
           (setf info 3))
          ((< kl 0)
           (setf info 4))
          ((< ku 0)
           (setf info 5))
          ((< lda (f2cl-lib:int-add kl ku 1))
           (setf info 8))
          ((= incx 0)
           (setf info 10))
          ((= incy 0)
           (setf info 13)))
        (cond
          ((/= info 0)
           (xerbla "ZGBMV " info)
           (go end_label)))
        (if (or (= m 0) (= n 0) (and (= alpha zero) (= beta one)))
            (go end_label))
        (setf noconj (lsame trans "T"))
        (cond
          ((lsame trans "N")
           (setf lenx n)
           (setf leny m))
          (t
           (setf lenx m)
           (setf leny n)))
        (cond
          ((> incx 0)
           (setf kx 1))
          (t
           (setf kx
                   (f2cl-lib:int-sub 1
                                     (f2cl-lib:int-mul
                                      (f2cl-lib:int-sub lenx 1)
                                      incx)))))
        (cond
          ((> incy 0)
           (setf ky 1))
          (t
           (setf ky
                   (f2cl-lib:int-sub 1
                                     (f2cl-lib:int-mul
                                      (f2cl-lib:int-sub leny 1)
                                      incy)))))
        (cond
          ((/= beta one)
           (cond
             ((= incy 1)
              (cond
                ((= beta zero)
                 (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                               ((> i leny) nil)
                   (tagbody
                     (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                             zero)
                    label10)))
                (t
                 (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                               ((> i leny) nil)
                   (tagbody
                     (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                             (* beta
                                (f2cl-lib:fref y-%data%
                                               (i)
                                               ((1 *))
                                               y-%offset%)))
                    label20)))))
             (t
              (setf iy ky)
              (cond
                ((= beta zero)
                 (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                               ((> i leny) nil)
                   (tagbody
                     (setf (f2cl-lib:fref y-%data% (iy) ((1 *)) y-%offset%)
                             zero)
                     (setf iy (f2cl-lib:int-add iy incy))
                    label30)))
                (t
                 (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                               ((> i leny) nil)
                   (tagbody
                     (setf (f2cl-lib:fref y-%data% (iy) ((1 *)) y-%offset%)
                             (* beta
                                (f2cl-lib:fref y-%data%
                                               (iy)
                                               ((1 *))
                                               y-%offset%)))
                     (setf iy (f2cl-lib:int-add iy incy))
                    label40))))))))
        (if (= alpha zero) (go end_label))
        (setf kup1 (f2cl-lib:int-add ku 1))
        (cond
          ((lsame trans "N")
           (setf jx kx)
           (cond
             ((= incy 1)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (cond
                    ((/= (f2cl-lib:fref x (jx) ((1 *))) zero)
                     (setf temp
                             (* alpha
                                (f2cl-lib:fref x-%data%
                                               (jx)
                                               ((1 *))
                                               x-%offset%)))
                     (setf k (f2cl-lib:int-sub kup1 j))
                     (f2cl-lib:fdo (i
                                    (max (the f2cl-lib:integer4 1)
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-add j
                                                                (f2cl-lib:int-sub
                                                                 ku))))
                                    (f2cl-lib:int-add i 1))
                                   ((> i
                                       (min (the f2cl-lib:integer4 m)
                                            (the f2cl-lib:integer4
                                                 (f2cl-lib:int-add j kl))))
                                    nil)
                       (tagbody
                         (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                                 (+
                                  (f2cl-lib:fref y-%data%
                                                 (i)
                                                 ((1 *))
                                                 y-%offset%)
                                  (* temp
                                     (f2cl-lib:fref a-%data%
                                                    ((f2cl-lib:int-add k i) j)
                                                    ((1 lda) (1 *))
                                                    a-%offset%))))
                        label50))))
                  (setf jx (f2cl-lib:int-add jx incx))
                 label60)))
             (t
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (cond
                    ((/= (f2cl-lib:fref x (jx) ((1 *))) zero)
                     (setf temp
                             (* alpha
                                (f2cl-lib:fref x-%data%
                                               (jx)
                                               ((1 *))
                                               x-%offset%)))
                     (setf iy ky)
                     (setf k (f2cl-lib:int-sub kup1 j))
                     (f2cl-lib:fdo (i
                                    (max (the f2cl-lib:integer4 1)
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-add j
                                                                (f2cl-lib:int-sub
                                                                 ku))))
                                    (f2cl-lib:int-add i 1))
                                   ((> i
                                       (min (the f2cl-lib:integer4 m)
                                            (the f2cl-lib:integer4
                                                 (f2cl-lib:int-add j kl))))
                                    nil)
                       (tagbody
                         (setf (f2cl-lib:fref y-%data% (iy) ((1 *)) y-%offset%)
                                 (+
                                  (f2cl-lib:fref y-%data%
                                                 (iy)
                                                 ((1 *))
                                                 y-%offset%)
                                  (* temp
                                     (f2cl-lib:fref a-%data%
                                                    ((f2cl-lib:int-add k i) j)
                                                    ((1 lda) (1 *))
                                                    a-%offset%))))
                         (setf iy (f2cl-lib:int-add iy incy))
                        label70))))
                  (setf jx (f2cl-lib:int-add jx incx))
                  (if (> j ku) (setf ky (f2cl-lib:int-add ky incy)))
                 label80)))))
          (t
           (setf jy ky)
           (cond
             ((= incx 1)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (setf temp zero)
                  (setf k (f2cl-lib:int-sub kup1 j))
                  (cond
                    (noconj
                     (f2cl-lib:fdo (i
                                    (max (the f2cl-lib:integer4 1)
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-add j
                                                                (f2cl-lib:int-sub
                                                                 ku))))
                                    (f2cl-lib:int-add i 1))
                                   ((> i
                                       (min (the f2cl-lib:integer4 m)
                                            (the f2cl-lib:integer4
                                                 (f2cl-lib:int-add j kl))))
                                    nil)
                       (tagbody
                         (setf temp
                                 (+ temp
                                    (*
                                     (f2cl-lib:fref a-%data%
                                                    ((f2cl-lib:int-add k i) j)
                                                    ((1 lda) (1 *))
                                                    a-%offset%)
                                     (f2cl-lib:fref x-%data%
                                                    (i)
                                                    ((1 *))
                                                    x-%offset%))))
                        label90)))
                    (t
                     (f2cl-lib:fdo (i
                                    (max (the f2cl-lib:integer4 1)
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-add j
                                                                (f2cl-lib:int-sub
                                                                 ku))))
                                    (f2cl-lib:int-add i 1))
                                   ((> i
                                       (min (the f2cl-lib:integer4 m)
                                            (the f2cl-lib:integer4
                                                 (f2cl-lib:int-add j kl))))
                                    nil)
                       (tagbody
                         (setf temp
                                 (+ temp
                                    (*
                                     (f2cl-lib:dconjg
                                      (f2cl-lib:fref a-%data%
                                                     ((f2cl-lib:int-add k i) j)
                                                     ((1 lda) (1 *))
                                                     a-%offset%))
                                     (f2cl-lib:fref x-%data%
                                                    (i)
                                                    ((1 *))
                                                    x-%offset%))))
                        label100))))
                  (setf (f2cl-lib:fref y-%data% (jy) ((1 *)) y-%offset%)
                          (+ (f2cl-lib:fref y-%data% (jy) ((1 *)) y-%offset%)
                             (* alpha temp)))
                  (setf jy (f2cl-lib:int-add jy incy))
                 label110)))
             (t
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (setf temp zero)
                  (setf ix kx)
                  (setf k (f2cl-lib:int-sub kup1 j))
                  (cond
                    (noconj
                     (f2cl-lib:fdo (i
                                    (max (the f2cl-lib:integer4 1)
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-add j
                                                                (f2cl-lib:int-sub
                                                                 ku))))
                                    (f2cl-lib:int-add i 1))
                                   ((> i
                                       (min (the f2cl-lib:integer4 m)
                                            (the f2cl-lib:integer4
                                                 (f2cl-lib:int-add j kl))))
                                    nil)
                       (tagbody
                         (setf temp
                                 (+ temp
                                    (*
                                     (f2cl-lib:fref a-%data%
                                                    ((f2cl-lib:int-add k i) j)
                                                    ((1 lda) (1 *))
                                                    a-%offset%)
                                     (f2cl-lib:fref x-%data%
                                                    (ix)
                                                    ((1 *))
                                                    x-%offset%))))
                         (setf ix (f2cl-lib:int-add ix incx))
                        label120)))
                    (t
                     (f2cl-lib:fdo (i
                                    (max (the f2cl-lib:integer4 1)
                                         (the f2cl-lib:integer4
                                              (f2cl-lib:int-add j
                                                                (f2cl-lib:int-sub
                                                                 ku))))
                                    (f2cl-lib:int-add i 1))
                                   ((> i
                                       (min (the f2cl-lib:integer4 m)
                                            (the f2cl-lib:integer4
                                                 (f2cl-lib:int-add j kl))))
                                    nil)
                       (tagbody
                         (setf temp
                                 (+ temp
                                    (*
                                     (f2cl-lib:dconjg
                                      (f2cl-lib:fref a-%data%
                                                     ((f2cl-lib:int-add k i) j)
                                                     ((1 lda) (1 *))
                                                     a-%offset%))
                                     (f2cl-lib:fref x-%data%
                                                    (ix)
                                                    ((1 *))
                                                    x-%offset%))))
                         (setf ix (f2cl-lib:int-add ix incx))
                        label130))))
                  (setf (f2cl-lib:fref y-%data% (jy) ((1 *)) y-%offset%)
                          (+ (f2cl-lib:fref y-%data% (jy) ((1 *)) y-%offset%)
                             (* alpha temp)))
                  (setf jy (f2cl-lib:int-add jy incy))
                  (if (> j ku) (setf kx (f2cl-lib:int-add kx incx)))
                 label140))))))
        (go end_label)
       end_label
        (return
         (values nil nil nil nil nil nil nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zgbmv fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::complex16)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::complex16)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil
                            nil)
           :calls '(fortran-to-lisp::xerbla fortran-to-lisp::lsame))))

