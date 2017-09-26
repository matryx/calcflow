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
  (defun zlascl (type kl ku cfrom cto m n a lda info)
    (declare (type (array f2cl-lib:complex16 (*)) a)
             (type (double-float) cto cfrom)
             (type (f2cl-lib:integer4) info lda n m ku kl)
             (type (simple-string *) type))
    (f2cl-lib:with-multi-array-data
        ((type character type-%data% type-%offset%)
         (a f2cl-lib:complex16 a-%data% a-%offset%))
      (prog ((bignum 0.0) (cfrom1 0.0) (cfromc 0.0) (cto1 0.0) (ctoc 0.0)
             (mul 0.0) (smlnum 0.0) (i 0) (itype 0) (j 0) (k1 0) (k2 0) (k3 0)
             (k4 0) (done nil))
        (declare (type (double-float) bignum cfrom1 cfromc cto1 ctoc mul
                                      smlnum)
                 (type (f2cl-lib:integer4) i itype j k1 k2 k3 k4)
                 (type f2cl-lib:logical done))
        (setf info 0)
        (cond
          ((lsame type "G")
           (setf itype 0))
          ((lsame type "L")
           (setf itype 1))
          ((lsame type "U")
           (setf itype 2))
          ((lsame type "H")
           (setf itype 3))
          ((lsame type "B")
           (setf itype 4))
          ((lsame type "Q")
           (setf itype 5))
          ((lsame type "Z")
           (setf itype 6))
          (t
           (setf itype -1)))
        (cond
          ((= itype (f2cl-lib:int-sub 1))
           (setf info -1))
          ((or (= cfrom zero) (disnan cfrom))
           (setf info -4))
          ((disnan cto)
           (setf info -5))
          ((< m 0)
           (setf info -6))
          ((or (< n 0) (and (= itype 4) (/= n m)) (and (= itype 5) (/= n m)))
           (setf info -7))
          ((and (<= itype 3)
                (< lda
                   (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m))))
           (setf info -9))
          ((>= itype 4)
           (cond
             ((or (< kl 0)
                  (> kl
                     (max
                      (the f2cl-lib:integer4
                           (f2cl-lib:int-add m (f2cl-lib:int-sub 1)))
                      (the f2cl-lib:integer4 0))))
              (setf info -2))
             ((or (< ku 0)
                  (> ku
                     (max
                      (the f2cl-lib:integer4
                           (f2cl-lib:int-add n (f2cl-lib:int-sub 1)))
                      (the f2cl-lib:integer4 0)))
                  (and (or (= itype 4) (= itype 5)) (/= kl ku)))
              (setf info -3))
             ((or (and (= itype 4) (< lda (f2cl-lib:int-add kl 1)))
                  (and (= itype 5) (< lda (f2cl-lib:int-add ku 1)))
                  (and (= itype 6)
                       (< lda (f2cl-lib:int-add (f2cl-lib:int-mul 2 kl) ku 1))))
              (setf info -9)))))
        (cond
          ((/= info 0)
           (xerbla "ZLASCL" (f2cl-lib:int-sub info))
           (go end_label)))
        (if (or (= n 0) (= m 0)) (go end_label))
        (setf smlnum (dlamch "S"))
        (setf bignum (/ one smlnum))
        (setf cfromc cfrom)
        (setf ctoc cto)
       label10
        (setf cfrom1 (* cfromc smlnum))
        (cond
          ((= cfrom1 cfromc)
           (setf mul (/ ctoc cfromc))
           (setf done f2cl-lib:%true%)
           (setf cto1 ctoc))
          (t
           (setf cto1 (/ ctoc bignum))
           (cond
             ((= cto1 ctoc)
              (setf mul ctoc)
              (setf done f2cl-lib:%true%)
              (setf cfromc one))
             ((and (> (abs cfrom1) (abs ctoc)) (/= ctoc zero))
              (setf mul smlnum)
              (setf done f2cl-lib:%false%)
              (setf cfromc cfrom1))
             ((> (abs cto1) (abs cfromc))
              (setf mul bignum)
              (setf done f2cl-lib:%false%)
              (setf ctoc cto1))
             (t
              (setf mul (/ ctoc cfromc))
              (setf done f2cl-lib:%true%)))))
        (cond
          ((= itype 0)
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                             ((> i m) nil)
                 (tagbody
                   (setf (f2cl-lib:fref a-%data%
                                        (i j)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                           (*
                            (f2cl-lib:fref a-%data%
                                           (i j)
                                           ((1 lda) (1 *))
                                           a-%offset%)
                            mul))
                  label20))
              label30)))
          ((= itype 1)
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                             ((> i m) nil)
                 (tagbody
                   (setf (f2cl-lib:fref a-%data%
                                        (i j)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                           (*
                            (f2cl-lib:fref a-%data%
                                           (i j)
                                           ((1 lda) (1 *))
                                           a-%offset%)
                            mul))
                  label40))
              label50)))
          ((= itype 2)
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                             ((> i
                                 (min (the f2cl-lib:integer4 j)
                                      (the f2cl-lib:integer4 m)))
                              nil)
                 (tagbody
                   (setf (f2cl-lib:fref a-%data%
                                        (i j)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                           (*
                            (f2cl-lib:fref a-%data%
                                           (i j)
                                           ((1 lda) (1 *))
                                           a-%offset%)
                            mul))
                  label60))
              label70)))
          ((= itype 3)
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                             ((> i
                                 (min
                                  (the f2cl-lib:integer4
                                       (f2cl-lib:int-add j 1))
                                  (the f2cl-lib:integer4 m)))
                              nil)
                 (tagbody
                   (setf (f2cl-lib:fref a-%data%
                                        (i j)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                           (*
                            (f2cl-lib:fref a-%data%
                                           (i j)
                                           ((1 lda) (1 *))
                                           a-%offset%)
                            mul))
                  label80))
              label90)))
          ((= itype 4)
           (setf k3 (f2cl-lib:int-add kl 1))
           (setf k4 (f2cl-lib:int-add n 1))
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                             ((> i
                                 (min (the f2cl-lib:integer4 k3)
                                      (the f2cl-lib:integer4
                                           (f2cl-lib:int-add k4
                                                             (f2cl-lib:int-sub
                                                              j)))))
                              nil)
                 (tagbody
                   (setf (f2cl-lib:fref a-%data%
                                        (i j)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                           (*
                            (f2cl-lib:fref a-%data%
                                           (i j)
                                           ((1 lda) (1 *))
                                           a-%offset%)
                            mul))
                  label100))
              label110)))
          ((= itype 5)
           (setf k1 (f2cl-lib:int-add ku 2))
           (setf k3 (f2cl-lib:int-add ku 1))
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (f2cl-lib:fdo (i
                              (max
                               (the f2cl-lib:integer4
                                    (f2cl-lib:int-add k1 (f2cl-lib:int-sub j)))
                               (the f2cl-lib:integer4 1))
                              (f2cl-lib:int-add i 1))
                             ((> i k3) nil)
                 (tagbody
                   (setf (f2cl-lib:fref a-%data%
                                        (i j)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                           (*
                            (f2cl-lib:fref a-%data%
                                           (i j)
                                           ((1 lda) (1 *))
                                           a-%offset%)
                            mul))
                  label120))
              label130)))
          ((= itype 6)
           (setf k1 (f2cl-lib:int-add kl ku 2))
           (setf k2 (f2cl-lib:int-add kl 1))
           (setf k3 (f2cl-lib:int-add (f2cl-lib:int-mul 2 kl) ku 1))
           (setf k4 (f2cl-lib:int-add kl ku 1 m))
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (f2cl-lib:fdo (i
                              (max
                               (the f2cl-lib:integer4
                                    (f2cl-lib:int-add k1 (f2cl-lib:int-sub j)))
                               (the f2cl-lib:integer4 k2))
                              (f2cl-lib:int-add i 1))
                             ((> i
                                 (min (the f2cl-lib:integer4 k3)
                                      (the f2cl-lib:integer4
                                           (f2cl-lib:int-add k4
                                                             (f2cl-lib:int-sub
                                                              j)))))
                              nil)
                 (tagbody
                   (setf (f2cl-lib:fref a-%data%
                                        (i j)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                           (*
                            (f2cl-lib:fref a-%data%
                                           (i j)
                                           ((1 lda) (1 *))
                                           a-%offset%)
                            mul))
                  label140))
              label150))))
        (if (not done) (go label10))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zlascl
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlamch fortran-to-lisp::xerbla
                    fortran-to-lisp::disnan fortran-to-lisp::lsame))))

