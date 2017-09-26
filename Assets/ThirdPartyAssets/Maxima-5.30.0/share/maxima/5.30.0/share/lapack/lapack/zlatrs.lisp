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


(let* ((zero 0.0) (half 0.5) (one 1.0) (two 2.0))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 0.5 0.5) half)
           (type (double-float 1.0 1.0) one)
           (type (double-float 2.0 2.0) two)
           (ignorable zero half one two))
  (defun zlatrs (uplo trans diag normin n a lda x scale cnorm info)
    (declare (type (array double-float (*)) cnorm)
             (type (double-float) scale)
             (type (array f2cl-lib:complex16 (*)) x a)
             (type (f2cl-lib:integer4) info lda n)
             (type (simple-string *) normin diag trans uplo))
    (f2cl-lib:with-multi-array-data
        ((uplo character uplo-%data% uplo-%offset%)
         (trans character trans-%data% trans-%offset%)
         (diag character diag-%data% diag-%offset%)
         (normin character normin-%data% normin-%offset%)
         (a f2cl-lib:complex16 a-%data% a-%offset%)
         (x f2cl-lib:complex16 x-%data% x-%offset%)
         (cnorm double-float cnorm-%data% cnorm-%offset%))
      (labels ((cabs1 (zdum)
                 (+ (abs (f2cl-lib:dble zdum)) (abs (f2cl-lib:dimag zdum))))
               (cabs2 (zdum)
                 (+ (abs (/ (f2cl-lib:dble zdum) 2.0))
                    (abs (/ (f2cl-lib:dimag zdum) 2.0)))))
        (declare (ftype (function (f2cl-lib:complex16)
                         (values double-float &rest t))
                        cabs1))
        (declare (ftype (function (f2cl-lib:complex16)
                         (values double-float &rest t))
                        cabs2))
        (prog ((csumj #C(0.0 0.0)) (tjjs #C(0.0 0.0)) (uscal #C(0.0 0.0))
               (zdum #C(0.0 0.0)) (bignum 0.0) (grow 0.0) (rec 0.0)
               (smlnum 0.0) (tjj 0.0) (tmax 0.0) (tscal 0.0) (xbnd 0.0)
               (xj 0.0) (xmax 0.0) (i 0) (imax 0) (j 0) (jfirst 0) (jinc 0)
               (jlast 0) (notran nil) (nounit nil) (upper nil))
          (declare (type (f2cl-lib:complex16) csumj tjjs uscal zdum)
                   (type (double-float) bignum grow rec smlnum tjj tmax tscal
                                        xbnd xj xmax)
                   (type (f2cl-lib:integer4) i imax j jfirst jinc jlast)
                   (type f2cl-lib:logical notran nounit upper))
          (setf info 0)
          (setf upper (lsame uplo "U"))
          (setf notran (lsame trans "N"))
          (setf nounit (lsame diag "N"))
          (cond
            ((and (not upper) (not (lsame uplo "L")))
             (setf info -1))
            ((and (not notran) (not (lsame trans "T")) (not (lsame trans "C")))
             (setf info -2))
            ((and (not nounit) (not (lsame diag "U")))
             (setf info -3))
            ((and (not (lsame normin "Y")) (not (lsame normin "N")))
             (setf info -4))
            ((< n 0)
             (setf info -5))
            ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
             (setf info -7)))
          (cond
            ((/= info 0)
             (xerbla "ZLATRS" (f2cl-lib:int-sub info))
             (go end_label)))
          (if (= n 0) (go end_label))
          (setf smlnum (dlamch "Safe minimum"))
          (setf bignum (/ one smlnum))
          (multiple-value-bind (var-0 var-1)
              (dlabad smlnum bignum)
            (declare (ignore))
            (setf smlnum var-0)
            (setf bignum var-1))
          (setf smlnum (/ smlnum (dlamch "Precision")))
          (setf bignum (/ one smlnum))
          (setf scale one)
          (cond
            ((lsame normin "N")
             (cond
               (upper
                (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                              ((> j n) nil)
                  (tagbody
                    (setf (f2cl-lib:fref cnorm-%data%
                                         (j)
                                         ((1 *))
                                         cnorm-%offset%)
                            (dzasum (f2cl-lib:int-sub j 1)
                             (f2cl-lib:array-slice a-%data%
                                                   f2cl-lib:complex16
                                                   (1 j)
                                                   ((1 lda) (1 *))
                                                   a-%offset%)
                             1))
                   label10)))
               (t
                (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                              ((> j (f2cl-lib:int-add n (f2cl-lib:int-sub 1)))
                               nil)
                  (tagbody
                    (setf (f2cl-lib:fref cnorm-%data%
                                         (j)
                                         ((1 *))
                                         cnorm-%offset%)
                            (dzasum (f2cl-lib:int-sub n j)
                             (f2cl-lib:array-slice a-%data%
                                                   f2cl-lib:complex16
                                                   ((+ j 1) j)
                                                   ((1 lda) (1 *))
                                                   a-%offset%)
                             1))
                   label20))
                (setf (f2cl-lib:fref cnorm-%data% (n) ((1 *)) cnorm-%offset%)
                        zero)))))
          (setf imax (idamax n cnorm 1))
          (setf tmax
                  (f2cl-lib:fref cnorm-%data% (imax) ((1 *)) cnorm-%offset%))
          (cond
            ((<= tmax (* bignum half))
             (setf tscal one))
            (t
             (setf tscal (/ half (* smlnum tmax)))
             (dscal n tscal cnorm 1)))
          (setf xmax zero)
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (setf xmax
                      (max xmax
                           (cabs2
                            (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%))))
             label30))
          (setf xbnd xmax)
          (cond
            (notran
             (tagbody
               (cond
                 (upper
                  (setf jfirst n)
                  (setf jlast 1)
                  (setf jinc -1))
                 (t
                  (setf jfirst 1)
                  (setf jlast n)
                  (setf jinc 1)))
               (cond
                 ((/= tscal one)
                  (setf grow zero)
                  (go label60)))
               (cond
                 (nounit
                  (setf grow (/ half (max xbnd smlnum)))
                  (setf xbnd grow)
                  (f2cl-lib:fdo (j jfirst (f2cl-lib:int-add j jinc))
                                ((> j jlast) nil)
                    (tagbody
                      (if (<= grow smlnum) (go label60))
                      (setf tjjs
                              (f2cl-lib:fref a-%data%
                                             (j j)
                                             ((1 lda) (1 *))
                                             a-%offset%))
                      (setf tjj (cabs1 tjjs))
                      (cond
                        ((>= tjj smlnum)
                         (setf xbnd (min xbnd (* (min one tjj) grow))))
                        (t
                         (setf xbnd zero)))
                      (cond
                        ((>= (+ tjj (f2cl-lib:fref cnorm (j) ((1 *)))) smlnum)
                         (setf grow
                                 (* grow
                                    (/ tjj
                                       (+ tjj
                                          (f2cl-lib:fref cnorm-%data%
                                                         (j)
                                                         ((1 *))
                                                         cnorm-%offset%))))))
                        (t
                         (setf grow zero)))
                     label40))
                  (setf grow xbnd))
                 (t
                  (setf grow (min one (/ half (max xbnd smlnum))))
                  (f2cl-lib:fdo (j jfirst (f2cl-lib:int-add j jinc))
                                ((> j jlast) nil)
                    (tagbody
                      (if (<= grow smlnum) (go label60))
                      (setf grow
                              (* grow
                                 (/ one
                                    (+ one
                                       (f2cl-lib:fref cnorm-%data%
                                                      (j)
                                                      ((1 *))
                                                      cnorm-%offset%)))))
                     label50))))
              label60))
            (t
             (tagbody
               (cond
                 (upper
                  (setf jfirst 1)
                  (setf jlast n)
                  (setf jinc 1))
                 (t
                  (setf jfirst n)
                  (setf jlast 1)
                  (setf jinc -1)))
               (cond
                 ((/= tscal one)
                  (setf grow zero)
                  (go label90)))
               (cond
                 (nounit
                  (setf grow (/ half (max xbnd smlnum)))
                  (setf xbnd grow)
                  (f2cl-lib:fdo (j jfirst (f2cl-lib:int-add j jinc))
                                ((> j jlast) nil)
                    (tagbody
                      (if (<= grow smlnum) (go label90))
                      (setf xj
                              (+ one
                                 (f2cl-lib:fref cnorm-%data%
                                                (j)
                                                ((1 *))
                                                cnorm-%offset%)))
                      (setf grow (min grow (/ xbnd xj)))
                      (setf tjjs
                              (f2cl-lib:fref a-%data%
                                             (j j)
                                             ((1 lda) (1 *))
                                             a-%offset%))
                      (setf tjj (cabs1 tjjs))
                      (cond
                        ((>= tjj smlnum)
                         (if (> xj tjj) (setf xbnd (* xbnd (/ tjj xj)))))
                        (t
                         (setf xbnd zero)))
                     label70))
                  (setf grow (min grow xbnd)))
                 (t
                  (setf grow (min one (/ half (max xbnd smlnum))))
                  (f2cl-lib:fdo (j jfirst (f2cl-lib:int-add j jinc))
                                ((> j jlast) nil)
                    (tagbody
                      (if (<= grow smlnum) (go label90))
                      (setf xj
                              (+ one
                                 (f2cl-lib:fref cnorm-%data%
                                                (j)
                                                ((1 *))
                                                cnorm-%offset%)))
                      (setf grow (/ grow xj))
                     label80))))
              label90)))
          (cond
            ((> (* grow tscal) smlnum)
             (ztrsv uplo trans diag n a lda x 1))
            (t
             (cond
               ((> xmax (* bignum half))
                (setf scale (/ (* bignum half) xmax))
                (zdscal n scale x 1)
                (setf xmax bignum))
               (t
                (setf xmax (* xmax two))))
             (cond
               (notran
                (f2cl-lib:fdo (j jfirst (f2cl-lib:int-add j jinc))
                              ((> j jlast) nil)
                  (tagbody
                    (setf xj
                            (cabs1
                             (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)))
                    (cond
                      (nounit
                       (setf tjjs
                               (*
                                (f2cl-lib:fref a-%data%
                                               (j j)
                                               ((1 lda) (1 *))
                                               a-%offset%)
                                tscal)))
                      (t
                       (setf tjjs (coerce tscal 'f2cl-lib:complex16))
                       (if (= tscal one) (go label110))))
                    (setf tjj (cabs1 tjjs))
                    (cond
                      ((> tjj smlnum)
                       (cond
                         ((< tjj one)
                          (cond
                            ((> xj (* tjj bignum))
                             (setf rec (/ one xj))
                             (zdscal n rec x 1)
                             (setf scale (* scale rec))
                             (setf xmax (* xmax rec))))))
                       (setf (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)
                               (zladiv
                                (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)
                                tjjs))
                       (setf xj
                               (cabs1
                                (f2cl-lib:fref x-%data%
                                               (j)
                                               ((1 *))
                                               x-%offset%))))
                      ((> tjj zero)
                       (cond
                         ((> xj (* tjj bignum))
                          (setf rec (/ (* tjj bignum) xj))
                          (cond
                            ((> (f2cl-lib:fref cnorm (j) ((1 *))) one)
                             (setf rec
                                     (/ rec
                                        (f2cl-lib:fref cnorm-%data%
                                                       (j)
                                                       ((1 *))
                                                       cnorm-%offset%)))))
                          (zdscal n rec x 1)
                          (setf scale (* scale rec))
                          (setf xmax (* xmax rec))))
                       (setf (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)
                               (zladiv
                                (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)
                                tjjs))
                       (setf xj
                               (cabs1
                                (f2cl-lib:fref x-%data%
                                               (j)
                                               ((1 *))
                                               x-%offset%))))
                      (t
                       (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                     ((> i n) nil)
                         (tagbody
                           (setf (f2cl-lib:fref x-%data%
                                                (i)
                                                ((1 *))
                                                x-%offset%)
                                   (coerce zero 'f2cl-lib:complex16))
                          label100))
                       (setf (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)
                               (coerce one 'f2cl-lib:complex16))
                       (setf xj one)
                       (setf scale zero)
                       (setf xmax zero)))
                   label110
                    (cond
                      ((> xj one)
                       (setf rec (/ one xj))
                       (cond
                         ((> (f2cl-lib:fref cnorm (j) ((1 *)))
                             (* (+ bignum (- xmax)) rec))
                          (setf rec (* rec half))
                          (zdscal n rec x 1)
                          (setf scale (* scale rec)))))
                      ((> (* xj (f2cl-lib:fref cnorm (j) ((1 *))))
                          (+ bignum (- xmax)))
                       (zdscal n half x 1)
                       (setf scale (* scale half))))
                    (cond
                      (upper
                       (cond
                         ((> j 1)
                          (zaxpy (f2cl-lib:int-sub j 1)
                           (*
                            (- (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%))
                            tscal)
                           (f2cl-lib:array-slice a-%data%
                                                 f2cl-lib:complex16
                                                 (1 j)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                           1 x 1)
                          (setf i (izamax (f2cl-lib:int-sub j 1) x 1))
                          (setf xmax
                                  (cabs1
                                   (f2cl-lib:fref x-%data%
                                                  (i)
                                                  ((1 *))
                                                  x-%offset%))))))
                      (t
                       (cond
                         ((< j n)
                          (zaxpy (f2cl-lib:int-sub n j)
                           (*
                            (- (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%))
                            tscal)
                           (f2cl-lib:array-slice a-%data%
                                                 f2cl-lib:complex16
                                                 ((+ j 1) j)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                           1
                           (f2cl-lib:array-slice x-%data%
                                                 f2cl-lib:complex16
                                                 ((+ j 1))
                                                 ((1 *))
                                                 x-%offset%)
                           1)
                          (setf i
                                  (f2cl-lib:int-add j
                                                    (izamax
                                                     (f2cl-lib:int-sub n j)
                                                     (f2cl-lib:array-slice
                                                      x-%data%
                                                      f2cl-lib:complex16
                                                      ((+ j 1))
                                                      ((1 *))
                                                      x-%offset%)
                                                     1)))
                          (setf xmax
                                  (cabs1
                                   (f2cl-lib:fref x-%data%
                                                  (i)
                                                  ((1 *))
                                                  x-%offset%)))))))
                   label120)))
               ((lsame trans "T")
                (f2cl-lib:fdo (j jfirst (f2cl-lib:int-add j jinc))
                              ((> j jlast) nil)
                  (tagbody
                    (setf xj
                            (cabs1
                             (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)))
                    (setf uscal (coerce tscal 'f2cl-lib:complex16))
                    (setf rec (/ one (max xmax one)))
                    (cond
                      ((> (f2cl-lib:fref cnorm (j) ((1 *)))
                          (* (+ bignum (- xj)) rec))
                       (setf rec (* rec half))
                       (cond
                         (nounit
                          (setf tjjs
                                  (*
                                   (f2cl-lib:fref a-%data%
                                                  (j j)
                                                  ((1 lda) (1 *))
                                                  a-%offset%)
                                   tscal)))
                         (t
                          (setf tjjs (coerce tscal 'f2cl-lib:complex16))))
                       (setf tjj (cabs1 tjjs))
                       (cond
                         ((> tjj one)
                          (setf rec (min one (* rec tjj)))
                          (setf uscal (zladiv uscal tjjs))))
                       (cond
                         ((< rec one)
                          (zdscal n rec x 1)
                          (setf scale (* scale rec))
                          (setf xmax (* xmax rec))))))
                    (setf csumj (coerce zero 'f2cl-lib:complex16))
                    (cond
                      ((= uscal (f2cl-lib:dcmplx one))
                       (cond
                         (upper
                          (setf csumj
                                  (zdotu (f2cl-lib:int-sub j 1)
                                   (f2cl-lib:array-slice a-%data%
                                                         f2cl-lib:complex16
                                                         (1 j)
                                                         ((1 lda) (1 *))
                                                         a-%offset%)
                                   1 x 1)))
                         ((< j n)
                          (setf csumj
                                  (zdotu (f2cl-lib:int-sub n j)
                                   (f2cl-lib:array-slice a-%data%
                                                         f2cl-lib:complex16
                                                         ((+ j 1) j)
                                                         ((1 lda) (1 *))
                                                         a-%offset%)
                                   1
                                   (f2cl-lib:array-slice x-%data%
                                                         f2cl-lib:complex16
                                                         ((+ j 1))
                                                         ((1 *))
                                                         x-%offset%)
                                   1)))))
                      (t
                       (cond
                         (upper
                          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                        ((> i
                                            (f2cl-lib:int-add j
                                                              (f2cl-lib:int-sub
                                                               1)))
                                         nil)
                            (tagbody
                              (setf csumj
                                      (+ csumj
                                         (*
                                          (f2cl-lib:fref a-%data%
                                                         (i j)
                                                         ((1 lda) (1 *))
                                                         a-%offset%)
                                          uscal
                                          (f2cl-lib:fref x-%data%
                                                         (i)
                                                         ((1 *))
                                                         x-%offset%))))
                             label130)))
                         ((< j n)
                          (f2cl-lib:fdo (i (f2cl-lib:int-add j 1)
                                         (f2cl-lib:int-add i 1))
                                        ((> i n) nil)
                            (tagbody
                              (setf csumj
                                      (+ csumj
                                         (*
                                          (f2cl-lib:fref a-%data%
                                                         (i j)
                                                         ((1 lda) (1 *))
                                                         a-%offset%)
                                          uscal
                                          (f2cl-lib:fref x-%data%
                                                         (i)
                                                         ((1 *))
                                                         x-%offset%))))
                             label140))))))
                    (cond
                      ((= uscal (f2cl-lib:dcmplx tscal))
                       (tagbody
                         (setf (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)
                                 (-
                                  (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)
                                  csumj))
                         (setf xj
                                 (cabs1
                                  (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)))
                         (cond
                           (nounit
                            (setf tjjs
                                    (*
                                     (f2cl-lib:fref a-%data%
                                                    (j j)
                                                    ((1 lda) (1 *))
                                                    a-%offset%)
                                     tscal)))
                           (t
                            (setf tjjs (coerce tscal 'f2cl-lib:complex16))
                            (if (= tscal one) (go label160))))
                         (setf tjj (cabs1 tjjs))
                         (cond
                           ((> tjj smlnum)
                            (cond
                              ((< tjj one)
                               (cond
                                 ((> xj (* tjj bignum))
                                  (setf rec (/ one xj))
                                  (zdscal n rec x 1)
                                  (setf scale (* scale rec))
                                  (setf xmax (* xmax rec))))))
                            (setf (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)
                                    (zladiv
                                     (f2cl-lib:fref x-%data%
                                                    (j)
                                                    ((1 *))
                                                    x-%offset%)
                                     tjjs)))
                           ((> tjj zero)
                            (cond
                              ((> xj (* tjj bignum))
                               (setf rec (/ (* tjj bignum) xj))
                               (zdscal n rec x 1)
                               (setf scale (* scale rec))
                               (setf xmax (* xmax rec))))
                            (setf (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)
                                    (zladiv
                                     (f2cl-lib:fref x-%data%
                                                    (j)
                                                    ((1 *))
                                                    x-%offset%)
                                     tjjs)))
                           (t
                            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                          ((> i n) nil)
                              (tagbody
                                (setf (f2cl-lib:fref x-%data%
                                                     (i)
                                                     ((1 *))
                                                     x-%offset%)
                                        (coerce zero 'f2cl-lib:complex16))
                               label150))
                            (setf (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)
                                    (coerce one 'f2cl-lib:complex16))
                            (setf scale zero)
                            (setf xmax zero)))
                        label160))
                      (t
                       (setf (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)
                               (-
                                (zladiv
                                 (f2cl-lib:fref x-%data%
                                                (j)
                                                ((1 *))
                                                x-%offset%)
                                 tjjs)
                                csumj))))
                    (setf xmax
                            (max xmax
                                 (cabs1
                                  (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%))))
                   label170)))
               (t
                (f2cl-lib:fdo (j jfirst (f2cl-lib:int-add j jinc))
                              ((> j jlast) nil)
                  (tagbody
                    (setf xj
                            (cabs1
                             (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)))
                    (setf uscal (coerce tscal 'f2cl-lib:complex16))
                    (setf rec (/ one (max xmax one)))
                    (cond
                      ((> (f2cl-lib:fref cnorm (j) ((1 *)))
                          (* (+ bignum (- xj)) rec))
                       (setf rec (* rec half))
                       (cond
                         (nounit
                          (setf tjjs
                                  (coerce
                                   (*
                                    (f2cl-lib:dconjg
                                     (f2cl-lib:fref a-%data%
                                                    (j j)
                                                    ((1 lda) (1 *))
                                                    a-%offset%))
                                    tscal)
                                   'f2cl-lib:complex16)))
                         (t
                          (setf tjjs (coerce tscal 'f2cl-lib:complex16))))
                       (setf tjj (cabs1 tjjs))
                       (cond
                         ((> tjj one)
                          (setf rec (min one (* rec tjj)))
                          (setf uscal (zladiv uscal tjjs))))
                       (cond
                         ((< rec one)
                          (zdscal n rec x 1)
                          (setf scale (* scale rec))
                          (setf xmax (* xmax rec))))))
                    (setf csumj (coerce zero 'f2cl-lib:complex16))
                    (cond
                      ((= uscal (f2cl-lib:dcmplx one))
                       (cond
                         (upper
                          (setf csumj
                                  (zdotc (f2cl-lib:int-sub j 1)
                                   (f2cl-lib:array-slice a-%data%
                                                         f2cl-lib:complex16
                                                         (1 j)
                                                         ((1 lda) (1 *))
                                                         a-%offset%)
                                   1 x 1)))
                         ((< j n)
                          (setf csumj
                                  (zdotc (f2cl-lib:int-sub n j)
                                   (f2cl-lib:array-slice a-%data%
                                                         f2cl-lib:complex16
                                                         ((+ j 1) j)
                                                         ((1 lda) (1 *))
                                                         a-%offset%)
                                   1
                                   (f2cl-lib:array-slice x-%data%
                                                         f2cl-lib:complex16
                                                         ((+ j 1))
                                                         ((1 *))
                                                         x-%offset%)
                                   1)))))
                      (t
                       (cond
                         (upper
                          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                        ((> i
                                            (f2cl-lib:int-add j
                                                              (f2cl-lib:int-sub
                                                               1)))
                                         nil)
                            (tagbody
                              (setf csumj
                                      (+ csumj
                                         (*
                                          (f2cl-lib:dconjg
                                           (f2cl-lib:fref a-%data%
                                                          (i j)
                                                          ((1 lda) (1 *))
                                                          a-%offset%))
                                          uscal
                                          (f2cl-lib:fref x-%data%
                                                         (i)
                                                         ((1 *))
                                                         x-%offset%))))
                             label180)))
                         ((< j n)
                          (f2cl-lib:fdo (i (f2cl-lib:int-add j 1)
                                         (f2cl-lib:int-add i 1))
                                        ((> i n) nil)
                            (tagbody
                              (setf csumj
                                      (+ csumj
                                         (*
                                          (f2cl-lib:dconjg
                                           (f2cl-lib:fref a-%data%
                                                          (i j)
                                                          ((1 lda) (1 *))
                                                          a-%offset%))
                                          uscal
                                          (f2cl-lib:fref x-%data%
                                                         (i)
                                                         ((1 *))
                                                         x-%offset%))))
                             label190))))))
                    (cond
                      ((= uscal (f2cl-lib:dcmplx tscal))
                       (tagbody
                         (setf (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)
                                 (-
                                  (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)
                                  csumj))
                         (setf xj
                                 (cabs1
                                  (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)))
                         (cond
                           (nounit
                            (setf tjjs
                                    (coerce
                                     (*
                                      (f2cl-lib:dconjg
                                       (f2cl-lib:fref a-%data%
                                                      (j j)
                                                      ((1 lda) (1 *))
                                                      a-%offset%))
                                      tscal)
                                     'f2cl-lib:complex16)))
                           (t
                            (setf tjjs (coerce tscal 'f2cl-lib:complex16))
                            (if (= tscal one) (go label210))))
                         (setf tjj (cabs1 tjjs))
                         (cond
                           ((> tjj smlnum)
                            (cond
                              ((< tjj one)
                               (cond
                                 ((> xj (* tjj bignum))
                                  (setf rec (/ one xj))
                                  (zdscal n rec x 1)
                                  (setf scale (* scale rec))
                                  (setf xmax (* xmax rec))))))
                            (setf (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)
                                    (zladiv
                                     (f2cl-lib:fref x-%data%
                                                    (j)
                                                    ((1 *))
                                                    x-%offset%)
                                     tjjs)))
                           ((> tjj zero)
                            (cond
                              ((> xj (* tjj bignum))
                               (setf rec (/ (* tjj bignum) xj))
                               (zdscal n rec x 1)
                               (setf scale (* scale rec))
                               (setf xmax (* xmax rec))))
                            (setf (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)
                                    (zladiv
                                     (f2cl-lib:fref x-%data%
                                                    (j)
                                                    ((1 *))
                                                    x-%offset%)
                                     tjjs)))
                           (t
                            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                          ((> i n) nil)
                              (tagbody
                                (setf (f2cl-lib:fref x-%data%
                                                     (i)
                                                     ((1 *))
                                                     x-%offset%)
                                        (coerce zero 'f2cl-lib:complex16))
                               label200))
                            (setf (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)
                                    (coerce one 'f2cl-lib:complex16))
                            (setf scale zero)
                            (setf xmax zero)))
                        label210))
                      (t
                       (setf (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)
                               (-
                                (zladiv
                                 (f2cl-lib:fref x-%data%
                                                (j)
                                                ((1 *))
                                                x-%offset%)
                                 tjjs)
                                csumj))))
                    (setf xmax
                            (max xmax
                                 (cabs1
                                  (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%))))
                   label220))))
             (setf scale (/ scale tscal))))
          (cond
            ((/= tscal one)
             (dscal n (/ one tscal) cnorm 1)))
          (go end_label)
         end_label
          (return (values nil nil nil nil nil nil nil nil scale nil info)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zlatrs
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string) (simple-string)
                        (simple-string) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*)) (double-float)
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil
                            fortran-to-lisp::scale nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::zdotc fortran-to-lisp::zdotu
                    fortran-to-lisp::izamax fortran-to-lisp::zaxpy
                    fortran-to-lisp::zladiv fortran-to-lisp::zdscal
                    fortran-to-lisp::ztrsv fortran-to-lisp::dscal
                    fortran-to-lisp::idamax fortran-to-lisp::dzasum
                    fortran-to-lisp::dlabad fortran-to-lisp::dlamch
                    fortran-to-lisp::xerbla fortran-to-lisp::lsame))))

