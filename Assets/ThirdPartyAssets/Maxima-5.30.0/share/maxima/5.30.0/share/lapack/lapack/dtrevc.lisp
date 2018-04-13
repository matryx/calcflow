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
  (defun dtrevc (side howmny select n t$ ldt vl ldvl vr ldvr mm m work info)
    (declare (type (array double-float (*)) work vr vl t$)
             (type (f2cl-lib:integer4) info m mm ldvr ldvl ldt n)
             (type (array f2cl-lib:logical (*)) select)
             (type (simple-string *) howmny side))
    (f2cl-lib:with-multi-array-data
        ((side character side-%data% side-%offset%)
         (howmny character howmny-%data% howmny-%offset%)
         (select f2cl-lib:logical select-%data% select-%offset%)
         (t$ double-float t$-%data% t$-%offset%)
         (vl double-float vl-%data% vl-%offset%)
         (vr double-float vr-%data% vr-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((x (make-array 4 :element-type 'double-float)) (beta 0.0)
             (bignum 0.0) (emax 0.0) (ovfl 0.0) (rec 0.0) (remax 0.0)
             (scale 0.0) (smin 0.0) (smlnum 0.0) (ulp 0.0) (unfl 0.0)
             (vcrit 0.0) (vmax 0.0) (wi 0.0) (wr 0.0) (xnorm 0.0) (i 0)
             (ierr 0) (ii 0) (ip 0) (is 0) (j 0) (j1 0) (j2 0) (jnxt 0) (k 0)
             (ki 0) (n2 0) (allv nil) (bothv nil) (leftv nil) (over nil)
             (pair nil) (rightv nil) (somev nil))
        (declare (type (array double-float (4)) x)
                 (type (double-float) beta bignum emax ovfl rec remax scale
                                      smin smlnum ulp unfl vcrit vmax wi wr
                                      xnorm)
                 (type (f2cl-lib:integer4) i ierr ii ip is j j1 j2 jnxt k ki
                                           n2)
                 (type f2cl-lib:logical allv bothv leftv over pair rightv
                                        somev))
        (setf bothv (lsame side "B"))
        (setf rightv (or (lsame side "R") bothv))
        (setf leftv (or (lsame side "L") bothv))
        (setf allv (lsame howmny "A"))
        (setf over (lsame howmny "B"))
        (setf somev (lsame howmny "S"))
        (setf info 0)
        (cond
          ((and (not rightv) (not leftv))
           (setf info -1))
          ((and (not allv) (not over) (not somev))
           (setf info -2))
          ((< n 0)
           (setf info -4))
          ((< ldt (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
           (setf info -6))
          ((or (< ldvl 1) (and leftv (< ldvl n)))
           (setf info -8))
          ((or (< ldvr 1) (and rightv (< ldvr n)))
           (setf info -10))
          (t
           (cond
             (somev
              (setf m 0)
              (setf pair f2cl-lib:%false%)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (cond
                    (pair
                     (setf pair f2cl-lib:%false%)
                     (setf (f2cl-lib:fref select-%data%
                                          (j)
                                          ((1 *))
                                          select-%offset%)
                             f2cl-lib:%false%))
                    (t
                     (cond
                       ((< j n)
                        (cond
                          ((=
                            (f2cl-lib:fref t$
                                           ((f2cl-lib:int-add j 1) j)
                                           ((1 ldt) (1 *)))
                            zero)
                           (if
                            (f2cl-lib:fref select-%data%
                                           (j)
                                           ((1 *))
                                           select-%offset%)
                            (setf m (f2cl-lib:int-add m 1))))
                          (t
                           (setf pair f2cl-lib:%true%)
                           (cond
                             ((or (f2cl-lib:fref select (j) ((1 *)))
                                  (f2cl-lib:fref select
                                                 ((f2cl-lib:int-add j 1))
                                                 ((1 *))))
                              (setf (f2cl-lib:fref select-%data%
                                                   (j)
                                                   ((1 *))
                                                   select-%offset%)
                                      f2cl-lib:%true%)
                              (setf m (f2cl-lib:int-add m 2)))))))
                       (t
                        (if
                         (f2cl-lib:fref select-%data%
                                        (n)
                                        ((1 *))
                                        select-%offset%)
                         (setf m (f2cl-lib:int-add m 1)))))))
                 label10)))
             (t
              (setf m n)))
           (cond
             ((< mm m)
              (setf info -11)))))
        (cond
          ((/= info 0)
           (xerbla "DTREVC" (f2cl-lib:int-sub info))
           (go end_label)))
        (if (= n 0) (go end_label))
        (setf unfl (dlamch "Safe minimum"))
        (setf ovfl (/ one unfl))
        (multiple-value-bind (var-0 var-1)
            (dlabad unfl ovfl)
          (declare (ignore))
          (setf unfl var-0)
          (setf ovfl var-1))
        (setf ulp (dlamch "Precision"))
        (setf smlnum (* unfl (/ n ulp)))
        (setf bignum (/ (- one ulp) smlnum))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%) zero)
        (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%) zero)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i (f2cl-lib:int-add j (f2cl-lib:int-sub 1))) nil)
              (tagbody
                (setf (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)
                        (+
                         (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)
                         (abs
                          (f2cl-lib:fref t$-%data%
                                         (i j)
                                         ((1 ldt) (1 *))
                                         t$-%offset%))))
               label20))
           label30))
        (setf n2 (f2cl-lib:int-mul 2 n))
        (cond
          (rightv
           (setf ip 0)
           (setf is m)
           (f2cl-lib:fdo (ki n (f2cl-lib:int-add ki (f2cl-lib:int-sub 1)))
                         ((> ki 1) nil)
             (tagbody
               (if (= ip 1) (go label130))
               (if (= ki 1) (go label40))
               (if
                (=
                 (f2cl-lib:fref t$-%data%
                                (ki (f2cl-lib:int-sub ki 1))
                                ((1 ldt) (1 *))
                                t$-%offset%)
                 zero)
                (go label40))
               (setf ip -1)
              label40
               (cond
                 (somev
                  (cond
                    ((= ip 0)
                     (if
                      (not
                       (f2cl-lib:fref select-%data%
                                      (ki)
                                      ((1 *))
                                      select-%offset%))
                      (go label130)))
                    (t
                     (if
                      (not
                       (f2cl-lib:fref select-%data%
                                      ((f2cl-lib:int-sub ki 1))
                                      ((1 *))
                                      select-%offset%))
                      (go label130))))))
               (setf wr
                       (f2cl-lib:fref t$-%data%
                                      (ki ki)
                                      ((1 ldt) (1 *))
                                      t$-%offset%))
               (setf wi zero)
               (if (/= ip 0)
                   (setf wi
                           (*
                            (f2cl-lib:fsqrt
                             (abs
                              (f2cl-lib:fref t$-%data%
                                             (ki (f2cl-lib:int-sub ki 1))
                                             ((1 ldt) (1 *))
                                             t$-%offset%)))
                            (f2cl-lib:fsqrt
                             (abs
                              (f2cl-lib:fref t$-%data%
                                             ((f2cl-lib:int-sub ki 1) ki)
                                             ((1 ldt) (1 *))
                                             t$-%offset%))))))
               (setf smin (max (* ulp (+ (abs wr) (abs wi))) smlnum))
               (cond
                 ((= ip 0)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add ki n))
                                       ((1 *))
                                       work-%offset%)
                          one)
                  (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                ((> k
                                    (f2cl-lib:int-add ki (f2cl-lib:int-sub 1)))
                                 nil)
                    (tagbody
                      (setf (f2cl-lib:fref work-%data%
                                           ((f2cl-lib:int-add k n))
                                           ((1 *))
                                           work-%offset%)
                              (-
                               (f2cl-lib:fref t$-%data%
                                              (k ki)
                                              ((1 ldt) (1 *))
                                              t$-%offset%)))
                     label50))
                  (setf jnxt (f2cl-lib:int-sub ki 1))
                  (f2cl-lib:fdo (j (f2cl-lib:int-add ki (f2cl-lib:int-sub 1))
                                 (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                                ((> j 1) nil)
                    (tagbody
                      (if (> j jnxt) (go label60))
                      (setf j1 j)
                      (setf j2 j)
                      (setf jnxt (f2cl-lib:int-sub j 1))
                      (cond
                        ((> j 1)
                         (cond
                           ((/=
                             (f2cl-lib:fref t$
                                            (j
                                             (f2cl-lib:int-add j
                                                               (f2cl-lib:int-sub
                                                                1)))
                                            ((1 ldt) (1 *)))
                             zero)
                            (setf j1 (f2cl-lib:int-sub j 1))
                            (setf jnxt (f2cl-lib:int-sub j 2))))))
                      (cond
                        ((= j1 j2)
                         (multiple-value-bind
                               (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                var-8 var-9 var-10 var-11 var-12 var-13 var-14
                                var-15 var-16 var-17)
                             (dlaln2 f2cl-lib:%false% 1 1 smin one
                              (f2cl-lib:array-slice t$-%data%
                                                    double-float
                                                    (j j)
                                                    ((1 ldt) (1 *))
                                                    t$-%offset%)
                              ldt one one
                              (f2cl-lib:array-slice work-%data%
                                                    double-float
                                                    ((+ j n))
                                                    ((1 *))
                                                    work-%offset%)
                              n wr zero x 2 scale xnorm ierr)
                           (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                            var-6 var-7 var-8 var-9 var-10
                                            var-11 var-12 var-13 var-14))
                           (setf scale var-15)
                           (setf xnorm var-16)
                           (setf ierr var-17))
                         (cond
                           ((> xnorm one)
                            (cond
                              ((> (f2cl-lib:fref work (j) ((1 *)))
                                  (f2cl-lib:f2cl/ bignum xnorm))
                               (setf (f2cl-lib:fref x (1 1) ((1 2) (1 2)))
                                       (/ (f2cl-lib:fref x (1 1) ((1 2) (1 2)))
                                          xnorm))
                               (setf scale (/ scale xnorm))))))
                         (if (/= scale one)
                             (dscal ki scale
                              (f2cl-lib:array-slice work-%data%
                                                    double-float
                                                    ((+ 1 n))
                                                    ((1 *))
                                                    work-%offset%)
                              1))
                         (setf (f2cl-lib:fref work-%data%
                                              ((f2cl-lib:int-add j n))
                                              ((1 *))
                                              work-%offset%)
                                 (f2cl-lib:fref x (1 1) ((1 2) (1 2))))
                         (daxpy (f2cl-lib:int-sub j 1)
                          (- (f2cl-lib:fref x (1 1) ((1 2) (1 2))))
                          (f2cl-lib:array-slice t$-%data%
                                                double-float
                                                (1 j)
                                                ((1 ldt) (1 *))
                                                t$-%offset%)
                          1
                          (f2cl-lib:array-slice work-%data%
                                                double-float
                                                ((+ 1 n))
                                                ((1 *))
                                                work-%offset%)
                          1))
                        (t
                         (multiple-value-bind
                               (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                var-8 var-9 var-10 var-11 var-12 var-13 var-14
                                var-15 var-16 var-17)
                             (dlaln2 f2cl-lib:%false% 2 1 smin one
                              (f2cl-lib:array-slice t$-%data%
                                                    double-float
                                                    ((+ j (f2cl-lib:int-sub 1))
                                                     (f2cl-lib:int-sub j 1))
                                                    ((1 ldt) (1 *))
                                                    t$-%offset%)
                              ldt one one
                              (f2cl-lib:array-slice work-%data%
                                                    double-float
                                                    ((+ j
                                                        (f2cl-lib:int-sub 1)
                                                        n))
                                                    ((1 *))
                                                    work-%offset%)
                              n wr zero x 2 scale xnorm ierr)
                           (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                            var-6 var-7 var-8 var-9 var-10
                                            var-11 var-12 var-13 var-14))
                           (setf scale var-15)
                           (setf xnorm var-16)
                           (setf ierr var-17))
                         (cond
                           ((> xnorm one)
                            (setf beta
                                    (max
                                     (f2cl-lib:fref work-%data%
                                                    ((f2cl-lib:int-sub j 1))
                                                    ((1 *))
                                                    work-%offset%)
                                     (f2cl-lib:fref work-%data%
                                                    (j)
                                                    ((1 *))
                                                    work-%offset%)))
                            (cond
                              ((> beta (f2cl-lib:f2cl/ bignum xnorm))
                               (setf (f2cl-lib:fref x (1 1) ((1 2) (1 2)))
                                       (/ (f2cl-lib:fref x (1 1) ((1 2) (1 2)))
                                          xnorm))
                               (setf (f2cl-lib:fref x (2 1) ((1 2) (1 2)))
                                       (/ (f2cl-lib:fref x (2 1) ((1 2) (1 2)))
                                          xnorm))
                               (setf scale (/ scale xnorm))))))
                         (if (/= scale one)
                             (dscal ki scale
                              (f2cl-lib:array-slice work-%data%
                                                    double-float
                                                    ((+ 1 n))
                                                    ((1 *))
                                                    work-%offset%)
                              1))
                         (setf (f2cl-lib:fref work-%data%
                                              ((f2cl-lib:int-add
                                                (f2cl-lib:int-sub j 1)
                                                n))
                                              ((1 *))
                                              work-%offset%)
                                 (f2cl-lib:fref x (1 1) ((1 2) (1 2))))
                         (setf (f2cl-lib:fref work-%data%
                                              ((f2cl-lib:int-add j n))
                                              ((1 *))
                                              work-%offset%)
                                 (f2cl-lib:fref x (2 1) ((1 2) (1 2))))
                         (daxpy (f2cl-lib:int-sub j 2)
                          (- (f2cl-lib:fref x (1 1) ((1 2) (1 2))))
                          (f2cl-lib:array-slice t$-%data%
                                                double-float
                                                (1 (f2cl-lib:int-sub j 1))
                                                ((1 ldt) (1 *))
                                                t$-%offset%)
                          1
                          (f2cl-lib:array-slice work-%data%
                                                double-float
                                                ((+ 1 n))
                                                ((1 *))
                                                work-%offset%)
                          1)
                         (daxpy (f2cl-lib:int-sub j 2)
                          (- (f2cl-lib:fref x (2 1) ((1 2) (1 2))))
                          (f2cl-lib:array-slice t$-%data%
                                                double-float
                                                (1 j)
                                                ((1 ldt) (1 *))
                                                t$-%offset%)
                          1
                          (f2cl-lib:array-slice work-%data%
                                                double-float
                                                ((+ 1 n))
                                                ((1 *))
                                                work-%offset%)
                          1)))
                     label60))
                  (cond
                    ((not over)
                     (dcopy ki
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            ((+ 1 n))
                                            ((1 *))
                                            work-%offset%)
                      1
                      (f2cl-lib:array-slice vr-%data%
                                            double-float
                                            (1 is)
                                            ((1 ldvr) (1 *))
                                            vr-%offset%)
                      1)
                     (setf ii
                             (idamax ki
                              (f2cl-lib:array-slice vr-%data%
                                                    double-float
                                                    (1 is)
                                                    ((1 ldvr) (1 *))
                                                    vr-%offset%)
                              1))
                     (setf remax
                             (/ one
                                (abs
                                 (f2cl-lib:fref vr-%data%
                                                (ii is)
                                                ((1 ldvr) (1 *))
                                                vr-%offset%))))
                     (dscal ki remax
                      (f2cl-lib:array-slice vr-%data%
                                            double-float
                                            (1 is)
                                            ((1 ldvr) (1 *))
                                            vr-%offset%)
                      1)
                     (f2cl-lib:fdo (k (f2cl-lib:int-add ki 1)
                                    (f2cl-lib:int-add k 1))
                                   ((> k n) nil)
                       (tagbody
                         (setf (f2cl-lib:fref vr-%data%
                                              (k is)
                                              ((1 ldvr) (1 *))
                                              vr-%offset%)
                                 zero)
                        label70)))
                    (t
                     (if (> ki 1)
                         (dgemv "N" n (f2cl-lib:int-sub ki 1) one vr ldvr
                          (f2cl-lib:array-slice work-%data%
                                                double-float
                                                ((+ 1 n))
                                                ((1 *))
                                                work-%offset%)
                          1
                          (f2cl-lib:fref work-%data%
                                         ((f2cl-lib:int-add ki n))
                                         ((1 *))
                                         work-%offset%)
                          (f2cl-lib:array-slice vr-%data%
                                                double-float
                                                (1 ki)
                                                ((1 ldvr) (1 *))
                                                vr-%offset%)
                          1))
                     (setf ii
                             (idamax n
                              (f2cl-lib:array-slice vr-%data%
                                                    double-float
                                                    (1 ki)
                                                    ((1 ldvr) (1 *))
                                                    vr-%offset%)
                              1))
                     (setf remax
                             (/ one
                                (abs
                                 (f2cl-lib:fref vr-%data%
                                                (ii ki)
                                                ((1 ldvr) (1 *))
                                                vr-%offset%))))
                     (dscal n remax
                      (f2cl-lib:array-slice vr-%data%
                                            double-float
                                            (1 ki)
                                            ((1 ldvr) (1 *))
                                            vr-%offset%)
                      1))))
                 (t
                  (cond
                    ((>=
                      (abs
                       (f2cl-lib:fref t$
                                      ((f2cl-lib:int-add ki
                                                         (f2cl-lib:int-sub 1))
                                       ki)
                                      ((1 ldt) (1 *))))
                      (abs
                       (f2cl-lib:fref t$
                                      (ki
                                       (f2cl-lib:int-add ki
                                                         (f2cl-lib:int-sub 1)))
                                      ((1 ldt) (1 *)))))
                     (setf (f2cl-lib:fref work-%data%
                                          ((f2cl-lib:int-add
                                            (f2cl-lib:int-sub ki 1)
                                            n))
                                          ((1 *))
                                          work-%offset%)
                             one)
                     (setf (f2cl-lib:fref work-%data%
                                          ((f2cl-lib:int-add ki n2))
                                          ((1 *))
                                          work-%offset%)
                             (/ wi
                                (f2cl-lib:fref t$-%data%
                                               ((f2cl-lib:int-sub ki 1) ki)
                                               ((1 ldt) (1 *))
                                               t$-%offset%))))
                    (t
                     (setf (f2cl-lib:fref work-%data%
                                          ((f2cl-lib:int-add
                                            (f2cl-lib:int-sub ki 1)
                                            n))
                                          ((1 *))
                                          work-%offset%)
                             (/ (- wi)
                                (f2cl-lib:fref t$-%data%
                                               (ki (f2cl-lib:int-sub ki 1))
                                               ((1 ldt) (1 *))
                                               t$-%offset%)))
                     (setf (f2cl-lib:fref work-%data%
                                          ((f2cl-lib:int-add ki n2))
                                          ((1 *))
                                          work-%offset%)
                             one)))
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add ki n))
                                       ((1 *))
                                       work-%offset%)
                          zero)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub ki 1)
                                         n2))
                                       ((1 *))
                                       work-%offset%)
                          zero)
                  (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                ((> k
                                    (f2cl-lib:int-add ki (f2cl-lib:int-sub 2)))
                                 nil)
                    (tagbody
                      (setf (f2cl-lib:fref work-%data%
                                           ((f2cl-lib:int-add k n))
                                           ((1 *))
                                           work-%offset%)
                              (*
                               (-
                                (f2cl-lib:fref work-%data%
                                               ((f2cl-lib:int-add
                                                 (f2cl-lib:int-sub ki 1)
                                                 n))
                                               ((1 *))
                                               work-%offset%))
                               (f2cl-lib:fref t$-%data%
                                              (k (f2cl-lib:int-sub ki 1))
                                              ((1 ldt) (1 *))
                                              t$-%offset%)))
                      (setf (f2cl-lib:fref work-%data%
                                           ((f2cl-lib:int-add k n2))
                                           ((1 *))
                                           work-%offset%)
                              (*
                               (-
                                (f2cl-lib:fref work-%data%
                                               ((f2cl-lib:int-add ki n2))
                                               ((1 *))
                                               work-%offset%))
                               (f2cl-lib:fref t$-%data%
                                              (k ki)
                                              ((1 ldt) (1 *))
                                              t$-%offset%)))
                     label80))
                  (setf jnxt (f2cl-lib:int-sub ki 2))
                  (f2cl-lib:fdo (j (f2cl-lib:int-add ki (f2cl-lib:int-sub 2))
                                 (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                                ((> j 1) nil)
                    (tagbody
                      (if (> j jnxt) (go label90))
                      (setf j1 j)
                      (setf j2 j)
                      (setf jnxt (f2cl-lib:int-sub j 1))
                      (cond
                        ((> j 1)
                         (cond
                           ((/=
                             (f2cl-lib:fref t$
                                            (j
                                             (f2cl-lib:int-add j
                                                               (f2cl-lib:int-sub
                                                                1)))
                                            ((1 ldt) (1 *)))
                             zero)
                            (setf j1 (f2cl-lib:int-sub j 1))
                            (setf jnxt (f2cl-lib:int-sub j 2))))))
                      (cond
                        ((= j1 j2)
                         (multiple-value-bind
                               (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                var-8 var-9 var-10 var-11 var-12 var-13 var-14
                                var-15 var-16 var-17)
                             (dlaln2 f2cl-lib:%false% 1 2 smin one
                              (f2cl-lib:array-slice t$-%data%
                                                    double-float
                                                    (j j)
                                                    ((1 ldt) (1 *))
                                                    t$-%offset%)
                              ldt one one
                              (f2cl-lib:array-slice work-%data%
                                                    double-float
                                                    ((+ j n))
                                                    ((1 *))
                                                    work-%offset%)
                              n wr wi x 2 scale xnorm ierr)
                           (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                            var-6 var-7 var-8 var-9 var-10
                                            var-11 var-12 var-13 var-14))
                           (setf scale var-15)
                           (setf xnorm var-16)
                           (setf ierr var-17))
                         (cond
                           ((> xnorm one)
                            (cond
                              ((> (f2cl-lib:fref work (j) ((1 *)))
                                  (f2cl-lib:f2cl/ bignum xnorm))
                               (setf (f2cl-lib:fref x (1 1) ((1 2) (1 2)))
                                       (/ (f2cl-lib:fref x (1 1) ((1 2) (1 2)))
                                          xnorm))
                               (setf (f2cl-lib:fref x (1 2) ((1 2) (1 2)))
                                       (/ (f2cl-lib:fref x (1 2) ((1 2) (1 2)))
                                          xnorm))
                               (setf scale (/ scale xnorm))))))
                         (cond
                           ((/= scale one)
                            (dscal ki scale
                             (f2cl-lib:array-slice work-%data%
                                                   double-float
                                                   ((+ 1 n))
                                                   ((1 *))
                                                   work-%offset%)
                             1)
                            (dscal ki scale
                             (f2cl-lib:array-slice work-%data%
                                                   double-float
                                                   ((+ 1 n2))
                                                   ((1 *))
                                                   work-%offset%)
                             1)))
                         (setf (f2cl-lib:fref work-%data%
                                              ((f2cl-lib:int-add j n))
                                              ((1 *))
                                              work-%offset%)
                                 (f2cl-lib:fref x (1 1) ((1 2) (1 2))))
                         (setf (f2cl-lib:fref work-%data%
                                              ((f2cl-lib:int-add j n2))
                                              ((1 *))
                                              work-%offset%)
                                 (f2cl-lib:fref x (1 2) ((1 2) (1 2))))
                         (daxpy (f2cl-lib:int-sub j 1)
                          (- (f2cl-lib:fref x (1 1) ((1 2) (1 2))))
                          (f2cl-lib:array-slice t$-%data%
                                                double-float
                                                (1 j)
                                                ((1 ldt) (1 *))
                                                t$-%offset%)
                          1
                          (f2cl-lib:array-slice work-%data%
                                                double-float
                                                ((+ 1 n))
                                                ((1 *))
                                                work-%offset%)
                          1)
                         (daxpy (f2cl-lib:int-sub j 1)
                          (- (f2cl-lib:fref x (1 2) ((1 2) (1 2))))
                          (f2cl-lib:array-slice t$-%data%
                                                double-float
                                                (1 j)
                                                ((1 ldt) (1 *))
                                                t$-%offset%)
                          1
                          (f2cl-lib:array-slice work-%data%
                                                double-float
                                                ((+ 1 n2))
                                                ((1 *))
                                                work-%offset%)
                          1))
                        (t
                         (multiple-value-bind
                               (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                var-8 var-9 var-10 var-11 var-12 var-13 var-14
                                var-15 var-16 var-17)
                             (dlaln2 f2cl-lib:%false% 2 2 smin one
                              (f2cl-lib:array-slice t$-%data%
                                                    double-float
                                                    ((+ j (f2cl-lib:int-sub 1))
                                                     (f2cl-lib:int-sub j 1))
                                                    ((1 ldt) (1 *))
                                                    t$-%offset%)
                              ldt one one
                              (f2cl-lib:array-slice work-%data%
                                                    double-float
                                                    ((+ j
                                                        (f2cl-lib:int-sub 1)
                                                        n))
                                                    ((1 *))
                                                    work-%offset%)
                              n wr wi x 2 scale xnorm ierr)
                           (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                            var-6 var-7 var-8 var-9 var-10
                                            var-11 var-12 var-13 var-14))
                           (setf scale var-15)
                           (setf xnorm var-16)
                           (setf ierr var-17))
                         (cond
                           ((> xnorm one)
                            (setf beta
                                    (max
                                     (f2cl-lib:fref work-%data%
                                                    ((f2cl-lib:int-sub j 1))
                                                    ((1 *))
                                                    work-%offset%)
                                     (f2cl-lib:fref work-%data%
                                                    (j)
                                                    ((1 *))
                                                    work-%offset%)))
                            (cond
                              ((> beta (f2cl-lib:f2cl/ bignum xnorm))
                               (setf rec (/ one xnorm))
                               (setf (f2cl-lib:fref x (1 1) ((1 2) (1 2)))
                                       (* (f2cl-lib:fref x (1 1) ((1 2) (1 2)))
                                          rec))
                               (setf (f2cl-lib:fref x (1 2) ((1 2) (1 2)))
                                       (* (f2cl-lib:fref x (1 2) ((1 2) (1 2)))
                                          rec))
                               (setf (f2cl-lib:fref x (2 1) ((1 2) (1 2)))
                                       (* (f2cl-lib:fref x (2 1) ((1 2) (1 2)))
                                          rec))
                               (setf (f2cl-lib:fref x (2 2) ((1 2) (1 2)))
                                       (* (f2cl-lib:fref x (2 2) ((1 2) (1 2)))
                                          rec))
                               (setf scale (* scale rec))))))
                         (cond
                           ((/= scale one)
                            (dscal ki scale
                             (f2cl-lib:array-slice work-%data%
                                                   double-float
                                                   ((+ 1 n))
                                                   ((1 *))
                                                   work-%offset%)
                             1)
                            (dscal ki scale
                             (f2cl-lib:array-slice work-%data%
                                                   double-float
                                                   ((+ 1 n2))
                                                   ((1 *))
                                                   work-%offset%)
                             1)))
                         (setf (f2cl-lib:fref work-%data%
                                              ((f2cl-lib:int-add
                                                (f2cl-lib:int-sub j 1)
                                                n))
                                              ((1 *))
                                              work-%offset%)
                                 (f2cl-lib:fref x (1 1) ((1 2) (1 2))))
                         (setf (f2cl-lib:fref work-%data%
                                              ((f2cl-lib:int-add j n))
                                              ((1 *))
                                              work-%offset%)
                                 (f2cl-lib:fref x (2 1) ((1 2) (1 2))))
                         (setf (f2cl-lib:fref work-%data%
                                              ((f2cl-lib:int-add
                                                (f2cl-lib:int-sub j 1)
                                                n2))
                                              ((1 *))
                                              work-%offset%)
                                 (f2cl-lib:fref x (1 2) ((1 2) (1 2))))
                         (setf (f2cl-lib:fref work-%data%
                                              ((f2cl-lib:int-add j n2))
                                              ((1 *))
                                              work-%offset%)
                                 (f2cl-lib:fref x (2 2) ((1 2) (1 2))))
                         (daxpy (f2cl-lib:int-sub j 2)
                          (- (f2cl-lib:fref x (1 1) ((1 2) (1 2))))
                          (f2cl-lib:array-slice t$-%data%
                                                double-float
                                                (1 (f2cl-lib:int-sub j 1))
                                                ((1 ldt) (1 *))
                                                t$-%offset%)
                          1
                          (f2cl-lib:array-slice work-%data%
                                                double-float
                                                ((+ 1 n))
                                                ((1 *))
                                                work-%offset%)
                          1)
                         (daxpy (f2cl-lib:int-sub j 2)
                          (- (f2cl-lib:fref x (2 1) ((1 2) (1 2))))
                          (f2cl-lib:array-slice t$-%data%
                                                double-float
                                                (1 j)
                                                ((1 ldt) (1 *))
                                                t$-%offset%)
                          1
                          (f2cl-lib:array-slice work-%data%
                                                double-float
                                                ((+ 1 n))
                                                ((1 *))
                                                work-%offset%)
                          1)
                         (daxpy (f2cl-lib:int-sub j 2)
                          (- (f2cl-lib:fref x (1 2) ((1 2) (1 2))))
                          (f2cl-lib:array-slice t$-%data%
                                                double-float
                                                (1 (f2cl-lib:int-sub j 1))
                                                ((1 ldt) (1 *))
                                                t$-%offset%)
                          1
                          (f2cl-lib:array-slice work-%data%
                                                double-float
                                                ((+ 1 n2))
                                                ((1 *))
                                                work-%offset%)
                          1)
                         (daxpy (f2cl-lib:int-sub j 2)
                          (- (f2cl-lib:fref x (2 2) ((1 2) (1 2))))
                          (f2cl-lib:array-slice t$-%data%
                                                double-float
                                                (1 j)
                                                ((1 ldt) (1 *))
                                                t$-%offset%)
                          1
                          (f2cl-lib:array-slice work-%data%
                                                double-float
                                                ((+ 1 n2))
                                                ((1 *))
                                                work-%offset%)
                          1)))
                     label90))
                  (cond
                    ((not over)
                     (dcopy ki
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            ((+ 1 n))
                                            ((1 *))
                                            work-%offset%)
                      1
                      (f2cl-lib:array-slice vr-%data%
                                            double-float
                                            (1 (f2cl-lib:int-sub is 1))
                                            ((1 ldvr) (1 *))
                                            vr-%offset%)
                      1)
                     (dcopy ki
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            ((+ 1 n2))
                                            ((1 *))
                                            work-%offset%)
                      1
                      (f2cl-lib:array-slice vr-%data%
                                            double-float
                                            (1 is)
                                            ((1 ldvr) (1 *))
                                            vr-%offset%)
                      1)
                     (setf emax zero)
                     (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                   ((> k ki) nil)
                       (tagbody
                         (setf emax
                                 (max emax
                                      (+
                                       (abs
                                        (f2cl-lib:fref vr-%data%
                                                       (k
                                                        (f2cl-lib:int-sub is
                                                                          1))
                                                       ((1 ldvr) (1 *))
                                                       vr-%offset%))
                                       (abs
                                        (f2cl-lib:fref vr-%data%
                                                       (k is)
                                                       ((1 ldvr) (1 *))
                                                       vr-%offset%)))))
                        label100))
                     (setf remax (/ one emax))
                     (dscal ki remax
                      (f2cl-lib:array-slice vr-%data%
                                            double-float
                                            (1 (f2cl-lib:int-sub is 1))
                                            ((1 ldvr) (1 *))
                                            vr-%offset%)
                      1)
                     (dscal ki remax
                      (f2cl-lib:array-slice vr-%data%
                                            double-float
                                            (1 is)
                                            ((1 ldvr) (1 *))
                                            vr-%offset%)
                      1)
                     (f2cl-lib:fdo (k (f2cl-lib:int-add ki 1)
                                    (f2cl-lib:int-add k 1))
                                   ((> k n) nil)
                       (tagbody
                         (setf (f2cl-lib:fref vr-%data%
                                              (k (f2cl-lib:int-sub is 1))
                                              ((1 ldvr) (1 *))
                                              vr-%offset%)
                                 zero)
                         (setf (f2cl-lib:fref vr-%data%
                                              (k is)
                                              ((1 ldvr) (1 *))
                                              vr-%offset%)
                                 zero)
                        label110)))
                    (t
                     (cond
                       ((> ki 2)
                        (dgemv "N" n (f2cl-lib:int-sub ki 2) one vr ldvr
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               ((+ 1 n))
                                               ((1 *))
                                               work-%offset%)
                         1
                         (f2cl-lib:fref work-%data%
                                        ((f2cl-lib:int-add
                                          (f2cl-lib:int-sub ki 1)
                                          n))
                                        ((1 *))
                                        work-%offset%)
                         (f2cl-lib:array-slice vr-%data%
                                               double-float
                                               (1 (f2cl-lib:int-sub ki 1))
                                               ((1 ldvr) (1 *))
                                               vr-%offset%)
                         1)
                        (dgemv "N" n (f2cl-lib:int-sub ki 2) one vr ldvr
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               ((+ 1 n2))
                                               ((1 *))
                                               work-%offset%)
                         1
                         (f2cl-lib:fref work-%data%
                                        ((f2cl-lib:int-add ki n2))
                                        ((1 *))
                                        work-%offset%)
                         (f2cl-lib:array-slice vr-%data%
                                               double-float
                                               (1 ki)
                                               ((1 ldvr) (1 *))
                                               vr-%offset%)
                         1))
                       (t
                        (dscal n
                         (f2cl-lib:fref work-%data%
                                        ((f2cl-lib:int-add
                                          (f2cl-lib:int-sub ki 1)
                                          n))
                                        ((1 *))
                                        work-%offset%)
                         (f2cl-lib:array-slice vr-%data%
                                               double-float
                                               (1 (f2cl-lib:int-sub ki 1))
                                               ((1 ldvr) (1 *))
                                               vr-%offset%)
                         1)
                        (dscal n
                         (f2cl-lib:fref work-%data%
                                        ((f2cl-lib:int-add ki n2))
                                        ((1 *))
                                        work-%offset%)
                         (f2cl-lib:array-slice vr-%data%
                                               double-float
                                               (1 ki)
                                               ((1 ldvr) (1 *))
                                               vr-%offset%)
                         1)))
                     (setf emax zero)
                     (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                   ((> k n) nil)
                       (tagbody
                         (setf emax
                                 (max emax
                                      (+
                                       (abs
                                        (f2cl-lib:fref vr-%data%
                                                       (k
                                                        (f2cl-lib:int-sub ki
                                                                          1))
                                                       ((1 ldvr) (1 *))
                                                       vr-%offset%))
                                       (abs
                                        (f2cl-lib:fref vr-%data%
                                                       (k ki)
                                                       ((1 ldvr) (1 *))
                                                       vr-%offset%)))))
                        label120))
                     (setf remax (/ one emax))
                     (dscal n remax
                      (f2cl-lib:array-slice vr-%data%
                                            double-float
                                            (1 (f2cl-lib:int-sub ki 1))
                                            ((1 ldvr) (1 *))
                                            vr-%offset%)
                      1)
                     (dscal n remax
                      (f2cl-lib:array-slice vr-%data%
                                            double-float
                                            (1 ki)
                                            ((1 ldvr) (1 *))
                                            vr-%offset%)
                      1)))))
               (setf is (f2cl-lib:int-sub is 1))
               (if (/= ip 0) (setf is (f2cl-lib:int-sub is 1)))
              label130
               (if (= ip 1) (setf ip 0))
               (if (= ip -1) (setf ip 1))
              label140))))
        (cond
          (leftv
           (setf ip 0)
           (setf is 1)
           (f2cl-lib:fdo (ki 1 (f2cl-lib:int-add ki 1))
                         ((> ki n) nil)
             (tagbody
               (if (= ip -1) (go label250))
               (if (= ki n) (go label150))
               (if
                (=
                 (f2cl-lib:fref t$-%data%
                                ((f2cl-lib:int-add ki 1) ki)
                                ((1 ldt) (1 *))
                                t$-%offset%)
                 zero)
                (go label150))
               (setf ip 1)
              label150
               (cond
                 (somev
                  (if
                   (not
                    (f2cl-lib:fref select-%data% (ki) ((1 *)) select-%offset%))
                   (go label250))))
               (setf wr
                       (f2cl-lib:fref t$-%data%
                                      (ki ki)
                                      ((1 ldt) (1 *))
                                      t$-%offset%))
               (setf wi zero)
               (if (/= ip 0)
                   (setf wi
                           (*
                            (f2cl-lib:fsqrt
                             (abs
                              (f2cl-lib:fref t$-%data%
                                             (ki (f2cl-lib:int-add ki 1))
                                             ((1 ldt) (1 *))
                                             t$-%offset%)))
                            (f2cl-lib:fsqrt
                             (abs
                              (f2cl-lib:fref t$-%data%
                                             ((f2cl-lib:int-add ki 1) ki)
                                             ((1 ldt) (1 *))
                                             t$-%offset%))))))
               (setf smin (max (* ulp (+ (abs wr) (abs wi))) smlnum))
               (cond
                 ((= ip 0)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add ki n))
                                       ((1 *))
                                       work-%offset%)
                          one)
                  (f2cl-lib:fdo (k (f2cl-lib:int-add ki 1)
                                 (f2cl-lib:int-add k 1))
                                ((> k n) nil)
                    (tagbody
                      (setf (f2cl-lib:fref work-%data%
                                           ((f2cl-lib:int-add k n))
                                           ((1 *))
                                           work-%offset%)
                              (-
                               (f2cl-lib:fref t$-%data%
                                              (ki k)
                                              ((1 ldt) (1 *))
                                              t$-%offset%)))
                     label160))
                  (setf vmax one)
                  (setf vcrit bignum)
                  (setf jnxt (f2cl-lib:int-add ki 1))
                  (f2cl-lib:fdo (j (f2cl-lib:int-add ki 1)
                                 (f2cl-lib:int-add j 1))
                                ((> j n) nil)
                    (tagbody
                      (if (< j jnxt) (go label170))
                      (setf j1 j)
                      (setf j2 j)
                      (setf jnxt (f2cl-lib:int-add j 1))
                      (cond
                        ((< j n)
                         (cond
                           ((/=
                             (f2cl-lib:fref t$
                                            ((f2cl-lib:int-add j 1) j)
                                            ((1 ldt) (1 *)))
                             zero)
                            (setf j2 (f2cl-lib:int-add j 1))
                            (setf jnxt (f2cl-lib:int-add j 2))))))
                      (cond
                        ((= j1 j2)
                         (cond
                           ((> (f2cl-lib:fref work (j) ((1 *))) vcrit)
                            (setf rec (/ one vmax))
                            (dscal (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1)
                             rec
                             (f2cl-lib:array-slice work-%data%
                                                   double-float
                                                   ((+ ki n))
                                                   ((1 *))
                                                   work-%offset%)
                             1)
                            (setf vmax one)
                            (setf vcrit bignum)))
                         (setf (f2cl-lib:fref work-%data%
                                              ((f2cl-lib:int-add j n))
                                              ((1 *))
                                              work-%offset%)
                                 (-
                                  (f2cl-lib:fref work-%data%
                                                 ((f2cl-lib:int-add j n))
                                                 ((1 *))
                                                 work-%offset%)
                                  (ddot (f2cl-lib:int-sub j ki 1)
                                   (f2cl-lib:array-slice t$-%data%
                                                         double-float
                                                         ((+ ki 1) j)
                                                         ((1 ldt) (1 *))
                                                         t$-%offset%)
                                   1
                                   (f2cl-lib:array-slice work-%data%
                                                         double-float
                                                         ((+ ki 1 n))
                                                         ((1 *))
                                                         work-%offset%)
                                   1)))
                         (multiple-value-bind
                               (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                var-8 var-9 var-10 var-11 var-12 var-13 var-14
                                var-15 var-16 var-17)
                             (dlaln2 f2cl-lib:%false% 1 1 smin one
                              (f2cl-lib:array-slice t$-%data%
                                                    double-float
                                                    (j j)
                                                    ((1 ldt) (1 *))
                                                    t$-%offset%)
                              ldt one one
                              (f2cl-lib:array-slice work-%data%
                                                    double-float
                                                    ((+ j n))
                                                    ((1 *))
                                                    work-%offset%)
                              n wr zero x 2 scale xnorm ierr)
                           (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                            var-6 var-7 var-8 var-9 var-10
                                            var-11 var-12 var-13 var-14))
                           (setf scale var-15)
                           (setf xnorm var-16)
                           (setf ierr var-17))
                         (if (/= scale one)
                             (dscal
                              (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1)
                              scale
                              (f2cl-lib:array-slice work-%data%
                                                    double-float
                                                    ((+ ki n))
                                                    ((1 *))
                                                    work-%offset%)
                              1))
                         (setf (f2cl-lib:fref work-%data%
                                              ((f2cl-lib:int-add j n))
                                              ((1 *))
                                              work-%offset%)
                                 (f2cl-lib:fref x (1 1) ((1 2) (1 2))))
                         (setf vmax
                                 (max
                                  (abs
                                   (f2cl-lib:fref work-%data%
                                                  ((f2cl-lib:int-add j n))
                                                  ((1 *))
                                                  work-%offset%))
                                  vmax))
                         (setf vcrit (/ bignum vmax)))
                        (t
                         (setf beta
                                 (max
                                  (f2cl-lib:fref work-%data%
                                                 (j)
                                                 ((1 *))
                                                 work-%offset%)
                                  (f2cl-lib:fref work-%data%
                                                 ((f2cl-lib:int-add j 1))
                                                 ((1 *))
                                                 work-%offset%)))
                         (cond
                           ((> beta vcrit)
                            (setf rec (/ one vmax))
                            (dscal (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1)
                             rec
                             (f2cl-lib:array-slice work-%data%
                                                   double-float
                                                   ((+ ki n))
                                                   ((1 *))
                                                   work-%offset%)
                             1)
                            (setf vmax one)
                            (setf vcrit bignum)))
                         (setf (f2cl-lib:fref work-%data%
                                              ((f2cl-lib:int-add j n))
                                              ((1 *))
                                              work-%offset%)
                                 (-
                                  (f2cl-lib:fref work-%data%
                                                 ((f2cl-lib:int-add j n))
                                                 ((1 *))
                                                 work-%offset%)
                                  (ddot (f2cl-lib:int-sub j ki 1)
                                   (f2cl-lib:array-slice t$-%data%
                                                         double-float
                                                         ((+ ki 1) j)
                                                         ((1 ldt) (1 *))
                                                         t$-%offset%)
                                   1
                                   (f2cl-lib:array-slice work-%data%
                                                         double-float
                                                         ((+ ki 1 n))
                                                         ((1 *))
                                                         work-%offset%)
                                   1)))
                         (setf (f2cl-lib:fref work-%data%
                                              ((f2cl-lib:int-add j 1 n))
                                              ((1 *))
                                              work-%offset%)
                                 (-
                                  (f2cl-lib:fref work-%data%
                                                 ((f2cl-lib:int-add j 1 n))
                                                 ((1 *))
                                                 work-%offset%)
                                  (ddot (f2cl-lib:int-sub j ki 1)
                                   (f2cl-lib:array-slice t$-%data%
                                                         double-float
                                                         ((+ ki 1)
                                                          (f2cl-lib:int-add j
                                                                            1))
                                                         ((1 ldt) (1 *))
                                                         t$-%offset%)
                                   1
                                   (f2cl-lib:array-slice work-%data%
                                                         double-float
                                                         ((+ ki 1 n))
                                                         ((1 *))
                                                         work-%offset%)
                                   1)))
                         (multiple-value-bind
                               (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                var-8 var-9 var-10 var-11 var-12 var-13 var-14
                                var-15 var-16 var-17)
                             (dlaln2 f2cl-lib:%true% 2 1 smin one
                              (f2cl-lib:array-slice t$-%data%
                                                    double-float
                                                    (j j)
                                                    ((1 ldt) (1 *))
                                                    t$-%offset%)
                              ldt one one
                              (f2cl-lib:array-slice work-%data%
                                                    double-float
                                                    ((+ j n))
                                                    ((1 *))
                                                    work-%offset%)
                              n wr zero x 2 scale xnorm ierr)
                           (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                            var-6 var-7 var-8 var-9 var-10
                                            var-11 var-12 var-13 var-14))
                           (setf scale var-15)
                           (setf xnorm var-16)
                           (setf ierr var-17))
                         (if (/= scale one)
                             (dscal
                              (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1)
                              scale
                              (f2cl-lib:array-slice work-%data%
                                                    double-float
                                                    ((+ ki n))
                                                    ((1 *))
                                                    work-%offset%)
                              1))
                         (setf (f2cl-lib:fref work-%data%
                                              ((f2cl-lib:int-add j n))
                                              ((1 *))
                                              work-%offset%)
                                 (f2cl-lib:fref x (1 1) ((1 2) (1 2))))
                         (setf (f2cl-lib:fref work-%data%
                                              ((f2cl-lib:int-add j 1 n))
                                              ((1 *))
                                              work-%offset%)
                                 (f2cl-lib:fref x (2 1) ((1 2) (1 2))))
                         (setf vmax
                                 (max
                                  (abs
                                   (f2cl-lib:fref work-%data%
                                                  ((f2cl-lib:int-add j n))
                                                  ((1 *))
                                                  work-%offset%))
                                  (abs
                                   (f2cl-lib:fref work-%data%
                                                  ((f2cl-lib:int-add j 1 n))
                                                  ((1 *))
                                                  work-%offset%))
                                  vmax))
                         (setf vcrit (/ bignum vmax))))
                     label170))
                  (cond
                    ((not over)
                     (dcopy (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            ((+ ki n))
                                            ((1 *))
                                            work-%offset%)
                      1
                      (f2cl-lib:array-slice vl-%data%
                                            double-float
                                            (ki is)
                                            ((1 ldvl) (1 *))
                                            vl-%offset%)
                      1)
                     (setf ii
                             (f2cl-lib:int-sub
                              (f2cl-lib:int-add
                               (idamax
                                (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1)
                                (f2cl-lib:array-slice vl-%data%
                                                      double-float
                                                      (ki is)
                                                      ((1 ldvl) (1 *))
                                                      vl-%offset%)
                                1)
                               ki)
                              1))
                     (setf remax
                             (/ one
                                (abs
                                 (f2cl-lib:fref vl-%data%
                                                (ii is)
                                                ((1 ldvl) (1 *))
                                                vl-%offset%))))
                     (dscal (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1) remax
                      (f2cl-lib:array-slice vl-%data%
                                            double-float
                                            (ki is)
                                            ((1 ldvl) (1 *))
                                            vl-%offset%)
                      1)
                     (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                   ((> k
                                       (f2cl-lib:int-add ki
                                                         (f2cl-lib:int-sub 1)))
                                    nil)
                       (tagbody
                         (setf (f2cl-lib:fref vl-%data%
                                              (k is)
                                              ((1 ldvl) (1 *))
                                              vl-%offset%)
                                 zero)
                        label180)))
                    (t
                     (if (< ki n)
                         (dgemv "N" n (f2cl-lib:int-sub n ki) one
                          (f2cl-lib:array-slice vl-%data%
                                                double-float
                                                (1 (f2cl-lib:int-add ki 1))
                                                ((1 ldvl) (1 *))
                                                vl-%offset%)
                          ldvl
                          (f2cl-lib:array-slice work-%data%
                                                double-float
                                                ((+ ki 1 n))
                                                ((1 *))
                                                work-%offset%)
                          1
                          (f2cl-lib:fref work-%data%
                                         ((f2cl-lib:int-add ki n))
                                         ((1 *))
                                         work-%offset%)
                          (f2cl-lib:array-slice vl-%data%
                                                double-float
                                                (1 ki)
                                                ((1 ldvl) (1 *))
                                                vl-%offset%)
                          1))
                     (setf ii
                             (idamax n
                              (f2cl-lib:array-slice vl-%data%
                                                    double-float
                                                    (1 ki)
                                                    ((1 ldvl) (1 *))
                                                    vl-%offset%)
                              1))
                     (setf remax
                             (/ one
                                (abs
                                 (f2cl-lib:fref vl-%data%
                                                (ii ki)
                                                ((1 ldvl) (1 *))
                                                vl-%offset%))))
                     (dscal n remax
                      (f2cl-lib:array-slice vl-%data%
                                            double-float
                                            (1 ki)
                                            ((1 ldvl) (1 *))
                                            vl-%offset%)
                      1))))
                 (t
                  (tagbody
                    (cond
                      ((>=
                        (abs
                         (f2cl-lib:fref t$
                                        (ki (f2cl-lib:int-add ki 1))
                                        ((1 ldt) (1 *))))
                        (abs
                         (f2cl-lib:fref t$
                                        ((f2cl-lib:int-add ki 1) ki)
                                        ((1 ldt) (1 *)))))
                       (setf (f2cl-lib:fref work-%data%
                                            ((f2cl-lib:int-add ki n))
                                            ((1 *))
                                            work-%offset%)
                               (/ wi
                                  (f2cl-lib:fref t$-%data%
                                                 (ki (f2cl-lib:int-add ki 1))
                                                 ((1 ldt) (1 *))
                                                 t$-%offset%)))
                       (setf (f2cl-lib:fref work-%data%
                                            ((f2cl-lib:int-add ki 1 n2))
                                            ((1 *))
                                            work-%offset%)
                               one))
                      (t
                       (setf (f2cl-lib:fref work-%data%
                                            ((f2cl-lib:int-add ki n))
                                            ((1 *))
                                            work-%offset%)
                               one)
                       (setf (f2cl-lib:fref work-%data%
                                            ((f2cl-lib:int-add ki 1 n2))
                                            ((1 *))
                                            work-%offset%)
                               (/ (- wi)
                                  (f2cl-lib:fref t$-%data%
                                                 ((f2cl-lib:int-add ki 1) ki)
                                                 ((1 ldt) (1 *))
                                                 t$-%offset%)))))
                    (setf (f2cl-lib:fref work-%data%
                                         ((f2cl-lib:int-add ki 1 n))
                                         ((1 *))
                                         work-%offset%)
                            zero)
                    (setf (f2cl-lib:fref work-%data%
                                         ((f2cl-lib:int-add ki n2))
                                         ((1 *))
                                         work-%offset%)
                            zero)
                    (f2cl-lib:fdo (k (f2cl-lib:int-add ki 2)
                                   (f2cl-lib:int-add k 1))
                                  ((> k n) nil)
                      (tagbody
                        (setf (f2cl-lib:fref work-%data%
                                             ((f2cl-lib:int-add k n))
                                             ((1 *))
                                             work-%offset%)
                                (*
                                 (-
                                  (f2cl-lib:fref work-%data%
                                                 ((f2cl-lib:int-add ki n))
                                                 ((1 *))
                                                 work-%offset%))
                                 (f2cl-lib:fref t$-%data%
                                                (ki k)
                                                ((1 ldt) (1 *))
                                                t$-%offset%)))
                        (setf (f2cl-lib:fref work-%data%
                                             ((f2cl-lib:int-add k n2))
                                             ((1 *))
                                             work-%offset%)
                                (*
                                 (-
                                  (f2cl-lib:fref work-%data%
                                                 ((f2cl-lib:int-add ki 1 n2))
                                                 ((1 *))
                                                 work-%offset%))
                                 (f2cl-lib:fref t$-%data%
                                                ((f2cl-lib:int-add ki 1) k)
                                                ((1 ldt) (1 *))
                                                t$-%offset%)))
                       label190))
                    (setf vmax one)
                    (setf vcrit bignum)
                    (setf jnxt (f2cl-lib:int-add ki 2))
                    (f2cl-lib:fdo (j (f2cl-lib:int-add ki 2)
                                   (f2cl-lib:int-add j 1))
                                  ((> j n) nil)
                      (tagbody
                        (if (< j jnxt) (go label200))
                        (setf j1 j)
                        (setf j2 j)
                        (setf jnxt (f2cl-lib:int-add j 1))
                        (cond
                          ((< j n)
                           (cond
                             ((/=
                               (f2cl-lib:fref t$
                                              ((f2cl-lib:int-add j 1) j)
                                              ((1 ldt) (1 *)))
                               zero)
                              (setf j2 (f2cl-lib:int-add j 1))
                              (setf jnxt (f2cl-lib:int-add j 2))))))
                        (cond
                          ((= j1 j2)
                           (cond
                             ((> (f2cl-lib:fref work (j) ((1 *))) vcrit)
                              (setf rec (/ one vmax))
                              (dscal
                               (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1) rec
                               (f2cl-lib:array-slice work-%data%
                                                     double-float
                                                     ((+ ki n))
                                                     ((1 *))
                                                     work-%offset%)
                               1)
                              (dscal
                               (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1) rec
                               (f2cl-lib:array-slice work-%data%
                                                     double-float
                                                     ((+ ki n2))
                                                     ((1 *))
                                                     work-%offset%)
                               1)
                              (setf vmax one)
                              (setf vcrit bignum)))
                           (setf (f2cl-lib:fref work-%data%
                                                ((f2cl-lib:int-add j n))
                                                ((1 *))
                                                work-%offset%)
                                   (-
                                    (f2cl-lib:fref work-%data%
                                                   ((f2cl-lib:int-add j n))
                                                   ((1 *))
                                                   work-%offset%)
                                    (ddot (f2cl-lib:int-sub j ki 2)
                                     (f2cl-lib:array-slice t$-%data%
                                                           double-float
                                                           ((+ ki 2) j)
                                                           ((1 ldt) (1 *))
                                                           t$-%offset%)
                                     1
                                     (f2cl-lib:array-slice work-%data%
                                                           double-float
                                                           ((+ ki 2 n))
                                                           ((1 *))
                                                           work-%offset%)
                                     1)))
                           (setf (f2cl-lib:fref work-%data%
                                                ((f2cl-lib:int-add j n2))
                                                ((1 *))
                                                work-%offset%)
                                   (-
                                    (f2cl-lib:fref work-%data%
                                                   ((f2cl-lib:int-add j n2))
                                                   ((1 *))
                                                   work-%offset%)
                                    (ddot (f2cl-lib:int-sub j ki 2)
                                     (f2cl-lib:array-slice t$-%data%
                                                           double-float
                                                           ((+ ki 2) j)
                                                           ((1 ldt) (1 *))
                                                           t$-%offset%)
                                     1
                                     (f2cl-lib:array-slice work-%data%
                                                           double-float
                                                           ((+ ki 2 n2))
                                                           ((1 *))
                                                           work-%offset%)
                                     1)))
                           (multiple-value-bind
                                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                  var-7 var-8 var-9 var-10 var-11 var-12 var-13
                                  var-14 var-15 var-16 var-17)
                               (dlaln2 f2cl-lib:%false% 1 2 smin one
                                (f2cl-lib:array-slice t$-%data%
                                                      double-float
                                                      (j j)
                                                      ((1 ldt) (1 *))
                                                      t$-%offset%)
                                ldt one one
                                (f2cl-lib:array-slice work-%data%
                                                      double-float
                                                      ((+ j n))
                                                      ((1 *))
                                                      work-%offset%)
                                n wr (- wi) x 2 scale xnorm ierr)
                             (declare (ignore var-0 var-1 var-2 var-3 var-4
                                              var-5 var-6 var-7 var-8 var-9
                                              var-10 var-11 var-12 var-13
                                              var-14))
                             (setf scale var-15)
                             (setf xnorm var-16)
                             (setf ierr var-17))
                           (cond
                             ((/= scale one)
                              (dscal
                               (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1)
                               scale
                               (f2cl-lib:array-slice work-%data%
                                                     double-float
                                                     ((+ ki n))
                                                     ((1 *))
                                                     work-%offset%)
                               1)
                              (dscal
                               (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1)
                               scale
                               (f2cl-lib:array-slice work-%data%
                                                     double-float
                                                     ((+ ki n2))
                                                     ((1 *))
                                                     work-%offset%)
                               1)))
                           (setf (f2cl-lib:fref work-%data%
                                                ((f2cl-lib:int-add j n))
                                                ((1 *))
                                                work-%offset%)
                                   (f2cl-lib:fref x (1 1) ((1 2) (1 2))))
                           (setf (f2cl-lib:fref work-%data%
                                                ((f2cl-lib:int-add j n2))
                                                ((1 *))
                                                work-%offset%)
                                   (f2cl-lib:fref x (1 2) ((1 2) (1 2))))
                           (setf vmax
                                   (max
                                    (abs
                                     (f2cl-lib:fref work-%data%
                                                    ((f2cl-lib:int-add j n))
                                                    ((1 *))
                                                    work-%offset%))
                                    (abs
                                     (f2cl-lib:fref work-%data%
                                                    ((f2cl-lib:int-add j n2))
                                                    ((1 *))
                                                    work-%offset%))
                                    vmax))
                           (setf vcrit (/ bignum vmax)))
                          (t
                           (setf beta
                                   (max
                                    (f2cl-lib:fref work-%data%
                                                   (j)
                                                   ((1 *))
                                                   work-%offset%)
                                    (f2cl-lib:fref work-%data%
                                                   ((f2cl-lib:int-add j 1))
                                                   ((1 *))
                                                   work-%offset%)))
                           (cond
                             ((> beta vcrit)
                              (setf rec (/ one vmax))
                              (dscal
                               (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1) rec
                               (f2cl-lib:array-slice work-%data%
                                                     double-float
                                                     ((+ ki n))
                                                     ((1 *))
                                                     work-%offset%)
                               1)
                              (dscal
                               (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1) rec
                               (f2cl-lib:array-slice work-%data%
                                                     double-float
                                                     ((+ ki n2))
                                                     ((1 *))
                                                     work-%offset%)
                               1)
                              (setf vmax one)
                              (setf vcrit bignum)))
                           (setf (f2cl-lib:fref work-%data%
                                                ((f2cl-lib:int-add j n))
                                                ((1 *))
                                                work-%offset%)
                                   (-
                                    (f2cl-lib:fref work-%data%
                                                   ((f2cl-lib:int-add j n))
                                                   ((1 *))
                                                   work-%offset%)
                                    (ddot (f2cl-lib:int-sub j ki 2)
                                     (f2cl-lib:array-slice t$-%data%
                                                           double-float
                                                           ((+ ki 2) j)
                                                           ((1 ldt) (1 *))
                                                           t$-%offset%)
                                     1
                                     (f2cl-lib:array-slice work-%data%
                                                           double-float
                                                           ((+ ki 2 n))
                                                           ((1 *))
                                                           work-%offset%)
                                     1)))
                           (setf (f2cl-lib:fref work-%data%
                                                ((f2cl-lib:int-add j n2))
                                                ((1 *))
                                                work-%offset%)
                                   (-
                                    (f2cl-lib:fref work-%data%
                                                   ((f2cl-lib:int-add j n2))
                                                   ((1 *))
                                                   work-%offset%)
                                    (ddot (f2cl-lib:int-sub j ki 2)
                                     (f2cl-lib:array-slice t$-%data%
                                                           double-float
                                                           ((+ ki 2) j)
                                                           ((1 ldt) (1 *))
                                                           t$-%offset%)
                                     1
                                     (f2cl-lib:array-slice work-%data%
                                                           double-float
                                                           ((+ ki 2 n2))
                                                           ((1 *))
                                                           work-%offset%)
                                     1)))
                           (setf (f2cl-lib:fref work-%data%
                                                ((f2cl-lib:int-add j 1 n))
                                                ((1 *))
                                                work-%offset%)
                                   (-
                                    (f2cl-lib:fref work-%data%
                                                   ((f2cl-lib:int-add j 1 n))
                                                   ((1 *))
                                                   work-%offset%)
                                    (ddot (f2cl-lib:int-sub j ki 2)
                                     (f2cl-lib:array-slice t$-%data%
                                                           double-float
                                                           ((+ ki 2)
                                                            (f2cl-lib:int-add j
                                                                              1))
                                                           ((1 ldt) (1 *))
                                                           t$-%offset%)
                                     1
                                     (f2cl-lib:array-slice work-%data%
                                                           double-float
                                                           ((+ ki 2 n))
                                                           ((1 *))
                                                           work-%offset%)
                                     1)))
                           (setf (f2cl-lib:fref work-%data%
                                                ((f2cl-lib:int-add j 1 n2))
                                                ((1 *))
                                                work-%offset%)
                                   (-
                                    (f2cl-lib:fref work-%data%
                                                   ((f2cl-lib:int-add j 1 n2))
                                                   ((1 *))
                                                   work-%offset%)
                                    (ddot (f2cl-lib:int-sub j ki 2)
                                     (f2cl-lib:array-slice t$-%data%
                                                           double-float
                                                           ((+ ki 2)
                                                            (f2cl-lib:int-add j
                                                                              1))
                                                           ((1 ldt) (1 *))
                                                           t$-%offset%)
                                     1
                                     (f2cl-lib:array-slice work-%data%
                                                           double-float
                                                           ((+ ki 2 n2))
                                                           ((1 *))
                                                           work-%offset%)
                                     1)))
                           (multiple-value-bind
                                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                  var-7 var-8 var-9 var-10 var-11 var-12 var-13
                                  var-14 var-15 var-16 var-17)
                               (dlaln2 f2cl-lib:%true% 2 2 smin one
                                (f2cl-lib:array-slice t$-%data%
                                                      double-float
                                                      (j j)
                                                      ((1 ldt) (1 *))
                                                      t$-%offset%)
                                ldt one one
                                (f2cl-lib:array-slice work-%data%
                                                      double-float
                                                      ((+ j n))
                                                      ((1 *))
                                                      work-%offset%)
                                n wr (- wi) x 2 scale xnorm ierr)
                             (declare (ignore var-0 var-1 var-2 var-3 var-4
                                              var-5 var-6 var-7 var-8 var-9
                                              var-10 var-11 var-12 var-13
                                              var-14))
                             (setf scale var-15)
                             (setf xnorm var-16)
                             (setf ierr var-17))
                           (cond
                             ((/= scale one)
                              (dscal
                               (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1)
                               scale
                               (f2cl-lib:array-slice work-%data%
                                                     double-float
                                                     ((+ ki n))
                                                     ((1 *))
                                                     work-%offset%)
                               1)
                              (dscal
                               (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1)
                               scale
                               (f2cl-lib:array-slice work-%data%
                                                     double-float
                                                     ((+ ki n2))
                                                     ((1 *))
                                                     work-%offset%)
                               1)))
                           (setf (f2cl-lib:fref work-%data%
                                                ((f2cl-lib:int-add j n))
                                                ((1 *))
                                                work-%offset%)
                                   (f2cl-lib:fref x (1 1) ((1 2) (1 2))))
                           (setf (f2cl-lib:fref work-%data%
                                                ((f2cl-lib:int-add j n2))
                                                ((1 *))
                                                work-%offset%)
                                   (f2cl-lib:fref x (1 2) ((1 2) (1 2))))
                           (setf (f2cl-lib:fref work-%data%
                                                ((f2cl-lib:int-add j 1 n))
                                                ((1 *))
                                                work-%offset%)
                                   (f2cl-lib:fref x (2 1) ((1 2) (1 2))))
                           (setf (f2cl-lib:fref work-%data%
                                                ((f2cl-lib:int-add j 1 n2))
                                                ((1 *))
                                                work-%offset%)
                                   (f2cl-lib:fref x (2 2) ((1 2) (1 2))))
                           (setf vmax
                                   (max
                                    (abs (f2cl-lib:fref x (1 1) ((1 2) (1 2))))
                                    (abs (f2cl-lib:fref x (1 2) ((1 2) (1 2))))
                                    (abs (f2cl-lib:fref x (2 1) ((1 2) (1 2))))
                                    (abs (f2cl-lib:fref x (2 2) ((1 2) (1 2))))
                                    vmax))
                           (setf vcrit (/ bignum vmax))))
                       label200))
                   label210
                    (cond
                      ((not over)
                       (dcopy (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1)
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              ((+ ki n))
                                              ((1 *))
                                              work-%offset%)
                        1
                        (f2cl-lib:array-slice vl-%data%
                                              double-float
                                              (ki is)
                                              ((1 ldvl) (1 *))
                                              vl-%offset%)
                        1)
                       (dcopy (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1)
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              ((+ ki n2))
                                              ((1 *))
                                              work-%offset%)
                        1
                        (f2cl-lib:array-slice vl-%data%
                                              double-float
                                              (ki (f2cl-lib:int-add is 1))
                                              ((1 ldvl) (1 *))
                                              vl-%offset%)
                        1)
                       (setf emax zero)
                       (f2cl-lib:fdo (k ki (f2cl-lib:int-add k 1))
                                     ((> k n) nil)
                         (tagbody
                           (setf emax
                                   (max emax
                                        (+
                                         (abs
                                          (f2cl-lib:fref vl-%data%
                                                         (k is)
                                                         ((1 ldvl) (1 *))
                                                         vl-%offset%))
                                         (abs
                                          (f2cl-lib:fref vl-%data%
                                                         (k
                                                          (f2cl-lib:int-add is
                                                                            1))
                                                         ((1 ldvl) (1 *))
                                                         vl-%offset%)))))
                          label220))
                       (setf remax (/ one emax))
                       (dscal (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1)
                        remax
                        (f2cl-lib:array-slice vl-%data%
                                              double-float
                                              (ki is)
                                              ((1 ldvl) (1 *))
                                              vl-%offset%)
                        1)
                       (dscal (f2cl-lib:int-add (f2cl-lib:int-sub n ki) 1)
                        remax
                        (f2cl-lib:array-slice vl-%data%
                                              double-float
                                              (ki (f2cl-lib:int-add is 1))
                                              ((1 ldvl) (1 *))
                                              vl-%offset%)
                        1)
                       (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                     ((> k
                                         (f2cl-lib:int-add ki
                                                           (f2cl-lib:int-sub
                                                            1)))
                                      nil)
                         (tagbody
                           (setf (f2cl-lib:fref vl-%data%
                                                (k is)
                                                ((1 ldvl) (1 *))
                                                vl-%offset%)
                                   zero)
                           (setf (f2cl-lib:fref vl-%data%
                                                (k (f2cl-lib:int-add is 1))
                                                ((1 ldvl) (1 *))
                                                vl-%offset%)
                                   zero)
                          label230)))
                      (t
                       (cond
                         ((< ki (f2cl-lib:int-add n (f2cl-lib:int-sub 1)))
                          (dgemv "N" n (f2cl-lib:int-sub n ki 1) one
                           (f2cl-lib:array-slice vl-%data%
                                                 double-float
                                                 (1 (f2cl-lib:int-add ki 2))
                                                 ((1 ldvl) (1 *))
                                                 vl-%offset%)
                           ldvl
                           (f2cl-lib:array-slice work-%data%
                                                 double-float
                                                 ((+ ki 2 n))
                                                 ((1 *))
                                                 work-%offset%)
                           1
                           (f2cl-lib:fref work-%data%
                                          ((f2cl-lib:int-add ki n))
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:array-slice vl-%data%
                                                 double-float
                                                 (1 ki)
                                                 ((1 ldvl) (1 *))
                                                 vl-%offset%)
                           1)
                          (dgemv "N" n (f2cl-lib:int-sub n ki 1) one
                           (f2cl-lib:array-slice vl-%data%
                                                 double-float
                                                 (1 (f2cl-lib:int-add ki 2))
                                                 ((1 ldvl) (1 *))
                                                 vl-%offset%)
                           ldvl
                           (f2cl-lib:array-slice work-%data%
                                                 double-float
                                                 ((+ ki 2 n2))
                                                 ((1 *))
                                                 work-%offset%)
                           1
                           (f2cl-lib:fref work-%data%
                                          ((f2cl-lib:int-add ki 1 n2))
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:array-slice vl-%data%
                                                 double-float
                                                 (1 (f2cl-lib:int-add ki 1))
                                                 ((1 ldvl) (1 *))
                                                 vl-%offset%)
                           1))
                         (t
                          (dscal n
                           (f2cl-lib:fref work-%data%
                                          ((f2cl-lib:int-add ki n))
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:array-slice vl-%data%
                                                 double-float
                                                 (1 ki)
                                                 ((1 ldvl) (1 *))
                                                 vl-%offset%)
                           1)
                          (dscal n
                           (f2cl-lib:fref work-%data%
                                          ((f2cl-lib:int-add ki 1 n2))
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:array-slice vl-%data%
                                                 double-float
                                                 (1 (f2cl-lib:int-add ki 1))
                                                 ((1 ldvl) (1 *))
                                                 vl-%offset%)
                           1)))
                       (setf emax zero)
                       (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                     ((> k n) nil)
                         (tagbody
                           (setf emax
                                   (max emax
                                        (+
                                         (abs
                                          (f2cl-lib:fref vl-%data%
                                                         (k ki)
                                                         ((1 ldvl) (1 *))
                                                         vl-%offset%))
                                         (abs
                                          (f2cl-lib:fref vl-%data%
                                                         (k
                                                          (f2cl-lib:int-add ki
                                                                            1))
                                                         ((1 ldvl) (1 *))
                                                         vl-%offset%)))))
                          label240))
                       (setf remax (/ one emax))
                       (dscal n remax
                        (f2cl-lib:array-slice vl-%data%
                                              double-float
                                              (1 ki)
                                              ((1 ldvl) (1 *))
                                              vl-%offset%)
                        1)
                       (dscal n remax
                        (f2cl-lib:array-slice vl-%data%
                                              double-float
                                              (1 (f2cl-lib:int-add ki 1))
                                              ((1 ldvl) (1 *))
                                              vl-%offset%)
                        1))))))
               (setf is (f2cl-lib:int-add is 1))
               (if (/= ip 0) (setf is (f2cl-lib:int-add is 1)))
              label250
               (if (= ip -1) (setf ip 0))
               (if (= ip 1) (setf ip -1))
              label260))))
        (go end_label)
       end_label
        (return
         (values nil nil nil nil nil nil nil nil nil nil nil m nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dtrevc
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string)
                        (array fortran-to-lisp::logical (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::m nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::ddot fortran-to-lisp::dgemv
                    fortran-to-lisp::idamax fortran-to-lisp::dcopy
                    fortran-to-lisp::daxpy fortran-to-lisp::dscal
                    fortran-to-lisp::dlaln2 fortran-to-lisp::dlabad
                    fortran-to-lisp::dlamch fortran-to-lisp::xerbla
                    fortran-to-lisp::lsame))))

