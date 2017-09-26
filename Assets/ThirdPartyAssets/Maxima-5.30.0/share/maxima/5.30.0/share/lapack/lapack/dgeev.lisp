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
  (defun dgeev (jobvl jobvr n a lda wr wi vl ldvl vr ldvr work lwork info)
    (declare (type (array double-float (*)) work vr vl wi wr a)
             (type (f2cl-lib:integer4) info lwork ldvr ldvl lda n)
             (type (simple-string *) jobvr jobvl))
    (f2cl-lib:with-multi-array-data
        ((jobvl character jobvl-%data% jobvl-%offset%)
         (jobvr character jobvr-%data% jobvr-%offset%)
         (a double-float a-%data% a-%offset%)
         (wr double-float wr-%data% wr-%offset%)
         (wi double-float wi-%data% wi-%offset%)
         (vl double-float vl-%data% vl-%offset%)
         (vr double-float vr-%data% vr-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((dum (make-array 1 :element-type 'double-float))
             (select (make-array 1 :element-type 't)) (anrm 0.0) (bignum 0.0)
             (cs 0.0) (cscale 0.0) (eps 0.0) (r 0.0) (scl 0.0) (smlnum 0.0)
             (sn 0.0) (hswork 0) (i 0) (ibal 0) (ierr 0) (ihi 0) (ilo 0)
             (itau 0) (iwrk 0) (k 0) (maxb 0) (maxwrk 0) (minwrk 0) (nout 0)
             (side
              (make-array '(1) :element-type 'character :initial-element #\ ))
             (lquery nil) (scalea nil) (wantvl nil) (wantvr nil))
        (declare (type (array double-float (1)) dum)
                 (type (array f2cl-lib:logical (1)) select)
                 (type (double-float) anrm bignum cs cscale eps r scl smlnum
                                      sn)
                 (type (f2cl-lib:integer4) hswork i ibal ierr ihi ilo itau iwrk
                                           k maxb maxwrk minwrk nout)
                 (type (simple-string 1) side)
                 (type f2cl-lib:logical lquery scalea wantvl wantvr))
        (setf info 0)
        (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
        (setf wantvl (lsame jobvl "V"))
        (setf wantvr (lsame jobvr "V"))
        (cond
          ((and (not wantvl) (not (lsame jobvl "N")))
           (setf info -1))
          ((and (not wantvr) (not (lsame jobvr "N")))
           (setf info -2))
          ((< n 0)
           (setf info -3))
          ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
           (setf info -5))
          ((or (< ldvl 1) (and wantvl (< ldvl n)))
           (setf info -9))
          ((or (< ldvr 1) (and wantvr (< ldvr n)))
           (setf info -11)))
        (setf minwrk 1)
        (cond
          ((and (= info 0) (or (>= lwork 1) lquery))
           (setf maxwrk
                   (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                     (f2cl-lib:int-mul n
                                                       (ilaenv 1 "DGEHRD" " " n
                                                        1 n 0))))
           (cond
             ((and (not wantvl) (not wantvr))
              (setf minwrk
                      (max (the f2cl-lib:integer4 1)
                           (the f2cl-lib:integer4 (f2cl-lib:int-mul 3 n))))
              (setf maxb
                      (max
                       (the f2cl-lib:integer4
                            (ilaenv 8 "DHSEQR" "EN" n 1 n -1))
                       (the f2cl-lib:integer4 2)))
              (setf k
                      (min (the f2cl-lib:integer4 maxb)
                           (the f2cl-lib:integer4 n)
                           (the f2cl-lib:integer4
                                (max (the f2cl-lib:integer4 2)
                                     (the f2cl-lib:integer4
                                          (ilaenv 4 "DHSEQR" "EN" n 1 n -1))))))
              (setf hswork
                      (max
                       (the f2cl-lib:integer4
                            (f2cl-lib:int-mul k (f2cl-lib:int-add k 2)))
                       (the f2cl-lib:integer4 (f2cl-lib:int-mul 2 n))))
              (setf maxwrk
                      (max (the f2cl-lib:integer4 maxwrk)
                           (the f2cl-lib:integer4 (f2cl-lib:int-add n 1))
                           (the f2cl-lib:integer4
                                (f2cl-lib:int-add n hswork)))))
             (t
              (setf minwrk
                      (max (the f2cl-lib:integer4 1)
                           (the f2cl-lib:integer4 (f2cl-lib:int-mul 4 n))))
              (setf maxwrk
                      (max (the f2cl-lib:integer4 maxwrk)
                           (the f2cl-lib:integer4
                                (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                  (f2cl-lib:int-mul
                                                   (f2cl-lib:int-sub n 1)
                                                   (ilaenv 1 "DORGHR" " " n 1 n
                                                    -1))))))
              (setf maxb
                      (max
                       (the f2cl-lib:integer4
                            (ilaenv 8 "DHSEQR" "SV" n 1 n -1))
                       (the f2cl-lib:integer4 2)))
              (setf k
                      (min (the f2cl-lib:integer4 maxb)
                           (the f2cl-lib:integer4 n)
                           (the f2cl-lib:integer4
                                (max (the f2cl-lib:integer4 2)
                                     (the f2cl-lib:integer4
                                          (ilaenv 4 "DHSEQR" "SV" n 1 n -1))))))
              (setf hswork
                      (max
                       (the f2cl-lib:integer4
                            (f2cl-lib:int-mul k (f2cl-lib:int-add k 2)))
                       (the f2cl-lib:integer4 (f2cl-lib:int-mul 2 n))))
              (setf maxwrk
                      (max (the f2cl-lib:integer4 maxwrk)
                           (the f2cl-lib:integer4 (f2cl-lib:int-add n 1))
                           (the f2cl-lib:integer4 (f2cl-lib:int-add n hswork))))
              (setf maxwrk
                      (max (the f2cl-lib:integer4 maxwrk)
                           (the f2cl-lib:integer4 (f2cl-lib:int-mul 4 n))))))
           (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                   (coerce (the f2cl-lib:integer4 maxwrk) 'double-float))))
        (cond
          ((and (< lwork minwrk) (not lquery))
           (setf info -13)))
        (cond
          ((/= info 0)
           (xerbla "DGEEV " (f2cl-lib:int-sub info))
           (go end_label))
          (lquery
           (go end_label)))
        (if (= n 0) (go end_label))
        (setf eps (dlamch "P"))
        (setf smlnum (dlamch "S"))
        (setf bignum (/ one smlnum))
        (multiple-value-bind (var-0 var-1)
            (dlabad smlnum bignum)
          (declare (ignore))
          (setf smlnum var-0)
          (setf bignum var-1))
        (setf smlnum (/ (f2cl-lib:fsqrt smlnum) eps))
        (setf bignum (/ one smlnum))
        (setf anrm (dlange "M" n n a lda dum))
        (setf scalea f2cl-lib:%false%)
        (cond
          ((and (> anrm zero) (< anrm smlnum))
           (setf scalea f2cl-lib:%true%)
           (setf cscale smlnum))
          ((> anrm bignum)
           (setf scalea f2cl-lib:%true%)
           (setf cscale bignum)))
        (if scalea
            (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                (dlascl "G" 0 0 anrm cscale n n a lda ierr)
              (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                               var-8))
              (setf ierr var-9)))
        (setf ibal 1)
        (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
            (dgebal "B" n a lda ilo ihi
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (ibal)
                                   ((1 *))
                                   work-%offset%)
             ierr)
          (declare (ignore var-0 var-1 var-2 var-3 var-6))
          (setf ilo var-4)
          (setf ihi var-5)
          (setf ierr var-7))
        (setf itau (f2cl-lib:int-add ibal n))
        (setf iwrk (f2cl-lib:int-add itau n))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
            (dgehrd n ilo ihi a lda
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (itau)
                                   ((1 *))
                                   work-%offset%)
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (iwrk)
                                   ((1 *))
                                   work-%offset%)
             (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwrk) 1) ierr)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
          (setf ierr var-8))
        (cond
          (wantvl
           (f2cl-lib:f2cl-set-string side "L" (string 1))
           (dlacpy "L" n n a lda vl ldvl)
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
               (dorghr n ilo ihi vl ldvl
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (itau)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (iwrk)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwrk) 1) ierr)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
             (setf ierr var-8))
           (setf iwrk itau)
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                  var-10 var-11 var-12 var-13)
               (dhseqr "S" "V" n ilo ihi a lda wr wi vl ldvl
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (iwrk)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwrk) 1) info)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12))
             (setf info var-13))
           (cond
             (wantvr
              (f2cl-lib:f2cl-set-string side "B" (string 1))
              (dlacpy "F" n n vl ldvl vr ldvr))))
          (wantvr
           (f2cl-lib:f2cl-set-string side "R" (string 1))
           (dlacpy "L" n n a lda vr ldvr)
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
               (dorghr n ilo ihi vr ldvr
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (itau)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (iwrk)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwrk) 1) ierr)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
             (setf ierr var-8))
           (setf iwrk itau)
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                  var-10 var-11 var-12 var-13)
               (dhseqr "S" "V" n ilo ihi a lda wr wi vr ldvr
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (iwrk)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwrk) 1) info)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12))
             (setf info var-13)))
          (t
           (setf iwrk itau)
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                  var-10 var-11 var-12 var-13)
               (dhseqr "E" "N" n ilo ihi a lda wr wi vr ldvr
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (iwrk)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwrk) 1) info)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12))
             (setf info var-13))))
        (if (> info 0) (go label50))
        (cond
          ((or wantvl wantvr)
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                  var-10 var-11 var-12 var-13)
               (dtrevc side "B" select n a lda vl ldvl vr ldvr n nout
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (iwrk)
                                      ((1 *))
                                      work-%offset%)
                ierr)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-12))
             (setf nout var-11)
             (setf ierr var-13))))
        (cond
          (wantvl
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
               (dgebak "B" "L" n ilo ihi
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (ibal)
                                      ((1 *))
                                      work-%offset%)
                n vl ldvl ierr)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8))
             (setf ierr var-9))
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i n) nil)
             (tagbody
               (cond
                 ((= (f2cl-lib:fref wi (i) ((1 *))) zero)
                  (setf scl
                          (/ one
                             (dnrm2 n
                              (f2cl-lib:array-slice vl-%data%
                                                    double-float
                                                    (1 i)
                                                    ((1 ldvl) (1 *))
                                                    vl-%offset%)
                              1)))
                  (dscal n scl
                   (f2cl-lib:array-slice vl-%data%
                                         double-float
                                         (1 i)
                                         ((1 ldvl) (1 *))
                                         vl-%offset%)
                   1))
                 ((> (f2cl-lib:fref wi (i) ((1 *))) zero)
                  (setf scl
                          (/ one
                             (dlapy2
                              (dnrm2 n
                               (f2cl-lib:array-slice vl-%data%
                                                     double-float
                                                     (1 i)
                                                     ((1 ldvl) (1 *))
                                                     vl-%offset%)
                               1)
                              (dnrm2 n
                               (f2cl-lib:array-slice vl-%data%
                                                     double-float
                                                     (1 (f2cl-lib:int-add i 1))
                                                     ((1 ldvl) (1 *))
                                                     vl-%offset%)
                               1))))
                  (dscal n scl
                   (f2cl-lib:array-slice vl-%data%
                                         double-float
                                         (1 i)
                                         ((1 ldvl) (1 *))
                                         vl-%offset%)
                   1)
                  (dscal n scl
                   (f2cl-lib:array-slice vl-%data%
                                         double-float
                                         (1 (f2cl-lib:int-add i 1))
                                         ((1 ldvl) (1 *))
                                         vl-%offset%)
                   1)
                  (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                ((> k n) nil)
                    (tagbody
                      (setf (f2cl-lib:fref work-%data%
                                           ((f2cl-lib:int-sub
                                             (f2cl-lib:int-add iwrk k)
                                             1))
                                           ((1 *))
                                           work-%offset%)
                              (+
                               (expt
                                (f2cl-lib:fref vl-%data%
                                               (k i)
                                               ((1 ldvl) (1 *))
                                               vl-%offset%)
                                2)
                               (expt
                                (f2cl-lib:fref vl-%data%
                                               (k (f2cl-lib:int-add i 1))
                                               ((1 ldvl) (1 *))
                                               vl-%offset%)
                                2)))
                     label10))
                  (setf k
                          (idamax n
                           (f2cl-lib:array-slice work-%data%
                                                 double-float
                                                 (iwrk)
                                                 ((1 *))
                                                 work-%offset%)
                           1))
                  (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                      (dlartg
                       (f2cl-lib:fref vl-%data%
                                      (k i)
                                      ((1 ldvl) (1 *))
                                      vl-%offset%)
                       (f2cl-lib:fref vl-%data%
                                      (k (f2cl-lib:int-add i 1))
                                      ((1 ldvl) (1 *))
                                      vl-%offset%)
                       cs sn r)
                    (declare (ignore var-0 var-1))
                    (setf cs var-2)
                    (setf sn var-3)
                    (setf r var-4))
                  (drot n
                   (f2cl-lib:array-slice vl-%data%
                                         double-float
                                         (1 i)
                                         ((1 ldvl) (1 *))
                                         vl-%offset%)
                   1
                   (f2cl-lib:array-slice vl-%data%
                                         double-float
                                         (1 (f2cl-lib:int-add i 1))
                                         ((1 ldvl) (1 *))
                                         vl-%offset%)
                   1 cs sn)
                  (setf (f2cl-lib:fref vl-%data%
                                       (k (f2cl-lib:int-add i 1))
                                       ((1 ldvl) (1 *))
                                       vl-%offset%)
                          zero)))
              label20))))
        (cond
          (wantvr
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
               (dgebak "B" "R" n ilo ihi
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (ibal)
                                      ((1 *))
                                      work-%offset%)
                n vr ldvr ierr)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8))
             (setf ierr var-9))
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i n) nil)
             (tagbody
               (cond
                 ((= (f2cl-lib:fref wi (i) ((1 *))) zero)
                  (setf scl
                          (/ one
                             (dnrm2 n
                              (f2cl-lib:array-slice vr-%data%
                                                    double-float
                                                    (1 i)
                                                    ((1 ldvr) (1 *))
                                                    vr-%offset%)
                              1)))
                  (dscal n scl
                   (f2cl-lib:array-slice vr-%data%
                                         double-float
                                         (1 i)
                                         ((1 ldvr) (1 *))
                                         vr-%offset%)
                   1))
                 ((> (f2cl-lib:fref wi (i) ((1 *))) zero)
                  (setf scl
                          (/ one
                             (dlapy2
                              (dnrm2 n
                               (f2cl-lib:array-slice vr-%data%
                                                     double-float
                                                     (1 i)
                                                     ((1 ldvr) (1 *))
                                                     vr-%offset%)
                               1)
                              (dnrm2 n
                               (f2cl-lib:array-slice vr-%data%
                                                     double-float
                                                     (1 (f2cl-lib:int-add i 1))
                                                     ((1 ldvr) (1 *))
                                                     vr-%offset%)
                               1))))
                  (dscal n scl
                   (f2cl-lib:array-slice vr-%data%
                                         double-float
                                         (1 i)
                                         ((1 ldvr) (1 *))
                                         vr-%offset%)
                   1)
                  (dscal n scl
                   (f2cl-lib:array-slice vr-%data%
                                         double-float
                                         (1 (f2cl-lib:int-add i 1))
                                         ((1 ldvr) (1 *))
                                         vr-%offset%)
                   1)
                  (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                ((> k n) nil)
                    (tagbody
                      (setf (f2cl-lib:fref work-%data%
                                           ((f2cl-lib:int-sub
                                             (f2cl-lib:int-add iwrk k)
                                             1))
                                           ((1 *))
                                           work-%offset%)
                              (+
                               (expt
                                (f2cl-lib:fref vr-%data%
                                               (k i)
                                               ((1 ldvr) (1 *))
                                               vr-%offset%)
                                2)
                               (expt
                                (f2cl-lib:fref vr-%data%
                                               (k (f2cl-lib:int-add i 1))
                                               ((1 ldvr) (1 *))
                                               vr-%offset%)
                                2)))
                     label30))
                  (setf k
                          (idamax n
                           (f2cl-lib:array-slice work-%data%
                                                 double-float
                                                 (iwrk)
                                                 ((1 *))
                                                 work-%offset%)
                           1))
                  (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                      (dlartg
                       (f2cl-lib:fref vr-%data%
                                      (k i)
                                      ((1 ldvr) (1 *))
                                      vr-%offset%)
                       (f2cl-lib:fref vr-%data%
                                      (k (f2cl-lib:int-add i 1))
                                      ((1 ldvr) (1 *))
                                      vr-%offset%)
                       cs sn r)
                    (declare (ignore var-0 var-1))
                    (setf cs var-2)
                    (setf sn var-3)
                    (setf r var-4))
                  (drot n
                   (f2cl-lib:array-slice vr-%data%
                                         double-float
                                         (1 i)
                                         ((1 ldvr) (1 *))
                                         vr-%offset%)
                   1
                   (f2cl-lib:array-slice vr-%data%
                                         double-float
                                         (1 (f2cl-lib:int-add i 1))
                                         ((1 ldvr) (1 *))
                                         vr-%offset%)
                   1 cs sn)
                  (setf (f2cl-lib:fref vr-%data%
                                       (k (f2cl-lib:int-add i 1))
                                       ((1 ldvr) (1 *))
                                       vr-%offset%)
                          zero)))
              label40))))
       label50
        (cond
          (scalea
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
               (dlascl "G" 0 0 cscale anrm (f2cl-lib:int-sub n info) 1
                (f2cl-lib:array-slice wr-%data%
                                      double-float
                                      ((+ info 1))
                                      ((1 *))
                                      wr-%offset%)
                (max (the f2cl-lib:integer4 (f2cl-lib:int-sub n info))
                     (the f2cl-lib:integer4 1))
                ierr)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8))
             (setf ierr var-9))
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
               (dlascl "G" 0 0 cscale anrm (f2cl-lib:int-sub n info) 1
                (f2cl-lib:array-slice wi-%data%
                                      double-float
                                      ((+ info 1))
                                      ((1 *))
                                      wi-%offset%)
                (max (the f2cl-lib:integer4 (f2cl-lib:int-sub n info))
                     (the f2cl-lib:integer4 1))
                ierr)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8))
             (setf ierr var-9))
           (cond
             ((> info 0)
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                  (dlascl "G" 0 0 cscale anrm (f2cl-lib:int-sub ilo 1) 1 wr n
                   ierr)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7 var-8))
                (setf ierr var-9))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                  (dlascl "G" 0 0 cscale anrm (f2cl-lib:int-sub ilo 1) 1 wi n
                   ierr)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7 var-8))
                (setf ierr var-9))))))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce (the f2cl-lib:integer4 maxwrk) 'double-float))
        (go end_label)
       end_label
        (return
         (values nil nil nil nil nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgeev fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::drot fortran-to-lisp::dlartg
                    fortran-to-lisp::idamax fortran-to-lisp::dlapy2
                    fortran-to-lisp::dscal fortran-to-lisp::dnrm2
                    fortran-to-lisp::dgebak fortran-to-lisp::dtrevc
                    fortran-to-lisp::dhseqr fortran-to-lisp::dorghr
                    fortran-to-lisp::dlacpy fortran-to-lisp::dgehrd
                    fortran-to-lisp::dgebal fortran-to-lisp::dlascl
                    fortran-to-lisp::dlange fortran-to-lisp::dlabad
                    fortran-to-lisp::dlamch fortran-to-lisp::xerbla
                    fortran-to-lisp::ilaenv fortran-to-lisp::lsame))))

