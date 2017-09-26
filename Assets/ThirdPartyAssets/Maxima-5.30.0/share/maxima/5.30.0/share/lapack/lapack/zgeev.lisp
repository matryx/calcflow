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
  (defun zgeev (jobvl jobvr n a lda w vl ldvl vr ldvr work lwork rwork info)
    (declare (type (array double-float (*)) rwork)
             (type (array f2cl-lib:complex16 (*)) work vr vl w a)
             (type (f2cl-lib:integer4) info lwork ldvr ldvl lda n)
             (type (simple-string *) jobvr jobvl))
    (f2cl-lib:with-multi-array-data
        ((jobvl character jobvl-%data% jobvl-%offset%)
         (jobvr character jobvr-%data% jobvr-%offset%)
         (a f2cl-lib:complex16 a-%data% a-%offset%)
         (w f2cl-lib:complex16 w-%data% w-%offset%)
         (vl f2cl-lib:complex16 vl-%data% vl-%offset%)
         (vr f2cl-lib:complex16 vr-%data% vr-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%)
         (rwork double-float rwork-%data% rwork-%offset%))
      (prog ((dum (make-array 1 :element-type 'double-float))
             (select (make-array 1 :element-type 't)) (tmp #C(0.0 0.0))
             (anrm 0.0) (bignum 0.0) (cscale 0.0) (eps 0.0) (scl 0.0)
             (smlnum 0.0) (hswork 0) (i 0) (ibal 0) (ierr 0) (ihi 0) (ilo 0)
             (irwork 0) (itau 0) (iwrk 0) (k 0) (maxwrk 0) (minwrk 0) (nout 0)
             (side
              (make-array '(1) :element-type 'character :initial-element #\ ))
             (lquery nil) (scalea nil) (wantvl nil) (wantvr nil))
        (declare (type (array double-float (1)) dum)
                 (type (array f2cl-lib:logical (1)) select)
                 (type (f2cl-lib:complex16) tmp)
                 (type (double-float) anrm bignum cscale eps scl smlnum)
                 (type (f2cl-lib:integer4) hswork i ibal ierr ihi ilo irwork
                                           itau iwrk k maxwrk minwrk nout)
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
           (setf info -8))
          ((or (< ldvr 1) (and wantvr (< ldvr n)))
           (setf info -10)))
        (cond
          ((= info 0)
           (cond
             ((= n 0)
              (setf minwrk 1)
              (setf maxwrk 1))
             (t
              (setf maxwrk
                      (f2cl-lib:int-add n
                                        (f2cl-lib:int-mul n
                                                          (ilaenv 1 "ZGEHRD"
                                                           " " n 1 n 0))))
              (setf minwrk (f2cl-lib:int-mul 2 n))
              (cond
                (wantvl
                 (setf maxwrk
                         (max (the f2cl-lib:integer4 maxwrk)
                              (the f2cl-lib:integer4
                                   (f2cl-lib:int-add n
                                                     (f2cl-lib:int-mul
                                                      (f2cl-lib:int-sub n 1)
                                                      (ilaenv 1 "ZUNGHR" " " n
                                                       1 n -1))))))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12)
                     (zhseqr "S" "V" n 1 n a lda w vl ldvl work -1 info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11))
                   (setf info var-12)))
                (wantvr
                 (setf maxwrk
                         (max (the f2cl-lib:integer4 maxwrk)
                              (the f2cl-lib:integer4
                                   (f2cl-lib:int-add n
                                                     (f2cl-lib:int-mul
                                                      (f2cl-lib:int-sub n 1)
                                                      (ilaenv 1 "ZUNGHR" " " n
                                                       1 n -1))))))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12)
                     (zhseqr "S" "V" n 1 n a lda w vr ldvr work -1 info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11))
                   (setf info var-12)))
                (t
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12)
                     (zhseqr "E" "N" n 1 n a lda w vr ldvr work -1 info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11))
                   (setf info var-12))))
              (setf hswork
                      (f2cl-lib:int
                       (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)))
              (setf maxwrk
                      (max (the f2cl-lib:integer4 maxwrk)
                           (the f2cl-lib:integer4 hswork)
                           (the f2cl-lib:integer4 minwrk)))))
           (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                   (coerce maxwrk 'f2cl-lib:complex16))
           (cond
             ((and (< lwork minwrk) (not lquery))
              (setf info -12)))))
        (cond
          ((/= info 0)
           (xerbla "ZGEEV " (f2cl-lib:int-sub info))
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
        (setf anrm (zlange "M" n n a lda dum))
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
                (zlascl "G" 0 0 anrm cscale n n a lda ierr)
              (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                               var-8))
              (setf ierr var-9)))
        (setf ibal 1)
        (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
            (zgebal "B" n a lda ilo ihi
             (f2cl-lib:array-slice rwork-%data%
                                   double-float
                                   (ibal)
                                   ((1 *))
                                   rwork-%offset%)
             ierr)
          (declare (ignore var-0 var-1 var-2 var-3 var-6))
          (setf ilo var-4)
          (setf ihi var-5)
          (setf ierr var-7))
        (setf itau 1)
        (setf iwrk (f2cl-lib:int-add itau n))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
            (zgehrd n ilo ihi a lda
             (f2cl-lib:array-slice work-%data%
                                   f2cl-lib:complex16
                                   (itau)
                                   ((1 *))
                                   work-%offset%)
             (f2cl-lib:array-slice work-%data%
                                   f2cl-lib:complex16
                                   (iwrk)
                                   ((1 *))
                                   work-%offset%)
             (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwrk) 1) ierr)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
          (setf ierr var-8))
        (cond
          (wantvl
           (f2cl-lib:f2cl-set-string side "L" (string 1))
           (zlacpy "L" n n a lda vl ldvl)
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
               (zunghr n ilo ihi vl ldvl
                (f2cl-lib:array-slice work-%data%
                                      f2cl-lib:complex16
                                      (itau)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:array-slice work-%data%
                                      f2cl-lib:complex16
                                      (iwrk)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwrk) 1) ierr)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
             (setf ierr var-8))
           (setf iwrk itau)
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                  var-10 var-11 var-12)
               (zhseqr "S" "V" n ilo ihi a lda w vl ldvl
                (f2cl-lib:array-slice work-%data%
                                      f2cl-lib:complex16
                                      (iwrk)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwrk) 1) info)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11))
             (setf info var-12))
           (cond
             (wantvr
              (f2cl-lib:f2cl-set-string side "B" (string 1))
              (zlacpy "F" n n vl ldvl vr ldvr))))
          (wantvr
           (f2cl-lib:f2cl-set-string side "R" (string 1))
           (zlacpy "L" n n a lda vr ldvr)
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
               (zunghr n ilo ihi vr ldvr
                (f2cl-lib:array-slice work-%data%
                                      f2cl-lib:complex16
                                      (itau)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:array-slice work-%data%
                                      f2cl-lib:complex16
                                      (iwrk)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwrk) 1) ierr)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
             (setf ierr var-8))
           (setf iwrk itau)
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                  var-10 var-11 var-12)
               (zhseqr "S" "V" n ilo ihi a lda w vr ldvr
                (f2cl-lib:array-slice work-%data%
                                      f2cl-lib:complex16
                                      (iwrk)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwrk) 1) info)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11))
             (setf info var-12)))
          (t
           (setf iwrk itau)
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                  var-10 var-11 var-12)
               (zhseqr "E" "N" n ilo ihi a lda w vr ldvr
                (f2cl-lib:array-slice work-%data%
                                      f2cl-lib:complex16
                                      (iwrk)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwrk) 1) info)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11))
             (setf info var-12))))
        (if (> info 0) (go label50))
        (cond
          ((or wantvl wantvr)
           (setf irwork (f2cl-lib:int-add ibal n))
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                  var-10 var-11 var-12 var-13 var-14)
               (ztrevc side "B" select n a lda vl ldvl vr ldvr n nout
                (f2cl-lib:array-slice work-%data%
                                      f2cl-lib:complex16
                                      (iwrk)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:array-slice rwork-%data%
                                      double-float
                                      (irwork)
                                      ((1 *))
                                      rwork-%offset%)
                ierr)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-12 var-13))
             (setf nout var-11)
             (setf ierr var-14))))
        (cond
          (wantvl
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
               (zgebak "B" "L" n ilo ihi
                (f2cl-lib:array-slice rwork-%data%
                                      double-float
                                      (ibal)
                                      ((1 *))
                                      rwork-%offset%)
                n vl ldvl ierr)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8))
             (setf ierr var-9))
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i n) nil)
             (tagbody
               (setf scl
                       (/ one
                          (dznrm2 n
                           (f2cl-lib:array-slice vl-%data%
                                                 f2cl-lib:complex16
                                                 (1 i)
                                                 ((1 ldvl) (1 *))
                                                 vl-%offset%)
                           1)))
               (zdscal n scl
                (f2cl-lib:array-slice vl-%data%
                                      f2cl-lib:complex16
                                      (1 i)
                                      ((1 ldvl) (1 *))
                                      vl-%offset%)
                1)
               (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                             ((> k n) nil)
                 (tagbody
                   (setf (f2cl-lib:fref rwork-%data%
                                        ((f2cl-lib:int-sub
                                          (f2cl-lib:int-add irwork k)
                                          1))
                                        ((1 *))
                                        rwork-%offset%)
                           (+
                            (expt
                             (f2cl-lib:dble
                              (f2cl-lib:fref vl-%data%
                                             (k i)
                                             ((1 ldvl) (1 *))
                                             vl-%offset%))
                             2)
                            (expt
                             (f2cl-lib:dimag
                              (f2cl-lib:fref vl-%data%
                                             (k i)
                                             ((1 ldvl) (1 *))
                                             vl-%offset%))
                             2)))
                  label10))
               (setf k
                       (idamax n
                        (f2cl-lib:array-slice rwork-%data%
                                              double-float
                                              (irwork)
                                              ((1 *))
                                              rwork-%offset%)
                        1))
               (setf tmp
                       (coerce
                        (/
                         (f2cl-lib:dconjg
                          (f2cl-lib:fref vl-%data%
                                         (k i)
                                         ((1 ldvl) (1 *))
                                         vl-%offset%))
                         (f2cl-lib:fsqrt
                          (f2cl-lib:fref rwork-%data%
                                         ((f2cl-lib:int-sub
                                           (f2cl-lib:int-add irwork k)
                                           1))
                                         ((1 *))
                                         rwork-%offset%)))
                        'f2cl-lib:complex16))
               (zscal n tmp
                (f2cl-lib:array-slice vl-%data%
                                      f2cl-lib:complex16
                                      (1 i)
                                      ((1 ldvl) (1 *))
                                      vl-%offset%)
                1)
               (setf (f2cl-lib:fref vl-%data%
                                    (k i)
                                    ((1 ldvl) (1 *))
                                    vl-%offset%)
                       (f2cl-lib:dcmplx
                        (f2cl-lib:dble
                         (f2cl-lib:fref vl-%data%
                                        (k i)
                                        ((1 ldvl) (1 *))
                                        vl-%offset%))
                        zero))
              label20))))
        (cond
          (wantvr
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
               (zgebak "B" "R" n ilo ihi
                (f2cl-lib:array-slice rwork-%data%
                                      double-float
                                      (ibal)
                                      ((1 *))
                                      rwork-%offset%)
                n vr ldvr ierr)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8))
             (setf ierr var-9))
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i n) nil)
             (tagbody
               (setf scl
                       (/ one
                          (dznrm2 n
                           (f2cl-lib:array-slice vr-%data%
                                                 f2cl-lib:complex16
                                                 (1 i)
                                                 ((1 ldvr) (1 *))
                                                 vr-%offset%)
                           1)))
               (zdscal n scl
                (f2cl-lib:array-slice vr-%data%
                                      f2cl-lib:complex16
                                      (1 i)
                                      ((1 ldvr) (1 *))
                                      vr-%offset%)
                1)
               (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                             ((> k n) nil)
                 (tagbody
                   (setf (f2cl-lib:fref rwork-%data%
                                        ((f2cl-lib:int-sub
                                          (f2cl-lib:int-add irwork k)
                                          1))
                                        ((1 *))
                                        rwork-%offset%)
                           (+
                            (expt
                             (f2cl-lib:dble
                              (f2cl-lib:fref vr-%data%
                                             (k i)
                                             ((1 ldvr) (1 *))
                                             vr-%offset%))
                             2)
                            (expt
                             (f2cl-lib:dimag
                              (f2cl-lib:fref vr-%data%
                                             (k i)
                                             ((1 ldvr) (1 *))
                                             vr-%offset%))
                             2)))
                  label30))
               (setf k
                       (idamax n
                        (f2cl-lib:array-slice rwork-%data%
                                              double-float
                                              (irwork)
                                              ((1 *))
                                              rwork-%offset%)
                        1))
               (setf tmp
                       (coerce
                        (/
                         (f2cl-lib:dconjg
                          (f2cl-lib:fref vr-%data%
                                         (k i)
                                         ((1 ldvr) (1 *))
                                         vr-%offset%))
                         (f2cl-lib:fsqrt
                          (f2cl-lib:fref rwork-%data%
                                         ((f2cl-lib:int-sub
                                           (f2cl-lib:int-add irwork k)
                                           1))
                                         ((1 *))
                                         rwork-%offset%)))
                        'f2cl-lib:complex16))
               (zscal n tmp
                (f2cl-lib:array-slice vr-%data%
                                      f2cl-lib:complex16
                                      (1 i)
                                      ((1 ldvr) (1 *))
                                      vr-%offset%)
                1)
               (setf (f2cl-lib:fref vr-%data%
                                    (k i)
                                    ((1 ldvr) (1 *))
                                    vr-%offset%)
                       (f2cl-lib:dcmplx
                        (f2cl-lib:dble
                         (f2cl-lib:fref vr-%data%
                                        (k i)
                                        ((1 ldvr) (1 *))
                                        vr-%offset%))
                        zero))
              label40))))
       label50
        (cond
          (scalea
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
               (zlascl "G" 0 0 cscale anrm (f2cl-lib:int-sub n info) 1
                (f2cl-lib:array-slice w-%data%
                                      f2cl-lib:complex16
                                      ((+ info 1))
                                      ((1 *))
                                      w-%offset%)
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
                  (zlascl "G" 0 0 cscale anrm (f2cl-lib:int-sub ilo 1) 1 w n
                   ierr)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7 var-8))
                (setf ierr var-9))))))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce maxwrk 'f2cl-lib:complex16))
        (go end_label)
       end_label
        (return
         (values nil nil nil nil nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zgeev fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::zscal fortran-to-lisp::idamax
                    fortran-to-lisp::zdscal fortran-to-lisp::dznrm2
                    fortran-to-lisp::zgebak fortran-to-lisp::ztrevc
                    fortran-to-lisp::zunghr fortran-to-lisp::zlacpy
                    fortran-to-lisp::zgehrd fortran-to-lisp::zgebal
                    fortran-to-lisp::zlascl fortran-to-lisp::zlange
                    fortran-to-lisp::dlabad fortran-to-lisp::dlamch
                    fortran-to-lisp::xerbla fortran-to-lisp::zhseqr
                    fortran-to-lisp::ilaenv fortran-to-lisp::lsame))))

