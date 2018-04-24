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


(let* ((zero 0.0) (one 1.0) (two 2.0))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (type (double-float 2.0 2.0) two)
           (ignorable zero one two))
  (defun dtrsna
         (job howmny select n t$ ldt vl ldvl vr ldvr s sep mm m work ldwork
          iwork info)
    (declare (type (array f2cl-lib:integer4 (*)) iwork)
             (type (array double-float (*)) work sep s vr vl t$)
             (type (f2cl-lib:integer4) info ldwork m mm ldvr ldvl ldt n)
             (type (array f2cl-lib:logical (*)) select)
             (type (simple-string *) howmny job))
    (f2cl-lib:with-multi-array-data
        ((job character job-%data% job-%offset%)
         (howmny character howmny-%data% howmny-%offset%)
         (select f2cl-lib:logical select-%data% select-%offset%)
         (t$ double-float t$-%data% t$-%offset%)
         (vl double-float vl-%data% vl-%offset%)
         (vr double-float vr-%data% vr-%offset%)
         (s double-float s-%data% s-%offset%)
         (sep double-float sep-%data% sep-%offset%)
         (work double-float work-%data% work-%offset%)
         (iwork f2cl-lib:integer4 iwork-%data% iwork-%offset%))
      (prog ((dummy (make-array 1 :element-type 'double-float)) (bignum 0.0)
             (cond$ 0.0) (cs 0.0) (delta 0.0) (dumm 0.0) (eps 0.0) (est 0.0)
             (lnrm 0.0) (mu 0.0) (prod 0.0) (prod1 0.0) (prod2 0.0) (rnrm 0.0)
             (scale 0.0) (smlnum 0.0) (sn 0.0) (i 0) (ierr 0) (ifst 0) (ilst 0)
             (j 0) (k 0) (kase 0) (ks 0) (n2 0) (nn 0) (pair nil) (somcon nil)
             (wantbh nil) (wants nil) (wantsp nil))
        (declare (type (array double-float (1)) dummy)
                 (type (double-float) bignum cond$ cs delta dumm eps est lnrm
                                      mu prod prod1 prod2 rnrm scale smlnum sn)
                 (type (f2cl-lib:integer4) i ierr ifst ilst j k kase ks n2 nn)
                 (type f2cl-lib:logical pair somcon wantbh wants wantsp))
        (setf wantbh (lsame job "B"))
        (setf wants (or (lsame job "E") wantbh))
        (setf wantsp (or (lsame job "V") wantbh))
        (setf somcon (lsame howmny "S"))
        (setf info 0)
        (cond
          ((and (not wants) (not wantsp))
           (setf info -1))
          ((and (not (lsame howmny "A")) (not somcon))
           (setf info -2))
          ((< n 0)
           (setf info -4))
          ((< ldt (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
           (setf info -6))
          ((or (< ldvl 1) (and wants (< ldvl n)))
           (setf info -8))
          ((or (< ldvr 1) (and wants (< ldvr n)))
           (setf info -10))
          (t
           (cond
             (somcon
              (setf m 0)
              (setf pair f2cl-lib:%false%)
              (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                            ((> k n) nil)
                (tagbody
                  (cond
                    (pair
                     (setf pair f2cl-lib:%false%))
                    (t
                     (cond
                       ((< k n)
                        (cond
                          ((=
                            (f2cl-lib:fref t$
                                           ((f2cl-lib:int-add k 1) k)
                                           ((1 ldt) (1 *)))
                            zero)
                           (if
                            (f2cl-lib:fref select-%data%
                                           (k)
                                           ((1 *))
                                           select-%offset%)
                            (setf m (f2cl-lib:int-add m 1))))
                          (t
                           (setf pair f2cl-lib:%true%)
                           (if
                            (or
                             (f2cl-lib:fref select-%data%
                                            (k)
                                            ((1 *))
                                            select-%offset%)
                             (f2cl-lib:fref select-%data%
                                            ((f2cl-lib:int-add k 1))
                                            ((1 *))
                                            select-%offset%))
                            (setf m (f2cl-lib:int-add m 2))))))
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
              (setf info -13))
             ((or (< ldwork 1) (and wantsp (< ldwork n)))
              (setf info -16)))))
        (cond
          ((/= info 0)
           (xerbla "DTRSNA" (f2cl-lib:int-sub info))
           (go end_label)))
        (if (= n 0) (go end_label))
        (cond
          ((= n 1)
           (cond
             (somcon
              (if
               (not (f2cl-lib:fref select-%data% (1) ((1 *)) select-%offset%))
               (go end_label))))
           (if wants (setf (f2cl-lib:fref s-%data% (1) ((1 *)) s-%offset%) one))
           (if wantsp
               (setf (f2cl-lib:fref sep-%data% (1) ((1 *)) sep-%offset%)
                       (abs
                        (f2cl-lib:fref t$-%data%
                                       (1 1)
                                       ((1 ldt) (1 *))
                                       t$-%offset%))))
           (go end_label)))
        (setf eps (dlamch "P"))
        (setf smlnum (/ (dlamch "S") eps))
        (setf bignum (/ one smlnum))
        (multiple-value-bind (var-0 var-1)
            (dlabad smlnum bignum)
          (declare (ignore))
          (setf smlnum var-0)
          (setf bignum var-1))
        (setf ks 0)
        (setf pair f2cl-lib:%false%)
        (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                      ((> k n) nil)
          (tagbody
            (cond
              (pair
               (setf pair f2cl-lib:%false%)
               (go label60))
              (t
               (if (< k n)
                   (setf pair
                           (coerce
                            (/=
                             (f2cl-lib:fref t$-%data%
                                            ((f2cl-lib:int-add k 1) k)
                                            ((1 ldt) (1 *))
                                            t$-%offset%)
                             zero)
                            'f2cl-lib:logical)))))
            (cond
              (somcon
               (cond
                 (pair
                  (if
                   (and
                    (not
                     (f2cl-lib:fref select-%data% (k) ((1 *)) select-%offset%))
                    (not
                     (f2cl-lib:fref select-%data%
                                    ((f2cl-lib:int-add k 1))
                                    ((1 *))
                                    select-%offset%)))
                   (go label60)))
                 (t
                  (if
                   (not
                    (f2cl-lib:fref select-%data% (k) ((1 *)) select-%offset%))
                   (go label60))))))
            (setf ks (f2cl-lib:int-add ks 1))
            (cond
              (wants
               (cond
                 ((not pair)
                  (setf prod
                          (ddot n
                           (f2cl-lib:array-slice vr-%data%
                                                 double-float
                                                 (1 ks)
                                                 ((1 ldvr) (1 *))
                                                 vr-%offset%)
                           1
                           (f2cl-lib:array-slice vl-%data%
                                                 double-float
                                                 (1 ks)
                                                 ((1 ldvl) (1 *))
                                                 vl-%offset%)
                           1))
                  (setf rnrm
                          (dnrm2 n
                           (f2cl-lib:array-slice vr-%data%
                                                 double-float
                                                 (1 ks)
                                                 ((1 ldvr) (1 *))
                                                 vr-%offset%)
                           1))
                  (setf lnrm
                          (dnrm2 n
                           (f2cl-lib:array-slice vl-%data%
                                                 double-float
                                                 (1 ks)
                                                 ((1 ldvl) (1 *))
                                                 vl-%offset%)
                           1))
                  (setf (f2cl-lib:fref s-%data% (ks) ((1 *)) s-%offset%)
                          (/ (abs prod) (* rnrm lnrm))))
                 (t
                  (setf prod1
                          (ddot n
                           (f2cl-lib:array-slice vr-%data%
                                                 double-float
                                                 (1 ks)
                                                 ((1 ldvr) (1 *))
                                                 vr-%offset%)
                           1
                           (f2cl-lib:array-slice vl-%data%
                                                 double-float
                                                 (1 ks)
                                                 ((1 ldvl) (1 *))
                                                 vl-%offset%)
                           1))
                  (setf prod1
                          (+ prod1
                             (ddot n
                              (f2cl-lib:array-slice vr-%data%
                                                    double-float
                                                    (1 (f2cl-lib:int-add ks 1))
                                                    ((1 ldvr) (1 *))
                                                    vr-%offset%)
                              1
                              (f2cl-lib:array-slice vl-%data%
                                                    double-float
                                                    (1 (f2cl-lib:int-add ks 1))
                                                    ((1 ldvl) (1 *))
                                                    vl-%offset%)
                              1)))
                  (setf prod2
                          (ddot n
                           (f2cl-lib:array-slice vl-%data%
                                                 double-float
                                                 (1 ks)
                                                 ((1 ldvl) (1 *))
                                                 vl-%offset%)
                           1
                           (f2cl-lib:array-slice vr-%data%
                                                 double-float
                                                 (1 (f2cl-lib:int-add ks 1))
                                                 ((1 ldvr) (1 *))
                                                 vr-%offset%)
                           1))
                  (setf prod2
                          (- prod2
                             (ddot n
                              (f2cl-lib:array-slice vl-%data%
                                                    double-float
                                                    (1 (f2cl-lib:int-add ks 1))
                                                    ((1 ldvl) (1 *))
                                                    vl-%offset%)
                              1
                              (f2cl-lib:array-slice vr-%data%
                                                    double-float
                                                    (1 ks)
                                                    ((1 ldvr) (1 *))
                                                    vr-%offset%)
                              1)))
                  (setf rnrm
                          (dlapy2
                           (dnrm2 n
                            (f2cl-lib:array-slice vr-%data%
                                                  double-float
                                                  (1 ks)
                                                  ((1 ldvr) (1 *))
                                                  vr-%offset%)
                            1)
                           (dnrm2 n
                            (f2cl-lib:array-slice vr-%data%
                                                  double-float
                                                  (1 (f2cl-lib:int-add ks 1))
                                                  ((1 ldvr) (1 *))
                                                  vr-%offset%)
                            1)))
                  (setf lnrm
                          (dlapy2
                           (dnrm2 n
                            (f2cl-lib:array-slice vl-%data%
                                                  double-float
                                                  (1 ks)
                                                  ((1 ldvl) (1 *))
                                                  vl-%offset%)
                            1)
                           (dnrm2 n
                            (f2cl-lib:array-slice vl-%data%
                                                  double-float
                                                  (1 (f2cl-lib:int-add ks 1))
                                                  ((1 ldvl) (1 *))
                                                  vl-%offset%)
                            1)))
                  (setf cond$ (/ (dlapy2 prod1 prod2) (* rnrm lnrm)))
                  (setf (f2cl-lib:fref s-%data% (ks) ((1 *)) s-%offset%) cond$)
                  (setf (f2cl-lib:fref s-%data%
                                       ((f2cl-lib:int-add ks 1))
                                       ((1 *))
                                       s-%offset%)
                          cond$)))))
            (cond
              (wantsp
               (dlacpy "Full" n n t$ ldt work ldwork)
               (setf ifst k)
               (setf ilst 1)
               (multiple-value-bind
                     (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                      var-9)
                   (dtrexc "No Q" n work ldwork dummy 1 ifst ilst
                    (f2cl-lib:array-slice work-%data%
                                          double-float
                                          (1 (f2cl-lib:int-add n 1))
                                          ((1 ldwork) (1 *))
                                          work-%offset%)
                    ierr)
                 (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-8))
                 (setf ifst var-6)
                 (setf ilst var-7)
                 (setf ierr var-9))
               (cond
                 ((or (= ierr 1) (= ierr 2))
                  (setf scale one)
                  (setf est bignum))
                 (t
                  (tagbody
                    (cond
                      ((= (f2cl-lib:fref work (2 1) ((1 ldwork) (1 *))) zero)
                       (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                                     ((> i n) nil)
                         (tagbody
                           (setf (f2cl-lib:fref work-%data%
                                                (i i)
                                                ((1 ldwork) (1 *))
                                                work-%offset%)
                                   (-
                                    (f2cl-lib:fref work-%data%
                                                   (i i)
                                                   ((1 ldwork) (1 *))
                                                   work-%offset%)
                                    (f2cl-lib:fref work-%data%
                                                   (1 1)
                                                   ((1 ldwork) (1 *))
                                                   work-%offset%)))
                          label20))
                       (setf n2 1)
                       (setf nn (f2cl-lib:int-sub n 1)))
                      (t
                       (setf mu
                               (*
                                (f2cl-lib:fsqrt
                                 (abs
                                  (f2cl-lib:fref work-%data%
                                                 (1 2)
                                                 ((1 ldwork) (1 *))
                                                 work-%offset%)))
                                (f2cl-lib:fsqrt
                                 (abs
                                  (f2cl-lib:fref work-%data%
                                                 (2 1)
                                                 ((1 ldwork) (1 *))
                                                 work-%offset%)))))
                       (setf delta
                               (dlapy2 mu
                                (f2cl-lib:fref work-%data%
                                               (2 1)
                                               ((1 ldwork) (1 *))
                                               work-%offset%)))
                       (setf cs (/ mu delta))
                       (setf sn
                               (/
                                (-
                                 (f2cl-lib:fref work-%data%
                                                (2 1)
                                                ((1 ldwork) (1 *))
                                                work-%offset%))
                                delta))
                       (f2cl-lib:fdo (j 3 (f2cl-lib:int-add j 1))
                                     ((> j n) nil)
                         (tagbody
                           (setf (f2cl-lib:fref work-%data%
                                                (2 j)
                                                ((1 ldwork) (1 *))
                                                work-%offset%)
                                   (* cs
                                      (f2cl-lib:fref work-%data%
                                                     (2 j)
                                                     ((1 ldwork) (1 *))
                                                     work-%offset%)))
                           (setf (f2cl-lib:fref work-%data%
                                                (j j)
                                                ((1 ldwork) (1 *))
                                                work-%offset%)
                                   (-
                                    (f2cl-lib:fref work-%data%
                                                   (j j)
                                                   ((1 ldwork) (1 *))
                                                   work-%offset%)
                                    (f2cl-lib:fref work-%data%
                                                   (1 1)
                                                   ((1 ldwork) (1 *))
                                                   work-%offset%)))
                          label30))
                       (setf (f2cl-lib:fref work-%data%
                                            (2 2)
                                            ((1 ldwork) (1 *))
                                            work-%offset%)
                               zero)
                       (setf (f2cl-lib:fref work-%data%
                                            (1 (f2cl-lib:int-add n 1))
                                            ((1 ldwork) (1 *))
                                            work-%offset%)
                               (* two mu))
                       (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                                     ((> i
                                         (f2cl-lib:int-add n
                                                           (f2cl-lib:int-sub
                                                            1)))
                                      nil)
                         (tagbody
                           (setf (f2cl-lib:fref work-%data%
                                                (i (f2cl-lib:int-add n 1))
                                                ((1 ldwork) (1 *))
                                                work-%offset%)
                                   (* sn
                                      (f2cl-lib:fref work-%data%
                                                     (1 (f2cl-lib:int-add i 1))
                                                     ((1 ldwork) (1 *))
                                                     work-%offset%)))
                          label40))
                       (setf n2 2)
                       (setf nn (f2cl-lib:int-mul 2 (f2cl-lib:int-sub n 1)))))
                    (setf est zero)
                    (setf kase 0)
                   label50
                    (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                        (dlacon nn
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (1 (f2cl-lib:int-add n 2))
                                               ((1 ldwork) (1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (1 (f2cl-lib:int-add n 4))
                                               ((1 ldwork) (1 *))
                                               work-%offset%)
                         iwork est kase)
                      (declare (ignore var-0 var-1 var-2 var-3))
                      (setf est var-4)
                      (setf kase var-5))
                    (cond
                      ((/= kase 0)
                       (cond
                         ((= kase 1)
                          (cond
                            ((= n2 1)
                             (multiple-value-bind
                                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10)
                                 (dlaqtr f2cl-lib:%true% f2cl-lib:%true%
                                  (f2cl-lib:int-sub n 1)
                                  (f2cl-lib:array-slice work-%data%
                                                        double-float
                                                        (2 2)
                                                        ((1 ldwork) (1 *))
                                                        work-%offset%)
                                  ldwork dummy dumm scale
                                  (f2cl-lib:array-slice work-%data%
                                                        double-float
                                                        (1
                                                         (f2cl-lib:int-add n
                                                                           4))
                                                        ((1 ldwork) (1 *))
                                                        work-%offset%)
                                  (f2cl-lib:array-slice work-%data%
                                                        double-float
                                                        (1
                                                         (f2cl-lib:int-add n
                                                                           6))
                                                        ((1 ldwork) (1 *))
                                                        work-%offset%)
                                  ierr)
                               (declare (ignore var-0 var-1 var-2 var-3 var-4
                                                var-5 var-6 var-8 var-9))
                               (setf scale var-7)
                               (setf ierr var-10)))
                            (t
                             (multiple-value-bind
                                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10)
                                 (dlaqtr f2cl-lib:%true% f2cl-lib:%false%
                                  (f2cl-lib:int-sub n 1)
                                  (f2cl-lib:array-slice work-%data%
                                                        double-float
                                                        (2 2)
                                                        ((1 ldwork) (1 *))
                                                        work-%offset%)
                                  ldwork
                                  (f2cl-lib:array-slice work-%data%
                                                        double-float
                                                        (1
                                                         (f2cl-lib:int-add n
                                                                           1))
                                                        ((1 ldwork) (1 *))
                                                        work-%offset%)
                                  mu scale
                                  (f2cl-lib:array-slice work-%data%
                                                        double-float
                                                        (1
                                                         (f2cl-lib:int-add n
                                                                           4))
                                                        ((1 ldwork) (1 *))
                                                        work-%offset%)
                                  (f2cl-lib:array-slice work-%data%
                                                        double-float
                                                        (1
                                                         (f2cl-lib:int-add n
                                                                           6))
                                                        ((1 ldwork) (1 *))
                                                        work-%offset%)
                                  ierr)
                               (declare (ignore var-0 var-1 var-2 var-3 var-4
                                                var-5 var-6 var-8 var-9))
                               (setf scale var-7)
                               (setf ierr var-10)))))
                         (t
                          (cond
                            ((= n2 1)
                             (multiple-value-bind
                                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10)
                                 (dlaqtr f2cl-lib:%false% f2cl-lib:%true%
                                  (f2cl-lib:int-sub n 1)
                                  (f2cl-lib:array-slice work-%data%
                                                        double-float
                                                        (2 2)
                                                        ((1 ldwork) (1 *))
                                                        work-%offset%)
                                  ldwork dummy dumm scale
                                  (f2cl-lib:array-slice work-%data%
                                                        double-float
                                                        (1
                                                         (f2cl-lib:int-add n
                                                                           4))
                                                        ((1 ldwork) (1 *))
                                                        work-%offset%)
                                  (f2cl-lib:array-slice work-%data%
                                                        double-float
                                                        (1
                                                         (f2cl-lib:int-add n
                                                                           6))
                                                        ((1 ldwork) (1 *))
                                                        work-%offset%)
                                  ierr)
                               (declare (ignore var-0 var-1 var-2 var-3 var-4
                                                var-5 var-6 var-8 var-9))
                               (setf scale var-7)
                               (setf ierr var-10)))
                            (t
                             (multiple-value-bind
                                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10)
                                 (dlaqtr f2cl-lib:%false% f2cl-lib:%false%
                                  (f2cl-lib:int-sub n 1)
                                  (f2cl-lib:array-slice work-%data%
                                                        double-float
                                                        (2 2)
                                                        ((1 ldwork) (1 *))
                                                        work-%offset%)
                                  ldwork
                                  (f2cl-lib:array-slice work-%data%
                                                        double-float
                                                        (1
                                                         (f2cl-lib:int-add n
                                                                           1))
                                                        ((1 ldwork) (1 *))
                                                        work-%offset%)
                                  mu scale
                                  (f2cl-lib:array-slice work-%data%
                                                        double-float
                                                        (1
                                                         (f2cl-lib:int-add n
                                                                           4))
                                                        ((1 ldwork) (1 *))
                                                        work-%offset%)
                                  (f2cl-lib:array-slice work-%data%
                                                        double-float
                                                        (1
                                                         (f2cl-lib:int-add n
                                                                           6))
                                                        ((1 ldwork) (1 *))
                                                        work-%offset%)
                                  ierr)
                               (declare (ignore var-0 var-1 var-2 var-3 var-4
                                                var-5 var-6 var-8 var-9))
                               (setf scale var-7)
                               (setf ierr var-10))))))
                       (go label50))))))
               (setf (f2cl-lib:fref sep-%data% (ks) ((1 *)) sep-%offset%)
                       (/ scale (max est smlnum)))
               (if pair
                   (setf (f2cl-lib:fref sep-%data%
                                        ((f2cl-lib:int-add ks 1))
                                        ((1 *))
                                        sep-%offset%)
                           (f2cl-lib:fref sep-%data%
                                          (ks)
                                          ((1 *))
                                          sep-%offset%)))))
            (if pair (setf ks (f2cl-lib:int-add ks 1)))
           label60))
        (go end_label)
       end_label
        (return
         (values nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 m
                 nil
                 nil
                 nil
                 info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dtrsna
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string)
                        (array fortran-to-lisp::logical (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::m nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlaqtr fortran-to-lisp::dlacon
                    fortran-to-lisp::dtrexc fortran-to-lisp::dlacpy
                    fortran-to-lisp::dlapy2 fortran-to-lisp::dnrm2
                    fortran-to-lisp::ddot fortran-to-lisp::dlabad
                    fortran-to-lisp::dlamch fortran-to-lisp::xerbla
                    fortran-to-lisp::lsame))))

