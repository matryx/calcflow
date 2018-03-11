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


(let* ((one 1.0) (zero 0.0) (negone (- 1.0)))
  (declare (type (double-float 1.0 1.0) one)
           (type (double-float 0.0 0.0) zero)
           (type (double-float) negone)
           (ignorable one zero negone))
  (defun dlasd3
         (nl nr sqre k d q ldq dsigma u ldu u2 ldu2 vt ldvt vt2 ldvt2 idxc ctot
          z info)
    (declare (type (array f2cl-lib:integer4 (*)) ctot idxc)
             (type (array double-float (*)) z vt2 vt u2 u dsigma q d)
             (type (f2cl-lib:integer4) info ldvt2 ldvt ldu2 ldu ldq k sqre nr
                                       nl))
    (f2cl-lib:with-multi-array-data
        ((d double-float d-%data% d-%offset%)
         (q double-float q-%data% q-%offset%)
         (dsigma double-float dsigma-%data% dsigma-%offset%)
         (u double-float u-%data% u-%offset%)
         (u2 double-float u2-%data% u2-%offset%)
         (vt double-float vt-%data% vt-%offset%)
         (vt2 double-float vt2-%data% vt2-%offset%)
         (z double-float z-%data% z-%offset%)
         (idxc f2cl-lib:integer4 idxc-%data% idxc-%offset%)
         (ctot f2cl-lib:integer4 ctot-%data% ctot-%offset%))
      (prog ((rho 0.0) (temp 0.0) (ctemp 0) (i 0) (j 0) (jc 0) (ktemp 0) (m 0)
             (n 0) (nlp1 0) (nlp2 0) (nrp1 0))
        (declare (type (double-float) rho temp)
                 (type (f2cl-lib:integer4) ctemp i j jc ktemp m n nlp1 nlp2
                                           nrp1))
        (setf info 0)
        (cond
          ((< nl 1)
           (setf info -1))
          ((< nr 1)
           (setf info -2))
          ((and (/= sqre 1) (/= sqre 0))
           (setf info -3)))
        (setf n (f2cl-lib:int-add nl nr 1))
        (setf m (f2cl-lib:int-add n sqre))
        (setf nlp1 (f2cl-lib:int-add nl 1))
        (setf nlp2 (f2cl-lib:int-add nl 2))
        (cond
          ((or (< k 1) (> k n))
           (setf info -4))
          ((< ldq k)
           (setf info -7))
          ((< ldu n)
           (setf info -10))
          ((< ldu2 n)
           (setf info -12))
          ((< ldvt m)
           (setf info -14))
          ((< ldvt2 m)
           (setf info -16)))
        (cond
          ((/= info 0)
           (xerbla "DLASD3" (f2cl-lib:int-sub info))
           (go end_label)))
        (cond
          ((= k 1)
           (setf (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%)
                   (abs (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%)))
           (dcopy m
            (f2cl-lib:array-slice vt2-%data%
                                  double-float
                                  (1 1)
                                  ((1 ldvt2) (1 *))
                                  vt2-%offset%)
            ldvt2
            (f2cl-lib:array-slice vt-%data%
                                  double-float
                                  (1 1)
                                  ((1 ldvt) (1 *))
                                  vt-%offset%)
            ldvt)
           (cond
             ((> (f2cl-lib:fref z (1) ((1 *))) zero)
              (dcopy n
               (f2cl-lib:array-slice u2-%data%
                                     double-float
                                     (1 1)
                                     ((1 ldu2) (1 *))
                                     u2-%offset%)
               1
               (f2cl-lib:array-slice u-%data%
                                     double-float
                                     (1 1)
                                     ((1 ldu) (1 *))
                                     u-%offset%)
               1))
             (t
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i n) nil)
                (tagbody
                  (setf (f2cl-lib:fref u-%data%
                                       (i 1)
                                       ((1 ldu) (1 *))
                                       u-%offset%)
                          (-
                           (f2cl-lib:fref u2-%data%
                                          (i 1)
                                          ((1 ldu2) (1 *))
                                          u2-%offset%)))
                 label10))))
           (go end_label)))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i k) nil)
          (tagbody
            (setf (f2cl-lib:fref dsigma-%data% (i) ((1 *)) dsigma-%offset%)
                    (-
                     (dlamc3
                      (f2cl-lib:fref dsigma-%data% (i) ((1 *)) dsigma-%offset%)
                      (f2cl-lib:fref dsigma-%data%
                                     (i)
                                     ((1 *))
                                     dsigma-%offset%))
                     (f2cl-lib:fref dsigma-%data%
                                    (i)
                                    ((1 *))
                                    dsigma-%offset%)))
           label20))
        (dcopy k z 1 q 1)
        (setf rho (dnrm2 k z 1))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
            (dlascl "G" 0 0 rho one k 1 z k info)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8))
          (setf info var-9))
        (setf rho (* rho rho))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j k) nil)
          (tagbody
            (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                (dlasd4 k j dsigma z
                 (f2cl-lib:array-slice u-%data%
                                       double-float
                                       (1 j)
                                       ((1 ldu) (1 *))
                                       u-%offset%)
                 rho (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                 (f2cl-lib:array-slice vt-%data%
                                       double-float
                                       (1 j)
                                       ((1 ldvt) (1 *))
                                       vt-%offset%)
                 info)
              (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-7))
              (setf (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%) var-6)
              (setf info var-8))
            (cond
              ((/= info 0)
               (go end_label)))
           label30))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i k) nil)
          (tagbody
            (setf (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                    (*
                     (f2cl-lib:fref u-%data% (i k) ((1 ldu) (1 *)) u-%offset%)
                     (f2cl-lib:fref vt-%data%
                                    (i k)
                                    ((1 ldvt) (1 *))
                                    vt-%offset%)))
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j (f2cl-lib:int-add i (f2cl-lib:int-sub 1))) nil)
              (tagbody
                (setf (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                        (* (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                           (/
                            (/
                             (*
                              (f2cl-lib:fref u-%data%
                                             (i j)
                                             ((1 ldu) (1 *))
                                             u-%offset%)
                              (f2cl-lib:fref vt-%data%
                                             (i j)
                                             ((1 ldvt) (1 *))
                                             vt-%offset%))
                             (-
                              (f2cl-lib:fref dsigma-%data%
                                             (i)
                                             ((1 *))
                                             dsigma-%offset%)
                              (f2cl-lib:fref dsigma-%data%
                                             (j)
                                             ((1 *))
                                             dsigma-%offset%)))
                            (+
                             (f2cl-lib:fref dsigma-%data%
                                            (i)
                                            ((1 *))
                                            dsigma-%offset%)
                             (f2cl-lib:fref dsigma-%data%
                                            (j)
                                            ((1 *))
                                            dsigma-%offset%)))))
               label40))
            (f2cl-lib:fdo (j i (f2cl-lib:int-add j 1))
                          ((> j (f2cl-lib:int-add k (f2cl-lib:int-sub 1))) nil)
              (tagbody
                (setf (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                        (* (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                           (/
                            (/
                             (*
                              (f2cl-lib:fref u-%data%
                                             (i j)
                                             ((1 ldu) (1 *))
                                             u-%offset%)
                              (f2cl-lib:fref vt-%data%
                                             (i j)
                                             ((1 ldvt) (1 *))
                                             vt-%offset%))
                             (-
                              (f2cl-lib:fref dsigma-%data%
                                             (i)
                                             ((1 *))
                                             dsigma-%offset%)
                              (f2cl-lib:fref dsigma-%data%
                                             ((f2cl-lib:int-add j 1))
                                             ((1 *))
                                             dsigma-%offset%)))
                            (+
                             (f2cl-lib:fref dsigma-%data%
                                            (i)
                                            ((1 *))
                                            dsigma-%offset%)
                             (f2cl-lib:fref dsigma-%data%
                                            ((f2cl-lib:int-add j 1))
                                            ((1 *))
                                            dsigma-%offset%)))))
               label50))
            (setf (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                    (f2cl-lib:sign
                     (f2cl-lib:fsqrt
                      (abs (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)))
                     (f2cl-lib:fref q-%data%
                                    (i 1)
                                    ((1 ldq) (1 *))
                                    q-%offset%)))
           label60))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i k) nil)
          (tagbody
            (setf (f2cl-lib:fref vt-%data% (1 i) ((1 ldvt) (1 *)) vt-%offset%)
                    (/
                     (/ (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%)
                        (f2cl-lib:fref u-%data%
                                       (1 i)
                                       ((1 ldu) (1 *))
                                       u-%offset%))
                     (f2cl-lib:fref vt-%data%
                                    (1 i)
                                    ((1 ldvt) (1 *))
                                    vt-%offset%)))
            (setf (f2cl-lib:fref u-%data% (1 i) ((1 ldu) (1 *)) u-%offset%)
                    negone)
            (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                          ((> j k) nil)
              (tagbody
                (setf (f2cl-lib:fref vt-%data%
                                     (j i)
                                     ((1 ldvt) (1 *))
                                     vt-%offset%)
                        (/
                         (/ (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                            (f2cl-lib:fref u-%data%
                                           (j i)
                                           ((1 ldu) (1 *))
                                           u-%offset%))
                         (f2cl-lib:fref vt-%data%
                                        (j i)
                                        ((1 ldvt) (1 *))
                                        vt-%offset%)))
                (setf (f2cl-lib:fref u-%data% (j i) ((1 ldu) (1 *)) u-%offset%)
                        (*
                         (f2cl-lib:fref dsigma-%data%
                                        (j)
                                        ((1 *))
                                        dsigma-%offset%)
                         (f2cl-lib:fref vt-%data%
                                        (j i)
                                        ((1 ldvt) (1 *))
                                        vt-%offset%)))
               label70))
            (setf temp
                    (dnrm2 k
                     (f2cl-lib:array-slice u-%data%
                                           double-float
                                           (1 i)
                                           ((1 ldu) (1 *))
                                           u-%offset%)
                     1))
            (setf (f2cl-lib:fref q-%data% (1 i) ((1 ldq) (1 *)) q-%offset%)
                    (/
                     (f2cl-lib:fref u-%data% (1 i) ((1 ldu) (1 *)) u-%offset%)
                     temp))
            (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                          ((> j k) nil)
              (tagbody
                (setf jc (f2cl-lib:fref idxc-%data% (j) ((1 *)) idxc-%offset%))
                (setf (f2cl-lib:fref q-%data% (j i) ((1 ldq) (1 *)) q-%offset%)
                        (/
                         (f2cl-lib:fref u-%data%
                                        (jc i)
                                        ((1 ldu) (1 *))
                                        u-%offset%)
                         temp))
               label80))
           label90))
        (cond
          ((= k 2)
           (dgemm "N" "N" n k k one u2 ldu2 q ldq zero u ldu)
           (go label100)))
        (cond
          ((> (f2cl-lib:fref ctot (1) ((1 *))) 0)
           (dgemm "N" "N" nl k
            (f2cl-lib:fref ctot-%data% (1) ((1 *)) ctot-%offset%) one
            (f2cl-lib:array-slice u2-%data%
                                  double-float
                                  (1 2)
                                  ((1 ldu2) (1 *))
                                  u2-%offset%)
            ldu2
            (f2cl-lib:array-slice q-%data%
                                  double-float
                                  (2 1)
                                  ((1 ldq) (1 *))
                                  q-%offset%)
            ldq zero
            (f2cl-lib:array-slice u-%data%
                                  double-float
                                  (1 1)
                                  ((1 ldu) (1 *))
                                  u-%offset%)
            ldu)
           (cond
             ((> (f2cl-lib:fref ctot (3) ((1 *))) 0)
              (setf ktemp
                      (f2cl-lib:int-add 2
                                        (f2cl-lib:fref ctot-%data%
                                                       (1)
                                                       ((1 *))
                                                       ctot-%offset%)
                                        (f2cl-lib:fref ctot-%data%
                                                       (2)
                                                       ((1 *))
                                                       ctot-%offset%)))
              (dgemm "N" "N" nl k
               (f2cl-lib:fref ctot-%data% (3) ((1 *)) ctot-%offset%) one
               (f2cl-lib:array-slice u2-%data%
                                     double-float
                                     (1 ktemp)
                                     ((1 ldu2) (1 *))
                                     u2-%offset%)
               ldu2
               (f2cl-lib:array-slice q-%data%
                                     double-float
                                     (ktemp 1)
                                     ((1 ldq) (1 *))
                                     q-%offset%)
               ldq one
               (f2cl-lib:array-slice u-%data%
                                     double-float
                                     (1 1)
                                     ((1 ldu) (1 *))
                                     u-%offset%)
               ldu))))
          ((> (f2cl-lib:fref ctot (3) ((1 *))) 0)
           (setf ktemp
                   (f2cl-lib:int-add 2
                                     (f2cl-lib:fref ctot-%data%
                                                    (1)
                                                    ((1 *))
                                                    ctot-%offset%)
                                     (f2cl-lib:fref ctot-%data%
                                                    (2)
                                                    ((1 *))
                                                    ctot-%offset%)))
           (dgemm "N" "N" nl k
            (f2cl-lib:fref ctot-%data% (3) ((1 *)) ctot-%offset%) one
            (f2cl-lib:array-slice u2-%data%
                                  double-float
                                  (1 ktemp)
                                  ((1 ldu2) (1 *))
                                  u2-%offset%)
            ldu2
            (f2cl-lib:array-slice q-%data%
                                  double-float
                                  (ktemp 1)
                                  ((1 ldq) (1 *))
                                  q-%offset%)
            ldq zero
            (f2cl-lib:array-slice u-%data%
                                  double-float
                                  (1 1)
                                  ((1 ldu) (1 *))
                                  u-%offset%)
            ldu))
          (t
           (dlacpy "F" nl k u2 ldu2 u ldu)))
        (dcopy k
         (f2cl-lib:array-slice q-%data%
                               double-float
                               (1 1)
                               ((1 ldq) (1 *))
                               q-%offset%)
         ldq
         (f2cl-lib:array-slice u-%data%
                               double-float
                               (nlp1 1)
                               ((1 ldu) (1 *))
                               u-%offset%)
         ldu)
        (setf ktemp
                (f2cl-lib:int-add 2
                                  (f2cl-lib:fref ctot-%data%
                                                 (1)
                                                 ((1 *))
                                                 ctot-%offset%)))
        (setf ctemp
                (f2cl-lib:int-add
                 (f2cl-lib:fref ctot-%data% (2) ((1 *)) ctot-%offset%)
                 (f2cl-lib:fref ctot-%data% (3) ((1 *)) ctot-%offset%)))
        (dgemm "N" "N" nr k ctemp one
         (f2cl-lib:array-slice u2-%data%
                               double-float
                               (nlp2 ktemp)
                               ((1 ldu2) (1 *))
                               u2-%offset%)
         ldu2
         (f2cl-lib:array-slice q-%data%
                               double-float
                               (ktemp 1)
                               ((1 ldq) (1 *))
                               q-%offset%)
         ldq zero
         (f2cl-lib:array-slice u-%data%
                               double-float
                               (nlp2 1)
                               ((1 ldu) (1 *))
                               u-%offset%)
         ldu)
       label100
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i k) nil)
          (tagbody
            (setf temp
                    (dnrm2 k
                     (f2cl-lib:array-slice vt-%data%
                                           double-float
                                           (1 i)
                                           ((1 ldvt) (1 *))
                                           vt-%offset%)
                     1))
            (setf (f2cl-lib:fref q-%data% (i 1) ((1 ldq) (1 *)) q-%offset%)
                    (/
                     (f2cl-lib:fref vt-%data%
                                    (1 i)
                                    ((1 ldvt) (1 *))
                                    vt-%offset%)
                     temp))
            (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                          ((> j k) nil)
              (tagbody
                (setf jc (f2cl-lib:fref idxc-%data% (j) ((1 *)) idxc-%offset%))
                (setf (f2cl-lib:fref q-%data% (i j) ((1 ldq) (1 *)) q-%offset%)
                        (/
                         (f2cl-lib:fref vt-%data%
                                        (jc i)
                                        ((1 ldvt) (1 *))
                                        vt-%offset%)
                         temp))
               label110))
           label120))
        (cond
          ((= k 2)
           (dgemm "N" "N" k m k one q ldq vt2 ldvt2 zero vt ldvt)
           (go end_label)))
        (setf ktemp
                (f2cl-lib:int-add 1
                                  (f2cl-lib:fref ctot-%data%
                                                 (1)
                                                 ((1 *))
                                                 ctot-%offset%)))
        (dgemm "N" "N" k nlp1 ktemp one
         (f2cl-lib:array-slice q-%data%
                               double-float
                               (1 1)
                               ((1 ldq) (1 *))
                               q-%offset%)
         ldq
         (f2cl-lib:array-slice vt2-%data%
                               double-float
                               (1 1)
                               ((1 ldvt2) (1 *))
                               vt2-%offset%)
         ldvt2 zero
         (f2cl-lib:array-slice vt-%data%
                               double-float
                               (1 1)
                               ((1 ldvt) (1 *))
                               vt-%offset%)
         ldvt)
        (setf ktemp
                (f2cl-lib:int-add 2
                                  (f2cl-lib:fref ctot-%data%
                                                 (1)
                                                 ((1 *))
                                                 ctot-%offset%)
                                  (f2cl-lib:fref ctot-%data%
                                                 (2)
                                                 ((1 *))
                                                 ctot-%offset%)))
        (if (<= ktemp ldvt2)
            (dgemm "N" "N" k nlp1
             (f2cl-lib:fref ctot-%data% (3) ((1 *)) ctot-%offset%) one
             (f2cl-lib:array-slice q-%data%
                                   double-float
                                   (1 ktemp)
                                   ((1 ldq) (1 *))
                                   q-%offset%)
             ldq
             (f2cl-lib:array-slice vt2-%data%
                                   double-float
                                   (ktemp 1)
                                   ((1 ldvt2) (1 *))
                                   vt2-%offset%)
             ldvt2 one
             (f2cl-lib:array-slice vt-%data%
                                   double-float
                                   (1 1)
                                   ((1 ldvt) (1 *))
                                   vt-%offset%)
             ldvt))
        (setf ktemp
                (f2cl-lib:int-add
                 (f2cl-lib:fref ctot-%data% (1) ((1 *)) ctot-%offset%)
                 1))
        (setf nrp1 (f2cl-lib:int-add nr sqre))
        (cond
          ((> ktemp 1)
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i k) nil)
             (tagbody
               (setf (f2cl-lib:fref q-%data%
                                    (i ktemp)
                                    ((1 ldq) (1 *))
                                    q-%offset%)
                       (f2cl-lib:fref q-%data%
                                      (i 1)
                                      ((1 ldq) (1 *))
                                      q-%offset%))
              label130))
           (f2cl-lib:fdo (i nlp2 (f2cl-lib:int-add i 1))
                         ((> i m) nil)
             (tagbody
               (setf (f2cl-lib:fref vt2-%data%
                                    (ktemp i)
                                    ((1 ldvt2) (1 *))
                                    vt2-%offset%)
                       (f2cl-lib:fref vt2-%data%
                                      (1 i)
                                      ((1 ldvt2) (1 *))
                                      vt2-%offset%))
              label140))))
        (setf ctemp
                (f2cl-lib:int-add 1
                                  (f2cl-lib:fref ctot-%data%
                                                 (2)
                                                 ((1 *))
                                                 ctot-%offset%)
                                  (f2cl-lib:fref ctot-%data%
                                                 (3)
                                                 ((1 *))
                                                 ctot-%offset%)))
        (dgemm "N" "N" k nrp1 ctemp one
         (f2cl-lib:array-slice q-%data%
                               double-float
                               (1 ktemp)
                               ((1 ldq) (1 *))
                               q-%offset%)
         ldq
         (f2cl-lib:array-slice vt2-%data%
                               double-float
                               (ktemp nlp2)
                               ((1 ldvt2) (1 *))
                               vt2-%offset%)
         ldvt2 zero
         (f2cl-lib:array-slice vt-%data%
                               double-float
                               (1 nlp2)
                               ((1 ldvt) (1 *))
                               vt-%offset%)
         ldvt)
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
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasd3
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil nil nil nil nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlacpy fortran-to-lisp::dgemm
                    fortran-to-lisp::dlasd4 fortran-to-lisp::dlascl
                    fortran-to-lisp::dnrm2 fortran-to-lisp::dlamc3
                    fortran-to-lisp::dcopy fortran-to-lisp::xerbla))))

