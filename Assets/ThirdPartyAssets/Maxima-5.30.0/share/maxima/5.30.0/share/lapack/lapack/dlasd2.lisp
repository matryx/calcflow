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


(let* ((zero 0.0) (one 1.0) (two 2.0) (eight 8.0))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (type (double-float 2.0 2.0) two)
           (type (double-float 8.0 8.0) eight)
           (ignorable zero one two eight))
  (defun dlasd2
         (nl nr sqre k d z alpha beta u ldu vt ldvt dsigma u2 ldu2 vt2 ldvt2
          idxp idx idxc idxq coltyp info)
    (declare (type (array f2cl-lib:integer4 (*)) coltyp idxq idxc idx idxp)
             (type (double-float) beta alpha)
             (type (array double-float (*)) vt2 u2 dsigma vt u z d)
             (type (f2cl-lib:integer4) info ldvt2 ldu2 ldvt ldu k sqre nr nl))
    (f2cl-lib:with-multi-array-data
        ((d double-float d-%data% d-%offset%)
         (z double-float z-%data% z-%offset%)
         (u double-float u-%data% u-%offset%)
         (vt double-float vt-%data% vt-%offset%)
         (dsigma double-float dsigma-%data% dsigma-%offset%)
         (u2 double-float u2-%data% u2-%offset%)
         (vt2 double-float vt2-%data% vt2-%offset%)
         (idxp f2cl-lib:integer4 idxp-%data% idxp-%offset%)
         (idx f2cl-lib:integer4 idx-%data% idx-%offset%)
         (idxc f2cl-lib:integer4 idxc-%data% idxc-%offset%)
         (idxq f2cl-lib:integer4 idxq-%data% idxq-%offset%)
         (coltyp f2cl-lib:integer4 coltyp-%data% coltyp-%offset%))
      (prog ((c 0.0) (eps 0.0) (hlftol 0.0) (s 0.0) (tau 0.0) (tol 0.0)
             (z1 0.0) (ct 0) (i 0) (idxi 0) (idxj 0) (idxjp 0) (j 0) (jp 0)
             (jprev 0) (k2 0) (m 0) (n 0) (nlp1 0) (nlp2 0)
             (ctot (make-array 4 :element-type 'f2cl-lib:integer4))
             (psm (make-array 4 :element-type 'f2cl-lib:integer4)))
        (declare (type (double-float) c eps hlftol s tau tol z1)
                 (type (f2cl-lib:integer4) ct i idxi idxj idxjp j jp jprev k2 m
                                           n nlp1 nlp2)
                 (type (array f2cl-lib:integer4 (4)) ctot psm))
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
        (cond
          ((< ldu n)
           (setf info -10))
          ((< ldvt m)
           (setf info -12))
          ((< ldu2 n)
           (setf info -15))
          ((< ldvt2 m)
           (setf info -17)))
        (cond
          ((/= info 0)
           (xerbla "DLASD2" (f2cl-lib:int-sub info))
           (go end_label)))
        (setf nlp1 (f2cl-lib:int-add nl 1))
        (setf nlp2 (f2cl-lib:int-add nl 2))
        (setf z1
                (* alpha
                   (f2cl-lib:fref vt-%data%
                                  (nlp1 nlp1)
                                  ((1 ldvt) (1 *))
                                  vt-%offset%)))
        (setf (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%) z1)
        (f2cl-lib:fdo (i nl (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                      ((> i 1) nil)
          (tagbody
            (setf (f2cl-lib:fref z-%data%
                                 ((f2cl-lib:int-add i 1))
                                 ((1 *))
                                 z-%offset%)
                    (* alpha
                       (f2cl-lib:fref vt-%data%
                                      (i nlp1)
                                      ((1 ldvt) (1 *))
                                      vt-%offset%)))
            (setf (f2cl-lib:fref d-%data%
                                 ((f2cl-lib:int-add i 1))
                                 ((1 *))
                                 d-%offset%)
                    (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
            (setf (f2cl-lib:fref idxq-%data%
                                 ((f2cl-lib:int-add i 1))
                                 ((1 *))
                                 idxq-%offset%)
                    (f2cl-lib:int-add
                     (f2cl-lib:fref idxq-%data% (i) ((1 *)) idxq-%offset%)
                     1))
           label10))
        (f2cl-lib:fdo (i nlp2 (f2cl-lib:int-add i 1))
                      ((> i m) nil)
          (tagbody
            (setf (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                    (* beta
                       (f2cl-lib:fref vt-%data%
                                      (i nlp2)
                                      ((1 ldvt) (1 *))
                                      vt-%offset%)))
           label20))
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i nlp1) nil)
          (tagbody
            (setf (f2cl-lib:fref coltyp-%data% (i) ((1 *)) coltyp-%offset%) 1)
           label30))
        (f2cl-lib:fdo (i nlp2 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf (f2cl-lib:fref coltyp-%data% (i) ((1 *)) coltyp-%offset%) 2)
           label40))
        (f2cl-lib:fdo (i nlp2 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf (f2cl-lib:fref idxq-%data% (i) ((1 *)) idxq-%offset%)
                    (f2cl-lib:int-add
                     (f2cl-lib:fref idxq-%data% (i) ((1 *)) idxq-%offset%)
                     nlp1))
           label50))
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf (f2cl-lib:fref dsigma-%data% (i) ((1 *)) dsigma-%offset%)
                    (f2cl-lib:fref d-%data%
                                   ((f2cl-lib:fref idxq (i) ((1 *))))
                                   ((1 *))
                                   d-%offset%))
            (setf (f2cl-lib:fref u2-%data% (i 1) ((1 ldu2) (1 *)) u2-%offset%)
                    (f2cl-lib:fref z-%data%
                                   ((f2cl-lib:fref idxq (i) ((1 *))))
                                   ((1 *))
                                   z-%offset%))
            (setf (f2cl-lib:fref idxc-%data% (i) ((1 *)) idxc-%offset%)
                    (f2cl-lib:fref coltyp-%data%
                                   ((f2cl-lib:fref idxq (i) ((1 *))))
                                   ((1 *))
                                   coltyp-%offset%))
           label60))
        (dlamrg nl nr
         (f2cl-lib:array-slice dsigma-%data%
                               double-float
                               (2)
                               ((1 *))
                               dsigma-%offset%)
         1 1
         (f2cl-lib:array-slice idx-%data%
                               f2cl-lib:integer4
                               (2)
                               ((1 *))
                               idx-%offset%))
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf idxi
                    (f2cl-lib:int-add 1
                                      (f2cl-lib:fref idx-%data%
                                                     (i)
                                                     ((1 *))
                                                     idx-%offset%)))
            (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                    (f2cl-lib:fref dsigma-%data%
                                   (idxi)
                                   ((1 *))
                                   dsigma-%offset%))
            (setf (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                    (f2cl-lib:fref u2-%data%
                                   (idxi 1)
                                   ((1 ldu2) (1 *))
                                   u2-%offset%))
            (setf (f2cl-lib:fref coltyp-%data% (i) ((1 *)) coltyp-%offset%)
                    (f2cl-lib:fref idxc-%data% (idxi) ((1 *)) idxc-%offset%))
           label70))
        (setf eps (dlamch "Epsilon"))
        (setf tol (max (abs alpha) (abs beta)))
        (setf tol
                (* eight
                   eps
                   (max (abs (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%))
                        tol)))
        (setf k 1)
        (setf k2 (f2cl-lib:int-add n 1))
        (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (cond
              ((<= (abs (f2cl-lib:fref z (j) ((1 *)))) tol)
               (setf k2 (f2cl-lib:int-sub k2 1))
               (setf (f2cl-lib:fref idxp-%data% (k2) ((1 *)) idxp-%offset%) j)
               (setf (f2cl-lib:fref coltyp-%data% (j) ((1 *)) coltyp-%offset%)
                       4)
               (if (= j n) (go label120)))
              (t
               (setf jprev j)
               (go label90)))
           label80))
       label90
        (setf j jprev)
       label100
        (setf j (f2cl-lib:int-add j 1))
        (if (> j n) (go label110))
        (cond
          ((<= (abs (f2cl-lib:fref z (j) ((1 *)))) tol)
           (setf k2 (f2cl-lib:int-sub k2 1))
           (setf (f2cl-lib:fref idxp-%data% (k2) ((1 *)) idxp-%offset%) j)
           (setf (f2cl-lib:fref coltyp-%data% (j) ((1 *)) coltyp-%offset%) 4))
          (t
           (cond
             ((<=
               (abs
                (+ (f2cl-lib:fref d (j) ((1 *)))
                   (- (f2cl-lib:fref d (jprev) ((1 *))))))
               tol)
              (setf s (f2cl-lib:fref z-%data% (jprev) ((1 *)) z-%offset%))
              (setf c (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%))
              (setf tau (dlapy2 c s))
              (setf c (/ c tau))
              (setf s (/ (- s) tau))
              (setf (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%) tau)
              (setf (f2cl-lib:fref z-%data% (jprev) ((1 *)) z-%offset%) zero)
              (setf idxjp
                      (f2cl-lib:fref idxq-%data%
                                     ((f2cl-lib:int-add
                                       (f2cl-lib:fref idx (jprev) ((1 *)))
                                       1))
                                     ((1 *))
                                     idxq-%offset%))
              (setf idxj
                      (f2cl-lib:fref idxq-%data%
                                     ((f2cl-lib:int-add
                                       (f2cl-lib:fref idx (j) ((1 *)))
                                       1))
                                     ((1 *))
                                     idxq-%offset%))
              (cond
                ((<= idxjp nlp1)
                 (setf idxjp (f2cl-lib:int-sub idxjp 1))))
              (cond
                ((<= idxj nlp1)
                 (setf idxj (f2cl-lib:int-sub idxj 1))))
              (drot n
               (f2cl-lib:array-slice u-%data%
                                     double-float
                                     (1 idxjp)
                                     ((1 ldu) (1 *))
                                     u-%offset%)
               1
               (f2cl-lib:array-slice u-%data%
                                     double-float
                                     (1 idxj)
                                     ((1 ldu) (1 *))
                                     u-%offset%)
               1 c s)
              (drot m
               (f2cl-lib:array-slice vt-%data%
                                     double-float
                                     (idxjp 1)
                                     ((1 ldvt) (1 *))
                                     vt-%offset%)
               ldvt
               (f2cl-lib:array-slice vt-%data%
                                     double-float
                                     (idxj 1)
                                     ((1 ldvt) (1 *))
                                     vt-%offset%)
               ldvt c s)
              (cond
                ((/= (f2cl-lib:fref coltyp (j) ((1 *)))
                     (f2cl-lib:fref coltyp (jprev) ((1 *))))
                 (setf (f2cl-lib:fref coltyp-%data%
                                      (j)
                                      ((1 *))
                                      coltyp-%offset%)
                         3)))
              (setf (f2cl-lib:fref coltyp-%data%
                                   (jprev)
                                   ((1 *))
                                   coltyp-%offset%)
                      4)
              (setf k2 (f2cl-lib:int-sub k2 1))
              (setf (f2cl-lib:fref idxp-%data% (k2) ((1 *)) idxp-%offset%)
                      jprev)
              (setf jprev j))
             (t
              (setf k (f2cl-lib:int-add k 1))
              (setf (f2cl-lib:fref u2-%data%
                                   (k 1)
                                   ((1 ldu2) (1 *))
                                   u2-%offset%)
                      (f2cl-lib:fref z-%data% (jprev) ((1 *)) z-%offset%))
              (setf (f2cl-lib:fref dsigma-%data% (k) ((1 *)) dsigma-%offset%)
                      (f2cl-lib:fref d-%data% (jprev) ((1 *)) d-%offset%))
              (setf (f2cl-lib:fref idxp-%data% (k) ((1 *)) idxp-%offset%) jprev)
              (setf jprev j)))))
        (go label100)
       label110
        (setf k (f2cl-lib:int-add k 1))
        (setf (f2cl-lib:fref u2-%data% (k 1) ((1 ldu2) (1 *)) u2-%offset%)
                (f2cl-lib:fref z-%data% (jprev) ((1 *)) z-%offset%))
        (setf (f2cl-lib:fref dsigma-%data% (k) ((1 *)) dsigma-%offset%)
                (f2cl-lib:fref d-%data% (jprev) ((1 *)) d-%offset%))
        (setf (f2cl-lib:fref idxp-%data% (k) ((1 *)) idxp-%offset%) jprev)
       label120
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j 4) nil)
          (tagbody (setf (f2cl-lib:fref ctot (j) ((1 4))) 0) label130))
        (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf ct (f2cl-lib:fref coltyp-%data% (j) ((1 *)) coltyp-%offset%))
            (setf (f2cl-lib:fref ctot (ct) ((1 4)))
                    (f2cl-lib:int-add (f2cl-lib:fref ctot (ct) ((1 4))) 1))
           label140))
        (setf (f2cl-lib:fref psm (1) ((1 4))) 2)
        (setf (f2cl-lib:fref psm (2) ((1 4)))
                (f2cl-lib:int-add 2 (f2cl-lib:fref ctot (1) ((1 4)))))
        (setf (f2cl-lib:fref psm (3) ((1 4)))
                (f2cl-lib:int-add (f2cl-lib:fref psm (2) ((1 4)))
                                  (f2cl-lib:fref ctot (2) ((1 4)))))
        (setf (f2cl-lib:fref psm (4) ((1 4)))
                (f2cl-lib:int-add (f2cl-lib:fref psm (3) ((1 4)))
                                  (f2cl-lib:fref ctot (3) ((1 4)))))
        (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf jp (f2cl-lib:fref idxp-%data% (j) ((1 *)) idxp-%offset%))
            (setf ct
                    (f2cl-lib:fref coltyp-%data% (jp) ((1 *)) coltyp-%offset%))
            (setf (f2cl-lib:fref idxc-%data%
                                 ((f2cl-lib:fref psm (ct) ((1 4))))
                                 ((1 *))
                                 idxc-%offset%)
                    j)
            (setf (f2cl-lib:fref psm (ct) ((1 4)))
                    (f2cl-lib:int-add (f2cl-lib:fref psm (ct) ((1 4))) 1))
           label150))
        (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf jp (f2cl-lib:fref idxp-%data% (j) ((1 *)) idxp-%offset%))
            (setf (f2cl-lib:fref dsigma-%data% (j) ((1 *)) dsigma-%offset%)
                    (f2cl-lib:fref d-%data% (jp) ((1 *)) d-%offset%))
            (setf idxj
                    (f2cl-lib:fref idxq-%data%
                                   ((f2cl-lib:int-add
                                     (f2cl-lib:fref idx
                                                    ((f2cl-lib:fref idxp
                                                                    ((f2cl-lib:fref
                                                                      idxc
                                                                      (j)
                                                                      ((1 *))))
                                                                    ((1 *))))
                                                    ((1 *)))
                                     1))
                                   ((1 *))
                                   idxq-%offset%))
            (cond
              ((<= idxj nlp1)
               (setf idxj (f2cl-lib:int-sub idxj 1))))
            (dcopy n
             (f2cl-lib:array-slice u-%data%
                                   double-float
                                   (1 idxj)
                                   ((1 ldu) (1 *))
                                   u-%offset%)
             1
             (f2cl-lib:array-slice u2-%data%
                                   double-float
                                   (1 j)
                                   ((1 ldu2) (1 *))
                                   u2-%offset%)
             1)
            (dcopy m
             (f2cl-lib:array-slice vt-%data%
                                   double-float
                                   (idxj 1)
                                   ((1 ldvt) (1 *))
                                   vt-%offset%)
             ldvt
             (f2cl-lib:array-slice vt2-%data%
                                   double-float
                                   (j 1)
                                   ((1 ldvt2) (1 *))
                                   vt2-%offset%)
             ldvt2)
           label160))
        (setf (f2cl-lib:fref dsigma-%data% (1) ((1 *)) dsigma-%offset%) zero)
        (setf hlftol (/ tol two))
        (if
         (<= (abs (f2cl-lib:fref dsigma-%data% (2) ((1 *)) dsigma-%offset%))
             hlftol)
         (setf (f2cl-lib:fref dsigma-%data% (2) ((1 *)) dsigma-%offset%)
                 hlftol))
        (cond
          ((> m n)
           (setf (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%)
                   (dlapy2 z1 (f2cl-lib:fref z-%data% (m) ((1 *)) z-%offset%)))
           (cond
             ((<= (f2cl-lib:fref z (1) ((1 *))) tol)
              (setf c one)
              (setf s zero)
              (setf (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%) tol))
             (t
              (setf c (/ z1 (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%)))
              (setf s
                      (/ (f2cl-lib:fref z-%data% (m) ((1 *)) z-%offset%)
                         (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%))))))
          (t
           (cond
             ((<= (abs z1) tol)
              (setf (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%) tol))
             (t
              (setf (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%) z1)))))
        (dcopy (f2cl-lib:int-sub k 1)
         (f2cl-lib:array-slice u2-%data%
                               double-float
                               (2 1)
                               ((1 ldu2) (1 *))
                               u2-%offset%)
         1 (f2cl-lib:array-slice z-%data% double-float (2) ((1 *)) z-%offset%)
         1)
        (dlaset "A" n 1 zero zero u2 ldu2)
        (setf (f2cl-lib:fref u2-%data% (nlp1 1) ((1 ldu2) (1 *)) u2-%offset%)
                one)
        (cond
          ((> m n)
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i nlp1) nil)
             (tagbody
               (setf (f2cl-lib:fref vt-%data%
                                    (m i)
                                    ((1 ldvt) (1 *))
                                    vt-%offset%)
                       (* (- s)
                          (f2cl-lib:fref vt-%data%
                                         (nlp1 i)
                                         ((1 ldvt) (1 *))
                                         vt-%offset%)))
               (setf (f2cl-lib:fref vt2-%data%
                                    (1 i)
                                    ((1 ldvt2) (1 *))
                                    vt2-%offset%)
                       (* c
                          (f2cl-lib:fref vt-%data%
                                         (nlp1 i)
                                         ((1 ldvt) (1 *))
                                         vt-%offset%)))
              label170))
           (f2cl-lib:fdo (i nlp2 (f2cl-lib:int-add i 1))
                         ((> i m) nil)
             (tagbody
               (setf (f2cl-lib:fref vt2-%data%
                                    (1 i)
                                    ((1 ldvt2) (1 *))
                                    vt2-%offset%)
                       (* s
                          (f2cl-lib:fref vt-%data%
                                         (m i)
                                         ((1 ldvt) (1 *))
                                         vt-%offset%)))
               (setf (f2cl-lib:fref vt-%data%
                                    (m i)
                                    ((1 ldvt) (1 *))
                                    vt-%offset%)
                       (* c
                          (f2cl-lib:fref vt-%data%
                                         (m i)
                                         ((1 ldvt) (1 *))
                                         vt-%offset%)))
              label180)))
          (t
           (dcopy m
            (f2cl-lib:array-slice vt-%data%
                                  double-float
                                  (nlp1 1)
                                  ((1 ldvt) (1 *))
                                  vt-%offset%)
            ldvt
            (f2cl-lib:array-slice vt2-%data%
                                  double-float
                                  (1 1)
                                  ((1 ldvt2) (1 *))
                                  vt2-%offset%)
            ldvt2)))
        (cond
          ((> m n)
           (dcopy m
            (f2cl-lib:array-slice vt-%data%
                                  double-float
                                  (m 1)
                                  ((1 ldvt) (1 *))
                                  vt-%offset%)
            ldvt
            (f2cl-lib:array-slice vt2-%data%
                                  double-float
                                  (m 1)
                                  ((1 ldvt2) (1 *))
                                  vt2-%offset%)
            ldvt2)))
        (cond
          ((> n k)
           (dcopy (f2cl-lib:int-sub n k)
            (f2cl-lib:array-slice dsigma-%data%
                                  double-float
                                  ((+ k 1))
                                  ((1 *))
                                  dsigma-%offset%)
            1
            (f2cl-lib:array-slice d-%data%
                                  double-float
                                  ((+ k 1))
                                  ((1 *))
                                  d-%offset%)
            1)
           (dlacpy "A" n (f2cl-lib:int-sub n k)
            (f2cl-lib:array-slice u2-%data%
                                  double-float
                                  (1 (f2cl-lib:int-add k 1))
                                  ((1 ldu2) (1 *))
                                  u2-%offset%)
            ldu2
            (f2cl-lib:array-slice u-%data%
                                  double-float
                                  (1 (f2cl-lib:int-add k 1))
                                  ((1 ldu) (1 *))
                                  u-%offset%)
            ldu)
           (dlacpy "A" (f2cl-lib:int-sub n k) m
            (f2cl-lib:array-slice vt2-%data%
                                  double-float
                                  ((+ k 1) 1)
                                  ((1 ldvt2) (1 *))
                                  vt2-%offset%)
            ldvt2
            (f2cl-lib:array-slice vt-%data%
                                  double-float
                                  ((+ k 1) 1)
                                  ((1 ldvt) (1 *))
                                  vt-%offset%)
            ldvt)))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j 4) nil)
          (tagbody
            (setf (f2cl-lib:fref coltyp-%data% (j) ((1 *)) coltyp-%offset%)
                    (f2cl-lib:fref ctot (j) ((1 4))))
           label190))
        (go end_label)
       end_label
        (return
         (values nil
                 nil
                 nil
                 k
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
  (setf (gethash 'fortran-to-lisp::dlasd2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (double-float) (double-float) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil fortran-to-lisp::k nil nil nil nil nil
                            nil nil nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlacpy fortran-to-lisp::dlaset
                    fortran-to-lisp::dcopy fortran-to-lisp::drot
                    fortran-to-lisp::dlapy2 fortran-to-lisp::dlamch
                    fortran-to-lisp::dlamrg fortran-to-lisp::xerbla))))

