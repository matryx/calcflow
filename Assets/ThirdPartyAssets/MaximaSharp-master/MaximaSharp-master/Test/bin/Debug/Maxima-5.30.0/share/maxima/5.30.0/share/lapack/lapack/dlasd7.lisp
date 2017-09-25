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
  (defun dlasd7
         (icompq nl nr sqre k d z zw vf vfw vl vlw alpha beta dsigma idx idxp
          idxq perm givptr givcol ldgcol givnum ldgnum c s info)
    (declare (type (array f2cl-lib:integer4 (*)) givcol perm idxq idxp idx)
             (type (double-float) s c beta alpha)
             (type (array double-float (*)) givnum dsigma vlw vl vfw vf zw z d)
             (type (f2cl-lib:integer4) info ldgnum ldgcol givptr k sqre nr nl
                                       icompq))
    (f2cl-lib:with-multi-array-data
        ((d double-float d-%data% d-%offset%)
         (z double-float z-%data% z-%offset%)
         (zw double-float zw-%data% zw-%offset%)
         (vf double-float vf-%data% vf-%offset%)
         (vfw double-float vfw-%data% vfw-%offset%)
         (vl double-float vl-%data% vl-%offset%)
         (vlw double-float vlw-%data% vlw-%offset%)
         (dsigma double-float dsigma-%data% dsigma-%offset%)
         (givnum double-float givnum-%data% givnum-%offset%)
         (idx f2cl-lib:integer4 idx-%data% idx-%offset%)
         (idxp f2cl-lib:integer4 idxp-%data% idxp-%offset%)
         (idxq f2cl-lib:integer4 idxq-%data% idxq-%offset%)
         (perm f2cl-lib:integer4 perm-%data% perm-%offset%)
         (givcol f2cl-lib:integer4 givcol-%data% givcol-%offset%))
      (prog ((eps 0.0) (hlftol 0.0) (tau 0.0) (tol 0.0) (z1 0.0) (i 0) (idxi 0)
             (idxj 0) (idxjp 0) (j 0) (jp 0) (jprev 0) (k2 0) (m 0) (n 0)
             (nlp1 0) (nlp2 0))
        (declare (type (double-float) eps hlftol tau tol z1)
                 (type (f2cl-lib:integer4) i idxi idxj idxjp j jp jprev k2 m n
                                           nlp1 nlp2))
        (setf info 0)
        (setf n (f2cl-lib:int-add nl nr 1))
        (setf m (f2cl-lib:int-add n sqre))
        (cond
          ((or (< icompq 0) (> icompq 1))
           (setf info -1))
          ((< nl 1)
           (setf info -2))
          ((< nr 1)
           (setf info -3))
          ((or (< sqre 0) (> sqre 1))
           (setf info -4))
          ((< ldgcol n)
           (setf info -22))
          ((< ldgnum n)
           (setf info -24)))
        (cond
          ((/= info 0)
           (xerbla "DLASD7" (f2cl-lib:int-sub info))
           (go end_label)))
        (setf nlp1 (f2cl-lib:int-add nl 1))
        (setf nlp2 (f2cl-lib:int-add nl 2))
        (cond
          ((= icompq 1)
           (setf givptr 0)))
        (setf z1
                (* alpha (f2cl-lib:fref vl-%data% (nlp1) ((1 *)) vl-%offset%)))
        (setf (f2cl-lib:fref vl-%data% (nlp1) ((1 *)) vl-%offset%) zero)
        (setf tau (f2cl-lib:fref vf-%data% (nlp1) ((1 *)) vf-%offset%))
        (f2cl-lib:fdo (i nl (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                      ((> i 1) nil)
          (tagbody
            (setf (f2cl-lib:fref z-%data%
                                 ((f2cl-lib:int-add i 1))
                                 ((1 *))
                                 z-%offset%)
                    (* alpha
                       (f2cl-lib:fref vl-%data% (i) ((1 *)) vl-%offset%)))
            (setf (f2cl-lib:fref vl-%data% (i) ((1 *)) vl-%offset%) zero)
            (setf (f2cl-lib:fref vf-%data%
                                 ((f2cl-lib:int-add i 1))
                                 ((1 *))
                                 vf-%offset%)
                    (f2cl-lib:fref vf-%data% (i) ((1 *)) vf-%offset%))
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
        (setf (f2cl-lib:fref vf-%data% (1) ((1 *)) vf-%offset%) tau)
        (f2cl-lib:fdo (i nlp2 (f2cl-lib:int-add i 1))
                      ((> i m) nil)
          (tagbody
            (setf (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                    (* beta (f2cl-lib:fref vf-%data% (i) ((1 *)) vf-%offset%)))
            (setf (f2cl-lib:fref vf-%data% (i) ((1 *)) vf-%offset%) zero)
           label20))
        (f2cl-lib:fdo (i nlp2 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf (f2cl-lib:fref idxq-%data% (i) ((1 *)) idxq-%offset%)
                    (f2cl-lib:int-add
                     (f2cl-lib:fref idxq-%data% (i) ((1 *)) idxq-%offset%)
                     nlp1))
           label30))
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf (f2cl-lib:fref dsigma-%data% (i) ((1 *)) dsigma-%offset%)
                    (f2cl-lib:fref d-%data%
                                   ((f2cl-lib:fref idxq (i) ((1 *))))
                                   ((1 *))
                                   d-%offset%))
            (setf (f2cl-lib:fref zw-%data% (i) ((1 *)) zw-%offset%)
                    (f2cl-lib:fref z-%data%
                                   ((f2cl-lib:fref idxq (i) ((1 *))))
                                   ((1 *))
                                   z-%offset%))
            (setf (f2cl-lib:fref vfw-%data% (i) ((1 *)) vfw-%offset%)
                    (f2cl-lib:fref vf-%data%
                                   ((f2cl-lib:fref idxq (i) ((1 *))))
                                   ((1 *))
                                   vf-%offset%))
            (setf (f2cl-lib:fref vlw-%data% (i) ((1 *)) vlw-%offset%)
                    (f2cl-lib:fref vl-%data%
                                   ((f2cl-lib:fref idxq (i) ((1 *))))
                                   ((1 *))
                                   vl-%offset%))
           label40))
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
                    (f2cl-lib:fref zw-%data% (idxi) ((1 *)) zw-%offset%))
            (setf (f2cl-lib:fref vf-%data% (i) ((1 *)) vf-%offset%)
                    (f2cl-lib:fref vfw-%data% (idxi) ((1 *)) vfw-%offset%))
            (setf (f2cl-lib:fref vl-%data% (i) ((1 *)) vl-%offset%)
                    (f2cl-lib:fref vlw-%data% (idxi) ((1 *)) vlw-%offset%))
           label50))
        (setf eps (dlamch "Epsilon"))
        (setf tol (max (abs alpha) (abs beta)))
        (setf tol
                (* eight
                   eight
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
               (if (= j n) (go label100)))
              (t
               (setf jprev j)
               (go label70)))
           label60))
       label70
        (setf j jprev)
       label80
        (setf j (f2cl-lib:int-add j 1))
        (if (> j n) (go label90))
        (cond
          ((<= (abs (f2cl-lib:fref z (j) ((1 *)))) tol)
           (setf k2 (f2cl-lib:int-sub k2 1))
           (setf (f2cl-lib:fref idxp-%data% (k2) ((1 *)) idxp-%offset%) j))
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
              (setf (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%) tau)
              (setf (f2cl-lib:fref z-%data% (jprev) ((1 *)) z-%offset%) zero)
              (setf c (/ c tau))
              (setf s (/ (- s) tau))
              (cond
                ((= icompq 1)
                 (setf givptr (f2cl-lib:int-add givptr 1))
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
                 (setf (f2cl-lib:fref givcol-%data%
                                      (givptr 2)
                                      ((1 ldgcol) (1 *))
                                      givcol-%offset%)
                         idxjp)
                 (setf (f2cl-lib:fref givcol-%data%
                                      (givptr 1)
                                      ((1 ldgcol) (1 *))
                                      givcol-%offset%)
                         idxj)
                 (setf (f2cl-lib:fref givnum-%data%
                                      (givptr 2)
                                      ((1 ldgnum) (1 *))
                                      givnum-%offset%)
                         c)
                 (setf (f2cl-lib:fref givnum-%data%
                                      (givptr 1)
                                      ((1 ldgnum) (1 *))
                                      givnum-%offset%)
                         s)))
              (drot 1
               (f2cl-lib:array-slice vf-%data%
                                     double-float
                                     (jprev)
                                     ((1 *))
                                     vf-%offset%)
               1
               (f2cl-lib:array-slice vf-%data%
                                     double-float
                                     (j)
                                     ((1 *))
                                     vf-%offset%)
               1 c s)
              (drot 1
               (f2cl-lib:array-slice vl-%data%
                                     double-float
                                     (jprev)
                                     ((1 *))
                                     vl-%offset%)
               1
               (f2cl-lib:array-slice vl-%data%
                                     double-float
                                     (j)
                                     ((1 *))
                                     vl-%offset%)
               1 c s)
              (setf k2 (f2cl-lib:int-sub k2 1))
              (setf (f2cl-lib:fref idxp-%data% (k2) ((1 *)) idxp-%offset%)
                      jprev)
              (setf jprev j))
             (t
              (setf k (f2cl-lib:int-add k 1))
              (setf (f2cl-lib:fref zw-%data% (k) ((1 *)) zw-%offset%)
                      (f2cl-lib:fref z-%data% (jprev) ((1 *)) z-%offset%))
              (setf (f2cl-lib:fref dsigma-%data% (k) ((1 *)) dsigma-%offset%)
                      (f2cl-lib:fref d-%data% (jprev) ((1 *)) d-%offset%))
              (setf (f2cl-lib:fref idxp-%data% (k) ((1 *)) idxp-%offset%) jprev)
              (setf jprev j)))))
        (go label80)
       label90
        (setf k (f2cl-lib:int-add k 1))
        (setf (f2cl-lib:fref zw-%data% (k) ((1 *)) zw-%offset%)
                (f2cl-lib:fref z-%data% (jprev) ((1 *)) z-%offset%))
        (setf (f2cl-lib:fref dsigma-%data% (k) ((1 *)) dsigma-%offset%)
                (f2cl-lib:fref d-%data% (jprev) ((1 *)) d-%offset%))
        (setf (f2cl-lib:fref idxp-%data% (k) ((1 *)) idxp-%offset%) jprev)
       label100
        (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf jp (f2cl-lib:fref idxp-%data% (j) ((1 *)) idxp-%offset%))
            (setf (f2cl-lib:fref dsigma-%data% (j) ((1 *)) dsigma-%offset%)
                    (f2cl-lib:fref d-%data% (jp) ((1 *)) d-%offset%))
            (setf (f2cl-lib:fref vfw-%data% (j) ((1 *)) vfw-%offset%)
                    (f2cl-lib:fref vf-%data% (jp) ((1 *)) vf-%offset%))
            (setf (f2cl-lib:fref vlw-%data% (j) ((1 *)) vlw-%offset%)
                    (f2cl-lib:fref vl-%data% (jp) ((1 *)) vl-%offset%))
           label110))
        (cond
          ((= icompq 1)
           (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (setf jp (f2cl-lib:fref idxp-%data% (j) ((1 *)) idxp-%offset%))
               (setf (f2cl-lib:fref perm-%data% (j) ((1 *)) perm-%offset%)
                       (f2cl-lib:fref idxq-%data%
                                      ((f2cl-lib:int-add
                                        (f2cl-lib:fref idx (jp) ((1 *)))
                                        1))
                                      ((1 *))
                                      idxq-%offset%))
               (cond
                 ((<= (f2cl-lib:fref perm (j) ((1 *))) nlp1)
                  (setf (f2cl-lib:fref perm-%data% (j) ((1 *)) perm-%offset%)
                          (f2cl-lib:int-sub
                           (f2cl-lib:fref perm-%data%
                                          (j)
                                          ((1 *))
                                          perm-%offset%)
                           1))))
              label120))))
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
                      (/ (- (f2cl-lib:fref z-%data% (m) ((1 *)) z-%offset%))
                         (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%)))))
           (drot 1
            (f2cl-lib:array-slice vf-%data%
                                  double-float
                                  (m)
                                  ((1 *))
                                  vf-%offset%)
            1
            (f2cl-lib:array-slice vf-%data%
                                  double-float
                                  (1)
                                  ((1 *))
                                  vf-%offset%)
            1 c s)
           (drot 1
            (f2cl-lib:array-slice vl-%data%
                                  double-float
                                  (m)
                                  ((1 *))
                                  vl-%offset%)
            1
            (f2cl-lib:array-slice vl-%data%
                                  double-float
                                  (1)
                                  ((1 *))
                                  vl-%offset%)
            1 c s))
          (t
           (cond
             ((<= (abs z1) tol)
              (setf (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%) tol))
             (t
              (setf (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%) z1)))))
        (dcopy (f2cl-lib:int-sub k 1)
         (f2cl-lib:array-slice zw-%data% double-float (2) ((1 *)) zw-%offset%)
         1 (f2cl-lib:array-slice z-%data% double-float (2) ((1 *)) z-%offset%)
         1)
        (dcopy (f2cl-lib:int-sub n 1)
         (f2cl-lib:array-slice vfw-%data%
                               double-float
                               (2)
                               ((1 *))
                               vfw-%offset%)
         1
         (f2cl-lib:array-slice vf-%data% double-float (2) ((1 *)) vf-%offset%)
         1)
        (dcopy (f2cl-lib:int-sub n 1)
         (f2cl-lib:array-slice vlw-%data%
                               double-float
                               (2)
                               ((1 *))
                               vlw-%offset%)
         1
         (f2cl-lib:array-slice vl-%data% double-float (2) ((1 *)) vl-%offset%)
         1)
        (go end_label)
       end_label
        (return
         (values nil
                 nil
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
                 givptr
                 nil
                 nil
                 nil
                 nil
                 c
                 s
                 info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasd7
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (double-float) (double-float) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil fortran-to-lisp::k nil nil nil nil
                            nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::givptr nil nil nil nil
                            fortran-to-lisp::c fortran-to-lisp::s
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dcopy fortran-to-lisp::drot
                    fortran-to-lisp::dlapy2 fortran-to-lisp::dlamch
                    fortran-to-lisp::dlamrg fortran-to-lisp::xerbla))))

