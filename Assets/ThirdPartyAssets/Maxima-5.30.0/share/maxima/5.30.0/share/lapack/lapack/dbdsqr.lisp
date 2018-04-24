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


(let* ((zero 0.0)
       (one 1.0)
       (negone (- 1.0))
       (hndrth 0.01)
       (ten 10.0)
       (hndrd 100.0)
       (meigth (- 0.125))
       (maxitr 6))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (type (double-float) negone)
           (type (double-float 0.01 0.01) hndrth)
           (type (double-float 10.0 10.0) ten)
           (type (double-float 100.0 100.0) hndrd)
           (type (double-float) meigth)
           (type (f2cl-lib:integer4 6 6) maxitr)
           (ignorable zero one negone hndrth ten hndrd meigth maxitr))
  (defun dbdsqr (uplo n ncvt nru ncc d e vt ldvt u ldu c ldc work info)
    (declare (type (array double-float (*)) work c u vt e d)
             (type (f2cl-lib:integer4) info ldc ldu ldvt ncc nru ncvt n)
             (type (simple-string *) uplo))
    (f2cl-lib:with-multi-array-data
        ((uplo character uplo-%data% uplo-%offset%)
         (d double-float d-%data% d-%offset%)
         (e double-float e-%data% e-%offset%)
         (vt double-float vt-%data% vt-%offset%)
         (u double-float u-%data% u-%offset%)
         (c double-float c-%data% c-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((abse 0.0) (abss 0.0) (cosl 0.0) (cosr 0.0) (cs 0.0) (eps 0.0)
             (f 0.0) (g 0.0) (h 0.0) (mu 0.0) (oldcs 0.0) (oldsn 0.0) (r 0.0)
             (shift 0.0) (sigmn 0.0) (sigmx 0.0) (sinl 0.0) (sinr 0.0)
             (sll 0.0) (smax 0.0) (smin 0.0) (sminl 0.0) (sminlo 0.0)
             (sminoa 0.0) (sn 0.0) (thresh 0.0) (tol 0.0) (tolmul 0.0)
             (unfl 0.0) (i 0) (idir 0) (isub 0) (iter 0) (j 0) (ll 0) (lll 0)
             (m 0) (maxit 0) (nm1 0) (nm12 0) (nm13 0) (oldll 0) (oldm 0)
             (lower nil) (rotate nil))
        (declare (type (double-float) abse abss cosl cosr cs eps f g h mu oldcs
                                      oldsn r shift sigmn sigmx sinl sinr sll
                                      smax smin sminl sminlo sminoa sn thresh
                                      tol tolmul unfl)
                 (type (f2cl-lib:integer4) i idir isub iter j ll lll m maxit
                                           nm1 nm12 nm13 oldll oldm)
                 (type f2cl-lib:logical lower rotate))
        (setf info 0)
        (setf lower (lsame uplo "L"))
        (cond
          ((and (not (lsame uplo "U")) (not lower))
           (setf info -1))
          ((< n 0)
           (setf info -2))
          ((< ncvt 0)
           (setf info -3))
          ((< nru 0)
           (setf info -4))
          ((< ncc 0)
           (setf info -5))
          ((or (and (= ncvt 0) (< ldvt 1))
               (and (> ncvt 0)
                    (< ldvt
                       (max (the f2cl-lib:integer4 1)
                            (the f2cl-lib:integer4 n)))))
           (setf info -9))
          ((< ldu (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 nru)))
           (setf info -11))
          ((or (and (= ncc 0) (< ldc 1))
               (and (> ncc 0)
                    (< ldc
                       (max (the f2cl-lib:integer4 1)
                            (the f2cl-lib:integer4 n)))))
           (setf info -13)))
        (cond
          ((/= info 0)
           (xerbla "DBDSQR" (f2cl-lib:int-sub info))
           (go end_label)))
        (if (= n 0) (go end_label))
        (if (= n 1) (go label160))
        (setf rotate (or (> ncvt 0) (> nru 0) (> ncc 0)))
        (cond
          ((not rotate)
           (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
               (dlasq1 n d e work info)
             (declare (ignore var-0 var-1 var-2 var-3))
             (setf info var-4))
           (go end_label)))
        (setf nm1 (f2cl-lib:int-sub n 1))
        (setf nm12 (f2cl-lib:int-add nm1 nm1))
        (setf nm13 (f2cl-lib:int-add nm12 nm1))
        (setf idir 0)
        (setf eps (dlamch "Epsilon"))
        (setf unfl (dlamch "Safe minimum"))
        (cond
          (lower
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
             (tagbody
               (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                   (dlartg (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                    (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%) cs sn r)
                 (declare (ignore var-0 var-1))
                 (setf cs var-2)
                 (setf sn var-3)
                 (setf r var-4))
               (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%) r)
               (setf (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)
                       (* sn
                          (f2cl-lib:fref d-%data%
                                         ((f2cl-lib:int-add i 1))
                                         ((1 *))
                                         d-%offset%)))
               (setf (f2cl-lib:fref d-%data%
                                    ((f2cl-lib:int-add i 1))
                                    ((1 *))
                                    d-%offset%)
                       (* cs
                          (f2cl-lib:fref d-%data%
                                         ((f2cl-lib:int-add i 1))
                                         ((1 *))
                                         d-%offset%)))
               (setf (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%) cs)
               (setf (f2cl-lib:fref work-%data%
                                    ((f2cl-lib:int-add nm1 i))
                                    ((1 *))
                                    work-%offset%)
                       sn)
              label10))
           (if (> nru 0)
               (dlasr "R" "V" "F" nru n
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (1)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (n)
                                      ((1 *))
                                      work-%offset%)
                u ldu))
           (if (> ncc 0)
               (dlasr "L" "V" "F" n ncc
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (1)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (n)
                                      ((1 *))
                                      work-%offset%)
                c ldc))))
        (setf tolmul (max ten (min hndrd (expt eps meigth))))
        (setf tol (* tolmul eps))
        (setf smax zero)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf smax
                    (max smax
                         (abs
                          (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))))
           label20))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
          (tagbody
            (setf smax
                    (max smax
                         (abs
                          (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%))))
           label30))
        (setf sminl zero)
        (cond
          ((>= tol zero)
           (tagbody
             (setf sminoa
                     (abs (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%)))
             (if (= sminoa zero) (go label50))
             (setf mu sminoa)
             (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                           ((> i n) nil)
               (tagbody
                 (setf mu
                         (*
                          (abs (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
                          (/ mu
                             (+ mu
                                (abs
                                 (f2cl-lib:fref e-%data%
                                                ((f2cl-lib:int-sub i 1))
                                                ((1 *))
                                                e-%offset%))))))
                 (setf sminoa (min sminoa mu))
                 (if (= sminoa zero) (go label50))
                label40))
            label50
             (setf sminoa (/ sminoa (f2cl-lib:fsqrt (f2cl-lib:dble n))))
             (setf thresh (max (* tol sminoa) (* maxitr n n unfl)))))
          (t
           (setf thresh (max (* (abs tol) smax) (* maxitr n n unfl)))))
        (setf maxit (f2cl-lib:int-mul maxitr n n))
        (setf iter 0)
        (setf oldll -1)
        (setf oldm -1)
        (setf m n)
       label60
        (if (<= m 1) (go label160))
        (if (> iter maxit) (go label200))
        (if
         (and (< tol zero)
              (<= (abs (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%))
                  thresh))
         (setf (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%) zero))
        (setf smax (abs (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%)))
        (setf smin smax)
        (f2cl-lib:fdo (lll 1 (f2cl-lib:int-add lll 1))
                      ((> lll (f2cl-lib:int-add m (f2cl-lib:int-sub 1))) nil)
          (tagbody
            (setf ll (f2cl-lib:int-sub m lll))
            (setf abss (abs (f2cl-lib:fref d-%data% (ll) ((1 *)) d-%offset%)))
            (setf abse (abs (f2cl-lib:fref e-%data% (ll) ((1 *)) e-%offset%)))
            (if (and (< tol zero) (<= abss thresh))
                (setf (f2cl-lib:fref d-%data% (ll) ((1 *)) d-%offset%) zero))
            (if (<= abse thresh) (go label80))
            (setf smin (min smin abss))
            (setf smax (max smax abss abse))
           label70))
        (setf ll 0)
        (go label90)
       label80
        (setf (f2cl-lib:fref e-%data% (ll) ((1 *)) e-%offset%) zero)
        (cond
          ((= ll (f2cl-lib:int-add m (f2cl-lib:int-sub 1)))
           (setf m (f2cl-lib:int-sub m 1))
           (go label60)))
       label90
        (setf ll (f2cl-lib:int-add ll 1))
        (cond
          ((= ll (f2cl-lib:int-add m (f2cl-lib:int-sub 1)))
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
               (dlasv2
                (f2cl-lib:fref d-%data%
                               ((f2cl-lib:int-sub m 1))
                               ((1 *))
                               d-%offset%)
                (f2cl-lib:fref e-%data%
                               ((f2cl-lib:int-sub m 1))
                               ((1 *))
                               e-%offset%)
                (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%) sigmn sigmx
                sinr cosr sinl cosl)
             (declare (ignore var-0 var-1 var-2))
             (setf sigmn var-3)
             (setf sigmx var-4)
             (setf sinr var-5)
             (setf cosr var-6)
             (setf sinl var-7)
             (setf cosl var-8))
           (setf (f2cl-lib:fref d-%data%
                                ((f2cl-lib:int-sub m 1))
                                ((1 *))
                                d-%offset%)
                   sigmx)
           (setf (f2cl-lib:fref e-%data%
                                ((f2cl-lib:int-sub m 1))
                                ((1 *))
                                e-%offset%)
                   zero)
           (setf (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%) sigmn)
           (if (> ncvt 0)
               (drot ncvt
                (f2cl-lib:array-slice vt-%data%
                                      double-float
                                      ((+ m (f2cl-lib:int-sub 1)) 1)
                                      ((1 ldvt) (1 *))
                                      vt-%offset%)
                ldvt
                (f2cl-lib:array-slice vt-%data%
                                      double-float
                                      (m 1)
                                      ((1 ldvt) (1 *))
                                      vt-%offset%)
                ldvt cosr sinr))
           (if (> nru 0)
               (drot nru
                (f2cl-lib:array-slice u-%data%
                                      double-float
                                      (1 (f2cl-lib:int-sub m 1))
                                      ((1 ldu) (1 *))
                                      u-%offset%)
                1
                (f2cl-lib:array-slice u-%data%
                                      double-float
                                      (1 m)
                                      ((1 ldu) (1 *))
                                      u-%offset%)
                1 cosl sinl))
           (if (> ncc 0)
               (drot ncc
                (f2cl-lib:array-slice c-%data%
                                      double-float
                                      ((+ m (f2cl-lib:int-sub 1)) 1)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                ldc
                (f2cl-lib:array-slice c-%data%
                                      double-float
                                      (m 1)
                                      ((1 ldc) (1 *))
                                      c-%offset%)
                ldc cosl sinl))
           (setf m (f2cl-lib:int-sub m 2))
           (go label60)))
        (cond
          ((or (> ll oldm) (< m oldll))
           (cond
             ((>= (abs (f2cl-lib:fref d (ll) ((1 *))))
                  (abs (f2cl-lib:fref d (m) ((1 *)))))
              (setf idir 1))
             (t
              (setf idir 2)))))
        (cond
          ((= idir 1)
           (cond
             ((or
               (<=
                (abs
                 (f2cl-lib:fref e
                                ((f2cl-lib:int-add m (f2cl-lib:int-sub 1)))
                                ((1 *))))
                (* (abs tol) (abs (f2cl-lib:fref d (m) ((1 *))))))
               (and (< tol zero)
                    (<=
                     (abs
                      (f2cl-lib:fref e
                                     ((f2cl-lib:int-add m
                                                        (f2cl-lib:int-sub 1)))
                                     ((1 *))))
                     thresh)))
              (setf (f2cl-lib:fref e-%data%
                                   ((f2cl-lib:int-sub m 1))
                                   ((1 *))
                                   e-%offset%)
                      zero)
              (go label60)))
           (cond
             ((>= tol zero)
              (setf mu (abs (f2cl-lib:fref d-%data% (ll) ((1 *)) d-%offset%)))
              (setf sminl mu)
              (f2cl-lib:fdo (lll ll (f2cl-lib:int-add lll 1))
                            ((> lll (f2cl-lib:int-add m (f2cl-lib:int-sub 1)))
                             nil)
                (tagbody
                  (cond
                    ((<= (abs (f2cl-lib:fref e (lll) ((1 *)))) (* tol mu))
                     (setf (f2cl-lib:fref e-%data% (lll) ((1 *)) e-%offset%)
                             zero)
                     (go label60)))
                  (setf sminlo sminl)
                  (setf mu
                          (*
                           (abs
                            (f2cl-lib:fref d-%data%
                                           ((f2cl-lib:int-add lll 1))
                                           ((1 *))
                                           d-%offset%))
                           (/ mu
                              (+ mu
                                 (abs
                                  (f2cl-lib:fref e-%data%
                                                 (lll)
                                                 ((1 *))
                                                 e-%offset%))))))
                  (setf sminl (min sminl mu))
                 label100)))))
          (t
           (cond
             ((or
               (<= (abs (f2cl-lib:fref e (ll) ((1 *))))
                   (* (abs tol) (abs (f2cl-lib:fref d (ll) ((1 *))))))
               (and (< tol zero)
                    (<= (abs (f2cl-lib:fref e (ll) ((1 *)))) thresh)))
              (setf (f2cl-lib:fref e-%data% (ll) ((1 *)) e-%offset%) zero)
              (go label60)))
           (cond
             ((>= tol zero)
              (setf mu (abs (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%)))
              (setf sminl mu)
              (f2cl-lib:fdo (lll (f2cl-lib:int-add m (f2cl-lib:int-sub 1))
                             (f2cl-lib:int-add lll (f2cl-lib:int-sub 1)))
                            ((> lll ll) nil)
                (tagbody
                  (cond
                    ((<= (abs (f2cl-lib:fref e (lll) ((1 *)))) (* tol mu))
                     (setf (f2cl-lib:fref e-%data% (lll) ((1 *)) e-%offset%)
                             zero)
                     (go label60)))
                  (setf sminlo sminl)
                  (setf mu
                          (*
                           (abs
                            (f2cl-lib:fref d-%data% (lll) ((1 *)) d-%offset%))
                           (/ mu
                              (+ mu
                                 (abs
                                  (f2cl-lib:fref e-%data%
                                                 (lll)
                                                 ((1 *))
                                                 e-%offset%))))))
                  (setf sminl (min sminl mu))
                 label110))))))
        (setf oldll ll)
        (setf oldm m)
        (cond
          ((and (>= tol zero)
                (<= (* n tol (f2cl-lib:f2cl/ sminl smax))
                    (max eps (* hndrth tol))))
           (setf shift zero))
          (t
           (cond
             ((= idir 1)
              (setf sll (abs (f2cl-lib:fref d-%data% (ll) ((1 *)) d-%offset%)))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlas2
                   (f2cl-lib:fref d-%data%
                                  ((f2cl-lib:int-sub m 1))
                                  ((1 *))
                                  d-%offset%)
                   (f2cl-lib:fref e-%data%
                                  ((f2cl-lib:int-sub m 1))
                                  ((1 *))
                                  e-%offset%)
                   (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%) shift r)
                (declare (ignore var-0 var-1 var-2))
                (setf shift var-3)
                (setf r var-4)))
             (t
              (setf sll (abs (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%)))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlas2 (f2cl-lib:fref d-%data% (ll) ((1 *)) d-%offset%)
                   (f2cl-lib:fref e-%data% (ll) ((1 *)) e-%offset%)
                   (f2cl-lib:fref d-%data%
                                  ((f2cl-lib:int-add ll 1))
                                  ((1 *))
                                  d-%offset%)
                   shift r)
                (declare (ignore var-0 var-1 var-2))
                (setf shift var-3)
                (setf r var-4))))
           (cond
             ((> sll zero)
              (if (< (expt (/ shift sll) 2) eps) (setf shift zero))))))
        (setf iter (f2cl-lib:int-sub (f2cl-lib:int-add iter m) ll))
        (cond
          ((= shift zero)
           (cond
             ((= idir 1)
              (setf cs one)
              (setf oldcs one)
              (f2cl-lib:fdo (i ll (f2cl-lib:int-add i 1))
                            ((> i (f2cl-lib:int-add m (f2cl-lib:int-sub 1)))
                             nil)
                (tagbody
                  (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                      (dlartg
                       (* (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%) cs)
                       (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%) cs sn r)
                    (declare (ignore var-0 var-1))
                    (setf cs var-2)
                    (setf sn var-3)
                    (setf r var-4))
                  (if (> i ll)
                      (setf (f2cl-lib:fref e-%data%
                                           ((f2cl-lib:int-sub i 1))
                                           ((1 *))
                                           e-%offset%)
                              (* oldsn r)))
                  (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                      (dlartg (* oldcs r)
                       (*
                        (f2cl-lib:fref d-%data%
                                       ((f2cl-lib:int-add i 1))
                                       ((1 *))
                                       d-%offset%)
                        sn)
                       oldcs oldsn
                       (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
                    (declare (ignore var-0 var-1))
                    (setf oldcs var-2)
                    (setf oldsn var-3)
                    (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                            var-4))
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub i ll)
                                         1))
                                       ((1 *))
                                       work-%offset%)
                          cs)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub i ll)
                                         1
                                         nm1))
                                       ((1 *))
                                       work-%offset%)
                          sn)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub i ll)
                                         1
                                         nm12))
                                       ((1 *))
                                       work-%offset%)
                          oldcs)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub i ll)
                                         1
                                         nm13))
                                       ((1 *))
                                       work-%offset%)
                          oldsn)
                 label120))
              (setf h (* (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%) cs))
              (setf (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%) (* h oldcs))
              (setf (f2cl-lib:fref e-%data%
                                   ((f2cl-lib:int-sub m 1))
                                   ((1 *))
                                   e-%offset%)
                      (* h oldsn))
              (if (> ncvt 0)
                  (dlasr "L" "V" "F"
                   (f2cl-lib:int-add (f2cl-lib:int-sub m ll) 1) ncvt
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (1)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (n)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice vt-%data%
                                         double-float
                                         (ll 1)
                                         ((1 ldvt) (1 *))
                                         vt-%offset%)
                   ldvt))
              (if (> nru 0)
                  (dlasr "R" "V" "F" nru
                   (f2cl-lib:int-add (f2cl-lib:int-sub m ll) 1)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         ((+ nm12 1))
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         ((+ nm13 1))
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice u-%data%
                                         double-float
                                         (1 ll)
                                         ((1 ldu) (1 *))
                                         u-%offset%)
                   ldu))
              (if (> ncc 0)
                  (dlasr "L" "V" "F"
                   (f2cl-lib:int-add (f2cl-lib:int-sub m ll) 1) ncc
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         ((+ nm12 1))
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         ((+ nm13 1))
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice c-%data%
                                         double-float
                                         (ll 1)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                   ldc))
              (if
               (<=
                (abs
                 (f2cl-lib:fref e-%data%
                                ((f2cl-lib:int-sub m 1))
                                ((1 *))
                                e-%offset%))
                thresh)
               (setf (f2cl-lib:fref e-%data%
                                    ((f2cl-lib:int-sub m 1))
                                    ((1 *))
                                    e-%offset%)
                       zero)))
             (t
              (setf cs one)
              (setf oldcs one)
              (f2cl-lib:fdo (i m (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                            ((> i (f2cl-lib:int-add ll 1)) nil)
                (tagbody
                  (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                      (dlartg
                       (* (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%) cs)
                       (f2cl-lib:fref e-%data%
                                      ((f2cl-lib:int-sub i 1))
                                      ((1 *))
                                      e-%offset%)
                       cs sn r)
                    (declare (ignore var-0 var-1))
                    (setf cs var-2)
                    (setf sn var-3)
                    (setf r var-4))
                  (if (< i m)
                      (setf (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)
                              (* oldsn r)))
                  (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                      (dlartg (* oldcs r)
                       (*
                        (f2cl-lib:fref d-%data%
                                       ((f2cl-lib:int-sub i 1))
                                       ((1 *))
                                       d-%offset%)
                        sn)
                       oldcs oldsn
                       (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
                    (declare (ignore var-0 var-1))
                    (setf oldcs var-2)
                    (setf oldsn var-3)
                    (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                            var-4))
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-sub i ll))
                                       ((1 *))
                                       work-%offset%)
                          cs)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub i ll)
                                         nm1))
                                       ((1 *))
                                       work-%offset%)
                          (- sn))
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub i ll)
                                         nm12))
                                       ((1 *))
                                       work-%offset%)
                          oldcs)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub i ll)
                                         nm13))
                                       ((1 *))
                                       work-%offset%)
                          (- oldsn))
                 label130))
              (setf h (* (f2cl-lib:fref d-%data% (ll) ((1 *)) d-%offset%) cs))
              (setf (f2cl-lib:fref d-%data% (ll) ((1 *)) d-%offset%)
                      (* h oldcs))
              (setf (f2cl-lib:fref e-%data% (ll) ((1 *)) e-%offset%)
                      (* h oldsn))
              (if (> ncvt 0)
                  (dlasr "L" "V" "B"
                   (f2cl-lib:int-add (f2cl-lib:int-sub m ll) 1) ncvt
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         ((+ nm12 1))
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         ((+ nm13 1))
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice vt-%data%
                                         double-float
                                         (ll 1)
                                         ((1 ldvt) (1 *))
                                         vt-%offset%)
                   ldvt))
              (if (> nru 0)
                  (dlasr "R" "V" "B" nru
                   (f2cl-lib:int-add (f2cl-lib:int-sub m ll) 1)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (1)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (n)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice u-%data%
                                         double-float
                                         (1 ll)
                                         ((1 ldu) (1 *))
                                         u-%offset%)
                   ldu))
              (if (> ncc 0)
                  (dlasr "L" "V" "B"
                   (f2cl-lib:int-add (f2cl-lib:int-sub m ll) 1) ncc
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (1)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (n)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice c-%data%
                                         double-float
                                         (ll 1)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                   ldc))
              (if
               (<= (abs (f2cl-lib:fref e-%data% (ll) ((1 *)) e-%offset%))
                   thresh)
               (setf (f2cl-lib:fref e-%data% (ll) ((1 *)) e-%offset%) zero)))))
          (t
           (cond
             ((= idir 1)
              (setf f
                      (*
                       (-
                        (abs (f2cl-lib:fref d-%data% (ll) ((1 *)) d-%offset%))
                        shift)
                       (+
                        (f2cl-lib:sign one
                                       (f2cl-lib:fref d-%data%
                                                      (ll)
                                                      ((1 *))
                                                      d-%offset%))
                        (/ shift
                           (f2cl-lib:fref d-%data% (ll) ((1 *)) d-%offset%)))))
              (setf g (f2cl-lib:fref e-%data% (ll) ((1 *)) e-%offset%))
              (f2cl-lib:fdo (i ll (f2cl-lib:int-add i 1))
                            ((> i (f2cl-lib:int-add m (f2cl-lib:int-sub 1)))
                             nil)
                (tagbody
                  (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                      (dlartg f g cosr sinr r)
                    (declare (ignore var-0 var-1))
                    (setf cosr var-2)
                    (setf sinr var-3)
                    (setf r var-4))
                  (if (> i ll)
                      (setf (f2cl-lib:fref e-%data%
                                           ((f2cl-lib:int-sub i 1))
                                           ((1 *))
                                           e-%offset%)
                              r))
                  (setf f
                          (+
                           (* cosr
                              (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
                           (* sinr
                              (f2cl-lib:fref e-%data%
                                             (i)
                                             ((1 *))
                                             e-%offset%))))
                  (setf (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)
                          (-
                           (* cosr
                              (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%))
                           (* sinr
                              (f2cl-lib:fref d-%data%
                                             (i)
                                             ((1 *))
                                             d-%offset%))))
                  (setf g
                          (* sinr
                             (f2cl-lib:fref d-%data%
                                            ((f2cl-lib:int-add i 1))
                                            ((1 *))
                                            d-%offset%)))
                  (setf (f2cl-lib:fref d-%data%
                                       ((f2cl-lib:int-add i 1))
                                       ((1 *))
                                       d-%offset%)
                          (* cosr
                             (f2cl-lib:fref d-%data%
                                            ((f2cl-lib:int-add i 1))
                                            ((1 *))
                                            d-%offset%)))
                  (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                      (dlartg f g cosl sinl r)
                    (declare (ignore var-0 var-1))
                    (setf cosl var-2)
                    (setf sinl var-3)
                    (setf r var-4))
                  (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%) r)
                  (setf f
                          (+
                           (* cosl
                              (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%))
                           (* sinl
                              (f2cl-lib:fref d-%data%
                                             ((f2cl-lib:int-add i 1))
                                             ((1 *))
                                             d-%offset%))))
                  (setf (f2cl-lib:fref d-%data%
                                       ((f2cl-lib:int-add i 1))
                                       ((1 *))
                                       d-%offset%)
                          (-
                           (* cosl
                              (f2cl-lib:fref d-%data%
                                             ((f2cl-lib:int-add i 1))
                                             ((1 *))
                                             d-%offset%))
                           (* sinl
                              (f2cl-lib:fref e-%data%
                                             (i)
                                             ((1 *))
                                             e-%offset%))))
                  (cond
                    ((< i (f2cl-lib:int-add m (f2cl-lib:int-sub 1)))
                     (setf g
                             (* sinl
                                (f2cl-lib:fref e-%data%
                                               ((f2cl-lib:int-add i 1))
                                               ((1 *))
                                               e-%offset%)))
                     (setf (f2cl-lib:fref e-%data%
                                          ((f2cl-lib:int-add i 1))
                                          ((1 *))
                                          e-%offset%)
                             (* cosl
                                (f2cl-lib:fref e-%data%
                                               ((f2cl-lib:int-add i 1))
                                               ((1 *))
                                               e-%offset%)))))
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub i ll)
                                         1))
                                       ((1 *))
                                       work-%offset%)
                          cosr)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub i ll)
                                         1
                                         nm1))
                                       ((1 *))
                                       work-%offset%)
                          sinr)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub i ll)
                                         1
                                         nm12))
                                       ((1 *))
                                       work-%offset%)
                          cosl)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub i ll)
                                         1
                                         nm13))
                                       ((1 *))
                                       work-%offset%)
                          sinl)
                 label140))
              (setf (f2cl-lib:fref e-%data%
                                   ((f2cl-lib:int-sub m 1))
                                   ((1 *))
                                   e-%offset%)
                      f)
              (if (> ncvt 0)
                  (dlasr "L" "V" "F"
                   (f2cl-lib:int-add (f2cl-lib:int-sub m ll) 1) ncvt
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (1)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (n)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice vt-%data%
                                         double-float
                                         (ll 1)
                                         ((1 ldvt) (1 *))
                                         vt-%offset%)
                   ldvt))
              (if (> nru 0)
                  (dlasr "R" "V" "F" nru
                   (f2cl-lib:int-add (f2cl-lib:int-sub m ll) 1)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         ((+ nm12 1))
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         ((+ nm13 1))
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice u-%data%
                                         double-float
                                         (1 ll)
                                         ((1 ldu) (1 *))
                                         u-%offset%)
                   ldu))
              (if (> ncc 0)
                  (dlasr "L" "V" "F"
                   (f2cl-lib:int-add (f2cl-lib:int-sub m ll) 1) ncc
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         ((+ nm12 1))
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         ((+ nm13 1))
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice c-%data%
                                         double-float
                                         (ll 1)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                   ldc))
              (if
               (<=
                (abs
                 (f2cl-lib:fref e-%data%
                                ((f2cl-lib:int-sub m 1))
                                ((1 *))
                                e-%offset%))
                thresh)
               (setf (f2cl-lib:fref e-%data%
                                    ((f2cl-lib:int-sub m 1))
                                    ((1 *))
                                    e-%offset%)
                       zero)))
             (t
              (setf f
                      (*
                       (- (abs (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%))
                          shift)
                       (+
                        (f2cl-lib:sign one
                                       (f2cl-lib:fref d-%data%
                                                      (m)
                                                      ((1 *))
                                                      d-%offset%))
                        (/ shift
                           (f2cl-lib:fref d-%data% (m) ((1 *)) d-%offset%)))))
              (setf g
                      (f2cl-lib:fref e-%data%
                                     ((f2cl-lib:int-sub m 1))
                                     ((1 *))
                                     e-%offset%))
              (f2cl-lib:fdo (i m (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                            ((> i (f2cl-lib:int-add ll 1)) nil)
                (tagbody
                  (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                      (dlartg f g cosr sinr r)
                    (declare (ignore var-0 var-1))
                    (setf cosr var-2)
                    (setf sinr var-3)
                    (setf r var-4))
                  (if (< i m)
                      (setf (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%) r))
                  (setf f
                          (+
                           (* cosr
                              (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
                           (* sinr
                              (f2cl-lib:fref e-%data%
                                             ((f2cl-lib:int-sub i 1))
                                             ((1 *))
                                             e-%offset%))))
                  (setf (f2cl-lib:fref e-%data%
                                       ((f2cl-lib:int-sub i 1))
                                       ((1 *))
                                       e-%offset%)
                          (-
                           (* cosr
                              (f2cl-lib:fref e-%data%
                                             ((f2cl-lib:int-sub i 1))
                                             ((1 *))
                                             e-%offset%))
                           (* sinr
                              (f2cl-lib:fref d-%data%
                                             (i)
                                             ((1 *))
                                             d-%offset%))))
                  (setf g
                          (* sinr
                             (f2cl-lib:fref d-%data%
                                            ((f2cl-lib:int-sub i 1))
                                            ((1 *))
                                            d-%offset%)))
                  (setf (f2cl-lib:fref d-%data%
                                       ((f2cl-lib:int-sub i 1))
                                       ((1 *))
                                       d-%offset%)
                          (* cosr
                             (f2cl-lib:fref d-%data%
                                            ((f2cl-lib:int-sub i 1))
                                            ((1 *))
                                            d-%offset%)))
                  (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                      (dlartg f g cosl sinl r)
                    (declare (ignore var-0 var-1))
                    (setf cosl var-2)
                    (setf sinl var-3)
                    (setf r var-4))
                  (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%) r)
                  (setf f
                          (+
                           (* cosl
                              (f2cl-lib:fref e-%data%
                                             ((f2cl-lib:int-sub i 1))
                                             ((1 *))
                                             e-%offset%))
                           (* sinl
                              (f2cl-lib:fref d-%data%
                                             ((f2cl-lib:int-sub i 1))
                                             ((1 *))
                                             d-%offset%))))
                  (setf (f2cl-lib:fref d-%data%
                                       ((f2cl-lib:int-sub i 1))
                                       ((1 *))
                                       d-%offset%)
                          (-
                           (* cosl
                              (f2cl-lib:fref d-%data%
                                             ((f2cl-lib:int-sub i 1))
                                             ((1 *))
                                             d-%offset%))
                           (* sinl
                              (f2cl-lib:fref e-%data%
                                             ((f2cl-lib:int-sub i 1))
                                             ((1 *))
                                             e-%offset%))))
                  (cond
                    ((> i (f2cl-lib:int-add ll 1))
                     (setf g
                             (* sinl
                                (f2cl-lib:fref e-%data%
                                               ((f2cl-lib:int-sub i 2))
                                               ((1 *))
                                               e-%offset%)))
                     (setf (f2cl-lib:fref e-%data%
                                          ((f2cl-lib:int-sub i 2))
                                          ((1 *))
                                          e-%offset%)
                             (* cosl
                                (f2cl-lib:fref e-%data%
                                               ((f2cl-lib:int-sub i 2))
                                               ((1 *))
                                               e-%offset%)))))
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-sub i ll))
                                       ((1 *))
                                       work-%offset%)
                          cosr)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub i ll)
                                         nm1))
                                       ((1 *))
                                       work-%offset%)
                          (- sinr))
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub i ll)
                                         nm12))
                                       ((1 *))
                                       work-%offset%)
                          cosl)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub i ll)
                                         nm13))
                                       ((1 *))
                                       work-%offset%)
                          (- sinl))
                 label150))
              (setf (f2cl-lib:fref e-%data% (ll) ((1 *)) e-%offset%) f)
              (if
               (<= (abs (f2cl-lib:fref e-%data% (ll) ((1 *)) e-%offset%))
                   thresh)
               (setf (f2cl-lib:fref e-%data% (ll) ((1 *)) e-%offset%) zero))
              (if (> ncvt 0)
                  (dlasr "L" "V" "B"
                   (f2cl-lib:int-add (f2cl-lib:int-sub m ll) 1) ncvt
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         ((+ nm12 1))
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         ((+ nm13 1))
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice vt-%data%
                                         double-float
                                         (ll 1)
                                         ((1 ldvt) (1 *))
                                         vt-%offset%)
                   ldvt))
              (if (> nru 0)
                  (dlasr "R" "V" "B" nru
                   (f2cl-lib:int-add (f2cl-lib:int-sub m ll) 1)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (1)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (n)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice u-%data%
                                         double-float
                                         (1 ll)
                                         ((1 ldu) (1 *))
                                         u-%offset%)
                   ldu))
              (if (> ncc 0)
                  (dlasr "L" "V" "B"
                   (f2cl-lib:int-add (f2cl-lib:int-sub m ll) 1) ncc
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (1)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (n)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice c-%data%
                                         double-float
                                         (ll 1)
                                         ((1 ldc) (1 *))
                                         c-%offset%)
                   ldc))))))
        (go label60)
       label160
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (cond
              ((< (f2cl-lib:fref d (i) ((1 *))) zero)
               (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                       (- (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)))
               (if (> ncvt 0)
                   (dscal ncvt negone
                    (f2cl-lib:array-slice vt-%data%
                                          double-float
                                          (i 1)
                                          ((1 ldvt) (1 *))
                                          vt-%offset%)
                    ldvt))))
           label170))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
          (tagbody
            (setf isub 1)
            (setf smin (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%))
            (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                          ((> j (f2cl-lib:int-add n 1 (f2cl-lib:int-sub i)))
                           nil)
              (tagbody
                (cond
                  ((<= (f2cl-lib:fref d (j) ((1 *))) smin)
                   (setf isub j)
                   (setf smin
                           (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%))))
               label180))
            (cond
              ((/= isub (f2cl-lib:int-add n 1 (f2cl-lib:int-sub i)))
               (setf (f2cl-lib:fref d-%data% (isub) ((1 *)) d-%offset%)
                       (f2cl-lib:fref d-%data%
                                      ((f2cl-lib:int-sub (f2cl-lib:int-add n 1)
                                                         i))
                                      ((1 *))
                                      d-%offset%))
               (setf (f2cl-lib:fref d-%data%
                                    ((f2cl-lib:int-sub (f2cl-lib:int-add n 1)
                                                       i))
                                    ((1 *))
                                    d-%offset%)
                       smin)
               (if (> ncvt 0)
                   (dswap ncvt
                    (f2cl-lib:array-slice vt-%data%
                                          double-float
                                          (isub 1)
                                          ((1 ldvt) (1 *))
                                          vt-%offset%)
                    ldvt
                    (f2cl-lib:array-slice vt-%data%
                                          double-float
                                          ((+ n 1 (f2cl-lib:int-sub i)) 1)
                                          ((1 ldvt) (1 *))
                                          vt-%offset%)
                    ldvt))
               (if (> nru 0)
                   (dswap nru
                    (f2cl-lib:array-slice u-%data%
                                          double-float
                                          (1 isub)
                                          ((1 ldu) (1 *))
                                          u-%offset%)
                    1
                    (f2cl-lib:array-slice u-%data%
                                          double-float
                                          (1
                                           (f2cl-lib:int-sub
                                            (f2cl-lib:int-add n 1)
                                            i))
                                          ((1 ldu) (1 *))
                                          u-%offset%)
                    1))
               (if (> ncc 0)
                   (dswap ncc
                    (f2cl-lib:array-slice c-%data%
                                          double-float
                                          (isub 1)
                                          ((1 ldc) (1 *))
                                          c-%offset%)
                    ldc
                    (f2cl-lib:array-slice c-%data%
                                          double-float
                                          ((+ n 1 (f2cl-lib:int-sub i)) 1)
                                          ((1 ldc) (1 *))
                                          c-%offset%)
                    ldc))))
           label190))
        (go label220)
       label200
        (setf info 0)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
          (tagbody
            (if (/= (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%) zero)
                (setf info (f2cl-lib:int-add info 1)))
           label210))
       label220
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
                 info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dbdsqr
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dswap fortran-to-lisp::dscal
                    fortran-to-lisp::dlas2 fortran-to-lisp::drot
                    fortran-to-lisp::dlasv2 fortran-to-lisp::dlasr
                    fortran-to-lisp::dlartg fortran-to-lisp::dlamch
                    fortran-to-lisp::dlasq1 fortran-to-lisp::xerbla
                    fortran-to-lisp::lsame))))

