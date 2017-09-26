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
  (defun dbdsdc (uplo compq n d e u ldu vt ldvt q iq work iwork info)
    (declare (type (array f2cl-lib:integer4 (*)) iwork iq)
             (type (array double-float (*)) work q vt u e d)
             (type (f2cl-lib:integer4) info ldvt ldu n)
             (type (simple-string *) compq uplo))
    (f2cl-lib:with-multi-array-data
        ((uplo character uplo-%data% uplo-%offset%)
         (compq character compq-%data% compq-%offset%)
         (d double-float d-%data% d-%offset%)
         (e double-float e-%data% e-%offset%)
         (u double-float u-%data% u-%offset%)
         (vt double-float vt-%data% vt-%offset%)
         (q double-float q-%data% q-%offset%)
         (work double-float work-%data% work-%offset%)
         (iq f2cl-lib:integer4 iq-%data% iq-%offset%)
         (iwork f2cl-lib:integer4 iwork-%data% iwork-%offset%))
      (prog ((cs 0.0) (eps 0.0) (orgnrm 0.0) (p 0.0) (r 0.0) (sn 0.0) (difl 0)
             (difr 0) (givcol 0) (givnum 0) (givptr 0) (i 0) (ic 0) (icompq 0)
             (ierr 0) (ii 0) (is 0) (iu 0) (iuplo 0) (ivt 0) (j 0) (k 0) (kk 0)
             (mlvl 0) (nm1 0) (nsize 0) (perm 0) (poles 0) (qstart 0)
             (smlsiz 0) (smlszp 0) (sqre 0) (start 0) (wstart 0) (z 0))
        (declare (type (double-float) cs eps orgnrm p r sn)
                 (type (f2cl-lib:integer4) difl difr givcol givnum givptr i ic
                                           icompq ierr ii is iu iuplo ivt j k
                                           kk mlvl nm1 nsize perm poles qstart
                                           smlsiz smlszp sqre start wstart z))
        (setf info 0)
        (setf iuplo 0)
        (if (lsame uplo "U") (setf iuplo 1))
        (if (lsame uplo "L") (setf iuplo 2))
        (cond
          ((lsame compq "N")
           (setf icompq 0))
          ((lsame compq "P")
           (setf icompq 1))
          ((lsame compq "I")
           (setf icompq 2))
          (t
           (setf icompq -1)))
        (cond
          ((= iuplo 0)
           (setf info -1))
          ((< icompq 0)
           (setf info -2))
          ((< n 0)
           (setf info -3))
          ((or (< ldu 1) (and (= icompq 2) (< ldu n)))
           (setf info -7))
          ((or (< ldvt 1) (and (= icompq 2) (< ldvt n)))
           (setf info -9)))
        (cond
          ((/= info 0)
           (xerbla "DBDSDC" (f2cl-lib:int-sub info))
           (go end_label)))
        (if (= n 0) (go end_label))
        (setf smlsiz (ilaenv 9 "DBDSDC" " " 0 0 0 0))
        (cond
          ((= n 1)
           (cond
             ((= icompq 1)
              (setf (f2cl-lib:fref q-%data% (1) ((1 *)) q-%offset%)
                      (f2cl-lib:sign one
                                     (f2cl-lib:fref d-%data%
                                                    (1)
                                                    ((1 *))
                                                    d-%offset%)))
              (setf (f2cl-lib:fref q-%data%
                                   ((f2cl-lib:int-add 1
                                                      (f2cl-lib:int-mul smlsiz
                                                                        n)))
                                   ((1 *))
                                   q-%offset%)
                      one))
             ((= icompq 2)
              (setf (f2cl-lib:fref u-%data% (1 1) ((1 ldu) (1 *)) u-%offset%)
                      (f2cl-lib:sign one
                                     (f2cl-lib:fref d-%data%
                                                    (1)
                                                    ((1 *))
                                                    d-%offset%)))
              (setf (f2cl-lib:fref vt-%data%
                                   (1 1)
                                   ((1 ldvt) (1 *))
                                   vt-%offset%)
                      one)))
           (setf (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%)
                   (abs (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%)))
           (go end_label)))
        (setf nm1 (f2cl-lib:int-sub n 1))
        (setf wstart 1)
        (setf qstart 3)
        (cond
          ((= icompq 1)
           (dcopy n d 1
            (f2cl-lib:array-slice q-%data% double-float (1) ((1 *)) q-%offset%)
            1)
           (dcopy (f2cl-lib:int-sub n 1) e 1
            (f2cl-lib:array-slice q-%data%
                                  double-float
                                  ((+ n 1))
                                  ((1 *))
                                  q-%offset%)
            1)))
        (cond
          ((= iuplo 2)
           (setf qstart 5)
           (setf wstart (f2cl-lib:int-sub (f2cl-lib:int-mul 2 n) 1))
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
               (cond
                 ((= icompq 1)
                  (setf (f2cl-lib:fref q-%data%
                                       ((f2cl-lib:int-add i
                                                          (f2cl-lib:int-mul 2
                                                                            n)))
                                       ((1 *))
                                       q-%offset%)
                          cs)
                  (setf (f2cl-lib:fref q-%data%
                                       ((f2cl-lib:int-add i
                                                          (f2cl-lib:int-mul 3
                                                                            n)))
                                       ((1 *))
                                       q-%offset%)
                          sn))
                 ((= icompq 2)
                  (setf (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%)
                          cs)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add nm1 i))
                                       ((1 *))
                                       work-%offset%)
                          (- sn))))
              label10))))
        (cond
          ((= icompq 0)
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                  var-10 var-11 var-12 var-13 var-14 var-15)
               (dlasdq "U" 0 n 0 0 0 d e vt ldvt u ldu u ldu
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (wstart)
                                      ((1 *))
                                      work-%offset%)
                info)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14))
             (setf info var-15))
           (go label40)))
        (cond
          ((<= n smlsiz)
           (cond
             ((= icompq 2)
              (dlaset "A" n n zero one u ldu)
              (dlaset "A" n n zero one vt ldvt)
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15)
                  (dlasdq "U" 0 n n n 0 d e vt ldvt u ldu u ldu
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (wstart)
                                         ((1 *))
                                         work-%offset%)
                   info)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7 var-8 var-9 var-10 var-11 var-12 var-13
                                 var-14))
                (setf info var-15)))
             ((= icompq 1)
              (setf iu 1)
              (setf ivt (f2cl-lib:int-add iu n))
              (dlaset "A" n n zero one
               (f2cl-lib:array-slice q-%data%
                                     double-float
                                     ((+ iu
                                         (f2cl-lib:int-mul
                                          (f2cl-lib:int-add qstart
                                                            (f2cl-lib:int-sub
                                                             1))
                                          n)))
                                     ((1 *))
                                     q-%offset%)
               n)
              (dlaset "A" n n zero one
               (f2cl-lib:array-slice q-%data%
                                     double-float
                                     ((+ ivt
                                         (f2cl-lib:int-mul
                                          (f2cl-lib:int-add qstart
                                                            (f2cl-lib:int-sub
                                                             1))
                                          n)))
                                     ((1 *))
                                     q-%offset%)
               n)
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15)
                  (dlasdq "U" 0 n n n 0 d e
                   (f2cl-lib:array-slice q-%data%
                                         double-float
                                         ((+ ivt
                                             (f2cl-lib:int-mul
                                              (f2cl-lib:int-add qstart
                                                                (f2cl-lib:int-sub
                                                                 1))
                                              n)))
                                         ((1 *))
                                         q-%offset%)
                   n
                   (f2cl-lib:array-slice q-%data%
                                         double-float
                                         ((+ iu
                                             (f2cl-lib:int-mul
                                              (f2cl-lib:int-add qstart
                                                                (f2cl-lib:int-sub
                                                                 1))
                                              n)))
                                         ((1 *))
                                         q-%offset%)
                   n
                   (f2cl-lib:array-slice q-%data%
                                         double-float
                                         ((+ iu
                                             (f2cl-lib:int-mul
                                              (f2cl-lib:int-add qstart
                                                                (f2cl-lib:int-sub
                                                                 1))
                                              n)))
                                         ((1 *))
                                         q-%offset%)
                   n
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (wstart)
                                         ((1 *))
                                         work-%offset%)
                   info)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7 var-8 var-9 var-10 var-11 var-12 var-13
                                 var-14))
                (setf info var-15))))
           (go label40)))
        (cond
          ((= icompq 2)
           (dlaset "A" n n zero one u ldu)
           (dlaset "A" n n zero one vt ldvt)))
        (setf orgnrm (dlanst "M" n d e))
        (if (= orgnrm zero) (go end_label))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
            (dlascl "G" 0 0 orgnrm one n 1 d n ierr)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8))
          (setf ierr var-9))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
            (dlascl "G" 0 0 orgnrm one nm1 1 e nm1 ierr)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8))
          (setf ierr var-9))
        (setf eps (dlamch "Epsilon"))
        (setf mlvl
                (f2cl-lib:int-add
                 (f2cl-lib:int
                  (/
                   (f2cl-lib:flog
                    (/ (f2cl-lib:dble n)
                       (f2cl-lib:dble (f2cl-lib:int-add smlsiz 1))))
                   (f2cl-lib:flog two)))
                 1))
        (setf smlszp (f2cl-lib:int-add smlsiz 1))
        (cond
          ((= icompq 1)
           (setf iu 1)
           (setf ivt (f2cl-lib:int-add 1 smlsiz))
           (setf difl (f2cl-lib:int-add ivt smlszp))
           (setf difr (f2cl-lib:int-add difl mlvl))
           (setf z (f2cl-lib:int-add difr (f2cl-lib:int-mul mlvl 2)))
           (setf ic (f2cl-lib:int-add z mlvl))
           (setf is (f2cl-lib:int-add ic 1))
           (setf poles (f2cl-lib:int-add is 1))
           (setf givnum (f2cl-lib:int-add poles (f2cl-lib:int-mul 2 mlvl)))
           (setf k 1)
           (setf givptr 2)
           (setf perm 3)
           (setf givcol (f2cl-lib:int-add perm mlvl))))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (cond
              ((< (abs (f2cl-lib:fref d (i) ((1 *)))) eps)
               (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                       (f2cl-lib:sign eps
                                      (f2cl-lib:fref d-%data%
                                                     (i)
                                                     ((1 *))
                                                     d-%offset%)))))
           label20))
        (setf start 1)
        (setf sqre 0)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i nm1) nil)
          (tagbody
            (cond
              ((or (< (abs (f2cl-lib:fref e (i) ((1 *)))) eps) (= i nm1))
               (cond
                 ((< i nm1)
                  (setf nsize (f2cl-lib:int-add (f2cl-lib:int-sub i start) 1)))
                 ((>= (abs (f2cl-lib:fref e (i) ((1 *)))) eps)
                  (setf nsize (f2cl-lib:int-add (f2cl-lib:int-sub n start) 1)))
                 (t
                  (setf nsize (f2cl-lib:int-add (f2cl-lib:int-sub i start) 1))
                  (cond
                    ((= icompq 2)
                     (setf (f2cl-lib:fref u-%data%
                                          (n n)
                                          ((1 ldu) (1 *))
                                          u-%offset%)
                             (f2cl-lib:sign one
                                            (f2cl-lib:fref d-%data%
                                                           (n)
                                                           ((1 *))
                                                           d-%offset%)))
                     (setf (f2cl-lib:fref vt-%data%
                                          (n n)
                                          ((1 ldvt) (1 *))
                                          vt-%offset%)
                             one))
                    ((= icompq 1)
                     (setf (f2cl-lib:fref q-%data%
                                          ((f2cl-lib:int-add n
                                                             (f2cl-lib:int-mul
                                                              (f2cl-lib:int-sub
                                                               qstart
                                                               1)
                                                              n)))
                                          ((1 *))
                                          q-%offset%)
                             (f2cl-lib:sign one
                                            (f2cl-lib:fref d-%data%
                                                           (n)
                                                           ((1 *))
                                                           d-%offset%)))
                     (setf (f2cl-lib:fref q-%data%
                                          ((f2cl-lib:int-add n
                                                             (f2cl-lib:int-mul
                                                              (f2cl-lib:int-sub
                                                               (f2cl-lib:int-add
                                                                smlsiz
                                                                qstart)
                                                               1)
                                                              n)))
                                          ((1 *))
                                          q-%offset%)
                             one)))
                  (setf (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                          (abs
                           (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)))))
               (cond
                 ((= icompq 2)
                  (multiple-value-bind
                        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                         var-9 var-10 var-11)
                      (dlasd0 nsize sqre
                       (f2cl-lib:array-slice d-%data%
                                             double-float
                                             (start)
                                             ((1 *))
                                             d-%offset%)
                       (f2cl-lib:array-slice e-%data%
                                             double-float
                                             (start)
                                             ((1 *))
                                             e-%offset%)
                       (f2cl-lib:array-slice u-%data%
                                             double-float
                                             (start start)
                                             ((1 ldu) (1 *))
                                             u-%offset%)
                       ldu
                       (f2cl-lib:array-slice vt-%data%
                                             double-float
                                             (start start)
                                             ((1 ldvt) (1 *))
                                             vt-%offset%)
                       ldvt smlsiz iwork
                       (f2cl-lib:array-slice work-%data%
                                             double-float
                                             (wstart)
                                             ((1 *))
                                             work-%offset%)
                       info)
                    (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                     var-7 var-8 var-9 var-10))
                    (setf info var-11)))
                 (t
                  (multiple-value-bind
                        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                         var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16
                         var-17 var-18 var-19 var-20 var-21 var-22 var-23)
                      (dlasda icompq smlsiz nsize sqre
                       (f2cl-lib:array-slice d-%data%
                                             double-float
                                             (start)
                                             ((1 *))
                                             d-%offset%)
                       (f2cl-lib:array-slice e-%data%
                                             double-float
                                             (start)
                                             ((1 *))
                                             e-%offset%)
                       (f2cl-lib:array-slice q-%data%
                                             double-float
                                             ((+ start
                                                 (f2cl-lib:int-mul
                                                  (f2cl-lib:int-add iu
                                                                    qstart
                                                                    (f2cl-lib:int-sub
                                                                     2))
                                                  n)))
                                             ((1 *))
                                             q-%offset%)
                       n
                       (f2cl-lib:array-slice q-%data%
                                             double-float
                                             ((+ start
                                                 (f2cl-lib:int-mul
                                                  (f2cl-lib:int-add ivt
                                                                    qstart
                                                                    (f2cl-lib:int-sub
                                                                     2))
                                                  n)))
                                             ((1 *))
                                             q-%offset%)
                       (f2cl-lib:array-slice iq-%data%
                                             f2cl-lib:integer4
                                             ((+ start (f2cl-lib:int-mul k n)))
                                             ((1 *))
                                             iq-%offset%)
                       (f2cl-lib:array-slice q-%data%
                                             double-float
                                             ((+ start
                                                 (f2cl-lib:int-mul
                                                  (f2cl-lib:int-add difl
                                                                    qstart
                                                                    (f2cl-lib:int-sub
                                                                     2))
                                                  n)))
                                             ((1 *))
                                             q-%offset%)
                       (f2cl-lib:array-slice q-%data%
                                             double-float
                                             ((+ start
                                                 (f2cl-lib:int-mul
                                                  (f2cl-lib:int-add difr
                                                                    qstart
                                                                    (f2cl-lib:int-sub
                                                                     2))
                                                  n)))
                                             ((1 *))
                                             q-%offset%)
                       (f2cl-lib:array-slice q-%data%
                                             double-float
                                             ((+ start
                                                 (f2cl-lib:int-mul
                                                  (f2cl-lib:int-add z
                                                                    qstart
                                                                    (f2cl-lib:int-sub
                                                                     2))
                                                  n)))
                                             ((1 *))
                                             q-%offset%)
                       (f2cl-lib:array-slice q-%data%
                                             double-float
                                             ((+ start
                                                 (f2cl-lib:int-mul
                                                  (f2cl-lib:int-add poles
                                                                    qstart
                                                                    (f2cl-lib:int-sub
                                                                     2))
                                                  n)))
                                             ((1 *))
                                             q-%offset%)
                       (f2cl-lib:array-slice iq-%data%
                                             f2cl-lib:integer4
                                             ((+ start
                                                 (f2cl-lib:int-mul givptr n)))
                                             ((1 *))
                                             iq-%offset%)
                       (f2cl-lib:array-slice iq-%data%
                                             f2cl-lib:integer4
                                             ((+ start
                                                 (f2cl-lib:int-mul givcol n)))
                                             ((1 *))
                                             iq-%offset%)
                       n
                       (f2cl-lib:array-slice iq-%data%
                                             f2cl-lib:integer4
                                             ((+ start
                                                 (f2cl-lib:int-mul perm n)))
                                             ((1 *))
                                             iq-%offset%)
                       (f2cl-lib:array-slice q-%data%
                                             double-float
                                             ((+ start
                                                 (f2cl-lib:int-mul
                                                  (f2cl-lib:int-add givnum
                                                                    qstart
                                                                    (f2cl-lib:int-sub
                                                                     2))
                                                  n)))
                                             ((1 *))
                                             q-%offset%)
                       (f2cl-lib:array-slice q-%data%
                                             double-float
                                             ((+ start
                                                 (f2cl-lib:int-mul
                                                  (f2cl-lib:int-add ic
                                                                    qstart
                                                                    (f2cl-lib:int-sub
                                                                     2))
                                                  n)))
                                             ((1 *))
                                             q-%offset%)
                       (f2cl-lib:array-slice q-%data%
                                             double-float
                                             ((+ start
                                                 (f2cl-lib:int-mul
                                                  (f2cl-lib:int-add is
                                                                    qstart
                                                                    (f2cl-lib:int-sub
                                                                     2))
                                                  n)))
                                             ((1 *))
                                             q-%offset%)
                       (f2cl-lib:array-slice work-%data%
                                             double-float
                                             (wstart)
                                             ((1 *))
                                             work-%offset%)
                       iwork info)
                    (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                     var-7 var-8 var-9 var-10 var-11 var-12
                                     var-13 var-14 var-15 var-16 var-17 var-18
                                     var-19 var-20 var-21 var-22))
                    (setf info var-23))
                  (cond
                    ((/= info 0)
                     (go end_label)))))
               (setf start (f2cl-lib:int-add i 1))))
           label30))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
            (dlascl "G" 0 0 one orgnrm n 1 d n ierr)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8))
          (setf ierr var-9))
       label40
        (f2cl-lib:fdo (ii 2 (f2cl-lib:int-add ii 1))
                      ((> ii n) nil)
          (tagbody
            (setf i (f2cl-lib:int-sub ii 1))
            (setf kk i)
            (setf p (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
            (f2cl-lib:fdo (j ii (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (cond
                  ((> (f2cl-lib:fref d (j) ((1 *))) p)
                   (setf kk j)
                   (setf p (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%))))
               label50))
            (cond
              ((/= kk i)
               (setf (f2cl-lib:fref d-%data% (kk) ((1 *)) d-%offset%)
                       (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
               (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%) p)
               (cond
                 ((= icompq 1)
                  (setf (f2cl-lib:fref iq-%data% (i) ((1 *)) iq-%offset%) kk))
                 ((= icompq 2)
                  (dswap n
                   (f2cl-lib:array-slice u-%data%
                                         double-float
                                         (1 i)
                                         ((1 ldu) (1 *))
                                         u-%offset%)
                   1
                   (f2cl-lib:array-slice u-%data%
                                         double-float
                                         (1 kk)
                                         ((1 ldu) (1 *))
                                         u-%offset%)
                   1)
                  (dswap n
                   (f2cl-lib:array-slice vt-%data%
                                         double-float
                                         (i 1)
                                         ((1 ldvt) (1 *))
                                         vt-%offset%)
                   ldvt
                   (f2cl-lib:array-slice vt-%data%
                                         double-float
                                         (kk 1)
                                         ((1 ldvt) (1 *))
                                         vt-%offset%)
                   ldvt))))
              ((= icompq 1)
               (setf (f2cl-lib:fref iq-%data% (i) ((1 *)) iq-%offset%) i)))
           label60))
        (cond
          ((= icompq 1)
           (cond
             ((= iuplo 1)
              (setf (f2cl-lib:fref iq-%data% (n) ((1 *)) iq-%offset%) 1))
             (t
              (setf (f2cl-lib:fref iq-%data% (n) ((1 *)) iq-%offset%) 0)))))
        (if (and (= iuplo 2) (= icompq 2))
            (dlasr "L" "V" "B" n n
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
        (go end_label)
       end_label
        (return
         (values nil nil nil nil nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dbdsdc
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlasr fortran-to-lisp::dswap
                    fortran-to-lisp::dlasda fortran-to-lisp::dlasd0
                    fortran-to-lisp::dlamch fortran-to-lisp::dlascl
                    fortran-to-lisp::dlanst fortran-to-lisp::dlaset
                    fortran-to-lisp::dlasdq fortran-to-lisp::dlartg
                    fortran-to-lisp::dcopy fortran-to-lisp::ilaenv
                    fortran-to-lisp::xerbla fortran-to-lisp::lsame))))

