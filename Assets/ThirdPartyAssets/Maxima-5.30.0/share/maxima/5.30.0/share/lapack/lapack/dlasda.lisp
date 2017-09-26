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
  (defun dlasda
         (icompq smlsiz n sqre d e u ldu vt k difl difr z poles givptr givcol
          ldgcol perm givnum c s work iwork info)
    (declare (type (array f2cl-lib:integer4 (*)) iwork perm givcol givptr k)
             (type (array double-float (*)) work s c givnum poles z difr difl
                                            vt u e d)
             (type (f2cl-lib:integer4) info ldgcol ldu sqre n smlsiz icompq))
    (f2cl-lib:with-multi-array-data
        ((d double-float d-%data% d-%offset%)
         (e double-float e-%data% e-%offset%)
         (u double-float u-%data% u-%offset%)
         (vt double-float vt-%data% vt-%offset%)
         (difl double-float difl-%data% difl-%offset%)
         (difr double-float difr-%data% difr-%offset%)
         (z double-float z-%data% z-%offset%)
         (poles double-float poles-%data% poles-%offset%)
         (givnum double-float givnum-%data% givnum-%offset%)
         (c double-float c-%data% c-%offset%)
         (s double-float s-%data% s-%offset%)
         (work double-float work-%data% work-%offset%)
         (k f2cl-lib:integer4 k-%data% k-%offset%)
         (givptr f2cl-lib:integer4 givptr-%data% givptr-%offset%)
         (givcol f2cl-lib:integer4 givcol-%data% givcol-%offset%)
         (perm f2cl-lib:integer4 perm-%data% perm-%offset%)
         (iwork f2cl-lib:integer4 iwork-%data% iwork-%offset%))
      (prog ((alpha 0.0) (beta 0.0) (i 0) (i1 0) (ic 0) (idxq 0) (idxqi 0)
             (im1 0) (inode 0) (itemp 0) (iwk 0) (j 0) (lf 0) (ll 0) (lvl 0)
             (lvl2 0) (m 0) (ncc 0) (nd 0) (ndb1 0) (ndiml 0) (ndimr 0) (nl 0)
             (nlf 0) (nlp1 0) (nlvl 0) (nr 0) (nrf 0) (nrp1 0) (nru 0)
             (nwork1 0) (nwork2 0) (smlszp 0) (sqrei 0) (vf 0) (vfi 0) (vl 0)
             (vli 0))
        (declare (type (double-float) alpha beta)
                 (type (f2cl-lib:integer4) i i1 ic idxq idxqi im1 inode itemp
                                           iwk j lf ll lvl lvl2 m ncc nd ndb1
                                           ndiml ndimr nl nlf nlp1 nlvl nr nrf
                                           nrp1 nru nwork1 nwork2 smlszp sqrei
                                           vf vfi vl vli))
        (setf info 0)
        (cond
          ((or (< icompq 0) (> icompq 1))
           (setf info -1))
          ((< smlsiz 3)
           (setf info -2))
          ((< n 0)
           (setf info -3))
          ((or (< sqre 0) (> sqre 1))
           (setf info -4))
          ((< ldu (f2cl-lib:int-add n sqre))
           (setf info -8))
          ((< ldgcol n)
           (setf info -17)))
        (cond
          ((/= info 0)
           (xerbla "DLASDA" (f2cl-lib:int-sub info))
           (go end_label)))
        (setf m (f2cl-lib:int-add n sqre))
        (cond
          ((<= n smlsiz)
           (cond
             ((= icompq 0)
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15)
                  (dlasdq "U" sqre n 0 0 0 d e vt ldu u ldu u ldu work info)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7 var-8 var-9 var-10 var-11 var-12 var-13
                                 var-14))
                (setf info var-15)))
             (t
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15)
                  (dlasdq "U" sqre n m n 0 d e vt ldu u ldu u ldu work info)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7 var-8 var-9 var-10 var-11 var-12 var-13
                                 var-14))
                (setf info var-15))))
           (go end_label)))
        (setf inode 1)
        (setf ndiml (f2cl-lib:int-add inode n))
        (setf ndimr (f2cl-lib:int-add ndiml n))
        (setf idxq (f2cl-lib:int-add ndimr n))
        (setf iwk (f2cl-lib:int-add idxq n))
        (setf ncc 0)
        (setf nru 0)
        (setf smlszp (f2cl-lib:int-add smlsiz 1))
        (setf vf 1)
        (setf vl (f2cl-lib:int-add vf m))
        (setf nwork1 (f2cl-lib:int-add vl m))
        (setf nwork2
                (f2cl-lib:int-add nwork1 (f2cl-lib:int-mul smlszp smlszp)))
        (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
            (dlasdt n nlvl nd
             (f2cl-lib:array-slice iwork-%data%
                                   f2cl-lib:integer4
                                   (inode)
                                   ((1 *))
                                   iwork-%offset%)
             (f2cl-lib:array-slice iwork-%data%
                                   f2cl-lib:integer4
                                   (ndiml)
                                   ((1 *))
                                   iwork-%offset%)
             (f2cl-lib:array-slice iwork-%data%
                                   f2cl-lib:integer4
                                   (ndimr)
                                   ((1 *))
                                   iwork-%offset%)
             smlsiz)
          (declare (ignore var-0 var-3 var-4 var-5 var-6))
          (setf nlvl var-1)
          (setf nd var-2))
        (setf ndb1 (the f2cl-lib:integer4 (truncate (+ nd 1) 2)))
        (f2cl-lib:fdo (i ndb1 (f2cl-lib:int-add i 1))
                      ((> i nd) nil)
          (tagbody
            (setf i1 (f2cl-lib:int-sub i 1))
            (setf ic
                    (f2cl-lib:fref iwork-%data%
                                   ((f2cl-lib:int-add inode i1))
                                   ((1 *))
                                   iwork-%offset%))
            (setf nl
                    (f2cl-lib:fref iwork-%data%
                                   ((f2cl-lib:int-add ndiml i1))
                                   ((1 *))
                                   iwork-%offset%))
            (setf nlp1 (f2cl-lib:int-add nl 1))
            (setf nr
                    (f2cl-lib:fref iwork-%data%
                                   ((f2cl-lib:int-add ndimr i1))
                                   ((1 *))
                                   iwork-%offset%))
            (setf nlf (f2cl-lib:int-sub ic nl))
            (setf nrf (f2cl-lib:int-add ic 1))
            (setf idxqi (f2cl-lib:int-sub (f2cl-lib:int-add idxq nlf) 2))
            (setf vfi (f2cl-lib:int-sub (f2cl-lib:int-add vf nlf) 1))
            (setf vli (f2cl-lib:int-sub (f2cl-lib:int-add vl nlf) 1))
            (setf sqrei 1)
            (cond
              ((= icompq 0)
               (dlaset "A" nlp1 nlp1 zero one
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (nwork1)
                                      ((1 *))
                                      work-%offset%)
                smlszp)
               (multiple-value-bind
                     (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                      var-9 var-10 var-11 var-12 var-13 var-14 var-15)
                   (dlasdq "U" sqrei nl nlp1 nru ncc
                    (f2cl-lib:array-slice d-%data%
                                          double-float
                                          (nlf)
                                          ((1 *))
                                          d-%offset%)
                    (f2cl-lib:array-slice e-%data%
                                          double-float
                                          (nlf)
                                          ((1 *))
                                          e-%offset%)
                    (f2cl-lib:array-slice work-%data%
                                          double-float
                                          (nwork1)
                                          ((1 *))
                                          work-%offset%)
                    smlszp
                    (f2cl-lib:array-slice work-%data%
                                          double-float
                                          (nwork2)
                                          ((1 *))
                                          work-%offset%)
                    nl
                    (f2cl-lib:array-slice work-%data%
                                          double-float
                                          (nwork2)
                                          ((1 *))
                                          work-%offset%)
                    nl
                    (f2cl-lib:array-slice work-%data%
                                          double-float
                                          (nwork2)
                                          ((1 *))
                                          work-%offset%)
                    info)
                 (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                  var-7 var-8 var-9 var-10 var-11 var-12 var-13
                                  var-14))
                 (setf info var-15))
               (setf itemp
                       (f2cl-lib:int-add nwork1 (f2cl-lib:int-mul nl smlszp)))
               (dcopy nlp1
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (nwork1)
                                      ((1 *))
                                      work-%offset%)
                1
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (vfi)
                                      ((1 *))
                                      work-%offset%)
                1)
               (dcopy nlp1
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (itemp)
                                      ((1 *))
                                      work-%offset%)
                1
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (vli)
                                      ((1 *))
                                      work-%offset%)
                1))
              (t
               (dlaset "A" nl nl zero one
                (f2cl-lib:array-slice u-%data%
                                      double-float
                                      (nlf 1)
                                      ((1 ldu) (1 *))
                                      u-%offset%)
                ldu)
               (dlaset "A" nlp1 nlp1 zero one
                (f2cl-lib:array-slice vt-%data%
                                      double-float
                                      (nlf 1)
                                      ((1 ldu) (1 *))
                                      vt-%offset%)
                ldu)
               (multiple-value-bind
                     (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                      var-9 var-10 var-11 var-12 var-13 var-14 var-15)
                   (dlasdq "U" sqrei nl nlp1 nl ncc
                    (f2cl-lib:array-slice d-%data%
                                          double-float
                                          (nlf)
                                          ((1 *))
                                          d-%offset%)
                    (f2cl-lib:array-slice e-%data%
                                          double-float
                                          (nlf)
                                          ((1 *))
                                          e-%offset%)
                    (f2cl-lib:array-slice vt-%data%
                                          double-float
                                          (nlf 1)
                                          ((1 ldu) (1 *))
                                          vt-%offset%)
                    ldu
                    (f2cl-lib:array-slice u-%data%
                                          double-float
                                          (nlf 1)
                                          ((1 ldu) (1 *))
                                          u-%offset%)
                    ldu
                    (f2cl-lib:array-slice u-%data%
                                          double-float
                                          (nlf 1)
                                          ((1 ldu) (1 *))
                                          u-%offset%)
                    ldu
                    (f2cl-lib:array-slice work-%data%
                                          double-float
                                          (nwork1)
                                          ((1 *))
                                          work-%offset%)
                    info)
                 (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                  var-7 var-8 var-9 var-10 var-11 var-12 var-13
                                  var-14))
                 (setf info var-15))
               (dcopy nlp1
                (f2cl-lib:array-slice vt-%data%
                                      double-float
                                      (nlf 1)
                                      ((1 ldu) (1 *))
                                      vt-%offset%)
                1
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (vfi)
                                      ((1 *))
                                      work-%offset%)
                1)
               (dcopy nlp1
                (f2cl-lib:array-slice vt-%data%
                                      double-float
                                      (nlf nlp1)
                                      ((1 ldu) (1 *))
                                      vt-%offset%)
                1
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (vli)
                                      ((1 *))
                                      work-%offset%)
                1)))
            (cond
              ((/= info 0)
               (go end_label)))
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j nl) nil)
              (tagbody
                (setf (f2cl-lib:fref iwork-%data%
                                     ((f2cl-lib:int-add idxqi j))
                                     ((1 *))
                                     iwork-%offset%)
                        j)
               label10))
            (cond
              ((and (= i nd) (= sqre 0))
               (setf sqrei 0))
              (t
               (setf sqrei 1)))
            (setf idxqi (f2cl-lib:int-add idxqi nlp1))
            (setf vfi (f2cl-lib:int-add vfi nlp1))
            (setf vli (f2cl-lib:int-add vli nlp1))
            (setf nrp1 (f2cl-lib:int-add nr sqrei))
            (cond
              ((= icompq 0)
               (dlaset "A" nrp1 nrp1 zero one
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (nwork1)
                                      ((1 *))
                                      work-%offset%)
                smlszp)
               (multiple-value-bind
                     (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                      var-9 var-10 var-11 var-12 var-13 var-14 var-15)
                   (dlasdq "U" sqrei nr nrp1 nru ncc
                    (f2cl-lib:array-slice d-%data%
                                          double-float
                                          (nrf)
                                          ((1 *))
                                          d-%offset%)
                    (f2cl-lib:array-slice e-%data%
                                          double-float
                                          (nrf)
                                          ((1 *))
                                          e-%offset%)
                    (f2cl-lib:array-slice work-%data%
                                          double-float
                                          (nwork1)
                                          ((1 *))
                                          work-%offset%)
                    smlszp
                    (f2cl-lib:array-slice work-%data%
                                          double-float
                                          (nwork2)
                                          ((1 *))
                                          work-%offset%)
                    nr
                    (f2cl-lib:array-slice work-%data%
                                          double-float
                                          (nwork2)
                                          ((1 *))
                                          work-%offset%)
                    nr
                    (f2cl-lib:array-slice work-%data%
                                          double-float
                                          (nwork2)
                                          ((1 *))
                                          work-%offset%)
                    info)
                 (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                  var-7 var-8 var-9 var-10 var-11 var-12 var-13
                                  var-14))
                 (setf info var-15))
               (setf itemp
                       (f2cl-lib:int-add nwork1
                                         (f2cl-lib:int-mul
                                          (f2cl-lib:int-sub nrp1 1)
                                          smlszp)))
               (dcopy nrp1
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (nwork1)
                                      ((1 *))
                                      work-%offset%)
                1
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (vfi)
                                      ((1 *))
                                      work-%offset%)
                1)
               (dcopy nrp1
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (itemp)
                                      ((1 *))
                                      work-%offset%)
                1
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (vli)
                                      ((1 *))
                                      work-%offset%)
                1))
              (t
               (dlaset "A" nr nr zero one
                (f2cl-lib:array-slice u-%data%
                                      double-float
                                      (nrf 1)
                                      ((1 ldu) (1 *))
                                      u-%offset%)
                ldu)
               (dlaset "A" nrp1 nrp1 zero one
                (f2cl-lib:array-slice vt-%data%
                                      double-float
                                      (nrf 1)
                                      ((1 ldu) (1 *))
                                      vt-%offset%)
                ldu)
               (multiple-value-bind
                     (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                      var-9 var-10 var-11 var-12 var-13 var-14 var-15)
                   (dlasdq "U" sqrei nr nrp1 nr ncc
                    (f2cl-lib:array-slice d-%data%
                                          double-float
                                          (nrf)
                                          ((1 *))
                                          d-%offset%)
                    (f2cl-lib:array-slice e-%data%
                                          double-float
                                          (nrf)
                                          ((1 *))
                                          e-%offset%)
                    (f2cl-lib:array-slice vt-%data%
                                          double-float
                                          (nrf 1)
                                          ((1 ldu) (1 *))
                                          vt-%offset%)
                    ldu
                    (f2cl-lib:array-slice u-%data%
                                          double-float
                                          (nrf 1)
                                          ((1 ldu) (1 *))
                                          u-%offset%)
                    ldu
                    (f2cl-lib:array-slice u-%data%
                                          double-float
                                          (nrf 1)
                                          ((1 ldu) (1 *))
                                          u-%offset%)
                    ldu
                    (f2cl-lib:array-slice work-%data%
                                          double-float
                                          (nwork1)
                                          ((1 *))
                                          work-%offset%)
                    info)
                 (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                  var-7 var-8 var-9 var-10 var-11 var-12 var-13
                                  var-14))
                 (setf info var-15))
               (dcopy nrp1
                (f2cl-lib:array-slice vt-%data%
                                      double-float
                                      (nrf 1)
                                      ((1 ldu) (1 *))
                                      vt-%offset%)
                1
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (vfi)
                                      ((1 *))
                                      work-%offset%)
                1)
               (dcopy nrp1
                (f2cl-lib:array-slice vt-%data%
                                      double-float
                                      (nrf nrp1)
                                      ((1 ldu) (1 *))
                                      vt-%offset%)
                1
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (vli)
                                      ((1 *))
                                      work-%offset%)
                1)))
            (cond
              ((/= info 0)
               (go end_label)))
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j nr) nil)
              (tagbody
                (setf (f2cl-lib:fref iwork-%data%
                                     ((f2cl-lib:int-add idxqi j))
                                     ((1 *))
                                     iwork-%offset%)
                        j)
               label20))
           label30))
        (setf j (expt 2 nlvl))
        (f2cl-lib:fdo (lvl nlvl (f2cl-lib:int-add lvl (f2cl-lib:int-sub 1)))
                      ((> lvl 1) nil)
          (tagbody
            (setf lvl2 (f2cl-lib:int-sub (f2cl-lib:int-mul lvl 2) 1))
            (cond
              ((= lvl 1)
               (setf lf 1)
               (setf ll 1))
              (t
               (setf lf (expt 2 (f2cl-lib:int-sub lvl 1)))
               (setf ll (f2cl-lib:int-sub (f2cl-lib:int-mul 2 lf) 1))))
            (f2cl-lib:fdo (i lf (f2cl-lib:int-add i 1))
                          ((> i ll) nil)
              (tagbody
                (setf im1 (f2cl-lib:int-sub i 1))
                (setf ic
                        (f2cl-lib:fref iwork-%data%
                                       ((f2cl-lib:int-add inode im1))
                                       ((1 *))
                                       iwork-%offset%))
                (setf nl
                        (f2cl-lib:fref iwork-%data%
                                       ((f2cl-lib:int-add ndiml im1))
                                       ((1 *))
                                       iwork-%offset%))
                (setf nr
                        (f2cl-lib:fref iwork-%data%
                                       ((f2cl-lib:int-add ndimr im1))
                                       ((1 *))
                                       iwork-%offset%))
                (setf nlf (f2cl-lib:int-sub ic nl))
                (setf nrf (f2cl-lib:int-add ic 1))
                (cond
                  ((= i ll)
                   (setf sqrei sqre))
                  (t
                   (setf sqrei 1)))
                (setf vfi (f2cl-lib:int-sub (f2cl-lib:int-add vf nlf) 1))
                (setf vli (f2cl-lib:int-sub (f2cl-lib:int-add vl nlf) 1))
                (setf idxqi (f2cl-lib:int-sub (f2cl-lib:int-add idxq nlf) 1))
                (setf alpha (f2cl-lib:fref d-%data% (ic) ((1 *)) d-%offset%))
                (setf beta (f2cl-lib:fref e-%data% (ic) ((1 *)) e-%offset%))
                (cond
                  ((= icompq 0)
                   (multiple-value-bind
                         (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                          var-9 var-10 var-11 var-12 var-13 var-14 var-15
                          var-16 var-17 var-18 var-19 var-20 var-21 var-22
                          var-23 var-24 var-25)
                       (dlasd6 icompq nl nr sqrei
                        (f2cl-lib:array-slice d-%data%
                                              double-float
                                              (nlf)
                                              ((1 *))
                                              d-%offset%)
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (vfi)
                                              ((1 *))
                                              work-%offset%)
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (vli)
                                              ((1 *))
                                              work-%offset%)
                        alpha beta
                        (f2cl-lib:array-slice iwork-%data%
                                              f2cl-lib:integer4
                                              (idxqi)
                                              ((1 *))
                                              iwork-%offset%)
                        perm
                        (f2cl-lib:fref givptr-%data%
                                       (1)
                                       ((1 *))
                                       givptr-%offset%)
                        givcol ldgcol givnum ldu poles difl difr z
                        (f2cl-lib:fref k-%data% (1) ((1 *)) k-%offset%)
                        (f2cl-lib:fref c-%data% (1) ((1 *)) c-%offset%)
                        (f2cl-lib:fref s-%data% (1) ((1 *)) s-%offset%)
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (nwork1)
                                              ((1 *))
                                              work-%offset%)
                        (f2cl-lib:array-slice iwork-%data%
                                              f2cl-lib:integer4
                                              (iwk)
                                              ((1 *))
                                              iwork-%offset%)
                        info)
                     (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                      var-9 var-10 var-12 var-13 var-14 var-15
                                      var-16 var-17 var-18 var-19 var-23
                                      var-24))
                     (setf alpha var-7)
                     (setf beta var-8)
                     (setf (f2cl-lib:fref givptr-%data%
                                          (1)
                                          ((1 *))
                                          givptr-%offset%)
                             var-11)
                     (setf (f2cl-lib:fref k-%data% (1) ((1 *)) k-%offset%)
                             var-20)
                     (setf (f2cl-lib:fref c-%data% (1) ((1 *)) c-%offset%)
                             var-21)
                     (setf (f2cl-lib:fref s-%data% (1) ((1 *)) s-%offset%)
                             var-22)
                     (setf info var-25)))
                  (t
                   (setf j (f2cl-lib:int-sub j 1))
                   (multiple-value-bind
                         (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                          var-9 var-10 var-11 var-12 var-13 var-14 var-15
                          var-16 var-17 var-18 var-19 var-20 var-21 var-22
                          var-23 var-24 var-25)
                       (dlasd6 icompq nl nr sqrei
                        (f2cl-lib:array-slice d-%data%
                                              double-float
                                              (nlf)
                                              ((1 *))
                                              d-%offset%)
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (vfi)
                                              ((1 *))
                                              work-%offset%)
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (vli)
                                              ((1 *))
                                              work-%offset%)
                        alpha beta
                        (f2cl-lib:array-slice iwork-%data%
                                              f2cl-lib:integer4
                                              (idxqi)
                                              ((1 *))
                                              iwork-%offset%)
                        (f2cl-lib:array-slice perm-%data%
                                              f2cl-lib:integer4
                                              (nlf lvl)
                                              ((1 ldgcol) (1 *))
                                              perm-%offset%)
                        (f2cl-lib:fref givptr-%data%
                                       (j)
                                       ((1 *))
                                       givptr-%offset%)
                        (f2cl-lib:array-slice givcol-%data%
                                              f2cl-lib:integer4
                                              (nlf lvl2)
                                              ((1 ldgcol) (1 *))
                                              givcol-%offset%)
                        ldgcol
                        (f2cl-lib:array-slice givnum-%data%
                                              double-float
                                              (nlf lvl2)
                                              ((1 ldu) (1 *))
                                              givnum-%offset%)
                        ldu
                        (f2cl-lib:array-slice poles-%data%
                                              double-float
                                              (nlf lvl2)
                                              ((1 ldu) (1 *))
                                              poles-%offset%)
                        (f2cl-lib:array-slice difl-%data%
                                              double-float
                                              (nlf lvl)
                                              ((1 ldu) (1 *))
                                              difl-%offset%)
                        (f2cl-lib:array-slice difr-%data%
                                              double-float
                                              (nlf lvl2)
                                              ((1 ldu) (1 *))
                                              difr-%offset%)
                        (f2cl-lib:array-slice z-%data%
                                              double-float
                                              (nlf lvl)
                                              ((1 ldu) (1 *))
                                              z-%offset%)
                        (f2cl-lib:fref k-%data% (j) ((1 *)) k-%offset%)
                        (f2cl-lib:fref c-%data% (j) ((1 *)) c-%offset%)
                        (f2cl-lib:fref s-%data% (j) ((1 *)) s-%offset%)
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (nwork1)
                                              ((1 *))
                                              work-%offset%)
                        (f2cl-lib:array-slice iwork-%data%
                                              f2cl-lib:integer4
                                              (iwk)
                                              ((1 *))
                                              iwork-%offset%)
                        info)
                     (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                      var-9 var-10 var-12 var-13 var-14 var-15
                                      var-16 var-17 var-18 var-19 var-23
                                      var-24))
                     (setf alpha var-7)
                     (setf beta var-8)
                     (setf (f2cl-lib:fref givptr-%data%
                                          (j)
                                          ((1 *))
                                          givptr-%offset%)
                             var-11)
                     (setf (f2cl-lib:fref k-%data% (j) ((1 *)) k-%offset%)
                             var-20)
                     (setf (f2cl-lib:fref c-%data% (j) ((1 *)) c-%offset%)
                             var-21)
                     (setf (f2cl-lib:fref s-%data% (j) ((1 *)) s-%offset%)
                             var-22)
                     (setf info var-25))))
                (cond
                  ((/= info 0)
                   (go end_label)))
               label40))
           label50))
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
                 nil
                 nil
                 nil
                 nil
                 info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasda
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlasd6 fortran-to-lisp::dcopy
                    fortran-to-lisp::dlaset fortran-to-lisp::dlasdt
                    fortran-to-lisp::dlasdq fortran-to-lisp::xerbla))))

