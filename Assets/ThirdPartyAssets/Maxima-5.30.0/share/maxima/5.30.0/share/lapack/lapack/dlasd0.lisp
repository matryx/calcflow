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


(defun dlasd0 (n sqre d e u ldu vt ldvt smlsiz iwork work info)
  (declare (type (array f2cl-lib:integer4 (*)) iwork)
           (type (array double-float (*)) work vt u e d)
           (type (f2cl-lib:integer4) info smlsiz ldvt ldu sqre n))
  (f2cl-lib:with-multi-array-data
      ((d double-float d-%data% d-%offset%)
       (e double-float e-%data% e-%offset%)
       (u double-float u-%data% u-%offset%)
       (vt double-float vt-%data% vt-%offset%)
       (work double-float work-%data% work-%offset%)
       (iwork f2cl-lib:integer4 iwork-%data% iwork-%offset%))
    (prog ((alpha 0.0) (beta 0.0) (i 0) (i1 0) (ic 0) (idxq 0) (idxqc 0)
           (im1 0) (inode 0) (itemp 0) (iwk 0) (j 0) (lf 0) (ll 0) (lvl 0)
           (m 0) (ncc 0) (nd 0) (ndb1 0) (ndiml 0) (ndimr 0) (nl 0) (nlf 0)
           (nlp1 0) (nlvl 0) (nr 0) (nrf 0) (nrp1 0) (sqrei 0))
      (declare (type (f2cl-lib:integer4) sqrei nrp1 nrf nr nlvl nlp1 nlf nl
                                         ndimr ndiml ndb1 nd ncc m lvl ll lf j
                                         iwk itemp inode im1 idxqc idxq ic i1
                                         i)
               (type (double-float) beta alpha))
      (setf info 0)
      (cond
        ((< n 0)
         (setf info -1))
        ((or (< sqre 0) (> sqre 1))
         (setf info -2)))
      (setf m (f2cl-lib:int-add n sqre))
      (cond
        ((< ldu n)
         (setf info -6))
        ((< ldvt m)
         (setf info -8))
        ((< smlsiz 3)
         (setf info -9)))
      (cond
        ((/= info 0)
         (xerbla "DLASD0" (f2cl-lib:int-sub info))
         (go end_label)))
      (cond
        ((<= n smlsiz)
         (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                var-10 var-11 var-12 var-13 var-14 var-15)
             (dlasdq "U" sqre n m n 0 d e vt ldvt u ldu u ldu work info)
           (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                            var-8 var-9 var-10 var-11 var-12 var-13 var-14))
           (setf info var-15))
         (go end_label)))
      (setf inode 1)
      (setf ndiml (f2cl-lib:int-add inode n))
      (setf ndimr (f2cl-lib:int-add ndiml n))
      (setf idxq (f2cl-lib:int-add ndimr n))
      (setf iwk (f2cl-lib:int-add idxq n))
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
      (setf ncc 0)
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
          (setf nrp1 (f2cl-lib:int-add nr 1))
          (setf nlf (f2cl-lib:int-sub ic nl))
          (setf nrf (f2cl-lib:int-add ic 1))
          (setf sqrei 1)
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15)
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
                                     (nlf nlf)
                                     ((1 ldvt) (1 *))
                                     vt-%offset%)
               ldvt
               (f2cl-lib:array-slice u-%data%
                                     double-float
                                     (nlf nlf)
                                     ((1 ldu) (1 *))
                                     u-%offset%)
               ldu
               (f2cl-lib:array-slice u-%data%
                                     double-float
                                     (nlf nlf)
                                     ((1 ldu) (1 *))
                                     u-%offset%)
               ldu work info)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-10 var-11 var-12 var-13 var-14))
            (setf info var-15))
          (cond
            ((/= info 0)
             (go end_label)))
          (setf itemp (f2cl-lib:int-sub (f2cl-lib:int-add idxq nlf) 2))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j nl) nil)
            (tagbody
              (setf (f2cl-lib:fref iwork-%data%
                                   ((f2cl-lib:int-add itemp j))
                                   ((1 *))
                                   iwork-%offset%)
                      j)
             label10))
          (cond
            ((= i nd)
             (setf sqrei sqre))
            (t
             (setf sqrei 1)))
          (setf nrp1 (f2cl-lib:int-add nr sqrei))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15)
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
                                     (nrf nrf)
                                     ((1 ldvt) (1 *))
                                     vt-%offset%)
               ldvt
               (f2cl-lib:array-slice u-%data%
                                     double-float
                                     (nrf nrf)
                                     ((1 ldu) (1 *))
                                     u-%offset%)
               ldu
               (f2cl-lib:array-slice u-%data%
                                     double-float
                                     (nrf nrf)
                                     ((1 ldu) (1 *))
                                     u-%offset%)
               ldu work info)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-10 var-11 var-12 var-13 var-14))
            (setf info var-15))
          (cond
            ((/= info 0)
             (go end_label)))
          (setf itemp (f2cl-lib:int-add idxq ic))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j nr) nil)
            (tagbody
              (setf (f2cl-lib:fref iwork-%data%
                                   ((f2cl-lib:int-sub
                                     (f2cl-lib:int-add itemp j)
                                     1))
                                   ((1 *))
                                   iwork-%offset%)
                      j)
             label20))
         label30))
      (f2cl-lib:fdo (lvl nlvl (f2cl-lib:int-add lvl (f2cl-lib:int-sub 1)))
                    ((> lvl 1) nil)
        (tagbody
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
              (cond
                ((and (= sqre 0) (= i ll))
                 (setf sqrei sqre))
                (t
                 (setf sqrei 1)))
              (setf idxqc (f2cl-lib:int-sub (f2cl-lib:int-add idxq nlf) 1))
              (setf alpha (f2cl-lib:fref d-%data% (ic) ((1 *)) d-%offset%))
              (setf beta (f2cl-lib:fref e-%data% (ic) ((1 *)) e-%offset%))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13)
                  (dlasd1 nl nr sqrei
                   (f2cl-lib:array-slice d-%data%
                                         double-float
                                         (nlf)
                                         ((1 *))
                                         d-%offset%)
                   alpha beta
                   (f2cl-lib:array-slice u-%data%
                                         double-float
                                         (nlf nlf)
                                         ((1 ldu) (1 *))
                                         u-%offset%)
                   ldu
                   (f2cl-lib:array-slice vt-%data%
                                         double-float
                                         (nlf nlf)
                                         ((1 ldvt) (1 *))
                                         vt-%offset%)
                   ldvt
                   (f2cl-lib:array-slice iwork-%data%
                                         f2cl-lib:integer4
                                         (idxqc)
                                         ((1 *))
                                         iwork-%offset%)
                   (f2cl-lib:array-slice iwork-%data%
                                         f2cl-lib:integer4
                                         (iwk)
                                         ((1 *))
                                         iwork-%offset%)
                   work info)
                (declare (ignore var-0 var-1 var-2 var-3 var-6 var-7 var-8
                                 var-9 var-10 var-11 var-12))
                (setf alpha var-4)
                (setf beta var-5)
                (setf info var-13))
              (cond
                ((/= info 0)
                 (go end_label)))
             label40))
         label50))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil nil nil nil info)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasd0
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlasd1 fortran-to-lisp::dlasdt
                    fortran-to-lisp::dlasdq fortran-to-lisp::xerbla))))

