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


(let* ((one 1.0) (zero 0.0))
  (declare (type (double-float 1.0 1.0) one)
           (type (double-float 0.0 0.0) zero)
           (ignorable one zero))
  (defun dlasd6
         (icompq nl nr sqre d vf vl alpha beta idxq perm givptr givcol ldgcol
          givnum ldgnum poles difl difr z k c s work iwork info)
    (declare (type (array f2cl-lib:integer4 (*)) iwork givcol perm idxq)
             (type (double-float) s c beta alpha)
             (type (array double-float (*)) work z difr difl poles givnum vl vf
                                            d)
             (type (f2cl-lib:integer4) info k ldgnum ldgcol givptr sqre nr nl
                                       icompq))
    (f2cl-lib:with-multi-array-data
        ((d double-float d-%data% d-%offset%)
         (vf double-float vf-%data% vf-%offset%)
         (vl double-float vl-%data% vl-%offset%)
         (givnum double-float givnum-%data% givnum-%offset%)
         (poles double-float poles-%data% poles-%offset%)
         (difl double-float difl-%data% difl-%offset%)
         (difr double-float difr-%data% difr-%offset%)
         (z double-float z-%data% z-%offset%)
         (work double-float work-%data% work-%offset%)
         (idxq f2cl-lib:integer4 idxq-%data% idxq-%offset%)
         (perm f2cl-lib:integer4 perm-%data% perm-%offset%)
         (givcol f2cl-lib:integer4 givcol-%data% givcol-%offset%)
         (iwork f2cl-lib:integer4 iwork-%data% iwork-%offset%))
      (prog ((orgnrm 0.0) (i 0) (idx 0) (idxc 0) (idxp 0) (isigma 0) (ivfw 0)
             (ivlw 0) (iw 0) (m 0) (n 0) (n1 0) (n2 0))
        (declare (type (double-float) orgnrm)
                 (type (f2cl-lib:integer4) i idx idxc idxp isigma ivfw ivlw iw
                                           m n n1 n2))
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
           (setf info -14))
          ((< ldgnum n)
           (setf info -16)))
        (cond
          ((/= info 0)
           (xerbla "DLASD6" (f2cl-lib:int-sub info))
           (go end_label)))
        (setf isigma 1)
        (setf iw (f2cl-lib:int-add isigma n))
        (setf ivfw (f2cl-lib:int-add iw m))
        (setf ivlw (f2cl-lib:int-add ivfw m))
        (setf idx 1)
        (setf idxc (f2cl-lib:int-add idx n))
        (setf idxp (f2cl-lib:int-add idxc n))
        (setf orgnrm (max (abs alpha) (abs beta)))
        (setf (f2cl-lib:fref d-%data%
                             ((f2cl-lib:int-add nl 1))
                             ((1 *))
                             d-%offset%)
                zero)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (cond
              ((> (abs (f2cl-lib:fref d (i) ((1 *)))) orgnrm)
               (setf orgnrm
                       (abs (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)))))
           label10))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
            (dlascl "G" 0 0 orgnrm one n 1 d n info)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8))
          (setf info var-9))
        (setf alpha (/ alpha orgnrm))
        (setf beta (/ beta orgnrm))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
               var-19 var-20 var-21 var-22 var-23 var-24 var-25 var-26)
            (dlasd7 icompq nl nr sqre k d z
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (iw)
                                   ((1 *))
                                   work-%offset%)
             vf
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (ivfw)
                                   ((1 *))
                                   work-%offset%)
             vl
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (ivlw)
                                   ((1 *))
                                   work-%offset%)
             alpha beta
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (isigma)
                                   ((1 *))
                                   work-%offset%)
             (f2cl-lib:array-slice iwork-%data%
                                   f2cl-lib:integer4
                                   (idx)
                                   ((1 *))
                                   iwork-%offset%)
             (f2cl-lib:array-slice iwork-%data%
                                   f2cl-lib:integer4
                                   (idxp)
                                   ((1 *))
                                   iwork-%offset%)
             idxq perm givptr givcol ldgcol givnum ldgnum c s info)
          (declare (ignore var-0 var-1 var-2 var-3 var-5 var-6 var-7 var-8
                           var-9 var-10 var-11 var-12 var-13 var-14 var-15
                           var-16 var-17 var-18 var-20 var-21 var-22 var-23))
          (setf k var-4)
          (setf givptr var-19)
          (setf c var-24)
          (setf s var-25)
          (setf info var-26))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11)
            (dlasd8 icompq k d z vf vl difl difr ldgnum
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (isigma)
                                   ((1 *))
                                   work-%offset%)
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (iw)
                                   ((1 *))
                                   work-%offset%)
             info)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10))
          (setf info var-11))
        (cond
          ((= icompq 1)
           (dcopy k d 1
            (f2cl-lib:array-slice poles-%data%
                                  double-float
                                  (1 1)
                                  ((1 ldgnum) (1 *))
                                  poles-%offset%)
            1)
           (dcopy k
            (f2cl-lib:array-slice work-%data%
                                  double-float
                                  (isigma)
                                  ((1 *))
                                  work-%offset%)
            1
            (f2cl-lib:array-slice poles-%data%
                                  double-float
                                  (1 2)
                                  ((1 ldgnum) (1 *))
                                  poles-%offset%)
            1)))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
            (dlascl "G" 0 0 one orgnrm n 1 d n info)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8))
          (setf info var-9))
        (setf n1 k)
        (setf n2 (f2cl-lib:int-sub n k))
        (dlamrg n1 n2 d 1 -1 idxq)
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
                 alpha
                 beta
                 nil
                 nil
                 givptr
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 k
                 c
                 s
                 nil
                 nil
                 info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasd6
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (double-float) (double-float)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (double-float) (double-float) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil fortran-to-lisp::alpha
                            fortran-to-lisp::beta nil nil
                            fortran-to-lisp::givptr nil nil nil nil nil nil nil
                            nil fortran-to-lisp::k fortran-to-lisp::c
                            fortran-to-lisp::s nil nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlamrg fortran-to-lisp::dcopy
                    fortran-to-lisp::dlasd8 fortran-to-lisp::dlasd7
                    fortran-to-lisp::dlascl fortran-to-lisp::xerbla))))

