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
  (defun dlasd1 (nl nr sqre d alpha beta u ldu vt ldvt idxq iwork work info)
    (declare (type (array f2cl-lib:integer4 (*)) iwork idxq)
             (type (double-float) beta alpha)
             (type (array double-float (*)) work vt u d)
             (type (f2cl-lib:integer4) info ldvt ldu sqre nr nl))
    (f2cl-lib:with-multi-array-data
        ((d double-float d-%data% d-%offset%)
         (u double-float u-%data% u-%offset%)
         (vt double-float vt-%data% vt-%offset%)
         (work double-float work-%data% work-%offset%)
         (idxq f2cl-lib:integer4 idxq-%data% idxq-%offset%)
         (iwork f2cl-lib:integer4 iwork-%data% iwork-%offset%))
      (prog ((orgnrm 0.0) (coltyp 0) (i 0) (idx 0) (idxc 0) (idxp 0) (iq 0)
             (isigma 0) (iu2 0) (ivt2 0) (iz 0) (k 0) (ldq 0) (ldu2 0)
             (ldvt2 0) (m 0) (n 0) (n1 0) (n2 0))
        (declare (type (double-float) orgnrm)
                 (type (f2cl-lib:integer4) coltyp i idx idxc idxp iq isigma iu2
                                           ivt2 iz k ldq ldu2 ldvt2 m n n1 n2))
        (setf info 0)
        (cond
          ((< nl 1)
           (setf info -1))
          ((< nr 1)
           (setf info -2))
          ((or (< sqre 0) (> sqre 1))
           (setf info -3)))
        (cond
          ((/= info 0)
           (xerbla "DLASD1" (f2cl-lib:int-sub info))
           (go end_label)))
        (setf n (f2cl-lib:int-add nl nr 1))
        (setf m (f2cl-lib:int-add n sqre))
        (setf ldu2 n)
        (setf ldvt2 m)
        (setf iz 1)
        (setf isigma (f2cl-lib:int-add iz m))
        (setf iu2 (f2cl-lib:int-add isigma n))
        (setf ivt2 (f2cl-lib:int-add iu2 (f2cl-lib:int-mul ldu2 n)))
        (setf iq (f2cl-lib:int-add ivt2 (f2cl-lib:int-mul ldvt2 m)))
        (setf idx 1)
        (setf idxc (f2cl-lib:int-add idx n))
        (setf coltyp (f2cl-lib:int-add idxc n))
        (setf idxp (f2cl-lib:int-add coltyp n))
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
               var-19 var-20 var-21 var-22)
            (dlasd2 nl nr sqre k d
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (iz)
                                   ((1 *))
                                   work-%offset%)
             alpha beta u ldu vt ldvt
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (isigma)
                                   ((1 *))
                                   work-%offset%)
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (iu2)
                                   ((1 *))
                                   work-%offset%)
             ldu2
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (ivt2)
                                   ((1 *))
                                   work-%offset%)
             ldvt2
             (f2cl-lib:array-slice iwork-%data%
                                   f2cl-lib:integer4
                                   (idxp)
                                   ((1 *))
                                   iwork-%offset%)
             (f2cl-lib:array-slice iwork-%data%
                                   f2cl-lib:integer4
                                   (idx)
                                   ((1 *))
                                   iwork-%offset%)
             (f2cl-lib:array-slice iwork-%data%
                                   f2cl-lib:integer4
                                   (idxc)
                                   ((1 *))
                                   iwork-%offset%)
             idxq
             (f2cl-lib:array-slice iwork-%data%
                                   f2cl-lib:integer4
                                   (coltyp)
                                   ((1 *))
                                   iwork-%offset%)
             info)
          (declare (ignore var-0 var-1 var-2 var-4 var-5 var-6 var-7 var-8
                           var-9 var-10 var-11 var-12 var-13 var-14 var-15
                           var-16 var-17 var-18 var-19 var-20 var-21))
          (setf k var-3)
          (setf info var-22))
        (setf ldq k)
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
               var-19)
            (dlasd3 nl nr sqre k d
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (iq)
                                   ((1 *))
                                   work-%offset%)
             ldq
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (isigma)
                                   ((1 *))
                                   work-%offset%)
             u ldu
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (iu2)
                                   ((1 *))
                                   work-%offset%)
             ldu2 vt ldvt
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (ivt2)
                                   ((1 *))
                                   work-%offset%)
             ldvt2
             (f2cl-lib:array-slice iwork-%data%
                                   f2cl-lib:integer4
                                   (idxc)
                                   ((1 *))
                                   iwork-%offset%)
             (f2cl-lib:array-slice iwork-%data%
                                   f2cl-lib:integer4
                                   (coltyp)
                                   ((1 *))
                                   iwork-%offset%)
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (iz)
                                   ((1 *))
                                   work-%offset%)
             info)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13 var-14
                           var-15 var-16 var-17 var-18))
          (setf info var-19))
        (cond
          ((/= info 0)
           (go end_label)))
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
                 alpha
                 beta
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
  (setf (gethash 'fortran-to-lisp::dlasd1
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (double-float) (double-float) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil fortran-to-lisp::alpha
                            fortran-to-lisp::beta nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlamrg fortran-to-lisp::dlasd3
                    fortran-to-lisp::dlasd2 fortran-to-lisp::dlascl
                    fortran-to-lisp::xerbla))))

