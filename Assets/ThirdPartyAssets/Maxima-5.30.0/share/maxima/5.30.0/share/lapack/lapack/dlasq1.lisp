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


(let* ((zero 0.0))
  (declare (type (double-float 0.0 0.0) zero) (ignorable zero))
  (defun dlasq1 (n d e work info)
    (declare (type (array double-float (*)) work e d)
             (type (f2cl-lib:integer4) info n))
    (f2cl-lib:with-multi-array-data
        ((d double-float d-%data% d-%offset%)
         (e double-float e-%data% e-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((eps 0.0) (scale 0.0) (safmin 0.0) (sigmn 0.0) (sigmx 0.0) (i 0)
             (iinfo 0))
        (declare (type (double-float) eps scale safmin sigmn sigmx)
                 (type (f2cl-lib:integer4) i iinfo))
        (setf info 0)
        (cond
          ((< n 0)
           (setf info -2)
           (xerbla "DLASQ1" (f2cl-lib:int-sub info))
           (go end_label))
          ((= n 0)
           (go end_label))
          ((= n 1)
           (setf (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%)
                   (abs (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%)))
           (go end_label))
          ((= n 2)
           (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
               (dlas2 (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%)
                (f2cl-lib:fref e-%data% (1) ((1 *)) e-%offset%)
                (f2cl-lib:fref d-%data% (2) ((1 *)) d-%offset%) sigmn sigmx)
             (declare (ignore var-0 var-1 var-2))
             (setf sigmn var-3)
             (setf sigmx var-4))
           (setf (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%) sigmx)
           (setf (f2cl-lib:fref d-%data% (2) ((1 *)) d-%offset%) sigmn)
           (go end_label)))
        (setf sigmx zero)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
          (tagbody
            (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                    (abs (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)))
            (setf sigmx
                    (max sigmx
                         (abs
                          (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%))))
           label10))
        (setf (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                (abs (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)))
        (cond
          ((= sigmx zero)
           (multiple-value-bind (var-0 var-1 var-2 var-3)
               (dlasrt "D" n d iinfo)
             (declare (ignore var-0 var-1 var-2))
             (setf iinfo var-3))
           (go end_label)))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf sigmx
                    (max sigmx
                         (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)))
           label20))
        (setf eps (dlamch "Precision"))
        (setf safmin (dlamch "Safe minimum"))
        (setf scale (f2cl-lib:fsqrt (/ eps safmin)))
        (dcopy n d 1
         (f2cl-lib:array-slice work-%data%
                               double-float
                               (1)
                               ((1 *))
                               work-%offset%)
         2)
        (dcopy (f2cl-lib:int-sub n 1) e 1
         (f2cl-lib:array-slice work-%data%
                               double-float
                               (2)
                               ((1 *))
                               work-%offset%)
         2)
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
            (dlascl "G" 0 0 sigmx scale
             (f2cl-lib:int-sub (f2cl-lib:int-mul 2 n) 1) 1 work
             (f2cl-lib:int-sub (f2cl-lib:int-mul 2 n) 1) iinfo)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8))
          (setf iinfo var-9))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i
                          (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                            (f2cl-lib:int-sub 1)))
                       nil)
          (tagbody
            (setf (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%)
                    (expt (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%)
                          2))
           label30))
        (setf (f2cl-lib:fref work-%data%
                             ((f2cl-lib:int-mul 2 n))
                             ((1 *))
                             work-%offset%)
                zero)
        (multiple-value-bind (var-0 var-1 var-2)
            (dlasq2 n work info)
          (declare (ignore var-0 var-1))
          (setf info var-2))
        (cond
          ((= info 0)
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i n) nil)
             (tagbody
               (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                       (f2cl-lib:fsqrt
                        (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%)))
              label40))
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
               (dlascl "G" 0 0 scale sigmx n 1 d n iinfo)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8))
             (setf iinfo var-9))))
        (go end_label)
       end_label
        (return (values nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasq1
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlasq2 fortran-to-lisp::dlascl
                    fortran-to-lisp::dcopy fortran-to-lisp::dlamch
                    fortran-to-lisp::dlasrt fortran-to-lisp::dlas2
                    fortran-to-lisp::xerbla))))

