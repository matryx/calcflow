;;; Compiled by f2cl version:
;;; ("f2cl1.l,v f0f149e72999 2010/10/08 03:05:30 rtoy $"
;;;  "f2cl2.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 11bea7dae5a0 2011/06/11 17:53:39 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 11bea7dae5a0 2011/06/11 17:53:39 toy $")

;;; Using Lisp CMU Common Lisp Snapshot 2011-10 ce1db1c (20B Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun dqagp
       (f a b npts2 points epsabs epsrel result abserr neval ier leniw lenw
        last$ iwork work)
  (declare (type (array f2cl-lib:integer4 (*)) iwork)
           (type (array double-float (*)) work points)
           (type (f2cl-lib:integer4) last$ lenw leniw ier neval npts2)
           (type (double-float) abserr result epsrel epsabs b a))
  (f2cl-lib:with-multi-array-data
      ((points double-float points-%data% points-%offset%)
       (work double-float work-%data% work-%offset%)
       (iwork f2cl-lib:integer4 iwork-%data% iwork-%offset%))
    (prog ((limit 0) (lvl 0) (l1 0) (l2 0) (l3 0) (l4 0))
      (declare (type (f2cl-lib:integer4) l4 l3 l2 l1 lvl limit))
      (setf ier 6)
      (setf neval 0)
      (setf last$ 0)
      (setf result 0.0)
      (setf abserr 0.0)
      (if
       (or (< leniw (f2cl-lib:int-sub (f2cl-lib:int-mul 3 npts2) 2))
           (< lenw (f2cl-lib:int-sub (f2cl-lib:int-mul leniw 2) npts2))
           (< npts2 2))
       (go label10))
      (setf limit (the f2cl-lib:integer4 (truncate (- leniw npts2) 2)))
      (setf l1 (f2cl-lib:int-add limit 1))
      (setf l2 (f2cl-lib:int-add limit l1))
      (setf l3 (f2cl-lib:int-add limit l2))
      (setf l4 (f2cl-lib:int-add limit l3))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18 var-19
             var-20)
          (dqagpe f a b npts2 points epsabs epsrel limit result abserr neval
           ier
           (f2cl-lib:array-slice work-%data%
                                 double-float
                                 (1)
                                 ((1 *))
                                 work-%offset%)
           (f2cl-lib:array-slice work-%data%
                                 double-float
                                 (l1)
                                 ((1 *))
                                 work-%offset%)
           (f2cl-lib:array-slice work-%data%
                                 double-float
                                 (l2)
                                 ((1 *))
                                 work-%offset%)
           (f2cl-lib:array-slice work-%data%
                                 double-float
                                 (l3)
                                 ((1 *))
                                 work-%offset%)
           (f2cl-lib:array-slice work-%data%
                                 double-float
                                 (l4)
                                 ((1 *))
                                 work-%offset%)
           (f2cl-lib:array-slice iwork-%data%
                                 f2cl-lib:integer4
                                 (1)
                                 ((1 *))
                                 iwork-%offset%)
           (f2cl-lib:array-slice iwork-%data%
                                 f2cl-lib:integer4
                                 (l1)
                                 ((1 *))
                                 iwork-%offset%)
           (f2cl-lib:array-slice iwork-%data%
                                 f2cl-lib:integer4
                                 (l2)
                                 ((1 *))
                                 iwork-%offset%)
           last$)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-12
                         var-13 var-14 var-15 var-16 var-17 var-18 var-19))
        (setf result var-8)
        (setf abserr var-9)
        (setf neval var-10)
        (setf ier var-11)
        (setf last$ var-20))
      (setf lvl 0)
     label10
      (if (= ier 6) (setf lvl 1))
      (if (/= ier 0)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (xermsg "SLATEC" "DQAGP" "ABNORMAL RETURN" ier lvl)
            (declare (ignore var-0 var-1 var-2))
            (when var-3
              (setf ier var-3))
            (when var-4
              (setf lvl var-4))))
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
               result
               abserr
               neval
               ier
               nil
               nil
               last$
               nil
               nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dqagp fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (double-float) (double-float)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (double-float) (double-float) (double-float)
                        (double-float) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil fortran-to-lisp::result
                            fortran-to-lisp::abserr fortran-to-lisp::neval
                            fortran-to-lisp::ier nil nil fortran-to-lisp::last$
                            nil nil)
           :calls '(fortran-to-lisp::dqagpe))))

