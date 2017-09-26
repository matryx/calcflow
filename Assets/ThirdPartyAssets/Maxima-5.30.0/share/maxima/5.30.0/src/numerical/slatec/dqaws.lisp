;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 46c1f6a93b0d 2012/05/03 04:40:28 toy $"
;;;  "f2cl2.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 46c1f6a93b0d 2012/05/03 04:40:28 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v fceac530ef0c 2011/11/26 04:02:26 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2012-04 (20C Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun dqaws
       (f a b alfa beta integr epsabs epsrel result abserr neval ier limit lenw
        last$ iwork work)
  (declare (type (array double-float (*)) work)
           (type (array f2cl-lib:integer4 (*)) iwork)
           (type (f2cl-lib:integer4) last$ lenw limit ier neval integr)
           (type (double-float) abserr result epsrel epsabs beta alfa b a))
  (f2cl-lib:with-multi-array-data
      ((iwork f2cl-lib:integer4 iwork-%data% iwork-%offset%)
       (work double-float work-%data% work-%offset%))
    (prog ((lvl 0) (l1 0) (l2 0) (l3 0))
      (declare (type (f2cl-lib:integer4) l3 l2 l1 lvl))
      (setf ier 6)
      (setf neval 0)
      (setf last$ 0)
      (setf result 0.0)
      (setf abserr 0.0)
      (if (or (< limit 2) (< lenw (f2cl-lib:int-mul limit 4))) (go label10))
      (setf l1 (f2cl-lib:int-add limit 1))
      (setf l2 (f2cl-lib:int-add limit l1))
      (setf l3 (f2cl-lib:int-add limit l2))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18)
          (dqawse f a b alfa beta integr epsabs epsrel limit result abserr
           neval ier
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
           iwork last$)
        (declare (ignore var-0 var-6 var-7 var-8 var-13 var-14 var-15 var-16
                         var-17))
        (setf a var-1)
        (setf b var-2)
        (setf alfa var-3)
        (setf beta var-4)
        (setf integr var-5)
        (setf result var-9)
        (setf abserr var-10)
        (setf neval var-11)
        (setf ier var-12)
        (setf last$ var-18))
      (setf lvl 0)
     label10
      (if (= ier 6) (setf lvl 1))
      (if (/= ier 0) (xermsg "SLATEC" "DQAWS" "ABNORMAL RETURN" ier lvl))
      (go end_label)
     end_label
      (return
       (values nil
               a
               b
               alfa
               beta
               integr
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
  (setf (gethash 'fortran-to-lisp::dqaws fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (double-float) (double-float) (double-float)
                        (double-float) (fortran-to-lisp::integer4)
                        (double-float) (double-float) (double-float)
                        (double-float) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)))
           :return-values '(nil fortran-to-lisp::a fortran-to-lisp::b
                            fortran-to-lisp::alfa fortran-to-lisp::beta
                            fortran-to-lisp::integr nil nil
                            fortran-to-lisp::result fortran-to-lisp::abserr
                            fortran-to-lisp::neval fortran-to-lisp::ier nil nil
                            fortran-to-lisp::last$ nil nil)
           :calls '(fortran-to-lisp::xermsg fortran-to-lisp::dqawse))))

