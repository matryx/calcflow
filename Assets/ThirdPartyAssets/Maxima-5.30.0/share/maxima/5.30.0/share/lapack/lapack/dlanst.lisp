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
  (defun dlanst (norm n d e)
    (declare (type (array double-float (*)) e d)
             (type (f2cl-lib:integer4) n)
             (type (simple-string *) norm))
    (f2cl-lib:with-multi-array-data
        ((norm character norm-%data% norm-%offset%)
         (d double-float d-%data% d-%offset%)
         (e double-float e-%data% e-%offset%))
      (prog ((anorm 0.0) (scale 0.0) (sum 0.0) (i 0) (dlanst 0.0))
        (declare (type (f2cl-lib:integer4) i)
                 (type (double-float) anorm scale sum dlanst))
        (cond
          ((<= n 0)
           (setf anorm zero))
          ((lsame norm "M")
           (setf anorm (abs (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)))
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
             (tagbody
               (setf anorm
                       (max anorm
                            (abs
                             (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))))
               (setf anorm
                       (max anorm
                            (abs
                             (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%))))
              label10)))
          ((or (lsame norm "O") (f2cl-lib:fstring-= norm "1") (lsame norm "I"))
           (cond
             ((= n 1)
              (setf anorm
                      (abs (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%))))
             (t
              (setf anorm
                      (max
                       (+ (abs (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%))
                          (abs
                           (f2cl-lib:fref e-%data% (1) ((1 *)) e-%offset%)))
                       (+
                        (abs
                         (f2cl-lib:fref e-%data%
                                        ((f2cl-lib:int-sub n 1))
                                        ((1 *))
                                        e-%offset%))
                        (abs (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)))))
              (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                            ((> i (f2cl-lib:int-add n (f2cl-lib:int-sub 1)))
                             nil)
                (tagbody
                  (setf anorm
                          (max anorm
                               (+
                                (abs
                                 (f2cl-lib:fref d-%data%
                                                (i)
                                                ((1 *))
                                                d-%offset%))
                                (abs
                                 (f2cl-lib:fref e-%data%
                                                (i)
                                                ((1 *))
                                                e-%offset%))
                                (abs
                                 (f2cl-lib:fref e-%data%
                                                ((f2cl-lib:int-sub i 1))
                                                ((1 *))
                                                e-%offset%)))))
                 label20)))))
          ((or (lsame norm "F") (lsame norm "E"))
           (setf scale zero)
           (setf sum one)
           (cond
             ((> n 1)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlassq (f2cl-lib:int-sub n 1) e 1 scale sum)
                (declare (ignore var-0 var-1 var-2))
                (setf scale var-3)
                (setf sum var-4))
              (setf sum (* 2 sum))))
           (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
               (dlassq n d 1 scale sum)
             (declare (ignore var-0 var-1 var-2))
             (setf scale var-3)
             (setf sum var-4))
           (setf anorm (* scale (f2cl-lib:fsqrt sum)))))
        (setf dlanst anorm)
        (go end_label)
       end_label
        (return (values dlanst nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlanst
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*)))
           :return-values '(nil nil nil nil)
           :calls '(fortran-to-lisp::dlassq fortran-to-lisp::lsame))))

