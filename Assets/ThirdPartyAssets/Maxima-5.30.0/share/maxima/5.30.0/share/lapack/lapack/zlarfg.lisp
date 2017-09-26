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
  (defun zlarfg (n alpha x incx tau)
    (declare (type (array f2cl-lib:complex16 (*)) x)
             (type (f2cl-lib:complex16) tau alpha)
             (type (f2cl-lib:integer4) incx n))
    (f2cl-lib:with-multi-array-data
        ((x f2cl-lib:complex16 x-%data% x-%offset%))
      (prog ((alphi 0.0) (alphr 0.0) (beta 0.0) (rsafmn 0.0) (safmin 0.0)
             (xnorm 0.0) (j 0) (knt 0))
        (declare (type (double-float) alphi alphr beta rsafmn safmin xnorm)
                 (type (f2cl-lib:integer4) j knt))
        (cond
          ((<= n 0)
           (setf tau (coerce zero 'f2cl-lib:complex16))
           (go end_label)))
        (setf xnorm (dznrm2 (f2cl-lib:int-sub n 1) x incx))
        (setf alphr (f2cl-lib:dble alpha))
        (setf alphi (f2cl-lib:dimag alpha))
        (cond
          ((and (= xnorm zero) (= alphi zero))
           (setf tau (coerce zero 'f2cl-lib:complex16)))
          (t
           (setf beta (- (f2cl-lib:sign (dlapy3 alphr alphi xnorm) alphr)))
           (setf safmin (/ (dlamch "S") (dlamch "E")))
           (setf rsafmn (/ one safmin))
           (setf knt 0)
           (cond
             ((< (abs beta) safmin)
              (tagbody
               label10
                (setf knt (f2cl-lib:int-add knt 1))
                (zdscal (f2cl-lib:int-sub n 1) rsafmn x incx)
                (setf beta (* beta rsafmn))
                (setf alphi (* alphi rsafmn))
                (setf alphr (* alphr rsafmn))
                (if (< (abs beta) safmin) (go label10))
                (setf xnorm (dznrm2 (f2cl-lib:int-sub n 1) x incx))
                (setf alpha (f2cl-lib:dcmplx alphr alphi))
                (setf beta
                        (- (f2cl-lib:sign (dlapy3 alphr alphi xnorm) alphr))))))
           (setf tau
                   (f2cl-lib:dcmplx (/ (- beta alphr) beta) (/ (- alphi) beta)))
           (setf alpha (zladiv (f2cl-lib:dcmplx one) (- alpha beta)))
           (zscal (f2cl-lib:int-sub n 1) alpha x incx)
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j knt) nil)
             (tagbody (setf beta (* beta safmin)) label20))
           (setf alpha (coerce beta 'f2cl-lib:complex16))))
        (go end_label)
       end_label
        (return (values nil alpha nil nil tau))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zlarfg
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (fortran-to-lisp::complex16)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::complex16))
           :return-values '(nil fortran-to-lisp::alpha nil nil
                            fortran-to-lisp::tau)
           :calls '(fortran-to-lisp::zscal fortran-to-lisp::zladiv
                    fortran-to-lisp::zdscal fortran-to-lisp::dlamch
                    fortran-to-lisp::dlapy3 fortran-to-lisp::dznrm2))))

