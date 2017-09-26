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


(let* ((itmax 5) (zero 0.0) (one 1.0) (two 2.0))
  (declare (type (f2cl-lib:integer4 5 5) itmax)
           (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (type (double-float 2.0 2.0) two)
           (ignorable itmax zero one two))
  (let ((jump 0)
        (jlast 0)
        (j 0)
        (iter 0)
        (i 0)
        (temp 0.0)
        (estold 0.0)
        (altsgn 0.0))
    (declare (type (f2cl-lib:integer4) jump jlast j iter i)
             (type (double-float) temp estold altsgn))
    (defun dlacon (n v x isgn est kase)
      (declare (type (double-float) est)
               (type (array f2cl-lib:integer4 (*)) isgn)
               (type (array double-float (*)) x v)
               (type (f2cl-lib:integer4) kase n))
      (f2cl-lib:with-multi-array-data
          ((v double-float v-%data% v-%offset%)
           (x double-float x-%data% x-%offset%)
           (isgn f2cl-lib:integer4 isgn-%data% isgn-%offset%))
        (prog ()
          (declare)
          (cond
            ((= kase 0)
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i n) nil)
               (tagbody
                 (setf (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%)
                         (/ one (f2cl-lib:dble n)))
                label10))
             (setf kase 1)
             (setf jump 1)
             (go end_label)))
          (f2cl-lib:computed-goto (label20 label40 label70 label110 label140)
                                  jump)
         label20
          (cond
            ((= n 1)
             (setf (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%)
                     (f2cl-lib:fref x-%data% (1) ((1 *)) x-%offset%))
             (setf est (abs (f2cl-lib:fref v-%data% (1) ((1 *)) v-%offset%)))
             (go label150)))
          (setf est (dasum n x 1))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%)
                      (f2cl-lib:sign one
                                     (f2cl-lib:fref x-%data%
                                                    (i)
                                                    ((1 *))
                                                    x-%offset%)))
              (setf (f2cl-lib:fref isgn-%data% (i) ((1 *)) isgn-%offset%)
                      (f2cl-lib:nint
                       (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%)))
             label30))
          (setf kase 2)
          (setf jump 2)
          (go end_label)
         label40
          (setf j (idamax n x 1))
          (setf iter 2)
         label50
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%) zero)
             label60))
          (setf (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%) one)
          (setf kase 1)
          (setf jump 3)
          (go end_label)
         label70
          (dcopy n x 1 v 1)
          (setf estold est)
          (setf est (dasum n v 1))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (if
               (/=
                (f2cl-lib:nint
                 (f2cl-lib:sign one
                                (f2cl-lib:fref x-%data%
                                               (i)
                                               ((1 *))
                                               x-%offset%)))
                (f2cl-lib:fref isgn-%data% (i) ((1 *)) isgn-%offset%))
               (go label90))
             label80))
          (go label120)
         label90
          (if (<= est estold) (go label120))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%)
                      (f2cl-lib:sign one
                                     (f2cl-lib:fref x-%data%
                                                    (i)
                                                    ((1 *))
                                                    x-%offset%)))
              (setf (f2cl-lib:fref isgn-%data% (i) ((1 *)) isgn-%offset%)
                      (f2cl-lib:nint
                       (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%)))
             label100))
          (setf kase 2)
          (setf jump 4)
          (go end_label)
         label110
          (setf jlast j)
          (setf j (idamax n x 1))
          (cond
            ((and
              (/= (f2cl-lib:fref x (jlast) ((1 *)))
                  (abs (f2cl-lib:fref x (j) ((1 *)))))
              (< iter itmax))
             (setf iter (f2cl-lib:int-add iter 1))
             (go label50)))
         label120
          (setf altsgn one)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%)
                      (* altsgn
                         (+ one
                            (/ (f2cl-lib:dble (f2cl-lib:int-sub i 1))
                               (f2cl-lib:dble (f2cl-lib:int-sub n 1))))))
              (setf altsgn (- altsgn))
             label130))
          (setf kase 1)
          (setf jump 5)
          (go end_label)
         label140
          (setf temp
                  (* two
                     (/ (dasum n x 1) (f2cl-lib:dble (f2cl-lib:int-mul 3 n)))))
          (cond
            ((> temp est)
             (dcopy n x 1 v 1)
             (setf est temp)))
         label150
          (setf kase 0)
          (go end_label)
         end_label
          (return (values nil nil nil nil est kase)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlacon
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)) (double-float)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil fortran-to-lisp::est
                            fortran-to-lisp::kase)
           :calls '(fortran-to-lisp::dcopy fortran-to-lisp::idamax
                    fortran-to-lisp::dasum))))

