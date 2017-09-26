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
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun zabs (zr zi)
  (declare (type (double-float) zi zr))
  (prog ((u 0.0) (v 0.0) (q 0.0) (s 0.0) (zabs 0.0))
    (declare (type (double-float) zabs s q v u))
    (setf u (abs zr))
    (setf v (abs zi))
    (setf s (+ u v))
    (setf s (* s 1.0))
    (if (= s 0.0) (go label20))
    (if (> u v) (go label10))
    (setf q (/ u v))
    (setf zabs (* v (f2cl-lib:fsqrt (+ 1.0 (* q q)))))
    (go end_label)
   label10
    (setf q (/ v u))
    (setf zabs (* u (f2cl-lib:fsqrt (+ 1.0 (* q q)))))
    (go end_label)
   label20
    (setf zabs 0.0)
    (go end_label)
   end_label
    (return (values zabs nil nil))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zabs fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float))
           :return-values '(nil nil)
           :calls 'nil)))

