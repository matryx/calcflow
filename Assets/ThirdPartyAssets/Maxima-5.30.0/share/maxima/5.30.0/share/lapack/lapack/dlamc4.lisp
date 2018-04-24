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


(defun dlamc4 (emin start base)
  (declare (type (double-float) start) (type (f2cl-lib:integer4) base emin))
  (prog ((a 0.0) (b1 0.0) (b2 0.0) (c1 0.0) (c2 0.0) (d1 0.0) (d2 0.0)
         (one 0.0) (rbase 0.0) (zero 0.0) (i 0))
    (declare (type (f2cl-lib:integer4) i)
             (type (double-float) zero rbase one d2 d1 c2 c1 b2 b1 a))
    (setf a start)
    (setf one (coerce (the f2cl-lib:integer4 1) 'double-float))
    (setf rbase (/ one base))
    (setf zero (coerce (the f2cl-lib:integer4 0) 'double-float))
    (setf emin 1)
    (setf b1 (dlamc3 (* a rbase) zero))
    (setf c1 a)
    (setf c2 a)
    (setf d1 a)
    (setf d2 a)
   label10
    (cond
      ((and (= c1 a) (= c2 a) (= d1 a) (= d2 a))
       (setf emin (f2cl-lib:int-sub emin 1))
       (setf a b1)
       (setf b1 (dlamc3 (/ a base) zero))
       (setf c1 (dlamc3 (* b1 base) zero))
       (setf d1 zero)
       (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                     ((> i base) nil)
         (tagbody (setf d1 (+ d1 b1)) label20))
       (setf b2 (dlamc3 (* a rbase) zero))
       (setf c2 (dlamc3 (/ b2 rbase) zero))
       (setf d2 zero)
       (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                     ((> i base) nil)
         (tagbody (setf d2 (+ d2 b2)) label30))
       (go label10)))
    (go end_label)
   end_label
    (return (values emin nil nil))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlamc4
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (double-float)
                        (fortran-to-lisp::integer4))
           :return-values '(fortran-to-lisp::emin nil nil)
           :calls '(fortran-to-lisp::dlamc3))))

