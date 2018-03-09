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
  (let ((first$ nil)
        (prec 0.0)
        (rmax 0.0)
        (emax 0.0)
        (rmin 0.0)
        (emin 0.0)
        (rnd 0.0)
        (t$ 0.0)
        (base 0.0)
        (sfmin 0.0)
        (eps 0.0))
    (declare (type f2cl-lib:logical first$)
             (type (double-float) prec rmax emax rmin emin rnd t$ base sfmin
                                  eps))
    (setq first$ f2cl-lib:%true%)
    (defun dlamch (cmach)
      (declare (type (simple-string *) cmach))
      (f2cl-lib:with-multi-array-data
          ((cmach character cmach-%data% cmach-%offset%))
        (prog ((rmach 0.0) (small 0.0) (beta 0) (imax 0) (imin 0) (it 0)
               (lrnd nil) (dlamch 0.0))
          (declare (type (f2cl-lib:integer4) beta imax imin it)
                   (type f2cl-lib:logical lrnd)
                   (type (double-float) rmach small dlamch))
          (cond
            (first$
             (setf first$ f2cl-lib:%false%)
             (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                 (dlamc2 beta it lrnd eps imin rmin imax rmax)
               (declare (ignore))
               (setf beta var-0)
               (setf it var-1)
               (setf lrnd var-2)
               (setf eps var-3)
               (setf imin var-4)
               (setf rmin var-5)
               (setf imax var-6)
               (setf rmax var-7))
             (setf base (coerce (the f2cl-lib:integer4 beta) 'double-float))
             (setf t$ (coerce (the f2cl-lib:integer4 it) 'double-float))
             (cond
               (lrnd
                (setf rnd one)
                (setf eps (/ (expt base (f2cl-lib:int-sub 1 it)) 2)))
               (t
                (setf rnd zero)
                (setf eps (expt base (f2cl-lib:int-sub 1 it)))))
             (setf prec (* eps base))
             (setf emin (coerce (the f2cl-lib:integer4 imin) 'double-float))
             (setf emax (coerce (the f2cl-lib:integer4 imax) 'double-float))
             (setf sfmin rmin)
             (setf small (/ one rmax))
             (cond
               ((>= small sfmin)
                (setf sfmin (* small (+ one eps)))))))
          (cond
            ((lsame cmach "E")
             (setf rmach eps))
            ((lsame cmach "S")
             (setf rmach sfmin))
            ((lsame cmach "B")
             (setf rmach base))
            ((lsame cmach "P")
             (setf rmach prec))
            ((lsame cmach "N")
             (setf rmach t$))
            ((lsame cmach "R")
             (setf rmach rnd))
            ((lsame cmach "M")
             (setf rmach emin))
            ((lsame cmach "U")
             (setf rmach rmin))
            ((lsame cmach "L")
             (setf rmach emax))
            ((lsame cmach "O")
             (setf rmach rmax)))
          (setf dlamch rmach)
          (go end_label)
         end_label
          (return (values dlamch nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlamch
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((simple-string))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::lsame
                                                     fortran-to-lisp::dlamc2))))

