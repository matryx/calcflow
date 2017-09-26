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


(let* ((zero 0.0) (one 1.0))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (ignorable zero one))
  (defun dlamc5 (beta p emin ieee emax rmax)
    (declare (type (double-float) rmax)
             (type f2cl-lib:logical ieee)
             (type (f2cl-lib:integer4) emax emin p beta))
    (prog ((oldy 0.0) (recbas 0.0) (y 0.0) (z 0.0) (exbits 0) (expsum 0) (i 0)
           (lexp 0) (nbits 0) (try 0) (uexp 0))
      (declare (type (double-float) oldy recbas y z)
               (type (f2cl-lib:integer4) exbits expsum i lexp nbits try uexp))
      (setf lexp 1)
      (setf exbits 1)
     label10
      (setf try (f2cl-lib:int-mul lexp 2))
      (cond
        ((<= try (f2cl-lib:int-sub emin))
         (setf lexp try)
         (setf exbits (f2cl-lib:int-add exbits 1))
         (go label10)))
      (cond
        ((= lexp (f2cl-lib:int-sub emin))
         (setf uexp lexp))
        (t
         (setf uexp try)
         (setf exbits (f2cl-lib:int-add exbits 1))))
      (cond
        ((> (f2cl-lib:int-add uexp emin)
            (f2cl-lib:int-add (f2cl-lib:int-sub lexp) (f2cl-lib:int-sub emin)))
         (setf expsum (f2cl-lib:int-mul 2 lexp)))
        (t
         (setf expsum (f2cl-lib:int-mul 2 uexp))))
      (setf emax (f2cl-lib:int-sub (f2cl-lib:int-add expsum emin) 1))
      (setf nbits (f2cl-lib:int-add 1 exbits p))
      (cond
        ((and (= (mod nbits 2) 1) (= beta 2))
         (setf emax (f2cl-lib:int-sub emax 1))))
      (cond
        (ieee
         (setf emax (f2cl-lib:int-sub emax 1))))
      (setf recbas (/ one beta))
      (setf z (- beta one))
      (setf y zero)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i p) nil)
        (tagbody
          (setf z (* z recbas))
          (if (< y one) (setf oldy y))
          (setf y (dlamc3 y z))
         label20))
      (if (>= y one) (setf y oldy))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i emax) nil)
        (tagbody (setf y (dlamc3 (* y beta) zero)) label30))
      (setf rmax y)
      (go end_label)
     end_label
      (return (values nil nil nil nil emax rmax)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlamc5
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) fortran-to-lisp::logical
                        (fortran-to-lisp::integer4) (double-float))
           :return-values '(nil nil nil nil fortran-to-lisp::emax
                            fortran-to-lisp::rmax)
           :calls '(fortran-to-lisp::dlamc3))))

