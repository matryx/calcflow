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


(let ((lieee1 nil) (lbeta 0) (lrnd nil) (lt$ 0) (first$ nil))
  (declare (type f2cl-lib:logical lieee1 lrnd first$)
           (type (f2cl-lib:integer4) lbeta lt$))
  (setq first$ f2cl-lib:%true%)
  (defun dlamc1 (beta t$ rnd ieee1)
    (declare (type f2cl-lib:logical ieee1 rnd)
             (type (f2cl-lib:integer4) t$ beta))
    (prog ((a 0.0) (b 0.0) (c 0.0) (f 0.0) (one 0.0) (qtr 0.0) (savec 0.0)
           (t1 0.0) (t2 0.0))
      (declare (type (double-float) t2 t1 savec qtr one f c b a))
      (cond
        (first$
         (tagbody
           (setf first$ f2cl-lib:%false%)
           (setf one (coerce (the f2cl-lib:integer4 1) 'double-float))
           (setf a (coerce (the f2cl-lib:integer4 1) 'double-float))
           (setf c (coerce (the f2cl-lib:integer4 1) 'double-float))
          label10
           (cond
             ((= c one)
              (setf a (* 2 a))
              (setf c (dlamc3 a one))
              (setf c (dlamc3 c (- a)))
              (go label10)))
           (setf b (coerce (the f2cl-lib:integer4 1) 'double-float))
           (setf c (dlamc3 a b))
          label20
           (cond
             ((= c a)
              (setf b (* 2 b))
              (setf c (dlamc3 a b))
              (go label20)))
           (setf qtr (/ one 4))
           (setf savec c)
           (setf c (dlamc3 c (- a)))
           (setf lbeta (f2cl-lib:int (+ c qtr)))
           (setf b (coerce (the f2cl-lib:integer4 lbeta) 'double-float))
           (setf f (dlamc3 (/ b 2) (/ (- b) 100)))
           (setf c (dlamc3 f a))
           (cond
             ((= c a)
              (setf lrnd f2cl-lib:%true%))
             (t
              (setf lrnd f2cl-lib:%false%)))
           (setf f (dlamc3 (/ b 2) (/ b 100)))
           (setf c (dlamc3 f a))
           (if (and lrnd (= c a)) (setf lrnd f2cl-lib:%false%))
           (setf t1 (dlamc3 (/ b 2) a))
           (setf t2 (dlamc3 (/ b 2) savec))
           (setf lieee1 (and (= t1 a) (> t2 savec) lrnd))
           (setf lt$ 0)
           (setf a (coerce (the f2cl-lib:integer4 1) 'double-float))
           (setf c (coerce (the f2cl-lib:integer4 1) 'double-float))
          label30
           (cond
             ((= c one)
              (setf lt$ (f2cl-lib:int-add lt$ 1))
              (setf a (* a lbeta))
              (setf c (dlamc3 a one))
              (setf c (dlamc3 c (- a)))
              (go label30))))))
      (setf beta lbeta)
      (setf t$ lt$)
      (setf rnd lrnd)
      (setf ieee1 lieee1)
      (go end_label)
     end_label
      (return (values beta t$ rnd ieee1)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlamc1
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        fortran-to-lisp::logical fortran-to-lisp::logical)
           :return-values '(fortran-to-lisp::beta fortran-to-lisp::t$
                            fortran-to-lisp::rnd fortran-to-lisp::ieee1)
           :calls '(fortran-to-lisp::dlamc3))))

