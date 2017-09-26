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


(let ((lbeta 0)
      (lemax 0)
      (lemin 0)
      (leps 0.0)
      (lrmax 0.0)
      (lrmin 0.0)
      (lt$ 0)
      (first$ nil)
      (iwarn nil))
  (declare (type (f2cl-lib:integer4) lbeta lemax lemin lt$)
           (type (double-float) leps lrmax lrmin)
           (type f2cl-lib:logical first$ iwarn))
  (setq first$ f2cl-lib:%true%)
  (setq iwarn f2cl-lib:%false%)
  (defun dlamc2 (beta t$ rnd eps emin rmin emax rmax)
    (declare (type (double-float) rmax rmin eps)
             (type f2cl-lib:logical rnd)
             (type (f2cl-lib:integer4) emax emin t$ beta))
    (prog ((a 0.0) (b 0.0) (c 0.0) (half 0.0) (one 0.0) (rbase 0.0)
           (sixth$ 0.0) (small 0.0) (third$ 0.0) (two 0.0) (zero 0.0) (gnmin 0)
           (gpmin 0) (i 0) (ngnmin 0) (ngpmin 0) (ieee nil) (lieee1 nil)
           (lrnd nil))
      (declare (type f2cl-lib:logical lrnd lieee1 ieee)
               (type (f2cl-lib:integer4) ngpmin ngnmin i gpmin gnmin)
               (type (double-float) zero two third$ small sixth$ rbase one half
                                    c b a))
      (cond
        (first$
         (tagbody
           (setf first$ f2cl-lib:%false%)
           (setf zero (coerce (the f2cl-lib:integer4 0) 'double-float))
           (setf one (coerce (the f2cl-lib:integer4 1) 'double-float))
           (setf two (coerce (the f2cl-lib:integer4 2) 'double-float))
           (multiple-value-bind (var-0 var-1 var-2 var-3)
               (dlamc1 lbeta lt$ lrnd lieee1)
             (declare (ignore))
             (setf lbeta var-0)
             (setf lt$ var-1)
             (setf lrnd var-2)
             (setf lieee1 var-3))
           (setf b (coerce (the f2cl-lib:integer4 lbeta) 'double-float))
           (setf a (expt b (f2cl-lib:int-sub lt$)))
           (setf leps a)
           (setf b (/ two 3))
           (setf half (/ one 2))
           (setf sixth$ (dlamc3 b (- half)))
           (setf third$ (dlamc3 sixth$ sixth$))
           (setf b (dlamc3 third$ (- half)))
           (setf b (dlamc3 b sixth$))
           (setf b (abs b))
           (if (< b leps) (setf b leps))
           (setf leps (coerce (the f2cl-lib:integer4 1) 'double-float))
          label10
           (cond
             ((and (> leps b) (> b zero))
              (setf leps b)
              (setf c (dlamc3 (* half leps) (* (expt two 5) (expt leps 2))))
              (setf c (dlamc3 half (- c)))
              (setf b (dlamc3 half c))
              (setf c (dlamc3 half (- b)))
              (setf b (dlamc3 half c))
              (go label10)))
           (if (< a leps) (setf leps a))
           (setf rbase (/ one lbeta))
           (setf small one)
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i 3) nil)
             (tagbody (setf small (dlamc3 (* small rbase) zero)) label20))
           (setf a (dlamc3 one small))
           (multiple-value-bind (var-0 var-1 var-2)
               (dlamc4 ngpmin one lbeta)
             (declare (ignore var-1 var-2))
             (setf ngpmin var-0))
           (multiple-value-bind (var-0 var-1 var-2)
               (dlamc4 ngnmin (- one) lbeta)
             (declare (ignore var-1 var-2))
             (setf ngnmin var-0))
           (multiple-value-bind (var-0 var-1 var-2)
               (dlamc4 gpmin a lbeta)
             (declare (ignore var-1 var-2))
             (setf gpmin var-0))
           (multiple-value-bind (var-0 var-1 var-2)
               (dlamc4 gnmin (- a) lbeta)
             (declare (ignore var-1 var-2))
             (setf gnmin var-0))
           (setf ieee f2cl-lib:%false%)
           (cond
             ((and (= ngpmin ngnmin) (= gpmin gnmin))
              (cond
                ((= ngpmin gpmin)
                 (setf lemin ngpmin))
                ((= (f2cl-lib:int-add gpmin (f2cl-lib:int-sub ngpmin)) 3)
                 (setf lemin (f2cl-lib:int-add (f2cl-lib:int-sub ngpmin 1) lt$))
                 (setf ieee f2cl-lib:%true%))
                (t
                 (setf lemin
                         (min (the f2cl-lib:integer4 ngpmin)
                              (the f2cl-lib:integer4 gpmin)))
                 (setf iwarn f2cl-lib:%true%))))
             ((and (= ngpmin gpmin) (= ngnmin gnmin))
              (cond
                ((= (abs (f2cl-lib:int-add ngpmin (f2cl-lib:int-sub ngnmin))) 1)
                 (setf lemin
                         (max (the f2cl-lib:integer4 ngpmin)
                              (the f2cl-lib:integer4 ngnmin))))
                (t
                 (setf lemin
                         (min (the f2cl-lib:integer4 ngpmin)
                              (the f2cl-lib:integer4 ngnmin)))
                 (setf iwarn f2cl-lib:%true%))))
             ((and
               (= (abs (f2cl-lib:int-add ngpmin (f2cl-lib:int-sub ngnmin))) 1)
               (= gpmin gnmin))
              (cond
                ((=
                  (f2cl-lib:int-add gpmin
                                    (f2cl-lib:int-sub
                                     (min (the f2cl-lib:integer4 ngpmin)
                                          (the f2cl-lib:integer4 ngnmin))))
                  3)
                 (setf lemin
                         (f2cl-lib:int-add
                          (f2cl-lib:int-sub
                           (max (the f2cl-lib:integer4 ngpmin)
                                (the f2cl-lib:integer4 ngnmin))
                           1)
                          lt$)))
                (t
                 (setf lemin
                         (min (the f2cl-lib:integer4 ngpmin)
                              (the f2cl-lib:integer4 ngnmin)))
                 (setf iwarn f2cl-lib:%true%))))
             (t
              (setf lemin
                      (min (the f2cl-lib:integer4 ngpmin)
                           (the f2cl-lib:integer4 ngnmin)
                           (the f2cl-lib:integer4 gpmin)
                           (the f2cl-lib:integer4 gnmin)))
              (setf iwarn f2cl-lib:%true%)))
           (cond
             (iwarn
              (setf first$ f2cl-lib:%true%)
              (f2cl-lib:fformat 6
                                ("~%" "~%"
                                 " WARNING. The value EMIN may be incorrect:-"
                                 "  EMIN = " 1 (("~8D")) "~%"
                                 " If, after inspection, the value EMIN looks"
                                 " acceptable please comment out " "~%"
                                 " the IF block as marked within the code of routine"
                                 " DLAMC2," "~%"
                                 " otherwise supply EMIN explicitly." "~%"
                                 "~%")
                                lemin)))
           (setf ieee (or ieee lieee1))
           (setf lrmin (coerce (the f2cl-lib:integer4 1) 'double-float))
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i (f2cl-lib:int-add 1 (f2cl-lib:int-sub lemin)))
                          nil)
             (tagbody (setf lrmin (dlamc3 (* lrmin rbase) zero)) label30))
           (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
               (dlamc5 lbeta lt$ lemin ieee lemax lrmax)
             (declare (ignore var-0 var-1 var-2 var-3))
             (setf lemax var-4)
             (setf lrmax var-5)))))
      (setf beta lbeta)
      (setf t$ lt$)
      (setf rnd lrnd)
      (setf eps leps)
      (setf emin lemin)
      (setf rmin lrmin)
      (setf emax lemax)
      (setf rmax lrmax)
      (go end_label)
     end_label
      (return (values beta t$ rnd eps emin rmin emax rmax)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlamc2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        fortran-to-lisp::logical (double-float)
                        (fortran-to-lisp::integer4) (double-float)
                        (fortran-to-lisp::integer4) (double-float))
           :return-values '(fortran-to-lisp::beta fortran-to-lisp::t$
                            fortran-to-lisp::rnd fortran-to-lisp::eps
                            fortran-to-lisp::emin fortran-to-lisp::rmin
                            fortran-to-lisp::emax fortran-to-lisp::rmax)
           :calls '(fortran-to-lisp::dlamc5 fortran-to-lisp::dlamc4
                    fortran-to-lisp::dlamc3 fortran-to-lisp::dlamc1))))

