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


(defun dqmomo (alfa beta ri rj rg rh integr)
  (declare (type (f2cl-lib:integer4) integr)
           (type (array double-float (*)) rh rg rj ri)
           (type (double-float) beta alfa))
  (f2cl-lib:with-multi-array-data
      ((ri double-float ri-%data% ri-%offset%)
       (rj double-float rj-%data% rj-%offset%)
       (rg double-float rg-%data% rg-%offset%)
       (rh double-float rh-%data% rh-%offset%))
    (prog ((i 0) (im1 0) (alfp1 0.0) (alfp2 0.0) (an 0.0) (anm1 0.0)
           (betp1 0.0) (betp2 0.0) (ralf 0.0) (rbet 0.0))
      (declare (type (double-float) rbet ralf betp2 betp1 anm1 an alfp2 alfp1)
               (type (f2cl-lib:integer4) im1 i))
      (setf alfp1 (+ alfa 1.0))
      (setf betp1 (+ beta 1.0))
      (setf alfp2 (+ alfa 2.0))
      (setf betp2 (+ beta 2.0))
      (setf ralf (expt 2.0 alfp1))
      (setf rbet (expt 2.0 betp1))
      (setf (f2cl-lib:fref ri-%data% (1) ((1 25)) ri-%offset%) (/ ralf alfp1))
      (setf (f2cl-lib:fref rj-%data% (1) ((1 25)) rj-%offset%) (/ rbet betp1))
      (setf (f2cl-lib:fref ri-%data% (2) ((1 25)) ri-%offset%)
              (/ (* (f2cl-lib:fref ri-%data% (1) ((1 25)) ri-%offset%) alfa)
                 alfp2))
      (setf (f2cl-lib:fref rj-%data% (2) ((1 25)) rj-%offset%)
              (/ (* (f2cl-lib:fref rj-%data% (1) ((1 25)) rj-%offset%) beta)
                 betp2))
      (setf an 2.0)
      (setf anm1 1.0)
      (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 1))
                    ((> i 25) nil)
        (tagbody
          (setf (f2cl-lib:fref ri-%data% (i) ((1 25)) ri-%offset%)
                  (/
                   (-
                    (+ ralf
                       (* an
                          (- an alfp2)
                          (f2cl-lib:fref ri-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 25))
                                         ri-%offset%))))
                   (* anm1 (+ an alfp1))))
          (setf (f2cl-lib:fref rj-%data% (i) ((1 25)) rj-%offset%)
                  (/
                   (-
                    (+ rbet
                       (* an
                          (- an betp2)
                          (f2cl-lib:fref rj-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 25))
                                         rj-%offset%))))
                   (* anm1 (+ an betp1))))
          (setf anm1 an)
          (setf an (+ an 1.0))
         label20))
      (if (= integr 1) (go label70))
      (if (= integr 3) (go label40))
      (setf (f2cl-lib:fref rg-%data% (1) ((1 25)) rg-%offset%)
              (/ (- (f2cl-lib:fref ri-%data% (1) ((1 25)) ri-%offset%)) alfp1))
      (setf (f2cl-lib:fref rg-%data% (2) ((1 25)) rg-%offset%)
              (- (/ (- (+ ralf ralf)) (* alfp2 alfp2))
                 (f2cl-lib:fref rg-%data% (1) ((1 25)) rg-%offset%)))
      (setf an 2.0)
      (setf anm1 1.0)
      (setf im1 2)
      (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 1))
                    ((> i 25) nil)
        (tagbody
          (setf (f2cl-lib:fref rg-%data% (i) ((1 25)) rg-%offset%)
                  (/
                   (-
                    (+
                     (* an
                        (- an alfp2)
                        (f2cl-lib:fref rg-%data% (im1) ((1 25)) rg-%offset%))
                     (* -1
                        an
                        (f2cl-lib:fref ri-%data% (im1) ((1 25)) ri-%offset%))
                     (* anm1
                        (f2cl-lib:fref ri-%data% (i) ((1 25)) ri-%offset%))))
                   (* anm1 (+ an alfp1))))
          (setf anm1 an)
          (setf an (+ an 1.0))
          (setf im1 i)
         label30))
      (if (= integr 2) (go label70))
     label40
      (setf (f2cl-lib:fref rh-%data% (1) ((1 25)) rh-%offset%)
              (/ (- (f2cl-lib:fref rj-%data% (1) ((1 25)) rj-%offset%)) betp1))
      (setf (f2cl-lib:fref rh-%data% (2) ((1 25)) rh-%offset%)
              (- (/ (- (+ rbet rbet)) (* betp2 betp2))
                 (f2cl-lib:fref rh-%data% (1) ((1 25)) rh-%offset%)))
      (setf an 2.0)
      (setf anm1 1.0)
      (setf im1 2)
      (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 1))
                    ((> i 25) nil)
        (tagbody
          (setf (f2cl-lib:fref rh-%data% (i) ((1 25)) rh-%offset%)
                  (/
                   (-
                    (+
                     (* an
                        (- an betp2)
                        (f2cl-lib:fref rh-%data% (im1) ((1 25)) rh-%offset%))
                     (* -1
                        an
                        (f2cl-lib:fref rj-%data% (im1) ((1 25)) rj-%offset%))
                     (* anm1
                        (f2cl-lib:fref rj-%data% (i) ((1 25)) rj-%offset%))))
                   (* anm1 (+ an betp1))))
          (setf anm1 an)
          (setf an (+ an 1.0))
          (setf im1 i)
         label50))
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 2))
                    ((> i 25) nil)
        (tagbody
          (setf (f2cl-lib:fref rh-%data% (i) ((1 25)) rh-%offset%)
                  (- (f2cl-lib:fref rh-%data% (i) ((1 25)) rh-%offset%)))
         label60))
     label70
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 2))
                    ((> i 25) nil)
        (tagbody
          (setf (f2cl-lib:fref rj-%data% (i) ((1 25)) rj-%offset%)
                  (- (f2cl-lib:fref rj-%data% (i) ((1 25)) rj-%offset%)))
         label80))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dqmomo
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil)
           :calls 'nil)))

