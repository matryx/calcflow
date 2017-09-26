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


(let* ((two 2.0) (one 1.0) (zero 0.0) (czero (f2cl-lib:cmplx 0.0 0.0)))
  (declare (type (double-float 2.0 2.0) two)
           (type (double-float 1.0 1.0) one)
           (type (double-float 0.0 0.0) zero)
           (type (f2cl-lib:complex16) czero)
           (ignorable two one zero czero))
  (defun zlartg (f g cs sn r)
    (declare (type (double-float) cs) (type (f2cl-lib:complex16) r sn g f))
    (labels ((abs1 (ff)
               (max (abs (f2cl-lib:dble ff)) (abs (f2cl-lib:dimag ff))))
             (abssq (ff)
               (+ (expt (f2cl-lib:dble ff) 2) (expt (f2cl-lib:dimag ff) 2))))
      (declare (ftype (function (f2cl-lib:complex16)
                       (values double-float &rest t))
                      abs1))
      (declare (ftype (function (f2cl-lib:complex16)
                       (values double-float &rest t))
                      abssq))
      (prog ((ff #C(0.0 0.0)) (fs #C(0.0 0.0)) (gs #C(0.0 0.0)) (d 0.0)
             (di 0.0) (dr 0.0) (eps 0.0) (f2 0.0) (f2s 0.0) (g2 0.0) (g2s 0.0)
             (safmin 0.0) (safmn2 0.0) (safmx2 0.0) (scale 0.0) (i 0)
             (count$ 0))
        (declare (type (f2cl-lib:complex16) ff fs gs)
                 (type (double-float) d di dr eps f2 f2s g2 g2s safmin safmn2
                                      safmx2 scale)
                 (type (f2cl-lib:integer4) count$ i))
        (setf safmin (dlamch "S"))
        (setf eps (dlamch "E"))
        (setf safmn2
                (expt (dlamch "B")
                      (f2cl-lib:int
                       (/
                        (/ (f2cl-lib:flog (/ safmin eps))
                           (f2cl-lib:flog (dlamch "B")))
                        two))))
        (setf safmx2 (/ one safmn2))
        (setf scale (max (abs1 f) (abs1 g)))
        (setf fs f)
        (setf gs g)
        (setf count$ 0)
        (cond
          ((>= scale safmx2)
           (tagbody
            label10
             (setf count$ (f2cl-lib:int-add count$ 1))
             (setf fs (* fs safmn2))
             (setf gs (* gs safmn2))
             (setf scale (* scale safmn2))
             (if (>= scale safmx2) (go label10))))
          ((<= scale safmn2)
           (tagbody
             (cond
               ((= g czero)
                (setf cs one)
                (setf sn czero)
                (setf r f)
                (go end_label)))
            label20
             (setf count$ (f2cl-lib:int-sub count$ 1))
             (setf fs (* fs safmx2))
             (setf gs (* gs safmx2))
             (setf scale (* scale safmx2))
             (if (<= scale safmn2) (go label20)))))
        (setf f2 (abssq fs))
        (setf g2 (abssq gs))
        (cond
          ((<= f2 (* (max g2 one) safmin))
           (cond
             ((= f czero)
              (setf cs zero)
              (setf r
                      (coerce (dlapy2 (f2cl-lib:dble g) (f2cl-lib:dimag g))
                              'f2cl-lib:complex16))
              (setf d (dlapy2 (f2cl-lib:dble gs) (f2cl-lib:dimag gs)))
              (setf sn
                      (f2cl-lib:dcmplx (/ (f2cl-lib:dble gs) d)
                                       (/ (- (f2cl-lib:dimag gs)) d)))
              (go end_label)))
           (setf f2s (dlapy2 (f2cl-lib:dble fs) (f2cl-lib:dimag fs)))
           (setf g2s (f2cl-lib:fsqrt g2))
           (setf cs (/ f2s g2s))
           (cond
             ((> (abs1 f) one)
              (setf d (dlapy2 (f2cl-lib:dble f) (f2cl-lib:dimag f)))
              (setf ff
                      (f2cl-lib:dcmplx (/ (f2cl-lib:dble f) d)
                                       (/ (f2cl-lib:dimag f) d))))
             (t
              (setf dr (* safmx2 (f2cl-lib:dble f)))
              (setf di (* safmx2 (f2cl-lib:dimag f)))
              (setf d (dlapy2 dr di))
              (setf ff (f2cl-lib:dcmplx (/ dr d) (/ di d)))))
           (setf sn
                   (* ff
                      (f2cl-lib:dcmplx (/ (f2cl-lib:dble gs) g2s)
                                       (/ (- (f2cl-lib:dimag gs)) g2s))))
           (setf r (+ (* cs f) (* sn g))))
          (t
           (setf f2s (f2cl-lib:fsqrt (+ one (/ g2 f2))))
           (setf r
                   (f2cl-lib:dcmplx (* f2s (f2cl-lib:dble fs))
                                    (* f2s (f2cl-lib:dimag fs))))
           (setf cs (/ one f2s))
           (setf d (+ f2 g2))
           (setf sn
                   (f2cl-lib:dcmplx (/ (f2cl-lib:dble r) d)
                                    (/ (f2cl-lib:dimag r) d)))
           (setf sn (* sn (f2cl-lib:dconjg gs)))
           (cond
             ((/= count$ 0)
              (cond
                ((> count$ 0)
                 (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                               ((> i count$) nil)
                   (tagbody (setf r (* r safmx2)) label30)))
                (t
                 (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                               ((> i (f2cl-lib:int-sub count$)) nil)
                   (tagbody (setf r (* r safmn2)) label40))))))))
        (go end_label)
       end_label
        (return (values nil nil cs sn r))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zlartg
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::complex16)
                        (fortran-to-lisp::complex16) (double-float)
                        (fortran-to-lisp::complex16)
                        (fortran-to-lisp::complex16))
           :return-values '(nil nil fortran-to-lisp::cs fortran-to-lisp::sn
                            fortran-to-lisp::r)
           :calls '(fortran-to-lisp::dlapy2 fortran-to-lisp::dlamch))))

