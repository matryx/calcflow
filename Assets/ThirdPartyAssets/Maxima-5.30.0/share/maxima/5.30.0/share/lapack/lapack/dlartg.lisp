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


(let* ((zero 0.0) (one 1.0) (two 2.0))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (type (double-float 2.0 2.0) two)
           (ignorable zero one two))
  (let ((first$ nil) (safmn2 0.0) (safmin 0.0) (safmx2 0.0))
    (declare (type f2cl-lib:logical first$)
             (type (double-float) safmn2 safmin safmx2))
    (setq first$ f2cl-lib:%true%)
    (defun dlartg (f g cs sn r)
      (declare (type (double-float) r sn cs g f))
      (prog ((eps 0.0) (f1 0.0) (g1 0.0) (scale 0.0) (i 0) (count$ 0))
        (declare (type (double-float) eps f1 g1 scale)
                 (type (f2cl-lib:integer4) count$ i))
        (cond
          (first$
           (setf first$ f2cl-lib:%false%)
           (setf safmin (dlamch "S"))
           (setf eps (dlamch "E"))
           (setf safmn2
                   (expt (dlamch "B")
                         (f2cl-lib:int
                          (/
                           (/ (f2cl-lib:flog (/ safmin eps))
                              (f2cl-lib:flog (dlamch "B")))
                           two))))
           (setf safmx2 (/ one safmn2))))
        (cond
          ((= g zero)
           (setf cs one)
           (setf sn zero)
           (setf r f))
          ((= f zero)
           (setf cs zero)
           (setf sn one)
           (setf r g))
          (t
           (setf f1 f)
           (setf g1 g)
           (setf scale (max (abs f1) (abs g1)))
           (cond
             ((>= scale safmx2)
              (tagbody
                (setf count$ 0)
               label10
                (setf count$ (f2cl-lib:int-add count$ 1))
                (setf f1 (* f1 safmn2))
                (setf g1 (* g1 safmn2))
                (setf scale (max (abs f1) (abs g1)))
                (if (>= scale safmx2) (go label10))
                (setf r (f2cl-lib:fsqrt (+ (expt f1 2) (expt g1 2))))
                (setf cs (/ f1 r))
                (setf sn (/ g1 r))
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i count$) nil)
                  (tagbody (setf r (* r safmx2)) label20))))
             ((<= scale safmn2)
              (tagbody
                (setf count$ 0)
               label30
                (setf count$ (f2cl-lib:int-add count$ 1))
                (setf f1 (* f1 safmx2))
                (setf g1 (* g1 safmx2))
                (setf scale (max (abs f1) (abs g1)))
                (if (<= scale safmn2) (go label30))
                (setf r (f2cl-lib:fsqrt (+ (expt f1 2) (expt g1 2))))
                (setf cs (/ f1 r))
                (setf sn (/ g1 r))
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i count$) nil)
                  (tagbody (setf r (* r safmn2)) label40))))
             (t
              (setf r (f2cl-lib:fsqrt (+ (expt f1 2) (expt g1 2))))
              (setf cs (/ f1 r))
              (setf sn (/ g1 r))))
           (cond
             ((and (> (abs f) (abs g)) (< cs zero))
              (setf cs (- cs))
              (setf sn (- sn))
              (setf r (- r))))))
        (go end_label)
       end_label
        (return (values nil nil cs sn r))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlartg
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float) (double-float))
           :return-values '(nil nil fortran-to-lisp::cs fortran-to-lisp::sn
                            fortran-to-lisp::r)
           :calls '(fortran-to-lisp::dlamch))))

