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


(defun dladiv (a b c d p q)
  (declare (type (double-float) q p d c b a))
  (prog ((e 0.0) (f 0.0))
    (declare (type (double-float) f e))
    (cond
      ((< (abs d) (abs c))
       (setf e (/ d c))
       (setf f (+ c (* d e)))
       (setf p (/ (+ a (* b e)) f))
       (setf q (/ (- b (* a e)) f)))
      (t
       (setf e (/ c d))
       (setf f (+ d (* c e)))
       (setf p (/ (+ b (* a e)) f))
       (setf q (/ (- (* b e) a) f))))
    (go end_label)
   end_label
    (return (values nil nil nil nil p q))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dladiv
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float))
           :return-values '(nil nil nil nil fortran-to-lisp::p
                            fortran-to-lisp::q)
           :calls 'nil)))

