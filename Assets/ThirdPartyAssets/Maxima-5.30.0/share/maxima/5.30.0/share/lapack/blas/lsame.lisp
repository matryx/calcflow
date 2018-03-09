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

(in-package :blas)


(defun lsame (ca cb)
  (declare (type (simple-string *) cb ca))
  (f2cl-lib:with-multi-array-data
      ((ca character ca-%data% ca-%offset%)
       (cb character cb-%data% cb-%offset%))
    (prog ((inta 0) (intb 0) (zcode 0) (lsame nil))
      (declare (type f2cl-lib:logical lsame)
               (type (f2cl-lib:integer4) zcode intb inta))
      (setf lsame (coerce (f2cl-lib:fstring-= ca cb) 'f2cl-lib:logical))
      (if lsame (go end_label))
      (setf zcode (f2cl-lib:ichar "Z"))
      (setf inta (f2cl-lib:ichar ca))
      (setf intb (f2cl-lib:ichar cb))
      (cond
        ((or (= zcode 90) (= zcode 122))
         (if (and (>= inta 97) (<= inta 122))
             (setf inta (f2cl-lib:int-sub inta 32)))
         (if (and (>= intb 97) (<= intb 122))
             (setf intb (f2cl-lib:int-sub intb 32))))
        ((or (= zcode 233) (= zcode 169))
         (if
          (or (and (>= inta 129) (<= inta 137))
              (and (>= inta 145) (<= inta 153))
              (and (>= inta 162) (<= inta 169)))
          (setf inta (f2cl-lib:int-add inta 64)))
         (if
          (or (and (>= intb 129) (<= intb 137))
              (and (>= intb 145) (<= intb 153))
              (and (>= intb 162) (<= intb 169)))
          (setf intb (f2cl-lib:int-add intb 64))))
        ((or (= zcode 218) (= zcode 250))
         (if (and (>= inta 225) (<= inta 250))
             (setf inta (f2cl-lib:int-sub inta 32)))
         (if (and (>= intb 225) (<= intb 250))
             (setf intb (f2cl-lib:int-sub intb 32)))))
      (setf lsame (coerce (= inta intb) 'f2cl-lib:logical))
     end_label
      (return (values lsame nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::lsame fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string))
           :return-values '(nil nil)
           :calls 'nil)))

