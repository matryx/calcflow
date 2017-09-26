;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.221 2010/05/26 19:25:52 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.204 2010/02/23 05:21:30 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.114 2010/05/17 01:42:14 rtoy Exp $")

;;; Using Lisp CMU Common Lisp CVS Head 2010-05-25 18:21:07 (20A Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :colnew)


(defun horder (i uhigh hi dmz ncomp k)
  (declare (type double-float hi)
           (type (array double-float (*)) dmz uhigh)
           (type (f2cl-lib:integer4) k ncomp i))
  (let ((colloc-coef
         (make-array 49
                     :element-type 'double-float
                     :displaced-to (colloc-part-0 *colloc-common-block*)
                     :displaced-index-offset 7)))
    (symbol-macrolet ((coef colloc-coef))
      (f2cl-lib:with-multi-array-data
          ((uhigh double-float uhigh-%data% uhigh-%offset%)
           (dmz double-float dmz-%data% dmz-%offset%))
        (prog ((fact 0.0) (j 0) (idmz 0) (kin 0) (id 0) (dn 0.0))
          (declare (type (f2cl-lib:integer4) id kin idmz j)
                   (type double-float dn fact))
          (setf dn (/ 1.0 (expt hi (f2cl-lib:int-sub k 1))))
          (f2cl-lib:fdo (id 1 (f2cl-lib:int-add id 1))
                        ((> id ncomp) nil)
            (tagbody
              (setf (f2cl-lib:fref uhigh-%data% (id) ((1 1)) uhigh-%offset%)
                      0.0)
             label10))
          (setf kin 1)
          (setf idmz
                  (f2cl-lib:int-add
                   (f2cl-lib:int-mul (f2cl-lib:int-sub i 1) k ncomp)
                   1))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j k) nil)
            (tagbody
              (setf fact (* dn (f2cl-lib:fref coef (kin) ((1 49)))))
              (f2cl-lib:fdo (id 1 (f2cl-lib:int-add id 1))
                            ((> id ncomp) nil)
                (tagbody
                  (setf (f2cl-lib:fref uhigh-%data%
                                       (id)
                                       ((1 1))
                                       uhigh-%offset%)
                          (+
                           (f2cl-lib:fref uhigh-%data%
                                          (id)
                                          ((1 1))
                                          uhigh-%offset%)
                           (* fact
                              (f2cl-lib:fref dmz-%data%
                                             (idmz)
                                             ((1 1))
                                             dmz-%offset%))))
                  (setf idmz (f2cl-lib:int-add idmz 1))
                 label20))
              (setf kin (f2cl-lib:int-add kin k))
             label30))
          (go end_label)
         end_label
          (return (values nil nil nil nil nil nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::horder
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (1))
                        double-float (array double-float (1))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil)
           :calls 'nil)))

