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


(defun dmzsol (kd mstar n v z dmz)
  (declare (type (array double-float (*)) z)
           (type (array double-float (*)) dmz v)
           (type (f2cl-lib:integer4) n mstar kd))
  (f2cl-lib:with-multi-array-data
      ((v double-float v-%data% v-%offset%)
       (dmz double-float dmz-%data% dmz-%offset%)
       (z double-float z-%data% z-%offset%))
    (prog ((l 0) (fact 0.0) (j 0) (i 0) (jz 0))
      (declare (type double-float fact) (type (f2cl-lib:integer4) jz i j l))
      (setf jz 1)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j mstar) nil)
            (tagbody
              (setf fact (f2cl-lib:fref z-%data% (jz) ((1 1)) z-%offset%))
              (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                            ((> l kd) nil)
                (tagbody
                  (setf (f2cl-lib:fref dmz-%data%
                                       (l i)
                                       ((1 kd) (1 1))
                                       dmz-%offset%)
                          (+
                           (f2cl-lib:fref dmz-%data%
                                          (l i)
                                          ((1 kd) (1 1))
                                          dmz-%offset%)
                           (* fact
                              (f2cl-lib:fref v-%data%
                                             (l jz)
                                             ((1 kd) (1 1))
                                             v-%offset%))))
                 label10))
              (setf jz (f2cl-lib:int-add jz 1))
             label20))
         label30))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dmzsol
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (1)) (array double-float (*)))
           :return-values '(nil nil nil nil nil nil)
           :calls 'nil)))

