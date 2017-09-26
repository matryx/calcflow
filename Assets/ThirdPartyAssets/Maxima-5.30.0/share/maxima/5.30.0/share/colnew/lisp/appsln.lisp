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


(defun appsln (x z fspace ispace)
  (declare (type (array f2cl-lib:integer4 (*)) ispace)
           (type (array double-float (*)) fspace z)
           (type double-float x))
  (f2cl-lib:with-multi-array-data
      ((z double-float z-%data% z-%offset%)
       (fspace double-float fspace-%data% fspace-%offset%)
       (ispace f2cl-lib:integer4 ispace-%data% ispace-%offset%))
    (prog ((a (make-array 28 :element-type 'double-float))
           (dummy (make-array 1 :element-type 'double-float)) (i 0) (is4 0)
           (is5 0) (is6 0))
      (declare (type (f2cl-lib:integer4) is6 is5 is4 i)
               (type (array double-float (1)) dummy)
               (type (array double-float (28)) a))
      (setf is6 (f2cl-lib:fref ispace-%data% (6) ((1 1)) ispace-%offset%))
      (setf is5
              (f2cl-lib:int-add
               (f2cl-lib:fref ispace-%data% (1) ((1 1)) ispace-%offset%)
               2))
      (setf is4
              (f2cl-lib:int-add is5
                                (f2cl-lib:int-mul
                                 (f2cl-lib:fref ispace-%data%
                                                (4)
                                                ((1 1))
                                                ispace-%offset%)
                                 (f2cl-lib:int-add
                                  (f2cl-lib:fref ispace-%data%
                                                 (1)
                                                 ((1 1))
                                                 ispace-%offset%)
                                  1))))
      (setf i 1)
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12 var-13 var-14 var-15 var-16)
          (approx i x z a
           (f2cl-lib:array-slice fspace double-float (is6) ((1 1)))
           (f2cl-lib:array-slice fspace double-float (1) ((1 1)))
           (f2cl-lib:fref ispace-%data% (1) ((1 1)) ispace-%offset%)
           (f2cl-lib:array-slice fspace double-float (is5) ((1 1)))
           (f2cl-lib:array-slice fspace double-float (is4) ((1 1)))
           (f2cl-lib:fref ispace-%data% (2) ((1 1)) ispace-%offset%)
           (f2cl-lib:fref ispace-%data% (3) ((1 1)) ispace-%offset%)
           (f2cl-lib:fref ispace-%data% (5) ((1 1)) ispace-%offset%)
           (f2cl-lib:array-slice ispace f2cl-lib:integer4 (8) ((1 1)))
           (f2cl-lib:fref ispace-%data% (4) ((1 1)) ispace-%offset%) 2 dummy 0)
        (declare (ignore var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
                         var-11 var-12 var-13 var-14 var-15 var-16))
        (setf i var-0)
        (setf x var-1))
      (go end_label)
     end_label
      (return (values x nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::appsln
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(double-float (array double-float (1))
                        (array double-float (1))
                        (array fortran-to-lisp::integer4 (1)))
           :return-values '(fortran-to-lisp::x nil nil nil)
           :calls '(fortran-to-lisp::approx))))

