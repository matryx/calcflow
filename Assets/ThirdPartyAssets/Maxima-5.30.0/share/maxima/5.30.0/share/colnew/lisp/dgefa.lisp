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


(defun dgefa (a lda n ipvt info)
  (declare (type (array f2cl-lib:integer4 (*)) ipvt)
           (type (f2cl-lib:integer4) info n lda)
           (type (array double-float (*)) a))
  (f2cl-lib:with-multi-array-data
      ((a double-float a-%data% a-%offset%)
       (ipvt f2cl-lib:integer4 ipvt-%data% ipvt-%offset%))
    (prog ((j 0) (k 0) (kp1 0) (l 0) (nm1 0) (t$ 0.0))
      (declare (type (double-float) t$)
               (type (f2cl-lib:integer4) nm1 l kp1 k j))
      (setf info 0)
      (setf nm1 (f2cl-lib:int-sub n 1))
      (if (< nm1 1) (go label70))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k nm1) nil)
        (tagbody
          (setf kp1 (f2cl-lib:int-add k 1))
          (setf l
                  (f2cl-lib:int-sub
                   (f2cl-lib:int-add
                    (idamax (f2cl-lib:int-add (f2cl-lib:int-sub n k) 1)
                     (f2cl-lib:array-slice a
                                           double-float
                                           (k k)
                                           ((1 lda) (1 1)))
                     1)
                    k)
                   1))
          (setf (f2cl-lib:fref ipvt-%data% (k) ((1 1)) ipvt-%offset%) l)
          (if (= (f2cl-lib:fref a-%data% (l k) ((1 lda) (1 1)) a-%offset%) 0.0)
              (go label40))
          (if (= l k) (go label10))
          (setf t$ (f2cl-lib:fref a-%data% (l k) ((1 lda) (1 1)) a-%offset%))
          (setf (f2cl-lib:fref a-%data% (l k) ((1 lda) (1 1)) a-%offset%)
                  (f2cl-lib:fref a-%data% (k k) ((1 lda) (1 1)) a-%offset%))
          (setf (f2cl-lib:fref a-%data% (k k) ((1 lda) (1 1)) a-%offset%) t$)
         label10
          (setf t$
                  (/ -1.0
                     (f2cl-lib:fref a-%data%
                                    (k k)
                                    ((1 lda) (1 1))
                                    a-%offset%)))
          (dscal (f2cl-lib:int-sub n k) t$
           (f2cl-lib:array-slice a double-float ((+ k 1) k) ((1 lda) (1 1))) 1)
          (f2cl-lib:fdo (j kp1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (setf t$
                      (f2cl-lib:fref a-%data%
                                     (l j)
                                     ((1 lda) (1 1))
                                     a-%offset%))
              (if (= l k) (go label20))
              (setf (f2cl-lib:fref a-%data% (l j) ((1 lda) (1 1)) a-%offset%)
                      (f2cl-lib:fref a-%data%
                                     (k j)
                                     ((1 lda) (1 1))
                                     a-%offset%))
              (setf (f2cl-lib:fref a-%data% (k j) ((1 lda) (1 1)) a-%offset%)
                      t$)
             label20
              (daxpy (f2cl-lib:int-sub n k) t$
               (f2cl-lib:array-slice a
                                     double-float
                                     ((+ k 1) k)
                                     ((1 lda) (1 1)))
               1
               (f2cl-lib:array-slice a
                                     double-float
                                     ((+ k 1) j)
                                     ((1 lda) (1 1)))
               1)
             label30))
          (go label50)
         label40
          (setf info k)
         label50
         label60))
     label70
      (setf (f2cl-lib:fref ipvt-%data% (n) ((1 1)) ipvt-%offset%) n)
      (if (= (f2cl-lib:fref a-%data% (n n) ((1 lda) (1 1)) a-%offset%) 0.0)
          (setf info n))
      (go end_label)
     end_label
      (return (values nil nil nil nil info)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgefa fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (1))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::daxpy fortran-to-lisp::dscal
                    fortran-to-lisp::idamax))))

