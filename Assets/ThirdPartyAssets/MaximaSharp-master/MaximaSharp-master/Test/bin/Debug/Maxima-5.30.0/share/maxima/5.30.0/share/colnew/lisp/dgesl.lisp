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


(defun dgesl (a lda n ipvt b job)
  (declare (type (array double-float (*)) b)
           (type (array f2cl-lib:integer4 (*)) ipvt)
           (type (f2cl-lib:integer4) job n lda)
           (type (array double-float (*)) a))
  (f2cl-lib:with-multi-array-data
      ((a double-float a-%data% a-%offset%)
       (ipvt f2cl-lib:integer4 ipvt-%data% ipvt-%offset%)
       (b double-float b-%data% b-%offset%))
    (prog ((k 0) (kb 0) (l 0) (nm1 0) (t$ 0.0))
      (declare (type (double-float) t$) (type (f2cl-lib:integer4) nm1 l kb k))
      (setf nm1 (f2cl-lib:int-sub n 1))
      (if (/= job 0) (go label50))
      (if (< nm1 1) (go label30))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k nm1) nil)
        (tagbody
          (setf l (f2cl-lib:fref ipvt-%data% (k) ((1 1)) ipvt-%offset%))
          (setf t$ (f2cl-lib:fref b-%data% (l) ((1 1)) b-%offset%))
          (if (= l k) (go label10))
          (setf (f2cl-lib:fref b-%data% (l) ((1 1)) b-%offset%)
                  (f2cl-lib:fref b-%data% (k) ((1 1)) b-%offset%))
          (setf (f2cl-lib:fref b-%data% (k) ((1 1)) b-%offset%) t$)
         label10
          (daxpy (f2cl-lib:int-sub n k) t$
           (f2cl-lib:array-slice a double-float ((+ k 1) k) ((1 lda) (1 1))) 1
           (f2cl-lib:array-slice b double-float ((+ k 1)) ((1 1))) 1)
         label20))
     label30
      (f2cl-lib:fdo (kb 1 (f2cl-lib:int-add kb 1))
                    ((> kb n) nil)
        (tagbody
          (setf k (f2cl-lib:int-sub (f2cl-lib:int-add n 1) kb))
          (setf (f2cl-lib:fref b-%data% (k) ((1 1)) b-%offset%)
                  (/ (f2cl-lib:fref b-%data% (k) ((1 1)) b-%offset%)
                     (f2cl-lib:fref a-%data%
                                    (k k)
                                    ((1 lda) (1 1))
                                    a-%offset%)))
          (setf t$ (- (f2cl-lib:fref b-%data% (k) ((1 1)) b-%offset%)))
          (daxpy (f2cl-lib:int-sub k 1) t$
           (f2cl-lib:array-slice a double-float (1 k) ((1 lda) (1 1))) 1
           (f2cl-lib:array-slice b double-float (1) ((1 1))) 1)
         label40))
      (go label100)
     label50
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k n) nil)
        (tagbody
          (setf t$
                  (ddot (f2cl-lib:int-sub k 1)
                   (f2cl-lib:array-slice a double-float (1 k) ((1 lda) (1 1)))
                   1 (f2cl-lib:array-slice b double-float (1) ((1 1))) 1))
          (setf (f2cl-lib:fref b-%data% (k) ((1 1)) b-%offset%)
                  (/ (- (f2cl-lib:fref b-%data% (k) ((1 1)) b-%offset%) t$)
                     (f2cl-lib:fref a-%data%
                                    (k k)
                                    ((1 lda) (1 1))
                                    a-%offset%)))
         label60))
      (if (< nm1 1) (go label90))
      (f2cl-lib:fdo (kb 1 (f2cl-lib:int-add kb 1))
                    ((> kb nm1) nil)
        (tagbody
          (setf k (f2cl-lib:int-sub n kb))
          (setf (f2cl-lib:fref b-%data% (k) ((1 1)) b-%offset%)
                  (+ (f2cl-lib:fref b-%data% (k) ((1 1)) b-%offset%)
                     (ddot (f2cl-lib:int-sub n k)
                      (f2cl-lib:array-slice a
                                            double-float
                                            ((+ k 1) k)
                                            ((1 lda) (1 1)))
                      1 (f2cl-lib:array-slice b double-float ((+ k 1)) ((1 1)))
                      1)))
          (setf l (f2cl-lib:fref ipvt-%data% (k) ((1 1)) ipvt-%offset%))
          (if (= l k) (go label70))
          (setf t$ (f2cl-lib:fref b-%data% (l) ((1 1)) b-%offset%))
          (setf (f2cl-lib:fref b-%data% (l) ((1 1)) b-%offset%)
                  (f2cl-lib:fref b-%data% (k) ((1 1)) b-%offset%))
          (setf (f2cl-lib:fref b-%data% (k) ((1 1)) b-%offset%) t$)
         label70
         label80))
     label90
     label100
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgesl fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (1))
                        (array double-float (1)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::ddot fortran-to-lisp::daxpy))))

