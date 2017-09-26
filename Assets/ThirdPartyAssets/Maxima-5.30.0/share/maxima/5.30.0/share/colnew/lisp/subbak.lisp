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


(defun subbak (w nrow ncol last$ x)
  (declare (type (f2cl-lib:integer4) last$ ncol nrow)
           (type (array double-float (*)) x w))
  (f2cl-lib:with-multi-array-data
      ((w double-float w-%data% w-%offset%)
       (x double-float x-%data% x-%offset%))
    (prog ((t$ 0.0) (i 0) (j 0) (k 0) (km1 0) (lm1 0) (lp1 0) (kb 0))
      (declare (type (f2cl-lib:integer4) kb lp1 lm1 km1 k j i)
               (type (double-float) t$))
      (setf lp1 (f2cl-lib:int-add last$ 1))
      (if (> lp1 ncol) (go label30))
      (f2cl-lib:fdo (j lp1 (f2cl-lib:int-add j 1))
                    ((> j ncol) nil)
        (tagbody
          (setf t$ (- (f2cl-lib:fref x-%data% (j) ((1 ncol)) x-%offset%)))
          (if (= t$ 0.0) (go label20))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i last$) nil)
            (tagbody
             label10
              (setf (f2cl-lib:fref x-%data% (i) ((1 ncol)) x-%offset%)
                      (+ (f2cl-lib:fref x-%data% (i) ((1 ncol)) x-%offset%)
                         (*
                          (f2cl-lib:fref w-%data%
                                         (i j)
                                         ((1 nrow) (1 ncol))
                                         w-%offset%)
                          t$)))))
         label20))
     label30
      (if (= last$ 1) (go label60))
      (setf lm1 (f2cl-lib:int-sub last$ 1))
      (f2cl-lib:fdo (kb 1 (f2cl-lib:int-add kb 1))
                    ((> kb lm1) nil)
        (tagbody
          (setf km1 (f2cl-lib:int-sub last$ kb))
          (setf k (f2cl-lib:int-add km1 1))
          (setf (f2cl-lib:fref x-%data% (k) ((1 ncol)) x-%offset%)
                  (/ (f2cl-lib:fref x-%data% (k) ((1 ncol)) x-%offset%)
                     (f2cl-lib:fref w-%data%
                                    (k k)
                                    ((1 nrow) (1 ncol))
                                    w-%offset%)))
          (setf t$ (- (f2cl-lib:fref x-%data% (k) ((1 ncol)) x-%offset%)))
          (if (= t$ 0.0) (go label50))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i km1) nil)
            (tagbody
             label40
              (setf (f2cl-lib:fref x-%data% (i) ((1 ncol)) x-%offset%)
                      (+ (f2cl-lib:fref x-%data% (i) ((1 ncol)) x-%offset%)
                         (*
                          (f2cl-lib:fref w-%data%
                                         (i k)
                                         ((1 nrow) (1 ncol))
                                         w-%offset%)
                          t$)))))
         label50))
     label60
      (setf (f2cl-lib:fref x-%data% (1) ((1 ncol)) x-%offset%)
              (/ (f2cl-lib:fref x-%data% (1) ((1 ncol)) x-%offset%)
                 (f2cl-lib:fref w-%data%
                                (1 1)
                                ((1 nrow) (1 ncol))
                                w-%offset%)))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::subbak
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil)
           :calls 'nil)))

