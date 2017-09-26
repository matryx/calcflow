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


(defun rkbas (s coef k m rkb dm mode)
  (declare (type (array double-float (*)) dm)
           (type (array double-float (*)) rkb)
           (type (f2cl-lib:integer4) mode m k)
           (type (array double-float (*)) coef)
           (type double-float s))
  (f2cl-lib:with-multi-array-data
      ((coef double-float coef-%data% coef-%offset%)
       (rkb double-float rkb-%data% rkb-%offset%)
       (dm double-float dm-%data% dm-%offset%))
    (prog ((t$ (make-array 10 :element-type 'double-float)) (j 0) (p 0.0)
           (lb 0) (l 0) (i 0) (kpm1 0))
      (declare (type double-float p)
               (type (f2cl-lib:integer4) kpm1 i l lb j)
               (type (array double-float (10)) t$))
      (if (= k 1) (go label70))
      (setf kpm1 (f2cl-lib:int-sub (f2cl-lib:int-add k m) 1))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i kpm1) nil)
        (tagbody
         label10
          (setf (f2cl-lib:fref t$ (i) ((1 10))) (/ s (f2cl-lib:dfloat i)))))
      (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                    ((> l m) nil)
        (tagbody
          (setf lb (f2cl-lib:int-add k l 1))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i k) nil)
            (tagbody
              (setf p
                      (f2cl-lib:fref coef-%data%
                                     (1 i)
                                     ((1 k) (1 1))
                                     coef-%offset%))
              (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (setf p
                          (+
                           (* p
                              (f2cl-lib:fref t$
                                             ((f2cl-lib:int-sub lb j))
                                             ((1 10))))
                           (f2cl-lib:fref coef-%data%
                                          (j i)
                                          ((1 k) (1 1))
                                          coef-%offset%)))
                 label20))
              (setf (f2cl-lib:fref rkb-%data% (i l) ((1 7) (1 1)) rkb-%offset%)
                      p)
             label30))
         label40))
      (if (= mode 0) (go end_label))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i k) nil)
        (tagbody
          (setf p
                  (f2cl-lib:fref coef-%data%
                                 (1 i)
                                 ((1 k) (1 1))
                                 coef-%offset%))
          (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                        ((> j k) nil)
            (tagbody
             label50
              (setf p
                      (+
                       (* p
                          (f2cl-lib:fref t$
                                         ((f2cl-lib:int-sub
                                           (f2cl-lib:int-add k 1)
                                           j))
                                         ((1 10))))
                       (f2cl-lib:fref coef-%data%
                                      (j i)
                                      ((1 k) (1 1))
                                      coef-%offset%)))))
          (setf (f2cl-lib:fref dm-%data% (i) ((1 1)) dm-%offset%) p)
         label60))
      (go end_label)
     label70
      (setf (f2cl-lib:fref rkb-%data% (1 1) ((1 7) (1 1)) rkb-%offset%) 1.0)
      (setf (f2cl-lib:fref dm-%data% (1) ((1 1)) dm-%offset%) 1.0)
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::rkbas fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(double-float (array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (7)) (array double-float (1))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil)
           :calls 'nil)))

