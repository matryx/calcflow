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


(defun factrb (w ipivot d nrow ncol last$ info)
  (declare (type (f2cl-lib:integer4) info last$ ncol nrow)
           (type (array f2cl-lib:integer4 (*)) ipivot)
           (type (array double-float (*)) d w))
  (f2cl-lib:with-multi-array-data
      ((w double-float w-%data% w-%offset%)
       (d double-float d-%data% d-%offset%)
       (ipivot f2cl-lib:integer4 ipivot-%data% ipivot-%offset%))
    (prog ((dabs$ 0.0) (dmax1$ 0.0) (colmax 0.0) (t$ 0.0) (s 0.0) (i 0) (j 0)
           (k 0) (l 0) (kp1 0))
      (declare (type (f2cl-lib:integer4) kp1 l k j i)
               (type (double-float) s t$ colmax dmax1$ dabs$))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i nrow) nil)
        (tagbody
          (setf (f2cl-lib:fref d-%data% (i) ((1 nrow)) d-%offset%) 0.0)
         label10))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j ncol) nil)
        (tagbody
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i nrow) nil)
            (tagbody
              (setf (f2cl-lib:fref d-%data% (i) ((1 nrow)) d-%offset%)
                      (f2cl-lib:dmax1
                       (f2cl-lib:fref d-%data% (i) ((1 nrow)) d-%offset%)
                       (f2cl-lib:dabs
                        (f2cl-lib:fref w-%data%
                                       (i j)
                                       ((1 nrow) (1 ncol))
                                       w-%offset%))))
             label20))))
     label20
      (setf k 1)
     label30
      (if (= (f2cl-lib:fref d-%data% (k) ((1 nrow)) d-%offset%) 0.0)
          (go label90))
      (if (= k nrow) (go label80))
      (setf l k)
      (setf kp1 (f2cl-lib:int-add k 1))
      (setf colmax
              (/
               (f2cl-lib:dabs
                (f2cl-lib:fref w-%data% (k k) ((1 nrow) (1 ncol)) w-%offset%))
               (f2cl-lib:fref d-%data% (k) ((1 nrow)) d-%offset%)))
      (f2cl-lib:fdo (i kp1 (f2cl-lib:int-add i 1))
                    ((> i nrow) nil)
        (tagbody
          (if
           (<=
            (f2cl-lib:dabs
             (f2cl-lib:fref w-%data% (i k) ((1 nrow) (1 ncol)) w-%offset%))
            (* colmax (f2cl-lib:fref d-%data% (i) ((1 nrow)) d-%offset%)))
           (go label40))
          (setf colmax
                  (/
                   (f2cl-lib:dabs
                    (f2cl-lib:fref w-%data%
                                   (i k)
                                   ((1 nrow) (1 ncol))
                                   w-%offset%))
                   (f2cl-lib:fref d-%data% (i) ((1 nrow)) d-%offset%)))
          (setf l i)
         label40))
      (setf (f2cl-lib:fref ipivot-%data% (k) ((1 nrow)) ipivot-%offset%) l)
      (setf t$ (f2cl-lib:fref w-%data% (l k) ((1 nrow) (1 ncol)) w-%offset%))
      (setf s (f2cl-lib:fref d-%data% (l) ((1 nrow)) d-%offset%))
      (if (= l k) (go label50))
      (setf (f2cl-lib:fref w-%data% (l k) ((1 nrow) (1 ncol)) w-%offset%)
              (f2cl-lib:fref w-%data% (k k) ((1 nrow) (1 ncol)) w-%offset%))
      (setf (f2cl-lib:fref w-%data% (k k) ((1 nrow) (1 ncol)) w-%offset%) t$)
      (setf (f2cl-lib:fref d-%data% (l) ((1 nrow)) d-%offset%)
              (f2cl-lib:fref d-%data% (k) ((1 nrow)) d-%offset%))
      (setf (f2cl-lib:fref d-%data% (k) ((1 nrow)) d-%offset%) s)
     label50
      (if
       (<=
        (+ (f2cl-lib:dabs t$)
           (f2cl-lib:fref d-%data% (k) ((1 nrow)) d-%offset%))
        (f2cl-lib:fref d-%data% (k) ((1 nrow)) d-%offset%))
       (go label90))
      (setf t$ (/ -1.0 t$))
      (f2cl-lib:fdo (i kp1 (f2cl-lib:int-add i 1))
                    ((> i nrow) nil)
        (tagbody
         label60
          (setf (f2cl-lib:fref w-%data% (i k) ((1 nrow) (1 ncol)) w-%offset%)
                  (*
                   (f2cl-lib:fref w-%data%
                                  (i k)
                                  ((1 nrow) (1 ncol))
                                  w-%offset%)
                   t$))))
      (f2cl-lib:fdo (j kp1 (f2cl-lib:int-add j 1))
                    ((> j ncol) nil)
        (tagbody
          (setf t$
                  (f2cl-lib:fref w-%data%
                                 (l j)
                                 ((1 nrow) (1 ncol))
                                 w-%offset%))
          (if (= l k) (go label62))
          (setf (f2cl-lib:fref w-%data% (l j) ((1 nrow) (1 ncol)) w-%offset%)
                  (f2cl-lib:fref w-%data%
                                 (k j)
                                 ((1 nrow) (1 ncol))
                                 w-%offset%))
          (setf (f2cl-lib:fref w-%data% (k j) ((1 nrow) (1 ncol)) w-%offset%)
                  t$)
         label62
          (if (= t$ 0.0) (go label70))
          (f2cl-lib:fdo (i kp1 (f2cl-lib:int-add i 1))
                        ((> i nrow) nil)
            (tagbody
             label64
              (setf (f2cl-lib:fref w-%data%
                                   (i j)
                                   ((1 nrow) (1 ncol))
                                   w-%offset%)
                      (+
                       (f2cl-lib:fref w-%data%
                                      (i j)
                                      ((1 nrow) (1 ncol))
                                      w-%offset%)
                       (*
                        (f2cl-lib:fref w-%data%
                                       (i k)
                                       ((1 nrow) (1 ncol))
                                       w-%offset%)
                        t$)))))
         label70))
      (setf k kp1)
      (if (<= k last$) (go label30))
      (go end_label)
     label80
      (if
       (>
        (+
         (f2cl-lib:dabs
          (f2cl-lib:fref w-%data% (nrow nrow) ((1 nrow) (1 ncol)) w-%offset%))
         (f2cl-lib:fref d-%data% (nrow) ((1 nrow)) d-%offset%))
        (f2cl-lib:fref d-%data% (nrow) ((1 nrow)) d-%offset%))
       (go end_label))
     label90
      (setf info k)
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil info)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::factrb
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil fortran-to-lisp::info)
           :calls 'nil)))

