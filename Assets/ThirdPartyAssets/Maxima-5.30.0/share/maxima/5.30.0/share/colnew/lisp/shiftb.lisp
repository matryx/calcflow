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


(defun shiftb (ai nrowi ncoli last$ ai1 nrowi1 ncoli1)
  (declare (type (f2cl-lib:integer4) ncoli1 nrowi1 last$ ncoli nrowi)
           (type (array double-float (*)) ai1 ai))
  (f2cl-lib:with-multi-array-data
      ((ai double-float ai-%data% ai-%offset%)
       (ai1 double-float ai1-%data% ai1-%offset%))
    (prog ((j 0) (jmax 0) (jmaxp1 0) (m 0) (mmax 0))
      (declare (type (f2cl-lib:integer4) mmax m jmaxp1 jmax j))
      (setf mmax (f2cl-lib:int-sub nrowi last$))
      (setf jmax (f2cl-lib:int-sub ncoli last$))
      (if (or (< mmax 1) (< jmax 1)) (go end_label))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j jmax) nil)
        (tagbody
          (f2cl-lib:fdo (m 1 (f2cl-lib:int-add m 1))
                        ((> m mmax) nil)
            (tagbody
              (setf (f2cl-lib:fref ai1-%data%
                                   (m j)
                                   ((1 nrowi1) (1 ncoli1))
                                   ai1-%offset%)
                      (f2cl-lib:fref ai-%data%
                                     ((f2cl-lib:int-add last$ m)
                                      (f2cl-lib:int-add last$ j))
                                     ((1 nrowi) (1 ncoli))
                                     ai-%offset%))))))
     label10
      (if (= jmax ncoli1) (go end_label))
      (setf jmaxp1 (f2cl-lib:int-add jmax 1))
      (f2cl-lib:fdo (j jmaxp1 (f2cl-lib:int-add j 1))
                    ((> j ncoli1) nil)
        (tagbody
          (f2cl-lib:fdo (m 1 (f2cl-lib:int-add m 1))
                        ((> m mmax) nil)
            (tagbody
              (setf (f2cl-lib:fref ai1-%data%
                                   (m j)
                                   ((1 nrowi1) (1 ncoli1))
                                   ai1-%offset%)
                      0.0)))))
     label20
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::shiftb
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil)
           :calls 'nil)))

