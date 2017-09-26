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

(in-package :lapack)


(let* ((two 2.0))
  (declare (type (double-float 2.0 2.0) two) (ignorable two))
  (defun dlasdt (n lvl nd inode ndiml ndimr msub)
    (declare (type (array f2cl-lib:integer4 (*)) ndimr ndiml inode)
             (type (f2cl-lib:integer4) msub nd lvl n))
    (f2cl-lib:with-multi-array-data
        ((inode f2cl-lib:integer4 inode-%data% inode-%offset%)
         (ndiml f2cl-lib:integer4 ndiml-%data% ndiml-%offset%)
         (ndimr f2cl-lib:integer4 ndimr-%data% ndimr-%offset%))
      (prog ((temp 0.0) (i 0) (il 0) (ir 0) (llst 0) (maxn 0) (ncrnt 0)
             (nlvl 0))
        (declare (type (double-float) temp)
                 (type (f2cl-lib:integer4) i il ir llst maxn ncrnt nlvl))
        (setf maxn (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
        (setf temp
                (/
                 (f2cl-lib:flog
                  (/ (f2cl-lib:dble maxn)
                     (f2cl-lib:dble (f2cl-lib:int-add msub 1))))
                 (f2cl-lib:flog two)))
        (setf lvl (f2cl-lib:int-add (f2cl-lib:int temp) 1))
        (setf i (the f2cl-lib:integer4 (truncate n 2)))
        (setf (f2cl-lib:fref inode-%data% (1) ((1 *)) inode-%offset%)
                (f2cl-lib:int-add i 1))
        (setf (f2cl-lib:fref ndiml-%data% (1) ((1 *)) ndiml-%offset%) i)
        (setf (f2cl-lib:fref ndimr-%data% (1) ((1 *)) ndimr-%offset%)
                (f2cl-lib:int-sub n i 1))
        (setf il 0)
        (setf ir 1)
        (setf llst 1)
        (f2cl-lib:fdo (nlvl 1 (f2cl-lib:int-add nlvl 1))
                      ((> nlvl (f2cl-lib:int-add lvl (f2cl-lib:int-sub 1))) nil)
          (tagbody
            (f2cl-lib:fdo (i 0 (f2cl-lib:int-add i 1))
                          ((> i (f2cl-lib:int-add llst (f2cl-lib:int-sub 1)))
                           nil)
              (tagbody
                (setf il (f2cl-lib:int-add il 2))
                (setf ir (f2cl-lib:int-add ir 2))
                (setf ncrnt (f2cl-lib:int-add llst i))
                (setf (f2cl-lib:fref ndiml-%data% (il) ((1 *)) ndiml-%offset%)
                        (the f2cl-lib:integer4
                             (truncate
                              (f2cl-lib:fref ndiml-%data%
                                             (ncrnt)
                                             ((1 *))
                                             ndiml-%offset%)
                              2)))
                (setf (f2cl-lib:fref ndimr-%data% (il) ((1 *)) ndimr-%offset%)
                        (f2cl-lib:int-sub
                         (f2cl-lib:fref ndiml-%data%
                                        (ncrnt)
                                        ((1 *))
                                        ndiml-%offset%)
                         (f2cl-lib:fref ndiml-%data%
                                        (il)
                                        ((1 *))
                                        ndiml-%offset%)
                         1))
                (setf (f2cl-lib:fref inode-%data% (il) ((1 *)) inode-%offset%)
                        (f2cl-lib:int-sub
                         (f2cl-lib:fref inode-%data%
                                        (ncrnt)
                                        ((1 *))
                                        inode-%offset%)
                         (f2cl-lib:fref ndimr-%data%
                                        (il)
                                        ((1 *))
                                        ndimr-%offset%)
                         1))
                (setf (f2cl-lib:fref ndiml-%data% (ir) ((1 *)) ndiml-%offset%)
                        (the f2cl-lib:integer4
                             (truncate
                              (f2cl-lib:fref ndimr-%data%
                                             (ncrnt)
                                             ((1 *))
                                             ndimr-%offset%)
                              2)))
                (setf (f2cl-lib:fref ndimr-%data% (ir) ((1 *)) ndimr-%offset%)
                        (f2cl-lib:int-sub
                         (f2cl-lib:fref ndimr-%data%
                                        (ncrnt)
                                        ((1 *))
                                        ndimr-%offset%)
                         (f2cl-lib:fref ndiml-%data%
                                        (ir)
                                        ((1 *))
                                        ndiml-%offset%)
                         1))
                (setf (f2cl-lib:fref inode-%data% (ir) ((1 *)) inode-%offset%)
                        (f2cl-lib:int-add
                         (f2cl-lib:fref inode-%data%
                                        (ncrnt)
                                        ((1 *))
                                        inode-%offset%)
                         (f2cl-lib:fref ndiml-%data%
                                        (ir)
                                        ((1 *))
                                        ndiml-%offset%)
                         1))
               label10))
            (setf llst (f2cl-lib:int-mul llst 2))
           label20))
        (setf nd (f2cl-lib:int-sub (f2cl-lib:int-mul llst 2) 1))
        (go end_label)
       end_label
        (return (values nil lvl nd nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasdt
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil fortran-to-lisp::lvl fortran-to-lisp::nd nil
                            nil nil nil)
           :calls 'nil)))

