;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 46c1f6a93b0d 2012/05/03 04:40:28 toy $"
;;;  "f2cl2.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 46c1f6a93b0d 2012/05/03 04:40:28 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v fceac530ef0c 2011/11/26 04:02:26 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2012-04 (20C Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun dqpsrt (limit last$ maxerr ermax elist iord nrmax)
  (declare (type (array f2cl-lib:integer4 (*)) iord)
           (type (array double-float (*)) elist)
           (type (double-float) ermax)
           (type (f2cl-lib:integer4) nrmax maxerr last$ limit))
  (f2cl-lib:with-multi-array-data
      ((elist double-float elist-%data% elist-%offset%)
       (iord f2cl-lib:integer4 iord-%data% iord-%offset%))
    (prog ((i 0) (ibeg 0) (ido 0) (isucc 0) (j 0) (jbnd 0) (jupbn 0) (k 0)
           (errmax 0.0) (errmin 0.0))
      (declare (type (double-float) errmin errmax)
               (type (f2cl-lib:integer4) k jupbn jbnd j isucc ido ibeg i))
      (if (> last$ 2) (go label10))
      (setf (f2cl-lib:fref iord-%data% (1) ((1 *)) iord-%offset%) 1)
      (setf (f2cl-lib:fref iord-%data% (2) ((1 *)) iord-%offset%) 2)
      (go label90)
     label10
      (setf errmax
              (f2cl-lib:fref elist-%data% (maxerr) ((1 *)) elist-%offset%))
      (if (= nrmax 1) (go label30))
      (setf ido (f2cl-lib:int-sub nrmax 1))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i ido) nil)
        (tagbody
          (setf isucc
                  (f2cl-lib:fref iord-%data%
                                 ((f2cl-lib:int-sub nrmax 1))
                                 ((1 *))
                                 iord-%offset%))
          (if
           (<= errmax
               (f2cl-lib:fref elist-%data% (isucc) ((1 *)) elist-%offset%))
           (go label30))
          (setf (f2cl-lib:fref iord-%data% (nrmax) ((1 *)) iord-%offset%)
                  isucc)
          (setf nrmax (f2cl-lib:int-sub nrmax 1))
         label20))
     label30
      (setf jupbn last$)
      (if (> last$ (+ (the f2cl-lib:integer4 (truncate limit 2)) 2))
          (setf jupbn (f2cl-lib:int-sub (f2cl-lib:int-add limit 3) last$)))
      (setf errmin (f2cl-lib:fref elist-%data% (last$) ((1 *)) elist-%offset%))
      (setf jbnd (f2cl-lib:int-sub jupbn 1))
      (setf ibeg (f2cl-lib:int-add nrmax 1))
      (if (> ibeg jbnd) (go label50))
      (f2cl-lib:fdo (i ibeg (f2cl-lib:int-add i 1))
                    ((> i jbnd) nil)
        (tagbody
          (setf isucc (f2cl-lib:fref iord-%data% (i) ((1 *)) iord-%offset%))
          (if
           (>= errmax
               (f2cl-lib:fref elist-%data% (isucc) ((1 *)) elist-%offset%))
           (go label60))
          (setf (f2cl-lib:fref iord-%data%
                               ((f2cl-lib:int-sub i 1))
                               ((1 *))
                               iord-%offset%)
                  isucc)
         label40))
     label50
      (setf (f2cl-lib:fref iord-%data% (jbnd) ((1 *)) iord-%offset%) maxerr)
      (setf (f2cl-lib:fref iord-%data% (jupbn) ((1 *)) iord-%offset%) last$)
      (go label90)
     label60
      (setf (f2cl-lib:fref iord-%data%
                           ((f2cl-lib:int-sub i 1))
                           ((1 *))
                           iord-%offset%)
              maxerr)
      (setf k jbnd)
      (f2cl-lib:fdo (j i (f2cl-lib:int-add j 1))
                    ((> j jbnd) nil)
        (tagbody
          (setf isucc (f2cl-lib:fref iord-%data% (k) ((1 *)) iord-%offset%))
          (if
           (< errmin
              (f2cl-lib:fref elist-%data% (isucc) ((1 *)) elist-%offset%))
           (go label80))
          (setf (f2cl-lib:fref iord-%data%
                               ((f2cl-lib:int-add k 1))
                               ((1 *))
                               iord-%offset%)
                  isucc)
          (setf k (f2cl-lib:int-sub k 1))
         label70))
      (setf (f2cl-lib:fref iord-%data% (i) ((1 *)) iord-%offset%) last$)
      (go label90)
     label80
      (setf (f2cl-lib:fref iord-%data%
                           ((f2cl-lib:int-add k 1))
                           ((1 *))
                           iord-%offset%)
              last$)
     label90
      (setf maxerr (f2cl-lib:fref iord-%data% (nrmax) ((1 *)) iord-%offset%))
      (setf ermax (f2cl-lib:fref elist-%data% (maxerr) ((1 *)) elist-%offset%))
      (go end_label)
     end_label
      (return (values nil nil maxerr ermax nil nil nrmax)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dqpsrt
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (double-float)
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil fortran-to-lisp::maxerr
                            fortran-to-lisp::ermax nil nil
                            fortran-to-lisp::nrmax)
           :calls 'nil)))

