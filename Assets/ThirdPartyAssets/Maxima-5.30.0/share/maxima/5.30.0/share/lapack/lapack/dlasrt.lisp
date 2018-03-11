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


(let* ((select 20))
  (declare (type (f2cl-lib:integer4 20 20) select) (ignorable select))
  (defun dlasrt (id n d info)
    (declare (type (array double-float (*)) d)
             (type (f2cl-lib:integer4) info n)
             (type (simple-string *) id))
    (f2cl-lib:with-multi-array-data
        ((id character id-%data% id-%offset%)
         (d double-float d-%data% d-%offset%))
      (prog ((stack (make-array 64 :element-type 'f2cl-lib:integer4)) (d1 0.0)
             (d2 0.0) (d3 0.0) (dmnmx 0.0) (tmp 0.0) (dir 0) (endd 0) (i 0)
             (j 0) (start 0) (stkpnt 0))
        (declare (type (array f2cl-lib:integer4 (64)) stack)
                 (type (double-float) d1 d2 d3 dmnmx tmp)
                 (type (f2cl-lib:integer4) dir endd i j start stkpnt))
        (setf info 0)
        (setf dir -1)
        (cond
          ((lsame id "D")
           (setf dir 0))
          ((lsame id "I")
           (setf dir 1)))
        (cond
          ((= dir (f2cl-lib:int-sub 1))
           (setf info -1))
          ((< n 0)
           (setf info -2)))
        (cond
          ((/= info 0)
           (xerbla "DLASRT" (f2cl-lib:int-sub info))
           (go end_label)))
        (if (<= n 1) (go end_label))
        (setf stkpnt 1)
        (setf (f2cl-lib:fref stack (1 1) ((1 2) (1 32))) 1)
        (setf (f2cl-lib:fref stack (2 1) ((1 2) (1 32))) n)
       label10
        (setf start (f2cl-lib:fref stack (1 stkpnt) ((1 2) (1 32))))
        (setf endd (f2cl-lib:fref stack (2 stkpnt) ((1 2) (1 32))))
        (setf stkpnt (f2cl-lib:int-sub stkpnt 1))
        (cond
          ((and (<= (f2cl-lib:int-add endd (f2cl-lib:int-sub start)) select)
                (> (f2cl-lib:int-add endd (f2cl-lib:int-sub start)) 0))
           (cond
             ((= dir 0)
              (f2cl-lib:fdo (i (f2cl-lib:int-add start 1)
                             (f2cl-lib:int-add i 1))
                            ((> i endd) nil)
                (tagbody
                  (f2cl-lib:fdo (j i (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                                ((> j (f2cl-lib:int-add start 1)) nil)
                    (tagbody
                      (cond
                        ((> (f2cl-lib:fref d (j) ((1 *)))
                            (f2cl-lib:fref d
                                           ((f2cl-lib:int-add j
                                                              (f2cl-lib:int-sub
                                                               1)))
                                           ((1 *))))
                         (setf dmnmx
                                 (f2cl-lib:fref d-%data%
                                                (j)
                                                ((1 *))
                                                d-%offset%))
                         (setf (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                                 (f2cl-lib:fref d-%data%
                                                ((f2cl-lib:int-sub j 1))
                                                ((1 *))
                                                d-%offset%))
                         (setf (f2cl-lib:fref d-%data%
                                              ((f2cl-lib:int-sub j 1))
                                              ((1 *))
                                              d-%offset%)
                                 dmnmx))
                        (t
                         (go label30)))
                     label20))
                 label30)))
             (t
              (f2cl-lib:fdo (i (f2cl-lib:int-add start 1)
                             (f2cl-lib:int-add i 1))
                            ((> i endd) nil)
                (tagbody
                  (f2cl-lib:fdo (j i (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                                ((> j (f2cl-lib:int-add start 1)) nil)
                    (tagbody
                      (cond
                        ((< (f2cl-lib:fref d (j) ((1 *)))
                            (f2cl-lib:fref d
                                           ((f2cl-lib:int-add j
                                                              (f2cl-lib:int-sub
                                                               1)))
                                           ((1 *))))
                         (setf dmnmx
                                 (f2cl-lib:fref d-%data%
                                                (j)
                                                ((1 *))
                                                d-%offset%))
                         (setf (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                                 (f2cl-lib:fref d-%data%
                                                ((f2cl-lib:int-sub j 1))
                                                ((1 *))
                                                d-%offset%))
                         (setf (f2cl-lib:fref d-%data%
                                              ((f2cl-lib:int-sub j 1))
                                              ((1 *))
                                              d-%offset%)
                                 dmnmx))
                        (t
                         (go label50)))
                     label40))
                 label50)))))
          ((> (f2cl-lib:int-add endd (f2cl-lib:int-sub start)) select)
           (setf d1 (f2cl-lib:fref d-%data% (start) ((1 *)) d-%offset%))
           (setf d2 (f2cl-lib:fref d-%data% (endd) ((1 *)) d-%offset%))
           (setf i (the f2cl-lib:integer4 (truncate (+ start endd) 2)))
           (setf d3 (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
           (cond
             ((< d1 d2)
              (cond
                ((< d3 d1)
                 (setf dmnmx d1))
                ((< d3 d2)
                 (setf dmnmx d3))
                (t
                 (setf dmnmx d2))))
             (t
              (cond
                ((< d3 d2)
                 (setf dmnmx d2))
                ((< d3 d1)
                 (setf dmnmx d3))
                (t
                 (setf dmnmx d1)))))
           (cond
             ((= dir 0)
              (tagbody
                (setf i (f2cl-lib:int-sub start 1))
                (setf j (f2cl-lib:int-add endd 1))
               label60
               label70
                (setf j (f2cl-lib:int-sub j 1))
                (if (< (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%) dmnmx)
                    (go label70))
               label80
                (setf i (f2cl-lib:int-add i 1))
                (if (> (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%) dmnmx)
                    (go label80))
                (cond
                  ((< i j)
                   (setf tmp (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
                   (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                           (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%))
                   (setf (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%) tmp)
                   (go label60)))
                (cond
                  ((> (f2cl-lib:int-add j (f2cl-lib:int-sub start))
                      (f2cl-lib:int-add endd
                                        (f2cl-lib:int-sub j)
                                        (f2cl-lib:int-sub 1)))
                   (setf stkpnt (f2cl-lib:int-add stkpnt 1))
                   (setf (f2cl-lib:fref stack (1 stkpnt) ((1 2) (1 32))) start)
                   (setf (f2cl-lib:fref stack (2 stkpnt) ((1 2) (1 32))) j)
                   (setf stkpnt (f2cl-lib:int-add stkpnt 1))
                   (setf (f2cl-lib:fref stack (1 stkpnt) ((1 2) (1 32)))
                           (f2cl-lib:int-add j 1))
                   (setf (f2cl-lib:fref stack (2 stkpnt) ((1 2) (1 32))) endd))
                  (t
                   (setf stkpnt (f2cl-lib:int-add stkpnt 1))
                   (setf (f2cl-lib:fref stack (1 stkpnt) ((1 2) (1 32)))
                           (f2cl-lib:int-add j 1))
                   (setf (f2cl-lib:fref stack (2 stkpnt) ((1 2) (1 32))) endd)
                   (setf stkpnt (f2cl-lib:int-add stkpnt 1))
                   (setf (f2cl-lib:fref stack (1 stkpnt) ((1 2) (1 32))) start)
                   (setf (f2cl-lib:fref stack (2 stkpnt) ((1 2) (1 32))) j)))))
             (t
              (tagbody
                (setf i (f2cl-lib:int-sub start 1))
                (setf j (f2cl-lib:int-add endd 1))
               label90
               label100
                (setf j (f2cl-lib:int-sub j 1))
                (if (> (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%) dmnmx)
                    (go label100))
               label110
                (setf i (f2cl-lib:int-add i 1))
                (if (< (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%) dmnmx)
                    (go label110))
                (cond
                  ((< i j)
                   (setf tmp (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
                   (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                           (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%))
                   (setf (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%) tmp)
                   (go label90)))
                (cond
                  ((> (f2cl-lib:int-add j (f2cl-lib:int-sub start))
                      (f2cl-lib:int-add endd
                                        (f2cl-lib:int-sub j)
                                        (f2cl-lib:int-sub 1)))
                   (setf stkpnt (f2cl-lib:int-add stkpnt 1))
                   (setf (f2cl-lib:fref stack (1 stkpnt) ((1 2) (1 32))) start)
                   (setf (f2cl-lib:fref stack (2 stkpnt) ((1 2) (1 32))) j)
                   (setf stkpnt (f2cl-lib:int-add stkpnt 1))
                   (setf (f2cl-lib:fref stack (1 stkpnt) ((1 2) (1 32)))
                           (f2cl-lib:int-add j 1))
                   (setf (f2cl-lib:fref stack (2 stkpnt) ((1 2) (1 32))) endd))
                  (t
                   (setf stkpnt (f2cl-lib:int-add stkpnt 1))
                   (setf (f2cl-lib:fref stack (1 stkpnt) ((1 2) (1 32)))
                           (f2cl-lib:int-add j 1))
                   (setf (f2cl-lib:fref stack (2 stkpnt) ((1 2) (1 32))) endd)
                   (setf stkpnt (f2cl-lib:int-add stkpnt 1))
                   (setf (f2cl-lib:fref stack (1 stkpnt) ((1 2) (1 32))) start)
                   (setf (f2cl-lib:fref stack (2 stkpnt) ((1 2) (1 32)))
                           j))))))))
        (if (> stkpnt 0) (go label10))
        (go end_label)
       end_label
        (return (values nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasrt
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::xerbla fortran-to-lisp::lsame))))

