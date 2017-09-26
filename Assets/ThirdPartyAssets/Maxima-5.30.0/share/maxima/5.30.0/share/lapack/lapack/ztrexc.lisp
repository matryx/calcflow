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


(defun ztrexc (compq n t$ ldt q ldq ifst ilst info)
  (declare (type (array f2cl-lib:complex16 (*)) q t$)
           (type (f2cl-lib:integer4) info ilst ifst ldq ldt n)
           (type (simple-string *) compq))
  (f2cl-lib:with-multi-array-data
      ((compq character compq-%data% compq-%offset%)
       (t$ f2cl-lib:complex16 t$-%data% t$-%offset%)
       (q f2cl-lib:complex16 q-%data% q-%offset%))
    (prog ((sn #C(0.0 0.0)) (t11 #C(0.0 0.0)) (t22 #C(0.0 0.0))
           (temp #C(0.0 0.0)) (cs 0.0) (k 0) (m1 0) (m2 0) (m3 0) (wantq nil)
           (dconjg$ 0.0f0))
      (declare (type (single-float) dconjg$)
               (type f2cl-lib:logical wantq)
               (type (f2cl-lib:integer4) m3 m2 m1 k)
               (type (double-float) cs)
               (type (f2cl-lib:complex16) temp t22 t11 sn))
      (setf info 0)
      (setf wantq (lsame compq "V"))
      (cond
        ((and (not (lsame compq "N")) (not wantq))
         (setf info -1))
        ((< n 0)
         (setf info -2))
        ((< ldt (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
         (setf info -4))
        ((or (< ldq 1)
             (and wantq
                  (< ldq
                     (max (the f2cl-lib:integer4 1)
                          (the f2cl-lib:integer4 n)))))
         (setf info -6))
        ((or (< ifst 1) (> ifst n))
         (setf info -7))
        ((or (< ilst 1) (> ilst n))
         (setf info -8)))
      (cond
        ((/= info 0)
         (xerbla "ZTREXC" (f2cl-lib:int-sub info))
         (go end_label)))
      (if (or (= n 1) (= ifst ilst)) (go end_label))
      (cond
        ((< ifst ilst)
         (setf m1 0)
         (setf m2 -1)
         (setf m3 1))
        (t
         (setf m1 -1)
         (setf m2 0)
         (setf m3 -1)))
      (f2cl-lib:fdo (k (f2cl-lib:int-add ifst m1) (f2cl-lib:int-add k m3))
                    ((> k (f2cl-lib:int-add ilst m2)) nil)
        (tagbody
          (setf t11
                  (f2cl-lib:fref t$-%data% (k k) ((1 ldt) (1 *)) t$-%offset%))
          (setf t22
                  (f2cl-lib:fref t$-%data%
                                 ((f2cl-lib:int-add k 1)
                                  (f2cl-lib:int-add k 1))
                                 ((1 ldt) (1 *))
                                 t$-%offset%))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (zlartg
               (f2cl-lib:fref t$-%data%
                              (k (f2cl-lib:int-add k 1))
                              ((1 ldt) (1 *))
                              t$-%offset%)
               (- t22 t11) cs sn temp)
            (declare (ignore var-0 var-1))
            (setf cs var-2)
            (setf sn var-3)
            (setf temp var-4))
          (if (<= (f2cl-lib:int-add k 2) n)
              (zrot (f2cl-lib:int-sub n k 1)
               (f2cl-lib:array-slice t$-%data%
                                     f2cl-lib:complex16
                                     (k (f2cl-lib:int-add k 2))
                                     ((1 ldt) (1 *))
                                     t$-%offset%)
               ldt
               (f2cl-lib:array-slice t$-%data%
                                     f2cl-lib:complex16
                                     ((+ k 1) (f2cl-lib:int-add k 2))
                                     ((1 ldt) (1 *))
                                     t$-%offset%)
               ldt cs sn))
          (zrot (f2cl-lib:int-sub k 1)
           (f2cl-lib:array-slice t$-%data%
                                 f2cl-lib:complex16
                                 (1 k)
                                 ((1 ldt) (1 *))
                                 t$-%offset%)
           1
           (f2cl-lib:array-slice t$-%data%
                                 f2cl-lib:complex16
                                 (1 (f2cl-lib:int-add k 1))
                                 ((1 ldt) (1 *))
                                 t$-%offset%)
           1 cs (f2cl-lib:dconjg sn))
          (setf (f2cl-lib:fref t$-%data% (k k) ((1 ldt) (1 *)) t$-%offset%)
                  t22)
          (setf (f2cl-lib:fref t$-%data%
                               ((f2cl-lib:int-add k 1) (f2cl-lib:int-add k 1))
                               ((1 ldt) (1 *))
                               t$-%offset%)
                  t11)
          (cond
            (wantq
             (zrot n
              (f2cl-lib:array-slice q-%data%
                                    f2cl-lib:complex16
                                    (1 k)
                                    ((1 ldq) (1 *))
                                    q-%offset%)
              1
              (f2cl-lib:array-slice q-%data%
                                    f2cl-lib:complex16
                                    (1 (f2cl-lib:int-add k 1))
                                    ((1 ldq) (1 *))
                                    q-%offset%)
              1 cs (f2cl-lib:dconjg sn))))
         label10))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil info)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::ztrexc
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::zrot fortran-to-lisp::zlartg
                    fortran-to-lisp::xerbla fortran-to-lisp::lsame))))

