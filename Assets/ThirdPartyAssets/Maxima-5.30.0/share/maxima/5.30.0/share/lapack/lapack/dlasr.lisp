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


(let* ((one 1.0) (zero 0.0))
  (declare (type (double-float 1.0 1.0) one)
           (type (double-float 0.0 0.0) zero)
           (ignorable one zero))
  (defun dlasr (side pivot direct m n c s a lda)
    (declare (type (array double-float (*)) a s c)
             (type (f2cl-lib:integer4) lda n m)
             (type (simple-string *) direct pivot side))
    (f2cl-lib:with-multi-array-data
        ((side character side-%data% side-%offset%)
         (pivot character pivot-%data% pivot-%offset%)
         (direct character direct-%data% direct-%offset%)
         (c double-float c-%data% c-%offset%)
         (s double-float s-%data% s-%offset%)
         (a double-float a-%data% a-%offset%))
      (prog ((ctemp 0.0) (stemp 0.0) (temp 0.0) (i 0) (info 0) (j 0))
        (declare (type (double-float) ctemp stemp temp)
                 (type (f2cl-lib:integer4) i info j))
        (setf info 0)
        (cond
          ((not (or (lsame side "L") (lsame side "R")))
           (setf info 1))
          ((not (or (lsame pivot "V") (lsame pivot "T") (lsame pivot "B")))
           (setf info 2))
          ((not (or (lsame direct "F") (lsame direct "B")))
           (setf info 3))
          ((< m 0)
           (setf info 4))
          ((< n 0)
           (setf info 5))
          ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
           (setf info 9)))
        (cond
          ((/= info 0)
           (xerbla "DLASR " info)
           (go end_label)))
        (if (or (= m 0) (= n 0)) (go end_label))
        (cond
          ((lsame side "L")
           (cond
             ((lsame pivot "V")
              (cond
                ((lsame direct "F")
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j (f2cl-lib:int-add m (f2cl-lib:int-sub 1)))
                                nil)
                   (tagbody
                     (setf ctemp
                             (f2cl-lib:fref c-%data% (j) ((1 *)) c-%offset%))
                     (setf stemp
                             (f2cl-lib:fref s-%data% (j) ((1 *)) s-%offset%))
                     (cond
                       ((or (/= ctemp one) (/= stemp zero))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i n) nil)
                          (tagbody
                            (setf temp
                                    (f2cl-lib:fref a-%data%
                                                   ((f2cl-lib:int-add j 1) i)
                                                   ((1 lda) (1 *))
                                                   a-%offset%))
                            (setf (f2cl-lib:fref a-%data%
                                                 ((f2cl-lib:int-add j 1) i)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (- (* ctemp temp)
                                       (* stemp
                                          (f2cl-lib:fref a-%data%
                                                         (j i)
                                                         ((1 lda) (1 *))
                                                         a-%offset%))))
                            (setf (f2cl-lib:fref a-%data%
                                                 (j i)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (+ (* stemp temp)
                                       (* ctemp
                                          (f2cl-lib:fref a-%data%
                                                         (j i)
                                                         ((1 lda) (1 *))
                                                         a-%offset%))))
                           label10))))
                    label20)))
                ((lsame direct "B")
                 (f2cl-lib:fdo (j (f2cl-lib:int-add m (f2cl-lib:int-sub 1))
                                (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                               ((> j 1) nil)
                   (tagbody
                     (setf ctemp
                             (f2cl-lib:fref c-%data% (j) ((1 *)) c-%offset%))
                     (setf stemp
                             (f2cl-lib:fref s-%data% (j) ((1 *)) s-%offset%))
                     (cond
                       ((or (/= ctemp one) (/= stemp zero))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i n) nil)
                          (tagbody
                            (setf temp
                                    (f2cl-lib:fref a-%data%
                                                   ((f2cl-lib:int-add j 1) i)
                                                   ((1 lda) (1 *))
                                                   a-%offset%))
                            (setf (f2cl-lib:fref a-%data%
                                                 ((f2cl-lib:int-add j 1) i)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (- (* ctemp temp)
                                       (* stemp
                                          (f2cl-lib:fref a-%data%
                                                         (j i)
                                                         ((1 lda) (1 *))
                                                         a-%offset%))))
                            (setf (f2cl-lib:fref a-%data%
                                                 (j i)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (+ (* stemp temp)
                                       (* ctemp
                                          (f2cl-lib:fref a-%data%
                                                         (j i)
                                                         ((1 lda) (1 *))
                                                         a-%offset%))))
                           label30))))
                    label40)))))
             ((lsame pivot "T")
              (cond
                ((lsame direct "F")
                 (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                               ((> j m) nil)
                   (tagbody
                     (setf ctemp
                             (f2cl-lib:fref c-%data%
                                            ((f2cl-lib:int-sub j 1))
                                            ((1 *))
                                            c-%offset%))
                     (setf stemp
                             (f2cl-lib:fref s-%data%
                                            ((f2cl-lib:int-sub j 1))
                                            ((1 *))
                                            s-%offset%))
                     (cond
                       ((or (/= ctemp one) (/= stemp zero))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i n) nil)
                          (tagbody
                            (setf temp
                                    (f2cl-lib:fref a-%data%
                                                   (j i)
                                                   ((1 lda) (1 *))
                                                   a-%offset%))
                            (setf (f2cl-lib:fref a-%data%
                                                 (j i)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (- (* ctemp temp)
                                       (* stemp
                                          (f2cl-lib:fref a-%data%
                                                         (1 i)
                                                         ((1 lda) (1 *))
                                                         a-%offset%))))
                            (setf (f2cl-lib:fref a-%data%
                                                 (1 i)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (+ (* stemp temp)
                                       (* ctemp
                                          (f2cl-lib:fref a-%data%
                                                         (1 i)
                                                         ((1 lda) (1 *))
                                                         a-%offset%))))
                           label50))))
                    label60)))
                ((lsame direct "B")
                 (f2cl-lib:fdo (j m (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                               ((> j 2) nil)
                   (tagbody
                     (setf ctemp
                             (f2cl-lib:fref c-%data%
                                            ((f2cl-lib:int-sub j 1))
                                            ((1 *))
                                            c-%offset%))
                     (setf stemp
                             (f2cl-lib:fref s-%data%
                                            ((f2cl-lib:int-sub j 1))
                                            ((1 *))
                                            s-%offset%))
                     (cond
                       ((or (/= ctemp one) (/= stemp zero))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i n) nil)
                          (tagbody
                            (setf temp
                                    (f2cl-lib:fref a-%data%
                                                   (j i)
                                                   ((1 lda) (1 *))
                                                   a-%offset%))
                            (setf (f2cl-lib:fref a-%data%
                                                 (j i)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (- (* ctemp temp)
                                       (* stemp
                                          (f2cl-lib:fref a-%data%
                                                         (1 i)
                                                         ((1 lda) (1 *))
                                                         a-%offset%))))
                            (setf (f2cl-lib:fref a-%data%
                                                 (1 i)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (+ (* stemp temp)
                                       (* ctemp
                                          (f2cl-lib:fref a-%data%
                                                         (1 i)
                                                         ((1 lda) (1 *))
                                                         a-%offset%))))
                           label70))))
                    label80)))))
             ((lsame pivot "B")
              (cond
                ((lsame direct "F")
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j (f2cl-lib:int-add m (f2cl-lib:int-sub 1)))
                                nil)
                   (tagbody
                     (setf ctemp
                             (f2cl-lib:fref c-%data% (j) ((1 *)) c-%offset%))
                     (setf stemp
                             (f2cl-lib:fref s-%data% (j) ((1 *)) s-%offset%))
                     (cond
                       ((or (/= ctemp one) (/= stemp zero))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i n) nil)
                          (tagbody
                            (setf temp
                                    (f2cl-lib:fref a-%data%
                                                   (j i)
                                                   ((1 lda) (1 *))
                                                   a-%offset%))
                            (setf (f2cl-lib:fref a-%data%
                                                 (j i)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (+
                                     (* stemp
                                        (f2cl-lib:fref a-%data%
                                                       (m i)
                                                       ((1 lda) (1 *))
                                                       a-%offset%))
                                     (* ctemp temp)))
                            (setf (f2cl-lib:fref a-%data%
                                                 (m i)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (-
                                     (* ctemp
                                        (f2cl-lib:fref a-%data%
                                                       (m i)
                                                       ((1 lda) (1 *))
                                                       a-%offset%))
                                     (* stemp temp)))
                           label90))))
                    label100)))
                ((lsame direct "B")
                 (f2cl-lib:fdo (j (f2cl-lib:int-add m (f2cl-lib:int-sub 1))
                                (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                               ((> j 1) nil)
                   (tagbody
                     (setf ctemp
                             (f2cl-lib:fref c-%data% (j) ((1 *)) c-%offset%))
                     (setf stemp
                             (f2cl-lib:fref s-%data% (j) ((1 *)) s-%offset%))
                     (cond
                       ((or (/= ctemp one) (/= stemp zero))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i n) nil)
                          (tagbody
                            (setf temp
                                    (f2cl-lib:fref a-%data%
                                                   (j i)
                                                   ((1 lda) (1 *))
                                                   a-%offset%))
                            (setf (f2cl-lib:fref a-%data%
                                                 (j i)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (+
                                     (* stemp
                                        (f2cl-lib:fref a-%data%
                                                       (m i)
                                                       ((1 lda) (1 *))
                                                       a-%offset%))
                                     (* ctemp temp)))
                            (setf (f2cl-lib:fref a-%data%
                                                 (m i)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (-
                                     (* ctemp
                                        (f2cl-lib:fref a-%data%
                                                       (m i)
                                                       ((1 lda) (1 *))
                                                       a-%offset%))
                                     (* stemp temp)))
                           label110))))
                    label120)))))))
          ((lsame side "R")
           (cond
             ((lsame pivot "V")
              (cond
                ((lsame direct "F")
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j (f2cl-lib:int-add n (f2cl-lib:int-sub 1)))
                                nil)
                   (tagbody
                     (setf ctemp
                             (f2cl-lib:fref c-%data% (j) ((1 *)) c-%offset%))
                     (setf stemp
                             (f2cl-lib:fref s-%data% (j) ((1 *)) s-%offset%))
                     (cond
                       ((or (/= ctemp one) (/= stemp zero))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i m) nil)
                          (tagbody
                            (setf temp
                                    (f2cl-lib:fref a-%data%
                                                   (i (f2cl-lib:int-add j 1))
                                                   ((1 lda) (1 *))
                                                   a-%offset%))
                            (setf (f2cl-lib:fref a-%data%
                                                 (i (f2cl-lib:int-add j 1))
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (- (* ctemp temp)
                                       (* stemp
                                          (f2cl-lib:fref a-%data%
                                                         (i j)
                                                         ((1 lda) (1 *))
                                                         a-%offset%))))
                            (setf (f2cl-lib:fref a-%data%
                                                 (i j)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (+ (* stemp temp)
                                       (* ctemp
                                          (f2cl-lib:fref a-%data%
                                                         (i j)
                                                         ((1 lda) (1 *))
                                                         a-%offset%))))
                           label130))))
                    label140)))
                ((lsame direct "B")
                 (f2cl-lib:fdo (j (f2cl-lib:int-add n (f2cl-lib:int-sub 1))
                                (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                               ((> j 1) nil)
                   (tagbody
                     (setf ctemp
                             (f2cl-lib:fref c-%data% (j) ((1 *)) c-%offset%))
                     (setf stemp
                             (f2cl-lib:fref s-%data% (j) ((1 *)) s-%offset%))
                     (cond
                       ((or (/= ctemp one) (/= stemp zero))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i m) nil)
                          (tagbody
                            (setf temp
                                    (f2cl-lib:fref a-%data%
                                                   (i (f2cl-lib:int-add j 1))
                                                   ((1 lda) (1 *))
                                                   a-%offset%))
                            (setf (f2cl-lib:fref a-%data%
                                                 (i (f2cl-lib:int-add j 1))
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (- (* ctemp temp)
                                       (* stemp
                                          (f2cl-lib:fref a-%data%
                                                         (i j)
                                                         ((1 lda) (1 *))
                                                         a-%offset%))))
                            (setf (f2cl-lib:fref a-%data%
                                                 (i j)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (+ (* stemp temp)
                                       (* ctemp
                                          (f2cl-lib:fref a-%data%
                                                         (i j)
                                                         ((1 lda) (1 *))
                                                         a-%offset%))))
                           label150))))
                    label160)))))
             ((lsame pivot "T")
              (cond
                ((lsame direct "F")
                 (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                               ((> j n) nil)
                   (tagbody
                     (setf ctemp
                             (f2cl-lib:fref c-%data%
                                            ((f2cl-lib:int-sub j 1))
                                            ((1 *))
                                            c-%offset%))
                     (setf stemp
                             (f2cl-lib:fref s-%data%
                                            ((f2cl-lib:int-sub j 1))
                                            ((1 *))
                                            s-%offset%))
                     (cond
                       ((or (/= ctemp one) (/= stemp zero))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i m) nil)
                          (tagbody
                            (setf temp
                                    (f2cl-lib:fref a-%data%
                                                   (i j)
                                                   ((1 lda) (1 *))
                                                   a-%offset%))
                            (setf (f2cl-lib:fref a-%data%
                                                 (i j)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (- (* ctemp temp)
                                       (* stemp
                                          (f2cl-lib:fref a-%data%
                                                         (i 1)
                                                         ((1 lda) (1 *))
                                                         a-%offset%))))
                            (setf (f2cl-lib:fref a-%data%
                                                 (i 1)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (+ (* stemp temp)
                                       (* ctemp
                                          (f2cl-lib:fref a-%data%
                                                         (i 1)
                                                         ((1 lda) (1 *))
                                                         a-%offset%))))
                           label170))))
                    label180)))
                ((lsame direct "B")
                 (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                               ((> j 2) nil)
                   (tagbody
                     (setf ctemp
                             (f2cl-lib:fref c-%data%
                                            ((f2cl-lib:int-sub j 1))
                                            ((1 *))
                                            c-%offset%))
                     (setf stemp
                             (f2cl-lib:fref s-%data%
                                            ((f2cl-lib:int-sub j 1))
                                            ((1 *))
                                            s-%offset%))
                     (cond
                       ((or (/= ctemp one) (/= stemp zero))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i m) nil)
                          (tagbody
                            (setf temp
                                    (f2cl-lib:fref a-%data%
                                                   (i j)
                                                   ((1 lda) (1 *))
                                                   a-%offset%))
                            (setf (f2cl-lib:fref a-%data%
                                                 (i j)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (- (* ctemp temp)
                                       (* stemp
                                          (f2cl-lib:fref a-%data%
                                                         (i 1)
                                                         ((1 lda) (1 *))
                                                         a-%offset%))))
                            (setf (f2cl-lib:fref a-%data%
                                                 (i 1)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (+ (* stemp temp)
                                       (* ctemp
                                          (f2cl-lib:fref a-%data%
                                                         (i 1)
                                                         ((1 lda) (1 *))
                                                         a-%offset%))))
                           label190))))
                    label200)))))
             ((lsame pivot "B")
              (cond
                ((lsame direct "F")
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j (f2cl-lib:int-add n (f2cl-lib:int-sub 1)))
                                nil)
                   (tagbody
                     (setf ctemp
                             (f2cl-lib:fref c-%data% (j) ((1 *)) c-%offset%))
                     (setf stemp
                             (f2cl-lib:fref s-%data% (j) ((1 *)) s-%offset%))
                     (cond
                       ((or (/= ctemp one) (/= stemp zero))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i m) nil)
                          (tagbody
                            (setf temp
                                    (f2cl-lib:fref a-%data%
                                                   (i j)
                                                   ((1 lda) (1 *))
                                                   a-%offset%))
                            (setf (f2cl-lib:fref a-%data%
                                                 (i j)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (+
                                     (* stemp
                                        (f2cl-lib:fref a-%data%
                                                       (i n)
                                                       ((1 lda) (1 *))
                                                       a-%offset%))
                                     (* ctemp temp)))
                            (setf (f2cl-lib:fref a-%data%
                                                 (i n)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (-
                                     (* ctemp
                                        (f2cl-lib:fref a-%data%
                                                       (i n)
                                                       ((1 lda) (1 *))
                                                       a-%offset%))
                                     (* stemp temp)))
                           label210))))
                    label220)))
                ((lsame direct "B")
                 (f2cl-lib:fdo (j (f2cl-lib:int-add n (f2cl-lib:int-sub 1))
                                (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                               ((> j 1) nil)
                   (tagbody
                     (setf ctemp
                             (f2cl-lib:fref c-%data% (j) ((1 *)) c-%offset%))
                     (setf stemp
                             (f2cl-lib:fref s-%data% (j) ((1 *)) s-%offset%))
                     (cond
                       ((or (/= ctemp one) (/= stemp zero))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i m) nil)
                          (tagbody
                            (setf temp
                                    (f2cl-lib:fref a-%data%
                                                   (i j)
                                                   ((1 lda) (1 *))
                                                   a-%offset%))
                            (setf (f2cl-lib:fref a-%data%
                                                 (i j)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (+
                                     (* stemp
                                        (f2cl-lib:fref a-%data%
                                                       (i n)
                                                       ((1 lda) (1 *))
                                                       a-%offset%))
                                     (* ctemp temp)))
                            (setf (f2cl-lib:fref a-%data%
                                                 (i n)
                                                 ((1 lda) (1 *))
                                                 a-%offset%)
                                    (-
                                     (* ctemp
                                        (f2cl-lib:fref a-%data%
                                                       (i n)
                                                       ((1 lda) (1 *))
                                                       a-%offset%))
                                     (* stemp temp)))
                           label230))))
                    label240))))))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasr fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string) (simple-string)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::xerbla fortran-to-lisp::lsame))))

