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


(let ((cnsts1
       (make-array 28
                   :element-type 'double-float
                   :initial-contents '(0.25 0.0625 0.072169 0.018342 0.019065
                                       0.05819 0.0054658 0.005337 0.01889
                                       0.027792 0.0016095 0.0014964 0.0075938
                                       0.0057573 0.018342 0.004673 4.15e-4
                                       0.001919 0.001468 0.006371 0.00461
                                       1.342e-4 1.138e-4 4.889e-4 4.177e-4
                                       0.001374 0.001654 0.002863)))
      (cnsts2
       (make-array 28
                   :element-type 'double-float
                   :initial-contents '(0.125 0.002604 0.008019 2.17e-5 7.453e-5
                                       5.208e-4 9.689e-8 3.689e-7 3.1e-6
                                       2.451e-5 2.691e-10 1.12e-9 1.076e-8
                                       9.405e-8 1.033e-6 5.097e-13 2.29e-12
                                       2.446e-11 2.331e-10 2.936e-9 3.593e-8
                                       7.001e-16 3.363e-15 3.921e-14 4.028e-13
                                       5.646e-12 7.531e-11 1.129e-9))))
  (declare (type (array double-float (28)) cnsts1 cnsts2))
  (defun consts (k rho coef)
    (declare (type (array double-float (*)) coef)
             (type (array double-float (*)) rho)
             (type (f2cl-lib:integer4) k))
    (let ((colord-m
           (make-array 20
                       :element-type 'f2cl-lib:integer4
                       :displaced-to (colord-part-0 *colord-common-block*)
                       :displaced-index-offset 5))
          (colbas-b
           (make-array 28
                       :element-type 'double-float
                       :displaced-to (colbas-part-0 *colbas-common-block*)
                       :displaced-index-offset 0))
          (colbas-acol
           (make-array 196
                       :element-type 'double-float
                       :displaced-to (colbas-part-0 *colbas-common-block*)
                       :displaced-index-offset 28))
          (colbas-asave
           (make-array 112
                       :element-type 'double-float
                       :displaced-to (colbas-part-0 *colbas-common-block*)
                       :displaced-index-offset 224))
          (colest-wgtmsh
           (make-array 40
                       :element-type 'double-float
                       :displaced-to (colest-part-0 *colest-common-block*)
                       :displaced-index-offset 40))
          (colest-wgterr
           (make-array 40
                       :element-type 'double-float
                       :displaced-to (colest-part-0 *colest-common-block*)
                       :displaced-index-offset 80))
          (colest-tolin
           (make-array 40
                       :element-type 'double-float
                       :displaced-to (colest-part-0 *colest-common-block*)
                       :displaced-index-offset 120))
          (colest-root
           (make-array 40
                       :element-type 'double-float
                       :displaced-to (colest-part-0 *colest-common-block*)
                       :displaced-index-offset 160))
          (colest-jtol
           (make-array 40
                       :element-type 'f2cl-lib:integer4
                       :displaced-to (colest-part-1 *colest-common-block*)
                       :displaced-index-offset 0))
          (colest-ltol
           (make-array 40
                       :element-type 'f2cl-lib:integer4
                       :displaced-to (colest-part-1 *colest-common-block*)
                       :displaced-index-offset 40)))
      (symbol-macrolet ((ncomp (aref (colord-part-0 *colord-common-block*) 1))
                        (mmax (aref (colord-part-0 *colord-common-block*) 4))
                        (m colord-m)
                        (b colbas-b)
                        (acol colbas-acol)
                        (asave colbas-asave)
                        (wgtmsh colest-wgtmsh)
                        (wgterr colest-wgterr)
                        (tolin colest-tolin)
                        (root colest-root)
                        (jtol colest-jtol)
                        (ltol colest-ltol)
                        (ntol (aref (colest-part-1 *colest-common-block*) 80)))
        (f2cl-lib:with-multi-array-data
            ((rho double-float rho-%data% rho-%offset%)
             (coef double-float coef-%data% coef-%offset%))
          (prog ((ltoli 0) (i 0) (mtot 0) (jcomp 0) (l 0) (mj 0) (j 0) (iz 0)
                 (koff 0) (dummy (make-array 1 :element-type 'double-float)))
            (declare (type (array double-float (1)) dummy)
                     (type (f2cl-lib:integer4) koff iz j mj l jcomp mtot i
                                               ltoli))
            (setf koff (the f2cl-lib:integer4 (truncate (* k (+ k 1)) 2)))
            (setf iz 1)
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j ncomp) nil)
              (tagbody
                (setf mj (f2cl-lib:fref m (j) ((1 20))))
                (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                              ((> l mj) nil)
                  (tagbody
                    (setf (f2cl-lib:fref wgterr (iz) ((1 40)))
                            (f2cl-lib:fref cnsts1
                                           ((f2cl-lib:int-add
                                             (f2cl-lib:int-sub koff mj)
                                             l))
                                           ((1 28))))
                    (setf iz (f2cl-lib:int-add iz 1))
                   label10))))
           label10
            (setf jcomp 1)
            (setf mtot (f2cl-lib:fref m (1) ((1 20))))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i ntol) nil)
              (tagbody
                (setf ltoli (f2cl-lib:fref ltol (i) ((1 40))))
               label20
                (if (<= ltoli mtot) (go label30))
                (setf jcomp (f2cl-lib:int-add jcomp 1))
                (setf mtot
                        (f2cl-lib:int-add mtot
                                          (f2cl-lib:fref m (jcomp) ((1 20)))))
                (go label20)
               label30
                (setf (f2cl-lib:fref jtol (i) ((1 40))) jcomp)
                (setf (f2cl-lib:fref wgtmsh (i) ((1 40)))
                        (/
                         (* 10.0
                            (f2cl-lib:fref cnsts2
                                           ((f2cl-lib:int-sub
                                             (f2cl-lib:int-add koff ltoli)
                                             mtot))
                                           ((1 28))))
                         (f2cl-lib:fref tolin (i) ((1 40)))))
                (setf (f2cl-lib:fref root (i) ((1 40)))
                        (/ 1.0
                           (f2cl-lib:dfloat
                            (f2cl-lib:int-add
                             (f2cl-lib:int-sub (f2cl-lib:int-add k mtot) ltoli)
                             1))))
               label40))
            (f2cl-lib:computed-goto
             (label50 label60 label70 label80 label90 label100 label110)
             k)
           label50
            (setf (f2cl-lib:fref rho-%data% (1) ((1 7)) rho-%offset%) 0.0)
            (go label120)
           label60
            (setf (f2cl-lib:fref rho-%data% (2) ((1 7)) rho-%offset%)
                    0.5773502691896257)
            (setf (f2cl-lib:fref rho-%data% (1) ((1 7)) rho-%offset%)
                    (- (f2cl-lib:fref rho-%data% (2) ((1 7)) rho-%offset%)))
            (go label120)
           label70
            (setf (f2cl-lib:fref rho-%data% (3) ((1 7)) rho-%offset%)
                    0.7745966692414834)
            (setf (f2cl-lib:fref rho-%data% (2) ((1 7)) rho-%offset%) 0.0)
            (setf (f2cl-lib:fref rho-%data% (1) ((1 7)) rho-%offset%)
                    (- (f2cl-lib:fref rho-%data% (3) ((1 7)) rho-%offset%)))
            (go label120)
           label80
            (setf (f2cl-lib:fref rho-%data% (4) ((1 7)) rho-%offset%)
                    0.8611363115940526)
            (setf (f2cl-lib:fref rho-%data% (3) ((1 7)) rho-%offset%)
                    0.33998104358485626)
            (setf (f2cl-lib:fref rho-%data% (2) ((1 7)) rho-%offset%)
                    (- (f2cl-lib:fref rho-%data% (3) ((1 7)) rho-%offset%)))
            (setf (f2cl-lib:fref rho-%data% (1) ((1 7)) rho-%offset%)
                    (- (f2cl-lib:fref rho-%data% (4) ((1 7)) rho-%offset%)))
            (go label120)
           label90
            (setf (f2cl-lib:fref rho-%data% (5) ((1 7)) rho-%offset%)
                    0.906179845938664)
            (setf (f2cl-lib:fref rho-%data% (4) ((1 7)) rho-%offset%)
                    0.5384693101056831)
            (setf (f2cl-lib:fref rho-%data% (3) ((1 7)) rho-%offset%) 0.0)
            (setf (f2cl-lib:fref rho-%data% (2) ((1 7)) rho-%offset%)
                    (- (f2cl-lib:fref rho-%data% (4) ((1 7)) rho-%offset%)))
            (setf (f2cl-lib:fref rho-%data% (1) ((1 7)) rho-%offset%)
                    (- (f2cl-lib:fref rho-%data% (5) ((1 7)) rho-%offset%)))
            (go label120)
           label100
            (setf (f2cl-lib:fref rho-%data% (6) ((1 7)) rho-%offset%)
                    0.932469514203152)
            (setf (f2cl-lib:fref rho-%data% (5) ((1 7)) rho-%offset%)
                    0.6612093864662645)
            (setf (f2cl-lib:fref rho-%data% (4) ((1 7)) rho-%offset%)
                    0.2386191860831969)
            (setf (f2cl-lib:fref rho-%data% (3) ((1 7)) rho-%offset%)
                    (- (f2cl-lib:fref rho-%data% (4) ((1 7)) rho-%offset%)))
            (setf (f2cl-lib:fref rho-%data% (2) ((1 7)) rho-%offset%)
                    (- (f2cl-lib:fref rho-%data% (5) ((1 7)) rho-%offset%)))
            (setf (f2cl-lib:fref rho-%data% (1) ((1 7)) rho-%offset%)
                    (- (f2cl-lib:fref rho-%data% (6) ((1 7)) rho-%offset%)))
            (go label120)
           label110
            (setf (f2cl-lib:fref rho-%data% (7) ((1 7)) rho-%offset%)
                    0.9491079912342758)
            (setf (f2cl-lib:fref rho-%data% (6) ((1 7)) rho-%offset%)
                    0.7415311855993945)
            (setf (f2cl-lib:fref rho-%data% (5) ((1 7)) rho-%offset%)
                    0.4058451513773972)
            (setf (f2cl-lib:fref rho-%data% (4) ((1 7)) rho-%offset%) 0.0)
            (setf (f2cl-lib:fref rho-%data% (3) ((1 7)) rho-%offset%)
                    (- (f2cl-lib:fref rho-%data% (5) ((1 7)) rho-%offset%)))
            (setf (f2cl-lib:fref rho-%data% (2) ((1 7)) rho-%offset%)
                    (- (f2cl-lib:fref rho-%data% (6) ((1 7)) rho-%offset%)))
            (setf (f2cl-lib:fref rho-%data% (1) ((1 7)) rho-%offset%)
                    (- (f2cl-lib:fref rho-%data% (7) ((1 7)) rho-%offset%)))
           label120
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j k) nil)
              (tagbody
                (setf (f2cl-lib:fref rho-%data% (j) ((1 7)) rho-%offset%)
                        (* 0.5
                           (+ 1.0
                              (f2cl-lib:fref rho-%data%
                                             (j)
                                             ((1 7))
                                             rho-%offset%))))
               label130))
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j k) nil)
              (tagbody
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i k) nil)
                  (tagbody
                   label135
                    (setf (f2cl-lib:fref coef-%data%
                                         (i j)
                                         ((1 k) (1 1))
                                         coef-%offset%)
                            0.0)))
                (setf (f2cl-lib:fref coef-%data%
                                     (j j)
                                     ((1 k) (1 1))
                                     coef-%offset%)
                        1.0)
                (vmonde rho
                 (f2cl-lib:array-slice coef double-float (1 j) ((1 k) (1 1)))
                 k)
               label140))
            (rkbas 1.0 coef k mmax b dummy 0)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i k) nil)
              (tagbody
                (rkbas (f2cl-lib:fref rho-%data% (i) ((1 7)) rho-%offset%) coef
                 k mmax
                 (f2cl-lib:array-slice acol double-float (1 i) ((1 28) (1 7)))
                 dummy 0)
               label150))
            (rkbas (/ 1.0 6.0) coef k mmax
             (f2cl-lib:array-slice asave double-float (1 1) ((1 28) (1 4)))
             dummy 0)
            (rkbas (/ 1.0 3.0) coef k mmax
             (f2cl-lib:array-slice asave double-float (1 2) ((1 28) (1 4)))
             dummy 0)
            (rkbas (/ 2.0 3.0) coef k mmax
             (f2cl-lib:array-slice asave double-float (1 3) ((1 28) (1 4)))
             dummy 0)
            (rkbas (/ 5.0 6.0) coef k mmax
             (f2cl-lib:array-slice asave double-float (1 4) ((1 28) (1 4)))
             dummy 0)
            (go end_label)
           end_label
            (return (values nil nil nil))))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::consts
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (7))
                        (array double-float (*)))
           :return-values '(nil nil nil)
           :calls '(fortran-to-lisp::rkbas fortran-to-lisp::vmonde))))

