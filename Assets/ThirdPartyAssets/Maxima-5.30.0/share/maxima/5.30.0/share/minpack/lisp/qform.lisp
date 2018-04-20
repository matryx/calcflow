;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.215 2009/04/07 22:05:21 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.200 2009/01/19 02:38:17 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.112 2009/01/08 12:57:19 rtoy Exp $")

;;; Using Lisp CMU Common Lisp 19f (19F)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :minpack)


(let ((one 1.0) (zero 0.0))
  (declare (type (double-float) one zero))
  (defun qform (m n q ldq wa)
    (declare (type (array double-float (*)) wa q)
             (type (f2cl-lib:integer4) ldq n m))
    (f2cl-lib:with-multi-array-data
        ((q double-float q-%data% q-%offset%)
         (wa double-float wa-%data% wa-%offset%))
      (prog ((sum 0.0) (temp 0.0) (i 0) (j 0) (jm1 0) (k 0) (l 0) (minmn 0)
             (np1 0))
        (declare (type (f2cl-lib:integer4) np1 minmn l k jm1 j i)
                 (type (double-float) temp sum))
        '"     **********"
        '""
        '"     subroutine qform"
        '""
        '"     this subroutine proceeds from the computed qr factorization of"
        '"     an m by n matrix a to accumulate the m by m orthogonal matrix"
        '"     q from its factored form."
        '""
        '"     the subroutine statement is"
        '""
        '"       subroutine qform(m,n,q,ldq,wa)"
        '""
        '"     where"
        '""
        '"       m is a positive integer input variable set to the number"
        '"         of rows of a and the order of q."
        '""
        '"       n is a positive integer input variable set to the number"
        '"         of columns of a."
        '""
        '"       q is an m by m array. on input the full lower trapezoid in"
        '"         the first min(m,n) columns of q contains the factored form."
        '"         on output q has been accumulated into a square matrix."
        '""
        '"       ldq is a positive integer input variable not less than m"
        '"         which specifies the leading dimension of the array q."
        '""
        '"       wa is a work array of length m."
        '""
        '"     subprograms called"
        '""
        '"       fortran-supplied ... min0"
        '""
        '"     argonne national laboratory. minpack project. march 1980."
        '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
        '""
        '"     **********"
        '""
        '"     zero out upper triangle of q in the first min(m,n) columns."
        '""
        (setf minmn (f2cl-lib:min0 m n))
        (if (< minmn 2) (go label30))
        (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                      ((> j minmn) nil)
          (tagbody
            (setf jm1 (f2cl-lib:int-sub j 1))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i jm1) nil)
              (tagbody
                (setf (f2cl-lib:fref q-%data% (i j) ((1 ldq) (1 m)) q-%offset%)
                        zero)
               label10))
           label20))
       label30
        '""
        '"     initialize remaining columns to those of the identity matrix."
        '""
        (setf np1 (f2cl-lib:int-add n 1))
        (if (< m np1) (go label60))
        (f2cl-lib:fdo (j np1 (f2cl-lib:int-add j 1))
                      ((> j m) nil)
          (tagbody
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (setf (f2cl-lib:fref q-%data% (i j) ((1 ldq) (1 m)) q-%offset%)
                        zero)
               label40))
            (setf (f2cl-lib:fref q-%data% (j j) ((1 ldq) (1 m)) q-%offset%)
                    one)
           label50))
       label60
        '""
        '"     accumulate q from its factored form."
        '""
        (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                      ((> l minmn) nil)
          (tagbody
            (setf k (f2cl-lib:int-add (f2cl-lib:int-sub minmn l) 1))
            (f2cl-lib:fdo (i k (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (setf (f2cl-lib:fref wa-%data% (i) ((1 m)) wa-%offset%)
                        (f2cl-lib:fref q-%data%
                                       (i k)
                                       ((1 ldq) (1 m))
                                       q-%offset%))
                (setf (f2cl-lib:fref q-%data% (i k) ((1 ldq) (1 m)) q-%offset%)
                        zero)
               label70))
            (setf (f2cl-lib:fref q-%data% (k k) ((1 ldq) (1 m)) q-%offset%)
                    one)
            (if (= (f2cl-lib:fref wa-%data% (k) ((1 m)) wa-%offset%) zero)
                (go label110))
            (f2cl-lib:fdo (j k (f2cl-lib:int-add j 1))
                          ((> j m) nil)
              (tagbody
                (setf sum zero)
                (f2cl-lib:fdo (i k (f2cl-lib:int-add i 1))
                              ((> i m) nil)
                  (tagbody
                    (setf sum
                            (+ sum
                               (*
                                (f2cl-lib:fref q-%data%
                                               (i j)
                                               ((1 ldq) (1 m))
                                               q-%offset%)
                                (f2cl-lib:fref wa-%data%
                                               (i)
                                               ((1 m))
                                               wa-%offset%))))
                   label80))
                (setf temp
                        (/ sum
                           (f2cl-lib:fref wa-%data% (k) ((1 m)) wa-%offset%)))
                (f2cl-lib:fdo (i k (f2cl-lib:int-add i 1))
                              ((> i m) nil)
                  (tagbody
                    (setf (f2cl-lib:fref q-%data%
                                         (i j)
                                         ((1 ldq) (1 m))
                                         q-%offset%)
                            (-
                             (f2cl-lib:fref q-%data%
                                            (i j)
                                            ((1 ldq) (1 m))
                                            q-%offset%)
                             (* temp
                                (f2cl-lib:fref wa-%data%
                                               (i)
                                               ((1 m))
                                               wa-%offset%))))
                   label90))
               label100))
           label110
           label120))
        (go end_label)
        '""
        '"     last card of subroutine qform."
        '""
       end_label
        (return (values nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::qform fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil)
           :calls 'nil)))

