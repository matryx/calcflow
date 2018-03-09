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


(let ((one 1.0))
  (declare (type (double-float) one))
  (defun r1mpyq (m n a lda v w)
    (declare (type (array double-float (*)) w v a)
             (type (f2cl-lib:integer4) lda n m))
    (f2cl-lib:with-multi-array-data
        ((a double-float a-%data% a-%offset%)
         (v double-float v-%data% v-%offset%)
         (w double-float w-%data% w-%offset%))
      (prog ((cos 0.0) (sin 0.0) (temp 0.0) (i 0) (j 0) (nmj 0) (nm1 0))
        (declare (type (f2cl-lib:integer4) nm1 nmj j i)
                 (type (double-float) temp sin cos))
        '"     **********"
        '""
        '"     subroutine r1mpyq"
        '""
        '"     given an m by n matrix a, this subroutine computes a*q where"
        '"     q is the product of 2*(n - 1) transformations"
        '""
        '"           gv(n-1)*...*gv(1)*gw(1)*...*gw(n-1)"
        '""
        '"     and gv(i), gw(i) are givens rotations in the (i,n) plane which"
        '"     eliminate elements in the i-th and n-th planes, respectively."
        '"     q itself is not given, rather the information to recover the"
        '"     gv, gw rotations is supplied."
        '""
        '"     the subroutine statement is"
        '""
        '"       subroutine r1mpyq(m,n,a,lda,v,w)"
        '""
        '"     where"
        '""
        '"       m is a positive integer input variable set to the number"
        '"         of rows of a."
        '""
        '"       n is a positive integer input variable set to the number"
        '"         of columns of a."
        '""
        '"       a is an m by n array. on input a must contain the matrix"
        '"         to be postmultiplied by the orthogonal matrix q"
        '"         described above. on output a*q has replaced a."
        '""
        '"       lda is a positive integer input variable not less than m"
        '"         which specifies the leading dimension of the array a."
        '""
        '"       v is an input array of length n. v(i) must contain the"
        '"         information necessary to recover the givens rotation gv(i)"
        '"         described above."
        '""
        '"       w is an input array of length n. w(i) must contain the"
        '"         information necessary to recover the givens rotation gw(i)"
        '"         described above."
        '""
        '"     subroutines called"
        '""
        '"       fortran-supplied ... dabs,dsqrt"
        '""
        '"     argonne national laboratory. minpack project. march 1980."
        '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
        '""
        '"     **********"
        '""
        '"     apply the first set of givens rotations to a."
        '""
        (setf nm1 (f2cl-lib:int-sub n 1))
        (if (< nm1 1) (go label50))
        (f2cl-lib:fdo (nmj 1 (f2cl-lib:int-add nmj 1))
                      ((> nmj nm1) nil)
          (tagbody
            (setf j (f2cl-lib:int-sub n nmj))
            (if
             (> (f2cl-lib:dabs (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%))
                one)
             (setf cos
                     (/ one (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%))))
            (if
             (> (f2cl-lib:dabs (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%))
                one)
             (setf sin (f2cl-lib:dsqrt (- one (expt cos 2)))))
            (if
             (<=
              (f2cl-lib:dabs (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%))
              one)
             (setf sin (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%)))
            (if
             (<=
              (f2cl-lib:dabs (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%))
              one)
             (setf cos (f2cl-lib:dsqrt (- one (expt sin 2)))))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (setf temp
                        (-
                         (* cos
                            (f2cl-lib:fref a-%data%
                                           (i j)
                                           ((1 lda) (1 n))
                                           a-%offset%))
                         (* sin
                            (f2cl-lib:fref a-%data%
                                           (i n)
                                           ((1 lda) (1 n))
                                           a-%offset%))))
                (setf (f2cl-lib:fref a-%data% (i n) ((1 lda) (1 n)) a-%offset%)
                        (+
                         (* sin
                            (f2cl-lib:fref a-%data%
                                           (i j)
                                           ((1 lda) (1 n))
                                           a-%offset%))
                         (* cos
                            (f2cl-lib:fref a-%data%
                                           (i n)
                                           ((1 lda) (1 n))
                                           a-%offset%))))
                (setf (f2cl-lib:fref a-%data% (i j) ((1 lda) (1 n)) a-%offset%)
                        temp)
               label10))
           label20))
        '""
        '"     apply the second set of givens rotations to a."
        '""
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j nm1) nil)
          (tagbody
            (if
             (> (f2cl-lib:dabs (f2cl-lib:fref w-%data% (j) ((1 n)) w-%offset%))
                one)
             (setf cos
                     (/ one (f2cl-lib:fref w-%data% (j) ((1 n)) w-%offset%))))
            (if
             (> (f2cl-lib:dabs (f2cl-lib:fref w-%data% (j) ((1 n)) w-%offset%))
                one)
             (setf sin (f2cl-lib:dsqrt (- one (expt cos 2)))))
            (if
             (<=
              (f2cl-lib:dabs (f2cl-lib:fref w-%data% (j) ((1 n)) w-%offset%))
              one)
             (setf sin (f2cl-lib:fref w-%data% (j) ((1 n)) w-%offset%)))
            (if
             (<=
              (f2cl-lib:dabs (f2cl-lib:fref w-%data% (j) ((1 n)) w-%offset%))
              one)
             (setf cos (f2cl-lib:dsqrt (- one (expt sin 2)))))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (setf temp
                        (+
                         (* cos
                            (f2cl-lib:fref a-%data%
                                           (i j)
                                           ((1 lda) (1 n))
                                           a-%offset%))
                         (* sin
                            (f2cl-lib:fref a-%data%
                                           (i n)
                                           ((1 lda) (1 n))
                                           a-%offset%))))
                (setf (f2cl-lib:fref a-%data% (i n) ((1 lda) (1 n)) a-%offset%)
                        (+
                         (* (- sin)
                            (f2cl-lib:fref a-%data%
                                           (i j)
                                           ((1 lda) (1 n))
                                           a-%offset%))
                         (* cos
                            (f2cl-lib:fref a-%data%
                                           (i n)
                                           ((1 lda) (1 n))
                                           a-%offset%))))
                (setf (f2cl-lib:fref a-%data% (i j) ((1 lda) (1 n)) a-%offset%)
                        temp)
               label30))
           label40))
       label50
        (go end_label)
        '""
        '"     last card of subroutine r1mpyq."
        '""
       end_label
        (return (values nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::r1mpyq
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*)))
           :return-values '(nil nil nil nil nil nil)
           :calls 'nil)))

