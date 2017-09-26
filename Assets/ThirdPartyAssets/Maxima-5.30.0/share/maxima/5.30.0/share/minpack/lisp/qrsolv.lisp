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


(let ((p5 0.5) (p25 0.25) (zero 0.0))
  (declare (type (double-float) p5 p25 zero))
  (defun qrsolv (n r ldr ipvt diag qtb x sdiag wa)
    (declare (type (array f2cl-lib:integer4 (*)) ipvt)
             (type (array double-float (*)) wa sdiag x qtb diag r)
             (type (f2cl-lib:integer4) ldr n))
    (f2cl-lib:with-multi-array-data
        ((r double-float r-%data% r-%offset%)
         (diag double-float diag-%data% diag-%offset%)
         (qtb double-float qtb-%data% qtb-%offset%)
         (x double-float x-%data% x-%offset%)
         (sdiag double-float sdiag-%data% sdiag-%offset%)
         (wa double-float wa-%data% wa-%offset%)
         (ipvt f2cl-lib:integer4 ipvt-%data% ipvt-%offset%))
      (prog ((cos 0.0) (cotan 0.0) (qtbpj 0.0) (sin 0.0) (sum 0.0) (tan 0.0)
             (temp 0.0) (i 0) (j 0) (jp1 0) (k 0) (kp1 0) (l 0) (nsing 0))
        (declare (type (f2cl-lib:integer4) nsing l kp1 k jp1 j i)
                 (type (double-float) temp tan sum sin qtbpj cotan cos))
        '"     **********"
        '""
        '"     subroutine qrsolv"
        '""
        '"     given an m by n matrix a, an n by n diagonal matrix d,"
        '"     and an m-vector b, the problem is to determine an x which"
        '"     solves the system"
        '""
        '"           a*x = b ,     d*x = 0 ,"
        '""
        '"     in the least squares sense."
        '""
        '"     this subroutine completes the solution of the problem"
        '"     if it is provided with the necessary information from the"
        '"     qr factorization, with column pivoting, of a. that is, if"
        '"     a*p = q*r, where p is a permutation matrix, q has orthogonal"
        '"     columns, and r is an upper triangular matrix with diagonal"
        '"     elements of nonincreasing magnitude, then qrsolv expects"
        '"     the full upper triangle of r, the permutation matrix p,"
        '"     and the first n components of (q transpose)*b. the system"
        '"     a*x = b, d*x = 0, is then equivalent to"
        '""
        '"                  t       t"
        '"           r*z = q *b ,  p *d*p*z = 0 ,"
        '""
        '"     where x = p*z. if this system does not have full rank,"
        '"     then a least squares solution is obtained. on output qrsolv"
        '"     also provides an upper triangular matrix s such that"
        '""
        '"            t   t               t"
        '"           p *(a *a + d*d)*p = s *s ."
        '""
        '"     s is computed within qrsolv and may be of separate interest."
        '""
        '"     the subroutine statement is"
        '""
        '"       subroutine qrsolv(n,r,ldr,ipvt,diag,qtb,x,sdiag,wa)"
        '""
        '"     where"
        '""
        '"       n is a positive integer input variable set to the order of r."
        '""
        '"       r is an n by n array. on input the full upper triangle"
        '"         must contain the full upper triangle of the matrix r."
        '"         on output the full upper triangle is unaltered, and the"
        '"         strict lower triangle contains the strict upper triangle"
        '"         (transposed) of the upper triangular matrix s."
        '""
        '"       ldr is a positive integer input variable not less than n"
        '"         which specifies the leading dimension of the array r."
        '""
        '"       ipvt is an integer input array of length n which defines the"
        '"         permutation matrix p such that a*p = q*r. column j of p"
        '"         is column ipvt(j) of the identity matrix."
        '""
        '"       diag is an input array of length n which must contain the"
        '"         diagonal elements of the matrix d."
        '""
        '"       qtb is an input array of length n which must contain the first"
        '"         n elements of the vector (q transpose)*b."
        '""
        '"       x is an output array of length n which contains the least"
        '"         squares solution of the system a*x = b, d*x = 0."
        '""
        '"       sdiag is an output array of length n which contains the"
        '"         diagonal elements of the upper triangular matrix s."
        '""
        '"       wa is a work array of length n."
        '""
        '"     subprograms called"
        '""
        '"       fortran-supplied ... dabs,dsqrt"
        '""
        '"     argonne national laboratory. minpack project. march 1980."
        '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
        '""
        '"     **********"
        '""
        '"     copy r and (q transpose)*b to preserve input and initialize s."
        '"     in particular, save the diagonal elements of r in x."
        '""
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf (f2cl-lib:fref r-%data% (i j) ((1 ldr) (1 n)) r-%offset%)
                        (f2cl-lib:fref r-%data%
                                       (j i)
                                       ((1 ldr) (1 n))
                                       r-%offset%))
               label10))
            (setf (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                    (f2cl-lib:fref r-%data% (j j) ((1 ldr) (1 n)) r-%offset%))
            (setf (f2cl-lib:fref wa-%data% (j) ((1 n)) wa-%offset%)
                    (f2cl-lib:fref qtb-%data% (j) ((1 n)) qtb-%offset%))
           label20))
        '""
        '"     eliminate the diagonal matrix d using a givens rotation."
        '""
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            '""
            '"        prepare the row of d to be eliminated, locating the"
            '"        diagonal element using p from the qr factorization."
            '""
            (setf l (f2cl-lib:fref ipvt-%data% (j) ((1 n)) ipvt-%offset%))
            (if (= (f2cl-lib:fref diag-%data% (l) ((1 n)) diag-%offset%) zero)
                (go label90))
            (f2cl-lib:fdo (k j (f2cl-lib:int-add k 1))
                          ((> k n) nil)
              (tagbody
                (setf (f2cl-lib:fref sdiag-%data% (k) ((1 n)) sdiag-%offset%)
                        zero)
               label30))
            (setf (f2cl-lib:fref sdiag-%data% (j) ((1 n)) sdiag-%offset%)
                    (f2cl-lib:fref diag-%data% (l) ((1 n)) diag-%offset%))
            '""
            '"        the transformations to eliminate the row of d"
            '"        modify only a single element of (q transpose)*b"
            '"        beyond the first n, which is initially zero."
            '""
            (setf qtbpj zero)
            (f2cl-lib:fdo (k j (f2cl-lib:int-add k 1))
                          ((> k n) nil)
              (tagbody
                '""
                '"           determine a givens rotation which eliminates the"
                '"           appropriate element in the current row of d."
                '""
                (if
                 (= (f2cl-lib:fref sdiag-%data% (k) ((1 n)) sdiag-%offset%)
                    zero)
                 (go label70))
                (if
                 (>=
                  (f2cl-lib:dabs
                   (f2cl-lib:fref r-%data% (k k) ((1 ldr) (1 n)) r-%offset%))
                  (f2cl-lib:dabs
                   (f2cl-lib:fref sdiag-%data% (k) ((1 n)) sdiag-%offset%)))
                 (go label40))
                (setf cotan
                        (/
                         (f2cl-lib:fref r-%data%
                                        (k k)
                                        ((1 ldr) (1 n))
                                        r-%offset%)
                         (f2cl-lib:fref sdiag-%data%
                                        (k)
                                        ((1 n))
                                        sdiag-%offset%)))
                (setf sin
                        (/ p5 (f2cl-lib:dsqrt (+ p25 (* p25 (expt cotan 2))))))
                (setf cos (* sin cotan))
                (go label50)
               label40
                (setf tan
                        (/
                         (f2cl-lib:fref sdiag-%data%
                                        (k)
                                        ((1 n))
                                        sdiag-%offset%)
                         (f2cl-lib:fref r-%data%
                                        (k k)
                                        ((1 ldr) (1 n))
                                        r-%offset%)))
                (setf cos (/ p5 (f2cl-lib:dsqrt (+ p25 (* p25 (expt tan 2))))))
                (setf sin (* cos tan))
               label50
                '""
                '"           compute the modified diagonal element of r and"
                '"           the modified element of ((q transpose)*b,0)."
                '""
                (setf (f2cl-lib:fref r-%data% (k k) ((1 ldr) (1 n)) r-%offset%)
                        (+
                         (* cos
                            (f2cl-lib:fref r-%data%
                                           (k k)
                                           ((1 ldr) (1 n))
                                           r-%offset%))
                         (* sin
                            (f2cl-lib:fref sdiag-%data%
                                           (k)
                                           ((1 n))
                                           sdiag-%offset%))))
                (setf temp
                        (+
                         (* cos
                            (f2cl-lib:fref wa-%data% (k) ((1 n)) wa-%offset%))
                         (* sin qtbpj)))
                (setf qtbpj
                        (+
                         (* (- sin)
                            (f2cl-lib:fref wa-%data% (k) ((1 n)) wa-%offset%))
                         (* cos qtbpj)))
                (setf (f2cl-lib:fref wa-%data% (k) ((1 n)) wa-%offset%) temp)
                '""
                '"           accumulate the tranformation in the row of s."
                '""
                (setf kp1 (f2cl-lib:int-add k 1))
                (if (< n kp1) (go label70))
                (f2cl-lib:fdo (i kp1 (f2cl-lib:int-add i 1))
                              ((> i n) nil)
                  (tagbody
                    (setf temp
                            (+
                             (* cos
                                (f2cl-lib:fref r-%data%
                                               (i k)
                                               ((1 ldr) (1 n))
                                               r-%offset%))
                             (* sin
                                (f2cl-lib:fref sdiag-%data%
                                               (i)
                                               ((1 n))
                                               sdiag-%offset%))))
                    (setf (f2cl-lib:fref sdiag-%data%
                                         (i)
                                         ((1 n))
                                         sdiag-%offset%)
                            (+
                             (* (- sin)
                                (f2cl-lib:fref r-%data%
                                               (i k)
                                               ((1 ldr) (1 n))
                                               r-%offset%))
                             (* cos
                                (f2cl-lib:fref sdiag-%data%
                                               (i)
                                               ((1 n))
                                               sdiag-%offset%))))
                    (setf (f2cl-lib:fref r-%data%
                                         (i k)
                                         ((1 ldr) (1 n))
                                         r-%offset%)
                            temp)
                   label60))
               label70
               label80))
           label90
            '""
            '"        store the diagonal element of s and restore"
            '"        the corresponding diagonal element of r."
            '""
            (setf (f2cl-lib:fref sdiag-%data% (j) ((1 n)) sdiag-%offset%)
                    (f2cl-lib:fref r-%data% (j j) ((1 ldr) (1 n)) r-%offset%))
            (setf (f2cl-lib:fref r-%data% (j j) ((1 ldr) (1 n)) r-%offset%)
                    (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%))
           label100))
        '""
        '"     solve the triangular system for z. if the system is"
        '"     singular, then obtain a least squares solution."
        '""
        (setf nsing n)
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (if
             (and
              (= (f2cl-lib:fref sdiag-%data% (j) ((1 n)) sdiag-%offset%) zero)
              (= nsing n))
             (setf nsing (f2cl-lib:int-sub j 1)))
            (if (< nsing n)
                (setf (f2cl-lib:fref wa-%data% (j) ((1 n)) wa-%offset%) zero))
           label110))
        (if (< nsing 1) (go label150))
        (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                      ((> k nsing) nil)
          (tagbody
            (setf j (f2cl-lib:int-add (f2cl-lib:int-sub nsing k) 1))
            (setf sum zero)
            (setf jp1 (f2cl-lib:int-add j 1))
            (if (< nsing jp1) (go label130))
            (f2cl-lib:fdo (i jp1 (f2cl-lib:int-add i 1))
                          ((> i nsing) nil)
              (tagbody
                (setf sum
                        (+ sum
                           (*
                            (f2cl-lib:fref r-%data%
                                           (i j)
                                           ((1 ldr) (1 n))
                                           r-%offset%)
                            (f2cl-lib:fref wa-%data%
                                           (i)
                                           ((1 n))
                                           wa-%offset%))))
               label120))
           label130
            (setf (f2cl-lib:fref wa-%data% (j) ((1 n)) wa-%offset%)
                    (/
                     (- (f2cl-lib:fref wa-%data% (j) ((1 n)) wa-%offset%) sum)
                     (f2cl-lib:fref sdiag-%data% (j) ((1 n)) sdiag-%offset%)))
           label140))
       label150
        '""
        '"     permute the components of z back to components of x."
        '""
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf l (f2cl-lib:fref ipvt-%data% (j) ((1 n)) ipvt-%offset%))
            (setf (f2cl-lib:fref x-%data% (l) ((1 n)) x-%offset%)
                    (f2cl-lib:fref wa-%data% (j) ((1 n)) wa-%offset%))
           label160))
        (go end_label)
        '""
        '"     last card of subroutine qrsolv."
        '""
       end_label
        (return (values nil nil nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::qrsolv
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil)
           :calls 'nil)))

