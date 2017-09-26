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


(let ((one 1.0) (p5 0.5) (p25 0.25) (zero 0.0))
  (declare (type (double-float) one p5 p25 zero))
  (defun r1updt (m n s ls u v w sing)
    (declare (type f2cl-lib:logical sing)
             (type (array double-float (*)) w v u s)
             (type (f2cl-lib:integer4) ls n m))
    (f2cl-lib:with-multi-array-data
        ((s double-float s-%data% s-%offset%)
         (u double-float u-%data% u-%offset%)
         (v double-float v-%data% v-%offset%)
         (w double-float w-%data% w-%offset%))
      (prog ((cos 0.0) (cotan 0.0) (giant 0.0) (sin 0.0) (tan 0.0) (tau 0.0)
             (temp 0.0) (i 0) (j 0) (jj 0) (l 0) (nmj 0) (nm1 0))
        (declare (type (f2cl-lib:integer4) nm1 nmj l jj j i)
                 (type (double-float) temp tau tan sin giant cotan cos))
        '"     **********"
        '""
        '"     subroutine r1updt"
        '""
        '"     given an m by n lower trapezoidal matrix s, an m-vector u,"
        '"     and an n-vector v, the problem is to determine an"
        '"     orthogonal matrix q such that"
        '""
        '"                   t"
        '"           (s + u*v )*q"
        '""
        '"     is again lower trapezoidal."
        '""
        '"     this subroutine determines q as the product of 2*(n - 1)"
        '"     transformations"
        '""
        '"           gv(n-1)*...*gv(1)*gw(1)*...*gw(n-1)"
        '""
        '"     where gv(i), gw(i) are givens rotations in the (i,n) plane"
        '"     which eliminate elements in the i-th and n-th planes,"
        '"     respectively. q itself is not accumulated, rather the"
        '"     information to recover the gv, gw rotations is returned."
        '""
        '"     the subroutine statement is"
        '""
        '"       subroutine r1updt(m,n,s,ls,u,v,w,sing)"
        '""
        '"     where"
        '""
        '"       m is a positive integer input variable set to the number"
        '"         of rows of s."
        '""
        '"       n is a positive integer input variable set to the number"
        '"         of columns of s. n must not exceed m."
        '""
        '"       s is an array of length ls. on input s must contain the lower"
        '"         trapezoidal matrix s stored by columns. on output s contains"
        '"         the lower trapezoidal matrix produced as described above."
        '""
        '"       ls is a positive integer input variable not less than"
        '"         (n*(2*m-n+1))/2."
        '""
        '"       u is an input array of length m which must contain the"
        '"         vector u."
        '""
        '"       v is an array of length n. on input v must contain the vector"
        '"         v. on output v(i) contains the information necessary to"
        '"         recover the givens rotation gv(i) described above."
        '""
        '"       w is an output array of length m. w(i) contains information"
        '"         necessary to recover the givens rotation gw(i) described"
        '"         above."
        '""
        '"       sing is a logical output variable. sing is set true if any"
        '"         of the diagonal elements of the output s are zero. otherwise"
        '"         sing is set false."
        '""
        '"     subprograms called"
        '""
        '"       minpack-supplied ... dpmpar"
        '""
        '"       fortran-supplied ... dabs,dsqrt"
        '""
        '"     argonne national laboratory. minpack project. march 1980."
        '"     burton s. garbow, kenneth e. hillstrom, jorge j. more,"
        '"     john l. nazareth"
        '""
        '"     **********"
        '""
        '"     giant is the largest magnitude."
        '""
        (setf giant (dpmpar 3))
        '""
        '"     initialize the diagonal element pointer."
        '""
        (setf jj
                (-
                 (the f2cl-lib:integer4 (truncate (* n (+ (- (* 2 m) n) 1)) 2))
                 (f2cl-lib:int-sub m n)))
        '""
        '"     move the nontrivial part of the last column of s into w."
        '""
        (setf l jj)
        (f2cl-lib:fdo (i n (f2cl-lib:int-add i 1))
                      ((> i m) nil)
          (tagbody
            (setf (f2cl-lib:fref w-%data% (i) ((1 m)) w-%offset%)
                    (f2cl-lib:fref s-%data% (l) ((1 ls)) s-%offset%))
            (setf l (f2cl-lib:int-add l 1))
           label10))
        '""
        '"     rotate the vector v into a multiple of the n-th unit vector"
        '"     in such a way that a spike is introduced into w."
        '""
        (setf nm1 (f2cl-lib:int-sub n 1))
        (if (< nm1 1) (go label70))
        (f2cl-lib:fdo (nmj 1 (f2cl-lib:int-add nmj 1))
                      ((> nmj nm1) nil)
          (tagbody
            (setf j (f2cl-lib:int-sub n nmj))
            (setf jj
                    (f2cl-lib:int-sub jj
                                      (f2cl-lib:int-add (f2cl-lib:int-sub m j)
                                                        1)))
            (setf (f2cl-lib:fref w-%data% (j) ((1 m)) w-%offset%) zero)
            (if (= (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%) zero)
                (go label50))
            '""
            '"        determine a givens rotation which eliminates the"
            '"        j-th element of v."
            '""
            (if
             (>=
              (f2cl-lib:dabs (f2cl-lib:fref v-%data% (n) ((1 n)) v-%offset%))
              (f2cl-lib:dabs (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%)))
             (go label20))
            (setf cotan
                    (/ (f2cl-lib:fref v-%data% (n) ((1 n)) v-%offset%)
                       (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%)))
            (setf sin (/ p5 (f2cl-lib:dsqrt (+ p25 (* p25 (expt cotan 2))))))
            (setf cos (* sin cotan))
            (setf tau one)
            (if (> (* (f2cl-lib:dabs cos) giant) one) (setf tau (/ one cos)))
            (go label30)
           label20
            (setf tan
                    (/ (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%)
                       (f2cl-lib:fref v-%data% (n) ((1 n)) v-%offset%)))
            (setf cos (/ p5 (f2cl-lib:dsqrt (+ p25 (* p25 (expt tan 2))))))
            (setf sin (* cos tan))
            (setf tau sin)
           label30
            '""
            '"        apply the transformation to v and store the information"
            '"        necessary to recover the givens rotation."
            '""
            (setf (f2cl-lib:fref v-%data% (n) ((1 n)) v-%offset%)
                    (+ (* sin (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%))
                       (* cos
                          (f2cl-lib:fref v-%data% (n) ((1 n)) v-%offset%))))
            (setf (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%) tau)
            '""
            '"        apply the transformation to s and extend the spike in w."
            '""
            (setf l jj)
            (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (setf temp
                        (-
                         (* cos
                            (f2cl-lib:fref s-%data% (l) ((1 ls)) s-%offset%))
                         (* sin
                            (f2cl-lib:fref w-%data% (i) ((1 m)) w-%offset%))))
                (setf (f2cl-lib:fref w-%data% (i) ((1 m)) w-%offset%)
                        (+
                         (* sin
                            (f2cl-lib:fref s-%data% (l) ((1 ls)) s-%offset%))
                         (* cos
                            (f2cl-lib:fref w-%data% (i) ((1 m)) w-%offset%))))
                (setf (f2cl-lib:fref s-%data% (l) ((1 ls)) s-%offset%) temp)
                (setf l (f2cl-lib:int-add l 1))
               label40))
           label50
           label60))
       label70
        '""
        '"     add the spike from the rank 1 update to w."
        '""
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i m) nil)
          (tagbody
            (setf (f2cl-lib:fref w-%data% (i) ((1 m)) w-%offset%)
                    (+ (f2cl-lib:fref w-%data% (i) ((1 m)) w-%offset%)
                       (* (f2cl-lib:fref v-%data% (n) ((1 n)) v-%offset%)
                          (f2cl-lib:fref u-%data% (i) ((1 m)) u-%offset%))))
           label80))
        '""
        '"     eliminate the spike."
        '""
        (setf sing f2cl-lib:%false%)
        (if (< nm1 1) (go label140))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j nm1) nil)
          (tagbody
            (if (= (f2cl-lib:fref w-%data% (j) ((1 m)) w-%offset%) zero)
                (go label120))
            '""
            '"        determine a givens rotation which eliminates the"
            '"        j-th element of the spike."
            '""
            (if
             (>=
              (f2cl-lib:dabs (f2cl-lib:fref s-%data% (jj) ((1 ls)) s-%offset%))
              (f2cl-lib:dabs (f2cl-lib:fref w-%data% (j) ((1 m)) w-%offset%)))
             (go label90))
            (setf cotan
                    (/ (f2cl-lib:fref s-%data% (jj) ((1 ls)) s-%offset%)
                       (f2cl-lib:fref w-%data% (j) ((1 m)) w-%offset%)))
            (setf sin (/ p5 (f2cl-lib:dsqrt (+ p25 (* p25 (expt cotan 2))))))
            (setf cos (* sin cotan))
            (setf tau one)
            (if (> (* (f2cl-lib:dabs cos) giant) one) (setf tau (/ one cos)))
            (go label100)
           label90
            (setf tan
                    (/ (f2cl-lib:fref w-%data% (j) ((1 m)) w-%offset%)
                       (f2cl-lib:fref s-%data% (jj) ((1 ls)) s-%offset%)))
            (setf cos (/ p5 (f2cl-lib:dsqrt (+ p25 (* p25 (expt tan 2))))))
            (setf sin (* cos tan))
            (setf tau sin)
           label100
            '""
            '"        apply the transformation to s and reduce the spike in w."
            '""
            (setf l jj)
            (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (setf temp
                        (+
                         (* cos
                            (f2cl-lib:fref s-%data% (l) ((1 ls)) s-%offset%))
                         (* sin
                            (f2cl-lib:fref w-%data% (i) ((1 m)) w-%offset%))))
                (setf (f2cl-lib:fref w-%data% (i) ((1 m)) w-%offset%)
                        (+
                         (* (- sin)
                            (f2cl-lib:fref s-%data% (l) ((1 ls)) s-%offset%))
                         (* cos
                            (f2cl-lib:fref w-%data% (i) ((1 m)) w-%offset%))))
                (setf (f2cl-lib:fref s-%data% (l) ((1 ls)) s-%offset%) temp)
                (setf l (f2cl-lib:int-add l 1))
               label110))
            '""
            '"        store the information necessary to recover the"
            '"        givens rotation."
            '""
            (setf (f2cl-lib:fref w-%data% (j) ((1 m)) w-%offset%) tau)
           label120
            '""
            '"        test for zero diagonal elements in the output s."
            '""
            (if (= (f2cl-lib:fref s-%data% (jj) ((1 ls)) s-%offset%) zero)
                (setf sing f2cl-lib:%true%))
            (setf jj
                    (f2cl-lib:int-add jj
                                      (f2cl-lib:int-add (f2cl-lib:int-sub m j)
                                                        1)))
           label130))
       label140
        '""
        '"     move w back into the last column of the output s."
        '""
        (setf l jj)
        (f2cl-lib:fdo (i n (f2cl-lib:int-add i 1))
                      ((> i m) nil)
          (tagbody
            (setf (f2cl-lib:fref s-%data% (l) ((1 ls)) s-%offset%)
                    (f2cl-lib:fref w-%data% (i) ((1 m)) w-%offset%))
            (setf l (f2cl-lib:int-add l 1))
           label150))
        (if (= (f2cl-lib:fref s-%data% (jj) ((1 ls)) s-%offset%) zero)
            (setf sing f2cl-lib:%true%))
        (go end_label)
        '""
        '"     last card of subroutine r1updt."
        '""
       end_label
        (return (values nil nil nil nil nil nil nil sing))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::r1updt
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) fortran-to-lisp::logical)
           :return-values '(nil nil nil nil nil nil nil fortran-to-lisp::sing)
           :calls '(fortran-to-lisp::dpmpar))))

