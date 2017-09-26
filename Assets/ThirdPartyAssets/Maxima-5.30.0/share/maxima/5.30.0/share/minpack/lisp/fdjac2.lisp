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


(let ((zero 0.0))
  (declare (type (double-float) zero))
  (defun fdjac2 (fcn m n x fvec fjac ldfjac iflag epsfcn wa)
    (declare (type (double-float) epsfcn)
             (type (array double-float (*)) wa fjac fvec x)
             (type (f2cl-lib:integer4) iflag ldfjac n m))
    (f2cl-lib:with-multi-array-data
        ((x double-float x-%data% x-%offset%)
         (fvec double-float fvec-%data% fvec-%offset%)
         (fjac double-float fjac-%data% fjac-%offset%)
         (wa double-float wa-%data% wa-%offset%))
      (prog ((eps 0.0) (epsmch 0.0) (h 0.0) (temp 0.0) (i 0) (j 0))
        (declare (type (f2cl-lib:integer4) j i)
                 (type (double-float) temp h epsmch eps))
        '"     **********"
        '""
        '"     subroutine fdjac2"
        '""
        '"     this subroutine computes a forward-difference approximation"
        '"     to the m by n jacobian matrix associated with a specified"
        '"     problem of m functions in n variables."
        '""
        '"     the subroutine statement is"
        '""
        '"       subroutine fdjac2(fcn,m,n,x,fvec,fjac,ldfjac,iflag,epsfcn,wa)"
        '""
        '"     where"
        '""
        '"       fcn is the name of the user-supplied subroutine which"
        '"         calculates the functions. fcn must be declared"
        '"         in an external statement in the user calling"
        '"         program, and should be written as follows."
        '""
        '"         subroutine fcn(m,n,x,fvec,iflag)"
        '"         integer m,n,iflag"
        '"         double precision x(n),fvec(m)"
        '"         ----------"
        '"         calculate the functions at x and"
        '"         return this vector in fvec."
        '"         ----------"
        '"         return"
        '"         end"
        '""
        '"         the value of iflag should not be changed by fcn unless"
        '"         the user wants to terminate execution of fdjac2."
        '"         in this case set iflag to a negative integer."
        '""
        '"       m is a positive integer input variable set to the number"
        '"         of functions."
        '""
        '"       n is a positive integer input variable set to the number"
        '"         of variables. n must not exceed m."
        '""
        '"       x is an input array of length n."
        '""
        '"       fvec is an input array of length m which must contain the"
        '"         functions evaluated at x."
        '""
        '"       fjac is an output m by n array which contains the"
        '"         approximation to the jacobian matrix evaluated at x."
        '""
        '"       ldfjac is a positive integer input variable not less than m"
        '"         which specifies the leading dimension of the array fjac."
        '""
        '"       iflag is an integer variable which can be used to terminate"
        '"         the execution of fdjac2. see description of fcn."
        '""
        '"       epsfcn is an input variable used in determining a suitable"
        '"         step length for the forward-difference approximation. this"
        '"         approximation assumes that the relative errors in the"
        '"         functions are of the order of epsfcn. if epsfcn is less"
        '"         than the machine precision, it is assumed that the relative"
        '"         errors in the functions are of the order of the machine"
        '"         precision."
        '""
        '"       wa is a work array of length m."
        '""
        '"     subprograms called"
        '""
        '"       user-supplied ...... fcn"
        '""
        '"       minpack-supplied ... dpmpar"
        '""
        '"       fortran-supplied ... dabs,dmax1,dsqrt"
        '""
        '"     argonne national laboratory. minpack project. march 1980."
        '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
        '""
        '"     **********"
        '""
        '"     epsmch is the machine precision."
        '""
        (setf epsmch (dpmpar 1))
        '""
        (setf eps (f2cl-lib:dsqrt (f2cl-lib:dmax1 epsfcn epsmch)))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf temp (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%))
            (setf h (* eps (f2cl-lib:dabs temp)))
            (if (= h zero) (setf h eps))
            (setf (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%) (+ temp h))
            (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                (funcall fcn m n x wa iflag)
              (declare (ignore var-2 var-3))
              (when var-0
                (setf m var-0))
              (when var-1
                (setf n var-1))
              (when var-4
                (setf iflag var-4)))
            (if (< iflag 0) (go label30))
            (setf (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%) temp)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (setf (f2cl-lib:fref fjac-%data%
                                     (i j)
                                     ((1 ldfjac) (1 n))
                                     fjac-%offset%)
                        (/
                         (- (f2cl-lib:fref wa-%data% (i) ((1 m)) wa-%offset%)
                            (f2cl-lib:fref fvec-%data%
                                           (i)
                                           ((1 m))
                                           fvec-%offset%))
                         h))
               label10))
           label20))
       label30
        (go end_label)
        '""
        '"     last card of subroutine fdjac2."
        '""
       end_label
        (return (values nil m n nil nil nil nil iflag nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::fdjac2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (double-float) (array double-float (*)))
           :return-values '(nil fortran-to-lisp::m fortran-to-lisp::n nil nil
                            nil nil fortran-to-lisp::iflag nil nil)
           :calls '(fortran-to-lisp::dpmpar))))

