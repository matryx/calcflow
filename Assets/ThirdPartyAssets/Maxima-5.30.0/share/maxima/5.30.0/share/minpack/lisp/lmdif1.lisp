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


(let ((factor 100.0) (zero 0.0))
  (declare (type (double-float) factor zero))
  (defun lmdif1 (fcn m n x fvec tol info iwa wa lwa)
    (declare (type (array f2cl-lib:integer4 (*)) iwa)
             (type (double-float) tol)
             (type (array double-float (*)) wa fvec x)
             (type (f2cl-lib:integer4) lwa info n m))
    (f2cl-lib:with-multi-array-data
        ((x double-float x-%data% x-%offset%)
         (fvec double-float fvec-%data% fvec-%offset%)
         (wa double-float wa-%data% wa-%offset%)
         (iwa f2cl-lib:integer4 iwa-%data% iwa-%offset%))
      (prog ((epsfcn 0.0) (ftol 0.0) (gtol 0.0) (xtol 0.0) (maxfev 0) (mode 0)
             (mp5n 0) (nfev 0) (nprint 0))
        (declare (type (f2cl-lib:integer4) nprint nfev mp5n mode maxfev)
                 (type (double-float) xtol gtol ftol epsfcn))
        '"     **********"
        '""
        '"     subroutine lmdif1"
        '""
        '"     the purpose of lmdif1 is to minimize the sum of the squares of"
        '"     m nonlinear functions in n variables by a modification of the"
        '"     levenberg-marquardt algorithm. this is done by using the more"
        '"     general least-squares solver lmdif. the user must provide a"
        '"     subroutine which calculates the functions. the jacobian is"
        '"     then calculated by a forward-difference approximation."
        '""
        '"     the subroutine statement is"
        '""
        '"       subroutine lmdif1(fcn,m,n,x,fvec,tol,info,iwa,wa,lwa)"
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
        '"         the user wants to terminate execution of lmdif1."
        '"         in this case set iflag to a negative integer."
        '""
        '"       m is a positive integer input variable set to the number"
        '"         of functions."
        '""
        '"       n is a positive integer input variable set to the number"
        '"         of variables. n must not exceed m."
        '""
        '"       x is an array of length n. on input x must contain"
        '"         an initial estimate of the solution vector. on output x"
        '"         contains the final estimate of the solution vector."
        '""
        '"       fvec is an output array of length m which contains"
        '"         the functions evaluated at the output x."
        '""
        '"       tol is a nonnegative input variable. termination occurs"
        '"         when the algorithm estimates either that the relative"
        '"         error in the sum of squares is at most tol or that"
        '"         the relative error between x and the solution is at"
        '"         most tol."
        '""
        '"       info is an integer output variable. if the user has"
        '"         terminated execution, info is set to the (negative)"
        '"         value of iflag. see description of fcn. otherwise,"
        '"         info is set as follows."
        '""
        '"         info = 0  improper input parameters."
        '""
        '"         info = 1  algorithm estimates that the relative error"
        '"                   in the sum of squares is at most tol."
        '""
        '"         info = 2  algorithm estimates that the relative error"
        '"                   between x and the solution is at most tol."
        '""
        '"         info = 3  conditions for info = 1 and info = 2 both hold."
        '""
        '"         info = 4  fvec is orthogonal to the columns of the"
        '"                   jacobian to machine precision."
        '""
        '"         info = 5  number of calls to fcn has reached or"
        '"                   exceeded 200*(n+1)."
        '""
        '"         info = 6  tol is too small. no further reduction in"
        '"                   the sum of squares is possible."
        '""
        '"         info = 7  tol is too small. no further improvement in"
        '"                   the approximate solution x is possible."
        '""
        '"       iwa is an integer work array of length n."
        '""
        '"       wa is a work array of length lwa."
        '""
        '"       lwa is a positive integer input variable not less than"
        '"         m*n+5*n+m."
        '""
        '"     subprograms called"
        '""
        '"       user-supplied ...... fcn"
        '""
        '"       minpack-supplied ... lmdif"
        '""
        '"     argonne national laboratory. minpack project. march 1980."
        '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
        '""
        '"     **********"
        (setf info 0)
        '""
        '"     check the input parameters for errors."
        '""
        (if
         (or (<= n 0)
             (< m n)
             (< tol zero)
             (< lwa
                (f2cl-lib:int-add (f2cl-lib:int-mul m n)
                                  (f2cl-lib:int-mul 5 n)
                                  m)))
         (go label10))
        '""
        '"     call lmdif."
        '""
        (setf maxfev (f2cl-lib:int-mul 200 (f2cl-lib:int-add n 1)))
        (setf ftol tol)
        (setf xtol tol)
        (setf gtol zero)
        (setf epsfcn zero)
        (setf mode 1)
        (setf nprint 0)
        (setf mp5n (f2cl-lib:int-add m (f2cl-lib:int-mul 5 n)))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18
               var-19 var-20 var-21 var-22 var-23)
            (lmdif fcn m n x fvec ftol xtol gtol maxfev epsfcn
             (f2cl-lib:array-slice wa double-float (1) ((1 lwa))) mode factor
             nprint info nfev
             (f2cl-lib:array-slice wa double-float ((+ mp5n 1)) ((1 lwa))) m
             iwa (f2cl-lib:array-slice wa double-float ((+ n 1)) ((1 lwa)))
             (f2cl-lib:array-slice wa
                                   double-float
                                   ((+ (f2cl-lib:int-mul 2 n) 1))
                                   ((1 lwa)))
             (f2cl-lib:array-slice wa
                                   double-float
                                   ((+ (f2cl-lib:int-mul 3 n) 1))
                                   ((1 lwa)))
             (f2cl-lib:array-slice wa
                                   double-float
                                   ((+ (f2cl-lib:int-mul 4 n) 1))
                                   ((1 lwa)))
             (f2cl-lib:array-slice wa
                                   double-float
                                   ((+ (f2cl-lib:int-mul 5 n) 1))
                                   ((1 lwa))))
          (declare (ignore var-0 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                           var-10 var-11 var-12 var-13 var-16 var-17 var-18
                           var-19 var-20 var-21 var-22 var-23))
          (setf m var-1)
          (setf n var-2)
          (setf info var-14)
          (setf nfev var-15))
        (if (= info 8) (setf info 4))
       label10
        (go end_label)
        '""
        '"     last card of subroutine lmdif1."
        '""
       end_label
        (return (values nil m n nil nil nil info nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::lmdif1
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (double-float)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil fortran-to-lisp::m fortran-to-lisp::n nil nil
                            nil fortran-to-lisp::info nil nil nil)
           :calls '(fortran-to-lisp::lmdif))))

