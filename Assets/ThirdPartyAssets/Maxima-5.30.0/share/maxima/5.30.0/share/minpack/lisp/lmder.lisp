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


(let ((one 1.0)
      (p1 0.1)
      (p5 0.5)
      (p25 0.25)
      (p75 0.75)
      (p0001 1.0e-4)
      (zero 0.0))
  (declare (type (double-float) one p1 p5 p25 p75 p0001 zero))
  (defun lmder
         (fcn m n x fvec fjac ldfjac ftol xtol gtol maxfev diag mode factor
          nprint info nfev njev ipvt qtf wa1 wa2 wa3 wa4)
    (declare (type (array f2cl-lib:integer4 (*)) ipvt)
             (type (double-float) factor gtol xtol ftol)
             (type (array double-float (*)) wa4 wa3 wa2 wa1 qtf diag fjac fvec
                                            x)
             (type (f2cl-lib:integer4) njev nfev info nprint mode maxfev ldfjac
                                       n m))
    (f2cl-lib:with-multi-array-data
        ((x double-float x-%data% x-%offset%)
         (fvec double-float fvec-%data% fvec-%offset%)
         (fjac double-float fjac-%data% fjac-%offset%)
         (diag double-float diag-%data% diag-%offset%)
         (qtf double-float qtf-%data% qtf-%offset%)
         (wa1 double-float wa1-%data% wa1-%offset%)
         (wa2 double-float wa2-%data% wa2-%offset%)
         (wa3 double-float wa3-%data% wa3-%offset%)
         (wa4 double-float wa4-%data% wa4-%offset%)
         (ipvt f2cl-lib:integer4 ipvt-%data% ipvt-%offset%))
      (prog ((actred 0.0) (delta 0.0) (dirder 0.0) (epsmch 0.0) (fnorm 0.0)
             (fnorm1 0.0) (gnorm 0.0) (par 0.0) (pnorm 0.0) (prered 0.0)
             (ratio 0.0) (sum 0.0) (temp 0.0) (temp1 0.0) (temp2 0.0)
             (xnorm 0.0) (i 0) (iflag 0) (iter 0) (j 0) (l 0))
        (declare (type (f2cl-lib:integer4) l j iter iflag i)
                 (type (double-float) xnorm temp2 temp1 temp sum ratio prered
                                      pnorm par gnorm fnorm1 fnorm epsmch
                                      dirder delta actred))
        '"     **********"
        '""
        '"     subroutine lmder"
        '""
        '"     the purpose of lmder is to minimize the sum of the squares of"
        '"     m nonlinear functions in n variables by a modification of"
        '"     the levenberg-marquardt algorithm. the user must provide a"
        '"     subroutine which calculates the functions and the jacobian."
        '""
        '"     the subroutine statement is"
        '""
        '"       subroutine lmder(fcn,m,n,x,fvec,fjac,ldfjac,ftol,xtol,gtol,"
        '"                        maxfev,diag,mode,factor,nprint,info,nfev,"
        '"                        njev,ipvt,qtf,wa1,wa2,wa3,wa4)"
        '""
        '"     where"
        '""
        '"       fcn is the name of the user-supplied subroutine which"
        '"         calculates the functions and the jacobian. fcn must"
        '"         be declared in an external statement in the user"
        '"         calling program, and should be written as follows."
        '""
        '"         subroutine fcn(m,n,x,fvec,fjac,ldfjac,iflag)"
        '"         integer m,n,ldfjac,iflag"
        '"         double precision x(n),fvec(m),fjac(ldfjac,n)"
        '"         ----------"
        '"         if iflag = 1 calculate the functions at x and"
        '"         return this vector in fvec. do not alter fjac."
        '"         if iflag = 2 calculate the jacobian at x and"
        '"         return this matrix in fjac. do not alter fvec."
        '"         ----------"
        '"         return"
        '"         end"
        '""
        '"         the value of iflag should not be changed by fcn unless"
        '"         the user wants to terminate execution of lmder."
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
        '"       fjac is an output m by n array. the upper n by n submatrix"
        '"         of fjac contains an upper triangular matrix r with"
        '"         diagonal elements of nonincreasing magnitude such that"
        '""
        '"                t     t           t"
        '"               p *(jac *jac)*p = r *r,"
        '""
        '"         where p is a permutation matrix and jac is the final"
        '"         calculated jacobian. column j of p is column ipvt(j)"
        '"         (see below) of the identity matrix. the lower trapezoidal"
        '"         part of fjac contains information generated during"
        '"         the computation of r."
        '""
        '"       ldfjac is a positive integer input variable not less than m"
        '"         which specifies the leading dimension of the array fjac."
        '""
        '"       ftol is a nonnegative input variable. termination"
        '"         occurs when both the actual and predicted relative"
        '"         reductions in the sum of squares are at most ftol."
        '"         therefore, ftol measures the relative error desired"
        '"         in the sum of squares."
        '""
        '"       xtol is a nonnegative input variable. termination"
        '"         occurs when the relative error between two consecutive"
        '"         iterates is at most xtol. therefore, xtol measures the"
        '"         relative error desired in the approximate solution."
        '""
        '"       gtol is a nonnegative input variable. termination"
        '"         occurs when the cosine of the angle between fvec and"
        '"         any column of the jacobian is at most gtol in absolute"
        '"         value. therefore, gtol measures the orthogonality"
        '"         desired between the function vector and the columns"
        '"         of the jacobian."
        '""
        '"       maxfev is a positive integer input variable. termination"
        '"         occurs when the number of calls to fcn with iflag = 1"
        '"         has reached maxfev."
        '""
        '"       diag is an array of length n. if mode = 1 (see"
        '"         below), diag is internally set. if mode = 2, diag"
        '"         must contain positive entries that serve as"
        '"         multiplicative scale factors for the variables."
        '""
        '"       mode is an integer input variable. if mode = 1, the"
        '"         variables will be scaled internally. if mode = 2,"
        '"         the scaling is specified by the input diag. other"
        '"         values of mode are equivalent to mode = 1."
        '""
        '"       factor is a positive input variable used in determining the"
        '"         initial step bound. this bound is set to the product of"
        '"         factor and the euclidean norm of diag*x if nonzero, or else"
        '"         to factor itself. in most cases factor should lie in the"
        '"         interval (.1,100.).100. is a generally recommended value."
        '""
        '"       nprint is an integer input variable that enables controlled"
        '"         printing of iterates if it is positive. in this case,"
        '"         fcn is called with iflag = 0 at the beginning of the first"
        '"         iteration and every nprint iterations thereafter and"
        '"         immediately prior to return, with x, fvec, and fjac"
        '"         available for printing. fvec and fjac should not be"
        '"         altered. if nprint is not positive, no special calls"
        '"         of fcn with iflag = 0 are made."
        '""
        '"       info is an integer output variable. if the user has"
        '"         terminated execution, info is set to the (negative)"
        '"         value of iflag. see description of fcn. otherwise,"
        '"         info is set as follows."
        '""
        '"         info = 0  improper input parameters."
        '""
        '"         info = 1  both actual and predicted relative reductions"
        '"                   in the sum of squares are at most ftol."
        '""
        '"         info = 2  relative error between two consecutive iterates"
        '"                   is at most xtol."
        '""
        '"         info = 3  conditions for info = 1 and info = 2 both hold."
        '""
        '"         info = 4  the cosine of the angle between fvec and any"
        '"                   column of the jacobian is at most gtol in"
        '"                   absolute value."
        '""
        '"         info = 5  number of calls to fcn with iflag = 1 has"
        '"                   reached maxfev."
        '""
        '"         info = 6  ftol is too small. no further reduction in"
        '"                   the sum of squares is possible."
        '""
        '"         info = 7  xtol is too small. no further improvement in"
        '"                   the approximate solution x is possible."
        '""
        '"         info = 8  gtol is too small. fvec is orthogonal to the"
        '"                   columns of the jacobian to machine precision."
        '""
        '"       nfev is an integer output variable set to the number of"
        '"         calls to fcn with iflag = 1."
        '""
        '"       njev is an integer output variable set to the number of"
        '"         calls to fcn with iflag = 2."
        '""
        '"       ipvt is an integer output array of length n. ipvt"
        '"         defines a permutation matrix p such that jac*p = q*r,"
        '"         where jac is the final calculated jacobian, q is"
        '"         orthogonal (not stored), and r is upper triangular"
        '"         with diagonal elements of nonincreasing magnitude."
        '"         column j of p is column ipvt(j) of the identity matrix."
        '""
        '"       qtf is an output array of length n which contains"
        '"         the first n elements of the vector (q transpose)*fvec."
        '""
        '"       wa1, wa2, and wa3 are work arrays of length n."
        '""
        '"       wa4 is a work array of length m."
        '""
        '"     subprograms called"
        '""
        '"       user-supplied ...... fcn"
        '""
        '"       minpack-supplied ... dpmpar,enorm,lmpar,qrfac"
        '""
        '"       fortran-supplied ... dabs,dmax1,dmin1,dsqrt,mod"
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
        (setf info 0)
        (setf iflag 0)
        (setf nfev 0)
        (setf njev 0)
        '""
        '"     check the input parameters for errors."
        '""
        (if
         (or (<= n 0)
             (< m n)
             (< ldfjac m)
             (< ftol zero)
             (< xtol zero)
             (< gtol zero)
             (<= maxfev 0)
             (<= factor zero))
         (go label300))
        (if (/= mode 2) (go label20))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (if (<= (f2cl-lib:fref diag-%data% (j) ((1 n)) diag-%offset%) zero)
                (go label300))
           label10))
       label20
        '""
        '"     evaluate the function at the starting point"
        '"     and calculate its norm."
        '""
        (setf iflag 1)
        (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
            (funcall fcn m n x fvec fjac ldfjac iflag)
          (declare (ignore var-2 var-3 var-4))
          (when var-0
            (setf m var-0))
          (when var-1
            (setf n var-1))
          (when var-5
            (setf ldfjac var-5))
          (when var-6
            (setf iflag var-6)))
        (setf nfev 1)
        (if (< iflag 0) (go label300))
        (setf fnorm (enorm m fvec))
        '""
        '"     initialize levenberg-marquardt parameter and iteration counter."
        '""
        (setf par zero)
        (setf iter 1)
        '""
        '"     beginning of the outer loop."
        '""
       label30
        '""
        '"        calculate the jacobian matrix."
        '""
        (setf iflag 2)
        (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
            (funcall fcn m n x fvec fjac ldfjac iflag)
          (declare (ignore var-2 var-3 var-4))
          (when var-0
            (setf m var-0))
          (when var-1
            (setf n var-1))
          (when var-5
            (setf ldfjac var-5))
          (when var-6
            (setf iflag var-6)))
        (setf njev (f2cl-lib:int-add njev 1))
        (if (< iflag 0) (go label300))
        '""
        '"        if requested, call fcn to enable printing of iterates."
        '""
        (if (<= nprint 0) (go label40))
        (setf iflag 0)
        (if (= (mod (f2cl-lib:int-sub iter 1) nprint) 0)
            (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                (funcall fcn m n x fvec fjac ldfjac iflag)
              (declare (ignore var-2 var-3 var-4))
              (when var-0
                (setf m var-0))
              (when var-1
                (setf n var-1))
              (when var-5
                (setf ldfjac var-5))
              (when var-6
                (setf iflag var-6))))
        (if (< iflag 0) (go label300))
       label40
        '""
        '"        compute the qr factorization of the jacobian."
        '""
        (qrfac m n fjac ldfjac f2cl-lib:%true% ipvt n wa1 wa2 wa3)
        '""
        '"        on the first iteration and if mode is 1, scale according"
        '"        to the norms of the columns of the initial jacobian."
        '""
        (if (/= iter 1) (go label80))
        (if (= mode 2) (go label60))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf (f2cl-lib:fref diag-%data% (j) ((1 n)) diag-%offset%)
                    (f2cl-lib:fref wa2-%data% (j) ((1 n)) wa2-%offset%))
            (if (= (f2cl-lib:fref wa2-%data% (j) ((1 n)) wa2-%offset%) zero)
                (setf (f2cl-lib:fref diag-%data% (j) ((1 n)) diag-%offset%)
                        one))
           label50))
       label60
        '""
        '"        on the first iteration, calculate the norm of the scaled x"
        '"        and initialize the step bound delta."
        '""
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf (f2cl-lib:fref wa3-%data% (j) ((1 n)) wa3-%offset%)
                    (* (f2cl-lib:fref diag-%data% (j) ((1 n)) diag-%offset%)
                       (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)))
           label70))
        (setf xnorm (enorm n wa3))
        (setf delta (* factor xnorm))
        (if (= delta zero) (setf delta factor))
       label80
        '""
        '"        form (q transpose)*fvec and store the first n components in"
        '"        qtf."
        '""
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i m) nil)
          (tagbody
            (setf (f2cl-lib:fref wa4-%data% (i) ((1 m)) wa4-%offset%)
                    (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%))
           label90))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (if
             (=
              (f2cl-lib:fref fjac-%data%
                             (j j)
                             ((1 ldfjac) (1 n))
                             fjac-%offset%)
              zero)
             (go label120))
            (setf sum zero)
            (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (setf sum
                        (+ sum
                           (*
                            (f2cl-lib:fref fjac-%data%
                                           (i j)
                                           ((1 ldfjac) (1 n))
                                           fjac-%offset%)
                            (f2cl-lib:fref wa4-%data%
                                           (i)
                                           ((1 m))
                                           wa4-%offset%))))
               label100))
            (setf temp
                    (/ (- sum)
                       (f2cl-lib:fref fjac-%data%
                                      (j j)
                                      ((1 ldfjac) (1 n))
                                      fjac-%offset%)))
            (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (setf (f2cl-lib:fref wa4-%data% (i) ((1 m)) wa4-%offset%)
                        (+ (f2cl-lib:fref wa4-%data% (i) ((1 m)) wa4-%offset%)
                           (*
                            (f2cl-lib:fref fjac-%data%
                                           (i j)
                                           ((1 ldfjac) (1 n))
                                           fjac-%offset%)
                            temp)))
               label110))
           label120
            (setf (f2cl-lib:fref fjac-%data%
                                 (j j)
                                 ((1 ldfjac) (1 n))
                                 fjac-%offset%)
                    (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%))
            (setf (f2cl-lib:fref qtf-%data% (j) ((1 n)) qtf-%offset%)
                    (f2cl-lib:fref wa4-%data% (j) ((1 m)) wa4-%offset%))
           label130))
        '""
        '"        compute the norm of the scaled gradient."
        '""
        (setf gnorm zero)
        (if (= fnorm zero) (go label170))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf l (f2cl-lib:fref ipvt-%data% (j) ((1 n)) ipvt-%offset%))
            (if (= (f2cl-lib:fref wa2-%data% (l) ((1 n)) wa2-%offset%) zero)
                (go label150))
            (setf sum zero)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i j) nil)
              (tagbody
                (setf sum
                        (+ sum
                           (*
                            (f2cl-lib:fref fjac-%data%
                                           (i j)
                                           ((1 ldfjac) (1 n))
                                           fjac-%offset%)
                            (/
                             (f2cl-lib:fref qtf-%data%
                                            (i)
                                            ((1 n))
                                            qtf-%offset%)
                             fnorm))))
               label140))
            (setf gnorm
                    (f2cl-lib:dmax1 gnorm
                                    (f2cl-lib:dabs
                                     (/ sum
                                        (f2cl-lib:fref wa2-%data%
                                                       (l)
                                                       ((1 n))
                                                       wa2-%offset%)))))
           label150
           label160))
       label170
        '""
        '"        test for convergence of the gradient norm."
        '""
        (if (<= gnorm gtol) (setf info 4))
        (if (/= info 0) (go label300))
        '""
        '"        rescale if necessary."
        '""
        (if (= mode 2) (go label190))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf (f2cl-lib:fref diag-%data% (j) ((1 n)) diag-%offset%)
                    (f2cl-lib:dmax1
                     (f2cl-lib:fref diag-%data% (j) ((1 n)) diag-%offset%)
                     (f2cl-lib:fref wa2-%data% (j) ((1 n)) wa2-%offset%)))
           label180))
       label190
        '""
        '"        beginning of the inner loop."
        '""
       label200
        '""
        '"           determine the levenberg-marquardt parameter."
        '""
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11)
            (lmpar n fjac ldfjac ipvt diag qtf delta par wa1 wa2 wa3 wa4)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-8
                           var-9 var-10 var-11))
          (setf par var-7))
        '""
        '"           store the direction p and x + p. calculate the norm of p."
        '""
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                    (- (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)))
            (setf (f2cl-lib:fref wa2-%data% (j) ((1 n)) wa2-%offset%)
                    (+ (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                       (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)))
            (setf (f2cl-lib:fref wa3-%data% (j) ((1 n)) wa3-%offset%)
                    (* (f2cl-lib:fref diag-%data% (j) ((1 n)) diag-%offset%)
                       (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)))
           label210))
        (setf pnorm (enorm n wa3))
        '""
        '"           on the first iteration, adjust the initial step bound."
        '""
        (if (= iter 1) (setf delta (f2cl-lib:dmin1 delta pnorm)))
        '""
        '"           evaluate the function at x + p and calculate its norm."
        '""
        (setf iflag 1)
        (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
            (funcall fcn m n wa2 wa4 fjac ldfjac iflag)
          (declare (ignore var-2 var-3 var-4))
          (when var-0
            (setf m var-0))
          (when var-1
            (setf n var-1))
          (when var-5
            (setf ldfjac var-5))
          (when var-6
            (setf iflag var-6)))
        (setf nfev (f2cl-lib:int-add nfev 1))
        (if (< iflag 0) (go label300))
        (setf fnorm1 (enorm m wa4))
        '""
        '"           compute the scaled actual reduction."
        '""
        (setf actred (- one))
        (if (< (* p1 fnorm1) fnorm)
            (setf actred (- one (expt (/ fnorm1 fnorm) 2))))
        '""
        '"           compute the scaled predicted reduction and"
        '"           the scaled directional derivative."
        '""
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf (f2cl-lib:fref wa3-%data% (j) ((1 n)) wa3-%offset%) zero)
            (setf l (f2cl-lib:fref ipvt-%data% (j) ((1 n)) ipvt-%offset%))
            (setf temp (f2cl-lib:fref wa1-%data% (l) ((1 n)) wa1-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i j) nil)
              (tagbody
                (setf (f2cl-lib:fref wa3-%data% (i) ((1 n)) wa3-%offset%)
                        (+ (f2cl-lib:fref wa3-%data% (i) ((1 n)) wa3-%offset%)
                           (*
                            (f2cl-lib:fref fjac-%data%
                                           (i j)
                                           ((1 ldfjac) (1 n))
                                           fjac-%offset%)
                            temp)))
               label220))
           label230))
        (setf temp1 (/ (enorm n wa3) fnorm))
        (setf temp2 (/ (* (f2cl-lib:dsqrt par) pnorm) fnorm))
        (setf prered (+ (expt temp1 2) (/ (expt temp2 2) p5)))
        (setf dirder (- (+ (expt temp1 2) (expt temp2 2))))
        '""
        '"           compute the ratio of the actual to the predicted"
        '"           reduction."
        '""
        (setf ratio zero)
        (if (/= prered zero) (setf ratio (/ actred prered)))
        '""
        '"           update the step bound."
        '""
        (if (> ratio p25) (go label240))
        (if (>= actred zero) (setf temp p5))
        (if (< actred zero)
            (setf temp (/ (* p5 dirder) (+ dirder (* p5 actred)))))
        (if (or (>= (* p1 fnorm1) fnorm) (< temp p1)) (setf temp p1))
        (setf delta (* temp (f2cl-lib:dmin1 delta (/ pnorm p1))))
        (setf par (/ par temp))
        (go label260)
       label240
        (if (and (/= par zero) (< ratio p75)) (go label250))
        (setf delta (/ pnorm p5))
        (setf par (* p5 par))
       label250
       label260
        '""
        '"           test for successful iteration."
        '""
        (if (< ratio p0001) (go label290))
        '""
        '"           successful iteration. update x, fvec, and their norms."
        '""
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                    (f2cl-lib:fref wa2-%data% (j) ((1 n)) wa2-%offset%))
            (setf (f2cl-lib:fref wa2-%data% (j) ((1 n)) wa2-%offset%)
                    (* (f2cl-lib:fref diag-%data% (j) ((1 n)) diag-%offset%)
                       (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)))
           label270))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i m) nil)
          (tagbody
            (setf (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                    (f2cl-lib:fref wa4-%data% (i) ((1 m)) wa4-%offset%))
           label280))
        (setf xnorm (enorm n wa2))
        (setf fnorm fnorm1)
        (setf iter (f2cl-lib:int-add iter 1))
       label290
        '""
        '"           tests for convergence."
        '""
        (if
         (and (<= (f2cl-lib:dabs actred) ftol)
              (<= prered ftol)
              (<= (* p5 ratio) one))
         (setf info 1))
        (if (<= delta (* xtol xnorm)) (setf info 2))
        (if
         (and (<= (f2cl-lib:dabs actred) ftol)
              (<= prered ftol)
              (<= (* p5 ratio) one)
              (= info 2))
         (setf info 3))
        (if (/= info 0) (go label300))
        '""
        '"           tests for termination and stringent tolerances."
        '""
        (if (>= nfev maxfev) (setf info 5))
        (if
         (and (<= (f2cl-lib:dabs actred) epsmch)
              (<= prered epsmch)
              (<= (* p5 ratio) one))
         (setf info 6))
        (if (<= delta (* epsmch xnorm)) (setf info 7))
        (if (<= gnorm epsmch) (setf info 8))
        (if (/= info 0) (go label300))
        '""
        '"           end of the inner loop. repeat if iteration unsuccessful."
        '""
        (if (< ratio p0001) (go label200))
        '""
        '"        end of the outer loop."
        '""
        (go label30)
       label300
        '""
        '"     termination, either normal or user imposed."
        '""
        (if (< iflag 0) (setf info iflag))
        (setf iflag 0)
        (if (> nprint 0)
            (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                (funcall fcn m n x fvec fjac ldfjac iflag)
              (declare (ignore var-2 var-3 var-4))
              (when var-0
                (setf m var-0))
              (when var-1
                (setf n var-1))
              (when var-5
                (setf ldfjac var-5))
              (when var-6
                (setf iflag var-6))))
        (go end_label)
        '""
        '"     last card of subroutine lmder."
        '""
       end_label
        (return
         (values nil
                 m
                 n
                 nil
                 nil
                 nil
                 ldfjac
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 info
                 nfev
                 njev
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::lmder fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (double-float)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)))
           :return-values '(nil fortran-to-lisp::m fortran-to-lisp::n nil nil
                            nil fortran-to-lisp::ldfjac nil nil nil nil nil nil
                            nil nil fortran-to-lisp::info fortran-to-lisp::nfev
                            fortran-to-lisp::njev nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::lmpar fortran-to-lisp::qrfac
                    fortran-to-lisp::enorm fortran-to-lisp::dpmpar))))

