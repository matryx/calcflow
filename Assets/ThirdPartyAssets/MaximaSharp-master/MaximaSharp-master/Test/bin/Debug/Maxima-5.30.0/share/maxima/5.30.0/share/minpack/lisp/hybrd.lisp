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


(let ((one 1.0) (p1 0.1) (p5 0.5) (p001 0.001) (p0001 1.0e-4) (zero 0.0))
  (declare (type (double-float) one p1 p5 p001 p0001 zero))
  (defun hybrd
         (fcn n x fvec xtol maxfev ml mu epsfcn diag mode factor nprint info
          nfev fjac ldfjac r lr qtf wa1 wa2 wa3 wa4)
    (declare (type (double-float) factor epsfcn xtol)
             (type (array double-float (*)) wa4 wa3 wa2 wa1 qtf r fjac diag
                                            fvec x)
             (type (f2cl-lib:integer4) lr ldfjac nfev info nprint mode mu ml
                                       maxfev n))
    (f2cl-lib:with-multi-array-data
        ((x double-float x-%data% x-%offset%)
         (fvec double-float fvec-%data% fvec-%offset%)
         (diag double-float diag-%data% diag-%offset%)
         (fjac double-float fjac-%data% fjac-%offset%)
         (r double-float r-%data% r-%offset%)
         (qtf double-float qtf-%data% qtf-%offset%)
         (wa1 double-float wa1-%data% wa1-%offset%)
         (wa2 double-float wa2-%data% wa2-%offset%)
         (wa3 double-float wa3-%data% wa3-%offset%)
         (wa4 double-float wa4-%data% wa4-%offset%))
      (prog ((actred 0.0) (delta 0.0) (epsmch 0.0) (fnorm 0.0) (fnorm1 0.0)
             (pnorm 0.0) (prered 0.0) (ratio 0.0) (sum 0.0) (temp 0.0)
             (xnorm 0.0) (jeval nil) (sing nil)
             (iwa (make-array 1 :element-type 'f2cl-lib:integer4)) (i 0)
             (iflag 0) (iter 0) (j 0) (jm1 0) (l 0) (msum 0) (ncfail 0)
             (ncsuc 0) (nslow1 0) (nslow2 0))
        (declare (type (f2cl-lib:integer4) nslow2 nslow1 ncsuc ncfail msum l
                                           jm1 j iter iflag i)
                 (type (array f2cl-lib:integer4 (1)) iwa)
                 (type f2cl-lib:logical sing jeval)
                 (type (double-float) xnorm temp sum ratio prered pnorm fnorm1
                                      fnorm epsmch delta actred))
        '"     **********"
        '""
        '"     subroutine hybrd"
        '""
        '"     the purpose of hybrd is to find a zero of a system of"
        '"     n nonlinear functions in n variables by a modification"
        '"     of the powell hybrid method. the user must provide a"
        '"     subroutine which calculates the functions. the jacobian is"
        '"     then calculated by a forward-difference approximation."
        '""
        '"     the subroutine statement is"
        '""
        '"       subroutine hybrd(fcn,n,x,fvec,xtol,maxfev,ml,mu,epsfcn,"
        '"                        diag,mode,factor,nprint,info,nfev,fjac,"
        '"                        ldfjac,r,lr,qtf,wa1,wa2,wa3,wa4)"
        '""
        '"     where"
        '""
        '"       fcn is the name of the user-supplied subroutine which"
        '"         calculates the functions. fcn must be declared"
        '"         in an external statement in the user calling"
        '"         program, and should be written as follows."
        '""
        '"         subroutine fcn(n,x,fvec,iflag)"
        '"         integer n,iflag"
        '"         double precision x(n),fvec(n)"
        '"         ----------"
        '"         calculate the functions at x and"
        '"         return this vector in fvec."
        '"         ---------"
        '"         return"
        '"         end"
        '""
        '"         the value of iflag should not be changed by fcn unless"
        '"         the user wants to terminate execution of hybrd."
        '"         in this case set iflag to a negative integer."
        '""
        '"       n is a positive integer input variable set to the number"
        '"         of functions and variables."
        '""
        '"       x is an array of length n. on input x must contain"
        '"         an initial estimate of the solution vector. on output x"
        '"         contains the final estimate of the solution vector."
        '""
        '"       fvec is an output array of length n which contains"
        '"         the functions evaluated at the output x."
        '""
        '"       xtol is a nonnegative input variable. termination"
        '"         occurs when the relative error between two consecutive"
        '"         iterates is at most xtol."
        '""
        '"       maxfev is a positive integer input variable. termination"
        '"         occurs when the number of calls to fcn is at least maxfev"
        '"         by the end of an iteration."
        '""
        '"       ml is a nonnegative integer input variable which specifies"
        '"         the number of subdiagonals within the band of the"
        '"         jacobian matrix. if the jacobian is not banded, set"
        '"         ml to at least n - 1."
        '""
        '"       mu is a nonnegative integer input variable which specifies"
        '"         the number of superdiagonals within the band of the"
        '"         jacobian matrix. if the jacobian is not banded, set"
        '"         mu to at least n - 1."
        '""
        '"       epsfcn is an input variable used in determining a suitable"
        '"         step length for the forward-difference approximation. this"
        '"         approximation assumes that the relative errors in the"
        '"         functions are of the order of epsfcn. if epsfcn is less"
        '"         than the machine precision, it is assumed that the relative"
        '"         errors in the functions are of the order of the machine"
        '"         precision."
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
        '"         interval (.1,100.). 100. is a generally recommended value."
        '""
        '"       nprint is an integer input variable that enables controlled"
        '"         printing of iterates if it is positive. in this case,"
        '"         fcn is called with iflag = 0 at the beginning of the first"
        '"         iteration and every nprint iterations thereafter and"
        '"         immediately prior to return, with x and fvec available"
        '"         for printing. if nprint is not positive, no special calls"
        '"         of fcn with iflag = 0 are made."
        '""
        '"       info is an integer output variable. if the user has"
        '"         terminated execution, info is set to the (negative)"
        '"         value of iflag. see description of fcn. otherwise,"
        '"         info is set as follows."
        '""
        '"         info = 0   improper input parameters."
        '""
        '"         info = 1   relative error between two consecutive iterates"
        '"                    is at most xtol."
        '""
        '"         info = 2   number of calls to fcn has reached or exceeded"
        '"                    maxfev."
        '""
        '"         info = 3   xtol is too small. no further improvement in"
        '"                    the approximate solution x is possible."
        '""
        '"         info = 4   iteration is not making good progress, as"
        '"                    measured by the improvement from the last"
        '"                    five jacobian evaluations."
        '""
        '"         info = 5   iteration is not making good progress, as"
        '"                    measured by the improvement from the last"
        '"                    ten iterations."
        '""
        '"       nfev is an integer output variable set to the number of"
        '"         calls to fcn."
        '""
        '"       fjac is an output n by n array which contains the"
        '"         orthogonal matrix q produced by the qr factorization"
        '"         of the final approximate jacobian."
        '""
        '"       ldfjac is a positive integer input variable not less than n"
        '"         which specifies the leading dimension of the array fjac."
        '""
        '"       r is an output array of length lr which contains the"
        '"         upper triangular matrix produced by the qr factorization"
        '"         of the final approximate jacobian, stored rowwise."
        '""
        '"       lr is a positive integer input variable not less than"
        '"         (n*(n+1))/2."
        '""
        '"       qtf is an output array of length n which contains"
        '"         the vector (q transpose)*fvec."
        '""
        '"       wa1, wa2, wa3, and wa4 are work arrays of length n."
        '""
        '"     subprograms called"
        '""
        '"       user-supplied ...... fcn"
        '""
        '"       minpack-supplied ... dogleg,dpmpar,enorm,fdjac1,"
        '"                            qform,qrfac,r1mpyq,r1updt"
        '""
        '"       fortran-supplied ... dabs,dmax1,dmin1,min0,mod"
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
        '""
        '"     check the input parameters for errors."
        '""
        (if
         (or (<= n 0)
             (< xtol zero)
             (<= maxfev 0)
             (< ml 0)
             (< mu 0)
             (<= factor zero)
             (< ldfjac n)
             (< lr (the f2cl-lib:integer4 (truncate (* n (+ n 1)) 2))))
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
        (multiple-value-bind (var-0 var-1 var-2 var-3)
            (funcall fcn n x fvec iflag)
          (declare (ignore var-1 var-2))
          (when var-0
            (setf n var-0))
          (when var-3
            (setf iflag var-3)))
        (setf nfev 1)
        (if (< iflag 0) (go label300))
        (setf fnorm (enorm n fvec))
        '""
        '"     determine the number of calls to fcn needed to compute"
        '"     the jacobian matrix."
        '""
        (setf msum (f2cl-lib:min0 (f2cl-lib:int-add ml mu 1) n))
        '""
        '"     initialize iteration counter and monitors."
        '""
        (setf iter 1)
        (setf ncsuc 0)
        (setf ncfail 0)
        (setf nslow1 0)
        (setf nslow2 0)
        '""
        '"     beginning of the outer loop."
        '""
       label30
        (setf jeval f2cl-lib:%true%)
        '""
        '"        calculate the jacobian matrix."
        '""
        (setf iflag 2)
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11)
            (fdjac1 fcn n x fvec fjac ldfjac iflag ml mu epsfcn wa1 wa2)
          (declare (ignore var-0 var-2 var-3 var-4 var-5 var-7 var-8 var-9
                           var-10 var-11))
          (setf n var-1)
          (setf iflag var-6))
        (setf nfev (f2cl-lib:int-add nfev msum))
        (if (< iflag 0) (go label300))
        '""
        '"        compute the qr factorization of the jacobian."
        '""
        (qrfac n n fjac ldfjac f2cl-lib:%false% iwa 1 wa1 wa2 wa3)
        '""
        '"        on the first iteration and if mode is 1, scale according"
        '"        to the norms of the columns of the initial jacobian."
        '""
        (if (/= iter 1) (go label70))
        (if (= mode 2) (go label50))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf (f2cl-lib:fref diag-%data% (j) ((1 n)) diag-%offset%)
                    (f2cl-lib:fref wa2-%data% (j) ((1 n)) wa2-%offset%))
            (if (= (f2cl-lib:fref wa2-%data% (j) ((1 n)) wa2-%offset%) zero)
                (setf (f2cl-lib:fref diag-%data% (j) ((1 n)) diag-%offset%)
                        one))
           label40))
       label50
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
           label60))
        (setf xnorm (enorm n wa3))
        (setf delta (* factor xnorm))
        (if (= delta zero) (setf delta factor))
       label70
        '""
        '"        form (q transpose)*fvec and store in qtf."
        '""
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf (f2cl-lib:fref qtf-%data% (i) ((1 n)) qtf-%offset%)
                    (f2cl-lib:fref fvec-%data% (i) ((1 n)) fvec-%offset%))
           label80))
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
             (go label110))
            (setf sum zero)
            (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf sum
                        (+ sum
                           (*
                            (f2cl-lib:fref fjac-%data%
                                           (i j)
                                           ((1 ldfjac) (1 n))
                                           fjac-%offset%)
                            (f2cl-lib:fref qtf-%data%
                                           (i)
                                           ((1 n))
                                           qtf-%offset%))))
               label90))
            (setf temp
                    (/ (- sum)
                       (f2cl-lib:fref fjac-%data%
                                      (j j)
                                      ((1 ldfjac) (1 n))
                                      fjac-%offset%)))
            (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf (f2cl-lib:fref qtf-%data% (i) ((1 n)) qtf-%offset%)
                        (+ (f2cl-lib:fref qtf-%data% (i) ((1 n)) qtf-%offset%)
                           (*
                            (f2cl-lib:fref fjac-%data%
                                           (i j)
                                           ((1 ldfjac) (1 n))
                                           fjac-%offset%)
                            temp)))
               label100))
           label110
           label120))
        '""
        '"        copy the triangular factor of the qr factorization into r."
        '""
        (setf sing f2cl-lib:%false%)
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf l j)
            (setf jm1 (f2cl-lib:int-sub j 1))
            (if (< jm1 1) (go label140))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i jm1) nil)
              (tagbody
                (setf (f2cl-lib:fref r-%data% (l) ((1 lr)) r-%offset%)
                        (f2cl-lib:fref fjac-%data%
                                       (i j)
                                       ((1 ldfjac) (1 n))
                                       fjac-%offset%))
                (setf l (f2cl-lib:int-sub (f2cl-lib:int-add l n) i))
               label130))
           label140
            (setf (f2cl-lib:fref r-%data% (l) ((1 lr)) r-%offset%)
                    (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%))
            (if (= (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%) zero)
                (setf sing f2cl-lib:%true%))
           label150))
        '""
        '"        accumulate the orthogonal factor in fjac."
        '""
        (qform n n fjac ldfjac wa1)
        '""
        '"        rescale if necessary."
        '""
        (if (= mode 2) (go label170))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf (f2cl-lib:fref diag-%data% (j) ((1 n)) diag-%offset%)
                    (f2cl-lib:dmax1
                     (f2cl-lib:fref diag-%data% (j) ((1 n)) diag-%offset%)
                     (f2cl-lib:fref wa2-%data% (j) ((1 n)) wa2-%offset%)))
           label160))
       label170
        '""
        '"        beginning of the inner loop."
        '""
       label180
        '""
        '"           if requested, call fcn to enable printing of iterates."
        '""
        (if (<= nprint 0) (go label190))
        (setf iflag 0)
        (if (= (mod (f2cl-lib:int-sub iter 1) nprint) 0)
            (multiple-value-bind (var-0 var-1 var-2 var-3)
                (funcall fcn n x fvec iflag)
              (declare (ignore var-1 var-2))
              (when var-0
                (setf n var-0))
              (when var-3
                (setf iflag var-3))))
        (if (< iflag 0) (go label300))
       label190
        '""
        '"           determine the direction p."
        '""
        (dogleg n r lr diag qtf delta wa1 wa2 wa3)
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
           label200))
        (setf pnorm (enorm n wa3))
        '""
        '"           on the first iteration, adjust the initial step bound."
        '""
        (if (= iter 1) (setf delta (f2cl-lib:dmin1 delta pnorm)))
        '""
        '"           evaluate the function at x + p and calculate its norm."
        '""
        (setf iflag 1)
        (multiple-value-bind (var-0 var-1 var-2 var-3)
            (funcall fcn n wa2 wa4 iflag)
          (declare (ignore var-1 var-2))
          (when var-0
            (setf n var-0))
          (when var-3
            (setf iflag var-3)))
        (setf nfev (f2cl-lib:int-add nfev 1))
        (if (< iflag 0) (go label300))
        (setf fnorm1 (enorm n wa4))
        '""
        '"           compute the scaled actual reduction."
        '""
        (setf actred (- one))
        (if (< fnorm1 fnorm) (setf actred (- one (expt (/ fnorm1 fnorm) 2))))
        '""
        '"           compute the scaled predicted reduction."
        '""
        (setf l 1)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf sum zero)
            (f2cl-lib:fdo (j i (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (setf sum
                        (+ sum
                           (* (f2cl-lib:fref r-%data% (l) ((1 lr)) r-%offset%)
                              (f2cl-lib:fref wa1-%data%
                                             (j)
                                             ((1 n))
                                             wa1-%offset%))))
                (setf l (f2cl-lib:int-add l 1))
               label210))
            (setf (f2cl-lib:fref wa3-%data% (i) ((1 n)) wa3-%offset%)
                    (+ (f2cl-lib:fref qtf-%data% (i) ((1 n)) qtf-%offset%)
                       sum))
           label220))
        (setf temp (enorm n wa3))
        (setf prered zero)
        (if (< temp fnorm) (setf prered (- one (expt (/ temp fnorm) 2))))
        '""
        '"           compute the ratio of the actual to the predicted"
        '"           reduction."
        '""
        (setf ratio zero)
        (if (> prered zero) (setf ratio (/ actred prered)))
        '""
        '"           update the step bound."
        '""
        (if (>= ratio p1) (go label230))
        (setf ncsuc 0)
        (setf ncfail (f2cl-lib:int-add ncfail 1))
        (setf delta (* p5 delta))
        (go label240)
       label230
        (setf ncfail 0)
        (setf ncsuc (f2cl-lib:int-add ncsuc 1))
        (if (or (>= ratio p5) (> ncsuc 1))
            (setf delta (f2cl-lib:dmax1 delta (/ pnorm p5))))
        (if (<= (f2cl-lib:dabs (- ratio one)) p1) (setf delta (/ pnorm p5)))
       label240
        '""
        '"           test for successful iteration."
        '""
        (if (< ratio p0001) (go label260))
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
            (setf (f2cl-lib:fref fvec-%data% (j) ((1 n)) fvec-%offset%)
                    (f2cl-lib:fref wa4-%data% (j) ((1 n)) wa4-%offset%))
           label250))
        (setf xnorm (enorm n wa2))
        (setf fnorm fnorm1)
        (setf iter (f2cl-lib:int-add iter 1))
       label260
        '""
        '"           determine the progress of the iteration."
        '""
        (setf nslow1 (f2cl-lib:int-add nslow1 1))
        (if (>= actred p001) (setf nslow1 0))
        (if jeval (setf nslow2 (f2cl-lib:int-add nslow2 1)))
        (if (>= actred p1) (setf nslow2 0))
        '""
        '"           test for convergence."
        '""
        (if (or (<= delta (* xtol xnorm)) (= fnorm zero)) (setf info 1))
        (if (/= info 0) (go label300))
        '""
        '"           tests for termination and stringent tolerances."
        '""
        (if (>= nfev maxfev) (setf info 2))
        (if (<= (* p1 (f2cl-lib:dmax1 (* p1 delta) pnorm)) (* epsmch xnorm))
            (setf info 3))
        (if (= nslow2 5) (setf info 4))
        (if (= nslow1 10) (setf info 5))
        (if (/= info 0) (go label300))
        '""
        '"           criterion for recalculating jacobian approximation"
        '"           by forward differences."
        '""
        (if (= ncfail 2) (go label290))
        '""
        '"           calculate the rank one modification to the jacobian"
        '"           and update qtf if necessary."
        '""
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j n) nil)
          (tagbody
            (setf sum zero)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
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
                                           ((1 n))
                                           wa4-%offset%))))
               label270))
            (setf (f2cl-lib:fref wa2-%data% (j) ((1 n)) wa2-%offset%)
                    (/
                     (- sum
                        (f2cl-lib:fref wa3-%data% (j) ((1 n)) wa3-%offset%))
                     pnorm))
            (setf (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                    (* (f2cl-lib:fref diag-%data% (j) ((1 n)) diag-%offset%)
                       (/
                        (*
                         (f2cl-lib:fref diag-%data% (j) ((1 n)) diag-%offset%)
                         (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%))
                        pnorm)))
            (if (>= ratio p0001)
                (setf (f2cl-lib:fref qtf-%data% (j) ((1 n)) qtf-%offset%) sum))
           label280))
        '""
        '"           compute the qr factorization of the updated jacobian."
        '""
        (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
            (r1updt n n r lr wa1 wa2 wa3 sing)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
          (setf sing var-7))
        (r1mpyq n n fjac ldfjac wa2 wa3)
        (r1mpyq 1 n qtf 1 wa2 wa3)
        '""
        '"           end of the inner loop."
        '""
        (setf jeval f2cl-lib:%false%)
        (go label180)
       label290
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
            (multiple-value-bind (var-0 var-1 var-2 var-3)
                (funcall fcn n x fvec iflag)
              (declare (ignore var-1 var-2))
              (when var-0
                (setf n var-0))
              (when var-3
                (setf iflag var-3))))
        (go end_label)
        '""
        '"     last card of subroutine hybrd."
        '""
       end_label
        (return
         (values nil
                 n
                 nil
                 nil
                 nil
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
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::hybrd fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (double-float)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (double-float) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)))
           :return-values '(nil fortran-to-lisp::n nil nil nil nil nil nil nil
                            nil nil nil nil fortran-to-lisp::info
                            fortran-to-lisp::nfev nil nil nil nil nil nil nil
                            nil nil)
           :calls '(fortran-to-lisp::r1mpyq fortran-to-lisp::r1updt
                    fortran-to-lisp::dogleg fortran-to-lisp::qform
                    fortran-to-lisp::qrfac fortran-to-lisp::fdjac1
                    fortran-to-lisp::enorm fortran-to-lisp::dpmpar))))

