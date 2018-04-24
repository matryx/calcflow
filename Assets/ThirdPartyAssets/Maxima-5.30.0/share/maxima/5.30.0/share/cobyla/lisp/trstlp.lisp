;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.222 2010/10/08 03:05:30 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.204 2010/02/23 05:21:30 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.114 2010/05/17 01:42:14 rtoy Exp $")

;;; Using Lisp CMU Common Lisp CVS Head 2010-09-27 22:45:24 (20B Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :cobyla)


(defun trstlp (n m a b rho dx ifull iact z zdota vmultc sdirn dxnew vmultd)
  (declare (type (array f2cl-lib:integer4 (*)) iact)
           (type double-float rho)
           (type (array double-float (*)) vmultd dxnew sdirn vmultc zdota z dx
                                          b a)
           (type (f2cl-lib:integer4) ifull m n))
  (f2cl-lib:with-multi-array-data
      ((a double-float a-%data% a-%offset%)
       (b double-float b-%data% b-%offset%)
       (dx double-float dx-%data% dx-%offset%)
       (z double-float z-%data% z-%offset%)
       (zdota double-float zdota-%data% zdota-%offset%)
       (vmultc double-float vmultc-%data% vmultc-%offset%)
       (sdirn double-float sdirn-%data% sdirn-%offset%)
       (dxnew double-float dxnew-%data% dxnew-%offset%)
       (vmultd double-float vmultd-%data% vmultd-%offset%)
       (iact f2cl-lib:integer4 iact-%data% iact-%offset%))
    (prog ((sumabs 0.0) (sum 0.0) (kl 0) (zdwabs 0.0) (zdotw 0.0) (resold 0.0)
           (step$ 0.0) (stpful 0.0) (ss 0.0) (sd 0.0) (dd 0.0) (vsave 0.0)
           (isave 0) (kw 0) (iout 0) (tempa 0.0) (zdvabs 0.0) (zdotv 0.0)
           (ratio 0.0) (beta 0.0) (alpha 0.0) (kp 0) (accb 0.0) (acca 0.0)
           (temp 0.0) (spabs 0.0) (sp 0.0) (tot 0.0) (kk 0) (nactx 0)
           (optnew 0.0) (icount 0) (optold 0.0) (icon 0) (k 0) (j 0) (i 0)
           (resmax 0.0) (nact 0) (mcon 0))
      (declare (type (f2cl-lib:integer4) mcon nact i j k icon icount nactx kk
                                         kp iout kw isave kl)
               (type double-float resmax optold optnew tot sp spabs temp acca
                                  accb alpha beta ratio zdotv zdvabs tempa
                                  vsave dd sd ss stpful step$ resold zdotw
                                  zdwabs sum sumabs))
      '""
      '"     this subroutine calculates an n-component vector dx by applying th"
      '"     following two stages. in the first stage, dx is set to the shortes"
      '"     vector that minimizes the greatest violation of the constraints"
      '"       a(1,k)*dx(1)+a(2,k)*dx(2)+...+a(n,k)*dx(n) .ge. b(k), k=2,3,...,"
      '"     subject to the euclidean length of dx being at most rho. if its le"
      '"     strictly less than rho, then we use the resultant freedom in dx to"
      '"     minimize the objective function"
      '"              -a(1,m+1)*dx(1)-a(2,m+1)*dx(2)-...-a(n,m+1)*dx(n)"
      '"     subject to no increase in any greatest constraint violation. this"
      '"     notation allows the gradient of the objective function to be regar"
      '"     the gradient of a constraint. therefore the two stages are disting"
      '"     by mcon .eq. m and mcon .gt. m respectively. it is possible that a"
      '"     degeneracy may prevent dx from attaining the target length rho. th"
      '"     value ifull=0 would be set, but usually ifull=1 on return."
      '""
      '"     in general nact is the number of constraints in the active set and"
      '"     iact(1),...,iact(nact) are their indices, while the remainder of i"
      '"     contains a permutation of the remaining constraint indices. furthe"
      '"     an orthogonal matrix whose first nact columns can be regarded as t"
      '"     result of gram-schmidt applied to the active constraint gradients."
      '"     j=1,2,...,nact, the number zdota(j) is the scalar product of the j"
      '"     column of z with the gradient of the j-th active constraint. dx is"
      '"     current vector of variables and here the residuals of the active"
      '"     constraints should be zero. further, the active constraints have"
      '"     nonnegative lagrange multipliers that are held at the beginning of"
      '"     vmultc. the remainder of this vector holds the residuals of the in"
      '"     constraints at dx, the ordering of the components of vmultc being"
      '"     agreement with the permutation of the indices of the constraints t"
      '"     in iact. all these residuals are nonnegative, which is achieved by"
      '"     shift resmax that makes the least residual zero."
      '""
      '"     initialize z and some other variables. the value of resmax will be"
      '"     appropriate to dx=0, while icon will be the index of a most violat"
      '"     constraint if resmax is positive. usually during the first stage t"
      '"     vector sdirn gives a search direction that reduces all the active"
      '"     constraint violations by one simultaneously."
      '""
      (setf ifull 1)
      (setf mcon m)
      (setf nact 0)
      (setf resmax 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
             label10
              (setf (f2cl-lib:fref z-%data% (i j) ((1 n) (1 *)) z-%offset%)
                      0.0)))
          (setf (f2cl-lib:fref z-%data% (i i) ((1 n) (1 *)) z-%offset%) 1.0)
         label20
          (setf (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%) 0.0)))
      (cond
        ((>= m 1)
         (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                       ((> k m) nil)
           (tagbody
             (cond
               ((> (f2cl-lib:fref b (k) ((1 *))) resmax)
                (setf resmax (f2cl-lib:fref b-%data% (k) ((1 *)) b-%offset%))
                (setf icon k)))
            label30))
         (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                       ((> k m) nil)
           (tagbody
             (setf (f2cl-lib:fref iact-%data% (k) ((1 *)) iact-%offset%) k)
            label40
             (setf (f2cl-lib:fref vmultc-%data% (k) ((1 *)) vmultc-%offset%)
                     (- resmax
                        (f2cl-lib:fref b-%data% (k) ((1 *)) b-%offset%)))))))
      (if (= resmax 0.0) (go label480))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label50
          (setf (f2cl-lib:fref sdirn-%data% (i) ((1 *)) sdirn-%offset%) 0.0)))
      '""
      '"     end the current stage of the calculation if 3 consecutive iteratio"
      '"     have either failed to reduce the best calculated value of the obje"
      '"     function or to increase the number of active constraints since the"
      '"     value was calculated. this strategy prevents cycling, but there is"
      '"     remote possibility that it will cause premature termination."
      '""
     label60
      (setf optold 0.0)
      (setf icount 0)
     label70
      (cond
        ((= mcon m)
         (setf optnew resmax))
        (t
         (setf optnew 0.0)
         (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                       ((> i n) nil)
           (tagbody
            label80
             (setf optnew
                     (- optnew
                        (* (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                           (f2cl-lib:fref a-%data%
                                          (i mcon)
                                          ((1 n) (1 *))
                                          a-%offset%))))))))
      (cond
        ((or (= icount 0) (< optnew optold))
         (setf optold optnew)
         (setf nactx nact)
         (setf icount 3))
        ((> nact nactx)
         (setf nactx nact)
         (setf icount 3))
        (t
         (setf icount (f2cl-lib:int-sub icount 1))
         (if (= icount 0) (go label490))))
      '""
      '"     if icon exceeds nact, then we add the constraint with index iact(i"
      '"     the active set. apply givens rotations so that the last n-nact-1 c"
      '"     of z are orthogonal to the gradient of the new constraint, a scala"
      '"     product being set to zero if its nonzero value could be due to com"
      '"     rounding errors. the array dxnew is used for working space."
      '""
      (if (<= icon nact) (go label260))
      (setf kk (f2cl-lib:fref iact-%data% (icon) ((1 *)) iact-%offset%))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label90
          (setf (f2cl-lib:fref dxnew-%data% (i) ((1 *)) dxnew-%offset%)
                  (f2cl-lib:fref a-%data% (i kk) ((1 n) (1 *)) a-%offset%))))
      (setf tot 0.0)
      (setf k n)
     label100
      (cond
        ((> k nact)
         (setf sp 0.0)
         (setf spabs 0.0)
         (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                       ((> i n) nil)
           (tagbody
             (setf temp
                     (* (f2cl-lib:fref z-%data% (i k) ((1 n) (1 *)) z-%offset%)
                        (f2cl-lib:fref dxnew-%data%
                                       (i)
                                       ((1 *))
                                       dxnew-%offset%)))
             (setf sp (+ sp temp))
            label110
             (setf spabs (+ spabs (abs temp)))))
         (setf acca (+ spabs (* 0.1 (abs sp))))
         (setf accb (+ spabs (* 0.2 (abs sp))))
         (if (or (>= spabs acca) (>= acca accb)) (setf sp 0.0))
         (cond
           ((= tot 0.0)
            (setf tot sp))
           (t
            (setf kp (f2cl-lib:int-add k 1))
            (setf temp (f2cl-lib:fsqrt (+ (* sp sp) (* tot tot))))
            (setf alpha (/ sp temp))
            (setf beta (/ tot temp))
            (setf tot temp)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf temp
                        (+
                         (* alpha
                            (f2cl-lib:fref z-%data%
                                           (i k)
                                           ((1 n) (1 *))
                                           z-%offset%))
                         (* beta
                            (f2cl-lib:fref z-%data%
                                           (i kp)
                                           ((1 n) (1 *))
                                           z-%offset%))))
                (setf (f2cl-lib:fref z-%data% (i kp) ((1 n) (1 *)) z-%offset%)
                        (-
                         (* alpha
                            (f2cl-lib:fref z-%data%
                                           (i kp)
                                           ((1 n) (1 *))
                                           z-%offset%))
                         (* beta
                            (f2cl-lib:fref z-%data%
                                           (i k)
                                           ((1 n) (1 *))
                                           z-%offset%))))
               label120
                (setf (f2cl-lib:fref z-%data% (i k) ((1 n) (1 *)) z-%offset%)
                        temp)))))
         (setf k (f2cl-lib:int-sub k 1))
         (go label100)))
      '""
      '"     add the new constraint if this can be done without a deletion from"
      '"     active set."
      '""
      (cond
        ((/= tot 0.0)
         (setf nact (f2cl-lib:int-add nact 1))
         (setf (f2cl-lib:fref zdota-%data% (nact) ((1 *)) zdota-%offset%) tot)
         (setf (f2cl-lib:fref vmultc-%data% (icon) ((1 *)) vmultc-%offset%)
                 (f2cl-lib:fref vmultc-%data% (nact) ((1 *)) vmultc-%offset%))
         (setf (f2cl-lib:fref vmultc-%data% (nact) ((1 *)) vmultc-%offset%) 0.0)
         (go label210)))
      '""
      '"     the next instruction is reached if a deletion has to be made from"
      '"     active set in order to make room for the new active constraint, be"
      '"     the new constraint gradient is a linear combination of the gradien"
      '"     the old active constraints. set the elements of vmultd to the mult"
      '"     of the linear combination. further, set iout to the index of the"
      '"     constraint to be deleted, but branch if no suitable index can be f"
      '""
      (setf ratio -1.0)
      (setf k nact)
     label130
      (setf zdotv 0.0)
      (setf zdvabs 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf temp
                  (* (f2cl-lib:fref z-%data% (i k) ((1 n) (1 *)) z-%offset%)
                     (f2cl-lib:fref dxnew-%data% (i) ((1 *)) dxnew-%offset%)))
          (setf zdotv (+ zdotv temp))
         label140
          (setf zdvabs (+ zdvabs (abs temp)))))
      (setf acca (+ zdvabs (* 0.1 (abs zdotv))))
      (setf accb (+ zdvabs (* 0.2 (abs zdotv))))
      (cond
        ((and (< zdvabs acca) (< acca accb))
         (setf temp
                 (/ zdotv
                    (f2cl-lib:fref zdota-%data% (k) ((1 *)) zdota-%offset%)))
         (cond
           ((and (> temp 0.0) (<= (f2cl-lib:fref iact (k) ((1 *))) m))
            (setf tempa
                    (/
                     (f2cl-lib:fref vmultc-%data% (k) ((1 *)) vmultc-%offset%)
                     temp))
            (cond
              ((or (< ratio 0.0) (< tempa ratio))
               (setf ratio tempa)
               (setf iout k)))))
         (cond
           ((>= k 2)
            (setf kw (f2cl-lib:fref iact-%data% (k) ((1 *)) iact-%offset%))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label150
                (setf (f2cl-lib:fref dxnew-%data% (i) ((1 *)) dxnew-%offset%)
                        (-
                         (f2cl-lib:fref dxnew-%data%
                                        (i)
                                        ((1 *))
                                        dxnew-%offset%)
                         (* temp
                            (f2cl-lib:fref a-%data%
                                           (i kw)
                                           ((1 n) (1 *))
                                           a-%offset%))))))))
         (setf (f2cl-lib:fref vmultd-%data% (k) ((1 *)) vmultd-%offset%) temp))
        (t
         (setf (f2cl-lib:fref vmultd-%data% (k) ((1 *)) vmultd-%offset%) 0.0)))
      (setf k (f2cl-lib:int-sub k 1))
      (if (> k 0) (go label130))
      (if (< ratio 0.0) (go label490))
      '""
      '"     revise the lagrange multipliers and reorder the active constraints"
      '"     that the one to be replaced is at the end of the list. also calcul"
      '"     new value of zdota(nact) and branch if it is not acceptable."
      '""
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k nact) nil)
        (tagbody
         label160
          (setf (f2cl-lib:fref vmultc-%data% (k) ((1 *)) vmultc-%offset%)
                  (f2cl-lib:dmax1 0.0
                                  (-
                                   (f2cl-lib:fref vmultc-%data%
                                                  (k)
                                                  ((1 *))
                                                  vmultc-%offset%)
                                   (* ratio
                                      (f2cl-lib:fref vmultd-%data%
                                                     (k)
                                                     ((1 *))
                                                     vmultd-%offset%)))))))
      (cond
        ((< icon nact)
         (tagbody
           (setf isave
                   (f2cl-lib:fref iact-%data% (icon) ((1 *)) iact-%offset%))
           (setf vsave
                   (f2cl-lib:fref vmultc-%data%
                                  (icon)
                                  ((1 *))
                                  vmultc-%offset%))
           (setf k icon)
          label170
           (setf kp (f2cl-lib:int-add k 1))
           (setf kw (f2cl-lib:fref iact-%data% (kp) ((1 *)) iact-%offset%))
           (setf sp 0.0)
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i n) nil)
             (tagbody
              label180
               (setf sp
                       (+ sp
                          (*
                           (f2cl-lib:fref z-%data%
                                          (i k)
                                          ((1 n) (1 *))
                                          z-%offset%)
                           (f2cl-lib:fref a-%data%
                                          (i kw)
                                          ((1 n) (1 *))
                                          a-%offset%))))))
           (setf temp
                   (f2cl-lib:fsqrt
                    (+ (* sp sp)
                       (expt
                        (f2cl-lib:fref zdota-%data%
                                       (kp)
                                       ((1 *))
                                       zdota-%offset%)
                        2))))
           (setf alpha
                   (/ (f2cl-lib:fref zdota-%data% (kp) ((1 *)) zdota-%offset%)
                      temp))
           (setf beta (/ sp temp))
           (setf (f2cl-lib:fref zdota-%data% (kp) ((1 *)) zdota-%offset%)
                   (* alpha
                      (f2cl-lib:fref zdota-%data% (k) ((1 *)) zdota-%offset%)))
           (setf (f2cl-lib:fref zdota-%data% (k) ((1 *)) zdota-%offset%) temp)
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i n) nil)
             (tagbody
               (setf temp
                       (+
                        (* alpha
                           (f2cl-lib:fref z-%data%
                                          (i kp)
                                          ((1 n) (1 *))
                                          z-%offset%))
                        (* beta
                           (f2cl-lib:fref z-%data%
                                          (i k)
                                          ((1 n) (1 *))
                                          z-%offset%))))
               (setf (f2cl-lib:fref z-%data% (i kp) ((1 n) (1 *)) z-%offset%)
                       (-
                        (* alpha
                           (f2cl-lib:fref z-%data%
                                          (i k)
                                          ((1 n) (1 *))
                                          z-%offset%))
                        (* beta
                           (f2cl-lib:fref z-%data%
                                          (i kp)
                                          ((1 n) (1 *))
                                          z-%offset%))))
              label190
               (setf (f2cl-lib:fref z-%data% (i k) ((1 n) (1 *)) z-%offset%)
                       temp)))
           (setf (f2cl-lib:fref iact-%data% (k) ((1 *)) iact-%offset%) kw)
           (setf (f2cl-lib:fref vmultc-%data% (k) ((1 *)) vmultc-%offset%)
                   (f2cl-lib:fref vmultc-%data% (kp) ((1 *)) vmultc-%offset%))
           (setf k kp)
           (if (< k nact) (go label170))
           (setf (f2cl-lib:fref iact-%data% (k) ((1 *)) iact-%offset%) isave)
           (setf (f2cl-lib:fref vmultc-%data% (k) ((1 *)) vmultc-%offset%)
                   vsave))))
      (setf temp 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label200
          (setf temp
                  (+ temp
                     (*
                      (f2cl-lib:fref z-%data%
                                     (i nact)
                                     ((1 n) (1 *))
                                     z-%offset%)
                      (f2cl-lib:fref a-%data%
                                     (i kk)
                                     ((1 n) (1 *))
                                     a-%offset%))))))
      (if (= temp 0.0) (go label490))
      (setf (f2cl-lib:fref zdota-%data% (nact) ((1 *)) zdota-%offset%) temp)
      (setf (f2cl-lib:fref vmultc-%data% (icon) ((1 *)) vmultc-%offset%) 0.0)
      (setf (f2cl-lib:fref vmultc-%data% (nact) ((1 *)) vmultc-%offset%) ratio)
      '""
      '"     update iact and ensure that the objective function continues to be"
      '"     treated as the last active constraint when mcon>m."
      '""
     label210
      (setf (f2cl-lib:fref iact-%data% (icon) ((1 *)) iact-%offset%)
              (f2cl-lib:fref iact-%data% (nact) ((1 *)) iact-%offset%))
      (setf (f2cl-lib:fref iact-%data% (nact) ((1 *)) iact-%offset%) kk)
      (cond
        ((and (> mcon m) (/= kk mcon))
         (setf k (f2cl-lib:int-sub nact 1))
         (setf sp 0.0)
         (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                       ((> i n) nil)
           (tagbody
            label220
             (setf sp
                     (+ sp
                        (*
                         (f2cl-lib:fref z-%data%
                                        (i k)
                                        ((1 n) (1 *))
                                        z-%offset%)
                         (f2cl-lib:fref a-%data%
                                        (i kk)
                                        ((1 n) (1 *))
                                        a-%offset%))))))
         (setf temp
                 (f2cl-lib:fsqrt
                  (+ (* sp sp)
                     (expt
                      (f2cl-lib:fref zdota-%data%
                                     (nact)
                                     ((1 *))
                                     zdota-%offset%)
                      2))))
         (setf alpha
                 (/ (f2cl-lib:fref zdota-%data% (nact) ((1 *)) zdota-%offset%)
                    temp))
         (setf beta (/ sp temp))
         (setf (f2cl-lib:fref zdota-%data% (nact) ((1 *)) zdota-%offset%)
                 (* alpha
                    (f2cl-lib:fref zdota-%data% (k) ((1 *)) zdota-%offset%)))
         (setf (f2cl-lib:fref zdota-%data% (k) ((1 *)) zdota-%offset%) temp)
         (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                       ((> i n) nil)
           (tagbody
             (setf temp
                     (+
                      (* alpha
                         (f2cl-lib:fref z-%data%
                                        (i nact)
                                        ((1 n) (1 *))
                                        z-%offset%))
                      (* beta
                         (f2cl-lib:fref z-%data%
                                        (i k)
                                        ((1 n) (1 *))
                                        z-%offset%))))
             (setf (f2cl-lib:fref z-%data% (i nact) ((1 n) (1 *)) z-%offset%)
                     (-
                      (* alpha
                         (f2cl-lib:fref z-%data%
                                        (i k)
                                        ((1 n) (1 *))
                                        z-%offset%))
                      (* beta
                         (f2cl-lib:fref z-%data%
                                        (i nact)
                                        ((1 n) (1 *))
                                        z-%offset%))))
            label230
             (setf (f2cl-lib:fref z-%data% (i k) ((1 n) (1 *)) z-%offset%)
                     temp)))
         (setf (f2cl-lib:fref iact-%data% (nact) ((1 *)) iact-%offset%)
                 (f2cl-lib:fref iact-%data% (k) ((1 *)) iact-%offset%))
         (setf (f2cl-lib:fref iact-%data% (k) ((1 *)) iact-%offset%) kk)
         (setf temp (f2cl-lib:fref vmultc-%data% (k) ((1 *)) vmultc-%offset%))
         (setf (f2cl-lib:fref vmultc-%data% (k) ((1 *)) vmultc-%offset%)
                 (f2cl-lib:fref vmultc-%data% (nact) ((1 *)) vmultc-%offset%))
         (setf (f2cl-lib:fref vmultc-%data% (nact) ((1 *)) vmultc-%offset%)
                 temp)))
      '""
      '"     if stage one is in progress, then set sdirn to the direction of th"
      '"     change to the current vector of variables."
      '""
      (if (> mcon m) (go label320))
      (setf kk (f2cl-lib:fref iact-%data% (nact) ((1 *)) iact-%offset%))
      (setf temp 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label240
          (setf temp
                  (+ temp
                     (* (f2cl-lib:fref sdirn-%data% (i) ((1 *)) sdirn-%offset%)
                        (f2cl-lib:fref a-%data%
                                       (i kk)
                                       ((1 n) (1 *))
                                       a-%offset%))))))
      (setf temp (- temp 1.0))
      (setf temp
              (/ temp
                 (f2cl-lib:fref zdota-%data% (nact) ((1 *)) zdota-%offset%)))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label250
          (setf (f2cl-lib:fref sdirn-%data% (i) ((1 *)) sdirn-%offset%)
                  (- (f2cl-lib:fref sdirn-%data% (i) ((1 *)) sdirn-%offset%)
                     (* temp
                        (f2cl-lib:fref z-%data%
                                       (i nact)
                                       ((1 n) (1 *))
                                       z-%offset%))))))
      (go label340)
      '""
      '"     delete the constraint that has the index iact(icon) from the activ"
      '""
     label260
      (cond
        ((< icon nact)
         (tagbody
           (setf isave
                   (f2cl-lib:fref iact-%data% (icon) ((1 *)) iact-%offset%))
           (setf vsave
                   (f2cl-lib:fref vmultc-%data%
                                  (icon)
                                  ((1 *))
                                  vmultc-%offset%))
           (setf k icon)
          label270
           (setf kp (f2cl-lib:int-add k 1))
           (setf kk (f2cl-lib:fref iact-%data% (kp) ((1 *)) iact-%offset%))
           (setf sp 0.0)
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i n) nil)
             (tagbody
              label280
               (setf sp
                       (+ sp
                          (*
                           (f2cl-lib:fref z-%data%
                                          (i k)
                                          ((1 n) (1 *))
                                          z-%offset%)
                           (f2cl-lib:fref a-%data%
                                          (i kk)
                                          ((1 n) (1 *))
                                          a-%offset%))))))
           (setf temp
                   (f2cl-lib:fsqrt
                    (+ (* sp sp)
                       (expt
                        (f2cl-lib:fref zdota-%data%
                                       (kp)
                                       ((1 *))
                                       zdota-%offset%)
                        2))))
           (setf alpha
                   (/ (f2cl-lib:fref zdota-%data% (kp) ((1 *)) zdota-%offset%)
                      temp))
           (setf beta (/ sp temp))
           (setf (f2cl-lib:fref zdota-%data% (kp) ((1 *)) zdota-%offset%)
                   (* alpha
                      (f2cl-lib:fref zdota-%data% (k) ((1 *)) zdota-%offset%)))
           (setf (f2cl-lib:fref zdota-%data% (k) ((1 *)) zdota-%offset%) temp)
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i n) nil)
             (tagbody
               (setf temp
                       (+
                        (* alpha
                           (f2cl-lib:fref z-%data%
                                          (i kp)
                                          ((1 n) (1 *))
                                          z-%offset%))
                        (* beta
                           (f2cl-lib:fref z-%data%
                                          (i k)
                                          ((1 n) (1 *))
                                          z-%offset%))))
               (setf (f2cl-lib:fref z-%data% (i kp) ((1 n) (1 *)) z-%offset%)
                       (-
                        (* alpha
                           (f2cl-lib:fref z-%data%
                                          (i k)
                                          ((1 n) (1 *))
                                          z-%offset%))
                        (* beta
                           (f2cl-lib:fref z-%data%
                                          (i kp)
                                          ((1 n) (1 *))
                                          z-%offset%))))
              label290
               (setf (f2cl-lib:fref z-%data% (i k) ((1 n) (1 *)) z-%offset%)
                       temp)))
           (setf (f2cl-lib:fref iact-%data% (k) ((1 *)) iact-%offset%) kk)
           (setf (f2cl-lib:fref vmultc-%data% (k) ((1 *)) vmultc-%offset%)
                   (f2cl-lib:fref vmultc-%data% (kp) ((1 *)) vmultc-%offset%))
           (setf k kp)
           (if (< k nact) (go label270))
           (setf (f2cl-lib:fref iact-%data% (k) ((1 *)) iact-%offset%) isave)
           (setf (f2cl-lib:fref vmultc-%data% (k) ((1 *)) vmultc-%offset%)
                   vsave))))
      (setf nact (f2cl-lib:int-sub nact 1))
      '""
      '"     if stage one is in progress, then set sdirn to the direction of th"
      '"     change to the current vector of variables."
      '""
      (if (> mcon m) (go label320))
      (setf temp 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label300
          (setf temp
                  (+ temp
                     (* (f2cl-lib:fref sdirn-%data% (i) ((1 *)) sdirn-%offset%)
                        (f2cl-lib:fref z-%data%
                                       (i (f2cl-lib:int-add nact 1))
                                       ((1 n) (1 *))
                                       z-%offset%))))))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label310
          (setf (f2cl-lib:fref sdirn-%data% (i) ((1 *)) sdirn-%offset%)
                  (- (f2cl-lib:fref sdirn-%data% (i) ((1 *)) sdirn-%offset%)
                     (* temp
                        (f2cl-lib:fref z-%data%
                                       (i (f2cl-lib:int-add nact 1))
                                       ((1 n) (1 *))
                                       z-%offset%))))))
      (go label340)
      '""
      '"     pick the next search direction of stage two."
      '""
     label320
      (setf temp
              (/ 1.0
                 (f2cl-lib:fref zdota-%data% (nact) ((1 *)) zdota-%offset%)))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label330
          (setf (f2cl-lib:fref sdirn-%data% (i) ((1 *)) sdirn-%offset%)
                  (* temp
                     (f2cl-lib:fref z-%data%
                                    (i nact)
                                    ((1 n) (1 *))
                                    z-%offset%)))))
      '""
      '"     calculate the step to the boundary of the trust region or take the"
      '"     that reduces resmax to zero. the two statements below that include"
      '"     factor 1.0e-6 prevent some harmless underflows that occurred in a"
      '"     calculation. further, we skip the step if it could be zero within"
      '"     reasonable tolerance for computer rounding errors."
      '""
     label340
      (setf dd (* rho rho))
      (setf sd 0.0)
      (setf ss 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (if
           (>= (abs (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))
               (* 1.0f-6 rho))
           (setf dd
                   (- dd
                      (expt (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                            2))))
          (setf sd
                  (+ sd
                     (* (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                        (f2cl-lib:fref sdirn-%data%
                                       (i)
                                       ((1 *))
                                       sdirn-%offset%))))
         label350
          (setf ss
                  (+ ss
                     (expt
                      (f2cl-lib:fref sdirn-%data% (i) ((1 *)) sdirn-%offset%)
                      2)))))
      (if (<= dd 0.0) (go label490))
      (setf temp (f2cl-lib:fsqrt (* ss dd)))
      (if (>= (abs sd) (* 1.0f-6 temp))
          (setf temp (f2cl-lib:fsqrt (+ (* ss dd) (* sd sd)))))
      (setf stpful (/ dd (+ temp sd)))
      (setf step$ stpful)
      (cond
        ((= mcon m)
         (setf acca (+ step$ (* 0.1 resmax)))
         (setf accb (+ step$ (* 0.2 resmax)))
         (if (or (>= step$ acca) (>= acca accb)) (go label480))
         (setf step$ (f2cl-lib:dmin1 step$ resmax))))
      '""
      '"     set dxnew to the new variables if step is the steplength, and redu"
      '"     resmax to the corresponding maximum residual if stage one is being"
      '"     because dxnew will be changed during the calculation of some lagra"
      '"     multipliers, it will be restored to the following value later."
      '""
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label360
          (setf (f2cl-lib:fref dxnew-%data% (i) ((1 *)) dxnew-%offset%)
                  (+ (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                     (* step$
                        (f2cl-lib:fref sdirn-%data%
                                       (i)
                                       ((1 *))
                                       sdirn-%offset%))))))
      (cond
        ((= mcon m)
         (setf resold resmax)
         (setf resmax 0.0)
         (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                       ((> k nact) nil)
           (tagbody
             (setf kk (f2cl-lib:fref iact-%data% (k) ((1 *)) iact-%offset%))
             (setf temp (f2cl-lib:fref b-%data% (kk) ((1 *)) b-%offset%))
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i n) nil)
               (tagbody
                label370
                 (setf temp
                         (- temp
                            (*
                             (f2cl-lib:fref a-%data%
                                            (i kk)
                                            ((1 n) (1 *))
                                            a-%offset%)
                             (f2cl-lib:fref dxnew-%data%
                                            (i)
                                            ((1 *))
                                            dxnew-%offset%))))))
             (setf resmax (f2cl-lib:dmax1 resmax temp))
            label380))))
      '""
      '"     set vmultd to the vmultc vector that would occur if dx became dxne"
      '"     device is included to force vmultd(k)=0.0 if deviations from this"
      '"     can be attributed to computer rounding errors. first calculate the"
      '"     lagrange multipliers."
      '""
      (setf k nact)
     label390
      (setf zdotw 0.0)
      (setf zdwabs 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf temp
                  (* (f2cl-lib:fref z-%data% (i k) ((1 n) (1 *)) z-%offset%)
                     (f2cl-lib:fref dxnew-%data% (i) ((1 *)) dxnew-%offset%)))
          (setf zdotw (+ zdotw temp))
         label400
          (setf zdwabs (+ zdwabs (abs temp)))))
      (setf acca (+ zdwabs (* 0.1 (abs zdotw))))
      (setf accb (+ zdwabs (* 0.2 (abs zdotw))))
      (if (or (>= zdwabs acca) (>= acca accb)) (setf zdotw 0.0))
      (setf (f2cl-lib:fref vmultd-%data% (k) ((1 *)) vmultd-%offset%)
              (/ zdotw
                 (f2cl-lib:fref zdota-%data% (k) ((1 *)) zdota-%offset%)))
      (cond
        ((>= k 2)
         (setf kk (f2cl-lib:fref iact-%data% (k) ((1 *)) iact-%offset%))
         (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                       ((> i n) nil)
           (tagbody
            label410
             (setf (f2cl-lib:fref dxnew-%data% (i) ((1 *)) dxnew-%offset%)
                     (- (f2cl-lib:fref dxnew-%data% (i) ((1 *)) dxnew-%offset%)
                        (*
                         (f2cl-lib:fref vmultd-%data%
                                        (k)
                                        ((1 *))
                                        vmultd-%offset%)
                         (f2cl-lib:fref a-%data%
                                        (i kk)
                                        ((1 n) (1 *))
                                        a-%offset%))))))
         (setf k (f2cl-lib:int-sub k 1))
         (go label390)))
      (if (> mcon m)
          (setf (f2cl-lib:fref vmultd-%data% (nact) ((1 *)) vmultd-%offset%)
                  (f2cl-lib:dmax1 0.0
                                  (f2cl-lib:fref vmultd-%data%
                                                 (nact)
                                                 ((1 *))
                                                 vmultd-%offset%))))
      '""
      '"     complete vmultc by finding the new constraint residuals."
      '""
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label420
          (setf (f2cl-lib:fref dxnew-%data% (i) ((1 *)) dxnew-%offset%)
                  (+ (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                     (* step$
                        (f2cl-lib:fref sdirn-%data%
                                       (i)
                                       ((1 *))
                                       sdirn-%offset%))))))
      (cond
        ((> mcon nact)
         (setf kl (f2cl-lib:int-add nact 1))
         (f2cl-lib:fdo (k kl (f2cl-lib:int-add k 1))
                       ((> k mcon) nil)
           (tagbody
             (setf kk (f2cl-lib:fref iact-%data% (k) ((1 *)) iact-%offset%))
             (setf sum
                     (- resmax
                        (f2cl-lib:fref b-%data% (kk) ((1 *)) b-%offset%)))
             (setf sumabs
                     (+ resmax
                        (abs
                         (f2cl-lib:fref b-%data% (kk) ((1 *)) b-%offset%))))
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i n) nil)
               (tagbody
                 (setf temp
                         (*
                          (f2cl-lib:fref a-%data%
                                         (i kk)
                                         ((1 n) (1 *))
                                         a-%offset%)
                          (f2cl-lib:fref dxnew-%data%
                                         (i)
                                         ((1 *))
                                         dxnew-%offset%)))
                 (setf sum (+ sum temp))
                label430
                 (setf sumabs (+ sumabs (abs temp)))))
             (setf acca (+ sumabs (* 0.1f0 (abs sum))))
             (setf accb (+ sumabs (* 0.2f0 (abs sum))))
             (if (or (>= sumabs acca) (>= acca accb))
                 (setf sum (coerce 0.0f0 'double-float)))
            label440
             (setf (f2cl-lib:fref vmultd-%data% (k) ((1 *)) vmultd-%offset%)
                     sum)))))
      '""
      '"     calculate the fraction of the step from dx to dxnew that will be t"
      '""
      (setf ratio 1.0)
      (setf icon 0)
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k mcon) nil)
        (tagbody
          (cond
            ((< (f2cl-lib:fref vmultd (k) ((1 *))) 0.0)
             (setf temp
                     (/
                      (f2cl-lib:fref vmultc-%data% (k) ((1 *)) vmultc-%offset%)
                      (-
                       (f2cl-lib:fref vmultc-%data%
                                      (k)
                                      ((1 *))
                                      vmultc-%offset%)
                       (f2cl-lib:fref vmultd-%data%
                                      (k)
                                      ((1 *))
                                      vmultd-%offset%))))
             (cond
               ((< temp ratio)
                (setf ratio temp)
                (setf icon k)))))
         label450))
      '""
      '"     update dx, vmultc and resmax."
      '""
      (setf temp (- 1.0 ratio))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label460
          (setf (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                  (+ (* temp (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))
                     (* ratio
                        (f2cl-lib:fref dxnew-%data%
                                       (i)
                                       ((1 *))
                                       dxnew-%offset%))))))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k mcon) nil)
        (tagbody
         label470
          (setf (f2cl-lib:fref vmultc-%data% (k) ((1 *)) vmultc-%offset%)
                  (f2cl-lib:dmax1 0.0
                                  (+
                                   (* temp
                                      (f2cl-lib:fref vmultc-%data%
                                                     (k)
                                                     ((1 *))
                                                     vmultc-%offset%))
                                   (* ratio
                                      (f2cl-lib:fref vmultd-%data%
                                                     (k)
                                                     ((1 *))
                                                     vmultd-%offset%)))))))
      (if (= mcon m) (setf resmax (+ resold (* ratio (- resmax resold)))))
      '""
      '"     if the full step is not acceptable then begin another iteration."
      '"     otherwise switch to stage two or end the calculation."
      '""
      (if (> icon 0) (go label70))
      (if (= step$ stpful) (go label500))
     label480
      (setf mcon (f2cl-lib:int-add m 1))
      (setf icon mcon)
      (setf (f2cl-lib:fref iact-%data% (mcon) ((1 *)) iact-%offset%) mcon)
      (setf (f2cl-lib:fref vmultc-%data% (mcon) ((1 *)) vmultc-%offset%) 0.0)
      (go label60)
      '""
      '"     we employ any freedom that may be available to reduce the objectiv"
      '"     function before returning a dx whose length is less than rho."
      '""
     label490
      (if (= mcon m) (go label480))
      (setf ifull 0)
     label500
      (go end_label)
     end_label
      (return
       (values nil nil nil nil nil nil ifull nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::trstlp
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        double-float (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*)))
           :return-values '(nil nil nil nil nil nil fortran-to-lisp::ifull nil
                            nil nil nil nil nil nil)
           :calls 'nil)))

