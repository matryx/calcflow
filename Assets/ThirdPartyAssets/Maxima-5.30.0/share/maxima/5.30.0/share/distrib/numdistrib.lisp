;;                 COPYRIGHT NOTICE
;;  
;;  Copyright (C) 2005-2009 Mario Rodriguez Riotorto
;;  
;;  This program is free software; you can redistribute
;;  it and/or modify it under the terms of the
;;  GNU General Public License as published by
;;  the Free Software Foundation; either version 2 
;;  of the License, or (at your option) any later version. 
;;  
;;  This program is distributed in the hope that it
;;  will be useful, but WITHOUT ANY WARRANTY;
;;  without even the implied warranty of MERCHANTABILITY
;;  or FITNESS FOR A PARTICULAR PURPOSE. See the 
;;  GNU General Public License for more details at
;;  http://www.gnu.org/copyleft/gpl.html

;;  This is a set of numerical routines used by package distrib.mac

;; For questions, suggestions, bugs and the like, feel free
;; to contact me at
;; mario @@@ edu DOT xunta DOT es



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;;
;;         Incomplete gamma and beta       ;;
;;           and related functions         ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Natural logarithm of the gamma function. This function is called from
;; some numerical routines below.
(defun lngamma (p)
  (simplify (list '(%log_gamma) p)))


;;  Natural logarithm of the beta function
;;  Conditions: 0<p,q<1.0e302
(defun lnbeta (p q)
   (+ (lngamma p) (lngamma q) (- (lngamma (+ p q)))) )


;; Lower regularized gamma incomplete. This function is called from
;; some numerical routines below.
(defun igamma (x p)
  (- 1.0 (simplify (list '(%gamma_incomplete_regularized) p x))) )


;;  Returns incomplete beta regularized in float format.
;;  Conditions: 0<=x<=1; a, b>0
(defun ibeta (x a b &aux ($numer t))
   (declare (type flonum x a b))
   (simplify (list '(%beta_incomplete_regularized) a b x)))


;;  Inverse of the incomplete beta function.
;;  Reference:
;;     algorithm as 109 appl. statist. (1977), vol.26, no.1
;;  Comments: Translated from Fortran.
;;  Conditions: 0<x<1; p,q>0
(defun iibeta (x p q)
   (declare (type flonum x p q))
   (let (fpu a pp qq indx r y s tt h w beta
         yprev sq tx prev iex acu xin g adj xiibeta
         (sae -300.0) (zero 0.0) (one 1.0) (two 2.0) (three 3.0)
         (four 4.0) (five 5.0) (six 6.0))
        (setf beta (lnbeta p q)
              fpu (expt 10.0 sae)
              xiibeta x)

        ;; change tail if necessary
        (if (<= x 0.5)
            (setf a x pp p qq q indx nil)
            (setf a (- one x) pp q qq p indx t))

        ;; calculate the initial approximation
        (setf r (sqrt (- (log (* a a))))
              y (+ r
                 (-
                  (/ (+ 2.30753 (* 0.27061 r))
                   (+ one (* (+ 0.99229 (* 0.04481 r)) r))))))

        (cond ((and (> pp one) (> qq one)) 
                 (setf r (/ (+ (* y y) (- three)) six))
                 (setf s (/ one (+ pp pp (- one))))
                 (setf tt (/ one (+ qq qq (- one))))
                 (setf h (/ two (+ s tt)))
                 (setf w (+ (/ (* y (sqrt (+ h r))) h)
                          (-
                           (* (- tt  s)
                            (+ r (/ five six) (/ (- two) (* three h)))))))
                 (setf xiibeta (/ pp (+ pp (* qq (exp (+ w w)))))))
              (t (setf r (+ qq qq))
                 (setf tt (/ one (* 9.0 qq)))
                 (setf tt (* r (expt (+ one (- tt) (* y (sqrt tt))) 3)) )
                 (cond ((<= tt zero)
                          (setf xiibeta (- one (exp (/ (+  (log (* (- one a) qq)) beta) qq)))))
                       (t (setf tt (/ (+ (* four pp) r (- two)) tt))
                          (cond ((<= tt one)
                                   (setf xiibeta (exp (/ (+ (log (* a pp)) beta) pp))))
                                (t (setf xiibeta (+ one (- (/ two (+ tt one)))))))))))

        ;; solve for x by a modified newton-raphson method,
        ;; using the function ibeta
        (setf r (- one pp))
        (setf tt (- one qq))
        (setf yprev zero)
        (setf sq one)
        (setf prev one)
        (if (< xiibeta 1.0e-4) (setf xiibeta 1.0e-4))
        (if (> xiibeta 0.9999) (setf xiibeta 0.9999))
        (setf iex (max 
                    (+ (/ -5.0 (expt pp 2)) (- (/ one (expt a 0.2))) -13.0)
                    sae))
        (setf acu (expt 10.0 iex))
        (tagbody
          loop7
          (setf y (ibeta xiibeta pp qq))
          (setf xin xiibeta)
          (setf y (* (- y a)
                     (exp
                        (+ beta
                           (* r (log xin))
                           (* tt (log (- one xin)))))))
          (if (<= (* y yprev) zero) (setf prev (max  sq fpu)))
          (setf g one)
          loop9
          (setf adj (* g y))
          (setf sq (* adj adj))
          (if (>= sq prev) (go loop10))
          (setf tx (- xiibeta adj))
          (if (and (>= tx zero) (<= tx one)) (go loop11))
          loop10
          (setf g (/ g three))
          (go loop9)
          loop11
          (if (<= prev acu) (go loop12))
          (if (<= (* y y) acu) (go loop12))
          (if (or (= tx zero) (= tx one)) (go loop10))
          (if (= tx xiibeta) (go loop12))
          (setf xiibeta tx)
          (setf yprev y)
          (go loop7)
          loop12)
        (if indx (- one xiibeta) xiibeta))  )


;;  Inverse of the incomplete gamma function.
;;  Comments: solves by the partition method the
;;            equation g(x)=0, where g(x)=igamma(x,p)-y
;;            This procedure is accurate about 14 significant
;;            digits except for small regions for x is in the vicinity of 0 and 1.
;;  Conditions: 0<x<1; p>0
(defun iigamma (x p)
   (declare (type flonum x p))
   (let (a b m (ga 100.0) (gb 100.0) (gm 100.0) (err 1.0e-16))
      (declare (type flonum ga gb gm err))
      (do ((i 0 (1+ i)))
          ((<= (* ga gb) 0.0) '$done)
          (setf a (- (expt 1.5 i) 1.0))
          (setf b (- (expt 1.5 (+ i 1)) 1.0))
          (setf ga (- (igamma a p) x))
          (setf gb (- (igamma b p) x)))
      ;; now we know the solution is in [a, b]
      (cond ((< (abs ga) err) a)
            ((< (abs gb) err) b)
            (t (loop (setf m (/ (+ a b) 2))
                     (if (or (= m a) (= m b)) (return m))
                     (setf gm (- (igamma m p) x))
                     (if (< (abs gm) err) (return m))
                     (if (<= (* ga gm) 0.0)
                         (setf b m gb gm)
                         (setf a m ga gm)))))  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                ;;
;;   Numerical routines for the noncentral chi2   ;;
;;                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Cumulative probability of the noncentral chi-square distribution.
;; Reference:
;;    Ding, C. G. (1992)
;;    Algorithm AS275: Computing the non-central chi-squared
;;    distribution function. Appl.Statist., 41, 478-482.
;; Comments: This is a translation of the C code in the R program.
;; Conditions: x>0; f>0 (degrees of freedom) ; theta>0 (non centrality param.)
(defun cdfnchi2 (x f theta errmax reltol itrmax)
   (declare (type flonum x f theta errmax reltol itrmax))
   (cond
      ((< theta 80)
          (let ((sum 0.0)
                (lamda (* 0.5 theta)))
             (declare (type flonum sum lamda))
             (do* ((i 0 (1+ i))
                   (pr (exp (- lamda)) (* pr (/ lamda i))))
                  ((= i 100) 'done)
                (setf sum (+ sum (* pr (igamma (* 0.5 x) (+ (* 0.5 f) i))))) )
             sum))
      (t
          (let* ((dbl_epsilon 1.77635683940025e-15)
                 (dbl_min_exp (* (log 2.0) -1021.0)) ; ln(2) * DBL_MIN_EXP
                 (lam (* 0.5 theta))
                 (lamsml (< (- lam) dbl_min_exp))
                 (lu -1.0)
                 (l_lam -1.0)
                 (l_x -1.0)
                 (u  1.0) (v  1.0) (x2 1.0) (f2 1.0) (f_x_2n 1.0) (tt 1.0)
                 (lt 1.0) tsml (ans 1.0) (term 1.0) is_b is_r is_it n (f_2n 1.0) (bound 1.0)  )
              (declare (type flonum dbl_epsilon dbl_min_exp lam lu l_lam l_x u 
                                    v x2 f2 f_x_2n tt lt l_x ans term f_2n bound)
                       (type boolean lamsml tsml is_b is_r is_it) )
             (cond
                (lamsml
                    ; non centrality parameter too large
                    (setf u 0.0
                          lu (- lam)
                          l_lam (log lam)))
                (t
                    (setf u (exp (- lam)))))

             ; evaluate the first term
             (setf v u
                   x2 (* 0.5 x)
                   f2 (* 0.5 f)
                   f_x_2n (- f x) )
             (setf tt (- x2 f2))
             (cond
                ((and (> (* f2 dbl_epsilon) 0.125)     ; very large f and x ~= f: probably needs
                      (< (abs tt)                      ; other algorithm
                         (* (sqrt dbl_epsilon) f2)))
                    ; evade cancellation error
                    (setf lt (- (* (- 1 tt) (- 2 (/ tt (+ f2 1))))
                                (* 0.5 (log (* 2 ($float '$%pi) (+ f2 1.0)))))))
                (t
                    ; Usual case 2: careful not to overflow
                    (setf lt (- (* f2 (log x2))
                                x2
                                (lngamma (+ f2 1.0))))))
             (setf tsml (< lt dbl_min_exp))
             (cond
                ((and tsml
                      (> x (+ f theta (* 5 (sqrt (* 2 (+ f (* 2 theta))))))))
                    ; x > E[X] + 5* sigma(X)
                    (setf ans 1.0))
                (t
                    (cond
                       (tsml
                          (setf l_x (log x)
                                ans 0.0
                                term 0.0
                                tt 0.0))
                       (t
                          (setf tt (exp lt))
                          (setf ans (* v tt))
                          (setf term ans)))
                    (setf is_b nil
                          is_r nil
                          is_it nil
                          n 1
                          f_2n (+ f 2.0)
                          f_x_2n (+ f_x_2n 2.0))
                    (loop
                       ;f_2n = f + 2*n
                       ;f_x_2n = f - x + 2*n   > 0  <=> (f+2n)  >   x
                       (when (> f_x_2n 0)
                          ; find the error bound and check for convergence
                          (setf bound (/ (* tt x) f_x_2n))
                          (setf is_b (<= bound errmax))
                          (setf is_r (<= term (* reltol ans)))
                          (setf is_it (> n itrmax))
                          ; convergence only if BOTH absolute and relative error < 'bnd'
                          (when (or (and is_b is_r) is_it)
                             (return) ))
                       ; evaluate the next term of the
                       ; expansion and then the partial sum
                       (cond
                          (lamsml
                             (setf lu (+ lu l_lam (- (log n))))
                             (when (>= lu dbl_min_exp)
                                ; no underflow anymore => change regime
                                (setf v (exp lu))  ; the first non-0 'u'
                                (setf u v)
                                (setf lamsml nil)))
                          (t
                             (setf u (* u (/ lam n)))
                             (setf v (+ v u))))
                       (cond
                          (tsml
                             (setf lt (+ lt l_x (- (log f_2n))))
                             (when (>= lt dbl_min_exp)
                                (setf tt (exp lt)) ; the first non-0 'tt'
                                (setf tsml nil)))
                          (t
                             (setf tt (* tt (/ x f_2n)))))
                       (when (and (not lamsml) (not tsml))
                          (setf term (* v tt))
                          (setf ans (+ ans term)))
                       (incf n)
                       (setf f_2n (+ f_2n 2.0))
                       (setf f_x_2n (+ f_x_2n 2.0)) ) ; loop
                    (when is_it
                       ($print (format nil "Warning: cdf_noncentral_chi2 not converged in ~a iterations" itrmax)))
                    ans))   ))) )


;; Quantiles of the noncentral chi-square distribution.
;; Method: First calculate suboptimal Pearson's approximation, then
;;     estimate an interval and apply bipartite method.
;; Comments: This is a translation of the C code in the R program.
;; Conditions: 0<p<1; f>0 (degrees of freedom) ; theta>0 (non centrality param.)
(defun qnchi2 (p df lambda)
   (let ((accu 1e-13)
         (racc (* 8 flonum-epsilon))
         (dbl_epsilon (* 8 flonum-epsilon))
         (dbl_min least-positive-normalized-flonum)
         (dbl_max most-positive-flonum)
         (eps 1e-11)    ; must be > accu
         (reps 1e-10)   ; relative tolerance
         (ux 1.0) (lx 1.0) (nx 1.0) (pp 1.0))
      (declare (type flonum accu eps reps ux lx nx pp dbl_epsilon dbl_min dbl_max))

      ; This is Pearson's (1959) approximation,
      ; which is usually good to 4 figs or so.
      (let (b c ff)
         (setf b (/ (* lambda lambda)
                    (+ df (* 3.0 lambda)) ))
         (setf c (/ (+ df (* 3.0 lambda))
                    (+ df (* 2.0 lambda))))
         (setf ff (/ (+ df (* 2.0 lambda))
                     (* c c)))
         (setf ux (+ b (* c 2.0 (iigamma p (* 0.5 ff)))))
         (when (< ux 0.0) (setf ux 1.0)))

      ; finding an upper and lower bound
      (cond
         ((> p (- 1 dbl_epsilon)) ; probability near 1
             '$inf)
         (t
            (setf pp (min (- 1 dbl_epsilon)
                          (* p (+ 1 eps))))
            (loop
               (when (or (>= ux dbl_max)
                         (>= (cdfnchi2 ux df lambda eps reps 10000.0) pp))
                  (return))
               (setf ux (* 2.0 ux)))
            (setf pp (* p (- 1 eps)))
            (setf lx (min ux dbl_max))
            (loop
               (when (or (<= lx dbl_min)
                         (<= (cdfnchi2 lx df lambda eps reps 10000.0) pp))
                  (return))
               (setf lx (* 0.5 lx)))

            ; interval (lx,ux)  halving
            (loop
               (setf nx (* 0.5 (+ lx ux)))
               (if (> (cdfnchi2 nx df lambda accu racc 100000.0) p)
                  (setf ux nx)
                  (setf lx nx))
               (when (<= (/ (- ux lx) nx) accu)
                  (return)))
            (* 0.5 (+ ux lx)) )) ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                             ;;
;;   Numerical routines for the noncentral t   ;;
;;                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Cumulative probability of the noncentral Student's t distribution.
;; Reference:
;;    Algorithm AS243: Appl.Statist., Lenth,R.V. (1989), vol. 38, no. 1
;; Conditions: df>0 (degrees of freedom)
(defun cdfnt (x df delta)
   (declare (type flonum x df delta))
   (let ((alnrpi 0.57236494292470008707)  ; log(sqrt(%pi))
         (r2pi   0.79788456080286535588)  ; sqrt(2/%pi)
         (r2     1.41421356237309504880)  ; sqrt(2)
         (errmax 1.e-12)
         (itrmax 1000)
         (negdel nil)
         (it 1)
         (tnc 0.0)
         a albeta b del errbd geven godd lambda p q rxb s tt xx xeven xodd) 
      (setf tt  x
            del delta)
      (when (< x 0.0)
         (setf negdel t
               tt     (- tt)
               del    (- del)))
      ; initialize twin series
      ; Guenther, J. (1978). Statist. Computn. Simuln. vol.6, 199.
      (setf xx (/ (* x x) (+ (* x x) df)))
      (when (> xx 0.0)
        (setf lambda (* del del))
        (setf p (* 0.5 (exp (* lambda -0.5))))
        (setf q (* r2pi p del))
        (setf s (- 0.5 p)
              a 0.5
              b (* 0.5 df))
        (setf rxb (expt (- 1.0 xx) b)
              albeta (+ alnrpi
                        (lngamma b)
                        (- (lngamma (+ a b)))))
        (setf xodd  (ibeta xx a b)
              godd  (* 2.0 rxb (exp (- (* a (log xx)) albeta)))
              xeven (- 1.0 rxb)
              geven (* b xx rxb))
        (setf tnc (+ (* p xodd) (* q xeven)))
        ; repeat until convergence or iteration limit
        (loop
           (setf a     (+ a 1.0))
           (setf xodd  (- xodd godd)
                 xeven (- xeven geven)
                 godd  (/ (* godd xx (+ a b -1.0)) a)
                 geven (/ (* geven xx (+ a b -0.5)) (+ a 0.5))
                 p     (/ (* p lambda) (* 2.0 it))
                 q     (/ (* q lambda) (+ (* 2.0 it) 1.0)))
           (setf s   (- s p)
                 tnc (+ tnc (* p xodd) (* q xeven))
                 it (+ it 1))
           (setf errbd (* 2.0 s (- xodd godd)))
           (when (or (< errbd errmax) (> it itrmax))
             (return))  ))
      (cond
         ((> it itrmax)
             ($print "Warning: cdf_noncentral_student_t didn't converge")
             ($funmake 'cdf_noncentral_student_t '((mlist) x df delta)) )
         (t
             (setf tnc (+ tnc ($float (+ 0.5 (* 0.5 ($erf (/ (- del) r2)))))))
             (if negdel
                (- 1.0 tnc)
                tnc)))))



;; Quantiles for the noncentral Student's t distribution
;; by the inverse method. Translation of file qnt.c from R.
;; Conditions: 0<=p<=1, df>0 (degrees of freedom)
(defun qnct (p df ncp)
   (declare (type flonum p df ncp))
   (let ((accu 1.0e-13)
         (eps 1.0e-11)
         (dbl_max most-positive-double-float) ; DBL_MAX
         (dbl_epsilon double-float-epsilon) ; DBL_EPSILON
         ux lx nx pp)

      ; 1. finding an upper and lower bound
      (setf pp (min (- 1.0 dbl_epsilon)
                    (* p (+ 1.0 eps))))
      (setf ux (max 1.0 ncp))
      (loop
         (when (or (>= ux dbl_max)
                   (>= (cdfnt ux df ncp) pp))
            (return 'done))
         (setf ux (* ux 2)))
      (setf pp (* p (- 1.0 eps)))
      (setf lx (min -1.0 (- ncp)))
      (loop
         (when (or (<= lx (- dbl_max))
                   (<= (cdfnt lx df ncp) pp))
            (return 'done))
         (setf lx (* lx 2)))

      ; 2. interval (lx,ux)  halving
      (loop
         (setf nx (* 0.5 (+ lx ux)))
         (if (> (cdfnt nx df ncp) p)
            (setf ux nx)
            (setf lx nx))
         (when (<= (- ux lx) accu)
            (return 'done)))
      (* 0.5 (+ lx ux))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;;
;;         Normal random simulation        ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  Generates random normal variates, with mean=0 and var=1,
;;  using the Box-Mueller method.
;;  Reference:
;;     Knuth, D.E. (1981) Seminumerical Algorithms.
;;     The Art of Computer Programming. Addison-Wesley.
(defvar *rnormal-iset* 0)   ;; this flag indicates whether there is a second random variate
(defvar *rnormal-gset*)     ;; stores the second random variate, if any
(defun rndnormal-box ()
  (let (v1 v2 rsq fac)
    (cond ((= *rnormal-iset* 0)
              (loop (setf v1 (- (* 2.0 ($random 1.0)) 1.0))
                    (setf v2 (- (* 2.0 ($random 1.0)) 1.0))
                    (setf rsq (+ (* v1 v1) (* v2 v2)))
                    (if (and (< rsq 1.0) (> rsq 0.0)) (return 'done)))
              (setf fac (sqrt (* (- 2.0) (log rsq) (/ 1.0 rsq))))
              (setf *rnormal-gset* (* v1 fac))
              (setf *rnormal-iset* 1)
              (* v2 fac))
          (t (setf *rnormal-iset* 0)
              *rnormal-gset*))))


;;  The sample size ss must be a non negative integer.
;;  If ss=0, returns a number, otherwise a maxima list
;;  of length ss
(defun rndnormal (ss &aux sample)
   (cond ((= ss 0)
            (rndnormal-box))
         (t (setf sample nil)
            (dotimes (i ss (cons '(mlist simp) sample))
                     (setf sample (cons (rndnormal 0) sample))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;;
;;     Exponential random simulation       ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  Generates random exponential variates (m),
;;  using the inverse method.
(defun rndexp-inverse (m)
   (declare (type flonum m))
   (let (u)
     (loop (setf u ($random 1.0))
          (if (/= u 0.0) (return 'done)))
     (/ (- (log (- 1.0 ($random 1.0)))) m)))


;;  The sample size ss must be a non negative integer.
;;  If ss=0, returns a number, otherwise a maxima list
;;  of length ss
(defun rndexp (m ss &aux sample)
   (cond ((= ss 0)
            (rndexp-inverse m))
         (t (setf sample nil)
            (dotimes (i ss (cons '(mlist simp) sample))
                     (setf sample (cons (rndexp m 0) sample))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;;
;;         Gamma random simulation         ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  Generates random gamma variates (a, b), combining
;;  Ahrens-Dieter and Cheng-Feast methods.
;;  References:
;;    [1] a >= 1
;;        Cheng, R.C.H. and Feast, G.M. (1979).
;;        Some simple gamma variate generators.
;;        Appl. Stat., 28, 3, 290-295.
;;    [2] 0 < a < 1.
;;        Ahrens, J.H. and Dieter, U. (1974).
;;        Computer methods for sampling from gamma, beta,
;;        poisson and binomial distributions.
;;        Computing, 12, 223-246.
(defun rndgamma-ahrens-cheng (aa bb)
  (declare (type flonum aa bb))
  (let (b c d f e1 r q x tt u1 u2 w (a (- aa 1.0)))
    (cond ((<= aa 1.0)    ;; Ahrens-Dieter algorithm
               (setf e1 3.678794411714423216e-1)
               (setf r (* aa e1))
               (setf q (* ($random 1.0) (+ r 1.0)))
               (cond ((< q 1.0)
                           (setf x (expt q (/ 1.0 aa)))
                           (setf tt (exp (- x))))
                     (t (setf x (- 1.0 (log (+ 1.0 (/ (- 1.0 q) r)))))
                        (setf tt (expt x a))))
               (loop (if (< ($random 1.0) tt) (return 'done))
                     (setf q (* ($random 1.0) (+ r 1.0)))
                     (cond ((< q 1.0)
                                (setf x (expt q (/ 1.0 aa)))
                                (setf tt (exp (- x))))
                           (t (setf x (- 1.0 (log (+ 1.0 (/ (- 1.0 q) r)))))
                              (setf tt (expt x a)))))
               (* bb x))

          (t             ;; Cheng-Feast algorithm GKM3
             (loop (setf b (/ (- aa (/ 1.0 (* 6.0 aa))) a))
                   (setf c (/ 2.0 a))
                   (setf d (+ 2.0 c))
                   (cond ((< aa 2.5)                       ;; GKM1
                               (setf u1 ($random 1.0))
                               (setf u2 ($random 1.0)))
                         (t (setf f (/ 1.0 (sqrt aa)))     ;; GKM2
                            (loop (setf u1 ($random 1.0))
                                  (setf u2 (+ u1 (* (- 1.0 (* 1.86 ($random 1.0))) f)))
                                  (if (and (< 0.0 u2) (< u2 1.0)) (return 'done)))))
                   (setf w (* b (/ u1 u2)))
                   (if (or (<= (+ (* c u2) (- d) w (/ 1.0 w)) 0.0)
                           (<  (+ (* c (log u2)) (- (log w)) w -1.0) 0.0))
                       (return 'done)))
             (* bb a w)))))


;;  The sample size ss must be a non negative integer.
;;  If ss=0, returns a number, otherwise a maxima list
;;  of length ss
(defun rndgamma (a b ss &aux sample)
   (cond ((= ss 0) 
            (rndgamma-ahrens-cheng a b) )
         (t (setf sample nil)
            (dotimes (i ss (cons '(mlist simp) sample))
                     (setf sample (cons (rndgamma a b 0) sample))) )) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;;
;;         Chi^2 random simulation         ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  The sample size ss must be a non negative integer.
;;  If ss=0, returns a number, otherwise a maxima list
;;  of length ss
(defun rndchi2 (n ss &aux sample)
   (cond ((= ss 0)
            (rndgamma-ahrens-cheng (* n 0.5) 2.0) )
         (t (setf sample nil)
            (dotimes (i ss (cons '(mlist simp) sample))
                     (setf sample (cons (rndchi2 n 0) sample))) )) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                           ;;
;;    Noncentral Chi^2 random simulation     ;;
;;                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Generates random noncentral chi^2 variates (df, lambda)
;; based on the fact that the noncentral chi^2 can be decomposed
;; as the sum of a central chisquare with df degrees of freedom plus a
;; noncentral chisquare with zero degrees of freedom (which is a Poisson
;; mixture of central chisquares with integer degrees of freedom),
;; see Formula (29.5b-c) in Johnson, Kotz, Balakrishnan (1995).
(defun rndnchi2-chi2-poisson (df lambda)
   (declare (type flonum df lambda))
   (cond
      ((= lambda 0.0)
          (rndgamma (* 0.5 df) 2.0 0))
      (t
          (let ((r (coerce (rndpoisson (/ lambda 2.0) 0) 'flonum)))
             (declare (type flonum r))
             (when (> r 0.0)
                (setf r (rndchi2 (* 2.0 r) 0)))
             (setf r (+ r (rndgamma (* 0.5 df) 2.0 0)))
             r) )))



;;  The sample size ss must be a non negative integer.
;;  If ss=0, returns a number, otherwise a maxima list
;;  of length ss
(defun rndnchi2 (df lambda ss &aux sample)
   (cond ((= ss 0)
            (rndnchi2-chi2-poisson df lambda))
         (t (setf sample nil)
            (dotimes (i ss (cons '(mlist simp) sample))
                     (setf sample (cons (rndnchi2 df lambda 0) sample))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;;
;;       Student's t random simulation     ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  Generates random Student's t variates (n), based
;;  on the fact that Z/sqrt(Y/n) is a Student's t random
;;  variable with n degrees of freedom, if Z~N(0,1)
;;  and Y~chi^2(n).
(defun rndstudent-ratio (n)
   (declare (type flonum n))
   (let (z y)
     (setf z (rndnormal 0)
           y (rndchi2 n 0))
     (/ z (sqrt (/ y n)))))


;;  The sample size ss must be a non negative integer.
;;  If ss=0, returns a number, otherwise a maxima list
;;  of length ss
(defun rndstudent (n ss &aux sample)
   (cond ((= ss 0) 
            (rndstudent-ratio n) )
         (t (setf sample nil)
            (dotimes (i ss (cons '(mlist simp) sample))
                     (setf sample (cons (rndstudent n 0) sample))) )) )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                    ;;
;;       Noncentral Student's t random simulation     ;;
;;                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  Generates random Noncentral Student variates (n), based
;;  on the fact that X/sqrt(Y/n) is a Noncentral Student random
;;  variable with n degrees of freedom and noncentrality parameter k,
;;  if X~N(k,1) and Y~chi^2(n).
(defun rndncstudent-ratio (n k)
   (declare (type flonum n k))
   (let (x y)
     (setf x (+ k (rndnormal 0))
           y (rndchi2 n 0))
     (/ x (sqrt (/ y n)))))


;;  The sample size ss must be a non negative integer.
;;  If ss=0, returns a number, otherwise a maxima list
;;  of length ss
(defun rndncstudent (n k ss &aux sample)
   (cond ((= ss 0) 
            (rndncstudent-ratio n k) )
         (t (setf sample nil)
            (dotimes (i ss (cons '(mlist simp) sample))
                     (setf sample (cons (rndncstudent-ratio n k) sample))) )) )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;;
;;      Snedecor's F random simulation     ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  Generates random Snedecor's F variates (m,n), based
;;  on the fact that (X/m)/(Y/n) is a Snedecor's F random
;;  variable with m and n degrees of freedom, if X~chi^2(m)
;;  and Y~chi^2(n).
(defun rndf-ratio (m n)
   (declare (type flonum m n))
   (let (x y)
      (setf x (rndchi2 m 0)
            y (rndchi2 n 0))
      (/ (* n x) (* m y))))


;;  The sample size ss must be a non negative integer.
;;  If ss=0, returns a number, otherwise a maxima list
;;  of length ss
(defun rndf (m n ss &aux sample)
   (cond ((= ss 0) 
            (rndf-ratio m n))
         (t (setf sample nil)
            (dotimes (i ss (cons '(mlist simp) sample))
                     (setf sample (cons (rndf m n 0) sample))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;;
;;         Beta random simulation          ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  Generates random beta variates (a, b)
;;  Reference:
;;    [1] Cheng, R.C.H. (1978). Generating Beta Variates
;;        with Nonintegral Shape Parameters.
;;        Communications of the ACM, 21:317-322
;;  Comments:
;;    [1] Algorithms BB and BC
;;    [2] This is a translation from C of the
;;        ranlib.c library. R statistical package uses this same
;;        algorithm but in a more structured style.
;;    [3] Some global variables are defined in order to
;;        save time when many random variables are generated.
(defvar *rbeta-olda* -1.0)
(defvar *rbeta-oldb* -1.0)
(defvar *rbeta-alpha*)
(defvar *rbeta-beta*)
(defvar *rbeta-gamma*)
(defvar *rbeta-delta*)
(defvar *rbeta-k1*)
(defvar *rbeta-k2*)
(defvar *rbeta-a*)
(defvar *rbeta-b*)
(defun rndbeta-cheng (aa bb )
  (declare (type flonum aa bb))
  (let (qsame u1 u2 v w z tt r s y genbet
        (expmax 7.0) (infnty 1.0e304))
        (declare (type flonum expmax infnty))
        (if (and (= *rbeta-olda* aa) (= *rbeta-oldb* bb))
            (setf qsame t)
            (setf qsame nil
                  *rbeta-olda* aa
                  *rbeta-oldb* bb))

        (tagbody
           (if (<= (min aa bb) 1.0) (go s100))

           ;; Algorithm BB - Initialize
           (if qsame (go s40))
           (setf *rbeta-a* (min aa bb))
           (setf *rbeta-b* (max aa bb))
           (setf *rbeta-alpha* (+ *rbeta-a* *rbeta-b*))
           (setf *rbeta-beta* (sqrt (/ (- *rbeta-alpha* 2.0)
                                       (- (* 2.0 *rbeta-a* *rbeta-b*) *rbeta-alpha*))))
           (setf *rbeta-gamma* (+ *rbeta-a* (/ 1.0 *rbeta-beta*)))

           s40
           (setf u1 ($random 1.0))

           ;; Step 1
           (setf u2 ($random 1.0))
           (setf v (* *rbeta-beta* (log (/ u1 (- 1.0 u1)))))
           (if (<= v expmax)
               (setf w (* *rbeta-a* (exp v)))
               (setf w infnty))
           (setf z (* u2 (expt u1 2)))
           (setf r (- (* *rbeta-gamma* v) 1.3862944))
           (setf s (+ *rbeta-a* r (- w)))

           ;; Step 2
           (if (>= (+ s 2.609438) (* 5.0 z)) (go s70))

           ;; Step 3
           (setf tt (log z))
           (if (> s tt) (go s70))

           ;; Step 4
           (if (< (+ r (* *rbeta-alpha* (log (/ *rbeta-alpha* (+ *rbeta-b* w))))) tt)
               (go s40))

           s70
           ;; Step 5
           (if (/= aa *rbeta-a*)
               (setf genbet (/ *rbeta-b* (+ *rbeta-b* w)))
               (setf genbet (/ w (+ *rbeta-b* w))))
           (go s230)

           s100
           ;; Algorithm BC - Initialize
           (if qsame (go s120))
           (setf *rbeta-a* (max aa bb))
           (setf *rbeta-b* (min aa bb))
           (setf *rbeta-alpha* (+ *rbeta-a* *rbeta-b*))
           (setf *rbeta-beta* (/ 1.0 *rbeta-b*))
           (setf *rbeta-delta* (+ 1.0 *rbeta-a* (- *rbeta-b*)))
           (setf *rbeta-k1* (/ (* *rbeta-delta* (+ 1.38889e-2 (* 4.16667e-2 *rbeta-b*)))
                               (- (* *rbeta-a* *rbeta-beta*) 0.777778)))
           (setf *rbeta-k2* (+ 0.25 (* (+ 0.5 (/ 0.25 *rbeta-delta*)) *rbeta-b*)))

           s120
           (setf u1 ($random 1.0))

           ;; Step 1
           (setf u2 ($random 1.0))
           (if (>= u1 0.5) (go s130))

           ;; Step 2
           (setf y (* u1 u2))
           (setf z (* u1 y))
           (if (>= (+ (* 0.25 u2) z (- y)) *rbeta-k1*) (go s120))
           (go s170)

           s130
           ;; Step 3
           (setf z (* (expt u1 2) u2))
           (if (> z 0.25) (go s160))
           (setf v (* *rbeta-beta* (log (/ u1 (- 1.0 u1)))))
           (if (<= v expmax)
               (setf w (* *rbeta-a* (exp v)))
               (setf w infnty))
           (go s200)

           s160
           (if (>= z *rbeta-k2*) (go s120))

           s170
           ;; Step 4 Step 5
           (setf v (* *rbeta-beta* (log (/ u1 (- 1.0 u1)))))
           (if (<= v expmax)
               (setf w (* *rbeta-a* (exp v)))
               (setf w infnty))

           s190
           (if (< (- (* *rbeta-alpha* (+ (log (/ *rbeta-alpha* (+ *rbeta-b* w))) v)) 1.3862944) (log z))
               (go s120))

           s200
           ;; Step 6
           (if (/= aa *rbeta-a*)
               (setf genbet (/ *rbeta-b* (+ *rbeta-b* w)))
               (setf genbet (/ w (+ *rbeta-b* w))))

           s230)
        genbet))


;;  The sample size ss must be a non negative integer.
;;  If ss=0, returns a number, otherwise a maxima list
;;  of length ss
(defun rndbeta (a b ss &aux sample)
   (cond ((= ss 0)
            (rndbeta-cheng a b) )
         (t (setf sample nil)
            (dotimes (i ss (cons '(mlist simp) sample))
                     (setf sample (cons (rndbeta a b 0) sample))) )) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;;
;;        Binomial random simulation       ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  Generates random binomial variates (n p)
;;  Reference:
;;    [1] Kachitvichyanukul, V., Schmeiser, B.W.
;;        Binomial Random Variate Generation.
;;        Communications of the ACM, 31, 2
;;        (February, 1988) 216
;;  Comments:
;;    [1] Algorithm BTPE
;;    [2] This is a translation from C of the
;;        ranlib.c library. R statistical package uses this same
;;        algorithm but in a more structured style.
;;    [3] Some global variables are defined in order to
;;        save time when many random variables are generated.
(defvar *rbin-psave* -1.0)
(defvar *rbin-nsave* -1) ;; integer
(defvar *rbin-m*) ;; integer
(defvar *rbin-xnp*)
(defvar *rbin-p*)
(defvar *rbin-q*)
(defvar *rbin-ffm*)
(defvar *rbin-xnpq*)
(defvar *rbin-fm*)
(defvar *rbin-xm*)
(defvar *rbin-xl*)
(defvar *rbin-xr*)
(defvar *rbin-c*)
(defvar *rbin-al*)
(defvar *rbin-xll*)
(defvar *rbin-xlr*)
(defvar *rbin-p1*)
(defvar *rbin-p2*)
(defvar *rbin-p3*)
(defvar *rbin-p4*)
(defvar *rbin-qn*)
(defvar *rbin-r*)
(defvar *rbin-g*)
(defun rndbinomial-kachit (n pp)
  (declare (type integer n)
           (type flonum pp))
  (let (u v x f amaxp ynorm alv x1 f1 z w z2 x2 f2 w2 ix k t1 mp ix1)
   (tagbody
      (if (/= pp *rbin-psave*) (go s10))
      (if (/= n *rbin-nsave*) (go s20))
      (if (< *rbin-xnp* 30.0) (go s150))
      (go s30)

      s10
      ;; setup, perform only when parameters change
      (setf *rbin-psave* pp)
      (setf *rbin-p* (min *rbin-psave* (- 1.0 *rbin-psave*)))
      (setf *rbin-q* (- 1.0 *rbin-p*))

      s20
      (setf *rbin-xnp* (* n *rbin-p*))
      (setf *rbin-nsave* n)
      (if (< *rbin-xnp* 30.0) (go s140))
      (setf *rbin-ffm* (+ *rbin-xnp* *rbin-p*))
      (setf *rbin-m* (round *rbin-ffm*))
      (setf *rbin-fm* *rbin-m*)
      (setf *rbin-xnpq* (* *rbin-xnp* *rbin-q*))
      (setf *rbin-p1* (+ 0.5 (round (- (* 2.195 (sqrt *rbin-xnpq*)) (* 4.6 *rbin-q*)))))
      (setf *rbin-xm* (+ 0.5 *rbin-fm*))
      (setf *rbin-xl* (- *rbin-xm* *rbin-p1*))
      (setf *rbin-xr* (+ *rbin-xm* *rbin-p1*))
      (setf *rbin-c* (+ 0.134 (/ 20.5 (+ 15.3 *rbin-fm*))))
      (setf *rbin-al* (/ (- *rbin-ffm* *rbin-xl*) (- *rbin-ffm* (* *rbin-xl* *rbin-p*))))
      (setf *rbin-xll* (* *rbin-al* (+ 1.0 (* 0.5 *rbin-al*))))
      (setf *rbin-al* (/ (- *rbin-xr* *rbin-ffm*) (* *rbin-xr* *rbin-q*)))
      (setf *rbin-xlr* (* *rbin-al* (+ 1.0 (* 0.5 *rbin-al*))))
      (setf *rbin-p2* (* *rbin-p1* (+ 1.0 *rbin-c* *rbin-c*)))
      (setf *rbin-p3* (+ *rbin-p2* (/ *rbin-c* *rbin-xll*)))
      (setf *rbin-p4* (+ *rbin-p3* (/ *rbin-c* *rbin-xlr*)))

      s30
      ;; generate variate
      (setf u (* *rbin-p4* ($random 1.0)))
      (setf v ($random 1.0))

      ;; triangular region
      (if (> u *rbin-p1*) (go s40))
      (setf ix (round (+ *rbin-xm* (- (* *rbin-p1* v)) u)))
      (go s170)

      s40
      ;; parallelogram region
      (if (> u *rbin-p2*) (go s50))
      (setf x (+ *rbin-xl* (/ (- u *rbin-p1*) *rbin-c*)))
      (setf v (+ (* v *rbin-c*) 1.0 (- (/ (abs (- *rbin-xm* x)) *rbin-p1*))))
      (if (or (> v 1.0) (<= v 0.0)) (go s30))
      (setf ix (round x))
      (go s70)

      s50
      ;; left tail
      (if (> u *rbin-p3*) (go s60))
      (setf ix (round (+ *rbin-xl* (/ (log v) *rbin-xll*))))
      (if (< ix 0) (go s30))
      (setf v (* v (- u *rbin-p2*) *rbin-xll*))
      (go s70)

      s60
      ;; right tail
      (setf ix (round (- *rbin-xr* (/ (log v) *rbin-xlr*))))
      (if (> ix n) (go s30))
      (setf v (* v (- u *rbin-p3*) *rbin-xlr*))

      s70
      ;; determine appropiate way to perform accept/reject test
      (setf k (abs (- ix *rbin-m*)))
      (if (and (> k 20) (< k (- (/ *rbin-xnpq* 2.0) 1.0))) (go s130))

      ;; explicit evaluation
      (setf f 1.0)
      (setf *rbin-r* (/ *rbin-p* *rbin-q*))
      (setf *rbin-g* (* *rbin-r* (+ n 1.0)))
      (setf t1 (- *rbin-m* ix))
      (if (< t1 0)
          (go s80)
          (if (= t1 0)
              (go s120)
              (go s100)))

      s80
      (setf mp (+ *rbin-m* 1))
      (do ((i mp (1+ i)))
          ((> i ix) 'done)
          (setf f (* f (- (/ *rbin-g* i) *rbin-r*))))
      (go s120)

      s100
      (setf ix1 (+ ix 1))
      (do ((i ix1 (1+ i)))
          ((> i *rbin-m*) 'done)
          (setf f (/ f (- (/ *rbin-g* i) *rbin-r*))))

      s120
      (if (<= v f) (go s170))
      (go s30)

      s130
      ;; squeezing using upper and lower bounds on alog(f(x))
      (setf amaxp (* (/ k *rbin-xnpq*)
                     (+ (/ (+ (* k (+ (/ k 3.0) 0.625)) 0.1666666666666) *rbin-xnpq*)
                        0.5)))
      (setf ynorm (- (/ (* k k) (* 2.0 *rbin-xnpq*))))
      (setf alv (log v))
      (if (< alv (- ynorm amaxp)) (go s170))
      (if (> alv (+ ynorm amaxp)) (go s30))

      ;; Stirling's formula to machine accuracy for
      ;; the final acceptance / rejection test
      (setf x1 (+ ix 1.0))
      (setf f1 (+ *rbin-fm* 1.0))
      (setf z (+ n 1.0 (- *rbin-fm*)))
      (setf w (+ n 1.0 (- ix)))
      (setf z2 (* z z))
      (setf x2 (* x1 x1))
      (setf f2 (* f1 f1))
      (setf w2 (* w w))
      (if (<= alv (+ (* *rbin-xm* (log (/ f1 x1)))
                     (* (+ n (- *rbin-m*) 0.5) (log (/ z w)))
                     (* (+ ix (- *rbin-m*)) (log (/ (* w *rbin-p*) (* x1 *rbin-q*))))
                     (/ (/ (+ 13860.0 (- (/ (+ 462.0 (- (/ (+ 132.0  
                          (- (/ (+ 99.0 (- (/ 140.0 f2))) f2))) f2))) f2))) f1) 166320.0)
                     (/ (/ (+ 13860.0 (- (/ (+ 462.0 (- (/ (+ 132.0
                          (- (/ (+ 99.0 (- (/ 140.0 z2))) z2))) z2))) z2))) z) 166320.0)
                     (/ (/ (+ 13860.0 (- (/ (+ 462.0 (- (/ (+ 132.0
                          (- (/ (+ 99.0 (- (/ 140.0 x2))) x2))) x2))) x2))) x1) 166320.0)
                     (/ (/ (+ 13860.0 (- (/ (+ 462.0 (- (/ (+ 132.0 
                          (- (/ (+ 99.0 (- (/ 140.0 w2))) w2))) w2))) w2))) w) 166320.0)))
          (go s170))
      (go s30)

      s140
      ;; inverse cdf logic for mean less than 30
      (setf *rbin-qn* (expt *rbin-q* n))
      (setf *rbin-r* (/ *rbin-p* *rbin-q*))
      (setf *rbin-g* (* *rbin-r* (+ n 1.0)))

      s150
      (setf ix 0)
      (setf f *rbin-qn*)
      (setf u ($random 1.0))

      s160
      (if (< u f) (go s170))
      (if (> ix 110) (go s150))
      (setf u (- u f))
      (setf ix (+ ix 1))
      (setf f (* f (- (/ *rbin-g* ix) *rbin-r*)))
      (go s160)

      s170)
   (if (> *rbin-psave* 0.5)
       (- n ix)
       ix)))


;;  The sample size ss must be a non negative integer.
;;  If ss=0, returns a number, otherwise a maxima list
;;  of length ss
(defun rndbinomial (n p ss &aux sample)
   (cond ((= ss 0)
            (rndbinomial-kachit n p))
         (t (setf sample nil)
            (dotimes (i ss (cons '(mlist simp) sample))
                     (setf sample (cons (rndbinomial n p 0) sample))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;;
;;        Poisson random simulation        ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  Generates random Poisson variates (m)
;;  Reference:
;;    [1] Ahrens, J.H. and Dieter, U.
;;        Computer Generation of Poisson Deviates
;;        From Modified Normal Distributions.
;;        ACM Trans. Math. Software, 8, 2
;;        (June 1982),163-179
;;  Comments:
;;    [1] Slightly modified version of the program in the
;;        above article
;;    [2] This is a translation from C of the
;;        ranlib.c library. R statistical package uses this same
;;        algorithm but in a more structured style.
;;    [3] Some global variables are defined in order to
;;        save time when many random variables are generated.
(defvar *rpos-muold* 0.0)
(defvar *rpos-muprev* 0.0)
(defvar *rpos-s*)
(defvar *rpos-l*)
(defvar *rpos-d*)
(defvar *rpos-p0*)
(defvar *rpos-omega*)
(defvar *rpos-b1*)
(defvar *rpos-b2*)
(defvar *rpos-c*)
(defvar *rpos-c0*)
(defvar *rpos-c1*)
(defvar *rpos-c2*)
(defvar *rpos-c3*)
(defvar *rpos-m*)
(defvar *rpos-p*)
(defvar *rpos-q*)
(defvar *rpos-pp* (make-array 35 :initial-element 0.0 :element-type 'flonum))
(defun rndpoisson-ahrens (mu)
   (declare (type flonum mu))
   (let ( ignpoi j kflag del difmuk e fk fx fy g px py tt u v x xx
          (a0 -0.5) (a1 0.3333333) (a2 -0.2500068) (a3 0.2000118)
          (a4 -0.1661269) (a5 0.1421878) (a6 -0.1384794) (a7 0.125006)
          (fact (make-array 10
                   :element-type 'flonum
                   :initial-contents '(1.0 1.0 2.0 6.0 24.0 120.0
                                       720.0 5040.0 40320.0 362880.0))))
       (declare (type flonum a0 a1 a2 a3 a4 a5 a6 a7))
       (declare (type (simple-array flonum (10)) fact))
       (tagbody
          (if (= mu *rpos-muprev*) (go s10))
          (if (< mu 10.0) (go s120))

          ;; Case A. (Recalculation of, *rpos-s*, *rpos-d*, *rpos-l* if mu has changed)
          (setf *rpos-muprev* mu)
          (setf *rpos-s* (sqrt mu))
          (setf *rpos-d* (* 6.0 mu mu))
          (setf *rpos-l* (floor (- mu 1.1484)))

          s10
          ;; Step n. Normal sample
          (setf g (+ mu (* *rpos-s* (rndnormal 0))))
          (if (< g 0.0) (go s20))
          (setf ignpoi (floor g))

          ;; Step I. Immediate acceptance if ignpoi is large enough
          (if (>= ignpoi *rpos-l*) (go s200))

          ;; Step S. Squeez acceptance
          (setf fk (coerce ignpoi 'flonum))
          (setf difmuk (- mu fk))
          (setf u ($random 1.0))
          (if (>= (* *rpos-d* u) (* difmuk difmuk difmuk)) (go s200))

          s20
          ;; Step P. Preparations for steps Q and H.
          ;; (Recalculations of parameters if necessary)
          ;; .3989423...=(2*%pi)^(-.5)  .416667...E-1=1./24.  .1428571...=1./7.
          ;; The quantities *rpos-b1*, *rpos-b2*, *rpos-c3*, *rpos-c2*, *rpos-c1*, *rpos-c0* are for thr Hermite
          ;; approximations to the discrete normal probabilities fk.
          ;; c=0.1069/mu guarantees majorization by the 'hat'-function.
          (if (= mu *rpos-muold*) (go s30))
          (setf *rpos-muold* mu)
          (setf *rpos-omega* (/ 0.39894228040143 *rpos-s*))
          (setf *rpos-b1* (/ 0.41666666666667e-1 mu))
          (setf *rpos-b2* (* 0.3 *rpos-b1* *rpos-b1*))
          (setf *rpos-c3* (* 0.14285714285714 *rpos-b1* *rpos-b2*))
          (setf *rpos-c2* (- *rpos-b2* (* 15.0 *rpos-c3*)))
          (setf *rpos-c1* (+ *rpos-b1* (* -6.0 *rpos-b2*) (* 45.0 *rpos-c3*)))
          (setf *rpos-c0* (+ 1.0 (- *rpos-b1*) (* 3.0 *rpos-b2*) (* -15.0 *rpos-c3*)))
          (setf *rpos-c* (/ 0.1069 mu))

          s30
          (if (< g 0.0) (go s50))

          ;; subroutine F is called
          (setf kflag 0)
          (go s70)

          s40
          ;; Step Q. Quotient acceptance (rare case)
          (if (<= (- fy (* u fy)) (* py (exp (- px fx)))) (go s200))

          s50
          ;; Step E. Exponential sample - rndexp is called for
          ;; standard exponential variate e and sample tt from the Laplace 'hat'
          ;; (if tt <= -.6744 then pk < fk for all mu >= 10.)
          (setf e (rndexp 1.0 0))
          (setf u ($random 1.0))
          (setf u (+ u (- u 1.0)))
          (setf tt (+ 1.8 (if (or (and (> u 0.0) (< e 0.0))
                                    (and (< u 0.0) (> e 0.0)))
                                (- e)
                                 e)))
          (if (<= tt -0.6744) (go s50))
          (setf ignpoi (floor (+ mu (* *rpos-s* tt))))
          (setf fk (coerce ignpoi 'flonum))
          (setf difmuk (- mu fk))

          ;; subroutine F is called
          (setf kflag 1)
          (go s70)

          s60
          ;; Step F. Hat acceptance (E is repeated on rejection)
          (if (> (* *rpos-c* (abs u)) (- (* py (exp (+ px e))) (* fy (exp (+ fx e))))) (go s50))
          (go s200)

          s70
          ;; Step F. Subroutine F. Calculation of px, py, fx, fy.
          ;; Case ignpoi < 10  uses factorials from table fact
          (if (>= ignpoi 10) (go s80))
          (setf px (- mu))
          (setf py (/ (expt mu ignpoi) (aref fact ignpoi)))
          (go s110)

          s80
          ;; Case ignpoi >= 10 uses polynomial approximation
          ;; a0-a7 for accuracy when advisable
          ;; .8333333E-1=1./12.  .3989423=(2*%pi)^(-.5)
          (setf del (/ 8.333333333333333e-2 fk))
          (setf del (- del (* 4.8 del del del)))
          (setf v (/ difmuk fk))
          (if (<= (abs v) 0.25) (go s90))
          (setf px (+ (* fk (log (+ 1.0 v))) (- difmuk) (- del)))
          (go s100)

          s90
          (setf px (+ (* fk v v 
                         (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* (+ (* a7 v)
                                 a6) v) a5) v) a4) v) a3) v) a2) v) a1) v) a0))
                      (- del)))

          s100
          (setf py (/ 0.39894228040143 (sqrt fk)))

          s110
          (setf x (/ (- 0.5 difmuk) *rpos-s*))
          (setf xx (* x x))
          (setf fx (* -0.5 xx))
          (setf fy (* *rpos-omega*  (+ (* (+ (* (+ (* *rpos-c3* xx)
                                  *rpos-c2*) xx) *rpos-c1*) xx) *rpos-c0*)))
          (if (<= kflag 0) (go s40))
          (go s60)

          s120
          ;; Case B. Start new table and calculate *rpos-p0* if necessary
          (setf *rpos-muprev* 0.0)
          (if (= mu *rpos-muold*) (go s130))
          (setf *rpos-muold* mu)
          (setf *rpos-m* (max 1 (floor mu)))
          (setf *rpos-l* 0)
          (setf *rpos-p* (exp (- mu)))
          (setf *rpos-p0* *rpos-p*)
          (setf *rpos-q* *rpos-p*)

          s130
          ;; Step U. Uniform sample for inversion method
          (setf u ($random 1.0))
          (setf ignpoi 0)
          (if (<= u *rpos-p0*) (go s200))

          ;; Step T.
          (if (= *rpos-l* 0) (go s150))
          (setf j 1)
          (if (> u 0.458) (setf j (min *rpos-l* *rpos-m*)))
          (do ((k j (1+ k)))
              ((> k *rpos-l*) 'done)
             (cond ((<= u (aref *rpos-pp* (- k 1)))
                        (setf ignpoi k)
                        (go s200))))
          (if (= *rpos-l* 35) (go s130))

          s150
          ;; Step C. Creation of new Poisson probabilities p
          ;; and their cumulatives *rpos-q*=*rpos-pp*(k)
          (setf *rpos-l* (1+ *rpos-l*))
          (do ((k *rpos-l* (1+ k)))
              ((> k 35) 'done)
             (setf *rpos-p* (/ (* *rpos-p* mu) (float k)))
             (setf *rpos-q* (+ *rpos-q* *rpos-p*))
             (setf (aref *rpos-pp* (- k 1)) *rpos-q*)
             (cond ((<= u *rpos-q*)
                       (setf *rpos-l* k)
                       (setf ignpoi k)
                       (go s200)) ) )
          (setf *rpos-l* 35)
          (go s130)

          s200 )
       ignpoi))


;;  The sample size ss must be a non negative integer.
;;  If ss=0, returns a number, otherwise a maxima list
;;  of length ss
(defun rndpoisson (m ss &aux sample)
   (declare (type flonum m))
   (declare (type integer ss))
   (cond ((= ss 0)
            (rndpoisson-ahrens m))
         (t (setf sample nil)
            (dotimes (i ss (cons '(mlist simp) sample))
                     (setf sample (cons (rndpoisson m 0) sample))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;;
;;       Geometric random simulation       ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  Generates random geometric variates (p)
;;  by simulation of Bernoulli trials.
(defun rndgeo-trials (p)
   (declare (type flonum p))
   (let ((sum 0))
      (declare (type integer sum))
      (loop (if (<= ($random 1.0) p) (return sum))
            (setf sum (1+ sum)))))


;;  The sample size ss must be a non negative integer.
;;  If ss=0, returns a number, otherwise a maxima list
;;  of length ss
(defun rndgeo (p ss &aux sample)
   (declare (type flonum p))
   (declare (type integer ss))
   (cond ((= ss 0)
            (rndgeo-trials p))
         (t (setf sample nil)
            (dotimes (i ss (cons '(mlist simp) sample))
                     (setf sample (cons (rndgeo p 0) sample))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;;
;;   Hypergeometric random simulation      ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;  Computes the logarithm of the factorial log(n!)
;;  This function is called from rndhypergeo-kachit
(defun afc (i)
   (declare (type integer i))
   (let ((di (float i))
         (al (make-array 9
                   :element-type 'flonum
                   :initial-contents '(	0.0
                                        0.0 ;; ln(0!)=ln(1)
                                        0.0 ;; ln(1!)=ln(1)
                                        0.69314718055994530941723212145817 ;; ln(2)
                                        1.79175946922805500081247735838070 ;; ln(6)
                                        3.17805383034794561964694160129705 ;; ln(24)
                                        4.78749174278204599424770093452324
                                        6.57925121201010099506017829290394
                                        8.52516136106541430016553103634712))))
       (declare (type flonum di))
       (declare (type (simple-array flonum (9)) al))
       (cond ((<= i 7)
                 (aref al (1+ i)))
             (t  (+ (* (+ di 0.5)
                       (log di))
                    (- di)
                    (/ 0.83333333333333333333333333333333 di)
                    (/ (- 0.00277777777777777777777777777778)
                       di di di)
                    0.91893853320467274178032973640562))) ) )


;;  Generates random hypergeometric variates (n1, n2, n),
;;  Reference:
;;    [1] Kachitvichyanukul, V., Schmeiser, B.W. (1985)
;;        Computer generation of hypergeometric random variates.
;;        Journal of Statistical Computation and Simulation 22, 127-145.
;;  Comments:
;;    [1] This is a translation from C of the
;;        rhyper.c file in the R statistical package.
;;    [2] Some global variables are defined in order to
;;        save time when many random variables are generated.
;; integer globals
(defvar *rhyp-n1s* -1)
(defvar *rhyp-n2s* -1)
(defvar *rhyp-ks* -1)
(defvar *rhyp-k*)
(defvar *rhyp-n1*)
(defvar *rhyp-n2*)
(defvar *rhyp-m*)
(defvar *rhyp-minjx*)
(defvar *rhyp-maxjx*)
;; long globals
(defvar *rhyp-tn*)
(defvar *rhyp-a*)
(defvar *rhyp-d*)
(defvar *rhyp-s*)
(defvar *rhyp-w*)
(defvar *rhyp-xl*)
(defvar *rhyp-xr*)
(defvar *rhyp-kl*)
(defvar *rhyp-kr*)
(defvar *rhyp-lamdl*)
(defvar *rhyp-lamdr*)
(defvar *rhyp-p1*)
(defvar *rhyp-p2*)
(defvar *rhyp-p3*)
(defun rndhypergeo-kachit (nn1 nn2 kk)
   (declare (type integer nn1 nn2 kk))
   (let (ix reject setup1 setup2 e f g p r tt u v y de dg dr ds dt gl
         gu nk nm ub xk xm xn y1 ym yn yk alv
         (con 57.56462733) (deltal 0.0078) (deltau 0.0034) (scale 1.e25))
       (declare (type flonum con deltal deltau scale))
       ;; if new parameter values, initialize
       (setf reject t)
       (cond ((or (/= nn1 *rhyp-n1s*) (/= nn2 *rhyp-n2s*))
                (setf setup1 t
                      setup2 t))
             (t (cond ((/= kk *rhyp-ks*)
                         (setf setup1 nil
                               setup2 t))
                      (t (setf setup1 nil
                               setup2 nil)))))
       (cond (setup1
                 (setf *rhyp-n1s* nn1
                       *rhyp-n2s* nn2
                       *rhyp-tn* (+ nn1 nn2))
                 (cond ((<= nn1 nn2)
                          (setf *rhyp-n1* nn1
                                *rhyp-n2* nn2))
                       (t (setf *rhyp-n1* nn2
                                *rhyp-n2* nn1)))))
       (cond (setup2
                 (setf *rhyp-ks* kk)
                 (cond ((>= (+ kk kk) *rhyp-tn*)
                          (setf *rhyp-k* (- *rhyp-tn* kk)))
                       (t (setf *rhyp-k* kk)))))
       (cond ((or setup1 setup2)
                  (setf *rhyp-m* (/ (* (+ *rhyp-k* 1.0) (+ *rhyp-n1* 1.0)) (+ *rhyp-tn* 2.0))
                        *rhyp-minjx* (max 0 (- *rhyp-k* *rhyp-n2*))
                        *rhyp-maxjx* (min *rhyp-n1* *rhyp-k*))))

       ;; generate random variate --- Three basic cases
       (cond ((= *rhyp-minjx* *rhyp-maxjx*)                ;; I: degenerate distribution ------------
                (setf ix *rhyp-maxjx*)
                (cond ((>= (+ kk kk) *rhyp-tn*)
                           (cond ((> nn1 nn2)
                                    (+ kk (- nn2) ix))
                                 (t (- nn1 ix))))
                      (t (cond ((> nn1 nn2)
                                  (- kk ix))
                               (t ix)))))
             ((< (- *rhyp-m* *rhyp-minjx*) 10)             ;; II: inverse transformation ------------
                (cond ((or setup1 setup2)
                         (cond ((< *rhyp-k* *rhyp-n2*)
                                   (setf *rhyp-w* (exp (+ con 
                                                          (afc *rhyp-n2*)
                                                          (afc (+ *rhyp-n1* *rhyp-n2* (- *rhyp-k*)))
                                                          (- (afc (- *rhyp-n2* *rhyp-k*)))
                                                          (- (afc (+ *rhyp-n1* *rhyp-n2*)))))))
                                (t (setf *rhyp-w* (exp (+ con
                                                          (afc *rhyp-n1*)
                                                          (afc *rhyp-k*)
                                                          (- (afc (- *rhyp-k* *rhyp-n2*)))
                                                          (- (afc (+ *rhyp-n1* *rhyp-n2*))))))))))
                (tagbody
                   l10
                   (setf p *rhyp-w*
                         ix *rhyp-minjx*
                         u (* ($random 1.0) scale))
                   l20
                   (cond ((> u p) 
                            (setf u (- u p))
                            (setf p (* p (- *rhyp-n1* ix) (- *rhyp-k* ix)))
                            (setf ix (1+ ix))
                            (setf p (/ p ix (+ *rhyp-n2* (- *rhyp-k*) ix)))
                            (if (> ix *rhyp-maxjx*) (go l10) (go l20))))))
             (t (cond ((or setup1 setup2)    ;; III : h2pe ----------------------------
                          (setf *rhyp-s* (sqrt (/ (* (- *rhyp-tn* *rhyp-k*) *rhyp-k* *rhyp-n1* *rhyp-n2*)
                                           (- *rhyp-tn* 1) *rhyp-tn* *rhyp-tn*)))
                          (setf *rhyp-d* (+ (round (* 1.5 *rhyp-s*)) 0.5))
                          (setf *rhyp-xl* (+ *rhyp-m* (- *rhyp-d*) 0.5)
                                *rhyp-xr* (+ *rhyp-m* *rhyp-d* 0.5))
                          (setf *rhyp-a* (+ (afc *rhyp-m*)
                                            (afc (- *rhyp-n1* *rhyp-m*))
                                            (afc (- *rhyp-k* *rhyp-m*))
                                            (afc (+ *rhyp-n2* (- *rhyp-k*) *rhyp-m*))))
                          (setf *rhyp-kl* (exp (- *rhyp-a* 
                                           (afc (round *rhyp-xl*))
                                           (afc (round (- *rhyp-n1* *rhyp-xl*)))
                                           (afc (round (- *rhyp-k* *rhyp-xl*)))
                                           (afc (round (+ *rhyp-n2* (- *rhyp-k*) *rhyp-xl*))))))
                          (setf *rhyp-kr* (exp (- *rhyp-a* 
                                           (afc (round (- *rhyp-xr* 1)))
                                           (afc (round (+ *rhyp-n1* (- *rhyp-xr*) 1)))
                                           (afc (round (+ *rhyp-k* (- *rhyp-xr*) 1)))
                                           (afc (round (+ *rhyp-n2* (- *rhyp-k*) *rhyp-xr* (- 1)))))))
                          (setf *rhyp-lamdl* (- (log (/ (* *rhyp-xl* (+ *rhyp-n2* (- *rhyp-k*) *rhyp-xl*)) 
                                                 (+ *rhyp-n1* (- *rhyp-xl*) 1.0)
                                                 (+ *rhyp-k* (- *rhyp-xl*) 1.0))))
                                *rhyp-lamdr* (- (log (/ (* (+ *rhyp-n1* (- *rhyp-xr*) 1.0)
                                                           (+ *rhyp-k* (- *rhyp-xr*) 1.0))
                                                         *rhyp-xr*
                                                         (+ *rhyp-n2* (- *rhyp-k*) *rhyp-xr*)))))
                          (setf *rhyp-p1* (+ *rhyp-d* *rhyp-d*)
                                *rhyp-p2* (+ *rhyp-p1* (/ *rhyp-kl* *rhyp-lamdl*))
                                *rhyp-p3* (+ *rhyp-p2* (/ *rhyp-kr* *rhyp-lamdr*)))))
                (tagbody
                   l30
                   (setf u (* ($random 1.0) *rhyp-p3*))
                   (setf v ($random 1.0))
                   (cond ((< u *rhyp-p1*) 
                              ;; rectangular region
                              (setf ix (round (+ *rhyp-xl* u))))
                         ((<= u *rhyp-p2*) 
                              ;; left tail
                              (setf ix (round (+ *rhyp-xl* (/ (log v) *rhyp-lamdl*))))
                              (if (< ix *rhyp-minjx*) (go l30))
                              (setf v (* v (- u *rhyp-p1*) *rhyp-lamdl*)))
                         (t   ;; right tail
                              (setf ix (round (- *rhyp-xr* (/ (log v) *rhyp-lamdr*))))
                              (if (> ix *rhyp-maxjx*) (go l30))
                              (setf v (* v (- u *rhyp-p2*) *rhyp-lamdr*))))
                   ;; acceptance/rejection test
                   (cond ((or (< *rhyp-m* 100) (<= ix 50)) 
                             ;; explicit evaluation
                             (setf f 1.0)
                             (cond ((< *rhyp-m* ix)
                                      (do ((i (+ *rhyp-m* 1) (1+ i)))
                                          ((> i ix) 'done)
                                          (setf f (/ (* f (+ *rhyp-n1* (- i) 1) (+ *rhyp-k* (- i) 1))
                                                     (+ *rhyp-n2* (- *rhyp-k*) i)
                                                     i))))
                                   ((> *rhyp-m* ix)
                                       (do ((i (+ ix 1) (1+ i)))
                                           ((> i *rhyp-m*) 'done)
                                           (setf f (/ (* f i (+ *rhyp-n2* (- *rhyp-k*) i))
                                                      (- *rhyp-n1* i)
                                                      (- *rhyp-k* i))))))
                             (cond ((<= v f) (setf reject nil))))
                         (t  ;; squeeze using upper and lower bounds
                             (setf y ix
                                   y1 (+ y 1.0)
                                   ym (- y *rhyp-m*)
                                   yn (+ *rhyp-n1* (- *rhyp-k*) 1.0)
                                   yk (+ *rhyp-k* (- y) 1.0)
                                   nk (+ *rhyp-n2* (- *rhyp-k*) y1)
                                   r (/ (- ym) y1)
                                   *rhyp-s* (/ ym yn)
                                   tt (/ ym yk)
                                   e (/ (- ym) nk)
                                   g (- (/ (* yn yk) (* y1 nk)) 1.0)
                                   dg 1.0)
                             (if (< g 1.0) (setf dg (+ 1.0 g)))
                             (setf gu (* g (+ 1.0 (* g (+ -0.5 (/ g 3.0)))))
                                   gl (- gu (/ (* g g g g 0.25) dg))
                                   xm (+ *rhyp-m* 0.5)
                                   xn (+ *rhyp-n1* (- *rhyp-m*) 0.5)
                                   xk (+ *rhyp-k* (- *rhyp-m*) 0.5)
                                   nm (+ *rhyp-n2* (- *rhyp-k*) xm)
                                   ub (+ (* y gu)
                                         (* (- *rhyp-m*) gl)
                                         deltau
                                         (* xm r (+ 1.0 (* r (+ -0.5 (/ r 3.0)))))
                                         (* xn *rhyp-s* (+ 1.0 (* *rhyp-s* (+ -0.5 (/ *rhyp-s* 3.0)))))
                                         (* xk tt (+ 1.0 (* tt (+ -0.5 (/ tt 3.0)))))
                                         (* nm e (+ 1.0 (* e (+ -0.5 (/ e 3.0)))))))
                             ;; test against upper bound
                             (setf alv (log v))
                             (cond ((> alv ub)
                                      (setf reject t))
                                   (t (setf dr (* r r r r xm))
                                      (if (< r 0.0) (setf dr (/ dr (+ 1.0 r))))
                                      (setf ds (* *rhyp-s* *rhyp-s* *rhyp-s* *rhyp-s* xn))
                                      (if (< *rhyp-s* 0.0) (setf ds (/ ds (+ 1.0 *rhyp-s*))))
                                      (setf dt (* tt tt tt tt xk))
                                      (if (< tt 0.0) (setf dt (/ dt (+ 1.0 tt))))
                                      (setf de (* e e e e nm))
                                      (if (< e 0.0) (setf de (/ de (+ 1.0 e))))
                                      (cond ((< alv (- ub
                                                       (* 0.25 (+ dr ds dt de))
                                                       (* (+ y *rhyp-m*) (- gl gu))
                                                       deltal)) 
                                               (setf reject nil))
                                            ((<= alv (- *rhyp-a*
                                                        (afc ix)
                                                        (afc (- *rhyp-n1* ix))
                                                        (afc (- *rhyp-k* ix))
                                                        (afc (+ *rhyp-n2* (- *rhyp-k*) ix)))) 
                                               (setf reject nil))
                                            (t (setf reject t)))))))
                   (if reject (go l30)))))
       ;; return appropriate variate
       (cond ((>= (+ kk kk) *rhyp-tn*)
                 (cond ((> nn1 nn2)
                          (+ kk (- nn2) ix))
                       (t (- nn1 ix))))
             (t  (cond ((> nn1 nn2) 
                          (- kk ix))
                       (t ix))))))


;;  The sample size ss must be a non negative integer.
;;  If ss=0, returns a number, otherwise a maxima list
;;  of length ss
(defun rndhypergeo (n1 n2 n ss &aux sample)
   (declare (type integer n1 n2 n ss))
   (cond ((= ss 0) 
            (rndhypergeo-kachit n1 n2 n) )
         (t (setf sample nil)
            (dotimes (i ss (cons '(mlist simp) sample))
                     (setf sample (cons (rndhypergeo n1 n2 n 0) sample))) )) )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                         ;;
;;   Negative binomial random simulation   ;;
;;                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  Generates random negative binomial variates (n p)
;;  Reference:
;;    [1] Devroye, L. (1986)
;;        Non-Uniform Random Variate Generation.
;;        Springer Verlag, p. 480
;;  Comments:
;;    [1] This is a translation from C of the
;;        rnbinom.c file in the R statistical package.
(defun rndnegbinom-devroye (n p)
   (declare (type flonum p))
   (rndpoisson (rndgamma n (/ (- 1.0 p) p) 0) 0) )


;;  The sample size ss must be a non negative integer.
;;  If ss=0, returns a number, otherwise a maxima list
;;  of length ss
(defun rndnegbinom (n p ss &aux sample)
   (cond ((= ss 0)
            (rndnegbinom-devroye n p) )
         (t (setf sample nil)
            (dotimes (i ss (cons '(mlist simp) sample))
                     (setf sample (cons (rndnegbinom n p 0) sample))) )) )


