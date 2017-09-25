;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 2edcbd958861 2012/05/30 03:34:52 toy $"
;;;  "f2cl2.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 3fe93de3be82 2012/05/06 02:17:14 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 3fe93de3be82 2012/05/06 02:17:14 toy $")

;;; Using Lisp CMU Common Lisp 20d (20D Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :lapack)


(let* ((maxit 20)
       (zero 0.0)
       (one 1.0)
       (two 2.0)
       (three 3.0)
       (four 4.0)
       (eight 8.0)
       (ten 10.0))
  (declare (type (f2cl-lib:integer4 20 20) maxit)
           (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (type (double-float 2.0 2.0) two)
           (type (double-float 3.0 3.0) three)
           (type (double-float 4.0 4.0) four)
           (type (double-float 8.0 8.0) eight)
           (type (double-float 10.0 10.0) ten)
           (ignorable maxit zero one two three four eight ten))
  (defun dlasd4 (n i d z delta rho sigma work info)
    (declare (type (double-float) sigma rho)
             (type (array double-float (*)) work delta z d)
             (type (f2cl-lib:integer4) info i n))
    (f2cl-lib:with-multi-array-data
        ((d double-float d-%data% d-%offset%)
         (z double-float z-%data% z-%offset%)
         (delta double-float delta-%data% delta-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((dd (make-array 3 :element-type 'double-float))
             (zz (make-array 3 :element-type 'double-float)) (a 0.0) (b 0.0)
             (c 0.0) (delsq 0.0) (delsq2 0.0) (dphi 0.0) (dpsi 0.0) (dtiim 0.0)
             (dtiip 0.0) (dtipsq 0.0) (dtisq 0.0) (dtnsq 0.0) (dtnsq1 0.0)
             (dw 0.0) (eps 0.0) (erretm 0.0) (eta 0.0) (phi 0.0) (prew 0.0)
             (psi 0.0) (rhoinv 0.0) (sg2lb 0.0) (sg2ub 0.0) (tau 0.0)
             (temp 0.0) (temp1 0.0) (temp2 0.0) (w 0.0) (ii 0) (iim1 0)
             (iip1 0) (ip1 0) (iter 0) (j 0) (niter 0) (orgati nil) (swtch nil)
             (swtch3 nil))
        (declare (type (array double-float (3)) dd zz)
                 (type (double-float) a b c delsq delsq2 dphi dpsi dtiim dtiip
                                      dtipsq dtisq dtnsq dtnsq1 dw eps erretm
                                      eta phi prew psi rhoinv sg2lb sg2ub tau
                                      temp temp1 temp2 w)
                 (type (f2cl-lib:integer4) ii iim1 iip1 ip1 iter j niter)
                 (type f2cl-lib:logical orgati swtch swtch3))
        (setf info 0)
        (cond
          ((= n 1)
           (setf sigma
                   (f2cl-lib:fsqrt
                    (+
                     (* (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%)
                        (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%))
                     (* rho
                        (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%)
                        (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%)))))
           (setf (f2cl-lib:fref delta-%data% (1) ((1 *)) delta-%offset%) one)
           (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%) one)
           (go end_label)))
        (cond
          ((= n 2)
           (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
               (dlasd5 i d z delta rho sigma work)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-6))
             (setf sigma var-5))
           (go end_label)))
        (setf eps (dlamch "Epsilon"))
        (setf rhoinv (/ one rho))
        (cond
          ((= i n)
           (setf ii (f2cl-lib:int-sub n 1))
           (setf niter 1)
           (setf temp (/ rho two))
           (setf temp1
                   (/ temp
                      (+ (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                         (f2cl-lib:fsqrt
                          (+
                           (* (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                              (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%))
                           temp)))))
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (setf (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)
                       (+ (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                          (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                          temp1))
               (setf (f2cl-lib:fref delta-%data% (j) ((1 *)) delta-%offset%)
                       (- (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                          (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                          temp1))
              label10))
           (setf psi zero)
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j (f2cl-lib:int-add n (f2cl-lib:int-sub 2))) nil)
             (tagbody
               (setf psi
                       (+ psi
                          (/
                           (* (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                              (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%))
                           (*
                            (f2cl-lib:fref delta-%data%
                                           (j)
                                           ((1 *))
                                           delta-%offset%)
                            (f2cl-lib:fref work-%data%
                                           (j)
                                           ((1 *))
                                           work-%offset%)))))
              label20))
           (setf c (+ rhoinv psi))
           (setf w
                   (+ c
                      (/
                       (* (f2cl-lib:fref z-%data% (ii) ((1 *)) z-%offset%)
                          (f2cl-lib:fref z-%data% (ii) ((1 *)) z-%offset%))
                       (*
                        (f2cl-lib:fref delta-%data%
                                       (ii)
                                       ((1 *))
                                       delta-%offset%)
                        (f2cl-lib:fref work-%data%
                                       (ii)
                                       ((1 *))
                                       work-%offset%)))
                      (/
                       (* (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%)
                          (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%))
                       (*
                        (f2cl-lib:fref delta-%data% (n) ((1 *)) delta-%offset%)
                        (f2cl-lib:fref work-%data%
                                       (n)
                                       ((1 *))
                                       work-%offset%)))))
           (cond
             ((<= w zero)
              (setf temp1
                      (f2cl-lib:fsqrt
                       (+
                        (* (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                           (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%))
                        rho)))
              (setf temp
                      (+
                       (/
                        (*
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub n 1))
                                        ((1 *))
                                        z-%offset%)
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub n 1))
                                        ((1 *))
                                        z-%offset%))
                        (*
                         (+
                          (f2cl-lib:fref d-%data%
                                         ((f2cl-lib:int-sub n 1))
                                         ((1 *))
                                         d-%offset%)
                          temp1)
                         (+
                          (- (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                             (f2cl-lib:fref d-%data%
                                            ((f2cl-lib:int-sub n 1))
                                            ((1 *))
                                            d-%offset%))
                          (/ rho
                             (+ (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                                temp1)))))
                       (/
                        (* (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%)
                           (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%))
                        rho)))
              (cond
                ((<= c temp)
                 (setf tau rho))
                (t
                 (setf delsq
                         (*
                          (- (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                             (f2cl-lib:fref d-%data%
                                            ((f2cl-lib:int-sub n 1))
                                            ((1 *))
                                            d-%offset%))
                          (+ (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                             (f2cl-lib:fref d-%data%
                                            ((f2cl-lib:int-sub n 1))
                                            ((1 *))
                                            d-%offset%))))
                 (setf a
                         (+ (* (- c) delsq)
                            (*
                             (f2cl-lib:fref z-%data%
                                            ((f2cl-lib:int-sub n 1))
                                            ((1 *))
                                            z-%offset%)
                             (f2cl-lib:fref z-%data%
                                            ((f2cl-lib:int-sub n 1))
                                            ((1 *))
                                            z-%offset%))
                            (* (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%)
                               (f2cl-lib:fref z-%data%
                                              (n)
                                              ((1 *))
                                              z-%offset%))))
                 (setf b
                         (* (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%)
                            (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%)
                            delsq))
                 (cond
                   ((< a zero)
                    (setf tau
                            (/ (* two b)
                               (- (f2cl-lib:fsqrt (+ (* a a) (* four b c)))
                                  a))))
                   (t
                    (setf tau
                            (/ (+ a (f2cl-lib:fsqrt (+ (* a a) (* four b c))))
                               (* two c))))))))
             (t
              (setf delsq
                      (*
                       (- (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                          (f2cl-lib:fref d-%data%
                                         ((f2cl-lib:int-sub n 1))
                                         ((1 *))
                                         d-%offset%))
                       (+ (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                          (f2cl-lib:fref d-%data%
                                         ((f2cl-lib:int-sub n 1))
                                         ((1 *))
                                         d-%offset%))))
              (setf a
                      (+ (* (- c) delsq)
                         (*
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub n 1))
                                         ((1 *))
                                         z-%offset%)
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub n 1))
                                         ((1 *))
                                         z-%offset%))
                         (* (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%)
                            (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%))))
              (setf b
                      (* (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%)
                         (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%)
                         delsq))
              (cond
                ((< a zero)
                 (setf tau
                         (/ (* two b)
                            (- (f2cl-lib:fsqrt (+ (* a a) (* four b c))) a))))
                (t
                 (setf tau
                         (/ (+ a (f2cl-lib:fsqrt (+ (* a a) (* four b c))))
                            (* two c)))))))
           (setf eta
                   (/ tau
                      (+ (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                         (f2cl-lib:fsqrt
                          (+
                           (* (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                              (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%))
                           tau)))))
           (setf sigma (+ (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%) eta))
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (setf (f2cl-lib:fref delta-%data% (j) ((1 *)) delta-%offset%)
                       (- (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                          (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                          eta))
               (setf (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)
                       (+ (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                          (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                          eta))
              label30))
           (setf dpsi zero)
           (setf psi zero)
           (setf erretm zero)
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j ii) nil)
             (tagbody
               (setf temp
                       (/ (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                          (*
                           (f2cl-lib:fref delta-%data%
                                          (j)
                                          ((1 *))
                                          delta-%offset%)
                           (f2cl-lib:fref work-%data%
                                          (j)
                                          ((1 *))
                                          work-%offset%))))
               (setf psi
                       (+ psi
                          (* (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                             temp)))
               (setf dpsi (+ dpsi (* temp temp)))
               (setf erretm (+ erretm psi))
              label40))
           (setf erretm (abs erretm))
           (setf temp
                   (/ (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%)
                      (*
                       (f2cl-lib:fref delta-%data% (n) ((1 *)) delta-%offset%)
                       (f2cl-lib:fref work-%data% (n) ((1 *)) work-%offset%))))
           (setf phi (* (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%) temp))
           (setf dphi (* temp temp))
           (setf erretm
                   (+ (- (+ (* eight (- (- psi) phi)) erretm) phi)
                      rhoinv
                      (* (abs tau) (+ dpsi dphi))))
           (setf w (+ rhoinv phi psi))
           (cond
             ((<= (abs w) (* eps erretm))
              (go label240)))
           (setf niter (f2cl-lib:int-add niter 1))
           (setf dtnsq1
                   (*
                    (f2cl-lib:fref work-%data%
                                   ((f2cl-lib:int-sub n 1))
                                   ((1 *))
                                   work-%offset%)
                    (f2cl-lib:fref delta-%data%
                                   ((f2cl-lib:int-sub n 1))
                                   ((1 *))
                                   delta-%offset%)))
           (setf dtnsq
                   (* (f2cl-lib:fref work-%data% (n) ((1 *)) work-%offset%)
                      (f2cl-lib:fref delta-%data% (n) ((1 *)) delta-%offset%)))
           (setf c (- w (* dtnsq1 dpsi) (* dtnsq dphi)))
           (setf a
                   (+ (* (+ dtnsq dtnsq1) w)
                      (* (- dtnsq) dtnsq1 (+ dpsi dphi))))
           (setf b (* dtnsq dtnsq1 w))
           (if (< c zero) (setf c (abs c)))
           (cond
             ((= c zero)
              (setf eta (- rho (* sigma sigma))))
             ((>= a zero)
              (setf eta
                      (/
                       (+ a
                          (f2cl-lib:fsqrt (abs (+ (* a a) (* (- four) b c)))))
                       (* two c))))
             (t
              (setf eta
                      (/ (* two b)
                         (- a
                            (f2cl-lib:fsqrt
                             (abs (+ (* a a) (* (- four) b c)))))))))
           (if (> (* w eta) zero) (setf eta (/ (- w) (+ dpsi dphi))))
           (setf temp (- eta dtnsq))
           (if (> temp rho) (setf eta (+ rho dtnsq)))
           (setf tau (+ tau eta))
           (setf eta (/ eta (+ sigma (f2cl-lib:fsqrt (+ eta (* sigma sigma))))))
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (setf (f2cl-lib:fref delta-%data% (j) ((1 *)) delta-%offset%)
                       (-
                        (f2cl-lib:fref delta-%data% (j) ((1 *)) delta-%offset%)
                        eta))
               (setf (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)
                       (+ (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)
                          eta))
              label50))
           (setf sigma (+ sigma eta))
           (setf dpsi zero)
           (setf psi zero)
           (setf erretm zero)
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j ii) nil)
             (tagbody
               (setf temp
                       (/ (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                          (*
                           (f2cl-lib:fref work-%data%
                                          (j)
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref delta-%data%
                                          (j)
                                          ((1 *))
                                          delta-%offset%))))
               (setf psi
                       (+ psi
                          (* (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                             temp)))
               (setf dpsi (+ dpsi (* temp temp)))
               (setf erretm (+ erretm psi))
              label60))
           (setf erretm (abs erretm))
           (setf temp
                   (/ (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%)
                      (* (f2cl-lib:fref work-%data% (n) ((1 *)) work-%offset%)
                         (f2cl-lib:fref delta-%data%
                                        (n)
                                        ((1 *))
                                        delta-%offset%))))
           (setf phi (* (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%) temp))
           (setf dphi (* temp temp))
           (setf erretm
                   (+ (- (+ (* eight (- (- psi) phi)) erretm) phi)
                      rhoinv
                      (* (abs tau) (+ dpsi dphi))))
           (setf w (+ rhoinv phi psi))
           (setf iter (f2cl-lib:int-add niter 1))
           (f2cl-lib:fdo (niter iter (f2cl-lib:int-add niter 1))
                         ((> niter maxit) nil)
             (tagbody
               (cond
                 ((<= (abs w) (* eps erretm))
                  (go label240)))
               (setf dtnsq1
                       (*
                        (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-sub n 1))
                                       ((1 *))
                                       work-%offset%)
                        (f2cl-lib:fref delta-%data%
                                       ((f2cl-lib:int-sub n 1))
                                       ((1 *))
                                       delta-%offset%)))
               (setf dtnsq
                       (* (f2cl-lib:fref work-%data% (n) ((1 *)) work-%offset%)
                          (f2cl-lib:fref delta-%data%
                                         (n)
                                         ((1 *))
                                         delta-%offset%)))
               (setf c (- w (* dtnsq1 dpsi) (* dtnsq dphi)))
               (setf a
                       (+ (* (+ dtnsq dtnsq1) w)
                          (* (- dtnsq1) dtnsq (+ dpsi dphi))))
               (setf b (* dtnsq1 dtnsq w))
               (cond
                 ((>= a zero)
                  (setf eta
                          (/
                           (+ a
                              (f2cl-lib:fsqrt
                               (abs (+ (* a a) (* (- four) b c)))))
                           (* two c))))
                 (t
                  (setf eta
                          (/ (* two b)
                             (- a
                                (f2cl-lib:fsqrt
                                 (abs (+ (* a a) (* (- four) b c)))))))))
               (if (> (* w eta) zero) (setf eta (/ (- w) (+ dpsi dphi))))
               (setf temp (- eta dtnsq))
               (if (<= temp zero) (setf eta (/ eta two)))
               (setf tau (+ tau eta))
               (setf eta
                       (/ eta
                          (+ sigma (f2cl-lib:fsqrt (+ eta (* sigma sigma))))))
               (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                             ((> j n) nil)
                 (tagbody
                   (setf (f2cl-lib:fref delta-%data%
                                        (j)
                                        ((1 *))
                                        delta-%offset%)
                           (-
                            (f2cl-lib:fref delta-%data%
                                           (j)
                                           ((1 *))
                                           delta-%offset%)
                            eta))
                   (setf (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)
                           (+
                            (f2cl-lib:fref work-%data%
                                           (j)
                                           ((1 *))
                                           work-%offset%)
                            eta))
                  label70))
               (setf sigma (+ sigma eta))
               (setf dpsi zero)
               (setf psi zero)
               (setf erretm zero)
               (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                             ((> j ii) nil)
                 (tagbody
                   (setf temp
                           (/ (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                              (*
                               (f2cl-lib:fref work-%data%
                                              (j)
                                              ((1 *))
                                              work-%offset%)
                               (f2cl-lib:fref delta-%data%
                                              (j)
                                              ((1 *))
                                              delta-%offset%))))
                   (setf psi
                           (+ psi
                              (*
                               (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                               temp)))
                   (setf dpsi (+ dpsi (* temp temp)))
                   (setf erretm (+ erretm psi))
                  label80))
               (setf erretm (abs erretm))
               (setf temp
                       (/ (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%)
                          (*
                           (f2cl-lib:fref work-%data%
                                          (n)
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref delta-%data%
                                          (n)
                                          ((1 *))
                                          delta-%offset%))))
               (setf phi
                       (* (f2cl-lib:fref z-%data% (n) ((1 *)) z-%offset%)
                          temp))
               (setf dphi (* temp temp))
               (setf erretm
                       (+ (- (+ (* eight (- (- psi) phi)) erretm) phi)
                          rhoinv
                          (* (abs tau) (+ dpsi dphi))))
               (setf w (+ rhoinv phi psi))
              label90))
           (setf info 1)
           (go label240))
          (t
           (setf niter 1)
           (setf ip1 (f2cl-lib:int-add i 1))
           (setf delsq
                   (*
                    (- (f2cl-lib:fref d-%data% (ip1) ((1 *)) d-%offset%)
                       (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
                    (+ (f2cl-lib:fref d-%data% (ip1) ((1 *)) d-%offset%)
                       (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))))
           (setf delsq2 (/ delsq two))
           (setf temp
                   (/ delsq2
                      (+ (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                         (f2cl-lib:fsqrt
                          (+
                           (* (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                              (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
                           delsq2)))))
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (setf (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)
                       (+ (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                          (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                          temp))
               (setf (f2cl-lib:fref delta-%data% (j) ((1 *)) delta-%offset%)
                       (- (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                          (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                          temp))
              label100))
           (setf psi zero)
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j (f2cl-lib:int-add i (f2cl-lib:int-sub 1))) nil)
             (tagbody
               (setf psi
                       (+ psi
                          (/
                           (* (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                              (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%))
                           (*
                            (f2cl-lib:fref work-%data%
                                           (j)
                                           ((1 *))
                                           work-%offset%)
                            (f2cl-lib:fref delta-%data%
                                           (j)
                                           ((1 *))
                                           delta-%offset%)))))
              label110))
           (setf phi zero)
           (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                         ((> j (f2cl-lib:int-add i 2)) nil)
             (tagbody
               (setf phi
                       (+ phi
                          (/
                           (* (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                              (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%))
                           (*
                            (f2cl-lib:fref work-%data%
                                           (j)
                                           ((1 *))
                                           work-%offset%)
                            (f2cl-lib:fref delta-%data%
                                           (j)
                                           ((1 *))
                                           delta-%offset%)))))
              label120))
           (setf c (+ rhoinv psi phi))
           (setf w
                   (+ c
                      (/
                       (* (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                          (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%))
                       (* (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%)
                          (f2cl-lib:fref delta-%data%
                                         (i)
                                         ((1 *))
                                         delta-%offset%)))
                      (/
                       (* (f2cl-lib:fref z-%data% (ip1) ((1 *)) z-%offset%)
                          (f2cl-lib:fref z-%data% (ip1) ((1 *)) z-%offset%))
                       (*
                        (f2cl-lib:fref work-%data% (ip1) ((1 *)) work-%offset%)
                        (f2cl-lib:fref delta-%data%
                                       (ip1)
                                       ((1 *))
                                       delta-%offset%)))))
           (cond
             ((> w zero)
              (setf orgati f2cl-lib:%true%)
              (setf sg2lb zero)
              (setf sg2ub delsq2)
              (setf a
                      (+ (* c delsq)
                         (* (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                            (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%))
                         (* (f2cl-lib:fref z-%data% (ip1) ((1 *)) z-%offset%)
                            (f2cl-lib:fref z-%data% (ip1) ((1 *)) z-%offset%))))
              (setf b
                      (* (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                         (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                         delsq))
              (cond
                ((> a zero)
                 (setf tau
                         (/ (* two b)
                            (+ a
                               (f2cl-lib:fsqrt
                                (abs (+ (* a a) (* (- four) b c))))))))
                (t
                 (setf tau
                         (/
                          (- a
                             (f2cl-lib:fsqrt
                              (abs (+ (* a a) (* (- four) b c)))))
                          (* two c)))))
              (setf eta
                      (/ tau
                         (+ (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                            (f2cl-lib:fsqrt
                             (+
                              (*
                               (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                               (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
                              tau))))))
             (t
              (setf orgati f2cl-lib:%false%)
              (setf sg2lb (- delsq2))
              (setf sg2ub zero)
              (setf a
                      (- (* c delsq)
                         (* (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%)
                            (f2cl-lib:fref z-%data% (i) ((1 *)) z-%offset%))
                         (* (f2cl-lib:fref z-%data% (ip1) ((1 *)) z-%offset%)
                            (f2cl-lib:fref z-%data% (ip1) ((1 *)) z-%offset%))))
              (setf b
                      (* (f2cl-lib:fref z-%data% (ip1) ((1 *)) z-%offset%)
                         (f2cl-lib:fref z-%data% (ip1) ((1 *)) z-%offset%)
                         delsq))
              (cond
                ((< a zero)
                 (setf tau
                         (/ (* two b)
                            (- a
                               (f2cl-lib:fsqrt
                                (abs (+ (* a a) (* four b c))))))))
                (t
                 (setf tau
                         (/
                          (-
                           (+ a
                              (f2cl-lib:fsqrt (abs (+ (* a a) (* four b c))))))
                          (* two c)))))
              (setf eta
                      (/ tau
                         (+ (f2cl-lib:fref d-%data% (ip1) ((1 *)) d-%offset%)
                            (f2cl-lib:fsqrt
                             (abs
                              (+
                               (*
                                (f2cl-lib:fref d-%data%
                                               (ip1)
                                               ((1 *))
                                               d-%offset%)
                                (f2cl-lib:fref d-%data%
                                               (ip1)
                                               ((1 *))
                                               d-%offset%))
                               tau))))))))
           (cond
             (orgati
              (setf ii i)
              (setf sigma
                      (+ (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%) eta))
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (setf (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)
                          (+ (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                             (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                             eta))
                  (setf (f2cl-lib:fref delta-%data% (j) ((1 *)) delta-%offset%)
                          (- (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                             (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                             eta))
                 label130)))
             (t
              (setf ii (f2cl-lib:int-add i 1))
              (setf sigma
                      (+ (f2cl-lib:fref d-%data% (ip1) ((1 *)) d-%offset%) eta))
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (setf (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)
                          (+ (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                             (f2cl-lib:fref d-%data% (ip1) ((1 *)) d-%offset%)
                             eta))
                  (setf (f2cl-lib:fref delta-%data% (j) ((1 *)) delta-%offset%)
                          (- (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%)
                             (f2cl-lib:fref d-%data% (ip1) ((1 *)) d-%offset%)
                             eta))
                 label140))))
           (setf iim1 (f2cl-lib:int-sub ii 1))
           (setf iip1 (f2cl-lib:int-add ii 1))
           (setf dpsi zero)
           (setf psi zero)
           (setf erretm zero)
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j iim1) nil)
             (tagbody
               (setf temp
                       (/ (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                          (*
                           (f2cl-lib:fref work-%data%
                                          (j)
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref delta-%data%
                                          (j)
                                          ((1 *))
                                          delta-%offset%))))
               (setf psi
                       (+ psi
                          (* (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                             temp)))
               (setf dpsi (+ dpsi (* temp temp)))
               (setf erretm (+ erretm psi))
              label150))
           (setf erretm (abs erretm))
           (setf dphi zero)
           (setf phi zero)
           (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                         ((> j iip1) nil)
             (tagbody
               (setf temp
                       (/ (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                          (*
                           (f2cl-lib:fref work-%data%
                                          (j)
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref delta-%data%
                                          (j)
                                          ((1 *))
                                          delta-%offset%))))
               (setf phi
                       (+ phi
                          (* (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                             temp)))
               (setf dphi (+ dphi (* temp temp)))
               (setf erretm (+ erretm phi))
              label160))
           (setf w (+ rhoinv phi psi))
           (setf swtch3 f2cl-lib:%false%)
           (cond
             (orgati
              (if (< w zero) (setf swtch3 f2cl-lib:%true%)))
             (t
              (if (> w zero) (setf swtch3 f2cl-lib:%true%))))
           (if (or (= ii 1) (= ii n)) (setf swtch3 f2cl-lib:%false%))
           (setf temp
                   (/ (f2cl-lib:fref z-%data% (ii) ((1 *)) z-%offset%)
                      (* (f2cl-lib:fref work-%data% (ii) ((1 *)) work-%offset%)
                         (f2cl-lib:fref delta-%data%
                                        (ii)
                                        ((1 *))
                                        delta-%offset%))))
           (setf dw (+ dpsi dphi (* temp temp)))
           (setf temp (* (f2cl-lib:fref z-%data% (ii) ((1 *)) z-%offset%) temp))
           (setf w (+ w temp))
           (setf erretm
                   (+ (* eight (- phi psi))
                      erretm
                      (* two rhoinv)
                      (* three (abs temp))
                      (* (abs tau) dw)))
           (cond
             ((<= (abs w) (* eps erretm))
              (go label240)))
           (cond
             ((<= w zero)
              (setf sg2lb (max sg2lb tau)))
             (t
              (setf sg2ub (min sg2ub tau))))
           (setf niter (f2cl-lib:int-add niter 1))
           (cond
             ((not swtch3)
              (setf dtipsq
                      (*
                       (f2cl-lib:fref work-%data% (ip1) ((1 *)) work-%offset%)
                       (f2cl-lib:fref delta-%data%
                                      (ip1)
                                      ((1 *))
                                      delta-%offset%)))
              (setf dtisq
                      (* (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%)
                         (f2cl-lib:fref delta-%data%
                                        (i)
                                        ((1 *))
                                        delta-%offset%)))
              (cond
                (orgati
                 (setf c
                         (+ (- w (* dtipsq dw))
                            (* delsq
                               (expt
                                (/
                                 (f2cl-lib:fref z-%data%
                                                (i)
                                                ((1 *))
                                                z-%offset%)
                                 dtisq)
                                2)))))
                (t
                 (setf c
                         (- w
                            (* dtisq dw)
                            (* delsq
                               (expt
                                (/
                                 (f2cl-lib:fref z-%data%
                                                (ip1)
                                                ((1 *))
                                                z-%offset%)
                                 dtipsq)
                                2))))))
              (setf a (+ (* (+ dtipsq dtisq) w) (* (- dtipsq) dtisq dw)))
              (setf b (* dtipsq dtisq w))
              (cond
                ((= c zero)
                 (cond
                   ((= a zero)
                    (cond
                      (orgati
                       (setf a
                               (+
                                (*
                                 (f2cl-lib:fref z-%data%
                                                (i)
                                                ((1 *))
                                                z-%offset%)
                                 (f2cl-lib:fref z-%data%
                                                (i)
                                                ((1 *))
                                                z-%offset%))
                                (* dtipsq dtipsq (+ dpsi dphi)))))
                      (t
                       (setf a
                               (+
                                (*
                                 (f2cl-lib:fref z-%data%
                                                (ip1)
                                                ((1 *))
                                                z-%offset%)
                                 (f2cl-lib:fref z-%data%
                                                (ip1)
                                                ((1 *))
                                                z-%offset%))
                                (* dtisq dtisq (+ dpsi dphi))))))))
                 (setf eta (/ b a)))
                ((<= a zero)
                 (setf eta
                         (/
                          (- a
                             (f2cl-lib:fsqrt
                              (abs (+ (* a a) (* (- four) b c)))))
                          (* two c))))
                (t
                 (setf eta
                         (/ (* two b)
                            (+ a
                               (f2cl-lib:fsqrt
                                (abs (+ (* a a) (* (- four) b c))))))))))
             (t
              (setf dtiim
                      (*
                       (f2cl-lib:fref work-%data% (iim1) ((1 *)) work-%offset%)
                       (f2cl-lib:fref delta-%data%
                                      (iim1)
                                      ((1 *))
                                      delta-%offset%)))
              (setf dtiip
                      (*
                       (f2cl-lib:fref work-%data% (iip1) ((1 *)) work-%offset%)
                       (f2cl-lib:fref delta-%data%
                                      (iip1)
                                      ((1 *))
                                      delta-%offset%)))
              (setf temp (+ rhoinv psi phi))
              (cond
                (orgati
                 (setf temp1
                         (/ (f2cl-lib:fref z-%data% (iim1) ((1 *)) z-%offset%)
                            dtiim))
                 (setf temp1 (* temp1 temp1))
                 (setf c
                         (+ (- temp (* dtiip (+ dpsi dphi)))
                            (*
                             (-
                              (-
                               (f2cl-lib:fref d-%data%
                                              (iim1)
                                              ((1 *))
                                              d-%offset%)
                               (f2cl-lib:fref d-%data%
                                              (iip1)
                                              ((1 *))
                                              d-%offset%)))
                             (+
                              (f2cl-lib:fref d-%data%
                                             (iim1)
                                             ((1 *))
                                             d-%offset%)
                              (f2cl-lib:fref d-%data%
                                             (iip1)
                                             ((1 *))
                                             d-%offset%))
                             temp1)))
                 (setf (f2cl-lib:fref zz (1) ((1 3)))
                         (* (f2cl-lib:fref z-%data% (iim1) ((1 *)) z-%offset%)
                            (f2cl-lib:fref z-%data% (iim1) ((1 *)) z-%offset%)))
                 (cond
                   ((< dpsi temp1)
                    (setf (f2cl-lib:fref zz (3) ((1 3))) (* dtiip dtiip dphi)))
                   (t
                    (setf (f2cl-lib:fref zz (3) ((1 3)))
                            (* dtiip dtiip (+ (- dpsi temp1) dphi))))))
                (t
                 (setf temp1
                         (/ (f2cl-lib:fref z-%data% (iip1) ((1 *)) z-%offset%)
                            dtiip))
                 (setf temp1 (* temp1 temp1))
                 (setf c
                         (+ (- temp (* dtiim (+ dpsi dphi)))
                            (*
                             (-
                              (-
                               (f2cl-lib:fref d-%data%
                                              (iip1)
                                              ((1 *))
                                              d-%offset%)
                               (f2cl-lib:fref d-%data%
                                              (iim1)
                                              ((1 *))
                                              d-%offset%)))
                             (+
                              (f2cl-lib:fref d-%data%
                                             (iim1)
                                             ((1 *))
                                             d-%offset%)
                              (f2cl-lib:fref d-%data%
                                             (iip1)
                                             ((1 *))
                                             d-%offset%))
                             temp1)))
                 (cond
                   ((< dphi temp1)
                    (setf (f2cl-lib:fref zz (1) ((1 3))) (* dtiim dtiim dpsi)))
                   (t
                    (setf (f2cl-lib:fref zz (1) ((1 3)))
                            (* dtiim dtiim (+ dpsi (- dphi temp1))))))
                 (setf (f2cl-lib:fref zz (3) ((1 3)))
                         (* (f2cl-lib:fref z-%data% (iip1) ((1 *)) z-%offset%)
                            (f2cl-lib:fref z-%data%
                                           (iip1)
                                           ((1 *))
                                           z-%offset%)))))
              (setf (f2cl-lib:fref zz (2) ((1 3)))
                      (* (f2cl-lib:fref z-%data% (ii) ((1 *)) z-%offset%)
                         (f2cl-lib:fref z-%data% (ii) ((1 *)) z-%offset%)))
              (setf (f2cl-lib:fref dd (1) ((1 3))) dtiim)
              (setf (f2cl-lib:fref dd (2) ((1 3)))
                      (*
                       (f2cl-lib:fref delta-%data% (ii) ((1 *)) delta-%offset%)
                       (f2cl-lib:fref work-%data% (ii) ((1 *)) work-%offset%)))
              (setf (f2cl-lib:fref dd (3) ((1 3))) dtiip)
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                  (dlaed6 niter orgati c dd zz w eta info)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5))
                (setf eta var-6)
                (setf info var-7))
              (if (/= info 0) (go label240))))
           (if (>= (* w eta) zero) (setf eta (/ (- w) dw)))
           (cond
             (orgati
              (setf temp1
                      (* (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%)
                         (f2cl-lib:fref delta-%data%
                                        (i)
                                        ((1 *))
                                        delta-%offset%)))
              (setf temp (- eta temp1)))
             (t
              (setf temp1
                      (*
                       (f2cl-lib:fref work-%data% (ip1) ((1 *)) work-%offset%)
                       (f2cl-lib:fref delta-%data%
                                      (ip1)
                                      ((1 *))
                                      delta-%offset%)))
              (setf temp (- eta temp1))))
           (cond
             ((or (> temp sg2ub) (< temp sg2lb))
              (cond
                ((< w zero)
                 (setf eta (/ (- sg2ub tau) two)))
                (t
                 (setf eta (/ (- sg2lb tau) two))))))
           (setf tau (+ tau eta))
           (setf eta (/ eta (+ sigma (f2cl-lib:fsqrt (+ (* sigma sigma) eta)))))
           (setf prew w)
           (setf sigma (+ sigma eta))
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (setf (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)
                       (+ (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)
                          eta))
               (setf (f2cl-lib:fref delta-%data% (j) ((1 *)) delta-%offset%)
                       (-
                        (f2cl-lib:fref delta-%data% (j) ((1 *)) delta-%offset%)
                        eta))
              label170))
           (setf dpsi zero)
           (setf psi zero)
           (setf erretm zero)
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j iim1) nil)
             (tagbody
               (setf temp
                       (/ (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                          (*
                           (f2cl-lib:fref work-%data%
                                          (j)
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref delta-%data%
                                          (j)
                                          ((1 *))
                                          delta-%offset%))))
               (setf psi
                       (+ psi
                          (* (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                             temp)))
               (setf dpsi (+ dpsi (* temp temp)))
               (setf erretm (+ erretm psi))
              label180))
           (setf erretm (abs erretm))
           (setf dphi zero)
           (setf phi zero)
           (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                         ((> j iip1) nil)
             (tagbody
               (setf temp
                       (/ (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                          (*
                           (f2cl-lib:fref work-%data%
                                          (j)
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref delta-%data%
                                          (j)
                                          ((1 *))
                                          delta-%offset%))))
               (setf phi
                       (+ phi
                          (* (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                             temp)))
               (setf dphi (+ dphi (* temp temp)))
               (setf erretm (+ erretm phi))
              label190))
           (setf temp
                   (/ (f2cl-lib:fref z-%data% (ii) ((1 *)) z-%offset%)
                      (* (f2cl-lib:fref work-%data% (ii) ((1 *)) work-%offset%)
                         (f2cl-lib:fref delta-%data%
                                        (ii)
                                        ((1 *))
                                        delta-%offset%))))
           (setf dw (+ dpsi dphi (* temp temp)))
           (setf temp (* (f2cl-lib:fref z-%data% (ii) ((1 *)) z-%offset%) temp))
           (setf w (+ rhoinv phi psi temp))
           (setf erretm
                   (+ (* eight (- phi psi))
                      erretm
                      (* two rhoinv)
                      (* three (abs temp))
                      (* (abs tau) dw)))
           (cond
             ((<= w zero)
              (setf sg2lb (max sg2lb tau)))
             (t
              (setf sg2ub (min sg2ub tau))))
           (setf swtch f2cl-lib:%false%)
           (cond
             (orgati
              (if (> (- w) (/ (abs prew) ten)) (setf swtch f2cl-lib:%true%)))
             (t
              (if (> w (/ (abs prew) ten)) (setf swtch f2cl-lib:%true%))))
           (setf iter (f2cl-lib:int-add niter 1))
           (f2cl-lib:fdo (niter iter (f2cl-lib:int-add niter 1))
                         ((> niter maxit) nil)
             (tagbody
               (cond
                 ((<= (abs w) (* eps erretm))
                  (go label240)))
               (cond
                 ((not swtch3)
                  (setf dtipsq
                          (*
                           (f2cl-lib:fref work-%data%
                                          (ip1)
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref delta-%data%
                                          (ip1)
                                          ((1 *))
                                          delta-%offset%)))
                  (setf dtisq
                          (*
                           (f2cl-lib:fref work-%data%
                                          (i)
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref delta-%data%
                                          (i)
                                          ((1 *))
                                          delta-%offset%)))
                  (cond
                    ((not swtch)
                     (cond
                       (orgati
                        (setf c
                                (+ (- w (* dtipsq dw))
                                   (* delsq
                                      (expt
                                       (/
                                        (f2cl-lib:fref z-%data%
                                                       (i)
                                                       ((1 *))
                                                       z-%offset%)
                                        dtisq)
                                       2)))))
                       (t
                        (setf c
                                (- w
                                   (* dtisq dw)
                                   (* delsq
                                      (expt
                                       (/
                                        (f2cl-lib:fref z-%data%
                                                       (ip1)
                                                       ((1 *))
                                                       z-%offset%)
                                        dtipsq)
                                       2)))))))
                    (t
                     (setf temp
                             (/
                              (f2cl-lib:fref z-%data% (ii) ((1 *)) z-%offset%)
                              (*
                               (f2cl-lib:fref work-%data%
                                              (ii)
                                              ((1 *))
                                              work-%offset%)
                               (f2cl-lib:fref delta-%data%
                                              (ii)
                                              ((1 *))
                                              delta-%offset%))))
                     (cond
                       (orgati
                        (setf dpsi (+ dpsi (* temp temp))))
                       (t
                        (setf dphi (+ dphi (* temp temp)))))
                     (setf c (- w (* dtisq dpsi) (* dtipsq dphi)))))
                  (setf a (+ (* (+ dtipsq dtisq) w) (* (- dtipsq) dtisq dw)))
                  (setf b (* dtipsq dtisq w))
                  (cond
                    ((= c zero)
                     (cond
                       ((= a zero)
                        (cond
                          ((not swtch)
                           (cond
                             (orgati
                              (setf a
                                      (+
                                       (*
                                        (f2cl-lib:fref z-%data%
                                                       (i)
                                                       ((1 *))
                                                       z-%offset%)
                                        (f2cl-lib:fref z-%data%
                                                       (i)
                                                       ((1 *))
                                                       z-%offset%))
                                       (* dtipsq dtipsq (+ dpsi dphi)))))
                             (t
                              (setf a
                                      (+
                                       (*
                                        (f2cl-lib:fref z-%data%
                                                       (ip1)
                                                       ((1 *))
                                                       z-%offset%)
                                        (f2cl-lib:fref z-%data%
                                                       (ip1)
                                                       ((1 *))
                                                       z-%offset%))
                                       (* dtisq dtisq (+ dpsi dphi)))))))
                          (t
                           (setf a
                                   (+ (* dtisq dtisq dpsi)
                                      (* dtipsq dtipsq dphi)))))))
                     (setf eta (/ b a)))
                    ((<= a zero)
                     (setf eta
                             (/
                              (- a
                                 (f2cl-lib:fsqrt
                                  (abs (+ (* a a) (* (- four) b c)))))
                              (* two c))))
                    (t
                     (setf eta
                             (/ (* two b)
                                (+ a
                                   (f2cl-lib:fsqrt
                                    (abs (+ (* a a) (* (- four) b c))))))))))
                 (t
                  (setf dtiim
                          (*
                           (f2cl-lib:fref work-%data%
                                          (iim1)
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref delta-%data%
                                          (iim1)
                                          ((1 *))
                                          delta-%offset%)))
                  (setf dtiip
                          (*
                           (f2cl-lib:fref work-%data%
                                          (iip1)
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref delta-%data%
                                          (iip1)
                                          ((1 *))
                                          delta-%offset%)))
                  (setf temp (+ rhoinv psi phi))
                  (cond
                    (swtch
                     (setf c (- temp (* dtiim dpsi) (* dtiip dphi)))
                     (setf (f2cl-lib:fref zz (1) ((1 3))) (* dtiim dtiim dpsi))
                     (setf (f2cl-lib:fref zz (3) ((1 3))) (* dtiip dtiip dphi)))
                    (t
                     (cond
                       (orgati
                        (setf temp1
                                (/
                                 (f2cl-lib:fref z-%data%
                                                (iim1)
                                                ((1 *))
                                                z-%offset%)
                                 dtiim))
                        (setf temp1 (* temp1 temp1))
                        (setf temp2
                                (*
                                 (-
                                  (f2cl-lib:fref d-%data%
                                                 (iim1)
                                                 ((1 *))
                                                 d-%offset%)
                                  (f2cl-lib:fref d-%data%
                                                 (iip1)
                                                 ((1 *))
                                                 d-%offset%))
                                 (+
                                  (f2cl-lib:fref d-%data%
                                                 (iim1)
                                                 ((1 *))
                                                 d-%offset%)
                                  (f2cl-lib:fref d-%data%
                                                 (iip1)
                                                 ((1 *))
                                                 d-%offset%))
                                 temp1))
                        (setf c (- temp (* dtiip (+ dpsi dphi)) temp2))
                        (setf (f2cl-lib:fref zz (1) ((1 3)))
                                (*
                                 (f2cl-lib:fref z-%data%
                                                (iim1)
                                                ((1 *))
                                                z-%offset%)
                                 (f2cl-lib:fref z-%data%
                                                (iim1)
                                                ((1 *))
                                                z-%offset%)))
                        (cond
                          ((< dpsi temp1)
                           (setf (f2cl-lib:fref zz (3) ((1 3)))
                                   (* dtiip dtiip dphi)))
                          (t
                           (setf (f2cl-lib:fref zz (3) ((1 3)))
                                   (* dtiip dtiip (+ (- dpsi temp1) dphi))))))
                       (t
                        (setf temp1
                                (/
                                 (f2cl-lib:fref z-%data%
                                                (iip1)
                                                ((1 *))
                                                z-%offset%)
                                 dtiip))
                        (setf temp1 (* temp1 temp1))
                        (setf temp2
                                (*
                                 (-
                                  (f2cl-lib:fref d-%data%
                                                 (iip1)
                                                 ((1 *))
                                                 d-%offset%)
                                  (f2cl-lib:fref d-%data%
                                                 (iim1)
                                                 ((1 *))
                                                 d-%offset%))
                                 (+
                                  (f2cl-lib:fref d-%data%
                                                 (iim1)
                                                 ((1 *))
                                                 d-%offset%)
                                  (f2cl-lib:fref d-%data%
                                                 (iip1)
                                                 ((1 *))
                                                 d-%offset%))
                                 temp1))
                        (setf c (- temp (* dtiim (+ dpsi dphi)) temp2))
                        (cond
                          ((< dphi temp1)
                           (setf (f2cl-lib:fref zz (1) ((1 3)))
                                   (* dtiim dtiim dpsi)))
                          (t
                           (setf (f2cl-lib:fref zz (1) ((1 3)))
                                   (* dtiim dtiim (+ dpsi (- dphi temp1))))))
                        (setf (f2cl-lib:fref zz (3) ((1 3)))
                                (*
                                 (f2cl-lib:fref z-%data%
                                                (iip1)
                                                ((1 *))
                                                z-%offset%)
                                 (f2cl-lib:fref z-%data%
                                                (iip1)
                                                ((1 *))
                                                z-%offset%)))))))
                  (setf (f2cl-lib:fref dd (1) ((1 3))) dtiim)
                  (setf (f2cl-lib:fref dd (2) ((1 3)))
                          (*
                           (f2cl-lib:fref delta-%data%
                                          (ii)
                                          ((1 *))
                                          delta-%offset%)
                           (f2cl-lib:fref work-%data%
                                          (ii)
                                          ((1 *))
                                          work-%offset%)))
                  (setf (f2cl-lib:fref dd (3) ((1 3))) dtiip)
                  (multiple-value-bind
                        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                      (dlaed6 niter orgati c dd zz w eta info)
                    (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5))
                    (setf eta var-6)
                    (setf info var-7))
                  (if (/= info 0) (go label240))))
               (if (>= (* w eta) zero) (setf eta (/ (- w) dw)))
               (cond
                 (orgati
                  (setf temp1
                          (*
                           (f2cl-lib:fref work-%data%
                                          (i)
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref delta-%data%
                                          (i)
                                          ((1 *))
                                          delta-%offset%)))
                  (setf temp (- eta temp1)))
                 (t
                  (setf temp1
                          (*
                           (f2cl-lib:fref work-%data%
                                          (ip1)
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref delta-%data%
                                          (ip1)
                                          ((1 *))
                                          delta-%offset%)))
                  (setf temp (- eta temp1))))
               (cond
                 ((or (> temp sg2ub) (< temp sg2lb))
                  (cond
                    ((< w zero)
                     (setf eta (/ (- sg2ub tau) two)))
                    (t
                     (setf eta (/ (- sg2lb tau) two))))))
               (setf tau (+ tau eta))
               (setf eta
                       (/ eta
                          (+ sigma (f2cl-lib:fsqrt (+ (* sigma sigma) eta)))))
               (setf sigma (+ sigma eta))
               (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                             ((> j n) nil)
                 (tagbody
                   (setf (f2cl-lib:fref work-%data% (j) ((1 *)) work-%offset%)
                           (+
                            (f2cl-lib:fref work-%data%
                                           (j)
                                           ((1 *))
                                           work-%offset%)
                            eta))
                   (setf (f2cl-lib:fref delta-%data%
                                        (j)
                                        ((1 *))
                                        delta-%offset%)
                           (-
                            (f2cl-lib:fref delta-%data%
                                           (j)
                                           ((1 *))
                                           delta-%offset%)
                            eta))
                  label200))
               (setf prew w)
               (setf dpsi zero)
               (setf psi zero)
               (setf erretm zero)
               (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                             ((> j iim1) nil)
                 (tagbody
                   (setf temp
                           (/ (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                              (*
                               (f2cl-lib:fref work-%data%
                                              (j)
                                              ((1 *))
                                              work-%offset%)
                               (f2cl-lib:fref delta-%data%
                                              (j)
                                              ((1 *))
                                              delta-%offset%))))
                   (setf psi
                           (+ psi
                              (*
                               (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                               temp)))
                   (setf dpsi (+ dpsi (* temp temp)))
                   (setf erretm (+ erretm psi))
                  label210))
               (setf erretm (abs erretm))
               (setf dphi zero)
               (setf phi zero)
               (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                             ((> j iip1) nil)
                 (tagbody
                   (setf temp
                           (/ (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                              (*
                               (f2cl-lib:fref work-%data%
                                              (j)
                                              ((1 *))
                                              work-%offset%)
                               (f2cl-lib:fref delta-%data%
                                              (j)
                                              ((1 *))
                                              delta-%offset%))))
                   (setf phi
                           (+ phi
                              (*
                               (f2cl-lib:fref z-%data% (j) ((1 *)) z-%offset%)
                               temp)))
                   (setf dphi (+ dphi (* temp temp)))
                   (setf erretm (+ erretm phi))
                  label220))
               (setf temp
                       (/ (f2cl-lib:fref z-%data% (ii) ((1 *)) z-%offset%)
                          (*
                           (f2cl-lib:fref work-%data%
                                          (ii)
                                          ((1 *))
                                          work-%offset%)
                           (f2cl-lib:fref delta-%data%
                                          (ii)
                                          ((1 *))
                                          delta-%offset%))))
               (setf dw (+ dpsi dphi (* temp temp)))
               (setf temp
                       (* (f2cl-lib:fref z-%data% (ii) ((1 *)) z-%offset%)
                          temp))
               (setf w (+ rhoinv phi psi temp))
               (setf erretm
                       (+ (* eight (- phi psi))
                          erretm
                          (* two rhoinv)
                          (* three (abs temp))
                          (* (abs tau) dw)))
               (if (and (> (* w prew) zero) (> (abs w) (/ (abs prew) ten)))
                   (setf swtch (not swtch)))
               (cond
                 ((<= w zero)
                  (setf sg2lb (max sg2lb tau)))
                 (t
                  (setf sg2ub (min sg2ub tau))))
              label230))
           (setf info 1)))
       label240
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil sigma nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasd4
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (double-float) (double-float)
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil fortran-to-lisp::sigma nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlaed6 fortran-to-lisp::dlamch
                    fortran-to-lisp::dlasd5))))

