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


(defun cobylb
       (n m mpp x rhobeg rhoend iprint maxfun con sim simi datmat a vsig veta
        sigbar dx w iact ierr)
  (declare (type (array f2cl-lib:integer4 (*)) iact)
           (type double-float rhoend rhobeg)
           (type (array double-float (*)) w dx sigbar veta vsig a datmat simi
                                          sim con x)
           (type (f2cl-lib:integer4) ierr maxfun iprint mpp m n))
  (f2cl-lib:with-multi-array-data
      ((x double-float x-%data% x-%offset%)
       (con double-float con-%data% con-%offset%)
       (sim double-float sim-%data% sim-%offset%)
       (simi double-float simi-%data% simi-%offset%)
       (datmat double-float datmat-%data% datmat-%offset%)
       (a double-float a-%data% a-%offset%)
       (vsig double-float vsig-%data% vsig-%offset%)
       (veta double-float veta-%data% veta-%offset%)
       (sigbar double-float sigbar-%data% sigbar-%offset%)
       (dx double-float dx-%data% dx-%offset%)
       (w double-float w-%data% w-%offset%)
       (iact f2cl-lib:integer4 iact-%data% iact-%offset%))
    (prog ((cmax 0.0) (cmin 0.0) (denom 0.0) (l 0) (edgmax 0.0) (ratio 0.0)
           (trured 0.0) (vmnew 0.0) (vmold 0.0) (prerem 0.0) (phi 0.0)
           (prerec 0.0) (barmu 0.0) (resnew 0.0) (ifull 0) (ivmd 0) (idxnew 0)
           (isdirn 0) (ivmc 0) (izdota 0) (iz 0) (dxsign 0.0) (sum 0.0)
           (cvmaxm 0.0) (cvmaxp 0.0) (weta 0.0) (wsig 0.0) (pareta 0.0)
           (parsig 0.0) (iflag 0) (error$ 0.0) (tempa 0.0) (nbest 0)
           (phimin 0.0) (k 0) (resmax 0.0) (f 0.0) (ibrnch 0) (jdrop 0) (j 0)
           (i 0) (temp 0.0) (nfvals 0) (parmu 0.0) (rho 0.0) (delta 0.0)
           (gamma 0.0) (beta 0.0) (alpha 0.0) (mp 0) (np 0) (iptemp 0)
           (iptem 0))
      (declare (type (f2cl-lib:integer4) iptem iptemp np mp nfvals i j jdrop
                                         ibrnch k nbest iflag iz izdota ivmc
                                         isdirn idxnew ivmd ifull l)
               (type double-float alpha beta gamma delta rho parmu temp f
                                  resmax phimin tempa error$ parsig pareta wsig
                                  weta cvmaxp cvmaxm sum dxsign resnew barmu
                                  prerec phi prerem vmold vmnew trured ratio
                                  edgmax denom cmin cmax))
      '""
      '"     set the initial values of some parameters. the last column of sim"
      '"     the optimal vertex of the current simplex, and the preceding n col"
      '"     hold the displacements from the optimal vertex to the other vertic"
      '"     further, simi holds the inverse of the matrix that is contained in"
      '"     first n columns of sim."
      '""
      (setf ierr 0)
      (setf iptem (f2cl-lib:min0 n 5))
      (setf iptemp (f2cl-lib:int-add iptem 1))
      (setf np (f2cl-lib:int-add n 1))
      (setf mp (f2cl-lib:int-add m 1))
      (setf alpha 0.25)
      (setf beta 2.1)
      (setf gamma 0.5)
      (setf delta 1.1)
      (setf rho rhobeg)
      (setf parmu 0.0)
      (if (>= iprint 2)
          (f2cl-lib:fformat t
                            ("~%" "~3@T" "The initial value of RHO is" 1
                             (("~13,6,2,1,'*,,'EE")) "~2@T"
                             "and PARMU is set to zero." "~%")
                            rho))
      (setf nfvals 0)
      (setf temp (/ 1.0 rho))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref sim-%data% (i np) ((1 n) (1 *)) sim-%offset%)
                  (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (setf (f2cl-lib:fref sim-%data% (i j) ((1 n) (1 *)) sim-%offset%)
                      0.0)
             label20
              (setf (f2cl-lib:fref simi-%data%
                                   (i j)
                                   ((1 n) (1 *))
                                   simi-%offset%)
                      0.0)))
          (setf (f2cl-lib:fref sim-%data% (i i) ((1 n) (1 *)) sim-%offset%)
                  rho)
         label30
          (setf (f2cl-lib:fref simi-%data% (i i) ((1 n) (1 *)) simi-%offset%)
                  temp)))
      (setf jdrop np)
      (setf ibrnch 0)
      '""
      '"     make the next call of the user-supplied subroutine calcfc. these"
      '"     instructions are also used for calling calcfc during the iteration"
      '"     the algorithm."
      '""
     label40
      (cond
        ((and (>= nfvals maxfun) (> nfvals 0))
         (if (>= iprint 1)
             (f2cl-lib:fformat t
                               ("~%" "~3@T"
                                "Return from subroutine COBYLA because the "
                                "MAXFUN limit has been reached." "~%")))
         (setf ierr 1)
         (go label600)))
      (setf nfvals (f2cl-lib:int-add nfvals 1))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
          (calcfc n m x f con)
        (declare (ignore var-0 var-1 var-2 var-4))
        (setf f var-3))
      (setf resmax 0.0)
      (cond
        ((> m 0)
         (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                       ((> k m) nil)
           (tagbody
            label60
             (setf resmax
                     (f2cl-lib:dmax1 resmax
                                     (-
                                      (f2cl-lib:fref con-%data%
                                                     (k)
                                                     ((1 *))
                                                     con-%offset%))))))))
      (cond
        ((or (= nfvals (f2cl-lib:int-add iprint (f2cl-lib:int-sub 1)))
             (= iprint 3))
         (f2cl-lib:fformat t
                           ("~%" "~3@T" "NFVALS =" 1 (("~5D")) "~3@T" "F =" 1
                            (("~13,6,2,1,'*,,'EE")) "~4@T" "MAXCV =" 1
                            (("~13,6,2,1,'*,,'EE")) "~%" "~3@T" "X =" 1
                            (("~13,6,2,1,'*,,'EE")) 4 (("~15,6,2,1,'*,,'EE"))
                            "~%")
                           nfvals
                           f
                           resmax
                           (do ((i 1 (f2cl-lib:int-add i 1))
                                (%ret nil))
                               ((> i iptem) (nreverse %ret))
                             (declare (type f2cl-lib:integer4 i))
                             (push
                              (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%)
                              %ret)))
         (if (< iptem n)
             (f2cl-lib:fformat t
                               (1 (("~19,6,2,1,'*,,'EE")) 4
                                (("~15,6,2,1,'*,,'EE")) "~%")
                               (do ((i iptemp (f2cl-lib:int-add i 1))
                                    (%ret nil))
                                   ((> i n) (nreverse %ret))
                                 (declare (type f2cl-lib:integer4 i))
                                 (push
                                  (f2cl-lib:fref x-%data%
                                                 (i)
                                                 ((1 *))
                                                 x-%offset%)
                                  %ret))))))
      (setf (f2cl-lib:fref con-%data% (mp) ((1 *)) con-%offset%) f)
      (setf (f2cl-lib:fref con-%data% (mpp) ((1 *)) con-%offset%) resmax)
      (if (= ibrnch 1) (go label440))
      '""
      '"     set the recently calculated function values in a column of datmat."
      '"     array has a column for each vertex of the current simplex, the ent"
      '"     each column being the values of the constraint functions (if any)"
      '"     followed by the objective function and the greatest constraint vio"
      '"     at the vertex."
      '""
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k mpp) nil)
        (tagbody
         label90
          (setf (f2cl-lib:fref datmat-%data%
                               (k jdrop)
                               ((1 mpp) (1 *))
                               datmat-%offset%)
                  (f2cl-lib:fref con-%data% (k) ((1 *)) con-%offset%))))
      (if (> nfvals np) (go label130))
      '""
      '"     exchange the new vertex of the initial simplex with the optimal ve"
      '"     necessary. then, if the initial simplex is not complete, pick its"
      '"     vertex and calculate the function values there."
      '""
      (cond
        ((<= jdrop n)
         (cond
           ((<= (f2cl-lib:fref datmat (mp np) ((1 mpp) (1 *))) f)
            (setf (f2cl-lib:fref x-%data% (jdrop) ((1 *)) x-%offset%)
                    (f2cl-lib:fref sim-%data%
                                   (jdrop np)
                                   ((1 n) (1 *))
                                   sim-%offset%)))
           (t
            (setf (f2cl-lib:fref sim-%data%
                                 (jdrop np)
                                 ((1 n) (1 *))
                                 sim-%offset%)
                    (f2cl-lib:fref x-%data% (jdrop) ((1 *)) x-%offset%))
            (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                          ((> k mpp) nil)
              (tagbody
                (setf (f2cl-lib:fref datmat-%data%
                                     (k jdrop)
                                     ((1 mpp) (1 *))
                                     datmat-%offset%)
                        (f2cl-lib:fref datmat-%data%
                                       (k np)
                                       ((1 mpp) (1 *))
                                       datmat-%offset%))
               label100
                (setf (f2cl-lib:fref datmat-%data%
                                     (k np)
                                     ((1 mpp) (1 *))
                                     datmat-%offset%)
                        (f2cl-lib:fref con-%data% (k) ((1 *)) con-%offset%))))
            (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                          ((> k jdrop) nil)
              (tagbody
                (setf (f2cl-lib:fref sim-%data%
                                     (jdrop k)
                                     ((1 n) (1 *))
                                     sim-%offset%)
                        (- rho))
                (setf temp (coerce 0.0f0 'double-float))
                (f2cl-lib:fdo (i k (f2cl-lib:int-add i 1))
                              ((> i jdrop) nil)
                  (tagbody
                   label110
                    (setf temp
                            (- temp
                               (f2cl-lib:fref simi-%data%
                                              (i k)
                                              ((1 n) (1 *))
                                              simi-%offset%)))))
               label120
                (setf (f2cl-lib:fref simi-%data%
                                     (jdrop k)
                                     ((1 n) (1 *))
                                     simi-%offset%)
                        temp)))))))
      (cond
        ((<= nfvals n)
         (setf jdrop nfvals)
         (setf (f2cl-lib:fref x-%data% (jdrop) ((1 *)) x-%offset%)
                 (+ (f2cl-lib:fref x-%data% (jdrop) ((1 *)) x-%offset%) rho))
         (go label40)))
     label130
      (setf ibrnch 1)
      '""
      '"     identify the optimal vertex of the current simplex."
      '""
     label140
      (setf phimin
              (+
               (f2cl-lib:fref datmat-%data%
                              (mp np)
                              ((1 mpp) (1 *))
                              datmat-%offset%)
               (* parmu
                  (f2cl-lib:fref datmat-%data%
                                 (mpp np)
                                 ((1 mpp) (1 *))
                                 datmat-%offset%))))
      (setf nbest np)
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (setf temp
                  (+
                   (f2cl-lib:fref datmat-%data%
                                  (mp j)
                                  ((1 mpp) (1 *))
                                  datmat-%offset%)
                   (* parmu
                      (f2cl-lib:fref datmat-%data%
                                     (mpp j)
                                     ((1 mpp) (1 *))
                                     datmat-%offset%))))
          (cond
            ((< temp phimin)
             (setf nbest j)
             (setf phimin temp))
            ((and (= temp phimin) (= parmu 0.0))
             (if
              (<
               (f2cl-lib:fref datmat-%data%
                              (mpp j)
                              ((1 mpp) (1 *))
                              datmat-%offset%)
               (f2cl-lib:fref datmat-%data%
                              (mpp nbest)
                              ((1 mpp) (1 *))
                              datmat-%offset%))
              (setf nbest j))))
         label150))
      '""
      '"     switch the best vertex into pole position if it is not there alrea"
      '"     and also update sim, simi and datmat."
      '""
      (cond
        ((<= nbest n)
         (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                       ((> i mpp) nil)
           (tagbody
             (setf temp
                     (f2cl-lib:fref datmat-%data%
                                    (i np)
                                    ((1 mpp) (1 *))
                                    datmat-%offset%))
             (setf (f2cl-lib:fref datmat-%data%
                                  (i np)
                                  ((1 mpp) (1 *))
                                  datmat-%offset%)
                     (f2cl-lib:fref datmat-%data%
                                    (i nbest)
                                    ((1 mpp) (1 *))
                                    datmat-%offset%))
            label160
             (setf (f2cl-lib:fref datmat-%data%
                                  (i nbest)
                                  ((1 mpp) (1 *))
                                  datmat-%offset%)
                     temp)))
         (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                       ((> i n) nil)
           (tagbody
             (setf temp
                     (f2cl-lib:fref sim-%data%
                                    (i nbest)
                                    ((1 n) (1 *))
                                    sim-%offset%))
             (setf (f2cl-lib:fref sim-%data%
                                  (i nbest)
                                  ((1 n) (1 *))
                                  sim-%offset%)
                     0.0)
             (setf (f2cl-lib:fref sim-%data% (i np) ((1 n) (1 *)) sim-%offset%)
                     (+
                      (f2cl-lib:fref sim-%data%
                                     (i np)
                                     ((1 n) (1 *))
                                     sim-%offset%)
                      temp))
             (setf tempa 0.0)
             (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                           ((> k n) nil)
               (tagbody
                 (setf (f2cl-lib:fref sim-%data%
                                      (i k)
                                      ((1 n) (1 *))
                                      sim-%offset%)
                         (-
                          (f2cl-lib:fref sim-%data%
                                         (i k)
                                         ((1 n) (1 *))
                                         sim-%offset%)
                          temp))
                label170
                 (setf tempa
                         (- tempa
                            (f2cl-lib:fref simi-%data%
                                           (k i)
                                           ((1 n) (1 *))
                                           simi-%offset%)))))
            label180
             (setf (f2cl-lib:fref simi-%data%
                                  (nbest i)
                                  ((1 n) (1 *))
                                  simi-%offset%)
                     tempa)))))
      '""
      '"     make an error return if sigi is a poor approximation to the invers"
      '"     the leading n by n submatrix of sig."
      '""
      (setf error$ 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (setf temp 0.0)
              (if (= i j) (setf temp (- temp 1.0)))
              (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                            ((> k n) nil)
                (tagbody
                 label190
                  (setf temp
                          (+ temp
                             (*
                              (f2cl-lib:fref simi-%data%
                                             (i k)
                                             ((1 n) (1 *))
                                             simi-%offset%)
                              (f2cl-lib:fref sim-%data%
                                             (k j)
                                             ((1 n) (1 *))
                                             sim-%offset%))))))
              (setf error$ (f2cl-lib:dmax1 error$ (abs temp)))))))
     label200
      (cond
        ((> error$ 0.1)
         (if (>= iprint 1)
             (f2cl-lib:fformat t
                               ("~%" "~3@T"
                                "Return from subroutine COBYLA because "
                                "rounding errors are becoming damaging." "~%")))
         (setf ierr 2)
         (go label600)))
      '""
      '"     calculate the coefficients of the linear approximations to the obj"
      '"     and constraint functions, placing minus the objective function gra"
      '"     after the constraint gradients in the array a. the vector w is use"
      '"     working space."
      '""
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k mp) nil)
        (tagbody
          (setf (f2cl-lib:fref con-%data% (k) ((1 *)) con-%offset%)
                  (-
                   (f2cl-lib:fref datmat-%data%
                                  (k np)
                                  ((1 mpp) (1 *))
                                  datmat-%offset%)))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
             label220
              (setf (f2cl-lib:fref w-%data% (j) ((1 *)) w-%offset%)
                      (+
                       (f2cl-lib:fref datmat-%data%
                                      (k j)
                                      ((1 mpp) (1 *))
                                      datmat-%offset%)
                       (f2cl-lib:fref con-%data% (k) ((1 *)) con-%offset%)))))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf temp 0.0)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                 label230
                  (setf temp
                          (+ temp
                             (* (f2cl-lib:fref w-%data% (j) ((1 *)) w-%offset%)
                                (f2cl-lib:fref simi-%data%
                                               (j i)
                                               ((1 n) (1 *))
                                               simi-%offset%))))))
              (if (= k mp) (setf temp (- temp)))
              (setf (f2cl-lib:fref a-%data% (i k) ((1 n) (1 *)) a-%offset%)
                      temp)))))
     label240
      '""
      '"     calculate the values of sigma and eta, and set iflag=0 if the curr"
      '"     simplex is not acceptable."
      '""
      (setf iflag 1)
      (setf parsig (* alpha rho))
      (setf pareta (* beta rho))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (setf wsig 0.0)
          (setf weta 0.0)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf wsig
                      (+ wsig
                         (expt
                          (f2cl-lib:fref simi-%data%
                                         (j i)
                                         ((1 n) (1 *))
                                         simi-%offset%)
                          2)))
             label250
              (setf weta
                      (+ weta
                         (expt
                          (f2cl-lib:fref sim-%data%
                                         (i j)
                                         ((1 n) (1 *))
                                         sim-%offset%)
                          2)))))
          (setf (f2cl-lib:fref vsig-%data% (j) ((1 *)) vsig-%offset%)
                  (/ 1.0 (f2cl-lib:fsqrt wsig)))
          (setf (f2cl-lib:fref veta-%data% (j) ((1 *)) veta-%offset%)
                  (f2cl-lib:fsqrt weta))
          (if
           (or (< (f2cl-lib:fref vsig-%data% (j) ((1 *)) vsig-%offset%) parsig)
               (> (f2cl-lib:fref veta-%data% (j) ((1 *)) veta-%offset%)
                  pareta))
           (setf iflag 0))
         label260))
      '""
      '"     if a new vertex is needed to improve acceptability, then decide wh"
      '"     vertex to drop from the simplex."
      '""
      (if (or (= ibrnch 1) (= iflag 1)) (go label370))
      (setf jdrop 0)
      (setf temp pareta)
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (cond
            ((> (f2cl-lib:fref veta (j) ((1 *))) temp)
             (setf jdrop j)
             (setf temp
                     (f2cl-lib:fref veta-%data% (j) ((1 *)) veta-%offset%))))
         label270))
      (cond
        ((= jdrop 0)
         (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                       ((> j n) nil)
           (tagbody
             (cond
               ((< (f2cl-lib:fref vsig (j) ((1 *))) temp)
                (setf jdrop j)
                (setf temp
                        (f2cl-lib:fref vsig-%data%
                                       (j)
                                       ((1 *))
                                       vsig-%offset%))))
            label280))))
      '""
      '"     calculate the step to the new vertex and its sign."
      '""
      (setf temp
              (* gamma
                 rho
                 (f2cl-lib:fref vsig-%data% (jdrop) ((1 *)) vsig-%offset%)))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label290
          (setf (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                  (* temp
                     (f2cl-lib:fref simi-%data%
                                    (jdrop i)
                                    ((1 n) (1 *))
                                    simi-%offset%)))))
      (setf cvmaxp 0.0)
      (setf cvmaxm 0.0)
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k mp) nil)
        (tagbody
          (setf sum 0.0)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label300
              (setf sum
                      (+ sum
                         (*
                          (f2cl-lib:fref a-%data%
                                         (i k)
                                         ((1 n) (1 *))
                                         a-%offset%)
                          (f2cl-lib:fref dx-%data%
                                         (i)
                                         ((1 *))
                                         dx-%offset%))))))
          (cond
            ((< k mp)
             (setf temp
                     (f2cl-lib:fref datmat-%data%
                                    (k np)
                                    ((1 mpp) (1 *))
                                    datmat-%offset%))
             (setf cvmaxp (f2cl-lib:dmax1 cvmaxp (- (- temp) sum)))
             (setf cvmaxm (f2cl-lib:dmax1 cvmaxm (- sum temp)))))
         label310))
      (setf dxsign 1.0)
      (if (> (* parmu (- cvmaxp cvmaxm)) (+ sum sum)) (setf dxsign -1.0))
      '""
      '"     update the elements of sim and simi, and set the next x."
      '""
      (setf temp 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                  (* dxsign (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)))
          (setf (f2cl-lib:fref sim-%data% (i jdrop) ((1 n) (1 *)) sim-%offset%)
                  (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))
         label320
          (setf temp
                  (+ temp
                     (*
                      (f2cl-lib:fref simi-%data%
                                     (jdrop i)
                                     ((1 n) (1 *))
                                     simi-%offset%)
                      (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))))))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label330
          (setf (f2cl-lib:fref simi-%data%
                               (jdrop i)
                               ((1 n) (1 *))
                               simi-%offset%)
                  (/
                   (f2cl-lib:fref simi-%data%
                                  (jdrop i)
                                  ((1 n) (1 *))
                                  simi-%offset%)
                   temp))))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (cond
            ((/= j jdrop)
             (setf temp 0.0)
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i n) nil)
               (tagbody
                label340
                 (setf temp
                         (+ temp
                            (*
                             (f2cl-lib:fref simi-%data%
                                            (j i)
                                            ((1 n) (1 *))
                                            simi-%offset%)
                             (f2cl-lib:fref dx-%data%
                                            (i)
                                            ((1 *))
                                            dx-%offset%))))))
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i n) nil)
               (tagbody
                label350
                 (setf (f2cl-lib:fref simi-%data%
                                      (j i)
                                      ((1 n) (1 *))
                                      simi-%offset%)
                         (-
                          (f2cl-lib:fref simi-%data%
                                         (j i)
                                         ((1 n) (1 *))
                                         simi-%offset%)
                          (* temp
                             (f2cl-lib:fref simi-%data%
                                            (jdrop i)
                                            ((1 n) (1 *))
                                            simi-%offset%))))))))
         label360
          (setf (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)
                  (+
                   (f2cl-lib:fref sim-%data% (j np) ((1 n) (1 *)) sim-%offset%)
                   (f2cl-lib:fref dx-%data% (j) ((1 *)) dx-%offset%)))))
      (go label40)
      '""
      '"     calculate dx=x(*)-x(0). branch if the length of dx is less than 0."
      '""
     label370
      (setf iz 1)
      (setf izdota (f2cl-lib:int-add iz (f2cl-lib:int-mul n n)))
      (setf ivmc (f2cl-lib:int-add izdota n))
      (setf isdirn (f2cl-lib:int-add ivmc mp))
      (setf idxnew (f2cl-lib:int-add isdirn n))
      (setf ivmd (f2cl-lib:int-add idxnew n))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12 var-13)
          (trstlp n m a con rho dx ifull iact
           (f2cl-lib:array-slice w double-float (iz) ((1 *)))
           (f2cl-lib:array-slice w double-float (izdota) ((1 *)))
           (f2cl-lib:array-slice w double-float (ivmc) ((1 *)))
           (f2cl-lib:array-slice w double-float (isdirn) ((1 *)))
           (f2cl-lib:array-slice w double-float (idxnew) ((1 *)))
           (f2cl-lib:array-slice w double-float (ivmd) ((1 *))))
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-7 var-8 var-9
                         var-10 var-11 var-12 var-13))
        (setf ifull var-6))
      (cond
        ((= ifull 0)
         (setf temp 0.0)
         (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                       ((> i n) nil)
           (tagbody
            label380
             (setf temp
                     (+ temp
                        (expt (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                              2)))))
         (cond
           ((< temp (* 0.25 rho rho))
            (setf ibrnch 1)
            (go label550)))))
      '""
      '"     predict the change to f and the new maximum constraint violation i"
      '"     variables are altered from x(0) to x(0)+dx."
      '""
      (setf resnew 0.0)
      (setf (f2cl-lib:fref con-%data% (mp) ((1 *)) con-%offset%) 0.0)
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k mp) nil)
        (tagbody
          (setf sum (f2cl-lib:fref con-%data% (k) ((1 *)) con-%offset%))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label390
              (setf sum
                      (- sum
                         (*
                          (f2cl-lib:fref a-%data%
                                         (i k)
                                         ((1 n) (1 *))
                                         a-%offset%)
                          (f2cl-lib:fref dx-%data%
                                         (i)
                                         ((1 *))
                                         dx-%offset%))))))
          (if (< k mp) (setf resnew (f2cl-lib:dmax1 resnew sum)))
         label400))
      '""
      '"     increase parmu if necessary and branch back if this change alters"
      '"     optimal vertex. otherwise prerem and prerec will be set to the pre"
      '"     reductions in the merit function and the maximum constraint violat"
      '"     respectively."
      '""
      (setf barmu 0.0)
      (setf prerec
              (-
               (f2cl-lib:fref datmat-%data%
                              (mpp np)
                              ((1 mpp) (1 *))
                              datmat-%offset%)
               resnew))
      (if (> prerec 0.0) (setf barmu (/ sum prerec)))
      (cond
        ((< parmu (* 1.5 barmu))
         (setf parmu (* 2.0 barmu))
         (if (>= iprint 2)
             (f2cl-lib:fformat t
                               ("~%" "~3@T" "Increase in PARMU to" 1
                                (("~13,6,2,1,'*,,'EE")) "~%")
                               parmu))
         (setf phi
                 (+
                  (f2cl-lib:fref datmat-%data%
                                 (mp np)
                                 ((1 mpp) (1 *))
                                 datmat-%offset%)
                  (* parmu
                     (f2cl-lib:fref datmat-%data%
                                    (mpp np)
                                    ((1 mpp) (1 *))
                                    datmat-%offset%))))
         (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                       ((> j n) nil)
           (tagbody
             (setf temp
                     (+
                      (f2cl-lib:fref datmat-%data%
                                     (mp j)
                                     ((1 mpp) (1 *))
                                     datmat-%offset%)
                      (* parmu
                         (f2cl-lib:fref datmat-%data%
                                        (mpp j)
                                        ((1 mpp) (1 *))
                                        datmat-%offset%))))
             (if (< temp phi) (go label140))
             (cond
               ((and (= temp phi) (= parmu 0.0f0))
                (if
                 (<
                  (f2cl-lib:fref datmat-%data%
                                 (mpp j)
                                 ((1 mpp) (1 *))
                                 datmat-%offset%)
                  (f2cl-lib:fref datmat-%data%
                                 (mpp np)
                                 ((1 mpp) (1 *))
                                 datmat-%offset%))
                 (go label140))))
            label420))))
      (setf prerem (- (* parmu prerec) sum))
      '""
      '"     calculate the constraint and objective functions at x(*). then fin"
      '"     actual reduction in the merit function."
      '""
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label430
          (setf (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%)
                  (+
                   (f2cl-lib:fref sim-%data% (i np) ((1 n) (1 *)) sim-%offset%)
                   (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)))))
      (setf ibrnch 1)
      (go label40)
     label440
      (setf vmold
              (+
               (f2cl-lib:fref datmat-%data%
                              (mp np)
                              ((1 mpp) (1 *))
                              datmat-%offset%)
               (* parmu
                  (f2cl-lib:fref datmat-%data%
                                 (mpp np)
                                 ((1 mpp) (1 *))
                                 datmat-%offset%))))
      (setf vmnew (+ f (* parmu resmax)))
      (setf trured (- vmold vmnew))
      (cond
        ((and (= parmu 0.0)
              (= f (f2cl-lib:fref datmat (mp np) ((1 mpp) (1 *)))))
         (setf prerem prerec)
         (setf trured
                 (-
                  (f2cl-lib:fref datmat-%data%
                                 (mpp np)
                                 ((1 mpp) (1 *))
                                 datmat-%offset%)
                  resmax))))
      '""
      '"     begin the operations that decide whether x(*) should replace one o"
      '"     vertices of the current simplex, the change being mandatory if tru"
      '"     positive. firstly, jdrop is set to the index of the vertex that is"
      '"     replaced."
      '""
      (setf ratio 0.0)
      (if (<= trured 0.0f0) (setf ratio (coerce 1.0f0 'double-float)))
      (setf jdrop 0)
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (setf temp 0.0)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label450
              (setf temp
                      (+ temp
                         (*
                          (f2cl-lib:fref simi-%data%
                                         (j i)
                                         ((1 n) (1 *))
                                         simi-%offset%)
                          (f2cl-lib:fref dx-%data%
                                         (i)
                                         ((1 *))
                                         dx-%offset%))))))
          (setf temp (abs temp))
          (cond
            ((> temp ratio)
             (setf jdrop j)
             (setf ratio temp)))
         label460
          (setf (f2cl-lib:fref sigbar-%data% (j) ((1 *)) sigbar-%offset%)
                  (* temp
                     (f2cl-lib:fref vsig-%data% (j) ((1 *)) vsig-%offset%)))))
      '""
      '"     calculate the value of ell."
      '""
      (setf edgmax (* delta rho))
      (setf l 0)
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (cond
            ((or (>= (f2cl-lib:fref sigbar (j) ((1 *))) parsig)
                 (>= (f2cl-lib:fref sigbar (j) ((1 *)))
                     (f2cl-lib:fref vsig (j) ((1 *)))))
             (setf temp (f2cl-lib:fref veta-%data% (j) ((1 *)) veta-%offset%))
             (cond
               ((> trured 0.0)
                (setf temp 0.0)
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i n) nil)
                  (tagbody
                   label470
                    (setf temp
                            (+ temp
                               (expt
                                (-
                                 (f2cl-lib:fref dx-%data%
                                                (i)
                                                ((1 *))
                                                dx-%offset%)
                                 (f2cl-lib:fref sim-%data%
                                                (i j)
                                                ((1 n) (1 *))
                                                sim-%offset%))
                                2)))))
                (setf temp (f2cl-lib:fsqrt temp))))
             (cond
               ((> temp edgmax)
                (setf l j)
                (setf edgmax temp)))))
         label480))
      (if (> l 0) (setf jdrop l))
      (if (= jdrop 0) (go label550))
      '""
      '"     revise the simplex by updating the elements of sim, simi and datma"
      '""
      (setf temp 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref sim-%data% (i jdrop) ((1 n) (1 *)) sim-%offset%)
                  (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))
         label490
          (setf temp
                  (+ temp
                     (*
                      (f2cl-lib:fref simi-%data%
                                     (jdrop i)
                                     ((1 n) (1 *))
                                     simi-%offset%)
                      (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))))))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label500
          (setf (f2cl-lib:fref simi-%data%
                               (jdrop i)
                               ((1 n) (1 *))
                               simi-%offset%)
                  (/
                   (f2cl-lib:fref simi-%data%
                                  (jdrop i)
                                  ((1 n) (1 *))
                                  simi-%offset%)
                   temp))))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (cond
            ((/= j jdrop)
             (setf temp 0.0)
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i n) nil)
               (tagbody
                label510
                 (setf temp
                         (+ temp
                            (*
                             (f2cl-lib:fref simi-%data%
                                            (j i)
                                            ((1 n) (1 *))
                                            simi-%offset%)
                             (f2cl-lib:fref dx-%data%
                                            (i)
                                            ((1 *))
                                            dx-%offset%))))))
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i n) nil)
               (tagbody
                label520
                 (setf (f2cl-lib:fref simi-%data%
                                      (j i)
                                      ((1 n) (1 *))
                                      simi-%offset%)
                         (-
                          (f2cl-lib:fref simi-%data%
                                         (j i)
                                         ((1 n) (1 *))
                                         simi-%offset%)
                          (* temp
                             (f2cl-lib:fref simi-%data%
                                            (jdrop i)
                                            ((1 n) (1 *))
                                            simi-%offset%))))))))
         label530))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k mpp) nil)
        (tagbody
         label540
          (setf (f2cl-lib:fref datmat-%data%
                               (k jdrop)
                               ((1 mpp) (1 *))
                               datmat-%offset%)
                  (f2cl-lib:fref con-%data% (k) ((1 *)) con-%offset%))))
      '""
      '"     branch back for further iterations with the current rho."
      '""
      (if (and (> trured 0.0) (>= trured (* 0.1 prerem))) (go label140))
     label550
      (cond
        ((= iflag 0)
         (setf ibrnch 0)
         (go label140)))
      '""
      '"     otherwise reduce rho if it is not at its least value and reset par"
      '""
      (cond
        ((> rho rhoend)
         (setf rho (* 0.5 rho))
         (if (<= rho (* 1.5 rhoend)) (setf rho rhoend))
         (cond
           ((> parmu 0.0)
            (setf denom 0.0)
            (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                          ((> k mp) nil)
              (tagbody
                (setf cmin
                        (f2cl-lib:fref datmat-%data%
                                       (k np)
                                       ((1 mpp) (1 *))
                                       datmat-%offset%))
                (setf cmax cmin)
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i n) nil)
                  (tagbody
                    (setf cmin
                            (f2cl-lib:dmin1 cmin
                                            (f2cl-lib:fref datmat-%data%
                                                           (k i)
                                                           ((1 mpp) (1 *))
                                                           datmat-%offset%)))
                   label560
                    (setf cmax
                            (f2cl-lib:dmax1 cmax
                                            (f2cl-lib:fref datmat-%data%
                                                           (k i)
                                                           ((1 mpp) (1 *))
                                                           datmat-%offset%)))))
                (cond
                  ((and (<= k m) (< cmin (* 0.5 cmax)))
                   (setf temp (- (f2cl-lib:dmax1 cmax 0.0) cmin))
                   (cond
                     ((<= denom 0.0)
                      (setf denom temp))
                     (t
                      (setf denom (f2cl-lib:dmin1 denom temp))))))
               label570))
            (cond
              ((= denom 0.0)
               (setf parmu 0.0))
              ((< (+ cmax (- cmin)) (* parmu denom))
               (setf parmu (/ (- cmax cmin) denom))))))
         (if (>= iprint 2)
             (f2cl-lib:fformat t
                               ("~%" "~3@T" "Reduction in RHO to" 1
                                (("~13,6,2,1,'*,,'EE")) "  and PARMU =" 1
                                (("~13,6,2,1,'*,,'EE")) "~%")
                               rho
                               parmu))
         (cond
           ((= iprint 2)
            (f2cl-lib:fformat t
                              ("~%" "~3@T" "NFVALS =" 1 (("~5D")) "~3@T" "F ="
                               1 (("~13,6,2,1,'*,,'EE")) "~4@T" "MAXCV =" 1
                               (("~13,6,2,1,'*,,'EE")) "~%" "~3@T" "X =" 1
                               (("~13,6,2,1,'*,,'EE")) 4
                               (("~15,6,2,1,'*,,'EE")) "~%")
                              nfvals
                              (f2cl-lib:fref datmat-%data%
                                             (mp np)
                                             ((1 mpp) (1 *))
                                             datmat-%offset%)
                              (f2cl-lib:fref datmat-%data%
                                             (mpp np)
                                             ((1 mpp) (1 *))
                                             datmat-%offset%)
                              (do ((i 1 (f2cl-lib:int-add i 1))
                                   (%ret nil))
                                  ((> i iptem) (nreverse %ret))
                                (declare (type f2cl-lib:integer4 i))
                                (push
                                 (f2cl-lib:fref sim-%data%
                                                (i np)
                                                ((1 n) (1 *))
                                                sim-%offset%)
                                 %ret)))
            (if (< iptem n)
                (f2cl-lib:fformat t
                                  (1 (("~19,6,2,1,'*,,'EE")) 4
                                   (("~15,6,2,1,'*,,'EE")) "~%")
                                  (do ((i iptemp (f2cl-lib:int-add i 1))
                                       (%ret nil))
                                      ((> i n) (nreverse %ret))
                                    (declare (type f2cl-lib:integer4 i))
                                    (push
                                     (f2cl-lib:fref x-%data%
                                                    (i)
                                                    ((1 *))
                                                    x-%offset%)
                                     %ret))))))
         (go label140)))
      '""
      '"     return the best calculated values of the variables."
      '""
      (if (>= iprint 1)
          (f2cl-lib:fformat t
                            ("~%" "~3@T" "Normal return from subroutine COBYLA"
                             "~%")))
      (if (= ifull 1) (go label620))
     label600
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label610
          (setf (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%)
                  (f2cl-lib:fref sim-%data%
                                 (i np)
                                 ((1 n) (1 *))
                                 sim-%offset%))))
      (setf f
              (f2cl-lib:fref datmat-%data%
                             (mp np)
                             ((1 mpp) (1 *))
                             datmat-%offset%))
      (setf resmax
              (f2cl-lib:fref datmat-%data%
                             (mpp np)
                             ((1 mpp) (1 *))
                             datmat-%offset%))
     label620
      (cond
        ((>= iprint 1)
         (f2cl-lib:fformat t
                           ("~%" "~3@T" "NFVALS =" 1 (("~5D")) "~3@T" "F =" 1
                            (("~13,6,2,1,'*,,'EE")) "~4@T" "MAXCV =" 1
                            (("~13,6,2,1,'*,,'EE")) "~%" "~3@T" "X =" 1
                            (("~13,6,2,1,'*,,'EE")) 4 (("~15,6,2,1,'*,,'EE"))
                            "~%")
                           nfvals
                           f
                           resmax
                           (do ((i 1 (f2cl-lib:int-add i 1))
                                (%ret nil))
                               ((> i iptem) (nreverse %ret))
                             (declare (type f2cl-lib:integer4 i))
                             (push
                              (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%)
                              %ret)))
         (if (< iptem n)
             (f2cl-lib:fformat t
                               (1 (("~19,6,2,1,'*,,'EE")) 4
                                (("~15,6,2,1,'*,,'EE")) "~%")
                               (do ((i iptemp (f2cl-lib:int-add i 1))
                                    (%ret nil))
                                   ((> i n) (nreverse %ret))
                                 (declare (type f2cl-lib:integer4 i))
                                 (push
                                  (f2cl-lib:fref x-%data%
                                                 (i)
                                                 ((1 *))
                                                 x-%offset%)
                                  %ret))))))
      (setf maxfun nfvals)
      (go end_label)
     end_label
      (return
       (values nil
               nil
               nil
               nil
               nil
               nil
               nil
               maxfun
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
               ierr)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::cobylb
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        double-float double-float (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil fortran-to-lisp::maxfun
                            nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::ierr)
           :calls '(fortran-to-lisp::trstlp fortran-to-lisp::calcfc))))

