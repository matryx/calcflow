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
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :common-lisp-user)


(let ((finish nil)
      (iter 0)
      (nfun 0)
      (point 0)
      (ispt 0)
      (iypt 0)
      (maxfev 0)
      (info 0)
      (bound 0)
      (npt 0)
      (cp 0)
      (i 0)
      (nfev 0)
      (inmc 0)
      (iycn 0)
      (iscn 0)
      (one 1.0)
      (zero 0.0)
      (gnorm 0.0)
      (stp1 0.0)
      (ftol 0.0)
      (stp 0.0)
      (ys 0.0)
      (yy 0.0)
      (sq 0.0)
      (yr 0.0)
      (beta 0.0)
      (xnorm 0.0))
  (declare (type f2cl-lib:logical finish)
           (type (f2cl-lib:integer4) iter nfun point ispt iypt maxfev info
                                     bound npt cp i nfev inmc iycn iscn)
           (type (double-float) one zero gnorm stp1 ftol stp ys yy sq yr beta
                                xnorm))
  (defun lbfgs (n m x f g diagco diag iprint eps xtol w iflag scache)
    (declare (type (array f2cl-lib:integer4 (*)) iprint)
             (type f2cl-lib:logical diagco)
             (type (double-float) xtol eps f)
             (type (array double-float (*)) scache w diag g x)
             (type (f2cl-lib:integer4) iflag m n))
    (let ()
      (symbol-macrolet ((gtol (lb3-gtol *lb3-common-block*))
                        (lp (lb3-lp *lb3-common-block*)))
        (f2cl-lib:with-multi-array-data
            ((x double-float x-%data% x-%offset%)
             (g double-float g-%data% g-%offset%)
             (diag double-float diag-%data% diag-%offset%)
             (w double-float w-%data% w-%offset%)
             (scache double-float scache-%data% scache-%offset%)
             (iprint f2cl-lib:integer4 iprint-%data% iprint-%offset%))
          (prog ()
            (declare)
            (if (= iflag 0) (go label10))
            (f2cl-lib:computed-goto (label172 label100) iflag)
           label10
            (setf iter 0)
            (if (or (<= n 0) (<= m 0)) (go label196))
            (cond
              ((<= gtol 1.0e-4)
               (if (> lp 0)
                   (f2cl-lib:fformat lp
                                     ("~%"
                                      "  GTOL IS LESS THAN OR EQUAL TO 1.D-04"
                                      "~%" " IT HAS BEEN RESET TO 9.D-01"
                                      "~%")))
               (setf gtol 0.9)))
            (setf nfun 1)
            (setf point 0)
            (setf finish f2cl-lib:%false%)
            (cond
              (diagco
               (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                             ((> i n) nil)
                 (tagbody
                  label30
                   (if
                    (<= (f2cl-lib:fref diag-%data% (i) ((1 n)) diag-%offset%)
                        zero)
                    (go label195)))))
              (t
               (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                             ((> i n) nil)
                 (tagbody
                  label40
                   (setf (f2cl-lib:fref diag-%data% (i) ((1 n)) diag-%offset%)
                           1.0)))))
            (setf ispt (f2cl-lib:int-add n (f2cl-lib:int-mul 2 m)))
            (setf iypt (f2cl-lib:int-add ispt (f2cl-lib:int-mul n m)))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label50
                (setf (f2cl-lib:fref w-%data%
                                     ((f2cl-lib:int-add ispt i))
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add
                                                           (f2cl-lib:int-mul 2
                                                                             m)
                                                           1))
                                        (f2cl-lib:int-mul 2 m))))
                                     w-%offset%)
                        (* (- (f2cl-lib:fref g-%data% (i) ((1 n)) g-%offset%))
                           (f2cl-lib:fref diag-%data%
                                          (i)
                                          ((1 n))
                                          diag-%offset%)))))
            (setf gnorm
                    (f2cl-lib:dsqrt
                     (multiple-value-bind
                           (ret-val var-0 var-1 var-2 var-3 var-4)
                         (ddot n g 1 g 1)
                       (declare (ignore var-1 var-2 var-3 var-4))
                       (when var-0
                         (setf n var-0))
                       ret-val)))
            (setf stp1 (/ one gnorm))
            (setf ftol 1.0e-4)
            (setf maxfev 20)
            (if
             (>= (f2cl-lib:fref iprint-%data% (1) ((1 2)) iprint-%offset%) 0)
             (lb1 iprint iter nfun gnorm n m x f g stp finish))
           label80
            (setf iter (f2cl-lib:int-add iter 1))
            (setf info 0)
            (setf bound (f2cl-lib:int-sub iter 1))
            (if (= iter 1) (go label165))
            (if (> iter m) (setf bound m))
            (setf ys
                    (multiple-value-bind
                          (ret-val var-0 var-1 var-2 var-3 var-4)
                        (ddot n
                         (f2cl-lib:array-slice w
                                               double-float
                                               ((+ iypt npt 1))
                                               ((1
                                                 (f2cl-lib:int-add
                                                  (f2cl-lib:int-mul n
                                                                    (f2cl-lib:int-add
                                                                     (f2cl-lib:int-mul
                                                                      2
                                                                      m)
                                                                     1))
                                                  (f2cl-lib:int-mul 2 m)))))
                         1
                         (f2cl-lib:array-slice w
                                               double-float
                                               ((+ ispt npt 1))
                                               ((1
                                                 (f2cl-lib:int-add
                                                  (f2cl-lib:int-mul n
                                                                    (f2cl-lib:int-add
                                                                     (f2cl-lib:int-mul
                                                                      2
                                                                      m)
                                                                     1))
                                                  (f2cl-lib:int-mul 2 m)))))
                         1)
                      (declare (ignore var-1 var-2 var-3 var-4))
                      (when var-0
                        (setf n var-0))
                      ret-val))
            (cond
              ((not diagco)
               (setf yy
                       (multiple-value-bind
                             (ret-val var-0 var-1 var-2 var-3 var-4)
                           (ddot n
                            (f2cl-lib:array-slice w
                                                  double-float
                                                  ((+ iypt npt 1))
                                                  ((1
                                                    (f2cl-lib:int-add
                                                     (f2cl-lib:int-mul n
                                                                       (f2cl-lib:int-add
                                                                        (f2cl-lib:int-mul
                                                                         2
                                                                         m)
                                                                        1))
                                                     (f2cl-lib:int-mul 2 m)))))
                            1
                            (f2cl-lib:array-slice w
                                                  double-float
                                                  ((+ iypt npt 1))
                                                  ((1
                                                    (f2cl-lib:int-add
                                                     (f2cl-lib:int-mul n
                                                                       (f2cl-lib:int-add
                                                                        (f2cl-lib:int-mul
                                                                         2
                                                                         m)
                                                                        1))
                                                     (f2cl-lib:int-mul 2 m)))))
                            1)
                         (declare (ignore var-1 var-2 var-3 var-4))
                         (when var-0
                           (setf n var-0))
                         ret-val))
               (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                             ((> i n) nil)
                 (tagbody
                  label90
                   (setf (f2cl-lib:fref diag-%data% (i) ((1 n)) diag-%offset%)
                           (/ ys yy)))))
              (t
               (setf iflag 2)
               (go end_label)))
           label100
            (cond
              (diagco
               (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                             ((> i n) nil)
                 (tagbody
                  label110
                   (if
                    (<= (f2cl-lib:fref diag-%data% (i) ((1 n)) diag-%offset%)
                        zero)
                    (go label195))))))
            (setf cp point)
            (if (= point 0) (setf cp m))
            (setf (f2cl-lib:fref w-%data%
                                 ((f2cl-lib:int-add n cp))
                                 ((1
                                   (f2cl-lib:int-add
                                    (f2cl-lib:int-mul n
                                                      (f2cl-lib:int-add
                                                       (f2cl-lib:int-mul 2 m)
                                                       1))
                                    (f2cl-lib:int-mul 2 m))))
                                 w-%offset%)
                    (/ one ys))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label112
                (setf (f2cl-lib:fref w-%data%
                                     (i)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add
                                                           (f2cl-lib:int-mul 2
                                                                             m)
                                                           1))
                                        (f2cl-lib:int-mul 2 m))))
                                     w-%offset%)
                        (- (f2cl-lib:fref g-%data% (i) ((1 n)) g-%offset%)))))
            (setf cp point)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i bound) nil)
              (tagbody
                (setf cp (f2cl-lib:int-sub cp 1))
                (if (= cp -1) (setf cp (f2cl-lib:int-sub m 1)))
                (setf sq
                        (multiple-value-bind
                              (ret-val var-0 var-1 var-2 var-3 var-4)
                            (ddot n
                             (f2cl-lib:array-slice w
                                                   double-float
                                                   ((+ ispt
                                                       (f2cl-lib:int-mul cp n)
                                                       1))
                                                   ((1
                                                     (f2cl-lib:int-add
                                                      (f2cl-lib:int-mul n
                                                                        (f2cl-lib:int-add
                                                                         (f2cl-lib:int-mul
                                                                          2
                                                                          m)
                                                                         1))
                                                      (f2cl-lib:int-mul 2
                                                                        m)))))
                             1 w 1)
                          (declare (ignore var-1 var-2 var-3 var-4))
                          (when var-0
                            (setf n var-0))
                          ret-val))
                (setf inmc (f2cl-lib:int-add n m cp 1))
                (setf iycn (f2cl-lib:int-add iypt (f2cl-lib:int-mul cp n)))
                (setf (f2cl-lib:fref w-%data%
                                     (inmc)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add
                                                           (f2cl-lib:int-mul 2
                                                                             m)
                                                           1))
                                        (f2cl-lib:int-mul 2 m))))
                                     w-%offset%)
                        (*
                         (f2cl-lib:fref w-%data%
                                        ((f2cl-lib:int-add n cp 1))
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul n
                                                             (f2cl-lib:int-add
                                                              (f2cl-lib:int-mul
                                                               2
                                                               m)
                                                              1))
                                           (f2cl-lib:int-mul 2 m))))
                                        w-%offset%)
                         sq))
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                    (daxpy n
                     (-
                      (f2cl-lib:fref w-%data%
                                     (inmc)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add
                                                           (f2cl-lib:int-mul 2
                                                                             m)
                                                           1))
                                        (f2cl-lib:int-mul 2 m))))
                                     w-%offset%))
                     (f2cl-lib:array-slice w
                                           double-float
                                           ((+ iycn 1))
                                           ((1
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-mul n
                                                                (f2cl-lib:int-add
                                                                 (f2cl-lib:int-mul
                                                                  2
                                                                  m)
                                                                 1))
                                              (f2cl-lib:int-mul 2 m)))))
                     1 w 1)
                  (declare (ignore var-1 var-2 var-3 var-4 var-5))
                  (when var-0
                    (setf n var-0)))
               label125))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label130
                (setf (f2cl-lib:fref w-%data%
                                     (i)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add
                                                           (f2cl-lib:int-mul 2
                                                                             m)
                                                           1))
                                        (f2cl-lib:int-mul 2 m))))
                                     w-%offset%)
                        (*
                         (f2cl-lib:fref diag-%data% (i) ((1 n)) diag-%offset%)
                         (f2cl-lib:fref w-%data%
                                        (i)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul n
                                                             (f2cl-lib:int-add
                                                              (f2cl-lib:int-mul
                                                               2
                                                               m)
                                                              1))
                                           (f2cl-lib:int-mul 2 m))))
                                        w-%offset%)))))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i bound) nil)
              (tagbody
                (setf yr
                        (multiple-value-bind
                              (ret-val var-0 var-1 var-2 var-3 var-4)
                            (ddot n
                             (f2cl-lib:array-slice w
                                                   double-float
                                                   ((+ iypt
                                                       (f2cl-lib:int-mul cp n)
                                                       1))
                                                   ((1
                                                     (f2cl-lib:int-add
                                                      (f2cl-lib:int-mul n
                                                                        (f2cl-lib:int-add
                                                                         (f2cl-lib:int-mul
                                                                          2
                                                                          m)
                                                                         1))
                                                      (f2cl-lib:int-mul 2
                                                                        m)))))
                             1 w 1)
                          (declare (ignore var-1 var-2 var-3 var-4))
                          (when var-0
                            (setf n var-0))
                          ret-val))
                (setf beta
                        (*
                         (f2cl-lib:fref w-%data%
                                        ((f2cl-lib:int-add n cp 1))
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul n
                                                             (f2cl-lib:int-add
                                                              (f2cl-lib:int-mul
                                                               2
                                                               m)
                                                              1))
                                           (f2cl-lib:int-mul 2 m))))
                                        w-%offset%)
                         yr))
                (setf inmc (f2cl-lib:int-add n m cp 1))
                (setf beta
                        (-
                         (f2cl-lib:fref w-%data%
                                        (inmc)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul n
                                                             (f2cl-lib:int-add
                                                              (f2cl-lib:int-mul
                                                               2
                                                               m)
                                                              1))
                                           (f2cl-lib:int-mul 2 m))))
                                        w-%offset%)
                         beta))
                (setf iscn (f2cl-lib:int-add ispt (f2cl-lib:int-mul cp n)))
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                    (daxpy n beta
                     (f2cl-lib:array-slice w
                                           double-float
                                           ((+ iscn 1))
                                           ((1
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-mul n
                                                                (f2cl-lib:int-add
                                                                 (f2cl-lib:int-mul
                                                                  2
                                                                  m)
                                                                 1))
                                              (f2cl-lib:int-mul 2 m)))))
                     1 w 1)
                  (declare (ignore var-2 var-3 var-4 var-5))
                  (when var-0
                    (setf n var-0))
                  (when var-1
                    (setf beta var-1)))
                (setf cp (f2cl-lib:int-add cp 1))
                (if (= cp m) (setf cp 0))
               label145))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label160
                (setf (f2cl-lib:fref w-%data%
                                     ((f2cl-lib:int-add ispt
                                                        (f2cl-lib:int-mul point
                                                                          n)
                                                        i))
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add
                                                           (f2cl-lib:int-mul 2
                                                                             m)
                                                           1))
                                        (f2cl-lib:int-mul 2 m))))
                                     w-%offset%)
                        (f2cl-lib:fref w-%data%
                                       (i)
                                       ((1
                                         (f2cl-lib:int-add
                                          (f2cl-lib:int-mul n
                                                            (f2cl-lib:int-add
                                                             (f2cl-lib:int-mul
                                                              2
                                                              m)
                                                             1))
                                          (f2cl-lib:int-mul 2 m))))
                                       w-%offset%))))
           label165
            (setf nfev 0)
            (setf stp one)
            (if (= iter 1) (setf stp stp1))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
               label170
                (setf (f2cl-lib:fref w-%data%
                                     (i)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add
                                                           (f2cl-lib:int-mul 2
                                                                             m)
                                                           1))
                                        (f2cl-lib:int-mul 2 m))))
                                     w-%offset%)
                        (f2cl-lib:fref g-%data% (i) ((1 n)) g-%offset%))))
           label172
            (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10 var-11)
                (mcsrch n x f g
                 (f2cl-lib:array-slice w
                                       double-float
                                       ((+ ispt (f2cl-lib:int-mul point n) 1))
                                       ((1
                                         (f2cl-lib:int-add
                                          (f2cl-lib:int-mul n
                                                            (f2cl-lib:int-add
                                                             (f2cl-lib:int-mul
                                                              2
                                                              m)
                                                             1))
                                          (f2cl-lib:int-mul 2 m)))))
                 stp ftol xtol maxfev info nfev diag)
              (declare (ignore var-0 var-1 var-2 var-3 var-4 var-6 var-7 var-8
                               var-11))
              (setf stp var-5)
              (setf info var-9)
              (setf nfev var-10))
            (cond
              ((= info (f2cl-lib:int-sub 1))
               (setf iflag 1)
               (go end_label)))
            (if (/= info 1) (go label190))
            (setf nfun (f2cl-lib:int-add nfun nfev))
            (setf npt (f2cl-lib:int-mul point n))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf (f2cl-lib:fref w-%data%
                                     ((f2cl-lib:int-add ispt npt i))
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add
                                                           (f2cl-lib:int-mul 2
                                                                             m)
                                                           1))
                                        (f2cl-lib:int-mul 2 m))))
                                     w-%offset%)
                        (* stp
                           (f2cl-lib:fref w-%data%
                                          ((f2cl-lib:int-add ispt npt i))
                                          ((1
                                            (f2cl-lib:int-add
                                             (f2cl-lib:int-mul n
                                                               (f2cl-lib:int-add
                                                                (f2cl-lib:int-mul
                                                                 2
                                                                 m)
                                                                1))
                                             (f2cl-lib:int-mul 2 m))))
                                          w-%offset%)))
               label175
                (setf (f2cl-lib:fref w-%data%
                                     ((f2cl-lib:int-add iypt npt i))
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add
                                                           (f2cl-lib:int-mul 2
                                                                             m)
                                                           1))
                                        (f2cl-lib:int-mul 2 m))))
                                     w-%offset%)
                        (- (f2cl-lib:fref g-%data% (i) ((1 n)) g-%offset%)
                           (f2cl-lib:fref w-%data%
                                          (i)
                                          ((1
                                            (f2cl-lib:int-add
                                             (f2cl-lib:int-mul n
                                                               (f2cl-lib:int-add
                                                                (f2cl-lib:int-mul
                                                                 2
                                                                 m)
                                                                1))
                                             (f2cl-lib:int-mul 2 m))))
                                          w-%offset%)))))
            (setf point (f2cl-lib:int-add point 1))
            (if (= point m) (setf point 0))
            (setf gnorm
                    (f2cl-lib:dsqrt
                     (multiple-value-bind
                           (ret-val var-0 var-1 var-2 var-3 var-4)
                         (ddot n g 1 g 1)
                       (declare (ignore var-1 var-2 var-3 var-4))
                       (when var-0
                         (setf n var-0))
                       ret-val)))
            (setf xnorm
                    (f2cl-lib:dsqrt
                     (multiple-value-bind
                           (ret-val var-0 var-1 var-2 var-3 var-4)
                         (ddot n x 1 x 1)
                       (declare (ignore var-1 var-2 var-3 var-4))
                       (when var-0
                         (setf n var-0))
                       ret-val)))
            (setf xnorm (f2cl-lib:dmax1 1.0 xnorm))
            (if (<= (/ gnorm xnorm) eps) (setf finish f2cl-lib:%true%))
            (if
             (>= (f2cl-lib:fref iprint-%data% (1) ((1 2)) iprint-%offset%) 0)
             (lb1 iprint iter nfun gnorm n m x f g stp finish))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf (f2cl-lib:fref scache-%data% (i) ((1 n)) scache-%offset%)
                        (f2cl-lib:fref x-%data% (i) ((1 n)) x-%offset%))
               label177))
            (cond
              (finish
               (setf iflag 0)
               (go end_label)))
            (go label80)
           label190
            (setf iflag -1)
            (if (> lp 0)
                (f2cl-lib:fformat lp
                                  ("~%" " IFLAG= -1 " "~%"
                                   " LINE SEARCH FAILED. SEE"
                                   " DOCUMENTATION OF ROUTINE MCSRCH" "~%"
                                   " ERROR RETURN" " OF LINE SEARCH: INFO= " 1
                                   (("~2D")) "~%"
                                   " POSSIBLE CAUSES: FUNCTION OR GRADIENT ARE INCORRECT"
                                   "~%" " OR INCORRECT TOLERANCES" "~%")
                                  info))
            (go end_label)
           label195
            (setf iflag -2)
            (if (> lp 0)
                (f2cl-lib:fformat lp
                                  ("~%" " IFLAG= -2" "~%" " THE" 1 (("~5D"))
                                   "-TH DIAGONAL ELEMENT OF THE" "~%"
                                   " INVERSE HESSIAN APPROXIMATION IS NOT POSITIVE"
                                   "~%")
                                  i))
            (go end_label)
           label196
            (setf iflag -3)
            (if (> lp 0)
                (f2cl-lib:fformat lp
                                  ("~%" " IFLAG= -3" "~%"
                                   " IMPROPER INPUT PARAMETERS (N OR M"
                                   " ARE NOT POSITIVE)" "~%")))
            (go end_label)
           end_label
            (return
             (values n nil nil nil nil nil nil nil nil nil nil iflag nil))))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::lbfgs fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (double-float)
                        (array double-float (*)) fortran-to-lisp::logical
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (2)) (double-float)
                        (double-float) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*)))
           :return-values '(fortran-to-lisp::n nil nil nil nil nil nil nil nil
                            nil nil fortran-to-lisp::iflag nil)
           :calls '(fortran-to-lisp::mcsrch fortran-to-lisp::lb1))))

