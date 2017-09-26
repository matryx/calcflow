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


(let* ((zero 0.0) (one 1.0) (ten 10.0) (ldd 4) (ldx 2))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (type (double-float 10.0 10.0) ten)
           (type (f2cl-lib:integer4 4 4) ldd)
           (type (f2cl-lib:integer4 2 2) ldx)
           (ignorable zero one ten ldd ldx))
  (defun dlaexc (wantq n t$ ldt q ldq j1 n1 n2 work info)
    (declare (type (array double-float (*)) work q t$)
             (type (f2cl-lib:integer4) info n2 n1 j1 ldq ldt n)
             (type f2cl-lib:logical wantq))
    (f2cl-lib:with-multi-array-data
        ((t$ double-float t$-%data% t$-%offset%)
         (q double-float q-%data% q-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((d
              (make-array (the fixnum (reduce #'* (list ldd 4)))
                          :element-type 'double-float))
             (u (make-array 3 :element-type 'double-float))
             (u1 (make-array 3 :element-type 'double-float))
             (u2 (make-array 3 :element-type 'double-float))
             (x
              (make-array (the fixnum (reduce #'* (list ldx 2)))
                          :element-type 'double-float))
             (cs 0.0) (dnorm 0.0) (eps 0.0) (scale 0.0) (smlnum 0.0) (sn 0.0)
             (t11 0.0) (t22 0.0) (t33 0.0) (tau 0.0) (tau1 0.0) (tau2 0.0)
             (temp 0.0) (thresh 0.0) (wi1 0.0) (wi2 0.0) (wr1 0.0) (wr2 0.0)
             (xnorm 0.0) (ierr 0) (j2 0) (j3 0) (j4 0) (k 0) (nd 0))
        (declare (type (array double-float (3)) u u1 u2)
                 (type (array double-float (*)) d x)
                 (type (double-float) cs dnorm eps scale smlnum sn t11 t22 t33
                                      tau tau1 tau2 temp thresh wi1 wi2 wr1 wr2
                                      xnorm)
                 (type (f2cl-lib:integer4) ierr j2 j3 j4 k nd))
        (setf info 0)
        (if (or (= n 0) (= n1 0) (= n2 0)) (go end_label))
        (if (> (f2cl-lib:int-add j1 n1) n) (go end_label))
        (setf j2 (f2cl-lib:int-add j1 1))
        (setf j3 (f2cl-lib:int-add j1 2))
        (setf j4 (f2cl-lib:int-add j1 3))
        (cond
          ((and (= n1 1) (= n2 1))
           (setf t11
                   (f2cl-lib:fref t$-%data%
                                  (j1 j1)
                                  ((1 ldt) (1 *))
                                  t$-%offset%))
           (setf t22
                   (f2cl-lib:fref t$-%data%
                                  (j2 j2)
                                  ((1 ldt) (1 *))
                                  t$-%offset%))
           (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
               (dlartg
                (f2cl-lib:fref t$-%data% (j1 j2) ((1 ldt) (1 *)) t$-%offset%)
                (- t22 t11) cs sn temp)
             (declare (ignore var-0 var-1))
             (setf cs var-2)
             (setf sn var-3)
             (setf temp var-4))
           (if (<= j3 n)
               (drot (f2cl-lib:int-sub n j1 1)
                (f2cl-lib:array-slice t$-%data%
                                      double-float
                                      (j1 j3)
                                      ((1 ldt) (1 *))
                                      t$-%offset%)
                ldt
                (f2cl-lib:array-slice t$-%data%
                                      double-float
                                      (j2 j3)
                                      ((1 ldt) (1 *))
                                      t$-%offset%)
                ldt cs sn))
           (drot (f2cl-lib:int-sub j1 1)
            (f2cl-lib:array-slice t$-%data%
                                  double-float
                                  (1 j1)
                                  ((1 ldt) (1 *))
                                  t$-%offset%)
            1
            (f2cl-lib:array-slice t$-%data%
                                  double-float
                                  (1 j2)
                                  ((1 ldt) (1 *))
                                  t$-%offset%)
            1 cs sn)
           (setf (f2cl-lib:fref t$-%data% (j1 j1) ((1 ldt) (1 *)) t$-%offset%)
                   t22)
           (setf (f2cl-lib:fref t$-%data% (j2 j2) ((1 ldt) (1 *)) t$-%offset%)
                   t11)
           (cond
             (wantq
              (drot n
               (f2cl-lib:array-slice q-%data%
                                     double-float
                                     (1 j1)
                                     ((1 ldq) (1 *))
                                     q-%offset%)
               1
               (f2cl-lib:array-slice q-%data%
                                     double-float
                                     (1 j2)
                                     ((1 ldq) (1 *))
                                     q-%offset%)
               1 cs sn))))
          (t
           (tagbody
             (setf nd (f2cl-lib:int-add n1 n2))
             (dlacpy "Full" nd nd
              (f2cl-lib:array-slice t$-%data%
                                    double-float
                                    (j1 j1)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
              ldt d ldd)
             (setf dnorm (dlange "Max" nd nd d ldd work))
             (setf eps (dlamch "P"))
             (setf smlnum (/ (dlamch "S") eps))
             (setf thresh (max (* ten eps dnorm) smlnum))
             (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                    var-10 var-11 var-12 var-13 var-14 var-15)
                 (dlasy2 f2cl-lib:%false% f2cl-lib:%false% -1 n1 n2 d ldd
                  (f2cl-lib:array-slice d
                                        double-float
                                        ((+ n1 1) (f2cl-lib:int-add n1 1))
                                        ((1 ldd) (1 4)))
                  ldd
                  (f2cl-lib:array-slice d
                                        double-float
                                        (1 (f2cl-lib:int-add n1 1))
                                        ((1 ldd) (1 4)))
                  ldd scale x ldx xnorm ierr)
               (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                var-8 var-9 var-10 var-12 var-13))
               (setf scale var-11)
               (setf xnorm var-14)
               (setf ierr var-15))
             (setf k (f2cl-lib:int-sub (f2cl-lib:int-add n1 n1 n2) 3))
             (f2cl-lib:computed-goto (label10 label20 label30) k)
            label10
             (setf (f2cl-lib:fref u (1) ((1 3))) scale)
             (setf (f2cl-lib:fref u (2) ((1 3)))
                     (f2cl-lib:fref x (1 1) ((1 ldx) (1 2))))
             (setf (f2cl-lib:fref u (3) ((1 3)))
                     (f2cl-lib:fref x (1 2) ((1 ldx) (1 2))))
             (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                 (dlarfg 3 (f2cl-lib:fref u (3) ((1 3))) u 1 tau)
               (declare (ignore var-0 var-2 var-3))
               (setf (f2cl-lib:fref u (3) ((1 3))) var-1)
               (setf tau var-4))
             (setf (f2cl-lib:fref u (3) ((1 3))) one)
             (setf t11
                     (f2cl-lib:fref t$-%data%
                                    (j1 j1)
                                    ((1 ldt) (1 *))
                                    t$-%offset%))
             (dlarfx "L" 3 3 u tau d ldd work)
             (dlarfx "R" 3 3 u tau d ldd work)
             (if
              (>
               (max (abs (f2cl-lib:fref d (3 1) ((1 ldd) (1 4))))
                    (abs (f2cl-lib:fref d (3 2) ((1 ldd) (1 4))))
                    (abs (- (f2cl-lib:fref d (3 3) ((1 ldd) (1 4))) t11)))
               thresh)
              (go label50))
             (dlarfx "L" 3 (f2cl-lib:int-add (f2cl-lib:int-sub n j1) 1) u tau
              (f2cl-lib:array-slice t$-%data%
                                    double-float
                                    (j1 j1)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
              ldt work)
             (dlarfx "R" j2 3 u tau
              (f2cl-lib:array-slice t$-%data%
                                    double-float
                                    (1 j1)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
              ldt work)
             (setf (f2cl-lib:fref t$-%data%
                                  (j3 j1)
                                  ((1 ldt) (1 *))
                                  t$-%offset%)
                     zero)
             (setf (f2cl-lib:fref t$-%data%
                                  (j3 j2)
                                  ((1 ldt) (1 *))
                                  t$-%offset%)
                     zero)
             (setf (f2cl-lib:fref t$-%data%
                                  (j3 j3)
                                  ((1 ldt) (1 *))
                                  t$-%offset%)
                     t11)
             (cond
               (wantq
                (dlarfx "R" n 3 u tau
                 (f2cl-lib:array-slice q-%data%
                                       double-float
                                       (1 j1)
                                       ((1 ldq) (1 *))
                                       q-%offset%)
                 ldq work)))
             (go label40)
            label20
             (setf (f2cl-lib:fref u (1) ((1 3)))
                     (- (f2cl-lib:fref x (1 1) ((1 ldx) (1 2)))))
             (setf (f2cl-lib:fref u (2) ((1 3)))
                     (- (f2cl-lib:fref x (2 1) ((1 ldx) (1 2)))))
             (setf (f2cl-lib:fref u (3) ((1 3))) scale)
             (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                 (dlarfg 3 (f2cl-lib:fref u (1) ((1 3)))
                  (f2cl-lib:array-slice u double-float (2) ((1 3))) 1 tau)
               (declare (ignore var-0 var-2 var-3))
               (setf (f2cl-lib:fref u (1) ((1 3))) var-1)
               (setf tau var-4))
             (setf (f2cl-lib:fref u (1) ((1 3))) one)
             (setf t33
                     (f2cl-lib:fref t$-%data%
                                    (j3 j3)
                                    ((1 ldt) (1 *))
                                    t$-%offset%))
             (dlarfx "L" 3 3 u tau d ldd work)
             (dlarfx "R" 3 3 u tau d ldd work)
             (if
              (>
               (max (abs (f2cl-lib:fref d (2 1) ((1 ldd) (1 4))))
                    (abs (f2cl-lib:fref d (3 1) ((1 ldd) (1 4))))
                    (abs (- (f2cl-lib:fref d (1 1) ((1 ldd) (1 4))) t33)))
               thresh)
              (go label50))
             (dlarfx "R" j3 3 u tau
              (f2cl-lib:array-slice t$-%data%
                                    double-float
                                    (1 j1)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
              ldt work)
             (dlarfx "L" 3 (f2cl-lib:int-sub n j1) u tau
              (f2cl-lib:array-slice t$-%data%
                                    double-float
                                    (j1 j2)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
              ldt work)
             (setf (f2cl-lib:fref t$-%data%
                                  (j1 j1)
                                  ((1 ldt) (1 *))
                                  t$-%offset%)
                     t33)
             (setf (f2cl-lib:fref t$-%data%
                                  (j2 j1)
                                  ((1 ldt) (1 *))
                                  t$-%offset%)
                     zero)
             (setf (f2cl-lib:fref t$-%data%
                                  (j3 j1)
                                  ((1 ldt) (1 *))
                                  t$-%offset%)
                     zero)
             (cond
               (wantq
                (dlarfx "R" n 3 u tau
                 (f2cl-lib:array-slice q-%data%
                                       double-float
                                       (1 j1)
                                       ((1 ldq) (1 *))
                                       q-%offset%)
                 ldq work)))
             (go label40)
            label30
             (setf (f2cl-lib:fref u1 (1) ((1 3)))
                     (- (f2cl-lib:fref x (1 1) ((1 ldx) (1 2)))))
             (setf (f2cl-lib:fref u1 (2) ((1 3)))
                     (- (f2cl-lib:fref x (2 1) ((1 ldx) (1 2)))))
             (setf (f2cl-lib:fref u1 (3) ((1 3))) scale)
             (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                 (dlarfg 3 (f2cl-lib:fref u1 (1) ((1 3)))
                  (f2cl-lib:array-slice u1 double-float (2) ((1 3))) 1 tau1)
               (declare (ignore var-0 var-2 var-3))
               (setf (f2cl-lib:fref u1 (1) ((1 3))) var-1)
               (setf tau1 var-4))
             (setf (f2cl-lib:fref u1 (1) ((1 3))) one)
             (setf temp
                     (* (- tau1)
                        (+ (f2cl-lib:fref x (1 2) ((1 ldx) (1 2)))
                           (* (f2cl-lib:fref u1 (2) ((1 3)))
                              (f2cl-lib:fref x (2 2) ((1 ldx) (1 2)))))))
             (setf (f2cl-lib:fref u2 (1) ((1 3)))
                     (- (* (- temp) (f2cl-lib:fref u1 (2) ((1 3))))
                        (f2cl-lib:fref x (2 2) ((1 ldx) (1 2)))))
             (setf (f2cl-lib:fref u2 (2) ((1 3)))
                     (* (- temp) (f2cl-lib:fref u1 (3) ((1 3)))))
             (setf (f2cl-lib:fref u2 (3) ((1 3))) scale)
             (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                 (dlarfg 3 (f2cl-lib:fref u2 (1) ((1 3)))
                  (f2cl-lib:array-slice u2 double-float (2) ((1 3))) 1 tau2)
               (declare (ignore var-0 var-2 var-3))
               (setf (f2cl-lib:fref u2 (1) ((1 3))) var-1)
               (setf tau2 var-4))
             (setf (f2cl-lib:fref u2 (1) ((1 3))) one)
             (dlarfx "L" 3 4 u1 tau1 d ldd work)
             (dlarfx "R" 4 3 u1 tau1 d ldd work)
             (dlarfx "L" 3 4 u2 tau2
              (f2cl-lib:array-slice d double-float (2 1) ((1 ldd) (1 4))) ldd
              work)
             (dlarfx "R" 4 3 u2 tau2
              (f2cl-lib:array-slice d double-float (1 2) ((1 ldd) (1 4))) ldd
              work)
             (if
              (>
               (max (abs (f2cl-lib:fref d (3 1) ((1 ldd) (1 4))))
                    (abs (f2cl-lib:fref d (3 2) ((1 ldd) (1 4))))
                    (abs (f2cl-lib:fref d (4 1) ((1 ldd) (1 4))))
                    (abs (f2cl-lib:fref d (4 2) ((1 ldd) (1 4)))))
               thresh)
              (go label50))
             (dlarfx "L" 3 (f2cl-lib:int-add (f2cl-lib:int-sub n j1) 1) u1 tau1
              (f2cl-lib:array-slice t$-%data%
                                    double-float
                                    (j1 j1)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
              ldt work)
             (dlarfx "R" j4 3 u1 tau1
              (f2cl-lib:array-slice t$-%data%
                                    double-float
                                    (1 j1)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
              ldt work)
             (dlarfx "L" 3 (f2cl-lib:int-add (f2cl-lib:int-sub n j1) 1) u2 tau2
              (f2cl-lib:array-slice t$-%data%
                                    double-float
                                    (j2 j1)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
              ldt work)
             (dlarfx "R" j4 3 u2 tau2
              (f2cl-lib:array-slice t$-%data%
                                    double-float
                                    (1 j2)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
              ldt work)
             (setf (f2cl-lib:fref t$-%data%
                                  (j3 j1)
                                  ((1 ldt) (1 *))
                                  t$-%offset%)
                     zero)
             (setf (f2cl-lib:fref t$-%data%
                                  (j3 j2)
                                  ((1 ldt) (1 *))
                                  t$-%offset%)
                     zero)
             (setf (f2cl-lib:fref t$-%data%
                                  (j4 j1)
                                  ((1 ldt) (1 *))
                                  t$-%offset%)
                     zero)
             (setf (f2cl-lib:fref t$-%data%
                                  (j4 j2)
                                  ((1 ldt) (1 *))
                                  t$-%offset%)
                     zero)
             (cond
               (wantq
                (dlarfx "R" n 3 u1 tau1
                 (f2cl-lib:array-slice q-%data%
                                       double-float
                                       (1 j1)
                                       ((1 ldq) (1 *))
                                       q-%offset%)
                 ldq work)
                (dlarfx "R" n 3 u2 tau2
                 (f2cl-lib:array-slice q-%data%
                                       double-float
                                       (1 j2)
                                       ((1 ldq) (1 *))
                                       q-%offset%)
                 ldq work)))
            label40
             (cond
               ((= n2 2)
                (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                    (dlanv2
                     (f2cl-lib:fref t$-%data%
                                    (j1 j1)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
                     (f2cl-lib:fref t$-%data%
                                    (j1 j2)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
                     (f2cl-lib:fref t$-%data%
                                    (j2 j1)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
                     (f2cl-lib:fref t$-%data%
                                    (j2 j2)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
                     wr1 wi1 wr2 wi2 cs sn)
                  (declare (ignore))
                  (setf (f2cl-lib:fref t$-%data%
                                       (j1 j1)
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                          var-0)
                  (setf (f2cl-lib:fref t$-%data%
                                       (j1 j2)
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                          var-1)
                  (setf (f2cl-lib:fref t$-%data%
                                       (j2 j1)
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                          var-2)
                  (setf (f2cl-lib:fref t$-%data%
                                       (j2 j2)
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                          var-3)
                  (setf wr1 var-4)
                  (setf wi1 var-5)
                  (setf wr2 var-6)
                  (setf wi2 var-7)
                  (setf cs var-8)
                  (setf sn var-9))
                (drot (f2cl-lib:int-sub n j1 1)
                 (f2cl-lib:array-slice t$-%data%
                                       double-float
                                       (j1 (f2cl-lib:int-add j1 2))
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                 ldt
                 (f2cl-lib:array-slice t$-%data%
                                       double-float
                                       (j2 (f2cl-lib:int-add j1 2))
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                 ldt cs sn)
                (drot (f2cl-lib:int-sub j1 1)
                 (f2cl-lib:array-slice t$-%data%
                                       double-float
                                       (1 j1)
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                 1
                 (f2cl-lib:array-slice t$-%data%
                                       double-float
                                       (1 j2)
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                 1 cs sn)
                (if wantq
                    (drot n
                     (f2cl-lib:array-slice q-%data%
                                           double-float
                                           (1 j1)
                                           ((1 ldq) (1 *))
                                           q-%offset%)
                     1
                     (f2cl-lib:array-slice q-%data%
                                           double-float
                                           (1 j2)
                                           ((1 ldq) (1 *))
                                           q-%offset%)
                     1 cs sn))))
             (cond
               ((= n1 2)
                (setf j3 (f2cl-lib:int-add j1 n2))
                (setf j4 (f2cl-lib:int-add j3 1))
                (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9)
                    (dlanv2
                     (f2cl-lib:fref t$-%data%
                                    (j3 j3)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
                     (f2cl-lib:fref t$-%data%
                                    (j3 j4)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
                     (f2cl-lib:fref t$-%data%
                                    (j4 j3)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
                     (f2cl-lib:fref t$-%data%
                                    (j4 j4)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
                     wr1 wi1 wr2 wi2 cs sn)
                  (declare (ignore))
                  (setf (f2cl-lib:fref t$-%data%
                                       (j3 j3)
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                          var-0)
                  (setf (f2cl-lib:fref t$-%data%
                                       (j3 j4)
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                          var-1)
                  (setf (f2cl-lib:fref t$-%data%
                                       (j4 j3)
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                          var-2)
                  (setf (f2cl-lib:fref t$-%data%
                                       (j4 j4)
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                          var-3)
                  (setf wr1 var-4)
                  (setf wi1 var-5)
                  (setf wr2 var-6)
                  (setf wi2 var-7)
                  (setf cs var-8)
                  (setf sn var-9))
                (if (<= (f2cl-lib:int-add j3 2) n)
                    (drot (f2cl-lib:int-sub n j3 1)
                     (f2cl-lib:array-slice t$-%data%
                                           double-float
                                           (j3 (f2cl-lib:int-add j3 2))
                                           ((1 ldt) (1 *))
                                           t$-%offset%)
                     ldt
                     (f2cl-lib:array-slice t$-%data%
                                           double-float
                                           (j4 (f2cl-lib:int-add j3 2))
                                           ((1 ldt) (1 *))
                                           t$-%offset%)
                     ldt cs sn))
                (drot (f2cl-lib:int-sub j3 1)
                 (f2cl-lib:array-slice t$-%data%
                                       double-float
                                       (1 j3)
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                 1
                 (f2cl-lib:array-slice t$-%data%
                                       double-float
                                       (1 j4)
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                 1 cs sn)
                (if wantq
                    (drot n
                     (f2cl-lib:array-slice q-%data%
                                           double-float
                                           (1 j3)
                                           ((1 ldq) (1 *))
                                           q-%offset%)
                     1
                     (f2cl-lib:array-slice q-%data%
                                           double-float
                                           (1 j4)
                                           ((1 ldq) (1 *))
                                           q-%offset%)
                     1 cs sn)))))))
        (go end_label)
       label50
        (setf info 1)
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlaexc
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(fortran-to-lisp::logical (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlanv2 fortran-to-lisp::dlarfx
                    fortran-to-lisp::dlarfg fortran-to-lisp::dlasy2
                    fortran-to-lisp::dlamch fortran-to-lisp::dlange
                    fortran-to-lisp::dlacpy fortran-to-lisp::drot
                    fortran-to-lisp::dlartg))))

