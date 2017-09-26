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


(let* ((zero 0.0) (one 1.0) (two 2.0) (const 1.5) (nsmax 15) (lds nsmax))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (type (double-float 2.0 2.0) two)
           (type (double-float 1.5 1.5) const)
           (type (f2cl-lib:integer4 15 15) nsmax)
           (type (f2cl-lib:integer4) lds)
           (ignorable zero one two const nsmax lds))
  (defun dhseqr (job compz n ilo ihi h ldh wr wi z ldz work lwork info)
    (declare (type (array double-float (*)) work z wi wr h)
             (type (f2cl-lib:integer4) info lwork ldz ldh ihi ilo n)
             (type (simple-string *) compz job))
    (f2cl-lib:with-multi-array-data
        ((job character job-%data% job-%offset%)
         (compz character compz-%data% compz-%offset%)
         (h double-float h-%data% h-%offset%)
         (wr double-float wr-%data% wr-%offset%)
         (wi double-float wi-%data% wi-%offset%)
         (z double-float z-%data% z-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((s
              (make-array (the fixnum (reduce #'* (list lds nsmax)))
                          :element-type 'double-float))
             (v
              (make-array (f2cl-lib:int-add nsmax 1)
                          :element-type 'double-float))
             (vv
              (make-array (f2cl-lib:int-add nsmax 1)
                          :element-type 'double-float))
             (absw 0.0) (ovfl 0.0) (smlnum 0.0) (tau 0.0) (temp 0.0) (tst1 0.0)
             (ulp 0.0) (unfl 0.0) (i 0) (i1 0) (i2 0) (ierr 0) (ii 0) (itemp 0)
             (itn 0) (its 0) (j 0) (k 0) (l 0) (maxb 0) (nh 0) (nr 0) (ns 0)
             (nv 0) (initz nil) (lquery nil) (wantt nil) (wantz nil))
        (declare (type (array double-float (*)) s v vv)
                 (type (double-float) absw ovfl smlnum tau temp tst1 ulp unfl)
                 (type (f2cl-lib:integer4) i i1 i2 ierr ii itemp itn its j k l
                                           maxb nh nr ns nv)
                 (type f2cl-lib:logical initz lquery wantt wantz))
        (setf wantt (lsame job "S"))
        (setf initz (lsame compz "I"))
        (setf wantz (or initz (lsame compz "V")))
        (setf info 0)
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce
                 (the f2cl-lib:integer4
                      (max (the f2cl-lib:integer4 1)
                           (the f2cl-lib:integer4 n)))
                 'double-float))
        (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
        (cond
          ((and (not (lsame job "E")) (not wantt))
           (setf info -1))
          ((and (not (lsame compz "N")) (not wantz))
           (setf info -2))
          ((< n 0)
           (setf info -3))
          ((or (< ilo 1)
               (> ilo
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n))))
           (setf info -4))
          ((or
            (< ihi (min (the f2cl-lib:integer4 ilo) (the f2cl-lib:integer4 n)))
            (> ihi n))
           (setf info -5))
          ((< ldh (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
           (setf info -7))
          ((or (< ldz 1)
               (and wantz
                    (< ldz
                       (max (the f2cl-lib:integer4 1)
                            (the f2cl-lib:integer4 n)))))
           (setf info -11))
          ((and
            (< lwork (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
            (not lquery))
           (setf info -13)))
        (cond
          ((/= info 0)
           (xerbla "DHSEQR" (f2cl-lib:int-sub info))
           (go end_label))
          (lquery
           (go end_label)))
        (if initz (dlaset "Full" n n zero one z ldz))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i (f2cl-lib:int-add ilo (f2cl-lib:int-sub 1))) nil)
          (tagbody
            (setf (f2cl-lib:fref wr-%data% (i) ((1 *)) wr-%offset%)
                    (f2cl-lib:fref h-%data% (i i) ((1 ldh) (1 *)) h-%offset%))
            (setf (f2cl-lib:fref wi-%data% (i) ((1 *)) wi-%offset%) zero)
           label10))
        (f2cl-lib:fdo (i (f2cl-lib:int-add ihi 1) (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf (f2cl-lib:fref wr-%data% (i) ((1 *)) wr-%offset%)
                    (f2cl-lib:fref h-%data% (i i) ((1 ldh) (1 *)) h-%offset%))
            (setf (f2cl-lib:fref wi-%data% (i) ((1 *)) wi-%offset%) zero)
           label20))
        (if (= n 0) (go end_label))
        (cond
          ((= ilo ihi)
           (setf (f2cl-lib:fref wr-%data% (ilo) ((1 *)) wr-%offset%)
                   (f2cl-lib:fref h-%data%
                                  (ilo ilo)
                                  ((1 ldh) (1 *))
                                  h-%offset%))
           (setf (f2cl-lib:fref wi-%data% (ilo) ((1 *)) wi-%offset%) zero)
           (go end_label)))
        (f2cl-lib:fdo (j ilo (f2cl-lib:int-add j 1))
                      ((> j (f2cl-lib:int-add ihi (f2cl-lib:int-sub 2))) nil)
          (tagbody
            (f2cl-lib:fdo (i (f2cl-lib:int-add j 2) (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (setf (f2cl-lib:fref h-%data% (i j) ((1 ldh) (1 *)) h-%offset%)
                        zero)
               label30))
           label40))
        (setf nh (f2cl-lib:int-add (f2cl-lib:int-sub ihi ilo) 1))
        (setf ns (ilaenv 4 "DHSEQR" (f2cl-lib:f2cl-// job compz) n ilo ihi -1))
        (setf maxb
                (ilaenv 8 "DHSEQR" (f2cl-lib:f2cl-// job compz) n ilo ihi -1))
        (cond
          ((or (<= ns 2) (> ns nh) (>= maxb nh))
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                  var-10 var-11 var-12 var-13)
               (dlahqr wantt wantz n ilo ihi h ldh wr wi ilo ihi z ldz info)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12))
             (setf info var-13))
           (go end_label)))
        (setf maxb
                (max (the f2cl-lib:integer4 3) (the f2cl-lib:integer4 maxb)))
        (setf ns
                (min (the f2cl-lib:integer4 ns)
                     (the f2cl-lib:integer4 maxb)
                     (the f2cl-lib:integer4 nsmax)))
        (setf unfl (dlamch "Safe minimum"))
        (setf ovfl (/ one unfl))
        (multiple-value-bind (var-0 var-1)
            (dlabad unfl ovfl)
          (declare (ignore))
          (setf unfl var-0)
          (setf ovfl var-1))
        (setf ulp (dlamch "Precision"))
        (setf smlnum (* unfl (/ nh ulp)))
        (cond
          (wantt
           (setf i1 1)
           (setf i2 n)))
        (setf itn (f2cl-lib:int-mul 30 nh))
        (setf i ihi)
       label50
        (setf l ilo)
        (if (< i ilo) (go label170))
        (f2cl-lib:fdo (its 0 (f2cl-lib:int-add its 1))
                      ((> its itn) nil)
          (tagbody
            (f2cl-lib:fdo (k i (f2cl-lib:int-add k (f2cl-lib:int-sub 1)))
                          ((> k (f2cl-lib:int-add l 1)) nil)
              (tagbody
                (setf tst1
                        (+
                         (abs
                          (f2cl-lib:fref h-%data%
                                         ((f2cl-lib:int-sub k 1)
                                          (f2cl-lib:int-sub k 1))
                                         ((1 ldh) (1 *))
                                         h-%offset%))
                         (abs
                          (f2cl-lib:fref h-%data%
                                         (k k)
                                         ((1 ldh) (1 *))
                                         h-%offset%))))
                (if (= tst1 zero)
                    (setf tst1
                            (dlanhs "1"
                             (f2cl-lib:int-add (f2cl-lib:int-sub i l) 1)
                             (f2cl-lib:array-slice h-%data%
                                                   double-float
                                                   (l l)
                                                   ((1 ldh) (1 *))
                                                   h-%offset%)
                             ldh work)))
                (if
                 (<=
                  (abs
                   (f2cl-lib:fref h-%data%
                                  (k (f2cl-lib:int-sub k 1))
                                  ((1 ldh) (1 *))
                                  h-%offset%))
                  (max (* ulp tst1) smlnum))
                 (go label70))
               label60))
           label70
            (setf l k)
            (cond
              ((> l ilo)
               (setf (f2cl-lib:fref h-%data%
                                    (l (f2cl-lib:int-sub l 1))
                                    ((1 ldh) (1 *))
                                    h-%offset%)
                       zero)))
            (if (>= l (f2cl-lib:int-add (f2cl-lib:int-sub i maxb) 1))
                (go label160))
            (cond
              ((not wantt)
               (setf i1 l)
               (setf i2 i)))
            (cond
              ((or (= its 20) (= its 30))
               (f2cl-lib:fdo (ii (f2cl-lib:int-add i (f2cl-lib:int-sub ns) 1)
                              (f2cl-lib:int-add ii 1))
                             ((> ii i) nil)
                 (tagbody
                   (setf (f2cl-lib:fref wr-%data% (ii) ((1 *)) wr-%offset%)
                           (* const
                              (+
                               (abs
                                (f2cl-lib:fref h-%data%
                                               (ii (f2cl-lib:int-sub ii 1))
                                               ((1 ldh) (1 *))
                                               h-%offset%))
                               (abs
                                (f2cl-lib:fref h-%data%
                                               (ii ii)
                                               ((1 ldh) (1 *))
                                               h-%offset%)))))
                   (setf (f2cl-lib:fref wi-%data% (ii) ((1 *)) wi-%offset%)
                           zero)
                  label80)))
              (t
               (dlacpy "Full" ns ns
                (f2cl-lib:array-slice h-%data%
                                      double-float
                                      ((+ i (f2cl-lib:int-sub ns) 1)
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-sub i ns)
                                        1))
                                      ((1 ldh) (1 *))
                                      h-%offset%)
                ldh s lds)
               (multiple-value-bind
                     (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                      var-9 var-10 var-11 var-12 var-13)
                   (dlahqr f2cl-lib:%false% f2cl-lib:%false% ns 1 ns s lds
                    (f2cl-lib:array-slice wr-%data%
                                          double-float
                                          ((+ i (f2cl-lib:int-sub ns) 1))
                                          ((1 *))
                                          wr-%offset%)
                    (f2cl-lib:array-slice wi-%data%
                                          double-float
                                          ((+ i (f2cl-lib:int-sub ns) 1))
                                          ((1 *))
                                          wi-%offset%)
                    1 ns z ldz ierr)
                 (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                  var-7 var-8 var-9 var-10 var-11 var-12))
                 (setf ierr var-13))
               (cond
                 ((> ierr 0)
                  (f2cl-lib:fdo (ii 1 (f2cl-lib:int-add ii 1))
                                ((> ii ierr) nil)
                    (tagbody
                      (setf (f2cl-lib:fref wr-%data%
                                           ((f2cl-lib:int-add
                                             (f2cl-lib:int-sub i ns)
                                             ii))
                                           ((1 *))
                                           wr-%offset%)
                              (f2cl-lib:fref s (ii ii) ((1 lds) (1 nsmax))))
                      (setf (f2cl-lib:fref wi-%data%
                                           ((f2cl-lib:int-add
                                             (f2cl-lib:int-sub i ns)
                                             ii))
                                           ((1 *))
                                           wi-%offset%)
                              zero)
                     label90))))))
            (setf (f2cl-lib:fref v (1) ((1 (f2cl-lib:int-add nsmax 1)))) one)
            (f2cl-lib:fdo (ii 2 (f2cl-lib:int-add ii 1))
                          ((> ii (f2cl-lib:int-add ns 1)) nil)
              (tagbody
                (setf (f2cl-lib:fref v (ii) ((1 (f2cl-lib:int-add nsmax 1))))
                        zero)
               label100))
            (setf nv 1)
            (f2cl-lib:fdo (j (f2cl-lib:int-add i (f2cl-lib:int-sub ns) 1)
                           (f2cl-lib:int-add j 1))
                          ((> j i) nil)
              (tagbody
                (cond
                  ((>= (f2cl-lib:fref wi (j) ((1 *))) zero)
                   (cond
                     ((= (f2cl-lib:fref wi (j) ((1 *))) zero)
                      (dcopy (f2cl-lib:int-add nv 1) v 1 vv 1)
                      (dgemv "No transpose" (f2cl-lib:int-add nv 1) nv one
                       (f2cl-lib:array-slice h-%data%
                                             double-float
                                             (l l)
                                             ((1 ldh) (1 *))
                                             h-%offset%)
                       ldh vv 1
                       (- (f2cl-lib:fref wr-%data% (j) ((1 *)) wr-%offset%)) v
                       1)
                      (setf nv (f2cl-lib:int-add nv 1)))
                     ((> (f2cl-lib:fref wi (j) ((1 *))) zero)
                      (dcopy (f2cl-lib:int-add nv 1) v 1 vv 1)
                      (dgemv "No transpose" (f2cl-lib:int-add nv 1) nv one
                       (f2cl-lib:array-slice h-%data%
                                             double-float
                                             (l l)
                                             ((1 ldh) (1 *))
                                             h-%offset%)
                       ldh v 1
                       (* (- two)
                          (f2cl-lib:fref wr-%data% (j) ((1 *)) wr-%offset%))
                       vv 1)
                      (setf itemp (idamax (f2cl-lib:int-add nv 1) vv 1))
                      (setf temp
                              (/ one
                                 (max
                                  (abs
                                   (f2cl-lib:fref vv
                                                  (itemp)
                                                  ((1
                                                    (f2cl-lib:int-add nsmax
                                                                      1)))))
                                  smlnum)))
                      (dscal (f2cl-lib:int-add nv 1) temp vv 1)
                      (setf absw
                              (dlapy2
                               (f2cl-lib:fref wr-%data%
                                              (j)
                                              ((1 *))
                                              wr-%offset%)
                               (f2cl-lib:fref wi-%data%
                                              (j)
                                              ((1 *))
                                              wi-%offset%)))
                      (setf temp (* temp absw absw))
                      (dgemv "No transpose" (f2cl-lib:int-add nv 2)
                       (f2cl-lib:int-add nv 1) one
                       (f2cl-lib:array-slice h-%data%
                                             double-float
                                             (l l)
                                             ((1 ldh) (1 *))
                                             h-%offset%)
                       ldh vv 1 temp v 1)
                      (setf nv (f2cl-lib:int-add nv 2))))
                   (setf itemp (idamax nv v 1))
                   (setf temp
                           (abs
                            (f2cl-lib:fref v
                                           (itemp)
                                           ((1 (f2cl-lib:int-add nsmax 1))))))
                   (cond
                     ((= temp zero)
                      (setf (f2cl-lib:fref v
                                           (1)
                                           ((1 (f2cl-lib:int-add nsmax 1))))
                              one)
                      (f2cl-lib:fdo (ii 2 (f2cl-lib:int-add ii 1))
                                    ((> ii nv) nil)
                        (tagbody
                          (setf (f2cl-lib:fref v
                                               (ii)
                                               ((1
                                                 (f2cl-lib:int-add nsmax 1))))
                                  zero)
                         label110)))
                     (t
                      (setf temp (max temp smlnum))
                      (dscal nv (/ one temp) v 1)))))
               label120))
            (f2cl-lib:fdo (k l (f2cl-lib:int-add k 1))
                          ((> k (f2cl-lib:int-add i (f2cl-lib:int-sub 1))) nil)
              (tagbody
                (setf nr
                        (min (the f2cl-lib:integer4 (f2cl-lib:int-add ns 1))
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-sub i k)
                                                    1))))
                (if (> k l)
                    (dcopy nr
                     (f2cl-lib:array-slice h-%data%
                                           double-float
                                           (k (f2cl-lib:int-sub k 1))
                                           ((1 ldh) (1 *))
                                           h-%offset%)
                     1 v 1))
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                    (dlarfg nr
                     (f2cl-lib:fref v (1) ((1 (f2cl-lib:int-add nsmax 1))))
                     (f2cl-lib:array-slice v
                                           double-float
                                           (2)
                                           ((1 (f2cl-lib:int-add nsmax 1))))
                     1 tau)
                  (declare (ignore var-0 var-2 var-3))
                  (setf (f2cl-lib:fref v (1) ((1 (f2cl-lib:int-add nsmax 1))))
                          var-1)
                  (setf tau var-4))
                (cond
                  ((> k l)
                   (setf (f2cl-lib:fref h-%data%
                                        (k (f2cl-lib:int-sub k 1))
                                        ((1 ldh) (1 *))
                                        h-%offset%)
                           (f2cl-lib:fref v
                                          (1)
                                          ((1 (f2cl-lib:int-add nsmax 1)))))
                   (f2cl-lib:fdo (ii (f2cl-lib:int-add k 1)
                                  (f2cl-lib:int-add ii 1))
                                 ((> ii i) nil)
                     (tagbody
                       (setf (f2cl-lib:fref h-%data%
                                            (ii (f2cl-lib:int-sub k 1))
                                            ((1 ldh) (1 *))
                                            h-%offset%)
                               zero)
                      label130))))
                (setf (f2cl-lib:fref v (1) ((1 (f2cl-lib:int-add nsmax 1))))
                        one)
                (dlarfx "Left" nr (f2cl-lib:int-add (f2cl-lib:int-sub i2 k) 1)
                 v tau
                 (f2cl-lib:array-slice h-%data%
                                       double-float
                                       (k k)
                                       ((1 ldh) (1 *))
                                       h-%offset%)
                 ldh work)
                (dlarfx "Right"
                 (f2cl-lib:int-add
                  (f2cl-lib:int-sub
                   (min (the f2cl-lib:integer4 (f2cl-lib:int-add k nr))
                        (the f2cl-lib:integer4 i))
                   i1)
                  1)
                 nr v tau
                 (f2cl-lib:array-slice h-%data%
                                       double-float
                                       (i1 k)
                                       ((1 ldh) (1 *))
                                       h-%offset%)
                 ldh work)
                (cond
                  (wantz
                   (dlarfx "Right" nh nr v tau
                    (f2cl-lib:array-slice z-%data%
                                          double-float
                                          (ilo k)
                                          ((1 ldz) (1 *))
                                          z-%offset%)
                    ldz work)))
               label140))
           label150))
        (setf info i)
        (go end_label)
       label160
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11 var-12 var-13)
            (dlahqr wantt wantz n l i h ldh wr wi ilo ihi z ldz info)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12))
          (setf info var-13))
        (if (> info 0) (go end_label))
        (setf itn (f2cl-lib:int-sub itn its))
        (setf i (f2cl-lib:int-sub l 1))
        (go label50)
       label170
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce
                 (the f2cl-lib:integer4
                      (max (the f2cl-lib:integer4 1)
                           (the f2cl-lib:integer4 n)))
                 'double-float))
        (go end_label)
       end_label
        (return
         (values nil nil nil nil nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dhseqr
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlarfx fortran-to-lisp::dlarfg
                    fortran-to-lisp::dlapy2 fortran-to-lisp::dscal
                    fortran-to-lisp::idamax fortran-to-lisp::dgemv
                    fortran-to-lisp::dcopy fortran-to-lisp::dlacpy
                    fortran-to-lisp::dlanhs fortran-to-lisp::dlabad
                    fortran-to-lisp::dlamch fortran-to-lisp::dlahqr
                    fortran-to-lisp::ilaenv fortran-to-lisp::dlaset
                    fortran-to-lisp::xerbla fortran-to-lisp::lsame))))

