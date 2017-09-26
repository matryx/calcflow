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


(let* ((cbias 1.5)
       (zero 0.0)
       (half 0.5)
       (one 1.0)
       (two 2.0)
       (four 4.0)
       (hundrd 100.0))
  (declare (type (double-float 1.5 1.5) cbias)
           (type (double-float 0.0 0.0) zero)
           (type (double-float 0.5 0.5) half)
           (type (double-float 1.0 1.0) one)
           (type (double-float 2.0 2.0) two)
           (type (double-float 4.0 4.0) four)
           (type (double-float 100.0 100.0) hundrd)
           (ignorable cbias zero half one two four hundrd))
  (defun dlasq2 (n z info)
    (declare (type (array double-float (*)) z)
             (type (f2cl-lib:integer4) info n))
    (f2cl-lib:with-multi-array-data
        ((z double-float z-%data% z-%offset%))
      (prog ((d 0.0) (desig 0.0) (dmin 0.0) (e 0.0) (emax 0.0) (emin 0.0)
             (eps 0.0) (oldemn 0.0) (qmax 0.0) (qmin 0.0) (s 0.0) (safmin 0.0)
             (sigma 0.0) (t$ 0.0) (temp 0.0) (tol 0.0) (tol2 0.0) (trace$ 0.0)
             (zmax 0.0) (i0 0) (i4 0) (iinfo 0) (ipn4 0) (iter 0) (iwhila 0)
             (iwhilb 0) (k 0) (n0 0) (nbig 0) (ndiv 0) (nfail 0) (pp 0)
             (splt 0) (ieee nil))
        (declare (type (double-float) d desig dmin e emax emin eps oldemn qmax
                                      qmin s safmin sigma t$ temp tol tol2
                                      trace$ zmax)
                 (type (f2cl-lib:integer4) i0 i4 iinfo ipn4 iter iwhila iwhilb
                                           k n0 nbig ndiv nfail pp splt)
                 (type f2cl-lib:logical ieee))
        (setf info 0)
        (setf eps (dlamch "Precision"))
        (setf safmin (dlamch "Safe minimum"))
        (setf tol (* eps hundrd))
        (setf tol2 (expt tol 2))
        (cond
          ((< n 0)
           (setf info -1)
           (xerbla "DLASQ2" 1)
           (go end_label))
          ((= n 0)
           (go end_label))
          ((= n 1)
           (cond
             ((< (f2cl-lib:fref z (1) ((1 *))) zero)
              (setf info -201)
              (xerbla "DLASQ2" 2)))
           (go end_label))
          ((= n 2)
           (cond
             ((or (< (f2cl-lib:fref z (2) ((1 *))) zero)
                  (< (f2cl-lib:fref z (3) ((1 *))) zero))
              (setf info -2)
              (xerbla "DLASQ2" 2)
              (go end_label))
             ((> (f2cl-lib:fref z (3) ((1 *))) (f2cl-lib:fref z (1) ((1 *))))
              (setf d (f2cl-lib:fref z-%data% (3) ((1 *)) z-%offset%))
              (setf (f2cl-lib:fref z-%data% (3) ((1 *)) z-%offset%)
                      (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%))
              (setf (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%) d)))
           (setf (f2cl-lib:fref z-%data% (5) ((1 *)) z-%offset%)
                   (+ (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%)
                      (f2cl-lib:fref z-%data% (2) ((1 *)) z-%offset%)
                      (f2cl-lib:fref z-%data% (3) ((1 *)) z-%offset%)))
           (cond
             ((> (f2cl-lib:fref z (2) ((1 *)))
                 (* (f2cl-lib:fref z (3) ((1 *))) tol2))
              (setf t$
                      (* half
                         (+
                          (- (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%)
                             (f2cl-lib:fref z-%data% (3) ((1 *)) z-%offset%))
                          (f2cl-lib:fref z-%data% (2) ((1 *)) z-%offset%))))
              (setf s
                      (* (f2cl-lib:fref z-%data% (3) ((1 *)) z-%offset%)
                         (/ (f2cl-lib:fref z-%data% (2) ((1 *)) z-%offset%)
                            t$)))
              (cond
                ((<= s t$)
                 (setf s
                         (* (f2cl-lib:fref z-%data% (3) ((1 *)) z-%offset%)
                            (/ (f2cl-lib:fref z-%data% (2) ((1 *)) z-%offset%)
                               (* t$
                                  (+ one (f2cl-lib:fsqrt (+ one (/ s t$)))))))))
                (t
                 (setf s
                         (* (f2cl-lib:fref z-%data% (3) ((1 *)) z-%offset%)
                            (/ (f2cl-lib:fref z-%data% (2) ((1 *)) z-%offset%)
                               (+ t$
                                  (* (f2cl-lib:fsqrt t$)
                                     (f2cl-lib:fsqrt (+ t$ s)))))))))
              (setf t$
                      (+ (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%)
                         (+ s (f2cl-lib:fref z-%data% (2) ((1 *)) z-%offset%))))
              (setf (f2cl-lib:fref z-%data% (3) ((1 *)) z-%offset%)
                      (* (f2cl-lib:fref z-%data% (3) ((1 *)) z-%offset%)
                         (/ (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%)
                            t$)))
              (setf (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%) t$)))
           (setf (f2cl-lib:fref z-%data% (2) ((1 *)) z-%offset%)
                   (f2cl-lib:fref z-%data% (3) ((1 *)) z-%offset%))
           (setf (f2cl-lib:fref z-%data% (6) ((1 *)) z-%offset%)
                   (+ (f2cl-lib:fref z-%data% (2) ((1 *)) z-%offset%)
                      (f2cl-lib:fref z-%data% (1) ((1 *)) z-%offset%)))
           (go end_label)))
        (setf (f2cl-lib:fref z-%data%
                             ((f2cl-lib:int-mul 2 n))
                             ((1 *))
                             z-%offset%)
                zero)
        (setf emin (f2cl-lib:fref z-%data% (2) ((1 *)) z-%offset%))
        (setf qmax zero)
        (setf zmax zero)
        (setf d zero)
        (setf e zero)
        (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 2))
                      ((> k
                          (f2cl-lib:int-mul 2
                                            (f2cl-lib:int-add n
                                                              (f2cl-lib:int-sub
                                                               1))))
                       nil)
          (tagbody
            (cond
              ((< (f2cl-lib:fref z (k) ((1 *))) zero)
               (setf info (f2cl-lib:int-sub (f2cl-lib:int-add 200 k)))
               (xerbla "DLASQ2" 2)
               (go end_label))
              ((< (f2cl-lib:fref z ((f2cl-lib:int-add k 1)) ((1 *))) zero)
               (setf info (f2cl-lib:int-sub (f2cl-lib:int-add 200 k 1)))
               (xerbla "DLASQ2" 2)
               (go end_label)))
            (setf d (+ d (f2cl-lib:fref z-%data% (k) ((1 *)) z-%offset%)))
            (setf e
                    (+ e
                       (f2cl-lib:fref z-%data%
                                      ((f2cl-lib:int-add k 1))
                                      ((1 *))
                                      z-%offset%)))
            (setf qmax
                    (max qmax (f2cl-lib:fref z-%data% (k) ((1 *)) z-%offset%)))
            (setf emin
                    (min emin
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-add k 1))
                                        ((1 *))
                                        z-%offset%)))
            (setf zmax
                    (max qmax
                         zmax
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-add k 1))
                                        ((1 *))
                                        z-%offset%)))
           label10))
        (cond
          ((<
            (f2cl-lib:fref z
                           ((f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                              (f2cl-lib:int-sub 1)))
                           ((1 *)))
            zero)
           (setf info
                   (f2cl-lib:int-sub
                    (f2cl-lib:int-sub
                     (f2cl-lib:int-add 200 (f2cl-lib:int-mul 2 n))
                     1)))
           (xerbla "DLASQ2" 2)
           (go end_label)))
        (setf d
                (+ d
                   (f2cl-lib:fref z-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-mul 2 n) 1))
                                  ((1 *))
                                  z-%offset%)))
        (setf qmax
                (max qmax
                     (f2cl-lib:fref z-%data%
                                    ((f2cl-lib:int-sub (f2cl-lib:int-mul 2 n)
                                                       1))
                                    ((1 *))
                                    z-%offset%)))
        (setf zmax (max qmax zmax))
        (cond
          ((= e zero)
           (f2cl-lib:fdo (k 2 (f2cl-lib:int-add k 1))
                         ((> k n) nil)
             (tagbody
               (setf (f2cl-lib:fref z-%data% (k) ((1 *)) z-%offset%)
                       (f2cl-lib:fref z-%data%
                                      ((f2cl-lib:int-sub (f2cl-lib:int-mul 2 k)
                                                         1))
                                      ((1 *))
                                      z-%offset%))
              label20))
           (multiple-value-bind (var-0 var-1 var-2 var-3)
               (dlasrt "D" n z iinfo)
             (declare (ignore var-0 var-1 var-2))
             (setf iinfo var-3))
           (setf (f2cl-lib:fref z-%data%
                                ((f2cl-lib:int-sub (f2cl-lib:int-mul 2 n) 1))
                                ((1 *))
                                z-%offset%)
                   d)
           (go end_label)))
        (setf trace$ (+ d e))
        (cond
          ((= trace$ zero)
           (setf (f2cl-lib:fref z-%data%
                                ((f2cl-lib:int-sub (f2cl-lib:int-mul 2 n) 1))
                                ((1 *))
                                z-%offset%)
                   zero)
           (go end_label)))
        (setf ieee
                (and (= (ilaenv 10 "DLASQ2" "N" 1 2 3 4) 1)
                     (= (ilaenv 11 "DLASQ2" "N" 1 2 3 4) 1)))
        (f2cl-lib:fdo (k (f2cl-lib:int-mul 2 n)
                       (f2cl-lib:int-add k (f2cl-lib:int-sub 2)))
                      ((> k 2) nil)
          (tagbody
            (setf (f2cl-lib:fref z-%data%
                                 ((f2cl-lib:int-mul 2 k))
                                 ((1 *))
                                 z-%offset%)
                    zero)
            (setf (f2cl-lib:fref z-%data%
                                 ((f2cl-lib:int-sub (f2cl-lib:int-mul 2 k) 1))
                                 ((1 *))
                                 z-%offset%)
                    (f2cl-lib:fref z-%data% (k) ((1 *)) z-%offset%))
            (setf (f2cl-lib:fref z-%data%
                                 ((f2cl-lib:int-sub (f2cl-lib:int-mul 2 k) 2))
                                 ((1 *))
                                 z-%offset%)
                    zero)
            (setf (f2cl-lib:fref z-%data%
                                 ((f2cl-lib:int-sub (f2cl-lib:int-mul 2 k) 3))
                                 ((1 *))
                                 z-%offset%)
                    (f2cl-lib:fref z-%data%
                                   ((f2cl-lib:int-sub k 1))
                                   ((1 *))
                                   z-%offset%))
           label30))
        (setf i0 1)
        (setf n0 n)
        (cond
          ((<
            (* cbias
               (f2cl-lib:fref z
                              ((f2cl-lib:int-add (f2cl-lib:int-mul 4 i0)
                                                 (f2cl-lib:int-sub 3)))
                              ((1 *))))
            (f2cl-lib:fref z
                           ((f2cl-lib:int-add (f2cl-lib:int-mul 4 n0)
                                              (f2cl-lib:int-sub 3)))
                           ((1 *))))
           (setf ipn4 (f2cl-lib:int-mul 4 (f2cl-lib:int-add i0 n0)))
           (f2cl-lib:fdo (i4 (f2cl-lib:int-mul 4 i0) (f2cl-lib:int-add i4 4))
                         ((> i4
                             (f2cl-lib:int-mul 2
                                               (f2cl-lib:int-add i0
                                                                 n0
                                                                 (f2cl-lib:int-sub
                                                                  1))))
                          nil)
             (tagbody
               (setf temp
                       (f2cl-lib:fref z-%data%
                                      ((f2cl-lib:int-sub i4 3))
                                      ((1 *))
                                      z-%offset%))
               (setf (f2cl-lib:fref z-%data%
                                    ((f2cl-lib:int-sub i4 3))
                                    ((1 *))
                                    z-%offset%)
                       (f2cl-lib:fref z-%data%
                                      ((f2cl-lib:int-sub ipn4 i4 3))
                                      ((1 *))
                                      z-%offset%))
               (setf (f2cl-lib:fref z-%data%
                                    ((f2cl-lib:int-sub ipn4 i4 3))
                                    ((1 *))
                                    z-%offset%)
                       temp)
               (setf temp
                       (f2cl-lib:fref z-%data%
                                      ((f2cl-lib:int-sub i4 1))
                                      ((1 *))
                                      z-%offset%))
               (setf (f2cl-lib:fref z-%data%
                                    ((f2cl-lib:int-sub i4 1))
                                    ((1 *))
                                    z-%offset%)
                       (f2cl-lib:fref z-%data%
                                      ((f2cl-lib:int-sub ipn4 i4 5))
                                      ((1 *))
                                      z-%offset%))
               (setf (f2cl-lib:fref z-%data%
                                    ((f2cl-lib:int-sub ipn4 i4 5))
                                    ((1 *))
                                    z-%offset%)
                       temp)
              label40))))
        (setf pp 0)
        (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                      ((> k 2) nil)
          (tagbody
            (setf d
                    (f2cl-lib:fref z-%data%
                                   ((f2cl-lib:int-sub
                                     (f2cl-lib:int-add (f2cl-lib:int-mul 4 n0)
                                                       pp)
                                     3))
                                   ((1 *))
                                   z-%offset%))
            (f2cl-lib:fdo (i4
                           (f2cl-lib:int-add
                            (f2cl-lib:int-mul 4
                                              (f2cl-lib:int-add n0
                                                                (f2cl-lib:int-sub
                                                                 1)))
                            pp)
                           (f2cl-lib:int-add i4 (f2cl-lib:int-sub 4)))
                          ((> i4 (f2cl-lib:int-add (f2cl-lib:int-mul 4 i0) pp))
                           nil)
              (tagbody
                (cond
                  ((<=
                    (f2cl-lib:fref z
                                   ((f2cl-lib:int-add i4 (f2cl-lib:int-sub 1)))
                                   ((1 *)))
                    (* tol2 d))
                   (setf (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub i4 1))
                                        ((1 *))
                                        z-%offset%)
                           (- zero))
                   (setf d
                           (f2cl-lib:fref z-%data%
                                          ((f2cl-lib:int-sub i4 3))
                                          ((1 *))
                                          z-%offset%)))
                  (t
                   (setf d
                           (*
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-sub i4 3))
                                           ((1 *))
                                           z-%offset%)
                            (/ d
                               (+ d
                                  (f2cl-lib:fref z-%data%
                                                 ((f2cl-lib:int-sub i4 1))
                                                 ((1 *))
                                                 z-%offset%)))))))
               label50))
            (setf emin
                    (f2cl-lib:fref z-%data%
                                   ((f2cl-lib:int-add (f2cl-lib:int-mul 4 i0)
                                                      pp
                                                      1))
                                   ((1 *))
                                   z-%offset%))
            (setf d
                    (f2cl-lib:fref z-%data%
                                   ((f2cl-lib:int-sub
                                     (f2cl-lib:int-add (f2cl-lib:int-mul 4 i0)
                                                       pp)
                                     3))
                                   ((1 *))
                                   z-%offset%))
            (f2cl-lib:fdo (i4 (f2cl-lib:int-add (f2cl-lib:int-mul 4 i0) pp)
                           (f2cl-lib:int-add i4 4))
                          ((> i4
                              (f2cl-lib:int-add
                               (f2cl-lib:int-mul 4
                                                 (f2cl-lib:int-add n0
                                                                   (f2cl-lib:int-sub
                                                                    1)))
                               pp))
                           nil)
              (tagbody
                (setf (f2cl-lib:fref z-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add i4
                                                         (f2cl-lib:int-mul -1
                                                                           2
                                                                           pp))
                                       2))
                                     ((1 *))
                                     z-%offset%)
                        (+ d
                           (f2cl-lib:fref z-%data%
                                          ((f2cl-lib:int-sub i4 1))
                                          ((1 *))
                                          z-%offset%)))
                (cond
                  ((<=
                    (f2cl-lib:fref z
                                   ((f2cl-lib:int-add i4 (f2cl-lib:int-sub 1)))
                                   ((1 *)))
                    (* tol2 d))
                   (setf (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub i4 1))
                                        ((1 *))
                                        z-%offset%)
                           (- zero))
                   (setf (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub
                                          (f2cl-lib:int-add i4
                                                            (f2cl-lib:int-mul
                                                             -1
                                                             2
                                                             pp))
                                          2))
                                        ((1 *))
                                        z-%offset%)
                           d)
                   (setf (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-add i4
                                                           (f2cl-lib:int-mul -1
                                                                             2
                                                                             pp)))
                                        ((1 *))
                                        z-%offset%)
                           zero)
                   (setf d
                           (f2cl-lib:fref z-%data%
                                          ((f2cl-lib:int-add i4 1))
                                          ((1 *))
                                          z-%offset%)))
                  ((and
                    (<
                     (* safmin
                        (f2cl-lib:fref z ((f2cl-lib:int-add i4 1)) ((1 *))))
                     (f2cl-lib:fref z
                                    ((f2cl-lib:int-add i4
                                                       (f2cl-lib:int-mul -1
                                                                         2
                                                                         pp)
                                                       (f2cl-lib:int-sub 2)))
                                    ((1 *))))
                    (<
                     (* safmin
                        (f2cl-lib:fref z
                                       ((f2cl-lib:int-add i4
                                                          (f2cl-lib:int-mul -1
                                                                            2
                                                                            pp)
                                                          (f2cl-lib:int-sub
                                                           2)))
                                       ((1 *))))
                     (f2cl-lib:fref z ((f2cl-lib:int-add i4 1)) ((1 *)))))
                   (setf temp
                           (/
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-add i4 1))
                                           ((1 *))
                                           z-%offset%)
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-sub
                                             (f2cl-lib:int-add i4
                                                               (f2cl-lib:int-mul
                                                                -1
                                                                2
                                                                pp))
                                             2))
                                           ((1 *))
                                           z-%offset%)))
                   (setf (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-add i4
                                                           (f2cl-lib:int-mul -1
                                                                             2
                                                                             pp)))
                                        ((1 *))
                                        z-%offset%)
                           (*
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-sub i4 1))
                                           ((1 *))
                                           z-%offset%)
                            temp))
                   (setf d (* d temp)))
                  (t
                   (setf (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-add i4
                                                           (f2cl-lib:int-mul -1
                                                                             2
                                                                             pp)))
                                        ((1 *))
                                        z-%offset%)
                           (*
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-add i4 1))
                                           ((1 *))
                                           z-%offset%)
                            (/
                             (f2cl-lib:fref z-%data%
                                            ((f2cl-lib:int-sub i4 1))
                                            ((1 *))
                                            z-%offset%)
                             (f2cl-lib:fref z-%data%
                                            ((f2cl-lib:int-sub
                                              (f2cl-lib:int-add i4
                                                                (f2cl-lib:int-mul
                                                                 -1
                                                                 2
                                                                 pp))
                                              2))
                                            ((1 *))
                                            z-%offset%))))
                   (setf d
                           (*
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-add i4 1))
                                           ((1 *))
                                           z-%offset%)
                            (/ d
                               (f2cl-lib:fref z-%data%
                                              ((f2cl-lib:int-sub
                                                (f2cl-lib:int-add i4
                                                                  (f2cl-lib:int-mul
                                                                   -1
                                                                   2
                                                                   pp))
                                                2))
                                              ((1 *))
                                              z-%offset%))))))
                (setf emin
                        (min emin
                             (f2cl-lib:fref z-%data%
                                            ((f2cl-lib:int-add i4
                                                               (f2cl-lib:int-mul
                                                                -1
                                                                2
                                                                pp)))
                                            ((1 *))
                                            z-%offset%)))
               label60))
            (setf (f2cl-lib:fref z-%data%
                                 ((f2cl-lib:int-sub (f2cl-lib:int-mul 4 n0)
                                                    pp
                                                    2))
                                 ((1 *))
                                 z-%offset%)
                    d)
            (setf qmax
                    (f2cl-lib:fref z-%data%
                                   ((f2cl-lib:int-sub (f2cl-lib:int-mul 4 i0)
                                                      pp
                                                      2))
                                   ((1 *))
                                   z-%offset%))
            (f2cl-lib:fdo (i4
                           (f2cl-lib:int-add (f2cl-lib:int-mul 4 i0)
                                             (f2cl-lib:int-sub pp)
                                             2)
                           (f2cl-lib:int-add i4 4))
                          ((> i4
                              (f2cl-lib:int-add (f2cl-lib:int-mul 4 n0)
                                                (f2cl-lib:int-sub pp)
                                                (f2cl-lib:int-sub 2)))
                           nil)
              (tagbody
                (setf qmax
                        (max qmax
                             (f2cl-lib:fref z-%data% (i4) ((1 *)) z-%offset%)))
               label70))
            (setf pp (f2cl-lib:int-sub 1 pp))
           label80))
        (setf iter 2)
        (setf nfail 0)
        (setf ndiv (f2cl-lib:int-mul 2 (f2cl-lib:int-sub n0 i0)))
        (f2cl-lib:fdo (iwhila 1 (f2cl-lib:int-add iwhila 1))
                      ((> iwhila (f2cl-lib:int-add n 1)) nil)
          (tagbody
            (if (< n0 1) (go label150))
            (setf desig zero)
            (cond
              ((= n0 n)
               (setf sigma zero))
              (t
               (setf sigma
                       (-
                        (f2cl-lib:fref z-%data%
                                       ((f2cl-lib:int-sub
                                         (f2cl-lib:int-mul 4 n0)
                                         1))
                                       ((1 *))
                                       z-%offset%)))))
            (cond
              ((< sigma zero)
               (setf info 1)
               (go end_label)))
            (setf emax zero)
            (cond
              ((> n0 i0)
               (setf emin
                       (abs
                        (f2cl-lib:fref z-%data%
                                       ((f2cl-lib:int-sub
                                         (f2cl-lib:int-mul 4 n0)
                                         5))
                                       ((1 *))
                                       z-%offset%))))
              (t
               (setf emin zero)))
            (setf qmin
                    (f2cl-lib:fref z-%data%
                                   ((f2cl-lib:int-sub (f2cl-lib:int-mul 4 n0)
                                                      3))
                                   ((1 *))
                                   z-%offset%))
            (setf qmax qmin)
            (f2cl-lib:fdo (i4 (f2cl-lib:int-mul 4 n0)
                           (f2cl-lib:int-add i4 (f2cl-lib:int-sub 4)))
                          ((> i4 8) nil)
              (tagbody
                (if
                 (<=
                  (f2cl-lib:fref z-%data%
                                 ((f2cl-lib:int-sub i4 5))
                                 ((1 *))
                                 z-%offset%)
                  zero)
                 (go label100))
                (cond
                  ((>= qmin (* four emax))
                   (setf qmin
                           (min qmin
                                (f2cl-lib:fref z-%data%
                                               ((f2cl-lib:int-sub i4 3))
                                               ((1 *))
                                               z-%offset%)))
                   (setf emax
                           (max emax
                                (f2cl-lib:fref z-%data%
                                               ((f2cl-lib:int-sub i4 5))
                                               ((1 *))
                                               z-%offset%)))))
                (setf qmax
                        (max qmax
                             (+
                              (f2cl-lib:fref z-%data%
                                             ((f2cl-lib:int-sub i4 7))
                                             ((1 *))
                                             z-%offset%)
                              (f2cl-lib:fref z-%data%
                                             ((f2cl-lib:int-sub i4 5))
                                             ((1 *))
                                             z-%offset%))))
                (setf emin
                        (min emin
                             (f2cl-lib:fref z-%data%
                                            ((f2cl-lib:int-sub i4 5))
                                            ((1 *))
                                            z-%offset%)))
               label90))
            (setf i4 4)
           label100
            (setf i0 (the f2cl-lib:integer4 (truncate i4 4)))
            (setf (f2cl-lib:fref z-%data%
                                 ((f2cl-lib:int-sub (f2cl-lib:int-mul 4 n0) 1))
                                 ((1 *))
                                 z-%offset%)
                    emin)
            (setf dmin
                    (-
                     (max zero
                          (+ qmin
                             (* (- two)
                                (f2cl-lib:fsqrt qmin)
                                (f2cl-lib:fsqrt emax))))))
            (setf pp 0)
            (setf nbig
                    (f2cl-lib:int-mul 30
                                      (f2cl-lib:int-add
                                       (f2cl-lib:int-sub n0 i0)
                                       1)))
            (f2cl-lib:fdo (iwhilb 1 (f2cl-lib:int-add iwhilb 1))
                          ((> iwhilb nbig) nil)
              (tagbody
                (if (> i0 n0) (go label130))
                (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10 var-11)
                    (dlasq3 i0 n0 z pp dmin sigma desig qmax nfail iter ndiv
                     ieee)
                  (declare (ignore var-0 var-2 var-3 var-11))
                  (setf n0 var-1)
                  (setf dmin var-4)
                  (setf sigma var-5)
                  (setf desig var-6)
                  (setf qmax var-7)
                  (setf nfail var-8)
                  (setf iter var-9)
                  (setf ndiv var-10))
                (setf pp (f2cl-lib:int-sub 1 pp))
                (cond
                  ((and (= pp 0)
                        (>= (f2cl-lib:int-add n0 (f2cl-lib:int-sub i0)) 3))
                   (cond
                     ((or
                       (<= (f2cl-lib:fref z ((f2cl-lib:int-mul 4 n0)) ((1 *)))
                           (* tol2 qmax))
                       (<=
                        (f2cl-lib:fref z
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-mul 4 n0)
                                         (f2cl-lib:int-sub 1)))
                                       ((1 *)))
                        (* tol2 sigma)))
                      (setf splt (f2cl-lib:int-sub i0 1))
                      (setf qmax
                              (f2cl-lib:fref z-%data%
                                             ((f2cl-lib:int-sub
                                               (f2cl-lib:int-mul 4 i0)
                                               3))
                                             ((1 *))
                                             z-%offset%))
                      (setf emin
                              (f2cl-lib:fref z-%data%
                                             ((f2cl-lib:int-sub
                                               (f2cl-lib:int-mul 4 i0)
                                               1))
                                             ((1 *))
                                             z-%offset%))
                      (setf oldemn
                              (f2cl-lib:fref z-%data%
                                             ((f2cl-lib:int-mul 4 i0))
                                             ((1 *))
                                             z-%offset%))
                      (f2cl-lib:fdo (i4 (f2cl-lib:int-mul 4 i0)
                                     (f2cl-lib:int-add i4 4))
                                    ((> i4
                                        (f2cl-lib:int-mul 4
                                                          (f2cl-lib:int-add n0
                                                                            (f2cl-lib:int-sub
                                                                             3))))
                                     nil)
                        (tagbody
                          (cond
                            ((or
                              (<= (f2cl-lib:fref z (i4) ((1 *)))
                                  (* tol2
                                     (f2cl-lib:fref z
                                                    ((f2cl-lib:int-add i4
                                                                       (f2cl-lib:int-sub
                                                                        3)))
                                                    ((1 *)))))
                              (<=
                               (f2cl-lib:fref z
                                              ((f2cl-lib:int-add i4
                                                                 (f2cl-lib:int-sub
                                                                  1)))
                                              ((1 *)))
                               (* tol2 sigma)))
                             (setf (f2cl-lib:fref z-%data%
                                                  ((f2cl-lib:int-sub i4 1))
                                                  ((1 *))
                                                  z-%offset%)
                                     (- sigma))
                             (setf splt (the f2cl-lib:integer4 (truncate i4 4)))
                             (setf qmax zero)
                             (setf emin
                                     (f2cl-lib:fref z-%data%
                                                    ((f2cl-lib:int-add i4 3))
                                                    ((1 *))
                                                    z-%offset%))
                             (setf oldemn
                                     (f2cl-lib:fref z-%data%
                                                    ((f2cl-lib:int-add i4 4))
                                                    ((1 *))
                                                    z-%offset%)))
                            (t
                             (setf qmax
                                     (max qmax
                                          (f2cl-lib:fref z-%data%
                                                         ((f2cl-lib:int-add i4
                                                                            1))
                                                         ((1 *))
                                                         z-%offset%)))
                             (setf emin
                                     (min emin
                                          (f2cl-lib:fref z-%data%
                                                         ((f2cl-lib:int-sub i4
                                                                            1))
                                                         ((1 *))
                                                         z-%offset%)))
                             (setf oldemn
                                     (min oldemn
                                          (f2cl-lib:fref z-%data%
                                                         (i4)
                                                         ((1 *))
                                                         z-%offset%)))))
                         label110))
                      (setf (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-sub
                                             (f2cl-lib:int-mul 4 n0)
                                             1))
                                           ((1 *))
                                           z-%offset%)
                              emin)
                      (setf (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-mul 4 n0))
                                           ((1 *))
                                           z-%offset%)
                              oldemn)
                      (setf i0 (f2cl-lib:int-add splt 1))))))
               label120))
            (setf info 2)
            (go end_label)
           label130
           label140))
        (setf info 3)
        (go end_label)
       label150
        (f2cl-lib:fdo (k 2 (f2cl-lib:int-add k 1))
                      ((> k n) nil)
          (tagbody
            (setf (f2cl-lib:fref z-%data% (k) ((1 *)) z-%offset%)
                    (f2cl-lib:fref z-%data%
                                   ((f2cl-lib:int-sub (f2cl-lib:int-mul 4 k)
                                                      3))
                                   ((1 *))
                                   z-%offset%))
           label160))
        (multiple-value-bind (var-0 var-1 var-2 var-3)
            (dlasrt "D" n z iinfo)
          (declare (ignore var-0 var-1 var-2))
          (setf iinfo var-3))
        (setf e zero)
        (f2cl-lib:fdo (k n (f2cl-lib:int-add k (f2cl-lib:int-sub 1)))
                      ((> k 1) nil)
          (tagbody
            (setf e (+ e (f2cl-lib:fref z-%data% (k) ((1 *)) z-%offset%)))
           label170))
        (setf (f2cl-lib:fref z-%data%
                             ((f2cl-lib:int-add (f2cl-lib:int-mul 2 n) 1))
                             ((1 *))
                             z-%offset%)
                trace$)
        (setf (f2cl-lib:fref z-%data%
                             ((f2cl-lib:int-add (f2cl-lib:int-mul 2 n) 2))
                             ((1 *))
                             z-%offset%)
                e)
        (setf (f2cl-lib:fref z-%data%
                             ((f2cl-lib:int-add (f2cl-lib:int-mul 2 n) 3))
                             ((1 *))
                             z-%offset%)
                (f2cl-lib:dble iter))
        (setf (f2cl-lib:fref z-%data%
                             ((f2cl-lib:int-add (f2cl-lib:int-mul 2 n) 4))
                             ((1 *))
                             z-%offset%)
                (/ (f2cl-lib:dble ndiv) (f2cl-lib:dble (expt n 2))))
        (setf (f2cl-lib:fref z-%data%
                             ((f2cl-lib:int-add (f2cl-lib:int-mul 2 n) 5))
                             ((1 *))
                             z-%offset%)
                (/ (* hundrd nfail) (f2cl-lib:dble iter)))
        (go end_label)
       end_label
        (return (values nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasq2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlasq3 fortran-to-lisp::ilaenv
                    fortran-to-lisp::dlasrt fortran-to-lisp::xerbla
                    fortran-to-lisp::dlamch))))

