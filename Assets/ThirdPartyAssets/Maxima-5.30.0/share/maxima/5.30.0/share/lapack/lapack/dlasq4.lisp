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


(let* ((cnst1 0.563)
       (cnst2 1.01)
       (cnst3 1.05)
       (qurtr 0.25)
       (third$ 0.333)
       (half 0.5)
       (zero 0.0)
       (one 1.0)
       (two 2.0)
       (hundrd 100.0))
  (declare (type (double-float 0.563 0.563) cnst1)
           (type (double-float 1.01 1.01) cnst2)
           (type (double-float 1.05 1.05) cnst3)
           (type (double-float 0.25 0.25) qurtr)
           (type (double-float 0.333 0.333) third$)
           (type (double-float 0.5 0.5) half)
           (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (type (double-float 2.0 2.0) two)
           (type (double-float 100.0 100.0) hundrd)
           (ignorable cnst1 cnst2 cnst3 qurtr third$ half zero one two hundrd))
  (let ((g zero))
    (declare (type (double-float) g))
    (defun dlasq4 (i0 n0 z pp n0in dmin dmin1$ dmin2 dn dn1 dn2 tau ttype)
      (declare (type (double-float) tau dn2 dn1 dn dmin2 dmin1$ dmin)
               (type (array double-float (*)) z)
               (type (f2cl-lib:integer4) ttype n0in pp n0 i0))
      (f2cl-lib:with-multi-array-data
          ((z double-float z-%data% z-%offset%))
        (prog ((a2 0.0) (b1 0.0) (b2 0.0) (gam 0.0) (gap1 0.0) (gap2 0.0)
               (s 0.0) (i4 0) (nn 0) (np 0))
          (declare (type (double-float) a2 b1 b2 gam gap1 gap2 s)
                   (type (f2cl-lib:integer4) i4 nn np))
          (cond
            ((<= dmin zero)
             (setf tau (- dmin))
             (setf ttype -1)
             (go end_label)))
          (setf nn (f2cl-lib:int-add (f2cl-lib:int-mul 4 n0) pp))
          (cond
            ((= n0in n0)
             (cond
               ((or (= dmin dn) (= dmin dn1))
                (setf b1
                        (*
                         (f2cl-lib:fsqrt
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub nn 3))
                                         ((1 *))
                                         z-%offset%))
                         (f2cl-lib:fsqrt
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub nn 5))
                                         ((1 *))
                                         z-%offset%))))
                (setf b2
                        (*
                         (f2cl-lib:fsqrt
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub nn 7))
                                         ((1 *))
                                         z-%offset%))
                         (f2cl-lib:fsqrt
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub nn 9))
                                         ((1 *))
                                         z-%offset%))))
                (setf a2
                        (+
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub nn 7))
                                        ((1 *))
                                        z-%offset%)
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub nn 5))
                                        ((1 *))
                                        z-%offset%)))
                (cond
                  ((and (= dmin dn) (= dmin1$ dn1))
                   (setf gap2 (- dmin2 a2 (* dmin2 qurtr)))
                   (cond
                     ((and (> gap2 zero) (> gap2 b2))
                      (setf gap1 (- a2 dn (* (/ b2 gap2) b2))))
                     (t
                      (setf gap1 (- a2 dn (+ b1 b2)))))
                   (cond
                     ((and (> gap1 zero) (> gap1 b1))
                      (setf s (max (- dn (* (/ b1 gap1) b1)) (* half dmin)))
                      (setf ttype -2))
                     (t
                      (setf s zero)
                      (if (> dn b1) (setf s (- dn b1)))
                      (if (> a2 (+ b1 b2)) (setf s (min s (- a2 (+ b1 b2)))))
                      (setf s (max s (* third$ dmin)))
                      (setf ttype -3))))
                  (t
                   (tagbody
                     (setf ttype -4)
                     (setf s (* qurtr dmin))
                     (cond
                       ((= dmin dn)
                        (setf gam dn)
                        (setf a2 zero)
                        (if
                         (>
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub nn 5))
                                         ((1 *))
                                         z-%offset%)
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub nn 7))
                                         ((1 *))
                                         z-%offset%))
                         (go end_label))
                        (setf b2
                                (/
                                 (f2cl-lib:fref z-%data%
                                                ((f2cl-lib:int-sub nn 5))
                                                ((1 *))
                                                z-%offset%)
                                 (f2cl-lib:fref z-%data%
                                                ((f2cl-lib:int-sub nn 7))
                                                ((1 *))
                                                z-%offset%)))
                        (setf np (f2cl-lib:int-sub nn 9)))
                       (t
                        (setf np (f2cl-lib:int-sub nn (f2cl-lib:int-mul 2 pp)))
                        (setf b2
                                (f2cl-lib:fref z-%data%
                                               ((f2cl-lib:int-sub np 2))
                                               ((1 *))
                                               z-%offset%))
                        (setf gam dn1)
                        (if
                         (>
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub np 4))
                                         ((1 *))
                                         z-%offset%)
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub np 2))
                                         ((1 *))
                                         z-%offset%))
                         (go end_label))
                        (setf a2
                                (/
                                 (f2cl-lib:fref z-%data%
                                                ((f2cl-lib:int-sub np 4))
                                                ((1 *))
                                                z-%offset%)
                                 (f2cl-lib:fref z-%data%
                                                ((f2cl-lib:int-sub np 2))
                                                ((1 *))
                                                z-%offset%)))
                        (if
                         (>
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub nn 9))
                                         ((1 *))
                                         z-%offset%)
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub nn 11))
                                         ((1 *))
                                         z-%offset%))
                         (go end_label))
                        (setf b2
                                (/
                                 (f2cl-lib:fref z-%data%
                                                ((f2cl-lib:int-sub nn 9))
                                                ((1 *))
                                                z-%offset%)
                                 (f2cl-lib:fref z-%data%
                                                ((f2cl-lib:int-sub nn 11))
                                                ((1 *))
                                                z-%offset%)))
                        (setf np (f2cl-lib:int-sub nn 13))))
                     (setf a2 (+ a2 b2))
                     (f2cl-lib:fdo (i4 np
                                    (f2cl-lib:int-add i4 (f2cl-lib:int-sub 4)))
                                   ((> i4
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 4 i0)
                                        (f2cl-lib:int-sub 1)
                                        pp))
                                    nil)
                       (tagbody
                         (if (= b2 zero) (go label20))
                         (setf b1 b2)
                         (if
                          (> (f2cl-lib:fref z-%data% (i4) ((1 *)) z-%offset%)
                             (f2cl-lib:fref z-%data%
                                            ((f2cl-lib:int-sub i4 2))
                                            ((1 *))
                                            z-%offset%))
                          (go end_label))
                         (setf b2
                                 (* b2
                                    (/
                                     (f2cl-lib:fref z-%data%
                                                    (i4)
                                                    ((1 *))
                                                    z-%offset%)
                                     (f2cl-lib:fref z-%data%
                                                    ((f2cl-lib:int-sub i4 2))
                                                    ((1 *))
                                                    z-%offset%))))
                         (setf a2 (+ a2 b2))
                         (if (or (< (* hundrd (max b2 b1)) a2) (< cnst1 a2))
                             (go label20))
                        label10))
                    label20
                     (setf a2 (* cnst3 a2))
                     (if (< a2 cnst1)
                         (setf s
                                 (/ (* gam (- one (f2cl-lib:fsqrt a2)))
                                    (+ one a2))))))))
               ((= dmin dn2)
                (setf ttype -5)
                (setf s (* qurtr dmin))
                (setf np (f2cl-lib:int-sub nn (f2cl-lib:int-mul 2 pp)))
                (setf b1
                        (f2cl-lib:fref z-%data%
                                       ((f2cl-lib:int-sub np 2))
                                       ((1 *))
                                       z-%offset%))
                (setf b2
                        (f2cl-lib:fref z-%data%
                                       ((f2cl-lib:int-sub np 6))
                                       ((1 *))
                                       z-%offset%))
                (setf gam dn2)
                (if
                 (or
                  (>
                   (f2cl-lib:fref z-%data%
                                  ((f2cl-lib:int-sub np 8))
                                  ((1 *))
                                  z-%offset%)
                   b2)
                  (>
                   (f2cl-lib:fref z-%data%
                                  ((f2cl-lib:int-sub np 4))
                                  ((1 *))
                                  z-%offset%)
                   b1))
                 (go end_label))
                (setf a2
                        (*
                         (/
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub np 8))
                                         ((1 *))
                                         z-%offset%)
                          b2)
                         (+ one
                            (/
                             (f2cl-lib:fref z-%data%
                                            ((f2cl-lib:int-sub np 4))
                                            ((1 *))
                                            z-%offset%)
                             b1))))
                (cond
                  ((> (f2cl-lib:int-add n0 (f2cl-lib:int-sub i0)) 2)
                   (tagbody
                     (setf b2
                             (/
                              (f2cl-lib:fref z-%data%
                                             ((f2cl-lib:int-sub nn 13))
                                             ((1 *))
                                             z-%offset%)
                              (f2cl-lib:fref z-%data%
                                             ((f2cl-lib:int-sub nn 15))
                                             ((1 *))
                                             z-%offset%)))
                     (setf a2 (+ a2 b2))
                     (f2cl-lib:fdo (i4
                                    (f2cl-lib:int-add nn (f2cl-lib:int-sub 17))
                                    (f2cl-lib:int-add i4 (f2cl-lib:int-sub 4)))
                                   ((> i4
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 4 i0)
                                        (f2cl-lib:int-sub 1)
                                        pp))
                                    nil)
                       (tagbody
                         (if (= b2 zero) (go label40))
                         (setf b1 b2)
                         (if
                          (> (f2cl-lib:fref z-%data% (i4) ((1 *)) z-%offset%)
                             (f2cl-lib:fref z-%data%
                                            ((f2cl-lib:int-sub i4 2))
                                            ((1 *))
                                            z-%offset%))
                          (go end_label))
                         (setf b2
                                 (* b2
                                    (/
                                     (f2cl-lib:fref z-%data%
                                                    (i4)
                                                    ((1 *))
                                                    z-%offset%)
                                     (f2cl-lib:fref z-%data%
                                                    ((f2cl-lib:int-sub i4 2))
                                                    ((1 *))
                                                    z-%offset%))))
                         (setf a2 (+ a2 b2))
                         (if (or (< (* hundrd (max b2 b1)) a2) (< cnst1 a2))
                             (go label40))
                        label30))
                    label40
                     (setf a2 (* cnst3 a2)))))
                (if (< a2 cnst1)
                    (setf s
                            (/ (* gam (- one (f2cl-lib:fsqrt a2)))
                               (+ one a2)))))
               (t
                (cond
                  ((= ttype (f2cl-lib:int-sub 6))
                   (setf g (+ g (* third$ (- one g)))))
                  ((= ttype (f2cl-lib:int-sub 18))
                   (setf g (* qurtr third$)))
                  (t
                   (setf g qurtr)))
                (setf s (* g dmin))
                (setf ttype -6))))
            ((= n0in (f2cl-lib:int-add n0 1))
             (cond
               ((and (= dmin1$ dn1) (= dmin2 dn2))
                (tagbody
                  (setf ttype -7)
                  (setf s (* third$ dmin1$))
                  (if
                   (>
                    (f2cl-lib:fref z-%data%
                                   ((f2cl-lib:int-sub nn 5))
                                   ((1 *))
                                   z-%offset%)
                    (f2cl-lib:fref z-%data%
                                   ((f2cl-lib:int-sub nn 7))
                                   ((1 *))
                                   z-%offset%))
                   (go end_label))
                  (setf b1
                          (/
                           (f2cl-lib:fref z-%data%
                                          ((f2cl-lib:int-sub nn 5))
                                          ((1 *))
                                          z-%offset%)
                           (f2cl-lib:fref z-%data%
                                          ((f2cl-lib:int-sub nn 7))
                                          ((1 *))
                                          z-%offset%)))
                  (setf b2 b1)
                  (if (= b2 zero) (go label60))
                  (f2cl-lib:fdo (i4
                                 (f2cl-lib:int-add (f2cl-lib:int-mul 4 n0)
                                                   (f2cl-lib:int-sub 9)
                                                   pp)
                                 (f2cl-lib:int-add i4 (f2cl-lib:int-sub 4)))
                                ((> i4
                                    (f2cl-lib:int-add (f2cl-lib:int-mul 4 i0)
                                                      (f2cl-lib:int-sub 1)
                                                      pp))
                                 nil)
                    (tagbody
                      (setf a2 b1)
                      (if
                       (> (f2cl-lib:fref z-%data% (i4) ((1 *)) z-%offset%)
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub i4 2))
                                         ((1 *))
                                         z-%offset%))
                       (go end_label))
                      (setf b1
                              (* b1
                                 (/
                                  (f2cl-lib:fref z-%data%
                                                 (i4)
                                                 ((1 *))
                                                 z-%offset%)
                                  (f2cl-lib:fref z-%data%
                                                 ((f2cl-lib:int-sub i4 2))
                                                 ((1 *))
                                                 z-%offset%))))
                      (setf b2 (+ b2 b1))
                      (if (< (* hundrd (max b1 a2)) b2) (go label60))
                     label50))
                 label60
                  (setf b2 (f2cl-lib:fsqrt (* cnst3 b2)))
                  (setf a2 (/ dmin1$ (+ one (expt b2 2))))
                  (setf gap2 (- (* half dmin2) a2))
                  (cond
                    ((and (> gap2 zero) (> gap2 (* b2 a2)))
                     (setf s
                             (max s
                                  (* a2
                                     (+ one (* (- cnst2) a2 (/ b2 gap2) b2))))))
                    (t
                     (setf s (max s (* a2 (- one (* cnst2 b2)))))
                     (setf ttype -8)))))
               (t
                (setf s (* qurtr dmin1$))
                (if (= dmin1$ dn1) (setf s (* half dmin1$)))
                (setf ttype -9))))
            ((= n0in (f2cl-lib:int-add n0 2))
             (cond
               ((and (= dmin2 dn2)
                     (<
                      (* two
                         (f2cl-lib:fref z
                                        ((f2cl-lib:int-add nn
                                                           (f2cl-lib:int-sub
                                                            5)))
                                        ((1 *))))
                      (f2cl-lib:fref z
                                     ((f2cl-lib:int-add nn
                                                        (f2cl-lib:int-sub 7)))
                                     ((1 *)))))
                (tagbody
                  (setf ttype -10)
                  (setf s (* third$ dmin2))
                  (if
                   (>
                    (f2cl-lib:fref z-%data%
                                   ((f2cl-lib:int-sub nn 5))
                                   ((1 *))
                                   z-%offset%)
                    (f2cl-lib:fref z-%data%
                                   ((f2cl-lib:int-sub nn 7))
                                   ((1 *))
                                   z-%offset%))
                   (go end_label))
                  (setf b1
                          (/
                           (f2cl-lib:fref z-%data%
                                          ((f2cl-lib:int-sub nn 5))
                                          ((1 *))
                                          z-%offset%)
                           (f2cl-lib:fref z-%data%
                                          ((f2cl-lib:int-sub nn 7))
                                          ((1 *))
                                          z-%offset%)))
                  (setf b2 b1)
                  (if (= b2 zero) (go label80))
                  (f2cl-lib:fdo (i4
                                 (f2cl-lib:int-add (f2cl-lib:int-mul 4 n0)
                                                   (f2cl-lib:int-sub 9)
                                                   pp)
                                 (f2cl-lib:int-add i4 (f2cl-lib:int-sub 4)))
                                ((> i4
                                    (f2cl-lib:int-add (f2cl-lib:int-mul 4 i0)
                                                      (f2cl-lib:int-sub 1)
                                                      pp))
                                 nil)
                    (tagbody
                      (if
                       (> (f2cl-lib:fref z-%data% (i4) ((1 *)) z-%offset%)
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub i4 2))
                                         ((1 *))
                                         z-%offset%))
                       (go end_label))
                      (setf b1
                              (* b1
                                 (/
                                  (f2cl-lib:fref z-%data%
                                                 (i4)
                                                 ((1 *))
                                                 z-%offset%)
                                  (f2cl-lib:fref z-%data%
                                                 ((f2cl-lib:int-sub i4 2))
                                                 ((1 *))
                                                 z-%offset%))))
                      (setf b2 (+ b2 b1))
                      (if (< (* hundrd b1) b2) (go label80))
                     label70))
                 label80
                  (setf b2 (f2cl-lib:fsqrt (* cnst3 b2)))
                  (setf a2 (/ dmin2 (+ one (expt b2 2))))
                  (setf gap2
                          (-
                           (+
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-sub nn 7))
                                           ((1 *))
                                           z-%offset%)
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-sub nn 9))
                                           ((1 *))
                                           z-%offset%))
                           (*
                            (f2cl-lib:fsqrt
                             (f2cl-lib:fref z-%data%
                                            ((f2cl-lib:int-sub nn 11))
                                            ((1 *))
                                            z-%offset%))
                            (f2cl-lib:fsqrt
                             (f2cl-lib:fref z-%data%
                                            ((f2cl-lib:int-sub nn 9))
                                            ((1 *))
                                            z-%offset%)))
                           a2))
                  (cond
                    ((and (> gap2 zero) (> gap2 (* b2 a2)))
                     (setf s
                             (max s
                                  (* a2
                                     (+ one (* (- cnst2) a2 (/ b2 gap2) b2))))))
                    (t
                     (setf s (max s (* a2 (- one (* cnst2 b2)))))))))
               (t
                (setf s (* qurtr dmin2))
                (setf ttype -11))))
            ((> n0in (f2cl-lib:int-add n0 2))
             (setf s zero)
             (setf ttype -12)))
          (setf tau s)
          (go end_label)
         end_label
          (return
           (values nil nil nil nil nil nil nil nil nil nil nil tau ttype)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasq4
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::tau fortran-to-lisp::ttype)
           :calls 'nil)))

