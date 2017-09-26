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
       (qurtr 0.25)
       (half 0.5)
       (one 1.0)
       (two 2.0)
       (hundrd 100.0))
  (declare (type (double-float 1.5 1.5) cbias)
           (type (double-float 0.0 0.0) zero)
           (type (double-float 0.25 0.25) qurtr)
           (type (double-float 0.5 0.5) half)
           (type (double-float 1.0 1.0) one)
           (type (double-float 2.0 2.0) two)
           (type (double-float 100.0 100.0) hundrd)
           (ignorable cbias zero qurtr half one two hundrd))
  (let ((tau zero)
        (dn2 zero)
        (dn1 zero)
        (dn zero)
        (dmin2 zero)
        (dmin1$ zero)
        (ttype 0))
    (declare (type (double-float) tau dn2 dn1 dn dmin2 dmin1$)
             (type (f2cl-lib:integer4) ttype))
    (defun dlasq3 (i0 n0 z pp dmin sigma desig qmax nfail iter ndiv ieee)
      (declare (type f2cl-lib:logical ieee)
               (type (double-float) qmax desig sigma dmin)
               (type (array double-float (*)) z)
               (type (f2cl-lib:integer4) ndiv iter nfail pp n0 i0))
      (f2cl-lib:with-multi-array-data
          ((z double-float z-%data% z-%offset%))
        (prog ((eps 0.0) (s 0.0) (safmin 0.0) (t$ 0.0) (temp 0.0) (tol 0.0)
               (tol2 0.0) (ipn4 0) (j4 0) (n0in 0) (nn 0))
          (declare (type (double-float) eps s safmin t$ temp tol tol2)
                   (type (f2cl-lib:integer4) ipn4 j4 n0in nn))
          (setf n0in n0)
          (setf eps (dlamch "Precision"))
          (setf safmin (dlamch "Safe minimum"))
          (setf tol (* eps hundrd))
          (setf tol2 (expt tol 2))
         label10
          (if (< n0 i0) (go end_label))
          (if (= n0 i0) (go label20))
          (setf nn (f2cl-lib:int-add (f2cl-lib:int-mul 4 n0) pp))
          (if (= n0 (f2cl-lib:int-add i0 1)) (go label40))
          (if
           (and
            (>
             (f2cl-lib:fref z-%data%
                            ((f2cl-lib:int-sub nn 5))
                            ((1 *))
                            z-%offset%)
             (* tol2
                (+ sigma
                   (f2cl-lib:fref z-%data%
                                  ((f2cl-lib:int-sub nn 3))
                                  ((1 *))
                                  z-%offset%))))
            (>
             (f2cl-lib:fref z-%data%
                            ((f2cl-lib:int-sub
                              (f2cl-lib:int-add nn (f2cl-lib:int-mul -1 2 pp))
                              4))
                            ((1 *))
                            z-%offset%)
             (* tol2
                (f2cl-lib:fref z-%data%
                               ((f2cl-lib:int-sub nn 7))
                               ((1 *))
                               z-%offset%))))
           (go label30))
         label20
          (setf (f2cl-lib:fref z-%data%
                               ((f2cl-lib:int-sub (f2cl-lib:int-mul 4 n0) 3))
                               ((1 *))
                               z-%offset%)
                  (+
                   (f2cl-lib:fref z-%data%
                                  ((f2cl-lib:int-sub
                                    (f2cl-lib:int-add (f2cl-lib:int-mul 4 n0)
                                                      pp)
                                    3))
                                  ((1 *))
                                  z-%offset%)
                   sigma))
          (setf n0 (f2cl-lib:int-sub n0 1))
          (go label10)
         label30
          (if
           (and
            (>
             (f2cl-lib:fref z-%data%
                            ((f2cl-lib:int-sub nn 9))
                            ((1 *))
                            z-%offset%)
             (* tol2 sigma))
            (>
             (f2cl-lib:fref z-%data%
                            ((f2cl-lib:int-sub
                              (f2cl-lib:int-add nn (f2cl-lib:int-mul -1 2 pp))
                              8))
                            ((1 *))
                            z-%offset%)
             (* tol2
                (f2cl-lib:fref z-%data%
                               ((f2cl-lib:int-sub nn 11))
                               ((1 *))
                               z-%offset%))))
           (go label50))
         label40
          (cond
            ((>
              (f2cl-lib:fref z
                             ((f2cl-lib:int-add nn (f2cl-lib:int-sub 3)))
                             ((1 *)))
              (f2cl-lib:fref z
                             ((f2cl-lib:int-add nn (f2cl-lib:int-sub 7)))
                             ((1 *))))
             (setf s
                     (f2cl-lib:fref z-%data%
                                    ((f2cl-lib:int-sub nn 3))
                                    ((1 *))
                                    z-%offset%))
             (setf (f2cl-lib:fref z-%data%
                                  ((f2cl-lib:int-sub nn 3))
                                  ((1 *))
                                  z-%offset%)
                     (f2cl-lib:fref z-%data%
                                    ((f2cl-lib:int-sub nn 7))
                                    ((1 *))
                                    z-%offset%))
             (setf (f2cl-lib:fref z-%data%
                                  ((f2cl-lib:int-sub nn 7))
                                  ((1 *))
                                  z-%offset%)
                     s)))
          (cond
            ((>
              (f2cl-lib:fref z
                             ((f2cl-lib:int-add nn (f2cl-lib:int-sub 5)))
                             ((1 *)))
              (*
               (f2cl-lib:fref z
                              ((f2cl-lib:int-add nn (f2cl-lib:int-sub 3)))
                              ((1 *)))
               tol2))
             (setf t$
                     (* half
                        (+
                         (-
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub nn 7))
                                         ((1 *))
                                         z-%offset%)
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub nn 3))
                                         ((1 *))
                                         z-%offset%))
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub nn 5))
                                        ((1 *))
                                        z-%offset%))))
             (setf s
                     (*
                      (f2cl-lib:fref z-%data%
                                     ((f2cl-lib:int-sub nn 3))
                                     ((1 *))
                                     z-%offset%)
                      (/
                       (f2cl-lib:fref z-%data%
                                      ((f2cl-lib:int-sub nn 5))
                                      ((1 *))
                                      z-%offset%)
                       t$)))
             (cond
               ((<= s t$)
                (setf s
                        (*
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub nn 3))
                                        ((1 *))
                                        z-%offset%)
                         (/
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub nn 5))
                                         ((1 *))
                                         z-%offset%)
                          (* t$ (+ one (f2cl-lib:fsqrt (+ one (/ s t$)))))))))
               (t
                (setf s
                        (*
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub nn 3))
                                        ((1 *))
                                        z-%offset%)
                         (/
                          (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub nn 5))
                                         ((1 *))
                                         z-%offset%)
                          (+ t$
                             (* (f2cl-lib:fsqrt t$)
                                (f2cl-lib:fsqrt (+ t$ s)))))))))
             (setf t$
                     (+
                      (f2cl-lib:fref z-%data%
                                     ((f2cl-lib:int-sub nn 7))
                                     ((1 *))
                                     z-%offset%)
                      (+ s
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub nn 5))
                                        ((1 *))
                                        z-%offset%))))
             (setf (f2cl-lib:fref z-%data%
                                  ((f2cl-lib:int-sub nn 3))
                                  ((1 *))
                                  z-%offset%)
                     (*
                      (f2cl-lib:fref z-%data%
                                     ((f2cl-lib:int-sub nn 3))
                                     ((1 *))
                                     z-%offset%)
                      (/
                       (f2cl-lib:fref z-%data%
                                      ((f2cl-lib:int-sub nn 7))
                                      ((1 *))
                                      z-%offset%)
                       t$)))
             (setf (f2cl-lib:fref z-%data%
                                  ((f2cl-lib:int-sub nn 7))
                                  ((1 *))
                                  z-%offset%)
                     t$)))
          (setf (f2cl-lib:fref z-%data%
                               ((f2cl-lib:int-sub (f2cl-lib:int-mul 4 n0) 7))
                               ((1 *))
                               z-%offset%)
                  (+
                   (f2cl-lib:fref z-%data%
                                  ((f2cl-lib:int-sub nn 7))
                                  ((1 *))
                                  z-%offset%)
                   sigma))
          (setf (f2cl-lib:fref z-%data%
                               ((f2cl-lib:int-sub (f2cl-lib:int-mul 4 n0) 3))
                               ((1 *))
                               z-%offset%)
                  (+
                   (f2cl-lib:fref z-%data%
                                  ((f2cl-lib:int-sub nn 3))
                                  ((1 *))
                                  z-%offset%)
                   sigma))
          (setf n0 (f2cl-lib:int-sub n0 2))
          (go label10)
         label50
          (cond
            ((or (<= dmin zero) (< n0 n0in))
             (cond
               ((<
                 (* cbias
                    (f2cl-lib:fref z
                                   ((f2cl-lib:int-add (f2cl-lib:int-mul 4 i0)
                                                      pp
                                                      (f2cl-lib:int-sub 3)))
                                   ((1 *))))
                 (f2cl-lib:fref z
                                ((f2cl-lib:int-add (f2cl-lib:int-mul 4 n0)
                                                   pp
                                                   (f2cl-lib:int-sub 3)))
                                ((1 *))))
                (setf ipn4 (f2cl-lib:int-mul 4 (f2cl-lib:int-add i0 n0)))
                (f2cl-lib:fdo (j4 (f2cl-lib:int-mul 4 i0)
                               (f2cl-lib:int-add j4 4))
                              ((> j4
                                  (f2cl-lib:int-mul 2
                                                    (f2cl-lib:int-add i0
                                                                      n0
                                                                      (f2cl-lib:int-sub
                                                                       1))))
                               nil)
                  (tagbody
                    (setf temp
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-sub j4 3))
                                           ((1 *))
                                           z-%offset%))
                    (setf (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub j4 3))
                                         ((1 *))
                                         z-%offset%)
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-sub ipn4 j4 3))
                                           ((1 *))
                                           z-%offset%))
                    (setf (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub ipn4 j4 3))
                                         ((1 *))
                                         z-%offset%)
                            temp)
                    (setf temp
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-sub j4 2))
                                           ((1 *))
                                           z-%offset%))
                    (setf (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub j4 2))
                                         ((1 *))
                                         z-%offset%)
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-sub ipn4 j4 2))
                                           ((1 *))
                                           z-%offset%))
                    (setf (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub ipn4 j4 2))
                                         ((1 *))
                                         z-%offset%)
                            temp)
                    (setf temp
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-sub j4 1))
                                           ((1 *))
                                           z-%offset%))
                    (setf (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub j4 1))
                                         ((1 *))
                                         z-%offset%)
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-sub ipn4 j4 5))
                                           ((1 *))
                                           z-%offset%))
                    (setf (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub ipn4 j4 5))
                                         ((1 *))
                                         z-%offset%)
                            temp)
                    (setf temp
                            (f2cl-lib:fref z-%data% (j4) ((1 *)) z-%offset%))
                    (setf (f2cl-lib:fref z-%data% (j4) ((1 *)) z-%offset%)
                            (f2cl-lib:fref z-%data%
                                           ((f2cl-lib:int-sub ipn4 j4 4))
                                           ((1 *))
                                           z-%offset%))
                    (setf (f2cl-lib:fref z-%data%
                                         ((f2cl-lib:int-sub ipn4 j4 4))
                                         ((1 *))
                                         z-%offset%)
                            temp)
                   label60))
                (cond
                  ((<= (f2cl-lib:int-add n0 (f2cl-lib:int-sub i0)) 4)
                   (setf (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 4 n0)
                                           pp)
                                          1))
                                        ((1 *))
                                        z-%offset%)
                           (f2cl-lib:fref z-%data%
                                          ((f2cl-lib:int-sub
                                            (f2cl-lib:int-add
                                             (f2cl-lib:int-mul 4 i0)
                                             pp)
                                            1))
                                          ((1 *))
                                          z-%offset%))
                   (setf (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub
                                          (f2cl-lib:int-mul 4 n0)
                                          pp))
                                        ((1 *))
                                        z-%offset%)
                           (f2cl-lib:fref z-%data%
                                          ((f2cl-lib:int-sub
                                            (f2cl-lib:int-mul 4 i0)
                                            pp))
                                          ((1 *))
                                          z-%offset%))))
                (setf dmin2
                        (min dmin2
                             (f2cl-lib:fref z-%data%
                                            ((f2cl-lib:int-sub
                                              (f2cl-lib:int-add
                                               (f2cl-lib:int-mul 4 n0)
                                               pp)
                                              1))
                                            ((1 *))
                                            z-%offset%)))
                (setf (f2cl-lib:fref z-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 4 n0)
                                        pp)
                                       1))
                                     ((1 *))
                                     z-%offset%)
                        (min
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 4 n0)
                                           pp)
                                          1))
                                        ((1 *))
                                        z-%offset%)
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 4 i0)
                                           pp)
                                          1))
                                        ((1 *))
                                        z-%offset%)
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-add
                                          (f2cl-lib:int-mul 4 i0)
                                          pp
                                          3))
                                        ((1 *))
                                        z-%offset%)))
                (setf (f2cl-lib:fref z-%data%
                                     ((f2cl-lib:int-sub (f2cl-lib:int-mul 4 n0)
                                                        pp))
                                     ((1 *))
                                     z-%offset%)
                        (min
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub
                                          (f2cl-lib:int-mul 4 n0)
                                          pp))
                                        ((1 *))
                                        z-%offset%)
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-sub
                                          (f2cl-lib:int-mul 4 i0)
                                          pp))
                                        ((1 *))
                                        z-%offset%)
                         (f2cl-lib:fref z-%data%
                                        ((f2cl-lib:int-add
                                          (f2cl-lib:int-sub
                                           (f2cl-lib:int-mul 4 i0)
                                           pp)
                                          4))
                                        ((1 *))
                                        z-%offset%)))
                (setf qmax
                        (max qmax
                             (f2cl-lib:fref z-%data%
                                            ((f2cl-lib:int-sub
                                              (f2cl-lib:int-add
                                               (f2cl-lib:int-mul 4 i0)
                                               pp)
                                              3))
                                            ((1 *))
                                            z-%offset%)
                             (f2cl-lib:fref z-%data%
                                            ((f2cl-lib:int-add
                                              (f2cl-lib:int-mul 4 i0)
                                              pp
                                              1))
                                            ((1 *))
                                            z-%offset%)))
                (setf dmin (- zero))))))
         label70
          (cond
            ((or (< dmin zero)
                 (< (* safmin qmax)
                    (min
                     (f2cl-lib:fref z
                                    ((f2cl-lib:int-add (f2cl-lib:int-mul 4 n0)
                                                       pp
                                                       (f2cl-lib:int-sub 1)))
                                    ((1 *)))
                     (f2cl-lib:fref z
                                    ((f2cl-lib:int-add (f2cl-lib:int-mul 4 n0)
                                                       pp
                                                       (f2cl-lib:int-sub 9)))
                                    ((1 *)))
                     (+ dmin2
                        (f2cl-lib:fref z
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-mul 4 n0)
                                         (f2cl-lib:int-sub pp)))
                                       ((1 *)))))))
             (tagbody
               (multiple-value-bind
                     (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                      var-9 var-10 var-11 var-12)
                   (dlasq4 i0 n0 z pp n0in dmin dmin1$ dmin2 dn dn1 dn2 tau
                    ttype)
                 (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                  var-7 var-8 var-9 var-10))
                 (setf tau var-11)
                 (setf ttype var-12))
              label80
               (multiple-value-bind
                     (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                      var-9 var-10 var-11)
                   (dlasq5 i0 n0 z pp tau dmin dmin1$ dmin2 dn dn1 dn2 ieee)
                 (declare (ignore var-0 var-1 var-2 var-3 var-4 var-11))
                 (setf dmin var-5)
                 (setf dmin1$ var-6)
                 (setf dmin2 var-7)
                 (setf dn var-8)
                 (setf dn1 var-9)
                 (setf dn2 var-10))
               (setf ndiv
                       (f2cl-lib:int-add ndiv
                                         (f2cl-lib:int-add
                                          (f2cl-lib:int-sub n0 i0)
                                          2)))
               (setf iter (f2cl-lib:int-add iter 1))
               (cond
                 ((and (>= dmin zero) (> dmin1$ zero))
                  (go label100))
                 ((and (< dmin zero)
                       (> dmin1$ zero)
                       (<
                        (f2cl-lib:fref z
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-mul 4
                                                           (f2cl-lib:int-add n0
                                                                             (f2cl-lib:int-sub
                                                                              1)))
                                         (f2cl-lib:int-sub pp)))
                                       ((1 *)))
                        (* tol (+ sigma dn1)))
                       (< (abs dn) (* tol sigma)))
                  (setf (f2cl-lib:fref z-%data%
                                       ((f2cl-lib:int-add
                                         (f2cl-lib:int-sub
                                          (f2cl-lib:int-mul 4
                                                            (f2cl-lib:int-sub
                                                             n0
                                                             1))
                                          pp)
                                         2))
                                       ((1 *))
                                       z-%offset%)
                          zero)
                  (setf dmin zero)
                  (go label100))
                 ((< dmin zero)
                  (setf nfail (f2cl-lib:int-add nfail 1))
                  (cond
                    ((< ttype (f2cl-lib:int-sub 22))
                     (setf tau zero))
                    ((> dmin1$ zero)
                     (setf tau (* (+ tau dmin) (- one (* two eps))))
                     (setf ttype (f2cl-lib:int-sub ttype 11)))
                    (t
                     (setf tau (* qurtr tau))
                     (setf ttype (f2cl-lib:int-sub ttype 12))))
                  (go label80))
                 ((/= dmin dmin)
                  (setf tau zero)
                  (go label80))
                 (t
                  (go label90))))))
         label90
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (dlasq6 i0 n0 z pp dmin dmin1$ dmin2 dn dn1 dn2)
            (declare (ignore var-0 var-1 var-2 var-3))
            (setf dmin var-4)
            (setf dmin1$ var-5)
            (setf dmin2 var-6)
            (setf dn var-7)
            (setf dn1 var-8)
            (setf dn2 var-9))
          (setf ndiv
                  (f2cl-lib:int-add ndiv
                                    (f2cl-lib:int-add (f2cl-lib:int-sub n0 i0)
                                                      2)))
          (setf iter (f2cl-lib:int-add iter 1))
          (setf tau zero)
         label100
          (cond
            ((< tau sigma)
             (setf desig (+ desig tau))
             (setf t$ (+ sigma desig))
             (setf desig (- desig (- t$ sigma))))
            (t
             (setf t$ (+ sigma tau))
             (setf desig (+ (- sigma (- t$ tau)) desig))))
          (setf sigma t$)
          (go end_label)
         end_label
          (return
           (values nil
                   n0
                   nil
                   nil
                   dmin
                   sigma
                   desig
                   qmax
                   nfail
                   iter
                   ndiv
                   nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasq3
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (double-float) (double-float) (double-float)
                        (double-float) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        fortran-to-lisp::logical)
           :return-values '(nil fortran-to-lisp::n0 nil nil
                            fortran-to-lisp::dmin fortran-to-lisp::sigma
                            fortran-to-lisp::desig fortran-to-lisp::qmax
                            fortran-to-lisp::nfail fortran-to-lisp::iter
                            fortran-to-lisp::ndiv nil)
           :calls '(fortran-to-lisp::dlasq6 fortran-to-lisp::dlasq5
                    fortran-to-lisp::dlasq4 fortran-to-lisp::dlamch))))

