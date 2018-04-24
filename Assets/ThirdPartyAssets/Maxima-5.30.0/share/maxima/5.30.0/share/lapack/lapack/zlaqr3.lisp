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


(let* ((zero (f2cl-lib:cmplx 0.0 0.0))
       (one (f2cl-lib:cmplx 1.0 0.0))
       (rzero 0.0)
       (rone 1.0))
  (declare (type (f2cl-lib:complex16) zero)
           (type (f2cl-lib:complex16) one)
           (type (double-float 0.0 0.0) rzero)
           (type (double-float 1.0 1.0) rone)
           (ignorable zero one rzero rone))
  (defun zlaqr3
         (wantt wantz n ktop kbot nw h ldh iloz ihiz z ldz ns nd sh v ldv nh t$
          ldt nv wv ldwv work lwork)
    (declare (type (array f2cl-lib:complex16 (*)) work wv t$ v sh z h)
             (type (f2cl-lib:integer4) lwork ldwv nv ldt nh ldv nd ns ldz ihiz
                                       iloz ldh nw kbot ktop n)
             (type f2cl-lib:logical wantz wantt))
    (f2cl-lib:with-multi-array-data
        ((h f2cl-lib:complex16 h-%data% h-%offset%)
         (z f2cl-lib:complex16 z-%data% z-%offset%)
         (sh f2cl-lib:complex16 sh-%data% sh-%offset%)
         (v f2cl-lib:complex16 v-%data% v-%offset%)
         (t$ f2cl-lib:complex16 t$-%data% t$-%offset%)
         (wv f2cl-lib:complex16 wv-%data% wv-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%))
      (labels ((cabs1 (cdum)
                 (+ (abs (f2cl-lib:dble cdum)) (abs (f2cl-lib:dimag cdum)))))
        (declare (ftype (function (f2cl-lib:complex16)
                         (values double-float &rest t))
                        cabs1))
        (prog ((i 0) (ifst 0) (ilst 0) (info 0) (infqr 0) (j 0) (jw 0) (kcol 0)
               (kln 0) (knt 0) (krow 0) (kwtop 0) (ltop 0) (lwk1 0) (lwk2 0)
               (lwk3 0) (lwkopt 0) (nmin 0) (foo 0.0) (safmax 0.0) (safmin 0.0)
               (smlnum 0.0) (ulp 0.0) (beta #C(0.0 0.0)) (cdum #C(0.0 0.0))
               (s #C(0.0 0.0)) (tau #C(0.0 0.0)) (dconjg$ 0.0f0))
          (declare (type (single-float) dconjg$)
                   (type (f2cl-lib:integer4) i ifst ilst info infqr j jw kcol
                                             kln knt krow kwtop ltop lwk1 lwk2
                                             lwk3 lwkopt nmin)
                   (type (double-float) foo safmax safmin smlnum ulp)
                   (type (f2cl-lib:complex16) beta cdum s tau))
          (setf jw
                  (min (the f2cl-lib:integer4 nw)
                       (the f2cl-lib:integer4
                            (f2cl-lib:int-add (f2cl-lib:int-sub kbot ktop)
                                              1))))
          (cond
            ((<= jw 2)
             (setf lwkopt 1))
            (t
             (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                 (zgehrd jw 1 (f2cl-lib:int-sub jw 1) t$ ldt work work -1 info)
               (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                var-7))
               (setf info var-8))
             (setf lwk1
                     (f2cl-lib:int
                      (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)))
             (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                    var-10 var-11 var-12 var-13)
                 (zunmhr "R" "N" jw jw 1 (f2cl-lib:int-sub jw 1) t$ ldt work v
                  ldv work -1 info)
               (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                var-8 var-9 var-10 var-11 var-12))
               (setf info var-13))
             (setf lwk2
                     (f2cl-lib:int
                      (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)))
             (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                    var-10 var-11 var-12 var-13 var-14)
                 (zlaqr4 f2cl-lib:%true% f2cl-lib:%true% jw 1 jw t$ ldt sh 1 jw
                  v ldv work -1 infqr)
               (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                var-8 var-9 var-10 var-11 var-12 var-13))
               (setf infqr var-14))
             (setf lwk3
                     (f2cl-lib:int
                      (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)))
             (setf lwkopt
                     (max
                      (the f2cl-lib:integer4
                           (f2cl-lib:int-add jw
                                             (max (the f2cl-lib:integer4 lwk1)
                                                  (the f2cl-lib:integer4
                                                       lwk2))))
                      (the f2cl-lib:integer4 lwk3)))))
          (cond
            ((= lwork (f2cl-lib:int-sub 1))
             (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                     (f2cl-lib:dcmplx lwkopt 0))
             (go end_label)))
          (setf ns 0)
          (setf nd 0)
          (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%) one)
          (if (> ktop kbot) (go end_label))
          (if (< nw 1) (go end_label))
          (setf safmin (dlamch "SAFE MINIMUM"))
          (setf safmax (/ rone safmin))
          (multiple-value-bind (var-0 var-1)
              (dlabad safmin safmax)
            (declare (ignore))
            (setf safmin var-0)
            (setf safmax var-1))
          (setf ulp (dlamch "PRECISION"))
          (setf smlnum (* safmin (/ (f2cl-lib:dble n) ulp)))
          (setf jw
                  (min (the f2cl-lib:integer4 nw)
                       (the f2cl-lib:integer4
                            (f2cl-lib:int-add (f2cl-lib:int-sub kbot ktop)
                                              1))))
          (setf kwtop (f2cl-lib:int-add (f2cl-lib:int-sub kbot jw) 1))
          (cond
            ((= kwtop ktop)
             (setf s zero))
            (t
             (setf s
                     (f2cl-lib:fref h-%data%
                                    (kwtop (f2cl-lib:int-sub kwtop 1))
                                    ((1 ldh) (1 *))
                                    h-%offset%))))
          (cond
            ((= kbot kwtop)
             (setf (f2cl-lib:fref sh-%data% (kwtop) ((1 *)) sh-%offset%)
                     (f2cl-lib:fref h-%data%
                                    (kwtop kwtop)
                                    ((1 ldh) (1 *))
                                    h-%offset%))
             (setf ns 1)
             (setf nd 0)
             (cond
               ((<= (cabs1 s)
                    (max smlnum
                         (* ulp
                            (cabs1
                             (f2cl-lib:fref h (kwtop kwtop) ((1 ldh) (1 *)))))))
                (setf ns 0)
                (setf nd 1)
                (if (> kwtop ktop)
                    (setf (f2cl-lib:fref h-%data%
                                         (kwtop (f2cl-lib:int-sub kwtop 1))
                                         ((1 ldh) (1 *))
                                         h-%offset%)
                            zero))))
             (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%) one)
             (go end_label)))
          (zlacpy "U" jw jw
           (f2cl-lib:array-slice h-%data%
                                 f2cl-lib:complex16
                                 (kwtop kwtop)
                                 ((1 ldh) (1 *))
                                 h-%offset%)
           ldh t$ ldt)
          (zcopy (f2cl-lib:int-sub jw 1)
           (f2cl-lib:array-slice h-%data%
                                 f2cl-lib:complex16
                                 ((+ kwtop 1) kwtop)
                                 ((1 ldh) (1 *))
                                 h-%offset%)
           (f2cl-lib:int-add ldh 1)
           (f2cl-lib:array-slice t$-%data%
                                 f2cl-lib:complex16
                                 (2 1)
                                 ((1 ldt) (1 *))
                                 t$-%offset%)
           (f2cl-lib:int-add ldt 1))
          (zlaset "A" jw jw zero one v ldv)
          (setf nmin (ilaenv 12 "ZLAQR3" "SV" jw 1 jw lwork))
          (cond
            ((> jw nmin)
             (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                    var-10 var-11 var-12 var-13 var-14)
                 (zlaqr4 f2cl-lib:%true% f2cl-lib:%true% jw 1 jw t$ ldt
                  (f2cl-lib:array-slice sh-%data%
                                        f2cl-lib:complex16
                                        (kwtop)
                                        ((1 *))
                                        sh-%offset%)
                  1 jw v ldv work lwork infqr)
               (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                var-8 var-9 var-10 var-11 var-12 var-13))
               (setf infqr var-14)))
            (t
             (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                    var-10 var-11 var-12)
                 (zlahqr f2cl-lib:%true% f2cl-lib:%true% jw 1 jw t$ ldt
                  (f2cl-lib:array-slice sh-%data%
                                        f2cl-lib:complex16
                                        (kwtop)
                                        ((1 *))
                                        sh-%offset%)
                  1 jw v ldv infqr)
               (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                var-8 var-9 var-10 var-11))
               (setf infqr var-12))))
          (setf ns jw)
          (setf ilst (f2cl-lib:int-add infqr 1))
          (f2cl-lib:fdo (knt (f2cl-lib:int-add infqr 1)
                         (f2cl-lib:int-add knt 1))
                        ((> knt jw) nil)
            (tagbody
              (setf foo
                      (cabs1
                       (f2cl-lib:fref t$-%data%
                                      (ns ns)
                                      ((1 ldt) (1 *))
                                      t$-%offset%)))
              (if (= foo rzero) (setf foo (cabs1 s)))
              (cond
                ((<=
                  (* (cabs1 s)
                     (cabs1 (f2cl-lib:fref v (1 ns) ((1 ldv) (1 *)))))
                  (max smlnum (* ulp foo)))
                 (setf ns (f2cl-lib:int-sub ns 1)))
                (t
                 (setf ifst ns)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                     (ztrexc "V" jw t$ ldt v ldv ifst ilst info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7))
                   (setf info var-8))
                 (setf ilst (f2cl-lib:int-add ilst 1))))
             label10))
          (if (= ns 0) (setf s zero))
          (cond
            ((< ns jw)
             (f2cl-lib:fdo (i (f2cl-lib:int-add infqr 1) (f2cl-lib:int-add i 1))
                           ((> i ns) nil)
               (tagbody
                 (setf ifst i)
                 (f2cl-lib:fdo (j (f2cl-lib:int-add i 1) (f2cl-lib:int-add j 1))
                               ((> j ns) nil)
                   (tagbody
                     (if
                      (>
                       (cabs1
                        (f2cl-lib:fref t$-%data%
                                       (j j)
                                       ((1 ldt) (1 *))
                                       t$-%offset%))
                       (cabs1
                        (f2cl-lib:fref t$-%data%
                                       (ifst ifst)
                                       ((1 ldt) (1 *))
                                       t$-%offset%)))
                      (setf ifst j))
                    label20))
                 (setf ilst i)
                 (if (/= ifst ilst)
                     (multiple-value-bind
                           (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                            var-8)
                         (ztrexc "V" jw t$ ldt v ldv ifst ilst info)
                       (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                        var-6 var-7))
                       (setf info var-8)))
                label30))))
          (f2cl-lib:fdo (i (f2cl-lib:int-add infqr 1) (f2cl-lib:int-add i 1))
                        ((> i jw) nil)
            (tagbody
              (setf (f2cl-lib:fref sh-%data%
                                   ((f2cl-lib:int-sub
                                     (f2cl-lib:int-add kwtop i)
                                     1))
                                   ((1 *))
                                   sh-%offset%)
                      (f2cl-lib:fref t$-%data%
                                     (i i)
                                     ((1 ldt) (1 *))
                                     t$-%offset%))
             label40))
          (cond
            ((or (< ns jw) (= s zero))
             (cond
               ((and (> ns 1) (/= s zero))
                (zcopy ns v ldv work 1)
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i ns) nil)
                  (tagbody
                    (setf (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%)
                            (coerce
                             (f2cl-lib:dconjg
                              (f2cl-lib:fref work-%data%
                                             (i)
                                             ((1 *))
                                             work-%offset%))
                             'f2cl-lib:complex16))
                   label50))
                (setf beta
                        (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%))
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                    (zlarfg ns beta
                     (f2cl-lib:array-slice work-%data%
                                           f2cl-lib:complex16
                                           (2)
                                           ((1 *))
                                           work-%offset%)
                     1 tau)
                  (declare (ignore var-0 var-2 var-3))
                  (setf beta var-1)
                  (setf tau var-4))
                (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%) one)
                (zlaset "L" (f2cl-lib:int-sub jw 2) (f2cl-lib:int-sub jw 2)
                 zero zero
                 (f2cl-lib:array-slice t$-%data%
                                       f2cl-lib:complex16
                                       (3 1)
                                       ((1 ldt) (1 *))
                                       t$-%offset%)
                 ldt)
                (zlarf "L" ns jw work 1 (f2cl-lib:dconjg tau) t$ ldt
                 (f2cl-lib:array-slice work-%data%
                                       f2cl-lib:complex16
                                       ((+ jw 1))
                                       ((1 *))
                                       work-%offset%))
                (zlarf "R" ns ns work 1 tau t$ ldt
                 (f2cl-lib:array-slice work-%data%
                                       f2cl-lib:complex16
                                       ((+ jw 1))
                                       ((1 *))
                                       work-%offset%))
                (zlarf "R" jw ns work 1 tau v ldv
                 (f2cl-lib:array-slice work-%data%
                                       f2cl-lib:complex16
                                       ((+ jw 1))
                                       ((1 *))
                                       work-%offset%))
                (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                    (zgehrd jw 1 ns t$ ldt work
                     (f2cl-lib:array-slice work-%data%
                                           f2cl-lib:complex16
                                           ((+ jw 1))
                                           ((1 *))
                                           work-%offset%)
                     (f2cl-lib:int-sub lwork jw) info)
                  (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                   var-7))
                  (setf info var-8))))
             (if (> kwtop 1)
                 (setf (f2cl-lib:fref h-%data%
                                      (kwtop (f2cl-lib:int-sub kwtop 1))
                                      ((1 ldh) (1 *))
                                      h-%offset%)
                         (* s
                            (f2cl-lib:dconjg
                             (f2cl-lib:fref v-%data%
                                            (1 1)
                                            ((1 ldv) (1 *))
                                            v-%offset%)))))
             (zlacpy "U" jw jw t$ ldt
              (f2cl-lib:array-slice h-%data%
                                    f2cl-lib:complex16
                                    (kwtop kwtop)
                                    ((1 ldh) (1 *))
                                    h-%offset%)
              ldh)
             (zcopy (f2cl-lib:int-sub jw 1)
              (f2cl-lib:array-slice t$-%data%
                                    f2cl-lib:complex16
                                    (2 1)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
              (f2cl-lib:int-add ldt 1)
              (f2cl-lib:array-slice h-%data%
                                    f2cl-lib:complex16
                                    ((+ kwtop 1) kwtop)
                                    ((1 ldh) (1 *))
                                    h-%offset%)
              (f2cl-lib:int-add ldh 1))
             (if (and (> ns 1) (/= s zero))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (zunmhr "R" "N" jw ns 1 ns t$ ldt work v ldv
                      (f2cl-lib:array-slice work-%data%
                                            f2cl-lib:complex16
                                            ((+ jw 1))
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-sub lwork jw) info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13)))
             (cond
               (wantt
                (setf ltop 1))
               (t
                (setf ltop ktop)))
             (f2cl-lib:fdo (krow ltop (f2cl-lib:int-add krow nv))
                           ((> krow
                               (f2cl-lib:int-add kwtop (f2cl-lib:int-sub 1)))
                            nil)
               (tagbody
                 (setf kln
                         (min (the f2cl-lib:integer4 nv)
                              (the f2cl-lib:integer4
                                   (f2cl-lib:int-sub kwtop krow))))
                 (zgemm "N" "N" kln jw jw one
                  (f2cl-lib:array-slice h-%data%
                                        f2cl-lib:complex16
                                        (krow kwtop)
                                        ((1 ldh) (1 *))
                                        h-%offset%)
                  ldh v ldv zero wv ldwv)
                 (zlacpy "A" kln jw wv ldwv
                  (f2cl-lib:array-slice h-%data%
                                        f2cl-lib:complex16
                                        (krow kwtop)
                                        ((1 ldh) (1 *))
                                        h-%offset%)
                  ldh)
                label60))
             (cond
               (wantt
                (f2cl-lib:fdo (kcol (f2cl-lib:int-add kbot 1)
                               (f2cl-lib:int-add kcol nh))
                              ((> kcol n) nil)
                  (tagbody
                    (setf kln
                            (min (the f2cl-lib:integer4 nh)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add
                                       (f2cl-lib:int-sub n kcol)
                                       1))))
                    (zgemm "C" "N" jw kln jw one v ldv
                     (f2cl-lib:array-slice h-%data%
                                           f2cl-lib:complex16
                                           (kwtop kcol)
                                           ((1 ldh) (1 *))
                                           h-%offset%)
                     ldh zero t$ ldt)
                    (zlacpy "A" jw kln t$ ldt
                     (f2cl-lib:array-slice h-%data%
                                           f2cl-lib:complex16
                                           (kwtop kcol)
                                           ((1 ldh) (1 *))
                                           h-%offset%)
                     ldh)
                   label70))))
             (cond
               (wantz
                (f2cl-lib:fdo (krow iloz (f2cl-lib:int-add krow nv))
                              ((> krow ihiz) nil)
                  (tagbody
                    (setf kln
                            (min (the f2cl-lib:integer4 nv)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add
                                       (f2cl-lib:int-sub ihiz krow)
                                       1))))
                    (zgemm "N" "N" kln jw jw one
                     (f2cl-lib:array-slice z-%data%
                                           f2cl-lib:complex16
                                           (krow kwtop)
                                           ((1 ldz) (1 *))
                                           z-%offset%)
                     ldz v ldv zero wv ldwv)
                    (zlacpy "A" kln jw wv ldwv
                     (f2cl-lib:array-slice z-%data%
                                           f2cl-lib:complex16
                                           (krow kwtop)
                                           ((1 ldz) (1 *))
                                           z-%offset%)
                     ldz)
                   label80))))))
          (setf nd (f2cl-lib:int-sub jw ns))
          (setf ns (f2cl-lib:int-sub ns infqr))
          (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                  (f2cl-lib:dcmplx lwkopt 0))
         end_label
          (return
           (values nil
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
                   ns
                   nd
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
                   nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zlaqr3
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(fortran-to-lisp::logical fortran-to-lisp::logical
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::ns fortran-to-lisp::nd nil nil nil
                            nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::zgemm fortran-to-lisp::zlarf
                    fortran-to-lisp::zlarfg fortran-to-lisp::ztrexc
                    fortran-to-lisp::zlahqr fortran-to-lisp::ilaenv
                    fortran-to-lisp::zlaset fortran-to-lisp::zcopy
                    fortran-to-lisp::zlacpy fortran-to-lisp::dlabad
                    fortran-to-lisp::dlamch fortran-to-lisp::zlaqr4
                    fortran-to-lisp::zunmhr fortran-to-lisp::zgehrd))))

