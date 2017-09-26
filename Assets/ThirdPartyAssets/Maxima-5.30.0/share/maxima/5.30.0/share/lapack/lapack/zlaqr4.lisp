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


(let* ((ntiny 11)
       (kexnw 5)
       (kexsh 6)
       (wilk1 0.75)
       (zero (f2cl-lib:cmplx 0.0 0.0))
       (one (f2cl-lib:cmplx 1.0 0.0))
       (two 2.0))
  (declare (type (f2cl-lib:integer4 11 11) ntiny)
           (type (f2cl-lib:integer4 5 5) kexnw)
           (type (f2cl-lib:integer4 6 6) kexsh)
           (type (double-float 0.75 0.75) wilk1)
           (type (f2cl-lib:complex16) zero)
           (type (f2cl-lib:complex16) one)
           (type (double-float 2.0 2.0) two)
           (ignorable ntiny kexnw kexsh wilk1 zero one two))
  (defun zlaqr4 (wantt wantz n ilo ihi h ldh w iloz ihiz z ldz work lwork info)
    (declare (type (array f2cl-lib:complex16 (*)) work z w h)
             (type (f2cl-lib:integer4) info lwork ldz ihiz iloz ldh ihi ilo n)
             (type f2cl-lib:logical wantz wantt))
    (f2cl-lib:with-multi-array-data
        ((h f2cl-lib:complex16 h-%data% h-%offset%)
         (w f2cl-lib:complex16 w-%data% w-%offset%)
         (z f2cl-lib:complex16 z-%data% z-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%))
      (labels ((cabs1 (cdum)
                 (+ (abs (f2cl-lib:dble cdum)) (abs (f2cl-lib:dimag cdum)))))
        (declare (ftype (function (f2cl-lib:complex16)
                         (values double-float &rest t))
                        cabs1))
        (prog ((zdum (make-array 1 :element-type 'f2cl-lib:complex16))
               (jbcmpz
                (make-array '(2)
                            :element-type 'character
                            :initial-element #\ ))
               (sorted nil) (i 0) (inf 0) (it 0) (itmax 0) (k 0) (kacc22 0)
               (kbot 0) (kdu 0) (ks 0) (kt 0) (ktop 0) (ku 0) (kv 0) (kwh 0)
               (kwtop 0) (kwv 0) (ld 0) (ls 0) (lwkopt 0) (ndec 0) (ndfl 0)
               (nh 0) (nho 0) (nibble 0) (nmin 0) (ns 0) (nsmax 0) (nsr 0)
               (nve 0) (nw 0) (nwmax 0) (nwr 0) (nwupbd 0) (s 0.0)
               (aa #C(0.0 0.0)) (bb #C(0.0 0.0)) (cc #C(0.0 0.0))
               (cdum #C(0.0 0.0)) (dd #C(0.0 0.0)) (det #C(0.0 0.0))
               (rtdisc #C(0.0 0.0)) (swap #C(0.0 0.0)) (tr2 #C(0.0 0.0)))
          (declare (type (array f2cl-lib:complex16 (1)) zdum)
                   (type (simple-string 2) jbcmpz)
                   (type f2cl-lib:logical sorted)
                   (type (f2cl-lib:integer4) i inf it itmax k kacc22 kbot kdu
                                             ks kt ktop ku kv kwh kwtop kwv ld
                                             ls lwkopt ndec ndfl nh nho nibble
                                             nmin ns nsmax nsr nve nw nwmax nwr
                                             nwupbd)
                   (type (double-float) s)
                   (type (f2cl-lib:complex16) aa bb cc cdum dd det rtdisc swap
                                              tr2))
          (setf info 0)
          (cond
            ((= n 0)
             (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%) one)
             (go end_label)))
          (cond
            ((<= n ntiny)
             (setf lwkopt 1)
             (if (/= lwork -1)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12)
                     (zlahqr wantt wantz n ilo ihi h ldh w iloz ihiz z ldz
                      info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11))
                   (setf info var-12))))
            (t
             (tagbody
               (setf info 0)
               (cond
                 (wantt
                  (f2cl-lib:fset-string (f2cl-lib:fref-string jbcmpz (1 1))
                                        "S"))
                 (t
                  (f2cl-lib:fset-string (f2cl-lib:fref-string jbcmpz (1 1))
                                        "E")))
               (cond
                 (wantz
                  (f2cl-lib:fset-string (f2cl-lib:fref-string jbcmpz (2 2))
                                        "V"))
                 (t
                  (f2cl-lib:fset-string (f2cl-lib:fref-string jbcmpz (2 2))
                                        "N")))
               (setf nwr (ilaenv 13 "ZLAQR4" jbcmpz n ilo ihi lwork))
               (setf nwr
                       (max (the f2cl-lib:integer4 2)
                            (the f2cl-lib:integer4 nwr)))
               (setf nwr
                       (min (f2cl-lib:int-add (f2cl-lib:int-sub ihi ilo) 1)
                            (the f2cl-lib:integer4 (truncate (- n 1) 3))
                            nwr))
               (setf nsr (ilaenv 15 "ZLAQR4" jbcmpz n ilo ihi lwork))
               (setf nsr
                       (min nsr
                            (the f2cl-lib:integer4 (truncate (+ n 6) 9))
                            (f2cl-lib:int-sub ihi ilo)))
               (setf nsr
                       (max (the f2cl-lib:integer4 2)
                            (the f2cl-lib:integer4
                                 (f2cl-lib:int-sub nsr (mod nsr 2)))))
               (multiple-value-bind
                     (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                      var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16
                      var-17 var-18 var-19 var-20 var-21 var-22 var-23 var-24)
                   (zlaqr2 wantt wantz n ilo ihi (f2cl-lib:int-add nwr 1) h ldh
                    iloz ihiz z ldz ls ld w h ldh n h ldh n h ldh work -1)
                 (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                  var-7 var-8 var-9 var-10 var-11 var-14 var-15
                                  var-16 var-17 var-18 var-19 var-20 var-21
                                  var-22 var-23 var-24))
                 (setf ls var-12)
                 (setf ld var-13))
               (setf lwkopt
                       (max (the f2cl-lib:integer4 (truncate (* 3 nsr) 2))
                            (f2cl-lib:int
                             (f2cl-lib:fref work-%data%
                                            (1)
                                            ((1 *))
                                            work-%offset%))))
               (cond
                 ((= lwork (f2cl-lib:int-sub 1))
                  (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                          (f2cl-lib:dcmplx lwkopt 0))
                  (go end_label)))
               (setf nmin (ilaenv 12 "ZLAQR4" jbcmpz n ilo ihi lwork))
               (setf nmin
                       (max (the f2cl-lib:integer4 ntiny)
                            (the f2cl-lib:integer4 nmin)))
               (setf nibble (ilaenv 14 "ZLAQR4" jbcmpz n ilo ihi lwork))
               (setf nibble
                       (max (the f2cl-lib:integer4 0)
                            (the f2cl-lib:integer4 nibble)))
               (setf kacc22 (ilaenv 16 "ZLAQR4" jbcmpz n ilo ihi lwork))
               (setf kacc22
                       (max (the f2cl-lib:integer4 0)
                            (the f2cl-lib:integer4 kacc22)))
               (setf kacc22
                       (min (the f2cl-lib:integer4 2)
                            (the f2cl-lib:integer4 kacc22)))
               (setf nwmax
                       (min (the f2cl-lib:integer4 (truncate (- n 1) 3))
                            (the f2cl-lib:integer4 (truncate lwork 2))))
               (setf nw nwmax)
               (setf nsmax
                       (min (the f2cl-lib:integer4 (truncate (+ n 6) 9))
                            (the f2cl-lib:integer4 (truncate (* 2 lwork) 3))))
               (setf nsmax (f2cl-lib:int-sub nsmax (mod nsmax 2)))
               (setf ndfl 1)
               (setf itmax
                       (f2cl-lib:int-mul
                        (max (the f2cl-lib:integer4 30)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-mul 2 kexsh)))
                        (max (the f2cl-lib:integer4 10)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-sub ihi ilo)
                                                    1)))))
               (setf kbot ihi)
               (f2cl-lib:fdo (it 1 (f2cl-lib:int-add it 1))
                             ((> it itmax) nil)
                 (tagbody
                   (if (< kbot ilo) (go label80))
                   (f2cl-lib:fdo (k kbot
                                  (f2cl-lib:int-add k (f2cl-lib:int-sub 1)))
                                 ((> k (f2cl-lib:int-add ilo 1)) nil)
                     (tagbody
                       (if
                        (=
                         (f2cl-lib:fref h-%data%
                                        (k (f2cl-lib:int-sub k 1))
                                        ((1 ldh) (1 *))
                                        h-%offset%)
                         zero)
                        (go label20))
                      label10))
                   (setf k ilo)
                  label20
                   (setf ktop k)
                   (setf nh (f2cl-lib:int-add (f2cl-lib:int-sub kbot ktop) 1))
                   (setf nwupbd
                           (min (the f2cl-lib:integer4 nh)
                                (the f2cl-lib:integer4 nwmax)))
                   (cond
                     ((< ndfl kexnw)
                      (setf nw
                              (min (the f2cl-lib:integer4 nwupbd)
                                   (the f2cl-lib:integer4 nwr))))
                     (t
                      (setf nw
                              (min (the f2cl-lib:integer4 nwupbd)
                                   (the f2cl-lib:integer4
                                        (f2cl-lib:int-mul 2 nw))))))
                   (cond
                     ((< nw nwmax)
                      (cond
                        ((>= nw (f2cl-lib:int-add nh (f2cl-lib:int-sub 1)))
                         (setf nw nh))
                        (t
                         (setf kwtop
                                 (f2cl-lib:int-add (f2cl-lib:int-sub kbot nw)
                                                   1))
                         (if
                          (>
                           (cabs1
                            (f2cl-lib:fref h-%data%
                                           (kwtop (f2cl-lib:int-sub kwtop 1))
                                           ((1 ldh) (1 *))
                                           h-%offset%))
                           (cabs1
                            (f2cl-lib:fref h-%data%
                                           ((f2cl-lib:int-sub kwtop 1)
                                            (f2cl-lib:int-sub kwtop 2))
                                           ((1 ldh) (1 *))
                                           h-%offset%)))
                          (setf nw (f2cl-lib:int-add nw 1)))))))
                   (cond
                     ((< ndfl kexnw)
                      (setf ndec -1))
                     ((or (>= ndec 0) (>= nw nwupbd))
                      (setf ndec (f2cl-lib:int-add ndec 1))
                      (if (< (f2cl-lib:int-sub nw ndec) 2) (setf ndec 0))
                      (setf nw (f2cl-lib:int-sub nw ndec))))
                   (setf kv (f2cl-lib:int-add (f2cl-lib:int-sub n nw) 1))
                   (setf kt (f2cl-lib:int-add nw 1))
                   (setf nho (f2cl-lib:int-add (f2cl-lib:int-sub n nw 1 kt) 1))
                   (setf kwv (f2cl-lib:int-add nw 2))
                   (setf nve (f2cl-lib:int-add (f2cl-lib:int-sub n nw kwv) 1))
                   (multiple-value-bind
                         (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                          var-9 var-10 var-11 var-12 var-13 var-14 var-15
                          var-16 var-17 var-18 var-19 var-20 var-21 var-22
                          var-23 var-24)
                       (zlaqr2 wantt wantz n ktop kbot nw h ldh iloz ihiz z ldz
                        ls ld w
                        (f2cl-lib:array-slice h-%data%
                                              f2cl-lib:complex16
                                              (kv 1)
                                              ((1 ldh) (1 *))
                                              h-%offset%)
                        ldh nho
                        (f2cl-lib:array-slice h-%data%
                                              f2cl-lib:complex16
                                              (kv kt)
                                              ((1 ldh) (1 *))
                                              h-%offset%)
                        ldh nve
                        (f2cl-lib:array-slice h-%data%
                                              f2cl-lib:complex16
                                              (kwv 1)
                                              ((1 ldh) (1 *))
                                              h-%offset%)
                        ldh work lwork)
                     (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                      var-7 var-8 var-9 var-10 var-11 var-14
                                      var-15 var-16 var-17 var-18 var-19 var-20
                                      var-21 var-22 var-23 var-24))
                     (setf ls var-12)
                     (setf ld var-13))
                   (setf kbot (f2cl-lib:int-sub kbot ld))
                   (setf ks (f2cl-lib:int-add (f2cl-lib:int-sub kbot ls) 1))
                   (cond
                     ((or (= ld 0)
                          (and
                           (<= (f2cl-lib:int-mul 100 ld)
                               (f2cl-lib:int-mul nw nibble))
                           (> (f2cl-lib:int-add kbot (f2cl-lib:int-sub ktop) 1)
                              (min (the f2cl-lib:integer4 nmin)
                                   (the f2cl-lib:integer4 nwmax)))))
                      (setf ns
                              (min (the f2cl-lib:integer4 nsmax)
                                   (the f2cl-lib:integer4 nsr)
                                   (the f2cl-lib:integer4
                                        (max (the f2cl-lib:integer4 2)
                                             (the f2cl-lib:integer4
                                                  (f2cl-lib:int-sub kbot
                                                                    ktop))))))
                      (setf ns (f2cl-lib:int-sub ns (mod ns 2)))
                      (cond
                        ((= (mod ndfl kexsh) 0)
                         (setf ks
                                 (f2cl-lib:int-add (f2cl-lib:int-sub kbot ns)
                                                   1))
                         (f2cl-lib:fdo (i kbot
                                        (f2cl-lib:int-add i
                                                          (f2cl-lib:int-sub 2)))
                                       ((> i (f2cl-lib:int-add ks 1)) nil)
                           (tagbody
                             (setf (f2cl-lib:fref w-%data%
                                                  (i)
                                                  ((1 *))
                                                  w-%offset%)
                                     (+
                                      (f2cl-lib:fref h-%data%
                                                     (i i)
                                                     ((1 ldh) (1 *))
                                                     h-%offset%)
                                      (* wilk1
                                         (cabs1
                                          (f2cl-lib:fref h-%data%
                                                         (i
                                                          (f2cl-lib:int-sub i
                                                                            1))
                                                         ((1 ldh) (1 *))
                                                         h-%offset%)))))
                             (setf (f2cl-lib:fref w-%data%
                                                  ((f2cl-lib:int-sub i 1))
                                                  ((1 *))
                                                  w-%offset%)
                                     (f2cl-lib:fref w-%data%
                                                    (i)
                                                    ((1 *))
                                                    w-%offset%))
                            label30)))
                        (t
                         (cond
                           ((<= (f2cl-lib:int-add kbot (f2cl-lib:int-sub ks) 1)
                                (f2cl-lib:f2cl/ ns 2))
                            (setf ks
                                    (f2cl-lib:int-add
                                     (f2cl-lib:int-sub kbot ns)
                                     1))
                            (setf kt
                                    (f2cl-lib:int-add (f2cl-lib:int-sub n ns)
                                                      1))
                            (zlacpy "A" ns ns
                             (f2cl-lib:array-slice h-%data%
                                                   f2cl-lib:complex16
                                                   (ks ks)
                                                   ((1 ldh) (1 *))
                                                   h-%offset%)
                             ldh
                             (f2cl-lib:array-slice h-%data%
                                                   f2cl-lib:complex16
                                                   (kt 1)
                                                   ((1 ldh) (1 *))
                                                   h-%offset%)
                             ldh)
                            (multiple-value-bind
                                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                   var-7 var-8 var-9 var-10 var-11 var-12)
                                (zlahqr f2cl-lib:%false% f2cl-lib:%false% ns 1
                                 ns
                                 (f2cl-lib:array-slice h-%data%
                                                       f2cl-lib:complex16
                                                       (kt 1)
                                                       ((1 ldh) (1 *))
                                                       h-%offset%)
                                 ldh
                                 (f2cl-lib:array-slice w-%data%
                                                       f2cl-lib:complex16
                                                       (ks)
                                                       ((1 *))
                                                       w-%offset%)
                                 1 1 zdum 1 inf)
                              (declare (ignore var-0 var-1 var-2 var-3 var-4
                                               var-5 var-6 var-7 var-8 var-9
                                               var-10 var-11))
                              (setf inf var-12))
                            (setf ks (f2cl-lib:int-add ks inf))
                            (cond
                              ((>= ks kbot)
                               (setf s
                                       (+
                                        (cabs1
                                         (f2cl-lib:fref h-%data%
                                                        ((f2cl-lib:int-sub kbot
                                                                           1)
                                                         (f2cl-lib:int-sub kbot
                                                                           1))
                                                        ((1 ldh) (1 *))
                                                        h-%offset%))
                                        (cabs1
                                         (f2cl-lib:fref h-%data%
                                                        (kbot
                                                         (f2cl-lib:int-sub kbot
                                                                           1))
                                                        ((1 ldh) (1 *))
                                                        h-%offset%))
                                        (cabs1
                                         (f2cl-lib:fref h-%data%
                                                        ((f2cl-lib:int-sub kbot
                                                                           1)
                                                         kbot)
                                                        ((1 ldh) (1 *))
                                                        h-%offset%))
                                        (cabs1
                                         (f2cl-lib:fref h-%data%
                                                        (kbot kbot)
                                                        ((1 ldh) (1 *))
                                                        h-%offset%))))
                               (setf aa
                                       (/
                                        (f2cl-lib:fref h-%data%
                                                       ((f2cl-lib:int-sub kbot
                                                                          1)
                                                        (f2cl-lib:int-sub kbot
                                                                          1))
                                                       ((1 ldh) (1 *))
                                                       h-%offset%)
                                        s))
                               (setf cc
                                       (/
                                        (f2cl-lib:fref h-%data%
                                                       (kbot
                                                        (f2cl-lib:int-sub kbot
                                                                          1))
                                                       ((1 ldh) (1 *))
                                                       h-%offset%)
                                        s))
                               (setf bb
                                       (/
                                        (f2cl-lib:fref h-%data%
                                                       ((f2cl-lib:int-sub kbot
                                                                          1)
                                                        kbot)
                                                       ((1 ldh) (1 *))
                                                       h-%offset%)
                                        s))
                               (setf dd
                                       (/
                                        (f2cl-lib:fref h-%data%
                                                       (kbot kbot)
                                                       ((1 ldh) (1 *))
                                                       h-%offset%)
                                        s))
                               (setf tr2 (/ (+ aa dd) two))
                               (setf det
                                       (- (* (- aa tr2) (- dd tr2)) (* bb cc)))
                               (setf rtdisc (f2cl-lib:fsqrt (- det)))
                               (setf (f2cl-lib:fref w-%data%
                                                    ((f2cl-lib:int-sub kbot 1))
                                                    ((1 *))
                                                    w-%offset%)
                                       (* (+ tr2 rtdisc) s))
                               (setf (f2cl-lib:fref w-%data%
                                                    (kbot)
                                                    ((1 *))
                                                    w-%offset%)
                                       (* (- tr2 rtdisc) s))
                               (setf ks (f2cl-lib:int-sub kbot 1))))))
                         (cond
                           ((> (f2cl-lib:int-add kbot (f2cl-lib:int-sub ks) 1)
                               ns)
                            (tagbody
                              (setf sorted f2cl-lib:%false%)
                              (f2cl-lib:fdo (k kbot
                                             (f2cl-lib:int-add k
                                                               (f2cl-lib:int-sub
                                                                1)))
                                            ((> k (f2cl-lib:int-add ks 1)) nil)
                                (tagbody
                                  (if sorted (go label60))
                                  (setf sorted f2cl-lib:%true%)
                                  (f2cl-lib:fdo (i ks (f2cl-lib:int-add i 1))
                                                ((> i
                                                    (f2cl-lib:int-add k
                                                                      (f2cl-lib:int-sub
                                                                       1)))
                                                 nil)
                                    (tagbody
                                      (cond
                                        ((<
                                          (cabs1 (f2cl-lib:fref w (i) ((1 *))))
                                          (cabs1
                                           (f2cl-lib:fref w
                                                          ((f2cl-lib:int-add i
                                                                             1))
                                                          ((1 *)))))
                                         (setf sorted f2cl-lib:%false%)
                                         (setf swap
                                                 (f2cl-lib:fref w-%data%
                                                                (i)
                                                                ((1 *))
                                                                w-%offset%))
                                         (setf (f2cl-lib:fref w-%data%
                                                              (i)
                                                              ((1 *))
                                                              w-%offset%)
                                                 (f2cl-lib:fref w-%data%
                                                                ((f2cl-lib:int-add
                                                                  i
                                                                  1))
                                                                ((1 *))
                                                                w-%offset%))
                                         (setf (f2cl-lib:fref w-%data%
                                                              ((f2cl-lib:int-add
                                                                i
                                                                1))
                                                              ((1 *))
                                                              w-%offset%)
                                                 swap)))
                                     label40))
                                 label50))
                             label60)))))
                      (cond
                        ((= (f2cl-lib:int-add kbot (f2cl-lib:int-sub ks) 1) 2)
                         (cond
                           ((<
                             (cabs1
                              (+ (f2cl-lib:fref w (kbot) ((1 *)))
                                 (-
                                  (f2cl-lib:fref h
                                                 (kbot kbot)
                                                 ((1 ldh) (1 *))))))
                             (cabs1
                              (+
                               (f2cl-lib:fref w
                                              ((f2cl-lib:int-add kbot
                                                                 (f2cl-lib:int-sub
                                                                  1)))
                                              ((1 *)))
                               (-
                                (f2cl-lib:fref h
                                               (kbot kbot)
                                               ((1 ldh) (1 *)))))))
                            (setf (f2cl-lib:fref w-%data%
                                                 ((f2cl-lib:int-sub kbot 1))
                                                 ((1 *))
                                                 w-%offset%)
                                    (f2cl-lib:fref w-%data%
                                                   (kbot)
                                                   ((1 *))
                                                   w-%offset%)))
                           (t
                            (setf (f2cl-lib:fref w-%data%
                                                 (kbot)
                                                 ((1 *))
                                                 w-%offset%)
                                    (f2cl-lib:fref w-%data%
                                                   ((f2cl-lib:int-sub kbot 1))
                                                   ((1 *))
                                                   w-%offset%))))))
                      (setf ns
                              (min (the f2cl-lib:integer4 ns)
                                   (the f2cl-lib:integer4
                                        (f2cl-lib:int-add
                                         (f2cl-lib:int-sub kbot ks)
                                         1))))
                      (setf ns (f2cl-lib:int-sub ns (mod ns 2)))
                      (setf ks (f2cl-lib:int-add (f2cl-lib:int-sub kbot ns) 1))
                      (setf kdu (f2cl-lib:int-sub (f2cl-lib:int-mul 3 ns) 3))
                      (setf ku (f2cl-lib:int-add (f2cl-lib:int-sub n kdu) 1))
                      (setf kwh (f2cl-lib:int-add kdu 1))
                      (setf nho
                              (f2cl-lib:int-add
                               (f2cl-lib:int-sub
                                (f2cl-lib:int-add (f2cl-lib:int-sub n kdu) 1)
                                4
                                (f2cl-lib:int-add kdu 1))
                               1))
                      (setf kwv (f2cl-lib:int-add kdu 4))
                      (setf nve
                              (f2cl-lib:int-add (f2cl-lib:int-sub n kdu kwv) 1))
                      (zlaqr5 wantt wantz kacc22 n ktop kbot ns
                       (f2cl-lib:array-slice w-%data%
                                             f2cl-lib:complex16
                                             (ks)
                                             ((1 *))
                                             w-%offset%)
                       h ldh iloz ihiz z ldz work 3
                       (f2cl-lib:array-slice h-%data%
                                             f2cl-lib:complex16
                                             (ku 1)
                                             ((1 ldh) (1 *))
                                             h-%offset%)
                       ldh nve
                       (f2cl-lib:array-slice h-%data%
                                             f2cl-lib:complex16
                                             (kwv 1)
                                             ((1 ldh) (1 *))
                                             h-%offset%)
                       ldh nho
                       (f2cl-lib:array-slice h-%data%
                                             f2cl-lib:complex16
                                             (ku kwh)
                                             ((1 ldh) (1 *))
                                             h-%offset%)
                       ldh)))
                   (cond
                     ((> ld 0)
                      (setf ndfl 1))
                     (t
                      (setf ndfl (f2cl-lib:int-add ndfl 1))))
                  label70))
               (setf info kbot)
              label80)))
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
                   nil
                   nil
                   info)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zlaqr4
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(fortran-to-lisp::logical fortran-to-lisp::logical
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::zlaqr5 fortran-to-lisp::zlacpy
                    fortran-to-lisp::zlaqr2 fortran-to-lisp::ilaenv
                    fortran-to-lisp::zlahqr))))

