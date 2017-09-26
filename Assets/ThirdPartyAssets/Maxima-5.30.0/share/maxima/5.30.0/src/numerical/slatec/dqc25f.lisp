;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 46c1f6a93b0d 2012/05/03 04:40:28 toy $"
;;;  "f2cl2.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 46c1f6a93b0d 2012/05/03 04:40:28 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v fceac530ef0c 2011/11/26 04:02:26 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2012-04 (20C Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((x
       (make-array 11
                   :element-type 'double-float
                   :initial-contents '(0.9914448613738104 0.9659258262890683
                                       0.9238795325112867 0.8660254037844386
                                       0.7933533402912352 0.7071067811865476
                                       0.6087614290087207 0.5
                                       0.3826834323650898 0.25881904510252074
                                       0.1305261922200516))))
  (declare (type (array double-float (11)) x))
  (defun dqc25f
         (f a b omega integr nrmom maxp1 ksave result abserr neval resabs
          resasc momcom chebmo)
    (declare (type (array double-float (*)) chebmo)
             (type (f2cl-lib:integer4) momcom neval ksave maxp1 nrmom integr)
             (type (double-float) resasc resabs abserr result omega b a))
    (f2cl-lib:with-multi-array-data
        ((chebmo double-float chebmo-%data% chebmo-%offset%))
      (prog ((cheb12 (make-array 13 :element-type 'double-float))
             (cheb24 (make-array 25 :element-type 'double-float))
             (d (make-array 25 :element-type 'double-float))
             (d1 (make-array 25 :element-type 'double-float))
             (d2 (make-array 25 :element-type 'double-float))
             (fval (make-array 25 :element-type 'double-float))
             (v (make-array 28 :element-type 'double-float)) (i 0) (iers 0)
             (isym 0) (j 0) (k 0) (m 0) (noequ 0) (noeq1 0) (ac 0.0) (an 0.0)
             (an2 0.0) (as 0.0) (asap 0.0) (ass 0.0) (centr 0.0) (conc 0.0)
             (cons$ 0.0) (cospar 0.0) (estc 0.0) (ests 0.0) (hlgth 0.0)
             (oflow 0.0) (parint 0.0) (par2 0.0) (par22 0.0) (p2 0.0) (p3 0.0)
             (p4 0.0) (resc12 0.0) (resc24 0.0) (ress12 0.0) (ress24 0.0)
             (sinpar 0.0))
        (declare (type (array double-float (28)) v)
                 (type (array double-float (25)) fval d2 d1 d cheb24)
                 (type (array double-float (13)) cheb12)
                 (type (double-float) sinpar ress24 ress12 resc24 resc12 p4 p3
                                      p2 par22 par2 parint oflow hlgth ests
                                      estc cospar cons$ conc centr ass asap as
                                      an2 an ac)
                 (type (f2cl-lib:integer4) noeq1 noequ m k j isym iers i))
        (setf oflow (f2cl-lib:d1mach 2))
        (setf centr (* 0.5 (+ b a)))
        (setf hlgth (* 0.5 (- b a)))
        (setf parint (* omega hlgth))
        (if (> (abs parint) 2.0) (go label10))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11 var-12)
            (dqk15w f #'dqwgtf omega p2 p3 p4 integr a b result abserr resabs
             resasc)
          (declare (ignore var-0 var-1 var-7 var-8))
          (setf omega var-2)
          (setf p2 var-3)
          (setf p3 var-4)
          (setf p4 var-5)
          (setf integr var-6)
          (setf result var-9)
          (setf abserr var-10)
          (setf resabs var-11)
          (setf resasc var-12))
        (setf neval 15)
        (go label170)
       label10
        (setf conc (* hlgth (cos (* centr omega))))
        (setf cons$ (* hlgth (sin (* centr omega))))
        (setf resasc oflow)
        (setf neval 25)
        (if (or (< nrmom momcom) (= ksave 1)) (go label120))
        (setf m (f2cl-lib:int-add momcom 1))
        (setf par2 (* parint parint))
        (setf par22 (+ par2 2.0))
        (setf sinpar (sin parint))
        (setf cospar (cos parint))
        (setf (f2cl-lib:fref v (1) ((1 28))) (/ (* 2.0 sinpar) parint))
        (setf (f2cl-lib:fref v (2) ((1 28)))
                (/
                 (+ (* 8.0 cospar) (/ (* (- (+ par2 par2) 8.0) sinpar) parint))
                 par2))
        (setf (f2cl-lib:fref v (3) ((1 28)))
                (/
                 (+ (* 32.0 (- par2 12.0) cospar)
                    (/ (* 2.0 (+ (* (- par2 80.0) par2) 192.0) sinpar) parint))
                 (* par2 par2)))
        (setf ac (* 8.0 cospar))
        (setf as (* 24.0 parint sinpar))
        (if (> (abs parint) 24.0) (go label30))
        (setf noequ 25)
        (setf noeq1 (f2cl-lib:int-sub noequ 1))
        (setf an 6.0)
        (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                      ((> k noeq1) nil)
          (tagbody
            (setf an2 (* an an))
            (setf (f2cl-lib:fref d (k) ((1 25)))
                    (* -2.0 (- an2 4.0) (- par22 an2 an2)))
            (setf (f2cl-lib:fref d2 (k) ((1 25)))
                    (* (- an 1.0) (- an 2.0) par2))
            (setf (f2cl-lib:fref d1 ((f2cl-lib:int-add k 1)) ((1 25)))
                    (* (+ an 3.0) (+ an 4.0) par2))
            (setf (f2cl-lib:fref v ((f2cl-lib:int-add k 3)) ((1 28)))
                    (- as (* (- an2 4.0) ac)))
            (setf an (+ an 2.0))
           label20))
        (setf an2 (* an an))
        (setf (f2cl-lib:fref d (noequ) ((1 25)))
                (* -2.0 (- an2 4.0) (- par22 an2 an2)))
        (setf (f2cl-lib:fref v ((f2cl-lib:int-add noequ 3)) ((1 28)))
                (- as (* (- an2 4.0) ac)))
        (setf (f2cl-lib:fref v (4) ((1 28)))
                (+ (f2cl-lib:fref v (4) ((1 28)))
                   (* -56.0 par2 (f2cl-lib:fref v (3) ((1 28))))))
        (setf ass (* parint sinpar))
        (setf asap
                (/
                 (-
                  (/
                   (+
                    (-
                     (/
                      (+
                       (-
                        (/
                         (- (* (- (* 210.0 par2) 1.0) cospar)
                            (* (- (* 105.0 par2) 63.0) ass))
                         an2)
                        (* (+ 1.0 (* -1 15.0 par2)) cospar))
                       (* 15.0 ass))
                      an2)
                     cospar)
                    (* 3.0 ass))
                   an2)
                  cospar)
                 an2))
        (setf (f2cl-lib:fref v ((f2cl-lib:int-add noequ 3)) ((1 28)))
                (+ (f2cl-lib:fref v ((f2cl-lib:int-add noequ 3)) ((1 28)))
                   (* -2.0 asap par2 (- an 1.0) (- an 2.0))))
        (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
            (dgtsl noequ d1 d d2
             (f2cl-lib:array-slice v double-float (4) ((1 28))) iers)
          (declare (ignore var-0 var-1 var-2 var-3 var-4))
          (setf iers var-5))
        (go label50)
       label30
        (setf an 4.0)
        (f2cl-lib:fdo (i 4 (f2cl-lib:int-add i 1))
                      ((> i 13) nil)
          (tagbody
            (setf an2 (* an an))
            (setf (f2cl-lib:fref v (i) ((1 28)))
                    (/
                     (+
                      (* (- an2 4.0)
                         (-
                          (* 2.0
                             (- par22 an2 an2)
                             (f2cl-lib:fref v
                                            ((f2cl-lib:int-sub i 1))
                                            ((1 28))))
                          ac))
                      as
                      (* (- par2)
                         (+ an 1.0)
                         (+ an 2.0)
                         (f2cl-lib:fref v ((f2cl-lib:int-sub i 2)) ((1 28)))))
                     (* par2 (- an 1.0) (- an 2.0))))
            (setf an (+ an 2.0))
           label40))
       label50
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j 13) nil)
          (tagbody
            (setf (f2cl-lib:fref chebmo-%data%
                                 (m
                                  (f2cl-lib:int-sub (f2cl-lib:int-mul 2 j) 1))
                                 ((1 maxp1) (1 25))
                                 chebmo-%offset%)
                    (f2cl-lib:fref v (j) ((1 28))))
           label60))
        (setf (f2cl-lib:fref v (1) ((1 28)))
                (/ (* 2.0 (- sinpar (* parint cospar))) par2))
        (setf (f2cl-lib:fref v (2) ((1 28)))
                (+ (/ (* (+ 18.0 (/ -48.0 par2)) sinpar) par2)
                   (/ (* (- (/ 48.0 par2) 2.0) cospar) parint)))
        (setf ac (* -24.0 parint cospar))
        (setf as (* -8.0 sinpar))
        (if (> (abs parint) 24.0) (go label80))
        (setf an 5.0)
        (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                      ((> k noeq1) nil)
          (tagbody
            (setf an2 (* an an))
            (setf (f2cl-lib:fref d (k) ((1 25)))
                    (* -2.0 (- an2 4.0) (- par22 an2 an2)))
            (setf (f2cl-lib:fref d2 (k) ((1 25)))
                    (* (- an 1.0) (- an 2.0) par2))
            (setf (f2cl-lib:fref d1 ((f2cl-lib:int-add k 1)) ((1 25)))
                    (* (+ an 3.0) (+ an 4.0) par2))
            (setf (f2cl-lib:fref v ((f2cl-lib:int-add k 2)) ((1 28)))
                    (+ ac (* (- an2 4.0) as)))
            (setf an (+ an 2.0))
           label70))
        (setf an2 (* an an))
        (setf (f2cl-lib:fref d (noequ) ((1 25)))
                (* -2.0 (- an2 4.0) (- par22 an2 an2)))
        (setf (f2cl-lib:fref v ((f2cl-lib:int-add noequ 2)) ((1 28)))
                (+ ac (* (- an2 4.0) as)))
        (setf (f2cl-lib:fref v (3) ((1 28)))
                (+ (f2cl-lib:fref v (3) ((1 28)))
                   (* -42.0 par2 (f2cl-lib:fref v (2) ((1 28))))))
        (setf ass (* parint cospar))
        (setf asap
                (/
                 (-
                  (/
                   (-
                    (/
                     (-
                      (+
                       (/
                        (+ (* (- (* 105.0 par2) 63.0) ass)
                           (* (- (* 210.0 par2) 1.0) sinpar))
                        an2)
                       (* (- (* 15.0 par2) 1.0) sinpar))
                      (* 15.0 ass))
                     an2)
                    (* 3.0 ass)
                    sinpar)
                   an2)
                  sinpar)
                 an2))
        (setf (f2cl-lib:fref v ((f2cl-lib:int-add noequ 2)) ((1 28)))
                (+ (f2cl-lib:fref v ((f2cl-lib:int-add noequ 2)) ((1 28)))
                   (* -2.0 asap par2 (- an 1.0) (- an 2.0))))
        (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
            (dgtsl noequ d1 d d2
             (f2cl-lib:array-slice v double-float (3) ((1 28))) iers)
          (declare (ignore var-0 var-1 var-2 var-3 var-4))
          (setf iers var-5))
        (go label100)
       label80
        (setf an 3.0)
        (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 1))
                      ((> i 12) nil)
          (tagbody
            (setf an2 (* an an))
            (setf (f2cl-lib:fref v (i) ((1 28)))
                    (/
                     (+
                      (* (- an2 4.0)
                         (+
                          (* 2.0
                             (- par22 an2 an2)
                             (f2cl-lib:fref v
                                            ((f2cl-lib:int-sub i 1))
                                            ((1 28))))
                          as))
                      ac
                      (* (- par2)
                         (+ an 1.0)
                         (+ an 2.0)
                         (f2cl-lib:fref v ((f2cl-lib:int-sub i 2)) ((1 28)))))
                     (* par2 (- an 1.0) (- an 2.0))))
            (setf an (+ an 2.0))
           label90))
       label100
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j 12) nil)
          (tagbody
            (setf (f2cl-lib:fref chebmo-%data%
                                 (m (f2cl-lib:int-mul 2 j))
                                 ((1 maxp1) (1 25))
                                 chebmo-%offset%)
                    (f2cl-lib:fref v (j) ((1 28))))
           label110))
       label120
        (if (< nrmom momcom) (setf m (f2cl-lib:int-add nrmom 1)))
        (if (and (< momcom (f2cl-lib:int-sub maxp1 1)) (>= nrmom momcom))
            (setf momcom (f2cl-lib:int-add momcom 1)))
        (setf (f2cl-lib:fref fval (1) ((1 25)))
                (* 0.5 (funcall f (+ centr hlgth))))
        (setf (f2cl-lib:fref fval (13) ((1 25)))
                (multiple-value-bind (ret-val var-0)
                    (funcall f centr)
                  (declare (ignore))
                  (when var-0
                    (setf centr var-0))
                  ret-val))
        (setf (f2cl-lib:fref fval (25) ((1 25)))
                (* 0.5 (funcall f (- centr hlgth))))
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i 12) nil)
          (tagbody
            (setf isym (f2cl-lib:int-sub 26 i))
            (setf (f2cl-lib:fref fval (i) ((1 25)))
                    (funcall f
                             (+
                              (* hlgth
                                 (f2cl-lib:fref x
                                                ((f2cl-lib:int-sub i 1))
                                                ((1 11))))
                              centr)))
            (setf (f2cl-lib:fref fval (isym) ((1 25)))
                    (funcall f
                             (- centr
                                (* hlgth
                                   (f2cl-lib:fref x
                                                  ((f2cl-lib:int-sub i 1))
                                                  ((1 11)))))))
           label130))
        (dqcheb x fval cheb12 cheb24)
        (setf resc12
                (* (f2cl-lib:fref cheb12 (13) ((1 13)))
                   (f2cl-lib:fref chebmo-%data%
                                  (m 13)
                                  ((1 maxp1) (1 25))
                                  chebmo-%offset%)))
        (setf ress12 0.0)
        (setf k 11)
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j 6) nil)
          (tagbody
            (setf resc12
                    (+ resc12
                       (* (f2cl-lib:fref cheb12 (k) ((1 13)))
                          (f2cl-lib:fref chebmo-%data%
                                         (m k)
                                         ((1 maxp1) (1 25))
                                         chebmo-%offset%))))
            (setf ress12
                    (+ ress12
                       (*
                        (f2cl-lib:fref cheb12
                                       ((f2cl-lib:int-add k 1))
                                       ((1 13)))
                        (f2cl-lib:fref chebmo-%data%
                                       (m (f2cl-lib:int-add k 1))
                                       ((1 maxp1) (1 25))
                                       chebmo-%offset%))))
            (setf k (f2cl-lib:int-sub k 2))
           label140))
        (setf resc24
                (* (f2cl-lib:fref cheb24 (25) ((1 25)))
                   (f2cl-lib:fref chebmo-%data%
                                  (m 25)
                                  ((1 maxp1) (1 25))
                                  chebmo-%offset%)))
        (setf ress24 0.0)
        (setf resabs (abs (f2cl-lib:fref cheb24 (25) ((1 25)))))
        (setf k 23)
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j 12) nil)
          (tagbody
            (setf resc24
                    (+ resc24
                       (* (f2cl-lib:fref cheb24 (k) ((1 25)))
                          (f2cl-lib:fref chebmo-%data%
                                         (m k)
                                         ((1 maxp1) (1 25))
                                         chebmo-%offset%))))
            (setf ress24
                    (+ ress24
                       (*
                        (f2cl-lib:fref cheb24
                                       ((f2cl-lib:int-add k 1))
                                       ((1 25)))
                        (f2cl-lib:fref chebmo-%data%
                                       (m (f2cl-lib:int-add k 1))
                                       ((1 maxp1) (1 25))
                                       chebmo-%offset%))))
            (setf resabs
                    (+ resabs
                       (abs (f2cl-lib:fref cheb24 (k) ((1 25))))
                       (abs
                        (f2cl-lib:fref cheb24
                                       ((f2cl-lib:int-add k 1))
                                       ((1 25))))))
            (setf k (f2cl-lib:int-sub k 2))
           label150))
        (setf estc (abs (- resc24 resc12)))
        (setf ests (abs (- ress24 ress12)))
        (setf resabs (* resabs (abs hlgth)))
        (if (= integr 2) (go label160))
        (setf result (- (* conc resc24) (* cons$ ress24)))
        (setf abserr (+ (abs (* conc estc)) (abs (* cons$ ests))))
        (go label170)
       label160
        (setf result (+ (* conc ress24) (* cons$ resc24)))
        (setf abserr (+ (abs (* conc ests)) (abs (* cons$ estc))))
       label170
        (go end_label)
       end_label
        (return
         (values nil
                 nil
                 nil
                 omega
                 integr
                 nil
                 nil
                 nil
                 result
                 abserr
                 neval
                 resabs
                 resasc
                 momcom
                 nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dqc25f
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (double-float) (double-float)
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (fortran-to-lisp::integer4)
                        (array double-float (*)))
           :return-values '(nil nil nil fortran-to-lisp::omega
                            fortran-to-lisp::integr nil nil nil
                            fortran-to-lisp::result fortran-to-lisp::abserr
                            fortran-to-lisp::neval fortran-to-lisp::resabs
                            fortran-to-lisp::resasc fortran-to-lisp::momcom
                            nil)
           :calls '(fortran-to-lisp::dqcheb fortran-to-lisp::dgtsl
                    fortran-to-lisp::dqk15w fortran-to-lisp::d1mach))))

