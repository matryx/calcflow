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


(defun dqawoe
       (f a b omega integr epsabs epsrel limit icall maxp1 result abserr neval
        ier last$ alist blist rlist elist iord nnlog momcom chebmo)
  (declare (type (array f2cl-lib:integer4 (*)) nnlog iord)
           (type (array double-float (*)) chebmo elist rlist blist alist)
           (type (f2cl-lib:integer4) momcom last$ ier neval maxp1 icall limit
                                     integr)
           (type (double-float) abserr result epsrel epsabs omega b a))
  (f2cl-lib:with-multi-array-data
      ((alist double-float alist-%data% alist-%offset%)
       (blist double-float blist-%data% blist-%offset%)
       (rlist double-float rlist-%data% rlist-%offset%)
       (elist double-float elist-%data% elist-%offset%)
       (chebmo double-float chebmo-%data% chebmo-%offset%)
       (iord f2cl-lib:integer4 iord-%data% iord-%offset%)
       (nnlog f2cl-lib:integer4 nnlog-%data% nnlog-%offset%))
    (prog ((rlist2 (make-array 52 :element-type 'double-float))
           (res3la (make-array 3 :element-type 'double-float)) (extrap nil)
           (noext nil) (extall nil) (id 0) (ierro 0) (iroff1 0) (iroff2 0)
           (iroff3 0) (jupbnd 0) (k 0) (ksgn 0) (ktmin 0) (maxerr 0) (nev 0)
           (nres 0) (nrmax 0) (nrmom 0) (numrl2 0) (abseps 0.0) (area 0.0)
           (area1 0.0) (area12 0.0) (area2 0.0) (a1 0.0) (a2 0.0) (b1 0.0)
           (b2 0.0) (correc 0.0) (defab1 0.0) (defab2 0.0) (defabs 0.0)
           (domega 0.0) (dres 0.0) (epmach 0.0) (erlarg 0.0) (erlast 0.0)
           (errbnd 0.0) (errmax 0.0) (error1 0.0) (erro12 0.0) (error2 0.0)
           (errsum 0.0) (ertest 0.0) (oflow 0.0) (resabs 0.0) (reseps 0.0)
           (small 0.0) (uflow 0.0) (width 0.0))
      (declare (type (array double-float (52)) rlist2)
               (type (array double-float (3)) res3la)
               (type (double-float) width uflow small reseps resabs oflow
                                    ertest errsum error2 erro12 error1 errmax
                                    errbnd erlast erlarg epmach dres domega
                                    defabs defab2 defab1 correc b2 b1 a2 a1
                                    area2 area12 area1 area abseps)
               (type (f2cl-lib:integer4) numrl2 nrmom nrmax nres nev maxerr
                                         ktmin ksgn k jupbnd iroff3 iroff2
                                         iroff1 ierro id)
               (type f2cl-lib:logical extall noext extrap))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf ier 0)
      (setf neval 0)
      (setf last$ 0)
      (setf result 0.0)
      (setf abserr 0.0)
      (setf (f2cl-lib:fref alist-%data% (1) ((1 *)) alist-%offset%) a)
      (setf (f2cl-lib:fref blist-%data% (1) ((1 *)) blist-%offset%) b)
      (setf (f2cl-lib:fref rlist-%data% (1) ((1 *)) rlist-%offset%) 0.0)
      (setf (f2cl-lib:fref elist-%data% (1) ((1 *)) elist-%offset%) 0.0)
      (setf (f2cl-lib:fref iord-%data% (1) ((1 *)) iord-%offset%) 0)
      (setf (f2cl-lib:fref nnlog-%data% (1) ((1 *)) nnlog-%offset%) 0)
      (if
       (or (and (/= integr 1) (/= integr 2))
           (and (<= epsabs 0.0) (< epsrel (max (* 50.0 epmach) 5.0e-29)))
           (< icall 1)
           (< maxp1 1))
       (setf ier 6))
      (if (= ier 6) (go label999))
      (setf domega (abs omega))
      (setf nrmom 0)
      (if (> icall 1) (go label5))
      (setf momcom 0)
     label5
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12 var-13 var-14)
          (dqc25f f a b domega integr nrmom maxp1 0 result abserr neval defabs
           resabs momcom chebmo)
        (declare (ignore var-0 var-1 var-2 var-5 var-6 var-7 var-14))
        (setf domega var-3)
        (setf integr var-4)
        (setf result var-8)
        (setf abserr var-9)
        (setf neval var-10)
        (setf defabs var-11)
        (setf resabs var-12)
        (setf momcom var-13))
      (setf dres (abs result))
      (setf errbnd (max epsabs (* epsrel dres)))
      (setf (f2cl-lib:fref rlist-%data% (1) ((1 *)) rlist-%offset%) result)
      (setf (f2cl-lib:fref elist-%data% (1) ((1 *)) elist-%offset%) abserr)
      (setf (f2cl-lib:fref iord-%data% (1) ((1 *)) iord-%offset%) 1)
      (if (and (<= abserr (* 100.0 epmach defabs)) (> abserr errbnd))
          (setf ier 2))
      (if (= limit 1) (setf ier 1))
      (if (or (/= ier 0) (<= abserr errbnd)) (go label200))
      (setf uflow (f2cl-lib:d1mach 1))
      (setf oflow (f2cl-lib:d1mach 2))
      (setf errmax abserr)
      (setf maxerr 1)
      (setf area result)
      (setf errsum abserr)
      (setf abserr oflow)
      (setf nrmax 1)
      (setf extrap f2cl-lib:%false%)
      (setf noext f2cl-lib:%false%)
      (setf ierro 0)
      (setf iroff1 0)
      (setf iroff2 0)
      (setf iroff3 0)
      (setf ktmin 0)
      (setf small (* (abs (- b a)) 0.75))
      (setf nres 0)
      (setf numrl2 0)
      (setf extall f2cl-lib:%false%)
      (if (> (* 0.5 (abs (- b a)) domega) 2.0) (go label10))
      (setf numrl2 1)
      (setf extall f2cl-lib:%true%)
      (setf (f2cl-lib:fref rlist2 (1) ((1 52))) result)
     label10
      (if (<= (* 0.25 (abs (- b a)) domega) 2.0) (setf extall f2cl-lib:%true%))
      (setf ksgn -1)
      (if (>= dres (* (- 1.0 (* 50.0 epmach)) defabs)) (setf ksgn 1))
      (f2cl-lib:fdo (last$ 2 (f2cl-lib:int-add last$ 1))
                    ((> last$ limit) nil)
        (tagbody
          (setf nrmom
                  (f2cl-lib:int-add
                   (f2cl-lib:fref nnlog-%data% (maxerr) ((1 *)) nnlog-%offset%)
                   1))
          (setf a1
                  (f2cl-lib:fref alist-%data% (maxerr) ((1 *)) alist-%offset%))
          (setf b1
                  (* 0.5
                     (+
                      (f2cl-lib:fref alist-%data%
                                     (maxerr)
                                     ((1 *))
                                     alist-%offset%)
                      (f2cl-lib:fref blist-%data%
                                     (maxerr)
                                     ((1 *))
                                     blist-%offset%))))
          (setf a2 b1)
          (setf b2
                  (f2cl-lib:fref blist-%data% (maxerr) ((1 *)) blist-%offset%))
          (setf erlast errmax)
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14)
              (dqc25f f a1 b1 domega integr nrmom maxp1 0 area1 error1 nev
               resabs defab1 momcom chebmo)
            (declare (ignore var-0 var-1 var-2 var-5 var-6 var-7 var-14))
            (setf domega var-3)
            (setf integr var-4)
            (setf area1 var-8)
            (setf error1 var-9)
            (setf nev var-10)
            (setf resabs var-11)
            (setf defab1 var-12)
            (setf momcom var-13))
          (setf neval (f2cl-lib:int-add neval nev))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14)
              (dqc25f f a2 b2 domega integr nrmom maxp1 1 area2 error2 nev
               resabs defab2 momcom chebmo)
            (declare (ignore var-0 var-1 var-2 var-5 var-6 var-7 var-14))
            (setf domega var-3)
            (setf integr var-4)
            (setf area2 var-8)
            (setf error2 var-9)
            (setf nev var-10)
            (setf resabs var-11)
            (setf defab2 var-12)
            (setf momcom var-13))
          (setf neval (f2cl-lib:int-add neval nev))
          (setf area12 (+ area1 area2))
          (setf erro12 (+ error1 error2))
          (setf errsum (- (+ errsum erro12) errmax))
          (setf area
                  (- (+ area area12)
                     (f2cl-lib:fref rlist-%data%
                                    (maxerr)
                                    ((1 *))
                                    rlist-%offset%)))
          (if (or (= defab1 error1) (= defab2 error2)) (go label25))
          (if
           (or
            (>
             (abs
              (- (f2cl-lib:fref rlist-%data% (maxerr) ((1 *)) rlist-%offset%)
                 area12))
             (* 1.0e-5 (abs area12)))
            (< erro12 (* 0.99 errmax)))
           (go label20))
          (if extrap (setf iroff2 (f2cl-lib:int-add iroff2 1)))
          (if (not extrap) (setf iroff1 (f2cl-lib:int-add iroff1 1)))
         label20
          (if (and (> last$ 10) (> erro12 errmax))
              (setf iroff3 (f2cl-lib:int-add iroff3 1)))
         label25
          (setf (f2cl-lib:fref rlist-%data% (maxerr) ((1 *)) rlist-%offset%)
                  area1)
          (setf (f2cl-lib:fref rlist-%data% (last$) ((1 *)) rlist-%offset%)
                  area2)
          (setf (f2cl-lib:fref nnlog-%data% (maxerr) ((1 *)) nnlog-%offset%)
                  nrmom)
          (setf (f2cl-lib:fref nnlog-%data% (last$) ((1 *)) nnlog-%offset%)
                  nrmom)
          (setf errbnd (max epsabs (* epsrel (abs area))))
          (if (or (>= (f2cl-lib:int-add iroff1 iroff2) 10) (>= iroff3 20))
              (setf ier 2))
          (if (>= iroff2 5) (setf ierro 3))
          (if (= last$ limit) (setf ier 1))
          (if
           (<= (max (abs a1) (abs b2))
               (* (+ 1.0 (* 100.0 epmach)) (+ (abs a2) (* 1000.0 uflow))))
           (setf ier 4))
          (if (> error2 error1) (go label30))
          (setf (f2cl-lib:fref alist-%data% (last$) ((1 *)) alist-%offset%) a2)
          (setf (f2cl-lib:fref blist-%data% (maxerr) ((1 *)) blist-%offset%)
                  b1)
          (setf (f2cl-lib:fref blist-%data% (last$) ((1 *)) blist-%offset%) b2)
          (setf (f2cl-lib:fref elist-%data% (maxerr) ((1 *)) elist-%offset%)
                  error1)
          (setf (f2cl-lib:fref elist-%data% (last$) ((1 *)) elist-%offset%)
                  error2)
          (go label40)
         label30
          (setf (f2cl-lib:fref alist-%data% (maxerr) ((1 *)) alist-%offset%)
                  a2)
          (setf (f2cl-lib:fref alist-%data% (last$) ((1 *)) alist-%offset%) a1)
          (setf (f2cl-lib:fref blist-%data% (last$) ((1 *)) blist-%offset%) b1)
          (setf (f2cl-lib:fref rlist-%data% (maxerr) ((1 *)) rlist-%offset%)
                  area2)
          (setf (f2cl-lib:fref rlist-%data% (last$) ((1 *)) rlist-%offset%)
                  area1)
          (setf (f2cl-lib:fref elist-%data% (maxerr) ((1 *)) elist-%offset%)
                  error2)
          (setf (f2cl-lib:fref elist-%data% (last$) ((1 *)) elist-%offset%)
                  error1)
         label40
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dqpsrt limit last$ maxerr errmax elist iord nrmax)
            (declare (ignore var-0 var-1 var-4 var-5))
            (setf maxerr var-2)
            (setf errmax var-3)
            (setf nrmax var-6))
          (if (<= errsum errbnd) (go label170))
          (if (/= ier 0) (go label150))
          (if (and (= last$ 2) extall) (go label120))
          (if noext (go label140))
          (if (not extall) (go label50))
          (setf erlarg (- erlarg erlast))
          (if (> (abs (- b1 a1)) small) (setf erlarg (+ erlarg erro12)))
          (if extrap (go label70))
         label50
          (setf width
                  (abs
                   (-
                    (f2cl-lib:fref blist-%data%
                                   (maxerr)
                                   ((1 *))
                                   blist-%offset%)
                    (f2cl-lib:fref alist-%data%
                                   (maxerr)
                                   ((1 *))
                                   alist-%offset%))))
          (if (> width small) (go label140))
          (if extall (go label60))
          (setf small (* small 0.5))
          (if (> (* 0.25 width domega) 2.0) (go label140))
          (setf extall f2cl-lib:%true%)
          (go label130)
         label60
          (setf extrap f2cl-lib:%true%)
          (setf nrmax 2)
         label70
          (if (or (= ierro 3) (<= erlarg ertest)) (go label90))
          (setf jupbnd last$)
          (if (> last$ (+ (the f2cl-lib:integer4 (truncate limit 2)) 2))
              (setf jupbnd
                      (f2cl-lib:int-sub (f2cl-lib:int-add limit 3) last$)))
          (setf id nrmax)
          (f2cl-lib:fdo (k id (f2cl-lib:int-add k 1))
                        ((> k jupbnd) nil)
            (tagbody
              (setf maxerr
                      (f2cl-lib:fref iord-%data%
                                     (nrmax)
                                     ((1 *))
                                     iord-%offset%))
              (setf errmax
                      (f2cl-lib:fref elist-%data%
                                     (maxerr)
                                     ((1 *))
                                     elist-%offset%))
              (if
               (>
                (abs
                 (-
                  (f2cl-lib:fref blist-%data% (maxerr) ((1 *)) blist-%offset%)
                  (f2cl-lib:fref alist-%data%
                                 (maxerr)
                                 ((1 *))
                                 alist-%offset%)))
                small)
               (go label140))
              (setf nrmax (f2cl-lib:int-add nrmax 1))
             label80))
         label90
          (setf numrl2 (f2cl-lib:int-add numrl2 1))
          (setf (f2cl-lib:fref rlist2 (numrl2) ((1 52))) area)
          (if (< numrl2 3) (go label110))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (dqelg numrl2 rlist2 reseps abseps res3la nres)
            (declare (ignore var-1 var-4))
            (setf numrl2 var-0)
            (setf reseps var-2)
            (setf abseps var-3)
            (setf nres var-5))
          (setf ktmin (f2cl-lib:int-add ktmin 1))
          (if (and (> ktmin 5) (< abserr (* 0.001 errsum))) (setf ier 5))
          (if (>= abseps abserr) (go label100))
          (setf ktmin 0)
          (setf abserr abseps)
          (setf result reseps)
          (setf correc erlarg)
          (setf ertest (max epsabs (* epsrel (abs reseps))))
          (if (<= abserr ertest) (go label150))
         label100
          (if (= numrl2 1) (setf noext f2cl-lib:%true%))
          (if (= ier 5) (go label150))
         label110
          (setf maxerr (f2cl-lib:fref iord-%data% (1) ((1 *)) iord-%offset%))
          (setf errmax
                  (f2cl-lib:fref elist-%data% (maxerr) ((1 *)) elist-%offset%))
          (setf nrmax 1)
          (setf extrap f2cl-lib:%false%)
          (setf small (* small 0.5))
          (setf erlarg errsum)
          (go label140)
         label120
          (setf small (* small 0.5))
          (setf numrl2 (f2cl-lib:int-add numrl2 1))
          (setf (f2cl-lib:fref rlist2 (numrl2) ((1 52))) area)
         label130
          (setf ertest errbnd)
          (setf erlarg errsum)
         label140))
     label150
      (if (or (= abserr oflow) (= nres 0)) (go label170))
      (if (= (f2cl-lib:int-add ier ierro) 0) (go label165))
      (if (= ierro 3) (setf abserr (+ abserr correc)))
      (if (= ier 0) (setf ier 3))
      (if (and (/= result 0.0) (/= area 0.0)) (go label160))
      (if (> abserr errsum) (go label170))
      (if (= area 0.0) (go label190))
      (go label165)
     label160
      (if (> (/ abserr (abs result)) (/ errsum (abs area))) (go label170))
     label165
      (if (and (= ksgn -1) (<= (max (abs result) (abs area)) (* defabs 0.01)))
          (go label190))
      (if
       (or (> 0.01 (/ result area))
           (> (/ result area) 100.0)
           (>= errsum (abs area)))
       (setf ier 6))
      (go label190)
     label170
      (setf result 0.0)
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k last$) nil)
        (tagbody
          (setf result
                  (+ result
                     (f2cl-lib:fref rlist-%data% (k) ((1 *)) rlist-%offset%)))
         label180))
      (setf abserr errsum)
     label190
      (if (> ier 2) (setf ier (f2cl-lib:int-sub ier 1)))
     label200
      (if (and (= integr 2) (< omega 0.0)) (setf result (- result)))
     label999
      (go end_label)
     end_label
      (return
       (values nil
               nil
               nil
               nil
               integr
               nil
               nil
               nil
               nil
               nil
               result
               abserr
               neval
               ier
               last$
               nil
               nil
               nil
               nil
               nil
               nil
               momcom
               nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dqawoe
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (double-float) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) (array double-float (*)))
           :return-values '(nil nil nil nil fortran-to-lisp::integr nil nil nil
                            nil nil fortran-to-lisp::result
                            fortran-to-lisp::abserr fortran-to-lisp::neval
                            fortran-to-lisp::ier fortran-to-lisp::last$ nil nil
                            nil nil nil nil fortran-to-lisp::momcom nil)
           :calls '(fortran-to-lisp::dqelg fortran-to-lisp::dqpsrt
                    fortran-to-lisp::dqc25f fortran-to-lisp::d1mach))))

