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
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((p 0.9) (pi$ 3.141592653589793))
  (declare (type (double-float) p pi$))
  (defun dqawfe
         (f a omega integr epsabs limlst limit maxp1 result abserr neval ier
          rslst erlst ierlst lst alist blist rlist elist iord nnlog chebmo)
    (declare (type (array f2cl-lib:integer4 (*)) nnlog iord ierlst)
             (type (array double-float (*)) chebmo elist rlist blist alist
                                            erlst rslst)
             (type (f2cl-lib:integer4) lst ier neval maxp1 limit limlst integr)
             (type (double-float) abserr result epsabs omega a))
    (f2cl-lib:with-multi-array-data
        ((rslst double-float rslst-%data% rslst-%offset%)
         (erlst double-float erlst-%data% erlst-%offset%)
         (alist double-float alist-%data% alist-%offset%)
         (blist double-float blist-%data% blist-%offset%)
         (rlist double-float rlist-%data% rlist-%offset%)
         (elist double-float elist-%data% elist-%offset%)
         (chebmo double-float chebmo-%data% chebmo-%offset%)
         (ierlst f2cl-lib:integer4 ierlst-%data% ierlst-%offset%)
         (iord f2cl-lib:integer4 iord-%data% iord-%offset%)
         (nnlog f2cl-lib:integer4 nnlog-%data% nnlog-%offset%))
      (prog ((psum (make-array 52 :element-type 'double-float))
             (res3la (make-array 3 :element-type 'double-float)) (ktmin 0)
             (l 0) (ll 0) (momcom 0) (nev 0) (nres 0) (numrl2 0) (abseps 0.0)
             (correc 0.0) (cycle 0.0) (c1 0.0) (c2 0.0) (dl 0.0) (drl 0.0)
             (ep 0.0) (eps 0.0) (epsa 0.0) (errsum 0.0) (fact 0.0) (p1 0.0)
             (reseps 0.0) (uflow 0.0) (last$ 0))
        (declare (type (array double-float (3)) res3la)
                 (type (array double-float (52)) psum)
                 (type (double-float) uflow reseps p1 fact errsum epsa eps ep
                                      drl dl c2 c1 cycle correc abseps)
                 (type (f2cl-lib:integer4) last$ numrl2 nres nev momcom ll l
                                           ktmin))
        (setf result 0.0)
        (setf abserr 0.0)
        (setf neval 0)
        (setf lst 0)
        (setf ier 0)
        (if (or (and (/= integr 1) (/= integr 2)) (<= epsabs 0.0) (< limlst 3))
            (setf ier 6))
        (if (= ier 6) (go label999))
        (if (/= omega 0.0) (go label10))
        (if (= integr 1)
            (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10 var-11 var-12 var-13 var-14 var-15)
                (dqagie f a 1 epsabs 0.0 limit result abserr neval ier alist
                 blist rlist elist iord last$)
              (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-10
                               var-11 var-12 var-13 var-14))
              (setf result var-6)
              (setf abserr var-7)
              (setf neval var-8)
              (setf ier var-9)
              (setf last$ var-15)))
        (setf (f2cl-lib:fref rslst-%data% (1) ((1 *)) rslst-%offset%) result)
        (setf (f2cl-lib:fref erlst-%data% (1) ((1 *)) erlst-%offset%) abserr)
        (setf (f2cl-lib:fref ierlst-%data% (1) ((1 *)) ierlst-%offset%) ier)
        (setf lst 1)
        (go label999)
       label10
        (setf l (f2cl-lib:int (abs omega)))
        (setf dl
                (coerce
                 (the f2cl-lib:integer4
                      (f2cl-lib:int-add (f2cl-lib:int-mul 2 l) 1))
                 'double-float))
        (setf cycle (/ (* dl pi$) (abs omega)))
        (setf ier 0)
        (setf ktmin 0)
        (setf neval 0)
        (setf numrl2 0)
        (setf nres 0)
        (setf c1 a)
        (setf c2 (+ cycle a))
        (setf p1 (- 1.0 p))
        (setf uflow (f2cl-lib:d1mach 1))
        (setf eps epsabs)
        (if (> epsabs (/ uflow p1)) (setf eps (* epsabs p1)))
        (setf ep eps)
        (setf fact 1.0)
        (setf correc 0.0)
        (setf abserr 0.0)
        (setf errsum 0.0)
        (f2cl-lib:fdo (lst 1 (f2cl-lib:int-add lst 1))
                      ((> lst limlst) nil)
          (tagbody
            (setf epsa (* eps fact))
            (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17
                   var-18 var-19 var-20 var-21 var-22)
                (dqawoe f c1 c2 omega integr epsa 0.0 limit lst maxp1
                 (f2cl-lib:fref rslst-%data% (lst) ((1 *)) rslst-%offset%)
                 (f2cl-lib:fref erlst-%data% (lst) ((1 *)) erlst-%offset%) nev
                 (f2cl-lib:fref ierlst-%data% (lst) ((1 *)) ierlst-%offset%)
                 last$ alist blist rlist elist iord nnlog momcom chebmo)
              (declare (ignore var-0 var-1 var-2 var-3 var-5 var-6 var-7 var-8
                               var-9 var-15 var-16 var-17 var-18 var-19 var-20
                               var-22))
              (setf integr var-4)
              (setf (f2cl-lib:fref rslst-%data% (lst) ((1 *)) rslst-%offset%)
                      var-10)
              (setf (f2cl-lib:fref erlst-%data% (lst) ((1 *)) erlst-%offset%)
                      var-11)
              (setf nev var-12)
              (setf (f2cl-lib:fref ierlst-%data% (lst) ((1 *)) ierlst-%offset%)
                      var-13)
              (setf last$ var-14)
              (setf momcom var-21))
            (setf neval (f2cl-lib:int-add neval nev))
            (setf fact (* fact p))
            (setf errsum
                    (+ errsum
                       (f2cl-lib:fref erlst-%data%
                                      (lst)
                                      ((1 *))
                                      erlst-%offset%)))
            (setf drl
                    (* 50.0
                       (abs
                        (f2cl-lib:fref rslst-%data%
                                       (lst)
                                       ((1 *))
                                       rslst-%offset%))))
            (if (and (<= (+ errsum drl) epsabs) (>= lst 6)) (go label80))
            (setf correc
                    (max correc
                         (f2cl-lib:fref erlst-%data%
                                        (lst)
                                        ((1 *))
                                        erlst-%offset%)))
            (if
             (/= (f2cl-lib:fref ierlst-%data% (lst) ((1 *)) ierlst-%offset%) 0)
             (setf eps (max ep (* correc p1))))
            (if
             (/= (f2cl-lib:fref ierlst-%data% (lst) ((1 *)) ierlst-%offset%) 0)
             (setf ier 7))
            (if (and (= ier 7) (<= (+ errsum drl) (* correc 10.0)) (> lst 5))
                (go label80))
            (setf numrl2 (f2cl-lib:int-add numrl2 1))
            (if (> lst 1) (go label20))
            (setf (f2cl-lib:fref psum (1) ((1 52)))
                    (f2cl-lib:fref rslst-%data% (1) ((1 *)) rslst-%offset%))
            (go label40)
           label20
            (setf (f2cl-lib:fref psum (numrl2) ((1 52)))
                    (+ (f2cl-lib:fref psum (ll) ((1 52)))
                       (f2cl-lib:fref rslst-%data%
                                      (lst)
                                      ((1 *))
                                      rslst-%offset%)))
            (if (= lst 2) (go label40))
            (if (= lst limlst) (setf ier 1))
            (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                (dqelg numrl2 psum reseps abseps res3la nres)
              (declare (ignore var-1 var-4))
              (setf numrl2 var-0)
              (setf reseps var-2)
              (setf abseps var-3)
              (setf nres var-5))
            (setf ktmin (f2cl-lib:int-add ktmin 1))
            (if (and (>= ktmin 15) (<= abserr (* 0.001 (+ errsum drl))))
                (setf ier 4))
            (if (and (> abseps abserr) (/= lst 3)) (go label30))
            (setf abserr abseps)
            (setf result reseps)
            (setf ktmin 0)
            (if
             (or (<= (+ abserr (* 10.0 correc)) epsabs)
                 (and (<= abserr epsabs) (>= (* 10.0 correc) epsabs)))
             (go label60))
           label30
            (if (and (/= ier 0) (/= ier 7)) (go label60))
           label40
            (setf ll numrl2)
            (setf c1 c2)
            (setf c2 (+ c2 cycle))
           label50))
       label60
        (setf abserr (+ abserr (* 10.0 correc)))
        (if (= ier 0) (go label999))
        (if
         (and (/= result 0.0) (/= (f2cl-lib:fref psum (numrl2) ((1 52))) 0.0))
         (go label70))
        (if (> abserr errsum) (go label80))
        (if (= (f2cl-lib:fref psum (numrl2) ((1 52))) 0.0) (go label999))
       label70
        (if
         (> (/ abserr (abs result))
            (/ (+ errsum drl) (abs (f2cl-lib:fref psum (numrl2) ((1 52))))))
         (go label80))
        (if (and (>= ier 1) (/= ier 7)) (setf abserr (+ abserr drl)))
        (go label999)
       label80
        (setf result (f2cl-lib:fref psum (numrl2) ((1 52))))
        (setf abserr (+ errsum drl))
       label999
        (go end_label)
       end_label
        (return
         (values nil
                 nil
                 nil
                 integr
                 nil
                 nil
                 nil
                 nil
                 result
                 abserr
                 neval
                 ier
                 nil
                 nil
                 nil
                 lst
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dqawfe
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (double-float) (double-float)
                        (fortran-to-lisp::integer4) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)))
           :return-values '(nil nil nil fortran-to-lisp::integr nil nil nil nil
                            fortran-to-lisp::result fortran-to-lisp::abserr
                            fortran-to-lisp::neval fortran-to-lisp::ier nil nil
                            nil fortran-to-lisp::lst nil nil nil nil nil nil
                            nil)
           :calls '(fortran-to-lisp::dqelg fortran-to-lisp::dqawoe
                    fortran-to-lisp::d1mach fortran-to-lisp::dqagie))))

