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


(defun dqawce
       (f a b c epsabs epsrel limit result abserr neval ier alist blist rlist
        elist iord last$)
  (declare (type (array f2cl-lib:integer4 (*)) iord)
           (type (array double-float (*)) elist rlist blist alist)
           (type (f2cl-lib:integer4) last$ ier neval limit)
           (type (double-float) abserr result epsrel epsabs c b a))
  (f2cl-lib:with-multi-array-data
      ((alist double-float alist-%data% alist-%offset%)
       (blist double-float blist-%data% blist-%offset%)
       (rlist double-float rlist-%data% rlist-%offset%)
       (elist double-float elist-%data% elist-%offset%)
       (iord f2cl-lib:integer4 iord-%data% iord-%offset%))
    (prog ((iroff1 0) (iroff2 0) (k 0) (krule 0) (maxerr 0) (nev 0) (nrmax 0)
           (aa 0.0) (area 0.0) (area1 0.0) (area12 0.0) (area2 0.0) (a1 0.0)
           (a2 0.0) (bb 0.0) (b1 0.0) (b2 0.0) (epmach 0.0) (errbnd 0.0)
           (errmax 0.0) (error1 0.0) (erro12 0.0) (error2 0.0) (errsum 0.0)
           (uflow 0.0))
      (declare (type (double-float) uflow errsum error2 erro12 error1 errmax
                                    errbnd epmach b2 b1 bb a2 a1 area2 area12
                                    area1 area aa)
               (type (f2cl-lib:integer4) nrmax nev maxerr krule k iroff2
                                         iroff1))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf uflow (f2cl-lib:d1mach 1))
      (setf ier 6)
      (setf neval 0)
      (setf last$ 0)
      (setf (f2cl-lib:fref alist-%data% (1) ((1 *)) alist-%offset%) a)
      (setf (f2cl-lib:fref blist-%data% (1) ((1 *)) blist-%offset%) b)
      (setf (f2cl-lib:fref rlist-%data% (1) ((1 *)) rlist-%offset%) 0.0)
      (setf (f2cl-lib:fref elist-%data% (1) ((1 *)) elist-%offset%) 0.0)
      (setf (f2cl-lib:fref iord-%data% (1) ((1 *)) iord-%offset%) 0)
      (setf result 0.0)
      (setf abserr 0.0)
      (if
       (or (= c a)
           (= c b)
           (and (<= epsabs 0.0) (< epsrel (max (* 50.0 epmach) 5.0e-29))))
       (go label999))
      (setf aa a)
      (setf bb b)
      (if (<= a b) (go label10))
      (setf aa b)
      (setf bb a)
     label10
      (setf ier 0)
      (setf krule 1)
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
          (dqc25c f aa bb c result abserr krule neval)
        (declare (ignore var-0 var-1 var-2))
        (setf c var-3)
        (setf result var-4)
        (setf abserr var-5)
        (setf krule var-6)
        (setf neval var-7))
      (setf last$ 1)
      (setf (f2cl-lib:fref rlist-%data% (1) ((1 *)) rlist-%offset%) result)
      (setf (f2cl-lib:fref elist-%data% (1) ((1 *)) elist-%offset%) abserr)
      (setf (f2cl-lib:fref iord-%data% (1) ((1 *)) iord-%offset%) 1)
      (setf (f2cl-lib:fref alist-%data% (1) ((1 *)) alist-%offset%) a)
      (setf (f2cl-lib:fref blist-%data% (1) ((1 *)) blist-%offset%) b)
      (setf errbnd (max epsabs (* epsrel (abs result))))
      (if (= limit 1) (setf ier 1))
      (if (or (< abserr (min (* 0.01 (abs result)) errbnd)) (= ier 1))
          (go label70))
      (setf (f2cl-lib:fref alist-%data% (1) ((1 *)) alist-%offset%) aa)
      (setf (f2cl-lib:fref blist-%data% (1) ((1 *)) blist-%offset%) bb)
      (setf (f2cl-lib:fref rlist-%data% (1) ((1 *)) rlist-%offset%) result)
      (setf errmax abserr)
      (setf maxerr 1)
      (setf area result)
      (setf errsum abserr)
      (setf nrmax 1)
      (setf iroff1 0)
      (setf iroff2 0)
      (f2cl-lib:fdo (last$ 2 (f2cl-lib:int-add last$ 1))
                    ((> last$ limit) nil)
        (tagbody
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
          (setf b2
                  (f2cl-lib:fref blist-%data% (maxerr) ((1 *)) blist-%offset%))
          (if (and (<= c b1) (> c a1)) (setf b1 (* 0.5 (+ c b2))))
          (if (and (> c b1) (< c b2)) (setf b1 (* 0.5 (+ a1 c))))
          (setf a2 b1)
          (setf krule 2)
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
              (dqc25c f a1 b1 c area1 error1 krule nev)
            (declare (ignore var-0 var-1 var-2))
            (setf c var-3)
            (setf area1 var-4)
            (setf error1 var-5)
            (setf krule var-6)
            (setf nev var-7))
          (setf neval (f2cl-lib:int-add neval nev))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
              (dqc25c f a2 b2 c area2 error2 krule nev)
            (declare (ignore var-0 var-1 var-2))
            (setf c var-3)
            (setf area2 var-4)
            (setf error2 var-5)
            (setf krule var-6)
            (setf nev var-7))
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
          (if
           (and
            (<
             (abs
              (- (f2cl-lib:fref rlist-%data% (maxerr) ((1 *)) rlist-%offset%)
                 area12))
             (* 1.0e-5 (abs area12)))
            (>= erro12 (* 0.99 errmax))
            (= krule 0))
           (setf iroff1 (f2cl-lib:int-add iroff1 1)))
          (if (and (> last$ 10) (> erro12 errmax) (= krule 0))
              (setf iroff2 (f2cl-lib:int-add iroff2 1)))
          (setf (f2cl-lib:fref rlist-%data% (maxerr) ((1 *)) rlist-%offset%)
                  area1)
          (setf (f2cl-lib:fref rlist-%data% (last$) ((1 *)) rlist-%offset%)
                  area2)
          (setf errbnd (max epsabs (* epsrel (abs area))))
          (if (<= errsum errbnd) (go label15))
          (if (and (>= iroff1 6) (> iroff2 20)) (setf ier 2))
          (if (= last$ limit) (setf ier 1))
          (if
           (<= (max (abs a1) (abs b2))
               (* (+ 1.0 (* 100.0 epmach)) (+ (abs a2) (* 1000.0 uflow))))
           (setf ier 3))
         label15
          (if (> error2 error1) (go label20))
          (setf (f2cl-lib:fref alist-%data% (last$) ((1 *)) alist-%offset%) a2)
          (setf (f2cl-lib:fref blist-%data% (maxerr) ((1 *)) blist-%offset%)
                  b1)
          (setf (f2cl-lib:fref blist-%data% (last$) ((1 *)) blist-%offset%) b2)
          (setf (f2cl-lib:fref elist-%data% (maxerr) ((1 *)) elist-%offset%)
                  error1)
          (setf (f2cl-lib:fref elist-%data% (last$) ((1 *)) elist-%offset%)
                  error2)
          (go label30)
         label20
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
         label30
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dqpsrt limit last$ maxerr errmax elist iord nrmax)
            (declare (ignore var-0 var-1 var-4 var-5))
            (setf maxerr var-2)
            (setf errmax var-3)
            (setf nrmax var-6))
          (if (or (/= ier 0) (<= errsum errbnd)) (go label50))
         label40))
     label50
      (setf result 0.0)
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k last$) nil)
        (tagbody
          (setf result
                  (+ result
                     (f2cl-lib:fref rlist-%data% (k) ((1 *)) rlist-%offset%)))
         label60))
      (setf abserr errsum)
     label70
      (if (= aa b) (setf result (- result)))
     label999
      (go end_label)
     end_label
      (return
       (values nil
               nil
               nil
               c
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
               nil
               nil
               last$)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dqawce
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (double-float) (double-float) (double-float)
                        (double-float) (double-float)
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil fortran-to-lisp::c nil nil nil
                            fortran-to-lisp::result fortran-to-lisp::abserr
                            fortran-to-lisp::neval fortran-to-lisp::ier nil nil
                            nil nil nil fortran-to-lisp::last$)
           :calls '(fortran-to-lisp::dqpsrt fortran-to-lisp::dqc25c
                    fortran-to-lisp::d1mach))))

