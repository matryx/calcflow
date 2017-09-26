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


(defun dqawse
       (f a b alfa beta integr epsabs epsrel limit result abserr neval ier
        alist blist rlist elist iord last$)
  (declare (type (array f2cl-lib:integer4 (*)) iord)
           (type (array double-float (*)) elist rlist blist alist)
           (type (f2cl-lib:integer4) last$ ier neval limit integr)
           (type (double-float) abserr result epsrel epsabs beta alfa b a))
  (f2cl-lib:with-multi-array-data
      ((alist double-float alist-%data% alist-%offset%)
       (blist double-float blist-%data% blist-%offset%)
       (rlist double-float rlist-%data% rlist-%offset%)
       (elist double-float elist-%data% elist-%offset%)
       (iord f2cl-lib:integer4 iord-%data% iord-%offset%))
    (prog ((ri (make-array 25 :element-type 'double-float))
           (rj (make-array 25 :element-type 'double-float))
           (rh (make-array 25 :element-type 'double-float))
           (rg (make-array 25 :element-type 'double-float)) (iroff1 0)
           (iroff2 0) (k 0) (maxerr 0) (nev 0) (nrmax 0) (area 0.0) (area1 0.0)
           (area12 0.0) (area2 0.0) (a1 0.0) (a2 0.0) (b1 0.0) (b2 0.0)
           (centre 0.0) (epmach 0.0) (errbnd 0.0) (errmax 0.0) (error1 0.0)
           (erro12 0.0) (error2 0.0) (errsum 0.0) (resas1 0.0) (resas2 0.0)
           (uflow 0.0))
      (declare (type (array double-float (25)) rj ri rh rg)
               (type (double-float) uflow resas2 resas1 errsum error2 erro12
                                    error1 errmax errbnd epmach centre b2 b1 a2
                                    a1 area2 area12 area1 area)
               (type (f2cl-lib:integer4) nrmax nev maxerr k iroff2 iroff1))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf uflow (f2cl-lib:d1mach 1))
      (setf ier 6)
      (setf neval 0)
      (setf last$ 0)
      (setf (f2cl-lib:fref rlist-%data% (1) ((1 *)) rlist-%offset%) 0.0)
      (setf (f2cl-lib:fref elist-%data% (1) ((1 *)) elist-%offset%) 0.0)
      (setf (f2cl-lib:fref iord-%data% (1) ((1 *)) iord-%offset%) 0)
      (setf result 0.0)
      (setf abserr 0.0)
      (if
       (or (<= b a)
           (and (= epsabs 0.0) (< epsrel (max (* 50.0 epmach) 5.0e-29)))
           (<= alfa -1.0)
           (<= beta -1.0)
           (< integr 1)
           (> integr 4)
           (< limit 2))
       (go label999))
      (setf ier 0)
      (dqmomo alfa beta ri rj rg rh integr)
      (setf centre (* 0.5 (+ b a)))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12 var-13 var-14 var-15)
          (dqc25s f a b a centre alfa beta ri rj rg rh area1 error1 resas1
           integr nev)
        (declare (ignore var-0 var-3 var-4 var-7 var-8 var-9 var-10))
        (setf a var-1)
        (setf b var-2)
        (setf alfa var-5)
        (setf beta var-6)
        (setf area1 var-11)
        (setf error1 var-12)
        (setf resas1 var-13)
        (setf integr var-14)
        (setf nev var-15))
      (setf neval nev)
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12 var-13 var-14 var-15)
          (dqc25s f a b centre b alfa beta ri rj rg rh area2 error2 resas2
           integr nev)
        (declare (ignore var-0 var-3 var-4 var-7 var-8 var-9 var-10))
        (setf a var-1)
        (setf b var-2)
        (setf alfa var-5)
        (setf beta var-6)
        (setf area2 var-11)
        (setf error2 var-12)
        (setf resas2 var-13)
        (setf integr var-14)
        (setf nev var-15))
      (setf last$ 2)
      (setf neval (f2cl-lib:int-add neval nev))
      (setf result (+ area1 area2))
      (setf abserr (+ error1 error2))
      (setf errbnd (max epsabs (* epsrel (abs result))))
      (if (> error2 error1) (go label10))
      (setf (f2cl-lib:fref alist-%data% (1) ((1 *)) alist-%offset%) a)
      (setf (f2cl-lib:fref alist-%data% (2) ((1 *)) alist-%offset%) centre)
      (setf (f2cl-lib:fref blist-%data% (1) ((1 *)) blist-%offset%) centre)
      (setf (f2cl-lib:fref blist-%data% (2) ((1 *)) blist-%offset%) b)
      (setf (f2cl-lib:fref rlist-%data% (1) ((1 *)) rlist-%offset%) area1)
      (setf (f2cl-lib:fref rlist-%data% (2) ((1 *)) rlist-%offset%) area2)
      (setf (f2cl-lib:fref elist-%data% (1) ((1 *)) elist-%offset%) error1)
      (setf (f2cl-lib:fref elist-%data% (2) ((1 *)) elist-%offset%) error2)
      (go label20)
     label10
      (setf (f2cl-lib:fref alist-%data% (1) ((1 *)) alist-%offset%) centre)
      (setf (f2cl-lib:fref alist-%data% (2) ((1 *)) alist-%offset%) a)
      (setf (f2cl-lib:fref blist-%data% (1) ((1 *)) blist-%offset%) b)
      (setf (f2cl-lib:fref blist-%data% (2) ((1 *)) blist-%offset%) centre)
      (setf (f2cl-lib:fref rlist-%data% (1) ((1 *)) rlist-%offset%) area2)
      (setf (f2cl-lib:fref rlist-%data% (2) ((1 *)) rlist-%offset%) area1)
      (setf (f2cl-lib:fref elist-%data% (1) ((1 *)) elist-%offset%) error2)
      (setf (f2cl-lib:fref elist-%data% (2) ((1 *)) elist-%offset%) error1)
     label20
      (setf (f2cl-lib:fref iord-%data% (1) ((1 *)) iord-%offset%) 1)
      (setf (f2cl-lib:fref iord-%data% (2) ((1 *)) iord-%offset%) 2)
      (if (= limit 2) (setf ier 1))
      (if (or (<= abserr errbnd) (= ier 1)) (go label999))
      (setf errmax (f2cl-lib:fref elist-%data% (1) ((1 *)) elist-%offset%))
      (setf maxerr 1)
      (setf nrmax 1)
      (setf area result)
      (setf errsum abserr)
      (setf iroff1 0)
      (setf iroff2 0)
      (f2cl-lib:fdo (last$ 3 (f2cl-lib:int-add last$ 1))
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
          (setf a2 b1)
          (setf b2
                  (f2cl-lib:fref blist-%data% (maxerr) ((1 *)) blist-%offset%))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15)
              (dqc25s f a b a1 b1 alfa beta ri rj rg rh area1 error1 resas1
               integr nev)
            (declare (ignore var-0 var-3 var-4 var-7 var-8 var-9 var-10))
            (setf a var-1)
            (setf b var-2)
            (setf alfa var-5)
            (setf beta var-6)
            (setf area1 var-11)
            (setf error1 var-12)
            (setf resas1 var-13)
            (setf integr var-14)
            (setf nev var-15))
          (setf neval (f2cl-lib:int-add neval nev))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15)
              (dqc25s f a b a2 b2 alfa beta ri rj rg rh area2 error2 resas2
               integr nev)
            (declare (ignore var-0 var-3 var-4 var-7 var-8 var-9 var-10))
            (setf a var-1)
            (setf b var-2)
            (setf alfa var-5)
            (setf beta var-6)
            (setf area2 var-11)
            (setf error2 var-12)
            (setf resas2 var-13)
            (setf integr var-14)
            (setf nev var-15))
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
          (if (or (= a a1) (= b b2)) (go label30))
          (if (or (= resas1 error1) (= resas2 error2)) (go label30))
          (if
           (and
            (<
             (abs
              (- (f2cl-lib:fref rlist-%data% (maxerr) ((1 *)) rlist-%offset%)
                 area12))
             (* 1.0e-5 (abs area12)))
            (>= erro12 (* 0.99 errmax)))
           (setf iroff1 (f2cl-lib:int-add iroff1 1)))
          (if (and (> last$ 10) (> erro12 errmax))
              (setf iroff2 (f2cl-lib:int-add iroff2 1)))
         label30
          (setf (f2cl-lib:fref rlist-%data% (maxerr) ((1 *)) rlist-%offset%)
                  area1)
          (setf (f2cl-lib:fref rlist-%data% (last$) ((1 *)) rlist-%offset%)
                  area2)
          (setf errbnd (max epsabs (* epsrel (abs area))))
          (if (<= errsum errbnd) (go label35))
          (if (= last$ limit) (setf ier 1))
          (if (or (>= iroff1 6) (>= iroff2 20)) (setf ier 2))
          (if
           (<= (max (abs a1) (abs b2))
               (* (+ 1.0 (* 100.0 epmach)) (+ (abs a2) (* 1000.0 uflow))))
           (setf ier 3))
         label35
          (if (> error2 error1) (go label40))
          (setf (f2cl-lib:fref alist-%data% (last$) ((1 *)) alist-%offset%) a2)
          (setf (f2cl-lib:fref blist-%data% (maxerr) ((1 *)) blist-%offset%)
                  b1)
          (setf (f2cl-lib:fref blist-%data% (last$) ((1 *)) blist-%offset%) b2)
          (setf (f2cl-lib:fref elist-%data% (maxerr) ((1 *)) elist-%offset%)
                  error1)
          (setf (f2cl-lib:fref elist-%data% (last$) ((1 *)) elist-%offset%)
                  error2)
          (go label50)
         label40
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
         label50
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dqpsrt limit last$ maxerr errmax elist iord nrmax)
            (declare (ignore var-0 var-1 var-4 var-5))
            (setf maxerr var-2)
            (setf errmax var-3)
            (setf nrmax var-6))
          (if (or (/= ier 0) (<= errsum errbnd)) (go label70))
         label60))
     label70
      (setf result 0.0)
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k last$) nil)
        (tagbody
          (setf result
                  (+ result
                     (f2cl-lib:fref rlist-%data% (k) ((1 *)) rlist-%offset%)))
         label80))
      (setf abserr errsum)
     label999
      (go end_label)
     end_label
      (return
       (values nil
               a
               b
               alfa
               beta
               integr
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
  (setf (gethash 'fortran-to-lisp::dqawse
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (double-float) (double-float) (double-float)
                        (double-float) (fortran-to-lisp::integer4)
                        (double-float) (double-float)
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil fortran-to-lisp::a fortran-to-lisp::b
                            fortran-to-lisp::alfa fortran-to-lisp::beta
                            fortran-to-lisp::integr nil nil nil
                            fortran-to-lisp::result fortran-to-lisp::abserr
                            fortran-to-lisp::neval fortran-to-lisp::ier nil nil
                            nil nil nil fortran-to-lisp::last$)
           :calls '(fortran-to-lisp::dqpsrt fortran-to-lisp::dqc25s
                    fortran-to-lisp::dqmomo fortran-to-lisp::d1mach))))

