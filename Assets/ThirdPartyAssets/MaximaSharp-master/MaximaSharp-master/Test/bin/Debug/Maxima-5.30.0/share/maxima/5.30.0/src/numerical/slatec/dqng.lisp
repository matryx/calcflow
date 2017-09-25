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


(let ((x1
       (make-array 5
                   :element-type 'double-float
                   :initial-contents '(0.9739065285171717 0.8650633666889845
                                       0.6794095682990244 0.4333953941292472
                                       0.14887433898163122)))
      (w10
       (make-array 5
                   :element-type 'double-float
                   :initial-contents '(0.06667134430868814 0.1494513491505806
                                       0.21908636251598204 0.26926671930999635
                                       0.29552422471475287)))
      (x2
       (make-array 5
                   :element-type 'double-float
                   :initial-contents '(0.9956571630258081 0.9301574913557082
                                       0.7808177265864169 0.5627571346686047
                                       0.2943928627014602)))
      (w21a
       (make-array 5
                   :element-type 'double-float
                   :initial-contents '(0.032558162307964725 0.07503967481091996
                                       0.10938715880229764 0.13470921731147334
                                       0.14773910490133849)))
      (w21b
       (make-array 6
                   :element-type 'double-float
                   :initial-contents '(0.011694638867371874
                                       0.054755896574351995 0.0931254545836976
                                       0.12349197626206584 0.14277593857706009
                                       0.1494455540029169)))
      (x3
       (make-array 11
                   :element-type 'double-float
                   :initial-contents '(0.999333360901932 0.9874334029080889
                                       0.9548079348142663 0.9001486957483283
                                       0.8251983149831141 0.732148388989305
                                       0.6228479705377252 0.4994795740710565
                                       0.36490166134658075 0.2222549197766013
                                       0.07465061746138332)))
      (w43a
       (make-array 10
                   :element-type 'double-float
                   :initial-contents '(0.016296734289666565 0.0375228761208695
                                       0.05469490205825544 0.06735541460947808
                                       0.07387019963239395 0.005768556059769796
                                       0.027371890593248842 0.04656082691042883
                                       0.06174499520144257
                                       0.07138726726869339)))
      (w43b
       (make-array 12
                   :element-type 'double-float
                   :initial-contents '(0.001844477640212414
                                       0.010798689585891651
                                       0.021895363867795427
                                       0.032597463975345686 0.04216313793519181
                                       0.050741939600184575 0.05837939554261925
                                       0.06474640495144589 0.06956619791235648
                                       0.07282444147183322 0.07450775101417512
                                       0.07472214751740301)))
      (x4
       (make-array 22
                   :element-type 'double-float
                   :initial-contents '(0.9999029772627293 0.9979898959866788
                                       0.9921754978606873 0.9813581635727128
                                       0.9650576238583847 0.9431676131336706
                                       0.9158064146855072 0.8832216577713164
                                       0.8457107484624157 0.8035576580352309
                                       0.7570057306854956 0.7062732097873218
                                       0.6515894665011779 0.5932233740579611
                                       0.531493605970832 0.46676362304202285
                                       0.3994248478592188 0.3298748771061883
                                       0.25850355920216156 0.18569539656834666
                                       0.11184221317990747
                                       0.03735212339461987)))
      (w87a
       (make-array 21
                   :element-type 'double-float
                   :initial-contents '(0.008148377384149173
                                       0.018761438201562824
                                       0.027347451050052287 0.03367770731163793
                                       0.036935099820427905
                                       0.0028848724302115306
                                       0.013685946022712702 0.02328041350288831
                                       0.03087249761171336 0.03569363363941877
                                       9.152833452022414e-4
                                       0.005399280219300471 0.01094767960111893
                                       0.016298731696787336
                                       0.021081568889203834
                                       0.025370969769253827
                                       0.029189697756475754 0.03237320246720279
                                       0.034783098950365146 0.03641222073135179
                                       0.037253875503047706)))
      (w87b
       (make-array 23
                   :element-type 'double-float
                   :initial-contents '(2.7414556376207234e-4
                                       0.0018071241550579428
                                       0.0040968692827591646
                                       0.006758290051847379
                                       0.009549957672201646
                                       0.012329447652244854
                                       0.015010447346388952 0.01754896798624319
                                       0.019938037786440887
                                       0.022194935961012286
                                       0.024339147126000805
                                       0.026374505414839208 0.0282869107887712
                                       0.030052581128092695 0.03164675137143993
                                       0.033050413419978504
                                       0.034255099704226064 0.03526241266015668
                                       0.0360769896228887 0.03669860449845609
                                       0.037120549269832576 0.03733422875193504
                                       0.037361073762679026))))
  (declare (type (array double-float (5)) x1 w10 x2 w21a)
           (type (array double-float (6)) w21b)
           (type (array double-float (11)) x3)
           (type (array double-float (10)) w43a)
           (type (array double-float (12)) w43b)
           (type (array double-float (22)) x4)
           (type (array double-float (21)) w87a)
           (type (array double-float (23)) w87b))
  (defun dqng (f a b epsabs epsrel result abserr neval ier)
    (declare (type (f2cl-lib:integer4) ier neval)
             (type (double-float) abserr result epsrel epsabs b a))
    (prog ((fv1 (make-array 5 :element-type 'double-float))
           (fv2 (make-array 5 :element-type 'double-float))
           (fv3 (make-array 5 :element-type 'double-float))
           (fv4 (make-array 5 :element-type 'double-float))
           (savfun (make-array 21 :element-type 'double-float)) (ipx 0) (k 0)
           (l 0) (absc 0.0) (centr 0.0) (dhlgth 0.0) (epmach 0.0) (fcentr 0.0)
           (fval 0.0) (fval1 0.0) (fval2 0.0) (hlgth 0.0) (res10 0.0)
           (res21 0.0) (res43 0.0) (res87 0.0) (resabs 0.0) (resasc 0.0)
           (reskh 0.0) (uflow 0.0))
      (declare (type (array double-float (21)) savfun)
               (type (array double-float (5)) fv4 fv3 fv2 fv1)
               (type (double-float) uflow reskh resasc resabs res87 res43 res21
                                    res10 hlgth fval2 fval1 fval fcentr epmach
                                    dhlgth centr absc)
               (type (f2cl-lib:integer4) l k ipx))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf uflow (f2cl-lib:d1mach 1))
      (setf result 0.0)
      (setf abserr 0.0)
      (setf neval 0)
      (setf ier 6)
      (if (and (<= epsabs 0.0) (< epsrel (max (* 50.0 epmach) 5.0e-29)))
          (go label80))
      (setf hlgth (* 0.5 (- b a)))
      (setf dhlgth (abs hlgth))
      (setf centr (* 0.5 (+ b a)))
      (setf fcentr
              (multiple-value-bind (ret-val var-0)
                  (funcall f centr)
                (declare (ignore))
                (when var-0
                  (setf centr var-0))
                ret-val))
      (setf neval 21)
      (setf ier 1)
      (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                    ((> l 3) nil)
        (tagbody
          (f2cl-lib:computed-goto (label5 label25 label45) l)
         label5
          (setf res10 0.0)
          (setf res21 (* (f2cl-lib:fref w21b (6) ((1 6))) fcentr))
          (setf resabs (* (f2cl-lib:fref w21b (6) ((1 6))) (abs fcentr)))
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k 5) nil)
            (tagbody
              (setf absc (* hlgth (f2cl-lib:fref x1 (k) ((1 5)))))
              (setf fval1 (funcall f (+ centr absc)))
              (setf fval2 (funcall f (- centr absc)))
              (setf fval (+ fval1 fval2))
              (setf res10 (+ res10 (* (f2cl-lib:fref w10 (k) ((1 5))) fval)))
              (setf res21 (+ res21 (* (f2cl-lib:fref w21a (k) ((1 5))) fval)))
              (setf resabs
                      (+ resabs
                         (* (f2cl-lib:fref w21a (k) ((1 5)))
                            (+ (abs fval1) (abs fval2)))))
              (setf (f2cl-lib:fref savfun (k) ((1 21))) fval)
              (setf (f2cl-lib:fref fv1 (k) ((1 5))) fval1)
              (setf (f2cl-lib:fref fv2 (k) ((1 5))) fval2)
             label10))
          (setf ipx 5)
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k 5) nil)
            (tagbody
              (setf ipx (f2cl-lib:int-add ipx 1))
              (setf absc (* hlgth (f2cl-lib:fref x2 (k) ((1 5)))))
              (setf fval1 (funcall f (+ centr absc)))
              (setf fval2 (funcall f (- centr absc)))
              (setf fval (+ fval1 fval2))
              (setf res21 (+ res21 (* (f2cl-lib:fref w21b (k) ((1 6))) fval)))
              (setf resabs
                      (+ resabs
                         (* (f2cl-lib:fref w21b (k) ((1 6)))
                            (+ (abs fval1) (abs fval2)))))
              (setf (f2cl-lib:fref savfun (ipx) ((1 21))) fval)
              (setf (f2cl-lib:fref fv3 (k) ((1 5))) fval1)
              (setf (f2cl-lib:fref fv4 (k) ((1 5))) fval2)
             label15))
          (setf result (* res21 hlgth))
          (setf resabs (* resabs dhlgth))
          (setf reskh (* 0.5 res21))
          (setf resasc
                  (* (f2cl-lib:fref w21b (6) ((1 6))) (abs (- fcentr reskh))))
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k 5) nil)
            (tagbody
              (setf resasc
                      (+ resasc
                         (* (f2cl-lib:fref w21a (k) ((1 5)))
                            (+ (abs (- (f2cl-lib:fref fv1 (k) ((1 5))) reskh))
                               (abs
                                (- (f2cl-lib:fref fv2 (k) ((1 5))) reskh))))
                         (* (f2cl-lib:fref w21b (k) ((1 6)))
                            (+ (abs (- (f2cl-lib:fref fv3 (k) ((1 5))) reskh))
                               (abs
                                (- (f2cl-lib:fref fv4 (k) ((1 5))) reskh))))))
             label20))
          (setf abserr (abs (* (- res21 res10) hlgth)))
          (setf resasc (* resasc dhlgth))
          (go label65)
         label25
          (setf res43 (* (f2cl-lib:fref w43b (12) ((1 12))) fcentr))
          (setf neval 43)
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k 10) nil)
            (tagbody
              (setf res43
                      (+ res43
                         (* (f2cl-lib:fref savfun (k) ((1 21)))
                            (f2cl-lib:fref w43a (k) ((1 10))))))
             label30))
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k 11) nil)
            (tagbody
              (setf ipx (f2cl-lib:int-add ipx 1))
              (setf absc (* hlgth (f2cl-lib:fref x3 (k) ((1 11)))))
              (setf fval
                      (+ (funcall f (+ absc centr))
                         (funcall f (- centr absc))))
              (setf res43 (+ res43 (* fval (f2cl-lib:fref w43b (k) ((1 12))))))
              (setf (f2cl-lib:fref savfun (ipx) ((1 21))) fval)
             label40))
          (setf result (* res43 hlgth))
          (setf abserr (abs (* (- res43 res21) hlgth)))
          (go label65)
         label45
          (setf res87 (* (f2cl-lib:fref w87b (23) ((1 23))) fcentr))
          (setf neval 87)
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k 21) nil)
            (tagbody
              (setf res87
                      (+ res87
                         (* (f2cl-lib:fref savfun (k) ((1 21)))
                            (f2cl-lib:fref w87a (k) ((1 21))))))
             label50))
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k 22) nil)
            (tagbody
              (setf absc (* hlgth (f2cl-lib:fref x4 (k) ((1 22)))))
              (setf res87
                      (+ res87
                         (* (f2cl-lib:fref w87b (k) ((1 23)))
                            (+ (funcall f (+ absc centr))
                               (funcall f (- centr absc))))))
             label60))
          (setf result (* res87 hlgth))
          (setf abserr (abs (* (- res87 res43) hlgth)))
         label65
          (if (and (/= resasc 0.0) (/= abserr 0.0))
              (setf abserr
                      (* resasc
                         (min 1.0 (expt (/ (* 200.0 abserr) resasc) 1.5)))))
          (if (> resabs (/ uflow (* 50.0 epmach)))
              (setf abserr (max (* epmach 50.0 resabs) abserr)))
          (if (<= abserr (max epsabs (* epsrel (abs result)))) (setf ier 0))
          (if (= ier 0) (go label999))
         label70))
     label80
      (xermsg "SLATEC" "DQNG" "ABNORMAL RETURN" ier 0)
     label999
      (go end_label)
     end_label
      (return (values nil nil nil nil nil result abserr neval ier)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dqng fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil fortran-to-lisp::result
                            fortran-to-lisp::abserr fortran-to-lisp::neval
                            fortran-to-lisp::ier)
           :calls '(fortran-to-lisp::xermsg fortran-to-lisp::d1mach))))

