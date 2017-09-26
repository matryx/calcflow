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
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((nbif 0)
      (nbig 0)
      (nbif2 0)
      (nbig2 0)
      (nbip1 0)
      (nbip2 0)
      (x3sml 0.0)
      (x32sml 0.0)
      (xbig 0.0)
      (bifcs
       (make-array 13
                   :element-type 'double-float
                   :initial-contents '(-0.01673021647198665 0.10252335834249446
                                       0.0017083092507381517
                                       1.1862545467744682e-5
                                       4.4932907017792135e-8
                                       1.0698207143387889e-10
                                       1.7480643399771825e-13
                                       2.081023107176171e-16
                                       1.8849814695665417e-19
                                       1.3425779173097804e-22
                                       7.715959342965888e-26
                                       3.653387961747857e-29
                                       1.4497565927953065e-32)))
      (bigcs
       (make-array 13
                   :element-type 'double-float
                   :initial-contents '(0.022466223248574523 0.03736477545301955
                                       4.4476218957212283e-4
                                       2.4708075636329383e-6
                                       7.919135339514964e-9
                                       1.649807985182778e-11
                                       2.4119906664835456e-14
                                       2.6103736236091437e-17
                                       2.1753082977160324e-20
                                       1.4386946400390432e-23
                                       7.734912561208347e-27
                                       3.4469292033849e-30
                                       1.2938919273216e-33)))
      (bif2cs
       (make-array 15
                   :element-type 'double-float
                   :initial-contents '(0.0998457269381604 0.47862497786300556
                                       0.02515521196043301 5.820693885232646e-4
                                       7.499765964437787e-6
                                       6.134602870349384e-8
                                       3.462753885148063e-10
                                       1.4288910080270254e-12
                                       4.496270429833464e-15
                                       1.1142323065833012e-17
                                       2.2304791066175003e-20
                                       3.6815778736393144e-23
                                       5.096086844933826e-26
                                       6.000338692628856e-29
                                       6.082749744657067e-32)))
      (big2cs
       (make-array 15
                   :element-type 'double-float
                   :initial-contents '(0.03330566214551434 0.16130921512319707
                                       0.006319007309613428
                                       1.1879045681625174e-4
                                       1.3045345886200265e-6
                                       9.374125995535217e-9
                                       4.745801886747251e-11
                                       1.783107265094814e-13
                                       5.167591927849581e-16
                                       1.1900450838682712e-18
                                       2.229828806664035e-21
                                       3.465519230276894e-24
                                       4.539263363205045e-27
                                       5.078849965135223e-30
                                       4.910206746965333e-33)))
      (bip1cs
       (make-array 47
                   :element-type 'double-float
                   :initial-contents '(-0.08322047477943448
                                       0.011461189273711743
                                       4.289644071891151e-4
                                       -1.4906639379950513e-4
                                       -1.307659726787629e-5
                                       6.327598396103035e-6
                                       -4.2226696982681925e-7
                                       -1.914718629865469e-7
                                       6.453106284558317e-8
                                       -7.844854677139772e-9
                                       -9.607721662378508e-10
                                       7.000471331644396e-10
                                       -1.7731789132814931e-10
                                       2.2720894783465238e-11
                                       1.654045631397205e-12
                                       -1.8517125559292317e-12
                                       5.957631247711729e-13
                                       -1.2194348147346564e-13
                                       1.3347869253513048e-14
                                       1.7278311524339746e-15
                                       -1.459073201301672e-15
                                       4.901031992711582e-16
                                       -1.1556545519261548e-16
                                       1.909880736707241e-17
                                       -1.176896685449218e-18
                                       -6.327192514953006e-19
                                       3.386183888071536e-19
                                       -1.0725825321758626e-19
                                       2.599570960561717e-20
                                       -4.8477583571081194e-21
                                       5.529891398212162e-22
                                       4.942166082606947e-23
                                       -5.51621219241457e-23
                                       2.143756041763255e-23
                                       -6.19103133876556e-24
                                       1.4629362707391247e-24
                                       -2.7918484471059006e-25
                                       3.645570316857025e-26
                                       5.851182190618871e-28
                                       -2.4946950487566512e-27
                                       1.0979323980338381e-27
                                       -3.4743388345961113e-28
                                       9.13734026353497e-29
                                       -2.0510352728210628e-29
                                       3.797698569854646e-30
                                       -4.847945849775557e-31
                                       -1.0558306941230714e-32)))
      (bip2cs
       (make-array 88
                   :element-type 'double-float
                   :initial-contents '(-0.11359673758598868 0.00413814739478816
                                       1.353470622119333e-4
                                       1.0427316653015353e-5
                                       1.3474954767849909e-6
                                       1.6965374054383983e-7
                                       -1.0096500865641625e-8
                                       -1.6729119493778474e-8
                                       -4.5815364485068385e-9
                                       3.736681366565548e-10
                                       5.766930320145245e-10
                                       6.218126508785033e-11
                                       -6.329412028274307e-11
                                       -1.4915047908598768e-11
                                       7.889621394248677e-12
                                       2.4960513721857797e-12
                                       -1.213007528729166e-12
                                       -3.740493910872728e-13
                                       2.2377278140321477e-13
                                       4.7490296312192465e-14
                                       -4.5261607991821224e-14
                                       -3.0172271841986073e-15
                                       9.105860355875405e-15
                                       -9.814923803380705e-16
                                       -1.6429400647889466e-15
                                       5.533483421427422e-16
                                       2.1750479864482656e-16
                                       -1.7379236200220656e-16
                                       -1.0470023471443715e-18
                                       3.9219145986056385e-17
                                       -1.1621293686345197e-17
                                       -5.402747449175425e-18
                                       4.544158212388461e-18
                                       -2.8775599625221075e-19
                                       -1.0017340927225342e-18
                                       4.482393121506837e-19
                                       7.613596865490894e-20
                                       -1.4448324094881347e-19
                                       4.046085944920536e-20
                                       2.0321085700338447e-20
                                       -1.9602795471446798e-20
                                       3.4273038443944824e-21
                                       3.7023705853905135e-21
                                       -2.687959517204159e-21
                                       2.812167846353171e-22
                                       6.09339636361778e-22
                                       -3.8666621897150843e-22
                                       2.5989331253566943e-23
                                       9.71943936229385e-23
                                       -5.93928178343751e-23
                                       3.8864949977113015e-24
                                       1.5334307393617272e-23
                                       -9.751355520976262e-24
                                       9.634064444048946e-25
                                       2.384199940020888e-24
                                       -1.6896986315019705e-24
                                       2.7352715888928363e-25
                                       3.566001618540958e-25
                                       -3.0234026608258827e-25
                                       7.500204160597394e-26
                                       4.840328757585139e-26
                                       -5.436413765444789e-26
                                       1.9281214470820962e-26
                                       5.0116355020532654e-27
                                       -9.504074458269326e-27
                                       4.637264615710198e-27
                                       2.1177170704466955e-29
                                       -1.5404850268168594e-27
                                       1.0387944293201214e-27
                                       -1.9890078156915416e-28
                                       -2.1022173878658494e-28
                                       2.1353099724525795e-28
                                       -7.904081074796134e-29
                                       -1.6575359960435586e-29
                                       3.886834285012411e-29
                                       -2.2309237330896867e-29
                                       2.777724442017626e-30
                                       5.707854347265773e-30
                                       -5.1743084445303856e-30
                                       1.841328075109584e-30
                                       4.4422562390957095e-31
                                       -9.85041426396298e-31
                                       5.88572013535851e-31
                                       -9.763607544042979e-32
                                       -1.3581011996074696e-31
                                       1.3999743518492413e-31
                                       -5.975490454524848e-32
                                       -4.039165387542831e-33)))
      (atr 8.750690570848434)
      (btr -2.0938363213560542)
      (first$ nil))
  (declare (type (f2cl-lib:integer4) nbif nbig nbif2 nbig2 nbip1 nbip2)
           (type (double-float) x3sml x32sml xbig atr btr)
           (type (simple-array double-float (13)) bifcs bigcs)
           (type (simple-array double-float (15)) bif2cs big2cs)
           (type (simple-array double-float (47)) bip1cs)
           (type (simple-array double-float (88)) bip2cs)
           (type f2cl-lib:logical first$))
  (setq first$ f2cl-lib:%true%)
  (defun dbie (x)
    (declare (type (double-float) x))
    (prog ((sqrtx 0.0) (theta 0.0) (xm 0.0) (z 0.0) (dbie 0.0) (eta 0.0f0))
      (declare (type (single-float) eta)
               (type (double-float) dbie z xm theta sqrtx))
      (cond
        (first$
         (setf eta (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3))))
         (setf nbif (initds bifcs 13 eta))
         (setf nbig (initds bigcs 13 eta))
         (setf nbif2 (initds bif2cs 15 eta))
         (setf nbig2 (initds big2cs 15 eta))
         (setf nbip1 (initds bip1cs 47 eta))
         (setf nbip2 (initds bip2cs 88 eta))
         (setf x3sml (coerce (expt eta 0.3333f0) 'double-float))
         (setf x32sml (* 1.3104 (expt x3sml 2)))
         (setf xbig (expt (f2cl-lib:d1mach 2) 0.6666))))
      (setf first$ f2cl-lib:%false%)
      (if (>= x -1.0) (go label20))
      (multiple-value-bind (var-0 var-1 var-2)
          (d9aimp x xm theta)
        (declare (ignore var-0))
        (setf xm var-1)
        (setf theta var-2))
      (setf dbie (* xm (sin theta)))
      (go end_label)
     label20
      (if (> x 1.0) (go label30))
      (setf z 0.0)
      (if (> (abs x) x3sml) (setf z (expt x 3)))
      (setf dbie
              (+ 0.625
                 (dcsevl z bifcs nbif)
                 (* x (+ 0.4375 (dcsevl z bigcs nbig)))))
      (if (> x x32sml)
          (setf dbie (* dbie (exp (/ (* -2.0 x (f2cl-lib:fsqrt x)) 3.0)))))
      (go end_label)
     label30
      (if (> x 2.0) (go label40))
      (setf z (/ (- (* 2.0 (expt x 3)) 9.0) 7.0))
      (setf dbie
              (* (exp (/ (* -2.0 x (f2cl-lib:fsqrt x)) 3.0))
                 (+ 1.125
                    (dcsevl z bif2cs nbif2)
                    (* x (+ 0.625 (dcsevl z big2cs nbig2))))))
      (go end_label)
     label40
      (if (> x 4.0) (go label50))
      (setf sqrtx (f2cl-lib:fsqrt x))
      (setf z (+ (/ atr (* x sqrtx)) btr))
      (setf dbie (/ (+ 0.625 (dcsevl z bip1cs nbip1)) (f2cl-lib:fsqrt sqrtx)))
      (go end_label)
     label50
      (setf sqrtx (f2cl-lib:fsqrt x))
      (setf z -1.0)
      (if (< x xbig) (setf z (- (/ 16.0 (* x sqrtx)) 1.0)))
      (setf dbie (/ (+ 0.625 (dcsevl z bip2cs nbip2)) (f2cl-lib:fsqrt sqrtx)))
      (go end_label)
     end_label
      (return (values dbie nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dbie fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::dcsevl
                                                     fortran-to-lisp::d9aimp
                                                     fortran-to-lisp::initds
                                                     fortran-to-lisp::d1mach))))

