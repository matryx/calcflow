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


(let ((tols -6.90775527898214)
      (con1 0.666666666666667)
      (con2 0.333333333333333)
      (con548 0.104166666666667)
      (ar
       (make-array 8
                   :element-type 'double-float
                   :initial-contents '(0.0835503472222222 0.128226574556327
                                       0.29184902646414 0.881627267443758
                                       3.32140828186277 14.9957629868626
                                       78.9230130115865 474.451538868264)))
      (br
       (make-array 10
                   :element-type 'double-float
                   :initial-contents '(-0.145833333333333 -0.0987413194444444
                                       -0.143312053915895 -0.317227202678414
                                       -0.94242914795712 -3.51120304082635
                                       -15.727263620368 -82.2814390971859
                                       -492.355370523671 -3316.21856854797)))
      (c
       (make-array 65
                   :element-type 'double-float
                   :initial-contents '(-0.208333333333333 0.125
                                       0.334201388888889 -0.401041666666667
                                       0.0703125 -1.02581259645062
                                       1.84646267361111 -0.8912109375
                                       0.0732421875 4.66958442342625
                                       -11.207002616223 8.78912353515625
                                       -2.3640869140625 0.112152099609375
                                       -28.2120725582002 84.6362176746007
                                       -91.81824154324 42.5349987453885
                                       -7.36879435947963 0.227108001708984
                                       212.570130039217 -765.252468141182
                                       1059.990452528 -699.579627376133
                                       218.190511744212 -26.4914304869516
                                       0.572501420974731 -1919.45766231841
                                       8061.72218173731 -13586.5500064341
                                       11655.3933368645 -5305.6469786134
                                       1200.90291321635 -108.090919788395
                                       1.72772750258446 20204.2913309661
                                       -96980.5983886375 192547.001232532
                                       -203400.177280416 122200.464983017
                                       -41192.6549688976 7109.51430248936
                                       -493.915304773088 6.07404200127348
                                       -242919.187900551 1311763.61466298
                                       -2998015.91853811 3763271.2976564
                                       -2813563.22658653 1268365.27332162
                                       -331645.172484564 45218.7689813627
                                       -2499.83048181121 24.3805296995561
                                       3284469.85307204 -1.97068191184322e7
                                       5.09526024926646e7 -7.41051482115327e7
                                       6.6344512274729e7 -3.75671766607634e7
                                       1.32887671664218e7 -2785618.12808645
                                       308186.404612662 -13886.089753717
                                       110.017140269247)))
      (alfa
       (make-array 104
                   :element-type 'double-float
                   :initial-contents '(-0.00444444444444444
                                       -9.22077922077922e-4
                                       -8.84892884892885e-5 1.6592768783245e-4
                                       2.46691372741793e-4 2.65995589346255e-4
                                       2.61824297061501e-4 2.48730437344656e-4
                                       2.32721040083232e-4 2.16362485712365e-4
                                       2.00738858762752e-4 1.86267636637545e-4
                                       1.73060775917876e-4 1.61091705929016e-4
                                       1.50274774160908e-4 1.4050349739127e-4
                                       1.31668816545923e-4 1.23667445598253e-4
                                       1.16405271474738e-4 1.09798298372713e-4
                                       1.03772410422993e-4 9.82626078369363e-5
                                       9.32120517249503e-5 8.85710852478712e-5
                                       8.429631057157e-5 8.03497548407791e-5
                                       6.93735541354589e-4 2.32241745182922e-4
                                       -1.41986273556691e-5
                                       -1.16444931672049e-4
                                       -1.50803558053049e-4
                                       -1.55121924918096e-4
                                       -1.46809756646466e-4
                                       -1.33815503867491e-4
                                       -1.19744975684254e-4
                                       -1.06184319207974e-4
                                       -9.37699549891194e-5
                                       -8.26923045588193e-5
                                       -7.29374348155221e-5
                                       -6.44042357721016e-5
                                       -5.69611566009369e-5
                                       -5.04731044303562e-5
                                       -4.48134868008883e-5
                                       -3.98688727717599e-5
                                       -3.55400532972042e-5
                                       -3.17414256609022e-5
                                       -2.83996793904175e-5
                                       -2.54522720634871e-5
                                       -2.28459297164725e-5
                                       -2.05352753106481e-5
                                       -1.84816217627666e-5
                                       -1.66519330021394e-5
                                       -3.54211971457744e-4
                                       -1.56161263945159e-4 3.04465503594936e-5
                                       1.30198655773243e-4 1.67471106699712e-4
                                       1.70222587683593e-4 1.56501427608595e-4
                                       1.36339170977445e-4 1.14886692029825e-4
                                       9.45869093034688e-5 7.64498419250898e-5
                                       6.07570334965197e-5 4.74394299290509e-5
                                       3.62757512005344e-5 2.69939714979225e-5
                                       1.93210938247939e-5 1.30056674793963e-5
                                       7.82620866744497e-6 3.59257485819352e-6
                                       1.44040049814252e-7 -2.65396769697939e-6
                                       -4.91346867098486e-6
                                       -6.72739296091248e-6
                                       -8.17269379678658e-6
                                       -9.31304715093561e-6
                                       -1.02011418798016e-5 3.78194199201773e-4
                                       2.02471952761816e-4 -6.37938506318862e-5
                                       -2.38598230603006e-4
                                       -3.10916256027362e-4
                                       -3.13680115247576e-4
                                       -2.78950273791323e-4
                                       -2.28564082619141e-4
                                       -1.75245280340847e-4 -1.2554406306069e-4
                                       -8.22982872820208e-5
                                       -4.62860730588116e-5
                                       -1.72334302366962e-5 5.60690482304602e-6
                                       2.31395443148287e-5 3.62642745856794e-5
                                       4.58006124490189e-5 5.24595294959114e-5
                                       5.68396208545815e-5 5.94349820393104e-5
                                       6.06478527578422e-5 6.08023907788436e-5
                                       6.0157789453946e-5 5.89199657344698e-5
                                       5.72515823777593e-5
                                       5.52804375585853e-5)))
      (beta
       (make-array 130
                   :element-type 'double-float
                   :initial-contents '(0.0179988721413553 0.00559964911064388
                                       0.00288501402231133 0.00180096606761054
                                       0.00124753110589199 9.22878876572938e-4
                                       7.14430421727287e-4 5.71787281789705e-4
                                       4.69431007606482e-4 3.93232835462917e-4
                                       3.34818889318298e-4 2.88952148495752e-4
                                       2.52211615549573e-4 2.22280580798883e-4
                                       1.97541838033063e-4 1.76836855019718e-4
                                       1.59316899661821e-4 1.44347930197334e-4
                                       1.31448068119965e-4 1.20245444949303e-4
                                       1.10449144504599e-4 1.01828770740567e-4
                                       9.41998224204238e-5 8.74130545753834e-5
                                       8.13466262162801e-5 7.59002269646219e-5
                                       -0.00149282953213429
                                       -8.78204709546389e-4
                                       -5.02916549572035e-4
                                       -2.94822138512746e-4
                                       -1.75463996970783e-4
                                       -1.04008550460816e-4
                                       -5.96141953046458e-5
                                       -3.12038929076098e-5 -1.2608973598023e-5
                                       -2.4289260857573e-7 8.05996165414274e-6
                                       1.36507009262147e-5 1.73964125472926e-5
                                       1.98672978842134e-5 2.14463263790823e-5
                                       2.23954659232457e-5 2.28967783814713e-5
                                       2.30785389811178e-5 2.30321976080909e-5
                                       2.28236073720349e-5 2.25005881105292e-5
                                       2.20981015361991e-5 2.16418427448104e-5
                                       2.11507649256221e-5 2.06388749782171e-5
                                       2.01165241997082e-5 5.52213076721293e-4
                                       4.47932581552385e-4 2.79520653992021e-4
                                       1.52468156198447e-4 6.93271105657044e-5
                                       1.76258683069991e-5 -1.35744996343269e-5
                                       -3.17972413350427e-5
                                       -4.18861861696693e-5
                                       -4.69004889379141e-5
                                       -4.87665447413787e-5
                                       -4.87010031186735e-5
                                       -4.74755620890087e-5
                                       -4.55813058138628e-5
                                       -4.33309644511266e-5 -4.0923019315775e-5
                                       -3.84822638603221e-5
                                       -3.60857167535411e-5
                                       -3.37793306123367e-5 -3.1588856077211e-5
                                       -2.95269561750807e-5
                                       -2.75978914828336e-5
                                       -2.58006174666884e-5 -2.4130835676128e-5
                                       -2.25823509518346e-5
                                       -2.11479656768913e-5 -4.7461779655996e-4
                                       -4.77864567147321e-4
                                       -3.20390228067038e-4
                                       -1.61105016119962e-4
                                       -4.25778101285435e-5 3.44571294294968e-5
                                       7.97092684075675e-5 1.03138236708272e-4
                                       1.12466775262204e-4 1.13103642108481e-4
                                       1.08651634848774e-4 1.01437951597662e-4
                                       9.29298396593364e-5 8.4029313301609e-5
                                       7.52727991349134e-5 6.69632521975731e-5
                                       5.92564547323195e-5 5.22169308826976e-5
                                       4.58539485165361e-5 4.01445513891487e-5
                                       3.50481730031328e-5 3.05157995034347e-5
                                       2.64956119950516e-5 2.29363633690998e-5
                                       1.97893056664022e-5 1.70091984636413e-5
                                       7.36465810572578e-4 8.72790805146194e-4
                                       6.22614862573135e-4 2.85998154194304e-4
                                       3.84737672879366e-6 -1.87906003636972e-4
                                       -2.97603646594555e-4
                                       -3.45998126832656e-4
                                       -3.53382470916038e-4
                                       -3.35715635775049e-4 -3.0432112478904e-4
                                       -2.66722723047613e-4 -2.2765421412282e-4
                                       -1.89922611854562e-4
                                       -1.55058918599094e-4
                                       -1.23778240761874e-4
                                       -9.62926147717644e-5
                                       -7.25178327714425e-5
                                       -5.22070028895634e-5
                                       -3.50347750511901e-5
                                       -2.06489761035552e-5
                                       -8.70106096849767e-6 1.136986866751e-6
                                       9.16426474122779e-6 1.56477785428873e-5
                                       2.08223629482467e-5)))
      (gama
       (make-array 26
                   :element-type 'double-float
                   :initial-contents '(0.629960524947437 0.251984209978975
                                       0.154790300415656 0.110713062416159
                                       0.0857309395527395 0.0697161316958684
                                       0.0586085671893714 0.0504698873536311
                                       0.0442600580689155 0.039372066154351
                                       0.0354283195924455 0.0321818857502098
                                       0.0294646240791158 0.0271581677112934
                                       0.0251768272973862 0.0234570755306079
                                       0.0219508390134907 0.0206210828235646
                                       0.0194388240897881 0.0183810633800683
                                       0.0174293213231963 0.0165685837786612
                                       0.0157865285987918 0.0150729501494096
                                       0.0144193250839955 0.0138184805735342))))
  (declare (type (double-float) tols con1 con2 con548)
           (type (array double-float (8)) ar)
           (type (array double-float (10)) br)
           (type (array double-float (65)) c)
           (type (array double-float (104)) alfa)
           (type (array double-float (130)) beta)
           (type (array double-float (26)) gama))
  (defun dasyjy (funjy x fnu flgjy in y wk iflw)
    (declare (type (array double-float (*)) wk y)
             (type (f2cl-lib:integer4) iflw in)
             (type (double-float) flgjy fnu x))
    (f2cl-lib:with-multi-array-data
        ((y double-float y-%data% y-%offset%)
         (wk double-float wk-%data% wk-%offset%))
      (prog ((cr (make-array 10 :element-type 'double-float))
             (dr (make-array 10 :element-type 'double-float))
             (kmax (make-array 5 :element-type 'f2cl-lib:integer4))
             (upol (make-array 10 :element-type 'double-float)) (abw2 0.0)
             (akm 0.0) (ap 0.0) (asum 0.0) (az 0.0) (bsum 0.0) (crz32 0.0)
             (dfi 0.0) (elim 0.0) (fi 0.0) (fn 0.0) (fn2 0.0) (phi 0.0)
             (rcz 0.0) (rden 0.0) (relb 0.0) (rfn2 0.0) (rtz 0.0) (rzden 0.0)
             (sa 0.0) (sb 0.0) (suma 0.0) (sumb 0.0) (s1 0.0) (ta 0.0)
             (tau 0.0) (tb 0.0) (tfn 0.0) (tol 0.0) (t2 0.0) (xx 0.0) (z 0.0)
             (z32 0.0) (i 0) (j 0) (jn 0) (jr 0) (ju 0) (k 0) (kb 0) (klast 0)
             (kp1 0) (ks 0) (ksp1 0) (kstemp 0) (l 0) (lr 0) (lrp1 0) (iseta 0)
             (isetb 0))
        (declare (type (array f2cl-lib:integer4 (5)) kmax)
                 (type (f2cl-lib:integer4) isetb iseta lrp1 lr l kstemp ksp1 ks
                                           kp1 klast kb k ju jr jn j i)
                 (type (array double-float (10)) upol dr cr)
                 (type (double-float) z32 z xx t2 tol tfn tb tau ta s1 sumb
                                      suma sb sa rzden rtz rfn2 relb rden rcz
                                      phi fn2 fn fi elim dfi crz32 bsum az asum
                                      ap akm abw2))
        (setf ta (f2cl-lib:d1mach 3))
        (setf tol (max ta 1.0e-15))
        (setf tb (f2cl-lib:d1mach 5))
        (setf ju (f2cl-lib:i1mach 15))
        (if (= flgjy 1.0) (go label6))
        (setf jr (f2cl-lib:i1mach 14))
        (setf elim (* -2.303 tb (f2cl-lib:int-add ju jr)))
        (go label7)
       label6
        (setf elim (* -2.303 (+ (* tb ju) 3.0)))
       label7
        (setf fn fnu)
        (setf iflw 0)
        (f2cl-lib:fdo (jn 1 (f2cl-lib:int-add jn 1))
                      ((> jn in) nil)
          (tagbody
            (setf xx (/ x fn))
            (setf (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%)
                    (- 1.0 (* xx xx)))
            (setf abw2 (abs (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%)))
            (setf (f2cl-lib:fref wk-%data% (2) ((1 *)) wk-%offset%)
                    (f2cl-lib:fsqrt abw2))
            (setf (f2cl-lib:fref wk-%data% (7) ((1 *)) wk-%offset%)
                    (expt fn con2))
            (if (> abw2 0.2775) (go label80))
            (setf sa 0.0)
            (if (= abw2 0.0) (go label10))
            (setf sa (/ tols (f2cl-lib:flog abw2)))
           label10
            (setf sb sa)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 5) nil)
              (tagbody
                (setf akm (max sa 2.0))
                (setf (f2cl-lib:fref kmax (i) ((1 5))) (f2cl-lib:int akm))
                (setf sa (+ sa sb))
               label20))
            (setf kb (f2cl-lib:fref kmax (5) ((1 5))))
            (setf klast (f2cl-lib:int-sub kb 1))
            (setf sa (f2cl-lib:fref gama (kb) ((1 26))))
            (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                          ((> k klast) nil)
              (tagbody
                (setf kb (f2cl-lib:int-sub kb 1))
                (setf sa
                        (+
                         (* sa
                            (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%))
                         (f2cl-lib:fref gama (kb) ((1 26)))))
               label30))
            (setf z (* (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%) sa))
            (setf az (abs z))
            (setf rtz (f2cl-lib:fsqrt az))
            (setf (f2cl-lib:fref wk-%data% (3) ((1 *)) wk-%offset%)
                    (* con1 az rtz))
            (setf (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%)
                    (* (f2cl-lib:fref wk-%data% (3) ((1 *)) wk-%offset%) fn))
            (setf (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)
                    (* rtz (f2cl-lib:fref wk-%data% (7) ((1 *)) wk-%offset%)))
            (setf (f2cl-lib:fref wk-%data% (6) ((1 *)) wk-%offset%)
                    (* (- (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%))
                       (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)))
            (if (<= z 0.0) (go label35))
            (if (> (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%) elim)
                (go label75))
            (setf (f2cl-lib:fref wk-%data% (6) ((1 *)) wk-%offset%)
                    (- (f2cl-lib:fref wk-%data% (6) ((1 *)) wk-%offset%)))
           label35
            (setf phi (f2cl-lib:fsqrt (f2cl-lib:fsqrt (+ sa sa sa sa))))
            (setf kb (f2cl-lib:fref kmax (5) ((1 5))))
            (setf klast (f2cl-lib:int-sub kb 1))
            (setf sb (f2cl-lib:fref beta (kb 1) ((1 26) (1 5))))
            (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                          ((> k klast) nil)
              (tagbody
                (setf kb (f2cl-lib:int-sub kb 1))
                (setf sb
                        (+
                         (* sb
                            (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%))
                         (f2cl-lib:fref beta (kb 1) ((1 26) (1 5)))))
               label40))
            (setf ksp1 1)
            (setf fn2 (* fn fn))
            (setf rfn2 (/ 1.0 fn2))
            (setf rden 1.0)
            (setf asum 1.0)
            (setf relb (* tol (abs sb)))
            (setf bsum sb)
            (f2cl-lib:fdo (ks 1 (f2cl-lib:int-add ks 1))
                          ((> ks 4) nil)
              (tagbody
                (setf ksp1 (f2cl-lib:int-add ksp1 1))
                (setf rden (* rden rfn2))
                (setf kstemp (f2cl-lib:int-sub 5 ks))
                (setf kb (f2cl-lib:fref kmax (kstemp) ((1 5))))
                (setf klast (f2cl-lib:int-sub kb 1))
                (setf sa (f2cl-lib:fref alfa (kb ks) ((1 26) (1 4))))
                (setf sb (f2cl-lib:fref beta (kb ksp1) ((1 26) (1 5))))
                (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                              ((> k klast) nil)
                  (tagbody
                    (setf kb (f2cl-lib:int-sub kb 1))
                    (setf sa
                            (+
                             (* sa
                                (f2cl-lib:fref wk-%data%
                                               (1)
                                               ((1 *))
                                               wk-%offset%))
                             (f2cl-lib:fref alfa (kb ks) ((1 26) (1 4)))))
                    (setf sb
                            (+
                             (* sb
                                (f2cl-lib:fref wk-%data%
                                               (1)
                                               ((1 *))
                                               wk-%offset%))
                             (f2cl-lib:fref beta (kb ksp1) ((1 26) (1 5)))))
                   label50))
                (setf ta (* sa rden))
                (setf tb (* sb rden))
                (setf asum (+ asum ta))
                (setf bsum (+ bsum tb))
                (if (and (<= (abs ta) tol) (<= (abs tb) relb)) (go label70))
               label60))
           label70
            (setf bsum
                    (/ bsum
                       (* fn
                          (f2cl-lib:fref wk-%data% (7) ((1 *)) wk-%offset%))))
            (go label160)
           label75
            (setf iflw 1)
            (go end_label)
           label80
            (setf (f2cl-lib:fref upol (1) ((1 10))) 1.0)
            (setf tau
                    (/ 1.0 (f2cl-lib:fref wk-%data% (2) ((1 *)) wk-%offset%)))
            (setf t2 (/ 1.0 (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%)))
            (if (>= (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%) 0.0)
                (go label90))
            (setf (f2cl-lib:fref wk-%data% (3) ((1 *)) wk-%offset%)
                    (abs
                     (- (f2cl-lib:fref wk-%data% (2) ((1 *)) wk-%offset%)
                        (atan
                         (f2cl-lib:fref wk-%data% (2) ((1 *)) wk-%offset%)))))
            (setf (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%)
                    (* (f2cl-lib:fref wk-%data% (3) ((1 *)) wk-%offset%) fn))
            (setf rcz
                    (/ (- con1)
                       (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%)))
            (setf z32
                    (* 1.5 (f2cl-lib:fref wk-%data% (3) ((1 *)) wk-%offset%)))
            (setf rtz (expt z32 con2))
            (setf (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)
                    (* rtz (f2cl-lib:fref wk-%data% (7) ((1 *)) wk-%offset%)))
            (setf (f2cl-lib:fref wk-%data% (6) ((1 *)) wk-%offset%)
                    (* (- (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%))
                       (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)))
            (go label100)
           label90
            (setf (f2cl-lib:fref wk-%data% (3) ((1 *)) wk-%offset%)
                    (abs
                     (-
                      (f2cl-lib:flog
                       (/
                        (+ 1.0
                           (f2cl-lib:fref wk-%data% (2) ((1 *)) wk-%offset%))
                        xx))
                      (f2cl-lib:fref wk-%data% (2) ((1 *)) wk-%offset%))))
            (setf (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%)
                    (* (f2cl-lib:fref wk-%data% (3) ((1 *)) wk-%offset%) fn))
            (setf rcz
                    (/ con1 (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%)))
            (if (> (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%) elim)
                (go label75))
            (setf z32
                    (* 1.5 (f2cl-lib:fref wk-%data% (3) ((1 *)) wk-%offset%)))
            (setf rtz (expt z32 con2))
            (setf (f2cl-lib:fref wk-%data% (7) ((1 *)) wk-%offset%)
                    (expt fn con2))
            (setf (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)
                    (* rtz (f2cl-lib:fref wk-%data% (7) ((1 *)) wk-%offset%)))
            (setf (f2cl-lib:fref wk-%data% (6) ((1 *)) wk-%offset%)
                    (* (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)
                       (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)))
           label100
            (setf phi (f2cl-lib:fsqrt (* (+ rtz rtz) tau)))
            (setf tb 1.0)
            (setf asum 1.0)
            (setf tfn (/ tau fn))
            (setf rden (/ 1.0 fn))
            (setf rfn2 (* rden rden))
            (setf rden 1.0)
            (setf (f2cl-lib:fref upol (2) ((1 10)))
                    (*
                     (+ (* (f2cl-lib:fref c (1) ((1 65))) t2)
                        (f2cl-lib:fref c (2) ((1 65))))
                     tfn))
            (setf crz32 (* con548 rcz))
            (setf bsum (+ (f2cl-lib:fref upol (2) ((1 10))) crz32))
            (setf relb (* tol (abs bsum)))
            (setf ap tfn)
            (setf ks 0)
            (setf kp1 2)
            (setf rzden rcz)
            (setf l 2)
            (setf iseta 0)
            (setf isetb 0)
            (f2cl-lib:fdo (lr 2 (f2cl-lib:int-add lr 2))
                          ((> lr 8) nil)
              (tagbody
                (setf lrp1 (f2cl-lib:int-add lr 1))
                (f2cl-lib:fdo (k lr (f2cl-lib:int-add k 1))
                              ((> k lrp1) nil)
                  (tagbody
                    (setf ks (f2cl-lib:int-add ks 1))
                    (setf kp1 (f2cl-lib:int-add kp1 1))
                    (setf l (f2cl-lib:int-add l 1))
                    (setf s1 (f2cl-lib:fref c (l) ((1 65))))
                    (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                                  ((> j kp1) nil)
                      (tagbody
                        (setf l (f2cl-lib:int-add l 1))
                        (setf s1 (+ (* s1 t2) (f2cl-lib:fref c (l) ((1 65)))))
                       label110))
                    (setf ap (* ap tfn))
                    (setf (f2cl-lib:fref upol (kp1) ((1 10))) (* ap s1))
                    (setf (f2cl-lib:fref cr (ks) ((1 10)))
                            (* (f2cl-lib:fref br (ks) ((1 10))) rzden))
                    (setf rzden (* rzden rcz))
                    (setf (f2cl-lib:fref dr (ks) ((1 10)))
                            (* (f2cl-lib:fref ar (ks) ((1 8))) rzden))
                   label120))
                (setf suma (f2cl-lib:fref upol (lrp1) ((1 10))))
                (setf sumb
                        (+
                         (f2cl-lib:fref upol
                                        ((f2cl-lib:int-add lr 2))
                                        ((1 10)))
                         (* (f2cl-lib:fref upol (lrp1) ((1 10))) crz32)))
                (setf ju lrp1)
                (f2cl-lib:fdo (jr 1 (f2cl-lib:int-add jr 1))
                              ((> jr lr) nil)
                  (tagbody
                    (setf ju (f2cl-lib:int-sub ju 1))
                    (setf suma
                            (+ suma
                               (* (f2cl-lib:fref cr (jr) ((1 10)))
                                  (f2cl-lib:fref upol (ju) ((1 10))))))
                    (setf sumb
                            (+ sumb
                               (* (f2cl-lib:fref dr (jr) ((1 10)))
                                  (f2cl-lib:fref upol (ju) ((1 10))))))
                   label130))
                (setf rden (* rden rfn2))
                (setf tb (- tb))
                (if (> (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%) 0.0)
                    (setf tb (abs tb)))
                (if (< rden tol) (go label131))
                (setf asum (+ asum (* suma tb)))
                (setf bsum (+ bsum (* sumb tb)))
                (go label140)
               label131
                (if (= iseta 1) (go label132))
                (if (< (abs suma) tol) (setf iseta 1))
                (setf asum (+ asum (* suma tb)))
               label132
                (if (= isetb 1) (go label133))
                (if (< (abs sumb) relb) (setf isetb 1))
                (setf bsum (+ bsum (* sumb tb)))
               label133
                (if (and (= iseta 1) (= isetb 1)) (go label150))
               label140))
           label150
            (setf tb (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%))
            (if (> (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%) 0.0)
                (setf tb (- tb)))
            (setf bsum (/ bsum tb))
           label160
            (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                (funcall funjy
                         (f2cl-lib:fref wk-%data% (6) ((1 *)) wk-%offset%)
                         (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)
                         (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%)
                         fi
                         dfi)
              (declare (ignore))
              (when var-0
                (f2cl-lib:fset
                 (f2cl-lib:fref wk-%data% (6) ((1 *)) wk-%offset%)
                 var-0))
              (when var-1
                (f2cl-lib:fset
                 (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)
                 var-1))
              (when var-2
                (f2cl-lib:fset
                 (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%)
                 var-2))
              (when var-3
                (setf fi var-3))
              (when var-4
                (setf dfi var-4)))
            (setf ta (/ 1.0 tol))
            (setf tb (* (f2cl-lib:d1mach 1) ta 1000.0))
            (if (> (abs fi) tb) (go label165))
            (setf fi (* fi ta))
            (setf dfi (* dfi ta))
            (setf phi (* phi tol))
           label165
            (setf (f2cl-lib:fref y-%data% (jn) ((1 *)) y-%offset%)
                    (/ (* flgjy phi (+ (* fi asum) (* dfi bsum)))
                       (f2cl-lib:fref wk-%data% (7) ((1 *)) wk-%offset%)))
            (setf fn (- fn flgjy))
           label170))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil iflw))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dasyjy
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil fortran-to-lisp::iflw)
           :calls '(fortran-to-lisp::i1mach fortran-to-lisp::d1mach))))

