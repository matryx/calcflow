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


(let ((n1 20)
      (n2 19)
      (n3 14)
      (m1 18)
      (m2 17)
      (m3 12)
      (n1d 21)
      (n2d 20)
      (n3d 19)
      (n4d 14)
      (m1d 19)
      (m2d 18)
      (m3d 17)
      (m4d 12)
      (fpi12 1.30899693899575)
      (spi12 1.83259571459405)
      (con1 0.666666666666667)
      (con2 7.74148278841779)
      (con3 0.364766105490356)
      (bk1
       (make-array 20
                   :element-type 'double-float
                   :initial-contents '(2.43202846447449 2.57132009754685
                                       1.02802341258616 0.341958178205872
                                       0.0841978629889284 0.0193877282587962
                                       0.00392687837130335 6.83302689948043e-4
                                       1.14611403991141e-4 1.74195138337086e-5
                                       2.41223620956355e-6 3.24525591983273e-7
                                       4.03509798540183e-8 4.70875059642296e-9
                                       5.35367432585889e-10
                                       5.70606721846334e-11
                                       5.80526363709933e-12
                                       5.76338988616388e-13
                                       5.42103834518071e-14
                                       4.91857330301677e-15)))
      (bk2
       (make-array 20
                   :element-type 'double-float
                   :initial-contents '(0.574830555784088 -0.00691648648376891
                                       0.00197460263052093 -5.24043043868823e-4
                                       1.22965147239661e-4 -2.27059514462173e-5
                                       2.23575555008526e-6 4.15174955023899e-7
                                       -2.84985752198231e-7 8.50187174775435e-8
                                       -1.70400826891326e-8 2.25479746746889e-9
                                       -1.09524166577443e-10
                                       -3.41063845099711e-11
                                       1.11262893886662e-11
                                       -1.75542944241734e-12
                                       1.36298600401767e-13
                                       8.76342105755664e-15
                                       -4.64063099157041e-15
                                       7.7877275873296e-16)))
      (bk3
       (make-array 20
                   :element-type 'double-float
                   :initial-contents '(0.566777053506912 0.00263672828349579
                                       5.1230335147313e-5 2.10229231564492e-6
                                       1.4221709511389e-7 1.28534295891264e-8
                                       7.28556219407507e-10
                                       -3.45236157301011e-10
                                       -2.11919115912724e-10
                                       -6.56803892922376e-11
                                       -8.14873160315074e-12
                                       3.03177845632183e-12
                                       1.73447220554115e-12
                                       1.67935548701554e-13
                                       -1.49622868806719e-13
                                       -5.15470458953407e-14
                                       8.7574184185783e-15 7.9673555352572e-15
                                       -1.29566137861742e-16
                                       -1.1187879441752e-15)))
      (bk4
       (make-array 14
                   :element-type 'double-float
                   :initial-contents '(0.485444386705114 -0.00308525088408463
                                       6.98748404837928e-5 -2.82757234179768e-6
                                       1.59553313064138e-7 -1.12980692144601e-8
                                       9.47671515498754e-10
                                       -9.08301736026423e-11
                                       9.70776206450724e-12
                                       -1.13687527254574e-12
                                       1.43982917533415e-13
                                       -1.95211019558815e-14
                                       2.81056379909357e-15
                                       -4.26916444775176e-16)))
      (bjp
       (make-array 19
                   :element-type 'double-float
                   :initial-contents '(0.134918611457638 -0.319314588205813
                                       0.0522061946276114 0.0528869112170312
                                       -0.0085810075607735 -0.00299211002025555
                                       4.21126741969759e-4 8.73931830369273e-5
                                       -1.06749163477533e-5
                                       -1.56575097259349e-6 1.68051151983999e-7
                                       1.89901103638691e-8 -1.81374004961922e-9
                                       -1.66339134593739e-10
                                       1.4295633578081e-11 1.10179811626595e-12
                                       -8.60187724192263e-14
                                       -5.71248177285064e-15
                                       4.08414552853803e-16)))
      (bjn
       (make-array 19
                   :element-type 'double-float
                   :initial-contents '(0.0659041673525697 -0.424905910566004
                                       0.28720974519583 0.129787771099606
                                       -0.0456354317590358 -0.010263017598254
                                       0.00250704671521101 3.78127183743483e-4
                                       -7.11287583284084e-5
                                       -8.08651210688923e-6 1.23879531273285e-6
                                       1.13096815867279e-7 -1.4623428317631e-8
                                       -1.11576315688077e-9
                                       1.24846618243897e-10
                                       8.18334132555274e-12
                                       -8.07174877048484e-13
                                       -4.63778618766425e-14
                                       4.09043399081631e-15)))
      (aa
       (make-array 14
                   :element-type 'double-float
                   :initial-contents '(-0.278593552803079 0.00352915691882584
                                       2.31149677384994e-5 -4.7131784226356e-6
                                       1.12415907931333e-7 2.00100301184339e-8
                                       -2.60948075302193e-9
                                       3.55098136101216e-11
                                       3.50849978423875e-11
                                       -5.83007187954202e-12
                                       2.04644828753326e-13
                                       1.10529179476742e-13
                                       -2.87724778038775e-14
                                       2.88205111009939e-15)))
      (bb
       (make-array 14
                   :element-type 'double-float
                   :initial-contents '(-0.490275424742791 -0.00157647277946204
                                       9.66195963140306e-5 -1.35916080268815e-7
                                       -2.98157342654859e-7 1.86824767559979e-8
                                       1.03685737667141e-9
                                       -3.28660818434328e-10
                                       2.5709141063278e-11 2.32357655300677e-12
                                       -9.57523279048255e-13
                                       1.20340828049719e-13
                                       2.90907716770715e-15
                                       -4.55656454580149e-15)))
      (dbk1
       (make-array 21
                   :element-type 'double-float
                   :initial-contents '(2.95926143981893 3.86774568440103
                                       1.80441072356289 0.578070764125328
                                       0.163011468174708 0.0392044409961855
                                       0.00790964210433812 0.00150640863167338
                                       2.56651976920042e-4 3.93826605867715e-5
                                       5.81097771463818e-6 7.86881233754659e-7
                                       9.93272957325739e-8 1.21424205575107e-8
                                       1.38528332697707e-9 1.50190067586758e-10
                                       1.58271945457594e-11
                                       1.57531847699042e-12
                                       1.50774055398181e-13
                                       1.40594335806564e-14
                                       1.24942698777218e-15)))
      (dbk2
       (make-array 20
                   :element-type 'double-float
                   :initial-contents '(0.549756809432471 0.00913556983276901
                                       -0.00253635048605507 6.60423795342054e-4
                                       -1.55217243135416e-4 3.00090325448633e-5
                                       -3.76454339467348e-6
                                       -1.33291331611616e-7 2.42587371049013e-7
                                       -8.07861075240228e-8 1.71092818861193e-8
                                       -2.41087357570599e-9
                                       1.53910848162371e-10 2.5646537319063e-11
                                       -9.88581911653212e-12
                                       1.60877986412631e-12
                                       -1.20952524741739e-13
                                       -1.0697827841082e-14
                                       5.02478557067561e-15
                                       -8.68986130935886e-16)))
      (dbk3
       (make-array 20
                   :element-type 'double-float
                   :initial-contents '(0.560598509354302 -0.00364870013248135
                                       -5.98147152307417e-5
                                       -2.33611595253625e-6
                                       -1.64571516521436e-7
                                       -2.06333012920569e-8 -4.2774543157311e-9
                                       -1.08494137799276e-9
                                       -2.37207188872763e-10
                                       -2.22132920864966e-11
                                       1.07238008032138e-11
                                       5.71954845245808e-12
                                       7.51102737777835e-13
                                       -3.81912369483793e-13
                                       -1.75870057119257e-13
                                       6.69641694419084e-15
                                       2.26866724792055e-14
                                       2.69898141356743e-15
                                       -2.67133612397359e-15
                                       -6.54121403165269e-16)))
      (dbk4
       (make-array 14
                   :element-type 'double-float
                   :initial-contents '(0.493072999188036 0.00438335419803815
                                       -8.37413882246205e-5 3.20268810484632e-6
                                       -1.7566197954827e-7 1.22269906524508e-8
                                       -1.01381314366052e-9
                                       9.63639784237475e-11
                                       -1.02344993379648e-11
                                       1.19264576554355e-12
                                       -1.50443899103287e-13
                                       2.03299052379349e-14
                                       -2.91890652008292e-15
                                       4.42322081975475e-16)))
      (dbjp
       (make-array 19
                   :element-type 'double-float
                   :initial-contents '(0.113140872390745 -0.208301511416328
                                       0.0169396341953138 0.0290895212478621
                                       -0.00341467131311549
                                       -0.00146455339197417 1.63313272898517e-4
                                       3.91145328922162e-5 -3.96757190808119e-6
                                       -6.51846913772395e-7 5.9870749526928e-8
                                       7.44108654536549e-9
                                       -6.21241056522632e-10
                                       -6.18768017313526e-11
                                       4.72323484752324e-12
                                       3.91652459802532e-13
                                       -2.74985937845226e-14
                                       -1.9503649776275e-15
                                       1.26669643809444e-16)))
      (dbjn
       (make-array 19
                   :element-type 'double-float
                   :initial-contents '(-0.018809126006885 -0.14779818082614
                                       0.546075900433171 0.152146932663116
                                       -0.0958260412266886 -0.016310273169613
                                       0.00575364806680105 7.12145408252655e-4
                                       -1.75452116846724e-4
                                       -1.71063171685128e-5 3.2443558063168e-6
                                       2.61190663932884e-7 -4.03026865912779e-8
                                       -2.76435165853895e-9
                                       3.59687929062312e-10
                                       2.14953308456051e-11
                                       -2.41849311903901e-12
                                       -1.28068004920751e-13
                                       1.26939834401773e-14)))
      (daa
       (make-array 14
                   :element-type 'double-float
                   :initial-contents '(0.277571356944231 -0.0044421283341992
                                       8.42328522190089e-5 2.5804031841871e-6
                                       -3.42389720217621e-7 6.24286894709776e-9
                                       2.36377836844577e-9
                                       -3.16991042656673e-10
                                       4.40995691658191e-12
                                       5.18674221093575e-12
                                       -9.64874015137022e-13
                                       4.9019057660871e-14 1.77253430678112e-14
                                       -5.55950610442662e-15)))
      (dbb
       (make-array 14
                   :element-type 'double-float
                   :initial-contents '(0.491627321104601 0.00311164930427489
                                       8.23140762854081e-5 -4.61769776172142e-6
                                       -6.13158880534626e-8 2.8729580465652e-8
                                       -1.81959715372117e-9
                                       -1.44752826642035e-10
                                       4.53724043420422e-11
                                       -3.99655065847223e-12
                                       -3.24089119830323e-13
                                       1.62098952568741e-13
                                       -2.40765247974057e-14
                                       1.69384811284491e-16))))
  (declare (type (f2cl-lib:integer4) n1 n2 n3 m1 m2 m3 n1d n2d n3d n4d m1d m2d
                                     m3d m4d)
           (type (double-float) fpi12 spi12 con1 con2 con3)
           (type (simple-array double-float (20)) bk1 bk2 bk3 dbk2 dbk3)
           (type (simple-array double-float (14)) bk4 aa bb dbk4 daa dbb)
           (type (simple-array double-float (19)) bjp bjn dbjp dbjn)
           (type (simple-array double-float (21)) dbk1))
  (defun dyairy (x rx c bi dbi)
    (declare (type (double-float) dbi bi c rx x))
    (prog ((ax 0.0) (cv 0.0) (d1 0.0) (d2 0.0) (ex 0.0) (e1 0.0) (e2 0.0)
           (f1 0.0) (f2 0.0) (rtrx 0.0) (s1 0.0) (s2 0.0) (t$ 0.0) (tc 0.0)
           (temp1 0.0) (temp2 0.0) (tt 0.0) (i 0) (j 0))
      (declare (type (f2cl-lib:integer4) j i)
               (type (double-float) tt temp2 temp1 tc t$ s2 s1 rtrx f2 f1 e2 e1
                                    ex d2 d1 cv ax))
      (setf ax (abs x))
      (setf rx (f2cl-lib:fsqrt ax))
      (setf c (* con1 ax rx))
      (if (< x 0.0) (go label120))
      (if (> c 8.0) (go label60))
      (if (> x 2.5) (go label30))
      (setf t$ (* (- (+ x x) 2.5) 0.4))
      (setf tt (+ t$ t$))
      (setf j n1)
      (setf f1 (f2cl-lib:fref bk1 (j) ((1 20))))
      (setf f2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m1) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref bk1 (j) ((1 20)))))
          (setf f2 temp1)
         label10))
      (setf bi (+ (- (* t$ f1) f2) (f2cl-lib:fref bk1 (1) ((1 20)))))
      (setf j n1d)
      (setf f1 (f2cl-lib:fref dbk1 (j) ((1 21))))
      (setf f2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m1d) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref dbk1 (j) ((1 21)))))
          (setf f2 temp1)
         label20))
      (setf dbi (+ (- (* t$ f1) f2) (f2cl-lib:fref dbk1 (1) ((1 21)))))
      (go end_label)
     label30
      (setf rtrx (f2cl-lib:fsqrt rx))
      (setf t$ (* (- (+ x x) con2) con3))
      (setf tt (+ t$ t$))
      (setf j n1)
      (setf f1 (f2cl-lib:fref bk2 (j) ((1 20))))
      (setf f2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m1) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref bk2 (j) ((1 20)))))
          (setf f2 temp1)
         label40))
      (setf bi (/ (+ (- (* t$ f1) f2) (f2cl-lib:fref bk2 (1) ((1 20)))) rtrx))
      (setf ex (exp c))
      (setf bi (* bi ex))
      (setf j n2d)
      (setf f1 (f2cl-lib:fref dbk2 (j) ((1 20))))
      (setf f2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m2d) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref dbk2 (j) ((1 20)))))
          (setf f2 temp1)
         label50))
      (setf dbi
              (* (+ (- (* t$ f1) f2) (f2cl-lib:fref dbk2 (1) ((1 20)))) rtrx))
      (setf dbi (* dbi ex))
      (go end_label)
     label60
      (setf rtrx (f2cl-lib:fsqrt rx))
      (setf t$ (- (/ 16.0 c) 1.0))
      (setf tt (+ t$ t$))
      (setf j n1)
      (setf f1 (f2cl-lib:fref bk3 (j) ((1 20))))
      (setf f2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m1) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref bk3 (j) ((1 20)))))
          (setf f2 temp1)
         label70))
      (setf s1 (+ (- (* t$ f1) f2) (f2cl-lib:fref bk3 (1) ((1 20)))))
      (setf j n2d)
      (setf f1 (f2cl-lib:fref dbk3 (j) ((1 20))))
      (setf f2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m2d) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref dbk3 (j) ((1 20)))))
          (setf f2 temp1)
         label80))
      (setf d1 (+ (- (* t$ f1) f2) (f2cl-lib:fref dbk3 (1) ((1 20)))))
      (setf tc (+ c c))
      (setf ex (exp c))
      (if (> tc 35.0) (go label110))
      (setf t$ (- (/ 10.0 c) 1.0))
      (setf tt (+ t$ t$))
      (setf j n3)
      (setf f1 (f2cl-lib:fref bk4 (j) ((1 14))))
      (setf f2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m3) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref bk4 (j) ((1 14)))))
          (setf f2 temp1)
         label90))
      (setf s2 (+ (- (* t$ f1) f2) (f2cl-lib:fref bk4 (1) ((1 14)))))
      (setf bi (/ (+ s1 (* (exp (- tc)) s2)) rtrx))
      (setf bi (* bi ex))
      (setf j n4d)
      (setf f1 (f2cl-lib:fref dbk4 (j) ((1 14))))
      (setf f2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m4d) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref dbk4 (j) ((1 14)))))
          (setf f2 temp1)
         label100))
      (setf d2 (+ (- (* t$ f1) f2) (f2cl-lib:fref dbk4 (1) ((1 14)))))
      (setf dbi (* rtrx (+ d1 (* (exp (- tc)) d2))))
      (setf dbi (* dbi ex))
      (go end_label)
     label110
      (setf bi (/ (* ex s1) rtrx))
      (setf dbi (* ex rtrx d1))
      (go end_label)
     label120
      (if (> c 5.0) (go label150))
      (setf t$ (- (* 0.4 c) 1.0))
      (setf tt (+ t$ t$))
      (setf j n2)
      (setf f1 (f2cl-lib:fref bjp (j) ((1 19))))
      (setf e1 (f2cl-lib:fref bjn (j) ((1 19))))
      (setf f2 0.0)
      (setf e2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m2) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf temp2 e1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref bjp (j) ((1 19)))))
          (setf e1 (+ (- (* tt e1) e2) (f2cl-lib:fref bjn (j) ((1 19)))))
          (setf f2 temp1)
          (setf e2 temp2)
         label130))
      (setf bi
              (- (+ (- (* t$ e1) e2) (f2cl-lib:fref bjn (1) ((1 19))))
                 (* ax (+ (- (* t$ f1) f2) (f2cl-lib:fref bjp (1) ((1 19)))))))
      (setf j n3d)
      (setf f1 (f2cl-lib:fref dbjp (j) ((1 19))))
      (setf e1 (f2cl-lib:fref dbjn (j) ((1 19))))
      (setf f2 0.0)
      (setf e2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m3d) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf temp2 e1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref dbjp (j) ((1 19)))))
          (setf e1 (+ (- (* tt e1) e2) (f2cl-lib:fref dbjn (j) ((1 19)))))
          (setf f2 temp1)
          (setf e2 temp2)
         label140))
      (setf dbi
              (+ (* x x (+ (- (* t$ f1) f2) (f2cl-lib:fref dbjp (1) ((1 19)))))
                 (+ (- (* t$ e1) e2) (f2cl-lib:fref dbjn (1) ((1 19))))))
      (go end_label)
     label150
      (setf rtrx (f2cl-lib:fsqrt rx))
      (setf t$ (- (/ 10.0 c) 1.0))
      (setf tt (+ t$ t$))
      (setf j n3)
      (setf f1 (f2cl-lib:fref aa (j) ((1 14))))
      (setf e1 (f2cl-lib:fref bb (j) ((1 14))))
      (setf f2 0.0)
      (setf e2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m3) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf temp2 e1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref aa (j) ((1 14)))))
          (setf e1 (+ (- (* tt e1) e2) (f2cl-lib:fref bb (j) ((1 14)))))
          (setf f2 temp1)
          (setf e2 temp2)
         label160))
      (setf temp1 (+ (- (* t$ f1) f2) (f2cl-lib:fref aa (1) ((1 14)))))
      (setf temp2 (+ (- (* t$ e1) e2) (f2cl-lib:fref bb (1) ((1 14)))))
      (setf cv (- c fpi12))
      (setf bi (/ (+ (* temp1 (cos cv)) (* temp2 (sin cv))) rtrx))
      (setf j n4d)
      (setf f1 (f2cl-lib:fref daa (j) ((1 14))))
      (setf e1 (f2cl-lib:fref dbb (j) ((1 14))))
      (setf f2 0.0)
      (setf e2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m4d) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf temp2 e1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref daa (j) ((1 14)))))
          (setf e1 (+ (- (* tt e1) e2) (f2cl-lib:fref dbb (j) ((1 14)))))
          (setf f2 temp1)
          (setf e2 temp2)
         label170))
      (setf temp1 (+ (- (* t$ f1) f2) (f2cl-lib:fref daa (1) ((1 14)))))
      (setf temp2 (+ (- (* t$ e1) e2) (f2cl-lib:fref dbb (1) ((1 14)))))
      (setf cv (- c spi12))
      (setf dbi (* (- (* temp1 (cos cv)) (* temp2 (sin cv))) rtrx))
      (go end_label)
     end_label
      (return (values nil rx c bi dbi)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dyairy
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float) (double-float))
           :return-values '(nil fortran-to-lisp::rx fortran-to-lisp::c
                            fortran-to-lisp::bi fortran-to-lisp::dbi)
           :calls 'nil)))

