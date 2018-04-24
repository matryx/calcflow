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


(let ((n1 14)
      (n2 23)
      (n3 19)
      (n4 15)
      (m1 12)
      (m2 21)
      (m3 17)
      (m4 13)
      (fpi12 1.30899693899575)
      (con2 5.03154716196777)
      (con3 0.380004589867293)
      (con4 0.833333333333333)
      (con5 0.866025403784439)
      (ak1
       (make-array 14
                   :element-type 'double-float
                   :initial-contents '(0.220423090987793 -0.1252902427877
                                       0.0103881163359194 8.22844152006343e-4
                                       -2.34614345891226e-4 1.63824280172116e-5
                                       3.06902589573189e-7 -1.29621999359332e-7
                                       8.22908158823668e-9 1.53963968623298e-11
                                       -3.39165465615682e-11
                                       2.03253257423626e-12
                                       -1.10679546097884e-14
                                       -5.1616949778508e-15)))
      (ak2
       (make-array 23
                   :element-type 'double-float
                   :initial-contents '(0.274366150869598 0.00539790969736903
                                       -0.0015733922062119 4.2742752824875e-4
                                       -1.12124917399925e-4 2.88763171318904e-5
                                       -7.36804225370554e-6 1.87290209741024e-6
                                       -4.75892793962291e-7 1.21130416955909e-7
                                       -3.09245374270614e-8 7.92454705282654e-9
                                       -2.03902447167914e-9
                                       5.26863056595742e-10
                                       -1.36704767639569e-10
                                       3.56141039013708e-11
                                       -9.3138829654843e-12
                                       2.44464450473635e-12
                                       -6.43840261990955e-13
                                       1.70106030559349e-13
                                       -4.50760104503281e-14
                                       1.19774799164811e-14
                                       -3.19077040865066e-15)))
      (ak3
       (make-array 14
                   :element-type 'double-float
                   :initial-contents '(0.280271447340791 -0.00178127042844379
                                       4.03422579628999e-5 -1.63249965269003e-6
                                       9.21181482476768e-8 -6.52294330229155e-9
                                       5.47138404576546e-10
                                       -5.2440825180026e-11
                                       5.60477904117209e-12
                                       -6.56375244639313e-13
                                       8.31285761966247e-14
                                       -1.12705134691063e-14
                                       1.62267976598129e-15
                                       -2.46480324312426e-16)))
      (ajp
       (make-array 19
                   :element-type 'double-float
                   :initial-contents '(0.0778952966437581 -0.184356363456801
                                       0.0301412605216174 0.0305342724277608
                                       -0.00495424702513079
                                       -0.00172749552563952 2.4313763783919e-4
                                       5.04564777517082e-5 -6.16316582695208e-6
                                       -9.03986745510768e-7 9.70243778355884e-8
                                       1.09639453305205e-8 -1.04716330588766e-9
                                       -9.60359441344646e-11
                                       8.25358789454134e-12
                                       6.36123439018768e-13
                                       -4.96629614116015e-14
                                       -3.29810288929615e-15
                                       2.35798252031104e-16)))
      (ajn
       (make-array 19
                   :element-type 'double-float
                   :initial-contents '(0.0380497887617242 -0.245319541845546
                                       0.165820623702696 0.0749330045818789
                                       -0.0263476288106641 -0.00592535597304981
                                       0.00144744409589804 2.18311831322215e-4
                                       -4.10662077680304e-5
                                       -4.66874994171766e-6 7.1521880727716e-7
                                       6.52964770854633e-8 -8.44284027565946e-9
                                       -6.44186158976978e-10
                                       7.20802286505285e-11
                                       4.72465431717846e-12
                                       -4.66022632547045e-13
                                       -2.67762710389189e-14
                                       2.36161316570019e-15)))
      (a
       (make-array 15
                   :element-type 'double-float
                   :initial-contents '(0.490275424742791 0.00157647277946204
                                       -9.66195963140306e-5 1.35916080268815e-7
                                       2.98157342654859e-7 -1.86824767559979e-8
                                       -1.03685737667141e-9
                                       3.28660818434328e-10
                                       -2.5709141063278e-11
                                       -2.32357655300677e-12
                                       9.57523279048255e-13
                                       -1.20340828049719e-13
                                       -2.90907716770715e-15
                                       4.55656454580149e-15
                                       -9.99003874810259e-16)))
      (b
       (make-array 15
                   :element-type 'double-float
                   :initial-contents '(0.278593552803079 -0.00352915691882584
                                       -2.31149677384994e-5 4.7131784226356e-6
                                       -1.12415907931333e-7
                                       -2.00100301184339e-8 2.60948075302193e-9
                                       -3.55098136101216e-11
                                       -3.50849978423875e-11
                                       5.83007187954202e-12
                                       -2.04644828753326e-13
                                       -1.10529179476742e-13
                                       2.87724778038775e-14
                                       -2.88205111009939e-15
                                       -3.32656311696166e-16)))
      (n1d 14)
      (n2d 24)
      (n3d 19)
      (n4d 15)
      (m1d 12)
      (m2d 22)
      (m3d 17)
      (m4d 13)
      (dak1
       (make-array 14
                   :element-type 'double-float
                   :initial-contents '(0.204567842307887 -0.0661322739905664
                                       -0.00849845800989287 0.00312183491556289
                                       -2.70016489829432e-4
                                       -6.35636298679387e-6 3.02397712409509e-6
                                       -2.18311195330088e-7
                                       -5.36194289332826e-10 1.1309803562231e-9
                                       -7.43023834629073e-11
                                       4.28804170826891e-13
                                       2.23810925754539e-13
                                       -1.39140135641182e-14)))
      (dak2
       (make-array 24
                   :element-type 'double-float
                   :initial-contents '(0.29333234388323 -0.00806196784743112
                                       0.0024254017233314 -6.82297548850235e-4
                                       1.85786427751181e-4 -4.97457447684059e-5
                                       1.32090681239497e-5 -3.49528240444943e-6
                                       9.24362451078835e-7 -2.44732671521867e-7
                                       6.4930783764891e-8 -1.72717621501538e-8
                                       4.60725763604656e-9 -1.2324905529155e-9
                                       3.30620409488102e-10
                                       -8.89252099772401e-11
                                       2.39773319878298e-11
                                       -6.4801392115345e-12
                                       1.75510132023731e-12
                                       -4.76303829833637e-13
                                       1.2949824110081e-13 -3.5267962221043e-14
                                       9.62005151585923e-15
                                       -2.62786914342292e-15)))
      (dak3
       (make-array 14
                   :element-type 'double-float
                   :initial-contents '(0.284675828811349 0.0025307307261908
                                       -4.83481130337976e-5 1.84907283946343e-6
                                       -1.01418491178576e-7 7.05925634457153e-9
                                       -5.85325291400382e-10
                                       5.56357688831339e-11 -5.908890947795e-12
                                       6.88574353784436e-13
                                       -8.68588256452194e-14
                                       1.17374762617213e-14
                                       -1.68523146510923e-15
                                       2.55374773097056e-16)))
      (dajp
       (make-array 19
                   :element-type 'double-float
                   :initial-contents '(0.0653219131311457 -0.120262933688823
                                       0.00978010236263823 0.0167948429230505
                                       -0.00197146140182132
                                       -8.45560295098867e-4 9.42889620701976e-5
                                       2.25827860945475e-5 -2.29067870915987e-6
                                       -3.76343991136919e-7 3.45663933559565e-8
                                       4.29611332003007e-9
                                       -3.58673691214989e-10
                                       -3.57245881361895e-11
                                       2.72696091066336e-12
                                       2.26120653095771e-13
                                       -1.58763205238303e-14
                                       -1.12604374485125e-15
                                       7.31327529515367e-17)))
      (dajn
       (make-array 19
                   :element-type 'double-float
                   :initial-contents '(0.0108594539632967 0.0853313194857091
                                       -0.315277068113058 -0.0878420725294257
                                       0.0553251906976048 0.00941674060503241
                                       -0.00332187026018996
                                       -4.11157343156826e-4 1.01297326891346e-4
                                       9.87633682208396e-6 -1.87312969812393e-6
                                       -1.50798500131468e-7 2.32687669525394e-8
                                       1.59599917419225e-9
                                       -2.07665922668385e-10
                                       -1.24103350500302e-11
                                       1.39631765331043e-12 7.3940097115574e-14
                                       -7.328874756275e-15)))
      (da
       (make-array 15
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
                                       1.69384811284491e-16
                                       8.17900786477396e-16)))
      (db
       (make-array 15
                   :element-type 'double-float
                   :initial-contents '(-0.277571356944231 0.0044421283341992
                                       -8.42328522190089e-5 -2.5804031841871e-6
                                       3.42389720217621e-7 -6.24286894709776e-9
                                       -2.36377836844577e-9
                                       3.16991042656673e-10
                                       -4.40995691658191e-12
                                       -5.18674221093575e-12
                                       9.64874015137022e-13
                                       -4.9019057660871e-14
                                       -1.77253430678112e-14
                                       5.55950610442662e-15
                                       -7.1179333757953e-16))))
  (declare (type (f2cl-lib:integer4) n1 n2 n3 n4 m1 m2 m3 m4 n1d n2d n3d n4d
                                     m1d m2d m3d m4d)
           (type (double-float) fpi12 con2 con3 con4 con5)
           (type (simple-array double-float (14)) ak1 ak3 dak1 dak3)
           (type (simple-array double-float (23)) ak2)
           (type (simple-array double-float (19)) ajp ajn dajp dajn)
           (type (simple-array double-float (15)) a b da db)
           (type (simple-array double-float (24)) dak2))
  (defun djairy (x rx c ai dai)
    (declare (type (double-float) dai ai c rx x))
    (prog ((ccv 0.0) (cv 0.0) (ec 0.0) (e1 0.0) (e2 0.0) (f1 0.0) (f2 0.0)
           (rtrx 0.0) (scv 0.0) (t$ 0.0) (temp1 0.0) (temp2 0.0) (tt 0.0) (i 0)
           (j 0))
      (declare (type (f2cl-lib:integer4) j i)
               (type (double-float) tt temp2 temp1 t$ scv rtrx f2 f1 e2 e1 ec
                                    cv ccv))
      (if (< x 0.0) (go label90))
      (if (> c 5.0) (go label60))
      (if (> x 1.2) (go label30))
      (setf t$ (* (- (+ x x) 1.2) con4))
      (setf tt (+ t$ t$))
      (setf j n1)
      (setf f1 (f2cl-lib:fref ak1 (j) ((1 14))))
      (setf f2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m1) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref ak1 (j) ((1 14)))))
          (setf f2 temp1)
         label10))
      (setf ai (+ (- (* t$ f1) f2) (f2cl-lib:fref ak1 (1) ((1 14)))))
      (setf j n1d)
      (setf f1 (f2cl-lib:fref dak1 (j) ((1 14))))
      (setf f2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m1d) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref dak1 (j) ((1 14)))))
          (setf f2 temp1)
         label20))
      (setf dai (- (+ (- (* t$ f1) f2) (f2cl-lib:fref dak1 (1) ((1 14))))))
      (go end_label)
     label30
      (setf t$ (* (- (+ x x) con2) con3))
      (setf tt (+ t$ t$))
      (setf j n2)
      (setf f1 (f2cl-lib:fref ak2 (j) ((1 23))))
      (setf f2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m2) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref ak2 (j) ((1 23)))))
          (setf f2 temp1)
         label40))
      (setf rtrx (f2cl-lib:fsqrt rx))
      (setf ec (exp (- c)))
      (setf ai
              (/ (* ec (+ (- (* t$ f1) f2) (f2cl-lib:fref ak2 (1) ((1 23)))))
                 rtrx))
      (setf j n2d)
      (setf f1 (f2cl-lib:fref dak2 (j) ((1 24))))
      (setf f2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m2d) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref dak2 (j) ((1 24)))))
          (setf f2 temp1)
         label50))
      (setf dai
              (* (- ec)
                 (+ (- (* t$ f1) f2) (f2cl-lib:fref dak2 (1) ((1 24))))
                 rtrx))
      (go end_label)
     label60
      (setf t$ (- (/ 10.0 c) 1.0))
      (setf tt (+ t$ t$))
      (setf j n1)
      (setf f1 (f2cl-lib:fref ak3 (j) ((1 14))))
      (setf f2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m1) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref ak3 (j) ((1 14)))))
          (setf f2 temp1)
         label70))
      (setf rtrx (f2cl-lib:fsqrt rx))
      (setf ec (exp (- c)))
      (setf ai
              (/ (* ec (+ (- (* t$ f1) f2) (f2cl-lib:fref ak3 (1) ((1 14)))))
                 rtrx))
      (setf j n1d)
      (setf f1 (f2cl-lib:fref dak3 (j) ((1 14))))
      (setf f2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m1d) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref dak3 (j) ((1 14)))))
          (setf f2 temp1)
         label80))
      (setf dai
              (* (- rtrx)
                 ec
                 (+ (- (* t$ f1) f2) (f2cl-lib:fref dak3 (1) ((1 14))))))
      (go end_label)
     label90
      (if (> c 5.0) (go label120))
      (setf t$ (- (* 0.4 c) 1.0))
      (setf tt (+ t$ t$))
      (setf j n3)
      (setf f1 (f2cl-lib:fref ajp (j) ((1 19))))
      (setf e1 (f2cl-lib:fref ajn (j) ((1 19))))
      (setf f2 0.0)
      (setf e2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m3) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf temp2 e1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref ajp (j) ((1 19)))))
          (setf e1 (+ (- (* tt e1) e2) (f2cl-lib:fref ajn (j) ((1 19)))))
          (setf f2 temp1)
          (setf e2 temp2)
         label100))
      (setf ai
              (- (+ (- (* t$ e1) e2) (f2cl-lib:fref ajn (1) ((1 19))))
                 (* x (+ (- (* t$ f1) f2) (f2cl-lib:fref ajp (1) ((1 19)))))))
      (setf j n3d)
      (setf f1 (f2cl-lib:fref dajp (j) ((1 19))))
      (setf e1 (f2cl-lib:fref dajn (j) ((1 19))))
      (setf f2 0.0)
      (setf e2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m3d) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf temp2 e1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref dajp (j) ((1 19)))))
          (setf e1 (+ (- (* tt e1) e2) (f2cl-lib:fref dajn (j) ((1 19)))))
          (setf f2 temp1)
          (setf e2 temp2)
         label110))
      (setf dai
              (+ (* x x (+ (- (* t$ f1) f2) (f2cl-lib:fref dajp (1) ((1 19)))))
                 (+ (- (* t$ e1) e2) (f2cl-lib:fref dajn (1) ((1 19))))))
      (go end_label)
     label120
      (setf t$ (- (/ 10.0 c) 1.0))
      (setf tt (+ t$ t$))
      (setf j n4)
      (setf f1 (f2cl-lib:fref a (j) ((1 15))))
      (setf e1 (f2cl-lib:fref b (j) ((1 15))))
      (setf f2 0.0)
      (setf e2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m4) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf temp2 e1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref a (j) ((1 15)))))
          (setf e1 (+ (- (* tt e1) e2) (f2cl-lib:fref b (j) ((1 15)))))
          (setf f2 temp1)
          (setf e2 temp2)
         label130))
      (setf temp1 (+ (- (* t$ f1) f2) (f2cl-lib:fref a (1) ((1 15)))))
      (setf temp2 (+ (- (* t$ e1) e2) (f2cl-lib:fref b (1) ((1 15)))))
      (setf rtrx (f2cl-lib:fsqrt rx))
      (setf cv (- c fpi12))
      (setf ccv (cos cv))
      (setf scv (sin cv))
      (setf ai (/ (- (* temp1 ccv) (* temp2 scv)) rtrx))
      (setf j n4d)
      (setf f1 (f2cl-lib:fref da (j) ((1 15))))
      (setf e1 (f2cl-lib:fref db (j) ((1 15))))
      (setf f2 0.0)
      (setf e2 0.0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m4d) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub j 1))
          (setf temp1 f1)
          (setf temp2 e1)
          (setf f1 (+ (- (* tt f1) f2) (f2cl-lib:fref da (j) ((1 15)))))
          (setf e1 (+ (- (* tt e1) e2) (f2cl-lib:fref db (j) ((1 15)))))
          (setf f2 temp1)
          (setf e2 temp2)
         label140))
      (setf temp1 (+ (- (* t$ f1) f2) (f2cl-lib:fref da (1) ((1 15)))))
      (setf temp2 (+ (- (* t$ e1) e2) (f2cl-lib:fref db (1) ((1 15)))))
      (setf e1 (+ (* ccv con5) (* 0.5 scv)))
      (setf e2 (- (* scv con5) (* 0.5 ccv)))
      (setf dai (* (- (* temp1 e1) (* temp2 e2)) rtrx))
      (go end_label)
     end_label
      (return (values nil nil nil ai dai)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::djairy
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float) (double-float))
           :return-values '(nil nil nil fortran-to-lisp::ai
                            fortran-to-lisp::dai)
           :calls 'nil)))

