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


(let ((nti1 0)
      (ntai1 0)
      (ntai12 0)
      (xmin 0.0)
      (xsml 0.0)
      (bi1cs
       (make-array 17
                   :element-type 'double-float
                   :initial-contents '(-0.0019717132610998596
                                       0.4073488766754648 0.03483899429995946
                                       0.0015453945563001237
                                       4.188852109837778e-5
                                       7.649026764836211e-7
                                       1.0042493924741179e-8
                                       9.93220779192381e-11
                                       7.663801791844764e-13
                                       4.741418923816739e-15
                                       2.404114404074518e-17
                                       1.0171505007093713e-19
                                       3.6450935657866947e-22
                                       1.1205749502562039e-24
                                       2.987544193446809e-27
                                       6.973231093919471e-30
                                       1.43679482206208e-32)))
      (ai1cs
       (make-array 46
                   :element-type 'double-float
                   :initial-contents '(-0.028467441818814786
                                       -0.019229532314432207
                                       -6.115185857943788e-4
                                       -2.0699712533502276e-5
                                       8.585619145810725e-6
                                       1.049498246711591e-6
                                       -2.9183389184479024e-7
                                       -1.559378146631739e-8
                                       1.3180123671449447e-8
                                       -1.4484234181830783e-9
                                       -2.908512243993142e-10
                                       1.2663889178753824e-10
                                       -1.6649477729192206e-11
                                       -1.666653644609433e-12
                                       1.2426024142907682e-12
                                       -2.731549379672432e-13
                                       2.0239478816458037e-14
                                       7.307950018116884e-15
                                       -3.332905634404675e-15
                                       7.175346558512954e-16
                                       -6.982530324796256e-17
                                       -1.2999442015627607e-17
                                       8.1209428642428e-18
                                       -2.194016207410737e-18
                                       3.6305161700296547e-19
                                       -1.6951397724391042e-20
                                       -1.2881848298979078e-20
                                       5.694428604967053e-21
                                       -1.4595970090904801e-21
                                       2.5145460106757173e-22
                                       -1.8447588831391248e-23
                                       -6.339760596227949e-24
                                       3.461441102031011e-24
                                       -1.0170623353713936e-24
                                       2.1498771470904314e-25
                                       -3.045252425238676e-26
                                       5.238082144721286e-28
                                       1.4435831070893824e-27
                                       -6.121302074890043e-28
                                       1.7000111174678184e-28
                                       -3.5965891079842444e-29
                                       5.448178578948419e-30
                                       -2.731831789689085e-31
                                       -1.8589050217086006e-31
                                       9.212682974513933e-32
                                       -2.8138351556535614e-32)))
      (ai12cs
       (make-array 69
                   :element-type 'double-float
                   :initial-contents '(0.02857623501828012
                                       -0.009761097491361469
                                       -1.1058893876262371e-4
                                       -3.882564808877691e-6
                                       -2.512236237870209e-7
                                       -2.6314688468895196e-8
                                       -3.835380385964237e-9
                                       -5.589743462196584e-10
                                       -1.8974958123505413e-11
                                       3.2526035830154884e-11
                                       1.4125807436613782e-11
                                       2.0356285441470896e-12
                                       -7.198551776245908e-13
                                       -4.0835511110921974e-13
                                       -2.1015418427726643e-14
                                       4.272440016711951e-14
                                       1.0420276984128802e-14
                                       -3.8144030724370075e-15
                                       -1.8803547755107825e-15
                                       3.3082023109209285e-16
                                       2.96262899764595e-16
                                       -3.209525921993424e-17
                                       -4.6503053684893586e-17
                                       4.414348323071708e-18
                                       7.517296310842105e-18
                                       -9.314178867326884e-19
                                       -1.242193275194891e-18
                                       2.4142767194548486e-19
                                       2.0269443840532852e-19
                                       -6.394267188269098e-20
                                       -3.049812452373096e-20
                                       1.6128418516514802e-20
                                       3.560913964309925e-21
                                       -3.752017947936439e-21
                                       -5.787037427074799e-23
                                       7.759997511648162e-22
                                       -1.4527908972022333e-22
                                       -1.3182252867390368e-22
                                       6.11665486290307e-23
                                       1.3762797624271266e-23
                                       -1.690837689959348e-23
                                       1.4305960885954331e-24
                                       3.409557828090594e-24
                                       -1.3094576662707602e-24
                                       -3.9407064112402574e-25
                                       4.277137426980877e-25
                                       -4.4246348309826066e-26
                                       -8.734113196230715e-26
                                       4.0454013356835337e-26
                                       7.06710065809469e-27
                                       -1.2494633445651053e-26
                                       2.867392244403437e-27
                                       2.0442928925042927e-27
                                       -1.5186366338204625e-27
                                       8.110181098187576e-29
                                       3.580379354773586e-28
                                       -1.6929290189279025e-28
                                       -2.2229024997024276e-29
                                       5.424535127145969e-29
                                       -1.7870684015780186e-29
                                       -6.565479068722815e-30
                                       7.807013165061145e-30
                                       -1.8165952606689797e-30
                                       -1.2877049526600847e-30
                                       1.1145481729881646e-30
                                       -1.8083431450393369e-31
                                       -2.231677718203772e-31
                                       1.6190295960803416e-31
                                       -1.8340799088049413e-32)))
      (first$ nil))
  (declare (type (f2cl-lib:integer4) nti1 ntai1 ntai12)
           (type (double-float) xmin xsml)
           (type (simple-array double-float (17)) bi1cs)
           (type (simple-array double-float (46)) ai1cs)
           (type (simple-array double-float (69)) ai12cs)
           (type f2cl-lib:logical first$))
  (setq first$ f2cl-lib:%true%)
  (defun dbsi1e (x)
    (declare (type (double-float) x))
    (prog ((y 0.0) (dbsi1e 0.0) (eta 0.0f0))
      (declare (type (single-float) eta) (type (double-float) dbsi1e y))
      (cond
        (first$
         (setf eta (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3))))
         (setf nti1 (initds bi1cs 17 eta))
         (setf ntai1 (initds ai1cs 46 eta))
         (setf ntai12 (initds ai12cs 69 eta))
         (setf xmin (* 2.0 (f2cl-lib:d1mach 1)))
         (setf xsml (f2cl-lib:fsqrt (* 4.5 (f2cl-lib:d1mach 3))))))
      (setf first$ f2cl-lib:%false%)
      (setf y (abs x))
      (if (> y 3.0) (go label20))
      (setf dbsi1e 0.0)
      (if (= y 0.0) (go end_label))
      (if (<= y xmin)
          (xermsg "SLATEC" "DBSI1E" "ABS(X) SO SMALL I1 UNDERFLOWS" 1 1))
      (if (> y xmin) (setf dbsi1e (* 0.5 x)))
      (if (> y xsml)
          (setf dbsi1e
                  (* x (+ 0.875 (dcsevl (- (/ (* y y) 4.5) 1.0) bi1cs nti1)))))
      (setf dbsi1e (* (exp (- y)) dbsi1e))
      (go end_label)
     label20
      (if (<= y 8.0)
          (setf dbsi1e
                  (/ (+ 0.375 (dcsevl (/ (- (/ 48.0 y) 11.0) 5.0) ai1cs ntai1))
                     (f2cl-lib:fsqrt y))))
      (if (> y 8.0)
          (setf dbsi1e
                  (/ (+ 0.375 (dcsevl (- (/ 16.0 y) 1.0) ai12cs ntai12))
                     (f2cl-lib:fsqrt y))))
      (setf dbsi1e (f2cl-lib:sign dbsi1e x))
      (go end_label)
     end_label
      (return (values dbsi1e nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dbsi1e
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::dcsevl
                                                     fortran-to-lisp::xermsg
                                                     fortran-to-lisp::initds
                                                     fortran-to-lisp::d1mach))))

