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


(let ((x
       (make-array 11
                   :element-type 'double-float
                   :initial-contents '(0.9914448613738104 0.9659258262890683
                                       0.9238795325112868 0.8660254037844386
                                       0.7933533402912352 0.7071067811865475
                                       0.6087614290087205 0.5
                                       0.3826834323650898 0.2588190451025208
                                       0.1305261922200516))))
  (declare (type (array double-float (11)) x))
  (defun dqc25s
         (f a b bl br alfa beta ri rj rg rh result abserr resasc integr nev)
    (declare (type (f2cl-lib:integer4) nev integr)
             (type (array double-float (*)) rh rg rj ri)
             (type (double-float) resasc abserr result beta alfa br bl b a))
    (f2cl-lib:with-multi-array-data
        ((ri double-float ri-%data% ri-%offset%)
         (rj double-float rj-%data% rj-%offset%)
         (rg double-float rg-%data% rg-%offset%)
         (rh double-float rh-%data% rh-%offset%))
      (prog ((cheb12 (make-array 13 :element-type 'double-float))
             (cheb24 (make-array 25 :element-type 'double-float))
             (fval (make-array 25 :element-type 'double-float)) (i 0) (isym 0)
             (centr 0.0) (dc 0.0) (factor 0.0) (fix 0.0) (hlgth 0.0)
             (resabs 0.0) (res12 0.0) (res24 0.0) (u 0.0))
        (declare (type (array double-float (25)) fval cheb24)
                 (type (array double-float (13)) cheb12)
                 (type (double-float) u res24 res12 resabs hlgth fix factor dc
                                      centr)
                 (type (f2cl-lib:integer4) isym i))
        (setf nev 25)
        (if (and (= bl a) (or (/= alfa 0.0) (= integr 2) (= integr 4)))
            (go label10))
        (if (and (= br b) (or (/= beta 0.0) (= integr 3) (= integr 4)))
            (go label140))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11 var-12)
            (dqk15w f #'dqwgts a b alfa beta integr bl br result abserr resabs
             resasc)
          (declare (ignore var-0 var-1 var-7 var-8))
          (setf a var-2)
          (setf b var-3)
          (setf alfa var-4)
          (setf beta var-5)
          (setf integr var-6)
          (setf result var-9)
          (setf abserr var-10)
          (setf resabs var-11)
          (setf resasc var-12))
        (setf nev 15)
        (go label270)
       label10
        (setf hlgth (* 0.5 (- br bl)))
        (setf centr (* 0.5 (+ br bl)))
        (setf fix (- b centr))
        (setf (f2cl-lib:fref fval (1) ((1 25)))
                (* 0.5 (funcall f (+ hlgth centr)) (expt (- fix hlgth) beta)))
        (setf (f2cl-lib:fref fval (13) ((1 25)))
                (*
                 (multiple-value-bind (ret-val var-0)
                     (funcall f centr)
                   (declare (ignore))
                   (when var-0
                     (setf centr var-0))
                   ret-val)
                 (expt fix beta)))
        (setf (f2cl-lib:fref fval (25) ((1 25)))
                (* 0.5 (funcall f (- centr hlgth)) (expt (+ fix hlgth) beta)))
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i 12) nil)
          (tagbody
            (setf u
                    (* hlgth
                       (f2cl-lib:fref x ((f2cl-lib:int-sub i 1)) ((1 11)))))
            (setf isym (f2cl-lib:int-sub 26 i))
            (setf (f2cl-lib:fref fval (i) ((1 25)))
                    (* (funcall f (+ u centr)) (expt (- fix u) beta)))
            (setf (f2cl-lib:fref fval (isym) ((1 25)))
                    (* (funcall f (- centr u)) (expt (+ fix u) beta)))
           label20))
        (setf factor (expt hlgth (+ alfa 1.0)))
        (setf result 0.0)
        (setf abserr 0.0)
        (setf res12 0.0)
        (setf res24 0.0)
        (if (> integr 2) (go label70))
        (dqcheb x fval cheb12 cheb24)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i 13) nil)
          (tagbody
            (setf res12
                    (+ res12
                       (* (f2cl-lib:fref cheb12 (i) ((1 13)))
                          (f2cl-lib:fref ri-%data% (i) ((1 25)) ri-%offset%))))
            (setf res24
                    (+ res24
                       (* (f2cl-lib:fref cheb24 (i) ((1 25)))
                          (f2cl-lib:fref ri-%data% (i) ((1 25)) ri-%offset%))))
           label30))
        (f2cl-lib:fdo (i 14 (f2cl-lib:int-add i 1))
                      ((> i 25) nil)
          (tagbody
            (setf res24
                    (+ res24
                       (* (f2cl-lib:fref cheb24 (i) ((1 25)))
                          (f2cl-lib:fref ri-%data% (i) ((1 25)) ri-%offset%))))
           label40))
        (if (= integr 1) (go label130))
        (setf dc (f2cl-lib:flog (- br bl)))
        (setf result (* res24 dc))
        (setf abserr (abs (* (- res24 res12) dc)))
        (setf res12 0.0)
        (setf res24 0.0)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i 13) nil)
          (tagbody
            (setf res12
                    (+ res12
                       (* (f2cl-lib:fref cheb12 (i) ((1 13)))
                          (f2cl-lib:fref rg-%data% (i) ((1 25)) rg-%offset%))))
            (setf res24
                    (+ res24
                       (* (f2cl-lib:fref cheb24 (i) ((1 25)))
                          (f2cl-lib:fref rg-%data% (i) ((1 25)) rg-%offset%))))
           label50))
        (f2cl-lib:fdo (i 14 (f2cl-lib:int-add i 1))
                      ((> i 25) nil)
          (tagbody
            (setf res24
                    (+ res24
                       (* (f2cl-lib:fref cheb24 (i) ((1 25)))
                          (f2cl-lib:fref rg-%data% (i) ((1 25)) rg-%offset%))))
           label60))
        (go label130)
       label70
        (setf (f2cl-lib:fref fval (1) ((1 25)))
                (* (f2cl-lib:fref fval (1) ((1 25)))
                   (f2cl-lib:flog (- fix hlgth))))
        (setf (f2cl-lib:fref fval (13) ((1 25)))
                (* (f2cl-lib:fref fval (13) ((1 25))) (f2cl-lib:flog fix)))
        (setf (f2cl-lib:fref fval (25) ((1 25)))
                (* (f2cl-lib:fref fval (25) ((1 25)))
                   (f2cl-lib:flog (+ fix hlgth))))
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i 12) nil)
          (tagbody
            (setf u
                    (* hlgth
                       (f2cl-lib:fref x ((f2cl-lib:int-sub i 1)) ((1 11)))))
            (setf isym (f2cl-lib:int-sub 26 i))
            (setf (f2cl-lib:fref fval (i) ((1 25)))
                    (* (f2cl-lib:fref fval (i) ((1 25)))
                       (f2cl-lib:flog (- fix u))))
            (setf (f2cl-lib:fref fval (isym) ((1 25)))
                    (* (f2cl-lib:fref fval (isym) ((1 25)))
                       (f2cl-lib:flog (+ fix u))))
           label80))
        (dqcheb x fval cheb12 cheb24)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i 13) nil)
          (tagbody
            (setf res12
                    (+ res12
                       (* (f2cl-lib:fref cheb12 (i) ((1 13)))
                          (f2cl-lib:fref ri-%data% (i) ((1 25)) ri-%offset%))))
            (setf res24
                    (+ res24
                       (* (f2cl-lib:fref cheb24 (i) ((1 25)))
                          (f2cl-lib:fref ri-%data% (i) ((1 25)) ri-%offset%))))
           label90))
        (f2cl-lib:fdo (i 14 (f2cl-lib:int-add i 1))
                      ((> i 25) nil)
          (tagbody
            (setf res24
                    (+ res24
                       (* (f2cl-lib:fref cheb24 (i) ((1 25)))
                          (f2cl-lib:fref ri-%data% (i) ((1 25)) ri-%offset%))))
           label100))
        (if (= integr 3) (go label130))
        (setf dc (f2cl-lib:flog (- br bl)))
        (setf result (* res24 dc))
        (setf abserr (abs (* (- res24 res12) dc)))
        (setf res12 0.0)
        (setf res24 0.0)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i 13) nil)
          (tagbody
            (setf res12
                    (+ res12
                       (* (f2cl-lib:fref cheb12 (i) ((1 13)))
                          (f2cl-lib:fref rg-%data% (i) ((1 25)) rg-%offset%))))
            (setf res24
                    (+ res24
                       (* (f2cl-lib:fref cheb24 (i) ((1 25)))
                          (f2cl-lib:fref rg-%data% (i) ((1 25)) rg-%offset%))))
           label110))
        (f2cl-lib:fdo (i 14 (f2cl-lib:int-add i 1))
                      ((> i 25) nil)
          (tagbody
            (setf res24
                    (+ res24
                       (* (f2cl-lib:fref cheb24 (i) ((1 25)))
                          (f2cl-lib:fref rg-%data% (i) ((1 25)) rg-%offset%))))
           label120))
       label130
        (setf result (* (+ result res24) factor))
        (setf abserr (* (+ abserr (abs (- res24 res12))) factor))
        (go label270)
       label140
        (setf hlgth (* 0.5 (- br bl)))
        (setf centr (* 0.5 (+ br bl)))
        (setf fix (- centr a))
        (setf (f2cl-lib:fref fval (1) ((1 25)))
                (* 0.5 (funcall f (+ hlgth centr)) (expt (+ fix hlgth) alfa)))
        (setf (f2cl-lib:fref fval (13) ((1 25)))
                (*
                 (multiple-value-bind (ret-val var-0)
                     (funcall f centr)
                   (declare (ignore))
                   (when var-0
                     (setf centr var-0))
                   ret-val)
                 (expt fix alfa)))
        (setf (f2cl-lib:fref fval (25) ((1 25)))
                (* 0.5 (funcall f (- centr hlgth)) (expt (- fix hlgth) alfa)))
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i 12) nil)
          (tagbody
            (setf u
                    (* hlgth
                       (f2cl-lib:fref x ((f2cl-lib:int-sub i 1)) ((1 11)))))
            (setf isym (f2cl-lib:int-sub 26 i))
            (setf (f2cl-lib:fref fval (i) ((1 25)))
                    (* (funcall f (+ u centr)) (expt (+ fix u) alfa)))
            (setf (f2cl-lib:fref fval (isym) ((1 25)))
                    (* (funcall f (- centr u)) (expt (- fix u) alfa)))
           label150))
        (setf factor (expt hlgth (+ beta 1.0)))
        (setf result 0.0)
        (setf abserr 0.0)
        (setf res12 0.0)
        (setf res24 0.0)
        (if (or (= integr 2) (= integr 4)) (go label200))
        (dqcheb x fval cheb12 cheb24)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i 13) nil)
          (tagbody
            (setf res12
                    (+ res12
                       (* (f2cl-lib:fref cheb12 (i) ((1 13)))
                          (f2cl-lib:fref rj-%data% (i) ((1 25)) rj-%offset%))))
            (setf res24
                    (+ res24
                       (* (f2cl-lib:fref cheb24 (i) ((1 25)))
                          (f2cl-lib:fref rj-%data% (i) ((1 25)) rj-%offset%))))
           label160))
        (f2cl-lib:fdo (i 14 (f2cl-lib:int-add i 1))
                      ((> i 25) nil)
          (tagbody
            (setf res24
                    (+ res24
                       (* (f2cl-lib:fref cheb24 (i) ((1 25)))
                          (f2cl-lib:fref rj-%data% (i) ((1 25)) rj-%offset%))))
           label170))
        (if (= integr 1) (go label260))
        (setf dc (f2cl-lib:flog (- br bl)))
        (setf result (* res24 dc))
        (setf abserr (abs (* (- res24 res12) dc)))
        (setf res12 0.0)
        (setf res24 0.0)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i 13) nil)
          (tagbody
            (setf res12
                    (+ res12
                       (* (f2cl-lib:fref cheb12 (i) ((1 13)))
                          (f2cl-lib:fref rh-%data% (i) ((1 25)) rh-%offset%))))
            (setf res24
                    (+ res24
                       (* (f2cl-lib:fref cheb24 (i) ((1 25)))
                          (f2cl-lib:fref rh-%data% (i) ((1 25)) rh-%offset%))))
           label180))
        (f2cl-lib:fdo (i 14 (f2cl-lib:int-add i 1))
                      ((> i 25) nil)
          (tagbody
            (setf res24
                    (+ res24
                       (* (f2cl-lib:fref cheb24 (i) ((1 25)))
                          (f2cl-lib:fref rh-%data% (i) ((1 25)) rh-%offset%))))
           label190))
        (go label260)
       label200
        (setf (f2cl-lib:fref fval (1) ((1 25)))
                (* (f2cl-lib:fref fval (1) ((1 25)))
                   (f2cl-lib:flog (+ fix hlgth))))
        (setf (f2cl-lib:fref fval (13) ((1 25)))
                (* (f2cl-lib:fref fval (13) ((1 25))) (f2cl-lib:flog fix)))
        (setf (f2cl-lib:fref fval (25) ((1 25)))
                (* (f2cl-lib:fref fval (25) ((1 25)))
                   (f2cl-lib:flog (- fix hlgth))))
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i 12) nil)
          (tagbody
            (setf u
                    (* hlgth
                       (f2cl-lib:fref x ((f2cl-lib:int-sub i 1)) ((1 11)))))
            (setf isym (f2cl-lib:int-sub 26 i))
            (setf (f2cl-lib:fref fval (i) ((1 25)))
                    (* (f2cl-lib:fref fval (i) ((1 25)))
                       (f2cl-lib:flog (+ u fix))))
            (setf (f2cl-lib:fref fval (isym) ((1 25)))
                    (* (f2cl-lib:fref fval (isym) ((1 25)))
                       (f2cl-lib:flog (- fix u))))
           label210))
        (dqcheb x fval cheb12 cheb24)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i 13) nil)
          (tagbody
            (setf res12
                    (+ res12
                       (* (f2cl-lib:fref cheb12 (i) ((1 13)))
                          (f2cl-lib:fref rj-%data% (i) ((1 25)) rj-%offset%))))
            (setf res24
                    (+ res24
                       (* (f2cl-lib:fref cheb24 (i) ((1 25)))
                          (f2cl-lib:fref rj-%data% (i) ((1 25)) rj-%offset%))))
           label220))
        (f2cl-lib:fdo (i 14 (f2cl-lib:int-add i 1))
                      ((> i 25) nil)
          (tagbody
            (setf res24
                    (+ res24
                       (* (f2cl-lib:fref cheb24 (i) ((1 25)))
                          (f2cl-lib:fref rj-%data% (i) ((1 25)) rj-%offset%))))
           label230))
        (if (= integr 2) (go label260))
        (setf dc (f2cl-lib:flog (- br bl)))
        (setf result (* res24 dc))
        (setf abserr (abs (* (- res24 res12) dc)))
        (setf res12 0.0)
        (setf res24 0.0)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i 13) nil)
          (tagbody
            (setf res12
                    (+ res12
                       (* (f2cl-lib:fref cheb12 (i) ((1 13)))
                          (f2cl-lib:fref rh-%data% (i) ((1 25)) rh-%offset%))))
            (setf res24
                    (+ res24
                       (* (f2cl-lib:fref cheb24 (i) ((1 25)))
                          (f2cl-lib:fref rh-%data% (i) ((1 25)) rh-%offset%))))
           label240))
        (f2cl-lib:fdo (i 14 (f2cl-lib:int-add i 1))
                      ((> i 25) nil)
          (tagbody
            (setf res24
                    (+ res24
                       (* (f2cl-lib:fref cheb24 (i) ((1 25)))
                          (f2cl-lib:fref rh-%data% (i) ((1 25)) rh-%offset%))))
           label250))
       label260
        (setf result (* (+ result res24) factor))
        (setf abserr (* (+ abserr (abs (- res24 res12))) factor))
       label270
        (go end_label)
       end_label
        (return
         (values nil
                 a
                 b
                 nil
                 nil
                 alfa
                 beta
                 nil
                 nil
                 nil
                 nil
                 result
                 abserr
                 resasc
                 integr
                 nev))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dqc25s
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil fortran-to-lisp::a fortran-to-lisp::b nil nil
                            fortran-to-lisp::alfa fortran-to-lisp::beta nil nil
                            nil nil fortran-to-lisp::result
                            fortran-to-lisp::abserr fortran-to-lisp::resasc
                            fortran-to-lisp::integr fortran-to-lisp::nev)
           :calls '(fortran-to-lisp::dqcheb fortran-to-lisp::dqk15w))))

