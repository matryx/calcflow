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


(let ((zeror 0.0) (zeroi 0.0) (coner 1.0) (conei 0.0))
  (declare (type (double-float) zeror zeroi coner conei))
  (defun zseri (zr zi fnu kode n yr yi nz tol elim alim)
    (declare (type (simple-array double-float (*)) yi yr)
             (type (f2cl-lib:integer4) nz n kode)
             (type (double-float) alim elim tol fnu zi zr))
    (prog ((wr (make-array 2 :element-type 'double-float))
           (wi (make-array 2 :element-type 'double-float)) (i 0) (ib 0)
           (idum 0) (iflag 0) (il 0) (k 0) (l 0) (m 0) (nn 0) (nw 0) (aa 0.0)
           (acz 0.0) (ak 0.0) (ak1i 0.0) (ak1r 0.0) (arm 0.0) (ascle 0.0)
           (atol 0.0) (az 0.0) (cki 0.0) (ckr 0.0) (coefi 0.0) (coefr 0.0)
           (crscr 0.0) (czi 0.0) (czr 0.0) (dfnu 0.0) (fnup 0.0) (hzi 0.0)
           (hzr 0.0) (raz 0.0) (rs 0.0) (rtr1 0.0) (rzi 0.0) (rzr 0.0) (s 0.0)
           (ss 0.0) (sti 0.0) (str 0.0) (s1i 0.0) (s1r 0.0) (s2i 0.0)
           (s2r 0.0))
      (declare (type (simple-array double-float (2)) wr wi)
               (type (double-float) s2r s2i s1r s1i str sti ss s rzr rzi rtr1
                                    rs raz hzr hzi fnup dfnu czr czi crscr
                                    coefr coefi ckr cki az atol ascle arm ak1r
                                    ak1i ak acz aa)
               (type (f2cl-lib:integer4) nw nn m l k il iflag idum ib i))
      (setf nz 0)
      (setf az (coerce (realpart (zabs zr zi)) 'double-float))
      (if (= az 0.0) (go label160))
      (setf arm (* 1000.0 (f2cl-lib:d1mach 1)))
      (setf rtr1 (f2cl-lib:fsqrt arm))
      (setf crscr 1.0)
      (setf iflag 0)
      (if (< az arm) (go label150))
      (setf hzr (* 0.5 zr))
      (setf hzi (* 0.5 zi))
      (setf czr zeror)
      (setf czi zeroi)
      (if (<= az rtr1) (go label10))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
          (zmlt hzr hzi hzr hzi czr czi)
        (declare (ignore var-0 var-1 var-2 var-3))
        (setf czr var-4)
        (setf czi var-5))
     label10
      (setf acz (coerce (realpart (zabs czr czi)) 'double-float))
      (setf nn n)
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
          (zlog hzr hzi ckr cki idum)
        (declare (ignore var-0 var-1))
        (setf ckr var-2)
        (setf cki var-3)
        (setf idum var-4))
     label20
      (setf dfnu (+ fnu (f2cl-lib:int-sub nn 1)))
      (setf fnup (+ dfnu 1.0))
      (setf ak1r (* ckr dfnu))
      (setf ak1i (* cki dfnu))
      (setf ak
              (multiple-value-bind (ret-val var-0 var-1)
                  (dgamln fnup idum)
                (declare (ignore var-0))
                (setf idum var-1)
                ret-val))
      (setf ak1r (- ak1r ak))
      (if (= kode 2) (setf ak1r (- ak1r zr)))
      (if (> ak1r (- elim)) (go label40))
     label30
      (setf nz (f2cl-lib:int-add nz 1))
      (setf (f2cl-lib:fref yr (nn) ((1 n))) zeror)
      (setf (f2cl-lib:fref yi (nn) ((1 n))) zeroi)
      (if (> acz dfnu) (go label190))
      (setf nn (f2cl-lib:int-sub nn 1))
      (if (= nn 0) (go end_label))
      (go label20)
     label40
      (if (> ak1r (- alim)) (go label50))
      (setf iflag 1)
      (setf ss (/ 1.0 tol))
      (setf crscr tol)
      (setf ascle (* arm ss))
     label50
      (setf aa (exp ak1r))
      (if (= iflag 1) (setf aa (* aa ss)))
      (setf coefr (* aa (cos ak1i)))
      (setf coefi (* aa (sin ak1i)))
      (setf atol (/ (* tol acz) fnup))
      (setf il (min (the f2cl-lib:integer4 2) (the f2cl-lib:integer4 nn)))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i il) nil)
        (tagbody
          (setf dfnu (+ fnu (f2cl-lib:int-sub nn i)))
          (setf fnup (+ dfnu 1.0))
          (setf s1r coner)
          (setf s1i conei)
          (if (< acz (* tol fnup)) (go label70))
          (setf ak1r coner)
          (setf ak1i conei)
          (setf ak (+ fnup 2.0))
          (setf s fnup)
          (setf aa 2.0)
         label60
          (setf rs (/ 1.0 s))
          (setf str (- (* ak1r czr) (* ak1i czi)))
          (setf sti (+ (* ak1r czi) (* ak1i czr)))
          (setf ak1r (* str rs))
          (setf ak1i (* sti rs))
          (setf s1r (+ s1r ak1r))
          (setf s1i (+ s1i ak1i))
          (setf s (+ s ak))
          (setf ak (+ ak 2.0))
          (setf aa (* aa acz rs))
          (if (> aa atol) (go label60))
         label70
          (setf s2r (- (* s1r coefr) (* s1i coefi)))
          (setf s2i (+ (* s1r coefi) (* s1i coefr)))
          (setf (f2cl-lib:fref wr (i) ((1 2))) s2r)
          (setf (f2cl-lib:fref wi (i) ((1 2))) s2i)
          (if (= iflag 0) (go label80))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (zuchk s2r s2i nw ascle tol)
            (declare (ignore var-0 var-1 var-3 var-4))
            (setf nw var-2))
          (if (/= nw 0) (go label30))
         label80
          (setf m (f2cl-lib:int-add (f2cl-lib:int-sub nn i) 1))
          (setf (f2cl-lib:fref yr (m) ((1 n))) (* s2r crscr))
          (setf (f2cl-lib:fref yi (m) ((1 n))) (* s2i crscr))
          (if (= i il) (go label90))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (zdiv coefr coefi hzr hzi str sti)
            (declare (ignore var-0 var-1 var-2 var-3))
            (setf str var-4)
            (setf sti var-5))
          (setf coefr (* str dfnu))
          (setf coefi (* sti dfnu))
         label90))
      (if (<= nn 2) (go end_label))
      (setf k (f2cl-lib:int-sub nn 2))
      (setf ak (coerce (the f2cl-lib:integer4 k) 'double-float))
      (setf raz (/ 1.0 az))
      (setf str (* zr raz))
      (setf sti (* (- zi) raz))
      (setf rzr (* (+ str str) raz))
      (setf rzi (* (+ sti sti) raz))
      (if (= iflag 1) (go label120))
      (setf ib 3)
     label100
      (f2cl-lib:fdo (i ib (f2cl-lib:int-add i 1))
                    ((> i nn) nil)
        (tagbody
          (setf (f2cl-lib:fref yr (k) ((1 n)))
                  (+
                   (* (+ ak fnu)
                      (-
                       (* rzr
                          (f2cl-lib:fref yr ((f2cl-lib:int-add k 1)) ((1 n))))
                       (* rzi
                          (f2cl-lib:fref yi
                                         ((f2cl-lib:int-add k 1))
                                         ((1 n))))))
                   (f2cl-lib:fref yr ((f2cl-lib:int-add k 2)) ((1 n)))))
          (setf (f2cl-lib:fref yi (k) ((1 n)))
                  (+
                   (* (+ ak fnu)
                      (+
                       (* rzr
                          (f2cl-lib:fref yi ((f2cl-lib:int-add k 1)) ((1 n))))
                       (* rzi
                          (f2cl-lib:fref yr
                                         ((f2cl-lib:int-add k 1))
                                         ((1 n))))))
                   (f2cl-lib:fref yi ((f2cl-lib:int-add k 2)) ((1 n)))))
          (setf ak (- ak 1.0))
          (setf k (f2cl-lib:int-sub k 1))
         label110))
      (go end_label)
     label120
      (setf s1r (f2cl-lib:fref wr (1) ((1 2))))
      (setf s1i (f2cl-lib:fref wi (1) ((1 2))))
      (setf s2r (f2cl-lib:fref wr (2) ((1 2))))
      (setf s2i (f2cl-lib:fref wi (2) ((1 2))))
      (f2cl-lib:fdo (l 3 (f2cl-lib:int-add l 1))
                    ((> l nn) nil)
        (tagbody
          (setf ckr s2r)
          (setf cki s2i)
          (setf s2r (+ s1r (* (+ ak fnu) (- (* rzr ckr) (* rzi cki)))))
          (setf s2i (+ s1i (* (+ ak fnu) (+ (* rzr cki) (* rzi ckr)))))
          (setf s1r ckr)
          (setf s1i cki)
          (setf ckr (* s2r crscr))
          (setf cki (* s2i crscr))
          (setf (f2cl-lib:fref yr (k) ((1 n))) ckr)
          (setf (f2cl-lib:fref yi (k) ((1 n))) cki)
          (setf ak (- ak 1.0))
          (setf k (f2cl-lib:int-sub k 1))
          (if (> (zabs ckr cki) ascle) (go label140))
         label130))
      (go end_label)
     label140
      (setf ib (f2cl-lib:int-add l 1))
      (if (> ib nn) (go end_label))
      (go label100)
     label150
      (setf nz n)
      (if (= fnu 0.0) (setf nz (f2cl-lib:int-sub nz 1)))
     label160
      (setf (f2cl-lib:fref yr (1) ((1 n))) zeror)
      (setf (f2cl-lib:fref yi (1) ((1 n))) zeroi)
      (if (/= fnu 0.0) (go label170))
      (setf (f2cl-lib:fref yr (1) ((1 n))) coner)
      (setf (f2cl-lib:fref yi (1) ((1 n))) conei)
     label170
      (if (= n 1) (go end_label))
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref yr (i) ((1 n))) zeror)
          (setf (f2cl-lib:fref yi (i) ((1 n))) zeroi)
         label180))
      (go end_label)
     label190
      (setf nz (f2cl-lib:int-sub nz))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nz nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zseri fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (simple-array double-float (*))
                        (simple-array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (double-float))
           :return-values '(nil nil nil nil nil nil nil fortran-to-lisp::nz nil
                            nil nil)
           :calls '(fortran-to-lisp::zdiv fortran-to-lisp::zuchk
                    fortran-to-lisp::dgamln fortran-to-lisp::zlog
                    fortran-to-lisp::zmlt fortran-to-lisp::d1mach
                    fortran-to-lisp::zabs))))

