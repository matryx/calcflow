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


(let ((pi$ 3.141592653589793) (coner 1.0) (conei 0.0))
  (declare (type (double-float) pi$ coner conei))
  (defun zbesi (zr zi fnu kode n cyr cyi nz ierr)
    (declare (type (simple-array double-float (*)) cyi cyr)
             (type (f2cl-lib:integer4) ierr nz n kode)
             (type (double-float) fnu zi zr))
    (prog ((i 0) (inu 0) (k 0) (k1 0) (k2 0) (nn 0) (aa 0.0) (alim 0.0)
           (arg 0.0) (csgni 0.0) (csgnr 0.0) (dig 0.0) (elim 0.0) (fnul 0.0)
           (rl 0.0) (r1m5 0.0) (str 0.0) (tol 0.0) (zni 0.0) (znr 0.0) (az 0.0)
           (bb 0.0) (fn 0.0) (ascle 0.0) (rtol 0.0) (atol 0.0) (sti 0.0))
      (declare (type (double-float) sti atol rtol ascle fn bb az znr zni tol
                                    str r1m5 rl fnul elim dig csgnr csgni arg
                                    alim aa)
               (type (f2cl-lib:integer4) nn k2 k1 k inu i))
      (setf ierr 0)
      (setf nz 0)
      (if (< fnu 0.0) (setf ierr 1))
      (if (or (< kode 1) (> kode 2)) (setf ierr 1))
      (if (< n 1) (setf ierr 1))
      (if (/= ierr 0) (go end_label))
      (setf tol (max (f2cl-lib:d1mach 4) 1.0e-18))
      (setf k1 (f2cl-lib:i1mach 15))
      (setf k2 (f2cl-lib:i1mach 16))
      (setf r1m5 (f2cl-lib:d1mach 5))
      (setf k
              (min (the f2cl-lib:integer4 (abs k1))
                   (the f2cl-lib:integer4 (abs k2))))
      (setf elim (* 2.303 (- (* k r1m5) 3.0)))
      (setf k1 (f2cl-lib:int-sub (f2cl-lib:i1mach 14) 1))
      (setf aa (* r1m5 k1))
      (setf dig (min aa 18.0))
      (setf aa (* aa 2.303))
      (setf alim (+ elim (max (- aa) -41.45)))
      (setf rl (+ (* 1.2 dig) 3.0))
      (setf fnul (+ 10.0 (* 6.0 (- dig 3.0))))
      (setf az (coerce (realpart (zabs zr zi)) 'double-float))
      (setf fn (+ fnu (f2cl-lib:int-sub n 1)))
      (setf aa (/ 0.5 tol))
      (setf bb (* (f2cl-lib:i1mach 9) 0.5))
      (setf aa (min aa bb))
      (if (> az aa) (go label260))
      (if (> fn aa) (go label260))
      (setf aa (f2cl-lib:fsqrt aa))
      (if (> az aa) (setf ierr 3))
      (if (> fn aa) (setf ierr 3))
      (setf znr zr)
      (setf zni zi)
      (setf csgnr coner)
      (setf csgni conei)
      (if (>= zr 0.0) (go label40))
      (setf znr (- zr))
      (setf zni (- zi))
      (setf inu (f2cl-lib:int fnu))
      (setf arg (* (- fnu inu) pi$))
      (if (< zi 0.0) (setf arg (- arg)))
      (setf csgnr (cos arg))
      (setf csgni (sin arg))
      (if (= (mod inu 2) 0) (go label40))
      (setf csgnr (- csgnr))
      (setf csgni (- csgni))
     label40
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12)
          (zbinu znr zni fnu kode n cyr cyi nz rl fnul tol elim alim)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-8 var-9
                         var-10 var-11 var-12))
        (setf nz var-7))
      (if (< nz 0) (go label120))
      (if (>= zr 0.0) (go end_label))
      (setf nn (f2cl-lib:int-sub n nz))
      (if (= nn 0) (go end_label))
      (setf rtol (/ 1.0 tol))
      (setf ascle (* (f2cl-lib:d1mach 1) rtol 1000.0))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i nn) nil)
        (tagbody
          (setf aa (f2cl-lib:fref cyr (i) ((1 n))))
          (setf bb (f2cl-lib:fref cyi (i) ((1 n))))
          (setf atol 1.0)
          (if (> (max (abs aa) (abs bb)) ascle) (go label55))
          (setf aa (* aa rtol))
          (setf bb (* bb rtol))
          (setf atol tol)
         label55
          (setf str (- (* aa csgnr) (* bb csgni)))
          (setf sti (+ (* aa csgni) (* bb csgnr)))
          (setf (f2cl-lib:fref cyr (i) ((1 n))) (* str atol))
          (setf (f2cl-lib:fref cyi (i) ((1 n))) (* sti atol))
          (setf csgnr (- csgnr))
          (setf csgni (- csgni))
         label50))
      (go end_label)
     label120
      (if (= nz -2) (go label130))
      (setf nz 0)
      (setf ierr 2)
      (go end_label)
     label130
      (setf nz 0)
      (setf ierr 5)
      (go end_label)
     label260
      (setf nz 0)
      (setf ierr 4)
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nz ierr)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zbesi fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (simple-array double-float (*))
                        (simple-array double-float (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil fortran-to-lisp::nz
                            fortran-to-lisp::ierr)
           :calls '(fortran-to-lisp::zbinu fortran-to-lisp::zabs
                    fortran-to-lisp::i1mach fortran-to-lisp::d1mach))))

