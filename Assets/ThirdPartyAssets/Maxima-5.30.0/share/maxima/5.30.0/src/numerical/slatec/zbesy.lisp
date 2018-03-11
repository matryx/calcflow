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


(defun zbesy (zr zi fnu kode n cyr cyi nz cwrkr cwrki ierr)
  (declare (type (simple-array double-float (*)) cwrki cwrkr cyi cyr)
           (type (f2cl-lib:integer4) ierr nz n kode)
           (type (double-float) fnu zi zr))
  (prog ((i 0) (k 0) (k1 0) (k2 0) (nz1 0) (nz2 0) (c1i 0.0) (c1r 0.0)
         (c2i 0.0) (c2r 0.0) (elim 0.0) (exi 0.0) (exr 0.0) (ey 0.0) (hcii 0.0)
         (sti 0.0) (str 0.0) (tay 0.0) (ascle 0.0) (rtol 0.0) (atol 0.0)
         (aa 0.0) (bb 0.0) (tol 0.0) (r1m5 0.0))
    (declare (type (double-float) r1m5 tol bb aa atol rtol ascle tay str sti
                                  hcii ey exr exi elim c2r c2i c1r c1i)
             (type (f2cl-lib:integer4) nz2 nz1 k2 k1 k i))
    (setf ierr 0)
    (setf nz 0)
    (if (and (= zr 0.0) (= zi 0.0)) (setf ierr 1))
    (if (< fnu 0.0) (setf ierr 1))
    (if (or (< kode 1) (> kode 2)) (setf ierr 1))
    (if (< n 1) (setf ierr 1))
    (if (/= ierr 0) (go end_label))
    (setf hcii 0.5)
    (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
        (zbesh zr zi fnu kode 1 n cyr cyi nz1 ierr)
      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
      (setf nz1 var-8)
      (setf ierr var-9))
    (if (and (/= ierr 0) (/= ierr 3)) (go label170))
    (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
        (zbesh zr zi fnu kode 2 n cwrkr cwrki nz2 ierr)
      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
      (setf nz2 var-8)
      (setf ierr var-9))
    (if (and (/= ierr 0) (/= ierr 3)) (go label170))
    (setf nz (min (the f2cl-lib:integer4 nz1) (the f2cl-lib:integer4 nz2)))
    (if (= kode 2) (go label60))
    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                  ((> i n) nil)
      (tagbody
        (setf str
                (- (f2cl-lib:fref cwrkr (i) ((1 n)))
                   (f2cl-lib:fref cyr (i) ((1 n)))))
        (setf sti
                (- (f2cl-lib:fref cwrki (i) ((1 n)))
                   (f2cl-lib:fref cyi (i) ((1 n)))))
        (setf (f2cl-lib:fref cyr (i) ((1 n))) (* (- sti) hcii))
        (setf (f2cl-lib:fref cyi (i) ((1 n))) (* str hcii))
       label50))
    (go end_label)
   label60
    (setf tol (max (f2cl-lib:d1mach 4) 1.0e-18))
    (setf k1 (f2cl-lib:i1mach 15))
    (setf k2 (f2cl-lib:i1mach 16))
    (setf k
            (min (the f2cl-lib:integer4 (abs k1))
                 (the f2cl-lib:integer4 (abs k2))))
    (setf r1m5 (f2cl-lib:d1mach 5))
    (setf elim (* 2.303 (- (* k r1m5) 3.0)))
    (setf exr (cos zr))
    (setf exi (sin zr))
    (setf ey 0.0)
    (setf tay (abs (+ zi zi)))
    (if (< tay elim) (setf ey (exp (- tay))))
    (if (< zi 0.0) (go label90))
    (setf c1r (* exr ey))
    (setf c1i (* exi ey))
    (setf c2r exr)
    (setf c2i (- exi))
   label70
    (setf nz 0)
    (setf rtol (/ 1.0 tol))
    (setf ascle (* (f2cl-lib:d1mach 1) rtol 1000.0))
    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                  ((> i n) nil)
      (tagbody
        (setf aa (f2cl-lib:fref cwrkr (i) ((1 n))))
        (setf bb (f2cl-lib:fref cwrki (i) ((1 n))))
        (setf atol 1.0)
        (if (> (max (abs aa) (abs bb)) ascle) (go label75))
        (setf aa (* aa rtol))
        (setf bb (* bb rtol))
        (setf atol tol)
       label75
        (setf str (* (- (* aa c2r) (* bb c2i)) atol))
        (setf sti (* (+ (* aa c2i) (* bb c2r)) atol))
        (setf aa (f2cl-lib:fref cyr (i) ((1 n))))
        (setf bb (f2cl-lib:fref cyi (i) ((1 n))))
        (setf atol 1.0)
        (if (> (max (abs aa) (abs bb)) ascle) (go label85))
        (setf aa (* aa rtol))
        (setf bb (* bb rtol))
        (setf atol tol)
       label85
        (setf str (- str (* (+ (* aa c1r) (* -1 bb c1i)) atol)))
        (setf sti (- sti (* (+ (* aa c1i) (* bb c1r)) atol)))
        (setf (f2cl-lib:fref cyr (i) ((1 n))) (* (- sti) hcii))
        (setf (f2cl-lib:fref cyi (i) ((1 n))) (* str hcii))
        (if (and (= str 0.0) (= sti 0.0) (= ey 0.0))
            (setf nz (f2cl-lib:int-add nz 1)))
       label80))
    (go end_label)
   label90
    (setf c1r exr)
    (setf c1i exi)
    (setf c2r (* exr ey))
    (setf c2i (* (- exi) ey))
    (go label70)
   label170
    (setf nz 0)
    (go end_label)
   end_label
    (return (values nil nil nil nil nil nil nil nz nil nil ierr))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zbesy fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (simple-array double-float (*))
                        (simple-array double-float (*))
                        (fortran-to-lisp::integer4)
                        (simple-array double-float (*))
                        (simple-array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil fortran-to-lisp::nz nil
                            nil fortran-to-lisp::ierr)
           :calls '(fortran-to-lisp::i1mach fortran-to-lisp::d1mach
                    fortran-to-lisp::zbesh))))

