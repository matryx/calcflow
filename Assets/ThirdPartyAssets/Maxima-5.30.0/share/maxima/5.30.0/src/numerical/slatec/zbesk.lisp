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


(defun zbesk (zr zi fnu kode n cyr cyi nz ierr)
  (declare (type (simple-array double-float (*)) cyi cyr)
           (type (f2cl-lib:integer4) ierr nz n kode)
           (type (double-float) fnu zi zr))
  (prog ((k 0) (k1 0) (k2 0) (mr 0) (nn 0) (nuf 0) (nw 0) (aa 0.0) (alim 0.0)
         (aln 0.0) (arg 0.0) (az 0.0) (dig 0.0) (elim 0.0) (fn 0.0) (fnul 0.0)
         (rl 0.0) (r1m5 0.0) (tol 0.0) (ufl 0.0) (bb 0.0))
    (declare (type (double-float) bb ufl tol r1m5 rl fnul fn elim dig az arg
                                  aln alim aa)
             (type (f2cl-lib:integer4) nw nuf nn mr k2 k1 k))
    (setf ierr 0)
    (setf nz 0)
    (if (and (= zi 0.0f0) (= zr 0.0f0)) (setf ierr 1))
    (if (< fnu 0.0) (setf ierr 1))
    (if (or (< kode 1) (> kode 2)) (setf ierr 1))
    (if (< n 1) (setf ierr 1))
    (if (/= ierr 0) (go end_label))
    (setf nn n)
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
    (setf fnul (+ 10.0 (* 6.0 (- dig 3.0))))
    (setf rl (+ (* 1.2 dig) 3.0))
    (setf az (coerce (realpart (zabs zr zi)) 'double-float))
    (setf fn (+ fnu (f2cl-lib:int-sub nn 1)))
    (setf aa (/ 0.5 tol))
    (setf bb (* (f2cl-lib:i1mach 9) 0.5))
    (setf aa (min aa bb))
    (if (> az aa) (go label260))
    (if (> fn aa) (go label260))
    (setf aa (f2cl-lib:fsqrt aa))
    (if (> az aa) (setf ierr 3))
    (if (> fn aa) (setf ierr 3))
    (setf ufl (* (f2cl-lib:d1mach 1) 1000.0))
    (if (< az ufl) (go label180))
    (if (> fnu fnul) (go label80))
    (if (<= fn 1.0) (go label60))
    (if (> fn 2.0) (go label50))
    (if (> az tol) (go label60))
    (setf arg (* 0.5 az))
    (setf aln (* (- fn) (f2cl-lib:flog arg)))
    (if (> aln elim) (go label180))
    (go label60)
   label50
    (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11)
        (zuoik zr zi fnu kode 2 nn cyr cyi nuf tol elim alim)
      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-9
                       var-10 var-11))
      (setf nuf var-8))
    (if (< nuf 0) (go label180))
    (setf nz (f2cl-lib:int-add nz nuf))
    (setf nn (f2cl-lib:int-sub nn nuf))
    (if (= nn 0) (go label100))
   label60
    (if (< zr 0.0) (go label70))
    (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10)
        (zbknu zr zi fnu kode nn cyr cyi nw tol elim alim)
      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-8 var-9
                       var-10))
      (setf nw var-7))
    (if (< nw 0) (go label200))
    (setf nz nw)
    (go end_label)
   label70
    (if (/= nz 0) (go label180))
    (setf mr 1)
    (if (< zi 0.0) (setf mr -1))
    (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12 var-13)
        (zacon zr zi fnu kode mr nn cyr cyi nw rl fnul tol elim alim)
      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-9
                       var-10 var-11 var-12 var-13))
      (setf nw var-8))
    (if (< nw 0) (go label200))
    (setf nz nw)
    (go end_label)
   label80
    (setf mr 0)
    (if (>= zr 0.0) (go label90))
    (setf mr 1)
    (if (< zi 0.0) (setf mr -1))
   label90
    (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11)
        (zbunk zr zi fnu kode mr nn cyr cyi nw tol elim alim)
      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-9
                       var-10 var-11))
      (setf nw var-8))
    (if (< nw 0) (go label200))
    (setf nz (f2cl-lib:int-add nz nw))
    (go end_label)
   label100
    (if (< zr 0.0) (go label180))
    (go end_label)
   label180
    (setf nz 0)
    (setf ierr 2)
    (go end_label)
   label200
    (if (= nw -1) (go label180))
    (setf nz 0)
    (setf ierr 5)
    (go end_label)
   label260
    (setf nz 0)
    (setf ierr 4)
    (go end_label)
   end_label
    (return (values nil nil nil nil nil nil nil nz ierr))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zbesk fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (simple-array double-float (*))
                        (simple-array double-float (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil fortran-to-lisp::nz
                            fortran-to-lisp::ierr)
           :calls '(fortran-to-lisp::zbunk fortran-to-lisp::zacon
                    fortran-to-lisp::zbknu fortran-to-lisp::zuoik
                    fortran-to-lisp::zabs fortran-to-lisp::i1mach
                    fortran-to-lisp::d1mach))))

