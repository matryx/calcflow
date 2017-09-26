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


(let ((pi$ 3.141592653589793))
  (declare (type (double-float) pi$))
  (defun zacai (zr zi fnu kode mr n yr yi nz rl tol elim alim)
    (declare (type (simple-array double-float (*)) yi yr)
             (type (f2cl-lib:integer4) nz n mr kode)
             (type (double-float) alim elim tol rl fnu zi zr))
    (prog ((cyr (make-array 2 :element-type 'double-float))
           (cyi (make-array 2 :element-type 'double-float)) (inu 0) (iuf 0)
           (nn 0) (nw 0) (arg 0.0) (ascle 0.0) (az 0.0) (csgnr 0.0) (csgni 0.0)
           (cspnr 0.0) (cspni 0.0) (c1r 0.0) (c1i 0.0) (c2r 0.0) (c2i 0.0)
           (dfnu 0.0) (fmr 0.0) (sgn 0.0) (yy 0.0) (znr 0.0) (zni 0.0))
      (declare (type (simple-array double-float (2)) cyi cyr)
               (type (double-float) zni znr yy sgn fmr dfnu c2i c2r c1i c1r
                                    cspni cspnr csgni csgnr az ascle arg)
               (type (f2cl-lib:integer4) nw nn iuf inu))
      (setf nz 0)
      (setf znr (- zr))
      (setf zni (- zi))
      (setf az (coerce (realpart (zabs zr zi)) 'double-float))
      (setf nn n)
      (setf dfnu (+ fnu (f2cl-lib:int-sub n 1)))
      (if (<= az 2.0) (go label10))
      (if (> (* az az 0.25) (+ dfnu 1.0)) (go label20))
     label10
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
             var-10)
          (zseri znr zni fnu kode nn yr yi nw tol elim alim)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-8 var-9
                         var-10))
        (setf nw var-7))
      (go label40)
     label20
      (if (< az rl) (go label30))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11)
          (zasyi znr zni fnu kode nn yr yi nw rl tol elim alim)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-8 var-9
                         var-10 var-11))
        (setf nw var-7))
      (if (< nw 0) (go label80))
      (go label40)
     label30
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
          (zmlri znr zni fnu kode nn yr yi nw tol)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-8))
        (setf nw var-7))
      (if (< nw 0) (go label80))
     label40
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
             var-10)
          (zbknu znr zni fnu kode 1 cyr cyi nw tol elim alim)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-8 var-9
                         var-10))
        (setf nw var-7))
      (if (/= nw 0) (go label80))
      (setf fmr (coerce (the f2cl-lib:integer4 mr) 'double-float))
      (setf sgn (coerce (- (f2cl-lib:dsign pi$ fmr)) 'double-float))
      (setf csgnr 0.0)
      (setf csgni sgn)
      (if (= kode 1) (go label50))
      (setf yy (- zni))
      (setf csgnr (* (- csgni) (sin yy)))
      (setf csgni (* csgni (cos yy)))
     label50
      (setf inu (f2cl-lib:int fnu))
      (setf arg (* (- fnu inu) sgn))
      (setf cspnr (cos arg))
      (setf cspni (sin arg))
      (if (= (mod inu 2) 0) (go label60))
      (setf cspnr (- cspnr))
      (setf cspni (- cspni))
     label60
      (setf c1r (f2cl-lib:fref cyr (1) ((1 2))))
      (setf c1i (f2cl-lib:fref cyi (1) ((1 2))))
      (setf c2r (f2cl-lib:fref yr (1) ((1 n))))
      (setf c2i (f2cl-lib:fref yi (1) ((1 n))))
      (if (= kode 1) (go label70))
      (setf iuf 0)
      (setf ascle (/ (* 1000.0 (f2cl-lib:d1mach 1)) tol))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
          (zs1s2 znr zni c1r c1i c2r c2i nw ascle alim iuf)
        (declare (ignore var-0 var-1 var-7 var-8))
        (setf c1r var-2)
        (setf c1i var-3)
        (setf c2r var-4)
        (setf c2i var-5)
        (setf nw var-6)
        (setf iuf var-9))
      (setf nz (f2cl-lib:int-add nz nw))
     label70
      (setf (f2cl-lib:fref yr (1) ((1 n)))
              (- (+ (- (* cspnr c1r) (* cspni c1i)) (* csgnr c2r))
                 (* csgni c2i)))
      (setf (f2cl-lib:fref yi (1) ((1 n)))
              (+ (* cspnr c1i) (* cspni c1r) (* csgnr c2i) (* csgni c2r)))
      (go end_label)
     label80
      (setf nz -1)
      (if (= nw -2) (setf nz -2))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil nz nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zacai fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (simple-array double-float (*))
                        (simple-array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (double-float) (double-float))
           :return-values '(nil nil nil nil nil nil nil nil fortran-to-lisp::nz
                            nil nil nil nil)
           :calls '(fortran-to-lisp::zs1s2 fortran-to-lisp::d1mach
                    fortran-to-lisp::zbknu fortran-to-lisp::zmlri
                    fortran-to-lisp::zasyi fortran-to-lisp::zseri
                    fortran-to-lisp::zabs))))

