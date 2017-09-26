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


(defun zbuni (zr zi fnu kode n yr yi nz nui nlast fnul tol elim alim)
  (declare (type (simple-array double-float (*)) yi yr)
           (type (f2cl-lib:integer4) nlast nui nz n kode)
           (type (double-float) alim elim tol fnul fnu zi zr))
  (prog ((cyr (make-array 2 :element-type 'double-float))
         (cyi (make-array 2 :element-type 'double-float))
         (bry (make-array 3 :element-type 'double-float)) (i 0) (iflag 0)
         (iform 0) (k 0) (nl 0) (nw 0) (ax 0.0) (ay 0.0) (csclr 0.0)
         (cscrr 0.0) (dfnu 0.0) (fnui 0.0) (gnu 0.0) (raz 0.0) (rzi 0.0)
         (rzr 0.0) (sti 0.0) (str 0.0) (s1i 0.0) (s1r 0.0) (s2i 0.0) (s2r 0.0)
         (ascle 0.0) (c1r 0.0) (c1i 0.0) (c1m 0.0))
    (declare (type (simple-array double-float (3)) bry)
             (type (simple-array double-float (2)) cyr cyi)
             (type (double-float) c1m c1i c1r ascle s2r s2i s1r s1i str sti rzr
                                  rzi raz gnu fnui dfnu cscrr csclr ay ax)
             (type (f2cl-lib:integer4) nw nl k iform iflag i))
    (setf nz 0)
    (setf ax (* (abs zr) 1.7321))
    (setf ay (abs zi))
    (setf iform 1)
    (if (> ay ax) (setf iform 2))
    (if (= nui 0) (go label60))
    (setf fnui (coerce (the f2cl-lib:integer4 nui) 'double-float))
    (setf dfnu (+ fnu (f2cl-lib:int-sub n 1)))
    (setf gnu (+ dfnu fnui))
    (if (= iform 2) (go label10))
    (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12)
        (zuni1 zr zi gnu kode 2 cyr cyi nw nlast fnul tol elim alim)
      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-9 var-10
                       var-11 var-12))
      (setf nw var-7)
      (setf nlast var-8))
    (go label20)
   label10
    (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12)
        (zuni2 zr zi gnu kode 2 cyr cyi nw nlast fnul tol elim alim)
      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-9 var-10
                       var-11 var-12))
      (setf nw var-7)
      (setf nlast var-8))
   label20
    (if (< nw 0) (go label50))
    (if (/= nw 0) (go label90))
    (setf str
            (coerce
             (realpart
              (zabs (f2cl-lib:fref cyr (1) ((1 2)))
               (f2cl-lib:fref cyi (1) ((1 2)))))
             'double-float))
    (setf (f2cl-lib:fref bry (1) ((1 3)))
            (/ (* 1000.0 (f2cl-lib:d1mach 1)) tol))
    (setf (f2cl-lib:fref bry (2) ((1 3)))
            (/ 1.0 (f2cl-lib:fref bry (1) ((1 3)))))
    (setf (f2cl-lib:fref bry (3) ((1 3))) (f2cl-lib:fref bry (2) ((1 3))))
    (setf iflag 2)
    (setf ascle (f2cl-lib:fref bry (2) ((1 3))))
    (setf csclr 1.0)
    (if (> str (f2cl-lib:fref bry (1) ((1 3)))) (go label21))
    (setf iflag 1)
    (setf ascle (f2cl-lib:fref bry (1) ((1 3))))
    (setf csclr (/ 1.0 tol))
    (go label25)
   label21
    (if (< str (f2cl-lib:fref bry (2) ((1 3)))) (go label25))
    (setf iflag 3)
    (setf ascle (f2cl-lib:fref bry (3) ((1 3))))
    (setf csclr tol)
   label25
    (setf cscrr (/ 1.0 csclr))
    (setf s1r (* (f2cl-lib:fref cyr (2) ((1 2))) csclr))
    (setf s1i (* (f2cl-lib:fref cyi (2) ((1 2))) csclr))
    (setf s2r (* (f2cl-lib:fref cyr (1) ((1 2))) csclr))
    (setf s2i (* (f2cl-lib:fref cyi (1) ((1 2))) csclr))
    (setf raz (coerce (realpart (/ 1.0 (zabs zr zi))) 'double-float))
    (setf str (* zr raz))
    (setf sti (* (- zi) raz))
    (setf rzr (* (+ str str) raz))
    (setf rzi (* (+ sti sti) raz))
    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                  ((> i nui) nil)
      (tagbody
        (setf str s2r)
        (setf sti s2i)
        (setf s2r (+ (* (+ dfnu fnui) (- (* rzr str) (* rzi sti))) s1r))
        (setf s2i (+ (* (+ dfnu fnui) (+ (* rzr sti) (* rzi str))) s1i))
        (setf s1r str)
        (setf s1i sti)
        (setf fnui (- fnui 1.0))
        (if (>= iflag 3) (go label30))
        (setf str (* s2r cscrr))
        (setf sti (* s2i cscrr))
        (setf c1r (abs str))
        (setf c1i (abs sti))
        (setf c1m (max c1r c1i))
        (if (<= c1m ascle) (go label30))
        (setf iflag (f2cl-lib:int-add iflag 1))
        (setf ascle (f2cl-lib:fref bry (iflag) ((1 3))))
        (setf s1r (* s1r cscrr))
        (setf s1i (* s1i cscrr))
        (setf s2r str)
        (setf s2i sti)
        (setf csclr (* csclr tol))
        (setf cscrr (/ 1.0 csclr))
        (setf s1r (* s1r csclr))
        (setf s1i (* s1i csclr))
        (setf s2r (* s2r csclr))
        (setf s2i (* s2i csclr))
       label30))
    (setf (f2cl-lib:fref yr (n) ((1 n))) (* s2r cscrr))
    (setf (f2cl-lib:fref yi (n) ((1 n))) (* s2i cscrr))
    (if (= n 1) (go end_label))
    (setf nl (f2cl-lib:int-sub n 1))
    (setf fnui (coerce (the f2cl-lib:integer4 nl) 'double-float))
    (setf k nl)
    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                  ((> i nl) nil)
      (tagbody
        (setf str s2r)
        (setf sti s2i)
        (setf s2r (+ (* (+ fnu fnui) (- (* rzr str) (* rzi sti))) s1r))
        (setf s2i (+ (* (+ fnu fnui) (+ (* rzr sti) (* rzi str))) s1i))
        (setf s1r str)
        (setf s1i sti)
        (setf str (* s2r cscrr))
        (setf sti (* s2i cscrr))
        (setf (f2cl-lib:fref yr (k) ((1 n))) str)
        (setf (f2cl-lib:fref yi (k) ((1 n))) sti)
        (setf fnui (- fnui 1.0))
        (setf k (f2cl-lib:int-sub k 1))
        (if (>= iflag 3) (go label40))
        (setf c1r (abs str))
        (setf c1i (abs sti))
        (setf c1m (max c1r c1i))
        (if (<= c1m ascle) (go label40))
        (setf iflag (f2cl-lib:int-add iflag 1))
        (setf ascle (f2cl-lib:fref bry (iflag) ((1 3))))
        (setf s1r (* s1r cscrr))
        (setf s1i (* s1i cscrr))
        (setf s2r str)
        (setf s2i sti)
        (setf csclr (* csclr tol))
        (setf cscrr (/ 1.0 csclr))
        (setf s1r (* s1r csclr))
        (setf s1i (* s1i csclr))
        (setf s2r (* s2r csclr))
        (setf s2i (* s2i csclr))
       label40))
    (go end_label)
   label50
    (setf nz -1)
    (if (= nw -2) (setf nz -2))
    (go end_label)
   label60
    (if (= iform 2) (go label70))
    (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12)
        (zuni1 zr zi fnu kode n yr yi nw nlast fnul tol elim alim)
      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-9 var-10
                       var-11 var-12))
      (setf nw var-7)
      (setf nlast var-8))
    (go label80)
   label70
    (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12)
        (zuni2 zr zi fnu kode n yr yi nw nlast fnul tol elim alim)
      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-9 var-10
                       var-11 var-12))
      (setf nw var-7)
      (setf nlast var-8))
   label80
    (if (< nw 0) (go label50))
    (setf nz nw)
    (go end_label)
   label90
    (setf nlast n)
    (go end_label)
   end_label
    (return (values nil nil nil nil nil nil nil nz nil nlast nil nil nil nil))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zbuni fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (simple-array double-float (*))
                        (simple-array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (double-float) (double-float))
           :return-values '(nil nil nil nil nil nil nil fortran-to-lisp::nz nil
                            fortran-to-lisp::nlast nil nil nil nil)
           :calls '(fortran-to-lisp::d1mach fortran-to-lisp::zabs
                    fortran-to-lisp::zuni2 fortran-to-lisp::zuni1))))

