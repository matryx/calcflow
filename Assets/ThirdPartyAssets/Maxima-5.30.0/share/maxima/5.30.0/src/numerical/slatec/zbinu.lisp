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


(let ((zeror 0.0) (zeroi 0.0))
  (declare (type (double-float) zeror zeroi))
  (defun zbinu (zr zi fnu kode n cyr cyi nz rl fnul tol elim alim)
    (declare (type (simple-array double-float (*)) cyi cyr)
             (type (f2cl-lib:integer4) nz n kode)
             (type (double-float) alim elim tol fnul rl fnu zi zr))
    (prog ((cwr (make-array 2 :element-type 'double-float))
           (cwi (make-array 2 :element-type 'double-float)) (i 0) (inw 0)
           (nlast 0) (nn 0) (nui 0) (nw 0) (az 0.0) (dfnu 0.0))
      (declare (type (simple-array double-float (2)) cwr cwi)
               (type (double-float) dfnu az)
               (type (f2cl-lib:integer4) nw nui nn nlast inw i))
      (setf nz 0)
      (setf az (coerce (realpart (zabs zr zi)) 'double-float))
      (setf nn n)
      (setf dfnu (+ fnu (f2cl-lib:int-sub n 1)))
      (if (<= az 2.0) (go label10))
      (if (> (* az az 0.25) (+ dfnu 1.0)) (go label20))
     label10
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
             var-10)
          (zseri zr zi fnu kode nn cyr cyi nw tol elim alim)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-8 var-9
                         var-10))
        (setf nw var-7))
      (setf inw (abs nw))
      (setf nz (f2cl-lib:int-add nz inw))
      (setf nn (f2cl-lib:int-sub nn inw))
      (if (= nn 0) (go end_label))
      (if (>= nw 0) (go label120))
      (setf dfnu (+ fnu (f2cl-lib:int-sub nn 1)))
     label20
      (if (< az rl) (go label40))
      (if (<= dfnu 1.0) (go label30))
      (if (< (+ az az) (* dfnu dfnu)) (go label50))
     label30
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11)
          (zasyi zr zi fnu kode nn cyr cyi nw rl tol elim alim)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-8 var-9
                         var-10 var-11))
        (setf nw var-7))
      (if (< nw 0) (go label130))
      (go label120)
     label40
      (if (<= dfnu 1.0) (go label70))
     label50
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11)
          (zuoik zr zi fnu kode 1 nn cyr cyi nw tol elim alim)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-9
                         var-10 var-11))
        (setf nw var-8))
      (if (< nw 0) (go label130))
      (setf nz (f2cl-lib:int-add nz nw))
      (setf nn (f2cl-lib:int-sub nn nw))
      (if (= nn 0) (go end_label))
      (setf dfnu (+ fnu (f2cl-lib:int-sub nn 1)))
      (if (> dfnu fnul) (go label110))
      (if (> az fnul) (go label110))
     label60
      (if (> az rl) (go label80))
     label70
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
          (zmlri zr zi fnu kode nn cyr cyi nw tol)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-8))
        (setf nw var-7))
      (if (< nw 0) (go label130))
      (go label120)
     label80
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11)
          (zuoik zr zi fnu kode 2 2 cwr cwi nw tol elim alim)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-9
                         var-10 var-11))
        (setf nw var-8))
      (if (>= nw 0) (go label100))
      (setf nz nn)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i nn) nil)
        (tagbody
          (setf (f2cl-lib:fref cyr (i) ((1 n))) zeror)
          (setf (f2cl-lib:fref cyi (i) ((1 n))) zeroi)
         label90))
      (go end_label)
     label100
      (if (> nw 0) (go label130))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12)
          (zwrsk zr zi fnu kode nn cyr cyi nw cwr cwi tol elim alim)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-8 var-9
                         var-10 var-11 var-12))
        (setf nw var-7))
      (if (< nw 0) (go label130))
      (go label120)
     label110
      (setf nui (f2cl-lib:int (+ (- fnul dfnu) 1)))
      (setf nui (max (the f2cl-lib:integer4 nui) (the f2cl-lib:integer4 0)))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12 var-13)
          (zbuni zr zi fnu kode nn cyr cyi nw nui nlast fnul tol elim alim)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-8 var-10
                         var-11 var-12 var-13))
        (setf nw var-7)
        (setf nlast var-9))
      (if (< nw 0) (go label130))
      (setf nz (f2cl-lib:int-add nz nw))
      (if (= nlast 0) (go label120))
      (setf nn nlast)
      (go label60)
     label120
      (go end_label)
     label130
      (setf nz -1)
      (if (= nw -2) (setf nz -2))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nz nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zbinu fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (simple-array double-float (*))
                        (simple-array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (double-float) (double-float)
                        (double-float))
           :return-values '(nil nil nil nil nil nil nil fortran-to-lisp::nz nil
                            nil nil nil nil)
           :calls '(fortran-to-lisp::zbuni fortran-to-lisp::zwrsk
                    fortran-to-lisp::zmlri fortran-to-lisp::zuoik
                    fortran-to-lisp::zasyi fortran-to-lisp::zseri
                    fortran-to-lisp::zabs))))

