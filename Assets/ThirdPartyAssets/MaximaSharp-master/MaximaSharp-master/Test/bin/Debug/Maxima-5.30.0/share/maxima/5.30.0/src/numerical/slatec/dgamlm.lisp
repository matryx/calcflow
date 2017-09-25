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


(defun dgamlm (xmin xmax)
  (declare (type (double-float) xmax xmin))
  (prog ((alnbig 0.0) (alnsml 0.0) (xln 0.0) (xold 0.0) (i 0))
    (declare (type (f2cl-lib:integer4) i)
             (type (double-float) xold xln alnsml alnbig))
    (setf alnsml (f2cl-lib:flog (f2cl-lib:d1mach 1)))
    (setf xmin (- alnsml))
    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                  ((> i 10) nil)
      (tagbody
        (setf xold xmin)
        (setf xln (f2cl-lib:flog xmin))
        (setf xmin
                (+ xmin
                   (/
                    (* (- xmin)
                       (+ (- (* (+ xmin 0.5) xln) xmin 0.2258) alnsml))
                    (+ (* xmin xln) 0.5))))
        (if (< (abs (- xmin xold)) 0.005) (go label20))
       label10))
    (xermsg "SLATEC" "DGAMLM" "UNABLE TO FIND XMIN" 1 2)
   label20
    (setf xmin (- 0.01 xmin))
    (setf alnbig (f2cl-lib:flog (f2cl-lib:d1mach 2)))
    (setf xmax alnbig)
    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                  ((> i 10) nil)
      (tagbody
        (setf xold xmax)
        (setf xln (f2cl-lib:flog xmax))
        (setf xmax
                (+ xmax
                   (/
                    (* (- xmax)
                       (- (+ (- (* (- xmax 0.5) xln) xmax) 0.9189) alnbig))
                    (- (* xmax xln) 0.5))))
        (if (< (abs (- xmax xold)) 0.005) (go label40))
       label30))
    (xermsg "SLATEC" "DGAMLM" "UNABLE TO FIND XMAX" 2 2)
   label40
    (setf xmax (- xmax 0.01))
    (setf xmin (max xmin (- 1.0 xmax)))
    (go end_label)
   end_label
    (return (values xmin xmax))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgamlm
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float))
           :return-values '(fortran-to-lisp::xmin fortran-to-lisp::xmax)
           :calls '(fortran-to-lisp::xermsg fortran-to-lisp::d1mach))))

