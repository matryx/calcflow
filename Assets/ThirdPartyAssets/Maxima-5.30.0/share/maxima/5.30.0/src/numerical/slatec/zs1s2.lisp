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
  (defun zs1s2 (zrr zri s1r s1i s2r s2i nz ascle alim iuf)
    (declare (type (f2cl-lib:integer4) iuf nz)
             (type (double-float) alim ascle s2i s2r s1i s1r zri zrr))
    (prog ((idum 0) (aa 0.0) (aln 0.0) (as1 0.0) (as2 0.0) (c1i 0.0) (c1r 0.0)
           (s1di 0.0) (s1dr 0.0))
      (declare (type (double-float) s1dr s1di c1r c1i as2 as1 aln aa)
               (type (f2cl-lib:integer4) idum))
      (setf nz 0)
      (setf as1 (coerce (realpart (zabs s1r s1i)) 'double-float))
      (setf as2 (coerce (realpart (zabs s2r s2i)) 'double-float))
      (if (and (= s1r 0.0) (= s1i 0.0)) (go label10))
      (if (= as1 0.0) (go label10))
      (setf aln (+ (- (- zrr) zrr) (f2cl-lib:flog as1)))
      (setf s1dr s1r)
      (setf s1di s1i)
      (setf s1r zeror)
      (setf s1i zeroi)
      (setf as1 zeror)
      (if (< aln (- alim)) (go label10))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
          (zlog s1dr s1di c1r c1i idum)
        (declare (ignore var-0 var-1))
        (setf c1r var-2)
        (setf c1i var-3)
        (setf idum var-4))
      (setf c1r (- c1r zrr zrr))
      (setf c1i (- c1i zri zri))
      (multiple-value-bind (var-0 var-1 var-2 var-3)
          (zexp c1r c1i s1r s1i)
        (declare (ignore var-0 var-1))
        (setf s1r var-2)
        (setf s1i var-3))
      (setf as1 (coerce (realpart (zabs s1r s1i)) 'double-float))
      (setf iuf (f2cl-lib:int-add iuf 1))
     label10
      (setf aa (max as1 as2))
      (if (> aa ascle) (go end_label))
      (setf s1r zeror)
      (setf s1i zeroi)
      (setf s2r zeror)
      (setf s2i zeroi)
      (setf nz 1)
      (setf iuf 0)
      (go end_label)
     end_label
      (return (values nil nil s1r s1i s2r s2i nz nil nil iuf)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zs1s2 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (fortran-to-lisp::integer4))
           :return-values '(nil nil fortran-to-lisp::s1r fortran-to-lisp::s1i
                            fortran-to-lisp::s2r fortran-to-lisp::s2i
                            fortran-to-lisp::nz nil nil fortran-to-lisp::iuf)
           :calls '(fortran-to-lisp::zexp fortran-to-lisp::zlog
                    fortran-to-lisp::zabs))))

