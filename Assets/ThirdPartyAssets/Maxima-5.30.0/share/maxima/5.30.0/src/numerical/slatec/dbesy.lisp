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


(let ((nulim
       (make-array 2
                   :element-type 'f2cl-lib:integer4
                   :initial-contents '(70 100))))
  (declare (type (simple-array f2cl-lib:integer4 (2)) nulim))
  (defun dbesy (x fnu n y)
    (declare (type (simple-array double-float (*)) y)
             (type (f2cl-lib:integer4) n)
             (type (double-float) fnu x))
    (prog ((w (make-array 2 :element-type 'double-float))
           (wk (make-array 7 :element-type 'double-float)) (azn 0.0) (cn 0.0)
           (dnu 0.0) (elim 0.0) (flgjy 0.0) (fn 0.0) (ran 0.0) (s 0.0) (s1 0.0)
           (s2 0.0) (tm 0.0) (trx 0.0) (w2n 0.0) (xlim 0.0) (xxn 0.0) (i 0)
           (iflw 0) (j 0) (nb 0) (nd 0) (nn 0) (nud 0))
      (declare (type (f2cl-lib:integer4) nud nn nd nb j iflw i)
               (type (simple-array double-float (7)) wk)
               (type (simple-array double-float (2)) w)
               (type (double-float) xxn xlim w2n trx tm s2 s1 s ran fn flgjy
                                    elim dnu cn azn))
      (setf nn (f2cl-lib:int-sub (f2cl-lib:i1mach 15)))
      (setf elim (* 2.303 (- (* nn (f2cl-lib:d1mach 5)) 3.0)))
      (setf xlim (* (f2cl-lib:d1mach 1) 1000.0))
      (if (< fnu 0.0) (go label140))
      (if (<= x 0.0) (go label150))
      (if (< x xlim) (go label170))
      (if (< n 1) (go label160))
      (setf nd n)
      (setf nud (f2cl-lib:int fnu))
      (setf dnu (- fnu nud))
      (setf nn (min (the f2cl-lib:integer4 2) (the f2cl-lib:integer4 nd)))
      (setf fn (- (+ fnu n) 1))
      (if (< fn 2.0) (go label100))
      (setf xxn (/ x fn))
      (setf w2n (- 1.0 (* xxn xxn)))
      (if (<= w2n 0.0) (go label10))
      (setf ran (f2cl-lib:fsqrt w2n))
      (setf azn (- (f2cl-lib:flog (/ (+ 1.0 ran) xxn)) ran))
      (setf cn (* fn azn))
      (if (> cn elim) (go label170))
     label10
      (if (< nud (f2cl-lib:fref nulim (nn) ((1 2)))) (go label20))
      (setf flgjy -1.0)
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
          (dasyjy #'dyairy x fnu flgjy nn y wk iflw)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
        (setf iflw var-7))
      (if (/= iflw 0) (go label170))
      (if (= nn 1) (go end_label))
      (setf trx (/ 2.0 x))
      (setf tm (/ (+ fnu fnu 2.0) x))
      (go label80)
     label20
      (if (/= dnu 0.0) (go label30))
      (setf s1 (dbesy0 x))
      (if (and (= nud 0) (= nd 1)) (go label70))
      (setf s2 (dbesy1 x))
      (go label40)
     label30
      (setf nb 2)
      (if (and (= nud 0) (= nd 1)) (setf nb 1))
      (dbsynu x dnu nb w)
      (setf s1 (f2cl-lib:fref w (1) ((1 2))))
      (if (= nb 1) (go label70))
      (setf s2 (f2cl-lib:fref w (2) ((1 2))))
     label40
      (setf trx (/ 2.0 x))
      (setf tm (/ (+ dnu dnu 2.0) x))
      (if (= nd 1) (setf nud (f2cl-lib:int-sub nud 1)))
      (if (> nud 0) (go label50))
      (if (> nd 1) (go label70))
      (setf s1 s2)
      (go label70)
     label50
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i nud) nil)
        (tagbody
          (setf s s2)
          (setf s2 (- (* tm s2) s1))
          (setf s1 s)
          (setf tm (+ tm trx))
         label60))
      (if (= nd 1) (setf s1 s2))
     label70
      (setf (f2cl-lib:fref y (1) ((1 *))) s1)
      (if (= nd 1) (go end_label))
      (setf (f2cl-lib:fref y (2) ((1 *))) s2)
     label80
      (if (= nd 2) (go end_label))
      (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 1))
                    ((> i nd) nil)
        (tagbody
          (setf (f2cl-lib:fref y (i) ((1 *)))
                  (- (* tm (f2cl-lib:fref y ((f2cl-lib:int-sub i 1)) ((1 *))))
                     (f2cl-lib:fref y ((f2cl-lib:int-sub i 2)) ((1 *)))))
          (setf tm (+ tm trx))
         label90))
      (go end_label)
     label100
      (if (<= fn 1.0) (go label110))
      (if (> (* (- fn) (- (f2cl-lib:flog x) 0.693)) elim) (go label170))
     label110
      (if (= dnu 0.0) (go label120))
      (dbsynu x fnu nd y)
      (go end_label)
     label120
      (setf j nud)
      (if (= j 1) (go label130))
      (setf j (f2cl-lib:int-add j 1))
      (setf (f2cl-lib:fref y (j) ((1 *))) (dbesy0 x))
      (if (= nd 1) (go end_label))
      (setf j (f2cl-lib:int-add j 1))
     label130
      (setf (f2cl-lib:fref y (j) ((1 *))) (dbesy1 x))
      (if (= nd 1) (go end_label))
      (setf trx (/ 2.0 x))
      (setf tm trx)
      (go label80)
     label140
      (xermsg "SLATEC" "DBESY" "ORDER, FNU, LESS THAN ZERO" 2 1)
      (go end_label)
     label150
      (xermsg "SLATEC" "DBESY" "X LESS THAN OR EQUAL TO ZERO" 2 1)
      (go end_label)
     label160
      (xermsg "SLATEC" "DBESY" "N LESS THAN ONE" 2 1)
      (go end_label)
     label170
      (xermsg "SLATEC" "DBESY" "OVERFLOW, FNU OR N TOO LARGE OR X TOO SMALL" 6
       1)
      (go end_label)
     end_label
      (return (values nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dbesy fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float)
                        (fortran-to-lisp::integer4)
                        (simple-array double-float (*)))
           :return-values '(nil nil nil nil)
           :calls '(fortran-to-lisp::xermsg fortran-to-lisp::dbsynu
                    fortran-to-lisp::dbesy1 fortran-to-lisp::dbesy0
                    fortran-to-lisp::dasyjy fortran-to-lisp::d1mach
                    fortran-to-lisp::i1mach))))

