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
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((x
       (make-array 11
                   :element-type 'double-float
                   :initial-contents '(0.9914448613738104 0.9659258262890683
                                       0.9238795325112867 0.8660254037844386
                                       0.7933533402912352 0.7071067811865476
                                       0.6087614290087207 0.5
                                       0.3826834323650898 0.25881904510252074
                                       0.1305261922200516))))
  (declare (type (array double-float (11)) x))
  (defun dqc25c (f a b c result abserr krul neval)
    (declare (type (f2cl-lib:integer4) neval krul)
             (type (double-float) abserr result c b a))
    (prog ((fval (make-array 25 :element-type 'double-float))
           (cheb12 (make-array 13 :element-type 'double-float))
           (cheb24 (make-array 25 :element-type 'double-float)) (i 0) (isym 0)
           (k 0) (kp 0) (ak22 0.0) (amom0 0.0) (amom1 0.0) (amom2 0.0) (cc 0.0)
           (centr 0.0) (hlgth 0.0) (p2 0.0) (p3 0.0) (p4 0.0) (resabs 0.0)
           (resasc 0.0) (res12 0.0) (res24 0.0) (u 0.0))
      (declare (type (array double-float (25)) fval cheb24)
               (type (array double-float (13)) cheb12)
               (type (double-float) u res24 res12 resasc resabs p4 p3 p2 hlgth
                                    centr cc amom2 amom1 amom0 ak22)
               (type (f2cl-lib:integer4) kp k isym i))
      (setf cc (/ (- (* 2.0 c) b a) (- b a)))
      (if (< (abs cc) 1.1) (go label10))
      (setf krul (f2cl-lib:int-sub krul 1))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12)
          (dqk15w f #'dqwgtc c p2 p3 p4 kp a b result abserr resabs resasc)
        (declare (ignore var-0 var-1 var-7 var-8))
        (setf c var-2)
        (setf p2 var-3)
        (setf p3 var-4)
        (setf p4 var-5)
        (setf kp var-6)
        (setf result var-9)
        (setf abserr var-10)
        (setf resabs var-11)
        (setf resasc var-12))
      (setf neval 15)
      (if (= resasc abserr) (setf krul (f2cl-lib:int-add krul 1)))
      (go label50)
     label10
      (setf hlgth (* 0.5 (- b a)))
      (setf centr (* 0.5 (+ b a)))
      (setf neval 25)
      (setf (f2cl-lib:fref fval (1) ((1 25)))
              (* 0.5 (funcall f (+ hlgth centr))))
      (setf (f2cl-lib:fref fval (13) ((1 25)))
              (multiple-value-bind (ret-val var-0)
                  (funcall f centr)
                (declare (ignore))
                (when var-0
                  (setf centr var-0))
                ret-val))
      (setf (f2cl-lib:fref fval (25) ((1 25)))
              (* 0.5 (funcall f (- centr hlgth))))
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                    ((> i 12) nil)
        (tagbody
          (setf u
                  (* hlgth
                     (f2cl-lib:fref x ((f2cl-lib:int-sub i 1)) ((1 11)))))
          (setf isym (f2cl-lib:int-sub 26 i))
          (setf (f2cl-lib:fref fval (i) ((1 25))) (funcall f (+ u centr)))
          (setf (f2cl-lib:fref fval (isym) ((1 25))) (funcall f (- centr u)))
         label20))
      (dqcheb x fval cheb12 cheb24)
      (setf amom0 (f2cl-lib:flog (abs (/ (- 1.0 cc) (+ 1.0 cc)))))
      (setf amom1 (+ 2.0 (* cc amom0)))
      (setf res12
              (+ (* (f2cl-lib:fref cheb12 (1) ((1 13))) amom0)
                 (* (f2cl-lib:fref cheb12 (2) ((1 13))) amom1)))
      (setf res24
              (+ (* (f2cl-lib:fref cheb24 (1) ((1 25))) amom0)
                 (* (f2cl-lib:fref cheb24 (2) ((1 25))) amom1)))
      (f2cl-lib:fdo (k 3 (f2cl-lib:int-add k 1))
                    ((> k 13) nil)
        (tagbody
          (setf amom2 (- (* 2.0 cc amom1) amom0))
          (setf ak22
                  (coerce
                   (the f2cl-lib:integer4
                        (f2cl-lib:int-mul (f2cl-lib:int-sub k 2)
                                          (f2cl-lib:int-sub k 2)))
                   'double-float))
          (if (= (* (the f2cl-lib:integer4 (truncate k 2)) 2) k)
              (setf amom2 (+ amom2 (/ -4.0 (- ak22 1.0)))))
          (setf res12 (+ res12 (* (f2cl-lib:fref cheb12 (k) ((1 13))) amom2)))
          (setf res24 (+ res24 (* (f2cl-lib:fref cheb24 (k) ((1 25))) amom2)))
          (setf amom0 amom1)
          (setf amom1 amom2)
         label30))
      (f2cl-lib:fdo (k 14 (f2cl-lib:int-add k 1))
                    ((> k 25) nil)
        (tagbody
          (setf amom2 (- (* 2.0 cc amom1) amom0))
          (setf ak22
                  (coerce
                   (the f2cl-lib:integer4
                        (f2cl-lib:int-mul (f2cl-lib:int-sub k 2)
                                          (f2cl-lib:int-sub k 2)))
                   'double-float))
          (if (= (* (the f2cl-lib:integer4 (truncate k 2)) 2) k)
              (setf amom2 (+ amom2 (/ -4.0 (- ak22 1.0)))))
          (setf res24 (+ res24 (* (f2cl-lib:fref cheb24 (k) ((1 25))) amom2)))
          (setf amom0 amom1)
          (setf amom1 amom2)
         label40))
      (setf result res24)
      (setf abserr (abs (- res24 res12)))
     label50
      (go end_label)
     end_label
      (return (values nil nil nil c result abserr krul neval)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dqc25c
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (double-float) (double-float) (double-float)
                        (double-float) (double-float)
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil fortran-to-lisp::c
                            fortran-to-lisp::result fortran-to-lisp::abserr
                            fortran-to-lisp::krul fortran-to-lisp::neval)
           :calls '(fortran-to-lisp::dqcheb fortran-to-lisp::dqk15w))))

