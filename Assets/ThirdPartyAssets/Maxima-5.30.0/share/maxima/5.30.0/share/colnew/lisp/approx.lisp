;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.221 2010/05/26 19:25:52 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.204 2010/02/23 05:21:30 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.114 2010/05/17 01:42:14 rtoy Exp $")

;;; Using Lisp CMU Common Lisp CVS Head 2010-05-25 18:21:07 (20A Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :colnew)


(defun approx (i x zval a coef xi n z dmz k ncomp mmax m mstar mode dmval modm)
  (declare (type (array f2cl-lib:integer4 (*)) m)
           (type (array double-float (*)) a)
           (type (array double-float (*)) dmval dmz z xi coef zval)
           (type double-float x)
           (type (f2cl-lib:integer4) modm mode mstar mmax ncomp k n i))
  (let ()
    (symbol-macrolet ((precis (aref (colout-part-0 *colout-common-block*) 0))
                      (iout (aref (colout-part-1 *colout-common-block*) 0))
                      (iprint (aref (colout-part-1 *colout-common-block*) 1)))
      (f2cl-lib:with-multi-array-data
          ((zval double-float zval-%data% zval-%offset%)
           (coef double-float coef-%data% coef-%offset%)
           (xi double-float xi-%data% xi-%offset%)
           (z double-float z-%data% z-%offset%)
           (dmz double-float dmz-%data% dmz-%offset%)
           (dmval double-float dmval-%data% dmval-%offset%)
           (a double-float a-%data% a-%offset%)
           (m f2cl-lib:integer4 m-%data% m-%offset%))
        (prog ((fact 0.0) (lb 0) (ll 0) (zsum 0.0) (ind 0) (mj 0) (jcomp 0)
               (idmz 0) (ir 0) (s 0.0) (iright 0) (l 0) (ileft 0) (j 0) (iz 0)
               (dm (make-array 7 :element-type 'double-float))
               (bm (make-array 4 :element-type 'double-float)))
          (declare (type (array double-float (4)) bm)
                   (type (array double-float (7)) dm)
                   (type (f2cl-lib:integer4) iz j ileft l iright ir idmz jcomp
                                             mj ind ll lb)
                   (type double-float s zsum fact))
          (f2cl-lib:computed-goto (label10 label30 label80 label90) mode)
         label10
          (setf x (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%))
          (setf iz (f2cl-lib:int-mul (f2cl-lib:int-sub i 1) mstar))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j mstar) nil)
            (tagbody
              (setf iz (f2cl-lib:int-add iz 1))
              (setf (f2cl-lib:fref zval-%data% (j) ((1 1)) zval-%offset%)
                      (f2cl-lib:fref z-%data% (iz) ((1 1)) z-%offset%))
             label20))
          (go end_label)
         label30
          (if
           (and
            (>= x (- (f2cl-lib:fref xi-%data% (1) ((1 1)) xi-%offset%) precis))
            (<= x
                (+
                 (f2cl-lib:fref xi-%data%
                                ((f2cl-lib:int-add n 1))
                                ((1 1))
                                xi-%offset%)
                 precis)))
           (go label40))
          (if (< iprint 1)
              (f2cl-lib:fformat iout
                                (" ****** DOMAIN ERROR IN APPROX ******" "~%"
                                 " X =" 1 (("~20,10,2,0,'*,,'DE")) "   ALEFT ="
                                 1 (("~20,10,2,0,'*,,'DE")) "   ARIGHT =" 1
                                 (("~20,10,2,0,'*,,'DE")) "~%")
                                x
                                (f2cl-lib:fref xi-%data%
                                               (1)
                                               ((1 1))
                                               xi-%offset%)
                                (f2cl-lib:fref xi-%data%
                                               ((f2cl-lib:int-add n 1))
                                               ((1 1))
                                               xi-%offset%)))
          (if (< x (f2cl-lib:fref xi-%data% (1) ((1 1)) xi-%offset%))
              (setf x (f2cl-lib:fref xi-%data% (1) ((1 1)) xi-%offset%)))
          (if
           (> x
              (f2cl-lib:fref xi-%data%
                             ((f2cl-lib:int-add n 1))
                             ((1 1))
                             xi-%offset%))
           (setf x
                   (f2cl-lib:fref xi-%data%
                                  ((f2cl-lib:int-add n 1))
                                  ((1 1))
                                  xi-%offset%)))
         label40
          (if (or (> i n) (< i 1))
              (setf i (the f2cl-lib:integer4 (truncate (+ n 1) 2))))
          (setf ileft i)
          (if (< x (f2cl-lib:fref xi-%data% (ileft) ((1 1)) xi-%offset%))
              (go label60))
          (f2cl-lib:fdo (l ileft (f2cl-lib:int-add l 1))
                        ((> l n) nil)
            (tagbody
              (setf i l)
              (if
               (< x
                  (f2cl-lib:fref xi-%data%
                                 ((f2cl-lib:int-add l 1))
                                 ((1 1))
                                 xi-%offset%))
               (go label80))
             label50))
          (go label80)
         label60
          (setf iright (f2cl-lib:int-sub ileft 1))
          (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                        ((> l iright) nil)
            (tagbody
              (setf i (f2cl-lib:int-sub (f2cl-lib:int-add iright 1) l))
              (if (>= x (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%))
                  (go label80))
             label70))
         label80
          (setf s
                  (/ (- x (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%))
                     (-
                      (f2cl-lib:fref xi-%data%
                                     ((f2cl-lib:int-add i 1))
                                     ((1 1))
                                     xi-%offset%)
                      (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%))))
          (rkbas s coef k mmax a dm modm)
         label90
          (setf (f2cl-lib:fref bm (1) ((1 4)))
                  (- x (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%)))
          (f2cl-lib:fdo (l 2 (f2cl-lib:int-add l 1))
                        ((> l mmax) nil)
            (tagbody
              (setf (f2cl-lib:fref bm (l) ((1 4)))
                      (/ (f2cl-lib:fref bm (1) ((1 4))) (f2cl-lib:dfloat l)))
             label95))
         label100
          (setf ir 1)
          (setf iz
                  (f2cl-lib:int-add
                   (f2cl-lib:int-mul (f2cl-lib:int-sub i 1) mstar)
                   1))
          (setf idmz (f2cl-lib:int-mul (f2cl-lib:int-sub i 1) k ncomp))
          (f2cl-lib:fdo (jcomp 1 (f2cl-lib:int-add jcomp 1))
                        ((> jcomp ncomp) nil)
            (tagbody
              (setf mj (f2cl-lib:fref m-%data% (jcomp) ((1 1)) m-%offset%))
              (setf ir (f2cl-lib:int-add ir mj))
              (setf iz (f2cl-lib:int-add iz mj))
              (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                            ((> l mj) nil)
                (tagbody
                  (setf ind (f2cl-lib:int-add idmz jcomp))
                  (setf zsum 0.0)
                  (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                ((> j k) nil)
                    (tagbody
                      (setf zsum
                              (+ zsum
                                 (*
                                  (f2cl-lib:fref a-%data%
                                                 (j l)
                                                 ((1 7) (1 1))
                                                 a-%offset%)
                                  (f2cl-lib:fref dmz-%data%
                                                 (ind)
                                                 ((1 1))
                                                 dmz-%offset%))))
                     label110
                      (setf ind (f2cl-lib:int-add ind ncomp))))
                  (f2cl-lib:fdo (ll 1 (f2cl-lib:int-add ll 1))
                                ((> ll l) nil)
                    (tagbody
                      (setf lb (f2cl-lib:int-sub (f2cl-lib:int-add l 1) ll))
                     label120
                      (setf zsum
                              (+ (* zsum (f2cl-lib:fref bm (lb) ((1 4))))
                                 (f2cl-lib:fref z-%data%
                                                ((f2cl-lib:int-sub iz ll))
                                                ((1 1))
                                                z-%offset%)))))
                 label130
                  (setf (f2cl-lib:fref zval-%data%
                                       ((f2cl-lib:int-sub ir l))
                                       ((1 1))
                                       zval-%offset%)
                          zsum)))
             label140))
          (if (= modm 0) (go end_label))
          (f2cl-lib:fdo (jcomp 1 (f2cl-lib:int-add jcomp 1))
                        ((> jcomp ncomp) nil)
            (tagbody
             label150
              (setf (f2cl-lib:fref dmval-%data% (jcomp) ((1 1)) dmval-%offset%)
                      0.0)))
          (setf idmz (f2cl-lib:int-add idmz 1))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j k) nil)
            (tagbody
              (setf fact (f2cl-lib:fref dm (j) ((1 7))))
              (f2cl-lib:fdo (jcomp 1 (f2cl-lib:int-add jcomp 1))
                            ((> jcomp ncomp) nil)
                (tagbody
                  (setf (f2cl-lib:fref dmval-%data%
                                       (jcomp)
                                       ((1 1))
                                       dmval-%offset%)
                          (+
                           (f2cl-lib:fref dmval-%data%
                                          (jcomp)
                                          ((1 1))
                                          dmval-%offset%)
                           (* fact
                              (f2cl-lib:fref dmz-%data%
                                             (idmz)
                                             ((1 1))
                                             dmz-%offset%))))
                  (setf idmz (f2cl-lib:int-add idmz 1))
                 label160))
             label170))
          (go end_label)
         end_label
          (return
           (values i
                   x
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::approx
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) double-float
                        (array double-float (1)) (array double-float (7))
                        (array double-float (1)) (array double-float (1))
                        (fortran-to-lisp::integer4) (array double-float (1))
                        (array double-float (1)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (1))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (1)) (fortran-to-lisp::integer4))
           :return-values '(fortran-to-lisp::i fortran-to-lisp::x nil nil nil
                            nil nil nil nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::rkbas))))

