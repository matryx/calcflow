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


(defun errchk (xi z dmz valstr ifin)
  (declare (type (f2cl-lib:integer4) ifin)
           (type (array double-float (*)) valstr dmz z xi))
  (let ((colord-m
         (make-array 20
                     :element-type 'f2cl-lib:integer4
                     :displaced-to (colord-part-0 *colord-common-block*)
                     :displaced-index-offset 5))
        (colbas-asave
         (make-array 112
                     :element-type 'double-float
                     :displaced-to (colbas-part-0 *colbas-common-block*)
                     :displaced-index-offset 224))
        (colest-wgterr
         (make-array 40
                     :element-type 'double-float
                     :displaced-to (colest-part-0 *colest-common-block*)
                     :displaced-index-offset 80))
        (colest-tolin
         (make-array 40
                     :element-type 'double-float
                     :displaced-to (colest-part-0 *colest-common-block*)
                     :displaced-index-offset 120))
        (colest-ltol
         (make-array 40
                     :element-type 'f2cl-lib:integer4
                     :displaced-to (colest-part-1 *colest-common-block*)
                     :displaced-index-offset 40)))
    (symbol-macrolet ((iout (aref (colout-part-1 *colout-common-block*) 0))
                      (iprint (aref (colout-part-1 *colout-common-block*) 1))
                      (k (aref (colord-part-0 *colord-common-block*) 0))
                      (ncomp (aref (colord-part-0 *colord-common-block*) 1))
                      (mstar (aref (colord-part-0 *colord-common-block*) 2))
                      (mmax (aref (colord-part-0 *colord-common-block*) 4))
                      (m colord-m)
                      (n (aref (colapr-part-0 *colapr-common-block*) 0))
                      (mshflg (aref (colmsh-part-0 *colmsh-common-block*) 0))
                      (asave colbas-asave)
                      (wgterr colest-wgterr)
                      (tolin colest-tolin)
                      (ltol colest-ltol)
                      (ntol (aref (colest-part-1 *colest-common-block*) 80)))
      (f2cl-lib:with-multi-array-data
          ((xi double-float xi-%data% xi-%offset%)
           (z double-float z-%data% z-%offset%)
           (dmz double-float dmz-%data% dmz-%offset%)
           (valstr double-float valstr-%data% valstr-%offset%))
        (prog ((mj 0) (lj 0) (ltjz 0) (ltolj 0) (l 0) (x 0.0) (kstore 0)
               (knew 0) (i 0) (iback 0) (j 0)
               (dummy (make-array 1 :element-type 'double-float))
               (errest (make-array 40 :element-type 'double-float))
               (err (make-array 40 :element-type 'double-float)))
          (declare (type (array double-float (40)) err errest)
                   (type (array double-float (1)) dummy)
                   (type double-float x)
                   (type (f2cl-lib:integer4) j iback i knew kstore l ltolj ltjz
                                             lj mj))
          (setf ifin 1)
          (setf mshflg 1)
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j mstar) nil)
            (tagbody label10 (setf (f2cl-lib:fref errest (j) ((1 40))) 0.0)))
          (f2cl-lib:fdo (iback 1 (f2cl-lib:int-add iback 1))
                        ((> iback n) nil)
            (tagbody
              (setf i (f2cl-lib:int-sub (f2cl-lib:int-add n 1) iback))
              (setf knew
                      (f2cl-lib:int-add
                       (f2cl-lib:int-mul
                        (f2cl-lib:int-add
                         (f2cl-lib:int-mul 4 (f2cl-lib:int-sub i 1))
                         2)
                        mstar)
                       1))
              (setf kstore
                      (f2cl-lib:int-add
                       (f2cl-lib:int-mul
                        (f2cl-lib:int-add
                         (f2cl-lib:int-mul 2 (f2cl-lib:int-sub i 1))
                         1)
                        mstar)
                       1))
              (setf x
                      (+ (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%)
                         (/
                          (*
                           (-
                            (f2cl-lib:fref xi-%data%
                                           ((f2cl-lib:int-add i 1))
                                           ((1 1))
                                           xi-%offset%)
                            (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%))
                           2.0)
                          3.0)))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16)
                  (approx i x
                   (f2cl-lib:array-slice valstr double-float (knew) ((1 1)))
                   (f2cl-lib:array-slice asave
                                         double-float
                                         (1 3)
                                         ((1 28) (1 4)))
                   dummy xi n z dmz k ncomp mmax m mstar 4 dummy 0)
                (declare (ignore var-2 var-3 var-4 var-5 var-6 var-7 var-8
                                 var-9 var-10 var-11 var-12 var-13 var-14
                                 var-15 var-16))
                (setf i var-0)
                (setf x var-1))
              (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                            ((> l mstar) nil)
                (tagbody
                  (setf (f2cl-lib:fref err (l) ((1 40)))
                          (* (f2cl-lib:fref wgterr (l) ((1 40)))
                             (f2cl-lib:dabs
                              (-
                               (f2cl-lib:fref valstr-%data%
                                              (knew)
                                              ((1 1))
                                              valstr-%offset%)
                               (f2cl-lib:fref valstr-%data%
                                              (kstore)
                                              ((1 1))
                                              valstr-%offset%)))))
                  (setf knew (f2cl-lib:int-add knew 1))
                  (setf kstore (f2cl-lib:int-add kstore 1))
                 label20))
              (setf knew
                      (f2cl-lib:int-add
                       (f2cl-lib:int-mul
                        (f2cl-lib:int-add
                         (f2cl-lib:int-mul 4 (f2cl-lib:int-sub i 1))
                         1)
                        mstar)
                       1))
              (setf kstore
                      (f2cl-lib:int-add
                       (f2cl-lib:int-mul 2 (f2cl-lib:int-sub i 1) mstar)
                       1))
              (setf x
                      (+ (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%)
                         (/
                          (-
                           (f2cl-lib:fref xi-%data%
                                          ((f2cl-lib:int-add i 1))
                                          ((1 1))
                                          xi-%offset%)
                           (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%))
                          3.0)))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16)
                  (approx i x
                   (f2cl-lib:array-slice valstr double-float (knew) ((1 1)))
                   (f2cl-lib:array-slice asave
                                         double-float
                                         (1 2)
                                         ((1 28) (1 4)))
                   dummy xi n z dmz k ncomp mmax m mstar 4 dummy 0)
                (declare (ignore var-2 var-3 var-4 var-5 var-6 var-7 var-8
                                 var-9 var-10 var-11 var-12 var-13 var-14
                                 var-15 var-16))
                (setf i var-0)
                (setf x var-1))
              (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                            ((> l mstar) nil)
                (tagbody
                  (setf (f2cl-lib:fref err (l) ((1 40)))
                          (+ (f2cl-lib:fref err (l) ((1 40)))
                             (* (f2cl-lib:fref wgterr (l) ((1 40)))
                                (f2cl-lib:dabs
                                 (-
                                  (f2cl-lib:fref valstr-%data%
                                                 (knew)
                                                 ((1 1))
                                                 valstr-%offset%)
                                  (f2cl-lib:fref valstr-%data%
                                                 (kstore)
                                                 ((1 1))
                                                 valstr-%offset%))))))
                  (setf knew (f2cl-lib:int-add knew 1))
                  (setf kstore (f2cl-lib:int-add kstore 1))
                 label30))
              (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                            ((> l mstar) nil)
                (tagbody
                  (setf (f2cl-lib:fref errest (l) ((1 40)))
                          (f2cl-lib:dmax1 (f2cl-lib:fref errest (l) ((1 40)))
                                          (f2cl-lib:fref err (l) ((1 40)))))
                 label40))
              (if (= ifin 0) (go label60))
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j ntol) nil)
                (tagbody
                  (setf ltolj (f2cl-lib:fref ltol (j) ((1 40))))
                  (setf ltjz
                          (f2cl-lib:int-add ltolj
                                            (f2cl-lib:int-mul
                                             (f2cl-lib:int-sub i 1)
                                             mstar)))
                  (if
                   (> (f2cl-lib:fref err (ltolj) ((1 40)))
                      (* (f2cl-lib:fref tolin (j) ((1 40)))
                         (+
                          (f2cl-lib:dabs
                           (f2cl-lib:fref z-%data% (ltjz) ((1 1)) z-%offset%))
                          1.0)))
                   (setf ifin 0))
                 label50))
             label60))
          (if (>= iprint 0) (go end_label))
          (f2cl-lib:fformat iout ("~%" " THE ESTIMATED ERRORS ARE," "~%"))
          (setf lj 1)
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j ncomp) nil)
            (tagbody
              (setf mj
                      (f2cl-lib:int-add (f2cl-lib:int-sub lj 1)
                                        (f2cl-lib:fref m (j) ((1 20)))))
              (f2cl-lib:fformat iout
                                (" U(" 1 (("~2D")) ") -" 4
                                 (("~12,4,2,0,'*,,'DE")) "~%")
                                j
                                (do ((l lj (f2cl-lib:int-add l 1))
                                     (%ret nil))
                                    ((> l mj) (nreverse %ret))
                                  (declare (type f2cl-lib:integer4 l))
                                  (push (f2cl-lib:fref errest (l) ((1 40)))
                                        %ret)))
              (setf lj (f2cl-lib:int-add mj 1))
             label70))
          (go end_label)
         end_label
          (return (values nil nil nil nil ifin)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::errchk
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (1)) (array double-float (1))
                        (array double-float (1)) (array double-float (1))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil fortran-to-lisp::ifin)
           :calls '(fortran-to-lisp::approx))))

