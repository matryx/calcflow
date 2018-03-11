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


(defun vwblok (xcol hrho jj wi vi ipvtw kd zval df acol dmzo ncomp dfsub msing)
  (declare (type (array double-float (*)) acol)
           (type (array double-float (*)) dmzo zval)
           (type (array f2cl-lib:integer4 (*)) ipvtw)
           (type (array double-float (*)) df vi wi)
           (type (f2cl-lib:integer4) msing ncomp kd jj)
           (type double-float hrho xcol))
  (let ((colord-m
         (make-array 20
                     :element-type 'f2cl-lib:integer4
                     :displaced-to (colord-part-0 *colord-common-block*)
                     :displaced-index-offset 5)))
    (symbol-macrolet ((k (aref (colord-part-0 *colord-common-block*) 0))
                      (mstar (aref (colord-part-0 *colord-common-block*) 2))
                      (mmax (aref (colord-part-0 *colord-common-block*) 4))
                      (m colord-m)
                      (nonlin (aref (colnln-part-0 *colnln-common-block*) 0))
                      (iter (aref (colnln-part-0 *colnln-common-block*) 1)))
      (f2cl-lib:with-multi-array-data
          ((wi double-float wi-%data% wi-%offset%)
           (vi double-float vi-%data% vi-%offset%)
           (df double-float df-%data% df-%offset%)
           (ipvtw f2cl-lib:integer4 ipvtw-%data% ipvtw-%offset%)
           (zval double-float zval-%data% zval-%offset%)
           (dmzo double-float dmzo-%data% dmzo-%offset%)
           (acol double-float acol-%data% acol-%offset%))
        (prog ((bl 0.0) (jdf 0) (ll 0) (lp1 0) (iw 0) (ajl 0.0) (jw 0) (jv 0)
               (mj 0) (jcomp 0) (jn 0) (i2 0) (i1 0) (i0 0) (ir 0) (jcol 0)
               (j 0) (l 0) (fact 0.0) (id 0)
               (basm (make-array 5 :element-type 'double-float))
               (ha (make-array 28 :element-type 'double-float)))
          (declare (type (array double-float (28)) ha)
                   (type (array double-float (5)) basm)
                   (type (f2cl-lib:integer4) id l j jcol ir i0 i1 i2 jn jcomp
                                             mj jv jw iw lp1 ll jdf)
                   (type double-float fact ajl bl))
          (if (> jj 1) (go label30))
          (f2cl-lib:fdo (id 1 (f2cl-lib:int-add id 1))
                        ((> id kd) nil)
            (tagbody
              (setf (f2cl-lib:fref wi-%data%
                                   (id id)
                                   ((1 kd) (1 1))
                                   wi-%offset%)
                      1.0)
             label10))
         label30
          (setf fact 1.0)
          (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                        ((> l mmax) nil)
            (tagbody
              (setf fact (/ (* fact hrho) (f2cl-lib:dfloat l)))
              (setf (f2cl-lib:fref basm (l) ((1 5))) fact)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (setf (f2cl-lib:fref ha (j l) ((1 7) (1 4)))
                          (* fact
                             (f2cl-lib:fref acol-%data%
                                            (j l)
                                            ((1 7) (1 4))
                                            acol-%offset%)))
                 label150))))
         label150
          (f2cl-lib:fdo (jcol 1 (f2cl-lib:int-add jcol 1))
                        ((> jcol mstar) nil)
            (tagbody
              (f2cl-lib:fdo (ir 1 (f2cl-lib:int-add ir 1))
                            ((> ir ncomp) nil)
                (tagbody
                  (setf (f2cl-lib:fref df-%data%
                                       (ir jcol)
                                       ((1 ncomp) (1 1))
                                       df-%offset%)
                          0.0)))))
         label40
          (multiple-value-bind (var-0 var-1 var-2)
              (funcall dfsub xcol zval df)
            (declare (ignore var-1 var-2))
            (when var-0
              (setf xcol var-0)))
          (setf i0 (f2cl-lib:int-mul (f2cl-lib:int-sub jj 1) ncomp))
          (setf i1 (f2cl-lib:int-add i0 1))
          (setf i2 (f2cl-lib:int-add i0 ncomp))
          (if (or (= nonlin 0) (> iter 0)) (go label60))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j mstar) nil)
            (tagbody
              (setf fact
                      (-
                       (f2cl-lib:fref zval-%data% (j) ((1 1)) zval-%offset%)))
              (f2cl-lib:fdo (id 1 (f2cl-lib:int-add id 1))
                            ((> id ncomp) nil)
                (tagbody
                  (setf (f2cl-lib:fref dmzo-%data%
                                       ((f2cl-lib:int-add i0 id))
                                       ((1 1))
                                       dmzo-%offset%)
                          (+
                           (f2cl-lib:fref dmzo-%data%
                                          ((f2cl-lib:int-add i0 id))
                                          ((1 1))
                                          dmzo-%offset%)
                           (* fact
                              (f2cl-lib:fref df-%data%
                                             (id j)
                                             ((1 ncomp) (1 1))
                                             df-%offset%))))
                 label50))))
         label50
         label60
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j mstar) nil)
            (tagbody
              (f2cl-lib:fdo (id 1 (f2cl-lib:int-add id 1))
                            ((> id ncomp) nil)
                (tagbody
                  (setf (f2cl-lib:fref vi-%data%
                                       ((f2cl-lib:int-add i0 id) j)
                                       ((1 kd) (1 1))
                                       vi-%offset%)
                          (f2cl-lib:fref df-%data%
                                         (id j)
                                         ((1 ncomp) (1 1))
                                         df-%offset%))
                 label70))))
         label70
          (setf jn 1)
          (f2cl-lib:fdo (jcomp 1 (f2cl-lib:int-add jcomp 1))
                        ((> jcomp ncomp) nil)
            (tagbody
              (setf mj (f2cl-lib:fref m (jcomp) ((1 20))))
              (setf jn (f2cl-lib:int-add jn mj))
              (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                            ((> l mj) nil)
                (tagbody
                  (setf jv (f2cl-lib:int-sub jn l))
                  (setf jw jcomp)
                  (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                ((> j k) nil)
                    (tagbody
                      (setf ajl (- (f2cl-lib:fref ha (j l) ((1 7) (1 4)))))
                      (f2cl-lib:fdo (iw i1 (f2cl-lib:int-add iw 1))
                                    ((> iw i2) nil)
                        (tagbody
                          (setf (f2cl-lib:fref wi-%data%
                                               (iw jw)
                                               ((1 kd) (1 1))
                                               wi-%offset%)
                                  (+
                                   (f2cl-lib:fref wi-%data%
                                                  (iw jw)
                                                  ((1 kd) (1 1))
                                                  wi-%offset%)
                                   (* ajl
                                      (f2cl-lib:fref vi-%data%
                                                     (iw jv)
                                                     ((1 kd) (1 1))
                                                     vi-%offset%))))
                         label80))
                     label90
                      (setf jw (f2cl-lib:int-add jw ncomp))))
                  (setf lp1 (f2cl-lib:int-add l 1))
                  (if (= l mj) (go label130))
                  (f2cl-lib:fdo (ll lp1 (f2cl-lib:int-add ll 1))
                                ((> ll mj) nil)
                    (tagbody
                      (setf jdf (f2cl-lib:int-sub jn ll))
                      (setf bl
                              (f2cl-lib:fref basm
                                             ((f2cl-lib:int-sub ll l))
                                             ((1 5))))
                      (f2cl-lib:fdo (iw i1 (f2cl-lib:int-add iw 1))
                                    ((> iw i2) nil)
                        (tagbody
                          (setf (f2cl-lib:fref vi-%data%
                                               (iw jv)
                                               ((1 kd) (1 1))
                                               vi-%offset%)
                                  (+
                                   (f2cl-lib:fref vi-%data%
                                                  (iw jv)
                                                  ((1 kd) (1 1))
                                                  vi-%offset%)
                                   (* bl
                                      (f2cl-lib:fref vi-%data%
                                                     (iw jdf)
                                                     ((1 kd) (1 1))
                                                     vi-%offset%))))
                         label100))
                     label110))
                 label130))
             label140))
          (if (< jj k) (go end_label))
          (setf msing 0)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dgefa wi kd kd ipvtw msing)
            (declare (ignore var-0 var-1 var-2 var-3))
            (setf msing var-4))
          (if (/= msing 0) (go end_label))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j mstar) nil)
            (tagbody
              (dgesl wi kd kd ipvtw
               (f2cl-lib:array-slice vi double-float (1 j) ((1 kd) (1 1))) 0)
             label250))
          (go end_label)
         end_label
          (return
           (values xcol
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
                   msing)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::vwblok
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(double-float double-float (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (1))
                        (fortran-to-lisp::integer4) (array double-float (1))
                        (array double-float (*)) (array double-float (28))
                        (array double-float (1)) (fortran-to-lisp::integer4) t
                        (fortran-to-lisp::integer4))
           :return-values '(fortran-to-lisp::xcol nil nil nil nil nil nil nil
                            nil nil nil nil nil fortran-to-lisp::msing)
           :calls '(fortran-to-lisp::dgesl fortran-to-lisp::dgefa))))

