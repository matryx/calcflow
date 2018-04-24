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


(defun lsyslv
       (msing xi xiold z dmz delz deldmz g w v rhs dmzo integs ipvtg ipvtw
        rnorm mode fsub dfsub gsub dgsub guess)
  (declare (type double-float rnorm)
           (type (array f2cl-lib:integer4 (*)) ipvtw ipvtg)
           (type (array f2cl-lib:integer4 (*)) integs)
           (type (array double-float (*)) dmzo rhs v w g deldmz delz dmz z
                                          xiold xi)
           (type (f2cl-lib:integer4) mode msing))
  (let ((colloc-rho
         (make-array 7
                     :element-type 'double-float
                     :displaced-to (colloc-part-0 *colloc-common-block*)
                     :displaced-index-offset 0))
        (colloc-coef
         (make-array 49
                     :element-type 'double-float
                     :displaced-to (colloc-part-0 *colloc-common-block*)
                     :displaced-index-offset 7))
        (colord-m
         (make-array 20
                     :element-type 'f2cl-lib:integer4
                     :displaced-to (colord-part-0 *colord-common-block*)
                     :displaced-index-offset 5))
        (colsid-zeta
         (make-array 40
                     :element-type 'double-float
                     :displaced-to (colsid-part-0 *colsid-common-block*)
                     :displaced-index-offset 0))
        (colbas-acol
         (make-array 196
                     :element-type 'double-float
                     :displaced-to (colbas-part-0 *colbas-common-block*)
                     :displaced-index-offset 28)))
    (symbol-macrolet ((precis (aref (colout-part-0 *colout-common-block*) 0))
                      (rho colloc-rho)
                      (coef colloc-coef)
                      (k (aref (colord-part-0 *colord-common-block*) 0))
                      (ncomp (aref (colord-part-0 *colord-common-block*) 1))
                      (mstar (aref (colord-part-0 *colord-common-block*) 2))
                      (kd (aref (colord-part-0 *colord-common-block*) 3))
                      (mmax (aref (colord-part-0 *colord-common-block*) 4))
                      (m colord-m)
                      (zeta colsid-zeta)
                      (aright (aref (colsid-part-0 *colsid-common-block*) 41))
                      (izeta (aref (colsid-part-1 *colsid-common-block*) 0))
                      (izsave (aref (colsid-part-1 *colsid-common-block*) 1))
                      (n (aref (colapr-part-0 *colapr-common-block*) 0))
                      (nold (aref (colapr-part-0 *colapr-common-block*) 1))
                      (nz (aref (colapr-part-0 *colapr-common-block*) 3))
                      (ndmz (aref (colapr-part-0 *colapr-common-block*) 4))
                      (iguess (aref (colnln-part-0 *colnln-common-block*) 4))
                      (acol colbas-acol))
      (f2cl-lib:with-multi-array-data
          ((xi double-float xi-%data% xi-%offset%)
           (xiold double-float xiold-%data% xiold-%offset%)
           (z double-float z-%data% z-%offset%)
           (dmz double-float dmz-%data% dmz-%offset%)
           (delz double-float delz-%data% delz-%offset%)
           (deldmz double-float deldmz-%data% deldmz-%offset%)
           (g double-float g-%data% g-%offset%)
           (w double-float w-%data% w-%offset%)
           (v double-float v-%data% v-%offset%)
           (rhs double-float rhs-%data% rhs-%offset%)
           (dmzo double-float dmzo-%data% dmzo-%offset%)
           (integs f2cl-lib:integer4 integs-%data% integs-%offset%)
           (ipvtg f2cl-lib:integer4 ipvtg-%data% ipvtg-%offset%)
           (ipvtw f2cl-lib:integer4 ipvtw-%data% ipvtw-%offset%))
        (prog ((izet 0) (iz 0) (value 0.0) (jj 0) (xcol 0.0) (hrho 0.0) (j 0)
               (gval 0.0) (h 0.0) (xii 0.0) (l 0) (lw 0) (nrow 0) (ncol 0)
               (iold 0) (lside 0) (iv 0) (iw 0) (ig 0) (irhs 0) (idmzo 0)
               (idmz 0) (i 0) (m1 0)
               (dummy (make-array 1 :element-type 'double-float))
               (at (make-array 28 :element-type 'double-float))
               (df (make-array 800 :element-type 'double-float))
               (dmval (make-array 20 :element-type 'double-float))
               (dgz (make-array 40 :element-type 'double-float))
               (f (make-array 40 :element-type 'double-float))
               (zval (make-array 40 :element-type 'double-float)))
          (declare (type (array double-float (40)) zval f dgz)
                   (type (array double-float (20)) dmval)
                   (type (array double-float (800)) df)
                   (type (array double-float (28)) at)
                   (type (array double-float (1)) dummy)
                   (type double-float xii h gval hrho xcol value)
                   (type (f2cl-lib:integer4) m1 i idmz idmzo irhs ig iw iv
                                             lside iold ncol nrow lw l j jj iz
                                             izet))
          (setf m1 (f2cl-lib:int-add mode 1))
          (f2cl-lib:computed-goto (label10 label30 label30 label30 label310)
                                  m1)
         label10
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i mstar) nil)
            (tagbody label20 (setf (f2cl-lib:fref zval (i) ((1 40))) 0.0)))
         label30
          (setf idmz 1)
          (setf idmzo 1)
          (setf irhs 1)
          (setf ig 1)
          (setf iw 1)
          (setf iv 1)
          (setf izeta 1)
          (setf lside 0)
          (setf iold 1)
          (setf ncol (f2cl-lib:int-mul 2 mstar))
          (setf rnorm 0.0)
          (if (> mode 1) (go label80))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf (f2cl-lib:fref integs-%data%
                                   (2 i)
                                   ((1 3) (1 1))
                                   integs-%offset%)
                      ncol)
              (if (< i n) (go label40))
              (setf (f2cl-lib:fref integs-%data%
                                   (3 n)
                                   ((1 3) (1 1))
                                   integs-%offset%)
                      ncol)
              (setf lside mstar)
              (go label60)
             label40
              (setf (f2cl-lib:fref integs-%data%
                                   (3 i)
                                   ((1 3) (1 1))
                                   integs-%offset%)
                      mstar)
             label50
              (if (= lside mstar) (go label60))
              (if
               (>= (f2cl-lib:fref zeta ((f2cl-lib:int-add lside 1)) ((1 40)))
                   (+ (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%)
                      precis))
               (go label60))
              (setf lside (f2cl-lib:int-add lside 1))
              (go label50)
             label60
              (setf nrow (f2cl-lib:int-add mstar lside))
             label70
              (setf (f2cl-lib:fref integs-%data%
                                   (1 i)
                                   ((1 3) (1 1))
                                   integs-%offset%)
                      nrow)))
         label80
          (if (= mode 2) (go label90))
          (setf lw (f2cl-lib:int-mul kd kd n))
          (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                        ((> l lw) nil)
            (tagbody
             label84
              (setf (f2cl-lib:fref w-%data% (l) ((1 1)) w-%offset%) 0.0)))
         label90
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf xii (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%))
              (setf h
                      (-
                       (f2cl-lib:fref xi-%data%
                                      ((f2cl-lib:int-add i 1))
                                      ((1 1))
                                      xi-%offset%)
                       (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%)))
              (setf nrow
                      (f2cl-lib:fref integs-%data%
                                     (1 i)
                                     ((1 3) (1 1))
                                     integs-%offset%))
             label100
              (if (> izeta mstar) (go label140))
              (if (> (f2cl-lib:fref zeta (izeta) ((1 40))) (+ xii precis))
                  (go label140))
              (if (= mode 0) (go label110))
              (if (/= iguess 1) (go label102))
              (multiple-value-bind (var-0 var-1 var-2)
                  (funcall guess xii zval dmval)
                (declare (ignore var-1 var-2))
                (when var-0
                  (setf xii var-0)))
              (go label110)
             label102
              (if (/= mode 1) (go label106))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16)
                  (approx iold xii zval at coef xiold nold z dmz k ncomp mmax m
                   mstar 2 dummy 0)
                (declare (ignore var-2 var-3 var-4 var-5 var-6 var-7 var-8
                                 var-9 var-10 var-11 var-12 var-13 var-14
                                 var-15 var-16))
                (setf iold var-0)
                (setf xii var-1))
              (go label110)
             label106
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16)
                  (approx i xii zval at dummy xi n z dmz k ncomp mmax m mstar 1
                   dummy 0)
                (declare (ignore var-2 var-3 var-4 var-5 var-6 var-7 var-8
                                 var-9 var-10 var-11 var-12 var-13 var-14
                                 var-15 var-16))
                (setf i var-0)
                (setf xii var-1))
             label108
              (if (= mode 3) (go label120))
             label110
              (multiple-value-bind (var-0 var-1 var-2)
                  (funcall gsub izeta zval gval)
                (declare (ignore var-1))
                (when var-0
                  (setf izeta var-0))
                (when var-2
                  (setf gval var-2)))
              (setf (f2cl-lib:fref rhs-%data%
                                   ((f2cl-lib:int-add ndmz izeta))
                                   ((1 1))
                                   rhs-%offset%)
                      (- gval))
              (setf rnorm (+ rnorm (expt gval 2)))
              (if (= mode 2) (go label130))
             label120
              (gderiv (f2cl-lib:array-slice g double-float (ig) ((1 1))) nrow
               izeta zval dgz 1 dgsub)
             label130
              (setf izeta (f2cl-lib:int-add izeta 1))
              (go label100)
             label140
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j k) nil)
                (tagbody
                  (setf hrho (* h (f2cl-lib:fref rho (j) ((1 7)))))
                  (setf xcol (+ xii hrho))
                  (if (= mode 0) (go label200))
                  (if (/= iguess 1) (go label160))
                  (multiple-value-bind (var-0 var-1 var-2)
                      (funcall guess
                               xcol
                               zval
                               (f2cl-lib:array-slice dmzo
                                                     double-float
                                                     (irhs)
                                                     ((1 1))))
                    (declare (ignore var-1 var-2))
                    (when var-0
                      (setf xcol var-0)))
                  (go label170)
                 label160
                  (if (/= mode 1) (go label190))
                  (multiple-value-bind
                        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                         var-9 var-10 var-11 var-12 var-13 var-14 var-15
                         var-16)
                      (approx iold xcol zval at coef xiold nold z dmz k ncomp
                       mmax m mstar 2
                       (f2cl-lib:array-slice dmzo double-float (irhs) ((1 1)))
                       1)
                    (declare (ignore var-2 var-3 var-4 var-5 var-6 var-7 var-8
                                     var-9 var-10 var-11 var-12 var-13 var-14
                                     var-15 var-16))
                    (setf iold var-0)
                    (setf xcol var-1))
                 label170
                  (multiple-value-bind (var-0 var-1 var-2)
                      (funcall fsub xcol zval f)
                    (declare (ignore var-1 var-2))
                    (when var-0
                      (setf xcol var-0)))
                  (f2cl-lib:fdo (jj 1 (f2cl-lib:int-add jj 1))
                                ((> jj ncomp) nil)
                    (tagbody
                      (setf value
                              (-
                               (f2cl-lib:fref dmzo-%data%
                                              (irhs)
                                              ((1 1))
                                              dmzo-%offset%)
                               (f2cl-lib:fref f (jj) ((1 40)))))
                      (setf (f2cl-lib:fref rhs-%data%
                                           (irhs)
                                           ((1 1))
                                           rhs-%offset%)
                              (- value))
                      (setf rnorm (+ rnorm (expt value 2)))
                      (setf irhs (f2cl-lib:int-add irhs 1))
                     label180))
                  (go label210)
                 label190
                  (multiple-value-bind
                        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                         var-9 var-10 var-11 var-12 var-13 var-14 var-15
                         var-16)
                      (approx i xcol zval
                       (f2cl-lib:array-slice acol
                                             double-float
                                             (1 j)
                                             ((1 28) (1 7)))
                       coef xi n z dmz k ncomp mmax m mstar 4 dummy 0)
                    (declare (ignore var-2 var-3 var-4 var-5 var-6 var-7 var-8
                                     var-9 var-10 var-11 var-12 var-13 var-14
                                     var-15 var-16))
                    (setf i var-0)
                    (setf xcol var-1))
                  (if (= mode 3) (go label210))
                  (multiple-value-bind (var-0 var-1 var-2)
                      (funcall fsub xcol zval f)
                    (declare (ignore var-1 var-2))
                    (when var-0
                      (setf xcol var-0)))
                  (f2cl-lib:fdo (jj 1 (f2cl-lib:int-add jj 1))
                                ((> jj ncomp) nil)
                    (tagbody
                      (setf value
                              (-
                               (f2cl-lib:fref dmz-%data%
                                              (irhs)
                                              ((1 1))
                                              dmz-%offset%)
                               (f2cl-lib:fref f (jj) ((1 40)))))
                      (setf (f2cl-lib:fref rhs-%data%
                                           (irhs)
                                           ((1 1))
                                           rhs-%offset%)
                              (- value))
                      (setf rnorm (+ rnorm (expt value 2)))
                      (setf irhs (f2cl-lib:int-add irhs 1))
                     label195))
                  (go label220)
                 label200
                  (multiple-value-bind (var-0 var-1 var-2)
                      (funcall fsub
                               xcol
                               zval
                               (f2cl-lib:array-slice rhs
                                                     double-float
                                                     (irhs)
                                                     ((1 1))))
                    (declare (ignore var-1 var-2))
                    (when var-0
                      (setf xcol var-0)))
                  (setf irhs (f2cl-lib:int-add irhs ncomp))
                 label210
                  (multiple-value-bind
                        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                         var-9 var-10 var-11 var-12 var-13)
                      (vwblok xcol hrho j
                       (f2cl-lib:array-slice w double-float (iw) ((1 1)))
                       (f2cl-lib:array-slice v double-float (iv) ((1 1)))
                       (f2cl-lib:array-slice ipvtw
                                             f2cl-lib:integer4
                                             (idmz)
                                             ((1 1)))
                       kd zval df
                       (f2cl-lib:array-slice acol
                                             double-float
                                             (1 j)
                                             ((1 28) (1 7)))
                       (f2cl-lib:array-slice dmzo double-float (idmzo) ((1 1)))
                       ncomp dfsub msing)
                    (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                     var-8 var-9 var-10 var-11 var-12))
                    (setf xcol var-0)
                    (setf msing var-13))
                  (if (/= msing 0) (go end_label))
                 label220))
              (if (/= mode 2)
                  (gblock h (f2cl-lib:array-slice g double-float (ig) ((1 1)))
                   nrow izeta
                   (f2cl-lib:array-slice w double-float (iw) ((1 1)))
                   (f2cl-lib:array-slice v double-float (iv) ((1 1))) kd dummy
                   (f2cl-lib:array-slice deldmz double-float (idmz) ((1 1)))
                   (f2cl-lib:array-slice ipvtw
                                         f2cl-lib:integer4
                                         (idmz)
                                         ((1 1)))
                   1))
              (if (< i n) (go label280))
              (setf izsave izeta)
             label240
              (if (> izeta mstar) (go label290))
              (if (= mode 0) (go label250))
              (if (/= iguess 1) (go label245))
              (multiple-value-bind (var-0 var-1 var-2)
                  (funcall guess aright zval dmval)
                (declare (ignore var-1 var-2))
                (when var-0
                  (setf aright var-0)))
              (go label250)
             label245
              (if (/= mode 1) (go label246))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16)
                  (approx (f2cl-lib:int-add nold 1) aright zval at coef xiold
                   nold z dmz k ncomp mmax m mstar 1 dummy 0)
                (declare (ignore var-0 var-2 var-3 var-4 var-5 var-6 var-7
                                 var-8 var-9 var-10 var-11 var-12 var-13 var-14
                                 var-15 var-16))
                (setf aright var-1))
              (go label250)
             label246
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16)
                  (approx (f2cl-lib:int-add n 1) aright zval at coef xi n z dmz
                   k ncomp mmax m mstar 1 dummy 0)
                (declare (ignore var-0 var-2 var-3 var-4 var-5 var-6 var-7
                                 var-8 var-9 var-10 var-11 var-12 var-13 var-14
                                 var-15 var-16))
                (setf aright var-1))
             label248
              (if (= mode 3) (go label260))
             label250
              (multiple-value-bind (var-0 var-1 var-2)
                  (funcall gsub izeta zval gval)
                (declare (ignore var-1))
                (when var-0
                  (setf izeta var-0))
                (when var-2
                  (setf gval var-2)))
              (setf (f2cl-lib:fref rhs-%data%
                                   ((f2cl-lib:int-add ndmz izeta))
                                   ((1 1))
                                   rhs-%offset%)
                      (- gval))
              (setf rnorm (+ rnorm (expt gval 2)))
              (if (= mode 2) (go label270))
             label260
              (gderiv (f2cl-lib:array-slice g double-float (ig) ((1 1))) nrow
               (f2cl-lib:int-add izeta mstar) zval dgz 2 dgsub)
             label270
              (setf izeta (f2cl-lib:int-add izeta 1))
              (go label240)
             label280
              (setf ig (f2cl-lib:int-add ig (f2cl-lib:int-mul nrow ncol)))
              (setf iv (f2cl-lib:int-add iv (f2cl-lib:int-mul kd mstar)))
              (setf iw (f2cl-lib:int-add iw (f2cl-lib:int-mul kd kd)))
              (setf idmz (f2cl-lib:int-add idmz kd))
              (if (= mode 1) (setf idmzo (f2cl-lib:int-add idmzo kd)))
             label290))
          (if (or (= mode 0) (= mode 3)) (go label300))
          (setf rnorm
                  (f2cl-lib:dsqrt
                   (/ rnorm (f2cl-lib:dfloat (f2cl-lib:int-add nz ndmz)))))
          (if (/= mode 2) (go label300))
          (go end_label)
         label300
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (fcblok g integs n ipvtg df msing)
            (declare (ignore var-0 var-1 var-2 var-3 var-4))
            (setf msing var-5))
          (setf msing (f2cl-lib:int-sub msing))
          (if (/= msing 0) (go end_label))
         label310
          (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                        ((> l ndmz) nil)
            (tagbody
              (setf (f2cl-lib:fref deldmz-%data% (l) ((1 1)) deldmz-%offset%)
                      (f2cl-lib:fref rhs-%data% (l) ((1 1)) rhs-%offset%))
             label311))
          (setf iz 1)
          (setf idmz 1)
          (setf iw 1)
          (setf izet 1)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf nrow
                      (f2cl-lib:fref integs-%data%
                                     (1 i)
                                     ((1 3) (1 1))
                                     integs-%offset%))
              (setf izeta (f2cl-lib:int-sub (f2cl-lib:int-add nrow 1) mstar))
              (if (= i n) (setf izeta izsave))
             label322
              (if (= izet izeta) (go label324))
              (setf (f2cl-lib:fref delz-%data%
                                   ((f2cl-lib:int-add (f2cl-lib:int-sub iz 1)
                                                      izet))
                                   ((1 1))
                                   delz-%offset%)
                      (f2cl-lib:fref rhs-%data%
                                     ((f2cl-lib:int-add ndmz izet))
                                     ((1 1))
                                     rhs-%offset%))
              (setf izet (f2cl-lib:int-add izet 1))
              (go label322)
             label324
              (setf h
                      (-
                       (f2cl-lib:fref xi-%data%
                                      ((f2cl-lib:int-add i 1))
                                      ((1 1))
                                      xi-%offset%)
                       (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%)))
              (gblock h (f2cl-lib:array-slice g double-float (1) ((1 1))) nrow
               izeta (f2cl-lib:array-slice w double-float (iw) ((1 1)))
               (f2cl-lib:array-slice v double-float (1) ((1 1))) kd
               (f2cl-lib:array-slice delz double-float (iz) ((1 1)))
               (f2cl-lib:array-slice deldmz double-float (idmz) ((1 1)))
               (f2cl-lib:array-slice ipvtw f2cl-lib:integer4 (idmz) ((1 1))) 2)
              (setf iz (f2cl-lib:int-add iz mstar))
              (setf idmz (f2cl-lib:int-add idmz kd))
              (setf iw (f2cl-lib:int-add iw (f2cl-lib:int-mul kd kd)))
              (if (< i n) (go label320))
             label326
              (if (> izet mstar) (go label320))
              (setf (f2cl-lib:fref delz-%data%
                                   ((f2cl-lib:int-add (f2cl-lib:int-sub iz 1)
                                                      izet))
                                   ((1 1))
                                   delz-%offset%)
                      (f2cl-lib:fref rhs-%data%
                                     ((f2cl-lib:int-add ndmz izet))
                                     ((1 1))
                                     rhs-%offset%))
              (setf izet (f2cl-lib:int-add izet 1))
              (go label326)
             label320))
          (sbblok g integs n ipvtg delz)
          (dmzsol kd mstar n v delz deldmz)
          (if (/= mode 1) (go end_label))
          (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                        ((> l ndmz) nil)
            (tagbody
              (setf (f2cl-lib:fref dmz-%data% (l) ((1 1)) dmz-%offset%)
                      (f2cl-lib:fref dmzo-%data% (l) ((1 1)) dmzo-%offset%))
             label321))
          (setf iz 1)
          (setf idmz 1)
          (setf iw 1)
          (setf izet 1)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf nrow
                      (f2cl-lib:fref integs-%data%
                                     (1 i)
                                     ((1 3) (1 1))
                                     integs-%offset%))
              (setf izeta (f2cl-lib:int-sub (f2cl-lib:int-add nrow 1) mstar))
              (if (= i n) (setf izeta izsave))
             label330
              (if (= izet izeta) (go label340))
              (setf (f2cl-lib:fref z-%data%
                                   ((f2cl-lib:int-add (f2cl-lib:int-sub iz 1)
                                                      izet))
                                   ((1 1))
                                   z-%offset%)
                      (f2cl-lib:fref dgz (izet) ((1 40))))
              (setf izet (f2cl-lib:int-add izet 1))
              (go label330)
             label340
              (setf h
                      (-
                       (f2cl-lib:fref xi-%data%
                                      ((f2cl-lib:int-add i 1))
                                      ((1 1))
                                      xi-%offset%)
                       (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%)))
              (gblock h (f2cl-lib:array-slice g double-float (1) ((1 1))) nrow
               izeta (f2cl-lib:array-slice w double-float (iw) ((1 1))) df kd
               (f2cl-lib:array-slice z double-float (iz) ((1 1)))
               (f2cl-lib:array-slice dmz double-float (idmz) ((1 1)))
               (f2cl-lib:array-slice ipvtw f2cl-lib:integer4 (idmz) ((1 1))) 2)
              (setf iz (f2cl-lib:int-add iz mstar))
              (setf idmz (f2cl-lib:int-add idmz kd))
              (setf iw (f2cl-lib:int-add iw (f2cl-lib:int-mul kd kd)))
              (if (< i n) (go label350))
             label342
              (if (> izet mstar) (go label350))
              (setf (f2cl-lib:fref z-%data%
                                   ((f2cl-lib:int-add (f2cl-lib:int-sub iz 1)
                                                      izet))
                                   ((1 1))
                                   z-%offset%)
                      (f2cl-lib:fref dgz (izet) ((1 40))))
              (setf izet (f2cl-lib:int-add izet 1))
              (go label342)
             label350))
          (sbblok g integs n ipvtg z)
          (dmzsol kd mstar n v z dmz)
          (go end_label)
         end_label
          (return
           (values msing
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
                   rnorm
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::lsyslv
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (1))
                        (array double-float (1)) (array double-float (1))
                        (array double-float (1)) (array double-float (1))
                        (array double-float (1)) (array double-float (1))
                        (array double-float (1)) (array double-float (1))
                        (array double-float (1)) (array double-float (1))
                        (array fortran-to-lisp::integer4 (3))
                        (array fortran-to-lisp::integer4 (1))
                        (array fortran-to-lisp::integer4 (1)) double-float
                        (fortran-to-lisp::integer4) t t t t t)
           :return-values '(fortran-to-lisp::msing nil nil nil nil nil nil nil
                            nil nil nil nil nil nil nil fortran-to-lisp::rnorm
                            nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::dmzsol fortran-to-lisp::sbblok
                    fortran-to-lisp::fcblok fortran-to-lisp::gblock
                    fortran-to-lisp::vwblok fortran-to-lisp::gderiv
                    fortran-to-lisp::approx))))

