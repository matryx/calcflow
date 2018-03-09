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


(defun newmsh (mode xi xiold z dmz valstr slope accum nfxpnt fixpnt)
  (declare (type (array double-float (*)) fixpnt accum slope valstr dmz z xiold
                                          xi)
           (type (f2cl-lib:integer4) nfxpnt mode))
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
        (colest-wgtmsh
         (make-array 40
                     :element-type 'double-float
                     :displaced-to (colest-part-0 *colest-common-block*)
                     :displaced-index-offset 40))
        (colest-root
         (make-array 40
                     :element-type 'double-float
                     :displaced-to (colest-part-0 *colest-common-block*)
                     :displaced-index-offset 160))
        (colest-jtol
         (make-array 40
                     :element-type 'f2cl-lib:integer4
                     :displaced-to (colest-part-1 *colest-common-block*)
                     :displaced-index-offset 0))
        (colest-ltol
         (make-array 40
                     :element-type 'f2cl-lib:integer4
                     :displaced-to (colest-part-1 *colest-common-block*)
                     :displaced-index-offset 40)))
    (symbol-macrolet ((precis (aref (colout-part-0 *colout-common-block*) 0))
                      (iout (aref (colout-part-1 *colout-common-block*) 0))
                      (iprint (aref (colout-part-1 *colout-common-block*) 1))
                      (k (aref (colord-part-0 *colord-common-block*) 0))
                      (ncomp (aref (colord-part-0 *colord-common-block*) 1))
                      (mstar (aref (colord-part-0 *colord-common-block*) 2))
                      (kd (aref (colord-part-0 *colord-common-block*) 3))
                      (mmax (aref (colord-part-0 *colord-common-block*) 4))
                      (m colord-m)
                      (n (aref (colapr-part-0 *colapr-common-block*) 0))
                      (nold (aref (colapr-part-0 *colapr-common-block*) 1))
                      (nmax (aref (colapr-part-0 *colapr-common-block*) 2))
                      (nz (aref (colapr-part-0 *colapr-common-block*) 3))
                      (ndmz (aref (colapr-part-0 *colapr-common-block*) 4))
                      (mshflg (aref (colmsh-part-0 *colmsh-common-block*) 0))
                      (mshnum (aref (colmsh-part-0 *colmsh-common-block*) 1))
                      (mshlmt (aref (colmsh-part-0 *colmsh-common-block*) 2))
                      (mshalt (aref (colmsh-part-0 *colmsh-common-block*) 3))
                      (iguess (aref (colnln-part-0 *colnln-common-block*) 4))
                      (aleft (aref (colsid-part-0 *colsid-common-block*) 40))
                      (aright (aref (colsid-part-0 *colsid-common-block*) 41))
                      (asave colbas-asave)
                      (wgtmsh colest-wgtmsh)
                      (root colest-root)
                      (jtol colest-jtol)
                      (ltol colest-ltol)
                      (ntol (aref (colest-part-1 *colest-common-block*) 80)))
      (f2cl-lib:with-multi-array-data
          ((xi double-float xi-%data% xi-%offset%)
           (xiold double-float xiold-%data% xiold-%offset%)
           (z double-float z-%data% z-%offset%)
           (dmz double-float dmz-%data% dmz-%offset%)
           (valstr double-float valstr-%data% valstr-%offset%)
           (slope double-float slope-%data% slope-%offset%)
           (accum double-float accum-%data% accum-%offset%)
           (fixpnt double-float fixpnt-%data% fixpnt-%offset%))
        (prog ((lcarry 0) (l 0) (tsum 0.0) (accr 0.0) (lnew 0) (lold 0)
               (accl 0.0) (in 0) (nmax2 0) (nmx 0) (naccum 0) (degequ 0.0)
               (avrg 0.0) (temp 0.0) (iflip 0) (slphmx 0.0) (jz 0) (jj 0)
               (oneovh 0.0) (hiold 0.0) (x 0.0) (hd6 0.0) (kstore 0) (n2 0)
               (dx 0.0) (nregn 0) (nmin 0) (iright 0) (xright 0.0) (xleft 0.0)
               (ileft 0) (np1 0) (j 0) (i 0) (noldp1 0) (nfxp1 0)
               (d2 (make-array 40 :element-type 'double-float))
               (d1 (make-array 40 :element-type 'double-float))
               (dummy (make-array 1 :element-type 'double-float)))
          (declare (type (array double-float (1)) dummy)
                   (type (array double-float (40)) d1 d2)
                   (type double-float xleft xright dx hd6 x hiold oneovh slphmx
                                      temp avrg degequ accl accr tsum)
                   (type (f2cl-lib:integer4) nfxp1 noldp1 i j np1 ileft iright
                                             nmin nregn n2 kstore jj jz iflip
                                             naccum nmx nmax2 in lold lnew l
                                             lcarry))
          (setf nfxp1 (f2cl-lib:int-add nfxpnt 1))
          (f2cl-lib:computed-goto (label180 label100 label50 label20 label10)
                                  mode)
         label10
          (setf mshlmt 1)
         label20
          (if (< iguess 2) (go label40))
          (setf noldp1 (f2cl-lib:int-add nold 1))
          (if (< iprint 1)
              (f2cl-lib:fformat iout
                                ("~%" " THE FORMER MESH (OF" 1 (("~5D"))
                                 " SUBINTERVALS)," 100
                                 ("~%" 8 (("~12,6,0,'*,F"))) "~%")
                                nold
                                (do ((i 1 (f2cl-lib:int-add i 1))
                                     (%ret nil))
                                    ((> i noldp1) (nreverse %ret))
                                  (declare (type f2cl-lib:integer4 i))
                                  (push
                                   (f2cl-lib:fref xiold-%data%
                                                  (i)
                                                  ((1 1))
                                                  xiold-%offset%)
                                   %ret))))
          (if (/= iguess 3) (go label40))
          (setf n (the f2cl-lib:integer4 (truncate nold 2)))
          (setf i 0)
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 2))
                        ((> j nold) nil)
            (tagbody
              (setf i (f2cl-lib:int-add i 1))
             label30
              (setf (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%)
                      (f2cl-lib:fref xiold-%data%
                                     (j)
                                     ((1 1))
                                     xiold-%offset%))))
         label40
          (setf np1 (f2cl-lib:int-add n 1))
          (setf (f2cl-lib:fref xi-%data% (1) ((1 1)) xi-%offset%) aleft)
          (setf (f2cl-lib:fref xi-%data% (np1) ((1 1)) xi-%offset%) aright)
          (go label320)
         label50
          (if (< n nfxp1) (setf n nfxp1))
          (setf np1 (f2cl-lib:int-add n 1))
          (setf (f2cl-lib:fref xi-%data% (1) ((1 1)) xi-%offset%) aleft)
          (setf ileft 1)
          (setf xleft aleft)
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j nfxp1) nil)
            (tagbody
              (setf xright aright)
              (setf iright np1)
              (if (= j nfxp1) (go label60))
              (setf xright
                      (f2cl-lib:fref fixpnt-%data%
                                     (j)
                                     ((1 1))
                                     fixpnt-%offset%))
              (setf nmin
                      (f2cl-lib:int
                       (+
                        (* (/ (- xright aleft) (- aright aleft))
                           (f2cl-lib:dfloat n))
                        1.5)))
              (if (> nmin (f2cl-lib:int-add (f2cl-lib:int-sub n nfxpnt) j))
                  (setf nmin (f2cl-lib:int-add (f2cl-lib:int-sub n nfxpnt) j)))
              (setf iright (f2cl-lib:max0 (f2cl-lib:int-add ileft 1) nmin))
             label60
              (setf (f2cl-lib:fref xi-%data% (iright) ((1 1)) xi-%offset%)
                      xright)
              (setf nregn (f2cl-lib:int-sub iright ileft 1))
              (if (= nregn 0) (go label80))
              (setf dx
                      (/ (- xright xleft)
                         (f2cl-lib:dfloat (f2cl-lib:int-add nregn 1))))
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i nregn) nil)
                (tagbody
                 label70
                  (setf (f2cl-lib:fref xi-%data%
                                       ((f2cl-lib:int-add ileft i))
                                       ((1 1))
                                       xi-%offset%)
                          (+ xleft (* (f2cl-lib:dfloat i) dx)))))
             label80
              (setf ileft iright)
              (setf xleft xright)
             label90))
          (go label320)
         label100
          (setf n2 (f2cl-lib:int-mul 2 n))
          (if (<= n2 nmax) (go label120))
          (if (= mode 2) (go label110))
          (setf n (the f2cl-lib:integer4 (truncate nmax 2)))
          (go label220)
         label110
          (if (< iprint 1)
              (f2cl-lib:fformat iout ("~%" "  EXPECTED N TOO LARGE " "~%")))
          (setf n n2)
          (go end_label)
         label120
          (if (= mshflg 0) (go label140))
          (setf kstore 1)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i nold) nil)
            (tagbody
              (setf hd6
                      (/
                       (-
                        (f2cl-lib:fref xiold-%data%
                                       ((f2cl-lib:int-add i 1))
                                       ((1 1))
                                       xiold-%offset%)
                        (f2cl-lib:fref xiold-%data%
                                       (i)
                                       ((1 1))
                                       xiold-%offset%))
                       6.0))
              (setf x
                      (+
                       (f2cl-lib:fref xiold-%data% (i) ((1 1)) xiold-%offset%)
                       hd6))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16)
                  (approx i x
                   (f2cl-lib:array-slice valstr double-float (kstore) ((1 1)))
                   (f2cl-lib:array-slice asave
                                         double-float
                                         (1 1)
                                         ((1 28) (1 4)))
                   dummy xiold nold z dmz k ncomp mmax m mstar 4 dummy 0)
                (declare (ignore var-2 var-3 var-4 var-5 var-6 var-7 var-8
                                 var-9 var-10 var-11 var-12 var-13 var-14
                                 var-15 var-16))
                (setf i var-0)
                (setf x var-1))
              (setf x (+ x (* 4.0 hd6)))
              (setf kstore
                      (f2cl-lib:int-add kstore (f2cl-lib:int-mul 3 mstar)))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16)
                  (approx i x
                   (f2cl-lib:array-slice valstr double-float (kstore) ((1 1)))
                   (f2cl-lib:array-slice asave
                                         double-float
                                         (1 4)
                                         ((1 28) (1 4)))
                   dummy xiold nold z dmz k ncomp mmax m mstar 4 dummy 0)
                (declare (ignore var-2 var-3 var-4 var-5 var-6 var-7 var-8
                                 var-9 var-10 var-11 var-12 var-13 var-14
                                 var-15 var-16))
                (setf i var-0)
                (setf x var-1))
              (setf kstore (f2cl-lib:int-add kstore mstar))
             label130))
          (go label160)
         label140
          (setf kstore 1)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf x (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%))
              (setf hd6
                      (/
                       (-
                        (f2cl-lib:fref xi-%data%
                                       ((f2cl-lib:int-add i 1))
                                       ((1 1))
                                       xi-%offset%)
                        (f2cl-lib:fref xi-%data% (i) ((1 1)) xi-%offset%))
                       6.0))
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j 4) nil)
                (tagbody
                  (setf x (+ x hd6))
                  (if (= j 3) (setf x (+ x hd6)))
                  (multiple-value-bind
                        (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                         var-9 var-10 var-11 var-12 var-13 var-14 var-15
                         var-16)
                      (approx i x
                       (f2cl-lib:array-slice valstr
                                             double-float
                                             (kstore)
                                             ((1 1)))
                       (f2cl-lib:array-slice asave
                                             double-float
                                             (1 j)
                                             ((1 28) (1 4)))
                       dummy xiold nold z dmz k ncomp mmax m mstar 4 dummy 0)
                    (declare (ignore var-2 var-3 var-4 var-5 var-6 var-7 var-8
                                     var-9 var-10 var-11 var-12 var-13 var-14
                                     var-15 var-16))
                    (setf i var-0)
                    (setf x var-1))
                  (setf kstore (f2cl-lib:int-add kstore mstar))
                 label150))))
         label150
         label160
          (setf mshflg 0)
          (setf mshnum 1)
          (setf mode 2)
          (setf j 2)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf (f2cl-lib:fref xi-%data% (j) ((1 1)) xi-%offset%)
                      (/
                       (+
                        (f2cl-lib:fref xiold-%data% (i) ((1 1)) xiold-%offset%)
                        (f2cl-lib:fref xiold-%data%
                                       ((f2cl-lib:int-add i 1))
                                       ((1 1))
                                       xiold-%offset%))
                       2.0))
              (setf (f2cl-lib:fref xi-%data%
                                   ((f2cl-lib:int-add j 1))
                                   ((1 1))
                                   xi-%offset%)
                      (f2cl-lib:fref xiold-%data%
                                     ((f2cl-lib:int-add i 1))
                                     ((1 1))
                                     xiold-%offset%))
             label170
              (setf j (f2cl-lib:int-add j 2))))
          (setf n n2)
          (go label320)
         label180
          (if (= nold 1) (go label100))
          (if (<= nold (f2cl-lib:int-mul 2 nfxpnt)) (go label100))
          (setf i 1)
          (setf hiold
                  (- (f2cl-lib:fref xiold-%data% (2) ((1 1)) xiold-%offset%)
                     (f2cl-lib:fref xiold-%data% (1) ((1 1)) xiold-%offset%)))
          (horder 1 d1 hiold dmz ncomp k)
          (setf hiold
                  (- (f2cl-lib:fref xiold-%data% (3) ((1 1)) xiold-%offset%)
                     (f2cl-lib:fref xiold-%data% (2) ((1 1)) xiold-%offset%)))
          (horder 2 d2 hiold dmz ncomp k)
          (setf (f2cl-lib:fref accum-%data% (1) ((1 1)) accum-%offset%) 0.0)
          (setf (f2cl-lib:fref slope-%data% (1) ((1 1)) slope-%offset%) 0.0)
          (setf oneovh
                  (/ 2.0
                     (- (f2cl-lib:fref xiold-%data% (3) ((1 1)) xiold-%offset%)
                        (f2cl-lib:fref xiold-%data%
                                       (1)
                                       ((1 1))
                                       xiold-%offset%))))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j ntol) nil)
            (tagbody
              (setf jj (f2cl-lib:fref jtol (j) ((1 40))))
              (setf jz (f2cl-lib:fref ltol (j) ((1 40))))
             label190
              (setf (f2cl-lib:fref slope-%data% (1) ((1 1)) slope-%offset%)
                      (f2cl-lib:dmax1
                       (f2cl-lib:fref slope-%data% (1) ((1 1)) slope-%offset%)
                       (expt
                        (/
                         (*
                          (f2cl-lib:dabs
                           (- (f2cl-lib:fref d2 (jj) ((1 40)))
                              (f2cl-lib:fref d1 (jj) ((1 40)))))
                          (f2cl-lib:fref wgtmsh (j) ((1 40)))
                          oneovh)
                         (+ 1.0
                            (f2cl-lib:dabs
                             (f2cl-lib:fref z-%data%
                                            (jz)
                                            ((1 1))
                                            z-%offset%))))
                        (f2cl-lib:fref root (j) ((1 40))))))))
          (setf slphmx
                  (* (f2cl-lib:fref slope-%data% (1) ((1 1)) slope-%offset%)
                     (- (f2cl-lib:fref xiold-%data% (2) ((1 1)) xiold-%offset%)
                        (f2cl-lib:fref xiold-%data%
                                       (1)
                                       ((1 1))
                                       xiold-%offset%))))
          (setf (f2cl-lib:fref accum-%data% (2) ((1 1)) accum-%offset%) slphmx)
          (setf iflip 1)
          (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                        ((> i nold) nil)
            (tagbody
              (setf hiold
                      (-
                       (f2cl-lib:fref xiold-%data%
                                      ((f2cl-lib:int-add i 1))
                                      ((1 1))
                                      xiold-%offset%)
                       (f2cl-lib:fref xiold-%data%
                                      (i)
                                      ((1 1))
                                      xiold-%offset%)))
              (if (= iflip -1) (horder i d1 hiold dmz ncomp k))
              (if (= iflip 1) (horder i d2 hiold dmz ncomp k))
              (setf oneovh
                      (/ 2.0
                         (-
                          (f2cl-lib:fref xiold-%data%
                                         ((f2cl-lib:int-add i 1))
                                         ((1 1))
                                         xiold-%offset%)
                          (f2cl-lib:fref xiold-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 1))
                                         xiold-%offset%))))
              (setf (f2cl-lib:fref slope-%data% (i) ((1 1)) slope-%offset%)
                      0.0)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j ntol) nil)
                (tagbody
                  (setf jj (f2cl-lib:fref jtol (j) ((1 40))))
                  (setf jz
                          (f2cl-lib:int-add (f2cl-lib:fref ltol (j) ((1 40)))
                                            (f2cl-lib:int-mul
                                             (f2cl-lib:int-sub i 1)
                                             mstar)))
                 label200
                  (setf (f2cl-lib:fref slope-%data% (i) ((1 1)) slope-%offset%)
                          (f2cl-lib:dmax1
                           (f2cl-lib:fref slope-%data%
                                          (i)
                                          ((1 1))
                                          slope-%offset%)
                           (expt
                            (/
                             (*
                              (f2cl-lib:dabs
                               (- (f2cl-lib:fref d2 (jj) ((1 40)))
                                  (f2cl-lib:fref d1 (jj) ((1 40)))))
                              (f2cl-lib:fref wgtmsh (j) ((1 40)))
                              oneovh)
                             (+ 1.0
                                (f2cl-lib:dabs
                                 (f2cl-lib:fref z-%data%
                                                (jz)
                                                ((1 1))
                                                z-%offset%))))
                            (f2cl-lib:fref root (j) ((1 40))))))))
              (setf temp
                      (*
                       (f2cl-lib:fref slope-%data% (i) ((1 1)) slope-%offset%)
                       (-
                        (f2cl-lib:fref xiold-%data%
                                       ((f2cl-lib:int-add i 1))
                                       ((1 1))
                                       xiold-%offset%)
                        (f2cl-lib:fref xiold-%data%
                                       (i)
                                       ((1 1))
                                       xiold-%offset%))))
              (setf slphmx (f2cl-lib:dmax1 slphmx temp))
              (setf (f2cl-lib:fref accum-%data%
                                   ((f2cl-lib:int-add i 1))
                                   ((1 1))
                                   accum-%offset%)
                      (+
                       (f2cl-lib:fref accum-%data% (i) ((1 1)) accum-%offset%)
                       temp))
             label210
              (setf iflip (f2cl-lib:int-sub iflip))))
          (setf avrg
                  (/
                   (f2cl-lib:fref accum-%data%
                                  ((f2cl-lib:int-add nold 1))
                                  ((1 1))
                                  accum-%offset%)
                   (f2cl-lib:dfloat nold)))
          (setf degequ (/ avrg (f2cl-lib:dmax1 slphmx precis)))
          (setf naccum
                  (f2cl-lib:int
                   (+
                    (f2cl-lib:fref accum-%data%
                                   ((f2cl-lib:int-add nold 1))
                                   ((1 1))
                                   accum-%offset%)
                    1.0)))
          (if (< iprint 0)
              (f2cl-lib:fformat iout
                                ("~%" " MESH SELECTION INFO," "~%"
                                 " DEGREE OF EQUIDISTRIBUTION = " 1
                                 (("~8,5,0,'*,F"))
                                 " PREDICTION FOR REQUIRED N =" 1 (("~8D"))
                                 "~%")
                                degequ
                                naccum))
          (if (< avrg precis) (go label100))
          (if (>= degequ 0.5) (go label100))
          (setf nmx
                  (the f2cl-lib:integer4
                       (truncate (f2cl-lib:max0 (+ nold 1) naccum) 2)))
          (setf nmax2 (the f2cl-lib:integer4 (truncate nmax 2)))
          (setf n (f2cl-lib:min0 nmax2 nold nmx))
         label220
          (setf noldp1 (f2cl-lib:int-add nold 1))
          (if (< n nfxp1) (setf n nfxp1))
          (setf mshnum (f2cl-lib:int-add mshnum 1))
          (if (< n nold) (setf mshnum mshlmt))
          (if (> n (the f2cl-lib:integer4 (truncate nold 2))) (setf mshalt 1))
          (if (= n (the f2cl-lib:integer4 (truncate nold 2)))
              (setf mshalt (f2cl-lib:int-add mshalt 1)))
          (setf mshflg 0)
          (setf in 1)
          (setf accl 0.0)
          (setf lold 2)
          (setf (f2cl-lib:fref xi-%data% (1) ((1 1)) xi-%offset%) aleft)
          (setf (f2cl-lib:fref xi-%data%
                               ((f2cl-lib:int-add n 1))
                               ((1 1))
                               xi-%offset%)
                  aright)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i nfxp1) nil)
            (tagbody
              (if (= i nfxp1) (go label250))
              (f2cl-lib:fdo (j lold (f2cl-lib:int-add j 1))
                            ((> j noldp1) nil)
                (tagbody
                  (setf lnew j)
                  (if
                   (<=
                    (f2cl-lib:fref fixpnt-%data% (i) ((1 1)) fixpnt-%offset%)
                    (f2cl-lib:fref xiold-%data% (j) ((1 1)) xiold-%offset%))
                   (go label240))
                 label230))
             label240
              (setf accr
                      (+
                       (f2cl-lib:fref accum-%data%
                                      (lnew)
                                      ((1 1))
                                      accum-%offset%)
                       (*
                        (-
                         (f2cl-lib:fref fixpnt-%data%
                                        (i)
                                        ((1 1))
                                        fixpnt-%offset%)
                         (f2cl-lib:fref xiold-%data%
                                        (lnew)
                                        ((1 1))
                                        xiold-%offset%))
                        (f2cl-lib:fref slope-%data%
                                       ((f2cl-lib:int-sub lnew 1))
                                       ((1 1))
                                       slope-%offset%))))
              (setf nregn
                      (f2cl-lib:int
                       (-
                        (*
                         (/ (- accr accl)
                            (f2cl-lib:fref accum-%data%
                                           (noldp1)
                                           ((1 1))
                                           accum-%offset%))
                         (f2cl-lib:dfloat n))
                        0.5)))
              (setf nregn
                      (f2cl-lib:min0 nregn
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-sub n in nfxp1)
                                      i)))
              (setf (f2cl-lib:fref xi-%data%
                                   ((f2cl-lib:int-add in nregn 1))
                                   ((1 1))
                                   xi-%offset%)
                      (f2cl-lib:fref fixpnt-%data%
                                     (i)
                                     ((1 1))
                                     fixpnt-%offset%))
              (go label260)
             label250
              (setf accr
                      (f2cl-lib:fref accum-%data%
                                     (noldp1)
                                     ((1 1))
                                     accum-%offset%))
              (setf lnew noldp1)
              (setf nregn (f2cl-lib:int-sub n in))
             label260
              (if (= nregn 0) (go label300))
              (setf temp accl)
              (setf tsum
                      (/ (- accr accl)
                         (f2cl-lib:dfloat (f2cl-lib:int-add nregn 1))))
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j nregn) nil)
                (tagbody
                  (setf in (f2cl-lib:int-add in 1))
                  (setf temp (+ temp tsum))
                  (f2cl-lib:fdo (l lold (f2cl-lib:int-add l 1))
                                ((> l lnew) nil)
                    (tagbody
                      (setf lcarry l)
                      (if
                       (<= temp
                           (f2cl-lib:fref accum-%data%
                                          (l)
                                          ((1 1))
                                          accum-%offset%))
                       (go label280))
                     label270))
                 label280
                  (setf lold lcarry)
                 label290
                  (setf (f2cl-lib:fref xi-%data% (in) ((1 1)) xi-%offset%)
                          (+
                           (f2cl-lib:fref xiold-%data%
                                          ((f2cl-lib:int-sub lold 1))
                                          ((1 1))
                                          xiold-%offset%)
                           (/
                            (- temp
                               (f2cl-lib:fref accum-%data%
                                              ((f2cl-lib:int-sub lold 1))
                                              ((1 1))
                                              accum-%offset%))
                            (f2cl-lib:fref slope-%data%
                                           ((f2cl-lib:int-sub lold 1))
                                           ((1 1))
                                           slope-%offset%))))))
             label300
              (setf in (f2cl-lib:int-add in 1))
              (setf accl accr)
              (setf lold lnew)
             label310))
          (setf mode 1)
         label320
          (setf np1 (f2cl-lib:int-add n 1))
          (if (< iprint 1)
              (f2cl-lib:fformat iout
                                ("~%" " THE NEW MESH (OF" 1 (("~5D"))
                                 " SUBINTERVALS), " 100
                                 ("~%" 8 (("~12,6,0,'*,F"))) "~%")
                                n
                                (do ((i 1 (f2cl-lib:int-add i 1))
                                     (%ret nil))
                                    ((> i np1) (nreverse %ret))
                                  (declare (type f2cl-lib:integer4 i))
                                  (push
                                   (f2cl-lib:fref xi-%data%
                                                  (i)
                                                  ((1 1))
                                                  xi-%offset%)
                                   %ret))))
          (setf nz (f2cl-lib:int-mul mstar (f2cl-lib:int-add n 1)))
          (setf ndmz (f2cl-lib:int-mul kd n))
          (go end_label)
         end_label
          (return (values mode nil nil nil nil nil nil nil nil nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::newmsh
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (1))
                        (array double-float (1)) (array double-float (1))
                        (array double-float (1)) (array double-float (1))
                        (array double-float (1)) (array double-float (1))
                        (fortran-to-lisp::integer4) (array double-float (1)))
           :return-values '(fortran-to-lisp::mode nil nil nil nil nil nil nil
                            nil nil)
           :calls '(fortran-to-lisp::horder fortran-to-lisp::approx))))

