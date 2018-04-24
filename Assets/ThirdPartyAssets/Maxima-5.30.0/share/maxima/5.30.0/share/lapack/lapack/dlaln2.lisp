;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 2edcbd958861 2012/05/30 03:34:52 toy $"
;;;  "f2cl2.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 3fe93de3be82 2012/05/06 02:17:14 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 3fe93de3be82 2012/05/06 02:17:14 toy $")

;;; Using Lisp CMU Common Lisp 20d (20D Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :lapack)


(let* ((zero 0.0) (one 1.0) (two 2.0))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (type (double-float 2.0 2.0) two)
           (ignorable zero one two))
  (let ((ipivot
         (make-array 16
                     :element-type 'f2cl-lib:integer4
                     :initial-contents '(1 2 3 4 2 1 4 3 3 4 1 2 4 3 2 1)))
        (rswap
         (make-array 4 :element-type 't :initial-contents '(nil t nil t)))
        (zswap
         (make-array 4 :element-type 't :initial-contents '(nil nil t t))))
    (declare (type (array f2cl-lib:integer4 (16)) ipivot)
             (type (array f2cl-lib:logical (4)) rswap zswap))
    (defun dlaln2
           (ltrans na nw smin ca a lda d1 d2 b ldb$ wr wi x ldx scale xnorm
            info)
      (declare (type (array double-float (*)) x b a)
               (type (double-float) xnorm scale wi wr d2 d1 ca smin)
               (type (f2cl-lib:integer4) info ldx ldb$ lda nw na)
               (type f2cl-lib:logical ltrans))
      (f2cl-lib:with-multi-array-data
          ((a double-float a-%data% a-%offset%)
           (b double-float b-%data% b-%offset%)
           (x double-float x-%data% x-%offset%))
        (prog ((ci (make-array 4 :element-type 'double-float))
               (civ (make-array 4 :element-type 'double-float))
               (cr (make-array 4 :element-type 'double-float))
               (crv (make-array 4 :element-type 'double-float)) (bbnd 0.0)
               (bi1 0.0) (bi2 0.0) (bignum 0.0) (bnorm 0.0) (br1 0.0) (br2 0.0)
               (ci21 0.0) (ci22 0.0) (cmax 0.0) (cnorm 0.0) (cr21 0.0)
               (cr22 0.0) (csi 0.0) (csr 0.0) (li21 0.0) (lr21 0.0) (smini 0.0)
               (smlnum 0.0) (temp 0.0) (u22abs 0.0) (ui11 0.0) (ui11r 0.0)
               (ui12 0.0) (ui12s 0.0) (ui22 0.0) (ur11 0.0) (ur11r 0.0)
               (ur12 0.0) (ur12s 0.0) (ur22 0.0) (xi1 0.0) (xi2 0.0) (xr1 0.0)
               (xr2 0.0) (icmax 0) (j 0))
          (declare (type (array double-float (4)) ci civ cr crv)
                   (type (double-float) bbnd bi1 bi2 bignum bnorm br1 br2 ci21
                                        ci22 cmax cnorm cr21 cr22 csi csr li21
                                        lr21 smini smlnum temp u22abs ui11
                                        ui11r ui12 ui12s ui22 ur11 ur11r ur12
                                        ur12s ur22 xi1 xi2 xr1 xr2)
                   (type (f2cl-lib:integer4) icmax j))
          (setf smlnum (* two (dlamch "Safe minimum")))
          (setf bignum (/ one smlnum))
          (setf smini (max smin smlnum))
          (setf info 0)
          (setf scale one)
          (cond
            ((= na 1)
             (cond
               ((= nw 1)
                (setf csr
                        (-
                         (* ca
                            (f2cl-lib:fref a-%data%
                                           (1 1)
                                           ((1 lda) (1 *))
                                           a-%offset%))
                         (* wr d1)))
                (setf cnorm (abs csr))
                (cond
                  ((< cnorm smini)
                   (setf csr smini)
                   (setf cnorm smini)
                   (setf info 1)))
                (setf bnorm
                        (abs
                         (f2cl-lib:fref b-%data%
                                        (1 1)
                                        ((1 ldb$) (1 *))
                                        b-%offset%)))
                (cond
                  ((and (< cnorm one) (> bnorm one))
                   (if (> bnorm (* bignum cnorm)) (setf scale (/ one bnorm)))))
                (setf (f2cl-lib:fref x-%data% (1 1) ((1 ldx) (1 *)) x-%offset%)
                        (/
                         (*
                          (f2cl-lib:fref b-%data%
                                         (1 1)
                                         ((1 ldb$) (1 *))
                                         b-%offset%)
                          scale)
                         csr))
                (setf xnorm
                        (abs
                         (f2cl-lib:fref x-%data%
                                        (1 1)
                                        ((1 ldx) (1 *))
                                        x-%offset%))))
               (t
                (setf csr
                        (-
                         (* ca
                            (f2cl-lib:fref a-%data%
                                           (1 1)
                                           ((1 lda) (1 *))
                                           a-%offset%))
                         (* wr d1)))
                (setf csi (* (- wi) d1))
                (setf cnorm (+ (abs csr) (abs csi)))
                (cond
                  ((< cnorm smini)
                   (setf csr smini)
                   (setf csi zero)
                   (setf cnorm smini)
                   (setf info 1)))
                (setf bnorm
                        (+
                         (abs
                          (f2cl-lib:fref b-%data%
                                         (1 1)
                                         ((1 ldb$) (1 *))
                                         b-%offset%))
                         (abs
                          (f2cl-lib:fref b-%data%
                                         (1 2)
                                         ((1 ldb$) (1 *))
                                         b-%offset%))))
                (cond
                  ((and (< cnorm one) (> bnorm one))
                   (if (> bnorm (* bignum cnorm)) (setf scale (/ one bnorm)))))
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                    (dladiv
                     (* scale
                        (f2cl-lib:fref b-%data%
                                       (1 1)
                                       ((1 ldb$) (1 *))
                                       b-%offset%))
                     (* scale
                        (f2cl-lib:fref b-%data%
                                       (1 2)
                                       ((1 ldb$) (1 *))
                                       b-%offset%))
                     csr csi
                     (f2cl-lib:fref x-%data% (1 1) ((1 ldx) (1 *)) x-%offset%)
                     (f2cl-lib:fref x-%data% (1 2) ((1 ldx) (1 *)) x-%offset%))
                  (declare (ignore var-0 var-1 var-2 var-3))
                  (setf (f2cl-lib:fref x-%data%
                                       (1 1)
                                       ((1 ldx) (1 *))
                                       x-%offset%)
                          var-4)
                  (setf (f2cl-lib:fref x-%data%
                                       (1 2)
                                       ((1 ldx) (1 *))
                                       x-%offset%)
                          var-5))
                (setf xnorm
                        (+
                         (abs
                          (f2cl-lib:fref x-%data%
                                         (1 1)
                                         ((1 ldx) (1 *))
                                         x-%offset%))
                         (abs
                          (f2cl-lib:fref x-%data%
                                         (1 2)
                                         ((1 ldx) (1 *))
                                         x-%offset%)))))))
            (t
             (setf (f2cl-lib:fref crv (1) ((1 4)))
                     (-
                      (* ca
                         (f2cl-lib:fref a-%data%
                                        (1 1)
                                        ((1 lda) (1 *))
                                        a-%offset%))
                      (* wr d1)))
             (setf (f2cl-lib:fref crv (4) ((1 4)))
                     (-
                      (* ca
                         (f2cl-lib:fref a-%data%
                                        (2 2)
                                        ((1 lda) (1 *))
                                        a-%offset%))
                      (* wr d2)))
             (cond
               (ltrans
                (setf (f2cl-lib:fref crv (3) ((1 4)))
                        (* ca
                           (f2cl-lib:fref a-%data%
                                          (2 1)
                                          ((1 lda) (1 *))
                                          a-%offset%)))
                (setf (f2cl-lib:fref crv (2) ((1 4)))
                        (* ca
                           (f2cl-lib:fref a-%data%
                                          (1 2)
                                          ((1 lda) (1 *))
                                          a-%offset%))))
               (t
                (setf (f2cl-lib:fref crv (2) ((1 4)))
                        (* ca
                           (f2cl-lib:fref a-%data%
                                          (2 1)
                                          ((1 lda) (1 *))
                                          a-%offset%)))
                (setf (f2cl-lib:fref crv (3) ((1 4)))
                        (* ca
                           (f2cl-lib:fref a-%data%
                                          (1 2)
                                          ((1 lda) (1 *))
                                          a-%offset%)))))
             (cond
               ((= nw 1)
                (setf cmax zero)
                (setf icmax 0)
                (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                              ((> j 4) nil)
                  (tagbody
                    (cond
                      ((> (abs (f2cl-lib:fref crv (j) ((1 4)))) cmax)
                       (setf cmax (abs (f2cl-lib:fref crv (j) ((1 4)))))
                       (setf icmax j)))
                   label10))
                (cond
                  ((< cmax smini)
                   (setf bnorm
                           (max
                            (abs
                             (f2cl-lib:fref b-%data%
                                            (1 1)
                                            ((1 ldb$) (1 *))
                                            b-%offset%))
                            (abs
                             (f2cl-lib:fref b-%data%
                                            (2 1)
                                            ((1 ldb$) (1 *))
                                            b-%offset%))))
                   (cond
                     ((and (< smini one) (> bnorm one))
                      (if (> bnorm (* bignum smini))
                          (setf scale (/ one bnorm)))))
                   (setf temp (/ scale smini))
                   (setf (f2cl-lib:fref x-%data%
                                        (1 1)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           (* temp
                              (f2cl-lib:fref b-%data%
                                             (1 1)
                                             ((1 ldb$) (1 *))
                                             b-%offset%)))
                   (setf (f2cl-lib:fref x-%data%
                                        (2 1)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           (* temp
                              (f2cl-lib:fref b-%data%
                                             (2 1)
                                             ((1 ldb$) (1 *))
                                             b-%offset%)))
                   (setf xnorm (* temp bnorm))
                   (setf info 1)
                   (go end_label)))
                (setf ur11 (f2cl-lib:fref crv (icmax) ((1 4))))
                (setf cr21
                        (f2cl-lib:fref crv
                                       ((f2cl-lib:fref ipivot
                                                       (2 icmax)
                                                       ((1 4) (1 4))))
                                       ((1 4))))
                (setf ur12
                        (f2cl-lib:fref crv
                                       ((f2cl-lib:fref ipivot
                                                       (3 icmax)
                                                       ((1 4) (1 4))))
                                       ((1 4))))
                (setf cr22
                        (f2cl-lib:fref crv
                                       ((f2cl-lib:fref ipivot
                                                       (4 icmax)
                                                       ((1 4) (1 4))))
                                       ((1 4))))
                (setf ur11r (/ one ur11))
                (setf lr21 (* ur11r cr21))
                (setf ur22 (- cr22 (* ur12 lr21)))
                (cond
                  ((< (abs ur22) smini)
                   (setf ur22 smini)
                   (setf info 1)))
                (cond
                  ((f2cl-lib:fref rswap (icmax) ((1 4)))
                   (setf br1
                           (f2cl-lib:fref b-%data%
                                          (2 1)
                                          ((1 ldb$) (1 *))
                                          b-%offset%))
                   (setf br2
                           (f2cl-lib:fref b-%data%
                                          (1 1)
                                          ((1 ldb$) (1 *))
                                          b-%offset%)))
                  (t
                   (setf br1
                           (f2cl-lib:fref b-%data%
                                          (1 1)
                                          ((1 ldb$) (1 *))
                                          b-%offset%))
                   (setf br2
                           (f2cl-lib:fref b-%data%
                                          (2 1)
                                          ((1 ldb$) (1 *))
                                          b-%offset%))))
                (setf br2 (- br2 (* lr21 br1)))
                (setf bbnd (max (abs (* br1 (* ur22 ur11r))) (abs br2)))
                (cond
                  ((and (> bbnd one) (< (abs ur22) one))
                   (if (>= bbnd (* bignum (abs ur22)))
                       (setf scale (/ one bbnd)))))
                (setf xr2 (/ (* br2 scale) ur22))
                (setf xr1 (- (* scale br1 ur11r) (* xr2 (* ur11r ur12))))
                (cond
                  ((f2cl-lib:fref zswap (icmax) ((1 4)))
                   (setf (f2cl-lib:fref x-%data%
                                        (1 1)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           xr2)
                   (setf (f2cl-lib:fref x-%data%
                                        (2 1)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           xr1))
                  (t
                   (setf (f2cl-lib:fref x-%data%
                                        (1 1)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           xr1)
                   (setf (f2cl-lib:fref x-%data%
                                        (2 1)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           xr2)))
                (setf xnorm (max (abs xr1) (abs xr2)))
                (cond
                  ((and (> xnorm one) (> cmax one))
                   (cond
                     ((> xnorm (f2cl-lib:f2cl/ bignum cmax))
                      (setf temp (/ cmax bignum))
                      (setf (f2cl-lib:fref x-%data%
                                           (1 1)
                                           ((1 ldx) (1 *))
                                           x-%offset%)
                              (* temp
                                 (f2cl-lib:fref x-%data%
                                                (1 1)
                                                ((1 ldx) (1 *))
                                                x-%offset%)))
                      (setf (f2cl-lib:fref x-%data%
                                           (2 1)
                                           ((1 ldx) (1 *))
                                           x-%offset%)
                              (* temp
                                 (f2cl-lib:fref x-%data%
                                                (2 1)
                                                ((1 ldx) (1 *))
                                                x-%offset%)))
                      (setf xnorm (* temp xnorm))
                      (setf scale (* temp scale)))))))
               (t
                (setf (f2cl-lib:fref civ (1) ((1 4))) (* (- wi) d1))
                (setf (f2cl-lib:fref civ (2) ((1 4))) zero)
                (setf (f2cl-lib:fref civ (3) ((1 4))) zero)
                (setf (f2cl-lib:fref civ (4) ((1 4))) (* (- wi) d2))
                (setf cmax zero)
                (setf icmax 0)
                (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                              ((> j 4) nil)
                  (tagbody
                    (cond
                      ((>
                        (+ (abs (f2cl-lib:fref crv (j) ((1 4))))
                           (abs (f2cl-lib:fref civ (j) ((1 4)))))
                        cmax)
                       (setf cmax
                               (+ (abs (f2cl-lib:fref crv (j) ((1 4))))
                                  (abs (f2cl-lib:fref civ (j) ((1 4))))))
                       (setf icmax j)))
                   label20))
                (cond
                  ((< cmax smini)
                   (setf bnorm
                           (max
                            (+
                             (abs
                              (f2cl-lib:fref b-%data%
                                             (1 1)
                                             ((1 ldb$) (1 *))
                                             b-%offset%))
                             (abs
                              (f2cl-lib:fref b-%data%
                                             (1 2)
                                             ((1 ldb$) (1 *))
                                             b-%offset%)))
                            (+
                             (abs
                              (f2cl-lib:fref b-%data%
                                             (2 1)
                                             ((1 ldb$) (1 *))
                                             b-%offset%))
                             (abs
                              (f2cl-lib:fref b-%data%
                                             (2 2)
                                             ((1 ldb$) (1 *))
                                             b-%offset%)))))
                   (cond
                     ((and (< smini one) (> bnorm one))
                      (if (> bnorm (* bignum smini))
                          (setf scale (/ one bnorm)))))
                   (setf temp (/ scale smini))
                   (setf (f2cl-lib:fref x-%data%
                                        (1 1)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           (* temp
                              (f2cl-lib:fref b-%data%
                                             (1 1)
                                             ((1 ldb$) (1 *))
                                             b-%offset%)))
                   (setf (f2cl-lib:fref x-%data%
                                        (2 1)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           (* temp
                              (f2cl-lib:fref b-%data%
                                             (2 1)
                                             ((1 ldb$) (1 *))
                                             b-%offset%)))
                   (setf (f2cl-lib:fref x-%data%
                                        (1 2)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           (* temp
                              (f2cl-lib:fref b-%data%
                                             (1 2)
                                             ((1 ldb$) (1 *))
                                             b-%offset%)))
                   (setf (f2cl-lib:fref x-%data%
                                        (2 2)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           (* temp
                              (f2cl-lib:fref b-%data%
                                             (2 2)
                                             ((1 ldb$) (1 *))
                                             b-%offset%)))
                   (setf xnorm (* temp bnorm))
                   (setf info 1)
                   (go end_label)))
                (setf ur11 (f2cl-lib:fref crv (icmax) ((1 4))))
                (setf ui11 (f2cl-lib:fref civ (icmax) ((1 4))))
                (setf cr21
                        (f2cl-lib:fref crv
                                       ((f2cl-lib:fref ipivot
                                                       (2 icmax)
                                                       ((1 4) (1 4))))
                                       ((1 4))))
                (setf ci21
                        (f2cl-lib:fref civ
                                       ((f2cl-lib:fref ipivot
                                                       (2 icmax)
                                                       ((1 4) (1 4))))
                                       ((1 4))))
                (setf ur12
                        (f2cl-lib:fref crv
                                       ((f2cl-lib:fref ipivot
                                                       (3 icmax)
                                                       ((1 4) (1 4))))
                                       ((1 4))))
                (setf ui12
                        (f2cl-lib:fref civ
                                       ((f2cl-lib:fref ipivot
                                                       (3 icmax)
                                                       ((1 4) (1 4))))
                                       ((1 4))))
                (setf cr22
                        (f2cl-lib:fref crv
                                       ((f2cl-lib:fref ipivot
                                                       (4 icmax)
                                                       ((1 4) (1 4))))
                                       ((1 4))))
                (setf ci22
                        (f2cl-lib:fref civ
                                       ((f2cl-lib:fref ipivot
                                                       (4 icmax)
                                                       ((1 4) (1 4))))
                                       ((1 4))))
                (cond
                  ((or (= icmax 1) (= icmax 4))
                   (cond
                     ((> (abs ur11) (abs ui11))
                      (setf temp (/ ui11 ur11))
                      (setf ur11r (/ one (* ur11 (+ one (expt temp 2)))))
                      (setf ui11r (* (- temp) ur11r)))
                     (t
                      (setf temp (/ ur11 ui11))
                      (setf ui11r (/ (- one) (* ui11 (+ one (expt temp 2)))))
                      (setf ur11r (* (- temp) ui11r))))
                   (setf lr21 (* cr21 ur11r))
                   (setf li21 (* cr21 ui11r))
                   (setf ur12s (* ur12 ur11r))
                   (setf ui12s (* ur12 ui11r))
                   (setf ur22 (- cr22 (* ur12 lr21)))
                   (setf ui22 (- ci22 (* ur12 li21))))
                  (t
                   (setf ur11r (/ one ur11))
                   (setf ui11r zero)
                   (setf lr21 (* cr21 ur11r))
                   (setf li21 (* ci21 ur11r))
                   (setf ur12s (* ur12 ur11r))
                   (setf ui12s (* ui12 ur11r))
                   (setf ur22 (+ (- cr22 (* ur12 lr21)) (* ui12 li21)))
                   (setf ui22 (- (* (- ur12) li21) (* ui12 lr21)))))
                (setf u22abs (+ (abs ur22) (abs ui22)))
                (cond
                  ((< u22abs smini)
                   (setf ur22 smini)
                   (setf ui22 zero)
                   (setf info 1)))
                (cond
                  ((f2cl-lib:fref rswap (icmax) ((1 4)))
                   (setf br2
                           (f2cl-lib:fref b-%data%
                                          (1 1)
                                          ((1 ldb$) (1 *))
                                          b-%offset%))
                   (setf br1
                           (f2cl-lib:fref b-%data%
                                          (2 1)
                                          ((1 ldb$) (1 *))
                                          b-%offset%))
                   (setf bi2
                           (f2cl-lib:fref b-%data%
                                          (1 2)
                                          ((1 ldb$) (1 *))
                                          b-%offset%))
                   (setf bi1
                           (f2cl-lib:fref b-%data%
                                          (2 2)
                                          ((1 ldb$) (1 *))
                                          b-%offset%)))
                  (t
                   (setf br1
                           (f2cl-lib:fref b-%data%
                                          (1 1)
                                          ((1 ldb$) (1 *))
                                          b-%offset%))
                   (setf br2
                           (f2cl-lib:fref b-%data%
                                          (2 1)
                                          ((1 ldb$) (1 *))
                                          b-%offset%))
                   (setf bi1
                           (f2cl-lib:fref b-%data%
                                          (1 2)
                                          ((1 ldb$) (1 *))
                                          b-%offset%))
                   (setf bi2
                           (f2cl-lib:fref b-%data%
                                          (2 2)
                                          ((1 ldb$) (1 *))
                                          b-%offset%))))
                (setf br2 (+ (- br2 (* lr21 br1)) (* li21 bi1)))
                (setf bi2 (- bi2 (* li21 br1) (* lr21 bi1)))
                (setf bbnd
                        (max
                         (* (+ (abs br1) (abs bi1))
                            (* u22abs (+ (abs ur11r) (abs ui11r))))
                         (+ (abs br2) (abs bi2))))
                (cond
                  ((and (> bbnd one) (< u22abs one))
                   (cond
                     ((>= bbnd (* bignum u22abs))
                      (setf scale (/ one bbnd))
                      (setf br1 (* scale br1))
                      (setf bi1 (* scale bi1))
                      (setf br2 (* scale br2))
                      (setf bi2 (* scale bi2))))))
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                    (dladiv br2 bi2 ur22 ui22 xr2 xi2)
                  (declare (ignore var-0 var-1 var-2 var-3))
                  (setf xr2 var-4)
                  (setf xi2 var-5))
                (setf xr1
                        (+ (- (* ur11r br1) (* ui11r bi1) (* ur12s xr2))
                           (* ui12s xi2)))
                (setf xi1
                        (- (+ (* ui11r br1) (* ur11r bi1))
                           (* ui12s xr2)
                           (* ur12s xi2)))
                (cond
                  ((f2cl-lib:fref zswap (icmax) ((1 4)))
                   (setf (f2cl-lib:fref x-%data%
                                        (1 1)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           xr2)
                   (setf (f2cl-lib:fref x-%data%
                                        (2 1)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           xr1)
                   (setf (f2cl-lib:fref x-%data%
                                        (1 2)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           xi2)
                   (setf (f2cl-lib:fref x-%data%
                                        (2 2)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           xi1))
                  (t
                   (setf (f2cl-lib:fref x-%data%
                                        (1 1)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           xr1)
                   (setf (f2cl-lib:fref x-%data%
                                        (2 1)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           xr2)
                   (setf (f2cl-lib:fref x-%data%
                                        (1 2)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           xi1)
                   (setf (f2cl-lib:fref x-%data%
                                        (2 2)
                                        ((1 ldx) (1 *))
                                        x-%offset%)
                           xi2)))
                (setf xnorm
                        (max (+ (abs xr1) (abs xi1)) (+ (abs xr2) (abs xi2))))
                (cond
                  ((and (> xnorm one) (> cmax one))
                   (cond
                     ((> xnorm (f2cl-lib:f2cl/ bignum cmax))
                      (setf temp (/ cmax bignum))
                      (setf (f2cl-lib:fref x-%data%
                                           (1 1)
                                           ((1 ldx) (1 *))
                                           x-%offset%)
                              (* temp
                                 (f2cl-lib:fref x-%data%
                                                (1 1)
                                                ((1 ldx) (1 *))
                                                x-%offset%)))
                      (setf (f2cl-lib:fref x-%data%
                                           (2 1)
                                           ((1 ldx) (1 *))
                                           x-%offset%)
                              (* temp
                                 (f2cl-lib:fref x-%data%
                                                (2 1)
                                                ((1 ldx) (1 *))
                                                x-%offset%)))
                      (setf (f2cl-lib:fref x-%data%
                                           (1 2)
                                           ((1 ldx) (1 *))
                                           x-%offset%)
                              (* temp
                                 (f2cl-lib:fref x-%data%
                                                (1 2)
                                                ((1 ldx) (1 *))
                                                x-%offset%)))
                      (setf (f2cl-lib:fref x-%data%
                                           (2 2)
                                           ((1 ldx) (1 *))
                                           x-%offset%)
                              (* temp
                                 (f2cl-lib:fref x-%data%
                                                (2 2)
                                                ((1 ldx) (1 *))
                                                x-%offset%)))
                      (setf xnorm (* temp xnorm))
                      (setf scale (* temp scale))))))))))
          (go end_label)
         end_label
          (return
           (values nil
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
                   scale
                   xnorm
                   info)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlaln2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(fortran-to-lisp::logical (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil fortran-to-lisp::scale
                            fortran-to-lisp::xnorm fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dladiv fortran-to-lisp::dlamch))))

