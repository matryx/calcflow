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


(let* ((zero 0.0) (one 1.0) (two 2.0) (half 0.5) (eight 8.0))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (type (double-float 2.0 2.0) two)
           (type (double-float 0.5 0.5) half)
           (type (double-float 8.0 8.0) eight)
           (ignorable zero one two half eight))
  (let ((bswpiv
         (make-array 4 :element-type 't :initial-contents '(nil t nil t)))
        (xswpiv
         (make-array 4 :element-type 't :initial-contents '(nil nil t t)))
        (locu22
         (make-array 4
                     :element-type 'f2cl-lib:integer4
                     :initial-contents '(4 3 2 1)))
        (locl21
         (make-array 4
                     :element-type 'f2cl-lib:integer4
                     :initial-contents '(2 1 4 3)))
        (locu12
         (make-array 4
                     :element-type 'f2cl-lib:integer4
                     :initial-contents '(3 4 1 2))))
    (declare (type (array f2cl-lib:logical (4)) bswpiv xswpiv)
             (type (array f2cl-lib:integer4 (4)) locu22 locl21 locu12))
    (defun dlasy2
           (ltranl ltranr isgn n1 n2 tl ldtl tr ldtr b ldb$ scale x ldx xnorm
            info)
      (declare (type (double-float) xnorm scale)
               (type (array double-float (*)) x b tr tl)
               (type (f2cl-lib:integer4) info ldx ldb$ ldtr ldtl n2 n1 isgn)
               (type f2cl-lib:logical ltranr ltranl))
      (f2cl-lib:with-multi-array-data
          ((tl double-float tl-%data% tl-%offset%)
           (tr double-float tr-%data% tr-%offset%)
           (b double-float b-%data% b-%offset%)
           (x double-float x-%data% x-%offset%))
        (prog ((btmp (make-array 4 :element-type 'double-float))
               (t16 (make-array 16 :element-type 'double-float))
               (tmp (make-array 4 :element-type 'double-float))
               (x2 (make-array 2 :element-type 'double-float))
               (jpiv (make-array 4 :element-type 'f2cl-lib:integer4)) (bet 0.0)
               (eps 0.0) (gam 0.0) (l21 0.0) (sgn 0.0) (smin 0.0) (smlnum 0.0)
               (tau1 0.0) (temp 0.0) (u11 0.0) (u12 0.0) (u22 0.0) (xmax 0.0)
               (i 0) (ip 0) (ipiv 0) (ipsv 0) (j 0) (jp 0) (jpsv 0) (k 0)
               (bswap nil) (xswap nil))
          (declare (type (array double-float (16)) t16)
                   (type (array double-float (4)) btmp tmp)
                   (type (array double-float (2)) x2)
                   (type (array f2cl-lib:integer4 (4)) jpiv)
                   (type (double-float) bet eps gam l21 sgn smin smlnum tau1
                                        temp u11 u12 u22 xmax)
                   (type (f2cl-lib:integer4) i ip ipiv ipsv j jp jpsv k)
                   (type f2cl-lib:logical bswap xswap))
          (setf info 0)
          (if (or (= n1 0) (= n2 0)) (go end_label))
          (setf eps (dlamch "P"))
          (setf smlnum (/ (dlamch "S") eps))
          (setf sgn (coerce (the f2cl-lib:integer4 isgn) 'double-float))
          (setf k (f2cl-lib:int-sub (f2cl-lib:int-add n1 n1 n2) 2))
          (f2cl-lib:computed-goto (label10 label20 label30 label50) k)
         label10
          (setf tau1
                  (+
                   (f2cl-lib:fref tl-%data% (1 1) ((1 ldtl) (1 *)) tl-%offset%)
                   (* sgn
                      (f2cl-lib:fref tr-%data%
                                     (1 1)
                                     ((1 ldtr) (1 *))
                                     tr-%offset%))))
          (setf bet (abs tau1))
          (cond
            ((<= bet smlnum)
             (setf tau1 smlnum)
             (setf bet smlnum)
             (setf info 1)))
          (setf scale one)
          (setf gam
                  (abs
                   (f2cl-lib:fref b-%data% (1 1) ((1 ldb$) (1 *)) b-%offset%)))
          (if (> (* smlnum gam) bet) (setf scale (/ one gam)))
          (setf (f2cl-lib:fref x-%data% (1 1) ((1 ldx) (1 *)) x-%offset%)
                  (/
                   (*
                    (f2cl-lib:fref b-%data% (1 1) ((1 ldb$) (1 *)) b-%offset%)
                    scale)
                   tau1))
          (setf xnorm
                  (abs
                   (f2cl-lib:fref x-%data% (1 1) ((1 ldx) (1 *)) x-%offset%)))
          (go end_label)
         label20
          (setf smin
                  (max
                   (* eps
                      (max
                       (abs
                        (f2cl-lib:fref tl-%data%
                                       (1 1)
                                       ((1 ldtl) (1 *))
                                       tl-%offset%))
                       (abs
                        (f2cl-lib:fref tr-%data%
                                       (1 1)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%))
                       (abs
                        (f2cl-lib:fref tr-%data%
                                       (1 2)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%))
                       (abs
                        (f2cl-lib:fref tr-%data%
                                       (2 1)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%))
                       (abs
                        (f2cl-lib:fref tr-%data%
                                       (2 2)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%))))
                   smlnum))
          (setf (f2cl-lib:fref tmp (1) ((1 4)))
                  (+
                   (f2cl-lib:fref tl-%data% (1 1) ((1 ldtl) (1 *)) tl-%offset%)
                   (* sgn
                      (f2cl-lib:fref tr-%data%
                                     (1 1)
                                     ((1 ldtr) (1 *))
                                     tr-%offset%))))
          (setf (f2cl-lib:fref tmp (4) ((1 4)))
                  (+
                   (f2cl-lib:fref tl-%data% (1 1) ((1 ldtl) (1 *)) tl-%offset%)
                   (* sgn
                      (f2cl-lib:fref tr-%data%
                                     (2 2)
                                     ((1 ldtr) (1 *))
                                     tr-%offset%))))
          (cond
            (ltranr
             (setf (f2cl-lib:fref tmp (2) ((1 4)))
                     (* sgn
                        (f2cl-lib:fref tr-%data%
                                       (2 1)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%)))
             (setf (f2cl-lib:fref tmp (3) ((1 4)))
                     (* sgn
                        (f2cl-lib:fref tr-%data%
                                       (1 2)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%))))
            (t
             (setf (f2cl-lib:fref tmp (2) ((1 4)))
                     (* sgn
                        (f2cl-lib:fref tr-%data%
                                       (1 2)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%)))
             (setf (f2cl-lib:fref tmp (3) ((1 4)))
                     (* sgn
                        (f2cl-lib:fref tr-%data%
                                       (2 1)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%)))))
          (setf (f2cl-lib:fref btmp (1) ((1 4)))
                  (f2cl-lib:fref b-%data% (1 1) ((1 ldb$) (1 *)) b-%offset%))
          (setf (f2cl-lib:fref btmp (2) ((1 4)))
                  (f2cl-lib:fref b-%data% (1 2) ((1 ldb$) (1 *)) b-%offset%))
          (go label40)
         label30
          (setf smin
                  (max
                   (* eps
                      (max
                       (abs
                        (f2cl-lib:fref tr-%data%
                                       (1 1)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%))
                       (abs
                        (f2cl-lib:fref tl-%data%
                                       (1 1)
                                       ((1 ldtl) (1 *))
                                       tl-%offset%))
                       (abs
                        (f2cl-lib:fref tl-%data%
                                       (1 2)
                                       ((1 ldtl) (1 *))
                                       tl-%offset%))
                       (abs
                        (f2cl-lib:fref tl-%data%
                                       (2 1)
                                       ((1 ldtl) (1 *))
                                       tl-%offset%))
                       (abs
                        (f2cl-lib:fref tl-%data%
                                       (2 2)
                                       ((1 ldtl) (1 *))
                                       tl-%offset%))))
                   smlnum))
          (setf (f2cl-lib:fref tmp (1) ((1 4)))
                  (+
                   (f2cl-lib:fref tl-%data% (1 1) ((1 ldtl) (1 *)) tl-%offset%)
                   (* sgn
                      (f2cl-lib:fref tr-%data%
                                     (1 1)
                                     ((1 ldtr) (1 *))
                                     tr-%offset%))))
          (setf (f2cl-lib:fref tmp (4) ((1 4)))
                  (+
                   (f2cl-lib:fref tl-%data% (2 2) ((1 ldtl) (1 *)) tl-%offset%)
                   (* sgn
                      (f2cl-lib:fref tr-%data%
                                     (1 1)
                                     ((1 ldtr) (1 *))
                                     tr-%offset%))))
          (cond
            (ltranl
             (setf (f2cl-lib:fref tmp (2) ((1 4)))
                     (f2cl-lib:fref tl-%data%
                                    (1 2)
                                    ((1 ldtl) (1 *))
                                    tl-%offset%))
             (setf (f2cl-lib:fref tmp (3) ((1 4)))
                     (f2cl-lib:fref tl-%data%
                                    (2 1)
                                    ((1 ldtl) (1 *))
                                    tl-%offset%)))
            (t
             (setf (f2cl-lib:fref tmp (2) ((1 4)))
                     (f2cl-lib:fref tl-%data%
                                    (2 1)
                                    ((1 ldtl) (1 *))
                                    tl-%offset%))
             (setf (f2cl-lib:fref tmp (3) ((1 4)))
                     (f2cl-lib:fref tl-%data%
                                    (1 2)
                                    ((1 ldtl) (1 *))
                                    tl-%offset%))))
          (setf (f2cl-lib:fref btmp (1) ((1 4)))
                  (f2cl-lib:fref b-%data% (1 1) ((1 ldb$) (1 *)) b-%offset%))
          (setf (f2cl-lib:fref btmp (2) ((1 4)))
                  (f2cl-lib:fref b-%data% (2 1) ((1 ldb$) (1 *)) b-%offset%))
         label40
          (setf ipiv (idamax 4 tmp 1))
          (setf u11 (f2cl-lib:fref tmp (ipiv) ((1 4))))
          (cond
            ((<= (abs u11) smin)
             (setf info 1)
             (setf u11 smin)))
          (setf u12
                  (f2cl-lib:fref tmp
                                 ((f2cl-lib:fref locu12 (ipiv) ((1 4))))
                                 ((1 4))))
          (setf l21
                  (/
                   (f2cl-lib:fref tmp
                                  ((f2cl-lib:fref locl21 (ipiv) ((1 4))))
                                  ((1 4)))
                   u11))
          (setf u22
                  (-
                   (f2cl-lib:fref tmp
                                  ((f2cl-lib:fref locu22 (ipiv) ((1 4))))
                                  ((1 4)))
                   (* u12 l21)))
          (setf xswap (f2cl-lib:fref xswpiv (ipiv) ((1 4))))
          (setf bswap (f2cl-lib:fref bswpiv (ipiv) ((1 4))))
          (cond
            ((<= (abs u22) smin)
             (setf info 1)
             (setf u22 smin)))
          (cond
            (bswap
             (setf temp (f2cl-lib:fref btmp (2) ((1 4))))
             (setf (f2cl-lib:fref btmp (2) ((1 4)))
                     (- (f2cl-lib:fref btmp (1) ((1 4))) (* l21 temp)))
             (setf (f2cl-lib:fref btmp (1) ((1 4))) temp))
            (t
             (setf (f2cl-lib:fref btmp (2) ((1 4)))
                     (- (f2cl-lib:fref btmp (2) ((1 4)))
                        (* l21 (f2cl-lib:fref btmp (1) ((1 4))))))))
          (setf scale one)
          (cond
            ((or
              (> (* two smlnum (abs (f2cl-lib:fref btmp (2) ((1 4)))))
                 (abs u22))
              (> (* two smlnum (abs (f2cl-lib:fref btmp (1) ((1 4)))))
                 (abs u11)))
             (setf scale
                     (/ half
                        (max (abs (f2cl-lib:fref btmp (1) ((1 4))))
                             (abs (f2cl-lib:fref btmp (2) ((1 4)))))))
             (setf (f2cl-lib:fref btmp (1) ((1 4)))
                     (* (f2cl-lib:fref btmp (1) ((1 4))) scale))
             (setf (f2cl-lib:fref btmp (2) ((1 4)))
                     (* (f2cl-lib:fref btmp (2) ((1 4))) scale))))
          (setf (f2cl-lib:fref x2 (2) ((1 2)))
                  (/ (f2cl-lib:fref btmp (2) ((1 4))) u22))
          (setf (f2cl-lib:fref x2 (1) ((1 2)))
                  (- (/ (f2cl-lib:fref btmp (1) ((1 4))) u11)
                     (* (/ u12 u11) (f2cl-lib:fref x2 (2) ((1 2))))))
          (cond
            (xswap
             (setf temp (f2cl-lib:fref x2 (2) ((1 2))))
             (setf (f2cl-lib:fref x2 (2) ((1 2)))
                     (f2cl-lib:fref x2 (1) ((1 2))))
             (setf (f2cl-lib:fref x2 (1) ((1 2))) temp)))
          (setf (f2cl-lib:fref x-%data% (1 1) ((1 ldx) (1 *)) x-%offset%)
                  (f2cl-lib:fref x2 (1) ((1 2))))
          (cond
            ((= n1 1)
             (setf (f2cl-lib:fref x-%data% (1 2) ((1 ldx) (1 *)) x-%offset%)
                     (f2cl-lib:fref x2 (2) ((1 2))))
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
                                      x-%offset%)))))
            (t
             (setf (f2cl-lib:fref x-%data% (2 1) ((1 ldx) (1 *)) x-%offset%)
                     (f2cl-lib:fref x2 (2) ((1 2))))
             (setf xnorm
                     (max
                      (abs
                       (f2cl-lib:fref x-%data%
                                      (1 1)
                                      ((1 ldx) (1 *))
                                      x-%offset%))
                      (abs
                       (f2cl-lib:fref x-%data%
                                      (2 1)
                                      ((1 ldx) (1 *))
                                      x-%offset%))))))
          (go end_label)
         label50
          (setf smin
                  (max
                   (abs
                    (f2cl-lib:fref tr-%data%
                                   (1 1)
                                   ((1 ldtr) (1 *))
                                   tr-%offset%))
                   (abs
                    (f2cl-lib:fref tr-%data%
                                   (1 2)
                                   ((1 ldtr) (1 *))
                                   tr-%offset%))
                   (abs
                    (f2cl-lib:fref tr-%data%
                                   (2 1)
                                   ((1 ldtr) (1 *))
                                   tr-%offset%))
                   (abs
                    (f2cl-lib:fref tr-%data%
                                   (2 2)
                                   ((1 ldtr) (1 *))
                                   tr-%offset%))))
          (setf smin
                  (max smin
                       (abs
                        (f2cl-lib:fref tl-%data%
                                       (1 1)
                                       ((1 ldtl) (1 *))
                                       tl-%offset%))
                       (abs
                        (f2cl-lib:fref tl-%data%
                                       (1 2)
                                       ((1 ldtl) (1 *))
                                       tl-%offset%))
                       (abs
                        (f2cl-lib:fref tl-%data%
                                       (2 1)
                                       ((1 ldtl) (1 *))
                                       tl-%offset%))
                       (abs
                        (f2cl-lib:fref tl-%data%
                                       (2 2)
                                       ((1 ldtl) (1 *))
                                       tl-%offset%))))
          (setf smin (max (* eps smin) smlnum))
          (setf (f2cl-lib:fref btmp (1) ((1 4))) zero)
          (dcopy 16 btmp 0 t16 1)
          (setf (f2cl-lib:fref t16 (1 1) ((1 4) (1 4)))
                  (+
                   (f2cl-lib:fref tl-%data% (1 1) ((1 ldtl) (1 *)) tl-%offset%)
                   (* sgn
                      (f2cl-lib:fref tr-%data%
                                     (1 1)
                                     ((1 ldtr) (1 *))
                                     tr-%offset%))))
          (setf (f2cl-lib:fref t16 (2 2) ((1 4) (1 4)))
                  (+
                   (f2cl-lib:fref tl-%data% (2 2) ((1 ldtl) (1 *)) tl-%offset%)
                   (* sgn
                      (f2cl-lib:fref tr-%data%
                                     (1 1)
                                     ((1 ldtr) (1 *))
                                     tr-%offset%))))
          (setf (f2cl-lib:fref t16 (3 3) ((1 4) (1 4)))
                  (+
                   (f2cl-lib:fref tl-%data% (1 1) ((1 ldtl) (1 *)) tl-%offset%)
                   (* sgn
                      (f2cl-lib:fref tr-%data%
                                     (2 2)
                                     ((1 ldtr) (1 *))
                                     tr-%offset%))))
          (setf (f2cl-lib:fref t16 (4 4) ((1 4) (1 4)))
                  (+
                   (f2cl-lib:fref tl-%data% (2 2) ((1 ldtl) (1 *)) tl-%offset%)
                   (* sgn
                      (f2cl-lib:fref tr-%data%
                                     (2 2)
                                     ((1 ldtr) (1 *))
                                     tr-%offset%))))
          (cond
            (ltranl
             (setf (f2cl-lib:fref t16 (1 2) ((1 4) (1 4)))
                     (f2cl-lib:fref tl-%data%
                                    (2 1)
                                    ((1 ldtl) (1 *))
                                    tl-%offset%))
             (setf (f2cl-lib:fref t16 (2 1) ((1 4) (1 4)))
                     (f2cl-lib:fref tl-%data%
                                    (1 2)
                                    ((1 ldtl) (1 *))
                                    tl-%offset%))
             (setf (f2cl-lib:fref t16 (3 4) ((1 4) (1 4)))
                     (f2cl-lib:fref tl-%data%
                                    (2 1)
                                    ((1 ldtl) (1 *))
                                    tl-%offset%))
             (setf (f2cl-lib:fref t16 (4 3) ((1 4) (1 4)))
                     (f2cl-lib:fref tl-%data%
                                    (1 2)
                                    ((1 ldtl) (1 *))
                                    tl-%offset%)))
            (t
             (setf (f2cl-lib:fref t16 (1 2) ((1 4) (1 4)))
                     (f2cl-lib:fref tl-%data%
                                    (1 2)
                                    ((1 ldtl) (1 *))
                                    tl-%offset%))
             (setf (f2cl-lib:fref t16 (2 1) ((1 4) (1 4)))
                     (f2cl-lib:fref tl-%data%
                                    (2 1)
                                    ((1 ldtl) (1 *))
                                    tl-%offset%))
             (setf (f2cl-lib:fref t16 (3 4) ((1 4) (1 4)))
                     (f2cl-lib:fref tl-%data%
                                    (1 2)
                                    ((1 ldtl) (1 *))
                                    tl-%offset%))
             (setf (f2cl-lib:fref t16 (4 3) ((1 4) (1 4)))
                     (f2cl-lib:fref tl-%data%
                                    (2 1)
                                    ((1 ldtl) (1 *))
                                    tl-%offset%))))
          (cond
            (ltranr
             (setf (f2cl-lib:fref t16 (1 3) ((1 4) (1 4)))
                     (* sgn
                        (f2cl-lib:fref tr-%data%
                                       (1 2)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%)))
             (setf (f2cl-lib:fref t16 (2 4) ((1 4) (1 4)))
                     (* sgn
                        (f2cl-lib:fref tr-%data%
                                       (1 2)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%)))
             (setf (f2cl-lib:fref t16 (3 1) ((1 4) (1 4)))
                     (* sgn
                        (f2cl-lib:fref tr-%data%
                                       (2 1)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%)))
             (setf (f2cl-lib:fref t16 (4 2) ((1 4) (1 4)))
                     (* sgn
                        (f2cl-lib:fref tr-%data%
                                       (2 1)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%))))
            (t
             (setf (f2cl-lib:fref t16 (1 3) ((1 4) (1 4)))
                     (* sgn
                        (f2cl-lib:fref tr-%data%
                                       (2 1)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%)))
             (setf (f2cl-lib:fref t16 (2 4) ((1 4) (1 4)))
                     (* sgn
                        (f2cl-lib:fref tr-%data%
                                       (2 1)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%)))
             (setf (f2cl-lib:fref t16 (3 1) ((1 4) (1 4)))
                     (* sgn
                        (f2cl-lib:fref tr-%data%
                                       (1 2)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%)))
             (setf (f2cl-lib:fref t16 (4 2) ((1 4) (1 4)))
                     (* sgn
                        (f2cl-lib:fref tr-%data%
                                       (1 2)
                                       ((1 ldtr) (1 *))
                                       tr-%offset%)))))
          (setf (f2cl-lib:fref btmp (1) ((1 4)))
                  (f2cl-lib:fref b-%data% (1 1) ((1 ldb$) (1 *)) b-%offset%))
          (setf (f2cl-lib:fref btmp (2) ((1 4)))
                  (f2cl-lib:fref b-%data% (2 1) ((1 ldb$) (1 *)) b-%offset%))
          (setf (f2cl-lib:fref btmp (3) ((1 4)))
                  (f2cl-lib:fref b-%data% (1 2) ((1 ldb$) (1 *)) b-%offset%))
          (setf (f2cl-lib:fref btmp (4) ((1 4)))
                  (f2cl-lib:fref b-%data% (2 2) ((1 ldb$) (1 *)) b-%offset%))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i 3) nil)
            (tagbody
              (setf xmax zero)
              (f2cl-lib:fdo (ip i (f2cl-lib:int-add ip 1))
                            ((> ip 4) nil)
                (tagbody
                  (f2cl-lib:fdo (jp i (f2cl-lib:int-add jp 1))
                                ((> jp 4) nil)
                    (tagbody
                      (cond
                        ((>= (abs (f2cl-lib:fref t16 (ip jp) ((1 4) (1 4))))
                             xmax)
                         (setf xmax
                                 (abs
                                  (f2cl-lib:fref t16 (ip jp) ((1 4) (1 4)))))
                         (setf ipsv ip)
                         (setf jpsv jp)))
                     label60))
                 label70))
              (cond
                ((/= ipsv i)
                 (dswap 4
                  (f2cl-lib:array-slice t16
                                        double-float
                                        (ipsv 1)
                                        ((1 4) (1 4)))
                  4 (f2cl-lib:array-slice t16 double-float (i 1) ((1 4) (1 4)))
                  4)
                 (setf temp (f2cl-lib:fref btmp (i) ((1 4))))
                 (setf (f2cl-lib:fref btmp (i) ((1 4)))
                         (f2cl-lib:fref btmp (ipsv) ((1 4))))
                 (setf (f2cl-lib:fref btmp (ipsv) ((1 4))) temp)))
              (if (/= jpsv i)
                  (dswap 4
                   (f2cl-lib:array-slice t16
                                         double-float
                                         (1 jpsv)
                                         ((1 4) (1 4)))
                   1
                   (f2cl-lib:array-slice t16 double-float (1 i) ((1 4) (1 4)))
                   1))
              (setf (f2cl-lib:fref jpiv (i) ((1 4))) jpsv)
              (cond
                ((< (abs (f2cl-lib:fref t16 (i i) ((1 4) (1 4)))) smin)
                 (setf info 1)
                 (setf (f2cl-lib:fref t16 (i i) ((1 4) (1 4))) smin)))
              (f2cl-lib:fdo (j (f2cl-lib:int-add i 1) (f2cl-lib:int-add j 1))
                            ((> j 4) nil)
                (tagbody
                  (setf (f2cl-lib:fref t16 (j i) ((1 4) (1 4)))
                          (/ (f2cl-lib:fref t16 (j i) ((1 4) (1 4)))
                             (f2cl-lib:fref t16 (i i) ((1 4) (1 4)))))
                  (setf (f2cl-lib:fref btmp (j) ((1 4)))
                          (- (f2cl-lib:fref btmp (j) ((1 4)))
                             (* (f2cl-lib:fref t16 (j i) ((1 4) (1 4)))
                                (f2cl-lib:fref btmp (i) ((1 4))))))
                  (f2cl-lib:fdo (k (f2cl-lib:int-add i 1)
                                 (f2cl-lib:int-add k 1))
                                ((> k 4) nil)
                    (tagbody
                      (setf (f2cl-lib:fref t16 (j k) ((1 4) (1 4)))
                              (- (f2cl-lib:fref t16 (j k) ((1 4) (1 4)))
                                 (* (f2cl-lib:fref t16 (j i) ((1 4) (1 4)))
                                    (f2cl-lib:fref t16 (i k) ((1 4) (1 4))))))
                     label80))
                 label90))
             label100))
          (if (< (abs (f2cl-lib:fref t16 (4 4) ((1 4) (1 4)))) smin)
              (setf (f2cl-lib:fref t16 (4 4) ((1 4) (1 4))) smin))
          (setf scale one)
          (cond
            ((or
              (> (* eight smlnum (abs (f2cl-lib:fref btmp (1) ((1 4)))))
                 (abs (f2cl-lib:fref t16 (1 1) ((1 4) (1 4)))))
              (> (* eight smlnum (abs (f2cl-lib:fref btmp (2) ((1 4)))))
                 (abs (f2cl-lib:fref t16 (2 2) ((1 4) (1 4)))))
              (> (* eight smlnum (abs (f2cl-lib:fref btmp (3) ((1 4)))))
                 (abs (f2cl-lib:fref t16 (3 3) ((1 4) (1 4)))))
              (> (* eight smlnum (abs (f2cl-lib:fref btmp (4) ((1 4)))))
                 (abs (f2cl-lib:fref t16 (4 4) ((1 4) (1 4))))))
             (setf scale
                     (/ (/ one eight)
                        (max (abs (f2cl-lib:fref btmp (1) ((1 4))))
                             (abs (f2cl-lib:fref btmp (2) ((1 4))))
                             (abs (f2cl-lib:fref btmp (3) ((1 4))))
                             (abs (f2cl-lib:fref btmp (4) ((1 4)))))))
             (setf (f2cl-lib:fref btmp (1) ((1 4)))
                     (* (f2cl-lib:fref btmp (1) ((1 4))) scale))
             (setf (f2cl-lib:fref btmp (2) ((1 4)))
                     (* (f2cl-lib:fref btmp (2) ((1 4))) scale))
             (setf (f2cl-lib:fref btmp (3) ((1 4)))
                     (* (f2cl-lib:fref btmp (3) ((1 4))) scale))
             (setf (f2cl-lib:fref btmp (4) ((1 4)))
                     (* (f2cl-lib:fref btmp (4) ((1 4))) scale))))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i 4) nil)
            (tagbody
              (setf k (f2cl-lib:int-sub 5 i))
              (setf temp (/ one (f2cl-lib:fref t16 (k k) ((1 4) (1 4)))))
              (setf (f2cl-lib:fref tmp (k) ((1 4)))
                      (* (f2cl-lib:fref btmp (k) ((1 4))) temp))
              (f2cl-lib:fdo (j (f2cl-lib:int-add k 1) (f2cl-lib:int-add j 1))
                            ((> j 4) nil)
                (tagbody
                  (setf (f2cl-lib:fref tmp (k) ((1 4)))
                          (- (f2cl-lib:fref tmp (k) ((1 4)))
                             (* temp
                                (f2cl-lib:fref t16 (k j) ((1 4) (1 4)))
                                (f2cl-lib:fref tmp (j) ((1 4))))))
                 label110))
             label120))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i 3) nil)
            (tagbody
              (cond
                ((/=
                  (f2cl-lib:fref jpiv
                                 ((f2cl-lib:int-add 4 (f2cl-lib:int-sub i)))
                                 ((1 4)))
                  (f2cl-lib:int-add 4 (f2cl-lib:int-sub i)))
                 (setf temp
                         (f2cl-lib:fref tmp ((f2cl-lib:int-sub 4 i)) ((1 4))))
                 (setf (f2cl-lib:fref tmp ((f2cl-lib:int-sub 4 i)) ((1 4)))
                         (f2cl-lib:fref tmp
                                        ((f2cl-lib:fref jpiv
                                                        ((f2cl-lib:int-sub 4
                                                                           i))
                                                        ((1 4))))
                                        ((1 4))))
                 (setf (f2cl-lib:fref tmp
                                      ((f2cl-lib:fref jpiv
                                                      ((f2cl-lib:int-sub 4 i))
                                                      ((1 4))))
                                      ((1 4)))
                         temp)))
             label130))
          (setf (f2cl-lib:fref x-%data% (1 1) ((1 ldx) (1 *)) x-%offset%)
                  (f2cl-lib:fref tmp (1) ((1 4))))
          (setf (f2cl-lib:fref x-%data% (2 1) ((1 ldx) (1 *)) x-%offset%)
                  (f2cl-lib:fref tmp (2) ((1 4))))
          (setf (f2cl-lib:fref x-%data% (1 2) ((1 ldx) (1 *)) x-%offset%)
                  (f2cl-lib:fref tmp (3) ((1 4))))
          (setf (f2cl-lib:fref x-%data% (2 2) ((1 ldx) (1 *)) x-%offset%)
                  (f2cl-lib:fref tmp (4) ((1 4))))
          (setf xnorm
                  (max
                   (+ (abs (f2cl-lib:fref tmp (1) ((1 4))))
                      (abs (f2cl-lib:fref tmp (3) ((1 4)))))
                   (+ (abs (f2cl-lib:fref tmp (2) ((1 4))))
                      (abs (f2cl-lib:fref tmp (4) ((1 4)))))))
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
                   scale
                   nil
                   nil
                   xnorm
                   info)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasy2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(fortran-to-lisp::logical fortran-to-lisp::logical
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (double-float) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::scale nil nil
                            fortran-to-lisp::xnorm fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dswap fortran-to-lisp::dcopy
                    fortran-to-lisp::idamax fortran-to-lisp::dlamch))))

