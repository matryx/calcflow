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


(let* ((zero 0.0) (one 1.0))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (ignorable zero one))
  (defun dgesvd (jobu jobvt m n a lda s u ldu vt ldvt work lwork info)
    (declare (type (array double-float (*)) work vt u s a)
             (type (f2cl-lib:integer4) info lwork ldvt ldu lda n m)
             (type (simple-string *) jobvt jobu))
    (f2cl-lib:with-multi-array-data
        ((jobu character jobu-%data% jobu-%offset%)
         (jobvt character jobvt-%data% jobvt-%offset%)
         (a double-float a-%data% a-%offset%)
         (s double-float s-%data% s-%offset%)
         (u double-float u-%data% u-%offset%)
         (vt double-float vt-%data% vt-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((dum (make-array 1 :element-type 'double-float)) (anrm 0.0)
             (bignum 0.0) (eps 0.0) (smlnum 0.0) (bdspac 0) (blk 0) (chunk 0)
             (i 0) (ie 0) (ierr 0) (ir 0) (iscl 0) (itau 0) (itaup 0) (itauq 0)
             (iu 0) (iwork 0) (ldwrkr 0) (ldwrku 0) (maxwrk 0) (minmn 0)
             (minwrk 0) (mnthr 0) (ncu 0) (ncvt 0) (nru 0) (nrvt 0) (wrkbl 0)
             (lquery nil) (wntua nil) (wntuas nil) (wntun nil) (wntuo nil)
             (wntus nil) (wntva nil) (wntvas nil) (wntvn nil) (wntvo nil)
             (wntvs nil))
        (declare (type (array double-float (1)) dum)
                 (type (double-float) anrm bignum eps smlnum)
                 (type (f2cl-lib:integer4) bdspac blk chunk i ie ierr ir iscl
                                           itau itaup itauq iu iwork ldwrkr
                                           ldwrku maxwrk minmn minwrk mnthr ncu
                                           ncvt nru nrvt wrkbl)
                 (type f2cl-lib:logical lquery wntua wntuas wntun wntuo wntus
                                        wntva wntvas wntvn wntvo wntvs))
        (setf info 0)
        (setf minmn (min (the f2cl-lib:integer4 m) (the f2cl-lib:integer4 n)))
        (setf mnthr (ilaenv 6 "DGESVD" (f2cl-lib:f2cl-// jobu jobvt) m n 0 0))
        (setf wntua (lsame jobu "A"))
        (setf wntus (lsame jobu "S"))
        (setf wntuas (or wntua wntus))
        (setf wntuo (lsame jobu "O"))
        (setf wntun (lsame jobu "N"))
        (setf wntva (lsame jobvt "A"))
        (setf wntvs (lsame jobvt "S"))
        (setf wntvas (or wntva wntvs))
        (setf wntvo (lsame jobvt "O"))
        (setf wntvn (lsame jobvt "N"))
        (setf minwrk 1)
        (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
        (cond
          ((not (or wntua wntus wntuo wntun))
           (setf info -1))
          ((or (not (or wntva wntvs wntvo wntvn)) (and wntvo wntuo))
           (setf info -2))
          ((< m 0)
           (setf info -3))
          ((< n 0)
           (setf info -4))
          ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
           (setf info -6))
          ((or (< ldu 1) (and wntuas (< ldu m)))
           (setf info -9))
          ((or (< ldvt 1) (and wntva (< ldvt n)) (and wntvs (< ldvt minmn)))
           (setf info -11)))
        (cond
          ((and (= info 0) (or (>= lwork 1) lquery) (> m 0) (> n 0))
           (cond
             ((>= m n)
              (setf bdspac (f2cl-lib:int-mul 5 n))
              (cond
                ((>= m mnthr)
                 (cond
                   (wntun
                    (setf maxwrk
                            (f2cl-lib:int-add n
                                              (f2cl-lib:int-mul n
                                                                (ilaenv 1
                                                                 "DGEQRF" " " m
                                                                 n -1 -1))))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul 2
                                                                          n
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           n n
                                                                           -1
                                                                           -1))))))
                    (if (or wntvo wntvas)
                        (setf maxwrk
                                (max (the f2cl-lib:integer4 maxwrk)
                                     (the f2cl-lib:integer4
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 3 n)
                                           (f2cl-lib:int-mul
                                            (f2cl-lib:int-sub n 1)
                                            (ilaenv 1 "DORGBR" "P" n n n
                                             -1)))))))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf minwrk
                            (max (the f2cl-lib:integer4 (f2cl-lib:int-mul 4 n))
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))
                   ((and wntuo wntvn)
                    (setf wrkbl
                            (f2cl-lib:int-add n
                                              (f2cl-lib:int-mul n
                                                                (ilaenv 1
                                                                 "DGEQRF" " " m
                                                                 n -1 -1))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add n
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORGQR"
                                                                           " "
                                                                           m n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul 2
                                                                          n
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           n n
                                                                           -1
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORGBR"
                                                                           "Q"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                                    wrkbl))
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                                    (f2cl-lib:int-mul m n)
                                                    n))))
                    (setf minwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 3 n) m))
                             (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))
                   ((and wntuo wntvas)
                    (setf wrkbl
                            (f2cl-lib:int-add n
                                              (f2cl-lib:int-mul n
                                                                (ilaenv 1
                                                                 "DGEQRF" " " m
                                                                 n -1 -1))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add n
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORGQR"
                                                                           " "
                                                                           m n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul 2
                                                                          n
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           n n
                                                                           -1
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORGBR"
                                                                           "Q"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul
                                                         (f2cl-lib:int-sub n 1)
                                                         (ilaenv 1 "DORGBR" "P"
                                                          n n n -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                                    wrkbl))
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                                    (f2cl-lib:int-mul m n)
                                                    n))))
                    (setf minwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 3 n) m))
                             (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))
                   ((and wntus wntvn)
                    (setf wrkbl
                            (f2cl-lib:int-add n
                                              (f2cl-lib:int-mul n
                                                                (ilaenv 1
                                                                 "DGEQRF" " " m
                                                                 n -1 -1))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add n
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORGQR"
                                                                           " "
                                                                           m n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul 2
                                                                          n
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           n n
                                                                           -1
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORGBR"
                                                                           "Q"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul n n) wrkbl))
                    (setf minwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 3 n) m))
                             (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))
                   ((and wntus wntvo)
                    (setf wrkbl
                            (f2cl-lib:int-add n
                                              (f2cl-lib:int-mul n
                                                                (ilaenv 1
                                                                 "DGEQRF" " " m
                                                                 n -1 -1))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add n
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORGQR"
                                                                           " "
                                                                           m n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul 2
                                                                          n
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           n n
                                                                           -1
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORGBR"
                                                                           "Q"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul
                                                         (f2cl-lib:int-sub n 1)
                                                         (ilaenv 1 "DORGBR" "P"
                                                          n n n -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul 2 n n) wrkbl))
                    (setf minwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 3 n) m))
                             (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))
                   ((and wntus wntvas)
                    (setf wrkbl
                            (f2cl-lib:int-add n
                                              (f2cl-lib:int-mul n
                                                                (ilaenv 1
                                                                 "DGEQRF" " " m
                                                                 n -1 -1))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add n
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORGQR"
                                                                           " "
                                                                           m n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul 2
                                                                          n
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           n n
                                                                           -1
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORGBR"
                                                                           "Q"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul
                                                         (f2cl-lib:int-sub n 1)
                                                         (ilaenv 1 "DORGBR" "P"
                                                          n n n -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul n n) wrkbl))
                    (setf minwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 3 n) m))
                             (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))
                   ((and wntua wntvn)
                    (setf wrkbl
                            (f2cl-lib:int-add n
                                              (f2cl-lib:int-mul n
                                                                (ilaenv 1
                                                                 "DGEQRF" " " m
                                                                 n -1 -1))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add n
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORGQR"
                                                                           " "
                                                                           m m
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul 2
                                                                          n
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           n n
                                                                           -1
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORGBR"
                                                                           "Q"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul n n) wrkbl))
                    (setf minwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 3 n) m))
                             (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))
                   ((and wntua wntvo)
                    (setf wrkbl
                            (f2cl-lib:int-add n
                                              (f2cl-lib:int-mul n
                                                                (ilaenv 1
                                                                 "DGEQRF" " " m
                                                                 n -1 -1))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add n
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORGQR"
                                                                           " "
                                                                           m m
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul 2
                                                                          n
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           n n
                                                                           -1
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORGBR"
                                                                           "Q"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul
                                                         (f2cl-lib:int-sub n 1)
                                                         (ilaenv 1 "DORGBR" "P"
                                                          n n n -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul 2 n n) wrkbl))
                    (setf minwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 3 n) m))
                             (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))
                   ((and wntua wntvas)
                    (setf wrkbl
                            (f2cl-lib:int-add n
                                              (f2cl-lib:int-mul n
                                                                (ilaenv 1
                                                                 "DGEQRF" " " m
                                                                 n -1 -1))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add n
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORGQR"
                                                                           " "
                                                                           m m
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul 2
                                                                          n
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           n n
                                                                           -1
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORGBR"
                                                                           "Q"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul
                                                         (f2cl-lib:int-sub n 1)
                                                         (ilaenv 1 "DORGBR" "P"
                                                          n n n -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul n n) wrkbl))
                    (setf minwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 3 n) m))
                             (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))))
                (t
                 (setf maxwrk
                         (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                           (f2cl-lib:int-mul
                                            (f2cl-lib:int-add m n)
                                            (ilaenv 1 "DGEBRD" " " m n -1 -1))))
                 (if (or wntus wntuo)
                     (setf maxwrk
                             (max (the f2cl-lib:integer4 maxwrk)
                                  (the f2cl-lib:integer4
                                       (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                         (f2cl-lib:int-mul n
                                                                           (ilaenv
                                                                            1
                                                                            "DORGBR"
                                                                            "Q"
                                                                            m n
                                                                            n
                                                                            -1)))))))
                 (if wntua
                     (setf maxwrk
                             (max (the f2cl-lib:integer4 maxwrk)
                                  (the f2cl-lib:integer4
                                       (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                         (f2cl-lib:int-mul m
                                                                           (ilaenv
                                                                            1
                                                                            "DORGBR"
                                                                            "Q"
                                                                            m m
                                                                            n
                                                                            -1)))))))
                 (if (not wntvn)
                     (setf maxwrk
                             (max (the f2cl-lib:integer4 maxwrk)
                                  (the f2cl-lib:integer4
                                       (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                         (f2cl-lib:int-mul
                                                          (f2cl-lib:int-sub n
                                                                            1)
                                                          (ilaenv 1 "DORGBR"
                                                           "P" n n n -1)))))))
                 (setf maxwrk
                         (max (the f2cl-lib:integer4 maxwrk)
                              (the f2cl-lib:integer4 bdspac)))
                 (setf minwrk
                         (max
                          (the f2cl-lib:integer4
                               (f2cl-lib:int-add (f2cl-lib:int-mul 3 n) m))
                          (the f2cl-lib:integer4 bdspac)))
                 (setf maxwrk
                         (max (the f2cl-lib:integer4 maxwrk)
                              (the f2cl-lib:integer4 minwrk))))))
             (t
              (setf bdspac (f2cl-lib:int-mul 5 m))
              (cond
                ((>= n mnthr)
                 (cond
                   (wntvn
                    (setf maxwrk
                            (f2cl-lib:int-add m
                                              (f2cl-lib:int-mul m
                                                                (ilaenv 1
                                                                 "DGELQF" " " m
                                                                 n -1 -1))))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul 2
                                                                          m
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           m m
                                                                           -1
                                                                           -1))))))
                    (if (or wntuo wntuas)
                        (setf maxwrk
                                (max (the f2cl-lib:integer4 maxwrk)
                                     (the f2cl-lib:integer4
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 3 m)
                                           (f2cl-lib:int-mul m
                                                             (ilaenv 1 "DORGBR"
                                                              "Q" m m m
                                                              -1)))))))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf minwrk
                            (max (the f2cl-lib:integer4 (f2cl-lib:int-mul 4 m))
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))
                   ((and wntvo wntun)
                    (setf wrkbl
                            (f2cl-lib:int-add m
                                              (f2cl-lib:int-mul m
                                                                (ilaenv 1
                                                                 "DGELQF" " " m
                                                                 n -1 -1))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add m
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORGLQ"
                                                                           " "
                                                                           m n
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul 2
                                                                          m
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           m m
                                                                           -1
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul
                                                         (f2cl-lib:int-sub m 1)
                                                         (ilaenv 1 "DORGBR" "P"
                                                          m m m -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                                    wrkbl))
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                                    (f2cl-lib:int-mul m n)
                                                    m))))
                    (setf minwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 3 m) n))
                             (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))
                   ((and wntvo wntuas)
                    (setf wrkbl
                            (f2cl-lib:int-add m
                                              (f2cl-lib:int-mul m
                                                                (ilaenv 1
                                                                 "DGELQF" " " m
                                                                 n -1 -1))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add m
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORGLQ"
                                                                           " "
                                                                           m n
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul 2
                                                                          m
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           m m
                                                                           -1
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul
                                                         (f2cl-lib:int-sub m 1)
                                                         (ilaenv 1 "DORGBR" "P"
                                                          m m m -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORGBR"
                                                                           "Q"
                                                                           m m
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                                    wrkbl))
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                                    (f2cl-lib:int-mul m n)
                                                    m))))
                    (setf minwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 3 m) n))
                             (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))
                   ((and wntvs wntun)
                    (setf wrkbl
                            (f2cl-lib:int-add m
                                              (f2cl-lib:int-mul m
                                                                (ilaenv 1
                                                                 "DGELQF" " " m
                                                                 n -1 -1))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add m
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORGLQ"
                                                                           " "
                                                                           m n
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul 2
                                                                          m
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           m m
                                                                           -1
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul
                                                         (f2cl-lib:int-sub m 1)
                                                         (ilaenv 1 "DORGBR" "P"
                                                          m m m -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul m m) wrkbl))
                    (setf minwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 3 m) n))
                             (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))
                   ((and wntvs wntuo)
                    (setf wrkbl
                            (f2cl-lib:int-add m
                                              (f2cl-lib:int-mul m
                                                                (ilaenv 1
                                                                 "DGELQF" " " m
                                                                 n -1 -1))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add m
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORGLQ"
                                                                           " "
                                                                           m n
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul 2
                                                                          m
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           m m
                                                                           -1
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul
                                                         (f2cl-lib:int-sub m 1)
                                                         (ilaenv 1 "DORGBR" "P"
                                                          m m m -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORGBR"
                                                                           "Q"
                                                                           m m
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul 2 m m) wrkbl))
                    (setf minwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 3 m) n))
                             (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))
                   ((and wntvs wntuas)
                    (setf wrkbl
                            (f2cl-lib:int-add m
                                              (f2cl-lib:int-mul m
                                                                (ilaenv 1
                                                                 "DGELQF" " " m
                                                                 n -1 -1))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add m
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORGLQ"
                                                                           " "
                                                                           m n
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul 2
                                                                          m
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           m m
                                                                           -1
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul
                                                         (f2cl-lib:int-sub m 1)
                                                         (ilaenv 1 "DORGBR" "P"
                                                          m m m -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORGBR"
                                                                           "Q"
                                                                           m m
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul m m) wrkbl))
                    (setf minwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 3 m) n))
                             (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))
                   ((and wntva wntun)
                    (setf wrkbl
                            (f2cl-lib:int-add m
                                              (f2cl-lib:int-mul m
                                                                (ilaenv 1
                                                                 "DGELQF" " " m
                                                                 n -1 -1))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add m
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORGLQ"
                                                                           " "
                                                                           n n
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul 2
                                                                          m
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           m m
                                                                           -1
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul
                                                         (f2cl-lib:int-sub m 1)
                                                         (ilaenv 1 "DORGBR" "P"
                                                          m m m -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul m m) wrkbl))
                    (setf minwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 3 m) n))
                             (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))
                   ((and wntva wntuo)
                    (setf wrkbl
                            (f2cl-lib:int-add m
                                              (f2cl-lib:int-mul m
                                                                (ilaenv 1
                                                                 "DGELQF" " " m
                                                                 n -1 -1))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add m
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORGLQ"
                                                                           " "
                                                                           n n
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul 2
                                                                          m
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           m m
                                                                           -1
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul
                                                         (f2cl-lib:int-sub m 1)
                                                         (ilaenv 1 "DORGBR" "P"
                                                          m m m -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORGBR"
                                                                           "Q"
                                                                           m m
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul 2 m m) wrkbl))
                    (setf minwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 3 m) n))
                             (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))
                   ((and wntva wntuas)
                    (setf wrkbl
                            (f2cl-lib:int-add m
                                              (f2cl-lib:int-mul m
                                                                (ilaenv 1
                                                                 "DGELQF" " " m
                                                                 n -1 -1))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add m
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORGLQ"
                                                                           " "
                                                                           n n
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul 2
                                                                          m
                                                                          (ilaenv
                                                                           1
                                                                           "DGEBRD"
                                                                           " "
                                                                           m m
                                                                           -1
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul
                                                         (f2cl-lib:int-sub m 1)
                                                         (ilaenv 1 "DORGBR" "P"
                                                          m m m -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORGBR"
                                                                           "Q"
                                                                           m m
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul m m) wrkbl))
                    (setf minwrk
                            (max
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add (f2cl-lib:int-mul 3 m) n))
                             (the f2cl-lib:integer4 bdspac)))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4 minwrk))))))
                (t
                 (setf maxwrk
                         (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                           (f2cl-lib:int-mul
                                            (f2cl-lib:int-add m n)
                                            (ilaenv 1 "DGEBRD" " " m n -1 -1))))
                 (if (or wntvs wntvo)
                     (setf maxwrk
                             (max (the f2cl-lib:integer4 maxwrk)
                                  (the f2cl-lib:integer4
                                       (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                         (f2cl-lib:int-mul m
                                                                           (ilaenv
                                                                            1
                                                                            "DORGBR"
                                                                            "P"
                                                                            m n
                                                                            m
                                                                            -1)))))))
                 (if wntva
                     (setf maxwrk
                             (max (the f2cl-lib:integer4 maxwrk)
                                  (the f2cl-lib:integer4
                                       (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                         (f2cl-lib:int-mul n
                                                                           (ilaenv
                                                                            1
                                                                            "DORGBR"
                                                                            "P"
                                                                            n n
                                                                            m
                                                                            -1)))))))
                 (if (not wntun)
                     (setf maxwrk
                             (max (the f2cl-lib:integer4 maxwrk)
                                  (the f2cl-lib:integer4
                                       (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                         (f2cl-lib:int-mul
                                                          (f2cl-lib:int-sub m
                                                                            1)
                                                          (ilaenv 1 "DORGBR"
                                                           "Q" m m m -1)))))))
                 (setf maxwrk
                         (max (the f2cl-lib:integer4 maxwrk)
                              (the f2cl-lib:integer4 bdspac)))
                 (setf minwrk
                         (max
                          (the f2cl-lib:integer4
                               (f2cl-lib:int-add (f2cl-lib:int-mul 3 m) n))
                          (the f2cl-lib:integer4 bdspac)))
                 (setf maxwrk
                         (max (the f2cl-lib:integer4 maxwrk)
                              (the f2cl-lib:integer4 minwrk)))))))
           (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                   (coerce (the f2cl-lib:integer4 maxwrk) 'double-float))))
        (cond
          ((and (< lwork minwrk) (not lquery))
           (setf info -13)))
        (cond
          ((/= info 0)
           (xerbla "DGESVD" (f2cl-lib:int-sub info))
           (go end_label))
          (lquery
           (go end_label)))
        (cond
          ((or (= m 0) (= n 0))
           (if (>= lwork 1)
               (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%) one))
           (go end_label)))
        (setf eps (dlamch "P"))
        (setf smlnum (/ (f2cl-lib:fsqrt (dlamch "S")) eps))
        (setf bignum (/ one smlnum))
        (setf anrm (dlange "M" m n a lda dum))
        (setf iscl 0)
        (cond
          ((and (> anrm zero) (< anrm smlnum))
           (setf iscl 1)
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
               (dlascl "G" 0 0 anrm smlnum m n a lda ierr)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8))
             (setf ierr var-9)))
          ((> anrm bignum)
           (setf iscl 1)
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
               (dlascl "G" 0 0 anrm bignum m n a lda ierr)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8))
             (setf ierr var-9))))
        (cond
          ((>= m n)
           (cond
             ((>= m mnthr)
              (cond
                (wntun
                 (setf itau 1)
                 (setf iwork (f2cl-lib:int-add itau n))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                     (dgeqrf m n a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itau)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                   (setf ierr var-7))
                 (dlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1) zero
                  zero
                  (f2cl-lib:array-slice a-%data%
                                        double-float
                                        (2 1)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                  lda)
                 (setf ie 1)
                 (setf itauq (f2cl-lib:int-add ie n))
                 (setf itaup (f2cl-lib:int-add itauq n))
                 (setf iwork (f2cl-lib:int-add itaup n))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10)
                     (dgebrd n n a lda s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itauq)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itaup)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9))
                   (setf ierr var-10))
                 (setf ncvt 0)
                 (cond
                   ((or wntvo wntvas)
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9)
                        (dorgbr "P" n n n a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itaup)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8))
                      (setf ierr var-9))
                    (setf ncvt n)))
                 (setf iwork (f2cl-lib:int-add ie n))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13 var-14)
                     (dbdsqr "U" n ncvt 0 0 s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      a lda dum 1 dum 1
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12
                                    var-13))
                   (setf info var-14))
                 (if wntvas (dlacpy "F" n n a lda vt ldvt)))
                ((and wntuo wntvn)
                 (cond
                   ((>= lwork
                        (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                          (max
                                           (the f2cl-lib:integer4
                                                (f2cl-lib:int-mul 4 n))
                                           (the f2cl-lib:integer4 bdspac))))
                    (setf ir 1)
                    (cond
                      ((>= lwork
                           (f2cl-lib:int-add
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add
                                       (f2cl-lib:int-mul lda n)
                                       n)))
                            (f2cl-lib:int-mul lda n)))
                       (setf ldwrku lda)
                       (setf ldwrkr lda))
                      ((>= lwork
                           (f2cl-lib:int-add
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add
                                       (f2cl-lib:int-mul lda n)
                                       n)))
                            (f2cl-lib:int-mul n n)))
                       (setf ldwrku lda)
                       (setf ldwrkr n))
                      (t
                       (setf ldwrku
                               (the f2cl-lib:integer4
                                    (truncate (- lwork (* n n) n) n)))
                       (setf ldwrkr n)))
                    (setf itau
                            (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr n)))
                    (setf iwork (f2cl-lib:int-add itau n))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                        (dgeqrf m n a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itau)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6))
                      (setf ierr var-7))
                    (dlacpy "U" n n a lda
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (ir)
                                           ((1 *))
                                           work-%offset%)
                     ldwrkr)
                    (dlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                     zero zero
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           ((+ ir 1))
                                           ((1 *))
                                           work-%offset%)
                     ldwrkr)
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8)
                        (dorgqr m n n a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itau)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7))
                      (setf ierr var-8))
                    (setf ie itau)
                    (setf itauq (f2cl-lib:int-add ie n))
                    (setf itaup (f2cl-lib:int-add itauq n))
                    (setf iwork (f2cl-lib:int-add itaup n))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10)
                        (dgebrd n n
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ir)
                                               ((1 *))
                                               work-%offset%)
                         ldwrkr s
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ie)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itauq)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itaup)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9))
                      (setf ierr var-10))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9)
                        (dorgbr "Q" n n n
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ir)
                                               ((1 *))
                                               work-%offset%)
                         ldwrkr
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itauq)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8))
                      (setf ierr var-9))
                    (setf iwork (f2cl-lib:int-add ie n))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                        (dbdsqr "U" n 0 n 0 s
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ie)
                                               ((1 *))
                                               work-%offset%)
                         dum 1
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ir)
                                               ((1 *))
                                               work-%offset%)
                         ldwrkr dum 1
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         info)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9 var-10 var-11
                                       var-12 var-13))
                      (setf info var-14))
                    (setf iu (f2cl-lib:int-add ie n))
                    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i ldwrku))
                                  ((> i m) nil)
                      (tagbody
                        (setf chunk
                                (min
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-sub m i)
                                                        1))
                                 (the f2cl-lib:integer4 ldwrku)))
                        (dgemm "N" "N" chunk n n one
                         (f2cl-lib:array-slice a-%data%
                                               double-float
                                               (i 1)
                                               ((1 lda) (1 *))
                                               a-%offset%)
                         lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ir)
                                               ((1 *))
                                               work-%offset%)
                         ldwrkr zero
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iu)
                                               ((1 *))
                                               work-%offset%)
                         ldwrku)
                        (dlacpy "F" chunk n
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iu)
                                               ((1 *))
                                               work-%offset%)
                         ldwrku
                         (f2cl-lib:array-slice a-%data%
                                               double-float
                                               (i 1)
                                               ((1 lda) (1 *))
                                               a-%offset%)
                         lda)
                       label10)))
                   (t
                    (setf ie 1)
                    (setf itauq (f2cl-lib:int-add ie n))
                    (setf itaup (f2cl-lib:int-add itauq n))
                    (setf iwork (f2cl-lib:int-add itaup n))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10)
                        (dgebrd m n a lda s
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ie)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itauq)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itaup)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9))
                      (setf ierr var-10))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9)
                        (dorgbr "Q" m n n a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itauq)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8))
                      (setf ierr var-9))
                    (setf iwork (f2cl-lib:int-add ie n))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                        (dbdsqr "U" n 0 m 0 s
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ie)
                                               ((1 *))
                                               work-%offset%)
                         dum 1 a lda dum 1
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         info)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9 var-10 var-11
                                       var-12 var-13))
                      (setf info var-14)))))
                ((and wntuo wntvas)
                 (cond
                   ((>= lwork
                        (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                          (max
                                           (the f2cl-lib:integer4
                                                (f2cl-lib:int-mul 4 n))
                                           (the f2cl-lib:integer4 bdspac))))
                    (setf ir 1)
                    (cond
                      ((>= lwork
                           (f2cl-lib:int-add
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add
                                       (f2cl-lib:int-mul lda n)
                                       n)))
                            (f2cl-lib:int-mul lda n)))
                       (setf ldwrku lda)
                       (setf ldwrkr lda))
                      ((>= lwork
                           (f2cl-lib:int-add
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add
                                       (f2cl-lib:int-mul lda n)
                                       n)))
                            (f2cl-lib:int-mul n n)))
                       (setf ldwrku lda)
                       (setf ldwrkr n))
                      (t
                       (setf ldwrku
                               (the f2cl-lib:integer4
                                    (truncate (- lwork (* n n) n) n)))
                       (setf ldwrkr n)))
                    (setf itau
                            (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr n)))
                    (setf iwork (f2cl-lib:int-add itau n))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                        (dgeqrf m n a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itau)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6))
                      (setf ierr var-7))
                    (dlacpy "U" n n a lda vt ldvt)
                    (dlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                     zero zero
                     (f2cl-lib:array-slice vt-%data%
                                           double-float
                                           (2 1)
                                           ((1 ldvt) (1 *))
                                           vt-%offset%)
                     ldvt)
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8)
                        (dorgqr m n n a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itau)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7))
                      (setf ierr var-8))
                    (setf ie itau)
                    (setf itauq (f2cl-lib:int-add ie n))
                    (setf itaup (f2cl-lib:int-add itauq n))
                    (setf iwork (f2cl-lib:int-add itaup n))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10)
                        (dgebrd n n vt ldvt s
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ie)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itauq)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itaup)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9))
                      (setf ierr var-10))
                    (dlacpy "L" n n vt ldvt
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (ir)
                                           ((1 *))
                                           work-%offset%)
                     ldwrkr)
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9)
                        (dorgbr "Q" n n n
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ir)
                                               ((1 *))
                                               work-%offset%)
                         ldwrkr
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itauq)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8))
                      (setf ierr var-9))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9)
                        (dorgbr "P" n n n vt ldvt
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itaup)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8))
                      (setf ierr var-9))
                    (setf iwork (f2cl-lib:int-add ie n))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                        (dbdsqr "U" n n n 0 s
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ie)
                                               ((1 *))
                                               work-%offset%)
                         vt ldvt
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ir)
                                               ((1 *))
                                               work-%offset%)
                         ldwrkr dum 1
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         info)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9 var-10 var-11
                                       var-12 var-13))
                      (setf info var-14))
                    (setf iu (f2cl-lib:int-add ie n))
                    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i ldwrku))
                                  ((> i m) nil)
                      (tagbody
                        (setf chunk
                                (min
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-sub m i)
                                                        1))
                                 (the f2cl-lib:integer4 ldwrku)))
                        (dgemm "N" "N" chunk n n one
                         (f2cl-lib:array-slice a-%data%
                                               double-float
                                               (i 1)
                                               ((1 lda) (1 *))
                                               a-%offset%)
                         lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ir)
                                               ((1 *))
                                               work-%offset%)
                         ldwrkr zero
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iu)
                                               ((1 *))
                                               work-%offset%)
                         ldwrku)
                        (dlacpy "F" chunk n
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iu)
                                               ((1 *))
                                               work-%offset%)
                         ldwrku
                         (f2cl-lib:array-slice a-%data%
                                               double-float
                                               (i 1)
                                               ((1 lda) (1 *))
                                               a-%offset%)
                         lda)
                       label20)))
                   (t
                    (setf itau 1)
                    (setf iwork (f2cl-lib:int-add itau n))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                        (dgeqrf m n a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itau)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6))
                      (setf ierr var-7))
                    (dlacpy "U" n n a lda vt ldvt)
                    (dlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1)
                     zero zero
                     (f2cl-lib:array-slice vt-%data%
                                           double-float
                                           (2 1)
                                           ((1 ldvt) (1 *))
                                           vt-%offset%)
                     ldvt)
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8)
                        (dorgqr m n n a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itau)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7))
                      (setf ierr var-8))
                    (setf ie itau)
                    (setf itauq (f2cl-lib:int-add ie n))
                    (setf itaup (f2cl-lib:int-add itauq n))
                    (setf iwork (f2cl-lib:int-add itaup n))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10)
                        (dgebrd n n vt ldvt s
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ie)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itauq)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itaup)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9))
                      (setf ierr var-10))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13)
                        (dormbr "Q" "R" "N" m n n vt ldvt
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itauq)
                                               ((1 *))
                                               work-%offset%)
                         a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9 var-10 var-11
                                       var-12))
                      (setf ierr var-13))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9)
                        (dorgbr "P" n n n vt ldvt
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itaup)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8))
                      (setf ierr var-9))
                    (setf iwork (f2cl-lib:int-add ie n))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                        (dbdsqr "U" n n m 0 s
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ie)
                                               ((1 *))
                                               work-%offset%)
                         vt ldvt a lda dum 1
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         info)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9 var-10 var-11
                                       var-12 var-13))
                      (setf info var-14)))))
                (wntus
                 (cond
                   (wntvn
                    (cond
                      ((>= lwork
                           (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                             (max
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-mul 4 n))
                                              (the f2cl-lib:integer4 bdspac))))
                       (setf ir 1)
                       (cond
                         ((>= lwork
                              (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul lda n)))
                          (setf ldwrkr lda))
                         (t
                          (setf ldwrkr n)))
                       (setf itau
                               (f2cl-lib:int-add ir
                                                 (f2cl-lib:int-mul ldwrkr n)))
                       (setf iwork (f2cl-lib:int-add itau n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgeqrf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "U" n n a lda
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (ir)
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr)
                       (dlaset "L" (f2cl-lib:int-sub n 1)
                        (f2cl-lib:int-sub n 1) zero zero
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              ((+ ir 1))
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorgqr m n n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie n))
                       (setf itaup (f2cl-lib:int-add itauq n))
                       (setf iwork (f2cl-lib:int-add itaup n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd n n
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "Q" n n n
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" n 0 n 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14))
                       (dgemm "N" "N" m n n one a lda
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (ir)
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr zero u ldu))
                      (t
                       (setf itau 1)
                       (setf iwork (f2cl-lib:int-add itau n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgeqrf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "L" m n a lda u ldu)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorgqr m n n u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie n))
                       (setf itaup (f2cl-lib:int-add itauq n))
                       (setf iwork (f2cl-lib:int-add itaup n))
                       (dlaset "L" (f2cl-lib:int-sub n 1)
                        (f2cl-lib:int-sub n 1) zero zero
                        (f2cl-lib:array-slice a-%data%
                                              double-float
                                              (2 1)
                                              ((1 lda) (1 *))
                                              a-%offset%)
                        lda)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd n n a lda s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13)
                           (dormbr "Q" "R" "N" m n n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12))
                         (setf ierr var-13))
                       (setf iwork (f2cl-lib:int-add ie n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" n 0 m 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            dum 1 u ldu dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14)))))
                   (wntvo
                    (cond
                      ((>= lwork
                           (f2cl-lib:int-add (f2cl-lib:int-mul 2 n n)
                                             (max
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-mul 4 n))
                                              (the f2cl-lib:integer4 bdspac))))
                       (setf iu 1)
                       (cond
                         ((>= lwork
                              (f2cl-lib:int-add wrkbl
                                                (f2cl-lib:int-mul 2 lda n)))
                          (setf ldwrku lda)
                          (setf ir
                                  (f2cl-lib:int-add iu
                                                    (f2cl-lib:int-mul ldwrku
                                                                      n)))
                          (setf ldwrkr lda))
                         ((>= lwork
                              (f2cl-lib:int-add wrkbl
                                                (f2cl-lib:int-mul
                                                 (f2cl-lib:int-add lda n)
                                                 n)))
                          (setf ldwrku lda)
                          (setf ir
                                  (f2cl-lib:int-add iu
                                                    (f2cl-lib:int-mul ldwrku
                                                                      n)))
                          (setf ldwrkr n))
                         (t
                          (setf ldwrku n)
                          (setf ir
                                  (f2cl-lib:int-add iu
                                                    (f2cl-lib:int-mul ldwrku
                                                                      n)))
                          (setf ldwrkr n)))
                       (setf itau
                               (f2cl-lib:int-add ir
                                                 (f2cl-lib:int-mul ldwrkr n)))
                       (setf iwork (f2cl-lib:int-add itau n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgeqrf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "U" n n a lda
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku)
                       (dlaset "L" (f2cl-lib:int-sub n 1)
                        (f2cl-lib:int-sub n 1) zero zero
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              ((+ iu 1))
                                              ((1 *))
                                              work-%offset%)
                        ldwrku)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorgqr m n n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie n))
                       (setf itaup (f2cl-lib:int-add itauq n))
                       (setf iwork (f2cl-lib:int-add itaup n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd n n
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (dlacpy "U" n n
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (ir)
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "Q" n n n
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "P" n n n
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" n n n 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14))
                       (dgemm "N" "N" m n n one a lda
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku zero u ldu)
                       (dlacpy "F" n n
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (ir)
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr a lda))
                      (t
                       (setf itau 1)
                       (setf iwork (f2cl-lib:int-add itau n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgeqrf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "L" m n a lda u ldu)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorgqr m n n u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie n))
                       (setf itaup (f2cl-lib:int-add itauq n))
                       (setf iwork (f2cl-lib:int-add itaup n))
                       (dlaset "L" (f2cl-lib:int-sub n 1)
                        (f2cl-lib:int-sub n 1) zero zero
                        (f2cl-lib:array-slice a-%data%
                                              double-float
                                              (2 1)
                                              ((1 lda) (1 *))
                                              a-%offset%)
                        lda)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd n n a lda s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13)
                           (dormbr "Q" "R" "N" m n n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12))
                         (setf ierr var-13))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "P" n n n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" n n m 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            a lda u ldu dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14)))))
                   (wntvas
                    (cond
                      ((>= lwork
                           (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                             (max
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-mul 4 n))
                                              (the f2cl-lib:integer4 bdspac))))
                       (setf iu 1)
                       (cond
                         ((>= lwork
                              (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul lda n)))
                          (setf ldwrku lda))
                         (t
                          (setf ldwrku n)))
                       (setf itau
                               (f2cl-lib:int-add iu
                                                 (f2cl-lib:int-mul ldwrku n)))
                       (setf iwork (f2cl-lib:int-add itau n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgeqrf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "U" n n a lda
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku)
                       (dlaset "L" (f2cl-lib:int-sub n 1)
                        (f2cl-lib:int-sub n 1) zero zero
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              ((+ iu 1))
                                              ((1 *))
                                              work-%offset%)
                        ldwrku)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorgqr m n n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie n))
                       (setf itaup (f2cl-lib:int-add itauq n))
                       (setf iwork (f2cl-lib:int-add itaup n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd n n
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (dlacpy "U" n n
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku vt ldvt)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "Q" n n n
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "P" n n n vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" n n n 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14))
                       (dgemm "N" "N" m n n one a lda
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku zero u ldu))
                      (t
                       (setf itau 1)
                       (setf iwork (f2cl-lib:int-add itau n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgeqrf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "L" m n a lda u ldu)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorgqr m n n u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (dlacpy "U" n n a lda vt ldvt)
                       (dlaset "L" (f2cl-lib:int-sub n 1)
                        (f2cl-lib:int-sub n 1) zero zero
                        (f2cl-lib:array-slice vt-%data%
                                              double-float
                                              (2 1)
                                              ((1 ldvt) (1 *))
                                              vt-%offset%)
                        ldvt)
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie n))
                       (setf itaup (f2cl-lib:int-add itauq n))
                       (setf iwork (f2cl-lib:int-add itaup n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd n n vt ldvt s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13)
                           (dormbr "Q" "R" "N" m n n vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12))
                         (setf ierr var-13))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "P" n n n vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" n n m 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            vt ldvt u ldu dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14)))))))
                (wntua
                 (cond
                   (wntvn
                    (cond
                      ((>= lwork
                           (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                             (max
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-add n m))
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-mul 4 n))
                                              (the f2cl-lib:integer4 bdspac))))
                       (setf ir 1)
                       (cond
                         ((>= lwork
                              (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul lda n)))
                          (setf ldwrkr lda))
                         (t
                          (setf ldwrkr n)))
                       (setf itau
                               (f2cl-lib:int-add ir
                                                 (f2cl-lib:int-mul ldwrkr n)))
                       (setf iwork (f2cl-lib:int-add itau n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgeqrf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "L" m n a lda u ldu)
                       (dlacpy "U" n n a lda
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (ir)
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr)
                       (dlaset "L" (f2cl-lib:int-sub n 1)
                        (f2cl-lib:int-sub n 1) zero zero
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              ((+ ir 1))
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorgqr m m n u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie n))
                       (setf itaup (f2cl-lib:int-add itauq n))
                       (setf iwork (f2cl-lib:int-add itaup n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd n n
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "Q" n n n
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" n 0 n 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14))
                       (dgemm "N" "N" m n n one u ldu
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (ir)
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr zero a lda)
                       (dlacpy "F" m n a lda u ldu))
                      (t
                       (setf itau 1)
                       (setf iwork (f2cl-lib:int-add itau n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgeqrf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "L" m n a lda u ldu)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorgqr m m n u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie n))
                       (setf itaup (f2cl-lib:int-add itauq n))
                       (setf iwork (f2cl-lib:int-add itaup n))
                       (dlaset "L" (f2cl-lib:int-sub n 1)
                        (f2cl-lib:int-sub n 1) zero zero
                        (f2cl-lib:array-slice a-%data%
                                              double-float
                                              (2 1)
                                              ((1 lda) (1 *))
                                              a-%offset%)
                        lda)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd n n a lda s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13)
                           (dormbr "Q" "R" "N" m n n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12))
                         (setf ierr var-13))
                       (setf iwork (f2cl-lib:int-add ie n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" n 0 m 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            dum 1 u ldu dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14)))))
                   (wntvo
                    (cond
                      ((>= lwork
                           (f2cl-lib:int-add (f2cl-lib:int-mul 2 n n)
                                             (max
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-add n m))
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-mul 4 n))
                                              (the f2cl-lib:integer4 bdspac))))
                       (setf iu 1)
                       (cond
                         ((>= lwork
                              (f2cl-lib:int-add wrkbl
                                                (f2cl-lib:int-mul 2 lda n)))
                          (setf ldwrku lda)
                          (setf ir
                                  (f2cl-lib:int-add iu
                                                    (f2cl-lib:int-mul ldwrku
                                                                      n)))
                          (setf ldwrkr lda))
                         ((>= lwork
                              (f2cl-lib:int-add wrkbl
                                                (f2cl-lib:int-mul
                                                 (f2cl-lib:int-add lda n)
                                                 n)))
                          (setf ldwrku lda)
                          (setf ir
                                  (f2cl-lib:int-add iu
                                                    (f2cl-lib:int-mul ldwrku
                                                                      n)))
                          (setf ldwrkr n))
                         (t
                          (setf ldwrku n)
                          (setf ir
                                  (f2cl-lib:int-add iu
                                                    (f2cl-lib:int-mul ldwrku
                                                                      n)))
                          (setf ldwrkr n)))
                       (setf itau
                               (f2cl-lib:int-add ir
                                                 (f2cl-lib:int-mul ldwrkr n)))
                       (setf iwork (f2cl-lib:int-add itau n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgeqrf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "L" m n a lda u ldu)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorgqr m m n u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (dlacpy "U" n n a lda
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku)
                       (dlaset "L" (f2cl-lib:int-sub n 1)
                        (f2cl-lib:int-sub n 1) zero zero
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              ((+ iu 1))
                                              ((1 *))
                                              work-%offset%)
                        ldwrku)
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie n))
                       (setf itaup (f2cl-lib:int-add itauq n))
                       (setf iwork (f2cl-lib:int-add itaup n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd n n
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (dlacpy "U" n n
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (ir)
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "Q" n n n
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "P" n n n
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" n n n 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14))
                       (dgemm "N" "N" m n n one u ldu
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku zero a lda)
                       (dlacpy "F" m n a lda u ldu)
                       (dlacpy "F" n n
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (ir)
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr a lda))
                      (t
                       (setf itau 1)
                       (setf iwork (f2cl-lib:int-add itau n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgeqrf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "L" m n a lda u ldu)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorgqr m m n u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie n))
                       (setf itaup (f2cl-lib:int-add itauq n))
                       (setf iwork (f2cl-lib:int-add itaup n))
                       (dlaset "L" (f2cl-lib:int-sub n 1)
                        (f2cl-lib:int-sub n 1) zero zero
                        (f2cl-lib:array-slice a-%data%
                                              double-float
                                              (2 1)
                                              ((1 lda) (1 *))
                                              a-%offset%)
                        lda)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd n n a lda s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13)
                           (dormbr "Q" "R" "N" m n n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12))
                         (setf ierr var-13))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "P" n n n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" n n m 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            a lda u ldu dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14)))))
                   (wntvas
                    (cond
                      ((>= lwork
                           (f2cl-lib:int-add (f2cl-lib:int-mul n n)
                                             (max
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-add n m))
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-mul 4 n))
                                              (the f2cl-lib:integer4 bdspac))))
                       (setf iu 1)
                       (cond
                         ((>= lwork
                              (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul lda n)))
                          (setf ldwrku lda))
                         (t
                          (setf ldwrku n)))
                       (setf itau
                               (f2cl-lib:int-add iu
                                                 (f2cl-lib:int-mul ldwrku n)))
                       (setf iwork (f2cl-lib:int-add itau n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgeqrf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "L" m n a lda u ldu)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorgqr m m n u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (dlacpy "U" n n a lda
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku)
                       (dlaset "L" (f2cl-lib:int-sub n 1)
                        (f2cl-lib:int-sub n 1) zero zero
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              ((+ iu 1))
                                              ((1 *))
                                              work-%offset%)
                        ldwrku)
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie n))
                       (setf itaup (f2cl-lib:int-add itauq n))
                       (setf iwork (f2cl-lib:int-add itaup n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd n n
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (dlacpy "U" n n
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku vt ldvt)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "Q" n n n
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "P" n n n vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" n n n 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14))
                       (dgemm "N" "N" m n n one u ldu
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku zero a lda)
                       (dlacpy "F" m n a lda u ldu))
                      (t
                       (setf itau 1)
                       (setf iwork (f2cl-lib:int-add itau n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgeqrf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "L" m n a lda u ldu)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorgqr m m n u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (dlacpy "U" n n a lda vt ldvt)
                       (dlaset "L" (f2cl-lib:int-sub n 1)
                        (f2cl-lib:int-sub n 1) zero zero
                        (f2cl-lib:array-slice vt-%data%
                                              double-float
                                              (2 1)
                                              ((1 ldvt) (1 *))
                                              vt-%offset%)
                        ldvt)
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie n))
                       (setf itaup (f2cl-lib:int-add itauq n))
                       (setf iwork (f2cl-lib:int-add itaup n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd n n vt ldvt s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13)
                           (dormbr "Q" "R" "N" m n n vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12))
                         (setf ierr var-13))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "P" n n n vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie n))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" n n m 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            vt ldvt u ldu dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14)))))))))
             (t
              (setf ie 1)
              (setf itauq (f2cl-lib:int-add ie n))
              (setf itaup (f2cl-lib:int-add itauq n))
              (setf iwork (f2cl-lib:int-add itaup n))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10)
                  (dgebrd m n a lda s
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (ie)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (itauq)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (itaup)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (iwork)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7 var-8 var-9))
                (setf ierr var-10))
              (cond
                (wntuas
                 (dlacpy "L" m n a lda u ldu)
                 (if wntus (setf ncu n))
                 (if wntua (setf ncu m))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9)
                     (dorgbr "Q" m ncu n u ldu
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itauq)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8))
                   (setf ierr var-9))))
              (cond
                (wntvas
                 (dlacpy "U" n n a lda vt ldvt)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9)
                     (dorgbr "P" n n n vt ldvt
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itaup)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8))
                   (setf ierr var-9))))
              (cond
                (wntuo
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9)
                     (dorgbr "Q" m n n a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itauq)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8))
                   (setf ierr var-9))))
              (cond
                (wntvo
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9)
                     (dorgbr "P" n n n a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itaup)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8))
                   (setf ierr var-9))))
              (setf iwork (f2cl-lib:int-add ie n))
              (if (or wntuas wntuo) (setf nru m))
              (if wntun (setf nru 0))
              (if (or wntvas wntvo) (setf ncvt n))
              (if wntvn (setf ncvt 0))
              (cond
                ((and (not wntuo) (not wntvo))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13 var-14)
                     (dbdsqr "U" n ncvt nru 0 s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      vt ldvt u ldu dum 1
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12
                                    var-13))
                   (setf info var-14)))
                ((and (not wntuo) wntvo)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13 var-14)
                     (dbdsqr "U" n ncvt nru 0 s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      a lda u ldu dum 1
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12
                                    var-13))
                   (setf info var-14)))
                (t
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13 var-14)
                     (dbdsqr "U" n ncvt nru 0 s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      vt ldvt a lda dum 1
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12
                                    var-13))
                   (setf info var-14)))))))
          (t
           (cond
             ((>= n mnthr)
              (cond
                (wntvn
                 (setf itau 1)
                 (setf iwork (f2cl-lib:int-add itau m))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                     (dgelqf m n a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itau)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                   (setf ierr var-7))
                 (dlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1) zero
                  zero
                  (f2cl-lib:array-slice a-%data%
                                        double-float
                                        (1 2)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                  lda)
                 (setf ie 1)
                 (setf itauq (f2cl-lib:int-add ie m))
                 (setf itaup (f2cl-lib:int-add itauq m))
                 (setf iwork (f2cl-lib:int-add itaup m))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10)
                     (dgebrd m m a lda s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itauq)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itaup)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9))
                   (setf ierr var-10))
                 (cond
                   ((or wntuo wntuas)
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9)
                        (dorgbr "Q" m m m a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itauq)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8))
                      (setf ierr var-9))))
                 (setf iwork (f2cl-lib:int-add ie m))
                 (setf nru 0)
                 (if (or wntuo wntuas) (setf nru m))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13 var-14)
                     (dbdsqr "U" m 0 nru 0 s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      dum 1 a lda dum 1
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12
                                    var-13))
                   (setf info var-14))
                 (if wntuas (dlacpy "F" m m a lda u ldu)))
                ((and wntvo wntun)
                 (cond
                   ((>= lwork
                        (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                          (max
                                           (the f2cl-lib:integer4
                                                (f2cl-lib:int-mul 4 m))
                                           (the f2cl-lib:integer4 bdspac))))
                    (setf ir 1)
                    (cond
                      ((>= lwork
                           (f2cl-lib:int-add
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add
                                       (f2cl-lib:int-mul lda n)
                                       m)))
                            (f2cl-lib:int-mul lda m)))
                       (setf ldwrku lda)
                       (setf chunk n)
                       (setf ldwrkr lda))
                      ((>= lwork
                           (f2cl-lib:int-add
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add
                                       (f2cl-lib:int-mul lda n)
                                       m)))
                            (f2cl-lib:int-mul m m)))
                       (setf ldwrku lda)
                       (setf chunk n)
                       (setf ldwrkr m))
                      (t
                       (setf ldwrku m)
                       (setf chunk
                               (the f2cl-lib:integer4
                                    (truncate (- lwork (* m m) m) m)))
                       (setf ldwrkr m)))
                    (setf itau
                            (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr m)))
                    (setf iwork (f2cl-lib:int-add itau m))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                        (dgelqf m n a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itau)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6))
                      (setf ierr var-7))
                    (dlacpy "L" m m a lda
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (ir)
                                           ((1 *))
                                           work-%offset%)
                     ldwrkr)
                    (dlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                     zero zero
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           ((+ ir ldwrkr))
                                           ((1 *))
                                           work-%offset%)
                     ldwrkr)
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8)
                        (dorglq m n m a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itau)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7))
                      (setf ierr var-8))
                    (setf ie itau)
                    (setf itauq (f2cl-lib:int-add ie m))
                    (setf itaup (f2cl-lib:int-add itauq m))
                    (setf iwork (f2cl-lib:int-add itaup m))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10)
                        (dgebrd m m
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ir)
                                               ((1 *))
                                               work-%offset%)
                         ldwrkr s
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ie)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itauq)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itaup)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9))
                      (setf ierr var-10))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9)
                        (dorgbr "P" m m m
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ir)
                                               ((1 *))
                                               work-%offset%)
                         ldwrkr
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itaup)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8))
                      (setf ierr var-9))
                    (setf iwork (f2cl-lib:int-add ie m))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                        (dbdsqr "U" m m 0 0 s
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ie)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ir)
                                               ((1 *))
                                               work-%offset%)
                         ldwrkr dum 1 dum 1
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         info)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9 var-10 var-11
                                       var-12 var-13))
                      (setf info var-14))
                    (setf iu (f2cl-lib:int-add ie m))
                    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i chunk))
                                  ((> i n) nil)
                      (tagbody
                        (setf blk
                                (min
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-sub n i)
                                                        1))
                                 (the f2cl-lib:integer4 chunk)))
                        (dgemm "N" "N" m blk m one
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ir)
                                               ((1 *))
                                               work-%offset%)
                         ldwrkr
                         (f2cl-lib:array-slice a-%data%
                                               double-float
                                               (1 i)
                                               ((1 lda) (1 *))
                                               a-%offset%)
                         lda zero
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iu)
                                               ((1 *))
                                               work-%offset%)
                         ldwrku)
                        (dlacpy "F" m blk
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iu)
                                               ((1 *))
                                               work-%offset%)
                         ldwrku
                         (f2cl-lib:array-slice a-%data%
                                               double-float
                                               (1 i)
                                               ((1 lda) (1 *))
                                               a-%offset%)
                         lda)
                       label30)))
                   (t
                    (setf ie 1)
                    (setf itauq (f2cl-lib:int-add ie m))
                    (setf itaup (f2cl-lib:int-add itauq m))
                    (setf iwork (f2cl-lib:int-add itaup m))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10)
                        (dgebrd m n a lda s
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ie)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itauq)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itaup)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9))
                      (setf ierr var-10))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9)
                        (dorgbr "P" m n m a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itaup)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8))
                      (setf ierr var-9))
                    (setf iwork (f2cl-lib:int-add ie m))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                        (dbdsqr "L" m n 0 0 s
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ie)
                                               ((1 *))
                                               work-%offset%)
                         a lda dum 1 dum 1
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         info)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9 var-10 var-11
                                       var-12 var-13))
                      (setf info var-14)))))
                ((and wntvo wntuas)
                 (cond
                   ((>= lwork
                        (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                          (max
                                           (the f2cl-lib:integer4
                                                (f2cl-lib:int-mul 4 m))
                                           (the f2cl-lib:integer4 bdspac))))
                    (setf ir 1)
                    (cond
                      ((>= lwork
                           (f2cl-lib:int-add
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add
                                       (f2cl-lib:int-mul lda n)
                                       m)))
                            (f2cl-lib:int-mul lda m)))
                       (setf ldwrku lda)
                       (setf chunk n)
                       (setf ldwrkr lda))
                      ((>= lwork
                           (f2cl-lib:int-add
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add
                                       (f2cl-lib:int-mul lda n)
                                       m)))
                            (f2cl-lib:int-mul m m)))
                       (setf ldwrku lda)
                       (setf chunk n)
                       (setf ldwrkr m))
                      (t
                       (setf ldwrku m)
                       (setf chunk
                               (the f2cl-lib:integer4
                                    (truncate (- lwork (* m m) m) m)))
                       (setf ldwrkr m)))
                    (setf itau
                            (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr m)))
                    (setf iwork (f2cl-lib:int-add itau m))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                        (dgelqf m n a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itau)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6))
                      (setf ierr var-7))
                    (dlacpy "L" m m a lda u ldu)
                    (dlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                     zero zero
                     (f2cl-lib:array-slice u-%data%
                                           double-float
                                           (1 2)
                                           ((1 ldu) (1 *))
                                           u-%offset%)
                     ldu)
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8)
                        (dorglq m n m a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itau)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7))
                      (setf ierr var-8))
                    (setf ie itau)
                    (setf itauq (f2cl-lib:int-add ie m))
                    (setf itaup (f2cl-lib:int-add itauq m))
                    (setf iwork (f2cl-lib:int-add itaup m))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10)
                        (dgebrd m m u ldu s
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ie)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itauq)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itaup)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9))
                      (setf ierr var-10))
                    (dlacpy "U" m m u ldu
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (ir)
                                           ((1 *))
                                           work-%offset%)
                     ldwrkr)
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9)
                        (dorgbr "P" m m m
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ir)
                                               ((1 *))
                                               work-%offset%)
                         ldwrkr
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itaup)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8))
                      (setf ierr var-9))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9)
                        (dorgbr "Q" m m m u ldu
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itauq)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8))
                      (setf ierr var-9))
                    (setf iwork (f2cl-lib:int-add ie m))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                        (dbdsqr "U" m m m 0 s
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ie)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ir)
                                               ((1 *))
                                               work-%offset%)
                         ldwrkr u ldu dum 1
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         info)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9 var-10 var-11
                                       var-12 var-13))
                      (setf info var-14))
                    (setf iu (f2cl-lib:int-add ie m))
                    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i chunk))
                                  ((> i n) nil)
                      (tagbody
                        (setf blk
                                (min
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-sub n i)
                                                        1))
                                 (the f2cl-lib:integer4 chunk)))
                        (dgemm "N" "N" m blk m one
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ir)
                                               ((1 *))
                                               work-%offset%)
                         ldwrkr
                         (f2cl-lib:array-slice a-%data%
                                               double-float
                                               (1 i)
                                               ((1 lda) (1 *))
                                               a-%offset%)
                         lda zero
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iu)
                                               ((1 *))
                                               work-%offset%)
                         ldwrku)
                        (dlacpy "F" m blk
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iu)
                                               ((1 *))
                                               work-%offset%)
                         ldwrku
                         (f2cl-lib:array-slice a-%data%
                                               double-float
                                               (1 i)
                                               ((1 lda) (1 *))
                                               a-%offset%)
                         lda)
                       label40)))
                   (t
                    (setf itau 1)
                    (setf iwork (f2cl-lib:int-add itau m))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                        (dgelqf m n a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itau)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6))
                      (setf ierr var-7))
                    (dlacpy "L" m m a lda u ldu)
                    (dlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1)
                     zero zero
                     (f2cl-lib:array-slice u-%data%
                                           double-float
                                           (1 2)
                                           ((1 ldu) (1 *))
                                           u-%offset%)
                     ldu)
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8)
                        (dorglq m n m a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itau)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7))
                      (setf ierr var-8))
                    (setf ie itau)
                    (setf itauq (f2cl-lib:int-add ie m))
                    (setf itaup (f2cl-lib:int-add itauq m))
                    (setf iwork (f2cl-lib:int-add itaup m))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10)
                        (dgebrd m m u ldu s
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ie)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itauq)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itaup)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9))
                      (setf ierr var-10))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13)
                        (dormbr "P" "L" "T" m n m u ldu
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itaup)
                                               ((1 *))
                                               work-%offset%)
                         a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9 var-10 var-11
                                       var-12))
                      (setf ierr var-13))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9)
                        (dorgbr "Q" m m m u ldu
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itauq)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8))
                      (setf ierr var-9))
                    (setf iwork (f2cl-lib:int-add ie m))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                        (dbdsqr "U" m n m 0 s
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ie)
                                               ((1 *))
                                               work-%offset%)
                         a lda u ldu dum 1
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iwork)
                                               ((1 *))
                                               work-%offset%)
                         info)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9 var-10 var-11
                                       var-12 var-13))
                      (setf info var-14)))))
                (wntvs
                 (cond
                   (wntun
                    (cond
                      ((>= lwork
                           (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                             (max
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-mul 4 m))
                                              (the f2cl-lib:integer4 bdspac))))
                       (setf ir 1)
                       (cond
                         ((>= lwork
                              (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul lda m)))
                          (setf ldwrkr lda))
                         (t
                          (setf ldwrkr m)))
                       (setf itau
                               (f2cl-lib:int-add ir
                                                 (f2cl-lib:int-mul ldwrkr m)))
                       (setf iwork (f2cl-lib:int-add itau m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgelqf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "L" m m a lda
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (ir)
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr)
                       (dlaset "U" (f2cl-lib:int-sub m 1)
                        (f2cl-lib:int-sub m 1) zero zero
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              ((+ ir ldwrkr))
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorglq m n m a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie m))
                       (setf itaup (f2cl-lib:int-add itauq m))
                       (setf iwork (f2cl-lib:int-add itaup m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd m m
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "P" m m m
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" m m 0 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr dum 1 dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14))
                       (dgemm "N" "N" m n m one
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (ir)
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr a lda zero vt ldvt))
                      (t
                       (setf itau 1)
                       (setf iwork (f2cl-lib:int-add itau m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgelqf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "U" m n a lda vt ldvt)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorglq m n m vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie m))
                       (setf itaup (f2cl-lib:int-add itauq m))
                       (setf iwork (f2cl-lib:int-add itaup m))
                       (dlaset "U" (f2cl-lib:int-sub m 1)
                        (f2cl-lib:int-sub m 1) zero zero
                        (f2cl-lib:array-slice a-%data%
                                              double-float
                                              (1 2)
                                              ((1 lda) (1 *))
                                              a-%offset%)
                        lda)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd m m a lda s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13)
                           (dormbr "P" "L" "T" m n m a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12))
                         (setf ierr var-13))
                       (setf iwork (f2cl-lib:int-add ie m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" m n 0 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            vt ldvt dum 1 dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14)))))
                   (wntuo
                    (cond
                      ((>= lwork
                           (f2cl-lib:int-add (f2cl-lib:int-mul 2 m m)
                                             (max
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-mul 4 m))
                                              (the f2cl-lib:integer4 bdspac))))
                       (setf iu 1)
                       (cond
                         ((>= lwork
                              (f2cl-lib:int-add wrkbl
                                                (f2cl-lib:int-mul 2 lda m)))
                          (setf ldwrku lda)
                          (setf ir
                                  (f2cl-lib:int-add iu
                                                    (f2cl-lib:int-mul ldwrku
                                                                      m)))
                          (setf ldwrkr lda))
                         ((>= lwork
                              (f2cl-lib:int-add wrkbl
                                                (f2cl-lib:int-mul
                                                 (f2cl-lib:int-add lda m)
                                                 m)))
                          (setf ldwrku lda)
                          (setf ir
                                  (f2cl-lib:int-add iu
                                                    (f2cl-lib:int-mul ldwrku
                                                                      m)))
                          (setf ldwrkr m))
                         (t
                          (setf ldwrku m)
                          (setf ir
                                  (f2cl-lib:int-add iu
                                                    (f2cl-lib:int-mul ldwrku
                                                                      m)))
                          (setf ldwrkr m)))
                       (setf itau
                               (f2cl-lib:int-add ir
                                                 (f2cl-lib:int-mul ldwrkr m)))
                       (setf iwork (f2cl-lib:int-add itau m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgelqf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "L" m m a lda
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku)
                       (dlaset "U" (f2cl-lib:int-sub m 1)
                        (f2cl-lib:int-sub m 1) zero zero
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              ((+ iu ldwrku))
                                              ((1 *))
                                              work-%offset%)
                        ldwrku)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorglq m n m a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie m))
                       (setf itaup (f2cl-lib:int-add itauq m))
                       (setf iwork (f2cl-lib:int-add itaup m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd m m
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (dlacpy "L" m m
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (ir)
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "P" m m m
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "Q" m m m
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" m m m 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14))
                       (dgemm "N" "N" m n m one
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku a lda zero vt ldvt)
                       (dlacpy "F" m m
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (ir)
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr a lda))
                      (t
                       (setf itau 1)
                       (setf iwork (f2cl-lib:int-add itau m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgelqf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "U" m n a lda vt ldvt)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorglq m n m vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie m))
                       (setf itaup (f2cl-lib:int-add itauq m))
                       (setf iwork (f2cl-lib:int-add itaup m))
                       (dlaset "U" (f2cl-lib:int-sub m 1)
                        (f2cl-lib:int-sub m 1) zero zero
                        (f2cl-lib:array-slice a-%data%
                                              double-float
                                              (1 2)
                                              ((1 lda) (1 *))
                                              a-%offset%)
                        lda)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd m m a lda s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13)
                           (dormbr "P" "L" "T" m n m a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12))
                         (setf ierr var-13))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "Q" m m m a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" m n m 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            vt ldvt a lda dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14)))))
                   (wntuas
                    (cond
                      ((>= lwork
                           (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                             (max
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-mul 4 m))
                                              (the f2cl-lib:integer4 bdspac))))
                       (setf iu 1)
                       (cond
                         ((>= lwork
                              (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul lda m)))
                          (setf ldwrku lda))
                         (t
                          (setf ldwrku m)))
                       (setf itau
                               (f2cl-lib:int-add iu
                                                 (f2cl-lib:int-mul ldwrku m)))
                       (setf iwork (f2cl-lib:int-add itau m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgelqf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "L" m m a lda
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku)
                       (dlaset "U" (f2cl-lib:int-sub m 1)
                        (f2cl-lib:int-sub m 1) zero zero
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              ((+ iu ldwrku))
                                              ((1 *))
                                              work-%offset%)
                        ldwrku)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorglq m n m a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie m))
                       (setf itaup (f2cl-lib:int-add itauq m))
                       (setf iwork (f2cl-lib:int-add itaup m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd m m
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (dlacpy "L" m m
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku u ldu)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "P" m m m
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "Q" m m m u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" m m m 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku u ldu dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14))
                       (dgemm "N" "N" m n m one
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku a lda zero vt ldvt))
                      (t
                       (setf itau 1)
                       (setf iwork (f2cl-lib:int-add itau m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgelqf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "U" m n a lda vt ldvt)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorglq m n m vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (dlacpy "L" m m a lda u ldu)
                       (dlaset "U" (f2cl-lib:int-sub m 1)
                        (f2cl-lib:int-sub m 1) zero zero
                        (f2cl-lib:array-slice u-%data%
                                              double-float
                                              (1 2)
                                              ((1 ldu) (1 *))
                                              u-%offset%)
                        ldu)
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie m))
                       (setf itaup (f2cl-lib:int-add itauq m))
                       (setf iwork (f2cl-lib:int-add itaup m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd m m u ldu s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13)
                           (dormbr "P" "L" "T" m n m u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12))
                         (setf ierr var-13))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "Q" m m m u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" m n m 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            vt ldvt u ldu dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14)))))))
                (wntva
                 (cond
                   (wntun
                    (cond
                      ((>= lwork
                           (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                             (max
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-add n m))
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-mul 4 m))
                                              (the f2cl-lib:integer4 bdspac))))
                       (setf ir 1)
                       (cond
                         ((>= lwork
                              (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul lda m)))
                          (setf ldwrkr lda))
                         (t
                          (setf ldwrkr m)))
                       (setf itau
                               (f2cl-lib:int-add ir
                                                 (f2cl-lib:int-mul ldwrkr m)))
                       (setf iwork (f2cl-lib:int-add itau m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgelqf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "U" m n a lda vt ldvt)
                       (dlacpy "L" m m a lda
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (ir)
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr)
                       (dlaset "U" (f2cl-lib:int-sub m 1)
                        (f2cl-lib:int-sub m 1) zero zero
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              ((+ ir ldwrkr))
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorglq n n m vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie m))
                       (setf itaup (f2cl-lib:int-add itauq m))
                       (setf iwork (f2cl-lib:int-add itaup m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd m m
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "P" m m m
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" m m 0 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr dum 1 dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14))
                       (dgemm "N" "N" m n m one
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (ir)
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr vt ldvt zero a lda)
                       (dlacpy "F" m n a lda vt ldvt))
                      (t
                       (setf itau 1)
                       (setf iwork (f2cl-lib:int-add itau m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgelqf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "U" m n a lda vt ldvt)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorglq n n m vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie m))
                       (setf itaup (f2cl-lib:int-add itauq m))
                       (setf iwork (f2cl-lib:int-add itaup m))
                       (dlaset "U" (f2cl-lib:int-sub m 1)
                        (f2cl-lib:int-sub m 1) zero zero
                        (f2cl-lib:array-slice a-%data%
                                              double-float
                                              (1 2)
                                              ((1 lda) (1 *))
                                              a-%offset%)
                        lda)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd m m a lda s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13)
                           (dormbr "P" "L" "T" m n m a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12))
                         (setf ierr var-13))
                       (setf iwork (f2cl-lib:int-add ie m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" m n 0 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            vt ldvt dum 1 dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14)))))
                   (wntuo
                    (cond
                      ((>= lwork
                           (f2cl-lib:int-add (f2cl-lib:int-mul 2 m m)
                                             (max
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-add n m))
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-mul 4 m))
                                              (the f2cl-lib:integer4 bdspac))))
                       (setf iu 1)
                       (cond
                         ((>= lwork
                              (f2cl-lib:int-add wrkbl
                                                (f2cl-lib:int-mul 2 lda m)))
                          (setf ldwrku lda)
                          (setf ir
                                  (f2cl-lib:int-add iu
                                                    (f2cl-lib:int-mul ldwrku
                                                                      m)))
                          (setf ldwrkr lda))
                         ((>= lwork
                              (f2cl-lib:int-add wrkbl
                                                (f2cl-lib:int-mul
                                                 (f2cl-lib:int-add lda m)
                                                 m)))
                          (setf ldwrku lda)
                          (setf ir
                                  (f2cl-lib:int-add iu
                                                    (f2cl-lib:int-mul ldwrku
                                                                      m)))
                          (setf ldwrkr m))
                         (t
                          (setf ldwrku m)
                          (setf ir
                                  (f2cl-lib:int-add iu
                                                    (f2cl-lib:int-mul ldwrku
                                                                      m)))
                          (setf ldwrkr m)))
                       (setf itau
                               (f2cl-lib:int-add ir
                                                 (f2cl-lib:int-mul ldwrkr m)))
                       (setf iwork (f2cl-lib:int-add itau m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgelqf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "U" m n a lda vt ldvt)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorglq n n m vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (dlacpy "L" m m a lda
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku)
                       (dlaset "U" (f2cl-lib:int-sub m 1)
                        (f2cl-lib:int-sub m 1) zero zero
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              ((+ iu ldwrku))
                                              ((1 *))
                                              work-%offset%)
                        ldwrku)
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie m))
                       (setf itaup (f2cl-lib:int-add itauq m))
                       (setf iwork (f2cl-lib:int-add itaup m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd m m
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (dlacpy "L" m m
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (ir)
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "P" m m m
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "Q" m m m
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" m m m 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ir)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrkr dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14))
                       (dgemm "N" "N" m n m one
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku vt ldvt zero a lda)
                       (dlacpy "F" m n a lda vt ldvt)
                       (dlacpy "F" m m
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (ir)
                                              ((1 *))
                                              work-%offset%)
                        ldwrkr a lda))
                      (t
                       (setf itau 1)
                       (setf iwork (f2cl-lib:int-add itau m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgelqf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "U" m n a lda vt ldvt)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorglq n n m vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie m))
                       (setf itaup (f2cl-lib:int-add itauq m))
                       (setf iwork (f2cl-lib:int-add itaup m))
                       (dlaset "U" (f2cl-lib:int-sub m 1)
                        (f2cl-lib:int-sub m 1) zero zero
                        (f2cl-lib:array-slice a-%data%
                                              double-float
                                              (1 2)
                                              ((1 lda) (1 *))
                                              a-%offset%)
                        lda)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd m m a lda s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13)
                           (dormbr "P" "L" "T" m n m a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12))
                         (setf ierr var-13))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "Q" m m m a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" m n m 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            vt ldvt a lda dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14)))))
                   (wntuas
                    (cond
                      ((>= lwork
                           (f2cl-lib:int-add (f2cl-lib:int-mul m m)
                                             (max
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-add n m))
                                              (the f2cl-lib:integer4
                                                   (f2cl-lib:int-mul 4 m))
                                              (the f2cl-lib:integer4 bdspac))))
                       (setf iu 1)
                       (cond
                         ((>= lwork
                              (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul lda m)))
                          (setf ldwrku lda))
                         (t
                          (setf ldwrku m)))
                       (setf itau
                               (f2cl-lib:int-add iu
                                                 (f2cl-lib:int-mul ldwrku m)))
                       (setf iwork (f2cl-lib:int-add itau m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgelqf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "U" m n a lda vt ldvt)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorglq n n m vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (dlacpy "L" m m a lda
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku)
                       (dlaset "U" (f2cl-lib:int-sub m 1)
                        (f2cl-lib:int-sub m 1) zero zero
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              ((+ iu ldwrku))
                                              ((1 *))
                                              work-%offset%)
                        ldwrku)
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie m))
                       (setf itaup (f2cl-lib:int-add itauq m))
                       (setf iwork (f2cl-lib:int-add itaup m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd m m
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (dlacpy "L" m m
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku u ldu)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "P" m m m
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "Q" m m m u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" m m m 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iu)
                                                  ((1 *))
                                                  work-%offset%)
                            ldwrku u ldu dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14))
                       (dgemm "N" "N" m n m one
                        (f2cl-lib:array-slice work-%data%
                                              double-float
                                              (iu)
                                              ((1 *))
                                              work-%offset%)
                        ldwrku vt ldvt zero a lda)
                       (dlacpy "F" m n a lda vt ldvt))
                      (t
                       (setf itau 1)
                       (setf iwork (f2cl-lib:int-add itau m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
                           (dgelqf m n a lda
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6))
                         (setf ierr var-7))
                       (dlacpy "U" m n a lda vt ldvt)
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8)
                           (dorglq n n m vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itau)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7))
                         (setf ierr var-8))
                       (dlacpy "L" m m a lda u ldu)
                       (dlaset "U" (f2cl-lib:int-sub m 1)
                        (f2cl-lib:int-sub m 1) zero zero
                        (f2cl-lib:array-slice u-%data%
                                              double-float
                                              (1 2)
                                              ((1 ldu) (1 *))
                                              u-%offset%)
                        ldu)
                       (setf ie itau)
                       (setf itauq (f2cl-lib:int-add ie m))
                       (setf itaup (f2cl-lib:int-add itauq m))
                       (setf iwork (f2cl-lib:int-add itaup m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10)
                           (dgebrd m m u ldu s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9))
                         (setf ierr var-10))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13)
                           (dormbr "P" "L" "T" m n m u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itaup)
                                                  ((1 *))
                                                  work-%offset%)
                            vt ldvt
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12))
                         (setf ierr var-13))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9)
                           (dorgbr "Q" m m m u ldu
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (itauq)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1)
                            ierr)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8))
                         (setf ierr var-9))
                       (setf iwork (f2cl-lib:int-add ie m))
                       (multiple-value-bind
                             (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                           (dbdsqr "U" m n m 0 s
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (ie)
                                                  ((1 *))
                                                  work-%offset%)
                            vt ldvt u ldu dum 1
                            (f2cl-lib:array-slice work-%data%
                                                  double-float
                                                  (iwork)
                                                  ((1 *))
                                                  work-%offset%)
                            info)
                         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                          var-6 var-7 var-8 var-9 var-10 var-11
                                          var-12 var-13))
                         (setf info var-14)))))))))
             (t
              (setf ie 1)
              (setf itauq (f2cl-lib:int-add ie m))
              (setf itaup (f2cl-lib:int-add itauq m))
              (setf iwork (f2cl-lib:int-add itaup m))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10)
                  (dgebrd m n a lda s
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (ie)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (itauq)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (itaup)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (iwork)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7 var-8 var-9))
                (setf ierr var-10))
              (cond
                (wntuas
                 (dlacpy "L" m m a lda u ldu)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9)
                     (dorgbr "Q" m m n u ldu
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itauq)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8))
                   (setf ierr var-9))))
              (cond
                (wntvas
                 (dlacpy "U" m n a lda vt ldvt)
                 (if wntva (setf nrvt n))
                 (if wntvs (setf nrvt m))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9)
                     (dorgbr "P" nrvt n m vt ldvt
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itaup)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8))
                   (setf ierr var-9))))
              (cond
                (wntuo
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9)
                     (dorgbr "Q" m m n a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itauq)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8))
                   (setf ierr var-9))))
              (cond
                (wntvo
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9)
                     (dorgbr "P" m n m a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itaup)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork iwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8))
                   (setf ierr var-9))))
              (setf iwork (f2cl-lib:int-add ie m))
              (if (or wntuas wntuo) (setf nru m))
              (if wntun (setf nru 0))
              (if (or wntvas wntvo) (setf ncvt n))
              (if wntvn (setf ncvt 0))
              (cond
                ((and (not wntuo) (not wntvo))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13 var-14)
                     (dbdsqr "L" m ncvt nru 0 s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      vt ldvt u ldu dum 1
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12
                                    var-13))
                   (setf info var-14)))
                ((and (not wntuo) wntvo)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13 var-14)
                     (dbdsqr "L" m ncvt nru 0 s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      a lda u ldu dum 1
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12
                                    var-13))
                   (setf info var-14)))
                (t
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13 var-14)
                     (dbdsqr "L" m ncvt nru 0 s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      vt ldvt a lda dum 1
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iwork)
                                            ((1 *))
                                            work-%offset%)
                      info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12
                                    var-13))
                   (setf info var-14))))))))
        (cond
          ((/= info 0)
           (cond
             ((> ie 2)
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i
                                (f2cl-lib:int-add minmn (f2cl-lib:int-sub 1)))
                             nil)
                (tagbody
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add i 1))
                                       ((1 *))
                                       work-%offset%)
                          (f2cl-lib:fref work-%data%
                                         ((f2cl-lib:int-sub
                                           (f2cl-lib:int-add i ie)
                                           1))
                                         ((1 *))
                                         work-%offset%))
                 label50))))
           (cond
             ((< ie 2)
              (f2cl-lib:fdo (i (f2cl-lib:int-add minmn (f2cl-lib:int-sub 1))
                             (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                            ((> i 1) nil)
                (tagbody
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add i 1))
                                       ((1 *))
                                       work-%offset%)
                          (f2cl-lib:fref work-%data%
                                         ((f2cl-lib:int-sub
                                           (f2cl-lib:int-add i ie)
                                           1))
                                         ((1 *))
                                         work-%offset%))
                 label60))))))
        (cond
          ((= iscl 1)
           (if (> anrm bignum)
               (multiple-value-bind
                     (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                      var-9)
                   (dlascl "G" 0 0 bignum anrm minmn 1 s minmn ierr)
                 (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                  var-7 var-8))
                 (setf ierr var-9)))
           (if (and (/= info 0) (> anrm bignum))
               (multiple-value-bind
                     (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                      var-9)
                   (dlascl "G" 0 0 bignum anrm (f2cl-lib:int-sub minmn 1) 1
                    (f2cl-lib:array-slice work-%data%
                                          double-float
                                          (2)
                                          ((1 *))
                                          work-%offset%)
                    minmn ierr)
                 (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                  var-7 var-8))
                 (setf ierr var-9)))
           (if (< anrm smlnum)
               (multiple-value-bind
                     (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                      var-9)
                   (dlascl "G" 0 0 smlnum anrm minmn 1 s minmn ierr)
                 (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                  var-7 var-8))
                 (setf ierr var-9)))
           (if (and (/= info 0) (< anrm smlnum))
               (multiple-value-bind
                     (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                      var-9)
                   (dlascl "G" 0 0 smlnum anrm (f2cl-lib:int-sub minmn 1) 1
                    (f2cl-lib:array-slice work-%data%
                                          double-float
                                          (2)
                                          ((1 *))
                                          work-%offset%)
                    minmn ierr)
                 (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                  var-7 var-8))
                 (setf ierr var-9)))))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (coerce (the f2cl-lib:integer4 maxwrk) 'double-float))
        (go end_label)
       end_label
        (return
         (values nil nil nil nil nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgesvd
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dorglq fortran-to-lisp::dgelqf
                    fortran-to-lisp::dormbr fortran-to-lisp::dgemm
                    fortran-to-lisp::dorgqr fortran-to-lisp::dlacpy
                    fortran-to-lisp::dbdsqr fortran-to-lisp::dorgbr
                    fortran-to-lisp::dgebrd fortran-to-lisp::dlaset
                    fortran-to-lisp::dgeqrf fortran-to-lisp::dlascl
                    fortran-to-lisp::dlange fortran-to-lisp::dlamch
                    fortran-to-lisp::xerbla fortran-to-lisp::lsame
                    fortran-to-lisp::ilaenv))))

