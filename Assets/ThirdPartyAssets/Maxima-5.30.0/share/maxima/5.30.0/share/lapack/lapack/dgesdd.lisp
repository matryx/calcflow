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
  (defun dgesdd (jobz m n a lda s u ldu vt ldvt work lwork iwork info)
    (declare (type (array f2cl-lib:integer4 (*)) iwork)
             (type (array double-float (*)) work vt u s a)
             (type (f2cl-lib:integer4) info lwork ldvt ldu lda n m)
             (type (simple-string *) jobz))
    (f2cl-lib:with-multi-array-data
        ((jobz character jobz-%data% jobz-%offset%)
         (a double-float a-%data% a-%offset%)
         (s double-float s-%data% s-%offset%)
         (u double-float u-%data% u-%offset%)
         (vt double-float vt-%data% vt-%offset%)
         (work double-float work-%data% work-%offset%)
         (iwork f2cl-lib:integer4 iwork-%data% iwork-%offset%))
      (prog ((dum (make-array 1 :element-type 'double-float))
             (idum (make-array 1 :element-type 'f2cl-lib:integer4)) (anrm 0.0)
             (bignum 0.0) (eps 0.0) (smlnum 0.0) (bdspac 0) (blk 0) (chunk 0)
             (i 0) (ie 0) (ierr 0) (il 0) (ir 0) (iscl 0) (itau 0) (itaup 0)
             (itauq 0) (iu 0) (ivt 0) (ldwkvt 0) (ldwrkl 0) (ldwrkr 0)
             (ldwrku 0) (maxwrk 0) (minmn 0) (minwrk 0) (mnthr 0) (nwork 0)
             (wrkbl 0) (lquery nil) (wntqa nil) (wntqas nil) (wntqn nil)
             (wntqo nil) (wntqs nil))
        (declare (type (array double-float (1)) dum)
                 (type (array f2cl-lib:integer4 (1)) idum)
                 (type (double-float) anrm bignum eps smlnum)
                 (type (f2cl-lib:integer4) bdspac blk chunk i ie ierr il ir
                                           iscl itau itaup itauq iu ivt ldwkvt
                                           ldwrkl ldwrkr ldwrku maxwrk minmn
                                           minwrk mnthr nwork wrkbl)
                 (type f2cl-lib:logical lquery wntqa wntqas wntqn wntqo wntqs))
        (setf info 0)
        (setf minmn (min (the f2cl-lib:integer4 m) (the f2cl-lib:integer4 n)))
        (setf mnthr (f2cl-lib:int (/ (* minmn 11.0) 6.0)))
        (setf wntqa (lsame jobz "A"))
        (setf wntqs (lsame jobz "S"))
        (setf wntqas (or wntqa wntqs))
        (setf wntqo (lsame jobz "O"))
        (setf wntqn (lsame jobz "N"))
        (setf minwrk 1)
        (setf maxwrk 1)
        (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
        (cond
          ((not (or wntqa wntqs wntqo wntqn))
           (setf info -1))
          ((< m 0)
           (setf info -2))
          ((< n 0)
           (setf info -3))
          ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
           (setf info -5))
          ((or (< ldu 1) (and wntqas (< ldu m)) (and wntqo (< m n) (< ldu m)))
           (setf info -8))
          ((or (< ldvt 1)
               (and wntqa (< ldvt n))
               (and wntqs (< ldvt minmn))
               (and wntqo (>= m n) (< ldvt n)))
           (setf info -10)))
        (cond
          ((and (= info 0) (> m 0) (> n 0))
           (cond
             ((>= m n)
              (cond
                (wntqn
                 (setf bdspac (f2cl-lib:int-mul 7 n)))
                (t
                 (setf bdspac
                         (f2cl-lib:int-add (f2cl-lib:int-mul 3 n n)
                                           (f2cl-lib:int-mul 4 n)))))
              (cond
                ((>= m mnthr)
                 (cond
                   (wntqn
                    (setf wrkbl
                            (f2cl-lib:int-add n
                                              (f2cl-lib:int-mul n
                                                                (ilaenv 1
                                                                 "DGEQRF" " " m
                                                                 n -1 -1))))
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
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add bdspac n))))
                    (setf minwrk (f2cl-lib:int-add bdspac n)))
                   (wntqo
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
                                                                           "DORMBR"
                                                                           "QLN"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "PRT"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add bdspac
                                                        (f2cl-lib:int-mul 3
                                                                          n)))))
                    (setf maxwrk
                            (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul 2 n n)))
                    (setf minwrk
                            (f2cl-lib:int-add bdspac
                                              (f2cl-lib:int-mul 2 n n)
                                              (f2cl-lib:int-mul 3 n))))
                   (wntqs
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
                                                                           "DORMBR"
                                                                           "QLN"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "PRT"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add bdspac
                                                        (f2cl-lib:int-mul 3
                                                                          n)))))
                    (setf maxwrk
                            (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul n n)))
                    (setf minwrk
                            (f2cl-lib:int-add bdspac
                                              (f2cl-lib:int-mul n n)
                                              (f2cl-lib:int-mul 3 n))))
                   (wntqa
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
                                                                           "DORMBR"
                                                                           "QLN"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "PRT"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add bdspac
                                                        (f2cl-lib:int-mul 3
                                                                          n)))))
                    (setf maxwrk
                            (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul n n)))
                    (setf minwrk
                            (f2cl-lib:int-add bdspac
                                              (f2cl-lib:int-mul n n)
                                              (f2cl-lib:int-mul 3 n))))))
                (t
                 (setf wrkbl
                         (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                           (f2cl-lib:int-mul
                                            (f2cl-lib:int-add m n)
                                            (ilaenv 1 "DGEBRD" " " m n -1 -1))))
                 (cond
                   (wntqn
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add bdspac
                                                        (f2cl-lib:int-mul 3
                                                                          n)))))
                    (setf minwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                              (max (the f2cl-lib:integer4 m)
                                                   (the f2cl-lib:integer4
                                                        bdspac)))))
                   (wntqo
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "QLN"
                                                                           m n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "PRT"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add bdspac
                                                        (f2cl-lib:int-mul 3
                                                                          n)))))
                    (setf maxwrk
                            (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul m n)))
                    (setf minwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                              (max (the f2cl-lib:integer4 m)
                                                   (the f2cl-lib:integer4
                                                        (f2cl-lib:int-add
                                                         (f2cl-lib:int-mul n n)
                                                         bdspac))))))
                   (wntqs
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "QLN"
                                                                           m n
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "PRT"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add bdspac
                                                        (f2cl-lib:int-mul 3
                                                                          n)))))
                    (setf minwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                              (max (the f2cl-lib:integer4 m)
                                                   (the f2cl-lib:integer4
                                                        bdspac)))))
                   (wntqa
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "QLN"
                                                                           m m
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                                        (f2cl-lib:int-mul n
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "PRT"
                                                                           n n
                                                                           n
                                                                           -1))))))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 maxwrk)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add bdspac
                                                        (f2cl-lib:int-mul 3
                                                                          n)))))
                    (setf minwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul 3 n)
                                              (max (the f2cl-lib:integer4 m)
                                                   (the f2cl-lib:integer4
                                                        bdspac)))))))))
             (t
              (cond
                (wntqn
                 (setf bdspac (f2cl-lib:int-mul 7 m)))
                (t
                 (setf bdspac
                         (f2cl-lib:int-add (f2cl-lib:int-mul 3 m m)
                                           (f2cl-lib:int-mul 4 m)))))
              (cond
                ((>= n mnthr)
                 (cond
                   (wntqn
                    (setf wrkbl
                            (f2cl-lib:int-add m
                                              (f2cl-lib:int-mul m
                                                                (ilaenv 1
                                                                 "DGELQF" " " m
                                                                 n -1 -1))))
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
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add bdspac m))))
                    (setf minwrk (f2cl-lib:int-add bdspac m)))
                   (wntqo
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
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "QLN"
                                                                           m m
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "PRT"
                                                                           m m
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add bdspac
                                                        (f2cl-lib:int-mul 3
                                                                          m)))))
                    (setf maxwrk
                            (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul 2 m m)))
                    (setf minwrk
                            (f2cl-lib:int-add bdspac
                                              (f2cl-lib:int-mul 2 m m)
                                              (f2cl-lib:int-mul 3 m))))
                   (wntqs
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
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "QLN"
                                                                           m m
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "PRT"
                                                                           m m
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add bdspac
                                                        (f2cl-lib:int-mul 3
                                                                          m)))))
                    (setf maxwrk
                            (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul m m)))
                    (setf minwrk
                            (f2cl-lib:int-add bdspac
                                              (f2cl-lib:int-mul m m)
                                              (f2cl-lib:int-mul 3 m))))
                   (wntqa
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
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "QLN"
                                                                           m m
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "PRT"
                                                                           m m
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add bdspac
                                                        (f2cl-lib:int-mul 3
                                                                          m)))))
                    (setf maxwrk
                            (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul m m)))
                    (setf minwrk
                            (f2cl-lib:int-add bdspac
                                              (f2cl-lib:int-mul m m)
                                              (f2cl-lib:int-mul 3 m))))))
                (t
                 (setf wrkbl
                         (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                           (f2cl-lib:int-mul
                                            (f2cl-lib:int-add m n)
                                            (ilaenv 1 "DGEBRD" " " m n -1 -1))))
                 (cond
                   (wntqn
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add bdspac
                                                        (f2cl-lib:int-mul 3
                                                                          m)))))
                    (setf minwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                              (max (the f2cl-lib:integer4 n)
                                                   (the f2cl-lib:integer4
                                                        bdspac)))))
                   (wntqo
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "QLN"
                                                                           m m
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "PRT"
                                                                           m n
                                                                           m
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add bdspac
                                                        (f2cl-lib:int-mul 3
                                                                          m)))))
                    (setf maxwrk
                            (f2cl-lib:int-add wrkbl (f2cl-lib:int-mul m n)))
                    (setf minwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                              (max (the f2cl-lib:integer4 n)
                                                   (the f2cl-lib:integer4
                                                        (f2cl-lib:int-add
                                                         (f2cl-lib:int-mul m m)
                                                         bdspac))))))
                   (wntqs
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "QLN"
                                                                           m m
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "PRT"
                                                                           m n
                                                                           m
                                                                           -1))))))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add bdspac
                                                        (f2cl-lib:int-mul 3
                                                                          m)))))
                    (setf minwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                              (max (the f2cl-lib:integer4 n)
                                                   (the f2cl-lib:integer4
                                                        bdspac)))))
                   (wntqa
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "QLN"
                                                                           m m
                                                                           n
                                                                           -1))))))
                    (setf wrkbl
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                                        (f2cl-lib:int-mul m
                                                                          (ilaenv
                                                                           1
                                                                           "DORMBR"
                                                                           "PRT"
                                                                           n n
                                                                           m
                                                                           -1))))))
                    (setf maxwrk
                            (max (the f2cl-lib:integer4 wrkbl)
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add bdspac
                                                        (f2cl-lib:int-mul 3
                                                                          m)))))
                    (setf minwrk
                            (f2cl-lib:int-add (f2cl-lib:int-mul 3 m)
                                              (max (the f2cl-lib:integer4 n)
                                                   (the f2cl-lib:integer4
                                                        bdspac))))))))))
           (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                   (coerce (the f2cl-lib:integer4 maxwrk) 'double-float))))
        (cond
          ((and (< lwork minwrk) (not lquery))
           (setf info -12)))
        (cond
          ((/= info 0)
           (xerbla "DGESDD" (f2cl-lib:int-sub info))
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
                (wntqn
                 (setf itau 1)
                 (setf nwork (f2cl-lib:int-add itau n))
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
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
                 (setf nwork (f2cl-lib:int-add itaup n))
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9))
                   (setf ierr var-10))
                 (setf nwork (f2cl-lib:int-add ie n))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dbdsdc "U" "N" n s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      dum 1 dum 1 dum idum
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      iwork info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13)))
                (wntqo
                 (setf ir 1)
                 (cond
                   ((>= lwork
                        (f2cl-lib:int-add (f2cl-lib:int-mul lda n)
                                          (f2cl-lib:int-mul n n)
                                          (f2cl-lib:int-mul 3 n)
                                          bdspac))
                    (setf ldwrkr lda))
                   (t
                    (setf ldwrkr
                            (the f2cl-lib:integer4
                                 (truncate (- lwork (* n n) (* 3 n) bdspac)
                                           n)))))
                 (setf itau (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr n)))
                 (setf nwork (f2cl-lib:int-add itau n))
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                   (setf ierr var-7))
                 (dlacpy "U" n n a lda
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (ir)
                                        ((1 *))
                                        work-%offset%)
                  ldwrkr)
                 (dlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1) zero
                  zero
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        ((+ ir 1))
                                        ((1 *))
                                        work-%offset%)
                  ldwrkr)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                     (dorgqr m n n a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itau)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7))
                   (setf ierr var-8))
                 (setf ie itau)
                 (setf itauq (f2cl-lib:int-add ie n))
                 (setf itaup (f2cl-lib:int-add itauq n))
                 (setf nwork (f2cl-lib:int-add itaup n))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10)
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9))
                   (setf ierr var-10))
                 (setf iu nwork)
                 (setf nwork (f2cl-lib:int-add iu (f2cl-lib:int-mul n n)))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dbdsdc "U" "I" n s
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
                      n vt ldvt dum idum
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      iwork info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "Q" "L" "N" n n n
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
                                            (iu)
                                            ((1 *))
                                            work-%offset%)
                      n
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "P" "R" "T" n n n
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
                      vt ldvt
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i ldwrkr))
                               ((> i m) nil)
                   (tagbody
                     (setf chunk
                             (min
                              (the f2cl-lib:integer4
                                   (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1))
                              (the f2cl-lib:integer4 ldwrkr)))
                     (dgemm "N" "N" chunk n n one
                      (f2cl-lib:array-slice a-%data%
                                            double-float
                                            (i 1)
                                            ((1 lda) (1 *))
                                            a-%offset%)
                      lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (iu)
                                            ((1 *))
                                            work-%offset%)
                      n zero
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ir)
                                            ((1 *))
                                            work-%offset%)
                      ldwrkr)
                     (dlacpy "F" chunk n
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ir)
                                            ((1 *))
                                            work-%offset%)
                      ldwrkr
                      (f2cl-lib:array-slice a-%data%
                                            double-float
                                            (i 1)
                                            ((1 lda) (1 *))
                                            a-%offset%)
                      lda)
                    label10)))
                (wntqs
                 (setf ir 1)
                 (setf ldwrkr n)
                 (setf itau (f2cl-lib:int-add ir (f2cl-lib:int-mul ldwrkr n)))
                 (setf nwork (f2cl-lib:int-add itau n))
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                   (setf ierr var-7))
                 (dlacpy "U" n n a lda
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (ir)
                                        ((1 *))
                                        work-%offset%)
                  ldwrkr)
                 (dlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1) zero
                  zero
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        ((+ ir 1))
                                        ((1 *))
                                        work-%offset%)
                  ldwrkr)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                     (dorgqr m n n a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itau)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7))
                   (setf ierr var-8))
                 (setf ie itau)
                 (setf itauq (f2cl-lib:int-add ie n))
                 (setf itaup (f2cl-lib:int-add itauq n))
                 (setf nwork (f2cl-lib:int-add itaup n))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10)
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9))
                   (setf ierr var-10))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dbdsdc "U" "I" n s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      u ldu vt ldvt dum idum
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      iwork info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "Q" "L" "N" n n n
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
                      u ldu
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "P" "R" "T" n n n
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
                      vt ldvt
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (dlacpy "F" n n u ldu
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (ir)
                                        ((1 *))
                                        work-%offset%)
                  ldwrkr)
                 (dgemm "N" "N" m n n one a lda
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (ir)
                                        ((1 *))
                                        work-%offset%)
                  ldwrkr zero u ldu))
                (wntqa
                 (setf iu 1)
                 (setf ldwrku n)
                 (setf itau (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku n)))
                 (setf nwork (f2cl-lib:int-add itau n))
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                   (setf ierr var-7))
                 (dlacpy "L" m n a lda u ldu)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                     (dorgqr m m n u ldu
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itau)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7))
                   (setf ierr var-8))
                 (dlaset "L" (f2cl-lib:int-sub n 1) (f2cl-lib:int-sub n 1) zero
                  zero
                  (f2cl-lib:array-slice a-%data%
                                        double-float
                                        (2 1)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                  lda)
                 (setf ie itau)
                 (setf itauq (f2cl-lib:int-add ie n))
                 (setf itaup (f2cl-lib:int-add itauq n))
                 (setf nwork (f2cl-lib:int-add itaup n))
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9))
                   (setf ierr var-10))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dbdsdc "U" "I" n s
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
                      n vt ldvt dum idum
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      iwork info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "Q" "L" "N" n n n a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itauq)
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "P" "R" "T" n n n a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itaup)
                                            ((1 *))
                                            work-%offset%)
                      vt ldvt
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (dgemm "N" "N" m n n one u ldu
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (iu)
                                        ((1 *))
                                        work-%offset%)
                  ldwrku zero a lda)
                 (dlacpy "F" m n a lda u ldu))))
             (t
              (setf ie 1)
              (setf itauq (f2cl-lib:int-add ie n))
              (setf itaup (f2cl-lib:int-add itauq n))
              (setf nwork (f2cl-lib:int-add itaup n))
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
                                         (nwork)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7 var-8 var-9))
                (setf ierr var-10))
              (cond
                (wntqn
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dbdsdc "U" "N" n s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      dum 1 dum 1 dum idum
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      iwork info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13)))
                (wntqo
                 (setf iu nwork)
                 (cond
                   ((>= lwork
                        (f2cl-lib:int-add (f2cl-lib:int-mul m n)
                                          (f2cl-lib:int-mul 3 n)
                                          bdspac))
                    (setf ldwrku m)
                    (setf nwork
                            (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku n)))
                    (dlaset "F" m n zero zero
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (iu)
                                           ((1 *))
                                           work-%offset%)
                     ldwrku))
                   (t
                    (setf ldwrku n)
                    (setf nwork
                            (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku n)))
                    (setf ir nwork)
                    (setf ldwrkr
                            (the f2cl-lib:integer4
                                 (truncate (- lwork (* n n) (* 3 n)) n)))))
                 (setf nwork (f2cl-lib:int-add iu (f2cl-lib:int-mul ldwrku n)))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dbdsdc "U" "I" n s
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
                      ldwrku vt ldvt dum idum
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      iwork info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "P" "R" "T" n n n a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itaup)
                                            ((1 *))
                                            work-%offset%)
                      vt ldvt
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (cond
                   ((>= lwork
                        (f2cl-lib:int-add (f2cl-lib:int-mul m n)
                                          (f2cl-lib:int-mul 3 n)
                                          bdspac))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13)
                        (dormbr "Q" "L" "N" m n n a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itauq)
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
                                               (nwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9 var-10 var-11
                                       var-12))
                      (setf ierr var-13))
                    (dlacpy "F" m n
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (iu)
                                           ((1 *))
                                           work-%offset%)
                     ldwrku a lda))
                   (t
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
                                               (nwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8))
                      (setf ierr var-9))
                    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i ldwrkr))
                                  ((> i m) nil)
                      (tagbody
                        (setf chunk
                                (min
                                 (the f2cl-lib:integer4
                                      (f2cl-lib:int-add (f2cl-lib:int-sub m i)
                                                        1))
                                 (the f2cl-lib:integer4 ldwrkr)))
                        (dgemm "N" "N" chunk n n one
                         (f2cl-lib:array-slice a-%data%
                                               double-float
                                               (i 1)
                                               ((1 lda) (1 *))
                                               a-%offset%)
                         lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (iu)
                                               ((1 *))
                                               work-%offset%)
                         ldwrku zero
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ir)
                                               ((1 *))
                                               work-%offset%)
                         ldwrkr)
                        (dlacpy "F" chunk n
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ir)
                                               ((1 *))
                                               work-%offset%)
                         ldwrkr
                         (f2cl-lib:array-slice a-%data%
                                               double-float
                                               (i 1)
                                               ((1 lda) (1 *))
                                               a-%offset%)
                         lda)
                       label20)))))
                (wntqs
                 (dlaset "F" m n zero zero u ldu)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dbdsdc "U" "I" n s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      u ldu vt ldvt dum idum
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      iwork info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "Q" "L" "N" m n n a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itauq)
                                            ((1 *))
                                            work-%offset%)
                      u ldu
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "P" "R" "T" n n n a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itaup)
                                            ((1 *))
                                            work-%offset%)
                      vt ldvt
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13)))
                (wntqa
                 (dlaset "F" m m zero zero u ldu)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dbdsdc "U" "I" n s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      u ldu vt ldvt dum idum
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      iwork info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13))
                 (dlaset "F" (f2cl-lib:int-sub m n) (f2cl-lib:int-sub m n) zero
                  one
                  (f2cl-lib:array-slice u-%data%
                                        double-float
                                        ((+ n 1) (f2cl-lib:int-add n 1))
                                        ((1 ldu) (1 *))
                                        u-%offset%)
                  ldu)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "Q" "L" "N" m m n a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itauq)
                                            ((1 *))
                                            work-%offset%)
                      u ldu
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "P" "R" "T" n n m a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itaup)
                                            ((1 *))
                                            work-%offset%)
                      vt ldvt
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13)))))))
          (t
           (cond
             ((>= n mnthr)
              (cond
                (wntqn
                 (setf itau 1)
                 (setf nwork (f2cl-lib:int-add itau m))
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
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
                 (setf nwork (f2cl-lib:int-add itaup m))
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9))
                   (setf ierr var-10))
                 (setf nwork (f2cl-lib:int-add ie m))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dbdsdc "U" "N" m s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      dum 1 dum 1 dum idum
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      iwork info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13)))
                (wntqo
                 (setf ivt 1)
                 (setf il (f2cl-lib:int-add ivt (f2cl-lib:int-mul m m)))
                 (cond
                   ((>= lwork
                        (f2cl-lib:int-add (f2cl-lib:int-mul m n)
                                          (f2cl-lib:int-mul m m)
                                          (f2cl-lib:int-mul 3 m)
                                          bdspac))
                    (setf ldwrkl m)
                    (setf chunk n))
                   (t
                    (setf ldwrkl m)
                    (setf chunk
                            (the f2cl-lib:integer4
                                 (truncate (- lwork (* m m)) m)))))
                 (setf itau (f2cl-lib:int-add il (f2cl-lib:int-mul ldwrkl m)))
                 (setf nwork (f2cl-lib:int-add itau m))
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                   (setf ierr var-7))
                 (dlacpy "L" m m a lda
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (il)
                                        ((1 *))
                                        work-%offset%)
                  ldwrkl)
                 (dlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1) zero
                  zero
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        ((+ il ldwrkl))
                                        ((1 *))
                                        work-%offset%)
                  ldwrkl)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                     (dorglq m n m a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itau)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7))
                   (setf ierr var-8))
                 (setf ie itau)
                 (setf itauq (f2cl-lib:int-add ie m))
                 (setf itaup (f2cl-lib:int-add itauq m))
                 (setf nwork (f2cl-lib:int-add itaup m))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10)
                     (dgebrd m m
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (il)
                                            ((1 *))
                                            work-%offset%)
                      ldwrkl s
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9))
                   (setf ierr var-10))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dbdsdc "U" "I" m s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      u ldu
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ivt)
                                            ((1 *))
                                            work-%offset%)
                      m dum idum
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      iwork info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "Q" "L" "N" m m m
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (il)
                                            ((1 *))
                                            work-%offset%)
                      ldwrkl
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itauq)
                                            ((1 *))
                                            work-%offset%)
                      u ldu
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "P" "R" "T" m m m
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (il)
                                            ((1 *))
                                            work-%offset%)
                      ldwrkl
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itaup)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ivt)
                                            ((1 *))
                                            work-%offset%)
                      m
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i chunk))
                               ((> i n) nil)
                   (tagbody
                     (setf blk
                             (min
                              (the f2cl-lib:integer4
                                   (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1))
                              (the f2cl-lib:integer4 chunk)))
                     (dgemm "N" "N" m blk m one
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ivt)
                                            ((1 *))
                                            work-%offset%)
                      m
                      (f2cl-lib:array-slice a-%data%
                                            double-float
                                            (1 i)
                                            ((1 lda) (1 *))
                                            a-%offset%)
                      lda zero
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (il)
                                            ((1 *))
                                            work-%offset%)
                      ldwrkl)
                     (dlacpy "F" m blk
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (il)
                                            ((1 *))
                                            work-%offset%)
                      ldwrkl
                      (f2cl-lib:array-slice a-%data%
                                            double-float
                                            (1 i)
                                            ((1 lda) (1 *))
                                            a-%offset%)
                      lda)
                    label30)))
                (wntqs
                 (setf il 1)
                 (setf ldwrkl m)
                 (setf itau (f2cl-lib:int-add il (f2cl-lib:int-mul ldwrkl m)))
                 (setf nwork (f2cl-lib:int-add itau m))
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                   (setf ierr var-7))
                 (dlacpy "L" m m a lda
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (il)
                                        ((1 *))
                                        work-%offset%)
                  ldwrkl)
                 (dlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1) zero
                  zero
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        ((+ il ldwrkl))
                                        ((1 *))
                                        work-%offset%)
                  ldwrkl)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                     (dorglq m n m a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itau)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7))
                   (setf ierr var-8))
                 (setf ie itau)
                 (setf itauq (f2cl-lib:int-add ie m))
                 (setf itaup (f2cl-lib:int-add itauq m))
                 (setf nwork (f2cl-lib:int-add itaup m))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10)
                     (dgebrd m m
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (il)
                                            ((1 *))
                                            work-%offset%)
                      ldwrkl s
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9))
                   (setf ierr var-10))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dbdsdc "U" "I" m s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      u ldu vt ldvt dum idum
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      iwork info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "Q" "L" "N" m m m
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (il)
                                            ((1 *))
                                            work-%offset%)
                      ldwrkl
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itauq)
                                            ((1 *))
                                            work-%offset%)
                      u ldu
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "P" "R" "T" m m m
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (il)
                                            ((1 *))
                                            work-%offset%)
                      ldwrkl
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itaup)
                                            ((1 *))
                                            work-%offset%)
                      vt ldvt
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (dlacpy "F" m m vt ldvt
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (il)
                                        ((1 *))
                                        work-%offset%)
                  ldwrkl)
                 (dgemm "N" "N" m n m one
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (il)
                                        ((1 *))
                                        work-%offset%)
                  ldwrkl a lda zero vt ldvt))
                (wntqa
                 (setf ivt 1)
                 (setf ldwkvt m)
                 (setf itau (f2cl-lib:int-add ivt (f2cl-lib:int-mul ldwkvt m)))
                 (setf nwork (f2cl-lib:int-add itau m))
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
                   (setf ierr var-7))
                 (dlacpy "U" m n a lda vt ldvt)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
                     (dorglq n n m vt ldvt
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itau)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7))
                   (setf ierr var-8))
                 (dlaset "U" (f2cl-lib:int-sub m 1) (f2cl-lib:int-sub m 1) zero
                  zero
                  (f2cl-lib:array-slice a-%data%
                                        double-float
                                        (1 2)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                  lda)
                 (setf ie itau)
                 (setf itauq (f2cl-lib:int-add ie m))
                 (setf itaup (f2cl-lib:int-add itauq m))
                 (setf nwork (f2cl-lib:int-add itaup m))
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
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9))
                   (setf ierr var-10))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dbdsdc "U" "I" m s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      u ldu
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ivt)
                                            ((1 *))
                                            work-%offset%)
                      ldwkvt dum idum
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      iwork info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "Q" "L" "N" m m m a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itauq)
                                            ((1 *))
                                            work-%offset%)
                      u ldu
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "P" "R" "T" m m m a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itaup)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ivt)
                                            ((1 *))
                                            work-%offset%)
                      ldwkvt
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (dgemm "N" "N" m n m one
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (ivt)
                                        ((1 *))
                                        work-%offset%)
                  ldwkvt vt ldvt zero a lda)
                 (dlacpy "F" m n a lda vt ldvt))))
             (t
              (setf ie 1)
              (setf itauq (f2cl-lib:int-add ie m))
              (setf itaup (f2cl-lib:int-add itauq m))
              (setf nwork (f2cl-lib:int-add itaup m))
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
                                         (nwork)
                                         ((1 *))
                                         work-%offset%)
                   (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7 var-8 var-9))
                (setf ierr var-10))
              (cond
                (wntqn
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dbdsdc "L" "N" m s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      dum 1 dum 1 dum idum
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      iwork info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13)))
                (wntqo
                 (setf ldwkvt m)
                 (setf ivt nwork)
                 (cond
                   ((>= lwork
                        (f2cl-lib:int-add (f2cl-lib:int-mul m n)
                                          (f2cl-lib:int-mul 3 m)
                                          bdspac))
                    (dlaset "F" m n zero zero
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (ivt)
                                           ((1 *))
                                           work-%offset%)
                     ldwkvt)
                    (setf nwork
                            (f2cl-lib:int-add ivt (f2cl-lib:int-mul ldwkvt n))))
                   (t
                    (setf nwork
                            (f2cl-lib:int-add ivt (f2cl-lib:int-mul ldwkvt m)))
                    (setf il nwork)
                    (setf chunk
                            (the f2cl-lib:integer4
                                 (truncate (- lwork (* m m) (* 3 m)) m)))))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dbdsdc "L" "I" m s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      u ldu
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ivt)
                                            ((1 *))
                                            work-%offset%)
                      ldwkvt dum idum
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      iwork info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "Q" "L" "N" m m n a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itauq)
                                            ((1 *))
                                            work-%offset%)
                      u ldu
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (cond
                   ((>= lwork
                        (f2cl-lib:int-add (f2cl-lib:int-mul m n)
                                          (f2cl-lib:int-mul 3 m)
                                          bdspac))
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13)
                        (dormbr "P" "R" "T" m n m a lda
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (itaup)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (ivt)
                                               ((1 *))
                                               work-%offset%)
                         ldwkvt
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (nwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9 var-10 var-11
                                       var-12))
                      (setf ierr var-13))
                    (dlacpy "F" m n
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (ivt)
                                           ((1 *))
                                           work-%offset%)
                     ldwkvt a lda))
                   (t
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
                                               (nwork)
                                               ((1 *))
                                               work-%offset%)
                         (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1)
                         ierr)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8))
                      (setf ierr var-9))
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
                                               (ivt)
                                               ((1 *))
                                               work-%offset%)
                         ldwkvt
                         (f2cl-lib:array-slice a-%data%
                                               double-float
                                               (1 i)
                                               ((1 lda) (1 *))
                                               a-%offset%)
                         lda zero
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (il)
                                               ((1 *))
                                               work-%offset%)
                         m)
                        (dlacpy "F" m blk
                         (f2cl-lib:array-slice work-%data%
                                               double-float
                                               (il)
                                               ((1 *))
                                               work-%offset%)
                         m
                         (f2cl-lib:array-slice a-%data%
                                               double-float
                                               (1 i)
                                               ((1 lda) (1 *))
                                               a-%offset%)
                         lda)
                       label40)))))
                (wntqs
                 (dlaset "F" m n zero zero vt ldvt)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dbdsdc "L" "I" m s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      u ldu vt ldvt dum idum
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      iwork info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "Q" "L" "N" m m n a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itauq)
                                            ((1 *))
                                            work-%offset%)
                      u ldu
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "P" "R" "T" m n m a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itaup)
                                            ((1 *))
                                            work-%offset%)
                      vt ldvt
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13)))
                (wntqa
                 (dlaset "F" n n zero zero vt ldvt)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dbdsdc "L" "I" m s
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (ie)
                                            ((1 *))
                                            work-%offset%)
                      u ldu vt ldvt dum idum
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      iwork info)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf info var-13))
                 (dlaset "F" (f2cl-lib:int-sub n m) (f2cl-lib:int-sub n m) zero
                  one
                  (f2cl-lib:array-slice vt-%data%
                                        double-float
                                        ((+ m 1) (f2cl-lib:int-add m 1))
                                        ((1 ldvt) (1 *))
                                        vt-%offset%)
                  ldvt)
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "Q" "L" "N" m m n a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itauq)
                                            ((1 *))
                                            work-%offset%)
                      u ldu
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))
                 (multiple-value-bind
                       (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                        var-9 var-10 var-11 var-12 var-13)
                     (dormbr "P" "R" "T" n n m a lda
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (itaup)
                                            ((1 *))
                                            work-%offset%)
                      vt ldvt
                      (f2cl-lib:array-slice work-%data%
                                            double-float
                                            (nwork)
                                            ((1 *))
                                            work-%offset%)
                      (f2cl-lib:int-add (f2cl-lib:int-sub lwork nwork) 1) ierr)
                   (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                    var-7 var-8 var-9 var-10 var-11 var-12))
                   (setf ierr var-13))))))))
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
           (if (< anrm smlnum)
               (multiple-value-bind
                     (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                      var-9)
                   (dlascl "G" 0 0 smlnum anrm minmn 1 s minmn ierr)
                 (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                  var-7 var-8))
                 (setf ierr var-9)))))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (f2cl-lib:dble maxwrk))
        (go end_label)
       end_label
        (return
         (values nil nil nil nil nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgesdd
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dorglq fortran-to-lisp::dgelqf
                    fortran-to-lisp::dorgbr fortran-to-lisp::dgemm
                    fortran-to-lisp::dormbr fortran-to-lisp::dorgqr
                    fortran-to-lisp::dlacpy fortran-to-lisp::dbdsdc
                    fortran-to-lisp::dgebrd fortran-to-lisp::dlaset
                    fortran-to-lisp::dgeqrf fortran-to-lisp::dlascl
                    fortran-to-lisp::dlange fortran-to-lisp::dlamch
                    fortran-to-lisp::xerbla fortran-to-lisp::ilaenv
                    fortran-to-lisp::lsame))))

