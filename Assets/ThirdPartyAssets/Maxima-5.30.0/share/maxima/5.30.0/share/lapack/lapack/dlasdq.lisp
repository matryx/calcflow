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


(let* ((zero 0.0))
  (declare (type (double-float 0.0 0.0) zero) (ignorable zero))
  (defun dlasdq (uplo sqre n ncvt nru ncc d e vt ldvt u ldu c ldc work info)
    (declare (type (array double-float (*)) work c u vt e d)
             (type (f2cl-lib:integer4) info ldc ldu ldvt ncc nru ncvt n sqre)
             (type (simple-string *) uplo))
    (f2cl-lib:with-multi-array-data
        ((uplo character uplo-%data% uplo-%offset%)
         (d double-float d-%data% d-%offset%)
         (e double-float e-%data% e-%offset%)
         (vt double-float vt-%data% vt-%offset%)
         (u double-float u-%data% u-%offset%)
         (c double-float c-%data% c-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((cs 0.0) (r 0.0) (smin 0.0) (sn 0.0) (i 0) (isub 0) (iuplo 0)
             (j 0) (np1 0) (sqre1 0) (rotate nil))
        (declare (type (double-float) cs r smin sn)
                 (type (f2cl-lib:integer4) i isub iuplo j np1 sqre1)
                 (type f2cl-lib:logical rotate))
        (setf info 0)
        (setf iuplo 0)
        (if (lsame uplo "U") (setf iuplo 1))
        (if (lsame uplo "L") (setf iuplo 2))
        (cond
          ((= iuplo 0)
           (setf info -1))
          ((or (< sqre 0) (> sqre 1))
           (setf info -2))
          ((< n 0)
           (setf info -3))
          ((< ncvt 0)
           (setf info -4))
          ((< nru 0)
           (setf info -5))
          ((< ncc 0)
           (setf info -6))
          ((or (and (= ncvt 0) (< ldvt 1))
               (and (> ncvt 0)
                    (< ldvt
                       (max (the f2cl-lib:integer4 1)
                            (the f2cl-lib:integer4 n)))))
           (setf info -10))
          ((< ldu (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 nru)))
           (setf info -12))
          ((or (and (= ncc 0) (< ldc 1))
               (and (> ncc 0)
                    (< ldc
                       (max (the f2cl-lib:integer4 1)
                            (the f2cl-lib:integer4 n)))))
           (setf info -14)))
        (cond
          ((/= info 0)
           (xerbla "DLASDQ" (f2cl-lib:int-sub info))
           (go end_label)))
        (if (= n 0) (go end_label))
        (setf rotate (or (> ncvt 0) (> nru 0) (> ncc 0)))
        (setf np1 (f2cl-lib:int-add n 1))
        (setf sqre1 sqre)
        (cond
          ((and (= iuplo 1) (= sqre1 1))
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
             (tagbody
               (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                   (dlartg (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                    (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%) cs sn r)
                 (declare (ignore var-0 var-1))
                 (setf cs var-2)
                 (setf sn var-3)
                 (setf r var-4))
               (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%) r)
               (setf (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)
                       (* sn
                          (f2cl-lib:fref d-%data%
                                         ((f2cl-lib:int-add i 1))
                                         ((1 *))
                                         d-%offset%)))
               (setf (f2cl-lib:fref d-%data%
                                    ((f2cl-lib:int-add i 1))
                                    ((1 *))
                                    d-%offset%)
                       (* cs
                          (f2cl-lib:fref d-%data%
                                         ((f2cl-lib:int-add i 1))
                                         ((1 *))
                                         d-%offset%)))
               (cond
                 (rotate
                  (setf (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%)
                          cs)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add n i))
                                       ((1 *))
                                       work-%offset%)
                          sn)))
              label10))
           (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
               (dlartg (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                (f2cl-lib:fref e-%data% (n) ((1 *)) e-%offset%) cs sn r)
             (declare (ignore var-0 var-1))
             (setf cs var-2)
             (setf sn var-3)
             (setf r var-4))
           (setf (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%) r)
           (setf (f2cl-lib:fref e-%data% (n) ((1 *)) e-%offset%) zero)
           (cond
             (rotate
              (setf (f2cl-lib:fref work-%data% (n) ((1 *)) work-%offset%) cs)
              (setf (f2cl-lib:fref work-%data%
                                   ((f2cl-lib:int-add n n))
                                   ((1 *))
                                   work-%offset%)
                      sn)))
           (setf iuplo 2)
           (setf sqre1 0)
           (if (> ncvt 0)
               (dlasr "L" "V" "F" np1 ncvt
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (1)
                                      ((1 *))
                                      work-%offset%)
                (f2cl-lib:array-slice work-%data%
                                      double-float
                                      (np1)
                                      ((1 *))
                                      work-%offset%)
                vt ldvt))))
        (cond
          ((= iuplo 2)
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
             (tagbody
               (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                   (dlartg (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%)
                    (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%) cs sn r)
                 (declare (ignore var-0 var-1))
                 (setf cs var-2)
                 (setf sn var-3)
                 (setf r var-4))
               (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%) r)
               (setf (f2cl-lib:fref e-%data% (i) ((1 *)) e-%offset%)
                       (* sn
                          (f2cl-lib:fref d-%data%
                                         ((f2cl-lib:int-add i 1))
                                         ((1 *))
                                         d-%offset%)))
               (setf (f2cl-lib:fref d-%data%
                                    ((f2cl-lib:int-add i 1))
                                    ((1 *))
                                    d-%offset%)
                       (* cs
                          (f2cl-lib:fref d-%data%
                                         ((f2cl-lib:int-add i 1))
                                         ((1 *))
                                         d-%offset%)))
               (cond
                 (rotate
                  (setf (f2cl-lib:fref work-%data% (i) ((1 *)) work-%offset%)
                          cs)
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-add n i))
                                       ((1 *))
                                       work-%offset%)
                          sn)))
              label20))
           (cond
             ((= sqre1 1)
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                  (dlartg (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%)
                   (f2cl-lib:fref e-%data% (n) ((1 *)) e-%offset%) cs sn r)
                (declare (ignore var-0 var-1))
                (setf cs var-2)
                (setf sn var-3)
                (setf r var-4))
              (setf (f2cl-lib:fref d-%data% (n) ((1 *)) d-%offset%) r)
              (cond
                (rotate
                 (setf (f2cl-lib:fref work-%data% (n) ((1 *)) work-%offset%) cs)
                 (setf (f2cl-lib:fref work-%data%
                                      ((f2cl-lib:int-add n n))
                                      ((1 *))
                                      work-%offset%)
                         sn)))))
           (cond
             ((> nru 0)
              (cond
                ((= sqre1 0)
                 (dlasr "R" "V" "F" nru n
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (1)
                                        ((1 *))
                                        work-%offset%)
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (np1)
                                        ((1 *))
                                        work-%offset%)
                  u ldu))
                (t
                 (dlasr "R" "V" "F" nru np1
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (1)
                                        ((1 *))
                                        work-%offset%)
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (np1)
                                        ((1 *))
                                        work-%offset%)
                  u ldu)))))
           (cond
             ((> ncc 0)
              (cond
                ((= sqre1 0)
                 (dlasr "L" "V" "F" n ncc
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (1)
                                        ((1 *))
                                        work-%offset%)
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (np1)
                                        ((1 *))
                                        work-%offset%)
                  c ldc))
                (t
                 (dlasr "L" "V" "F" np1 ncc
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (1)
                                        ((1 *))
                                        work-%offset%)
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (np1)
                                        ((1 *))
                                        work-%offset%)
                  c ldc)))))))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11 var-12 var-13 var-14)
            (dbdsqr "U" n ncvt nru ncc d e vt ldvt u ldu c ldc work info)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13))
          (setf info var-14))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf isub i)
            (setf smin (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
            (f2cl-lib:fdo (j (f2cl-lib:int-add i 1) (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (cond
                  ((< (f2cl-lib:fref d (j) ((1 *))) smin)
                   (setf isub j)
                   (setf smin
                           (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%))))
               label30))
            (cond
              ((/= isub i)
               (setf (f2cl-lib:fref d-%data% (isub) ((1 *)) d-%offset%)
                       (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%))
               (setf (f2cl-lib:fref d-%data% (i) ((1 *)) d-%offset%) smin)
               (if (> ncvt 0)
                   (dswap ncvt
                    (f2cl-lib:array-slice vt-%data%
                                          double-float
                                          (isub 1)
                                          ((1 ldvt) (1 *))
                                          vt-%offset%)
                    ldvt
                    (f2cl-lib:array-slice vt-%data%
                                          double-float
                                          (i 1)
                                          ((1 ldvt) (1 *))
                                          vt-%offset%)
                    ldvt))
               (if (> nru 0)
                   (dswap nru
                    (f2cl-lib:array-slice u-%data%
                                          double-float
                                          (1 isub)
                                          ((1 ldu) (1 *))
                                          u-%offset%)
                    1
                    (f2cl-lib:array-slice u-%data%
                                          double-float
                                          (1 i)
                                          ((1 ldu) (1 *))
                                          u-%offset%)
                    1))
               (if (> ncc 0)
                   (dswap ncc
                    (f2cl-lib:array-slice c-%data%
                                          double-float
                                          (isub 1)
                                          ((1 ldc) (1 *))
                                          c-%offset%)
                    ldc
                    (f2cl-lib:array-slice c-%data%
                                          double-float
                                          (i 1)
                                          ((1 ldc) (1 *))
                                          c-%offset%)
                    ldc))))
           label40))
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
                 info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasdq
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dswap fortran-to-lisp::dbdsqr
                    fortran-to-lisp::dlasr fortran-to-lisp::dlartg
                    fortran-to-lisp::xerbla fortran-to-lisp::lsame))))

