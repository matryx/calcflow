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


(let* ((one 1.0))
  (declare (type (double-float 1.0 1.0) one) (ignorable one))
  (defun dgebrd (m n a lda d e tauq taup work lwork info)
    (declare (type (array double-float (*)) work taup tauq e d a)
             (type (f2cl-lib:integer4) info lwork lda n m))
    (f2cl-lib:with-multi-array-data
        ((a double-float a-%data% a-%offset%)
         (d double-float d-%data% d-%offset%)
         (e double-float e-%data% e-%offset%)
         (tauq double-float tauq-%data% tauq-%offset%)
         (taup double-float taup-%data% taup-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((ws 0.0) (i 0) (iinfo 0) (j 0) (ldwrkx 0) (ldwrky 0) (lwkopt 0)
             (minmn 0) (nb 0) (nbmin 0) (nx 0) (lquery nil))
        (declare (type (double-float) ws)
                 (type (f2cl-lib:integer4) i iinfo j ldwrkx ldwrky lwkopt minmn
                                           nb nbmin nx)
                 (type f2cl-lib:logical lquery))
        (setf info 0)
        (setf nb
                (max (the f2cl-lib:integer4 1)
                     (the f2cl-lib:integer4
                          (ilaenv 1 "DGEBRD" " " m n -1 -1))))
        (setf lwkopt (f2cl-lib:int-mul (f2cl-lib:int-add m n) nb))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (f2cl-lib:dble lwkopt))
        (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
        (cond
          ((< m 0)
           (setf info -1))
          ((< n 0)
           (setf info -2))
          ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
           (setf info -4))
          ((and
            (< lwork
               (max (the f2cl-lib:integer4 1)
                    (the f2cl-lib:integer4 m)
                    (the f2cl-lib:integer4 n)))
            (not lquery))
           (setf info -10)))
        (cond
          ((< info 0)
           (xerbla "DGEBRD" (f2cl-lib:int-sub info))
           (go end_label))
          (lquery
           (go end_label)))
        (setf minmn (min (the f2cl-lib:integer4 m) (the f2cl-lib:integer4 n)))
        (cond
          ((= minmn 0)
           (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                   (coerce (the f2cl-lib:integer4 1) 'double-float))
           (go end_label)))
        (setf ws
                (coerce
                 (the f2cl-lib:integer4
                      (max (the f2cl-lib:integer4 m)
                           (the f2cl-lib:integer4 n)))
                 'double-float))
        (setf ldwrkx m)
        (setf ldwrky n)
        (cond
          ((and (> nb 1) (< nb minmn))
           (setf nx
                   (max (the f2cl-lib:integer4 nb)
                        (the f2cl-lib:integer4
                             (ilaenv 3 "DGEBRD" " " m n -1 -1))))
           (cond
             ((< nx minmn)
              (setf ws
                      (coerce
                       (the f2cl-lib:integer4
                            (f2cl-lib:int-mul (f2cl-lib:int-add m n) nb))
                       'double-float))
              (cond
                ((< lwork ws)
                 (setf nbmin (ilaenv 2 "DGEBRD" " " m n -1 -1))
                 (cond
                   ((>= lwork (f2cl-lib:int-mul (f2cl-lib:int-add m n) nbmin))
                    (setf nb (the f2cl-lib:integer4 (truncate lwork (+ m n)))))
                   (t
                    (setf nb 1)
                    (setf nx minmn))))))))
          (t
           (setf nx minmn)))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i nb))
                      ((> i (f2cl-lib:int-add minmn (f2cl-lib:int-sub nx))) nil)
          (tagbody
            (dlabrd (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1)
             (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1) nb
             (f2cl-lib:array-slice a-%data%
                                   double-float
                                   (i i)
                                   ((1 lda) (1 *))
                                   a-%offset%)
             lda
             (f2cl-lib:array-slice d-%data%
                                   double-float
                                   (i)
                                   ((1 *))
                                   d-%offset%)
             (f2cl-lib:array-slice e-%data%
                                   double-float
                                   (i)
                                   ((1 *))
                                   e-%offset%)
             (f2cl-lib:array-slice tauq-%data%
                                   double-float
                                   (i)
                                   ((1 *))
                                   tauq-%offset%)
             (f2cl-lib:array-slice taup-%data%
                                   double-float
                                   (i)
                                   ((1 *))
                                   taup-%offset%)
             work ldwrkx
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   ((+ (f2cl-lib:int-mul ldwrkx nb) 1))
                                   ((1 *))
                                   work-%offset%)
             ldwrky)
            (dgemm "No transpose" "Transpose"
             (f2cl-lib:int-add (f2cl-lib:int-sub m i nb) 1)
             (f2cl-lib:int-add (f2cl-lib:int-sub n i nb) 1) nb (- one)
             (f2cl-lib:array-slice a-%data%
                                   double-float
                                   ((+ i nb) i)
                                   ((1 lda) (1 *))
                                   a-%offset%)
             lda
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   ((+ (f2cl-lib:int-mul ldwrkx nb) nb 1))
                                   ((1 *))
                                   work-%offset%)
             ldwrky one
             (f2cl-lib:array-slice a-%data%
                                   double-float
                                   ((+ i nb) (f2cl-lib:int-add i nb))
                                   ((1 lda) (1 *))
                                   a-%offset%)
             lda)
            (dgemm "No transpose" "No transpose"
             (f2cl-lib:int-add (f2cl-lib:int-sub m i nb) 1)
             (f2cl-lib:int-add (f2cl-lib:int-sub n i nb) 1) nb (- one)
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   ((+ nb 1))
                                   ((1 *))
                                   work-%offset%)
             ldwrkx
             (f2cl-lib:array-slice a-%data%
                                   double-float
                                   (i (f2cl-lib:int-add i nb))
                                   ((1 lda) (1 *))
                                   a-%offset%)
             lda one
             (f2cl-lib:array-slice a-%data%
                                   double-float
                                   ((+ i nb) (f2cl-lib:int-add i nb))
                                   ((1 lda) (1 *))
                                   a-%offset%)
             lda)
            (cond
              ((>= m n)
               (f2cl-lib:fdo (j i (f2cl-lib:int-add j 1))
                             ((> j
                                 (f2cl-lib:int-add i nb (f2cl-lib:int-sub 1)))
                              nil)
                 (tagbody
                   (setf (f2cl-lib:fref a-%data%
                                        (j j)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                           (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%))
                   (setf (f2cl-lib:fref a-%data%
                                        (j (f2cl-lib:int-add j 1))
                                        ((1 lda) (1 *))
                                        a-%offset%)
                           (f2cl-lib:fref e-%data% (j) ((1 *)) e-%offset%))
                  label10)))
              (t
               (f2cl-lib:fdo (j i (f2cl-lib:int-add j 1))
                             ((> j
                                 (f2cl-lib:int-add i nb (f2cl-lib:int-sub 1)))
                              nil)
                 (tagbody
                   (setf (f2cl-lib:fref a-%data%
                                        (j j)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                           (f2cl-lib:fref d-%data% (j) ((1 *)) d-%offset%))
                   (setf (f2cl-lib:fref a-%data%
                                        ((f2cl-lib:int-add j 1) j)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                           (f2cl-lib:fref e-%data% (j) ((1 *)) e-%offset%))
                  label20))))
           label30))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
            (dgebd2 (f2cl-lib:int-add (f2cl-lib:int-sub m i) 1)
             (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1)
             (f2cl-lib:array-slice a-%data%
                                   double-float
                                   (i i)
                                   ((1 lda) (1 *))
                                   a-%offset%)
             lda
             (f2cl-lib:array-slice d-%data%
                                   double-float
                                   (i)
                                   ((1 *))
                                   d-%offset%)
             (f2cl-lib:array-slice e-%data%
                                   double-float
                                   (i)
                                   ((1 *))
                                   e-%offset%)
             (f2cl-lib:array-slice tauq-%data%
                                   double-float
                                   (i)
                                   ((1 *))
                                   tauq-%offset%)
             (f2cl-lib:array-slice taup-%data%
                                   double-float
                                   (i)
                                   ((1 *))
                                   taup-%offset%)
             work iinfo)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8))
          (setf iinfo var-9))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%) ws)
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgebrd
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dgebd2 fortran-to-lisp::dgemm
                    fortran-to-lisp::dlabrd fortran-to-lisp::xerbla
                    fortran-to-lisp::ilaenv))))

