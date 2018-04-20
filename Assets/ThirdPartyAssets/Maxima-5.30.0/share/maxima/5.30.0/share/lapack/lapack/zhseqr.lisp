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


(let* ((ntiny 11)
       (nl 49)
       (zero (f2cl-lib:cmplx 0.0 0.0))
       (one (f2cl-lib:cmplx 1.0 0.0))
       (rzero 0.0))
  (declare (type (f2cl-lib:integer4 11 11) ntiny)
           (type (f2cl-lib:integer4 49 49) nl)
           (type (f2cl-lib:complex16) zero)
           (type (f2cl-lib:complex16) one)
           (type (double-float 0.0 0.0) rzero)
           (ignorable ntiny nl zero one rzero))
  (defun zhseqr (job compz n ilo ihi h ldh w z ldz work lwork info)
    (declare (type (array f2cl-lib:complex16 (*)) work z w h)
             (type (f2cl-lib:integer4) info lwork ldz ldh ihi ilo n)
             (type (simple-string *) compz job))
    (f2cl-lib:with-multi-array-data
        ((job character job-%data% job-%offset%)
         (compz character compz-%data% compz-%offset%)
         (h f2cl-lib:complex16 h-%data% h-%offset%)
         (w f2cl-lib:complex16 w-%data% w-%offset%)
         (z f2cl-lib:complex16 z-%data% z-%offset%)
         (work f2cl-lib:complex16 work-%data% work-%offset%))
      (prog ((initz nil) (lquery nil) (wantt nil) (wantz nil) (kbot 0) (nmin 0)
             (hl
              (make-array (the fixnum (reduce #'* (list nl nl)))
                          :element-type 'f2cl-lib:complex16))
             (workl (make-array nl :element-type 'f2cl-lib:complex16)))
        (declare (type f2cl-lib:logical initz lquery wantt wantz)
                 (type (f2cl-lib:integer4) kbot nmin)
                 (type (array f2cl-lib:complex16 (*)) hl workl))
        (setf wantt (lsame job "S"))
        (setf initz (lsame compz "I"))
        (setf wantz (or initz (lsame compz "V")))
        (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                (f2cl-lib:dcmplx
                 (f2cl-lib:dble
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
                 rzero))
        (setf lquery (coerce (= lwork -1) 'f2cl-lib:logical))
        (setf info 0)
        (cond
          ((and (not (lsame job "E")) (not wantt))
           (setf info -1))
          ((and (not (lsame compz "N")) (not wantz))
           (setf info -2))
          ((< n 0)
           (setf info -3))
          ((or (< ilo 1)
               (> ilo
                  (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n))))
           (setf info -4))
          ((or
            (< ihi (min (the f2cl-lib:integer4 ilo) (the f2cl-lib:integer4 n)))
            (> ihi n))
           (setf info -5))
          ((< ldh (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
           (setf info -7))
          ((or (< ldz 1)
               (and wantz
                    (< ldz
                       (max (the f2cl-lib:integer4 1)
                            (the f2cl-lib:integer4 n)))))
           (setf info -10))
          ((and
            (< lwork (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
            (not lquery))
           (setf info -12)))
        (cond
          ((/= info 0)
           (xerbla "ZHSEQR" (f2cl-lib:int-sub info))
           (go end_label))
          ((= n 0)
           (go end_label))
          (lquery
           (multiple-value-bind
                 (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                  var-10 var-11 var-12 var-13 var-14)
               (zlaqr0 wantt wantz n ilo ihi h ldh w ilo ihi z ldz work lwork
                info)
             (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                              var-8 var-9 var-10 var-11 var-12 var-13))
             (setf info var-14))
           (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                   (f2cl-lib:dcmplx
                    (max
                     (f2cl-lib:dble
                      (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%))
                     (f2cl-lib:dble
                      (max (the f2cl-lib:integer4 1)
                           (the f2cl-lib:integer4 n))))
                    rzero))
           (go end_label))
          (t
           (if (> ilo 1)
               (zcopy (f2cl-lib:int-sub ilo 1) h (f2cl-lib:int-add ldh 1) w 1))
           (if (< ihi n)
               (zcopy (f2cl-lib:int-sub n ihi)
                (f2cl-lib:array-slice h-%data%
                                      f2cl-lib:complex16
                                      ((+ ihi 1) (f2cl-lib:int-add ihi 1))
                                      ((1 ldh) (1 *))
                                      h-%offset%)
                (f2cl-lib:int-add ldh 1)
                (f2cl-lib:array-slice w-%data%
                                      f2cl-lib:complex16
                                      ((+ ihi 1))
                                      ((1 *))
                                      w-%offset%)
                1))
           (if initz (zlaset "A" n n zero one z ldz))
           (cond
             ((= ilo ihi)
              (setf (f2cl-lib:fref w-%data% (ilo) ((1 *)) w-%offset%)
                      (f2cl-lib:fref h-%data%
                                     (ilo ilo)
                                     ((1 ldh) (1 *))
                                     h-%offset%))
              (go end_label)))
           (setf nmin
                   (ilaenv 12 "ZHSEQR" (f2cl-lib:f2cl-// job compz) n ilo ihi
                    lwork))
           (setf nmin
                   (max (the f2cl-lib:integer4 ntiny)
                        (the f2cl-lib:integer4 nmin)))
           (cond
             ((> n nmin)
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14)
                  (zlaqr0 wantt wantz n ilo ihi h ldh w ilo ihi z ldz work
                   lwork info)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7 var-8 var-9 var-10 var-11 var-12
                                 var-13))
                (setf info var-14)))
             (t
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12)
                  (zlahqr wantt wantz n ilo ihi h ldh w ilo ihi z ldz info)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7 var-8 var-9 var-10 var-11))
                (setf info var-12))
              (cond
                ((> info 0)
                 (setf kbot info)
                 (cond
                   ((>= n nl)
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                        (zlaqr0 wantt wantz n ilo kbot h ldh w ilo ihi z ldz
                         work lwork info)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9 var-10 var-11
                                       var-12 var-13))
                      (setf info var-14)))
                   (t
                    (zlacpy "A" n n h ldh hl nl)
                    (setf (f2cl-lib:fref hl
                                         ((f2cl-lib:int-add n 1) n)
                                         ((1 nl) (1 nl)))
                            zero)
                    (zlaset "A" nl (f2cl-lib:int-sub nl n) zero zero
                     (f2cl-lib:array-slice hl
                                           f2cl-lib:complex16
                                           (1 (f2cl-lib:int-add n 1))
                                           ((1 nl) (1 nl)))
                     nl)
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                        (zlaqr0 wantt wantz nl ilo kbot hl nl w ilo ihi z ldz
                         workl nl info)
                      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                       var-6 var-7 var-8 var-9 var-10 var-11
                                       var-12 var-13))
                      (setf info var-14))
                    (if (or wantt (/= info 0))
                        (zlacpy "A" n n hl nl h ldh))))))))
           (if (and (or wantt (/= info 0)) (> n 2))
               (zlaset "L" (f2cl-lib:int-sub n 2) (f2cl-lib:int-sub n 2) zero
                zero
                (f2cl-lib:array-slice h-%data%
                                      f2cl-lib:complex16
                                      (3 1)
                                      ((1 ldh) (1 *))
                                      h-%offset%)
                ldh))
           (setf (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)
                   (f2cl-lib:dcmplx
                    (max
                     (f2cl-lib:dble
                      (max (the f2cl-lib:integer4 1)
                           (the f2cl-lib:integer4 n)))
                     (f2cl-lib:dble
                      (f2cl-lib:fref work-%data% (1) ((1 *)) work-%offset%)))
                    rzero))))
       end_label
        (return
         (values nil nil nil nil nil nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zhseqr
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (simple-string)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::info)
           :calls '(fortran-to-lisp::zlacpy fortran-to-lisp::zlahqr
                    fortran-to-lisp::ilaenv fortran-to-lisp::zlaset
                    fortran-to-lisp::zcopy fortran-to-lisp::zlaqr0
                    fortran-to-lisp::xerbla fortran-to-lisp::lsame))))

