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
  (defun dtrexc (compq n t$ ldt q ldq ifst ilst work info)
    (declare (type (array double-float (*)) work q t$)
             (type (f2cl-lib:integer4) info ilst ifst ldq ldt n)
             (type (simple-string *) compq))
    (f2cl-lib:with-multi-array-data
        ((compq character compq-%data% compq-%offset%)
         (t$ double-float t$-%data% t$-%offset%)
         (q double-float q-%data% q-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((here 0) (nbf 0) (nbl 0) (nbnext 0) (wantq nil))
        (declare (type (f2cl-lib:integer4) here nbf nbl nbnext)
                 (type f2cl-lib:logical wantq))
        (setf info 0)
        (setf wantq (lsame compq "V"))
        (cond
          ((and (not wantq) (not (lsame compq "N")))
           (setf info -1))
          ((< n 0)
           (setf info -2))
          ((< ldt (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
           (setf info -4))
          ((or (< ldq 1)
               (and wantq
                    (< ldq
                       (max (the f2cl-lib:integer4 1)
                            (the f2cl-lib:integer4 n)))))
           (setf info -6))
          ((or (< ifst 1) (> ifst n))
           (setf info -7))
          ((or (< ilst 1) (> ilst n))
           (setf info -8)))
        (cond
          ((/= info 0)
           (xerbla "DTREXC" (f2cl-lib:int-sub info))
           (go end_label)))
        (if (<= n 1) (go end_label))
        (cond
          ((> ifst 1)
           (if
            (/=
             (f2cl-lib:fref t$-%data%
                            (ifst (f2cl-lib:int-sub ifst 1))
                            ((1 ldt) (1 *))
                            t$-%offset%)
             zero)
            (setf ifst (f2cl-lib:int-sub ifst 1)))))
        (setf nbf 1)
        (cond
          ((< ifst n)
           (if
            (/=
             (f2cl-lib:fref t$-%data%
                            ((f2cl-lib:int-add ifst 1) ifst)
                            ((1 ldt) (1 *))
                            t$-%offset%)
             zero)
            (setf nbf 2))))
        (cond
          ((> ilst 1)
           (if
            (/=
             (f2cl-lib:fref t$-%data%
                            (ilst (f2cl-lib:int-sub ilst 1))
                            ((1 ldt) (1 *))
                            t$-%offset%)
             zero)
            (setf ilst (f2cl-lib:int-sub ilst 1)))))
        (setf nbl 1)
        (cond
          ((< ilst n)
           (if
            (/=
             (f2cl-lib:fref t$-%data%
                            ((f2cl-lib:int-add ilst 1) ilst)
                            ((1 ldt) (1 *))
                            t$-%offset%)
             zero)
            (setf nbl 2))))
        (if (= ifst ilst) (go end_label))
        (cond
          ((< ifst ilst)
           (tagbody
             (if (and (= nbf 2) (= nbl 1))
                 (setf ilst (f2cl-lib:int-sub ilst 1)))
             (if (and (= nbf 1) (= nbl 2))
                 (setf ilst (f2cl-lib:int-add ilst 1)))
             (setf here ifst)
            label10
             (cond
               ((or (= nbf 1) (= nbf 2))
                (setf nbnext 1)
                (cond
                  ((<= (f2cl-lib:int-add here nbf 1) n)
                   (if
                    (/=
                     (f2cl-lib:fref t$-%data%
                                    ((f2cl-lib:int-add here nbf 1)
                                     (f2cl-lib:int-add here nbf))
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
                     zero)
                    (setf nbnext 2))))
                (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                    (dlaexc wantq n t$ ldt q ldq here nbf nbnext work info)
                  (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                   var-7 var-8 var-9))
                  (setf info var-10))
                (cond
                  ((/= info 0)
                   (setf ilst here)
                   (go end_label)))
                (setf here (f2cl-lib:int-add here nbnext))
                (cond
                  ((= nbf 2)
                   (if
                    (=
                     (f2cl-lib:fref t$-%data%
                                    ((f2cl-lib:int-add here 1) here)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
                     zero)
                    (setf nbf 3)))))
               (t
                (setf nbnext 1)
                (cond
                  ((<= (f2cl-lib:int-add here 3) n)
                   (if
                    (/=
                     (f2cl-lib:fref t$-%data%
                                    ((f2cl-lib:int-add here 3)
                                     (f2cl-lib:int-add here 2))
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
                     zero)
                    (setf nbnext 2))))
                (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                    (dlaexc wantq n t$ ldt q ldq (f2cl-lib:int-add here 1) 1
                     nbnext work info)
                  (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                   var-7 var-8 var-9))
                  (setf info var-10))
                (cond
                  ((/= info 0)
                   (setf ilst here)
                   (go end_label)))
                (cond
                  ((= nbnext 1)
                   (multiple-value-bind
                         (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                          var-9 var-10)
                       (dlaexc wantq n t$ ldt q ldq here 1 nbnext work info)
                     (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                      var-7 var-8 var-9))
                     (setf info var-10))
                   (setf here (f2cl-lib:int-add here 1)))
                  (t
                   (if
                    (=
                     (f2cl-lib:fref t$-%data%
                                    ((f2cl-lib:int-add here 2)
                                     (f2cl-lib:int-add here 1))
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
                     zero)
                    (setf nbnext 1))
                   (cond
                     ((= nbnext 2)
                      (multiple-value-bind
                            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-10)
                          (dlaexc wantq n t$ ldt q ldq here 1 nbnext work info)
                        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                         var-6 var-7 var-8 var-9))
                        (setf info var-10))
                      (cond
                        ((/= info 0)
                         (setf ilst here)
                         (go end_label)))
                      (setf here (f2cl-lib:int-add here 2)))
                     (t
                      (multiple-value-bind
                            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-10)
                          (dlaexc wantq n t$ ldt q ldq here 1 1 work info)
                        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                         var-6 var-7 var-8 var-9))
                        (setf info var-10))
                      (multiple-value-bind
                            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-10)
                          (dlaexc wantq n t$ ldt q ldq
                           (f2cl-lib:int-add here 1) 1 1 work info)
                        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                         var-6 var-7 var-8 var-9))
                        (setf info var-10))
                      (setf here (f2cl-lib:int-add here 2))))))))
             (if (< here ilst) (go label10))))
          (t
           (tagbody
             (setf here ifst)
            label20
             (cond
               ((or (= nbf 1) (= nbf 2))
                (setf nbnext 1)
                (cond
                  ((>= here 3)
                   (if
                    (/=
                     (f2cl-lib:fref t$-%data%
                                    ((f2cl-lib:int-sub here 1)
                                     (f2cl-lib:int-sub here 2))
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
                     zero)
                    (setf nbnext 2))))
                (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                    (dlaexc wantq n t$ ldt q ldq (f2cl-lib:int-sub here nbnext)
                     nbnext nbf work info)
                  (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                   var-7 var-8 var-9))
                  (setf info var-10))
                (cond
                  ((/= info 0)
                   (setf ilst here)
                   (go end_label)))
                (setf here (f2cl-lib:int-sub here nbnext))
                (cond
                  ((= nbf 2)
                   (if
                    (=
                     (f2cl-lib:fref t$-%data%
                                    ((f2cl-lib:int-add here 1) here)
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
                     zero)
                    (setf nbf 3)))))
               (t
                (setf nbnext 1)
                (cond
                  ((>= here 3)
                   (if
                    (/=
                     (f2cl-lib:fref t$-%data%
                                    ((f2cl-lib:int-sub here 1)
                                     (f2cl-lib:int-sub here 2))
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
                     zero)
                    (setf nbnext 2))))
                (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                       var-9 var-10)
                    (dlaexc wantq n t$ ldt q ldq (f2cl-lib:int-sub here nbnext)
                     nbnext 1 work info)
                  (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                   var-7 var-8 var-9))
                  (setf info var-10))
                (cond
                  ((/= info 0)
                   (setf ilst here)
                   (go end_label)))
                (cond
                  ((= nbnext 1)
                   (multiple-value-bind
                         (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                          var-9 var-10)
                       (dlaexc wantq n t$ ldt q ldq here nbnext 1 work info)
                     (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                      var-7 var-8 var-9))
                     (setf info var-10))
                   (setf here (f2cl-lib:int-sub here 1)))
                  (t
                   (if
                    (=
                     (f2cl-lib:fref t$-%data%
                                    (here (f2cl-lib:int-sub here 1))
                                    ((1 ldt) (1 *))
                                    t$-%offset%)
                     zero)
                    (setf nbnext 1))
                   (cond
                     ((= nbnext 2)
                      (multiple-value-bind
                            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-10)
                          (dlaexc wantq n t$ ldt q ldq
                           (f2cl-lib:int-sub here 1) 2 1 work info)
                        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                         var-6 var-7 var-8 var-9))
                        (setf info var-10))
                      (cond
                        ((/= info 0)
                         (setf ilst here)
                         (go end_label)))
                      (setf here (f2cl-lib:int-sub here 2)))
                     (t
                      (multiple-value-bind
                            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-10)
                          (dlaexc wantq n t$ ldt q ldq here 1 1 work info)
                        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                         var-6 var-7 var-8 var-9))
                        (setf info var-10))
                      (multiple-value-bind
                            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-10)
                          (dlaexc wantq n t$ ldt q ldq
                           (f2cl-lib:int-sub here 1) 1 1 work info)
                        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5
                                         var-6 var-7 var-8 var-9))
                        (setf info var-10))
                      (setf here (f2cl-lib:int-sub here 2))))))))
             (if (> here ilst) (go label20)))))
        (setf ilst here)
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil ifst ilst nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dtrexc
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-string) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil fortran-to-lisp::ifst
                            fortran-to-lisp::ilst nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dlaexc fortran-to-lisp::xerbla
                    fortran-to-lisp::lsame))))

