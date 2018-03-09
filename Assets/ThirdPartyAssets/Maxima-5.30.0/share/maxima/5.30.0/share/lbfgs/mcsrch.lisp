;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.215 2009/04/07 22:05:21 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.200 2009/01/19 02:38:17 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.112 2009/01/08 12:57:19 rtoy Exp $")

;;; Using Lisp CMU Common Lisp 19f (19F)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :common-lisp-user)


(let ((dg 0.0)
      (dgm 0.0)
      (dginit 0.0)
      (dgtest 0.0)
      (dgx 0.0)
      (dgxm 0.0)
      (dgy 0.0)
      (dgym 0.0)
      (finit 0.0)
      (ftest1 0.0)
      (fm 0.0)
      (fx 0.0)
      (fxm 0.0)
      (fy 0.0)
      (fym 0.0)
      (p5 0.5)
      (p66 0.66)
      (stx 0.0)
      (sty 0.0)
      (stmin 0.0)
      (stmax 0.0)
      (width 0.0)
      (width1 0.0)
      (xtrapf 4.0)
      (zero 0.0)
      (brackt nil)
      (stage1 nil)
      (infoc 0)
      (j 0))
  (declare (type (double-float) dg dgm dginit dgtest dgx dgxm dgy dgym finit
                                ftest1 fm fx fxm fy fym p5 p66 stx sty stmin
                                stmax width width1 xtrapf zero)
           (type f2cl-lib:logical brackt stage1)
           (type (f2cl-lib:integer4) infoc j))
  (defun mcsrch (n x f g s stp ftol xtol maxfev info nfev wa)
    (declare (type (double-float) xtol ftol stp f)
             (type (array double-float (*)) wa s g x)
             (type (f2cl-lib:integer4) nfev info maxfev n))
    (let ()
      (symbol-macrolet ((stpmax (lb3-stpmax *lb3-common-block*))
                        (stpmin (lb3-stpmin *lb3-common-block*))
                        (gtol (lb3-gtol *lb3-common-block*))
                        (lp (lb3-lp *lb3-common-block*)))
        (f2cl-lib:with-multi-array-data
            ((x double-float x-%data% x-%offset%)
             (g double-float g-%data% g-%offset%)
             (s double-float s-%data% s-%offset%)
             (wa double-float wa-%data% wa-%offset%))
          (prog ()
            (declare)
            (if (= info -1) (go label45))
            (setf infoc 1)
            (if
             (or (<= n 0)
                 (<= stp zero)
                 (< ftol zero)
                 (< gtol zero)
                 (< xtol zero)
                 (< stpmin zero)
                 (< stpmax stpmin)
                 (<= maxfev 0))
             (go end_label))
            (setf dginit zero)
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (setf dginit
                        (+ dginit
                           (* (f2cl-lib:fref g-%data% (j) ((1 n)) g-%offset%)
                              (f2cl-lib:fref s-%data%
                                             (j)
                                             ((1 n))
                                             s-%offset%))))
               label10))
            (cond
              ((>= dginit zero)
               (f2cl-lib:fformat lp
                                 ("~%"
                                  "  THE SEARCH DIRECTION IS NOT A DESCENT DIRECTION"
                                  "~%"))
               (go end_label)))
            (setf brackt f2cl-lib:%false%)
            (setf stage1 f2cl-lib:%true%)
            (setf nfev 0)
            (setf finit f)
            (setf dgtest (* ftol dginit))
            (setf width (- stpmax stpmin))
            (setf width1 (/ width p5))
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (setf (f2cl-lib:fref wa-%data% (j) ((1 n)) wa-%offset%)
                        (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%))
               label20))
            (setf stx zero)
            (setf fx finit)
            (setf dgx dginit)
            (setf sty zero)
            (setf fy finit)
            (setf dgy dginit)
           label30
            (cond
              (brackt
               (setf stmin (min stx sty))
               (setf stmax (max stx sty)))
              (t
               (setf stmin stx)
               (setf stmax (+ stp (* xtrapf (- stp stx))))))
            (setf stp (max stp stpmin))
            (setf stp (min stp stpmax))
            (if
             (or (and brackt (or (<= stp stmin) (>= stp stmax)))
                 (>= nfev (f2cl-lib:int-sub maxfev 1))
                 (= infoc 0)
                 (and brackt (<= (- stmax stmin) (* xtol stmax))))
             (setf stp stx))
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (setf (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                        (+ (f2cl-lib:fref wa-%data% (j) ((1 n)) wa-%offset%)
                           (* stp
                              (f2cl-lib:fref s-%data%
                                             (j)
                                             ((1 n))
                                             s-%offset%))))
               label40))
            (setf info -1)
            (go end_label)
           label45
            (setf info 0)
            (setf nfev (f2cl-lib:int-add nfev 1))
            (setf dg zero)
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (setf dg
                        (+ dg
                           (* (f2cl-lib:fref g-%data% (j) ((1 n)) g-%offset%)
                              (f2cl-lib:fref s-%data%
                                             (j)
                                             ((1 n))
                                             s-%offset%))))
               label50))
            (setf ftest1 (+ finit (* stp dgtest)))
            (if
             (or (and brackt (or (<= stp stmin) (>= stp stmax))) (= infoc 0))
             (setf info 6))
            (if (and (= stp stpmax) (<= f ftest1) (<= dg dgtest))
                (setf info 5))
            (if (and (= stp stpmin) (or (> f ftest1) (>= dg dgtest)))
                (setf info 4))
            (if (>= nfev maxfev) (setf info 3))
            (if (and brackt (<= (- stmax stmin) (* xtol stmax))) (setf info 2))
            (if (and (<= f ftest1) (<= (f2cl-lib:dabs dg) (* gtol (- dginit))))
                (setf info 1))
            (if (/= info 0) (go end_label))
            (if (and stage1 (<= f ftest1) (>= dg (* (min ftol gtol) dginit)))
                (setf stage1 f2cl-lib:%false%))
            (cond
              ((and stage1 (<= f fx) (> f ftest1))
               (setf fm (- f (* stp dgtest)))
               (setf fxm (- fx (* stx dgtest)))
               (setf fym (- fy (* sty dgtest)))
               (setf dgm (- dg dgtest))
               (setf dgxm (- dgx dgtest))
               (setf dgym (- dgy dgtest))
               (multiple-value-bind
                     (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                      var-9 var-10 var-11 var-12)
                   (mcstep stx fxm dgxm sty fym dgym stp fm dgm brackt stmin
                    stmax infoc)
                 (declare (ignore var-7 var-8 var-10 var-11))
                 (setf stx var-0)
                 (setf fxm var-1)
                 (setf dgxm var-2)
                 (setf sty var-3)
                 (setf fym var-4)
                 (setf dgym var-5)
                 (setf stp var-6)
                 (setf brackt var-9)
                 (setf infoc var-12))
               (setf fx (+ fxm (* stx dgtest)))
               (setf fy (+ fym (* sty dgtest)))
               (setf dgx (+ dgxm dgtest))
               (setf dgy (+ dgym dgtest)))
              (t
               (multiple-value-bind
                     (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                      var-9 var-10 var-11 var-12)
                   (mcstep stx fx dgx sty fy dgy stp f dg brackt stmin stmax
                    infoc)
                 (declare (ignore var-7 var-8 var-10 var-11))
                 (setf stx var-0)
                 (setf fx var-1)
                 (setf dgx var-2)
                 (setf sty var-3)
                 (setf fy var-4)
                 (setf dgy var-5)
                 (setf stp var-6)
                 (setf brackt var-9)
                 (setf infoc var-12))))
            (cond
              (brackt
               (if (>= (f2cl-lib:dabs (- sty stx)) (* p66 width1))
                   (setf stp (+ stx (* p5 (- sty stx)))))
               (setf width1 width)
               (setf width (f2cl-lib:dabs (- sty stx)))))
            (go label30)
           end_label
            (return
             (values nil nil nil nil nil stp nil nil nil info nfev nil))))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::mcsrch
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (double-float) (array double-float (*))
                        (array double-float (*)) (double-float) (double-float)
                        (double-float) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil fortran-to-lisp::stp nil nil
                            nil fortran-to-lisp::info fortran-to-lisp::nfev
                            nil)
           :calls '(fortran-to-lisp::mcstep))))

