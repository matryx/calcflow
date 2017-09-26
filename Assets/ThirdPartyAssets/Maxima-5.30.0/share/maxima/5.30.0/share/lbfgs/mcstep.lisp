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


(defun mcstep (stx fx dx sty fy dy stp fp dp brackt stpmin stpmax info)
  (declare (type (f2cl-lib:integer4) info)
           (type f2cl-lib:logical brackt)
           (type (double-float) stpmax stpmin dp fp stp dy fy sty dx fx stx))
  (prog ((gamma 0.0) (p 0.0) (q 0.0) (r 0.0) (s 0.0) (sgnd 0.0) (stpc 0.0)
         (stpf 0.0) (stpq 0.0) (theta 0.0) (bound nil))
    (declare (type f2cl-lib:logical bound)
             (type (double-float) theta stpq stpf stpc sgnd s r q p gamma))
    (setf info 0)
    (if
     (or (and brackt (or (<= stp (min stx sty)) (>= stp (max stx sty))))
         (>= (* dx (- stp stx)) 0.0)
         (< stpmax stpmin))
     (go end_label))
    (setf sgnd (* dp (/ dx (f2cl-lib:dabs dx))))
    (cond
      ((> fp fx)
       (setf info 1)
       (setf bound f2cl-lib:%true%)
       (setf theta (+ (/ (* 3 (- fx fp)) (- stp stx)) dx dp))
       (setf s
               (max (f2cl-lib:dabs theta)
                    (f2cl-lib:dabs dx)
                    (f2cl-lib:dabs dp)))
       (setf gamma
               (* s
                  (f2cl-lib:dsqrt
                   (- (expt (/ theta s) 2) (* (/ dx s) (/ dp s))))))
       (if (< stp stx) (setf gamma (- gamma)))
       (setf p (+ (- gamma dx) theta))
       (setf q (+ (- gamma dx) gamma dp))
       (setf r (/ p q))
       (setf stpc (+ stx (* r (- stp stx))))
       (setf stpq
               (+ stx
                  (* (/ (/ dx (+ (/ (- fx fp) (- stp stx)) dx)) 2)
                     (- stp stx))))
       (cond
         ((< (f2cl-lib:dabs (+ stpc (- stx))) (f2cl-lib:dabs (+ stpq (- stx))))
          (setf stpf stpc))
         (t
          (setf stpf (+ stpc (/ (- stpq stpc) 2)))))
       (setf brackt f2cl-lib:%true%))
      ((< sgnd 0.0)
       (setf info 2)
       (setf bound f2cl-lib:%false%)
       (setf theta (+ (/ (* 3 (- fx fp)) (- stp stx)) dx dp))
       (setf s
               (max (f2cl-lib:dabs theta)
                    (f2cl-lib:dabs dx)
                    (f2cl-lib:dabs dp)))
       (setf gamma
               (* s
                  (f2cl-lib:dsqrt
                   (- (expt (/ theta s) 2) (* (/ dx s) (/ dp s))))))
       (if (> stp stx) (setf gamma (- gamma)))
       (setf p (+ (- gamma dp) theta))
       (setf q (+ (- gamma dp) gamma dx))
       (setf r (/ p q))
       (setf stpc (+ stp (* r (- stx stp))))
       (setf stpq (+ stp (* (/ dp (- dp dx)) (- stx stp))))
       (cond
         ((> (f2cl-lib:dabs (+ stpc (- stp))) (f2cl-lib:dabs (+ stpq (- stp))))
          (setf stpf stpc))
         (t
          (setf stpf stpq)))
       (setf brackt f2cl-lib:%true%))
      ((< (f2cl-lib:dabs dp) (f2cl-lib:dabs dx))
       (setf info 3)
       (setf bound f2cl-lib:%true%)
       (setf theta (+ (/ (* 3 (- fx fp)) (- stp stx)) dx dp))
       (setf s
               (max (f2cl-lib:dabs theta)
                    (f2cl-lib:dabs dx)
                    (f2cl-lib:dabs dp)))
       (setf gamma
               (* s
                  (f2cl-lib:dsqrt
                   (max 0.0 (- (expt (/ theta s) 2) (* (/ dx s) (/ dp s)))))))
       (if (> stp stx) (setf gamma (- gamma)))
       (setf p (+ (- gamma dp) theta))
       (setf q (+ gamma (- dx dp) gamma))
       (setf r (/ p q))
       (cond
         ((and (< r 0.0) (/= gamma 0.0))
          (setf stpc (+ stp (* r (- stx stp)))))
         ((> stp stx)
          (setf stpc stpmax))
         (t
          (setf stpc stpmin)))
       (setf stpq (+ stp (* (/ dp (- dp dx)) (- stx stp))))
       (cond
         (brackt
          (cond
            ((< (f2cl-lib:dabs (+ stp (- stpc)))
                (f2cl-lib:dabs (+ stp (- stpq))))
             (setf stpf stpc))
            (t
             (setf stpf stpq))))
         (t
          (cond
            ((> (f2cl-lib:dabs (+ stp (- stpc)))
                (f2cl-lib:dabs (+ stp (- stpq))))
             (setf stpf stpc))
            (t
             (setf stpf stpq))))))
      (t
       (setf info 4)
       (setf bound f2cl-lib:%false%)
       (cond
         (brackt
          (setf theta (+ (/ (* 3 (- fp fy)) (- sty stp)) dy dp))
          (setf s
                  (max (f2cl-lib:dabs theta)
                       (f2cl-lib:dabs dy)
                       (f2cl-lib:dabs dp)))
          (setf gamma
                  (* s
                     (f2cl-lib:dsqrt
                      (- (expt (/ theta s) 2) (* (/ dy s) (/ dp s))))))
          (if (> stp sty) (setf gamma (- gamma)))
          (setf p (+ (- gamma dp) theta))
          (setf q (+ (- gamma dp) gamma dy))
          (setf r (/ p q))
          (setf stpc (+ stp (* r (- sty stp))))
          (setf stpf stpc))
         ((> stp stx)
          (setf stpf stpmax))
         (t
          (setf stpf stpmin)))))
    (cond
      ((> fp fx)
       (setf sty stp)
       (setf fy fp)
       (setf dy dp))
      (t
       (cond
         ((< sgnd 0.0)
          (setf sty stx)
          (setf fy fx)
          (setf dy dx)))
       (setf stx stp)
       (setf fx fp)
       (setf dx dp)))
    (setf stpf (min stpmax stpf))
    (setf stpf (max stpmin stpf))
    (setf stp stpf)
    (cond
      ((and brackt bound)
       (cond
         ((> sty stx)
          (setf stp (min (+ stx (* 0.66 (- sty stx))) stp)))
         (t
          (setf stp (max (+ stx (* 0.66 (- sty stx))) stp))))))
    (go end_label)
   end_label
    (return (values stx fx dx sty fy dy stp nil nil brackt nil nil info))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::mcstep
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float)
                        fortran-to-lisp::logical (double-float) (double-float)
                        (fortran-to-lisp::integer4))
           :return-values '(fortran-to-lisp::stx fortran-to-lisp::fx
                            fortran-to-lisp::dx fortran-to-lisp::sty
                            fortran-to-lisp::fy fortran-to-lisp::dy
                            fortran-to-lisp::stp nil nil
                            fortran-to-lisp::brackt nil nil
                            fortran-to-lisp::info)
           :calls 'nil)))

