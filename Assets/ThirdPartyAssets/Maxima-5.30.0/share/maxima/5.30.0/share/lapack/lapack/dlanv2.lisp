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


(let* ((zero 0.0) (half 0.5) (one 1.0) (multpl 4.0))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 0.5 0.5) half)
           (type (double-float 1.0 1.0) one)
           (type (double-float 4.0 4.0) multpl)
           (ignorable zero half one multpl))
  (defun dlanv2 (a b c d rt1r rt1i rt2r rt2i cs sn)
    (declare (type (double-float) sn cs rt2i rt2r rt1i rt1r d c b a))
    (prog ((aa 0.0) (bb 0.0) (bcmax 0.0) (bcmis 0.0) (cc 0.0) (cs1 0.0)
           (dd 0.0) (eps 0.0) (p 0.0) (sab 0.0) (sac 0.0) (scale 0.0)
           (sigma 0.0) (sn1 0.0) (tau 0.0) (temp 0.0) (z 0.0))
      (declare (type (double-float) aa bb bcmax bcmis cc cs1 dd eps p sab sac
                                    scale sigma sn1 tau temp z))
      (setf eps (dlamch "P"))
      (cond
        ((= c zero)
         (setf cs one)
         (setf sn zero)
         (go label10))
        ((= b zero)
         (setf cs zero)
         (setf sn one)
         (setf temp d)
         (setf d a)
         (setf a temp)
         (setf b (- c))
         (setf c zero)
         (go label10))
        ((and (= (+ a (- d)) zero)
              (/= (f2cl-lib:sign one b) (f2cl-lib:sign one c)))
         (setf cs one)
         (setf sn zero)
         (go label10))
        (t
         (setf temp (- a d))
         (setf p (* half temp))
         (setf bcmax (max (abs b) (abs c)))
         (setf bcmis
                 (* (min (abs b) (abs c))
                    (f2cl-lib:sign one b)
                    (f2cl-lib:sign one c)))
         (setf scale (max (abs p) bcmax))
         (setf z (+ (* (/ p scale) p) (* (/ bcmax scale) bcmis)))
         (cond
           ((>= z (* multpl eps))
            (setf z
                    (+ p
                       (f2cl-lib:sign
                        (* (f2cl-lib:fsqrt scale) (f2cl-lib:fsqrt z))
                        p)))
            (setf a (+ d z))
            (setf d (- d (* (/ bcmax z) bcmis)))
            (setf tau (dlapy2 c z))
            (setf cs (/ z tau))
            (setf sn (/ c tau))
            (setf b (- b c))
            (setf c zero))
           (t
            (setf sigma (+ b c))
            (setf tau (dlapy2 sigma temp))
            (setf cs (f2cl-lib:fsqrt (* half (+ one (/ (abs sigma) tau)))))
            (setf sn (* (- (/ p (* tau cs))) (f2cl-lib:sign one sigma)))
            (setf aa (+ (* a cs) (* b sn)))
            (setf bb (+ (* (- a) sn) (* b cs)))
            (setf cc (+ (* c cs) (* d sn)))
            (setf dd (+ (* (- c) sn) (* d cs)))
            (setf a (+ (* aa cs) (* cc sn)))
            (setf b (+ (* bb cs) (* dd sn)))
            (setf c (+ (* (- aa) sn) (* cc cs)))
            (setf d (+ (* (- bb) sn) (* dd cs)))
            (setf temp (* half (+ a d)))
            (setf a temp)
            (setf d temp)
            (cond
              ((/= c zero)
               (cond
                 ((/= b zero)
                  (cond
                    ((= (f2cl-lib:sign one b) (f2cl-lib:sign one c))
                     (setf sab (f2cl-lib:fsqrt (abs b)))
                     (setf sac (f2cl-lib:fsqrt (abs c)))
                     (setf p (f2cl-lib:sign (* sab sac) c))
                     (setf tau (/ one (f2cl-lib:fsqrt (abs (+ b c)))))
                     (setf a (+ temp p))
                     (setf d (- temp p))
                     (setf b (- b c))
                     (setf c zero)
                     (setf cs1 (* sab tau))
                     (setf sn1 (* sac tau))
                     (setf temp (- (* cs cs1) (* sn sn1)))
                     (setf sn (+ (* cs sn1) (* sn cs1)))
                     (setf cs temp))))
                 (t
                  (setf b (- c))
                  (setf c zero)
                  (setf temp cs)
                  (setf cs (- sn))
                  (setf sn temp)))))))))
     label10
      (setf rt1r a)
      (setf rt2r d)
      (cond
        ((= c zero)
         (setf rt1i zero)
         (setf rt2i zero))
        (t
         (setf rt1i (* (f2cl-lib:fsqrt (abs b)) (f2cl-lib:fsqrt (abs c))))
         (setf rt2i (- rt1i))))
      (go end_label)
     end_label
      (return (values a b c d rt1r rt1i rt2r rt2i cs sn)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlanv2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float)
                        (double-float))
           :return-values '(fortran-to-lisp::a fortran-to-lisp::b
                            fortran-to-lisp::c fortran-to-lisp::d
                            fortran-to-lisp::rt1r fortran-to-lisp::rt1i
                            fortran-to-lisp::rt2r fortran-to-lisp::rt2i
                            fortran-to-lisp::cs fortran-to-lisp::sn)
           :calls '(fortran-to-lisp::dlapy2 fortran-to-lisp::dlamch))))

