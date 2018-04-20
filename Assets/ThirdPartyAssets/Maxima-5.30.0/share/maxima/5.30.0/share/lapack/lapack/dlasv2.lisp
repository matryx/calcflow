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


(let* ((zero 0.0) (half 0.5) (one 1.0) (two 2.0) (four 4.0))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 0.5 0.5) half)
           (type (double-float 1.0 1.0) one)
           (type (double-float 2.0 2.0) two)
           (type (double-float 4.0 4.0) four)
           (ignorable zero half one two four))
  (defun dlasv2 (f g h ssmin ssmax snr csr snl csl)
    (declare (type (double-float) csl snl csr snr ssmax ssmin h g f))
    (prog ((a 0.0) (clt 0.0) (crt 0.0) (d 0.0) (fa 0.0) (ft 0.0) (ga 0.0)
           (gt 0.0) (ha 0.0) (ht 0.0) (l 0.0) (m 0.0) (mm 0.0) (r 0.0) (s 0.0)
           (slt 0.0) (srt 0.0) (t$ 0.0) (temp 0.0) (tsign 0.0) (tt 0.0)
           (pmax 0) (gasmal nil) (swap nil))
      (declare (type (double-float) a clt crt d fa ft ga gt ha ht l m mm r s
                                    slt srt t$ temp tsign tt)
               (type (f2cl-lib:integer4) pmax)
               (type f2cl-lib:logical gasmal swap))
      (setf ft f)
      (setf fa (abs ft))
      (setf ht h)
      (setf ha (abs h))
      (setf pmax 1)
      (setf swap (> ha fa))
      (cond
        (swap
         (setf pmax 3)
         (setf temp ft)
         (setf ft ht)
         (setf ht temp)
         (setf temp fa)
         (setf fa ha)
         (setf ha temp)))
      (setf gt g)
      (setf ga (abs gt))
      (cond
        ((= ga zero)
         (setf ssmin ha)
         (setf ssmax fa)
         (setf clt one)
         (setf crt one)
         (setf slt zero)
         (setf srt zero))
        (t
         (setf gasmal f2cl-lib:%true%)
         (cond
           ((> ga fa)
            (setf pmax 2)
            (cond
              ((< (f2cl-lib:f2cl/ fa ga) (dlamch "EPS"))
               (setf gasmal f2cl-lib:%false%)
               (setf ssmax ga)
               (cond
                 ((> ha one)
                  (setf ssmin (/ fa (/ ga ha))))
                 (t
                  (setf ssmin (* (/ fa ga) ha))))
               (setf clt one)
               (setf slt (/ ht gt))
               (setf srt one)
               (setf crt (/ ft gt))))))
         (cond
           (gasmal
            (setf d (- fa ha))
            (cond
              ((= d fa)
               (setf l one))
              (t
               (setf l (/ d fa))))
            (setf m (/ gt ft))
            (setf t$ (- two l))
            (setf mm (* m m))
            (setf tt (* t$ t$))
            (setf s (f2cl-lib:fsqrt (+ tt mm)))
            (cond
              ((= l zero)
               (setf r (abs m)))
              (t
               (setf r (f2cl-lib:fsqrt (+ (* l l) mm)))))
            (setf a (* half (+ s r)))
            (setf ssmin (/ ha a))
            (setf ssmax (* fa a))
            (cond
              ((= mm zero)
               (cond
                 ((= l zero)
                  (setf t$ (* (f2cl-lib:sign two ft) (f2cl-lib:sign one gt))))
                 (t
                  (setf t$ (+ (/ gt (f2cl-lib:sign d ft)) (/ m t$))))))
              (t
               (setf t$ (* (+ (/ m (+ s t$)) (/ m (+ r l))) (+ one a)))))
            (setf l (f2cl-lib:fsqrt (+ (* t$ t$) four)))
            (setf crt (/ two l))
            (setf srt (/ t$ l))
            (setf clt (/ (+ crt (* srt m)) a))
            (setf slt (/ (* (/ ht ft) srt) a))))))
      (cond
        (swap
         (setf csl srt)
         (setf snl crt)
         (setf csr slt)
         (setf snr clt))
        (t
         (setf csl clt)
         (setf snl slt)
         (setf csr crt)
         (setf snr srt)))
      (if (= pmax 1)
          (setf tsign
                  (* (f2cl-lib:sign one csr)
                     (f2cl-lib:sign one csl)
                     (f2cl-lib:sign one f))))
      (if (= pmax 2)
          (setf tsign
                  (* (f2cl-lib:sign one snr)
                     (f2cl-lib:sign one csl)
                     (f2cl-lib:sign one g))))
      (if (= pmax 3)
          (setf tsign
                  (* (f2cl-lib:sign one snr)
                     (f2cl-lib:sign one snl)
                     (f2cl-lib:sign one h))))
      (setf ssmax (f2cl-lib:sign ssmax tsign))
      (setf ssmin
              (f2cl-lib:sign ssmin
                             (* tsign
                                (f2cl-lib:sign one f)
                                (f2cl-lib:sign one h))))
      (go end_label)
     end_label
      (return (values nil nil nil ssmin ssmax snr csr snl csl)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlasv2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float))
           :return-values '(nil nil nil fortran-to-lisp::ssmin
                            fortran-to-lisp::ssmax fortran-to-lisp::snr
                            fortran-to-lisp::csr fortran-to-lisp::snl
                            fortran-to-lisp::csl)
           :calls '(fortran-to-lisp::dlamch))))

