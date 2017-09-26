;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 46c1f6a93b0d 2012/05/03 04:40:28 toy $"
;;;  "f2cl2.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 46c1f6a93b0d 2012/05/03 04:40:28 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v fceac530ef0c 2011/11/26 04:02:26 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2012-04 (20C Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((nti0 0)
      (xsml 0.0)
      (xmax 0.0)
      (bi0cs
       (make-array 18
                   :element-type 'double-float
                   :initial-contents '(-0.07660547252839145 1.9273379539938083
                                       0.22826445869203013 0.013048914667072904
                                       4.3442709008164877e-4
                                       9.422657686001934e-6
                                       1.434006289510691e-7
                                       1.613849069661749e-9
                                       1.3966500445356697e-11
                                       9.579451725505446e-14
                                       5.333981859862503e-16
                                       2.4587160884374706e-18
                                       9.53568089024877e-21
                                       3.154382039721427e-23
                                       9.004564101094637e-26
                                       2.24064736912367e-28
                                       4.9030346032428375e-31
                                       9.508172606122666e-34)))
      (first$ nil))
  (declare (type (f2cl-lib:integer4) nti0)
           (type (double-float) xsml xmax)
           (type (simple-array double-float (18)) bi0cs)
           (type f2cl-lib:logical first$))
  (setq first$ f2cl-lib:%true%)
  (defun dbesi0 (x)
    (declare (type (double-float) x))
    (prog ((y 0.0) (dbesi0 0.0))
      (declare (type (double-float) dbesi0 y))
      (cond
        (first$
         (setf nti0
                 (initds bi0cs 18
                  (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
         (setf xsml (f2cl-lib:fsqrt (* 4.5 (f2cl-lib:d1mach 3))))
         (setf xmax (f2cl-lib:flog (f2cl-lib:d1mach 2)))))
      (setf first$ f2cl-lib:%false%)
      (setf y (abs x))
      (if (> y 3.0) (go label20))
      (setf dbesi0 1.0)
      (if (> y xsml)
          (setf dbesi0 (+ 2.75 (dcsevl (- (/ (* y y) 4.5) 1.0) bi0cs nti0))))
      (go end_label)
     label20
      (if (> y xmax)
          (xermsg "SLATEC" "DBESI0" "ABS(X) SO BIG I0 OVERFLOWS" 2 2))
      (setf dbesi0 (* (exp y) (dbsi0e x)))
      (go end_label)
     end_label
      (return (values dbesi0 nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dbesi0
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::dbsi0e
                                                     fortran-to-lisp::xermsg
                                                     fortran-to-lisp::dcsevl
                                                     fortran-to-lisp::initds
                                                     fortran-to-lisp::d1mach))))

