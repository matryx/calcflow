;;; Compiled by f2cl version 2.0 beta Date: 2006/01/11 22:57:58 
;;; Using Lisp SBCL 0.9.9
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package :common-lisp-user)


(defun fgcompute (f g x n)
  (declare (type f2cl-lib:integer4 n)
           (type (array double-float (*)) x g)
           (type double-float f))
  (f2cl-lib:with-multi-array-data
      ((g double-float g-%data% g-%offset%)
       (x double-float x-%data% x-%offset%))
    (prog ((t1 0.0d0) (t2 0.0d0) (j 0))
      (declare (type f2cl-lib:integer4 j) (type double-float t2 t1))
      (setf f 0.0d0)
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 2))
                    ((> j n) nil)
        (tagbody
          (setf t1 (- 1.0d0 (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)))
          (setf t2
                  (* 10.0d0
                     (-
                      (f2cl-lib:fref x-%data%
                                     ((f2cl-lib:int-add j 1))
                                     ((1 n))
                                     x-%offset%)
                      (expt (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                            2))))
          (f2cl-lib:fset
           (f2cl-lib:fref g-%data% ((f2cl-lib:int-add j 1)) ((1 n)) g-%offset%)
           (* 20.0d0 t2))
          (f2cl-lib:fset (f2cl-lib:fref g-%data% (j) ((1 n)) g-%offset%)
                         (* -2.0d0
                            (+
                             (* (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                                (f2cl-lib:fref g-%data%
                                               ((f2cl-lib:int-add j 1))
                                               ((1 n))
                                               g-%offset%))
                             t1)))
          (setf f (+ f (expt t1 2) (expt t2 2)))
         label30))
      (go end_label)
     end_label
      (return (values f nil nil nil)))))

;;; Compiled by f2cl version 2.0 beta Date: 2006/01/11 22:57:58 
;;; Using Lisp SBCL 0.9.9
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package :common-lisp-user)


(let* ((ndim 2000)
       (msave 7)
       (nwork (+ (* ndim (+ (* 2 msave) 1)) (* 2 msave)))
       (nfevalmax 42))
  (declare (type f2cl-lib:integer4 ndim))
  (declare (type f2cl-lib:integer4 msave))
  (declare (type f2cl-lib:integer4 nwork))
  (declare (type f2cl-lib:integer4 nfevalmax))
  (defun sdrive ()
    (let ()
      (symbol-macrolet ((stpmax (lb3-stpmax *lb3-common-block*))
                        (stpmin (lb3-stpmin *lb3-common-block*))
                        (gtol (lb3-gtol *lb3-common-block*))
                        (lp (lb3-lp *lb3-common-block*))
                        (mp (lb3-mp *lb3-common-block*)))
        (f2cl-lib:with-multi-array-data
            nil
          (prog ((scache (make-array ndim :element-type 'double-float))
                 (w (make-array nwork :element-type 'double-float))
                 (diag (make-array ndim :element-type 'double-float))
                 (g (make-array ndim :element-type 'double-float))
                 (x (make-array ndim :element-type 'double-float)) (t2 0.0d0)
                 (t1 0.0d0) (xtol 0.0d0) (eps 0.0d0) (f 0.0d0) (j 0) (m 0)
                 (n 0) (icall 0) (iflag 0)
                 (iprint (make-array 2 :element-type 'f2cl-lib:integer4))
                 (diagco nil))
            (declare (type (array double-float (*)) scache w diag g x)
                     (type double-float t2 t1 xtol eps f)
                     (type f2cl-lib:integer4 j m n icall iflag)
                     (type (array f2cl-lib:integer4 (2)) iprint)
                     (type f2cl-lib:logical diagco))
            (setf n 100)
            (setf m 5)
            (f2cl-lib:fset (f2cl-lib:fref iprint (1) ((1 2))) 1)
            (f2cl-lib:fset (f2cl-lib:fref iprint (2) ((1 2))) 0)
            (setf diagco f2cl-lib:%false%)
            (setf eps 1.d-5)
            (setf xtol 1.d-16)
            (setf icall 0)
            (setf iflag 0)
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 2))
                          ((> j n) nil)
              (tagbody
                (f2cl-lib:fset (f2cl-lib:fref x (j) ((1 ndim))) -1.2d0)
                (f2cl-lib:fset
                 (f2cl-lib:fref x ((f2cl-lib:int-add j 1)) ((1 ndim)))
                 1.0d0)
               label10))
           label20
            (multiple-value-bind
                (var-0 var-1 var-2 var-3)
                (fgcompute f g x n)
              (declare (ignore var-1 var-2 var-3))
              (setf f var-0))
            (multiple-value-bind
                (var-0 var-1
                       var-2
                       var-3
                       var-4
                       var-5
                       var-6
                       var-7
                       var-8
                       var-9
                       var-10
                       var-11
                       var-12)
                (lbfgs n m x f g diagco diag iprint eps xtol w iflag scache)
              (declare
               (ignore var-2 var-4 var-5 var-6 var-7 var-8 var-10 var-12))
              (setf n var-0)
              (setf m var-1)
              (setf f var-3)
              (setf xtol var-9)
              (setf iflag var-11))
            (if (<= iflag 0) (go label50))
            (setf icall (f2cl-lib:int-add icall 1))
            (if (>= icall nfevalmax) (go label50))
            (go label20)
           label50
            (f2cl-lib:fformat 6
                              ("SEARCH TERMINATED AFTER " 1 (("~4D"))
                               " FUNCTION EVALUATIONS" " (LIMIT: " 1 (("~4D"))
                               ")" "~%" "CURRENT SOLUTION VECTOR: " "~%")
                              icall
                              nfevalmax)
            (f2cl-lib:fformat 6
                              (4 ("~2@T" 1 (("~22,15,2,1,'*,,'DE"))) "~%")
                              (do ((i 1 (f2cl-lib:int-add i 1))
                                   (ret nil
                                        (append ret
                                                (list
                                                 (f2cl-lib:fref x
                                                                (i)
                                                                ((1 ndim)))))))
                                  ((> i n) ret)
                                (declare (type f2cl-lib:integer4 i))))
            (f2cl-lib:fformat 6 ("SOLUTION CACHE: " "~%") nil)
            (f2cl-lib:fformat 6
                              (4 ("~2@T" 1 (("~22,15,2,1,'*,,'DE"))) "~%")
                              (do ((i 1 (f2cl-lib:int-add i 1))
                                   (ret nil
                                        (append ret
                                                (list
                                                 (f2cl-lib:fref scache
                                                                (i)
                                                                ((1 ndim)))))))
                                  ((> i n) ret)
                                (declare (type f2cl-lib:integer4 i))))
            (multiple-value-bind
                (var-0 var-1 var-2 var-3)
                (fgcompute f g x n)
              (declare (ignore var-1 var-2 var-3))
              (setf f var-0))
            (f2cl-lib:fformat 6
                              ("F(CURRENT SOLUTION VECTOR) = " 1
                               (("~22,15,2,1,'*,,'DE")) "~%")
                              f)
            (multiple-value-bind
                (var-0 var-1 var-2 var-3)
                (fgcompute f g scache n)
              (declare (ignore var-1 var-2 var-3))
              (setf f var-0))
            (f2cl-lib:fformat 6
                              ("F(SOLUTION CACHE) = " 1
                               (("~22,15,2,1,'*,,'DE")) "~%")
                              f)
           end_label
            (return nil)))))))

