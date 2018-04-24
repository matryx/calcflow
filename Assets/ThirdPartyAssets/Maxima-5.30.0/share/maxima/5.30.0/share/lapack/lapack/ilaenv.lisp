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


(defun ilaenv (ispec name opts n1 n2 n3 n4)
  (declare (type (simple-string *) opts name)
           (type (f2cl-lib:integer4) n4 n3 n2 n1 ispec))
  (f2cl-lib:with-multi-array-data
      ((name character name-%data% name-%offset%)
       (opts character opts-%data% opts-%offset%))
    (prog ((i 0) (ic 0) (iz 0) (nb 0) (nbmin 0) (nx 0)
           (subnam
            (make-array '(6) :element-type 'character :initial-element #\ ))
           (c3 (make-array '(3) :element-type 'character :initial-element #\ ))
           (c2 (make-array '(2) :element-type 'character :initial-element #\ ))
           (c4 (make-array '(2) :element-type 'character :initial-element #\ ))
           (c1 (make-array '(1) :element-type 'character :initial-element #\ ))
           (cname nil) (sname nil) (ilaenv 0))
      (declare (type f2cl-lib:logical sname cname)
               (type (simple-string 1) c1)
               (type (simple-string 2) c4 c2)
               (type (simple-string 3) c3)
               (type (simple-string 6) subnam)
               (type (f2cl-lib:integer4) ilaenv nx nbmin nb iz ic i))
      (f2cl-lib:computed-goto
       (label100 label100 label100 label400 label500 label600 label700 label800
        label900 label1000 label1100)
       ispec)
      (setf ilaenv -1)
      (go end_label)
     label100
      (setf ilaenv 1)
      (f2cl-lib:f2cl-set-string subnam name (string 6))
      (setf ic (f2cl-lib:ichar (f2cl-lib:fref-string subnam (1 1))))
      (setf iz (f2cl-lib:ichar "Z"))
      (cond
        ((or (= iz 90) (= iz 122))
         (cond
           ((and (>= ic 97) (<= ic 122))
            (f2cl-lib:fset-string (f2cl-lib:fref-string subnam (1 1))
                                  (f2cl-lib:fchar (f2cl-lib:int-sub ic 32)))
            (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                          ((> i 6) nil)
              (tagbody
                (setf ic (f2cl-lib:ichar (f2cl-lib:fref-string subnam (i i))))
                (if (and (>= ic 97) (<= ic 122))
                    (f2cl-lib:fset-string (f2cl-lib:fref-string subnam (i i))
                                          (f2cl-lib:fchar
                                           (f2cl-lib:int-sub ic 32))))
               label10)))))
        ((or (= iz 233) (= iz 169))
         (cond
           ((or (and (>= ic 129) (<= ic 137))
                (and (>= ic 145) (<= ic 153))
                (and (>= ic 162) (<= ic 169)))
            (f2cl-lib:fset-string (f2cl-lib:fref-string subnam (1 1))
                                  (f2cl-lib:fchar (f2cl-lib:int-add ic 64)))
            (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                          ((> i 6) nil)
              (tagbody
                (setf ic (f2cl-lib:ichar (f2cl-lib:fref-string subnam (i i))))
                (if
                 (or (and (>= ic 129) (<= ic 137))
                     (and (>= ic 145) (<= ic 153))
                     (and (>= ic 162) (<= ic 169)))
                 (f2cl-lib:fset-string (f2cl-lib:fref-string subnam (i i))
                                       (f2cl-lib:fchar
                                        (f2cl-lib:int-add ic 64))))
               label20)))))
        ((or (= iz 218) (= iz 250))
         (cond
           ((and (>= ic 225) (<= ic 250))
            (f2cl-lib:fset-string (f2cl-lib:fref-string subnam (1 1))
                                  (f2cl-lib:fchar (f2cl-lib:int-sub ic 32)))
            (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                          ((> i 6) nil)
              (tagbody
                (setf ic (f2cl-lib:ichar (f2cl-lib:fref-string subnam (i i))))
                (if (and (>= ic 225) (<= ic 250))
                    (f2cl-lib:fset-string (f2cl-lib:fref-string subnam (i i))
                                          (f2cl-lib:fchar
                                           (f2cl-lib:int-sub ic 32))))
               label30))))))
      (f2cl-lib:f2cl-set-string c1
                                (f2cl-lib:fref-string subnam (1 1))
                                (string 1))
      (setf sname (or (f2cl-lib:fstring-= c1 "S") (f2cl-lib:fstring-= c1 "D")))
      (setf cname (or (f2cl-lib:fstring-= c1 "C") (f2cl-lib:fstring-= c1 "Z")))
      (if (not (or cname sname)) (go end_label))
      (f2cl-lib:f2cl-set-string c2
                                (f2cl-lib:fref-string subnam (2 3))
                                (string 2))
      (f2cl-lib:f2cl-set-string c3
                                (f2cl-lib:fref-string subnam (4 6))
                                (string 3))
      (f2cl-lib:f2cl-set-string c4 (f2cl-lib:fref-string c3 (2 3)) (string 2))
      (f2cl-lib:computed-goto (label110 label200 label300) ispec)
     label110
      (setf nb 1)
      (cond
        ((f2cl-lib:fstring-= c2 "GE")
         (cond
           ((f2cl-lib:fstring-= c3 "TRF")
            (cond
              (sname
               (setf nb 64))
              (t
               (setf nb 64))))
           ((or (f2cl-lib:fstring-= c3 "QRF")
                (f2cl-lib:fstring-= c3 "RQF")
                (f2cl-lib:fstring-= c3 "LQF")
                (f2cl-lib:fstring-= c3 "QLF"))
            (cond
              (sname
               (setf nb 32))
              (t
               (setf nb 32))))
           ((f2cl-lib:fstring-= c3 "HRD")
            (cond
              (sname
               (setf nb 32))
              (t
               (setf nb 32))))
           ((f2cl-lib:fstring-= c3 "BRD")
            (cond
              (sname
               (setf nb 32))
              (t
               (setf nb 32))))
           ((f2cl-lib:fstring-= c3 "TRI")
            (cond
              (sname
               (setf nb 64))
              (t
               (setf nb 64))))))
        ((f2cl-lib:fstring-= c2 "PO")
         (cond
           ((f2cl-lib:fstring-= c3 "TRF")
            (cond
              (sname
               (setf nb 64))
              (t
               (setf nb 64))))))
        ((f2cl-lib:fstring-= c2 "SY")
         (cond
           ((f2cl-lib:fstring-= c3 "TRF")
            (cond
              (sname
               (setf nb 64))
              (t
               (setf nb 64))))
           ((and sname (f2cl-lib:fstring-= c3 "TRD"))
            (setf nb 32))
           ((and sname (f2cl-lib:fstring-= c3 "GST"))
            (setf nb 64))))
        ((and cname (f2cl-lib:fstring-= c2 "HE"))
         (cond
           ((f2cl-lib:fstring-= c3 "TRF")
            (setf nb 64))
           ((f2cl-lib:fstring-= c3 "TRD")
            (setf nb 32))
           ((f2cl-lib:fstring-= c3 "GST")
            (setf nb 64))))
        ((and sname (f2cl-lib:fstring-= c2 "OR"))
         (cond
           ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3 (1 1)) "G")
            (cond
              ((or (f2cl-lib:fstring-= c4 "QR")
                   (f2cl-lib:fstring-= c4 "RQ")
                   (f2cl-lib:fstring-= c4 "LQ")
                   (f2cl-lib:fstring-= c4 "QL")
                   (f2cl-lib:fstring-= c4 "HR")
                   (f2cl-lib:fstring-= c4 "TR")
                   (f2cl-lib:fstring-= c4 "BR"))
               (setf nb 32))))
           ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3 (1 1)) "M")
            (cond
              ((or (f2cl-lib:fstring-= c4 "QR")
                   (f2cl-lib:fstring-= c4 "RQ")
                   (f2cl-lib:fstring-= c4 "LQ")
                   (f2cl-lib:fstring-= c4 "QL")
                   (f2cl-lib:fstring-= c4 "HR")
                   (f2cl-lib:fstring-= c4 "TR")
                   (f2cl-lib:fstring-= c4 "BR"))
               (setf nb 32))))))
        ((and cname (f2cl-lib:fstring-= c2 "UN"))
         (cond
           ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3 (1 1)) "G")
            (cond
              ((or (f2cl-lib:fstring-= c4 "QR")
                   (f2cl-lib:fstring-= c4 "RQ")
                   (f2cl-lib:fstring-= c4 "LQ")
                   (f2cl-lib:fstring-= c4 "QL")
                   (f2cl-lib:fstring-= c4 "HR")
                   (f2cl-lib:fstring-= c4 "TR")
                   (f2cl-lib:fstring-= c4 "BR"))
               (setf nb 32))))
           ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3 (1 1)) "M")
            (cond
              ((or (f2cl-lib:fstring-= c4 "QR")
                   (f2cl-lib:fstring-= c4 "RQ")
                   (f2cl-lib:fstring-= c4 "LQ")
                   (f2cl-lib:fstring-= c4 "QL")
                   (f2cl-lib:fstring-= c4 "HR")
                   (f2cl-lib:fstring-= c4 "TR")
                   (f2cl-lib:fstring-= c4 "BR"))
               (setf nb 32))))))
        ((f2cl-lib:fstring-= c2 "GB")
         (cond
           ((f2cl-lib:fstring-= c3 "TRF")
            (cond
              (sname
               (cond
                 ((<= n4 64)
                  (setf nb 1))
                 (t
                  (setf nb 32))))
              (t
               (cond
                 ((<= n4 64)
                  (setf nb 1))
                 (t
                  (setf nb 32))))))))
        ((f2cl-lib:fstring-= c2 "PB")
         (cond
           ((f2cl-lib:fstring-= c3 "TRF")
            (cond
              (sname
               (cond
                 ((<= n2 64)
                  (setf nb 1))
                 (t
                  (setf nb 32))))
              (t
               (cond
                 ((<= n2 64)
                  (setf nb 1))
                 (t
                  (setf nb 32))))))))
        ((f2cl-lib:fstring-= c2 "TR")
         (cond
           ((f2cl-lib:fstring-= c3 "TRI")
            (cond
              (sname
               (setf nb 64))
              (t
               (setf nb 64))))))
        ((f2cl-lib:fstring-= c2 "LA")
         (cond
           ((f2cl-lib:fstring-= c3 "UUM")
            (cond
              (sname
               (setf nb 64))
              (t
               (setf nb 64))))))
        ((and sname (f2cl-lib:fstring-= c2 "ST"))
         (cond
           ((f2cl-lib:fstring-= c3 "EBZ")
            (setf nb 1)))))
      (setf ilaenv nb)
      (go end_label)
     label200
      (setf nbmin 2)
      (cond
        ((f2cl-lib:fstring-= c2 "GE")
         (cond
           ((or (f2cl-lib:fstring-= c3 "QRF")
                (f2cl-lib:fstring-= c3 "RQF")
                (f2cl-lib:fstring-= c3 "LQF")
                (f2cl-lib:fstring-= c3 "QLF"))
            (cond
              (sname
               (setf nbmin 2))
              (t
               (setf nbmin 2))))
           ((f2cl-lib:fstring-= c3 "HRD")
            (cond
              (sname
               (setf nbmin 2))
              (t
               (setf nbmin 2))))
           ((f2cl-lib:fstring-= c3 "BRD")
            (cond
              (sname
               (setf nbmin 2))
              (t
               (setf nbmin 2))))
           ((f2cl-lib:fstring-= c3 "TRI")
            (cond
              (sname
               (setf nbmin 2))
              (t
               (setf nbmin 2))))))
        ((f2cl-lib:fstring-= c2 "SY")
         (cond
           ((f2cl-lib:fstring-= c3 "TRF")
            (cond
              (sname
               (setf nbmin 8))
              (t
               (setf nbmin 8))))
           ((and sname (f2cl-lib:fstring-= c3 "TRD"))
            (setf nbmin 2))))
        ((and cname (f2cl-lib:fstring-= c2 "HE"))
         (cond
           ((f2cl-lib:fstring-= c3 "TRD")
            (setf nbmin 2))))
        ((and sname (f2cl-lib:fstring-= c2 "OR"))
         (cond
           ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3 (1 1)) "G")
            (cond
              ((or (f2cl-lib:fstring-= c4 "QR")
                   (f2cl-lib:fstring-= c4 "RQ")
                   (f2cl-lib:fstring-= c4 "LQ")
                   (f2cl-lib:fstring-= c4 "QL")
                   (f2cl-lib:fstring-= c4 "HR")
                   (f2cl-lib:fstring-= c4 "TR")
                   (f2cl-lib:fstring-= c4 "BR"))
               (setf nbmin 2))))
           ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3 (1 1)) "M")
            (cond
              ((or (f2cl-lib:fstring-= c4 "QR")
                   (f2cl-lib:fstring-= c4 "RQ")
                   (f2cl-lib:fstring-= c4 "LQ")
                   (f2cl-lib:fstring-= c4 "QL")
                   (f2cl-lib:fstring-= c4 "HR")
                   (f2cl-lib:fstring-= c4 "TR")
                   (f2cl-lib:fstring-= c4 "BR"))
               (setf nbmin 2))))))
        ((and cname (f2cl-lib:fstring-= c2 "UN"))
         (cond
           ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3 (1 1)) "G")
            (cond
              ((or (f2cl-lib:fstring-= c4 "QR")
                   (f2cl-lib:fstring-= c4 "RQ")
                   (f2cl-lib:fstring-= c4 "LQ")
                   (f2cl-lib:fstring-= c4 "QL")
                   (f2cl-lib:fstring-= c4 "HR")
                   (f2cl-lib:fstring-= c4 "TR")
                   (f2cl-lib:fstring-= c4 "BR"))
               (setf nbmin 2))))
           ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3 (1 1)) "M")
            (cond
              ((or (f2cl-lib:fstring-= c4 "QR")
                   (f2cl-lib:fstring-= c4 "RQ")
                   (f2cl-lib:fstring-= c4 "LQ")
                   (f2cl-lib:fstring-= c4 "QL")
                   (f2cl-lib:fstring-= c4 "HR")
                   (f2cl-lib:fstring-= c4 "TR")
                   (f2cl-lib:fstring-= c4 "BR"))
               (setf nbmin 2)))))))
      (setf ilaenv nbmin)
      (go end_label)
     label300
      (setf nx 0)
      (cond
        ((f2cl-lib:fstring-= c2 "GE")
         (cond
           ((or (f2cl-lib:fstring-= c3 "QRF")
                (f2cl-lib:fstring-= c3 "RQF")
                (f2cl-lib:fstring-= c3 "LQF")
                (f2cl-lib:fstring-= c3 "QLF"))
            (cond
              (sname
               (setf nx 128))
              (t
               (setf nx 128))))
           ((f2cl-lib:fstring-= c3 "HRD")
            (cond
              (sname
               (setf nx 128))
              (t
               (setf nx 128))))
           ((f2cl-lib:fstring-= c3 "BRD")
            (cond
              (sname
               (setf nx 128))
              (t
               (setf nx 128))))))
        ((f2cl-lib:fstring-= c2 "SY")
         (cond
           ((and sname (f2cl-lib:fstring-= c3 "TRD"))
            (setf nx 32))))
        ((and cname (f2cl-lib:fstring-= c2 "HE"))
         (cond
           ((f2cl-lib:fstring-= c3 "TRD")
            (setf nx 32))))
        ((and sname (f2cl-lib:fstring-= c2 "OR"))
         (cond
           ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3 (1 1)) "G")
            (cond
              ((or (f2cl-lib:fstring-= c4 "QR")
                   (f2cl-lib:fstring-= c4 "RQ")
                   (f2cl-lib:fstring-= c4 "LQ")
                   (f2cl-lib:fstring-= c4 "QL")
                   (f2cl-lib:fstring-= c4 "HR")
                   (f2cl-lib:fstring-= c4 "TR")
                   (f2cl-lib:fstring-= c4 "BR"))
               (setf nx 128))))))
        ((and cname (f2cl-lib:fstring-= c2 "UN"))
         (cond
           ((f2cl-lib:fstring-= (f2cl-lib:fref-string c3 (1 1)) "G")
            (cond
              ((or (f2cl-lib:fstring-= c4 "QR")
                   (f2cl-lib:fstring-= c4 "RQ")
                   (f2cl-lib:fstring-= c4 "LQ")
                   (f2cl-lib:fstring-= c4 "QL")
                   (f2cl-lib:fstring-= c4 "HR")
                   (f2cl-lib:fstring-= c4 "TR")
                   (f2cl-lib:fstring-= c4 "BR"))
               (setf nx 128)))))))
      (setf ilaenv nx)
      (go end_label)
     label400
      (setf ilaenv 6)
      (go end_label)
     label500
      (setf ilaenv 2)
      (go end_label)
     label600
      (setf ilaenv
              (f2cl-lib:int
               (*
                (f2cl-lib:freal
                 (min (the f2cl-lib:integer4 n1) (the f2cl-lib:integer4 n2)))
                1.6f0)))
      (go end_label)
     label700
      (setf ilaenv 1)
      (go end_label)
     label800
      (setf ilaenv 50)
      (go end_label)
     label900
      (setf ilaenv 25)
      (go end_label)
     label1000
      (setf ilaenv 0)
      (cond
        ((= ilaenv 1)
         (setf ilaenv (ieeeck 0 0.0f0 1.0f0))))
      (go end_label)
     label1100
      (setf ilaenv 0)
      (cond
        ((= ilaenv 1)
         (setf ilaenv (ieeeck 1 0.0f0 1.0f0))))
      (go end_label)
     end_label
      (return (values ilaenv nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::ilaenv
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (simple-string)
                        (simple-string) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::ieeeck))))

