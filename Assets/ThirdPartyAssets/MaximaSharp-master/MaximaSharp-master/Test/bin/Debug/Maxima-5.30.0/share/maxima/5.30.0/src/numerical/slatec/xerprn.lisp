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


(let* ((newlin "$$"))
  (declare (type (simple-string 2) newlin) (ignorable newlin))
  (defun xerprn (prefix npref messg nwrap)
    (declare (type (f2cl-lib:integer4) nwrap npref)
             (type (simple-string *) messg prefix))
    (prog ((iu (make-array 5 :element-type 'f2cl-lib:integer4)) (nunit 0)
           (cbuff
            (make-array '(148) :element-type 'character :initial-element #\ ))
           (idelta 0) (lpiece 0) (nextc 0) (lenmsg 0) (lwrap 0) (lpref 0) (i 0)
           (n 0))
      (declare (type (simple-array f2cl-lib:integer4 (5)) iu)
               (type (f2cl-lib:integer4) n i lpref lwrap lenmsg nextc lpiece
                                         idelta nunit)
               (type (simple-string 148) cbuff))
      (multiple-value-bind (var-0 var-1)
          (xgetua iu nunit)
        (declare (ignore var-0))
        (setf nunit var-1))
      (setf n (f2cl-lib:i1mach 4))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i nunit) nil)
        (tagbody
          (if (= (f2cl-lib:fref iu (i) ((1 5))) 0)
              (setf (f2cl-lib:fref iu (i) ((1 5))) n))
         label10))
      (cond
        ((< npref 0)
         (setf lpref (f2cl-lib:len prefix)))
        (t
         (setf lpref npref)))
      (setf lpref
              (min (the f2cl-lib:integer4 16) (the f2cl-lib:integer4 lpref)))
      (if (/= lpref 0)
          (f2cl-lib:fset-string (f2cl-lib:fref-string cbuff (1 lpref)) prefix))
      (setf lwrap
              (max (the f2cl-lib:integer4 16)
                   (the f2cl-lib:integer4
                        (min (the f2cl-lib:integer4 132)
                             (the f2cl-lib:integer4 nwrap)))))
      (setf lenmsg (f2cl-lib:len messg))
      (setf n lenmsg)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (if
           (f2cl-lib:fstring-/= (f2cl-lib:fref-string messg (lenmsg lenmsg))
                                " ")
           (go label30))
          (setf lenmsg (f2cl-lib:int-sub lenmsg 1))
         label20))
     label30
      (cond
        ((= lenmsg 0)
         (f2cl-lib:fset-string
          (f2cl-lib:fref-string cbuff ((+ lpref 1) (f2cl-lib:int-add lpref 1)))
          " ")
         (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                       ((> i nunit) nil)
           (tagbody
             (f2cl-lib:fformat (f2cl-lib:fref iu (i) ((1 5)))
                               (t (("~A")) "~%")
                               (f2cl-lib:fref-string cbuff
                                                     (1
                                                      (f2cl-lib:int-add lpref
                                                                        1))))
            label40))
         (go end_label)))
      (setf nextc 1)
     label50
      (setf lpiece
              (f2cl-lib:index (f2cl-lib:fref-string messg (nextc lenmsg))
                              newlin))
      (cond
        ((= lpiece 0)
         (tagbody
           (setf idelta 0)
           (setf lpiece
                   (min (the f2cl-lib:integer4 lwrap)
                        (the f2cl-lib:integer4
                             (f2cl-lib:int-sub (f2cl-lib:int-add lenmsg 1)
                                               nextc))))
           (cond
             ((< lpiece (f2cl-lib:int-add lenmsg 1 (f2cl-lib:int-sub nextc)))
              (f2cl-lib:fdo (i (f2cl-lib:int-add lpiece 1)
                             (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                            ((> i 2) nil)
                (tagbody
                  (cond
                    ((f2cl-lib:fstring-=
                      (f2cl-lib:fref-string messg
                                            ((+ nextc i (f2cl-lib:int-sub 1))
                                             (f2cl-lib:int-add nextc
                                                               i
                                                               (f2cl-lib:int-sub
                                                                1))))
                      " ")
                     (setf lpiece (f2cl-lib:int-sub i 1))
                     (setf idelta 1)
                     (go label54)))
                 label52))))
          label54
           (f2cl-lib:fset-string
            (f2cl-lib:fref-string cbuff
                                  ((+ lpref 1)
                                   (f2cl-lib:int-add lpref lpiece)))
            (f2cl-lib:fref-string messg
                                  (nextc
                                   (f2cl-lib:int-sub
                                    (f2cl-lib:int-add nextc lpiece)
                                    1))))
           (setf nextc (f2cl-lib:int-add nextc lpiece idelta))))
        ((= lpiece 1)
         (setf nextc (f2cl-lib:int-add nextc 2))
         (go label50))
        ((> lpiece (f2cl-lib:int-add lwrap 1))
         (tagbody
           (setf idelta 0)
           (setf lpiece lwrap)
           (f2cl-lib:fdo (i (f2cl-lib:int-add lpiece 1)
                          (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                         ((> i 2) nil)
             (tagbody
               (cond
                 ((f2cl-lib:fstring-=
                   (f2cl-lib:fref-string messg
                                         ((+ nextc i (f2cl-lib:int-sub 1))
                                          (f2cl-lib:int-add nextc
                                                            i
                                                            (f2cl-lib:int-sub
                                                             1))))
                   " ")
                  (setf lpiece (f2cl-lib:int-sub i 1))
                  (setf idelta 1)
                  (go label58)))
              label56))
          label58
           (f2cl-lib:fset-string
            (f2cl-lib:fref-string cbuff
                                  ((+ lpref 1)
                                   (f2cl-lib:int-add lpref lpiece)))
            (f2cl-lib:fref-string messg
                                  (nextc
                                   (f2cl-lib:int-sub
                                    (f2cl-lib:int-add nextc lpiece)
                                    1))))
           (setf nextc (f2cl-lib:int-add nextc lpiece idelta))))
        (t
         (setf lpiece (f2cl-lib:int-sub lpiece 1))
         (f2cl-lib:fset-string
          (f2cl-lib:fref-string cbuff
                                ((+ lpref 1) (f2cl-lib:int-add lpref lpiece)))
          (f2cl-lib:fref-string messg
                                (nextc
                                 (f2cl-lib:int-sub
                                  (f2cl-lib:int-add nextc lpiece)
                                  1))))
         (setf nextc (f2cl-lib:int-add nextc lpiece 2))))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i nunit) nil)
        (tagbody
          (f2cl-lib:fformat (f2cl-lib:fref iu (i) ((1 5)))
                            (t (("~A")) "~%")
                            (f2cl-lib:fref-string cbuff
                                                  (1
                                                   (f2cl-lib:int-add lpref
                                                                     lpiece))))
         label60))
      (if (<= nextc lenmsg) (go label50))
      (go end_label)
     end_label
      (return (values nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::xerprn
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::a nil) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::a nil) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil)
           :calls '(fortran-to-lisp::i1mach fortran-to-lisp::xgetua))))

