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


(defun xermsg (librar subrou messg nerr level)
  (declare (type (f2cl-lib:integer4) level nerr)
           (type (simple-string *) messg subrou librar))
  (prog ((lfirst
          (make-array '(20) :element-type 'character :initial-element #\ ))
         (temp
          (make-array '(72) :element-type 'character :initial-element #\ ))
         (xlibr
          (make-array '(8) :element-type 'character :initial-element #\ ))
         (xsubr
          (make-array '(8) :element-type 'character :initial-element #\ ))
         (ltemp 0) (mkntrl 0) (llevel 0) (lerr 0) (kount 0) (i 0) (kdummy 0)
         (maxmes 0) (lkntrl 0))
    (declare (type (f2cl-lib:integer4) lkntrl maxmes kdummy i kount lerr llevel
                                       mkntrl ltemp)
             (type (simple-string 8) xsubr xlibr)
             (type (simple-string 72) temp)
             (type (simple-string 20) lfirst))
    (setf lkntrl (j4save 2 0 f2cl-lib:%false%))
    (setf maxmes (j4save 4 0 f2cl-lib:%false%))
    (cond
      ((or (< nerr (f2cl-lib:int-sub 9999999))
           (> nerr 99999999)
           (= nerr 0)
           (< level (f2cl-lib:int-sub 1))
           (> level 2))
       (xerprn " ***" -1
        (f2cl-lib:f2cl-//
         (f2cl-lib:f2cl-// "FATAL ERROR IN...$$ "
                           "XERMSG -- INVALID ERROR NUMBER OR LEVEL$$ ")
         "JOB ABORT DUE TO FATAL ERROR.")
        72)
       (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
           (xersve " " " " " " 0 0 0 kdummy)
         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5))
         (setf kdummy var-6))
       (xerhlt " ***XERMSG -- INVALID INPUT")
       (go end_label)))
    (setf i (j4save 1 nerr f2cl-lib:%true%))
    (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
        (xersve librar subrou messg 1 nerr level kount)
      (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5))
      (setf kount var-6))
    (if (and (= level -1) (> kount 1)) (go end_label))
    (f2cl-lib:f2cl-set-string xlibr librar (string 8))
    (f2cl-lib:f2cl-set-string xsubr subrou (string 8))
    (f2cl-lib:f2cl-set-string lfirst messg (string 20))
    (setf lerr nerr)
    (setf llevel level)
    (xercnt xlibr xsubr lfirst lerr llevel lkntrl)
    (setf lkntrl
            (max (the f2cl-lib:integer4 -2)
                 (the f2cl-lib:integer4
                      (min (the f2cl-lib:integer4 2)
                           (the f2cl-lib:integer4 lkntrl)))))
    (setf mkntrl (abs lkntrl))
    (if (and (< level 2) (= lkntrl 0)) (go label30))
    (if (and (= level 0) (> kount maxmes)) (go label30))
    (if (and (= level 1) (> kount maxmes) (= mkntrl 1)) (go label30))
    (if
     (and (= level 2)
          (> kount
             (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 maxmes))))
     (go label30))
    (cond
      ((/= lkntrl 0)
       (f2cl-lib:fset-string (f2cl-lib:fref-string temp (1 21))
                             "MESSAGE FROM ROUTINE ")
       (setf i
               (min (the f2cl-lib:integer4 (f2cl-lib:len subrou))
                    (the f2cl-lib:integer4 16)))
       (f2cl-lib:fset-string
        (f2cl-lib:fref-string temp (22 (f2cl-lib:int-add 21 i)))
        (f2cl-lib:fref-string subrou (1 i)))
       (f2cl-lib:fset-string
        (f2cl-lib:fref-string temp ((+ 22 i) (f2cl-lib:int-add 33 i)))
        " IN LIBRARY ")
       (setf ltemp (f2cl-lib:int-add 33 i))
       (setf i
               (min (the f2cl-lib:integer4 (f2cl-lib:len librar))
                    (the f2cl-lib:integer4 16)))
       (f2cl-lib:fset-string
        (f2cl-lib:fref-string temp ((+ ltemp 1) (f2cl-lib:int-add ltemp i)))
        (f2cl-lib:fref-string librar (1 i)))
       (f2cl-lib:fset-string
        (f2cl-lib:fref-string temp
                              ((+ ltemp i 1) (f2cl-lib:int-add ltemp i 1)))
        ".")
       (setf ltemp (f2cl-lib:int-add ltemp i 1))
       (xerprn " ***" -1 (f2cl-lib:fref-string temp (1 ltemp)) 72)))
    (cond
      ((> lkntrl 0)
       (cond
         ((<= level 0)
          (f2cl-lib:fset-string (f2cl-lib:fref-string temp (1 20))
                                "INFORMATIVE MESSAGE,")
          (setf ltemp 20))
         ((= level 1)
          (f2cl-lib:fset-string (f2cl-lib:fref-string temp (1 30))
                                "POTENTIALLY RECOVERABLE ERROR,")
          (setf ltemp 30))
         (t
          (f2cl-lib:fset-string (f2cl-lib:fref-string temp (1 12))
                                "FATAL ERROR,")
          (setf ltemp 12)))
       (cond
         ((or (and (= mkntrl 2) (>= level 1)) (and (= mkntrl 1) (= level 2)))
          (f2cl-lib:fset-string
           (f2cl-lib:fref-string temp
                                 ((+ ltemp 1) (f2cl-lib:int-add ltemp 14)))
           " PROG ABORTED,")
          (setf ltemp (f2cl-lib:int-add ltemp 14)))
         (t
          (f2cl-lib:fset-string
           (f2cl-lib:fref-string temp
                                 ((+ ltemp 1) (f2cl-lib:int-add ltemp 16)))
           " PROG CONTINUES,")
          (setf ltemp (f2cl-lib:int-add ltemp 16))))
       (cond
         ((> lkntrl 0)
          (f2cl-lib:fset-string
           (f2cl-lib:fref-string temp
                                 ((+ ltemp 1) (f2cl-lib:int-add ltemp 20)))
           " TRACEBACK REQUESTED")
          (setf ltemp (f2cl-lib:int-add ltemp 20)))
         (t
          (f2cl-lib:fset-string
           (f2cl-lib:fref-string temp
                                 ((+ ltemp 1) (f2cl-lib:int-add ltemp 24)))
           " TRACEBACK NOT REQUESTED")
          (setf ltemp (f2cl-lib:int-add ltemp 24))))
       (xerprn " ***" -1 (f2cl-lib:fref-string temp (1 ltemp)) 72)))
    (xerprn " *  " -1 messg 72)
    (cond
      ((> lkntrl 0)
       (tagbody
         (f2cl-lib:fformat temp ("ERROR NUMBER = " 1 (("~8D")) "~%") nerr)
         (f2cl-lib:fdo (i 16 (f2cl-lib:int-add i 1))
                       ((> i 22) nil)
           (tagbody
             (if (f2cl-lib:fstring-/= (f2cl-lib:fref-string temp (i i)) " ")
                 (go label20))
            label10))
        label20
         (xerprn " *  " -1
          (f2cl-lib:f2cl-// (f2cl-lib:fref-string temp (1 15))
                            (f2cl-lib:fref-string temp (i 23)))
          72)
         (fdump))))
    (cond
      ((/= lkntrl 0)
       (xerprn " *  " -1 " " 72)
       (xerprn " ***" -1 "END OF MESSAGE" 72)
       (xerprn "    " 0 " " 72)))
   label30
    (if (or (<= level 0) (and (= level 1) (<= mkntrl 1))) (go end_label))
    (cond
      ((and (> lkntrl 0)
            (< kount
               (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 maxmes))))
       (cond
         ((= level 1)
          (xerprn " ***" -1 "JOB ABORT DUE TO UNRECOVERED ERROR." 72))
         (t
          (xerprn " ***" -1 "JOB ABORT DUE TO FATAL ERROR." 72)))
       (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
           (xersve " " " " " " -1 0 0 kdummy)
         (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5))
         (setf kdummy var-6))
       (xerhlt " "))
      (t
       (xerhlt messg)))
    (go end_label)
   end_label
    (return (values nil nil nil nil nil))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::xermsg
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::a nil) (fortran-to-lisp::a nil)
                        (fortran-to-lisp::a nil) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil)
           :calls '(fortran-to-lisp::xercnt fortran-to-lisp::xerhlt
                    fortran-to-lisp::xersve fortran-to-lisp::xerprn
                    fortran-to-lisp::j4save))))

