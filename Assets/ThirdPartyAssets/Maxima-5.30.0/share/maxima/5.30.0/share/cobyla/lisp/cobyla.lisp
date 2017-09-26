;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.222 2010/10/08 03:05:30 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.204 2010/02/23 05:21:30 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.114 2010/05/17 01:42:14 rtoy Exp $")

;;; Using Lisp CMU Common Lisp CVS Head 2010-09-27 22:45:24 (20B Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :cobyla)


(defun cobyla (n m x rhobeg rhoend iprint maxfun w iact ierr)
  (declare (type (array f2cl-lib:integer4 (*)) iact)
           (type double-float rhoend rhobeg)
           (type (array double-float (*)) w x)
           (type (f2cl-lib:integer4) ierr maxfun iprint m n))
  (f2cl-lib:with-multi-array-data
      ((x double-float x-%data% x-%offset%)
       (w double-float w-%data% w-%offset%)
       (iact f2cl-lib:integer4 iact-%data% iact-%offset%))
    (prog ((iwork 0) (idx 0) (isigb 0) (iveta 0) (ivsig 0) (ia 0) (idatm 0)
           (isimi 0) (isim 0) (icon 0) (mpp 0))
      (declare (type (f2cl-lib:integer4) mpp icon isim isimi idatm ia ivsig
                                         iveta isigb idx iwork))
      '""
      '"     this subroutine minimizes an objective function f(x) subject to m"
      '"     inequality constraints on x, where x is a vector of variables that"
      '"     n components. the algorithm employs linear approximations to the"
      '"     objective and constraint functions, the approximations being forme"
      '"     linear interpolation at n+1 points in the space of the variables."
      '"     we regard these interpolation points as vertices of a simplex. the"
      '"     parameter rho controls the size of the simplex and it is reduced"
      '"     automatically from rhobeg to rhoend. for each rho the subroutine t"
      '"     to achieve a good vector of variables for the current size, and th"
      '"     rho is reduced until the value rhoend is reached. therefore rhobeg"
      '"     rhoend should be set to reasonable initial changes to and the requ"
      '"     accuracy in the variables respectively, but this accuracy should b"
      '"     viewed as a subject for experimentation because it is not guarante"
      '"     the subroutine has an advantage over many of its competitors, howe"
      '"     which is that it treats each constraint individually when calculat"
      '"     a change to the variables, instead of lumping the constraints toge"
      '"     into a single penalty function. the name of the subroutine is deri"
      '"     from the phrase constrained optimization by linear approximations."
      '""
      '"     the user must set the values of n, m, rhobeg and rhoend, and must"
      '"     provide an initial vector of variables in x. further, the value of"
      '"     iprint should be set to 0, 1, 2 or 3, which controls the amount of"
      '"     printing during the calculation. specifically, there is no output"
      '"     iprint=0 and there is output only at the end of the calculation if"
      '"     iprint=1. otherwise each new value of rho and sigma is printed."
      '"     further, the vector of variables and some function information are"
      '"     given either when rho is reduced or when each new value of f(x) is"
      '"     computed in the cases iprint=2 or iprint=3 respectively. here sigm"
      '"     is a penalty parameter, it being assumed that a change to x is an"
      '"     improvement if it reduces the merit function"
      '"                f(x)+sigma*max(0.0,-c1(x),-c2(x),...,-cm(x)),"
      '"     where c1,c2,...,cm denote the constraint functions that should bec"
      '"     nonnegative eventually, at least to the precision of rhoend. in th"
      '"     printed output the displayed term that is multiplied by sigma is"
      '"     called maxcv, which stands for 'maximum constraint violation'. the"
      '"     argument maxfun is an integer variable that must be set by the use"
      '"     limit on the number of calls of calcfc, the purpose of this routin"
      '"     given below. the value of maxfun will be altered to the number of"
      '"     of calcfc that are made. the arguments w and iact provide real and"
      '"     integer arrays that are used as working space. their lengths must"
      '"     least n*(3*n+2*m+11)+4*m+6 and m+1 respectively."
      '""
      '"     in order to define the objective and constraint functions, we requ"
      '"     a subroutine that has the name and arguments"
      '"                subroutine calcfc (n,m,x,f,con)"
      '"                dimension x(*),con(*)  ."
      '"     the values of n and m are fixed and have been defined already, whi"
      '"     x is now the current vector of variables. the subroutine should re"
      '"     the objective and constraint functions at x in f and con(1),con(2)"
      '"     ...,con(m). note that we are trying to adjust x so that f(x) is as"
      '"     small as possible subject to the constraint functions being nonneg"
      '""
      '"     partition the working space array w to provide the storage that is"
      '"     for the main calculation."
      '""
      (setf mpp (f2cl-lib:int-add m 2))
      (setf icon 1)
      (setf isim (f2cl-lib:int-add icon mpp))
      (setf isimi (f2cl-lib:int-add isim (f2cl-lib:int-mul n n) n))
      (setf idatm (f2cl-lib:int-add isimi (f2cl-lib:int-mul n n)))
      (setf ia (f2cl-lib:int-add idatm (f2cl-lib:int-mul n mpp) mpp))
      (setf ivsig (f2cl-lib:int-add ia (f2cl-lib:int-mul m n) n))
      (setf iveta (f2cl-lib:int-add ivsig n))
      (setf isigb (f2cl-lib:int-add iveta n))
      (setf idx (f2cl-lib:int-add isigb n))
      (setf iwork (f2cl-lib:int-add idx n))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18 var-19)
          (cobylb n m mpp x rhobeg rhoend iprint maxfun
           (f2cl-lib:array-slice w double-float (icon) ((1 *)))
           (f2cl-lib:array-slice w double-float (isim) ((1 *)))
           (f2cl-lib:array-slice w double-float (isimi) ((1 *)))
           (f2cl-lib:array-slice w double-float (idatm) ((1 *)))
           (f2cl-lib:array-slice w double-float (ia) ((1 *)))
           (f2cl-lib:array-slice w double-float (ivsig) ((1 *)))
           (f2cl-lib:array-slice w double-float (iveta) ((1 *)))
           (f2cl-lib:array-slice w double-float (isigb) ((1 *)))
           (f2cl-lib:array-slice w double-float (idx) ((1 *)))
           (f2cl-lib:array-slice w double-float (iwork) ((1 *))) iact ierr)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-8 var-9
                         var-10 var-11 var-12 var-13 var-14 var-15 var-16
                         var-17 var-18))
        (setf maxfun var-7)
        (setf ierr var-19))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil maxfun nil nil ierr)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::cobyla
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) double-float double-float
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil fortran-to-lisp::maxfun nil
                            nil fortran-to-lisp::ierr)
           :calls '(fortran-to-lisp::cobylb))))

