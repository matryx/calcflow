
;;; Package to allow Maxima-level user-defined simplifying functions
;;;
;;; For example, suppose we want to write a step function stepfn(x)
;;; which is 0 for x<- and 1 for x>0.
;;;
;;; /* Define simplifying function */
;;; simp_stepfn(x):=
;;;   block([prederror:false],
;;;      if is(x<=0)=true then 0
;;;      elseif is(x>0)=true then 1
;;;      else simpfuncall('stepfn,x))$
;;; /* Declare stepfn to be simplifying */
;;; simplifying('stepfn,'simp_stepfn)$
;;;
;;; /* Test simple cases */
;;; stepfn(-x^2);      /* 0 */
;;; stepfn(x^2+1);     /* 1 */
;;; ex: stepfn(x^2);   /* stepfn(x^2) -- no simplifications apply */
;;; assume(x>0)$
;;; ex;                /* Assumptions not consulted */
;;; resimplify(ex):=expand(ex,0,0)$
;;; /* Force resimplification */
;;; resimplify(ex);    /* 1 */                                             
;;; forget(x>0)$
;;; resimplify(ex);    /* stepfn(x^2) */

;;; Utilities

(defun defined-functionp (ex)
  (cond ((null ex) nil)
	((symbolp ex)
	 (if (or (fboundp ex)
		 (safe-mgetl ex '(mexpr mmacro)))
	     t))
	((and (not (atom ex))
	      (eq (caar ex) 'lambda))
	 t)
	(t nil)))

(defmacro mwarn (str &rest args)
  `(mtell ,(concatenate 'string "Warning: " str) ,@args))


;;; Declare a user Maxima function to be a simplifying function
;;; simplifying(f,g) -- uses g as the simplifier
;;; simplifying(f,false) -- removes simplifying property
;;;
;;; You can override built-in simplifiers, but it is not recommended

(defun $simplifying (f simplifier)
  (if (not (symbolp f))
      (merror "Simplifying function ~M must be a symbol" f))
  (if (and simplifier (not (defined-functionp simplifier)))
      (mwarn "simplifier function ~M is not defined" simplifier))
  (if (and (get f 'operators) (not (get f 'user-simplifying)))
      (mwarn "~M is overriding built-in simplifier for ~M" simplifier f))
  (setf (get f 'user-simplifying) simplifier)
  (setf (get f 'operators) (if simplifier #'user-simplifying nil))
  f)

;;; Create the expression fun(args...) and mark it as simplified.
;;; Thus, simpfuncall(sin,0) => sin(0), not 0, but resimplifying with
;;; expand(simpfuncall(sin,0)) does simplify to 0.
;;; It is generally not recommended to use this for functions with
;;; built-in simplifiers. (i.e. be very careful)

(defun $simpfuncall (fun &rest args) (simpfunmake fun args))

(defun $simpfunmake (fun args)
  (simpfunmake fun
	       (if ($listp args)
		   (cdr args)
		 (merror "Bad second argument to `simpfunmake': ~M" args))))
  
(defun simpfunmake (fun args)
  (if (not (or (symbolp fun) ($subvarp fun)
	       (and (not (atom fun)) (eq (caar fun) 'lambda))))
      (merror "Bad first argument to `simpfuncall/make': ~M" fun))
  (simpcons (getopr fun) args))

(defmfun simpcons (op args)
  (if (symbolp op)
      `((,op simp) ,@args)
    `((mqapply simp) ,op ,@args)))
						  
;;; The generic simplifying function for user simplification functions
(defun user-simplifying (l ignore simpflag)
  (declare (ignore ignore))
  (let* ((op (caar l))	 
	 (simplifier (get op 'user-simplifying))
	 ;; args are (re)simplified *outside* the simplification fnc
	 (args (mapcar #'(lambda (i) (simpcheck i simpflag)) (cdr l))))
    (let ( ;; args have already been resimplified if necessary
	  (dosimp nil))
      (declare (special dosimp))
      (if (defined-functionp simplifier)
	  (mapply simplifier args op)
	(simpcons op args)))))