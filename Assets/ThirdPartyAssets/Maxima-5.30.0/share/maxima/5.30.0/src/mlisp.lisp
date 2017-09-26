;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;	** (c) Copyright 1982 Massachusetts Institute of Technology **

(macsyma-module mlisp)

(eval-when
    #+gcl (compile eval)
    #-gcl (:compile-toplevel :execute)

    (defvar *old-read-base* *read-base*)
    (setq *read-base* 10.))

(defmvar $mapprint t
  "If TRUE, messages about map/fullmap truncating on the shortest list
or if apply is being used are printed.")
  
(declare-top (special derivflag derivlist $labels $values $functions $arrays 
                      $rules $gradefs $dependencies $aliases
		      $myoptions $props genvar $maxposex $maxnegex $expop $expon
		      $numer state-pdl *mdebug* *refchkl* *baktrcl*
		      $norepeat $detout $doallmxops $doscmxops opers
		      *mopl* *alphabet* $%% %e-val
		      $macros linel $ratfac $ratwtlvl
		      $operators $partswitch *gcdl*
		      *builtin-$props* $infolists))

(declare-top (unspecial args))

(defvar mspeclist nil)
(defvar bindlist nil)
(defvar loclist nil)
(defvar mproplist nil)
(defvar *nounl* nil)
(defvar scanmapp nil)
(defvar maplp nil)
(defvar mprogp nil)
(defvar evp nil)
(defvar mdop nil)
(defvar mlocp nil)
(defvar aexprp nil)
(defvar fmaplvl 0)
(defvar dsksetp nil)
(defvar aryp nil)
(defvar msump nil)
(defvar evarrp nil)
(defvar factlist nil)
(defvar mfexprp t)
(defvar *nounsflag* nil)
(defvar transp nil)
(defvar noevalargs nil)
(defvar rulefcnl nil)
(defvar featurel
  '($integer $noninteger $even $odd $rational $irrational $real $imaginary $complex
             $analytic $increasing $decreasing $oddfun $evenfun $posfun $constant
             $commutative $lassociative $rassociative $symmetric $antisymmetric))

(defmvar $features (cons '(mlist simp) (append featurel nil)))
(defmvar $%enumer nil)
(defmvar $float nil)
(defmvar $refcheck nil)
(defmvar $translate nil)
(defmvar $transrun t)
(defmvar $savedef t)
(defmvar $maperror t)
(defmvar $optionset nil)
(defmvar $setcheckbreak nil)
(defmvar $infeval nil)
(defmvar $piece '$piece)
(defmvar $setval '$setval)

;; These three variables are what get stuck in array slots as magic
;; unbound objects.  They are for T, FIXNUM, and FLONUM type arrays
;; respectively.

(defvar munbound '|#####|)

(defvar fixunbound most-negative-fixnum)

(defvar flounbound most-negative-flonum)

(defmvar munbindp nil
  "Used for safely `munbind'ing incorrectly-bound variables."
  no-reset)

(defmvar $setcheck nil)

(mapc #'(lambda (x) (setf (symbol-value x) (ncons '(mlist simp))))
      '($values $functions $macros $arrays $myoptions $rules $props))

(defmfun mapply1 (fn args fnname form)
  (declare (special aryp))
  (cond ((atom fn)
	 (cond ((atom fn)
		(cond ((functionp fn)
		       (apply fn args))
              ((and (symbolp fn) (fboundp fn) (not (macro-function fn)))
               (mapply1 (symbol-function fn) args fn form))
		      ((and (symbolp fn) (symbol-array fn))
		       (mapply1 (symbol-array fn) args fn form))
		      (t
		       (setq fn (getopr fn))
		       (badfunchk fnname fn nil)
		       (let ((noevalargs t))
			 (meval (cons (ncons fn) args))))))))

	;; GCL considers interpreted functions and lambdas to be non-atoms
	#+gcl((functionp fn)
	 (apply fn args))

	;; extension for pdiff; additional extension are welcomed.
        ;; (AND (CONSP FN) (CONSP (CAR FN)) ...) is an attempt to identify
        ;; conventional Maxima expressions ((FOO) X Y Z); probably should
        ;; encapsulate somewhere, maybe it is already ??
	((and (consp fn) (consp (car fn)) (symbolp (mop fn)) (get (mop fn) 'mapply1-extension)
	      (apply (get (mop fn) 'mapply1-extension) (list fn args fnname form))))
	((eq (car fn) 'lambda)
	 (apply (coerce fn 'function) args))
	((eq (caar fn) 'lambda) (mlambda fn args fnname t form))
	((eq (caar fn) 'mquote) (cons (cdr fn) args))
	((and aryp (member (caar fn) '(mlist $matrix) :test #'eq))
	 (if (not (or (= (length args) 1)
		      (and (eq (caar fn) '$matrix) (= (length args) 2))))
	     (merror (intl:gettext "apply: wrong number of indices; found: ~M") (cons '(mlist) args)))
	 (if (member 0 args :test #'eq)
	     (merror (intl:gettext "apply: no such ~M element: ~M") (if (eq (caar fn) 'mlist) (intl:gettext "list") (intl:gettext "matrix"))
		     `((mlist) ,@args)))
	 (do ((args1 args (cdr args1)))
	     ((null args1) (let (($piece $piece) ($partswitch 'mapply))
			     (apply #'$inpart (cons fn args))))
	   (unless (fixnump (car args1))
	     (if evarrp (throw 'evarrp 'notexist))
	     (merror (intl:gettext "apply: subscript must be an integer; found: ~M") (car args1)))))
	(aryp
	 (cons '(mqapply array) (cons fn args)))
	((member 'array (cdar fn) :test #'eq)
	 (cons '(mqapply) (cons fn args)))
	(t
	 (badfunchk fnname fn t))))

;; the last argument to mapply1 for the lineinfo is not correct here..
(defmfun mcall (fn &rest args)
  (mapply1 fn args fn nil))

(defun mevalargs (args)
  (cond (noevalargs (setq noevalargs nil) args)
	(t (mapcar #'meval args))))

;;Function Call stack each element is
;; (fname . bindlist) where bindlist was the value at time of entry.
;; So you can use this to compute what the bindings were at any
;; function call.
(defvar *mlambda-call-stack* (make-array 30 :fill-pointer 0 :adjustable t ))

;;; The frame info for a function call consists of 5 consecutive
;;; entries in *MLAMBDA-CALL-STACK*.  I call the topmost object of
;;; such a quintuple the `function designator' belonging to this
;;; frame.

(defun pop-mlambda-call-stack (&optional fnname)
  "Deactivate the topmost function call frame info.
Return the function designator for this frame and check that it
is EQ to FNNAME if the latter is non-NIL."
  (let ((ar *mlambda-call-stack*) mlambda)
    (symbol-macrolet ((mlambda-pointer (fill-pointer ar)))
      (prog1
	  (setq mlambda (aref ar (1- mlambda-pointer)))
	(when fnname
	  ;; Different frames can have the same function designator,
	  ;; so this doesn't prove anything, it's just a check.
	  (assert (eq mlambda fnname)
		  (*mlambda-call-stack*)
		  "Expected ~a but got ~a on mlambda call stack."
		  fnname mlambda))
	(decf mlambda-pointer 5)))))

(defun mlambda (fn args fnname noeval form)
  (cond ((not ($listp (cadr fn)))
	 (merror (intl:gettext "lambda: first argument must be a list; found: ~M") (cadr fn))))
  (setq noevalargs nil)
  (let ((params  (cdadr fn))( mlocp  t))
    (setq loclist (cons nil loclist))
    (do ((a) (p))
	((or (null params) (and (null args) (not (mdeflistp params))))
	 (setq args (nreconc a args) params (nreconc p params)))
      (cond ((mdeflistp params)
	     (setq params (cdar params) args (ncons (cons '(mlist) args)))))
      (cond ((and mfexprp (mquotep (car params)))
	     (setq a (cons (car args) a) p (cons (cadar params) p)))
	    ((atom (car params))
	     (setq p (cons (car params) p)
		   a (cons (cond (noeval (car args))
				 (t (meval (car args)))) a)))
	    (t (merror (intl:gettext "lambda: formal argument must be a symbol or quoted symbol; found: ~M") (car params))))
      (setq args (cdr args) params (cdr params)))
    (let (finish2033 (finish2032 params) (ar *mlambda-call-stack*))
      (declare (type (vector t) ar))
      (unwind-protect
	   (progn
	     (unless (> (array-total-size ar) (+ (fill-pointer ar) 10))
	       (setq ar (adjust-array ar (+ (array-total-size ar) 50)	:fill-pointer (fill-pointer ar))))
	     (vector-push bindlist ar)
	     ;; rather than pushing all on *baktrcl* it might be good
	     ;; to make a *last-form* global that is set in meval1
	     ;; and is pushed here.
	     (vector-push form ar)
	     (vector-push params ar)
	     (vector-push args ar)
	     (vector-push fnname ar)
	     (mbind finish2032 args fnname)
	     (setq finish2033 t)
	     (prog1
		 (let ((aexprp (and aexprp (not (atom (caddr fn)))
				    (eq (caar (caddr fn)) 'lambda))))
		   (cond ((null (cddr fn)) (merror (intl:gettext "lambda: no body present.")))
			 ((cdddr fn) (mevaln (cddr fn)))
			 (t (meval (caddr fn)))))
	       nil))
	(if finish2033
	    (progn
	      (incf (fill-pointer *mlambda-call-stack*) -5)
	      (munlocal)
	      (munbind finish2032)))))))


(defmspec mprogn (form)
  (mevaln (cdr form)))

(defmfun mevaln (l) ;; called in a few places externally.
  (do ((body l (cdr body))
       ($%% '$%%))
      ((null (cdr body)) (meval (car body)))
    (setq $%% (meval (car body)))))

(defun mqapply1 (form)
  (declare (special aryp))
  (destructuring-let (((fn . argl) (cdr form)) (aexprp))
    (unless (mquotep fn) (setq fn (meval fn)))
    (cond ((atom fn)
	   (meval (cons (cons fn aryp) argl)))
	  ((eq (caar fn) 'lambda)
	   (if aryp
	       (merror (intl:gettext "lambda: cannot apply lambda as an array function."))
	       (mlambda fn argl (cadr form) noevalargs form)))
	  (t
	   (mapply1 fn (mevalargs argl) (cadr form) form)))))

(defmfun meval (form)
  (simplifya (meval1 form) nil))

;;temporary hack to see what's going on:
(defmfun safe-mgetl (atom inds)
  (and (symbolp atom)
       (let ((props (get atom 'mprops)))
	 (and props (getl props inds)))))

(defmfun safe-mget (atom inds)
  (and (symbolp atom)
       (let ((props (get atom 'mprops)))
	 (and props (getf (cdr props) inds)))))

(defvar *last-meval1-form* nil)

(defmfun meval1 (form)
  (declare (special *nounl* *break-points* *break-step*))
  (cond
    ((atom form)
     (prog (val)
       (cond ((not (symbolp form)) (return form))
             ((and $numer
                   (setq val (safe-mget form '$numer))
                   (or (not (eq form '$%e)) $%enumer))
              (return (meval1 val)))
             ((not (boundp form))
              (if (safe-get form 'bindtest)
                  (merror (intl:gettext "evaluation: unbound variable ~:M")
                          form)
                  (return form))))
       (setq val (symbol-value form))
       (when (and $refcheck
                  (member form (cdr $values) :test #'eq)
                  (not (member form *refchkl* :test #'eq)))
         (setq *refchkl* (cons form *refchkl*))
         (mtell (intl:gettext "evaluation: ~:M has a value.~%") form))
       (return val)))
    ((or (and (atom (car form))
              (setq form (cons (ncons (car form)) (cdr form))))
         (atom (caar form)))
     (let ((*baktrcl* *baktrcl*) transp)
       (prog (u aryp)
         (declare (special aryp))
         (setq *last-meval1-form* form)
         (setq aryp (member 'array (cdar form) :test #'eq))
         (cond ((and (not aryp)
                     (member (caar form)
                             '(mplus mtimes mexpt mnctimes) :test #'eq))
                (go c))
               ;; dont bother pushing mplus and friends on *baktrcl*
               ;; should maybe even go below aryp.
               ((and *mdebug*
                     (progn
                       ;; if wanting to step, the *break-points*
                       ;; variable will be set to a vector (possibly empty).
                       (when (and *break-points*
                                  (or (null  *break-step*)
                                      (null (funcall *break-step* form))))
                         (let ((ar *break-points*))
                           (declare (type (vector t) ar))
                           (loop for i below (fill-pointer ar)
                                 when (eq (car (aref ar i)) form)
                                 do (*break-points* form)
                                 (loop-finish))))
                       nil)))
               ((eq (caar form) 'mqapply) (return (mqapply1 form))))
         (badfunchk (caar form) (caar form) nil)
       a 
         (setq u
               (or (safe-getl (caar form) '(noun))
                   (and *nounsflag*
                        (eq (getcharn (caar form) 1) #\%)
                        (not (or (getl-lm-fcn-prop (caar form) '(subr))
                                 (safe-getl (caar form) '(mfexpr*))))
                        (prog2 ($verbify (caar form))
                               (safe-getl (caar form) '(noun))))
                   (and (not aryp)
                        $transrun
                        (setq transp
                              (safe-getl (caar form) '(translated-mmacro))))
                   (and (not aryp)
                        (setq u
                              (or (safe-mget (caar form) 'trace)
                                  (and $transrun
                                       (safe-get (caar form) 'translated)
                                       (not (safe-mget (caar form) 'local-fun))
                                       (setq transp t)
                                       (caar form))))
                        (getl-lm-fcn-prop u '(subr)))
                   (cond (aryp (safe-mgetl (caar form) '(hashar array)))
                         ((safe-mgetl (caar form) '(mexpr mmacro)))
                         (t
                          (or (safe-getl (caar form) '(mfexpr*))
                              (getl-lm-fcn-prop (caar form) '(subr macro)))))))
         (when (null u) (go b))
         (return
           (cond ((eq (car u) 'hashar)
                  (harrfind (cons (car form) (mevalargs (cdr form)))))
                 ((eq (car u) 'subr)
                  (apply (caar form) (mevalargs (cdr form))))
                 ((eq (car u) 'noun)
                  (cond ((or (member (caar form) *nounl* :test #'eq) *nounsflag*)
                         (setq form (cons (cons (cadr u) (cdar form))
                                          (cdr form)))
                         (go a))
                        (aryp (go b))
                        ((member (caar form) '(%sum %product) :test #'eq)
                         (setq u (do%sum (cdr form) (caar form))
                               noevalargs nil)
                         (cons (ncons (caar form)) u))
                        (t (meval2 (mevalargs (cdr form)) form))))
                 ((eq (car u) 'array)
                  (arrfind (cons (car form) (mevalargs (cdr form)))))
                 ((eq (car u) 'mexpr)
                  (mlambda (cadr u) (cdr form) (caar form) noevalargs form))
                 ((member (car u) '(mmacro translated-mmacro) :test #'eq)
                  (setq noevalargs nil)
                  (meval (mmacro-apply (cadr u) form)))
                 ((eq (car u) 'mfexpr*)
                  (setq noevalargs nil)
                  (apply (cadr u) (ncons form)))
                 ((eq (car u) 'macro)
                  (setq noevalargs nil)
                  (setq form (cons(caar form) (cdr form)))
                  (eval form))
                 (t
                  (apply (cadr u) (mevalargs (cdr form))))))
       b 
         (if (and (not aryp) (load-function (caar form) t)) (go a))
         (badfunchk (caar form) (caar form) nil)
         (if (symbolp (caar form))
             (setq u (boundp (caar form)))
             (return (meval1-extend form)))
       c 
         (cond ((or (null u)
                    (and (safe-get (caar form) 'operators) (not aryp))
                    (eq (caar form) (setq u (symbol-value (caar form)))))
                (setq form (meval2 (mevalargs (cdr form)) form))
                (return (or (and (safe-mget (caar form) 'atvalues)
                                 (at1 form))
                            form)))
               ((and aryp
                     (safe-get (caar form) 'nonarray))
                (return (cons (cons (caar form) aryp)
                              (mevalargs (cdr form)))))
               ((atom u)
                (badfunchk (caar form) u nil)
                (setq form (cons (cons (getopr u) aryp) (cdr form)))
                (go a))
               ((eq (caar u) 'lambda)
                (if aryp
                    (merror (intl:gettext "lambda: cannot apply lambda as an array function."))
                    (return (mlambda u (cdr form)
                                       (caar form) noevalargs form))))
               (t
                (return
                  (mapply1 u (mevalargs (cdr form)) (caar form) form)))))))
    (t
     (mapply1 (caar form) (mevalargs (cdr form)) (caar form) form))))

(defun getl-lm-fcn-prop (sym props &aux fn typ)
  (check-arg sym symbolp "symbol")
  (setq fn sym)
  (cond ((functionp fn)
	 (setq typ 'subr))
	((macro-function sym)
	 (setq typ 'macro))
	((setq fn (symbol-array sym))
	 (setq typ 'array))
	((setq fn (get sym 'mfexpr*))
	 (setq typ 'mfexpr*)))
  (and typ (member typ props :test #'eq) (list typ fn)))


(defmfun meval2 (newargs old)
  (declare (special aryp))
  (let ((new (cons (car old) newargs)) nosimp)
    (cond ((not (member 'simp (cdar old) :test #'eq))
	   (if (and (not (eq (caar new) 'mlist)) (equal new old)) old new))
	  ((prog2 (setq nosimp (not (get (caar new) 'operators))) (alike1 new old))
	   (if nosimp old (cons (delsimp (car old)) (cdr old))))
	  (nosimp (if aryp new (cons (cons (caar new) '(simp)) newargs)))
	  (t (cons (cons (caar new) aryp) newargs)))))

(defun mparams (vars)
  (mapcar #'(lambda (x)
	      (cond ((atom x) x)
		    ((atom (cadr x)) (cadr x))
		    (t (cadadr x))))
	  (cdr vars)))

(defmfun mop (form)
  (if (eq (caar form) 'mqapply)
      (cadr form)
      (caar form)))

(defmfun margs (form)
  (if (eq (caar form) 'mqapply)
      (cddr form)
      (cdr form)))

(defun badfunchk (name val flag)
  (if (or flag (numberp val) (member val '(t nil $%e $%pi $%i) :test #'eq))
      (if (and (atom name) (not (equal val name)))
        (merror (intl:gettext "apply: found ~M evaluates to ~M where a function was expected.") name val)
        (merror (intl:gettext "apply: found ~M where a function was expected.") val))))

;; To store the value of $errormsg in mbind. This value is looked up in the
;; routine mbind-doit. This is a hack to get the expected behavior, when the
;; option variable $errormsg is used as a local variable in a block.
(defvar *$errormsg-value* nil)

(defun mbind-doit (lamvars fnargs fnname)
  "Makes a new frame where the variables in the list LAMVARS are bound
to the corresponding elements in FNARGS.  Note that these elements are
used tels quels, without calling MEVAL.
If FNNAME is non-NIL, it designates a function call frame.
This function does not handle errors properly, use the MBIND
wrapper for this."
  (declare (special bindlist mspeclist))
  (do ((vars lamvars (cdr vars))
       (args fnargs (cdr args)))
      ((cond ((and vars args) nil)
	     ((and (null vars) (null args)))
	     (t (assert fnname (fnname)
			"Expected a maxima function designator but got NIL.")
		(merror (intl:gettext "~M arguments supplied to ~M; found: ~M")
			(if vars (intl:gettext "Too few") (intl:gettext "Too many"))
			(cons (ncons fnname) lamvars)
			(cons '(mlist) fnargs)))))
    (let ((var (car vars)))
      (if (not (symbolp var))
	  (merror (intl:gettext "Only symbols can be bound; found: ~M") var))
      (let ((value (if (boundp var)
                       (if (eq var '$errormsg)
                           ;; Do not take the actual value of $errormsg. It is
                           ;; always NIL at this point, but the value which
                           ;; is stored in *$errormsg-value*.
                           *$errormsg-value*
                           (symbol-value var))
                       munbound)))
	(mset var (car args))
	(psetq bindlist (cons var bindlist)
	       mspeclist (cons value mspeclist))))))

(defun mbind (lamvars fnargs fnname)
  "Error-handling wrapper around MBIND-DOIT."
  (handler-case
      (let ((old-bindlist bindlist) win)
	(declare (special bindlist))
        ;; At this point store the value of $errormsg in a global. The macro
        ;; with-$error sets the value of $errormsg to NIL, but we need the
        ;; actual value in the routine mbind-doit.
        (setq *$errormsg-value* $errormsg)
	(unwind-protect
	     (prog1
		 (with-$error (mbind-doit lamvars fnargs fnname))
	       (setq win t))
	  (unless win
	    (unless (eq bindlist old-bindlist)
	      (munbind (nreverse (ldiff bindlist old-bindlist))))
	    (when fnname
	      (pop-mlambda-call-stack fnname)))))
    (maxima-$error (c)
      ;; HMM, HERE'S A CALL TO MERROR. I CAN'T TELL WHERE ARE THE ERROR MESSAGES.
      ;; IF I DID, I'D WRAP THEM IN A CALL TO GETTEXT
      (apply #'merror (cdr (the-$error c)))
      ;; Make absolutely sure that this handler (and mbind) doesn't
      ;; return in this situation since other code depends on this
      ;; behaviour.
      (throw 'macsyma-quit t))))

;;; For testing purposes

#+ignore
(defmfun $show_mbind_data ()
  (format t "~&~{~a = ~a~%~}"
	  (mapcan #'(lambda (x) (list x (symbol-value x)))
		  '(bindlist mspeclist $values *mlambda-call-stack*)))
  (finish-output)
  (values))

(defmfun munbind (vars)
  (dolist (var (reverse vars))
    (cond ((eq (car mspeclist) munbound)
	   (makunbound var)
	   (setf $values (delete var $values :count 1 :test #'eq)))
	  (t (let ((munbindp t)) (mset var (car mspeclist)))))
    (setq mspeclist (cdr mspeclist) bindlist (cdr bindlist))))

;;This takes the place of something like
;; (DELETE (ASSOC (NCONS VAR) $DEPENDENCIES) $DEPENDENCIES 1)

(defun mfunction-delete (var fn-a-list)
  (delete (assoc (ncons var) fn-a-list :test #'equal) fn-a-list :count 1 :test #'equal))

(defmspec mlocal (l)
  (push nil loclist)
  (let ((mlocp t))
    (meval `(($local) ,@(cdr l)))))

(defmspec $local (l)
  (setq l (cdr l))
  (unless mlocp
    (merror (intl:gettext "local: must be called within a block or lambda.")))
  (dolist (var l)
    (cond ((not (symbolp var))
	   (improper-arg-err var '$local))
	  ((and (mget var 'array)
		(arrayp (symbol-array var)))
       ;; HMM. I DON'T UNDERSTAND WHY DECLARED ARRAYS ARE OFF-LIMITS:
       ;; THE ARRAY IS JUST A PROPERTY LIKE ANY OTHER, IS IT NOT ??
       (merror (intl:gettext "local: argument cannot be a declared array; found: ~M") var)))
    (setq mproplist (cons (get var 'mprops) mproplist)
	  factlist (cons (get var 'data) factlist))
    (dolist (fact (car factlist))
      (putprop fact -1 'ulabs))
    (progn
      (mfunction-delete var $functions)
      (mfunction-delete var $macros)
      (mfunction-delete var $dependencies))
    (setf $arrays (delete var $arrays :count 1 :test #'eq))
    (zl-remprop var 'mprops)
    (zl-remprop var 'data))
  (rplaca loclist (reverse l))
  (setq mlocp nil)
  '$done)

(defun munlocal ()
  (dolist (var (car loclist))
    (let ((mprop (car mproplist))
	  (y nil)
	  (fact (car factlist)))
      (remcompary var)
      (cput var mprop 'mprops)
      (cond ((setq y (old-get mprop 'mexpr))
	     (add2lnc (cons (ncons var) (cdadr y)) $functions))
	    (t (mfunction-delete var $functions)))
      (cond ((setq y (old-get mprop 'mmacro))
	     (add2lnc (cons (ncons var) (cdadr y)) $macros))
	    (t (mfunction-delete var $macros)))
      (cond ((or (old-get mprop 'array) (old-get mprop 'hashar))
	     (add2lnc var $arrays))
	    (t (setf $arrays (delete var $arrays :count 1 :test #'eq))))
      (cond ((setq y (old-get mprop 'depends))
	     (add2lnc (cons (ncons var) y) $dependencies))
	    (t (mfunction-delete var $dependencies)))
      (rempropchk var)
      (mapc #'remov (get var 'data))
      (cput var fact 'data)
      (dolist (u fact)
	(zl-remprop u 'ulabs))
      (setq mproplist (cdr mproplist)
	    factlist (cdr factlist))))
  (setq loclist (cdr loclist)))

(defmacro msetq (a b)
  `(mset ',a ,b))

;; A "run-time macro" needed by MATCOM/MATRUN.
;;works with the defms
(defmspec msetq (l)
  (twoargcheck l)
  (mset (simplifya (cadr l) nil) (meval (caddr l))))

(defun mset (x y)
  (prog ()
     (cond ((or (null $setcheck)
		(eq $setcheck '$setcheck)))
	   ((and (or (atom $setcheck)
		     (memalike x (cdr $setcheck))
		     (and (not (atom x))
			  (memalike (caar x) (cdr $setcheck))))
		 (not (eq x y)))
	    (displa (list '(mtext) (disp2 x) '| set to | y))
	    (if (and $setcheckbreak (not (eq x '$setval)))
		(let (($setval y))
		  (merrbreak t)
		  (setq y $setval)))))
     (cond ((atom x)
	    (when (or (not (symbolp x))
		      (member x '(t nil) :test #'eq)
                      (mget x '$numer)
                      (get x 'sysconst))
	      (if munbindp (return nil))
	      (if (mget x '$numer)
		  (merror (intl:gettext "assignment: cannot assign to ~M; it is a declared numeric quantity.") x)
		  (merror (intl:gettext "assignment: cannot assign to ~M") x)))
	    (let ((f (get x 'assign)))
	      (if (and f (or (not (eq x y))
			     (member f '(neverset read-only-assign) :test #'eq)))
		  (if (eq (funcall f x y) 'munbindp) (return nil))))
	    (cond ((and (not (boundp x))
			(not dsksetp))
		   (add2lnc x $values))
		  ((and (not (eq x y))
			(optionp x))
		   (if $optionset (mtell (intl:gettext "assignment: assigning to option ~M") x))
		   (if (not (eq x '$linenum)) (add2lnc x $myoptions))))
	    (return (setf (symbol-value x) y)))

           ;; ---------- begin code copied & modified from defstruct.lisp

           ;; Check to see if the operator has an mset_extension_operator.
           ;; If so, this says how to do assignments. Examples, a@b:x. Put mset_extension_operator
           ;; of mrecord-assign on the atom $@.  To allow [a,b]:[3,4] put op on mlist.
           ;; arguably we could use mget, mfuncall, and $mset_extension_operator  and
           ;; allow this to be done at the maxima level instead of lisp.

           ;; X is could be something like (($FOO ARRAY) 42), in which case it is meaningful
           ;; to look for an assignment operator associated either with $FOO itself or with
           ;; $FOO's object type, with "object type" = (CAAR (SYMBOL-VALUE '$FOO)).

           ((let*
              ((x-value (if (boundp (caar x)) (symbol-value (caar x))))
               (mset-extension-op
                 (cond
                   ((get (caar x) 'mset_extension_operator))
                   ((and
                      (not (atom x-value))
                      (get (caar x-value) 'defstruct-template)
                      (get (caar x-value) 'mset_extension_operator))))))
              (if mset-extension-op
                (return-from mset (funcall mset-extension-op x y)))))

           ;; ---------- end code copied & modified from defstruct.lisp

	   ((member 'array (cdar x) :test #'eq)
	    (return (arrstore x y)))
	   (t (merror (intl:gettext "assignment: cannot assign to ~M") x)))))

;; ---------- begin code copied & modified from defstruct.lisp

;; CHANGES WRT FATEMAN'S STUFF.
;; (1) $NEW BARFS IF #ARGUMENTS != 1, OR ARGUMENT HAS NO DEFSTRUCT, OR WRONG NUMBER OF INITIALIZERS.
;; (2) $DEFSTRUCT ALLOWS 1 OR MORE ARGUMENTS, RETURNS A LIST OF DEFSTRUCTS.
;; (3) USE $PUT AND $GET TO MAINTAIN DEFSTRUCT PROPERTIES 
;;     (RENAMED TO $DEFSTRUCT_DEFAULT AND $DEFSTRUCT_TEMPLATE).
;;     THIS MAKES DEFSTRUCT PROPERTIES VISIBLE TO USER VIA GET AND PROPVARS.
;;     ALSO, THIS MAKES `KILL' KILL DEFSTRUCTS.
;; (4) @ EVALUATES LHS AND QUOTES RHS
;; (5) $STRUCTURES INFOLIST
;; (6) LBP = 200, RBP = 201 (HIGHER PRECEDENCE, LEFT-ASSOCIATIVE)
;; (7) A@B => A@B WHEN B IS NOT BOUND TO SOMETHING OTHER THAN ITSELF
;; (8) DISALLOW @ APPLIED TO EXPRESSIONS W/ OPERATOR NOT DECLARED BY DEFSTRUCT
;; (9) MAKE RECORD AND LIST ASSIGNMENT FUNCTIONS LISP FUNCTIONS (STRIP OFF $ FROM NAME)
;;     ALSO MAKE PROPERTY SYMBOLS LISP SYMBOLS (STRIP OFF $ FROM NAME)
;; (10) EXTEND KILL TO TAKE ITEMS OFF $STRUCTURES AND REMOVE DEFSTRUCT PROPERTIES
;; (11) EXTEND KILL TO RECOGNIZE KILL(X@Y)
;; (12) EVALUATE INITIALIZERS IN $DEFSTRUCT AND IN $NEW
;; (13) DISPLAY FIELDS WHICH HAVE BEEN ASSIGNED VALUES AS FOO(X = BAR, Y = BAZ)
;; (14) ASSIGN TRANSLATION PROPERTY TO 'DEFSTRUCT AND DEF-SAME%TR ALL STRUCTURES

(setf (get '$@ 'mset_extension_operator) 'mrecord-assign)

;;  defstruct(f(x,y,z));
;;  myrecord: new(f);
;;  myrecord@y:45;
;;  myrecord;  ==>   f(x,45,z)

;; initializers are possible
;; defstruct(f(x,y=3.14159, z));
;; ff:new(f)  ==>   f(x,3.14159,z)
;; ff@y:2.71828 ==>  ff is  f(x,2.71828,z).

;; the @ syntax can also be used instead of substinpart.

;; k:  h(g(aa,bb),cc);
;; k@1@2:dd; change aa to dd.
;; k;

(defmfun mrecord-assign (@-expr value)
  ;; assume @-expr is  (($@..)  instance-name  field-name)
  (let*
    ((instance (cadr @-expr))
	 (field (caddr @-expr))
	 (object (meval instance))
     template)
    (if (not (and (consp object) (consp (car object)) (setq template (get (caar object) 'defstruct-template))))
      (merror "MRECORD-ASSIGN: left-hand side doesn't appear to be a defstruct object:~%~M" instance)
      (let
        ((index
           (if (integerp field)
             field ;;; allow foo@3, also
             (position field template)))) ;field->integer
        (if (null index) (merror (intl:gettext "assignment: no such field: ~M @ ~M") instance field))
        (if (< 0 index (length object)) (setf (elt object index) value)
          (merror (intl:gettext "assignment: no such field: ~M @ ~M") instance field))
        value))))

;; MRECORD-KILL is very similar to MRECORD-ASSIGN. Might consider merging the two somehow.

(defmfun mrecord-kill (@-expr)
  (let*
    ((instance (cadr @-expr))
     (field (caddr @-expr))
     (object (meval instance))
     template)
    (if (not (and (consp object) (consp (car object)) (setq template (get (caar object) 'defstruct-template))))
      (merror "MRECORD-KILL: left-hand side doesn't appear to be a defstruct object:~%~M" instance)
      (let
        ((index
           (if (integerp field)
             field
             (position field template))))
        (if (null index) (merror (intl:gettext "kill: no such field: ~M @ ~M") instance field))
        (if (< 0 index (length object)) (setf (elt object index) (elt template index))
          (merror (intl:gettext "kill: no such field: ~M @ ~M") instance field))))))

(defmspec $@ (L)
  (let*
    ((a (cadr L))
     (b (caddr L))
     (e ($@-function (meval a) b)))
    (if (eq e b) L e)))

(defun $@-function (in fn)
  (cond
    ((not (listp in))
     (list '(%@) in fn)) ;; noun form
    ((get (caar in) 'defstruct-template)
     (let*
       ((index
         (if (integerp fn) fn ;; allow foo@3
             (position fn (get (caar in) 'defstruct-template))))) ;; field->integer
       (if (null index) (merror (intl:gettext "@: no such field: ~M @ ~M") in fn))
       (if  (< 0 index (length in))
         (elt in index)
         (merror (intl:gettext "@: no such field: ~M @ ~M") in fn))))
    (t
      (list '($@) in fn))))

(defun dimension-defstruct (form result)
  (let
    ((L1 (cdr (get (caar form) 'defstruct-template)))
     (L2 (cdr form)))
    (dimension-function (cons (car form) (mapcar #'(lambda (e1 e2) (if (eq e1 e2) e1 `((mequal) ,e1 ,e2))) L1 L2)) result)))

;; L looks like defstruct (foo(...), bar(...), baz(...)).
;; Process each argument and return a list of declared structures.

(defmspec $defstruct (L)
  `((mlist) ,@(mapcar 'defstruct1 (cdr L))))

;; trivial translation to quiet complaint about lack of translation for this defmspec
(def%tr $defstruct (x) `($any . (meval ',x)))

(defvar $structures '((mlist)))

(defun defstruct-translate (form)
  (let ((translated-args (mapcar #'translate (cdr form))))
    `($any simplify (list '(,(caar form)) ,@(mapcar #'cdr translated-args)))))

(defun defstruct1 (z) ;; z will look like (($whatever) $a $b $c)
   ;; store the template
  (putprop (caar z) (namesonly z) 'defstruct-template)
  ;; set the initialization
  (putprop (caar z) (initializersmostly z) 'defstruct-default)
  (setf (get (caar z) 'dimension) 'dimension-defstruct)
  (nconc $structures (list (get (caar z) 'defstruct-default)))
  (setf (get (caar z) 'translate) #'defstruct-translate)
  (get (caar z) 'defstruct-default))

(defun namesonly(r)			; f(a,b,c) unchanged, f(a=3,b=4,c=5) -> f(a,b,c)
  (cons (car r)(mapcar #'(lambda(z)
			   (cond((symbolp z) z)
				((eq (caar z) 'mequal)(second z))
				(t (merror (intl:gettext "defstruct: expected a record initializer; found: ~M") z))))
		       (cdr r))))

(defun initializersmostly(r);; f(a=3,b,c=5) -> f(3,b,5)
  (cons (car r)(mapcar #'(lambda(z)
			   (cond((symbolp z) z)
				((eq (caar z) 'mequal) (meval (third z)))
				(t (merror (intl:gettext "defstruct: expected a record initializer; found: ~M") z))))
		       (cdr r))))

(defmspec $new (h)
  (unless (= (length (cdr h)) 1)
    (merror (intl:gettext "~new: expected exactly one argument; found: ~M") (length (cdr h))))

  (let ((recordname (cadr h)))
    (cond
      ((symbolp recordname)  ;; the case of, e.g.  new(f);
       (if (null (get recordname 'defstruct-default))
         (merror (intl:gettext "new: no such structure ~M") recordname))

       (copy-tree (get recordname 'defstruct-default)))

      ;; assume there is some initialization here e.g. new (f(5,6,7))
      (t
        (let ((recordop (caar recordname)) (recordargs (cdr recordname)))
          (if (null (get recordop 'defstruct-default))
            (merror (intl:gettext "new: no such structure ~M") recordop))

          (if (not (= (length recordargs) (length (cdr (get recordop 'defstruct-default)))))
            (merror (intl:gettext "new: wrong number of arguments in initializer; expected ~M, not ~M.")
                    (length (cdr (get recordop 'defstruct-default))) (length recordargs)))

          `(,(car recordname) ,@(mapcar #'meval (cdr recordname))))))))

;; trivial translation to quiet complaint about lack of translation for this defmspec
(def%tr $new (x) `($any . (meval ',x)))

;; Following property assignments comprise the Lisp code equivalent to infix("@", 200, 201)

(defprop $@ %@ verb) 
(defprop $@ "@" op) 
(putopr "@" '$@) 
;; !! FOLLOWING LINE MOVED TO NPARSE.LISP TO AVOID COMPILER ERROR
;; !! (MOVING SUPRV1.LISP HIGHER IN MAXIMA.SYSTEM CAUSES MYSTERIOUS ERROR)
;; !! (define-symbol "@") 
(defprop $@ dimension-infix dimension) 
(defprop $@ (#\@) dissym) 
(defprop $@ msize-infix grind) 
(defprop $@ 200 lbp) 
(defprop $@ 201 rbp) 
(defprop $@ parse-infix led) 
(defprop %@ dimension-infix dimension) 
(defprop %@ (#\@) dissym) 
(defprop %@ $@ noun) 

;; The follow code implements PARALLEL LIST assignment.
;; it is consistent with commercial macsyma.  [a,b,c]:[x,y,z] means
;;  about the same as a:x, b:y, c:z.  Actually it
;; evaluates x,y,z  BEFORE any assignments to a,b,c, hence parallel.
;; Also implemented is [a,b,c]:x  which evaluates x once and assigns
;; to a,b,c.
;; value returned is (evaluated x to ex)  [ex,ex,ex].

;; quiz .  [a,b]:[b,2*a].  produces values a=b, b= 2*a.
;; re-execute the statement 4 times. what do you get?  [4b, 8a]
;;         
;; a neat application of parallel assignment is this version of
;; a gcd algorithm (for integers)...
;; kgcd(a,b):=(while b#0 do [a,b]:[b,remainder(a,b)], abs(a));
;; The extended euclidean algorithm looks even better with parallel
;; assignment.

;; add MLIST to possible operators on the left hand side of 
;; an assignment statement.

(setf (get 'mlist 'mset_extension_operator) 'mlist-assign)

(defmfun mlist-assign (tlist vlist)
  ;;  tlist is  ((mlist..)  var[0]... var[n])  of targets
  ;; vlist is either((mlist..)  val[0]... val[n]) of values
  ;; or possibly just one value.
  ;; should insert some checking code here
  (if (and (listp vlist)
	   (eq (caar vlist) 'mlist)
	   (not (= (length tlist)(length vlist))))
      (merror (intl:gettext "assignment: lists must be the same length; found: ~M, ~M") tlist vlist))
  (setq tlist
        `((mlist)
          ,@(mapcar
              #'(lambda (x) (if (or (symbolp x) ($listp x)) x `(,(car x) ((mquote) ,@(mapcar #'meval (cdr x))))))
              (cdr tlist))))
  (unless (and (listp vlist)
	   (eq (caar vlist) 'mlist))
    (setf vlist (cons (car tlist) ;; if [a,b,c]:v  then make a list [v,v,v]
		      (make-sequence 'list (1-(length tlist)) :initial-element vlist))))
  (map nil #'mset (cdr tlist)(cdr vlist))
   vlist)

;; ---------- end code copied & modified from defstruct.lisp

(defmspec $ev (l)
  (setq l (cdr l))
  (let ((evp t) (*nounl* *nounl*) ($float $float) ($numer $numer)
	($expop $expop) ($expon $expon) ($doallmxops $doallmxops)
	($doscmxops $doscmxops) (derivflag derivflag) ($detout $detout)
	(*nounsflag* *nounsflag*) (rulefcnl rulefcnl))
    (if (and (cdr l) (null (cddr l)) (eq (car l) '$%e) (eq (cadr l) '$numer))
	(setq l (append l '($%enumer))))
    (do ((l (cdr l) (cdr l)) (bndvars) (bndvals) (locvars) (exp (car l))
	 (subsl) (evflg 0) (ratf) (derivlist) (evfunl) (funcl) (predflg)
	 (noeval (member '$noeval (cdr l) :test #'eq)))
	((null l)
	 (mbinding (bndvars bndvars)
		   (meval `((mlocal) ,@locvars))
		   (let ($translate) (mapc #'meval1 funcl))
		   (let ($numer) (setq exp (mevalatoms exp)))
		   (if ($ratp exp) (setq ratf t exp ($ratdisrep exp)))
		   (if (specrepp exp) (setq exp (specdisrep exp)))
		   (when subsl
		     (setq exp (simplify exp))
		     (dolist (item subsl)
		       (setq exp (maxima-substitute (meval (car item))
						    (meval (cdr item))
						    exp)))))
	 (mbinding (bndvars bndvals)
		   (if (and $numer noeval $%enumer)
		       (setq exp (maxima-substitute %e-val '$%e exp)))
		   (setq exp (if noeval
				 (resimplify exp)
				 (simplify (if predflg (mevalp exp) (meval1 exp)))))
		   (if (or (> evflg 0) $infeval)
		       (prog (exp1)
			  (setq exp (specrepcheck exp))
			  loop (do ((l evfunl (cdr l)) (exp2 exp))
				   ((null l) (setq exp1 (meval exp2)))
				 (setq exp2 (list (ncons (car l)) exp2)))
			  (dolist (item subsl)
			    (setq exp1 (maxima-substitute (meval (car item))
							  (meval (cdr item))
							  exp1)))
			  (cond ((or (and (not $infeval)
					  (= (setq evflg (1- evflg)) 0))
				     (prog2 (setq exp1 (specrepcheck exp1))
					 (alike1 exp exp1)))
				 (setq exp exp1))
				(t (setq exp exp1) (go loop)))))
		   (if (and ratf (not $numer) (not $float))
		       (setq exp (let ($norepeat) (ratf exp)))))
	 (munlocal)
	 exp)
      (if (not (or (atom (car l))
		   (member 'array (cdaar l) :test #'eq)
		   (member (caaar l) '(mquote msetq mlist mequal mdefine mset
				     mdefmacro $expand $local $derivlist) :test #'eq)))
	  (setq l (cons (meval (car l)) (cdr l))))
      (cond ((or (atom (car l)) (member 'array (cdaar l) :test #'eq) (eq (caaar l) 'mquote))
	     (or (and (symbolp (car l))
		      (cond ((eq (car l) '$eval) (setq evflg (1+ evflg)))
			    ((member (car l) '($noeval $rescan) :test #'eq))
			    ((eq (car l) '$detout)
			     (setq $doallmxops nil $doscmxops nil $detout t))
			    ((eq (car l) '$numer) (setq $numer t $float t))
			    ((eq (car l) '$nouns) (setq *nounsflag* t))
			    ((eq (car l) '$pred) (setq predflg t))
			    ((eq (car l) '$expand)
			     (setq $expop $maxposex $expon $maxnegex))
			    ((eq (car l) '%derivative)
			     (setq derivflag t derivlist nil))
			    ((get (car l) 'evflag)
			     (setq bndvars (cons (car l) bndvars)
				   bndvals (cons (get (car l) 'evflag) bndvals)))
			    ((get (car l) 'evfun)
			     (setq exp (evfunmake (car l) exp)
				   evfunl (nconc evfunl (ncons (car l)))))))
		 (let ((fl (meval (car l))))
		   (cond ((symbolp fl)
			  (cond ((eq fl '$diff)
				 (setq l (list* nil '$del (cdr l))))
				((eq fl '$risch)
				 (setq l (list* nil '$integrate (cdr l)))))
			  (setq *nounl* (cons ($nounify fl) *nounl*)))
			 ((numberp fl) (improper-arg-err (car l) '$ev))
			 ((stringp fl) (improper-arg-err (car l) '$ev))
			 ((eq (caar fl) 'mlist)
			  (setq l (append fl (cdr l))))
			 ((member (caar fl)
				'(msetq mequal mdefine mdefmacro mset) :test #'eq)
			  (setq l (list* nil fl (cdr l))))
			 (t (improper-arg-err (car l) '$ev))))))
	    ((not (member (caaar l) '(msetq mlist mequal mdefine mdefmacro
				    $expand $local $derivlist mset) :test #'eq))
	     (improper-arg-err (car l) '$ev))
	    ((eq (caaar l) '$expand)
	     (cond ((null (cdar l)) (setq $expop $maxposex $expon $maxnegex))
		   ((null (cddar l)) (setq $expop (cadar l) $expon $maxnegex))
		   (t (setq $expop (cadar l) $expon (caddar l)))))
	    ((member (caaar l) '(mdefine mdefmacro) :test #'eq)
	     (let ((fun (cadar l)) $use_fast_arrays)
	       (if (eq (caar fun) 'mqapply) (setq fun (cadr fun)))
	       (setq fun ($verbify (caar fun)))
	       (setq funcl (nconc funcl (ncons (car l)))
		     locvars (append locvars (ncons fun)))
	       (if (rulechk fun) (setq rulefcnl (cons fun rulefcnl)))))
	    ((eq (caaar l) '$local) (setq locvars (append locvars (cdar l))))
	    ((eq (caaar l) '$derivlist) (setq derivflag t derivlist (cdar l)))
	    ((and (eq (caaar l) 'mset)
		  (setq l (cons (list '(msetq) (meval (cadar l)) (caddar l))
				(cdr l)))
		  nil))
	    ((member (caaar l) '(msetq mequal) :test #'eq)
	     (if (and (msetqp (car l)) (msetqp (caddar l)))
		 (setq l (nconc (|:SPREAD| (car l)) (cdr l))))
	     (if (or noeval (not (atom (cadar l))))
		 (setq subsl (nconc subsl (list (cons (caddar l) (cadar l))))))
	     (if (atom (cadar l))
		 (setq bndvars (cons (cadar l) bndvars)
		       bndvals (cons (meval (specrepcheck (caddar l))) bndvals))))
        (t (setq l (append (car l) (cdr l))))))))

(defmfun mevalatoms (exp)
  (cond ((atom exp) (meval1 exp))
	((member 'array (cdar exp) :test #'eq)
	 (let (exp1)
	   (let ((evarrp t)) (setq exp1 (catch 'evarrp (meval1 exp))))
	   (if (eq exp1 'notexist)
	       (cons (car exp) (mapcar #'mevalatoms (cdr exp)))
	       exp1)))
	((eq (caar exp) 'mquote) (cadr exp))
	((member (caar exp) '(msetq $define) :test #'eq)
	 (list (car exp) (cadr exp) (mevalatoms (caddr exp))))
	((or (and (eq (caar exp) '$ev)
		  (cdr exp)
		  (or (null (cddr exp)) (equal (cddr exp) '($eval))))
	     (eq (caar exp) 'mprogn))
	 (cons (car exp) (cons (mevalatoms (cadr exp)) (cddr exp))))
	((member (caar exp) '($sum $product %sum %product) :test #'eq)
	 (if msump
	     (meval exp)
	     (list (car exp) (cadr exp) (caddr exp)
		   (mevalatoms (cadddr exp)) (mevalatoms (car (cddddr exp))))))
	((and (eq (caar exp) '$%th) (eq (ml-typep (simplify (cadr exp))) 'fixnum))
	 (meval1 exp))
	((prog2 (autoldchk (caar exp))
	     (and (getl (caar exp) '(mfexpr*))
		  (not (get (caar exp) 'evok))))
	 exp)
	((mgetl (caar exp) '(mfexprp))
	 (cons (car exp)
	       (do ((a (cdadr (mget (caar exp) 'mexpr)) (cdr a))
		    (b (cdr exp) (cdr b)) (l))
		   ((not (and a b)) (nreverse l))
		 (cond ((mdeflistp a)
			(return (nreconc l (if (mquotep (cadar a))
					       b
					       (mapcar #'mevalatoms b)))))
		       ((mquotep (car a)) (setq l (cons (car b) l)))
		       (t (setq l (cons (mevalatoms (car b)) l)))))))
	((or (eq (caar exp) 'mmacroexpanded)
	     (and $transrun (get (caar exp) 'translated-mmacro))
	     (mget (caar exp) 'mmacro))
	 (mevalatoms (mmacroexpand exp)))
	(t (cons (car exp) (mapcar #'mevalatoms (cdr exp))))))

;; evok properties
(mapc #'(lambda (x) (putprop x t 'evok))
      '($map $maplist $fullmap $matrixmap $fullmapl $outermap $scanmap $apply))

(defun evfunmake (fun exp)
  (if (msetqp exp)
      (list (car exp) (cadr exp) (evfunmake fun (caddr exp)))
      (list (ncons fun) exp)))

(defun |:SPREAD| (x)
  (do ((val (do ((x x (caddr x))) (nil)
	      (if (not (msetqp (caddr x))) (return (caddr x)))))
       (x x (caddr x)) (l))
      ((not (msetqp x)) l)
    (setq l (cons (list (car x) (cadr x) val) l))))

(defmfun msetqp (x)
  (and (not (atom x)) (eq (caar x) 'msetq)))

(defmfun mquotep (x)
  (and (not (atom x)) (eq (caar x) 'mquote)))

(defmspec mquote (form)
  (cadr form))

(defmfun $subvarp (x)
  (and (not (atom x)) (member 'array (cdar x) :test #'eq) t))

(defmfun mseterr (x y)
  (if munbindp
      'munbindp
      (merror (intl:gettext "assignment: cannot assign ~M to ~:M") y x)))

;; assign properties
(mapc #'(lambda (x) (putprop (car x) (cadr x) 'assign))
      '(($linel msetchk) (*read-base* msetchk) (*print-base* msetchk) (modulus msetchk)
	($infolists neverset) ($trace neverset) ($ratweights msetchk)
	($ratvars msetchk) ($setcheck msetchk) ($gcd msetchk)
	($dotassoc msetchk) ($ratwtlvl msetchk) ($ratfac msetchk)
	($all neverset) ($numer numerset) ($fortindent msetchk)
	($gensumnum msetchk) ($genindex msetchk) ($fpprintprec msetchk)
	($floatwidth msetchk) ($parsewindow msetchk) ($optimprefix msetchk)
	($ttyintnum msetchk)))

(defmfun msetchk (x y)
  (cond ((member x '(*read-base* *print-base*) :test #'eq)
	 (cond ((eq y 'roman))
	       ((or (not (fixnump y)) (< y 2) (> y 36)) (mseterr x y))
	       ((eq x '*read-base*))))
	((member x '($linel $fortindent $gensumnum $fpprintprec $floatwidth
		   $parsewindow $ttyintnum) :test #'eq)
	 (if (not (fixnump y)) (mseterr x y))
         (if (eq x '$linel)
             (cond ((not (and (> y 0)         ; at least one char per line
                              (< y 1000001))) ; arbitrary chosen big value
                    (mseterr x y))
                   (t
                    (setq linel y))))
	 (cond ((and (member x '($fortindent $gensumnum $floatwidth $ttyintnum) :test #'eq) (< y 0))
		(mseterr x y))
	       ((and (eq x '$parsewindow) (< y -1)) (mseterr x y))
	       ((and (eq x '$fpprintprec) (or (< y 0) (= y 1))) (mseterr x y))))
	((member x '($genindex $optimprefix) :test #'eq) (if (not (symbolp y)) (mseterr x y)))
	((eq x '$dotassoc) (cput 'mnctimes y 'associative))
	((eq x 'modulus)
	 (cond ((null y))
	       ((and (integerp y) (plusp y))
	        ;; modulus must be an integer > 0. Give a warning if not
	        ;; a prime number.
	        (if (not (primep y))
	            (mtell (intl:gettext "warning: assigning ~:M, a non-prime, to 'modulus'~&") y)))
	       (t (mseterr x y))))
	((eq x '$setcheck)
	 (if (not (or (member y '($all t nil) :test #'eq) ($listp y))) (mseterr x y)))
	((eq x '$gcd) (if (not (or (null y) (member y *gcdl* :test #'eq))) (mseterr x y)))
	((eq x '$ratvars)
	 (if ($listp y) (apply #'$ratvars (cdr y)) (mseterr x y)))
	((eq x '$ratfac)
	 (if (and y $ratwtlvl)
	     (merror (intl:gettext "assignment: 'ratfac' and 'ratwtlvl' may not both be used at the same time."))))
	((eq x '$ratweights)
	 (cond ((not ($listp y)) (mseterr x y))
	       ((null (cdr y)) (kill1 '$ratweights))
	       (t (apply #'$ratweight (cdr y)))))
	((eq x '$ratwtlvl)
	 (if (and y (not (fixnump y))) (mseterr x y))
	 (if (and y $ratfac)
	     (merror (intl:gettext "assignment: 'ratfac' and 'ratwtlvl' may not both be used at the same time."))))))

(defmfun numerset (assign-var y)
  (declare (ignore assign-var))
  (mset '$float y))

(defmfun neverset (x assign-val)
  (declare (ignore assign-val))
  (if munbindp 
      'munbindp 
      (merror (intl:gettext "assignment: cannot assign to ~:M") x)))

;; Check assignment to be a positive integer including zero
(defun posintegerset (x y)
  (if (or (not (integerp y))
          (not (>= y 0)))
      (merror
        (intl:gettext "assignment: '~:M must be a positive integer. Found: ~:M")
        x y)))

(defmfun mmapev (l)
  (if (null (cddr l))
      (merror (intl:gettext "~:M: expected two or more arguments; found: ~M") (caar l) (cdr l)))
  (let ((op (getopr (meval (cadr l)))))
    (autoldchk op)
    (badfunchk (cadr l) op nil)
    (cons op (mapcar #'meval (cddr l)))))

(defmspec $map (l)
  (apply #'map1 (mmapev l)))

(defmfun map1 n
  (do ((i n (1- i))
       (argi (setarg n (format1 (arg n))) (format1 (arg (1- i))))
       (op (or (mapatom (arg n)) (mop (arg n))))
       (flag (mapatom (arg n))
	     (or flag
		 (setq flag (mapatom argi))
		 (and (not maplp) (not (alike1 (mop argi) op)))))
       (argl nil (cons argi argl))
       (cdrl nil (or flag (cons (margs argi) cdrl))))
      ((= i 1) (if flag
		   (cond ((not $maperror)
			  (when $mapprint (mtell (intl:gettext "map: calling 'apply'")))
			  (funcer (arg 1) argl))
			 ((and (= n 2) (mapatom (arg 2)))
			  (improper-arg-err (arg 2) '$map))
			 (t (merror (intl:gettext "map: arguments must have same main operator; found: ~M, ~M") op (mop (first argl)))))
		   (mcons-op-args op (apply #'mmapcar (cons (arg 1) cdrl)))))))

(defmspec $maplist (l)
  (let ((maplp t) res)
    (setq res (apply #'map1 (mmapev l)))
    (cond ((atom res) (list '(mlist) res))
	  ((eq (caar res) 'mlist) res)
	  (t (cons '(mlist) (margs res))))))

(defmfun mmapcar n
  (do ((ans nil (cons (funcer (arg 1) argl) ans))
       (argl nil nil))
      ((do ((i n (1- i)))
	   ((= i 1) nil)
	 (when (null (arg i))
	   (when (or (< i n)
		     (do ((j 2 (1+ j)))
			 ((= j n) nil)
		       (when (arg j) (return t))))
	     (when $maperror
	       (merror (intl:gettext "map: arguments must be the same length.")))
	     (when $mapprint (mtell (intl:gettext "map: truncating one or more arguments."))))
	   (return t))
	 (push (car (arg i)) argl)
	 (setarg i (cdr (arg i))))
       (nreverse ans))))

(defun mapatom (x)
  (or (symbolp x) (mnump x) ($subvarp x) (stringp x)))

(defmfun $mapatom (x)
  (if (mapatom (specrepcheck x)) t))

(defmspec $fullmap (l)
  (setq l (mmapev l))
  (fmap1 (car l) (cdr l) nil))

(defun fmap1 (fn argl fmapcaarl)
  (setq argl (mapcar #'format1 argl))
  (do ((op (or (mapatom (car argl)) (mop (car argl))))
       (fmaplvl (1- fmaplvl)) (cdr1 argl (cdr cdr1)) (argi nil nil)
       (cdrl nil (cons (margs (car cdr1)) cdrl)))
      ((null cdr1)
       (do ((ans nil (cons (if bottom (funcer fn carargl)
			       (fmap1 fn carargl fmapcaarl))
			   ans))
	    (carargl nil nil) (cdrargl nil nil)
	    (cdrl cdrl cdrargl) (bottom nil nil)
	    (done (when (member nil cdrl :test #'eq)
		    (when (dolist (e cdrl) (if e (return t)))
		      (when $maperror
			(merror (intl:gettext "fullmap: arguments must have same formal structure.")))
		      (when $mapprint
			(mtell (intl:gettext "fullmap: truncating one or more arguments.~%"))))
		    t)))
	   (done (mcons-op-args op (nreverse ans)))
	 (do ((op (or (setq bottom (or (zerop fmaplvl) (mapatom (caar cdrl))))
		      (mop (caar cdrl))))
	      (eleml cdrl (cdr eleml)) (caareleml nil nil))
	     ((null eleml)
	      (when (and done (dolist (e cdrargl) (if e (return t))))
		(if $maperror
		    (merror (intl:gettext "fullmap: arguments must have same formal structure.")))
		(if $mapprint (mtell (intl:gettext "fullmap: truncating one or more arguments.~%")))))
	   (setq caareleml (caar eleml))
	   (or bottom
	       (setq bottom
		     (or (mapatom caareleml)
			 (not (alike1 op (mop caareleml)))
			 (and fmapcaarl (not (eq (caar caareleml) fmapcaarl))))))
	   (or done (setq done (null (cdar eleml))))
	   (setq carargl (nconc (ncons caareleml) carargl)
		 cdrargl (nconc cdrargl (ncons (cdar eleml)))))))
    (setq argi (car cdr1))
    (if (or (mapatom argi)
	    (not (alike1 op (mop argi)))
	    (and fmapcaarl (not (eq (caar argi) fmapcaarl))))
	(cond ($maperror (merror (intl:gettext "fullmap: arguments must have same operators.")))
	      (t (if $mapprint (mtell (intl:gettext "fullmap: calling 'apply'.~%")))
		 (return (funcer fn argl)))))))

(defmspec $matrixmap (l)
  (let ((fmaplvl 2))
    (apply #'fmapl1 (mmapev l))))

(defmspec $fullmapl (l)
  (apply #'fmapl1 (mmapev l)))

(defmfun fmapl1 (fun &rest args)
  (let* ((header '(mlist))
	 (argl (fmap1 fun
		      (mapcar #'(lambda (z)
				  (cond ((not (mxorlistp z))
					 (merror (intl:gettext "fullmapl: argument must be a list or matrix; found: ~M") (or (and (consp z) (mop z)) z)))
					((eq (caar z) '$matrix)
					 (setq header '($matrix))
					 (cons '(mlist simp) (cdr z)))
					(t z)))
			      args)
		      'mlist)))
    (if (dolist (e (cdr argl))
	  (unless ($listp e) (return t)))
	argl
	(cons header (cdr argl)))))

(defun $outermap (x y &rest z)
  (if z
    (apply #'outermap1 x y z)
    (fmapl1 x y)))

(defmfun outermap1 n
  (let (outargs1 outargs2)
    (declare (special outargs1 outargs2))
    (cond ((mxorlistp (arg 2))
	   (setq outargs1 (ncons (arg 1))
		 outargs2 (listify (- 2 n)))
	   (fmapl1 #'outermap2 (arg 2)))
	  (t (do ((i 3 (1+ i)))
		 ((> i n) (funcer (arg 1) (listify (- 1 n))))
	       (when (mxorlistp (arg i))
		 (setq outargs1 (listify (1- i))
		       outargs2 (if (< i n) (listify (- i n))))
		 (return (fmapl1 #'outermap2 (arg i)))))))))

(defmfun outermap2 (&rest args)
  (declare (special outargs1 outargs2))
  (unless (null args)
    (apply #'outermap1 (append outargs1 (list (first args)) outargs2))))

(defmfun funcer (fn args)
  (cond ((member fn '(mplus mtimes mexpt mnctimes) :test #'eq)
	 (simplify (cons (ncons fn) args)))
	((or (member fn '(outermap2 constfun) :test #'eq)
	     (and $transrun (symbolp fn) (get fn 'translated)
		  (not (mget fn 'local-fun)) (fboundp fn)))
	 (apply fn (mapcar #'simplify args)))
	(t (mapply1 fn (mapcar #'simplify args) fn
		    nil	;; try to get more info to pass
		    ))))

(defmspec $qput (l)
  (setq l (cdr l))
  (unless (= (length l) 3)
    (wna-err '$qput))
  ($put (car l) (cadr l) (caddr l)))

(defmfun $rem (atom ind)
  (prop1 '$rem atom nil ind))

(defmfun $put (atom val ind)
  (prog1
      (prop1 '$put atom val ind)
    (add2lnc atom $props)))

(defun prop1 (fun atom val ind)
  (unless (or (symbolp atom) (stringp atom))
    (merror (intl:gettext "~:M: argument must be a symbol or a string; found: ~M") fun atom))
  (unless (or (symbolp ind) (stringp ind))
    (merror (intl:gettext "~:M: indicator must be a symbol or a string; found: ~M") fun ind))
  (unless (symbolp atom)
    (setq atom (intern atom)))
  (unless (symbolp ind)
    (setq ind (intern ind)))
  (let ((u (mget atom '$props)))
    (cond ((eq fun '$get) (and u (old-get u ind)))
	  ((eq fun '$rem) (and u (zl-remprop u ind) '$done))
	  ((not u) (mputprop atom (list nil ind val) '$props) val)
	  (t (putprop u val ind)))))

(defmspec $declare (l)
  (setq l (cdr l))
  (when (oddp (length l))
    (merror (intl:gettext "declare: number of arguments must be a multiple of 2.")))
  (do ((l l (cddr l)) (vars) (flag nil nil))
      ((null l)
       '$done)
    (cond (($listp (cadr l))
	   (do ((l1 (cdadr l) (cdr l1))) ((if (null l1) (setq flag t)))
	     (meval `(($declare) ,(car l) ,(car l1)))))
	  ((nonsymchk (cadr l) '$declare))
	  (t (setq vars (declsetup (car l) '$declare))))
    (cond (flag)
	  ((member (cadr l) '($evfun $evflag $nonarray $bindtest) :test #'eq)
	   (declare1 vars t (stripdollar (cadr l)) nil))
	  ((eq (cadr l) '$noun)
	   (dolist (var vars) (alias (getopr var) ($nounify var))))
	  ((member (cadr l) '($nonscalar $scalar $mainvar) :test #'eq)
	   (declare1 vars t (cadr l) t))
	  ((eq (cadr l) '$alphabetic) (declare1 vars t t '$alphabetic))
	  ((member (cadr l) opers :test #'eq)
	   (if (member (cadr l) (cdr $features) :test #'eq) (declare1 vars t (cadr l) 'kind))
	   (declare1 (mapcar #'getopr vars) t (cadr l) 'opers))
	  ((member (cadr l) (cdr $features) :test #'eq) (declare1 vars t (cadr l) 'kind))
	  ((eq (cadr l) '$feature)
	   (dolist (var vars) (nonsymchk var '$declare) (add2lnc var $features)))
	  (t (merror (intl:gettext "declare: unknown property ~:M") (cadr l))))))

(defun declare1 (vars val prop mpropp)
  (dolist (var vars)
    (setq var (getopr var))
    (unless (or (symbolp var) (stringp var))
      (merror (intl:gettext "declare: argument must be a symbol or a string; found: ~M") var))
    (cond ((eq mpropp 'kind) (declarekind var prop))
	  ((eq mpropp 'opers)
	   (putprop (setq var (linchk var)) t prop) (putprop var t 'opers)
	   (if (and (symbolp var) (not (get var 'operators)))
         (putprop var 'simpargs1 'operators)))

	  ((eq mpropp '$alphabetic)
       ; Explode var into characters and put each one on the *alphabet* list,
       ; which is used by src/nparse.lisp .
       (dolist (1-char (coerce var 'list))
         (add2lnc 1-char *alphabet*)))

	  (mpropp
	   (if (and (member prop '($scalar $nonscalar) :test #'eq)
		    (mget var (if (eq prop '$scalar) '$nonscalar '$scalar)))
	       (merror (intl:gettext "declare: inconsistent declaration ~:M") `(($declare) ,var ,prop)))
	   (mputprop var val prop))
	  (t (putprop var val prop)))
    (if (and (safe-get var 'op) (operatorp1 var)
	     (not (member (setq var (get var 'op)) (cdr $props) :test #'eq)))
	(setq *mopl* (cons var *mopl*)))
    (add2lnc (getop var) $props)))

(defun linchk (var)
  (if (member var '($sum $integrate $limit $diff $transpose) :test #'eq)
      ($nounify var)
      var))

(defmspec $remove (form)
  (i-$remove (cdr form)))

(defmfun i-$remove (l)
  (when (oddp (length l))
    (merror (intl:gettext "remove: number of arguments must be a multiple of 2.")))
  (do ((l l (cddr l)) (vars) (flag nil nil)) ((null l) '$done)
    (cond (($listp (cadr l))
	   (do ((l1 (cdadr l) (cdr l1))) ((if (null l1) (setq flag t)))
	     (i-$remove (list (car l) (car l1)))))
      ((unless (or (symbolp (cadr l)) (stringp (cadr l)))
        (merror (intl:gettext "remove: argument must be a symbol or a string; found: ~M") (cadr l))))
	  (t (setq vars (declsetup (car l) '$remove))))
    (cond (flag)
	  ((eq (cadr l) '$value) (i-$remvalue vars))
	  ((eq (cadr l) '$function)
	   (remove1 (mapcar #'$verbify vars) 'mexpr t $functions t))
	  ((eq (cadr l) '$macro)
	   (remove1 (mapcar #'$verbify vars) 'mmacro t $macros t))
	  ((eq (cadr l) '$array) (meval `(($remarray) ,@vars)))
	  ((member (cadr l) '($alias $noun) :test #'eq) (remalias1 vars (eq (cadr l) '$alias)))
	  ((eq (cadr l) '$matchdeclare) (remove1 vars 'matchdeclare t t nil))
	  ((eq (cadr l) '$rule) (remrule vars))
	  ((member (cadr l) '($evfun $evflag $nonarray $bindtest
			    $autoload $assign) :test #'eq)
	   (remove1 vars (stripdollar (cadr l)) nil t nil))
	  ((member (cadr l) '($mode $modedeclare) :test #'eq) (remove1 vars 'mode nil 'foo nil))
	  ((eq (cadr l) '$atvalue) (remove1 vars 'atvalues t t nil))
	  ((member (cadr l) '($nonscalar $scalar $mainvar $numer $atomgrad) :test #'eq)
	   (remove1 vars (cadr l) t t nil))
	  ((member (cadr l) opers :test #'eq) (remove1 (mapcar #'linchk vars) (cadr l) nil t nil))
	  ((member (cadr l) (cdr $features) :test #'eq) (remove1 vars (cadr l) nil t nil))
	  ((eq (cadr l) '$feature)
	   (dolist (var vars)
	     (setf $features (delete var $features :count 1 :test #'eq))))
	  ((member (cadr l) '($alphabetic $transfun) :test #'eq)
	   (remove1 vars (cadr l) nil t nil))
	  ((member (cadr l) '($gradef $grad) :test #'eq) (remove1 vars 'grad nil $gradefs t))
	  ((member (cadr l) '($dependency $depend $depends) :test #'eq)
	   (remove1 vars 'depends t $dependencies t))
	  ((member (cadr l) '($op $operator) :test #'eq) (remove1 vars '$op nil 'foo nil))
	  ((member (cadr l) '($deftaylor $taylordef) :test #'eq) (remove1 vars 'sp2 nil t nil))
	  (t (merror (intl:gettext "remove: unknown property ~:M") (cadr l))))))

(defun declsetup (x fn)
  (cond ((atom x) (ncons x))
	((eq (caar x) '$nounify) (ncons (meval x)))
	((eq (caar x) 'mlist)
	 (mapcar #'(lambda (var)
		     (cond ((atom var) var)
			   ((eq (caar var) '$nounify) (meval var))
			   (t (improper-arg-err var fn))))
		 (cdr x)))
	(t (improper-arg-err x fn))))

(defmfun remove1 (vars prop mpropp info funp)
  (do ((vars vars (cdr vars)) (allflg))
      ((null vars))
    (unless (or (symbolp (car vars)) (stringp (car vars)))
      (merror (intl:gettext "remove: argument must be a symbol or a string; found: ~M") (car vars)))
    (cond ((and (eq (car vars) '$all) (null allflg))
	   (setq vars (append vars (cond ((atom info) (cdr $props))
					 (funp (mapcar #'caar (cdr info)))
					 (t (cdr info))))
		 allflg t))
	  (t
        (if (and (stringp (car vars)) (eq prop '$op) (getopr0 (car vars)))
          (kill-operator (getopr0 (car vars))))

	   (let ((var  (getopr (car vars)))( flag  nil))
	     (cond (mpropp (mremprop var prop)
			   (when (member prop '(mexpr mmacro) :test #'eq)
			     (mremprop var 'mlexprp)
			     (mremprop var 'mfexprp)
			     (remprop var 'lineinfo)
			     (if (mget var 'trace)
				 (macsyma-untrace var))))
		   ((and (eq prop '$alphabetic) (stringp var))
	    (dolist (1-char (coerce var 'list))
	      (setf *alphabet* (delete 1-char *alphabet* :count 1 :test #'equal))))
		   ((eq prop '$transfun)
		    (remove-transl-fun-props var)
		    (remove-transl-array-fun-props var))
		   ((or (setq flag (member prop (cdr $features) :test #'eq)) (member prop opers :test #'eq))
		    (if flag (unkind var prop))
		    (zl-remprop var prop)
		    (if (not (getl var (delete prop (copy-list opers) :count 1 :test #'eq)))
			(zl-remprop var 'opers)))
		   (t (zl-remprop var prop)))
	     (cond ((eq info t) (rempropchk var))
		   ((eq info 'foo))
		   (funp
		    (mfunction-delete var info))
		   (t
		    (setf info (delete var info :count 1 :test #'eq)))))))))

(defun remove-transl-fun-props (fun)
  (if (mget fun 'trace)
      (macsyma-untrace fun))
  (when (and (get fun 'translated) (not (eq $savedef '$all)))
    (fmakunbound fun)
    (zl-remprop fun 'translated-mmacro)
    (zl-remprop fun 'function-mode)
    (when (not (getl fun '(a-expr a-subr)))
	(zl-remprop fun 'once-translated)
	(zl-remprop fun 'translated))))

(defun remove-transl-array-fun-props (fun)
  (when (and (get fun 'translated) (not (eq $savedef '$all)))
    (zl-remprop fun 'a-expr)
    (zl-remprop fun 'a-subr)
    (if (not (fboundp fun)) (zl-remprop fun 'translated))))

(defmfun rempropchk (var)
  (if (and 
        (or
          (not (symbolp var))
          (and
            (not (mgetl var '($nonscalar $scalar $mainvar $numer
                                        matchdeclare $atomgrad atvalues)))
            (not (getl var '(evfun evflag translated nonarray bindtest
                                   sp2 operators opers data autoload mode)))))
	   (not (member var *builtin-$props* :test #'equal)))
      (delete var $props :count 1 :test #'equal)))

(defmspec $remfunction (l)
  (setq l (cdr l))
  (cond ((member '$all l :test #'eq)
	 (setq l (nconc (mapcar #'caar (cdr $functions))
			(mapcar #'caar (cdr $macros)))))
	(t (setq l (mapcar #'$verbify l))
	   (do ((l1 l (cdr l1))) ((null l1) t)
	     (if (not (or (assoc (ncons (car l1)) (cdr $functions) :test #'equal)
			  (assoc (ncons (car l1)) (cdr $macros) :test #'equal)))
		 (rplaca l1 nil)))))
  (remove1 l 'mexpr t $functions t)
  (remove1 l 'mmacro t $macros t)
  (cons '(mlist) l))

(defmspec $remarray (l)
  (setq l (cdr l))
  (cons '(mlist)
	(do ((l l (cdr l)) (x) (pred)) ((null l) (nreverse x))
	  (cond ((eq (car l) '$all) (setq l (append l (cdr $arrays))))
		(t (remcompary (car l)) (setq pred (mremprop (car l) 'array))
		   (setq pred (or (mremprop (car l) 'hashar) pred))
		   (setq pred (or (mremprop (car l) 'aexpr) pred))
		   (setq x (cons (and pred (prog2
					       (setf $arrays (delete (car l) $arrays :count 1 :test #'eq))
					       (car l)))
				 x)))))))

(defun remcompary (x)
  (cond ((eq x (mget x 'array))
	 (zl-remprop x 'array-mode)
	 (zl-remprop x 'array))))

(defmspec $remvalue (form)
  (i-$remvalue (cdr form)))

(defmfun i-$remvalue (l)
  (cons '(mlist)
	(do ((l l (cdr l)) (x) (y)) ((null l) (nreverse x))
	  (cond ((eq (car l) '$all) (setq l (append l (cdr $values))))
		(t (setq x (cons (cond ((atom (car l))
					(if (remvalue (car l) '$remvalue) (car l)))
				       ((setq y (mgetl (caaar l) '(hashar array)))
					(remarrelem y (car l)) (car l)))
				 x)))))))

(defmfun remarrelem (ary form)
  (let ((y (car (arraydims (cadr ary)))))
    (arrstore form (cond ((eq y 'fixnum) 0) ((eq y 'flonum) 0.0) (t munbound)))))

(defmfun remrule (l)
  (do ((l l (cdr l)) (u))
      ((null l))
    (cond ((eq (car l) '$all) (setq l (append l (cdr $rules))))
	  ((get (car l) 'operators) ($remrule (car l) '$all))
	  ((setq u (ruleof (car l))) ($remrule u (car l)))
	  ((mget (car l) '$rule)
	   (zl-remprop (car l) 'expr) (mremprop (car l) '$rule)
	   (setf $rules (delete (car l) $rules :count 1 :test #'eq))))))

(defmfun remalias1 (l aliasp)
  (do ((l l (cdr l)) (u)) ((null l))
    (cond ((eq (car l) '$all) (setq l (append l (cdr $aliases))))
	  ((or aliasp (get (car l) 'noun)) (remalias (car l) t))
	  ((setq u (get (car l) 'verb))
	   (zl-remprop (car l) 'verb) (zl-remprop u 'noun)))))

(defmfun mremprop (atom ind)
  (let ((props (get atom 'mprops))) (and props (zl-remprop props ind))))

(defmfun mgetl (atom inds)
  (let ((props (get atom 'mprops))) (and props (getl props inds))))

(defmspec $matrix (L)
  (if (null (cdr L))
    '(($matrix))
    (let ((rows (mapcar #'meval (cdr L))))
      (dolist (row rows)
	(if (not ($listp row)) (merror (intl:gettext "matrix: row must be a list; found: ~M") row)))
      (matcheck rows)
      (cons '($matrix) rows))))

(defmfun matcheck (l)
  (do ((l1 (cdr l) (cdr l1)) (n (length (car l)))) ((null l1))
    (if (not (= n (length (car l1))))
	(merror (intl:gettext "matrix: all rows must be the same length.")))))

(defun harrfind (form)
  (prog (ary y lispsub iteml sub ncells nitems)
     (setq ary (symbol-array (mget (caar form) 'hashar)))
     (cond ((not (= (aref ary 2) (length (cdr form))))
	    (merror (intl:gettext "evaluation: array ~:M must have ~:M indices; found: ~M")
		    (caar form) (aref ary 2) form)))
     (setq sub (cdr form))
     (setq iteml (aref ary (setq lispsub (+ 3 (rem (hasher sub) (aref ary 0))))))
     a    (cond ((null iteml) (go b))
		((alike (caar iteml) sub) (return (cdar iteml))))
     (setq iteml (cdr iteml))
     (go a)
     b    (cond (evarrp (throw 'evarrp 'notexist))
		((null (setq y (arrfunp (caar form)))) (return (meval2 sub form))))
     (setq y (arrfuncall y sub form))
     (setq ary (symbol-array (mget (caar form) 'hashar)))
     (setq iteml (aref ary (setq lispsub (+ 3 (rem (hasher sub) (aref ary 0))))))
     (setq sub (ncons (cons sub y)))
     (cond (iteml (nconc iteml sub)) (t (setf (aref ary lispsub) sub)))
     (setf (aref ary 1) (setq nitems (1+ (aref ary 1))))
     (cond ((> nitems (setq ncells (aref ary 0)))
	    (arraysize (caar form) (+ ncells ncells))))
     (return y)))

(defun arrfind (form)
  (let ((sub (cdr form)) u v type)
    (setq v (dimcheck (caar form) sub nil))
    (cond (v (setq type (car (arraydims (mget (caar form) 'array))))))
    (cond ((and v (prog2
		      (setq u (apply 'aref (symbol-array (mget (caar form) 'array)) sub))
		      (cond ((eq type 'flonum) (not (= u flounbound)))
			    ((eq type 'fixnum) (not (= u fixunbound)))
			    (t (not (eq u munbound))))))
	   u)
	  (evarrp (throw 'evarrp 'notexist))
	  ((or (not v) (null (setq u (arrfunp (caar form)))))
	   (cond ((eq type 'flonum) 0.0)
		 ((eq type 'fixnum) 0)
		 (t (meval2 sub form))))
	  (t (setq u (arrfuncall u sub form))
	     (setf (apply #'aref (symbol-array (mget (caar form) 'array))
			  sub) u)

	     u))))

(defmspec $array (x)
  (setq x (cdr x))
  (cond ($use_fast_arrays
         (let ((type (if (symbolp (cadr x)) (cadr x) '$any))
               (name (car x))
               (diml (if (symbolp (cadr x)) (cddr x) (cdr x))))
           (mset name
                 (apply '$make_array
                        type
                        (mapcar #'(lambda (dim)
                                  ;; let make_array catch bad vals
                                    (add 1 (meval dim)))
                                diml)))))
	((symbolp (car x))
	 (let ((compp (assoc (cadr x) '(($complete . t) ($integer . fixnum) ($fixnum . fixnum)
					($float . flonum) ($flonum . flonum)))))
	   (let ((fun ($verbify (car x)))
		 (diml (cond (compp (setq compp (cdr compp))
				    (cddr x))
			     (t (cdr x))))
		 funp
		 old
		 new
		 (ncells 0))
	     (when (member '$function diml :test #'eq)
	       (setq diml (delete '$function diml :count 1 :test #'eq)
		     funp t))
	     (setq diml (mapcar #'meval diml))
	     (cond ((null diml)
		    (wna-err '$array))
		   ((> (length diml) 5)
		    (merror (intl:gettext "array: number of dimensions must be 5 or less; found: ~M") (length diml)))
		   ((member nil (mapcar #'(lambda (u) (eq (ml-typep u) 'fixnum)) diml)
			    :test #'eq)
		    (merror (intl:gettext "array: all dimensions must be integers."))))
	     (setq diml (mapcar #'1+ diml))
	     (setq new (if compp fun (gensym)))
	     (setf (symbol-array new) 
	           (make-array diml :initial-element (case compp
	                                               (fixnum 0)
	                                               (flonum 0.0)
	                                               (otherwise munbound))))
	     (when (or funp (arrfunp fun))
	       (fillarray new (list (if (eq compp 'fixnum) fixunbound flounbound))))
	     (cond ((null (setq old (mget fun 'hashar)))
		    (mputprop fun new 'array))
		   (t (unless (= (aref (symbol-array old) 2) (length diml))
			(merror (intl:gettext "array: array ~:M must have ~:M dimensions; found: ~M") fun (aref (symbol-array old) 2) (length diml)))
		      (setq ncells (+ 2 (aref (symbol-array old) 0)))
		      (do ((n 3 (1+ n)))
			  ((> n ncells))
			(do ((items (aref (symbol-array old) n) (cdr items)))
			    ((null items))
			  (do ((x (caar items) (cdr x)) (y diml (cdr y)))
			      ((null x)
			       (if (and (member compp '(fixnum flonum) :test #'eq)
					(not (eq (ml-typep (cdar items)) compp)))
				   (merror (intl:gettext "array: existing elements must be ~M; found: ~M") compp (cdar items)))
			       (setf (apply #'aref (symbol-array new) (caar items))
				     (cdar items)))
			    (if (or (not (eq (ml-typep (car x)) 'fixnum))
				    (< (car x) 0)
				    (not (< (car x) (car y))))
				(merror (intl:gettext "array: index must be nonnegative integer less than ~M; found: ~M") (car y) (car x))))))
		      (mremprop fun 'hashar)
		      (mputprop fun new 'array)))
	     (add2lnc fun $arrays)
	     (when (eq compp 'fixnum)
	       (putprop fun '$fixnum 'array-mode))
	     (when (eq compp 'flonum)
	       (putprop fun '$float 'array-mode))
	     fun)))
	(($listp (car x))
	 (dolist (u (cdar x))
	   (meval `(($array) ,u ,@(cdr x))))
	 (car x))
	(t
	 (merror (intl:gettext "array: first argument must be a symbol or a list; found: ~M") (car x)))))


(defmfun $show_hash_array (x)
  (maphash #'(lambda (k v) (format t "~%~A-->~A" k v)) x))

;; If this is T then arrays are stored in the value cell,
;; whereas if it is false they are stored in the function cell
(defmvar $use_fast_arrays nil)

(defmfun arrstore (l r &aux tem index)
  (cond ($use_fast_arrays
	 (cond ((and (boundp (caar l)) (setq tem (symbol-value (caar l))))
		(setq index (mevalargs (cdr l)))
		(let ((the-type (ml-typep tem)))
		  (cond ((eq the-type 'array)
			 (setf (apply #'aref tem index)  r))
			((eq the-type 'hash-table)
             ;; SINCE A "FAST ARRAY" IS JUST A HASH TABLE, SEEMS LIKE THESE !!
             ;; TESTS ON THE NUMBER OF INDICES OBSTRUCTS LEGITIMATE HASH TABLE !!
             ;; USE; I RECOMMEND CUTTING THEM OUT !!
			 (cond ((gethash 'dim1 tem)
				(if (cdr index)
				    (merror (intl:gettext "assignment: array has one dimension, but two or more indices supplied."))))
			       (t (or (cdr index)
				      (merror (intl:gettext "assignment: array has two or more dimensions, but one index supplied.")))))
			 (setf (gethash	(if (cdr index)
					    index
					    (car index))
					tem) r))
			((eq the-type 'list)
			 (cond ((eq (caar tem) 'mlist)
				(setq index (car index))
				(setf (nth index tem) r)
				r)
			       ((eq (caar tem) '$matrix)
				(setf (nth (second index) (nth (first index) tem)) r)
				r)
			       (t
				(merror (intl:gettext "assignment: ~A is not a hash table, an array, Maxima list, or a matrix.")
                        (caar l)))))
			(t
			 (cond ((eq tem (caar l))
				(meval* `((mset) ,(caar l)
					  ,(make-equal-hash-table
					    (cdr (mevalargs (cdr l))))))
				(arrstore l r))
			       (t
				(merror (intl:gettext "assignment: ~A is not a hash table, an array, a Maxima list, or a matrix.")
                        (caar l))))))))
	       (t
		(cond ((mget (caar l) 'hashar)
		       (let ($use_fast_arrays)
			 (arrstore l r)))
		      (t
		       (meval* `((mset) ,(caar l)
				 ,(make-equal-hash-table
				   (cdr (mevalargs (cdr l))))))
		       (arrstore l r))))))
	(t
	 (let ((fun ($verbify (caar l))) ary sub (lispsub 0) hashl mqapplyp)
	   (cond ((setq ary (mget fun 'array))
		  (dimcheck fun (setq sub (mapcar #'meval (cdr l))) t)
		  (if (and (member (setq fun (car (arraydims ary))) '(fixnum flonum) :test #'eq)
			   (not (eq (ml-typep r) fun)))
		      (merror (intl:gettext "assignment: attempt to assign ~M to an array of type ~M.") r fun))
		  (setf (apply #'aref (symbol-array ary) sub)  r))
		 ((setq ary (mget fun 'hashar))
		  (if (not (= (aref (symbol-array ary) 2) (length (cdr l))))
		      (merror (intl:gettext "assignment: array ~:M has dimension ~:M, but it was called by ~:M")
			      fun (aref (symbol-array ary) 2) l))
		  (setq sub (mapcar #'meval (cdr l)))
		  (setq hashl (aref (symbol-array ary)
				    (setq lispsub (+ 3 (rem (hasher sub)
							    (aref (symbol-array ary) 0))))))
		  (do ((hashl1 hashl (cdr hashl1)))
		      ((null hashl1)
		       (cond ((not (eq r munbound))
			      (setq sub (ncons (cons sub r)))
			      (cond ((null hashl) (setf (aref (symbol-array ary) lispsub) sub))
				    (t (nconc hashl sub)))
			      (setf (aref (symbol-array ary) 1) (1+ (aref (symbol-array ary) 1))))))
		    (cond ((alike (caar hashl1) sub)
			   (cond ((eq r munbound) (setf (aref (symbol-array ary) 1)
							 (1- (aref (symbol-array ary) 1))))
				 (t (nconc hashl (ncons (cons sub r)))))
			   (setf (aref (symbol-array ary) lispsub)
				  (delete (car hashl1) hashl :count 1 :test #'equal))
			   (return nil))))
		  (if (> (aref (symbol-array ary) 1) (aref (symbol-array ary) 0))
		      (arraysize fun (* 2 (aref (symbol-array ary) 0))))
		  r)
		 ((and (eq fun 'mqapply) (mxorlistp (setq ary (meval (cadr l))))
		       (prog2
			   (setq mqapplyp t l (cdr l))
			   nil)))
		 ((and (not mqapplyp)
		       (or (not (boundp fun))
		           (not (or (mxorlistp (setq ary (symbol-value fun)))
		                    (arrayp ary)
		                    (typep ary 'hash-table)
		                    (eq (type-of ary) 'mgenarray)))))
		  (if (member fun '(mqapply $%) :test #'eq) (merror (intl:gettext "assignment: cannot assign to ~M") l))
		  (add2lnc fun $arrays)
		  (setq ary (gensym))
		  (mputprop fun ary 'hashar)
		  (setf (symbol-array ary) (make-array 7 :initial-element nil))
		  (setf (aref (symbol-array ary) 0) 4)
		  (setf (aref (symbol-array ary) 1) 0)
		  (setf (aref (symbol-array ary) 2) (length (cdr l)))
		  (arrstore l r))
	         ((or (arrayp ary)
	              (typep ary 'hash-table)
	              (eq (type-of ary) 'mgenarray))
		  (arrstore-extend ary (mevalargs (cdr l)) r))
		 ((or (eq (caar ary) 'mlist) (= (length l) 2))
		  (cond ((eq (caar ary) '$matrix)
			 (cond ((or (not ($listp r)) (not (= (length (cadr ary)) (length r))))
				(merror (intl:gettext "assignment: matrix row must be a list, and same length as first row;~%found:~%~M") r))))
			((not (= (length l) 2))
			 (merror (intl:gettext "assignment: matrix row must have one index; found: ~M") (cons '(mlist) (cdr l)))))
		  (let ((index (meval (cadr l))))
		    (cond ((not (eq (ml-typep index) 'fixnum))
			   (merror (intl:gettext "assignment: matrix row index must be an integer; found: ~M") index))
			  ((and (> index 0) (< index (length ary)))
			   (rplaca (nthcdr (1- index) (cdr ary)) r))
			  (t (merror (intl:gettext "assignment: matrix row index ~A out of range.") index))))
		  r)
		 (t (if (not (= (length l) 3))
			(merror (intl:gettext "assignment: matrix must have two indices; found: ~M") (cons '(mlist) (cdr l))))
		    ($setelmx r (meval (cadr l)) (meval (caddr l)) ary)
		    r))))))

(defun arrfunp (x)
  (or (and $transrun (getl x '(a-expr))) (mgetl x '(aexpr))))

(defun arrfuncall (arrfun subs form)
  (let ((aexprp t))
    (case (car arrfun)
      (aexpr (mapply1 (cadr arrfun) subs (cadr arrfun) form))
      (a-expr (apply (cadr arrfun) subs))
      (a-subr (apply (cadr arrfun) subs)))))

(defun hasher (l)  ; This is not the best way to write a hasher.  But,
  (if (null l)	   ; please don't change this code or you're liable to
      0					; break SAVE files.
      (logand #o77777
	      (let ((x (car l)))
		(cond (($ratp x) (merror (intl:gettext "hash function: cannot hash a special expression (CRE or Taylor).")))
		      ((or (fixnump x) (floatp x))
		       (+ (if (fixnump x) x (floor (+ x 5e-4)))
			   (* 7 (hasher (cdr l)))))
		      ((atom x) (+ (sxhash x) (hasher (cdr l))))
		      (t (+ 1 (sxhash (caar x)) (hasher (cdr x))
			     (hasher (cdr l)))))))))

(defun arraysize (fun n)
  (prog (old new indx ncells cell item i y)
     (setq old (symbol-array (mget fun 'hashar)))
     (setq new (gensym))
     (mputprop fun new 'hashar)
     (setf (symbol-array new) (make-array (+ n 3) :initial-element nil))
     (setq new (symbol-array new))
     (setf (aref new 0) n)
     (setf (aref new 1) (aref old 1))
     (setf (aref new 2) (aref old 2))
     (setq indx 2 ncells (+ 2 (aref old 0)))
     a    (if (> (setq indx (1+ indx)) ncells) (return t))
     (setq cell (aref old indx))
     b    (if (null cell) (go a))
     (setq i (+ 3 (rem (hasher (car (setq item (car cell)))) n)))
     (if (setq y (aref new i))
	 (nconc y (ncons item))
	 (setf (aref new i) (ncons item)))
     (setq cell (cdr cell))
     (go b)))

(defun dimcheck (ary sub fixpp)
  (do ((x sub (cdr x))
       (ret t)
       (y (cdr (arraydims (mget ary 'array))) (cdr y)))
      ((null y)
       (if x (merror (intl:gettext "Array ~:M has dimensions ~:M, but was called with ~:M")
		     ary
		     `((mlist) ,@(mapcar #'1- (cdr (arraydims (mget ary 'array)))))
		     `((mlist) ,@sub))
	   ret))
    (cond ((or (null x) (and (eq (ml-typep (car x)) 'fixnum)
			     (or (< (car x) 0) (not (< (car x) (car y))))))
	   (setq y nil x (cons nil t)))
	  ((not (fixnump (car x)) )
	   (if fixpp (setq y nil x (cons nil t)) (setq ret nil))))))

(defun constlam (x &aux (lam x))
  (if aexprp
      `(,(car lam) ,(cadr lam) ,@(mbinding ((mparams (cadr lam)))
					   (mapcar #'meval (cddr lam))))

      lam))

(defmspec $define (l)
  (twoargcheck l)
  (setq l (cdr l))
  (meval `((mdefine)
	   ,(cond ((mquotep (car l)) (cadar l))
		  ((and (not (atom (car l)))
			(member (caaar l) '($ev $funmake $arraymake) :test #'eq))
		   (meval (car l)))
		  (t (disp2 (car l))))
	   ,(meval (cadr l)))))

(defun set-lineinfo (fnname lineinfo body)
  (cond ((and (consp lineinfo) (eq 'src (third lineinfo)))
	 (setf (cdddr lineinfo) (list fnname (first lineinfo)))
	 (setf (get fnname 'lineinfo) body))
	(t (remprop fnname 'lineinfo))))

(defmspec mdefine (l )
  (let ($use_fast_arrays) ;;for mdefine's we allow use the oldstyle hasharrays
    (twoargcheck l)
    (setq l (cdr l))
    (let ((fun (car l)) (body (cadr l)) args subs ary fnname mqdef redef)
      (cond ((or (atom fun)
		 (and (setq mqdef (eq (caar fun) 'mqapply))
		      (member 'array (cdar fun) :test #'eq)))
	     (merror (intl:gettext "define: argument cannot be an atom or a subscripted memoizing function; found: ~M") fun))
	    (mqdef (if (or (atom (cadr fun))
			   (not (setq ary (member 'array (cdaadr fun) :test #'eq))))
		       (merror (intl:gettext "define: expected a subscripted expression; found: ~M") (cadr fun)))
		   (setq subs (cdadr fun) args (cddr fun) fun (cadr fun)
			 fnname ($verbify (caar fun)))
		   (if (and (not (mgetl fnname '(hashar array)))
			    (get fnname 'specsimp))
		       (mtell (intl:gettext "define: warning: redefining built-in subscripted function ~:M~%")
			      fnname)))
	    ((prog2 (setq fnname ($verbify (caar fun)))
		 (or (mopp fnname) (member fnname '($all $allbut $%) :test #'eq)))
	     (merror (intl:gettext "define: function name cannot be a built-in operator or special symbol; found: ~:@M") fnname))
	    ((setq ary (member 'array (cdar fun) :test #'eq)) (setq subs (cdr fun)))
	    (t (setq args (cdr fun) redef (mredef-check fnname))))
      (if (not ary) (remove1 (ncons fnname) 'mmacro t $macros t))
      (mdefchk fnname (or args (and (not mqdef) subs)) ary mqdef)
      (if (not (eq fnname (caar fun))) (rplaca (car fun) fnname))
      (cond ((not ary) (if (and evp (member fnname (car loclist) :test #'eq))
			   (mputprop fnname t 'local-fun)
			   (remove-transl-fun-props fnname))
	     (add2lnc (cons (ncons fnname) args) $functions)
	     (set-lineinfo fnname (cadar fun) body)
	     (mputprop fnname (mdefine1 args body) 'mexpr)
	     (if $translate (translate-function fnname)))
	    ((prog2 (add2lnc fnname $arrays)
	       (setq ary (mgetl fnname '(hashar array)))
	       (remove-transl-array-fun-props fnname))
	     (if (not (= (if (eq (car ary) 'hashar)
			     (aref (symbol-array (cadr ary)) 2)
			     (length (cdr (arraydims (cadr ary)))))
			 (length subs)))
		 (merror (intl:gettext "define: ~:M already defined with different number of subscripts.")
			 fnname))
	     (mdefarray fnname subs args body mqdef))
	    (t
	     (setq ary (gensym))
	     (mputprop fnname ary 'hashar)
	     (setf (symbol-array ary) (make-array 7 :initial-element nil))
	     (setf (aref (symbol-array ary) 0) 4)
	     (setf (aref (symbol-array ary) 1) 0)
	     (setf (aref (symbol-array ary) 2) (length subs))
	     (mdefarray fnname subs args body mqdef)))
      (cons '(mdefine simp) (copy-list l)))))

;; Checks to see if a user is clobbering the name of a system function.
;; Prints a warning and returns T if he is, and NIL if he isn't.
(defun mredef-check (fnname)
  (when (and (not (mget fnname 'mexpr))
	     (or (and (or (get fnname 'autoload)
			  (getl-lm-fcn-prop fnname '(subr)))
		      (not (get fnname 'translated)))
		 (mopp fnname)))
    (format t (intl:gettext "define: warning: redefining the built-in ~:[function~;operator~] ~a~%")
	    (getl fnname '(verb operators))
	    (print-invert-case (stripdollar fnname)))
    t))

(defun mdefarray (fun subs args body mqdef)
  (when (hash-table-p fun)
    ;; PRETTY SURE THIS NEXT MESSAGE IS UNREACHABLE (FUN IS ALWAYS A SYMBOL FROM WHAT I CAN TELL) !!
    (error "~a is already a hash table.  Make it a function first" fun))
  (cond ((and (null args) (not mqdef)) (mputprop fun (mdefine1 subs body) 'aexpr))
	((null (dolist (u subs)
		 (unless (or (consp u) ($constantp u) (stringp u))
		     (return t))))
	 (arrstore (cons (ncons fun) subs) (mdefine1 args body)))
	(t (mdefchk fun subs t nil)
	   (mputprop fun (mdefine1 subs (mdefine1 args body)) 'aexpr))))

(defmfun mspecfunp (fun)
  (and (or (getl-lm-fcn-prop fun '(macro))
	   (getl fun '(mfexpr*))
	   (and $transrun (get fun 'translated-mmacro))
	   (mget fun 'mmacro))
       (not (get fun 'evok))))

(defun mdefine1 (args body)
  (list '(lambda) (cons '(mlist) args) body))

(defun mdefchk (fun args ary mqdef)
  (do ((l args (cdr l)) (mfex) (mlex))
      ((null l) (and mfex (not mqdef) (mputprop fun mfex 'mfexprp))
       (and mlex (not mqdef) (mputprop fun mlex 'mlexprp)))
    (if (not (or (mdefparam (car l))
		 (and (or (not ary) mqdef)
		      (or (and mfexprp (mquotep (car l))
			       (mdefparam (cadar l)) (setq mfex t))
			  (and (mdeflistp l)
			       (or (mdefparam (cadar l))
				   (and mfexprp (mquotep (cadar l))
					(mdefparam (cadr (cadar l)))
					(setq mfex t)))
			       (setq mlex t))))))
	(merror (intl:gettext "define: in definition of ~:M, found bad argument ~M") fun (car l)))))

(defun mdefparam (x)
  (and (atom x) (not (maxima-constantp x)) (not (stringp x))))

(defun mdeflistp (l)
  (and (null (cdr l)) ($listp (car l)) (cdar l) (null (cddar l))))

(defmfun mopp (fun)
  (and (not (eq fun 'mqapply))
       (or (mopp1 fun)
	   (and (get fun 'operators) (not (rulechk fun))
		(not (member fun rulefcnl :test #'eq)) (not (get fun 'opers))))))

(defmfun mopp1 (fun)
  (and (setq fun (get fun 'op)) (not (member fun (cdr $props) :test #'eq))))

;; maybe should have a separate version, or a macro..
(defun mapply (a b c)
  (mapply1 a b c nil))

(defun $apply (fun arg)
  (unless ($listp arg)
    (merror (intl:gettext "apply: second argument must be a list; found: ~M") arg))
  (let ((fun-opr (getopr fun)))
    (autoldchk fun-opr)
    (mapply1 fun-opr (cdr arg) fun `(($apply) ,fun ,arg))))

(defun autoldchk (fun)
  (if (and (symbolp fun)
	   (get fun 'autoload)
	   (not (or (fboundp fun) (mfboundp fun))))
      (load-function fun t)))

(defmspec $dispfun (l)
  (setq l (cdr l))
  (cond ((or (cdr l) (not (eq (car l) '$all))) (dispfun1 l nil nil))
	(t
	 `((mlist simp)
	   ,@(apply #'append
		    (list (cdr (dispfun1 (cdr $functions) t nil))
			  (cdr (dispfun1
				(mapcan #'(lambda (x) (if (mget x 'aexpr) (ncons x)))
					(cdr $arrays)) nil t))
			  (cdr (dispfun1 (cdr $macros) t nil))))))))

(defun dispfun1 (l flag maexprp)
  `((mlist simp)
    ,@(loop for fun in l collect
	    (cadr ($ldisp (consfundef (if flag (caar fun) fun) maexprp nil))))))

(defmspec $fundef (x)
  (consfundef (fexprcheck x) nil nil))

(defun consfundef (x maexprp stringp)
  (prog (arryp name fun)
     (setq arryp (and (not (atom x)) (not (eq (caar x) 'mqapply)) (member 'array (cdar x) :test #'eq)))
     (cond ((atom x) (setq name ($verbify x)
			   fun (or (and (not maexprp) (mgetl name '(mexpr mmacro)))
				   (mgetl name '(aexpr)))))
	   (arryp (setq fun (meval1 (setq name (cons (list ($verbify (caar x)) 'array) (cdr x)))))
		  (if (or (atom fun) (not (eq (caar fun) 'lambda))) (setq fun nil))))
     (cond ((not fun)
            (cond (stringp (return x)) 
                  ((member 'edit state-pdl :test #'eq) (terpri)))
            (merror (intl:gettext "fundef: no such function: ~:M") x)))
     (return
       (cons (if (eq (car fun) 'mmacro) '(mdefmacro simp) '(mdefine simp))
	     (cond (arryp (cons (cons '(mqapply) (cons name (cdadr fun))) (cddr fun)))
		   (t (funcall #'(lambda (body)
				   (cond ((and (eq (car fun) 'aexpr) (not (atom body))
					       (eq (caar body) 'lambda))
					  (list (cons '(mqapply) (cons (cons (cons name '(array))
									     (cdr (cadadr fun)))
								       (cdadr body)))
						(caddr body)))
					 (t (list (cons (cons name (if (eq (car fun) 'aexpr) '(array)))
							(cdr (cadadr fun)))
						  body))))
			       (caddr (cadr fun)))))))))


(defmfun $funmake (fun args)
  (if (not (or (stringp fun) (symbolp fun) ($subvarp fun)
	       (and (not (atom fun)) (eq (caar fun) 'lambda))))
      (merror (intl:gettext "funmake: first argument must be a symbol, subscripted symbol, string, or lambda expression; found: ~M") fun))
  (if (not ($listp args)) (merror (intl:gettext "funmake: second argument must be a list; found: ~M") args))
  (mcons-op-args (getopr fun) (cdr args)))

(defmfun mcons-op-args (op args)
  (if (symbolp op)
      (cons (ncons op) args)
      (list* '(mqapply) op args)))

(defmfun optionp (x)
  (and (boundp x)
       (not (member x (cdr $values) :test #'eq))
       (not (member x (cdr $labels) :test #'eq))))

(defmspec mcond (form)
  (setq form (cdr form))
  (do ((u form (cddr u)) (v))
      ((null u) nil)
    (cond ((eq (setq v (mevalp (car u))) t) (return (meval (cadr u))))
	  (v (return (list* '(mcond) v (mapcar #'meval-atoms (cdr u))))))))

(defun meval-atoms (form)
  (cond ((atom form) (meval1 form))
	((eq (caar form) 'mquote) (cadr form))
	((and (getl (caar form) '(mfexpr*))
	      (not (member (caar form) '(mcond mand mor mnot mprogn mdo mdoin) :test #'eq)))
	 form)
	(t (recur-apply #'meval-atoms form))))

(defmspec mdo (form)
  (setq form (cdr form))
  (let ((mdop t) (my-var (or (car form) 'mdo)) my-step next test do-body)
    (setq my-step (if (caddr form) (meval (caddr form)) 1)
          next (or (cadddr form) (list '(mplus) my-step my-var))
          test (list '(mor)
                     (cond ((null (car (cddddr form))) nil)
                           (t (list (if (mnegp ($numfactor my-step))
                                      '(mlessp)
                                      '(mgreaterp))
                                    my-var (car (cddddr form)))))
                     (cadr (cddddr form)))
          do-body (caddr (cddddr form)))
    (mbinding ((ncons my-var)
               (ncons (if (null (cadr form)) 1 (meval (cadr form)))))
              (do ((val) (bindl bindlist))
                ((is test) '$done)
                (cond ((null (setq val (catch 'mprog (prog2 (meval do-body) nil))))
                       (mset my-var (meval next)))
                      ((atom val) (merror (intl:gettext "do loop: 'go' not within 'block': ~M") val))
                      ((not (eq bindl bindlist))
                       (merror (intl:gettext "do loop: illegal 'return': ~M") (car val)))
                      (t (return (car val))))))))

(defmspec mdoin (form)
  (setq form (cdr form))
  (funcall #'(lambda  (mdop my-var set test action)
	       (setq set (if ($atom (setq set (format1 (meval (cadr form)))))
			     (merror (intl:gettext "do loop: 'in' argument must be a nonatomic expression; found: ~M") set)
			     (margs set))
		     test (list '(mor)
				(if (car (cddddr form))
				    (list '(mgreaterp) my-var (car (cddddr form))))
				(cadr (cddddr form)))
		     action (caddr (cddddr form)))
	       (cond ((atom set) '$done)
		     (t (mbinding ((ncons my-var) (ncons (car set)))
				  (do ((val) (bindl bindlist))
				      ((or (atom set) (is test))
				       '$done)
				    (cond ((null (setq val (catch 'mprog (prog2 (meval action) nil))))
					   (if (setq set (cdr set)) (mset my-var (car set))))
					  ((atom val) (merror (intl:gettext "do loop: 'go' not within 'block': ~M") val))
					  ((not (eq bindl bindlist))
					   (merror (intl:gettext "do loop: illegal 'return': ~M") (car val)))
					  (t (return (car val)))))))))
	   t (or (car form) 'mdo) nil nil nil))

(defmspec mprog (prog)
  (setq prog (cdr prog))
  (let (vars vals (mlocp t))
    (if ($listp (car prog)) (setq vars (cdar prog) prog (cdr prog)))
    (setq loclist (cons nil loclist))
    (do ((l vars (cdr l))) ((null l) (setq vals vars))
      (if (not (atom (car l))) (return (setq vals t))))
    (if (eq vals t)
	(setq vals (mapcar #'(lambda (v)
			       (cond ((atom v) v)
				     ((eq (caar v) 'msetq) (meval (caddr v)))
				     (t (merror
					 (intl:gettext "block: variable list must comprise only atoms and assignment expressions; found: ~M")
					 v))))
			   vars)
	      vars (mapcar #'(lambda (v) (if (atom v) v (cadr v))) vars)))
    (mbinding (vars vals)
	      (do ((prog prog (cdr prog)) (mprogp prog)
		   (bindl bindlist) (val '$done) (retp) (x) ($%% '$%%))
		  ((null prog) (munlocal) val)
		(cond ((atom (car prog))
		       (if (null (cdr prog))
			   (setq retp t val (meval (car prog)))))
		      ((null (setq x (catch 'mprog
				       (prog2 (setq val (setq $%% (meval (car prog))))
					   nil)))))
		      ((not (eq bindl bindlist))
		       (if (not (atom x))
                 ;; DUNNO WHAT'S "ILLEGAL" HERE
			   (merror (intl:gettext "block: illegal 'return': ~M") (car x))
                 ;; DUNNO WHAT'S "ILLEGAL" HERE
			   (merror (intl:gettext "block: illegal 'go': ~M") x)))
		      ((not (atom x)) (setq retp t val (car x)))
		      ((not (setq prog (member x mprogp :test #'equal)))
		       (merror (intl:gettext "block: no such tag: ~:M") x)))
		(if retp (setq prog '(nil)))))))

(defmfun mreturn (&optional (x nil) &rest args)
  (cond 
    ((not (null args))
       (merror (intl:gettext "return: too many arguments; found: ~M") `((mlist) ,x ,@args) ))
    ((and (not mprogp) (not mdop))
       (merror (intl:gettext "return: not within 'block'")))
    (t (throw 'mprog (ncons x)) ) ))

(defmspec mgo (tag)
  (setq tag (fexprcheck tag))
  (cond ((not mprogp) (merror (intl:gettext "go: not within 'block'")))
	((atom tag) (throw 'mprog tag))
	(t (merror (intl:gettext "go: argument must be an atom; found: ~M") tag))))

(defmspec $subvar (l)
  (setq l (cdr l))
  (if (null l)
      (wna-err '$subvar))
  (meval (cons '(mqapply array) l)))

(defmfun rat (x y)
  `((rat simp) ,x ,y))

(defmfun add2lnc (item llist)
  (unless (memalike item (if ($listp llist) (cdr llist) llist))
    (unless (atom item)
      (setf llist (delete (assoc (car item) llist :test #'equal) llist :count 1 :test #'equal)))
    (nconc llist (ncons item))))

(defmfun bigfloatm* (bf)
  (unless (member 'simp (cdar bf) :test #'eq)
    (setq bf (cons (list* (caar bf) 'simp (cdar bf)) (cdr bf))))
  (if $float ($float bf) bf))

(defmfun $allbut (&rest args)
  (cons '($allbut) args))

(defquote dsksetq (&rest l)
  (let ((dsksetp t))
    (mset (car l) (eval (cadr l)))))

(defmfun dskrat (x)
  (orderpointer (caddar x))
  (mapc #'(lambda (a b) (dskrat-subst a b (cddddr (car x))) ; for TAYLOR forms
		  (dskrat-subst a b (cdr x)))
	genvar (cadddr (car x)))
  (rplaca (cdddar x) genvar)
  (if (member 'trunc (car x) :test #'eq)
      (srconvert x) x))			; temporary

(defun dskrat-subst (x y z)
  (cond ((atom z) z)
	(t (if (eq y (car z)) (rplaca z x) (dskrat-subst x y (car z)))
	   (dskrat-subst x y (cdr z))
	   z)))

(defmfun |''MAKE-FUN| (noun-name x)
  (simplifya (list (ncons noun-name) (resimplify x)) t))

(macrolet ((|''MAKE| (fun noun)
	     `(defmfun ,fun (x) (|''MAKE-FUN| ',noun x))))
  (|''MAKE| $log %log)
  (|''MAKE| $sin %sin) (|''MAKE| $cos %cos) (|''MAKE| $tan %tan)
  (|''MAKE| $cot %cot) (|''MAKE| $sec %sec) (|''MAKE| $csc %csc)
  (|''MAKE| $sinh %sinh) (|''MAKE| $cosh %cosh) (|''MAKE| $tanh %tanh)
  (|''MAKE| $coth %coth) (|''MAKE| $sech %sech) (|''MAKE| $csch %csch)
  (|''MAKE| $asin %asin) (|''MAKE| $acos %acos) (|''MAKE| $atan %atan)
  (|''MAKE| $acot %acot) (|''MAKE| $asec %asec) (|''MAKE| $acsc %acsc)
  (|''MAKE| $asinh %asinh) (|''MAKE| $acosh %acosh) (|''MAKE| $atanh %atanh)
  (|''MAKE| $acoth %acoth) (|''MAKE| $asech %asech) (|''MAKE| $acsch %acsch)
  (|''MAKE| $round %round) (|''MAKE| $truncate %truncate) (|''MAKE| $plog %plog)
  (|''MAKE| $signum %signum) (|''MAKE| $gamma %gamma))

;; evfun properties
(mapc #'(lambda (x) (putprop x t 'evfun))
      '($radcan $factor $ratsimp $trigexpand $trigreduce $logcontract
	$rootscontract $bfloat $ratexpand $fullratsimp $rectform $polarform))

;; evflag properties
(mapc #'(lambda (x) (putprop x t 'evflag))
      '($exponentialize $%emode $demoivre $logexpand $logarc
	$radexpand $keepfloat $listarith $float $ratsimpexpons $ratmx
	$simp $simpsum $simpproduct $algebraic $ratalgdenom $factorflag $ratfac
	$infeval $%enumer $programmode $lognegint $logabs $letrat
	$halfangles $exptisolate $isolate_wrt_times $sumexpand
	$cauchysum $numer_pbranch $m1pbranch $dotscrules $trigexpand))

;;; Float constants, to 2048 bits of precision.
;;; (EXP 1)
(mdefprop $%e     2.7182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274274663919320030599218174135966290435729003342952605956307381323286279434907632338298807531952510190115738341879307021540891499348841675092447614606680822648001684774118537423454424371075390777449920695517027618386062613313845830007520449338265602976067371132007093287091274437470472306969772093101416928368190255151086574637721112523897844250569536967707854499699679468644549059879316368892300987931277361782154249992295763514822082698951936680331825288693984964651058209392398294887933203625094431173012381970684161404
	  $numer)
;;; (ATAN 0 -1)
(mdefprop $%pi    3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271452635608
	  $numer)
;;; (1+sqrt(5))/2
(mdefprop $%phi   1.6180339887498948482045868343656381177203091798057628621354486227052604628189024497072072041893911374847540880753868917521266338622235369317931800607667263544333890865959395829056383226613199282902678806752087668925017116962070322210432162695486262963136144381497587012203408058879544547492461856953648644492410443207713449470495658467885098743394422125448770664780915884607499887124007652170575179788341662562494075890697040002812104276217711177780531531714101170466659914669798731761356006708748071013179523689427521948435305678300228785699782977834784587822891109762500302696156170025046433824377648610283831268330372
	  $numer)
;;; Euler's constant
(mdefprop $%gamma 0.57721566490153286060651209008240243104215933593992359880576723488486772677766467093694706329174674951463144724980708248096050401448654283622417399764492353625350033374293733773767394279259525824709491600873520394816567085323315177661152862119950150798479374508570574002992135478614669402960432542151905877553526733139925401296742051375413954911168510280798423487758720503843109399736137255306088933126760017247953783675927135157722610273492913940798430103417771778088154957066107501016191663340152278935867965497252036212879226555953669628176388792726801324310104765059637039473949576389065729679296010090151251959509223
	  $numer)

(mdefprop $herald_package (nil $transload t) $props)
(mdefprop $load_package (nil $transload t) $props)

(defprop bigfloat bigfloatm* mfexpr*)
(defprop lambda constlam mfexpr*)
(defprop quote cadr mfexpr*)		; Needed by MATCOM/MATRUN.

(eval-when
    #+gcl (compile eval)
    #-gcl (:compile-toplevel :execute)

    (setq  *read-base* *old-read-base*))
