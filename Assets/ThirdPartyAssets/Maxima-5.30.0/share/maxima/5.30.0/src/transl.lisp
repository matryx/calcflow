;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;                 GJC 9:29am  Saturday, 5 April 1980		 	 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module transl)

;;; File directory.

;;; TRANSL   Driver. Basic translation properties.
;;; TRANSS   User-interaction, FILE-I/O etc.
;;; TRANS1   Translation of JPG;MLISP and other FSUBRS.
;;;          which take call-by-name parameters.
;;; TRANS2   LISTS, ARRAYs, other random operators.
;;; TRANS3   LAMBDA. CLOSURES. also used by fsubr call-by-name
;;;          compatibility package.              
;;; TRANS4   operators, ".", "^^" some functions such as GAMMA.
;;; TRANS5   FSUBRS from COMM, and others, these are mere MACRO
;;;          FSUBRS.
;;; TRANSF   floating point intensive properties. BIGFLOAT stuff.
;;; TROPER   Basic OPERATORS.
;;; TRUTIL   transl utilities.
;;; TRMODE   definition of MODEDECLARE. run time error checking code.
;;; TRDATA   this is the MODE data for the "built-in" functions.
;;; TRANSM   This defines the macro DEF%TR. When compiled on MC
;;;          DEF%TR produces autoload definitions for TRANS1 thru L.
;;; PROCS    macro's needed.
;;; TRANSQ   these are macros for translated code. Loaded by TPRELU
;;;          this is compile-time only.
;;; MDEFUN   contains the macro which defines macsyma functions.
;;;          runtime and compile-time.
;;; ACALL    is some run time support for translated code, array calls.
;;; FCALL    run-time translated function call support for uncompiled
;;;          code. Many FSUBRS which are macros in TRANSQ.
;;; EVALW    EVAL-WHEN definition for interpreter.
;;; MLOAD    This has a hack hook into BATCH, which is needed to do
;;;          TRANSLATE_FILE I/O. when using old-i/o SUPRV.


;;; Functions and literals have various MODE properties;;; >
;;; (at user level set up by $MODEDECLARE), such as "$FLOAT" and "$ANY".
;;; The main problem solved by this translater (and the reason that
;;; it works on forms from the "inside out" as an evaluator would do
;;; (expect for macro forms)), is the problem of type (MODE) dependant
;;; function calling and mode conversion. The function TRANSLATE
;;; returns a list  where the CAR of the list is the MODE of the
;;; expression and the CDR is the expression to be evaluated by
;;; the lisp evaluator to give the equivalent result of evaluating
;;; the given macsyma expression with the macsyma evaluator.
;;; One doesn't know the MODE of an expression until seeing the modes
;;; of all its parts. See "*UNION-MODE"

;;; weak points in the code
;;; [1] duplication of functionality in the translaters for
;;; MPLUS MTIMES etc. 
;;; [3] primitive mode scheme. lack of even the most primitive general
;;; type coercion code. Most FORTRAN compilers are better than this.
;;; [4] for a compiler, this code SUCKS when it comes to error checking
;;; of the code it is munging. It doesn't even do a WNA check of system
;;; functions!
;;; [5]
;;; The duplication of the code which handles lambda binding, in MDO, MDOIN
;;; TR-LAMBDA, and MPROG, is very stupid. For macsyma this is one of
;;; the hairier things. Declarations must be handled, ASSIGN properties...
;;; -> Binding of ASSIGN properties should be handled with he "new"
;;; UNWIND-PROTECT instead of at each RETURN, and at "hope" points such as
;;; the ERRLIST. {Why wasn't this obvious need for UNWIND-PROTECT made
;;; known to the lisp implementers by the macsyma implementers? Why did it
;;; have to wait for the lisp machine group? Isn't this just a generalization
;;; of special binding?}
;;; [6] the DCONVX idea here is obscurely coded, incomplete, and totally
;;; undocumented. It was probably an attempt to hack efficient
;;; internal representations (internal to a given function), for some macsyma
;;; data constructs, and yet still be sure that fully general legal data
;;; frobs are seen outside of the functions. Note: this can be done
;;; simply by type coercion and operator folding.

;;; General comments on the stucture of the code.
;;; A function named TR-<something> means that it translates
;;; something having to do with that something.
;;; N.B. It does not mean that that is the translate property for <something>.


(defvar *untranslated-functions-called* nil)

(defmvar *declared-translated-functions* nil
         "List of functions which are believed to be translated.")

(defmvar $tr_semicompile nil
  "If TRUE TRANSLATE_FILE and COMPFILE output forms which will~
	 be macroexpanded but not compiled into machine code by the~
	 lisp compiler.")

(defmvar  $transcompile  t
  "If TRUE TRANSLATE_FILE outputs declarations for the COMPLR.
	  The only use of the switch is to save the space declarations take
	  up in interpreted code.")

(defmvar tstack nil " stack of local variable modes ")

(defmvar local nil "T if a $local statement is in the body.")
(defmvar arrays nil "arrays to declare to `complr'")
(defmvar lexprs nil "Lexprs to declare.")
(defmvar exprs nil "what else?")
(defmvar fexprs nil "Fexprs to declare.")
(defmvar tr-progret t)
(defmvar inside-mprog nil)
(defmvar returns nil "list of `translate'd return forms in the block.")
(defmvar return-mode nil "the highest(?) mode of all the returns.")
(defmvar need-prog? nil)
(defmvar assigns nil "These are very-special variables which have a Maxima
	assign property which must be called to bind and unbind the variable
	whenever it is `lambda' bound.")

(defmvar specials nil "variables to declare special to the complr.")

(defmvar translate-time-evalables
    '($modedeclare $alias $declare $infix $nofix $declare_translated
      $matchfix $prefix $postfix $compfile))

(defmvar *transl-backtrace* nil
  " What do you think? ")

(defmvar *transl-debug* nil "if T it pushes `backtrace' and `trace' ")

(defmvar tr-abort nil "set to T if abortion is requested by any of the
	sub-parts of the translation. A *THROW would be better, although it
	wouldn't cause the rest of the translation to continue, which may
	be useful in translation for MAXIMA-ERROR checking.")

(defmvar tr-unique (gensym)
  "this is just a unque object used for random purposes,
	such as the second (file end) argument of READ.")


(defmvar $tr_warn_undeclared '$compile
  "When to send warnings about undeclared variables to the TTY")

(defmvar $tr_warn_meval '$compfile
  "If `meval' is called that indicates problems in the translation")

(defmvar $tr_warn_fexpr
    '$compfile
  "FEXPRS should not normally be output in translated code, all legitimate
special program forms are translated.")

(defmvar $tr_warn_mode '$all
  "Warn when variables are assigned values out of their mode.")

(defmvar $tr_warn_undefined_variable '$all
  "Warn when undefined global variables are seen.")


(defmvar *warned-un-declared-vars* nil "Warning State variable")
(defmvar *warned-fexprs* nil "Warning State variable")
(defmvar *warned-mode-vars* nil "Warning State variable")

(defmvar $tr_function_call_default '$general
  "
FALSE means punt to MEVAL, EXPR means assume lisp fixed arg function.
GENERAL, the default gives code good for mexprs and mlexprs but not macros.
GENERAL assures variable bindings are correct in compiled code.
In GENERAL mode, when translating F(X), if F is a bound variable, then
it assumes that APPLY(F,[X]) is meant, and translates a such, with 
apropriate warning. There is no need to turn this off.
APPLY means like APPLY.")

(defmvar $tr_array_as_ref t
  "If true runtime code uses value of the variable as the array.")

(defmvar $tr_numer nil
  "If `true' numer properties are used for atoms which have them, e.g. %pi")

(defmvar $tr_predicate_brain_damage nil
  "If TRUE, output possible multiple evaluations in an attempt
  to interface to the COMPARE package.")

(defvar boolean-object-table
  '(($true . ($boolean . t))
    ($false . ($boolean . nil))
    (t . ($boolean . t))
    (nil . ($boolean . nil))))

(defvar mode-init-value-table
  '(($float . 0.0)
    ($fixnum . 0)
    ($number  . 0)
    ($list . '((mlist)))
    ($boolean  . nil)))

(defvar tr-lambda-punt-assigns nil
  "Kludge argument to `tr-lambda' due to lack of keyword argument passing")

(or (boundp '*in-compile*)
    (setq *in-compile* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun barfo (&rest l)
  (apply #'tr-format
	 (nconc (list (intl:gettext "translator: internal error. Message: ~:M~%")) l))
  (cond (*transl-debug*
	 (break "transl barfo ~S" t))
	(t
	 (setq tr-abort t)
	 nil)))

(defun specialp (var)
  (cond ((or (optionp var)
	     (get var 'special))
	 (if $transcompile (pushnew var specials :test #'eq))
	 t)))


;;; The error message system. Crude as it is.
;;; I tell you how this aught to work:
;;; (1) All state should be in one structure, one state variable.
;;; (2) Should print out short message on-the-fly so that it
;;;     gives something to watch, and also so that it says something
;;;     if things crash.
;;; (3) Summaries on a cross-referenced per-function and per-item
;;;     should be printed at the end, as a table.
;;;     e.g.
;;;     Undefined Functions     used in
;;;     FOO                     BAR, BAZ,BOMB
;;;     FOOA                    P,Q
;;;     Undefined Variables ... same thing
;;;     Incomprehensible special forms
;;;     EV                      ....
;;;     Predicate Mode Targetting failures.
;;;     .....  -gjc

;;; The way it works now is to print too little or too much.
;;; Many items are only warned about the first time seen.
;;; However, this isn't too much of a problem when using Emacs
;;; to edit code, because searching for warned-about tokens
;;; is quick and easy.

(defmvar *tr-warn-break* t
  " if in debug mode `warning's signaled go to lisp break loops ")

(defmacro tr-warnbreak ()
  `(and *transl-debug* *tr-warn-break* (break "transl ~S" t )))

(defun tr-warnp (val)
  (and val
       (cond (*in-compile*
	      (member val '($all $compile $compfile $translate) :test #'eq))
	     ((or *in-compfile* *in-translate-file*)
	      (member val '($all $compfile $translate) :test #'eq))
	     (*in-translate*
	      (member val '($all $translate) :test #'eq)))))

(defvar warned-undefined-variables nil)

(defun warn-undefined-variable (form)
  (and (tr-warnp $tr_warn_undefined_variable)
       (cond ((member form warned-undefined-variables :test #'eq))
	     (t
	      (push form warned-undefined-variables)
	      (tr-format (intl:gettext "warning: encountered undefined variable ~:M in translation.~%") form)
	      (tr-warnbreak)))))

(defun warn-undeclared (form &optional comment)
  (and (tr-warnp $tr_warn_undeclared)
       (cond ((member form *warned-un-declared-vars* :test #'equal) t)
	     (t
	      (push form *warned-un-declared-vars*)
	      (tr-format (intl:gettext "warning: no type declaration for ~:M; assume type is 'any'.~%") form)
	      (tr-format (intl:gettext "note: 'modedeclare' declares types for translation.~%"))
	      (cond (comment
		     (dolist (v *translation-msgs-files*)
		       (terpri v)
		       (princ comment v))))
	      (tr-warnbreak)
	      nil))))

(defun warn-meval (form &optional comment)
  (cond ((tr-warnp $tr_warn_meval)
	 (tr-format (intl:gettext "warning: emit call to MEVAL for expression: ~:M~%") form)
	 (cond (comment (dolist (v *translation-msgs-files*)
			  (terpri v)
			  (princ comment v))))
	 (tr-warnbreak)
	 'warned)))


(defun warn-mode (var mode newmode &optional comment)
  (cond ((eq mode newmode))
	(t
	 (cond ((and (tr-warnp $tr_warn_mode)
		     (not (covers mode newmode))
		     (not (member (list var mode newmode) *warned-mode-vars* :test #'equal)))
		(push (list var mode newmode) *warned-mode-vars*)
		(tr-format (intl:gettext "warning: variable ~:M (declared type ~:M) assigned type ~:M.~%") var mode newmode)
		(cond (comment
		       (dolist (v *translation-msgs-files*)
			 (terpri v)
			 (princ comment v))))
		(tr-warnbreak))))))

(defun warn-fexpr (form &optional comment)
  (cond ((and (tr-warnp $tr_warn_fexpr)
	      (not (member form *warned-fexprs* :test #'equal)))
	 (push  form *warned-fexprs*)
	 (tr-format (intl:gettext "warning: ~:M is a special function without a full Lisp translation.~%") form)
         (tr-format (intl:gettext "warning: calling ~:M in compiled code might not have the desired effect.~%") form)
	 (cond (comment
		(dolist (v *translation-msgs-files*)
		  (terpri v)
		  (princ comment v))))
	 (tr-warnbreak))))


(defun macsyma-special-macro-p (fcn)
  (getl-lm-fcn-prop fcn '(macro)))

(defun macsyma-special-op-p (f)
  (getl f '(fsubr fexpr mfexpr* mfexpr*s *fexpr)))

(defun possible-predicate-op-p (f)
  (member f '(mnotequal mequal $equal mgreaterp mgeqp mlessp mleqp) :test #'eq))

;;;***************************************************************;;;

;;; This function is the way to call the TRANSLATOR on an expression with
;;; locally bound internal mode declarations. Result of TR-LAMBDA will be
;;; (MODE . (LAMBDA (...) (DECLARE ...) TRANSLATED-EXP))

(defun tr-local-exp (exp &rest vars-modes)
  (let ((loc (let ((tr-lambda-punt-assigns t))
	       (tr-lambda `((lambda) ((mlist) ,@(do ((l vars-modes (cddr l))
						     (ll nil (cons (car l) ll)))
						    ((null l) ll)
						    (or (variable-p (car l))
							(bad-var-warn (car l)))))
			    (($modedeclare) ,@vars-modes)
			    ,exp)))))
    (let ((mode (car loc))
	  (exp (car (last loc)))) ;;; length varies with TRANSCOMPILE.
      (cons mode exp))))

(defun tr-args (form)
  (mapcar #'(lambda (x) (dconvx (translate x))) form))

(defun dtranslate (form) (cdr (translate form)))

(defun dconv (x mode) 
  (cond ((eq '$float mode) (dconv-$float x))
	((eq '$cre mode) (dconv-$cre x))
	(t (cdr x))))

(defun dconvx (x) 
  (if (member (car x) '(ratexpr pexpr) :test #'eq)
      (dconv-$cre x)
      (cdr x)))

(defun dconv-$float (x)
  (cond ((member (car x) '($fixnum $number) :test #'eq)
	 (if (integerp (cdr x)) (float (cdr x)) (list 'float (cdr x))))
	((eq '$rational (car x))
	 (cond ((not (eq 'quote (cadr x)))
		`($float ,(cdr x)))
	       (t
		(/ (float (cadadr (cdr x))) (float (caddr (caddr x)))))))
	(t (cdr x))))

(defun dconv-$cre (x)
  (if (eq '$cre (car x))
      (cdr x)
      `(ratf ,(cdr x))))

(defmvar *$any-modes* '($any $list))

(defun covers (mode1 mode2)
  (cond ((eq mode1 mode2) t)
	((eq '$float mode1) (member mode2 '($float $fixnum $rational) :test #'eq))
	((eq '$number mode1) (member mode2 '($fixnum $float) :test #'eq))
	((member mode1 *$any-modes* :test #'eq) t)))

;;; takes a function name as input.

(defun tr-mfun (name &aux (*transl-backtrace* nil))
  (let ((def-form (consfundef name nil nil)))
    (cond ((null def-form)
	   (setq tr-abort t))
	  (t
	   (tr-mdefine-toplevel def-form)))))

;;; DEFUN
;;; All the hair here to deal with macsyma fexprs has been flushed.
;;; Right now this handles MDEFMACRO and MDEFINE. The decisions
;;; of where to put the actual properties and what kind of
;;; defuns to make (LEXPR EXPR for maclisp) are punted to the
;;; macro package.

(defun tr-mdefine-toplevel (form &aux (and-restp nil))
  (destructuring-let (((((name . flags) . args) body) (cdr form))
		      (a-args) kind out-forms)

    (do ((args args (cdr args))
	 ;; array functions cannot be LEXPR-like. gee.
	 ;; there is no good reason for that, except for efficiency,
	 ;; and I know that efficiency was not a consideration.
	 (full-restricted-flag (or (eq name 'mqapply)
				   (member 'array flags :test #'eq))))
	((null args) (setq a-args (nreverse a-args)))
      (let ((u (car args)))
	(cond ((atom u)
	       (push u a-args))
	      ((and (not full-restricted-flag)
		    (not and-restp)
		    (eq (caar u) 'mlist)
		    (cdr u) (atom (cadr u)))
	       (push (cadr u) a-args)
	       (setq and-restp t))
	      (t
	       (push tr-unique a-args)))))

    
    (cond ((eq name 'mqapply) 
	   ;; don't you love syntax!
	   ;; do a switch-a-roo here. Calling ourselves recursively
	   ;; like this allows all legal forms and also catches
	   ;; errors. However, certain generalizations are also
	   ;; allowed. They won't get passed the interpreter, but
	   ;; interesting things may happen here. Thats what you
	   ;; get from too much syntax, so don't sweat it.
	   (tr-mdefine-toplevel
	    `(,(car form) ,(car args)
	      ((lambda) ((mlist) ,@(cdr args)) ,body))))
	  ((member tr-unique a-args :test #'eq)
	   ;; WHAT IS "BAD" ABOUT THE ARGUMENT LIST HERE ??
	   (tr-format (intl:gettext "error: unhandled argument list in function definition: ~:M~%") `((mlist),@args))
	   (setq tr-abort t)
	   nil)
	  ((member (caar form) '(mdefine mdefmacro) :test #'eq)
	   (setq kind (cond ((eq (caar form) 'mdefmacro) 'macro)
			    ((member 'array flags :test #'eq) 'array)
			    (t 'func)))
	   (let* ((t-form
		   (tr-lambda `((lambda) ((mlist) ,@a-args) ,body)))
		  (desc-header
		   `(,name ,(car t-form) ,(caar form)
		     ,and-restp ,(eq kind 'array))))
	     (cond ((eq kind 'func)
		    (push-pre-transl-form
		     `(defmtrfun-external ,desc-header))
		    (and (not (member (car t-form) '($any nil) :test #'eq))
			 (putprop name (car t-form) 'function-mode)))
		   ((eq kind 'array)
		    (and (not (member (car t-form) '($any nil) :test #'eq))
			 (decmode-arrayfun name (car t-form)))))

	     (cond ((or *in-translate* (not $packagefile))
					; These are all properties which tell the
					; user that functions are in the environment,
					; and that also allow him to SAVE the functions.
		    (push `(defprop ,name t translated) out-forms)
		    (push `(add2lnc ',name $props) out-forms)
		    (cond ((eq '$all $savedef)
			   (push
			    `(add2lnc
			      '((,name ,@flags) ,@args)
			      ,(case kind
				     (array '$arrays)
				     (func '$functions)
				     (macro '$macros))) out-forms)))))
	     (cond ((eq '$all $savedef)
		    ;; For some reason one may want to save the
		    ;; interpreted definition even if in a PACKAGEFILE.
		    ;; not a good idea to use SAVEDEF anyway though.
		    (push `(mdefprop ,name
			    ((lambda) ((mlist) ,@args) ,body)
			    ,(case kind
				   (array 'aexpr)
				   (macro 'mmacro)
				   (func 'mexpr)))
			  out-forms)))
	     ;;once a function has been translated we want to make sure mfunction-call is eliminated.
	     (progn
	       (remprop (car desc-header) 'undefined-warnp)
	       (setf (get (car desc-header) 'once-translated) "I was once translated"))
	     `(progn
		,@(nreverse out-forms)
		(defmtrfun ,desc-header ,@(cdr (cdr t-form))))))
	  (t
	   (barfo '?)))))


(defun lisp-fcn-typep (fcn type)
  (get fcn type))

(defun translate-function (name)
  (bind-transl-state
   (setq *in-translate* t)
   (let ((lisp-def-form (tr-mfun name))
	 (delete-subr? (and (get name 'translated)
			    (not (lisp-fcn-typep name 'expr)))))
     (cond (tr-abort
	    (trfail name))
	   (t
	    (if delete-subr? (remprop name 'subr))
	    (if (mget name 'trace) (macsyma-untrace name))
	    (if (not $savedef) (meval `(($remfunction) ,name)))
	    (let ((lisp-action
					; apply EVAL so it is easy to TRACE.
					; ERRSET is crude, but...
		   (errset (apply 'eval (list lisp-def-form)))))
	      (cond ((not lisp-action)
		     (trfail name))
		    (t (values name lisp-def-form)))))))))


(defun trfail (x)
  (tr-format x (intl:gettext "error: failed to translate.~%")) nil)

(defmfun translate-and-eval-macsyma-expression (form)
  ;; this is the hyper-random entry to the transl package!
  ;; it is used by MLISP for TRANSLATE:TRUE ":=".
  (bind-transl-state
   (setq *in-translate* t)
   ;; Use FUNCALL so that we can be sure we can TRACE this even when
   ;; JPG sets PURE to NIL. Also, use a function named TRANSLATOR-EVAL
   ;; so we don't have to lose badly by tracing EVAL!
   (funcall (progn 'translator-eval)
	    (funcall (progn 'translate-macexpr-toplevel) form))))

(defun translator-eval (x)
  (eval x))

;; This basically tells the msetq def%tr to use defparameter insetad
;; of setq because we're doing a setq at top-level, which isn't
;; specified by ANSI CL.
(defvar *macexpr-top-level-form-p* nil)

(defmfun translate-macexpr-toplevel (form &aux (*transl-backtrace* nil) tr-abort)
  ;; there are very few top-level special cases, I don't
  ;; think it would help the code any to generalize TRANSLATE
  ;; to target levels.
  ;;
  ;; Except msetq at top-level is special for ANSI CL.  See below.
  (setq form (toplevel-optimize form))
  (cond ((atom form) nil)
	((eq (caar form) '$eval_when)
	 (let ((whens (cadr form))
	       (body (cddr form)) tr-whens)
	   (setq whens (cond (($listp whens) (cdr whens))
			     ((atom whens) (list whens))
			     (t
			      (tr-format (intl:gettext "error: 'eval-when' argument must be a list or atom; found: ~:M~%") (cadr form))
			      nil)))
	   (setq tr-whens (mapcar 'stripdollar whens))
	   (cond ((member '$translate whens :test #'eq)
		  (mapc 'meval body)))
	   (cond ((member '$loadfile whens :test #'eq)
		  `(progn
		     ,@(mapcar 'translate-macexpr-toplevel body)))
		 ((setq tr-whens (intersect tr-whens '(:compile-toplevel :load-toplevel :execute)))
		  `(eval-when
		       ,tr-whens
		     ,@(mapcar 'translate-macexpr-toplevel body)))
		 ((member '$compile whens :test #'eq)
		  ;; strictly for the knowledgeable user.
		  ;; I.E. so I can use EVAL_WHEN(COMPILE,?SPECIALS:TRUE)
		  `(eval-when
		       #+gcl (compile)
		       #-gcl (:compile-toplevel)
		       ,@(mapcar 'translate-macexpr-toplevel body))))))
	((member (caar form) translate-time-evalables :test #'eq)
	 (meval1 form)
	 `(eval-when
	      #+gcl (compile load eval)
	      #-gcl (:compile-toplevel :load-toplevel :execute)
	      (meval* ',form)))
	((member  (caar form) '(mdefine mdefmacro) :test #'eq)
	 (let ((name (caaadr form))
	       (trl))
	   (tr-format (intl:gettext "note: translating ~:@M~%") name)
	   (setq trl (tr-mdefine-toplevel form))
	   (cond (tr-abort
		  (tr-format (intl:gettext "error: failed to translate ~:@M~%") name)
		  (tr-format (intl:gettext "note: keep going and hope for the best.~%"))
		  `(meval* ',form))
		 (t trl))))
	((eq 'mprogn (caar form))
	 ;; flatten out all PROGN's of course COMPLR needs PROGN 'COMPILE to
	 ;; tell it to flatten. I don't really see the use of that since one
	 ;; almost allways wants to. flatten.
	 ;; note that this ignores the $%% crock.
	 `(progn 'compile
		 ,@(mapcar #'translate-macexpr-toplevel (cdr form))))
	((eq 'msetq (caar form))
	 ;; Toplevel msetq's should really be defparameter instead of
	 ;; setq for Common Lisp.  
	 (let ((*macexpr-top-level-form-p* t))
	   (dtranslate form)))
	((eq '$define_variable (caar form))
	 ;; Toplevel msetq's should really be defparameter instead of
	 ;; setq for Common Lisp.  
	 (let ((*macexpr-top-level-form-p* t))
	   (dtranslate form)))
	(t		
	 (let  ((t-form (dtranslate form)))
	   (cond (tr-abort
		  `(meval* ',form))
		 (t
		  t-form))))))



(defmvar $tr_optimize_max_loop 100.
  "The maximum number of times the macro-expansion and optimization
	 pass of the translator will loop in considering a form.
	 This is to catch macro expansion errors, and non-terminating
	 optimization properties.")

(defun toplevel-optimize (form)
  ;; it is vital that optimizations be done within the
  ;; context of variable meta bindings, declarations, etc.
  ;; Also: think about calling the simplifier here.
  (cond ((atom form)
	 (cond ((symbolp form)
        ;; FOLLOWING CODE APPEARS TO BE BROKEN; NOT SURE WHAT WAS THE INTENT.
        ;; FOLLOWING CODE ALWAYS RETURNS SYMBOL ITSELF EVEN WHEN '$CONSTANTP IS A PROPERTY
        ;; IS IT SUPPOSED TO FETCH A DECLARED CONSTANT VALUE ??
        ;; JUST LEAVE IT BE FOR NOW; DO NOT TRY TO REVISE WITH KINDP
		(let ((v (getl (mget form '$props) '($constant))))
		  (if v (cadr v) form)))
	       (t form)))
	(t
	 (do ((new-form)
	      (kount 0 (1+ kount)))
	     ;; tailrecursion should always arrange for a counter
	     ;; to check for mobylossage.
	     ((> kount $tr_optimize_max_loop)
	      (tr-format (intl:gettext "warning: I've looped ~A times in macro expansion; just give up and return ~:@M~%")
	       $tr_optimize_max_loop (caar form))
	      form)
	   (setq new-form (toplevel-optimize-1 form))
	   (cond ((atom new-form)
		  (return (toplevel-optimize new-form)))
		 ((eq new-form form)
		  (return form))
		 (t
		  (setq form new-form)))))))

(defun toplevel-optimize-1 (form &aux (op (car form)) prop)
  (cond ((or (atom op)
	     (member 'array op :test #'eq)) form)
	((progn (setq op (car op))
		(setq prop
		      (if $transrun	; crock a minute.
			  (or (get op 'translated-mmacro)
			      (mget op 'mmacro))
			  (or (mget op 'mmacro)
			      (get op 'translated-mmacro)))))
	 (mmacro-apply prop form))
	((setq prop ($get op '$optimize))
	 ;; interesting, the MAPPLY here causes the simplification
	 ;; of the form and the result.
	 ;; The optimize property can be used to implement
	 ;; such niceties as the $%% crock.
	 (mapply1 prop (list form) "an optimizer property" nil))
	((and ($get op '$transload)
	      (get op 'autoload)
	      ;; check for all reasonable definitions,
	      ;; $OPTIMIZE and MACRO already checked.
	      (not (or (get-lisp-fun-type op)
		       (getl op '(translate mfexpr* mfexpr*s
				  fsubr fexpr *fexpr
				  macro
				  ;; foobar?
				  ))
		       (mgetl op '(mexpr)))))
	 (load-function op t)
	 ;; to loop.
	 (cons (car form) (cdr form)))
	(t form)))

(defun translate (form)
  (and *transl-debug* (push form *transl-backtrace*))
  (setq form (toplevel-optimize form))
  (and *transl-debug* (pop *transl-backtrace*))
  (prog2
      (and *transl-debug* (push form *transl-backtrace*))
      (cond ((atom form)
	     (translate-atom form))
	    ((consp form)
	     (translate-form form))
	    (t
	     (barfo "help")))
    ;; hey boy, reclaim that cons, just don't pop it!
    (and *transl-debug* (pop *transl-backtrace*))))

(defun translate-atom (form &aux temp)
  (cond ((numberp form) (cons (tr-class form) form))
	((setq temp (assoc form boolean-object-table :test #'eq))
	 (cdr temp))
	((and (setq temp (mget form '$numer)) $tr_numer)
	 `($float . ,temp))
	((setq temp (implied-quotep form))
	 `($any . ',temp))
	((tboundp form)
	 (specialp form) ;; notes its usage if special.
	 (setq form (teval form))
	 `(,(value-mode form) . ,form))
	(t
	 (cond ((not (specialp form))
		(warn-undefined-variable form)
		(if $transcompile (pushnew form specials :test #'eq))))
	 ;; note that the lisp analysis code must know that
	 ;; the TRD-MSYMEVAL form is a semantic variable.
	 (let* ((mode (value-mode form))		
		(init-val (assoc mode mode-init-value-table :test #'eq)))
	   (setq init-val (cond (init-val (cdr init-val))
				(t `',form)))
	   ;; in the compiler TRD-MSYMEVAL doesn't do a darn
	   ;; thing, but it provides dynamic initialization of
	   ;; variables in interpreted code which is translated
	   ;; in-core. In FILE loaded code the DEFVAR will take
	   ;; care of this.
	   (push-defvar form init-val)
	   `(,mode . (trd-msymeval ,form ,init-val))))))

(defun translate-form (form &aux temp)
  (cond ((eq (car form) 'meval) (cons '$any form)) ;;for those lispy macsyma forms
	((not (atom (caar form)))
	 ;; this is a check like that in the simplifier. form could
	 ;; result from substitution macros.
	 (translate `((mqapply) ,(caar form) . ,(cdr form))))
	((member 'array (cdar form) :test #'eq)
	 ;; dispatch this bad-boy to another module quick.
	 (tr-arraycall form))
	;; TRANSLATE properties have priority.
	((setq temp (get (caar form) 'translate))
	 (funcall temp form))
	((setq temp (get-lisp-fun-type (caar form)))
	 (tr-lisp-function-call form temp))
	((setq temp (macsyma-special-macro-p (caar form)))
	 (attempt-translate-random-macro-op form temp))
	((setq temp (macsyma-special-op-p (caar form)))
	 ;; a special form not handled yet! foobar!
	 (attempt-translate-random-special-op form temp))
	((getl (caar form) '(noun operators))
	 ;; puntastical case. the weird ones are presumably taken care
	 ;; of by TRANSLATE properties by now.
	 (tr-infamous-noun-form form))

	;; "What does a macsyma function call mean?".
	;; By the way, (A:'B,B:'C,C:'D)$ A(3) => D(3)
	;; is not supported.
	(t
	 (tr-macsyma-user-function-call (caar form) (cdr form) form))))



(defmvar $tr_bound_function_applyp t)

(defun tr-macsyma-user-function-call (function args form)
  ;; this needs some work, output load-time code to
  ;; check for MMACRO properties, etc, to be really
  ;; foolproof.
  (cond ((eq $tr_function_call_default '$apply)
	 (translate `(($apply) ,(caar form) ((mlist) ,@(cdr form)))))
	((eq $tr_function_call_default '$expr)
	 (tr-lisp-function-call form 'subr))
	((eq $tr_function_call_default '$general)
	 (cond 
	     ;;; G(F,X):=F(X+1); case.
	   ((and $tr_bound_function_applyp (tboundp function))
	    (let ((new-form `(($apply) ,function ((mlist) ,@args))))
	      (tr-format (intl:gettext "warning: ~:M is a bound variable in ~:M, but it is used as a function.~%") function form)
	      (tr-format (intl:gettext "note: instead I'll translate it as: ~:M~%") new-form)
	      (translate new-form)))
	   ;; MFUNCTION-CALL cleverely punts this question to a FSUBR in the
	   ;; interpreter, and a macro in the compiler. This is good style,
	   ;; if a user is compiling then assume he is less lossage prone.
	   (t
	    (pushnew (caar form) *untranslated-functions-called*)
	      (call-and-simp
	       (function-mode (caar form))
	       'mfunction-call `(,(caar form) ,@(tr-args args))))))
	(t
	 ;; This case used to be the most common, a real loser.
	 (warn-meval form)
	 `(,(function-mode (caar form)) . (meval ',form)))))


(defun attempt-translate-random-macro-op (form typel &aux tem)
  (warn-fexpr form)
  (setq tem (translate-atoms form))
  (setf (car tem) (caar tem))
  `($any . ,tem))

(defun attempt-translate-random-special-op (form typel)
  (warn-fexpr form)
  `(,(function-mode (caar form)) . (meval ',(translate-atoms form))))


(defun tr-lisp-function-call (form type)
  (let ((op (caar form)) (mode) (args))
    (setq args (cond ((member type '(subr lsubr expr) :test #'eq)
		      (if $transcompile
			  (case type
			    ((subr) (pushnew op exprs :test #'eq))
			    ((lsubr) (pushnew op lexprs :test #'eq))
			    (t nil)))
		      (mapcar #'(lambda (llis) (dconvx (translate llis)))
			      (cdr form)))
		     (t
		      (if $transcompile (pushnew op fexprs :test #'eq))
		      (mapcar 'dtranslate (cdr form))))
	  mode (function-mode op))
    (call-and-simp mode op args)))

;;the once-translated is so that inside translate file where a function
;;has been translated, subsequent calls won't use mfunction call
(defun get-lisp-fun-type (fun &aux temp)
  ;; N.B. this is Functional types. NOT special-forms,
  ;; lisp special forms are meaningless to macsyma.
  (cond ((get fun '*lexpr) 'lsubr)
	((get fun '*expr) 'subr)
	;; *LEXPR & *EXPR gotten from DEFMFUN declarations
	;; which is loaded by TrData.
	((mget fun '$fixed_num_args_function)
	 'subr)
	((mget fun '$variable_num_args_function)
	 'lsubr)
	((setq temp (getl fun '(expr subr lsubr)))
	 (car temp))
	((get fun 'once-translated))
	((get fun 'translated))
	(t nil)))

(defun tr-infamous-noun-form (form)
  ;; 'F(X,Y) means noun-form. The arguments are evaluated.
  ;;  but the function is cons on, not applied.
  ;;  N.B. for special forms and macros this is totally wrong.
  ;;  But, those cases are filtered out already, presumably.
       
  (let ((op (cond ((member 'array (car form) :test #'eq)
		   `(,(caar form) array))
		  (t `(,(caar form)))))
	(args (tr-args (cdr form))))
    `($any . (simplify (list ',op ,@args)))))



;;; Some atoms, soley by usage, are self evaluating. 

(defun implied-quotep (atom)
  (cond
	((stringp atom)
	 (cond
       ;; I WONDER IF THIS NEXT CONDITION CAN BE CUT OUT ?? !!
       ((equal atom "**") ;;; foolishness. The PARSER should do this.
		;; Losing Fortran hackers.
		(tr-format "~% `**' is obsolete, use `^' !!!")
		"^")
       (t atom)))
    ((get atom 'implied-quotep) atom)
	(t nil)))

(defun translate-atoms (form)
  ;; This is an oldy moldy which tries to declare everthing
  ;; special so that calling fexpr's will work in compiled
  ;; code. What a joke.
  (cond ((atom form)
	 (cond ((or (numberp form) (member form '(t nil) :test #'eq)) form)
	       ((tboundp form)
		(if $transcompile
		    (or (specialp form)
			(pushnew form specials :test #'eq)))
		form)
	       (t
		(if $transcompile (pushnew form specials :test #'eq))
		form)))
	((eq 'mquote (caar form)) form)
	(t (cons (car form) (mapcar #'translate-atoms (cdr form))))))


;;; the Translation Properties. the heart of TRANSL.

;;; This conses up the call to the function, adding in the
;;; SIMPLIFY i the mode is $ANY. This should be called everywhere.
;;; instead of duplicating the COND everywhere, as is done now in TRANSL.

(defun tr-nosimpp (op)
  (cond ((atom op)
	 (get op 'tr-nosimp))
	(t nil)))

(defun call-and-simp (mode fun args)
  (cond ((or (not (eq mode '$any))
	     (tr-nosimpp fun))
	 `(,mode ,fun . ,args))
	(t
	 `(,mode simplify (,fun . ,args)))))

(defmspec $declare_translated (fns)
  (setq fns (cdr fns))
  (loop for v in fns
	when (or (symbolp v) (and (stringp v) (setq v ($verbify v))))
	do (setf (get v 'once-translated) t)
	(pushnew v *declared-translated-functions*)
	else do (merror (intl:gettext "declare_translated: arguments must be symbols or strings; found: ~:M") v)))

(def%tr $declare (form)
  (do ((l (cdr form) (cddr l)) (nl))
      ((null l) (if nl `($any $declare . ,(nreverse nl))))
      (setq nl (cons (cadr l) (cons (car l) nl)))))

(def%tr $eval_when (form)
  (tr-format (intl:gettext "error: found 'eval_when' in a function or expression: ~:M~%") form)
  (tr-format (intl:gettext "note: 'eval_when' can appear only at the top level in a file.~%"))
  (setq tr-abort t)
  '($any . nil))

(def%tr mdefine (form) ;; ((MDEFINE) ((F) ...) ...)
  `($any . (meval ',form)))

(def%tr mdefmacro (form)
  (meval form) ;; HMM, THIS HAS A SIDE EFFECT AT THE TIME OF TRANSLATION !!
  `($any . (meval ',form)))

(def%tr $local (form)
  (cond (local
	 (tr-format (intl:gettext "error: there is already a 'local' in this block.~%"))
	 (setq tr-abort t))
	(t
	 (setq local t)))
  (cons nil `(mapply 'mlocal ',(cdr form) '$local)))


(def%tr mquote (form)
  (list (tr-class (cadr form)) 'quote (cadr form)))


(defun tr-lambda (form &optional (tr-body #'tr-seq) &rest tr-body-argl
		  &aux
		  (arglist (mparams (cadr form)))
		  (easy-assigns nil))
  ;; This function is defined to take a simple macsyma lambda expression and
  ;; return a simple lisp lambda expression. The optional TR-BODY hook
  ;; can be used for translating other special forms that do lambda binding.
  
  ;; Local SPECIAL declarations are not used because
  ;; the multics lisp compiler does not support them. They are of course
  ;; a purely syntactic construct that doesn't buy much. I have been
  ;; advocating the use of DEFINE_VARIABLE in macsyma user programs so
  ;; that the use of DECLARE(FOO,SPECIAL) will be phased out at that level.

  (mapc #'tbind arglist)
  (destructuring-let (((mode . nbody) (apply tr-body (cddr form) tr-body-argl))
		      (local-declares (make-declares arglist t)))
    ;; -> BINDING of variables with ASSIGN properties may be difficult to
    ;; do correctly and efficiently if arbitrary code is to be run.
    (if (or tr-lambda-punt-assigns
	    (do ((l arglist (cdr l)))
		((null l) t)
	      (let* ((var (car l))
		     (assign (get var 'assign)))
		(if assign
		    (cond ((member assign '(assign-mode-check) :test #'eq)
			   (push `(,assign ',var ,(teval var)) easy-assigns))
			  (t
			   (return nil)))))))
	;; Case with EASY or no ASSIGN's
	(progn ;;-have to undo any local assignments. --wfs
	  `(,mode . (lambda ,(tunbinds arglist)
		      ,local-declares
		      ,@easy-assigns
		      ,@nbody)))
	;; Case with arbitrary ASSIGN's.
	(let ((temps (mapcar #'(lambda (ign) ign (tr-gensym)) arglist)))
	  `(,mode . (lambda ,temps
		      (unwind-protect
			   (progn
			     ;; [1] Check before binding.
			     ,@(mapcan #'(lambda (var val)
					   (let ((assign (get var 'assign)))
					     (if assign
						 (list `(,assign ',var ,val)))))
				       arglist temps)
			     ;; [2] do the binding.
			     ((lambda ,(tunbinds arglist)
				,local-declares
				,@nbody)
			      ,@temps))
			;; [2] check when unbinding too.
			,@(mapcan #'(lambda (var)
				      (let ((assign (get var 'assign)))
					(if assign
					    (list `(,assign ',var
						    ;; use DTRANSLATE to
						    ;; catch global
						    ;; scoping if any.
						    ,(dtranslate var))))))
				  arglist))))))))


(defun make-declares (varlist localp &aux (dl) (fx) (fl) specs)
  (when $transcompile
    (do ((l varlist (cdr l))
	 (mode) (var))
	((null l))
      
      ;; When a variable is declared special, be sure to declare it
      ;; special here.
      (when (and localp (get (car l) 'special))
	(push (car l) specs))
      
      (when (or (not localp)
		(not (get (car l) 'special)))
	;; don't output local declarations on special variables.
	(setq var (teval (car l)) mode (value-mode var))
	(setq specs (cons var specs))
		
	(cond ((eq '$fixnum mode) (pushnew var fx :test #'eq))
	      ((eq '$float mode)  (pushnew var fl :test #'eq)))))
    (if fx (pushnew `(fixnum  . ,fx) dl :test #'eq))
    (if fl (pushnew `(type flonum  . ,fl) dl :test #'eq))
    (if specs (pushnew `(special  . ,specs) dl :test #'eq))
    (if dl `(declare . ,dl))))

(def%tr dolist (form)
  (translate `((mprogn) . ,(cdr form))))

(defun tr-seq (l)
  (do ((mode nil)
       (body nil))
      ((null l)
       (cons mode (nreverse body)))
    (let ((exp (translate (pop l))))
      (setq mode (car exp))
      (push (cdr exp) body))))

(def%tr mprogn (form)
  (setq form (tr-seq (cdr form)))
  (cons (car form) `(progn ,@(cdr form))))
	

(def%tr mprog (form)
  (let (arglist body val-list)
    ;; [1] normalize the MPROG syntax.
    (cond (($listp (cadr form))
	   (setq arglist (cdadr form)
		 body (cddr form)))
	  (t
	   (setq arglist nil
		 body (cdr form))))
    (cond ((null body)
	   (setq body '(((mquote) $done)))))
    (setq val-list (mapcar #'(lambda (u)
			       (if (atom u) u
				   (translate (caddr u))))
			   arglist)
	  arglist (mapcar #'(lambda (u)
			      ;;  X or ((MSETQ) X Y)
			      (if (atom u) u (cadr u)))
			  arglist))
    (setq form
	  (tr-lambda
	   ;; [2] call the lambda translator.
	   `((lambda) ((mlist) ,@arglist) ,@body)
	   ;; [3] supply our own body translator.
	   #'tr-mprog-body
	   val-list
	   arglist))
    (cons (car form) `(,(cdr form) ,@val-list))))

(defun tr-mprog-body (body val-list arglist
		      &aux 
		      (inside-mprog t)
		      (return-mode nil)
		      (need-prog? nil)
		      (returns nil) ;; not used but must be bound.
		      (local nil)
		      )
  (do ((l nil))
      ((null body)
       ;; [5] hack the val-list for the mode context.
       ;; Perhaps the only use of the function MAP in all of macsyma.
       (mapl #'(lambda (val-list arglist)
		 (cond ((atom (car val-list))
			(rplaca val-list
				(or (cdr (assoc (value-mode (car arglist))
						mode-init-value-table :test #'eq))
				    `',(car arglist))))
		       (t
			(warn-mode (car arglist)
				   (value-mode (car arglist))
				   (car (car val-list))
				   "in a `block' statement")
			(rplaca val-list (cdr (car val-list))))))
	     val-list arglist)
       (setq l (nreverse l))
       (cons return-mode
	     (if need-prog?
		 `((prog () ,@(delete nil l :test #'equal)))
		 l)))
    ;; [4] translate a form in the body
    (let ((form (pop body)))
      (cond ((null body)
	     ;; this is a really bad case.
	     ;; we don't really know if the return mode
	     ;; of the expression is for the value of the block.
	     ;; Some people write RETURN at the end of a block
	     ;; and some don't. In any case, the people not
	     ;; use the PROG programming style won't be screwed
	     ;; by this.
	     (setq form (translate form))
	     (setq return-mode (*union-mode (car form) return-mode))
	     (setq form (cdr form))
	     (if (and need-prog?
		      (or (atom form)
			  (not (eq (car form) 'return))))
		 ;; put a RETURN on just in case.
		 (setq form `(return ,form))))
	    ((symbolp form))
	    (t
	     (setq form (dtranslate form))))
      (push form l))))

(def%tr mreturn (form)
  (if (null inside-mprog)
      (tr-format (intl:gettext "warning: 'return' not within 'block' or 'do': ~:M~%") form))
  (setq need-prog? t)
  (setq form (translate (cadr form)))
  (setq return-mode (if return-mode (*union-mode (car form) return-mode)
			(car form)))
  (setq form `(return ,(cdr form)))
  (push form returns) ;; USED by lusing MDO etc not yet re-written.
  ;; MODE here should be $PHANTOM or something.
  `($any . ,form))

(def%tr mgo (form)
  (if (null inside-mprog)
      (tr-format (intl:gettext "warning: 'go' not within 'block' or 'do': ~:M~%") form))
  (if (not (symbolp (cadr form)))
      (tr-format (intl:gettext "warning: 'go' tag must be a symbol: ~:M~%") form))
  (setq need-prog? t)
  `($any . (go ,(cadr form))))

(def%tr mqapply (form)
  (let     ((fn (cadr form)) (args (cddr form)) 
	    (aryp (member 'array (cdar form) :test #'eq)))
    (cond ((atom fn) 
	   ;; I'm guessing (ATOM FN) is a parser error or other Lisp error,
	   ;; so don't bother to translate the following error message.
	   (mformat *translation-msgs-files* "translator: MQAPPLY operator must be a cons; found: ~:M" form)
	   nil)
	  ((eq (caar fn) 'mquote) 
	   `($any list ',(cons (cadr fn) aryp) ,@(tr-args args)))
	  ((eq (caar fn) 'lambda)
	   ;; LAMBDA([X,'Y,[L]],...)(A,B,C) is a bogus form. Don't bother with it.
	   ;; ((LAMBDA) ((MLIST) ....) ....)
	   (cond ((member 'bogus (mapcar #'(lambda (arg)
					   (cond ((or (mquotep arg)
						      ($listp arg))
						  'bogus)))
				       (cdr (cadr fn))) :test #'eq)
		  (tr-format (intl:gettext "error: quote or list arguments are not allowed in MQAPPLY; found: ~:M~%") form)
		  (setq tr-abort t)
		  nil)
		 (t
		  (setq 	fn (tr-lambda fn)
				args (tr-args args))
		  `(,(car fn) ,(cdr fn) ,@args))))
	  ((not aryp)
	   `($any simplify (mapply ,(dconvx (translate fn))
			    (list ,@(tr-args args))
			    ',fn)))
	  (t
	   (warn-meval form)
	   `($any meval ',form)))))



(def%tr mcond (form) 
  (prog (dummy mode nl) 
     (setq dummy (translate (caddr form)) 
	   mode (car dummy) 
	   nl (list dummy (translate-predicate (cadr form))))
     (do ((l (cdddr form) (cddr l))) ((null l))
       (cond ((and (not (atom (cadr l))) (eq 'mcond (caaadr l)))
	      (setq l (cdadr l))))
       (setq dummy (translate (cadr l)) 
	     mode (*union-mode mode (car dummy)) 
	     nl (cons dummy
		      (cons (translate-predicate (car l))
			    nl))))
     (setq form nil)
     (do ((l nl (cddr l))) ((null l))
       (cond ((and (eq t (cadr l)) (null (cdar l))))
	     (t (setq form
		      (cons (cons (cadr l)
				  (cond ((and (not (atom (cdar l)))
					      (cddar l)
					      (eq (cadar l) 'progn))
					 (nreverse 
					  (cons (dconv (cons (caar l)
							     (car (reverse (cddar l))))
						       mode)
						(cdr (reverse (cddar l))))))
					((and (equal (cdar l) (cadr l))
					      (atom (cdar l))) nil)
					(t (list (cdr (car l))))))
			    form)))))
     (return (cons mode (cons 'cond form)))))



;; The MDO and MDOIN translators should be changed to use the TR-LAMBDA.
;; Perhaps a mere expansion into an MPROG would be best.

(declare-top (special shit))

(def%tr mdo (form)
  (let (returns assigns return-mode local (inside-mprog t) tem need-prog?)
    (let (mode var init next test action varmode end-var init-end-var)
      (setq var (cond ((cadr form)) (t 'mdo)))
      (specialp var)
      (tbind var)
      (setq init (if (caddr form) (translate (caddr form)) '($fixnum . 1)))
      (cond ((not (setq varmode (get var 'mode)))
	     (declvalue var (car init) t)))
      (setq next (translate (cond ((cadddr form) (list '(mplus) (cadddr form) var))
				  ((car (cddddr form)))
				  (t (list '(mplus) 1 var)))))
      (setq form (copy-list form))
      ;;to make the end test for thru be numberp if the index is numberp
      ;;and to eliminate reevaluation
      tem
      (cond ((not varmode)
	     (declvalue var (*union-mode (car init) (car next)) t))
	    (t
	     (warn-mode var varmode (*union-mode (car init) (car next)))))
      (setq test (translate-predicate
		  (list '(mor)
			(cond ((null (cadr (cddddr form)))  nil)
			      ((and (cadddr form)
				    (mnegp ($numfactor (simplify (cadddr form)))))
			       (list '(mlessp) var (cadr (cddddr form))))
			      (t (list '(mgreaterp) var (cadr (cddddr form)))))
			(caddr (cddddr form)))))
      (cond ((and end-var (symbolp end-var)) (remprop end-var 'mode)
	     (remprop end-var 'tbind)))
      (setq action (translate (cadddr (cddddr form)))
	    mode (cond ((null returns) '$any)
		       (t
			(if shit
			    (do ((l returns (cdr l))) ((null l))
			      (rplaca (cdar l) (dconv (cadar l) return-mode))))
			return-mode)))
      (setq var (tunbind (cond ((cadr form)) (t 'mdo))))
      `(,mode do ((,var ,(cdr init) ,(cdr next))
		  ,@ init-end-var )
	      (,test '$done) . ((declare (special ,var)) .
	      ,(cond ((atom (cdr action)) nil)
		     ((eq 'progn (cadr action)) (cddr action))
		     (t (list (cdr action)))))))))

(setq shit nil)

(def%tr mdoin (form)
  (let (returns assigns return-mode local (inside-mprog t) need-prog?)
    (prog (mode var init action)
       (setq var (tbind (cadr form))) (tbind 'mdo)
       (specialp var)
       (setq init (dtranslate (caddr form)))
       (cond ((or (cadr (cddddr form)) (caddr (cddddr form)))
	      (tunbind 'mdo) (tunbind (cadr form))
	      (return `($any meval '((mdoin) . ,(cdr form))))))
       (setq action (translate (cadddr (cddddr form)))
	     mode (cond ((null returns) '$any)
			(t
			 (if shit
			     (do ((l returns (cdr l))) ((null l))
			       (rplaca (cdar l) (dconv (cadar l) return-mode))))
			 return-mode)))
       (tunbind 'mdo) (tunbind (cadr form))
       (return
	 `(,mode do ((,var) (mdo (cdr ,init) (cdr mdo)))
		 ((null mdo) '$done) .
		  ((declare (special ,var)) (setq ,var (car mdo)) .
		 ,(cond ((atom (cdr action)) nil)
			((eq 'progn (cadr action)) (cddr action))
			(t (list (cdr action))))))))))


(defun lambda-wrap1 (tn val form)
  (if (or (atom val)
	  (eq (car val) 'quote))
      (subst val tn form)
      `((lambda (,tn) ,form) ,val)))
	  
(def%tr msetq (form)
  (let ((var (cadr form))
	(val (caddr form))
	assign
	mode)
    (cond ((atom var)
	   (setq mode (value-mode var) val (translate val))
	   (cond ((not (tboundp var))
		  (pushnew var specials :test #'eq)))
	   (warn-mode var mode (car val))
	   (if (eq '$any mode)
	       (setq mode (car val) val (cdr val))
	       (setq val (dconv val mode)))
	   (cons mode
		 (if (setq assign (get var 'assign))
		     (let ((tn (tr-gensym)))
		       (lambda-wrap1 tn val `(progn (,assign ',var ,tn)
					      (setq ,(teval var) ,tn))))
                     `(progn
                        (if (not (boundp ',(teval var)))
                            (add2lnc ',(teval var) $values))
                        (,(if *macexpr-top-level-form-p*
                              'defparameter
                              'setq)
                         ,(teval var) ,val)))))
	  ((member 'array (car var) :test #'eq)
	   (tr-arraysetq var val))
	  (t
	   (unless (safe-get (caar var) 'mset_extension_operator)
         (tr-format (intl:gettext "warning: no assignment operator known for ~:M~%") var)
         (tr-format (intl:gettext "note: just keep going and hope for the best.~%")))
	   (setq val (translate val))
	   `(,(car val) mset ',(translate-atoms var) ,(cdr val))))))

(def%tr $rat (form)
  (cond ((null (cddr form)) (cons '$cre (dconv-$cre (translate (cadr form)))))
	(t (setq tr-abort t) (cons '$any form))))


(def%tr $max (x) (translate-$max-$min x))
(def%tr $min (x) (translate-$max-$min x))
(def%tr %max (x) (translate-$max-$min x))
(def%tr %min (x) (translate-$max-$min x))

(defun translate-$max-$min (form)
  (let   ((mode) (arglist) (op (stripdollar (caar form))))
    (setq arglist 
	  (mapcar #'(lambda (l) (setq l (translate l)
				      mode (*union-mode (car l) mode))
			    l)
		  (cdr form)))
    (if (member mode '($fixnum $float $number) :test #'eq)
	`(,mode  ,(if (eq 'min op) 'min 'max) . ,(mapcar 'cdr arglist))
	`($any ,(if (eq 'min op) '$lmin '$lmax)
	  (list '(mlist) . ,(mapcar 'dconvx arglist))))))


;;; mode acessing, binding, handling. Super over-simplified.

(defun tr-class (x)
  (cond ((integerp x) '$fixnum)
	((floatp x) '$float)
	((member x '(t nil) :test #'eq) '$boolean)
	((atom x) '$any)
	((eq 'rat (caar x)) '$rational)
	(t '$any)))

(defun *union-mode (mode1 mode2) 
  (cond ((eq mode1 mode2) mode1)
	((null mode1) mode2)
	((null mode2) mode1)
	((member mode2 *$any-modes* :test #'eq) '$any)
	((member mode1 *$any-modes* :test #'eq) '$any)
	((eq '$fixnum mode1) mode2)
	((eq '$float mode1)
	 (if (eq '$number mode2) '$number '$float))
	((eq '$rational mode1)
	 (if (eq '$float mode2) '$float '$any))
	((eq '$number mode1)
	 (if (eq '$rational mode2) '$any '$number))
	(t '$any)))

(defun value-mode (var)
  (cond ((get var 'mode))
	(t
	 (warn-undeclared var)
	 '$any)))

(defun decmode-arrayfun (f m)
  (putprop f m 'arrayfun-mode))

(defun array-mode (ar)
  (cond ((get ar 'array-mode)) (t '$any)))

(defun arrayfun-mode (ar)
  (cond ((get ar 'arrayfun-mode)) (t '$any)))

(defun function-mode (f)
  (cond ((get f 'function-mode)) (t '$any)))

(defun function-mode-@ (f)
  (ass-eq-ref (get f 'val-modes) 'function-mode '$any))

(defun array-mode-@ (f)
  (ass-eq-ref (get f 'val-modes) 'array-mode '$any))


(defvar $tr_bind_mode_hook nil
  "A hack to allow users to key the modes of variables
  off of variable spelling, and other things like that.")

(defstruct (tstack-slot (:conc-name tstack-slot-))
  mode 
  tbind
  val-modes
  ;; an alist telling second order info
  ;; about APPLY(VAR,[X]), ARRAYAPPLY(F,[X]) etc.
  special)

;;; should be a macro (TBINDV <var-list> ... forms)
;;; so that TUNBIND is assured, and also so that the stupid ASSQ doesn't
;;; have to be done on the darn TSTACK. This will have to wait till
;;; the basic special form translation properties are rewritten.

(defun variable-p (var)
  (and var (symbolp var) (not (eq var t))))

(defun bad-var-warn (var)
  (tr-format (intl:gettext "warning: ~:M cannot be used as a variable.~%") var))

(defun tbind (var &aux old)
  (cond ((variable-p var)
	 (setq old (make-tstack-slot :mode (get var 'mode)
				     :tbind (get var 'tbind)
				     :val-modes (get var 'val-modes)
				     :special (get var 'special)))
	 (push (cons var old) tstack)
	 (cond ((not (specialp var))
		;; It is the lisp convention in use to inherit
		;; specialness from higher context.
		;; Spurious MODEDECLARATIONS get put in the environment
		;; when code is MEVAL'd since there is no way to stack
		;; the mode properties. Certainly nobody is willing
		;; to hack MEVAL in JPG;MLISP
		(remprop var 'val-modes)
		(remprop var 'mode)
		(remprop var 'special)))
	 (putprop var var 'tbind)
	 (if $tr_bind_mode_hook
	     (let ((mode? (mapply $tr_bind_mode_hook
				  (list var)
				  '$tr_bind_mode_hook)))
	       (if mode? (tr-declare-varmode var mode?))))
	 var)
	(t
	 (bad-var-warn var))))

(defun tunbind (var &aux (old (assoc var tstack :test #'eq)))
  (when (variable-p var)
    (prog1
	(teval var)
      (cond (old
	     (setq tstack (delete old tstack :test #'eq)) ; POP should be all we need.
	     (setq old (cdr old))
	     (putprop1 var (tstack-slot-mode old) 'mode)
	     (putprop1 var (tstack-slot-tbind old) 'tbind)
	     (putprop1 var (tstack-slot-val-modes old) 'val-modes)
	     (putprop1 var (tstack-slot-special old) 'special))))))

(defun putprop1 (name value key)
  ;; leaves property list clean after unwinding, this
  ;; is an efficiency/storage issue only.
  (if value
      (putprop name value key)
      (progn
	(remprop name key)
	nil)))

(defun tunbinds (l)
  (do ((nl))
      ((null l) nl)
    (setq nl (cons (tunbind (caar tstack)) nl)
	  l (cdr l))))

(defun tboundp (var)
  ;; really LEXICAL-VARP.
  (and (symbolp var) (get var 'tbind) (not (get var 'special))))

(defun teval (var)
  (or (and (symbolp var) (get var 'tbind)) var))

;; Local Modes:
;; Mode: LISP
;; Comment Col: 40
;; END:
