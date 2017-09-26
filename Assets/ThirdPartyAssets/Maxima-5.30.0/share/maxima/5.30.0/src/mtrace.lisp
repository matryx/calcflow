;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module mtrace)

(declare-top (special $functions $transrun trace-allp))

;;; a reasonable trace capability for macsyma users.
;;; 8:10pm  Saturday, 10 January 1981 -GJC.

;; TRACE(F1,F2,...)   /* traces the functions */
;; TRACE()            /* returns a list of functions under trace */
;; UNTRACE(F1,F2,...) /* untraces the functions */
;; UNTRACE()          /* untraces all functions. */
;; TRACE_MAX_INDENT   /* The maximum indentation of trace printing. */
;;
;; TRACE_OPTIONS(F,option1,option2,...)  /* gives F options */
;;
;; TRACE_BREAK_ARG    /* Bound to list of argument during BREAK ENTER,
;;                      and the return value during BREAK EXIT.
;;                      This lets you change the arguments to a function,
;;                      or make a function return a different value,
;;                      which are both usefull debugging hacks.
;;
;;                      You probably want to give this a short alias
;;                      for typing convenience.
;;                    */
;;
;; An option is either a keyword, FOO.
;; or an expression  FOO(PREDICATE_FUNCTION);
;;
;; A keyword means that the option is in effect, an keyword
;; expression means to apply the predicate function to some arguments
;; to determine if the option is in effect. The argument list is always
;; [LEVEL,DIRECTION, FUNCTION, ITEM] where
;; LEVEL      is the recursion level for the function.
;; DIRECTION  is either ENTER or EXIT.
;; FUNCTION   is the name of the function.
;; ITEM       is either the argument list or the return value.
;;
;; ----------------------------------------------
;; | Keyword    | Meaning of return value       |
;; ----------------------------------------------
;; | NOPRINT    | If TRUE do no printing.       |
;; | BREAK      | If TRUE give a breakpoint.    |
;; | LISP_PRINT | If TRUE use lisp printing.    |
;; | INFO       | Extra info to print           |
;; | ERRORCATCH | If TRUE errors are caught.    |
;; ----------------------------------------------
;;
;; General interface functions. These would be called by user debugging utilities.
;;
;; TRACE_IT('F)             /* Trace the function named F */
;; TRACE                    /* list of functions presently traced. */
;; UNTRACE_IT('F)           /* Untrace the function named F */
;; GET('F,'TRACE_OPTIONS)  /* Access the trace options of F */
;;
;; Sophisticated feature:
;; TRACE_SAFETY a variable with default value TRUE.
;; Example: F(X):=X; BREAKP([L]):=(PRINT("Hi!",L),FALSE),
;; TRACE(F,BREAKP); TRACE_OPTIONS(F,BREAK(BREAKP));
;; F(X); Note that even though BREAKP is traced, and it is called,
;; it does not print out as if it were traced. If you set
;; TRACE_SAFETY:FALSE; then F(X); will cause a normal trace-printing
;; for BREAKP. However, then consider TRACE_OPTIONS(BREAKP,BREAK(BREAKP));
;; When TRACE_SAFETY:FALSE; F(X); will give an infinite recursion,
;; which it would not if safety were turned on.
;; [Just thinking about this gives me a headache.]

;; Internal notes on this package:				-jkf
;; Trace works by storing away the real definition of a function and
;; replacing it by a 'shadow' function.  The shadow function prints a
;; message, calls the real function, and then prints a message as it
;; leaves.  The type of the shadow function may differ from the
;; function being shadowed.  The chart below shows what type of shadow
;; function is needed for each type of Macsyma function.
;;
;; Macsyma function	shadow type	hook type	mget
;; ____________________________________________________________
;;	subr		 expr		  expr
;;	expr		 expr		  expr
;;	lsubr		 expr		  expr
;;	fsubr		 fexpr		  fexpr
;;	fexpr		 fexpr		  fexpr
;;	mexpr		 expr		  expr		t
;;	mfexpr*		 mfexpr*	  macro
;;	mfexpr*s	 mfexpr*	  macro
;;
;; The 'hook type' refers to the form of the shadow function.  'expr' types
;; are really lexprs, they expect any number of evaluated arguments.
;; 'fexpr' types expect one unevaluated argument which is the list of
;; arguments.  'macro' types expect one argument, the caar of which is the
;; name of the function, and the cdr of which is a list of arguments.
;;
;; For systems which store all function properties on the property list,
;; it is easy to shadow a function.  For systems with function cells,
;; the situation is a bit more difficult since the standard types of
;; functions are stored in the function cell (expr,fexpr,lexpr), whereas
;; the macsyma functions (mfexpr*,...) are stored on the property list.
;;

;; 1) The variety of maxima functions is much more restricted than
;;    what the table above shows.  I think the following table gives
;;    the correct picture (like its counterpart, it ignores maxima
;;    macros or functional arrays).
;;
;;
;; Maxima function	shadow type	hook type	mget
;; ____________________________________________________________
;;	expr		 expr		  expr
;;	mexpr		 expr		  expr		t
;;	mfexpr*		 mfexpr*	  expr
;;
;; These types have the following meaning: Suppose MFUN evaluates to some
;; symbol in the MAXIMA package.  That this symbol is of type
;;
;;  - EXPR (or SUBR) implies that it has a lisp function definition
;;    (SYMBOL-FUNCTION MFUN).
;;
;;  - MEXPR implies that it has a (parsed) maxima language definition
;;    (MGET MFUN 'MEXPR) and all arguments are evaluated by MEVAL.
;;
;;  - MFEXPR* implies that it has a lisp function definition
;;    (GET MFUN 'MFEXPR*) and its arguments are not automatically
;;    evaluated by MEVAL.
;;
;; Note that the shadow type has to agree with the original function's
;; type in the way arguments are evaluated.  On the other hand, I think
;; we are left with EXPR as the only hook type; as a matter of fact, this
;; is equivalent to the next point:
;;
;; 2) There is no need for MAKE-TRACE-HOOK to dispatch with respect to
;; HOOK-TYPE since, roughly speaking, proper handling of the traced
;; function's arguments is done by the trace handler in concert with
;; MEVAL*.
;;
;; Note that I also removed the COPY-LIST used to pass the traced
;; function's argument list to the trace handler.
;;
;; There remains an annoying problem with translated functions: tracing
;; some function of type MEXPR and then loading its translated version
;; (which is of type EXPR) will not cleanly untrace it (i.e., it is
;; effectively no longer traced but it remains on the list of traced
;; functions).  I think that this has to be fixed somewhere in the
;; translation package. -wj


;;; Structures.

(eval-when
    #+gcl (compile load eval)
    #-gcl (:compile-toplevel :load-toplevel :execute)
    (defmacro trace-p (x)
      `(mget ,x 'trace))
    (defmacro trace-type (x)
      `(mget ,x 'trace-type))
    (defmacro trace-level (x)
      `(mget ,x 'trace-level))
    (defmacro trace-options (x)
      `($get ,x '$trace_options))
    (defmacro trace-oldfun (x)
      `(mget ,x 'trace-oldfun)))

;;; User interface functions.

(defmvar $trace (list '(mlist)) "List of functions actively traced")

(defun mlistcan-$all (fun llist default)
  "totally random utility function"
  (let (trace-allp)
    (if (null llist)
	default
	`((mlist) ,@(mapcan fun
			    (if (member (car llist) '($all $functions) :test #'eq)
				(prog2
				    (setq trace-allp t)
				    (mapcar #'caar (cdr $functions)))
				llist))))))

(defmspec $trace (form)
  (mlistcan-$all #'macsyma-trace (cdr form) $trace))

(defmfun $trace_it (function)
  `((mlist) ,@(macsyma-trace function)))

(defmspec $untrace (form)
  `((mlist) ,@(mapcan #'macsyma-untrace (or (cdr form) (cdr $trace)))))

(defmfun $untrace_it (function)
  `((mlist) ,@(macsyma-untrace function)))

(defmspec $trace_options (form)
  (setf (trace-options (cadr form))
	`((mlist) ,@(cddr form))))

;;; System interface functions.

(defvar hard-to-trace '(trace-handler listify args trace-apply *apply mapply))

;; A list of functions called by the TRACE-HANDLEr at times when
;; it cannot possibly shield itself from a continuation which would
;; cause infinite recursion. We are assuming the best-case of
;; compile code.

(defun macsyma-trace (fun)
  (macsyma-trace-sub fun 'trace-handler $trace))

(defun macsyma-trace-sub (fun handler ilist &aux temp)
  (cond ((not (symbolp (setq fun (getopr fun))))
	 (mtell (intl:gettext "trace: argument is apparently not a function or operator: ~M~%") fun)
	 nil)
	((trace-p fun)
	 ;; Things which redefine should be expected to reset this
	 ;; to NIL.
	 (if (not trace-allp) (mtell (intl:gettext "trace: function ~@:M is already traced.~%") fun))
	 nil)
	((member fun hard-to-trace :test #'eq)
	 (mtell (intl:gettext "trace: ~@:M cannot be traced.~%") fun)
	 nil)
	((not (setq temp (car (macsyma-fsymeval fun))))
	 (mtell (intl:gettext "trace: ~@:M has no functional properties.~%") fun)
	 nil)
	((member temp '(mmacro translated-mmacro) :test #'eq)
	 (mtell (intl:gettext "trace: ~@:M is a macro, so it won't trace well; try 'macroexpand' to debug it.~%") fun)
	 nil)
	((get temp 'shadow)
	 (put-trace-info fun temp ilist)
	 (trace-fshadow fun temp (make-trace-hook fun temp handler))
	 (list fun))
	(t
	 (mtell (intl:gettext "trace: ~@:M is an unknown type of function.~%") fun)
	 nil)))

(defvar trace-handling-stack ())

(defun macsyma-untrace (fun)
  (macsyma-untrace-sub fun 'trace-handler $trace))

(defun macsyma-untrace-sub (fun handler ilist)
  (prog1
      (cond ((not (symbolp (setq fun (getopr fun))))
	     (mtell (intl:gettext "untrace: argument is apparently not a function or operator: ~M~%") fun)
	     nil)
	    ((not (trace-p fun))
	     (mtell (intl:gettext "untrace: ~@:M is not traced.~%") fun)
	     nil)
	    (t
	     (trace-unfshadow fun (trace-type fun))
	     (rem-trace-info fun ilist)
	     (list fun)))
    (if (member fun trace-handling-stack :test #'eq)
	;; yes, he has re-defined or untraced the function
	;; during the trace-handling application.
	;; This is not strange, in fact it happens all the
	;; time when the user is using the $ERRORCATCH option!
	(macsyma-trace-sub fun handler ilist))))

(defun put-trace-info (fun type ilist)
  (setf (trace-p fun) fun)	 ; needed for MEVAL at this time also.
  (setf (trace-type fun) type)
  (setf (trace-oldfun fun) (and (fboundp fun) (symbol-function fun)))
  (let ((sym (gensym)))
    (setf (symbol-value sym) 0)
    (setf (trace-level fun) sym))
  (push fun (cdr ilist))
  (list fun))

(defun rem-trace-info (fun ilist)
  (setf (trace-p fun) nil)
  (or (member fun trace-handling-stack :test #'eq)
      (setf (trace-level fun) nil))
  (setf (trace-type fun) nil)
  (setq ilist (delete fun ilist :test #'eq))
  (list fun))

;; Placing the TRACE functional hook.
;; Because the function properties in macsyma are used by the EDITOR, SAVE,
;; and GRIND commands it is not possible to simply replace the function
;; being traced with a hook and to store the old definition someplace.
;; [We do know how to cons up machine-code hooks on the fly, so that
;;  is not stopping us].


;; This data should be formalized somehow at the time of
;; definition of the DEFining form.

(defprop subr expr shadow)
(defprop lsubr expr shadow)
(defprop expr expr shadow)
(defprop mfexpr*s mfexpr* shadow)
(defprop mfexpr* mfexpr* shadow)

(defprop subr t uuolinks)
(defprop lsubr t uuolinks)
(defprop fsubr t uuolinks)		; believe it or not.

(defprop mexpr t mget)
(defprop mexpr expr shadow)

(defun get! (x y)
  (or (get x y)
      (maxima-error (intl:gettext "GET!: property ~a of symbol ~a undefined.") y x)))

(defun trace-fshadow (fun type value)
  (let ((shadow (get! type 'shadow)))
    (cond ((member shadow '(expr subr) :test #'eq)
	   (setf (trace-oldfun fun) (and (fboundp fun) (symbol-function fun)))
	   (setf (symbol-function fun) value))
	  (t
	   (setf (symbol-plist fun) `(,shadow ,value ,@(symbol-plist fun)))))))

(defun trace-unfshadow (fun type)
  ;; At this point, we know that FUN is traced.
  (cond ((member type '(expr subr) :test #'eq)
	 (let ((oldf (trace-oldfun fun)))
	   (if (not (null oldf))
	       (setf (symbol-function  fun)  oldf)
	       (fmakunbound fun))))
	(t (remprop fun (get! type 'shadow))
	   (fmakunbound fun))))

;;--- trace-fsymeval :: find original function
;;  fun : a function which is being traced.  The original defintion may
;;	 be hidden on the property list behind the shadow function.
;;
(defun trace-fsymeval (fun)
  (or
   (let ((type-of (trace-type fun)))
     (cond ((get type-of 'mget)
	    (if (eq (get! type-of 'shadow) type-of)
		(mget (cdr (mgetl fun (list type-of))) type-of)
		(mget fun type-of)))
	   ((eq (get! type-of 'shadow) 'expr)
	    (trace-oldfun fun))
	   (t (if (eq (get! type-of 'shadow) type-of)
		  (cadr (getl (cdr (getl fun `(,type-of))) `(,type-of)))
		  (get fun type-of)))))
   (trace-fsymeval
    (merror "internal error: trace property for ~:@M went away without hook." fun))))

;;; The handling of a traced call.

(defvar trace-indent-level -1)

(defmacro bind-sym (symbol value . body)
  ;; is by far the best dynamic binding generally available.
  `(progv (list ,symbol)
    (list ,value)
    ,@body))

;; We really want to (BINDF (TRACE-LEVEL FUN) (1+ (TRACE-LEVEL FUN)) ...)
;; (Think about PROGV and SETF and BINDF. If the trace object where
;; a closure, then we want to fluid bind instance variables.)

(declare-top (special errcatch lisperrprint bindlist loclist))

(defmacro macsyma-errset (form &aux (ret (gensym)))
  `(let ((errcatch (cons bindlist loclist)) ,ret)
    (setq ,ret (errset ,form lisperrprint))
    (or ,ret (errlfun1 errcatch))
    ,ret))

(defvar predicate-arglist nil)

(defvar return-to-trace-handle nil)

(defun trace-handler (fun largs)
  (if (or return-to-trace-handle
          (and (not (atom (car largs)))
               (not (atom (caar largs)))
               (eq (caaar largs) '$untrace)
               (eq (cadar largs) fun)))
      ;; We were called by the trace-handler or by $untrace and the function
      ;; fun is to be untraced.
      (trace-apply fun largs)
      (let ((trace-indent-level (1+ trace-indent-level))
	    (return-to-trace-handle t)
	    (trace-handling-stack (cons fun trace-handling-stack))
	    (level-sym (trace-level fun))
	    (level))
	(setq level (1+ (symbol-value level-sym)))
	(bind-sym level-sym level
		  (do ((ret-val)
		       (continuation)
		       (predicate-arglist))
		      (nil)
		    (setq predicate-arglist `(,level $enter ,fun ((mlist) ,@largs)))
		    (setq largs (trace-enter-break fun level largs))
		    (trace-enter-print fun level largs)
		    (cond ((trace-option-p fun '$errorcatch)
			   (setq ret-val (macsyma-errset (trace-apply fun largs)))
			   (cond ((null ret-val)
				  (setq ret-val (trace-error-break fun level largs))
				  (setq continuation (car ret-val)
					ret-val (cdr ret-val)))
				 (t
				  (setq continuation 'exit
					ret-val (car ret-val)))))
			  (t
			   (setq continuation 'exit
				 ret-val (trace-apply fun largs))))
		    (case continuation
		      ((exit)
		       (setq predicate-arglist `(,level $exit ,fun ,ret-val))
		       (setq ret-val (trace-exit-break fun level ret-val))
		       (trace-exit-print fun level ret-val)
		       (return ret-val))
		      ((retry)
		       (setq largs ret-val)
		       (mtell "TRACE-HANDLER: reapplying the function ~:@M~%" fun))
		      ((maxima-error)
		       (merror "~%TRACE-HANDLER: signaling 'maxima-error' for function ~:@M~%" fun))))))))


;; The (Trace-options function) access is not optimized to take place
;; only once per trace-handle call. This is so that the user may change
;; options during his break loops.
;; Question: Should we bind return-to-trace-handle to NIL when we
;; call the user's predicate? He has control over his own lossage.

(defmvar $trace_safety t "This is subtle")

(defun trace-option-p (function keyword)
  (do ((options
	(let ((options (trace-options function)))
	  (cond ((null options) nil)
		(($listp options) (cdr options))
		(t
		 (mtell "TRACE-OPTION-P: trace options for ~:@M not a list, so ignored.~%" function)
		 nil)))
	(cdr options))
       (option))
      ((null options) nil)
    (setq option (car options))
    (cond ((atom option)
	   (if (eq option keyword) (return t)))
	  ((eq (caar option) keyword)
	   (let ((return-to-trace-handle $trace_safety))
	     (return (mapply (cadr option) predicate-arglist
			     "A trace option predicate")))))))


(defun trace-enter-print (fun lev largs &aux (mlargs `((mlist) ,@largs)))
  (if (not (trace-option-p fun '$noprint))
      (let ((info (trace-option-p fun '$info)))
	(cond ((trace-option-p fun '$lisp_print)
	       (trace-print `(,lev enter ,fun ,largs ,@info)))
	      (t
	       (trace-mprint lev (intl:gettext " Enter ") (mopstringnam fun) " " mlargs
			     (if info " -> " "")
			     (if info info "")))))))

(defun mopstringnam (x)
  (maknam (mstring (getop x))))

(defun trace-exit-print (fun lev ret-val)
  (if (not (trace-option-p fun '$noprint))
      (let ((info (trace-option-p fun '$info)))
	(cond ((trace-option-p fun '$lisp_print)
	       (trace-print `(,lev exit ,fun ,ret-val ,@info)))
	      (t
	       (trace-mprint lev (intl:gettext " Exit  ") (mopstringnam fun) " " ret-val
			     (if info " -> " "")
			     (if info info "")))))))

(defmvar $trace_break_arg '$trace_break_arg
  "During trace Breakpoints bound to the argument list or return value")

(defun trace-enter-break (fun lev largs)
  (if (trace-option-p fun '$break)
      (do ((return-to-trace-handle nil)
	   ($trace_break_arg `((mlist) ,@largs)))(nil)
	($break "Trace entering" fun "level" lev)
	(cond (($listp $trace_break_arg)
	       (return (cdr $trace_break_arg)))
	      (t
	       (mtell "TRACE-ENTER-BREAK: 'trace_break_arg' must be a list.~%"))))
      largs))

(defun trace-exit-break (fun lev ret-val)
  (if (trace-option-p fun '$break)
      (let (($trace_break_arg ret-val)
	    (return-to-trace-handle nil))
	($break "Trace exiting" fun "level" lev)
	$trace_break_arg)
      ret-val))

(defun pred-$read (predicate argl bad-message)
  (do ((ans))(nil)
    (setq ans (apply #'$read argl))
    (if (funcall predicate ans) (return ans))
    (mtell "PRED-$READ: unacceptable input: ~A~%" bad-message)))

(declare-top (special upper))

(defun ask-choicep (llist &rest header-message)
  (do ((j 0 (1+ j))
       (dlist nil (list* #\newline `((marrow) ,j ,(car ilist)) dlist))
       (ilist llist (cdr ilist)))
      ((null ilist)
       (setq dlist (nconc header-message (cons #\newline (nreverse dlist))))
       (let ((upper (1- j)))
	 (pred-$read #'(lambda (val)
			 (and (integerp val)
			      (>= val 0)
			      (<= val upper)))
		     dlist
		     "please reply with an integer from the menue.")))))

(declare-top (unspecial upper))

;; I GUESS ALL OF THE STRINGS IN THIS FUNCTION NEED TO BE GETTEXT'D TOO
;; JUST CAN'T BRING MYSELF TO DO IT

(defun trace-error-break (fun level largs)
  (case (ask-choicep '("Signal an `maxima-error', i.e. punt?"
		       "Retry with same arguments?"
		       "Retry with new arguments?"
		       "Exit with user supplied value")
		     "Error during application of" (mopstringnam fun)
		     "at level" level
		     #\newline "Do you want to:")
    ((0)
     '(maxima-error))
    ((1)
     (cons 'retry largs))
    ((2)
     (cons 'retry (let (($trace_break_arg `((mlist) ,largs)))
		    (cdr (pred-$read '$listp
				     (list
				      "Enter new argument list for"
				      (mopstringnam fun))
				     "please enter a list.")))))

    ((3)
     (cons 'exit ($read "Enter value to return")))))

;;; application dispatch, and the consing up of the trace hook.

(defun macsyma-fsymeval (fun)
  (let ((try (macsyma-fsymeval-sub fun)))
    (cond (try try)
	  ((get fun 'autoload)
	   (load-and-tell (get fun 'autoload))
	   (setq try (macsyma-fsymeval-sub fun))
	   (or try
	       (mtell (intl:gettext "trace: ~@:M has no functional properties after autoloading.~%")
		      fun))
	   try)
	  (t try))))

(defun macsyma-fsymeval-sub (fun)
  ;; The semantics of $TRANSRUN are herein taken from DESCRIBE,
  ;; a carefull reading of MEVAL1 reveals, well... I've promised to watch
  ;; my language in these comments.

  (let ((mprops (mgetl fun '(mexpr mmacro)))
	(lprops (getl  fun '(translated-mmacro mfexpr* mfexpr*s)))
	(fcell-props (getl-lm-fcn-prop fun '(subr lsubr expr macro))))
    (cond ($transrun
	   ;; the default, so its really a waste to have looked for
	   ;; those mprops. Its better to fix the crock than to
	   ;; optimize this though!
	   (or lprops fcell-props mprops))
	  (t
	   (or mprops lprops fcell-props)))))

(defprop expr expr hook-type)
(defprop mexpr expr hook-type)
(defprop subr expr hook-type)
(defprop lsubr expr hook-type)
(defprop mfexpr* macro hook-type)
(defprop mfexpr*s macro hook-type)

(defun make-trace-hook (fun type handler)
  ;; Argument handling according to FUN's TYPE is already done
  ;; elsewhere: HANDLER, meval...
  (declare (ignore type))
  #'(lambda (&rest trace-args)
      (funcall handler fun trace-args)))

(defmacro trace-setup-call (prop fun type)
  (declare (ignore fun type))
  `(setf (symbol-function 'the-trace-apply-hack) ,prop))

(defun trace-apply (fun largs)
  (let ((prop (trace-fsymeval fun))
	(type (trace-type fun))
	(return-to-trace-handle nil))
    (case type
      ((mexpr)
       (mapply prop largs "A traced function"))
      ((expr)
       (apply prop largs))
      ((subr lsubr)
       (setf (symbol-plist 'the-trace-apply-hack) (list type prop))
       (apply (second (getl 'the-trace-apply-hack '(subr lsubr))) largs))
      ((mfexpr* mfexpr*s)
       (funcall prop (car largs))))))

;;; I/O cruft

(defmvar $trace_max_indent 15. "max number of spaces it will go right" fixnum)

(putprop '$trace_max_indent 'assign-mode-check 'assign)
(putprop '$trace_max_indent '$fixnum 'mode)

(defun-prop (spaceout dimension) (form result)
  (dimension-string (make-list (cadr form) :initial-element #\space) result))

(defun trace-mprint (&rest l)
  (mtell-open "~M" `((mtext) ((spaceout) ,(min $trace_max_indent trace-indent-level)) ,@l)))

(defun trace-print (form)
  (terpri)
  (do ((j (min $trace_max_indent trace-indent-level) (1- j)))
      ((not (> j 0)))
    (write-char #\space))
  (if *prin1*
      (funcall *prin1* form)
      (prin1 form))
  (write-char #\space))


;; 9:02pm  Monday, 18 May 1981 -GJC
;; A function benchmark facility using trace utilities.
;; This provides medium accuracy, enough for most user needs.

(defmvar $timer '((mlist)) "List of functions under active timetrace")

(defmspec $timer (form)
  (mlistcan-$all #'macsyma-timer (cdr form) $timer))

(defmspec $untimer (form)
  `((mlist) ,@(mapcan #'macsyma-untimer (or (cdr form) (cdr $timer)))))

(defun micro-to-sec (runtime)
  (mul runtime (float (/ internal-time-units-per-second)) '$sec))

(defun micro-per-call-to-sec (runtime calls)
  (div (micro-to-sec runtime)
       (if (zerop calls) 1 calls)))

(defun timer-mlist (function calls runtime gctime)
  `((mlist simp) ,function
    ,(micro-per-call-to-sec (+ runtime gctime) calls)
    ,calls
    ,(micro-to-sec runtime)
    ,(micro-to-sec gctime)))

(defmspec $timer_info (form)
  (do ((l (or (cdr form) (cdr $timer))
	  (cdr l))
       (v nil)
       (total-runtime 0)
       (total-gctime 0)
       (total-calls 0))
      ((null l)
       `(($matrix simp)
	 ((mlist simp) $function $time//call $calls $runtime $gctime)
	 ,.(nreverse v)
	 ,(timer-mlist '$total total-calls total-runtime total-gctime)))
    (let*
      ((fun-opr (getopr (car l)))
       (runtime ($get fun-opr '$runtime))
       (gctime  ($get fun-opr '$gctime))
       (calls   ($get fun-opr '$calls)))
      (when runtime
	(incf total-calls calls)
	(incf total-runtime runtime)
	(incf total-gctime gctime)
	(push (timer-mlist (car l) calls runtime gctime) v)))))

(defun macsyma-timer (fun)
  (prog1
      (macsyma-trace-sub fun 'timer-handler $timer)
      (let ((fun-opr (getopr fun)))
        ($put fun-opr 0 '$runtime)
        ($put fun-opr 0 '$gctime)
        ($put fun-opr 0 '$calls))))

(defun macsyma-untimer (fun) (macsyma-untrace-sub fun 'timer-handler $timer))

(defvar runtime-devalue 0)
(defvar gctime-devalue 0)

(defmvar $timer_devalue nil
  "If true, then time spent inside calls to other timed functions is
  subtracted from the timing figure for a function.")

(defun timer-handler (fun largs)
  ;; N.B. Doesn't even try to account for use of DYNAMIC CONTROL
  ;; such as ERRSET ERROR and CATCH and THROW, as these are
  ;; rare and the overhead for the unwind-protect is high.
  (let ((runtime (get-internal-run-time))
	(gctime (status gctime))
	(old-runtime-devalue runtime-devalue)
	(old-gctime-devalue gctime-devalue))
    (prog1 (trace-apply fun largs)
      (setq old-runtime-devalue (- runtime-devalue old-runtime-devalue))
      (setq old-gctime-devalue (- gctime-devalue old-gctime-devalue))
      (setq runtime (- (get-internal-run-time) runtime old-runtime-devalue))
      (setq gctime (- (status gctime) gctime old-gctime-devalue))
      (when $timer_devalue
	(incf runtime-devalue runtime)
	(incf gctime-devalue gctime))
      ($put fun (+ ($get fun '$runtime) runtime) '$runtime)
      ($put fun (+ ($get fun '$gctime) gctime) '$gctime)
      ($put fun (1+ ($get fun '$calls)) '$calls))))
