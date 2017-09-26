;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       (c) Copyright 1980 Massachusetts Institute of Technology       ;;;
;;;       Maintained by GJC                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module trans3)

;;; The translation of macsyma LAMBDA into lexicaly scoped closures.
;;; Two cases [1] the downward transmission of variable binding environment,
;;; e.g. MAP(LAMBDA([U],F(U,X)),EXP)
;;; [2] downward and upward, requiring a full closure, e.g.
;;; MAP(LAMBDA([U],SUM:SUM+U),EXP);

;;; LAMBDA([U],F(U,X)) =>
;;; (DOWN-CLOSE (LAMBDA (U) (F U X)) (X))

;;; TBIND, TBOUNDP, and TUNBIND and TUNBINDS hack lexical scoping.

;;; A function to determine free vars from a lisp expression.
;;; It returns a <var-set> which is a list of pairs
;;; (<var> . <side-effectp>)

;;; N.B. This code does a veritable storm of consing, it need not
;;; do any if it used the lambda-bound plist scheme of GJC;UTRANS >
;;; a compiler is allowed to cons though, isn't it?

(defun free-lisp-vars (exp &aux prop)
  (cond ((atom exp)
	 (cond ((or (null exp)(eq t exp)) nil)
	       ((symbolp exp) `((,exp . nil)))
	       (t nil)))
	((atom (car exp))
	 (cond ((setq prop (get (car exp) 'free-lisp-vars))
		(funcall prop exp))
	       ((setq prop (get (car exp) 'free-lisp-vars-macro))
		(free-lisp-vars (funcall prop exp)))
	       ((setq prop (get (car exp) 'macro))
		(free-lisp-vars (funcall prop exp)))
	       ((getl (car exp) '(fsubr fexpr))
		(warn-fexpr (car exp)
			    "environment may fail to be correct.")
		(free-lisp-vars-of-argl (cdr exp)))
	       (t
		(free-lisp-vars-of-argl (cdr exp)))))
	((eq (caar exp) 'lambda)
	 (sum-var-sets (free-lisp-vars (car exp))
		       (free-lisp-vars-of-argl (cdr exp))))
	(t
	 (barfo "Bad lisp expression generated."))))


(defun free-lisp-vars-of-argl (argl)
  (union-var-set (mapcar #'free-lisp-vars argl)))

;;; (REDUCE-VAR-SET '((A . NIL) NIL (B . T) (B . NIL))) => ((A . NIL) (B . T))
;;;  mult-set reduction.

(defun reduce-var-set&op (var-set op)
  (do ((var-set var-set (cdr var-set))
       (reduced-var-set nil)
       (var1)
       (var2))
      ((null var-set) reduced-var-set)
    (setq var1 (car var-set))
    (cond ((null var1))
	  ((setq var2 (assoc (car var1) reduced-var-set :test #'eq))
	   (rplacd var2 (funcall op (cdr var1) (cdr var2))))
	  (t
	   (push var1 reduced-var-set)))))

(defun reduce-var-set (var-set)
  (reduce-var-set&op var-set #'(lambda (p1 p2)(or p1 p2))))

;;; S1 - S2. S1 reduced, minus any vars that are in S2.

(defun difference-var-sets (s1 s2)
  (setq s1 (reduce-var-set s1))
  (do ((s nil))
      ((null s1) s)
    (cond ((assoc (caar s1) s2 :test #'eq))	;;; is the first elem of S1 a member of S2?
	  (t
	   (push (car s1) s)))  ;;; yes. shove it in.
    (pop s1)))

;;; N.B. union of var sets is defined classicaly ala G.F.

(defun union-var-set (set-of-var-sets)
  (reduce-var-set (apply #'append set-of-var-sets)))

;;; SUM-VAR-SETS is the usual convention.

(defun sum-var-sets (&rest l)
  (reduce-var-set (apply #'append l))) ; consing up a storm aren't we?

(defun make-var-set (vars)
  (loop for v in vars collect (ncons v)))

;;; (LAMBDA <BVL> . <BODY>)

(defun-prop (lambda free-lisp-vars) (form)
  (difference-var-sets (free-lisp-vars-of-argl (cddr form))
		       (cond ((null (cadr form))
			      nil)
			     ((atom (cadr form))
			      (make-var-set (list (cadr form))))
			     (t
			      (make-var-set (cadr form))))))

;;; (PROG <BVL> . <BODY>)

(defun-prop (prog free-lisp-vars) (form)
  (difference-var-sets (union-var-set
			(mapcar #'(lambda (u)
				    (cond ((atom u) nil) ;; go tag.
					  (t
					   (free-lisp-vars u))))
				(cddr form)))
		       (make-var-set (cadr form))))

;;; no computed gos please.
(defun-prop (go free-lisp-vars) (ignor)ignor nil)

;;; (DO ((<V> <V> <V>) ...) ((<in-scope>) ..) ...)

(defun-prop (do free-lisp-vars) (form)
  (difference-var-sets
   (sum-var-sets (free-lisp-vars-of-argl (cdddr form))
		 (free-lisp-vars-of-argl (caddr form))
		 (union-var-set (mapcar #'(lambda (do-iter)
					    (free-lisp-vars-of-argl
					     (cdr do-iter)))
					(cadr form))))
   (make-var-set (mapcar #'car (cadr form)))))

;;; (COND (<I> ..) (<J> ..) ...)

(defun-prop (cond free-lisp-vars) (form)
  (union-var-set (mapcar #'free-lisp-vars-of-argl (cdr form))))

(defun-prop (quote free-lisp-vars) (ignor)ignor nil)
(defun-prop (function free-lisp-vars) (ignor)ignor nil)

;;; (SETQ ... ODD AND EVENS...)

(defun-prop (setq free-lisp-vars) (form)
  (do ((free-vars nil (sum-var-sets `((,(car form) . t))
				    (free-lisp-vars (cadr form))
				    free-vars))
       (form (cdr form) (cddr form)))
      ((null form) free-vars)))

;;; uhm. LAMBDA, PROG, GO, DO, COND, QUOTE, SETQ.

(defun-prop (and free-lisp-vars)(form)(free-lisp-vars-of-argl (cdr form)))
(defun-prop (or free-lisp-vars)(form)(free-lisp-vars-of-argl (cdr form)))

(defun-prop (comment free-lisp-vars) (ignor)ignor nil)
(defun-prop (declare free-lisp-vars) (ignor) ignor nil)

;;; these next forms are generated by TRANSLATE.

(defprop $piece t sort-of-lexical)

(defun-prop (trd-msymeval free-lisp-vars) (form)
  (if (get (cadr form) 'sort-of-lexical)
      ;; acts like a lexical variable because of the $SUBSTPART translator.
      (list (list (cadr form)))
      ()))

(defun-prop (mfunction-call free-lisp-vars) (form)
  ;; it is not strictly known if the name of the function being called
  ;; is a variable or not. lets say its not.
  (free-lisp-vars-of-argl (cddr form)))

;;; (FUNGEN&ENV-FOR-MEVAL () () EXP)
(defun-prop (fungen&env-for-meval free-lisp-vars) (form)
  (free-lisp-vars (car (cdddr form))))

;;; the various augmented lambda forms.

(defun free-lisp-vars-m-tlambda (form)
  (difference-var-sets (free-lisp-vars-of-argl (cddr form))
		       (free-lisp-vars-of-argl (cadr form))))

(mapc #'(lambda (u) (putprop u 'free-lisp-vars-m-tlambda 'free-lisp-vars))
      '(m-tlambda m-tlambda&))

(defun free-lisp-vars-m-tlambda&env (form)
  (difference-var-sets (free-lisp-vars-of-argl (cddr form))
		       (free-lisp-vars-of-argl (car (cadr form)))))

(defprop m-tlambda&env free-lisp-vars-m-tlambda&env free-lisp-vars)
(defprop m-tlambda&env& free-lisp-vars-m-tlambda&env free-lisp-vars)

;;; Other entry points:

(defun tbound-free-vars (free-varl)
  ;; Takes a FREE-VAR list and returns a list of two lists.
  ;; the tbound free vars and the tbound free vars that are
  ;; side effected also.
  (do ((free nil)
       (free&s nil))
      ((null free-varl) (list free free&s))
    (let ((v (pop free-varl)))
      (cond ((and (tboundp (car v))
		  (not (get (car v) 'special)))
	     (push (car v) free)
	     (cond ((cdr v)
		    (push (car v) free&s))))))))

(defun side-effect-free-check (varl form)
  (cond ((null varl) t)
	(t
	 (tr-format (intl:gettext "error: unsupported side effects on ~:M in expression ~M~%") `((mlist) ,@varl) form)
	 nil)))


;;; O.K. here is the translate property for LAMBDA.
;;; given catch and throw we don't know where a funarg lambda
;;; may end up.

;;; Cases:
;;; I. No side effects on free variables.
;;;    A. one funarg only, not reconsed. e.g.
;;;       F(N,L):=MAP(LAMBDA([U],Q(N,U)),L)$
;;;       (PROGN (SET-ENV <*LINK*> N)
;;;              (FUNCTION (LAMBDA (U) (LET ((N (GET-ENV *LINK*))) (f* U N)))))
;;;    B. need new instance of the environment each time,
;;;       F(N):=LAMBDA([U],N*U);
;;;       `(LAMBDA (U) (gen-func U 'N)) without extend loaded.
;;; II. side effects.
;;;    A. Those since effects need to be propogated to the environment
;;;       where the LAMBDA was made. This is difficult to do in the
;;;       present translator. e.g.
;;;       F(L):=BLOCK([SUM:0],FULLMAP(LAMBDA([U],SUM:SUM+U),L),SUM);
;;;       every function which guarantees the order of argument evalation
;;;       (MPROG and MPROGN), must translate and expression and get information
;;;       about environment propagation.
;;;       (PROGN (FULLMAP (PROGN (SET-ENV) '(LAMBDA ...)) L)
;;;              (GET-ENV)), uhm. this is pretty tricky anyway.
;;;    B. side effects only have to be maintained inside the LAMBDA.
;;;       this is easier, and if you have it, you really don't need II.A.
;;;       since you can always ask the LAMBDA for its environment by
;;;       calling it on the proper message {If the LAMBDA is written that way}.


;;; ((LAMBDA) ((MLIST) X Y ((MLIST Z))) . <BODY>)
;;; must also handle the &REST arguments. N.B. MAPPLY correctly handles
;;; the application of a lisp lambda form.


;;; Some forms know that the lambda is not going to
;;; be an upward funarg, that it is not possible (wanted)
;;; have two different lambda's generated from the same
;;; place. e.g. INTERPOLATE(SIN(X^2)=A,X,0,N) (implied lambda
;;; which is contructed by the translation property for
;;; interpolate. MAP(LAMBDA([U],...),L) is another example)
;;; these forms will be called I-LAMBDA's, and will be generated
;;; from LAMBDA's by the functions that want to. All this
;;; is meaningless in the present macsyma evaluator of course, since
;;; it uses dynamic binding and just hopes for the best.

(def%tr lambda (form)
  (gen-tr-lambda form))

;;; we keep a pointer to the original FORM so that we can
;;; generate messages with it if need be.

(defun gen-tr-lambda (form &aux arg-info mode frees t-form)
  (setq arg-info (mapcar #'(lambda (v)
			     (cond ((atom v) nil)
				   ((and (eq (caar v) 'mlist)
					 (atom (cadr v)))
				    t)
				   (t '*bad*)))
			 (cdr (cadr form))))
  (cond ((or (member '*bad* arg-info :test #'eq)
	     (and (member t arg-info :test #'eq)
		  (cdr (member t arg-info :test #'eq)))) ;;; the &REST is not the last one.
	 (tr-format (intl:gettext "error: unsupported argument list ~:M in lambda expression.~%") (cadr form))
	 (setq tr-abort t)
	 nil)
	(t
	 (setq arg-info (member t arg-info :test #'eq) ;; &RESTP
	       t-form
	       (tr-lambda `((lambda)
			    ((mlist) ,@(mapcar #'(lambda (v)
						   (cond ((atom v) v)
							 (t (cadr v))))
					       (cdr (cadr form))))
			    ,@(cddr form)))
	       mode (car t-form)   ; not much to do with the mode now,
	       t-form (cdr t-form) ; could be use by a global optimizer.
	       frees (tbound-free-vars (free-lisp-vars t-form)))))
					; with this info we now dispatch to the various macros forms.
					; (cadr t-form) is a lambda list. (cddr t-form) is a progn body.
  (cond ((null (car frees))		; woopie.
	 (cond ((null arg-info)
		`($any . (m-tlambda ,@(cdr t-form))))
	       (t
		`($any . (m-tlambda& ,@(cdr t-form))))))
	((null (cadr frees))
	`($any . (,(cond ((null arg-info) 'm-tlambda&env)
			 (t               'm-tlambda&env&))
		   (,(cadr t-form) ,(car frees))
		   ,@(cddr t-form))))
	(t
	 (warn-meval form)
	 (side-effect-free-check (cadr frees) form)
	 `($any . (meval ',form)))))
