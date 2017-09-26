;;; -*- Mode: LISP; Syntax: Common-lisp; Package: CLIMAX; Base: 10 -*-
;;;>******************************************************************************************
;;;>   Software developed by Bruce R. Miller (miller@cam.nist.gov)
;;;>   using Symbolics Common Lisp (system 425.111, ivory revision 4)
;;;>   at NIST - Computing and Applied Mathematics Laboratory
;;;>   a part of the U.S. Government; it is therefore not subject to copyright.
;;;>******************************************************************************************
;;;;******************************************************************************************
;;;; FORMAT: Package for restructuring expressions in Macsyma
;;;;******************************************************************************************

(in-package :maxima)

(defmacro mlist* (arg1 &rest more-args)
  `(list* '(mlist simp) ,arg1 ,@more-args))

(defun mrelationp (expr)
  (and (listp expr)
       (member (caar expr) '(mequal mnotequal mgreaterp mlessp mgeqp mleqp))))

;;;;******************************************************************************************
;;; format(expr,template,...)
;;; Formats EXPR according to the TEMPLATEs given:

(defvar *template* nil "The current template")
(defvar *templates* nil "The current template chain")
(defvar *subtemplates* nil "Current template's subtemplates")

(defun $format (expr &rest templates) (format-from-chain expr templates))

;; format according to chain in *templates*
(defun format-from-chain (expr &optional (*templates* *templates*))
  (if (null *templates*) expr
      (format-one expr (pop *templates*))))

;; format according to tmp, then pieces according to *templates*
(defun format-one (expr tmp)
  (multiple-value-bind (*template* formatter parms *subtemplates*)(parse-template tmp)
    (cond (formatter (apply #'mfuncall formatter expr parms))
	  ((or (symbolp tmp)			; Apply SPEC as function, if it CAN be
	       (and (listp tmp)(or (eq (caar tmp) 'lambda)(member 'array (cdar tmp)))))
	   (format-from-chain (let ((*templates* nil)) (mfuncall tmp expr))))
	  (t (merror "FORMAT: template ~M must be a template or function." tmp)))))

;;; Format a `piece' of an expression, accounting for any current subtemplates.
;;; If NTH is given, use NTH subtemplate for this piece, else use next subtemplate.
;;; Account for %DITTO'd templates.
(defun $format_piece (piece &optional nth)
  (flet ((dittop (ptrn)				; If %ditto form, return repeated template
	   (and (listp ptrn)(eq (caar ptrn) '$%ditto) (cadr ptrn))))
    (let ((subtmp (cond ((null *subtemplates*) nil)	; no piecewise templates.
			((null nth)(or (dittop (car *subtemplates*))	; next one %ditto's
				       (pop *subtemplates*)))	; otherwise, remove next one.
			((setq nth (nth (1- nth) *subtemplates*))	; nth subtemplate?
			 (or (dittop nth) nth))	; strip off possible %ditto
			((dittop (car (last *subtemplates*)))))))	; last dittos, reuse it
      (if subtmp (format-one piece subtmp)(format-from-chain piece)))))

;; Format expr according to remaining chain, but disallowing subtemplates.
(defun format-w/o-subtemplates (expr)
  (when *subtemplates*
    (merror "FORMAT: Template ~M was given subtemplates ~M" *template*
	    (mlist* *subtemplates*)))
  (format-from-chain expr))

;;; given a candidate format template, return:
;;; template name, formatter function, parameters (if any) and subtemplates (if any),
(defun parse-template (template)
  (let (op name formatter)
    (flet ((getform (symbol)
	     (and (setq formatter (or ($get symbol '$formatter)(get symbol 'formatter)))
		  (setq name symbol))))
      (cond (($numberp template) nil)
	    ((atom template) (values (getform template) formatter nil nil))
	    ((eq (caar template) 'mqapply)	; Template w/ subtemplates AND parms
	     (when (and (listp (setq op (cadr template)))
			(getform (caar op))
			(not (member 'array (cdar op))))	; but not T[...][...]
	       (values name formatter (cdr op) (cddr template))))
	    ((getform (caar template))		; Template w/ parameters OR subtemplates
	     (if (member 'array (cdar template))
		 (values name formatter nil (cdr template))
		 (values name formatter (cdr template) nil)))))))

;;;;******************************************************************************************
;;; Defining format commands.
;;; (user defined ones go on the macsyma property list)

(defmacro def-formatter (names parms &body body)
  (let* ((names (if (listp names) names (list names)))
	 (fmtr (if (atom parms) parms
		   (make-symbol (concatenate 'string (string (car names))
					     (symbol-name '#:-formatter))))))
    `(progn
       ,(unless (atom parms) `(defun ,fmtr ,parms ,@body))
       ,@(mapcar #'(lambda (name) `(setf (get ',name 'formatter) ',fmtr)) names))))

;;;;******************************************************************************************
;;; Subtemplate aids.

(def-formatter mlist (expr &rest elements)	; merge elements w/ following chain.
  (format-from-chain expr (append elements *templates*)))

(def-formatter $%preformat (expr &rest templates)	; preformat using template chain
  (format-w/o-subtemplates (format-from-chain expr templates)))

(def-formatter $%noop format-w/o-subtemplates)	; subtemplate filler.

;;;;******************************************************************************************
;;; Arithmetic template: eg.  a*%p(x)-b

(defun template-p (expr)			; is there a template in expr?
  (if (and (listp expr)(member (caar expr) '(mplus mtimes mexpt)))
      (some #'template-p (cdr expr))		; for arithmetic, find a `real' format in args
      (parse-template expr)))

(defun partition-arithmetic-template (op args)
  ;; Find the 1 (!) term or factor with a regular template in it.
  (let ((pat (remove-if-not #'template-p args)))	; find arg with template in it
    (when (or (null pat)(cdr pat))
      (merror "FORMAT: Pattern must contain exactly 1 template ~M" (cons (list op) args)))
    (values (car pat) (simplify (cons (list op) (remove (car pat) args))))))

(def-formatter mplus (expr &rest terms)
  (multiple-value-bind (template rest)(partition-arithmetic-template 'mplus terms)
    (add (format-one (sub expr rest) template) rest)))

(def-formatter mtimes (expr &rest factors)
  (multiple-value-bind (template rest)(partition-arithmetic-template 'mtimes factors)
    (mul (format-one (div expr rest) template) rest)))

(def-formatter mexpt (expr b p)			; b^p
  (cond ((template-p b)(power (format-one (power expr (inv p)) b) p))
	((template-p p)(power b (format-one (div ($log expr)($log b)) p)))
	((merror "FORMAT: Pattern must contain exactly 1 template ~M" (power b p)))))

;;;;******************************************************************************************
;;; Control templates

;;; IF ... ELSEIF ... ELSE
(def-formatter $%if (expr &rest predicates)
  ($format_piece expr (do ((ps predicates (cdr ps))
			   (i 1 (1+ i)))
			  ((or (null ps)(is-boole-check (mfuncall (car ps) expr))) i))))

(def-formatter ($%expr $%expression)(expr)	; format arguments/operands
  (when ($atom expr)
    (merror "FORMAT %EXPR: ~M doesn't have parts" expr))
  (map1 #'$format_piece expr))

;;; Convenience templates
(def-formatter $%subst (expr &rest listofeqns)
  (format-w/o-subtemplates ($substitute (mlist* listofeqns) expr)))

(def-formatter $%ratsubst (expr &rest listofeqns)
  (autoldchk '$lratsubst)
  (format-w/o-subtemplates ($lratsubst (mlist* listofeqns) expr)))

;;;;******************************************************************************************
;;; `Bag' & Relation templates.

;;; This function tries to get OPER at the top level of EXPR.
;;; OPER must be a BAG or RELATION, as must the top layers of EXPR
;;; (down to wherever OPER is found).
;;; The interpretation is that a list of equations is equivalent to an equation
;;; whose rhs & lhs are lists.  (and ditto for all permutations).
(defun $coerce_bag (oper expr)
  (unless (or (mbagp expr)(mrelationp expr))
    (merror "Error: ~M is not a relation, list or array: can't be made into an ~M" expr oper))
  (setq oper (getopr oper))
  (flet ((swap (op x)
	   (cons (list op)
		 (mapcar #'(lambda (l)(simplify (cons (car x) l)))
			 (transpose (mapcar #'(lambda (y)(cdr ($coerce_bag op y)))(cdr x)))))))
    (cond ((eq (caar expr) oper) expr)		; oper is already at top level.
	  ((eq (caar expr) '$matrix)		; swap levels 2 & 3 (mlist & oper), then 1&2
	   (swap oper (map1 #'(lambda (x)(swap oper x)) expr)))
	  ((eq oper '$matrix)			; swap level 1 & 2 (oper & matrix), then 2 & 3.
	   (map1 #'(lambda (l)(swap 'mlist l))(swap oper expr)))
	  (t (swap oper expr)))))		; swap 1st & 2nd levels.

(defun format-bag (expr op)
  (map1 #'$format_piece ($coerce_bag op expr)))

(def-formatter ($%eq $%equation $%rel $%relation) (r &optional (op 'mequal))
  (format-bag r op))
(def-formatter $%list (expr)  (format-bag expr 'mlist))
(def-formatter $%matrix (expr)(format-bag expr '$matrix))
;;; Note: %matrix subtemplates apply to ROWS. To target elements, use %list for rows.

;;;;******************************************************************************************
;;; Targetting templates.
;;; mostly shorthand for things which can be done using subtemplates, but more concise.

(defun format-nth (expr n)
  (unless (and ($integerp n) (plusp n)(< n (length expr)))
    (merror "FORMAT ~M: ~M doesn't have an argument #~M" *template* expr n))
  (let ((new (copy-list expr)))
    (setf (nth n new)(format-w/o-subtemplates (nth n expr)))
    (simplify new)))

(def-formatter $%arg format-nth)
(def-formatter $%lhs (expr &optional (op 'mequal))
  (format-nth ($coerce_bag op expr) 1))
(def-formatter $%rhs (expr &optional (op 'mequal))
  (format-nth ($coerce_bag op expr) 2))

(def-formatter ($%el $%element)(expr &rest indices)
  (let ((array ($copymatrix ($coerce_bag '$matrix expr))))
    (apply #'marrayset ($format_piece (apply #'marrayref array indices)) array indices)
    array))

(def-formatter $%num (frac)
  (div (format-w/o-subtemplates ($num frac))($denom frac)))
(def-formatter $%denom (frac)
  (div ($num frac)(format-w/o-subtemplates ($denom frac))))

(def-formatter $%match (expr predicate)
  (labels ((rec (xpr)
	     (cond ((is-boole-check (mfuncall predicate xpr))(format-w/o-subtemplates xpr))
		   ((atom xpr) xpr)
		   (t (recur-apply #'rec xpr)))))
    (rec expr)))

;; Actually, more like bothcoeff
(def-formatter $%coeff (expr var &optional (n 1))
  (when (and (listp var)(eq (caar var) 'mexpt))
    (setq var (cadr var) n (mul n (caddr var))))
  (let ((coefs ($coeffs expr var)))
    (add (mul ($format_piece ($get_coef coefs n)) (power (car (cddadr coefs)) n))
	 ($format_piece ($uncoef (delete n coefs :test #'alike1 :key #'caddr))))))

;;;;******************************************************************************************
;;; Polynomial, Trig & Series templates.

(defun format-clist (clist &optional (function #'$format_piece))
  (flet ((mp1 (l)(mapcar #'(lambda (p)(mlist* (funcall function (cadr p)) (cddr p))) l)))
    ($uncoef (mlist* (cadr clist)
		     (case (cadadr clist)
		       (($%poly $%series $%taylor) (mp1 (cddr clist)))
		       ($%trig (mapcar #'(lambda (l)(mlist* (mp1 (cdr l))))(cddr clist))))))))

;; %POLY(vars,...) : express EXPR as a polynomial in VARS, format the coeffs.
(def-formatter ($%poly $%p) (expr &rest vars)
  (autoldchk '$coeffs)
  (format-clist (apply #'$coeffs expr vars)))

;; %MONICPOLY : format leading coeff, then poly/lc.
(def-formatter ($%monicpoly $%mp) (expr &rest vars)
  (autoldchk '$coeffs)
  (let* ((cl (apply #'$coeffs expr vars))
	 (c0 (cadar (last cl))))
    (mul ($format_piece c0)(format-clist cl #'(lambda (c)($format_piece (div c c0)))))))

;; %TRIG(vars,...): express EXPR as trig. series in VARS, format the coeffs.
(def-formatter ($%trig $%t) (expr &rest vars)
  (autoldchk '$trig_coeffs)
  (format-clist (apply #'$trig_coeffs expr vars)))

;; %SERIES(var,order), %TAYLOR(var,order): expand EXPR as series in VAR to order ORDER,
;; formats the coeffs.  %SERIES only expands arithmetic expressions.
(def-formatter ($%series $%s) (expr var order)
  (autoldchk '$series_coeffs)
  (format-clist ($series_coeffs expr var order)))

(def-formatter $%taylor (expr var order)
  (autoldchk '$taylor_coeffs)
  (format-clist ($taylor_coeffs expr var order)))

;;;;******************************************************************************************
;;; Sums

(defun format-sum (sum)
  (cond ((atom sum)($format_piece sum))
	((specrepp sum) (format-sum (specdisrep sum)))
	((eq (caar sum) 'mplus)(simplify (map1 #'format-sum sum)))
	((eq (caar sum) '%sum) (cons (car sum) (cons ($format_piece (cadr sum))(cddr sum))))
	(t ($format_piece sum))))

(def-formatter $%sum format-sum)
(def-formatter ($%partfrac $%pf)(expr var)
  (format-sum ($partfrac expr var)))

;;;;******************************************************************************************
;;; Products

(defun format-product (prod)
  (cond ((atom prod) ($format_piece prod))
	((specrepp prod) (format-product (specdisrep prod)))
	(t (case (caar prod)
	     (mtimes (simplify (map1 #'format-product prod)))
	     (mexpt (power (format-product (second prod))(third prod)))
	     (%product (cons (car prod)(cons ($format_piece (cadr prod))(cddr prod))))
	     (t ($format_piece prod))))))

(def-formatter ($%product $%prod) format-product)
(def-formatter ($%sqfr $%sf)(expr)
  (format-product ($sqfr expr)))
(def-formatter ($%factor $%f) (expr &optional minpoly)
  (format-product (cond (($numberp expr) expr)
			(minpoly ($factor expr minpoly))
			(t ($factor expr)))))

;;;;******************************************************************************************
;;; Fractions

(defun format-fraction (frac)
  (div ($format_piece ($num frac))
       ($format_piece ($denom frac))))

(def-formatter $%frac format-fraction)
(def-formatter ($%ratsimp $%r) (expr)
  (format-fraction ($ratsimp expr)))

;;;;******************************************************************************************
;;; Complex number templates.

;; Express EXPR = A+%I*B; format A & B.
(def-formatter ($%rectform $%g) (expr)
  (let ((pair (trisplit expr)))
    (add ($format_piece (car pair))
	 (mul ($format_piece (cdr pair)) '$%i))))

;; Express EXPR = R*exp(%I*P); format R & P.
(def-formatter $%polarform (expr)
  (let ((pair (absarg expr)))
    (mul ($format_piece (car pair))
	 (power '$%e (mul '$%i ($format_piece (cdr pair)))))))

;;;********************************************************************************
;;; Examples of user defined templates:
;;; format_piece automatically handles the piecewise templates & remaining templates.
#||
put(%myrectform,
     lambda([expr],
       block([pair:rectformlist(expr)],
	  format_piece(pair[1]) +%I* format_piece(pair[2]))),
     formatter)$

put(%myif,
     lambda([expr,test],
	if test(expr) then format_piece(expr,1)
	else               format_piece(expr,2)),
     formatter)$

put(%part, /* Note workaround for substpart (a special form!) */
     lambda([expr,[spec]],
       apply(substpart,cons(format_piece(apply(part,cons(expr,spec))),cons(expr,spec)))),
    formatter)$
||#
