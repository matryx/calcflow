;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gjc: 6:27pm  sunday, 20 july 1980
;;;       (c) copyright 1979 massachusetts institute of technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module trmode)

(defmvar $mode_checkp t "if true, modedeclare checks the modes of bound variables.")
(defmvar $mode_check_warnp t "if true, mode errors are described.")
(defmvar $mode_check_errorp nil "if true, modedeclare calls error.")

(defun mseemingly-unbound (x)
  (or (not (boundp x)) (eq (symbol-value x) x)))

(defmfun assign-mode-check (var value)
  (let ((mode (get var 'mode))
	(user-level ($get var '$value_check)))
    (when mode
      (let (($mode_check_warnp t)
	    ($mode_check_errorp t))
	(chekvalue var mode value)))
    (when user-level
      (mcall user-level value)))
  value)

(defvar defined_variables ())

(defvar $define_variable ())

(def%tr $define_variable (form)	;;VAR INIT MODE.
  (cond ((> (length form) 3)
	 (destructuring-let (((var val mode) (cdr form)))
			    (let ((mode-form `(($modedeclare) ,var ,mode)))
			      (translate mode-form)
			      (push-pre-transl-form
			       ;; POSSIBLE OVERKILL HERE
			       `(declare (special ,var)))
			      (push var defined_variables)
			      ;; Get rid of previous definitions put on by
			      ;; the translator.
			      (do ((l *pre-transl-forms* (cdr l)))
				  ((null l))
				;; REMOVE SOME OVERKILL
				(cond ((and (eq (caar l) 'def-mtrvar)
					    (eq (cadar l) var))
				       (setq *pre-transl-forms* (delete (car l) *pre-transl-forms* :test #'eq)))))
			      (if (not (eq mode '$any))
				  ;; so that the rest of the translation gronks this.
				  (putprop var 'assign-mode-check 'assign))
			      `($any . (eval-when
					   #+gcl (compile load eval)
					   #-gcl (:compile-toplevel :load-toplevel :execute)
					   (meval* ',mode-form)
					   ,(if (not (eq mode '$any))
						`(defprop ,var assign-mode-check assign))
					   (def-mtrvar ,(cadr form) ,(dtranslate (caddr form))))))))
	(t
	 (tr-format (intl:gettext "error: 'define_variable' must have 3 arguments; found: ~:M~%") form)
	 nil)))

;; the priority fails when a DEF-MTRVAR is done, then the user
;; sets the variable, because the set-priority stays the same.
;; This causes some Define_Variable's to over-ride the user setting,
;; but only in the case of re-loading, what we were worried about
;; is pre-setting of variables of autoloading files.

(defmspec $define_variable  (l)
  (setq l (cdr l))
  (unless (> (length l) 2)
    (merror (intl:gettext "define_variable: expected three arguments; found: ~M") `((mprogn) ,@l)))
  (unless (symbolp (car l))
    (merror (intl:gettext "define_variable: first argument must be a symbol; found: ~M") (car l)))
  (meval `(($modedeclare) ,(car l) ,(caddr l)))
  (unless (eq (caddr l) '$any)
    (putprop (car l) 'assign-mode-check 'assign))
  (if (mseemingly-unbound (car l))
      (meval `((msetq) ,(car l) ,(cadr l)))
      (meval (car l))))


(defmspec $mode_identity (l)
  (setq l (cdr l))
  (unless (= (length l) 2)
    (merror (intl:gettext "mode_identity: expected two arguments; found: ~M") `((mprogn) ,@l)))
  (let* ((obj (cadr l))
	 (v (meval obj)))
    (chekvalue obj (ir-or-extend (car l)) v)
    v))

(def%tr $mode_identity (form)
  `(,(ir-or-extend (cadr form)) . ,(dtranslate (caddr form))))

(defun ir-or-extend (x)
  (let ((built-in-type (case x
			 (($float $real $floatp $flonum $floatnum) '$float)
			 (($fixp $fixnum $integer) '$fixnum)
			 (($rational $rat) '$rational)
			 (($number $bignum $big) '$number)
			 (($boolean $bool) '$boolean)
			 (($list $listp) '$list)
			 ($complex '$complex)
			 (($any $none $any_check) '$any))))
    (if built-in-type built-in-type
	(prog1
	    x
	  (mtell (intl:gettext "modedeclare: ~M is not a built-in type; assuming it is a Maxima extension type.") x)))))

(def%tr $modedeclare (form)
  (do ((l (cdr form) (cddr l)))
      ((null l))
    (declmode (car l) (ir-or-extend (cadr l)) t)))

(defmfun ass-eq-ref (table key &optional dflt)
  (let ((val (assoc key table :test #'eq)))
    (if val
	(cdr val)
	dflt)))

(defmfun ass-eq-set (val table key)
  (let ((cell (assoc key table :test #'eq)))
    (if cell
	(setf (cdr cell) val)
	(push (cons key val) table)))
  table)


;;; Possible calls to MODEDECLARE.
;;; MODEDECLARE(<oblist>,<mode>,<oblist>,<mode>,...)
;;; where <oblist> is:
;;; an ATOM, signifying a VARIABLE.
;;; a LIST, giving a list of objects of <mode>
;;;

(defmspec $modedeclare (x)
  (setq x (cdr x))
  (when (oddp (length x))
    (merror (intl:gettext "mode_declare: expected an even number of arguments; found: ~M") `((mprogn) ,@x)))
  (do ((l x (cddr l)) (nl))
      ((null l) (cons '(mlist) (nreverse nl)))
    (declmode (car l) (ir-or-extend (cadr l)) nil)
    (push (car l) nl)))

(defun tr-declare-varmode (variable mode)
  (declvalue variable (ir-or-extend mode) t))

;;; If TRFLAG is TRUE, we are in the translator, if NIL, we are in the
;;; interpreter.
(declare-top (special trflag mode form))

(defun declmode (form mode trflag)
  (cond ((atom form)
	 (declvalue form mode trflag)
	 (and (not trflag) $mode_checkp (chekvalue form mode)))
	((eq 'mlist (caar form))
	 (mapc #'(lambda (l) (declmode l mode trflag)) (cdr form)))
	((member 'array (cdar form) :test #'eq)
	 (declarray (caar form) mode))
	((eq '$function (caar form))
	 (mapc
       #'(lambda (l)
           (if (stringp l) (setq l ($verbify l)))
           (declfun l mode))
       (cdr form)))
	((member (caar form) '($fixed_num_args_function $variable_num_args_function) :test #'eq)
	 (mapc
       #'(lambda (f)
           (if (stringp f) (setq f ($verbify f)))
           (declfun f mode)
           (mputprop f t (caar form)))
       (cdr form)))
	((eq '$completearray (caar form))
	 (mapc #'(lambda (l)
		   (putprop (if (atom l) l (caar l)) mode 'array-mode))
	       (cdr form)))
	((eq '$array (caar form))
	 (mapc #'(lambda (l) (mputprop l mode 'array-mode)) (cdr form)))
	((eq '$arrayfun (caar form))
	 (mapc #'(lambda (l) (mputprop l mode 'arrayfun-mode)) (cdr form)))
	(t
	 (declfun (caar form) mode))))

(declare-top (unspecial trflag mode form))

(defun declvalue (v mode trflag)
  (when trflag (setq v (teval v)))
  (add2lnc v $props)
  (putprop v mode 'mode))

(defun chekvalue (my-v mode &optional (val (meval1 my-v) val-givenp))
  (cond ((or val-givenp (not (eq my-v val)))
	 ;; hack because macsyma PROG binds variable to itself. 
	 (let ((checker (assoc mode `(($float . floatp)
				      ($fixnum . integerp)
				      ($number . numberp)
				      ($list . $listp)
				      ($boolean . ,#'(lambda (u) (member u '(t nil) :test #'eq))))
			       :test #'eq))
	       (nchecker (assoc mode '(($float . $real)
				       ($fixnum . $integer)
				       ($complex . $complex))
				:test #'eq))
	       (not-done t))
	   (if (cond ((and checker (not (funcall (cdr checker) val))
			   (if nchecker
			       (prog1
				   (not (mfuncall '$featurep val (cdr nchecker)))
				 (setq not-done nil))
			       t)))
		     ((if not-done (and nchecker (not (mfuncall '$featurep val (cdr nchecker)))))))
	       (signal-mode-error my-v mode val))))))

(defun signal-mode-error (object mode value)
  (cond ((and $mode_check_warnp (not $mode_check_errorp))
	 (mtell (intl:gettext "translator: ~:M was declared with mode ~:M, but it has value: ~M") object mode value))
	($mode_check_errorp
	 (merror (intl:gettext "translator: ~:M was declared with mode ~:M, but it has value: ~M") object mode value))))
			  
(defun put-mode (name mode type)
  (if (get name 'tbind)
      (setf (get name 'val-modes) (ass-eq-set mode (get name 'val-modes) type))
      (setf (get name type) mode)))

(defun declarray (ar mode)
  (put-mode ar mode 'array-mode))

(defun declfun (f mode)
  (put-mode f mode 'function-mode))

;;; 1/2 is not $RATIONAL. bad name. it means CRE form.

(defun udm-err (mode)
  (mtell (intl:gettext "translator: no such mode declaration: ~:M~%") mode))
