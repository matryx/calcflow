;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module merror)

;;; Macsyma error signalling. 
;;; 2:08pm  Tuesday, 30 June 1981 George Carrette.

(defmvar $error `((mlist simp) "No error.")
  "During an MAXIMA-ERROR break this is bound to a list
  of the arguments to the call to MAXIMA-ERROR, with the message
  text in a compact format.")

(defmvar $errormsg 't
  "If `false' then no maxima-error message is printed!")

(defmfun $error (&rest l)
  "Signals a Maxima user error."
  (apply #'merror (fstringc l)))

(defmvar $error_size 60.
  "Expressions greater in SOME size measure over this value
  are replaced by symbols {ERREXP1, ERREXP2,...} in the MAXIMA-ERROR
  display, the symbols being set to the expressions, so that one can
  look at them with expression editing tools. The default value of
  this variable may be determined by factors of terminal speed and type.")

(defun error-size (exp)
  ; RATDISREP the argument in case it's a CRE. Ugh.
  ; But RATDISREP simplifies its argument, which is a no-no if we got here
  ; because some simplification code is complaining, so inhibit simplification. Double ugh.
  (let (($simp nil))
    (declare (special $simp))
    (setq exp (ratdisrep exp)))

  (if (atom exp)
      0
      (do ((l (cdr exp) (cdr l))
	   (n 1 (1+ (+ n (error-size (car l))))))
	  ((or (atom l)
	       ;; no need to go any further, and this will save us
	       ;; from circular structures. (Which the display
	       ;; package would have a hell of a time with too.)
	       (> n $error_size))
	   n))))

;;; Problem: Most macsyma users do not take advantage of break-points
;;; for debugging. Therefore they need to have the error variables
;;; SET (as the old ERREXP was), and not PROGV bound. The problem with
;;; this is that recursive errors will bash the old value of the
;;; error variables. However, since we do bind the value of the
;;; variable $ERROR, calling the function $ERRORMSG will always
;;; set things back. It would be better to bind these variables,
;;; for, amoung other things, then the values could get garbage 
;;; collected.

(define-condition maxima-$error (error)
  ((message :initform $error :reader the-$error))
  (:documentation "Muser error, to be signalled by MERROR, usually.")
  (:report (lambda (c stream)
	     (declare (ignore c))
	     (let ((*standard-output* stream))
	       ($errormsg)))))

(defun merror (sstring &rest l)
  (declare (special errcatch *mdebug*))
  (setq $error `((mlist) ,sstring ,@ l))
  (and $errormsg ($errormsg))
  (cond (*mdebug*
	 (let ((dispflag t) ret)
	   (declare (special dispflag))
	   (format t (intl:gettext " -- an error.  Entering the Maxima debugger.~%~
                       Enter ':h' for help.~%"))
	   (progn
	     (setq ret (break-dbm-loop nil))
	     (cond ((eql ret :resume)
		    (break-quit))))))
	(errcatch  (error 'maxima-$error))
	(t
	 (fresh-line *standard-output*)
	 ($backtrace 3)
	 (format t (intl:gettext "~& -- an error. To debug this try: debugmode(true);~%"))
	 (throw 'macsyma-quit 'maxima-error))))

(defmacro with-$error (&body body)
  "Let MERROR signal a MAXIMA-$ERROR condition."
  `(let ((errcatch t)
	 *mdebug*		       ;let merror signal a lisp error
	 $errormsg)			;don't print $error
     (declare (special errcatch *mdebug* $errormsg))
     ,@body))

;; Sample:
;; (defun h (he)
;;   (merror "hi there ~:M and ~:M" he he))
;; This will signal a MAXIMA-$ERROR condition:
;; (with-$error (h '$you))

(defmvar $error_syms '((mlist) $errexp1 $errexp2 $errexp3)
  "Symbols to bind the too-large `maxima-error' expresssions to")

(defun-prop ($error_syms assign) (var val)
  (if (not (and ($listp val)
		(do ((l (cdr val) (cdr l)))
		    ((null l) (return t))
		  (if (not (symbolp (car l))) (return nil)))))
      (merror (intl:gettext "assignment: assignment to ~M must be a list of symbols; found: ~M")
	      var val)))

(defun process-error-argl (l)
  ;; This returns things so that we could set or bind.
  (do ((error-symbols nil)
       (error-values nil)
       (new-argl nil)
       (symbol-number 0))
      ((null l)
       (list (nreverse error-symbols)
	     (nreverse error-values)
	     (nreverse new-argl)))
    (let ((form (pop l)))
      (cond ((> (error-size form) $error_size)
	     (incf symbol-number)
	     (let ((sym (nthcdr symbol-number $error_syms)))
	       (cond (sym
		      (setq sym (car sym)))
		     (t
		      (setq sym (intern (format nil "~A~D" '$errexp
						symbol-number)))
		      (tuchus $error_syms sym)))
	       (push sym error-symbols)
	       (push form error-values)
	       (push sym new-argl)))
	    (t
	     (push form new-argl))))))

(defmfun $errormsg ()
  "errormsg() redisplays the maxima-error message while in a `maxima-error' break."
  ;; Don't optimize out call to PROCESS-ERROR-ARGL in case of
  ;; multiple calls to $ERRORMSG, because the user may have changed
  ;; the values of the special variables controling its behavior.
  ;; The real expense here is when MFORMAT calls the DISPLA package.
  (let ((the-jig (process-error-argl (cddr $error))))
    (mapc #'set (car the-jig) (cadr the-jig))
    (fresh-line)
    (let ((errset nil))
      (if (null (errset
		 (apply #'mformat nil
			(cadr $error) (caddr the-jig))))
	  (mtell (intl:gettext "~%** error while printing error message **~%~A~%")
		 (cadr $error)
		 )))
    (fresh-line))
  '$done)

(defmfun read-only-assign (var val)
  (if munbindp
      'munbindp
      (merror (intl:gettext "assignment: attempting to assign read-only variable ~:M the value ~M") var val)))


(defprop $error read-only-assign  assign)

;; THIS THROWS TO  (CATCH 'RATERR ...), WHEN A PROGRAM ANTICIPATES
;; AN ERROR (E.G. ZERO-DIVIDE) BY SETTING UP A CATCH  AND SETTING
;; ERRRJFFLAG TO T.  Someday this will be replaced with SIGNAL.
;; Such skill with procedure names!  I'd love to see how he'd do with
;; city streets.

;;; N.B. I think the above comment is by CWH, this function used
;;; to be in RAT;RAT3A. Its not a bad try really, one of the better
;;; in macsyma. Once all functions of this type are rounded up
;;; I'll see about implementing signaling. -GJC

(defmfun errrjf (&rest args)
  (if errrjfflag
      (throw 'raterr nil)
      (apply #'merror args)))

;;; The user-error function is called on "strings" and expressions.
;;; Cons up a format string so that $ERROR can be bound.
;;; This might also be done at code translation time.
;;; This is a bit crude.

(defmfun fstringc (l)
  (do ((sl nil) (s) (sb)
       (se nil))
      ((null l)
       (setq sl (maknam sl))
       (cons sl (nreverse se)))
    (setq s (pop l))
    (cond ((stringp s)
	   (setq sb (mapcan #'(lambda (x)
				(if (char= x #\~)
				    (list x x)
				    (list x)))
			    (coerce s 'list))))
	  (t
	   (push s se)
	   (setq sb (list #\~ #\M))))
    (setq sl (nconc sl sb (if (null l) nil (list #\space))))))
