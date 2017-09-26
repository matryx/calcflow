;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Compilation environment for TRANSLATED MACSYMA code.        ;;;
;;;       (c) Copyright 1980 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module mdefun macro)

(load-macsyma-macros transm)

;;; DEFMTRFUN will be the new standard.
;;; It will punt macsyma fexprs since the macro scheme is now
;;; available. I have tried to generalize this enough to do
;;; macsyma macros also.

;;; (DEFMTRFUN-EXTERNAL ($FOO <mode> <property> <&restp>))

;;we don't make function type declarations yet.
(defmacro defmtrfun-external (&rest ig)
  (declare (ignore ig))
  nil)

;;; (DEFMTRFUN ($FOO <mode> <property> <&restp>) <ARGL> . BODY)
;;; If the MODE is numeric it should do something about the
;;; number declarations for compiling. Also, the information about the
;;; modes of the arguments should not be thrown away.

(defmacro defmtrfun  ((name mode prop restp . array-flag) argl . body )
  (let ((def-header))
    (and array-flag
	 ;; old DEFMTRFUN's might have this extra bit NIL
	 ;; new ones will have (NIL) or (T)
	 (setq array-flag (car array-flag)))
    (setq def-header
	  (cond ((eq prop 'mdefine)
		 (cond (array-flag `(:property ,name a-subr))
		       (t name)))
		(t `(,name translated-mmacro))))

    `(eval-when
      #+gcl (compile load eval)
      #-gcl (:compile-toplevel :load-toplevel :execute)

      ,@(and (not array-flag) `((remprop ',name 'translate)))
      ,@(and mode `((defprop ,name ,mode
		      ,(cond (array-flag 'arrayfun-mode)
			     (t 'function-mode)))))
      ,@(cond (array-flag
	       ;; when loading in hashed array properties
	       ;; most exist or be created. Other
	       ;; array properties must be consistent if
	       ;; they exist.
	       `((insure-array-props ',name ',mode ',(length argl)))))
      ,@(cond ((and (eq prop 'mdefine) (not array-flag))
	       `((cond ((status feature macsyma)
			(mputprop ',name t
				  ,(cond ((not restp)
					  ''$fixed_num_args_function)
					 (t
					  ''$variable_num_args_function)))))
		 ,(cond ((not restp) nil)))))
      (,(if (consp def-header)
	    'defun-prop
	    'defmfun)
       ,def-header ,(cond ((not restp) argl)
			  (t '|mlexpr NARGS|))
       ,@(cond ((not restp)
		body)
	       (t
		(let ((nl (1- (length argl))))
		  `((cond ((< |mlexpr NARGS| ,nl)
			   ($error 'maxima-error ',name ,(intl:gettext " takes no less than ")
			    ,nl
			    ',(cond ((= nl 1)
				     (intl:gettext " argument."))
				    (t
				     (intl:gettext " arguments.")))))
			  (t
			   ((lambda ,argl ,@body)
			    ;; this conses up the
			    ;; calls to ARGS and LISTIFY.
			    ,@(do ((j 1 (1+ j))
				   (p-argl nil))
				  ((> j nl)
				   (push
				    `(cons '(mlist) (listify (- ,nl |mlexpr NARGS|)))
				    p-argl)
				   (nreverse p-argl))
				  (push `(arg ,j) p-argl)))))))))))))
