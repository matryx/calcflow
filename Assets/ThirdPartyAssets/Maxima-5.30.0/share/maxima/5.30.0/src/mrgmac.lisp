;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module mrgmac macro)

(defun zl-get (sym tag)
  (cond ((symbolp sym) (get sym tag))
	((consp sym) (getf (cdr sym) tag))))

(defun define-macro (name lambda-exp)
  (cond ((symbolp lambda-exp)
	 (setq lambda-exp (symbol-function lambda-exp))))
  (setf (macro-function name) lambda-exp))

(declare-top (special name bas selector))

(defvar *mobjects* nil)

(defprop mode (c-mode s-mode a-mode) mode)

(defmacro c-mode (&rest l)
  `(list ,@l))

(defmacro s-mode (&rest x)
  (push 's-mode x)
  (cond ((eq 'c (third x)) `(first ,(second x)))
	((eq 'sel (third x)) `(second ,(second x)))
	((eq 'push+sto (third x)) `(third ,(second x)))))

(defmacro a-mode (&rest x)
  (push 'a-mode x)
  (cond ((eq 'c (third x)) `(rplaca (second x) ,(fourth x)))
	((eq 'sel (third x)) `(rplaca (cdr ,(second x)) ,(fourth x)))
	((eq 'push+sto (third x)) `(rplaca (cddr ,(second x)) ,(fourth x)))))

(defmacro defmode (&rest x)
  (push 'defmode x)
  (let ((selector (member 'selector (cddddr x) :test #'eq)))
    (define-mode (second x) (fourth x))
    (mapc 'eval (cddddr x))
    `',(second x)))

(defun define-mode (name desc)
  (let ((c (intern (format nil "C-~A" name)))
	(s (intern (format nil "S-~A" name)))
	(a (intern (format nil "A-~A" name))))
    (define-macro c (defc desc))
    (define-macro s (defs desc))
    (define-macro a (defa desc))
    (putprop name (c-mode c s a) 'mode)))

(defun defc (desc)
  (let ((bas 'x))
    (coerce `(lambda (x &optional env)
	      (declare (ignore env))
	      ,(defc1 desc))
	    'function)))

(defun defc1 (desc)
  (cond ((atom desc) (list 'quote desc))
	((eq 'selector (car desc))
	 (cond ((not (null (cdddr desc))) (list 'quote (fourth desc)))
	       (t (setq bas (list 'cdr bas))
		  (list 'car bas))))
	((eq (quote atom) (car desc))
	 `(list 'c-atom '',(mapcar 'second (cdr desc)) (cons (quote list) (cdr x))))
	((eq 'cons (car desc)) `(list 'cons ,(defc1 (second desc)) ,(defc1 (third desc))))
	((eq (quote list) (car desc))
	 (do ((l (cdr desc) (cdr l)) (nl))
	     ((null l) `(list (quote list) ,@(nreverse nl)))
	   (push (defc1 (car l)) nl)))
	((eq 'struct (car desc)) (defc1 (cons (quote list) (cdr desc))))
	(t (list 'quote desc))))


(defun defs (desc)
  (coerce `(lambda (x &optional env)
	    (declare (ignore env))
	    (cond ,@(nreverse (defs1 desc '(second x) nil))))
	  'function ))

(defun defs1 (desc bas result)
  (cond ((atom desc) result)
	((eq 'selector (car desc))
	 (putprop (second desc)
	      (cons (cons name (third desc)) (zl-get (second desc) 'modes))
	      'modes)
	 (putprop name
	      (cons (cons (second desc) (third desc)) (zl-get name 'sels))
	      'sels)
	 (if selector (define-macro (second desc) 'selector))
	 (cons `((eq ',(second desc) (third x)) ,bas) result))
	((eq (quote atom) (car desc))
	 (do ((l (cdr desc) (cdr l))) ((null l))
	   (putprop (cadar l) (cons (cons name (caddar l))
				(zl-get (cadar l) 'modes)) 'modes)
	   (putprop name (cons (cons (cadar l) (caddar l))
			   (zl-get name 'sels)) 'sels)
	   (if selector (define-macro (cadar l) 'selector)))
	 (cons `((member (third x) ',(mapcar 'second (cdr desc)) :test #'eq)
		 (list 'zl-get ,bas (list 'quote (third x))))
	       result))
	((eq 'cons (car desc))
	 (setq result (defs1 (second desc) `(list 'car ,bas) result))
	 (defs1 (third desc) `(list 'cdr ,bas) result))
	((eq (quote list) (car desc))
	 (do ((l (cdr desc) (cdr l))) ((null l))
	   (setq result (defs1 (car l) `(list 'car ,bas) result)
		 bas `(list 'cdr ,bas)))
	 result)
	((eq 'struct (car desc)) (defs1 (cons (quote list) (cdr desc)) bas result))
	(t result)))

(defun defa (desc)
  (coerce `(lambda (x &optional env)
	    (declare (ignore env))
	    (cond ,@(nreverse (defa1 desc '(second x) nil nil))))
	  'function))

(defun defa1 (desc bas cdr result)
  (cond ((atom desc) result)
	((eq 'selector (car desc))
	 (setq bas (cond ((not cdr) `(list 'car (list 'rplaca ,(third bas) (fourth x))))
			 (t `(list 'cdr (list 'rplacd ,(third bas) (fourth x))))))
	 (cons `((eq ',(second desc) (third x)) ,bas) result))
	((eq  (quote atom) (car desc))
	 (list `(t (list 'cput (second x) (fourth x) (list 'quote (third x))))))
	((eq 'cons (car desc))
	 (setq result (defa1 (second desc) `(list 'car ,bas) nil result))
	 (defa1 (third desc) `(list 'cdr ,bas) t result))
	((eq (quote list) (car desc))
	 (do ((l (cdr desc) (cdr l))) ((null l))
	   (setq result (defa1 (car l) `(list 'car ,bas) nil result)
		 bas `(list 'cdr ,bas)))
	 result)
	((eq 'struct (car desc)) (defa1 (cons (quote list) (cdr desc)) bas cdr result))
	(t result)))


(defun mode (x)
  (cdr (assoc x *mobjects* :test #'equal)))

(defmacro modedeclare (&rest l)
  `(modeclare-internal ',l))

(defun modedeclare-internal (x)
  (mapc #'(lambda (l)
	    (mapc #'(lambda (v) (push (cons v (car l)) *mobjects*))
		  (cdr l)))
	x))

(defmacro sel (&rest x)
  (push 'sel x )
  (let ((s (fsel (mode (second x)) (cddr x))))
    (cond ((null s) (merror "SEL: ~a ~a" (second x) (cddr x)))
	  (t (setq x (second x))
	     (do ()
		 ((null (cdr s)) x)
	       (setq x (cons (second (zl-get (car s) 'mode)) (rplaca s x))
		     s (cddr s))
	       (rplacd (cddr x) nil))))))

(defun fsel (m sels) ;;This has a bug in it.
  (cond ((null sels) (list m))
	((null m)
	 (do ((l (zl-get (car sels) 'modes) (cdr l)))
	     ((null l))
	   (if (setq m (fsel (cdar l) (cdr sels)))
	       (return (cons (caar l) (cons (car sels) m))))))
	((let (dum)
	   (if (setq dum (assoc (car sels) (zl-get m 'sels) :test #'eq))
	       (cons m (cons (car sels) (fsel (cdr dum) (cdr sels)))))))
	(t (do ((l (zl-get m 'sels) (cdr l)) (dum))
	       ((null l))
	     (if (setq dum (fsel (cdar l) sels))
		 (return (cons m (cons (caar l) dum))))))))

(defun selector (x env)
  (declare (ignore env))
  (if (null (cddr x))
      `(sel ,(second x) ,(car x))
      `(push+sto (sel ,(second x) ,(car x)) ,(third x))))

(defmacro push+sto (&rest x)
  (push 'push+sto x )
  `(sto ,@(cdr x)))

(defmacro sto (&rest x)
  (push ' sto x )
  (do ((l (cdr x) (cddr l))
       (s)
       (nl))
      ((null l) `(progn ,@(nreverse nl)))
    (cond ((atom (car l))
	   (push `(setq ,(first l) ,(second l)) nl))
	  ((and (eq 'sel (caar l)) (setq s (fsel (mode (cadar l)) (cddar l))))
	   (setq x (cadar l))
	   (do ((l (cddr s) (cddr l)))
	       ((null (cdr l)))
	     (setq x (cons (second (zl-get (first l) 'mode)) (rplaca l x)))
	     (rplacd (cddr x) nil))
	   (push (list (third (zl-get (first s) 'mode)) x (second s) (second l)) nl))
	  (t (merror "STO: ~a" (car l))))))

(defmacro cons-exp (op &rest args)
  `(simplify (list (list ,op) ,@args)))

;; Local Modes:
;; Mode: LISP
;; Comment Col: 40
;; End:
