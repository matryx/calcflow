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

(macsyma-module defcal macro)

;; Compile-time support for defining things which dispatch
;; off the property list. The Macsyma parser uses this.

(defun make-parser-fun-def (op p bvl body)
  ;; Used by the Parser at compile time.
  (if (not (consp op))
      `(,(symbolconc 'def- p '-fun) ,op ,bvl
	,(car bvl)
	;; so compiler won't warn about
	;; unused lambda variable.
	. ,body)
      `(progn
	,(make-parser-fun-def (car op) p bvl body)
	,@(mapcar #'(lambda (x)
		      `(inherit-propl ',x ',(car op) (,(symbolconc p '-propl))))
		  (cdr op)))))

;;; The tokenizer use the famous CSTR to represent the possible extended token
;;; symbols. The derivation of the name and implementation is obscure, but I've
;;; heard it has something to do with an early Fortran compiler written in Lisp.
;;;  -GJC

;;; (CSTRSETUP <description>)
;;;
;;;  <description> ::= (<descriptor> <descriptor> ...)
;;;  <descriptor>  ::= <name> ! (<name> <translation>)
;;;
;;;  If no translation is supplied, $<name> is the default.
;;;
;;;  Sets up a CSTR [Command STRucture] object which may be used
;;;  in conjunction with the CEQ predicate to determine if the
;;;  LINBUF cursor is currently pointing at any keyword in that
;;;  structure.
;;;
;;;  Note: Names containing shorter names as initial segments
;;;        must follow the shorter names in arg to CSTRSETUP.

(defvar symbols-defined () "For safe keeping.")
(defvar macsyma-operators ())

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun *define-initial-symbols (l)
    (setq symbols-defined
	  (sort (copy-list l) #'(lambda (x y) (< (flatc x) (flatc y)))))
    (setq macsyma-operators (cstrsetup symbols-defined))))

(defmacro define-initial-symbols (&rest l)
  (let ((symbols-defined ())
	(macsyma-operators ()))
    (*define-initial-symbols l)
    `(progn
      (declare-top (special symbols-defined macsyma-operators))
      (setq symbols-defined (copy-list ',symbols-defined))
      (setq macsyma-operators (subst () () ',macsyma-operators)))))

(defun undefine-symbol (opr)
  (*define-initial-symbols (delete opr symbols-defined :test #'equal)))

(defun define-symbol (x)
  (*define-initial-symbols (cons x symbols-defined))
  (symbolconc '$ (maybe-invert-string-case x)))

(defun cstrsetup (arg)
  (do ((arg arg (cdr arg))
       (tree nil))
      ((null arg) (list* () '(ans ()) tree))
    (if (atom (car arg))
	(setq tree (add2cstr (car arg)
                         tree
                         (symbolconc '$
                                     (if (stringp (car arg))
                                       (maybe-invert-string-case (car arg))
                                       (car arg)))))
	(setq tree (add2cstr (caar arg) tree (cadar arg))))))

;;; (ADD2CSTR <name> <tree> <translation>)
;;;
;;;  Adds the information <name> -> <translation> to a
;;;  CSTR-style <tree>.

(defun add2cstr (x tree ans)
  (add2cstr1 (nconc (exploden x) (ncons (list 'ans ans))) tree))

;;; (ADD2CSTR1 <translation-info> <tree>)
;;;
;;;  Helping function for ADD2CSTR. Puts information about a
;;;  keyword into the <tree>

(defun add2cstr1 (x tree)
  (cond ((null tree) x)
	((atom (car tree))
	 (cond ((equal (car tree) (car x))
		(rplacd tree (add2cstr1 (cdr x) (cdr tree))))
	       (t (list tree (cond ((atom (car x)) x)
				   ((equal (caar x) 'ans) (car x))
				   (t x))))))
	((equal (caar tree) (car x))
	 (rplacd (car tree) (add2cstr1 (cdr x) (cdar tree)))
	 tree)
	((null (cdr tree))
	 (rplacd tree (list x))
	 tree)
	(t (rplacd tree (add2cstr1 x (cdr tree)))
	   tree)))
