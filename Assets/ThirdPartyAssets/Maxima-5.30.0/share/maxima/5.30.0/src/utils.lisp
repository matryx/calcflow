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

(macsyma-module utils)

;;; General purpose Lisp utilities.  This file contains runtime functions which
;;; are simple extensions to Lisp.  The functions here are not very general, 
;;; but generalized forms would be useful in future Lisp implementations.
;;;
;;; No knowledge of the Macsyma system is kept here.  
;;;
;;; Every function in this file is known about externally.

(defmacro while (cond &rest body)
  `(do ()
       ((not ,cond))
     ,@body))

(defun maxima-getenv (envvar)
  #+gcl     (si::getenv envvar)
  #+ecl     (si::getenv envvar)
  #+allegro (system:getenv envvar)
  #+(or cmu scl) (cdr (assoc envvar ext:*environment-list* :test #'string=))
  #+sbcl    (sb-ext:posix-getenv envvar)
  #+clisp   (ext:getenv envvar)
  #+(or openmcl mcl)     (ccl::getenv envvar)
  #+lispworks (hcl:getenv envvar)
  )

;; CMUCL needs because when maxima reaches EOF, it calls BYE, not $QUIT.

(defun bye ()
  #+(or cmu scl clisp) (ext:quit)
  #+sbcl               (sb-ext:quit)
  #+allegro            (excl:exit)
  #+(or mcl openmcl)   (ccl:quit)
  #+gcl                (lisp:quit)
  #+ecl                (si:quit)
  #+lispworks          (lispworks:quit)
  )


;;; F is assumed to be a function of two arguments.  It is mapped down L
;;; and applied to consequtive pairs of elements of the list.
;;; Useful for iterating over property lists.

(defmfun map2c (f l)
  (do ((llt l (cddr llt)) (lans))
      ((null llt) lans)
    (push (funcall f (car llt) (cadr llt)) lans)))

;;; Like MAPCAR, except if an application of F to any of the elements of L
;;; returns NIL, then the function returns NIL immediately.

(defmfun andmapcar (f l &aux d answer)
  (do ((l l (cdr l)))
      ((null l) (nreverse answer))
    (setq d (funcall f (car l)))
    (if d (push d answer) (return nil))))

;;; Returns T if either A or B is NIL, but not both.

(defmfun xor (a b)
  (or (and (not a) b) (and (not b) a)))
  
;;; A MEMQ which works at all levels of a piece of list structure.
;;;
;;; Note that (AMONG NIL '(A B C)) is T, however.  This could cause bugs.
;;; > This is false. (AMONG NIL anything) returns NIL always. -kmp

(defmfun among (x l) 
  (cond ((null l) nil)
	((atom l) (eq x l))
	(t (or (among x (car l)) (among x (cdr l)))))) 

;;; Similar to AMONG, but takes a list of objects to look for.  If any
;;; are found in L, returns T.

(defmfun amongl (x l) 
  (cond ((null l) nil)
	((atom l) (member l x :test #'eq))
	(t (or (amongl x (car l)) (amongl x (cdr l)))))) 

;;; Takes a list in "alist" form and converts it to one in
;;; "property list" form, i.e. ((A . B) (C . D)) --> (A B C D).
;;; All elements of the list better be conses.

(defmfun dot2l (l)
  (cond ((null l) nil)
	(t (list* (caar l) (cdar l) (dot2l (cdr l))))))

;;; (C-PUT  sym value    selector)
;;;
;;;  Make a symbol's property list look like a structure.
;;;
;;;  If the value to be stored is NIL,
;;;     then flush the property.
;;;     else store the value under the appropriate property.
;;;

(defmfun cput (bas val sel)
  (cond ((null val)
	 (zl-remprop bas sel)
	 nil)
	(t
	 (putprop bas val sel))))
