;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;	** (c) Copyright 1982 Massachusetts Institute of Technology **

(macsyma-module optim)

(declare-top (unspecial args))

(defvar *subexp* (make-array 64 :initial-element nil))

(defmvar $optimprefix '$%)

(defmvar $optimwarn t "warns if `optimize' encounters a special form.")

;; $OPTIMIZE takes a Macsyma expression and returns a BLOCK form which is
;; equivalent, but which uses local variables to store the results of computing
;; common subexpressions.  These subexpressions are found by hashing them.

(defmfun $optimize (x0)
  (let (($optimwarn $optimwarn)
	*setqs*
	vars
	(*optimcount* 0)
	(*xvars* (cdr ($listofvars x0))))
    (declare (special *optimcount* *xvars* *setqs* vars))
    (fill *subexp* nil)
    (prog ((x (collapse (opformat (collapse x0)))))
       (when (atom x) (return x))
       (comexp x)
       (setq x (optim x))
       (return (prog1 (cond ((null vars) x0)
			    (t (if (or (not (eq (caar x) 'mprog))
				       (and ($listp (cadr x)) (cdadr x)))
				   (setq x (nreverse (cons x *setqs*)))
				   (setq x (nreconc *setqs* (cddr x))))
			       `((mprog simp) ((mlist) ,@(nreverse vars)) ,@x)))
		 (fill *subexp* nil))))))

(defun opformat (x)
  (cond ((atom x) x)
	((specrepp x) (opformat (specdisrep x)))
	((and $optimwarn
	      (mspecfunp (caar x))
	      (prog2 (mtell (intl:gettext "optimize: encountered a special form; result may be wrong."))
		  (setq $optimwarn nil))))
	((eq (caar x) 'mexpt) (opmexpt x))
	(t (let ((newargs (mapcar #'opformat (cdr x))))
	     (if (alike newargs (cdr x)) x (cons (car x) newargs))))))

(defun opmexpt (x)
  (let ((*base (opformat (cadr x))) (exp (opformat (caddr x))) xnew negexp)
    (setq negexp
	  (cond ((and (realp exp) (minusp exp)) (- exp))
		((and (ratnump exp) (minusp (cadr exp)))
		 (list (car exp) (- (cadr exp)) (caddr exp)))
		((and (mtimesp exp) (realp (cadr exp)) (minusp (cadr exp)))
		 (if (equal (cadr exp) -1)
		     (if (null (cdddr exp)) (caddr exp)
			 (cons (car exp) (cddr exp)))
		     (list* (car exp) (- (cadr exp)) (cddr exp))))
		((and (mtimesp exp) (ratnump (cadr exp)) (minusp (cadadr exp)))
		 (list* (car exp)
			(list (caadr exp) (- (cadadr exp)) (caddr (cadr exp)))
			(cddr exp)))))
    (setq xnew
	  (cond (negexp
		 `((mquotient)
		   1
		   ,(cond ((equal negexp 1) *base)
			  (t (setq xnew (list (car x) *base negexp))
			     (if (and (ratnump negexp) (equal (caddr negexp) 2))
				 (opmexpt xnew)
				 xnew)))))
		((and (ratnump exp) (equal (caddr exp) 2)) 
		 (setq exp (cadr exp))
		 (if (equal exp 1) `((%sqrt) ,*base)
		     `((mexpt) ((%sqrt) ,*base) ,exp)))
		(t (list (car x) *base exp))))
    (if (alike1 x xnew) x xnew)))

(defmfun $collapse (x)
  (fill *subexp* nil)
  (prog1 (collapse x) (fill *subexp* nil)))

(defun collapse (x)
  (cond ((atom x) x)
	((specrepp x) (collapse (specdisrep x)))
	(t (let ((n (opt-hash (caar x))))
	     (do ((l (cdr x) (cdr l)))
		 ((null l))
	       (if (not (eq (collapse (car l)) (car l)))
		   (rplaca l (collapse (car l))))
	       (setq n (rem (+ (opt-hash (car l)) n) 12553.)))
	     (setq n (logand 63 n))
	     (do ((l (aref *subexp* n) (cdr l)))
		 ((null l) (setf (aref *subexp* n) (cons (list x) (aref *subexp* n))) x)
	       (if (alike1 x (caar l)) (return (caar l))))))))

(defun comexp (x)
  (if (not (or (atom x) (eq (caar x) 'rat)))
      (let ((n (opt-hash (caar x))))
	(dolist (u (cdr x)) (setq n (rem (+ (opt-hash u) n) 12553.)))
	(setq x (assol x (aref *subexp* (logand 63. n))))
	(cond ((null (cdr x)) (rplacd x 'seen) (mapc #'comexp (cdar x)))
	      (t (rplacd x 'comexp))))))

(defun optim (x)
  (declare (special *setqs*))
  (cond ((atom x) x)
	((and (member 'array (cdar x) :test #'eq)
	      (not (eq (caar x) 'mqapply))
	      (not (mget (caar x) 'arrayfun-mode)))
	 x)
	((eq (caar x) 'rat) x)
	(t (let ((n (opt-hash (caar x))) (nx (list (car x))))
	     (dolist (u (cdr x))
	       (setq n (rem (+ (opt-hash u) n) 12553.)
		     nx (cons (optim u) nx)))
	     (setq x (assol x (aref *subexp* (logand 63. n))) nx (nreverse nx))
	     (cond ((eq (cdr x) 'seen) nx)
		   ((eq (cdr x) 'comexp)
		    (rplacd x (getoptimvar))
		    (push `((msetq) ,(cdr x) ,nx) *setqs*)
		    (cdr x))
		   (t (cdr x)))))))

(defun opt-hash (exp)		   ; EXP is in general representation.
  (rem (if (atom exp)
	   (sxhash exp)
	   (do ((n (opt-hash (caar exp)))
		(args (cdr exp) (cdr args)))
	       ((null args) n)
	     (setq n (rem (+ (opt-hash (car args)) n) 12553.))))
       12553.))		       ; a prime number < 2^14 ; = PRIME(1500)


(defun getoptimvar ()
  (declare (special *optimcount* *xvars* vars))
  (loop with var
     do
     (incf *optimcount*)
     (setq var (make-symbol (format nil "~A~D" $optimprefix *optimcount*)))
     while (member var *xvars* :test #'eq)
     finally
     (push var vars)
     (return var)))
