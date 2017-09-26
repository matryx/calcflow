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

(macsyma-module nforma)

(declare-top (special 1//2 -1//2 displayp in-p))

(defmvar $powerdisp nil)
(defmvar $pfeformat nil)
(defmvar $%edispflag nil)
(defmvar $exptdispflag t)
(defmvar $sqrtdispflag t)
(defmvar $negsumdispflag t)

(setq in-p nil)

(defmfun nformat (form &aux (p nil))
  (cond ((atom form)
	 (cond ((and (realp form) (minusp form)) (list '(mminus) (- form)))
	       ((eq t form) (if in-p t '$true))
	       ((eq nil form) (if in-p nil '$false))
	       ;; revision, extension by Richard Fateman 3/2013.
	       ;;  Perhaps some object is an atom, maybe a CLOS object or structure. 
	       ;; Either its type is a symbolp.. 
	       ;; e.g. a structure like (defstruct (ri ...)) is type ri.
	       ;; so we look for a formatter on the type or car of the type.
	       ;; OR
	       ;; if car of the type is also not a symbol, we look for formatter on nil
	       ;; where it isn't.
	       ;; depending on the lisp, type-of may be more or less sophisticated.
	       ;; a "good" lisp
	       ;;  may return a list, e.g. (type-of "abc") is (simple-array character (3))
	       ;; in some lisps, e.g. GCL the type is just  string.
	       
	       ((and (setf p(type-of form))
		     (if (not (symbolp p)) (setf p (car p)) p)
		     (setf p (get (and (symbolp p) p) 'formatter)) 
		     ;; form is an atom of a type with a formatter property
		     (funcall p form)))
	       ;; just display as a lisp symbol, number, or other atom.
	       (t form)))
	((atom (car form))  form) ;; probably an illegal form; just return it.
	((null (cdar form)) form) ;; probably an illegal or unsimplified form; just return it.
	
	;; this next section is for the ordinary maxima objects that are tagged by
	;; their main operator or CAAR,  e.g. ((mplus) a b) has CAAR mplus ...
	((setf p (get (caar form) 'formatter)) ;; find the formatter.  If there is one, call it.
	 (funcall p form))
	(t form)))			; if there is no formatter. Just return form unchanged.

 
(defun form-mplus (form &aux args trunc)
  (setq args (mapcar #'nformat (cdr form)))
  (setq trunc (member 'trunc (cdar form) :test #'eq))
  (cons (if trunc '(mplus trunc) '(mplus))
	(cond ((and (member 'ratsimp (cdar form) :test #'eq)
		    (not (member 'simp (cdar form) :test #'eq)))
	       (if $powerdisp (nreverse args) args))
	      ((and trunc (not (member 'simp (cdar form) :test #'eq))) (nreverse args))
	      ((or $powerdisp trunc (member 'cf (cdar form) :test #'eq)) args)
	      ((and $negsumdispflag (null (cdddr form)))
	       (if (and (not (mmminusp (car args)))
			(mmminusp (cadr args)))
		   args
		   (nreverse args)))
	      (t (nreverse args)))))

(defun form-mtimes (form)
  (cond ((null (cdr form)) '((mtimes)))
	((equal -1 (cadr form)) (list '(mminus) (form-mtimes (cdr form))))
        (t (prog (num den minus flag)
	      (do ((l (cdr form) (cdr l)) (dummy)) ((null l))
		(setq dummy (nformat (car l)))
		(cond ((atom dummy) (setq num (cons dummy num)))
		      ((eq 'mminus (caar dummy))
		       (setq minus (not minus) l (append dummy (cdr l))))
		      ((or (eq 'mquotient (caar dummy))
			   (and (not $pfeformat) (eq 'rat (caar dummy))))
		       (cond ((not (equal 1 (cadr dummy)))
			      (setq num (cons (cadr dummy) num))))
		       (setq den (cons (caddr dummy) den)))
		      (t (setq num (cons dummy num)))))
	      (setq num (cond ((null num) 1)
			      ((null (cdr num)) (car num))
			      (t (cons '(mtimes) (nreverse num))))
		    den (cond ((null den) (setq flag t) nil)
			      ((null (cdr den)) (car den))
			      (t (cons '(mtimes) (nreverse den)))))
	      (if (not flag) (setq num (list '(mquotient) num den)))
	      (return (if minus (list '(mminus) num) num))))))

(defun form-mexpt (form &aux exp)
  (cond ((and $sqrtdispflag (alike1 1//2 (caddr form))) (list '(%sqrt) (cadr form)))
	((and $sqrtdispflag (alike1 -1//2 (caddr form)))
	 (list '(mquotient) 1 (list '(%sqrt) (cadr form))))
	((and (or (and $%edispflag (eq '$%e (cadr form)))
		  (and $exptdispflag (not (eq '$%e (cadr form)))))
	      (not (atom (setq exp (nformat (caddr form)))))
	      (eq 'mminus (caar exp)))
	 (list '(mquotient) 1 (if (equal 1 (cadr exp)) (cadr form)
				  (list '(mexpt) (cadr form) (cadr exp)))))
	(t (cons '(mexpt) (cdr form)))))

(defun form-mrat (form)
  (let ((trunc (member 'trunc (cdar form) :test #'eq)) exact)
    (if (and trunc (eq (cadr form) 'ps))
	(setq exact (null (car (cadddr form)))))
    (setq form (ratdisrepd form))
    (rdis1 form)
    (if (and trunc (or (atom form)
		       ;; A constant, e.g. ((mplus) $a 1)
		       (not (member (car form) '((mplus exact) (mplus trunc)) :test #'equal))))
	(cons (if exact '(mplus exact) '(mplus trunc)) (ncons form))
	(nformat form))))

(defun rdis1 (form)
  (cond ((or (atom form) (specrepp form)))
	((null (cdar form)) (rplaca form (list (caar form) 'ratsimp)))
	(t (mapc #'rdis1 (cdr form)))))

;;(DEFMFUN NFORMAT-ALL (FORM)
;;  (SETQ FORM (NFORMAT FORM))
;;  (IF (OR (ATOM FORM) (EQ (CAAR FORM) 'BIGFLOAT))
;;      FORM
;;      (CONS (DELSIMP (CAR FORM)) (MAPCAR #'NFORMAT-ALL (CDR FORM)))))
;;Update from F302
;; used only in comm.lisp substitute, mpart.
(defmfun nformat-all (form)
  (setq form (nformat form))
  (if (or (atom form) (eq (caar form) 'bigfloat))
      form
      (cons (delsimp (car form))
	    (if (member (caar form) '(mdo mdoin) :test #'eq)
		(mapcar #'(lambda (u) (if u (nformat-all u))) (cdr form))
		(mapcar #'nformat-all (cdr form))))))


;;; we should define all the formatters in the file after the helper functions like  form-mplus
	   
(setf (get 'rat 'formatter) 
  #'(lambda(form)(cond ((minusp (cadr form))
			(list '(mminus) (list '(rat) (- (cadr form)) (caddr form))))
		       (t (cons '(rat) (cdr form))))))

(setf (get 'mmacroexpanded 'formatter) 
  #'(lambda(form)(nformat (caddr form))))

(setf (get 'mplus 'formatter)  #'form-mplus)
(setf (get 'mtimes 'formatter)  #'form-mtimes)
(setf (get 'mexpt 'formatter)  #'form-mexpt)
(setf (get 'mrat 'formatter)  #'form-mrat)
(setf (get 'mpois 'formatter)  #'(lambda(form)(nformat ($outofpois form))))

(setf (get 'bigfloat 'formatter)  
  #'(lambda(form)
	 (if (minusp (cadr form))
	     (list '(mminus) (list (car form) (- (cadr form)) (caddr form)))
	   (cons (car form) (cdr form)))))

(setf (get 'ratio 'formatter)  ;; in case a common lisp ratio is returned somehow.
  #'(lambda (form)
      (cond ((minusp form)
		(list '(mminus) (list '(rat) (- (numerator form)) (denominator form))))
	    (t (list '(rat) (numerator form)(denominator form))))))

(setf (get 'complex 'formatter)  ;; in case a common lisp complex number is returned somehow.
  #'(lambda(form)
          (if (complexp form)
            (nformat `((mplus) ,(realpart form)
                     ((mtimes) ,(imagpart form) $%i)))
            ;; some random form with caar COMPLEX
            ;;not really a CL complex
            form)))

;; something I added for fun
(defstruct (ri (:constructor $interval (lo hi) ))lo hi)
(setf (get 'ri 'formatter) ;; in case a structure of type ri  [real interval] is computed
  #'(lambda(r) (list '($interval simp) (ri-lo r)(ri-hi r)))) ;; this prints it.

;;  so in maxima, we can construct ri structures by typing interval(1,2)
;; and if we display it,  it  appear as  interval(1,2).
;; but ?print(interval(1,2))  shows the lisp value which is the structure,
;; #s(ri :lo 1 :hi 2).   

;; we could set up formatters for , say,  (simple-array single-float <dimensions>)
;; or share the burden with display program .

