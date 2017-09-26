;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  (c) Copyright 1976, 1983 Massachusetts Institute of Technology      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module inmis)

(declare-top (special listofvars))

(defmvar $listconstvars nil
  "Causes LISTOFVARS to include %E, %PI, %I, and any variables declared
   constant in the list it returns if they appear in exp.  The default is
   to omit these." boolean see-also $listofvars)

(defmvar $listdummyvars t)

(defmfun $unknown (f) (catch 'unknown (unknown (specrepcheck f))))

(defun unknown (f)
  (and (not (mapatom f))
       (cond ((and (eq (caar f) 'mqapply)
		   (not (oldget (caaadr f) 'specsimp)))
	      (throw 'unknown t))
	     ((not (oldget (caar f) 'operators)) (throw 'unknown t))
	     (t (mapc #'unknown (cdr f)) nil))))

(defmfun $listofvars (e) 
  (let ((listofvars (ncons '(mlist))))
    (when ($ratp e)
      (and (member 'trunc (cddar e) :test #'eq) (setq e ($taytorat e)))
      (setq e (cons '(mlist)
		    (sublis (mapcar #'cons
				    (car (cdddar e))
				    ;; GENSYMLIST
				    (caddar e))
			    ;; VARLIST
			    (union* (listovars (cadr e))
				    (listovars (cddr e)))))))
    (atomvars e)
    (if (not $listdummyvars)
	(dolist (u (cdr listofvars))
	  (if (freeof u e)
	      (setq listofvars (delete u listofvars :count 1 :test #'equal)))))
    listofvars))

(defun atomvars (e) 
  (cond ((and (symbolp e)
              (or $listconstvars
                  ;; Do not add constants or boolean values to list of vars.
                  (and (not ($constantp e))
                       (not (member e '(t $true nil $false))))))
         (add2lnc e listofvars))
	((atom e))
	((specrepp e) (atomvars (specdisrep e)))
	((member 'array (car e) :test #'eq) (myadd2lnc e listofvars))
	(t (mapc #'atomvars (margs e))))) 

(defun myadd2lnc (item list) 
  (and (not (memalike item list)) (nconc list (ncons item)))) 

;; Bind variables declared with DEFMVAR to their initial values.
;; Some initial values are non-Maxima expressions, e.g., (2 3 5 7)
;; Attempt to handle those as well as Maxima expressions.
;; No attempt is made to handle variables declare with DEFVAR or by other means.

(defun maybe-reset (key val actually-reset reset-verbose)
  (declare (special munbindp))
  ; MAYBE DEFMVAR VALUES SHOULD ONLY BE MAXIMA EXPRESSIONS ??
  (let ((non-maxima (and (consp val) (not (consp (car val))))))
    (when
      ; TEST (BOUNDP KEY), OTHERWISE ATTEMPT TO COMPARE VALUES FAILS ...
      (and (boundp key)
        (if non-maxima
          ; Apply EQUALP to non-Maxima expressions.
          (not (equalp (symbol-value key) val))
          ; Apply ALIKE1 to Maxima expressions.
          (not (alike1 (symbol-value key) val))))
      
      (when reset-verbose
        ; ATTEMPT TO COPE WITH NON-MAXIMA EXPRESSIONS FOR BENEFIT OF DISPLA
        (let ((displa-val (if non-maxima `((mprogn) ,@val) val)))
          (displa `((mtext) "reset: bind " ,key " to " ,displa-val))))
      (nconc actually-reset (list key))
      (let ((munbindp t))
        (meval `((msetq) ,key ((mquote) ,val)))))))

(defmspec $reset_verbosely (L)
  (reset-do-the-work (cdr L) t))

(defmspec $reset (L)
  (reset-do-the-work (cdr L) nil))

(defun reset-do-the-work (args reset-verbose)

  (let ((actually-reset (copy-tree '((mlist)))) ($lispdisp t))
    (if args
      (mapcar
        #'(lambda (key)
            (multiple-value-bind (val found-p) (gethash key *variable-initial-values*)
              (if found-p (maybe-reset key val actually-reset reset-verbose))))
        args)

      (maphash
        #'(lambda (key val)
            (maybe-reset key val actually-reset reset-verbose))
        *variable-initial-values*))
  
    actually-reset))
