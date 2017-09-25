;;  Author Barton Willis
;;  University of Nebraska at Kearney
;;  Copyright (C) 2008 Barton Willis

;;  This program is free software; you can redistribute it and/or modify 
;;  it under the terms of the GNU General Public License as published by	 
;;  the Free Software Foundation; either version 2 of the License, or		 
;;  (at your option) any later version.					 
 		       								 
;;  This program is distributed in the hope that it will be useful,		 
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of		 
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the		 
;;  GNU General Public License for more details.

($load "fourier_elim")

(defun $simp_inequality (e)
  (let ((ee (standardize-inequality e)))
    (if (or (eq ee t) (eq ee nil)) ee e)))

;; non-short-circuited boolean operators and or.

(mfuncall '$nary "%and")
(defprop %and wxxml-nary wxxml)
(defprop %and"<mspace/><fnm> %and </fnm><mspace/>" wxxmlsym)
(defprop %and "<fnm> %and </fnm>" wxxmlword)
(defprop %and 120. wxxml-lbp)
(defprop %and 120. wxxml-rbp)
(displa-def %and dimension-nary " %and ")
(defprop %and tex-nary tex)
(defprop %and (" \\land ") texsym)
(setf (get '$%and 'operators) 'simp-%and)
(setf (get '%and 'operators) 'simp-%and)

;; Efficiency hack (see nset.lisp) -- this tells xreduce that %and is nary.
(def-nary '$%and (s) (simplify (cons '(%and) s)) t)

(defun simp-%and (e yy z)
  (declare (ignore yy))
  (let ((not-e) (acc) (b))

    ;; flatten and simplify each argument
    (setq e (margs e))
    (dolist (ek e)
      (setq ek (simplifya (specrepcheck ek) z))
      (setq b (standardize-inequality ek))
      (setq ek (if (or (eq b t) (eq b nil)) b ek))
      (if (op-equalp ek '%and) (setq acc (append acc (margs ek))) (push ek acc)))

    ;; setify and remove true
    (setq e ($disjoin t (opapply '$set acc)))

    ;; logically negate each member of e
    (setq not-e (opapply '$set (mapcar #'(lambda (s) (take '(mnot) s)) (margs e))))

    ;; simplifications:
    ;;  (1) if intersect(e, not(e)) # empty, return false,
    ;;  (2) if false in e, return false,
    ;;  (3) if e is empty, return true,
    ;;  (4) if e is a singleton set, return x.
   
    (cond ((not ($emptyp ($intersection e not-e))) nil)
	  (($elementp nil e) nil)
	  (($emptyp e) t)
	  ((not (cddr e)) (cadr e))
	  (t `((%and simp) ,@(margs e))))))
  
(mfuncall '$nary "%or")
(defprop %or wxxml-nary wxxml)
(defprop %or "<mspace/><fnm> %or </fnm><mspace/>" wxxmlsym)
(defprop %or "<fnm> %or </fnm>" wxxmlword)
(defprop %or 120. wxxml-lbp)
(defprop %or 120. wxxml-rbp)

(defprop %or tex-nary tex)
(defprop %or (" \\lor ") texsym)

(displa-def %or dimension-nary " %or ")
(setf (get '$%or 'operators) 'simp-%or)
(setf (get '%or 'operators) 'simp-%or)

(defun $disjunction_p (e)
  (op-equalp e '%or))

(defun $conjunction_p (e)
  (op-equalp e '%and))


;; Efficiency hack (see nset.lisp) -- this tells xreduce that %or is nary.
(def-nary '$%or (s) (simplify (cons '(%or) s)) nil)

(defun simp-%or (e yy z)
  (declare (ignore yy))
  (let ((not-e) (acc) (b))

    ;; flatten and simplify each argument
    (setq e (margs e))
    (dolist (ek e)
      (setq ek (simplifya (specrepcheck ek) z))
      (setq b (standardize-inequality ek))
      (setq ek (if (or (eq b t) (eq b nil)) b ek))
      (if (op-equalp ek '%or) (setq acc (append acc (margs ek))) (push ek acc)))

    ;; setify and remove false
    (setq e ($disjoin nil (opapply '$set acc)))

    ;; logically negate each member of e
    (setq not-e (opapply '$set (mapcar #'(lambda (s) (take '(mnot) s)) (margs e))))
  
    ;; simplifications:
    ;;  (1) if intersect(e, not(e)) # empty, return true
    ;;  (2) if true e in e, return true,
    ;;  (3) if e is empty, return false,
    ;;  (4) if is a singleton set, return x.
    
    (cond ((not ($emptyp ($intersection e not-e))) t)
	  (($elementp t e) t)
	  (($emptyp e) nil)
	  ((not (cddr e)) (cadr e))
	  (t `((%or simp) ,@(margs e))))))

(setf (get '$%union 'operators) 'simp-%union)

(defun simp-%union (e yy z)
  (declare (ignore yy))
  (let ((acc))
    ;; flatten and simplify each argument
    (setq e (margs e))
    (dolist (ek e)
      (setq ek (simplifya (specrepcheck ek) z))
      (if (op-equalp ek '$%union) (setq acc (append acc (margs ek))) (push ek acc)))
    ;; setify and remove $emptyset.
    (setq e (margs ($disjoin (take '($set)) (opapply '$set acc))))
    `(($%union simp) ,@e)))
  
;; TeX support
(defprop $%union tex-nary tex)
(defprop $%union (" \\cup ") texsym)

(setf (get '$%if 'operators) 'simp-%if)

(defun simp-%if (e yy z)
  (declare (ignore yy))
  (pop e) ;; remove ($%if simp)
  (let (($domain '$complex)
	(cnd (if e (simpcheck (pop e) z) (wna-err '$%if)))
	(a (if e (pop e) (wna-err '$%if)))
	(b (if e (pop e) (wna-err '$%if))))
    (if e (wna-err '$%if))
    (setq cnd (standardize-inequality ($substitute '%or 'mor ($substitute '%and 'mand cnd))))
    (setq cnd ($substitute '%or 'mor ($substitute '%and 'mand cnd)))
    (cond ((eq cnd t) (simpcheck a z))
	  ((eq cnd nil) (simpcheck b z))
	  (t
	   (setq a (simpcheck a z))
	   (setq b (simpcheck b z))
	   (if (like a b) a `(($%if simp) ,cnd ,a ,b))))))

(setf (get '$%integerp 'operators) 'simp-%integerp)

(defun simp-%integerp (e yy z)
  (declare (ignore yy))
  (oneargcheck e)
  (let ((sgn))
    (setq e (simplifya (second e) z))
    (setq sgn ($compare e (take '($floor) e)))
    (cond ((equal sgn "=") t)
	  ((member sgn '("<" ">" "#") :test #'equal) nil)
	  ((and (symbolp e) ($featurep e '$noninteger)) nil)
	  (t `(($%integerp simp) ,e)))))
    		  
(setf (get '$isnonnegative_p 'operators) 'simp-isnonnegative-p)

(defun simp-isnonnegative-p (e yy z)
  (declare (ignore yy))
  (oneargcheck e)
  (let (($domain '$complex) (is-real) (sgn))
    (setq e (simplifya (specrepcheck (cadr e)) z))
    (setq is-real (take '($isreal_p) e))
    (cond ((eq t is-real)
	   (setq sgn (csign e))
	   (cond ((memq sgn '($zero $pz $pos)) t)
		 ((eq sgn '$neg) nil)
		 (t `(($isnonnegative_p simp) ,e))))
	  ((eq nil is-real) nil)
	  (t `(($isnonnegative_p simp) ,e)))))

;; Similar to sublis, but allow for substitutions of nonatoms.

(defun $subst_parallel (l e)
  (let ((alist nil) (is-a-rat ($ratp e)) (old) (new))
    (setq l (if ($listp l) (margs l) (list l)))

    ;; Build an association list for the Common Lisp sublis function.
    (dolist (lk l) 
      (if (mequalp lk) 
	  (progn
	    (setq old (cadr lk))
	    (setq new (caddr lk))
	    (setq old (if (stringp old) (amperchk old) old))
	    (push (cons old new) alist))
	(merror "Each substitution must be an equation; found" lk)))
    (setq e (resimplify (sublis alist ($ratdisrep e) :test #'alike))) ;;or like?
    (if is-a-rat ($rat e) e)))
