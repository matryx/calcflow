;;; -*- Mode:LISP; Package:MACSYMA -*-
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; Comments: Code to generate ctensor programs from itensor expressions
;;

;	** (c) Copyright 1979 Massachusetts Institute of Technology **

(in-package :maxima)


(declare-top (special $imetric $metricconvert indlist empty))

;$METRICCONVERT if non-NIL will allow $IC_CONVERT to rename the metric tensor
;   ($IMETRIC must be bound) with 2 covariant indices to LG and with 2
;   contravariant indices to UG.

(defun $ic_convert (ee)
       (prog (e free lhs rhs)
         (setq e ($expand ee))
	     (cond ((or (atom e) (not (eq (caar e) 'mequal)))
		    (merror "IC_CONVERT requires an equation as an argument"))
		   ((equal (setq free ($indices e)) empty)
		    (return (cons '(msetq) (cdr e))))
		   ((or (eq (ml-typep (cadr e)) 'symbol)           ;If a symbol or
			(and (rpobj (cadr e))  ;an indexed object with no dummy
			     (null (cdaddr ($indices2 (cadr e))))))    ;indices
		    (setq lhs (cadr e) rhs (caddr e)))
		   ((or (eq (ml-typep (caddr e)) 'symbol)
			(and (rpobj (caddr e))
			     (null (cdaddr ($indices2 (caddr e))))))
		    (setq lhs (caddr e) rhs (cadr e)))
		   (t (merror "At least one side of the equation must be a~
			      ~%symbol or a single indexed object")))
	     (cond ((and (not (eq (ml-typep lhs) 'symbol))
			 (not (null (cdddr lhs))))
		    (merror "Cannot assign to indexed objects with derivative ~
			    indices:~%~M"
			    (ishow lhs))))
	     (setq free (nreverse (itensor-sort (cdadr free)))  ;Set FREE to just the
		   indlist nil)                           ;free indices
	     (and $metricconvert (boundp '$imetric)
		  (setq lhs (changename $imetric t 0 2 '$ug
					(changename $imetric t 2 0 '$lg lhs))
			rhs (changename $imetric t 0 2 '$ug
					(changename $imetric t 2 0 '$lg rhs))))
	     (tabulate rhs)
	     (setq indlist (unique indlist))
	     (do ((q (mapcar 'car indlist) (cdr q)))
		 ((null q))
		 (cond ((member (car q) (cdr q) :test #'eq)
			(merror "~
IC_CONVERT cannot currently handle indexed objects of the same name~
~%with different numbers of covariant and//or contravariant indices:~%~M"
				(car q)))))
	     (cond ((not (eq (ml-typep lhs) 'symbol))
		    (do ((test) (flag) (name))
			(flag)
			(setq test (list (caar lhs) (length (cdadr lhs))
					 (length (cdaddr lhs))))
			(cond ((or (member test indlist :test #'equal)
				   (not (member (car test)
						(mapcar 'car indlist) :test #'eq)))
			       (setq flag t))
			      (t
			       (mtell "Assignment is to be made to ~M~
~%This name with a different number of covariant and//or contravariant~
~%indices appears on the other side of the equation. To avoid array name~
~%conflicts, choose a new name for this object:~%"
				      (ishow lhs))
                               (cond ((not (eq (ml-typep
						(setq name
						      (retrieve nil nil)))
					       'symbol))
				      (merror "Name not an atom")))
			       (setq lhs (cons (ncons name) (cdr lhs))))))))
	     (return (do ((free free (cdr free))
			  (equ (cons '(msetq) (list (changeform lhs)
						    (t-convert
						     (summer1 rhs))))))
			 ((null free) equ)
			 (setq equ (append '((mdo)) (ncons (car free))
					   '(1 1 nil $dim nil)
					   (ncons equ)))))))

(defun tabulate (e)        ;For each indexed object in E, appends a list of the
       (cond ((atom e))    ;name of that object and the number of covariant and
	     ((rpobj e)    ;contravariant indices to the global list INDLIST
	      (setq indlist (cons (list (caar e) (length (cdadr e))
					(length (cdaddr e)))
				  indlist)))
	     ((or (eq (caar e) 'mplus) (eq (caar e) 'mtimes))
	      (mapcar 'tabulate (cdr e)))))

(defun unique (l)                   ;Returns a list of the unique elements of L
       (do ((a l (cdr a)) (b))
	   ((null a) b)
	   (cond ((not (member (car a) b :test #'equal))
		  (setq b (cons (car a) b))))))

(defun summer1 (e)     ;Applies SUMMER to the products and indexed objects in E
       (cond ((atom e) e)
	     ((eq (caar e) 'mplus)
  	      (cons (car e) (mapcar 'summer1 (cdr e))))
	     ((or (eq (caar e) 'mtimes) (rpobj e))
	      (summer e (cdaddr ($indices e))))
	     (t e)))

(defun summer (p dummy) ;Makes implicit sums explicit in the product or indexed
                        ;object P where DUMMY is the list of dummy indices of P
       (prog (dummy2 scalars indexed s dummy3)                   ;at this level
	     (setq dummy2 (intersect (all ($indices2 p)) dummy))
	     (do ((p (cond ((eq (caar p) 'mtimes) (cdr p))
			   (t (ncons p))) (cdr p))
		  (obj))
		 ((null p))
		 (setq obj (car p))
		 (cond ((atom obj)
			(setq scalars (cons obj scalars)))
		       ((rpobj obj)
			(cond ((null (intersect dummy2 (all ($indices2 obj))))
			       (setq scalars (cons obj scalars)))
			      (t (setq indexed (cons obj indexed)))))
		       ((eq (caar obj) 'mplus)
			(setq s t)
			(cond ((null (intersect dummy (all ($indices obj))))
			       (setq scalars
				     (cons (summer1 obj) scalars)))
			      (t (setq indexed
				       (cons (summer1 obj) indexed)))))
		       (t (setq scalars (cons obj scalars)))))
	     (cond ((and s
			 (not (samelists dummy2
					 (setq s
					       (cdaddr
						($indices
						 (append '((mtimes))
							 scalars indexed)))))))
		    (setq dummy3 s
			  s scalars
			  scalars nil)
		    (do ((p s (cdr p)) (obj))
			((null p))
			(setq obj (car p))
			(cond ((null (intersect dummy3 (all ($indices obj))))
			       (setq scalars (cons obj scalars)))
			      (t (setq indexed (cons obj indexed)))))))
	     (return
	      (simptimes
	       (nconc (ncons '(mtimes))
		      scalars
		      (cond ((not (null indexed))
		             (do ((indxd (simptimes (cons '(mtimes) indexed)
						    1 nil))
			          (dummy (itensor-sort dummy2) (cdr dummy)))
			         ((null dummy) (ncons indxd))
			         (setq indxd (nconc (ncons '($sum))
						    (ncons indxd)
						    (ncons (car dummy))
						    '(1 $dim)))))
			    (t nil)))
	       1 nil))))

(defun all (l)                        ;Converts [[A, B], [C, D]] into (A B C D)
       (append (cdadr l) (cdaddr l)))

(defun t-convert (e)        ;Applies CHANGEFORM to each individual object in an
       (cond ((atom e) e)   ;expression
	     ((or (eq (caar e) 'mplus) (eq (caar e) 'mtimes))
	      (cons (car e) (mapcar 't-convert (cdr e))))
	     ((eq (caar e) '$sum)
	      (append (ncons (car e)) (ncons (t-convert (cadr e))) (cddr e)))
	     (t (changeform e))))

(defun changeform (e)           ;Converts a single object from ITENSOR format to
       (cond ((atom e) e)       ;ETENSR format
	     ((rpobj e)
	      (do ((deriv (cdddr e) (cdr deriv))
;		   (new (cond ((and (null (cdadr e)) (null (cdaddr e)))
		   (new (cond ((and (null (covi e)) (null (conti e)))
			       (caar e))     ;If no covariant and contravariant
			                     ;indices then make into an atom
			      (t (cons (cons (equiv-table (caar e)) '(array))
;				       (append (cdadr e) (cdaddr e)))))))
				       (append (covi e) (conti e)))))))
		  ((null deriv) new)
		  (setq new (append '(($diff)) (ncons new)
				    (ncons (cons '($ct_coords array)
						 (ncons (car deriv))))))))
	     (t e)))

(defun equiv-table (a)                ;Makes appropiate name changes converting
       (cond ((member a '($ichr1 %ichr1) :test #'eq) '$lcs)            ;from ITENSOR to ETENSR
	     ((member a '($ichr2 %ichr2) :test #'eq) '$mcs)
	     (t a)))

(declare-top (unspecial indlist))

(declare-top (special smlist $funcs))
(setq $funcs '((mlist)))

(defun $makebox (e name)
       (cond ((atom e) e)
	     ((mtimesp e) (makebox e name))
	     ((mplusp e)
	      (mysubst0 (simplifya (cons '(mplus)
					 (mapcar
					  (function
					   (lambda (q) ($makebox q name)))
					  (cdr e)))
				   nil)
			e))
	     ((mexptp e) (list (car e) ($makebox (cadr e) name) (caddr e)))
	     (t e))) 

(defun makebox (e name)
       (prog (l1 l2 x l3 l) 
	     (setq l (cdr e))
	again(setq x (car l))
	     (cond ((rpobj x)
		    (cond ((and (eq (caar x) name) (null (cdddr x))
				(null (cdadr x)) (= (length (cdaddr x)) 2))
			   (setq l1 (cons x l1)))
			  ((cdddr x) (setq l2 (cons x l2)))
			  (t (setq l3 (cons x l3)))))
		   (t (setq l3 (cons x l3))))
	     (and (setq l (cdr l)) (go again))
	     (cond ((or (null l1) (null l2)) (return e)))
	     (do ((l2 l2 (cdr l2)))
		 ((null l2) )
		 (setq l l1)
;	     (do l2 l2 (cdr l2)
;	      (null l2)
;	      (setq l l1)..)

	     (tagbody
	      loop
	      (setq x (car l))
	      (cond
	       ((and (member (car (cdaddr x)) (cdddar l2) :test #'eq)
		     (member (cadr (cdaddr x))(cdddar l2) :test #'eq))
		(setq 
		 l3
		 (cons (nconc
		  (list
		   (ncons
		    (implode (append '([ ])
				    (cdr (explodec (caaar l2))))))
		   (cadar l2)
		   (caddar l2)) (setdiff (cdddar l2)(cdaddr x)))
		  l3))
		(setq l1 (delete x l1 :count 1 :test #'equal)))
	       ((setq l (cdr l)) (go loop))
	       (t (setq l3 (cons (car l2) l3))))))
	     (return (simptimes (cons '(mtimes) (nconc l1 l3))
				1.
				nil)))) 

(declare-top (special tensr))

(defmfun $average n ((lambda (tensr) (simplifya (average (arg 1)) nil))
		   (and (= n 2) (arg 2))))

(defun average (e)
       (cond ((atom e ) e)
	     ((rpobj e) (cond ((or (not tensr) (eq (caar e) tensr))
			       (average1 e))
			      (t e)))
	     (t (cons (ncons (caar e)) (mapcar (function average) (cdr e))))))

(defun average1 (e)
       (cond ((= (length (cdadr e)) 2)
	      (setq e (list '(mtimes) '((rat simp) 1 2)
			    (list '(mplus)
				  (cons (car e)
					(cons (arev (cadr e)) (cddr e))) e))))
	     ((= (length (cdaddr e)) 2)
	      (setq e (list '(mtimes) '((rat smp) 1 2)
			    (list '(mplus)
				  (cons (car e)
					(cons (cadr e)
					      (cons (arev (caddr e))
						    (cdddr e)))) e)))))
       e)

(defun arev (l) (list (car l) (caddr l) (cadr l)))

(declare-top (unspecial tensr))
(add2lnc '(($average) $tensor) $funcs)

(defun $conmetderiv (e g)
       (cond ((not (eq (ml-typep g) 'symbol))
	      (merror "Invalid metric name: ~M" g))
	     (t (conmetderiv e g ((lambda (l) (append (cdadr l) (cdaddr l)))
				  ($indices e))))))

(defun conmetderiv (e g indexl)
       (cond ((atom e) e)
	     ((rpobj e)
	      (cond ((and (eq (caar e) g) (null (cdadr e))
			  (equal (length (cdaddr e)) 2)
			  (not (null (cdddr e))))
		     (do ((e (cmdexpand (car e) (car (cdaddr e))
					(cadr (cdaddr e)) (cadddr e) indexl))
			  (deriv (cddddr e) (cdr deriv)))
			 ((null deriv) e)
			 (setq e (conmetderiv ($idiff e (car deriv))
					      g indexl))))
		    (t e)))
	     (t (mysubst0 (cons (car e)
				(mapcar
				 (function (lambda (q) 
						   (conmetderiv q g indexl)))
				 (cdr e))) e))))

(defun cmdexpand (g i j k indexl)
       (do ((dummy) (flag))
	   (flag (list '(mplus simp)
		       (list '(mtimes simp) -1
			     (list g (ncons smlist) (list smlist dummy i))
			     (list '($ichr2 simp) (list smlist dummy k)
				   (list smlist j)))
		       (list '(mtimes simp) -1
			     (list g (ncons smlist) (list smlist dummy j))
			     (list '($ichr2 simp) (list smlist dummy k)
				   (list smlist i)))))
	   (setq dummy ($idummy))
	   (and (not (member dummy indexl :test #'eq)) (setq flag t))))

(add2lnc '(($conmetderiv) $exp $name) $funcs)

(defun $flush1deriv (e g)
       (cond ((not (eq (ml-typep g) 'symbol))
	      (merror "Invalid metric name: ~M" g))
	     (t (flush1deriv e g))))

(defun flush1deriv (e g)
       (cond ((atom e) e)
	     ((rpobj e)
	      (cond ((and (eq (caar e) g) (equal (length (cdddr e)) 1)
			  (or (and (equal (length (cdadr e)) 2)
				   (null (cdaddr e)))
			      (and (equal (length (cdaddr e)) 2)
				   (null (cdadr e)))))
		     0)
		    (t e)))
	     (t (subst0 (cons (ncons (caar e))
			      (mapcar
			       (function (lambda (q) (flush1deriv q g)))
			       (cdr e))) e))))

(add2lnc '(($flush1deriv) $exp $name) $funcs)

(defun $igeodesic_coords (exp g)
       ($flush1deriv ($flush exp '$ichr2 '%ichr2) g))

(add2lnc '(($igeodesic_coords) $exp $name) $funcs)


