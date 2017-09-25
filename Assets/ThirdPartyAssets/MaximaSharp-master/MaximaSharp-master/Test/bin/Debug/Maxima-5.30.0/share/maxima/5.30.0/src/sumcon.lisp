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

(macsyma-module sumcon)

(declare-top (special $genindex $niceindicespref $sumexpand))

(defmfun $sumcontract (e)	       ; e is assumed to be simplified
  (cond ((atom e) e)
	((eq (caar e) 'mplus)
	 (do ((x (cdr e) (cdr x)) (sums) (notsums) (car-x))
	     ((null x) (cond ((null sums)
			      (subst0 (cons '(mplus)
					    (nreverse notsums))
				      e))
			     (t (setq sums (sumcontract1 sums))
				(addn (cons sums notsums) t))))
	   (setq car-x (car x))
	   (cond ((atom car-x)
		  (setq notsums (cons car-x notsums)))
		 ((eq (caar car-x) '%sum)
		  (setq sums (cons (cons ($sumcontract (cadr car-x))
					 (cddr car-x))
				   sums)))
		 (t (setq notsums (cons car-x notsums))))))
	(t (recur-apply #'$sumcontract e))))

(defmfun $intosum (e)		       ; e is assumed to be simplified
  (let (($sumexpand t))
    (cond ((atom e) e)
	  ((eq (caar e) 'mtimes)	;puts outside product inside
	   (do ((x (cdr e) (cdr x)) (sum) (notsum))
	       ((null x) (cond ((null sum)
				(subst0 (cons '(mtimes)
					      (nreverse notsum))
					e))
			       (t (simpsum
				   (let ((new-index
					  (if (free (cons nil notsum) (caddr sum))
					      (caddr sum)
					      (get-free-index (cons nil (cons sum notsum))))))
				     (setq sum (subst new-index (caddr sum) sum))
				     (rplaca (cdr sum) (muln (cons (cadr sum) notsum) t))
				     (rplacd (car sum) nil)
				     sum)
				   1 t))))
	     (cond ((atom (car x))
		    (setq notsum (cons (car x) notsum)))
		   ((eq (caaar x) '%sum)
		    (setq sum (if (null sum)
				  (copy-tree (car x))
				  (muln (list sum (car x)) t))))
		   (t (setq notsum (cons ($sumcontract (car x))
					 notsum))))))
	  (t (recur-apply #'$intosum e)))))

(defun sumcontract1 (sums)
  (addn (sumcontract2 nil sums) t))

(defun sumcontract2 (result left)
  (if (null left)
      result
      (let ((x (sumcombine1 (car left) (cdr left))))
	(sumcontract2 (append (car x) result) (cdr x)))))

(defun sumcombine1 (pattern llist)
  (do ((sum pattern) (non-sums nil)
       (un-matched-sums nil) (try-this-one)
       (llist llist (cdr llist)))
      ((null llist) (cons (cons (simplify (cons '(%sum) sum))
				non-sums)
			  un-matched-sums))
    (setq try-this-one (car llist))
    (cond ((and (numberp (sub* (caddr sum) (caddr try-this-one)))
		(numberp (sub* (cadddr sum) (cadddr try-this-one))))
	   (let ((x (sumcombine2 try-this-one sum)))
	     (setq sum (cdar x)
		   non-sums (cons (cdr x) non-sums))))
	  (t (setq un-matched-sums (cons try-this-one un-matched-sums))))))

(defun sumcombine2 (sum1 sum2)
  (let* ((e1 (car sum1))
	 (e2 (car sum2))
	 (i1 (cadr sum1))
	 (i2 (cadr sum2))
	 (l1 (caddr sum1))
	 (l2 (caddr sum2))
	 (h1 (cadddr sum1))
	 (h2 (cadddr sum2))
	 (newl (simplify `(($max) ,l1 ,l2)))
	 (newh (simplify `(($min) ,h1 ,h2)))
	 (newi (cond ((eq i1 i2) i1)
		     ((free e1 i2) i2)
		     ((free e2 i1) i1)
		     (t (get-free-index (list nil i1 i2 e1 e2	l1 l2 h1 h2)))))
	 (extracted nil)
	 (new-sum nil))
    (setq e1 (subst newi i1 e1))
    (setq e2 (subst newi i2 e2))
    (setq new-sum (list '(%sum) (add2 e1 e2) newi newl newh))
    (setq extracted
	  (addn
	   (mapcar #'dosum
		   (list e1 e1 e2 e2)
		   (list newi newi newi newi)
		   (list l1 (add2 newh 1) l2 (add2 newh 1))
		   (list (sub* newl 1) h1 (sub* newl 1) h2)
		   '(t t t t))
	   t))
    (cons new-sum extracted)))

(defmvar $niceindicespref '((mlist simp) $i $j $k $l $m $n))

(defun get-free-index (llist &optional i)
  (or (do ((try-list (cdr $niceindicespref) (cdr try-list)))
	  ((null try-list))
	(if (or (free llist (car try-list))
		(eq i (car try-list)))
	    (return (car try-list))))
      (do ((n 0 (1+ n)) (try))
	  (nil)
	(setq try (intern (format nil "~a~d" (cadr $niceindicespref) n)))
	(if (free llist try) (return try)))))

(defmfun $bashindices (e)	       ; e is assumed to be simplified
  (let (($genindex '$j))
    (cond ((atom e) e)
	  ((member (caar e) '(%sum %product) :test #'eq)
	   (sumconsimp (subst (gensumindex) (caddr e) e)))
	  (t (recur-apply #'$bashindices e)))))

(defmfun $niceindices (e)
  (if (atom e)
      e
      (let ((e (recur-apply #'$niceindices e)))
	(cond ((atom e) e)
	      ((member (caar e) '(%sum %product) :test #'eq)
	       (sumconsimp (subst (get-free-index e (caddr e)) (caddr e) e)))
	      (t e)))))

(defun sumconsimp (e)
  (if (and (not (atom e)) (member (caar e) '(%sum %product) :test #'eq))
      (list* (car e) (sumconsimp (cadr e)) (cddr e))
      (resimplify e)))
