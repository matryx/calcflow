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

(macsyma-module mstuff)

(defmfun $sort (l &optional (f 'lessthan))
  (let ((llist l) comparfun bfun ($prederror t))
    (unless ($listp llist)
      (merror (intl:gettext "sort: first argument must be a list; found: ~M") llist))
    (setq llist (copy-list (cdr llist))
	  comparfun 
	  (mfunction1 (setq bfun (getopr f))))
    (when (member bfun '(lessthan great) :test #'eq)
      (setq llist (mapcar #'ratdisrep llist)))
    (cons '(mlist) (stable-sort llist comparfun))))

;; cmulisp does not like the closure version.  Clisp insists on the
;; closure version.  Gcl likes either...  For the moment we will
;; leave a conditional here.
(defun mfunction1 (fun)
  (if (functionp fun)
      fun
      #+(or cmu scl)
      (lambda (x y) (mevalp `((,fun) ((mquote) ,x) ((mquote) ,y))))
      #-(or cmu scl)
      #'(lambda (x y) (mevalp `((,fun) ((mquote) ,x) ((mquote) ,y))))))

(defun lessthan (a b)
  (great b a))

(defmspec $makelist (x)
  (setq x (cdr x))
  (prog (n form arg a b c d lv)
     (setq n (length x))
     (cond
       ((= n 0) (return '((mlist))))
       ((= n 1)
        (setq form (first x))
        (return
          `((mlist) ,(meval `(($ev) ,@(list (list '(mquote) form)))))))
       ((= n 2)
        (setq form (first x))
        (setq b ($float (meval (second x))))
        (if (numberp b)
            (return
              (do
               ((m 1 (1+ m)) (ans))
               ((> m b) (cons '(mlist) (nreverse ans)))
                (push (meval `(($ev) ,@(list (list '(mquote) form))))
                      ans)))
            (merror (intl:gettext "makelist: second argument must evaluate to a number; found: ~M") b)))
       ((= n 3)
        (setq form (first x))
        (setq arg (second x))
        (setq b (meval (third x)))
        (if ($listp b)
            (setq lv (mapcar #'(lambda (u) (list '(mquote) u)) (cdr b)))
            (progn
              (setq b ($float (meval b)))
              (if ($numberp b)
                  (return
                    (do
                     ((m 1 (1+ m)) (ans))
                     ((> m b) (cons '(mlist) (nreverse ans)))
                      (push
                       (meval
                        `(($ev) ,@(list (list '(mquote) form)
                                        (list '(mequal) arg m)))) ans)))
                (merror (intl:gettext "makelist: third argument must be a number or a list; found: ~M") b)))))
       ((= n 4)
        (setq form (first x))
        (setq arg (second x))
        (setq a (meval (third x)))
        (setq b (meval (fourth x)))
        (setq d ($float (meval `((mplus) ,b ((mtimes) ,a -1)))))
        (if (numberp d)
            (setq lv (interval2 a 1 d))
            (merror (intl:gettext "makelist: the fourth argument minus the third one must evaluate to a number; found: ~M") d)))
       ((= n 5)
        (setq form (first x))
        (setq arg (second x))
        (setq a (meval (third x)))
        (setq b (meval (fourth x)))
        (setq c (meval (fifth x)))
        (setq d ($float
                 (meval 
                  `((mtimes) ((mplus) ,b ((mtimes) ,a -1)) ((mexpt) ,c -1)))))
        (if (numberp d)
            (setq lv (interval2 a c d))
            (merror (intl:gettext "makelist: the fourth argument minus the third one, divided by the fifth one must evaluate to a number; found: ~M") d)))
       (t (merror (intl:gettext "makelist: maximum 5 arguments allowed; found: ~M.~%To create a list with sublists, use nested makelist commands.") n)))
     (return 
       (do ((lv lv (cdr lv))
	    (ans))
	   ((null lv) (cons '(mlist) (nreverse ans)))
	 (push (meval `(($ev)
			,@(list (list '(mquote) form)
				(list '(mequal) arg (car lv)))))
	       ans)))))

(defun interval2 (i s d)
  (do ((nn i (meval `((mplus) ,s ,nn)))
       (m 0 (1+ m))
       (ans))
      ((> m d) (nreverse ans))
    (push nn ans)))

(defun interval (i j)
  (do ((nn i (add2 1 nn))
       (m 0 (1+ m))
       (k (sub* j i))
       (ans))
      ((> m k) (nreverse ans))
    (push nn ans)))

(defmfun $sublist (a f)
  (unless ($listp a)
    (merror (intl:gettext "sublist: first argument must be a list; found: ~M") a) )
  (do ((a (cdr a) (cdr a))
       (x))
      ((null a) (cons '(mlist) (nreverse x)))
    (if (definitely-so (mfuncall f (car a)))
	(push (car a) x))))
