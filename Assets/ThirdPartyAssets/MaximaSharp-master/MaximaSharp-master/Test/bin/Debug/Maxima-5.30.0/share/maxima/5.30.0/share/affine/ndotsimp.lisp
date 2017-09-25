;;; -*- Package: CL-MAXIMA; Mode: LISP; Syntax: Common-lisp -*-
(in-package :maxima)

;(defun $check_associative (a b c &aux tem tem1 tem2)
;;  (show (dotsimp (n. a b)))
;;  (show (dotsimp (n. b c)))
;;  (show (n. a (dotsimp (n. b c))))
;;  (show (setq hee(n.  (dotsimp (n. a b)) c)))
;  (setq tem1 (new-dotsimp(n. (new-dotsimp (n. a b)) c)))
;  (setq tem2 (new-dotsimp(n. a (new-dotsimp (n. b c)))))
;;  (show tem1 tem2)
;  (setq tem (n- tem1 tem2))
;  (cond ((pzerop tem) tem)
;	(t (header-poly tem))))

(defun $check_associative (a b c &aux tem tem1 tem2)
  (setq tem1 (n. (new-dotsimp (n. a b)) c))
  (setq tem2 (n. a (new-dotsimp (n. b c))))
  (setq tem (n- tem1 tem2))
  (setq tem (new-dotsimp tem))
  (cond ((pzerop tem) tem)
	(t (header-poly tem))))

(defun ncdot-list (lis)
  (cond ((null lis) 1)
	((null (cdr lis))
	 (car lis))
	(t (cons '(mnctimes) lis))))

(defun dot-subword (small big &aux tem leng)
  "(dot-subword #$x$ #$z.u.x.y.z.z.y$) ==> (list #$z.u$ #$y.z.z.y$))
  (dot-subword #$x.y$ #$z.u.x.y.z.z.y$) ==> (list #$z.u$ #$z.z.y$))
  (dot-subword #$z.u.x.y$ #$z.u.x.y.z.z.y$) ==> (list #$1$ #$z.z.y$))"
  (cond ((atom big)
	 (cond ((eql big small) (list 1  1))))
	((atom small)
	 (cond ((setq tem (member small big :test #'eq))
		(list (ncdot-list (subseq (cdr big) 0 (- (length big) (length tem) 1)))
		      (ncdot-list (cdr tem))))))
	(t (setq leng (-  (length small) 1))
	   (loop for v on (cdr big)
		 when ;;first part of v is equal to small
		 (loop
		   initially (cond ((> leng (length v)) (return nil)))
		   for vv in v
		   for w in (cdr small)
		   when (not (equal w vv))
		   do (return nil)
		   finally (return t))
		 do (return (list  (ncdot-list
				     (subseq (cdr big) 0 (- (length big) (length v) 1)))
				   (ncdot-list (nthcdr (length (cdr small)) v))))))))

(defun split-numerator (reduced rest)
  "splits into two polynomials, the first one needs no replacement and the second has its leading term needing
   replacement"
  (cond ((pzerop rest) (values reduced 0))
	((poly-scalarp rest) (values (n+ rest reduced) 0))
	(($must_replacep (get (p-var rest) 'disrep))
	 (values reduced rest))
	(t (split-numerator  (n+ (subseq rest 0 3) reduced) (or (fifth rest) 0)))))

(defun new-dotsimp (ratl-fun &aux mon repl num den)
  (format t "~%Beginning to simplify:")
  (sh ratl-fun)
  (loop
     with expr = ratl-fun with answer = 0
     when (pzerop expr) do (return answer)
     do   (setq-num-den num den expr)
       (cond ((poly-scalarp num)(setq answer (n+ answer expr))
	      (format t "~%Final answer:")
	      (sh answer)
	      (return answer))
	     (($must_replacep (setq mon (get (p-var num) 'disrep)))
	      (cond (*verbose-check-overlaps*
		     (format t "~%Simplifying the worst monomial ")
		     (dot-show mon)))
	      (setq repl (simp-once-monomial mon))
	      (setq repl (n* (p-cof num) repl))
	      (cond ((fifth num)
		     (setq num (n+ repl (fifth num))))
		    (t (setq num repl)))
	      (setq expr (nred num den)))
	     (t (multiple-value-bind
		      (reduced rest)
		    (split-numerator 0 num)
		  (setq answer (n+ (nred reduced den) answer))
		  (setq expr (nred rest den))
		  (cond (*verbose-check-overlaps*
			 (format t "~%Simplifying the worst monomial ")
			 (dot-show mon) (format t " adding  to the answer" )))
		  )))
					;      (format t "~%Expr:") (sh expr)
					;      (format t "~%Reduced part:") (sh answer)
       ))

;;;old reliable
;(defun new-dotsimp (ratl-fun &aux mon repl num den)
;  (progn
;    (loop
;      with expr = ratl-fun with answer = 0
;      when (pzerop expr) do (return answer)
;      do   (setq-num-den num den expr)
;      (cond ((poly-scalarp num)(setq answer (n+ answer expr))
;			       (return answer))
;	    (($must_replacep (setq mon (get (p-var num) 'disrep)))
;	     (format t "~%Simplifying the worst monomial ") (dot-show mon)
;	     (setq repl (simp-once-monomial mon))
;	     (setq repl (n* (p-cof num) repl))
;	     (cond ((fifth num)
;		    (setq num (n+ repl (fifth num))))
;		   (t (setq num repl)))
;	     (setq expr (nred num den)))
;	    (t (setq answer (n+ (nred (firstn 3 num) den) answer))
;	       (format t "~%Simplifying the worst monomial ")
;	       (dot-show mon) (format t " adding to the answer")
;	       (cond ((fifth num)
;		      (setq expr (nred (fifth num) den)))
;		     (t (setq expr 0)))))
;      )))

(defun force-poly (repl)
  (cond ((numberp repl) repl)
	(t (cdr repl))))

(defvar $dot_eps nil)

(defun simp-once-monomial (monom &aux tem)
  (cond ((atom monom)
	 (loop for (mon repl) on (cdr $dot_simplifications) by #'cddr
	       when (eql mon monom)
	       do (return (force-poly repl))))
	((and $dot_eps
	     (setq tem (member '$eps monom :test 'eq))
	     (member'$eps (cdr tem) :test 'eq))
	 0)
	(t (loop for (mon repl) on (cdr $dot_simplifications) by #'cddr
		when (setq tem (dot-subword mon monom))
		do
		(return (n. (n.  (first tem) (force-poly repl)) (second tem)))
		finally (return (st-rat monom))))))

(defun $dotsimp (expr)
  (declare (special $new_fast_dotsimp))
  (cond (($listp expr) (cons '(mlist) (mapcar '$dotsimp (cdr expr))))
	(t
	 (cond ((or (rational-functionp expr)(polynomialp expr)) nil)
	       (t (setq expr (new-rat expr))))
	 (header-poly (new-dotsimp expr)))))




(defun $dot_factor (form variables &optional (slot 1))
"Form is a non commutative polynomial, and variables a list of non commutative variables.
Form is split into a list of forms of length variables, so that
variables.dot_factor(form) = form "

  (check-arg variables $listp "macsyma list")
  (let* ((fo (st-rat form))
	 (vari (list-variables fo))
	 (result (make-list (length variables) :initial-element 0))
	 pos vv)
    (loop for v in vari
	  do (setq vv (get v 'disrep))
	  (cond ((not (symbolp vv))
		 (setq pos (position (nth slot vv) variables))
		 (cond (pos (setf (nth pos result)
				  (n+ (nth pos result) (n* (pcoeff fo v) (st-rat
							   (meval* (cons '(mnctimes)
								     (let ((mon (copy-list (cdr vv))))
									(setf (nth (1- slot) mon) 1) mon))))))))
		       (t (fsignal "does not have a factor in slot"))))
		((setq pos (position vv variables))
		 (cond (pos (setf (nth pos result)
				  (n+ (nth pos result) (pcoeff fo (list v 1 1)))))))))
    (cons '(mlist) (mapcar 'new-disrep (cdr result)))))
