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

(macsyma-module schatc)

;;;; I think this is described in Chapter 3 of J. Moses' thesis,
;;;; "Symbolic Integration", MIT-LCS-TR-047.  A scanned version of the
;;;; thesis is available at
;;;; http://www.lcs.mit.edu/publications/pubs/pdf/MIT-LCS-TR-047.pdf.
;;;;
;;;; Unfortunately, some important pages in the scan are all black.
;;;;
;;;; A version with the missing pages is available (2008-12-14) from
;;;; http://www.softwarepreservation.org/projects/LISP/MIT
;;;;
;;;; Schatchen is Yiddish for "matchmaker" and Schatchen here is a
;;;; pattern matching routine.

(declare-top (special ans))

(defvar *schatfactor* nil)	 ;DETERMINES WHETHER FACTORING SHOULD BE USED.

(defmacro push-context ()
  '(push nil ans))

(defmacro push-loop-context ()
  '(rplacd ans (cons '*loop (cdr ans))))

(defmacro preserve (z)
  `(rplacd ans (cons (cons ,z (cdr ,z)) (cdr ans))))

(defmacro add-to (var val)
  `(rplacd ans (cons (cons ,var ,val) (cdr ans))))

(defmacro var-pat (x)
  `(atom (car ,x)))

;;VARIOUS SIMPLE PATTERNS

(defun free1 (a)
  (declare (special var))
  (and (null (pzerop a)) (free a var)))

(defun not-zero-free (a var)
  (declare (special var))
  (free1 a))

(defun linear* (e var)
  (declare(special var))
  (prog (a n)
     (setq n ($ratcoef e var))
     (when (null (free n var))
       (return nil))
     (setq a (simplus (list '(mplus) e (list '(mtimes) -1 n var)) 1 nil))
     (return (cond ((free a var) (cons a n))))))

(defun dvcoe (e pat args)
  (m1 ($ratsimp (list '(mtimes) e args)) pat))

;;; SCHATCHEN pattern matcher.
;;;
;;; Match the (maxima) expression in E with the pattern given by P.
;;;
;;; The pattern language is partially described in Moses thesis.  We
;;; summarize here some of the main ideas.  (This is mostly taken from
;;; his thesis.)
;;;
;;; A variable in the pattern is written in the form (VAR name pred
;;; arg1 arg2 ... argn)
;;;
;;; where
;;;
;;;   name  = name of variable
;;;   pred  = predicate associated with the variable
;;;   argi  = arguments 2 through n+1 for pred
;;;
;;; The first arg of pred is assumed to the expression that the match
;;; assigns to the variable.
;;;
;;; If the variable has a mode, it is written in prefix form.  Thus
;;; A*x, where A is a number and is a coefficient of plus or times
;;; becomes (coeffpt (var a number) x).
;;;
;;; Some modes:
;;;
;;; coefft - coefficient of TIMES (matches A in A*x) coeffp -
;;; coefficient of PLUS (matches B in x + B) coeffpt - coefficient of
;;; PLUS and TIMES (like coefft and coeffp and matches things like
;;; 2*x^2+sqrt(2)*x^2 so that the coefficient of x^2 is 2+sqrt(2).
;;;
;;; A brief description of the algorithm:
;;;
;;; If E equals P, the match succeeds.
;;;
;;; If P is of the form (VAR name pred arg1 ... argn), then (pred e
;;; arg1 arg2 ... argn) is evaluated.  If the value of the pred is
;;; true, the match succeeds and ((name . e) is appended to the
;;; answer.  Otherwise the match fails.
;;;
;;; If P is of the form (op p1 ... pn) and op is not PLUS, TIMES, or
;;; EXPT, then E must be of the form (op1 e1 ... en) and each pi must
;;; match i1 and op must match op1.  Otherwise the match fails.
;;;
;;; If the pattern is of the form (EXPT p1 p2) then
;;;   1) e is (EXPT e1 e2) and p1 matches e1 and p2 matches e2 or
;;;   2) e is 0 and p1 matches 0 or
;;;   3) e is 1 and
;;;      a) p2 matches 0 or
;;;      b) p1 matches 1
;;;   4) p2 matches 1 and p1 matches e
;;;
;;; Otherwise the match fails
;;;
;;; If the pattern is of the form (op p1 p2 ... pn) and op = PLUS or
;;; TIMES, then if E is not of the form (op e1 ... em), E is
;;; transformed to (op E).  In this case an attempt is made to match
;;; each pi with some ej.  The scan starts with p1 matched with e1.
;;; If that fails p1 is matched with e2.  If pi matches some ej, ej is
;;; deleted (destructively) from E and the scan continues with pi=1
;;; matched with he first subexpression remaining in E.  If for some
;;; pi no ej can be found to match it, then pi is matched with 0 if op
;;; = PLUS or 1 if op = TIMES.  If that also fails, the match fails.
;;; If all the pi have been matched, but some ej have not, the match
;;; fails.
;;;
;;; Exceptions to the above are due to modes.  If op = PLUS, and pi is
;;; of the form (coeffpt (var name pred arg1 ... argn) p1 ... pk),
;;; then the remaining expression is traversed with the pattern
;;; (coefft (var name pred arg1 ... argn) p1 ... pk).  Each
;;; subexpression that is thus matched is deleted from the expression.
;;; The simplified sum of the result of the scan becomes the value of
;;; the variable.  If no subexpression could thuse be matched, then
;;; (pred 0 arg1 ... argn) is attempted.  If this too fails, the match
;;; fails.
;;;
;;; If op = PLUS and pn is of the form (coeffp (var name pred arg1
;;; ... argn), then if e is currently of the form (PLUS ei ... en),
;;; then (pred e arg1 ... argn) is evaluated. If the value of pred is
;;; true, ((name . e)) is appended.  If no subexpressions remain in e,
;;; then pred 0 arg1 ... argn) is attempted.  If it succeeds, ((name
;;; . )) is appended.  Otherwise, the match fails.
;;;
;;; If op = PLUS and pi is of the form (coefft (var name pred arg1
;;; ... argn) p1 ... pk) then (times p1 .... pk) is matched with e.
;;; If the match succeeds and e remains of the form (times e1 ... en),
;;; then (pred e arg1 ... argn) is attempted.  If it fails, the match
;;; fails.  If no subexpressions remain in e, then (pred 1 arg1
;;; ... argn) is attempted.  If this succeeds, ((name . 1) is
;;; appended.

(defmfun schatchen (e p)
  (m2 e p))

;;THE RESTORE FUNCTIONS RESTORE THE SPEC-VAR ANS
;;AND RETURN TRUE OR FALSE AS FOLLOWS
;;RESTORE - FLASE
;;RESTORE1 - TRUE AND CLEARS UP ANS
;;RESTORE2 - TRUE AND CLEARS OFF *LOOP INDICATORS
;;	    DOES NOT FIX UP THE EXPRESSION AND
;;	    IS THUS TO BE USED ONLY INTERNALLY
;;
;;TO INSURE THAT THERE IS NO CONFLICT IN SPECIAL VARIABLES,
;;ESPECIALLY WITH THE VAR* (SET) MODE ALL SCHATCHEN VARIABLES
;;ARE TO BE PRECEEDED BY A "%"

(defvar *splist*)

(defmfun m2 (e p)
  (let ((ans (list nil))
        (*splist* nil))
    (declare (special *splist*))
    (cond ((null (m1 (copy-tree e) p)) nil)
	  ((null (cdr ans)))
	  ((cdr ans)))))

(defun sav&del (x)
  (preserve x)
  (rplacd x (cddr x)))

(defmfun m1 (e p)
  (cond ((equal e p) t)
	((atom p) nil)
	((var-pat p)
	 (push-context)
	 (cond ((testa p e nil)
		(restore1))
	       ((restore))))
	((atom (caar p))
	 (cond ((member 'simp (cdar p) :test #'eq) (alike1 e p))
	       ((member (caar p) '(mplus mtimes) :test #'eq)
		(loopp e p))
	       ((member (caar p) '(mexpt zepow) :test #'eq) (zepow e p t))
	       ((and (not (atom e)) (eq (caar e) (caar p))) (eachp e p))
	       ((eq (caar p) 'coefft) (coefft e p t))
	       ((eq (caar p) 'coeffpt) (coeffpt e p t))
	       ((eq (caar p) 'coeffp) (coeffp e p t))
	       ((eq (caar p) 'coefftt)
		(coefftt e (cadr p) t 'mtimes))
	       ((eq (caar p) 'coeffpp)
		(coefftt e (cadr p) t 'mplus))))
	((var-pat (caar p))	       ;HAIRY OPERATOR MATCHING SCHEME
	 (cond ((atom e) nil)		;NO OPERATOR TO MATCH
	       ((prog2 (push-context)	;BIND THE CONTEXT
		    (testa (caar p) (car e) nil)) ;TRY IT
		(cond ((member (caar e) '(mplus mtimes) :test #'eq) ;CHECK FOR COMMUTIVITY
		       (cond ((loopp e (cons (car e) (cdr p)))
			      (restore1))
			     ((restore))))
		      ((eachp e p)
		       (restore1))
		      ((restore))))
	       ((restore))))))

(defun loopp (e p)
  (prog (x z)
     (setq e (cond  ((atom e) (list (car p) e))
		    ((null (eq (caar p) (caar e)))
		     (cond ((and *schatfactor*
				 (eq (caar e) 'mplus)
				 (mtimesp (setq x ($factor e))))
			    x)
			   ((list (car p) e))))
		    (e)))
     (push-context)
     (setq z p)
     loop (setq z (cdr z))
     (cond ((null z)
	    (return (cond ((null (cdr e)) (restore1))
			  ((restore))))))
     (setq x e)
     l5	(cond ((null (cdr x))
	       (let ((ident (opident (caar p))))
		 (cond ((and ident (m1 ident (car z)))
			(go loop))
		       ((return (restore))))))
	      ((or (atom (car z)) (var-pat (car z)))
	       (when (m1 (cadr x) (car z))
		 (sav&del x)
		 (go loop)))
	      ((eq (caaar z) 'coefft)
	       (cond ((coefft e (car z) nil)
		      (go loop))
		     ((return (restore)))))
	      ((eq (caaar z) 'coeffp)
	       (cond ((coeffp e (car z) nil)
		      (go loop))
		     ((return (restore)))))
	      ((eq (caaar z) 'coeffpt)
	       (cond ((coeffpt e (car z) nil) (go loop))
		     ((return (restore)))))
	      ((eq (caaar z) 'coefftt)
	       (cond ((coefftt e (cadar z) nil 'mtimes) (go loop))
		     ((return (restore)))))
	      ((eq (caaar z) 'coeffpp)
	       (cond ((coefftt e (cadar z) nil 'mplus) (go loop))
		     ((return (restore)))))
	      ((member (caaar z) '(mexpt zepow) :test #'eq)
	       (when (zepow (cadr x) (car z) t)
		 (sav&del x)
		 (go loop)))
	      ((eq (caaar z) 'loop)
	       (cond ((sch-loop e (cdar z)) (go loop))
		     ((return (restore)))))
	      ((m1 (cadr x) (car z))
	       (sav&del x)
	       (go loop)))
     (setq x (cdr x))
     (go l5)))

;;; IND = T MEANS AN INTERNAL CALL (USUALLY FROM LOOPP)

(defun coeffp (e p ind)
  (push-context)
  (cond ((or (and (null (mplusp e)) ;;;WITH IND SET, OR E = (PLUS <EXPR>)
		  (setq e (list '(mplus) e)))
	     ind (null (cddr e)))
	 (coeffport e p 0 ind))	;;; USE COEFFPORT
	((and (null (cddr p))  ;;; P = ((COEFFP) (<VAR> <PRED> . . .))
	      (var-pat (cadr p)))	;;; SO CALL TESTA
	 (cond ((testa (cadr p) e nil)
		(cond ((mplusp e)
		       (preserve e)
		       (rplacd e nil)
		       t)
		      ((merror "COEFFP: incorrect arguments; E=~M, P=~M, IND=~M" e p ind))))))
	((do ((x e (cdr x)))
	     ((null (cdr x))
	      (cond ((m1 0 p) (restore2))
		    ((restore))))
	   (cond ((coeffp (cadr x) p t)
		  (sav&del x)
		  (return (restore2))))))))

(defun coefft (e p ind)
  (push-context)
  (cond ((and (null ind) (null (atom e)) (member (caar e) '(mplus mtimes) :test #'eq))
	 (do ((x e (cdr x)))
	     ((null (cdr x))
	      (cond ((m1 1 p) (restore2))
		    ((restore))))
	   (cond ((coefft (cadr x) p t)
		  (sav&del x)
		  (return (restore2))))))
	((and (mplusp e) (cddr e))
	 (cond ((and *schatfactor* (mtimesp (setq e ($factor e))))
		(coeffport e p 1 ind))
	       ((restore))))
	(t (coeffport (if (mtimesp e) e (list '(mtimes) e)) p 1 ind))))

(defun coeffport (e p ident ind)
  (do ((z (cddr p) (cdr z))
       (x e e))
      ((null z)
       (coeffret e (cadr p) ident ind))
   l	;;; EACH TIME HERE WE HAVE CDR'D DOWN THE EXP.
    (cond ((null (cdr x))
	   (and (null (m1 ident (car z)))
		(return (restore))))
	  ((or (atom (car z))
	       (var-pat (car z))))
	  ((eq (caaar z) 'coefftt)
	   (and (null (coefftt e (cadar z) nil 'mtimes))
		(return (coeffret e p ident ind))))
	  ((eq (caaar z) 'coeffpp)
	   (and (null (coefftt e (cadar z) nil 'mplus))
		(return (coeffret e p ident ind)))))
    (cond ((null (cdr x)))
	  ((m1 (cadr x) (car z))
	   (sav&del x))
	  (t (setq x (cdr x))
	     (go l)))))

(defun coeffret (e p ident ind)
  (cond ((null (cdr e))
	 (cond ((testa p ident nil)
		(cond (ind (restore1))
		      ((restore2))))
	       ((restore))))
	((testa p (cond ((cddr e) (copy-list e ))
			((cadr e)))
		nil)
	 (cond (ind (restore1))
	       (t (preserve e)
		  (rplacd e nil)
		  (restore2))))
	((restore))))

(defun coeffpt (e p ind) ;THE PATTERN LIST (P) MUST BE OF VAR-PATTERNS
  (push-context)
  (do ((z (cond ((mplusp e) e) ((list '(mplus) e))))
       (zz (cons '(coefft) (cdr p)))) ;THIS ROUTINE IS THE ONE WHICH PUTS
					;MOST OF THE THE GARBAGE ON ANS IT
      ((null (cdr z))			;IT CANNOT USE THE *SPLIST* HACK
       (setq z (findit (cond ((eq (caadr p) 'var*) ;BECAUSE IT COULD BE USING
			      (car (cddadr p)))	;MANY DIFFERENT VARIABLES ALTHOUGH
			     ((caadr p))))) ;THOUGHT THE FIRST IS THE ONLY ONE
       (let ((q (cond ((null z) 0)
		      ((null (cdr z)) (car z))
		      ((simplus (cons '(mplus) z) 1 nil))))
	     (fl (if (and z (cdr z)) 'coeffpt))) ;WHICH BECOMES A SUM AND MIGHT BE RESET
	 (cond ((null (testa (cadr p) q fl))
		(restore))
	       (ind (restore1))
	       (t (restore2) q))))
    (cond ((null (m1 (cadr z) zz))	;THIS IS THE DO BODY
	   (setq z (cdr z)))
	  ((sav&del z)))))

(defun zepow (e p fl)		    ;FL=NIL INDICATES A RECURSIVE CALL
    (and fl (push-context))		;SO ANS SHOULD NOT BE MARKED
    (cond ((atom e)
	   (cond ((equal e 1)
		  (cond ((not (or (m1 0 (caddr p)) (m1 1 (cadr p))))
			 (restore))
			((restore1))))
		 ((equal e 0)
		  (cond ((null (m1 0 (cadr p))) (restore))
			((restore1))))
		 ((and (m1 e (cadr p)) (m1 1 (caddr p)))
		  (restore1))
		 ((restore))))
	  ((and *schatfactor*
		(mplusp e)
		(setq e ($factor e))
		nil))
	  ((and (eq (caar e) 'mtimes)
		(mexptp (cadr e)))
	   (do ((e (cddr e) (cdr e))
		(b (cadadr e))
		(x (caddr (cadr e)))
		(z))
	       ((null e)		;OK NOW LETS TRY AGAIN
		(zepow (list '(mexpt) (simplifya b t)
			     (simplifya x t)) p nil))
	     (cond ((mexptp (car e))
		    (cond ((alike1 (cadar e) b)
			   (setq x (simplus (list '(mplus) x (caddar e)) 1 nil)))
			  ((alike1 (caddar e) x)
			   (setq b (simptimes (list '(mtimes) b (cadar e)) 1 nil)))
			  ((signp e (caddr (setq z ($divide x (caddar e)))))
			   (setq b (simptimes (list '(mtimes) b
						    (list '(mexpt) (cadar e)
							  (list '(mtimes) (caddar e) (cadr z)))) 1 nil)))
			  ((return (restore)))))
		   ((alike1 b (car e))
		    (setq x (simplus (list '(mplus) 1 x) 1 t)))
		   ((return (restore))))))
	  ((or (and (eq (caar e) 'mexpt)
		    (m1 (cadr e) (cadr p))
		    (m1 (caddr e) (caddr p)))
	       (and (m1 e (cadr p))
		    (m1 1 (caddr p))))
	   (restore1))
	  ((restore))))

(defun eachp (e p)
  (cond ((= (length e) (length p))
	 (push-context)
	 (do ((e (cdr e) (cdr e)))
	     ((null e) (restore1))
	   (unless (m1 (car e) (cadr p)) (return (restore)))
	   (setq p (cdr p))))))

(defun sch-loop (e lp)
  (push-context) (push-loop-context)
  (do ((x lp) (z e) (y))		;Y A PSEUDO SAVE
      (nil)
    (cond ((null (m1 (cadr z) (car x)))	;DIDN'T MATCH
	   (setq z (cdr z))		;NEXT ARG FOR LOOP
	   (cond ((cdr z))
		 ((eq x lp) (return (restore)))
		 (t
		  (setq x (caar y)
			z (cdar y))
		  (setq y (cdr y)
			ans (cdr ans))
		  (pop-loop-context))))
	  (t
	   (push (cons x z) y)
	   (sav&del z)
	   (setq x (cdr x))
	   (cond ((null x) (return (restore2)))
		 (t (push-loop-context)
		    (setq z e)))))))

(defun coefftt (exp pat ind opind)	;OPIND IS MPLUS OR MTIMES
  (push-context)
  (when (or (atom exp) (and ind (not (eq (caar exp) opind))))
    (setq exp (list (list opind) exp)))
  (push (car pat) *splist*)		;SAVE VAR NAME HERE
  (do ((z exp) (res))
      ((null (cdr z))
       (setq *splist* (cdr *splist*))	;KILL NAME SAVED
       (cond (res (setq res (cond ((cdr res) (cons (list opind) res))
				  ((car res))))
		  (cond ((and (eq (car pat) 'var*)
			      (member 'set (cadr pat) :test #'eq))
			 (add-to (caddr pat) (setf (symbol-value (caddr pat)) (simplifya res nil))))
			((add-to (car pat) (simplifya res nil))))
		  (cond (ind (restore1))
			((restore2))))
	     ((null (testa pat (opident opind) nil))
	      (restore))
	     (ind (restore1))
	     ((restore2))))
    (cond ((testa pat (cadr z) nil)
	   (push (cadr z) res)
	   (sav&del z))
	  (t (setq z (cdr z))))))

(defun restore nil
  (do ((y (cdr ans) (cdr y)))
      ((null y) nil)
    (cond ((eq (car y) '*loop)
	   (rplaca y (cadr y))
	   (rplacd y (cddr y)))
	  ((null (car y))
	   (setq ans y)
	   (return nil))
	  ((null (atom (caar y)))
	   (rplacd (caar y) (cdar y))))))

(defun restore1 nil
  (do ((y ans) (l))			;L IS A LIST OF VAR'S NOTED
      ((null (cdr y)) t)
    (cond ((null (cadr y))		;END OF CONTEXT
	   (rplacd y (cddr y))		;SPLICE OUT THE CONTEXT MARKER
	   (return t))
	  ((not (atom (caadr y)))	;FIXUP NECESSARY
	   (rplacd (caadr y) (cdadr y))
	   (rplacd y (cddr y)))
	  ((member (car y) l :test #'eq)	       ;THIS VAR HAS ALREADY BEEN SEEN
	   (rplacd y (cddr y)))	   ;SO SPLICE IT OUT TO KEEP ANS CLEAN
	  ((setq y (cdr y)
		 l (cons (caar y) l))))))

(defun restore2 nil
  (do ((y (cdr ans) (cdr y)))
      ((null (cdr y)) t)
    (cond ((eq (cadr y) '*loop)
	   (rplacd y (cddr y)))
	  ((null (cadr y))
	   (rplacd y (cddr y))
	   (return t)))))

(defun pop-loop-context nil
  (do ((y ans))
      ((eq (cadr y) '*loop) nil)
    (or (atom (caadr y))
	(rplacd (caadr y) (cdadr y)))
    (rplacd y (cddr y))))

;;WHEN THE CAR OF ALA IS VAR* THE CADR IS A LIST OF
;;THE VARIOUS SWITCHES WHICH MAY BE SET.
;;UVAR- INDICATES THIS SHOULD MATCH SOMETHING WHICH IS ALREADY ON ANS.
;;SET - ACTUALLY SET THIS VARIABLE TO ITS VALUE IF IT MATCHES.
;;COEFFPT - SPECIAL ARGUMENT IF IN COEFFPT.

(defun testa (ala exp b)
  (cond ((eq (car ala) 'mvar*)
	 (testa* ala exp t))
	((eq (car ala) 'var*)
	 (do ((z (cadr ala) (cdr z))
	      (ala (cddr ala))
	      (y) (set) (uvar))
	     ((null z)
	      (setq y (cond (uvar (m1 exp y))
			    ((testa* ala exp nil))))
	      (cond ((null y) nil)
		    (set (setf (symbol-value (car ala)) exp))
		    (y)))
	   (cond ((eq (car z) 'set) (setq set t))
		 ((eq (car z) 'uvar)
		  (cond ((setq y (cdr (assoc (car ala) ans :test #'equal)))
			 (setq uvar t))))
		 ((eq (car z) 'coeffpt)
		  (and (eq b 'coeffpt)
		       (setq ala (cadr z)))
		  (setq z (cdr z)))
		 ((merror "TESTA: invalid switch ~M in pattern." (car z))))))
	((testa* ala exp nil))))

;; ALA IS THE PREDICATE LIST (VAR PREDFN ARG2 ARG3 ARG4 . . .)

(defun testa* (ala exp loc)
  (declare (special var))
  (cond ((cond ((eq (cadr ala) 'freevar)
		(cond ((eq var '*novar) (equal exp 1))
		      ((free exp var))))
	       ((eq (cadr ala) 'numberp) (mnump exp))
	       ((eq (cadr ala) 'true) t)
	       ((eq (cadr ala) 'linear*)
		(setq exp (linear* exp (caddr ala))))
	       ((null loc)
		(cond ((atom (cadr ala))
		       (cond ((fboundp (cadr ala))
			      (apply (cadr ala)
				     (findthem exp (cddr ala))))
			     ((mget (cadr ala) 'mexpr)
			      (mapply (cadr ala)
				      (findthem exp (cddr ala))
				      (cadr ala)))))
		      ((member (caadr ala) '(lambda function *function quote) :test #'eq)
			     ;;;THE LAMBDA IS HERE ONLY BECAUSE OF SIN!!!
		       (apply (cadr ala) (findthem exp (cddr ala))))
		      ((eval-pred (cadr ala) (car ala) exp)))))
	 (cond ((member (car ala) *splist* :test #'eq))
	       ((add-to (car ala) exp))))
	((cond ((and loc (atom (cadr ala))
		     (fboundp (cadr ala)))
		(mapc #'(lambda (q v) (and (null (member q *splist* :test #'eq))
					   (add-to q v)))
		      (car ala)
		      (apply (cadr ala) (findthem exp (cddr ala)))))))))

(defun eval-pred (exp %var value)
  (progv (list %var) (list value)
    (eval exp)))

(defun findthem (exp args)
  (cons exp
	(mapcar #'(lambda (q)
		    (cond ((atom q)
			   (or (cdr (assoc q ans :test #'eq))
			       ;; Evaluate a symbol which has a value.
			       (and (symbolp q) (boundp q) (symbol-value q))
			       ;; Otherwise return the symbol.
			       q))
			  (q)))
		args)))

(defun findit (a)
  (do ((y ans) (z))
      ((or (null (cdr y)) (null (cadr y))) z)
    (cond ((eq (caadr y) a)
	   (setq z (nconc z (list (cdadr y))))
	   (rplacd y (cddr y)))
	  ((setq y (cdr y))))))

(defun sch-replace (dict exp1)
  (declare (special dict))
  (replac exp1))

(defun replac (exp1)
  (declare (special dict))
  (let ((w1 nil))
    (cond ((null exp1) nil)
	  ((not (atom exp1))
	   (cond ((eq (car exp1) 'eval)
		  (simplifya (eval (replac (cadr exp1))) nil))
		 ((eq (car exp1) 'quote) (cadr exp1))
		 (t (setq w1 (mapcar #'replac (cdr exp1)))
		    (cond ((equal w1 (cdr exp1))
			   exp1)
			  ((simplifya (cons (list (caar exp1)) w1) t))))))
	  ((numberp exp1) exp1)
	  ((setq w1 (assoc exp1 dict :test #'eq))
	   (cdr w1))
	  (exp1))))

(declare-top (unspecial var ans))
