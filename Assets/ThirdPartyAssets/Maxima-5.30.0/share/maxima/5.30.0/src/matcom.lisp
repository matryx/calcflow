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

(macsyma-module matcom)

;; This is the Match Compiler.

(declare-top (special $rules $props boundlist reflist topreflist program))

(defvar *afterflag nil)

(defmvar $announce_rules_firing nil)

(defmspec $matchdeclare (form)
  (let ((meta-prop-p nil))
    (proc-$matchdeclare (cdr form))))

(defun proc-$matchdeclare (x)
  (if (oddp (length x))
      (merror (intl:gettext "matchdeclare: must be an even number of arguments.")))
  (do ((x x (cddr x))) ((null x))
    (cond ((symbolp (car x))
	   (cond ((and (not (symbolp (cadr x)))
		       (or (numberp (cadr x))
			   (member (caaadr x) '(mand mor mnot mcond mprog) :test #'eq)))
		  (improper-arg-err (cadr x) '$matchdeclare)))
	   (meta-add2lnc (car x) '$props)
	   (meta-mputprop (car x) (ncons (cadr x)) 'matchdeclare))
	  ((not ($listp (car x)))
	   (improper-arg-err (car x) '$matchdeclare))
	  (t (do ((l (cdar x) (cdr l))) ((null l))
	       (proc-$matchdeclare (list (car l) (cadr x)))))))
  '$done)

(defun compileatom (e p) 
  (prog (d) 
     (setq d (getdec p e))
     (return (cond ((null d)
		    (emit (list 'cond
				(list (list 'not
					    (list 'equal
						  e
						  (list 'quote p)))
				      '(matcherr)))))
		   ((member p boundlist :test #'eq)
		    (emit (list 'cond
				(list (list 'not (list 'equal e p))
				      '(matcherr)))))
		   (t (setq boundlist (cons p boundlist)) (emit d))))))

(defun emit (x) (setq program (nconc program (list x))))

(defun memqargs (x)
  (cond ((or (numberp x) (member x boundlist :test #'eq)) x)
	((and (symbolp x) (get x 'operators)) `(quote ,x))
	;; ((NULL BOUNDLIST) (LIST 'SIMPLIFYA (LIST 'QUOTE X) NIL))
	(t `(meval (quote ,x)))))

(defun makepreds (l gg) 
  (cond ((null l) nil)
	(t (cons (cond ((atom (car l))
			(list 'lambda (list (setq gg (gensym)))
			      `(declare (special ,gg))
			      (getdec (car l) gg)))
		       (t (defmatch1 (car l) (gensym))))
		 (makepreds (cdr l) nil)))))

(defun defmatch1 (pt e) 
  (prog (topreflist program prog-variables) 
     (setq topreflist (list e))
     (cond ((atom (errset (compilematch e pt)))
	    (merror (intl:gettext "defmatch: failed to compile match for pattern ~M") pt))
	   (t
         ;; NOTE TO TRANSLATORS: MEANING OF FOLLOWING TEXT IS UNKNOWN
         (mtell "defmatch: ~M will be matched uniquely since sub-parts would otherwise be ambigious.~%" pt)
	      (return (list 'lambda
			    (list e)
			    `(declare (special ,e))
			    (list 'catch ''match
				  (nconc (list 'prog)
					 (list (setq prog-variables (cdr (reverse topreflist))))
                     `((declare (special ,@ prog-variables)))
					 program
					 (list (list 'return t))))))))))

(defun compileplus (e p) 
  (prog (reflist f g h flag leftover) 
   a    (setq p (cdr p))
   a1   (cond ((null p)
	       (cond ((null leftover)
		      (return (emit (list 'cond
					  (list (list 'not (list 'equal e 0.))
						'(matcherr))))))
		     ((null (cdr leftover)) (return (compilematch e (car leftover))))
		     ((setq f (intersection leftover boundlist :test #'equal))
		      (emit (list 'setq
				  e
				  (list 'meval
					(list 'quote
					      (list '(mplus)
						    e
						    (list '(mminus) (car f)))))))
		      (setq leftover (delete (car f) leftover :test #'equal))
		      (go a1))
		     (t
		      ;; Almost nobody knows what this means. Just suppress the noise.
		      ;; (mtell "COMPILEPLUS: ~M partitions '+' expression.~%" (cons '(mplus) leftover))
		      (setq boundlist (append boundlist (atomson leftover)))
		      (return (emit (list 'cond
					  (list (list 'part+
						      e
						      (list 'quote leftover)
						      (list 'quote
							    (makepreds leftover nil))))
					  '(t (matcherr))))))))
	      ((fixedmatchp (car p))
	       (emit (list 'setq
			   e
			   (list 'meval
				 (list 'quote
				       (list '(mplus)
					     e
					     (list '(mminus) (car p))))))))
	      ((atom (car p))
	       (cond ((cdr p) (setq leftover (cons (car p) leftover)) (setq p (cdr p)) (go a1))
		     (leftover (setq leftover (cons (car p) leftover)) (setq p nil) (go a1)))
	       (setq boundlist (cons (car p) boundlist))
	       (emit (getdec (car p) e))
	       (cond ((null (cdr p)) (return nil)) (t (go a))))
	      ((eq (caaar p) 'mtimes)
	       (cond ((and (not (or (numberp (cadar p))
				    (and (not (atom (cadar p)))
					 (eq (caar (cadar p)) 'rat))))
			   (fixedmatchp (cadar p)))
		      (setq flag nil)
		      (emit `(setq ,(genref)
			      (ratdisrep
			       (ratcoef ,e ,(memqargs (cadar p))))))
		      (compiletimes (car reflist) (cons '(mtimes) (cddar p)))
		      (emit `(setq ,e (meval
				       (quote
					(($ratsimp)
					 ((mplus) ,e
					  ((mtimes) -1 ,(car reflist)
					   ,(cadar p)))))))))
		     ((null flag)
		      (setq flag t) (rplacd (car p) (reverse (cdar p))) (go a1))
		     (t (setq leftover (cons (car p) leftover)) (go a))))
	      ((eq (caaar p) 'mexpt)
	       (cond ((fixedmatchp (cadar p))
		      (setq f 'findexpon)
		      (setq g (cadar p))
		      (setq h (caddar p)))
		     ((fixedmatchp (caddar p))
		      (setq f 'findbase)
		      (setq g (caddar p))
		      (setq h (cadar p)))
		     (t (go functionmatch)))
	       (emit (list 'setq
			   (genref)
			   (list f e (setq g (memqargs g)) ''mplus)))
	       (emit (list 'setq
			   e
			   (list 'meval
				 (list 'quote
				       (list '(mplus)
					     e
					     (list '(mminus)
						   (cond ((eq f 'findexpon)
							  (list '(mexpt)
								g
								(car reflist)))
							 (t (list '(mexpt)
								  (car reflist)
								  g)))))))))
	       (compilematch (car reflist) h))
	      ((not (fixedmatchp (caaar p)))
	       (cond ((cdr p)
		      (setq leftover (cons (car p) leftover))
		      (setq p (cdr p))
		      (go a1))
                      (leftover (setq leftover (cons (car p) leftover)) (setq p nil) (go a1)))
	       (setq boundlist (cons (caaar p) boundlist))
	       (emit (list 'msetq
			   (caaar p)
			   (list 'kaar (genref))))
	       (go functionmatch))
	      (t (go functionmatch)))
   (go a)
   functionmatch
   (emit (list 'setq
	       (genref)
	       (list 'findfun e (memqargs (caaar p)) ''mplus)))
   (cond ((eq (caaar p) 'mplus)
	  (mtell (intl:gettext "COMPILEPLUS: warning: '+' within '+' in: ~M~%") (car p))
	  (compileplus (car reflist) (car p)))
	 (t (emit (list 'setq (genref) (list 'kdr (cadr reflist))))
	    (compileeach (car reflist) (cdar p))))
   (emit (list 'setq
	       e
	       (list 'meval
		     (list 'quote
			   (list '(mplus) e (list '(mminus) (car p)))))))
   (go a)))

(defun compiletimes (e p) 
  (prog (reflist f g h leftover) 
   a    (setq p (cdr p))
   a1   (cond ((null p)
	       (cond ((null leftover)
		      (return (emit (list 'cond
					  (list (list 'not (list 'equal e 1.))
						'(matcherr))))))
		     ((null (cdr leftover)) (return (compilematch e (car leftover))))
		     ((setq f (intersection leftover boundlist :test #'equal))
		      (emit (list 'setq
				  e
				  (list 'meval
					(list 'quote
					      (list '(mquotient) e (car f))))))
		      (setq leftover (delete (car f) leftover :test #'equal))
		      (go a1))
		     (t
		      ;; Almost nobody knows what this means. Just suppress the noise.
		      ;; (mtell "COMPILETIMES: ~M partitions '*' expression.~%" (cons '(mtimes) leftover))
		      (setq boundlist (append boundlist (atomson leftover)))
		      (return (emit (list 'cond
					  (list (list 'part*
						      e
						      (list 'quote leftover)
						      (list 'quote
							    (makepreds leftover nil))))
					  '(t (matcherr))))))))
	      ((fixedmatchp (car p))
	       (emit (list 'setq
			   e
			   (list 'meval
				 (list 'quote (list '(mquotient) e (car p)))))))
	      ((atom (car p))
	       (cond ((cdr p) (setq leftover (cons (car p) leftover)) (setq p (cdr p)) (go a1))
		     (leftover (setq leftover (cons (car p) leftover)) (setq p nil) (go a1)))
	       (setq boundlist (cons (car p) boundlist))
	       (emit (getdec (car p) e))
	       (cond ((null (cdr p)) (return nil)) (t (go a))))
	      ((eq (caaar p) 'mexpt)
	       (cond ((fixedmatchp (cadar p))
		      (setq f 'findexpon)
		      (setq g (cadar p))
		      (setq h (caddar p)))
		     ((fixedmatchp (caddar p))
		      (setq f 'findbase)
		      (setq g (caddar p))
		      (setq h (cadar p)))
		     (t (go functionmatch)))
	       (emit (list 'setq
			   (genref)
			   (list f e (setq g (memqargs g)) ''mtimes)))
	       (cond ((eq f 'findbase)
		      (emit (list 'cond
				  (list (list 'equal (car reflist) 0)
					'(matcherr))))))
	       (emit (list 'setq
			   e
			   (list 'meval
				 (list 'quote
				       (list '(mquotient)
					     e
					     (cond ((eq f 'findexpon)
						    (list '(mexpt) g (car reflist)))
						   (t (list '(mexpt)
							    (car reflist)
							    g))))))))
	       (compilematch (car reflist) h))
	      ((not (fixedmatchp (caaar p)))
	       (cond ((cdr p)
		      (setq leftover (cons (car p) leftover))
		      (setq p (cdr p))
		      (go a1))
                     (leftover (setq leftover (cons (car p) leftover)) (setq p nil) (go a1)))
	       (setq boundlist (cons (caaar p) boundlist))
	       (emit (list 'msetq
			   (caaar p)
			   (list 'kaar (genref))))
	       (go functionmatch))
	      (t (go functionmatch)))
   (go a)
   functionmatch
   (emit (list 'setq
	       (genref)
	       (list 'findfun e (memqargs (caaar p)) ''mtimes)))
   (cond ((eq (caaar p) 'mtimes)
	  (mtell (intl:gettext "COMPILETIMES: warning: '*' within '*' in: ~M~%") (car p))
	  (compiletimes (car reflist) (car p)))
	 (t (emit (list 'setq (genref) (list 'kdr (cadr reflist))))
	    (compileeach (car reflist) (cdar p))))
   (emit (list 'setq
	       e
	       (list 'meval
		     (list 'quote (list '(mquotient) e (car p))))))
   (go a)))


(defmspec $defmatch (form)
  (let ((meta-prop-p nil))
    (proc-$defmatch (cdr form))))

(defun proc-$defmatch (l) 
  (prog (pt pt* args a boundlist reflist topreflist program name tem) 
     (setq name (car l))
     (setq pt (copy-tree (setq pt* (simplify (cadr l)))))
     (cond ((atom pt)
	    (setq pt (copy-tree (setq pt* (meval pt))))
	    (mtell (intl:gettext "defmatch: evaluation of atomic pattern yields: ~M~%") pt)))
     (setq args (cddr l))
     (cond ((null (allatoms args)) (mtell (intl:gettext "defmatch: some pattern variables are not atoms."))
	    (return nil)))
     (setq boundlist args)
     (setq a (genref))
     (cond ((atom (errset (compilematch a pt)))
	    (merror (intl:gettext "defmatch: failed to compile match for pattern ~M") pt))
	   (t (meta-fset name
			 (list 'lambda
			       (cons a args)
			       `(declare (special ,a ,@ args))
			       (list 'catch ''match
				     (nconc (list 'prog)
					    (list (setq tem  (cdr (reverse topreflist))))
					    `((declare (special ,@ tem)))
					    program
					    (list (list 'return
							(cond (boundlist (cons 'retlist
									       boundlist))
							      (t t))))))))
	      (meta-add2lnc name '$rules) 
	      (meta-mputprop name (list '(mlist) pt* (cons '(mlist) args)) '$rule)
	      (return name)))))


(defun atomson (l) 
  (cond ((null l) nil)
	((atom (car l)) (cons (car l) (atomson (cdr l))))
	(t (atomson (cdr l)))))


(defmspec $tellsimp (form)
  (let ((meta-prop-p nil))
    (proc-$tellsimp (cdr form))))

(defun $clear_rules ()
  (mapc 'kill1 (cdr $rules))
  (loop for v in '(mexpt mplus mtimes)
	 do (setf (mget v 'rulenum) nil)))

(defun proc-$tellsimp (l) 
  (prog (pt rhs boundlist reflist topreflist a program name tem
	 oldstuff pgname oname rulenum) 
     (setq pt (copy-tree (simplifya (car l) nil)))
     (setq name pt) 
     (setq rhs (copy-tree (simplifya (cadr l) nil)))
     (cond ((alike1 pt rhs) (merror (intl:gettext "tellsimp: circular rule attempted.")))
	   ((atom pt) (merror (intl:gettext "tellsimp: pattern must not be an atom; found: ~A") (fullstrip1 (getop name))))
	   ((mget (setq name (caar pt)) 'matchdeclare)
	    (merror (intl:gettext "tellsimp: main operator of pattern must not be match variable; found: ~A") (fullstrip1 (getop name))))
	   ((member name '(mplus mtimes) :test #'eq)
	    (mtell (intl:gettext "tellsimp: warning: putting rules on '+' or '*' is inefficient, and may not work.~%"))))
     (setq a (genref))
     (cond ((atom (errset (compileeach a (cdr pt))))
	    (merror (intl:gettext "tellsimp: failed to compile match for pattern ~M") (cdr pt))))
     (setq oldstuff (get name 'operators))
     (setq rulenum (mget name 'rulenum))
     (cond ((null rulenum) (setq rulenum 1.)))
     (setq oname (getop name))
     (setq pgname (implode (append (%to$ (explodec oname))
				   '(|r| |u| |l| |e|)
				   (mexploden rulenum))))
     (meta-mputprop pgname name 'ruleof)
     (meta-add2lnc pgname '$rules)
     (meta-mputprop name (f1+ rulenum) 'rulenum)
     (meta-fset pgname
		(list 'lambda '(x a2 a3)
		      `(declare (special x a2 a3))
		      (list 'prog
			    (list 'ans a 'rule-hit)
			    `(declare (special ans ,a))
			    (list 'setq
				  'x
				  (list 'cons
					'(car x)
					(list 'setq
					      a
					      '(cond (a3 (cdr x)) 
						(t (mapcar #'(lambda (h) (simplifya h a3))
						    (cdr x)))))))
			    (list
			     'multiple-value-setq
			     '(ans rule-hit)
			     (list 'catch ''match
				   (nconc (list 'prog)
					  (list (setq tem (nconc boundlist
								 (cdr (reverse topreflist)))))
					  `((declare (special ,@ tem)))
					  program
					  (list (list 'return
						      (list 'values (memqargs rhs) t))))))
			    (cond ((not (member name '(mtimes mplus) :test #'eq))
				   (list 'return
					 (list 'cond
					       '(rule-hit ans) '((and (not dosimp) (member 'simp (cdar x) :test #'eq))x)
					       (list t
						     (cond (oldstuff (cons oldstuff
									   '(x a2 t)))
							   (t '(eqtest x x)))))))
				  ((eq name 'mtimes)
				   (list 'return
					 (list 'cond
					       (list '(and (equal 1. a2) rule-hit) 'ans)
					       '(rule-hit (meval '((mexpt) ans a2)))
					       (list t
						     (cond (oldstuff (cons oldstuff
									   '(x a2 a3)))
							   (t '(eqtest x x)))))))
				  ((eq name 'mplus)
				   (list 'return
					 (list 'cond
					       (list '(and (equal 1. a2) rule-hit) 'ans)
					       '(rule-hit (meval '((mtimes) ans a2)))
					       (list t
						     (cond (oldstuff (cons oldstuff
									   '(x a2 a3)))
							   (t '(eqtest x x)))))))))))
     (meta-mputprop pgname (list '(mequal) pt rhs) '$rule)
     (cond ((null (mget name 'oldrules))
	    (meta-mputprop name
			   (list (get name 'operators))
			   'oldrules)))
     (meta-putprop name pgname 'operators)
     (return (cons '(mlist)
		   (meta-mputprop name
				  (cons pgname (mget name 'oldrules))
				  'oldrules)))))

(defun %to$ (l) (cond ((eq (car l) '%) (rplaca l '$)) (l)))


(defmspec $tellsimpafter (form)
  (let ((meta-prop-p nil))
    (proc-$tellsimpafter (cdr form))))

(defun proc-$tellsimpafter (l) 
  (prog (pt rhs boundlist reflist topreflist a program name oldstuff plustimes pgname oname tem
	 rulenum) 
     (setq pt (copy-tree (simplifya (car l) nil)))
     (setq name pt)
     (setq rhs (copy-tree (simplifya (cadr l) nil)))
     (cond ((alike1 pt rhs) (merror (intl:gettext "tellsimpafter: circular rule attempted.")))
	   ((atom pt) (merror (intl:gettext "tellsimpafter: pattern must not be an atom; found: ~A") (fullstrip1 (getop name))))
	   ((mget (setq name (caar pt)) 'matchdeclare)
	    (merror (intl:gettext "tellsimpafter: main operator of pattern must not be match variable; found: ~A") (fullstrip1 (getop name)))))
     (setq a (genref))
     (setq plustimes (member name '(mplus mtimes) :test #'eq))
     (if (atom (if plustimes (errset (compilematch a pt))
		   (errset (compileeach a (cdr pt)))))
	 (merror (intl:gettext "tellsimpafter: failed to compile match for pattern ~M") (cdr pt)))
     (setq oldstuff (get name 'operators))
     (setq rulenum (mget name 'rulenum))
     (if (null rulenum) (setq rulenum 1))
     (setq oname (getop name))
     (setq pgname (implode (append (%to$ (explodec oname))
				   '(|r| |u| |l| |e|) (mexploden rulenum))))
     (meta-mputprop pgname name 'ruleof)
     (meta-add2lnc pgname '$rules)
     (meta-mputprop name (f1+ rulenum) 'rulenum)
     (meta-fset
      pgname
      (list
       'lambda
       '(x ans a3)
       (if oldstuff
         (list 'setq 'x (list oldstuff 'x 'ans 'a3))
         (list 'setq 'x (list 'simpargs1 'x 'ans 'a3)))
       (list
	'cond
	'(*afterflag x)
	(list 't
	      (nconc (list 'prog)
		     (list (cons a '(*afterflag rule-hit)))
		     `((declare (special ,a *afterflag)))
		     (list '(setq *afterflag t))
		     (cond (oldstuff (subst (list 'quote name)
					    'name
					    '((cond ((or (atom x) (not (eq (caar x) name)))
						     (return x)))))))
		     (list (list 'setq
				 a
				 (cond (plustimes 'x) (t '(cdr x)))))
		     (list (list 'multiple-value-setq
				 '(ans rule-hit)
				 (list 'catch ''match
				       (nconc (list 'prog)
					      (list (setq tem(nconc boundlist
								    (cdr (reverse topreflist)))))
					      `((declare (special ,@ tem)))
					      program
                          (cond
                            ($announce_rules_firing
                              (list (list 'return (list 'values (list 'announce-rule-firing `',pgname 'x (memqargs rhs)) t))))
                            (t
                              (list (list 'return (list 'values (memqargs rhs) t)))))))))
		     (list '(return (if rule-hit ans (eqtest x x)))))))))
     (meta-mputprop pgname (list '(mequal) pt rhs) '$rule)
     (cond ((null (mget name 'oldrules))
	    (meta-mputprop name (list (get name 'operators)) 'oldrules)))
     (meta-putprop name pgname 'operators)
     (return (cons '(mlist)
		   (meta-mputprop name
				  (cons pgname (mget name 'oldrules))
				  'oldrules)))))

(defun announce-rule-firing (rulename expr simplified-expr)
  (let (($display2d nil) ($stringdisp nil))
    ($print "By" rulename "," expr "-->" simplified-expr))
  simplified-expr)

(defmspec $defrule (form)
  (let ((meta-prop-p nil))
    (proc-$defrule (cdr form))))

;;(defvar *match-specials* nil);;Hell lets declare them all special, its safer--wfs
(defun proc-$defrule (l) 
  (prog (pt rhs boundlist reflist topreflist name a program lhs* rhs*   tem) 
     (if (not (= (length l) 3)) (wna-err '$defrule))
     (setq name (car l))
     (if (or (not (symbolp name)) (mopp name) (member name '($all $%) :test #'eq))
	 (merror (intl:gettext "defrule: rule name must be a symbol, and not an operator or 'all' or '%'; found: ~M") name))
     (setq pt (copy-tree (setq lhs* (simplify (cadr l)))))
     (setq rhs (copy-tree (setq rhs* (simplify (caddr l)))))
     (setq a (genref))
     (cond ((atom (errset (compilematch a pt)))
	    (merror (intl:gettext "defrule: failed to compile match for pattern ~M") pt))
	   (t (meta-fset name
			 (list 'lambda
			       (list a)
			       `(declare (special ,a))
			       (list 'catch ''match
				     (nconc (list 'prog)
					    (list (setq tem (nconc boundlist
								   (cdr (reverse topreflist)))))
					    `((declare (special ,@ tem)))
					    program
					    (list (list 'return
							(list 'values (memqargs rhs) t)))))))
	      (meta-add2lnc name '$rules)
	      (meta-mputprop name (setq l (list '(mequal) lhs* rhs*)) '$rule)
	      (meta-mputprop name '$defrule '$ruletype)
	      (return (list '(msetq) name (cons '(marrow) (cdr l))))))))

; GETDEC constructs an expression of the form ``if <match> then <assign value> else <match failed>''.

; matchdeclare (aa, true);
;  :lisp (symbol-plist '$aa) => (MPROPS (NIL MATCHDECLARE (T)))
; tellsimpafter (fa(aa), ga(aa));
;  getdec => (MSETQ $AA TR-GENSYM~1)

; matchdeclare (bb, integerp);
;  :lisp (symbol-plist '$bb) => (MPROPS (NIL MATCHDECLARE ($INTEGERP)))
; tellsimpafter (fb(bb), gb(bb));
;  getdec => (COND ((IS '(($INTEGERP) TR-GENSYM~3)) (MSETQ $BB TR-GENSYM~3)) ((MATCHERR)))

; my_p(x) := integerp(x) and x>100;
; matchdeclare (cc, my_p);
;  :lisp (symbol-plist '$cc) => (MPROPS (NIL MATCHDECLARE ($MY_P)))
; tellsimpafter (fc(cc), gc(cc));
;  getdec => (COND ((IS '(($MY_P) TR-GENSYM~5)) (MSETQ $CC TR-GENSYM~5)) ((MATCHERR)))

; :lisp (defun $my_p2 (y x) (is `((mgeqp) ,x ,y)))
; matchdeclare (dd, my_p2 (200));
;  :lisp (symbol-plist '$dd) => (MPROPS (NIL MATCHDECLARE ((($MY_P2) 200))))
; tellsimpafter (fd(dd), gd(dd));
;  getdec => (COND ((IS '(($MY_P2) 200 TR-GENSYM~7)) (MSETQ $DD TR-GENSYM~7)) ((MATCHERR)))

; my_p3 (y, x) := is (x > y);
; matchdeclare (ee, my_p3 (300));
;  :lisp (symbol-plist '$ee) => (MPROPS (NIL MATCHDECLARE ((($MY_P3) 300))))
; tellsimpafter (fe(ee), ge(ee));
;  getdec => (COND ((IS '(($MY_P3) 300 TR-GENSYM~9)) (MSETQ $EE TR-GENSYM~9)) ((MATCHERR)))

; matchdeclare (ff, lambda ([x], x > 400));
;  :lisp (symbol-plist '$ff) => (MPROPS (NIL MATCHDECLARE (((LAMBDA) ((MLIST) $X) ((MGREATERP) $X 400)))))
; tellsimpafter (fff(ff), ggg(ff));
;  getdec => (COND ((IS (MAPPLY1 '((LAMBDA) ((MLIST) $X) ((MGREATERP) $X 400)) (LIST TR-GENSYM~11) T NIL)) (MSETQ $FF TR-GENSYM~11)) ((MATCHERR))) 

; matchdeclare (gg, lambda ([y, x], x > y) (500));
;  :lisp (symbol-plist '$gg) => (MPROPS (NIL MATCHDECLARE (((MQAPPLY) ((LAMBDA) ((MLIST) $Y $X) ((MGREATERP) $X $Y)) 500))))
; tellsimpafter (fg(gg), gg(gg));
;  getdec => (COND ((IS (MEVAL '((MQAPPLY) ((LAMBDA) ((MLIST) $Y $X) ((MGREATERP) $X $Y)) 500 TR-GENSYM~13))) (MSETQ $GG TR-GENSYM~13)) ((MATCHERR)))

; pattern-variable is the pattern variable (as declared by matchdeclare)
; match-against is the expression to match against

; Return T if $MAYBE returns T, otherwise NIL.
; That makes all non-T values (e.g. $UNKNOWN or noun expressions) act like NIL.

(defun definitely-so (e)
  (eq (mfuncall '$maybe e) t))

(defun getdec (pattern-variable match-against)
  (let (p)
    (if (setq p (mget pattern-variable 'matchdeclare))
      ; P is (<foo>) where <foo> is the matchdeclare predicate
      ; If <foo> is an atom, it is T or the name of a Lisp or Maxima function
      ; Otherwise, <foo> is ((<op>) <args>)

      ; If <foo> is $TRUE, T, or $ALL, generated code always assigns gensym value to pattern variable
      (if (and (atom (car p)) (member (car p) '($true t $all) :test #'eq))
        `(msetq ,pattern-variable ,match-against)

        ; Otherwise, we have some work to do.

        (let ((p-op (car p)) (p-args) (test-expr))
          (setq test-expr
                (if (atom p-op)
                  ; P-OP is the name of a function. Try to generate a Lisp function call.
                  (if (and (fboundp p-op) (not (get p-op 'translated)))   ; WHY THE TEST FOR TRANSLATED PROPERTY ??
                    `(eq t (,p-op ,@(ncons match-against)))
                    `(definitely-so '((,p-op) ,@(ncons match-against))))

                  ; Otherwise P-OP is something like ((<op>) <args>).
                  (progn
                    (setq p-args (cdr p-op))
                    (cond
                      ((eq (caar p-op) 'lambda)
                       `(definitely-so (mapply1 ',p-op (list ,match-against) t nil)))
                      ((eq (caar p-op) 'mqapply)
                       `(definitely-so (meval ',(append p-op (ncons match-against)))))
                      ; Otherwise P-OP must be a function call with the last arg missing.
                      (t
                        (if (and (consp (car p-op)) (mget (caar p-op) 'mmacro))
                          `(definitely-so (cons ',(car p-op) ,(append '(list) (mapcar 'memqargs p-args) (ncons match-against))))
                          `(definitely-so (cons ',(car p-op) ',(append (mapcar 'memqargs p-args) (ncons match-against))))))))))

          `(cond
             (,test-expr (msetq ,pattern-variable ,match-against))
             ((matcherr))))))))

(defun compilematch (e p) 
  (prog (reflist) 
     (cond ((fixedmatchp p)
	    (emit (list 'cond
			(list (list 'not
				    (list 'alike1
					  e
					  (list 'meval (list 'quote
							     p))))
			      '(matcherr)))))
	   ((atom p) (compileatom e p))
	   ((eq (caar p) 'mplus) (compileplus e p))
	   ((eq (caar p) 'mtimes) (compiletimes e p))
	   (t (compileatom (list 'kaar e)
			   (caar p))
	      (emit (list 'setq
			  (genref)
			  (list 'kdr e)))
	      (compileeach (car reflist) (cdr p))))
     (return program)))

(defun genref nil 
  (prog (a) 
     (setq a (tr-gensym))
     (setq topreflist (cons a topreflist))
     (return (car (setq reflist (cons a reflist))))))
(defun compileeach (elist plist) 
    (prog (reflist count) 
       (setq count 0)
       (setq reflist (cons elist reflist))
       a    (setq count (f1+ count))
       (cond ((null plist)
	      (return (emit (list 'cond
				  (list (list 'nthkdr elist (f1- count))
					'(matcherr)))))))
       (emit (list 'setq (genref) (list 'kar (cadr reflist))))
       (compilematch (car reflist) (car plist))
       (setq plist (cdr plist))
       (setq reflist (cons (list 'kdr (cadr reflist)) reflist))
       (go a)))

(defun fixedmatchp (x)
  (cond ((numberp x) t)
	((atom x)
	 (if (or (member x boundlist :test #'eq) (null (mget x 'matchdeclare))) t))
	(t (and (or (member (caar x) boundlist :test #'eq)
		    (null (mget (caar x) 'matchdeclare)))
		(fmp1 (cdr x))))))

(defun fmp1 (x)
  (if (null x) t (and (fixedmatchp (car x)) (fmp1 (cdr x)))))
