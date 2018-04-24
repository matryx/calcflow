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

(macsyma-module csimp)

(declare-top (special rsn* $factlim $exponentialize
		      var varlist genvar $%emode $ratprint
		      nn* dn* $errexp sqrt3//2 -sqrt3//2
		      $demoivre errorsw $keepfloat $ratfac))

(load-macsyma-macros rzmac)

(declare-top (special $nointegrate $lhospitallim $tlimswitch $limsubst
		      $abconvtest plogabs))

(setq $demoivre nil rsn* nil $nointegrate nil $lhospitallim 4
      $tlimswitch t $limsubst nil $abconvtest nil
      plogabs nil)

;; Simplified shortcuts of constant expressions involving %pi.
(defvar %p%i '((mtimes) $%i $%pi))
(defvar fourth%pi '((mtimes) ((rat simp) 1 4) $%pi))
(defvar half%pi '((mtimes) ((rat simp) 1 2) $%pi))
(defvar %pi2 '((mtimes) 2 $%pi))
(defvar half%pi3 '((mtimes) ((rat simp) 3 2) $%pi))

(defmvar $sumsplitfact t) ;= nil minfactorial is applied after a factocomb.

(loop for (a b) on
       '(%sin %asin %cos %acos %tan %atan
	 %cot %acot %sec %asec %csc %acsc
	 %sinh %asinh %cosh %acosh %tanh %atanh
	 %coth %acoth %sech %asech %csch %acsch)
       by #'cddr
       do  (putprop a b '$inverse) (putprop b a '$inverse))

(defmfun $demoivre (exp)
  (let ($exponentialize nexp)
    (cond ((atom exp) exp)
	  ((and (eq (caar exp) 'mexpt) (eq (cadr exp) '$%e)
		(setq nexp (demoivre (caddr exp))))
	   nexp)
	  (t (recur-apply #'$demoivre exp)))))

(defun demoivre (l)
  (cond ($exponentialize
	 (merror (intl:gettext "demoivre: 'demoivre' and 'exponentialize' cannot both be true.")))
	(t (setq l (islinear l '$%i))
	   (and l (not (equal (car l) 0))
		(m* (m^ '$%e (cdr l))
		    (m+ (list '(%cos) (car l))
			(m* '$%i (list '(%sin) (car l)))))))))

;; If expr is of the form a*var1+b where a is freeof var1
;; then (a . b) is returned else nil.
(defun islinear (expr var1)
  (declare (special *islinp*))
  (let ((a (let ((*islinp* t))
             (sdiff expr var1))))
    (if (freeof var1 a)
        (cons a (maxima-substitute 0 var1 expr)))))

(defmfun $partition (e var1)
  (prog (k)
     (setq e (mratcheck e) var1 (getopr var1))
     (cond (($listp e)
	    (return (do ((l (cdr e) (cdr l)) (l1) (l2) (x))
			((null l) (list '(mlist simp)
					(cons '(mlist simp) (nreverse l1))
					(cons '(mlist simp) (nreverse l2))))
		      (setq x (mratcheck (car l)))
		      (cond ((free x var1) (setq l1 (cons x l1)))
			    (t (setq l2 (cons x l2)))))))
	   ((mplusp e) (setq e (cons '(mtimes) (cdr e)) k 0))
	   ((mtimesp e) (setq k 1))
	   (t
	    (merror (intl:gettext "partition: first argument must be a list or '+' or '*' expression; found ~M") e)))
     (setq e (partition e var1 k))
     (return (list '(mlist simp) (car e) (cdr e)))))

(defun partition (exp var1 k)	  ; k is 1 for MTIMES and 0 for MPLUS.
  (prog (const varbl op)
     (setq op (cond ((= k 0) '(mplus)) (t '(mtimes))))
     (cond ((or (alike1 exp var1) (not (eq (caar exp) 'mtimes)))
	    (return (cons k exp))))
     (setq exp (cdr exp))
     loop (cond ((freeof var1 (car exp)) (setq const (cons (car exp) const)))
		(t (setq varbl (cons (car exp) varbl))))
     (cond ((null (setq exp (cdr exp)))
	    (return (cons (cond ((null const) k)
				((null (cdr const)) (car const))
				(t (simplifya (cons op (nreverse const)) t)))
			  (cond ((null varbl) k)
				((null (cdr varbl)) (car varbl))
				(t (simplifya (cons op (nreverse varbl)) t)))))))
     (go loop)))

;;To use this INTEGERINFO and *ASK* need to be special.
;;(defun integerpw (x)
;; ((lambda (*ask*)
;;    (integerp10 (ssimplifya (sublis '((z** . 0) (*z* . 0)) x))))
;;  t))

;;(defun integerp10 (x)
;; ((lambda (d)
;;   (cond ((or (null x) (not (free x '$%i))) nil)
;;	 ((mnump x) (integerp x))
;;	 ((setq d (assolike x integerinfo)) (eq d 'yes))
;;	 (*ask* (setq d (cond ((integerp x) 'yes) (t (needinfo x))))
;;		(setq integerinfo (cons (list x d) integerinfo))
;;		(eq d 'yes))))
;; nil))

(setq var (make-symbol "foo"))

(defun numden (e)
  (prog (varlist)
     (setq varlist (list var))
     (newvar (setq e (fmt e)))
     (setq e (cdr (ratrep* e)))
     (setq dn*
	   (simplifya (pdis (ratdenominator e))
		      nil))
     (setq nn*
	   (simplifya (pdis (ratnumerator e))
		      nil))))

(defun fmt (exp)
  (let (nn*)
    (cond ((atom exp) exp)
	  ((mnump exp) exp)
	  ((eq (caar exp) 'mexpt)
	   (cond ((and (mnump (caddr exp))
		       (eq ($sign (caddr exp)) '$neg))
		  (list '(mquotient)
			1
			(cond ((equal (caddr exp) -1)
			       (fmt (cadr exp)))
			      (t (list (list (caar exp))
				       (fmt (cadr exp))
				       (timesk -1 (caddr exp)))))))
		 ((atom (caddr exp))
		  (list (list (caar exp))
			(fmt (cadr exp))
			(caddr exp)))
		 ((and (mtimesp (setq nn* (sratsimp (caddr exp))))
		       (mnump (cadr nn*))
		       (equal ($sign (cadr nn*)) '$neg))
		  (list '(mquotient)
			1
			(list (list (caar exp))
			      (fmt (cadr exp))
			      (cond ((equal (cadr nn*) -1)
				     (cons '(mtimes)
					   (cddr nn*)))
				    (t (neg nn*))))))
		 ((eq (caar nn*) 'mplus)
		  (fmt (spexp (cdr nn*) (cadr exp))))
		 (t (cons (ncons (caar exp))
			  (mapcar #'fmt (cdr exp))))))
	  (t (cons (delsimp (car exp)) (mapcar #'fmt (cdr exp)))))))

(defun spexp (expl dn*)
  (cons '(mtimes) (mapcar #'(lambda (e) (list '(mexpt) dn* e)) expl)))

(defun subin (y x)
  (cond ((not (among var x)) x)
	(t (maxima-substitute y var x))))

;; Right-hand side (rhs) and left-hand side (lhs) of binary infix expressions.
;; These are unambiguous for relational operators, some other built-in infix operators,
;; and user-defined infix operators (declared by the infix function).

;; a - b and a / b are somewhat problematic, since subtraction and division are not
;; ordinarily represented as such (rather a - b = a + (-1)*b and a / b = a * b^(-1)).
;; Also, - can be unary. So let's not worry about - and / .

;; Other problematic cases: The symbols $< $<= $= $# $>= $> have a LED property,
;; but these symbols never appear in expressions returned by the Maxima parser;
;; MLESSP, MLEQP, MEQUAL etc are substituted. So ignore those symbols here.
(let
  ((relational-ops
     ;; < <= = # equal notequal >= >
     '(mlessp mleqp mequal mnotequal $equal $notequal mgeqp mgreaterp
       %mlessp %mleqp %mequal %mnotequal %equal %notequal %mgeqp %mgreaterp))

   (other-infix-ops
     ;; := ::= : :: ->
     '(mdefine mdefmacro msetq mset marrow
       %mdefine %mdefmacro %msetq %mset %marrow)))

  (defmfun $rhs (rel)
     (if (atom rel)
       0
       (if (or (member (caar rel) (append relational-ops other-infix-ops) :test #'eq)
	       ;; This test catches user-defined infix operators.
	       (eq (get (caar rel) 'led) 'parse-infix))
	 (caddr rel)
	 0)))

  (defmfun $lhs (rel)
     (if (atom rel)
       rel
       (if (or (member (caar rel) (append relational-ops other-infix-ops) :test #'eq)
	       ;; This test catches user-defined infix operators.
	       (eq (get (caar rel) 'led) 'parse-infix))
	 (cadr rel)
	 rel))))

(defun ratgreaterp (x y)
  (cond ((and (mnump x) (mnump y))
	 (great x y))
	((equal ($asksign (m- x y)) '$pos))))

;; Simplify the exponential function of the type exp(p/q*%i*%pi+x) using the
;; periodicity of the exponential function and special values for rational
;; numbers with a denominator q = 2, 3, 4, or 6. e is the argument of the 
;; exponential function. For float and bigfloat numbers in the argument e only
;; simplify for an integer representation or a half integral value.
;; The result is an exponential function with a simplified argument.
(defun %especial (e)
  (prog (varlist y k kk j ans $%emode $ratprint genvar)
     (let (($keepfloat nil) ($float nil))
       (unless (setq y (pip ($ratcoef e '$%i))) (return nil))
       ;; Subtract the term y*%i*%pi from the expression e.
       (setq k ($expand (add e (mul -1 '$%pi '$%i y)) 1))
       ;; This is a workaround to get the type (integer, float, or bigfloat)
       ;; of the expression. kk must evaluate to 1, 1.0, or 1.0b0.
       ;; Furthermore, if e is nonlinear, kk does not simplify to a number ONE.
       ;; Because of this we do not simplify something like exp((2+x)^2*%i*%pi)
       (setq kk (div (sub ($expand e) k) (mul '$%i '$%pi y)))
       ;; Return if kk is not an integer or kk is ONE, but y not an integer
       ;; or a half integral value.
       (if (not (or (integerp kk)
                    (and (onep1 kk)
                         (integerp (add y y)))))
           (return nil))
       (setq j (trigred y))
       (setq ans (spang1 j t)))
     (cond ((among '%sin ans)
            (cond ((equal y j) (return nil))
                  ((zerop1 k)
                   ;; To preverse the type we add k into the result.
                   (return (power '$%e (mul '$%pi '$%i (add k j)))))
                  (t 
                    ;; To preserve the type we multiply kk into the result.
                    (return 
                      (power '$%e (add (mul kk k) (mul kk '$%pi '$%i j))))))))
     (setq y (spang1 j nil))
     ;; To preserve the type we multiply kk into the result.
     (return (mul (power '$%e (mul kk k)) (add y (mul '$%i ans))))))

(defun trigred (r)
  (prog (m n eo flag)
     (cond ((numberp r) (return (cond ((even r) 0) (t 1)))))
     (setq m (cadr r))
     (cond ((minusp m) (setq m (- m)) (setq flag t)))
     (setq n (caddr r))
     loop (cond ((> m n)
		 (setq m (- m n))
		 (setq eo (not eo))
		 (go loop)))
     (setq m (list '(rat)
		   (cond (flag (- m)) (t m))
		   n))
     (return (cond (eo (addk m (cond (flag 1) (t -1))))
		   (t m)))))

(defun polyinx (exp x ind)
  (prog (genvar varlist var $ratfac)
     (setq var x)
     (cond ((numberp exp)(return t))
	   ((polyp exp)
	    (cond (ind (go on))
		  (t (return t))))
	   (t (return nil)))
     on	(setq genvar nil)
     (setq varlist (list x))
     (newvar exp)
     (setq exp (cdr (ratrep* exp)))
     (cond
       ((or (numberp (cdr exp))
	    (not (eq (car (last genvar)) (cadr exp))))
	(setq x (pdis (cdr exp)))
	(return (cond ((eq ind 'leadcoef)
		       (div* (pdis (caddr (car exp))) x))
		      (t (setq exp (car exp))
			 (div* (cond ((atom exp) exp)
				     (t
				      (pdis (list (car exp)
						  (cadr exp)
						  (caddr exp)))))
			       x))
		      ))))))

(defun polyp (a)
  (cond ((atom a) t)
	((member (caar a) '(mplus mtimes) :test #'eq)
	 (every #'polyp (cdr a)))
	((eq (caar a) 'mexpt)
	 (cond ((free (cadr a) var)
		(free (caddr a) var))
	       (t (and (integerp (caddr a))
		       (> (caddr a) 0)
		       (polyp (cadr a))))))
	(t (andmapcar #'(lambda (subexp)
			  (free subexp var))
		      (cdr a)))))

(defun pip (e)
  (prog (varlist d c)
     (newvar e)
     (cond ((not (member '$%pi varlist :test #'eq)) (return nil)))
     (setq varlist '($%pi))
     (newvar e)
     (let (($ratfac nil))
       ;; non-nil $ratfac changes form of CRE
       (setq e (cdr (ratrep* e))))
     (setq d (cdr e))
     (cond ((not (atom d)) (return nil))
	   ((equal e '(0 . 1))
	    (setq c 0)
	    (go loop)))
     (setq c (pterm (cdar e) 1))
     loop (cond ((atom c)
		 (cond ((equal c 0) (return nil))
		       ((equal 1 d) (return c))
		       (t (return (list '(rat) c d))))))
     (setq c (pterm (cdr c) 0))
     (go loop)))

(defun spang1 (j ind)
  (prog (ang ep $exponentialize $float $keepfloat)
     (cond ((floatp j) (setq j (maxima-rationalize j))
	    (setq j (list '(rat simp) (car j) (cdr j)))))
     (setq ang j)
     (cond
       (ind nil)
       ((numberp j)
	(cond ((zerop j) (return 1)) (t (return -1))))
       (t (setq j
		(trigred (add2* '((rat simp) 1 2)
				(list (car j)
				      (- (cadr j))
				      (caddr j)))))))
     (cond ((numberp j) (return 0))
	   ((mnump j) (setq j (cdr j))))
     (return
       (cond ((equal j '(1 2)) 1)
	     ((equal j '(-1 2)) -1)
	     ((or (equal j '(1 3))
		  (equal j '(2 3)))
              (div ($sqrt 3) 2))
	     ((or (equal j '(-1 3))
		  (equal j '(-2 3)))
             (div ($sqrt 3) -2))
	     ((or (equal j '(1 6))
		  (equal j '(5 6)))
	      '((rat simp) 1 2))
	     ((or (equal j '(-1 6))
		  (equal j '(-5 6)))
	      '((rat simp) -1 2))
	     ((or (equal j '(1 4))
		  (equal j '(3 4)))
              (div 1 ($sqrt 2)))
	     ((or (equal j '(-1 4))
		  (equal j '(-3 4)))
             (div -1 ($sqrt 2)))
	     (t (cond ((mnegp ang)
		       (setq ang (timesk -1 ang) ep t)))
		(setq ang (list '(mtimes simp)
				ang
				'$%pi))
		(cond (ind (cond (ep (list '(mtimes simp)
					   -1
					   (list '(%sin simp) ang)))
				 (t (list '(%sin simp) ang))))
		      (t (list '(%cos simp) ang))))))))

(defun archk (a b v)
  (simplify
   (cond ((and (equal a 1) (equal b 1)) v)
	 ((and (equal b -1) (equal 1 a))
	  (list '(mtimes) -1 v))
	 ((equal 1 b)
	  (list '(mplus) '$%pi (list '(mtimes) -1 v)))
	 (t (list '(mplus) v (list '(mtimes) -1 '$%pi))))))

(defun genfind (h v)
;;; finds gensym coresponding to v h
  (do ((varl (caddr h) (cdr varl))
       (genl (cadddr h) (cdr genl)))
;;;is car of rat form
      ((eq (car varl) v) (car genl))))
