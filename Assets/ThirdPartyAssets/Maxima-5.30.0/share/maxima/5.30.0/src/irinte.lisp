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

(macsyma-module irinte)

(load-macsyma-macros rzmac)

(declare-top (special checkcoefsignlist var zerosigntest productcase))

(defun hasvar (exp) (not (freevar exp)))

(defun zerp (a) (equal a 0))

(defun integerpfr (a) (if (not (maxima-integerp a)) (integerp1 a)))

(defun nonzerp (a) (not (equal a 0)))

(defun freevnz (a) (and (freevar a) (not (equal a 0))))

(defun inte (funct x)
  (let ((checkcoefsignlist nil)
	(*globalcareflag* nil)
	($radexpand t))
    (declare (special checkcoefsignlist *globalcareflag* $radexpand))
    (intir-ref funct x)))

(defun intir-ref (fun x)
  (prog (a)
     (when (setq a (intir1 fun x)) (return a))
     (when (setq a (intir2 fun x)) (return a))
     (return (intir3 fun x))))

(defun intir1 (fun x)
  (prog (assoclist e0 r0 e1 e2 r1 r2 d p)
     (setq assoclist (factpow (specrepcheck fun) x))
     (setq e1 (cdras 'e1 assoclist)
	   e2 (cdras 'e2 assoclist))
     (cond ((null assoclist)(return nil)))
     (setq d (cdras 'd assoclist)
	   p (cdras 'p assoclist)
	   e0 (cdras 'e0 assoclist)
	   r0 (cdras 'r0 assoclist)
	   r1 (cdras 'r1 assoclist)
	   r2 (cdras 'r2 assoclist))
     (cond ((floatp e0)
	    (setq e0 (rdis (ration1 e0)))))
     (cond ((floatp e1)
	    (setq e1 (rdis (ration1 e1)))))
     (cond ((floatp e2)
	    (setq e2 (rdis (ration1 e2)))))
     (return (intir1-ref d p r0 e0 r1 e1 r2 e2 x))))

(defun intir2 (funct x)
  (let ((res (intir funct x)))
    (if res
	res
	(intirfactoroot funct x))))

(defun intir3 (exp x)
  (prog ((assoclist (elliptquad exp x)) e f g r0)
     (cond (assoclist
	    (setq e (cdras 'e assoclist) f (cdras 'f assoclist)
		  g (cdras 'g assoclist) r0 (cdras 'r0 assoclist))
	    (assume `(($notequal) ,e 0))
	    (return (intir3-r0test assoclist x e f g r0))))
     (return nil)))

(defun intir3-r0test (assoclist x e f g r0)
  (if (root+anything r0 x)
      nil
      (intir3-ref assoclist x e f g r0)))

;; Handle integrals of the form d*p(x)*r1(x)^e1*r2(x)^e2*r0(x)^e0,
;; where p(x) is a polynomial, e1 and e2 are both half an odd integer,
;; and e3 is an integer.
(defun intir1-ref (d p r0 e0 r1 e1 r2 e2 x)
  (let ((nume1 (cadr e1))	;; nume1 = 2*e1
	(nume2 (cadr e2)))	;; nume2 = 2*e2
    ;; I think what this does is try to rationalize r1(x)^e1 or
    ;; r2(x)^e2 so we end up with a new integrand of the form
    ;; d*p'(x)*r0'(x)^e0*ra(x)^ea, where p'(x) is a new polynomial
    ;; obtained from rationaling one term and r0'(x) is a more
    ;; complicated term.
    (cond ((and (plusp nume1) (plusp nume2))
	   (pp-intir1 d p r0 e0 r1 e1 r2 e2 x))
	  ((and (minusp nume1) (minusp nume2))
	   (mm-intir1 d p r0 e0 r1 e1 r2 e2 x))
	  ((plusp nume1)
	   (pm-intir1 d p r0 e0 r1 e1 r2 e2 x))
	  (t
	   (pm-intir1 d p r0 e0 r2 e2 r1 e1 x)))))

(defun pp-intir1 (d p r0 e0 r1 e1 r2 e2 x)
  (if (> (cadr e1) (cadr e2))
      (pp-intir1-exec d p r0 e0 r1 e1 r2 e2 x)
      (pp-intir1-exec d p r0 e0 r2 e2 r1 e1 x)))

;; Handle integrals of the form d*p(x)*r0(x)^e0*r1(x)^e1*r2(x)^e2
;; where p(x) is a polynomial, e1 < 0, and e2 < 0 and are both half an
;; odd integer, and e3 is an integer.
(defun mm-intir1 (d p r0 e0 r1 e1 r2 e2 x)
  (if (> (cadr e1) (cadr e2))
      (mm-intir1-exec d p r0 e0 r1 e1 r2 e2 x)
      (mm-intir1-exec d p r0 e0 r2 e2 r1 e1 x)))

;; Handle integrals of the form d*p(x)*r0(x)^e0*r1(x)^e1*r2(x)^e2
;; where p(x) is a polynomial, e1 > 0, and e2 < 0 and are both half an
;; odd integer, and e3 is an integer.
;;
(defun pm-intir1 (d p r0 e0 rofpos epos rofneg eneg x)
  (let ((numepos (cadr epos))                  ;; numepos = 2*epos = 2*e1
	(numul-1eneg (mul -1 (cadr eneg))))    ;; numul-1eneg = -2*eneg = -2*e2
    (cond ((> numepos numul-1eneg)
	   (mm-intir1 d (mul p (power rofpos (sub epos eneg)))
		      r0 e0 rofpos eneg rofneg eneg x))
	  ((or (equal e0 0) (plusp e0))
	   (pp-intir1 d (mul p (power rofneg (sub eneg epos)))
		      r0 e0 rofpos epos rofneg epos x))
	  (t
	   (mm-intir1 d (mul p (power rofpos (sub epos eneg)))
		      r0 e0 rofpos eneg rofneg eneg x)))))

(defun pp-intir1-exec (d p r0 e0 rofmax emax rofmin emin x)
  (intir (mul d p (if (equal e0 0) 1 (power r0 e0))
	      (power rofmax (add emax (mul -1 emin)))
	      (power ($expand (mul rofmax rofmin)) emin)) x))

;; Handle integrals of the form d*p(x)*r0(x)^e0*r1(x)^e1*r2(x)^e2
;; where p(x) is a polynomial, e1 < 0, and e2 < 0 and are both half an
;; odd integer, and e3 is an integer.  And e2 > e1.
(defun mm-intir1-exec (d p r0 e0 rofmin emin rofmax emax x)
  (intir (mul d p
	      (if (equal e0 0) 1
		  (power r0 e0))
	      (power rofmax (add emax (mul -1 emin)))
	      (power ($expand (mul rofmax rofmin)) emin)) x))

;; Integrating the form (e*x^2+f*x+g)^m*r0(x)^e0.

(defun intir3-ref (assoclist x e f g r0)
  (let ((signdisc (signdiscr e f g))
	(d (cdras 'd assoclist))
	(p (cdras 'p assoclist))
	(e0 (cdras 'e0 assoclist)))
    (cond ((eq signdisc '$positive)
	   (pns-intir3 x e f g d p r0 e0))
	  ((eq signdisc '$negative)
	   (ns-intir3 x e f g d p r0 e0))
	  (t
	   (zs-intir3 x e f d p r0 e0)))))

(defun root+anything (exp var)
  (m2 exp '((mplus)
	    ((coeffpt) (c nonzerp) ((mexpt) (u hasvar) (v integerpfr)))
	    ((coeffpp) (c true)))))

;; Handle d*p(x)/(e*x^2+f*x+g)*r0(x)^e0.  We know that e*x^2+f*x+g has
;; no repeated roots.  Let D be the discriminant of this quadratic,
;; sqrt(f^2-4*e*g).  (If we're here, we already know that f^2-4*e*g >
;; 0).  Thus, we can factor this quadratic as
;; (2*e*x+f-D)*(2*e*x+f+D)/(4*e).  Thus, the original integrand
;; becomes
;;
;; 4*e*d/(2*e*x+f-D)/(2*e*x+f+D)*p(x)*r0(x)^e0.
;;
;; We can separate this as a partial fraction to get
;;
;; (2*d*e/D/(2*e*x+f-D) - 2*d*e/D/(2*e*x+f+D))*p(x)*r0(x)^e0.
;;
;; So we have separated this into two "simpler" integrals.
(defun pns-intir3 (x e f g d p r0 e0)
  (let* ((discr (power (sub (mul f f) (mul 4 e g)) 1//2)) ;; Compute discriminant of
	 (p*r0^e0 (mul p (power r0 e0)))                  ;; quadratic:  sqrt(f^2-4*e*g)
	 (2*e*x+f (add (mul 2 e x) f))
	 (2*e*d*invdisc (mul 2 e d (inv discr))))
    (mul 2*e*d*invdisc
	 (sub (intir2 (mul (inv (sub 2*e*x+f discr)) p*r0^e0) x)
	      (intir2 (mul (inv (add 2*e*x+f discr)) p*r0^e0) x)))))

;; Handle d*p(x)/(e*x^2+f*x+g)*r0(x)^e0.  We know that e*x^2+f*x+g has
;; repeated roots.
;;
(defun zs-intir3 (x e f d p r0 e0)
  ;; Since e*x^2+f*x+g has repeated roots, it can be written as e*(x+r)^2.
  ;; We easily see that r = f/(2*e), so rewrite the integrand as
  ;;
  ;; d*p(x)/e/(x+r)^2*r0(x)^e0.
  (intir2 (mul d p (inv e)
	       (power (add x (div f (add e e))) -2)
	       (power r0 e0))
	  x))

;; Handle d*p(x)/(e*x^2+f*x+g)*r0(x)^e0.  We know that e*x^2+f*x+g has
;; no real roots.
;;
;; G&R 2.252 shows how we can handle these integrals, but I'm too lazy
;; to implement them right now, so return NIL to indicate we don't
;; know what to do.  But whatever it is we do, it's definitely not
;; calling intir or intir2 like zs-intir3 or pns-intir3 do because
;; they eventually call inti which only handles linear forms (e = 0.)
;; We'll need to write custom versions.
(defun ns-intir3 (xx ee fff gg dd pp r0 e0)
  (declare (ignore xx ee fff gg dd pp r0 e0))
  nil)

(defun cdras (a b)
  (cdr (assoc a b :test #'equal)))

(defun intir (funct x)
  (inti funct x (jmaug (specrepcheck funct) x)))

;; Integrate d*p(x)*(f*x+e)^m*(a*x^2+b*x+c)^n.  p(x) is a polynomial,
;; m is an integer, n is a number(?).  a, b, c, e, and f are
;; expressions independent of x (var).
(defun inti (funct x assoclist)
  (prog (met n expr f e #+nil denom)
     (setq n (cdras 'n assoclist))
     (when (or (null assoclist) (maxima-integerp n))
       (return nil))
     (setq f (cdras 'f assoclist)
	   e (cdras 'e assoclist))
     ;; If e is 0 (or not given, we don't have to do the
     ;; transformation.  Just integrate it and return.
     (when (or (equal e 0) (null e))
       (return (intira funct x)))

     ;; (unless (numberp f) (go jump))
     ;; (when (plusp f) (go jump))
     ;; I (rtoy) think this is the case where f is a negative number.
     ;; I think this is trying to convert f*x+e to -f*x-e to make the
     ;; coefficient of x positive.  And if I'm right, the code below
     ;; isn't doing it correctly, except when m = 1 or m = -1.
     ;; (setq denom (add (mul f x) e)
     ;;	   f (mul -1 f)
     ;;	   e (mul -1 e)
     ;;	   funct (mul -1 (div (meval (mul denom funct))
     ;;			      (add (mul f x) e))))

     jump
     ;; Apply the linear substitution y = f*x+e.  That is x = (y-e)/f.
     ;; Then use INTIRA to integrate this.  The integrand becomes
     ;; something like p(y)*y^m and other terms.
     (setq expr (intira (distrexpandroot
			 (cdr ($substitute
			       (mul (inv f)
				    (add (setq met (make-symbol (symbol-name '#:yannis)))
					 (mul -1 e)))
			       x funct)))
			met))
     (setq expr (and expr (mul (inv f) expr)))
     (return ($expand ($substitute (add (mul f x) e) met expr)))))

(defun distrexpandroot (expr)
  (if (null expr)
      1
      (mul (expandroot (car expr))
	   (distrexpandroot (cdr expr)))))

(defun expandroot (expr)
  (if (atom expr)
      expr
      (if (and (eq (caar expr) 'mexpt)
	       (integerpfr (caddr expr)))
	  ($expand expr)
	  expr)))

(defun intirfactoroot (expr x)
  (declare (special *globalcareflag*))
  (prog (assoclist (exp expr))
     (when (setq assoclist (jmaug (setq expr (distrfactor (timestest expr) x)) x))
       (return (inti expr x assoclist)))
     (setq *globalcareflag* 't)
     (when (setq assoclist (jmaug (setq exp (distrfactor (timestest exp) x)) x))
       (setq *globalcareflag* nil)
       (return (inti exp x assoclist)))
     (setq *globalcareflag* nil)
     (return nil)))

(defun distrfactor (expr x)
  (if (null expr)
      1
      (mul (factoroot (car expr) x)
	   (distrfactor (cdr expr) x))))

(defun factoroot (expr var)
  (if (atom expr)
      expr
      (if (and (eq (caar expr) 'mexpt)
	       (hasvar expr)
	       (integerpfr (caddr expr)))
	  (carefulfactor expr var)
	  expr)))

(defun carefulfactor (expr x)
  (declare (special *globalcareflag*))
  (if (null *globalcareflag*)
      ($factor expr)
      (restorex ($factor (power (div (cadr expr) x) (caddr expr))) x)))

(defun restorex (expr var)
  (if (atom expr)
      expr
      (if (eq (caar expr) 'mtimes)
	  (distrestorex (cdr expr) var)
	  expr)))

(defun distrestorex (expr var)
  (if (null expr)
      1
      (mul (restoroot (car expr) var)
	   (distrestorex (cdr expr) var))))

(defun restoroot (expr var)
  (if (atom expr)
      expr
      (if (and (eq (caar expr) 'mexpt)
	       (integerpfr (caddr expr))
	       (mplusp (cadr expr)))
	  (power ($expand (mul var (cadr expr))) (caddr expr))
	  expr)))

(defun timestest (expr)
  (if (atom expr)
      (list expr)
      (if (eq (caar expr) 'mtimes)
	  (cdr expr)
	  (list expr))))

;; Integrate a function of the form d*p(y)*y^m*(a*y^2+b*x+c)^n.
;; n is half of an integer.
(defun intira (funct x)
  (prog (a b c *ec-1* d m n (assoclist (jmaug (specrepcheck funct) x))
	 pluspowfo1 pluspowfo2 minuspowfo
	 polfact signn poszpowlist negpowlist)
     (declare (special *ec-1*))
     (setq n (cdras 'n assoclist))
     ;; r12 1//2)
     ;; (format t "n = ~A~%" n)
     (when (or (null assoclist)
	       (maxima-integerp n))
       (return nil))
     (when (floatp n)
       (setq n (rdis (ration1 n))))
     (setq d (cdras 'd assoclist))
     (when (equal d 0) (return 0))
     (setq c (cdras 'a assoclist))
     (when (equal c 0) (return nil))
     (setq m (cdras 'm assoclist)
	   polfact (cdras 'p assoclist)
	   ;; We know that n is of the form s/2, so just make n = s,
	   ;; and remember that the actual exponent needs to be
	   ;; divided by 2.
	   n (cadr n)
	   signn (checksigntm n)
	   *ec-1* (power c -1)
	   b (cdras 'b assoclist)
	   a (cdras 'c assoclist)
	   ;; pluspowfo1 = 1/2*(n-1), That is, the original exponent - 1/2.
	   pluspowfo1 (mul 1//2 (+ n -1))
	   ;; minupowfo = 1/2*(n+1), that is, the original exponent + 1/2.
	   minuspowfo (mul 1//2 (+ n 1))
	   ;; pluspowfo2 = -1/2*(n+1), that is, the negative of 1/2
	   ;; plus the original exponent.
	   pluspowfo2 (* -1 minuspowfo))
     (destructuring-bind (pos &optional neg)
	 (powercoeflist polfact m x)
       (setf poszpowlist pos)
       (setf negpowlist neg))

     #+nil (progn
	     (format t "n = ~A~%" n)
	     (format t "pluspowfo1 = ~A~%" pluspowfo1)
	     (format t "minuspowfo = ~A~%" minuspowfo)
	     (format t "pluspowfo2 = ~A~%" pluspowfo2))

     ;; I (rtoy) think powercoeflist computes p(x)/x^m as a Laurent
     ;; series.  POSZPOWLIST is a list of coefficients of the positive
     ;; powers and NEGPOWLIST is a list of the negative coefficients.
     (when (and (null negpowlist)
		(not (null poszpowlist)))
       ;; Only polynomial parts.
       (when (eq signn '$positive)
	 (return (augmult (mul d
			       (nummnumn poszpowlist
					 pluspowfo1
					 minuspowfo c b a x)))))
       (return (augmult (mul d
			     (nummdenn poszpowlist
				       pluspowfo2 c b a x)))))
     (when (and (null poszpowlist)
		(not (null negpowlist)))
       ;; No polynomial parts
       (when (eq signn '$positive)
	 (return (augmult (mul d
			       (denmnumn negpowlist minuspowfo c b a x)))))
       (return (augmult (mul d
			     (denmdenn negpowlist pluspowfo2 c b a x)))))
     (when (and (not (null negpowlist))
		(not (null poszpowlist)))
       ;; Positive and negative powers.
       (when (eq signn '$positive)
	 (return (add (augmult (mul d
				    (nummnumn poszpowlist
					      pluspowfo1
					      minuspowfo c b a x)))
		      (augmult (mul d
				    (denmnumn negpowlist
					      minuspowfo c b a x))))))
       (return (add (augmult (mul d
				  (nummdenn poszpowlist
					    pluspowfo2 c b a x)))
		    (augmult (mul d
				  (denmdenn negpowlist
					    pluspowfo2 c b a x))))))))

;; Match d*p(x)*(f*x+e)^m*(a*x^2+b*x+c)^n.  p(x) is a polynomial, m is
;; an integer, n is half of an integer.  a, b, c, e, and f are
;; expressions independent of x (var).
(defun jmaug (exp var)
  (m2 exp '((mtimes)
	    ((coefftt) (d freevar))
	    ((coefftt) (p polyp))
	    ((mexpt) ((mplus) ((coeffpt) (f freevar) (x varp))
		      ((coeffpp)(e freevar)))
	     (m maxima-integerp))
	    ((mexpt) ((mplus)
		      ((coeffpt) (a freevar) ((mexpt) (x varp) 2))
		      ((coeffpt) (b freevar) (x varp))
		      ((coeffpp) (c freevar)))
	     (n integerp1)))))

;; Match d*p(x)*r1(x)^e1*r2(x)^e2*r0(x)^e0, where p(x) is a
;; polynomial, e1 and e2 are both half an odd integer, and e3 is an
;; integer.
(defun factpow (exp var)
  (m2 exp '((mtimes) ((coefftt) (d freevar))
	    ((coefftt) (p polyp))
	    ((mexpt) (r1 hasvar)
	     (e1 integerpfr))
	    ((mexpt) (r2 hasvar)
	     (e2 integerpfr))
	    ((mexpt) (r0 hasvar)
	     (e0 maxima-integerp)))))

;; Match EXP to the form
;; d*p/(e*x^2+f*x+g)*r0(x)^e0.  p is a polynomial in x.
(defun elliptquad (exp var)
  (m2 exp '((mtimes)
	    ((coefftt) (d freevar))
	    ((coefftt) (p polyp))
	    ((mexpt) ((mplus) ((coeffpt) (e freevnz) ((mexpt) (x varp) 2))
		      ((coeffpt) (f freevar) (x varp))
		      ((coeffpp) (g freevar)))
	     -1)
	    ((mexpt) (r0 hasvar)
	     (e0 integerpfr)))))

;; From the set of coefficients, generate the polynomial c*x^2+b*x+a.
(defun polfoo (c b a x)
  (add (mul c x x)
       (mul b x)
       a))

;; I think this is computing the list of coefficients of fun(x)/x^m,
;; where fun(x) is a polynomial and m is a non-negative integer.  The
;; result is a list of two lists.  The first list contains the
;; polynomial part of fun(x)/x^m.  The second is the principal part
;; containing negative powers.
;;
;; Each of the lists is itself a list of power and coefficient itself.
;;
;; Thus (x+3)^2/x^2 = 1 + 6/x + 9/x^2 returns
;;
;; '(((0 1)) ((1 6) (2 9)))
(defun powercoeflist (fun m var)
  (prog ((expanfun (unquote ($expand (mul (prevconstexpan fun var) (power var m)))))
	 maxpowfun powfun coef poszpowlist negpowlist)
     (when (and (equal fun 1) (plusp m))
       (return (cons nil (list (list (cons m (list 1)))))))
     (when (and (equal fun 1) (minusp m))
       (return (cons nil (list (list (cons (- m) (list 1)))))))
     (when (equal expanfun 1)
       (return (cons (list (cons 0 (list 1))) (list nil))))
     (setq maxpowfun ($hipow expanfun var)
	   powfun ($lopow expanfun var))
     loop (setq coef ($coeff expanfun (power var powfun)))
     (when (numberp coef) (go testjump))
     (go nojump)
     testjump (when (and (not (zerop powfun)) (zerop coef))
		(go jump))
     nojump   (when (plusp powfun)
		(setq poszpowlist (append poszpowlist
					  (list (cons powfun (list coef))))))
     (when (zerop powfun)
       (setq poszpowlist
	     (append poszpowlist
		     (list (cons 0 (list (consterm (cdr expanfun) var)))))))
     (when (minusp powfun)
       (setq negpowlist (append negpowlist (list (cons (- powfun) (list coef))))))
     (when (= powfun maxpowfun)
       (return (list poszpowlist (reverse negpowlist))))
     jump (incf powfun)
     (go loop)))

(defun consterm (fun var)
  (cond ((null fun) 0)
	((freeof var (car fun))
	 (add (car fun) (consterm (cdr fun) var)))
	(t (consterm (cdr fun) var))))

(defun prevconstexpan (fun var)
  (cond ((atom fun) fun)
	((eq (caar fun) 'mplus)
	 (cond ((and (freeof var fun)
		     (not (inside fun 'mexpt)))
		(list '(mquote) fun))
	       ((and (freeof var fun) (inside fun 'mexpt))
		(list '(mquote)
		      (distrinplusprev (cdr fun) var)))
	       ((inside fun 'mexpt)
		(distrinplusprev (cdr fun) var))
	       (t fun)))
	((eq (caar fun) 'mtimes)
	 (distrintimesprev (cdr fun) var))
	((and (not (inside (cdr fun) var))
	      (eq (caar fun) 'mexpt))
	 (power (prevconstexpan (cadr fun) var) (caddr fun)))
	(t fun)))

(defun distrinplusprev (fun var)
  (cond ((null fun) 0)
	(t (add (prevconstexpan (car fun) var)
		(distrinplusprev (cdr fun) var)))))

(defun distrintimesprev (fun var)
  (cond ((null fun) 1)
	(t (mul (prevconstexpan (car fun) var)
		(distrintimesprev (cdr fun) var)))))

(defun inside (fun arg)
  (cond ((atom fun)(equal fun arg))
	((inside (car fun) arg) t)
	(t (inside (cdr fun) arg))))

(defun unquote (fun)
  (cond ((not (inside fun 'mquote)) fun)
	(t (unquote (meval fun)))))

(defun checksigntm (expr)
  (prog (aslist quest zerosigntest productcase)
     (setq aslist checkcoefsignlist)
     (cond ((atom expr) (go loop)))
     (cond ((eq (caar expr) 'mtimes)(setq productcase t)))
     loop (cond ((null aslist)
		 (setq checkcoefsignlist
		       (append checkcoefsignlist
			       (list (cons expr
					   (list
					    (setq quest (checkflagandact expr)))))))
		 (return quest)))
     (cond ((equal (caar aslist) expr) (return (cadar aslist))))
     (setq aslist (cdr aslist))
     (go loop)))

(defun checkflagandact (expr)
  (cond (productcase
	 (setq productcase nil)
	 (findsignoftheirproduct (findsignofactors (cdr expr))))
	(t (asksign ($realpart expr)))))

(defun findsignofactors (listofactors)
  (cond ((null listofactors) nil)
	((eq zerosigntest '$zero) '$zero)
	(t (append (list (setq zerosigntest (checksigntm (car listofactors))))
		   (findsignofactors (cdr listofactors))))))

(defun findsignoftheirproduct (llist)
  (prog (sign)
     (cond ((eq llist '$zero) (return '$zero)))
     (setq sign '$positive)
     loop (cond ((null llist) (return sign)))
     (cond ((eq (car llist) '$positive)
	    (setq llist (cdr llist))
	    (go loop)))
     (cond ((eq (car llist) '$negative)
	    (setq sign (changesign sign) llist (cdr llist))
	    (go loop)))
     (return '$zero)))

(defun changesign (sign)
  (if (eq sign '$positive)
      '$negative
      '$positive))

;; Integrate 1/sqrt(c*x^2+b*x+a).
;;
;; G&R 2.261 gives the following, where R = c*x^2+b*x+a and D =
;; 4*a*b-b^2:
;;
;; c > 0:
;;   1/sqrt(c)*log(2*sqrt(c*R)+2*c*x+b)
;;
;; c > 0, D > 0:
;;   1/sqrt(c)*asinh((2*c*x+b)/sqrt(D))
;;
;; c < 0, D < 0:
;;   -1/sqrt(-c)*asin((2*c*x+b)/sqrt(-D))
;;
;; c > 0, D = 0:
;;   1/sqrt(c)*log(2*c*x+b)
;;
(defun den1 (c b a x)
  (let* ((expr (add (mul 2 c x) b)) ;; expr = 2*c*x+b
	 (signc (checksigntm (power c -1)))
	 (signb (checksigntm (power b 2)))
	 (signdiscrim (signdis2 c b a signc signb)))
    (when (and (eq signc '$positive)
	       (eq signdiscrim '$negative))
      ;; c > 0, D > 0
      (return-from den1 (augmult (mul* (power  c -1//2)
				       `((%asinh)
					 ,(mul expr
					       (power (add (mul 4 c a)
							   (mul -1 b b))
						      -1//2)))))))
    (when (and (eq signc '$positive)
	       (eq signdiscrim '$zero))
      ;; c > 0, D = 0
      (return-from den1 (augmult (mul* (power -1 expr)
				       (power c -1//2)
				       `((%log) ,expr)))))
    (when (eq signc '$positive)
      ;; c > 0
      (return-from den1 (augmult (mul* (power c -1//2)
				       `((%log)
					 ,(add (mul 2
						    (power c 1//2)
						    (power (polfoo c b a x) 1//2))
					       expr))))))
    (when (and (eq signc '$negative)
	       (eq signdiscrim '$positive))
      ;; c < 0, D > 0
      (return-from den1 (augmult (mul* -1
				       (power (mul -1 c) -1//2)
				       `((%asin)
					 ,(mul expr
					       (power (add (mul b b)
							   (mul -4 c a))
						      -1//2)))))))
    (when (eq signc '$negative)
      ;; c < 0.  We try again, but flip the sign of the
      ;; polynomial, and multiply by -%i.
      (return-from den1 (augmult (mul (power -1 -1//2)
				      (den1 (mul -1 c)
					    (mul -1 b)
					    (mul -1 a)
					    x)))))))

;; Compute sign of discriminant of the quadratic c*x^2+b*x+a.  That
;; is, the sign of b^2-4*c*a.
(defun signdiscr (c b a)
  (checksigntm (simplifya (add (power b 2) (mul -4 c a)) nil)))

(defun askinver (a)
  (checksigntm (inv a)))

(defun signdis1 (c b a)
  (cond ((equal (mul b a) 0)
	 (if (and (equal b 0) (equal a 0))
	     '$zero
	     '$nonzero))
	(t
	 ;; Why are we checking the sign of (b^2-4*a*c)^2?
	 (checksigntm (power (add (mul b b) (mul -4 c a)) 2)))))

;; Check sign of discriminant of c*x^2+b*x+a, but also taking into
;; account the sign of c and b.
(defun signdis2 (c b a signc signb)
  (cond ((equal signb '$zero)
	 (cond ((equal a 0) '$zero)
	       (t (let ((askinv (askinver a)))
		    (if (or (and (eq signc '$positive)
				 (eq askinv '$negative))
			    (and (eq signc '$negative)
				 (eq askinv '$positive)))
			'$positive
			'$negative)))))
	(t (if (equal a 0)
	       '$positive
	       (signdiscr c b a)))))

(defun signdis3 (c b a signa)
  (declare (special *ec-1*))
  (cond ((equal b 0)
	 (if (equal (checksigntm *ec-1*) signa)
	     '$negative
	     '$positive))
	(t (signdiscr c b a))))

;; Integrate things like x^m*R^(p-1/2), p > 0, m > 0.
;;
;; I think pluspowfo1 = p - 1.
(defun nummnumn (poszpowlist pluspowfo1 p c b a x)
  (declare (special *ec-1*))
  (let ((expr (power (polfoo c b a x) (add p 1//2))) ;; expr = R^(p+1/2)
	(expo *ec-1*)				     ;; expo = 1/c
	(ex (power c -2)))			     ;; ex = 1/c^2
    (prog ((result 0)
	   (controlpow (caar poszpowlist))
	   (coef (cadar poszpowlist))
	   count res1 res2 m partres)
       #+nil (format t "p = ~A~%pluspowfo1 = ~A~%" p pluspowfo1)
       (when (zerop controlpow)
	 ;; Integrate R^(p-1/2)
	 (setq result (augmult (mul coef (numn pluspowfo1 c b a x)))
	       count 1)
	 (go loop))

       jump1
       ;; Handle x*R^(p-1/2)
       ;;
       ;; G&R 2.260, m = 1
       ;;
       ;; integrate(x*R^(2*p-1),x) =
       ;;   R^(p+1/2)/(2*p+1)/c
       ;;     - b/2/c*integrate(sqrt(R^(2*p-1)),x)
       (setq res1 (add (augmult (mul expr expo
				     (power (+ p p 1) -1)))
		       (augmult (mul -1 b 1//2 expo
				     (numn pluspowfo1 c b a x)))))
       (when (equal controlpow 1)
	 (setq result (add result (augmult (mul coef res1)))
	       count 2)
	 (go loop))

       jump2
       ;; Handle x^2*R^(p-1/2)
       ;;
       ;; G&R 2.260, m = 2
       ;;
       ;; integrate(x^2*R^(2*p-1),x) =
       ;;   x*R^(p+1/2)/(2*p+2)/c
       ;;     - (2*p+3)*b/2/(2*p+2)/c*integrate(x*sqrt(R^(2*p-1)),x)
       ;;     - a/(2*p+2)/c*integrate(sqrt(R^(2*p-1)),x)
       (setq res2 (add (augmult (mul* x expr expo
				      (inv (+ p p 2))))
		       (augmult (mul* b (+ p p 3)
				      '((rat) -1 4)
				      ex
				      (inv (+ p p p 1
					      (* p p)
					      (* p p)))
				      expr))
		       (augmult (mul (inv (1+ p))
				     ex
				     '((rat simp) 1 8)
				     (add (mul (power b 2)
					       (+ p p 3))
					  (mul -4 a c))
				     (numn pluspowfo1 c b a x)))))
       (when (equal controlpow 2)
	 (setq result (add result (augmult (mul coef res2)))
	       count 3)
	 (go loop))

       jump3
       (setq count 4
	     m 3)
       jump
       ;; The general case:  x^m*R^(p-1/2)
       (setq partres
	     (let ((pro (inv (+ m p p))))
	       ;; pro = 1/(m+2*p)
	       ;;
	       ;; G&R 2.260, m = 2
	       ;;
	       ;; integrate(x^m*R^(2*p-1),x) =
	       ;;   x^(m-1)*R^(p+1/2)/(m+2*p)/c
	       ;;     - (2*m+2*p-1)*b/2/(m+2*p)/c*integrate(x^(m-1)*sqrt(R^(2*p-1)),x)
	       ;;     - (m-1)*a/(m+2*p)/c*integrate(x^(m-2)*sqrt(R^(2*p-1)),x)
	       (add (augmult (mul (power x (1- m))
				  expr expo pro))
		    (augmult (mul -1 b (+ p p m m -1)
				  1//2 expo pro res2))
		    (augmult (mul -1 a (1- m)
				  expo pro res1)))))
       (incf m)
       (when (> m controlpow)
	 (setq result (add result (augmult (mul coef partres))))
	 (go loop))

       jump4
       (setq res1 res2
	     res2 partres)
       (go jump)

       loop
       (setq poszpowlist (cdr poszpowlist))
       (when (null poszpowlist) (return result))
       (setq coef (cadar poszpowlist))
       (setq controlpow (caar poszpowlist))
       (when (equal count 4) (go jump4))
       (when (equal count 1) (go jump1))
       (when (equal count 2) (go jump2))
       (go jump3))))

;; Integrate R^(p+1/2)
(defun numn (p c b a x)
  (declare (special *ec-1*))
  (let ((exp1 *ec-1*)			      ;; exp1 = 1/c
	(exp2 (add b (mul 2 c x)))	      ;; exp2 = b+2*c*x
	(exp4 (add (mul 4 a c) (mul -1 b b))) ;; exp4 = 4*a*c-b^2
        (exp5 (div 1 (1+ p))))                ;; exp5 = 1/(p+1)
    (if (zerop p)
	;; integrate(sqrt(R),x)
	;;
	;; G&R 2.262 says
	;;
	;; integrate(sqrt(R),x) =
	;;   (2*c*x+b)*sqrt(R)/4/c + del/8/c*integrate(1/sqrt(R),x)
	;;
	;; del = 4*a*c-b^2
	(add (augmult (mul '((rat simp) 1 4)
			   exp1 exp2
			   (power (polfoo c b a x) 1//2)))
	     (augmult (mul '((rat simp) 1 8)
			   exp1 exp4
			   (den1 c b a x))))

	;; The general case
	;;
	;; G&R 2.260, eq. 2:
	;;
	;; integrate(sqrt(R^(2*p+1)),x) =
	;;   (2*c*x+b)/4/(p+1)/c*R^(p+1/2)
	;;     + (2*p+1)*del/8/(p+1)/c*integrate(sqrt(R^(2*p-1)),x)
	(add (augmult (mul '((rat simp) 1 4)
			   exp1 exp5 exp2
			   (power (polfoo c b a x) (add p 1//2))))
	     (augmult (mul '((rat simp) 1 8)
			   exp1 exp5 (+ p p 1)
			   exp4
			   (numn (1- p) c b a x)))))))

(defun augmult (x)
  ($multthru (simplifya x nil)))

;; Integrate things like 1/x^m/R^(p+1/2), p > 0.
(defun denmdenn (negpowlist p c b a x)
  (let ((exp1 (power (polfoo c b a x) (add 1//2 (- p)))))  ;; exp1 = 1/R^(p-1/2)
    (prog (result controlpow coef count res1 res2 m partres
	   (signa (checksigntm (simplifya a nil)))
	   ea-1)
       (when (eq signa '$zero)
	 (return (noconstquad negpowlist p c b x)))
       (setq result 0
	     controlpow (caar negpowlist)
	     ea-1 (power a -1))
       (setq coef (cadar negpowlist))
       (when (zerop controlpow)
	 ;; I'm not sure we ever get here because m = 0 is
	 ;; usually handled elsewhere.
	 (setq result (augmult  (mul coef (denn p c b a x)))
	       count 1)
	 (go loop))

       jump1
       ;; Handle 1/x/R^(p+1/2)
       (setq res1 (den1denn p c b a x))
       (when (equal controlpow 1)
	 (setq result (add result (augmult (mul coef res1)))
	       count 2)
	 (go loop))

       jump2
       ;; Handle 1/x^2/R^(p+1/2)
       ;;
       ;; G&R 2.268, m = 2
       ;;
       ;; integrate(1/x^2/R^(p+1/2),x) =
       ;;   -1/a/x/sqrt(R^(2*p-1))
       ;;     -(2*p+1)*b/2/a*integrate(1/x/sqrt(R^(2*p+1)),x)
       ;;     -2*p*c/a*integrate(1/sqrt(R^(2*p+1)),x)
       (setq res2 (add (augmult (mul -1 ea-1 (power x -1) exp1))
		       (augmult (mul -1 b (+ 1 p p) 1//2
				     ea-1 (den1denn p c b a x)))
		       (augmult (mul -2 p c ea-1 (denn p c b a x)))))
       (when (equal controlpow 2)
	 (setq result (add result (augmult (mul coef res2)))
	       count 3)
	 (go loop))

       jump3
       (setq count 4
	     m 3)

       jump
       ;; General case 1/x^m/R^(p+1/2)
       ;;
       ;; G&R 2.268
       ;;
       ;; integrate(1/x^2/R^(p+1/2),x) =
       ;;   -1/(m-1)/a/x^(m-1)/sqrt(R^(2*p-1))
       ;;     -(2*p+2*m-3)*b/2/(m-1)/a*integrate(1/x^(m-1)/sqrt(R^(2*p+1)),x)
       ;;     -(2*n+m-2)*c/(m-1)/a*integrate(1/x^(m-2)/sqrt(R^(2*p+1)),x)
       (setq partres
	     (let ((exp2 (div -1 (1- m))))
	       ;; exp2 = -1/(m-1)
	       (add (augmult (mul exp2 ea-1
				  (power x (1+ (- m)))
				  exp1))
		    (augmult (mul b (+ p p m m -3) 1//2
				  ea-1 exp2 res2))
		    (augmult (mul c ea-1 exp2
				  (+ p p m -2) res1)))))
       (incf m)
       (when (> m controlpow)
	 (setq result (add result (augmult (mul coef partres))))
	 (go loop))

       jump4
       (setq res1 res2 res2 partres)
       (go jump)

       loop
       (setq negpowlist (cdr negpowlist))
       (when (null negpowlist) (return result))
       (setq coef (cadar negpowlist)
	     controlpow (caar negpowlist))
       (when (equal count 4) (go jump4))
       (when (equal count 1) (go jump1))
       (when (equal count 2) (go jump2))
       (go jump3))))

;; Integral of 1/(c*x^2+b*x+a)^(n), n > 0.  p = n + 1/2.
;;
;; See G&R 2.263 formula 3.
;;
;; Let R = c*x^2+b*x+a.
(defun denn (p c b a x)
  (let ((signdisc (signdis1 c b a))
	(exp1 (add b (mul 2 c x)))             ;; exp1 = b + 2*c*x;
	(exp2 (add (mul 4 a c)	(mul b b -1))) ;; exp2 = (4*a*c-b^2)
	(exp3 (inv (+ p p -1)))		       ;; exp3 = 1/(2*p-1)
	(*ec-1* (inv c)))
    (declare (special *ec-1*))
    #+nil (format t "signdisc = ~A~%p = ~A~%" signdisc p)
    (cond ((and (eq signdisc '$zero) (zerop p))
	   ;; 1/sqrt(R), and R has duplicate roots.  That is, we have
	   ;; 1/sqrt(c*(x+b/(2c))^2) = 1/sqrt(c)/sqrt((x+b/2/c)^2).
	   ;;
	   ;; We return 1/sqrt(c)*log(x+b/2/c).  Shouldn't we return
	   ;; 1/c*log(|x+b/2/c|)?
	   (augmult (mul* (power *ec-1* 1//2)
			  `((%log) ,(add x (mul b 1//2 *ec-1*))))))
	  ((and (eq signdisc '$zero) (plusp p))
	   ;; 1/sqrt(R^(2*p+1)), with duplicate roots.
	   ;;
	   ;; That is, 1/sqrt((c*(x+b/2/c)^(2)^(2*p+1))), or
	   ;; 1/c^(p+1/2)/(x+b/2/c)^(2*p+1).  So the result is
	   ;; -1/2/p*c^(-p-1/2)/(x+b/2/c)^(2*p)
	   (augmult (mul (div -1 (+ p p))
			 (power c (mul -1//2 (+ p p 1)))
			 (power (add x (mul b 1//2  *ec-1*)) (* -2 p)))))
	  ((zerop p)
	   ;; 1/sqrt(R)
	   (den1 c b a x))
	  ((equal p 1)
	   ;; 1/sqrt(R^3).
	   ;;
	   ;; G&R 2.264 eq. 5 says
	   ;;
	   ;; 2*(2*c*x+b)/del/sqrt(R).
	   (augmult (mul 2 exp1 (inv exp2)
			 (power (polfoo c b a x) -1//2))))
	  (t
	   ;; The general case.  G&R 2.263 eq. 3.
	   ;;
	   ;; integrate(1/sqrt(R^(2*p+1)),x) =
	   ;;    2*(2*c*x+b)/(2*p-1)/c/sqrt(R^(2*p-1))
	   ;;    + 8*(p-1)*c/(2*p-1)/del*integrate(1/sqrt(R^(2*p-1)),x)
	   ;;
	   ;; where del = 4*a*c-b^2.
	   (add (augmult (mul 2 exp1
			      exp3 (inv exp2)
			      (power (polfoo c b a x)
				     (add 1//2 (- p)))))
		(augmult (mul 8 c (1- p)
			      exp3 (inv exp2)
			      (denn (1- p) c b a x))))))))

;; Integral of 1/x/(c*x^2+b*x+a)^(p+1/2), p > 0.
(defun den1denn (p c b a x)
  (let ((signa (checksigntm (power a 2))) ;; signa = sign of a^2
	(ea-1 (inv a)))		  ;; ea-1 = 1/a
    (cond ((eq signa '$zero)
	   ;; This is wrong because noconstquad expects a
	   ;; powercoeflist for th first arg.  But I don't think
	   ;; there's any way to get here from anywhere.  I'm pretty
	   ;; sure den1denn is never called with a equal to 0.
	   (noconstquad 1 p c b x))
	  ((zerop p)
	   ;; 1/x/sqrt(c*x^2+b*x+a)
	   (den1den1 c b a x))
	  (t
	   ;; The general case.  See G&R 2.268:
	   ;;
	   ;; R=(c*x^2+b*x+a)
	   ;;
	   ;; integrate(1/x/sqrt(R^(2*p+1)),x) =
	   ;;
	   ;;   1/(2*p-1)/a/sqrt(R^(2*p-1))
	   ;;     - b/2/a*integrate(1/sqrt(R^(2*p+1)),x)
	   ;;     + 1/a*integrate(1/x/sqrt(R^(2*p-1)),x)
	   (add (augmult (mul (inv (+ p p -1))
			      ea-1
			      (power (polfoo c b a x)
				     (add 1//2 (- p)))))
		(augmult (mul ea-1 (den1denn (1- p) c b a x)))
		(augmult (mul -1 1//2 ea-1 b (denn p c b a x))))))))

;; Integral of 1/x/sqrt(c*x^2+b*x+a).
;;
;; G&R 2.266 gives the following results, where del is the
;; discriminant 4*a*c-b^2.  (I think this is the opposite of what we
;; compute below for the discriminant.)
;;
(defun den1den1 (c b a x)
  (let ((exp2 (add (mul b x) a a))                ; exp2 = b*x+2*a
        (exp3 (inv (simplify (list '(mabs) x))))) ; exp3 = 1/abs(x)
    (prog (signdiscrim
	   (condition (add (mul b x) a a))
	   (signa (checksigntm (simplifya a nil)))
	   exp1)
       (when (eq signa '$zero)
	 (return (noconstquad '((1 1)) 0 c b x)))
       (setq signdiscrim (signdis3 c b a signa)
	     exp1 (power a (inv -2)))
       #+nil (format t "signa = ~A~%signdiscrim = ~A~%" signa signdiscrim)

       (when (and (eq signa '$positive)
		  (eq signdiscrim '$negative))
	 ;; G&R case a > 0, del > 0
	 ;;
	 ;; -1/sqrt(a)*asinh((2*a+b*x)/x/sqrt(del)
	 (return (mul* -1 exp1
		       `((%asinh)
			 ,(augmult (mul exp2 exp3
					(power (add (mul 4 a c)
						    (mul -1 b b))
					       -1//2)))))))
       (when (and (eq signdiscrim '$zero)
		  (eq signa '$positive))
	 ;; G&R case del = 0, a > 0
	 ;;
	 ;; 1/sqrt(a)*log(x/(2*a+b*x))
	 (return (mul* (power -1 condition)
		       -1 exp1
		       `((%log) ,(augmult (mul exp3 exp2))))))
       (when (eq signa '$positive)
	 ;; G&R case a > 0
	 ;;
	 ;; -1/sqrt(a)*log((2*a+b*x+2*sqrt(a*R))/x)
	 ;;
	 ;; R = c*x^2+b*x+a.
	 (return (mul* -1 exp1
		       `((%log)
			 ,(add b (mul 2 a exp3)
			       (mul 2 exp3
				    (power a 1//2)
				    (power (polfoo c b a x) 1//2)))))))
       (when (and (eq signa '$negative)
		  (eq signdiscrim '$positive))
	 ;; G&R case a < 0, del < 0
	 ;;
	 ;; 1/sqrt(-a)*asin((2*a+b*x)/x/sqrt(b^2-4*a*c))
	 (return (mul* (power (mul -1 a) -1//2)
		       `((%asin)
			 ,(augmult (mul exp2 exp3
					(power (add (mul b b) (mul -4 a c)) -1//2)))))))
       ;; I think this is the case of a < 0.  We flip the sign of
       ;; coefficients of the quadratic to make a positive, and
       ;; multiply the result by 1/%i.
       ;;
       ;; Why can't we use the case a < 0 in G&R 2.266:
       ;;
       ;; 1/sqrt(-a)*atan((2*a+b*x)/2/sqrt(-a)/sqrt(R)
       ;;
       ;; FIXME:  Why the multiplication by -1?
       (return (mul #+nil -1
		    (power -1 1//2)
		    (den1den1 (mul -1 c) (mul -1 b) (mul -1 a) x))))))

;; Integral of d/x^m/(c*x^2+b*x)^(p+1/2), p > 0.  The values of m and
;; d are in NEGPOWLIST.
(defun noconstquad (negpowlist p c b x)
  (let ((exp1 (inv (+ p p 1)))	 ;; exp1 = 1/(2*p+1)
	(exp2 (inv x))	         ;; exp2 = 1/x
	(exp3 (- p)))		 ;; exp3 = -p
    (prog (result controlpow coef count res1 signb m partres eb-1)
       (setq signb (checksigntm (power b 2)))
       (when (eq signb '$zero)
	 (return (trivial1 negpowlist p c x)))
       (setq result 0
	     controlpow (caar negpowlist)
	     coef (cadar negpowlist)
	     eb-1 (inv b))
       (when (zerop controlpow)
	 ;; Not sure if we ever actually get here.  The case of
	 ;; m=0 is usually handled at a higher level.
	 (setq result (augmult (mul coef (denn p c b 0 x)))
	       count 1)
	 (go loop))
       jump1
       ;; Handle 1/x/R^(p+1/2)
       ;;
       ;; G&R 2.268, a = 0, m = 1
       ;;
       ;; integrate(1/x^m/sqrt(R^(2*p+1)),x) =
       ;;   -2/(2*p+1)/b/x/sqrt(R^(2*p-1))
       ;;     -4*p*c/(2*p+1)/b*integrate(1/sqrt(R^(2*p+1)),x)
       (setq res1 (add (augmult (mul -2 exp1 eb-1 exp2
				     (power (polfoo c b 0 x)
					    (add 1//2 exp3))))
		       (augmult (mul -4 p c exp1 eb-1 (denn p c b 0 x)))))
       (when (equal controlpow 1)
	 (setq result (add result (augmult (mul coef res1)))
	       count 2)
	 (go loop))
       jump2 (setq count 3 m 2)
       jump
       ;; Handle general case 1/x^m/R^(p+1/2)
       ;;
       ;; G&R 2.268, a = 0
       ;;
       ;; integrate(1/x^m/sqrt(R^(2*p+1)),x) =
       ;;   -2/(2*p+2*m-1)/b/x^m/sqrt(R^(2*p+1))
       ;;     -(4*p+2*m-2)*c/(2*p+2*m-1)/b*integrate(1/x^(m-1)/sqrt(R^(2*p+1)),x)
       (setq partres
	     (add (augmult (mul -2 (inv (+ p p m m -1))
				eb-1
				(power x (mul -1 m))
				(power (polfoo c b 0 x)
				       (add 1//2 exp3))))
		  (augmult (mul -2 c (+ p p m -1)
				eb-1
				(inv (+ p p m m -1))
				res1))))
       (incf m)
       (when (> m controlpow)
	 (setq result (add result (augmult (mul coef partres))))
	 (go loop))
       jump3
       (setq res1 partres)
       (go jump)
       loop
       (setq negpowlist (cdr negpowlist))
       (when (null negpowlist) (return result))
       (setq coef (cadar negpowlist)
	     controlpow (caar negpowlist))
       (when (= count 3) (go jump3))
       (when (= count 1) (go jump1))
       (go jump2))))

;; The trivial case of d/x^m/(c*x^2+b*x+a)^(p+1/2), p > 0, and a=b=0.
(defun trivial1 (negpowlist p c x)
  (cond ((null negpowlist) 0)
	(t
	 ;; d/x^m/c^(p+1/2)/x^(2*p+1) = d/c^(p+1/2)/x^(m+2*p+1)
	 ;; The integral is obviously
	 ;;
	 ;; -d/c^(p+1/2)/x^(m+2*p)/(m+2*p)
	 (add (augmult (mul (power x
				   (add (* -2 p)
					(mul -1 (caar negpowlist))))
			    (cadar negpowlist)
			    (power c (add (- p) -1//2))
			    (inv (add (* -2 p)
				      (mul -1 (caar negpowlist))))))
	      (trivial1 (cdr negpowlist) p c x)))))

;; Integrate pl(x)/(c*x^2+b*x+a)^(p+1/2) where pl(x) is a polynomial
;; and p > 0.  The polynomial is given in POSZPOWLIST.
(defun nummdenn (poszpowlist p c b a x)
  (declare (special *ec-1*))
  (let ((exp1 (inv (+ p p -1)))	;; exp1 = 1/(2*p-1)
	(exp2 (power (polfoo c b a x) (add 1//2 (- p)))) ;; exp2 = (a*x^2+b*x+c)^(p-1/2)
	(exp3 (add (mul 4 a c) (mul -1 b b))) ;; exp3 = (4*a*c-b^2) (negative of the discriminant)
	(exp4 (add x (mul b 1//2 *ec-1*)))    ;; exp4 = x+b/2/c
	(exp5 (power c -2))		      ;; exp5 = 1/c^2
	(exp6 (+ 2 (* -2 p)))		      ;; exp6 = -2*p+2
	(exp7 (1+ (* -2 p))))		      ;; exp7 = -2*p+1
    (prog (result controlpow coef count res1 res2 m partres signdiscrim)
       ;; Let S=R^(p+1/2).
       ;;
       ;; We are trying to integrate pl(x)/S
       ;; = (p0 + p1*x + p2*x^3 + ...)/S
       ;;
       ;; So the general term is pm*x^m/S.  This integral is given by
       ;; G&R 2.263, eq.1:
       ;;
       ;; integrate(x^m/sqrt(R^(2*p+1)),x) =
       ;;
       ;;    x^(m-1)/(m-2*n)/sqrt(R^(2*p-1))
       ;;    - (2*m-2*n-1)*b/2/(m-2*n)/c*integrate(x^(m-1)/sqrt(R^(2*p+1)),x)
       ;;    - (m-1)*a/(m-2*n)/c*integrate(x^(m-2)/sqrt(R^(2*p+1)),x)
       ;;
       ;; Thus the integration of x^m/S involves x^(m-1)/S and x^(m-2)/S.
       ;;
       ;; I think what this loop does is integrate x^(m-1)/S and
       ;; x^(m-2)/S, and remember them so that we can integrate x^m/S
       ;; without having to do all the integrals again.  Thus we
       ;; start with the constant term and work our way up to the
       ;; highest term.
       ;;
       ;; I think this would be much simpler if we used memoization
       ;; and start with the highest power.  Then all the
       ;; intermediate forms will have been computed, and we can just
       ;; simply integrate all the lower terms by looking them up.
       (setq result 0
	     controlpow (caar poszpowlist))
       (setq coef (cadar poszpowlist)
	     signdiscrim (signdis1 c b a))
       ;; We're looking at coef*x^controlpow/R^(p+1/2) right now.
       (when (zerop controlpow)
	 ;; Actually it's coef/R^(p+1/2), so handle that now, go
	 ;; to the next coefficient.
	 (setq result (augmult (mul coef (denn p c b a x)))
	       count 1)
	 (go loop))

       jump1
       ;;
       ;; This handles the case coef*x/R^(p+1/2)
       ;;
       ;; res1 = -1/c/(2*p-1)*R^(p-1/2)
       ;;         -b/2/c*integrate(R^(p+1/2),x)
       ;;
       (setq res1
	     (add (augmult (mul -1  *ec-1* exp1 exp2))
		  (augmult (mul b -1//2 *ec-1* (denn p c b a x)))))
       (when (= controlpow 1)
	 ;; Integrate coef*x/R^(p+1/2).
	 ;;
	 ;; x/R^(p+1/2) is in res1.
	 (setq result (add result (augmult (mul coef res1)))
	       count 2)
	 (go loop))
       jump2
       ;; This handles the case coef*x^2/R^(p+1/2)
       (when (and (plusp p) (not (eq signdiscrim '$zero)))
	 ;; p > 0, no repeated roots
	 (setq res2
	       (add (augmult (mul *ec-1* exp1 (inv exp3) exp2
				  (add (mul 2 a b)
				       (mul 2 b b x)
				       (mul -4 a c x))))
		    (augmult (mul *ec-1* (inv exp3) exp1
				  (add (mul 4 a c)
				       (mul 2 b b p)
				       (mul -3 b b))
				  (denn (+ p -1)
					c b a x))))))
       (when (and (zerop p) (not (eq signdiscrim '$zero)))
	 ;; x^2/sqrt(R), no repeated roots.
	 ;;
	 ;; G&R 2.264, eq. 3
	 ;;
	 ;; integrate(x^2/sqrt(R),x) =
	 ;;   (x/2/c-3*b/4/c^2)*sqrt(R)
	 ;;   + (3*b^2/8/c^2-a/2/c)*integrate(1/sqrt(R),x)
	 ;;
	 ;;  = (2*c*x-3*b)/4/c^2*sqrt(R)
	 ;;      + (3*b^2-4*a*c)/8/c^2*integrate(1/sqrt(R),x)
	 (setq res2
	       (add (augmult (mul '((rat simp) 1 4)
				  exp5
				  (add (mul 2 c x)
				       (mul -3 b))
				  (power (polfoo c b a x)
					 1//2)))
		    (augmult (mul '((rat simp) 1 8)
				  exp5
				  (add (mul 3 b b)
				       (mul -4 a c))
				  (den1 c b a x))))))
       (when (and (zerop p) (eq signdiscrim '$zero))
	 ;; x^2/sqrt(R), repeated roots
	 ;;
	 ;; With repeated roots, R is really of the form
	 ;; c*x^2+b*x+b^2/4/c = c*(x+b/2/c)^2.  So we have
	 ;;
	 ;; x^2/sqrt(c)/(x+b/2/c)
	 ;;
	 ;; And the integral of this is
	 ;;
	 ;; b^2*log(x+b/2/c)/4/c^(5/2) + x^2/2/sqrt(c) - b*x/2/c^(3/2).
	 ;;
	 (setq res2
	       ;; (add (augmult (mul* b b (list '(rat) 1 4)
	       ;;			   (power c -3)
	       ;;			   (list '(%log) exp4)))
	       ;;	    (augmult (mul *ec-1* 1//2 (power exp4 2)))
	       ;;	    (augmult (mul -1 b x exp5)))
	       (add (augmult (mul* b b '((rat) 1 4)
				   (power c (div -5 2))
				   `((%log) ,exp4)))
		    (augmult (mul (power c -1//2) 1//2 (power x 2)))
		    (augmult (mul -1//2 b x (power c (div -3 2)))))))

       (when (and (= p 1) (eq signdiscrim '$zero))
	 ;; x^2/sqrt(R^3), repeated roots
	 ;;
	 ;; As above, we have c*(x+b/2/c)^2, so
	 ;;
	 ;; x^2/sqrt(R^3) = x^2/sqrt(c^3)/(x+b/2/c)^3
	 ;;
	 ;; And the integral is
	 ;;
	 ;; log(x+b/2/c)/c^(3/2) + z*(3*z+4*x)/2/c^(3/2)/(z+x)^2
	 ;;
	 ;; where z = b/2/c.
	 (setq res2
	       ;; (add (augmult (mul* *ec-1* (list '(%log) exp4)))
	       ;;	    (augmult (mul b exp5 (power exp4 -1)))
	       ;;	    (augmult (mul (list '(rat) -1 8)
	       ;;			  (power c -3)
	       ;;			  b b (power exp4 -2))))
	       (add (augmult (mul* (power c (div -3 2)) `((%log) ,exp4)))
		    (augmult (mul b x (power c (div -5 2)) (power exp4 -2)))
		    (augmult (mul (div 3 8)
				  (power c (div -7 2))
				  b b (power exp4 -2))))))

       (when (and (eq signdiscrim '$zero) (> p 1))
	 ;; x^2/R^(p+1/2), repeated roots, p > 1
	 ;;
	 ;; As above, we have R=c*(x+b/2/c)^2, so the integral is
	 ;;
	 ;; x^2/(x+b/2/c)^(2*p+1)/c^(p+1/2).
	 ;;
	 ;; Let d = b/2/c.  Then write x^2 =
	 ;; (x+d)^2-2*d*(x+d)+d^2.  The integrand becomes
	 ;;
	 ;; 1/(x+d)^(2*p-1) - 2*d/(x+d)^(2*p) + d^2/(x+d)^(2*p+1)
	 ;;
	 ;; whose integral is
	 ;;
	 ;; 1/(2*p-2)/(x+d)^(2*p-2) - 2*d/(2*p-1)/(x+d)^(2*p-1)
	 ;;   + d^2/(2*p)/(x+d)^(2*p)
	 ;;
	 ;; And don't forget the factor c^(-p-1/2).  Finally,
	 ;;
	 ;; 1/c^(p+1/2)/(2*p-2)/(x+d)^(2*p-2)
	 ;;  - b/c^(p+3/2)/(2*p-1)/(x+d)^(2*p-1)
	 ;;  + b^2/8/c^(p+5/2)/p/(x+d)^(2*p)
	 (setq res2
	       ;; (add (augmult (mul *ec-1*
	       ;;			  (power exp4 exp6)
	       ;;			  (inv exp6)))
	       ;;	    (augmult (mul -1 b exp5 (inv exp7)
	       ;;			  (power exp4 exp7)))
	       ;;	    (augmult (mul b b (list '(rat) -1 8)
	       ;;			  (power c -3)
	       ;;			  (inv p)
	       ;;			  (power exp4
	       ;;				 (* -2 p)))))
	       (add (augmult (mul (inv (power c (add p 1//2)))
				  (power exp4 exp6)
				  (inv exp6)))
		    (augmult (mul -1 b
				  (inv (power c (add p (div 3 2))))
				  (inv exp7)
				  (power exp4 exp7)))
		    (augmult (mul b b '((rat simp) -1 8)
				  (inv (power c (add p (div 5 2))))
				  (inv p)
				  (power exp4
					 (* -2 p)))))))
       (when (= controlpow 2)
	 ;; x^2/R^(p+1/2)
	 ;;
	 ;; We computed this result above, so just multiply by
	 ;; the coefficient and add it to the result.
	 (setq result (add result (augmult (mul coef res2)))
	       count 3)
	 (go loop))
       jump3
       (setq count 4
	     m 3)
       jump
       ;; coef*x^m/R^(p+1/2).  m >= 3
       (setq partres
	     (let ((denom (+ p p (- m))))
	       ;; denom = 2*p-m

	       ;; G&R 2.263, eq 1:
	       ;;
	       ;; integrate(x^m/sqrt(R^(2*p+1)),x) =
	       ;;   x^(m-1)/c/(m-2*p)/sqrt(R^(2*p-1))
	       ;;     - b*(2*m-2*p-1)/2/(m-2*p)*integrate(x^(m-1)/sqrt(R^(2*p+1)),x)
	       ;;     + (m-1)*a/(m-2*p)/c*integrate(x^(m-2)/sqrt(R^(2*p+1)),x)
	       ;;
	       ;; The two integrals here were computed above in res2
	       ;; and res1, respectively.
	       (add (augmult (mul* (power x (1- m))
				   *ec-1* (div -1 denom)
				   (power (polfoo c b a x)
					  (add 1//2 (- p)))))
		    (augmult (mul b (+ p p 1 (* -2 m))
				  -1//2
				  *ec-1* (inv denom) res2))
		    (augmult (mul a (1- m) *ec-1* (inv denom) res1)))))
       on
       ;; Move on to next higher power
       (incf m)
       (when (> m controlpow)
	 (setq result (add result (augmult (mul coef partres))))
	 (go loop))
       jump4
       (setq res1 res2
	     res2 partres)
       (when (= m (+ p p))
	 (setq partres
	       (let ((expr (nummdenn (list (list (1- m) 1)) p c b a x)))
		 (add (mul x expr)
		      (mul -1 (distrint (cdr ($expand expr)) x)))))
	 (go on))
       (go jump)
       loop
       ;; Done with first term of polynomial.  Exit if we're done.
       (setq poszpowlist (cdr poszpowlist))
       (when (null poszpowlist) (return result))
       (setq coef (cadar poszpowlist) controlpow (caar poszpowlist))
       (when (= count 4) (go jump4))
       (when (= count 1) (go jump1))
       (when (= count 2) (go jump2))
       (go jump3))))

;; Integrate functions of the form coef*R^(pow-1/2)/x^m.  NEGPOWLIST
;; contains the list of coef's and m's.
(defun denmnumn (negpowlist pow c b a x)
  (let ((exp1 (inv x))		    ;; exp1 = 1/x
	(exp2 (+ pow pow -1)))	    ;; exp2 = 2*pow-1
    (prog (result controlpow coef count res1 res2 m partres signa ea-1
	   (p (+ pow pow -1))) ;; p = 2*pow-1.
			       ;; NOTE: p is not the same here as in other routines!
       ;; Why is there this special case for negpowlist?
       ;; CASE1 calls this in this way.
       (when (eq (car negpowlist) 't)
	 (setq negpowlist (cdr negpowlist))
	 (go there))
       (setq signa (checksigntm (power a 2)))
       (when (eq signa '$zero)
	 (return (nonconstquadenum negpowlist p c b x)))
       (setq ea-1 (inv a))
       there
       (setq result 0
	     controlpow (caar negpowlist)
	     coef (cadar negpowlist))
       (when (zerop controlpow)
	 ;; integrate(sqrt(R)).
	 ;; I don't think we can normally get here.
	 (setq result (augmult (mul coef
				    (numn (add (mul p 1//2) 1//2)
					  c b a x)))
	       count 1)
	 (go loop))
       jump1
       ;; Handle integrate(sqrt(R^(2*pow-1))/x),x
       (setq res1 (den1numn pow c b a x))
       (when (equal controlpow 1)
	 (setq result (add result (augmult (mul coef res1)))
	       count 2)
	 (go loop))
       jump2
       ;; Handle integrate(sqrt(R^(2*pow-1))/x^2,x)
       (unless (= p 1)
	 ;; integrate(sqrt(R^(2*pow-1))/x^2,x)
	 ;;
	 ;; We can use integration by parts to get
	 ;;
	 ;; integrate(sqrt(R^(2*pow-1))/x^2,x) =
	 ;;   -R^(pow-1/2)/x
	 ;;     + (2*pow-1)*b/2*integrate(sqrt(R^(2*pow-3))/x,x)
	 ;;     + (2*pow-1)*c*integrate(sqrt(R^(2*pow-3)),x)
	 (setq res2
	       (add (augmult (mul -1 exp1
				  (power (polfoo c b a x)
					 (add pow -1//2))))
		    (augmult (mul b (div exp2 2)
				  (den1numn (1- pow) c b a x)))
		    (augmult (mul c exp2 (numn (- pow 2) c b a x))))))
       (when (= p 1)
	 ;; integrate(sqrt(R)/x^2,x)
	 ;;
	 ;; G&R 2.267, eq. 2
	 ;;
	 ;; integrate(sqrt(R)/x^2,x) =
	 ;;   -sqrt(R)/x
	 ;;     + b/2*integrate(1/x/sqrt(R),x)
	 ;;     + c*integrate(1/sqrt(R),x)
	 ;;
	 (setq res2 (add (augmult (mul -1 (power (polfoo c b a x) 1//2)
					    exp1))
			      (augmult (mul b 1//2 (den1den1 c b a x)))
			      (augmult (mul c (den1 c b a x))))))
       (when (equal controlpow 2)
	 (setq result (add result (augmult (mul coef res2)))
	       count 3)
	 (go loop))
       jump3
       (setq count 4
	     m 3)
       jump
       ;; The general case sqrt(R^(2*p-1))/x^m
       ;;
       ;; G&R 2.265
       ;;
       ;; integrate(sqrt(R^(2*p-1))/x^m,x) =
       ;;   -sqrt(R^(2*p+1))/(m-1)/a/x^(m-1)
       ;;     + (2*p-2*m+3)*b/2/(m-1)/a*integrate(sqrt(R^(2*p-3))/x^(m-1),x)
       ;;     + (2*p-m+2)*c/(m-1)/a*integrate(sqrt(R^(2*n-3))/x^(m-2),x)
       ;;
       ;; NOTE: The p here is 2*pow-1.  And we're
       ;; integrating R^(pow-1/2).

       (setq partres
	     (add (augmult (mul (div -1 (1- m))
				ea-1
				(power x (1+ (- m)))
				(power (polfoo c b a x)
				       (add (div p 2) 1))))
		  (augmult (mul (inv (+ m m -2))
				ea-1 b
				(+ p 4 (* -2 m))
				res2))
		  (augmult (mul c ea-1
				(+ p 3 (- m))
				(inv (1- m)) res1))))
       (incf m)
       (when (> m controlpow)
	 (setq result (add result (augmult (mul coef partres))))
	 (go loop))
       jump4
       (setq res1 res2
	     res2 partres)
       (go jump)
       loop
       (setq negpowlist (cdr negpowlist))
       (when (null negpowlist) (return result))
       (setq coef (cadar negpowlist)
	     controlpow (caar negpowlist))
       (when (= count 4)
	 (go jump4))
       (when (= count 1)
	 (go jump1))
       (when (= count 2)
	 (go jump2))
       (go jump3))))

;; Like denmnumn, but a = 0.
(defun nonconstquadenum (negpowlist p c b x)
  (prog (result coef m)
     (cond ((equal p 1)
	    (return (case1 negpowlist c b x))))
     (setq result 0)
     loop
     (setq m (caar negpowlist)
	   coef (cadar negpowlist))
     (setq result (add result (augmult (mul coef (casegen m p c b x))))
	   negpowlist (cdr negpowlist))
     (cond ((null negpowlist) (return result)))
     (go loop)))

;; Integrate (c*x^2+b*x)^(p-1/2)/x^m
(defun casegen (m p c b x)
  (let ((exp1 (power (polfoo c b 0 x) (div p 2)))    ;; exp1 = R^(p/2)
	(exp3 (power x (1+ (- m)))))                 ;; exp3 = 1/x^(m-1)
    (cond ((= p 1)
	   ;; sqrt(c*x^2+b*x)/x^m
	   (case1 (list (list m 1)) c b x))
	  ((zerop m)
	   ;; (c*x^2+b*x)^(p-1/2)
	   (case0 p c b x))
	  ((= m (1+ p))
	   ;; (c*x^2+b*x)^(p-1/2)/x^(p+1)
	   (add (augmult (mul -1 exp1 (inv (1- m)) exp3))
		(augmult (mul b 1//2 (casegen (1- m) (- p 2) c b x)))
		(augmult (mul c (casegen (- m 2) (- p 2) c b x)))))
	  ((= m 1)
	   ;; (c*x^2+b*x)^(p-1/2)/x
	   ;;
	   (add (augmult (mul (inv p) exp1))
		(augmult (mul b 1//2 (case0 (- p 2) c b x)))))
	  (t
	   ;; (c*x^2+b*x)^(p-1/2)/x^m
	   (add (augmult (mul -1 exp1 (inv (- m (1+ p))) exp3))
		(augmult (mul -1 p b 1//2 (inv (- m (1+ p)))
			      (casegen (1- m) (- p 2) c b x))))))))

;; Integrate things like sqrt(c*x^2+b*x))/x^m.
(defun case1 (negpowlist c b x)
  (declare (special *ec-1*))
  (let ((exp1 (power c -1//2)) ;; exp1 = 1/sqrt(c)
	(eb-1 (inv b)))	       ;; eb-1 = 1/b
    (prog ((result 0) (controlpow (caar negpowlist)) (coef (cadar negpowlist))
	   m1 count res1 res2 m signc signb partres res)
       (setq m1 (- controlpow 2))
       (when (zerop controlpow)
	 (setq result (augmult (mul coef (case0 1 c b x)))
	       count 1)
	 (go loop))
       jump1
       ;; sqrt(R)/x
       (when (= controlpow 1)
	 (setq result
	       (add result
		    (augmult (mul coef (den1numn 1 c b 0 x))))
	       count 2)
	 (go loop))
       jump2
       ;; sqrt(R)/x^2
       (when (= controlpow 2)
	 (setq result
	       (add result
		    (augmult (mul coef
				  (denmnumn '(t (2 1)) 1 c b 0 x))))
	       count 3)
	 (go loop))
       jump3
       (setq signb (checksigntm (power b 2)))
       (when (eq signb '$zero)
	 (setq count 5)
	 (go jump5))
       (setq count 4
	     m 0
	     signc (checksigntm *ec-1*))
       (when (eq signc '$positive)
	 (setq res
	       (augmult (mul* 2 exp1
			      `((%log)
				,(add (power (mul c x) 1//2)
				      (power (add b (mul c x)) 1//2))))))
	 (go jump4))
       (setq res
	     (augmult (mul* 2 exp1
			    `((%atan)
			      ,(power (mul c x
					   (inv (add b (mul -1 c x))))
				      1//2)))))
       jump4
       (incf m)
       (setq res (add (augmult (mul -2 (power (polfoo c b 0 x) 1//2)
				    eb-1 (inv (+ m m -1))
				    (power x (- m))))
		      (augmult (mul (div -2 (+ m m -1))
				    c (1- m) eb-1 res))))
       (when (= m m1)
	 (setq res2 res)
	 (go jump4))
       (when (= (1- m) m1)
	 (if (null res2)
	     (return nil))
	 (setq res1 res
	       partres (add (augmult (mul -1
					  (power (polfoo c b 0 x) 1//2)
					  (r1m m)
					  (power x (- m))))
			    (augmult (mul b 1//2 (r1m m) res1))
			    (augmult (mul c (r1m m) res2))))
	 (go on))
       (go jump4)
       jump5
       (setq m controlpow)
       (when (zerop m)
	 (setq partres (mul* exp1 `((%log) ,x)))
	 (go on))
       (setq partres (mul -1 exp1 (power x (- m)) (r1m m)))
       on
       (setq result (add result (augmult (mul coef partres))))
       loop
       (setq negpowlist (cdr negpowlist))
       (when (null negpowlist) (return result))
       (setq coef (cadar negpowlist)
	     controlpow (caar negpowlist))
       (when (= count 5) (go jump5))
       (when (= count 1) (go jump1))
       (when (= count 2) (go jump2))
       (when (= count 3) (go jump3))
       (setq m1 (- controlpow 2))
       (when (= m1 m)
	 (setq res2 res1))
       (go jump4))))

(defun r1m (m)
  (div 1 m))

;; Integrate (c*x^2+b*x)^(p-1/2)
(defun case0 (power c b x)
  (let ((exp1 '((rat simp) 1 4))
	(exp2 (add b (mul 2 c x)))
	(exp3 (power c '((rat simp) -3 2)))
	(exp4 (add (mul 2 c x) (mul -1 b))))
    ;; exp1 = 1/4
    ;; exp2 = b+2*c*x
    ;; exp3 = 1/c^(3/2)
    ;; exp4 = 2*c*x-b
    (declare (special *ec-1*))
    (prog (signc p result)
       (setq signc (checksigntm *ec-1*)
	     p 1)
       ;; sqrt(c*x^2+b*x)
       ;;
       ;; This could be handled by numn.  Why don't we?
       (when (eq signc '$positive)
	 (setq result
	       (add (augmult (mul exp1 *ec-1* exp2
				  (power (polfoo c b 0 x) 1//2)))
		    (augmult (mul* b b '((rat) -1 8)
				   exp3
				   `((%log)
				     ,(add exp2
					   (mul 2
						(power c 1//2)
						(power (polfoo c b 0 x) 1//2)))))))))
       (when (eq signc '$negative)
	 (setq result
	       (add (augmult (mul exp1 *ec-1* exp4
				  (power (polfoo (mul -1 c) b 0 x) 1//2)))
		    (augmult (mul* b b '((rat) 1 8)
				   exp3
				   `((%asin) ,(mul (inv b) exp4)))))))
       loop
       (when (equal power p) (return result))
       (incf p 2)

       ;; integrate(sqrt(R^(2*n+1)),x) =
       ;;   (2*c*x+b)/4/(n+1)/c*sqrt(R^(2*n+1))
       ;;     + (2*n+1)*del/8/(n+1)/c*integrate(sqrt(R^(2*n-1)),x)

       (setq result (add (augmult (mul 1//2 *ec-1* (inv (1+ p)) exp2
				       (power (polfoo c b 0 x)
					      (div p 2))))
			 (augmult (mul p b b '((rat simp) -1 4)
				       *ec-1* (inv (1+ p)) result))))
       (go loop))))

;; Integrate R^(p-1/2)/x, p >= 1.
(defun den1numn (p c b a x)
  (cond ((= p 1)
	 ;; integrate(sqrt(R)/x,x)
	 ;;
	 ;; G&R 2.267 eq. 1
	 ;;
	 ;; integrate(sqrt(R)/x,x) =
	 ;;  sqrt(R)
	 ;;    + a*integrate(1/x/sqrt(R),x)
	 ;;    + b/2*integrate(1/sqrt(R),x)
	 (add (power (polfoo c b a x) 1//2)
	      (augmult (mul a (den1den1 c b a x)))
	      (augmult (mul b 1//2 (den1 c b a x)))))
	(t
	 ;; General case
	 ;;
	 ;; G&R 2.265
	 ;;
	 ;; integrate(sqrt(R^(2*p-1)/x,x) =
	 ;;   R^(p-1/2)/(2*p-1)
	 ;;     + b/2*integrate(sqrt(R^(2*p-3)),x)
	 ;;     + a*integrate(sqrt(2^(2*p-3))/x,x)
	 (add (augmult (mul (power (polfoo c b a x)
				   (add p -1//2))
			    (inv (+ p p -1))))
	      (augmult (mul a (den1numn (+ p -1) c b a x)))
	      (augmult (mul b 1//2 (numn (+ p -2) c b a x)))))))

;; L is a list of expressions that INTIRA should be applied to.
;; Sum up the results of applying INTIRA to each.
(defun distrint (l x)
  (addn (mapcar #'(lambda (e)
		    (let ((ie (intira e x)))
		      (if ie 
			  ie 
			`((%integrate simp) ,e ,x)))) 
		l)
	t))
