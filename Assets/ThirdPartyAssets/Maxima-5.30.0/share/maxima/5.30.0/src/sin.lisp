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

(macsyma-module sin)

;;; Reference:  J. Moses, Symbolic Integration, MIT-LCS-TR-047, 12-1-1967.
;;; http://www.lcs.mit.edu/publications/pubs/pdf/MIT-LCS-TR-047.pdf.
;;;;
;;;; Unfortunately, some important pages in the scan are all black.
;;;;
;;;; A version with the missing pages is available (2008-12-14) from
;;;; http://www.softwarepreservation.org/projects/LISP/MIT

(declare-top (special $radexpand $%e_to_numlog quotind l ans arcpart coef
		      aa powerlist *a* *b* k stack e w y expres arg var
		      *powerl* *c* *d* exp varlist genvar $liflag $opsubst))

(defvar *debug-integrate* nil
  "Enable debugging for the integrator routines.")

;; When T do not call the risch integrator. This flag can be set by the risch 
;; integrator to avoid endless loops when calling the integrator from risch.
(defvar *in-risch-p* nil)

(defmacro op (frob)
  `(get ,frob 'operators))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Predicate functions

(declaim (inline varp))
(defun varp (x)
  (alike1 x var))

(defun integerp1 (x)
  "Returns 2*x if 2*x is an integer, else nil"
  (integerp2 (mul2* 2 x)))

(defun integerp2 (x)
  "Returns x if x is an integer, else false"
  (let (u)
    (cond ((not (numberp x)) nil)
	  ((not (floatp x)) x)
	  ((prog2 (setq u (maxima-rationalize x))
	       (equal (cdr u) 1)) (car u)))))

;; This predicate is used with m2 pattern matcher.
;; A rational expression in var.
(defun rat8 (ex)
  (cond ((or (varp ex) (freevar ex))
	 t)
	((member (caar ex) '(mplus mtimes) :test #'eq)
	 (do ((u (cdr ex) (cdr u)))
	     ((null u) t)
	   (if (not (rat8 (car u)))
	       (return nil))))
	((not (eq (caar ex) 'mexpt))
	 nil)
	((integerp (caddr ex))
	 (rat8 (cadr ex)))))

(defun rat8prime (c)
  (and (rat8 c)
       (or (not (mnump c))
           (not (zerop1 c)))))

(defun elem (a)
  (cond ((freevar a) t)
	((atom a) nil)
	((m2 a expres) t)
	(t (every #'elem (cdr a)))))

(defun freevar (a)
  (cond ((atom a) (not (eq a var)))
	((varp a) nil)
	((and (not (atom (car a)))
	      (member 'array (cdar a) :test #'eq))
	 (cond ((freevar (cdr a)) t)
	       (t (merror "~&FREEVAR: variable of integration appeared in subscript."))))
	(t (and (freevar (car a)) (freevar (cdr a))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; possibly a bug: For var = x and *d* =3, we have expand(?subst10(x^9 * (x+x^6))) --> x^5+x^4, but
;; ?subst10(expand(x^9 * (x+x^6))) --> x^5+x^3. (Barton Willis)

(defun subst10 (ex)
  (cond ((atom ex) ex)
	((and (eq (caar ex) 'mexpt) (eq (cadr ex) var))
	 (list '(mexpt) var (integerp2 (quotient (caddr ex) *d*))))
	(t (cons (remove 'simp (car ex))
		 (mapcar #'(lambda (c) (subst10 c)) (cdr ex))))))

(defun rationalizer (x)
  (let ((ex (simplify ($factor x))))
    (if (not (alike1 ex x)) ex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stage II of the Integrator
;;;
;;; Check if the problem can be transformed or solved by special methods.
;;; 11 Methods are implemented by Moses, some more have been added.

(defun intform (expres &aux w)
  (cond ((freevar expres) nil)
        ((atom expres) nil)
        
        ;; Map the function intform over the arguments of a sum or a product
	((member (caar expres) '(mplus mtimes) :test #'eq)
	 (let ((l (cdr expres)))
	   (prog (y)
	    loop (cond ((setq y (intform (car l))) (return y))
		       ((not (setq l (cdr l))) (return nil))
	           (t (go loop))))))
        
        ((or (eq (caar expres) '%log)
             (arcp (caar expres)))
         (cond
           ;; Method 9: Rational function times a log or arctric function
	   ((setq arg (m2 exp
			  `((mtimes) ((,(caar expres)) (b rat8))
			    ((coefftt) (c rat8prime)))))
	    ;; Integrand is of the form R(x)*F(S(x)) where F is a log, or 
	    ;; arctric function and R(x) and S(x) are rational functions.
	    (ratlog exp var (cons (cons 'a expres) arg)))
	   (t
	    (prog (y z)
	       (cond
	         ((setq y (intform (cadr expres))) (return y))
	         
	         ;; Method 10: Rational function times log(b*x+a)
		 ((and (eq (caar expres) '%log)
		       (setq z (m2-b*x+a (cadr expres)))
		       (setq y (m2 exp
				   '((mtimes)
				     ((coefftt) (c rat8))
				     ((coefftt) (d elem))))))
		  (return
		    (let ((a (cdr (assoc 'a z :test #'eq)))
			  (b (cdr (assoc 'b z :test #'eq)))
			  (c (cdr (assoc 'c y :test #'eq)))
			  (d (cdr (assoc 'd y :test #'eq)))
		          (newvar (gensym "intform")))
		      ;; keep var from appearing in questions to user
		      (putprop newvar t 'internal)
		      ;; Substitute y = log(b*x+a) and integrate again
		      (substint
		       expres
		       newvar
		       (integrator
			(muln
			 (list (maxima-substitute
				`((mquotient) ((mplus) ((mexpt) $%e ,newvar)
					       ((mtimes) -1 ,a))
				  ,b)
				var
				c)
			       `((mquotient) ((mexpt) $%e ,newvar) ,b)
			       (maxima-substitute newvar expres d))
			 nil)
			newvar)))))
		 (t (return nil)))))))
      
      ;; We have a special function with an integral on the property list.
      ;; After the integral property was defined for the trig functions,
      ;; in rev 1.52, need to exclude trig functions here.
      ((and (not (atom (car expres)))
            (not (optrig (caar expres)))
	    (not (eq (caar expres) 'mexpt))
	    (get (caar expres) 'integral))
       (when *debug-integrate*
	 (format t "~&INTFORM: found 'INTEGRAL on property list~%"))
       (cond
	 ((setq arg
	    (m2 exp `((mtimes) ((,(caar expres)) (b rat8)) ((coefftt) (c rat8prime)))))
	  ;; A rational function times the special function.
	  ;; Integrate with the method integration-by-parts.
	  (partial-integration (cons (cons 'a expres) arg) var))
	 ;; The method of integration-by-parts can not be applied.
	 ;; Maxima tries to get a clue for the argument of the function which
	 ;; allows a substitution for the argument.
	 ((intform (cadr expres)))
	 (t nil)))
        
        ;; Method 6: Elementary function of trigonometric functions
	((optrig (caar expres))
	 (cond ((not (setq w (m2-b*x+a (cadr expres))))
		(intform (cadr expres)))
	       (t
		(prog2
		  (setq *powerl* t)
		  (monstertrig exp var (cadr expres))))))
        
	((and (eq (caar expres) '%derivative)
	      (eq (caar exp) (caar expres))
	      (or (atom (cadr exp))
		  (not (eq (caaadr exp) 'mqapply))
		  (merror (intl:gettext "integrate: invalid argument: ~M") exp))
	      (checkderiv exp)))
        
        ;; Stop intform if we have not a power function.
        ((not (eq (caar expres) 'mexpt)) nil)
        
        ;; Method 2: Substitution for an integral power
        ((integerp (caddr expres)) (intform (cadr expres)))
        
        ;; Method 1: Elementary function of exponentials
        ((freevar (cadr expres))
         (cond ((setq w (m2-b*x+a (caddr expres)))
                (superexpt exp var (cadr expres) w))
               ((intform (caddr expres)))
               ((and (eq '$%e (cadr expres))
                     (isinop (caddr expres) '%log))
                ;; Found something like exp(r*log(x))
                (let* (($%e_to_numlog t)
                       ($radexpand nil) ; do not simplify sqrt(x^2) -> abs(x)
                       (nexp (resimplify exp)))
                  (cond ((alike1 exp nexp) nil)
                        (t (integrator (setq exp nexp) var)))))
               (t nil)))
        
        ;; The base is not a rational function. Try to get a clue for the base.
	((not (rat8 (cadr expres)))
	 (intform (cadr expres)))
        
        ;; Method 3: Substitution for a rational root
	((and (setq w (m2-ratrootform (cadr expres))) ; e*(a*x+b) / (c*x+d)
	      (denomfind (caddr expres))) ; expon is ratnum
	 (cond ((setq w (prog2
			  (setq *powerl* t)
			  (ratroot exp var (cadr expres) w))) w)
	   (t (inte exp var))))
        
        ;; Method 4: Binomial - Chebyschev
	((not (integerp1 (caddr expres))) ; 2*exponent not integer
	 (cond ((m2-chebyform exp)
		(chebyf exp var))
	       (t (intform (cadr expres)))))
        
        ;; Method 5: Arctrigonometric substitution
	((setq w (m2-c*x^2+b*x+a (cadr expres))) ; sqrt(c*x^2+b*x+a)
	 #+nil
	 (format t "expres = sqrt(c*x^2+b*x+a)~%")
	 ;; I think this is method 5, arctrigonometric substitutions.
	 ;; (Moses, pg 80.)  The integrand is of the form
	 ;; R(x,sqrt(c*x^2+b*x+a)).  This method first eliminates the b
	 ;; term of the quadratic, and then uses an arctrig substitution.
	 (inte exp var))
        
        ;; Method 4: Binomial - Chebyschev
	((m2-chebyform exp )
	 (chebyf exp var))
        
        ;; Expand expres.
        ;; Substitute the expanded factor into the integrand and try again.
	((not (m2 (setq w ($expand (cadr expres)))
                (cadr expres)))
	 (prog2
	   (setq exp (maxima-substitute w (cadr expres) exp))
	   (intform (simplify (list '(mexpt) w (caddr expres))))))
        
        ;; Factor expres.
        ;; Substitute the factored factor into the integrand and try again.
	((setq w (rationalizer (cadr expres)))
	 ;; The forms below used to have $radexpand set to $all.  But I
	 ;; don't think we really want to do that here because that makes
	 ;; sqrt(x^2) become x, which might be totally wrong.  This is one
	 ;; reason why we returned -4/3 for the
	 ;; integrate(sqrt(x+1/x-2),x,0,1).  We were replacing
	 ;; sqrt((x-1)^2) with x - 1, which is totally wrong since 0 <= x
	 ;; <= 1.
	 (setq exp (let (($radexpand $radexpand))
		     (maxima-substitute w (cadr expres) exp)))
	 (intform (let (($radexpand '$all))
		    (simplify (list '(mexpt) w (caddr expres))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun separc (ex)
  (cond ((arcfuncp ex) (setq arcpart ex coef 1))
	((eq (caar ex) 'mtimes)
	 (arclist (cdr ex))
	 (setq coef (cond ((null (cdr coef)) (car coef))
			  (t (setq coef (cons (car ex) coef))))))))

(defun arclist (list)
  (cond ((null list) nil)
	((and (arcfuncp (car list)) (null arcpart))
	 (setq arcpart (car list)) (arclist (cdr list)))
	(t (setq coef (cons (car list) coef))
	   (arclist (cdr list)))))

(defun arcfuncp (ex)
  (and (not (atom ex))
       (or (arcp (caar ex))
	   (eq (caar ex) '%log)     ; Experimentally treat logs also.
	   (and (eq (caar ex) 'mexpt)
		(integerp2 (caddr ex))
		(> (integerp2 (caddr ex)) 0)
		(arcfuncp (cadr ex))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Five pattern for the Integrator and other routines.

;; This is matching the pattern e*(a*x+b)/(c*x+d), where
;; a, b, c, d, and e are free of x, and x is the variable of integration.
(defun m2-ratrootform (expr)
  (m2 expr
      `((mtimes)
        ((coefftt) (e freevar))
        ((mplus)
         ((coeffpt) (a freevar) (var varp))
         ((coeffpt) (b freevar)))
        ((mexpt)
         ((mplus)
          ((coeffpt) (c freevar) (var varp))
          ((coeffpt) (d freevar)))
         -1))))

;; This is for matching the pattern a*x^r1*(c1+c2*x^q)^r2.
(defun m2-chebyform (expr)
  (m2 expr
      `((mtimes)
        ((mexpt) (var varp) (r1 numberp))
        ((mexpt)
         ((mplus)
          ((mtimes)
           ((coefftt) (c2 freevar))
           ((mexpt) (var varp) (q free1)))
          ((coeffpp) (c1 freevar)))
         (r2 numberp))
        ((coefftt) (a freevar)))))

;; Pattern to match b*x + a
(defun m2-b*x+a (expr)
  (m2 expr
      `((mplus)
        ((coeffpt) (b freevar) (x varp))
        ((coeffpt) (a freevar)))))

;; This is the pattern c*x^2 + b * x + a.
(defun m2-c*x^2+b*x+a (expr)
  (m2 expr
      `((mplus)
        ((coeffpt) (c freevar) ((mexpt) (x varp) 2))
        ((coeffpt) (b freevar) (x varp))
        ((coeffpt) (a freevar)))))

;; This is the pattern (a*x+b)*(c*x+d)
(defun m2-a*x+b/c*x+d (expr)
  (m2 expr
      `((mtimes)
        ((mplus)
         ((coeffpt) (a freevar) (var varp))
         ((coeffpt) (b freevar)))
        ((mplus)
         ((coeffpt) (c freevar) (var varp))
         ((coeffpt) (d freevar))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is the main integration routine.  It is called from sinint.

(defun integrator (exp var)
  (prog (y arg *powerl* const *b* w arcpart coef integrand result)
     (declare (special *integrator-level*))
     ;; Increment recursion counter
     (incf *integrator-level*)
     
     ;; Trivial case. exp is not a function of var.
     (if (freevar exp) (return (mul2* exp var)))
     
     ;; Remove constant factors
     (setq w (partition exp var 1))
     (setq const (car w))
     (setq exp (cdr w))
     #+nil
     (progn
       (format t "w = ~A~%" w)
       (format t "const = ~A~%" const)
       (format t "exp = ~A~%" exp))
     
     (cond ;; First stage, Method I: Integrate a sum.
           ((mplusp exp)
            (return (mul2* const (integrate1 (cdr exp)))))
           
           ;; Convert atan2(a,b) to atan(a/b) and try again.
           ((setq w (isinop exp '$atan2))
            (setq exp
                  (maxima-substitute (take '(%atan) (div (cadr w) (caddr w)))
                                     w
                                     exp))
            (return (mul* const
                          (integrator exp var))))
           
           ;; First stage, Method II: Integrate sums.
	   ((and (not (atom exp))
		 (eq (caar exp) '%sum))
	    (return (mul2* const (intsum exp var))))
           
           ;; First stage, Method III: Try derivative-divides method.
           ;; This is the workhorse that solves many integrals.
           ((setq y (diffdiv exp var))
	    (return (mul2* const y))))
     
     ;; At this point, we have EXP as a product of terms.  Make Y a
     ;; list of the terms of the product.
     (setq y (cond ((eq (caar exp) 'mtimes)
		    (cdr exp))
		   (t
		    (list exp))))
     
     ;; Second stage:
     ;; We're looking at each term of the product and check if we can
     ;; apply one of the special methods.
     loop
     #+nil
     (progn
       (format t "car y =~%")
       (maxima-display (car y)))
     (cond ((rat8 (car y))
	    #+nil
	    (format t "In loop, go skip~%")
	    (go skip))
	   ((and (setq w (intform (car y)))
		 ;; Do not return a noun form as result at this point, because
		 ;; we would like to check for further special integrals.
		 ;; We store the result for later use.
		 (setq result w)
		 (not (isinop w '%integrate)))
	    #+nil
	    (format t "In loop, case intform~%")
	    (return (mul2* const w)))
	   (t
	    #+nil
	    (format t "In loop, go special~%")
	    ;; Store a possible partial result
	    (setq result w)
	    (go special)))
     skip
     (setq y (cdr y))
     (cond ((null y)
            ;; Method 8: Rational functions
	    (return (mul2* const (cond ((setq y (powerlist exp var)) y)
				       (t (ratint exp var)))))))
     (go loop)
        
     special
     ;; Third stage: Try more general methods
     
     ;; SEPARC SETQS ARCPART AND COEF SUCH THAT
     ;; COEF*ARCEXP=EXP WHERE ARCEXP IS OF THE FORM
     ;; ARCFUNC^N AND COEF IS ITS ALGEBRAIC COEFFICIENT
     (separc exp)
     
     #+nil
     (progn
       (format t "arcpart = ~A~%" arcpart)
       (format t "coef =~%")
       (maxima-display coef))
     (cond ((and (not (null arcpart))
		 (do  ((stacklist stack (cdr stacklist)))
		      ((null stacklist) t)
		   (cond ((alike1 (car stacklist) coef)
			  (return nil))))
		 (not (isinop (setq w (let ((stack (cons coef stack)))
					(integrator coef var)))
			      '%integrate))
		 (setq integrand (mul2 w (sdiff arcpart var)))
		 (do ((stacklist stack (cdr stacklist)))
		     ((null stacklist) t)
		   (cond ((alike1 (car stacklist) integrand)
			  (return nil))))
		 (not (isinop
		       (setq y (let ((stack (cons integrand stack))
				     (integ integrand))
				 (integrator integ var)))
		       '%integrate)))
	    (return (add* (list '(mtimes) const w arcpart)
			  (list '(mtimes) -1 const y))))
	   (t
	    (return
		(mul* const
		      (cond ((setq y (scep exp var))
			     (cond ((cddr y)
				    #+nil
				    (progn
				      (format t "cddr y =~%")
				      (maxima-display (cddr y)))
				    (integrator ($trigreduce exp) var))
				   (t (sce-int (car y) (cadr y) var))))
			    ;; I don't understand why we do this. This
			    ;; causes the stack overflow in Bug
			    ;; 1487703, because we keep expanding exp
			    ;; into a form that matches the original
			    ;; and therefore we loop forever.  To
			    ;; break this we keep track how how many
			    ;; times we've tried this and give up
			    ;; after 4 (arbitrarily selected) times.
			    ((and (< *integrator-level* 4)
				  (not (alike1 exp (setq y ($expand exp)))))
			     #+nil
			     (progn
			       (format t "exp = ~A~%" exp)
			       (maxima-display exp)
			       (format t "y   = ~A~%" y)
			       (maxima-display y)
			       (break))
			     (integrator y var))
			    ((and (not *powerl*)
				  (setq y (powerlist exp var)))
			     y)
			    ((and (not *in-risch-p*)  ; Not called from rischint
			          (setq y (rischint exp var))
				  ;; rischint has not found an integral but
				  ;; returns a noun form. Do not return that
				  ;; noun form as result at this point, but
				  ;; store it for later use.
				  (setq result y)
				  (not (isinop y '%integrate)))
			     y)
			    ((setq y (integrate-exp-special exp var))
			     ;; Maxima found an integral for a power function
			     y)
			    (t
			     ;; Integrate-exp-special has not found an integral
			     ;; We look for a previous result obtained by
			     ;; intform or rischint.
			     (if result
				 result
				 (list '(%integrate) exp var))))))))))

(defun optrig (x)
  (member x '(%sin %cos %sec %tan %csc %cot) :test #'eq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stage I
;;; Implementation of Method 1: Integrate a sum

;;after finding a non-integrable summand usually better to pass rest to risch
(defun integrate1 (exp)
  (do ((terms exp (cdr terms)) (ans))
      ((null terms) (addn ans nil))
    (let ($liflag)					; don't gen li's for
      (push (integrator (car terms) var) ans))		; parts of integrand
    (when (and (not *in-risch-p*)                     ; Not called from rischint
               (not (free (car ans) '%integrate))
               (cdr terms))
	  (return (addn (cons (rischint (cons '(mplus) terms) var) (cdr ans))
			nil)))))

(defun scep (expr var &aux trigl exp)	; Product of SIN, COS, EXP
  (and (mtimesp expr)			;	of linear args.
       (loop for fac in (cdr expr) do
	     (cond ((atom fac) (return nil))
		   ((trig1 (car fac))
		    (if (linearp (cadr fac) var) (push fac trigl)
			(return nil)))
		   ((and (mexptp fac)
			 (eq (cadr fac) '$%e)
			 (linearp (caddr fac) var))
		    ;; should be only one exponential factor
		    (setq exp fac))
		   (t (return nil)))
	     finally (return (cons exp trigl)))))

;; Integrates exponential * sin or cos, all with linear args.

(defun sce-int (exp s-c var)		; EXP is non-trivial
  (let* ((e-coef (car (islinear (caddr exp) var)))
         (sc-coef (car (islinear (cadr s-c) var)))
         (sc-arg (cadr s-c))
         (abs-val (add (power e-coef 2) (power sc-coef 2))))
    (if (zerop1 abs-val)
        ;; The numerator is zero. Exponentialize the integrand and try again.
        (integrator ($exponentialize (mul exp s-c)) var)
        (mul (div exp abs-val)
             (add (mul e-coef s-c)
                  (if (eq (caar s-c) '%sin)
                      (mul* (neg sc-coef) `((%cos) ,sc-arg))
                      (mul* sc-coef `((%sin) ,sc-arg))))))))

(defun checkderiv (expr)
  (checkderiv1 (cadr expr) (cddr expr) () ))

;; CHECKDERIV1 gets called on the expression being differentiated,
;; an alternating list of variables being differentiated with
;; respect to and powers thereof, and a reversed list of the latter
;; that have already been examined.  It returns either the antiderivative
;; or (), saying this derivative isn't wrt the variable of integration.

(defun checkderiv1 (expr wrt old-wrt)
  (cond ((varp (car wrt))
	 (if (equal (cadr wrt) 1)	;Power = 1?
	     (if (null (cddr wrt))	;single or partial
		 (if (null old-wrt)
		     expr		;single
		     `((%derivative), expr ;partial in old-wrt
		       ,.(nreverse old-wrt)))
		 `((%derivative) ,expr	;Partial, return rest
		   ,.(nreverse old-wrt)
		   ,@(cddr wrt)))
	     `((%derivative) ,expr	;Higher order, reduce order
	       ,.(nreverse old-wrt)
	       ,(car wrt) ,(add* (cadr wrt) -1)
	       ,@ (cddr wrt))))
	((null (cddr wrt)) () )		;Say it doesn't apply here
	(t (checkderiv1 expr (cddr wrt)	;Else we check later terms
			(list* (cadr wrt) (car wrt) old-wrt)))))

(defun integrallookups (exp)
  (let (form dummy-args real-args)
  (cond
	((eq (caar exp) 'mqapply)
	 ;; Transform to functional form and try again.
	 ;; For example:
	 ;; ((MQAPPLY SIMP) (($PSI SIMP ARRAY) 1) $X)
	 ;; => (($PSI) 1 $X)
	 (integrallookups `((,(caaadr exp)) ,@(cdadr exp) ,@(cddr exp))))

	;; Lookup algorithm for integral of a special function. 
	;; The integral form is put on the property list, and can be a 
	;; lisp function of the args.  If the form is nil, or evaluates 
        ;; to nil, then return noun form unevaluated.
	((and (not (atom (car exp)))
	    (setq form (get (caar exp) 'integral))
	    (setq dummy-args (car form))
	    (setq real-args (cdr exp))
	    ;; search through the args of exp and find the arg containing var
	    ;; look up the integral wrt this arg from form
	    (setq form
	      (do ((x real-args (cdr x))
		   (y (cdr form) (cdr y)))
		  ((or (null x) (null y)) nil)
		  (if (not (freevar (car x))) (return (car y)))))
	    ;; If form is a function then evaluate it with actual args
	    (or (not (functionp form))
		(setq form (apply form real-args))))
	 (when *debug-integrate*
	   (format t "~&INTEGRALLOOKUPS: Found integral ~A.~%" (caar exp)))
	 (substitutel real-args dummy-args form))

	((eq (caar exp) 'mplus)
	 (muln (list '((rat simp) 1 2) exp exp) nil))

	(t nil))))

;; Integrals of elementary special functions
;; This may not be the best place for this definition, but it is close 
;; to the original code.
(defprop %log  ((x) ((mplus) ((mtimes) x ((%log) x)) ((mtimes) -1 x))) integral)
(defprop %sin  ((x) ((mtimes) -1 ((%cos) x))) integral)
(defprop %cos  ((x) ((%sin) x)) integral)
(defprop %tan  ((x) ((%log) ((%sec) x))) integral)
(defprop %csc  ((x) ((mtimes) -1 ((%log) ((mplus) ((%csc) x) ((%cot) x))))) integral)
(defprop %sec  ((x) ((%log) ((mplus) ((%sec) x) ((%tan) x)))) integral)
(defprop %cot  ((x) ((%log) ((%sin) x))) integral)
(defprop %sinh ((x) ((%cosh) x))  integral)
(defprop %cosh ((x) ((%sinh) x)) integral)
(defprop %tanh ((x) ((%log) ((%cosh) x))) integral)
(defprop %coth ((x) ((%log) ((%sinh) x))) integral)
(defprop %sech ((x) ((%atan) ((%sinh)x))) integral)
(defprop %csch ((x) ((%log) ((%tanh) ((mtimes) ((rat simp) 1 2) x)))) integral)

;; Integral of a^b == ((mexpt) a b)
(putprop 'mexpt
  `((a b)
  ;;integrate(a^b,a);
  ,(lambda (a b)
    (cond
      ((or (equal b -1)
	   (and (not (mnump b))
		(freeof '$%i b)
		(eq (asksign (power (add b 1) 2)) '$zero)))
         (logmabs a))
      (t
       '((mtimes) ((mexpt) a ((mplus) b 1)) ((mexpt) ((mplus) b 1) -1)))))
  ;; integrate(a^b,b);
  ((mtimes) ((mexpt) a b) ((mexpt) ((%log) a) -1)))
  'integral)

(defun rat10 (ex)
  (cond ((freevar ex) t)
	((varp ex) nil)
	((eq (caar ex) 'mexpt)
	 (if (varp (cadr ex))
	     (if (integerp2 (caddr ex))
		 (setq powerlist (cons (caddr ex) powerlist)))
	     (and (rat10 (cadr ex)) (rat10 (caddr ex)))))
	((member (caar ex) '(mplus mtimes) :test #'eq)
	 (do ((u (cdr ex) (cdr u))) ((null u) t)
	     (if (not (rat10 (car u))) (return nil))))
	(t
	 (let ((examine (margs ex)))
; Commenting out the if-clause, go in all cases through the list of arguments.
;	   (if (atom (first examine))
	       (do* ((element examine (rest element))
		     (result (rat10 (first examine))
			     (and result (rat10 (first element)))))
		   ((or (null result) (null element)) result))))))
;	     (rat10 (first examine)))))))

(defun integrate5 (ex var)
  (if (rat8 ex)
      (ratint ex var)
      (integrator ex var)))

(defun denomfind (x)
  (cond ((ratnump x) (caddr x))
	((not (numberp x)) nil)
	((not (floatp x)) 1)
	(t (cdr (maxima-rationalize x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stage II
;;; Implementation of Method 1: Elementary function of exponentials
;;;
;;; The following examples are integrated with this method:
;;;
;;;   integrate(exp(x)/(2+3*exp(2*x)),x)
;;;   integrate(exp(x+1)/(1+exp(x)),x)
;;;   integrate(10^x*exp(x),x)

(let ((bas nil)       ; The common base.
      (pow nil)       ; The common power of the form b*x+a. The values are
                      ; stored in a list which is returned from m2.
      (exptflag nil)) ; When T, the substitution is not possible.
  
  (defun superexpt (exp var bas1 pow1)
    (prog (y)
      (setq bas bas1
            pow pow1
            exptflag nil)
      ;; Transform the integrand. At this point resimplify, because it is not
      ;; guaranteed, that a correct simplified expression is returned.
      (setq y (resimplify (elemxpt exp)))
      (when exptflag (return nil))
      ;; Integrate the transformed integrand and substitute back.
      (return
        ($multthru
          (substint (list '(mexpt) bas
                          (list '(mplus) (cdras 'a pow)
                                (list '(mtimes) (cdras 'b pow) var)))
                    var
                    (integrator (div y
                                     (mul var
                                          (cdras 'b pow)
                                          (take '(%log) bas))) var))))))
  
  ;; Transform expressions like g^(b*x+a) to the common base bas and
  ;; do the substitution y = bas^(b*x+a) in the expr.
  (defun elemxpt (expr &aux w)
    (cond ((freevar expr) expr)
          ;; var is the base of a subexpression. The transformation fails.
          ((atom expr) (setq exptflag t))
          ((not (eq (caar expr) 'mexpt))
           (cons (car expr)
                 (mapcar #'(lambda (c) (elemxpt c)) (cdr expr))))
          ((not (freevar (cadr expr)))
           (list '(mexpt)
                 (elemxpt (cadr expr))
                 (elemxpt (caddr expr))))
          ;; Transform the expression to the common base.
          ((not (eq (cadr expr) bas))
           (elemxpt (list '(mexpt)
                          bas
                          (mul (power (take '(%log) bas) -1)
                               (take '(%log) (cadr expr))
                               (caddr expr)))))
          ;; The exponent must be linear in the variable of integration.
          ((not (setq w (m2-b*x+a (caddr expr))))
           (list (car expr) bas (elemxpt (caddr expr))))
          ;; Do the substitution y = g^(b*x+a).
          (t
           (setq w (cons (cons 'bb (cdras 'b pow)) w))
           (setq w (cons (cons 'aa (cdras 'a pow)) w))
           (setq w (cons (cons 'bas bas) w))
           (subliss w '((mtimes)
                        ((mexpt) bas a)
                        ((mexpt)
                         bas
                         ((mquotient)
                          ((mtimes) -1 aa b) bb))
                        ((mexpt)
                         x
                         ((mquotient) b bb)))))))
) ; End of let

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stage II
;;; Implementation of Method 3:
;;; Substitution for a rational root of a linear fraction of x
;;;
;;; This method is applicable when the integrand is of the form:
;;;
;;;   /
;;;   [       a x + b n1/m1   a x + b n1/m2
;;;   I R(x, (-------)   ,   (-------)     , ...) dx
;;;   ]       c x + d         c x + d
;;;   /
;;;
;;; Substitute 
;;;
;;;    (1) t = ((a*x+b)/(c*x+d))^(1/k), or
;;;
;;;    (2) x = (b-d*t^k)/(c*t^k-a)
;;;
;;; where k is the least common multiplier of m1, m2, ... and
;;;
;;;    (3) dx = k*(a*d-b*c)*t^(k-1)/(a-c*t^k)^2 * dt
;;;
;;; First, the algorithm calls the routine RAT3 to collect the roots of the
;;; form ((a*x+b)/(c*x+d))^(n/m) in the list *ROOTLIST*.
;;; search for the least common multiplier of m1, m2, ... then the
;;; substitutions (2) and (3) are done and the new problem is integrated.
;;; As always, W is an alist which associates to the coefficients
;;; a, b... (and to VAR) their values.

(defvar *ratroot* nil)  ; Expression of the form (a*x+b)/(c*x+d)
(defvar *rootlist* nil) ; List of powers of the expression *ratroot*.

(defun ratroot (exp var *ratroot* w)
  (prog (*rootlist* k y w1)
     ;; Check if the integrand has a chebyform, if so return the result.
     (when (setq y (chebyf exp var)) (return y))
     ;; Check if the integrand has a suitably form and collect the roots
     ;; in the global special variable *ROOTLIST*.
     (unless (rat3 exp t) (return nil))
     ;; Get the least common multiplier of m1, m2, ...
     (setq k (apply #'lcm *rootlist*))
     (setq w1 (cons (cons 'k k) w))
     ;; Substitute for the roots.
     (setq y
           (subst41 exp
                    (subliss w1
                             '((mquotient)
                               ((mplus) ((mtimes) b e)
                                ((mtimes) -1 d ((mexpt) var k)))
                               ((mplus) ((mtimes) c ((mexpt) var k))
                                ((mtimes) -1 e a))))
                    var))
     ;; Integrate the new problem.
     (setq y
           (integrator
             (mul y
                  (subliss w1
                           '((mquotient)
                             ((mtimes) e
                              ((mplus)
                               ((mtimes) a d k
                                ((mexpt) var ((mplus) -1 k)))
                               ((mtimes) -1
                                ((mtimes) b c k
                                 ((mexpt) var ((mplus) -1 k))))))
                             ((mexpt) ((mplus)
                                       ((mtimes) c ((mexpt) var k))
                                       ((mtimes) -1 a e))
                              2))))
             var))
     ;; Substitute back and return the result.
     (return (substint (power *ratroot* (power k -1)) var y))))

(defun rat3 (ex ind)
  (cond ((freevar ex) t)
	((atom ex) ind)
	((member (caar ex) '(mtimes mplus) :test #'eq)
	 (do ((u (cdr ex) (cdr u)))
	     ((null u) t)
	   (if (not (rat3 (car u) ind))
	       (return nil))))
	((not (eq (caar ex) 'mexpt))
	 (rat3 (car (margs ex)) t))
	((freevar (cadr ex))
	 (rat3 (caddr ex) t))
	((integerp (caddr ex))
	 (rat3 (cadr ex) ind))
        ((and (m2 (cadr ex) *ratroot*)
	      (denomfind (caddr ex)))
         (setq *rootlist* (cons (denomfind (caddr ex)) *rootlist*)))
        (t (rat3 (cadr ex) nil))))

(let ((rootform nil) ; Expression of the form x = (b*e-d*t^k)/(c*t^k-e*a).
      (rootvar nil)) ; The variable we substitute for the root.
  
  (defun subst4 (ex)
    (cond ((freevar ex) ex)
          ((atom ex) rootform)
          ((not (eq (caar ex) 'mexpt))
           (mapcar #'(lambda (u) (subst4 u)) ex))
          ((m2 (cadr ex) *ratroot*)
           (list (car ex) rootvar (integerp2 (timesk k (caddr ex)))))
          (t (list (car ex) (subst4 (cadr ex)) (subst4 (caddr ex))))))
  
  (defun subst41 (exp a b)
    (setq rootform a
          rootvar b)
    ;; At this point resimplify, because it is not guaranteed, that a correct 
    ;; simplified expression is returned.
    (resimplify (subst4 exp)))
) ; End of let

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stage II
;;; Implementation of Method 4: Binomial Chebyschev

;; exp = a*t^r1*(c1+c2*t^q)^r2, where var = t.
;;
;; G&S 2.202 has says this integral can be expressed by elementary
;; functions ii:
;;
;; 1. q is an integer
;; 2. (r1+1)/q is an integer
;; 3. (r1+1)/q+r2 is an integer.
;;
;; I (rtoy) think that for this code to work, r1, r2, and q must be numbers.
(defun chebyf (exp var)
  (prog (r1 r2 d1 d2 n1 n2 w q)
     ;; Return NIL if the expression doesn't match.
     (when (not (setq w (m2-chebyform exp)))
       (return nil))
     #+nil
     (format t "w = ~A~%" w)
     (when (zerop1 (cdr (assoc 'c1 w :test #'eq)))
       ;; rtoy: Is it really possible to be in this routine with c1 =
       ;; 0?
       (return
	 (mul*
	  ;; This factor is locally constant as long as t and
	  ;; c2*t^q avoid log's branch cut.
	  (subliss w '((mtimes) a ((mexpt) var ((mtimes) -1 q r2))
		       ((mexpt) ((mtimes) c2 ((mexpt) var q)) r2)))
	  (integrator
	   (subliss w '((mexpt) var ((mplus) r1 ((mtimes) q r2)))) var))))
     (setq q (cdr (assoc 'q w :test #'eq)))
     ;; Reset parameters.  a = a/q, r1 = (1 - q + r1)/q
     (setq w
	   (list* (cons 'a (div* (cdr (assoc 'a w :test #'eq)) q))
		  (cons
		   'r1
		   (div* (addn (list 1 (neg (simplify q)) (cdr (assoc 'r1 w :test #'eq))) nil) q))
		  w))
     #+nil
     (format t "new w = ~A~%" w)
     (setq r1 (cdr (assoc 'r1 w :test #'eq))
	   r2 (cdr (assoc 'r2 w :test #'eq)))
     #+nil
     (progn
       (format t "new r1 = ~A~%" r1)
       (format t "r2     = ~A~%" r2))
     ;; Write r1 = d1/n1, r2 = d2/n2, if possible.  Update w with
     ;; these values, if so.  If we can't, give up.  I (rtoy) think
     ;; this only happens if r1 or r2 can't be expressed as rational
     ;; numbers.  Hence, r1 and r2 have to be numbers, not variables.
     (cond
       ((not (and (setq d1 (denomfind r1))
		  (setq d2 (denomfind r2))
		  (setq n1 (integerp2 (timesk r1 d1)))
		  (setq n2 (integerp2 (timesk r2 d2)))
		  (setq w (list* (cons 'd1 d1) (cons 'd2 d2)
				 (cons 'n1 n1) (cons 'n2 n2)
				 w))))
	#+nil
	(progn
	  (format t "cheby can't find one of d1,d2,n1,n2:~%")
	  (format t "  d1 = ~A~%" d1)
	  (format t "  d2 = ~A~%" d2)
	  (format t "  n1 = ~A~%" n1)
	  (format t "  n2 = ~A~%" n2))
	(return nil))
       ((and (integerp2 r1) (> r1 0))
	#+nil (format t "integer r1 > 0~%")
	;; (r1+q-1)/q is positive integer.
	;;
	;; I (rtoy) think we are using the substitution z=(c1+c2*t^q).
	;; Maxima thinks the resulting integral should then be
	;;
	;; a/q*c2^(-r1/q-1/q)*integrate(z^r2*(z-c1)^(r1/q+1/q-1),z)
	;;
	(return
	  (substint
	   (subliss w '((mplus) c1 ((mtimes) c2 ((mexpt) var q))))
	   var
	   (integrator
	    (expands (list (subliss w
				    ;; a*t^r2*c2^(-r1-1)
				    '((mtimes)
				      a
				      ((mexpt) var r2)
				      ((mexpt)
				       c2
				       ((mtimes)
					-1
					((mplus) r1 1))))))
		     (cdr
		      ;; (t-c1)^r1
		      (expandexpt (subliss w
					   '((mplus)
					     var
					     ((mtimes) -1 c1)))
				  r1)))
	    var))))
       ((integerp2 r2)
	#+nil (format t "integer r2~%")
	;; I (rtoy) think this is using the substitution z = t^(q/d1).
	;;
	;; The integral (as maxima will tell us) becomes
	;;
	;; a*d1/q*integrate(z^(n1/q+d1/q-1)*(c1+c2*z^d1)^r2,z)
	;;
	;; But be careful because the variable A in the code is
	;; actually a/q.
	(return
	  (substint (subliss w '((mexpt) var ((mquotient) q d1)))
		    var
		    (ratint (simplify (subliss w
					       '((mtimes)
						 d1 a
						 ((mexpt)
						  var
						  ((mplus)
						   n1 d1 -1))
						 ((mexpt)
						  ((mplus)
						   ((mtimes)
						    c2
						    ((mexpt)
						     var d1))
						   c1)
						  r2))))
			    var))))
       ((and (integerp2 r1) (< r1 0))
	#+nil (format t "integer r1 < 0~%")
	;; I (rtoy) think this is using the substitution
	;;
	;; z = (c1+c2*t^q)^(1/d2)
	;;
	;; With this substitution, maxima says the resulting integral
	;; is
	;;
	;;  a/q*c2^(-r1/q-1/q)*d2*
	;;    integrate(z^(n2+d2-1)*(z^d2-c1)^(r1/q+1/q-1),z)
	(return
	  (substint (subliss w
			     ;; (c1+c2*t^q)^(1/d2)
			     '((mexpt)
			       ((mplus)
				c1
				((mtimes) c2 ((mexpt) var q)))
			       ((mquotient) 1 d2)))
		    var
		    (ratint (simplify (subliss w
					       ;; This is essentially
					       ;; the integrand above,
					       ;; except A and R1 here
					       ;; are not the same as
					       ;; derived above.
					       '((mtimes)
						 a d2
						 ((mexpt)
						  c2
						  ((mtimes)
						   -1
						   ((mplus)
						    r1 1)))
						 ((mexpt)
						  var
						  ((mplus)
						   n2 d2 -1))
						 ((mexpt)
						  ((mplus)
						   ((mexpt)
						    var d2)
						   ((mtimes) -1 c1))
						  r1))))
			    var))))
       ((integerp2 (add* r1 r2))
	#+nil (format t "integer r1+r2~%")
	;; If we're here,  (r1-q+1)/q+r2 is an integer.
	;;
	;; I (rtoy) think this is using the substitution
	;;
	;; z = ((c1+c2*t^q)/t^q)^(1/d1)
	;;
	;; With this substitution, maxima says the resulting integral
	;; is
	;;
	;; a*d2/q*c1^(r2+r1/q+1/q)*
	;;   integrate(z^(d2*r2+d2-1)*(z^d2-c2)^(-r2-r1/q-1/q-1),z)
	(return
	  (substint (let (($radexpand '$all))
		      ;; Setting $radexpand to $all here gets rid of
		      ;; ABS in the subtitution.  I think that's ok in
		      ;; this case.  See Bug 1654183.
		      (subliss w
			       '((mexpt)
				 ((mquotient)
				  ((mplus)
				   c1
				   ((mtimes) c2 ((mexpt) var q)))
				  ((mexpt) var q))
				 ((mquotient) 1 d1))))
		    var
		    (ratint (simplify (subliss w
					       '((mtimes)
						 -1 a d1
						 ((mexpt)
						  c1
						  ((mplus)
						   r1 r2 1))
						 ((mexpt)
						  var
						  ((mplus)
						   n2 d1 -1))
						 ((mexpt)
						  ((mplus)
						   ((mexpt)
						    var d1)
						   ((mtimes)
						    -1 c2))
						  ((mtimes)
						   -1
						   ((mplus)
						    r1 r2
						    2))))))
			    var))))
       (t (return (list '(%integrate) exp var))))))

(defun greaterratp (x1 x2)
  (cond ((and (numberp x1) (numberp x2))
	 (> x1 x2))
	((ratnump x1)
	 (greaterratp (quotient (float (cadr x1))
				(caddr x1))
		      x2))
	((ratnump x2)
	 (greaterratp x1
		      (quotient (float (cadr x2))
				(caddr x2))))))

(defun trig1 (x)
  (member (car x) '(%sin %cos) :test #'eq))

(defun supertrig (exp)
  (declare (special *notsame* *trigarg*))
  (cond ((freevar exp) t)
	((atom exp) nil)
	((member (caar exp) '(mplus mtimes) :test #'eq)
	 (and (supertrig (cadr exp))
	      (or (null (cddr exp))
		  (supertrig (cons (car exp)
				   (cddr exp))))))
	((eq (caar exp) 'mexpt)
	 (and (supertrig (cadr exp))
	      (supertrig (caddr exp))))
	((eq (caar exp) '%log)
	 (supertrig (cadr exp)))
	((member (caar exp)
	       '(%sin %cos %tan %sec %cot %csc) :test #'eq)
	 (cond ((m2 (cadr exp) *trigarg*) t)
               ((m2-b*x+a (cadr exp))
                (and (setq *notsame* t) nil))
	       (t (supertrig (cadr exp)))))
	(t (supertrig (cadr exp)))))

(defun subst2s (ex pat)
  (cond ((null ex) nil)
	((m2 ex pat) var)
	((atom ex) ex)
	(t (cons (subst2s (car ex) pat)
		 (subst2s (cdr ex) pat)))))

;; Match (c*x+b), where c and b are free of x
(defun simple-trig-arg (exp)
  (m2 exp '((mplus) ((mtimes)
		     ((coefftt) (c freevar))
		     ((coefftt) (v varp)))
	    ((coeffpp) (b freevar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stage II
;;; Implementation of Method 6: Elementary function of trigonometric functions

(defun monstertrig (exp var *trigarg*)
  (declare (special *trigarg*))
  (when (and (not (atom *trigarg*))
             ;; Do not exute the following code when called from rischint.
             (not *in-risch-p*))
    (let ((arg (simple-trig-arg *trigarg*)))
      (cond (arg
	     ;; We have trig(c*x+b).  Use the substitution y=c*x+b to
	     ;; try to compute the integral.  Why?  Because x*sin(n*x)
	     ;; takes longer and longer as n gets larger and larger.
	     ;; This is caused by the Risch integrator.  This is a
	     ;; work-around for this issue.
	     (let* ((c (cdras 'c arg))
		    (b (cdras 'b arg))
		    (new-var (gensym "NEW-VAR-"))
		    (new-exp (maxima-substitute (div (sub new-var b) c)
						var exp)))
	       (if (every-trigarg-alike new-exp new-var)
		   ;; avoid endless recursion when more than one
		   ;; trigarg exists or c is a float
		   (return-from monstertrig 
		     (maxima-substitute 
		      *trigarg* 
		      new-var 
		      (div (integrator new-exp new-var) c))))))
	    (t
	     (return-from monstertrig (rischint exp var))))))
  (prog (*notsame* w a b y d)
     (declare (special *notsame*))
     (cond
       ((supertrig exp) (go a))
       ((null *notsame*) (return nil))
       ;; Check for an expression like a*trig1(m*x)*trig2(n*x),
       ;; where trig1 and trig2 are sin or cos.
       ((not (setq y (m2 exp
                         '((mtimes)
                           ((coefftt) (a freevar))
                           (((b trig1))
                            ((mtimes)
                             (x varp)
                             ((coefftt) (m freevar))))
                           (((d trig1))
                            ((mtimes)
                             (x varp)
                             ((coefftt) (n freevar))))))))
        (go b))
; This check has been done with the pattern match.
;       ((not (and (member (car (setq b (cdr (assoc 'b y :test #'eq)))) '(%sin %cos) :test #'eq)
;                  (member (car (setq d (cdr (assoc 'd y :test #'eq)))) '(%sin %cos) :test #'eq)))
;        (return nil))
       ((progn
	  ;; The tests after this depend on values of b and d being
	  ;; set.  Set them here unconditionally, before doing the
	  ;; tests.
	  (setq b (cdras 'b y))
	  (setq d (cdras 'd y))
	  (and (eq (car b) '%sin)
	       (eq (car d) '%sin)))
        ;; We have a*sin(m*x)*sin(n*x).
        ;; The integral is: a*(sin((m-n)*x)/(2*(m-n))-sin((m+n)*x)/(2*(m+n))
        (return (subliss y
                         '((mtimes) a
                           ((mplus)
                            ((mquotient)
                             ((%sin) ((mtimes) ((mplus) m ((mtimes) -1 n)) x))
                             ((mtimes) 2 ((mplus) m ((mtimes) -1 n))))
                            ((mtimes) -1
                             ((mquotient)
                              ((%sin) ((mtimes) ((mplus) m n) x))
                              ((mtimes) 2 ((mplus) m n)))))))))
       ((and (eq (car b) '%cos) (eq (car d) '%cos))
        ;; We have a*cos(m*x)*cos(n*x).
        ;; The integral is: a*(sin((m-n)*x)/(2*(m-n))+sin((m+n)*x)/(2*(m+n))
        (return (subliss y
                         '((mtimes) a
                           ((mplus)
                            ((mquotient)
                             ((%sin) ((mtimes) ((mplus) m ((mtimes) -1 n)) x))
                             ((mtimes) 2
                              ((mplus) m ((mtimes) -1 n))))
                            ((mquotient)
                             ((%sin) ((mtimes) ((mplus) m n) x))
                             ((mtimes) 2 ((mplus) m n))))))))
       ((or (and (eq (car b) '%cos)
		 ;; The following (destructively!) swaps the values of
		 ;; m and n if first trig term is sin.  I (rtoy) don't
		 ;; understand why this is needed.  The formula
		 ;; doesn't depend on that.
                 (setq w (cdras 'm y ))
                 (rplacd (assoc 'm y) (cdras 'n y))
                 (rplacd (assoc 'n y) w))
            t)
        ;; We have a*cos(n*x)*sin(m*x).
        ;; The integral is: -a*(cos((m-n)*x)/(2*(m-n))+cos((m+n)*x)/(2*(m+n))
        (return (subliss y
                         '((mtimes) -1 a
                           ((mplus)
                            ((mquotient)
                             ((%cos) ((mtimes) ((mplus) m ((mtimes) -1 n)) x))
                             ((mtimes) 2 ((mplus) m ((mtimes) -1 n))))
                            ((mquotient)
                             ((%cos) ((mtimes) ((mplus) m n) x))
                             ((mtimes) 2 ((mplus) m n)))))))))
  b  ;; At this point we have trig functions with different arguments,
     ;; but not a product of sin and cos.
     (cond ((not (setq y (prog2 
                           (setq *trigarg* var)
                           (m2 exp
                               '((mtimes)
                                 ((coefftt) (a freevar))
                                 (((b trig1))
                                  ((mtimes) 
                                   (x varp)
                                   ((coefftt) (n integerp2))))
                                 ((coefftt) (c supertrig)))))))
            (return nil)))
     ;; We have a product of trig functions: trig1(n*x)*trig2(y).
     ;; trig1 is sin or cos, where n is a numerical integer. trig2 is not a sin
     ;; or cos. The cos or sin function is expanded.
     (return
       (integrator
         ($expand
           (list '(mtimes)
                 (cdras 'a y)                             ; constant factor
                 (cdras 'c y)                             ; trig functions
                 (cond ((eq (car (cdras 'b y)) '%cos)     ; expand cos(n*x)
                        (maxima-substitute var
                                           'x
                                           (supercosnx (cdras 'n y))))
                       (t                                 ; expand sin(x*x)
                        (maxima-substitute var
                                           'x
                                           (supersinx (cdras 'n y)))))))
         var))
  a  ;; A product of trig functions and all trig functions have the same
     ;; argument *trigarg*. Maxima substitutes *trigarg* with the variable var
     ;; of integration and calls trigint to integrate the new problem.
     (setq w (subst2s exp *trigarg*))
     (setq b (cdras 'b (m2-b*x+a *trigarg*)))
     (setq a (substint *trigarg* var (trigint (div* w b) var)))
     (return (if (isinop a '%integrate)
                 (list '(%integrate) exp var)
                 a))))

(defun trig2 (x)
  (member (car x) '(%sin %cos %tan %cot %sec %csc) :test #'eq))

(defun supersinx (n)
  (let ((i (if (< n 0) -1 1)))
    ($expand (list '(mtimes) i (sinnx (timesk i n))))))

(defun supercosnx (n)
  ($expand (cosnx (timesk (if (< n 0) -1 1) n))))

(defun sinnx (n)
  (if (equal n 1)
      '((%sin) x)
      (list '(mplus)
	    (list '(mtimes) '((%sin) x) (cosnx (1- n)))
	    (list '(mtimes) '((%cos) x) (sinnx (1- n))))))

(defun cosnx (n)
  (if (equal n 1)
      '((%cos) x)
      (list '(mplus)
	    (list '(mtimes) '((%cos) x) (cosnx (1- n)))
	    (list '(mtimes) -1 '((%sin) x) (sinnx (1- n))))))

(defun poseven (x)
  (and (even x) (> x -1)))

(defun trigfree (x)
  (if (atom x)
      (not (member x '(sin* cos* sec* tan*) :test #'eq))
      (and (trigfree (car x)) (trigfree (cdr x)))))

(defun rat1 (exp)
  (prog (*b1* *notsame*)
     (declare (special *yy* *b1* *notsame*))
     (when (and (numberp exp) (zerop exp))
       (return nil))
     (setq *b1* (subst *b* 'b '((mexpt) b (n even))))
     (return (prog2
		 (setq *yy* (rats exp))
		 (cond ((not *notsame*) *yy*))))))

(defun rats (exp)
  (prog (y)
     (declare (special *notsame* *b1*))
     (return
       (cond ((eq exp *a*) 'x)
	     ((atom exp)
	      (cond ((member exp '(sin* cos* sec* tan*) :test #'eq)
		     (setq *notsame* t))
		    (t exp)))
	     ((setq y (m2 exp *b1*))
	      (f3 y))
	     (t (cons (car exp) (mapcar #'(lambda (g) (rats g)) (cdr exp))))))))

(defun f3 (y)
  (maxima-substitute *c*
		     'c
		     (maxima-substitute (quotient (cdr (assoc 'n y :test #'eq)) 2)
					'n
					'((mexpt)
					  ((mplus)
					   1
					   ((mtimes)
					    c
					    ((mexpt) x 2)))
					  n))))

(defun odd1 (n)
  (declare (special *yz*))
  (cond ((not (numberp n)) nil)
	((not (equal (rem n 2) 0))
	 (setq *yz*
	       (maxima-substitute *c*
				  'c
				  (list '(mexpt)
					'((mplus) 1 ((mtimes) c ((mexpt) x 2)))
					(quotient (1- n) 2)))))
	(t nil)))

(defun subvar (x)
  (maxima-substitute var 'x x))

(defun subvardlg (x)
  (mapcar #'(lambda (m)
	      (cons (maxima-substitute var 'x (car m)) (cdr m)))
	  x))

;; This appears to be the implementation of Method 6, pp.82 in Moses' thesis.

(defun trigint (exp var)
  (prog (y repl y1 y2 *yy* z m n *c* *yz* *a* *b* )
     (declare (special *yy* *yz*))
     ;; Transform trig(x) into trig* (for simplicity?)  Convert cot to
     ;; tan and csc to sin.
     (setq y2
	   (subliss (subvardlg '((((%sin) x) . sin*)
				 (((%cos) x) . cos*)
				 (((%tan) x) . tan*)
				 (((%cot) x) . ((mexpt) tan* -1))
				 (((%sec) x) . sec*)
				 (((%csc) x) . ((mexpt) sin* -1))))
		    exp))
     
     (when *debug-integrate*
       (format t "~& in TRIGINT:~%")
       (format t "~&   : y2 = ~A~%" y2))
     
     ;; Now transform tan to sin/cos and sec to 1/cos.
     (setq y1 (setq y (subliss '((tan* . ((mtimes) sin*
                                          ((mexpt) cos* -1)))
                                 (sec* . ((mexpt) cos* -1)))
                               y2)))
     
     (when *debug-integrate* (format t "~&   : y  = ~A~%" y))
     
     (when (null (setq z
                       (m2 y
                           '((mtimes)
                             ((coefftt) (b trigfree))
                             ((mexpt) sin* (m poseven))
                             ((mexpt) cos* (n poseven))))))
       ;; Go if y is not of the form sin^m*cos^n for positive even m and n.
       (go l1))
     
     ;; Case III:
     ;; Handle the case of sin^m*cos^n, m, n both non-negative and even.
     
     (setq m (cdras 'm z))
     (setq n (cdras 'n z))
     (setq *a* (integerp2 (* 0.5 (if (< m n) 1 -1) (+ n (* -1 m)))))
     (setq z (cons (cons 'a *a*) z))
     (setq z (cons (cons 'x var) z))
     
     (when *debug-integrate*
       (format t "~& CASE III:~%")
       (format t "~&   : m, n = ~A ~A~%" m n)
       (format t "~&   : a    = ~A~%" *a*)
       (format t "~&   : z    = ~A~%" z))
     
     ;; integrate(sin(y)^m*cos(y)^n,y) is transformed to the following form:
     ;;
     ;; m < n:  integrate((sin(2*y)/2)^n*(1/2+1/2*cos(2*y)^((n-m)/2),y)
     ;; m >= n: integrate((sin(2*y)/2)^n*(1/2-1/2*cos(2*y)^((m-n)/2),y)
     (return
       (mul (cdras 'b z)
            (div 1 2)
            (substint 
              (mul 2 var)
              var
              (integrator 
                (cond ((< m n)
                       (subliss z
                                '((mtimes)
                                  ((mexpt)
                                   ((mtimes) ((rat simp) 1 2) ((%sin) x))
                                   m)
                                  ((mexpt)
                                   ((mplus)
                                    ((rat simp) 1 2)
                                    ((mtimes)
                                     ((rat simp) 1 2) ((%cos) x))) a))))
                      (t
                       (subliss z
                                '((mtimes)
                                  ((mexpt)
                                   ((mtimes) ((rat simp) 1 2) ((%sin) x))
                                   n)
                                  ((mexpt)
                                   ((mplus)
                                    ((rat simp) 1 2)
                                    ((mtimes)
                                     ((rat simp) -1 2) 
                                     ((%cos) x))) a)))))
                var))))
  l1 
     ;; Case IV:
     ;; I think this is case IV, working on the expression in terms of
     ;; sin and cos.
     ;;
     ;; Elem(x) means constants, x, trig functions of x, log and
     ;; inverse trig functions of x, and which are closed under
     ;; addition, multiplication, exponentiation, and substitution.
     ;;
     ;; Elem(f(x)) is the same as Elem(x), but f(x) replaces x in the
     ;; definition.
     
     (when *debug-integrate* (format t "~& Case IV:~%"))
     
     (setq *c* -1)
     (setq *a* 'sin*)
     (setq *b* 'cos*)
     (when (and (m2 y '((coeffpt) (c rat1) ((mexpt) cos* (n odd1))))
                (setq repl (list '(%sin) var)))
       ;; The case cos^(2*n+1)*Elem(cos^2,sin).  Use the substitution z = sin.
       (go getout))
     (setq *a* *b*)
     (setq *b* 'sin*)
     (when (and (m2 y '((coeffpt) (c rat1) ((mexpt) sin* (n odd1))))
                (setq repl (list '(%cos) var)))
       ;; The case sin^(2*n+1)*Elem(sin^2,cos).  Use the substitution z = cos.
       (go get3))
     
     ;; Case V:
     ;; Transform sin and cos to tan and sec to see if the integral is
     ;; of the form Elem(tan, sec^2).  If so, use the substitution z = tan.
     
     (when *debug-integrate* (format t "~& Case V:~%"))
     
     (setq y (subliss '((sin* (mtimes) tan* ((mexpt) sec* -1))
                        (cos* (mexpt) sec* -1))
                      y2))
     (setq *c* 1)
     (setq *a* 'tan*)
     (setq *b* 'sec*)
     (when (and (rat1 y) (setq repl (list '(%tan) var)))
       (go get1))
     (setq *a* *b*)
     (setq *b* 'tan*)
     (when (and (m2 y '((coeffpt) (c rat1) ((mexpt) tan* (n odd1))))
           (setq repl (list '(%sec) var)))
       (go getout))
     (when (not (alike1 (setq repl ($expand exp)) exp))
       (return (integrator repl var)))
     (setq y (subliss '((sin* (mtimes) 2 x
                              ((mexpt) ((mplus) 1 ((mexpt) x 2)) -1))
                        (cos* (mtimes)
                              ((mplus) 1 ((mtimes) -1 ((mexpt) x 2)))
                              ((mexpt) ((mplus) 1 ((mexpt) x 2)) -1)))
                      y1))
     (setq y (list '(mtimes) 
                   y 
                   '((mtimes) 2 ((mexpt) ((mplus) 1 ((mexpt) x 2)) -1))))
     (setq repl (subvar '((mquotient) ((%sin) x) ((mplus) 1 ((%cos) x)))))
     (go get2)
  get3
     (setq y (list '(mtimes) -1 *yy* *yz*))
     (go get2)
  get1
     (setq y (list '(mtimes) '((mexpt) ((mplus) 1 ((mexpt) x 2)) -1) *yy*))
     (go get2)
  getout
     (setq y (list '(mtimes) *yy* *yz*))
  get2
     (when *debug-integrate*
       (format t "~& Call the INTEGRATOR with:~%")
       (format t "~&   : y    = ~A~%" y)
       (format t "~&   : repl = ~A~%" repl))
     ;; See Bug 2880797.  We want atan(tan(x)) to simplify to x, so
     ;; set $triginverses to '$all.
     (return
       ;; Do not integrate for the global variable VAR, but substitute it.
       ;; This way possible assumptions on VAR are no longer present. The
       ;; algorithm of DEFINT depends on this behavior. See Bug 3085498.
       (let (($triginverses '$all) (newvar (gensym)))
         (substint repl
                   newvar
                   (integrator (maxima-substitute newvar 'x y) newvar))))))

(defmvar $integration_constant_counter 0)
(defmvar $integration_constant '$%c)

;; This is the top level of the integrator
(defmfun sinint (exp var)
  ;; *integrator-level* is a recursion counter for INTEGRATOR.  See
  ;; INTEGRATOR for more details.  Initialize it here.
  (let ((*integrator-level* 0))
    (declare (special *integrator-level*))
    (cond ((mnump var) (merror (intl:gettext "integrate: variable must not be a number; found: ~:M") var))
	  (($ratp var) (sinint exp (ratdisrep var)))
	  (($ratp exp) (sinint (ratdisrep exp) var))
	  ((mxorlistp exp)    ;; if exp is an mlist or matrix
	   (cons (car exp)
		 (mapcar #'(lambda (y) (sinint y var)) (cdr exp))))
	  ;; if exp is an equality, integrate both sides
	  ;; and add an integration constant
	  ((mequalp exp)
	   (list (car exp) (sinint (cadr exp) var)
		 (add (sinint (caddr exp) var)
	      ($concat $integration_constant (incf $integration_constant_counter)))))
	  ((and (atom var)
		(isinop exp var))
	   (list '(%integrate) exp var))
	  ((let ((ans (simplify
		       (let ($opsubst varlist genvar stack)
			 (integrator exp var)))))
	     (if (sum-of-intsp ans)
		 (list '(%integrate) exp var)
		 ans))))))

(defun sum-of-intsp (ans)
  (cond ((atom ans) (not (eq ans var)))
	((mplusp ans) (every #'sum-of-intsp (cdr ans)))
	((eq (caar ans) '%integrate) t)
	((mtimesp ans)
	 (do ((facs (cdr ans) (cdr facs))
	      (ints))
	     ((null facs)
	      (< (length ints) 2))
	   (unless (freeof var (car facs))
	     (if (sum-of-intsp (car facs))
		 (push (car facs) ints)
		 (return nil)))))
	((freeof var ans) t)
	(t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stage I
;;; Implementation of Method 2: Integrate a summation

(defun intsum (form var)
  (prog (exp idx ll ul pair val)
     (setq exp (cadr form)
	   idx (caddr form)
	   ll (cadddr form)
	   ul (car (cddddr form)))
     (if (or (not (atom var))
	     (not (free idx var))
	     (not (free ll var))
	     (not (free ul var)))
	 (return (list '(%integrate) form var)))
     (setq pair (partition exp var 1))
     (when (and (mexptp (cdr pair))
		(eq (caddr pair) var))
       (setq val (maxima-substitute ll idx (cadddr pair)))
       (cond ((equal val -1)
	      (return (add (integrator (maxima-substitute ll idx exp) var)
			    (intsum1 exp idx (add 1 ll) ul var))))
	     ((mlsp val -1)
	      (return (list '(%integrate) form var)))))
     (return (intsum1 exp idx ll ul var))))

(defun intsum1 (exp idx ll ul var)
  (assume (list '(mgeqp) idx ll))
  (if (not (eq ul '$inf))
      (assume (list '(mgeqp) ul idx)))
  (simplifya (list '(%sum) (integrator exp var) idx ll ul) t))

(defun finds (x)
  (if (atom x)
      (member x '(%log %integrate %atan) :test #'eq)
      (or (finds (car x)) (finds (cdr x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stage II
;;; Implementation of Method 9:
;;; Rational function times a log or arctric function

;;; ratlog is called for an expression containing a log or arctrig function
;;; The integrand is like log(x)*f'(x). To obtain the result the technique of
;;; partial integration is applied: log(x)*f(x)-integrate(1/x*f(x),x)

(defun ratlog (exp var form)
  (prog (b c d y z w)
     (setq y form)
     (setq b (cdr (assoc 'b y :test #'eq)))
     (setq c (cdr (assoc 'c y :test #'eq)))
     (setq y (integrator c var))
     (when (finds y) (return nil))
     (setq d (sdiff (cdr (assoc 'a form :test #'eq)) var))
     
     (setq z (integrator (mul2* y d) var))
     (setq d (cdr (assoc 'a form :test #'eq)))
     (return (simplify (list '(mplus)
			     (list '(mtimes) y d)
			     (list '(mtimes) -1 z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; partial-integration is an extension of the algorithm of ratlog to support
;;; the technique of partial integration for more cases. The integrand
;;; is like g(x)*f'(x) and the result is g(x)*f(x)-integrate(g'(x)*f(x),x).
;;;
;;; Adding integrals properties for elementary functions led to infinite recursion 
;;; with integrate(z*expintegral_shi(z),z). This was resolved by limiting the 
;;; recursion depth. *integrator-level* needs to be at least 3 to solve 
;;;  o  integrate(expintegral_ei(1/sqrt(x)),x)
;;;  o  integrate(sqrt(z)*expintegral_li(z),z)
;;; while a value of 4 causes testsuite regressions with 
;;;  o  integrate(z*expintegral_shi(z),z)
(defun partial-integration (form var)
  (declare (special *integrator-level*))
  (let ((g  (cdr (assoc 'a form)))   ; part g(x)
	(df (cdr (assoc 'c form)))   ; part f'(x)
	(f  nil))
    (setq f (integrator df var))     ; integrate f'(x) wrt var
    (cond
      ((or (isinop f '%integrate)    ; no result or
	   (isinop f (caar g))       ; g in result
	   (> *integrator-level* 3))
       nil)                          ; we return nil
      (t
       ;; Build the result: g(x)*f(x)-integrate(g'(x)*f(x))
       (add (mul f g)
	    (mul -1 (integrator (mul f (sdiff g var)) var)))))))

;; returns t if argument of every trig operation in y matches arg
(defun every-trigarg-alike (y arg)
  (cond ((atom y) t)
	((optrig (caar y)) (alike1 arg (cadr y)))
	(t (every (lambda (exp)
		    (every-trigarg-alike exp arg))
		  (cdr y)))))

;; return argument of first trig operation encountered in y
(defun find-first-trigarg (y)
  (cond ((atom y) nil)
	((optrig (caar y)) (cadr y))
	(t (some (lambda (exp)
		   (find-first-trigarg exp))
		 (cdr y)))))

(defun matchsum (alist blist)
  (prog (r s *c* *d*)
     (setq s (m2 (car alist)
		 '((mtimes)
		   ((coefftt) (a freevar))
		   ((coefftt) (c true)))))
     (setq *c* (cdr (assoc 'c s :test #'eq)))
     (cond ((not (setq r
		       (m2 (cons '(mplus) blist)
			   (list '(mplus)
				 (cons '(mtimes)
				       (cons '((coefftt) (b free1))
					     (cond ((mtimesp *c*)
						    (cdr *c*))
						   (t (list *c*)))))
				 '(d true)))))
	    (return nil)))
     (setq *d* (simplify (list '(mtimes)
			     (subliss s 'a)
			     (list '(mexpt)
				   (subliss r 'b)
				   -1))))
     (cond ((m2 (cons '(mplus) alist)
		(timesloop *d* blist))
	    (return *d*))
	   (t (return nil)))))

(defun timesloop (a b)
  (cons '(mplus) (mapcar #'(lambda (c) (mul2* a c)) b)))

(defun expands (aa b)
  (addn (mapcar #'(lambda (c) (timesloop c aa)) b) nil))

(defun powerlist (exp var)
  (prog (y *c* *d* powerlist *b*)
     (setq y (m2 exp
		 '((mtimes)
		   ((mexpt) (var varp) (c integerp2))
		   ((coefftt) (a freevar))
		   ((coefftt) (b true)))))
     (setq *b* (cdr (assoc 'b y :test #'eq)))
     (setq *c* (cdr (assoc 'c y :test #'eq)))
     (unless  (rat10 *b*) (return nil))
     (setq *d* (apply #'gcd (cons (1+ *c*) powerlist)))
     (when (or (eql 1 *d*) (zerop *d*)) (return nil))
     (return
       (substint
	(list '(mexpt) var *d*)
	var
	(integrate5 (simplify (list '(mtimes)
				    (power* *d* -1)
				    (cdr (assoc 'a y :test #'eq))
				    (list '(mexpt) var (1- (quotient (1+ *c*) *d*)))
				    (subst10 *b*)))
		    var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stage I
;;; Implementation of Method 3: Derivative-divides algorithm

;; This is the derivative-divides algorithm of Moses.
;;
;;                /
;;                [
;; Look for form  I  c * op(u(x)) * u'(x) dx
;;                ]
;;                /
;;
;;  where:  c     is a constant
;;          u(x)  is an elementary expression in x
;;          u'(x) is its derivative
;;          op    is an elementary operator:
;;                - the indentity, or
;;                - any function that can be integrated by INTEGRALLOOKUPS
;;
;; The method of solution, once the problem has been determined to
;; posses the form above, is to look up OP in a table and substitute
;; u(x) for each occurrence of x in the expression given in the table.
;; In other words, the method performs an implicit substitution y = u(x),
;; and obtains the integral of op(y)dy by a table look up.
;;
(defun diffdiv (exp var)
  (prog (y *a* x v *d* z w r)
     (cond ((and (mexptp exp)
		 (mplusp (cadr exp))
		 (integerp (caddr exp))
		 (< (caddr exp) 6)
		 (> (caddr exp) 0))
	    (return (integrator (expandexpt (cadr exp) (caddr exp)) var))))

     ;; If not a product, transform to a product with one term
     (setq exp (cond ((mtimesp exp) exp) (t (list '(mtimes) exp))))

     ;; Loop over the terms in exp
     (setq z (cdr exp))
     a    (setq y (car z))

     ;; This m2 pattern matches const*(exp/y)
     (setq r (list '(mplus)
		   (cons '(coeffpt)
			 (cons '(c free1)
			       (remove y (cdr exp) :count 1)))))
     (cond
      ;; Case u(var) is the identity function. y is a term in exp.
      ;; Match if diff(y,var) == c*(exp/y).
      ;; This even works when y is a function with multiple args.
       ((setq w (m2 (sdiff y var) r))
	(return (muln (list y y (power* (mul2* 2 (cdr (assoc 'c w :test #'eq))) -1)) nil))))

     ;; w is the arg in y.
     (let ((arg-freevar))
       (setq w
	 (cond
	  ((or (atom y) (member (caar y) '(mplus mtimes) :test #'eq)) y)
	  ;; Take the argument of a function with one value.
	  ((= (length (cdr y)) 1) (cadr y))
	  ;; A function has multiple args, and exactly one arg depends on var
	  ((= (count-if #'null (setq arg-freevar (mapcar #'freevar (cdr y)))) 1)
	   (do ((args (cdr y) (cdr args))
		(argf arg-freevar (cdr argf)))
	       ((if (not (car argf)) (return (car args))))))
	  (t 0))))

     (cond
       ((setq w (cond ((and (setq x (sdiff w var))
			    (mplusp x)
			    (setq *d* (remove y (cdr exp) :count 1))
			    (setq v (car *d*))
			    (mplusp v)
			    (not (cdr *d*)))
		       (cond ((setq *d* (matchsum (cdr x) (cdr v)))
			      (list (cons 'c *d*)))
			     (t nil)))
		      (t (m2 x r))))
	(return (cond ((null (setq x (integrallookups y))) nil)
		      ((eq w t) x)
		      (t (mul2* x (power* (cdr (assoc 'c w :test #'eq)) -1)))))))
     (setq z (cdr z))
     (when (null z) (return nil))
     (go a)))

(defun subliss (alist expr)
  "Alist is an alist consisting of a variable (symbol) and its value.  expr is
  an expression.  For each entry in alist, substitute the corresponding
  value into expr."
  (let ((x expr))
    (dolist (a alist x)
      (setq x (maxima-substitute (cdr a) (car a) x)))))

(defun substint (x y expres)
  (if (and (not (atom expres)) (eq (caar expres) '%integrate))
      (list (car expres) exp var)
      (substint1 (maxima-substitute x y expres))))

(defun substint1 (exp)
  (cond ((atom exp) exp)
	((and (eq (caar exp) '%integrate)
	      (null (cdddr exp))
	      (not (symbolp (caddr exp)))
	      (not (free (caddr exp) var)))
	 (simplify (list '(%integrate)
			 (mul2 (cadr exp) (sdiff (caddr exp) var))
			 var)))
	(t (recur-apply #'substint1 exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;:; Extension of the integrator for more integrals with power functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Recognize (a^(c*(z^r)^p+d)^v

(defun m2-exp-type-1a (expr)
  (m2 expr
      '((mexpt)
        ((mexpt)
         (a freevar0)
         ((mplus)
          ;; The order of the pattern is critical. If we change it,
          ;; we do not get the expected match.
          ((coeffpp) (d freevar))
          ((coefft) (c freevar0)
           ((mexpt)
            ((mexpt) (z varp) (r freevar0))
            (p freevar)))))
        (v freevar))))

;;; Recognize z^v*a^(b*z^r+d)

(defun m2-exp-type-2 (expr)
  (m2 expr
      '((mtimes)
        ((mexpt) (z varp) (v freevar0))
        ((mexpt)
         (a freevar0)
         ((mplus)
          ((coeffpp) (d freevar))
          ((coefft) (b freevar0) ((mexpt) (z varp) (r freevar0))))))))

;;; Recognize z^v*%e^(a*z^r+b)^u

(defun m2-exp-type-2-1 (expr)
  (m2 expr
      '((mtimes)
        ((mexpt) (z varp) (v freevar0))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (b freevar))
           ((coefft) (a freevar0) ((mexpt) (z varp) (r freevar0)))))
         (u freevar)))))

;;; Recognize (a*z+b)^p*%e^(c*z+d)

(defun m2-exp-type-3 (expr)
  (m2 expr
    '((mtimes)
	((mexpt)
	   ((mplus)
	      ((coefft) (a freevar0) (z varp))
	      ((coeffpp) (b freevar)))
	   (p freevar0))
      ((mexpt)
	 $%e
	 ((mplus)
	    ((coefft) (c freevar0) (z varp))
	    ((coeffpp) (d freevar)))))))

;;; Recognize d^(a*z^2+b/z^2+c)

(defun m2-exp-type-4 (expr)
  (m2 expr
    '((mexpt)
	(d freevar0)
	((mplus)
	   ((coefft) (a freevar0) ((mexpt) (z varp) 2))
	   ((coefft) (b freevar0) ((mexpt) (z varp) -2))
	   ((coeffpp) (c freevar))))))

;;; Recognize z^(2*n)*d^(a*z^2+b/z^2+c)

(defun m2-exp-type-4-1 (expr)
  (m2 expr
    '((mtimes)
	((mexpt) (z varp) (n freevar0))
	((mexpt)
	   (d freevar0)
	   ((mplus)
	      ((coefft)  (a freevar0) ((mexpt) (z varp) 2))
	      ((coefft)  (b freevar0) ((mexpt) (z varp) -2))
	      ((coeffpp) (c freevar)))))))

;;; Recognize z^n*d^(a*z^2+b*z+c)

(defun m2-exp-type-5 (expr)
  (m2 expr
      '((mtimes)
        ((mexpt) (z varp) (n freevar))
        ((mexpt)
         (d freevar0)
         ((mplus)
          ((coeffpt) (a freevar) ((mexpt) (z varp) 2))
          ((coeffpt) (b freevar) (z varp))
          ((coeffpp) (c freevar)))))))

;;; Recognize z^n*(%e^(a*z^2+b*z+c))^u

(defun m2-exp-type-5-1 (expr)
  (m2 expr
      '((mtimes)
        ((mexpt) (z varp) (n freevar0))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (c freevar))
           ((coefft) (a freevar0) ((mexpt) (z varp) 2))
           ((coefft) (b freevar0) (z varp))))
         (u freevar)))))

;;; Recognize z^n*d^(a*sqrt(z)+b*z+c)

(defun m2-exp-type-6 (expr)
  (m2 expr
    '((mtimes)
	((mexpt) (z varp) (n freevar0))
	((mexpt)
	   (d freevar0)
	   ((mplus)
	      ((coefft) (a freevar0) ((mexpt) (z varp) ((rat) 1 2)))
	      ((coefft) (b freevar0) (z varp))
	      ((coeffpp) (c freevar)))))))

;;; Recognize z^n*(%e^(a*sqrt(z)+b*z+c))^u

(defun m2-exp-type-6-1 (expr)
  (m2 expr
      '((mtimes)
        ((mexpt) (z varp) (n freevar0))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (c freevar))
           ((coefft) (a freevar0) ((mexpt) (z varp) ((rat) 1 2)))
           ((coefft) (b freevar0) (z varp))))
         (u freevar)))))

;;; Recognize z^n*a^(b*z^r+e)*h^(c*z^r+g)

(defun m2-exp-type-7 (expr)
  (m2 expr
    '((mtimes)
	((mexpt) (z varp) (n freevar))
	((mexpt)
	   (a freevar0)
	   ((mplus)
	      ((coefft)
		 (b freevar0)
		 ((mexpt) (z varp) (r freevar0)))
	      ((coeffpp) (e freevar))))
	((mexpt)
	   (h freevar0)
	   ((mplus)
	      ((coefft)
		 (c freevar0)
		 ((mexpt) (z varp) (r1 freevar0)))
	      ((coeffpp) (g freevar)))))))

;;; Recognize z^v*(%e^(b*z^r+e))^q*(%e^(c*z^r+g))^u

(defun m2-exp-type-7-1 (expr)
  (m2 expr
      '((mtimes)
        ((mexpt) (z varp) (v freevar))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (e freevar))
           ((coefft) (b freevar0) ((mexpt) (z varp) (r freevar0)))))
         (q freevar))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (g freevar))
           ((coefft) (c freevar0) ((mexpt) (z varp) (r1 freevar0)))))
         (u freevar)))))

;;; Recognize a^(b*sqrt(z)+d*z+e)*h^(c*sqrt(z)+f*z+g)

(defun m2-exp-type-8 (expr)
  (m2 expr
    '((mtimes)
	((mexpt)
	   (a freevar0)
	   ((mplus)
	      ((coeffpt) (b freevar) ((mexpt) (z varp) ((rat) 1 2)))
	      ((coeffpt) (d freevar) (z varp))
	      ((coeffpp) (e freevar))))
	((mexpt)
	   (h freevar0)
	   ((mplus)
	      ((coeffpt) (c freevar) ((mexpt) (z varp) ((rat) 1 2)))
	      ((coeffpt) (f freevar) (z varp))
	      ((coeffpp) (g freevar)))))))

;;; Recognize (%e^(b*sqrt(z)+d*z+e))^u*(%e^(c*sqrt(z)+f*z+g))^v

(defun m2-exp-type-8-1 (expr)
  (m2 expr
      '((mtimes)
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (e freevar))
           ((coeffpt) (b freevar) ((mexpt) (z varp) ((rat) 1 2)))
           ((coeffpt) (d freevar) (z varp))))
         (u freevar))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (g freevar))
           ((coeffpt) (c freevar) ((mexpt) (z varp) ((rat) 1 2)))
           ((coeffpt) (f freevar) (z varp))))
         (v freevar)))))

;;; Recognize (%e^(b*z^r+e))^u*(%e^(c*z^r+g))^v

(defun m2-exp-type-8-2 (expr)
  (m2 expr
      '((mtimes)
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (e freevar))
           ((coefft) (b freevar) ((mexpt) (z varp) (r freevar0)))))
         (u freevar))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (g freevar))
           ((coefft) (c freevar) ((mexpt) (z varp) (r1 freevar0)))))
         (v freevar)))))

;;; Recognize z^n*a^(b*z^2+d*z+e)*h^(c*z^2+f*z+g)

(defun m2-exp-type-9 (expr)
  (m2 expr
    '((mtimes)
      ((mexpt) (z varp) (n freevar))
      ((mexpt)
	 (a freevar0)
	 ((mplus)
	    ((coeffpt)  (b freevar) ((mexpt) (z varp) 2))
	    ((coeffpt)  (d freevar) (z varp))
	    ((coeffpp) (e freevar))))
      ((mexpt)
	 (h freevar0)
	 ((mplus)
	    ((coeffpt)  (c freevar) ((mexpt) (z varp) 2))
	    ((coeffpt)  (f freevar) (z varp))
	    ((coeffpp) (g freevar)))))))

;;; Recognize z^n*(%e^(b*z^2+d*z+e))^q*(%e^(c*z^2+f*z+g))^u

(defun m2-exp-type-9-1 (expr)
  (m2 expr
      '((mtimes)
        ((mexpt) (z varp) (n freevar))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (e freevar))
           ((coeffpt) (b freevar) ((mexpt) (z varp) 2))
           ((coeffpt) (d freevar) (z varp))))
         (q freevar))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (g freevar))
           ((coeffpt) (c freevar) ((mexpt) (z varp) 2))
           ((coeffpt) (f freevar) (z varp))))
         (u freevar)))))

;;; Recognize z^n*a^(b*sqrt(z)+d*z+e)*h^(c*sqrt(z+)f*z+g)

(defun m2-exp-type-10 (expr)
  (m2 expr
    '((mtimes)
	((mexpt) (z varp) (n freevar))
	((mexpt)
	   (a freevar0)
	   ((mplus)
	      ((coeffpt)  (b freevar) ((mexpt) (z varp) ((rat) 1 2)))
	      ((coeffpt)  (d freevar) (z varp))
	      ((coeffpp) (e freevar))))
	((mexpt)
	   (h freevar0)
	   ((mplus)
	      ((coeffpt)  (c freevar) ((mexpt) (z varp) ((rat) 1 2)))
	      ((coeffpt)  (f freevar) (z varp))
	      ((coeffpp) (g freevar)))))))

;;; Recognize z^n*(%e^(b*sqrt(z)+d*z+e))^q*(%e^(c*sqrt(z)+f*z+g))^u

(defun m2-exp-type-10-1 (expr)
  (m2 expr
      '((mtimes)
        ((mexpt) (z varp) (n freevar))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (e freevar))
           ((coeffpt) (b freevar) ((mexpt) (z varp) ((rat) 1 2)))
           ((coeffpt) (d freevar) (z varp))))
         (q freevar))
        ((mexpt)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpp) (g freevar))
           ((coeffpt) (c freevar) ((mexpt) (z varp) ((rat) 1 2)))
           ((coeffpt) (f freevar) (z varp))))
         (u freevar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun integrate-exp-special (expr var &aux w const)

  ;; First factor the expression.
  (setq expr ($factor expr))
  
  ;; Remove constant factors.
  (setq w (partition expr var 1))
  (setq const (car w))
  (setq expr (cdr w))
  
  (cond
    ((setq w (m2-exp-type-1a (facsum-exponent expr)))
     (let ((a (cdras 'a w))
           (c (cdras 'c w))
           (d (cdras 'd w))
           (r (cdras 'r w))
           (p (cdras 'p w))
           (v (cdras 'v w)))
       
       (when *debug-integrate*
         (format t "~&Type 1a: (a^(c*(z^r)^p+d)^v : w = ~A~%" w))
       
       (mul -1
            const
            (inv (mul p r (power a (mul c v (power (power var r) p)))))
            var
            (power (power a (add d (mul c (power (power var r) p)))) v)
            (take '(%gamma_incomplete)
                  (inv (mul p r))
                  (mul -1 c v (power (power var r) p) (take '(%log) a)))
            (power (mul -1 c v (power (power var r) p) (take '(%log) a))
                   (div -1 (mul p r))))))
    
    ((setq w (m2-exp-type-2 (facsum-exponent expr)))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (d (cdras 'd w))
	   (v (cdras 'v w))
	   (r (cdras 'r w)))

       (when *debug-integrate*
	 (format t "~&Type 2: z^v*a^(b*z^r+d) : w = ~A~%" w))

       (mul
         const
	 (div -1 r)
	 (power a d)
	 (power var (add v 1))
	 ($gamma_incomplete
	   (div (add v 1) r)
	   (mul -1 b (power var r) ($log a)))
	 (power
	   (mul -1 b (power var r) ($log a))
	   (mul -1 (div (add v 1) r))))))
    
    ((setq w (m2-exp-type-2-1 (facsum-exponent expr)))
     (let ((a (cdras 'a w))
           (b (cdras 'b w))
           (v (cdras 'v w))
           (r (cdras 'r w))
           (u (cdras 'u w)))
       
       (when *debug-integrate*
         (format t "~&Type 2-1: z^v*(%e^(a*z^r+b))^u : w = ~A~%" w))
       
       (mul const
            -1
            (inv r)
            (power '$%e (mul -1 a u (power var r)))
            (power (power '$%e (add (mul a (power var r)) b)) u)
            (power var (add v 1))
            (power (mul -1 a u (power var r)) (div (mul -1 (add v 1)) r))
            (take '(%gamma_incomplete)
                  (div (add v 1) r)
                  (mul -1 a u (power var r))))))
    
    ((setq w (m2-exp-type-3 (facsum-exponent expr)))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (d (cdras 'd w))
	   (p (cdras 'p w)))

       (when *debug-integrate*
	 (format t "~&Type 3: (a*z+b)^p*%e^(c*z+d) : w = ~A~%" w))

       (mul
         const
	 (div -1 a)
	 (power '$%e (sub d (div (mul b c) a)))
	 (power (add b (mul a var)) (add p 1))
	 ($expintegral_e (mul -1 p) (mul (div -1 a) c (add b (mul a var)))))))

    ((setq w (m2-exp-type-4 expr))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (d (cdras 'd w))
	   ($trigsign nil)) ; Do not simplify erfc(-x) !

       (when *debug-integrate*
	 (format t "~&Type 4: d^(a*z^2+b/z^2+c) : w = ~A~%" w))

       (mul
         const
	 (div 1 (mul 4 (power (mul -1 a ($log d)) (div 1 2))))
	 (mul
	   (power d c)
	   (power '$%pi (div 1 2))
	   (power '$%e
	     (mul -2
	       (power (mul -1 a ($log d)) (div 1 2))
	       (power (mul -1 b ($log d)) (div 1 2))))
	   (add
	     ($erfc
	       (add
		 (div (power (mul -1 b ($log d)) (div 1 2)) var)
		 (mul -1 var (power (mul -1 a ($log d)) (div 1 2)))))
	     (mul -1
	       (power '$%e
		 (mul 4
		   (power (mul -1 a ($log d)) (div 1 2))
		   (power (mul -1 b ($log d)) (div 1 2))))
	       ($erfc
		 (add
		   (mul var (power (mul -1 a ($log d)) (div 1 2)))
		   (div (power (mul -1 b ($log d)) (div 1 2)) var)))))))))

    ((and (setq w (m2-exp-type-4-1 expr))
	  ($evenp (cdras 'n w))   ; only for n an even integer
	  (symbolp (cdras 'a w))) ; a has to be a symbol
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (d (cdras 'd w))
	   (n (cdras 'n w))
	   ($trigsign nil)) ; Do not simplify erfc(-x) !

       (when *debug-integrate*
	 (format t "~&Type 4-1: z^(2*n)*d^(a*z^2+b/z^2+c) : w = ~A~%" w))

       (setq n (div n 2))

       (mul const
            (div 1 4)
	    (power d c)
	    (power '$%pi (div 1 2))
	    (simplify (list '(%derivative)
	     (div
	       (sub
		 (mul
		   (power ($log d) (mul -1 n))
		   (add
		     (mul
		       (power
			 '$%e
			 (mul -2
			   (power (mul -1 a ($log d)) (div 1 2))
			   (power (mul -1 b ($log d)) (div 1 2))))
		     ($erfc
		       (sub
			 (div
			   (power (mul -1 b ($log d)) (div 1 2))
			   var)
			 (mul var (power (mul -1 ($log d)) (div 1 2))))))))
		 (mul
		   (power
		     '$%e
		     (mul 2
		       (power (mul -1 a ($log d)) (div 1 2))
		       (power (mul -1 b ($log d)) (div 1 2))))
		   ($erfc
		     (add
		       (power (mul -1 a ($log d)) (div 1 2))
		       (div (power (mul -1 b ($log d)) (div 1 2)) var)))))
	       (power (mul -1 a ($log d)) (div 1 2)))
	     a n)))))

    ((and (setq w (m2-exp-type-5 (facsum-exponent expr)))
	  (maxima-integerp (cdras 'n w))
	  (eq ($sign (cdras 'n w)) '$pos))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (d (cdras 'd w))
	   (n (cdras 'n w)))

       (when *debug-integrate*
	 (format t "~&Type 5: z^n*d^(a*z^2+b*z+c) : w = ~A~%" w))

       (mul
         const
	 (div -1 (mul 2 (power (mul a ($log d)) (div 1 2))))
	 (mul
	   (power d (sub c (div (mul b b) (mul 4 a))))
	   (let ((index (gensumindex))
	         ($simpsum t))
	     (mfuncall '$sum
	       (mul
		 (power 2 (sub index n))
		 ($binomial n index)
		 ($gamma_incomplete
		   (div (add index 1) 2)
		   (mul
		     (div -1 (mul 4 a))
		     (power (add b (mul 2 a var)) 2)
		     ($log d)))
		 (power (mul a ($log d)) (mul -1 (add n (div 1 2))))
		 (power (mul -1 b ($log d)) (sub n index))
		 (power (mul (add b (mul 2 a var)) ($log d)) (add index 1))
		 (power
		   (mul (div -1 a) (power (add b (mul 2 a var)) 2) ($log d))
		   (mul (div -1 2) (add index 1))))
	       index 0 n))))))
    
    ((and (setq w (m2-exp-type-5-1 (facsum-exponent expr)))
          (maxima-integerp (cdras 'n w))
          (eq ($sign (cdras 'n w)) '$pos))
     (let ((a (cdras 'a w))
           (b (cdras 'b w))
           (c (cdras 'c w))
           (u (cdras 'u w))
           (n (cdras 'n w)))
       
       (when *debug-integrate*
         (format t "~&Type 5-1: z^n*(%e^(a*z^2+b*z+c))^u : w = ~A~%" w))
       
       (mul const
            (div -1 2)
            (power '$%e
                   (add (mul -1 (div (mul b b u) (mul 4 a)))
                        (mul -1 u (add (mul a var var) (mul b var)))))
            (power a (mul -1 (add n 1)))
            (power (power '$%e
                          (add (mul a var var) (mul b var) c))
                   u)
            (let ((index (gensumindex))
                  ($simpsum t))
              (dosum
                (mul (power 2 (sub index n))
                     (power (mul -1 b) (sub n index))
                     (power (add b (mul 2 a var)) (add index 1))
                     (power (div (mul -1 u (power (add b (mul 2 a var)) 2)) a)
                            (mul (div -1 2) (add index 1)))
                     (take '(%binomial) n index)
                     (take '(%gamma_incomplete)
                           (div (add index 1) 2)
                           (div (mul -1 u (power (add b (mul 2 a var)) 2))
                                (mul 4 a))))
                index 0 n t)))))
    
    ((and (setq w (m2-exp-type-6 (facsum-exponent expr)))
	  (maxima-integerp (cdras 'n w))
	  (eq ($sign (cdras 'n w)) '$pos))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (d (cdras 'd w))
	   (n (cdras 'n w)))

       (when *debug-integrate*
	 (format t "~&Type 6: z^n*d^(a*sqrt(z)+b*z+c) : w = ~A~%" w))

       (mul
         const
	 (power 2 (mul -1 (add n 1)))
	 (power d (sub c (div (mul a a) (mul 4 b))))
	 (power (mul b ($log d)) (mul -2 (add n 1)))
	 (let ((index1 (gensumindex))
	       (index2 (gensumindex))
	       ($simpsum t))
	   (mfuncall '$sum
	     (mfuncall '$sum
	       (mul
		 (power -1 (sub index1 index2))
		 (power 4 index1)
		 ($binomial index1 index2)
		 ($binomial n index1)
		 ($log d)
		 (power (mul a ($log d)) (sub (mul 2 n) (add index1 index2)))
		 (power
		   (mul (add a (mul 2 b (power var (div 1 2)))) ($log d))
		   (add index1 index2))
		 (power
		   (mul
		     (div -1 b)
		     (power (add a (mul 2 b (power var (div 1 2)))) 2)
		     ($log d))
		   (mul (div -1 2) (add index1 index2 1)))
		 (add
		   (mul 2 b
		     (power
		       (mul
			 (div -1 b)
			 (power (add a (mul 2 b (power var (div 1 2)))) 2)
			 ($log d))
		       (div 1 2))
		     ($gamma_incomplete
		       (div (add index1 index2 2) 2)
		       (mul
			 (div -1 (mul 4 b))
			 (power (add a (mul 2 b (power var (div 1 2)))) 2)
			 ($log d))))
		   (mul a
		     (add a (mul 2 b (power var (div 1 2))))
		     ($log d)
		     ($gamma_incomplete
		       (div (add index1 index2 1) 2)
		       (mul
			 (div -1 (mul 4 b))
			 (power (add a (mul 2 b (power var (div 1 2)))) 2)
			 ($log d))))))
	       index2 0 index1)
	     index1 0 n)))))
    
    ((and (setq w (m2-exp-type-6-1 (facsum-exponent expr)))
          (maxima-integerp (cdras 'n w))
          (eq ($sign (cdras 'n w)) '$pos))
     (let ((a (cdras 'a w))
           (b (cdras 'b w))
           (c (cdras 'c w))
           (u (cdras 'u w))
           (n (cdras 'n w)))
       
       (when *debug-integrate*
         (format t "~&Type 6-1: z^n*(%e^(a*sqrt(z)+b*z+c))^u : w = ~A~%" w))
       
       (mul const
            (power 2 (mul -1 (add (mul 2 n) 1)))
            (power '$%e
                   (add (div (mul -1 u a a) (mul 4 b))
                        (mul u (add (mul a (power var (div 1 2)))
                                    (mul b var)
                                    c))))
            (power b (mul -2 (add n 1)))
            (power (power '$%e
                          (add (mul a (power var (div 1 2)))
                               (mul b var)))
                   u)
            (let ((index1 (gensumindex))
                  (index2 (gensumindex))
                  ($simpsum t))
              (dosum
                (dosum
                  (mul (power -1 (sub index1 index2))
                       (power 4 index1)
                       (power a (add (neg index2) (neg index1) (mul 2 n)))
                       (power (add a (mul 2 b (power var (div 1 2))))
                              (add index1 index2))
                       (power (div (mul -1 u
                                        (power (add a
                                                    (mul 2
                                                         b
                                                         (power var (div 1 2))))
                                               2))
                                   b)
                              (mul (div -1 2) (add index1 index2 1)))
                       (take '(%binomial) index1 index2)
                       (take '(%binomial) n index1)
                       (add (mul a
                                 (add a (mul 2 b (power var (div 1 2))))
                                 (take '(%gamma_incomplete)
                                       (div (add index1 index2 1) 2)
                                       (div (mul -1 u 
                                                 (power (add a 
                                                             (mul 2 b 
                                                                  (power var 
                                                                         (div 1 2))))
                                                        2))
                                            (mul 4 b))))
                            (mul (inv u)
                                 (power (div (mul -1 u 
                                                  (power (add a 
                                                              (mul 2 b 
                                                                   (power var 
                                                                          (div 1 2))))
                                                         2))
                                             b)
                                        (div 1 2))
                                 (mul 2 b)
                                 (take '(%gamma_incomplete)
                                       (div (add index1 index2 2) 2)
                                       (div (mul -1 u 
                                                 (power (add a 
                                                             (mul 2 b 
                                                                  (power var (div 1 2))))
                                                        2))
                                            (mul 4 b))))))
                  index2 0 index1 t)
                index1 0 n t)))))
    
    ((and (setq w (m2-exp-type-7 (facsum-exponent expr)))
	  (eq ($sign (sub (cdras 'r w) (cdras 'r1 w))) '$zero))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (e (cdras 'e w))
	   (g (cdras 'g w))
	   (h (cdras 'h w))
	   (r (cdras 'r w))
	   (n (cdras 'n w)))

       (when *debug-integrate*
	 (format t "~&Type 7: z^n*a^(b*z^r+e)*h^(c*z^r+g) : w = ~A~%" w))

       (setq n (add n 1))

       (mul
         const
	 (power var n)
	 (div -1 r)
	 (power a e)
	 (power h g)
	 (power
	   (mul -1
	     (power var r)
	     (add (mul b ($log a)) (mul c ($log h))))
	   (div (mul -1 n) r))
	 ($gamma_incomplete
	   (div n r)
	   (mul -1 (power var r) (add (mul b ($log a)) (mul c ($log h))))))))
    
    ((and (setq w (m2-exp-type-7-1 (facsum-exponent expr)))
          (eq ($sign (sub (cdras 'r w) (cdras 'r1 w))) '$zero))
     (let ((b (cdras 'b w))
           (c (cdras 'c w))
           (e (cdras 'e w))
           (g (cdras 'g w))
           (r (cdras 'r w))
           (v (cdras 'v w))
           (q (cdras 'q w))
           (u (cdras 'u w)))
       
       (when *debug-integrate*
         (format t "~&Type 7-1: z^v*(%e^(b*z^r+e))^q*(%e^(c*z^r+g))^u : w = ~A~%" w))
       
       (mul const
            (div -1 r)
            (power '$%e (mul -1 (power var r) (add (mul b q) (mul c u))))
            (power (power '$%e (add e (mul b (power var r)))) q)
            (power (power '$%e (add g (mul c (power var r)))) u)
            (power var (add v 1))
            (power (mul -1 (power var r) (add (mul b q) (mul c u)))
                   (div (mul -1 (add v 1)) r))
            (take '(%gamma_incomplete)
                  (div (add v 1) r)
                  (mul -1 (power var r) (add (mul b q) (mul c u)))))))
    
    ((setq w (m2-exp-type-8 (facsum-exponent expr)))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (d (cdras 'd w))
	   (e (cdras 'e w))
	   (f (cdras 'f w))
	   (g (cdras 'g w))
	   (h (cdras 'h w)))

       (when *debug-integrate*
	 (format t "~&Type 8: a^(b*sqrt(z)+d*z+e)*h^(c*sqrt(z)+f*z+g)")
	 (format t "~&   : w = ~A~%" w))

       (mul
         const
	 (div 1 2)
	 (power a e)
	 (power h g)
	 (add
	   (mul 2
	     (power a (add (mul b (power var (div 1 2))) (mul d var)))
	     (power h (add (mul c (power var (div 1 2))) (mul f var)))
	     (div 1 (add (mul d ($log a)) (mul f ($log h)))))
	   (mul -1
	     (power '$%pi (div 1 2))
	     (power '$%e
	       (mul -1
		 (div
		   (power (add (mul b ($log a)) (mul c ($log h))) 2)
		   (mul 4 (add (mul d ($log a)) (mul f ($log h)))))))
	     ($erfi
	       (div
		 (add
		   (mul b ($log a))
		   (mul c ($log h))
		   (mul 2
		     (power var (div 1 2))
		     (add (mul d ($log a)) (mul f ($log h)))))
		 (mul 2
		   (power (add (mul d ($log a)) (mul f ($log h))) (div 1 2)))))
	     (add (mul b ($log a)) (mul c ($log h)))
	     (power (add (mul d ($log a)) (mul f ($log h))) (div -3 2)))))))
    
    ((setq w (m2-exp-type-8-1 (facsum-exponent expr)))
     (let ((b (cdras 'b w))
	   (c (cdras 'c w))
	   (d (cdras 'd w))
	   (e (cdras 'e w))
	   (f (cdras 'f w))
	   (g (cdras 'g w))
           (u (cdras 'u w))
           (v (cdras 'v w)))
       
       (when *debug-integrate*
	 (format t "~&Type 8-1: (%e^(b*sqrt(z)+d*z+e))^u*(%e^(c*sqrt(z)+f*z+g))^v")
	 (format t "~&   : w = ~A~%" w))
       
       (mul const
            (div 1 2)
            (power (add (mul d u) (mul f v)) (div -3 2))
            (mul (power '$%e
                        (mul -1 
                             (power (add (mul b u)
                                         (mul 2 d u (power var (div 1 2)))
                                         (mul v (add c (mul 2 f (power var (div 1 2))))))
                                    2)
                             (inv (mul 4 (add (mul d u) (mul f v))))))
                 (power (power '$%e
                               (add (mul b (power var (div 1 2)))
                                    e
                                    (mul d var)))
                        u)
                 (power (power '$%e
                               (add (mul c (power var (div 1 2)))
                                    g
                                    (mul f var)))
                        v)
                 (add (mul 2
                           (power '$%e
                                  (mul (power (add (mul b u)
                                                   (mul 2 d u (power var (div 1 2)))
                                                   (mul v (add c (mul 2 f (power var (div 1 2))))))
                                              2)
                                       (inv (mul 4 (add (mul d u) (mul f v))))))
                           (power (add (mul d u) (mul f v)) (div 1 2)))
                      (mul -1
                           (power '$%pi (div 1 2))
                           (add (mul b u) (mul c v))
                           (take '(%erfi)
                                 (div (add (mul b u)
                                           (mul 2 d u (power var (div 1 2)))
                                           (mul c v)
                                           (mul 2 f v (power var (div 1 2))))
                                      (mul 2
                                           (power (add (mul d u) (mul f v))
                                                  (div 1 2)))))))))))
    
    ((and (setq w (m2-exp-type-8-2 (facsum-exponent expr)))
          (eq ($sign (sub (cdras 'r w) (cdras 'r1 w))) '$zero))
     (let ((b (cdras 'b w))
           (c (cdras 'c w))
           (e (cdras 'e w))
           (g (cdras 'g w))
           (r (cdras 'r w))
           (u (cdras 'u w))
           (v (cdras 'v w)))
       
       (when *debug-integrate*
         (format t "~&Type 8-2: (%e^(b*z^r+e))^u*(%e^(c*z^r+g))^v")
         (format t "~&   : w = ~A~%" w))
       
       (mul const
            -1
            (inv r)
            (power '$%e
                   (mul -1
                        (power var r)
                        (add (mul b u) (mul c v))))
            (power (power '$%e
                          (add (power var r) e))
                   u)
            (power (power '$%e
                          (add (power var r) g))
                   v)
            var
            (power (mul -1
                        (power var r)
                        (add (mul b u) (mul c v)))
                   (div -1 r))
            (take '(%gamma_incomplete)
                  (div 1 r)
                  (mul -1 (power var r) (add (mul b u) (mul c v)))))))
    
    ((and (setq w (m2-exp-type-9 (facsum-exponent expr)))
	  (maxima-integerp (cdras 'n w))
	  (eq ($sign (cdras 'n w)) '$pos)
	  (or (not (eq ($sign (cdras 'b w)) '$zero))
	      (not (eq ($sign (cdras 'c w)) '$zero))))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (d (cdras 'd w))
	   (e (cdras 'e w))
	   (f (cdras 'f w))
	   (g (cdras 'g w))
	   (h (cdras 'h w))
	   (n (cdras 'n w)))

       (when *debug-integrate*
	 (format t "~&Type 9: z^n*a^(b*z^2+d*z+e)*h^(c*z^2+f*z+g)")
	 (format t "~&   : w = ~A~%" w))

       (mul
         const
	 (div -1 2)
	 (power a e)
	 (power h g)
	 (power '$%e
	   (div
	     (power (add (mul d ($log a)) (mul f ($log h))) 2)
	     (mul -4 (add (mul b ($log a)) (mul c ($log h))))))
	 (power (add (mul b ($log a)) (mul c ($log h))) (mul -1 (add n 1)))
         (let ((index (gensumindex))
               ($simpsum t))
	   (mfuncall '$sum
	     (mul
	       (power 2 (sub index n))
	       ($binomial n index)
	       (power
		 (add (mul -1 d ($log a)) (mul -1 f ($log h)))
		 (sub n index))
	       (power
		 (add
		   (mul (add d (mul 2 b var)) ($log a))
		   (mul (add f (mul 2 c var)) ($log h)))
		 (add index 1))
	       (power
		 (mul -1
		   (div
		     (power
		       (add
			 (mul (add d (mul 2 b var)) ($log a))
			 (mul (add f (mul 2 c var)) ($log h)))
		       2)
		     (add (mul b ($log a)) (mul c ($log h)))))
		 (div (add index 1) -2))
	       ($gamma_incomplete
		 (div (add index 1) 2)
		 (mul -1
		   (div
		     (power
		       (add
			 (mul (add d (mul 2 b var)) ($log a))
			 (mul (add f (mul 2 c var)) ($log h)))
		       2)
		     (mul 4 (add (mul b ($log a)) (mul c ($log h))))))))
	             index 0 n)))))
    
    ((and (setq w (m2-exp-type-9-1 (facsum-exponent expr)))
          (maxima-integerp (cdras 'n w))
          (eq ($sign (cdras 'n w)) '$pos)
          (or (not (eq ($sign (cdras 'b w)) '$zero))
              (not (eq ($sign (cdras 'c w)) '$zero))))
     (let ((b (cdras 'b w))
           (c (cdras 'c w))
           (d (cdras 'd w))
           (e (cdras 'e w))
           (f (cdras 'f w))
           (g (cdras 'g w))
           (q (cdras 'q w))
           (u (cdras 'u w))
           (n (cdras 'n w)))
       
       (when *debug-integrate*
         (format t "~&Type 9-1: z^n*(%e^(b*z^2+d*z+e))^q*(%e^(c*z^2+f*z+g))^u")
         (format t "~&   : w = ~A~%" w))
       
       (mul const
            (div -1 2)
            (power (add (mul b q) (mul c u)) (div -1 2))
            (power '$%e
                   (add (div (power (add (mul d q) (mul f u)) 2)
                             (mul -4 (add (mul b q) (mul c u))))
                        (mul -1 var
                             (add (mul d q)
                                  (mul b q var)
                                  (mul f u)
                                  (mul c u var)))))
            (power (power '$%e
                          (add e
                               (mul var (add d (mul b var)))))
                   q)
            (power (power '$%e
                          (add g
                               (mul var (add f (mul c var)))))
                   u)
            (let ((index (gensumindex))
                  ($simpsum t))
              (dosum
               (mul (power 2 (sub index n))
                    (power (add (mul b q) (mul c u)) (neg (add n (div 1 2))))
                    (power (add (neg (mul d q)) (neg (mul f u)))
                           (sub n index))
                    (power (add (mul d q)
                                (mul f u)
                                (mul 2 var (add (mul b q) (mul c u))))
                           (add index 1))
                    (power (div (power (add (mul d q)
                                            (mul f u)
                                            (mul 2
                                                 (add (mul b q)
                                                      (mul c u))
                                                 var))
                                       2)
                                (neg (add (mul b q) (mul c u))))
                           (mul (div -1 2) (add index 1)))
                    (take '(%binomial) n index)
                    (take '(%gamma_incomplete)
                          (div (add index 1) 2)
                          (div (power (add (mul d q)
                                           (mul f u)
                                           (mul 2
                                                (add (mul b q)
                                                     (mul c u))
                                                var))
                                      2)
                               (mul -4 (add (mul b q) (mul c u))))))
               index 0 n t)))))

    ((and (setq w (m2-exp-type-10 (facsum-exponent expr)))
          (maxima-integerp (cdras 'n w))
          (eq ($sign (cdras 'n w)) '$pos)
          (or (not (eq ($sign (cdras 'b w)) '$zero))
              (not (eq ($sign (cdras 'c w)) '$zero))))
     (let ((a (cdras 'a w))
           (b (cdras 'b w))
           (c (cdras 'c w))
           (d (cdras 'd w))
           (e (cdras 'e w))
           (f (cdras 'f w))
           (g (cdras 'g w))
           (h (cdras 'h w))
           (n (cdras 'n w)))
       
       (when *debug-integrate*
         (format t "~&Type 10: z^n*a^(b*sqrt(z)+d*z+e)*h^(c*sqrt(z)+f*z+g)")
         (format t "~&   : w = ~A~%" w))
       
       (mul const
            (power 2 (add (mul -2 n) -1))
            (power a e)
            (power h g)
            (power '$%e
                   (div (power (add (mul b ($log a)) (mul c ($log h))) 2)
                        (mul -4 (add (mul d ($log a)) (mul f ($log h))))))
            (power (add (mul d ($log a)) (mul f ($log h))) (mul -2 (add n 1)))
            (let ((index1 (gensumindex))
                  (index2 (gensumindex))
                  ($simpsum t))
              (dosum
               (dosum
                (mul (power -1 (sub index1 index2))
                     (power 4 index1)
                     ($binomial index1 index2)
                     ($binomial n index1)
                     (power (add (mul b ($log a)) (mul c ($log h)))
                            (sub (mul 2 n) (add index1 index2)))
                     (power (add (mul b ($log a))
                                 (mul c ($log h))
                                 (mul 2
                                      (power var (div 1 2))
                                      (add (mul d ($log a)) (mul f ($log h)))))
                            (add index1 index2))
                     (power (mul -1
                                 (div (power (add (mul b ($log a))
                                                  (mul c ($log h))
                                                  (mul 2
                                                       (power var (div 1 2))
                                                       (add (mul d ($log a)) 
                                                            (mul f ($log h)))))
                                             2)
                                      (add (mul d ($log a)) (mul f ($log h)))))
                            (mul (div -1 2) (add index1 index2 1)))
                     (add (mul ($gamma_incomplete (mul (div 1 2)
                                                       (add index1 index2 1))
                                                  (mul (div -1 4)
                                                       (div (power (add (mul b ($log a))
                                                                        (mul c ($log h))
                                                                        (mul 2
                                                                             (power var (div 1 2))
                                                                             (add (mul d ($log a)) (mul f ($log h)))))
                                                                   2)
                                                            (add (mul d ($log a)) (mul f ($log h))))))
                               (add (mul b ($log a)) (mul c ($log h)))
                               (add (mul b ($log a))
                                    (mul c ($log h))
                                    (mul 2
                                         (power var (div 1 2))
                                         (add (mul d ($log a)) (mul f ($log h))))))
                          (mul 2
                               ($gamma_incomplete (mul (div 1 2)
                                                       (add index1 index2 2))
                                                  (mul (div -1 4)
                                                       (div (power (add (mul b ($log a))
                                                                        (mul c ($log h))
                                                                        (mul 2
                                                                             (power var (div 1 2))
                                                                             (add (mul d ($log a))
                                                                                  (mul f ($log h)))))
                                                                   2)
                                                            (add (mul d ($log a))
                                                                 (mul f ($log h))))))
                               (add (mul d ($log a)) (mul f ($log h)))
                               (power (mul -1
                                           (div (power (add (mul b ($log a))
                                                            (mul c ($log h))
                                                            (mul 2
                                                                 (power var (div 1 2))
                                                                 (add (mul d ($log a))
                                                                      (mul f ($log h)))))
                                                       2)
                                                (add (mul d ($log a))
                                                     (mul f ($log h)))))
                                      (div 1 2)))))
                index2 0 index1 t)
               index1 0 n t)))))
    
    ((and (setq w (m2-exp-type-10-1 (facsum-exponent expr)))
          (maxima-integerp (cdras 'n w))
          (eq ($sign (cdras 'n w)) '$pos)
          (or (not (eq ($sign (cdras 'b w)) '$zero))
              (not (eq ($sign (cdras 'c w)) '$zero))))
     (let* ((b (cdras 'b w))
            (c (cdras 'c w))
            (d (cdras 'd w))
            (e (cdras 'e w))
            (f (cdras 'f w))
            (g (cdras 'g w))
            (q (cdras 'q w))
            (u (cdras 'u w))
            (n (cdras 'n w))
            (bq+cu (add (mul b q) (mul c u)))
            (dq+fu (add (mul d q) (mul f u))))
       
       (when *debug-integrate*
         (format t "~&Type 10-1: z^n*(%e^(b*sqrt(z)+d*z+e))^q*(%e^(c*sqrt(z)+f*z+g))^u")
         (format t "~&   : w = ~A~%" w))
       
       (mul const
            (power 2 (mul -1 (add (mul 2 n) 1)))
            (power '$%e
                   (add (div (mul -1 (power bq+cu 2)) (mul 4 dq+fu))
                        (mul -1 d var q)
                        (mul -1 b (power var (div 1 2)) q)
                        (mul -1 f var u)
                        (mul -1 c (power var (div 1 2)) u)))
            (power (power '$%e 
                          (add (mul b (power var (div 1 2)))
                               (mul d var)
                               e))
                   q)
            (power (power '$%e
                          (add (mul c (power var (div 1 2)))
                               (mul f var)
                               g))
                   u)
            (power dq+fu (mul -2 (add n 1)))
            (let ((index1 (gensumindex))
                  (index2 (gensumindex))
                  ($simpsum t))
              (dosum
               (dosum
                (mul (power -1 (sub index1 index2))
                     (power 4 index1)
                     (power bq+cu
                            (add (neg index1) (neg index2) (mul 2 n)))
                     (power (add bq+cu
                                 (mul 2 (power var (div 1 2)) dq+fu))
                            (add index1 index2))
                     (power (div (power (add bq+cu
                                             (mul 2 
                                                  (power var (div 1 2))
                                                  dq+fu))
                                        2)
                                 (mul -1 dq+fu))
                            (mul (div -1 2)
                                 (add index1 index2 1)))
                     (take '(%binomial) index1 index2)
                     (take '(%binomial) n index1)
                     (add (mul bq+cu
                               (add bq+cu
                                    (mul 2
                                         (power var (div 1 2))
                                         dq+fu))
                               (take '(%gamma_incomplete)
                                     (mul (div 1 2)
                                          (add index1 index2 1))
                                     (div (power (add (mul b q)
                                                      (mul c u)
                                                      (mul 2
                                                           (power var (div 1 2))
                                                           dq+fu))
                                                 2)
                                          (mul -4
                                               dq+fu))))
                          (mul 2
                               (power (div (power (add bq+cu
                                                       (mul 2
                                                            (power var (div 1 2))
                                                            dq+fu))
                                                  2)
                                           (mul 1 dq+fu))
                                      (div 1 2))
                               dq+fu
                               (take '(%gamma_incomplete)
                                     (mul (div 1 2)
                                          (add index1 index2 2))
                                     (div (power (add bq+cu
                                                      (mul 2
                                                           (power var (div 1 2))
                                                           dq+fu))
                                                 2)
                                          (mul -4
                                               dq+fu))))))
                index2 0 index1 t)
               index1 0 n t)))))
    
    (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Do a facsum for the exponent of power functions.
;;; This is necessary to integrate all general forms. The pattern matcher is
;;; not powerful enough to do the job.

(defun facsum-exponent (expr)
  ;; Make sure that expr has the form ((mtimes) factor1 factor2 ...)
  (when (not (mtimesp expr)) (setq expr (list '(mtimes) expr)))
  (do ((result nil)
       (l (cdr expr) (cdr l)))
      ((null l) (cons (list 'mtimes) result))
    (cond
      ((mexptp (car l))
       ;; Found an power function. Factor the exponent with facsum.
       (let* ((fac (mfuncall '$facsum (caddr (car l)) var))
              (num ($num fac))
              (den ($denom fac)))
         (setq result
               (cons (cons (list 'mexpt) 
                           (cons (cadr (car l))
                                 (if (equal 1 den)
                                     (list num)
                                     (list ($multthru (inv den) num)))))
                     result))))
      (t
       ;; Nothing to do.
       (setq result (cons (car l) result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

