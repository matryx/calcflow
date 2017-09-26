;;; -*- Package: MAXIMA; Base: 10.; Syntax: Common-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(eval-when
    #+gcl (compile load eval)
    #-gcl (:compile-toplevel :load-toplevel :execute)

    (defmacro p-cof (f)			;leading coefficient of f
      `(third ,f))

    (defmacro p-next-term (f)
      `(cddr ,f))

    (defmacro p-deg (f)
      `(second ,f))

    (defmacro term-cof (terms)
      `(second ,terms))

    (defmacro term-deg (terms)
      `(first ,terms)))

;;(make-poly var x)==> (list x 1 1)

(defun make-polynomial (&key var terms)
  (psimp var terms))

(defun afc-quotient (f g)
  (cquotient f g))

(defun fsignal (&rest l)
  (error (car l)))

(defmacro working-modulo (list-of-monic-polynomials &body body &aux (old-tellrats (make-symbol "old-tellrats")))
  "The computations in body are done modulo the list-of-monic-polynomials.  The
   results of squareing,multiplication, and exponentiating should be of lower degree in each of the
   monic polynomials than the degree of the monic polynomial"
  `(let ((,old-tellrats (list-previous-tellrats ,list-of-monic-polynomials)))
  (unwind-protect
      (progn
	(set-tellrats ,list-of-monic-polynomials)
	,@body)
     (undo-tellrats ,old-tellrats))))

;;sample usage
;;(working-modulo (st-rat #$[x^2+x+1,y^4+y]$) (ptimes ...))

(defun list-previous-tellrats (new-tellrats)
  (loop for v in new-tellrats
	collecting (cons (car  v) (get (car v) 'tellrat))))

(defun set-tellrats (new-tellrats)
  (loop for v in new-tellrats
	do (putprop (car v) (cdr v) 'tellrat)))

(defun undo-tellrats (old-list)
  (loop for v in old-list
	when (null (cdr v))
	  do (remprop (car v) 'tellrat)
	else do (putprop (car v) (cdr v) 'tellrat)))

;;version for possibly some zero terms resulting
;;using nconc less space but slower since (list a b) is cdr coded and it has to fix up.
(defmacro term-operation (f g operation-function &optional deg-shift-result)
  "If f and g are polynomials this constructs polynomial whose main variable is the same as f  ~
   with coefficients (operation-function f_i g)..[f_i means the i'th coefficient of f
  degree (+ i deg-shift-result)"
  (let ((cof (make-symbol "cof"))
	(deg (make-symbol "deg"))
	(tem (gensym)))
    `(psimp  (p-var ,f)
		      (loop for (,deg ,cof) on (cdr ,f) by #'cddr
			    with ,tem
			    do (setq ,tem (,operation-function ,cof ,g))
			    when (not (pzerop ,tem))
			    nconc (list , (cond (deg-shift-result `(+ ,deg ,deg-shift-result))
					 (t  deg))
					,tem)))))

(defmacro plain-term-operation (terms-f terms-g operation-function &optional deg-shift-result)
  "constructs terms of a polynomial with coefficients (operation-function f_i terms-g) in
  degree (+ i deg-shift-result)"
  (let ((cof (make-symbol "cof"))
	(deg (make-symbol "deg"))
	(tem (gensym)))
    `(loop for (,deg ,cof) on ,terms-f by #'cddr
	   with ,tem
	   do (setq ,tem (,operation-function ,cof ,terms-g))
	   when (not (pzerop ,tem))
	     nconc (list ,(cond (deg-shift-result `(+ ,deg ,deg-shift-result))
			  (t deg))
			 ,tem))))

(defmacro quotient-not-exact ()
  `(cond ((and (boundp '*testing*)
	     *testing*)
	 (throw 'quotient-not-exact t))
	(t (fsignal 'quotient-not-exact))))


(DEFUN afp-CQUOTIENT (A B)
  (declare (special *testing*))
       (COND ((EQl A 0) 0)
	     ((NULL MODULUS)
	       (let ((quot (quotient a b)))
		     (cond ((eql (* quot b) a)
			    quot)
			   (t (quotient-not-exact)))))
	     (t (ctimes a (crecip b)))))


;;The following works to eliminate zero terms when using modulus etc.
;;It is also faster than ptimes by a slight margin.
;;unlike ptimes which may introduce zero terms eg:
;;(let ((modulus 4))(ptimes (x+y+z)^4 (x+y+z)^4) gives a bad result



(defun afp-terms-times (terms-f terms-g &aux answ (g-orig terms-g) prod-exp tail prod-cof)
  "Returns the terms of the polynomial which is the product of the two polynomials ~
   whose terms are terms-f and terms-g.  If modulus is in effect the the result will have its ~
   coefficients reduced by modulus."
  (cond
    (terms-g (setq answ (plain-term-operation terms-g (term-cof terms-f) afp-times (term-deg terms-f)))
       (loop for (f-exp f-cof ) on (cddr terms-f) by #'cddr
	     do
	 (setq terms-g g-orig)
	 (prog ()
	    first-product
	       (cond ((null terms-g)(return nil)))
	       (setq prod-exp (+ f-exp (term-deg terms-g)))
	       (setq prod-cof (afp-times(term-cof terms-g) f-cof))
	       (cond ((pzerop prod-cof) (setq terms-g (cddr terms-g)) (go first-product))
		     ((or (null answ) (> prod-exp (term-deg answ)))
		      (setq answ (cons prod-exp (cons prod-cof answ)))
		      (setq tail (cdr answ))                   (go next-product))
		     ((eql prod-exp (term-deg answ))
		      (setq prod-cof (pplus (term-cof answ) prod-cof))
		      (cond ((pzerop prod-cof) (setq answ (cddr answ))
			     (setq terms-g (cddr terms-g))          (go first-product))
			    (t (setf (term-cof answ) prod-cof)
			       (setq tail (cdr answ))        (go next-product)))))
	       ;;below here assume answ not empty and (term-deg answ)
	       ;;greater any possible future prod-exp (until next
	       ;;f-exp,f-cof)  Once below this point we stay below,
	       ;;until next f-exp,f-cof .  Tail begins with the
	       ;;coefficient whose corresponding degree is definitely
	       ;;higher than any prod-exp to be encountered.
	       (setq tail (cdr answ))
	    tail-certain
	       (cond ((and (cdr tail)(> (second tail) prod-exp))
		      (setq tail (cddr tail))               (go tail-certain)))
	       (cond ((or (null (cdr tail))(< (second tail) prod-exp))
		      (setf (cdr tail) (cons prod-exp (cons prod-cof (cdr tail))))
		      (setq tail (cddr tail))                (go next-product)))
	       (cond ((pzerop (setq prod-cof (pplus (third tail) prod-cof)))
		      (setf (cdr tail) (cdddr tail)))
		     (t (setf (third tail) prod-cof) (setq  tail (cddr tail))))
	    next-product
	       (setq terms-g (cddr terms-g))
	       (cond ((null terms-g) (return nil)))
	       (setq prod-exp (+ f-exp (car terms-g)))
	       (setq prod-cof (afp-times (second terms-g) f-cof))
	       (cond ((pzerop prod-cof)                    (go next-product)))
	       (go tail-certain)))))
  answ)

(defmacro afp-main-plus-non-main (constant  f-main)
  "Adds a polynomial CONSTANT to a polynomial whose main variable is higher than
   any in F-MAIN"
  `(psimp (p-var ,f-main) (afp-constant-term-plus ,constant (cdr ,f-main))))

(defmacro afp-number-plus (number poly)
  "Adds a NUMBER to a polynomial POLY, returning the result"
  `(cond ((numberp ,poly)(cplus ,number ,poly))
	 (t (afp-main-plus-non-main ,number  ,poly))))

(defun afp-plus (f g)
  "Returns the sum of the two polynomials f and g"
  (cond ((numberp f) (afp-number-plus f g))
	((numberp g) (afp-number-plus g f))
	((eq (p-var f) (p-var g))
	 (psimp (p-var f)
		    (afp-terms-plus (cdr f) (cdr g))))
	((pointergp (p-var f) (p-var g)) (afp-main-plus-non-main g f))
	(t  (afp-main-plus-non-main f g))))

(defun afp-constant-term-plus (constant terms)
  "Adds a polynomial (CONSTANT) not involving the main variable of a polynomial whose
  terms are TERMS.  Naturally the main variable is assumed higher than any in the CONSTANT. ~
  The result is the terms of the sum polynomial"
  (cond ((pzerop constant) terms)
	((null terms)
	 (list  0 constant))
	((zerop (car terms))(setq constant  (afp-plus constant (second terms)))
	 (cond ((pzerop constant)nil)
		(t (list 0 constant))))
	(t (cons (car terms) (cons (second terms) (afp-constant-term-plus constant (cddr terms)))))))


(defun afp-terms-plus (terms-f terms-g &aux e)
  "Returns the terms of the polynomial which is the sum of f and g
  if the terms of f and the terms of g are the two arguments."
    (cond ((null terms-f) terms-g)
	  ((null terms-g) terms-f)
	  ((eql (car terms-f) (car terms-g))
	   (setq e (afp-plus (second terms-f) (second terms-g)))
	   (cond ((pzerop e)(afp-terms-plus (cddr terms-f) (cddr terms-g)))
		 (t (cons (car terms-f) (cons e (afp-terms-plus (cddr terms-f) (cddr terms-g)))))))
	  ((> (car terms-f) (car terms-g ))(cons (car terms-f) (cons (second terms-f)
				    (afp-terms-plus terms-g (cddr terms-f)))))
	  (t(cons (car terms-g) (cons (second terms-g)
				    (afp-terms-plus terms-f (cddr terms-g)))))))


(defmacro qfirstn (n l)
  (case n
    (1 `(list (car ,l)))
    (2 `(list (car ,l) (second ,l)))
    (t `(subseq ,l 0 ,n))))

(defun afp-minus (f)
  "makes no check that keeping in the modulus range nor that the result is reduced,
   but the result g will satisfy (afp-plus f g) ==> 0"
  (cond ((numberp f) (cminus f))
	(t (cons (car f) (afp-terms-minus (cdr f))))))

(defun afp-terms-minus (terms-f)
  (loop for (deg pol) on terms-f by #'cddr nconc (list deg (afp-minus pol))))

(defmacro add-one-term (deg cof terms)
  (cond ((symbolp cof)
	 `(cond ((pzerop ,cof) ,terms)
		(t (cons ,deg (cons ,cof ,terms)))))
	(t
	`(let ((.cof. ,cof))
	   (cond ((pzerop .cof.) ,terms)
		 (t (cons ,deg (cons .cof. ,terms))))))))

(defun afp-difference (f g)
  (cond ((numberp f)
	 (cond ((numberp g)(cdifference f g))
	       (t (psimp (p-var g) (afp-terms-constant-main-differ f (cdr g))))))
	((numberp g)
	 (psimp (p-var f) (afp-terms-main-constant-differ (cdr f)  g)))
	((eq (p-var f) (p-var g))
	 (psimp (p-var f) (afp-terms-differ (p-terms f) (p-terms g))))
	((pointergp (p-var f) (p-var g))
	 (psimp (p-var f) (afp-terms-main-constant-differ
				   (cdr f) g)))
	(t(psimp (p-var g) (afp-terms-main-constant-differ
				 f (cdr g))))))



(defun afp-terms-differ (terms-f terms-g)
  (cond ((null terms-f) (afp-terms-minus terms-g))
	((null terms-g) terms-f)
	((eql (term-deg terms-f) (term-deg terms-g))
	 (add-one-term (term-deg terms-f) (afp-difference (term-cof terms-f) (term-cof terms-g))
		       (afp-terms-differ (cddr terms-f) (cddr terms-g))))
	((> (term-deg terms-f) (term-deg terms-g))
	 (cons (term-deg terms-f) (cons  (term-cof terms-f) (afp-terms-differ (p-next-term terms-f) terms-g))))
	(t
	 (cons (term-deg terms-g) (cons  (afp-minus (term-cof terms-g)) (afp-terms-differ  terms-f (p-next-term terms-g)))))))

(defun afp-terms-constant-main-differ (const main)
  "main is terms const is a polynomial"
  (cond ((null main)(cond ((pzerop const) nil)
			  (t (list 0 const))))
	((zerop  (car main))
	 (add-one-term 0 (afp-difference (second main) const) nil))
	(t (cons (car main) (cons  (afp-minus (second main)) (afp-terms-constant-main-differ const (cddr main)))))))

(defun afp-terms-main-constant-differ (main const)
  (cond ((null main)(cond ((pzerop const) nil)
			  (t (list 0 (afp-minus const)))))
	((zerop (car main))
	 (add-one-term 0 (afp-difference (second main) const) nil))
	(t (cons (car main) (cons (second main) (afp-terms-main-constant-differ (cddr main) const))))))

;;assumes divides evenly, integer coefficients.
;;signal error otherwise the flavor of the error should be specified.
(defun afp-quotient (f g)
  "Tries to divide the polynomial f by g. One has result*g=f, or else ~
  it signals an error."
  (declare (special *testing*))
  (cond ((numberp f)
	 (cond ((numberp g)
		(afp-cquotient f g))
	       ((zerop f) 0)
	       (t (quotient-not-exact))))
	((numberp g)
	 (term-operation f g afp-quotient))
	((pointergp (p-var f) (p-var g))
	 (term-operation f g afp-quotient))
	((eq (p-var f) (p-var g))
	 (loop
	       with quot with deg-dif with main-var = (p-var f) with minus-quot
	       do
	       (setq  deg-dif (- (p-deg f) (p-deg g)))
	       while (>= deg-dif 0)
       	       collecting deg-dif into q
	       collecting  (setq quot (afp-quotient (p-cof f) (p-cof g)))
	       into q
	       do (setq minus-quot (pminus quot))
	       (setq f (pplus
			f
			(term-operation g
					;;-fn/gm
					minus-quot
					ptimes
					deg-dif)))
	       while (not (and (numberp f) (zerop f)))
	       when (or (numberp f) (not (eq main-var (p-var f)))
			(< (p-deg f) (p-deg g)))
		 do (quotient-not-exact)
	       finally (return (make-polynomial :terms q :var main-var))))
	(t (quotient-not-exact))))


(defun afp-test-divide (f g &aux ( *testing* t) quot)
  (declare (special *testing*))
  (catch 'quotient-not-exact (setq quot (afp-quotient f g)))
  (cond (quot quot)
	(t nil)))

(defun afc-remainder (n divisor)
  (multiple-value-bind (q r) (truncate n divisor) (values r q)))

;;pseudo division as in Knuth's book.

(defun afp-pseudo-quotient (f g &aux (creqd 1))
  "This function returns the values: quotient, remainder and creqd ~
   so that creqd*f=g*quotient+remainder.  Creqd  does not involve the
   main variable of f.  The remainder has degree lower than g with respect
   to the main variable of f."
  (cond ((numberp f)
	 (cond ((numberp g)
		(apply #'values (append '(1) (multiple-value-list (afc-remainder f g)))))
	       (t (values 0 f 1))))
	((numberp g)
	 (values f 0 g))
	((pointergp (p-var f) (p-var g))
	 (values f 0 g))
	((eq (p-var f) (p-var g))
	 (loop with quot with deg-dif with main-var = (p-var f)
	    with remainder =
	      (cond ((and (numberp (p-cof g)) modulus) f)
		    ((and (numberp (p-cof g))
			  (eql (abs (p-cof g)) 1)) f)
		    (t 	(ptimes f (setq creqd (pexpt (p-cof g)
						     (+ 1 (- (p-deg f) (p-deg g))))))))
	    do
	      (setq deg-dif (- (p-deg remainder) (p-deg g)))
	    while (>= deg-dif 0)
	    collecting deg-dif into q
	    collecting  (setq quot (afp-quotient (p-cof remainder) (p-cof g)))
	    into q
	    do
	      (setq remainder (pplus
			       remainder
			       (term-operation g
					       ;;-fn/gm
					       (pminus quot)
					       ptimes deg-dif)))
	    while (and (not (numberp remainder))
		       (eql (p-var remainder) main-var))
	    finally
	      (setq quot (make-polynomial :terms q :var main-var))
					;		 (iassert (eql 0 (pdifference  (ptimes f creqd ) (Pplus remainder (ptimes quot g)))))
	      (return (values quot remainder creqd))))
	(t (values 0 f 1))))

(defmacro assume-pointerg-or-equal-p (f g &optional reverse-flag)
  `(cond (( gen-pointergp ,f ,g) nil)
	 (t
	  ,@ (cond (reverse-flag (list `(setq ,reverse-flag (null ,reverse-flag)))))
	  (rotatef ,f ,g))))

(defun gen-pointergp (f g)
   (cond ((numberp g) t)
	 ((numberp f) nil)
	 (t (pointergp (p-var f) (p-var g)))))

(defun gen-degree (f)
  (cond ((numberp f) 0)
	(t (p-deg f))))

(defmacro assume-greater-equal-degree (f g &optional reverse-flag)
     `(cond ((>= (gen-degree ,f) (gen-degree ,g)) nil)
	    (t
	    	  ,@ (cond (reverse-flag (list `(setq (,reverse-flag (null ,reverse-flag))))))
	     (rotatef ,f ,g))))


(defun afp-content (f )
  "Returns the gcd of the coefficients of f (with respect to the main variable) ~
   if f is a polynomial"
  (cond ((numberp f) f)
	(t (loop for (deg cof) on (cdddr f) by #'cddr
		 with cont = (p-cof f)
		 do (setq cont (afp-gcd cont cof))
		 finally (return cont)))))

(defun principal-part (f)
  (afp-quotient f (afp-content f)))

(defvar *afp-gcd* 'subresultant)

(defun afp-gcd (f g  &aux answer)
    "Returns the gcd of its two arguments which may be any polynomial ~
   with integer coefficients. "
   (setq answer
   (case *afp-gcd*
	    (euclidean (afp-euclidean-gcd f g))
	    (subresultant (afp-subresultant-gcd f g))
	    (t (merror "~%The value of the switch *afp-gcd* ~A is not legal." *afp-gcd*))))
   (cond (modulus (afp-try-make-monic answer))
	 (t answer)))

;;This is the Euclidean gcd algorithm, as in Knuth.
(defun afp-euclidean-gcd (f g &aux u v r d  contf contg)
    "Returns the gcd of its two arguments which may be any polynomial ~
   with integer coefficients.  Currently uses the Euclidean algorithm, but should
   have a switch"
  (assume-pointerg-or-equal-p f g)
  (cond ((numberp f)(gcd f g))
	((gen-pointergp f g)
	 (afp-gcd (afp-content f) g))
;	 (loop for (deg cof) on (p-terms f) by #'cddr
;			with gcd = g
;			do (setq gcd (afp-gcd cof gcd))
;			   finally (return gcd)))
	((eq (p-var f) (p-var g))
 	 (setq d (afp-gcd
		   (setq contf (afp-content f))(setq contg (afp-content g))))
	 (setq u (afp-quotient f contf))
	 (setq v (afp-quotient g contg))
	 (assume-greater-equal-degree u v)
	 (loop with unused
	    do (multiple-value-setq (unused  r) (afp-pseudo-quotient u v))
	    when (pzerop r)
	      do (return (ptimes d v))
	    when (gen-pointergp f r)
	      do (return  d)
	    do (setq u v)
	       (setq v (afp-quotient r (afp-content r)))))
	(t (fsignal 'should-not-get-here))))


;;This was about twice as fast as the regular maxima pgcd on
;;some simple functions: 121 msec. as opposed to 250 msec.
;;this was with the afp-content in terms of the old afp-gcd.
;; (afp-subresultant-gcd-compare (st-rat #$8*3*(x^2+1)*(x+1)*(z^2+2)*(x+2)$)(st-rat #$15*(x^2-1)*(x^2+1)*(y^2+4)*(z^4+2*z+1)$))
;(user:compare-recursive-functions
(defun afp-subresultant-gcd (f g &aux delta u v r d contf contg  answ)
  "Returns the gcd of its two arguments which may be any polynomial ~
   with integer coefficients.  It uses the subresultant algorithm"
  (assume-pointerg-or-equal-p f g)
  (cond ((numberp f)(gcd f g))
	((gen-pointergp f g)
	 (cond ((pzerop g) f)
	       (t
		(afp-subresultant-gcd (afp-content f) g))))
	((eq (p-var f) (p-var g))
	 (setq d (afp-subresultant-gcd
		   (setq contf (afp-content f))
		   (setq contg (afp-content g))))
	 (setq u (afp-quotient f contf))
	 (setq v (afp-quotient g contg))
	 (assume-greater-equal-degree u v)
	 (setq answ (loop with gg = 1 with h = 1 with g^delta with unused
			  do
		      (setq delta (- (p-deg u) (p-deg v)))
		      (multiple-value-setq (unused  r) (afp-pseudo-quotient u v))
			  when (pzerop r)
			    do (return (ptimes d (principal-part v)))
			  when (gen-pointergp f r)
			    do (return  d)
			  do (setq u v)
			     (setq v (afp-quotient r (ptimes gg (pexpt h delta))))
			     (setq gg (p-cof u))
			     (setq g^delta (pexpt gg delta))
			     (setq h (cond ((eql delta 1)  g^delta)
					   ((> delta 1) (afp-quotient g^delta
								      (pexpt h (- delta 1))))
					   ;;here delta=0
					   (t (ptimes g^delta h))))))
	 (afp-try-make-monic answ))))


(defun one-ptimes (f g)
  (cond ((eql f 1) g)
	((eql g 1) f)
	(t (ptimes f g))))

(defun exponent-product (&rest alternating-factor-exponent-list)
  "Exponents may be positive or negative, but assumes result is poly"
  (loop for (fact deg) on alternating-factor-exponent-list by #'cddr
	with numer = 1 with  denom = 1
	when (= deg 1)
	do (setq numer (one-ptimes numer fact))
	else
	when (> deg 1)
	do (setq numer (one-ptimes numer (pexpt fact deg)))
	else
	when (= deg -1)
	do (setq denom (one-ptimes denom fact))
	else
	when (< deg -1)
	do (setq denom (one-ptimes denom (pexpt fact (- deg))))
	finally (return (afp-quotient numer denom))))

(defun same-main-and-degree (f g)
  (cond ((numberp f)(numberp g))
	((numberp g)(numberp f))
	(t
	 (and (eq (car f) (car g))
	      (eq (second f) (second g))))))
;;unfinished.
;(defun afp-sqfr (f &aux deriv d)
;  (cond ((numberp f) f)
;        (t
;         (loop
;	 do
;         (setq deriv (pderivative f (p-var f)))
;        (setq d (afp-gcd f deriv))
;           (cond ((numberp d ) f)
;               ((same-main-and-degree d f)
;                (make-polynomial :var (p-var f)
;                                 :teerms
;                                 (loop for (deg cof) on (cdr f) by #'cddr
;                                       collecting (quotient deg modulus)
;                                       collecting cof)))
;               (t (setq f (afp-quotient f d))))))))

(defun afp-big-gcd (f g &aux tem)
  "The arguments may be polynomials with integer coefficients. ~
   Three values are returned:  gcd , f/gcd,  and g/gcd."
  (values (setq tem (afp-subresultant-gcd f g)) (afp-quotient f tem) (afp-quotient g tem)))

(defun afp-pgcdcofacts (f g)
  (multiple-value-list (afp-big-gcd f g)))
;;this used 3 times the space and was much slower for the (x+y+z)^10
;;problem but if I put z=1 then it went much faster, and used less
;;space, but still not as good as the subresultant algorithm.  It was
;;about as fast as my subresultant algorithm.
;(defun afp-big-gcd (f g)
;  (declare (values gcd-of-f-g f-over-gcd g-over-gcd))
;  (apply 'values (pgcdcofacts f g)))

#+debug
(defun test (f g)
  (multiple-value-bind (d qf qg)
      (afp-big-gcd f g)
    (iassert (equal f (ptimes d qf)))    (iassert (equal g (ptimes d qg)))))

(defun afp-square-free-with-modulus (u &aux (vari (list-variables u)) root deriv  quot agcd)
  (check-arg vari (null (cdr vari)) "univariate when modulus")
  (check-arg modulus (primep modulus) "prime")
  (cond ((numberp u) u)
	(t (setq agcd (afp-gcd u (setq deriv (pderivative u (p-var u)))))
	   (cond ((numberp agcd) (list u 1))
		 ((> (p-deg u) (p-deg agcd))
		  (setq quot (afp-quotient u agcd))
		  (append (afp-square-free-with-modulus quot) (afp-square-free-with-modulus
								agcd)))
		 ;;deriv is 0
		 (t (check-arg deriv (eql 0 deriv) "zero")
		    (setq root (psimp (p-var u)
				      (loop for (deg cof) on (cdr u) by #'cddr
					    collecting (quotient deg modulus)
					    collecting cof)))
		    (loop for (pol deg) on  (afp-square-free-with-modulus root) by #'cddr
			  collecting pol collecting (* deg modulus)))))))

;;timing on factoring the (x+y+z)^10 2.6 sec 10,047 words
;;timing on factoring the (x+y+z)^20 20.2 sec 37,697 words

(defun afp-square-free-factorization (u &aux d tx v1 w1 some-factors unit)
  "returns an alternating list of factors and exponents. In the characteristic 0 case each factor is ~
  guaranteed square free and relatively prime to the other factors. Note that if
  modulus is not zero then u should be univariate.  Otherwise for eg mod 3, x^3-t
  is not square free in the field with cube root of t adjoined, and it can't be factored
  in Z/3Z[x,t]."
  (cond (modulus
	 (afp-square-free-with-modulus u))
	(t
	 (cond ((numberp u)
		u)
	       (t
		(setq d (afp-content u))
		(setq u (term-operation u d afp-quotient))
		(multiple-value-setq (tx v1 w1)
		  (afp-big-gcd u (pderivative u (p-var u))))
					;       (show tx v1 w1)
		(setq some-factors
		      (cond ((eql tx 1)
			     (list u 1))
			    ((numberp tx)
			     (fsignal 'how-did-this-happen))
			    (t
			     (loop for i from 1
				with vi = v1 with wi = w1 with videriv with vi+1 with ui with wi+1
				with main-var = (p-var u)
				do	; (show i)
				(setq videriv (pderivative vi main-var))
					;			  (show factor-list)
				when (equal wi videriv)
				do
				(return (append factor-list (list vi i)))
				do
				(multiple-value-setq (ui vi+1 wi+1)
				  (afp-big-gcd vi (pdifference wi
							       videriv)))
					;		   (show vi wi ui  vi+1 wi+1)
				when (not (eql ui 1))
				nconc (list ui i) into factor-list
				do
				(setq vi vi+1)
				(setq wi wi+1)
				))))
					;       (show some-factors)
		;;this is all to collect some numbers and fix the unit multiple.
		(loop for (pol deg) on some-factors by #'cddr
		   with answ = d
		   do (setq answ (ptimes answ (pexpt (p-cof pol) deg)))
		   finally
		     (setq unit (afp-quotient (p-cof u) answ))
		     (cond ((eql unit 1) nil)
			   ((eql unit -1) (loop for (pol1 deg1) on some-factors by #'cddr
					     for i from 0 by 2
					     when (oddp deg1) do (setf (nth i some-factors)
								       (pminus pol1))
					       (return 'done)
					     finally (merror "no odd factors yet differs by minus ")))
			   (t (fsignal "not handled yet"))))
		(cond ((eql d 1) some-factors)
		      (t (append (afp-square-free-factorization d) some-factors))))))))



;;tested on (x+y+z)^10 times itself and got about
;;the same time as ptimes 3100 milliseconds. in temporary area.
;;It used 10% more space. 205,000 for (x+y+z)^20*(x+y+z)^10 for ptimes.
;;note on the st-rat form it only takes 510 msec. for (x+y+z)^10  and
;;2.00 sec for (x+y+z)^20  as opposed to 3 sec and 15 sec resp with
;;;50000
;                         afp-square-free-factorization   pfactor
;  poly (x+y+z)^10 cre       .51   7,887                     3.0       17000
;  poly (x+y+z)^10 general   1.8   28,00                     4.5       68,000
;  poly (x+y+z)^20 cre       2.0   28,000                   15.       252,000
;  poly (x+y+z)^20 general   7.2   105,000                  20.8      325,000
;
;
;(compare-functions
;(defun af-fake-times (f g)
; (user:tim (progn (afp-times f g) nil)))
;(defun reg-fake-times (f g)
; (user:tim   (progn (ptimes f g) nil)))
;)


;;essentially same speed as regualar ptimes or maybe 5 %faster
;;does (afp-times (x+y+z)^10 times itself in 2530 msec. and 2640 for ptimes.


(defun afp-times (f g)
  "The two arguments are polynomials and the result is the product polynomial"
  (cond ((numberp f)
	 (cond ((zerop f) 0)
	       (t (afp-pctimes g f ))))
	((numberp g)
	 (cond ((zerop g) 0)
	       (t(afp-pctimes f g))))
	((eq (p-var f) (p-var g))
	 (palgsimp (p-var f) (afp-terms-times (cdr f)(cdr g)) (alg f)))
	((pointergp (p-var f) (p-var g))
	 (psimp (p-var f) (plain-term-operation (cdr f)  g afp-times)))
	(t(psimp (p-var g) (plain-term-operation (cdr g)  f afp-times)))))


;;;the following shuffles the terms together and is about the same speed as the
;;;corresponding maxima function ptimes1
;(defun afp-terms-times (terms-f terms-g &aux prev to-add repl new-deg one-product)
;  "assumes same main variable"
;  (loop while terms-f
;	do
;    (setq one-product
;	  (plain-term-operation terms-g (term-cof terms-f) afp-times (term-deg terms-f)))
;	until one-product
;	do (setq terms-f (cddr terms-f)))
;  (cond ((null one-product) nil)
;	(t
;	 (loop for (deg-f cof-f) on (cddr terms-f) by 'cddr
;	       do
;	   (loop for (deg cof) on terms-g by #'cddr     ;;sue
;		 ;;computes the coeff of x^deg+deg-g and adds it in to
;		 ;;the terms of one-product.  Prev stands for the terms beginning
;		 ;;with where the previous one was added on, or
;		 initially
;		   (setq prev one-product)
;		 do (setq new-deg (+ deg deg-f))
;		    (setq to-add (afp-times cof-f cof))
;		    (cond ((pzerop to-add))
;			  ;;you should only get here when not in an integral domain
;			  ((>= new-deg(car prev))
;			   (cond ((> new-deg (car prev))
;				  (setq one-product (setq prev	(cons new-deg (cons to-add prev)))))
;				 (t
;				  ;;claim this can't happen unless prev = one-product
;				  ;;Since otherwise have had a non-trivial to-add already in the sue
;				  ;;loop and this would have added something in degree new-deg, but back
;				  ;;one step, and prev  would have that new-deg as its first element.
;				  ;;That new-deg must be bigger than the current new-deg.
;				  (iassert (eq prev one-product))
;				  (cond ((pzerop (setq repl (afp-plus (term-cof prev) to-add)))
;					 (setq prev (setq one-product (cddr prev))))
;					(t(setf (second prev) repl))))))
;			  (t ;;each to-add term has lower new-deg than the previous (or to-add=0)
;			   (loop for vvv on  (cddr prev) by #'cddr
;				   do
;			       (cond ((> (term-deg vvv) new-deg) (setq prev vvv))
;				     ((eql (term-deg vvv) new-deg)
;				      (cond ((pzerop (setq repl (afp-plus (term-cof vvv) to-add)))
;					     (setf (cddr prev)(cddr vvv)))
;					    (t (setf (term-cof vvv) repl)
;					       	  (setq prev (cddr prev))))
;				      (return nil))
;				     (t (setf (cddr prev) (cons new-deg (cons to-add vvv)))
;					(setq prev (cddr prev))
;					(return nil)))
;				   finally (setf (cddr prev)(list new-deg to-add))))))
;	       finally (return one-product)))))


(defun test-decrease (terms)
  (loop for (deg cof) on terms by #'cddr
	with d0 = 1000
	when (not (> d0 deg)) do (fsignal 'bad-order)
				 else do (setq d0 deg)))

(defun afp-pctimes (poly number)
  "Its first argument must be a polynomial and second argument a number,~
    and it returns the product"
  (cond ((atom poly)(ctimes poly number))
	(t (term-operation poly number afp-pctimes))))

(defmacro butlastn (n list)
  "knocks off the last n items of a list"
  `(setf ,list  (cond ((< ,n (length ,list))
		       (setf (cdr (lastn (1+ ,n) ,list)) nil) ,list)
		      (t nil))))

(defun test-times (f g &key empty)
  (cond (modulus (iassert (equal (tim (afp-times f g))
				 (remove-zero-coefficients (tim (ptimes f g))))))
	(empty (tim (progn
		      (afp-times f g)
		      nil))
	       (tim (progn
		      (ptimes f g)
		      nil)))
	(t	     (iassert (equal (tim (afp-times f g))
				     (tim (ptimes f g)))))))


(defun remove-zero-coefficients (poly)
  (cond ((numberp poly)poly)
	(t (loop with v = poly
		 while (cdr v)
		 do  (setf (third v) (remove-zero-coefficients (third v)))
		 when (pzerop (third v))
		     do(setf (cdr v) (cdddr v))
		     else
		 do (setq v (cddr v))
		 finally (return  (cond ((null (cdr poly)) 0)
					((zerop (second poly))(third poly))
					(t poly)))))))


(defmacro with-area-used (&rest body)
  `(progn
     (prog1
	 (progn ,@body))))

(defun recursive-ideal-gcd1 (f g )
   "assumes that f and g are polynomials of one variable and that modulus is non trivial
   and that deg f >= deg g   gcd = a*f +b*g , and deg a < deg g, deg b < deg f"
  (cond ((numberp g)(setq g (cmod g))
	 (cond ((zerop g)(values f 1 0))
	       (t (values 1 0  (crecip g)))))
	((not (eql (p-var g) (p-var f)))(merror 'not-function-of-one-variable))
	(t(multiple-value-bind (quot zl-rem creqd)
	      (afp-pseudo-quotient f g)
	    creqd ;;ignore
	    (multiple-value-bind (gcd a b)
		(recursive-ideal-gcd1 g zl-rem)
	      (values gcd b (pdifference a (ptimes quot b))))))))



(defun recursive-ideal-gcd (f g &aux rev? fact)
  "assumes that f and g are polynomials of one variable and that modulus is non trivial
   It returns (gcd a b) such that gcd = a*f +b*g , and deg a < deg g, deg b < deg f where
   gcd is the gcd of f and g in the polynomial ring modulo modulus."
  (cond ((null modulus) (merror "polynomials over the integers are not a PID")))
   (assume-pointerg-or-equal-p f g rev?)
   (cond ((numberp f)(values 1 (crecip f) 0))
	 (t (multiple-value-bind (gcd a b)
		(recursive-ideal-gcd1 f g)
;	       (iassert (zerop (pdifference gcd (pplus  (ptimes f a)  (ptimes g b)))))
;	      (iassert (numberp (afp-quotient gcd (pgcd f g))))
	      (cond ((numberp gcd)
		     (cond ((not(equal gcd 1))(setq fact (crecip gcd)))))
		    (t (cond ((not(equal (p-cof gcd) 1))(setq fact (crecip (p-cof gcd)))))))
	      (cond (fact (setq gcd (ptimes fact gcd))
			  (setq a (ptimes a fact))
			  (setq b (ptimes b fact))))
	      (cond (rev? (values gcd b a))
		    (t (values gcd a b)))))))



(defvar $e_poles nil)

;;;do the following at macsyma level to specify which poles you want:
;;;    e_poles:[e^-2,e^-1]$

(defun $list_poles_of_sum (sum)
  (cond ((numberp sum) '((mlist simp) 0 0))
	(t
  (check-arg sum (and (consp sum)(eql (caar sum) 'mplus)) "macsyma sum")
	 (let ((pol   $e_poles))
	  (cons (car pol)  (loop for u in (cdr pol)
		 collecting
	   (loop for v in (cdr sum)
		 when (equal u v)
		   collecting 1 into cof
		 else
		 when (and (listp v) (member u  (cdr v) :test 'equal))
		  collecting
		    (cond ((eql (length v) 3)
			   (loop for vv in (cdr v )
				   when (not (equal vv u))
				     do (return vv)))
			  (t (loop for vv in v
				   when (not (equal vv u))
				     collecting vv)))
		    into cof
		 finally (return  (cond ((null cof) 0)
			       ((eql (length cof) 1) (car cof))
			       (t (apply 'add* cof)))))))))))

(defun constant-psublis (alist polynomial)
  (setq alist (sort alist #'pointergp :key #'car))
  (constant-psublis1 alist polynomial))

(defun constant-psublis1 (alist polynomial)
  (cond ((numberp polynomial) polynomial)
	((null alist) polynomial)
	(t
	 (block sue
	   (prog
	       (main-var tem)
	      (setq main-var (p-var polynomial))
	      reduce-subs
	      (cond ((and alist (pointergp (caar alist) main-var))
		     (setq alist (cdr alist)) (go reduce-subs)))
	      (cond ((null alist) (return polynomial))
		    ((eql (caar alist) main-var)
		     (loop for (deg cof) on (p-next-term (p-terms polynomial))
			by #'p-next-term
			with repl = (cdr (car alist))
			with answ =  (afp-pctimes
				      (constant-psublis1
				       (cdr alist) (p-cof polynomial))
				      (cexpt repl (p-deg polynomial)))
			do (setq answ
				 (pplus answ
					(cond ((zerop deg)
					       (constant-psublis1 (cdr alist) cof))
					      (t  (afp-pctimes (constant-psublis1
								(cdr alist) cof)
							       (cexpt repl deg))))))
			finally (return-from sue  answ)))
		    (t (return
			 (psimp main-var
				    (loop for (deg cof)
				       on (p-terms polynomial) by #'p-next-term
				       unless (pzerop (setq tem (constant-psublis1 alist cof)))
				       nconc (list deg tem)))))))))))

;;afp-pcsubsty used 1/2 space and was twice as fast as pcsubsty on substituting for one variable y
;;in (x+y+z)^10  (33.5 msec. and 70 msec respect).
;
;(compare-functions
;(defun afp-pcsubsty (vals vars pol)
;  (constant-psublis (subs-for-psublis vars vals) pol))
;(defun reg-pcsubsty (vals vars pol)
;  (pcsubsty vals vars pol)))

(defun my-evenp (n &aux nn)
   (setq nn (ash n -1))
   (and (equal (ash nn 1) n) nn))

;;On symbolics the regular gcd is only 40% of the speed of fast-gcd.
;;and I compared the values on several hundred random
;;m n and it was correct.
;;on (test 896745600000 7890012  1000) of 1000 repeats
;;the fast-gcd was .725 sec and the regualar gcd was 5.6 sec. using 0 and 23000 words resp.
;;On Explorer: Release 1.0)the fast-gcd was 1.9 sec and the regualar gcd was .102 sec. using 0 and 0 words resp.
;(defun test (m n rep &aux a b)
;  (tim (loop for i below rep
;	do (setq a (fast-gcd m n))))
;  (tim (loop for i below rep
;	      do (setq b (\\\\ m n))))
;  (assert (equal a b)))
;;for testing two gcd's give same results.
;(defun test ( rep &aux m n a b)
;  (loop for i below rep
;	do (setq m (* (random (expt2 32))(setq n (random (^ 2 32)))))
;	when (oddp i) do(setq n (- n))
;	do
;	(setq n (* n (random (^ 2 32))))
;	do (setq a (gcd m n))
;	do (setq b (\\\\ m n))
;	(show (list m n a))
;	(assert (equal a b))))


(defun fast-gcd (m n)
  (setq m (abs m) n (abs n))
  (cond ((< n m)nil)
	(t (rotatef m n)))
  (cond ((zerop n) m)
	((fixnump n)
	 (setq m (mod m n))
	 (cond ((zerop m) n)
	       (t (bin-gcd m n))))
	(t (gcd  n (mod m n)))))


(defun bin-gcd (u v &aux (k 0)u2 v2 t2 tt)
  (loop
	do (setq u2 (ash u -1))
	when (not (eql (ash u2 1) u))
	  do (return k)
	do (setq v2 (ash v -1))
	when (not (eql (ash v2 1) v))
	  do (return k)
       do (setq u u2 v v2 k (1+ k)))
  (prog ()
     b2
	(cond ((oddp u) (setq tt (- v)))
	      (t (setq tt (ash u -1))))
     b3b4
	(loop  do (setq t2 (ash tt -1))
	       when  (eql (ash t2 1) tt)
	       do (setq tt t2)
		  else do (return nil))
	(cond ((> tt 0) (setq u tt))
	      (t (setq v (- tt))))
	(setq tt (- u v))
	(cond ((zerop tt)(return (ash u k)))
	      (t (go b3b4)))))

(defun poly-length (poly)
  (cond ((numberp poly ) 0)
	(t (length (cdr poly)))))

(defun poly-to-row (poly &optional row &aux leng)
  (cond (row
	 (cond ((< (array-total-size row) (length poly))
		(setq row (adjust-array row (+ 10 (poly-length poly)) :fill-pointer (fill-pointer row))))))
	(t
	 (setq row (make-array (+ 10 (setq leng (poly-length poly))) :fill-pointer 0 :adjustable t))))
  (cond ((numberp poly)
	 (vector-push 0 row)
	 (vector-push poly row))
	(t
	 (loop for u in (cdr poly) do
	      (vector-push u row))))
  row)

(defun row-to-terms (row)
  (listarray (sort-grouped-array row 2 '>)))

;;seems afp-square is roughly twice as fast on univariate.
;(user:compare-recursive-functions

(defun afp-square (poly)
  (cond ((numberp poly)(ctimes poly poly))
	(t (palgsimp (p-var poly) (afp-terms-square (p-terms poly)) (alg poly)))))

;;comparison for squaring (x+y+z)^10 and (x+y+z)^4 (x+1)^10
;;;done in temp area:
;             afp-square (time space)  pexpt (time space)
;(x+y+z)^10   1450 ms.  25,305           1780ms 32,270
;(x+y+z)^4      80 ms.   1,443            111ms  2,099
;(x+1)^10       82 ms.     527            190ms  2,911

;; (defun test-square (f &key empty)
;;   (cond (modulus
;; 	 (iassert (equal (tim (afp-square f)) (remove-zero-coefficients (tim (pexpt f 2 ))))))
;; 	(empty
;; 	 (tim (progn
;; 		(afp-square f)
;; 		nil))
;; 	       (tim (progn
;; 		      (pexpt f 2)
;; 		      nil)))
;; 	(t
;; 	 (iassert (equal (tim (afp-square f)) (tim (pexpt f 2)))))))

;; (defun test-square (f &key empty)
;;   (cond (modulus
;; 	 (iassert (equal (tim (afp-square f)) (remove-zero-coefficients (tim (ptimes f f))))))
;; 	(empty
;; 	 (tim (progn
;; 		(afp-square f)
;; 		nil))
;; 	       (tim (progn
;; 		      (pexpt f 2)
;; 		      nil)))
;; 	(t
;; 	 (iassert (equal (tim (afp-square f)) (tim (ptimes f f)))))))

;;pexpt is not accurate for modulus=9
;;note example set al:(x+y+z)^4 in polynomial form.
;;then (let ((modulus 9))  (equal (ptimes al al)  (pexpt al 2))) ==> nil
;;but afp-square does work.

(defun afp-terms-square (p-terms)
  (prog (lead orig-p2-terms p2-terms prod-exp prod-cof tail answ)
     begin
	(cond (p-terms (setq answ (afp-square (term-cof p-terms)))
		       (setq orig-p2-terms(setq p2-terms (plain-term-operation (cddr p-terms) 2 afp-pctimes)))
		       (setq answ (plain-term-operation p2-terms (term-cof p-terms) afp-times (term-deg p-terms)))
		       (setq lead (afp-square (term-cof p-terms)))
		       (cond ((pzerop lead) nil)
			     (t (setq answ (cons (* 2 (term-deg p-terms))
						 (cons lead
						       answ)))))
		       (cond ((null answ)
			      (setq p-terms (cddr p-terms))
			      (setq p2-terms (cddr p2-terms))
			      (go begin))
			     (t (go second-leading-square))))
	      (t (return nil)))
     tail-certain ;;add in term to tail
	(cond ((and (cdr tail)(> (second tail) prod-exp))
	       (setq tail (cddr tail))               (go tail-certain)))
	(cond ((or (null (cdr tail))(< (second tail) prod-exp))
	       (setf (cdr tail) (cons prod-exp (cons prod-cof (cdr tail))))
	       (setq tail (cddr tail))                (go next-double-product)))
	(cond ((pzerop (setq prod-cof (pplus (third tail) prod-cof)))
	       (setf (cdr tail) (cdddr tail)))
	      (t (setf (third tail) prod-cof) (setq  tail (cddr tail))))
     next-double-product
	(setq p2-terms (cddr p2-terms))
	(cond ((null p2-terms)(go next-leading-square)))
	(setq prod-cof (afp-times (term-cof p-terms) (term-cof p2-terms)))
	(cond ((pzerop prod-cof) (go next-double-product)))
	(setq prod-exp (+ (term-deg p-terms) (term-deg p2-terms)))
	(go tail-certain)
     next-leading-square
	(setq orig-p2-terms (cddr orig-p2-terms))
     second-leading-square
	(setq p-terms (cddr p-terms))
	(cond ((null p-terms)(return answ)))
	(setq prod-cof (afp-square (term-cof p-terms)))
	(setq p2-terms orig-p2-terms)
	(setq tail (cdr answ))
	(cond ((pzerop prod-cof) (go next-double-product)))
	(setq prod-exp (* 2 (term-deg p-terms)))
	(go tail-certain)))


(defmacro def-test (f1 f2)
  `(defun ,(intern (format nil "~A-~A" '#:test f1)) (&rest rest-args)
     (let (empty (*print-level* 2)(*print-length* 3) ansa ansb)


       (cond ((member ':empty rest-args :test #'eq)
	      (setq rest-args (subseq rest-args 0 (- (length rest-args)
						     (length (member ':empty rest-args :test #'eq)))))
	      (setq empty t)))
       (format t "~%For functions ~A and ~A respectively, ~%with argument list being ~A" ',f1 ',f2 rest-args)
       (cond (empty (format t  "~%All computations done in a temporary area:")))
       (cond ((and (null empty)modulus)
	      (progn
		(iassert (equal (setq ansa(tim (apply ',f1 rest-args)))
				(setq ansb (remove-zero-coefficients (tim (apply ',f2 rest-args ))))))))
	     (empty (tim (progn
			   (apply ',f1 rest-args )
			   nil))
		    (tim (progn
			   (apply ',f2 rest-args)
			   nil)))
	     (t (progn
		  (iassert (equal (setq ansa (tim (apply ',f1 rest-args )))
				  (setq ansb(tim (apply ',f2 rest-args)))))))))))

;;timings for afp-expt and pexpt respectively with 5 th power
;(x+y+z)^4
;For functions AFP-EXPT and PEXPT respectively,
;with argument list (POLY EXPONENT) being ((Z 4 1 ...) 5)
;All computations done in a temporary area:
;2223.207 msec. at priority 1
;Reclaiming 39,793 in polynomial space (total 4,176,070).14:20:53
;3029.429 msec. at priority 1
;Reclaiming 50,579 in polynomial space (total 4,226,649).14:20:56"
;
;(x+1)^10
;For functions AFP-EXPT and PEXPT respectively,
;with argument list (POLY EXPONENT) being ((X 10 1 ...) 10)
;All computations done in a temporary area:
;1972.551 msec. at priority 1
;Reclaiming 17,220 in polynomial space (total 3,825,741).14:19:00
;3312.806 msec. at priority 1
;Reclaiming 28,634 in polynomial space (total 3,854,375).14:19:03"

;;note ( pexpt (x+y+z)^4 5) yields false result.. if modulus is 4.
;;I checked that afp-expt was ok, by comparing to  the following
;;simple exponent function.
;(def-test afp-expt pexpt-simple)
;(defun pexpt-simple (u n)
;		       (loop for i from 1 to n
;			     with answ = 1
;			     do (setq answ (ptimes answ u))
;				finally (return answ)))
(defun afp-expt-modulo (poly exponent &rest work-modulo)
  (working-modulo work-modulo
    (afp-expt poly exponent)))

(defun afp-expt (poly exponent)
  "Raises the polynomial POLY to EXPONENT.  It never performs more
  than a squaring before simplifying, so that if modulus or tellrat are
  in effect, it will still be reasonable."
  (cond ((eql exponent 1) poly)
	((eql exponent 0) poly)
	((< exponent 0) (merror "Use positive exponents"))
	(t (cond ((numberp poly)(cexpt poly exponent))
		 ((or (cdddr poly) (alg poly))
		  ;;main case
		  (loop for i from 0
			with  2^i-power-poly =  poly
			with answer = 1
			do
		    (cond ((oddp exponent)
			   (cond ((eq answer 1)
				  (setq answer  2^i-power-poly))
				 (t
			   (setq answer (afp-times answer 2^i-power-poly))))))

		    (setq exponent (ash exponent -1))
		    (cond ((zerop exponent)(return answer)))
		    (setq 2^i-power-poly (afp-square 2^i-power-poly))))
		 (t ;;monomial of variable not tellrated
		  (let ((pow (afp-expt (p-cof poly) exponent)))
		    (cond ((pzerop pow) 0)
			  (t
			   (psimp (p-var poly)
				     (list (* (p-deg poly) exponent)
				      pow))))))))))
(defmacro push-poly-number-deg-cof (rows poly-number deg cof &aux (row (make-symbol "row")))
  `(let ((,row (aref ,rows ,deg)))
     (vector-push-extend  ,poly-number ,row)
     (vector-push-extend  ,cof ,row)))

  ;;want to find sol'ns of v^p-v=0 (mod u)
(defun berlekamp-set-up-and-reduce-matrix ( u p &aux rows powers estimated-size  sp)
  (setq rows (make-array (p-deg u) :fill-pointer (p-deg u)))
  (let ((modulus p)(tellratlist (list u)))
    (working-modulo (list u)
		    (setq powers
			  (loop for i from 0
			     with pol = (afp-expt (list (p-var u) 1 1) p)
			     with pow = 1 with belo =  (1- (p-deg u))
			     collecting pow
			     while (< i belo)
			     when (eql pow 1)
			     do (setq pow pol)
			     else do (setq pow (afp-times pol pow)))))
    (loop for i below (max 5 (length powers))
       for vv in powers
       summing (or (and (atom vv) 0) (length vv)) into count
       finally (setq estimated-size (+ 10 (quotient count 5))))
    (loop for i below (fill-pointer rows)
       do (setf (aref rows i) (make-array estimated-size :fill-pointer 0 :adjustable t)))
    ;;putting the entries in the sparse matrix.  Each polynomial is a column.
    (loop for vv  in (cdr powers)
       for i from 1
       when (numberp vv)
       do
	 (push-poly-number-deg-cof rows i 0 vv)
	 (push-poly-number-deg-cof rows i i -1)
       else
       do
	 (loop for (deg cof) on (p-terms vv) by #'p-next-term
	    with subtracted-it
	    when (eql i deg)
	    do
	      (setq cof (- cof 1))
	      (setq subtracted-it t)
	    do
	      (cond ((not (zerop cof))
		     (push-poly-number-deg-cof rows i deg cof)))
	    finally (cond ((null subtracted-it)
			   (push-poly-number-deg-cof rows i i -1))))
	 )
    (setq sp (make-sparse-matrix :rows rows
				 :type-of-entries p
				 :columns-used-to-pivot (make-hash-table :test 'equal)))
    (sp-set-rows sp rows (loop for i from 1 below (p-deg u) collecting i))
    (sp-reduce sp)
    sp))

(defun berlekamp-polynomial-solutions (sp polynomial &aux sp-sols)
   (sp-solve sp )
    (setq sp-sols (sp-solutions sp))
    (loop for i below (row-length (sp-rows sp-sols))
	  collecting (psimp (p-var polynomial)(listarray (sort-grouped-array (sp-row sp-sols i) 2 '>)))))


(defun berlekamp-get-factors-little-prime (u reduced-sparse-matrix prime &aux number-of-factors b-polys poly tem
					   			      (factor-list (list u)))
  (setq number-of-factors   (- (p-deg u) (sp-number-of-pivots reduced-sparse-matrix)  ))
  (show number-of-factors)
  (sp-show-matrix reduced-sparse-matrix)
  (cond ((eql 1 number-of-factors) ;;irreducible
	 factor-list)
	(t (setq b-polys (berlekamp-polynomial-solutions
			   reduced-sparse-matrix u))
	   (loop named sue
		 for v in b-polys
		 with half-p = (ash prime -1)
		 do
	     (loop for j from (- half-p) to half-p
		   do
	       (loop for u-fact in factor-list
		     do
	       ;;make more efficient.
	       (setq poly (pplus v j))
	       (setq tem   (afp-gcd poly  u-fact))
		   when (not (or (numberp tem)
				     (eql (p-deg tem) (p-deg u-fact))))
		     do (setq tem (afp-make-monic tem))
			(setq factor-list (cons tem
						(cons (afp-quotient u-fact
								    tem)
						      (delete u-fact factor-list :test #'equal))))
			when (eql (length factor-list) number-of-factors)
			  do (return-from sue factor-list)))))))

(defun berlekamp-get-factors-big-prime (u reduced-sparse-matrix prime &aux number-of-factors b-polys
			      (factor-list (list u)))
  (setq number-of-factors   (- (p-deg u) (sp-number-of-pivots reduced-sparse-matrix)  ))
  	(show number-of-factors)

  (cond ((eql 1 number-of-factors) ;;irreducible
	 factor-list)
	(t (setq b-polys (berlekamp-polynomial-solutions
			   reduced-sparse-matrix u))
	   (prog (half-p  tem pow v)
		 (setq half-p (ash prime -1))
	      added-factor
		 (cond ((>= (length factor-list) number-of-factors)(return factor-list)))
		 (setq v (loop for vv in (cdr b-polys)
			       with answ = (afp-pctimes (car b-polys) (random (max 500 prime)))
			       do (setq answ (pplus answ (afp-pctimes vv (random prime))))
			       finally (return answ)))
		 (working-modulo
		   (list u)
		   (setq pow (pdifference (afp-expt v half-p) 1)))
		 (setq factor-list
		       (loop for w in factor-list
		       do (setq tem (afp-make-monic  (afp-gcd pow w)))
		       when (not (or (numberp tem)
				     (eql (p-deg tem) (p-deg w))))
			 collecting tem
		       and
		       collecting (afp-quotient w tem)
		       else collecting w))
		 (go added-factor)))))

(defun afp-make-monic (poly)
  (cond ((numberp poly) 1)
	((not (or modulus (eql (abs (p-cof poly)) 1)))
	 (merror "not finite modulus or unit coefficient"))
	(t (let ((inv (crecip (p-cof poly ))))
	      (afp-pctimes poly inv)))))



(defun afp-try-make-monic (poly)
  (cond ((numberp poly) 1)
	((eql (p-cof poly) 1) poly)
	((eql (p-cof poly) -1)
	 (pminus poly))
	(modulus
	   (cond ((numberp (p-cof poly))
		  (afp-pctimes poly (crecip (p-cof poly))))
		 (t poly)))
	(t poly)))

(defun afp-berlekamp-factor (pol)
  (afp-berlekamp-factor1 pol modulus :use-little (<= modulus 13)))

(defun afp-berlekamp-factor1 (pol p &key use-little use-big &aux sp (modulus p) answ)
  (setq sp  (berlekamp-set-up-and-reduce-matrix pol p) )
 (cond ((and (null use-big)(or use-little (< p 13))) (setq answ  (berlekamp-get-factors-little-prime pol sp p)))
       (t  (setq answ  (berlekamp-get-factors-big-prime pol sp p))))
 (loop for v in answ
       with ans = 1
       do (setq ans (ptimes ans v))
      finally (show ans) (iassert (equal (afp-mod pol) ans)))
 (show answ)
 (mapcar #'afp-mod answ))

(defvar *mod-p-factor* 'afp-berlekamp-factor)
;(defvar *mod-p-factor* 'afp-distinct-degree-factor)

(defvar *sort-factors* t)

(defun afp-factor (pol &key square-free-arg &aux factor-function facts vars    lead  answ)
  (cond (modulus  (setq lead (p-cof pol))
		  (setq pol (afp-try-make-monic pol))))
  (cond (square-free-arg (setq facts (list pol 1)))
	(t(setq facts (afp-square-free-factorization pol))))
  (setq vars (list-variables pol))
  (setq answ (cond (modulus
		    (cond ((cdr vars) (fsignal 'not-univariate))
			  (t (setq factor-function  *mod-p-factor* )
			     (setq answ
				   (loop for (pol deg)  on facts by #'cddr
					 with const = 1
					 when (consp pol)
					   appending
					     (loop for fa in (funcall factor-function pol)
						   collecting fa
						   collecting deg) into all
					 else do (setq const  (ctimes const pol))
						 (iassert (eql deg 1))
					 finally (setq const (* lead const))
						 (return(cond ((not (eql const 1))
							(cons const (cons 1 all)))
						       (t all))))))))
		   (t (fsignal 'not-yet-without-modulus))))
  (cond (*sort-factors* (sort-grouped-list answ 2 'alphagreatp))
	(t answ)))

(defun afp-mod (pol)
  (afp-pctimes pol 1))

(defun get-factors (u use-for-gcd p &aux tem)
  (let ((modulus p))
    (loop for w in use-for-gcd
	  appending
	    (loop for i below p
		  when (not (numberp (setq tem (afp-gcd (pplus w i) u))))
		    do (show i) and
		  collecting (monize tem)))))


(defun afp-distinct-degree-factor (u &optional ( prime modulus) &aux (v u )(ww (list (p-var u) 1 1)) (d 0) gd
				   mon (modulus prime) answer)
  "U should be univariate and square free.  Calculations are done modulo PRIME.  It
  returns an alternating list of factors"
  (setq mon ww)
  (setq answer
	(loop do (cond ((numberp v) (return answ))
		       ((> (* 2 (1+ d)) (p-deg v))(return (cons v answ))))
	     (incf d)
	     (working-modulo (list v) (setq ww(afp-expt ww prime)))
	     (setq gd (afp-gcd (pdifference ww mon) v))
	     (cond ((and (consp gd) (> d (p-deg gd)))(fsignal 'big)))
	   when (not (numberp gd))
	   do
	     (setq v (afp-quotient v gd))
	     (and (consp v) (consp ww) (setq ww (palgsimp (p-var v) (cdr ww) (cdr v))))
	   when (not (numberp gd))
	   appending (one-degree-factors (afp-try-make-monic gd) d prime) into answ))
  (iassert (eql (p-deg  u)
		(loop for v in answer
		       summing (p-deg v) )))
  answer)

#+debug
(defun tel (u n p &aux (modulus p))
  (check-arg p (oddp p) "odd")
    (iassert (eql (p-deg  u)
		(loop for v in answ
		 summing (p-deg v) )))

  (eql (p-deg u) n))
(defun one-degree-factors (u deg p &aux pow tt tem facts used-tt answ (modulus p))
  "assumes u has its irreducible factors of degree DEG and works modulo P.  U should
   be univariate and square free.  The result is thus just a list of the factors (powers
    are unnecessary"
  (check-arg p (oddp p) "odd")
  (cond((eql (p-deg u) deg)(setq answ (list u )))
       (t (setq pow (quotient  (- (expt  p deg) 1) 2))
	  (setq facts (list u))
	  (loop named sue for i from 1
		do
	    (loop  do (setq tt (generate-t-for-one-degree-factors u deg p i))
		   unless (or  (numberp tt) (member tt used-tt :test #'equal))
		     do (push tt used-tt) (return tt))
	    (working-modulo (list u)
	      (setq tt  (afp-expt tt pow) ))
	    (loop for v in facts
		  do
	      (setq  tem (afp-gcd v (pplus tt 1)))
	      (cond ((not (or (numberp tem) (eql (p-deg tem) (p-deg v))))
		     (cond ((eql (p-deg tem) deg)(push (afp-make-monic tem) answ))
			   (t (push tem facts)))
		     (cond ((eql (p-deg (setq tem (afp-quotient v tem))) deg)
			    (push (afp-make-monic tem) answ))
;			   ((numberp tem))
			   (t (push tem facts)))
		     (setq facts (delete v facts :test #'equal))))
		  when (null facts) do (return-from sue answ)))))
  (cond ((member 1 answ) (fsignal 'bad)))
  (iassert (eql (p-deg  u)
		(loop for pol  in answ
		      when (and (consp pol))
		 summing  (p-deg pol) )))
   answ)

(defun generate-t-for-one-degree-factors (u deg p i &aux tem)
  (cond ((< i 5)
	 (list (p-var u) 1 1 0 (random p)))
	((psimp (p-var u) (loop for j downfrom (setq tem (+ 1 (random (- (* 2 deg) 1)))) to 0
				   ;;make semi sparse
			       	       when (evenp (random 2))
				   collecting j and collecting (1+ (random (1- p))))))))

(defun ff (u &optional ( prime modulus) &aux fact1 facts)
  (let ((modulus prime))
    (setq facts (afp-square-free-factorization u))
    (sort-grouped-list
     (loop for (pol pow) on facts by #'cddr
	do (show pol pow)
	  (setq fact1 (afp-distinct-degree-factor pol prime))
	appending
	  (loop for ww in fact1
		 collecting ww collecting pow))
     2
     'alphagreatp)))

(defun nnpfactor (pol)
  (sort-grouped-list (npfactor pol) 2 'alphagreatp))

(def-test nnpfactor afp-factor)

;(setq w2 (st-rat  #$x^2+91*x+11$))
;(setq v2 (st-rat  #$x^2+19*x+11$))
;(setq v1 (let ((modulus 7)) (afp-mod v2)))
;(setq w1 (let ((modulus 7)) (afp-mod w2)))
;(setq prod (ptimes v2 w2))

(defun hensel-lift (product v w prime up-to-size &aux a b gcd
			    (facts (list v w)))
  "Lifts v and w which satisfy product=v*w mod(prime) to a list FACTS = (uu vv)
   satisfying product = uu*vv mod (up-to-size).  Product, v, and w are assumed to
   have leading coefficient 1"
;  (declare (values fact-pair power))
  (let ((modulus prime)) (multiple-value-setq (gcd a b)
					      (recursive-ideal-gcd v w))
       (cond ((not (numberp gcd))(fsignal "must have gcd of factors a unit")))
       (check-arg v (eql 1 (afp-mod (p-cof v))) "monic"))
  (loop	with power = 1
	do (setq power (* power prime))
	while (< power up-to-size)
	do
    (setq v (car facts) ) (setq w (second facts))
			  (setq facts (hensel-lift1 product v w power prime a b))
	finally (return (values facts power))))


(defun gen-afp-times (&rest lis)
  (setq lis (delete 1 (copy-list lis) :test #'equal))
  (cond ((null lis) 1)
	((null (cdr lis))(car lis))
	(t (afp-times (car lis) (cond ((cddr lis)
				       (apply 'gen-afp-times1 (cdr lis)))
				      (t (second lis)))))))

(defun gen-afp-times1 (&rest lis)
  (cond ((null lis) 1)
	((null (cdr lis))(car lis))
	(t (afp-times (car lis) (cond ((cddr lis)
				       (apply 'gen-afp-times (cdr lis)))
				      (t (second lis)))))))

(defun smallest-power-bigger (p up-to)
  (loop	with pow = p
	 while (< pow up-to)
	do (setq pow (* p pow))
	finally (return pow)))

(defun hensel-lift-list (product factor-list prime up-to-size &aux lift )
  (cond ((eql (length factor-list)1 ) (list product))
	((eql (length factor-list) 2) (hensel-lift product (first factor-list)
						   (second factor-list) prime up-to-size))
	(t  (setq lift (hensel-lift product (first factor-list) (let((modulus prime))(apply 'gen-afp-times (cdr factor-list)))
				    prime up-to-size))
	    (cons (car lift) (hensel-lift-list (second lift) (cdr factor-list) prime up-to-size)))))

(defun hensel-lift1 (product ve we prev-modulus prime  &optional a b &aux dif h kk   quot zl-rem creqd new-modulus gcd)
  "lifts u=ve*we mod (p^e) to u=ve+1*we+1 mod (p^e+1) with ve=ve+1 and we=we+1 mod (p^e+1)
  and deg(ve+1)<=deg(ve) deg(we+1)<=deg(we)  and returns the list of  ve+1 and we+1"
;  (declare (values (list ve+1 we+1)))
  (setq new-modulus   (* prime prev-modulus))
  (let ((modulus new-modulus)) (setq dif (pdifference product (ptimes ve we))))
  (cond ((pzerop dif)(list ve we))
	(t
  (let ((modulus prime) )
    (cond ((null a)
	   (multiple-value-setq (gcd a b)
	     (recursive-ideal-gcd ve we)))))
  (let ((modulus new-modulus))
    (setq h (ptimes b dif))
    (setq kk (ptimes a dif))
    (let ((modulus new-modulus))
      (multiple-value-setq ( quot zl-rem creqd)
			   (afp-pseudo-quotient h ve)))
    (setq h zl-rem)
    (setq kk (pplus kk (ptimes quot we)))
    (list (pplus ve h) (pplus we kk))))))
   ;(let ((modulus (expt  prime (1+ e)))) (values (pplus ve h) (pplus we kk))))


(defun collect-number-factors (fact-list &aux answ)
  "Makes sure there are no factors with leading cof = -1 and collects all constants together
   and puts them first"
  (loop for (pol deg) on fact-list by #'cddr
	with constant = 1
	do
	(cond ((numberp pol)
		(setq constant (ctimes constant (cexpt pol deg))))
	      ((eql (p-cof pol) -1)
	       (setq constant (ctimes constant (cexpt (p-cof pol) deg)))
	       (setq pol (pminus pol))
	       (setq answ (nconc answ (list pol deg))))
	      (t(setq answ (nconc answ (list pol deg)))))
	finally (return
		  (cond ((eql 1 constant) answ)
			(t (cons constant (cons 1 answ)))))))

(defun integer-univariate-factor (poly &aux facts)
  "returns an alternating list of irreducible polynomials and their powers in the factorization over the integers
   there will be at most one factor which is a number and it will be to the first power.  The other irreducible
   polynomials will be relatively prime"
  (setq facts (afp-square-free-factorization poly))
  (show facts)
  (collect-number-factors (loop for (pol deg) on facts by #'cddr
				appending (loop for pol1 in  (integer-univariate-factor1 pol)
					       collecting pol1 collecting deg))))

;;(defvar *small-primes* '(3 5 7 11 13 17 19)) ; overrides *small-primes* in src/ifactor.lisp; not used

(defun find-small-prime-so-square-free (poly &optional (variable (p-var poly)) (prime-start 13)&aux deriv cof the-gcd)
  "finds a small prime P so that the roots of poly with respect to variable VARIABLE are distinct and so
    that poly has the same degree reduced modulo P.  It will continue for ever if the input is not square free over
    the integers."
   (setq deriv (pderivative poly variable))
  (setq cof (pcoeff poly (list variable (pdegree poly variable) 1)))
  (cond ( (eql *mod-p-factor* 'afp-distinct-degree-factor)
	(setq prime-start (max prime-start 3))))
  (loop for p from prime-start
	when  (> (- p prime-start) 500) do (format t "~%I hope you have given me a square free polynomial!!")
	when (and (q-primep p)
		  (let ((modulus p))(and (not (pzerop (afp-mod cof)))
					 (or (numberp (setq the-gcd (afp-gcd (afp-mod poly) (afp-mod deriv))))
					     (not (member variable (list-variables the-gcd) :test #'eq))))))
	  do (return p)))
(defun integer-univariate-factor1 (pol &aux p ( up-to 1000) facts lifts mod)
  "Argument pol is square free"
  (setq p (find-small-prime-so-square-free pol ))
  (let ((modulus p)) (setq facts (afp-factor pol :square-free-arg t)))
  (setq mod  (smallest-power-bigger p up-to))
  (setq facts (loop for (pol deg) on facts by #'cddr
		    when (not (eql deg 1)) do (fsignal "bad-degree-for-square-free")
		    collecting pol))
  (setq lifts (hensel-lift-list pol facts p up-to))
  (correct-factors pol lifts p mod))

(defun sub-lists (leng list)
  "Returns list of ordered sublists of LIST of length LENG"
  (cond ((zerop leng) nil)
	((eql 1 leng)(mapcar 'list list))
	(t
	 (loop for v on list
	       appending (loop for w in (sub-lists (1- leng) (cdr v))
			       collecting (cons (car v) w))))))

(defun correct-factors (pol factors p mod &aux tried  d answ quot prod)
  "Given the factors FACTORS of polynomial POL modulo MOD, we determine
   what the factors are over the integers.  It is assumed that MOD is sufficiently
   large so that any real factors of POL lie in the range -MOD/2 to MOD/2.  Also
   POL is assumed square free so that the factors are listed without their multiplicities"
  p
  (prog ()
	(setq d 1)
     look-for-factors
	(cond ((> (* d 2) (length factors))
	       (push pol answ) (return answ)))
	(loop for v in (sub-lists d factors)
	      when (member v tried :test #'equal)
		nconc nil
	      else
		do (push v tried)
		   (let ((modulus mod)) (setq prod (apply 'gen-ptimes v)))
;		   (setq prod (gen-afp-times (p-cof pol) prod))
		   (cond ((setq quot (afp-test-divide pol prod))
			  (setq pol quot)
			  (push prod answ)
			  (loop for vv in v
				do (setq factors (delete vv factors :count 1 :test #'equal)))
			  (go look-for-factors)))
	      finally (incf d)
		      (go look-for-factors))))


(defun q-primep (i &aux lis)
  (cond ((car (member i (setq lis '(2 3 5 7 11 13)))))
	((evenp i) nil)
	((loop for v in (cdr lis)
			 when  (zerop (mod i v))
			   do (return t))
	 nil)
	(t (primep i))))


(defun test-integer-factor (pol &aux facts)
  (setq facts (integer-univariate-factor pol))
  (iassert (equal pol (apply 'exponent-product facts)))
  facts)

(defun subs-translate-sublis ( point &optional inv)
  (cond (inv  (loop for v in point
		    when (not (pzerop (cdr v)))
		    collecting (cons (car v)
				     (pplus (list (car v) 1 1) (cdr v)))))
	(t
	 (loop for v in point
	       when (not (pzerop (cdr v)))
	       collecting (cons (car v)
				(pdifference (list (car v) 1 1) (cdr v)))))))

;;change to a defremember. and then clear it when starting new problem.
;;or alternately don't really need to clear since the modulus is stored.
(defun express-x^i (f g k &optional (modulus modulus) &aux a b  zl-REM quot gc mon case0 cre)
  "f and g should be univariate and the leading coefficient of g invertible modulo modulus.
   A list of two coefficients (a b) are returned such that x^i= a*f + b*g and deg(a)< deg (g)"
  (check-arg modulus (not (null modulus)) "non-trivial")
  (cond ((eql k 0) (multiple-value-setq (gc a b)
		       (recursive-ideal-gcd f g))
	 (list a b))
	(t  (setq case0 (express-x^i f g 0))
	    (setq a (ptimes (car case0) (setq mon (list (car f) k 1))))
	    (setq b (ptimes (second case0) mon))
	    (multiple-value-setq (quot zl-rem cre) (afp-pseudo-quotient a g))
	    (setq a zl-rem)
	    (setq b (pplus b
			   (ptimes quot f)))
	    ;;(iassert (equal mon (afp-plus (afp-times f a) (afp-times g b))))
	    (list a b))))

;(setq point (rerat '((z . 0) (y . 0) )))
;(setq u (st-rat #$zzx^4+zzx^3*(3-z)+zzx^2*((y-3)*z-y^2-13)
;		+zzx*((y^2+3*y+15)*z +6)
;		+(-y^3-15*y)*z-2*y^2-30$))
;(setq f (st-rat #$ (zzx^2+2)$))
;(setq g (st-rat #$zzx^2+3*zzx-15$))
;;the wr-lift worked for the above data

;;speed hacks: The following has two areas which can be improved
;; 1.  should not truncate the polynomial, should multiply with truncation.
;; 2.  should make express-x^i a defremember.
(defun wr-lift (u fi gi up-to-k big-mod  point
		&aux (modulus big-mod) (main-var (p-var u)) tem fi+1 gi+1 v f0 g0 varl w aw bw)
  (setq v (psublis (subs-translate-sublis point) 1  u))
  (setq varl (delete (car u) (list-variables u) :test #'equal))
  (setq f0 fi)
  (setq g0 gi)
  (loop for i from 1 to up-to-k
     do
       (setq w (pdifference  (afp-times fi gi) u))
       (setq w (afp-truncate-powers w varl i))
       (mshow  w)
       (cond ((pzerop w) 'nothing-to-do)
	     (t
	      (cond ((or (numberp w) (not (eql (p-var w) main-var)))
		     (setq aw (afp-times w (first (setq tem (express-x^i f0 g0 0)))))
		     (setq aw (afp-times w (second tem))))
		    (t
		     (loop for (deg cof) on (cdr w) by #'cddr
			initially  (setq aw 0 bw 0)
			do
			  (setq aw (afp-plus aw (afp-times (first (setq tem (express-x^i f0 g0 deg))) cof)))
			  (setq bw (afp-plus bw (afp-times (second tem) cof))))

		     (setq fi+1 (pdifference fi bw))
		     (setq gi+1 (pdifference gi aw))

		     (setq fi fi+1 gi gi+1)
		     (mshow aw bw fi gi)))))
     finally
       (show (pdifference v (ptimes fi gi)))
       (return (list fi gi))))

(defun afp-truncate-powers (pol varl deg)
  (setq varl  (sort varl 'pointergp))
  ( afp-truncate-powers1 pol varl deg))

(defun afp-truncate-powers1 (pol varl above-deg &aux new-cof tem)
  (cond ((< above-deg 0) 0)
	((numberp pol) pol)
	((null varl) pol)
	((member (p-var pol) varl :test #'eq)
	 (psimp (p-var pol)
		(loop for (exp cof) on (cdr pol) by #'cddr
		      do (setq tem (- above-deg exp))
		      when (< tem 0)
			nconc nil
		      else
			do (setq new-cof (afp-truncate-powers1 cof (cdr varl) tem))
			and when (not(pzerop new-cof))
			      nconc (list exp new-cof))))
	(t(psimp (p-var pol)
		 (loop for (exp cof) on (cdr pol) by #'cddr
		       when(not (pzerop (setq new-cof (afp-truncate-powers1 cof varl above-deg))))
			 nconc (list exp new-cof))))))



(defun normalize-factor-list (factor-list &aux tot-deg)
  "checks factor-list and eliminates repeats.  All integer factors are grouped at the beginning if any"
  (let (numerical-factor(facts (collect-number-factors factor-list)))
    (cond ((numberp (car facts))
	   (setq numerical-factor (subseq facts 0 2))
	   (setq facts (cddr facts))))
    (nconc numerical-factor
	   (loop for (pol deg) on facts by #'cddr
		 for rest-facts on facts by #'cddr
		 do (show facts deg)
		 when deg
		 do (setq tot-deg
			  (+ deg
			     (loop for v on (cddr rest-facts) by #'cddr
				   when (and (second v) (equal pol (car v)))
				   summing (second v)
				   and do (setf (second v) nil))))
		 and
		 collecting pol and collecting tot-deg))))

;; (defmacro while (test &body body)
;;   `(loop (cond ((null ,test) (return)))
;; 	 ,@ body))

(defun find-factor-list-given-irreducible-factors (pol fact-list &aux deg divisor answ final-answ)
  (while (setq divisor (car fact-list))
    (setq deg 1)
    (while (setq answ (test-divide pol divisor))
      (setq pol answ)
      (incf deg))
    (setq final-answ (cons deg final-answ))
    (setq final-answ (cons pol final-answ)))
  (unless (equal final-answ 1)
    (fsignal "bad factorization"))
  final-answ)

;(defun afp-multi-factor (pol)
;  (let ((content (afp-content pol)))
;    (cond (content (normalize-factorlist
;		     (append (afp-multi-factor content)
;			     (afp-multi-factor (afp-quotient pol content )))))
;	  (t (afp-)))))

(defun try-multi-factor0 (pol &aux facs fac deg)
  "input may be non square free"
  (setq facs (afp-square-free-factorization pol))
  (prog (result)
    next-factor
      (setq fac (car facs) deg (second facs) facs (cddr facs))
      (prog
	((newfacs (try-multi-factor1 fac)))
	next-factor
	   (push (* (second newfacs) deg) result)
	   (push (car newfacs) result)
	   (setq newfacs (cddr newfacs))
	   (and newfacs (go next-factor)))
      (and facs (go next-factor))
      (return result)))


(defun afp-degvector (pol vars &aux (result (make-list (length vars))))
  (do  ((v vars (cdr v))
	(w result (cdr result)))
       ((null v) result)
    (setf (car w) (pdegree pol (car v)))))

(defun afp-smallest-var (degvector list-variables &aux (best (expt  2 20)) best-variable )
  (do ((v degvector (cdr v))
       (w list-variables (cdr w)))
      ((null v) best-variable)
    (cond ((< (car v) best)
	   (setq best-variable (car w))))))

;;description of eez algorithm with enhancements:
;;
;1) make U squarefree and content 1
;2) make main variable be smallest degree variable
;3) factor leading coefficient into F1,f2,..fk,fk+1 (fk+1 = content)
;4) find a point in x2,...,xn space such that
;there exist p1,..,pk+1, pi|fj iff i = j
;and
;such that we have the right number of factors of U mod (point, B) (B big bound).
;5) Match up pi, and the univariate factors.
;6)

;(defun try-multi-factor1 (pol)
;  "Pol is square free multivariate"
;  (let* ((list-variables (list-variables pol))
;	 (degvector (afp-degvector pol list-variables))
;	 (best-variable (afp-smallest-var degvector list-variables))
;	 (leading-cof (pcoeff pol best-variable))
;	 (point (delq best-variable (copy-list list-variables))))
;    (do ((v point (cdr v)))
;	(null v)
;      (setf (car v (cons (car v) (nth (random 5) *small-primes*)))))
;    (let* ((lead-cof-at-point (psublis point 1 leading-cof)))
;      (cond ((eql 0
