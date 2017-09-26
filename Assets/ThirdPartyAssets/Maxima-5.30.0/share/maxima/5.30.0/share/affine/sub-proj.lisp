;;; -*- mode: lisp; package: cl-maxima; syntax: common-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defun rational-subst (poly var denom )
  "does a substitution like var --> var/denom  where var is a genvar
and denom is a poly.  It calculates the numerator after bringing a common
denominator of denom^(pdegree poly var)"
   (let ((deg  (pdegree poly var)))
     (setq poly (copy-tree poly))
     (sub-rat2 poly var denom deg)))

(cond ((not (fboundp 'p-var)) (load "msm:ratmac.fasl")))

(defmacro with-main-variable (mvar &body body)
  `(let ((,mvar (make-symbol "zzzzzz")))
     (setf (symbol-value ,mvar) 40000)
     (setf (get ,mvar 'disrep) 'zzzzzzz)
     ,@ body))

(defmacro with-main  (main form-to-reorder resubstitute &body body)
  (cond ((null resubstitute)
	 `(let ((newvar (make-symbol "zzzzzz"))
		(oldmain ,main))
	    (setf (symbol-value newvar) 40000)
	    (setq ,main newvar)
	    (setf (get newvar 'disrep) 'zzzzzzz)
	    (setf ,form-to-reorder (psublis (list (cons oldmain (list newvar 1 1))) 1
					     ,form-to-reorder))
	    (progn ,@ body)))
	(t
	 `(let ((newvar (make-symbol "zzzzzz"))
		(oldmain ,main))
	    (setf (symbol-value newvar) 40000)
	    (setq ,main newvar)
	    (setf (get newvar 'disrep) 'zzzzzzz)
	    (setf ,form-to-reorder
		  (psublis (list  (cons oldmain (list newvar 1 1))) 1
			    ,form-to-reorder))
	    (psublis  (list  (cons newvar (list oldmain 1 1))) 1
		      (progn ,@ body))))))


(defmacro sum-over-polynomial (deg cof poly summand)
  `(do ((-tail- (cdr ,poly) (cddr -tail-))
	(-answer- 0)(,deg)(,cof))
       ((null -tail-) -answer-)
    (setq ,cof (cadr -tail-))
    (setq ,deg (car -tail-))
    (setq -answer- (pplus -answer- ,summand))))
(defmacro replace-polynomial-coefficients (deg cof poly replacement)
  `(do ((-tail- (cdr ,poly) (cddr -tail-))
       (,deg)(,cof))
      ((null -tail-) ,poly)
    (setq ,cof (cadr -tail-))
    (setq ,deg (car -tail-))
    (setf (cadr -tail-) ,replacement)))

(defun $degree (form var)
  (setq form (st-rat form))
  (pdegree  form (car (st-rat var))))

(defun sub-rat2 (poly var denom deg)
  (cond ((atom poly) (setq poly (ptimes poly (pexpt denom deg))))
	((eq (car poly) var)
	 (cond ((pointergp var (car denom))
		(replace-polynomial-coefficients
		  degree cof poly
		  (ptimes cof
			  (pexpt denom (- deg degree)))))
	       (t
		(setq poly
		(sum-over-polynomial degree cof poly
		  (ptimes (list (car poly) degree 1)
			  (ptimes cof (pexpt denom (- deg degree)))))))))
	((pointergp (car poly) var)
	 (cond ((pointergp (car poly) (car denom))
		(print 'a)
		(replace-polynomial-coefficients
		  degree cof poly
		  (sub-rat2 cof var denom deg)))
	       (t (print 'b)
		(setq poly
		(sum-over-polynomial
		  degree cof poly
		  (ptimes (list (car poly) degree 1)
			  (sub-rat2 cof var denom deg)))))))
	(t(setq poly (ptimes poly (pexpt denom deg)))))
  poly)

(defun $sub1 (poly var denom &optional deg)
  deg
  (setq poly (st-rat poly))
  (setq var (car (st-rat var)))
  (setq denom (st-rat denom))
  (header-poly (rational-subst poly var denom )))


(defun poly-subst (poly var repl)
  "This substitutes for var (a genvar) the repl (a poly) staying in cre form"
;;  (cond   (not (and (symbolp var)   ;error check
  (poly-subst1 (copy-tree poly) var repl))


(defun poly-subst1 (poly var repl)
	(cond ((atom poly) poly)
	      ((eq (car poly) var)
	       (do ((tail (cdr poly)(cddr tail))
		    (answer 0))
		   ((null tail) (setq poly answer))
		 (setq answer (pplus
			       (ptimes (pexpt repl(car tail))
				       (cadr tail))
			       answer))))
	      ((pointergp (car poly) (car repl))
	       (do ((tail (cdr poly)(cddr tail)))
		   ((null tail))
		 (setf (cadr tail)
		       (poly-subst (cadr tail) var repl))))
	      (t (cond
		  ((eq 0 (pdegree poly var)) nil)
		  (t
		   (do ((tail (cdr poly)(cddr tail))
			(answer 0))
		       ((null tail) (setq poly answer))
		     (setq answer
			   (pplus answer
				  (ptimes
				   (list (car poly) (car tail) 1)

				   (poly-subst (cadr tail) var repl)))))))))
poly)

(defun $polysub (poly var repl)
  (setq poly (st-rat poly))
  (setq var (car (st-rat var)))
  (setq repl (st-rat repl))
  (header-poly (poly-subst poly var repl )))

(defun rsublis (a-list denom poly &key degree vars-to-sub reduce &aux answ)
  "does a general sublis : a-list of form (list (cons old-var repl-poly)....)
   denom is a poly "
  (let ((tem (cond (vars-to-sub vars-to-sub)
		   (t (loop for v in a-list collecting (car v)))))
	deg)
    (cond ((polynomialp poly)
	   (cond (degree (setq deg degree))
		 (t(setq deg (poly-degree
			       poly
			       tem ))))
	   (setq answ (psublis1 a-list denom poly
				deg
				tem))
	   (cond (reduce (ratreduce  answ  (pexpt denom deg)))
		 (t (cons answ  (pexpt denom deg)))))
	  ((rational-functionp poly)
	   (ratquotient (rsublis a-list denom (num poly) :degree degree :vars-to-sub
				 vars-to-sub )
			(rsublis a-list denom (denom poly) :degree degree :vars-to-sub
				 vars-to-sub)))
	  (t (merror "bad type for poly : should be ratl fn or poly")))))

(defun $psublis (a-list denom poly)
  "use psublis([y=x^2,v=u^3],denom,poly)"
  (header-poly(psublis  (loop for (u v  repl)  in (cdr a-list) by  #'cdr
			      do (check-arg u (eq (car u) 'mequal) "Type a=repl")
			      collecting
			      (cons (p-var (st-rat v))
				    (st-rat repl)))
			(st-rat denom) (st-rat poly))))

(defun $coll_linear (expr &aux answ)
  (cond ((mbagp expr)
	 (setq answ (loop for v in (cdr expr) collecting (coll-linear (st-rat v)))))
	(t (setq answ (coll-linear expr))))
  (setq answ (apply 'append answ))
  (loop for w in answ
	collecting (get w 'disrep) into tem
	finally (return (cons '(mlist) tem))))

(defun psubst ( repl var poly)
  (psublis (list (cons var repl)) 1 poly))
(defun  pdiscriminant (poly var &aux main old-var answ)  ;;main variable
   ;;change to main variable if necessary
  (cond ((eq var :main)(setq var (p-var poly)))
	((not (eql var (p-var poly)))
	 (setq main (gensym))
	 (setf (symbol-value main) 40000000)
	 (setf (get main 'disrep) 'main-var)
	 (show main)(setq old-var var)
	 (setq poly (psubst (list main 1 1) var poly))
	 (print 'hi)
	 (setq var main)))
  (show poly)
   (mshow poly)
   (setq answ   (resultant poly (pderivative poly var) ))
   (cond (main
	  (setq answ (psublis (list (cons main (list old-var 1 1))) 1 answ)))
	 (t answ)))

(defun presultant (p1 p2 var)
  (cond ((and (consp p1) (consp p2)(eq (p-var p1) (p-var p2)) (eq var (p-var p1)))
	 (resultant p1 p2 ))
	(t
	 (let ((args (list p1 p2)))
	   (with-main var  args t
	     (resultant (car args) (second args)))))))



;;incorrect needs
(defun gen-vrem (b divisor &aux tem)
  (cond
    ((atom divisor) 0)
    ((atom b)b )
    ((pointergp (p-var divisor) (p-var b))
     b)
    ((eq (p-var divisor) (p-var b))
	 (second (vdivide b divisor)))
    ((pointergp (p-var b) (p-var divisor))
     (loop for (deg cof) on (cdr b) by #'cddr
	   do (setq tem (gen-vrem  cof divisor))
	   when (not  (pzerop  tem))
	   collecting deg into lis
	   collecting tem into lis
	   finally (return (cond ((eq (car lis) 0) (second lis))
 				 (t (cons (p-var b) lis))))))))

;;incorrect doesn't take into account the
;;denoms eg z^3+ z*p1(x) +p2(x) must find the c for p1 and for p2 and then mult
;;up appropriateley.
;;won't work for x^2*y+x+1 where
(defun gen-vrem (b divisor &aux tem)
  (cond
    ((atom divisor) 0)
    ((atom b)b )
    ((pointergp (p-var divisor) (p-var b))
     b)
    ((eq (p-var divisor) (p-var b))
	 (second (vdivide b divisor)))
    ((pointergp (p-var b) (p-var divisor))
     (loop for (deg cof) on (cdr b) by #'cddr
	   do (setq tem (gen-vrem  cof divisor))
	   when (not  (pzerop  tem))
	   collecting deg into lis
	   collecting tem into lis
	   finally (return (cond ( (eq (car lis) 0)
				  (second lis))
				 (t (cons (p-var b) lis))))))))

;;for resubstitute = nil then we would not want to
;(defun pdeg (pol var)
;  (with-main var pol t
;     (print    (second pol))(print var) pol))
;
;(defun pdeg (pol var)
;  (with-main var pol nil
;     (print    (second pol)) pol))
;



(defun highest-power-dividing (f divisor &aux quot )
;  (declare (values power final-quotient))
  (loop for i from 1
	with prev-quot = f
	do
	(setq quot (testdivide prev-quot divisor))
	when (null quot)
	do (return (values (1- i) prev-quot))
	else do (setq prev-quot quot)))

;c-reqd*f=g*quot+remaind
(defun gen-prem (f g var &aux remainder c-reqd)
  (cond ((< (pdegree f var) (pdegree g var))
	 (setq remainder f)(setq c-reqd 1))
	(t
	 (cond ((and (eq (p-var f) (p-var g))
		     (eq (p-var f) var))
		 (setq remainder  (vdivide f g))
		 (setq c-reqd (third remainder))
		 (setq remainder (second remainder)))
	       (t (with-main-variable u
		    (setq f(psublis (list (cons var (list u 1 1))) 1 f))
		    (setq g(psublis (list (cons var (list u 1 1))) 1 g))
		    (setq remainder  (vdivide f g))
		    ;;this should not involve main variable..
		    (setq c-reqd (third remainder))
		    (setq remainder (second remainder))
		   (setq remainder (psublis (list (cons u
							(list var 1 1)))
					    1  remainder)))))))
  (values remainder c-reqd))

;
;;;tests that (gen-prem  f g var) does same as numerator(remainder (f,g,var))
;;;it was 25 times faster .
;(defun $test (f g var &aux ans2 ans1 dis)
; (user:tim   (setq ans1 ($numerator ($remainder f g var))))
; (user:tim (setq f (st-rat f)) (setq g (st-rat g)))
;  (setq ans2 (gen-prem  f  g (add-newvar var)))
;  (setq dis (new-disrep ans2))
;;  (setq dis ($totaldisrep (header-poly ans2)))
;   (list '(mlist) ans1 dis ($ratsimp (sub* ans1 dis))))

;;works now c*b=q*divisor +r
(defun vdivide (b divisor &aux rnew rfactor leading-gcd deltaq)
  (let ((q 0)(c 1)(r  b))
    (cond
      ((atom divisor) (list b 0 divisor)) ;;should b/gcd(c(b),divisor) 0 a/same
      ((atom b) (list 0 b 1))
      ((pointergp (p-var divisor) (p-var b))
       (list 0 b 1))
      ((pointergp (p-var b) (p-var divisor))
       (list b 0 divisor))
      (t (loop until (or (atom r)
			 (not (eql (p-var r) (p-var divisor)))
			 (< (p-le r) (p-le divisor)))
	       with mon = (list (p-var r) 1 1)
	       do
	       (setq leading-gcd (pgcd (p-lc r) (p-lc divisor)))
	       (setq rfactor (pquotient (p-lc divisor) leading-gcd))
	       ;;make r so that you can form r-divisor*q ==> lower degree
	       (setq rnew (ptimes rfactor r))
	       (setq c (ptimes c rfactor))
	       ;;want rnew-divisor*deltaq to be lower degre in main variable
	       ;;so need (p-lc deltaq) = (p-lc rnew)/(p-lc divisor)
	       ;;but (p-lc rnew)=(p-lc divisor) (p-lc r) /leading-gcd
	       ;;so (p-lc r)/leading-gcd will be the right (p-lc deltaq)
	       ;;then multiply by (p-var r)^ m where m=(differenc of leading degrees)
	       (setq deltaq (ptimes (pquotient (p-lc r) leading-gcd)
				    (pexpt mon (- (p-le r) (p-le divisor)))))
; 	       (mshow  rnew divisor (p-lc rnew) (p-lc divisor) deltaq)

;	       (setq prev-rdeg (p-le r))
	       (setq r (pdifference rnew
				    (ptimes divisor
					    deltaq)))

;	       (cond ((not (< (p-le r) prev-rdeg)) (break t)))
	       (setq q (pplus (ptimes rfactor q) deltaq))
	       finally (return (list q r c)))))))




;(defmacro mshow (&rest l)
;  (loop for v in l
;	collecting `(format t "~%The value of ~A is.. " ',v) into tem
;	collecting `(sh ,v) into tem
;	finally (return (cons 'progn tem))))
;
;(defun test ( b divisor)
;  (let ((answ (vdivide b divisor)))
;    (pdifference (ptimes (third answ) b)
;		 (pplus (ptimes (first answ) a)
;			(second answ)))))

(defun coll-linear  (f &aux *linear*)
	(declare (special *linear*))
	(coll-linear1 f)
	*linear*)

(defun constant-polyp (f)
  (cond ((atom f) t)
	(t (cond ((get (car f)'constant) t)
		 (t (do ((tail (cdr f) (cddr tail)))
			((null tail) t)
		      (cond ((null (constant-polyp (cadr f)))
			     (return nil)))))))))

;;checked thisn
(defun coll-linear1 (poly)
  (declare (special *non-linear*))
  (declare (special *linear*))
  ;;here we collect things that look linear but u*z+u  would collect u
  (cond ((atom poly) nil)
	((get (car poly) 'constant)
	 (loop for (deg cof) on (cdr poly) by #'cddr
	       do (coll-linear1 cof)))
	(t (cond ((and (eq (p-le poly) 1)
		       (constant-functionp (p-lc poly)))
		  (pushnew (p-var poly) *linear*)))
	   (cond ((eq 0 (nth (- (length poly)  2) poly))
		  (coll-linear1 (car (last poly)))))))
  ;;now must check these are really linear to remove the u collected above.
  (cond ((consp poly)
	 (let ((ldeg 0))
	 (cond ((get (p-var poly) 'constant)(setq ldeg 1)))
	 (loop for u in *linear*
	       do
	 (loop for (deg cof) on  (cdr poly) by 'cddr while
	       (> deg 0)
	       when (> (pdegree cof u) ldeg)
	       do  (setq *linear* (delete u *linear* :test #'equal)))))))
  )

;(compare-functions

(defun gen-psublis (old new poly)
  (multiple-value-bind (subs denom)
      (subs-for-psublis old new)
    (psublis subs denom poly)))

(defun gen-rat-sublis (old new f &optional (switch t) &aux answ)
  (simple-rat-sublis (subs-for-simple-rat-sublis old new) f))

(defun subs-for-psublis (old new &optional &aux (lcd 1) a-denom)
  (cond (($listp old) (setq old (cdr old))))
  (cond (($listp new) (setq new (cdr new))))
  (loop for v in old when  (not (get v 'disrep))
     do (return (loop for v in old
		   when (not (get v 'disrep ))
		   do (cond ((symbolp v)
			     (setq v (add-newvar v)))
			    (t (merror "only for replacing symbols")))
		   collecting v into tem
		   finally (setq old tem))))
  (setq new (loop for v in new
	       when (symbolp v)
	       do (cond ((get v 'disrep)
			 (setq v (list v 1 1)))
			(t (setq v (st-rat v))))
	       else when (polynomialp v) nconc nil
	       else do (setq v (new-rat v)) (setq a-denom t)
		 (setq lcd (plcm lcd (denom v)))
	       collecting v))
  (cond (a-denom
	 (loop for v on new
	    when (polynomialp (car v)) do (setf (car v)	(gen-ptimes lcd (car v)))
	    else
	    do (setf (car v) (gen-ptimes (num (car v)) (pquotient lcd (denom (car v)))))))
	(t (setq new (subseq new 0 (length old)))))
  (loop for v in old
     for w on new
     do (setf (car w) (cons v (car w))))
  (values new lcd))

(defun simple-rat-sublis ( subs f &optional (switch t) &aux sub)
  (cond ((atom f) (cons f 1))
	((and (numberp (car f))(numberp (cdr f))) f)
	((polynomialp f)
	 (setq sub (cdr (assoc (p-var f) subs :test #'equal)))
	 (cond ((null sub)(setq sub (cons  (list  (p-var f) 1 1) 1))))
	 (loop for (deg cof) on (cdr f) by #'cddr
	       with answ = (cons 0 1)
	       do (setq answ (ratplus answ (rattimes (ratexpt sub deg)
						     (simple-rat-sublis subs cof) switch)))
	       finally (return answ)))
	((rational-functionp f)
	 (ratquotient (simple-rat-sublis subs (num  f) switch)
	       (simple-rat-sublis subs (denom  f) switch)))
	((loop for v in f collecting (simple-rat-sublis subs v switch)))))


(defun subs-for-simple-rat-sublis (varl subs)
  (loop for v in varl
	for w in subs
	when (not  (get v 'disrep))
	do (setq v (add-newvar v))
	do (cond ((polynomialp w)
		  (setq w (cons w 1)))
		 ((rational-functionp w) nil)
		 ((get w 'disrep)(setq w (cons (list w 1 1)1)))
		 (t  (setq w (new-rat w))))
	collecting (cons v w)))




(defun gen-rat-sublis (old new f &optional (switch t) &aux )
   (simple-rat-sublis (subs-for-simple-rat-sublis old new) f))

;
;;;I tested it on a number of fns and it worked.
;(defun test (old new fn)
;  (setq ans1 (gen-psublis old new fn))
;  (setq ans1 (ratreduce (num ans1) (denom ans1)))
;  (setq ans2  (simple-rat-sublis (subs-for-simple-rat-sublis old new) fn))
;  (mshow ans2 ans1)
;  (equal ans1 ans2))

(defun collect-monomial-coefficients (poly variables)
  "Returns a list of all coefficients of all monomials in VARIABLES occurring in POLY"
  (cond ((null variables) (list poly))
	(($zerop poly) (list poly))
	(t (loop for i from 0 to (pdegree poly (car variables))
		 when (>= i  1)
		 appending (collect-monomial-coefficients
			     (pcoeff poly (list (car variables) i 1)) (cdr variables))
		 else
		 appending
		  (collect-monomial-coefficients
		    (pcoeff poly 1 (subseq variables 0 1)) (cdr variables))))))


(defun collect-monomial-coefficients-and-monomials (poly variables &optional (monomial 1))
  "Returns a list of two element lists,each whose first element is the monomial and
   whose second is the coefficient.  See the associated function which collects and
   then verifies it has the whole sum"

  (cond ((null variables) (list (list monomial poly)))
	(t (loop for i from 0 to (pdegree poly (car variables))
		 for mon = (list (car variables) i 1)
		 when (>= i  1)
		 appending (collect-monomial-coefficients-and-monomials
			     (pcoeff poly mon)
			     (cdr variables)
			     (ptimes monomial mon))
		 else
		 appending
		 (collect-monomial-coefficients-and-monomials
		   (pcoeff poly 1 (subseq variables 0 1)) (cdr variables) monomial)))))


(defun collect-and-verify-coefficients (poly variables &aux answ)
  (setq answ (collect-monomial-coefficients-and-monomials poly variables))
  (loop for v in answ
	with answer = 0
	do (setq answer (pplus answer (ptimes (first v) (second v))))
	finally (iassert (equal answer poly))
	(return (mapcar 'cadr answ))))

(defun va (h)
  (list-variables  (st-rat h)))
