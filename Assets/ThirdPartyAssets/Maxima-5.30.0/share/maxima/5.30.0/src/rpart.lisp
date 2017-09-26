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

(macsyma-module rpart)

;;;	Complex variable utilities
;;;
;;; Macsyma functions: $realpart $imagpart $rectform $polarform
;;;		       $cabs $carg
;;; Utility functions: trisplit risplit absarg cabs andmapc andmapcar

(load-macsyma-macros rzmac)

(declare-top (special $%emode $radexpand rp-polylogp $domain $m1pbranch
		      $logarc rischp $keepfloat complexsign))

(defmvar implicit-real nil "If t RPART assumes radicals and logs
	 of real quantities are real and doesn't ask sign questions")

(defmvar generate-atan2 t "Controls whether RPART will generate ATAN's
			or ATAN2's, default is to make ATAN2's")
;; generate-atan2 is set to nil when doing integration to avoid
;; generating discontinuities that defint can't handle.

;;; Realpart gives the real part of an expr.

(defmfun $realpart (xx) (car (trisplit xx)))

(defprop $realpart %realpart verb)
(defprop %realpart $realpart noun)
(defprop %realpart simp-realpart operators)

(defun simp-realpart (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (let ((sgn nil))
    (cond ((mnump z) z)
          ((eq (setq sgn ($csign z)) '$imaginary)
           0)
          ((eq sgn '$complex)
           (cond ((complex-number-p ($expand z) 'bigfloat-or-number-p)
                  ($realpart z))
                 (t 
                  (eqtest (list '(%realpart) z) expr))))
          (t 
           (eqtest (list '(%realpart) z) expr)))))

;;; Imagpart gives the imaginary part of an expr.

(defmfun $imagpart (xx) (cdr (trisplit xx)))

(defprop $imagpart %imagpart verb)
(defprop %imagpart $imagpart noun)
(defprop %imagpart simp-imagpart operators)

(defun simp-imagpart (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (let ((sgn nil))
    (cond ((mnump z) 0)
          ((eq (setq sgn ($csign z)) '$imaginary)
           (mul -1 '$%i z))
          ((eq sgn '$complex)
           (cond ((complex-number-p ($expand z) 'bigfloat-or-number-p)
                  ($imagpart z))
                 (t 
                  (eqtest (list '(%imagpart) z) expr))))
          (t 
           (eqtest (list '(%imagpart) z) expr)))))

;;; Rectform gives a result of the form a+b*%i.

(defmfun $rectform (xx)
  (let ((ris (trisplit xx)))
    (add (car ris) (mul (cdr ris) '$%i))))

;;; Polarform gives a result of the form a*%e^(%i*b).

(defmfun $polarform (xx)
  (cond ((and (not (atom xx)) (member (caar xx) '(mequal mlist $matrix) :test #'eq))
	 (cons (car xx) (mapcar #'$polarform (cdr xx))))
	(t
	 (let ((aas (absarg xx)) ($%emode nil))
	   (mul (car aas) (powers '$%e (mul '$%i (cdr aas))))))))

;;; Cabs gives the complex absolute value.  Nota bene: an expression may
;;; be syntactically real without being real (e.g. sqrt(x), x<0).  Thus
;;; Cabs must lead an independent existence from Abs.

(defmfun $cabs (xx) (cabs xx))

(defprop $cabs %cabs verb)
(defprop %cabs $cabs noun)
(defprop %cabs simp-cabs operators)

(defun simp-cabs (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (let ((sgn nil))
    (cond ((member (setq sgn ($csign z)) '($complex $imaginary))
           (cond ((complex-number-p ($expand z) 'bigfloat-or-number-p)
                  (simplify (list '(mabs) z)))
                 (t
                  (eqtest (list '(mabs) z) expr))))
          ((eq sgn '$zero)
           0)
          ((member sgn '($pos $pz))
           z)
          ((eq sgn '$neg)
            (mul -1 z))
          (t 
           (eqtest (list '(mabs) z) expr)))))

;;; Carg gives the complex argument.

(defmfun $carg (xx)
  (cond ((and (not (atom xx)) 
              (member (caar xx) '(mequal mlist $matrix) :test #'eq))
	 (cons (car xx) (mapcar #'$carg (cdr xx))))
	(t (cdr (absarg xx)))))

(defprop $carg %carg verb)
(defprop %carg $carg noun)
(defprop %carg simp-carg operators)

(defun simp-carg (expr z simpflag)
  (oneargcheck expr)
  (setq z (simpcheck (cadr expr) simpflag))
  (let ((sgn nil))
    (cond ((eq z '$%i)
           (div '$%pi 2))
          ((member (setq sgn ($csign z)) '($complex $imaginary))
           (cond ((complex-number-p ($expand z) 'bigfloat-or-number-p)
                  ($carg z))
                 (t 
                  (eqtest (list '(%carg) z) expr))))
          ((member sgn '($pos $pz $zero))
           0)
          ((eq sgn '$neg)
            '$%pi)
          (t 
           (eqtest (list '(%carg) z) expr)))))

;; The internal cabs, used by other Macsyma programs.
(defmfun cabs (xx) (car (absarg xx t)))

;; Some objects can only appear at the top level of a legal simplified
;; expression: CRE forms and equations in particular.

(defun trisplit (el) ; Top level of risplit
  (cond ((atom el) (risplit el))
	((specrepp el) (trisplit (specdisrep el)))
	((eq (caar el) 'mequal) (dot-sp-ri (cdr el) '(mequal simp)))
	(t (risplit el))))

;;; Auxiliaries

;; These are Macsyma equivalents to (mapcar 'trisplit ...).  They must
;; differ from other maps for two reasons: the lists are Macsyma lists,
;; and therefore prefixed with list indicators; and the results must
;; be separated: ((a . b) (c . d)) becomes something like ([a,c].[b,d]).

(defun dsrl (el) (dot-sp-ri (cdr el) '(mlist simp)))

(defun dot-sp-ri (el ind)
  (dot--ri (mapcar #'trisplit el) ind))

;; Dot--ri does the ((a.b)(c.d))->([a,c].[b,d]) transformation with
;; minimal Cons'ing.

(defun dot--ri (el ind)
  (do ((i el (cdr i)) (k))
      ((null i) (cons (cons ind (nreverse k)) (cons ind el)))
    (let ((cdari (cdar i)))
      (setq k (rplacd (car i) k))
      (rplaca i cdari))))

(defun risplit-mplus (l)
  (do ((rpart) (ipart) (m (cdr l) (cdr m)))
      ((null m) (cons (addn rpart t) (addn ipart t)))
    (let ((sp (risplit (car m))))
      (cond ((=0 (car sp)))
	    (t (setq rpart (cons (car sp) rpart))))
      (cond ((=0 (cdr sp)))
	    (t (setq ipart (cons (cdr sp) ipart)))))))

(defun risplit-times (l)
  (let ((risl (do ((purerl nil)
		   (compl nil)
		   (l (cdr l) (cdr l)))
		  ((null l) (cons purerl compl))
		(let ((sp (risplit (car l))))
		  (cond ((=0 (cdr sp))
			 (setq purerl (rplacd sp purerl)))
			((or (atom (car sp)) (atom (cdr sp)))
			 (setq compl (cons sp compl)))
			((and (eq (caaar sp) 'mtimes)
;;;Try risplit z/w and notice denominator.  If this check were not made,
;;; the real and imaginary parts would not each be over a common denominator.
			      (eq (caadr sp) 'mtimes)
			      (let ((nr (nreverse (cdar sp)))
				    (ni (nreverse (cddr sp))))
				(cond ((equal (car nr) (car ni))
				       (push (car nr) purerl)
				       (push (cons (muln (nreverse (cdr nr)) t)
						   (muln (nreverse (cdr ni)) t))
					     compl))
				      (t
				       (setq nr (nreverse nr))
				       (setq ni (nreverse ni))
				       nil)))))
			(t
			 (push sp compl)))))))
    (cond ((null (cdr risl))
	   (cons (muln (car risl) t) 0))
	  (t
	   (do ((rpart 1) (ipart 0) (m (cdr risl) (cdr m)))
	       ((null m)
		(cons (muln (cons rpart (car risl)) t)
		      (muln (cons ipart (car risl)) t)))
	     (psetq rpart (sub (mul rpart (caar m)) (mul ipart (cdar m)))
		    ipart (add (mul ipart (caar m)) (mul rpart (cdar m)))))))))

(defun risplit-expt (l)
  (let ((pow (caddr l))
	($radexpand nil)
	(ris nil))		   ; Don't want 'simplifications' like
    (cond ((fixnump pow)	   ; Sqrt(-x) -> %i*sqrt(x)
	   (let ((sp (risplit (cadr l))))
	     (cond ((= pow -1)
		    ;; Handle the case of 1/(x+%i*y) carefully.  This
		    ;; is needed if x and y are (Lisp) numbers to
		    ;; prevent spurious underflows/overflows.  See Bug
		    ;; 2954472.
		    ;; https://sourceforge.net/tracker/?func=detail&atid=104933&aid=2954472&group_id=4933.
		    ;;
		    (if (and (or (numberp (car sp))
				 (ratnump (car sp)))
			     (or (numberp (cdr sp))
				 (ratnump (cdr sp))))
			(sprecip sp)
			(let ((a2+b2 (spabs sp)))
			  (cons (div (car sp) a2+b2)
				(mul -1 (div (cdr sp) a2+b2))))))
		   ((> (abs pow) $maxposex)
		    (cond ((=0 (cdr sp))
			   (cons (powers (car sp) pow) 0))
			  (t
			   (let ((abs^n (powers (add (powers (car sp) 2)
						     (powers (cdr sp) 2))
						(*red pow 2)))
				 (natan (mul pow (genatan (cdr sp) (car sp)))))
			     (cons (mul abs^n (take '(%cos) natan))
				   (mul abs^n (take '(%sin) natan)))))))
		   ((> pow 0)
		    (expanintexpt sp pow))
		   (t
		    (let ((abbas (powers (spabs sp) (- pow)))
			  (basspli (expanintexpt sp (- pow))))
		      (cons (div (car basspli) abbas)
			    (neg (div (cdr basspli) abbas))))))))
	  ((and (ratnump pow)
		(fixnump (cadr pow))
		(not (< (cadr pow) (- $maxnegex)))
		(not (> (cadr pow) $maxposex))
		(prog2
		    (setq ris (risplit (cadr l)))
		    (or (= (caddr pow) 2) (=0 (cdr ris)))))
	   (cond ((=0 (cdr ris))
		  (case (cond ((mnegp (car ris)) '$neg)
			      (implicit-real '$pos)
			      (t ($sign (car ris)))) ; Use $sign not asksign
		    ($neg (risplit (mul2 (power -1 pow) 
                                         (power (neg (car ris)) pow))))
		    ($zero (cons (power 0 pow) 0))
                    ($pos (cons (power (car ris) pow) 0)) ; Add the case $pos
                    (t
                     ;; The sign is unknown. Return a general form.
                     (let ((sp (risplit (caddr l)))
		           (aa (absarg1 (cadr l))))
                       (let ((pre (mul (powers '$%e (mul (cdr aa) (mul (cdr sp) -1)))
                                       (powers (car aa) (car sp))))
                             (post (add (mul (cdr sp) (take '(%log) (car aa)))
                                        (mul (car sp) (cdr aa)))))
                       (cons (mul pre (take '(%cos) post))
                             (mul pre (take '(%sin) post))))))))
		 (t
		  (let ((abs2 (spabs ris))
			(n (abs (cadr pow)))
			(pos? (> (cadr pow) -1)))
		    (let ((abs (power abs2 (1//2)))
                          (sign-imagpart ($sign (cdr ris)))) ; Do we know the sign?
                      (cond ((member sign-imagpart '($neg $pos))
                             (divcarcdr
                               (expanintexpt
                                 (cons (power (add abs (car ris)) (1//2))
                                       (porm (let ((a pos?)
                                                   (b (eq (asksign (cdr ris)) 
                                                          '$negative)))
                                               (cond (a (not b))
                                                     (b t)))
                                             (power (sub abs (car ris)) (1//2))))
                                 n)
                               (if pos?
                                   (power 2 (div n 2))
                                   (power (mul 2 abs2) (div n 2)))))
                            (t
                             ;; The sign is unknown. Return a general form.
                             (let ((sp (risplit (caddr l)))
                                   (aa (absarg1 (cadr l))))
                               (let ((pre (mul (powers '$%e 
                                                       (mul (cdr aa) 
                                                       (mul (cdr sp) -1)))
                                               (powers (car aa) (car sp))))
                                     (post (add (mul (cdr sp) 
                                                     (take '(%log) (car aa)))
                                                (mul (car sp) (cdr aa)))))
                                 (cons (mul pre (take '(%cos) post))
                                       (mul pre (take '(%sin) post))))))))))))
	  ((and (floatp (setq ris (cadr l))) (floatp pow))
	   (risplit (let (($numer t))
		      (exptrl ris pow))))
	  (t
	   (let ((sp (risplit (caddr l)))
		 (aa (absarg1 (cadr l))))
	     ;;If all else fails, we use the trigonometric form.
	     (let ((pre (mul (powers '$%e (mul (cdr aa) (mul (cdr sp) -1)))
			     (powers (car aa) (car sp))))
		   (post (add (mul (cdr sp) (take '(%log) (car aa)))
			      (mul (car sp) (cdr aa)))))
	       (cons (mul pre (take '(%cos) post))
		     (mul pre (take '(%sin) post)))))))))

(defun risplit-noun (l)
  (cons (simplify (list '(%realpart) l)) (simplify (list '(%imagpart) l))))


(defun absarg1 (arg)
  (let ((arg1 arg) ($keepfloat t))
    (cond ((and (or (free arg '$%i)
		    (free (setq arg1 (sratsimp arg)) '$%i))
		(not (eq (csign arg1) t)))
	   (setq arg arg1)
	   (if implicit-real
	       (cons arg 0)
	       (unwind-protect
		    (prog2 (assume `(($notequal) ,arg 0))
			(absarg arg))
		 (forget `(($notequal) ,arg 0)))))
	  (t (absarg arg)))))

;;;	Main function
;;; Takes an expression and returns the dotted pair
;;; (<Real part> . <imaginary part>).

(defun risplit (l)
  (let (($domain '$complex) ($m1pbranch t) $logarc op)
    (cond ((atom l)
           ;; Symbols are assumed to represent real values, unless they have
           ;; been declared to be complex. If they have been declared to be both
           ;; real and complex, they are taken to be real.
	   (cond ((eq l '$%i) (cons 0 1))
		 ((eq l '$infinity) (cons '$und '$und))
		 ((and (decl-complexp l) (not (decl-realp l))) (risplit-noun l))
		 (t (cons l 0))))
	  ((eq (caar l) 'rat) (cons l 0))
	  ((eq (caar l) 'mplus) (risplit-mplus l))
	  ((eq (caar l) 'mtimes) (risplit-times l))
	  ((eq (caar l) 'mexpt) (risplit-expt l))
	  ((eq (caar l) '%log)
	   (let ((aa (absarg1 (cadr l))))
	     (rplaca aa (take '(%log) (car aa)))))
	  ((eq (caar l) 'bigfloat) (cons l 0)) ;All numbers are real.
	  ((and (member (caar l) '(%integrate %derivative %laplace %sum) :test #'eq)
		(freel (cddr l) '$%i))
	   (let ((ris (risplit (cadr l))))
	     (cons (simplify (list* (ncons (caar l)) (car ris) (cddr l)))
		   (simplify (list* (ncons (caar l)) (cdr ris) (cddr l))))))
          ((eq (caar l) '$conjugate)
           (cons (simplify (list '(%realpart) (cadr l)))
                 (mul -1 (simplify (list '(%imagpart) (cadr l))))))
	  ((let ((ass (assoc (caar l)
			     '((%sin %cosh %cos . %sinh)
			       (%cos %cosh %sin . %sinh)
			       (%sinh %cos %cosh . %sin)
			       (%cosh %cos %sinh . %sin)) :test #'eq)))
;;;This clause handles the very similar trigonometric and hyperbolic functions.
;;; It is driven by the table at the end of the lambda.
	     (and ass
		  (let ((ri (risplit (cadr l))))
		    (cond ((=0 (cdr ri)) ;Pure real case.
			   (cons (take (list (car ass)) (car ri)) 0))
			  (t
			   (cons (mul (take (list (car ass)) (car ri))
				      (take (list (cadr ass)) (cdr ri)))
				 (negate-if (eq (caar l) '%cos)
					    (mul (take (list (caddr ass)) (car ri))
						 (take (list (cdddr ass)) (cdr ri)))))))))))
	  ((member (caar l) '(%tan %tanh) :test #'eq)
	   (let ((sp (risplit (cadr l))))
;;;The similar tan and tanh cases.
	     (cond ((=0 (cdr sp))
		    (cons l 0))
		   (t
		    (let* ((2rl (mul (car sp) 2))
			   (2im (mul (cdr sp) 2))
			   (denom (inv (if (eq (caar l) '%tan)
					   (add (take '(%cosh) 2im) (take '(%cos) 2rl))
					   (add (take '(%cos) 2im) (take '(%cosh) 2rl))))))
		      (if (eq (caar l) '%tan)
			  (cons (mul (take '(%sin) 2rl) denom)
				(mul (take '(%sinh) 2im) denom))
			  (cons (mul (take '(%sinh) 2rl) denom)
				(mul (take '(%sin) 2im) denom))))))))
	  ((and (member (caar l) '(%atan %csc %sec %cot %csch %sech %coth) :test #'eq)
		(=0 (cdr (risplit (cadr l)))))
	   (cons l 0))
          ((and (eq (caar l) '$atan2)
                (not (zerop1 (caddr l)))
                (=0 (cdr (risplit (div (cadr l) (caddr l))))))
           ;; Case atan2(y,x) and y/x a real expression.
           (cons l 0))
	  ((or (arcp (caar l)) (eq (caar l) '$atan2))
	   (let ((ans (risplit (let (($logarc t))
				 (resimplify l)))))
	     (when (eq (caar l) '$atan2)
	       (setq ans (cons (sratsimp (car ans)) (sratsimp (cdr ans)))))
	     (if (and (free l '$%i) (=0 (cdr ans)))
		 (cons l 0)
		 ans)))
	  ((eq (caar l) '%plog)
	   ;;  (princ '|Warning: Principal value not guaranteed for Plog in Rectform/|)
	   (risplit (cons '(%log) (cdr l))))
	  ((member (caar l) '(%realpart %imagpart mabs) :test #'eq) (cons l 0))
	  ((eq (caar l) '%erf)
	   (let ((ris (risplit (cadr l))) orig cc)
	     (setq orig (simplify (list '(%erf) (add (car ris) (mul '$%i (cdr ris))))))
	     (setq cc (simplify (list '(%erf) (sub (car ris) (mul '$%i (cdr ris))))))
	     (cons (div (add orig cc) 2) (div (sub orig cc) (mul 2 '$%i)))))
	  ;; Look for a risplit-function on the property list to handle the
	  ;; realpart and imagpart for this function.
          ((setq op (safe-get (mop l) 'risplit-function))
	   (funcall op l))
;;; ^ All the above are guaranteed pure real.
;;; The handling of lists and matrices below has to be thought through.
	  ((eq (caar l) 'mlist) (dsrl l))
	  ((eq (caar l) '$matrix)
	   (dot--ri (mapcar #'dsrl (cdr l)) '($matrix simp)))
	  ((member (caar l) '(mlessp mleqp mgreaterp mgeqp) :test #'eq)
	   (let ((ris1 (risplit (cadr l))) (ris2 (risplit (caddr l))))
	     (cons (simplify (list (ncons (caar l)) (car ris1) (car ris2)))
		   (simplify (list (ncons (caar l)) (cdr ris1) (cdr ris2))))))
;;;The Coversinemyfoot clause covers functions which can be converted
;;; to functions known by risplit, such as the more useless trigonometrics.
	  ((let ((foot (coversinemyfoot l)))
	     (and foot (risplit foot))))
          ((or (safe-get (mop l) 'real-valued)
               (decl-realp (mop l)))
           ;; Simplification for a real-valued function
           (cons l 0))
          ((or (safe-get (mop l) 'commutes-with-conjugate)
               (safe-get (mop l) 'conjugate-function))
	   ;; A function with Mirror symmetry. The general expressions for
	   ;; the realpart and imagpart simplifies accordingly.
	   (cons (mul (div 1 2)
		      (add (simplify (list '($conjugate) l)) l))
		 (mul (div 1 2) '$%i
		      (sub (simplify (list '($conjugate) l)) l))))
;;; A MAJOR ASSUMPTION:
;;;  All random functions are pure real, regardless of argument.
;;;  This is evidently assumed by some of the integration functions.
;;;  Perhaps the best compromise is to return 'realpart/'imagpart
;;;   under the control of a switch set by the integrators.  First
;;;   all such dependencies must be found in the integ
	  ((and rp-polylogp (mqapplyp l) (eq (subfunname l) '$li)) (cons l 0))
	  ((prog2 (setq op (if (eq (caar l) 'mqapply) (caaadr l) (caar l)))
	       (decl-complexp op))
	   (risplit-noun l))
	  ((and (eq (caar l) '%product) (not (free (cadr l) '$%i)))
	   (risplit-noun l))
          (($subvarp l)
           ;; return a real answer for subscripted variable
           (cons l 0))
          (t
           (cons (list '(%realpart simp) l)
                 (list '(%imagpart simp) l))))))

(defun coversinemyfoot (l)
  (prog (recip)
     (cond ((not (member (caar l) '(%csc %sec %cot %csch %sech %coth) :test #'eq)))
	   ((null (setq recip (get (caar l) 'recip))))
	   (t (return (div 1 (cons (list recip) (cdr l))))))))

(defun powers (c d)
  (cond ((=1 d) c)
	((equal d 0) 1)		      ;equal to preclude 0^(pdl 0)->0:
	((=0 c) 0)			; see comment before =0.
	((=1 c) 1)
	(t (power c d))))

(defun spabs (sp)
  ;; SP is a cons of the real part and imaginary part of a complex
  ;; number.  SPABS computes the sum of squares of the real and
  ;; imaginary parts.
  (add (powers (car sp) 2)
       (powers (cdr sp) 2)))

;; Compute 1/(x+%i*y) when both x and y are Lisp numbers or Maxima
;; rationals.  Return a cons of the real and imaginary part of the
;; result.  We count on the underlying Lisp to be able to compute (/
;; (complex x y)) accurately and without unnecessary overflow or
;; underflow..  If not, complain to your Lisp vendor.  Well, it seems
;; that Clisp, CMUCL, and SBCL do a nice job.  But others apparently
;; do not.  (I tested ecl 9.12.3 and ccl 1.4, which both fail.)
;; Workaround those deficiencies.
(defun sprecip (sp)
  (destructuring-bind (x . y)
      sp
    #+(or cmu sbcl)
    (let* ((x (bigfloat:to x))
	   (y (bigfloat:to y))
	   (q (bigfloat:/ (bigfloat:complex x y))))
      (cons (to (bigfloat:realpart q))
	    (to (bigfloat:imagpart q))))
    #-(or cmu sbcl)
    (let ((x (bigfloat:to x))
	  (y (bigfloat:to y)))
      ;; 1/(x+%i*y).
      ;;
      ;; Assume abs(x) > abs(y).  Let r = y/x.  Then
      ;; 1/(x+%i*y) = 1/x/(1+%i*r)
      ;;            = (1-%i*r)/(x*(1+r*r))
      ;;
      ;; The case for abs(x) <= abs(y) is similar with r = x/y:
      ;; 1/(x+%i*y) = 1/y/(r+%i)
      ;;            = (r-%i)/(y*(1+r^2))
      (if (> (bigfloat:abs x) (bigfloat:abs y))
	  (let* ((r (bigfloat:/ y x))
		 (dn (bigfloat:* x (bigfloat:+ 1 (bigfloat:* r r)))))
	    (cons (to (bigfloat:/ dn))
		  (to (bigfloat:/ (bigfloat:- r) dn))))
	  (let* ((r (bigfloat:/ x y))
		 (dn (bigfloat:* y (bigfloat:+ 1 (bigfloat:* r r)))))
	    (cons (to (bigfloat:/ r dn))
		  (to (bigfloat:/ (bigfloat:- dn)))))))))
      
  


(defvar negp* (let ((l (list nil nil t t))) (nconc l l)))

(defun divcarcdr (a b)
  (cons (div (car a) b) (div (cdr a) b)))


;;Expand bas^n, where bas is (<real part> . <imaginary part>)

(defun expanintexpt (bas n)
  (cond ((= n 1) bas)
	(t (do ((rp (car bas))
		(ip (cdr bas))
		(c 1 (quotient (* c ex) i))
		(ex n (1- ex)) (i 1 (1+ i))
		(rori t (not rori)) (negp negp* (cdr negp))
		(rpt nil) (ipt nil))
	       ((< ex 0) (cons (addn rpt t) (addn ipt t)))
	     (declare (fixnum ex i))
	     (set-either rpt ipt
			 rori
			 (cons (negate-if (car negp)
					  (mul c
					       (powers rp ex)
					       (powers ip (1- i))))
			       (cond (rori rpt) (t ipt))))))))



;;;   Subtract out multiples of 2*%pi with a minimum of consing.
;;;   Attempts to reduce to interval (-pi,pi].

(defun 2pistrip (exp)
  (cond ((atom exp) exp)
	((eq (caar exp) 'mtimes)
	 (cond ((and (mnump (cadr exp))
		     (eq (caddr exp) '$%pi)
		     (null (cdddr exp)))
		(cond ((integerp (cadr exp))	; 5*%pi
		       (mul (mod (cadr exp) 2) '$%pi))
		      ((floatp (cadr exp))	; 1.5*%pi
		       (mul (1- (mod (1+ (cadr exp)) 2))
			    '$%pi))
		      ;; Neither 0 nor 1 appears as a coef
		      ((and (listp (cadr exp))
			    (eq 'rat (caaadr exp))) ;5/2*%pi
		       (mul (list* '(rat simp)
				   (- (mod (+ (cadadr exp) (car (cddadr exp)))
					   (* 2 (car (cddadr exp))))
				      (car (cddadr exp)))
				   (cddadr exp))
			    '$%pi))
		      (t exp)))
	       (t exp)))
	((eq (caar exp) 'mplus)
	 (let ((res (2pirec (cdr exp))))
	   (if (eq res (cdr exp))
	       exp
	       (addn res t))))
	(t exp)))

(defun 2pirec (fm)			;Takes a list of exprs
  (cond ((null (cdr fm))		;If monad, just return.
	 (let ((2pf (2pistrip (car fm))))
	   (cond ((eq 2pf (car fm)) fm)
		 ((=0 2pf) nil)
		 (t (list 2pf)))))
	(t
	 (let ((2pfma (2pistrip (car fm)))
	       (2pfmd (2pirec (cdr fm))))
	   (cond ((or (null 2pfmd) (=0 2pfmd)) 2pfma)
		 ((and (eq 2pfmd (cdr fm)) (eq 2pfma (car fm))) fm)
		 (t (cons 2pfma 2pfmd)))))))

;;;	Rectify into polar form; Arguments similar to risplit

(defun argnum (n)
  (if (minusp n)
      (simplify '$%pi)
      0))


;; absarg
;; returns pair (abs . arg)
;; if absflag is true, arg result is not guaranteed to be correct

;; The function of Absflag is to communicate that only the absolute
;; value part of the result is wanted.  This allows Absarg to avoid asking
;; questions irrelevant to the absolute value.  For instance, Cabs(x) is
;; invariably Abs(x), while the complex phase may be 0 or %pi.  Note also
;; the steps taken in Absarg to assure that Asksign's will happen before Sign's
;; as often as possible, so that, for instance, Abs(x) can be simplified to
;; x or -x if the sign of x must be known for some other reason.  These
;; techniques, however, are not perfect.

(defun absarg (l &optional (absflag nil))
;; Commenting out the the expansion of the expression l. It seems to be not
;; necessary, but can cause expression swelling (DK 01/2010).
;  (setq l ($expand l))
  (cond ((atom l)
	 (cond ((eq l '$%i)
		(cons 1 (simplify '((mtimes) ((rat simp) 1 2) $%pi))))
	       ((numberp l)
		(cons (abs l) (argnum l)))
	       ((member l '($%e $%pi) :test #'eq) (cons l 0))
	       ((eq l '$infinity) (cons '$inf '$ind))
               ((decl-complexp l)
                (cons (list '(mabs simp) l) ; noun form with mabs
                      (list '(%carg simp) l)))
	       (absflag (cons (take '(mabs) l) 0))
	       (t
                ;; At this point l is representing a real value. Try to 
                ;; determine the sign and return a general form when the sign is
                ;; unknown.
		(let ((gs (if (eq rischp l) '$pos ($sign l))))
		  (cond ((member gs '($pos $pz)) (cons l 0))
			((eq gs '$zero) (cons 0 0))
			((eq gs '$neg)
			 (cons (neg l) (simplify '$%pi)))
			(t (cons (take '(mabs) l) (genatan 0 l))))))))
	((eq '$zero (let ((sign-imag-errp nil)) (catch 'sign-imag-err ($sign l))))
	 (cond ((some-bfloatp l)
		(cons bigfloatzero bigfloatzero))	; contagious
	       ((some-floatp l)
		(cons 0.0 0.0))
	       (t (cons 0 0))))
	((member (caar l) '(rat bigfloat) :test #'eq)
	 (cons (list (car l) (abs (cadr l)) (caddr l))
	       (argnum (cadr l))))
	((eq (caar l) 'mtimes)
	 (do ((n (cdr l) (cdr n))
	      (abars)
	      (argl () (cons (cdr abars) argl))
	      (absl () (rplacd abars absl)))
	     (())
	   (unless n
	     (return (cons (muln absl t) (2pistrip (addn argl t)))))
	   (setq abars (absarg (car n) absflag))))
        ((eq (caar l) 'mexpt)
         ;; An expression z^a
         (let ((aa (absarg (cadr l) nil)) ; (abs(z) . arg(z))
               (sp (risplit (caddr l)))   ; (realpart(a) . imagpart(a))
               ($radexpand nil))
           (cond ((and (zerop1 (cdr sp))
                       (eq ($sign (sub 1 (take '(mabs) (car sp)))) '$pos))
                  ;; Special case: a is real and abs(a) < 1.
                  ;; This simplifies e.g. carg(sqrt(z)) -> carg(z)/2
                  (cons (mul (power (car aa) (car sp))
                             (power '$%e (neg (mul (cdr aa) (cdr sp)))))
                        (mul (caddr l) (cdr aa))))
                 (t
                  ;; General case for z and a
                  (let ((arg (add (mul (cdr sp) (take '(%log) (car aa)))
                                  (mul (cdr aa) (car sp)))))
                    (cons (mul (power (car aa) (car sp))
                               (power '$%e (neg (mul (cdr aa) (cdr sp)))))
                          (if generate-atan2
			      (take '($atan2)
				    (take '(%sin) arg)
				    (take '(%cos) arg))
			    (take '(%atan) (take '(%tan) arg)))))))))
	((and (member (caar l) '(%tan %tanh) :test #'eq)
	      (not (=0 (cdr (risplit (cadr l))))))
	 (let* ((sp (risplit (cadr l)))
		(2frst (mul (cdr sp) 2))
		(2scnd (mul (car sp) 2)))
	   (when (eq (caar l) '%tanh)
	     (psetq 2frst 2scnd 2scnd 2frst))
	   (cons (let ((cosh (take '(%cosh) 2frst))
		       (cos (take '(%cos) 2scnd)))
		   (root (div (add cosh (neg cos))
			      (add cosh cos))
			 2))
		 (take '(%atan)
		       (if (eq (caar l) '%tan)
			   (div (take '(%sinh) 2frst) (take '(%sin) 2scnd))
			   (div (take '(%sin) 2frst) (take '(%sinh) 2scnd)))))))
	((specrepp l) (absarg (specdisrep l) absflag))
	((let ((foot (coversinemyfoot l)))
	   (and foot (not (=0 (cdr (risplit (cadr l))))) (absarg foot absflag))))
	(t
	 (let ((ris (trisplit l)))
	   (xcons
;;; Arguments must be in this order so that the side-effect of the Atan2,
;;; that is, determining the Asksign of the argument, can happen before
;;; Take Mabs does its Sign.  Blame JPG for noticing this lossage.
	    (if absflag 0 (genatan (cdr ris) (car ris)))
	    (cond ((equal (car ris) 0) (absarg-mabs (cdr ris)))
		  ((equal (cdr ris) 0) (absarg-mabs (car ris)))
		  (t (powers ($expand (add (powers (car ris) 2)
					   (powers (cdr ris) 2))
				      1 0)
			     (half)))))))))

(defun genatan (num den)
  (let ((arg (take '($atan2) num den)))
    (if (or generate-atan2
            (zerop1 den)
            (free arg '$atan2))
        arg
        (take '(%atan) (div num den)))))

(defun absarg-mabs (l)
  (cond ((eq (csign l) t)
         (if (member (caar l) '(mabs %cabs) :test #'eq) 
             l 
             (list '(mabs simp) l))) ; mabs and not %cabs as noun form
        ((member ($csign l) '($complex $imaginary))
         ;; Do not try to simplify a complex expression at this point,
         ;; this would cause an endless loop. Return a noun form.
         (list '(mabs simp) l))
        (t 
         (take '(mabs) l))))
