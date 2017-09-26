;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;	** (c) Copyright 1982 Massachusetts Institute of Technology **

(macsyma-module comm)

(declare-top (special $exptsubst $linechar $nolabels $inflag $piece $dispflag
		      $gradefs $props $dependencies derivflag derivlist
		      $linenum $partswitch *linelabel* nn* dn*
		      $powerdisp atvars $errexp $derivsubst $dotdistrib
		      $opsubst $subnumsimp $transrun in-p substp $sqrtdispflag
		      $pfeformat dummy-variable-operators))

(defvar *islinp* nil) ; When T, sdiff is called from the function islinear.
(defvar *atp* nil)    ; When T, prevents substitution from applying to vars 
                      ; bound by %sum, %product, %integrate, %limit

;; op and opr properties

(defvar *opr-table* (make-hash-table :test #'equal))

(defmfun getopr0 (x)
  (or
    (and (symbolp x) (get x 'opr))
    (and (stringp x) (gethash x *opr-table*))))

(defmfun getopr (x)
  (or (getopr0 x) x))

(defmfun putopr (x y)
  (or
    (and (symbolp x) (setf (get x 'opr) y))
    (and (stringp x) (setf (gethash x *opr-table*) y))))

(defmfun remopr (x)
  (or
    (and (symbolp x) (remprop x 'opr))
    (and (stringp x) (remhash x *opr-table*))))

(mapc #'(lambda (x) (putprop (car x) (cadr x) 'op) (putopr (cadr x) (car x)))
      '((mplus "+") (mminus "-") (mtimes "*") (mexpt "**") (mexpt "^")
	(mnctimes ".") (rat "/") (mquotient "/") (mncexpt "^^")
	(mequal "=") (mgreaterp ">") (mlessp "<") (mleqp "<=") (mgeqp ">=")
	(mnotequal "#") (mand "and") (mor "or") (mnot "not") (msetq ":")
	(mdefine ":=") (mdefmacro "::=") (mquote "'") (mlist "[")
	(mset "::") (mfactorial "!") (marrow "-->") (mprogn "(")
	(mcond "if")))

(mapc #'(lambda (x) (putprop (car x) (cadr x) 'op))
      '((mqapply $subvar) (bigfloat $bfloat)))

(setq $exptsubst nil
      $partswitch nil
      $inflag nil
      $gradefs '((mlist simp))
      $dependencies '((mlist simp))
      atvars '($@1 $@2 $@3 $@4)
      $derivsubst nil
      $opsubst t
      in-p nil
      substp nil)

(defmvar $vect_cross nil
  "If TRUE allows DIFF(X~Y,T) to work where ~ is defined in
	  SHARE;VECT where VECT_CROSS is set to TRUE.")

(defmfun $substitute (old new &optional (expr nil three-arg?))
  (cond (three-arg? (maxima-substitute old new expr))
	(t
	 (let ((l old) (z new))
	   (cond ((and ($listp l) ($listp (cadr l)) (null (cddr l)))
		  ($substitute (cadr l) z))
		 ((notloreq l) (improper-arg-err l '$substitute))
		 ((eq (caar l) 'mequal) (maxima-substitute (caddr l) (cadr l) z))
		 (t (do ((l (cdr l) (cdr l)))
			((null l) z)
		      (setq z ($substitute (car l) z)))))))))

;; Define an alias $psubst and a reversealias for $psubstitute
(defprop $psubst $psubstitute alias)
(defprop $psubstitute $psubst reversealias)

;; $psubstitute is similar to $substitute. In distinction from $substitute
;; the function $psubstitute does parallel substitution, if the first argument
;; is a list of equations.
(defun $psubstitute (old new &optional (expr nil three-arg?))
  (cond (three-arg? (maxima-substitute old new expr))
        (t
         (let ((l old) (z new))
           (cond ((and ($listp l)
                       ($listp (cadr l))
                       (null (cddr l)))
                  ;; A nested list.
                  ($psubstitute (cadr l) z))
                 ((and ($listp l)
                       (eq (caar (cadr l)) 'mequal)
                       (null (cddr l)))
                  ;; A list with one equation.
                  ($psubstitute (cadr l) z))
                 ((notloreq l) (improper-arg-err l '$psubstitute))
                 ((eq (caar l) 'mequal)
                  ;; Do a substitution for one equation.
                  (maxima-substitute (caddr l) (cadr l) z))
                 (t
                  ;; We have a list of equations. We do parallel subsitution.
                  (let (gensymbol genlist eqn ($simp nil))
                    ;; At first substitute a gensym for the expressions of
                    ;; the left hand side of the equations.
                    (do ((l (cdr l) (cdr l)))
                        ((null l) z)
                      (setq eqn (car l))
                      (when (not (eq 'mequal (caar eqn)))
                        (improper-arg-err old '$substitute))
                      (setq gensymbol (gensym))
                      ;; Store the gensym and the new expression into a list.
                      (push (cons gensymbol (caddr eqn)) genlist)
                      ;; Substitute a gensym for the old expression.
                      (setq z (maxima-substitute gensymbol (cadr eqn) z)))
                      ;; Substitute the new expressions for the gensyms.
                      (do ((l genlist (cdr l)))
                          ((null l)
                           ;; Resimplify the result.
                           (let (($simp t)) (resimplify z)))
                        (setq z (maxima-substitute (cdar l) (caar l) z))))))))))

(defmfun maxima-substitute (x y z) ; The args to SUBSTITUTE are assumed to be simplified.
  (let ((in-p t) (substp t))
    (if (and (mnump y) (= (signum1 y) 1))
	(let ($sqrtdispflag ($pfeformat t)) (setq z (nformat-all z))))
    (simplifya
     (if (atom y)
	 (cond ((equal y -1)
		(setq y '((mminus) 1)) (subst2 x y (nformat-all z) nil nil)) ;; negxpty and timesp don't matter in this call since (caar y) != 'mexpt
	       (t
		(cond ((and (not (symbolp x))
			    (functionp x))
		       (let ((tem (gensym)))
			 (setf (get  tem  'operators) 'application-operator)
			 (setf (symbol-function tem) x)
			 (setq x tem))))
		(subst1 x y z)))
	 (let ((negxpty (if (and (eq (caar y) 'mexpt)
				 (= (signum1 (caddr y)) 1))
			    (mul2 -1 (caddr y))))
	       (timesp (if (eq (caar y) 'mtimes) (setq y (nformat y)))))
	   (subst2 x y z negxpty timesp)))
     nil)))

;;Remainder of page is update from F302 --gsb

;;Used in COMM2 (AT), limit, and below.
(defvar dummy-variable-operators '(%product %sum %laplace %integrate %limit %at))

(defun subst1 (x y z)			; Y is an atom
  (cond ((atom z) (if (equal y z) x z))
	((specrepp z) (subst1 x y (specdisrep z)))
	((eq (caar z) 'bigfloat) z)
	((and (eq (caar z) 'rat) (or (equal y (cadr z)) (equal y (caddr z))))
	 (div (subst1 x y (cadr z)) (subst1 x y (caddr z))))
	((at-substp z) (subst-except-second-arg x y z))
	((and (eq y t) (eq (caar z) 'mcond))
	 (list (cons (caar z) nil) (subst1 x y (cadr z)) (subst1 x y (caddr z))
	       (cadddr z) (subst1 x y (car (cddddr z)))))
	(t (let ((margs (mapcar #'(lambda (z1) (subst1 x y z1)) (cdr z)))
                 (oprx (getopr x)) (opry (getopr y)))
	     (if (and $opsubst
		      (or (eq opry (caar z))
			  (and (eq (caar z) 'rat) (eq opry 'mquotient))))
		 (if (or (numberp x)
			 (member x '(t nil $%e $%pi $%i) :test #'eq)
			 (and (not (atom x))
			      (not (or (eq (car x) 'lambda)
				       (eq (caar x) 'lambda)))))
		     (if (or (and (member 'array (cdar z) :test #'eq)
				  (or (and (mnump x) $subnumsimp)
				      (and (not (mnump x)) (not (atom x)))))
			     ($subvarp x))
			 (let ((substp 'mqapply))
			   (subst0 (list* '(mqapply) x margs) z))
			 (merror (intl:gettext "subst: cannot substitute ~M for operator ~M in expression ~M") x y z))
		     (subst0 (cons (cons oprx nil) margs) z))
		 (subst0 (cons (cons (caar z) nil) margs) z))))))

(defun subst2 (x y z negxpty timesp)
  (let (newexpt)
    (cond ((atom z) z)
	  ((specrepp z) (subst2 x y (specdisrep z) negxpty timesp))
	  ((and *atp* (member (caar z) '(%derivative %laplace) :test #'eq)) z)
	  ((at-substp z) z)
	  ((alike1 y z) x)
	  ((and timesp (eq (caar z) 'mtimes) (alike1 y (setq z (nformat z)))) x)
	  ((and (eq (caar y) 'mexpt) (eq (caar z) 'mexpt) (alike1 (cadr y) (cadr z))
		(setq newexpt (cond ((alike1 negxpty (caddr z)) -1)
				    ($exptsubst (expthack (caddr y) (caddr z))))))
	   (list '(mexpt) x newexpt))
	  ((and $derivsubst (eq (caar y) '%derivative) (eq (caar z) '%derivative)
		(alike1 (cadr y) (cadr z)))
	   (let ((tail (subst-diff-match (cddr y) (cdr z))))
	     (cond ((null tail) z)
		   (t (cons (cons (caar z) nil) (cons x (cdr tail)))))))
	  (t (recur-apply #'(lambda (z1) (subst2 x y z1 negxpty timesp)) z)))))

;; replace y with x in z, but leave z's second arg unchanged.
;; This is for cases like at(integrate(x, x, a, b), [x=3])
;; where second arg of integrate binds a new variable x,
;; and we do not wish to subst 3 for x inside integrand.
(defun subst-except-second-arg (x y z)
  (cond 
    ((member (caar z) '(%integrate %sum %product %limit))
     (append 
       (list (car z)
             (if (eq y (third z))     ; if (third z) is new var that shadows y
                 (second z)           ; leave (second z) unchanged
                 (subst1 x y (second z))) ; otherwise replace y with x in (second z)
             (third z))               ; never change integration var
       (mapcar (lambda (z) (subst1 x y z)); do subst in limits of integral
               (cdddr z))))
    (t z)))

(defmfun subst0 (new old)
  (cond ((atom new) new)
	((alike (cdr new) (cdr old))
	 (cond ((eq (caar new) (caar old)) old)
	       (t (simplifya (cons (cons (caar new) (member 'array (cdar old) :test #'eq)) (cdr old))
			     nil))))
	((member 'array (cdar old) :test #'eq)
	 (simplifya (cons (cons (caar new) '(array)) (cdr new)) nil))
	(t (simplifya new nil))))

(defun expthack (y z)
  (prog (nn* dn* yn yd zn zd qd)
     (cond ((and (mnump y) (mnump z))
	    (return (if (numberp (setq y (div* z y))) y)))
	   ((atom z) (if (not (mnump y)) (return nil)))
	   ((or (ratnump z) (eq (caar z) 'mplus)) (return nil)))
     (numden y)				; (CSIMP) sets NN* and DN*
     (setq yn nn* yd dn*)
     (numden z)
     (setq zn nn* zd dn*)
     (setq qd (cond ((and (equal zd 1) (equal yd 1)) 1)
		    ((prog2 (numden (div* zd yd))
			 (and (equal dn* 1) (equal nn* 1)))
		     1)
		    ((equal nn* 1) (div* 1 dn*))
		    ((equal dn* 1) nn*)
		    (t (return nil))))
     (numden (div* zn yn))
     (if (equal dn* 1) (return (div* nn* qd)))))

(defun subst-diff-match (l1 l2)
  (do ((l l1 (cddr l)) (l2 (copy-list l2)) (failed nil nil))
      ((null l) l2)
    (do ((l2 l2 (cddr l2)))
	((null (cdr l2)) (setq failed t))
      (if (alike1 (car l) (cadr l2))
	  (if (and (fixnump (cadr l)) (fixnump (caddr l2)))
	      (cond ((< (cadr l) (caddr l2))
		     (return (rplacd (cdr l2)
				     (cons (- (caddr l2) (cadr l))
					   (cdddr l2)))))
		    ((= (cadr l) (caddr l2))
		     (return (rplacd l2 (cdddr l2))))
		    (t (return (setq failed t))))
	      (return (setq failed t)))))
    (if failed (return nil))))

;;This probably should be a subst or macro.
(defun at-substp (z)
  (and *atp*
       (or (member (caar z) '(%derivative %del) :test #'eq)
           (member (caar z) dummy-variable-operators :test #'eq))))

(defmfun recur-apply (fun e)
  (cond ((eq (caar e) 'bigfloat) e)
	((specrepp e) (funcall fun (specdisrep e)))
	(t (let ((newargs (mapcar fun (cdr e))))
	     (if (alike newargs (cdr e))
		 e
		 (simplifya (cons (cons (caar e) (member 'array (cdar e) :test #'eq)) newargs)
			    nil))))))

(defmfun $depends (&rest args)
  (when (oddp (length args))
    (merror (intl:gettext "depends: number of arguments must be even.")))
  (do ((args args (cddr args))
       (l))
      ((null args) (i-$dependencies (nreverse l)))
    (if ($listp (first args))
	(mapc #'(lambda (e) (push (depends1 e (second args)) l))
	      (cdr (first args)))
	(push (depends1 (first args) (second args)) l))))

(defun depends1 (x y)
  (nonsymchk x '$depends)
  (cons (cons x nil) (if ($listp y) (cdr y) (cons y nil))))

(defmspec $dependencies (form)
  (i-$dependencies (cdr form)))

(defun i-$dependencies (l &aux res)
  (dolist (z l)
    (cond
      ((atom z)
       (merror
         (intl:gettext
           "depends: argument must be a non-atomic expression; found ~M") z))
      (t
       (do ((zz z (cdr zz))
            (y nil))
           ((null zz)
            (mputprop (caar z) (setq y (reverse y)) 'depends)
            (setq res (push (cons (ncons (caar z)) y) res))
            (unless (cdr $dependencies)
              (setq $dependencies (copy-list '((mlist simp)))))
            (add2lnc (cons (cons (caar z) nil) y) $dependencies))
         (cond ((and ($subvarp (cadr zz))
                     (not (member (caar (cadr zz)) y)))
                (setq y (push (cadr zz) y)))
               ((not (symbolp (cadr zz)))
                (merror
                  (intl:gettext "depends: argument must be a symbol; found ~M")
                  (cadr zz)))
               ((and (cadr zz)
                     (not (member (cadr zz) y)))
                (setq y (push (cadr zz) y))))))))
  (cons '(mlist simp) (reverse res)))

(defmspec $gradef (l)
  (setq l (cdr l))
  (let ((z (car l)) (n 0))
    (cond ((atom z)
	   (if (not (= (length l) 3)) (merror (intl:gettext "gradef: expected exactly three arguments.")))
	   (mputprop z
		     (cons (cons (cadr l) (meval (caddr l)))
			   (mget z '$atomgrad))
		     '$atomgrad)
	   (i-$dependencies (cons (cons (ncons z)
	                                ;; Append existing dependencies
	                                (cons (cadr l) (mget z 'depends)))
	                          nil))
	   (add2lnc z $props)
	   z)
	  ((or (mopp1 (caar z)) (member 'array (cdar z) :test #'eq))
	   (merror (intl:gettext "gradef: argument cannot be a built-in operator or subscripted expression; found ~M") z))
	  ((prog2 (setq n (- (length z) (length l))) (minusp n))
	   (wna-err '$gradef))
	  (t (do ((zl (cdr z) (cdr zl))) ((null zl))
	       (if (not (symbolp (car zl)))
		   (merror (intl:gettext "gradef: argument must be a symbol; found ~M") (car zl))))
	     (setq l (nconc (mapcar #'(lambda (x) (remsimp (meval x)))
				    (cdr l))
			    (mapcar #'(lambda (x) (list '(%derivative) z x 1))
				    (nthcdr (- (length z) n) z))))
	     (putprop (caar z)
		      (sublis (mapcar #'cons (cdr z) (mapcar #'stripdollar (cdr z)))
			      (cons (cdr z) l))
		      'grad)
	     (or (cdr $gradefs) (setq $gradefs (copy-list '((mlist simp)))))
	     (add2lnc (cons (cons (caar z) nil) (cdr z)) $gradefs) z))))

(defmfun $diff (&rest args)
  #-gcl
  (declare (dynamic-extent args))
  (let (derivlist)
    (deriv args)))

(defmfun $del (e)
  (stotaldiff e))

(defun deriv (e)
  (prog (exp z count)
     (cond ((null e) (wna-err '$diff))
	   ((null (cdr e)) (return (stotaldiff (car e))))
	   ((null (cddr e)) (nconc e '(1))))
     (setq exp (car e) z (setq e (copy-list e)))
     loop (if (or (null derivlist) (member (cadr z) derivlist :test #'equal)) (go doit))
					; DERIVLIST is set by $EV
     (setq z (cdr z))
     loop2(cond ((cdr z) (go loop))
		((null (cdr e)) (return exp))
		(t (go noun)))
     doit (cond ((nonvarcheck (cadr z) '$diff))
		((null (cddr z)) (wna-err '$diff))
		((not (eq (ml-typep (caddr z)) 'fixnum)) (go noun))
		((minusp (setq count (caddr z)))
		 (merror (intl:gettext "diff: order of derivative must be a nonnegative integer; found ~M") count)))
     loop1(cond ((zerop count) (rplacd z (cdddr z)) (go loop2))
		((equal (setq exp (sdiff exp (cadr z))) 0) (return 0)))
     (setq count (1- count))
     (go loop1)
     noun (return (diff%deriv (cons exp (cdr e))))))

(defun chainrule (e x)
  (let (w)
    (cond (*islinp*
           ;; sdiff is called from the function islinear.
           (if (and (not (atom e))
                    (eq (caar e) '%derivative)
                    (not (freel (cdr e) x)))
               (diff%deriv (list e x 1))
               0))
	  ((atomgrad e x))
	  ((not (setq w (mget (cond ((atom e) e)
				    ((member 'array (cdar e) :test #'eq) (caar e))
				    ((atom (cadr e)) (cadr e))
				    (t (caaadr e)))
			      'depends)))
	   0)
	  (t (let (derivflag)
	       (addn (mapcar
		      #'(lambda (u)
			  (let ((y (sdiff u x)))
			    (if (equal y 0)
				0
				(list '(mtimes)
				      (or (atomgrad e u)
					  (list '(%derivative) e u 1))
				      y))))
		      w)
		     nil))))))

(defun atomgrad (e x)
  (let (y)
    (and (atom e) (setq y (mget e '$atomgrad)) (assolike x y))))

(defun depends (e x &aux l)
  (setq e (specrepcheck e))
  (cond ((alike1 e x) t)
        ((mnump e) nil)
        ((and (symbolp e) (setq l (mget e 'depends)))
         ;; Go recursively through the list of dependencies.
         ;; This code detects indirect dependencies like a(x) and x(t).
         (dependsl l x))
        ((atom e) nil)
        (t (or (depends (caar e) x)
               (dependsl (cdr e) x)))))

(defun dependsl (l x)
  (dolist (u l)
    (if (depends u x) (return t))))

(defmfun sdiff (e x) ; The args to SDIFF are assumed to be simplified.
  ;; Remove a special representation from the variable of differentiation
  (setq x (specrepcheck x))
  (cond ((alike1 e x) 1)
	((mnump e) 0)
	((or (atom e) (member 'array (cdar e) :test #'eq)) (chainrule e x))
	((eq (caar e) 'mrat) (ratdx e x))
        ((eq (caar e) 'mpois) ($poisdiff e x)) ; Poisson series
	((eq (caar e) 'mplus) (addn (sdiffmap (cdr e) x) t))
	((mbagp e) (cons (car e) (sdiffmap (cdr e) x)))
	((member (caar e) '(%sum %product) :test #'eq) (diffsumprod e x))
	((eq (caar e) '%at) (diff-%at e x))
	((not (depends e x)) 0)
	((eq (caar e) 'mtimes) (addn (sdifftimes (cdr e) x) t))
	((eq (caar e) 'mexpt) (diffexpt e x))
	((eq (caar e) 'mnctimes)
	 (let (($dotdistrib t))
	   (add2 (ncmuln (cons (sdiff (cadr e) x) (cddr e)) t)
		 (ncmul2 (cadr e) (sdiff (cons '(mnctimes) (cddr e)) x)))))
	((and $vect_cross (eq (caar e) '|$~|))
	 (add2* `((|$~|) ,(cadr e) ,(sdiff (caddr e) x))
		`((|$~|) ,(sdiff (cadr e) x) ,(caddr e))))
	((eq (caar e) 'mncexpt) (diffncexpt e x))
	((member (caar e) '(%log %plog) :test #'eq)
	 (sdiffgrad (cond ((and (not (atom (cadr e))) (eq (caaadr e) 'mabs))
			   (cons (car e) (cdadr e)))
			  (t e))
		    x))
	((eq (caar e) '%derivative)
	 (cond ((or (atom (cadr e)) (member 'array (cdaadr e) :test #'eq)) (chainrule e x))
	       ((freel (cddr e) x) (diff%deriv (cons (sdiff (cadr e) x) (cddr e))))
	       (t (diff%deriv (list e x 1)))))
	((member (caar e) '(%binomial $beta) :test #'eq)
	 (let ((efact ($makefact e)))
	   (mul2 (factor (sdiff efact x)) (div e efact))))
	((eq (caar e) '%integrate) (diffint e x))
	((eq (caar e) '%laplace) (difflaplace e x))
	((eq (caar e) '%at) (diff-%at e x))
; This rule is not correct. We cut it out.
;	((member (caar e) '(%realpart %imagpart) :test #'eq)
;	 (list (cons (caar e) nil) (sdiff (cadr e) x)))
	((and (eq (caar e) 'mqapply)
	      (eq (caaadr e) '$%f))
	 ;; Handle %f, hypergeometric function
	 ;;
	 ;; The derivative of %f[p,q]([a1,...,ap],[b1,...,bq],z) is
	 ;;
	 ;; a1*a2*...*ap/(b1*b2*...*bq)
	 ;;   *%f[p,q]([a1+1,a2+1,...,ap+1],[b1+1,b2+1,...,bq+1],z)
	 (let* ((arg1 (cdr (third e)))
		(arg2 (cdr (fourth e)))
		(v (fifth e)))
	   (mul (sdiff v x)
		(div (mull arg1) (mull arg2))
		`((mqapply) (($%f array) ,(length arg1) ,(length arg2))
		  ((mlist) ,@(incr1 arg1))
		  ((mlist) ,@(incr1 arg2))
		  ,v))))
	(t (sdiffgrad e x))))

(defun sdiffgrad (e x)
  (let ((fun (caar e)) grad args result)
    (cond ((and (eq fun 'mqapply) (oldget (caaadr e) 'grad))
           ;; Change the array function f[n](x) to f(n,x), call sdiffgrad again.
           (setq result
	         (sdiffgrad (cons (cons (caaadr e) nil) 
	                          (append (cdadr e) (cddr e)))
		            x))
           ;; If noun form for f(n,x), adjust the noun form for f[n](x)
           (if (isinop result '%derivative)
               (if (not (depends e x))
                   0
                   (diff%deriv (list e x 1)))
               result))

	  ;; extension for pdiff.
	  ((and (get '$pderivop 'operators) (sdiffgrad-pdiff e x)))

	  ;; two line extension for hypergeometric.
	  ((and (equal fun '$hypergeometric) (get '$hypergeometric 'operators))
	   (diff-hypergeometric (second e) (third e) (fourth e) x))

	  ((or (eq fun 'mqapply) (null (setq grad (oldget fun 'grad))))
	   (if (not (depends e x)) 0 (diff%deriv (list e x 1))))
	  ((not (= (length (cdr e)) (length (car grad))))
	   (merror (intl:gettext "~:M: expected exactly ~M arguments.") 
	           fun 
	           (length (car grad))))
	  (t
           (setq args (sdiffmap (cdr e) x))
           (setq result
                 (addn
                   (mapcar 
                     #'mul2
                     (cdr 
                       (substitutel
                         (cdr e) 
                         (car grad)
                         (do ((l1 (cdr grad) (cdr l1))
                              (args args (cdr args)) 
                              (l2))
                             ((null l1) (cons '(mlist) (nreverse l2)))
                           (setq l2
                                 (cons (cond ((equal (car args) 0) 0)
                                             ((functionp (car l1))
                                              ;; Evaluate a lambda expression
                                              ;; given as a derivative.
                                              (apply (car l1) (cdr e)))
                                             (t (car l1)))
                                       l2)))))
                     args)
                   t))
           (if (or (null result) (not (freeof nil result)))
               ;; A derivative has returned NIL. Return a noun form.
               (if (not (depends e x))
                   0 
                   (diff%deriv (list e x 1)))
               result)))))

(defun sdiffmap (e x)
  (mapcar #'(lambda (term) (sdiff term x)) e))

(defun sdifftimes (l x)
  (prog (term left out)
   loop (setq term (car l) l (cdr l))
   (setq out (cons (muln (cons (sdiff term x) (append left l)) t) out))
   (if (null l) (return out))
   (setq left (cons term left))
   (go loop)))

(defun diffexpt (e x)
  (if (mnump (caddr e))
      (mul3 (caddr e) (power (cadr e) (addk (caddr e) -1)) (sdiff (cadr e) x))
      (mul2 e (add2 (mul3 (power (cadr e) -1) (caddr e) (sdiff (cadr e) x))
		    (mul2 (simplifya (list '(%log) (cadr e)) t)
			  (sdiff (caddr e) x))))))

(defun diff%deriv (e)
  (let (derivflag)
    (simplifya (cons '(%derivative) e) t)))


;; grad properties

(let ((header '(x)))
  (mapc #'(lambda (z) (putprop (car z) (cons header (cdr z)) 'grad))
	;; All these GRAD templates have been simplified and then the SIMP flags
	;;	 (which are unnecessary) have been removed to save core space.
	'((%log ((mexpt) x -1)) (%plog ((mexpt) x -1))
	  (%gamma ((mtimes) ((mqapply) (($psi array) 0) x) ((%gamma) x)))
	  (mfactorial ((mtimes) ((mqapply) (($psi array) 0) ((mplus) 1 x)) ((mfactorial) x)))
	  (%sin ((%cos) x))
	  (%cos ((mtimes) -1 ((%sin) x)))
	  (%tan ((mexpt) ((%sec) x) 2))
	  (%cot ((mtimes) -1 ((mexpt) ((%csc) x) 2)))
	  (%sec ((mtimes) ((%sec) x) ((%tan) x)))
	  (%csc ((mtimes) -1 ((%cot) x) ((%csc) x)))
	  (%asin ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) x 2))) ((rat) -1 2)))
	  (%acos ((mtimes) -1 ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) x 2)))
			       ((rat) -1 2))))
	  (%atan ((mexpt) ((mplus) 1 ((mexpt) x 2)) -1))
	  (%acot ((mtimes) -1 ((mexpt) ((mplus) 1 ((mexpt) x 2)) -1)))
	  (%acsc ((mtimes) -1
		  ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) x -2)))
		   ((rat) -1 2))
		  ((mexpt) x -2)))
	  (%asec ((mtimes)
		  ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) x -2)))
		   ((rat) -1 2))
		  ((mexpt) x -2)))
	  (%sinh ((%cosh) x))
	  (%cosh ((%sinh) x))
	  (%tanh ((mexpt) ((%sech) x) 2))
	  (%coth ((mtimes) -1 ((mexpt) ((%csch) x) 2)))
	  (%sech ((mtimes) -1 ((%sech) x) ((%tanh) x)))
	  (%csch ((mtimes) -1 ((%coth) x) ((%csch) x)))
	  (%asinh ((mexpt) ((mplus) 1 ((mexpt) x 2)) ((rat) -1 2)))
	  (%acosh ((mexpt) ((mplus) -1 ((mexpt) x 2)) ((rat) -1 2)))
	  (%atanh ((mexpt) ((mplus) 1 ((mtimes) -1 ((mexpt) x 2))) -1))
	  (%acoth ((mtimes) -1 ((mexpt) ((mplus) -1 ((mexpt) x 2)) -1)))
	  (%asech ((mtimes) -1
		   ((mexpt) ((mplus) -1 ((mexpt) x -2)) ((rat) -1 2))
		   ((mexpt) x -2)))
	  (%acsch ((mtimes) -1
		   ((mexpt) ((mplus) 1 ((mexpt) x -2)) ((rat) -1 2))
		   ((mexpt) x -2)))
	  (mabs ((mtimes) x ((mexpt) ((mabs) x) -1)))
	  (%erf ((mtimes) 2 ((mexpt) $%pi ((rat) -1 2))
		 ((mexpt) $%e ((mtimes) -1 ((mexpt) x 2)))))
	  )))

(defprop $atan2 ((x y) ((mtimes) y ((mexpt) ((mplus) ((mexpt) x 2) ((mexpt) y 2)) -1))
		 ((mtimes) -1 x ((mexpt) ((mplus) ((mexpt) x 2) ((mexpt) y 2)) -1)))
  grad)

(defprop $li 
  ((n x)
; Do not put a noun form on the property list, but NIL.
; SDIFFGRAD generates the noun form.
;   ((%derivative) ((mqapply) (($li array) n) x) n 1)
   nil
   ((mtimes) ((mqapply) (($li array) ((mplus) -1 n)) x) ((mexpt) x -1)))
  grad)

(defprop $psi 
  ((n x)
; Do not put a noun form on the property list, but NIL.
; SDIFFGRAD generates the noun form.
   nil
   ((mqapply) (($psi array) ((mplus) 1 n)) x))
  grad)

(defmfun atvarschk (argl)
  (do ((largl (length argl) (1- largl))
       (latvrs (length atvars))
       (l))
      ((not (< latvrs largl)) (nconc atvars l))
    (setq l (cons (implode (cons '$ (cons '@ (mexploden largl)))) l))))

(defmfun notloreq (x)
  (or (atom x)
      (not (member (caar x) '(mlist mequal) :test #'eq))
      (and (eq (caar x) 'mlist)
	   (dolist (u (cdr x))
	     (if (not (mequalp u)) (return t))))))

(defmfun substitutel (l1 l2 e)
  "l1 is a list of expressions.  l2 is a list of variables. For each 
   element in list l2, substitute corresponding element of l1 into e"
  (do ((l1 l1 (cdr l1))
       (l2 l2 (cdr l2)))
      ((null l1) e)
    (setq e (maxima-substitute (car l1) (car l2) e))))

(defmfun union* (a b)
  (do ((a a (cdr a))
       (x b))
      ((null a) x)
    (if (not (memalike (car a) b)) (setq x (cons (car a) x)))))

(defmfun intersect* (a b)
  (do ((a a (cdr a))
       (x))
      ((null a) x)
    (if (memalike (car a) b) (setq x (cons (car a) x)))))

(defmfun nthelem (n e)
  (car (nthcdr (1- n) e)))

(defmfun delsimp (e)
  (delete 'simp (copy-list e) :count 1 :test #'eq))

(defmfun remsimp (e)
  (if (atom e) e (cons (delsimp (car e)) (mapcar #'remsimp (cdr e)))))

(defmfun $trunc (e)
  (cond ((atom e) e)
	((eq (caar e) 'mplus) (cons (append (car e) '(trunc)) (cdr e)))
	((mbagp e) (cons (car e) (mapcar #'$trunc (cdr e))))
	((specrepp e) ($trunc (specdisrep e)))
	(t e)))

(defmfun nonvarcheck (e fn)
  (if (or (mnump e)
	  (maxima-integerp e)
	  (and (not (atom e)) (not (eq (caar e) 'mqapply)) (mopp1 (caar e))))
      (merror (intl:gettext "~:M: second argument must be a variable; found ~M") fn e)))

(defmspec $ldisplay (form)
  (disp1 (cdr form) t t))

(defmfun $ldisp (&rest args)
  #-gcl
  (declare (dynamic-extent args))
  (disp1 args t nil))

(defmspec $display (form)
  (disp1 (cdr form) nil t))

(defmfun $disp (&rest args)
  #-gcl
  (declare (dynamic-extent args))
  (disp1 args nil nil))

(defun disp1 (ll lablist eqnsp)
  (if lablist (setq lablist (cons '(mlist simp) nil)))
  (do ((ll ll (cdr ll))
       (l)
       (ans)
       ($dispflag t)
       (tim 0))
      ((null ll) (or lablist '$done))
    (setq l (car ll) ans (if eqnsp (meval l) l))
    (if (and eqnsp (not (mequalp ans)))
	(setq ans (list '(mequal simp) (disp2 l) ans)))
    (if lablist (nconc lablist (cons (elabel ans) nil)))
    (setq tim (get-internal-run-time))
    (let ((*display-labels-p* nil))
      (declare (special *display-labels-p*))
      (displa (list '(mlabel) (if lablist *linelabel*) ans)))
    (mterpri)
    (timeorg tim)))

(defun disp2 (e)
  (cond ((atom e) e)
	((eq (caar e) 'mqapply)
	 (cons '(mqapply) (cons (cons (caadr e) (mapcar #'meval (cdadr e)))
				(mapcar #'meval (cddr e)))))
	((eq (caar e) 'msetq) (disp2 (cadr e)))
	((eq (caar e) 'mset) (disp2 (meval (cadr e))))
	((eq (caar e) 'mlist) (cons (car e) (mapcar #'disp2 (cdr e))))
	((mspecfunp (caar e)) e)
	(t (cons (car e) (mapcar #'meval (cdr e))))))

; Construct a new intermediate result label,
; and bind it to the expression e.
; The global flag $NOLABELS is ignored; the label is always bound.
; Otherwise (if ELABEL were to observe $NOLABELS) it would be
; impossible to programmatically refer to intermediate result expression.

(defmfun elabel (e)
  (if (not (checklabel $linechar)) (setq $linenum (1+ $linenum)))
  (let (($nolabels nil)) ; <-- This is pretty ugly. MAKELABEL should take another argument.
    (makelabel $linechar))
  (setf (symbol-value *linelabel*) e)
  *linelabel*)

(defmfun $dispterms (e)
  (cond ((or (atom e) (eq (caar e) 'bigfloat)) (displa e))
	((specrepp e) ($dispterms (specdisrep e)))
	(t (let (($dispflag t))
	     (mterpri)
	     (displa (getop (mop e)))
	     (do ((e (if (and (eq (caar e) 'mplus) (not $powerdisp))
			 (reverse (cdr e))
			 (margs e))
		     (cdr e))) ((null e)) (mterpri) (displa (car e)) (mterpri)))
	   (mterpri)))
  '$done)

(defmfun $dispform (e &optional (flag nil flag?))
  (when (and flag? (not (eq flag '$all)))
    (merror (intl:gettext "dispform: second argument, if present, must be 'all'; found ~M") flag))
  (if (or (atom e)
	  (atom (setq e (if flag? (nformat-all e) (nformat e))))
	  (member 'simp (cdar e) :test #'eq))
      e
      (cons (cons (caar e) (cons 'simp (cdar e)))
	    (if (and (eq (caar e) 'mplus) (not $powerdisp))
		(reverse (cdr e))
		(cdr e)))))

;;; These functions implement the Macsyma functions $op and $operatorp.
;;; Dan Stanger
(defmfun $op (expr)
  ($part expr 0))

(defmfun $operatorp (expr oplist)
  (if ($listp oplist)
      ($member ($op expr) oplist)
      (equal ($op expr) oplist)))

(defmfun $part (&rest args)
  #-gcl
  (declare (dynamic-extent args))
  (mpart args nil nil $inflag '$part))

(defmfun $inpart (&rest args)
  #-gcl
  (declare (dynamic-extent args))
  (mpart args nil nil t '$inpart))

(defmspec $substpart (l)
  (let ((substp t))
    (mpart (cdr l) t nil $inflag '$substpart)))

(defmspec $substinpart (l)
  (let ((substp t))
    (mpart (cdr l) t nil t '$substinpart)))

(defmfun part1 (arglist substflag dispflag inflag) ; called only by TRANSLATE
  (let ((substp t))
    (mpart arglist substflag dispflag inflag '$substpart)))

(defmfun mpart (arglist substflag dispflag inflag fn)
  (prog (substitem arg arg1 exp exp1 exp* sevlist count prevcount n specp
	 lastelem lastcount)
     (setq specp (or substflag dispflag))
     (if substflag (setq substitem (car arglist) arglist (cdr arglist)))
     (if (null arglist) (wna-err '$part))
     (setq exp (if substflag (meval (car arglist)) (car arglist)))
     (when (null (setq arglist (cdr arglist)))
       (setq $piece exp)
       (return (cond (substflag (meval substitem))
		     (dispflag (box exp dispflag))
		     (t exp))))
     (cond ((not inflag)
	    (cond ((or (and ($listp exp) (null (cdr arglist)))
		       (and ($matrixp exp)
			    (or (null (cdr arglist)) (null (cddr arglist)))))
		   (setq inflag t))
		  ((not specp) (setq exp (nformat exp)))
		  (t (setq exp (nformat-all exp)))))
	   ((specrepp exp) (setq exp (specdisrep exp))))
     (when (and (atom exp) (null $partswitch))
       (merror (intl:gettext "~:M: argument must be a non-atomic expression; found ~:M") fn exp))
     (when (and inflag specp)
       (setq exp (copy-tree exp)))
     (when substflag
       ;; Replace all ocurrences of 'rat with 'mquotient when in subst.
       (setq exp (let (($simp nil)) (maxima-substitute 'mquotient 'rat exp))))
     (setq exp* exp)
     start (cond ((or (atom exp) (eq (caar exp) 'bigfloat)) (go err))
		 ((equal (setq arg (if substflag (meval (car arglist)) (car arglist)))
			 0)
		  (setq arglist (cdr arglist))
		  (cond ((mnump substitem)
			 (merror (intl:gettext "~:M: argument cannot be a number; found ~M") fn substitem))
			((and specp arglist)
			 (if (eq (caar exp) 'mqapply)
			     (prog2 (setq exp (cadr exp)) (go start))
                 ;; NOT CLEAR WHAT IS INVALID HERE. OH WELL.
			     (merror (intl:gettext "~:M: invalid operator.") fn)))
			(t (setq $piece (getop (mop exp)))
			   (return
			     (cond (substflag
				    (setq substitem (getopr (meval substitem)))
				    (cond ((mnump substitem)
					   (merror (intl:gettext "~:M: argument cannot be a number; found ~M") fn substitem))
					  ((not (atom substitem))
					   (if (not (eq (caar exp) 'mqapply))
					       (rplaca (rplacd exp (cons (car exp)
									 (cdr exp)))
						       '(mqapply)))
					   (rplaca (cdr exp) substitem)
					   (return (resimplify exp*)))
					  ((eq (caar exp) 'mqapply)
					   (rplacd exp (cddr exp))))
				    (rplaca exp (cons substitem
						      (if (and (member 'array (cdar exp) :test #'eq)
							       (not (mopp substitem)))
							  '(array))))
				    (resimplify exp*))
				   (dispflag
				    (rplacd exp (cdr (box (copy-tree exp) dispflag)))
				    (rplaca exp (if (eq dispflag t)
						    '(mbox)
						    '(mlabox)))
				    (resimplify exp*))
				   (t (when arglist (setq exp $piece) (go a))
				      $piece))))))
		 ((not (atom arg)) (go several))
		 ((not (fixnump arg))
		  (merror (intl:gettext "~:M: argument must be an integer; found ~M") fn arg))
		 ((< arg 0) (go bad)))
     (if (eq (caar exp) 'mqapply) (setq exp (cdr exp)))
     loop (cond ((not (zerop arg)) (setq arg (1- arg) exp (cdr exp))
		 (if (null exp) (go err))
		 (go loop))
		((null (setq arglist (cdr arglist)))
		 (return (cond (substflag (setq $piece (resimplify (car exp)))
					  (rplaca exp (meval substitem))
					  (resimplify exp*))
			       (dispflag (setq $piece (resimplify (car exp)))
					 (rplaca exp (box (car exp) dispflag))
					 (resimplify exp*))
			       (inflag (setq $piece (car exp)))
			       (t (setq $piece (simplify (car exp))))))))
     (setq exp (car exp))
     a    (cond ((and (not inflag) (not specp)) (setq exp (nformat exp)))
		((specrepp exp) (setq exp (specdisrep exp))))
     (go start)
     err  (cond ((eq $partswitch 'mapply)
		 (merror (intl:gettext "part: invalid index of list or matrix.")))
		($partswitch (return (setq $piece '$end)))
		(t (merror (intl:gettext "~:M: fell off the end.") fn)))
     bad  (improper-arg-err arg fn)
     several
     (if (or (not (member (caar arg) '(mlist $allbut) :test #'eq)) (cdr arglist))
	 (go bad))
     (setq exp1 (cons (caar exp) (if (member 'array (cdar exp) :test #'eq) '(array))))
     (if (eq (caar exp) 'mqapply)
	 (setq sevlist (list (cadr exp) exp1) exp (cddr exp))
	 (setq sevlist (ncons exp1) exp (cdr exp)))
     (setq arg1 (cdr arg) prevcount 0 exp1 exp)
     (dolist (arg* arg1)
       (if (not (fixnump arg*))
	   (merror (intl:gettext "~:M: argument must be an integer; found ~M") fn arg*)))
     (when (and specp (eq (caar arg) 'mlist))
       (if substflag (setq lastelem (car (last arg1))))
       (setq arg1 (sort (copy-list arg1) #'<)))
     (when (eq (caar arg) '$allbut)
       (setq n (length exp))
       (dolist (i arg1)
	 (if (or (< i 1) (> i n))
	     (merror (intl:gettext "~:M: index must be in range 1 to ~M, inclusive; found ~M") fn n i)))
       (do ((i n (1- i)) (arg2))
	   ((= i 0) (setq arg1 arg2))
	 (if (not (member i arg1 :test #'equal)) (setq arg2 (cons i arg2))))
       (if substflag (setq lastelem (car (last arg1)))))
     (if (null arg1) (if specp (go bad) (go end)))
     (if substflag (setq lastcount lastelem))
     sevloop
     (if specp
	 (setq count (- (car arg1) prevcount) prevcount (car arg1))
	 (setq count (car arg1)))
     (if (< count 1) (go bad))
     (if (and substflag (< (car arg1) lastelem))
	 (setq lastcount (1- lastcount)))
     count(cond ((null exp) (go err))
		((not (= count 1)) (setq count (1- count) exp (cdr exp)) (go count)))
     (setq sevlist (cons (car exp) sevlist))
     (setq arg1 (cdr arg1))
     end  (cond ((null arg1)
		 (setq sevlist (nreverse sevlist))
		 (setq $piece (if (or inflag (not specp))
				  (simplify sevlist)
				  (resimplify sevlist)))
		 (return (cond (substflag (rplaca (nthcdr (1- lastcount) exp1)
						  (meval substitem))
					  (resimplify exp*))
			       (dispflag (rplaca exp (box (car exp) dispflag))
					 (resimplify exp*))
			       (t $piece))))
		(substflag (if (null (cdr exp)) (go err))
			   (rplaca exp (cadr exp)) (rplacd exp (cddr exp)))
		(dispflag (rplaca exp (box (car exp) dispflag))
			  (setq exp (cdr exp)))
		(t (setq exp exp1)))
     (go sevloop)))

(defmfun getop (x)
  (or (and (symbolp x) (get x 'op)) x))

(defmfun $listp (x)
  (and (not (atom x))
       (not (atom (car x)))
       (eq (caar x) 'mlist)))

(defmfun $cons (x e)
  (atomchk (setq e (specrepcheck e)) '$cons t)
  (mcons-exp-args e (cons x (margs e))))

(defmfun $endcons (x e)
  (atomchk (setq e (specrepcheck e)) '$endcons t)
  (mcons-exp-args e (append (margs e) (ncons x))))

(defmfun $reverse (e)
  (atomchk (setq e (format1 e)) '$reverse nil)
  (mcons-exp-args e (reverse (margs e))))

(defmfun $append (&rest args)
  (if (null args)
      '((mlist simp))
      (let ((arg1 (specrepcheck (first args))) op arrp)
	(atomchk arg1 '$append nil)
	(setq op (mop arg1)
	      arrp (if (member 'array (cdar arg1) :test #'eq) t))
	(mcons-exp-args
	 arg1
	 (apply #'append
		(mapcar #'(lambda (u)
			    (atomchk (setq u (specrepcheck u)) '$append nil)
			    (unless (and (alike1 op (mop u))
					 (eq arrp (if (member 'array (cdar u) :test #'eq) t)))
			      (merror (intl:gettext "append: operators of arguments must all be the same.")))
			    (margs u))
			args))))))

(defun mcons-exp-args (e args)
  (if (eq (caar e) 'mqapply)
      (list* (delsimp (car e)) (cadr e) args)
      (cons (if (eq (caar e) 'mlist) (car e) (delsimp (car e))) args)))

(defmfun $member (x e)
  (atomchk (setq e ($totaldisrep e)) '$member t)
  (if (memalike ($totaldisrep x) (margs e)) t))

(defmfun atomchk (e fun 2ndp)
  (if (or (atom e) (eq (caar e) 'bigfloat))
      (merror (intl:gettext "~:M: ~Margument must be a non-atomic expression; found ~M") fun (if 2ndp "2nd " "") e)))

(defmfun format1 (e)
  (cond (($listp e) e)
	($inflag (specrepcheck e))
	(t (nformat e))))

(defmfun $first (e)
  (atomchk (setq e (format1 e)) '$first nil)
  (if (null (cdr e)) (merror (intl:gettext "first: empty argument.")))
  (car (margs e)))

;; This macro is used to create functions second thru tenth.
;; Rather than try to modify mformat for ~:R, use the quoted symbol

(macrolet ((make-nth (si i)
	     (let ((sim (intern (concatenate 'string "$" (symbol-name si)))))
	       `(defmfun ,sim (e)
		  (atomchk (setq e (format1 e)) ',sim nil)
		  (if (< (length (margs e)) ,i)
		      (merror (intl:gettext "~:M: no such element in ~M") ',sim e))
		  (,si (margs e))))))

  (make-nth second  2)
  (make-nth third   3)
  (make-nth fourth  4)
  (make-nth fifth   5)
  (make-nth sixth   6)
  (make-nth seventh 7)
  (make-nth eighth  8)
  (make-nth ninth   9)
  (make-nth tenth  10))

(defmfun $rest (e &optional (n 1 n?))
  (prog (m fun fun1 revp)
     (when (and n? (equal n 0))
       (return e))
     (atomchk (setq m (format1 e)) '$rest nil)
     (cond ((and n? (not (fixnump n)))
	    (merror (intl:gettext "rest: second argument, if present, must be an integer; found ~M") n))
	   ((minusp n)
	    (setq n (- n) revp t)))
     (if (< (length (margs m)) n)
	 (if $partswitch
	     (return '$end)
	     (merror (intl:gettext "rest: fell off the end."))))
     (setq fun (car m))
     (when (eq (car fun) 'mqapply)
       (setq fun1 (cadr m)
	     m (cdr m)))
     (setq m (cdr m))
     (when revp (setq m (reverse m)))
     (setq m (nthcdr n m))
     (setq m (cons (if (eq (car fun) 'mlist) fun (delsimp fun))
		   (if revp (nreverse m) m)))
     (when (eq (car fun) 'mqapply)
       (return (cons (car m) (cons fun1 (cdr m)))))
     (return m)))

(defmfun $last (e)
  (atomchk (setq e (format1 e)) '$last nil)
  (when (null (cdr e))
    (merror (intl:gettext "last: empty argument.")))
  (car (last e)))

(defmfun $args (e)
  (atomchk (setq e (format1 e)) '$args nil)
	 (cons '(mlist) (margs e)))

(defmfun $delete (x l &optional (n -1 n?))
  (when (and n? (or (not (fixnump n)) (minusp n))) ; if n is set, it must be a nonneg fixnum
    (merror (intl:gettext "delete: third argument, if present, must be a nonnegative integer; found ~M") n))
  (atomchk (setq l (specrepcheck l)) '$delete t)
  (setq x (specrepcheck x)
	l (cons (delsimp (car l)) (copy-list (cdr l))))
  (do ((l1 (if (eq (caar l) 'mqapply) (cdr l) l)))
      ((or (null (cdr l1)) (zerop n)) l)
    (if (alike1 x (specrepcheck (cadr l1)))
	(progn
	  (decf n)
	  (rplacd l1 (cddr l1)))
	(setq l1 (cdr l1)))))

(defmfun $length (e)
  (setq e (cond (($listp e) e)
		((or $inflag (not ($ratp e))) (specrepcheck e))
		(t ($ratdisrep e))))
  (cond ((symbolp e) (merror (intl:gettext "length: argument cannot be a symbol; found ~:M") e))
	((or (numberp e) (eq (caar e) 'bigfloat))
	 (if (and (not $inflag) (mnegp e))
	     1
	     (merror (intl:gettext "length: argument cannot be a number; found ~:M") e)))
	((or $inflag (not (member (caar e) '(mtimes mexpt) :test #'eq))) (length (margs e)))
	((eq (caar e) 'mexpt)
	 (if (and (alike1 (caddr e) '((rat simp) 1 2)) $sqrtdispflag) 1 2))
	(t (length (cdr (nformat e))))))

(defmfun $atom (x)
  (setq x (specrepcheck x)) (or (atom x) (eq (caar x) 'bigfloat)))

(defmfun $symbolp (x)
  (setq x (specrepcheck x)) (symbolp x))

(defmfun $num (e)
  (let (x)
    (cond ((atom e) e)
	  ((eq (caar e) 'mrat) ($ratnumer e))
	  ((eq (caar e) 'rat) (cadr e))
	  ((eq (caar (setq x (nformat e))) 'mquotient) (simplify (cadr x)))
	  ((and (eq (caar x) 'mminus) (not (atom (setq x (cadr x))))
		(eq (caar x) 'mquotient))
	   (simplify (list '(mtimes) -1 (cadr x))))
	  (t e))))

(defmfun $denom (e)
  (cond ((atom e) 1)
	((eq (caar e) 'mrat) ($ratdenom e))
	((eq (caar e) 'rat) (caddr e))
	((or (eq (caar (setq e (nformat e))) 'mquotient)
	     (and (eq (caar e) 'mminus) (not (atom (setq e (cadr e))))
		  (eq (caar e) 'mquotient)))
	 (simplify (caddr e)))
	(t 1)))

(defmfun $entier (e) (take '($floor) e))

(defmfun $fix (e) (take '($floor) e))

;; Evaluate THUNK ignoring any error that might occur.  If the THUNK
;; returns a good number (not infinity or NaN), then return the
;; number.  Otherwise, return NIL to indicate that we the computation
;; failed.  This is a pretty brute-force approach.
(defun try-float-computation (thunk)
  (let ((errcatch (cons bindlist loclist))
	(*mdebug* nil))
    (let ((result (errset (funcall thunk))))
      (labels ((bad-number-p (num)
		 (if (complexp num)
		     (or (bad-number-p (realpart num))
			 (bad-number-p (imagpart num)))
		     (or (float-inf-p num)
			 (float-nan-p num)))))
	(if (and result (bad-number-p (car result)))
	    nil
	    (car result))))))
	    
(defmfun $float (e)
  (cond ((numberp e) (float e))
	((and (symbolp e) (mget e '$numer)))
	((or (atom e) (member 'array (cdar e) :test #'eq)) e)
	((eq (caar e) 'rat) (fpcofrat e))
	((eq (caar e) 'bigfloat) (fp2flo e))
	((member (caar e) '(mexpt mncexpt) :test #'eq)
	 ;; avoid x^2 -> x^2.0, allow %e^%pi -> 23.14
	 (let ((res (recur-apply #'$float e)))
	   (if (floatp res)
	       res
	       (list (ncons (caar e)) ($float (cadr e)) (caddr e)))))
	((and (eq (caar e) '%log)
	      (complex-number-p (second e) '$ratnump))
	 ;; Basically we try to compute float(log(x)) as directly as
	 ;; possible, expecting Lisp to return some error if it can't.
	 ;; Then we do a more complicated approach to compute the
	 ;; result.  However, gcl and ecl don't signal errors in these
	 ;; cases, so we always use the complicated approach for these lisps.
	 (let ((n (second e)))
	   (cond ((integerp n)
		  ;; float(log(int)).  First try to compute (log
		  ;; (float n)).  If that works, we're done.
		  ;; Otherwise we need to do more.  
		  (to (or (try-float-computation #'(lambda ()
						     (log (float n))))
			  (let ((m (integer-length n)))
			    ;; Write n as (n/2^m)*2^m where m is the number of
			    ;; bits in n.  Then log(n) = log(2^m) + log(n/2^m).
			    ;; n/2^m is approximately 1, so converting that to a
			    ;; float is no problem.  log(2^m) = m * log(2).
			    (+ (* m (log 2e0))
			       (log (float (/ n (ash 1 m)))))))))
		 (($ratnump n)
		  ;; float(log(n/m)) where n and m are integers.  Try computing
		  ;; it first.  If it fails, compute as log(n) - log(m).
		  (let ((try (try-float-computation #'(lambda() 
							(log (fpcofrat n))))))
		    (if try
			(to try)
			(sub  ($float `((%log) ,(second n)))
			      ($float `((%log) ,(third n)))))))
		 ((complex-number-p n 'integerp)
		  ;; float(log(n+m*%i)).
		  (let ((re ($realpart n))
			(im ($imagpart n)))
		    (to (or (try-float-computation #'(lambda ()
						       (log (complex (float re)
								     (float im)))))
			    (let* ((size (max (integer-length re)
					      (integer-length im)))
				   (scale (ash 1 size)))
			      (+ (* size (log 2e0))
				 (log (complex (float (/ re scale))
					       (float (/ im scale))))))))))
		 (t
		  ;; log(n1/d1 + n2/d2*%i) =
		  ;;   log(s*(n+m*%i)) = log(s) + log(n+m*%i)
		  ;;
		  ;; where s = lcm(d1, d2), n and m are integers
		  ;;
		  (let* ((s (lcm ($denom ($realpart n))
				 ($denom ($imagpart n))))
			 (p ($expand (mul s n))))
		    (add ($float `((%log) ,s))
			 ($float `((%log) ,p))))))))
	((and (eq (caar e) '%erf)
	      (eq (second e) '$%i))
	 ;; Handle like erf(%i).  float(%i) (via recur-apply below)
	 ;; just returns %i, so we never numerically evaluate it.
	 (complexify (complex-erf (complex 0 1d0))))
	(t (recur-apply #'$float e))))

(defmfun $coeff (e x &optional (n 1))
  (if (equal n 0)
      (coeff e x 0)
      (coeff e (power x n) 1)))

(defun coeff (e var pow)
  (simplify
   (cond ((alike1 e var) (if (equal pow 1) 1 0))
	 ((atom e) (if (equal pow 0) e 0))
	 ((eq (caar e) 'mexpt)
	  (cond ((alike1 (cadr e) var)
		 (if (or (equal pow 0) (not (alike1 (caddr e) pow))) 0 1))
		((equal pow 0) e)
		(t 0)))
	 ((or (eq (caar e) 'mplus) (mbagp e))
	  (cons (if (eq (caar e) 'mplus) '(mplus) (car e))
		(mapcar #'(lambda (e) (coeff e var pow)) (cdr e))))
	 ((eq (caar e) 'mrat) (ratcoeff e var pow))
         ((equal pow 0) (if (coeff-contains-powers e var) 0 e))
	 ((eq (caar e) 'mtimes)
	  (let ((term (if (equal pow 1) var (power var pow))))
	    (if (memalike term (cdr e)) ($delete term e 1) 0)))
	 (t 0))))

(defun coeff-contains-powers (e var)
  (cond ((alike1 e var) t)
        ((atom e) nil)
        ((eq (caar e) 'mexpt)
         (alike1 (cadr e) var))
        ((eq (caar e) 'mtimes)
         (member t (mapcar #'(lambda (e)
                               (coeff-contains-powers e var)) (cdr e))))
        (t nil)))

(let (my-powers my-num my-flag)
  (declare (special my-powers my-num my-flag))

  (defmfun $hipow (e var)
    (findpowers e t var))
  
  ;; These work best on expanded "simple" expressions.
  
  (defmfun $lopow (e var)
    (findpowers e nil var))
  
  (defun findpowers (e hiflg var)
    (let (my-powers my-num my-flag)
      (declare (special my-powers my-num my-flag))
      (findpowers1 e hiflg var)
      (cond ((null my-powers) (if (null my-num) 0 my-num))
  	  (t (when my-num (setq my-powers (cons my-num my-powers)))
  	     (maximin my-powers (if hiflg '$max '$min))))))
  
  (defun findpowers1 (e hiflg var)
    (cond ((alike1 e var) (checkpow 1 hiflg))
  	((atom e))
  	((eq (caar e) 'mplus)
  	 (cond ((not (freel (cdr e) var))
  		(do ((e (cdr e) (cdr e))) ((null e))
  		  (setq my-flag nil) (findpowers1 (car e) hiflg var)
  		  (if (null my-flag) (checkpow 0 hiflg))))))
  	((and (eq (caar e) 'mexpt) (alike1 (cadr e) var)) (checkpow (caddr e) hiflg))
  	((specrepp e) (findpowers1 (specdisrep e) hiflg var))
  	(t (mapc #'(lambda (x) (findpowers1 x hiflg var)) (cdr e)))))
  
  (defun checkpow (pow hiflg)
    (setq my-flag t)
    (cond ((not (numberp pow)) (setq my-powers (cons pow my-powers)))
  	((null my-num) (setq my-num pow))
  	(hiflg (if (> pow my-num) (setq my-num pow)))
  	((< pow my-num) (setq my-num pow)))))
