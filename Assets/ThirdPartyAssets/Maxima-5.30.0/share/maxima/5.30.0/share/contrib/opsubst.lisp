#|
  Copyright 2006 by Barton Willis

  This is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License,
  http://www.gnu.org/copyleft/gpl.html.

 This software has NO WARRANTY, not even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Usage: The function 'opsubst' is similar to the function 'subst', except that
'opsubst' only makes substitutions for the operators in an expression. Specifically,

 opsubst(f,g,e) --> When 'f' is an operator in the expression e, substitute 'g' 
                    for 'f' in the expression 'e'. 
 opsubst(g=f,e) --> opsubst(f,g,e).
 opsubst([],e) --> e.
 opsubst([g1=f1, g2=f2, ..., gn=fn],e) --> opsubst([g2=f2,...,gn=fn], opsubst(f1=f1, e)).

 Examples:

(%i1) opsubst(f,g,g(g(x)));
(%o1) f(f(x))
(%i2) opsubst(f,g,g(g));
(%o2) f(g)
(%i3) opsubst(f,g[x],g[x](z));
(%o3) f(z)
(%i4) opsubst(g[x],f, f(z));
(%o4) g[x](z)
(%i5) opsubst(tan, sin, sin(sin));
(%o5) tan(SIN)
(%i6) opsubst([f=g,g=h],f(x));
(%o6) h(x)

To determine the operator, 'opsubst' sets 'inflag' to true. This means
'opsubst' substitutes for the internal, not the displayed, operator.
Since Maxima does not internally use the unary negation or division
operators, substituting for these operators will not work; examples:

(%i1) opsubst("+","-",a-b);
(%o1) a-b
(%i2) opsubst("f","-",-a);
(%o2) -a
(%i3) opsubst("^^","/",a/b);
(%o3) a/b

The internal representation of -a*b is *(-1,a,b); thus

(%i4) opsubst("[","*", -a*b);
(%o4) [-1,a,b]

If opsubst did not locally set 'inflag' to true, we'd have:

(%i1) opsubst("[","*", -a*b), listarith : true;
(%o1) [-a,-b]
(%i2) opsubst("[","*", -a*b), listarith : false;
(%o2) -[a,b]

So opsubst("*","[", opsubst("[","*", -a*b)) # -a*b. There is
nothing wrong with this; however, With 'inflag' set to true, 
we have (regardless of the value of listarith)

(%i1) opsubst("[","*", -a*b);
(%o1) [-1,a,b]
(%i2) opsubst("*","[",%);
(%o2) -a*b

To me, it seems that it is better to substitute for the internal
rather than the displayed operator. But do not be mislead by this
example, the equation
 
   opsubst(f,g,opsubst(g,f,e)) = e

is not an identity.

When either the first or second arguments of 'opsubst' are not Maxima
symbols, generally some other function will signal an error; for
example

(%i5) opsubst(a+b,f, f(x));
Improper name or value in functional position:b+a

However, the first two arguments to 'opsubst' can be 
subscripted:

(%i6) opsubst(g[5],f, f(x));
(%o6) g[5](x)

|#

;; Applies op to args and simplifies the result. The function my-take isn't supposed
;; to evaluate args. I think the maxima 'take' macro doesn't handle subscripted 
;; operators correctly--this function my-take should be OK with subscripted operators.
;; (Also the take macro special-cases a few operators for the simplification function. Yeech.)

(defun my-take (op args)
  (simplify (if (and (consp op) (member 'array (car op))) `((mqapply) ,op ,@args) `((,op) ,@args))))

(defun $opsubst (&rest q)
  (let ((e))
    (cond ((= 3 (length q)) (apply 'op-subst q))
	  ((= 2 (length q))
	   (setq e (second q))
	   (setq q (if ($listp (first q)) (margs (first q)) (list (first q))))
	   (dolist (qi q e)
	     (if (op-equalp qi 'mequal) (setq e (op-subst ($rhs qi) ($lhs qi) e))
	       (merror "Expected an expression of the form `a = b'; instead found ~:M" qi))))
	  (t (wna-err '$opsubst)))))

;; If op is a string, verbify it; otherwise, return op. Without this transformation,
;; things like opsubst("[",f, f(a,b,c)) would fail. Notice that subst(f[1] = "[", f[1](1,2,3))
;; doesn't work correctly.

(defun verbify-string (op)
  (if (stringp op) ($verbify op) op))

;; If op is a symbol, verbify it; otherwise, return op.

(defun safe-verbify (op)
  (if (symbolp op) ($verbify op) op))

(defun op-subst (f g e)
  (setq f (verbify-string f))
  (setq g (verbify-string g))

  (let (($inflag t))
    (if ($mapatom e) e
      (my-take (if (like (safe-verbify g) (safe-verbify (mop e))) f (mop e)) 
	       (mapcar #'(lambda (s) (op-subst f g s)) (margs ($args e)))))))

;; If prd(e) evaluates to true, do the substitution opsubst(id, e). The
;; first argument should be an equation of the form symbol = symbol | lambda form
;; or a list of such equations.

(defun $opsubstif (id prd e)
  (setq id (if ($listp id) (margs id) (list id)))
  (dolist (qi id)
    (if (op-equalp qi 'mequal) (setq e (op-subst-if (verbify-string ($rhs qi))
						    (verbify-string ($lhs qi)) prd e))
      (merror "Expected an expression of the form `a = b'; instead found ~:M" qi)))
  e)
        	  
(defun op-subst-if (fn fo prd e)
  (let (($inflag t) ($prederror nil))
    (cond (($mapatom e) e)
	  (t
	   (my-take (if (and (like (safe-verbify fo) (safe-verbify (mop e)))
			     (eq t (mevalp (mfuncall prd ($args e))))) fn (mop e))
		    (mapcar #'(lambda (s) (op-subst-if fn fo prd s)) (margs ($args e))))))))
	  	   	  
;; Return a list of all the arguments to the operator 'op.' Each argument is
;; a list (what 'args' would return).  Examples:

;; (%i1) gatherargs(f(x) + f(y),'f);
;; (%o1) [[x],[y]]

;; In the expression 42 + f(f(x)), both x and f(x) are arguments to f; thus

;; (%i2) gatherargs(42 + f(f(x)),'f);
;; (%o2) [[f(x)],[x]]

;; (%i3) gatherargs(f^2 + %pi,'f);
;; (%o3) []

	   
(defun $gatherargs (e op)
  `((mlist) ,@(gatherargs e op)))

(defun gatherargs (e op)
  (if ($mapatom e) nil
    (append (if (op-equalp e op ($nounify op) ($verbify op)) `(((mlist) ,@(margs e))))
	    (mapcan #'(lambda (s) (gatherargs s op)) (margs e)))))
	 	  
(defun $gatherops (e)
  ($setify `((mlist) ,@(gatherops e))))

(defun gatherops (e)
  (if ($mapatom e) nil (cons ($op e) (mapcan #'gatherops (margs e)))))



  
