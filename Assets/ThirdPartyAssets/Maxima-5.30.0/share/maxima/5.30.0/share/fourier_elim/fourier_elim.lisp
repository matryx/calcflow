#| Copyright 2007, 2008 by Barton Willis

  This is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License,
  http://www.gnu.org/copyleft/gpl.html.

 This software has NO WARRANTY, not even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
|#


($load '$polynomialp)
(if (not ($get '$to_poly '$version)) ($load '$to_poly))

(mfuncall '$declare '$one_to_one '$feature)
(mfuncall '$declare '$sinh '$one_to_one)
(mfuncall '$declare '$log  '$one_to_one)
(mfuncall '$declare '$tanh '$one_to_one)
(mfuncall '$declare '$log '$increasing)
  
;; The macro opcons is defined elsewhere; I don't know if opapply is also defined elsewhere.
;; I need to check...

(defmacro opapply (op args)
  `(simplify (cons (list ,op) ,args)))

(defmacro opcons (op &rest args)
  `(simplify (list (list ,op) ,@args)))

;; Maybe I should use csign instead of these functions...

(defun number-sign (x)
  (cond ((or (integerp x) (floatp x))
	 (cond ((< x 0) '$neg)
	       ((= x 0) '$zero)
	       ((> x 0) '$pos)))
	
	(($ratnump x) (number-sign ($num x)))
	(($bfloatp x) (number-sign (second x)))
	((eq '$minf x) '$neg)
	((member x '($inf $%pi $%e $%phi) :test #'eq) '$pos)
	((member x '($%i '$infinity) :test #'eq) '$complex)
	(($constantp x) (constant-expression-sign x))
	(t nil)))
	 
(defun constant-expression-sign (x)
  (let ((f1) (f2) (f3))
   
    ;; I can do better. A big float running error evaluator would be nice.
    ;; (or interval arithematic).  But for now let's evaluate with 25, 50, 
    ;; and 75 digits. Provided all three agree, and |f3| > 2^-50, return the common sign.

    ;; I need to check for complex...
	   
    (setq f1 (bind-fpprec 25 ($bfloat x)))
    (setq f2 (bind-fpprec 50 ($bfloat x)))
    (setq f3 (bind-fpprec 75 ($bfloat x)))
    
    (if (and ($bfloatp f1) ($bfloatp f2) ($bfloatp f3)
	     (eq (number-sign f1) (number-sign f2))
	     (eq (number-sign f2) (number-sign f3))
	     (> (first (last f3)) -50)) (number-sign f1) nil)))
       
(defun cartesian-product (b)
  (cond ((null b)
         nil)
        (t
         (let ((a) (acc (mapcar #'list (car b))))
           (setq b (cdr b))
           (dolist (bi b acc)
             (setq a nil)
             (dolist (bij bi (setq acc a))
               (setq a (append a (mapcar #'(lambda (x) `(,@x ,bij)) acc)))))))))

(defun expand-and-over-or (e)
  (cond (($mapatom e) e)
        ((op-equalp e 'mand)
         (setq e (mapcar #'(lambda (s) (if (op-equalp s 'mor) (margs s) (list s))) (margs e)))
         (setq e (cartesian-product e))
         (setq e (mapcar #'(lambda (s) (opapply 'mand s)) e))
         (opapply 'mor e))
        (t (opapply (mop e) (mapcar 'expand-and-over-or (margs e))))))

(defun make-positive-product (l)
  (let* ((n (length l)) (m (expt 2 n)) (k) (p) (acc nil))
    (dotimes (i m)
      (if (evenp (logcount i))
          (progn 
            (setq p nil)
            (setq k 0)
            (dolist (li l)
              (push (apply 'm> (if (logbitp k i) (list 0 li nil) (list li 0 nil))) p)
              (incf k))
           
            (setq p (opapply 'mand p))
            (push p acc))))
    (opapply 'mor acc)))

(defun in-real-domain (e)
  (cond ((eq e '$%i) nil)
	(($mapatom e) t)

	((op-equalp e 'mplus 'mtimes '%atan '%cos '%sin '%cosh '%sinh '%asinh)
	 (opapply 'mand (mapcar 'in-real-domain (margs e))))

	((op-equalp e '%acos '%asin)
	 (setq e (first (margs e)))
	 (opapply 'mand (list (m>= e -1) (m>= 1 e))))

	((op-equalp e '%log)
	 (m> (first (margs e)) 0))

	((op-equalp e '%acosh)
	 (m>= (first (margs e)) 1))

	((op-equalp e 'mexpt)
	 (let ((x (first (margs e))) (n (second (margs e))))
	   (cond ((member ($compare n 0) '("<" "<=") :test 'equal)
		  (take '(mand) (m-neq x 0) (in-real-domain x) (in-real-domain n)))
		 (t
		  (take '(mand) (m-neq x 0) (in-real-domain x) (in-real-domain n))))))
	(t (m= e (take '($conjugate) e)))))
	   
(defun freeof-floats (e)
  (if ($mapatom e) (not (or (floatp e) ($bfloatp e)))
    (every 'freeof-floats (margs (ratdisrep e)))))

;; Splitify an expression; this does e -> ((e1, boolean) (e2, boolean), ...). Examples:

;;   x -->  ((x t)),
;;   |x| --> ((-x, x < 0) (x, x >= 0)),
;;   max(a,b) --> ((a, a > b) (b, b >= a)),
;;   a + |b| --> a + ((-b, b < 0) (b, b >=0)) --> ((a-b, b < 0) (a + b, b >= 0)),

(defun $splitify (e)
  (cons '(mlist) (mapcar #'(lambda (s) (push '(mlist) s)) (splitify e))))

(defun splitify (e)
  (let ((acc nil) (f))
    (cond (($mapatom e) (list (list e t)))
	
	  ((op-equalp e '$max)
	   (splitify (max-to-abs (margs e))))
	  
	  ((op-equalp e '$min)
	   (splitify (min-to-abs (margs e))))

	  ((op-equalp e 'mabs)
	   (setq e (first (margs e)))
	   (list (list (neg e) (m> 0 e)) (list e (m>= e 0))))
	  
	  ((op-equalp e 'mexpt)
	   (mapcar #'(lambda (s) (list (take '(mexpt) (first s) (third e)) (second s))) (splitify (second e))))
	   
	  ((op-equalp e 'mplus 'mtimes)
	   (setq f (if (op-equalp e 'mplus) 'add 'mult))
	   (setq e (mapcar 'splitify (margs e)))
	   (setq e (cartesian-product e))
	   (dolist (ek e acc)
	     (push
	      (reduce 
	       #'(lambda (a b) 
		   (list (funcall f (first a) (first b)) (take '(mand) (second a) (second b)))) ek) acc)))

	  (t (list (list e t))))))
	       
(defun m> (a b &optional (expand nil) (use-splitify t))
  (let* ((z) (sgn) (z-split) (acc nil))
    (setq a ($ratdisrep a))
    (setq b ($ratdisrep b))

    ;; This uses p / q > 0 == q^2 * (p / q) > 0 == p * q > 0. Skip the
    ;; p / q > 0 --> p * q > 0 transformation when z contains a floating 
    ;; point number. 
    
    (setq z (sub a b))
    (setq z (if (freeof-floats z) ($factor (mul ($num z) ($denom z))) z))
    (setq sgn (compare-using-empty-context a b))
   
    (cond ((equal sgn ">") t)

	  ((member sgn '("<" "<=" "=") :test 'equal) nil)

	  ;; Catch four easy cases before we splitify z. Without the checks for the easy cases, 
	  ;; we'd get things like |x| > 1 --> (-1 < x & x < 0) or (x = 0) or (x < 1, x > 0).

	  ;; First, |a| > b --> (b < 0) or (a > b, b >= 0) or (-a > b, b >= 0).

	  ((and (op-equalp a 'mabs) ($freeof 'mabs '$min '$max b))
	   (setq a (first (margs a)))
	   (opapply 'mor (list
			  (opapply 'mand (list (m> 0 b)))
			  (opapply 'mand (list (m>= b 0) (m> a b)))
			  (opapply 'mand (list (m>= b 0) (m> (neg a) b))))))
		    
	  ;; Second, a > |b| == a > b and  a > -b.

	  ((and (op-equalp b 'mabs) ($freeof 'mabs '$min '$max a))
	   (setq b (first (margs b)))
	   (opapply 'mand (list (m> a b) (m> a (neg b)))))
	  
          ;; Third, min(a1,a2,..., an) > b --> a1 > b and a2 > b and .. an > b.
	  
          ((and (op-equalp a '$min) ($freeof 'mabs '$min '$max b))
	   (opapply 'mand (mapcar #'(lambda (s) (m> s b)) (margs a))))

          ;; Fourth, a > max(b1,b2, ..., bn) --> a > b1 and a > b2 and ... and a > bn.
	  
          ((and (op-equalp b '$max) ($freeof 'mabs '$min '$max a))
	   (opapply 'mand (mapcar #'(lambda (s) (m> a s)) (margs b))))
	  	  	  
	  ;; Do z^n > 0 --> z # 0  n even,  z > 0, n odd.
	  ((and (op-equalp z 'mexpt) (integerp (third z)))
	   (if (even (third z)) (m-neq (second z) 0) (m> (second z) 0)))

	  ;; Do f(a) > f(b), where f is increasing --> a > b.
	  ((and (not ($mapatom a)) (not ($mapatom b)) (eq (mop a) (mop b)) ($featurep (mop a) '$increasing))
	   (opapply 'mand (list 
			   (m> (first (margs a)) (first (margs b)))
			   (in-real-domain a)
			   (in-real-domain b))))
	  
	  ;; Do f(a) > f(b), where f is decreasing --> a < b.
	  ((and (not ($mapatom a)) (not ($mapatom b)) (eq (mop a) (mop b)) ($featurep (mop a) '$decreasing))
	   (m> (first (margs b)) (first (margs a))))
	  	  
	  ;; Do a^x > a^y, where a > 1 --> x > y,
	  ((and (not ($mapatom a)) (not ($mapatom b)) (eq (mop a) (mop b)) 
		(op-equalp a 'mexpt) (eq (second a) (second b))
		(eq (compare-using-empty-context (second a) 1) ">"))
	   (m> (first (margs a)) (first (margs b))))
	  
	  ;; Do a * b > 0 --> (a > 0, b > 0) or (a < 0, b < 0). We only do this when
	  ;; z has two or more non-constant factors. This check seems spendy--is there
	  ;; a way to bailout before we get here?

          ((and (op-equalp z 'mtimes) expand) 
	   (make-positive-product (margs z)))
	  ;; Finally, take care of the abs, max, and min cases that the previous
	  ;; four cases miss. 
	  
	  ((and use-splitify (op-equalp z '$max '$min 'mabs 'mtimes 'mplus))
	   (setq z-split (splitify z))
	   (dolist (zk z-split)
	     (push `((mand) ,(m> (first zk) 0 expand nil) ,@(rest zk)) acc))
	   (push '(mor) acc)
	   (simplifya acc nil))

          (t 
	   (opapply 'mgreaterp (list z 0))))))
              
(defun m= (a b &optional (use-splitify t))
  (let* ((z (sub a b)) (nz) (acc) (z-split) (sgn  (compare-using-empty-context a b)))
    (setq z (if (freeof-floats z) ($factor z) z))
    
    (cond 

     ;; If compare says a = b, return true.
     ((equal sgn "=") t)          
     
     ;; If compare says a # b, return false.
     ((member sgn '("<" ">" "#") :test 'equal) nil)
          
     ;; for complex numbers, look at the real and imaginary parts.
     ((and (complex-number-p a '$numberp) (complex-number-p b '$numberp))
      (take '(mand) (m= ($realpart a) ($realpart b)) (m= ($imagpart a) ($imagpart b))))

     ;; z^n = 0 --> false if n <= 0 else z = 0.
     ((and (op-equalp z 'mexpt) (mnump (third z)))
      (if (member (number-sign (third z)) '($neg $zero) :test #'eq) nil (m= (second z) 0)))
          
     ;; f(a) = f(b), where f is one-to-one --> a = b.
     ((and (not ($mapatom a)) (not ($mapatom b)) (eq (mop a) (mop b)) ($featurep (mop a) '$one_to_one))
      (opapply 'mand (append (list (in-real-domain a) (in-real-domain b)) (mapcar #'m= (margs a) (margs b)))))
     
     ;; a * b  = 0 --> a = 0 or b = 0; also a / b --> a = 0 and b # 0.
     ((op-equalp z 'mtimes)
      (setq nz (m-neq ($ratdenom z) 0))
      (expand-and-over-or 
       (take '(mand) nz (opapply 'mor (mapcar #'(lambda (s) (m= s 0)) (margs z))))))

     ((and use-splitify (op-equalp z '$max '$min 'mabs 'mtimes 'mplus))
      (setq z-split (splitify z))
      (dolist (zk z-split)
	(push `((mand) ,(m= (first zk) 0 nil) ,@(rest zk)) acc))
      (push '(mor) acc)
      (expand-and-over-or (simplifya acc nil)))
     
     (t (take '(mequal) z 0)))))

(defun m-neq (a b)
  (let ((save-context $context) (new-context (gensym)) (sgn))
    (unwind-protect
     (progn
       (setq sgn (mnqp a b))
       (cond ((or (eq sgn t) (eq sgn nil)) sgn)
	     ((eq $domain '$real) (opcons 'mor (m> a b t) (m> b a t)))
	     (t (take '(mnot) (m= a b)))))
     (if ($member new-context $contexts) ($killcontext new-context))
     (setq $context save-context))))
	  
(defun m>= (a b)
  (let ((sgn (compare-using-empty-context a b)))
    (cond ((member sgn '(">=" ">") :test 'equal) t)
	  ((equal sgn "<") nil)
	  (t (opcons 'mor (m> a b  t) (m= a b))))))

(defun standardize-inequality (e)
  (let ((a) (b))
    (cond ((op-equalp e 'mand)
	   (opapply 'mand (mapcar 'standardize-inequality (margs e))))
	  
	  ((op-equalp e 'mor)
	   (opapply 'mor (mapcar 'standardize-inequality (margs e))))

	  ((or (mrelationp e) (op-equalp e 'mnotequal '$equal))
           (setq a (second e))
           (setq b (third e))

           (cond ((op-equalp e 'mlessp) (m> b a t))
                 ((op-equalp e 'mleqp) (opcons 'mor (m> b a  t) (m= a b )))
                 ((op-equalp e 'mequal '$equal) (m= a b ))
                 ((op-equalp e 'mgreaterp) (m> a b  t))
                 ((op-equalp e 'mgeqp) (m>= a b))
                 ((op-equalp e 'mnotequal) (m-neq a b))
                 (t e)))
          (t e))))

(defun affine-expression-p (e v)
  ($polynomialp ($expand e) v 
		`((lambda) ((mlist) s) ($lfreeof ,v s))
		`((lambda) ((mlist) s) ((mor) ((mequal) s 0) ((mequal) s 1)))))
 
(defun linear-elimination (l v)
  (let (($linsolve_params nil) ($backsubst t) ($programmode t) 
	($linsolvewarn nil) ($globalsolve nil) (subs) (vars))
    
    (setq l ($elim l v))
    (cond (($member 1 ($first l)) '$emptyset)
	  (t
	   (setq subs ($linsolve ($second l) v))
	   (setq vars (mapcar '$lhs (margs subs)))
	   (setq vars (push '(mlist) vars))
	   `((mlist) ,subs ,($first l) ,vars)))))
	  	   
(defun $fourier_elim (l vars)
 
  (let ((eq-list nil) (pos-list nil) (other-list nil) (acc) ($listconstvars nil) ($ratprint nil))
    
    ;; Check the arguments

    (setq l (if (op-equalp l 'mand 'mlist) (margs l) (list l)))
    (require-list-or-set vars "$fourier_elim")
    
    ;; Standardize each inequality and simplify. To simplify, apply 'mand. After that,
    ;; expand 'and' over 'or.'
    
    (setq l (opapply 'mand (mapcar #'(lambda (s) (standardize-inequality s)) l)))
    (setq l (expand-and-over-or l))
   
    (cond ((eq t l) '$universalset)
          ((eq nil l) '$emptyset)
          ((op-equalp l 'mor)
           (setq l (margs l))
           (dolist (li l)
	     (push ($fourier_elim li vars) acc))

           (setq acc (delete '$emptyset acc))
           (cond ((null acc) '$emptyset)
                 ((member t acc) '$universalset)
                 (t (opapply 'mor acc))))

          (t
	   (setq l (if (op-equalp l 'mand) (margs l) (list l)))
           (setq l (delete t l))
	   
           ;; Push all non-affine expressions into other-list; push all equalities 
           ;; into eq-list, push all > equalities into pos-list, and push everything else
           ;; into other-list. 

	   (dolist (li l)
	     (cond ((not (affine-expression-p ($lhs li) ($listofvars li))) (push li other-list))
                   ((op-equalp li 'mequal) (push li eq-list))
		   ((op-equalp li 'mgreaterp) (push ($lhs li) pos-list))
                   (t (push li other-list))))
           
	   ;; Using eq-list, elimination as many variables as possible.
	   
	   (push '(mlist) eq-list)
	   (push '(mlist) pos-list)
	   (push '(mlist) other-list)
	   (setq eq-list (linear-elimination eq-list vars))
	   
	   (cond ((eq '$emptyset eq-list) (setq pos-list '$emptyset))
		 (t
		  ;;;(setq elim-vars ($third eq-list))
		  (setq other-list (append other-list (margs ($second eq-list))))
		  (setq eq-list ($first eq-list))
		  (setq pos-list ($substitute eq-list pos-list))
		  (setq other-list ($substitute eq-list other-list))))
	   
           (cond ((eq pos-list '$emptyset) pos-list)
                 (t
		  (setq pos-list (fourier-elim (margs pos-list) (margs vars)))
		  (if (eq pos-list '$emptyset) pos-list
		    ($append eq-list pos-list other-list))))))))

;; Without the post-fourier-elim-simp, we get

;; (%i2) eqs : [0 < x, x<1, 0 < y, y <1, x+y+z < 4, z > 0]$
;; (%i3) fourier_elim(eqs,[z,y,x]);
;; (%o3) [0<z,z<-y-x+4,0<y,y<min(1,4-x),0<x,x<1]

;; Since 0 < x < 1, Maxima should be able to deduce that min(1,4-x) = 1. So (%o3)
;; should simplify to [0<z,z<-y-x+4,0<y,y<1,0<x,x<1]. 
;; To do this simplification, it's necessary to set dosimp to true--why, I don't
;; know (alternatively, use ($expand e 0 0)).

(defun post-fourier-elim-simp (pos)
  (let ((save-context $context) (new-context (gensym)))

    ;; (a) Declare a new context (b) put every member of the CL list 
    ;; pos into the fact database (c) simplify each member of pos 
    ;; (d) kill the new context and restore the old context.
    
    (unwind-protect
	(progn
	  ($newcontext new-context)
	  (mapcar #'(lambda (s) (mfuncall '$assume s)) pos)
	  (let ((dosimp t)) 
	    (setq pos (mapcar #'(lambda (s) (simplifya s nil)) pos))
	    (delete-if #'(lambda (s) (equal t (standardize-inequality s))) pos)))
	  
      (if ($member new-context $contexts) ($killcontext new-context))
      (setq $context save-context))))
	    
(defun fourier-elim (pos vars)
  (let ((vi) (acc nil) (w))
    (while (and (not (eq pos '$emptyset)) (first pos) vars)
      (setq vi (pop vars))
      (setq w (fourier-elim-one-variable pos vi))
      (setq acc (append acc (first w)))
      (setq pos (second w)))
    (if (eq '$emptyset pos) '$emptyset
      (progn
	(setq pos (delete t (mapcar #'(lambda (s) (m> s 0)) pos)))
	(if (consp (member 'nil pos)) '$emptyset
	  (opapply 'mlist (post-fourier-elim-simp (append acc pos))))))))
    
;; Do Fourier elimination on a list l of inequalities with respect to the 
;; indeterminate x. Each member of l is a polynomial, and each polynomial p in
;; in this list  implies the inequality p > 0. The function fourier-elim-one-variable 
;; returns a list of the form
;;
;;   ((lb < x , x < ub), other),
;;
;; where other list of list of the implied inequalities (require that each lower bound is less than 
;; each upper bound) and members of p that do not determine a bound on v.

;; Each member of l is a polynomial with real coefficients. Also each member p of l 
;; determines an inequality p > 0. 

(defun simplify-fe-args (pos)

  ;; (1) Delete all manifestly positive members of pos.
  ;; (2) If pos has a zero or negative member set pos to (-1).
  ;; (3) Remove the extremal members of pos (apply min).
  
  (cond ((member pos '($inf $minf) :test #'eq) (list pos))
	(t
	 (let ((save-context $context) (new-context (gensym)))
	   (unwind-protect
	       (progn
		 ($newcontext new-context)
		 (setq pos (delete-if #'(lambda (s) (eq '$pos (csign s))) pos))
		 (if (some #'(lambda (s) (member (csign s) '($neg $nz $zero) :test #'eq)) pos) (setq pos (list -1)))
		 (setq pos (apply-min pos))
		 (if (op-equalp pos '$min) (margs pos) (list pos)))
	     (if ($member new-context $contexts) ($killcontext new-context))
	     (setq $context save-context))))))

(defun fourier-elim-one-variable (pos x &optional (need-simp t))
 
  (let ((cf) (acc nil) (lb nil) (ub nil) (lb-args) (ub-args) (sgn) (sol) (bounds nil))
        (if need-simp (setq pos (simplify-fe-args pos)))
   
    ;; Find the lower and upper bounds for x; push the lower bounds into the list 'lb', and the upper
    ;; bounds into holds 'ub' holds the upper bounds. All polynomials that don't determine a bound get 
    ;; pushed into acc. 

    ;; Example: say 1 - x > 0. The coefficient of x is negative, and solving 1 - x = 0 for
    ;; x gives an *upper* bound for x (that is x < 1) . Simiarly, say 1 + x > 0. The 
    ;; coefficient of x is positive and solving 1 + x = 0 for x gives a *lower* bound for x
    ;; (that is x > -1).
    
    (dolist (li pos)
      (setq cf ($ratcoef li x))
      (setq sgn (number-sign cf))
      (if (member sgn '($neg $pos) :test #'eq)
          (progn 
            (setq sol ($expand (div (sub (mul cf x) li) cf)))
            (if (eq sgn '$neg) (push sol ub) (push sol lb)))
        (push li acc)))
    
    ;; Delete the non-extremal members of lb and ub 

    (setq lb (apply-max lb))
    (setq ub (apply-min ub))
    (setq lb-args (if (op-equalp lb '$max) (margs lb) (list lb)))
    (setq ub-args (if (op-equalp ub '$min) (margs ub) (list ub)))

    (setq lb-args (delete '$minf lb-args))
    (setq ub-args (delete '$inf ub-args))
    
    ;; Require that each lower bound be smaller than each upper bound. We could use
    ;; m> instead of sub here. But I think it's not needed, and m> is more spendy.
    
    (setq acc (append acc (mapcar #'(lambda (s) (sub (second s) (first s)))
				  (cartesian-product (list lb-args ub-args)))))
    
    ;; Return ((lb < x x < ub) acc).
      
    (if (not (eq '$inf ub)) (push (opcons 'mlessp x ub) bounds))
    (if (not (eq '$minf lb)) (push (opcons 'mlessp lb x) bounds))
    
    (list
     ;;(list (opcons 'mlessp lb x) (opcons 'mlessp x ub))
     bounds
     (if (some #'(lambda (s) (or (eq 0 s) (eq nil s))) acc) '$emptyset acc))))

;; Apply max and min without looking at the current context; if something goes wrong,
;; cleanup the mess.

(defun apply-max (l)
  (let ((save-context $context) (new-context (gensym)))
    (unwind-protect
	(progn
	  ($newcontext new-context)
	  (setq l (mapcar '$expand l))
	  (opapply '$max l))
      (if ($member new-context $contexts) ($killcontext new-context))
      (setq $context save-context))))

(defun apply-min (l)
  (let ((save-context $context) (new-context (gensym)))
    (unwind-protect
      (progn
	($newcontext new-context)
	(setq l (mapcar '$expand l))
	(opapply '$min l))
      (if ($member new-context $contexts) ($killcontext new-context))
      (setq $context save-context))))
 
(defun compare-using-empty-context (a b)
  (let ((save-context $context) (new-context (gensym)))
    (unwind-protect
	(progn
	  ($newcontext new-context)
	  ($compare a b))
      (if ($member new-context $contexts) ($killcontext new-context))
      (setq $context save-context))))

