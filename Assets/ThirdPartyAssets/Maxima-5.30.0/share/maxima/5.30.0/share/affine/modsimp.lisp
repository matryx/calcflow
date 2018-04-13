;;; -*- Mode: LISP; Package: cl-maxima; Syntax: Common-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :maxima)

;;then have a compiler optimizer which does the selection.
(defun nn+ (&rest l)
   (cond ((null l) 0)
	 ((cddr l)
	  (n+ (car l) (apply 'nn+ (cdr l))))
	 (t (n+ (car l)  (second l)))))

;;a and b should be polynomials or rational-functions with the following
;;restrictions:  Any noncommutative variables or monomials occur with
;;higher pointergp than any scalars.  This will be the case with
;;add-newvar if you stick to using a,b,%zeta, etc for the scalars and
;;x,y,z,.. for the noncommutative variables.  All the nc parts thus will
;;come first so that a polynomial will be scalar if its main variable is
;;scalar.  General assumption is that nc polynomial will have all the nc
;;parts before any of the nc parts and no non commutative denominators.
;;We also assume that  a noncommutative polynomial looks like
;;   ncprod-or-var*cof+ncpolynomial  where cof is a scalar polynomial
;;probably the default should be that variables are scalar, but
;;at present the central variables must satisfy ($scalarp var) ==> t

(defun n. (a b)
   (cond ((numberp a)
	  (n* a b))
	 ((numberp b) (n* a b))
	 ((atom b) (setq b (st-rat b))(n. a b))
	 ((polynomialp a)
	  (cond ((polynomialp b)
		 (let ((va (get (p-var a) 'disrep)) vb ncprod)
		   (cond (($scalarp va)
			  (ptimes a b))
			 (($scalarp (setq vb (get (p-var b) 'disrep)))
			  (ptimes a b))
			 (t (check-arg a (and (<=  (length a) 5)
					      (<= (p-deg a) 1)) "an nc polynomial")
			    (check-arg b (and (<=  (length b) 5)
					      (<= (p-deg b) 1)) "an nc polynomial")
			    (setq ncprod(add-newvar  (ncmul* va vb)))
			    (nn+ (ptimes (list ncprod 1 1) (ptimes (p-cof a) (p-cof b)))
				 (cond ((cdddr a) (n. (fifth a) (subseq b 0 3)))
				       (t 0))
				 (cond ((cdddr b) (n.   (subseq a 0 3) (fifth b) ))
				       (t 0))
				 (cond ((and (cdddr a) (cdddr b))
					(n. (fifth a ) (fifth b)))
				       (t 0)))))))
		((rational-functionp b) (nred (n. a (num b)) (denom b)))
		(t (n. a (st-rat b)))))
	 ((rational-functionp a)
	  (nred (n. (num a) b) (denom a)))
	 (t  (setq a (st-rat a))
             (n. a b)
	     )))

(defun $find_sygy (degn &aux monoms general-term eqns sols simp)
  (setq monoms ($mono $current_variables (1- degn)))
  (setq monoms
	(cons '(mlist)
	      (loop for v in (cdr monoms)
		 collecting (ncmul* v '$x)
		 collecting (ncmul* v '$y))))
  (mshow monoms)
  (setq general-term ($general_sum monoms $aaaa))
  (setq simp ($ratdisrep ($dotsimp general-term)))
  (mshow simp)
  (mshow ($list_nc_monomials simp))
  (setq simp ($ratsimp ($numerator simp)))
  (setq eqns ($extract_linear_equations `((mlist) ,simp)))
  (setq sols ($fast_Linsolve eqns ($list_variables eqns "aa")))
  (mshow eqns sols)
  ($separate_parameters ($sublis sols general-term)))

 
(defun $find_sygy (degn matrix
		   &aux monoms  eqns sols pref general-image  general-from from-n to-n simp scal)
  " find_sygy(2,matrix([x],[y]))===> pq"
  (setq from-n ($length matrix))
  (setq to-n ($length (second matrix)))
  (setq monoms ($mono $current_variables degn))
  (setq pref (loop for i
		from 1 to from-n
		collecting ($concat '$aa i)))
  (setq general-from
	(cons '(mlist) (loop for u in pref	do
			    (setq scal (loop for i below ($length monoms)
					  collecting ($concat u i)))
			    ($declare_scalar_list (setq scal(cons '(mlist) scal)))
			  collecting ($general_sum monoms scal))))
  (mshow general-from)
  (setq general-image (ncmul* general-from matrix))
  (mshow general-image)
  (break t)
  (setq simp ($ratdisrep ($dotsimp general-image)))
  (displa simp)
  (mshow simp)
  (mshow ($list_nc_monomials simp))
  (setq simp ($ratsimp ($numerator simp)))
  (cond (($listp simp) nil)
	(($matrixp simp)(setq simp ($list_matrix_entries simp)))
	(t (setq simp (list '(mlist) simp))))
  (setq eqns ($extract_linear_equations simp))
  (setq sols ($fast_Linsolve eqns ($list_variables eqns "aa")))
  (mshow eqns sols)
  ($separate_parameters ($sublis sols general-from)))

(defmacro matrix-row (mat n)`(nth ,n ,mat))

(defun matrix-column (mat n)
 (cons '(mlist) (loop for v in (cdr mat)
	collecting (nth n v))))

(defvar $module_simp_for_kernel nil)
(defun $find_kernel (in-deg matrix &optional multiply-on-left
		   &aux first eqns sols general-image $general_from   simp  )
"if matrix is m x n then it finds the kernel of the MAPL on the right
multiplication B:A^m-->A^n     v |---> vB or   the kernel of the left
multiplication A^m <--- A^n:B    Bv<----|v"
  (cond (multiply-on-left (setq first (matrix-row matrix 1)))
	(t
	 (setq first (matrix-column matrix 1))))

  ;;in-deg refers to degree of image in 1-1 position
  (setq $general_from (cons '(mlist)
			    (loop for u in (cdr first)
				  for i from 1
			   collecting ($scalar_sum ($concat '$ff i)
						   ($mono $current_variables
							  (- in-deg ($nc_degree u)))))))
  (mshow $general_from)
  (cond (multiply-on-left
	 (setq general-image (ncmul*   matrix ($transpose  $general_from) )))
	(t 	(setq general-image (ncmul*  $general_from matrix))))
  (mshow general-image)
  (setq simp ($ratdisrep ($dotsimp general-image)))
  (cond ($module_simp_for_kernel (format t "~%Simplifying by ~A" $module_simp_for_kernel)
	 (setq simp($totaldisrep (funcall $module_simp_for_kernel simp)))))
  (mshow simp)
  (setq simp ($ratsimp ($numerator simp)))
  (cond (($listp simp) nil)
	(($matrixp simp)(setq simp ($list_matrix_entries simp)))
	(t (setq simp (list '(mlist) simp))))
  (setq eqns ($extract_linear_equations simp))
  (setq sols ($fast_Linsolve eqns ($list_variables eqns "ff")))
  (mshow eqns sols)
  ($separate_parameters ($sublis sols $general_from) "par" "ff"))

(defun $find_basis (list-vectors &aux sum eqns sols rk)
  (setq sum ($scalar_sum '$aa list-vectors))
  (setq eqns ($ratsimp ($numerator ($ratdisrep ($dotsimp sum)))))
  (setq eqns ($extract_linear_equations eqns))
  (setq sols ($fast_Linsolve eqns ($list_variables eqns "aa")))
  (setq rk (sp-number-of-pivots (pv-the-sparse-matrix $poly_vector)))
  ($separate_parameters ($sublis sols sum) "par" "aa"))

(defun $scalar_sum (prefix monoms &aux scal )
;  (declare (values general-sum scalars))
  (setq scal (loop for i below ($length monoms)
		   collecting ($concat prefix i)))
  ($declare_scalar_list (setq scal(cons '(mlist) scal)))
  (values ($general_sum monoms scal) scal))

(defun replace-parameters-by (expr prefix &aux olds pars tem )
  (setq pars (cdr ($list_variables expr "par")))
  (setq olds ($list_variables expr prefix))
  (loop for i from 0 while pars
	when (not (member (setq tem ($concat prefix i)) olds :test #'equal))
	  collecting (cons (car pars) tem) into repl
	and
	do (setq pars (cdr pars))
	finally (cond (repl
		       (loop for v in repl do (format t "~%Replacing ~A by ~A" (car v) (cdr v)))
		       (return (sublis repl expr)))
		      (t (return expr)))))

(defun $leftsocle (in-degree module-relations rad &aux h newh relats allrelats
		   monoms eqns vari solns  hh condition tem)
  "currently the modulue is a cyclic left module M=A/A.module-relations and
   we test to find solutions for h of v.h-A.module-relations for v in rad
  everything is supposed to be homogeneous"
  (setq h ($scalar_sum '$bbb ($mono $current_variables in-degree)))
  (setq relats
  (loop for v in (cdr module-relations)
	for i from 0
	do
	(setq tem  ($scalar_sum ($concat '$ccc i)
				($mono $current_variables
				       (1+ (- in-degree ($nc_degree v))))))
	collecting (ncmul* tem v)))
 (setq allrelats (meval* (cons '(mplus) relats)))
  (mshow allrelats)
  (setq monoms ($mono $current_variables in-degree))
  ;;impose condition x.h-general-sum module-relations
  (loop for v in (cdr rad)
	with all-rel = (num (new-rat  allrelats))
	do (setq v (st-rat v))
	   (setq hh (n. v (num (new-rat h))))
	   (setq condition (n- hh all-rel))
;	   (setq condition (sub* (ncmul* v h)
;				 allrelats))
;	   (setq conditon (num (new-rat 
;	  (setq condition ($numerator condition))
;	   (setq condition ($ratdisrep ($numerator ($dotsimp condition))))
        (setq condition (new-rat-dotsimp (cons condition 1)))
	(displa condition)
	(setq condition ($totaldisrep ($numerator condition)))
	(setq eqns ($extract_linear_equations (list '(mlist) condition)))
	(setq vari ($list_variables eqns "bbb" "ccc" ))
	(setq solns ($fast_linsolve eqns vari))
	(setq newh ($ratsimp($sublis solns h)))
	(cond (($zerop newh)
	       (return '((mlist)))))
	(setq h (replace-parameters-by newh '$bbb))
	(displa h)
	finally (return ($separate_parameters newh "bbb" "par"))))

(defun $try_bezout( f g  &optional (linears   #$[z-a*x,y-b*x]$) &aux (deg ($nc_degree f))  eqns cofs cof-eqns answ det tem)
  "solve f=f1*(z-a*x)+f2*(z-b*x),g=g1*(z-a*x)+g2*(z-b*x)"
  (setq cofs (loop for v in '(f1 f2 g1 g2)
		     do (setq tem 
			      ($scalar_sum ($concat '$ccc v)
					   ($mono $current_variables (- deg 1))))
		     collecting tem))
  (setq eqns
	(loop for (aa bb) on cofs by #'cddr
	      for ff in (list f g)
	      collecting
		(loop  for lin in (cdr linears)
		  for acof in (list aa bb)
		  with eqn = ff
		  do (setq eqn (sub* eqn (ncmul* acof lin)))
		  finally (return eqn))))
  (setq eqns (loop for v in eqns collecting ($ratdisrep ($dotsimp v))))
  (setq cof-eqns ($extract_linear_equations (cons '(mlist) eqns) ($mono $current_variables deg)))
  (mshow cof-eqns)
  (setq answ ($fast_linsolve cof-eqns ($list_variables cof-eqns "cc")))
  (setq det (sp-determinant (pv-the-sparse-matrix $poly_vector)))
  (list '(mlist) answ (new-disrep det)))

(defun $intersect_principals_variety ($f1 $f2 in-deg &optional in-free-ring
			      &aux  $g1 $g2 eqn eqns )
  "Af1 intersect f2A yielding g1.f1=f2.g2, and returns a equations.."
  (declare (special $g1 $g2 $f1 $f2))
	 (setq $g1 ($scalar_sum '$gg1 ($mono $current_variables (- in-deg ($nc_degree $f1)))))
	 (setq $g2 ($scalar_sum '$gg2 ($mono $current_variables (- in-deg ($nc_degree $f2)))))
	 	 (setq eqn #$[g1.f1-f2.g2]$)
	 (cond (in-free-ring nil)
	       (t  (setq eqn ($totaldisrep ($dotsimp eqn)))))
        (setq eqns ($extract_linear_equations eqn)))

(defun $intersect_principals ($f1 $f2 in-deg &optional modulo-free-ring
			      &aux free-result result $g1 $g2 eqn eqns solns)
  "Af1 intersect f2A yielding g1.f1=f2.g2, and returns a matrix([g1,g2],[g1p,g2p],...)"
  (declare (special $g1 $g2 $f1 $f2 $eqns))
  (cond (modulo-free-ring
	 (let ($dot_simplifications )
	   (setq free-result  ($intersect_principals $f1 $f2 in-deg)))
	 (setq free-result ($totaldisrep ($dotsimp free-result)))
	 (setq result  ($intersect_principals $f1 $f2 in-deg))
	 ($nc_matrix_quotient result free-result))
	(t
	 (setq $g1 ($scalar_sum '$gg1 ($mono $current_variables (- in-deg ($nc_degree $f1)))))
	 (setq $g2 ($scalar_sum '$gg2 ($mono $current_variables (- in-deg ($nc_degree $f2)))))
	 (setq eqn #$totaldisrep(dotsimp([g1.f1-f2.g2]))$)
	 (mshow eqn)
	 (setq $eqns (setq eqns ($extract_linear_equations eqn)))
	 (setq solns ($fast_linsolve eqns ($list_variables eqns "gg")))
	 (setq result ($sublis solns #$[g1,g2]$))
	 (cons '($matrix) (cdr ($separate_parameters result "par" "gg")))
	 )))

;(defun $find_basis_module (in-deg matrix-relations &optional right)
;  "given matrix([[f1,f2,..,fr],[g1,g2,..,gr],...]) compute a basis
;  for the free module A^r modulo the left(resp right) submodule generated by the 
;  rows of matrix.  Each row must satisfy nc_degree(fi)= nc_degree(fj)."

(defvar $module_simplifications nil)

(defun module-monom-must-replacep (monom &aux repl)
  (cond ((atom monom)
	 (setq repl (member monom (cdr $module_simplifications) :test #'eq))
	 (cond (repl (list monom (second repl)))))
	(t
	 (loop for v on (cdr $module_simplifications) by #'cddr
	       with leng =  (length monom)
	       with leng-first-v
	       when
		 (or (and (listp (car v))
			  (>= leng (setq leng-first-v(length (car v))))
			  (equal (nthcdr (- leng leng-first-v) (cdr monom)) (cdar v)))
		     (eql (car v) (nth (1- (length monom)) monom)))
		 do (return (list (first v) (cond ((numberp (second v)) (second v))
						  (t (cdr (second v))))))))))
;(defun module-must-replacep (poly)
;  (*catch 'found-replace
;    (mod-must-replace1 poly)))

(defun module-must-replacep (poly &aux monom)
  (loop while poly
	do
    (cond ((atom poly) (return nil))
	  ((module-monom-must-replacep (setq monom (get (car poly) 'disrep)))
	   (return (cons (car poly)   (module-monom-must-replacep monom))))
	  (t (setq poly (fifth poly))))))

(defun rat-simplifications (simps)
 (cons '(mlist) (loop for (mon rep) on (cdr simps ) by #'cddr
	collecting mon
	collecting (cons *fake-rat* (new-rat rep)))))

(defun module-simp (fun &aux did-replace num-fun den-fun
		    mon actual-repl repl den prev-mon lft symbo num-repl     repl-data )
  "argument satisfies rational-functionp"
  (cond ((polynomialp fun)(setq num-fun fun)(setq den-fun 1))
	((rational-functionp fun)(desetq (num-fun . den-fun) fun))
	(t (fsignal "takes poly or rat'l function for argument")))
;  (show $module_simplifications)
  (loop while (setq repl-data (module-must-replacep num-fun)) do
    (desetq (symbo mon repl) repl-data)
    (setq prev-mon (get symbo 'disrep))
    (setq did-replace t)
    (setq lft (dot-right-quotient prev-mon mon))
    (setq actual-repl (n. (st-rat lft) repl))
    (cond ((polynomialp actual-repl) (setq num-repl actual-repl) (setq den 1))
	  ((rational-functionp actual-repl)(desetq (num-repl .  den) actual-repl))
	  (t (fsignal "bad repl")))
    (format t "~%Module simp replacing ~A" (get symbo 'disrep))
    (setq num-fun (psublis(list  (cons symbo num-repl)) den num-fun))
    (setq den-fun (n* den den-fun))
	finally (return (cond (did-replace (ratreduce num-fun den-fun))
		      (t fun)))))

(defun delete-from-simps (simps mon repl)
  (loop for v on (cdr simps) by #'cdr
	when (and (equal (car v) mon)
		  (equal (second v) repl))
	  do (return (cons '(mlist)(nconc tem (cddr v))))
	  else
	  nconc (subseq v 0 2) into tem))


;;;needs work:note that we have not been using new-rat-dotsimp and we need to!
;;;maybe the best plan is to sort the simps in increasing order of degree,
;;;then to (let the modsimps be just one term and one replacement and check
;;;alll remaining ones for subs.  Any replacement will be further down the
;;;list.   Also if you do a replacement you could not get earlier in the list
;;;if  they are sorted by degree, and within degree by worst monom
;(defun simplify-module-simplifications ()
;  (loop named sue for (mon rep) on (cdr $dot_simplifications) by #'cddr
;          do
;	  (let (($module_simplifications (delete-from-simps $module_simplifications mon repl)))
;	      (loop for (mon1 rep1) on (cdr $dot_simplifications) by #'cddr
;		    when (or (module-monom-must-replacep mon1)
;			     (module-must-replacep rep1))
;		      do
;		(setq relat (function-numerator (module-simp (make-relation mon1 rep1))))
;                (setq relat (module-simp relat))
;		(setq changed t)
;		(return-from sue 'start-over))
;		(setq new (plain-add-to-module-simps relat)))))

(defun make-relation (monom repl)
;  (iassert (or (numberp repl) (eql (caar repl) 'mrat)))
  (cond ((numberp repl) nil)
	((polynomialp repl)(setq repl (cons repl 1)))
	((rational-functionp repl) nil)
	((eql (caar repl) 'mrat) (setq repl (cdr repl)))
	(t (fsignal "bad repl")))
  (function-numerator  (n- monom repl)))


(defun sort-simps-by-degree (simps)
 (cons (car simps) (sort-grouped-list (cdr simps) 2 #'(lambda (u v)
        						(cond ((atom u))
							 ((atom v) nil)
							 ((< (length u) (length v)) )
							 ((> (length u) (length v)) nil)
							 (( funcall $order_function  v u)))))))


(defun module-and-dot-simp (pol  &aux (changed  t))
  (loop
    do
    (cond ((module-must-replacep pol)
	   (setq pol (module-simp pol)) (setq changed t)))
    (cond (($must_replacep pol)
	  (setq pol (cdr (new-rat-dotsimp pol)))
	  (setq changed t)))
    while changed
    finally (return pol)))

(defun gen-dot-check-associative (a b c &optional (simplifier 'module-and-dot-simp) &aux answ)
  "simplifier should return a polynomial or rational function"
 (setq answ (n- (n. (funcall simplifier (n. a b)) c)
      (n. a (funcall simplifier a b))))
 (cond ((pzerop answ) (format t "~%It was associative"))
       (t answ)))


;(defun find-right-overlap (left right &aux answ)
;  (setq answ
;	(cond	((atom left)
;		 
;		 (cond ((eql left right)(list nil left nil))
;		       ((equal left (car (last right)))
;			(list  (butlast right) left nil))
;		       (t nil)))
;		((atom right)
;		 (cond ((equal right (car (last left)))
;			(list (butlast left) right nil))
;		       (t nil)))
;		(t
;		 (multiple-value-bind (lap right-excess)
;		     (find-lap1 (cdr left) (cdr right))
;		 (cond (lap (list (firstn (- (length left)  1 (length lap)) (cdr left))   lap right-excess)))))))
;  (show answ)
;  (and answ
;
;
;       (loop for v on answ
;	     for a in answ
;	     do 
;
;	 finally (return answ))))

(defun coerce-nctimes (a)
  "nil-->1, '(a) -->a '(x y z) --> x.y.z , x.y--> x.y"
	 (cond ((null a)
                   1)
	       ((listp a)
		(cond ((listp (car a))
		       (cond ((eql (caar a) 'mnctimes)
			      (cond ((null (cdr a)) 1)
				    ((cddr a) a)
				    (t (second a))))
			     (t (fsignal "what am i like?"))))
		      ((not (cdr a))
		       (first a))
		      (t  (cons '(mnctimes) a))))
	       (t a)))

(defun $lastn_ncdegree (mon n)
  "Returns an final segment of mon of degree n if possible, otherwise nil"
  (cond ((zerop n) 1)
	((atom mon)
	 (and (equal ($nc_degree mon) n)
	      mon))
	(t (loop 
		 for i downfrom (1- (length mon)) to 1
		 for u = (nth i mon)
		 summing ($nc_degree u) into tot
		 when (>= tot n)
		   do
		     (cond ((eql tot n)
			    (return (coerce-nctimes (nthcdr i mon))))
			   (t (return nil)))))))

(defun $firstn_ncdegree (mon n)
  "Returns an initial segment of mon of degree n if possible, otherwise nil"
  (cond ((zerop n) 1)
	((atom mon)
	 (and (equal ($nc_degree mon) n)
	      mon))
	(t (loop for u in (cdr mon)
		 for i from 2
		 summing ($nc_degree u) into tot
		 when (>= tot n)
		   do
		     (cond ((eql tot n)
			    (return (coerce-nctimes (subseq mon 0 i))))
			   (t (return nil)))))))



(defun $find_right_lap (left right tot-deg &aux size-lap a-lap )
  "tot-deg is the degree of the lapped left and right. It will not catch left a subset
   of right since it is presumed the right is in reduced form "
  (let ((ld ($nc_degree left))
	(rd ($nc_degree right)))
    (setq size-lap (- (+ ld rd) tot-deg))
    (cond ((<= size-lap 0) nil)
	  (t (setq a-lap ($lastn_ncdegree left size-lap))
	    (cond((and a-lap
		 (or
		   (equal a-lap right)
		   (cond ((listp right)
			  (cond ((listp a-lap)
				 (initial-seg (cdr a-lap) (cdr right)))
				(t (equal (second right) a-lap)))))))
		   (list ($firstn_ncdegree left (- ld size-lap))
			 a-lap
			 ($lastn_ncdegree right (- rd size-lap)))))))))

(defun $find_right_lap (left right tot-deg &aux size-lap a-lap )
  "tot-deg is the degree of the lapped left and right. It will not catch left a subset
   of right since it is presumed the right is in reduced form "
  (let ((ld ($nc_degree left))
	(rd ($nc_degree right)))
    (setq size-lap (- (+ ld rd) tot-deg))
    (cond ((<= size-lap 0) nil)
	  (t (setq a-lap ($lastn_ncdegree left size-lap))
	   (cond ((and a-lap  ;;have match
		    (cond ((equal a-lap right))
			  ((and (listp a-lap) (listp right)
			      (initial-seg (cdr a-lap) (cdr right))))
			  ((and (atom a-lap) (listp right)
				(equal (second right) a-lap)))))
		(list ($firstn_ncdegree left (- ld size-lap))
		      a-lap
		      ($lastn_ncdegree right (- rd size-lap)))))))))

;
;(defun $find_right_lap (left right tot-deg &aux size-lap a-lap )
;  "tot-deg is the degree of the lapped left and right. It will not catch left a subset
;   of right since it is presumed the right is in reduced form "
;  (let ((ld ($nc_degree left))
;	(rd ($nc_degree right)))
;    (setq size-lap (- (+ ld rd) tot-deg ))
;    (cond ((<= size-lap 0) nil)
;	  (t (setq a-lap ($lastn_ncdegree left size-lap))
;;	    (cond( a-lap
;;	    (setq b-lap ($firstn_ncdegree right size-lap))))
;	     (cond 
;	       ((and a-lap
;		     (or (equal a-lap right)
;			 (and (listp a-lap) (listp right)
;			      (initial-seg (cdr a-lap) (cdr right)))
;			 (listp right) (equal (second right) a-lap))
;			      )
;;		(equal (cdr a-lap) (cdr b-lap)))))
;		(list ($firstn_ncdegree left (- ld size-lap))
;		      a-lap
;		      ($lastn_ncdegree right (- rd size-lap)))))))))

(defun initial-seg (seg lis)
  "'(1 2) '(1 2 3) --> values: t '(3), '(1 2) '(0 1 2 3) --> nil, '(1 2) '(1) --> nil"
  (prog ()
	a
	(or (eql (car seg) (car lis)) (return nil))
	(setq lis (cdr lis))
	(setq seg (cdr seg))
	(cond ((null seg) (return (values t lis))))
	(and (null lis)  (return nil))
	(go a)))




(defun plain-add-to-module-simps (poly)
  (cond ($module_simplifications nil)
	(t (setq $module_simplifications '((mlist)))))
	   (setq $module_simplifications
		 (append $module_simplifications  (make-simp (function-numerator poly)))))


;;must do two things 
;;I reduce module-simps leading terms wrt themselves.
;;II If M is a leading term of f and DM is a dotsimp replacement,
;; and there is a dotsimp repl R and the tail of R overlaps the left side of M
;;then we have two ways of simplifying and need to add the difference.
;;add in the simp dotsimp(Df).  Note this is essentially checking for overlaps
;;between the module simps and the dotsimps.
;;Have two functions: I one which checks for type I and if finds, returns the (changed list of simps , t)
;;else returns simps,nil
;;II one which checks for the II type and returns the new list of simps ,t or just old list of simps.


(defun make-simp (relat &aux answ)
  "inverse of make-relation, except that constants will be cancelled."
 (setq answ (cond ((numberp relat)
	 (cond ((zerop relat) nil)
	       (t (list 1 0))))
	(t (list (get (car relat) 'disrep)
	   (cond ((fifth relat)
		   (cons *fake-rat* (ratreduce (pminus (fifth relat)) (third relat))))
		 (t 0))))))

 answ)

(defun dot-right-quotient (a b &aux tem answ )
  "a.b^^-1 with error if b not final segment"
  (cond ((atom b)
	 (cond ((atom a)
		(cond ((eql a b) 1)
		      (t (fsignal "doesn't divide evenly"))))
	       ((equal (car (last a)) b)
		(setq answ (butlast a)))
	       (t (fsignal "not tail"))))
	(t
	 (loop for v in (cdr b)
	       for w in (nthcdr (setq tem  (1+ (- (length a) (length b)))) a)
;	       do ;(show v w tem)
	       when (not (equal v w))
		 do (fsignal "b is not a tail of a"))
	 (setq answ (subseq a 0 tem))))
  (cond ((cddr answ) answ)
	((cdr answ) (second answ))
	(t 1)))

(defvar *module-overlaps-checked* nil)

(defun simplify-module-simps (to-deg &aux changed new-simps ( present-deg 0))
  (declare (special present-deg))
  (user-supply *module-overlaps-checked*)
  (loop	do (setq changed nil)
       (loop for f in '(type-i-simp type-ii-simp) do
	    (multiple-value-setq
		(new-simps changed)
	      (funcall f present-deg to-deg ))
	    (setq $module_simplifications new-simps)
	    (cond (changed (return 'try-again))))
     while changed
     finally (return $module_simplifications)))

(defun type-i-simp (&rest ignore &aux old-simps relat new-relat)
  (declare (ignore ignore))
  "eliminate simps such that one leading term is a left multiple of another, ie common right overlap"
  (setq old-simps (setq $module_simplifications (sort-simps-by-degree $module_simplifications)))
  (loop named sue for tail-simps on (cdr old-simps) by #'cddr
	for i from 3 by  2
	do
    (let (($module_simplifications (cons '(mlist) (subseq tail-simps 0 2))))
      (loop for (mon repl) on (cddr tail-simps) by #'cddr
	    for jj from (+ i 2) by 2
	    when (or  (module-monom-must-replacep mon)
		      (and (listp repl)
			   (module-must-replacep (function-numerator (cdr repl)))))
	      do (setq relat  (make-relation mon repl))
		 (setq new-relat (module-simp relat))
		 (setq new-relat (new-rat-dotsimp new-relat))
                 (cond ((numberp new-relat)
			(cond ((zerop new-relat) nil)))
		       (t (setq new-relat (cdr new-relat))
			  (iassert (rational-functionp new-relat))
			  (setq new-relat (num new-relat))))
		 (return-from sue (values (nconc (subseq old-simps 0 (- jj 2))
						 (make-simp new-relat)
						 (nthcdr jj old-simps))
					  t))
	    finally (return-from sue (values old-simps nil))))))

(defun dotsimp (ratl-or-poly)
;  (declare (values ratl-function))
    (new-rat  (new-rat-dotsimp ratl-or-poly)))

(defun ratl-function-id (number-or-rational-function &aux tem)
  (cond ((polynomialp number-or-rational-function)
	 number-or-rational-function)
	((rational-functionp number-or-rational-function)
	 number-or-rational-function)
	((and (listp number-or-rational-function)
	      (listp (setq tem (car number-or-rational-function)))
	      (eql (car tem) 'mrat))
	 (cdr number-or-rational-function))
	(t (fsignal "not right type"))))

(defun type-ii-simp (from-deg to-deg &aux old-simps new-repl tem tem1 a b c)
  "checks overlaps of type (a b c) where a.b is a dotsimp and b.c is a modsimp and deg(a.b.c)<=i
   then replacement(a.b).c is a module relation to be added."
  (declare (special present-deg))
  (setq old-simps (setq $module_simplifications (sort-simps-by-degree $module_simplifications)))
  (loop for i from from-deg to to-deg do
       (block sue
	 (setq present-deg i)
	 (loop for (mon repl) on (cdr $dot_simplifications) by #'cddr do
	      (loop for (monmod repmod) on (cdr old-simps) by #'cddr 
		 when (setq tem ($find_right_lap mon monmod i))
		 do
		 ;;(show (list mon monmod))
		 ;;should really use
		 ;;(setq tem (list mon monmod i)) ;but only maximal overlaps need to be checked and since we go up in degree
		   (setq tem1 (cons mon monmod)) 
		   (cond ((not (member tem1 *module-overlaps-checked* :test #'equal))
			  (push tem1 *module-overlaps-checked*)
			  (iassert (nc-equal (ncmul* (first tem) (second tem)) mon))
			  (iassert (nc-equal (ncmul* (second tem) (third tem)) monmod))
			  (iassert (nc-equal ($nc_degree  (ncmul*  mon (third tem))) i))
			  (iassert (not (numberp (second tem))))
			  ;;(setq mod-prod (n. (first tem)(function-numerator repmod)))
			  ;;(setq new-repl (n-  (n. (function-numerator repl) (third tem)) mod-prod))
			  ;; rep.c - a.repmod is in the ideal defining the module!
			  (desetq (a b c) tem)
			  (setq new-repl (n- (n. (ratl-function-id repl) c)
					     (n. a (ratl-function-id repmod))))
			  (setq new-repl (dotsimp new-repl))
			  (setq new-repl (function-numerator new-repl))
			  (setq new-repl (function-numerator (module-simp new-repl)))
			  (mshow new-repl)
			  (mapcar #'displa (make-simp new-repl))
			  (cond ((pzerop new-repl) nil)
				(t (return-from sue (values (append old-simps (make-simp new-repl)) t)))))))))
     finally (return (values old-simps nil))))

;(defun type-II-simp ( to-deg &aux old-simps new-repl repl-data tem)
;  (setq old-simps (setq $module_simplifications (sort-simps-by-degree $module_simplifications)))
;  (loop for  (mon repl) on (cdr $dot_simplifications) by #'cddr 
;	when (setq repl-data (module-monom-must-replacep mon))
;	  do (cond ((not (member (setq tem (cons mon (car repl-data))) *module-overlaps-checked*))
;		    (push tem *module-overlaps-checked*)
;	    (setq new-repl (n. (dot-right-quotient mon (car repl-data)) (apply 'make-relation repl-data)))
;	     (setq new-repl (cadr (new-rat-dotsimp new-repl)))
;	     (return (values (append old-simps (make-simp new-repl))
;			     t))))
;	finally (return (values old-simps nil))))

;(let (($module_simplifications)(*module-overlaps-checked*))
;  (loop for v in (st-rat #$[x,-(y.z+x.x)]$)
;           do (plain-add-to-module-simps v))
;  ( simplify-module-simps))

(defun fake-header (form)
  (cond
	 ((polynomialp form)(cons *fake-rat* (cons form 1)))
	 ((rational-functionp form) (cons *fake-rat* form))
	 (t (fsignal "not poly or rat'l fun"))))
(defun $modsimp(form)
  (cond ((atom form)(fake-header (module-simp (new-rat form))))
	((or (polynomialp form) 
	 (rational-functionp form))
	 (fake-header (module-simp form)))
	((mbagp form)(cons (car form) (mapcar '$modsimp (cdr form))))
	(t (fake-header (module-simp (new-rat form))))))

(defun $cyclic_module_basis (degree &optional (variables $current_variables) )
  (cyclic-module-basis variables degree ($replacements) ($module_replacements)))

(defun $module_replacements( &optional (module-simps $module_simplifications))
  (cons '(mlist) (loop for (mon rep) on (cdr module-simps) by #'cddr
		       collecting mon)))

(defremember  cyclic-module-basis (variables deg  dot-replacements module-replacements &aux tem)
  (cond ((eql deg 0)
	 (cond ((not (or (member 1 dot-replacements :test #'equal)
			 (member 1 module-replacements :test #'equal)))
		'((mlist) 1))
	       (t '((mlist)))))
	(t
	 (cons '(mlist)
	       (loop for v in (cdr variables)
		      appending
		      (loop for w in   (cdr (cyclic-module-basis variables (1- deg) dot-replacements module-replacements))
			     do (setq tem (ncmul* v w ))
			     unless (or (module-monom-must-replacep tem) ($must_replacep tem))
			     collecting tem))))))


(defun $module_dimensions (to-n &optional (variables $current_variables) &aux tem)
  (loop for i from 0 to to-n
	collecting (setq tem ($length ($cyclic_module_basis i variables))) into dims
	summing tem into tot
	do (format t "~%The dimension of the cyclic module in deg ~A is ~A." i tem)
	   finally (format t "~%The total dimension through degree ~A is ~A." to-n tot)
		   (return (cons '(mlist) dims ))))


(defun $set_up_module_simplifications(relations &optional to-deg)
  (setq $module_simplifications nil)
  (setq *module-overlaps-checked* nil)
  (loop for v in (st-rat relations) do
       (plain-add-to-module-simps  v))
  (cond (to-deg
	 (simplify-module-simps to-deg)))
  $module_simplifications)

(defun $cyclic_module_basis (deg &optional (variables $current_variables))
 (cons '(mlist) (loop for v in (cdr  ($mono variables deg))
    when (not (module-monom-must-replacep v))
      collecting v)))

(defun $shift_sequence (seq n &aux (tem (make-list (abs n) :initial-element 0)))
  "shifts a sequence of degrees over by n to use for adding to check sums"
  (cond ((zerop n) seq)
	((> n 0)
	 (append ($rest seq  n) tem))
	(t ($append (cons '(mlist) tem) ($rest seq  n)))))

(defun $euler_sum (deg-seq size-of-module map-degs)
  (loop for v in (reverse (cdr map-degs))
	for s in (cdr size-of-module)
	for i from 1
	with ans = deg-seq
		 summing v into tot
	do (mshow ans)
	when (evenp i)
	  do (setq ans (add* ans (mul* s ($shift_sequence deg-seq (- tot)))))
	else do (setq ans (sub* ans (mul* s ($shift_sequence deg-seq (- tot)))))
	  finally (return ans)))

(defun $solve_sp (try eqns &aux  simp syst solns ld)
 (setq eqns  ($sublis try eqns))
 (setq ld  (make-ldata :eqns (mapcar 'function-numerator(st-rat  eqns))))
 (des ld)
 (setq simp (simplify-ldata ld))
 (des simp)
 (setq syst (loop for u in simp collecting (cons '(mlist) (mapcar 'new-disrep (ldata-eqns u)))))
 (setq solns (loop for v in syst collecting ($append try  ($fast_linsolve v ($list_variables v)))))
 (cons '(mlist) solns))


(defun $nc_linear_factor (pol &aux answ (deg ($nc_degree pol)) lin fac2 result eqns solns general)
  (setq lin ($scalar_sum '$bb ($mono $current_variables 1)))
  (setq fac2 ($scalar_sum '$cc ($mono $current_variables (- deg 1))))
  (setq result (sub* pol (ncmul* lin fac2)))
  (setq result ($dotsimp result))
  (setq eqns  ($extract_linear_equations (list '(mlist) result)))
  (mshow eqns)
  (setq answ
	(loop
     with try
     do
	  (format t "~%Supply a macsyma list of possible solns")
	  (setq try (mread-noprompt))
	  (mshow try)
	  (setq solns ($solve_sp try eqns))
	  (mshow solns)
	  (setq solns ($ratsimp solns))
	  (cond ((null (y-or-n-p "Try again?"))
		 (setq general (list '(mlist) lin fac2))
		 (return (cons '(mlist) (loop for v in (cdr solns)
					      appending (cdr ($separate_parameters ($sublis v general) "bb" "cc" "par")))))))))
  (cond((y-or-n-p "Verify factors:?")
	(loop for v in (cdr answ)
	      for i from 1
	      with simp
	      do (setq simp ($dotsimp (sub* pol  (ncmul* (meval*(second v)) (meval* (third v))))))
		 (cond ((pzerop simp)(format t "~%soln ~A is correct:" i)
			(displa (list '(mequal) pol (cons '(mnctimes) (cdr v) ))))
		       (t(format t "~%soln ~A is not correct ****************"))))))
  answ)

(defun $degless (u v &aux (u-deg ($nc_degree u)) (v-deg ($nc_degree v)))
  (cond ((< u-deg v-deg))
        ((eql  u-deg v-deg) (funcall $order_function u v))
	(t nil)))


