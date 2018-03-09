;;; -*- mode: lisp; package: cl-maxima; syntax: common-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defun $circle_times (&rest l)
  (case  (length l)
    (0 1)
    (1 (car l))
    (2 (let (firs sec (u (car l)) (v (second l)))
	 (setq firs (sub* (mul* (nth 1 u) (nth 1 v))
			  (mul* (nth 2 u) (nth 2 v))))
	 (setq sec (add* (mul* (nth 1 u) (nth 2 v))
			 (mul* (nth 2 u) (nth 1 v))))
       (list '(mlist simp) firs sec)))
    (otherwise
     ($circle_times (car l) (apply '$circle_times (cdr l))))))

(defun sort-grouped-list (a-list group-size pred &aux if-necessary answer tem (prev (car a-list)))
  (loop for v in a-list
     for i from 0
     when (zerop (mod i group-size))
     do (cond ((funcall pred v prev)
	       (setq if-necessary t)
	       (return t)))
       (setq prev v))
  (cond (if-necessary
	 (loop  with v = a-list
	    while v
	    collect v into all
	    do (setf v (nthcdr group-size v))
	    finally
	      (setq all (sort all pred :key 'car ))
	      (return
		(loop for v in all
		   nconc
		     (loop for w in v for i below group-size
			collect w)))))
	(t a-list)))

(defun $sort_dot_simplifications (&optional (pred $order_function) &aux rev-pred)
  (cond ((eq pred '$monomial_alphalessp)(setq rev-pred '$monomial_alphagreatp))
	(t (setq rev-pred #'(lambda (x y)
			      (and (not (funcall pred x y))(not (equal x y)))))))
  (rplacd $dot_simplifications
	  (sort-grouped-list (cdr $dot_simplifications) 2 rev-pred)))

;see polyd.lisp for current def
;(defun $simplify_dot_simplifications (&optional (from-degree 0) &aux temp1 repl temp2 temp3
;				      relat
;				      (dot-simps $dot_simplifications))
;
;  ($sort_dot_simplifications)
;  (loop for term in (cdr $dot_simplifications)
;	for i from 0
;	when (evenp i)
;	do
;	(let (($dot_simplifications
;		(append (firstn (1+ i) dot-simps)
;			(nthcdr (+ 3 i) dot-simps))))
;	  (cond (($must_replacep term)
;                 (setq repl (nth (1+ i)  (cdr dot-simps)))
;
;                 (setq relat
;		       (cond ($new_fast_dotsimp
;			      (header-poly (n- repl term)))
;			     (t (vsub* repl term))))
;                 (setq dot-simps $dot_simplifications)
;		 (return 'start-over))))
;	when (oddp i)
;	do
;	(setq temp2 term)
;	(show temp2)
;;;;	(cond ((and
;;;;;		 (>= ($nc_degree temp2) from-degree)
;;;;		 ($must_replacep temp2))
;
;	       (loop until  (null ($must_replacep temp2))
;		     do
;		     (break 'here)(setq temp1 temp2)
;		     (cond ($free_dot (setq temp2 ($dotsimp temp2)))
;			   (t
;			    (setq temp3 (meval* temp1))
;			    (cond ((not (equal temp1 temp3))
;				   (setq temp2 ($ratsimp temp3))))))
;		     finally
;		     (setf (nth (1+ i) $dot_simplifications) temp2)))
;  (cond (relat (setq $dot_simplifications dot-simps)
;	       (convert-relation-to-dot-simp ($dotsimp relat)))
;	(t
;	 $dot_simplifications))
;   (cond ($fast_dotsimp ($rat_the_dot_simplifications))
;	 (t $dot_simplifications)))
;
;;;second The above definition worked for not fast_dotsimp
;
;
;(defun $simplify_dot_simplifications (&optional (from-degree 0) &aux
;				      relat
;				      (dot-simps $dot_simplifications))
;   ($sort_dot_simplifications)
;   (loop for v on (cdr $dot_simplifications) by #'cddr
;	 for i from 1 by 2
;	 do
;         (setq dot-simps $dot_simplifications)
;	 (setq $dot_simplifications (append (firstn i $dot_simplifications) (cddr v)))
;	 (cond ((and (not ($zerop (second v)))
;		     (or ($must_replacep (car v))($must_replacep (second v))))
;		(setq relat (cond ($new_fast_dotsimp
;				   (header-poly (n- (car v) (second v))))
;				  (t (vsub* (car v) (second v)))))
;		(setq relat ($dotsimp relat))
;		(cond (($zerop relat))
;		      (t (convert-relation-to-dot-simp relat )))     ;;	;(copy-tree relat))))
;		($simplify_dot_simplifications from-degree)
;		(return 'start-over))
;	       (t (setq $dot_simplifications dot-simps))))
;   $dot_simplifications)



(defmfun ncmul2* (x y)
  (simplifya `((mnctimes) ,x ,y) nil))

(defun contains-a-zero-replacement (form &aux u)
  (or (in-nth-power-radical form $radical_nilpotent_of_order)
      (cond ((atom form)
	     (loop for v on (cdr $dot_simplifications) by #'cddr
		   when (and (eq 0 (second v)) (eq (car v) form))
		   do (return t)))
	    (t (loop for v on (cdr $dot_simplifications) by #'cddr
		     when (eq 0 (second v))
		     do (cond ((atom (setq u (car v)))
			       (cond ((member u form :test #'equal) (return t))))
			      (t (cond ((ordered-sublist (cdr u) form)(return t))))))))))

(defun	contains-a-replacement (form &aux tem1 u)
  "returns t if (ncmuln form nil) would cause a replacement  when simplified (form is
 not a product"
  (setq tem1 (cdr $dot_simplifications))
  (cond ((atom form)
	 (loop for u in tem1
	    for i from 1
	    when (and (oddp i)(eq u form))
	    do(return t)))
	(t
	 (loop while tem1
	       do (setq u (car tem1))
	       (setq tem1 (cddr tem1))
	       (cond ((atom u)(cond ((member u form :test #'equal) (return t))))
		     (t (cond ((ordered-sublist (cdr u) form )(return t)))))))))



(defvar *already-found* nil)

(defun $must_replacep (expression &aux(ptype (poly-type expression)) poly)
  (setq *already-found* nil)
  (case ptype
    (:rational-function (setq poly (num expression)))
    (:polynomial (setq poly expression))
    (:$rat (cond ($new_fast_dotsimp (setq poly (num (cdr expression)))))))
  (cond (poly
	 (cond ((numberp poly) nil)
	       (t
		(loop while poly
		      when ($must_replacep (get (car poly) 'disrep))
		      do (return t)
		      when (numberp       (setq poly (fifth poly)))
		      do (return nil)))))
	(($ratp expression)
	 (cond ((member (caadr expression) *genvar* :test #'eq)
		($must_replacep (cadr expression)))
	       (t
		(fsignal "this cre form should not be here, since its leading monom is not in *genvar*"))))
   	(t
	 (catch 'must-replace (must-replacep1 expression)))))

(defun must-replacep1-by-zero ( expression)
  (cond (*already-found* nil)
	((atom expression) (cond ((contains-a-zero-replacement expression)
				  (throw 'must-replace t))))
	((atom (car expression))(must-replacep1-by-zero (cdr expression)))
	((eq (caar expression) 'mnctimes)(cond ((contains-a-zero-replacement (cdr expression))
						(setq *already-found* t)
						(throw 'must-replace t) )))
	(t (must-replacep1-by-zero (car expression))(must-replacep1-by-zero
						      (cdr expression)))))

(defun must-replacep1 ( expression)
  (cond (*already-found* nil)
	((atom expression) (cond ((contains-a-replacement expression)
				  (throw 'must-replace t))))
	((atom (car expression))(must-replacep1 (cdr expression)))
	((eq (caar expression) 'mnctimes)(cond ((contains-a-replacement (cdr expression))
						(setq *already-found* t)
						(throw 'must-replace t) )))
	(t (must-replacep1 (car expression))(must-replacep1 (cdr expression)))))

;(defun $check_associative (a b c &aux tem)
;  (setq tem (sub* ($ratsimp (ncmul*  (ncmul* a b) c)) ($ratsimp (ncmul* a (ncmul* b c))))))
;(defun $check_associative (a b c &aux tem)
;  (setq tem  ($ratsimp (sub*  ($simp_ncmul
;			       ($simp_ncmul a b) c)
;			     ($simp_ncmul a ($simp_ncmul b c))))))

;(defun mread-noprompt (&rest read-args)
;  (let ((*mread-prompt* ""))
;  (caddr (apply #'mread read-args))))

(defun remove-header (x)
       (cond ((numberp x) x)
	     (t (cdr x))))

(defun new-rat-ncmul1 (a b c &aux tem)
  (cond ((numberp b)
	 (cond (($zerop (setq tem (ncmul* a b c))) 0)
	       (t (new-rat tem))))
	((polynomialp b)(poly-ncmul1 a b c))
	((rational-functionp b)(cons (poly-ncmul1 a (num b) c) (denom b)))
	(($ratp b)(new-rat-ncmul1 a (cdr b) c))
	(t (new-rat-ncmul a (new-rat b) c))))
;(defun $check_associative (a b c &aux tem term1 term2 answer)
;  (cond ($free_dot
;	 (cond ($fast_dotsimp
;		(cond
;		  ($new_fast_dotsimp
;		   (setq term1 (new-rat-ncmul1 1 (remove-header ($dotsimp  (ncmul* a b))) c))
;		   (setq term2 (new-rat-ncmul1  a (remove-header ($dotsimp  (ncmul* b c))) 1))
;		   (setq tem  (n- term1 term2))
;;		   (check-rat-order tem)
;		   (setq answer ($dotsimp (header-poly tem)))
;		   (format t "~%Here is the associator")
;		   (displa answer)
;		   answer)
;		  (t
;
;		   (setq term1 (ncmul1 1 ($dotsimp ($vrat (ncmul* a b))) c))
;		   (setq term2 (ncmul1  a ($dotsimp ($vrat (ncmul* b c))) 1))
;		   (setq tem  (vsub* term1 term2))
;		   (check-rat-order tem)
;		   ($dotsimp tem))))))
;
;	 (t
;	   (setq tem  ($ratsimp
;			($dotsimp ($ratsimp
;				    (sub*  (ncmul*
;					     ($simp_ncmul a b) c)
;					   (ncmul* a ($simp_ncmul b c))))))))))

;;;works out a little simpler in the new notation. ;;see new-dotsimp.lisp
;(defun $check_associative (a b c &aux tem tem1 tem2)
;;  (show (dotsimp (n. a b)))
;;  (show (dotsimp (n. b c)))
;;  (show (n. a (dotsimp (n. b c))))
;;  (show (setq hee(n.  (dotsimp (n. a b)) c)))
;  (setq tem1 (dotsimp(n. (dotsimp (n. a b)) c)))
;  (setq tem2 (dotsimp(n. a (dotsimp (n. b c)))))
;;  (show tem1 tem2)
;  (setq tem (n- tem1 tem2))
;  (cond ((pzerop tem) tem)
;	(t (header-poly tem))))

;(defmacro simp-zerop (n )
;  `(cond ((numberp ,n) (zerop ,n))
;	 ((atom ,n) nil)
;	 (t (cond ((member (caar ,n) '(mrat rat) :test #'eq)
;		   (cond ((eq (second (car ,n)) 'simp)
;			  (equal (cdr ,n) (rzero)))
;			 (t (zerop (setq ,n ($ratsimp ,n)))
;			    (format t "~having to Ratsimp ~A" ,n))))
;		   (t (and (numberp (setq ,n ($ratsimp ,n)))(zerop ,n)))))))

(defvar *previously-checked-pairs* nil)
(defvar $global_dimension_three nil)
(defvar $rank_function nil)

(defun two-times-n (n) (cond ((> n 4) (* 2 n))
			     (t 0)))
(defun rank-dimension-three-modulo-cubic (n)
  (cond ((< n  4) 0)
	(t (- (rank-dimension-three n) (rank-dimension-three (- n 3))))))

(defun rank-1-1-1-9 (n)
  (cond ((< n 9) (polynomial-ring-1-1-1 n))
	(t
	 (- (* n 9)  27))))

(defvar *all-rank-functions* '($global_dimension_3
			       rank-dimension-three
			       two-times-n rank-dimension-three-modulo-cubic
			       polynomial-ring-1-1-1 three-times-n
			       $standard_gorenstein rank-1-1-1-9 ))

(defun $check_overlaps (up-to-degree &optional (add-to-simps nil)
					       (maybe-reset t) (from-degree nil)
					       &aux tem to-replace test-list tem2 bef-overlap
					       lowest-degree   deg
					       tem1 assoc-list (ok t)
					       list-of-degrees-to-mod-out
					       dim reset-monomials)
  (or *previously-checked-pairs*
      (setq *previously-checked-pairs*
	    (make-hash-table :test 'equal)))
  (cond (maybe-reset
	 (cond ((y-or-n-p "Reset *previously-checked-pairs* ?")
		(or *previously-checked-pairs*
		    (setq *previously-checked-pairs*
			  (make-hash-table :test 'equal)))
		(clrhash *previously-checked-pairs*)
		))
	 (cond ((y-or-n-p "use Hilbert for rank function")
		(user-supply list-of-degrees-to-mod-out)
		(setq $rank_function (hilbert-modulo list-of-degrees-to-mod-out)))
	       ((y-or-n-p "Set the $Rank_function to a non nil value?")
		(loop for v in *all-rank-functions*
		      do
		      (format t "~%Use ~A ?" v)

		      (cond ((y-or-n-p ) (setq $rank_function v)(return 'done))
			    (t nil))
		      finally (format t
				      "~%Enter a value for $rank_function:")
		      (setq $rank_function  (read))))
	       (t (setq $rank_function nil)))
	 (format t "~%~%The current variables are:")(displa $current_variables)
	 (cond ((not (y-or-n-p "Keep the current variables?"))
		(setq reset-monomials t)(format t "~%Enter a Macsyma list:")
		(setq $current_variables  (mread-noprompt *standard-input*)))))

	(t (format t "
~%Starting to check overlaps without resetting *previously-checked-pairs*")))
  (show $rank_function)
  (setq to-replace
	(loop for mon in $dot_simplifications
	      for i from 0
	      when (and (oddp i) (not (atom mon)))
	      collecting mon))
  (loop for mon in to-replace
	minimize (setq deg ($nc_degree mon)) into the-min
	maximize deg into the-max
	finally (setq lowest-degree the-min)
	(cond ((not (numberp up-to-degree))
	       (setq up-to-degree (1- (* 2 the-max))))))

  (cond (from-degree (setq lowest-degree  from-degree)))
 (setq deg lowest-degree)


  (loop named angela
    while (<= deg up-to-degree)
    for deg from (1+ lowest-degree) to up-to-degree

    do
    (cond ($rank_function
	(format t "~%current variables are")
	(displa $current_variables)
	(loop while (< deg (1+ up-to-degree))
	      do
	      (setq dim (1- (length
				($mono $current_variables  deg reset-monomials))))
	      (format t
		      "~%There are ~A independent monomials in degree ~A and rank function is ~A" dim deg (funcall $rank_function deg))
	      (cond ((<= dim (funcall $rank_function  deg))
		     (setq lowest-degree  (min up-to-degree deg)) (setq deg (1+ deg)))
		    (t (return 'done))))))
    (cond ((<= deg up-to-degree)
    (loop
      for right1 in to-replace
      do
      (setq bef-overlap (- deg ($nc_degree right1)))
      (loop
	for left in to-replace
	do
	(loop while (< deg-so-far bef-overlap)
	      for v in (cdr left)
	      for ii from 2
	      summing ($nc_degree v) into deg-so-far
	      do
	      (cond ((and (= deg-so-far bef-overlap)
			  (setq tem2 (nthcdr ii left))
			  (initial-equal tem2 (cdr right1)))
		     (setq tem (nthcdr (- (length left)
					  ii) (cdr right1)))
		     (setq test-list (append (cddr left) tem))
		     (cond ((or
			      (contains-a-replacement
				(cddr (butlast test-list)))
			      (ordered-pair-in-list left right1
						    *previously-checked-pairs* ))
			    nil )
			   (t  (setf (gethash
					      (list left right1) *previously-checked-pairs*)
				    t)
			      (setq assoc-list
				    (list (ncmuln (cdr (subseq left 0 ii)) t)
					  (ncmuln (nthcdr ii left) t)
					  (ncmuln tem t)))
			      (format t "~%Checking the overlap for")
			      (displa (list '(mlist simp) left right1))
			      (setq tem1 (apply  '$check_associative
						 assoc-list))
			      (cond (($zerop tem1)
				     (format t "~%The overlap was associative."))
				    (t
				     (setq ok nil)
				     (format t "~%The overlap was not associative.")
				     (displa (cons '(mlist simp) assoc-list))
				     (cond (add-to-simps
					    (convert-relation-to-dot-simp tem1)))
				     (cond ($fast_dotsimp ($rat_the_dot_simplifications)))
				     ($check_overlaps
				       up-to-degree
				       add-to-simps nil lowest-degree)
				     (return-from angela 'done)))))))))))))
  $dot_simplifications)

(defun rank-dimension-three (n)
  (cond ((oddp n)(div* (* (1+ n) (+ n 3)) 4))
	(t (div* (* (+ 2 n) (+ 2 n)) 4))))

;  (cond ((null ok)
;	(cond ((y-or-n-p "Would you like to try again?")
;	      (format t "~%Up to what degree would you like to test?
;  ~%Enter a number: ")
;		   (setq  up-to-degree (read))
;		   ($check_overlaps up-to-degree t)))))

(defvar *temp-pair* (list nil nil))

(defun ordered-pair-in-list (a b a-list-of-pairs)
  (let ((lis *temp-pair*))
    (setf (car lis) a (second lis) b)
    (gethash  lis a-list-of-pairs)))

(defmacro spsafe (&rest ll)
  (loop for u in ll
	do
	(cond ((get u 'special)(format t "~%***Warning**** ~A is a special" u))
	      (t (format t "~%~A is not special" u)))))

;;used &quote before but since never use old method of relations now.

(defun $set_up_dot_simplifications ( relations &optional check-thru-deg
				    (ordering $order_function))
  (setq $dot_simplifications '((mlist simp)))
  (show $order_function)
  (cond (($listp (setq relations (meval* relations)))(setq relations (cdr relations))))
  (loop for relat in relations
	do
	 (convert-relation-to-dot-simp relat ordering))
  (format t "~%The new dot simplifications are set up")
  (cond (check-thru-deg
	 (displa $dot_simplifications)
	 ($check_overlaps check-thru-deg t)))
  $dot_simplifications)

(defun $tes (relat) (convert-relation-to-dot-simp relat))





(defun convert-relation-to-dot-simp (relat &optional (ordering $order_function) &aux
				     cof worst  )
  ordering
  (cond ($fast_dotsimp (setq relat ($dotsimp relat )))              ;;($vrat relat))))
	((null $free_dot)(setq relat (meval* relat)))
	(t (setq relat ($dotsimp relat))))
  (cond
    (($zerop relat ) nil)
    ((numberp relat) (merror "Adding a constant relation  will result in the trivial algebra"))
    (t
     (cond
       ($new_fast_dotsimp
	(setq relat (num (cdr relat)))
	(setq worst (get (car relat) 'disrep)
	      cof (third  relat))
	(setq relat  (nred relat cof))
       (setq relat (n-  worst relat))                ;;; should take cdddr etc. not subtractt

	(setq relat (header-poly relat)))
;;;;	(setq relat (vsub* worst relat)))
       (t
	(multiple-value (worst cof )(find-worst-nc-monomial relat))
	(cond ((null $fast_dotsimp)
	       (setq relat (div* relat cof))
	       (setq relat
		     ($ratsimp (sub* worst relat))))
	      (t
	       (setq relat (vdiv* relat cof))
	       (setq relat (vsub* ($vrat worst) relat))
;;;            (setq relat (check-rat-order relat))
	       (setq relat (minimize-varlist relat))))))
     (format t "~%Adding  ")
     (displa (cons '(mlist simp) (list worst relat)))
     (format t "    to dot_simplifications")
     ($add_to_simps    (list  worst relat)))))           ;;;;(copy-tree relat))))))

(defvar $all_dotsimp_denoms nil)

(defun convert-relation-to-dot-simp (relat &optional (ordering $order_function) &aux
				       tem tem1)
  ordering
  (cond ((or (null $fast_dotsimp) (null $free_dot))
	 (fsignal "old code")))
  (setq relat ($dotsimp relat))
  (cond (($zerop relat) nil)
	(t (setq relat (cdr relat))
	   (and (consp (num relat))
		(consp (setq tem (third (num relat))))
		(progn (format t "~%Denom : ")
		       (displa (setq tem1 (new-disrep tem)))
		       (if $all_dotsimp_denoms
			   (nconc $all_dotsimp_denoms (list tem1)))))
	   ($add_to_simps (setq tem (make-simp (num relat))))
	   (format t "~%Adding to simps: ")(displa (cons '(mlist) tem)))))



;;       ;;;Cre form examples;;;
;;
;;   The global variables varlis and genvar are the third and fourth
;;items in the the (car expr) of expr if expr is in cre form.  The (cdr expr)
;;contains the actual information with its numerator being the car  and
;;the denominator the cdr.  When you add two expressions  expr1 and expr2 using add* the
;;current value of varlist and genvar are used.  If you put varlist equal to
;;nil then, ratrep* and its friend newvar will splice together the lists of
;;variables and set them up.  Prep1 does the actual calling of ratplus on the
;;cdr of the new expr1 and expr2.  They will have a common varlist which,
;;if the global varlist was nil, would be just the alphabetical splicing of
;;the two varlists.  Otherwise the global varlist would form the end of the
;;new varlist.  Since we usually just want to splice them, we introduce vadd*,etc.
;;which locally put the varlist to nil.  This method involves no ratdisrepping,
;;and so is quite fast.  The elements of genvar get reused again and again, unless
;;you specify genvar nil say, or somehow hide the old elements of genvar.
;;
;;   The elements of genvar sometimes get a 'disrep property, after disrepping.
;;This will not happen if you were not displaying intermediate results etc.
;;The elements of genvar get assigned the numerical values 1,2 ,3,4,...  as shown
;;below.
;;
;;   $gcd does not cause disrepping and can be used to find the common factor
;;of a numerator and denominator, so as to have the same effect as ratsimp
;;but without disrepping.
;;
;;   The order of varlist when splicing is alphabetical.  Thus if you want
;;to have zeta before a use %zeta or @zeta.  Otherwise you would have to add
;;a to varlist first, and that would give it precedence over b etc.  Note
;;that lists come after any atom in alphalessp order.
;;
;;   I add v to the name if we let varlist be nil.  Thus ($vrat '$a) will cause
;;the result to have only a varlist of ($a) .  The genvar will be the global
;;genvar.
;;
;;  The function minimize-varlist removes all elements from varlist and genvar
;;which don't appear in the expression.
;;
;;
;;
;;                                                     numerator     denominator:
;;((MRAT SIMP ($B) (#:G0558 #:G0557 #:G0556 #:G0555)) (#:G0558 1 1) . 1)  the cdr is 1
;;
;;             B
;;
;;             varlist       genvar                             numerator starts
;((MRAT SIMP ($A $B $C $D) (#:G0558 #:G0557 #:G0556 #:G0555))(#:G0556 1
;;                                                                     (#:G0557 1
;;                                                                              1
;;                                                                              0
;;                                                                              (#:G0558 1 1))
;;                                                                      0
;;                                                                      2)
;;                                                             #:G0555  <--denominator starts
;;                                                             2           and is cdr
;;                                                             1
;;                                                             0
;;                                                             1)
;;
;;          (B + A) C + 2
;;          -------------
;;              2
;;             D  + 1
;;
;;
;;   G0558 disreps $B value 1
;;   G0557 disreps $B value 2
;;   G0556 disreps $C value 3
;;   G0555 disreps $D value 4
;;
;;
;;   If one is going to do a lot of computations with entries of a matrix
;;say which all have more or less the same variables occurring one could
;;prepare them all to have the same varlist and genvar as follows.  Go
;;through them collecting the total varlist and sorting it into the right
;;order.  Call it new-varlist. You could at the same time collect
;;genvar's.  Now let varlist be the new collected one and genvar be the
;;collected one.  Then do (setq expr (cons (standard) (prep1 expr))) for
;;each entry expr, where standard is (list 'mrat simp new-varlist
;;new-genvar).  Then outside everything put a (let ((genvar new-genvar))
;;
;;   Prep1 does not disrep expr if the global varlist contains all
;;(varlist expr) in the right order, and if the global genvar
;;is as long as the varlist.  Note that rattimes will not automatically
;;simplifying such things as %i^2, so one may still want to just use vmul*.
;;Prep1 does not alter (cdr expr) if the varlist and genvar of expr are equal to
;;the global genvar and varlist.
;
;(defun vadd* (&rest llist)
;  (let ((varlist))
;    (apply 'add*  llist)))
;(defun vsub* (a b )
;  (setq b (vmul* -1 b))
;  (vadd* a b))
;(defun vsub* (a b )
;  (vadd* a (vminus* b)))
;

;(defun vmul* (&rest a-list))
;



;;;the following convert all to rat form using $new_rat unless given a polynomialp or $ratp



;(setq $new_fast_dotsimp t)
(defun vadd* (&rest a-list)
  (cond ((null a-list) 0)
	((eq (length a-list) 1) (car a-list))
	(t (header-poly (n+ (car a-list) (apply 'vadd* (cdr a-list)))))))
(defun vmul* (&rest a-list)
  (cond ((null a-list) 1)
	((eq (length a-list) 1) (car a-list))
	(t (header-poly (n* (car a-list) (apply 'vadd* (cdr a-list)))))))
(defun vsub* (a b) (header-poly (n- a b)))
(defun vdiv* (a b) (header-poly (nred  a b)))
(defun vminus* (a)(header-poly (n- 0 a)))
(defun $vrat (&rest a-list)
  (apply '$new_rat a-list))




;;second
;(setq $new_fast_dotsimp nil)


(defun $vrat (&rest a-list)
  (let ((varlist))
    (apply '$rat  a-list)))
(defun vadd* (&rest a-list)
  (let ((varlist ))
    (cond ((< (length a-list) 3)(apply 'add* a-list))
	  (t (vadd* (car a-list) (apply 'vadd* (cdr a-list)))))))
(defun vmul* (&rest a-list)
  (let ((varlist ))
    (cond ((< (length a-list) 3)(apply 'mul* a-list))
	  (t (vmul* (car a-list) (apply  'vmul* (cdr a-list)))))))
(defun vsub* (a b)
  (let ((varlist))
    (sub* a b)))
(defun vdiv* (a b)
  "This needs to be fixed.  It still allows ratdisrep"
  (let ((varlist))
   (simplifya  (list '(mquotient) a b) nil)))
(defun vminus* (a)
  (let ((varlist))
    (simplifya (list '(mminus) a) nil)))

;(defun $add_relation_to_dot_simplifications( relat &optional (ordering $order_function))
;  (convert-relation-to-dot-simp relat ordering))
(defun $add_to_simps (a-list &optional(ordering $order_function) )
  (cond (($listp a-list)(setq a-list (cdr a-list))))
  (cond ((and (zerop $expop)(null $fast_dotsimp))
	 (setf (second a-list) ($multthru (second a-list)))))
  (loop for u in $dot_simplifications
	for i from 0
	when (and (oddp i) (not (funcall ordering  (car a-list) u)))
	do (setq $dot_simplifications (append (subseq $dot_simplifications 0 i)
					       a-list
				       (nthcdr i $dot_simplifications)))
	(return 'done)
	finally (setq $dot_simplifications (append $dot_simplifications a-list)))
  ($simplify_dot_simplifications ($nc_degree (car a-list))))

(defmacro $set(a b)
  (let (($dot_simplifications nil))
    `(setq ,a  ' ,(meval* b))))

(defmacro $with_no_simp (form)
  (let (($dot_simplifications nil))
    `(progn  ' ,(meval* form))))

(defun $inverse_modulo (n modulo-p)
  "The inverse of any number n modulo-p.  ok if n negative, but n=0 gives 0
and modulo-p not prime gives false answer"
  (mod (expt  n (- modulo-p 2)) modulo-p))

(defun $nu (i llist &aux
  (p1  (length llist)))
  (let ((fact ($inverse_modulo i p1)))
  (loop for ii from 1 below p1
	collecting
	(nth (mod (* ii fact) p1) llist) into tem
	do (show tem ii)
	finally (return (cons '(mlist simp) tem)))))
(defun $phi (l )
  (check-arg l '$listp "macsyma list")
  (setq l (cdr l))
  (let ((p0 (length l)))
    (show p0)
    (add*  (mul* p0 (loop for i from 1 to (1- p0)
			  collecting (mul* (nth i l) (power '$sig (mod (- i) p0)))
			  into tem
			  finally (return (meval* (cons '(mplus) tem))))
		 (car l)
		 (mul* (- p0) (loop for i from 1 to (1- p0)
					collecting (nth i l) into tem
					finally (return (meval* (cons '(mplus) tem)))))))))


(defvar $prime_order nil)

(defun $sig (n &optional (p0 $prime_order))
  (power '$sig (mod n p0)))

(defun $nu_poly (i poly)
  ($ratsimp (subst ($sig i $prime_order) '$sig poly)))

(defun $scalar_concat (&rest l)
  (let ((tem (apply '$concat l)))
    (putprop tem '(nil $scalar t) 'mprops)
    tem))

(defun $rel (i j)
  (let ((ii (max i j)) ans (jj (min i j)))
    (cond ((not (equal i j))
	   (setq ans (list (ncmul* ($concat '$e ii) ($concat '$e jj))
			   (sub* ($scalar_concat '$a jj ii)
				 (ncmul* ($concat '$e jj) ($concat '$e ii))))))
	  (t (setq ans (list (ncmul* ($concat '$e ii) ($concat '$e jj))
			   ($scalar_concat '$a jj ii)))))
    (cons '(mlist simp) ans)))

(defun $clifford_dot_simplifications (n )
  (loop for i from 1 to n
	appending
	(loop for j from 1 to i
	      appending (cdr ($rel i j)))
	into tem
	finally (return (cons '(mlist simp) tem))))


(defun $replacements (&aux (tem (cdr $dot_simplifications)))
  (loop while tem
	collecting (car tem) into a-list
	do (setq tem (cddr tem))
	finally (return (cons '(mlist simp) a-list))))


(defvar $list_of_zeroes nil)

(defun $earlier_mono (monom &optional (variables $current_variables) &aux answer)
  (setq answer ($mono variables ($nc_degree monom)))
  (loop for v in (cdr answer)
	 when (funcall $order_function v monom)
	 collecting v into tem
	 finally (return (cons '(mlist simp) (sort tem $order_function)))))

(defun parse-string (a-string)
  (cond ((null (or (search "$" a-string :test #'char-equal) (search ";" a-string :test #'char-equal)))
	 (setq a-string  (string-append a-string "$"))))
  (with-input-from-string (stream a-string)
    (let ((*standard-input* stream) answer)
	 (setq answer (caddr (mread)))
	   answer)))
(defmacro   mac (a-string )
  "Parses a Macsyma string but does not call meval*"
      (list 'quote (parse-string  a-string)))

(defmacro   mac-eval-after (a-string )
  "Parses a Macsyma string but evaluates it at run time"
      (list 'quote `(meval* ,(parse-string  a-string))))
(defmacro   mac-eval (a-string )
  "Reads a macsyma string and evaluates it at compile time returning the quoted form"
      (list 'quote (meval* (parse-string  a-string))))

(defun $hh (a)
  (cond ((atom a) nil)
	(t (equal (caar a) 'mtimes))))

(defun $monomial_dimensions (n &optional (order-weight nil)  &aux j  the-count)
  (cond ($current_variables
	 (format t "~%The current variables are")
	 (displa $current_variables)
	 (cond ((y-or-n-p "Use them?"))
		(t (setq $current_variables nil)))))
  (cond ($current_variables nil)
	(t (format t "~%Enter Macsyma list of variables to use:") (setq $current_variables
						      (caddr (mread-raw *standard-input*)))))
  (cond (order-weight
	 (loop for j from 1 to n
	       do (setq the-count 0)
	       (loop for i from 1 to n
		     do
		     (loop for v in (cdr ($mono $current_variables i))
			   when (eq ($nc_degree v :order-weight) j)
			   do (incf the-count)))
	       (format t "~%There are ~A monomials of order weight ~A and of degree less than ~A"
		       the-count j n)
	       summing the-count into tem
	       finally (format t "~%The total number of monomials of weight and degree less than ~A is ~A." n (1+ tem)))))

  (loop for i from 1 to n
	do
	(format t "~%There are ~A independent monomials in degree ~A."
		(setq j (length  (cdr ($mono $current_variables i)))) i)
		collecting (list '(mlist) i j ) into tem
	summing j into the-sum
  finally (format t "~%The sum of the dimensions from dimension 0 through dimension ~A is ~A"
		  n (+ the-sum 1)) (return (cons '(mlist simp) tem))))


(defun $mono_weighted (n &aux the-list)
  (loop for i from 1 to n
	do
	(loop for v in (cdr ($mono $current_variables i))
	      when (eq ($nc_degree v :order-weight) n)
	      do (setq the-list (cons v the-list)))
	finally (return (cons '(mlist) the-list))))

(defmacro subscript (d i)
  `((,d  :array) ,i))
(defvar $last_equations nil)
(defvar $last_solutions nil)
(defun $find_relations_among (a-list &optional (dotsimp-first nil) (term-names nil)&aux eqns gen-sum answers)
  (check-arg a-list '$listp "macsyma list")
  (cond (dotsimp-first (setq a-list
			     (loop for v in (cdr a-list)
			     collecting ($dotsimp v) into tem
			     finally (return (cons '(mlist) tem))))))
  (setq gen-sum ($general_sum  a-list $aaaa))
  (cond ((null dotsimp-first) (setq gen-sum ($dotsimp gen-sum))))
  (setq $last_equations (setq eqns ($nc_coefficients ($ratsimp `((mlist) , gen-sum)))))
  (setq answers (apply '$fast_linsolve (cdr eqns)))
  (setq $last_solutions answers)
  (cond ((null term-names)(setq term-names (loop for i from 1 to (length (cdr a-list))
				       collecting ($concat '$term i) into tem
				       finally (return (cons '(mlist) tem))))))
  ($sublis answers ($general_sum term-names
				 $aaaa)))




(deff $te #'$power_series_monomial_alphalessp)

(defun $check_skew_relations (&optional(variables $current_variables) &aux tem answer)
  (loop for  u in (cdr variables)
	unless ($must_replacep u)
	do
	(loop for v in (cdr variables)
	      when (and (funcall $order_function u v) (not (equal u v)))
	      do
	      (cond (($must_replacep (setq tem (ncmul* v u))) nil)
		    (t (format t "~%no replacement for ~A" tem)
		       (setq answer (cons tem answer)))))
	finally (cond ((null answer)(return t))
		      (t (cons '(mlist) answer)))))

(defun get-alt (item a-list)
  (loop while a-list
	when (equal item (car a-list))
	do(return (second a-list))
	else do (setq a-list (cddr a-list))))

(defun $collect_skew_relations (&optional(variables $current_variables) &aux tem  tem2)
  (loop for  u in (cdr variables)		;a
	unless ($must_replacep u)
	appending
	(loop for v in (cdr variables)
	      when (and (funcall $order_function u v) (not (equal u v))
		   (setq tem2 (get-alt (setq tem (ncmul* v u)) (cdr $dot_simplifications))))
	      collecting tem
	      and
	      collecting tem2 )
	into a-list
	finally (return (cons '(mlist simp) a-list))))

(defun $relations_from_dot_simps(&optional (dot-simps $dot_simplifications))
  (let ((a-list (cdr dot-simps)))
	(loop
	      while a-list
	      collecting  (sub* (first a-list) ($totaldisrep (second a-list))) into tem
	      do
	      (setq a-list (cddr a-list))
	      finally (return (cons '(mlist) tem)))))
(defun repl-deg ()
  (sort (mapcar '$nc_degree (cdr ($replacements))) 'alphalessp))

(defun $pb (n)
  (mfuncall '$playback (list '(mlist) n 1000)))

(defvar $centrals_so_far nil)

(defun $find_central_thru_deg (begin to-n &aux tem)
  (displa $current_variables)
  (setq $centrals_so_far nil)
  (loop for i from begin  to to-n
	do
	($check_overlaps (1+ i) t nil i)
	(setq $centrals_so_far
	      (cons (list '(mlist) i
			  (setq tem (mfuncall '$central_elements $current_variables i)))
		    (cdr $centrals_so_far)))
	(maclist $centrals_so_far)
	(format t "~%Central in degree ~A " i)
	(displa tem))
  $centrals_so_far)

(defmacro maclist (x)
  `(setq ,x (cons '(mlist) ,x)))

(defmacro rat-variable-info (cre-form)
  `(car ,cre-form))

(defun fast-scalarp (x)
  (cond ((atom x)
	 (cond ((numberp x) t)
	       ((symbolp x) (mget x '$scalar))
	       (t ($scalarp x))))
	(t
	 (case (caar x)
	   (mnctimes nil)
	   (mrat
	    (let ((gen-vars (fourth (rat-variable-info x))))
	      (loop for vv in gen-vars
		     when (not (fast-scalarp (get vv 'disrep)))
		     do
		     (cond ((appears-in  (cdr x) vv) (return nil)))
		     finally (return t))))
	   (otherwise ($scalarp x))))))

(defun my-ratcoeff (expr monom &optional (deg 1) &aux answer )
 (setq answer
       (catch 'bad-variable-order
    (cond ((atom expr)(cond ((eq expr monom) 1)
			  (t 0)))
	((and (equal( caar expr) 'mrat) (eq (second (car expr)) 'simp))
	 (my-ratcoeff1 expr monom deg))
	(t ($ratcoef expr monom)))))
 (cond ((eq answer 'bad-order)(format t "~%Variable ~A is out of order so using $ratcoef."
				      monom)
	($ratcoef expr monom deg))
       (t answer)))

(defun my-ratcoeff1 (expr monom &optional (deg 1))
  (let ((var-list (varlist expr))
	(gen-vars (genvar expr)))
    (loop for v in var-list
	  for w in gen-vars
	  when (equal v monom)			;
	  do
	  (cond ((not (appears-in (num (cdr expr)) w)) (return 0))
		(t
		 (return (cons (rat-variable-info expr) (cons (my-ratcoeff2 (num (cdr expr))
									    w
									    deg)
							      (denom (cdr expr)))))))
	  finally (return 0))))
;
;(defun my-ratcoeff2(rat-expr gen-var &optional (deg 1))
;  (cond ((atom rat-expr) (rzero))
;
;	(t (let ((rat-num rat-expr))
;						; (gshow rat-num)
;	     (cond ((eq (car rat-num) gen-var)
;		    (setq rat-num (cdr rat-num))
;		    (loop while rat-num
;			  when (eq (car rat-num) deg)
;			  do
;			  (return  (second rat-num) )
;			  when (eq (car rat-num) 0)
;			  do
;			  (return (my-ratcoeff2 (second rat-expr) gen-var))
;			  else
;
;			  do
;			  (setq rat-num (cddr rat-num))))
;		   (t (setq rat-num (cdr rat-num))
;;		      (break t)
;		    (loop while rat-num
;			    when (eq (car rat-num) 0)
;			    do
;			    (return (my-ratcoeff2 (second rat-num) gen-var))
;			    else
;
;			    do (setq rat-num (cddr rat-num))
;;			    (gshow rat-num)
;			    )))))))


(defun my-ratcoeff2(rat-expr gen-var &optional (deg 1))
  (cond ((atom rat-expr) (rzero))

	(t (let ((rat-num rat-expr))
						; (gshow rat-num)
	     (cond ((eq (car rat-num) gen-var)
		    (setq rat-num (cdr rat-num))
		    (loop while rat-num
			  when (eq (car rat-num) deg)
			  do
			  (return  (second rat-num) )
			  when (eq (car rat-num) 0)
			  do
			  (return (my-ratcoeff2 (second rat-expr) gen-var))
			  else

			  do
			  (setq rat-num (cddr rat-num))))
		   (t (setq rat-num (cdr rat-num))
;		      (break t)
		    (loop while rat-num
			    when (eq (car rat-num) 0)
			    do
			    (return (my-ratcoeff2 (second rat-num) gen-var))
			    else
			    do
			    (cond ((appears-in (cadr rat-num) gen-var)
				   (throw 'bad-variable-order 'bad-order))
				  (t(setq rat-num (cddr rat-num))))
;			    (gshow rat-num)
			    )))))))
;
;(defun $numerator (expr)
;;  (check-arg expr '$ratp "macsyma cre form")
;
;  (cond
;    ((numberp expr) expr)
;    ((mbagp expr)(cons (car expr) (mapcar '$numerator (cdr expr))))
;    (($ratp expr) (cons (car expr)(cons (cadr expr) 1)))
;        ((equal (caar expr) 'mtimes)
;		 (loop for v in (cdr expr)
;		       unless  (and (not (atom v))
;				  (equal (caar v) 'mexpt)
;;				  (show (third v))
;				   (<  (third v) 0))
;		       collecting v into tem
;		       finally (return (cons '(mtimes) tem))))
;	(t ($numerator ($vrat expr)))))


(defun $denominator (expr &aux tem1)
  (cond (($ratp expr)
	 (cons (car expr)(cons (cddr expr) 1)))
	((equal (caar expr) 'mtimes)
		 (loop for v in (cdr expr)
		       when  (and (not (atom v))
				  (equal (caar v) 'mexpt)
;				  (show (third v))
				   (<  (third v) 0))
		       collecting (progn
				    (setq tem1 (copy-list v))
				    (setf (third tem1) (- (third v)))
				    tem1)
				  into tem
		       finally (return (simplifya (cons '(mtimes) tem) nil))))
	(t ($denominator ($vrat expr)))))
(defun $te (x y)(my-ratcoeff x y))
(defun variables-same-divide (f g)
  (cons (car f) (car (ratdivide (cdr f) (cdr g)))))
(defun variables-same-divide (f g)
  (vdiv* f g))
(defun simplify-rational-quotient (expr &aux the-gcd )
  "Divides top and bottom of a cre expression by their gcd"
  (check-arg expr '$ratp "Cre form")
  (let ((the-num ($numerator expr))
	(the-denom ($denominator expr)))
    (cond ((equal (cdr the-denom) (cons 1 1))
	   expr)
	  (t (setq the-gcd ($gcd the-num the-denom))
	     (setq the-num (variables-same-divide the-num the-gcd))
	     (setq the-denom (variables-same-divide the-denom the-gcd))
	     (append (list (car expr) (num (cdr the-num))) (num (cdr the-denom)))))))



(defun two (x) x 2)

(defmvar $skew3_conditions)
(defvar $bbbb)
(defvar $cccc)

(defun $check_skew3_conditions ( relations  &aux monoms3 ($expop 100))

  (setq relations
	(list '(mlist)  (div*  (nth 1 relations)
			      ($coeff (nth 1 relations) (ncmul* '$y '$x '$x)))
	      (div*  (nth 2 relations)
			      ($coeff (nth 2 relations) (ncmul* '$y '$y '$x)))))
  (displa relations)
  (let (($dot_simplifications nil)(result t) coeff-values answer )
    (setq monoms3 ($mono '((mlist) $x $y) 3))
    (setq coeff-values
      (loop for relat in (cdr relations)
	  for coeff-names in (list $bbbb $cccc)
	  appending
	  (loop for u in (cdr monoms3)
		for j from 1
		collecting `((mequal) ,(nth j coeff-names) ,($coeff  relat u)))
	  into tem
	  finally (return (cons '(mlist) tem))))
    (setq answer ($sublis coeff-values $skew3_conditions))
    (loop for v in (cdr answer)
	  when (not ($zerop (sub*  (nth 1 v) (nth 2 v))))
	  do (format t "~%Inconsistent condition ")
	  (displa v)
	  (setq result nil))
    (cond ((null result)(format t "~%Trying to interchange f and g...")

	   ($check_skew3_conditions (list '(mlist) (reverse (cdr relations)))))
	  (t result))))
(defun $list_variables (expr &rest key-strings &aux string-v tem)
  (cond ((null key-strings) ($listofvars expr))
	(t
	 (loop for v in key-strings
	       collecting (string-trim "&" v) into tem
	       finally (setq key-strings tem))
	 (loop for v in (cdr (setq tem ($listofvars expr)))
	       do (setq string-v (string v))
	       when (not (loop for key in key-strings
			       when  (search key string-v :test #'char-equal)
			       do (return t)))
	       do (setq tem (delete v tem :test #'equal))
	       finally (return tem)))))
(defun $socle (variables deg &aux  f unknowns eqns tem1 parameters answer )
  (setq variables (cons '(mlist) (sort (cdr variables) $order_function)))
  (let* ((monoms ($mono variables deg))
	(monoms-higher ($mono variables (1+ deg))))

    (setq f ($general_sum monoms $aaaa))
    (loop for v in (cdr variables)
	  do
  	  (setq unknowns ($list_variables f "aa" "par"))
	  (setq parameters (loop for vv in (cdr unknowns)
				 when (search "par" (string vv) :test #'char-equal)
				 collecting vv))
	  (setq tem1 (cdr $aaaa))
	  (loop for vv in parameters
		do (loop while tem1
			 when (not (member (car tem1) unknowns :test #'eq))
			 do (setq f (subst (car tem1) vv f))
			 (setq unknowns (subst (car tem1) vv unknowns))
			 (format t "~%Replacing ~A by ~A in f." vv (car tem1))
			 (setq tem1 (cdr tem1)) (return 'done)
			 do (setq tem1 (cdr tem1))))
	  (setq tem1 (list '(mlist) (ncmul* f v) (ncmul* v f)))
	  (setq eqns ($extract_linear_equations tem1 monoms-higher))
	  (show unknowns)
	  (setq answer ($fast_linsolve eqns unknowns))
	  (setq f (meval* ($sublis answer f)))
	  (displa f)
	  finally (return f))))



(defvar $commutators '((mlist)))

(defvar $centralizers  '((mlist)))

(defun $fast_normal_elements (deg &key (variables $current_variables) up_to_deg
				  auto &aux  f unknowns eqns tem1 parameters answer tem )
  "Returns elements F in degree DEG in variables, which satisfy u.F=F.phi(u)
  where phi is the auto  which defaults to the identity and can be specified by
  the list of images of VARIABLES"
     (cond ((null auto) (setq auto variables)))
     (setq tem (sort (pairlis (cdr variables) (cdr auto)) #'$order_function :key #'car))
     (setq variables (cons '(mlist) (mapcar 'car tem)) auto (cons'(mlist)
							      (mapcar 'cdr tem)))
  (setq $commutators nil $centralizers nil)
  (let* ((monoms (cond (up_to_deg (cons '(mlist) (loop for i from 1 to deg appending (cdr  ($mono variables i)))))
		       (t ($mono variables deg))))
;	(monoms-higher ($mono variables (1+ deg)))
	)
    (setq f ($general_sum monoms $aaaa))
    (loop for v in (cdr variables)
	  for im in (cdr auto)
	  do
  	  (setq unknowns ($list_variables f "aa" "par"))
	  (setq parameters (loop for vv in (cdr unknowns)
				 when (search "par" (string vv) :test #'char-equal)
				 collecting vv))
	  (setq tem1 (cdr $aaaa))
	  (loop for vv in parameters
		do (loop while tem1
			 when (not (member (car tem1) unknowns :test #'eq))
			 do (setq f (subst (car tem1) vv f))
			 (setq unknowns (subst (car tem1) vv unknowns))
			 (format t "~%Replacing ~A by ~A in f." vv (car tem1))
			 (setq tem1 (cdr tem1)) (return 'done)
			 do (setq tem1 (cdr tem1))))
	  (setq tem1 (list '(mlist)($totaldisrep ($dotsimp (sub* (ncmul* v f) (ncmul* f  im))))))
	  (setq $commutators (append `((mlist) ,v ,(second tem1)) (cdr $commutators)))
;	  (setq eqns ($extract_linear_equations tem1 monoms-higher))
  	  (setq eqns ($extract_linear_equations tem1 ($list_nc_monomials tem1)))
	  (show unknowns)
	  (setq answer ($fast_linsolve eqns unknowns))
	  (setq f ($ratsimp ($sublis answer f)))
  	  (setq $centralizers (append `((mlist) ,v ,f) (cdr $centralizers)))
	  (displa f)
	  finally (return ($separate_parameters f)))))
(defun $fast_central_elements (variables deg  &aux  f unknowns eqns tem1 parameters answer )
  (setq variables (cons '(mlist) (sort (cdr variables) $order_function)))
  (setq $commutators nil $centralizers nil)
  (let* ((monoms ($mono variables deg))
;	(monoms-higher ($mono variables (1+ deg)))
	)

    (setq f ($general_sum monoms $aaaa))
    (loop for v in (cdr variables)
	  do
  	  (setq unknowns ($list_variables f "aa" "par"))
	  (setq parameters (loop for vv in (cdr unknowns)
				 when (search "par" (string vv) :test #'char-equal)
				 collecting vv))
	  (setq tem1 (cdr $aaaa))
	  (loop for vv in parameters
		do (loop while tem1
			 when (not (member (car tem1) unknowns :test #'eq))
			 do (setq f (subst (car tem1) vv f))
			 (setq unknowns (subst (car tem1) vv unknowns))
			 (format t "~%Replacing ~A by ~A in f." vv (car tem1))
			 (setq tem1 (cdr tem1)) (return 'done)
			 do (setq tem1 (cdr tem1))))
	  (setq tem1 (list '(mlist) ($com f v)))
	  (setq $commutators (append `((mlist) ,v ,(second tem1)) (cdr $commutators)))
;	  (setq eqns ($extract_linear_equations tem1 monoms-higher))
  	  (setq eqns ($extract_linear_equations tem1 ($list_nc_monomials tem1)))
	  (show unknowns)
	  (setq answer ($fast_linsolve eqns unknowns))
	  (setq f ($ratsimp ($sublis answer f)))
  	  (setq $centralizers (append `((mlist) ,v ,f) (cdr $centralizers)))
	  (displa f)
	  finally (return ($separate_parameters f)))))

(defun $central_elements_in_degrees (&rest degrees &aux $centrals_so_far)
  (loop for i in degrees do
    ($check_overlaps (1+ i) t nil)
    (push ($fast_central_elements $current_variables i)
			    $centrals_so_far)
		      (mapcar 'displa $centrals_so_far))
  (cons '(mlist) $centrals_so_far))


(defun $pbi (n &aux tem)
  (let ((linel (- linel 10)))
    (loop for i from n below $linenum
	  do  (format t "~% ~4A: ~A" (string-trim "$" (string (setq tem ($concat '$c i))))
		      (string-grind (meval* tem))))))

(defvar *pbi-string* (make-array '(64) :element-type 'standard-char
				 :fill-pointer 0 :adjustable t))

(defun new-concat (a &rest b &aux me)
  (loop for v in b collecting
	(format nil "~A" v) into tem
	finally (setq me (apply 'string-append tem))
	(return (intern (format nil "~A~A" a me) 'maxima))))


(defun $list_sublis (a-list b-list expr)
  (cond (($listp a-list) (setq a-list (cdr a-list))))
  (cond ( ($listp b-list )(setq b-list (cdr b-list))))
  (loop for v in a-list
	for w in b-list
	collecting (cons v w) into tem
	finally (return (sublis tem expr))))


(defun string-grind (x &key ($display2d) stream &aux answ)
  (setq answ(with-output-to-string (st)
	      (cond ((null $display2d)  (mgrind x st))
		    (t  (let ((*standard-output* st))
			  (displa x))))))
  (setq answ (string-trim '(#\newline #\space  #\$) answ))
  (cond (stream  (format stream "~A" answ)) ;
	(t answ)))

(defun macsyma-typep (x)
  "acceptable to displa"
  (or (numberp x)
      (and (consp x)(consp (car x))(get (caar x) 'dimension))))

;If foo is (#:X2 1 1 0 (#:X1 1 1)) for instance, then
;(format t "The functions  ~VQ  are the inverses" foo 'fsh)
;(format t "~%The functions  ~VQ  are  inverses" (st-rat #$[x1+1,1/ (x1+1)]$)  'fsh)
;The functions  X1+1 and 1/(X1+1)  are  inverses

(defun fsh ( x &optional (stream *standard-output*))
  (let ($display2d)
    (cond
      ((macsyma-typep x)(string-grind x :stream stream))
      ((or (polynomialp x) (rational-functionp x))
       (string-grind (header-poly x) :stream stream))
      ((and ( listp x)(or (polynomialp (car x)) (rational-functionp (car x))))
       (case (length x)
	 (1 (fsh x stream))
	 (2 (fsh (first x) stream) (format stream " and ") (fsh (second x) stream))
	 (t
	   (loop for v in  x
		 for i from 1 below (length x)
		 do (fsh v stream) (format stream ",")
		 finally (format stream " and ") (fsh (car (last x)) stream)))))
      ((and (listp x) (macsyma-typep (car x)))
       (format stream "(")
       (loop for v in x do (fsh v) (format t " ")
	     finally (format t ")")))
      (t (des x)))))
