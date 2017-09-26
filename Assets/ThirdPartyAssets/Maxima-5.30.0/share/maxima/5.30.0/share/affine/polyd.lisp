;;; -*- Mode:Lisp; Package:CL-MAXIMA; Syntax:COMMON-LISP; Base:10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defvar $last_answer nil)
(defvar *show-specials* nil)

(defun which-centrals-to-add (&aux *previously-checked-pairs* ($dot_simplifications))
  "Checks you aren't adding a trivial product of centrals etc.
It affects the dot simplifications which become the dot simplifications
modulo ones-to-add checked thru the highest degree of ones to add."
					;  (unwind-protect
  (progn
    (loop for v in (cdr $centrals_so_far)
	   until (eq (length ones-to-add) 3)
	   do
	   (cond (($zerop ($dotsimp v))(setq v 0))
		 (t (format t "~%Supposedly checking overlaps to deg ~a" ($nc_degree v))
		    (setq $rank_function (hilbert-modulo
					  (append  ones-to-add (list v))))
		    (setq *previously-checked-pairs* nil)
		    ($check_overlaps ($nc_degree  v) t nil)))
	   when (not ($zerop ($dotsimp v)))
	   collecting v into ones-to-add
	   and do ($add_relation_to_dot_simplifications v)
	   finally (return ones-to-add))))
;    nil)
 ; (setq $dot_simplifications dot-simps)))

(defun find-number-of-centrals
       (n &optional(from-degree 2) (to-degree 10) (reset-centrals-so-far nil))
  "Finds at least  n centrals if there are n thru to-degree
It also checks overlaps and so affects the dot_simplifications.  It stores
them in $centrals_so_far.  If later asked it won't check below the centrals so
far degree"
  (cond (reset-centrals-so-far (setq $centrals_so_far '((mlist)))))
  (cond ((> 1 (length $centrals_so_far))
	 (setq from-degree (1+ (apply 'max from-degree
				     (mapcar '$nc_degree (cdr $centrals_so_far)))))))
  (loop for i from (1+ from-degree) to (1+ to-degree)
	do ($check_overlaps i t nil (1- i ))
;	(show i )(format t "~~%*****************")
;        (displa $centrals_so_far)
	(loop for j from 1 to i
	      when
	      (and $rank_function
		   (not (eql ($length ($mono $current_variables  j))
			(funcall $rank_function j))))
	      do
	      (format t "~%Using Current Variables of..")(displa $current_variables)
	      (format t "   and rank function ~A." $rank_function)
	      (throw 'check_a_case
		      (format nil "In spite of checking overlaps to degree ~A the monomial dimensions were ~A and the rank functions was ~A in degree ~A.  We conclude not finite global dimension." i ($length ($mono $current_variables  j))
			      (funcall $rank_function j) j)))
	(setq $centrals_so_far
	      ($append $centrals_so_far
		       ($fast_central_elements
			 $current_variables (1- i))))
	(format t "~%Central elements thru degree ~A" (1- i))
	(displa $centrals_so_far)
	when (>= ($length $centrals_so_far) n)
	do
	(return 'done)))

(defun monomials-and-rank-function-agree (n &aux mono rank)
  (loop for i from 1 to n
	do
	(setq mono ($length ($mono $current_variables i))
	      rank (funcall $rank_function i))
	(format t "~%In degree ~A the monomial dimension is ~A and the rank function ~A."
		i mono rank)
	(cond ((not (eql mono rank))(return nil)))
	collecting mono into tem
	summing mono into total
	finally (push 1 tem) (push "total" tem) (push (incf total) tem)
	(format t "~%The total is ~A." total)(return  tem)))


(eval-when
    #+gcl (load)
    #-gcl (:load-toplevel)
  (cond ((not (member 'hilbert *all-rank-functions* :test #'eq))
	 (push 'hilbert *all-rank-functions*))))

(defun $check_a_case
       (relations  &optional (to-degree 10) (rank-function '$global_dimension_3)
	(variables nil)
	&aux tem ones-to-add dims answer free-dot-simps full-dot-simps)
  "look for centrals thru degree to-degree then mod them out"
  (setq $centrals_so_far '((mlist)))
  (cond (variables (setq $current_variables variables))
	(t (setq $current_variables ($list_variables relations "X" "Y" "Z"))
	   (loop for v in (cdr $current_variables)
		 when (or ($scalarp v) (search "%" (string v) :test #'char-equal))
		 do (setq $current_variables (delete v $current_variables :test #'equal)))))
  (show $current_variables)
  (funcall '$set_up_dot_simplifications  relations )
  (setq full-dot-simps $dot_simplifications)
  (setq answer
	(catch 'check_a_case
	  (setq free-dot-simps $dot_simplifications)
	  (loop for i from 3 until (or (eq i 6) (eq (length ones-to-add) 3))
		;; 6 ;;enough?
		do
		(setq $dot_simplifications full-dot-simps)
		(setq $rank_function rank-function)
		(setq *previously-checked-pairs* nil)
		;;you've changed them in ones-to-add
		(find-number-of-centrals i 2 (1+ to-degree))
		;;gets centrals thru to-degree.
		;;may throw if not finite global dim.
		(setq full-dot-simps $dot_simplifications)
		;;should be free ones to deg i
		(setq ones-to-add (which-centrals-to-add))
		;; dotsimps from which.. stay
		finally
		(cond ((>= (length ones-to-add) 3)
		       (setq to-degree ($nc_degree (car (last ones-to-add))))))
		(setq $rank_function
		      (hilbert-modulo ones-to-add))
		(cond (ones-to-add
		       (cond((setq dims
				   (monomials-and-rank-function-agree
				     (floor (1+ (* 2.2 ($nc_degree (car (last ones-to-add))))))))
			     (return nil))))
		      (t (return (setq answer
				       (list '(mlist) relations
					     (format nil "There are no centrals up to degree ~A"
						     to-degree)
					     ($ratsimp $dot_simplifications))))))
		;;hopefully we don't get here and tessing worked.
		(format t "~%~%HOW DID WE GET HERE?~%")
		(setq *previously-checked-pairs* nil)
		(funcall '$set_up_dot_simplifications
			 (append relations ones-to-add ))
		($check_overlaps 12 t nil))))
  (cond (answer answer)
	(t (cond ((null dims)
		  (setq dims
			(with-no-query
			  (cdr ($monomial_dimensions 15))))))
	   (setq $last_answer
		 (list '(mlist)
		       "Relations:" relations
		       "Centrals added:" (cons '(mlist) ones-to-add)
		       "Monomial dimensions:" (cons '(mlist) dims)
		       (format nil "All Centrals to Degree ~A:" to-degree)
		       $centrals_so_far
		       "Dot Simplifications:"
		       ($ratsimp $dot_simplifications) ))))
  (format t "~%Conclusion:~%The algebra is a finite module of rank ~D over a polynomial ring.
 The polynomial ring is generated by elements in degrees ~D .  The actual generators are "  (second (nth 6 $last_answer)) (mapcar '$nc_degree (cdr $centrals_so_far)))
  (displa $centrals_so_far)
  (format t "~%The dimension of the spaces of monomials in degrees 0 through ~D are~%"
	  (1-  (length (setq tem (cdddr (delete 0 (nth 6 $last_answer) :test #'equal))))))
  (format t "  ~A "  tem)
  'qed)

(defvar $last_answer nil)
(defun te (&aux tem)
 (format t "~%Conclusion:~%The algebra is a finite module of rank ~D over a polynomial with 3 generators.
 The polynomial ring is generated by 3 elements in degrees ~D .  The generators are "  (second (nth 6 $last_answer)) (mapcar '$nc_degree (cdr $centrals_so_far)))
;  (displa $centrals_so_far)
  (format t "~%The dimension of the spaces of monomials in degrees 0 through ~D are~%"
	(1-  (length (setq tem (cdddr (delete 0 (nth 6 $last_answer) :test #'equal))))))
 (format t "  ~A "  tem) 'qed)

(defun $check_programs (list-relations)
  (loop for v in (cdr list-relations)
	do (reset-vgp)
	collecting ($check_a_case v) into tem
	finally (return (cons '(mlist) tem))))


(defun new-convert-relation-to-dot-simp (relat  &optional (ordering $order_function)
				       (then-simplify nil) &aux
				     cof worst  )
	  (setq relat ($dotsimp relat))
	  (setq relat (num (cdr relat)))
	  (setq worst (get (car relat) 'disrep)
		cof (third  relat))
	  (setq relat  (nred relat cof))
	  (setq relat (n-  worst relat))     ;;; should take cdddr etc. not subtractt
	  (setq relat (header-poly relat))
	  (format t "~%Adding  ")
	  (displa (cons '(mlist simp) (list worst relat)))
	  (format t "    to dot_simplifications")
	  (setq $dot_simplifications (append $dot_simplifications (list worst relat)))
	  ($sort_dot_simplifications ordering)
	  (cond (then-simplify ($simplify_dot_simplifications)))
	  $dot_simplifications)

;;This is only for $new_fast_dotsimp  tried to avoid calling itself and
;;added an unwind protect so that the dotsimps don't get screwed up if
;;you abort part way thru.
(defun $simplify_dot_simplifications (&optional (from-degree 0) &aux
				      relat resimplify dot-simps leng)
  (check-arg $new_fast_dotsimp (eq $new_fast_dotsimp t) "Use the old $simplify_dot_simplifications in polynomials.lisp for other dotsimps")
   ($sort_dot_simplifications)
     (format t "~%starting to resimplify dot simplifications..")
     (cond ((< (setq leng ($length $dot_simplifications)) 10) (displa $dot_simplifications))
	   (t (format t "~%There are ~A of them." (truncate leng 2))))

   (setq dot-simps $dot_simplifications)
   (loop named sue for v on (cdr $dot_simplifications) by #'cddr
	 for i from 1 by 2
	 do
	 (unwind-protect
	   (progn
	     (setq $dot_simplifications (append (firstn i $dot_simplifications) (cddr v)))
	     (cond ((and ;(not ($zerop (second v)))
			 (or ($must_replacep (car v))($must_replacep (second v))))
		    (setq relat (cond ($new_fast_dotsimp
				       (header-poly (n- (car v) (second v))))
				      (t (vsub* (car v) (second v)))))
		    (setq relat ($dotsimp relat))
		    (cond (($zerop relat))
			  (t (new-convert-relation-to-dot-simp relat )))
		    (setq dot-simps $dot_simplifications)
		    (setq resimplify t)
		    (return-from sue 'start-over))))
	   (setq $dot_simplifications dot-simps)))
   (cond (resimplify ($simplify_dot_simplifications))
	 (t (format t "~%They were OK")))
   $dot_simplifications)

(defun $monomial_and_degree_lessp (x y)
  (let ((x-deg ($nc_degree x)) (y-deg ($nc_degree y)))
    (cond ((> x-deg y-deg) t)
	  ((eq x-deg y-deg)($monomial_alphalessp x y))
	  (t nil))))

(defun mono-dimension (n) ($length ($mono $current_variables n)))

(defun hilbert (n &rest l)
  (cond ((< n 0) 0)
	((eq n 0) 1)
	((null l)($global_dimension_3 n))
	(t (- (apply 'hilbert n (cdr l))
	      (apply 'hilbert (- n (car l)) (cdr l))))))

;(defun $global_dimension_3 (n)
;  (case ($length $current_variables)
;    (2 (rank-dimension-three n))
;    (3 (polynomial-ring-1-1-1 n))))


(defun our-dimensions (n)
 "for 2 3 and 4 variables we have unique sequences so we plug them in here."
  (case (length (cdr $current_variables))
	(0 (fsignal "no variables"))
	(1 1)
	(2 (dimension-from-sequence '(1 2 2 1) '(1 2 1) n 2))
	(3 (dimension-from-sequence '(1 3 3 1) '(1 1 1) n 3))
	(4  (dimension-from-sequence '(1 4 6 4 1) '(1 1 1 1) n 4))
	(t (fsignal "not handled yet"))))

(defun $global_dimension_3 (n) (our-dimensions n))

(defun dimension-from-sequence
       (list-powers list-degree-maps degree number-variables &aux ( answ 0))
  "the sequences start from the right tail."
  (check-arg list-powers (eql (car list-powers) 1) "first should be one")
  (cond ((< degree 0) 0)
	((zerop degree) 1)
	((eq 1 degree) number-variables)
	;;(< degree degree-relations) (expt  number-variables degree))
	(t
	 (loop for deg-map in  list-degree-maps
	       for u in (cdr list-powers)
	       for i from 0
	       with j = degree
	       while (>= j 0)
	       do (setq j (- j deg-map))	;(show (list degree j answ))
	       (setq answ (+ answ (* (expt -1 i) u  (dimension-from-sequence list-powers list-degree-maps j
									  number-variables ))))
;	       (show answ)(show u)
	       finally (return (abs answ))))))

(defun $sum_hilbert (&rest l &aux tem)
  (loop for i below 100  until (zerop (setq tem (apply #'hilbert i 3 l)))
	summing tem))

;;for creating a one argument function that can be funcalled
(defun $hilbert_modulo (l)
  (check-arg l $listp "macsyma list")
  (funcall 'hilbert-modulo (cdr l)))

(defun hilbert-modulo (l)
  (cond ((or (null l) (numberp (car l)))
	 `(lambda (n) (apply 'hilbert n ',(copy-list l))))
	(t (hilbert-modulo  (mapcar #'$nc_degree l)))))

(defun $tes (relat)
  (new-convert-relation-to-dot-simp relat $order_function t))

(defun $add_relation_to_dot_simplifications (relat )
  (new-convert-relation-to-dot-simp relat $order_function t))

(defun $rat_the_dot_simplifications (&aux res v)
 "Does the new-rat for New_fast_dotsimp"
  (loop for w on (cdr $dot_simplifications) by #'cddr
	do (push (car w) res)
	(setq v (cadr w))
	(push (if ($ratp v) (header-poly (new-rat (ratdisrep v))) (header-poly  (new-rat v)))
		  res)
	)
  (setq $dot_simplifications (cons '(mlist) (nreverse res))))

;
;
;
;
;(defun x$-macro-read (&optional  subchar
;		      (stream *standard-input*) arg &aux ( meval-flag t))
;
;  (declare (ignore subchar)) subchar
;  (setf (fill-pointer *my-stream* ) 0)
;  (cond ((eq #\$ (tyipeek t stream))(send stream :tyi)
;	 (setq meval-flag nil)))
;  (with-output-to-string (st *my-stream*)
;    (let (char)
;      (loop while (not (eql char #\$))
;	    do
;	    (setq char (send stream :tyi))
;	    (send st :tyo char)
;	    finally (cond ((not (eql  char #\$))
;			   (zwei:barf "There was no matching $" ))))))
;  (cond (meval-flag
;	 (list 'meval* (list 'quote   (parse-string *my-stream*))))
;	(t  (list 'quote   (parse-string *my-stream*)))))
;
;(si:SET-SYNTAX-/#-MACRO-CHAR #\$ 'x$-macro-read)

;;the following will get the arg x of h into the list before the meval*.
;;it is hard to see any other way of accomplishing this since the form must
;;;be evaluated by simplifya etc, and so we need to have the
;(defun h (x)
; (declare (special x))       ;;these all work including the binding of x to ?x.
;       #$3*[?x,y,z]$)        ;; maybe better form to use (defun h ($x) and avoid the
;(setq me #$$y.y.y$)
;
;;;probably want one of the preceding with meval* built in and one without.
;(defun h (x)
;  #$factor(x^2+x+1)$)
;
;(defun h ($x $y)
;  (declare (special $x $y))
;  #$x^y$)
;
;       "~%Declaring special~
; ~#[none ~; ~A ~; ~A and ~A ~; ~@{~#[~1; and ~] ~A~^,~}~]."
;		       tem)
(eval-when
    #+gcl (load compile)
    #-gcl (:load-toplevel :compile-toplevel)
  (defvar *show-specials* nil))

(defmacro def$fun (&body l)
  "Will declare special any variables in the lambda list beginning with $
 This is useful in conjunction with the #$ reader macro so that variables bound
 in the arglist  will get declared and so can be referred to in the meval"
  (setq l (copy-list l))

  (loop for v in (second l)
	when  (and (symbolp v) (eq (char v 0) #\$))
	collecting v into tem
	when   (and (listp v) (symbolp  (setq v (car v))) (eq (char  v 0) #\$))
	collecting v into tem
	finally
	(cond (*show-specials*
	(apply 'format t "~@{~%The variable ~A has been declared special for scoping~}" tem)))
	(return `(defun ,(car l) ,(second l)
			   ,@ (cons `(declare (special ,@ tem))
				    (cddr l))))))

;
;(def$fun te ($x &aux $i (u 3) ($z 4))
;   (loop for  $i below $z
;	 collecting #$y^i+3*x^z$))


;;;this expands into
;(DEFUN TE ($X &AUX (U 3) ($Z 4))
;    (DECLARE (SPECIAL $X $Z))
;    (LET ((Y 3)) (MEVAL* '((MPLUS) $X 1))))
;(te 2)==> 3
;(def$fun te ($x n &aux $i)
;	 (loop for $i below n
;	       collecting #$x^i+3+y^i$
;	       into tem
;	       finally (return (cons '(mlist) tem))))
;;expands to which does collect the right thing.
;(DEFUN TE ($X N &AUX $I)
;    (DECLARE (SPECIAL $X $I))
;    (LOOP FOR
;          $I
;          BELOW
;          N
;          COLLECTING
;          (MEVAL* '((MPLUS) ((MEXPT) $X $I) 3 ((MEXPT) $Y $I)))
;          INTO
;          TEM
;          FINALLY
;          (RETURN (CONS '(MLIST) TEM))))
;(def$fun te ($x n &aux $i)
;	 (loop for $i below n
;	       collecting #$x^i+3+y^i$
;	       into tem
;	       finally (return (cons '(mlist) tem))))
;;expands to which does collect the right thing.

(defun $fast_central_elements_given_commutator (variables deg comut   used_var
					       &aux  f unknowns eqns tem1 parameters answer )
  (setq variables (cons '(mlist) (sort (cdr variables) $order_function)))
  (setq $commutators nil $centralizers nil)
  (let* ((monoms ($mono variables deg))
;	(monoms-higher ($mono variables (1+ deg)))
	  monoms-higher)
    (setq f ($general_sum monoms $aaaa))
    (setq unknowns ($list_variables f "aa" "par"))
;    (setq f prev-comut)
    (setq tem1 `((mlist) ,comut))                    ;;; (list '(mlist) ($com f v))
;	  (setq $commutators (append `((mlist) ,v ,(second tem1)) (cdr $commutators)))
;	  (setq eqns ($extract_linear_equations tem1 monoms-higher))
    (setq monoms-higher ($list_nc_monomials tem1))
    (setq eqns ($extract_linear_equations tem1 monoms-higher ))
    (show unknowns)
     (break t)
    (setq answer ($fast_linsolve eqns unknowns))
    (setq f ($ratsimp ($sublis answer f)))
    (setq $centralizers (append `((mlist) xy ,f) (cdr $commutators)))
    (displa f)
    (loop for v in (nthcdr ($length used_var) (cdr variables))
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
  	  (setq $centralizers (append `((mlist) ,v ,f) (cdr $commutators)))
	  (displa f)
	  finally (return ($separate_parameters f)))))



(defun $read_Lisp_string (a-string)
  (eval (read-from-string (string-trim "&" a-string))))


(defun $try (u &aux tem centrals)
;  (funcall '$set_up_dot_simplifications (append $relations (list u)))
  ($add_relation_to_dot_simplifications u)
  (setq centrals nil)
  (setq *previously-checked-pairs* nil)
  (setq $rank_function nil)
  ($check_overlaps 10 t nil)
  (loop for i from 1 below 8
	when (setq tem (cdr  ($fast_central_elements $current_variables i)))
	do (loop for v in tem do ($add_relation_to_dot_simplifications v))
	(setq centrals (append centrals tem))
	($check_overlaps 10 t nil)
	finally (return (cons '(mlist) centrals))))

(defun list-variables (expr &aux *all-vars*)
    (declare (special *all-vars*))
  (list-variables1 expr)
  *all-vars*)

(defun list-variables1 (expr)
  (declare (special *all-vars*))
   (cond   ((atom expr) nil)
	   ((polynomialp expr)(pushnew (p-var expr) *all-vars*)
	    (loop for (deg cof) on (cdr expr) by #'cddr
		  do (list-variables1 cof)))
	   ((rational-functionp expr)(list-variables1 (car expr))
	    (list-variables1 (cdr expr)))
	   (t (loop for v in expr do (list-variables1 v)))))

(defun reset-rat (tree &aux tem)
  (cond ((atom tree)
	 (cond ((and (symbolp tree)
		     (setq tem (get tree 'disrep)))
		(add-newvar tem))
	       (t tree)))
	(t (setf (car tree) (reset-rat (car tree)))
	   (setf (cdr tree) (reset-rat (cdr tree))))))

(defun npfactor (p  )
  (let ((varl (list-variables p)))
    (let ((genvar (reverse (sort varl #'pointergp))))
      (pfactor p))))

(defun nplcm (x y &aux (genvar *genvar*))
  (plcm x y))

(defun npgcd (f g)
  (let ((varl (list-variables (list f g))))
    (let ((genvar (reverse (sort varl 'pointergp))))
      (pgcd f g))))
