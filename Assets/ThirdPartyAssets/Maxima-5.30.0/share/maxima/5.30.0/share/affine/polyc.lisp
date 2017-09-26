;;;; -*- mode: lisp; package: cl-maxima; syntax: common-lisp;  -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)


;;(newvar expr) will add in a sorted manner all the new variables to the beginning
;;of the varlist.

(defvar *genpairs* nil)

(defun reset-vgp ()  (setq *genpairs* nil  *genvar*  nil  *varlist* nil)
       (setq *xxx*
	     (let ((*nopoint t) *print-radix*)
	       (loop for i from 1 to 30 collecting
		      (add-newvar (intern (format nil "$X~A" i))))))
       'done)


(defun show-vgp ()
  (show *genpairs* genpairs *genvar* genvar *varlist* varlist))

(defvar vlist nil)

(defun put-tellrat-property (va the-gensym)
  (loop for v in tellratlist
	when (equal va (car v))
	do (putprop the-gensym (cdr v) 'tellrat)))


(defun poly-ncmul1 (mon   poly  mon1 &aux monom new-monom tem gen-sym)
  (cond ((null mon)(setq mon 1))
	((null mon1) (setq mon1 1)))
  (cond ((not (polynomialp poly))(break t)))

  (cond ((and (numberp mon)(numberp mon1))
	 (n* poly (n* mon mon1)))
	((or (numberp poly)($scalarp (setq monom (get (car poly) 'disrep))))
	 (setq gen-sym (add-newvar (setq monom (ncmul* mon mon1))))
	 (n* (list gen-sym 1 1) ;;(assolike monom *genpairs*);;might be long list to look in
	     poly))
	(t (setq new-monom (ncmul* mon monom mon1))
	   (cond ((contains-a-zero-replacement new-monom)
		  (cond ((and (eq (second poly) 1)(eq (fourth poly) 0))
			 (poly-ncmul1 mon (fifth poly) mon1))
			((eq (second poly) 1) 0)
			(t (merror "There is a bad order in nc polynomial ~A" poly))))
		 (t
		  (setq gen-sym (add-newvar new-monom))
		  (cond ((and (eq (second poly) 1)(eq (fourth poly) 0))
			 (setq tem (poly-ncmul1 mon (fifth poly) mon1))
			 (cond
			   ((eq tem 0)(list gen-sym 1 (third poly)))
			   (t
			    (list gen-sym 1 (third poly) 0
				   tem))))
			((eq (second poly) 1)
			 (list gen-sym 1 (third poly)))
			(t (merror "There is a bad order in nc polynomial ~A" poly))))))))


;the above checks for terms in the radical and zero simplifications.
;(defun poly-ncmul1 (mon   poly  mon1 &aux monom new-monom gen-sym)
;  (cond ((null mon)(setq mon 1))
;	((null mon1) (setq mon1 1)))
;  (cond ((not (polynomialp poly))(break t)))
;
;  (cond ((and (numberp mon)(numberp mon1))
;	 (n* poly (n* mon mon1)))
;        ((or (numberp poly)($scalarp (setq monom (get (car poly) 'disrep))))
;	 (setq gen-sym (add-newvar (setq monom (ncmul* mon mon1))))
;	 (n* (list gen-sym 1 1) ;;(assolike monom *genpairs*);;might be long list to look in
;	     poly))
;	(t (setq new-monom (ncmul* mon monom mon1))
;
;	   (setq gen-sym (add-newvar new-monom))
;	   (cond ((and (eq (second poly) 1)(eq (fourth poly) 0))
;		  (list gen-sym 1 (third poly) 0 (poly-ncmul1 mon (fifth poly) mon1)))
;		 ((eq (second poly) 1)
;		  (list gen-sym 1 (third poly)))
;		 (t (merror "There is a bad order in nc polynomial ~A" poly))))))
;(defun new-rat-ncmul (a nc-rat-expr c)
;  (cons (poly-ncmul1 a (num nc-rat-expr) c) (denom nc-rat-expr)))

(defun poly-ncmul (&rest ll)
  "broken"
  (loop for v in ll when (not (polynomialp v))do (break t))
  (cond ((null ll) 1)
	((eq (length ll) 1) (car ll))
	((numberp (car ll))(n* (car ll) (apply 'poly-ncmul (cdr ll))))
	((polynomialp (car ll))
	      (apply
				  'poly-ncmul (poly-ncmul1 1 (car ll) (second ll))
				  (nthcdr 2 ll)))
	((polynomialp (second ll))
	 (cond ((polynomialp (third ll)) (merror "poly-ncmul can't have two 'polynomialp objects in its arg yet"))
	       (t (apply 'poly-ncmul
				 (poly-ncmul1 (car ll)  (second ll) (third ll))
				 (nthcdr 3 ll)))))))

(defun reset-gen () (setq genvar nil varlist nil genpairs nil))

(defun show-vg (&optional (varl *varlist* ) (genv *genvar*))
  (loop for v in varl
	for w in genv
	do (format t "~%~A ~A disrep ~A value ~A" w v (get w 'disrep) (symbol-value w))
	when (not (equal (get w 'disrep) v))do (format t" **** bad disrep ***")
	when (not (equal (get-genvar v) w)) do (format t" **** bad *genpair***~A" (assolike v *genpairs*)))
  (format t "~%~A and ~A are the lengths of genv and varl " (length genv) (length varl)))


(defun last2 (a-list)
  (loop for v on a-list
	while (cddr v)
	finally (return v)))

(defun lastn (n a-list)
  (loop for v on a-list
	while (nthcdr n v)
	finally (return v)))



(defun choose (m n)
 (quotient (factorial m) (* (factorial n) (factorial (- m n)))))

(defun polynomial-ring-1-1-1  (deg)
	(choose (+ deg 2) deg))

(defun three-times-n (n) (* 3 n))

(defun list-equations1 (expr)
  "converts a single macsyma equation into a lisp list of cons's suitable for sublis"
  (cond ((and (listp (car expr))(eq (caar expr) 'mequal))(setq expr (cdr expr)))
	(t (merror "not a equation ~A " expr)))
  (cond ((atom (first expr))(list (cons (first expr) (second expr))))
	((member (caar (first expr)) '($matrix mlist) :test #'eq)
	 (loop for v in (cdr (first expr))
	       for w in (cdr (second expr))
	       appending (list-equations1 (list v w))))))

(defun list-equations-macsyma1 (expr)
  (cond ((and (listp (car expr))(eq (caar expr) 'mequal))
	 (cond ((atom (second expr)) (list expr))
	       ((not (member (caar (second expr)) '($matrix mlist) :test #'eq)) (list expr))
	       (t (setq expr (cdr expr))
		  (cond ((member (caar (first expr)) '($matrix mlist) :test #'eq)
			 (cond ((or (atom (second expr))(not (eql (caar (first expr))
							    (caar (second expr)))))
				(loop for v in (cdr (first expr))
				      appending (list-equations-macsyma1
						  (list '(mequal) v (second expr)))))
			       (t
			 (loop for v in (cdr (first expr))
			       for w in (cdr (second expr))
			       appending (list-equations-macsyma1
					   (list '(mequal) v w))))))))))))


(defun $list_factors(poly)
  (cond
    ((atom poly ) poly)
    ((mbagp poly) (cons (car poly) (mapcar #'$list_factors (cdr poly))))
    ((eq (caar poly) 'mtimes)
     (cons '(mlist) (cdr poly)))
    (t poly)))

(defun $list_equations (a-list)
  (check-arg a-list '$listp "macsyma list")
  (loop for v in (cdr a-list)
	appending (list-equations-macsyma1 v) into tem
	finally (return (cons '(mlist) tem))))

(defun $sub_list (eqns expr)
  (check-arg eqns '$listp "macsyma list")
  (setq eqns ($list_equations eqns))
  ($sublis eqns expr))

;  (setq eqns (loop for v in (cdr eqns)
;	appending (list-equations1 v) ))
;  (new-sublis eqns expr))

(defun new-sublis (subs expr &aux rat-form answer)
  (cond (($ratp expr)(setq expr ($ratdisrep  expr)) (setq rat-form t)))
    (cond ((mbagp expr)(cons (car expr)(mapcar #'(lambda (x) (new-sublis subs x))
					       (cdr expr))))
	  (t (setq answer (sublis subs expr))(cond (rat-form ($new_rat answer))
						   (t (meval* expr))))))

(defun remove-from-*genpairs* (expr)
  (setq *genvar* (delete (get-genvar expr) *genvar* :test #'equal))
  (loop for v in *genpairs*
	for i from 0
	when (equal (car v) expr)
	do
	(cond ((eq i 0)(setq *genpairs* (cdr *genpairs*)))
	      (t (setq *genpairs* (delete v *genpairs* :test #'equal)))))
  (setq *varlist* (delete expr *varlist* :test #'equal)))

;
;(defmacro must-replacep (symbol-to-set rat-poly)
;  `(let ((.chang.))
;     (multiple-value (,symbol-to-set .chang.)
;       (rat-polysimp ,rat-poly))
;     .chang.))


(defun pe (&aux tem)
    (format t "Enter a Macsyma expression:")
  (setq tem (mread-noprompt *standard-input*))
  (cdr ($new_rat (meval* tem))))

(defvar *fake-rat* '(mrat nil nil nil))
;
;(defun sh (expr)
;  (cond ((numberp expr)(displa expr))
;	((polynomialp expr)(displa (cons  *fake-rat* (cons expr 1))))
;	((rational-functionp expr)(displa (cons *fake-rat* expr)))
;	(t (displa expr)))
;  expr)


(defvar $homework_done '((mlist)))
(defquote $homework ( a-list &aux answer work )
  (loop for v in (cdr a-list)
	when (symbolp v)
	do (setq $homework_done (append $homework_done (list  (setq work (meval* v)) )))
	else
	do (setq $homework_done  (append $homework_done (list  (setq work  v))))
	do (format t "~%Working on ...") (displa work)
	do  (setq $homework_done (append $homework_done (list  (setq answer(meval* work)))))
	(displa answer))
  $homework_done)

(defun $eij (n i j)
  ($setelmx 1 i j ($ident n)))

(defun $unipotent_invariant_vectors (representation n1 n2 &aux answer eqns)
  "representation should take two arguments a (size n1) and b (size n2) where a is
   in GLn1 and b is a macsyma list of length n2. It returns the general vector b left invariant  by the upper triangular (unipotent)  matrices."
  (loop for i from 1 below n1
     appending (cdr ($list_equations
		      (list '(mlist)
			    (list '(mequal)
				  (mfuncall representation ($eij n1 i (1+ i))
					    ($firstn n2 $aaaa))
				  ($firstn n2 $aaaa))))) into tem
    finally (setq answer ($fast_linsolve (setq eqns (cons '(mlist) tem)) ($firstn n2 $aaaa))))

  ($separate_parameters ($sublis answer ($firstn n2 $aaaa))))
(defun $matrix_from_list (a-list &aux tem)
  (let ((n (round (setq tem (expt ($length a-list) .5)))))
    (cond ((eq (expt  n 2) ($length a-list)) 'fine)
	  (t (merror "The length of list is not a square")))
    (setq a-list (cdr a-list))
    (loop while a-list
	  collecting (cons '(mlist) (subseq a-list 0 n)) into tem1
	  do (setq a-list (nthcdr n a-list))
	  finally (return  (cons '($matrix ) tem1)))))

(defun kronecker-row-product (a b)
  (loop for v in (cdr b)
	appending
	(loop for w in (cdr a)
		 collecting (mul* v w))
	into tem
	finally (return (cons '(mlist) tem))))

(defun $kronecker_product (m n)
  (loop for row in (cdr n)
	appending
	(loop for rowm in (cdr m)
	      collecting (kronecker-row-product rowm row))
	into tem1
	finally (return (cons '($matrix) tem1))))

(defun $standard_rep (mat v)($list_matrix_entries(ncmul* mat v)))

(defun $unlist_matrix_entries (vect &optional size  )
  (cond (size nil)
	(t (setq size (round (expt  (length vect) .5)))))
  (setq vect (cdr vect))
  (loop for i below size
	collecting (cons '(mlist) (subseq vect 0 size)) into tem
	do (setq vect (nthcdr  size vect))
	finally (return (cons '($matrix) tem))))

(defun sub-list (a-list expr)
  (loop for v on a-list by 'cddr
	collecting `((mequal) ,(car v) ,(second v)) into tem
	finally (return ($sub_list (cons '(mlist) tem) expr))))
(defmacro defrep (name argu &rest body &aux v mat)
   (setq mat (first argu) v (second argu))
  (setq body (sublis (list (cons v 'gen-vector )(cons mat 'gen-mat )) body))
  `(defun ,name (,mat ,v &aux  info gen-mat gen-vector image)
     (remprop ',name 'representation)
     (cond ((setq info (get ',name 'representation))
	    (sub-list (list (first info) ,mat
			    (second info) ,v)
		      (third info)))
	   (t	 (setq gen-mat ($general_matrix (1- (length (second ,mat))) '$p))
		 (setq gen-vector (subseq $aaaa 0 (length ,v)))
		 (setq image (progn ,@body))
		 (putprop  '$tensor4 (list gen-mat gen-vector image) 'representation)
		 ($tensor4 mat v)))))

;(defrep sta (ma va)
; (ncmul* ma va))

(defun $tensor4 (mat v &aux info gen-mat gen-vector image)
  (cond ((setq info (get '$tensor4 'representation))

	 (sub-list (list (first info) mat
			 (second info) v)
		   (third info)))
	(t	 (setq gen-mat ($general_matrix (1- (length (second mat))) '$p))
		 (setq gen-vector (subseq $aaaa 0 (length v)))

		 (setq image ($tensor_product_representation
			       '$tensor2
			       4
			       '$tensor2
			       4
			       gen-mat gen-vector))
		 (putprop  '$tensor4 (list gen-mat gen-vector image) 'representation)
		 ($tensor4 mat v))))


(defun $tensor4 (mat v)($tensor_product_representation
			       '$tensor2
			       4
			       '$tensor2
			       4
			 mat v))

(defun $tensor2 (mat v)($tensor_product_representation
			 '$standard_rep
			 2
			 '$standard_rep
			 2
			 mat v))

(defun $tensor3 (mat v)($tensor_product_representation

			 '$tensor2
			 4
			 '$standard_rep
			 2
			 mat v))



(defun $tensor2 (mat v)($tensor_product_representation
			 '$standard_rep
			 3
			 '$standard_rep
			 3
			 mat v))

(defun $tensor3 (mat v)($tensor_product_representation

			 '$tensor2
			 9
			'$standard_rep
			 3

			 mat v))

(defun $tensor3 (mat v)($tensor_product_representation
			 '$standard_rep
			 3
			 '$tensor2
			 9
			 mat v))




(defun $sum5_standard_rep(mat v)
  (let ((size ($length mat)))
    (loop for i below 5
	  appending (cdr ($standard_rep mat ($firstn size v))) into tem
	  do (setq v (cons '(mlist) (nthcdr size (cdr v))))
	  finally (return (cons '(mlist) tem)))))

;;The representation of ei^Vej^Vek as a list is done by varying the first
;;index fastest thus if dim V = 2 then get
;;[e1^Ve1^Ve1,
;  e2^Ve1^Ve1,
;  e1^Ve2^Ve1,
;  e2^Ve2^Ve1,
;  e1^Ve1^Ve2,
;  e2^Ve1^Ve2,
;  e1^Ve2^Ve2,
;  e2^Ve2^Ve2]


;;the first two are in the above representation and the second two are for reversed
;;indexing where the subscripts vary the fastest at the right.(as usual in zeta lisp)

(defun list-ind-to-tensor-ind (dimv tensor-power ind)
  (setq ind (1- ind))
  (loop for i below tensor-power
	collecting (1+ (mod ind dimv) )
	do (setq ind (quotient ind dimv))))


(defun tensor-ind-to-list-ind (dimv &rest ll)
  (loop for m in ll
	for i from 0
	summing (* (expt  dimv i) (1- m)) into tem
	finally (return (1+ tem))))

(defun tensor-ind-to-list-ind (dimv &rest ll)
  (loop for m in ll
	for i to 0 downfrom (1- (length ll))
	summing (* (expt  dimv i) (1- m)) into tem
	finally (return (1+ tem))))

(defun list-ind-to-tensor-ind (dimv tensor-power ind)
  (setq ind (1- ind))
  (loop for i below tensor-power
	collecting (1+ (mod ind dimv) ) into tem
	do (setq ind (quotient ind dimv))
	finally (return (nreverse tem))))


(defun $tensor_product_representation (rep1 d1 rep2 d2 p w &aux mm1 mm2 )
  (setq mm1 ($find_matrix_of_operator
	      `(lambda (v) (mfuncall ',rep1 ',p v))
				      ($standard_basis d1)))
  (setq mm2 ($find_matrix_of_operator
	      `(lambda (v) (mfuncall ',rep2 ',p v))($standard_basis d2)))
  ($list_matrix_entries (ncmul* (mfuncall '$kronecker_product mm1 mm2) w)))
(defvar $binary_quartic_monomials
	($ratsimp (subst 'mtimes 'mnctimes ($commutative_dot_monomials '((mlist) $x  $y) 4))))

(defun $find_matrix_of_operator (operator basis )
  "operator acts on the space spanned by basis"
  (let (answer zero-b unknowns gen-a gen-sum gen-b)
    (setq gen-b ($firstn ($length basis) $bbbb))
    (setq gen-sum ($general_sum basis $bbbb))
    (setq answer ($fast_linsolve
		  ($list_equations
		   (list '(mlist)
			 (list '(mequal)
			       (mfuncall operator gen-sum)
			       ($general_sum basis (setq unknowns
							 (subseq $aaaa 0 (length basis)))))))
		  unknowns))
    (setq gen-a ($sublis answer unknowns))
    (setq zero-b (loop for w in (cdr gen-b)
		    collecting (cons w 0)))

    (loop for v in (cdr gen-b)
       collecting (meval* (sublis zero-b (subst 1 v  gen-a)))
       into tem
       finally (return
		 ($transpose  (cons '($matrix simp) tem))))))

(defun $find_matrix_of_operator (operator basis &aux answer zero-b
				 unknowns gen-a gen-sum gen-b rhs lhs eqns)
  "operator acts on the space spanned by basis"
  (setq gen-b ($firstn ($length basis) $bbbb))
  (setq gen-sum ($general_sum basis $bbbb))
  (setq lhs (mfuncall operator gen-sum))
  (setq rhs    ($general_sum basis (setq unknowns (subseq $aaaa 0 (length basis)))))

  (setq eqns ($list_equations (list '(mlist) (list '(mequal) lhs rhs))))
  (setq answer ($fast_linsolve eqns unknowns))
  (setq gen-a ($sublis answer unknowns))
  (setq zero-b (loop for w in (cdr gen-b) collecting (cons w 0)))
  (loop for v in (cdr gen-b)
	collecting (meval* (sublis zero-b (subst 1 v  gen-a)))
	into tem
	finally (return
		  ($transpose  (cons '($matrix simp) tem)))))

(defun $restriction_representation (repr elt-gln vector sub-basis)
  (let ((repr-of-p ($find_matrix_of_operator `(lambda (x) (mfuncall ',repr ',elt-gln x))
					     sub-basis)))
    (ncmul* repr-of-p vector)))
(defun $standard_basis (n)
  (cons '(mlist) (cdr ($ident n))))

(defun $find_highest_weight_vectors (rep n1 n2 &aux uni-vectors answer)

		 (setq uni-vectors     ($unipotent_invariant_vectors rep n1 n2))
	(setq answer	   ($diagonalize_torus rep n1 n2 uni-vectors))
	(list  '(mlist) uni-vectors answer))

(defvar *some-primes* '(2 3 5 7 11 13 17 19 23 29 31 37))

(defun $diagonalize_torus (representation n1 n2 basis &aux diag tem mat cpoly answer eigen-values weight-vector)
  "here operator has args a and b where a is in torus and b is a sum of elements of basis"
  (setq diag ($ident n1))
  (loop for i from 1 to n1
     do
       (setq diag ($setelmx (nth (1- i) *some-primes*) i i diag)))

  (setq tem `(lambda (x)
	       (mfuncall ',representation ',diag x)))
  (setq mat  ($find_matrix_of_operator tem basis))
  (setq cpoly ($charpoly mat '$x))
  (setq answer ($solve cpoly '$x) )
  (setq eigen-values (loop for u in (cdr answer)
			collecting (third u)))
  (loop for u in eigen-values
     collecting ($find_eigenvectors mat u) into part-basis
     collecting (setq weight-vector (exponent-vector u n1)) into weight-vectors
     do (show weight-vector)
     finally (return (list '(mlist) (cons '(mlist) part-basis)
			   (cons '(mlist) weight-vectors)))))
(defun $find_eigenvectors (mat eigenvalue &aux answer eqns)
  (let* ((row-length ($length (second mat)))
	(gen-vector ($firstn row-length $aaaa)))
    (setq eqns ($list_equations
		 (list '(mlist)
		 (list '(mequal) (ncmul* (sub* mat
					       (mul* eigenvalue
						     ($ident row-length)))
					 gen-vector)
		       0))))
    (setq answer ($fast_linsolve eqns gen-vector))
    ($separate_parameters ($sublis answer gen-vector))))

(defun $rank_of_representation (rep n1 list-of-generators &aux mat)
  (setq mat ($general_matrix n1 $cccc))
  ($find_rank_of_list_of_polynomials
    (loop for v in (cdr list-of-generators)
	  appending (cdr (mfuncall rep mat v))
	  into tem
	  finally (return (cons '(mlist) tem)))))

(defun $general_matrix (n a-list)
  (cond ((listp a-list)
  ($unlist_matrix_entries a-list n))
	(t (loop for i from 1 to n
		 appending
		 (loop for j from 1 to n
		       collecting (new-concat a-list i j))
		 into tem
		 finally (return ($unlist_matrix_entries (cons '(mlist ) tem) n ))))))

(defun exponent-vector (n &optional (length-vector 10) &aux wnum denom num wdnom)
  (cond ((numberp n)
  (loop for u in *some-primes*
	for j below length-vector
	collecting
	(loop for i from 1
	      when (not (zerop (mod n (expt  u i))))
	      do (return (1- i)))))
	((equal (caar n) 'rat)(setq num (second n) denom (third n))
	 (setq wnum (exponent-vector num length-vector))
	 (setq wdnom (exponent-vector denom length-vector))
	 (loop for v in wnum
	       for w in wdnom
	       collecting (- v w)))))


(defun replace-parameters-by-aa (expr &aux unknowns  parameters tem1)
	  (setq unknowns ($list_variables expr "aa" "par"))
	  (setq parameters (loop for vv in (cdr unknowns)
				 when (search "par" (string vv) :test #'char-equal)
				 collecting vv))
	  (setq tem1 (cdr $aaaa))

      (loop for vv in parameters
	    appending (loop while tem1
			    when (not (member (car tem1) unknowns :test #'eq))
			    collecting (cons vv (car tem1)) into subs1
			    and do
			    (let ((*print-level* 3))
			      (format t "~%Replacing ~A by ~A in ~A." vv (car tem1) expr))
			    (setq tem1 (cdr tem1)) (return subs1)
			    else
			    do (setq tem1 (cdr tem1)))

	    into subs
	    finally
	    (show subs)
	    (setq expr (sublis subs expr))
	    (setq unknowns (sublis subs unknowns)))
      (values expr unknowns))

(defun function-denominator (pol)
  (cond ((numberp pol) (denominator pol))
	((polynomialp pol) 1)
	((rational-functionp pol) (denom pol))
	(t (denom (new-rat pol)))))

(defun function-numerator (pol)
  (cond ((rationalp pol) (numerator pol))
	((polynomialp pol) pol)
	((rational-functionp pol) (car pol))
	(t (car (new-rat pol)))))

(defun poly-type (x )
  (cond ((numberp x) ':number)
	((polynomialp x) ':polynomial)
	((rational-functionp x) ':rational-function)
	(($ratp x) ':$rat)
	((and (listp x) (eq (caar x) 'rat)) ':rat)
	(t nil)))

(defun check-rat-variables(expr)
  (let ((varlist (varlist expr))
	(genvar (genvar expr)))
    (check-vgp-correspond)))

(defun check-vgp-correspond()
  (loop for v in varlist
	for w in genvar
	when (or (not (equal v (get w 'disrep)))
		 (not (eq w (get-genvar v))))
       do (merror "bad ~A and ~A" v w)))


(defun $slow_gcdlist (&rest ll)
  (cond ((null ll) 1)
	(($listp (car ll)) (apply '$gcdlist (cdar ll)))
	(t
  (case (length ll)
    (1 (car ll))
    (2 ($gcd (car ll) (second ll)))
    (otherwise ($gcd (car ll) (apply '$gcdlist (cdr ll))))))))

(defun $slow_projective (l)
  ($ratsimp (simplifya (list '(mquotient) l ($slow_gcdlist l)) nil)))

;
;(defmacro allow-rest-argument (new-funct binary-op  answer-for-null-argument rest-arg)
; `(cond  ((null ,rest-arg) ,answer-for-null-argument)
;        (t	(case (length ,rest-arg)
;	  (,2 (,binary-op (first ,rest-arg)(second ,rest-arg)))
;	  (,1 (car ,rest-arg))
;	  (otherwise (apply ',new-funct (,binary-op (first ,rest-arg)
;					   (second ,rest-arg)) (cddr ,rest-arg)))))))



;(defmacro polyop (x y identity-x identity-y number-op poly-op rat-op &optional rat-switch )
;  (cond (rat-switch (setq rat-switch '(t))))
;  `(cond
;     ((and (numberp ,x)(numberp ,y))(,number-op ,x ,y))
;     ((eq ,x ,identity-x) ,y)
;     ((eq ,y ,identity-y) ,x)
;     (t
;      (let ((xx (poly-type ,x)) answer
;	    (yy (poly-type ,y)))
;
;;	(cond ((or (eq xx '$rat)(eq yy '$rat))(with-vgp(check-vgp-correspond))));;remove later
;	(cond ((null xx)(setq ,x (cdr ($new_rat ,x)) xx ':rational-function))
;	      ((eq xx ':$rat)
;	       (setq ,x (cdr ,x) xx ':rational-function))
;	      ((eq xx ':rat ) (setq ,x (cons (second ,x) (third ,x))
;				   xx ':rational-function)))
;
;	(cond
;	  ((null yy)(setq ,y (cdr ($new_rat ,y)) yy ':rational-function))
;	  ((eq yy ':$rat)(setq ,y (cdr ,y) yy ':rational-function))
;	  ((eq yy ':rat ) (setq ,y (cons (second ,y) (third ,y))
;			       yy ':rational-function)))
;	(cond ((and (eq xx ':rational-function) (eq (denom ,x) 1))
;	       (setq ,x (car ,x) xx :polynomial)))
;	(cond ((and (eq yy ':rational-function) (eq (denom ,y) 1))
;	       (setq ,y (car ,y) yy :polynomial)))
;	(setq answer
;	      (case xx
;		(:number   (case yy
;			     (:number (,number-op ,x ,y))
;			     (:polynomial (,poly-op ,x ,y))
;			     (:rational-function
;			      (,rat-op (cons ,x 1) ,y ,@ rat-switch))))
;		(:polynomial
;		 (case yy
;		   (:number (,poly-op ,x ,y))
;		   (:polynomial (,poly-op ,x ,y))
;		   (:rational-function (,rat-op (cons ,x 1) ,y,@ rat-switch))
;		   (otherwise (merror "unknown type for polyop "))))
;		(:rational-function
;		 (case yy
;		   (:number (,rat-op ,x  (cons ,y 1) ,@ rat-switch))
;		   (:polynomial (,rat-op ,x (cons ,y 1)  ,@ rat-switch))
;		   (:rational-function (,rat-op ,x ,y ,@ rat-switch))))
;		(otherwise (merror "unknown arg"))))
;	(cond ((polynomialp answer) answer)
;	      ((rational-functionp answer)
;	       (cond ((eq 1 (cdr answer))(car answer))
;		     (t answer)))
;	      (t answer))))))
;
;(defun n* (&rest l)
;  (case (length l)
;    (0 1)
;    (1 (car l))
;    (2 (n1* (car l) (second l)))
;    (t (n1* (car l) (apply 'n* (cdr l))))))
;
;(defun n+ (x y) (polyop x y  0 0 + pplus ratplus ))
;(defun n1* (x y)(polyop x y 1 1 * ptimes rattimes t))
;(defun n- (x y)(polyop x y nil 0 - pdifference ratdifference))
;(defun nred (x y &aux answer)
;  (setq answer (polyop x y nil 1  ratreduce ratreduce ratquotient))
;  (setq answer (rationalize-denom-zeta3 answer))
;  (cond ((numberp answer) answer)
;	((eq (denom answer) 1) (car answer))
;	(t answer)))



;(defun new-disrep (expr)
;  (cond ((atom expr) expr)
;	(t
;	 (let ((type (poly-type expr)))
;	   (case type
;	     (:number expr)
;	     (:polynomial ($ratdisrep (header-poly expr)))
;	     (:rational-function
;	      (cond (($numberp expr)
;		     (cond ((zerop (num expr) 0))
;			   (t
;		     (list '(rat simp) (num expr) (denom expr)))))
;		    (t
;		     ($ratdisrep (header-poly expr)))))
;	     (otherwise  (cond ((mbagp expr)(cons (car expr) (mapcar 'new-disrep expr)))
;			       (($ratp expr)($ratdisrep expr))
;			       (t expr))))))))



(defun sp-add* (&rest llist)
  (let ((varlist))
    (apply 'add*  llist)))

(defun sp-minus* (a)
  (let ((varlist))
    (simplifya (list '(mminus) a) nil)))

(defun sp-sub* (a b )
  (sp-add* a (sp-minus* b)))


(defun sp-mul* (&rest a-list)
  (let ((varlist ))
    (cond ((< (length a-list) 3)(apply 'mul* a-list))
	  (t (sp-mul* (car a-list) (apply 'sp-mul* (cdr a-list)))))))

(defun sp-div* (a b)
  (let ((varlist))
   (simplifya  (list '(mquotient) a b) nil)))


;;the following don't always seem to work
(defun sp-add* (&rest l)(allow-rest-argument sp-add* n+ 0 l))
(defun sp-mul* (&rest l)(allow-rest-argument sp-mul* n* 1  l))
(defun sp-div* (a b) (nred a b))
(defun sp-minus* ( b)(n- 0 b))
(defun sp-sub* (a b)(n- a b))


(defun $numberp (n)
  "we changed this to allow polynomial-type"
  (cond ((numberp n) t)
	((atom  n) nil)
	((and (numberp (car n))(numberp (cdr n))) t)
	((atom (car n)) nil)
	((eq (caar n) 'rat))
	((eq (caar n) 'mrat)(and (numberp (cadr n)) (numberp (cddr n))))
	(($floatnump n))
	(($bfloatp n))))

(defun poly-scalarp (a &aux tem)
  (cond ((numberp a))
	((atom a)($scalarp a))
	((setq tem(polynomialp a)) ($scalarp tem))
	((and (setq tem(polynomialp (car a))) (polynomialp (cdr a)))
	 ($scalarp tem))))


(defun $coefficient_matrix (a-list variables)
  (check-arg a-list  '$listp nil)
  (check-arg variables  '$listp nil)
  (loop for w in (cdr a-list)
	collecting
	(loop for v in (cdr variables)
	      collecting
	      ($nc_coeff w v) into tem
	      finally (return (cons '(mlist) tem)))
	into bil
	finally (return (cons '($matrix) bil))))
;
;
;
;
;
;
;
;
;
;
;(DEFMFUN new-RATREP* (X)
;  (LET ()					; (GENPAIRS)
;;	    (ORDERPOINTER VARLIST)  ;;creates enough new genvar and numbers them done
;    (RATSETUP1 VARLIST GENVAR)
;    ;which is now in new-ratf
;
;    (MAPC #'(LAMBDA (Y Z) (cond ((assolike y genpairs)  nil)
;				(t (PUSH (CONS Y (RGET Z)) GENPAIRS))))
;	  VARLIST GENVAR)
;
;    (RATSETUP2 VARLIST GENVAR)
;    (XCONS (PREP1 X)				; PREP1 changes VARLIST
;	   (LIST* 'MRAT 'SIMP VARLIST GENVAR	;    when $RATFAC is T.
;		  (IF (AND (NOT (ATOM X)) (MEMber 'IRREDUCIBLE (CDAR X) :test #'eq))
;		      '(IRREDUCIBLE))))))
;
;(DEFMFUN new-RATREP* (X)
;  (LET ()					; (GENPAIRS)
;;	    (ORDERPOINTER VARLIST)  ;;creates enough new genvar and numbers them done
;    (RATSETUP1 VARLIST GENVAR)
;    ;which is now in new-ratf
;;
;;    (MAPC #'(LAMBDA (Y Z) (cond ((assolike y genpairs)  nil)           ;;do this in new
;;				(t (PUSH (CONS Y (RGET Z)) GENPAIRS))))
;;	  VARLIST GENVAR)
;
;    (RATSETUP2 VARLIST GENVAR)
;    (XCONS (PREP1 X)				; PREP1 changes VARLIST
;	   (LIST* 'MRAT 'SIMP VARLIST GENVAR	;    when $RATFAC is T.
;		  (IF (AND (NOT (ATOM X)) (MEMber 'IRREDUCIBLE (CDAR X) :test #'eq))
;		      '(IRREDUCIBLE))))))
;
;(Defun New-RATF (L &aux tem)
;  (with-vgp
;    (PROG (U *WITHINRATF*)
;	  (SETQ *WITHINRATF* T)
;	  (WHEN (EQ '%% (CATCH 'RATF (new-NEWVAR L)))
;	    (SETQ *WITHINRATF* NIL) (RETURN (SRF L)))
;	  (loop for v in varlist
;		for i from 1
;		when  (setq tem (get-genvar v))
;		collecting tem into a-list
;		else
;		collecting
;		(prog1 (setq tem (gensym))
;		       (putprop tem v 'disrep)
;		       (show (list tem v 'disrep))
;		       (push (cons v (rget tem)) genpairs )
;		       )
;		into a-list
;		do (setf (symbol-value tem) i)
;		finally (setq genvar a-list))
;	  (SETQ U (CATCH 'RATF (new-RATREP* L)))	; for truncation routines
;	  (RETURN (OR U (PROG2 (SETQ *WITHINRATF* NIL) (SRF L)))))))
;
;(DEFUN new-NEWVAR (L  )
;  (let (( vlist varlist))
;
;  (NEWVAR1 L)
;  (setq varlist (sortgreat vlist))
;  vlist))
 ; (SETQ VARLIST (NCONC (SORTGREAT VLIST) VARLIST)))


(defun $new_rat (expr)
  (cond
    ((and ($ratp expr)(equal *genvar* (genvar expr))(equal *varlist* (varlist expr)))
     expr)
    (($ratp expr)(cond ((loop for v in (varlist expr)   ;;what about minimize varlist here.
			      for w in (genvar expr)	;or in new-ratf
			      when
			      (not (eq w (get-genvar v)))
			      do
			      (return 'bad-order))
			(new-ratf expr))
		       (t expr)))
    ((mbagp expr) (cons (car expr) (mapcar #'$new_rat (cdr expr))) ($new_rat expr))
    ((and (not (atom expr))(not (atom (car expr))))(cond ((equal (caar expr) 'rat) expr)
							 (t (new-ratf expr))))

    (t (new-ratf expr))))
;(if (mbagp exp) (cons (car exp) (mapcar #'rat0 (cdr exp))) (ratf exp)))


(defun $shift (expr &optional (variables $current_variables) &aux tem)
 (sublis (loop for v on (cdr variables)
	when (second v)
	do (setq tem  (second v))
	else do (setq tem (second variables))

	collecting (cons (car v) tem)) expr))
(defun $spur (expr function n &aux (tem expr) (answer 0))
  (loop for i below n
	do
	(setq answer (add* answer (setq tem (funcall function tem)))))
 ($ratsimp answer))


(defun $basis_of_invariant_ring (n &optional (variables $current_variables) &aux
				 monoms trace_monoms f new-f eqns solns tem ar sp pivots)
     (setq monoms ($mono variables n))
	(setq trace_monoms (loop for u in (cdr monoms)
			    collecting ($spur u '$shift (1- (length variables)) ) into tem
			    finally (return (cons '(mlist ) tem))))
	(break t)
	(setq f ($general_sum trace_monoms $aaaa))
	(setq new-f ($dotsimp f))
	(setq eqns ($extract_linear_equations (list '(mlist) new-f) monoms))
	(setq solns ($fast_linsolve eqns (subseq $aaaa 0 (length monoms))))
	(setq sp(send $poly_vector :the-sparse-matrix))
    (setq ar (send sp ':column-used-in-row))
   (setq pivots (loop for i below (length (the cl:array ar))
	  when (setq tem (aref ar i))
	  collecting tem into tem1
	  finally (return  tem1)))
   (loop for i in pivots
		 collecting (nth (1+ i) trace_monoms)
		 into tem1
		 finally (return (cons '(mlist) tem1))))

(defvar $mmm)
(defvar $conditions_on_q nil)

(defun $case_rep (mat vect triple  &aux  answer  subs ($expop 100)
		 ind-used vmmm prod mat.rtx relat-mat)
  (setq triple (cdr triple))
  (setq subs (list (cons '$%alpha (first triple))
		   (cons '$%beta (second triple))
		   (cons '$%gamma (third triple))))

  (let ((mmm (sublis subs $mmm)))
    (setq mmm ($ratsimp mmm))
    (displa mmm)
    (loop for  v in (cdr $conditions_on_q)
	  for i from 1
	  when ($zerop ($ratsimp (sublis subs v)))
	  collecting (nth i mmm) into tem
	  and
	  collecting i into ind
	  finally (setq ind-used ind)(setq relat-mat tem))
    (format t "~%Dimension of representation is ~A." (length ind-used))
    (show ind-used)
    (setq vmmm
	  (loop for i from 1 to (length ind-used)
		collecting (mul* (nth i vect)(nth (1- i) relat-mat)) into tem
		finally (return (meval* (cons '(mplus) tem)))))
    (setq mat.rtx (ncmul* mat '(($matrix simp)
				((mlist simp) $x) ((mlist simp) $y) ((mlist simp) $z))))

    (setq prod
	  (ncmul*  ($transpose mat)
		   ($sub_list `((mlist simp) ((mequal simp) (($matrix simp)
							     ((mlist simp) $x)
							     ((mlist simp) $y)
							     ((mlist simp) $z))
					      ,mat.rtx)) vmmm) mat))
    (setq answer (mfuncall '$cof prod))
    (loop for i in ind-used
	  collecting (nth i answer) into tem
	  finally (return (cons '(mlist) tem)))))

(defun $make_commutative (expr)
  (resimplify (subst 'mtimes 'mnctimes expr)))
