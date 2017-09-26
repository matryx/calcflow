;;; -*- Mode:Lisp; Package:CL-MAXIMA; Syntax:COMMON-LISP; Base:10 -*-;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;  (:init-keywords :the-sparse-matrix :table :array-of-tables
;			       :array-of-polynomials :type-of-entries :constants-column-number
;			       :solution-in-macsyma-format :relations :variables :rows)
;
;           (:settable-instance-variables verify-conversion)
;	   :gettable-instance-variables)

; rows
; length-of-array-of-tables
; array-of-tables
; type-of-polynomials
; table
; array-of-tables
; last-column-number
; number-of-independent-terms   ;;never occurs
; array-of-polynomials
; type-of-entries
; verify-conversion
; constants-column-number
; the-sparse-matrix
; solution-in-macsyma-format
; relations
; variables

;;The idea will be to take a list of vectors containing polynomials so
;;why not an array such that the  rows will represent the rows of the
;;eventual sparse matrix to be created.  The columns (there may be only
;;one) will correspond to possibly different slots of some matrix.  Thus
;;if we have 20 2 by 2 matrices with polynomial entries, we shall
;;consider an array 20 by 4.  Or better yet why not 20 by 2 by 2.  We
;;wish do go through and create a table for each slot of the 2 by 2
;;which lists the terms occurring somewhere in that slot. Then we will
;;go through the polynomials and associate 20 sparse-matrix type rows
;;whose entries will be the coefficients obtained from looking in the
;;hash table.

;;Alternateley it is probably better to store the polynomial vectors as
;;elements of a 1 dimensional art-q array. Thus each slot would contain
;;a 2 by 2 art q array its entries being the appropriate polynomial.

(defvar $current_variables  nil)
(defvar $current_monomials nil)
(defvar $fast_dotsimp t "will use rat-ncmul")
(defvar $order_function '$monomial_alphalessp
  "Order used for dot monomials. Tail-alphalessp is another reasonable value.")
(defvar $aaaa nil  "Should contain a list of scalar coefficients $aa1 etc.")
(defvar $dot_simplifications nil "Macsyma list of pattern ,replacements")

(setq $dotdistrib t)

(defmacro get-rid-of-array (ar)
  `(return-array (prog1 ,ar (setq ,ar nil))))

;(defmethod (polynomial-vectors :fasd-form) ()
;  (let ((options-present
;	  (loop for u in (list :the-sparse-matrix :table :array-of-tables
;			       :array-of-polynomials :type-of-entries :constants-column-number
;			       :solution-in-macsyma-format :relations :variables :rows)
;		when (catch-error (send self u) nil)
;		appending (list u (send self u)))))
;    (setq options-present (cons nil options-present))
;  `(instantiate-flavor 'polynomial-vectors ',options-present t)))
;(defmethod (polynomial-vectors :init) (plist &aux value )
;  (loop for value-name in (list :the-sparse-matrix :table :array-of-tables
;			       :array-of-polynomials :type-of-entries :constants-column-number
;			       :solution-in-macsyma-format :relations :variables :rows)
;	when (setq value (get plist value-name))
;	do (send self :eval-inside-yourself
;		 `(setq  ,
;		    (INTERN (string-trim ":"
;					 (string value-name)) 'MACSYMA ) VALUE)) ))

(defun macsyma-display (form)
  "Takes a macsyma form and displays it"
  (displa `((mlabel) 1 ,form)))

(defun gt (form)
  (grind-top-level form))

(defun list-terms (poly &aux terms)
  (cond  ((eq poly 0) nil)
	 ((atom poly) (setq terms (list poly)))
	 (t
	  (setq terms
		(case (caar poly)
		  (mplus (cdr poly))
		  (mtimes (list poly))
		  (mexpt (list poly))
		  (mnctimes (list poly))
		  (mncexpt (list poly))
		  (otherwise (error "~A is not a polynomial" poly))))))
  terms)
;  (cond ((user:appears-in terms 'mplus)
;	 (format t "~%~A was not fully expanded, so using its expansion" poly)
;	 (list-terms ($expand poly)))
;	(t terms)))
(defmacro remove-second (a-list)
  `(rplaca (cdr ,a-list) (car ,a-list)))

(defun numerical-coefficient-and-monomial (term &aux (answer  1) (monomial 1))
  "returns two values the numerical coefficient and either 1 or
   an atomic variable or a power eg ((mexpt simp) $x 2) or a list l
   which whose elements would be multiplied ( by doing (cons '(mtimes simp) l) )
   to obtain the appropriate monomial.  This does work."

  (cond ((numberp term) (setq answer term))
	((atom term) (setq monomial term))
	(t
	 (case (caar term)
	   (mtimes (cond (($numberp  (second term)) (setq answer (second term))
			  (setq monomial (cddr term)))
			 (t (setq monomial (cdr term))))
		   (cond ((atom (car monomial))nil)
			 ((appears-in  (caar monomial) 'mplus)
			  (error "Some monomial with a sum in it"))))
	   (mexpt  (setq monomial term))
	   (mnctimes (setq monomial term))
	   (mncexpt (setq monomial term))
	   (otherwise (error "~A is not a term." term)))
	 (cond ((eq  1 (length monomial)) (setq monomial (car monomial))))))
  (values answer monomial)) ;;Note this a monomial less the '(mtimes simp)!!!

;;definition:  A monomial is 1, or a symbol, or ((mexpt simp) var
;;number) or ((mncexpt simp) var number) or ?((mnctimes simp) var number)?
;;or ((mtimes simp) a1 a2 ... an) where a1, a2, ... an are in the above
;;list( but not 1).  Here var stands for a symbol and number stands for
;;an integer bigger than 1.

;;definition:  A term is a number, or a monomial, or ((mtimes simp)
;;number monomial)

;;definition:  A polynomial is either a term or ((mplus simp) t1 t2 ...
;;tn) where  the t1,.. tn are terms.

;;definition:  A premonomial is a monomial with the (mtimes simp)
;;deleted.

;;The keys in our hash table will be premonomials.  The reason is it is
;;simpler to just remove the ((mtimes simp) number at beginning of a
;;list rather than keeping it.  It will also make the hashing slightly
;;better.

(defun make-monomial ( premonomial)
  "Returns a monomial given a premonomial"
  (cond ((atom premonomial) premonomial)
	((atom (car premonomial)) (cons '(mtimes simp) premonomial))
;	((eq (caar premonomial) 'mexpt) premonomial)
;	(t (cons '(mtimes simp) premonomial))))
	(t (case (caar premonomial)
	     (mexpt premonomial)
	     (mnctimes premonomial)
	     (mncexpt premonomial)
	     (otherwise (cons '(mtimes simp) premonomial))))))

(defun make-premonomial (monomial)
  (cond ((atom monomial) monomial)
	(t
	 (delete '(mtimes simp) monomial :test #'equal))))
(defun macsyma-list (&rest l)
  (cons '(mlist simp) (copy-list l)))

(defun show-coeff (poly )
  (loop for u in (list-terms poly)
	do (multiple-value-bind (coefficient guts-monomial)
	       (numerical-coefficient-and-monomial u)
	     (format t "~%~A has coefficient ~D and monomial ~A" u coefficient
		     (make-monomial guts-monomial))
	     (displa (macsyma-list u coefficient (make-monomial guts-monomial))))))

;(setf (function $tee) (function convert-polynomial-to-vector))

(defvar *one-dimensional-array* (make-array 100 :fill-pointer 0 :adjustable t) "A temporary one dimensional array")

(defun convert-polynomial-to-vector (polynomial hash-table &optional last-column-number parts-of-monomial)
  "Returns the row corresponding to the polynomial and the new last-column-number.
   The first argument is a regualar macsyma polynomial in expanded form as returned
   by Expand(polynomial);. The second argument is the hash-table, and the last is the
   number of entries in the hash table."
  (let* ((terms (list-terms polynomial))
	answer
;	(last-column-number (send hash-table :filled-elements))
	(a-table hash-table)
	 col)
    (setq answer (make-array (* 2 (length terms)) :fill-pointer 0 :adjustable t))
    (cond ((null last-column-number)
	   (setq last-column-number
		 (hash-table-count hash-table))))
    (cond ((null parts-of-monomial) (setq parts-of-monomial
					  'numerical-coefficient-and-monomial)))
    (loop for u in terms
	  do (multiple-value-bind (coeff mon)
		 (funcall parts-of-monomial u)
	       (cond ((setq col (gethash  mon a-table )))
		     (t (setq col (1+ last-column-number))
			(setq last-column-number col)
			(setf (gethash  mon a-table)  col)))
	       (vector-push-extend  col answer)
	       (vector-push-extend  coeff answer)))

    (collect-coefficients answer)
    (maybe-move-back-fill-pointer answer)
    (values answer last-column-number)))

(defmacro reverse-two-values (form)
  `(multiple-value-bind (.first. .second.),form
     (values .second. .first.)))

(defun mac-add* (&rest l &aux tem)
  (setq tem (apply #'add* l))
  (cond (($zerop tem) 0)
	(t tem)))

(defun collect-coefficients ( a-row  &aux col sum)
  (loop for i  below (length (the cl:array a-row)) by 2
     when (setq col (aref a-row i))
     do

       (setq sum (aref a-row (1+ i)))
       (loop for ii from (+ i 2) below (length (the cl:array a-row)) by 2
	      when (equal col (aref a-row ii))
	      do

	      (setq sum (mac-add* sum (aref a-row (1+ ii))))
	      (setf (aref a-row ii)  nil)
	      (setf (aref a-row (1+ i))  sum))))

(defun sc_and_nc_parts (monomial)
  (reverse-two-values (extract_nc_and_sc_parts monomial)))

(defun $list_terms (f)
  (macsyma-list (list-terms f)))


(defun get-key (table value)
  (loop for ke being the hash-keys in table using (hash-value val)
	 when (equal val value)
	 do (return ke)))

;; (defun convert-polynomial-row (row hash-table &aux terms cof col mon)
;;   "This should actually return something equal to the original polynomial
;;   which was fed into convert-polynomial-to-vector"
;;   (setq terms
;; 	(loop for ii below (length (the cl:array row)) by 2
;; 	      when (setq col (aref row ii ))
;; 	      do
;; 	      (setq mon (get-key hash-table  col))
;; 	      (setq cof (aref row (1+ ii )))
;; 	      collecting
;; 	      (mul* cof (make-monomial mon))))
;; ;	      (cond ((atom mon)(list  '(mtimes simp) cof mon))
;; ;			       ((eq (car mon) 'mexpt) (list '(mtimes simp) cof mon))
;; ;			       (t (append (list '(mtimes simp) cof) mon)))))
;;    (append '((mplus simp)) terms))

(defun convert-polynomial-row (row hash-table &aux terms cof col mon)
  "This should actually return something equal to the original polynomial
  which was fed into convert-polynomial-to-vector"
  (setq terms
	(loop for ii below (length (the cl:array row)) by 2
	      when (setq col (aref row ii ))
	      do
	      (setq mon (get-key hash-table col))
	      (setq cof (aref row (1+ ii )))
	      and
	      collecting
	       (mul* cof (make-monomial mon))))
   (apply #'add* terms))

(defun make-one-dimensional (an-array)
  (if (= (array-rank an-array) 1)
      an-array
      (make-array (apply #'* (array-dimensions an-array)) :adjustable t :displaced-to an-array)))

(defun total-dimension (an-array)
  (apply #'* (array-dimensions an-array)))

(defmacro equation-length (eqn)
  `(cond ((atom ,eqn) 1)
	(($plusp ,eqn) (1- (length ,eqn)))
	(t 1)))

(defun  pv-get-rows-from-macsyma-equations
	   (self equations				; &optional variables
	     &aux tem temp-array )		;  corresponding-variables-column)
  (check-arg equations $listp "Macsyma list")
  (setq temp-array (make-array (length equations) :fill-pointer 0 :adjustable t))
  (loop for u in (cdr equations)
	do
	(setq tem (bring-to-left-side u))
						;(print tem)
	(cond ((numberp tem)
	       (cond ((zerop tem)(format t "~%Eliminate Trivial equation"))
		     (t (format t "~%Warning: Inconsistent equation ~A = 0" tem))))
	      (t	(vector-push-extend  tem temp-array))))


  (  pv-get-rows-from-array-of-polynomials self   temp-array)
  (setf (pv-constants-column-number self) (gethash  1 (pv-table self) ))
  ( pv-set-up-sparse-matrix-from-rows self ))
(defun  pv-set-up-sparse-matrix-from-rows (self)
  (cond ( (pv-the-sparse-matrix self) nil)
	(t (setf (pv-the-sparse-matrix self) (make-sparse-matrix))))
  ( sp-set-rows  (pv-the-sparse-matrix self) (pv-rows self))
  ( sp-set-type-of-entries  (pv-the-sparse-matrix self)
   (pv-type-of-entries self))
  (cond ((pv-constants-column-number self)
	 ( sp-set-constants-column
	  (pv-the-sparse-matrix self)
	  (pv-constants-column-number self)))
	(t (setf (sp-constants-column
		   (pv-the-sparse-matrix self)) nil))))

(defun  pv-reduce (self)
  ( pv-set-up-sparse-matrix-from-rows self )
  ( sp-reduce  (pv-the-sparse-matrix self))
  (format t "~%The rank is ~D"
	  ( sp-number-of-pivots  (pv-the-sparse-matrix self))))

(defmacro solution-corresponds-to-column (a-solution)
;  `(array-leader ,a-solution 1))
   `(getf (pv-solution-plist self) ,a-solution))

(defvar *show-solve* t)
(defvar $solution_parameter '$par "The prefix for the parameters in solve-suitable-to-sublis,
 a value of nil uses the values in table instead.")

(defun poly-identity (x)
  (cond ((null x) 0)
	(t x)))

(defun  pv-solve-suitable-to-sublis
	   (self &aux the-solutions the-special-solution dim tem col-list no-pivot
	    answer final-answer this-row (val 0) parameter
	    special-contribution  corresponding-variables-column
	    columns-used-to-pivot )
   (cond (*show-solve*
	  (format t "~%Starting to solve.  There are ~D equations with ~D unknowns occurring."
	  ( sp-number-of-rows  (pv-the-sparse-matrix self))
	  (length ( sp-list-of-all-columns-occurring  (pv-the-sparse-matrix self))))))

 ( sp-solve  (pv-the-sparse-matrix self) :reduce t)
  (setf col-list ( sp-list-of-all-columns-occurring  (pv-the-sparse-matrix self)))
  (setq the-solutions  ( sp-rows  ( sp-solutions  (pv-the-sparse-matrix self))))
  (setq the-special-solution
	( sp-special-solution
			      ( sp-solutions  (pv-the-sparse-matrix self))))
  (setq dim (array-total-size the-solutions))
  (setq no-pivot (sp-columns-with-no-pivot(pv-the-sparse-matrix self)))
  (cond (the-special-solution
	 (setq columns-used-to-pivot
	       (sp-columns-used-to-pivot(pv-the-sparse-matrix self) ))))
 (cond (*show-solve* (format t "~%The dimension of the solution space is ~D" dim)))
  (setq
    final-answer
    (loop for i in col-list
	  do
	  (setq answer 0)
	  (setq special-contribution 0)
	  (loop for ii below (length (the cl:array the-solutions))
		do
		(setq this-row (aref the-solutions ii))
		(setq corresponding-variables-column
		      (solution-corresponds-to-column this-row))
		(setq tem  (row-entry this-row i))
		;;;corresponding-variables-column
		when tem
		do
		(cond ($solution_parameter (setq parameter (intern (format nil "~a~d" $solution_parameter ii))))
		      (t
		       (error "unexpected case")
		       (setq parameter
			       (get-key (pv-table self)
				      corresponding-variables-column))))
		(setq val (sp-mul*  tem
				    parameter))
		(setq answer (sp-add* answer val)))
	  (cond (the-special-solution
		 (setq special-contribution
		       (poly-identity (row-entry the-special-solution i)))))
	  (setq answer (sp-add* answer special-contribution))
	  collecting
	  (list '(mequal simp)
		(get-key (pv-table self)  i)
		(new-disrep answer))))
  (setf (pv-solution-in-macsyma-format self) (cons '(mlist) final-answer)))


(defun  pv-get-rows-from-list-of-polynomials
	   (self mac-list &optional parts-of-monomial &aux ar)
  (setq ar (make-array (length (cdr mac-list)) :adjustable t))
  (fillarray ar (cdr mac-list))
  ( pv-get-rows-from-array-of-polynomials self  ar parts-of-monomial))

(defun $find_rank_of_list_of_polynomials (a-list &aux cols)
  (declare (special $poly_vector))
  ( pv-get-rows-from-list-of-polynomials $poly_vector  a-list)
  ( pv-set-up-sparse-matrix-from-rows $poly_vector )
  (let ((sp ( pv-the-sparse-matrix $poly_vector )))
	( sp-reduce sp)
	(setq cols ( sp-column-used-in-row sp))
	(loop for i below (length (the cl:array cols))
	      when  (aref cols i)
	      count i into rank
	      finally (return rank))))

(defun  pv-get-rows-from-array-of-polynomials
	   (self an-array &optional parts-of-monomial last-column-number)
    "Takes AN-ARRAY whose entries are polynomials or Art-q-n arrays of polynomials.
     It uses the parts-of-monomial if supplied to get the coefficients"
    (let* ((number-of-rows (length (the cl:array an-array)))
	   first-element a-row poly partial-row)
      (cond ((> 0 number-of-rows )(setq first-element (aref an-array 0))))
      (cond ((ml-typep first-element 'array)
	     (setf (pv-length-of-array-of-tables self) (total-dimension first-element))
	     (setf (pv-type-of-polynomials self) :polynomial-vectors)
	     (cond ( (pv-array-of-tables self) nil)
		   (t ( pv-set-up-hash-tables self ))))
	    (t (setf (pv-type-of-polynomials self) :polynomial)
	       (cond (( pv-table self) (clrhash (pv-table self) ))
		     (t  ( pv-set-up-hash-tables self )))))
      (setf (pv-rows self) (make-array number-of-rows :adjustable t))

      (setf (pv-array-of-polynomials self) an-array)
      (case
	(pv-type-of-polynomials self)
	(:polynomial
	 (loop for i below number-of-rows
	       do
	       (setq poly (aref an-array i))
	       (setq a-row (convert-polynomial-to-vector
			     poly (pv-table self) nil parts-of-monomial))
	      (setf (aref (pv-rows self) i)  a-row)

	       (cond ((pv-verify-conversion self)
		      (check-conversion poly a-row (pv-table self))))))
	(:polynomial-vectors
	 (let (poly-vector)
	   (loop for i below number-of-rows
		 do
		 (setq poly-vector (make-one-dimensional
				     (aref (pv-array-of-polynomials self) i)))
		(setf (aref (pv-array-of-polynomials self) i)   poly-vector)
		 (setq a-row (make-array (length poly-vector) :fill-pointer 0 :adjustable t))
		(setf (aref  (pv-rows self) i)  a-row)
		 (loop for ii below (array-total-size poly-vector)
		       do
		       (setq poly (aref poly-vector ii))
		       (multiple-value (partial-row last-column-number )
			 (convert-polynomial-to-vector
			   poly
			   (aref (pv-array-of-tables self) i)
			   (pv-last-column-number self) parts-of-monomial))
		       (setf (pv-last-column-number self) last-column-number)
		       (cond ((pv-verify-conversion self)
			      (check-conversion poly partial-row
						(aref (pv-array-of-tables self) i))))
		       (loop for k below (length (the cl:array partial-row))
			     do (vector-push-extend  (aref partial-row k) a-row))
		       ;;;		       (get-rid-of-array partial-row)
		       ))))))
    ( pv-check-type-of-entries self ))

(defun check-conversion (polynomial row table &aux poly)
  (cond ((equal (setq poly (convert-polynomial-row  row table)) polynomial)
	 (format t "~%Checking polynomial ~A and table ~A. Conversion OK." row table))
	(t (format t
		   "~%The converted row ~A and the polynomial ~A with translation table
     ~A do not agree.  They differ by " row polynomial table)
     (displa (mul* poly (add* -1 polynomial))))))

(defun  pv-check-type-of-entries (self &aux a-row)
   (setf (pv-type-of-entries self) (or modulus :rational))
  (catch 'done
    (loop for ii below (length (the cl:array (pv-rows self)))
	  do
	  (setq a-row (aref (pv-rows self) ii))
	  (loop for i below (length (the cl:array a-row)) by 2
		when (and (aref a-row i) (not (numberp (aref a-row (1+ i)))))
		do (setf (pv-type-of-entries self) ':any-macsyma)
		(throw 'done 'done))))
  (format t "~%Using entry type ~A." (pv-type-of-entries self)))

(defun  pv-set-up-hash-tables  (self)
  (case (pv-type-of-polynomials self)
    (:polynomial (setf (pv-table self) (make-hash-table :test 'equal)))
    (:polynomial-vectors
     (setf (pv-last-column-number self) 0)
     (setf (pv-array-of-tables self)
	   (make-array (pv-length-of-array-of-tables self) :fill-pointer 0 :adjustable t))
     (loop for i below (pv-length-of-array-of-tables self)
	   do
	   (vector-push  (make-hash-table :test 'equal) (pv-array-of-tables self))))))


(defun $my_sum (quote-form quote-index  start  &optional end &aux answer)
  (setq quote-form  (subst 'ind quote-index quote-form))
  (setq start (meval start))
  (setq end (meval end))
  (setq answer
	(cond (end
		 (loop for ind from start to end
				       collecting
		       (meval quote-form)))
	      (t
	       (loop for ind  in (cdr start)
		     collecting
		     (meval quote-form)))))
  (setq answer (meval* (cons '(mplus) answer)))
   answer)

(defun $list_dot (l ll)
 (apply #'add*  (loop for u in (cdr l) for v in (cdr ll)
	collecting (mul* u v))))

(defun permutations (a-list &optional (size (length a-list)) &aux answer )
  (setq answer  (loop for u in a-list collecting (list u)))
  (loop for i below (1- size)
	do
	(setq answer
	      (loop for v in a-list
		    appending
		    (loop for perm in answer
			  when (not (member v perm :test #'equal))
			  collecting (cons v perm)))))

  answer)

(defun sign-of-permutation (perm-of-first-n-non-negative-integers)
  "Will return the proper sign of a permutation of the first n non-negative integers"
  (nsign-of-permutation (copy-list perm-of-first-n-non-negative-integers)))

(defun nsign-of-permutation (perm &aux tem)
  (let ((sub1-leng (1- (length perm))))
  (cond ((zerop sub1-leng) 1)
	((eq (setq tem (car (last perm))) sub1-leng)
	 (nsign-of-permutation (nbutlast perm)))
	(t (nsubst tem sub1-leng perm)
	   (- (nsign-of-permutation (nbutlast perm)))))))

(defun $make_art_q (&rest indices)
  (make-array indices :adjustable t))

(defun $first_variable (monomial )
  (cond ((numberp monomial) nil)
	((atom monomial) monomial)
	(t (loop for u in monomial do (print u)
		 (cond ((numberp u) nil)
		       ((atom u) (return u))
		       ((member u '((mtimes simp) (mtimes)
				    (mexpt simp) (mncexpt simp) (mncexpt)
				    (mnctimes simp)(mnctimes)) :test #'equal) nil)
		       ((member (car u)'((mtimes simp) (mtimes)
				    (mnctimes simp)(mnctimes)) :test #'equal)
			(return ($first_variable (cdr u))))
		       (t (return (second u))))))))


(defun m. (&rest l)
  (simplifya (cons '(mnctimes) (copy-list l)) nil))


(defun $convert_right_to_left (a b)
  (let* ((f ($expand (add* (m. a '$x) (m. b '$y))))
	(terms (list-terms f)) x-terms y-terms)
    (loop for u in terms do
	  (format t "~%For term ~A" u)
	  (cond ((equal ($first_variable u) '$x) (setq x-terms (cons u x-terms)))
		((equal ($first_variable u) '$y) (setq y-terms (cons u y-terms)))
		(t (error "~A has first variable not x or y" u))))

    (macsyma-list (apply 'add* x-terms) (apply 'add* y-terms))))

(defun $split_into_x_and_y (f)
  (let	((terms (list-terms f)) x-terms y-terms)
    (loop for u in terms do
	  (format t "~%For term ~A" u)
	  (cond ((equal ($first_variable u) '$x) (setq x-terms (cons u x-terms)))
		((equal ($first_variable u) '$y) (setq y-terms (cons u y-terms)))
		(t (error "~A has first variable not x or y" u))))

    (macsyma-list (apply 'add* x-terms) (apply 'add* y-terms))))

(defun $matrix_entry(x i j) (nth i (nth j x)))

(defun initial-equal (list-a list-b)
 (not (loop for u in list-a for v in list-b
	when (not (eql u v))
	do (return t))))

(defun initial-sublist (lista biglist)
  (cond ((< (length biglist) (length lista)) nil)
	(t (initial-equal lista biglist))))


;;faster version
(defun $replace_the_pattern (adp pattern replacement &aux tem)
  "replaces the pattern in ADP (a dot product) by replacement. When
 inserted in the simplifier simpmnct it does the replacement whenever
 called."
  (cond ((atom adp) adp)
	((equal (caar adp) 'mnctimes)
	 (cond ((setq tem (ordered-sublist (cdr pattern ) (cdr adp)))
		 (ncmul* (ncmuln (car tem) t ) replacement (ncmuln (cadr tem)t)))
	       (t adp)))
	(t adp)))
(defvar $free_dot t

  "If nil the simplifier will use $dot_simplifications to reduce
dot_products, much the same as can be obtained by doing $dotsimp")

(defun dotsimp-one-term (monom coeff ignor) ignor
  (mul* coeff ($expand ($dot_simp_monomial monom))))

(defun dotsimp-atom (expr)
  (loop for u in (cdr $dot_simplifications)
		      for i from 1
		      when (and (oddp i)(eq u expr))
		      do (return (nth (1+ i) $dot_simplifications))
		      finally (return expr)))
(defvar *troublesome-variables* nil)
(defun check-rat-order (expr &optional (variables *troublesome-variables*)
			&aux maybe-out-of-order nc-product-appeared)
  (cond ((and (consp expr)($ratp expr))
	 (loop for v in (third (car expr))
	       when (not (fast-scalarp v))
	       do (setq nc-product-appeared t)
	       when (and (member v variables :test #'eq) nc-product-appeared)
	       do (setq maybe-out-of-order t))))
  (cond (maybe-out-of-order
	(setq nc-product-appeared nil)
	(setq maybe-out-of-order nil)
	 (loop for v in (third (car expr))
	       for w in (fourth (car expr))
	       when (and (not (atom v))(eq (caar v) 'mnctimes))
	       do (setq nc-product-appeared t)
	       when (and (member v variables :test #'eq) nc-product-appeared
			 (appears-in (cdr expr) w))
	       do
	       (setq maybe-out-of-order t)))
	 (t nil))
  (cond (maybe-out-of-order
	 (setq nc-product-appeared nil)
	 (cond ((loop for v in (third (car expr))
		      for w in (fourth (car expr))
		      when (and (not (atom v)) (eq (caar v) 'mnctimes)
				(appears-in (cdr expr) w))
		      do (setq nc-product-appeared t)
		      when (and (member v variables :test #'eq) nc-product-appeared
				(appears-in (cdr expr) w))
		      do (return t))
		(format t "~%~%~%*************Changing rat order")
		(beep)(break 'change)
		(setq expr ($nc_rat expr))))))
  expr)
(defun get-varlist (expr)
  (cond (($ratp expr)(third (car expr)))
	(t nil)))
;(defun minimize-varlist-and-genvar (expr)
;  (cond (($ratp expr)
;	 (let ((var-list (third (car expr)))
;	       (gen-var (fourth (car expr))))
;
;	 (loop for v in var-list
;	       for w in gen-var
;	       when (user:appears-in (cdr expr) w)
;	       collecting v into new-varlist
;	       and
;	       collecting w into new-genvar
;	       finally (setf (third (car expr)) new-varlist)
;	       (setf (fourth (car expr)) new-genvar)))))
;  expr)
(defmacro check-nc-rat-order (expr)
 ` (progn  (format t "~%checking order for ~A" ',expr)
  (setq ,expr (check-rat-order ,expr))))
(defun varlist (expr)
  (third (car expr)))
(defun genvar (expr)
  (fourth (car expr)))
(defmacro show-varlist (expr)
 `(progn (format t "~%The varlist for ~A is"',expr)
	 (gt (varlist ,expr))))


(defvar $radical nil)
(defvar $radical_nilpotent_of_order 0)
(defun nil-radicalp (monom )
  "gives nil if monom not in radical otherwise gives the power it is in"
  (cond ((atom monom) (setq monom (list nil monom))))
  (cond ($radical
	 (let ((answer 0))
	 (loop for v in (cdr $radical)
	       do
	       (loop for w in (cdr monom)
		     when (eq w v) do (setq answer (1+ answer))))
	 (cond ((zerop answer) nil)
	       (t answer))))))
(defun in-nth-power-radical  (monom n &aux tem)
  (cond ((setq tem (nil-radicalp monom))(> tem (1- n)))))




;;;newest  version with saving possible reset inside
(defun dot-show (nc-list)
  (cond ((atom nc-list) (princ nc-list))
	(t(format t "~A" (string-trim "$" (string (second nc-list))))
	  (loop for v in (cddr nc-list) do
		 (format t ".~A" (string-trim "$" (string v)))))))

(defun new-rat-dotsimp (expr &aux  (answer 0) repl the-num the-denom the-rest tem)
  (format t "~%Beginnning to new-rat-dotsimp ")
  (cond (($must_replacep expr)
	 (loop while(not ($zerop expr))
	    do
	      (show answer)
	      (setq tem nil)
	      (cond (($must_replacep expr))
		    (t (return (header-poly (n+ answer expr)))))
	      (setq-num-den the-num the-denom expr)
	      (show the-num)
					;	     (cond ((polynomialp  expr) (setq the-num expr the-denom 1))
					;		   ((rational-functionp expr) (setq the-num (num expr) the-denom (denom expr)))
					;		   (t (fsignal "expr is supposed to be poly or rational-functionp")))
					;	     (cond ((numberp the-num)(setq answer (n+ answer expr))
	      (cond ((poly-scalarp the-num)(setq answer (n+ answer expr))
		     (return (header-poly answer))))
	      (setq tem  (get (car the-num) 'disrep))
	    when
	      (contains-a-zero-replacement tem)
	    do
	      (format t "~%Simplifying the worst monomial: ")(dot-show tem)
	      (cond ((setq expr (fifth the-num)))
		    (t (setq expr 0)))
	    else
	    when ($must_replacep tem) ;; (setq tem  (get (car the-num) 'disrep)))
	    do
	      (format t "~%Simplifying the worst monomial: ")(dot-show tem)
	      (setq repl (n* (third the-num)  ($dot_simp_monomial tem)))
	      (cond ((setq the-rest (fifth the-num))
		     (show the-rest repl)
		     (setq expr (n+ the-rest repl))
		     (show expr)
		     (setq expr (nred expr the-denom)))
		    (t (setq expr (nred repl the-denom))))
	    else
	    do
	      (format t "~%Simplifying the worst monomial: ")(dot-show tem)
	      (format t "adding it to answer")
	      (cond ((fifth the-num)(setq expr (nred  (fifth the-num) the-denom))) ;cons?
		    (t (setq expr 0)))
	      (setq answer (n+ answer (nred (subseq the-num 0 3) the-denom)))
	    do
	      (setq repl nil)
	      (setq tem nil the-num nil the-denom nil )
	    finally (return (header-poly answer))))
	(t (header-poly expr))))

(defun rat-dotsimp (expr &aux (answer 0) repl monom term cof tem simped-monom )
  (format t "~%Beginning to rat-dotsimp..")
  (setq expr (minimize-varlist expr))
; (setq upper-bound-for-gen-var (expt  (length (cdr $current_variables))
;				   ($nc_degree expr)))
;  (cond (*genvar* nil)
;	(t (setq *genvar* (copy-list genvar))))
;  (setq the-dif (max 0 (- (length *genvar*) upper-bound-for-gen-var (length (varlist expr))))
;  (setq gen-vars (nthcdr the-dif *genvar*))
;  (let ((genvar gen-vars))
;  (displa expr)
  (loop while (and (not ($zerop expr))($must_replacep expr))
	do
	(multiple-value (monom cof) (find-worst-nc-monomial expr))
						; (check-nc-rat-order expr)))
	(cond (($must_replacep monom)
	       (setq expr (vsub* expr (vmul* cof monom)))
;	       (check-nc-rat-order expr)
	       (setq simped-monom ($dot_simp_monomial monom))
	       (setq repl (vmul* cof simped-monom))
						;       (check-nc-rat-order expr)
						;      (check-nc-rat-order repl)
	       (cond (($must_replacep repl)
		      (setq tem  (vadd* repl expr))
		      (setq expr tem))
		     (t  (setq answer (vadd* repl answer)))))
	      (t (setq expr (vsub* expr (setq term (vmul* cof monom))))
		 (setq answer (vadd* term answer))))
	finally
;	(setq *genvar* (copy-list  genvar))
	(return (minimize-varlist  (vadd* expr answer)))))
   ;   )


(defun $dotsimp (expr &aux (answer 0) term cof varlist monom repl (prev-monom 'zzzzz))
  (cond ((and $fast_dotsimp (not(or (mbagp expr) ($ratp expr))))
	 (cond ($new_fast_dotsimp (setq expr ($new_rat expr)))
	       (t (setq expr ($vrat expr))))))
  (cond ((not (or (atom expr)$fast_dotsimp  ($ratp expr)))
	 (setq expr ($multthru ($ratsimp ($expand expr))))))
  (cond
    ((atom expr)(setq answer (dotsimp-atom expr)))
    (($ratp expr)
     (cond ($new_fast_dotsimp      (setq answer (new-rat-dotsimp (cdr expr))))
	   (t  (setq answer (rat-dotsimp  expr)))))
    ((mbagp expr)(setq answer (cons (car expr) (mapcar #'$dotsimp (cdr expr)))))
    ((member (caar expr) '(mtimes mnctimes) :test #'equal)
     (setq answer(apply 'dotsimp-one-term
				(multiple-value-list (find-worst-nc-monomial expr))))
     (cond (($must_replacep answer)
	    (setq answer ($dotsimp answer)))
	   (t answer)))
    ((equal (caar expr) 'mplus)
       (loop while  (not (and (numberp expr) (zerop expr)))
	count t into the-count
	do
	(cond ((not(atom expr))
	       (show (length expr))))
	(multiple-value (monom cof term)(find-worst-nc-monomial expr))
	(cond ((not (funcall $order_function monom prev-monom))
	       (show  prev-monom monom)
	       (format t "****** out of order ****")
	       (beep)))
	(setq prev-monom monom)
	(cond (($must_replacep monom)
;;the following expand may be unnecessary with dotdistrib at true.

	       (setq repl ($multthru (mul* cof ($expand ($dot_simp_monomial monom)))))
	       (cond ((loop for v in  *troublesome-variables*
			    when
			    (appears-in repl v )
			    do (return t))
		      (format t "~%Ratsimping repl")
		      (setq repl ($multthru ($ratsimp repl)))))
	       (setq expr (sub* expr  term ))
	       (setq expr  (add* repl expr)))
	      (t
	       (cond ((fast-scalarp term)(setq expr ($multthru ($ratsimp expr)))))
	       (setq expr (sub*   expr term))(setq answer (add* answer term))))
	finally (show the-count))))
  answer)


;
;(defun $tes (expr &aux tem1 tem2 tem3)
;     (let (( $free_dot t))
;      (setq tem1 ($dotsimp expr))
;      (setq $free_dot nil)
;      (setq tem3 expr)
;      (setq tem2 nil)
;      (setq tem2 (loop until (equal tem2 tem3)do
;                       (setq tem2 tem3)
;		       (displa tem2)
;		       (setq tem3  (meval* ($expand (meval* tem3))))   ;(displa tem3)
;		       finally (return tem3)))
;      ($ratsimp (sub* tem1 tem2))))
;
(defun remove_nth (n a-list)
  (cond ((zerop n)(rplaca a-list (cadr a-list))(rplacd  a-list (cddr a-list)) a-list)
	((< n (length a-list))
	 (rplacd (nthcdr (1- n) a-list) (nthcdr  (1+ n) a-list))
	 a-list)
	(t a-list)))

(defun $simp_ncmuln (a-list flag)
  (cond ((and $free_dot $dot_simplifications)
	 ($dotsimp  (ncmuln a-list flag) ))
	(t (ncmuln a-list flag))))
(defun $simp_ncmul (&rest a-list)
  ($simp_ncmuln (copy-list a-list) nil))

;(defun rat-ncmul (&rest a-list)
;  (cond ((and $free_dot $dot_simplifications)
;	 ($dotsimp  ($nc_rat (ncmuln a-list nil) )))
;	(t ($nc_rat (ncmuln (copy-list a-list) nil)))))

(defun $dncmul(f g)
  (cond (($plusp f)
	 (loop for v in (cdr f)
	       collecting ($dncmul v g) into vtemp
	       finally (return (apply 'add vtemp))))

	(t (cond (($plusp g)
		  (loop for u in (cdr g)
			collecting (ncmul f u) into tem
			finally (return (apply 'add tem))))
		 (t (ncmul f g))))))

(defun dotsimp2 (expression )
  (cond ((atom expression) nil)
	((equal (caar expression) 'mnctimes)(setq expression ($dot_simp_monomial expression)))
	(t (dotsimp1 expression)))
  expression)

(defun dotsimp1 (expression )
    (cond ((atom expression) expression)
	(t (rplaca expression ($dot_simp_monomial (car expression)))
	   (dotsimp1 (car expression))
	   (dotsimp1 (cdr expression)))))

(defvar *nonsense* (list 'nonsense))

(defun $dot_simp_monomial (monom &aux answer tem (init-monom monom))
  (cond ((atom monom)
	 (dotsimp-atom monom))
	((atom (car monom))
	 monom)
	((equal (caar monom) 'mnctimes)
	 (cond ($dot_simplifications
		(setq answer
		      (catch 'found-replace
			(let (pattern replacement tema)
			  (loop while monom
			     do
			       (setq tem (cdr $dot_simplifications))
			       (setq monom (cdr monom))
			       (loop while tem
				  when (and $new_fast_dotsimp
					    (or (eq 0 (second tem))
						(eq (rzero) (cdr (second tem)))))
					;					    ($zerop (second tem)))
				  do (setq tem (cddr tem))
				  else
				  do
				    (cond ((atom (setq pattern (car tem)))
					   (cond ((member pattern monom :test #'equal)
						  ;;we have to handle
						  ;;the case of an atomic pattern by making
						  ;;it a standard list '(nonsense)
						  (setq pattern (list pattern)))
						 (t (setq pattern *nonsense*))))
					  (t (setq pattern (cdr pattern))))
				    (setq  tem (cdr tem))
				    (setq replacement (car tem) tem (cdr tem))
				  and
				  when
				    (initial-sublist pattern monom)
				  do
				    (setq tema (subseq (cdr init-monom) 0 (- (length init-monom) (length monom) 1)))
				    (cond ($new_fast_dotsimp
					   (setq answer
						 (new-rat-ncmul1 (ncmuln tema t)
								 replacement
								 (ncmuln (nthcdr (length pattern) monom) t))))
					  (t
					   (setq answer
						 (rat-ncmul (ncmuln tema t)
							    replacement
							    (ncmuln (nthcdr (length pattern) monom) t)))))
				    (throw 'found-replace answer))))))
		(if answer answer init-monom))
	       (t init-monom)))
	(t monom)))

(defmacro $declare_order_weights (&rest l)
  (loop for i below (length l) by 2
	do
	(putprop (nth i l)  (nth (1+ i) l) :order-weight)))

(defmacro $declare_weights (&rest l)
  (loop for i below (length l) by 2
	do
	(putprop (nth i l)  (nth (1+ i) l) :weight)))

(defun $dot_productp (x)
  (cond ((atom x) nil)
	((atom (car x)) nil)
	((equal (caar x) 'mnctimes))))

;could add to end of simpnct to change the simplifier.
;;;The next part uses the $dot_simplifications to change the result, by doing replacement
;;;For example Dot_simplifications:[y.x.x,-x.x.y,w.v,v.w] will do y.x.x-->-x.x.y and w.v-->v.w
;;;Be careful that dot simplifications in effect will affect the attempt to define
;;;new dot_simplifications. It might be better to set it to false first and then redefine it.
;
;    (cond (dot-simps (setq dot-simps (cdr dot-simps))
;		     (loop while dot-simps
;			   do
;			   (setq answer ($replace_the_pattern answer (car dot-simps)
;							   (second dot-simps)))
;			   (setq dot-simps (cddr dot-simps))
;			   finally (return answer)))
;	  (t answer))))
(defun $dot (l)
  (apply 'm. l))
(defun $my_coeff (f x)
  "like $coeff except it always works on general rep, and if x is 1 it picks the scalar part"
  (cond (($ratp f)(setq f ($ratsimp f))))
  (cond ((atom f)(cond ((equal f x) 1)
		       (t 0)))
	((equal f x) 1)
	((eq (caar f) 'mtimes)(cond ((member x f :test #'equal)
				     (delete x (copy-list f) :test #'equal))
				    ((and (equal x 1) ($scalarp f)) f)
				    (t 0)))
	((member  (caar f) '(mplus mlist mequal) :test #'eq)
	 (simplifya (cons (list (caar f))
			  (mapcar #'(lambda (y)
				     ( $my_coeff y x)) (cdr f))) nil))))

(defun $ncoeff (f x &optional (exponent 1))
  (cond ((eq x 1) ($my_coeff f x))
	((equal exponent 1)($coeff f x))
	(t ($coeff f x exponent))))


;(defun $extract_linear_equations (equations independent-monomials &aux ($expop 100)
;				  answer tem ttemp)
;
;   "Equations is a list of equations or expressions. Matrix
;    equations could have been treated by subtracting the right side from
;    the left side and then adding the list of entries to those in
;    equations."
;
;  (check-arg  equations $listp "A Macsyma List")
;  (check-arg independent-monomials $listp "A Macsyma List")
;  (setq answer  (loop for u in (cdr    independent-monomials)
;	appending
;	(loop for eqn in (cdr equations)
;	      when (atom eqn)
;	      collecting ($nc_coeff eqn u )
;	      when (appears-in eqn '$matrix)
;	      appending
;	      (cond ((eq (caar eqn) 'mequal)
;			(setq tem (sub (second eqn) (third eqn))))
;		    (t (setq tem eqn)))
;	      (loop for row in (cdr tem)
;			      appending
;			      (loop for element in (cdr row)
;				    when (not ($zerop (setq ttemp ($nc_coeff element u))))
;				    collecting ttemp))
;	      else
;	      collecting ($nc_coeff eqn u ))))
;  (cons '(mlist simp) (union answer)))


(defun $matrix_equationp (expr)
   (or ($matrixp expr) (and (null (atom expr))(eq (caar expr) 'mequal)
			    (null (atom (second expr)))($matrixp (second expr)))))


(defvar $last_monomials '((mlist)))

(defun $extract_linear_equations (equations &optional (independent-monomials
							($list_nc_monomials equations)))
    (check-arg  equations $listp "A Macsyma List")
    (loop for v in (cdr equations)
	  do (assert (or (atom v)  (not (equal(caar v) 'mequal)))))
  (cons '(mlist) (mapcar 'new-disrep
			 (extract-linear-equations (st-rat equations)
						   (st-rat independent-monomials)))))

;(defun $extract_linear_equations (equations &optional (independent-monomials ($list_nc_monomials equations))&aux ($expop 100)
;				  answer   tem ttemp)
;
;   "Equations is a list of equations or expressions. Matrix
;    equations could have been treated by subtracting the right side from
;    the left side and then adding the list of entries to those in
;    equations."
;
;  (check-arg  equations $listp "A Macsyma List")
;  (setq $last_monomials independent-monomials)
;  (check-arg independent-monomials $listp "A Macsyma List")
;  (setq answer  (loop for u in (cdr    independent-monomials)
;	appending
;	(loop for eqn in (cdr equations)
;	      when (atom eqn)
;	      collecting ($nc_coeff eqn u )
;	      when ($matrix_equationp eqn)
;	      appending
;	      (progn
;		(cond ((eq (caar eqn) 'mequal)
;		       (setq tem (sub (second eqn) (third eqn))))
;		      (t (setq tem eqn)))
;		(loop for row in (cdr tem)
;		      appending
;		      (loop for element in (cdr row)
;			    when (not ($zerop (setq ttemp ($nc_coeff element u))))
;			    collecting ttemp)))
;	      else
;	      collecting ($nc_coeff eqn u ))))
;  (cons '(mlist simp) (zl-UNION answer)))


(defun extract-linear-equations (list-eqns &optional tem all-ind-mons)
  (loop for v in list-eqns
	with varl with ind-mons = all-ind-mons
	do(cond ((polynomialp v) nil)
		((rational-functionp v)
		 (setq v (function-numerator v)))
		(t (fsignal
		     "list-eqns should be a list of polynomials or rational functions")))
	(cond ((null all-ind-mons)
	       (setq varl (list-variables list-eqns))
	       (setq ind-mons (loop for vv in varl with mon = (list nil 1 1)
				    do (setf (car mon) vv)
				    when (not (poly-scalarp  mon))
				    collecting vv))))

	appending
	(loop for va in ind-mons
	      when (not (pzerop (setq tem  (pcoeff v (list va 1 1)))))
	      collecting tem)))


(defun member-even (item a-list &aux lis answer)
  (cond ((evenp (length a-list)) (setq lis a-list))
	 (t (setq lis (butlast a-list)) (cond ((equal (car (last a-list)) item)
					      (setq answer item)))))
  (cond ((null answer)
    (loop while lis
	  do
	  (cond ((equal item (car lis)) (return item)))
	  (setq lis (cddr lis))))
	(t answer)))

(defun initial-segment-is-replaced (a-list)
  (cond ((atom a-list) (member-even a-list (cdr $dot_simplifications)))
	(t
  (loop for i from 1 to (length a-list)
	do
	(cond ((member-even  (subseq a-list 0 i) (cdr $dot_simplifications)) (return t)))))))
;
;(defun $mono (a-list n &optional $sort &aux answer tem ans1 sorted-cdr-list)
;  "Returns the dot-monomials which will not be replaced by $dot_simplifications
;  They are tail sorted if the optional third argument is given"
;  (check-arg a-list $listp "a Macsyma list")
;  (cond ((eq n 0) (cons '(mlist simp) '(1)))
;	((eq n 1) (setq answer (copy-list a-list))
;	 (loop for v in (cdr answer)
;	       when (member-even v $dot_simplifications)
;	       do (setq answer (delete  v answer)))
;	 answer)
;	(t
;  (setq sorted-cdr-list (sort (cdr a-list) '(lambda (x y) (alphalessp y x))))
;  (setq answer   (loop for u in  sorted-cdr-list collecting (cons '(mnctimes simp) (list u))))
;  (loop for i from 1 below n
;	do
;	(loop for u in  sorted-cdr-list
;              do
;	      (loop for v in answer
;		    do
;		    (setq tem (cons u (cdr v)))
;		    (setq tem (cons '(mnctimes simp) tem))
;		    (cond ((initial-segment-is-replaced tem) nil)
;			  (t (setq answer1 (cons tem answer1 ))))))
;	(setq answer answer1)
;	(setq answer1 nil))
;  (cond ($sort (setq answer (sort answer 'tail-alphalessp))))
;  (cons '(mlist simp) answer)) ))
;;a*x.y^^2.z*b*c we want to extract x.y^^2.z
(defun $extract_nc_part(monomial  &aux sc-part nc-part)
  "Returns first value nc-part of monomial and second value the sc-part if
   that can be obtained without consing, nil otherwise.  It uses the scalar property
   to check if a single atom is scalar or not, with things like ((mexpt simp) z 2) always
   assumed scalar.  If there are two non-scalars * together it only takes the first one.
   If there are two ((mnctimes ..) ) It only takes the first one. "
						;(setq monomial (simplifya monomial nil))
  (cond
    ((atom monomial) (cond (($scalarp monomial) (setq nc-part 1 sc-part monomial))
			   (t (setq nc-part monomial) (setq sc-part 1))))
    (t (case (caar monomial)
	 (mtimes
	  (loop for u in (cdr monomial)
		do
		(cond
		  ((and (not (atom u)) (eq (caar u) 'mnctimes))
		   (setq nc-part u)
		   (cond ((eq (length monomial) 3)
			  (cond ((eq (second monomial) u)
				 (setq sc-part (third monomial)))
				(t (setq sc-part
					 (second monomial))))))
		   (return 'done))))
		 (cond ((null nc-part)
			(loop for u in (cdr monomial)
			      when (and (atom u) (not ($scalarp u)))
			      do (setq nc-part u )
			      (cond ((eq (length monomial) 3)
				     (cond ((eq (second monomial) u)
					    (setq sc-part (third monomial)))
					   (t (setq sc-part
						    (second monomial))))))
			      (return 'done))))
		 (cond ((null nc-part) (setq nc-part 1 sc-part monomial))))
	 (mnctimes (setq nc-part monomial sc-part 1))
	 (mexpt (setq nc-part 1 sc-part monomial))	;
	 (otherwise (error "~A is not a product" monomial)))))
  (values nc-part sc-part))

(defun extract_nc_and_sc_parts (monomial )
  (multiple-value-bind (nc-part sc-part) ($extract_nc_part monomial)
    (cond ((null sc-part) (setq sc-part (loop for u in monomial
					      when (not(eq u nc-part))
					      collecting u))
	   (cond ((eq (length sc-part) 2) (setq sc-part (cadr sc-part))))))


    (values nc-part sc-part)))
(defun $extract_nc_and_sc_parts (monomial )
  (multiple-value-bind (nc-part sc-part) (extract_nc_and_sc_parts monomial)

    (cons '(mlist simp) (list nc-part sc-part))))

;;fast_linsolve should be like linsolve but should  convert the system
;;to a sparse matrix and then solve it.  It should return the answer as a list
;;of solutions suitable for sublis if  a certain flag is on.  Otherwise
;;it would leave them as the sparse-matrix.

;;Polynomial_linsolve should extract the equations from some equations using
;;certain independent monomials occurring.  It should then solve the system,
;;and if a flag is on return the list of solutions suitable for sublis.
;;a slow version could be:
(defvar $fast_solve t "Generally used to decide whether to call $linsolve or $fast_linsolve.
 The latter works only for number coefficients, while the former works using rattimes, etc.")

(defun $setfy (a-list)
  (loop for v in a-list
	when (not (member v tem :test #'equal))
	collecting v into tem
	finally (return tem)))

(defun $list_nc_monomials (expr &aux answer )
  (cond ((atom expr) '((mlist simp)))
	((eq (caar expr) 'mnctimes)`((mlist simp) ,expr))
	((member (caar expr) '(mlist mequal $matrix) :test #'eq)
	 (loop for u in (cdr expr)
	       appending (cdr ($list_nc_monomials u)) into a-list
	       finally (return (cons '(mlist simp) ($setfy a-list)))))
	(t  (setq expr ($expand expr))
	    (cond ((eq (caar expr) 'mplus)
		   (loop for u in (cdr expr)
			 when (not (member (setq answer (extract_nc_and_sc_parts u))
					   a-list :test #'equal))
			 collecting answer into a-list
			 finally (return (cons '(mlist simp) a-list))))
		  ((eq (caar expr) 'mtimes)
		   (cons '(mlist simp)
			 (list (extract_nc_and_sc_parts expr))))
		  (t '((mlist simp)))))))
(defun $reverse_equation (eqn)
  (cond ((atom eqn) eqn)
	((eq (caar eqn) 'mequal)(list (car eqn) (third eqn) (second eqn)))
	(t eqn)))

(defvar *monomial-table*  (make-hash-table :test 'equal))

(defun nc-monomial-table (monom &aux repl)
  (cond ((atom monom) monom)
	((atom (car monom)) monom)
	(t (cond ((eq (caar monom) 'mnctimes)

		  (setq repl (gethash monom *monomial-table* ))
		  (cond (repl (copy-tree repl))
			(t (setq repl ($rat monom monom))
			   (setf (gethash  monom *monomial-table*)  repl)
			   (copy-tree repl))))
		 (t monom)))))

(defun new-rat-ncmul (&rest list-of-terms &aux (denom 1))
  (setq list-of-terms (copy-list list-of-terms))
  (loop for v in list-of-terms
	for i from 0
	when (and (not (numberp v))
		  (or (polynomialp v)
		  (rational-functionp v)))
	do
	(cond ((rational-functionp v)(setq denom (denom v))))
	(return (cons (poly-ncmul1 (ncmuln (subseq list-of-terms 0 i) nil)
			   (num v)
			   (ncmuln (nthcdr (1+ i) list-of-terms) nil)) denom))
	finally  (return (ncmuln list-of-terms nil))))

(defun rat-ncmul (&rest list-of-terms)
  (setq list-of-terms (copy-list list-of-terms))
  (loop for v in list-of-terms
	for i from 0
	when ($ratp v)
	do
	(return (ncmul1 (ncmuln (subseq list-of-terms 0 i) nil)
			   v
			   (ncmuln (nthcdr (1+ i) list-of-terms) nil)))
	finally (return (ncmuln list-of-terms nil))))

(defun ncmul1 (mon expr mon1)
	(setq expr (minimize-varlist expr))
	(loop for v in (third (car expr))
	      when (not (fast-scalarp v))
	      collecting (ncmul* mon v mon1) into tem
	      else collecting v into tem
	      finally (return (cons (list 'mrat 'simp tem (fourth (car expr)))
				    (cdr expr)))))

(defun mysub (expr replacement-function)
  (mysub1 (copy-tree expr) replacement-function))
(defun mysub1 (vv h)
       (cond ((atom vv) (funcall h vv))
	     (t (rplaca vv (funcall h (car vv)))
		(rplacd vv (funcall h (cdr vv)))))
       (cond ((atom vv) vv)
	     (t (rplaca vv (mysub1 (car vv) h))
		(cond ((not (eq (car vv) 'mrat)) ( rplacd vv (mysub1 (cdr vv) h)))
		      (t vv)))))

(defun max-for-pred (a-list pred)
  (let ((w (car a-list)))
  (loop for v in a-list
	when (funcall pred v w)
	do (setq w v)
	finally (return w))))

;
;(defun find-worst-nc-monomial (expr &aux answer ans1 tem1
;			       cof coefficient answeru repeat-flag)
; "expr should be somewhat expanded. The result of ($multthru expr) is ok."
;  (cond ((and (not ($plusp expr))
;	      (user:appears-in expr 'mplus))(setq expr ($multthru ($ratsimp ($expand expr))))
;	 (format t "~%find-worst-nc-monomial was applied to a non sum.  Expanding and continuing with " )(displa expr)))
;  (cond ((atom expr) (setq answer expr coefficient 1 answeru expr))
;	((eq (caar expr) 'mnctimes)(setq answer expr coefficient 1 answeru expr))
;;	((member (caar expr) '(mlist mequal) :test #'eq) (loop for u in (cdr expr)
;;			     appending (cdr ($list_nc_monomials u)) into a-list
;;			     finally (return (cons '(mlist simp) ($setfy a-list)))))
;	((eq (caar expr) 'mplus) (loop for u in (cdr expr)
;			    when (null answer)do (setq answeru u)
;			    (multiple-value ( answer coefficient)
;					     (extract_nc_and_sc_parts u))
;			    else when
;			    (funcall $order_function answer
;					(progn (multiple-value (ans1 cof)
;								(extract_nc_and_sc_parts u))
;					       ans1))
;			    do (setq answer ans1 coefficient cof answeru u)
;			    else when (equal ans1 answer)
;			    do (setq repeat-flag answer) ;(show repeat-flag)
;			    finally (return answer)))
;	((eq (caar expr) 'mtimes)(multiple-value (answer coefficient
;						  (extract_nc_and_sc_parts expr))
;	 (setq answeru expr)))
;  (cond ((equal repeat-flag answer)(setq coefficient ($my_nc_coef expr answer))
;	 (setq answeru (mul* coefficient answer))
;	 (cond (($zerop coefficient)
;		(format t "~%Having to ratsimp expression of length ~A"(length expr))
;		(setq tem1 ($multthru ($ratsimp expr)))
;		(cond ((fast-scalarp tem1)(setq coefficient tem1)
;		       (setq answeru tem1) (setq answer 1)(format t "...it was scalar"))
;		      (t
;		       (format t "...starting to look for worst monomial again")
;		       (multiple-value (answer coefficient answeru)
;		       (find-worst-nc-monomial  tem1))))))))
;  (show answer)
;  (values answer coefficient answeru)))

(defun $nc_rat (expr &aux monoms)
  (setq monoms (cdr ($list_nc_monomials expr)))
  (apply '$rat (cons expr monoms)))

(defun $re_rat_the_dot_simplifications ()
  (loop for v in (cdr $dot_simplifications)
	for i from 1
	when (and (evenp i) )
	collecting (minimize-varlist ($new_rat (new-disrep (cdr  v)))) into tem
	else collecting v into tem
	finally (return (cons '(mlist simp) tem))))

;(defun $rat_the_dot_simplifications ()
;  (loop for v in (cdr $dot_simplifications)
;	for i from 1
;	when (and (evenp i) (not ($ratp v)))
;	collecting (minimize-varlist ($new_rat v)) into tem
;	else collecting v into tem
;	finally (return (cons '(mlist simp) tem))))

(defun show-genvar (&optional(a-list genvar))
  (loop for v in a-list
     do
       (format t "~%~A disreps ~A value ~A" v (get v 'disrep) (symbol-value v))))

(defun find-worst-nc-monomial (expr &aux answer ans1 tem1 cof coefficient answeru the-worst repeat-flag)
  "expr should be somewhat expanded. The result of ($multthru expr) is ok."
  (cond (($ratp expr)
	 (loop for v in (third (car expr))
	    for gen-var in (fourth (car expr))
	    when (and (not ($scalarp v))
		      (appears-in (cdr expr) gen-var)
		      (or (null the-worst)(funcall $order_function
						   the-worst v)))
	    do
	      (setq the-worst v)
	    finally (setq answer the-worst))
	 (setq coefficient ($nc_coeff expr answer)))
	((and (not ($plusp expr))
	      (appears-in expr 'mplus))
	 (setq expr ($multthru ($ratsimp ($expand expr))))
	 (format t "~%find-worst-nc-monomial was applied to a non sum.  Expanding and continuing with " )(displa expr)))
  (cond (($ratp expr) nil)
	((atom expr) (setq answer expr coefficient 1 answeru expr))
	((eq (caar expr) 'mnctimes)(setq answer expr coefficient 1 answeru expr))
	((eq (caar expr) 'mplus) (loop for u in (cdr expr)
				    when (null answer)
				    do (setq answeru u)
				      (multiple-value ( answer coefficient)
						      (extract_nc_and_sc_parts u))
				    else when
				      (funcall $order_function
					       answer
					       (progn (multiple-value
						       (ans1 cof)
						       (extract_nc_and_sc_parts u))
						      ans1))
				    do (setq answer ans1 coefficient cof answeru u)
				    else when (equal ans1 answer)
				    do (setq repeat-flag answer) ;(show repeat-flag)
				    finally (return answer)))
	((eq (caar expr) 'mtimes)(multiple-value (answer coefficient)
						 (extract_nc_and_sc_parts expr))
	 (setq answeru expr)))
  (cond ((equal repeat-flag answer)(setq coefficient ($nc_coeff expr answer))
	 (setq answeru (mul* coefficient answer))
	 (cond (($zerop coefficient)
		(format t "~%Having to ratsimp expression of length ~A"(length expr))
		(setq tem1 ($multthru ($ratsimp expr)))
		(cond ((fast-scalarp tem1)(setq coefficient tem1)
		       (setq answeru tem1) (setq answer 1)
		       (format t "...it was scalar"))
		      (t
		       (format t "...starting to look for worst monomial again")
		       (multiple-value (answer coefficient answeru)
				       (find-worst-nc-monomial  tem1))))))))
  (show answer)
  (values answer coefficient answeru))

(defun $my_nc_coef (expr var &aux  answer coeff)
  (check-arg expr '$plusp "macsyma sum")
  (loop for v in (cdr expr)
	when (equal var (progn
			  (multiple-value ( answer coeff) (extract_nc_and_sc_parts v))
			  answer))
	collecting coeff into the-coeff
	finally (return ($ratsimp (cons '(mplus) the-coeff)))))

;;note that in cre form the noncommutative monomials seem to always come
;;after the atomic variables in the genvar list ( use (mapcar 'describe genvar) to see this).
;;This means the my-ratcoeff is justified since it assumes the noncommutative variables
;;are more main, ie come later in the list.

(defun $sump (x)
  (and (consp x) (eq (caar x) 'mplus)))

(defun $nc_coeff (expr monom &optional (deg 1) &aux tem1 answer)
  (setq answer (cond ((atom expr)
		      (if (eq monom expr) 1 0))
		     (t
		      (case (caar expr)
			(mtimes (cond ((appears-in expr monom)
				       (loop for v in (cdr expr)
					  when (appears-in v monom)
					  do (setq tem1 v)
					  else
					  collecting v into the-rest
					  finally (return (mul* (apply #'mul* the-rest)	($nc_coeff tem1 monom)))))
				      (t 0)))
			(mplus (loop named sue for v in (cdr expr)
				  when (appears-in v monom)
				  collecting v into tem1
				  finally
				    (if tem1
					(loop for w in tem1
					   collecting ($nc_coeff w monom) into the-cof
					   finally (return-from sue (apply #'add* the-cof)))
					(return-from sue 0))))
			(mexpt 0)
			(rat (cond ((appears-in (cdr expr) monom)
				    (div* ($nc_coeff (second expr) monom) (third expr)))))

			(mnctimes (cond ((equal (cdr monom) (cdr expr)) 1)))
			(mrat (my-ratcoeff expr monom deg))
			(otherwise (error "invalid expression~A" expr))))))
  (if answer answer 0))

(defun number-and-zerop (x)
  (and (numberp x) (zerop x)))

;;this version may not work if the expr is not expanded
;; (defun $nc_coeff (expr monom &optional (deg 1) &aux tem1 answer)
;;   "This will probably work only on forms that are in expanded form"
;;   (setq answer (cond ((atom expr)
;; 		      (if (eq monom expr) 1 0))
;; 		     (t
;; 		      (case (caar expr)
;; 			(mtimes (loop for u in (cdr expr)
;; 				   when (equal u monom)
;; 				   do
;; 				   (return
;; 				     (apply #'mul* (cdr (delete monom (copy-list expr) :test #'equal))))
;; 				   when (and ($sump u)
;; 					     (setq tem1 ($nc_coeff u monom))
;; 					     (not (number-and-zerop tem1)))
;; 				   do
;; 				   (return (apply #'mul* tem1 (delete u (copy-list (cdr expr)) :test #'equal)))
;; 				   finally (return 0)))
;; 			(mplus (loop named sue for v in (cdr expr)
;; 				  do (setq tem1 ($nc_coeff v monom))
;; 				  when (not (and (numberp tem1)(zerop tem1)))
;; 				  collecting tem1 into tem
;; 				  finally
;; 				  (if tem
;; 				      (return-from sue (apply #'add* tem))
;; 				      (return-from sue 0))))
;; 			(mexpt 0)
;; 			(rat (cond ((appears-in (cdr expr) monom)
;; 				    (div* ($nc_coeff (second expr) monom)
;; 					  (third expr)))))

;; 			(mnctimes
;; 			 (cond ((equal (cdr monom) (cdr expr)) 1)))
;; 			(mrat (my-ratcoeff expr monom deg))
;; 			(otherwise (error "invalid expression~A" expr))))))
;;   (cond (answer answer)
;; 	(t 0)))

(defun ncsimp (expr)
  (cond (($scalarp expr) expr)
	((atom expr) expr)
	((member 'ncsimp (car expr)) expr :test #'eq)
	(t
	 (let ((nc-monoms ($list_nc_monomials expr)))
	   (loop for vv in (cdr nc-monoms)
		 collecting (mul* vv ($ratsimp ($nc_coeff expr vv)))into  the-sum
		 finally (cond ((> (length (cdr nc-monoms)) 1)

				(return (rplaca (apply 'add* the-sum) '(mplus simp ncsimp))))
			       (t (car the-sum))))))))



(defun $independent_linsolve (eqns independent-monomials &optional unknowns &aux equations)
  (setq equations ($extract_linear_equations eqns independent-monomials))
  (cond ($fast_solve ($fast_linsolve equations unknowns))
	(t
	 (cond ((null unknowns)
		(setq unknowns
		      ($list_of_variables equations))))
	 ($linsolve equations unknowns))))
(defun $extr_eqns (eqns independent-monomials &optional unknowns &aux equations)
  (setq equations ($extract_linear_equations eqns independent-monomials))
  (cond ((null unknowns)(setq unknowns ($list_of_variables equations))))
  (macsyma-list equations unknowns))

(defun $nc_coefficients (eqns &aux ans)
  (let (vars (rat-subs ($tellrat)))
    (setq ans ($extr_eqns eqns ($list_nc_monomials eqns)))
    (setq vars (third ans))
    (loop for v in (cdr vars)
	   when (or ($constantp v)
		    (appears-in rat-subs v))
	   do (setq vars (delete v vars :test #'equal)))
    (list '(mlist simp) (second ans) vars)))

(defun $find_relations_in_free_ring (a-list &aux answers eqns gen-sum repr)
   (check-arg a-list '$listp "macsyma list")
   (setq gen-sum ($general_sum a-list $aaaa))
   (setq eqns ($nc_coefficients (list  '(mlist simp) gen-sum)))
   (setq answers ($fast_linsolve eqns))
   (setq repr($sublis answers ( $general_sum
			       (loop for i from 1 to ($length (cdr a-list))
				     collecting ($concat '$term i) into temm
				     finally (return (cons '(mlist) temm)))
;	    ($create_list ($concat '$term 'i) 'i 1 ($length (cdr a-list)))
	    $aaaa)))
   (list '(mlist simp) repr answers))

(defun $list_of_variables (l &aux variables)
	(declare (special variables))
       (cond ((atom l) (error "~A is not a list" l))
	     (t (aux_to_list_of_variables  l 'variables)))
       (cons '(mlist simp)  variables ))

(defun aux_to_list_of_variables (l var1)
  (cond ((atom l)
	 (cond ((and l
		     (not (numberp l))
		     (not (member l '(simp mtimes mplus mexpt mlist mrat rat mnctimes
				      mncexpt mequal $matrix) :test #'eq))
		     (not (member l (symbol-value var1) :test #'eq)))
		(setf (symbol-value var1) (cons l (symbol-value var1))))))
	(t
	 (loop for u in l
	       do
	       (aux_to_list_of_variables u var1)))))

(defun $firstn (n a-list)
  (subseq a-list 0 (1+ n)))

(defun create-list2 (form l)
  (cons '(mlist) (apply #'create-list1 form l)))

(defun $general_sum (terms &optional coefficients &aux answer)
  (when (null coefficients)
    (setq coefficients (create-list2 '($concat '$aa i) `(i 1 ,(1- (length terms)))))
    (mfuncall '$declare_scalar_list coefficients))
  (when (> (length terms) (length coefficients))
    (error "not enough coefficients"))
  (setq answer (loop for u in (cdr terms) with sum = 0
		  for v in (cdr coefficients)
		  do (setq  sum (add* sum  (mul* u v)))
		  finally (return sum)))
  (values  answer coefficients))

(defun a-less (u v) (or (alphalessp u v) (eq u v)))

(defremember commutative-monomials (a-list n &optional (type-of-weight :weight) (reset nil))
  (let ((atomic-terms))
  (cond (reset (remprop 'commutative-monomials :memory-table)))
  (cond ((eq n 0) nil)
	((eq n 1)
	 (loop for v in a-list
	       when (eq n (degree v type-of-weight))
	       collecting (list v) into tem
	       finally (setq atomic-terms tem))
	 atomic-terms)

	(t
	 (loop for v in a-list
	       when (eq n (degree v type-of-weight ))
	       collecting (list v) into tem
	       finally (setq atomic-terms tem))
	 (loop for v in a-list
	       appending
	       (loop for w in (commutative-monomials a-list
						     (- n (degree v type-of-weight))
						     type-of-weight)

		     when (a-less v (car w))
		     collecting (cons v w))
	       into tem1
	       finally (return (append atomic-terms tem1)))))))



(defun degree (a-list &optional (type-of-weight :weight) &aux tem)
  (cond ((atom a-list) (cond ((setq tem (get a-list type-of-weight)) tem)
			(t 1)))
	(t
  (loop for v in a-list
	when (setq tem (get v type-of-weight))
	summing tem into answer
	else summing 1 into answer
	finally(return answer)))))

(defun $commutative_dot_monomials (a-list degree &aux answer)
  (setq answer (commutative-monomials (cdr a-list) degree :weight t))
  (setq answer (loop for u in answer collecting   (cons '(mnctimes) u)))
  (cons '(mlist simp) answer))

(defun $declare_scalar_list (l &aux fn)
  (setq fn (get '$declare 'mfexpr*))
  (cond (fn
	 (loop for u in (cdr l) do (funcall fn (list nil  u '$scalar)) ))
	(t (loop for u in (cdr l) do (eval `($declare  ,u $scalar))))))


(defun $plusp (x)
  (and (consp x) (consp (car x)) (eq (caar x) 'mplus)))

(defun $list_nc_parts (f &aux answer)
  (cond ((numberp f) nil)
	((atom f)(cond (($scalarp f) nil)
		       (t f)))
	((eq (caar f) 'mnctimes) f)
	((eq (caar f) 'mtimes)($extract_nc_part f))
	(t
  (check-arg f $plusp  "Maxima sum")
  (setq answer  (loop for u in (cdr f)
	collecting  ($extract_nc_part u)))
  (cons '(mlist simp) answer))))

(eval-when
    #+gcl (load  compile)
    #-gcl (:load-toplevel :compile-toplevel)
    (defvar $poly_vector (make-polynomial-vectors)))

(defvar $type_of_entries_for_poly_vector :any-macsyma)

(defun $constant_term (expr variables)
  (cond ((atom expr)
	 (cond ((member expr variables :test #'eq) 0)
	       (t expr)))
	((not ($plusp expr))
	 (cond ((appears-in expr 'mplus)
		(error "should expand expr before taking const"))
	       (t
		(loop for v in (cdr variables)
		   when (appears-in expr v)
		   do (return 0)
		   finally (return expr)))))
	(t
	 (loop for v in (cdr expr)
		when (not (loop for w in (cdr variables)
				 when (appears-in v w)
				 do (return t)))
		collecting v into the-constants
		finally  (return (addn the-constants t))))))
;(defun sh ( poly &aux tem)
;  (cond ((equal (cdr poly) 1)nil)
;	(t (setq poly (cons poly 1))))
;  (setq tem (cons (list 'mrat 'simp varlist genvar) poly))
;  (displa tem)
;  tem)

(defvar $sparse_matrix nil)

(defun  pv-get-rows-from-macsyma-equations-and-variables
	   (self equations the-variables &aux (eqn-no -1) rat-vars rat-eqn
	    constants-column const  cof a-row)
  (setf (pv-rows self) (make-array (length equations) :fill-pointer 0 :adjustable t))
  (cond (( pv-table self)(clrhash (pv-table self) ))
	(t (setf (pv-table self) (make-hash-table :test 'equal))))
  (setf (pv-type-of-entries self) :rational)
  (let
    ((.table. (pv-table self)))
    (setq constants-column nil)
    (setq rat-vars  (mapcar 'add-newvar (cdr the-variables)))
    (cond (($listp equations)(setq equations (cdr equations))))
    (loop for eqn in equations
	  do
       (cond ((or (rational-functionp eqn) (polynomialp eqn)) nil)
	     (t (cond (($ratp eqn) (setq eqn ($ratdisrep eqn))))
		(setq eqn (bring-to-left-side eqn))))
	  (cond (($zerop eqn) (format t "~%Eliminating Trivial Equation."))
		(($numberp eqn) (error "~%Inconsistent equation ~A" eqn))
		(t (incf eqn-no 1)
		   (setq a-row (make-array (* 2 (equation-length eqn)) :fill-pointer 0 :adjustable t))
		   (vector-push-extend  a-row (pv-rows self))
		    (setq rat-eqn (function-numerator (st-rat eqn)))
		   (loop for v in rat-vars
			 for vv in (cdr the-variables)
			 count v into n
			 when (not ($zerop
				     (setq cof (pcoeff  rat-eqn (list v 1 1)))))
			 do
			 (cond ((not (numberp cof))
				(setf (pv-type-of-entries self) :any-macsyma)))
			 (vector-push-extend  n a-row)
			 (vector-push-extend  cof a-row)
			 (setf (gethash  vv .table.)  n))
		   (cond ((not ($zerop (setq const
					     (pcoeff rat-eqn 1 rat-vars))))
			  (cond ((not (numberp const))
				(setf (pv-type-of-entries self) ':any-macsyma)))
			  (cond (constants-column nil)
				(t (setq constants-column
					 (make-array (length equations)
						     :fill-pointer
						     (length equations)
						     :adjustable t
						     :initial-element '0))
				   ))
			 (setf (aref constants-column eqn-no)  const)
			  (setf (pv-constants-column-number self) 0))))))

   (adjust-array (pv-rows self)
		 (max (length (the cl:array (pv-rows self)))
		      1)
		 :fill-pointer (fill-pointer (pv-rows self)))
    (cond (constants-column (setf (pv-constants-column-number self) 0)
			    (adjust-array
			      constants-column
			      (max (length (the cl:array (pv-rows self))) 1)

			      :fill-pointer
			      (fill-pointer (pv-rows self)))
			      )))
    (pv-set-up-sparse-matrix-from-rows self )
    (cond (constants-column
	   (setf (sp-constants-column (pv-the-sparse-matrix self)) constants-column))))
;
;(defun  pv-get-rows-from-macsyma-equations-and-variables
;	   (self equations the-variables &aux (eqn-no -1)
;	    constants-column const  cof a-row)
;  (setf (pv-rows self) (make-array (length equations) :fill-pointer 0 :adjustable t))
;  (cond (( pv-table self)(clrhash (pv-table self) ))
;	(t (setf (pv-table self) (make-hash-table :test 'equal))))
;  (setf (pv-type-of-entries self) :rational)
;  (let
;    ((.table. (pv-table self)))
;    (setq constants-column nil)
;    (loop for eqn in (cdr equations)
;	  do (cond (($ratp eqn)	  (setq eqn ($ratdisrep eqn))))
;	  (setq eqn (bring-to-left-side eqn))
;	  (cond (($zerop eqn) (format t "~%Eliminating Trivial Equation."))
;		(($numberp eqn) (error "~%Inconsistent equation ~A" eqn))
;		(t (incf eqn-no 1)
;
;		   (setq a-row (make-array (* 2 (equation-length eqn)) :fill-pointer 0 :adjustable t))
;		   (array-push-extend (pv-rows self) a-row)
;		   (loop for v in (cdr the-variables)
;			 count v into n
;			 when (not ($zerop
;				     (setq cof ($nc_coeff  eqn v))))
;			 do
;			 (cond ((not (numberp cof))
;				(setf (pv-type-of-entries self) :any-macsyma)))
;			 (array-push-extend a-row n)
;			 (array-push-extend a-row cof)
;			 (setf (gethash  v .table.)  n))
;		   (cond ((not ($zerop (setq const
;					     ($constant_term eqn the-variables))))
;			  (cond ((not (numberp const))
;				(setf (pv-type-of-entries self) ':any-macsyma)))
;			  (cond (constants-column nil)
;				(t (setq constants-column
;					 (make-array (length equations)))
;				   (fillarray constants-column '(0))))
;			  (aset const constants-column eqn-no)
;			  (setf (pv-constants-column-number self) 0))))))
;
;   (adjust-array-size (pv-rows self) (array-active-length (pv-rows self)))
;    (cond (constants-column (setf (pv-constants-column-number self) 0)
;			    (adjust-array-size constants-column (array-active-length (pv-rows self)))))
;    (pv-set-up-sparse-matrix-from-rows self )
;    (cond (constants-column
;	   (setf (sp-constants-column (pv-the-sparse-matrix self)) constants-column)))))


(defun fast-linsolve (poly-eqns variables &aux answer (add-to-dim 0) table)
  (cond ((get (car variables) 'disrep))
	(t (fsignal "variables should be genvars")))
 (setq variables (cons '(mlist) (loop for v in variables collecting (get v 'disrep))))
   ( pv-get-rows-from-macsyma-equations-and-variables $poly_vector
			 poly-eqns variables)

  (format t "~%Assuming entries of type ~A" ( pv-type-of-entries $poly_vector ))
  (cond (variables (setf table ( pv-table $poly_vector ))
		   (setf add-to-dim
			 (loop for u in (cdr variables)
			       when (null (gethash u table ))
			       counting u and do
			       (format t "~%The variable ~A did not appear." u)))
		   (cond
		     ((not (zerop add-to-dim))
		      (format
			t "~%Thus we should add ~D to the dimension of the solution space."
			add-to-dim)))))
  (setf answer ( pv-solve-suitable-to-sublis $poly_vector ))
  answer)

(defun $fast_linsolve (eqn &optional variables &aux answer table add-to-dim)
  "Solves list of EQUATIONS in a list of VARIABLES"
  (cond ((polynomial-vectors-p $poly_vector) nil)
	 (t (setf $poly_vector (make-polynomial-vectors ))))
  (setf (pv-type-of-entries $poly_vector) $type_of_entries_for_poly_vector )
  (cond (variables ( pv-get-rows-from-macsyma-equations-and-variables $poly_vector
			 eqn variables))
	(t
	 ( pv-get-rows-from-macsyma-equations $poly_vector  eqn)))
  (cond (modulus   (setf (pv-type-of-entries $poly_vector) modulus)))
  (format t "~%Assuming entries of type ~A" ( pv-type-of-entries $poly_vector ))
  (cond (variables (setf table ( pv-table $poly_vector ))
		   (setf add-to-dim
			 (loop for u in (cdr variables)
			       when (null (gethash u table ))
			       counting u and do
			       (format t "~%The variable ~A did not appear." u)))
		   (cond
		     ((not (zerop add-to-dim))
		      (format
			t "~%Thus we should add ~D to the dimension of the solution space."
			add-to-dim)))))
  (setf answer ( pv-solve-suitable-to-sublis $poly_vector ))
  answer)

(defun $nexp (u n &aux answer)
  (cond ((atom u) (ncmuln (loop for i below n collecting u) t))
	(t
  (setf answer
	(loop for i below n
	      appending (cdr u)))
  (ncmuln answer t))))

(defun $collect (expr)
  (let (($expop 0))
    ($ratsimp expr)))

(defun $subtract_monomial (expr mon)
  (sub expr (mul ($nc_coeff expr mon) mon)))

(defvar *start-dot-replace* nil)

(defun replace-dot-simplifications (expr)
  (let ((dot-simps $dot_simplifications) (answer expr) )
    (cond ((and dot-simps *start-dot-replace*)
	   (setf dot-simps (cdr dot-simps))
		     (loop while dot-simps
			   do
			   (setf answer (test_pattern answer (car dot-simps)
							    (second dot-simps)))
			   (setf dot-simps (cddr dot-simps))
			   finally (return answer)))
	  (t answer))))

;;faster version
(defun test_pattern (adp pattern replacement &aux tem)
  "replaces the pattern in ADP (a dot product) by replacement. When
 inserted in the simplifier simpmnct it does the replacement whenever
 called."
  (cond ((atom adp) adp)
	((equal (caar adp) 'mnctimes)
	 (cond ((setf tem (ordered-sublist (cdr pattern ) (cdr adp)))
		 (ncmul (ncmuln (car tem) t ) replacement (ncmuln (cadr tem) t)))
	       (t adp)))
	(t adp)))

(defun $nsdot (&rest l)
  (let (($dot_simplifications nil))
    (ncmuln l t)))
(defun $sort_mono (variables degree)
  ($sort ($mono variables degree) $order_function))

(defun tail-alphalessp (x y)
  "If x and y are lists ordering priority is based on the ends not the beginning of the lists"
  (cond ((and (atom x) (atom y)) (alphalessp x y))
	((atom x) (alphalessp x (car (last y))))
	((atom y) (alphalessp (car (last x)) y))
	(t (let ((lengx (length x))(lengy (length y)) xi yj)
	     (loop for i downfrom (1- lengx) to 0
		   for j downfrom (1- lengy) to 0
		   do
		   (setf xi (nth i x)) (setf  yj (nth j y))
		   (cond ((alphalessp xi yj)(return t))
			 ((alphalessp yj xi)(return nil)))
		   finally (return (< lengx lengy)))))))

(deff $tail_alphalessp #'tail-alphalessp)

(defun unreplaced-monomials-in-dot-simplifications
       (&optional   (macsyma-list $dot_simplifications)&aux tem answer)
  (check-arg macsyma-list $listp "macsyma list")
  (loop for i from 2 below (length  macsyma-list) by 2
	do
	(setf tem
	       ($list_nc_parts (nth i macsyma-list)))
	(cond (($listp tem)(setf tem (cdr tem)))
	      (t (setf tem (list tem))))

	(setf answer (zl-union answer tem)))
  answer)


(defun ordered-sublist (small big &aux answer (some-of-big big))
  "Returns nil unless small appears in big in which case a list
   of alternating terms before and terms after each occurrence is returned"
  (loop  while some-of-big
	 when (initial-sublist small some-of-big)
	 do (setf answer (append
			      (list  (subseq big 0 (- (length big) (length some-of-big)))
			    (nthcdr (length small) some-of-big))
			      answer))
			 do
	 (setf some-of-big (cdr some-of-big)))
  answer)

(defvar $table_of_unreplaced nil)

(defvar $unreplaced nil)

(defun $set_dot_simplifications( l)
  (setf $dot_simplifications nil)
  (setf l (meval l))
  (setf $dot_simplifications (copy-list l))

	(show l)
	(setf $dot_simplifications (subseq l 0 3))
	(setf l (cdddr l))

	(loop while l
	      do
	      (setf $dot_simplifications (append $dot_simplifications
						  (list (meval (car l)))))
	      (setf $dot_simplifications (append $dot_simplifications
						 (list  (meval (cadr l)))))

	      (setf l (cddr l))))


(defvar $free_monomials nil)
(defvar $do_not_change_free_monomials nil)

(defun $generate_array_of_relations (variables degree relations
				     &aux left-times-gen
				     additional-degree answer ($expop 100) ans1
				     ($dot_simplifications nil))

  (setf
    answer (make-array 64 :fill-pointer 0 :adjustable t))
	(loop for gen in (cdr relations)
	      do
	      (setf additional-degree (- degree ($nc_degree gen)))
	      (loop for j to additional-degree
		    do
		    (loop for left-multiple in (cdr ($mono variables j))
			  do
			  (setf left-times-gen (ncmul left-multiple gen))

			  (loop for right-multiple in (cdr ($mono variables
							    (sub additional-degree j)))
				do
				(vector-push-extend
						      (ncmul left-times-gen
							     right-multiple) answer)))))

  (setf ans1  (adjust-array answer (length (the cl:array answer))
			    :fill-pointer (fill-pointer  answer)))
  answer)




;(defun $tes (n  relations &aux tem)
;  (setf tem ($generate_array_of_relations '((mlist simp) $x $y) n relations))
;  ( pv-get-rows-from-array-of-polynomials $poly_vector  tem 'sc_and_nc_parts)
;  ( pv-set-up-sparse-matrix-from-rows $poly_vector  )
;  ( pv-reduce $poly_vector ))
(defvar $poly_vector1  (make-polynomial-vectors))

(defun $find_rank (nc-polynomials )
  ( pv-get-rows-from-array-of-polynomials
   $poly_vector1  nc-polynomials 'sc_and_nc_parts)
  ( pv-set-up-sparse-matrix-from-rows $poly_vector1  )
  ( pv-reduce $poly_vector1 )
  (   sp-number-of-pivots ( pv-the-sparse-matrix $poly_vector1 )))


(defun $find_simplifications (nc-polynomial-relations)
   (pv-get-rows-from-array-of-polynomials $poly_vector  nc-polynomial-relations
	 'sc_and_nc_parts)
  (pv-set-up-sparse-matrix-from-rows $poly_vector  )
  (pv-solve-suitable-to-sublis $poly_vector ))

(defun join-arrays (&rest l &aux answer)
  (setf answer (make-array 100 :fill-pointer 0 :adjustable t))
  (loop for u in l
	do
	(loop for i below (length (the cl:array u))
	      do
		 (vector-push-extend  (aref u i) answer)))
  (adjust-array answer (length (the cl:array answer))
		:fill-pointer (fill-pointer  answer)))


(defvar $new_fast_dotsimp t)
(defun $com (x y &optional (zet 1) &aux term1 term2 )
  (setf term1 ( ncmul* x y))
  (setf term2 ( ncmul* y x))
  (setf term2 (mul* zet term2))
  ($ratsimp  ($dotsimp (sub* term1 term2))))





(defvar $relations nil)
(defvar $previous_degree nil)
(defvar $graded_relations nil)
(defun set-up-graded-relations (the-variables deg new-relations
				&aux poly-vectors nc-polynomials finished)
  (cond ((not (ml-typep $graded_relations :array))(setf $graded_relations (make-array 25 :adjustable t))))
  (cond ((null (aref $graded_relations deg))
	(setf (aref  $graded_relations deg)  (make-polynomial-vectors))))
  (setf poly-vectors (aref $graded_relations deg))
  (cond ((or (not (equal ( pv-relations poly-vectors ) new-relations))
	     (not (equal ( pv-variables poly-vectors ) the-variables)))
	 (unwind-protect
	   (progn
	 (format t "~%Calculating relations in degree ~A of" deg)
	 (displa new-relations)
	 (setf (pv-relations poly-vectors) new-relations )
	 (setf  ( pv-variables poly-vectors) the-variables)
	 (setf nc-polynomials ($generate_array_of_relations
				the-variables deg new-relations))
	 ( pv-get-rows-from-array-of-polynomials poly-vectors  nc-polynomials
	       'sc_and_nc_parts)
	 ( pv-set-up-sparse-matrix-from-rows poly-vectors  )
	 (setf finished t))
	   (cond (finished nil)
		 (t (setf  (pv-relations poly-vectors) nil))))
	 ( pv-reduce poly-vectors ))))

(defun $find_central_elements (variables degree &optional (relations $relations) ;(reset nil)
			       &aux poly-vectors terms gen-sum answer a-row com basis all-eqns)
  "assumes relations homogeneous of same degree"
  (check-arg relations $listp "macsyma list")

;	 (setf tem ($generate_array_of_relations variables (1+ degree) relations))
;	 ($find_rank tem)
  (set-up-graded-relations variables (1+ degree) relations)
  (set-up-graded-relations variables  degree relations)
  (setf basis  (cons '(mlist simp)( pv-get-basis (aref $graded_relations degree) )))
  (setf poly-vectors (aref $graded_relations (1+ degree)))
  (cond ((< (length $aaaa) (length (setf terms basis)))
	 (setf $aaaa (loop for i below (length terms)
			   collecting ($concat '$aa i) into vars
			   finally (return (cons '(mlist simp) vars))))
	 ($declare_scalar_list $aaaa)))
  (setf gen-sum ($general_sum basis $aaaa))

  (loop for u in (cdr variables)
	do
	(setf com ($com gen-sum u))
	(setf a-row (convert-polynomial-to-vector com ( pv-table poly-vectors )
						  nil 'sc_and_nc_parts))
	(sp-set-type-of-entries ( pv-the-sparse-matrix poly-vectors ) ':any-macsyma)
	(sp-reduce-row-with-respect-to-rows ( pv-the-sparse-matrix poly-vectors ) a-row)
	appending (loop for i below (length (the cl:array a-row)) by 2
			 when (aref a-row i)
			 collecting ($rat (aref a-row (1+ i))))
	into eqns
	finally (setf all-eqns (cons '(mlist simp) eqns)))
;
;   (loop for v in (cdr all-eqns) do (displa v))

  (setf answer ($fast_linsolve all-eqns (subseq $aaaa 0 (length terms))))
  (setf answer ($sublis answer gen-sum)))
;  (break aft)
;  ($express_in_terms_of_basis variables answer relations))

(defvar *ans* nil)

(defun  pv-get-basis (self &aux monoms)
  "Grabs the non-pivots"
  (setf *ans* nil)
 (let ((col-used-to-pivot ( sp-columns-used-to-pivot (pv-the-sparse-matrix self)))
       ($dot_simplifications nil))

  (setf monoms (cdr ($mono (pv-variables self)
			   ($nc_degree (get-key (pv-table self)  1)))))
  (loop for v in monoms
	when (not (gethash
			(gethash v (pv-table self) ) col-used-to-pivot ))
	collecting v)))

;
;(defun $test ($generators &aux tem temp2 relations)
;  (setf tem ($generate_array_of_relations 1 2 (cons '(mlist simp)
;						     (list (nth 2 $generators)))))
;  (setf temp2 ($generate_array_of_relations 1 1 (cons '(mlist simp)
;						      (list (nth 1 $generators)
;							    (nth 3 $generators)))))
;  (setf relations (join-arrays tem temp2))
;  ($find_rank relations))


(defun $express_in_terms_of_basis ( variables form &optional
				   (relations $relations);(reset nil)
				   &aux a-row deg poly-vectors)
  (setf deg ($nc_degree form))
  (set-up-graded-relations variables deg relations)
  (setf poly-vectors (aref $graded_relations deg))
  (setf a-row (convert-polynomial-to-vector form ( pv-table poly-vectors )
						  nil 'sc_and_nc_parts))
	(sp-set-type-of-entries ( pv-the-sparse-matrix poly-vectors ) ':any-macsyma)
	(sp-reduce-row-with-respect-to-rows
	      ( pv-the-sparse-matrix poly-vectors ) a-row)
	(loop for ii below (length (the cl:array a-row)) by 2
	      when (aref a-row ii)
	      collecting (mul* (aref a-row (1+ ii))
			       ( get-key ( pv-table poly-vectors )  (aref a-row ii)))
	      into tem
	      finally (return (apply 'add* tem))))

(defun $test ($generators )
  ($find_rank ($generate_array_of_relations (macsyma-list '$w '$x '$y) 4 $generators)))
(defun $find_rank_of_relations (var deg &optional (relat $relations))
  ($find_rank ($generate_array_of_relations var deg relat)))

(defvar $leading_monomial_is_lowest_degree t "Should be true for power series")

(defun $nc_degree (f  &optional (type-of-weight :weight) &aux tem)
  (cond ((null f) 0)
	((atom f)
	 (cond ((or (numberp f)($scalarp f))0)
	       (t (cond ((setf tem (get f type-of-weight)) tem)
			(t 1)))))
	((polynomialp f) (+ ($nc_degree (get (car f) 'disrep)) ($nc_degree (fifth f))))

;;;	(($plusp f)($nc_degree (second f) type-of-weight ))
	(($ratp f) ($nc_degree (num (cdr f))))
	(($plusp f)
	 (cond ($leading_monomial_is_lowest_degree
		(loop for v in (cdr f)
		      minimize ($nc_degree v type-of-weight )))
	       (t (loop for v in (cdr f)
			maximize ($nc_degree v type-of-weight )))))
	((eq (caar f) '$power) (vmul* ($nc_degree (second f) type-of-weight) (third f)))
	(($scalarp f) 0)
	((eq (caar f) 'mtimes)
	 (loop for u in (cdr f)
	       maximize ($nc_degree u type-of-weight)))
;;;	 ($nc_degree (extract_nc_and_sc_parts f) type-of-weight ))
	((member (caar f) '(rat) :test #'eq) 0)
	((eq (caar f) 'mrat)
	 (cond ($leading_monomial_is_lowest_degree
		(loop for v in (varlist f)
		      for w in (genvar f)
		      when (and (not (fast-scalarp v))(appears-in (cdr f) w))
		      minimize ($nc_degree v type-of-weight)))
	       (t
		(loop for v in (varlist f)
		      for w in (genvar f)
		      when (and (not (fast-scalarp v))(appears-in (cdr f) w))
		      maximize ($nc_degree v type-of-weight)))))
	((eq (caar f) 'mnctimes)
	 (loop for u in (cdr f)
	       summing
	       ($nc_degree u type-of-weight)))
	(t (error  "The degree of ~A is undefined." f))))
(defvar  $current_replacements nil)
(defvar  $current_weights nil)

(defun free-ncmul (&rest l)
  (let (($dot_simplifications nil))
    (ncmuln (copy-list l) t)))

(defun $mono (variables n &optional reset &aux deg)
 (cond (reset (clear-memory-function '$mono-aux)))
 (loop for v in  (cdr $dot_simplifications) by #'cddr
		    when (<= (setq deg ($nc_degree v)) n)
		    collecting v into repls
		    when (eql deg 0)
		    do (return '(mlist))
		    finally (return
			      ($mono-aux variables n repls))))

(defremember $food (a b)
  (+ a b))

(defremember $mono-aux (variables n repls &aux  tem)
  (cond ((< n 0) nil)
	((eql n 0) '((mlist) 1))
	(t
	 (cons '(mlist)
	       (loop for v in (cdr variables)
		     for deg = ($nc_degree v)
		     with op = '(mnctimes simp)
		     appending
		     (loop for mon in (cdr ($mono-aux variables (- n deg) repls))
			   with answ = nil
			   do
			   (cond ((numberp mon)
				  (cond ((not ($must_replacep v))
					 (push v answ))))
				 ((symbolp mon)
				  (cond ((not ($must_replacep
						(setq tem (list op v mon))))
					 (push tem answ))))
				 ((listp mon)
				  (cond ((not ($must_replacep
						(setq tem (cons op (cons v (cdr mon))))))
					 (push tem answ)))))
			   finally (return (nreverse answ))))))))

;;;this is too messy.
;(defun $mono (variables n &optional reset &aux answer tem)
;  (cond (reset (setq $current_variables nil)))
;  (cond
;    ((< n 0) (cons '(mlist simp)nil))
;    (t
;     (check-current-variables variables n)
;     (cond ((> (1+ n) (array-total-size $current_monomials))
;			   (setq $current_monomials
;				 (adjust-array-size $current_monomials (+ n 10)))))
;     (loop for v in (cdr variables)
;	   when ($must_replacep v)
;	   do (setq variables (zl-delete v variables )))
;     (cond
;       ((setq answer (aref $current_monomials n)) answer)
;	   ((eq n 0) (aset '((mlist simp) 1) $current_monomials 0)
;	    '((mlist simp) 1))
;	   (t
;
;	      (loop for u in (cdr variables)
;		    when (not (zerop ($nc_degree u)))
;		    appending
;		    (loop for ww
;			  in (cdr ($mono variables (- n ($nc_degree u))))
;			  when (not (initial-segment-is-replaced
;				      (setq tem (free-ncmul u ww))))
;			  collecting tem)
;		    into monos
;		    else
;		    appending
;		    (list u ) into monos
;		    finally (setq answer (cons '(mlist simp) monos))
;
;
;		    (aset answer
;			  $current_monomials n)(return answer)))))))
;
;
;(defun check-current-variables (variables n &aux deg )
;  (cond ((or (null $current_variables)(null $current_replacements))
;	 (setq $current_variables  variables)
;	 (setq $current_weights
;	       (loop for v in (cdr variables)
;		     collecting
;		     ($nc_degree v) into weight-list
;		     finally (return (cons '(mlist simp) weight-list))))
;;	 (loop for arr in '($current_replacements $current_monomials)
;;	       do
;;	       (cond ((arrayp (symbol-value arr))(fillarray (symbol-value arr)  nil))
;;		     (t  (setf (symbol-value arr) (make-array 35 :fill-pointer 0 adjustable t :leader-length 2 )))))
;	 (cond ((arrayp $current_monomials)(fillarray  $current_monomials   nil))
;	       (t (setq $current_monomials
;			(zl-make-array 35 :fill-pointer 0 :adjustable t :leader-length 2 ))))
;	 (cond ((arrayp $current_replacements)(fillarray   $current_replacements nil))
;	       (t (setq $current_replacements
;			(make-array 35 :fill-pointer 0 :adjustable t :leader-length 2 ))))
;	 (loop for u in (cdr $dot_simplifications)
;	       by #'cddr
;	       do
;	       (setq deg ($nc_degree u))
;	       (aset (cons u (aref $current_replacements deg))
;		     $current_replacements deg))
;	 (setf (array-leader $current_replacements 1) $current_variables))
;	(t (cond
;	     ((not
;		(and
;		  (equal $current_variables variables)
;		  (equal (array-leader $current_replacements 1) variables)
;		  (loop for u in (cdr variables) and v in (cdr $current_weights)
;			when (not (eql ($nc_degree u) v))
;			do
;			(show u v)
;			(return nil)
;			finally (return t))))
;	      (setq $current_variables nil)(check-current-variables variables n))
;	     ((not
;		(loop for u in (cdr $dot_simplifications)
;		      by #'cddr
;		      when (<=
;			     (setq deg ($nc_degree u)) n)
;		      count 1 into num-repl
;		      and do
;		      (cond ((not (zl-member u(aref $current_replacements deg)))
;				    (return nil)))
;		      finally (cond ((not (eq num-repl
;					      (loop for i to n summing
;						    (length (aref
;							      $current_replacements i)))))
;				     (return nil))
;				    (t (return t)))))
;	      (loop for i from n below (array-total-size $current_monomials)
;		    do (aset nil $current_monomials i)
;		    (aset nil $current_replacements i))
;	      (loop for u in (cdr $dot_simplifications)
;		    by #'cddr
;		    when (>=	       (setq deg ($nc_degree u)) n)
;		    do
;		    (aset
;			  (cons u (aref
;				    $current_replacements deg))
;			   $current_replacements deg)))
;	     (t 'they-were-ok)))))

;(defun $separate_parameters (expr list-parameters)
;
;  (loop for par in (cdr list-parameters)
;	do (setf answer (copy-list expr))
;
;	collecting
;	(loop for other-par in (cdr list-parameters)
;	      when (not (eq other-par par))
;	      do (setf answer (nsubst 0 other-par  answer))
;	      else do (setf answer (nsubst 1 other-par answer))
;	      do (displa answer)
;	      finally (return ($ratsimp answer)))
;	into tem
;	finally (return (cons '(mlist simp) tem))))

(defun $separate_parameters (expression &rest parameters &aux answer list-parameters)
   (cond ((null parameters) (setf parameters '("aa" "par"))))
  (setf list-parameters (apply '$list_variables expression parameters))
  (loop for par in (cdr list-parameters)
	do (setf answer (copy-list expression))
	collecting
	  (loop for other-par in (cdr list-parameters)
		when (not (eq other-par par))
		  collecting (list '(mequal) other-par 0) into subs
		else collecting (cons '(mequal) (list other-par 1)) into subs
		finally  (return ($ratsimp
				   ($sublis (cons '(mlist simp)subs) answer))))
	  into tems
	finally (return (cond ((null (cdr list-parameters))
			       (format t "~%Special soln:")
			       (list '(mlist) expression))
			      (t (cons '(mlist simp) tems))))))

(defun  pv-clean-up (self)
    (setf (pv-relations self) nil)
    (setf (pv-variables self) nil)
    (setf (sp-rows (pv-the-sparse-matrix self)) nil)
    (setf (pv-rows self) nil)
    (clrhash (pv-table self) ))



(defun $find_relation (variables degree monoms &optional (relations $relations)
		       ;;(reset nil)
			       &aux poly-vectors terms gen-sum answer a-row eqns all-eqns)
  "assumes relations homogeneous of same degree"
  (check-arg relations $listp "macsyma list")
    (check-arg monoms $listp "macsyma list")
;	 (setf tem ($generate_array_of_relations variables (1+ degree) relations))
;	 ($find_rank tem)

  (set-up-graded-relations variables  degree relations)
;  (setf basis  (cons '(mlist simp)( pv-get-basis (aref $graded_relations degree) )))
  (setf poly-vectors (aref $graded_relations degree))


  (cond ((< (length $aaaa) (length (setf terms monoms)))
	 (setf $aaaa (loop for i below (length terms)
			   collecting ($concat '$aa i) into vars
			   finally (return (cons '(mlist simp) vars))))
	 ($declare_scalar_list $aaaa)))
  (setf gen-sum ($general_sum monoms $aaaa))

	(setf a-row (convert-polynomial-to-vector gen-sum ( pv-table poly-vectors )
						  nil 'sc_and_nc_parts))
	(sp-set-type-of-entries
	  ( pv-the-sparse-matrix poly-vectors ) ':any-macsyma)
	(sp-reduce-row-with-respect-to-rows
	  ( pv-the-sparse-matrix poly-vectors ) a-row)
	(setf eqns (loop for i below (length (the cl:array a-row)) by 2
			 when (aref a-row i)
			 collecting ($rat (aref a-row (1+ i)))))

	(setf all-eqns (cons '(mlist simp) eqns))
  (setf answer ($fast_linsolve all-eqns (subseq $aaaa 0 (length monoms))))
  (setf answer ($sublis answer  gen-sum)))
(defvar *show-entry-type* t)

(defun $determinant_of_equations (eqn &optional variables &aux answer )
  (cond ((ml-typep $poly_vector 'polynomial-vectors) nil)
	(t (setq $poly_vector (make-polynomial-vectors))))
  (setf (pv-type-of-entries $poly_vector) $type_of_entries_for_poly_vector)
  (cond (variables ( pv-get-rows-from-macsyma-equations-and-variables $poly_vector
		    eqn variables))
	(t
	 ( pv-get-rows-from-macsyma-equations $poly_vector  eqn)))
   (cond (*show-entry-type*
	  (format t "~%Assuming entries of type ~A" (pv-type-of-entries $poly_vector))))
  (setq answer (sp-determinant (pv-the-sparse-matrix $poly_vector)))
  (new-disrep answer))


;(defun fast-linsolve (eqns vari &aux solu)
;  (setq solu ($fast_linsolve eqns (cons '(mlist)(mapcar #'(lambda (u) (get u 'disrep)) vari))))
;  (show (length solu))
;  (loop for v in (cdr solu)
;	collecting (cons (add-newvar (second v)) (st-rat (third v)))))



(defun $special_find_central_elements (vari deg infinites
				       &aux mons gen-sum  $vari_left
				       comu ncmonos eqns solus  general)
  (setq mons ($mono vari deg))
  (setq mons
	      (append mons (loop for v in (cdr mons)
				 collecting (ncmul* infinites v))))
  (displa mons)
  (setq gen-sum ($general_sum mons $aaaa))
  (setq general gen-sum)
  (loop for v in (cdr vari)
	do
    (setq comu ($com general v))
    (setq ncmonos ($list_nc_monomials comu))
    (setq eqns    ($extract_linear_equations `((mlist) ,comu)  ncmonos))
    (setq solus ($fast_linsolve eqns ($list_variables eqns "aa")))
    (setq general (meval* ($sublis solus general)))
    (displa general)
    (setq $vari_left ($list_variables general "aa" "par"))
    (loop for v in (cdr $vari_left)
	  for aa in (cdr $aaaa)
	  collecting (cons v aa) into subs
	  finally (setq general (sublis subs general)))
    (displa general))
  ($separate_parameters general))


(defun $solve_nc_coefficients ($list-eqns &rest variable-strings)
  ($fast_linsolve ($extract_linear_equations $list-eqns ($list_nc_monomials $list-eqns))
		  (apply '$list_variables $list-eqns  variable-strings)))



(defun gencoeff (form mon &optional( vars-to-exclude (list-variables mon)) &aux answ)
  "coerces to poly if possible.  FORM should be a polynomialp or rational-functionp"
  (cond ((polynomialp form) (pcoeff form mon vars-to-exclude))
	((rational-functionp form)(setq answ (pcoeff (num form) mon vars-to-exclude) )
	 (cond ((pzerop answ) 0)
	       (t (setq answ (ratreduce answ (denom form)))
		  (cond ((eql 1 (denom answ))(num answ))
			(t answ)))))
	(t (fsignal "unknown type.  Wants a polynomial or rational-function."))))

(defun poly-data-from-nc-matrix (nc-matrix &aux mat-polys monom-vector monoms-in-each-column
						     the-segment
						     rows a-row cof )
  "Takes input like matrix([x.y+a*y.z,z.z+v,..],[...,...,..],...,[...,...,]])
  and returns a poly-data. pd-convert-to-maxima-format is the inverse."
  (iassert ($matrixp  nc-matrix))
 (setq mat-polys
       (loop for v in (cdr nc-matrix)
	collecting
	(loop for vv in (cdr v)
	      collecting (st-rat vv))))
 (setq monom-vector
      (loop for i from 0 below (length (first mat-polys))
			 collecting
			   (loop for v in mat-polys
				 with tem
				 do (setq tem (zl-union (list-variables (nth i v)) tem))
				 when (and (not (member 1 tem))
					   (not (pzerop (gencoeff (nth i v) 1 tem))))
				   do (setq tem (cons 1 tem))
				 finally (return
				   (loop for va in tem when (eql va 1)
							 collecting va
					 else when (> ($nc_degree (get va 'disrep)) 0)
							       collecting (list va 1 1 ))))))
  (setq monoms-in-each-column (mapcar 'length monom-vector))
 (setq the-segment (loop for v in monoms-in-each-column
	collecting all
	summing v into all))
  (setq rows (make-array (length mat-polys) :fill-pointer (length mat-polys)))
  (loop for ro in mat-polys
	for ii from 0
	do
    (setq a-row (make-array 10 :fill-pointer 0 :adjustable t))
    (setf (aref rows ii) a-row)
    (loop for pol in ro
	for monoms in monom-vector
	for n in monoms-in-each-column
	  do
      (loop for mon in monoms
	    for i from 0
	    when (eql 1 mon)
	    do(setq  cof (gencoeff pol mon(list-variables monoms)))
	      else
		do (setq cof (gencoeff pol mon))
	   when (not (pzerop cof))
	      do
		 (vector-push-extend  (+ i tot-monoms) a-row)
		  (vector-push-extend   cof a-row))
	summing n into tot-monoms))
  (make-poly-data :segments the-segment
		  :rows rows
		  :big-monom-list (loop for v in monom-vector nconc v)))

(defun segment-interval (segments spot)
  (loop for v in segments
	for ii from 0
	when (> v spot)
	  do (return (1- ii))
	     finally (return ii)))

(defun pd-convert-to-maxima-format (pd &aux tem where lis a-row)
 (cons '($matrix)
       (loop for i below (fill-pointer (pd-rows pd))
	do
    (setq lis (make-list (length (pd-segments pd)) :initial-element 0))
    (loop for k below (fill-pointer (setq a-row (aref (pd-rows pd) i))) by 2
	  when (setq tem (aref a-row k))
	   do (setq where  (segment-interval (pd-segments pd) tem) )
	      (setf (nth where lis) (n+   (nth where lis)
					  (n* (aref a-row (1+ k))
					      (nth tem (pd-big-monom-list pd))))))
    (setq lis    (mapcar 'new-disrep lis))
    when (loop for v in lis when (not (pzerop v))do (return t))
    collecting (cons '(mlist) (mapcar 'new-disrep lis)))))

(defun id (mat &aux (pd  (poly-data-from-nc-matrix mat)) )
	   (describe pd)
	   (pd-convert-to-maxima-format pd))

(defun reduce-poly-data (pd &aux sp)
 (setq sp (sp-make-sparse-matrix (pd-rows pd)))
 (sp-reduce sp)
 sp)


(defun $nc_matrix_row_reduce (mat &aux pd)
 "Does row reduction, assuming that the set of non commutative monomials
are linearly independent, so that matrix([x.y,0],[x,0],[1,0]) is already
reduced.  Like most of the nc stuff it assumes that scalars preced non
scalars in the alphabet"
  (setq pd (poly-data-from-nc-matrix mat))
  (reduce-poly-data pd)
  (pd-convert-to-maxima-format pd))

(defun $nc_matrix_quotient (mat1 modulo-mat &aux all-rows big-mat big-pd )
  "returns a matrix whose rows are a basis for the quotient row space of mat1 modulo modulo-mat"
  (iassert ($matrixp modulo-mat))  (iassert ($matrixp mat1))
  (setq big-mat (append modulo-mat (cdr mat1)))
  (setq big-pd (poly-data-from-nc-matrix big-mat))
  (setq all-rows (listarray (pd-rows big-pd)))
  (multiple-value-bind (sp1 sp2)
      (sp-quotient-space-basis (subseq all-rows 0 ($length modulo-mat))
			       (nthcdr  ($length modulo-mat) all-rows))
    sp2
   (setf (pd-rows big-pd) (sp-rows sp1))
    (pd-convert-to-maxima-format big-pd)))
