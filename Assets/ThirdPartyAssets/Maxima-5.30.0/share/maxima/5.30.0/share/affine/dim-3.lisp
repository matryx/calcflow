;;; -*- Mode:Lisp; Package:CL-MAXIMA; Syntax:COMMON-LISP; Base:10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defun point-of-tensors (special-tensor standard-tensor
			       &key (nc-monomials ($list_nc_monomials standard-tensor)))
 (let ((  st-tensor (st-rat standard-tensor))
       ( sp-tensor (st-rat special-tensor))
       (nc-monoms  (st-rat nc-monomials)))
  (setq nc-monoms  (sort nc-monoms 'alphalessp))
   (loop for v in nc-monoms
	 collecting (pcoeff st-tensor v) into variable-names
	 collecting (pcoeff sp-tensor v) into variable-values
	 finally (return
		   (loop for vv in variable-names
			 for ww in variable-values
			 when (and (consp vv) (polynomialp vv)
				   (eq (length vv) 3))
			 collecting (cons (car vv) ww))))))

(defun gen-matrix-rows (mat )
  (cond ((matrix-p mat)(cdr mat))
	(($matrixp mat)  (mapcar 'cdr (cdr mat)))
	(t (merror "not a matrix"))))

(defun point-of-algebras (special-tensor standard-tensor q standard-q &aux tensor-subs)
  (setq tensor-subs (point-of-tensors special-tensor standard-tensor))
  (append tensor-subs
	  (loop for v in (mapcar #'st-rat (apply #'append (gen-matrix-rows q)))
	     for w in (mapcar #'st-rat (apply #'append (gen-matrix-rows standard-q)))
	collecting (cons (car w) v))))

(defun $dimensions_for_replacements ($repl-list &optional (degree 7))
 (let (($dot_simplifications
	 (cons '(mlist) (loop for v in (cdr $repl-list)
			      collecting v collecting 0)))
       ($current_variables ($list_variables $repl-list)))
   (loop for i below degree do
	(format t "~%The dimension in deg ~A is ~A and hilbert is ~A"
		($length ($mono $current_variables i))
		i
		(hilbert i)))))

(defun $normalizing_element_p (elmt &aux tem answ eqns solns  oth)
  (loop for v in (cdr $current_variables)
	do (setq tem (sub*  (ncmul* v elmt)
			    (ncmul*  elmt (setq oth ($general_sum $current_variables $aaaa )))))
	(setq answ ($totaldisrep ($numerator ($dotsimp tem))))
	(setq eqns ($extract_linear_equations  (list '(mlist) answ)))
	(setq solns ($fast_linsolve eqns (subseq $aaaa 0 (length $current_variables))))
	collecting ($sublis solns oth) into final
	do (mshow (cons '(mlist)final))
	finally (return (cons '(mlist) final))))

(defmacro while (test &body body)
       `(loop
	   (when (null ,test) (return))
	   ,@body))

(defun collect-atoms (tree &optional (pred #'(lambda (x) t)) &aux result)
   (declare (special result))
   (collect-atoms1 tree pred) result)

(defun collect-atoms1 (tree pred)
   (declare (special result))
  (cond ((atom (car tree))(cond ((funcall pred (car tree))
				 (pushnew (car tree) result :test 'equal))))
	(t (collect-atoms1 (car tree) pred)))
  (cond ((atom (cdr tree))(cond ((funcall pred (cdr tree))
				 (pushnew (cdr tree) result :test 'equal))))
	(t (collect-atoms1 (cdr tree) pred))))


(defun $find_coefficients_for_linear_combination( items in-terms-of monoms &optional (cofs $aaaa)
		 &aux (gen-sum 0) rest-monoms final-answ answ tem dif)
   (setq monoms (nreverse (st-rat  monoms)))
  (while (setq in-terms-of (cdr in-terms-of))
    (setq cofs (cdr cofs))
    (setq gen-sum (n+ gen-sum (n* (car cofs) (car in-terms-of)))))
  (while (setq items (cdr items))
    (setq dif (n- (car items) gen-sum))
    (setq answ nil)
    (setq rest-monoms  monoms)
    (while (and (setq tem (car rest-monoms))(setq rest-monoms (cdr rest-monoms)))
	      (setq tem (gencoeff dif tem))
	     (push (new-disrep tem) answ))
    (push (cons '(mlist) answ) final-answ))
  (cons '(mlist) final-answ))

(defun $generate_matrix (fun i j)
  (cons '($matrix)
	(loop for ii from 1 to i
	   collecting (cons'(mlist)
			   (loop for jj from 1 to j
				  collecting (meval* `((,fun) ,ii ,jj)))))))


(defun gen-matrix-row-plus (row1 row2 &key row1-multiple  row2-multiple)
       "form row1+row2 where the multiple you may specify multiples."
       (cond (row1-multiple (setq  row1-multiple (st-rat  row1-multiple))))
       (cond (row1-multiple (setq  row2-multiple (st-rat  row2-multiple))))
       (loop for v in (cdr row1) for w in (cdr row2)
	     when row1-multiple do (setq v (n* row1-multiple v))
	     when row2-multiple do (setq w (n* row2-multiple w))
	     collecting (n+ v w) into result
	     finally (return (cons '(mlist) result))))


(defun $interchange_matrix_columns (matrix i j &optional copy)
  (cond (copy (setq matrix
		    (cons (car matrix)
			  (loop for u in (cdr matrix)
				with max =  (1+ (max i j))
				collecting (nconc (subseq u 0 max) (nthcdr max u)))))))

  (loop for u in (cdr matrix)
	do (swapf (nth i u) (nth j u)))
  matrix)

(defun $interchange_matrix_rows (matrix i j &optional copy)
  "interchange MATRIX rows I and J.  If COPY then copy then don't do it destructively"
  (cond (copy (setq matrix (nconc (subseq matrix 0 (setq copy (1+ (max i j))))
				   (nthcdr copy matrix )))))
  (swapf (nth i matrix) (nth j matrix))
  matrix)

(defmacro matrix-entry (matrix i j)
  `(nth ,j (nth ,i ,matrix)))

(defun $multiply_row (list factor)
  (cons (car list) (loop for v in (cdr list) collecting (n* factor v))))

;;still must remove the factors along the diagonal.
(defun $decompose_symmetric_matrix (matrix &aux (ident ($ident ($length matrix))) fact
				    ident-pivot-row pivot-row orig )
  "Write MATRIX as a product of P.DIAGONAL.TRANSPOSE(P).  This returns value P."
  (setq orig matrix)
  (setq matrix (cons (car matrix) (loop for v in (cdr matrix) collecting (copy-list v))))
  (loop for ii from 1 below ($length matrix)
	do
   (loop for i from ii to ($length matrix)
	 when (not (pzerop (matrix-entry matrix i ii)))
	 do
	 (cond ((eq i ii) (return))
	       (t (setf (nth ii matrix) (gen-matrix-row-plus (nth i matrix) (nth ii matrix)))
		  (setf (nth ii ident) (gen-matrix-row-plus (nth i ident) (nth ii ident)))
		  (loop for i1 from ii to ($length matrix)
			do (setf (matrix-entry matrix i1 ii)
				 (n+ (matrix-entry matrix i1 ii) (matrix-entry matrix i1 i))))
		  (return)))
	 finally (fsignal "This implementation assumes the matrix is nonsingular"))
   (setq pivot-row ($multiply_row (nth ii matrix) (setq fact (nred 1 (matrix-entry matrix ii ii)))))
   (setf ident-pivot-row ($multiply_row (nth ii ident) fact))
   (loop for i from (1+ ii) to ($length matrix)
	 do (setf (nth i matrix) (gen-matrix-row-plus (nth i matrix) pivot-row
				    :row2-multiple (setq fact (n* -1 (matrix-entry matrix i ii)))))
	 (setf (nth i ident)(gen-matrix-row-plus (nth i ident) ident-pivot-row
				    :row2-multiple fact))))
;should be diag; (let(( answ ($new_disrep ident)))    (show (ncmul* answ orig ($transpose answ))))
 ($new_disrep ident))

(defun $new_disrep (item)
  (cond ((atom item) (new-disrep item))
	((and (listp (car item))(mbagp item)) (cons (car item) (mapcar '$new_disrep (cdr item))))
	(t (new-disrep item))))

(defun $symbol_matrix(symbol rows columns)
  "Generate the matrix of symbols starting with SYMBOL with ROWS rows
  and COLUMNS columns."
  (cons '($matrix simp)
	(loop for i from 1 to rows
	      collecting (loop for j from 1 to columns
			       collecting
			       ($concat symbol i j) into tem
			       finally (return (cons '(mlist simp) tem))))))

(defun replace-symbols (form containing-string &key prefix replacements &aux subs vari)
  (setq vari ($list_variables form containing-string))
  (setq subs (cond (prefix (loop for v in (cdr vari)
				 collecting (cons v ($concat prefix v))))
		   (t (check-arg replacements (and (listp replacements)
						   (>= ($length replacements)
						       ($length vari))) "long enough macsyma-list")
		      (cond (prefix (setf (cdr vari) (sort (cdr vari) 'alphalessp))))
		      (pairlis (cdr vari) (subseq (cdr replacements) 0 ($length vari))))))
  (sublis subs form))

(defun check-cases (roots-of-one &aux relats)
  (loop for v in (cdr roots-of-one)
	do (setq relats  (subst v '$b $relationsb))
      ($set_up_dot_simplifications relats)
      (loop for i in '(2 5 10 3 4)
	    do ($check_overlaps (1+ i) t nil)
	    (push ($fast_central_elements $current_variables i) $centrals_so_far)
	    (displa (cons '(mlist) $centrals_so_far)))))


(defun check-centrals (in-degree &key relations set_up &aux)
  (cond (set_up     ($set_up_dot_simplifications relations)))
      (loop for i in in-degree
	    do ($check_overlaps (1+ i) t nil)
	    (push ($fast_central_elements $current_variables i) $centrals_so_far)
	    (displa (cons '(mlist) $centrals_so_far))))




(defun $check_transpose_condition_2_1 (mm q &optional (variables $current_variables) &aux he)
    (setq he (ncmul*  variables mm))
    (sub* he ($transpose (ncmul* q mm ($transpose variables)))))

(defun $m_from_f (f &aux n)
  (loop for v in (cdr f) when (not (numberp v)) do (setq n ($nc_degree v)) (return))
  (let ((monoms ($mono $current_variables (1- n))))
    (cons '($matrix)
	  (loop for fi in (cdr f)
		collecting
		(cons '(mlist)
		      (loop for v in (cdr $current_variables)
			    collecting
			    (loop for w in (cdr monoms)
				  with answ = 0 with mon
				  do (setq answ (add* answ (mul*  ($ratcoef fi (ncmul* w v))
								  w)))

				  finally (return answ))))))))

(defun $diagonal (lis)
  (cons '($matrix simp) (loop for v in (cdr lis)
			      for i from 0
			      with tem
			      do (setq tem (make-list (length (cdr lis)) :initial-element 0))
			      (setf (nth i tem) v)
			      collecting (cons '(mlist) tem))))

;;checks that the relations of specified type, satisfy the xm=qf rule
;;I did it for all diagonal types and they were ok.
;(defun $dim3_relations_check(type number-variables &aux rels subs-and-relations mat qq)
;  (declare (special $relations_2 $relations_3 $rtx $rtx3))
;  (setq subs-and-relations
;	(case number-variables
;	  (2 (setq rels $relations_2)
;	     (setq mat $rtx)
;	     (setq vars '((mlist) $%alpha $%beta ))
;	     (case type
;	       ($a (nth 1 rels))
;	       ($e (nth 2 rels))
;	       ($h (nth 3 rels))
;	       ($s1(nth 4 rels))
;	       ($s2(nth 5 rels))
;	       ($s2p(nth 6 rels))
;	       (t (fsignal "type one of a,b,h,s1,s2,or s2p"))))
;	  (3 (setq rels $relations_3)
;	     (setq vars '((mlist) $%alpha $%beta $%gamma))
;	     (setq mat $rtx3)
;	     (case type
;	       ($a (nth 1 rels))
;	       ($b (nth 2 rels))
;	       ($e (nth 3 rels))
;	       ($h(nth 4 rels))
;	       ($s1(nth 5 rels))
;	       ($s1p(nth 6 rels))
;	       ($s1pp(nth 7 rels))
;	       ($s2(nth 8 rels))
;	       (t (fsignal "type one of a,b,e,h,s1,s1p,s1pp,  or s2p"))))))
; (setq rels  ($totaldisrep  ($sub_list `((mlist) ((mequal)
;						  ,vars ,(second subs-and-relations)))
;				       ($list_matrix_entries
;					 (ncmul*
;					   (third subs-and-relations) mat)))))
; (setq qq ($diagonal (second subs-and-relations)))
;  ($ratsimp ($check_transpose_condition_2_1 ($m_from_f rels) qq)))

(defun $dim3_relations(type number-variables &aux rels subs-and-relations mat)
  (declare (special $relations_2 $relations_3 $rtx $rtx3))
  (setq subs-and-relations
	(case number-variables
	  (2 (setq rels $relations_2)
	     (setq mat $rtx)
	     (setq vars '((mlist) $%alpha $%beta ))
	     (case type
	       ($a (nth 1 rels))
	       ($e (nth 2 rels))
	       ($h (nth 3 rels))
	       ($s1(nth 4 rels))
	       ($s2(nth 5 rels))
	       ($s2p(nth 6 rels))
	       (t (fsignal "type one of a,b,h,s1,s2,or s2p"))))
	  (3 (setq rels $relations_3)
	     (setq vars '((mlist) $%alpha $%beta $%gamma))
	     (setq mat $rtx3)
	     (case type
	       ($a (nth 1 rels))
	       ($b (nth 2 rels))
	       ($e (nth 3 rels))
	       ($h(nth 4 rels))
	       ($s1(nth 5 rels))
	       ($s1p(nth 6 rels))
	       ($s1pp(nth 7 rels))
	       ($s2(nth 8 rels))
	       (t (fsignal "type one of a,b,e,h,s1,s1p,s1pp,  or s2p"))))))
  ($totaldisrep ($sub_list `((mlist) ((mequal)
						  ,vars ,(second subs-and-relations)))
				       ($list_matrix_entries
					 (ncmul*
					   (third subs-and-relations) mat)))))

(defun $maybe_ldata_solve (eqns &key (inequality 1) yes &aux ld)
 (declare (special $answer $last_equations ))
 (setq $last_equations eqns )
 (format t "The value of Last_equations is :..")(displa eqns)
 (cond ((or yes (y-or-n-p "solve the system"))
	 (setq ld (make-ldata :eqns (st-rat eqns)))
	 (setq $answer (simplify-ldata ld :open-g (st-rat inequality)))
	 (setq $answer (delete-redundant-ldata $answer :ignore-ldata-inequalities t))
	 (format t "~%**The final answer is (stored in $answer) :")
	  ($ldata_disrep $answer))
	(t eqns)))



(defun $normalizing_conditions (deg-or-element &optional (auto-data) &aux  tem ld eqns )
    (declare (special $answer gen $current_variables  $bbbb))
  "Find the conditions for an ELEMENT or if element is a number for the
  general nc-polynomial in that degree to be normalized.  You may
  supply the optional equations for the automorphispm group of the
  algebra to assist in the computation.  The current variables and
  dotsimps are used."
  (cond ((numberp deg-or-element)
	 (setq gen ($general_sum ($mono $current_variables deg-or-element) $bbbb)))
	(t (setq gen deg-or-element)))
  (setq tem (cons '(mlist) (loop for v in (cdr $current_variables)
				 collecting (sub* (ncmul* v gen)
						  (ncmul* gen ($scalar_sum
							       ($concat '$c (string-grind v))
								$current_variables))))))
  (mshow tem)
  (setq tem ($dotsimp tem))
  (mshow tem)
  (setq eqns ($extract_linear_equations ($totaldisrep ($numerator tem))))
  (cond (auto-data (loop for v in (cdr auto-data )
			 collecting ($maybe_ldata_solve ($append v eqns) :yes t ) into all
			 finally (return (apply '$append all))))
	(t
	 ($maybe_ldata_solve eqns))))


(defun $ldata_disrep (lis)
  (cond ((ldatap lis)
	 (cons '(mlist) (loop for v in (ldata-eqns lis)
	       collecting (new-disrep v))))
	((ldatap (car lis))
	 (cons '(mlist) (mapcar '$ldata_disrep lis)))
	(t (fsignal "ldata_disrep only ldata or lists of ldata"))))

(defun $linear_automorphism_group (&optional
				    (relations ($relations_from_dot_simps))
				    (set_up_simps nil)
				    (variables $current_variables)
					    &aux mat subs subs-for-sublis eqns
					    image deg)
  (mshow relations)
  (or variables (seta $current_variables ($list_nc_variables relations)))
  (mshow $current_variables)
  (cond (set_up_simps
	 (setq deg (apply 'max (mapcar '$nc_degree (cdr relations))))
	 ($set_up_dot_simplifications relations deg)))
  (setq subs (loop for v in (cdr variables)
		    collecting ($scalar_sum
				($concat '$c (string-grind v))
				variables)))
  (mshow (cons '(mlist)subs))
  (setq mat  ($coefmatrix (cons '(mlist) subs) variables))
  (setq subs-for-sublis
	(cons '(mlist)
	      (loop for v in (cdr variables)
		 for w in subs
		 collecting `((mequal) ,v ,w))))
  (mshow subs-for-sublis)
  (setq image ($dotsimp ($expand ($sublis subs-for-sublis
		       relations))))
  (setq eqns ($extract_linear_equations ($totaldisrep ($numerator image))))
  ($maybe_ldata_solve eqns :inequality ($determinant mat)))

(defremember $dotsimp_remember (elmnt &optional ($dot_simplifications $dot_simplifications))
  ($dotsimp elmnt))

(defun $algebra_trace (element  basis &aux cof tem rbasis  den num)
  (setq rbasis (st-rat basis))
  (setq vars (loop for v in rbasis
		   when (listp v) collecting (car v)
		  and  do (assert (get (car v) 'disrep))))
  (loop for v in (cdr basis)
	for rv in rbasis
	do (show v rv)
	with tra = 0
	do (show v) (setq tem  ($dotsimp_remember (ncmul* element v)))
	(setq den (function-denominator tem))
	(setq num (function-numerator tem))
	(setq cof  (pcoeff num rv vars))
	when (not (pzerop cof)) do (mshow num cof)
	do
	(setq tra (n+ tra (nred cof den)))
	finally (return (new-disrep tra))))

(defun poly-scalar-p (poly)
  (and (polynomialp poly) (or (atom poly) ($scalarp (get (car poly) 'disrep)))))

(defun set-up-trace-subs (basis)
    (check-arg basis $listp "maxima list")
  (loop for v in  (cdr basis)
	collecting (add-newvar v) into old
	collecting ($algebra_trace v basis) into new
	finally (return (list old new))))

(defun $algebra_trace_matrix (basis &aux elt subs ar result simp )
  (setq subs  (set-up-trace-subs basis))
  (setq tr (make-pairing-function (cdr basis) (second subs) :test 'nc-equal))
  (check-arg basis $listp "maxima list") (setq basis (cdr basis))
  (setq ar (make-array (list (length basis) (length  basis))))
  (loop for i from 0
	for v in basis
	do
	(loop for j from i below (length  basis)
	      do (setq elt (ncmul* (nth i basis) (nth j basis)))
	      (show elt)
	      (setq simp (new-rat ($dotsimp_remember elt)))
	      (setq result (new-disrep (apply-linear-function tr  simp)))
	      (setf (aref ar i j) result)
	      (setf (aref ar j i) result)))
  (values (maxima-matrix-from-array ar) subs tr))

(defun $trace_general_element (elt basis basis-trace &aux result simp)
  (setq tr (make-pairing-function (cdr basis) (cdr basis-trace) :test 'nc-equal))
  (setq simp (new-rat ($dotsimp_remember elt)))
  (setq result (new-disrep (apply-linear-function tr  simp))))


(defun maxima-matrix-from-array  (ar &aux (dims  (array-dimensions ar)))
  (cons '($matrix) (loop for i below (car dims)
			 collecting
			 (cons '(mlist) (loop for j below (second dims)
					     collecting (aref ar i j))))))

(defun make-pairing-function (domain range &key (test 'alike))
  #'(lambda (u) (loop for v in domain
		      for w in range
		      when (funcall test u v)
		      do (return w)
		      finally (error "u was not in the domain ~A" domain))))


(defun apply-linear-function (fn expr &aux (den 1) result num-answ)
  "fn should be a function which is defined on the monomials and this extends it to all"
   (cond ((polynomialp expr) (setq den 1))
	 ((rational-functionp expr)   (setq den (denom expr))(setq expr (num expr)))
	 (t (fsignal "expr must be a polynomial or rational function")))
   (show den)
   (setq num-answ
	 (cond ((atom expr)(nred (n* expr (funcall fn 1)) den))
	       (t (cond (($scalarp (get (car expr) 'disrep))(show expr)
			 (n* expr (funcall fn 1)))
			(t (assert (poly-scalar-p (p-cof expr)))
			   (setq result(n* (p-cof expr)
					   (funcall fn (get (car expr) 'disrep))))
			   (cond ((cdddr expr)
				  (assert (and (eql (fourth expr) 0)
					       (null (nthcdr 5 expr))))
				  (n+ (apply-linear-function fn (fifth expr))
				      result))
				 (t result)))))))
   (nred num-answ den))

(defun dual-basis-element (number-of-element-in-basis trace-matrix &optional (basis $basis)
						      &aux result cond0 actual-conditions
						      vari)
  (declare( special $basis))
  "if trace-matrix is the <ui,uj> for an inner product, find the elt u, such that
   <u,ui>=1 and <u,uj>=0 for i not = j, where i= number-of-element-in-basis."
  (setq cond0 ($list_matrix_entries
		(ncmul* trace-matrix (setq vari (subseq $aaaa 0 (length basis))))))
  (setq actual-conditions   (cons '(mlist) (loop for v in (cdr cond0 )
						for i from 1
						when (eql i number-of-element-in-basis)
						collecting (sub* v 1)
						else collecting v)))
  (setq result ($fast_linsolve actual-conditions vari))
  ($sublis result ($general_sum basis $aaaa)))

(defun $linear_variables (eqns &optional constants &aux tem vari var-eqns)
  (cond (constants (setq constants (list-variables (st-rat constants)))))
  (setq eqns (st-rat eqns))
  (setq vari (list-variables eqns))
  (setq var-eqns (mapcar 'list-variables eqns))
  (loop for v in vari
	with mon = (list nil 1 1)
	do
	(loop for w in var-eqns
	      for eqn in eqns
	      when (member v w)
	      do (setf (car mon) v) (setq tem (pcoeff eqn mon))
	      (cond ((> (pdegree eqn v) 1)(setq vari (delete v vari)))
		    (t
		     (cond ((numberp tem))
			   ((and constants (every #'(lambda (var)(member var constants))
						  (list-variables tem))))
			   (t
			    (setq vari (delete v vari))))))))
  (values (cons '(mlist) (loop for v in vari collecting (get v 'disrep)))
	  vari))

(defun $fast_linsolve_inconsistent_equations ( &optional (disrep t))
  (let ((eqns (find-extra-conditions (pv-the-sparse-matrix $poly_vector))))
    (cond (disrep (cons '(mlist) (mapcar 'new-disrep eqns)))
	  (t eqns))))

(defun find-extra-conditions (sp-mat)
  (loop for i below (array-total-size (sp-column-used-in-row sp-mat))
     when( and  (null (aref (sp-column-used-in-row sp-mat) i))
		(not (new-zerop (aref (sp-constants-column sp-mat) i))))
     collecting (aref (sp-constants-column sp-mat) i)))

(defun $my_phi(form)
  (setq form ($ratsimp form))
  (rota1 form))

(defun rota1 (form)
   (cond ((atom form) form)
	 ((eql (caar form) 'mnctimes)
	  (cons (car form) (cons (car (last form)) (butlast (cdr form) 1))))
	 (t (cons (car form) (loop for v in (cdr form ) collect (rota1 v))))))

(defun $central_rels( lis)
 (cons '(mlist) (loop for v in (cdr lis)
	 append
	 (loop for w in (cdr ($append lis  $current_variables))
	    collect (new-disrep (n- (n. v w) (n. w v)))))))

;;Make lis1 commute with lis2

(defun $centralize_rels (lis1 lis2)
  (cons '(mlist)
  (loop for v in (cdr lis1)
	 append
	 (loop for w in (cdr lis2)
		collect
		(new-disrep (n- (n. v w) (n. w v)))))))

(defun $set_up (rels &optional n)
  (setq *previously-checked-pairs* nil)
  ($set_up_dot_simplifications rels)
    (setq $bil (cons '(mlist) *all-dotsimp-denoms*))
    (setq  *all-dotsimp-denoms* nil)
    ( format t "~%Beginning check overlaps")
     (prog1 ($check_overlaps n t nil)
 (setq $denoms (cons '(mlist) *all-dotsimp-denoms* ))))

(defun $half_quantum_rels (rel3 m)
  (let* ($dot_simplifications
	 (vec ($sort ($list_variables ($list_nc_monomials rel3))))
	 (matt ($generate_matrix '(lambda (i j) ($concat '$u i j)) m m))
	 (joe ($sub_list `((mlist)
			   ((mequal)
			    ,vec ,($list_matrix_entries
				   (ncmul* matt ($transpose vec)))))
			 rel3)))
    (format t "Using variables:" )(displa vec)
    (setq joe (simplifya joe nil))
    (displa joe)
    (let ((ctls ($centralize_rels ($list_matrix_entries matt) vec))
	  dotted mon)
	  (displa ctls)
      ($set_up ($append ctls rel3) 4)
      (setq dotted ($dotsimp joe))
      (setq mon ($mono vec 2))
      ($set_up (sub* mon ($firstn ($length mon) $aaaa)) 1)
      ($print $dot_simplifications)
      (setq dotted ($dotsimp dotted))
      (apply  '$append (cdr ($separate_parameters dotted "aa"))))))






(defvar *nvars* 6)
(defun list-ordered-pairs-with-repeat-count(i j n nrepeats)
  ;; return the list of 2*n integers all less than i
  ;; such that there are no more than NREPEATS.
  (let ((*nvars* i))
    (list-ordered-pairs-with-repeat-count1 i j n nrepeats)))

(defun repeat-ok1 (i j lis nrepeats &aux (n 0))

  ;; return t if (append (list i j) lis) has no more than nrepeats
  (cond ((eql i j) (setq n 1)))
  (cond ((member i lis) (setq n (+ n 1))))
  (cond ((member j lis) (setq n (+ n 1))))
  (cond ((> n nrepeats) nil)
	(t (loop for x on lis when (member (car x) (cdr x))
		  do (setq n (1+ n)))
	   (cond ((> n nrepeats) nil)
		 (t t)))))

(defun list-ordered-pairs-with-repeat-count1 (i j number-pairs number-repeats)
  (cond ((<= number-pairs 0) (list nil))
	(t
	 (loop for ii to i
		when ( < ii *nvars*)
		append
		(loop for jj below (if (eql ii i) j *nvars* )
		       append
		       (loop for u in (list-ordered-pairs-with-repeat-count1
					ii jj (- number-pairs 1) number-repeats)
			      when (repeat-ok1 ii jj u number-repeats)
				      collect (append (list ii jj) u)))))))



(defun extract_cof1 (f vars)
  (cond ((atom f) (list f))
	((unless (polynomialp f)
	    (progn  (setq f (numerator f)) nil)))
	((member (car f) vars)
	 (loop for (deg cof) on (cdr f) by #'cddr
		nconc (extract_cof1 cof vars)))
	(t (list f))))

(defun $extract_linear_from_commutative (form vars)
  (assert  (listp form))
  (setq form (st-rat form))
  (setq vars (list-variables (st-rat vars)))
  (cons '(mlist)
	(mapcar #'new-disrep
		(loop for f in form
		   appending
		     (extract_cof1 f vars)))))
