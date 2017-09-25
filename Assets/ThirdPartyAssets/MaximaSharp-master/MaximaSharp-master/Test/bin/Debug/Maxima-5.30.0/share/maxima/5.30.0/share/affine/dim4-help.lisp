;;; -*- Mode:Lisp; Package:CL-MAXIMA; Base:10 -*-
(in-package :maxima)

(defun $complexity(v &optional sum)
  (cond ((eql v 0)(if sum -1 '$z))
	((mbagp v)
	 (cond (sum
		(loop for u in  (cdr v)
		      sum ($complexity u sum)))
	       (t (cons (car v) (mapcar '$complexity (cdr v))))))
	(t (gen-pcomplexity (st-rat v)))))

(defun complexity-difference1 (x y)
 (cond ((eq x '$z) (setq x -1)))
  (cond ((eq y '$z) (setq y -1)))
 (cond ((numberp x)
	(- x y))
       ((mbagp x)
	(loop for v in (cdr x) for w in (cdr y)
	      sum (complexity-difference1 v w)))))


(defun $complexity_difference(a-list v )
  (cond ((eql v 0) -1)
	((let ((x ($complexity  ($psublis a-list v)))
	       (y  ($complexity  v)))
	 (complexity-difference1 y x)))))

(defun $pdegree(v va)
  (cond ((eql v 0) '$z)
	((mbagp v)
	 (cons (car v)
	       (loop for w in (cdr v) collect ($pdegree w va))))
	(t (pdegree (st-rat v) (car (st-rat va))))))
						;

(defun $delete_matrix_row(mat i)
  (cons (car mat)
	(loop for v in (cdr mat) for j from 1
	       unless (eql j i) collect v)))

(defun $complexity_less_p (x y) (< ($complexity x t) ($complexity y t)))

(defun $my_rat (x)
  (cond ((mbagp x)
	 (cons (car x) (mapcar '$my_rat (cdr x))))
	(t (header-poly (st-rat x)))))

(defun $mat_entry(mat i j)
  (nth  j (nth  i mat)))

(defvar $pivot_list '((mlist)))

(defun $eliminate_pivot (mat i j)
  (setq mat ($my_rat mat))
  (let ((pivot ($mat_entry mat i j))
	(pivot-row (nth  i mat)))
    (format t "~%Using pivot:")
    (sh pivot)
   (setq $pivot_list ($append $pivot_list `((mlist) ,(header-poly pivot) ,i ,j)))
    (cons (car mat)
    (loop for v in (cdr mat) for ii from 1
	  when (not (eql i ii))
	  collect
	  (let* ((this (st-rat (nth j v))))
	    (cond ((eql 0 this) v)
		  (t (let ((quot (nred this pivot)))
		       (cons (car v)
			     (loop for u in (cdr v)
				   for a in (cdr pivot-row)
				   collect
				   (header-poly
				     (n- (n* (function-numerator quot) a)
					 (n* (function-denominator quot) u)))))))))))))


(defun $psublis (a-list poly &optional(denom 1) palist)
  "use psublis([y=x^2,v=u^3],denom,poly)"
  (or palist
      (setq palist
	    (loop for (u v  repl)  in (cdr a-list) by  'cdr
		  do (check-arg u (eq (car u) 'mequal) "Type a=repl")
		  collecting
		  (cons (p-var (st-rat v))
			(st-rat repl)))))
  (cond	((mbagp poly) (cons (car poly)
			    (loop for v in (cdr poly) collect
				  ($psublis a-list v denom palist))))
	(t (header-poly(psublis palist (st-rat denom) (st-rat poly))))))


(defun $eliminate_pivot_list_hack (matrix pivot-list)
  (loop for (piv i j) on (cdr pivot-list) by 'cdddr
	do
	(format t "~%Pivot in list was :") (displa  ($matrix_entry matrix i j))
	(setq matrix ($eliminate_pivot matrix i j))
	finally (return matrix)))


(defun $get_relation_matrix(relations mons)
  (setq mons (st-rat mons))
  (cons '($matrix)
	(loop for v in (cdr relations)
	      do (setq v (st-rat v))
	      collect
	      (cons '(mlist)
		    (loop for w in mons
			  collect (header-poly  (pcoeff v w)))))))
 
(defun $switch_rows(mat i j)
  (let ((nmat (copy-list mat)))
    (setf (nth i nmat) (nth j mat))
    (setf (nth j nmat) (nth i mat))
    nmat))

(defun number_zeros(a)
  (loop for v in a when (eql v '$z) count t))

(defun $row_less (a b) (> (number_zeros a) (number_zeros b)))

(defun $row_sort (mat pred)
  (cons '($matrix) (sort (copy-list (cdr mat)) pred)))

(defun $reorder_matrix(mat &aux rows)
  (setq rows
	(loop for u in (cdr mat)
	collect (cons (loop for v in (cdr u) sum (gen-pcomplexity (st-rat v))) u)))
  (setq rows (sort rows #'< :key #'car))
  (cons '($matrix) (mapcar 'cdr rows)))

(defun $best_row(mat &aux  tem at)
	(loop for u in (cdr mat) for i from 1
	      minimize(setq tem (loop for v in (cdr u) sum (gen-pcomplexity (st-rat v)))) into the-min
	      when (eql tem the-min)
	      do (setq at i)
	      finally (return at)))


(defun $best_piv (row &aux tem at )
  (loop for v in (cdr row) for i from 1
	unless (eql (setq v (st-rat v)) 0)
	minimize (setq tem (gen-pcomplexity (st-rat v))) into the-min
	and
	when (eql tem the-min) do (setq at  i)
	finally (return at)))

(defvar $current nil)

;;sort by column.
(defun $invertible_pivots(mat g &aux pivs)
  (setq g (st-rat g))
  (loop for ro in (cdr ($transpose mat)) for i from 1
	 append
	 (loop for u in (cdr ro) for j from 1
		   do (setq u (st-rat u))
		   when (may-invertp u g)
		   collect (list '(mlist) (header-poly u) j i ($complexity ro t) (pcomplexity u)))
	 into all
	 finally (setq pivs (sort all '< :key #'(lambda (x) (nth 4 x))))
	 (return (cons '(mlist) pivs))))

;(defun $invertible_pivots(mat g &aux pivs)
;  (setq g (st-rat g))
;  (loop for ro in (cdr mat) for i from 1
;	 append
;	 (loop for u in (cdr ro) for j from 1
;		   do (setq u (st-rat u))
;		   when (may-invertp u g)
;		   collect (list '(mlist) (header-poly u) i j ($complexity ro t) (pcomplexity u)))
;	 into all
;	 finally (setq pivs (sort all '< :key '(lambda (x) (nth 4 x))))
;	 (return (cons '(mlist) pivs))))

(defun $reduce_by_complexity (&optional mat )
  (setq $current (or mat $current))
  (let* ((i ($best_row mat))
	 (j ($best_piv (nth i mat))))
    (print (list i j))
    (setq $current ($eliminate_pivot mat i j))))


(defun $sort_complexity(list)
  (loop for v in (cdr list)
	 collect (cons (pcomplexity (st-rat v)) v) into all
	 finally (setq all (sort all '< :key 'car))
	 (return (cons (car list) (mapcar 'cdr all)))))

(defvar $gcds_used '((mlist)))

(defun $p_projective (lis &optional agcd &aux (zero-row t))
  "Cancel a gcd of AGCD and The rest of the elements of LIS.  If LIS is a
list of lists call this function on each of the lists independently."
  (assert (mbagp lis))
  (and agcd (setq agcd (st-rat agcd)))
  (cond ((mbagp (second lis))
	 (cons (car lis)
	       (loop for w in (cdr lis)
		     collect ($p_projective w agcd))))
	(t
	 (loop for v in (cdr lis)
	       do (setq v (st-rat v))
	       (assert (polynomialp v))
	       (cond ((and zero-row (not (pzerop v)) (setq zero-row nil))))
	       (setq agcd (cond (agcd (pgcd agcd v))
				(t v)))
	       finally (return
			 (cond ((numberp agcd) lis)
			       (zero-row lis)
			       (t
			       
				(setq $gcds_used ($append $gcds_used `((mlist) ,(header-poly agcd))))
				(format t "~%Found a non trivial factor:") (sh agcd)
				(cons '(mlist)
				      (loop for vv in (cdr lis)
					    collect (header-poly (pquotient (st-rat vv) agcd)))))))))))



(defun $cancel_pivot(lis piv)
 (setq piv (st-rat piv))
	   (assert (polynomialp piv))
  (cond ((mbagp lis)
	 (cons (car lis) (loop for w in (cdr lis) collect($cancel_pivot  w piv))))
	(t
	   (setq lis (st-rat lis))
	   (assert (polynomialp lis))
	   (let ((tem (pgcd lis piv)))
	     (cond ((numberp tem) (header-poly lis))
		   (t (header-poly (pquotient lis tem))))))))

(defun $linearize_nc (x)
  (cond ((consp x)
	 (cond ((and (consp (car x)) (eq (caar x) 'mnctimes))
		(setf (car x) '(mtimes))
		(loop for v on (cdr x)
		   for term in (cdr x)
		   for i from 1
		   do (setf (car v) (intern (format nil "~a~d" term i)))))
	       (t (cons ($linearize_nc (car x)) ($linearize_nc (cdr x))))))
	(t x)))

(defun $copy (x) (copy-tree x))

(defun $linearize_nc_to_nc (x)
  (cond ((consp x)
	 (cond ((and (consp (car x)) (eq (caar x) 'mnctimes))
		(loop for v on (cdr x)
		   for term in (cdr x)
		   for i from 1
		   do (setf (car v) (intern (format nil "~a~d" term i)))))
	       (t (cons ($linearize_nc_to_nc (car x)) ($linearize_nc_to_nc (cdr x)))))
	 x)
	(t x)))

(defun times_4_n (n)
  (if (zerop n)
      1
      (* 4 n)))

(defun hilbert_4(n)
  (round ($binomial (+ n 3) 3)))

(push 'hilbert_4 *all-rank-functions*)

;(loop for i below 10 collect (list i (hilbert_tem i)))

(defun $minors2_2 (mat cols)
  (let ((nmat (apply '$submatrix mat (cdr cols))))
     (cons '(mlist)
	   (loop for ro in (list-tableaux 2 (1- (length nmat)))
	    do (show ro)
	    collect (cons '($matrix) (loop for i in ro
					collect (nth i nmat)))))))

(defun $minors (n mat cols)
  (let ((nmat (apply '$submatrix mat (cdr cols))))
     (cons '(mlist)
	   (loop for ro in (list-tableaux n (1- (length nmat)))
	    do (show ro)
	    collect (cons '($matrix) (loop for i in ro
					collect (nth i nmat)))))))

(loop for i below 5 collect i)

;(setq he '(1 2))

(defun cartesian-product (&rest l)
  (cond ((null l) (list nil))
	(t
	 (loop for v in (car l)
	  append (loop
		   for w in (apply #'cartesian-product (cdr l)) collect (cons  v w))))))

(defun $cartesian_product(&rest l)
  (lis (apply #'cartesian-product
	      (mapcar #'cdr l))))

(defun sublist (l pred)
  (loop for v in l when (apply pred v) collect v))

(defun lis (l)
  (cons '(mlist) (loop for w in l collect (cons '(mlist) w))))

;(setq $lis1 (lis (sublist (cartesian-product he he he he) #'(lambda ( i j l m) (and (< i j) (<= m l))))))

;(setq $lis2 (lis		 (sublist (cartesian-product he he he he) #'(lambda ( i j l m) (and (<= i j) (< m l))))))

;; POLYS is a list of polynomials and a parallel list DEGS of degrees.
;; We compute all monomials in POLYS of deg l
(defun monoms (polys degs &optional (cross '(nil)))
  (cond ((null polys) '(1))
 	(t
	  (loop for v in (monoms (cdr polys) (cdr degs))
		 append
		 (loop for j to (car degs)
			collect (n* (pexpt (car polys) j) v))))))

(defun $extract_c_equations(poly vars-to-exclude)
  (let ((monoms (st-rat vars-to-exclude))
	(vars1  (mapcar 'car (st-rat vars-to-exclude))))
    (assert ($listp poly))
  (let (ans all-monoms some-monoms)
    (setq ans
    (loop for v in (cdr poly)
	   do (setq v (function-numerator (st-rat v)))
	   (setq degs (loop for w in vars1
			     collect (pdegree v w)))
	   (setq some-monoms (monoms monoms degs))
	   (setq all-monoms (append all-monoms some-monoms))
	   nconc
	   (loop for w in some-monoms
		  collect (new-disrep (pcoeff v w vars1)))))
    (list '(mlist)
      (cons '(mlist) ans)
      (cons '(mlist) (mapcar 'new-disrep all-monoms))))))

(defun $pb ()
  (save-linenumbers :file "/tmp/lines"))
