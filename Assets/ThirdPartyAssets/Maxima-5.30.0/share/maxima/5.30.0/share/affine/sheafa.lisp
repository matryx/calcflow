;;; -*- mode: lisp; package: cl-maxima; syntax: common-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;(defstruct ( bil (:type list) :named  (:conc-name go-))
;  head
;  tail)


(defun update (ldata)
  (cond ((< (length ldata) 6)
	 (nconc ldata (loop for i from (length ldata) below 6
			    collecting nil)))))
;
;(defstruct  (subscheme (:type list) :named )
;  s-var
;  ;;    for each open of the s-var need
;  ;;the defining equations
;  ideal-sheaf)
;
;
;(defstruct (open :named (:conc-name open-))
;    ;; a list of variables
;    variables
;    ;;a polynomial in prev variables to invert
;    inequality
;    ;;record the ordered pair of opens this came from
;    intersection)
;
;(defstruct (map :named (:conc-name map-))
;  ;;an open
;  domain
;  ;;an open
;  range
;  ;;a list of polynomial functions( maybe the same denominator)
;  substitutions
;  denom)
;
;(defstruct (morphism :named (:conc-name morphism-))
;  ;;two s-varieties
;  domain
;  range
;  ;;for each open in (sv-opens domain) a map  to one in
;  ;; (sv-opens range)
;  list-of-maps)
;;
;;(defun sv-find-intersections (s-var &key (set-them t) &aux tem answ)
;;  (setq answ (loop for v in (sv-opens s-var)
;;		   appending
;;		   (loop for w   in (sv-opens s-var)
;;			 when (not (eql v w))
;;			 collecting
;;			 (setq tem (make-open))
;;			 and
;;			 do
;;			 (setf (open-variables tem) (open-variables w))
;;			 ;;ought to set the inequality here but with out the map how??
;;			 (setf (open-inequality tem) (open-inequality v))
;;			 (setf (open-intersection tem )(list v w)))))
;;  (cond (set-them (setf (sv-intersections s-var) answ)))
;;  answ)
;(defun set_up_s_var(a-list birat-string &aux map answ amap subl subs intop )
;  "takes alternating list of coord inequal equations "
;  (setq answ (make-s-var))
;  (setf (sv-opens answ)
;	(loop for (coord inequal ident) on (cdr a-list) by #'cdddr
;	      with op
;	      collecting (setq op (make-open))
;	      do (setf (open-variables op) (loop for v in (cdr coord)
;						 collecting (car (st-rat v))))
;	      (setf (open-inequality op) (st-rat inequal))
;	      (setf (open-intersection op) ident)))
;  (loop for op1 in (sv-opens answ)
;        appending
;	(loop for op2 in (sv-opens answ)
;	      for (coord inequal ident) on (cdr a-list) by #'cdddr
;	      when (not (eql op1 op2))
;	      collecting (setq intop (make-open))
;	      and
;	      do
;              (setf (open-variables intop) (open-variables op1))
;	      (setq map ($find_birational_map (open-intersection op1)
;					     (open-intersection op2)
;					     birat-string
;					     coord
;					     ))
;	      ;;;temporarily put it in the inequal slot
;	      (setf (open-inequality intop) map)
;	      (setf (open-intersection intop) (list op1 op2)))
;
;	into ints
;	finally (setf (sv-intersections answ) ints))
;  (loop for intop in (sv-intersections answ)
;	with op1 with op2
;	do (setq op1 (first (open-intersection intop)))
;	 (show intop op1 op2)
;	(setq op2 (second (open-intersection intop)))
;	collecting (setq amap (make-map domain op1
;					range op2 ))
;	into glues
;	do
;	(setq map (open-inequality intop))
;	(setq subs
;	      (loop for v in (cdr map)collecting
;		    (third v)))
;	(show subs)
;	(setq subl (loop for v in (open-variables op1)
;			 for vv in *vvv*
;			 collecting (cons vv  (get v 'disrep) )))
;	(setq subs (sublis subl subs))
;	(show subs)
;	(setq subs (mapcar 'new-rat subs))
;	(loop for v in subs
;	      with denom = 1
;	      do (setq denom (nplcm denom (denom v)))
;	      finally (loop for v in subs collecting
;			     (ptimes (num v)(pquotient denom
;							   (denom v)))
;			     into sub
;			    finally (setf (map-substitutions amap)  sub)
;			    (show sub)
;			    (setf (map-denom amap) denom)))
;	(setf (open-inequality intop)
;	      (apply-map amap (open-inequality op1)))
;	finally (setf (sv-glueing answ)glues))
;  answ)
;
;(defun find-map (list-maps dom rang)
;  (loop for ma in list-maps
;	when (and (eq (map-domain ma) dom)
;		  (eq (map-range ma) rang))
;	do (return ma)))

;(defun check-compatibility (svar)
;  (loop for v in (sv-intersections svar)
;	with rev
;	do (setq rev (reverse (open-intersection v)))
;	(loop for w in (sv-intersections svar)
;	      when (equal (rev (open-intersection w)))
;	      do
;	      (loop for va in (open-variables v) with var
;		    do
;	      (setq var (list va 1 1))
;	      (setq ma (find-map (sv-glueing svar)v w))
;	      (setq rev-ma (find-map (sv-glueing svar)w v))
;	(setq answ (ratrquotient (apply-map rev-ma (num (apply-map ma var)))a
;			    (apply-map rev-ma (denom (apply-map ma var)))))
;		(cond ((not (and (equal (num answ) var)) (denom answ) 1))
;		       (merror "not the identity map")))))))
;
;(defun describe-s-var (svar)
;  (mapcar 'describe-open (sv-opens svar))
;  (mapcar 'describe (sv-glueing svar))
;  (mapcar 'describe (sv-intersections svar)))
;(setq hi
;#$ sublis([a1=z1,a2=z2,a0=1],[ [x1,x2],[x1-a1/a0,x2-a2/a0],[x1,x2],[x1-a0/a1,x2-a2/a1],[x1,x2],[x1-a0/a2,x2-a1/a2]])$)
;
;[[X1,X2],1,[X1-Z1,X2-Z2],[X1,X2],1,[X1-1/Z1,X2-Z2/Z1],[X1,X2],1,[X1-1/Z2,X2-Z1/Z2]]$

(defvar *xxx*
  (let ((*nopoint t))
    (loop for i from 1 to 30 collecting (add-newvar (intern (format nil "$X~A" i))))))

;(defun describe-open (open &aux tem)
;  (format t "~%It is ~D space coordinatized by ~A "
;	  (length (setq tem (open-variables open))) tem)
;  (cond ((not (numberp  (open-inequality open)))
;	 (format t "less the locus where ")
;	 (sh (open-inequality open))(format t "vanishes.")))
;  (cond ((Open-intersection open)
;	 (format t "~%It is an intersection of ~A" (open-intersection open)))))
;;
;(defun $find_birational_map (eqn1 eqn2 birat-sstring coord2 &aux sol1 birat-vars
;			     answ eqns)
;  (setq eqn1  ($numerator eqn1))
;  (setq eqn2 ($numerator eqn2))
;;  (mshow eqn1)
;  (setq birat-vars ($list_variables eqn1 birat-sstring))
;  (setq sol1 ($fast_linsolve ($expand eqn1) birat-vars))
;  (setq eqn2 (sublis *vforx* eqn2 ))
;  (setq coord2 (sublis *vforx* coord2))
;    (setq eqns ($sublis sol1 eqn2))
;;    (mshow eqns coord2)
;  (setq answ ($fast_Linsolve  eqns coord2))
;  (sublis answ (sublis *xforv* answ))
;  answ)

(defun zl-pairlis (a b)
       (loop for v in a for w in b
	     collecting (cons v w)))

(defvar *vvv* (loop for i from 0 for u in *xxx* collecting (intern (format nil "$VV~A" i))))
(defvar *vforx* (zl-pairlis *xxx* *vvv*))
(defvar *xforv* (zl-pairlis *vvv* *xxx*))

;(defun construct-birational-glueing-data (s-var ring-data birat-string)
;  "ring-data is a list one for each open of equations like x0=x1*z1,x2=x1*z2
; where the x0,x1,x2 are the open coordinates, and the zi are coordinates of the
; function field"
;  (check-arg $listp ring-data "macsyma list")
; (loop for int in (s-var intersections)
;       do (setq map
;		(make-map))
;       (int-opens (open-intersection int))
;       (setf (map-domain map) int)
;       (setf (map-range map)
;	     (loop for w in int
;			      with rev = (reverse
;					   (open-intersection int))
;			      when (equal (open-intersection w)
;					  rev)
;			      do (return w)))
;       (setf (map-substitutions)
;	     (progn
;	       (loop for v in (cdr  ring-data)
;		   for w in (sv-opens s-var)
;		   when (equal w (first int-opens))
;		   do(setq eqn1 v)
;		   when (equal w (second int-opens))
;		   do (setq eqn2 w))
;		   (setq eqn2 (sublis *vforx* eqn2)
;                   (setq answ
;			 ($find_birational_map eqn1 eqn2 birat-string
;					 (firstn (length eqn2) *vvv*)))
;		 (setq substit
;		       (loop for w in answ collecting (new-rat (third w))))
;		 (loop for w in substit
;		       with denom =1
;		       do (setq denom (plcm (denom w)))
;		       finally
;		       (setq substit (loop for w in substit
;					   collecting (num (rattimes w
;								     denom))))
;		       (setf (map-denom map) denom)
;		       (setf (map-substitutions map) substit)))))))

(defvar *p3* (make-s-var))
;
;(setf (sv-opens *p3*) (loop for i from 0 to 2
;			    collecting (setq tem (make-open))
;			    do
;			    (setf (open-variables tem)
;				  (loop for j from 0 to 2
;					when (not (eql j i))
;					collecting (car (st-rat (nth j *xxx* )))))
;			    (setf (open-inequality tem) (st-rat (nth i *xxx*)))))


;;(setf (sv-intersections *p3*
;(defun apply-map (map poly &aux answ subl)
; (setq subl (pairlis (open-variables (map-range map)) (map-substitutions map)))
;  (setq answ  (psublis subl (map-denom map) poly))
;  (remove-common-factors answ (map-denom map)))
;
;(defun apply-map (map poly &aux answ subl)
; (setq subl (pairlis (open-variables (map-range map)) (map-substitutions map)))
;  (setq answ  (psublis subl (map-denom map) poly))
;  (remove-common-factors answ (map-denom map)))

(defun remove-common-factors (from-poly divisor &aux answ)
  (setq answ (pgcdcofacts from-poly divisor))
  (if (eq 1 (car answ))
      (second answ)
      (remove-common-factors (second answ) (first answ))))

(defun strings-search (keys sstring &aux tem)
  (loop for v in keys when (setq tem (search v sstring :test #'char-equal))
	do (return tem)))

;; will replace the genvar with more readable symbols
(defun replace-genvar(&rest strings &aux tem hi)
  (loop for v in *genvar*
	when (and (atom  (setq tem  (get v 'disrep)))
		  (eq (aref (string v) 0) #\G)
		  (cond (strings (strings-search strings (string tem)))
			(t t)))
	do
	(setq hi (make-symbol (string-trim "$" tem)))
	(setf (symbol-value hi) (symbol-value v))
	(setf (symbol-plist hi) (copy-list (symbol-plist v)))
	and
	collecting hi into answ
	else
	collecting v into answ
	finally (setq *genvar* answ)))

(defun describe-components (comp &aux answ)
  (check-arg comp (and (listp comp) (eq (car comp) 'components)) "components")
;  (setq comp (cdr comp))
  (loop for v in (car (cdr comp))
	for i from 0
	do (format t "~%Component ~A occurs on opens ~A on ldata ~A" i
		   (loop for  w in v  when (cdr w) collecting (car w)
			 into tem
			 and
			 collecting (cdr w) into tem2
			 finally (setq answ (list tem tem2))
			 (return (car answ)))
		   (second answ)))
  (cond ((third comp) (format t "~%There were bad components so the above must be taken with a grain of salt.  They were ~A" (third comp)))
	(t (format t "~%There were no bad components" nil))))



(defun tableaux-lessp (a b)
  (let ((stringa (string a))
	(stringb (string b)))
    (loop for j from 3  below (string-length a)
	  when (> (aref stringa j) (aref stringb j))
	  do (return nil)
	  finally (return t))))

(defun last-string (x)
  (aref x (1- (string-length x))))

(defremember list-tableaux ( row-length dimension )
  (case row-length
    (0  nil)
    (1 (loop for i from 1 to dimension
	     collecting (list i)))
    (t (loop for j from 1 to dimension
	     appending
	     (loop for u in (list-tableaux (1- row-length ) dimension)
		   when (> j (car (last u)))
		   collecting (append u (list j)))))))

(defun $list_tableaux (length dimension)
  (cons '(mlist) (mapcar #'(lambda (x)
			    (apply #'$concat '$ta x))
			 (list-tableaux length dimension))))

(defun $sub_matrix (mat  rows-to-take cols-to-take &aux row)
  (loop for ii in rows-to-take
	 do (setq row (nth ii mat))
	 collecting
	 (loop for i from 1
		for v  in (cdr  row)
		when (member i cols-to-take)
		collecting v into new-row
		finally (return (cons '(mlist) new-row)))
	 into matrix
	 finally (return (cons '($matrix simp) matrix))))

(defun $sub_matrix_columns (mat &rest cols-to-take)
  (loop for row in (cdr mat)
	 collecting
	 (loop for i from 1
		for v  in (cdr  row)
		when (member i cols-to-take)
		collecting v into new-row
		finally (return (cons '(mlist) new-row)))
	 into matrix
	 finally (return (cons '($matrix simp) matrix))))

(defun $plucker (vector-list)
  (check-arg vector-list '$listp "macsyma list")
  (let ((mat (cons '($matrix) (cdr vector-list))))
    (cons '(mlist)
	  (loop for
		u in
		(list-tableaux ($length vector-list) ($length (second vector-list)))
		collecting ($determinant (apply '$sub_matrix_columns mat u))))))

(defun $remove_number (llist)
  (loop for v in  llist when (not (numberp v))
	collecting v))

(defmacro from-case (a-list)
  `(and (listp ,a-list) (cdr (member '$case ,a-list :test #'eq))))

(defvar $solution_tree '(mlist))
(defun $sort_solution_tree ()
 (setq $solution_tree
       (cons '(mlist)(sort (cdr $solution_tree)
			   #'(lambda (x y )
			       (let ((fromx (from-case x))
				     (fromy (from-case y)))
				 (cond ((null fromx) t)
				       (t

				($monomial_alphalessp fromx
						      fromy)))))))))
(defmacro type? (a form &optional descrip &aux (ctrl-string "not a ~A"))
  (cond (descrip nil)
	(t (setq descrip form) (setq ctrl-string "Doesn't satisfy ~A")))
  (cond ((functionp form)
	 `(cond ( (, form ,a) nil)
		(t (merror (format nil ,ctrl-string ,descrip)))))
	(t `(cond (,form nil)
		  (t        (t (merror (format nil ,ctrl-string ,descrip))))))))

(defvar $nonzero_factors nil)

(defun $dichotomy (polynomials dich &optional add-to-tree  &aux answer)
  (check-arg polynomials $listp  "macsyma list")
  (cond ($nonzero_factors
	 (format t "~%Assuming the following factors are nonzero and cancelling")
	 (displa $nonzero_factors)
	 (setq polynomials ($Eliminate_nonzero_factors polynomials))))
;  (cond (add-to-tree (type? $solution_tree (and ($listp $solution_tree)
;						(or (null (second $solution_tree))
;						    ($listp (second $solution_tree)))))))

  (cond (($listp dich)(setq dich(second dich))))
  (cond ((numberp dich)(setq dich (nth  dich polynomials))))
  (setq dich
	(loop for v in (cdr ($list_irreducible_factors dich))
	      when (not (numberp v))
	      collecting v))
  (format t "We have the dichotomy among..")
    (displa (cons '(mlist) dich))
  (loop for w on dich
	for v in dich
	collecting
	(let (($nonzero_factors (append (cons '(mlist) (cdr  $nonzero_factors))
					(cdr w))))
	  ( $factor($eliminate_nonzero_factors ($simplify_relative  polynomials v))))
	into tem
	finally (setq answer (cons '(mlist) tem)))
  (cond (add-to-tree ($add_to_solution_tree answer)))
  answer)

(defun constant-term (poly)
  (cond ((atom poly) poly)
	(t (constant-term
	     (let ((leng (length poly)))
	       (cond ((eq (nth (- leng 2) poly) 0)
		      (constant-term (nth (1- leng) poly) ))
		     (t 0)))))))


(defun gone-prepared (poly &key (inequal 1) linear-variables &aux tem)
   (cond ((null linear-variables) (setq linear-variables
				      (degree-one-variables poly))))
  (cond ((numberp  inequal )
	 (one-prepared poly :linear-variables linear-variables))
	(t
	 (cond  ((atom poly) nil)
		(t (loop for v in linear-variables
			 when (eq (pdegree poly v) 1)
			 do (setq tem  (zero-sublis poly v))
			 (cond ((may-invertp tem inequal)
				      (return v)))))))))

(defun degree-one-variables (poly)
  (loop for v in (list-variables poly)
	when (eq (pdegree poly v) 1)
	collecting v ))

(defun one-prepared (poly &key linear-variables &aux tem)
 (cond ((null linear-variables) (setq linear-variables
				      (degree-one-variables poly))))
  (cond  ((atom poly) nil)
	 ((zerop (constant-term poly)) nil)
	 (t (loop for v in  linear-variables
		  when (eq (pdegree poly v) 1)
		  do (setq tem  (zero-sublis  poly v))
		  (cond ((numberp tem) (return v)))))))

;;the following was gm-prepared by x4,x3 !!
;;2*X3*X6+2*X3^2*X4-1
;;(SETQ F (RERAT (QUOTE (X6 1 (X3 1 2) 0 (X4 1 (X3 2 2) 0 -1)))))
;(defun gm-prepared (poly &key  m (inequal 1) linear-variables &aux lins tem answ)
;  (cond (m
;  (cond ((atom poly ) nil)
;	((eq 1 m)(cond ((setq tem (gone-prepared poly inequal :linear-variables linear-variables)))
;			(list tem))))
;	(t (loop for v in (list-variables poly)
;		 when
;		 (eq (pdegree poly v) 1)
;		 do (setq tem (zero-sublis poly v))
;		 (cond ((setq answ (gm-prepared tem :m (1- m) :inequal inequal))
;			(return (cons v answ))))))))
;	(t (setq lins (loop for v in (list-variables poly)
;			    when (eq (pdegree poly v) 1)
;			    count 1))
;	   (loop for i from 1 to lins
;		 when (setq answ (gm-prepared poly :m i :inequal inequal))
;		 do (return answ))))
;;;failed to check gm-prepared on this.
;(des-editor badg)1/10/85 18:02:59
;(AA1*AA2-AA1)*BB1*CC6+(AA1^2*AA2-AA1^2)*CC5+BB1*CC3+AA1*CC2+((AA1*AA2^2-AA1*AA2)*BB6+AA2*BB3-1)
;*CC1+(AA1^2*AA2-AA1^2)*BB1*BB6+(AA1^3*AA2-AA1^3)*BB5+AA1*BB1*BB3+AA1^2*BB2-AA1*BB1
;
;(SETQ BADG (RERAT (QUOTE (CC6 1 (BB1 1 (AA2 1 (AA1 1 1) 0 (AA1 1 -1))) 0 (CC5 1 (AA2 1 (AA1 2 1) 0 (AA1 2 -1)) 0 (CC3 1 (BB1 1 1) 0 (CC2 1 (AA1 1 1) 0 (CC1 1 (BB6 1 (AA2 2 (AA1 1 1) 1 (AA1 1 -1)) 0 (BB3 1 (AA2 1 1) 0 -1)) 0 (BB6 1 (BB1 1 (AA2 1 (AA1 2 1) 0 (AA1 2 -1))) 0 (BB5 1 (AA2 1 (AA1 3 1) 0 (AA1 3 -1)) 0 (BB3 1 (BB1 1 (AA1 1 1)) 0 (BB2 1 (AA1 2 1) 0 (BB1 1 (AA1 1 -1))))))))))))))
;NIL



(defun gm-all-prepared (poly &key m (inequal 1) linear-variables &aux tem)
  (cond ((null linear-variables) (setq linear-variables
				       (degree-one-variables poly))))
  (cond (m
	 (cond ((eq  m 0)(cond ((may-invertp poly inequal)(list 'ok))
			       (t nil)))
	       (t
		(loop for v in (list-variables poly)
		      when (and  (member v linear-variables :test #'eq)
				 (setq tem (gm-all-prepared
					     (zero-sublis poly v)
					     :linear-variables linear-variables
					     :m (1- m)
					     :inequal	inequal)))

		      appending (loop for ww in tem collecting (cons v ww))))))
	(t (setq linear-variables (degree-one-variables poly))
	   (loop for i from 1 to (length linear-variables)
		 when (setq tem (gm-all-prepared poly
						 :linear-variables linear-variables
						 :m i
						 :inequal	inequal))
		 do (return tem)))))

(defvar *maximum-size-for-m-prepared* 1)

(defun gm-prepared (poly &key  m (inequal 1) linear-variables &aux lins tem answ)
     (cond ((null linear-variables) (setq linear-variables
				      (degree-one-variables poly))))
  (cond (m
	 (cond ((atom poly ) nil)
	       ((eql 1 m)(cond ((setq tem (gone-prepared poly :inequal inequal  :linear-variables
							linear-variables))
			       (list tem))))
	       (t (loop for v in (list-variables poly)
			when
			  (member v linear-variables :test #'eq)
			  do (setq tem (zero-sublis poly v))
			     (cond ((setq answ (gm-prepared tem :m (1- m) :inequal inequal
							    :linear-variables linear-variables))
				    (return (cons v answ))))))))

	(t (setq lins (loop for v in linear-variables
			    when (eq (pdegree poly v) 1)
			      count 1))
	   (loop for i from 1 to (min lins  *maximum-size-for-m-prepared* )
		 when (setq answ (gm-prepared poly :m i :inequal inequal :linear-variables
					      linear-variables))
		   do (return answ)))))


(defun m-prepared (poly &key m linear-variables &aux  tem answ)
  (cond ((null linear-variables) (setq linear-variables
				       (degree-one-variables poly))))
  (cond (m
	 (cond ((atom poly ) nil)
	       ((zerop (constant-term poly)) nil)
	       ((eq 1 m)(cond ((setq tem (one-prepared poly))
			       (list tem))))
	       (t (loop for v in linear-variables
			do (setq tem  (zero-sublis poly v))
			(cond ((setq answ (m-prepared tem :m (1- m)))
			       (return (cons v answ))))))))
	(t
	 (loop for i from 1 to (length linear-variables)
	       when (setq answ (m-prepared poly :m i))
	       do (return answ)))))

(defun non-constant-factors (poly &optional invert &aux tem (genvar *genvar*))
  (setq tem (npfactor poly))
  (loop for (v deg) on tem by #'cddr
     when (and (not (numberp v))
	       (or (null invert)(null (may-invertp v invert))))
     collecting v
     and
     collecting deg))

(defvar *factored-list* nil)
(defvar *all-factors* nil)

(defun gen-ptimes (&rest l)
  (cond ((null l) 1)
	((eq (length l) 1) (car l))
	(t (ptimes (car l) (apply 'gen-ptimes (cdr l))))))


(defun find-good-dichotomy (ldata &key ( open-g 1) &aux tem list-factors answ
			    (list-polys (ldata-eqns ldata))
			    (gg (ldata-inequality ldata)))
  (setq gg (nplcm gg open-g))
  (cond ((member nil list-polys :test #'eq) (break 'here)))
  (setq list-factors
	(loop for v in list-polys
	      ;;only collect non trivial factors
	      when (> (length  (setq tem      (non-constant-factors v gg)))
		      2)
	      collecting  tem into multiple-factors
	      ;;when have a constant factor
	      when (null tem)
	      do (show tem) (setf (ldata-eqns ldata) '(1))
	      (setf (ldata-usedup ldata ) 1)
	      (return 'done-unit-ideal)
	      collecting tem into all-factors
	      collecting
	      (apply 'gen-ptimes (loop for (fac deg) on tem by #'cddr
;				       when (not (may-invertp fac gg))
				       collecting fac))
	      into terms

	      finally
	      (cond ((not (member 1 terms))
		     (setf (ldata-eqns ldata) terms))
		    (t (setf (ldata-usedup ldata) 1)
		       (setf (ldata-eqns ldata) '(1))))
	      (setq *all-factors* all-factors)
	      (return multiple-factors)))

  (cond ((member '(nil 1) *all-factors* :test #'equal) (break 'hii)))
  (cond
    ((eq list-factors 'done-unit-ideal) nil)
    (t
     (setq *factored-list* list-factors)
     (setq list-factors
	   (sort list-factors #'(lambda (u v)(cond ((atom u) t)
						   ((atom v) nil)
						   (t (< (length u) (length v)))))))
     (cond
       ((setq answ
	      ;; grabs the shortest product a^2*b^3 where a,b are variables
	      (loop for v in list-factors
		    when (loop for (fac deg) on v by #'cddr
			       when (not (or (atom fac)
					     (< (length fac) 4)))
			       do
			       (return nil)
			       finally (return t))
		    do (return v))))

       ((setq answ (loop named kay
			 for i from 1 to (length (list-variables list-polys))
			 do
			 (loop for v in list-factors
			       when (loop for (fac deg) on v by #'cddr
					 when (not (or (atom fac)
						       (< (length fac) 4)
						       (gm-prepared fac :m i :inequal gg)))
					 do
					 (return nil)
					 finally (return t))
			       do (return-from kay v)))))
       ;;grab the shortest one with a variable factor
       ((setq answ
	      (loop named pat
		    for
		    v in list-factors
		    do (loop for (fac deg) on v by #'cddr
			     when  (< (length fac ) 4)
			     do (return-from pat v)))))
       ((setq answ
	      (loop named sue
		    for
		    v in list-factors
		    do (loop for (fac deg) on v by #'cddr
			     when  (or (one-prepared fac)
				       (any-linearp fac
						    (ldata-inequality ldata)))
			     do (return-from sue v))))))
     answ)))

(defun order-dichotomy ( dich  &aux mult)
  (setq dich (loop for (v deg) on dich by #'cddr
		   collecting v))
  (setq mult
	(loop for v in dich
	      collecting
	      (loop for u in *all-factors*
		    when (member v u :test #'equal)
		    count 1 )))
  (setq dich
	(loop for i downfrom (length *all-factors*) to 0
	      when  (member i mult)
	      appending
	      (loop for v in mult
		    for u in dich
		    when (eq v i)
		    collecting u))))



;;LDATA : ((F1 F2 F3 ..) G)
;;LDATA MEANS HAVE FI=0 AND G NOT ZERO
;;SIMPLIFICATION STRATEGY FOR A LIST OF LDATUM
;;IF U^2+5*G*X IS IN THE LIST OF EQUATIONS ELIMINATE X FROM ALL OF THE EQUATIONS
;;(EXCEPT ITSELF)
;;IF 5+U^2*Y*X APPEARS SOLVE FOR X ELIMINATING IT FROM THE OTHERS AND SAVING THAT
;;EQUATION BUT MODIFYING THE INEQUALITY BY  G:LCM (G,U^2*Y)
;;MUST ELIMINATE FACTORS OF G FROM THE Fi
;;If 5+u^2*x+v^2*y occurs in the fi we create two ldata in place of the one ldata
;;   must solve for x=.. and add u^2 to the G making one ldata
;;   must solve for y=.. and add v^2 to the G making another ldata
;;Also if 5+u*v is an fi can set G: (lcm(g, u*v) and can eliminate u
;;from all equations
;;
;
;(defun dichotomy (list-eqns inequality)
;  (setq dich (find-good-dichotomy list-eqns))
;  (setq dich  (order-dichotomy dich))
;  (loop for v in dich
;;list of simplification types and their tests: where g is the inequal
;;and f is the equation to use in the replacement.
;;;   f                  test            replacement of h
;;1 u^2+5*g*x  (poly-linearp f x g)         (gen-prem h f x)
;;2 g1+u*x     (gone-prepared f g) (progn (setq g (lcm g u) ) (gen-prem h f x))
;;3 u1+u2*x+g*x^2  (invertible-leading-coefficient f x g)   (gen-prem h f x))

(defun any-linearp (f g &key variables-to-exclude among-variables)
  (cond ((null among-variables)(setq among-variables (list-variables f))))
  (loop for v in among-variables

	when (and (not (member v variables-to-exclude :test #'eq)) (poly-linearp f v g))
	do (return v)))

(defvar *clear-above* nil)

(defun any-invertible-leading-coefficient
       (f g &aux deg (varl (list-variables f)))
  (loop for v in varl
	 when  (may-invertp (list v 1 1) g)
	 do (setq varl (delete v varl :test #'equal)))
  (loop for v in varl
	 when (and (or (null *clear-above*) (not (<= (loop for w in *clear-above*
							    when (eq (car w) v)
							    minimize (cdr w))
						     (setq deg (pdegree f v)))))
		   (may-invertp (pcoeff f (list v deg 1)) g))
	 do (return v)))

(defun invertible-leading-coefficient (poly var inequal)
  (may-invertp (pcoeff poly (list var (pdegree poly var) 1)) inequal))

(defun plength-order (u v)
  (cond ((atom u) t)
	((atom v) nil)
	(t (< (length u) (length v)))))

;(defun replace-functions (list-to-replace f var &aux tem)
;  (loop for h in list-to-replace
;	when (not (eql f h))
;	when (not (pzerop (setq tem
;			    (square-free (gen-prem h f var)))))
;	collecting
;	tem))
;
;(defun replace-functions (list-to-replace f var &AUX original &aux tem)
;  (loop for h in list-to-replace
;	when (not (eql f h))
;	when (not (pzerop (setq tem
;				(gen-prem h f var))))
;	when (not (eq h tem))
;	collecting (square-free tem)
;	else collecting h))

(defun replace-functions (list-to-replace f var &key  general-leading-cof (invertible-g 1) &aux c-reqd remaind tem)
  (cond ((null general-leading-cof)
	 (loop for h in list-to-replace
	    when (not (eql f h))
	    when (not (pzerop (setq tem
				    (gen-prem h f var))))
	    when (not (eq h tem))
	    collecting (square-free tem)
	    else collecting h))
	(t
	 (loop for h in list-to-replace
	    when (eq f h) do (setq h nil)
	    when h
	    do
	    (multiple-value
	     (remaind c-reqd)
	     (gen-prem h f var))
	    when  h
	    when (eq h remaind) collecting h
	    else collecting (square-free remaind)
	    ;;collect h if the c-reqd is not a unit.
	    when  h
	    when (not (may-invertp c-reqd invertible-g))
	    collecting h))))

(defun any-gm-prepared (poly gg)
  (gm-prepared poly :inequal gg))

(defvar *inside-simplify-svar-ldata* nil)
(defvar   *stop-simplify* nil)



(defun check-for-gm-prepared (list-eqns gg &aux tem)
  (loop for eqn in list-eqns
	when (and (not (any-linearp eqn gg))
		  (setq tem (gm-prepared eqn :inequal gg)))
	do (setq *stop-simplify* (list eqn (length tem)))
	(return 'done))
  *stop-simplify*)

;;if we have a gm-prepared poly then we can simplify the
;;ldata by moving to m+1 opens where we are able to eliminate
;;variables:
;;eg.if  x*u+g=0  is an equation in ldata then we can assume u is
;;generically nonzero so we can on the open set u not zero solve
;;for x=-g/u.  The u=0 locus does not meet the x*u+g=0 locus so as far
;;as calculation of components goes, we need not keep track.
;;If however we wish to make a change of coordinates and look at some
;;other data then we must cover ourselves with two open sets:
;;u not zero and x*u+g not zero.
;;criterion for f going in used-up should be that there is
;;no possibility of using f any more.  This will be the case
;;for example if there is a variable in f and no other polynomials in the ldata
;;have that variable occurring. This happens after a linear is used eg. Otherwise
;;there is the possibility of dividing one by the other or taking a resultant.

;;note the possibility of x1+x2+x3*x4 = 0 being an equation.  It
;;might be that the replacement of x1 in the other equations is preferable
;;to the replacement of x2 or vice versa.eg in the following x7 replacement is better
;;since it leads to splitting into two components.
;;2*X6*X7-2*X6^2+X4*X5
;;X7-X6-X1*X5

(defmacro check-containments (ldata lis-ldat &optional gg)
  `(cond (error-check-containments
	 (check-component-containment ,ldata ,lis-ldat ,gg))))
(Defun Ldata-Simplifications (ldata &key (open-g 1) error-check-containments
			      recursive-p
			      &aux tem *clear-above* simplif answ unused stop-simplify
			      used-up var (changed t)  orig-ldata)
  (declare (special *in-linear-dich*))
  (check-arg ldata (eq (car ldata) 'ldata) "not an ldata")
  (setq orig-ldata ldata)
  (setq ldata (copy-list ldata))

  (let ((fns (ldata-eqns ldata))
	(gg (ldata-inequality ldata)))
    (setq gg (nplcm open-g gg))
    ;;note that using this gg we may produce I1 with I ^P I1R[gg-1] but
    ;;unless I1 is prime we will not have I ^P I1
    (setq fns  (loop for f in fns
		     when (not (and (numberp f) (zerop f)))
		     do (setq tem (square-free (remove-common-factors f gg)))
			and
		     when (numberp tem) do (setf (ldata-usedup ldata) 1)
		     (return (setq fns '(1)))
		     else
		     collecting  tem))
    (setq used-up (subseq fns 0 (ldata-usedup ldata)))
    (setq fns (nthcdr (ldata-usedup ldata) fns))
    (setq fns  (sort (copy-list fns) 'plength-order))
    (setq vars (list-variables fns))
    (loop while changed
	  do (setq changed nil)
	  (loop named sue for test in '(any-linearp
					 any-invertible-leading-coefficient
;					 any-irreducible
					 )
		do

		(loop for f in fns
		      when (setq var (funcall test f gg))
		      do
;                      (cond ((eq test 'any-irreducible)
;
;			     (setq f var) (setq var (find-variable-with-simple-lc
;						      f 1 1))
;			     (sh f) (show  var)))
		      (setq fns
			    (replace-functions fns f var))
		      (setq used-up (replace-functions used-up f var))
		      (cond ((or (eq test 'any-linearp)
				 (variable-doesnt-occur var fns used-up))
			     (push f used-up)
			     )
			    ;;put f last so that the lower degree ones will come first.
			    (t (setq fns (nconc fns (list f)))))

		      (cond ((eq test 'any-invertible-leading-coefficient)
			     (push (cons var (pdegree f var)) *clear-above*)
			     (show *clear-above*)))

		      (setq changed t)
		      (return-from sue))))

    ;;if contain number 'done else try to fix leading cofs
    (check-containments orig-ldata (list(make-ldata :eqns (zl-UNION fns used-up)
						    :inequality (ldata-inequality
								 orig-ldata))))

    (cond ((loop for f in (setq unused  (zl-union (delete 0 fns)))
		 when (numberp f) do (setq used-up '(1) unused nil)
		(return 'done)))
	  (t
;	   nil))
	   (setq simplif
;		 unused)
		 (simp-lead unused :open-g gg))
	   (cond ((eq simplif 'try-dichotomy) nil)
		 (t (setq unused  (zl-UNION simplif))))))
    (setf (ldata-inequality ldata) gg)
    (setf (ldata-eqns ldata)
	  (setq fns  (append  used-up unused)))
    (setf (ldata-usedup ldata) (length used-up))
    ;;the following may make  *stop-simplify* and ask to refine.
    ;;this check is done in MAKE-DICHOTOMY (check-for-gm-prepared  unused open-g)
    (setq stop-simplify *stop-simplify*)
    (check-containments orig-ldata (list ldata) (ldata-inequality ldata))
    (cond ((null *stop-simplify*)  (setq answ (MAKE-DICHOTOMY ldata :open-g open-g)))
	  (t (setq answ (list ldata))))
    ;;should make this so it won't check for redundant ldata more than once.
    (cond ((> (length answ ) 1)
	   (setq answ (delete-redundant-ldata answ :gg open-g)))))

  (check-containments orig-ldata answ)
  (cond ((and (null *stop-simplify*)
	      ;;this makes the divide dichotomy only apply after no more prod. dichot.
	      (eq (length answ ) 1))
	 (setq answ
	       (divide-dichotomy (car answ) :open-g open-g))))
  (check-containments orig-ldata answ)
;  (cond ((not (equal (length answ) 1))  (mshow answ)))
  (cond ((and (null *stop-simplify*)
	      (eq (length answ ) 1))
	 (setq answ (try-factor-irreducible-ldata (car answ) open-g))))
   (cond ((and (null *stop-simplify*) (not recursive-p)

	  (let ((*in-linear-dich* t))
	    (setq answ
		  (loop for ld in answ
			appending (linear-dichotomy ld :open-g open-g )))))))
;   (cond ((and (null *stop-simplify*)(not (variable-boundp *in-linear-dich*))
;	       (not recursive-p)
;	       (eq (length answ ) 1))
;	  (let ((*in-linear-dich* t))
;	    (setq answ (linear-dichotomy (car answ) :open-g open-g )))))
;    (format t "~%Verifying the ~A component contain the original" (length answ))
;    (check-components-contain-original ldata answ)
  (check-containments orig-ldata answ)
;  (cond ((null recursive-p)
;	 (setq answ (loop for v in answ
;			  appending
;		       (jacobian-dichotomy ldata :open-g open-g)))))
  (setq answ (delete-redundant-ldata answ :gg open-g))
  (check-containments orig-ldata answ)
  answ)


(defun any-irreducible (f gg &aux fac)
  (setq fac (non-constant-factors f gg))
  (cond ((> (length fac) 2)nil)
	(t (car fac))))

(defun leading-cof (poly var &aux (deg (pdegree poly var)))
  (cond ((zerop deg) 0)
	(t (pcoeff poly (list var (pdegree poly var)  1)))))

(defun find-variable-with-simple-lc (f  fns gg)
  fns gg
  (let ((varl (list-variables f)))
    (loop for v in varl collecting (pcomplexity (leading-cof f v)) into tem
	  finally(return (nth  (find-position-in-list (apply 'min tem) tem)
			       varl)))))

(defun ldata-unused (ld)
  (nthcdr (ldata-usedup ld)(ldata-eqns ld)))
(defvar *refine-opens* t)

;;not sure about the *refine-opens* = nil mode working.
#+old
(defun MAKE-DICHOTOMY (ldata &key (open-g 1)&aux int-open-g all-facs stop-simplify  eqns-modv ld  answ gg dich tem lin-dich)
  "If stop-simplify is true then it only works if have linear dichotomy.  It returns
  a list of  ldata "
  (cond (*stop-simplify* (merror "how did *stop-simplify* get  here")))
  ;;(setq *stop-simplify* nil)
  (setq dich (find-good-dichotomy  ldata))
  (setq all-facs *all-factors*)
  (setq dich (order-dichotomy dich))
  (show dich)
  (setq gg (nplcm open-g (ldata-inequality ldata)))
  (cond ((null *refine-opens*) (setq int-open-g gg) )
	(t (setq int-open-g open-g)))
  (setq lin-dich
	(loop for v in dich when (not (any-linearp v gg)) do (return nil)
	      finally (return (and dich t))))
  (show lin-dich)
;; I think the gm-prepared business should all be done after.
;; this may not be true.  The simplifications from finding a gm-prepared
;; and performing the elimination of variables might be necessary.
  (cond ((null lin-dich)
	 (check-for-gm-prepared (ldata-eqns ldata) int-open-g)
	 (show *stop-simplify*)))
  (setq stop-simplify *stop-simplify*)
  (cond ((and (null *refine-opens*) *stop-simplify* )
	 (multiple-value-bind
	   (lds ggs) (ldata-refinement ldata (car *stop-simplify*)
				       (second *stop-simplify*) :inequality
				       int-open-g)
	   (setq *stop-simplify* nil)
	   ;;this does not take into account the fact we should be contracting
	   ;;the ideal back to the current open..
	   (setq answ (loop for ld in lds
			    for ggi in ggs
			    do (setq tem (ldata-simplifications
					ld
					:open-g (sftimes int-open-g ggi)
					:recursive-p t))
			    appending
			    (loop for v in tem do
				 (zl-copy-structure v ldata- open-inequality
						 (sftimes ggi (ldata-open-inequality
								v))))))))

;	 (setq answ (ldata-refinement ldata (car *stop-simplify*)
;				      (second *stop-simplify*) :inequality int-open-g))
;;	 (mshow answ)  (format t "**Is the refinement ")
;	 (setq *stop-simplify* nil)
;	 (setq answ (loop for v in answ
;					  appending (ldata-simplifications v :open-g open-g :recursive-p t)))

  ;;priority 1 Linear-dichotomy
  ;;         2 gm-prepared equation
  ;;         3 any dichotomy
  ;;proceed with dich if dich is linear or if found no gm-prepared
	((or lin-dich (null *stop-simplify*))
	 (cond (dich
		(loop for v in dich
		      with so-far = 1
		      appending
			(progn
			  (setq eqns-modv
				(loop for facs in all-facs
				      when (not (member v facs :test #'equal))
					collecting
					  (apply #'gen-ptimes (loop for ter in facs by #'cddr
								   when (not
									  (may-invertp
									    ter so-far))
								     collecting ter))))
			  (cond ((member nil eqns-modv :test #'eq) (merror "nil should not be here")))
			  (cond ((eq v nil) (merror "nil should not be here")))
			  (setq ld (make-ldata))
			  (setf (ldata-eqns ld) (cons v eqns-modv))
			  (setf (ldata-inequality ld)(nplcm gg so-far))
			  (setf so-far (nplcm so-far v))
			  (cond ((Null *stop-simplify*)
				 (LDATA-SIMPLIFICATIONS ld :open-g open-g
							:recursive-p t))
				(t (list  ld))))
			into list-of-ld
		      finally
			(cond ((null list-of-ld)
			       (setq answ (list (make-ldata :eqns '(1) :inequality 1))))
			      (t (setq answ list-of-ld)))))
	       (t (setq answ (list ldata))))))

  (cond ((null answ) (setq answ (list ldata))))
;;  (cond ((null answ) (setq answ (list (make-ldata :eqns '(1) :inequality 1)))))
;  (setq answ (delete-redundant-ldata answ)))
  answ)
;;not sure about the *refine-opens* = nil mode working.
(defun MAKE-DICHOTOMY (ldata &key (open-g 1)&aux int-open-g all-facs stop-simplify  eqns-modv ld  answ gg dich lin-dich)
  "If stop-simplify is true then it only works if have linear dichotomy.  It returns
  a list of  ldata "
  (cond (*stop-simplify* (merror "how did *stop-simplify* get  here")))
  ;;(setq *stop-simplify* nil)
  (setq dich (find-good-dichotomy  ldata))
  (setq all-facs *all-factors*)
  (setq dich (order-dichotomy dich))
  (show dich)
  (setq gg (nplcm open-g (ldata-inequality ldata)))
  (cond ((null *refine-opens*) (setq int-open-g gg) )
	(t (setq int-open-g open-g)))
  (setq lin-dich
	(loop for v in dich when (not (any-linearp v gg)) do (return nil)
	      finally (return (and dich t))))
  (show lin-dich)
;; I think the gm-prepared business should all be done after.
;; this may not be true.  The simplifications from finding a gm-prepared
;; and performing the elimination of variables might be necessary.
  (cond ((null lin-dich)
	 (check-for-gm-prepared (ldata-eqns ldata) int-open-g)
	 (show *stop-simplify*)))
  (setq stop-simplify *stop-simplify*)
  (cond ((and (null *refine-opens*) *stop-simplify* )
	 (multiple-value-bind
	   (lds ggs) (ldata-refinement ldata (car *stop-simplify*)
				       (second *stop-simplify*) :inequality
				       int-open-g)
	   (setq *stop-simplify* nil)
	   ;;this does not take into account the fact we should be contracting
	   ;;the ideal back to the current open..
	   (setq answ (loop for ld in lds
			    for ggi in ggs
			    appending (ldata-simplifications
					ld
					:open-g (sftimes int-open-g ggi)
					:recursive-p t))))

;	 (setq answ (ldata-refinement ldata (car *stop-simplify*)
;				      (second *stop-simplify*) :inequality int-open-g))
;;	 (mshow answ)  (format t "**Is the refinement ")
;	 (setq *stop-simplify* nil)
;	 (setq answ (loop for v in answ
;					  appending (ldata-simplifications v :open-g open-g :recursive-p t)))
	 )
  ;;priority 1 Linear-dichotomy
  ;;         2 gm-prepared equation
  ;;         3 any dichotomy
  ;;proceed with dich if dich is linear or if found no gm-prepared
	((or lin-dich (null *stop-simplify*))
	 (cond (dich
		(loop for v in dich
		      with so-far = 1
		      appending
			(progn
			  (setq eqns-modv
				(loop for facs in all-facs
				      when (not (member v facs :test #'equal))
					collecting
					  (apply 'gen-ptimes (loop for ter in facs by #'cddr
								   when (not
									  (may-invertp
									    ter so-far))
								     collecting ter))))
			  (cond ((member nil eqns-modv :test #'eq) (merror "nil should not be here")))
			  (cond ((eq v nil) (merror "nil should not be here")))
			  (setq ld (make-ldata))
			  (setf (ldata-eqns ld) (cons v eqns-modv))
			  (setf (ldata-inequality ld)(nplcm gg so-far))
			  (setf so-far (nplcm so-far v))
			  (cond ((Null *stop-simplify*)
				 (LDATA-SIMPLIFICATIONS ld :open-g open-g
							:recursive-p t))
				(t (list  ld))))
			into list-of-ld
		      do (mshow list-of-ld)
		      finally
			(cond ((null list-of-ld)
			       (setq answ (list (make-ldata :eqns '(1) :inequality 1))))
			      (t (setq answ list-of-ld)))))
	       (t (setq answ (list ldata))))))

  (cond ((null answ) (setq answ (list ldata))))
;;  (cond ((null answ) (setq answ (list (make-ldata :eqns '(1) :inequality 1)))))
;  (setq answ (delete-redundant-ldata answ)))
  answ)
(defun order-variables-by-occurence (list-eqns &aux var-lists all-vars)
  (setq var-lists
	(loop for v in list-eqns
	collecting (list-variables v) ))
 (setq all-vars (apply #'zl-UNION var-lists))
 (loop for v in all-vars
       collecting v
       collecting
       (loop for lis in var-lists
	     when (member v lis :test #'eq)
	     count 1 )))


;;there is an error in simp-lead: it produced some equations which did not
;;give ideal containment.
(defun simp-lead (eqns &key (open-g 1) variables &aux fac answ)
  (setq eqns (copy-list eqns))
  (show (length eqns))
  (setq answ (loop for v on eqns
	do
	(setq fac (non-constant-factors (car v) open-g))
	(cond ((and (numberp (car v)) (zerop (car v))) nil)
	      ((> (length fac ) 2)
	       (return 'try-dichotomy))
	      ((null fac)  (return '(1)))
	      (t (setf (car v) (car fac))
		 ))
	finally
	 (return (simp-lead1 eqns :open-g open-g :variables variables))))
    (show (or (symbolp answ) (length answ)))
  answ)

(defun test-simp-lead (eqns)
  (shl eqns)
  (let ((answ (simp-lead1 eqns)))
    (cond ((atom answ) (show answ))
	  (t (cond ((not (ideal-subsetp answ eqns))
		    (break 't1))
		   ((not (ideal-subsetp eqns answ))
		    (break 't2)))
    (format t "both ok")
    (shl answ) answ))))

(defun simp-lead1 (eqns &key (open-g 1) variables &aux  deg-vector cof tem
		  answ changed fac varl)
  (setq eqns (union-equal eqns))
    (cond (variables (setq varl variables ))
	  (t (setq varl (list-variables eqns))))
    (loop named sue for var in varl
	  for vari on varl
	  do (setq variables vari)
       (setq tem var)
	  (setq deg-vector (loop for u in eqns
				  collecting (pdegree u var)))
	  (loop for i in (sort (zl-union (delete 0 (copy-list  deg-vector))) 'alphalessp)
		do
		(loop for j in deg-vector
		      for f in eqns
		      when (eql j i)
		      do
		      (or (eq tem var) (error "bad"))
		      (setq cof (pcoeff f (list var j 1)))
		      (loop for ff in eqns
			    for jj in deg-vector
			    ;;ff=a*y^i+b f=c*y^j+d
			    ;;if j<=i  want to replace ff by
			    ;;ff-f*a/c*y^(i-j)  (i.e. c*ff-a*f*y^i-j )
			    ;;and this will be ok if
			    ;; (denom (a/c) is invertible )
			    when (and (>= jj j)(not (eql f ff)))
			    when (may-invertp (denom (setq fac
							   (ratreduce
							     (pcoeff ff (list var jj 1))
							     cof)))
					      open-g)
			    do (setq eqns (delete ff eqns :test #'equal))
;			    (format t "replacing for variable ~A .."var)
;			    (sh ff)
;			    (format t "using " ) (sh f)
			    (setq answ (pdifference
					 (ptimes (denom fac)
						 ff)
					 (gen-ptimes f (pexpt (list var 1 1)
							      (- jj j)) (num fac))))
;			    (format t "   by") (sh answ)

			    (cond ((not ($zerop answ))

				   (setq eqns (cons answ eqns))))
;			    (show (length eqns))
				   (setq changed t)

;			    (cond ((and changed (equal eqns orig))
;				   (merror "the equations did not change")))
			    (return-from sue 'start-over)))))
    (cond (changed (simp-lead1 eqns :open-g open-g :variables variables))
	  (t eqns)))


(defun affine-open (list-vars &optional (inequal 1) &aux coords fns)
  (cond ((atom (car list-vars))
	 (cond ((get  (car list-vars) 'disrep)
	 (setq fns (loop for v in list-vars
	       collecting (list v 1 1 ))))))
	((mbagp list-vars) (setq fns (st-rat list-vars)))
	(t (setq fns list-vars)))
   (setq coords (construct-rmap fns))
   (make-zopen :coord coords :inv coords :inequality inequal))
;  (make-zopen :coord (make-rmap  fns fns
;				denom 1)
;	      inv      (make-rmap  fns fns
;				denom 1)
;	      inequality (st-rat inequal)))
(defun construct-pre-ldata-sheaves (&key s-var data opens)
  (check-arg data (or (null data)(null (car data)) (eq (caaar data) 'ldata)) "list of lists of ldata")
  (check-arg s-var (or (null s-var) (eq (car s-var) 's-var)) "s-var")
  (cond (opens (setq s-var (make-s-var :zopens opens))))
  (make-pre-ldata-sheaves :s-var s-var :data data))

;;;old forms worked
;(defremember grobner-basis-remember (basis)
;  (setq *poly-simplifications* (grobner-basis basis)))
;(defun grobner-remember (basis)
;    (setq *poly-simplifications* (grobner-basis-remember basis)))


;;to try to implemememnt not checking twice and timing out.
(defremember grobner-basis-remember (basis &aux tem answ)
  (setq   answ (catch 'took-too-long
		(setq tem  (grobner-basis basis))))
  (cond (tem tem)
	(t (list 'took-too-long *timed-grobner-basis* *poly-simplifications*))))

(defun grobner-remember (basis &aux simps)
  (setq  simps (grobner-basis-remember basis))
  (cond ((eq (car simps ) 'took-too-long)
	 (cond ((eq (second simps) *timed-grobner-basis*)
		(setq *poly-simplifications* (third simps))(throw 'took-too-long 'took-too-long))
	       (t (setf (gethash  (get 'grobner-basis-remember :memory-table) basis) nil)
		  (grobner-remember basis))))
	(t (setq *poly-simplifications* (grobner-basis-remember basis)))))

;(defun delete-redundant-ldata (list-ld)
;  (loop for v in list-ld
;	do
;	(loop for w in (ldata-eqns v)
;	      when (numberp w)
;	      do (setq list-ld (delete v list-ld))))
;  (cond ((>= (length list-ld) 2)
;	 (loop for v on list-ld
;	       do (grobner-basis (ldata-eqns (first  v)))
;	       (loop for u in list-ld
;		     when(and (not (eql u (first v)))
;			      (ideal-subsetp (ldata-eqns u)
;					 (ldata-eqns (first v))
;					 :reset-basis nil :verbose nil))
;		     do (format t "~%deleting redundant component..")
;		     (setq list-ld (delete (first v) list-ld))
;		     (return t)))))
;
;  list-ld)

(defun variety-ldata-subset (ld1 ld2 &key (open-g 1) ignore-ldata-inequalities)
  (cond (ignore-ldata-inequalities
	(multiple-value-bind (cont unit)
	  (grobner-subset (ldata-eqns ld2) (ldata-eqns ld1) open-g)
	  (values cont (and unit 'empty))))
	(t
  (let ((gg (nplcm open-g (ldata-inequality ld1))))
    (and (unit-idealp (cons (ldata-inequality ld2) (ldata-eqns ld1) )
		      gg)
	 ;;return only one value since we don't want to test if ld1 is empty on open-g
	 (values(grobner-subset (ldata-eqns ld2) (ldata-eqns ld1)
			 (nplcm gg (ldata-inequality ld2)))))))))


;;;version 4.0 of delete has check of
;; V(I1,c1)^PV(I2,c2) where I1,c1 has inequality
;;if I2^PI1[c1^-1,c2^-1] and <c2,I1>[c1^-1] = <1>
;;ie if where c1 and c2 nonzero get containment, and also insist that V(I1,c1) does
;;not meet the c2=0 set.

;(defun delete-redundant-ldata (list-ld &key     (gg 1)
;			       &aux cc cint use-inverse (complexity-for-inverse 50))
;  (loop for v in list-ld
;	do
;	(loop for w in (ldata-eqns v)
;	      when (numberp w)
;	      do (setq list-ld (delete v list-ld))))
;  (cond ((>= (length list-ld) 2)
;	 (loop for v on list-ld
;	       do
;	       (setq cc (nplcm gg  (ldata-inequality (first v))))
;	       (loop for u in list-ld
;			when (not (eql u  (first v)))
;			do
;			(setq cint (nplcm cc (ldata-inequality u)))
;
;			(multiple-value-bind (cont unit)
;			    (grobner-subset (ldata-eqns u)
;					    (ldata-eqns (first  v))
;					    cint)
;			  (cond ((and (or unit
;				     cont) (unit-idealp (cons (ldata-inequality u)
;							      (ldata-eqns (first v)))
;							cc))
;				 (setq list-ld (delete (first v) list-ld))
;				 (format t "~%Deleting reduntant component")
;				 (des (first v))
;				 (format t "~%Because of ")
;				 (show (list unit cont))
;				 (cond ((null unit)
;					(des u)))
;				 (return t))))))))
;  list-ld)
;
;;;putting use-inverse = t can be dangerous I am moving it to the aux
;;;this version is not correct but involves less calculation.
;;;maybe its ok if the resultant things are prime.
;
;(defun delete-redundant-ldata (list-ld &key     (gg 1)
;			       &aux use-inverse (complexity-for-inverse 50))
;  (loop for v in list-ld
;	do
;	(loop for w in (ldata-eqns v)
;	      when (numberp w)
;	      do (setq list-ld (delete v list-ld))))
;  (cond ((>= (length list-ld) 2)
;	 (loop for v on list-ld
;	       do (loop for u in list-ld
;			when (not (eql u  (first v)))
;			do
;			(cond (use-inverse (setq gg
;						 (nplcm gg (ldata-inequality (first v))))))
;;			(cond ( (> (pcomplexity gg) complexity-for-inverse)
;;
;;			       (format t "%Not using inverse for deletion :.."
;;				       (des gg))
;;							       (setq gg 1)))
;			(multiple-value-bind (cont unit)
;			    (grobner-subset (ldata-eqns u)
;					    (ldata-eqns (first  v))
;					    gg)
;			  (cond ((or unit
;				     cont)
;				 (setq list-ld (delete (first v) list-ld))
;				 (format t "~%Deleting reduntant component")
;				 (des (first v))
;				 (format t "~%Because of ")
;				 (show (list unit cont))
;				 (cond ((null unit)
;					(des u)))
;				 (return t))))))))
;  list-ld)


;;timed
(defun delete-redundant-ldata (list-ld &key (open-g 1)
			       (gg 1)	ignore-ldata-inequalities
			       &aux redundant )
  (setq gg (sftimes open-g gg))
  (setq list-ld (loop for v in list-ld
	when (not (unit-idealp (ldata-eqns v) (nplcm (ldata-inequality v) gg)))
	collecting v
	else do (format t "~2%Deleting the empty ldata ")
	(fsh v)))
  (loop for v in list-ld
	for vi from 0
	do
	(loop for w in list-ld
	      for wi from 0
	      when (not (or (member vi redundant :test #'equal)
			    (member wi redundant :test #'equal)
			    (eql vi wi)))
	      when
	      (variety-ldata-subset v w :open-g gg :ignore-ldata-inequalities
				    ignore-ldata-inequalities)
	      do (push vi redundant)
	      (format t "~2%Deleting redundant component")
	      (fsh v)
	      (format t "~%contained in ") (fsh w)))
  (loop for v in list-ld
	for vi from 0
	when (not (member vi redundant :test #'equal))
	collecting v))

(defun describe-ldata (ld)
  (cond ((eq (first ld) 'ldata)
	 (format t "~%The ~A  equations are .." (length (ldata-eqns ld)))
	 (shl (ldata-eqns ld))
	 (format t "~%The inequality for the component calculation was ..")
	 (sh (ldata-inequality ld)))
	(t (mapcar 'describe-ldata ld))))
;
; (setq mons
;      (loop for v in (cdr mat)
;	    collecting
;	    (loop for u in '($x1 $x2 $x3 $x4 $x5)
;		  for w in (cdr v)
;		  with answ = 1
;		  do (setq tem (st-rat u))
;		  (setq answ (ptimes answ (pexpt tem w)))
;		  finally (return answ))))

;;will return a list of ldata

(defun $add_to_solution_tree (a-list &aux tem tem1)
  (check-arg a-list $listp "macsymya list")
  ;;check this case had not been done yet:
  (loop named pat for u in (cdr a-list)
	do
	(check-arg u $listp "macsyma list")
	(setq tem (from-case  u))
	(loop  for v in $solution_tree
	      when (equal (from-case a-list) tem)
	      do (cond ((yes-or-no-p "Remove from $solution_tree case ~A" tem)
			(loop for v in $solution_tree
			      when (not (and (initial-equal
					       (setq tem1(from-case v)) tem)
					     (>= (length tem1)  tem)))
			      collecting v into tempp
			      finally (setq $solution_tree tempp)
			      (return-from pat  'fixed))))))
  (setq $solution_tree
	(cons '(mlist) (append (cdr a-list) (cdr $solution_tree))))
  ($sort_solution_tree))


(defun $simplify_relative (system poly)
  ($grobner_basis (list '(mlist) poly))
   (append  (delete 0 ($totaldisrep ($polysimp system))) (list poly)))


(defun $list_irreducible_factors(poly &aux answer)
  (cond
    ((atom poly ) (list '(mlist) poly))
    ((MBAGP poly) (CONS (CAR poly) (MAPCAR #'$list_irreducible_factors (CDR poly))))
    ((eq (caar poly) 'mtimes)
     (cond ((member 'factored (car poly) :test #'eq)
	    (loop for u in (cdr poly)
		  do
		  (cond ((atom u)
			 (push u answer))
			((eq (caar u) 'mplus)
			 (push u answer))
			((eq (caar u) 'mexpt)
			 (push (second u) answer )))
		  finally (return ($sort (cons '(mlist) (zl-UNION  answer))))))
	   (t ($list_irreducible_factors ($factor poly)))))
    ((eq (caar poly) 'mexpt) ($list_irreducible_factors ($factor (second poly))))
    ((eq (caar poly) 'mplus)
     (cond ((member 'irreducible (car poly) :test #'eq) (list '(mlist) poly))
				   (t ($list_irreducible_factors ($factor poly)))))
    (t (merror "how did you get here"))))


(defun $show_solution_tree (&optional full-format &aux tem)
  (loop for v in (cdr $solution_tree)
	do
	(setq tem (from-case v))
	(case (length tem)
	  (0 (format t "~%Cases for the solution tree")
	  (cond (full-format  (i-$grind  v))))
	  (1 (format t "~%  Case: ~A"(string-grind (car tem)))
		  (cond (full-format  (i-$grind v))))
	  (2 (format t "~%    Subcase ~A"(string-grind (second tem))))
	  (3 (format t "~%       Subsubcase ~A") (string-grind (third tem)))
	  (t (format t "~%         Sub......~A" ) (string-grind (nthcdr 3 tem)))))
  '$done)

(defun $eliminate_nonzero_factors (system &rest l)
  "rather crude see cancel below"
  (cond ($nonzero_factors
	 (setq l (append l (cdr $nonzero_factors)))))
  (cond (L
  (setq system (div* system (power (apply 'mul*  l) 10)))
     ($numerator system))
	(t system)))


(defun $cancel_factors_and_denominators (variety factors-to-elim
					 &optional (homogeneous nil)
					 &aux system rat-variety type)
  "tries to return same type"
  (cond ((atom factors-to-elim) (setq factors-to-elim (list factors-to-elim)))
	(($listp factors-to-elim) (setq factors-to-elim (cdr factors-to-elim)))
	(t factors-to-elim))
  (check-arg variety  $listp  "macsyma list")
  (setq type (cond ((polynomialp (second variety)) 'polynomial)
		   ((rational-functionp (second variety)) 'rational-function)
		   (($ratp (second variety)) '$rat)
		   (t 'general)))
  (setq rat-variety
	(loop for v in (cdr variety)
	       collecting (new-rat v)))
  (setq system rat-variety)
  (cond (homogeneous
	 (loop for w in rat-variety
		with tem = 1
		do (setq tem (nplcm tem (denom w)))
		finally
		(setq tem (cons tem 1))
		(setq system
		      (loop for w in system
			     collecting
			     (num (rattimes tem
					    w t))))))
	(t (setq system
		 (loop for w in rat-variety
			collecting (num w)))))
  (show factors-to-elim)
  (loop for v in factors-to-elim
	 with tem
	 do
	 (setq tem   (pexpt (st-rat v) 7))
	 (cond (homogeneous
		(loop for w in system
		       do (setq tem (npgcd tem w))
		       (cond ((numberp tem) (return 'done)))
		       finally
		       (setq system
			     (loop for w in system
				    collecting
				    (pquotient w tem)))))
	       (t (setq system
			(loop for w in system
			       collecting
			       (num (ratreduce w tem)))))))
  (case type
    (general
     (loop for w in system
	    collecting
	    (new-disrep w)
	    into tem
	    finally (return (cons '(mlist) tem))))
    ($rat (cons '(mlist) (mapcar 'header-poly system)))
    (t system)))

(defun $blowup_chart (chart variety &optional homogeneous
		      &aux divisor transform)
  "Performs the substitutions of chart and cancels the exceptional divisor
 and gets rid of denominators.  It does this homogeneously if the value
 of homogeneous is not nil."

  (check-arg chart $listp "macsyma list")
  (check-arg variety $listp "macsyma list")
  (loop for w in (cdr chart)
	with tem = (third (second chart))
	do
	(show tem )
	(setq tem ($gcd tem (third w)))
	finally (setq divisor tem)
	(format t "~%The exceptional divisor is ~A" tem))
  (setq transform  ($sublis chart variety))
  ($cancel_factors_and_denominators transform (list '(mlist)
						divisor)
				    homogeneous))

;;;want this to blowup where the locus is given by a complete intersection.
;(defun $blowup (variety locus-to-blowup new-var-prefix)
;  (check-arg $listp locus-to-blowup "macsyma list")
;  (loop for u in (cdr locus-to-blowup)
;	collecting))

(defun my-minor (mat  rows-to-take cols-to-take)
  ($determinant ($sub_matrix mat rows-to-take cols-to-take)))

(defun $wedge_matrix (mat n)
 (let ((tabl (list-tableaux n ($length (second mat)) )))
   (show tabl)
   (loop for u in tabl
	collecting
	(loop for v in tabl
	      collecting (my-minor mat u v) into tem
	      finally (return (cons '(mlist) tem)))
	into temm
	finally (return (cons '($matrix) temm)))))

(defun $sublis_and_add (eqns expr &rest  elimin &aux answer)
 (setq answer ($sublis eqns expr))
 (show elimin)
 (setq answer (append answer (mapcar 'bring-to-left-side (cdr  eqns ))))
 (setq answer ($factor (apply '$eliminate_nonzero_factors
				       answer elimin))))

;(defun pcomplexity  (poly)
;  (cond ((atom poly) 0)
;	(t (loop for (deg cof) on (cdr poly) by #'cddr
;		 summing (+ deg (pcomplexity cof))))))

;(defun $remove_linears (eqns &aux (best t)used-eqns pcompw rat-eqns tem)
;  (setq rat-eqns
;  (loop for v in (cdr eqns) until (eq v '$case)
;	collecting (st-rat v) ))
;  (loop while best
;	do
;	(loop for w in rat-eqns
;	      initially (setq best   nil)
;	      with prev-compl = 4000000
;	      when (setq tem (coll-linear w))
;	      do
;	      (show tem)
;	      (cond ((< (setq pcompw (pcomplexity w)) prev-compl)
;		     (setq prev-compl pcompw ) (setq best (cons (car tem) w))))
;	      finally (cond (best
;			     (format t "~%Substituting for ~A by using"
;				     (get (car best ) 'disrep))
;			     (sh (cdr best))
;			     (loop for v in rat-eqns
;				   collecting
;				   (psublis (list(cons (car best)
;						       (pcoeff (cdr best) 1
;							       (list (car best)))))
;					    (pcoeff (cdr best) (car best))
;					    (cdr best))
;				   into new-eqns
;				   finally (setq rat-eqns new-eqns)))))
;	(push (cdr best) used-eqns) )
;  (cond (used-eqns
;	 (cons '(mlist) (append (loop for v in rat-eqns
;		       collecting (header-poly v))
;		 (member '$case eqns :test #'eq)
;		 (loop for v in used-eqns
;		       collecting (header-poly v)))))))


(defun $remove_Linears (eqns  &aux subs lins)
  (loop for v in (cdr eqns)
	when (setq lins (cdr  ($coll_linear eqns)))
	appending lins into lin-vars
	and
	collecting v into lin-eqns
	finally (cond (lins
		       (setq subs
			     ($fast_linsolve (cons '(mlist) lin-eqns)
					     (cons '(mlist)
						   (zl-UNION lin-vars))))
		       (format t "~%Eliminating linear variables ~A" lin-vars)
		       ))
	(cond (subs  (setq eqns ($sublis subs eqns))
		     (setq eqns (append (delete 0 ($ratsimp eqns))
					(loop for v in subs collecting
					      ($numerator
						(sub* (second v) (third v)))))))
	      (t eqns))))


;
;(defmacro te (x v)
;  (cond ((functionp x) `(,x ,v))
;	(t 5)))

(defmacro find-minimal (in-list ordering &optional   such-that ind   )
  (cond (such-that
	 (cond ((functionp such-that)(setq ind '-ind-)
		(setq such-that `(,such-that -ind-)))
	       (t
	 (check-arg ind (not (null ind)) "non nil.  Must specify index")))
	 ` (loop for ,ind in ,in-list
		 with prev-min
		 when  ,such-that
		 do (cond (prev-min
			   (cond ((funcall ,ordering ,ind prev-min)
				  (setq prev-min ,ind))))
			  (t (setq prev-min ,ind)))
		 finally (return prev-min)))
	(t   `(loop for v in ,in-list
		    with prev-min
		    do (cond (prev-min
			      (cond ((funcall , ordering v prev-min)
				     (setq prev-min v))))
			     (t (setq prev-min v)))
		    finally (return prev-min)))))


;(defun find-minimal (in-list &optional  such-that ordering  &aux  prev-min)
;    (cond (such-that
;	 (loop for v in in-list
;	       when (funcall such-that v)
;	       do (cond (prev-min
;			 (cond ((funcall ordering v prev-min)
;				(setq prev-min v))))
;			(t (setq prev-min v)))))
;	(t  (loop for v in in-list
;			  do (cond (prev-min
;			    (cond ((funcall ordering v prev-min)
;				   (setq prev-min v))))
;			   (t (setq prev-min v))))))
;
;  prev-min)

;(defun find-good-variable-order (ideal)
;  (setq vars (mapcar '$list_variables (cdr ideal)))
; (setq vars  (mapcar 'cdr vars))
; (setq all-vars (union (apply 'append vars)))
; (setq f #'(lambda (v) (loop for w in vars
;			     when (member v :test #'eq) vars
;			     count1
; (sort all-vars #'(lambda (u v)
;
; (setq vars (sort vars #'(lambda( u v)(< (length u) (length v)))))
; (setq all-vars (union (apply 'append  vars))))

(defun find-good-variable-order (ideal &aux f all-vars vars)
  (declare (special  f vars))
  (setq vars (mapcar '$list_variables (cdr ideal)))
  (setq vars  (mapcar 'cdr vars))
   (setq all-vars (zl-union (apply 'append  vars)))
  (setq f #'(lambda (v) (loop for w in vars when (member v vars) count 1 into tem
			      finally (return tem))))
   (sort all-vars #'(lambda( u v)(< (funcall f u) (funcall f v)))))

(defun may-invertp (poly invertible-poly)
  (if ($zerop poly)
      nil
      (numberp (denom (ratreduce invertible-poly (square-free poly))))))

(defvar $char_set nil)
(defvar $ideal nil)

(defun poly-linearp (poly  var may-invert &aux cof)
  (cond ((numberp poly ) nil)
	((eq (pdegree poly var) 1)
	 (setq cof (pcoeff poly (list var 1 1)))
	 (cond ((atom cof) cof)
	       ((may-invertp cof may-invert) cof)
	       (t nil)))))

;(defun $te (eqns may-invert &aux ans)
;  (setq ans (remove-linears (mapcar 'st-rat (cdr eqns) ) (st-rat may-invert)))
;  (cons '(mlist) (mapcar 'new-disrep ans)))

;;;this seems the most efficient.  If may-invert is x^2-1 then u*(x+1)+h(w,v)
;;;will cause the replacement u--> -h(w,v)/(x+1) in the remaining eqns.
;;;then it calls itself on the remaining equations.
(defun remove-linears (eqns &optional (may-invert 1) &aux sub cof)
  (check-arg eqns (polynomialp (car eqns)) "first term not  polynomial")
  (loop for v in eqns do
       (block sue
	 (loop for vari in (list-variables v)
	    when (setq cof (poly-linearp v  vari may-invert))
	    do (setq sub (cons vari (pdifference (ptimes (list vari 1 1) cof) v))
		     denom cof)
	      (loop for uu in eqns
		 when (not (equal uu v))
		 collecting (psublis (list sub) denom uu) into new-eqn
		 finally (return-from sue (cons (num (ratreduce v cof))
						(remove-linears new-eqn	may-invert))))))
     finally (return eqns)))

(defvar *to-invert* nil)

(defun $ritt_set (ideal-generators &optional (may-invert 1) &aux rat-ideal tem id)
  "makes $char_set from an ideal-generators pushing the leading coefs into *to-invert*"
  (setq $ideal (setq id (make-ideal)))

  (setf (ideal-variable-correspondence id)
	(order-variables (find-good-variable-order ideal-generators)))
     (ideal-variable-correspondence id)
    (setq rat-ideal
	  (loop for u in (cdr ideal-generators)
		with tem
		when (not (pzerop (setq tem (st-rat u))))
		collecting tem ))
    (setq may-invert (st-rat may-invert))
    (setq rat-ideal (remove-linears rat-ideal may-invert))
    (setq vars (reverse (sort  (list-variables rat-ideal)'pointergp)))
    (show vars)
    (show (car rat-ideal))
    (setq *to-invert* (list may-invert))
    (setq rat-ideal
	  (loop for v in rat-ideal
		collecting (remove-common-factors v may-invert)))
    (setq $char_set
	  (loop for v in vars
		with temm
		when (setq temm (find-minimal
				 rat-ideal
			    #'(lambda (u v)(< (p-le u) (p-le v)))
				 (and (listp pol) (eq (p-var pol) v)) pol))

		do (show temm); (sh v)
		and
		collecting  temm))
    (loop for v in rat-ideal
	  do
	  (setq tem (ritt-reduce v $char_set))
	  (mshow tem)
	  (show (length $char_set))
	  (cond ((pzerop tem) nil)
		(t (setq $char_set (add-to-chain tem $char_set)))))
    (setq $char_set (eliminate-factors $char_set *to-invert*))
    (setf (ideal-char-set id) $char_set)
    (setf (ideal-localization  id) *to-invert*)
    ($disrep_ideal id))

(defun eliminate-factors (good factors-to-elim )
  (loop for v in factors-to-elim
	with mon
	do (setq mon (pexpt v 5))
	(setq good
	(loop for w in good
	      collecting (num (ratreduce w mon)))))
  good)

(defun header-fake (expr)
  (cond ((or (numberp expr)(get  (car expr) 'disrep))
	 (cons '(mrat nil nil) (cons expr 1)))
	((and (or (numberp (car expr))( get (caar expr) 'disrep))
	      (or (numberp (cdr expr)) (get (cadr expr) 'disrep)))
	 (cons '(mrat nil nil) expr))
	(t (merror "not a rat'l fun or poly (at least no disrep prop)"))))
(defun fake-disrep (pol)
   ($totaldisrep (header-fake pol)))

(defun $disrep_ideal (ideal)
  (list '(mlist)
	(cons '(mlist)(mapcar 'fake-disrep (ideal-char-set ideal)))
	(cons '(mlist)(mapcar 'fake-disrep (ideal-localization ideal)))))

;(defun ideal-disrep item ideal)
;  (loop with added = t
;	while added
;	do (setq added nil)
;	(loop for v in rat-ideal
;	      do (show (length $char_set))
;	       (setq tem (ritt-reduce v $char_set))
;	      when (not (pzerop tem))
;	      do
;	      (mshow tem)
;	      (setq $char_set (add-to-chain tem $char_set))
;	      (show (length $char_set))
;	      else do (setq rat-ideal (delete v rat-ideal)) (format t "its zero")
;	      finally (setq rat-ideal nil)))
;  $char_set)

(defun square-free (p)
  (let ((facts (psqfr p)))
    (if (cddr facts)
	(loop for v in (cddr facts) by #'cddr
	   with answer = (car facts) do
	     (setq answer (ptimes v answer))
	   finally (return answer))
	(car facts))))

(defun $square_free_numerators (expr)
  (cond ((atom expr) expr)
	((polynomialp expr)(square-free expr))
	((rational-functionp expr) (square-free (num expr)))
	(($ratp expr) (cons (car expr) (cons (square-free (num (cdr expr)))
					     1)))
	((mbagp expr) (cons (car expr)
			    (mapcar '$square_free_numerators (cdr expr))))
	(t (let ((tem (new-rat expr)))

	     (header-poly (cons (square-free (num tem)) 1))))))



(defun add-to-chain (poly chain)
  (setq poly (square-free poly))
  (cond ((eq poly 0) chain)
	(t  (loop for v in chain
		       when (eq (p-var poly)(p-var v))
		       do (cond ((< (p-le poly) (p-le v))(show 'deleting)
				 (setq chain (delete v chain :test #'equal)))
				((>= (p-le poly)(p-le v))
				 (mshow poly v) (merror "not reduced")))
		       finally(format t "~%adding .." )
		       (sh poly)
		       (setq chain
			     (sort (cons poly chain)
					 #'(lambda (u v)
					     (pointergp (p-var u) (p-var v)))))
		       (return chain)))))


(defun order-variables (list-of-vars &aux vc)
  (cond (($listp list-of-vars) (setq list-of-vars (cdr list-of-vars))))
  (setq vc (make-variable-correspondence))
  (loop for v in list-of-vars
	for i from 1
	with w
	collecting (setq w (gensym)) into gen
	do
	(setf (get w 'disrep) v)
	(setf (symbol-value w) i)
	finally (setf (vc-genvar vc) gen)
	(setf (vc-varlist vc) (copy-list list-of-vars)))
  vc)

(defmacro with-vc (var-corr &rest body)
 `(let ((*genvar* (vc-genvar ,var-corr))
	(*varlist* (vc-varlist ,var-corr)))
    (unwind-protect
      (progn ,@ body)
      (setf (vc-genvar ,var-corr) *genvar*)
      (setf (vc-varlist ,var-corr ) *varlist*))))

;(defun sh (f)
;  (cond ($display2d (displa (header-poly f)))
;	(t (string-grind (header-poly f) :stream t))))
;(defun shl (l) (mapcar 'sh l))
;(defun shl (l)
;  (cond ($display2d (mapcar 'sh l))
;       (t (loop for v in l
;		for i from 0
;		initially (format t "~%[")
;		do  (sh (header-poly v))
;		when (< i (1- (length l))) do(format t ",~%")
;		finally (format t "]")))))


;(defun add-to-chain (poly chain)
;  (cond ((eq poly 0) chain)
;	(t
;  (loop for v on chain
;	collecting (car v) into tem
;	when (eq (p-var poly)(p-var (car v)))
;	do (cond ((< (p-le poly) (p-le (car v)))
;		  (return (nconc tem (cdr v)))))
;
;	finally (return (cons poly chain))))))




(defun must-ritt-reducep (poly ch-set)
  (cond ((atom poly) nil)
	(t
  (loop for v in ch-set
	when (eq (p-var poly) (p-var v))
	do (cond ((>= (p-le poly) (p-le v))
		  (return t)))))))


(defun ritt-reduce (poly ch-set &aux tem)
  ;;assumes the ch-set is sorted by main variables
  (loop while (must-ritt-reducep poly ch-set)
	do
	(loop for v in ch-set
	      when (numberp poly)
	      do (return poly)
	      when (eq (p-var poly) (p-var v))
	      do (cond ((>= (p-le poly) (p-le v))
			(setq poly (second (setq tem (vdivide poly v))))
;			(push-new (third tem) *to-invert*) 'equal
			 (cond ((not (numberp (third tem)))
				(setq *to-invert*
				      (list (nplcm (third tem)
						    (car *to-invert*))))))
			(show *to-invert*)))
	      finally (return poly)))
  poly)

;;rational-map  will be type rmap
;;  funs (f1 f2 ... fn) denom gg
;;zopen set : (((p1,  pn)gp) (q1,..qn) gq),gg
;;where zi=qi(x1,x2,  xn)/gp and xi=pi(z1,..,zn)/gq

(defmfun my-testdivide (x y)
  (let ((errrjfflag t))
	   (catch 'raterr (pquotient x y))))

(defun new-testdivide (f g &aux quot)
  (setq quot (ratreduce f g))
  (setq quot (cond ((equal (denom quot) 1) (num quot))
	(t nil)))
  (iassert (equal quot (my-testdivide f g)))
  quot)

(defun eliminate-highest-power-dividing-homogeneously  (list-fns divisor &optional
							(highest-deg 10000)
							&aux quot )
  (cond ((and (numberp divisor)(Equal (abs divisor) 1))
	 list-fns)
	(t
	 (loop named sue
	       with prev-list-quotients = list-fns
	       for i from 1
	       do
	       (loop for v in prev-list-quotients
		     do
		     (setq quot	       (my-testdivide v divisor))
		     when (or  (null quot) (> i highest-deg))
		     do
		     (return-from sue prev-list-quotients)
		     else
		     collecting quot into new-quotients
		     finally (setq prev-list-quotients new-quotients))))))

;
;(defun eliminate-common-factors (list-fns &aux facts simple-fn)
;  (loop for v in list-fns collecting (pcomplexity v)
;	      into tem
;	      when (not (pzerop v))
;	      minimize (car tem) into the-min
;	      finally
;              (setq simple-fn (nth (setq the-min (find-position-in-list the-min tem)) list-fns)))
;  (setq facts (npfactor simple-fn))
;  (loop for (pol deg)  on facts by #'cddr
;	with quot = list-fns
;	do (setq quot (eliminate-highest-power-dividing-homogeneously quot pol deg))
;	finally (return quot)))

(defun sort-remember (list predicate &key key)
  (setq list (loop for i from 0 for v in list
	collecting (cons i v)))
  (setq list (sort list predicate :key (or (and key  #'(lambda (x)  (key (cdr x))))
				#'cdr)))
  (loop for v on list
	collecting (caar v) into ordering
	do (setf (car v) (cdar v))
	finally (return (values list ordering))))

(defun un-sort (list ordering &aux (newlist (make-list (length ordering))))
  (loop for v in ordering
	for w in list
	do (setf (nth v newlist) w))
  newlist)

(defun sort-by-ordering (list ordering)
  (loop for v in ordering
	collecting (nth v list)))

(defun copy-sort (list predicate &key key slow-key remember)
  "copies the list and sorts by predicate. If slow-key, it uses only one application of the key per item.
If slow-key or remember it returns a second value : the ordering so that (un-sort  result ordering)
would restore the list"
  (cond  (slow-key
	  (multiple-value-bind (result order)
	      (sort-remember (mapcar slow-key list) predicate)
	    (values (sort-by-ordering list order) order)))
	 (remember (sort-remember list predicate :key key))
	 (t (sort (copy-list list) predicate :key key))))

;;this works for rational functions as well should be fixed to use complexity to.
(defun eliminate-common-factors (list-functions &aux num-gcd denom-gcd firs)
  "try to speed up by putting simple ones first"
  (multiple-value-bind (list-fns order)
      (copy-sort list-functions '< :slow-key
		 #'(lambda (x) (+ (pcomplexity (function-numerator x))
				  (pcomplexity (function-denominator x)))))
    (setq firs (first list-fns))
    (cond ((polynomialp firs)
	   (setq num-gcd   firs)
	   (setq denom-gcd 1))
	  ((rational-functionp firs)
	   (setq num-gcd (num  firs))
	   (setq denom-gcd (denom firs))))
    (loop for v in (cdr list-fns)
	  when (polynomialp v)
	  do (setq num-gcd (pgcd num-gcd v))
	  (setq denom-gcd 1)
	  else
	  do (check-arg v 'rational-functionp "rational function")
	  (setq num-gcd (pgcd num-gcd (num v))))
    (un-sort (loop for v in list-fns
		   collecting
		   (st-rat (cons (pquotient (function-numerator v) num-gcd)
				 (pquotient (function-denominator v) denom-gcd))))
	     order)))




(defun reduce-rational-map-old (rmap &aux answ (gc (rmap-denom rmap)) (genvar *genvar*))
  (let ((genvar (nreverse(sort (list-variables (cons (rmap-denom rmap)(rmap-fns rmap)))
		      'pointergp))))
  (loop for v in (rmap-fns rmap)
	do
	(setq gc (pgcd gc v)))
  (setq answ (make-rmap))
  (setf (rmap-fns answ)
	(loop for v in (rmap-fns rmap)
	      collecting (pquotient v gc)))
  (setf (rmap-denom answ) (pquotient (rmap-denom rmap)gc))
  answ))



(defun reduce-rational-map (rmap &aux red-fns fns (genvar *genvar*) answ)
  (let ((genvar (nreverse(sort (list-variables (cons (rmap-denom rmap)(rmap-fns rmap)))
			       'pointergp))))
    (setq fns (cons (rmap-denom rmap) (rmap-fns rmap)))
    (setq red-fns   (eliminate-common-factors fns))
    (setq answ   (zl-copy-structure rmap rmap- fns (cdr red-fns)
					 denom (car red-fns)))
    answ))

(defun convert-rmap-to-new (name)
	 (zl-copy-structure
	       name rmap-
		    fns
		    (loop for v in (rmap-fns name)
			  collecting (ratreduce v (rmap-denom name)))))



(defun new-rmap-p (rmap)
  (and (eq (car rmap) 'rmap)
       (loop for v in (rmap-fns rmap)
	     when (not (rational-functionp v))
	     do (return nil) finally (return t))))

(defmacro new-rmap (f)
  `(cond ((not (new-rmap-p ,f))(format t "~%Converting an rmap")
	  (setq ,f (convert-rmap-to-new ,f)))))


(defun my-pairlis (l g)
  (loop for v in l
	for w in g
	collecting (cons v w)))

(defremember compose-rmap (f g)
  (let (fns-f subs)
    (new-rmap f) (new-rmap g)
    (setq subs (loop for gg in (rmap-fns g)
		  for v in *xxx* collecting (cons v gg)))
    (setq fns-f (loop for ff in (rmap-fns f) collecting (simple-rat-sublis subs ff)))
    (construct-rmap fns-f)))

;;should take [(x1+x2)

(defun construct-rmap (list-funs &aux (answ 1) rat-fns)
  (cond (($listp list-funs)(setq list-funs (cdr list-funs))))
  (setq rat-fns (loop for v in  list-funs
		      when (polynomialp v) collecting (cons v 1)
		      else when (rational-functionp v) collecting v
		      else when (get v 'disrep) collecting (cons (list v 1 1) 1)
		      else collecting (new-rat v)))
  (loop for w in rat-fns
	do (setq answ (nplcm answ (denom w))))
  (make-rmap :fns rat-fns :denom answ))



(defun describe-rmap (rmap)
  (cond ((eq (car rmap) 'rmap)
	 (loop for v in (rmap-fns rmap)
	       collecting  (header-poly  v) into tem
	       finally (displa (cons '(mlist) tem)) (format t "Common denom is ..")
	       (displa ($factor (new-disrep (rmap-denom rmap))))))))

(defvar *give-coordinates* t)

(defun describe-zopen (op)
  (cond ((zopen-history op)
	 (format t "~%The opens history is ~A " (zopen-history op))))
  (cond (*give-coordinates*
	   (format t "~%Zopen with Coord and inverse")
	   (describe-rmap (zopen-coord op))
			      (describe-rmap (zopen-inv op))))
  (format t "~%inequality is ..")
  (displa ($factor (new-disrep  (zopen-inequality op)))))

(defun xxx (i)
  (list (nth (1- i) *xxx* ) 1 1))

;; I is the index of the special slot so this will return
;;the ith cover of the blowup of the FIRSTK coordinates of
;; DIM-affine space
(defun ichart (i firstk dim &aux fns qss pss)
  (setq fns
    (loop for j from 1 to dim
	  when (and (<= j firstk) (not (eql j i)))
	  collecting  (xxx j)
	  else
	  collecting   (ptimes (xxx i) (xxx j))))
  (setq fns (loop for v in fns
		  with den = (xxx i)
		  collecting (ratreduce v den)))
  (setq pss  (construct-rmap fns ))
  (setq fns
	(loop for j from 1 to dim
	      when (and (<= j firstk) (not (eql i j)))
	      collecting  (ptimes (xxx i) (xxx j))
	      else collecting  (xxx j) ))
  (setq qss (construct-rmap fns ))
  (make-zopen :coord pss :inv qss  :inequality 1))

(defun des (obj &optional (stream *standard-output*))
  (let ((*standard-output* stream))
    (cond
      ((atom obj)(format t "~s" obj))
      ((or (polynomialp obj)(rational-functionp obj))(sh obj))
      (t (case (car obj)
	   (zopen (show (zopen-history obj)))
;	    (describe-zopen obj))
	   (rmap (format t "~%Rmap with coordinate functions") (describe-rmap obj))
	   (ldata (format t "~%Ldata with the comoponent equations")(describe-ldata obj))
	   (pre-ldata-sheaves (format t "~%Pre Ldata sheaves")(describe-pls obj))
	   (components (describe-components obj))
	   (s-var (format t "~%S Variety with basic open sets :")(des (cdr obj) stream))

	   (t (cond ((macsyma-typep obj) (string-grind obj :stream stream))
		    (t
		     (loop for v in obj
		    for i from 0
		    do
		    (cond ((and (listp v)(member (car v) '(ldata zopen) :test #'eq))
			   (format t "~%Number ~D :" i)))
		    (des v stream))))))))
    obj))

(defun des (obj &optional (stream *standard-output*))
  (let ((*standard-output* stream))
    (cond
      ((atom obj)(format t "~s" obj))
      ((or (polynomialp obj)(rational-functionp obj))(sh obj))
      (t (case (car obj)
	   (zopen
	    (describe-zopen obj))
	   (rmap (format t "~%Rmap with coordinate functions") (describe-rmap obj))
	   (ldata (format t "~%Ldata with the comoponent equations")(describe-ldata obj))
	   (pre-ldata-sheaves (format t "~%Pre Ldata sheaves")(describe-pls obj))
	   (components (describe-components obj))
	   (s-var (format t "~%S Variety with basic open sets :")(des (cdr obj) stream))

	   (t (cond ((macsyma-typep obj) (string-grind obj :stream stream))
		    (t
		     (loop for v in obj
		    for i from 0
		    do
		    (cond ((and (listp v) (member (car v) '(ldata zopen) :test #'eq))
			   (format t "Number ~D :" i)))
		    (des v stream))))))))
    obj))



(eval-when
    #+gcl (compile eval load)
    #-gcl (:compile-toplevel :load-toplevel :execute)
  (defmacro pls-opens (pls) `(sv-zopens (pls-s-var ,pls))))

(defvar *reorder-eqns* t)

(defun describe-pls (pls &aux dim eqns  ch-set)
  (format t "~%It has ~D opens having ~A components."
	  (length (pls-opens pls)) (loop for u in (pls-data pls)
				     collecting (length u)))
  (loop for v in (sv-zopens (pls-s-var pls))
	for w in (pls-data pls)
	for i from 0
	do  (des v)
	(setq dim (length (rmap-fns (zopen-coord v))))
	(loop for u in w
	      when   *reorder-eqns*
	      when
	      (progn (multiple-value (eqns ch-set) (order-equations (ldata-eqns u)))
		     ch-set)
	      collecting (- dim (length (ldata-eqns u))) into tem
	      else collecting "?" into tem
	      when *reorder-eqns*
	      collecting (make-ldata :eqns eqns :inequality
				     (ldata-inequality u)
				     :usedup (ldata-usedup u))
	      into newl
	      finally
	      (format t "~%with the ~D ldata" (length w))
	      (cond (*reorder-eqns*	    (format t " of dimension ~A "  tem)
					    (setq w newl)))
	      (format t "on  open number ~D: "i))
	(des w)))

(defun make-component-history (pls &key (add-to-open-history t) &aux dim eqns ch-set)
  (loop named sue for op in (pls-opens pls)
	for lis-dat in (pls-data pls)
	do
	(setq dim (length (rmap-fns (zopen-coord op))))
	collecting
	(loop for u in lis-dat
	      when
	      (progn (multiple-value (eqns ch-set) (order-equations (ldata-eqns u)))
		     ch-set)
	      collecting (- dim (length (ldata-eqns u))) into tem
	      else collecting "?" into tem
	      finally (cond (add-to-open-history
			     (push (cons 'dimensions tem) (zopen-history op))))
	      (return (cons 'dimensions tem)))))

(defun zopen-dim (zop)
  (length (rmap-fns (zopen-coord zop))))

;;tried to simplify and write better.
;(defun new-ldata-simplifications (ldata &key (open-g 1)
;				  &aux fns used-up answ var  *clear-above*)
;  (prog
;    sue
; nil
;    (setq fns (copy-list (ldata-eqns ldata)))
; eliminate-linears
;    (loop for f in fns
;
;	  when (and	   (not (member f used-up)) (setq var (any-linearp f open-g)))
;	  do
;	  (setq fns  (replace-functions fns
;					f var))
;	  (setq used-up  (replace-functions used-up
;					    f var))
;	  (push f used-up)
;	  (go eliminate-linears))
;    (setq fns (union-equal fns))
; eliminate-invertible-leading
;    (loop for f in fns
;	  when (setq var (any-invertible-leading-coefficient f open-g))
;	  when (not (loop for v in *clear-above*
;			  with deg = (pdegree f var)
;			  when (and (eq (car v) var) (>= deg (second v)))
;			  do (return nil) finally (return t)))
;	  do
;	  (setq fns  (replace-functions fns
;					f var))
;	  (setq fns (cons f fns))
;	  (go eliminate-linears))
;
;    (setq ldata (copy-structure ldata ldata- eqns (delete 0 (union-equal used-up fns))))
; make-dichotomy
;    (setq answ (new-make-dichotomy ldata :open-g open-g))
;    ;;if no dichotomy continue
;    (cond ((eq (length answ) 1) (setq ldata (car answ)))
;	  (t (return (delete-redundant-ldata answ))))
; divide-dichotomy
;    (setq answ (new-divide-dichotomy ldata :open-g open-g))
;    (cond ((eq (length answ) 1) (setq ldata (car answ)))
;	  (t (return (delete-redundant-ldata answ))))
;    (return answ)))
;
;
;;;not sure about the *refine-opens* = nil mode working.
;(defun new-MAKE-DICHOTOMY (ldata &key (open-g 1)&aux all-facs stop-simplify  eqns-modv ld  answ gg dich lin-dich)
;  "If stop-simplify is true then it only works if have linear dichotomy.  It returns
;  a list of  ldata "
;  (setq dich (find-good-dichotomy  ldata))
;  (setq all-facs *all-factors*)
;  (setq dich (order-dichotomy dich))
;  (show dich)
;  (setq gg (nplcm open-g (ldata-inequality ldata)))
;  (cond ((null *refine-opens*) (setq open-g gg) ))
;  (setq lin-dich
;	(loop for v in dich when (not (any-linearp v gg)) do (return nil)
;	      finally (return (and dich t))))
;  (show lin-dich)
;;; I think the gm-prepared business should all be done after.
;;; this may not be true.  The simplifications from finding a gm-prepared
;;; and performing the elimination of variables might be necessary.
;  (cond ((null lin-dich)
;	 (check-for-gm-prepared (ldata-eqns ldata) open-g)
;	 (show *stop-simplify*)))
;  (setq stop-simplify *stop-simplify*)
;  (cond ((and ;;(null *refine-opens*)
;	      *stop-simplify* )
;	 (setq answ (ldata-refinement ldata (car *stop-simplify*)
;				      (second *stop-simplify*) :inequality open-g))
;;	 (mshow answ)  (format t "**Is the refinement ")
;	 (setq *stop-simplify* nil)
;	 (setq answ (loop for v in answ
;					  appending (new-ldata-simplifications v :open-g
;									       open-g))))
;  ;;priority 1 Linear-dichotomy
;  ;;         2 gm-prepared equation
;  ;;         3 any dichotomy
;  ;;proceed with dich if dich is linear or if found no gm-prepared
;	((or lin-dich (null *stop-simplify*))
;	 (cond (dich
;		(loop for v in dich
;		      with so-far = 1
;		      appending
;		      (progn
;			(setq eqns-modv
;			      (loop for facs in all-facs
;				    when (not (member v facs))
;				    collecting
;				    (apply 'gen-ptimes (loop for ter in facs by #'cddr
;							     when (not
;								    (may-invertp
;								      ter so-far))
;							     collecting ter))))
;			(cond ((member nil eqns-modv :test #'eq) (merror "nil should not be here")))
;			(cond ((eq v nil) (merror "nil should not be here")))
;			(setq ld (make-ldata))
;			(setf (ldata-eqns ld) (cons v eqns-modv))
;			(setf (ldata-inequality ld)(nplcm gg so-far))
;			(setf so-far (nplcm so-far v))
;			(cond ((Null *stop-simplify*)
;			       (new-LDATA-SIMPLIFICATIONS ld :open-g open-g))
;			      (t (list  ld))))
;		      into list-of-ld
;		      finally
;		      (cond ((null list-of-ld)
;			     (setq answ (list (make-ldata eqns '(1) inequality 1))))
;						     (t (setq answ list-of-ld)))))
;	       (t (setq answ (list ldata))))))
;
;  (cond ((null answ) (setq answ (list ldata))))
;;;  (cond ((null answ) (setq answ (list (make-ldata :eqns '(1) :inequality 1)))))
;;  (setq answ (delete-redundant-ldata answ)))
;  answ)
;
;(defun new-divide-dichotomy (ldata &key (open-g 1) &aux (eqns (ldata-eqns ldata)) f new-eqns do-dichotomy
;	try		     occurs vars highest-vars orig-rep repeat eqns-rep)
;
;  "endeavors to turn ldata into an  triangular list of eqns
; so that each equation has  possibly one more variable occurring than the
; previous.  It takes a good order for the variables and then takes the first variable
; to be highest in two succeeding eqns, and does a division to try to correct this.  If
; the leading variable is not invertible it does a dichotomy.   This dichotomy must be
; at the level of open sets, since otherwise we will not get component containment."
;  ;;ordering should take into account open-g
;  (setq vars  (good-order-variables eqns))
;  (setq occurs (second vars))
;  (setq vars (first vars))
;  (setq highest-vars
;	(loop for v in occurs
;	      collecting (loop for u in vars
;			       when (member u v :test #'eq)
;			       do (return u))))
;  (show vars highest-vars)
;  (setq repeat
;	(loop named rep for v in vars
;	      do
;	      (loop for w on highest-vars
;		    when (and (eq (car w) v)
;			      (member v (cdr w) :test #'eq))
;		    do (return-from rep v))))
;  (cond
;    (repeat
;     (setq eqns-rep  (loop for v in eqns
;			   for u in highest-vars
;			   when (eq u repeat)
;			   collecting v))
;     (setq orig-rep (copy-list eqns-rep))
;     (show (length eqns-rep))
;     (setq eqns-rep (sort-key eqns-rep '< 'pdegree repeat))
;     (loop for v on eqns-rep while (>= (length v ) 2)
;           do
;	   (cond ((eq ( pdegree (first v) repeat)
;		      (pdegree (second v) repeat))
;		  ;;choose the least complex leading coefficient to divide by
;		  (setq try
;			(sort-key (firstn 2 v) '<
;				  #'(lambda (u var) (pcomplexity
;						      (leading-coefficient u var)))
;				  repeat)))
;		 (t (setq try (firstn 2 v))))
;	   (setq f (second try))
;	   (multiple-value-bind (rem c-reqd)
;	       (gen-prem  f (first try) repeat)
;	     (show rem)
;	     (cond ((null rem) (merror "empty")))
;	     (cond ((may-invertp c-reqd open-g)
;		    (return (new-ldata-simplifications
;			      (copy-structure ldata ldata-
;					      eqns
;					      (cons rem	(delete f(copy-list  eqns))))
;			      :open-g open-g)))))
;	   finally (return (list ldata))))))
;
;
;

;resolution(f):=
;block([g,h],
;      for i thru 5   do
;      (g: coeff(f,x),
;       h: f-x*g,
;       f: expand(subst(x+g/2,x,h)+g^2/4)),
;	return(f));
;(defun resolution (f n var &aux h g gg ggg)
;  (loop for i below n
;	with mon = (list var 1 1)
;	do (setq g  (pcoeff (num f) mon))
;	(setq h (ratdifference f (setq gg (ratreduce (ptimes g mon) (denom f)))))
;	(setq ff (gen-rat-sublis
;		  (list var)
;		  (list (setq ggg (ratplus (cons mon 1) (ratreduce g (* 2 (denom f))))))
;		  h))
;	(setq g (ratreduce g (denom f)))
;	(setq ff  (ratplus ff (rattimes (cons 1 4)  (rattimes g g nil) t)))
;	(show (denom h))
;	(setq f ff)
;	finally (return f)))


;(defun new-solve-ldata (ldata &key (open-g 1))
;  (user:function-let
;    (ldata-simplifications #'(lambda (&rest l) (list (car l))))
;
;   (loop until (equal list-ld prev-ld)
;	 with list-ld = (list ldata)
;	 do
;     (format t "~%make-dichotomy")
;     (setq list-ld
;	   (loop for ld in list-ld
;		 appending (make-dichotomy ld :open-g open-g)))
;     (mshow list-ld)
;     (format t "~%divide-dichotomy")
;     (setq list-ld
;	   (loop for ld in list-ld
;		 appending (divide-dichotomy ld :open-g open-g)))
;     (mshow list-ld))))
;
;
;;;ideas have a simpflag slot
;;;which contains things like 'all-irreducible 'no-linears 'no-
;;;a clear above slot which has '(x 3 y 1) to indicate that only one
;;;note if z+y is a polynomial can't use it for z and y so it has to
;;;be in the used up group.
