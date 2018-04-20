(in-package :maxima)

(defstruct tc-state
  (nvars 0 :type integer)
  (ncosets 0 :type integer)
  (multiply-table nil)
  (relations nil)
  (subgroup-generators nil)
  (row1-relations nil))

(defvar $todd_coxeter_state)

;;  To turn on debug printing set to T
(defvar *debug* nil)

;;  When *debug* is not nil, this holds the multiplications for
;; the current row.
(defvar *this-row* nil)

(deftype coset nil 'integer)

;; The data type we use to enumerate cosets.

(defvar *todo* (make-array 10 :element-type 'coset :fill-pointer 0 :adjustable t :initial-element 0))

(defmacro with-multiply-table (&body body)
  `(let ((nvars (tc-state-nvars $todd_coxeter_state))
	 (multiply-table (tc-state-multiply-table $todd_coxeter_state)))
     (declare (type (vector t) multiply-table))
     ,@body))

(defmacro undef (s)
  `(eql 0 ,s))

;; Multiply coset K times variable R
(defmacro tc-mult (k r)
  `(the coset (aref (table ,r) ,k)))

;; Force  k . r = s and  k = s . r^-1
(defmacro define-tc-mult (k r s)
  `(progn
     (setf (tc-mult ,k ,r) ,s)
     (setf (tc-mult ,s (- ,r)) ,k)))

;; cosets M < N are to be made equal
(defmacro push-todo (m n)
  `(progn
     (vector-push-extend ,m *todo*)
     (vector-push-extend ,n *todo*)))

;; The multiplication table for variable i
;; (can only be used inside with-multiply-table)
(defmacro table (i)
  `(the (vector (coset)) (aref multiply-table (+ ,i nvars))))


;; NVARS is the number of of variables.   It should be the maximum
;; of the absolute values of the entries in the relations RELS.
;; The format of the relations is variables X,Y,.. correspond to
;; numbers 1,2,.. and X^^-1, Y^^-1 .. are -1,-2,...   RELS is
;; a list of lists in these variables.
;; Thus rels = '((1 -2 -1) (2 2 3) ..)  (ie [x1.x2^^-1 . x1^^-1, x2.x2.x3,.. ))
;; SUBGP is also a list of lists.
;; Returns order of G/H, where G is Free/(rels), and H is
;; This is the main entry point at lisp level.
;; Example: (TODD-COXETER 2 '((1 1) (1 2 1 2 1 2) (2 2)))
;; returns 6.   In (tc-state-multiply-table $todd_coxeter_state) we find the current
;; state of the action of the variables on the cosets G/H.
;; For computing the symmetric group using the relations
;; p(i,j) :=concat(x,i).concat(x,j);
;; symet(n):=create_list(if (j - i) = 1 then (p(i,j))^^3 else
;;     if (not i = j) then (p(i,j))^^2 else p(i,i) , j,1,n-1,i,1,j);
;; todd_coxeter(symet(n)) == n!
;; the running time of the first version of this code is observed to be quadratic
;; in the number of cosets.  On a rios it is approx 5*10^-5 * (ncosets)^2.

(defun todd-coxeter (nvars rels subgp &aux (i 1) (c 0))
  (set-up nvars rels subgp)
  (loop while (>= (tc-state-ncosets $todd_coxeter_state) i)
	 do  (incf c) ;; count how many row tries..
	 (cond ((doing-row i) ;; row still being done
		(replace-coset-in-multiply-table))
	       ((> (fill-pointer *todo*) 0) ;; row finished but there is work to do
		(incf i)
		(replace-coset-in-multiply-table))
	       (t ;; row finished -- no work
		(incf i))))
  (format t "~%Rows tried ~d~%" c)
  (tc-state-ncosets $todd_coxeter_state))

;; Store the data in $todd_coxeter_state, and build multiply-table.
(defun set-up (nvars rels subgp)
  (setf (fill-pointer *todo*) 0)
  (setf $todd_coxeter_state (make-tc-state :nvars nvars
					   :ncosets 1
					   :relations rels
					   :subgroup-generators subgp
					   :row1-relations (append subgp rels)
					   :multiply-table (make-array (1+ (* 2 nvars)))))

  (with-multiply-table
      (loop for rel in (tc-state-row1-relations $todd_coxeter_state) do
	   (loop for v in rel
	      do (unless (<= 1 (abs v) nvars)
		   (error "Vars must be integers with absolute value between 1 and ~d" nvars))))
    (loop for i from (- nvars) to nvars
       unless (zerop i)
       do (setf (table i) (make-array 10 :adjustable t :element-type 'coset :initial-element 0)))))

;; Starts multiplying coset i times the relations.  Basic fact is i . rel = i.
;; This gives a condition on the multiplication table.  Once we have made it all
;; the way through the relations for a given coset i, and NOT had any
;; incosistency in our current multiplication table, then we go on the the next
;; coset.  The coset 1 denotes H.  so for generators h of H we we have 1 . h = 1.
;; So when we do row 1, we add to the relations the generators of H.

;; When we do find an inconsistency eg: 7 . y = 1 and 4 . y = 1 or 7 = 1 . y^^-1
;; and 4 . y = 1, then we would know that 4 and 7 represent the same coset, and
;; so we put 4 and 7 in the *todo* vector and return t so that
;; replace-coset-in-multiply-table will identify them.  While we are running
;; inside doing-row, the multiply-table is accurate, up to our current state of
;; knowledge.  Note that once we find such a nonpermutation action of y, we could
;; not maintain the consistency of (table i) and (table -i).  We exit doing-row
;; with value t, to indicate replacements should be done, and that we must
;; return to complete row i.  (Actually we return t even in the case we were
;; finished the row and found the duplicate in the last step).

(defun doing-row (i &aux (j 0) (k 0) (r 0)(s 0) *this-row* relations)
  (setf relations (if (eql i 1)
		      (tc-state-row1-relations $todd_coxeter_state)
		      (tc-state-relations $todd_coxeter_state)))
  (with-multiply-table
      (loop for rel in relations
	 for v on relations
	 do
	 (setq k i)
	 (loop
	    do
	    (setq r (car rel))
	    (setq s (tc-mult k r))
	    (cond ((undef s)
		   (cond ((cdr rel)
			  (setq s (next-coset))
			  (define-tc-mult k r s))
			 (t (setq s (tc-mult i (- r)))
			    (cond ((undef s) (define-tc-mult k r i))
				  ((< k s) (push-todo k s)(return-from doing-row (cdr v)))
				  ((> k s) (push-todo s k)(return-from doing-row (cdr v))))
			    (loop-finish)))))
	    (cond ((setq rel (cdr rel))
		   (when *debug*
		     (push s *this-row*)
		     (my-print (reverse *this-row*) i))
		   (setq k s)
		   (incf j))
		  ((< i s)
		   (push-todo i s) (return-from doing-row (cdr v)))
		  ((> i s)
		   (push-todo s i) (return-from doing-row (cdr v)))
		  (t		      ;rel is exhausted and it matched
		   (loop-finish))))))
  (when *debug*
    (dcheck-tables)
    (my-print (reverse *this-row*) i))
  nil)

;; FILL-IN-INVERSES not only completes the (table i) for i < 0
;; but at the same time checks that (table i) for i > 0
;; does not have repeats.   eg if  5 . y = 3 and 7 . y = 3,
;; then this would show up when we go to build the inverse.
;; if it does we add 5 and 7 to the *todo* vector.

(defun fill-in-inverses (&aux (s 0) (sp 0))
  (with-multiply-table
      (loop for i from 1 to nvars
	 do (let ((ta1 (table i))
		  (ta2 (table (- i))))
	      (declare (type (vector (coset)) ta1 ta2))
	      (loop for j from 1 to (tc-state-ncosets $todd_coxeter_state) do
		   (setf (aref ta2 j) 0))
	      (loop for j from 1 to (tc-state-ncosets $todd_coxeter_state) do
		   (setf s (aref ta1 j))
		   when (not (eql 0 s))
		   do
		   (setf sp (aref ta2 s))
		   (cond ((eql 0 sp) (setf (aref ta2 s) j))
			 (t ;; there's a duplicate!
			  (push-todo sp j)
			  (return-from fill-in-inverses t))))))))

;; set n (vector-pop *todo*) , m (vector-pop *todo*)
;; and replace n by m in multiply-table and in *todo*.
;; The replacement is done carefully so as not to lose ANY
;; information from multiply-table, without recording it in
;; *todo*.   It finishes by doing FILL-IN-INVERSES which may
;; in turn cause entries to be added to *todo*.

(defun replace-coset-in-multiply-table (&aux (m 0) (n 0) (s 0) (s2 0) )
  (with-multiply-table
      (tagbody
       again
	 (setf n (vector-pop *todo*))
	 (setf m (vector-pop *todo*))
	 (unless (eql m n)
	   (dprint-state)
	   (when *debug* (format t "     ~a --> ~a " n m))

	   (loop for i from 1 to nvars
		  do
		  (let ((ta (table i)))
		    (declare (type  (vector (coset)) ta))
		    (setq s2 (tc-mult n i))
		    (unless (undef s2)
		      (setq s (tc-mult m i))
		      (cond ((undef s) (setf (tc-mult m i) s2))
			    ((< s s2) (push-todo s s2))
			    ((> s s2)(push-todo s2 s))))
		    (loop for  j downfrom (1- n) to 1
			   do (setq s (aref ta j))
			   (cond ((>  s n) (setf (aref ta j) (1- s)))
				 ((eql s n) (setf (aref ta j) m) )))
		    (loop for  j from n below (tc-state-ncosets $todd_coxeter_state)
			   do (setq s (aref ta (1+ j)))
			   (cond ((>  s n) (setf (aref ta j) (1- s)))
				 ((eql s n) (setf (aref ta j) m) )
				 (t  (setf (aref ta j) s))))))

	   (loop for i downfrom (1- (fill-pointer *todo*)) to 0
		  do (setf s (aref *todo* i))
		  (cond ((> s n) (setf (aref *todo* i) (1- s)))
			((eql s n)(setf (aref *todo* i)  m))))
	   (decf (tc-state-ncosets $todd_coxeter_state))
	   (dprint-state))

	 (when (> (fill-pointer *todo*) 0)
	   (go again))
	 ;;(format t "~%There are now ~a cosets" (tc-state-ncosets $todd_coxeter_state))
	 ;; check for new duplicates introduced!!
	 (when (fill-in-inverses)
	   (go again)))))

;; Get the next coset number, making sure the multiply-table will
;; have room for it, and is appropriately cleaned up.
(defun next-coset ()
  (let* ((n (1+ (tc-state-ncosets $todd_coxeter_state)))
	 (m 0))
    (with-multiply-table
	(let ((ta (table 1)))
	  (unless (> (array-total-size ta) (1+ n))
	    (setf m (+ n (ash n -1)))
	    (loop for i from (- nvars) to nvars
		   when (not (eql i 0))
		   do (setf ta (table i))
		   (setf (table i) (adjust-array ta m))))
	  (loop for i from 1 to nvars
		 do (setf (aref (table i) n) 0)
		 (setf (aref (table (- i)) n) 0))))
    (setf (tc-state-ncosets $todd_coxeter_state) n)))



;;  $todd_coxeter parses maxima args
;; todd_coxeter(rels, subgrp) computes the
;; order of G/H where G = Free(listofvars(rels))/subgp_generated(rels));
;; and H is generated by subgp.   Subgp defaults to [].
;; todd_coxeter([x^^3,y.x.y^^-1 . x^^-1],[])  gives 6  the order of the symmetric group
;; on 3 elements.
;; todd_coxeter([a^^8,b^^7,a.b.a.b,(a^^-1 . b)^^3],[a^^2, a^^-1 . b]); gives 448

(defun $todd_coxeter (rels &optional (subgp '((mlist))))
  (let ((vars ($sort ($listofvars rels)))
	(neg 1))
    (declare (special neg vars))
    (todd-coxeter ($length vars) (mapcar #'coerce-rel (cdr rels)) (mapcar #'coerce-rel (cdr subgp)))))

(defun coerce-rel (rel)
  (declare (special vars neg))
  (if (atom rel)
      (list (* neg (position rel vars)))
      (case (caar rel)
	(mnctimes (apply #'append (mapcar #'coerce-rel (cdr rel))))
	(mncexpt (let* ((n (meval* (third rel)))
			(neg (signum n))
			(v (coerce-rel (second rel))))
		   (declare (special neg))
		   (loop for i below (abs (third rel))
		      append v)))
	(otherwise (error "bad rel")))))

;; The following functions are for debugging purposes, and
;; for displaying the rows as they are computed.

(defvar *names* '(nil x y z))

(defun my-print (ro i &aux relations)
  (when *debug*
    (fresh-line)
    (format t "Row ~a " i)
    (setq relations (if (eql i 1)
			(tc-state-row1-relations $todd_coxeter_state)
			(tc-state-relations $todd_coxeter_state)))
    (loop for rel in relations do
	 (loop for v on rel do
	      (format t (if (> (car v) 0) "~a" "~(~a~)")
		      (nth (abs (car v)) *names*))
	      (when (null ro) (return-from my-print))
	      (if (cdr v)
		  (princ (pop ro))
		  (format t "~a | ~a" i i))))))

(defun has-repeat (ar &aux (j (1+ (tc-state-ncosets $todd_coxeter_state))) ans tem)
  (loop for k from 1 to (tc-state-ncosets $todd_coxeter_state) do
       (setq tem (aref ar k))
       (when (and (not (eql tem 0))
		  (find tem ar :start (1+ k) :end j))
	 (pushnew tem ans)))
  ans)

(defun dcheck-tables (&aux tem)
  (when *debug*
    (with-multiply-table
	(loop for i from 1 to nvars
	   do (if (setq tem (has-repeat (table i)))
		  (format t "~%Table ~a has repeat ~a " i tem))))))

(defun dprint-state ()
  (when *debug*
    (with-multiply-table
	(format t "~%Ncosets = ~a, *todo* = ~a" (tc-state-ncosets $todd_coxeter_state) *todo*)
      (loop for i from 1 to nvars do
	   (format t "~%~a:~a" (nth i *names*) (subseq (table i) 1 (1+ (tc-state-ncosets $todd_coxeter_state)))))
      (my-print (reverse *this-row*) 0))))
