;;;;;;  -*- Mode:Lisp; Package:CL-MAXIMA; Syntax:COMMON-LISP; Base:10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defun get-altq (item a-list)
  (loop for v on a-list by #'cddr
	when (eq item  (car v))
	do (return (second v))))

(defun mdegree (monom var)
  "Degree of a monomial in var Returns nil if var not in monom"
  (do ((v monom ))
      (nil)
    (cond ((numberp v) (return nil)))
    (cond ((eq (car v) var)(return (second v))))
    (setq v (caddr v))))

(defun part-above-degree (poly  monomial-degs &aux tem)
  "The argument is  a Cre polynomial and the
  monomial degs is an alternating list of the variable and the degree
  Note these must be ordered in increasing degree for the degree sequence"

  (cond ((null monomial-degs)poly)
	((numberp monomial-degs) poly)
	((numberp poly)(cond ((loop for w in (cdr monomial-degs) by #'cddr
				    when (not (eq w 0))
				    do(return t)) nil)
			     (t poly)))
	(t (let ((deg))
	     (cond ((eq (car monomial-degs)(car poly))
		    (setq deg (1- (second monomial-degs)))
		    (setq monomial-degs (cddr monomial-degs)))
		   (t  (setq deg (get-altq (car poly) monomial-degs))
		       (cond (deg (setq deg (1- deg)))
			     (t (setq deg -1)))))
	     (loop for v  on (cdr poly) by #'cddr
		   while (> (car v)  deg)
		   when (setq tem (part-above-degree (second v)  monomial-degs))
		   collecting (car v) into the-cof
		   and
		   collecting tem into the-cof
		   finally (cond (the-cof (setq the-cof  (cons (car poly) the-cof))
					  (return the-cof))
				 (t nil)))))))

(defun part-above (poly monom)
 "x^3*y+x^2*u, x^2 ==> x*y+u"
  (cond ((part-above1 poly monom))
	(t 0)))

(defun part-above1 (poly monom &aux tem (true-deg 0))
  (cond ((numberp monom) poly)
	((numberp poly) nil)
	(t (let ((deg))
	     (cond ((eq (p-var monom) (p-var poly))
		    (setq deg (1- (second monom)))
		    (setq true-deg (second monom))
		    (setq monom (caddr monom)))
		   (t (setq deg (mdegree monom (car poly)))
		      (cond (deg (setq deg (1- deg)))
			    (t (setq deg -1)))))
	     (loop for v on (cdr poly) by #'cddr
		    while (> (car v) deg)
		    when (setq tem (part-above1 (second v) monom))
		    collecting (- (car v) true-deg)
		    into the-cof
		    and
		    collecting tem into the-cof
		    finally (cond (the-cof (setq the-cof (cons (car poly) the-cof))
					   (return the-cof))
				  (t nil)))))))
;
;(defmacro push-new (item llist)
;	`(cond ((not (member  ,item ,llist :test #'eq) )
;	       (push ,item ,llist))))


;;see polyd
;(defun list-variables (u &aux a-list)
;  (declare (special a-list))
;  (list-variables2 u)
;  a-list)
;
;(defun list-variables2 (expr)
;  (cond ((atom expr) nil)
;	((polynomialp expr) (list-variables1 expr))
;	((rational-functionp expr) (list-variables2 (car expr))
;	 (list-variables2 (cdr EXPR)))
;       (t(loop for v on expr while (listp v) do (list-variables2 (car v))))))
;
;(defun list-variables1 (u)
;   (declare (special a-list))
;  (cond ((atom u) niL)
;	(t
;	 (push-new (car u) a-list)
;	 (loop for (def cof) on (cdr u) by #'cddr
;	       do (list-variables1 cof)))))
;

(defun pcoeff (poly monom &optional
	       (variables-to-exclude-from-cof (list-variables monom)))

  "x^3*y+x^2*u, x^2 ==> u . Rule: if a variable appears in monom it must be to
the exact power, and if it is in variables to exclude it may not appear unless it
was in monom to the exact power.  (pcoeff pol 1 ..) will exclude variables like
substituting them to be zero."
  (cond ((eq monom 1) nil)
	((atom monom) (setq monom (list monom 1 1))))
  (check-arg monom (or (listp monom)(eq monom 1)) "1 or a monomial")
  (cond ((pcoeff1 poly monom variables-to-exclude-from-cof ))
	(t 0)))
(defun pcoeff1 (poly monom &optional
		(variables-to-exclude-from-cof (list-variables monom))
		&aux tem (true-deg 0))
  (cond
	((and (numberp monom) (numberp poly)) poly)
	((numberp poly) nil)
	(t (let ( (first-case t) )
	     (cond ((numberp monom)
		    (cond ((member (p-var poly) variables-to-exclude-from-cof :test #'eq)
				    (setq true-deg 0))
			  (t (setq first-case nil))))
		   ((eq (p-var monom) (p-var poly))
		    (setq true-deg (second monom))
		    (setq monom (caddr monom)))
		   ((member (p-var poly) variables-to-exclude-from-cof :test #'eq)
		    (setq true-deg 0))
		   (t (setq first-case nil)))
	     (cond (first-case
		    (loop for (deg cof) on (cdr poly) by #'cddr
			  when (eq deg true-deg)
			  do
			  (return (pcoeff1 cof monom
					      variables-to-exclude-from-cof))))
		   ((or (numberp monom)(pointergp (car poly) (car monom)))
		    (loop for (deg cof) on (cdr poly) by #'cddr
			  when (setq tem (pcoeff1 cof monom variables-to-exclude-from-cof))
			  collecting deg into temm
			  and collecting tem into temm
			  finally (cond (temm
					 (cond ((eql (car temm) 0)
						(return (second temm)))
					       (t (return (cons (car poly) temm)))))))))))))


(defun replacements (&optional (simps *poly-simplifications*))
  (loop for v in simps by #'cddr collecting v))

;;These check for  occurence of seq in poly.  But using this seems to
;;be slower than just checking the part-above just in case we will need
;;to replace. The only advantage is this does no consing.  But
;;with our new storage techniques..
(defun occurs-in (poly seq)
 (cond ((eq (catch 'its-there (occurs-in1 poly seq)) 'ok)
	t)
       (t nil)))

(defun occurs-in1 (poly seq)
  (catch 'not-in-this-one
    (cond ((null seq)(throw 'its-there 'ok))
	  ((numberp poly))
	  (t (let ((var (first seq))
		   (deg (second seq)))
	       (cond ((pointergp (car poly) var)
		      (loop for (deg cof) on (cdr poly) by #'cddr
			    do (occurs-in1 cof seq)))
		     ((eq (car poly) var)
		      (cond ((< (second poly ) deg)
			     (throw 'not-in-this-one nil))
			    (t (loop for (degg cof) on (cdr poly) by #'cddr
				     while (>= degg deg)
				     do (occurs-in1 cof (cddr seq))))))
		     (t (throw 'not-in-this-one nil))))))))

(defun convert-deg-sequence-to-monomial (sequ &aux tem)
  (cond ((null sequ) nil)
	((numberp  sequ) sequ)
	(t (setq tem (convert-deg-sequence-to-monomial (cddr sequ)))
	   (cond (tem (cond ((not (eq 0 (second sequ))) (list (car sequ) (second sequ) tem))
			    (t tem)))
		 (t (list (car sequ) (second sequ) 1))))))

(defun constant-deg-sequencep (seq)
  (loop for w in seq by #'cddr
	when (not (eq w 0))
	do (return nil)
	finally (return t)))

(defun $declare_constant_list( ll)
  (apply #'declare-constant (cdr ll)))

(defun declare-constant (&rest l)
  (loop for v in l
	 when (get v 'disrep)
	 do (putprop v t 'constant)
	 else do
	 (putprop  (car (num (new-rat v)))  t 'constant)))

(defun show-constants ()
  (loop for v in *genvar* when (get v 'constant)
	do (format t "~A disreps to ~A and is constant." v (get v 'disrep))))

(defun remove-constant-property (&rest a-list)
  (loop for u in a-list do
  (loop for v in *varlist*
	for w in *genvar*
	when (or (eq v u) (eq w u))
	do (remprop w 'constant))))

(defvar *use-constants* t)

(defun constant-functionp( poly)
  "Assumes that the variables have been ordered so that
  the constant variables are the least main"
   (or (numberp poly)(numberp (car poly))
       (and *use-constants* (symbolp (car poly)) (get (car poly) 'constant))))


(defun find-main-monomial (poly &aux cof seq)
  (cond ((numberp poly) (values nil poly))
	(t
	 (setq seq (loop while poly
			 collecting (car poly)
			 collecting (second poly)
			 do (setq poly (third poly))
			 when (constant-functionp poly) do (setq cof poly poly nil)))
	 (values seq cof))))

(defun find-least-main-monomial (poly &aux   tem mono-so-far cof)
  (loop while poly
	do
	(setq tem (last2 poly))
	(cond ((not  (and (null mono-so-far)(zerop (car tem)) (numberp (second tem))))
	       (cond ((not (zerop (car tem)))

	       (setq mono-so-far (cons (car tem) (cons (car poly)
						       mono-so-far) )))))
	      (t (setq tem (lastn 4 poly))(setq mono-so-far (cons (car tem)
								     (cons (car poly)
									   mono-so-far)))))
	(cond ((numberp (second tem))(setq poly nil)(setq cof (second tem)))
	      (t (setq poly (second tem)))))
  (values (nreverse mono-so-far) cof))

;;(defvar  *monomial-order-function* 'seq-lessp)
;;(defvar  *monomial-order-function* 'xyz-and-degree)
(defvar  *monomial-order-function* 'xyz-order-sequence)

(defun deg-sequence (variables degrees)
  (loop for v in variables
	for w in degrees
	unless (eq w 0)
	collecting v
	and
	collecting w))

(defun factor-out-monomial (poly monomial-degs copy)
  (cond (copy (setq poly (copy-tree poly))))
  (cond ((or (numberp poly)	(numberp monomial-degs))
	 poly)
	(t
	 (let ((deg (get-altq (car poly)  monomial-degs)))
	   (loop
	     for v on poly by #'cddr
	     while (third v)
	     when deg
	     do
	     (setf (second v)(sub (second v) deg))
	     do (setf (third v)(factor-out-monomial
				 (third v)
				 monomial-degs
				 nil))
	     finally (return poly))))))

(defun replace-monomial (poly replacement monomial-degs
			 &optional part-above &aux to-replace denom-replacement)
  "replacement is a poly not a rat-poly"
  (check-arg poly polynomialp "polynomial")
  (check-arg replacement rational-functionp "rat'l function")
  (cond (part-above (setq to-replace part-above))
	(t  (setq to-replace (part-above-degree poly monomial-degs))))
  (let (
	answer tem )
    (cond
      ((and (numberp poly) (zerop poly))(rzero))
      (to-replace
;	   (format t "~%Replacing occurrence of ~A in.." (disrep-list monomial-degs))
;	   (sh poly)
;	   (format t "~% by the replacement")
;	   (cond (denom-replacement (sh (cons replacement denom-replacement)))
;		 (t	     (sh replacement)))
       (setq answer (pdifference poly to-replace))
       (setq denom-replacement (denom replacement))
       (setq answer (ptimes denom-replacement answer))
       (setq tem (ptimes
		   (num replacement)
		   (factor-out-monomial to-replace monomial-degs t)))	;copy?
       (setq answer (pplus answer tem))
       (setq answer (maybe-ratreduce answer denom-replacement))
;	   (format t "The result is..")
;	   (sh  answer)
       answer)
      (t poly))))

(defun replace-monomial-rat  (poly replacement monomial-degs
			 &optional part-above &aux to-replace denom-replacement)
  "replacement is a poly not a rat-poly"
  (check-arg poly rational-functionp "rat'l function")
  (check-arg replacement rational-functionp "rat'l function")
  (cond (part-above (setq to-replace part-above))
	(t  (setq to-replace (part-above-degree (num poly) monomial-degs))))
  (let (
	answer tem )
    (cond
      ((and (numberp (num poly)) (zerop (num poly)))(rzero))
      (to-replace
;	   (format t "~%Replacing occurrence of ~A in.." (disrep-list monomial-degs))
;	   (sh poly)
;	   (format t "~% by the replacement")
;	   (cond (denom-replacement (sh (cons replacement denom-replacement)))
;		 (t	     (sh replacement)))
       (setq answer (pdifference (num poly) to-replace))
       (setq denom-replacement (denom replacement))
       (setq answer (ptimes denom-replacement answer))
       (setq tem (ptimes
		   (num replacement)
		   (factor-out-monomial to-replace monomial-degs t)))	;copy?
       (setq answer (pplus answer tem))
       (cond ((eq 1 (denom poly))
	      (setq answer (maybe-ratreduce answer  denom-replacement)))
	     (t  (setq answer (maybe-ratreduce answer
					       (ptimes (denom poly )
						       denom-replacement)))))
       answer)
      (t poly))))

(defvar *reduce* t)
(defun maybe-ratreduce (a b)(cond (*reduce* (ratreduce a b))
				  (t (cons a b))))
(defun $add_to_poly_simplifications (rat-monom)
  (setq rat-monom ($new_rat rat-monom))
  (setq rat-monom (cdr rat-monom))
  (add-to-poly-simplifications rat-monom))
(defvar *poly-simplifications* nil)

(defvar *degenerate-conditions* nil)



(defun add-to-poly-simplifications (rat-monom &optional (simplify t) &aux orig added
				     replacement replacement-num)
  (setq orig rat-monom)
  (cond (*simplify-rhs* (setq rat-monom (polysimp rat-monom)))
	(t (setq rat-monom (new-rat rat-monom))))
  (cond ((numberp rat-monom)
	 (cond ((or (not (zerop rat-monom))
		    (not  ($zerop orig))) (setq *poly-simplifications*
						(list 1 (rzero))))
	       (t *poly-simplifications*)))
	((eq 0 (num rat-monom))  (princ ".")(setq simplify nil) *poly-simplifications*)
	((or (numberp rat-monom) (and (listp rat-monom)
				      (constant-functionp (num rat-monom))))
	 (setq *poly-simplifications* (list 1 (rzero)))
	 (let ((*print-level* 2))
	 (format t  "There is a constant ~A in the *poly-simplifications*" rat-monom)))
	(t
	 (multiple-value-bind (seq cof)(find-main-monomial (num rat-monom))
	   (setq replacement-num (pdifference
				   (ptimes cof
					   (convert-deg-sequence-to-monomial
					     seq))
				   (num rat-monom)
				   ))
	   (cond ((and (not (numberp (denom rat-monom)))
		       (not (constant-functionp (denom rat-monom))))
		  (format t "*****Warning a denominator was discared which might take the value 0*****" nil)))
	   (setq replacement (ratreduce replacement-num cof))
;	   (format t "~%Adding to simplifications..")
;         (sh (convert-deg-sequence-to-monomial seq))(format t "  ==>  ") (sh replacement)
	   (cond ((not (numberp cof))
		  (pushnew cof *degenerate-conditions* :test 'equal)))
	   (setq *poly-simplifications* (append *poly-simplifications*
						(list seq replacement))))
	(setq added t)
	))
  (cond (simplify  (simplify-poly-simplifications))
	(t *poly-simplifications*))
;  (cond (added (conclusionp)))      ;11/30/85, chou
  *poly-simplifications*
   )

(defun grobner-basis (list-polys &key homogeneous)
  (setq *poly-simplifications* nil)
  (setq *degenerate-conditions* nil)
  (loop for v in list-polys
	do (add-to-poly-simplifications v))
  (check-overlaps 40 :reset t :homogeneous homogeneous)
  *poly-simplifications*)


;(defun ideal-subsetp (id1 id2 &key (reset-basis t) &aux tem)
; (cond (reset-basis  (grobner-basis id2)))
;  (loop for v in id1
;	when (not (rzerop (setq tem  (polysimp v))))
;	do (format t "~A is not zero modulo the second ideal. It is congruent to ~A"
;		   v tem)
;	(return nil)
;	finally (return t)))

(defun ideal-subsetp (id1 id2 &key ( verbose t) (reset-basis t) &aux tem)
  (cond (reset-basis  (grobner-basis id2)))
  (loop for v in id1
	when (not (rzerop (setq tem  (polysimp v))))
	do
	(cond (verbose (format t "~A is not zero modulo the second ideal. It is congruent to ~A"
			       id2 tem)))
	(return nil)
	finally (return t)))

(defun $set_up_poly_simplifications(a-list)
  (check-arg a-list '$listp nil)
  (setq *poly-simplifications* nil)
  (loop for v in (cdr a-list)
	do
	(add-to-poly-simplifications (st-rat v))
  finally (return (convert-poly-simplifications-to-macsyma-form *poly-simplifications*))))

(defun $show_simps ()
  (convert-poly-simplifications-to-macsyma-form *poly-simplifications*))

;;tried using occurs-in instead of part-above-degree but wasted time...

(eval-when
    #+gcl (load compile)
    #-gcl (:load-toplevel :compile-toplevel)
  (fmakunbound 'must-replacep)
  (defun must-replacep ( poly &aux part-above monom  replace)
					;  (declare (values part-above monom replace))
    (loop for (seq repl) on *poly-simplifications* by #'cddr
	   when (setq part-above (part-above-degree poly seq))
	   do (setq monom (convert-deg-sequence-to-monomial seq) replace repl)
	   (return 'found))
    (setq *must-replace-data* (list part-above monom replace))
    (values part-above monom replace)))

(defvar *must-replace-data* nil)
(defun monomial-finish-replace ( )
  (destructuring-let (((part-above mon repl) *must-replace-data*))
    (cons (ptimes (num  repl) (num (ratreduce part-above mon))) (denom repl))))

(defun sort-poly-simplifications ()
 (setq *poly-simplifications*
       (sort-grouped-list
	 *poly-simplifications*
	 2 #'(lambda (x y)
	       (cond ((equal x y) nil)
		     (t (funcall *monomial-order-function* y x)))))))

(defun check-simp (a simped &optional (tag 'simps-not-equal) )
  (cond ((not (equal (polysimp a) simped))(break tag))))
(defvar *show-grob* nil)
(defvar *conclusion* nil)
(defun conclusionp ()
  (cond (*conclusion*
	 (format t "~%Checking conclusion... ")
	 (cond ((zero-modulo-simplifications *conclusion*)
		(format t "~%The conclusion already follows.")
		(throw 'its-proven t))
	       (t (format t "no good yet"))))))

(defun new-zerop (x)
  (or (eq x  0)
      (and (listp x) (eq (car x) 0))))

(defvar *simplify-rhs* t)

(defun simplify-poly-simplifications (&aux seq repl hi old-simps repl-simp try-conclusion
				      resimplify mono dif leng replacement-of-mono)
  ;;  (if test-all-theorems (check-proof-time))
  (cond
    (*show-grob*
     (setq leng (length *poly-simplifications*))
     (format t "~%Starting to simplify ~A poly-simplifications " (truncate leng 2))
     (cond ((> 10 leng )
	    (show-simps))
	   (t (format t "~%Replacements are :") (show-replacements)))))
  (cond
    ((loop
	for v in *poly-simplifications* by #'cddr
	when (numberp v)
	do (setq *poly-simplifications*
		 (list 1 (rzero))) (return 'unit-ideal)))
    (t
     (sort-poly-simplifications)
     (loop
	;;for (seq repl) on *poly-simplifications* by #'cddr
	for i from 2 by 2 while (<= i (length *poly-simplifications*))
	do
	  (setq seq (nth (- i 2) *poly-simplifications*)
		repl(nth (- i 1) *poly-simplifications*))
	  (setq old-simps *poly-simplifications*)
	  (setq *poly-simplifications* (append     ;;;nconac ok..
					(subseq *poly-simplifications* 0 (- i 2))
					(nthcdr i *poly-simplifications*)))
	  (cond ((must-replacep (setq mono (convert-deg-sequence-to-monomial seq)))
		 (cond (*show-grob*  (sh mono) (format t "   is not  reduced")))

		 (cond ((equal (setq replacement-of-mono
				     ;;  (polysimp mono)
				     (monomial-finish-replace))
			       repl)
			(setq i (- i 2))
			(format t "eliminated one")
			;;  (check-simp mono replacement-of-mono 'a)
			)
		       (t (setq dif
				(pdifference	  ;;;just need the numerator...
				 (ptimes (num replacement-of-mono)
					 (denom repl))
				 (ptimes (num repl) (denom replacement-of-mono))))

			  (cond (*simplify-rhs*
				 (setq dif (polysimp dif))))
			  (cond ((not (new-zerop dif))
				 (add-to-poly-simplifications   dif nil)
				 (setq try-conclusion t)))
			  (setq       resimplify t)
					; (format t "~%having to resimplify.." ) (sh dif)
			  (return 'added))))
		((and *simplify-rhs*
		      (must-replacep (num repl)))
		 ;;replace the replacement only so monom ok..
		 (setq repl-simp (polysimp (num repl)))
		 (setq repl-simp
		       (ratreduce (num repl-simp)
				  (ptimes (denom repl-simp) (denom repl))))
					;(check-simp (nth (1- i) old-simps) repl-simp 'c)
		 (setf (nth (1- i) old-simps ) repl-simp)
		 (setq *poly-simplifications* old-simps))
		(t (setq *poly-simplifications* old-simps))))))
  ;;?? this was not in earlier version:
  (cond (resimplify (simplify-poly-simplifications)))
  ;;  (cond (try-conclusion (conclusionp)))  ;;moved to add-to-simps..
  *poly-simplifications*)


(defun seq-op (seqa seqb &optional (op #'-))
  (cond ((null seqa) seqb)
	((null seqb) seqa)
	((eq (car seqa) (car seqb))
	 (nconc (list (car seqa ) (funcall op (second seqa) (second seqb)))
		      (seq-op (cddr seqa) (cddr seqb) op)))
	((pointergp (car seqa) (car seqb))
	 (nconc (list (car seqa )  (second seqa))
		(seq-op (cddr seqa)  seqb) op))
	(t (nconc (list (car seqb )  (second seqb))
		  (seq-op  seqa (cddr seqb) op)))))
(defun seq-lessp (seqa seqb)
 (cond ((catch 'less (seq-op seqa seqb
			     #'(lambda (x y)
				 (cond ((> x y)(throw 'less t))))))
	nil)
       (t t)))

(defun seq-minus (seqa)
  (setq seqa (copy-list seqa))
  (loop for v on (cdr seqa) by #'cddr
	do (setf (first v) (- (first v))))
  seqa)
;;see other one above
;(defun simplify-poly-simplifications ( &aux hi new monom new-simps)
;  "This works but is not too quick"
;  (setq *poly-simplifications* (sort-grouped-list
;				 *poly-simplifications*
;				 2 '(lambda (x y)
;				      (cond ((equal x y) nil)
;					    (t (funcall *monomial-order-function* y x))))))
;  (cond (*show-grob*  (format t "~%Starting to simplify ..") (show-simps)))
;  (loop for (deg repl) on *poly-simplifications* by #'cddr
;	for i from 2 by 2
;        do
;	(setq new-simps nil)
;	(let ((*poly-simplifications* (append     ;;;nconc ok..
;					(firstn (- i 2) *poly-simplifications*)
;					(nthcdr i *poly-simplifications*))))
;	  (cond ((or (must-replacep (setq monom (convert-deg-sequence-to-monomial deg)))
;		     (must-replacep (num  repl)))
;		 ;;should use values of must-replacep to save time
;		 (setq new (pdifference  (num repl) (ptimes monom (denom repl))))
;		 (setq new (polysimp new))
;		 (setq new-simps (add-to-poly-simplifications new nil))
;		 (show-simps new-simps))))
;	(cond (new-simps (setq *poly-simplifications* new-simps)
;			 (return 'found-one))))
;  (cond (new-simps (simplify-poly-simplifications)))
;  *poly-simplifications*)

(defun basis_from_simps(&optional(simps *poly-simplifications*))
  (cons '(mlist)
	(loop for (u v) on simps by #'cddr
	       collecting (n- u v))))

(defun convert-poly-simplifications-to-macsyma-form (simps)
  (loop for (u v) on simps by #'cddr
	collecting (list '(mlist) (header-poly (convert-deg-sequence-to-monomial u))
			 (header-poly v)) into tem
	finally (return (cons '(mlist) tem))))


(defun overlap-degree (seqa seqb &aux answer temm  tem)
 (setq answer (+ (loop for v on seqa by #'cddr
	when (and (setq tem (get-altq (car v) seqb))
		  (not (zerop (setq temm (min tem (second v))))))
	summing (- (second v) temm)
	else summing (second v))
      (degree-of-deg-sequence seqb))))

(defun overlap (seqa seqb &aux tem)
  (loop for v on seqa by #'cddr
	when (and (setq tem (get-altq (car v) seqb))
		  (not (zerop (setq tem (min tem (second v))))))
	collecting (car v)
	and
	collecting tem))
(defun degree-of-deg-sequence (seq)
  (loop for v in (cdr seq)  by #'cddr
	summing v))

(defun replacement-sequences ()
  (loop for v in *poly-simplifications* by #'cddr collecting v))
(defun show-replacements ()
  (displa ($poly_replacements)))
(defun $polY_replacements()
  (loop for v in (replacements)
	collecting (header-poly (convert-deg-sequence-to-monomial v))
	into tem finally (return (cons '(mlist) tem))))

(defun degree-of-worst-monomial (poly)
  (degree-of-deg-sequence (find-main-monomial poly)))
(defun show-simps (&optional (simps *poly-simplifications*))
  (displa (convert-poly-simplifications-to-macsyma-form simps)))
(defvar  *overlaps-already-checked* nil)
(defvar *homogeneous-relations* nil)
(defvar *good-thru-degree* 0)

(defvar *timed-grobner-basis* nil)
(defun timedbasis (initial-time)
  (and  *timed-grobner-basis*
	(<  *timed-grobner-basis*
	    (- (get-internal-run-time) initial-time)
	    )
	(progn (if-verbose (format t "~%Grobner basis took too long, didn't finish")
	       (show-simps))

	       (throw 'took-too-long 'took-too-long))))

(defvar *show-complexity* t)

(defun check-overlaps (deg  &key reset  homogeneous (from-degree 1)
		       (add-to-simps t) (maybe-reset t)
		       (initial-time (get-internal-run-time))
		       &aux tem associator v)
  (cond (*homogeneous-relations*
	 (show *homogeneous-relations*)
	 (format t "Assuming homogeneous relations")
	 (setq homogeneous t)))

  (cond (reset (setq *overlaps-already-checked* nil))
	(maybe-reset
	 (cond ((y-or-n-p "Reset  *overlaps-already-checked* to nil")
		(setq  *overlaps-already-checked* nil)))))
  (loop for i from from-degree to deg do
       (block big-sue
	 (loop for subl on (nreverse (replacement-sequences))
	    do (setq v (car subl))
	      (loop for (w) on (cdr subl)
		 when
		   (and w
			(= (overlap-degree v w) i)
			(setq tem (overlap v w));;could check without consing
			(not (member (list v w) *overlaps-already-checked* :test #'equal)))
		 do
		   (timedbasis initial-time)
		   (setq associator (check-associative v tem w))
		   (push (list v w) *overlaps-already-checked* )
		   (cond ((rzerop associator)
			  (setq initial-time (+ initial-time 30))
			  (if-verbose (show (- (get-internal-run-time) initial-time))
				      (format t " zero associator, ")))
			 (add-to-simps
			  (add-to-poly-simplifications associator)
			  (cond (*show-complexity*
				 (terpri)
				 (loop for v in (cdr *poly-simplifications*) by #'cddr
				    do (format t " . ~a" (+ (pcomplexity (num v))
							    (pcomplexity (denom v)))))))
			  (conclusionp)
			  (cond ((null homogeneous)
				 (setq from-degree (min from-degree (degree-of-worst-monomial (car associator))))))
			  (check-overlaps deg :homogeneous homogeneous
					  :from-degree from-degree :initial-time initial-time
					  :add-to-simps add-to-simps :maybe-reset nil)
			  (return-from big-sue 'done))
			 (t (break 'found-non-associative))))
	    finally (return *poly-simplifications*)))
       (cond (homogeneous (setq from-degree i))))
  *poly-simplifications*)


(defun $polysimp (f &aux answer)
  (setq answer  (case (poly-type f)
			 (:number f)
			 (:polynomial (polysimp f))
			 (:rational-function  (ratquotient (polysimp  (num f))
							   (polysimp (denom f))))
			 (t nil)))
  (cond (answer
	 (header-poly answer))
	(t  (cond ((mbagp f)(cons (car f) (mapcar '$polysimp (cdr f))))
		  (t ($polysimp (new-rat ($totaldisrep f))))))))

(defun check-associative (a overlap b &aux answer simpa simpb)
  (when *show-grob*
    (format t "Checking overlap ~A for ~A ~A "(disrep-list overlap) (disrep-list a) (disrep-list b)))
  (let ((ma (convert-deg-sequence-to-monomial a))
	( mo (convert-deg-sequence-to-monomial overlap))
	(mb (convert-deg-sequence-to-monomial b)))
					;   (setq simpa (polysimp (cons ma 1)))
    (setq simpa  (get-altq a *poly-simplifications*))
    (setq simpb  (get-altq b *poly-simplifications*))
					;    (setq simpb (polysimp (cons mb 1)))

    (setq simpa (cons (ptimes (num simpa) (num (ratreduce mb mo))) (denom simpa)))
    (setq simpb (cons (ptimes (num simpb) (num (ratreduce ma mo))) (denom simpb)))

    ;;everything you're just shifting a numerator there
    (cond ((equal simpa simpb)
	   (princ ".")
	   (cond (*show-grob*   (format t"..The overlap is associative")(rzero)))
	   (rzero))
	  (t
	   (setq answer (ratdifference simpa simpb))
	   (cond ((null *simplify-rhs* ) (setq plain-answer answer)
		  (setq answer (nred (polysimp (function-numerator answer))
				     (function-denominator answer)))))
	   (cond ((null *simplify-rhs*)
		  (cond ((new-zerop answer))
			(t (setq answer plain-answer)))))
	   (cond (*show-grob*
		  (format t "~%The difference  is ..")(sh answer)))
	   (cond ((new-zerop answer) (princ ".")(rzero))
		 (t answer))))))

(defvar *simplify-simplifications* nil)

(defun reverse-z-order-sequence (seqa seqb)
  (and (not (equal seqa seqb))(not (z-order-sequence seqb seqa))))

(defun z-order-sequence (seqa seqb)
   (loop for v on seqa by #'cddr
	 for w on seqb by #'cddr
	 when (or (< (symbol-value (car v)) (symbol-value (car w)))
		  (< (second v)(second w)))
	 do (return nil)
	 finally (return (not (equal v w)))))

(defun n** (&rest l)
  (case (length l)
    (0 1)
    (1 (car l))
    (2 (n* (car l) (cadr l)))
    (t (n* (car l) (apply 'n** (cdr l))))))

;(setq me #$[x,x^2,z,z^2,z^2*x,z^2*x*t]$)
(defun xyz-order-sequence (seqa seqb)
  " so x<x^2<z<z^2<z^2*x<z^2*x*t the worst monomial  is the most main monomial ie
  highest degree in z then highest in y etc. "
  (loop for (var deg) on seqb by #'cddr while seqa
     do
       (cond ((eq (car seqa) var)
	      (cond ((> (second seqa) deg) (return nil))
		    ((< (second seqa) deg) (return t))
		    (t (setq seqa (cddr seqa)))))
	     ((pointergp (car seqa) var) (return nil))
	     (t (return t)))
     finally (cond ((null seqa) (return (and seqb)))
		   (t (return nil)))))


(defun degree-of-sequence (seq)
  (loop for v on (cdr seq) by #'cddr
	summing (car v)))
(defun xyz-and-degree (seqa seqb &aux (d1 (degree-of-sequence seqa))
			  (d2 (degree-of-sequence seqb)))
  (cond ((eq d1 d2)(xyz-order-sequence seqa seqb))
	((< d1 d2) t)
	(t nil)))

(defun remove-0 (lis)
  (loop for (var deg) on lis by #'cddr
	unless (zerop deg)
	collecting var and collecting deg))

;(defun xyz-order-sequence (seqa seqb)
;  " so x<x^2<z<z^2<z^2*x<z^2*x*t the worst monomial  is the most main monomial ie
;  highest degree in z then highest in y etc. "
;  (loop for v on seqa by #'cddr
;	for w on seqb by #'cddr
;	do
;	(cond ((eq (car v) (car w))
;	       (cond ((eq (second w)(second v))nil)
;		     ((< (second v)(second w))(return t))
;		     (t (return nil))))
;	      ((< (symbol-value (car v)) (symbol-value  (car w)))(return t))
;	      (t (return nil)))))

(defun reverse-xyz-order-sequence (seqa seqb)
  (and (not (equal seqa seqb))(not (xyz-order-sequence seqb seqa))))

;(defun polysimp (poly &aux (changes t) changed tem repl (replaced-th 0))
;  (cond ((polynomialp poly)(setq poly (cons poly 1)))
;	((rational-functionp poly) nil)
;	(t (setq poly (new-rat poly))))
;  (cond ((constant-functionp poly) (values  poly nil))
;	(t
;	 (loop while changes
;	       do (setq changes nil)
;	       (loop for v on *poly-simplifications* by #'cddr
;		     for i from 0 by 2
;		     when (setq tem (part-above-degree (num poly) (car v)))
;		     do (setq changes t changed t)
;		     (cond ((< i replaced-th)
;			    (format t "~%Replacing by earlier ~A before ~A"
;				  (disrep-list  (nth i *poly-simplifications* ))
;				 (disrep-list   (nth replaced-th *poly-simplifications* ))
;				 (break t))))
;
;		      (setq replaced-th i)
;
;		     (setq repl (replace-monomial-rat  poly (second v) (car v) tem ))
;		     (cond ((eq (num repl) 0)(setq changes nil)))
;		     (setq poly repl)
;		     (return 'changed))
;	       finally (return 'done))
;	 (values poly changed))))

(defun zero-modulo-simplifications (poly )
  (pzerop (polysimp-ignore-denominator poly)))


(defun polysimp-ignore-denominator  (poly &aux (changes t) *reduce* changed tem repl)
  "Returns something G where G*unit= poly modulo simplifications"
  (setq poly
	(cond ((polynomialp poly)(cons poly 1))
	      ((rational-functionp poly) (cons (num poly ) 1))
	      (t  (cons (num (new-rat poly)) 1))))
  (cond ((constant-functionp poly) nil)
	(t
	 (loop while changes
	       do (setq changes nil)
	       (loop for v on *poly-simplifications* by #'cddr
		     when (setq tem (part-above-degree (num poly) (car v)))
		     do (setq changes t changed t)
		     (setq repl (replace-monomial-rat  poly (second v) (car v) tem ))
		     (cond ((eq (num repl) 0)(setq changes nil)))
		     (setq poly (cons (num repl) 1))
		     (return 'changed))
	       finally (return 'done))))
  (function-numerator poly))

(defun polysimp (poly &aux (changes t) changed tem repl)
  (cond ((polynomialp poly)(setq poly (cons poly 1)))
	((rational-functionp poly) nil)
	(t (fsignal "bad-poly ~a" poly) (setq poly (new-rat poly))))
  (cond ((constant-functionp poly)
	 (cond ((get-altq 1 *poly-simplifications*)(values 0 nil))
	       (t (values  poly nil))))
	(t
	 (loop while changes
	       do (setq changes nil)
	       (loop for v on *poly-simplifications* by #'cddr
		     when (setq tem (part-above-degree (num poly) (car v)))
		     do (setq changes t changed t)
		     (setq repl (replace-monomial-rat  poly (second v) (car v) tem ))
		     (cond ((eq (num repl) 0)(setq changes nil)))
		     (setq poly repl)
		     (return 'changed))
	       finally (return 'done))
	 (values poly changed))))
;(defun polysimp (poly &aux (changes t) changed tem repl)
;  (cond ((polynomialp poly)(setq poly (cons poly 1)))
;	((rational-functionp poly) nil)
;	(t (setq poly (new-rat poly))))
;  (cond ((constant-functionp poly) (values  poly nil))
;	(t
;	 (loop while changes
;	       do (setq changes nil)
;	       (loop for v on *poly-simplifications* by #'cddr
;		     when (setq tem (part-above-degree (num poly) (car v)))
;		     do (setq changes t changed t)
;		     (setq repl (replace-monomial (num poly) (second v) (car v) tem ))
;		     (cond ((eq (num repl) 0)(setq changes nil)))
;		     (setq poly (ratreduce (num repl) (ptimes (denom poly) (denom repl))))
;		     (return 'changed))
;	       finally (return 'done))
;	 (values poly changed))))
;
;(defun polysimp (poly &aux (changes t) tem)
;  (cond ((polynomialp poly)nil)
;	((rational-functionp poly) nil)
;	(t (setq poly (new-rat poly))))
;  (loop while changes
;	do (setq changes nil)
;	(loop for v on *poly-simplifications* by #'cddr
;	      when (setq tem (part-above-degree poly (car v)))
;	      do (setq changes t)
;	      (setq poly (replace-monomial poly (second v) (car v)))
;	      (return 'changed))
;	finally (return poly)))


(DEFUN CONVERT-MONOMIAL-TO-DEG-SEQUENCE (MONOM &AUX TEM)
    (COND ((OR (ATOM MONOM)
	       (NULL MONOM))
	   NIL)
	  (T
	   (SETQ TEM (CONVERT-MONOMIAL-TO-DEG-SEQUENCE (THIRD MONOM)))
	   (APPEND (LIST (CAR MONOM) (SECOND MONOM)) TEM))))

;
;;;could introduce *remaining-poly-simplifications* which is set by rat-polysimp
;;;initially  to all *poly-simplifications* and then gradually reduced by rat-polysimp-once
;(defun rat-polysimp-once (poly &aux new-denom tem)
;  (cond ((not (numberp poly))
;	 (loop for (deg-seq repl) on *poly-simplifications* by #'cddr
;	       when (setq tem (part-above-degree poly deg-seq))
;	       do
;	       (setq new-denom (denom repl))
;	       (setq poly (replace-monomial poly  repl deg-seq))
;		     (return 'changed))
;	 (values poly new-denom))
;	(t poly)))
; ;;new-denom will be nil unless there was a changed
;
;
;;;;we just use polysimp now.  it accepts a rat or non rat f'n checking...
;
;(defun rat-polysimp (rat-poly &aux (changes t)  a b changed-since-reduction
;		     changed-at-least-once answer)
;;  (format t "~%beginning to rat-polysimp..") (sh rat-poly)
;  (setq a (num rat-poly) b (denom rat-poly))
;  (setq answer
;	(loop while changes
;	      do (setq changes nil)
;	      (multiple-value-bind (new-poly changed-denom)
;		  (rat-polysimp-once  a)
;		(cond (changed-denom (setq changes t)(setq changed-since-reduction t)
;				     (setq changed-at-least-once t)
;				     (setq b (ptimes changed-denom b))
;				     (setq a new-poly)
;				     (cond ((numberp b) nil)
;					   (t (setq rat-poly (ratreduce a b))
;					      (setq changed-since-reduction nil)
;					      (setq a (num rat-poly)  b (denom rat-poly)))))))
;	      finally(cond (changed-since-reduction (return (ratreduce a b)))
;			   (t (return rat-poly)))))
;;  (format t "~%ending rat-polysimp with answer...")  (sh answer)
;  (values answer changed-at-least-once))

(defun $current_grobner_basis (&optional (simps *poly-simplifications*))
  (loop for (seq repl) on simps by #'cddr
	when (numberp seq) do (return '((mlist) 1))
	collecting (header-poly
		     (cons  (pdifference
			      (num repl)
			      (ptimes (convert-deg-sequence-to-monomial seq)
				      (denom repl)))
			    1))
	into tem
	finally (return (cons '(mlist) tem))))

(defun $grobner_basis (ideal &optional no-concl homogeneous (upto 100))
  (cond (no-concl (setq *conclusion* nil)))
  (check-arg ideal $listp "macsyma list")

  ($set_up_poly_simplifications ideal)
;  (show-simps)
  (conclusionp)
  (check-overlaps upto :reset t :homogeneous homogeneous)
  ($current_grobner_basis))

(defun delete-pair (first-item a-list )
  "deletes first occurrence"
  (loop
     for w on a-list by #'cddr
     when (not (equal (car w) first-item))
     collecting (car w) into tem
     and
     collecting (second w) into tem
     else do (return  (nconc tem (cddr w)))))

(defun seq-subset (seqa seqb &aux answer)
  (setq answer (catch 'not-in
		 (seq-op seqa seqb #'(lambda (.a. b) (cond ( (<= .a. b)
							    (- b .a.))
							   (t (throw 'not-in 'no)))))))
  (cond ((eq answer 'no) nil)
	(t answer)))



(defvar *old-genvar* nil)

;;this is dangerous for some reason...
(defun substitute-real-symbols-for-genvar (&optional a-list)
  (cond ((null a-list)
	(setq a-list (loop for v in *genvar* collecting
			   (intern (string-trim "$" (get v 'disrep)))))))
  (check-arg a-list  (eq (length a-list) (length *genvar*)) "not right length")
  (loop for v in *genvar*
	for w in a-list
	do
	(setf (symbol-value w) (symbol-value v))
	(setf (symbol-plist w) (symbol-plist v))
	finally (setq *genvar* a-list *old-genvar* *genvar*)))

(defun $generate_matrix (funct rows cols)
  (loop for i from 1 to rows
	collecting
	(loop for j from 1 to cols
	      collecting (mfuncall funct i j)
	      into tem
	      finally (return (cons '(mlist) tem)))
	into temm
	finally (return (cons '($matrix) temm))))

(defun $nilpotent ($f &aux (old-simps *poly-simplifications*) )
 (unwind-protect
   (let (*conclusion* (me (sub* (mul* $f '$ggggg) 1)) mee)
    (setq mee (new-rat me))
    (add-to-poly-simplifications mee)
    (check-overlaps 10 :add-to-simps t :reset t)
      (cond ((eq 0 ($polysimp '$ggggg)) t)
	    (t nil)))
      (setq *poly-simplifications* old-simps)))

(defun rat-variable( a-var)
  (loop for v in *varlist*
	for w in *genvar*
	when (eq v a-var)
	do (return (list w 1 1))
	finally (show a-var  v w)(new-rat (mul* a-var 2))
	(return (rat-variable a-var))))

(defun must-replace-monom (monom repl)
  (loop for seq in repl
	when (occurs-in monom seq)
	do (return t)))

(defun sort-list-of-monomials (a-list &aux grouped)
 (setq grouped
       (loop for v in a-list collecting (convert-monomial-to-deg-sequence v)
	collecting v))
       (setq grouped (sort-grouped-list grouped 2 *monomial-order-function*))
       (loop for w in (cdr grouped) by #'cddr collecting w))
;
;(user:defremember grobner-monomials(variables n &optional (replacements
;							     (replacements))
;					       reset
;			  )
;  "Note that in this code the replacements are mereley for the remember feature,
; and *poly-simplifications* are the ones used for the must-replacep.  The version
; below is similar but just uses occurs-in instead of must-replacep"
;  (let (rat-vars tem vv)
;  (check-arg variables $listp "macsyma list")
;  (cond ((= n 0)(cond ((member 1 replacements :test #'eq) nil)
;		      (t (list 1))))
;	((= n 1)
;	 (setq rat-vars (loop for v in (cdr variables)
;			      do (setq vv (rat-variable v))
;			      when (not (must-replacep  vv))
;			      collecting vv ))
;	 (setq rat-vars (sort rat-vars #'(lambda (x y) (pointergp (car x) (car  y)))))
;	 rat-vars)
;	(t (loop for w in (grobner-monomials variables (1- n) replacements reset)
;		 appending
;		 (loop for uu in (grobner-monomials variables 1 replacements reset)
;		 when (and
;			(>= (symbol-value (car uu))
;			    (symbol-value (car w)))
;			(not  (must-replacep (setq tem (ptimes w uu)) )))
;		 collecting tem))))))


(defun $poly_monomial_dimensions (variables to-deg &aux tem)
  (loop for i from 0 to to-deg
	do (format t "~%There are ~A independent monomials in degree ~A"
		   (setq tem (length (grobner-monomials variables i))) i)
		summing tem into tot
	finally (format t "~% The total is ~A." tot)(return tot)))

(defremember grobner-bi-monomials(variables-y variables-x n-y n-x &optional (replacements
							     (replacements))
					       reset
			  )
  (cond (reset (clear-memory-function 'grobner-monomials)))
  (let ( tem )
  (check-arg variables-y $listp "macsyma list")
  (cond ((= n-x 0)
	 (grobner-monomials variables-y n-y replacements reset))
	((= n-y 0)
	 (grobner-monomials variables-x n-x replacements reset))
	(t (loop for w in (grobner-bi-monomials variables-y variables-x  (1- n-y) n-x replacements reset)
		 appending
		 (loop for uu in (grobner-monomials variables-y 1 replacements reset)
		 when (and
			(>= (symbol-value (car uu))
			    (symbol-value (car w)))
			(not  (must-replace-monom (setq tem (ptimes w uu)) replacements)))
		 collecting tem))))))

(defremember grobner-monomials(variables n &optional (replacements
							     (replacements))
					       reset
			  )
  (cond (reset (clear-memory-function 'grobner-monomials)))
  (let (rat-vars tem vv)
  (check-arg variables $listp "macsyma list")
  (cond ((= n 0)(cond ((member 1 replacements :test #'eq) nil)
		      (t (list 1))))
	((= n 1)
	 (setq rat-vars (loop for v in (cdr variables)
			      do (setq vv (rat-variable v))
			      when (not (must-replace-monom  vv replacements))
			      collecting vv ))
	 (setq rat-vars (sort rat-vars #'(lambda (x y) (pointergp (car x) (car  y)))))
	 rat-vars)
	(t (loop for w in (grobner-monomials variables (1- n) replacements reset)
		 appending
		 (loop for uu in (grobner-monomials variables 1 replacements reset)
		 when (and
			(>= (symbol-value (car uu))
			    (symbol-value (car w)))
			(not  (must-replace-monom (setq tem (ptimes w uu)) replacements)))
		 collecting tem))))))

(defun $grobner_monomials (variables n &optional (replacements
							     (replacements))
					       reset)
  (cons '(Mlist) (mapcar 'new-disrep (grobner-monomials variables n replacements reset))))


(defun $list_relations (&rest l)
  (loop for v in l
	when ($listp v)
	appending (cdr (apply '$list_relations (cdr v))) into tem
	else
	 when (equal (caar v) 'mequal)
	 collecting (sub* (second v) (third v))into tem
	  else collecting v into tem
	  finally (return (cons '(mlist) tem))))

(defun rat-degree (poly var)
  (cond ((numberp poly)0)
	((eq (car poly) var)(second poly))
	(t (loop for (deg cof) on (cdr poly) by #'cddr
		 maximize (rat-degree cof var)))))

(defun rational-sub  (poly var new-denom &aux numerator)
  "to do y-->y/u where u is earlier in the order and u is a poly"
  (let ((deg (rat-degree poly var)))
    (setq poly (copy-tree poly))
    (setq numerator (rational-sub1 poly var new-denom deg))
    (values poly new-denom deg)))

(defun rational-sub1 (poly var new-denom degree)
  (cond ((atom poly)(setq poly (ptimes poly (pexpt new-denom degree) )))
	((eq (car poly) var)
	 (loop for tail  on (cdr poly) by #'cddr
		for (deg cof) on (cdr poly) by #'cddr
		do (setf (second tail)
			 (ptimes cof (pexpt new-denom (- degree deg))))))
	(t (loop for tail on (cdr poly) by #'cddr
		  do (setf (second tail)
			   (rational-sub1 (second tail) var new-denom degree)))))
  Poly)

(defun $te (poly var repl)
  (header-poly (rational-sub (num (new-rat poly)) (caar (new-rat var)) (num (new-rat repl)))))

(defun $te (poly var)
  (rat-degree(num (new-rat poly )) (caar (new-rat var))))

(defun $Contract_to_variables (a-list upto-variable &optional (before-var nil) &aux tem)
   (check-arg a-list $listp "Macsyma list")
   (cond ((null before-var)(setq before-var (+ (symbol-value (get-genvar upto-variable)) .5)))
	 (t (setq before-var  (symbol-value  (get-genvar before-var)))))
   (loop for v in (cdr a-list)
	 do
	 (setq tem nil)
	 (case (poly-type v)
	   (:polynomial (setq tem (car v)))
	   (:rational-function (setq tem (caar v)))
	   (:$rat (setq tem (caadr v))))
	 (cond ((> before-var (symbol-value tem))
		(setq tem v))
	       (t (setq tem nil)))
	 when tem
	 collecting tem into llist
	 finally (return (cons '(mlist ) llist))))
;
;(defun st-rat (x &aux answer)
;  "Returns a a number or cre polynomial or rational-function corresponding to x
;where x may be a polynomial"
;  (cond ((and (listp x) (mbagp x))
;	 (cons (car x) (mapcar 'st-rat (cdr x)))))
;  (cond ((case (poly-type x)
;	   (:number t)
;	   (:polynomial (member (car x) *genvar* :test #'eq))
;	   (:rational-function (member (caar x) *genvar* :test #'eq))
;	   (:$rat (member (caadr x) *genvar* :test #'eq)(setq x (cdr x)))
;	   (t nil))
;	 (cond ((numberp x) x)
;	       ((and (rational-functionp x)(eq (denom x) 1))(num x))
;	       (t x)))
;	(t (cond ((member (poly-type x) '(:rational-function :polynomial :number) :test #'eq)
;		  (merror "not in standard rat form ~A " x))
;		 (t (setq answer (new-rat x))(show (denom answer))
;		    (cond ((and (rational-functionp answer)
;				(eq (denom answer) 1)) (num answer))
;			  (t answer)))))))

(defun st-rat (x)
  (cond ((and (listp x)(listp (car x)) (mbagp x))
	  (mapcar 'st-rat1 (cdr x)))
	(t (st-rat1 x))))

(defun st-rat1 (x)
  (cond ((polynomialp x) x)
	((rational-functionp x) (cond ((eq (denom x) 1)(num x))
				      (t x)))
	(($ratp x) (cond ((numberp (num (cdr x))) (st-rat (cdr x)))
			 ((member  (caadr x) *genvar* :test #'eq)(st-rat (cdr x)))
			 (t (st-rat ($totaldisrep x)))))
	(t (st-rat (new-rat x)))))

(defun get-genvar (var)
  (caar (new-rat var)))

;;Thus  h is in ideal1 ^R ideal2 iff h is in ideal1*zz+ideal2*(1-zz)
;;the ==> direction is trivial and
;; for <==  h(x,y)=i1zz+i2(1-zz) and h is free of zz then i1=i2 and h=i2.
(defun $ideal_intersection (ideal1 ideal2)
   "Returns the grobner basis of the intersection of the ideals generated by the
  two lists ideal1 and ideal2"
   (check-arg ideal1 '$listp "macsyma list")
  (let ((new-var (intern "ZZZZZ1"))
	gen	basis)
    (setq gen (get-genvar new-var))
    (setq basis (append
		  (loop for w in (cdr ideal1)
			collecting (n* new-var w))
		  (loop for w in (cdr ideal2)
			collecting (n* (n- 1 new-var ) w))))
    (setq basis ($grobner_basis (cons '(mlist)basis)))
    ($contract_to_variables basis nil new-var)))

(defun $grobner_basis_of_slice (slices original-relations &aux relats)
  (check-arg slices '$listp "macsyma list")
  (check-arg original-relations '$listp "macsyma list")
  (setq relats
	(append
	  (loop for u in (cdr slices )
		collecting (bring-to-left-side u))
	  (cdr original-relations)))
  (displa (setq relats (cons '(mlist) relats)))
  ($grobner_basis  relats))

;
;(DEFUN TE ($X)
;    (MEVAL `((MPLUS) $X 3)))
;
;
;(defmacro vb (&rest l)
;  (loop for v in l
;	collecting (list 'eval? v) into tem
;    finally (return (cons 'list  tem))))
;
;
;(defmacro eval? (ssymbol)
;  `(cond ( ,(list 'variable-boundp ssymbol) ,ssymbol)
;	 (t (print 'special) ',ssymbol)))
;(defmacro teee (x)
;  (funcall 'variable-boundp x)
;  '(cons 1 1))
;
;
;(hii 3)
;(defun te (b)
;  (teee b)
;  (vb b x))
;
;si:
;(defun describe-region (region &key octal  (stream standard-output) &aux bits)
;  (cond (octal (let ((sstring  (format nil "~A" region))
;		     (ibase #10r8))
;		 (setq region (read-from-string sstring)))))
;
;	     (SETQ BITS (REGION-BITS REGION))
;	     (FORMAT stream
;		     "~%  Region #~O: Origin ~:O, Length ~:O, Free ~:O, GC ~:O, Type ~A ~A, ~[~;Read only, ~]~[~;Temp, ~]~@[Swapin ~O, ~]~[NoScav~;Scav~]~[~; Safeguard~]~[~; NoCons~]~[~; Ephemeral~]~:[ Level=~D~]~%"
;		     REGION (REGION-ORIGIN REGION) (REGION-LENGTH REGION)
;		     (REGION-FREE-POINTER REGION) (REGION-GC-POINTER REGION)
;		     (NTH (LDB %%REGION-REPRESENTATION-TYPE BITS)
;			  '(LIST STRUC "REP=2" "REP=3"))
;		     (OR (NTH (LDB %%REGION-SPACE-TYPE BITS)
;			      '(FREE OLD NEW COPY STATIC STACK))
;			 (FORMAT NIL "TYPE=~O" (LDB %%REGION-SPACE-TYPE BITS)))
;		     (LDB %%REGION-READ-ONLY BITS)
;		     (LDB %%REGION-TEMPORARY BITS)
;		     (AND (PLUSP (LDB %%REGION-SWAPIN-QUANTUM BITS))
;			  (LDB %%REGION-SWAPIN-QUANTUM BITS))
;                     (LDB %%REGION-SCAVENGE-ENABLE BITS)
;		     (LDB %%REGION-SAFEGUARD BITS)
;		     (LDB %%REGION-NO-CONS BITS)
;		     (LDB %%REGION-EPHEMERAL BITS)
;		     (ZEROP (LDB %%REGION-LEVEL BITS))
;		     (LDB %%REGION-LEVEL BITS)))
;
;
;si:
;(DEFUN DESCRIBE-AREA (AREA &key
;		      (stream standard-output) &AUX  LENGTH USED N-REGIONS)
;  (AND (NUMBERP AREA) (SETQ AREA (AREA-NAME AREA)))
;  (DO AREA-NUMBER 0 (1+ AREA-NUMBER) (= AREA-NUMBER (length (the lisp::array #'AREA-NAME)))
;    (COND ((EQ AREA (AREA-NAME AREA-NUMBER))
;	   (MULTIPLE-VALUE (LENGTH USED N-REGIONS) (ROOM-GET-AREA-LENGTH-USED AREA-NUMBER))
;	   (FORMAT stream "~%Area #~O: ~S has ~D region~P, max size ~:O, region size ~:O (octal):~%"
;		     AREA-NUMBER AREA N-REGIONS N-REGIONS
;		     (AREA-MAXIMUM-SIZE AREA-NUMBER) (AREA-REGION-SIZE AREA-NUMBER))
;	   (DO ((REGION (AREA-REGION-LIST AREA-NUMBER) (REGION-LIST-THREAD REGION))
;		(BITS))
;	       ((MINUSP REGION))
;	     (SETQ BITS (REGION-BITS REGION))
;	     (FORMAT stream "  Region #~O: Origin ~:O, Length ~:O, Free ~:O, GC ~:O, Type ~A ~A, ~[~;Read only, ~]~[~;Temp, ~]~@[Swapin ~O, ~]~[NoScav~;Scav~]~[~; Safeguard~]~[~; NoCons~]~[~; Ephemeral~]~:[ Level=~D~]~%"
;		     REGION (REGION-ORIGIN REGION) (REGION-LENGTH REGION)
;		     (REGION-FREE-POINTER REGION) (REGION-GC-POINTER REGION)
;		     (NTH (LDB %%REGION-REPRESENTATION-TYPE BITS)
;			  '(LIST STRUC "REP=2" "REP=3"))
;		     (OR (NTH (LDB %%REGION-SPACE-TYPE BITS)
;			      '(FREE OLD NEW COPY STATIC STACK))
;			 (FORMAT NIL "TYPE=~O" (LDB %%REGION-SPACE-TYPE BITS)))
;		     (LDB %%REGION-READ-ONLY BITS)
;		     (LDB %%REGION-TEMPORARY BITS)
;		     (AND (PLUSP (LDB %%REGION-SWAPIN-QUANTUM BITS))
;			  (LDB %%REGION-SWAPIN-QUANTUM BITS))
;                     (LDB %%REGION-SCAVENGE-ENABLE BITS)
;		     (LDB %%REGION-SAFEGUARD BITS)
;		     (LDB %%REGION-NO-CONS BITS)
;		     (LDB %%REGION-EPHEMERAL BITS)
;		     (ZEROP (LDB %%REGION-LEVEL BITS))
;		     (LDB %%REGION-LEVEL BITS)))
;	   (RETURN t)))))
;
;si:
;(defvar *bad-length* nil)
;si:
;(DEFUN RESET-AREA (AREA &OPTIONAL FREE-REGIONS &aux (reset t) nex)
;  (DO ((REGION (AREA-REGION-LIST AREA) NEXT-REGION)
;       (NEXT-REGION))
;      ((MINUSP REGION))
;    (SETQ NEXT-REGION (REGION-LIST-THREAD REGION))
;    (cond
;      ((> (region-free-pointer region) (region-length region))
;       (setq reset nil)
;       (push
;	 (List
;	   (time:print-current-time nil)
;	   (region-free-pointer region)
;	   (region-length region)
;	   (describe-region region :stream nil)
;	   (describe-region (%region-number (setq nex (+ (region-origin region)
;							 (region-free-pointer region))))
;			    :stream nil)
;	   (describe-area 31 :stream nil)
;	   region (region-bits region) area) *Bad-length*)
;       (beep)
;       (format tv:who-line-documentation-window
;	       "~A****The region too long***" (describe-region region :stream nil))
;       (with-open-file
;	 (str  "isaac:>wfs>bad-length" (list :out))
;	 (describe-area (%area-number nex) :stream str)
;	 (describe-area 31 :stream str)
;	 (format str "~A" *bad-length*))
;       (beep))))
;  (cond (reset
;	 (without-interrupts
;	   (prog1
;	 (DO ((REGION (AREA-REGION-LIST AREA) NEXT-REGION)
;	      (NEXT-REGION))
;	     ((MINUSP REGION))
;	   (SETQ NEXT-REGION (REGION-LIST-THREAD REGION))
;	   (GC-RESET-FREE-POINTER REGION 0)
;	   (COND (FREE-REGIONS
;		  (SETF (AREA-REGION-LIST AREA) NEXT-REGION)
;		  (%GC-FREE-REGION REGION))))
;	 (cond ((eq area macsyma:*temporary-polynomial-area*)
;		(Si:clear-cons-caches))))
;	 ))))

(defun display (expr)
  (cond ((atom expr)(displa expr))
	((or (polynomialp expr)(rational-functionp expr))(sh expr))
	((or (polynomialp (car expr))(rational-functionp (car expr)))
	 (loop for v in expr do (display v)))
	(t (displa expr))))


(defun ldata-simplifications-write (ldata &key (open-g 1)
				    (pathname "haskell:>wfs>answer.lisp") &aux answ)
  (setq answ (ldata-simplifications ldata :open-g open-g))
  (with-open-file (st pathname :direction :output)
    (let ((*standard-output* st) *print-radix*)
      (for-editor (des answ))
      (format st "~%(setq (answ (rerat '~A)))" answ))))
