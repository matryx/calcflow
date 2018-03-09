;;; -*-  mode: lisp; package: cl-maxima; syntax: common-lisp ;base: 10; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defstruct (sparse-matrix :named (:conc-name sp-))
  rows
  number-of-rows
  type-of-entries
  current-row
  current-row-number
  current-row-length
  pivot-row
  pivot-row-number
  characteristic
  inverse-array
  special-inverse
  special-plus
  (minimum-size-to-grow 1)
  (allow-reorder t)
  last-good-row
  columns-used-to-pivot ;;key is the column ==>row number
  column-used-in-row  ;;(aref column-used-in-row row-num)
  ;;==>column for row row-num
  list-of-all-columns-occurring
  pivot-entry
  (pivot-test-list '(unit  min))
  row-number-before-swap
  special-multiply
  transpose
  (reduced nil)
  sort-pivot
  (sign-of-row-permutation 1)
  current-column-above-pivot-row-number
  columns-with-no-pivot ;;list of them
  rows-with-no-pivot  ;;has a 1 in slot i --> no pivot in row i
  solutions
  constants-column-number
  special-solution  ;; a sparse-matrix row which dots with rows
  (constants-column nil))			;to give the constants-column.

(defstruct (solution-row :named (:conc-name solution-row-))
  number
  data)


(defmacro row-length (arow)
  `(array-dimension-n 1 ,arow))

;(defmacro set-keywords (listt key-list &aux body)
;  (let ((pack package))
;  (loop for v in key-list
;	do(show pack)
;	(show v)
;	collecting `(, v (setq ,(intern-local  v pack) (car (setq key (cdr key)))))
;        into tem
;	finally (setq body
;		      (append tem (list '(otherwise
;					   (merror "unrecognized key word"))))))
;  `(do ((key (cdr ,listt) (cdr key)))
;       ((null key))
;     (case (car key)
;       ,@ body))))
;
;(defun foo ()(set-keywords '(:hi 4 :bye 5) (:hiii :hi :bye :extra :byye :there)))
;(DO ((KEY (CDR '(:HI 4)) (CDR KEY)))
;    ((NULL KEY))
;  (CASE (CAR KEY)
;           (:HI (SETQ HI (CAR (SETQ KEY (CDR KEY)))))
;           (:BYYE (SETQ :BYYE (CAR (SETQ KEY (CDR KEY)))))
;           (:THERE (SETQ THERE (CAR (SETQ KEY (CDR KEY)))))
;           (OTHERWISE (FERROR "unrecognized key word"))))

(deff sp-rational-quotient  #'/)

(defmacro without-double-evaluation (list-of-forms &body body &aux repl)
  (setq repl (loop for v in list-of-forms
		when (atom v) nconc nil
		else collecting (list (gensym) v)))
  (cond ((> (length body) 1)
	 (setq body `(progn ,@body)))
	(t (setq body (car body))))
  (cond (repl
	 `(let ,repl
	    ,(sublis (loop for u in repl collecting (cons (second u) (car u)))
		     body :test 'equal)))
	(t `,body)))

;;this is really like vector-push-extend except it may speed up later references.
(defmacro array-push-extend-replace (aarray data &key amount-to-grow replace &aux forms)
  (declare (ignore replace))
  `(without-double-evaluation (,data)
     (cond ((vector-push ,data ,aarray))
	   (t (let ((.new. (adjust-array ,aarray (+ ,(or amount-to-grow 100) (array-total-size ,aarray))
					 :fill-pointer (fill-pointer ,aarray))))
		(vector-push ,data .new.),@ forms)))))

;(defun sp-add* (&rest llist)
;  (let ((varlist))
;    (apply 'add*  llist)))
;
;(defun sp-minus* (a)
;  (let ((varlist))
;    (simplifya (list '(mminus) a) nil)))
;
;(defun sp-sub* (a b )
;  (sp-add* a (sp-minus* b)))
;
;(defun sp-mul* (&rest a-list)
;  (let ((varlist ))
;    (cond ((< (length a-list) 3)(apply 'mul* a-list))
;	  (t (sp-mul* (car a-list) (apply 'sp-mul* (cdr a-list)))))))
;
;(defun sp-div* (a b)
;  (let ((varlist))
;   (simplifya  (list '(mquotient) a b) nil)))
;;see polyc

(defmacro sp-number-of-rows (self)
  `(fill-pointer (sp-rows ,self)))

;(defun make-one-dimensional (aarray &aux ans)
;  (cond ((eql (array-#-dims aarray) 1) aarray)
;	(t (setq ans (make-array (apply '* (array-dimensions aarray)) :fill-pointer 0 :adjustable t))
;	   (loop for i below (row-length aarray)
;		 when (aref aarray i 0)
;		 do
;		 (array-push-extend-replace ans (aref aarray i 0))
;		 (array-push-extend-replace ans (aref aarray i 1)))
;	   ans)))

(defmacro with-characteristic (&body body &aux body1 body2 body3)
  (setq body1 (sublis  '((special-times . *) (special-plus  .  +)) body))
  (setq body2 (sublis '((special-times . finite-characteristic-times)
			(special-plus . finite-characteristic-plus)) body))
  (setq body3 (sublis '((special-times . sp-mul*)
			(special-plus . sp-add*)) body))
  `(cond ((equal (sp-type-of-entries sp-mat) ':any-macsyma) ,@ body3)
	  ((zerop (sp-characteristic sp-mat)) ,@ body1)
	 (t (let ((.characteristic. (sp-characteristic sp-mat)))
	      ,@ body2))))

(defmacro with-once-only (variables &body body1 &aux  ll  var reset)
  "This macro is to save re-evaluation of instance variables again and
   again. It unwind protects the resetting of variables in case of throws etc.
   Variables may be generalized"
; (setq body1 (cdr body))
; (setq variables (car body))
  (loop for u in variables
	when (appears-in  body1 u)
	do
	(setq ll (cons (setq var (list (gensym) u)) ll))
	(setq body1 (subst (car var) (cadr var) body1))
	(setq reset (append reset (list `(setf ,@(reverse var))))))
  (cond (ll  `(let (,@ ll)
		(unwind-protect   (progn ,@ body1) ,@ reset)))
	(t   `(progn ,@  body1))))

;;(with-once-only ((sp-rows sp-mat) hi)
;;	(setq hi 4)	(+ 1 2) 3)
;;((LAMBDA (#:G2972)
;;     (UNWIND-PROTECT (PROGN (SETQ #:G2972 4)
;;                            (+ 1 2)
;;                            3)
;;                     (SETF HI #:G2972)))
;; HI)

(defmacro finite-characteristic-times (&rest l)
  `(mod (* ,@l) .characteristic.))

(defmacro finite-characteristic-plus (&rest l)
  `(mod (+ ,@l) .characteristic.))

(defmacro special-minus (a) `(special-times -1 ,a))

(defmacro special-times (&rest l)
  `(cond ((equal (sp-type-of-entries sp-mat) ':any-macsyma)(sp-mul*  ,@ l))
	((zerop (sp-characteristic sp-mat))(* ,@ l))
	(t (mod (* ,@ l) (sp-characteristic sp-mat)))))

(defmacro special-plus (&rest l)
  `(cond ((eq (sp-type-of-entries sp-mat) ':any-macsyma) (sp-add* ,@ l))
	 ((zerop (sp-characteristic sp-mat)) (+ ,@ l))
	 (t (mod (+ ,@ l) (sp-characteristic sp-mat)))))

(defmacro non-negative-remainder (x y)
  `(progn (setf ,x (rem ,x ,y))
	  (cond ((minusp ,x)   (+ ,x ,y))
		(t  ,x))))
;
;(defsubst row-entry ( arow j )
; (let ((.this-row. arow)(.j. j) ind)
;  (catch 'entry
;    (loop for ii below (array-active-length .this-row.) by 2
;	  when (and (setf ind (aref .this-row. ii )) (eq ind .j.))
;	 do (throw 'entry (aref .this-row. (1+ ii )))))))

(defun row-entry ( arow j &aux col)
  (catch 'entry
    (loop for ii below (length (the cl:array arow)) by 2
	  when (and (setf col (aref arow ii )) (eql j col))
	 do (throw 'entry (aref arow (1+ ii ))))))

;(defmacro row-dot (.this-row. brow ) "dot product of rows arow and brow"
;  `(let ((.val.)
;	 (.char. characteristic)  .prod.)
;     (loop for .iii. below (row-length ,arow)
;	when (and (aref ,arow .iii. 0)  (setf .val. (row-entry ,brow (aref ,arow .iii. 0))))
;	summing (special-times
;			 .val.
;			 (aref ,arow .iii. 1)))))

;(defun row-dot (arow brow ) "dot product of rows arow and brow"
;  (let ((val)
;	 prod)
;     (loop for iii below (row-length arow)
;	when (and (aref arow iii 0)  (setq val (row-entry brow (aref arow iii 0))))
;	summing  (special-times
;			 val
;			 (aref arow iii 1)))))


;(defun make-number (n)
;  (cond ((null n) 0)
;	(t n)))

(defun sp-row-dot (sp-mat arow brow &aux aindex (ans 0))
  (with-characteristic
      (let (val)
	(loop for iii below (length (the cl:array arow)) by 2
	   when (and (setf aindex (aref arow iii ))
		     (setq val (row-entry brow aindex)))
	   do (setq ans
		    (special-plus ans
				  (special-times
				   val
				   (aref arow (1+ iii) )))))))
  ans)


(defun sp-row-dot-row-numbers (sp-mat i j)
  (sp-row-dot sp-mat (aref (sp-rows sp-mat) i) (aref (sp-rows sp-mat) j)))

(defmacro below-fill (a)
  `(max (- (length (the cl:array ,a)) 2) 0))
;(defsubst set-fill-pointer (aarray n)
;  (store-array-leader n aarray 0))
;
;(defsubst maybe-move-back-fill-pointer (arow)
;  (cond ((equal (array-active-length arow) 0) nil)
;	(t
;	 (let ((this-row arow))
;	   (loop for i downfrom (below-fill this-row) to 0 by 2
;		 when (aref this-row i)
;		 do (setf (fill-pointer this-row) (+ i 2)) (return 'done)
;		 finally (setf (fill-pointer this-row) 0))))))

(defun maybe-move-back-fill-pointer (arow)
  (cond ((equal (length (the cl:array arow)) 0) nil)
	(t
	 (loop for i downfrom (below-fill arow) to 0 by 2
	       when (aref arow i)
	       do (setf (fill-pointer arow) (+ i 2)) (return 'done)
	       finally (setf (fill-pointer arow) 0)))))

(defun sp-reduce-elements-for-type (sp-mat )
  (cond ((fixnump (sp-type-of-entries sp-mat))
	 (setf (sp-characteristic sp-mat) (sp-type-of-entries sp-mat))
	 (loop for iii below (sp-number-of-rows sp-mat)
	    do (let ((this-row (aref (sp-rows sp-mat) iii)))
		 (loop for ii below (length (the cl:array  this-row)) by 2
			when (aref this-row ii )
			do (let ((value (aref this-row (1+ ii))))
			     (cond (value
				    (setq value (mod value (sp-type-of-entries sp-mat)))
				    (cond ((eql value 0)(setf (aref this-row ii  )  nil))
					  (t (setf (aref this-row (1+ ii)) value)))))))
					;(setf;(aref this-row ii 1)  value)))))))
		 (maybe-move-back-fill-pointer this-row))))))

(defun sp-reset-list-of-all-columns-occurring (sp-mat &aux  ind)
  (setf (sp-list-of-all-columns-occurring sp-mat)
	(loop for ii below (sp-number-of-rows sp-mat)
	   for this-row = (aref  (sp-rows sp-mat) ii)
	   appending
	     (loop for jj below (length (the cl:array  this-row)) by 2
		when (and (setq ind (aref this-row jj ))
			  (not (member ind temp :test #'eq)))
		collecting ind)
	   into temp
	   finally (return temp))))

(defun sp-set-rows (sp-mat the-rows &optional list-of-columns)
  (setf (sp-rows sp-mat)
	(if (array-has-fill-pointer-p the-rows)
	    the-rows
	    (make-array (array-dimension the-rows 0) :adjustable t
			:displaced-to the-rows :fill-pointer t)))
  (setf (sp-number-of-rows sp-mat) (fill-pointer (sp-rows sp-mat)))
 ; (cond ((zerop (fill-pointer the-rows)) (break 'why-no-rows)))
 ; (show (sp-rows sp-mat))
  (setf (sp-last-good-row sp-mat) (1- (sp-number-of-rows sp-mat)))
  (cond ((null list-of-columns) ( sp-reset-list-of-all-columns-occurring sp-mat))
	(t (setf (sp-list-of-all-columns-occurring sp-mat) list-of-columns)))
  (setf (sp-column-used-in-row sp-mat)
	(MAKE-ARRAY (fill-pointer (sp-rows sp-mat)) :adjustable t))
  (setf (sp-rows-with-no-pivot sp-mat)
	(make-ARRAY (fill-pointer (sp-rows sp-mat)) :element-type '(mod 2)))
  (sp-check-set-up sp-mat)
  (cond ((and  (sp-columns-used-to-pivot sp-mat)
	      (hash-table-p (sp-columns-used-to-pivot sp-mat) ))
	 (clrhash (sp-columns-used-to-pivot sp-mat) ))
	(t
	 (setf (sp-columns-used-to-pivot sp-mat)
	       (make-hash-table :size (sp-number-of-rows sp-mat))))))

(defun ml-sort (lis &optional (predicate #'alphalessp))
  (sort lis predicate))

(defun sp-check-set-up (sp-mat &aux tem)
  (setf (sp-current-row sp-mat) nil)
  (setf (sp-reduced sp-mat ) nil)
  (setf (sp-last-good-row sp-mat) (sp-number-of-rows sp-mat))
  (setf   ( sp-current-column-above-pivot-row-number sp-mat) nil)
  (cond ((setq tem (sp-columns-used-to-pivot sp-mat))
	(clrhash tem ))
	(t (setf (sp-columns-used-to-pivot sp-mat) (make-hash-table))))
  (setf (sp-columns-with-no-pivot sp-mat) nil)
;  (setf (sp-column-used-in-row sp-mat) nil)
  )


;(defun compare-structures (a b &aux slotb slota)
;  (let ((slots (fourth (get (ml-typep a)  'si:defstruct-description))))
;    (loop for v in slots
;	  for i from 0
;	  when (not (equalp (setq slota (funcall (car (last v)) a))
;			    (setq slotb (funcall (car (last v)) b))))
;	  do (show (car (last v)) slota slotb))))

(defun sp-set-current-row  (sp-mat i)
  (setf (sp-current-row-number sp-mat) i)
  (setf (sp-current-row sp-mat)  (aref (sp-rows sp-mat) i))
  (setf (sp-current-row-length sp-mat)
	(length (the cl:array (sp-current-row sp-mat))))
  (sp-current-row sp-mat))

(eval-when
    #+gcl (load compile eval) ;;because of symbolics bug
    #-gcl (:load-toplevel :compile-toplevel :execute)
    (defmacro sp-row  (sp-mat i)
      `(aref (sp-rows ,sp-mat) ,i)))

(defun sp-set-pivot-row (sp-mat i)
	   (setf (sp-pivot-row-number sp-mat) i)
	  (setf (sp-pivot-row sp-mat)  (aref (sp-rows sp-mat) i))

  (sp-pivot-row sp-mat))

(defun sp-entry (sp-mat  i j &aux (this-row (aref (sp-rows sp-mat) i)))
    (catch 'entry
      (loop for ii below (length (the cl:array this-row)) by 2
	    when (eql j (aref this-row ii ))
	    do (throw 'entry (aref this-row (1+ ii))))))

(defun fix-even (n)
  (* 2 (round (quotient n 2.0))))

(defun sp-grow-current-row (sp-mat  &optional (ratio 1.3))
  (let ((new-length
	  (max (fix-even (* ratio (row-length
				    (aref (sp-rows sp-mat)
					  (sp-current-row-number sp-mat)) )))
			 (sp-minimum-size-to-grow sp-mat))))
   (setf (aref
      (sp-rows sp-mat) (sp-current-row-number sp-mat))
      (array-grow (sp-current-row sp-mat) new-length ))
    ( sp-set-current-row sp-mat (sp-current-row-number sp-mat))))

(defun sp-grow-row (sp-mat row-number &optional (ratio 1.3))
  (let ((new-length (max (fix-even (* ratio (row-length  (aref (sp-rows sp-mat) row-number) )))
			 (sp-minimum-size-to-grow sp-mat))))
   (setf (aref
      (sp-rows sp-mat) row-number)
      (array-grow (aref (sp-rows sp-mat) row-number)  new-length ))))

;(defmacro current-rowset ( element index sslot)
;  `(progn (cond ((equal ,element 0) (aset nil (sp-current-row sp-mat)  ,sslot )
;		 (maybe-move-back-fill-pointer (sp-current-row sp-mat)))
;		(t (cond (< ,sslot
;		 (aset  ,element (sp-current-row sp-mat) (1+ ,sslot) )
;		 (aset  ,index (sp-current-row sp-mat) , sslot )))))

;(defmacro this-row-set ( element index sslot)
;  `(progn (cond ((equal ,element 0) (aset nil this-row  ,sslot )
;		 (maybe-move-back-fill-pointer this-row))
;		(t
;		 (aset  ,element this-row (1+ ,sslot) )
;		 (aset  ,index this-row , sslot )))))
;
;(defsubst set-entry ( value arow index)
;  (catch 'entry-is-set
;    (let* ((.this-row. arow)
;	  (.ind. index) j (.val. value)
;	  first-empty-slot
;	  (active-length (array-active-length .this-row.)))
;      (setf first-empty-slot
;	    (loop for ii below (array-active-length .this-row.) by 2
;		  do
;		  (cond ((null (setq j (aref .this-row. ii))) (return ii))
;			((eql j .ind.)
;			 (cond (($zerop .val.) (aset nil .this-row. .ind.)
;				(if (eql ii (- active-length 2))
;				    (maybe-move-back-fill-pointer .this-row.))
;				)
;			       (t
;				(aset .val. .this-row. (1+ ii ))
;				(aset .ind. .this-row. ii)))
;			 (throw 'entry-is-set t)))))
;      (cond (first-empty-slot
;	     (loop for ii from first-empty-slot below active-length by 2
;		   when (eql .ind. (aref .this-row. ii))
;		   do
;		   (cond (($zerop  .val.) (aset nil .this-row. .ind.)
;			  (if (eql ii (- active-length 2))
;			      (maybe-move-back-fill-pointer .this-row.))
;			  )
;			 (t
;			  (aset .val. .this-row. (1+ ii ))
;			  (aset .ind. .this-row. ii)))
;		   (throw 'entry-is-set t)
;		   finally
;		   (cond (($zerop  .val.) nil)
;			 (t (aset .val. .this-row. (1+ first-empty-slot))
;			    (aset .ind. .this-row. first-empty-slot)))))
;	    ((not ($zerop  .val.))
;	     (array-push-extend .this-row. .ind.)
;	     (array-push-extend .this-row. .val.))))))

(defun set-entry (value arow index &aux (aarow arow) ;; so can declare it an array-register
		  j first-empty-slot (active-length (fill-pointer arow)))
  (catch 'entry-is-set
    (setf first-empty-slot
	  (loop for ii below (fill-pointer aarow) by 2
	     do
	       (cond ((null (setq j (aref aarow ii))) (return ii))
		     ((eql j index)
		      (cond (($zerop value)(setf (aref aarow index)  nil)
			     (if (eql ii (- active-length 2))
				 (maybe-move-back-fill-pointer aarow))
			     )
			    (t
			     (setf (aref aarow (1+ ii ))  value)
			     (setf (aref aarow ii)  index)))
		      (throw 'entry-is-set t)))))
    (cond (first-empty-slot
	   (loop for ii from first-empty-slot below active-length by 2
		  when (eql index (aref aarow ii))
		  do
		  (cond (($zerop  value)(setf (aref aarow index)  nil)
			 (if (eql ii (- active-length 2))
			     (maybe-move-back-fill-pointer aarow))
			 )
			(t
			 (setf	(aref aarow (1+ ii ))  value)
			 (setf	(aref aarow ii)  index)))
		  (throw 'entry-is-set t)
		  finally
		  (cond (($zerop  value) nil)
			(t(setf (aref aarow (1+ first-empty-slot))  value)
			  (setf (aref aarow first-empty-slot)  index)))))
	  ((not ($zerop  value))
	   (vector-push-extend  index aarow)
	   (vector-push-extend  value aarow)))))

;;; did not seem to be calling this
;(defsubst set-entry-without-moving-back-fill-pointer ( value arow index)
;
;(defun sp-rem-current-row-entry (sp-mat  index)
;  (aset nil (sp-current-row sp-mat) index 0))

(defun sp-set-current-row-entry (sp-mat   value ind)
 (set-entry value (sp-current-row sp-mat) ind))

(defun sp-set-entry (sp-mat   value i j)
  (set-entry value (aref (sp-rows sp-mat) i) j))

;(defun row-set-entry (value this-row j)
;  (let ((first-empty-slot nil))
;    (catch 'finished
;      (setq first-empty-slot
;	    (catch 'first
;	      (loop for ii below (row-length this-row)
;		    do (cond ((eql (aref this-row ii 0) j) (this-row-set value j ii)
;			      (throw 'finished t))
;			     ((null (aref this-row ii 0)) (throw 'first ii))
;			     (t nil)))))
;      (cond ( first-empty-slot
;	     (loop for ii from first-empty-slot  below (row-length this-row)
;		   do (cond ((eql (aref this-row ii 0) j) (this-row-set value j ii)
;			     (throw 'finished t))))
;	     (this-row-set value j first-empty-slot))
;	    (t
;	     (let ((last-spot (row-length this-row)))
;	       (array-grow this-row (list (round (* 1.3 (row-length this-row))) 2))
;	       (this-row-set value j last-spot)))))))

;(defmacro pivot-row-ref (index)
;  `(catch 'pivot-row-ref
;     (loop for .ii. below (array-active-length pivot-row) by 2
;	   do (cond ((eql ,index (aref pivot-row .ii.))
;		     (throw 'pivot-row-ref (aref pivot-row (1+ .ii. ))))))))

;(defmacro current-row-ref (index)
;  `(catch 'current-row-ref
;     (loop for .ii. below (array-active-length (sp-current-row sp-mat)) by 2
;	   do (cond ((eql ,index (aref (sp-current-row sp-mat) .ii. ))
;		     (throw 'current-row-ref (aref (sp-current-row sp-mat) (1+ .ii.) )))))))

;(defmacro current-row-slot (index)
;  `(catch 'current-row-slot
;     (loop for .ii. below (array-active-length (sp-current-row sp-mat)) by 2
;	   do (cond ((eql ,index (aref (sp-current-row sp-mat) .ii. ))
;		     (throw 'current-row-slot .ii.))))))



;(defmacro matrix-entry ( i j) "This finds the entry of a sparse-matrix
; the ith row and jth column"
;  `(catch 'matrix-entry
;     (let ((.row. (aref (sp-rows sp-mat) ,i)))
;     (loop for .ii. below (row-length .row.)
;	   do (cond ((eql ,j (aref .row. .ii. 0))
;		     (throw 'matrix-entry (aref .row. .ii. 1))))))))

;(defmacro set-matrix-entry (value i j) "This sets the entry of a sparse-matrix
; the ith row and jth column"
;  `(catch 'set-matrix-entry
;     (setq first-empty-slot (catch 'where
;     (let ((.row. (aref (sp-rows sp-mat) ,i)))
;     (loop for .ii. below (row-length .row.)
;	   do (cond ((null (aref .row. .ii. 0) (throw 'where .ii.))))))))))
;		    ((eql ,j (aref .row. .ii. 0))
;		     (aset value .row. .ii. 1)
;		     (throw 'set-matrix-entry t)))))) nil)

(defmacro row-slot (row index)
  "Returns the slot that the INDEX (column) appears in ROW"
  `(catch 'row-slot
     (let ((.row. ,row))
     (loop for .ii. below (length (the cl:array .row.)) by 2
	   do (cond ((eql ,index (aref .row. .ii. ))
		     (throw 'row-slot .ii.)))))))

(defmacro set-current-row-entry-in-new-column (value row index)
  "Puts new entry in a column not occurring in row.  Would work
  for (sp-rows sp-mat) other than current-row except for wanting the row number
  to be able to replace it in ROWS if ROW is grown."
  `(let ((.val. ,value)
	(.ind. ,index))
  (cond (($zerop  .val.) nil)
	(t
  (loop for ii below (length (the cl:array ,row)) by 2
	when (null (aref ,row ii))
	do(setf (aref ,row ii)  .ind.)
(setf	(aref ,row (1+ ii))  .val.)
	(return 'entry-is-set)
	finally (array-push-extend-replace ,row .ind. :replace ((aref (sp-rows sp-mat) (sp-current-row-number sp-mat))))
	(array-push-extend-replace ,row .val. :replace ((aref (sp-rows sp-mat) (sp-current-row-number sp-mat)))))))))

;;Note: the with-once-only makes local variables for the pivot-row
;;and the current-row and it is these that are declared
(defun sp-row-operation  (sp-mat factor &aux current-row-slot new-value
			  piv-col temp)
  "this replaces the current-row by current-row + factor pivot-row"
  (with-characteristic
      (with-once-only
	  ((sp-pivot-row sp-mat) (sp-current-row sp-mat))
	(cond
	  ((equal factor 0) nil)
	  (t
	   (loop for ii below (length (the cl:array (sp-pivot-row sp-mat))) by 2
	      when (setq piv-col (aref (sp-pivot-row sp-mat) ii ))
	      do (cond ((setq current-row-slot
			      (row-slot  (sp-current-row sp-mat) piv-col))
			(setq  new-value
			       (special-plus
				(special-times
				 factor (aref (sp-pivot-row sp-mat) (1+ ii )))
				(aref (sp-current-row sp-mat)
				      (1+ current-row-slot))))
			(cond (($zerop  new-value )
			       (setf (aref (sp-current-row sp-mat) current-row-slot )  nil))
			      (t(setf (aref
				       (sp-current-row sp-mat)
				       (1+ current-row-slot ))  new-value))))
		       (t (set-current-row-entry-in-new-column
			   (special-times factor
					  (aref (sp-pivot-row sp-mat) (1+ ii)))
			   (sp-current-row sp-mat) piv-col))))
	   (maybe-move-back-fill-pointer (sp-current-row sp-mat)))))
    (cond ((and (sp-constants-column sp-mat)
		(not ($zerop  (setq temp (aref (sp-constants-column sp-mat)
					       (sp-pivot-row-number sp-mat))))))
	   (setf (aref
		  (sp-constants-column sp-mat) (sp-current-row-number sp-mat))  (special-plus (aref (sp-constants-column sp-mat)
												    (sp-current-row-number sp-mat))
											      (special-times factor temp)))))))

;(with-characteristic
;  (special-times 3 4))

(defun get-rows-from-array ( matrix)
  (let* ((dims (array-dimensions matrix))
	 (the-rows (MAKE-ARRAY (car dims) :adjustable t :fill-pointer (car dims)))
	 arow)
    (loop for i below (car dims)
	  do (setq arow (MAKE-ARRAY (* (second dims) 2) :fill-pointer 0 :adjustable t))
	 (setf (aref the-rows i)  arow)
	  (loop for j below (second dims)
		do (let ((entry (aref matrix i j)))
		     (cond ((not (equal entry 0))
			    (vector-push  j arow)
			    (vector-push  entry arow)))))
	  finally (return the-rows))))

(defun random-matrix (m n &optional (random-size 5))
  (let ((mat (MAKE-ARRAY (list  m n) :adjustable t)) )
    (loop for i below m
	  do (loop for j below n
		   do(setf (aref mat i j)  (random random-size)))
	  finally (return mat))))

(defun sp-gcd-row (sp-mat i)
  (let ((ans ( sp-first-element-of-row sp-mat i))
	(this-row (aref (sp-rows sp-mat) i)))
       (loop for ii below (length (the cl:array this-row)) by 2
	     when (aref this-row ii )
	     do (setq ans (gcd ans (aref this-row (1+ ii ))))
	     finally (return ans))))
(defun sp-first-element-of-row (sp-mat ii)
  (catch 'first-element
    (let ((this-row (aref (sp-rows sp-mat) ii)))
   (loop for i below (length (the cl:array this-row)) by 2
	 when (aref this-row i )
	 do (throw 'first-element (aref this-row (1+ i )))))))


(defmacro show-row (arow)
  `(let ((this-row ,arow) ind)
     (loop for jj below (length (the cl:array this-row)) by 2
	   when (setq ind (aref this-row jj))
	   do (format t "~%In slot ~D ~D-->~D "
		      (truncate jj 2) ind (aref this-row (1+ jj))))))

(defun sp-show-row (sp-mat i)
  (let ((this-row (aref (sp-rows sp-mat) i)))
   (format t "~%Row ~D is ~A." i this-row)
   (show-row this-row)))

(defun sp-show-current-and-pivot (sp-mat )
  (let ((crn (sp-current-row-number sp-mat)))
    (format t "~%Current row is ~D which is ~A." crn (sp-current-row sp-mat))
    (show-row (sp-current-row sp-mat))
    (format t "~%Pivot Row is ~D which is ~A." (sp-pivot-row-number sp-mat)
	    (sp-pivot-row sp-mat))
    (show-row (sp-pivot-row sp-mat))))

;(defmacro special-inverse (x)
;  `(cond ((equal (sp-type-of-entries sp-mat)
;		 ':any-macsyma)(sp-div* 1 ,x))
;	 ((numberp (sp-type-of-entries sp-mat))
;	  (aref (sp-inverse-array sp-mat)
;		(mod ,x (sp-type-of-entries sp-mat))))
;	 (t (sp-rational-quotient 1 ,x))))

(defmacro special-inverse (x)
  `(cond ((equal (sp-type-of-entries sp-mat)
		 ':any-macsyma)(sp-div* 1 ,x))
	 ((numberp (sp-type-of-entries sp-mat))
	  (let ((modulus (sp-type-of-entries sp-mat)))
	    (crecip ,x)))
	 (t (sp-rational-quotient 1 ,x))))

(defun sp-rat (x)
  (cond ((or (polynomialp x)(rational-functionp x)) x)
	(($bfloatp x) x)
	(t (new-rat x))))


(defun sp-choose-type-of-entries (sp-mat &aux  rational float tem1
					 (rows (sp-rows sp-mat)))
  (loop named sue for i below (length (the cl:array rows))
	do
	(let ((a-row (aref rows i)))
	  (loop for ii below (length (the cl:array a-row)) by 2
		when (aref a-row ii)
		do
		(cond ((numberp (setq tem1 (aref a-row (1+ ii))))
		       (cond ((or float (floatp tem1))(setq float t))
			     ((rationalp tem1) (setq rational t))))
		      (t (sp-set-type-of-entries sp-mat ':any-macsyma)
			 (return-from sue 'done)))))
	finally
	;;if any floating use float else if any rational use rational
	;;else use integer
	(cond (float (sp-set-type-of-entries sp-mat ':float))
	      (rational (sp-set-type-of-entries sp-mat :rational))
	      (t  (sp-set-type-of-entries sp-mat :integer)))))

(defun sp-set-type-of-entries (sp-mat y)
  (setf (sp-type-of-entries sp-mat) y)
  (cond ((equal y ':integer)
	 (setf (sp-characteristic sp-mat) (or modulus 0))
	 (setf (sp-pivot-test-list sp-mat) '(unit gcd-column )))
	;;any is bad do gcd of col etc.
	((equal y ':rational)
	 (cond (modulus (setf (sp-characteristic sp-mat) modulus)
			(setf (sp-type-of-entries sp-mat)modulus))
	       (t
		(setf (sp-pivot-test-list sp-mat) '(unit  any)) ;;removed gcd
		(setf (sp-characteristic sp-mat) 0))))
	((equal y ':float)
	 (setf (sp-pivot-test-list sp-mat) '(any))
	 (setf (sp-characteristic sp-mat) (or modulus 0)))
	((equal y ':any-macsyma)
	 (setf (sp-pivot-test-list sp-mat) '(:any-macsyma))
	 (setf (sp-characteristic sp-mat) (or modulus 0))
	 (loop for i below (array-total-size (sp-rows sp-mat))
	       do
	       (let ((this-row (aref (sp-rows sp-mat) i)))
		 (loop for j below (array-total-size this-row) by 2
		       when (aref this-row j)
		       do(setf (aref
				this-row (1+ j))  (sp-rat (aref this-row (1+ j))))))))
	((and (fixnump y) (> y 0))
	 (setf (sp-characteristic sp-mat) y))
	(t (merror "At present only positive integers (ie finite char)
		    ,integer, rational, or':any-macsyma are valid types"))))


(defun sp-enter-pivot-data (sp-mat col)
  (setf (gethash
	  col
	  (sp-columns-used-to-pivot sp-mat))
	(sp-pivot-row-number sp-mat))
 (setf (aref
	(sp-column-used-in-row sp-mat)  (sp-pivot-row-number sp-mat))  col)
  (fmat t "Row ~D has pivot ~D in column ~D." (sp-pivot-row-number sp-mat)
	(sp-pivot-entry sp-mat)
	col))

(defun sp-remove-zero-entries-from-row (sp-mat i)
  (let ((this-row (aref (sp-rows sp-mat) i)))
    (loop for ii below (length (the cl:array this-row)) by 2
	  when (aref this-row ii )
	  do (if (null (aref this-row (1+ ii) ))(setf (aref this-row ii 0)  nil)))
    (maybe-move-back-fill-pointer this-row)))

(defun show-hash (tabl)
  (loop for x being the hash-keys in tabl
     using (hash-value y)
     do (format t "~%~A -->~A" x y)))

(defun sp-show-rows(sp-mat &rest l)
  (cond ((null l) (setq l (loop for i below (sp-number-of-rows sp-mat)
				collecting i))))
   (loop for i in l do ( sp-show-row sp-mat i))
   (format t "~%Current row is row ~D." (sp-current-row-number sp-mat))
   (format t "~%Pivot row is row ~D." (sp-pivot-row-number sp-mat)))

(defun sp-list-pivots (sp-mat &optional pred &aux entry col)
  (cond ((null pred) (setq pred (function (lambda (ign)ign t))))
	((equal pred :non-unit)
	 (setq pred (function (lambda (x) (not (eql (abs x) 1)))))))
    (loop for i below (sp-number-of-rows sp-mat)
	  when (and (setq col (aref (sp-column-used-in-row sp-mat) i))
		    (funcall pred
			     (setq entry ( sp-entry sp-mat i col))))
	  collecting  entry into tem
;	  (format t "~%Row ~D has pivot ~D in column ~D." i entry col)
	  finally (return tem)))

(defun sp-show-pivots (sp-mat &optional pred &aux entry col)
  (cond ((null pred) (setq pred (function (lambda (ignor)ignor t))))
	((equal pred :non-unit)
	 (setq pred (function (lambda (x) (not (eql (abs x) 1)))))))
    (loop for i below (sp-number-of-rows sp-mat)
	  when (and (setq col (aref (sp-column-used-in-row sp-mat) i))
		    (funcall pred (setq entry ( sp-entry sp-mat i col))))
	  do
	  (format t "~%Row ~D has pivot ~D in column ~D." i entry col)))

(defmacro null-to-zero (x)
  `(cond ((null ,x) 0)
	 (t ,x)))

(defun sp-show-matrix(sp-mat )
  (cond ( (sp-list-of-all-columns-occurring sp-mat) nil)
	(t
	 (setf (sp-list-of-all-columns-occurring sp-mat)
	       (sort (sp-list-of-all-columns-occurring sp-mat) '<))))
  (format t "~%       " )
  (loop for i in (sp-list-of-all-columns-occurring sp-mat) do
	(format t "C~D   " i))
  (cond ((sp-constants-column sp-mat)
	 (format t "Const~D"  (sp-constants-column-number sp-mat))))
  (loop for i below (sp-number-of-rows sp-mat)
	do (format t "~%R~2D " i)
	(loop for j in (sp-list-of-all-columns-occurring sp-mat)
	      do (format t "~5D"
			 (null-to-zero ( sp-entry sp-mat i j))))
	(cond ((sp-constants-column sp-mat)
	       (format t "~5D" (aref (sp-constants-column sp-mat) i)))))
  (cond ((and  (sp-special-solution sp-mat) )
	 (format t "~%SpSol")
	 (loop for j in (sp-list-of-all-columns-occurring sp-mat)
	       do (format t "~5D"
			  (null-to-zero (row-entry
					  (sp-special-solution sp-mat) j)))))))

;(defun sp-reduce (sp-mat &optional type-of-entry &aux current-row-entry)
;  (cond (type-of-entry ( sp-set-type-of-entries sp-mat type-of-entry)))
;  (cond ((typep (sp-type-of-entries sp-mat) :fixnum)
;	 ( sp-reduce-elements-for-type sp-mat)))
;  ( sp-clear-pivots sp-mat)
;  (with-characteristic
;   (loop for i below (sp-number-of-rows sp-mat)
;	do
;	( sp-set-pivot-row sp-mat i)
;	( sp-best-pivot sp-mat)
;	(if (sp-pivot-entry sp-mat)
;	    (let ((pivoting-column
;		    (aref (sp-column-used-in-row sp-mat)
;			  (sp-pivot-row-number sp-mat)))
;		  (minus-inverse-pivot-entry
;		    (special-minus (special-inverse (sp-pivot-entry sp-mat)))))
;	    (loop for ii from (+ 1 i) below (sp-number-of-rows sp-mat)
;		  do
;		  ( sp-set-current-row sp-mat ii)
;		  (cond ((setq current-row-entry
;			       ( sp-entry sp-mat ii
;				    pivoting-column ))
;			 (    sp-row-operation sp-mat
;				(special-times
;					current-row-entry
;					minus-inverse-pivot-entry)
;			       )))
;;;this gets rid of the denoms but you should gcd and.. what about constants
;		  (cond ((and (eql (sp-type-of-entries sp-mat) :integer)
;			      (not (typep minus-inverse-pivot-entry :fixnum)))
;
;			       (sp-multiply-row sp-mat ii  (sp-pivot-entry sp-mat))))
;;;        (sp-show-matrix sp-mat)
;
;	    )))))
;
;  (Setf (sp-reduced sp-mat) t))

(defun sp-reduce (sp-mat &optional type-of-entry &aux current-row-entry
		  (original-number-of-rows (sp-number-of-rows sp-mat)))
  (cond (type-of-entry ( sp-set-type-of-entries sp-mat type-of-entry)))
  (cond ((fixnump (sp-type-of-entries sp-mat))
	 ( sp-reduce-elements-for-type sp-mat)))
  (sp-clear-pivots sp-mat)
  (with-characteristic
    (loop for i from 0  while (< i (sp-number-of-rows sp-mat))
	  do
	  ( sp-set-pivot-row sp-mat i)
	  ( sp-best-pivot sp-mat)
	  (if (sp-pivot-entry sp-mat)
	      (let ((pivoting-column
		      (aref (sp-column-used-in-row sp-mat)
			    (sp-pivot-row-number sp-mat)))
		    (minus-inverse-pivot-entry
		      (special-minus (special-inverse
				       (sp-pivot-entry sp-mat)))))
		(loop for ii from (+ 1 i) below (sp-number-of-rows sp-mat)
		      do
		      ( sp-set-current-row sp-mat ii)
		      (cond ((setq current-row-entry
				   (sp-entry sp-mat ii
					     pivoting-column ))
			     (sp-row-operation sp-mat
					       (special-times
						 current-row-entry
						 minus-inverse-pivot-entry)
					       ))))
		))))
  (sp-kill-extra-rows sp-mat original-number-of-rows)
  (setf (sp-reduced sp-mat) t))

(defun sp-kill-extra-rows (sp-mat original-number-of-rows &aux up-bound)
  (let ((rows (sp-rows sp-mat)))
    (loop for i from original-number-of-rows below (sp-number-of-rows sp-mat)
	  when (not (zerop (fill-pointer (aref  rows i)) ))
	  do (setq up-bound (1+ i))
	  finally(cond(up-bound nil) (t (setq up-bound original-number-of-rows)))
	  (setf (fill-pointer rows) up-bound)
	  (setf (sp-number-of-rows sp-mat) up-bound))))


(defun sp-multiply-row (sp-mat row-number factor)
  (let ((this-row (aref (sp-rows sp-mat) row-number)))
    (cond ((equal 0 (special-times 1 factor))
	   (loop for i below (fill-pointer this-row)
		 do (setf (aref this-row i )  nil)
	   (setf (fill-pointer this-row) 0)))
	  ((equal 1 factor) nil)
	  (t
	   (with-characteristic
	     (loop for i below (length (the cl:array this-row)) by 2
		   when (aref this-row i )
		   do(setf (aref this-row (1+ i) )    (special-times
				(aref this-row (1+ i ))
				factor ))))))))


(defmacro dsend (object &rest body)
 `(progn (send ,object .,body)
	 (send ,object :show-matrix)))

(defun sp-clear-pivots(sp-mat )
 (clrhash (sp-columns-used-to-pivot sp-mat))
  (fillarray (sp-column-used-in-row sp-mat)  nil)
  (fillarray (sp-rows-with-no-pivot sp-mat)  '(0))
  (loop for i below (sp-number-of-rows sp-mat)
	do(setf (aref (sp-column-used-in-row sp-mat) i)  nil)))

(defun sp-get-rows-from-array (sp-mat aarray &optional (type :integer))
   ( sp-set-rows sp-mat (get-rows-from-array aarray))
    ( sp-set-type-of-entries sp-mat type)
    (sp-check-set-up sp-mat))


;(defun h (i j) ($concat '$dd i j))
;(defun z
;  (loop for u in a-list do (apply 'aset 0 aarray u)))
;(zero-out aa '((0 1) (0 3) (0 5) (0 7)(1 0)(1 2)(1 4) (1 6)(3 1) (3 3)(3 5)(3 7)
;	       (4 0)(4 2)(4 4)(4 6)))

(defun genmatrix (m n fun)
  (let ((ans (MAKE-ARRAY (list m n) :adjustable t)))
    (loop for i below m
       do (loop for j below n do
		 (setf (aref ans i j)  (funcall fun i j ))))
    ans))

(defun macsyma-array-to-row  ( aarray &optional (grow-ratio 1))
  (let* ((aarray (symbol-array (mget  aarray 'hashar)))
	(actual-size (aref aarray 1))
	(index 0)
	ans  val )
    (setq ans (MAKE-ARRAY  (round (* actual-size 2 grow-ratio)) :fill-pointer 0 :adjustable t))
    (loop for i from 3 below (car (array-dimensions aarray))
	  do (cond ((setq val (aref aarray i))
		    (loop for u in val do
			(array-push-extend-replace ans (caar u))
			(array-push-extend-replace ans (cdr u))))))
    (cond ((not (equal (setq index (sp-rational-quotient (fill-pointer ans) 2) )
		       actual-size))
	   (format t "We only found ~D elements while
		      there should have been ~D elements."
		   index actual-size)))
    ans))

(defun list-of-macsyma-arrays-to-rows (llist)
  (let* ((size (length (cdr llist)))
	 (rows (MAKE-ARRAY size :fill-pointer 0 :adjustable t)))
    (loop for u in (cdr llist)
       do (vector-push-extend  (funcall 'macsyma-array-to-row u) rows))
    rows))

;(defun get-array (&quote macsyma-array)
; (fsymeval  (mget macsyma-array 'hashar)))
;
;(defmacro mydump (  name-of-object-to-dump &optional filename  file-atribute-list )
;  (cond ((null filename) (setq filename (string name-of-object-to-dump))))
;  `(sys:dump-forms-to-file ,filename
;			   (list (list 'setq ',name-of-object-to-dump
;				       (list 'quote ,name-of-object-to-dump)))
;			   ,file-atribute-list))
;
;(defun te (n   c &aux a aa)
;  (setq a c)   ;;96 ms. this is best.93 ms. if put the aa let outside the do loop
;  (loop for i below n do (let(( aa (* i 2))) (cond ((zerop a) aa)
;						 (t (rem aa a))))))
;
;(defun te (n   c &aux d)  ;;130ms.
;  (local-declare ((special d)) (setq d c)
;  (loop for i below n do ((lambda(a b)(cond  ((eql d 0) (* a b))
;					    (t (rem (* a b) d)))) i 2 ))))
;
;(defun te (n c &aux d)
;  (let ((d c))       ;;108 ms. slower than if you set up variable for (* i 2)
;    (loop for i below n do (cond ((zerop d) (* i 2))
;				 (t (rem (* i 2) d))))))
;(defun te (n c &aux d) ;;faster than without the setq d! approx 75ms.
;  (loop for i below n do  (setq d (aref c 500)) ))
;
;(defun te (n c &aux sp)
;  (loop for i below n do (setq c (  i 2))))
;
;(defmacro with-speed (&body body)
;  `(let ((.char. characteristic) .prod. .sum.)
;     ,@body))
;
;
;(defun te (n   c &aux )
;  (let ((d c))             ;;120 ms.
;  (loop for i below n do ((lambda(a b d)(cond  ((eql d 0) (* a b))
;					    (t (rem (* a b) d)))) i 2 d ))))
;
;(progn 'compile (setf (function sp-ti) (function (lambda(a b d)(cond  ((eql d 0) (* a b))
;					    (t (rem (* a b) d)))) )))
;(defun sp-test (sp-mat n)
;  (let (((sp-type-of-entries sp-mat) type-of-entries))  ;;fairly-fast
;  (loop for i below n do (hh i 2 type-of-entries))))
;
;(defun sp-test (sp-mat n &aux a)
; (let ((type-of-entries type-of-entries))
;  (loop for i below n do
;	(setq a (* i 2)) (cond ((zerop type-of-entries) a)
;						(t (rem type-of-entries a))))))
;
;(defun sp-test (sp-mat n)
;  (with-characteristic ; 77ms. in characteristic=0 this is best!!
;    (loop for i below n do (special-times i 2))))
;
;
;(defun-method  h sparse-matrix (a b)  ;;slow
;				      (cond ((eql type-of-entries 0) (* a b))
;					    (t (rem (* a b) type-of-entries))))

(defun sp-number-of-pivots(sp-mat &optional (below-row (sp-number-of-rows sp-mat)))
  (let ((count 0))
      (loop for i below below-row
	when (aref (sp-column-used-in-row sp-mat) i) do (setf count (1+ count))
	finally (return count))))

					;(defun te (&rest l &aux with bar five)
; (keyword-extract l vv    ((:with bar)  five ))
; (print (list bar five)))
;(for element of row with index in slot do body)
;(defmacro for (u &rest l &aux element row index slot)
;   (keyword-extract l vv ( (:of row) (:with index) (:in slot) (:do body)))
;    `(let ((,row (aref rows ,i)) ,element   ,index )
;    (loop for ,slot below (row-length ,row)
;	  when (aref ,row ,slot 0)
;	  do (setq ,element (aref ,row ,slot 1))
;	  (setq ,index (aref ,row ,slot 0))
;	 . ,body)))

(defun sp-solve (sp-mat &key reduce reset-list-of-columns-occurring)
  (cond (reset-list-of-columns-occurring ( sp-reset-list-of-all-columns-occurring sp-mat)))
  (cond ((or reduce (null (sp-reduced sp-mat)))
	 ( sp-clear-pivots sp-mat)
	 ( sp-reduce sp-mat)))
  (setf (sp-columns-with-no-pivot sp-mat)
	(loop for u in (sp-list-of-all-columns-occurring sp-mat)
	   when (null (gethash  u (sp-columns-used-to-pivot sp-mat)))
	   collecting u))
  (let*	((number-of-columns (length (sp-list-of-all-columns-occurring sp-mat)))
	 (number-of-pivots ( sp-number-of-pivots sp-mat))
	 (number-of-solutions (- number-of-columns number-of-pivots))
	 a-solution a-new-entry a-special-solution
	 (solution-rows
	  (MAKE-ARRAY (length (sp-columns-with-no-pivot sp-mat))
		      :fill-pointer 0 :adjustable t)))
    (cond ((not (equal (length (sp-columns-with-no-pivot sp-mat))
		       number-of-solutions))
	   (format t "~%There are ~D columns and ~D pivots and ~D columns with no pivot,
something is wrong" (length (sp-list-of-all-columns-occurring sp-mat)) number-of-pivots (length (sp-columns-with-no-pivot sp-mat)))))

    (cond
      ((>  number-of-solutions 0)
       (with-characteristic

	   (loop for u in  (sp-columns-with-no-pivot sp-mat)
	      do (setf a-solution (MAKE-ARRAY (* (1+ number-of-pivots) 2)
					      :fill-pointer 0 :adjustable t))

		(vector-push  a-solution solution-rows)
		(fmat t "~%solution-rows ~A"  (listarray solution-rows))
		(set-entry 1 a-solution u)
		(loop for nn downfrom (1- (fill-pointer (sp-rows sp-mat))) to 0
		       when (aref (sp-column-used-in-row sp-mat) nn)
		       do
		       (setf (sp-pivot-entry sp-mat)
			     ( sp-entry sp-mat nn
					(aref (sp-column-used-in-row sp-mat) nn)))
		       (setq a-new-entry
			     (special-times
			      -1
			      (special-inverse  (sp-pivot-entry sp-mat))
			      ( sp-row-dot sp-mat a-solution (aref (sp-rows sp-mat) nn))))

		       (cond ((not ($zerop  a-new-entry))
			      (vector-push  (aref (sp-column-used-in-row sp-mat) nn) a-solution)
			      (vector-push  a-new-entry a-solution))))))))
    (cond ((sp-constants-column sp-mat)
	   (loop for i below (array-total-size (sp-column-used-in-row sp-mat))
		  when (null (aref (sp-column-used-in-row sp-mat) i))
		  do
		  (cond ((not ($zerop  (aref (sp-constants-column sp-mat) i)))
			 (format t "~%~%  ******************************~%")
			 (format	  t "~%Row ~D has no pivot but the (sp-constants-column sp-mat) has entry ~D.
		   ~%WARNING.  The equations were INCONSISTENT.
		   ~%The special-solution is NOT VALID.
		   ~%  ******************************" i (aref  (sp-constants-column sp-mat) i)))))
	   (show (sp-type-of-entries sp-mat))
	   (with-characteristic
	       (setq a-special-solution (MAKE-ARRAY (* (1+ number-of-pivots) 2)
						    :fill-pointer 0 :adjustable t))
	     (loop for nn downfrom (1- (fill-pointer (sp-rows sp-mat))) to 0
		    when (aref (sp-column-used-in-row sp-mat) nn)
		    do
		    (setf (sp-pivot-entry sp-mat)
			  ( sp-entry sp-mat nn (aref
						(sp-column-used-in-row sp-mat) nn)))
		    (setq a-new-entry
			  (special-times -1
					 (special-inverse  (sp-pivot-entry sp-mat))
					 (special-plus
					  ( sp-row-dot sp-mat a-special-solution
						       (aref (sp-rows sp-mat) nn))
					  (aref (sp-constants-column sp-mat) nn))))
		    (cond ((not ($zerop  a-new-entry))
			   (vector-push
			    (aref (sp-column-used-in-row sp-mat) nn) a-special-solution)
			   (vector-push  a-new-entry a-special-solution)))))))
    (cond ((and  (sp-solutions sp-mat)(ml-typep
				       (sp-solutions sp-mat) 'sparse-matrix)))
	  (t (setf (sp-solutions sp-mat) (make-sparse-matrix ))))
    (sp-set-rows (sp-solutions sp-mat) solution-rows)
    (sp-set-type-of-entries (sp-solutions sp-mat)
			    (sp-type-of-entries sp-mat))
    (setf  (sp-special-solution (sp-solutions sp-mat)) a-special-solution)))

;
;(defun sp-solve  (sp-mat &rest l &aux reset-list-of-columns-flag reduce-flag)
;  (keyword-extract l vv nil ((:reduce reduce-flag)
;			     (:reset-list-of-columns-occurring reset-list-of-columns-flag)))
;  (cond (reset-list-of-columns-flag ( sp-reset-list-of-all-columns-occurring sp-mat)))
;  (cond ((or reduce-flag (null (sp-reduced sp-mat)))
;	 ( sp-clear-pivots sp-mat)
;	 ( sp-reduce sp-mat)))
;
;  (setf (sp-columns-with-no-pivot sp-mat)
;	(loop for u in (sp-list-of-all-columns-occurring sp-mat)
;	      when (null (gethash  u (sp-columns-used-to-pivot sp-mat))) collecting u))
;  (let*	((number-of-columns (length (sp-list-of-all-columns-occurring sp-mat)))
;	 (number-of-pivots ( sp-number-of-pivots sp-mat))
;	 (number-of-solutions (- number-of-columns number-of-pivots))
;	 a-solution a-new-entry a-special-solution
;	 (solution-rows  (make-array (length (sp-columns-with-no-pivot sp-mat)) :fill-pointer 0 :adjustable t)))
;    (cond ((not (equal (length (sp-columns-with-no-pivot sp-mat)) number-of-solutions))
;	   (format t "~%There are ~D columns and ~D pivots and ~D columns with no pivot,
;something is wrong" (length (sp-list-of-all-columns-occurring sp-mat)) number-of-pivots (length (sp-columns-with-no-pivot sp-mat)))))
;
;    (cond
;      ((>  number-of-solutions 0)
;       (with-characteristic
;	 (loop for u in  (sp-columns-with-no-pivot sp-mat)
;	       do (setf a-solution (make-array (* (1+ number-of-pivots) 2)
;					       :leader-list (list 0 u)))
;
;	       (array-push solution-rows a-solution)
;	     (fmat t "~%solution-rows ~A"  (listarray solution-rows))
;	       (set-entry 1 a-solution u)
;	       (loop for nn downfrom (1- (row-length (sp-rows sp-mat))) downto 0
;		     when (aref (sp-column-used-in-row sp-mat) nn)
;		     do
;		     (setf (sp-pivot-entry sp-mat) ( sp-entry sp-mat nn (aref (sp-column-used-in-row sp-mat) nn)))
;		     (setq a-new-entry (special-times -1
;					  (special-inverse  (sp-pivot-entry sp-mat))
;					 ( sp-row-dot sp-mat a-solution (aref (sp-rows sp-mat) nn))))
;;		      (+ ( sp-row-dot sp-mat a-solution (aref (sp-rows sp-mat) nn))
;;			 (cond ((null (sp-constants-column sp-mat)) 0)
;;			       (t (aref (sp-constants-column sp-mat) nn))))))
;		     (cond ((not ($zerop  a-new-entry))
;			    (array-push a-solution (aref (sp-column-used-in-row sp-mat) nn))
;			    (array-push a-solution a-new-entry))))))))
;    (cond ((sp-constants-column sp-mat)
;	   (loop for i below (array-total-size (sp-column-used-in-row sp-mat))
;		 when (null (aref (sp-column-used-in-row sp-mat) i))
;		 do
;		 (cond ((not ($zerop  (aref (sp-constants-column sp-mat) i)))
;			(format t "~%~%  ******************************~%")
;		(format	  t "~%Row ~D has no pivot but the (sp-constants-column sp-mat) has entry ~D.
;                   ~%WARNING.  The equations were INCONSISTENT.
;                   ~%The special-solution is NOT VALID.
;                   ~%  ******************************" i (aref  (sp-constants-column sp-mat) i)))))
;	   (with-characteristic
;	     (setq a-special-solution (make-array (* (1+ number-of-pivots) 2)
;						  :leader-list (list 0 )))
;	     (loop for nn downfrom (1- (row-length (sp-rows sp-mat))) downto 0
;		   when (aref (sp-column-used-in-row sp-mat) nn)
;		   do
;		   (setf (sp-pivot-entry sp-mat) ( sp-entry sp-mat nn (aref (sp-column-used-in-row sp-mat) nn)))
;		   (setq a-new-entry
;			 (special-times -1
;			   (special-inverse  (sp-pivot-entry sp-mat))
;			   (special-plus ( sp-row-dot sp-mat a-special-solution (aref (sp-rows sp-mat) nn))
;					 (aref (sp-constants-column sp-mat) nn))))
;		   (cond ((not ($zerop  a-new-entry))
;			  (array-push a-special-solution (aref (sp-column-used-in-row sp-mat) nn))
;			  (array-push a-special-solution a-new-entry)))))))
;    (cond ((and  (sp-solutions sp-mat)(typep (sp-solutions sp-mat) 'sparse-matrix))
;;;why do they not work
;;	   ( sp-set-rows sp-mat solution-rows )
;;	   ( sp-set-type-of-entries sp-mat (sp-type-of-entries sp-mat)))
;           (sp-set-rows (sp-solutions sp-mat) solution-rows)
;	   (sp-set-type-of-entries (sp-solutions sp-mat) (sp-type-of-entries sp-mat))
;;	   (setf (sp-rows (sp-solutions sp-mat))solution-rows)
;;	   (setf (sp-type-of-entries (sp-solutions sp-mat)  ) (sp-type-of-entries sp-mat))
;	   )
;	  (t (setf (sp-solutions sp-mat) (make-sparse-matrix rows solution-rows
;					    type-of-entries (sp-type-of-entries sp-mat)))))
;    (setf  (sp-special-solution (sp-solutions sp-mat)) a-special-solution)))

(defun bring-to-left-side (possible-eqn)
  "Converts a=b into a-b, or a into a also doing nothing if b is 0 "
  (cond ((atom possible-eqn) possible-eqn)
	((eq (caar possible-eqn) 'mequal)
	 (cond ((eql (third possible-eqn) 0)(second possible-eqn))
	       (t (sub (second possible-eqn) (third possible-eqn)))))
	(t possible-eqn)))



(defun sp-show-solutions(sp-mat )
   ( sp-show-matrix sp-mat))

(defun sp-verify-solutions (sp-mat  &aux tem temm spec)
  (loop for i below (sp-number-of-rows sp-mat)
	do
	(loop for j below (sp-number-of-rows (sp-solutions sp-mat))
	      do
	      (format t "~%The dot product of row ~D and solution ~D is ~D"
		      i j ( sp-row-dot sp-mat
			   ( sp-row sp-mat i)
			   (sp-row (sp-solutions sp-mat) j)))))
  (cond ((setq spec (sp-special-solution (sp-solutions sp-mat)))
	 (let ((const (sp-constants-column sp-mat)))
	   (loop for i below (sp-number-of-rows sp-mat)
		 do
		 (setq tem ( sp-row-dot sp-mat
				( sp-row sp-mat i)
				spec))
		 (format t "~%The dot product of the special solution and row ~D is ~A while the constant is ~A"
			 i tem (aref const i))
		 (iassert (or ($zerop(setq temm (special-plus (aref const i)   tem )))
			      (and (numberp temm)
				   (< (abs temm) .00001))))
		 finally (format t "~%The special solution is ok"))))))
(defun nil-lessp (x y)
  (cond ((null y)  (cond ((null x) nil)
			 (t t)))
	((null x) nil)
	(t (< x y))))



(defun sp-sort-row (sp-mat i)
  (sort-grouped-array  (aref (sp-rows sp-mat) i) 2 'nil-lessp))



(defmacro may-have-pivot (row-number)
  `(not (eql 1 (aref (sp-rows-with-no-pivot sp-mat) ,row-number))))

(defun desirable-row (x) x nil)			;
;;?? fix array-leader stuff..
;(defun desirable-row (x) (eq (array-leader-length x) 2))

(defun sp-best-pivot (sp-mat &aux  value temp-col)
  (setf (sp-pivot-entry sp-mat) nil)
  (setf (sp-row-number-before-swap sp-mat) nil)
  (fmat t "~%What is best pivot in Row ~D?  " (sp-pivot-row-number sp-mat))
  (cond
    ((eql 1 (aref (sp-rows-with-no-pivot sp-mat) (sp-pivot-row-number sp-mat)))
     (fmat t "Row ~D was previously found to have no possible pivot."
	     (sp-pivot-row-number sp-mat)))
    (t
     (catch 'done
       (loop
	 for test-name in (sp-pivot-test-list sp-mat)
	 when (may-have-pivot (sp-pivot-row-number sp-mat))
	 do
	 (setf (sp-last-good-row sp-mat) (1- (sp-number-of-rows sp-mat)))
	 (let ((fresh-row t)
	       best no-gcd test col-to-pivot)
	   (loop
	     while fresh-row
	     do
	     (cond
	       ((and (desirable-row (sp-pivot-row sp-mat))
		     (equal (sp-type-of-entries sp-mat) :integer))
		(setf test-name 'gcd-column)))
	     (case test-name
	       (any (setq test (function (lambda (ignor  ignor1)ignor
					   ignor1 t)))
		    (setq best 1))
	       (:any-macsyma ( sp-best-pivot-macsyma sp-mat )
			    (throw 'done 'here-maybe-pivot))
	       (gcd (multiple-value (best no-gcd)
		      (catch-error ( sp-gcd-row sp-mat
				    (sp-pivot-row-number sp-mat)) nil))
		    (setq test (function equal)))
	       (min (setq best ( sp-smallest-possible-pivot sp-mat))
		    (setq test (function equal)))
	       (gcd-column (setq best ( sp-smallest-possible-pivot sp-mat))
			   (setq test (function equal)))
	       (unit (cond ((numberp (sp-type-of-entries sp-mat)) (setq best 1)
			    (setq test (function (lambda (ignor  ignor1)ignor
						   ignor1 t))))
			   (t (setq best 1) (setq test (function equal)))))
	       (otherwise (merror "~A is not a possible test." test-name)))
	     (setq value nil)
	     (cond
	       (no-gcd nil)
	       ((or   (null best) ($zerop  best))
		(fmat t "~%Row ~D was found to be zero while looking for ~A."
			(sp-pivot-row-number sp-mat)
			test-name)
		( sp-put-back-row-with-no-pivot sp-mat))
	       ((and (eq test-name 'gcd) no-gcd) nil)
	       (t
		(with-once-only
		  ((sp-pivot-row sp-mat))
		  (loop
		    for ii below (length (the cl:array (sp-pivot-row sp-mat))) by 2
		    when (aref (sp-pivot-row sp-mat) ii)
;				 (and (aref (sp-pivot-row sp-mat) ii)
;					   (null (gethash  ;;these entries should be zero!!
;						   (aref (sp-pivot-row sp-mat) ii )
;						   (sp-columns-used-to-pivot sp-mat))))
		    do  (setq value (abs (aref (sp-pivot-row sp-mat) (1+ ii ))))

		    (cond ((funcall test value best)
			   (setq col-to-pivot (aref (sp-pivot-row sp-mat) ii))
			   (cond ((equal test-name 'gcd-column)
				  (cond
				    ((and
				       (desirable-row (sp-pivot-row sp-mat))
				       (setq temp-col
					     (
					      sp-find-good-column-to-pivot sp-mat)))
				     (setq col-to-pivot temp-col)))

				  (  sp-force-pivot-row-to-contain-gcd sp-mat
				   col-to-pivot)
				  (setf (sp-pivot-entry sp-mat)
					(sp-entry sp-mat (sp-pivot-row-number sp-mat)
									  col-to-pivot))
				  (setf (sp-pivot-row sp-mat)
					(aref (sp-rows sp-mat)
					      (sp-pivot-row-number sp-mat))))
				 (t (setf (sp-pivot-entry sp-mat)
					  (aref (sp-pivot-row sp-mat) (1+ ii) ))))
			   ( sp-enter-pivot-data sp-mat col-to-pivot )
			   (throw 'done 'here-is-pivot)))))))
	     (cond ( no-gcd nil)
		   ((null value)
		    ( sp-put-back-row-with-no-pivot sp-mat)
		    (fmat t "It is zero.")))
;			 (fmat t "~%Row ~D has no possible pivot." (sp-pivot-row-number sp-mat))
;						(aset 1 (sp-rows-with-no-pivot sp-mat) (sp-pivot-row-number sp-mat))
;						(throw 'done 'no-pivot)))
;		  (cond ((member test-name '(min any gcd-column) :test #'eq)
;			 (fmat t "There  is no possible pivot in row ~D." (sp-pivot-row-number sp-mat))
;			 (aset 1 (sp-rows-with-no-pivot sp-mat) (sp-pivot-row-number sp-mat))))
	     (cond (no-gcd (format t "~%Row ~D has no gcd." (sp-pivot-row-number sp-mat)))
		   ((eq test-name 'gcd)
		    (fmat t "~%Row ~D has gcd ~D but no pivot of that size."
			    (sp-pivot-row-number sp-mat) best ))
		   ((eq test-name 'unit)
		    (fmat t "~%Row ~D does not have a unit for a pivot."
			    (sp-pivot-row-number sp-mat))))
	     (setf fresh-row ( sp-swap-pivot-row-with-later-one sp-mat)))))))))

(defmacro swap-rows-and-constants (m n)
  `(prog (tem) (rotatef (aref (sp-rows sp-mat) ,m) (aref (sp-rows sp-mat) ,n))
	 (setf (sp-sign-of-row-permutation sp-mat) (- (sp-sign-of-row-permutation sp-mat)))
	 (cond ((setq tem (sp-current-column-above-pivot-row-number sp-mat))
		(rotatef (aref tem ,m) (aref tem ,n))))
	 ;(cond (-boundp sign)(setq sign (- sign))) ;;keep track of sign for det
	 (cond ((sp-constants-column sp-mat)
		(rotatef (aref (sp-constants-column sp-mat) ,m) (aref (sp-constants-column sp-mat) ,n))))))

(defun sp-put-back-row-with-no-pivot(sp-mat )
  (cond ((sp-row-number-before-swap sp-mat)
	 (swap-rows-and-constants (sp-pivot-row-number sp-mat) (sp-row-number-before-swap sp-mat))
		;(rotatef (aref (sp-rows sp-mat) (sp-pivot-row-number sp-mat)) (aref (sp-rows sp-mat) (sp-row-number-before-swap sp-mat) ))
	 (fmat t "There is no pivot in row ~D so exchanging it back"
		 (sp-row-number-before-swap sp-mat) )
	 (setf (sp-pivot-row sp-mat) (aref (sp-rows sp-mat) (sp-pivot-row-number sp-mat)))
	(setf (aref (sp-rows-with-no-pivot sp-mat) (sp-row-number-before-swap sp-mat) )  1)
	 (setf (sp-row-number-before-swap sp-mat) nil))))

(defun sp-swap-pivot-row-with-later-one(sp-mat )
  "Returns t if it did nil if it did not"
  (catch 'exchanged
    (loop for j downfrom (sp-last-good-row sp-mat)
	  until (<= j (sp-pivot-row-number sp-mat))
	  when  ($zerop  (aref (sp-rows-with-no-pivot sp-mat) j))
	  do (swap-rows-and-constants (sp-pivot-row-number sp-mat) j)
	  ;(rotatef (aref (sp-rows sp-mat) (sp-pivot-row-number sp-mat)) (aref (sp-rows sp-mat) j))
	  (setf (sp-pivot-row sp-mat) (aref (sp-rows sp-mat) (sp-pivot-row-number sp-mat)))

	(setf (sp-last-good-row sp-mat) (1- j) )
	(setf (sp-row-number-before-swap sp-mat) j)
	(fmat t " Exchanging Row ~D for Row ~D." (sp-pivot-row-number sp-mat) j)
	(throw 'exchanged t))
    nil))
;
;(defun sp-rat (x)($vrat x))
;;the new generic functions n* n+ etc. do not require rat form entries, they convert to
;;them.
;
;(defun sp-best-pivot-macsyma(sp-mat )
;  (loop for i below (array-active-length (sp-pivot-row sp-mat)) by 2
;	when (aref (sp-pivot-row sp-mat) i)
;	do
;	(setf (sp-pivot-entry sp-mat) (aref (sp-pivot-row sp-mat) (1+ i)))
;	(cond ((numberp (sp-pivot-entry sp-mat)) nil)
;	      (t (cond (($numberp (sp-pivot-entry sp-mat))
;			(setf (sp-pivot-entry sp-mat)
;			      (cond ((atom (sp-pivot-entry sp-mat))(sp-pivot-entry sp-mat))
;				    ((or (polynomialp (sp-pivot-entry sp-mat))
;					 (rational-functionp (sp-pivot-entry sp-mat)))
;				     (sp-pivot-entry sp-mat))
;				    (t
;			      ($ratsimp (sp-pivot-entry sp-mat))))))
;		       (t
;			(setq (sp-pivot-entry sp-mat) (sp-rat (sp-pivot-entry sp-mat)))
;			))
;		 (aset  (sp-pivot-entry sp-mat) (sp-pivot-row sp-mat) (1+ i) )))
;	(send self :enter-pivot-data (aref (sp-pivot-row sp-mat) i))
;	(return 'done)
;	finally (cond (( sp-swap-pivot-row-with-later-one sp-mat)
;		       (send self :best-pivot-macsyma)))))
;
;(defun sp-best-pivot-macsyma(sp-mat )
;  (cond ((loop for i below (array-active-length (sp-pivot-row sp-mat)) by 2
;	       when (and (aref (sp-pivot-row sp-mat) i)
;			 ($numberp
;			   (aref (sp-pivot-row sp-mat) (1+ i))))
;	       do
;	       (setq (sp-pivot-entry sp-mat) (aref (sp-pivot-row sp-mat) (1+ i)))
;	       (send self :enter-pivot-data (aref (sp-pivot-row sp-mat) i))
;	       (return 'done)))
;	(t
;	 (loop for i below (array-active-length (sp-pivot-row sp-mat)) by 2
;	       when  (aref (sp-pivot-row sp-mat) i)
;	       do
;	       (setq (sp-pivot-entry sp-mat) (aref (sp-pivot-row sp-mat) (1+ i)))
;	       (send self :enter-pivot-data (aref (sp-pivot-row sp-mat) i))
;	       (return 'done)
;	       finally (cond ((send self :swap-pivot-row-with-later-one)
;			      (send self :best-pivot-macsyma)))))))


(defun sp-best-pivot-macsyma(sp-mat )
  (loop named sue for test in (list #'(lambda (x) (or (eql x -1)(eql x 1)))
				    #'(lambda (x) (and (numberp x) (< (abs x) 100)))
				    #'(lambda (x) (numberp x))
				    #'(lambda (x) ($numberp x))
				    #'(lambda (ignor) ignor t))
	do
	(loop for i below (length (the cl:array (sp-pivot-row sp-mat))) by 2
	      when (and (aref (sp-pivot-row sp-mat) i)
			(funcall test
				 (aref (sp-pivot-row sp-mat) (1+ i))))
	      do
	      (setf (sp-pivot-entry sp-mat) (aref (sp-pivot-row sp-mat) (1+ i)))
	      ( sp-enter-pivot-data sp-mat (aref (sp-pivot-row sp-mat) i))
	      (return-from sue 'done))
	finally (cond (( sp-swap-pivot-row-with-later-one sp-mat)
		       ( sp-best-pivot-macsyma sp-mat)))))



(defun sp-smallest-possible-pivot(sp-mat &aux
				  (.pivot-row. (sp-pivot-row sp-mat)))
  (loop for ii below (length (the cl:array  .pivot-row.)) by 2
	when (aref .pivot-row. ii)
		       minimize (abs (aref .pivot-row. (1+ ii )))))
;
;(defun test (m n   &key mat
;	     (type :rational) solve
;	     constants constants-column sparse-matrix &aux  const sp)
;  (cond (sparse-matrix (setq sp sparse-matrix))
;	(t (setq sp (make-sparse-matrix))))
;  (cond (mat nil)
;	(t
;	 (setq mat (random-matrix m n))))
;  (cond (constants
;	 (setq
;	   constants-column
;	   (make-array m
;		       :fill-pointer m))
;	 (loop for i below m
;	       do (setf (aref  constants-column i) (random 6)))))
;
;  (sp-get-rows-from-array sp mat type)
;  (setf (sp-constants-column sp) constants-column)
;  (sp-show-matrix sp)
;  (cond (solve
;	 (cond ((eq type :float) (sp-float sp)))
;	 (if constants	 (setq const (copy-atomic-structure  constants-column 4)))
;	 (sp-solve sp :reduce t)
;         (sp-get-rows-from-array sp mat type)
;	 (cond ((eq type :float) (sp-float sp)))
;	 (setf (sp-constants-column sp) const)
;	 (format t "~%Verifying the solutions with the original matrix and constants:")
;	 (sp-verify-solutions sp))
;	(t
;	 (sp-reduce sp)))
;  (sp-show-matrix sp)
;
;  mat )
(defun sp-verify-solutions-for-original-array (sp-mat aarray)
  ( sp-get-rows-from-array sp-mat aarray (sp-type-of-entries sp-mat))
  ( sp-verify-solutions sp-mat))


(defun sp-init (sp-mat plist)
  (let ((initial-data (get plist ':rows))
	(atype-of-entries (get plist ':type-of-entries))
	(asolutions (get plist :solutions))
	(acolumn-used-in-row (get plist :column-used-in-row))
	(acolumns-used-to-pivot (get plist :columns-used-to-pivot)))
    (if initial-data ( sp-set-rows sp-mat initial-data))
    (if atype-of-entries ( sp-set-type-of-entries sp-mat atype-of-entries))
    (if asolutions (setf (sp-solutions sp-mat) asolutions))
    (if acolumns-used-to-pivot
	(setf (sp-columns-used-to-pivot sp-mat) acolumns-used-to-pivot))
    (if acolumn-used-in-row
	(setf (sp-column-used-in-row sp-mat) acolumn-used-in-row))))

;(once (piv div ) (setq div 4)(setq piv 3));;watch out if control leaves in middle of body the
		;values of the variables may not be restored!!

;;The t in the instantiate-flavor tells it to send the :init message to
;;the newly created flavor.  It uses the values present in the options-present
;;list.  The catch-error is for those instance variables which may be unbound.
;;We do not want them on the options-present list.

;(defun sp-fasd-form (sp-mat )
;  (let ((options-present
;	  (loop for u in (list :solutions :column-used-in-row
;		:columns-used-to-pivot :rows :type-of-entries)
;		when (catch-error ( u) nil)
;		appending (list u (send self u)))))
;    (setf options-present (cons nil options-present))
;  `(instantiate-flavor 'sparse-matrix ',options-present t)))

(defun sp-make-transpose (sp-mat )
  "Creates the transpose of the sparse-matrix.  The result is stored in the transpose
   instance-variable.  The original column is stored in slot 1 of the array-leader of the row"
  (let ((transpose-rows (MAKE-ARRAY
			  (length (sp-list-of-all-columns-occurring sp-mat)) :adjustable t))
	rowj val)
    (setf (sp-list-of-all-columns-occurring sp-mat)
	  (sort (sp-list-of-all-columns-occurring sp-mat) '<))
    (loop for j in (sp-list-of-all-columns-occurring sp-mat)
	  for i below (length (sp-list-of-all-columns-occurring sp-mat))
	  do
	  (setq rowj (make-solution-row))
	  (setf (solution-row-data rowj ) (MAKE-ARRAY 100 :fill-pointer 0 :adjustable t))
	  (setf (solution-row-number rowj) j)
	  (loop for ii below (sp-number-of-rows sp-mat)
		when (setq val ( sp-entry sp-mat ii j))
		do
		(array-push-extend-replace (solution-row-data rowj) ii)
		(array-push-extend-replace (solution-row-data rowj) val))
	 (setf (aref transpose-rows i) (solution-row-data rowj)))

    (setf (sp-transpose sp-mat) (make-sparse-matrix
				    :rows transpose-rows
				    :type-of-entries (sp-type-of-entries sp-mat)))))
;conflicts never called
;(defmacro make-sparse-matrix (aarray name)
; ` (progn (setf ,name (make-instance 'sparse-matrix))
;  (send ,name :get-rows-from-array ,aarray)))


;;   To handle integer matrices properly we will have to find the pivot
;;in a given column.  To do this we will find the gcd of that column.
;;Then find a row where it occurs or create a linear combination of rows
;;where it occurs. WE add that to the set of rows.  Then use that row to
;;clean the given column.  The span over Z of the set of rows is the
;;same as before (since adding a row did not hurt and cleaning out with
;;respect to a given row is a reversible operation, since all entries in
;;that column are integer multiples of the given entry).

;;   We repeat the process applying it to the "set of rows occurring
;;after our row and to the remaining columns" Note that the previous
;;pivot column does not occur anyway.  Eventually we end up with a set
;;of rows which span the same lattice as the original, but which are
;;clearly independent over the rationals.  Therfore the number of them
;;must be correct and they must be a basis.

;;   Alternateley we could take our row and choose some column say j.
;;Then we could perform row' -q*row + remainder.  We would then replace
;;row' by remainder.  Here row' is another row that has an entry in
;;column j which is not a multiple of the the entry in row column j.
;;The remainder will have an entry smaller than that of row. Then we swap
;; the new row' and row.  If row now has entry in column j
;;equal to the gcd of the column, fine we use the remainder as our
;;pivot.  Otherwise we repeat with remainder taking the place of row and
;;we find some row' whose entry in column j is not a multiple of the
;;entry in row column j.

(defun sp-set-current-column-above-pivot-row-number  (sp-mat j)
;  (setq current-column-number j)
  (cond ((sp-current-column-above-pivot-row-number sp-mat)
	 (setf (sp-current-column-above-pivot-row-number sp-mat)
	       (adjust-array (sp-current-column-above-pivot-row-number sp-mat)
			     (sp-number-of-rows sp-mat)
			     :fill-pointer
			     (fill-pointer
			      (sp-current-column-above-pivot-row-number
			       sp-mat))
			     ))
	 (fillarray (sp-current-column-above-pivot-row-number sp-mat) nil))
	(t (setf (sp-current-column-above-pivot-row-number sp-mat)
		 (MAKE-ARRAY (sp-number-of-rows sp-mat) :adjustable t ))))
	   (loop for i from (sp-pivot-row-number sp-mat) below (sp-number-of-rows sp-mat)
		 do(setf (aref
			  (sp-current-column-above-pivot-row-number sp-mat)  i)  ( sp-entry sp-mat i j))))

(defun sp-set-constants-column (sp-mat j &aux this-row)
  (setf (sp-constants-column-number sp-mat) j)
  (cond ((sp-constants-column sp-mat)
	 (if (< (array-total-size (sp-constants-column sp-mat))
		(array-total-size (sp-rows sp-mat)))
	     (setf (sp-constants-column sp-mat)
		   (adjust-array (sp-constants-column sp-mat)
				  (array-total-size (sp-rows sp-mat))
				 :fill-pointer
				 (fill-pointer (sp-constants-column sp-mat))
				   ))))
	(t (setf (sp-constants-column sp-mat)
		 (MAKE-ARRAY (array-total-size (sp-rows sp-mat))
			     :fill-pointer 0 :adjustable t))))
  (loop for i below (array-total-size (sp-constants-column sp-mat))
	 do (setf  (aref (sp-constants-column sp-mat) i) 0))
  (setf (sp-list-of-all-columns-occurring sp-mat)
	(delete (sp-constants-column-number sp-mat)
		(sp-list-of-all-columns-occurring sp-mat) :test #'equal))
  (loop for i below (sp-number-of-rows sp-mat)
	do
	(setf this-row (aref (sp-rows sp-mat) i))
	(loop for ii below (length (the cl:array this-row)) by 2
	      when (eql (aref this-row ii) j)
	      do
	     (setf (aref this-row ii)  nil)
 (setf	(aref (sp-constants-column sp-mat) i)  (aref this-row (1+ ii))))))

;(defun where-the-min (an-array &aux temp the-min where-the-min)
;  "Where an-array has its minimum"
;  (loop for i below (array-total-size an-array) do
;	(cond ((setq the-min (aref an-array i)) (setq where-the-min i) (return 'done))))
;    (cond ((null where-the-min) nil)
;	(t (setq the-min (abs the-min))
;	   (loop for i below (array-total-size an-array)
;		 when (and (setq temp (aref an-array i)) (< (abs temp) the-min))
;		 do (setq where-the-min i))))
;  where-the-min)

(defun where-the-min (an-array &aux null-vector tem the-min where-the-min)
  (loop for i below (array-total-size an-array)
	when (aref an-array i)
	do (setq the-min (abs (aref an-array i)))
	(setq where-the-min i)
	(return 'done)
	finally (setq null-vector t))
  (cond ((Not null-vector)
	 (loop for i below (array-total-size an-array)
	       when (setq tem (aref an-array i))
	       do
	       (cond ((< (abs tem ) the-min)(setq where-the-min i)
		      (setq the-min (abs tem))))
	       finally (return where-the-min)))
	(t nil)))

(defun sp-force-pivot-row-to-contain-gcd (sp-mat j &aux
	 temp
	 piv-entry
	 tem constant crow
	 gcdfacts new-row
	 the-gcd where-the-min  the-min-gcd where-the-min-gcd)
  "J is column number.  Elementary row operations and swaps of rows
   are performed with rows occurring after (sp-pivot-row-number sp-mat) to ensure that
   the pivot-row contains the gcd of the entries in rows greater than
   the (sp-pivot-row-number sp-mat) and in column j."
  (declare (special *verbose*))
  ( sp-set-current-column-above-pivot-row-number sp-mat j)
  (let* ((colj (sp-current-column-above-pivot-row-number sp-mat))
	(first-entry (aref colj (sp-pivot-row-number sp-mat))))

    (cond ((not (eql colj (sp-current-column-above-pivot-row-number sp-mat))) (merror 'wow)))
    (setq temp first-entry)
    (setq the-gcd temp)
    (loop for i below (row-length
			(sp-current-column-above-pivot-row-number sp-mat))
	  when (setq temp (aref colj i))
	  do (setq the-gcd (gcd temp the-gcd)))

    (loop
      do
      (setq where-the-min (where-the-min colj))

      (cond ((not (eql where-the-min (sp-pivot-row-number sp-mat)))
	     (swap-rows-and-constants (sp-pivot-row-number sp-mat)
				      where-the-min)
	    ; (rotatef (aref (sp-rows sp-mat) (sp-pivot-row-number sp-mat)) (aref (sp-rows sp-mat) where-the-min))
	     ;(rotatef (aref colj (sp-pivot-row-number sp-mat)) (aref colj where-the-min))
	     (fmat t "~%Exchanging rows ~D and ~D to help force gcd ~D into pivot-row."
		     (sp-pivot-row-number sp-mat) where-the-min the-gcd)
	     ( sp-set-pivot-row sp-mat (sp-pivot-row-number sp-mat))))
      (cond
	((equal the-gcd (abs (null-to-zero (aref colj  (sp-pivot-row-number sp-mat) ))))
	 (fmat t "~%Row ~D has ~D in column ~D which is the gcd of the column above row ~D."
	       (sp-pivot-row-number sp-mat)
	       (aref colj  (sp-pivot-row-number sp-mat)) j (sp-pivot-row-number sp-mat))
	 (return the-gcd))
	(t (setq piv-entry (aref colj (sp-pivot-row-number sp-mat)))
	   (setq  the-min-gcd (abs piv-entry))

	   (loop for i below (array-total-size colj)
		 when (aref colj i)
		 do

		 (cond((eql (setq tem (abs (gcd piv-entry (aref colj i)))) the-gcd)
		       (setq where-the-min-gcd i)(return i)))
		 (cond ( (< tem the-min-gcd)
			(setq the-min-gcd tem)
			(setq where-the-min-gcd i))))

	   (setq gcdfacts (general-gcd piv-entry (aref colj where-the-min-gcd)))
	   ;;add new row (first gcdfacts)*pivot-row +(second gcdfacts)*(sp-row self (+ pivot-row-number where-the-min-gcd)
	   ;;and fix  constants. redo force pivot row;
;	   (show (listarray colj))
;	   (sp-show-matrix sp-mat)
	   (sp-set-current-row sp-mat where-the-min-gcd)
	   (setq crow (sp-current-row sp-mat))
	   (setq new-row
		 (MAKE-ARRAY (fill-pointer crow) :adjustable t :fill-pointer (fill-pointer crow)))
	   (fillarray new-row crow)
	   (cond ((setq tem (sp-constants-column sp-mat))
		  (setq constant (aref tem (sp-current-row-number sp-mat)))))

	   (sp-add-new-row sp-mat new-row :constant constant)
	   (sp-multiply-row sp-mat (sp-current-row-number sp-mat) (second gcdfacts))
	   (sp-row-operation sp-mat (first gcdfacts) )
   	   (cond (*verbose* (format t "~%The gcd is ~A Adding a new row with  entry in col ~A of ~A" the-gcd j
				    (sp-entry sp-mat (sp-current-row-number sp-mat) j )  )))
;	   (sp-show-matrix sp-mat)
;	   (show (listarray colj))
	   (sp-force-pivot-row-to-contain-gcd sp-mat j)(return 'start-over)))))

  'done)

(defun sp-add-new-row (self a-row &key constant &aux ok)
  (let ((dim (1+ (sp-number-of-rows self))))
    (loop for i below (1- dim)
	  do
	  (maybe-move-back-fill-pointer (sp-row self i))
	  when (eql (fill-pointer (sp-row self i)) 0)
	  do
	  (cond (constant (cond ((eql 0 (null-to-zero (aref (sp-constants-column self)
							     i)))
				 (setf (aref (sp-constants-column self) i) constant)
				 (setq ok t))))
		(t (setq ok t)))
	  (cond (ok
				 (setf (aref (sp-rows-with-no-pivot self) i) 0)
				 (setf (sp-row self i) a-row))))
    (cond ((not ok)
	  (setf  (sp-current-column-above-pivot-row-number self) nil)
    (setf (sp-rows-with-no-pivot self)
	  (array-grow (sp-rows-with-no-pivot self)  dim))
    (setf (sp-column-used-in-row self)
	  (array-grow (sp-column-used-in-row self)  dim))
    (vector-push-extend   a-row (sp-rows self))
    (setf  (sp-number-of-rows self) dim)
    (cond (constant (vector-push-extend  constant (sp-constants-column self))))))))

;
;(defun sp-force-pivot-row-to-contain-gcd (sp-mat j)
;  "J is column number.  Elementary row operations and swaps of rows
;   are performed with rows occurring after (sp-pivot-row-number sp-mat) to ensure that
;   the pivot-row contains the gcd of the entries in rows greater than
;   the (sp-pivot-row-number sp-mat) and in column j."
;  ( sp-set-current-column-above-pivot-row-number sp-mat j)
;  (let* ((colj (sp-current-column-above-pivot-row-number sp-mat))
;	(first-entry (aref colj (sp-pivot-row-number sp-mat))) temp
;	 rem factor  where-the-smallest-remainder
;	 the-gcd where-the-min smallest-remainder )
;    (setq temp first-entry)
;    (setq the-gcd temp)
;    (loop for i below (row-length (sp-current-column-above-pivot-row-number sp-mat))
;	  when (setq temp (aref colj i))
;	  do (setq the-gcd (gcd temp the-gcd)))
;    (loop
;      do
;      (setq where-the-min (where-the-min colj))
;      (cond ((not (eql where-the-min (sp-pivot-row-number sp-mat)))
;	     (swap-rows-and-constants (sp-pivot-row-number sp-mat) where-the-min)
;	    ; (rotatef (aref (sp-rows sp-mat) (sp-pivot-row-number sp-mat)) (aref (sp-rows sp-mat) where-the-min))
;	     ;(rotatef (aref colj (sp-pivot-row-number sp-mat)) (aref colj where-the-min))
;	     (fmat t "~%Exchanging rows ~D and ~D to help force gcd ~D into pivot-row."
;		     (sp-pivot-row-number sp-mat) where-the-min the-gcd)
;	     ( sp-set-pivot-row sp-mat (sp-pivot-row-number sp-mat))))
;      (cond
;	((equal the-gcd (abs (aref colj  (sp-pivot-row-number sp-mat) )))
;	 (fmat t "~%Row ~D has ~D in column ~D which is the gcd of the column above row ~D."
;		 (sp-pivot-row-number sp-mat) (aref colj  (sp-pivot-row-number sp-mat)) j (sp-pivot-row-number sp-mat))
;	 (return the-gcd))
;	(t
;	 (setq smallest-remainder (abs (aref colj  (sp-pivot-row-number sp-mat))))
;	 (loop for i below (array-total-size colj)
;	       when (and
;		      (aref colj i)
;		      (not ($zerop  (setq rem (mod (aref colj i)
;						    (aref colj (sp-pivot-row-number sp-mat))))))
;			 (< rem smallest-remainder))
;	       do (setq where-the-smallest-remainder i) (setq smallest-remainder rem))
;	 (setq factor (special-times -1
;				      (aref colj where-the-smallest-remainder)
;				(special-inverse       (aref colj (sp-pivot-row-number sp-mat)))))
;	 ( sp-set-current-row sp-mat where-the-smallest-remainder)
;	 ( sp-row-operation sp-mat factor)
;	 (aset (special-plus (special-times (aref colj (sp-pivot-row-number sp-mat))
;					    factor)
;			     (aref colj (sp-current-row-number sp-mat)))
;	       colj (sp-current-row-number sp-mat)))))
;    (aref colj (sp-pivot-row-number sp-mat))))

(defun gcd-array (an-array &aux where the-gcd temp)
 "Gcd of all non-nil elements occurring in an array"
  (loop for ii below (array-total-size an-array)
	until (setq the-gcd (aref an-array ii)))
  (loop for ii below (array-total-size an-array)
	when (setq temp (aref an-array ii))
	do (setq the-gcd (gcd the-gcd temp))
	(cond ((equal the-gcd (abs temp)) (setq where ii)
	       (cond ((eql the-gcd 1) (return 'done))))))
  (values the-gcd where))

(defun rational-inverse (x)
  (sp-rational-quotient 1 x))

(defun divides (a b)
  ($zerop (mod b a)))

(defun sp-find-good-column-to-pivot (sp-mat  &aux the-gcd)
  "Trys to find a column where the gcd is equal to the entry of the pivot-row
   without changing the pivot row"
  (loop for ii below (length (the cl:array (sp-pivot-row sp-mat))) by 2
	when (aref (sp-pivot-row sp-mat) ii)
	do ( sp-set-current-column-above-pivot-row-number sp-mat
	    (aref (sp-pivot-row sp-mat) ii))
	(setq the-gcd (gcd-array (sp-current-column-above-pivot-row-number sp-mat)))
	(cond ((eql (abs (aref (sp-pivot-row sp-mat) (1+ ii))) the-gcd)(return (aref (sp-pivot-row sp-mat) ii))))))

(defun sp-gcd-column (sp-mat j )
  (setf (sp-pivot-row-number sp-mat) 0)
  ( sp-set-current-column-above-pivot-row-number sp-mat j)
    (gcd-array (sp-current-column-above-pivot-row-number sp-mat)))

(defun show-gcd-of-columns (a-sparse-matrix)
  (let (the-gcd where (all-col ( sp-list-of-all-columns-occurring  a-sparse-matrix)))
    (loop for i in all-col do
	  (multiple-value (the-gcd where) ( sp-gcd-column  a-sparse-matrix i))
	  (format t "~%Column ~D has gcd ~D at it occurs in row ~A. "i the-gcd where))))

(defun sp-show-row-array-leaders(sp-mat )
  (loop for i below (array-total-size (sp-rows sp-mat))
	do
	(format t "~% Row ~D has array-leader ~A." i (list-array-leader
						       ( sp-row  sp-mat i)))))

(defun sp-make-rows-desirable (sp-mat &rest a-list &aux temp this-row)
  (loop for i in a-list
	do
	(setq this-row (aref (sp-rows sp-mat) i))
;	(setq temp (make-array (array-total-size (aref (sp-rows sp-mat) i))
;			       :leader-list (list (fill-pointer this-row ) nil)))
	(setq temp (make-sparse-solution :row (MAKE-ARRAY (array-total-size  (aref (sp-rows sp-mat) i)) :adjustable t
			       :fill-pointer (fill-pointer this-row ) )))
	(fillarray (sparse-solution-row temp) this-row)
(setf	(aref (sp-rows sp-mat) i)   temp)))

;si:
;(defun-hash-table sp-get-key (sp-mat value)
;  (catch 'key
;    (send self :map-hash `(lambda (u v) (cond ((eq v ,value) (throw 'key u)))))))

(defvar *verbose* nil)
(defun fmat (&rest l)
  (cond (*verbose* (apply 'format l))
	(t nil)))


(defun sp-show-column (sp-mat j)
	   (loop for i below (sp-number-of-rows sp-mat) do
		 (format t "~% entry in row ~D col ~D is ~A" i j ( sp-entry  sp-mat i j))))
(defun sp-verify-columns-above-pivots-are-zero(sp-mat &aux (ok t) col )
  "Verifies the entries above the pivot are indeed 0"
  (loop for i below (sp-number-of-rows sp-mat)
	when (setq col (aref (sp-column-used-in-row sp-mat) i))
	do
	(setq ok t)
	(loop for ii from (1+ i) below (sp-number-of-rows sp-mat)
	      when ( sp-entry  sp-mat ii col)
	      do (format t "~%Pivot in row ~A has nonzero entry above it in column ~A row ~D."
			 i col ii)
	      (setq ok nil))
	(cond (ok
	       (format t "~%For Row ~D column ~D is ok above the pivot" i col)))))

(defun sp-reduce-row-with-respect-to-rows (sp-mat a-row
							      &aux (not-done t)
							      piv-row col factor)
  "Cleans out all the columns in a-row such that rows has a pivot in that column."
  (setf (sp-current-row sp-mat) a-row)
  (setf (sp-current-row-number sp-mat) "Fake row number")
  (loop while not-done
	do
  (loop for ii below (length (the cl:array a-row)) by 2
	when (and (setq col (aref a-row ii))(setq piv-row (gethash  col (sp-columns-used-to-pivot sp-mat)
								)))
	do
	( sp-set-pivot-row  sp-mat piv-row)
	(setf (sp-pivot-entry sp-mat) ( sp-entry sp-mat piv-row col))
	(setq factor
	      (special-times -1
			     (special-inverse (sp-pivot-entry sp-mat))
			     (aref a-row (1+ ii))))
	( sp-row-operation sp-mat factor)
	(return 'try-again)
	finally (setq not-done nil)))
  ( sp-set-current-row sp-mat 0)
  a-row)



;;((0 0 2 1 1 0) (2 6 6 4 0 6) (4 0 5 0 0 5) (4 1 2 0 2 3) (5 6 5 4 5 6) (5 1 0 0 6 6))
;;need to fix this, or the reduce function:  If given an
;;integer matrix it may return a matrix with more rows than
;;it started with,  Actually I guess the reduce function should
;;be fixed for example with the above matrix it returns to big a thing..
(defun sp-determinant(sp-mat &aux answer col entry (correct 0) tem)
  (setf (sp-sign-of-row-permutation sp-mat) 1)
  (cond ((eq (sp-type-of-entries sp-mat) :integer)
	 (format t "Integer type is not good for determinant")
	 (iassert (null (sp-reduced sp-mat)))
	 (sp-set-type-of-entries sp-mat :any-macsyma)))
  ( sp-reduce sp-mat)
  (loop for i below (sp-number-of-rows sp-mat)
	do (show i)
	when (eql 0 (setq tem (aref (sp-column-used-in-row sp-mat) i)))
	do (return 'done)
	when (null tem) do (return (setq  answer 0))
	finally (setq correct -1))
   (cond ((null answer)
	   (setq answer (sp-sign-of-row-permutation sp-mat))
  (loop for i below (sp-number-of-rows sp-mat)
	when (setq col (aref (sp-column-used-in-row sp-mat) i))
	do
	(setq entry ( sp-entry sp-mat i col))
	(setq answer (sp-mul* answer entry))
	and
	collecting (+ col correct) into tem
	else
	do (format t "~%There is no pivot in row ~A" i)(return (setq answer 0 ))
	finally
	(return (setq answer  (sp-mul* answer
					       (sign-of-permutation tem)))))))
  answer)

(defun sp-product-of-pivots (sp-mat &aux entry (answer 1) col )
    (loop for i below (sp-number-of-rows sp-mat)
	when (setq col (aref (sp-column-used-in-row sp-mat) i))
	do
	(setq entry ( sp-entry sp-mat i col))
	(cond (entry
	(setq answer (sp-mul* answer entry)))
	      (t  (format t "~%Zero entry in (~A ,~A)" i col)))
	else
	do (format t "~%There is no pivot in row ~A" i)
	finally
	(return answer)))

(defun gcd-and-cofs (b a  )
  (let ((bp 0 ) (b 1)(ap 1)( a 0)
		(rp a ) (rpp b)r q)
    (loop
     do
      (setq q (quotient rpp rp))
	   ; (print (list  r q a b))
      (setq r (- rpp (* q rp)))
      (cond ((eql r 0) (return (list   a  b rp))))
      (setq b (prog1 (- bp (* b q))
		     (setq bp b)))
      (setq a (prog1 (- ap (* a q))
		     (setq ap a)))
      (setq rpp rp rp r))))

;(defun test (m n) (setq ans (gcd-and-cofs m n))
; (setq gc (+   (* m (nth 0 ans)) (* n (nth 1 ans))))
; (- gc (gcd m n)))

(defun convert-to-sparse-matrix (row-list-matrix &key re-use-sparse-matrix
				 (old-index-base 1)
				 (rows-to-use nil ruse) (columns-to-use nil cuse)
				 (rows-not-to-use nil rnuse)
				 (renumber-columns 0)
				 (columns-not-to-use nil cnuse)
				 &aux sp  ar ar-rows  jj)
"converts a macsyma matrix or a list of lists to the corresponding sparse matrix
 and sets up the type of entries.  It can be used to pick out a submatrix.  Note
 If you don't renumber the columns  the determinant of submatrices may have wrong
 sign"

  (setq row-list-matrix (if ($matrixp row-list-matrix)  (cdr row-list-matrix)
			    row-list-matrix))
  (loop for v in row-list-matrix
	for i from old-index-base
	do (setq v (if ($listp v) (cdr v) v))
	when (and (or (null ruse) (member i rows-to-use))
		  ( or (null  rnuse) (not (member i rows-not-to-use))))
	collecting
	(setq ar (make-array (* 2 (loop for u in v when (not ($zerop u)) counting u))
			     :fill-pointer 0 :adjustable t))
	into rows
	and
	do
	(setq jj (1- renumber-columns))
	(loop for w
	      in v
	      for j from old-index-base
	      when
	      (and (or (null cuse) (member j columns-to-use))
		   ( or (null  cnuse) (not (member j columns-not-to-use))))
	      do
      	      (cond (renumber-columns (incf jj))
			 (t (setq jj j)))

	      and when
	      (not ($zerop w) )
	      do
	      (vector-push-extend  jj ar)
	      (vector-push-extend  w ar ))
	finally
	(setq ar-rows (make-array (length rows) :fill-pointer (length rows)))
	(fillarray ar-rows rows)

	(cond (re-use-sparse-matrix (setq sp re-use-sparse-matrix))
	      (t (setq sp (make-sparse-matrix))))

	  (setf (sp-constants-column sp) nil)
	(sp-set-rows  sp ar-rows)
	(sp-choose-type-of-entries sp)
	(return sp)))



;;((5 3 1 1 4) (1 1 2 1 5) (4 5 0 1 4) (1 1 1 1 0) (3 1 5 2 4))
;;the sp-determinant function does not work in float mode.
;;I think the problem is that it allows pivots which should be zero entries
;;since the $zerop function only recognize 3.1111e-50 as non zero yet its really
;;zero.

;;the following matrix has determinant -36 according to math:determinant.
;((1 1 3 5 3) (3 3 3 1 3) (2 1 4 3 4) (0 0 0 4 4) (4 3 0 0 4))
;;Macsyma makes it 152 and my program makes it +152.
;;((5 5 ) (3 4)) gave a determinant of 20 with their program but 5 with mine.
;;Note that the :integer type for the determinant program is bad since the
;;addition of new rows makes trouble.

;(defun test-determinant (n &aux mat)
; (setq mat (loop for i below n
;	collecting
;	(loop for j below n collecting (random 6))))
; (dett mat))

;;;I had to wrap a floating point $zerop around it :
;;; (function-let ($zerop float-zerop)
;;;          (sp-determinant sp))
;;;this should be somehow automatic but I hates to change $zerop to check
;;;for floating point since I use it so rarely..
;;;these agreed (since I outlawed the use of sp-determinant coerces away from rational
;;;type
;
;(compare-functions
;(defun dett (mat &aux a1)
;  (show  (setq a1    ($determinant ($coerce_matrix mat))))
;  (iassert (equal (listarray (te1 mat)) (apply 'append mat)))
;  (show mat)
;  (convert-to-sparse-matrix mat :re-use-sparse-matrix *sparse-matrix*)
;  (iassert (equal a1 (sp-determinant *sparse-matrix*))))
;(defun dett (mat &aux )
;  (setq mat (copy-list mat))
;  (setf (car mat) (mapcar 'float (car mat)))
;  (convert-to-sparse-matrix mat :re-use-sparse-matrix *sparse-matrix*)
; (round (sp-determinant *sparse-matrix*)))
;)

(defun make-test (n)
  (cons '($matrix)
	(loop for i from 1 to (expt   n 2)
	      collecting
	      (cons '(mlist)
		    (loop for
			  j from 1 to (expt   n 2)
			  when (eql i j) collecting 4.
			  else
			  when (eql (abs (- i j)) 1) collecting -1
			  else
			  when (eql (abs (- i j)) n) collecting -1
			  else collecting 0)))))


;;for n=10 the 100 by 100 matrix has determinant
;;6265706907310779461620940495418760520632027297067936
;;the only factors I could find were: (613 1 23 1 13 1 2 5)

(defvar *sparse-matrix* (make-sparse-matrix))

(defun $coerce_matrix(mat &aux rows)
  (cond (($matrixp mat) mat)
	((arrayp mat)
	 (let ((dims (array-dimensions mat)))
	   (case (length dims)
	     (1 (setq rows (cons '(mlist) (listarray mat))))
	     (2  (setq rows (loop for i below (car dims)
		      collecting
		      (cons '(mlist) (loop for j below (second dims)
			    collecting (aref mat i j)))))))
	   (cons '($matrix) rows)))
	(t (loop for v in mat
		 collecting (cons '(mlist) v) into tem
		 finally (return (cons '($matrix ) tem))))))


(defun sp-float (sp-mat &aux tem)
  (loop for i below (sp-number-of-rows sp-mat)
	do (let ((row (aref (sp-rows sp-mat) i)))
	     (loop for j from 1 below (fill-pointer row) by 2
		   when (numberp (setq tem (aref row j)))
		   do (setf (aref row j) (float tem))))
	finally  (sp-set-type-of-entries  sp-mat :float)))

(defun float-zerop (n)
  (cond ((numberp n) (or (zerop n)(and (floatp n) (< (abs n) .0001))))
	((atom n) nil)
	((polynomialp n) nil)
	((rational-functionp n)(float-zerop (car n)))
	(t
	 (let (tem type-of-n)
	   (setf type-of-n (caar n))
	   (cond ((member type-of-n '(mrat rat) :test #'eq)
		  (equal (cdr n) (rzero)))
		 (t (and (numberp (setq tem ($ratsimp n)))(zerop tem))))))))

(defun sp-make-sparse-matrix (list-of-rows &key (sp (make-sparse-matrix)) (type-of-entries :any-macsyma) &aux rows)
  "Takes a List-of-rows which is a list or array of arrays with fill-pointer and alternating column number and entry
  and converts to a sparse matrix."
  (cond ((arrayp list-of-rows)(setq rows list-of-rows))
	(t
	 (setq rows (MAKE-ARRAY (length list-of-rows) :fill-pointer (length list-of-rows)))
	(fillarray rows list-of-rows)))
  (sp-set-rows sp rows)
  (sp-set-type-of-entries sp type-of-entries)
  sp)

(defun sp-quotient-space-basis ( list-subspace-rows all-rows  &aux sp-sub sp)
  "takes list-subspace-rows and finds a basis for the space spanned by all-rows
   modulo list-subspace-rows.  Returns two values: the reduce sparse matrix for the
   subspace and then the basis for the quotient space.  Note all-rows need not
   contain list-subspace-rows."
;  (declare (values sp-quotient sp-sub))
  (setq sp-sub (sp-make-sparse-matrix  list-subspace-rows))
  (sp-reduce sp-sub)
  (cond ((arrayp all-rows)(setq all-rows (listarray all-rows))))
  (loop for v in all-rows do (sp-reduce-row-with-respect-to-rows sp-sub v)
	    when (not (zerop (fill-pointer v)))
	      collecting v into real-rows
	finally
		(setq all-rows real-rows))
  (setq sp (sp-make-sparse-matrix all-rows))
  (sp-reduce sp)
 (values sp  sp-sub))

(defun $fast_determinant(matrix &aux sp)
  (cond ((null *sparse-matrix*) (setq *sparse-matrix* (make-sparse-matrix))))
  (setq sp (convert-to-sparse-matrix matrix :re-use-sparse-matrix *sparse-matrix*))
  (new-disrep (sp-determinant sp)))
