;;; -*- mode: lisp; package: cl-maxima; syntax: common-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defvar *varlist* nil)
(defvar *genvar* nil)

(defun polynomialp (x)
  (cond  ((numberp x))
	 ((atom x) nil)
	 ((and (atom (car x)) (symbolp (car x)) (get (car x) 'disrep)))
	 (t nil)))

(Defun header-poly (expr)
   (cond ((numberp expr) expr)
	 ((and (numberp (car expr))
	       (numberp (cdr expr)))
	  (cond ((zerop (car expr))0)
		(t (list '(rat simp) (car expr) (cdr expr)))))
	 ((polynomialp expr)(cons (list 'mrat 'simp *varlist* *genvar*) (cons expr 1)))
	 ((rational-functionp expr)(cons (list 'mrat 'simp *varlist* *genvar* ) expr ))
	 (t expr)))

(defun $zerop ( n &aux type-of-n tem )
  (cond ((numberp n) (zerop n))
	((atom n) nil)
	((polynomialp n) nil)
	((rational-functionp n)(rzerop   n))
	(($bfloatp n) (eql (car n) 0))
	(t (setf type-of-n (caar n))
	   (cond ((member type-of-n '(mrat rat) :test #'eq)
		  (equal (cdr n) (rzero)))
		 (t (and (numberp (setq tem ($ratsimp n)))(zerop tem)))))))

(defun rational-functionp (x)
  (cond ((numberp x))
	((atom x) nil)
	(t (and (polynomialp (car x))
		(polynomialp (cdr x))))))

(defun psublis (a-list denom poly &key degree vars-to-sub)
  "does a general sublis : a-list of form (list (cons old-var repl-poly)....)
   denom is a poly "
  (let ((tem (cond (vars-to-sub vars-to-sub)
		   (t (loop for v in a-list collecting (car v)))))
	deg)
    (cond ((polynomialp poly)
	   (cond (degree (setq deg degree))
		 (t (setq deg (poly-degree poly tem))))
	   (psublis1 a-list denom poly
		     deg  tem))
	  ((rational-functionp poly) (rsublis a-list denom poly :degree degree :vars-to-sub
					      vars-to-sub :reduce t))
	  ((atom poly) poly)
	  (t (loop for v in poly collecting (psublis a-list denom v :degree degree
						   :vars-to-sub vars-to-sub))))))

;;should take into account when the variables don't need replacing.
(defun psublis1 (a-list denom poly degree varl)
   (cond ((atom poly) (ptimes poly (pexpt denom degree)))
	((member (p-var poly) varl :test #'eq)
	 (loop for (deg cof) on (cdr poly) by #'cddr
	       with answer = 0
	       do (setq answer
			(pplus answer
			       (ptimes
					(psublis1 a-list denom cof
						      (- degree deg) varl)
				       (pexpt (cdr (assoc (p-var poly) a-list :test #'equal))
					      deg))))

	       finally (return answer)))
	((> (valget (p-var poly))
	     (loop for v in varl minimize (valget v)))
	 (loop for (deg cof) on  (cdr poly ) by #'cddr
	       with answer = 0
	       with mon = (list (p-var poly ) 1 1)
	       do (setq answer (pplus answer
				      (ptimes
				       (pexpt mon deg)
				       (psublis1 a-list denom cof degree varl))))

	       finally (return answer)))
	(t (ptimes (pexpt denom degree) poly))))

(defun zero-sublis (poly &rest list-vars)
  (pcoeff poly 1 list-vars))

(defun pcomplexity  (poly)
  (cond ((atom poly) 0)
	(t (loop for (deg cof) on (cdr poly) by #'cddr
		 summing (+ deg (pcomplexity cof))))))

(defun $numerator (expr)
  (cond ((atom expr) expr)
	((mbagp expr) (cons (car expr) (mapcar '$numerator (cdr expr))))
	(($ratp expr) (cons (car expr) (cons (car (cdr expr)) 1)))
	(($totaldisrep (header-poly (cons (num (new-rat expr)) 1))))))

(defmacro allow-rest-argument (new-funct binary-op  answer-for-null-argument rest-arg)
  `(cond  ((null ,rest-arg) ,answer-for-null-argument)
	  (t (case (length ,rest-arg)
	       (,2 (,binary-op (first ,rest-arg)(second ,rest-arg)))
	       (,1 (car ,rest-arg))
	       (otherwise (apply ',new-funct (,binary-op (first ,rest-arg)
							 (second ,rest-arg)) (cddr ,rest-arg)))))))

(defmacro polyop (x y identity-x identity-y number-op poly-op rat-op &optional rat-switch )
  (cond (rat-switch (setq rat-switch '(t))))
  `(cond
     ((and (numberp ,x)(numberp ,y))(,number-op ,x ,y))
     ((eq ,x ,identity-x) ,y)
     ((eq ,y ,identity-y) ,x)
     (t
      (let ((xx (poly-type ,x)) answer
	    (yy (poly-type ,y)))

;	(cond ((or (eq xx '$rat)(eq yy '$rat))(with-vgp(check-vgp-correspond))));;remove later
	(cond ((null xx)(setq ,x (cdr ($new_rat ,x)) xx ':rational-function))
	      ((eq xx ':$rat)
	       (setq ,x (cdr ,x) xx ':rational-function))
	      ((eq xx ':rat ) (setq ,x (cons (second ,x) (third ,x))
				   xx ':rational-function)))

	(cond
	  ((null yy)(setq ,y (cdr ($new_rat ,y)) yy ':rational-function))
	  ((eq yy ':$rat)(setq ,y (cdr ,y) yy ':rational-function))
	  ((eq yy ':rat ) (setq ,y (cons (second ,y) (third ,y))
			       yy ':rational-function)))
	(cond ((and (eq xx ':rational-function) (eq (denom ,x) 1))
	       (setq ,x (car ,x) xx :polynomial)))
	(cond ((and (eq yy ':rational-function) (eq (denom ,y) 1))
	       (setq ,y (car ,y) yy :polynomial)))
	(setq answer
	      (case xx
		(:number   (case yy
			     (:number (,number-op ,x ,y))
			     (:polynomial (,poly-op ,x ,y))
			     (:rational-function
			      (,rat-op (cons ,x 1) ,y ,@ rat-switch))))
		(:polynomial
		 (case yy
		   (:number (,poly-op ,x ,y))
		   (:polynomial (,poly-op ,x ,y))
		   (:rational-function (,rat-op (cons ,x 1) ,y,@ rat-switch))
		   (otherwise (merror "unknown type for polyop "))))
		(:rational-function
		 (case yy
		   (:number (,rat-op ,x  (cons ,y 1) ,@ rat-switch))
		   (:polynomial (,rat-op ,x (cons ,y 1)  ,@ rat-switch))
		   (:rational-function (,rat-op ,x ,y ,@ rat-switch))))
		(otherwise (merror "unknown arg"))))
	(cond ((polynomialp answer) answer)
	      ((rational-functionp answer)
	       (cond ((eq 1 (cdr answer))(car answer))
		     (t answer)))
	      (t answer))))))

(defun n* (&rest l)
  (case (length l)
    (0 1)
    (1 (car l))
    (2 (n1* (car l) (second l)))
    (t (n1* (car l) (apply 'n* (cdr l))))))

(defun n+ (x y)
  (polyop x y  0 0 + pplus ratplus ))

(defun n1* (x y)
  (polyop x y 1 1 * ptimes rattimes t))

(defun n- (x y)
  (polyop x y nil 0 - pdifference ratdifference))

(defun nred (x y &aux answer)
  (setq answer (polyop x y nil 1  ratreduce ratreduce ratquotient))
  (setq answer (rationalize-denom-zeta3 answer))
  (cond ((numberp answer) answer)
	((eq (denom answer) 1) (car answer))
	(t answer)))

(defun new-disrep (expr)
  (cond ((atom expr) expr)
	(t
	 (let ((type (poly-type expr)))
	   (case type
	     (:number expr)
	     (:polynomial ($ratdisrep (header-poly expr)))
	     (:rational-function
	      (cond (($numberp expr)
		     (cond ((zerop (num expr)) 0)
			   (t
		     (list '(rat simp) (num expr) (denom expr)))))
		    (t
		     ($ratdisrep (header-poly expr)))))
	     (otherwise  (cond ((mbagp expr)(cons (car expr) (mapcar 'new-disrep expr)))
			       (($ratp expr)($ratdisrep expr))
			       (t expr))))))))
 (defun poly-degree (poly varl)
  (cond ((atom poly) 0)
	((member (p-var poly) varl :test #'eq)
	 (loop for (deg cof) on (cdr poly) by #'cddr
	       maximize (+ deg (poly-degree cof varl))))
	(t
	 (loop for (deg cof) on (cdr poly) by #'cddr
	       maximize (poly-degree cof varl)))))

(defun shl (l)
  (cond ($display2d (mapcar 'sh l))
       (t (loop for v in l
		for i from 0
		initially (format t "~%[")
		when (numberp v) do (princ v)
		else
		do  (sh (header-poly v))
		when (< i (1- (length l))) do(format t ",~%")
		finally (format t "]")))))

(defun sh (expr &optional (stream *standard-output*) &aux (disp 'string-grind) answ)
  (declare (special *fake-rat*))
  (cond ( $display2d (setq disp 'displa)))
  (let ((*standard-output* stream))
    (setq answ(cond ((numberp expr) (funcall disp expr))
		    ((polynomialp expr)(funcall disp (cons  *fake-rat* (cons expr 1))))
		    ((rational-functionp expr)(funcall disp (cons *fake-rat* expr)))
		    (t (funcall disp expr))))
    (cond (answ (format t "~A" answ)))))

(defmacro setq-num-den (num den expr &aux form .expr.)
  (cond ((symbolp expr) (setq .expr. expr))
	(t (setq .expr. '.expr.)))
  (setq form
	`(cond ((polynomialp  , .expr.) (setq ,num , .expr. , den  1))
	       ((rational-functionp , .expr.) (setq ,num (num , .expr.) ,den (denom , .expr.)))
	       (t (fsignal "expr is supposed to be poly or rational-functionp"))))
  (cond ((symbolp expr)
	 form)
	(t `(let ((.expr. ,expr))
	      ,form))))

(defun splice-in (after-nth item a-list )
  (cond ((eq after-nth -1)(cons item a-list))
	(t (nconc (subseq a-list 0 (1+ after-nth))
		  (cons item (cdr (nthcdr after-nth a-list)))))))


(defun nsplice-in (after-nth item a-list &aux tem)
  (cond ((eq after-nth -1)(cons item a-list))
	(t
;	 (rplacd (setq tem (nthcdr after-nth a-list))
;		 (cons item (cdr tem)))
	 (setf (cdr (setq tem (nthcdr after-nth a-list)))
		 (cons item (cdr tem)))

	 a-list)))

(defun nc-equal (p1 p2)
  (or (and  (atom p1) (eql p1 p2))
      (and (listp p1) (listp p2) (equal (cdr p1 ) (cdr p2)))))

(defvar *check-order* nil)

(defun find-in-ordered-list (x a-list &optional  (order-function (function alphalessp))
			     (order-equal #'nc-equal)
			     &aux (offset 0)
			     (leng 0) tem (after 0)
			     (mid 0) (ub 0) (lbound 0) already-there)
  (declare (fixnum leng mid ub lbound after))
  (setq leng (length a-list))

  (cond ((funcall order-function x (car a-list))(setq after -1))
	((funcall order-equal  x (car a-list))(setq after 0)(setq already-there a-list))
	((funcall order-equal  x (car(setq tem  (last a-list))))(setq after (1- leng))
	 (setq already-there tem))
	((not (funcall order-function x (car tem)))(setq after (1- leng)))

	(t (setq offset 0 lbound 0 ub (1- leng))
	   (loop while (> (- ub lbound) 1)
		 do
		 (setq mid (truncate (+ ub lbound) 2))
		 (cond ((funcall order-function x (car(setq tem
							(nthcdr mid a-list))))(setq ub mid))
		       ((funcall order-equal  (car tem) x)(setq after (+ offset mid)
					   already-there tem)(return 'done))
		       (t (setq offset (+ mid offset)
				a-list tem
				ub (- ub mid))))
		 finally(setq after offset))))
  (and *check-order*
       (cond ((setq tem (member x a-list :test order-equal))
	      (iassert (eql (not already-there) (not t)))
	      (iassert (eql after (- leng (length tem )))))))
  (values after already-there a-list))

(defun disrep-list (expr)
  "Just substitutes the any disrep property for a symbol for the symbol"
  (cond ((atom expr)(cond ((symbolp expr)(get expr 'disrep))
			  (t expr)))
		(t (cons (disrep-list (car expr)) (disrep-list (cdr expr))))))

;;Perhaps we should have our own prep1 function
;;It just goes through a general form converting it to cre, and
;;when it comes to a symbol it can't find in genpairs it
;;calls newsym which is much like our add-newvar . We could
;;have it look things up in a hash table rather than genpairs
;;The checking for being on tellratlist is done by the ratsetup2
;;One has to be careful with the with-vgp form since nested ones
;;will cause havoc.  Also if inside one you must refer to varlist
;;not *varlist* yet outside you may want *varlist*.  So for example
;;in


(defun check-repeats (varl)
  (loop for v on varl
	when (member (car v) (cdr v) :test 'nc-equal)
	  do (show (car v) ) (fsignal "repeat in varlist")))

(defun check-order (varl)
  (declare (special $order_function))
  (loop for (v w) on varl
	while w
	  when (not (funcall $order_function  v w))
	    do (fsignal "bad order ~A and ~A "  v w )))


(defun generate-new-variable-gensym (variable  &aux tem)
       (setq tem (gensym))
		       (putprop tem variable 'disrep)
		       (push (cons variable (rget tem)) genpairs )
		    tem   )

(defmacro with-vg (&rest body)
  `(let ((varlist *varlist*)
	 (genvar *genvar*))
     ,@body
     (setq *varlist* varlist)
     (setq *genvar* genvar)))

(defmacro with-vgp (&rest body)
  `(let ((varlist *varlist*)
	(genvar *genvar*)
	(genpairs *genpairs*))
	(prog1 (progn ,@ body)
	(setq *varlist* varlist *genpairs* genpairs)
	(setq *genvar* genvar))))

(defun zl-union (&rest l)
  (case (length l)
	(1 (loop for v on (car l) unless (member (car v) (cdr v))
		   collect (car v)))
	(2 (loop for v in (car l) with sec = (second l)
		  unless (member v sec)
		  collect v into result
		  finally (return (nconc result (zl-union sec)))))
	(t (zl-union (car l) (apply #'zl-union (cdr l))))))
