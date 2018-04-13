;;; -*- Mode:Lisp; Package:CL-MAXIMA; Syntax:COMMON-LISP; Base:10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defmacro iassert (expr)
  `(cond ((null ,expr) (fsignal "The expression ~A was not true" ',expr))))

(defun alphagreatp (x y)
  (and (not (alphalessp x y)) (not (equal x y))))

(defun $monomial_alphalessp (x y)
  (cond ((atom x)
	 (cond ((atom y) (alphalessp x y))
	       ((null (cdr y)) (alphalessp x (car y) ))
					; x<x.z , x< x.*
	       ((eql x (cadr y)))
	       (t (alphalessp x (cadr y)))))
	((atom y)
	 (cond ((null (cdr x)) (alphalessp (car x) y)) ;x cannot be an atom here
	       (t (alphalessp (cadr x) y))))
	(t
	 (alphalessp (cdr x) (cdr y)))))

(defun $monomial_alphagreatp (x y)
   (not (or (equal x y) ($monomial_alphalessp x y))))

(defun $power_series_monomial_alphalessp (x y)
  (cond ((equal x y) nil)
	(t
  (let ((x-deg ($nc_degree x :order-weight))(y-deg ($nc_degree y :order-weight)))
    (cond ((eql x-deg y-deg)($monomial_alphalessp x y))
	  ((zerop y-deg) (not (zerop x-deg)))
	  ((zerop x-deg) nil)
	  ((> x-deg y-deg) t)
	  (t nil))))))

(defun $polynomial_monomial_alphalessp (x y)
  (cond ((equal x y) nil)
	(t
  (let ((x-deg ($nc_degree x :order-weight))(y-deg ($nc_degree y :order-weight)))
    (cond ((eql x-deg y-deg)($monomial_alphalessp x y))
	  ((zerop x-deg) (not (zerop y-deg)))
	  ((zerop y-deg) nil)
	  ((< x-deg y-deg) t)
	  (t nil))))))

(defun fix-optional (args  &aux (tem (copy-list args)))
  (loop for v on tem
	for i from 0
	when (member (car v) '(&optional  &key))
	do (loop for w on (cdr v)
		 until (and (atom (car w)) (search "&" (string (car w)) :test #'char-equal))
		 when (atom (car w))
		 do (setq tem  (car w))
		 else do (setq tem  (caar w))
		 when (eq (car v) '&key)
		 collecting (intern (string-upcase (string tem)) 'keyword) into opts
		 collecting tem into opts
		 finally
		 (setq args (append (subseq args 0 i)  opts (cdr w)))(return 'done))
		 (return 'done))
  args)

(defun delete-from-&aux (list)
  (loop for u in list
	for i from 1
	when (eq u '&aux) do (return (subseq list 0 (1- i)))
	finally (return list)))

(defun clear-memory-function (f &aux tem)
  (cond ((setq tem  (get f ':memory-table))
	 (clrhash tem))))

(defmacro defremember (func-spec arglist &rest body
		       &aux hash-args hash-put-args (call-arglist (copy-list arglist)))
  "Like defun but defines function foo that remembers previous calls to
  it  it unless the calls were made while (disable-remember foo) until
  (enable-remember foo) is done.  To clear memory do (clear-hash (get 'foo
  :memory-table)) .   Redefining clears memory.  The equal-hash table is
  stored as the :memory-table property of foo.     also it is useful to
  use as optional arguments of  global variables or other data not
  included in the arguments upon which  the function call depends.  These
  will then become part of the  argument."

  (cond ((get func-spec	:memory-table)
	 (clrhash (get func-spec :memory-table))))
  (remprop func-spec :dont-remember)
;; (show call-arglist)

  (setq call-arglist  (fix-optional(delete-from-&aux call-arglist)))
  (cond ((member '&aux call-arglist  :test #'eq)
	 (format t "~%It is inadvisable to use &aux with defremember")))
;;  (show call-arglist)
  (cond ((equal (length arglist) 1)(setq hash-put-args (setq hash-args (car arglist))))
	((equal (car arglist) '&rest) (setq hash-args (second arglist))
	 (setq hash-put-args `(copy-list ,(second arglist)))
	 (setq call-arglist (second arglist)))
	((member '&rest arglist :test #'eq) (setq hash-args `(list ,@ (DELETE '&rest call-arglist)))
	 (setq hash-put-args (copy-list hash-args))
	 (setf (nth (1- (length hash-put-args)) hash-put-args)
	       `(copy-list ,(car (last hash-put-args)))))
	(t (setq hash-put-args (setq hash-args (cons 'list call-arglist)))))
  `(defun ,func-spec ,arglist
     (cond ((null (get ',func-spec :dont-remember))
	    (let (hash-table ans)
	      (cond ((setq hash-table (get ',func-spec :memory-table))
		     (cond ((setq ans (gethash ,hash-args hash-table)) ans)
			   (t (setq ans (progn ,@ body))
			      (setf (gethash ,hash-put-args hash-table) ans)
			      ans)))
		    (t (setf (get ',func-spec :memory-table)
			      (make-hash-table :test 'equal)
			      )
		       ,(cond ((member '&rest arglist  :test #'eq)
			       `(apply ',func-spec ,@
				       ` ,(DELETE '&rest
						     (copy-list call-arglist))))
			      (t `(,func-spec ,@ call-arglist)))))))
	   (t (progn ,@ body)))))

(defmacro disable-remember (f)
  `(putprop ',f t :dont-remember))

(defmacro enable-remember (f)
  `(remprop ',f :dont-remember))

(defmacro function-let (alist &body body &aux sym)
  (loop for v in alist
	for i from 1
	do (setq sym (gensym))
	collecting sym into locals
	collecting `(setq ,sym (symbol-function ',(first v))) into initial
	collecting `(setf (symbol-function ',(first v))
			  (symbol-function ',(second v))) into initial
	collecting `(cond (,sym (setf (symbol-function ',(first v)) ,sym))) into protects
	finally (return `(let ,locals
			   (unwind-protect
			     (progn   ,@ initial
			     ,@ body)
			   ,@ protects)))))

(defmacro with-no-query (&rest body)
  `(function-let ((fquery no-query-aux)) ,@body))

(defmacro with-no-query-answer-no (&rest body)
  `(function-let ((fquery (lambda (ignore ctrl &rest args) (apply 'format  t ctrl args ) nil)))	,@body))

(defmacro find-minimal (in-list ordering &optional such-that ind)
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

(defmacro user-supply (var)
  `(setq ,var (user-supply1 ',var ,var)))

(defun user-supply1 (var val)
  (let ((*print-level* 3)
	.new.
	.ch.)
    (loop do
	  (format t "~%The value of ~A is ~A ." var val)
	  (format t "~%Supply a form to evaluate to use for ~A or hit return to keep same :" var)
	  (setq .ch. (read-char *standard-input*))
	  (cond ((eql .ch. #\newline)
		 (return val))
		(t (unread-char .ch. *standard-input*)))
	  (setq .new. (eval (read)))
	  (cond ((eq .new. 'keep)
		 (return var))
		(t
		 (format t "~%Use ~A?" .new.)
		 (cond ((y-or-n-p)
			(return (setq var .new.)))))))))

(defvar  *timed-process-priority* 1)

(defmacro tim (&rest body)
  `(time (progn ,@body)))

;;Idea is that you have two function definitions possibly with the
;;same name, and lots of places where the first one is called.
;;you may want to replace the first function with the second one
;;and think you have the definition correct.  You want to compare
;;the speed and whether they work the same.  So wrap
;;compare-functions around the two definitions, then it will compare
;;them as to speed (and value) in the applications.  It might be reasonable to implement
;;this as an emacs macro wrapping it around a region.

(defmacro compare-functions (defa defb &rest assertions)
  (let (.fa. .fb.
	.boda. .bodb.
	(.assert. (loop for v in assertions
			collecting `(iassert ,v) into tem
			finally
			(return (cons `(iassert (equal .ansa. .ansb.)) tem)))))
    (check-arg defa (eq (car defa) 'defun) "Should be a defun")
    (cond ((atom defb)
	   (setq defb `(defun ,(string-append defb (symbol-name '#:-compare)) (&rest .l.)
			 (apply ',defb .l.)))))
    (setq .fa. (intern (string-append (second defa) (symbol-name '#:-a))))
    (setq .fb. (intern (string-append (second defb) (symbol-name '#:-b))))
    (setq .boda. (subst .fa. (second defa) (cdddr defa)))
    (setq .bodb. (subst .fb. (second defb) (cdddr defb)))
    `(progn 'compile
	    nil
	    (defun , .fa. ,(third defa)
		   (tim ,@ .boda. ))
	    (defun , .fb. ,(third defb)
		   (tim ,@ .bodb.))
	    (defun ,(second defa)
		   (&rest l)
	      (declare (arglist ,. (loop for v in (third defa)
					 until (eq v '&aux) collecting v)))
	      (let (.ansa. .ansb.)
		(format t "~%Comparing the functions ~A and ~A " ', .fa. ', .fb.)
		 (setq .ansa.
		       (multiple-value-list (apply   ', .fa.  (copy-list l))))
		 (setq .ansb. (multiple-value-list (apply   ', .fb.  (copy-list l))))
		,@ .assert.
		(apply 'values .ansa.))))))

(defmacro compare-recursive-functions (fn-a fn-b &rest assertions)
  `(progn 'compile
	 ,fn-a
	 ,fn-b
  , `(compare-functions
	(defun ,(intern (string-append (second fn-a) (symbol-name '#:-compare))) ,@ (cddr fn-a))
	(defun ,(intern (string-append (second fn-b) (symbol-name '#:-compare))) ,@ (cddr fn-b))
	,@ assertions)))

;;sample usage of compare-recursive-functions
;(compare-recursive-functions
;  (defun my-pgcd (f g) ...
;	 ...
;	 (my-pgcd (cof f) ..))
;  (defun pgcd (f g)
;    ...)
;)

;;then to invoke the comparison call
;;(my-pgcd-compare u v) and it will calculate the gcd using the two methods check they
;;are equal (or check other assertions you specify) and print the time spent
;;in my-pgcd-compare-a and in my-pgcd-compare-b

;(compare-functions
;(defun ff (u &optional v)
;  (values  (+ u v) ))
;+
;)


;(compare-functions
;(defun ff (u &optional v)
;  (values  (+ u v) u))
;(defun ff (u &optional v)
;  (values (+ v u) v))
;)
;;then if you run some functions that call ff it will give the comparison
;;note that ff and gg may have the same name
;(defun test (c d)
;  (ff c d))

(defun write-object (filename obj)
    (with-open-file  (st filename :direction :output)
  (format st "~A" obj)))

(defstruct (s-var (:type list) :named (:conc-name sv-))
  zopens)  ;; list of open sets

(defmacro set-slots (struct conc-prefix &rest alt-list)
  (loop for v on alt-list by #'cddr
	collecting `(setf ( ,(intern (format nil "~a~a" conc-prefix (car v))) ,struct) ,(second v))
	into tem
	finally (return (cons 'progn tem))))

;; Note that ZL-COPY-STRUCTURE works only for structures which are
;; represented by lists.  The `ZL' is just for analogy with other private
;; versions of list related functions in Maxima and no reference to any
;; historical version of Lisp is intended.
(defmacro zl-copy-structure (struct conc-prefix &rest alt-list)
 ` (let (( .ans. (copy-list ,struct)))
	 (set-slots .ans. ,conc-prefix ,@ alt-list)
	 .ans.))

;(defstruct (s-var (:type  list ) :named (:conc-name sv-))
;  zopens  ;; list of open sets
;  ;; a list of opens each having its intersection slot specified
;  intersections
;  ;;glueing:  for each element of the intersections will have a
;  ;;a list of maps between elements of intersections
;  ;;so that the map from that element to the reverse intersection
;  glueing
;  )

(defstruct (pre-ldata-sheaves (:type list ) :named (:conc-name pls-))
  s-var
  ;;for each open a list of ldata
  data)

(defstruct (ldata (:type  list ) :named (:conc-name ldata-))
  eqns
  (inequality 1)
  (usedup 0)
  (open-inequality 1)
  variables)

(defstruct (rmap (:type  list ) :named (:conc-name rmap-))
  fns
  denom)

(defstruct (zopen (:type  list) :named (:conc-name zopen-))
  ;;coord is an r-map
  coord
  ;;inv is an r-map
  inv
  inequality
  history)

(defstruct (variable-correspondence :named (:conc-name vc-))
  genvar
  varlist
  add-method)

(defstruct (ideal :named (:conc-name ideal-))
  char-set
  generators
  localization
  variable-correspondence)

(defstruct ( polynomial-vectors :named (:conc-name pv-))
  length-of-array-of-tables
  rows
  (constants-column-number nil)
  (relations nil)
  (type-of-entries :integer)
  (variables nil)
  (verify-conversion nil)
  array-of-polynomials
  array-of-tables
  last-column-number
  number-of-independent-terms   ;;never occurs
  solution-in-macsyma-format
  table
  the-sparse-matrix
  type-of-polynomials
  solution-plist
  )

(defstruct (poly-data :named   (:conc-name pd-))
  rows
  sparse-matrix
  segments ;;beginning of each new group of monoms in big-monom-list
  big-monom-list)

(defmacro gshow (form)
  `(progn (format t "~%The value of ~A is" ',form)
	  (grind-top-level ,form)))

(defmacro mshow (&rest l)
  (loop for v in l
	collecting `(format t "~%The value of ~A is.. " ',v) into tem
	collecting `(des ,v) into tem
	finally (return (cons 'progn tem))))

(defmacro ncmul* (&rest factors)
	  (cond ((= (length factors) 2) `(ncmul2* . ,factors))
		(t `(ncmuln (list . ,factors) nil))))

(defstruct (matrix (:type list) :named (:conc-name matrix-))
  rows)  ;; list of open sets

;(defmacro matrix-p (mat)
;  `(and (listp ,mat) (eq (car ,mat) 'matrix)))

(defvar *verbose-check-overlaps* nil)

(defmacro if-verbose (&rest l)
  `(when *verbose-check-overlaps* ,@l))
