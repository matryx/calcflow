;; A Maxima ring stucture
;; Copyright (C) 2005, 2007, Barton Willis

;; Barton Willis
;; Department of Mathematics
;; University of Nebraska at Kearney
;; Kearney NE 68847
;; willisb@unk.edu

;; This source code is licensed under the terms of the Lisp Lesser 
;; GNU Public License (LLGPL). The LLGPL consists of a preamble, published
;; by Franz Inc. (http://opensource.franz.com/preamble.html), and the GNU 
;; Library General Public License (LGPL), version 2, or (at your option)
;; any later version.  When the preamble conflicts with the LGPL, 
;; the preamble takes precedence. 

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Library General Public License for details.

;; You should have received a copy of the GNU Library General Public
;; License along with this library; if not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301, USA.

;; Let's have version numbers 1,2,3,...

(eval-when (:compile-toplevel :load-toplevel :execute)
  ($put '$mring 1 '$version))

;; (1) In maxima-grobner.lisp, there is a structure 'ring.'  

;; (2) Some functions in this structure, for example 'great' might 
;;     not be defined for a ring; when this is the case, a function 
;;     can signal an error.

;; (3) Floating point addition isn't associative; so a mring needn't
;;     be a ring.  But a mring is 'close' to being a ring.

;; Description of the mring fields:

(defstruct mring
  name
  coerce-to-lisp-float
  abs
  great
  add
  div
  rdiv
  reciprocal
  mult
  sub
  negate
  psqrt
  add-id
  mult-id
  fzerop
  adjoint
  maxima-to-mring
  mring-to-maxima)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmvar $%mrings `((mlist) $floatfield $complexfield $rationalfield $crering $generalring $bigfloatfield
		      $runningerror $noncommutingring)))
		
(defun $require_ring (ringname pos fun)
  (if ($member ringname $%mrings) (get ringname 'ring)
    (merror "The ~:M argument of the function '~:M' must be the name of a ring" pos fun)))

(defparameter *floatfield*
  (make-mring
   :name '$floatfield
   :coerce-to-lisp-float #'cl:identity
   :abs #'abs
   :great #'>
   :add #'+
   :div #'/
   :rdiv #'/
   :reciprocal #'/
   :mult #'*
   :sub #'-
   :negate #'-
   :psqrt #'(lambda (s) (if (>= s 0) (cl:sqrt s) nil))
   :add-id #'(lambda () 0.0)
   :mult-id #'(lambda () 1.0)
   :fzerop #'(lambda (s)  (< (abs s) (* 4 flonum-epsilon))) 
   :adjoint #'cl:identity
   :mring-to-maxima #'cl:identity
   :maxima-to-mring #'(lambda (s) 
			(setq s ($float s))
			(if (floatp s) s (merror "Unable to convert ~:M to a long float" s)))))

(setf (get '$floatfield 'ring) *floatfield*)

(defparameter *complexfield*
  (make-mring
   :name '$complexfield
   :coerce-to-lisp-float #'cl:identity
   :abs #'abs
   :great #'>
   :add #'+
   :div #'/
   :rdiv #'/
   :reciprocal #'/
   :mult #'*
   :sub #'-
   :negate #'-
   :psqrt #'(lambda (s) (if (and (= 0 (imagpart s)) (>= (realpart s) 0)) (cl:sqrt s) nil))
   :add-id #'(lambda () 0.0)
   :mult-id #'(lambda () 1.0)
   :fzerop #'(lambda (s) (< (abs s) (* 4 flonum-epsilon)))
   :adjoint #'cl:conjugate
   :mring-to-maxima #'(lambda (s) (add (realpart s) (mult '$%i (imagpart s)))) ;; was complexify
   :maxima-to-mring #'(lambda (s) 
			(progn 
			  (setq s (coerce-expr-to-clcomplex ($rectform (meval s))))
			  (if (complexp s)
			      s
			      (merror "Unable to convert ~:M to a complex long float" s))))))

(defun coerce-expr-to-clcomplex (s)
  (complex (funcall (coerce-float-fun ($realpart s))) (funcall (coerce-float-fun ($imagpart s)))))

(setf (get '$complexfield 'ring) *complexfield*)

(defparameter *rationalfield*
  (make-mring
   :name '$rationalfield 
   :coerce-to-lisp-float #'(lambda (s) ($float s))
   :abs #'abs
   :great #'>
   :add #'+
   :div #'/
   :rdiv #'/
   :reciprocal #'/
   :mult #'*
   :sub #'-
   :negate #'-
   :psqrt #'(lambda (s) (let ((x))
			  (cond ((>= s 0)
				 (setq x (isqrt (numerator s)))
				 (setq x (/ x (isqrt (denominator s))))
				 (if (= s (* x x)) x nil))
				(t nil))))
   :add-id #'(lambda () 0)
   :mult-id #'(lambda () 1)
   :fzerop #'(lambda (s) (= s 0))
   :adjoint #'cl:identity
   :mring-to-maxima #'(lambda (s) (simplify `((rat) ,(numerator s) ,(denominator s))))
   :maxima-to-mring 
   #'(lambda (s) 
       (if (or (floatp s) ($bfloatp s)) (setq s ($rationalize s)))
       (if ($ratnump s) (if (integerp s) s (/ ($num s) ($denom s)))
	 (merror "Unable to convert ~:M to a rational number" s)))))

(setf (get '$rationalfield 'ring) *rationalfield*)

(defparameter *crering*
  (make-mring
   :name '$crering
   :coerce-to-lisp-float nil
   :abs #'(lambda (s) (simplify (mfuncall '$cabs s)))
   :great #'(lambda (a b) (declare (ignore a)) (eq t (meqp b 0)))
   :add #'add
   :div #'div
   :rdiv #'div
   :reciprocal #'(lambda (s) (div 1 s))
   :mult #'mult
   :sub #'sub
   :negate #'(lambda (s) (mult -1 s))
   :psqrt #'(lambda (s) (if (member (csign ($ratdisrep s)) `($pos $pz $zero)) (take '(%sqrt) s) nil))
   :add-id #'(lambda () 0)
   :mult-id #'(lambda () 1)
   :fzerop #'(lambda (s) (eq t (meqp s 0)))
   :adjoint #'(lambda (s) (take '($conjugate) s))
   :mring-to-maxima #'(lambda (s) s)
   :maxima-to-mring #'(lambda (s) ($rat s))))
		
(setf (get '$crering 'ring) *crering*)

(defparameter *generalring*
  (make-mring
   :name '$generalring
   :coerce-to-lisp-float nil
   :abs #'(lambda (s) (simplify (mfuncall '$cabs s)))
   :great #'(lambda (a b) (declare (ignore a)) (eq t (meqp b 0)))
   :add #'(lambda (a b) ($rectform (add a b)))
   :div #'(lambda (a b) ($rectform (div a b)))
   :rdiv #'(lambda (a b) ($rectform (div a b)))
   :reciprocal #'(lambda (s) (div 1 s))
   :mult #'(lambda (a b) ($rectform (mult a b)))
   :sub #'(lambda (a b) ($rectform (sub a b)))
   :negate #'(lambda (a) (mult -1 a))
   :psqrt #'(lambda (s) (if (member (csign s) `($pos $pz $zero)) (take '(%sqrt) s) nil))
   :add-id #'(lambda () 0)
   :mult-id #'(lambda () 1)
   :fzerop #'(lambda (s) (eq t (meqp s 0)))
   :adjoint #'(lambda (s) (take '($conjugate) s))
   :mring-to-maxima #'(lambda (s) s)
   :maxima-to-mring #'(lambda (s) s)))

(setf (get '$generalring 'ring) *generalring*)

(defparameter *bigfloatfield*
  (make-mring
   :name '$bigfloatfield
   :coerce-to-lisp-float #'(lambda (s) 
			     (setq s ($rectform ($float s)))
			     (complex ($realpart s) ($imagpart s)))
   
   :abs #'(lambda (s) (simplify (mfuncall '$cabs s)))
   :great #'mgrp
   :add #'(lambda (a b) ($rectform (add a b)))
   :div #'(lambda (a b) ($rectform (div a b)))
   :rdiv #'(lambda (a b) ($rectform (div a b)))
   :reciprocal #'(lambda (s) (div 1 s))
   :mult #'(lambda (a b) ($rectform (mult a b)))
   :sub #'(lambda (a b) ($rectform (sub a b)))
   :negate #'(lambda (a) (mult -1 a))
   :psqrt #'(lambda (s) (if (mlsp s 0) nil (take '(%sqrt) s)))
   :add-id #'(lambda () bigfloatzero)
   :mult-id #'(lambda () bigfloatone)
   :fzerop #'(lambda (s) (like s bigfloatzero))
   :adjoint #'cl:identity
   :mring-to-maxima #'(lambda (s) s)
   :maxima-to-mring #'(lambda (s) 
			(setq s ($rectform ($bfloat s)))
			(if (or (eq s '$%i) (complex-number-p s 'bigfloat-or-number-p)) s
				(merror "Unable to convert matrix entry to a big float")))))

(setf (get '$bigfloatfield 'ring) *bigfloatfield*)

(defun fp-abs (a)
  (list (abs (first a)) (second a)))

(defun fp+ (a b)
  (cond ((= (first a) 0.0) b)
	((= (first b) 0.0) a)
	(t
	 (let ((s (+ (first a) (first b))))
	   (if (= 0.0 s) (merror "floating point divide by zero"))
	   (list s (ceiling (+ 1
			       (abs (/ (* (first a) (second a)) s))
			       (abs (/ (* (first b) (second b)) s)))))))))
				
(defun fp- (a b)
  (cond ((= (first a) 0.0) (list (- (first b)) (second b)))
	((= (first b) 0.0) a)
	(t
	 (let ((s (- (first a) (first b))))
	   (if (= 0.0 s) (merror "floating point divide by zero"))
	   (list s (ceiling (+ 1
			       (abs (/ (* (first a) (second a)) s))
			       (abs (/ (* (first b) (second b)) s)))))))))

(defun fp* (a b)
  (if (or (= (first a) 0.0) (= (first b) 0.0)) (list 0.0 0)
    (list (* (first a) (first b)) (+ 1 (second a) (second b)))))

(defun fp/ (a b)
  (if (= (first a) 0) (list 0.0 0)
    (list (/ (first a) (first b)) (+ 1 (second a) (second b)))))

(defun $addmatrices(fn &rest m)
  (mfuncall '$apply '$matrixmap `((mlist) ,fn ,@m)))

(defparameter *runningerror*
  (make-mring 
   :name '$runningerror
   :coerce-to-lisp-float #'(lambda (s) (if (consp s) (first s) s))
   :abs #'fp-abs
   :great #'(lambda (a b) (> (first a) (first b)))
   :add #'fp+
   :div #'fp/
   :rdiv #'fp/
   :reciprocal #'(lambda (s) (fp/ (list 1 0) s))
   :mult #'fp*
   :sub #'fp-
   :negate #'(lambda (s) (list (- (first s)) (second s)))
   :psqrt #'(lambda (s) (if (> (first s) 0) (list (cl:sqrt (first s)) (+ 1 (second s))) nil))
   :add-id #'(lambda () (list 0 0))
   :mult-id #'(lambda () (list 1 0))
   :fzerop #'(lambda (s) (like (first s) 0))
   :adjoint #'cl:identity
   :mring-to-maxima #'(lambda (s) `((mlist) ,@s))
   :maxima-to-mring #'(lambda (s) (if ($listp s) (cdr s) (list ($float s) 1)))))

(setf (get '$runningerror 'ring) *runningerror*)

(defparameter *noncommutingring* 
  (make-mring
   :name '$noncommutingring
   :coerce-to-lisp-float nil
   :abs #'(lambda (s) (simplify (mfuncall '$cabs s)))
   :great #'(lambda (a b) (declare (ignore a)) (eq t (meqp b 0)))
   :add #'(lambda (a b) (add a b))
   :div #'(lambda (a b) (progn
			  (let (($matrix_element_mult ".")
				($matrix_element_transpose '$transpose))
			    (setq b (if ($matrixp b) ($invert_by_lu b '$noncommutingring)
				      (take '(mncexpt) b -1)))
			    (take '(mnctimes) a b))))
   
   :rdiv #'(lambda (a b) (progn
			   (let (($matrix_element_mult ".")
				 ($matrix_element_transpose '$transpose))
			     (setq b (if ($matrixp b) ($invert_by_lu b '$noncommutingring)
				       (take '(mncexpt) b -1)))
			     (take  '(mnctimes) b a))))
				

   :reciprocal #'(lambda (s) (progn
			       (let (($matrix_element_mult ".")
				     ($matrix_element_transpose '$transpose))
				 (if ($matrixp s) ($invert_by_lu s '$noncommutingring) 
				   (take '(mncexpt) s -1)))))

   :mult #'(lambda (a b) (progn 
			   (let (($matrix_element_mult ".")
				 ($matrix_element_transpose '$transpose))
			     (take  '(mnctimes) a b))))


   :sub #'(lambda (a b) (sub a b))
   :negate #'(lambda (a) (mult -1 a))
   :add-id #'(lambda () 0)
   :psqrt #'(lambda (s) (take '(%sqrt) s))
   :mult-id #'(lambda () 1)
   :fzerop #'(lambda (s) (eq t (meqp s 0)))
   :adjoint #'(lambda (s) ($transpose (take '($conjugate) s)))
   :mring-to-maxima #'cl:identity
   :maxima-to-mring #'cl:identity))

(setf (get '$noncommutingring 'ring) *noncommutingring*)

(defun ring-eval (e fld)
  (let ((fadd (mring-add fld))
	(fnegate (mring-negate fld))
	(fmult (mring-mult fld))
	(fdiv (mring-div fld))
	(fabs (mring-abs fld))
	(fconvert (mring-maxima-to-mring fld)))
    
    (cond ((or ($numberp e) (symbolp e)) 
	   (funcall fconvert (meval e)))
	  
	  ;; I don't think an empty sum or product is possible here. If it is, append
	  ;; the appropriate initial-value to reduce. Using the :inital-value isn't
	  ;; a problem, but (fp* (a b) (1 0)) --> (a (+ b 1)).  A better value is
	  ;; (fp* (a b) (1 0)) --> (a b).

	  ((op-equalp e 'mplus) 
	   (reduce fadd (mapcar #'(lambda (s) (ring-eval s fld)) (margs e)) :from-end t))
	  
	  ((op-equalp e 'mminus)
	   (funcall fnegate (ring-eval (first (margs e)) fld)))
	  
	  ((op-equalp e 'mtimes) 
	   (reduce fmult (mapcar #'(lambda (s) (ring-eval s fld)) (margs e)) :from-end t))
		 
	  ((op-equalp e 'mquotient)
	   (funcall fdiv (ring-eval (first (margs e)) fld)(ring-eval (second (margs e)) fld)))
	   
	  ((op-equalp e 'mabs) (funcall fabs (ring-eval (first (margs e)) fld)))
	
	  ((and (or (eq (mring-name fld) '$floatfield) (eq (mring-name fld) '$complexfield))
		(consp e) (consp (car e)) (gethash (mop e) *flonum-op*))
	   (apply (gethash (mop e) *flonum-op*) (mapcar #'(lambda (s) (ring-eval s fld)) (margs e))))
	  
	  (t (merror "Unable to evaluate ~:M in the ring '~:M'" e (mring-name fld))))))
  
(defmspec $ringeval (e)
  (let ((fld (get (or (car (member (nth 2 e) $%mrings)) '$generalring) 'ring)))
    (funcall (mring-mring-to-maxima fld) (ring-eval (nth 1 e) fld))))
