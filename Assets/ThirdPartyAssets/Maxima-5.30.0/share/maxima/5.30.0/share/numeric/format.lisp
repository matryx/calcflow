;;; -*- Mode:LISP; Package:MACSYMA -*-

;	** (c) Copyright 1981 Massachusetts Institute of Technology **

(macsyma-module format)

(declare (special $floatformat $floatint $floatfrac $floatprec $floatwidth
		  $floatoptions $aliases $stringdisp $lispdisp))

(defmvar $floatformat nil)
(defmvar $floatint 1)
(defmvar $floatfrac 2)
(defmvar $floatprec 3)
(defmvar $floatwidth 10.)
(defmvar $floatoptions nil)

(defprop print-fixed-field-floating (fformat fasl dsk liblsp) autoload)
(defprop print-fixed-precision-floating (fformat fasl dsk liblsp) autoload)

(defun number-exploden (form)
       (cond ((and $floatformat (floatp form))
	      ((lambda (list)
		       (cond ((null list) (exploden form))
			     ((= $floatwidth 0) (delete 32. list))
			     (t list)))
	       (cond ((eq $floatformat '$f)
		      (print-fixed-field-floating
		       form (cond ((= $floatwidth 0) 15.) (t $floatwidth)) $floatfrac
		       (cons 'exploden (and $floatoptions
					    (mapcar 'stripdollar
						    (cdr $floatoptions))))))
		     (t
		      (print-fixed-precision-floating
		       form (cond ((= $floatwidth 0) 15.) (t $floatwidth)) $floatprec
		       (cons 'exploden (and $floatoptions
					    (mapcar 'stripdollar
						    (cdr $floatoptions))))
			(cond ((numberp $floatint) $floatint)
			      (t (cdr $floatint))))))))
	      (t (exploden form))))

(declare (eval (read)))
(setsyntax '/# 'macro 'tyi)

(defun makestring (form)
  ((lambda (dummy)
    (cond ((numberp form) (number-exploden form))
	  ((and (setq dummy (get form 'reversealias))
		(not (and (member form $aliases :test #'eq) (get form 'noun))))
	   (exploden dummy))
	  (t (setq dummy (exploden form))
	     (cond ((= #$ (car dummy)) (cdr dummy))
		   ((and $stringdisp (stringp form)) (cons #" (nconc dummy (list #"))))
		   ((= #% (car dummy)) (cdr dummy))
		   ($lispdisp (cons #? dummy))
		   (t dummy)))))
   nil))

(defun string* (x)
  (or (and (numberp x) (number-exploden x))
      (string*1 x)))

(declare (eval (read)))

(setsyntax '/# 'macro nil)
