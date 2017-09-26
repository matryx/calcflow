;; Author Barton Willis
;; University of Nebraska at Kearney
;; Copyright (C) 2004, Barton Willis

;; Brief Description: Maxima code for linear homogeneous second order
;; differential equations.

;; Maxima odelin is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License,
;; http://www.gnu.org/copyleft/gpl.html.

;; Maxima odelin has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

($put '$sqfr 1 '$version)

(eval-when
    #+gcl (load compile eval)
    #-gcl (:load-toplevel :compile-toplevel :execute)
    ($load "odeutils"))

;; If x is a symbol for a subvarp, return its general representation.
;; Otherwise signal an error---the argument f is the string name of
;; a function that requires the symbol.

(defun require-symbol (x f &optional a)
  (declare (ignore a))
  (setq x ($ratdisrep x))
  (if (or (symbolp x) ($subvarp x)) x 
    (merror "Function ~:M requires a symbol; instead found ~:M" f x))
  x)

(defun $strictmysqfr (p x)
  (setq p ($expand ($ratdisrep p)))
  (setq x (require-symbol x "$mysqfr"))
  (let ((i 1) (lc 1) (r) (ai) (w) (bad) (acc nil) (q) (s) 
	($gcd '$spmod) ($algebraic t) ($resultant '$red) ($ratfac nil) 
	($ratprint nil))
    
    (setq lc ($coeff p x ($hipow p x)))
    (setq p ($expand (div p lc)))
    (while (not ($freeof x p))
      (setq r ($gcd p ($diff p x) x))
      (setq q ($first ($divide p r)))
      (setq ai ($first ($divide q ($gcd q r x) x)))
      (setq ai (div ai ($coeff ai x ($hipow ai x))))
      (push ai acc)
      (setq p r)
      (incf i))
  
    (setq acc (reverse acc))
    (setq r lc)
  
    (setq i 0)
    (while acc
      (setq ai (pop acc))
      (setq r (mul r (power ai (incf i))))
      (setq s ($resultant ai ($diff ai x) x))
      (if (not ($constantp s)) (push ($factor s) bad))
      (dolist (wi w)
	(setq s ($resultant ai wi x))
	(if (not ($constantp s)) (push ($factor s) bad)))
      (push ai w))
    
    (setq bad `(($set) ,@bad))
    (setq bad (mbag-map #'(lambda (w) `((mnotequal) ,w 0)) bad))
    (if (not ($emptyp bad)) (mtell "Proviso: assuming ~:M~%" bad))
    (values r bad)))

(defun $mysqfr (p x)
  (setq p ($expand ($ratdisrep p)))
  (setq x (require-symbol x "$mysqfr"))
  (let ((i 0) (r) (ai) (acc) (q) ($gcd '$spmod) ($algebraic t))
    
    (cond ((like 0 p) 0)
	  (t
	   (setq p ($expand p))
	   (setq acc ($coeff p x ($hipow p x)))
	   (setq p (div p acc))
	   (while (not ($freeof x p))
	     (setq r ($gcd p ($diff p x) x))
	     (setq q ($first ($divide p r x)))
	     (setq ai ($first ($divide q ($gcd q r x) x)))
	     (setq ai (div ai ($coeff ai x ($hipow ai x))))
	     (setq acc (mul acc (power ai (incf i))))
	     (setq p r))
	   acc))))

