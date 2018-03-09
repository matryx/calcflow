;;; Airy functions Ai(z) and Bi(z) - A&S 10.4
;;;
;;; airy_ai(z)   - Airy function Ai(z)
;;; airy_dai(z)  - Derivative of Airy function Ai(z)
;;; airy_bi(z)   - Airy function Bi(z)
;;; airy_dbi(z)  - Derivative of Airy function Bi(z)

;;;; Copyright (C) 2005 David Billinghurst

;;;; airy.lisp is free software; you can redistribute it
;;;; and/or modify it under the terms of the GNU General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2, or (at your option) any later version.

;;;; airy.lisp is distributed in the hope that it will be
;;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;; See the GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with command-line.lisp; see the file COPYING.  If not,
;;;; write to the Free Software Foundation, Inc., 59 Temple Place -
;;;; Suite 330, Boston, MA 02111-1307, USA.

(in-package :maxima)

(declaim (special *flonum-op*))

;; Airy Ai function 
(defmfun $airy_ai (z)
  "Airy function Ai(z)"
  (simplify (list '(%airy_ai) (resimplify z))))

(defprop $airy_ai %airy_ai alias)
(defprop $airy_ai %airy_ai verb)
(defprop %airy_ai $airy_ai reversealias)
(defprop %airy_ai $airy_ai noun)
(defprop %airy_ai simp-%airy_ai operators)
(defprop %airy_ai ((z) ((%airy_dai) z)) grad)

;; airy_ai distributes over lists, matrices, and equations
(defprop %airy_ai (mlist $matrix mequal) distribute_over)

;; airy_ai has mirror symmetry
(defprop %airy_ai t commutes-with-conjugate)

;; Integral of Ai(z)
;; http://functions.wolfram.com/03.05.21.0002.01
;; (z/(3^(2/3)*gamma(2/3)))*hypergeometric([1/3],[2/3,4/3],z^3/9)
;; - (3^(1/6)/(4*%pi))*z^2*gamma(2/3)*hypergeometric([2/3],[4/3,5/3],z^3/9);
(defprop %airy_ai
  ((z)
   ((mplus)
    ((mtimes) 
     ((mexpt) 3 ((rat) -2 3))
     ((mexpt) ((%gamma) ((rat) 2 3)) -1)
     (($hypergeometric) 
      ((mlist) ((rat) 1 3))
      ((mlist) ((rat) 2 3) ((rat) 4 3)) 
      ((mtimes) ((rat) 1 9) ((mexpt) z 3)))
     z)
   ((mtimes) 
    ((rat) -1 4) ((mexpt) 3 ((rat) 1 6)) ((mexpt) $%pi -1) ((%gamma) ((rat) 2 3))
    (($hypergeometric) 
     ((mlist) ((rat) 2 3)) 
     ((mlist) ((rat)  4 3) ((rat) 5 3))
     ((mtimes) ((rat) 1 9) ((mexpt) z 3)))
    ((mexpt) z 2))))
  integral)

(defun airy-ai (z)
  (cond ((floatp z) (airy-ai-real z))
	((complexp z) (airy-ai-complex z))))

(setf (gethash '%airy_ai *flonum-op*) #'airy-ai)

(defmfun simp-%airy_ai (form unused x)
  (declare (ignore unused))
  (oneargcheck form)
  (let ((z (simpcheck (cadr form) x)))
    (cond ((equal z 0) ; A&S 10.4.4: Ai(0) = 3^(-2/3)/gamma(2/3)
	    '((mtimes simp)
	      ((mexpt simp) 3 ((rat simp) -2 3))
	      ((mexpt simp) ((%gamma simp) ((rat simp) 2 3)) -1)))
	  ((flonum-eval (mop form) z))
	  (t (eqtest (list '(%airy_ai) z) form)))))


;; Derivative dAi/dz of Airy function Ai(z)
(defmfun $airy_dai (z)
  "Derivative dAi/dz of Airy function Ai(z)"
  (simplify (list '(%airy_dai) (resimplify z))))

(defprop $airy_dai %airy_dai alias)
(defprop $airy_dai %airy_dai verb)
(defprop %airy_dai $airy_dai reversealias)
(defprop %airy_dai $airy_dai noun)
(defprop %airy_dai simp-%airy_dai operators)
(defprop %airy_dai ((z) ((mtimes) z ((%airy_ai) z))) grad)
(defprop %airy_dai ((z) ((%airy_ai) z)) integral)

;; airy_dai distributes over lists, matrices, and equations
(defprop %airy_dai (mlist $matrix mequal) distribute_over)

;; airy_dai has mirror symmetry
(defprop %airy_dai t commutes-with-conjugate)

(defun airy-dai (z)
  (cond ((floatp z) (airy-dai-real z))
	((complexp z) (airy-dai-complex z))))

(setf (gethash '%airy_dai *flonum-op*) #'airy-dai)

(defmfun simp-%airy_dai (form unused x)
  (declare (ignore unused))
  (oneargcheck form)
  (let ((z (simpcheck (cadr form) x)))
    (cond ((equal z 0) ; A&S 10.4.5: Ai'(0) = -3^(-1/3)/gamma(1/3)
          '((mtimes simp) -1
	      ((mexpt simp) 3 ((rat simp) -1 3))
	      ((mexpt simp) ((%gamma simp) ((rat simp) 1 3)) -1)))
	  ((flonum-eval (mop form) z))
	  (t (eqtest (list '(%airy_dai) z) form)))))

;; Airy Bi function 
(defmfun $airy_bi (z)
  "Airy function Bi(z)"
  (simplify (list '(%airy_bi) (resimplify z))))

(defprop $airy_bi %airy_bi alias)
(defprop $airy_bi %airy_bi verb)
(defprop %airy_bi $airy_bi reversealias)
(defprop %airy_bi $airy_bi noun)
(defprop %airy_bi simp-%airy_bi operators)
(defprop %airy_bi ((z) ((%airy_dbi) z)) grad)

;; airy_bi distributes over lists, matrices, and equations
(defprop %airy_bi (mlist $matrix mequal) distribute_over)

;; airy_bi has mirror symmetry
(defprop %airy_bi t commutes-with-conjugate)

;; Integral of Bi(z)
;; http://functions.wolfram.com/03.06.21.0002.01
;; (z/(3^(1/6)*gamma(2/3)))*hypergeometric([1/3],[2/3,4/3],z^3/9)
;; + (3^(2/3)/(4*%pi))*z^2*gamma(2/3)*hypergeometric([2/3],[4/3,5/3],z^3/9);
(defprop %airy_bi
  ((z)
   ((mplus)
    ((mtimes) 
     ((mexpt) 3 ((rat) -1 6))
     ((mexpt) ((%gamma) ((rat) 2 3)) -1)
     (($hypergeometric) 
      ((mlist) ((rat) 1 3))
      ((mlist) ((rat) 2 3) ((rat) 4 3)) 
      ((mtimes) ((rat) 1 9) ((mexpt) z 3)))
     z)
   ((mtimes) 
    ((rat) 1 4) ((mexpt) 3 ((rat) 2 3)) ((mexpt) $%pi -1) ((%gamma) ((rat) 2 3))
    (($hypergeometric) 
     ((mlist) ((rat) 2 3)) 
     ((mlist) ((rat)  4 3) ((rat) 5 3))
     ((mtimes) ((rat) 1 9) ((mexpt) z 3)))
    ((mexpt) z 2))))
  integral)

(defun airy-bi (z)
  (cond ((floatp z) (airy-bi-real z))
	((complexp z) (airy-bi-complex z))))

(setf (gethash '%airy_bi *flonum-op*) #'airy-bi)

(defmfun simp-%airy_bi (form unused x)
  (declare (ignore unused))
  (oneargcheck form)
  (let ((z (simpcheck (cadr form) x)))
    (cond ((equal z 0) ; A&S 10.4.4: Bi(0) = sqrt(3) 3^(-2/3)/gamma(2/3)
	    '((mtimes simp)
	      ((mexpt simp) 3 ((rat simp) -1 6))
	      ((mexpt simp) ((%gamma simp) ((rat simp) 2 3)) -1)))
	  ((flonum-eval (mop form) z))
	  (t (eqtest (list '(%airy_bi) z) form)))))

;; Derivative dBi/dz of Airy function Bi(z)
(defmfun $airy_dbi (z)
  "Derivative dBi/dz of Airy function Bi(z)"
  (simplify (list '(%airy_dbi) (resimplify z))))

(defprop $airy_dbi %airy_dbi alias)
(defprop $airy_dbi %airy_dbi verb)
(defprop %airy_dbi $airy_dbi reversealias)
(defprop %airy_dbi $airy_dbi noun)
(defprop %airy_dbi simp-%airy_dbi operators)
(defprop %airy_dbi ((z) ((mtimes) z ((%airy_bi) z))) grad)
(defprop %airy_dbi ((z) ((%airy_bi) z)) integral)

;; airy_dbi distributes over lists, matrices, and equations
(defprop %airy_dbi (mlist $matrix mequal) distribute_over)

;; airy_dbi has mirror symmetry
(defprop %airy_dbi t commutes-with-conjugate)

(defun airy-dbi (z)
  (cond ((floatp z) (airy-dbi-real z))
	((complexp z) (airy-dbi-complex z))))

(setf (gethash '%airy_dbi *flonum-op*) #'airy-dbi)

(defmfun simp-%airy_dbi (form unused x)
  (declare (ignore unused))
  (oneargcheck form)
  (let ((z (simpcheck (cadr form) x)))
    (cond ((equal z 0) ; A&S 10.4.5: Bi'(0) = sqrt(3) 3^(-1/3)/gamma(1/3)
          '((mtimes simp) 
	    ((mexpt simp) 3 ((rat simp) 1 6))
	    ((mexpt simp) ((%gamma simp) ((rat simp) 1 3)) -1)))
	  ((flonum-eval (mop form) z))
	  (t (eqtest (list '(%airy_dbi) z) form)))))

;; Numerical routines using slatec functions

(defun airy-ai-real (z)
  " Airy function Ai(z) for real z"
  (declare (type flonum z))
  ;; slatec:dai issues underflow warning for z > zmax.  See dai.{f,lisp}
  ;; This value is correct for IEEE double precision
  (let ((zmax 92.5747007268))
    (declare (type flonum zmax))
    (if (< z zmax) (slatec:dai z) 0.0))) 

(defun airy-ai-complex (z)
  "Airy function Ai(z) for complex z"
  (declare (type (complex flonum) z))
  (multiple-value-bind (var-0 var-1 var-2 var-3 air aii nz ierr)
      (slatec:zairy (realpart z) (imagpart z) 0 1 0.0 0.0 0 0)
    (declare (type flonum air aii)
	     (type f2cl-lib:integer4 nz ierr)
	     (ignore var-0 var-1 var-2 var-3))
    ;; Check nz and ierr for errors
    (if (and (= nz 0) (= ierr 0)) (complex air aii) nil)))

(defun airy-dai-real (z)
  "Derivative dAi/dz of Airy function Ai(z) for real z"
  (declare (type flonum z))
  (let ((rz (sqrt (abs z)))
	(c (* 2/3 (expt (abs z) 3/2))))
    (declare (type flonum rz c))
    (multiple-value-bind (var-0 var-1 var-2 ai dai)
	(slatec:djairy z rz c 0.0 0.0)
      (declare (ignore var-0 var-1 var-2 ai))
      dai)))

(defun airy-dai-complex (z)
  "Derivative dAi/dz of Airy function Ai(z) for complex z"
  (declare (type (complex flonum) z))
  (multiple-value-bind (var-0 var-1 var-2 var-3 air aii nz ierr)
      (slatec:zairy (realpart z) (imagpart z) 1 1 0.0 0.0 0 0)
    (declare (type flonum air aii)
	     (type f2cl-lib:integer4 nz ierr)
	     (ignore var-0 var-1 var-2 var-3))
    ;; Check nz and ierr for errors
    (if (and (= nz 0) (= ierr 0)) (complex air aii) nil)))

(defun airy-bi-real (z)
  "Airy function Bi(z) for real z"
  (declare (type flonum z))
  ;; slatec:dbi issues overflows for z > zmax.  See dbi.{f,lisp}
  ;; This value is correct for IEEE double precision
  (let ((zmax 104.2179765192136))
    (declare (type flonum zmax))
    (if (< z zmax) (slatec:dbi z) nil)))

(defun airy-bi-complex (z)
  "Airy function Bi(z) for complex z"
  (declare (type (complex flonum) z))
  (multiple-value-bind (var-0 var-1 var-2 var-3 bir bii ierr)
      (slatec:zbiry (realpart z) (imagpart z) 0 1 0.0 0.0 0)
    (declare (type flonum bir bii)
	     (type f2cl-lib:integer4 ierr)
	     (ignore var-0 var-1 var-2 var-3))
    ;; Check ierr for errors
    (if (= ierr 0) (complex bir bii) nil)))

(defun airy-dbi-real (z)
  "Derivative dBi/dz of Airy function Bi(z) for real z"
  (declare (type flonum z))
  ;; Overflows for z > zmax.
  ;; This value is correct for IEEE double precision
  (let ((zmax 104.1525))
    (declare (type flonum zmax))
    (if (< z zmax)
	(let ((rz (sqrt (abs z)))
	      (c (* 2/3 (expt (abs z) 3/2))))
        (declare (type flonum rz c))
        (multiple-value-bind (var-0 var-1 var-2 bi dbi)
	    (slatec:dyairy z rz c 0.0 0.0)
	  (declare (type flonum bi dbi)
		   (ignore var-0 var-1 var-2 bi))
	  dbi))
      ;; Will overflow.  Return unevaluated.
      nil)))

(defun airy-dbi-complex (z)
  "Derivative dBi/dz of Airy function Bi(z) for complex z"
  (declare (type (complex flonum) z))
  (multiple-value-bind (var-0 var-1 var-2 var-3 bir bii ierr)
      (slatec:zbiry (realpart z) (imagpart z) 1 1 0.0 0.0 0)
    (declare (type flonum bir bii)
	     (type f2cl-lib:integer4 ierr)
	     (ignore var-0 var-1 var-2 var-3))
    ;; Check ierr for errors
    (if (= ierr 0) (complex bir bii) nil)))
