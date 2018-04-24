;;;; f90.lisp -- Application command line argument retrieval
;;;;                      and processing for Common Lisp.

;;;; Copyright (C) 2004 James F. Amundson

;;;; f90.lisp is free software; you can redistribute it
;;;; and/or modify it under the terms of the GNU General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2, or (at your option) any later version.

;;;; f90.lisp is distributed in the hope that it will be
;;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;; See the GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with f90.lisp; see the file COPYING.  If not,
;;;; write to the Free Software Foundation, Inc., 59 Temple Place -
;;;; Suite 330, Boston, MA 02111-1307, USA.

;;;; Based on fortra.lisp. Copyright statements for fortra.lisp follow:
;;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas
;;;;     All rights reserved
;;;;  (c) Copyright 1980 Massachusetts Institute of Technology

;;;; Output from f90 is "free form": no special attention to columns.
;;;; Lines longer than *F90-OUTPUT-LINE-LENGTH-MAX* are broken with
;;;; trailing ampersand (no additional spaces).

;;;; Commentary from the Texinfo for f90:
;;;; "The @code{f90} implementation was done as a quick hack.
;;;; It is not a necessarily a good example upon which to base
;;;; other language translations."

(in-package :maxima)

(macsyma-module f90)

(defvar *f90-output-line-length-max* 65.)

(defun f90-print (x
		  &aux
		  ;; This is a poor way of saying that array references
		  ;; are to be printed with parens instead of brackets.
		  (*lb* #\()
		  (*rb* #\)))
  ;; Restructure the expression for displaying.
  (setq x (fortscan x))
  ;; Linearize the expression using MSTRING.  Some global state must be
  ;; modified for MSTRING to generate using Fortran syntax.  This must be
  ;; undone so as not to modifiy the toplevel behavior of MSTRING.
  (unwind-protect
       (defprop mexpt msize-infix grind)
    (defprop mminus 100 lbp)

    (defprop msetq (#\:) strsym)
    (setq x (coerce (mstring x) 'string))
    ;; Make sure this gets done before exiting this frame.
    (defprop mexpt msz-mexpt grind)
    (remprop 'mminus 'lbp))

  (if (>= (length x) *f90-output-line-length-max*)

    ;; Split this line and print it with trailing ampersand.
    ;; Previous scheme to break the lines nicely had some bugs;
    ;; it's simpler to break at a fixed length.

    (let ((line x) (break-point *f90-output-line-length-max*))
      (princ (subseq line 0 break-point))
      (princ "&")
      (terpri)
      (princ "&")
      (setf line (subseq line break-point))
      
      (loop while (> (length line) break-point) do
        (princ (subseq line 0 break-point))
        (princ "&")
        (terpri)
        (princ "&")
        (setf line (subseq line break-point)))

      (if (> (length line) 0)
        (princ line)))

    (princ x))

  (terpri)
  '$done)

;; Takes a name and a matrix and prints a sequence of F90 assignment
;; statements of the form
;;  NAME(I,J) = <corresponding matrix element>
;; or, when the second argument is a list,
;;  NAME(I) = <list element>

(defmfun $f90mx (name mat)
  (cond ((not (symbolp name))
	 (merror "f90mx: first argument must be a symbol; found: ~M" name))
	((not (or ($matrixp mat) ($listp mat)))
	 (merror "f90mx: second argument must be a list or matrix; found: ~M" mat)))
  (cond
    (($matrixp mat)
     (do ((mat (cdr mat) (cdr mat)) (i 1 (1+ i)))
       ((null mat))
       (do ((m (cdar mat) (cdr m)) (j 1 (1+ j)))
         ((null m))
         (f90-print `((mequal) ((,name) ,i ,j) ,(car m))))))
    (($listp mat)
     (do ((mat (cdr mat) (cdr mat)) (i 1 (1+ i)))
       ((null mat))
       (f90-print `((mequal) ((,name) ,i) ,(car mat))))))
  '$done)

(defmspec $f90 (expr)
  (dolist (l (cdr expr))
  (let ((value (strmeval l)))
    (cond ((msetqp l) (setq value `((mequal) ,(cadr l) ,(meval l)))))
    (cond ((and (symbolp l) (or ($matrixp value) ($listp value)))
	   ($f90mx l value))
	  ((and (not (atom value)) (eq (caar value) 'mequal)
		(symbolp (cadr value)) (or ($matrixp (caddr value)) ($listp (caddr value))))
	   ($f90mx (cadr value) (caddr value)))
	  (t (f90-print value))))))
