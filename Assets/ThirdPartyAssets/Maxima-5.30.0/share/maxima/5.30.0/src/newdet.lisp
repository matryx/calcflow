;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module newdet)

;; THIS IS A VERSION OF THE GENTLEMAN-JOHNSON TREE-MINOR DETERMINANT
;; USING RATIONAL FUNCTIONS.  "A" CAN BE A MATRIX OR AN ARRAY.
;; ANSWER IS IN RATIONAL FORM.
;; RJF  5/2/73

(declare-top (special vlist varlist genvar aryp))

;;these are general type arrays

(defvar *i*)
(defvar *minor1*)
(defvar *binom*)
(defvar *input*)

(defmfun $newdet (mat)
  (cond ((not (or (mbagp mat) ($matrixp mat)))
         (if ($scalarp mat) mat (list '(%newdet simp) mat)))
        (t
         (setq mat (check mat))
         (unless (= (length mat) (length (cadr mat)))
           (merror
             (intl:gettext 
               "newdet: Matrix must be square; found ~M rows, ~M columns.")
            (length (cdr mat))
            (length (cdadr mat))))
         (newdet mat (length (cdr mat)) nil))))

(defmfun $permanent (mat)
  (cond ((not (or (mbagp mat) ($matrixp mat)))
         (if ($scalarp mat) mat (list '(%permanent simp) mat)))
        (t
         (setq mat (check mat))
         (unless (= (length mat) (length (cadr mat)))
           (merror
             (intl:gettext 
               "permanent: Matrix must be square; found ~M rows, ~M columns.")
            (length (cdr mat))
            (length (cdadr mat))))
         (newdet mat (length (cdr mat)) t))))

(defun newdet (a n perm)
  (prog (rr k j old new vlist m loc addr sign)
     (when (> n 50)
       (merror (intl:gettext "newdet: matrix must be 50 by 50 or smaller; found size: ~M") n))
     (setq  *binom* (make-array (list (1+ n) (1+ n)) :element-type 'integer))
     (setq  *minor1* (make-array (list 2 (1+ (setq rr (pascal n))))))
     (setq  *i* (make-array (+ 2 n)))
     (do ((k 0 (1+ k)))
	 ((> k 1))
       (do ((j 0 (1+ j)))
	   ((> j rr))
	 (setf (aref *minor1* k j) '(0 . 1))))
     (do ((k 0 (1+ k)))
	 ((> k (1+ n)))
       (setf (aref *i* k) -1))
     (setq  *input* (make-array (list (1+ n) (1+ n))))
     (do ((k 1 (1+ k)))
	 ((> k n))
       (do ((j 1 (1+ j)))
	   ((> j n))
	 (newvar1 (setf (aref *input* k j) (let ((aryp t)) (maref a k j))))))
     (newvar (cons '(mtimes) vlist))
     (do ((k 1 (1+ k)))
	 ((> k n))
       (do ((j 1 (1+ j)))
	   ((> j n))
	 (setf (aref *input* k j) (cdr (ratrep* (aref *input* k j))))))
     (setq new 1)
     (setq old 0)
     (setf (aref *i* 0) n)
     (do ((loc 1 (1+ loc)))
	 ((> loc n))
       (setf (aref *minor1* old (1- loc)) (aref *input* 1 loc)))
     (setq m 1)
     g0193 (when (> m (1- n)) (go ret))
     (setq loc 0)
     (setq j 1)
     g0189 (when (> j m) (go nextminor))
     (setf (aref *i* j) (- m j))
     (incf j)
     (go g0189)
     nextminor
     (cond ((not (equal (aref *minor1* old loc) '(0 . 1)))
	    (setq k (1- n))
	    (setq j 0)
	    (setq addr (+ loc (aref *binom* k (1+ m))))
	    (setq sign 1))
	   (t (go over)))
     nextuse
     (cond
       ((equal k (aref *i* (1+ j)))
	(incf j)
	(setq sign (- sign)))
       (t
	(setf (aref *minor1* new addr)
	      (ratplus
	       (aref *minor1* new addr)
	       (rattimes (aref *minor1* old loc)
			 (cond ((or (= sign 1) perm)
				(aref *input* (1+ m) (1+ k)))
			       (t (ratminus (aref *input* (1+ m) (1+ k)))))
			 t)))))
     (when (> k 0)
       (decf k)
       (decf addr (aref *binom* k (- m j)))
       (go nextuse))
     (setf (aref *minor1* old loc)  '(0 . 1))
     over (incf loc)
     (setq j m)
     back (when (> 1 j)
	    (incf m)
	    (setq old (- 1 old))
	    (setq new (- 1 new))
	    (go g0193))
     (setf (aref *i* j) (1+ (aref *i* j)))
     (if (> (aref *i* (1- j)) (aref *i* j))
	 (go nextminor)
	 (setf (aref *i* j) (- m j)))

     (decf j)
     (go back)
     ret
     (return (cons (list 'mrat 'simp varlist genvar) (aref *minor1* old 0)))))

(defun pascal (n)
  (setf (aref *binom* 0 0) 1)
  (do ((h 1 (1+ h)))
      ((> h n) (1- (aref *binom* n (ash n -1))))
    (setf (aref *binom* h 0) 1)
    (setf (aref *binom* (1- h) h) 0)
    (do ((j 1 (1+ j)))
	((> j h))
      (setf (aref *binom* h j) (+ (aref *binom* (1- h) (1- j)) (aref *binom* (1- h) j))))))
