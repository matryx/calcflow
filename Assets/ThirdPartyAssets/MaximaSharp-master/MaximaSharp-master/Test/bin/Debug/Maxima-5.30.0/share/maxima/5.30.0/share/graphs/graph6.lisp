;;;
;;;  GRAPHS - graph theory package for Maxima
;;;
;;;  Copyright (C) 2007 Andrej Vodopivec <andrej.vodopivec@gmail.com>
;;;
;;;  This program is free software; you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation; either version 2 of the License, or	 
;;;  (at your option) any later version. 
;;;
;;;  This program is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with this program; if not, write to the Free Software
;;;  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;

(in-package :maxima)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file provides read/write support for the graph6 format and
;; read support for the sparse6 format. The description of the formats
;; is here
;;   http://cs.anu.edu.au/~bdm/data/formats.txt
;;
;; There are sites which provide large collections of graphs in these
;; two formats:
;;   http://cs.anu.edu.au/~bdm/data/graphs.html
;;   http://people.csse.uwa.edu.au/gordon/data.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; helper functions
;;

(defun get-n (n)
  (if (< n 63)
      (list (+ n 63))
      (list 126
	    (+ 63 (boole boole-and 63 (ash n -12)))
	    (+ 63 (boole boole-and 63 (ash n -6)))
	    (+ 63 (boole boole-and 63 n)))))

(defun get-r (n)
  (let ((val ()))
    (loop while (> n 0) do
	 (setq val (cons (+ 63 (boole boole-and n 63)) val))
	 (setq n (ash n -6)))
    val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; encode to graph6 string
;;

(defmfun $graph6_encode (gr)
  (require-graph 'graph6_decode 1 gr)
  (graph6-string gr))

(defun graph6-string (gr)
  (let ((n ($graph_order gr))
	(names (get-canonical-names (vertices gr)))
	(e-val))

    ;; encode edges
    (setq e-val 1)
    (loop for i from 1 to (1- n) do
	 (loop for j from 0 to (1- i) do
	      (let ((u (car (rassoc i names)))
		    (v (car (rassoc j names))))
		(setq e-val (ash e-val 1))
		(if (is-edge-in-graph (list (min u v) (max u v)) gr)
		    (incf e-val)))))

    ;; add zeros to the right
    (let ((n1 (mod (/ (* n (1- n)) 2) 6)))
      (unless (= n1 0)
	(setq e-val (ash e-val (- 6 n1)))))

    ;; change bits to string
    (format nil "~{~c~}" (mapcar #'code-char 
				 (append (get-n n) (cdr (get-r e-val))))) ))

(defmfun $graph6_export (graphs fl)
  (unless (stringp fl)
    (merror "`graph6_export': second argument is not a string."))
  (unless ($listp graphs)
    (merror "`graph6_export': first argument is not a list."))
  (with-open-file (stream fl :direction :output :if-exists :supersede)
    (dolist (g (cdr graphs))
      (format stream "~a~%" ($graph6_encode g))))
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; decode from graph6 string
;;

(defmfun $graph6_decode (str)
  ;; get a lisp string
  (unless (stringp str)
    (merror "`graph6_decode': argument is not a string."))

  ;; read bits from string
  (if (and (> (length str) 10)
	   (string= ">>graph6<<" (subseq str 0 10)))
      (setq str (subseq str 10)))

  ;; check if it is actually sparse6
  (when (string= ":" (subseq str 0 1))
    (merror "`graph6_decode': wrong format."))

  ;; read the data
  (let ((n) (g) (e-bits) (e-val 0)
	(bits (mapcar #'char-code (coerce str 'list))))

    (if (< (car bits) 126)
	(progn
	  (setq n (- (car bits) 63))
	  (setq e-bits (cdr bits)))
	(progn
	  (setq bits (cdr bits))
	  (setq n (+ (ash (- (car bits) 63) 12)
		     (ash (- (cadr bits) 63) 6)
		     (- (caddr bits) 63)))
	  (setq e-bits (cdddr bits))))

    (dolist (v e-bits)
      (setq e-val (ash e-val 6))
      (incf e-val (- v 63)))

    ;; remove trailing zeros
    (let ((n1 (mod (/ (* n (1- n)) 2) 6)))
      (unless (= n1 0)
	(setq e-val (ash e-val (- n1 6)))))

    ;; construct graph
    (setq g ($empty_graph n))
    (loop for i from (1- n) downto 0 do
	 (loop for j from (1- i) downto 0 do
	      (if (> (boole boole-and e-val 1) 0)
		  (add-edge (list j i) g))
	      (setq e-val (ash e-val -1))))
    g))

(defmfun $graph6_import (fl)
  (unless (stringp fl)
    (merror "`graph6_import': argument is not a string."))
  (with-open-file (stream fl)
    (let ((lst ()))
      (do ((line (read-line stream nil 'eof)
		 (read-line stream nil 'eof)))
	  ((eq line 'eof))
	(setq lst (cons ($graph6_decode line) lst)))
      `((mlist simp) ,@lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; decode from sparse6 string
;;

(defmfun $sparse6_decode (str)
  ;; get a lisp string
  (unless (stringp str)
    (merror "`sparse6_decode': argument is not a string."))

  ;; read bits from string
  (if (and (> (length str) 11)
	   (string= ">>sparse6<<" (subseq str 0 11)))
      (setq str (subseq str 11)))

  ;; the first character must be ':' and it is ignored
  (unless (string= ":" (subseq str 0 1))
    (merror "`sparse6_decode': wrong format."))
  (setq str (subseq str 1))

  ;; read the data
  (let ((n) (g) (e-bits) (e-val 0) (k 0)
	(bits (mapcar #'char-code (coerce str 'list))))

    (if (< (car bits) 126)
	(progn
	  (setq n (- (car bits) 63))
	  (setq e-bits (cdr bits)))
	(progn
	  (setq bits (cdr bits))
	  (setq n (+ (ash (- (car bits) 63) 12)
		     (ash (- (cadr bits) 63) 6)
		     (- (caddr bits) 63)))
	  (setq e-bits (cdddr bits))))

    (dolist (v e-bits)
      (setq e-val (ash e-val 6))
      (incf e-val (- v 63)))

    ;; find k
    (setq k (integer-length (1- n)))

    ;; construct g
    (setq g ($empty_graph n))
    (let ((v 0) (bi) (xi) (i (1- (integer-length e-val))))

      (loop while (>= i k) do
	   (setq bi (if (logbitp i e-val) 1 0))
	   (decf i)
	   (setq xi 0)
	   (loop for l from 0 to (1- k) do
		(setq xi (ash xi 1))
		(when (logbitp i e-val)
		    (incf xi))
		(decf i))
	   (when (= bi 1)
	     (incf v))
	   (if (> xi v)
	       (setq v xi)
	       (unless (>= v n)
		 ($add_edge `((mlist simp) ,xi ,v) g)))
	   (when (>= v n)
	     (setq i 0))))
    g))

(defmfun $sparse6_import (fl)
  (unless (stringp fl)
    (merror "`sparse6_import': argument is not a string."))
  (with-open-file (stream fl)
    (let ((lst ()))
      (do ((line (read-line stream nil 'eof)
		 (read-line stream nil 'eof)))
	  ((eq line 'eof))
	(setq lst (cons ($sparse6_decode line) lst)))
      `((mlist simp) ,@lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; encode to sparse6 string
;;

(defmfun $sparse6_encode (gr)
  (require-graph 'sparse_encode 1 gr)
  (sparse6-string gr))

(defun sparse6-string (gr)
  (let* ((vrt (reverse (vertices gr)))
	 (n ($graph_order gr))
	 (k (integer-length (1- n)))
	 (names (get-canonical-names vrt))
	 (edges (mapcar #'(lambda (u) (list (cdr (assoc (first u) names))
					    (cdr (assoc (second u) names))))
			(edges gr)))
	 (e-val 0)
	 (v 0))

    ;; Sort the edges
    (setq edges (sort edges
		      #'(lambda (u v) (or (< (second u) (second v))
					  (and (= (second u) (second v))
					       (< (first u) (first v)))))))
    (dolist (e edges)
      (let ((x (first e))
	    (y (second e)))
	
	(if (= y v)
	    (setq e-val (+ (ash e-val (1+ k)) x))
	    (progn
	      (setq e-val (1+ (ash e-val 1)))
	      (incf v)
	      (if (= y v)
		  (setq e-val (+ (ash e-val k) x))
		  (progn
		    (setq e-val (+ (ash e-val k) y))
		    (setq v y)
		    (setq e-val (+ (ash e-val (1+ k)) x))))))))

    (unless (= (mod (integer-length e-val) 6) 0)
      (let ((n1 (mod (integer-length e-val) 6)))
	(setq e-val (1- (ash (1+ e-val) (- 6 n1))))))

    (format nil ":~{~c~}" (mapcar #'code-char 
				 (append (get-n n) (get-r e-val)))) ))

(defmfun $sparse6_export (graphs fl)
  (unless (stringp fl)
    (merror "`sparse6_export': second argument is not a string."))
  (unless ($listp graphs)
    (merror "`sparse6_export': first argument is not a list."))
  (with-open-file (stream fl :direction :output :if-exists :supersede)
    (dolist (g (cdr graphs))
      (format stream "~a~%" ($sparse6_encode g))))
  'done)

;;;;;;;;;;;;;;
;;;
;;; dig6 is similar to graph6 but for directed graphs
;;; (dig6 is used in SAGE)
;;;

(defmfun $dig6_encode (gr)
  (require-digraph 'dig6_decode 1 gr)
  (dig6-string gr))

(defun dig6-string (gr)
  (let ((n ($graph_order gr))
	(names (get-canonical-names (vertices gr)))
	(e-val))

    ;; encode edges
    (setq e-val 1)
    (loop for i from 0 to (1- n) do
	 (loop for j from 0 to (1- n) do
	      (let ((u (car (rassoc i names)))
		    (v (car (rassoc j names))))
		(setq e-val (ash e-val 1))
		(if (is-edge-in-graph (list u v) gr)
		    (incf e-val)))))

    ;; add zeros to the right
    (let ((n1 (mod (* n n) 6)))
      (unless (= n1 0)
	(setq e-val (ash e-val (- 6 n1)))))

    ;; change bits to string
    (format nil "~{~c~}" (mapcar #'code-char 
				 (append (get-n n) (cdr (get-r e-val))))) ))

(defmfun $dig6_export (graphs fl)
  (unless (stringp fl)
    (merror "`dig6_export': second argument is not a string."))
  (unless ($listp graphs)
    (merror "`dig6_export': first argument is not a list."))
  (with-open-file (stream fl :direction :output :if-exists :supersede)
    (dolist (g (cdr graphs))
      (format stream "~a~%" ($dig6_encode g))))
  'done)


(defmfun $dig6_decode (str)
  ;; get a lisp string
  (unless (stringp str)
    (merror "`dig6_decode': argument is not a string."))

  ;; read bits from string
  (if (and (> (length str) 8)
	   (string= ">>dig6<<" (subseq str 0 8)))
      (setq str (subseq str 0)))

  ;; read the data
  (let ((n) (g) (e-bits) (e-val 0)
	(bits (mapcar #'char-code (coerce str 'list))))

    (if (< (car bits) 126)
	(progn
	  (setq n (- (car bits) 63))
	  (setq e-bits (cdr bits)))
	(progn
	  (setq bits (cdr bits))
	  (setq n (+ (ash (- (car bits) 63) 12)
		     (ash (- (cadr bits) 63) 6)
		     (- (caddr bits) 63)))
	  (setq e-bits (cdddr bits))))

    (dolist (v e-bits)
      (setq e-val (ash e-val 6))
      (incf e-val (- v 63)))

    ;; remove trailing zeros
    (let ((n1 (mod (* n n) 6)))
      (unless (= n1 0)
	(setq e-val (ash e-val (- n1 6)))))

    ;; construct graph
    (setq g ($empty_digraph n))
    (loop for i from (1- n) downto 0 do
	 (loop for j from (1- n) downto 0 do
	      (if (> (boole boole-and e-val 1) 0)
		  (add-edge (list j i) g))
	      (setq e-val (ash e-val -1))))
    g))

(defmfun $dig6_import (fl)
  (unless (stringp fl)
    (merror "`dig6_import': argument is not a string."))
  (with-open-file (stream fl)
    (let ((lst ()))
      (do ((line (read-line stream nil 'eof)
		 (read-line stream nil 'eof)))
	  ((eq line 'eof))
	(setq lst (cons ($dig6_decode line) lst)))
      `((mlist simp) ,@lst))))
