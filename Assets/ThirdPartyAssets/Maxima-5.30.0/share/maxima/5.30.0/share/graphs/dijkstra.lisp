;;;
;;;  GRAPHS - graph theory package for Maxima
;;;
;;;  Copyright (C) 2008 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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

;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implementation of a binary heap priority queue
;;;

(in-package :maxima)

(defstruct graphs-pqueue
  (data (make-array 129 :adjustable t))
  (last 0)  ;; last index used
  (max 128) ;; max possible index
  (index (make-hash-table))
  (weights (make-hash-table)))

(defun graphs-pqueue-insert (v w queue)
  (if (= (graphs-pqueue-last queue) 0)
      (progn
	;; insert into an empty queue
	(setf (graphs-pqueue-last queue) 1)
	(setf (aref (graphs-pqueue-data queue) 1) v)
	(setf (gethash v (graphs-pqueue-index queue)) 1)
	(setf (gethash v (graphs-pqueue-weights queue)) w))
      (progn
	;; resize the queue if needed
	(when (= (graphs-pqueue-max queue) (graphs-pqueue-last queue))
	  (adjust-array (graphs-pqueue-data queue)
			(1+ (* (graphs-pqueue-max queue) 2)))
	  (setf (graphs-pqueue-max queue)
		(* (graphs-pqueue-max queue) 2)))
	;; insert the element
	(incf (graphs-pqueue-last queue))
	(setf (aref (graphs-pqueue-data queue) (graphs-pqueue-last queue)) v)
	(setf (gethash v (graphs-pqueue-index queue)) (graphs-pqueue-last queue))
	(setf (gethash v (graphs-pqueue-weights queue)) w)
	;; balance the queue
	(let* ((ind (graphs-pqueue-last queue))
	       (ind-new (truncate ind 2)))
	  (loop while (and (> ind 1)
			   (mlsp w (gethash (aref (graphs-pqueue-data queue) ind-new)
					    (graphs-pqueue-weights queue))))
	     do
	       (rotatef (aref (graphs-pqueue-data queue) ind)
			(aref (graphs-pqueue-data queue) ind-new))
	       (rotatef (gethash (aref (graphs-pqueue-data queue) ind) (graphs-pqueue-index queue))
			(gethash (aref (graphs-pqueue-data queue) ind-new) (graphs-pqueue-index queue)))
	       (setq ind ind-new)
	       (setq ind-new (truncate ind 2)))))))

(defun graphs-pqueue-emptyp (queue)
  (= (graphs-pqueue-last queue) 0))

(defun graphs-pqueue-first (queue)
  (aref (graphs-pqueue-data queue) 1))

(defun graphs-pqueue-get-weight (v queue)
  (gethash v (graphs-pqueue-weights queue)))

(defun graphs-pqueue-pop (queue)
  (when (graphs-pqueue-emptyp queue)
    (return-from graphs-pqueue-pop nil))
  (let ((top (graphs-pqueue-first queue))
	(last (aref (graphs-pqueue-data queue)
		    (graphs-pqueue-last queue)))
	(pos 1)
	min-child)
    ;; remove the top element
    (remhash top (graphs-pqueue-weights queue))
    (remhash top (graphs-pqueue-index queue))
    (decf (graphs-pqueue-last queue))
    ;; put the last element into the first position
    (setf (aref (graphs-pqueue-data queue) 1) last)
    (setf (gethash last (graphs-pqueue-index queue)) 1)
    ;; rebalance the queue
    (loop while t do
	 ;; find the min child
	 (setf min-child (* 2 pos))
	 (when (> min-child (graphs-pqueue-last queue))
	   (return-from graphs-pqueue-pop top))
	 (when (and (<= (1+ min-child) (graphs-pqueue-last queue))
		    (mlsp (gethash (aref (graphs-pqueue-data queue) (1+ min-child))
				   (graphs-pqueue-weights queue))
			  (gethash (aref (graphs-pqueue-data queue) min-child)
				   (graphs-pqueue-weights queue))))
	   (setq min-child (1+ min-child)))
	 ;; rotate if needed
	 (when (mlsp (gethash (aref (graphs-pqueue-data queue) pos)
			      (graphs-pqueue-weights queue))
		     (gethash (aref (graphs-pqueue-data queue) min-child)
			      (graphs-pqueue-weights queue)))
	   (return-from graphs-pqueue-pop top))
	 (rotatef (aref (graphs-pqueue-data queue) pos)
		  (aref (graphs-pqueue-data queue) min-child))
	 (rotatef (gethash (aref (graphs-pqueue-data queue) pos) (graphs-pqueue-index queue))
		  (gethash (aref (graphs-pqueue-data queue) min-child) (graphs-pqueue-index queue)))
	 (setq pos min-child))))

(defun graphs-pqueue-contains (v queue)
  (not (eq (gethash v (graphs-pqueue-weights queue) 'not-contained)
	   'not-contained)))

(defun graphs-pqueue-set-weight (v w queue)
  (setf (gethash v (graphs-pqueue-weights queue)) w)
  (let ((pos (gethash v (graphs-pqueue-index queue))))
    ;; rebalance the queue
    (let* ((ind pos)
	   (ind-new (truncate ind 2)))
      (loop while (and (> ind 1)
		       (mlsp w (gethash (aref (graphs-pqueue-data queue) ind-new)
					(graphs-pqueue-weights queue))))
	 do
	   (rotatef (aref (graphs-pqueue-data queue) ind)
		    (aref (graphs-pqueue-data queue) ind-new))
	   (rotatef (gethash (aref (graphs-pqueue-data queue) ind) (graphs-pqueue-index queue))
		    (gethash (aref (graphs-pqueue-data queue) ind-new) (graphs-pqueue-index queue)))
	   (setq ind ind-new)
	   (setq ind-new (truncate ind 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dijkstra's algorithm for weighted shortest paths
;;;

(defun dijkstra (v v1 g)
  (let ((previous (make-hash-table))
	(distance (make-hash-table))
	(cont t)
	(my-inf 1)
	(pq (make-graphs-pqueue)))

    (dolist (e (cdr ($edges g)))
      (setq my-inf (m+ my-inf ($abs ($get_edge_weight e g 1)))))

    ;; initialize the pqueue
    (dolist (u (vertices g))
      (if (= u v)
	  (progn
	    (graphs-pqueue-insert u 0 pq)
	    (setf (gethash u distance) 0))
	  (progn
	    (graphs-pqueue-insert u my-inf pq)
	    (setf (gethash u distance) my-inf))))
    ;; find the shortest path
    (loop while (and cont (not (graphs-pqueue-emptyp pq))) do
	 ;; find the closest remaining vertex
	 (let* ((u (graphs-pqueue-pop pq))
		(u-distance (gethash u distance)))
	   (if (or (eq (gethash u distance) '$inf)
		   (= u v1))
	       (setq cont nil)
	       (progn
		 (dolist (w (if (graph-p g) (neighbors u g) (out-neighbors u g)))
		   (when (graphs-pqueue-contains w pq)
		     (let ((alt (m+ u-distance ($get_edge_weight `((mlist simp) ,u ,w) g))))
		       (when (mlsp alt (graphs-pqueue-get-weight w pq))
			 (setf (gethash w previous) u)
			 (setf (gethash w distance) alt)
			 (graphs-pqueue-set-weight w alt pq)))))))))

    (dolist (v (vertices g))
      (when (equal (gethash v distance) my-inf)
	(setf (gethash v distance) '$inf)))

    (values distance previous)))

(defmfun $shortest_weighted_path (v u g)
  (require-graph-or-digraph 'shortest_weighted_path 3 g)
  (require-vertex 'shortest_weighted_path 1 v)
  (require-vertex-in-graph 'shortest_weighted_path v g)
  (require-vertex 'shortest_weighted_path 2 u)
  (require-vertex-in-graph 'shortest_weighted_path u g)
  (multiple-value-bind (l prev)
      (dijkstra v u g)
    (setq l (gethash u l))
    (if (eq l '$inf)
	'((mlist simp) $inf ((mlist simp)))
	(let ((tu u) (p (list u)))
	  (loop while (not (= tu v)) do
	       (setq tu (gethash tu prev))
	       (setq p (cons tu p)))
	  `((mlist simp) ,l ((mlist simp) ,@p))))))
