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


(defmfun $is_isomorphic (gr1 gr2)
  (or (= 0 ($graph_order gr1) ($graph_order gr2))
      (and (graph-p gr1) (graph-p gr2) (isomorphism-graphs gr1 gr2) t)
      (and (digraph-p gr1) (digraph-p gr2) (isomorphism-digraphs gr1 gr2) t)))

(defmfun $isomorphism (gr1 gr2)
  (cond ((graph-p gr1)
	 (if (graph-p gr2)
	     (let (res
		   (iso (isomorphism-graphs gr1 gr2)))
	       (when iso
		 (maphash
		  (lambda (key val)
		    (setq res (cons `((marrow simp) ,key ,val) res)))
		  iso))
	       (cons '(mlist simp) res))
	     (merror "Wrong inputs to isomorphism")))
	((digraph-p gr1)
	 (if (digraph-p gr2)
	     (let (res
		   (iso (isomorphism-digraphs gr1 gr2)))
	       (when iso
		 (maphash
		  (lambda (key val)
		    (setq res (cons `((marrow simp) ,key ,val) res)))
		  iso))
	       (cons '(mlist simp) res))
	     (merror "Wrong inputs to isomorphism")))
	(t
	 (merror "Wrong inputs to isomorphism"))))

(defun isomorphism-graphs (gr1 gr2)
  
  ;; check the degree sequences
  (let ((gr1-degrees (cdr ($degree_sequence gr1)))
	(gr2-degrees (cdr ($degree_sequence gr2))))
    (unless (= (length gr1-degrees)
	       (length gr2-degrees))
      (return-from isomorphism-graphs nil))
    (loop while (and gr1-degrees gr2-degrees) do
	 (unless (= (car gr1-degrees)
		    (car gr2-degrees))
	   (return-from isomorphism-graphs nil))
	 (pop gr1-degrees)
	 (pop gr2-degrees))
    (when (or gr1-degrees gr2-degrees)
      (return-from isomorphism-graphs nil)))

  (let ((n (graph-order gr1))
	(m (graph-size gr1)))
    (when (< (* n (1- n)) (* 4 m))
      (setq gr1 ($complement_graph gr1))
      (setq gr2 ($complement_graph gr2))))

  (let ((mapping (make-hash-table))
	(m1) (m2) (out1) (out2))
    (extend-isomorphism-graphs mapping m1 m2 out1 out2 gr1 gr2)))

(defun extend-isomorphism-graphs (mapping m1 m2 out1 out2 gr1 gr2)
  ;; check if we have found an isomorphism
  (when (= (length m1) ($graph_order gr1))
    (return-from extend-isomorphism-graphs mapping))

  ;; try extending the mapping
  (let (n-set m)
    ;; compute the new candidates for mattching
    (cond
      (out1
       (unless out2
	 (return-from extend-isomorphism-graphs nil))
       (setq n-set out1)
       (setq m (apply #'min out2)))
      (out2
       (return-from extend-isomorphism-graphs nil))
      (t
       (dolist (v (vertices gr1))
	 (unless (member v m1)
	   (push v n-set)))
       (let ((m-set))
	 (dolist (v (vertices gr2))
	   (unless (member v m2)
	     (push v m-set)))
	 (setq m (apply #'min m-set)))))

    ;; try extending the mapping
    (dolist (n n-set)

      ;; we have a pair (n->m)
      (let ((ok t))
	;; n and m must have the same degree
	(unless (= ($vertex_degree n gr1)
		   ($vertex_degree m gr2))
	  (setq ok nil))
	;; check if adjacencies are preserved
	(loop for x in m1 while ok do
	     (if (member x (neighbors n gr1))
		 (unless (member (gethash x mapping)
				 (neighbors m gr2))
		   (setq ok nil))
		 (when (member (gethash x mapping)
			       (neighbors m gr2))
		   (setq ok nil))))
	
	(when ok
	  ;; compute the new state
	  (let ((out1-prime) (out2-prime)
		(m1-prime (cons n m1))
		(m2-prime (cons m m2)))
	    ;; compute new out sets
	    (dolist (v m1-prime)
	      (dolist (u (neighbors v gr1))
		(unless (or (member u out1-prime)
			    (member u m1-prime))
		  (push u out1-prime))))
	    (dolist (v m2-prime)
	      (dolist (u (neighbors v gr2))
		(unless (or (member u out2-prime)
			    (member u m2-prime))
		  (push u out2-prime))))
	    
	    ;; if we are compatible try extending
	    (when (= (length out1-prime) (length out2-prime))
	      (setf (gethash n mapping) m)
	      (let ((extended (extend-isomorphism-graphs mapping
							 m1-prime m2-prime
							 out1-prime out2-prime
							 gr1 gr2)))
		(when extended
		  (return-from extend-isomorphism-graphs extended)))
	      (remhash n mapping)))) ))))

(defun isomorphism-digraphs (gr1 gr2)
  
  ;; check the degree sequences
  (let ((indegrees1) (outdegrees1)
	(indegrees2) (outdegrees2))
    (dolist (v (vertices gr1))
      (push ($vertex_in_degree v gr1) indegrees1)
      (push ($vertex_out_degree v gr1) outdegrees1))
    (dolist (v (vertices gr2))
      (push ($vertex_in_degree v gr2) indegrees2)
      (push ($vertex_out_degree v gr2) outdegrees2))
    (setq indegrees1 (sort indegrees1 #'<))
    (setq indegrees2 (sort indegrees2 #'<))
    (setq outdegrees1 (sort outdegrees1 #'<))
    (setq outdegrees2 (sort outdegrees2 #'<))
    (loop while (and indegrees1 indegrees2) do
	 (unless (and (= (car indegrees1) (car indegrees2))
		      (= (car outdegrees1) (car outdegrees2)))
	   (return-from isomorphism-digraphs nil))
	 (setq indegrees1 (cdr indegrees1))
	 (setq indegrees2 (cdr indegrees2))
	 (setq outdegrees1 (cdr outdegrees1))
	 (setq outdegrees2 (cdr outdegrees2)))
    (when (or indegrees1 indegrees2)
      (return-from isomorphism-digraphs nil)))

  (let ((mapping (make-hash-table))
	(m1) (m2) (out1) (out2) (in1) (in2))
    (extend-isomorphism-digraphs mapping m1 m2 out1 out2 in1 in2 gr1 gr2)))

(defun extend-isomorphism-digraphs (mapping m1 m2 out1 out2 in1 in2 gr1 gr2)
  ;; check if we have found an isomorphism
  (when (= (length m1) ($graph_order gr1))
    (return-from extend-isomorphism-digraphs mapping))

  ;; try extending the mapping
  (let (n-set m)
    ;; compute the new candidates for mattching
    (cond
      (out1
       (unless out2
	 (return-from extend-isomorphism-digraphs nil))
       (setq n-set out1)
       (setq m (apply #'min out2)))
      (out2
       (return-from extend-isomorphism-digraphs nil))
      (in1
       (unless in2
	 (return-from extend-isomorphism-digraphs nil))
       (setq n-set in1)
       (setq m (apply #'min in2)))
      (t
       (dolist (v (vertices gr1))
	 (unless (member v m1)
	   (push v n-set)))
       (let ((m-set))
	 (dolist (v (vertices gr2))
	   (unless (member v m2)
	     (push v m-set)))
	 (setq m (apply #'min m-set)))))

    ;; try extending the mapping
    (dolist (n n-set)

      ;; we have a pair (n->m)
      (let ((ok t))
	;; n and m must have the same degree
	(unless (and (= ($vertex_in_degree n gr1)
			($vertex_in_degree m gr2))
		     (= ($vertex_out_degree n gr1)
			($vertex_out_degree m gr2)))
	  (setq ok nil))
	;; check if adjacencies are preserved
	(loop for x in m1 while ok do
	     (if (member x (out-neighbors n gr1))
		 (unless (member (gethash x mapping)
				 (out-neighbors m gr2))
		   (setq ok nil))
		 (when (member (gethash x mapping)
			       (out-neighbors m gr2))
		   (setq ok nil)))
	     (if (member x (out-neighbors n gr1))
		 (unless (member (gethash x mapping)
				 (out-neighbors m gr2))
		   (setq ok nil))
		 (when (member (gethash x mapping)
			       (out-neighbors m gr2))
		   (setq ok nil))))
	
	(when ok
	  ;; compute the new state
	  (let ((out1-prime) (out2-prime)
		(in1-prime) (in2-prime)
		(m1-prime (cons n m1))
		(m2-prime (cons m m2)))
	    ;; compute new out sets
	    (dolist (v m1-prime)
	      (dolist (u (in-neighbors v gr1))
		(unless (or (member u in1-prime)
			    (member u m1-prime))
		  (push u in1-prime)))
	      (dolist (u (out-neighbors v gr1))
		(unless (or (member u out1-prime)
			    (member u m1-prime))
		  (push u out1-prime))))
	    (dolist (v m2-prime)
	      (dolist (u (in-neighbors v gr2))
		(unless (or (member u in2-prime)
			    (member u m2-prime))
		  (push u in2-prime)))
	      (dolist (u (out-neighbors v gr2))
		(unless (or (member u out2-prime)
			    (member u m2-prime))
		  (push u out2-prime))))
	    
	    ;; if we are compatible try extending
	    (when (and (= (length out1-prime) (length out2-prime))
		       (= (length in1-prime) (length in2-prime)))
	      (setf (gethash n mapping) m)
	      (let ((extended (extend-isomorphism-digraphs mapping
							 m1-prime m2-prime
							 out1-prime out2-prime
							 in1-prime in2-prime
							 gr1 gr2)))
		(when extended
		  (return-from extend-isomorphism-digraphs extended)))
	      (remhash n mapping)))) ))))
