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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; matching algorithms
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmfun $max_matching (gr)
  (require-graph 'maximum_matching 1 gr)
  (let ((partition (cdr ($bipartition gr))))
    (if (null partition)
	(maximum-matching-general gr)
	(maximum-matching-bipartite gr (cdr (car partition)) (cdr (cadr partition))))))

;;;;;;;;;;
;;
;; the bipartite case
;;

(defun maximum-matching-bipartite (gr a b &optional cover)
  (let ((matching (make-hash-table))
	(done))

    ;; greedy matching
    (loop for e in (edges gr) do
	 (let ((u (first e))
	       (v (second e)))
	   (when (and (null (gethash u matching))
		      (null (gethash v matching)))
	     (setf (gethash u matching) v)
	     (setf (gethash v matching) u))))

    ;; augment the matching
    (loop while (not done) do
	 (setq done t)
	 (let ((au) (bu))

	   ;; find unmatched vertices
	   (loop for u in a do
		(when (null (gethash u matching))
		  (push u au)))
	   (loop for u in b do
		(when (null (gethash u matching))
		  (push u bu)))

	   ;; find augmenting path
	   (when (not (or (null au)
			  (null bu)))
	     (let ((prev (make-hash-table))
		   (new1 au) (new2) (x))
	       (loop while (not (null new1)) do

		  ;; add edges in M^c
		    (setq new2 ())
		    (loop for u in new1 do
			 (loop for v in (neighbors u gr) do
			      (when (and (null (gethash v prev))
					 (or (null (gethash v matching))
					     (not (= (gethash v matching) u))))
				(setf (gethash v prev) u)
				(push v new2))))
		    (setq new1 ())

		  ;; add edges in M
		    (loop for v in new2 do
			 (let ((u (gethash v matching)))
			   (unless (null u)
			     (push u new1)
			     (setf (gethash u prev) v)))))

	       ;; chech for augmenting path
	       (loop for v in bu while (null x) do
		    (unless (null (gethash v prev))
		      (setq x v)))

	       ;; extend the matching
	       (unless (null x)
		 (setq done nil)
		 (loop while (not (null x)) do
		      (let ((u x)
			    (v (gethash x prev)))
			(setf (gethash u matching) v)
			(setf (gethash v matching) u)
			(setq x (gethash v prev))))) )) ))
    (if (null cover)
	;; we want the matching
	(let ((mmatching ()))
	  (maphash #'(lambda (u v) (if (< u v)
				       (setq mmatching (cons `((mlist simp) ,u ,v) mmatching))))
		   matching)
	  (cons '(mlist simp) mmatching))
	;; we want the vertex cover
	(get-cover-from-matching gr a matching)) ))

(defun get-cover-from-matching (gr a matching)
  (let ((au) (cov))
    (loop for x in (cdr a) do
	 (when (null (gethash x matching))
	   (push x au)))
    (if (null au)
	`((mlist simp) ,@(sort (cdr a) #'<))
	
	;; construct the Hungarian tree
	(let ((prev (make-hash-table))
	      (new1 au) (new2))
	  (loop while (not (null new1)) do
	     ;; add edges in M^c
	       (setq new2 ())
	       (loop for u in new1 do
		    (loop for v in (neighbors u gr) do
			 (when (and (null (gethash v prev))
				    (or (null (gethash v matching))
					(not (= (gethash v matching) u))))
			   (setf (gethash v prev) u)
			   (push v new2))))
		     (setq new1 ())
	       
	     ;; add edges in M
	       (loop for v in new2 do
		    (let ((u (gethash v matching)))
		      (unless (null u)
			(push u new1)
			(setf (gethash u prev) v)))))
	  (maphash #'(lambda (u v)
		       (when (member v a)
			 (if (null (gethash v prev))
			     (push v cov)
			     (push u cov))))
		   matching)
	  `((mlist simp) ,@(sort cov #'<))))))

;;;;;;;;;;;
;;
;; set partiton using hash-tales
;;

(defun main-vertex (v sp)
  (let ((m v))
    (loop while (not (= m (gethash m sp))) do
	 (setq m (gethash m sp)))
    (setf (gethash v sp) m)
    m))

(defun join-sets (v u sp)
  (setf (gethash (main-vertex u sp) sp)
	(main-vertex v sp)))

(defun set-main-vertex (m v sp)
  (setf (gethash (main-vertex v sp) sp) m)
  (setf (gethash m sp) m))

;;;;;;;;;;;
;;
;; the general case
;;

(defvar *matching-state*)
(defvar *matching-matching*)
(defvar *matching-bridge*)
(defvar *matching-prev*)
(defvar *matching-path*)

(defun maximum-matching-general (gr)
  (let ((*matching-matching* (make-hash-table))
	(done))

    ;; greedy matching
    (loop for e in (edges gr) do
	 (let ((u (first e))
	       (v (second e)))
	   (when (and (null (gethash u *matching-matching*))
		      (null (gethash v *matching-matching*)))
	     (setf (gethash u *matching-matching*) v)
	     (setf (gethash v *matching-matching*) u))))

    ;; augment the matching
    (loop while (not done) do
	 (setq done t)
	 (let ((unmatched-vertices)
	       (dfsnum)
	       (dfsnum-curr)
	       (vertex-partition)
	       (*matching-state*)
	       (*matching-bridge* (make-hash-table))
	       (active-edges)
	       (*matching-prev* (make-hash-table))
	       (x))

	   ;; find unmatched vertices
	   (loop for v in (vertices gr) do
		(when (null (gethash v *matching-matching*))
		  (push v unmatched-vertices)))

	   (loop for first-vertex in unmatched-vertices while (null x) do
		(setq dfsnum (make-hash-table))
		(setq dfsnum-curr 0)
		(setq *matching-state* (make-hash-table))
		(setq *matching-bridge* (make-hash-table))
		(setq vertex-partition (make-hash-table))
		(setq *matching-state* (make-hash-table))
		(setf (gethash first-vertex *matching-state*) 'A)
		(setq active-edges ())

		(loop for v in (vertices gr) do
		     (setf (gethash v vertex-partition) v))		
		(loop for u in (neighbors first-vertex gr) do
		     (push (list first-vertex u) active-edges))
		
		
	      ;; find augmenting path
		(loop while (and (not (null active-edges)) (null x)) do
		     (let* ((e (pop active-edges))
			    (v (first e))
			    (w (second e))
			    (v-main (main-vertex v vertex-partition))
			    (w-main (main-vertex w vertex-partition)))
		       
		       (when (null (gethash v-main dfsnum))
			 (setf (gethash v-main dfsnum) dfsnum-curr)
			 (incf dfsnum-curr))
		       
		       (cond

			 ;; we found an agugmented path
			 ((and (member w-main unmatched-vertices)
			       (not (= w-main first-vertex)))
			  (setf (gethash w-main *matching-prev*) v)
			  (setq x w-main))

			 ;; we didn't visit w yet
			 ((null (gethash w-main *matching-state*))
			  (setf (gethash w-main *matching-state*) 'B)
			  (let ((z (gethash w-main *matching-matching*)))
			    (setf (gethash z *matching-state*) 'A)
			    (loop for x in (neighbors z gr) do
				 (unless (= x w-main)
				   (push (list z x) active-edges))))
			  (setf (gethash w-main *matching-prev*) v))

			 ;; found a blossom
			 ((and (not (= w-main v-main)) ;; in the same blossom already
			       (eql (gethash w-main *matching-state*) 'A))

			  (let ((fst) (lst) (b))
			    (if (> (gethash w-main dfsnum)
				   (gethash v-main dfsnum))
				(setq fst w-main
				      lst v-main
				      b (list w v))
				(setq fst v-main
				      lst w-main
				      b (list v w)))

			    (let ((tmp-vrt fst))
			      (loop while (not (= tmp-vrt lst)) do
				   (setf (gethash tmp-vrt *matching-bridge*) b)
				   (join-sets lst tmp-vrt vertex-partition)
				   (if (eql (gethash tmp-vrt *matching-state*) 'A)

				       (progn
					 (setq tmp-vrt (main-vertex (gethash tmp-vrt *matching-matching*)
								    vertex-partition))
					 (loop for u in (neighbors tmp-vrt gr) do
					      (setq u (main-vertex u vertex-partition))
					      (push (list tmp-vrt u) active-edges)))

				       (progn
					 (setq tmp-vrt (main-vertex (gethash tmp-vrt *matching-prev*) 
								    vertex-partition))))))) )
			 )))
		
	      ;; augment the path
		(unless (null x)
		  (setq done nil)
		  (let ((*matching-path* (list x)))
		    (find-augmenting-path first-vertex (gethash x *matching-prev*))
		    (loop while *matching-path* do
			 (let ((a (car *matching-path*))
			       (b (cadr *matching-path*)))
			   (setf (gethash a *matching-matching*) b)
			   (setf (gethash b *matching-matching*) a)
			   (setf *matching-path* (cddr *matching-path*))))
		    ))) ))

    
    (let ((matching ()))
      (maphash #'(lambda (u v) (if (< u v) (setq matching (cons `((mlist simp) ,u ,v) matching))))
	       *matching-matching*)
      (cons '(mlist simp) matching))))

;; returns the path from b to a
(defun find-augmenting-path (a b)
  (cond
    ((= a b)
     (push a *matching-path*))

    ((eql (gethash b *matching-state*) 'A)
     (push b *matching-path*)
     (push (gethash b *matching-matching*) *matching-path*)
     (find-augmenting-path a (gethash (gethash b *matching-matching*) *matching-prev*)))

    (t
     (push b *matching-path*)
     (find-augmenting-path-1 (gethash b *matching-matching*) (first (gethash b *matching-bridge*)))
     (find-augmenting-path a (second (gethash b *matching-bridge*)))) ))

(defun find-augmenting-path-1 (a b)
  (cond
    ((= a b)
     (push a *matching-path*))

    ((eql (gethash b *matching-state*) 'A)
     (find-augmenting-path-1 a (gethash (gethash b *matching-matching*) *matching-prev*))
     (push (gethash b *matching-matching*) *matching-path*)
     (push b *matching-path*))

    (t
     (find-augmenting-path-1 a (second (gethash b *matching-bridge*)))
     (find-augmenting-path (gethash b *matching-matching*) (first (gethash b *matching-bridge*)))
     (push b *matching-path*)) ))
