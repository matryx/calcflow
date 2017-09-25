;;;
;;;  GRAPHS - graph theory package for Maxima
;;;
;;;  Copyright (C) 2007-2011 Andrej Vodopivec <andrej.vodopivec@gmail.com>
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; force-based graph embedding algorithm
;;;
;;; Based on:
;;; T.M.J. Fruchterman, E.M. Reingold, Graph drawing by force-directed
;;; placement, Software practice and experience 21 (1991), 11, 1129--1164.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(defvar *vertex-position*)
(defvar *optimal-distance*)

(defvar *epsilon-distance* 0.5)
(defvar *frame-width* 10.0)

(defvar *fixed-vertices* nil)

(defun attractive-force (d)
  (/ (* d d) *optimal-distance*))

(defun repulsive-force (d)
  (let ((d (max d *epsilon-distance*)))
    (/ (* *optimal-distance* *optimal-distance*) d 100)))

(defun distance (p1 p2)
  (let ((d (mapcar #'- p1 p2)))
    (sqrt (apply #'+ (mapcar #'* d d)))))

(defun random-positions (v-list dimension)
  (when *fixed-vertices*
    (let ((n (length *fixed-vertices*)))
      (dotimes (i (length *fixed-vertices*))
	(let ((v (nth i *fixed-vertices*))
	      (x (* *frame-width* ($sin (/ (* 2 i pi) n))))
	      (y (* *frame-width* ($cos (/ (* 2 i pi) n)))))
	  (setf (gethash v *vertex-position*)
		(list x y))))))
  (dolist (v v-list)
    (unless (member v *fixed-vertices*)
      (let* ((x (- *frame-width* (random (* 2 *frame-width*))))
	     (y (- *frame-width* (random (* 2 *frame-width*))))
	     (z (- *frame-width* (random (* 2 *frame-width*)))))
	(setf (gethash v *vertex-position*)
	      (if (= dimension 3)
		  (list x y z)
		  (list x y)))))))

(defmfun $spring_embedding (g depth fixed-vertices dimension continue)
  (let ((*vertex-position* (make-hash-table))
	(vertex-displacement (make-hash-table))
	(*fixed-vertices* (cdr fixed-vertices))
	(*optimal-distance* (/ (* 2 *frame-width*)
			       (sqrt ($graph_order g)))))

    ;; Start with current positions if we already have some.
    (if (and continue
             (> (length ($get_positions g)) 1)
             (= ($length ($first ($get_positions g))) dimension))
        (dolist (v (cdr ($get_positions g)))
          (setf (gethash (cadr v) *vertex-position*) (cdaddr v)))
        (random-positions (vertices g) dimension))

    (let* ((step (/ *frame-width* 5))
	   (d-step (/ step (1+ depth))))
      (dotimes (i depth)
	(setq step (- step d-step))
      
	(dolist (v (vertices g))
	  (setf (gethash v vertex-displacement) (if (= dimension 2) (list 0 0) (list 0 0 0))))

	;; calculate repulsive forces
	(when (null *fixed-vertices*)
	  (let ((v-vrt (vertices g)))
	    (loop while v-vrt do
		 (let* ((v (car v-vrt))
			(u-vrt (cdr v-vrt))
			(v-pos (gethash v *vertex-position*)))
		   (loop while u-vrt do
			(let* ((u (car u-vrt))
			       (u-pos (gethash u *vertex-position*))
			       (delta (mapcar #'- v-pos u-pos))
			       (delta-abs (distance v-pos u-pos))
			       (force (repulsive-force delta-abs))
			       (vu-disp (mapcar
					 #'(lambda (u) (* (/ u (max delta-abs *epsilon-distance*)) force))
					 delta))
			       (v-disp (gethash v vertex-displacement))
			       (u-disp (gethash u vertex-displacement)))
			  (setf (gethash v vertex-displacement)
				(mapcar #'+ v-disp vu-disp)
				(gethash u vertex-displacement)
				(mapcar #'- u-disp vu-disp))
			  (setq u-vrt (cdr u-vrt)))))
		 (setq v-vrt (cdr v-vrt)))))
	
	;; calculate attractive forces
	(dolist (e (edges g))
	  (let* ((v (first e))
		 (u (second e))
		 (v-pos (gethash v *vertex-position*))
		 (u-pos (gethash u *vertex-position*))
		 (delta (mapcar #'- v-pos u-pos))
		 (delta-abs (distance v-pos u-pos))
		 (v-disp (gethash v vertex-displacement))
		 (u-disp (gethash u vertex-displacement))
		 (force (attractive-force delta-abs))
		 (vu-disp (mapcar
			   #'(lambda (u)
			       (* (/ u (max delta-abs *epsilon-distance*)) force))
			   delta)))
	    (setf (gethash v vertex-displacement)
		  (mapcar #'- v-disp vu-disp)
		  (gethash u vertex-displacement)
		  (mapcar #'+ u-disp vu-disp))))
	
	;; Limit the displacement
	(dolist (v (vertices g))
	  (unless (member v *fixed-vertices*)
	    (let* ((v-disp (gethash v vertex-displacement))
		   (v-disp (mapcar #'(lambda (u) (/ u 2)) v-disp))
		   (v-disp-abs (sqrt (apply #'+ (mapcar #'* v-disp v-disp))))
		   (v-pos (gethash v *vertex-position*)))
	      (if (> v-disp-abs step)
		  (setq v-pos (mapcar #'(lambda (u v)
					  (+ u (* (/ v v-disp-abs) step)))
				      v-pos v-disp))
		  (setq v-pos (mapcar #'+ v-pos v-disp)))
	      (setq v-pos (mapcar #'(lambda (u) (min *frame-width* (max u (- *frame-width*))))
				  v-pos))
	      (setf (gethash v *vertex-position*) v-pos))))
	))
	
    (let (result)
      (maphash #'(lambda (vrt pos)
		   (setq result
			 (cons `((mlist simp) ,vrt ((mlist simp) ,@pos))
			       result)))
	       *vertex-position*)
      (cons '(mlist simp) result)) ))
