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
;;; Demoucron planarity test algorithm (a simple quadratic-time
;;; planarity test algorithm)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; debugging
;;

(defvar $demoucron_debug nil)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; finds a cycle in g - g should be connected
;;

(defvar *back-edges*)
(defvar *dfsnum*)
(defvar *dfsnum-curr*)
(defvar *dfs-prev*)

(defun find-cycle (gr)
  (let ((*back-edges* ())
	(*dfsnum* (make-hash-table))
	(*dfs-prev* (make-hash-table))
	(*dfsnum-curr* 0)
	(v (first (vertices gr)))
	(cycle ()))
    (setf (gethash v *dfs-prev*) v)
    (dfs-find-cycle v gr)
    (if (null *back-edges*)
	()
	(progn
	  (let ((u (caar *back-edges*))
		(v (cadar *back-edges*)))
	    (loop while (not (= u v)) do
		 (push u cycle)
		 (setq u (gethash u *dfs-prev*)))
	    (push v cycle))
	  cycle))))

(defun dfs-find-cycle (v gr)
  (setf (gethash v *dfsnum*) *dfsnum-curr*)
  (incf *dfsnum-curr*)
  (loop for u in (neighbors v gr) do
       (if (null (gethash u *dfsnum*))
	   (progn
	     (setf (gethash u *dfs-prev*) v)
	     (dfs-find-cycle u gr))
	   (when (not (= u (gethash v *dfs-prev*)))
	     (push (list u v) *back-edges*)))))

(defmfun $find_cycle (gr)
  (require-graph 'find_cycle 1 gr)
  (let ((cycle (find-cycle gr)))
    `((mlist simp) ,@cycle)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; demoucron algorithm for a biconnected graph gr
;;

(defvar *h-vertices*)
(defvar *h-edges*)
(defvar *g-vertices*)
(defvar *embedding*)

;;;;;;;;
;;
;; finds facial walks of h (running time o(E(h)))
;;

(defvar *facial-walks*)

(defun find-facial-walks (edges)
  (let ((visited-edges (make-hash-table :test #'equal)))
    (loop for e in edges do
	 (when (null (gethash e visited-edges))
	   (let* ((u (first e))
		  (v (second e))
		  (walk (list u))
		  (z (gethash (list v u) *embedding*)))
	     (setf (gethash (list u v) visited-edges) t)
	     (setq u v)
	     (setq v z)
	     (while (not (and (= u (first e))
			      (= v (second e))))
	       (setf (gethash (list u v) visited-edges) t)
	       (push u walk)
	       (setq z (gethash (list v u) *embedding*))
	       (setq u v)
	       (setq v z))
	     (push walk *facial-walks*)))) ))

;;;;;;;;
;;
;; finds the bridges in g-h
;;  - a bridge is a list (vertices-of-the-bridge attachements-of-the-bridge)
;;

(defvar *bridges*)
(defvar *current-bridge*)
(defvar *current-attachements*)
(defvar *visited-vertices*)

(defun find-bridges (gr)
  (setq *bridges* ())
  (setq *visited-vertices* (make-hash-table))
  (loop for v in *g-vertices* do
       (unless (gethash v *visited-vertices*)
	 (setq *current-bridge* (list v))
	 (setq *current-attachements* ())
	 (dfs-find-bridge v gr)
	 (dolist (v *current-attachements*)
	   (remhash v *visited-vertices*))
	 (push (list *current-bridge* *current-attachements*) *bridges*)))
  (loop for e in (edges gr) do
       (if (and (subsetp e *h-vertices*)
		(null (gethash e *h-edges*)))
	   (push (list e e) *bridges*))))

(defun dfs-find-bridge (v gr)
  (setf (gethash v *visited-vertices*) t)
  (loop for u in (neighbors v gr) do
       (when (null (gethash u *visited-vertices*))
	 (push u *current-bridge*)
	 (if (member u *h-vertices*)
	     (progn
	       (setf (gethash u *visited-vertices*) t)
	       (push u *current-attachements*))
	     (dfs-find-bridge u gr)))))

;;;;;;;;
;;
;; for each bridge find facial walks in which the bridge can be embedded
;;  - a bridge is a list (vertices-of-the-bridge attachements-of-the-bridge)
;;

(defvar *available-faces*)

(defun match-bridges-to-walks ()
  (setq *available-faces* (make-hash-table :test #'equal))
  (loop for b in *bridges* do
       (let ((walks ()))
	 (loop for w in *facial-walks* do
	      (if (subsetp (second b) w)
		  (push w walks)))
	 (setf (gethash b *available-faces*) walks))))

;;;;;;;
;;
;; finds a path between attachment vertices in a bridge
;;

(defvar *dfs-visited*)

(defun find-path (bridge gr)
  (when (= (length (first bridge)) 2)
    (return-from find-path (first bridge)))
  (let ((*dfs-prev* (make-hash-table))
	(*dfs-visited* (make-hash-table))
	(attachements (second bridge)))
    (dfs-find-path (first attachements) (car bridge) gr)
    ;; should not happen since graphs are biconnected!
    (if (< (length attachements) 2)
	(merror "Too few attachements"))
    (let ((u (second attachements))
	  (path ()))
      (loop while (not (null (gethash u *dfs-prev*))) do
	   (push u path)
	   (setq u (gethash u *dfs-prev*)))
      (push u path)
      path)))

(defun dfs-find-path (v bridge gr)
  (setf (gethash v *dfs-visited*) t)
  (loop for u in (neighbors v gr) do
       (unless (gethash (list v u) *h-edges*)
	 (when (and (null (gethash u *dfs-visited*))
		    (member u bridge))
	   (setf (gethash u *dfs-prev*) v)
	   (unless (member u *h-vertices*)
	     (dfs-find-path u bridge gr))))))

;;;;;;;
;;
;; ads the path to h and updates the embedding of h
;;

(defun embedd-path (path face)

  ;; add path to h
  (let ((inner (cdr path)))
    (loop while (not (null (cdr inner))) do
	 (push (car inner) *h-vertices*)
	 (setq *g-vertices* (remove (car inner) *g-vertices*))
	 (setq inner (cdr inner))))
  (let ((inner path))
    (loop while (not (null (cdr inner))) do
	 (setf (gethash (list (first inner) (second inner)) *h-edges*) t)
	 (setf (gethash (list (second inner) (first inner)) *h-edges*) t)
	 (setq inner (cdr inner))))

  ;; add vertices in the inside of the path into the embedding
  (let ((path-embedd path))
    (loop while (not (null (cddr path-embedd))) do
	 (let ((u (car path-embedd))
	       (v (cadr path-embedd))
	       (z (caddr path-embedd)))
	   (setf (gethash (list v z) *embedding*) u)
	   (setf (gethash (list v u) *embedding*) z))
	 (setq path-embedd (cdr path-embedd))))

  ;; add the first vertex into the embedding
  (let ((a (first path))
	(b (second path))
	(rface (reverse face)))
    (cond
      ((= a (first face))
       (let ((c (second face)))
	 (setf (gethash (list a c) *embedding*) b)
	 (setf (gethash (list a b) *embedding*) (first rface))))
      ((= a (first rface))
       (let ((c (second rface)))
	 (setf (gethash (list a b) *embedding*) c)
	 (setf (gethash (list a (first face)) *embedding*) b)))
      (t
       (let ((face face))
	 (loop while (not (= a (second face))) do
	      (setq face (cdr face)))
	 (setf (gethash (list a (third face)) *embedding*) b)
	 (setf (gethash (list a b) *embedding*) (first face))))))

  ;; add the second vertex into the embedding
  (setq path (reverse path))
  (let ((a (first path))
	(b (second path))
	(rface (reverse face)))
    (cond
      ((= a (first face))
       (let ((c (second face)))
	 (setf (gethash (list a c) *embedding*) b)
	 (setf (gethash (list a b) *embedding*) (first rface))))
      ((= a (first rface))
       (let ((c (second rface)))
	 (setf (gethash (list a b) *embedding*) c)
	 (setf (gethash (list a (first face)) *embedding*) b)))
      (t
       (let ((face face))
	 (loop while (not (= a (second face))) do
	      (setq face (cdr face)))
	 (setf (gethash (list a (third face)) *embedding*) b)
	 (setf (gethash (list a b) *embedding*) (first face)))))) )
     

;;;;;;;;;;;;;;;;;;;;;;
;;
;; this is the demoucron planarity test algorithm
;; - g should be 2-connected
;;

(defun demoucron (g return-walks)

  (when (> ($graph_size g) (- (* 3 ($graph_order g)) 6))
    (return-from demoucron nil))

  (let ((*h-vertices*)
	(*bridges*)
	(*h-edges* (make-hash-table :test #'equal))
	(*g-vertices* (cdr ($vertices g)))
	(*embedding* (make-hash-table :test #'equal)))

    ;; find a cycle - assumes there are no degree one vertices!
    (setq *h-vertices* (find-cycle g))
    (let ((vrt *h-vertices*))
      (loop while (not (null vrt)) do
	   (setf (gethash (list (first vrt) (second vrt)) *h-edges*) t)
	   (setf (gethash (list (second vrt) (first vrt)) *h-edges*) t)
	   (setq vrt (cdr vrt))))
    (let ((v (first *h-vertices*))
	  (u (first (reverse *h-vertices*))))
      (setf (gethash (list u v) *h-edges*) t)
      (setf (gethash (list v u) *h-edges*) t))
    (dolist (v *h-vertices*)
      (setq *g-vertices* (remove v *g-vertices*)))

    ;; embedd h
    (let ((cycle *h-vertices*))
      (loop while (not (null (cdr cycle))) do
	   (let ((u (car cycle))
		 (v (cadr cycle))
		 (z (if (null (caddr cycle)) (car *h-vertices*) (caddr cycle))))
	     (setf (gethash (list v z) *embedding*) u)
	     (setf (gethash (list v u) *embedding*) z))
	   (setq cycle (cdr cycle))))
    (let ((u (car (reverse *h-vertices*)))
	  (v (car *h-vertices*))
	  (z (cadr *h-vertices*)))
      (setf (gethash (list v z) *embedding*) u)
      (setf (gethash (list v u) *embedding*) z))

    ;; find the bridges in g-h
    (find-bridges g)

    ;; find facial walks
    (setq *facial-walks* ())
    (find-facial-walks (list (list (first *h-vertices*) (second *h-vertices*))
			     (list (second *h-vertices*) (first *h-vertices*))))

    (while (not (null *bridges*))

      (when $demoucron_debug
	(print "++++++++++++++++++")
	(print "--- facial walks:")
	(print *facial-walks*)
	(print "---      bridges:")
	(mapcar #'print *bridges*))
      ;; for each bridge find facial walks in which it can be embedded
      (match-bridges-to-walks)
      ;; select the bridge with the smallest number of available walks
      (let ((bridge (first *bridges*))
	    (path))
	;; find the bridge with the smalles number of possible facial walks
	(loop for b in *bridges* do
	     (when (< (length (gethash b *available-faces*))
		      (length (gethash bridge *available-faces*)))
	       (setq bridge b)))
	(when $demoucron_debug
	  (print "---    embedding:")
	  (print bridge))
	;; if the bridge can't be embedded, the graph is not planar
	(if (= 0 (length (gethash bridge *available-faces* bridge)))
	    (return-from demoucron nil))
	;; find a path in the bridge
	(setq path (find-path bridge g))
	;; embedd the path
	(embedd-path path (first (gethash bridge *available-faces*)))
	(when $demoucron_debug
	  (print "---         path:")
	  (print path))
	(setq *facial-walks* (remove (first (gethash bridge *available-faces*)) *facial-walks*))
	;; find the facial walks of the embedding of h
	(find-facial-walks (list (list (first path) (second path))
				 (list (second path) (first path))))
	;; find new bridges
	(find-bridges g)))

    ;; if we come here we have embedded the graph into the plane
    (if return-walks
	(cons '(mlist simp) (mapcar #'(lambda (u) (cons '(mlist simp) u)) *facial-walks*))
	t) ))

(defun is-planar-unconnected (g)
  (loop for c in (cdr ($connected_components g)) do
       (unless ($is_planar ($induced_subgraph c g))
	 (return-from is-planar-unconnected nil)))
  t)

(defmfun $planar_embedding (gr)
  (require-graph 'planar_embedding 1 gr)
  (unless ($is_biconnected gr)
    ($error "planar_embedding: the graph is not biconnected."))
  (demoucron gr t))

(defmfun $is_planar (gr)
  (require-graph 'is_planar 1 gr)
  (when (< ($graph_order gr) 5)
    (return-from $is_planar t))
  (when (> ($graph_size gr) (- (* 3 ($graph_order gr)) 6))
    (return-from $is_planar nil))
  (unless ($is_connected gr)
    (return-from $is_planar (is-planar-unconnected gr)))
  (when (< ($graph_size gr) ($graph_order gr)) ;; gr is a tree
    (return-from $is_planar t))
  (let ((bicomponents ($biconnected_components gr)))
    (loop for c in (cdr bicomponents) do
	 (if (> (length c) 4)
	     (unless (demoucron ($induced_subgraph c gr) nil)
	       (return-from $is_planar nil))))
    t))
