;;                 COPYRIGHT NOTICE
;;  
;;  Copyright (C) 2006-2012 Mario Rodriguez Riotorto
;;  
;;  This program is free software; you can redistribute
;;  it and/or modify it under the terms of the
;;  GNU General Public License as published by
;;  the Free Software Foundation; either version 2 
;;  of the License, or (at your option) any later version. 
;;  
;;  This program is distributed in the hope that it
;;  will be useful, but WITHOUT ANY WARRANTY;
;;  without even the implied warranty of MERCHANTABILITY
;;  or FITNESS FOR A PARTICULAR PURPOSE. See the 
;;  GNU General Public License for more details at
;;  http://www.gnu.org/copyleft/gpl.html

;;  This package handles objects returned by statistical inference
;;  procedures. Some code was copied and adapted from displa.lisp

;; For questions, suggestions, bugs and the like, feel free
;; to contact me at
;; mario @@@ edu DOT xunta DOT es

(in-package :maxima)

;; Constructs the 'inference_result' object, with
;;    title: string with the name of the inference procedure
;;    val: Maxima list, the elements of which are lists of
;;         the form: ["Value's name", value]
;;    selection: Maxima list numbering the values to be displayed
(defun $inference_result (title val selection)
  (list '($inference_result) title val selection))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                              ;;
;;    DISPLAY PROPERTIES FOR inference_result   ;;
;;                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Functions d-matrixz, matoutz, coloutz and dim-$inference_result are similar to
;; d-matrix, matout, colout and dim-$matrix, respectively, from displa.lisp.
;; The left and right characters ($lmxchar and $rmxchar in displa.lisp) are 
;; defined without additional global variables.


(defun d-matrixz (linear? direction h d)
  (declare (fixnum h d))
  (d-vbar linear? h d 
	  (getcharn (if (eq direction 'right) " " "|" ) 1)))  ; <- right and left characters


(defun matoutz (dmstr cstr rstr result)
  (push `(d-matrixz left ,height ,depth) result)
  (push #\space result)
  (do ((d dmstr (cdr d)) (c cstr (cdr c)) (w 0 0)) ((null d))
    (declare (fixnum w))
    (do ((d (car d) (cdr d)) (r rstr (cdr r))) ((null d))
      (rplaca (cddar d) (f- height (car r)))
      (rplaca (cdar d) (f- (truncate (f- (car c) (caar d)) 2) w))
      (setq w (truncate (f+ (car c) (caar d)) 2))
      (rplaca d (cdar d)))
    (setq result (cons (list (f+ 2 (f- (car c) w)) 0) (nreconc (car d) result))))
  (setq width (f+ 2 width))
  (update-heights height depth)
  (rplaca (car result) (f1- (caar result)))
  (push `(d-matrixz right ,height ,depth) result)
  result)

(defun coloutz (dmstr cstr result)
  (setq width 0 height 1 depth 0)
  (do ((r dmstr (cdr r)) (c cstr (cdr c)) (col 1 (f1+ col)) (w 0 0) (h -1 -1) (d 0))
      ((null r))
    (declare (fixnum col w h d))
    (push-string " Col " result)
    (setq result (nreconc (exploden col) result))
    (push-string " = " result)
    (setq width (f+ 8 (flatc col) width))
    (do ((r (car r) (cdr r))) ((null r))
      (setq h (f+ 1 h (cadar r) (caddar r)))
      (rplaca (cddar r) (f- h (cadar r)))
      (rplaca (cdar r) (f- (truncate (f- (car c) (caar r)) 2) w))
      (setq w (truncate (f+ (car c) (caar r)) 2))
      (rplaca r (cdar r)))
    (setq d (truncate h 2) h (f- h d))
    (push `(d-matrixz left ,h ,d) result)
    (push #\space result)
    (push `(0 ,(f- d) . ,(nreverse (car r))) result)
    (push `(,(f1+ (f- (car c) w)) 0) result)
    (push `(d-matrixz right ,h ,d) result)
    (setq width (f+ 4 (car c) width) height (max h height) depth (max d depth))
    (update-heights h d)
    (checkbreak result width))
  result)

(displa-def $inference_result dim-$inference_result)

(defun dim-$inference_result (form result)
  (declare (special linearray))
  (prog (dmstr rstr cstr consp)
     (if (or (null (cdr form))
	     (not (member 'simp (cdar form) :test #'eq))
	     (memalike '((mlist simp)) (cdr form))
	     (dolist (row (cdr form)) (if (not ($listp row)) (return t))))
	 (return (dimension-function form result)))
     (do ((l (cdadr form) (cdr l))) ((null l))
       (setq dmstr (cons nil dmstr) cstr (cons 0 cstr)))
     (do ((r (cdr form) (cdr r)) (h1 0) (d1 0))
	 ((or consp (null r))
	  (setq width 0)
	  (do ((cs cstr (cdr cs))) ((null cs)) (setq width (f+ 2 (car cs) width)))
	  (setq h1 (f1- (f+ h1 d1)) depth (truncate h1 2) height (f- h1 depth)))
       (declare (fixnum h1 d1))
       (do ((c (cdar r) (cdr c))
	    (nc dmstr (cdr nc))
	    (cs cstr (cdr cs)) (dummy) (h2 0) (d2 0))
	   ((null c) (setq d1 (f+ d1 h1 h2) h1 (f1+ d2)))
	 (declare (fixnum h2 d2))
	 (setq dummy (dimension (car c) nil 'mparen 'mparen nil 0)
	       h2 (max h2 height) d2 (max d2 depth))
	 (cond ((not (checkfit (f+ 14. width))) (setq consp t) (return nil))
	       (t (rplaca nc (cons (list* width height depth dummy) (car nc)))
		  (rplaca cs (max width (car cs))))))
       (setq rstr (cons d1 rstr)))
     (when (> (+ height depth) (length linearray))
       (setq consp t))
     (return
       (cond ((and (not consp) (checkfit (f+ 2 width)))
	      (matoutz dmstr cstr rstr result))
	     ((and (not consp) (<= level 2)) (coloutz dmstr cstr result))
	     (t (dimension-function form result))))))


;; Sets display properties
(displa-def $inference_result dimension-inference)

(defun dimension-inference (form result)
  (let ((title (cadr form))
	(outputitems (reverse (cdr (cadddr form))))
         (output nil) aux)
    (dolist (k outputitems 'done)
      (setf aux (rest (nth k (caddr form))))
      (push (list '(mlist simp) (list '(mequal simp) (car aux) (cadr aux))) 
	    output))
   ; variable output has the following structure:
   ; '(($inference_result simp)
   ;      ((mlist simp) ,title)
   ;      ((mlist) ((mequal simp) value_name1 value1))
   ;      ((mlist) ((mequal simp) value_name2 value2))
   ;      ((mlist) ((mequal simp) value_name3 value3)))
   (setf output (append (list '($inference_result simp) (list '(mlist simp) title)) output))
   (dim-$inference_result output result)))


;; Format TeX output
(defprop $inference_result tex-inference_result tex)

(defun tex-inference_result (x l r)
  ;; inference_result looks like 
  ;; ((inference_result) string ((mlist) ((mlist)..) ((mlist)..)..))
  (append l `("\\left | \\matrix{" )
          (list "\\hbox{" (cadr x) "} \\cr \\matrix{")
	  (mapcan #'(lambda(y)
		      (tex-list `(((mequal) ,(cadr y) ,(caddr y))) nil (list "\\cr ") "&"))
		  (cdar (butlast (cddr x))))
	  '("}} \\right .") r))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  ;;
;;  FUNCTIONS FOR inference_result  ;;
;;                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Returns true or false, depending on whether 'obj' is an
;; 'inference_result' object or not.
(defun $inferencep (obj)
  (and (listp obj)
       (equalp (car obj) '($inference_result simp)) ))


;;  Returns a Maxima list with the names
;;  of the items stored in the 'inference_result' object
(defun $items_inference (obj)
  (let ((items (cdaddr obj)))
    (cons '(mlist) (mapcar #'second items))))


;;  Returns the n-th value of the 'inference_result' object,
;;  or the list of values associated to the indices in n, 
;;  if n is a Maxima list.
(defun $take_inference (n obj)
  (if ($inferencep obj)
      (cond ((and ($integerp n) (<= n (length (cdaddr obj))))
              (caddr (nth (- n 1) (cdaddr obj))))
            (($listp n) 
              (let ((values nil)
                    (items (reverse (rest n))))
                 (dolist (k items (cons '(mlist) values))
                    (setf values (cons ($take_inference k obj) values)))) )
            (t
              (let ((m (position n (mapcar #'second (cdaddr obj)))))
                 (if (equal m nil)
                     (merror "Wrong label in 'take_inference' call")
                     (caddr (nth m (cdaddr obj)))))))
      (merror "Wrong object in 'take_inference' call")  ))


;;  Returns the title of the 'inference_result' object
(defun $title_inference (obj)
  (if ($inferencep obj)
    (second obj)
    (merror "Wrong object in 'title_inference' call")) )

