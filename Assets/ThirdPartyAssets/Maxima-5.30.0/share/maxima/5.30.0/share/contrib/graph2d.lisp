;; graph2d.lisp - Adds a function graph2d() to Maxima, which is a
;;                replacement for openplot_curves, with a simpler syntax.
;;   
;; Copyright (C) 2005 Jaime E. Villate <villate@gnu.org>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.
;;
;; See graph2d.usg (which should come together with this program) for
;; a usage summary, and graph2d.dem for examples.
;;
;; $Id: graph2d.lisp,v 1.3 2009-09-13 01:50:03 robert_dodier Exp $

(in-package :maxima)

;; default graph2d options
(defvar $graph2d_options '((mlist)
			  ;; Width in x direction of the x values
			  ((mlist) $xradius 10)
			  ;; Height in y direction of the y values
			  ((mlist) $yradius 10)
			  ;; Width of canvas in pixels
			  ((mlist) $width 500)
			  ;; Height of canvas in pixels
			  ((mlist) $height 500)
			  ;; (xcenter,ycenter) is the origin of the window
			  ((mlist) $xcenter 0)
			  ((mlist) $ycenter 0)
			  ;; A semi colon separated list of functions to plot
			  ((mlist) $xfun "")
			  ;; xmin ymin xmax ymax .. overrides the -xcenter etc
			  ((mlist) $bbox -10 -10 10 10)
			  ;; if not 0 plot the points at pointsize
			  ((mlist) $plotpoints 0)
			  ;; If not 0, plot points and nolines
			  ((mlist) $nolines 0)
			  ;; Width of plot lines
			  ((mlist) $linewidth 0.6)
			  ;; radius in pixels of points
			  ((mlist) $pointsize 2)
			  ;; colors to use for lines in data plots
			  ((mlist) $linecolors
			   "blue green red brown gray black")
			  ;; Position for the curve labels nw corner
			  ((mlist) $labelposition "10 35")
			  ;; Label for the x axis
			  ((mlist) $xaxislabel "")
			  ;; Label for the y axis
			  ((mlist) $yaxislabel "")
			  ;; Label for the legend
			  ((mlist) $label "")
			  ;; Line/point's color
			  ((mlist) $color "")
			  ;; Set {x,y}center and {x,y}range depending on data
			  ;; and function. Value of y means autoscale in y
			  ;; direction, value of {x y} means scale in both.
			  ;; Supplying data will automatically turn this on.
			  ((mlist) $autoscale "y")
			  ;; Factor to zoom the x and y axis when zooming.
			  ;; Zoom out will be reciprocal
			  ((mlist) $zoomfactor "1.6 1.6")
			  ;; If not 0 width in pixels of errorbar. Two y values
			  ;; supplied for each x: {y1low y1high y2low y2high
			  ;; .. }"}
			  ((mlist) $errorbar 0)
			  ;; If not 0, the width of the bars on a bar graph
			  ((mlist) $bargraph 0)
			  ;; List of parameters and values eg k=3,l=7+k
			  ((mlist) $parameters "")
			  ;; List of parameters ranges k=3:5,u
			  ((mlist) $sliders "")
			  ))

;; gets the value of a graph2d option
(defun $get_graph2d_option (name &optional n)
  (loop for v in (rest $graph2d_options)
	 when (eq (second v) name) do
	 (return (if n (nth n  v) v))))

;; parses a graph2d option into a tcl string
(defun tcl-get-graph2d-option (name)
  (let (vv)
    (with-output-to-string (st)
			   (loop for v in (rest $graph2d_options)
				  when (eq (second v) name)
				  do (setq vv (mapcar #'stripdollar (rest v)))
				  (format st "{~a {~{~(~a~)~^ ~}}}"
					  (first vv) (rest vv))))))

;; changes the value of a graph2d option
(defun $set_graph2d_option ( value)
  (setq $graph2d_options ($copylist $graph2d_options))
  (unless (and  ($listp value)
		(symbolp (setq name (second value))))
    (merror "~M is not a graph2d option.  Must be [symbol,..data]" value))
  (setq value
	(case name
	      ($xradius (check-list-items name (rest (rest value)) 'number 1))
	      ($yradius (check-list-items name (rest (rest value)) 'number 1))
	      ($width (check-list-items name (rest (rest value)) 'fixnum 1))
	      ($height (check-list-items name (rest (rest value)) 'fixnum 1))
	      ($xcenter (check-list-items name (rest (rest value)) 'number 1))
	      ($ycenter (check-list-items name (rest (rest value)) 'number 1))
	      ($bbox (check-list-items name (rest (rest value)) 'number 4))
	      ($plotpoints (check-list-items name (rest (rest value)) 'number 1))
	      ($nolines (check-list-items name (rest (rest value)) 'fixnum 1))
	      ($linewidth (check-list-items name (rest (rest value)) 'number 1))
	      ($pointsize (check-list-items name (rest (rest value)) 'number 1))
	      ($linecolors value)
	      ($labelposition value)
	      ($xaxislabel value)
	      ($yaxislabel value)
	      ($label value)
	      ($color value)
	      ($autoscale value)
	      ($zoomfactor value)
	      ($errorbar (check-list-items name (rest (rest value)) 'fixnum 1))
	      ($bargraph (check-list-items name (rest (rest value)) 'number 1))
	      ($parameters value)
	      ($sliders value)
	      ))
  (loop for v on (rest $graph2d_options)
	 when (eq (second (first v)) name)
	 do (setf (first v) value))
  $graph2d_options
  )
 
;; plots one or more sets of points [[x1,y1],[x2,y2],....] or
;; [x1, y1, x2, y2, ...]
;;
(defun $graph2d(&rest lis)
  (let (l xdata (points 0))
    (setq lis (cons '(mlist) lis))
    (show-open-plot
     (with-output-to-string
       (st )
       (cond ($show_openplot (format st "plot2d -data {~%"))
	     (t (format st "{plot2d ~%")))
       (or (and ($listp lis) ($listp (second lis)))
	   (merror "Needs a list of points: [x1,x2,...],[y1,y2,...] or [[x1,y1],[x2,y2],...]"))
    
       (loop for v in (rest lis)
	      do
	      (or ($listp v) (merror "the dataset should be a list"))
	      (setq l v)
	      (setq v (rest v))
	      (format st "")
	      (if (and (first v) (symbolp (first v)))
		  (progn
		    (if (= points 2)
			(merror "Data should be [x1,...,xn], [y1,...,yn]"))
		    ($set_graph2d_option l)
		    (format st " ~a " (tcl-get-graph2d-option (first v))))
	     (progn
	       (or (= points 2) (format st "~%{ xversusy  "))
	       (loop with (this xvals yvals)
		      while v
		      do
		      (cond   ((numberp (car v))
			       (if (= points 2) 
				   (progn
				     (if (= (length xdata) (length v))
					 (progn
					   (setq xvals (nreverse xdata))
					   (setq yvals (nreverse v)))
				       (merror "Data should be [x1,...,xn], [y1,...,yn]"))
				     (setq v nil) (setq points 1))
				 (progn
				   (setq xdata v) (setq v nil)
				   (setq points 2))))
			      (($listp (first v))
			       (if (= points 2)
				   (merror "Data should be [x1,...,xn], [y1,...,yn]"))
			       (setq points 1)
			       (setq this (cdar v))
			       (push (car this) xvals)
			       (push (second this) yvals)
			       (and (third this) (push (third this ) yvals))
			       (setq v (cdr v)))
			      (t (princ (list 'bad (car v))) (setq v (cdr v))))
		      
		      finally
		      (or (= points 2) (progn
					 (tcl-output-list st (nreverse xvals))
					 (tcl-output-list st (nreverse yvals)))))
	       (or (= points 2) (format st " }"))
	       )))
       (format st "}~%")
       (or (= points 1) (merror "No data or wrong data format"))
    ))))
