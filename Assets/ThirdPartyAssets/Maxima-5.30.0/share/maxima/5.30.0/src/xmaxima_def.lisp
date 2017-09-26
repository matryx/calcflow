;; xmaxima.lisp: routines for Maxima's interface to xmaxima
;; Copyright (C) 2007,2009 J. Villate
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA  02110-1301, USA

(in-package :maxima)

(defun xmaxima-color (color)
  (if (and (stringp color)(string= (subseq color 0 1) "#")(= (length color) 7))
      color
      (case color
	($red "#ff0000") ($green "#00ff00") ($blue "#0000ff")
	($magenta "#ff00ff") ($cyan "#00ffff") ($black "#000000")
	(t "#000000"))))

(defun xmaxima-colors (n)
  (let ((colors (cddr ($get_plot_option '$color))))
    (unless (integerp n) (setq n (round n)))
    (xmaxima-color (nth (mod (- n 1) (length colors)) colors))))

(defun xmaxima-curve-style (style i)
;; style is a list starting with a symbol from the list: points, lines,
;; linespoints or dots,
;; The meaning of the numbers that follow the symbol are:
;;
;;   lines, linewidth, color
;;   points, radius, color
;;   linespoints, linewidth, radius, color
;;   dots, color
;;
;; linewidth and radius are measured in the same units and can be
;; floating-point numbers.
;;
;; color can be given in RGB format as, for instance #f102d2 or as one of
;; the following colors: red, green, blue, magenta, cyan or black.
  (with-output-to-string
    (st)
    (case (first style)
      ($dots
       (format st "\{ nolines 1 \} \{ plotpoints 1 \} \{ pointsize 0.7 \}")
       (if (integerp (second style))
	 (format st " \{ color ~a \}" (xmaxima-colors (second style)))
         (format st " \{ color ~a \}" (xmaxima-colors i))))
      ($lines
       (format st "\{ nolines 0 \} \{ plotpoints 0 \}")
       (if (numberp (second style))
	 (format st " \{ linewidth ~,2f \}" (second style)))
       (if (integerp (third style))
	 (format st " \{ color ~a \}" (xmaxima-colors (third style)))
	 (format st " \{ color ~a \}" (xmaxima-colors i))))
      ($points
       (format st "\{ nolines 1 \} \{ plotpoints 1 \}")
       (if (numberp (second style))
	 (format st " \{ pointsize ~,2f \}" (second style))
	 (format st " \{ pointsize 3 \}"))
       (if (integerp (third style))
	 (format st " \{ color ~a \}" (xmaxima-colors (third style)))
	 (format st " \{ color ~a \}" (xmaxima-colors i))))
      ($linespoints
       (format st "\{ nolines 0 \} \{ plotpoints 1 \}")
       (if (numberp (second style))
	 (format st " \{ linewidth ~,2f \}" (second style)))
       (if (numberp (third style))
	 (format st " \{ pointsize ~,2f \}" (third style))
	 (format st " \{ pointsize 3 \}"))
       (if (integerp (fourth style))
	 (format st " \{ color ~a \}" (xmaxima-colors (fourth style)))
	 (format st " \{ color ~a \}" (xmaxima-colors i))))
      (t
       (format st "\{ nolines 0 \} \{ plotpoints 0 \} \{ color ~a \}" (xmaxima-colors i))))))

(defun xmaxima-palette (palette)
;; palette should be a list starting with one of the symbols: hue,
;; saturation, value or gray.
;;
;; If the symbol is gray, it should be followed by two floating point
;; numbers that indicate the initial gray level and the interval of 
;; gray values.
;;
;; If the symbol is one of hue, saturation or value, it must be followed
;; by three numbers that specify the hue, saturation and value for the
;; initial color, and a fourth number that gives the range of values for
;; the increment of hue, saturation or value.
;; 
;; The values for the initial hue, saturation, value and grayness should
;; be within 0 and 1, while the range can be higher or even negative. 
  (let (hue sat val gray range fun)
    (with-output-to-string
        (st)
      (case (length (rest palette))
	(2
	 (setq gray (second palette))
	 (setq range (third palette))
	 (when (or (< gray 0) (> gray 1)) (setq gray (- gray (floor gray))))
	 (format st "\{ value ~,3f \} \{ colorrange ~,3f \}" gray range))
	(4
	 (setq hue (second palette))
	 (setq sat (third palette))
	 (setq val (fourth palette))
	 (setq range (fifth palette))
	 (when (or (< hue 0) (> hue 1)) (setq hue (- hue (floor hue))))
	 (when (or (< sat 0) (> sat 1)) (setq sat (- sat (floor sat))))
	 (when (or (< val 0) (> val 1)) (setq val (- val (floor val))))       
	 (format st
		 " \{ hue ~,3f \} \{ saturation ~,3f \} \{ value ~,3f \} \{ colorrange ~,3f \}"
		 hue sat val range)))
      (case (first palette)
        ($hue (format st " \{ colorscheme hue \}"))
        ($saturation (format st " \{ colorscheme saturation \}"))
        ($value (format st " \{ colorscheme value \}"))
        ($gray (format st " \{ colorscheme gray \}"))
        (t
         (merror
          (intl:gettext
           "palette: no such keyword ~M. Must be hue, saturation, value or gray")
          (first palette)))))))

(defun xmaxima-palletes (n &aux (palettes (cddr ($get_plot_option '$palette))))
  (unless (integerp n) (setq n (round n)))
    (xmaxima-palette (rest (nth (mod (- n 1) (length palettes)) palettes))))
			 
(defun output-points-tcl (dest pl m i
			  &aux (palette ($get_plot_option '$palette 2)))
  (if palette
      (format dest " ~a~%" (xmaxima-palletes i))
      (format dest " {mesh_lines ~a}" (xmaxima-colors i)))
  (format dest " {matrix_mesh ~%")
  ;; we do the x y z  separately:
  (loop for off from 0 to 2
     with ar = (polygon-pts pl)
     with  i of-type fixnum = 0
     do (setq i off)
       (format dest "~%{")
       (loop 
	  while (< i (length ar))
	  do (format dest "~% {")
	    (loop for j to m
	       do (print-pt (aref ar i))
		 (setq i (+ i 3)))
	    (format dest "}~%"))
       (format dest "}~%"))
  (format dest "}~%"))

(defun xmaxima-print-header (dest features)
  (cond ($show_openplot (format dest "~a -data {~%" (getf features :type)))
	(t (format dest "{~a " (getf features :type))))
  (when (string= (getf features :type) "plot3d")
    (let ((meshcolor '$black) (elev ($get_plot_option '$elevation))
	  (azim ($get_plot_option '$azimuth)) palette meshcolor_opt)
      (if (setq palette ($get_plot_option '$palette 2))
	  (progn
	    (when (setq meshcolor_opt ($get_plot_option '$mesh_lines_color))
	      (setq meshcolor (third meshcolor_opt)))
	    (if meshcolor
		(format dest " {mesh_lines ~a}" (xmaxima-color meshcolor))
		(format dest " {mesh_lines 0}")))
	  (format dest " {colorscheme 0}~%"))
      (when elev (format dest " {el ~d}" (third elev)))
      (when azim (format dest " {az ~d}" (third azim)))
      (format dest "~%")))

  (when (getf features :psfile)
    (format dest " {psfile ~s}" (getf features :psfile)))
  (when
      (and (getf features :legend)(not (first (getf features :legend))))
    (format dest " {nolegend 1}"))
  (when (and (getf features :box) (not (first (getf features :box))))
    (format dest " {nobox 1}"))
  (if (getf features :axes)
      (case (getf features :axes)
	($x (format dest " {axes {x} }"))
	($y (format dest " {axes {y} }"))
	(t (format dest " {axes {xy} }")))
      (format dest " {axes 0}"))
  (when (and (getf features :xmin) (getf features :xmax))
    (format dest " {xrange ~g ~g}"
	    (getf features :xmin) (getf features :xmax)))
  (when (and (getf features :ymin) (getf features :ymax))
    (format dest " {yrange ~g ~g}"
	    (getf features :ymin) (getf features :ymax)))
  (when (getf features :xlabel)
    (format dest " {xaxislabel ~s}" (getf features :xlabel)))
  (when (getf features :ylabel)
    (format dest " {yaxislabel ~s}" (getf features :ylabel)))
  (when (and (getf features :zmin) (getf features :zmax))
    (format $pstream " {zcenter ~g }"
	    (/ (+ (getf features :zmax) (getf features :zmin)) 2))
    (format $pstream " {zradius ~g }~%"
	    (/ (- (getf features :zmax) (getf features :zmin)) 2)))
  (format dest "~%"))

