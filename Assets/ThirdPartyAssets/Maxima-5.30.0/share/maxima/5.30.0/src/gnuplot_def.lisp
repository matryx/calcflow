;; gnuplot.lisp: routines for Maxima's interface to gnuplot 4.0
;; Copyright (C) 2007 J. Villate
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

(defun gnuplot-color (color)
  (case color
    ($red 1) ($green 2) ($blue 3) ($magenta 4) ($cyan 5) ($black -1) (t -1)))

(defun gnuplot-colors (n)
  (let ((colors (cddr ($get_plot_option '$color))))
    (unless (integerp n) (setq n (round n)))
    (gnuplot-color (nth (mod (- n 1) (length colors)) colors))))

(defun gnuplot-pointtype (type)
  (case type
    ($bullet 7) ($circle 6) ($plus 1) ($times 2) ($asterisk 3) ($box 5)
    ($square 4) ($triangle 9) ($delta 8) ($wedge 11) ($nabla 10)
    ($diamond 13) ($lozenge 12) (t 7)))

(defun gnuplot-pointtypes (n)
  (let ((types (cddr ($get_plot_option '$point_type))))
    (unless (integerp n) (setq n (round n)))
    (gnuplot-pointtype (nth (mod (- n 1) (length types)) types))))

(defun gnuplot-curve-style (style i)
;; style is a list starting with one of the symbols: points, lines,
;; linespoints or dots,
;; The meaning of the numbers that follow the symbol are:
;;
;;   lines, linewidth, color
;;   points, radius, color, pointtype
;;   linespoints, linewidth, radius, color, pointtype
;;   dots, color
;;
;; linewidth and radius are measured in the same units and can be
;; floating-point numbers.
;;
;; color and pointtype must be an integers.
  (with-output-to-string
    (st)
    (case (first style)
      ($dots
       (format st "with dots")
       (if (integerp (second style))
         (format st " lt ~d" (gnuplot-colors (second style)))
         (format st " lt ~d" (gnuplot-colors i))))
      ($impulses
       (format st "with impulses")
       (if (integerp (second style))
         (format st " lt ~d" (gnuplot-colors (second style)))
         (format st " lt ~d" (gnuplot-colors i))))
      ($lines
       (format st "with lines")
       (if (numberp (second style))
         (format st " lw ~,2f" (second style)))
       (if (integerp (third style))
         (format st " lt ~d" (gnuplot-colors (third style)))
         (format st " lt ~d" (gnuplot-colors i))))
      ($points
       (format st "with points")
       (if (numberp (second style))
         (format st " ps ~,2f" (/ (second style) 2))
         (format st " ps 1.5"))
       (if (integerp (third style))
         (format st " lt ~d" (gnuplot-colors (third style)))
         (format st " lt ~d" (gnuplot-colors i)))
       (if (integerp (fourth style))
         (format st " pt ~d" (gnuplot-pointtypes (fourth style)))
         (format st " pt ~d" (gnuplot-pointtypes i))))
      ($linespoints
       (format st "with linespoints")
       (if (numberp (second style))
         (format st " lw ~,2f" (second style)))
       (if (numberp (third style))
         (format st " ps ~,2f" (/ (third style) 2))
         (format st " ps 1.5"))
       (if (integerp (fourth style))
         (format st " lt ~d" (gnuplot-colors (fourth style)))
         (format st " lt ~d" (gnuplot-colors i)))
       (if (integerp (fifth style))
         (format st " pt ~d" (gnuplot-pointtypes (fifth style)))
         (format st " pt ~d" (gnuplot-pointtypes i))))
      (t (format st "with lines lt ~d" (gnuplot-colors i))))))

(defun gnuplot-palette (palette)
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
    (case (length (rest palette))
      (2
       (setq gray (second palette))
       (setq range (third palette))
       (when (or (< gray 0) (> gray 1)) (setq gray (- gray (floor gray)))))
      (4
       (setq hue (second palette))
       (setq sat (third palette))
       (setq val (fourth palette))
       (setq range (fifth palette))
       (when (or (< hue 0) (> hue 1)) (setq hue (- hue (floor hue))))
       (when (or (< sat 0) (> sat 1)) (setq sat (- sat (floor sat))))
       (when (or (< val 0) (> val 1)) (setq val (- val (floor val))))))       
    (with-output-to-string
        (st)
      (case (first palette)
        ($hue
         (if (or (< (+ hue range) 0) (> (+ hue range) 1))
             (setq fun (format nil "~,3f+~,3f*gray-floor(~,3f+~,3f*gray)"
                               hue range hue range))
             (setq fun (format nil "~,3f+~,3f*gray" hue range)))
         (format st "model HSV functions ~a, ~,3f, ~,3f" fun sat val))
        ($saturation
         (if (or (< (+ sat range) 0) (> (+ sat range) 1))
             (setq fun (format nil "~,3f+~,3f*gray-floor(~,3f+~,3f*gray)"
                               sat range sat range))
             (setq fun (format nil "~,3f+~,3f*gray" sat range)))
         (format st "model HSV functions ~,3f, ~a, ~,3f" hue fun val))
        ($value
         (if (or (< (+ val range) 0) (> (+ val range) 1))
             (setq fun (format nil "~,3f+~,3f*gray" val range))
             (setq fun (format nil "~,3f+~,3f*gray-floor(~,3f+~,3f*gray)"
                               val range val range)))
         (format st "model HSV functions ~,3f, ~,3f, ~a" hue sat fun))
        ($gray
         (if (or (< (+ gray range) 0) (> (+ gray range) 1))
             (setq fun (format nil "~,3f+~,3f*gray" gray range))
             (setq fun (format nil "~,3f+~,3f*gray-floor(~,3f+~,3f*gray)"
                               gray range gray range)))
         (format st "model RGB functions ~a, ~a, ~a" fun fun fun))
        (t
         (merror
          (intl:gettext
           "palette: wrong keyword ~M. Must be hue, saturation, value or gray")
          (first palette)))))))

(defun gnuplot-print-header (dest features)
  (let ((gnuplot-out-file nil) (meshcolor '$black) (colorbox nil)
	(colors (cddr ($get_plot_option '$color)))
        preamble palette meshcolor_opt colorbox_opt)
    (setq preamble (get-plot-option-string '$gnuplot_preamble))
    (if (and ($get_plot_option '$gnuplot_preamble) (> (length preamble) 0))
      (format dest "~a~%" preamble)
      (progn
        (when (string= (getf features :type) "plot3d")
          (format dest "set ticslevel 0~%")
          (if (setq palette ($get_plot_option '$palette 2))
              (progn
                (when (setq meshcolor_opt ($get_plot_option '$mesh_lines_color))
                  (setq meshcolor (third meshcolor_opt)))
                (if meshcolor
                    (progn
                      (format dest "set style line 100 lt ~d lw 1~%"
                              (gnuplot-color meshcolor))
                      (format dest "set pm3d hidden3d 100~%")
                      (unless ($get_plot_option '$gnuplot_4_0 2)
                        (format dest "set pm3d depthorder~%")))
                    (format dest "set pm3d~%"))
                (format dest "unset hidden3d~%")
                (when (setq colorbox_opt ($get_plot_option '$colorbox))
                  (setq colorbox (third colorbox_opt)))
		(unless colorbox (format dest "unset colorbox~%"))
                (format dest "set palette ~a~%"
                        (gnuplot-palette (rest palette))))
              (format dest "set hidden3d offset ~d~%"
		      (- (gnuplot-color (nth (mod 1 (length colors)) colors))
			 (gnuplot-color (first colors)))))
          (let ((elev ($get_plot_option '$elevation))
                (azim ($get_plot_option '$azimuth)))
              (when (or elev azim)
                (if elev
                    (format dest "set view ~d" (third elev))
                    (format dest "set view "))
                (when azim (format dest ", ~d" (third azim)))
                (format dest "~%"))))))

    ;; ----- BEGIN GNUPLOT 4.0 WORK-AROUND -----
    ;; When the expression to be plotted is a constant, Gnuplot fails
    ;; with a division by 0.  Explicitly assigning cbrange prevents
    ;; the error. Also set zrange to match cbrange.
    ;; When the bug is fixed in Gnuplot (maybe 4.1 ?) this hack can go away.
    (when (floatp (getf features :const-expr))
      (format
       dest "set cbrange [~a : ~a]~%"
       (1- (getf features :const-expr)) (1+ (getf features :const-expr)))
      (format
       dest "set zrange [~a : ~a]~%"
       (1- (getf features :const-expr)) (1+ (getf features :const-expr))))
    ;; -----  END GNUPLOT 4.0 WORK-AROUND  -----
    
    (if ($get_plot_option '$gnuplot_out_file 2)
        (setf gnuplot-out-file (get-plot-option-string '$gnuplot_out_file)))
        ;; default output file name for for all formats except default
    (if (and (not (eq ($get_plot_option '$gnuplot_term 2) '$default)) 
             (null gnuplot-out-file))
        (setq gnuplot-out-file 
              (plot-temp-file (format nil "maxplot.~(~a~)" 
                                          (get-gnuplot-term ($get_plot_option '$gnuplot_term 2)))) ))
    (case ($get_plot_option '$gnuplot_term 2)
      ($default
       (format dest "~a~%" 
               (get-plot-option-string '$gnuplot_default_term_command)))
      ($ps
       (format dest "~a~%" 
               (get-plot-option-string '$gnuplot_ps_term_command))
       (if gnuplot-out-file
           (format dest "set out ~s~%" gnuplot-out-file)))
      ($dumb
       (format dest "~a~%" 
               (get-plot-option-string '$gnuplot_dumb_term_command))
       (if gnuplot-out-file
           (format dest "set out ~s~%" gnuplot-out-file)))
      (t
       (format dest "set term ~a~%" 
               (get-plot-option-string '$gnuplot_term))
       (if gnuplot-out-file
           (format dest "set out ~s~%" gnuplot-out-file))) )
    (when (getf features :log-x) (format dest "set log x~%"))
    (when (getf features :log-y) (format dest "set log y~%"))
    (when (getf features :xlabel)
      (format dest "set xlabel ~s~%" (getf features :xlabel)))
    (when (getf features :ylabel)
      (format dest "set ylabel ~s~%" (getf features :ylabel)))
    (when (getf features :zlabel)
      (format dest "set zlabel ~s~%" (getf features :zlabel)))
    (when
        (and (getf features :legend)
             (not (first (getf features :legend))))
      (format dest "unset key~%"))
    (when (and (getf features :box) (not (first (getf features :box))))
      (format dest "unset border; unset xtics; unset ytics; unset ztics~%"))
    (when (and (getf features :xmin) (getf features :xmax))
      (format dest "set xrange [~g : ~g]~%"
              (getf features :xmin) (getf features :xmax)))
    (when (and (getf features :ymin) (getf features :ymax))
      (format dest "set yrange [~g : ~g]~%"
              (getf features :ymin) (getf features :ymax)))
    (when (and (getf features :zmin) (getf features :zmax))
      (format dest "set zrange [~g : ~g]~%"
              (getf features :zmin) (getf features :zmax)))
    (when (and (string= (getf features :type) "plot2d") (getf features :axes))
      (case (getf features :axes)
        ($x (format dest "set xzeroaxis~%"))
        ($y (format dest "set yzeroaxis~%"))
        (t (format dest "set zeroaxis~%"))))
    (format dest "set datafile missing ~s~%" *missing-data-indicator*)))

(defun gnuplot-plot3d-command (file titles n) 
(let (title (style "with pm3d")
	    (palette ($get_plot_option '$palette 2))
	    (gstyles (cddr ($get_plot_option '$gnuplot_curve_styles))))
  (with-output-to-string (out)
    (format out "splot ")
  (do ((i 1 (+ i 1))) ((> i n) (format out "~%"))
    (unless palette
      (if gstyles
	  (setq style (ensure-string (nth (mod i (length gstyles)) gstyles)))
	  (setq style (format nil "with lines lt ~a" (gnuplot-colors i)))))
    (when (> i 1) (format out ", "))
    (setq title (nth (mod i (length titles)) titles))
    (format out "~s title ~s ~a" file title style)))))

