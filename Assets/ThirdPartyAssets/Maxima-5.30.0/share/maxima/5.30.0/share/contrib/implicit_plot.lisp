;;; -*-  Mode: Lisp -*-                                                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;  Author:  Andrej Vodopivec <andrejv@users.sourceforge.net>                ;;
;;  Licence: GPL                                                             ;;
;;                                                                           ;;
;;  Usage:                                                                   ;;
;;   implicit_plot(expr, xrange, yrange, [options]);                         ;;
;;      Plots the curve `expr' in the region given by `xrange' and `yrange'. ;;
;;   `expr' is a curve in plane defined in implicit form or a list of such   ;;
;;   curves. If `expr' is not an equality, then `expr=0' is assumed. Works   ;;
;;   by tracking sign changes, so it will fail if expr is something like     ;;
;;   `(y-x)^2=0'.                                                            ;;
;;      Optional argument `options' can be anything that is recognized by    ;;
;;   `plot2d'. Options can also be set using `set_plot_option'.              ;;
;;                                                                           ;;
;;  Examples:                                                                ;;
;;   implicit_plot(y^2=x^3-2*x+1, [x,-4,4], [y,-4,4],                        ;;
;;                 [gnuplot_preamble, "set zeroaxis"])$                      ;;
;;   implicit_plot([x^2-y^2/9=1,x^2/4+y^2/9=1], [x,-2.5,2.5], [y,-3.5,3.5]); ;;
;;   implicit_plot(x^2+2*y^3=15, [x,-10, 10], [y,-5,5])$                     ;;
;;   implicit_plot(x^2*y^2=(y+1)^2*(4-y^2), [x,-10, 10], [y,-3,3]);          ;;
;;   implicit_plot(x^3+y^3 = 3*x*y^2-x-1, [x,-4,4], [y,-4,4]);               ;;
;;   implicit_plot(x^2*sin(x+y)+y^2*cos(x-y)=1, [x,-10,10], [y,-10,10]);     ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module implicit_plot)

(defmvar $ip_grid `((mlist simp)  50  50)
  "Grid for the first sampling.")
(defmvar $ip_grid_in `((mlist simp)  5  5)
  "Grid for the second sampling.")

(defmvar $ip_epsilon  0.0000001
  "Epsilon for implicit plot routine.")

(defun contains-zeros (i j sample)
  (not (and (> (* (aref sample i j) (aref sample (1+ i)     j  )) 0)
            (> (* (aref sample i j) (aref sample     i  (1+ j) )) 0)
            (> (* (aref sample i j) (aref sample (1+ i) (1+ j) )) 0) )))

(defun sample-data (expr xmin xmax ymin ymax sample grid)
  (let* ((xdelta (/ (- xmax xmin) ($first grid)))
         (ydelta (/ (- ymax ymin) ($second grid))))
    (do ((x-val xmin (+ x-val xdelta))
         (i 0 (1+ i)))
        ((> i ($first grid)))
      (do ((y-val ymin (+ y-val ydelta))
           (j 0 (1+ j)))
          ((> j ($second grid)))
        (let ((fun-val (funcall expr x-val y-val)))
          (if (or (eq fun-val t) (>= fun-val $ip_epsilon))
              (setf (aref sample i j) 1)
              (setf (aref sample i j) -1)))))))

(defvar ip-gnuplot nil)

(defun print-segment (file points xmin xdelta ymin ydelta)
  (let* ((point1 (car points)) (point2 (cadr points))
         (x1 (+ xmin (/ (* xdelta (+ (car point1) (caddr point1))) 2)))
         (y1 (+ ymin (/ (* ydelta (+ (cadr point1) (cadddr point1))) 2)))
         (x2 (+ xmin (/ (* xdelta (+ (car point2) (caddr point2))) 2)))
         (y2 (+ ymin (/ (* ydelta (+ (cadr point2) (cadddr point2))) 2))))
    (if ip-gnuplot
        (progn
          (format file "~f ~f~%" x1 y1)
          (format file "~f ~f~%~%" x2 y2))
        (progn
          (format file "  { ~f ~f ~f }~%" (float x1) (float (/ (+ x1 x2) 2)) (float x2))
          (format file "  { ~f ~f ~f }~%" (float y1) (float (/ (+ y1 y2) 2)) (float y2)))) ))
        

(defun print-square (file xmin xmax ymin ymax sample grid)
  (let* ((xdelta (/ (- xmax xmin) ($first grid)))
         (ydelta (/ (- ymax ymin) ($second grid))))
    (do ((i 0 (1+ i)))
        ((= i ($first grid)))
      (do ((j 0 (1+ j)))
          ((= j ($second grid)))
        (if (contains-zeros i j sample)
            (let ((points ()))
              (if (< (* (aref sample i j) (aref sample (1+ i) j)) 0)
                  (setq points (cons `(,i ,j ,(1+ i) ,j) points)))
              (if (< (* (aref sample (1+ i) j) (aref sample (1+ i) (1+ j))) 0)
                  (setq points (cons `(,(1+ i) ,j ,(1+ i) ,(1+ j)) points)))
              (if (< (* (aref sample i (1+ j)) (aref sample (1+ i) (1+ j))) 0)
                  (setq points (cons `(,i ,(1+ j) ,(1+ i) ,(1+ j)) points)))
              (if (< (* (aref sample i j) (aref sample i (1+ j))) 0)
                  (setq points (cons `(,i ,j ,i ,(1+ j)) points)))
              (print-segment file points xmin xdelta ymin ydelta)) )))))

(defun imp-pl-prepare-factor (expr)
  (cond 
    ((or ($numberp expr) (atom expr))
     expr)
    ((eq (caar expr) 'mexpt)
     (cadr expr))
    (t
     expr)))

(defun imp-pl-prepare-expr (expr)
  (let ((expr1 ($factor (m- ($rhs expr) ($lhs expr)))))
    (cond ((or ($numberp expr) (atom expr1)) expr1)
          ((eq (caar expr1) 'mtimes)
           `((mtimes simp factored 1)
             ,@(mapcar #'imp-pl-prepare-factor (cdr expr1))))
          ((eq (caar expr) 'mexpt)
           (imp-pl-prepare-factor expr1))
          (t
           expr1))))

(defun $implicit_plot (expr xrange yrange &rest options)
  (let* (($numer t) ($plot_options $plot_options)
         (plot-name)
         (i 0)
         (xmin ($second xrange))
         (xmax ($third xrange))
         (xdelta (/ (- xmax xmin) ($first $ip_grid)))
         (ymin ($second yrange))
         (ymax ($third yrange))
         (ydelta (/ (- ymax ymin) ($second $ip_grid)))
         (sample (make-array `(,(1+ ($first $ip_grid))
                               ,(1+ ($second $ip_grid)))))
         (ssample (make-array `(,(1+ ($first $ip_grid_in))
                                ,(1+ ($second $ip_grid_in)))))
         file-name gnuplot-out-file gnuplot-term features
         (xmaxima-titles ()))
    
    ;; Parse the given options into the list features
    (setf (getf features :type) "plot2d")
    (setq features (plot-options-parser options features))
    (setq xrange (check-range xrange))
    (setq yrange (check-range yrange))
    (setf (getf features :xmin) (third xrange))
    (setf (getf features :xmax) (fourth xrange))
    (setf (getf features :ymin) (third yrange))
    (setf (getf features :ymax) (fourth yrange))
    (unless (getf features :xlabel)
      (setf (getf features :xlabel) (ensure-string (second xrange))))
    (unless (getf features :ylabel)
      (setf (getf features :ylabel) (ensure-string (second yrange))))

;    ($set_plot_option '((mlist simp) $gnuplot_pm3d nil))
    
    (if (not ($listp expr))
        (setq expr `((mlist simp) ,expr)))

    (setf gnuplot-term ($get_plot_option '$gnuplot_term 2))
    
    (if (eq (getf features :plot-format) '$xmaxima)
        (setq ip-gnuplot nil)
        (setq ip-gnuplot t))

    (if  ip-gnuplot
         (if (and (eq gnuplot-term '$default)
                  gnuplot-out-file)
             (setf file-name gnuplot-out-file)
             (setf file-name (plot-temp-file "maxout.gnuplot")))
         (setf file-name (plot-temp-file "maxout.xmaxima")))
    
    ;; output data
    (with-open-file
        (file file-name :direction :output :if-exists :supersede)
      (if ip-gnuplot
          (progn
            (gnuplot-print-header file features)
            (format file "set style data lines~%")
            (format file "plot"))
            (xmaxima-print-header file features))
      (let ((legend (getf features :legend))
            (styles (getf features :styles)))
        (dolist (v (cdr expr))
          (incf i)
          (setq plot-name nil)
          (if legend        ; legend in the command line has priority
              (setq plot-name
                    (if (first legend)
                        (ensure-string
                         (nth (mod (- i 1) (length legend)) legend))
                        nil)) ; no legend if option [legend,false]
              (unless (= 2 (length expr)) ;no legend for a single function
                (setq plot-name
                      (let ((string ""))
                        (if (atom v) 
                            (setf string (coerce (mstring v) 'string))
                            (setf string (coerce (mstring v) 'string)))
                        (if (< (length string) 80)
                            string
                            (format nil "fun~a" i))))))
          (if ip-gnuplot
              (progn
                (if (> i 1)
                    (format file ","))
                (let ((title (get-plot-option-string '$gnuplot_curve_titles i))
                      (style (get-plot-option-string '$gnuplot_curve_styles i)))
                  (when (or (equal title "false") (equal title "default"))
                    (if plot-name
                        (setq title (format nil " title \"~a\"" plot-name))
                        (setq title " notitle")))
                  (when (equal style "false")
                    (if styles
                        (progn
                          (setq style (nth (mod i (length styles)) styles))
                          (setq style (if ($listp style) (cdr style) `(,style))))
                        (setq style nil))
                    (setq style (gnuplot-curve-style style i)))
                  (format file " '-' ~a ~a" title style)))
              (progn
                (let (title style)
                  (when plot-name
                    (setq title (format nil " {label \"~a\"}" plot-name)))
                  (if styles
                      (progn
                        (setq style (nth (mod i (length styles)) styles))
                        (setq style (if ($listp style) (cdr style) `(,style))))
                      (setq style nil))
                  (setq style (xmaxima-curve-style style i))
                  (setq xmaxima-titles (cons (format nil "~a ~a~%" title style)
                                             xmaxima-titles)))))))
      (format file "~%")
      (dolist (e (cdr expr))
        (unless ip-gnuplot
          (progn
            (format file (pop xmaxima-titles))
            (format file " {xversusy~%")))
        (setq e (coerce-float-fun (if (atom e) e ($float (imp-pl-prepare-expr e)))
                                  `((mlist simp)
                                    ,($first xrange)
                                    ,($first yrange))))
        (sample-data e xmin xmax ymin ymax sample $ip_grid)
        (do ((i 0 (1+ i)))
            ((= i ($first $ip_grid)))
          (do ((j 0 (1+ j)))
              ((= j ($second $ip_grid)))
            (if (contains-zeros i j sample)
                (let* ((xxmin (+ xmin (* i xdelta)))
                       (xxmax (+ xxmin xdelta))
                       (yymin (+ ymin (* j ydelta)))
                       (yymax (+ yymin ydelta)))
                  (sample-data e xxmin xxmax yymin yymax
                               ssample $ip_grid_in)
                  (print-square file xxmin xxmax yymin yymax
                                ssample $ip_grid_in) )) ))
        (if ip-gnuplot
            (format file "e~%")
            (format file "}~%")) )
      (unless ip-gnuplot
        (format file "}~%")))
    
    ;; call plotter
    (if ip-gnuplot
        (gnuplot-process file-name)
        ($system (concatenate 'string *maxima-prefix*
                              "/bin/" $xmaxima_plot_command)
                 (format nil " \"~a\"" (plot-temp-file "maxout.xmaxima")))))
    '$done)
