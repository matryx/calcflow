;;Copyright William F. Schelter 1990, All Rights Reserved

(in-package :maxima)

#|
Examples

/* plot of z^(1/3)...*/
plot3d(r^.33*cos(th/3),[r,0,1],[th,0,6*%pi],['grid,12,80],['transform_xy,polar_to_xy],['plot_format,geomview]) ;

/* plot of z^(1/2)...*/
plot3d(r^.5*cos(th/2),[r,0,1],[th,0,6*%pi],['grid,12,80],['transform_xy,polar_to_xy],['plot_format,xmaxima]) ;

/* moebius */
plot3d([cos(x)*(3+y*cos(x/2)),sin(x)*(3+y*cos(x/2)),y*sin(x/2)],[x,-%pi,%pi],[y,-1,1],['grid,50,15]) ;

/* klein bottle */
plot3d([5*cos(x)*(cos(x/2)*cos(y)+sin(x/2)*sin(2*y)+3.0) - 10.0,
-5*sin(x)*(cos(x/2)*cos(y)+sin(x/2)*sin(2*y)+3.0),
5*(-sin(x/2)*cos(y)+cos(x/2)*sin(2*y))],[x,-%pi,%pi],[y,-%pi,%pi],
['grid,40,40])                          ;
/* torus */
plot3d([cos(y)*(10.0+6*cos(x)),
sin(y)*(10.0+6*cos(x)),
-6*sin(x)], [x,0,2*%pi],[y,0,2*%pi],['grid,40,40]) ;
|#

(defun ensure-string (x)
  (cond
    ((stringp x) x)
    ((symbolp x) (print-invert-case (stripdollar x)))
    (t (maybe-invert-string-case (string (implode (strgrind x)))))))

(defmfun $join (x y)
  (if (and ($listp x) ($listp y))
      (cons '(mlist) (loop for w in (cdr x) for u in (cdr y) collect w collect u))
      (merror (intl:gettext "join: both arguments must be lists."))))

(defun coerce-float (x) ($float (meval* x)))

(defvar *maxima-plotdir* "")
(declare-top (special *maxima-tempdir* *maxima-prefix*))

(defvar *z-range* nil)
(defvar *original-points* nil)
(defvar $axes_length 4.0)
(defvar *rot* (make-array 9 :element-type 'flonum))
(defvar $rot nil)

(defvar $plot_options 
  `((mlist)
    ((mlist) $t -3 3)
    ((mlist) $grid 30 30)
    ((mlist) $transform_xy nil)
    ((mlist) $run_viewer t)
    ((mlist) $axes t)
    ((mlist) $plot_format
     ,(if (string= *autoconf-win32* "true")
          '$gnuplot
          '$gnuplot_pipes))
    ((mlist) $color $blue $red $green $magenta
     $black $cyan)
    ((mlist) $point_type $bullet $circle $plus $times
     $asterisk $box $square $triangle $delta $wedge
     $nabla $diamond $lozenge)
    ((mlist) $palette
     ((mlist) $hue 0.25 0.7 0.8 0.5)
     ((mlist) $hue 0.65 0.8 0.9 0.55)
     ((mlist) $hue 0.55 0.8 0.9 0.4)
     ((mlist) $hue 0.95 0.7 0.8 0.5))
    ((mlist) $gnuplot_term $default)
    ((mlist) $gnuplot_out_file nil)
    ;; With adaptive plotting, 100 is probably too
    ;; many ticks.
    ((mlist) $nticks 29)
    ;; Controls the number of splittings
    ;; adaptive-plotting will do.
    ((mlist) $adapt_depth 5)
    ((mlist) $gnuplot_preamble "")
    ((mlist) $gnuplot_default_term_command ,(if (string= *autoconf-win32* "true") "set term windows" "set term pop"))
    ((mlist) $gnuplot_dumb_term_command
     "set term dumb 79 22")
    ((mlist) $gnuplot_ps_term_command
     "set size 1.5, 1.5;set term postscript eps enhanced color solid 24")
    ((mlist) $plot_realpart nil)))

;; $plot_realpart option is false by default but *plot-realpart* is true
;; because coerce-float-fun is used outside of plot package too.
(defvar *plot-realpart* t)

(defun maybe-realpart (x)
  (if *plot-realpart*
      ($realpart x)
      (if (zerop1 ($imagpart x))
          ($realpart x)
          nil)))

(defvar *missing-data-indicator* "NaN")

(defvar *gnuplot-stream* nil)
(defvar *gnuplot-command* "")

(defvar $gnuplot_command (if (string= *autoconf-win32* "true")
                             "wgnuplot"
                             "gnuplot"))

(defun start-gnuplot-process (path)
  #+clisp (setq *gnuplot-stream* (ext:make-pipe-output-stream path))
  #+lispworks (setq *gnuplot-stream* (system:open-pipe path))
  #+cmu (setq *gnuplot-stream*
              (ext:process-input (ext:run-program path nil :input :stream
                                                  :output nil :wait nil)))
  #+scl (setq *gnuplot-stream*
              (ext:process-input (ext:run-program path nil :input :stream
                                                  :output nil :wait nil)))
  #+sbcl (setq *gnuplot-stream*
               (sb-ext:process-input (sb-ext:run-program path nil
                                                         :input :stream
                                                         :output nil :wait nil
                                                         :search t)))
  #+gcl (setq *gnuplot-stream*
              (open (concatenate 'string "| " path) :direction :output))
  #+ecl (progn
          (setq *gnuplot-stream* (ext:run-program path nil :input :stream :output t :error :output :wait nil)))
  #+ccl (setf *gnuplot-stream*
              (ccl:external-process-input-stream
               (ccl:run-program path nil
                                :wait nil :output nil
                                :input :stream)))
  #+allegro (setf *gnuplot-stream* (excl:run-shell-command
                    path :input :stream :output nil :wait nil))
  #+abcl (setq *gnuplot-stream* (system::process-input (system::run-program path nil :wait nil)))
  #-(or clisp cmu sbcl gcl scl lispworks ecl ccl allegro abcl)
  (merror (intl:gettext "plotting: I don't know how to tell this Lisp to run Gnuplot."))
  
  ;; set mouse must be the first command send to gnuplot
  (send-gnuplot-command "set mouse"))

(defun check-gnuplot-process ()
  (if (null *gnuplot-stream*)
      (start-gnuplot-process $gnuplot_command)))

(defun $gnuplot_close ()
  (stop-gnuplot-process)
  "")

(defun $gnuplot_start ()
  (check-gnuplot-process)
  "")

(defun $gnuplot_restart ()
  ($gnuplot_close)
  ($gnuplot_start))

(defun stop-gnuplot-process ()
  (unless (null *gnuplot-stream*)
      (progn
        (close *gnuplot-stream*)
        (setq *gnuplot-stream* nil))))

(defun send-gnuplot-command (command)
  (if (null *gnuplot-stream*)
      (start-gnuplot-process $gnuplot_command))
  (format *gnuplot-stream* "~a ~%" command)
  (force-output *gnuplot-stream*))

(defun $gnuplot_reset ()
  (send-gnuplot-command "unset output")
  (send-gnuplot-command (translate-gnuplot-term-option))
  (send-gnuplot-command "reset"))

;; If embedded in output, the gnuplot_term option makes Gnuplot unhappy,
;; so translate gnuplot_term into something Gnuplot actually wants to see.
;; Logic copied from GNUPLOT-PRINT-HEADER.

(defun translate-gnuplot-term-option ()
  (case ($get_plot_option '$gnuplot_term 2)
    ($default (get-plot-option-string '$gnuplot_default_term_command))
    ($ps (get-plot-option-string '$gnuplot_ps_term_command))
    ($dumb (get-plot-option-string '$gnuplot_dumb_term_command))
    (t (format nil "set term ~a" (get-plot-option-string '$gnuplot_term)))))

(defun $gnuplot_replot (&optional s)
  (if (null *gnuplot-stream*)
      (merror (intl:gettext "gnuplot_replot: Gnuplot is not running.")))
  (cond ((null s)
         (send-gnuplot-command "replot"))
        ((stringp s)
         (send-gnuplot-command s)
         (send-gnuplot-command "replot"))
        (t
         (merror (intl:gettext "gnuplot_replot: argument, if present, must be a string; found: ~M") s)))
  "")

;; allow this to be set in a system init file (sys-init.lsp)

(defun $get_plot_option (name &optional n)
  (loop for v in (cdr $plot_options)
         when (eq (second v) name) do
         (return (if n (nth n  v) v))))

(defun get-plot-option-string (option &optional (index 1))
  (let* ((val ($get_plot_option option 2))
         (val-list (if ($listp val)
                       (cdr val)
                       `(,val))))
    (ensure-string (nth (mod (- index 1) (length val-list)) val-list))))

(defun check-list-items (name lis type length)
  (or (eql (length lis) length)
      (merror (intl:gettext "set_plot_option: expected ~M items in the ~M list; found ~M items.") length name (length lis)))
  `((mlist) , name ,@
    (loop for v in lis
     do (setq v (meval* v))
     when (not (typep v type))
     do
     (merror (intl:gettext "set_plot_option: expected only ~M items in the ~M list; found a ~M.") type name (type-of v))
     collect v)))

(defun $set_plot_option ( value)
  (setq $plot_options ($copylist $plot_options))
  (unless (and ($listp value) (symbolp (setq name (second value))))
    (merror
     (intl:gettext
      "set_plot_option: plot option must be a list whose first element is a symbol; found: ~M")
     value))
  (setq value
        (case name
          (($x $y $z $t)
           (check-range value)
           )
          ($grid  (check-list-items name (cddr value) 'fixnum 2))
          ($nticks  (check-list-items name (cddr value) 'fixnum 1))
          (($run_viewer $transform_xy $gnuplot_pm3d $box $colorbox $gnuplot_4_0)
           (check-list-items name (cddr value) 't 1))
          ($axes
           (unless (member (third value) '($x $y t nil))
             (merror
              (intl:gettext "set_plot_option: axes must be either true, false, x or y; found: ~M") (third value)))
           value)
          ($plot_format
           (case (third value) ($openmath (setf (third value) '$xmaxima)))
           (unless (member (third value)
                       (if (string= *autoconf-win32* "true")
                           '($geomview $gnuplot $mgnuplot $xmaxima)
                           '($geomview $gnuplot $gnuplot_pipes
                             $mgnuplot $xmaxima)))
             (merror
              (intl:gettext
               "set_plot_option: plot_format must be either gnuplot, mgnuplot, xmaxima, or geomview; found: ~M") (third value)))
           value)
          (($color $point_type $palette) value)
          (($azimuth $elevation)
           (unless (integerp (third value))
             (merror (intl:gettext "set_plot_option: azimuth must be an integer; found: ~M") (third value)))
           value)
          ($mesh_lines_color
           (unless (or (symbolp (third value)) (stringp (third value)))
             (merror (intl:gettext "set_plot_option: mesh_lines_color must be a symbol or string; found: ~M") (third value)))
           value)
          ($gnuplot_term
           (unless (or (symbolp (third value)) (stringp (third value)))
               (merror (intl:gettext "set_plot_option: gnuplot_term must be a symbol or string; found: ~M") (third value)))
           value)
          ($gnuplot_out_file value)
          ($gnuplot_curve_titles (if ($listp value)
                                     value
                                     `((mlist) ,value)))
          ($gnuplot_curve_styles (if ($listp value)
                                     value
                                     `((mlist) ,value)))
          ($gnuplot_preamble value)
          ($gnuplot_default_term_command value)
          ($gnuplot_dumb_term_command value)
          ($gnuplot_ps_term_command value)
          ($adapt_depth (check-list-items name (cddr value) 'fixnum 1))
          ($plot_realpart value)
          (t
           (merror
            (intl:gettext "set_plot_option: unknown plot option: ~M")
            name))))

  (let ((v (rassoc name (cdr $plot_options) :test #'(lambda (a b) (eq a (car b))))))
    (if v
        (setf (cdr v) (cdr value))
        (nconc $plot_options (list value))))  
  $plot_options)

(defun get-gnuplot-term (term)
  (let* ((sterm (string-downcase (ensure-string term)))
         (pos   (search " " sterm)))
    (if pos  
      (subseq sterm 0 pos)
      sterm)))
  
(defvar $pstream nil)

(defun print-pt1 (f str)
  (if (floatp f)
    (format str "~g " f)
    (format str "~a " *missing-data-indicator*)))

(defstruct (polygon (:type list)
                    (:constructor %make-polygon (pts edges)))
  (dummy '($polygon simp))
  pts edges)

(eval-when
    #+gcl (compile eval)
    #-gcl (:compile-toplevel :execute)

    (defmacro z-pt (ar i) `(aref ,ar (the fixnum (+ 2 (* ,i 3)))))
    (defmacro y-pt (ar i) `(aref ,ar (the fixnum (1+ (* ,i 3)))))
    (defmacro x-pt (ar i) `(aref ,ar (the fixnum (* ,i 3))))
    (defmacro rot (m i j) `(aref ,m (the fixnum (+ ,i (the fixnum (* 3 ,j))))))

    (defmacro print-pt (f)
      `(print-pt1 ,f $pstream ))

    (defmacro make-polygon (a b)
      `(list '($polygon) ,a ,b)))

(defun draw3d (f minx maxx miny maxy  nxint nyint)
  (let* ((epsx (/ (- maxx minx) nxint))
         (x 0.0)  ( y 0.0)
         (epsy (/ (- maxy miny) nyint))
         (nx (+ nxint 1))
         (l 0)
         (ny (+ nyint 1))
         (ar (make-array  (+ 12         ; 12  for axes
                             (* 3 nx ny))  :fill-pointer (* 3 nx ny)
                             :element-type t :adjustable t)))
    (declare (type flonum x y epsy epsx)
             (fixnum nx  ny l)
             (type (cl:array t) ar))
    (loop for j below ny
           initially (setq y miny)
           do (setq x minx)
           (loop for i below nx
                  do
                  (setf (x-pt ar l) x)
                  (setf (y-pt ar l) y)
                  (setf (z-pt ar l) (funcall f x y))
                  (incf l)
                  (setq x (+ x epsx))
                  )
           (setq y (+ y epsy)))
    (make-polygon  ar  (make-grid-vertices nxint nyint))))

;; The following is 3x2 = 6 rectangles
;; call (make-vertices 3 2)
;; there are 4x3 = 12 points.
;; ordering is x0,y0,z0,x1,y1,z1,....,x11,y11,z11
;; ----
;; ||||
;; ----
;; ||||
;; ----

(defun make-grid-vertices (nx ny)
  (declare (fixnum nx ny))
  (let* ((tem (make-array (+ 15 (* 5 nx ny)) :fill-pointer (* 5 nx ny)
                          :adjustable t
                          :element-type '(mod  65000)))
         (m  nx )
         (nxpt (+ nx 1))
         (i 0)
         )
    (declare (fixnum i nxpt m)
             (type (cl:array (mod 65000)) tem))
    (loop for k below (length tem)
           do
           (setf (aref tem k) i)
           (setf (aref tem (incf k))
                 (+ nxpt i))
           (setf (aref tem (incf k))
                 (+ nxpt (incf i )))
           (setf (aref tem (incf k)) i)
           (setf (aref tem (incf k)) 0) ;place for max
           (setq m (- m 1))
           (cond ((eql  m 0)
                  (setq m nx)
                  (setq i (+ i 1))))
           )
    tem))

(defun $rotation1 (phi th)
  (let ((sinph (sin phi))
        (cosph (cos phi))
        (sinth (sin th))
        (costh (cos th)))
    `(($matrix simp)
      ((mlist simp) ,(* cosph costh)
       ,(* -1.0 cosph sinth)
       ,sinph)
      ((mlist simp) ,sinth ,costh 0.0)
      ((mlist simp) ,(- (*  sinph costh))
       ,(* sinph sinth)
       ,cosph))))
   
;; pts is a vector of bts [x0,y0,z0,x1,y1,z1,...] and each tuple xi,yi,zi is rotated
;; also the *z-range* is computed.
#-abcl (defun $rotate_pts(pts rotation-matrix)
  (or ($matrixp rotation-matrix) (merror (intl:gettext "rotate_pts: second argument must be a matrix.")))
  (let* ((rot *rot*)
         (l (length pts))
         (x 0.0) (y 0.0) (z 0.0)
         )
    (declare (type flonum  x y z))
    (declare (type (cl:array flonum) rot))
    ($copy_pts rotation-matrix *rot* 0)
        
    (loop with j = 0
           while (< j l)
           do
           (setq x (aref pts j))
           (setq y (aref pts (+ j 1)))
           (setq z (aref pts (+ j 2)))
           (loop for i below 3 with a of-type flonum = 0.0
                  do
                  (setq a (* x (aref rot (+ (* 3 i) 0))))
                  (setq a (+ a (* y (aref rot (+ (* 3 i) 1)))))
                  (setq a (+ a (* z (aref rot (+ (* 3 i) 2)))))
                  (setf (aref pts (+ j i )) a))
           (setf j (+ j 3)))))

(defun $rotate_list (x)
  (cond ((and ($listp x) (not (mbagp (second x))))
         ($list_matrix_entries (ncmul2  $rot x)))
        ((mbagp x) (cons (car x) (mapcar '$rotate_list (cdr x))))))

(defun $get_range (pts k &aux (z 0.0) (max most-negative-flonum) (min most-positive-flonum))
  (declare (type flonum z max min))
  (declare (type (vector flonum) pts))
  (loop for i from k below (length pts) by 3
         do (setq z (aref pts i))
         (cond ((< z min) (setq min z)))
         (cond ((> z max) (setq max z))))
  (list min max (- max min)))


;; figure out the rotation to make pt the direction from which we view,
;; and to rotate z axis to vertical.
;; First get v, so p.v=0 then do u= p X v to give image of y axis
;; ans = transpose(matrix( v,u,p))

(defun length-one (pt)
  (flet (($norm (pt) (loop for v in (cdr pt) sum (* v v))))
    (let ((len (sqrt ($norm pt))))
      (cons '(mlist) (loop for v in (cdr pt) collect (/  (float v) len))))))

(defun cross-product (u v)
  (flet ((cp (i j)
           (- (* (nth i u) (nth j v))
              (* (nth i v) (nth j u)))))
    `((mlist) ,(cp 2 3) ,(cp 3 1) ,(cp 1 2))))
        
(defun get-rotation (pt)
  (setq pt (length-one pt))
  (let (v tem u)
    (cond((setq tem (position 0.0 pt))
          (setq v (cons '(mlist) (list 0.0 0.0 0.0)))
          (setf (nth tem v) 1.0))
         (t (setq v (length-one `((mlist) ,(- (third pt))      , (second pt) 0.0)))))
    (setq u (cross-product pt v))
    (let* (($rot   `(($matrix) ,v,u,pt))
           (th (get-theta-for-vertical-z
                (fourth (second $rot))
                (fourth (third $rot)))))
      (or (zerop th)
          (setq $rot (ncmul2 ($rotation1 0.0 th)     $rot)))
      $rot)))

(defun get-theta-for-vertical-z (z1 z2)
  (cond ((eql z1 0.0)
         (if (> z2 0.0)
             0.0
             (coerce pi 'flonum)))
        (t
         (cl:atan  z2 z1 ))))

(defun $polar_to_xy (pts &aux (r 0.0) (th 0.0))
  (declare (type flonum r th))
  (declare (type (cl:array t) pts))
  (assert (typep pts '(vector t)))
  (loop for i below (length pts) by 3
         do (setq r (aref pts i))
         (setq th (aref pts (+ i 1)))
         (setf (aref pts i) (* r (cos th)))
         (setf (aref pts (+ i 1)) (* r (sin th)))))

;; Transformation from spherical coordinates to rectangular coordinates,
;; to be used in plot3d. Example of its use:
;; plot3d (expr, [th, 0, %pi], [ph, 0, 2*%pi], [transform_xy, spherical_to_xyz])
;; where expr gives the value of r in terms of the inclination (th)
;; and azimuth (ph).
;;
(defun $spherical_to_xyz (pts &aux (r 0.0) (th 0.0) (ph 0.0)) 
  (declare (type flonum r th ph))
  (declare (type (cl:array t) pts))
  (assert (typep pts '(vector t)))
  (loop for i below (length pts) by 3
     do (setq th (aref pts i))
       (setq ph (aref pts (+ i 1)))
       (setq r (aref pts (+ i 2)))
       (setf (aref pts i) (* r (sin th) (cos ph)))
       (setf (aref pts (+ i 1)) (* r (sin th) (sin ph)))
       (setf (aref pts (+ i 2)) (* r (cos th)))))
      

;; return a function suitable for the transform function in plot3d.
;; FX, FY, and FZ are functions of three arguments.
(defun $make_transform (lvars fx fy fz)
  (setq fx (coerce-float-fun fx lvars))
  (setq fy (coerce-float-fun fy lvars))
  (setq fz (coerce-float-fun fz lvars))
  (let ((sym (gensym "transform")))
    (setf (symbol-function sym)
          #'(lambda (pts &aux  (x1 0.0)(x2 0.0)(x3 0.0))
              (declare (type flonum  x1 x2 x3))
              (declare (type (cl:array t) pts))
              (loop for i below (length pts) by 3
                     do 
                     (setq x1 (aref pts i))
                     (setq x2 (aref pts (+ i 1)))
                     (setq x3 (aref pts (+ i 2)))
                     (setf (aref pts i) (funcall fx x1 x2 x3))
                     (setf (aref pts (+ 1 i)) (funcall fy x1 x2 x3))
                     (setf (aref pts (+ 2 i)) (funcall fz x1 x2 x3)))))))

;; Return value is a Lisp function which evaluates EXPR to a float.
;; COERCE-FLOAT-FUN always returns a function and never returns a symbol,
;; even if EXPR is a symbol.
;;
;; Following cases are recognized:
;; EXPR is a symbol
;;   name of a Lisp function
;;   name of a Maxima function
;;   name of a DEFMSPEC function
;;   name of a Maxima macro
;;   a string which is the name of a Maxima operator (e.g., "!")
;;   name of a simplifying function
;; EXPR is a Maxima lambda expression
;; EXPR is a general Maxima expression
;;
;; %COERCE-FLOAT-FUN is the main internal routine for this.
;; COERCE-FLOAT-FUN is the user interface for creating a function that
;; returns floats.  COERCE-BFLOAT-FUN is the same, except bfloats are
;; returned.
(defun %coerce-float-fun (float-fun expr &optional lvars)
  (cond ((and (consp expr) (functionp expr))
         (let ((args (if lvars (cdr lvars) (list (gensym)))))
           (coerce-lisp-function-or-lisp-lambda args expr :float-fun float-fun)))
        ;; expr is a string which names an operator
        ;; (e.g. "!" "+" or a user-defined operator)
        ((and (stringp expr) (getopr0 expr))
         (let ((a (if lvars lvars `((mlist) ,(gensym)))))
           (%coerce-float-fun float-fun `(($apply) ,(getopr0 expr) ,a) a)))
        ((and (symbolp expr) (not (member expr lvars)) (not ($constantp expr)))
         (cond
           ((fboundp expr)
            (let ((args (if lvars (cdr lvars) (list (gensym)))))
              (coerce-lisp-function-or-lisp-lambda args expr :float-fun float-fun)))

           ;; expr is name of a Maxima function defined by := or
           ;; define
           ((mget expr 'mexpr)
            (let*
                ((mexpr (mget expr 'mexpr))
                 (args (cdr (second mexpr))))
              (coerce-maxima-function-or-maxima-lambda args expr :float-fun float-fun)))

           ((or
             ;; expr is the name of a function defined by defmspec
             (get expr 'mfexpr*)
             ;; expr is the name of a Maxima macro defined by ::=
             (mget expr 'mmacro)
             ;; expr is the name of a simplifying function, and the
             ;; simplification property is associated with the noun
             ;; form
             (get ($nounify expr) 'operators)
             ;; expr is the name of a simplifying function, and the
             ;; simplification property is associated with the verb
             ;; form
             (get ($verbify expr) 'operators))
            (let ((a (if lvars lvars `((mlist) ,(gensym)))))
              (%coerce-float-fun float-fun `(($apply) ,expr ,a) a)))
           (t
            (merror (intl:gettext "COERCE-FLOAT-FUN: no such Lisp or Maxima function: ~M") expr))))

	((and (consp expr) (eq (caar expr) 'lambda))
	 (let ((args (cdr (second expr))))
	   (coerce-maxima-function-or-maxima-lambda args expr :float-fun float-fun)))

        (t
         (let* ((vars (or lvars ($sort ($listofvars expr))))
		(subscripted-vars ($sublist vars '((lambda) ((mlist) $x) ((mnot) (($atom) $x)))))
		gensym-vars save-list-gensym subscripted-vars-save
		subscripted-vars-mset subscripted-vars-restore)

	   ;; VARS and SUBSCRIPTED-VARS are Maxima lists.  Other lists are
	   ;; Lisp lists.
	   (when (cdr subscripted-vars)
	     (setq gensym-vars (mapcar #'(lambda (ign) (declare (ignore ign)) (gensym))
				       (cdr subscripted-vars)))
	     (mapcar #'(lambda (a b) (setq vars (subst b a vars :test 'equal)))
		     (cdr subscripted-vars) gensym-vars)

	     ;; This stuff about saving and restoring array variables
	     ;; should go into MBINDING, and the lambda expression
	     ;; constructed below should call MBINDING.  (At present
	     ;; MBINDING barfs on array variables.)
	     (setq save-list-gensym (gensym))
	     (setq subscripted-vars-save
		   (mapcar #'(lambda (a) `(push (meval ',a) ,save-list-gensym))
			   (cdr subscripted-vars)))
	     (setq subscripted-vars-mset
		   (mapcar #'(lambda (a b) `(mset ',a ,b))
			   (cdr subscripted-vars) gensym-vars))
	     (setq subscripted-vars-restore
		   (mapcar #'(lambda (a) `(mset ',a (pop ,save-list-gensym)))
			   (reverse (cdr subscripted-vars)))))

	   (coerce
	    `(lambda ,(cdr vars)
	       (declare (special ,@(cdr vars) errorsw))

	       ;; Nothing interpolated here when there are no subscripted
	       ;; variables.
	       ,@(if save-list-gensym `((declare (special ,save-list-gensym))))

	       ;; Nothing interpolated here when there are no subscripted
	       ;; variables.
	       ,@(if (cdr subscripted-vars)
		     `((progn (setq ,save-list-gensym nil)
			      ,@(append subscripted-vars-save subscripted-vars-mset))))

	       (let (($ratprint nil)
		     ;; We don't want to set $numer to T when coercing
		     ;; to a bigfloat.  By doing so, things like
		     ;; log(400)^400 get converted to double-floats,
		     ;; which causes a double-float overflow.  But the
		     ;; whole point of coercing to bfloat is to use
		     ;; bfloats, not doubles.
		     ;;
		     ;; Perhaps we don't even need to do this for
		     ;; double-floats?  It would be nice to remove
		     ;; this.  For backward compatibility, we bind
		     ;; numer to T if we're not trying to bfloat.
		     ($numer ,(not (eq float-fun '$bfloat)))
		     (*nounsflag* t)
		     (errorsw t)
		     (errcatch t))
		 (declare (special errcatch))
		 ;; Catch any errors from evaluating the
		 ;; function.  We're assuming that if an error
		 ;; is caught, the result is not a number.  We
		 ;; also assume that for such errors, it's
		 ;; because the function is not defined there,
		 ;; not because of some other maxima error.
		 ;;
		 ;; GCL 2.6.2 has handler-case but not quite ANSI yet. 
		 (let ((result
			#-gcl
			 (handler-case 
			     (catch 'errorsw
			       (,float-fun (maybe-realpart (meval* ',expr))))
			   ;; Should we just catch all errors here?  It is
			   ;; rather nice to only catch errors we care
			   ;; about and let other errors fall through so
			   ;; that we don't pretend to do something when
			   ;; it is better to let the error through.
			   (arithmetic-error () t)
			   (maxima-$error () t))
			 #+gcl
			 (handler-case 
			     (catch 'errorsw
			       (,float-fun (maybe-realpart (meval* ',expr))))
			   (cl::error () t))
			 ))

		   ;; Nothing interpolated here when there are no
		   ;; subscripted variables.
		   ,@(if (cdr subscripted-vars) `((progn ,@subscripted-vars-restore)))

		   result)))
	    'function)))))

(defun coerce-float-fun (expr &optional lvars)
  (%coerce-float-fun '$float expr lvars))

(defun coerce-bfloat-fun (expr &optional lvars)
  (%coerce-float-fun '$bfloat expr lvars))

(defun coerce-maxima-function-or-maxima-lambda (args expr &key (float-fun '$float))
  (let ((gensym-args (loop for x in args collect (gensym))))
    (coerce
      `(lambda ,gensym-args (declare (special ,@gensym-args))
         (let* (($ratprint nil)
                ($numer t)
                (*nounsflag* t)
		(errorsw t)
		(errcatch t))
	   (declare (special errcatch))
	   ;; Just always try to convert the result to a float,
	   ;; which handles things like $%pi.  See also BUG
	   ;; #2880115
	   ;; http://sourceforge.net/tracker/?func=detail&atid=104933&aid=2880115&group_id=4933
	   ;;
	   ;; Should we use HANDLER-CASE like we do above in
	   ;; %coerce-float-fun?  Seems not necessary for what we want
	   ;; to do.
	   (catch 'errorsw
	     (,float-fun
	      (maybe-realpart (mapply ',expr (list ,@gensym-args) t))))))
      'function)))

;; Same as above, but call APPLY instead of MAPPLY.

(defun coerce-lisp-function-or-lisp-lambda (args expr &key (float-fun '$float))
  (let ((gensym-args (loop for x in args collect (gensym))))
    (coerce
      `(lambda ,gensym-args (declare (special ,@gensym-args))
         (let* (($ratprint nil)
                ($numer t)
                (*nounsflag* t)
                (result (maybe-realpart (apply ',expr (list ,@gensym-args)))))
           ;; Always use $float.  See comment for
           ;; coerce-maxima-function-ormaxima-lambda above.
           (,float-fun result)))
      'function)))

(defmacro zval (points verts i) `(aref ,points (+ 2 (* 3 (aref ,verts ,i)))))

;;sort the edges array so that drawing the edges will happen from the back towards
;; the front.   The if n==4 the edges array coming in looks like
;; v1 v2 v3 v4 0 w1 w2 w3 w4 0 ...
;; where vi,wi are indices pointint into the points array specifiying a point
;; in 3 space.   After the sorting is done, the 0 is filled in with the vertex
;; which is closer to us (ie highest z component after rotating towards the user)
;; and this is then they are sorted in groups of 5.   
(defun sort-ngons (points edges n &aux lis )
  (declare (type (cl:array (flonum))  points)
           (type (cl:array (mod 65000)) edges)
           (fixnum n))
  (let ((new (make-array (length edges) :element-type  (array-element-type edges)))
        (i 0)
        (z 0.0)
        (z1 0.0)
        (n1 (- n 1))
        (at 0)
        (leng (length edges))
        )
    (declare (type (cl:array (mod 65000)) new)
             (fixnum i leng n1 at )
             )
    (declare (type flonum z z1))
    
    (setq lis
          (loop  for i0 below leng by (+ n 1)
                  do 
                  (setq i i0)
                  (setq at 0)
                  (setq z (zval points edges i))
                  (setq i (+ i 1))
                  (loop for j below n1
                         do (if (> (setq z1 (zval points edges i))  z)
                                (setq z z1 at (aref edges i) ))
                         (setq i (+ i 1))
                         )
                  (setf (aref edges i) at)
                  collect (cons z i0)))
    (setq lis (sort lis #'alphalessp :key #'car))
    (setq i 0)
    (loop for v in lis
           do
           (loop for j from (cdr v) 
                  for k to n
                  do (setf (aref new i) (aref edges j))
                  (incf i))
           )
    (copy-array-portion edges new  0 0 (length edges))
    ))

(defun copy-array-portion (ar1 ar2 i1 i2 n1)
  (declare (fixnum i1 i2 n1))
  (loop while (>= (setq n1 (- n1 1)) 0)
         do (setf (aref ar1 i1) (aref ar2 i2))
         (setq i1 (+ i1 1))
         (setq i2 (+ i2 1))))


(defun $concat_polygons (pl1 pl2 &aux tem new)
  (setq new
        (loop for v in pl1 
               for w in pl2
               for l = (+ (length v) (length w))
               do (setq tem (make-array l
                                        :element-type (array-element-type v)
                                        :fill-pointer  l
                                        )
                        )
               collect tem))
  (setq new (make-polygon (first new) (second new)) )

  (copy-array-portion (polygon-pts pl1) (polygon-pts new)
                      0 0 (length (polygon-pts pl1)))
  (copy-array-portion (polygon-pts pl2) (polygon-pts new)
                      (length (polygon-pts pl1))
                      0 (length (polygon-pts pl2)))
  (copy-array-portion (polygon-edges pl1) (polygon-edges new)
                      0 0 (length (polygon-edges pl1)))
  (loop for i from (length (polygon-edges pl1))
         for j from 0 below (length (polygon-edges pl2))
         with  lpts1  =  (length (polygon-pts pl1))
         with ar2   =  (polygon-edges pl2)
         with arnew =  (polygon-edges new)
         do (setf (aref arnew i) (+ lpts1 (aref ar2 j)))))

(defun $copy_pts(lis vec start)
  (declare (fixnum start))
  (let ((tem vec))
    (declare (type (cl:array flonum) tem))
    (cond ((numberp lis)
           (or (typep lis 'flonum) (setq lis (float lis)))
           (setf (aref tem start) lis)
           (1+ start))
          ((typep lis 'cons)
           ($copy_pts (cdr lis) vec  ($copy_pts (car lis) vec start)))
          ((symbolp lis) start)
          (t (merror (intl:gettext "copy_pts: unrecognized first argument: ~M") lis)))))

;; parametric ; [parametric,xfun,yfun,[t,tlow,thigh],[nticks ..]]
;; the rest of the parametric list after the list will be pushed plot_options

(defun draw2d-parametric (param range1 &aux range tem)
  (cond ((and ($listp (setq tem (nth 4 param)))
              (symbolp (cadr tem))
              (eql ($length tem) 3))
         ;; sure looks like a range
         (setq range tem)))
  (let* (($plot_options ($append ($rest param 3)
                                 (if range1
                                     ($cons range1 $plot_options)
                                     $plot_options)))
         (nticks (third($get_plot_option '$nticks)))
         (trange (or range ($get_plot_option '$t)))
         (xrange ($get_plot_option '$x))
         (yrange ($get_plot_option '$y))
         (tmin (coerce-float (third trange)))
         (tmax (coerce-float (fourth trange)))
         (xmin (coerce-float (third xrange)))
         (xmax (coerce-float (fourth xrange)))
         (ymin (coerce-float (third yrange)))
         (ymax (coerce-float (fourth yrange)))
         (x 0.0)         ; have to initialize to some floating point..
         (y 0.0)
         (tt tmin)
         (eps (/ (- tmax tmin) (- nticks 1)))
         f1 f2 in-range-y in-range-x in-range last-ok 
         )
    (declare (type flonum x y tt ymin ymax xmin xmax tmin tmax eps))
    (setq f1 (coerce-float-fun (third param) `((mlist), (second trange))))
    (setq f2 (coerce-float-fun (fourth param) `((mlist), (second trange))))
    (cons '(mlist simp)    
          (loop 
           do 
           (setq x (funcall f1 tt))
           (setq y (funcall f2 tt))
           (setq in-range-y (and (<= y ymax) (>= y ymin)))
           (setq in-range-x  (and  (<= x xmax) (>= x xmin)))
           (setq in-range (and in-range-x in-range-y))
           when (and (not in-range) (not last-ok))
           collect  'moveto and collect 'moveto
           do
           (setq last-ok in-range)
           collect (if in-range-x x (if (> x xmax) xmax xmin))
           collect (if in-range-y y (if (> y ymax) ymax ymin))
           when (>= tt tmax) do (loop-finish)
           do (setq tt (+ tt eps))
           (if (>= tt tmax) (setq tt tmax))
           )))
  )

(defun draw2d-discrete (f)
  (let* ((f (copy-tree f))              ; Copy all of F because we destructively modify it below.
         (x (third f))
         (y (fourth f)))
    (let
      ((data
         (cond
           ((= (length f) 4)                 ; [discrete,x,y]
            (if (not ($listp x))
              (merror (intl:gettext "draw2d (discrete): argument must be a list; found: ~M") x))
            (if (not ($listp y))
              (merror (intl:gettext "draw2d (discrete): argument must be a list; found: ~M") y))
            (cons '(mlist) (mapcan #'list (rest x) (rest y))))
           ((= (length f) 3)                 ; [discrete,xy]
            (if (not ($listp x))
              (merror (intl:gettext "draw2d (discrete): argument must be a list; found: ~M") x))
            (let ((tmp (mapcar #'rest (rest x))))
              (cons '(mlist) (mapcan #'append tmp))))
           (t                                ; error
             (merror
               (intl:gettext "draw2d (discrete): argument must be [discrete, x, y] or [discrete, xy]; found: ~M") f)))))

      ;; Encourage non-floats to become floats here.

      ($float data))))


;;; Adaptive plotting, based on the adaptive plotting code from
;;; YACAS. See http://yacas.sourceforge.net/Algo.html#c3s1 for a
;;; description of the algorithm.  More precise details can be found
;;; in the file yacas/scripts/plots.rep/plot2d.ys.


;; Determine if we have a slow oscillation of the function.
;; Basically, for each 3 consecutive function values, we check to see
;; if the function is monotonic or not.  There are 3 such sets, and
;; the function is considered slowly oscillating if at most 2 of them
;; are not monotonic.
(defun slow-oscillation-p (f0 f1 f2 f3 f4)
  (flet ((sign-change (x y z)
           (cond ((not (and (numberp x) (numberp y) (numberp z)))
                  ;; Something is not a number.  Assume the
                  ;; oscillation is not slow.
                  2)
                 ((or (and (> y x) (> y z))
                      (and (< y x) (< y z)))
                  1)
                 (t
                  0))))
    (<= (+ (sign-change f0 f1 f2)
           (sign-change f1 f2 f3)
           (sign-change f2 f3 f4))
        2)))

;; Determine if the function values are smooth enough.  This means
;; that integrals of the functions on the left part and the right part
;; of the range are approximately the same.
;;
;; 
(defun smooth-enough-p (f-a f-a1 f-b f-b1 f-c eps)
  (cond ((every #'numberp (list f-a f-a1 f-b f-b1 f-c))
         (let ((quad (/ (+ f-a
                           (* -5 f-a1)
                           (* 9 f-b)
                           (* -7 f-b1)
                           (* 2 f-c))
                        24))
               (quad-b (/ (+ (* 5 f-b)
                             (* 8 f-b1)
                             (- f-c))
                          12)))
           ;; According to the Yacas source code, quad is the Simpson
           ;; quadrature for the (fb,fb1) subinterval (using points b,b1,c),
           ;; subtracted from the 4-point Newton-Cotes quadrature for the
           ;; (fb,fb1) subinterval (using points a, a1, b, b1.
           ;;
           ;; quad-b is the Simpson quadrature for the (fb,f1) subinterval.
           ;;
           ;; This used to test for diff <= 0.  But in some
           ;; situations, like plot2d(0.99,[x,0,5]), roundoff prevents
           ;; this from happening.  So we do diff < delta instead, for
           ;; some value of delta.
           ;;
           ;; XXX: What is the right value for delta?  Does this break
           ;; other things?  Simple tests thus far show that
           ;; 100*flonum-epsilon is ok.
           (let ((diff (- (abs quad)
                          (* eps (- quad-b (min f-a f-a1 f-b f-b1 f-c)))))
                 (delta (* 150 flonum-epsilon)))
             (<= diff delta))))
        (t
         ;; Something is not a number, so assume it's not smooth enough.
         nil)))
                    
    
(defun adaptive-plot (fcn a b c f-a f-b f-c depth eps)
  ;; Step 1:  Split the interval [a, c] into 5 points
  (let* ((a1 (/ (+ a b) 2))
         (b1 (/ (+ b c) 2))
         (f-a1 (funcall fcn a1))
         (f-b1 (funcall fcn b1))
         )
    (cond ((or (not (plusp depth))
               (and (slow-oscillation-p f-a f-a1 f-b f-b1 f-c)
                    (smooth-enough-p f-a f-a1 f-b f-b1 f-c eps)))
           ;; Everything is nice and smooth so we're done.  Don't
           ;; refine anymore.
           (list a f-a
                 a1 f-a1
                 b f-b
                 b1 f-b1
                 c f-c))
          ;; We are not plotting the real part of the function and the
          ;; function is undefined at all points - assume it has complex value
          ;; on [a,b]. Maybe we should refine it a couple of times just to make sure?
          ((and (null *plot-realpart*)
                (null f-a) (null f-a1) (null f-b) (null f-b1) (null f-c))
           (list a f-a
                 a1 f-a1
                 b f-b
                 b1 f-b1
                 c f-c))
          (t
           ;; Need to refine.  Split the interval in half, and try to plot each half.  
           (let ((left (adaptive-plot fcn a a1 b f-a f-a1 f-b (1- depth) (* 2 eps)))
                 (right (adaptive-plot fcn b b1 c f-b f-b1 f-c (1- depth) (* 2 eps))))
             (append left (cddr right)))))))

(defun draw2d (fcn range features)
  (if (and ($listp fcn) (equal '$parametric (cadr fcn)))
      (return-from draw2d (draw2d-parametric fcn range)))
  (if (and ($listp fcn) (equal '$discrete (cadr fcn)))
      (return-from draw2d (draw2d-discrete fcn)))
  (let* ((nticks (third ($get_plot_option '$nticks)))
         (yrange ($get_plot_option '$y))
         (depth (third ($get_plot_option '$adapt_depth))))

    (setq fcn (coerce-float-fun fcn `((mlist), (second range))))

    (let* ((x-start (coerce-float (third range)))
           (xend (coerce-float (fourth range)))
           (x-step (/ (- xend x-start) (coerce-float nticks) 2))
           (ymin (coerce-float (third yrange)))
           (ymax (coerce-float (fourth yrange)))
           (n-clipped 0) (n-non-numeric 0)
           ;; What is a good EPS value for adaptive plotting?
                                        ;(eps 1e-5)
           x-samples y-samples result
           )
      (declare (type flonum ymin ymax))
      ;; Divide the region into NTICKS regions.  Each region has a
      ;; start, mid and endpoint. Then adaptively plot over each of
      ;; these regions.  So it's probably a good idea not to make
      ;; NTICKS too big.  Since adaptive plotting splits the sections
      ;; in half, it's also probably not a good idea to have NTICKS be
      ;; a power of two.
      (when (getf features :log-x)
        (setf x-start (log x-start))
        (setf xend (log xend))
        (setf x-step (/ (- xend x-start) (coerce-float nticks) 2)))

      (flet ((fun (x)
               (let ((y (if (getf features :log-x)
                            (funcall fcn (exp x))
                            (funcall fcn x))))
                 (if (getf features :log-y)
                     (log y)
                     y))))
        
        (dotimes (k (1+ (* 2 nticks)))
          (let ((x (+ x-start (* k x-step))))
            (push x x-samples)
            (push (fun x) y-samples)))
        (setf x-samples (nreverse x-samples))
        (setf y-samples (nreverse y-samples))

        ;; For each region, adaptively plot it.
        (do ((x-start x-samples (cddr x-start))
             (x-mid (cdr x-samples) (cddr x-mid))
             (x-end (cddr x-samples) (cddr x-end))
             (y-start y-samples (cddr y-start))
             (y-mid (cdr y-samples) (cddr y-mid))
             (y-end (cddr y-samples) (cddr y-end)))
            ((null x-end))
          ;; The region is x-start to x-end, with mid-point x-mid.
          ;;
          ;; The cddr is to remove the one extra sample (x and y value)
          ;; that adaptive plot returns. But on the first iteration,
          ;; result is empty, so we don't want the cddr because we want
          ;; all the samples returned from adaptive-plot.  On subsequent
          ;; iterations, it's a duplicate of the last ponit of the
          ;; previous interval.
          (setf result
                (if result
                    (append result
                            (cddr
                             (adaptive-plot #'fun (car x-start) (car x-mid) (car x-end)
                                            (car y-start) (car y-mid) (car y-end)
                                            depth 1e-5)))
                    (adaptive-plot #'fun (car x-start) (car x-mid) (car x-end)
                                   (car y-start) (car y-mid) (car y-end)
                                   depth 1e-5))))

        ;; Fix up out-of-range values
        ;; and clobber non-numeric values.

        (do ((x result (cddr x))
             (y (cdr result) (cddr y)))
            ((null y))
          (when (getf features :log-x)
            (setf (car x) (exp (car x))))
          (when (getf features :log-y)
            (setf (car y) (exp (car y))))
          (if (numberp (car y))
            (unless (<= ymin (car y) ymax)
              (incf n-clipped)
              (setf (car x) 'moveto)
              (setf (car y) 'moveto))
            (progn
              (incf n-non-numeric)
              (setf (car x) 'moveto)
              (setf (car y) 'moveto))))

        ;; Filter out any MOVETO's which do not precede a number.
        ;; Code elsewhere in this file expects MOVETO's to
        ;; come in pairs, so leave two MOVETO's before a number.
        (let ((n (length result)))
          (dotimes (i n)
            (when
              (and
                (evenp i)
                (eq (nth i result) 'moveto)
                (eq (nth (1+ i) result) 'moveto)
                (or 
                  (eq i (- n 2))
                  (eq (nth (+ i 2) result) 'moveto)))
              (setf (nth i result) nil)
              (setf (nth (1+ i) result) nil))))

        (let ((result-sans-nil (delete nil result)))
          (if (null result-sans-nil)
            (cond
              ((= n-non-numeric 0)
               (mtell (intl:gettext "plot2d: all values were clipped.~%")))
              ((= n-clipped 0)
               (mtell (intl:gettext "plot2d: expression evaluates to non-numeric value everywhere in plotting range.~%")))
              (t
                (mtell (intl:gettext "plot2d: all values are non-numeric, or clipped.~%"))))
            (progn
              (if (> n-non-numeric 0)
                (mtell (intl:gettext "plot2d: expression evaluates to non-numeric value somewhere in plotting range.~%")))
              (if (> n-clipped 0)
                (mtell (intl:gettext "plot2d: some values were clipped.~%")))))
          (cons '(mlist) result-sans-nil))))))

(defun get-range (lis)
  (let ((ymin most-positive-flonum)
        (ymax most-negative-flonum))
    (declare (type flonum ymin ymax))
    (do ((l lis (cddr l)))
        ((null l))
      (or (floatp (car l)) (setf (car l) (float (car l))))
      (cond ((< (car l) ymin)
             (setq ymin (car l))))
      (cond ((< ymax (car l))
             (setq ymax (car l)))))
    (list '(mlist) ymin ymax)))

(defvar $gnuplot_view_args (if (string= *autoconf-win32* "true")
                               "~s -"
                               "-persist ~s"))

(defvar $gnuplot_file_args "~s")

(defvar $mgnuplot_command "mgnuplot")
(defvar $geomview_command "geomview")

(defvar $xmaxima_plot_command "xmaxima")

(defun plot-temp-file (file)
  (if *maxima-tempdir* 
    (format nil "~a/~a" *maxima-tempdir* file)
    file))

(defun gnuplot-process (&optional file)
  (let ((gnuplot-term ($get_plot_option '$gnuplot_term 2))
        (gnuplot-out-file ($get_plot_option '$gnuplot_out_file 2))
        (gnuplot-out-file-string (get-plot-option-string '$gnuplot_out_file))
        (run-viewer ($get_plot_option '$run_viewer 2))
        (gnuplot-preamble (string-downcase (get-plot-option-string '$gnuplot_preamble)))
        (view-file))
    ;; default output file name for for all formats except default
    (when (and (not (eq ($get_plot_option '$gnuplot_term 2) '$default)) 
               (null gnuplot-out-file))
      (setq gnuplot-out-file 
        (plot-temp-file (format nil "maxplot.~(~a~)" (get-gnuplot-term gnuplot-term))))
      (setq gnuplot-out-file-string gnuplot-out-file)) 
    ;; run gnuplot in batch mode if necessary before viewing
    (if (and gnuplot-out-file (not (eq gnuplot-term '$default)))
        ($system (format nil "~a ~s" $gnuplot_command file)))
    (when run-viewer
      (if (eq gnuplot-term '$default)
          (setf view-file file)
          (setf view-file gnuplot-out-file-string))
      (case gnuplot-term
        ($default
         ($system (format nil "~a ~a" $gnuplot_command
                          (format nil (if (search "set out " gnuplot-preamble) 
                                         $gnuplot_file_args 
                                         $gnuplot_view_args)
                                      view-file))))
        ($dumb
         (if gnuplot-out-file
             ($printfile view-file)
             (merror (intl:gettext "plotting: option 'gnuplot_out_file' not defined."))))))
    (if gnuplot-out-file
        gnuplot-out-file-string
        "")))

(defun plot-options-parser (options features)
;; Given a maxima list with options, it creates a lisp list with
;; keywords for the corresponding options. If an option has been
;; previously set up, its value will be updated.
  (dolist (v options)
    (if ($listp v)
        (case (second v)
          ($logx (setf (getf features :log-x) t))
          ($logy (setf (getf features :log-y) t))
          ($box (setf (getf features :box) (cddr v)))
          ($xlabel (setf (getf features :xlabel) (ensure-string (third v))))
          ($ylabel (setf (getf features :ylabel) (ensure-string (third v))))
          ($zlabel (setf (getf features :zlabel) (ensure-string (third v))))
          ($x
           (setq v (check-range v))
           (setf (getf features :xmin) (third v))
           (setf (getf features :xmax) (fourth v))
           ($set_plot_option `((mlist) $x ,(third v) ,(fourth v)))
           (unless (getf features :xlabel)
             (setf (getf features :xlabel) "x")))
          ($y
           (setq v (check-range v))
           (setf (getf features :ymin) (third v))
           (setf (getf features :ymax) (fourth v))
           ($set_plot_option `((mlist) $y ,(third v) ,(fourth v)))
           (unless (getf features :ylabel)
             (setf (getf features :ylabel) "y")))
          ($z
           (setq v (check-range v))
           (setf (getf features :zmin) (third v))
           (setf (getf features :zmax) (fourth v)))
          ($style (setf (getf features :styles) (cddr v)))
          ($legend
	   (setf (getf features :legend)
		 (if ($listp (third v)) (cdr (third v)) (cddr v))))
          ($psfile
           ($set_plot_option '((mlist simp) $gnuplot_term $ps))
           ($set_plot_option `((mlist simp) $gnuplot_out_file ,(third v)))
           (setf (getf features :psfile) (ensure-string (third v))))
          (t ($set_plot_option v)))
        (merror (intl:gettext "plotting: argument must be a list; found: ~M") v)))
  (when ($get_plot_option '$axes 2)
    (setf (getf features :axes) ($get_plot_option '$axes 2)))
  (setf (getf features :grid) ($get_plot_option '$grid))
  (if ($get_plot_option '$plot_format 2)
      (setf (getf features :plot-format) ($get_plot_option '$plot_format 2))
      (setf (getf features :plot-format) '$gnuplot))
  features)


;; plot2d
;;
;; Examples:
;; plot2d (sec(x), [x, -2, 2], [y, -20, 20], [nticks, 200])$
;;
;; plot2d (exp(3*s), [s, -2, 2], [logy])$
;;
;; plot2d ([parametric, cos(t), sin(t), [t, -%pi, %pi], [nticks, 80]],
;;         [x, -4/3, 4/3])$
;;
;; xy:[[10,.6], [20,.9], [30,1.1], [40,1.3], [50,1.4]]$
;; plot2d ( [ [discrete, xy], 2*%pi*sqrt(l/980) ], [l, 0, 50],
;;          [style, points, lines], [color, red, blue], [point_type, box],
;;          [legend, "experiment", "theory"],
;;          [xlabel, "pendulum's length (cm)"], [ylabel, "period (s)"])$
;;
;; plot2d ( x^2-1, [x, -3, 3], [y, -2, 10], [box, false], [color, red],
;;          [ylabel, "x^2-1"], [plot_format, xmaxima])$

(defun $plot2d (fun &optional range &rest options)
  (let (($display2d nil)
        (*plot-realpart* *plot-realpart*)
        ($plot_options $plot_options) (i 0)
        (output-file "") features
        gnuplot-term gnuplot-out-file file points-lists)

    ;; 1- Put fun in its most general form: a maxima list with several objects
    ;; that can be expressions (simple functions) and maxima lists (parametric
    ;; functions or discrete sets of points).

    ;; If there is a single parametric function use its range as the range for
    ;; the plot and put it inside another maxima list
    (setf (getf features :type) "plot2d")
    (when (and (consp fun) (eq (second fun) '$parametric))
      (unless range
        (setq range (check-range (nth 4 fun))))
      (setq fun `((mlist) ,fun)))

    ;; If there is a single set of discrete points put it inside a maxima list
    (when (and (consp fun) (eq (second fun) '$discrete))
      (setq fun `((mlist) ,fun)))

    ;; If at this point fun is not a maxima list, it is then a single function
    (unless ($listp fun ) (setq fun `((mlist) ,fun)))

    ;; 2- Get names for the two axis and values for xmin and xmax if needed.

    ;; If any of the objects in the fun list is a simple function,
    ;; the range option is mandatory and will provide the name of
    ;; the horizontal axis and the values of xmin and xmax.
    (let ((no-range-required t) small huge)
      #-clisp (setq small (- (/ most-positive-flonum 1024)))
      #+clisp (setq small (- (/ most-positive-double-float 1024.0)))
      #-clisp (setq huge (/ most-positive-flonum 1024))
      #+clisp (setq huge (/ most-positive-double-float 1024.0))
      ($set_plot_option `((mlist) $y ,small ,huge))
      (dolist (subfun (rest fun))
        (if (not ($listp subfun))
            (setq no-range-required nil))) 
      (unless no-range-required
        (setq range (check-range range))
        (setf (getf features :xlabel) (ensure-string (second range)))
        (setf (getf features :xmin) (third range))
        (setf (getf features :xmax) (fourth range)))
      (when no-range-required
        ;; Make the default ranges on X nd Y large so parametric plots
        ;; don't get prematurely clipped. Don't use most-positive-flonum
        ;; because draw2d will overflow.
        ($set_plot_option `((mlist) $x ,small ,huge))
        (when range
          ;; second argument was really a plot option, not a range
          (setq options (cons range options)))))

    ;; When only one function is being plotted:
    ;; If a simple function use, its name for the vertical axis.
    ;; If parametric, give the axes the names of the two parameters.
    ;; If discrete points, name the axes x and y.
    (when (= (length fun) 2)
      (let ((v (second fun)) label)
        (cond ((atom v) 
               (setq label (coerce (mstring v) 'string))
               (if (< (length label) 80)
                   (setf (getf features :ylabel) label)))
              ((eq (second v) '$parametric)
               (setq label (coerce (mstring (third v)) 'string))
               (if (< (length label) 80)
                   (setf (getf features :xlabel) label))
               (setq label (coerce (mstring (fourth v)) 'string))
               (if (< (length label) 80)
                   (setf (getf features :ylabel) label)))
              ((eq (second v) '$discrete)
               (setf (getf features :xlabel) "x")
               (setf (getf features :ylabel) "y"))
              (t
               (setq label (coerce (mstring v) 'string))
               (if (< (length label) 80)
                   (setf (getf features :ylabel) label))))))

    ;; Parse the given options into the list features
    (setq features (plot-options-parser options features))

    ;; Remove axes labels when no box is used in gnuplot
    (unless (or (null (getf features :box)) (first (getf features :box))
		(eq (getf features :plot-format) '$xmaxima))
      (remf features :xlabel)
      (remf features :ylabel))


    (let ((xmin (getf features :xmin)) (xmax (getf features :xmax)))
      (when
        (and (getf features :log-x) xmin xmax)
        (if (> xmax 0)
            (when (<= xmin 0)
              (let ((revised-xmin (/ xmax 1000)))
                (mtell (intl:gettext "plot2d: lower bound must be positive when 'logx' in effect.~%plot2d: assuming lower bound = ~M instead of ~M") revised-xmin xmin)
                (setf (getf features :xmin) revised-xmin)
                (setq range `((mlist) ,(second range) ,revised-xmin ,xmax))
                ($set_plot_option `((mlist) $x ,revised-xmin ,xmax))))
            (merror (intl:gettext "plot2d: upper bound must be positive when 'logx' in effect; found: ~M") xmax))))

    (let ((ymin (getf features :ymin)) (ymax (getf features :ymax)))
      (when (and (getf features :log-y) ymin ymax)
        (if (> ymax 0)
            (when (<= ymin 0)
              (let ((revised-ymin (/ ymax 1000)))
                (mtell (intl:gettext "plot2d: lower bound must be positive when 'logy' in effect.~%plot2d: assuming lower bound = ~M instead of ~M") revised-ymin ymin)
                (setf (getf features :ymin) revised-ymin)
                ($set_plot_option `((mlist) $y ,revised-ymin ,ymax))))
            (merror (intl:gettext "plot2d: upper bound must be positive when 'logy' in effect; found: ~M") ymax))))

    (setq *plot-realpart* ($get_plot_option '$plot_realpart 2))

    ;; Compute points to plot for each element of FUN.
    ;; If no plottable points are found, return immediately from $PLOT2D.

    (setq points-lists (mapcar #'(lambda (f) (cdr (draw2d f range features))) (cdr fun)))
    (when (= (count-if #'(lambda (x) x) points-lists) 0)
      (mtell (intl:gettext "plot2d: nothing to plot.~%"))
      (return-from $plot2d))

    (setq gnuplot-term ($get_plot_option '$gnuplot_term 2))
    (if ($get_plot_option '$gnuplot_out_file 2)
      (setq gnuplot-out-file (get-plot-option-string '$gnuplot_out_file)))
    (if (and (eq (getf features :plot-format) '$gnuplot)
             (eq gnuplot-term '$default) 
             gnuplot-out-file)
      (setq file gnuplot-out-file)
      (setq file
            (plot-temp-file
             (format nil "maxout.~(~a~)"
                     (ensure-string (getf features :plot-format))))))

    ;; old function $plot2dopen incorporated here
    (case (getf features :plot-format)
      ($xmaxima
       (show-open-plot
        (with-output-to-string
          (st)
          (xmaxima-print-header st features)
          (let ((legend (getf features :legend))
                (styles (getf features :styles)) style plot-name)
            (loop for f in (cdr fun) for points-list in points-lists do
                 (when points-list
                   (if styles
                       (progn
                         (setq style (nth (mod i (length styles)) styles))
                         (setq style (if ($listp style) (cdr style) `(,style))))
                       (setq style nil))
                   (incf i)
                   (if legend        ; legend in the command line has priority
                       (setq plot-name
                             (if (first legend)
                                 (ensure-string
                                  (nth (mod (- i 1) (length legend)) legend))
                                 nil)) ; no legend if option [legend,false]
                       (if (= 2 (length fun))
                           (progn
                             (setq plot-name nil) ;no legend for single function
                             (format st " {nolegend 1}"))
                           (setq plot-name
                                 (let ((string ""))
                                   (cond
                                     ((atom f) 
                                      (setq
                                       string (coerce (mstring f) 'string)))
                                     ((eq (second f) '$parametric)
                                      (setq
                                       string 
                                       (concatenate 
                                        'string
                                        (coerce (mstring (third f)) 'string)
                                        ", " (coerce (mstring (fourth f)) 'string))))
                                     ((eq (second f) '$discrete)
                                      (setq string
                                            (format nil "discrete~a" i)))
                                     (t
                                      (setq string
                                            (coerce (mstring f) 'string))))
                                   (cond ((< (length string) 80) string)
                                         (t (format nil "fun~a" i)))))))
                   (when plot-name 
                     (format st " {label ~s}" plot-name))
                   (format st " ~a~%" (xmaxima-curve-style style i))
                   (format st " {xversusy~%")
                   (let ((lis points-list))
                     (loop while lis
                        do
                          (loop while (and lis (not (eq (car lis) 'moveto)))
                             collecting (car lis) into xx
                             collecting (cadr lis) into yy
                             do (setq lis (cddr lis))
                             finally
                             ;; only output if at least two points for line
                               (when (cdr xx)
                                 (tcl-output-list st xx)
                                 (tcl-output-list st yy)))
                        ;; remove the moveto
                          (setq lis (cddr lis))))
                   (format st "}"))))
          (format st "} "))))
      (t
       (with-open-file (st file :direction :output :if-exists :supersede)
         (case (getf features :plot-format)
           ($gnuplot
            (gnuplot-print-header st features)
            (format st "plot")
            (when (and (getf features :xmin)(getf features :xmax))
              (format st " [~g:~g]" (getf features :xmin)(getf features :xmax)))
            (when (and (getf features :ymin)(getf features :ymax))
              (unless (and (getf features :xmin)(getf features :xmax))
                (format st " []")) 
              (format st " [~g:~g]" (getf features :ymin)(getf features :ymax))))
           ($gnuplot_pipes
            (check-gnuplot-process)
            ($gnuplot_reset)
            (gnuplot-print-header *gnuplot-stream* features)
            (setq *gnuplot-command* (format nil "plot"))
            (when (and (getf features :xmin)(getf features :xmax))
              (setq
               *gnuplot-command*
               ($sconcat
                *gnuplot-command* 
                (format nil " [~g:~g]"
                        (getf features :xmin)(getf features :xmax)))))
            (when (and (getf features :ymin)(getf features :ymax)) 
              (unless (and (getf features :xmin)(getf features :xmax))
                (setq *gnuplot-command*
                      ($sconcat *gnuplot-command* (format nil " []"))))
              (setq
               *gnuplot-command*
               ($sconcat
                *gnuplot-command* 
                (format nil " [~g:~g]"  (getf features :ymin)(getf features :ymax)))))))
         (let ((legend (getf features :legend))
                (styles (getf features :styles)) style plot-name)
           (loop for v in (cdr fun) for points-list in points-lists do
                (when points-list
                  (case (getf features :plot-format)
                    ($gnuplot_pipes
                     (if (> i 0)
                         (setq *gnuplot-command*
                               ($sconcat *gnuplot-command* ", ")))
                     (setq *gnuplot-command*
                           ($sconcat *gnuplot-command* 
                                     (format nil "~s index ~a " file i)))))
                  (if styles
                      (progn
                        (setq style (nth (mod i (length styles)) styles))
                        (setq style (if ($listp style) (cdr style) `(,style))))
                      (setq style nil))
                  (incf i)
                  (if legend
                      (setq plot-name ; legend in the command line has priority
                            (if (first legend)
                                (ensure-string
                                 (nth (mod (- i 1) (length legend)) legend))
                                nil)) ; no legend if option [legend,false]
                      (if (= 2 (length fun))
                          (setq plot-name nil) ; no legend if just one function
                          (setq plot-name
                                (let ((string ""))
                                  (cond ((atom v) 
                                         (setq string
                                               (coerce (mstring v) 'string)))
                                        ((eq (second v) '$parametric)
                                         (setq
                                          string 
                                          (concatenate
                                           'string
                                           (coerce (mstring (third v)) 'string)
                                           ", "
                                           (coerce (mstring (fourth v)) 'string))))
                                        ((eq (second v) '$discrete)
                                         (setq
                                          string (format nil "discrete~a" i)))
                                        (t
                                         (setq string 
                                               (coerce (mstring v) 'string))))
                                  (cond ((< (length string) 80) string)
                                        (t (format nil "fun~a" i)))))))
                  (case (getf features :plot-format)
                    ($gnuplot
                     (when (> i 1) (format st ","))
                     (format st " '-'")
                     (if plot-name
                         (format st " title ~s" plot-name)
                         (format st " notitle"))
                     (format st " ~a" (gnuplot-curve-style style i)))
                    ($gnuplot_pipes
                     (setq *gnuplot-command*
                           ($sconcat
                            *gnuplot-command*
                            (if plot-name 
                                (format nil " title ~s ~a" plot-name 
                                        (gnuplot-curve-style style i))
                                (format nil " notitle ~a"
                                        (gnuplot-curve-style style i))))))))))
         (case (getf features :plot-format)
           ($gnuplot
            (format st "~%"))
           ($gnuplot_pipes
            (format st "~%")))
         (setq i 0)
         (loop for v in (cdr fun) for points-list in points-lists do
              (when points-list
           (incf i)
           (case (getf features :plot-format)
             ($gnuplot
              (if (> i 1)
                  (format st "e~%")))
             ($gnuplot_pipes
              (if (> i 1)
                  (format st "~%~%")))
             ($mgnuplot
              (format st "~%~%# \"Fun~a\"~%" i))
             )
           (let (in-discontinuity points)
             (loop for (v w) on points-list by #'cddr
                do
                  (cond ((eq v 'moveto)
                         (cond 
                           ((find (getf features :plot-format) '($gnuplot_pipes $gnuplot))
                            ;; A blank line means a discontinuity
                            (if (null in-discontinuity)
                                (progn
                                  (format st "~%")
                                  (setq in-discontinuity t))))
                           ((equal (getf features :plot-format) '$mgnuplot)
                            ;; A blank line means a discontinuity
                            (format st "~%"))
                           (t
                            (format st "move "))))
                        (t (format st "~g ~g ~%" v w)
                           (setq points t)
                           (setq in-discontinuity nil))))
             (if (and (null points) (getf features :xmin) (getf features :ymin))
                 (format
                  st "~g ~g ~%"
                  (getf features :xmin)(getf features :ymin)))))))))

    (case (getf features :plot-format)
      ($gnuplot 
       (setq output-file (gnuplot-process file)))
      ($gnuplot_pipes
       (send-gnuplot-command *gnuplot-command*))
      ($mgnuplot 
       ($system (concatenate 'string *maxima-plotdir* "/" $mgnuplot_command) 
                (format nil " -plot2d ~s -title ~s" file "Fun1"))))
output-file))


(defun msymbolp (x)
  (and (symbolp x) (char= (char (symbol-value x) 0) #\$)))


(defun $tcl_output (lis i &optional (skip 2))
  (when (not (typep i 'fixnum))
    (merror
      (intl:gettext "tcl_ouput: second argument must be an integer; found ~M")
                    i))
  (when (not ($listp lis))
    (merror
      (intl:gettext "tcl_output: first argument must be a list; found ~M") lis))
  (format *standard-output* "~% {")
  (cond (($listp (second lis))
         (loop for v in lis
                do
                (format *standard-output* "~,10g " (nth i v))))
        (t
         (setq lis (nthcdr i lis))
         (loop  with v = lis  while v
                 do
                 (format *standard-output* "~,10g " (car v))
                 (setq v (nthcdr skip v)))))
  (format *standard-output* "~% }"))


(defun tcl-output-list ( st lis )
  (cond ((null lis) )
        ((atom (car lis))
         (princ " {  " st)
         (loop for v in lis
                count t into n
                when (eql 0 (mod n 5))
                do (terpri st)
                do
                (format st "~,10g " v))
         (format st  " }~%"))
        (t (tcl-output-list st (car lis))
           (tcl-output-list st (cdr lis)))))

(defvar *some-colours*
  ;; from rgb.txt
  '(135 206 250         lightskyblue
    70 130 180          steelblue
    205  92  92         indianred
    178  34  34         firebrick
    176  48  96         maroon
    221 160 221         plum
    238 130 238         violet))

(defun check-range (range &aux tem a b)
  (or (and ($listp range)
           (setq tem (cdr range))
           (or (symbolp (car tem)) ($subvarp (car tem)))
           (numberp (setq a ($float (meval* (second tem)))))
           (numberp (setq b ($float (meval* (third tem)))))
           (< a b))
      (if range
          (merror 
           (intl:gettext "plotting: range must be of the form [variable, min, max]; found: ~M")
           range)
          (merror 
           (intl:gettext "plotting: no range given; must supply range of the form [variable, min, max]"))))
  `((mlist) ,(car tem) ,(float a) ,(float b)))

(defun $zero_fun (x y) x y 0.0)

(defun output-points (pl &optional m)
  "If m is supplied print blank line every m lines"
  (let ((j -1))
    (declare (fixnum j))
    (loop for i below (length (polygon-pts pl))
           with ar = (polygon-pts pl)
           do (print-pt (aref ar i))
           (setq i (+ i 1))
           (print-pt (aref ar i))
           (setq i (+ i 1))
           (print-pt (aref ar i))
           (terpri $pstream)
           (cond (m
                  (setq j (+ j 1))
                  (cond ((eql j (the fixnum m))
                         (terpri $pstream)
                         (setq j -1)))))
           )))

(defun show-open-plot (ans)
  (cond ($show_openplot
         (with-open-file (st1 (plot-temp-file "maxout.xmaxima") :direction :output :if-exists :supersede)
           (princ  ans st1))
         ($system (concatenate 'string *maxima-prefix* 
                                       (if (string= *autoconf-win32* "true") "\\bin\\" "/bin/") 
                                       $xmaxima_plot_command)
                  (format nil " \"~a\"" (plot-temp-file "maxout.xmaxima"))))
        (t (princ ans) "")))


; contour_plot -- set some parameters for Gnuplot and punt to plot3d
;
; We go to some trouble here to avoid clobbering the Gnuplot preamble
; specified by the user, either as a global option (via set_plot_option)
; or specified in arguments to contour_plot. Just append or prepend
; the parameters for contour plotting to the user-specified preamble.
; Assume that arguments take precedence over global options.
;
; contour_plot knows how to set parameters only for Gnuplot.
; If the plot_format is not a Gnuplot format, complain.
;
; Examples:
;
;   contour_plot (x^2 + y^2, [x, -4, 4], [y, -4, 4]);
;   contour_plot (sin(y) * cos(x)^2, [x, -4, 4], [y, -4, 4]);
;   F(x, y) := x^3 + y^2;
;   contour_plot (F, [u, -4, 4], [v, -4, 4]);
;   contour_plot (F, [u, -4, 4], [v, -4, 4], [gnuplot_preamble, "set size ratio -1"]);
;   set_plot_option ([gnuplot_preamble, "set cntrparam levels 12"]);
;   contour_plot (F, [u, -4, 4], [v, -4, 4]);
;   set_plot_option ([plot_format, xmaxima]);
;   contour_plot (F, [u, -4, 4], [v, -4, 4]); => error: must be gnuplot format
;   contour_plot (F, [u, -4, 4], [v, -4, 4], [plot_format, gnuplot]);

(defun $contour_plot (expr &rest optional-args)
  (let*
    ((plot-format-in-plot-options ($get_plot_option '$plot_format 2))
     (plot-format-in-arguments
       (let (($plot_options `((mlist) ,@optional-args))) ($get_plot_option '$plot_format 2)))

     (preamble-in-plot-options ($get_plot_option '$gnuplot_preamble 2))
     (preamble-in-arguments
       (let (($plot_options `((mlist) ,@optional-args))) ($get_plot_option '$gnuplot_preamble 2)))

     (contour-preamble "set contour; unset surface; set view map")
     (gnuplot-formats '($gnuplot $gnuplot_pipes)))

    ; Ensure that plot_format is some gnuplot format.
    ; Argument takes precedence over global option.

    (if
      (or
        (and plot-format-in-arguments
             (not (member plot-format-in-arguments gnuplot-formats :test #'eq)))
        (and (not plot-format-in-arguments)
             (not (member plot-format-in-plot-options gnuplot-formats :test #'eq))))

      (progn
        (mtell (intl:gettext "contour_plot: plot_format = ~a not recognized; must be a gnuplot format.~%")
               (ensure-string (or plot-format-in-arguments plot-format-in-plot-options)))
        (return-from $contour_plot))

      ; Prepend contour preamble to preamble in arguments (if given)
      ; and pass concatenated preamble as an argument to plot3d.
      ; Otherwise if there is a global option preamble, 
      ; append contour preamble to global option preamble.
      ; Otherwise just set global option preamble to the contour preamble.

      ; All this complication is to avoid clobbering the preamble
      ; if one was specified somehow (either global option or argument).

      (if preamble-in-arguments
        (let
          ((args-sans-preamble (remove-if #'(lambda (e) (and ($listp e) (eq ($first e) '$gnuplot_preamble))) optional-args)))
          (setq preamble-in-arguments
                ($sconcat contour-preamble (format nil "~%") preamble-in-arguments))
          (apply #'$plot3d
                 (append (list expr)
                         args-sans-preamble
			 (list '((mlist simp) $palette nil))
                         (list `((mlist) $gnuplot_preamble ,preamble-in-arguments)))))

        (let (($plot_options $plot_options))
        
          (if preamble-in-plot-options
            ($set_plot_option
              `((mlist) $gnuplot_preamble
                        ,($sconcat preamble-in-plot-options (format nil "~%")
				   contour-preamble)))
            ($set_plot_option `((mlist) $gnuplot_preamble ,contour-preamble)))
          (apply #'$plot3d 
		 (cons expr
		       (append optional-args
			       (list '((mlist simp) $palette nil))))))))))
 
;; plot3d
;;
;; Examples:
;; plot3d (2^(-u^2 + v^2), [u, -3, 3], [v, -2, 2], [palette, false]);
;;
;; plot3d ( log ( x^2*y^2 ), [x, -2, 2], [y, -2, 2], [grid, 29, 29]);
;;
;; expr_1: cos(y)*(10.0+6*cos(x))$
;; expr_2: sin(y)*(10.0+6*cos(x))$
;; expr_3: -6*sin(x)$
;; plot3d ([expr_1, expr_2, expr_3], [x, 0, 2*%pi], [y, 0, 2*%pi],
;;         ['grid, 40, 40], [z,-8,8]);
;;
;; plot3d (cos (-x^2 + y^3/4), [x, -4, 4], [y, -4, 4],
;; [mesh_lines_color, false], [elevation, 0], [azimuth, 0], [colorbox, true],
;; [grid, 150, 150])$

;; spherical: make_transform ([th, phi,r], r*sin(phi)*cos(th),
;;            r*sin(phi)*sin(th), r*cos(phi))$
;; plot3d ( 5, [th, 0, 2*%pi], [phi, 0, %pi], [transform_xy, spherical],
;;          [palette, [value, 0.65, 0.7, 0.1, 0.9]], [plot_format,xmaxima]);
;;
;; V: 1 / sqrt ( (x+1)^2+y^2 ) - 1 / sqrt( (x-1)^2+y^2 )$
;; plot3d ( V, [x, -2, 2], [y, -2, 2], [z, -4, 4])$


(defun $plot3d
    ( fun &rest options &aux
     lvars trans xrange yrange *original-points*
     functions exprn domain tem ($plot_options $plot_options)
     ($in_netmath $in_netmath) features
     (*plot-realpart* *plot-realpart*)
     gnuplot-term gnuplot-out-file file titles (output-file "")
     (usage (intl:gettext
"plot3d: Usage.
To plot a single function f of 2 variables v1 and v2:
  plot3d (f, [v1, min, max], [v2, min, max], options)
A parametric representation of a surface with parameters v1 and v2:
  plot3d ([f1, f2, f3], [v1, min, max], [v2, min, max], options)
Several functions depending on the two variables v1 and v2:
  plot3d ([f1, f2, ..., fn, [v1, min, max], [v2, min, max]], options)")))
  
  (declare (special *original-points*))
  (setf (getf features :type) "plot3d")
  
  ;; Ensure that fun is a list of expressions and maxima lists, followed
  ;; by a domain definition
  (if ($listp fun)
      (if (= 1 (length (check-list-plot3d fun)))
          ;; fun consisted of a single parametric expression
          (setq fun `(,fun ,(pop options) ,(pop options)))
          ;; fun was a maxima list with several independent surfaces
          (pop fun))
      ;; fun consisted of a single expression
      (setq fun `(,fun ,(pop options) ,(pop options))))
  
  ;; go through all the independent surfaces creating the functions stack
  (loop
     (setq exprn (pop fun))
     (if ($listp exprn)
         (progn
           (setq domain (check-list-plot3d exprn))
           (case (length domain)
             (1
              ;; exprn is a parametric representation of a surface
              (let (vars1 vars2 vars3)
                ;; list fun should have two valid ranges after exprn
                (setq xrange (check-range (pop fun)))
                (setq yrange (check-range (pop fun)))
                ;; list of the two variables for the parametric equations
                (setq lvars `((mlist),(second xrange) ,(second yrange)))
                ;; make sure that the 3 parametric equations depend only
                ;; on the two variables in lvars
                (setq vars1
                      ($listofvars (mfuncall
                                    (coerce-float-fun (second exprn) lvars)
                                    (second lvars) (third lvars))))
                (setq vars2
                      ($listofvars (mfuncall
                                    (coerce-float-fun (third exprn) lvars)
                                    (second lvars) (third lvars))))
                (setq vars3
                      ($listofvars (mfuncall
                                    (coerce-float-fun (fourth exprn) lvars)
                                    (second lvars) (third lvars))))
                (setq lvars ($listofvars `((mlist) ,vars1 ,vars2 ,vars3)))
                (if (<= ($length lvars) 2)
                    ;; we do have a valid parametric set. Push it into
                    ;; the functions stack, along with their domain
                    (progn
                      (push `(,exprn ,xrange ,yrange) functions)
                      ;; add a title to the titles stack
                      (push "Parametric function" titles)
                      ;; unknown variables in the parametric equations
                      ;; ----- GNUPLOT 4.0 WORK-AROUND -----
                      (when (and ($constantp (fourth exprn))
                                 ($get_plot_option '$gnuplot_4_0 2))
                        (setf (getf features :const-expr)
                              ($float (meval (fourth exprn))))))
                    (merror
                     (intl:gettext "plot3d: there must be at most two variables; found: ~M")
                     lvars))))
             
             (3
              ;; expr is a simple function with its own domain. Push the
              ;; function and its domain into the functions stack
              (setq xrange (second domain))
              (setq yrange (third domain))
              (push `(,(second exprn) ,xrange ,yrange) functions)
              ;; push a title for this plot into the titles stack
              (if (< (length (ensure-string (second exprn))) 36)
                  (push (ensure-string (second exprn)) titles)
                  (push "Function" titles)))
             
             (t
              ;; syntax error. exprn does not have the expected form
              (merror
               (intl:gettext "plot3d: argument must be a list of three expressions; found: ~M")
               exprn))))
         (progn
           ;; exprn is a simple function, defined in the global domain.
           (if (and (getf features :xvar) (getf features :yvar))
               ;; the global domain has already been defined; use it.
               (progn
                 (setq xrange `((mlist) ,(getf features :xvar)
                                ,(getf features :xmin) ,(getf features :xmax)))
                 (setq yrange `((mlist) ,(getf features :yvar)
                                ,(getf features :ymin) ,(getf features :ymax))))
               ;; the global domain should be defined by the last two lists
               ;; in fun. Extract it and check whether it is valid.
               (progn
                 (setq
                  domain
                  (check-list-plot3d (append `((mlist) ,exprn) (last fun 2))))
                 (setq fun (butlast fun 2))
                 (if (= 3 (length domain))
                     ;; it is a valid domain that should become the global domain.
                     (progn
                       (setq xrange (second domain))
                       (setq yrange (third domain))
                       (setf (getf features :xvar) (second xrange))
                       (setf (getf features :xmin) (third xrange))
                       (setf (getf features :xmax) (fourth xrange))
                       (setf (getf features :yvar) (second yrange))
                       (setf (getf features :ymin) (third yrange))
                       (setf (getf features :ymax) (fourth yrange)))
                     (merror usage))))
           ;; ----- GNUPLOT 4.0 WORK-AROUND -----
           (when (and ($constantp exprn)
                      ($get_plot_option '$gnuplot_4_0 2))
             (setf (getf features :const-expr) ($float (meval exprn))))
           ;; push the function and its domain into the functions stack
           (push `(,exprn ,xrange ,yrange) functions)
           ;; push a title for this plot into the titles stack
           (if (< (length (ensure-string exprn)) 36)
               (push (ensure-string exprn) titles)
               (push "Function" titles))))
     (when (= 0 (length fun)) (return)))
  
  ;; recover the original ordering for the functions and titles stacks
  (setq functions (reverse functions))
  (setq titles (reverse titles))
  
  ;; parse the options given to plot3d
  (setq features (plot-options-parser options features))
  (setq tem ($get_plot_option '$transform_xy 2))
  (setq *plot-realpart* ($get_plot_option '$plot_realpart 2))
  
  ;; set up the labels for the axes
  (when (or (null (getf features :box)) (first (getf features :box)))
    (if (and (getf features :xvar) (getf features :yvar) (null tem))
	(progn
	  ;; Don't set xlabel (ylabel) if the user specified one.
	  (unless (getf features :xlabel)
	    (setf (getf features :xlabel) (ensure-string (getf features :xvar))))
	  (unless (getf features :ylabel)
	    (setf (getf features :ylabel) (ensure-string (getf features :yvar)))))
	(progn
	  (setf (getf features :xlabel) "x")
	  (setf (getf features :ylabel) "y")))
    (unless (getf features :zlabel) (setf (getf features :zlabel) "z")))
  
  ;; x and y should not be bound, when an xy transformation function is used
  (when tem (remf features :xmin) (remf features :xmax)
        (remf features :ymin) (remf features :ymax))
  
  (setf gnuplot-term ($get_plot_option '$gnuplot_term 2))
  (if ($get_plot_option '$gnuplot_out_file 2)
      (setf gnuplot-out-file (get-plot-option-string '$gnuplot_out_file)))
  (if (and (eq (getf features :plot-format) '$gnuplot) 
           (eq gnuplot-term '$default) 
           gnuplot-out-file)
      (setf file gnuplot-out-file)
      (setf file 
            (plot-temp-file
             (format nil "maxout.~(~a~)"
                     (ensure-string (getf features :plot-format))))))
  (and $in_netmath 
       (setq $in_netmath (eq (getf features :plot-format) '$xmaxima)))
  
  ;; Set up the output file stream
  (let (($pstream
         (cond ($in_netmath *standard-output*)
               (t (open file :direction :output :if-exists :supersede))))
        (legend (getf features :legend)) (n (length functions)))
    ;; titles will be a maxima list. The titles given in the legend option
    ;; will have priority over the titles generated by plot3d.
    ;; no legend if option [legend,false]
    (when (first legend) (setq titles legend))

    (unwind-protect
         (case (getf features :plot-format)
           ($gnuplot
            (gnuplot-print-header $pstream features)
            (format $pstream "~a" (gnuplot-plot3d-command "-" titles n)))
           ($gnuplot_pipes
            (setq output-file (check-gnuplot-process))
            ($gnuplot_reset)
            (gnuplot-print-header *gnuplot-stream* features)
            (setq *gnuplot-command* (gnuplot-plot3d-command file titles n)))
           ($xmaxima
            (xmaxima-print-header $pstream features))
           ($geomview
            (format $pstream "LIST~%")))
      
      ;; generate the mesh points for each surface in the functions stack
      (let ((i 0))
        (dolist (f functions)
          (setq i (+ 1 i))
          (setq fun (first f))
          (setq xrange (second f))
          (setq yrange (third f))
          (if ($listp fun)
              (progn
                (setq trans
                      ($make_transform `((mlist) ,(second xrange)
                                         ,(second yrange) $z)
                                       (second fun) (third fun) (fourth fun)))
                (setq fun '$zero_fun))
              (progn
                (setq lvars `((mlist) ,(second xrange) ,(second yrange)))
                (setq fun (coerce-float-fun fun lvars))
                (when (cdr
                       ($delete
                        (second lvars)
                        ($delete
                         (third lvars)
                         ($listofvars (mfuncall fun (second lvars) (third lvars))))))
                  (mtell (intl:gettext "plot3d: expected <expr. of v1 and v2>, [v1, min, max], [v2, min, max]~%"))
                  (mtell (intl:gettext "plot3d: keep going and hope for the best.~%")))))
          (let* ((pl
                  (draw3d
                   fun (third xrange) (fourth xrange) (third yrange)
                   (fourth yrange) (third (getf features :grid))
                   (fourth (getf features :grid))))
                 (ar (polygon-pts pl)))
            (declare (type (cl:array t) ar))
            
            (if trans (mfuncall trans ar))
            (if tem (mfuncall tem ar))
            
            (case (getf features :plot-format)
              ($gnuplot
               (when (> i 1) (format $pstream "e~%"))
               (output-points pl (third (getf features :grid))))
              ($gnuplot_pipes
               (when (> i 1) (format $pstream "~%~%"))
               (output-points pl (third (getf features :grid))))
              ($mgnuplot
               (when (> i 1) (format st "~%~%# \"Fun~a\"~%" i))
               (output-points pl (third (getf features :grid))))
              ($xmaxima
               (output-points-tcl $pstream pl (third (getf features :grid)) i))
              ($geomview
               (format $pstream "{ appearance { +smooth }~%MESH ~a ~a ~%"
                       (+ 1 (third (getf features :grid)))
                       (+ 1 (fourth (getf features :grid))))
               (output-points pl nil)
               (format $pstream "}~%"))))))
      
      ;; close the stream and plot..
      (cond ($in_netmath (format $pstream "}~%") (return-from $plot3d ""))
            ((eql (getf features :plot-format) '$xmaxima)
             (format $pstream "}~%")
             (close $pstream))
            (t (close $pstream)
               (setq $pstream nil))))
    (if (eql (getf features :plot-format) '$gnuplot)
        (gnuplot-process file)
        (cond (($get_plot_option '$run_viewer 2)
               (case (getf features :plot-format)
                 ($xmaxima
                  ($system
                   (concatenate
                    'string *maxima-prefix* 
                    (if (string= *autoconf-win32* "true") "\\bin\\" "/bin/")
                    $xmaxima_plot_command) 
                   (format nil " \"~a\"" file)))
                 ($geomview 
                  ($system $geomview_command
                           (format nil " \"~a\"" file)))
                 ($gnuplot_pipes
                  (send-gnuplot-command *gnuplot-command*))
                 ($mgnuplot 
                  ($system
                   (concatenate
                    'string *maxima-plotdir* "/" $mgnuplot_command)
                   (format nil " -parametric3d \"~a\"" file))))))))
  output-file)

;; Given a Maxima list with 3 elements, checks whether it represents a function
;; defined in a 2-dimensional domain or a parametric representation of a
;; 3-dimensional surface, depending on two parameters.
;; The return value will be a Maxima list if the test is succesfull or nil
;; otherwise.
;; In the case of a function and a domain it returns the domain.
;; When it is a parametric representation it returns an empty Maxima list.
;;
(defun check-list-plot3d (lis)
  (let (xrange yrange)
    ;; wrong syntax: lis must be [something, something, something]
    (unless ($listp lis) (return-from check-list-plot3d nil))
    (unless (= 3 ($length lis)) (return-from check-list-plot3d nil))
    (if ($listp (second lis))
        ;; wrong syntax: [list, something, something]
        (return-from check-list-plot3d nil)
        ;; we might have a function with domain or a parametric representation
        (if ($listp (third lis))
            ;; lis is probably a function with a valid domain
            (if ($listp (fourth lis))
                ;; we do have a function and a domain. Return the domain
                (progn
                  (setq xrange (check-range (third lis)))
                  (setq yrange (check-range (fourth lis)))
                  (return-from check-list-plot3d `((mlist) ,xrange ,yrange)))
                ;; wrong syntax: [expr1, list, expr2]
                (return-from check-list-plot3d nil))
            ;; lis is probably a parametric representation
            (if ($listp (fourth lis))
                ;; wrong syntax: [expr1, expr2, list]
                (return-from check-list-plot3d nil)
                ;; we do have a parametric representation. Return an empty list
                (return-from check-list-plot3d '((mlist))))))))
