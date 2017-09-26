;; complex_dynamics.lisp - functions julia, mandelbrot and rk
;;   
;; Copyright (C) 2006 Jaime E. Villate <villate@fe.up.pt>
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
;; $Id: complex_dynamics.lisp,v 1.1 2007-04-01 22:46:42 villate Exp $

(in-package :maxima)

;; Function hsv2rgb transforms a color specification triplet
;; (hue,saturation,value), into RGB values. Hue should be an integer (angle
;; in degrees), while saturation and value should be floating-point numbers
;; between 0 and 1.
;; The RGB values returned are six hexadecimal digits: two digits for R,
;; followed by two digits for G and two digits for B.
 
(defun hsv2rgb(hue sat val)
  (let (v rgb h i f u p q r)
  (setq v (* val 255))
  (if (= sat 0)
      (setq rgb (+ v (* 256 (+ v (* 256 v)))))     ; achromatic case
    (progn
      (setq h (/ (mod hue 360) 60))                ; 0<= h <6
      (setq i (truncate h))                        ; largest integer <= h
      (setq f (- h i))                             ; fractional part of h  
      (setq u (round v))                           ; u, p, q and r are all
      (setq p (round (* v (- 1 sat))))             ; integers between 0
      (setq q (round (* v (- 1 (* sat f)))))       ; and 255
      (setq r (round (* v (- 1 (* sat (- 1 f))))))
      (cond
       ((eql i 0) (setq rgb (+ p (* 256 (+ r (* 256 u))))))     ; red
       ((eql i 1) (setq rgb (+ p (* 256 (+ u (* 256 q))))))     ; yellow
       ((eql i 2) (setq rgb (+ r (* 256 (+ u (* 256 p))))))     ; green
       ((eql i 3) (setq rgb (+ u (* 256 (+ q (* 256 p))))))     ; cyan
       ((eql i 4) (setq rgb (+ u (* 256 (+ p (* 256 r))))))     ; blue
       ((eql i 5) (setq rgb (+ q (* 256 (+ p (* 256 u)))))))))  ; magenta
  (format nil "~6,'0x" (round rgb))))

;; Function $mandelbrot creates an XPM graphic file with the Mandelbrot set
;; on the region: xmin < x <xmax, ymin < y <ymax.
;; The input argument m must be a positive integer, which defines the number
;; of iterations to be made, before considering a point as part of the set.

(defun $mandelbrot(&rest options)
(let (a b c d e colorbytes colorcodes colors line num
      dx dy x xmax xmin y ymax ymin
      (xcenter 0.0) (ycenter 0.0) (radius 2.0)
      (m 12) (nx 400) (ny 400) (file "mandelbrot.xpm")
      (huerange 360) (hue0 -60) (sat 0.76) (val 0.96) (color "000000")
      ($numer t) (optname nil)
      (letters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

  (dolist (option options)
    (setf optname (second option))
    (cond
     ((eql optname '$size)
      (if (> (length option) 3)
	  (progn (setq nx (third option)) (setq ny (fourth option)))
	  (progn (setq nx (third option)) (setq ny nx))))
     ((eql optname '$levels) (setq m (third option)))
     ((eql optname '$huerange) (setq huerange (third option)))
     ((eql optname '$hue) (setq hue0 (third option)))
     ((eql optname '$saturation) (setq sat (third option)))
     ((eql optname '$value) (setq val (third option)))
     ((eql optname '$color)
      (setq color (hsv2rgb (third option) (fourth option) (fifth option))))
     ((eql optname '$center)
      (setq xcenter (coerce-float (third option)))
      (setq ycenter (coerce-float (fourth option)))) 
     ((eql optname '$radius) (setq radius (coerce-float (third option))))
     ((eql optname '$filename)
      (setq file (format nil "~a.xpm"
                     (print-invert-case (stripdollar (third option))))))))

  (setq xmin (- xcenter radius)) 
  (setq xmax (+ xcenter radius)) 
  (setq ymin (- ycenter radius)) 
  (setq ymax (+ ycenter radius)) 
  (setq dx (/ (float (- xmax xmin)) nx))   ; x displacement per pixel
  (setq dy (/ (float (- ymax ymin)) ny))   ; y displacement per pixel

  ;; setup color tables
  (if (> m 52) (setq colorbytes 2) (setq colorbytes 1))
  (setq colors (list color))
  (setq colorcodes
	(cond ((eql colorbytes 1) '(".")) ((eql colorbytes 2) '(".."))))
  (dotimes (i m)
    (push (hsv2rgb (+ hue0 (* huerange (- 1 (/ i (- m 1))))) sat val) colors)
    (push (cond
	   ((eql colorbytes 1) (string (char letters i)))
	   ((eql colorbytes 2) (format nil "~a~a" (char letters (floor i 50))
				       (char letters (mod i 50)))))
	  colorcodes))

  (with-open-file (st file :direction :output :if-exists :supersede)
  ;; print XPM file header
    (format st "/* XPM */~%static char *mandelbrot[] = {~%")
    (format st "\"~a ~a ~a ~a\",~%" nx ny (+ m 1) colorbytes)

  ;; print colors table
    (dotimes (i (+ m 1))            
      (format st "\"~a c #~a\",~%" (nth i colorcodes) (nth i colors)))

  ;; iteration through all grid points
    (dotimes (i ny) 
      (setq y (+ ymin (* (- ny (+ i 1)) dy)))
      (setq line "")
      (dotimes (j nx)
	(setq x (+ xmin (* j dx)))
	(setq a 0) (setq b 0)
	(setq num m)
	(dotimes (l m)
	  (setq c (* a a))
	  (setq d (* b b))
	  (setq e (* 2 a b))
	  (when (> (+ c d) 4) (progn (setq num l) (return)))
	  (setq a (+ x (- c d)))
	  (setq b (+ y e)))
	(setq line (concatenate 'string line (nth num colorcodes))))
      (if (= i (- ny 1))
	  (format st "\"~a\"~%" line) (format st "\"~a\",~%" line)))
    (format st "};~%"))
  (format t "File ~a was created.~%" file)))

;; Function $julia creates an XPM graphic file with the Julia set for the
;; point (x,y) of the complex plane, on the region: xmin < x <xmax,
;; ymin < y <ymax.
;; The input argument m must be a positive integer, which defines the number
;; of iterations to be made, before considering a point as part of the set.

(defun $julia(x y &rest options)
(let (a b b0 c d e colorbytes colorcodes colors line num
      dx dy xmax xmin ymax ymin
      (xcenter 0.0) (ycenter 0.0) (radius 2.0)
      (m 12) (nx 400) (ny 400) (file "julia.xpm")
      (huerange 360) (hue0 -60) (sat 0.76) (val 0.96) (color "000000")
      ($numer t) (optname nil)
      (letters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

  (dolist (option options)
    (setf optname (second option))
    (cond
     ((eql optname '$size)
      (if (> (length option) 3)
	  (progn (setq nx (third option)) (setq ny (fourth option)))
	  (progn (setq nx (third option)) (setq ny nx))))
     ((eql optname '$levels) (setq m (third option)))
     ((eql optname '$huerange) (setq huerange (third option)))
     ((eql optname '$hue) (setq hue0 (third option)))
     ((eql optname '$saturation) (setq sat (third option)))
     ((eql optname '$value) (setq val (third option)))
     ((eql optname '$color)
      (setq color (hsv2rgb (third option) (fourth option) (fifth option))))
     ((eql optname '$center)
      (setq xcenter (coerce-float (third option)))
      (setq ycenter (coerce-float (fourth option)))) 
     ((eql optname '$radius) (setq radius (coerce-float (third option))))
     ((eql optname '$filename)
      (setq file (format nil "~a.xpm"
                     (print-invert-case (stripdollar (third option))))))))

  (setq xmin (- xcenter radius)) 
  (setq xmax (+ xcenter radius)) 
  (setq ymin (- ycenter radius)) 
  (setq ymax (+ ycenter radius)) 
  (setq dx (/ (float (- xmax xmin)) nx))   ; x displacement per pixel
  (setq dy (/ (float (- ymax ymin)) ny))   ; y displacement per pixel

  ;; setup color tables
  (if (> m 52) (setq colorbytes 2) (setq colorbytes 1))
  (setq colors (list color))
  (setq colorcodes
	(cond ((eql colorbytes 1) '(".")) ((eql colorbytes 2) '(".."))))
  (dotimes (i m)
    (push (hsv2rgb (+ hue0 (* huerange (- 1 (/ i (- m 1))))) sat val) colors)
    (push (cond
	   ((eql colorbytes 1) (string (char letters i)))
	   ((eql colorbytes 2) (format nil "~a~a" (char letters (floor i 50))
				       (char letters (mod i 50)))))
	  colorcodes))

  (with-open-file (st file :direction :output :if-exists :supersede)
  ;; print XPM file header
    (format st "/* XPM */~%static char *mandelbrot[] = {~%")
    (format st "\"~a ~a ~a ~a\",~%" nx ny (+ m 1) colorbytes)

  ;; print colors table
    (dotimes (i (+ m 1))            
      (format st "\"~a c #~a\",~%" (nth i colorcodes) (nth i colors)))

  ;; iteration through all grid points
    (dotimes (i ny) 
      (setq b0 (+ ymin (* (- ny (+ i 1)) dy)))
      (setq line "")
      (dotimes (j nx)
	(setq a (+ xmin (* j dx)))
	(setq b b0)
	(setq num m)
	(dotimes (l m)
	  (setq c (* a a))
	  (setq d (* b b))
	  (setq e (* 2 a b))
	  (when (> (+ c d) 4) (progn (setq num l) (return)))
	  (setq a (+ x (- c d)))
	  (setq b (+ y e)))
	(setq line (concatenate 'string line (nth num colorcodes))))
      (if (= i (- ny 1))
	  (format st "\"~a\"~%" line) (format st "\"~a\",~%" line)))
    (format st "};~%"))
  (format t "File ~a was created.~%" file)))

;; Function $rk implements the 4th order Runge-Kutta numerical method.
;;  exprs:   an expression or maxima list with n expressions
;;  vars:    name of (or list of names of) the independent variable(s)
;;  initial: initial value for the variable (or list of initial values)
;;  domain:  maxima list with four elements (name of the independent
;;           variable, its initial and final values and its increment)
(defun $rk (exprs vars initial domain
             &aux d u fun k1 k2 k3 k4 r1 r2 r3 traj r
             (it (mapcar #'coerce-float (cddr domain))))
  (unless ($listp exprs) (setq exprs `((mlist) ,exprs)))
  (unless ($listp initial) (setq initial `((mlist) ,initial)))
  (unless ($listp vars) (setq vars `((mlist) ,vars)))
  (dolist (var (cdr vars))
    (unless (symbolp var)
      (merror (intl:gettext "rk: variable name expected; found: ~M") var)))
  (unless (symbolp (cadr domain))
      (merror (intl:gettext "rk: variable name expected; found: ~M")
              (cadr domain)))
  (setq vars (concatenate 'list '((mlist)) (list (cadr domain)) (cdr vars)))
  (setq r (concatenate 'list `(,(car it)) (mapcar #'coerce-float (cdr initial))))
  (setq fun (mapcar #'(lambda (x) (coerce-float-fun x vars)) (cdr exprs)))
  (setq d (floor (/ (- (cadr it) (car it)) (caddr it))))
  (setq traj (list (cons '(mlist) r)))
  (dotimes (m d)
    (ignore-errors
      (setq k1 (mapcar #'(lambda (x) (apply x r)) fun))
      (setq r1 (map 'list #'+ (cdr r) (mapcar #'(lambda (x) (* (/ (caddr it) 2) x)) k1)))
      (push (+ (car r) (/ (caddr it) 2)) r1)
      (setq k2 (mapcar #'(lambda (x) (apply x r1)) fun))
      (setq r2 (map 'list #'+ (cdr r) (mapcar #'(lambda (x) (* (/ (caddr it) 2) x)) k2)))
      (push (+ (car r) (/ (caddr it) 2)) r2)
      (setq k3 (mapcar #'(lambda (x) (apply x r2)) fun))
      (setq r3 (map 'list #'+ (cdr r) (mapcar #'(lambda (x) (* (caddr it) x)) k3)))
      (push (+ (car r) (caddr it)) r3)
      (setq k4 (mapcar #'(lambda (x) (apply x r3)) fun))
      (setq u (map 'list #'+
                   (mapcar #'(lambda (x) (* 1/6 x)) k1)
                   (mapcar #'(lambda (x) (* 1/3 x)) k2)
                   (mapcar #'(lambda (x) (* 1/3 x)) k3)
                   (mapcar #'(lambda (x) (* 1/6 x)) k4)))
      (setq r 
        (concatenate 'list
           `(,(* (1+ m) (caddr it)))
            (map 'list #'+ (cdr r) (mapcar #'(lambda (x) (* (caddr it) x)) u))))
      (push (cons '(mlist) r) traj)))
  (cons '(mlist) (nreverse traj)))
