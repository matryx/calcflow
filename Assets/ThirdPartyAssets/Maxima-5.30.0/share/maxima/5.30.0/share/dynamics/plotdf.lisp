;; plotdf.mac - Adds a function plotdf() to Maxima, which draws a Direction
;;              Field for an ordinary 1st order differential equation,
;;              or for a system of two autonomous 1st order equations.
;;   
;; Copyright (C) 2004, 2008, 2011 Jaime E. Villate <villate@fe.up.pt>
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
;; MA  02110-1301, USA.
;;
;; See plotdf.usg (which should come together with this program) for
;; a usage summary
;;
;; $Id: plotdf.lisp,v 1.12 2011-03-09 11:33:46 villate Exp $

(in-package :maxima)

;; parses a plotdf option into a command-line option for tcl scripts
(defun plotdf-option-to-tcl (value s1 s2)
  (let (vv)
    (unless (and  ($listp value)
                  (symbolp (setq name (second value))))
      (merror
       (intl:gettext "plotdf-option-to-tcl: ~M is not a plotdf option. Should be [symbol,...data]")
       value))
    (setq value
      (case name
        (($xradius $yradius $xcenter $ycenter $tinitial $tstep)
         (check-list-items name (rest (rest value)) 'number 1))
        (($width $height $nsteps $versus_t)
         (check-list-items name (rest (rest value)) 'fixnum 1))
        ($trajectory_at
         (check-list-items name (rest (rest value)) 'number 2))
        ($bbox (check-list-items name (rest (rest value)) 'number 4))
        (($xfun $parameters $sliders $vectors $fieldlines $curves 
		$windowtitle $xaxislabel $yaxislabel $psfile) value)
	($axes
	 (if (not (third value))
	     (setq value '((mlist) $axes 0))  
	   (case (third value)
		 ($x (setq value '((mlist) $axes "x")))
		 ($y (setq value '((mlist) $axes "y")))
		 (t (setq value '((mlist) $axes "xy"))))))
	($box
	 (if (not (third value))
	     (setq value '((mlist) $nobox 1))
	   (setq value '((mlist) $nobox 0))))
        ($direction
         (or
          (member (ensure-string (third value)) '("forward" "backward" "both") :test #'equal)
          (merror
           (intl:gettext "plotdf-option-to-tcl: direction should be forward, backward or both."))) 
         value)
        (t (cond
            ((eql name s1)
	     (setq value (check-range value))
             (check-list-items '$x (rest (rest value)) 'number 2))
            ((eql name s2)
	     (setq value (check-range value))
             (check-list-items '$y (rest (rest value)) 'number 2))
            (t (merror (intl:gettext "plotdf-option-to-tcl: unknown option ~M") name))))))
    (setq vv (mapcar #'(lambda (a) (if (symbolp a) (ensure-string a) a)) (cdr value)))
    (with-output-to-string (st)
      (cond ((or (equal (first vv) "x") (equal (first vv) "y"))
             (format st "-~(~a~)center " (first vv))
             (format st "{~a} " (/ (+ (third vv) (second vv)) 2))
             (format st "-~(~a~)radius " (first vv))
             (format st "{~a}" (/ (- (third vv) (second vv)) 2)))
            (t
             (format st "-~(~a~) " (first vv))
             (format st "{~{~a~^ ~}}" (rest vv)))))))

;; applies float(ev(expression, numer)) to an expression, and returns a string

(defun expr_to_str (fun)
  (mstring (mfuncall '$float (mfuncall '$ev fun '$numer))))

;; plots the direction field for an ODE  dy/dx = f(x,y), or for an autonomous
;; system of 2 equations dx/dt = f(x,y), dy/dt = g(x,y) 
;;
(defun $plotdf (ode &rest options)  
  (let (cmd (opts " ") (s1 '$x) (s2 '$y))
    (unless ($listp ode) (setf ode `((mlist) ,ode)))

    ;; parse arguments and prepare string cmd with the equation(s)
    (unless
	(member (second (first options))
		'($xradius $yradius $xcenter $ycenter $tinitial $tstep
			   $width $height $nsteps $versus_t $xfun $parameters
			   $sliders $vector $trajectory_at $orthogonal))
      (if (and (listp (first options)) (= (length (first options)) 3)
	       (symbolp (second (first options)))
	       (symbolp (third (first options))))
	  (progn
	    (setf s1 (second (first options)))
	    (setf s2 (third (first options)))))
      (setf options (cdr options)))

    ;; parse options and copy them to string opts
    (cond (options
           (dolist (v options) 
             (setq opts (concatenate 'string opts " "
                                  (plotdf-option-to-tcl v s1 s2))))))

    (unless (search "-xaxislabel " opts)
      (setq opts (concatenate 'string opts " -xaxislabel " (ensure-string s1))))
    (unless (search "-yaxislabel " opts)
      (setq opts (concatenate 'string opts " -yaxislabel " (ensure-string s2))))

    ;; check that the expressions given contain only the axis variables
    (unless (search "-parameters " opts)
      (dolist (var (cdr (mfuncall '$listofvars ode)))
        (unless (or (eq var s1) (eq var s2))
          (merror 
           (intl:gettext "plotdf: expression(s) given can only depend on ~M and ~M~%Found extra variable ~M") s1 s2 var))))

    ;; substitute $x by s1 and $y by s2
    (defun subxy (expr)
      (if (listp expr)
          (mapcar #'subxy expr)
        (cond ((eq expr s1) '$x) ((eq expr s2) '$y) (t expr))))
    (setf ode (mapcar #'subxy ode))

    ;; parse the differential equation expressions
    (case (length ode)
          (3 (setq cmd (concatenate 'string " -dxdt \""
                                    (expr_to_str (second ode)) "\" -dydt \""
                                    (expr_to_str (third ode)) "\"")))
          (2 (setq cmd (concatenate 'string " -dydx \""
                                    (expr_to_str (second ode)) "\"")))
          (t (merror 
              (intl:gettext "plotdf: first argument must be either an expression or a list with two expressions."))))
    
    (show-open-plot
     (with-output-to-string (st)
                  (cond ($show_openplot (format st "plotdf ~a ~a~%" cmd opts))
                              (t (format st "{plotdf ~a ~a} " cmd opts)))))))

;; plot equipotential curves for a scalar field f(x,y)
(defun $ploteq (fun &rest options)
  
  (let (cmd mfun (opts " ") (s1 '$x) (s2 '$y))
    (setf mfun `((mtimes) -1 ,fun))
    ;; parse arguments and prepare string cmd with the equation(s)
    (unless
	(member (second (first options))
		'($xradius $yradius $xcenter $ycenter $tinitial $tstep
			   $width $height $nsteps $versus_t $xfun $parameters
			   $sliders $vector $trajectory $orthogonal))
      (if (and (listp (first options)) (= (length (first options)) 3)
	       (symbolp (second (first options)))
	       (symbolp (third (first options))))
	  (progn
	    (setf s1 (second (first options)))
	    (setf s2 (third (first options)))
	    (defun subxy (expr)
	      (if (listp expr)
		  (mapcar #'subxy expr)
		(cond ((eq expr s1) '$x) ((eq expr s2) '$y) (t expr))))
	    (setf mfun (mapcar #'subxy mfun))
	    (setf options (cdr options)))))
;; the next two lines should take into account parameters given in the options
;;    (if (delete '$y (delete '$x (rest (mfuncall '$listofvars ode))))
;;        (merror "The equation(s) can depend only on 2 variable which must be specified!"))
    (setq cmd (concatenate 'string " -dxdt \""
			   (expr_to_str (mfuncall '$diff mfun '$x))
			   "\" -dydt \""
			   (expr_to_str (mfuncall '$diff mfun '$y)) 
			   "\" "))
    
    ;; parse options and copy them to string opts
    (cond (options
           (dolist (v options) 
             (setq opts (concatenate 'string opts " "
                                  (plotdf-option-to-tcl v s1 s2))))))

    (unless (search "-vectors " opts)
      (setq opts (concatenate 'string opts " -vectors {}")))
    (unless (search "-fieldlines " opts)
      (setq opts (concatenate 'string opts " -fieldlines {}")))
    (unless (search "-curves " opts)
      (setq opts (concatenate 'string opts " -curves {red}")))
    (unless (search "-windowtitle " opts)
      (setq opts (concatenate 'string opts " -windowtitle {Ploteq}")))
    (unless (search "-xaxislabel " opts)
      (setq opts (concatenate 'string opts " -xaxislabel " (ensure-string s1))))
    (unless (search "-yaxislabel " opts)
      (setq opts (concatenate 'string opts " -yaxislabel " (ensure-string s2))))
							      
    (show-open-plot
     (with-output-to-string (st)
                  (cond ($show_openplot (format st "plotdf ~a ~a~%" cmd opts))
                              (t (format st "{plotdf ~a ~a}" cmd opts)))))))
