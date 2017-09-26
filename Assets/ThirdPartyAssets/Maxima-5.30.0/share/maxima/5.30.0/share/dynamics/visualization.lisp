;; visualization.lisp
;;   
;; Copyright (c) 2011, Jaime E. Villate <villate@fe.up.pt>
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
;; $Id: visualization.lisp,v 1.1 2011-03-09 11:32:39 villate Exp $

(in-package :maxima)

(defun tcl-output-number-list (maxlist)
  (with-output-to-string (st)
    (format st "{ ")
    (dolist (num (rest maxlist))
      (cond
        ((floatp num) (format st "~f " num))
        (($numberp num) (format st "~f " (coerce-float num)))
        ((and ($constantp num) ($freeof '$%i num) (not (member num '(t nil)))
              (not ($listp num)))
         (format st "~f " (coerce-float num)))
        (($listp num) (format st "~a" (tcl-output-number-list num)))
        (t (merror "Wrong argument for object: ~M" num))))
    (format st "} ")))

;; parses a scene object into text for tcl scripts passed to Xmaxima
(defun scene-object-to-tcl (value)
  (let (vv)
    (unless (and  ($listp value)
                  (symbolp (setq name (second value))))
      (merror
       (intl:gettext "~M is not an object option. Must be [property,value]")
       value))
    (setq value
      (case name
        (($azimuth $elevation $tstep)
         (check-list-items name (rest (rest value)) 'number 1))
        (($width $height $restart)
         (check-list-items name (rest (rest value)) 'fixnum 1))
        ($background
         (check-list-items name (rest (rest value)) 'number 3))
        (($windowtitle $windowname $animate) value)
        (t (merror (intl:gettext "Unknown property ~M") name))))
    (setq vv (mapcar #'stripdollar (rest value)))
    (with-output-to-string (st)
      (format st "-~(~a~) " (first vv))
      (format st "{~{~a~^ ~}}" (rest vv)))))

;; parses a scene option into a command-line option passed to Xmaxima
(defun scene-option-to-tcl (value)
  (let (vv)
    (unless (and  ($listp value)
                  (symbolp (setq name (second value))))
      (merror
       (intl:gettext "~M is not a scene option. Must be [symbol,...data]")
       value))
    (setq value
      (case name
        (($azimuth $elevation $tstep)
         (check-list-items name (rest (rest value)) 'number 1))
        (($width $height $restart)
         (check-list-items name (rest (rest value)) 'fixnum 1))
        ($background
         (check-list-items name (rest (rest value)) 'number 3))
        (($windowtitle $windowname $animate) value)
        (t (merror (intl:gettext "Unknown option ~M") name))))
    (setq vv (mapcar #'stripdollar (rest value)))
    (with-output-to-string (st)
      (format st "-~(~a~) " (first vv))
      (format st "{~{~a~^ ~}}" (rest vv)))))

(defun $scene (objects &rest options)
  (let ((objs "") (opts " ") name vtkname prop (lf (format NIL "~%"))
        (classes '(($cube . "Cube") ($sphere . "Sphere")
                     ($cylinder . "Cylinder") ($cone . "Cone")))
        ;; VTK methods for the objects in classes
        (cmethods '(($center . "Center") ($radius . "Radius")
                    ($height . "Height") ($resolution . "Resolution")
                    ($latlongtessellation . "LatLongTessellation")
                    ($thetaresolution . "ThetaResolution")
                    ($phiresolution . "PhiResolution")
                    ($starttheta . "StartTheta") ($endtheta . "EndTheta")
                    ($startphi . "StartTheta") ($endphi . "EndTheta")
                    ($capping . "Capping") ($direction . "Direction")
                    ($xlength . "XLength") ($ylength . "YLength")
                    ($zlength . "ZLength") ($bounds . "Bounds")
                    ($angle . "Angle")))
        ;; VTK methods for properties
        (pmethods '(($color . "Color") ($opacity . "Opacity")
                    ($ambient . "Ambient") ($ambientcolor . "AmbientColor")
                    ($specular . "Specular") ($specularcolor . "SpecularColor") 
                    ($diffuse . "Diffuse") ($diffusecolor . "DiffuseColor")
                    ($edgevisibility . "EdgeVisibility")
                    ($edgecolor . "EdgeColor") ($linewidth . "LineWidth")
                    ($pointsize . "PointSize") ($lightning . "Lightning")
                    ($shading . "Shading") ($texture . "Texture")
                    ($representation . "Representation")
                    ($points . "RepresentationToPoints")
                    ($wireframe . "RepresentationToWireframe")
                    ($surface . "RepresentationToSurface")
                    ($interpolation . "Interpolation")
                    ($flat . "InterpolationToFlat")
                    ($gourand . "InterpolationToGourand")
                    ($phong . "InterpolationToPhong")
                    ($stipplepattern . "LineStipplePattern")
                    ($stipplerepeat . "LineStippleRepeatFactor")
                    ($frontculling . "FrontFaceCulling")
                    ($backculling . "BackFaceCulling")))
        ;; VTK methods for actors
        (amethods '(($origin . "Origin") ($scale . "Scale")
                    ($position . "Position") ($orientation . "Orientation")
                    ($usertransform . "UserTransform"))))
    ;; prepare list of objects
    (if ($listp objects)
        (if ($listp (second objects))
            (setq objects (rest objects)) (setq objects `(,objects)))
        (merror
         (intl:gettext
          "First argument should be an object or a list of objects")))
    ;; parse objects
    (dolist (v objects)
      (let ((copts "") (popts "") (aopts "") animate)
        (unless (and ($listp v) (symbolp (setq name (second v))))
          (merror
           (intl:gettext "~M is not an object. Expecting [class, options]")
           v))
        (unless (setq vtkname (cdr (assoc name classes)))
          (merror (intl:gettext "Unknown object class: ~M") name))
        ;; parse object properties 
        (dolist (w (cddr v))
          (unless ($listp w)
            (merror
             (intl:gettext "Wrong option format; expecting a list, found  ~M")
             w))
          (cond
            ((setq prop (cdr (assoc (second w) cmethods)))
             (setq copts
                   (concatenate 'string copts "{" prop
                                (format NIL "~{ ~a~}" (cddr w)) "} ")))
            ((setq prop (cdr (assoc (second w) pmethods)))
             (setq popts
                   (concatenate 'string popts "{" prop
                                (format NIL "~{ ~a~}" (cddr w)) "} ")))
            ((setq prop (cdr (assoc (second w) amethods)))
             (setq aopts
                   (concatenate 'string aopts "{" prop
                                (format NIL "~{ ~a~}" (cddr w)) "} ")))
            ((eql (second w) '$animate)
             (unless (setq prop (cdr (assoc (third w) amethods)))
               (merror (intl:gettext "~M cannot be animated.") (third w)))
             (setq animate
                   (concatenate 'string "{" prop " 0 "
                          (tcl-output-number-list (fourth w)) "}")))
            ((eql (second w) '$track)
             (setq animate
                   (concatenate 'string "{Position 1 "
                          (tcl-output-number-list (third w)) "}")))
            (t (mtell (intl:gettext "Ignored option: ~M") (second w)))))
        ;; save object name and properties in string objs
        (setq objs 
              (concatenate 'string objs "{" vtkname lf "{" copts "}" lf
                           "{" popts "}" lf "{" aopts "}" lf animate "}" lf))))

    ;; parse scene options and copy them to string opts
    (cond (options
           (dolist (v options) 
             (setq opts (concatenate 'string opts " "
                                     (scene-option-to-tcl v))))))
    (show-open-plot
     (with-output-to-string (st)
                  (cond ($show_openplot
                         (format st "scene ~a -objects {~a}~%" opts objs))
                        (t (format st "{scene ~a -objects {~a}}" opts objs)))))))

