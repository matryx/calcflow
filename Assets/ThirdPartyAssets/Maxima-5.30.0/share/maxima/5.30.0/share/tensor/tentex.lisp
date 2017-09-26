;;; -*- Mode:LISP; Package:MACSYMA -*-
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; Comments: TeX formatting module for itensor.lisp
;;

(in-package :maxima)

;; top level command the result of tex'ing the expression x.
;; Lots of messing around here to get C-labels verbatim printed
;; and function definitions verbatim "ground"

;(defmspec $tex(l) ;; mexplabel, and optional filename
;  (let ((args (cdr l)))
;  (apply 'tex1  args)))
(defprop $dl "\\Delta" texword)
(defprop $divg "(\\nabla\\cdot G)" texword)
(defprop $og "(\\Omega\\cdot G)" texword)
(defprop $dl_1 "\\Delta^{\\star}" texword)
(defprop $dl_c "\\Delta_{\\chi}" texword)
(defprop $omega "\\Omega" texword)
(defprop $om "\\Omega^{\\star}" texword)
(defprop $dlt "\\delta" texword)
(defprop $phi  "\\Phi" texword)
(defprop $d_t  "\\frac{\\partial}{\\partial t}" texword)

(defprop $kdelta "\\delta" texword)
(defprop %kdelta "\\delta" texword)
(defprop $icurvature "\\cal{R}" texword)
(defprop %icurvature "\\cal{R}" texword)
(defprop $ichr1 "\\Gamma" texword)
(defprop %ichr1 "\\Gamma" texword)
(defprop $ichr2 "\\Gamma" texword)
(defprop %ichr2 "\\Gamma" texword)
;(defprop $icc1 "c" texword)
;(defprop %icc1 "c" texword)
;(defprop $icc2 "c" texword)
;(defprop %icc2 "c" texword)
(defprop $ifc1 "\\gamma" texword)
(defprop %ifc1 "\\gamma" texword)
(defprop $ifc2 "\\gamma" texword)
(defprop %ifc2 "\\gamma" texword)
(defprop $ifb "\\lambda" texword)
(defprop %ifb "\\lambda" texword)
;(defprop $ifr "e" texword)
;(defprop %ifr "e" texword)
;(defprop $ifri "e" texword)
;(defprop %ifri "e" texword)
(defprop $ifg "\\nu" texword)
(defprop $levi_civita  "\\varepsilon" texword)
(defprop %levi_civita  "\\varepsilon" texword)

(defun $tentex (x) (meval (list '$tex (tenreform x))))

(defun tenreform (x)
  (cond
    ((atom x) x)
    ((atom (car x)) (cons (car x) (tenreform (cdr x))))
    (
      (rpobj x)
      (
        (lambda (u v) (cond (v (list '(mexpt simp) u v)) (t u)))
        (
          (lambda (y)
            (cond (y (cons (cons (name x) '(simp array)) y)) (t (name x)))
          )
          (append
            (and (covi x) (list (cons '(mtimes simp) (covi x))))
            (and
              (deri x)
              (list
                (cons
                  '(mtimes simp) 
                  (append (and (not (covi x)) (deri x) '(",")) (deri x))
                )
              )
            )
          )
        )
        (and (conti x) (cons '(mtimes simp) (conti x)))
      )
    )
    (t (cons (tenreform (car x)) (tenreform (cdr x))))
  )
)
