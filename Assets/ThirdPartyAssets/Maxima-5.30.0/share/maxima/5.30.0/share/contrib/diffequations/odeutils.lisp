;; Author Barton Willis
;; University of Nebraska at Kearney
;; Copyright (C) 2004, Barton Willis

;; Brief Description: Maxima code for linear homogeneous second order
;; differential equations.

;; Maxima odelin is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License,
;; http://www.gnu.org/copyleft/gpl.html.

;; Maxima odelin has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

($put '$odeutils 1 '$version)

;; Map the function f onto a mbag and simplify the result.  When the
;; bag is an equality, list, or matrix, simplification isn't needed;
;; however, new types of bags (say sets) may need simplification after 
;; the mapping.

;; Maxima is hit-or-miss about mapping functions over mbags. I suggest
;; we develop that a function similar to this one and that we use it
;; everywhere a function is mapped over a mbag. 

;; If the arguments of bag are in CRE form, margs changes them to general 
;; form---and mbag-map may return an expression in general form. Maybe this 
;; behavior is impolite?

(defun mbag-map (f bag)
  (simplify `((,(mop bag)) ,@(mapcar f (margs bag)))))

(defun number-of-unks (p unks)
  (let (($listconstvars nil))
    ($cardinality ($intersection ($setify ($listofvars p)) unks))))
