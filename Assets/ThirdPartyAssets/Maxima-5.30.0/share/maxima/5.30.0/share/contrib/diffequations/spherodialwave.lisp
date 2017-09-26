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

($put '$spherodialwave 1 '$version)

(putprop '$spherodialwave_a
	 '((b c q x)
	   ((unk) "$order" "$spherodialwave_a")
	   ((unk) "$order" "$spherodialwave_a")
	   ((unk) "$order" "$spherodialwave_a")
	   (($dspherodialwave_a) b c q x))
	 'grad)

(putprop '$dspherodialwave_a
	 '((b c q x)
	   ((unk) "$order" "$dspherodialwave_a")
	   ((unk) "$order" "$dspherodialwave_a")
	   ((unk) "$order" "$dspherodialwave_a")
	   ((mtimes) -1
	    ((mexpt) ((mplus) -1 ((mexpt) x 2)) -1)
	    ((mplus)
	     ((mtimes) -1 (($spherodialwave_a) b c q x) c)
	     ((mtimes) ((mplus) 2 ((mtimes) 2 b))
	      (($dspherodialwave_a) b c q x) x)
	     ((mtimes) 4 (($spherodialwave_a) b c q x) q
	      ((mexpt) x 2)))))
	 'grad)

(putprop '$spherodialwave_b
	 '((b c q x)
	   ((unk) "$order" "$spherodialwave_b")
	   ((unk) "$order" "$spherodialwave_b")
	   ((unk) "$order" "$spherodialwave_b")
	   (($dspherodialwave_b) b c q x))
	 'grad)

(putprop '$dspherodialwave_b
	 '((b c q x)
	   ((unk) "$order" "$dspherodialwave_b")
	   ((unk) "$order" "$dspherodialwave_b")
	   ((unk) "$order" "$dspherodialwave_b")
	   ((mtimes) -1
	    ((mexpt) ((mplus) -1 ((mexpt) x 2)) -1)
	    ((mplus)
	     ((mtimes) -1 (($spherodialwave_b ) b c q x) c)
	     ((mtimes) ((mplus) 2 ((mtimes) 2 b))
	      (($dspherodialwave_b) b c q x) x)
	     ((mtimes) 4 (($spherodialwave_b) b c q x) q
	      ((mexpt) x 2)))))
	 'grad)