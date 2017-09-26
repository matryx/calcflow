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

($put '$gauss 1 '$version)

(putprop '$gauss_a
	 `((a b c x)
	   ((unk) "$first" "$gauss_a")
	   ((unk) "$second" "$gauss_a")
	   ((unk) "$third" "$gauss_a")
	   (($dgauss_a) a b c x))
	 'grad)

(putprop '$dgauss_a
	 `((a b c x)
	   ((unk) "$first" "$dgauss_a")
	   ((unk) "$second" "$dgauss_a")
	   ((unk) "$third" "$dgauss_a")
	   ((mtimes) ((mexpt) ((mplus) 1 ((mtimes) -1 x)) -1)
	    ((mexpt) x -1)
	    ((mplus) ((mtimes) a (($gauss_a) a b c x) b)
	     ((mtimes) (($dgauss_a) a b c x)
	      ((mplus) ((mtimes) -1 c)
	       ((mtimes) ((mplus) 1 a b) x))))))
	 'grad)

(putprop '$gauss_b
	 `((a b c x)
	   ((unk) "$first" "$gauss_b")
	   ((unk) "$second" "$gauss_b")
	   ((unk) "$third" "$gauss_b")
	   (($dgauss_b) a b c x))
	 'grad)

(putprop '$dgauss_b
	 `((a b c x)
	   ((unk) "$first" "$dgauss_b")
	   ((unk) "$second" "$dgauss_b")
	   ((unk) "$third" "$dgauss_b")
	   ((mtimes) ((mexpt) ((mplus) 1 ((mtimes) -1 x)) -1)
	    ((mexpt) x -1)
	    ((mplus) ((mtimes) a (($gauss_b) a b c x) b)
	     ((mtimes) (($dgauss_b) a b c x)
	      ((mplus) ((mtimes) -1 c)
	       ((mtimes) ((mplus) 1 a b) x))))))
	 'grad)
