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

($put '$extrabessel 1 '$version)

(putprop '$fbessel_j
	 '((mu x)
	   ((unk) "$order" "$fbessel_j")
	   (($dbessel_j) mu x))
	 'grad)

(putprop '$dbessel_j
	 '((mu x)
	   ((unk) "$order" "$dbessel_j")
	   ((mtimes ) -1 ((mexpt ) x -2)
	    ((mplus )
	     ((mtimes )
	      ((mplus ) ((mtimes ) -1 ((mexpt) mu 2))
	       ((mexpt  ) x 2))
	      (($fbessel_j) mu x))
	     ((mtimes) x (($dbessel_j) mu x)))))
	 'grad)

(putprop '$fbessel_y
	 '((mu x)
	   ((unk) "$order" "$fbessel_y")
	   (($dbessel_y) mu x))
	 'grad)

(putprop '$dbessel_y
	 '((mu x)
	   ((unk) "$order" "$dbessel_y")
	   ((mtimes ) -1 ((mexpt ) x -2)
	    ((mplus )
	     ((mtimes )
	      ((mplus ) ((mtimes ) -1 ((mexpt) mu 2))
	       ((mexpt  ) x 2))
	      (($fbessel_y) mu x))
	     ((mtimes) x (($dbessel_y) mu x)))))
	 'grad)

