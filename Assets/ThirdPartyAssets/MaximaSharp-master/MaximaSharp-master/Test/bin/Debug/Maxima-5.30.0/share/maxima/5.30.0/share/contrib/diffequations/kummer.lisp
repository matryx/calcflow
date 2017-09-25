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

($put '$kummer 1 '$version)

(putprop '$kummer_m
	 '((a b x)
	   ((unk) "$order" "$kummer_m")
	   ((unk) "$order" "$kummer_m")
	   (($dkummer_m) a b x))
	 'grad)
	 
(putprop '$dkummer_m
	 '((a b x)
	   ((unk) "$order" "$kummer_m")
	   ((unk) "$order" "$kummer_m")
	   ((mtimes ) ((mexpt  ) x -1)
	    ((mplus ) ((mtimes  ) a 
		       (($kummer_m ) a b x))
	     ((mtimes  ) -1 (($dkummer_m ) a b x) b)
	     ((mtimes  ) (($dkummer_m ) a b x) x))))
	 'grad)

(putprop '$kummer_u
	 '((a b x)
	   ((unk) "$order" "$kummer_u")
	   ((unk) "$order" "$kummer_u")
	   (($dkummer_u) a b x))
	 'grad)

(putprop '$dkummer_u
	 '((a b x)
	   ((unk) "$order" "$dkummer_u")
	   ((unk) "$order" "$dkummer_u")
	   ((mtimes ) ((mexpt  ) x -1)
	    ((mplus ) ((mtimes  ) a 
			   (($kummer_u ) a b x))
	     ((mtimes  ) -1 (($dkummer_u ) a b x) b)
	     ((mtimes  ) (($dkummer_u ) a b x) x))))
	 'grad)



