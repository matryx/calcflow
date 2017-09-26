;; Support for Maxima sets.
;; Author: Barton Willis
;; Send bug reports to willisb@unk.edu

;; This code is in the public domain.  It has no warranty. Use this
;; code at your own risk. 

(in-package :maxima)

;; Use the predicate canonlt to order the elements of a set.  The
;; default is $charsets_unorderedp.  The predicate $charsets_unorderedp always 
;; returns true; when canonlt is its default value, sets are 
;; never sorted. Other choices for $charsets_canonlt include $ordergreatp 
;; and $orderlessp.

(defun $charsets_unorderedp (a b) t)
(defmvar $charsets_canonlt '$charsets_unorderedp)

;; The set package doesn't distinguish between sets and lists.  We're
;; in trouble if we need to work simultaneously with a set of 
;; lists and a set of sets.  The commerical Macsyma seems to treat
;; all set elements as lists; thus setify([[1,2],[2,1]) returns 
;; [[1,2],[2,1]] because [1,2] and [2,1] are treated as lists 
;; (and consequently they are not equal).  In this package, the 
;; user may decide if set elements that are lists are treated as 
;; lists or as sets.  When $charsets_set_elements_can_be_sets is true 
;; (the default),  set elements that are lists are treated 
;; as sets; otherwise,  when  $charsets_set_elements_can_be_sets is 
;; false, set elements that are lists are treated as lists.

(defmvar $charsets_set_elements_can_be_sets t)

;; For non-lists x and y, equalp(x,y) returns is(ratsimp(x-y)=0).
;; Signal an error if either x or y is a list. Since equalp uses 
;; ratsimp, equalp(x/x,1) is true and equalp(x^(a*b),(x^a)^b)
;; is false. 

(defun $charsets_equalp (x y)
  (cond ((or ($listp x) ($listp y))
	 (merror "Both arguments to `equalp' must be non-lists."))	
	(t ($charsets_xequalp x y))))

;; If you are certain that x and y are not lists, you might call
;; (at Maxima level) ?xequalp instead of equalp.

(defun $charsets_xequalp (x y)
  (like 0 ($ratsimp (add* x (*mminus y)))))

;; If x and y are not lists, $charsets_elem_equalp(x,y) returns 
;; equalp(x,y).  If x and y are both lists, return 
;; setequality(x,y) if set_elements_can_be_sets; otherwise 
;; return equalp(x[1],y[1]) and equalp(x[2],y[2]) and ....
;; Finally, if exactly one of x or y is a list, return false. 

(defun $charsets_elem_equalp (x  y)
  (cond ((and ($listp x) ($listp y)) 
	 (cond ($charsets_set_elements_can_be_sets
		($charsets_setequality x y))
	       ((and ($charsets_emptyp x) ($charsets_emptyp y)) t)
	       (t
		(and 
		 (= ($length x) ($length y))
		 ($charsets_elem_equalp ($first x) ($first y))
		 ($charsets_elem_equalp ($rest x) ($rest y))))))
	((or ($listp x) ($listp y)) nil)
	(t ($charsets_xequalp x y))))

;;  Adjoin x to the Maxima list a; use equalp for the equality test.
;;  When a isn't a list, signal an error.

(defun $charsets_adjoin (x a)
  (cond (($listp a)
	 (cons '(mlist) (adjoin x (margs a) :test #'$charsets_elem_equalp)))
	(t (merror "The second argument to `adjoin' must be a list"))))

;; Setify removes duplicates from a Maxima list and sorts the
;; list using the partial ordering function canonlt. To remove the
;; duplicates from the list, we use element_equalp to test for equality.
;; When the argument isn't a list, signal an error.

(defun $charsets_setify (a)
  (cond (($listp a)
	 (charsets_mysort (cons '(mlist) (remove-duplicates (margs a) :test #'$charsets_elem_equalp))))
	(t (merror "The argument to `setify' must be a list."))))

;; When $charsets_canonlt is $charsets_unorderedp, don't sort; when $charsets_canonlt isn't
;; $charsets_unorderedp, sort the list using the predicate $charsets_canonlt.

(defun charsets_mysort (a)
  (cond ((eq $charsets_canonlt '$charsets_unorderedp) a)
	(t ($sort a $charsets_canonlt))))

;; The maxima function call union(a1,a2,...an) forms the union of the
;; sets a1,a2,...an.

(defmfun $charsets_union ( &rest a)
  (setq a (margs a))
  (cond ((member nil (mapcar #'$listp a))
	 (merror "Each argument to `union' must be a list."))
	(t
	 (cons '(mlist) (remove-duplicates (apply 'append  (map 'list 'rest a)) :test #'$charsets_elem_equalp)))))

;; Remove elements of b from a.  Signal an error if a or b aren't lists.
;; Use element_equalp for the equality test.

(defun $charsets_setdifference (a b)
  (cond ((and ($listp a) ($listp b))
	 (cons '(mlist) (set-difference (margs a) (margs b) :test #'$charsets_elem_equalp)))
	(t (merror "Both arguments to `setdifference' must be lists."))))

;; Return the intersection of lists a and b.  Use element_equalp for the
;; equality test. Signal an error if a or b aren't lists.

(defmfun $charsets_intersection ( &rest a)
  (setq a (margs a))
  (cond ((member nil (mapcar #'$listp a))
	 (merror "Each argument to `intersection' must be a list."))
	(t
	 (setq a (mapcar #'margs a))
	 (cons '(mlist)
	       (reduce #'(lambda (x y)
			   (intersection x y :test #'$charsets_elem_equalp))
		       a :from-end nil)))))

;; Return true iff a is a subset of b.  Signal an error if
;; a or b aren't Maxima lists.

(defun $charsets_subsetp (a b)
  (cond ((and ($listp a) ($listp b))
	 (charsets_xsubsetp (margs a) b))
	(t (merror "Both arguments to `subsetp' must be lists."))))

;; charsets_xsubsetp returns true if and only if each element of the Lisp
;; list a is a member of the Maxima list b.  This function isn't 
;; inteneded to be a user function; it doesn't check whether b is a 
;; Maxima list. Notice that the empty set is a subset of every 
;; set.

(defun charsets_xsubsetp (a b)
  (cond ((null a) t)
	(t
	 (and ($charsets_elementp (car a) b) (charsets_xsubsetp (cdr a) b)))))

;; Return true iff a is a subset of b and b is a subset of a; return
;; false if a or b are not lists.

(defun $charsets_setequality (a b)
  (cond ((and ($listp a) ($listp b))
	 (if (and ($charsets_subsetp a b) ($charsets_subsetp b a)) t nil))
	(t nil)))


;; Return true iff x as an element of the list a; use $charsets_elem_equalp 
;; to test for equality if x isn't a list and use $charsets_setequality to 
;; test for equality if x is a list.  Return false if a isn't a list.

(defun $charsets_elementp (x a)
  (cond (($listp a)
	 (cond (($listp x)
		(cond ($charsets_set_elements_can_be_sets
		       (if (member x (margs a) :test #'$charsets_setequality) t nil))
		      (t
		       (if (member x (margs a) :test #'$charsets_elem_equalp) t nil))))
	       (t
		(if (member x (margs a) :test #'$charsets_elem_equalp) t nil))))
	(t nil)))

;; Return true if e is an empty Maxima list; otherwise, signal an
;; error.

(defun $charsets_emptyp(e)
  (cond (($listp e)
	 (like e '((mlist))))
	(t (merror "Argument to `emptyp' must be a list."))))

;; Return an n element Maxima list [e,e,e,...e]. When n < 0 or
;; n isn't an integer, signal an error.

(defun $charsets_dupe (e n)
  (cond ((and (integerp n) (> n -1))
	 (cons '(mlist) (make-list n :initial-element e)))
	(t (merror "Second argument to `dupe' must be a nonnegative integer."))))

;; Return true if and only if the lists a and b are disjoint;
;; signal an error if a or b aren't lists.

(defun $charsets_disjointp (a b)
  (cond ((and ($listp a) ($listp b))
	 (not (intersection (margs a) (margs b) :test #'$charsets_elem_equalp)))
	(t (merror "Both arguments to `disjointp' must be lists."))))

;; Return those elements of a for which the predicate f evaluates
;; to true; signal an error if a isn't a list.

(defun $charsets_subset (a f)
  (cond (($listp a)
	 (setq a (margs a))
	 (let ((acc nil))
	   (dolist (x a (cons '(mlist) acc))
	     (if (mfuncall f x) (setq acc (cons x acc))))))
	(t (merror "First argument to `subset' must be a list."))))

;; Return the union of a - b and b - a; signal an error if a or b
;; aren't lists.

(defun $charsets_symmdifference (a b)
  (cond ((and ($listp a) ($listp b))
	 (mfuncall '$charsets_union ($charsets_setdifference a b) ($charsets_setdifference b a)))
	(t (merror "Both arguments to `symmdifference' must be lists."))))

;; Return a list of the elements in b that are not in a.

(defun $charsets_complement (a b)
  (cond ((and ($listp a) ($listp b))
	 ($charsets_setdifference b a))
	(t (merror "Both arguments to `complement' must be lists."))))

;; Return true if and only if the argument is a Maxima list and the
;; list does not have duplicate elements.  charsets_setp doesn't check that
;; the list is ordered according to canonlt.

(defun $charsets_setp (a)
  (and ($listp a) (charsets_setp (margs a))))

(defun charsets_setp (a)
  (cond ((null a) t)
	(t (and (charsets_setp (cdr a)) (not (member (car a) (cdr a) :test #'$charsets_elem_equalp))))))

;; Return the set of all subsets of a.  If a has n elements, charsets_powerset(a) has
;; 2^n elements.  Signal an error if the argument isn't a Maxima list.

(defun $charsets_powerset (a)
  (cond (($listp a)
	 (setq a ($charsets_setify a))
	 (cons '(mlist) (mapcar #'(lambda (x) (cons '(mlist) x))
				(charsets_powerset (margs a)))))
	(t (merror "Argument to `charsets_powerset' must be a list."))))

(defun charsets_powerset (a)
  (cond ((null a) (list nil))
	(t
	 (let ((x (car a))
	       (b (charsets_powerset (cdr a))))
	   (append b (mapcar #'(lambda (u) (cons x u)) b))))))

;; Return the set of all subsets of a that have exactly n elements.
;; Signal an error if the first argument isn't a Maxima list or if
;; the second argument isn't a nonnegative integer.

(defun $charsets_subpowerset (a n)
  (cond (($listp a)
	 (setq a ($charsets_setify a))
	 (cond ((and (integerp n) (> n -1))
		(cons '(mlist) (mapcar #'(lambda (x) (cons '(mlist) x))
				       (charsets_subpowerset (margs a) n))))
	       (t
		(merror "Second argument to SUBPOWERSET must
be a nonnegative integer."))))
	(t (merror "First argument to `charsets_subpowerset' must be a list."))))

(defun charsets_subpowerset (a n)
  (cond ((or (< n 1) (null a))
	 nil)
	((= n 1) (mapcar #'list a))
	(t (let ((x (car a))
		 (b (charsets_subpowerset (cdr a) (- n 1))))
	     (append (charsets_subpowerset (cdr a) n)
		     (mapcar #'(lambda (u) (cons x u)) b))))))

