;;  Copyright 2002-2003 by
;;  Stavros Macrakis (macrakis@alum.mit.edu) and
;;  Barton Willis (willisb@unk.edu)

;;  Maxima nset is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; Maxima nset has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;; A Maxima set package

(in-package :maxima)

(macsyma-module nset)

($put '$nset 1.21 '$version)

;; Display sets as { .. }.

(defprop $set msize-matchfix grind) 

(setf (get '$set 'dissym) '((#\{ ) #\} ))
(setf (get '$set  'dimension) 'dimension-match)

;; Parse {a, b, c} into set(a, b, c).

(putopr "{" '$set)

(def-nud-equiv |$}| delim-err)
(def-led-equiv |$}| erb-err)
(def-lbp     |$}| 5.)

(def-nud-equiv	|${| parse-matchfix)
(def-match	|${| |$}|)
(def-lbp	|${| 200.)
;No RBP
(def-mheader	|${| ($set))
(def-pos	|${| $any)
;No LPOS
;No RPOS

(def-operator "{" '$any nil '$any nil nil nil nil '(nud . parse-matchfix) 'msize-matchfix 'dimension-match "}")

;; Support for TeXing sets. If your mactex doesn't TeX the empty set
;; correctly, get the latest mactex.lisp.

(defprop $set tex-matchfix tex)
(defprop $set (("\\left \\{" ) " \\right \\}") texsym)

(defun require-set (x context-string)
  (if ($setp x) (cdr x) (merror (intl:gettext "~:M: argument must be a set; found: ~:M") context-string x)))

;; If x is a Maxima list, return a Lisp list of its members; otherwise,
;; signal an error. Unlike require-set, the function require-list does not
;; coerce the result to a set.

(defun require-list (x context-string)
  (if ($listp x) (cdr x)
    (merror (intl:gettext "~:M: argument must be a list; found: ~:M") context-string x)))

;; If x is a Maxima list or set, return a Lisp list of its members; otherwise,
;; signal an error.  Unlike require-set, the function require-list-or-set 
;; does not coerce the result to a set.

(defun require-list-or-set (x context-string)
  (if (or ($listp x) ($setp x)) (cdr x)
    (merror (intl:gettext "~:M: argument must be a list or a set; found: ~:M") context-string x)))

;; When a is a list, return a list of the unique elements of a.
;; Otherwise just return a.

(defun $unique (x)
  (if ($listp x)
    `((mlist) ,@(sorted-remove-duplicates (sort (copy-list (cdr x)) '$orderlessp)))
    x))

;; When a is a list, setify(a) is equivalent to apply(set, a). When a 
;; isn't a list, signal an error. 

(defun $setify (a)
  (simplifya `(($set) ,@(require-list a "$setify")) nil))

;; When a is a list, convert a and all of its elements that are lists
;; into sets.  When a isn't a list, return a.

(defun $fullsetify (a)
  (cond (($listp a) 
	 `(($set) ,@(mapcar '$fullsetify (cdr a))))
	(t a)))

;; If a is a set, convert the top-level set to a list; when a isn't a
;; list, return a.

(defun $listify (a)
  (if ($setp a) `((mlist simp) ,@(cdr a)) a))

;; full_listify(a) converts all sets in a into lists.

(defun $full_listify (a)
  (setq a ($ratdisrep a))
  (cond (($mapatom a) a)
	(($setp a) (simplify (cons (list 'mlist) (mapcar #'$full_listify (cdr a)))))
	(t (simplify (cons (car a) (mapcar #'$full_listify (cdr a)))))))

(defprop $set simp-set operators)

;; Simplify a set. 

(defun simp-set (a yy z)
  (declare (ignore yy))
  (setq a (mapcar #'(lambda (x) (simplifya x z)) (cdr a)))
  (setq a (sorted-remove-duplicates (sort a '$orderlessp)))
  `(($set simp) ,@a))

;; Return true iff a is an empty set or list

(defun $emptyp (a)
  (or (like a `(($set))) (like a `((mlist))) (and ($matrixp a) (every '$emptyp (margs a)))))

;; Return true iff the operator of a is set.

(defun $setp (a)
  (and (consp a) (consp (car a)) (eq (caar a) '$set)))

;; Return the cardinality of a set. This function works even when $simp is false.
 
(defun $cardinality (a)
  (if $simp (length (require-set a "$cardinality"))
    (let (($simp t)) ($cardinality (simplify a)))))

;; Return true iff a is a subset of b. If either argument is a list, first 
;; convert it to a set. Signal an error if a or b aren't lists or sets.

(defun $subsetp (a b)
  (setq a (require-set a "$subsetp"))
  (setq b (require-set b "$subsetp"))
  (and (<= (length a) (length b)) (set-subsetp a b)))

;; Return true iff sets a and b are equal;  If either argument is a list, first
;; convert convert it to a set. Signal an error if either a or b aren't lists
;; or sets.

(defun $setequalp (a b)
  (setq a (require-set a "$setequalp"))
  (setq b (require-set b "$setequalp"))
  (and (= (length a) (length b)) (every #'like a b)))


;;  Adjoin x to the list or set a and return a set.

(defun $adjoin (x a)
  (setq a (require-set a "$adjoin"))
  (multiple-value-bind (f i b) (b-search-expr x a 0 (length a))
    (if (not f) (setq a (prefixconc a i (cons x b))))
    `(($set simp) ,@a)))

;; If x is a member of the set or list a, delete x from setify(a); otherwise, return 
;; setify(a). For a set a, disjoin(x,a) == delete(x,a) == setdifference(a,set(x)); 
;; however, disjoin should be the fastest way to delete a member from a set.

(defun $disjoin (x a)
 (setq a (require-set a "$disjoin"))
  (multiple-value-bind (f i b) (b-search-expr x a 0 (length a))
    `(($set simp) ,@(if f (prefixconc a i b) a))))

;; (previxconc l len rest) is equivalent to (nconc (subseq l len) rest)

(defun prefixconc (l len rest)
  (do ((res nil (cons (car l) res))
       (i len (decf i))
       (l l (cdr l)))
      ((= i 0) (nreconc res rest))
    (declare (fixnum i))))

;; union(a1,a2,...an) returns the union of the sets a1,a2,...,an. 
;; If any argument is a list, convert it to a set. Signal an error 
;; if one of the arguments isn't a list or a set. When union receives 
;; no arguments, it returns the empty set.

(defun $union (&rest a)
  (let ((acc nil))
    (dolist (ai a `(($set simp) ,@acc))
      (setq acc (set-union acc (require-set ai "$union"))))))

;; Remove elements of b from a. Works on lists or sets.

(defun $setdifference (a b)
  `(($set simp) ,@(sset-difference (require-set a "$setdifference")
				   (require-set b "$setdifference"))))

;; intersection(a1,a2,...an) returns the intersection of the sets 
;; a1,a2,...,an. Signal an error if one of the arguments isn't a 
;; list or a set. intersection must receive at least one argument.

(defun $intersection (a &rest b)
  (let ((acc (require-set a "$intersection")))
    (cond ((consp b)
	   (dolist (bi b)
	     (setq acc (set-intersect acc (require-set bi "$intersection"))))))
    `(($set simp) ,@acc)))
    
;; intersect is an alias for intersection.

(defun $intersect (a &rest b)
  (apply '$intersection (cons a b)))

;; Return true iff x as an element of the set or list a.  Use like 
;; to test for equality. Signal an error if a isn't a set or list.

(defun $elementp (x a)
  (setq a (require-set a "$elementp"))
  (b-search-expr x a 0 (length a)))
 
;; Return true if and only if the lists or sets a and b are disjoint;
;; signal an error if a or b aren't lists or sets.

#|
(defun $disjointp-binary-search-version (a b)
  (setq a (require-set a "$disjointp"))
  (setq b (require-set b "$disjointp"))
  (if (> (length a) (length b)) (rotatef a b))
  (let ((n (length b)))
    (catch 'disjoint 
      (dolist (ai a)
	(if (b-search-expr ai b 0 n) (throw 'disjoint nil)))
      t)))
|#

(defun $disjointp (a b)
  (setq a (require-set a "$disjointp"))
  (setq b (require-set b "$disjointp"))
  (set-disjointp a b))

;; Return the set of elements of the list or set a for which the predicate 
;; f evaluates to true; signal an error if a isn't a list or a set. Also,
;; signal an error if the function f doesn't evaluate to true, false, or
;; unknown.

(defun $subset (a f)
  (setq a (require-set a "$subset"))
  (let ((acc nil) (b))
    (dolist (x a `(($set simp) ,@(nreverse acc)))
      (setq b (mfuncall f x))
      (cond ((eq t b) (push x acc))
	    ((not (or (eq b nil) (eq b '$unknown)))
	     (merror (intl:gettext "subset: ~:M(~:M) evaluates to a non-boolean.") f x))))))
	     
;; Return a list of three sets. The first set is the subset of a for which
;; the predicate f evaluates to true, the second is the subset of a
;; for which f evaluates to false, and the third is the subset of a
;; for which f evalute to unknown.

(defun $partition_set (a f)
  (setq a (require-set a "$partition_set"))
  (let ((t-acc) (f-acc) (b))
    (dolist (x a `((mlist simp) 
		   (($set simp) ,@(nreverse f-acc)) 
		   (($set simp) ,@(nreverse t-acc))))
      (setq b (mfuncall f x))
      (cond ((eq t b) (push x t-acc))
	    ((or (eq b nil) (eq b '$unknown)) (push x f-acc))
	    (t 
	     (merror (intl:gettext "partition_set: ~:M(~:M) evaluates to a non-boolean.") f x))))))
	       
;; The symmetric difference of sets, that is (A-B) union (B - A), is associative.
;; Thus the symmetric difference extends unambiguously to n-arguments.

(defun $symmdifference (&rest l)
  (let ((acc nil))
    (dolist (lk l (cons '($set simp) acc))
      (setq acc (set-symmetric-difference acc (require-set lk "$symmdifference"))))))
            
;; Return {x | x in exactly one set l1, l2, ...}

(defun $in_exactly_one (&rest l)
  ;; u = union of l1, l2,...
  ;; r = members that are in two or more l1, l2, ...
  (let ((u nil) (r nil))
    (dolist (lk l)
      (setq lk (require-set lk "$in_exactly_one"))
      (setq r (set-union r (set-intersect u lk)))
      (setq u (set-union u lk)))
    (cons '($set simp) (sset-difference u r))))

;; When k is a positive integer, return the set of all subsets of a 
;; that have exactly k elements; when k is nil, return the power set
;; of a. Signal an error if the first argument isn't a list or a set.

(defun $powerset (a &optional k)
  (setq a (require-set a "$powerset"))
  (cond ((null k)
	 (cons `($set simp) 
	       (mapcar #'(lambda (s) 
			   (cons '($set simp) s)) (power-set  a))))
	((and (integerp k) (> k -1))
	 (powerset-subset a k (length a)))))

(defun power-set (a)
  (cond ((null a) `(()))
	(t
	 (let ((x (car a)) (b (power-set (cdr a))))
	   (append `(()) (mapcar #'(lambda (u) (cons x u)) b) (cdr b))))))
	
(defun powerset-subset (a k n)
  (let ((s) (b) (acc))
    (cond ((= k 0)
	   (setq acc (cons `(($set)) acc)))
     	  ((<= k n)
	   (dotimes (i k)
	     (setq s (cons i s)))
	   (setq s (nreverse s))
	   (while (not (null s))
	     (setq b nil)
	     (dotimes (i k)
	       (setq b (cons (nth (nth i s) a) b)))
	     (setq acc (cons (cons `($set simp) (nreverse b)) acc))
	     (setq s (ksubset-lex-successor s k n)))))
    (cons `($set simp) (nreverse acc))))

;; This code is based on Algorithm 2.6 "Combinatorial Algorithms Generation,
;; Enumeration, and Search," CRC Press, 1999 by Donald Kreher and Douglas
;; Stinson. 
      
(defun ksubset-lex-successor (s k n)
  (let ((u (copy-list s))
	(i (- k 1)) (j) (si (- n k)))
    (while (and (>= i 0) (= (nth i s) (+ si i)))
      (decf i))
    (cond ((< i 0)
	   nil)
	  (t
	   (setq j i)
	   (setq si (+ 1 (- (nth i s) i)))
	   (while (< j k)
	     (setf (nth j u) (+ si j))
	     (incf j))
	   u))))

;; When the list a is redundant, need-to-simp is set to true; this flag
;; determines if acc needs to be simplified. Initially, p = (0,1,2,..,n);
;; the 

(defun $permutations (a)
  (cond (($listp a) 
	 (setq a (sort (copy-list (cdr a)) '$orderlessp)))
	(t
	 (setq a (require-set a "$permutations"))))
  
  (let* ((n (length a)) (p (make-array (+ n 1) :element-type 'fixnum))
	 (r (make-array (+ n 1) :initial-element 0 :element-type 'fixnum))
	 (b (make-array (+ n 1) :initial-element 0))
	 (i) (acc) (q) 
	 (need-to-simp (not (= (length a) 
			       (length (sorted-remove-duplicates (copy-list a)))))))
    
    (dotimes (i (+ 1 n))
      (setf (aref p i) i))
    (dotimes (i n)
      (setf (aref b (+ i 1)) (nth i a)))
    
    (cond ((not (null a))
	   (while (not (null p))
	     (setq i 1)
	     (setq q nil)
	     (while (<= i n)
	       (setq q (cons (aref b (aref p i)) q))
	       (incf i))
	     (setq acc (cons (cons '(mlist simp) (nreverse q)) acc))
	     (setq p (permutation-lex-successor n p r))))
	  (t
	   (setq acc `(((mlist))))))
    (setq acc (nreverse acc))
    (if need-to-simp `(($set) ,@acc)
      `(($set simp) ,@acc))))

;; This code is based on Algorithm 2.14 "Combinatorial Algorithms Generation,
;; Enumeration, and Search," CRC Press, 1999 by Donald Kreher and Douglas
;; Stinson. 

;; The array elements p(1) thru p(n) specify the permutation; the array
;; r gets used for swapping elements of p.  Initially p = (0,1,2,..,n).
 
(defun permutation-lex-successor (n p r)
  (declare (type (simple-array fixnum *) p r))
  (declare (type fixnum n))
  (let ((i (- n 1)) (j n) (m) (tm))
    (setf (aref p 0) 0)
    (while (< (aref p (+ i 1)) (aref p i))
      (decf i))
    (cond ((= i 0)
	   nil)
	  (t
	   (while (< (aref p j) (aref p i))
	     (decf j))
	   (setq tm (aref p j))
	   (setf (aref p j) (aref p i))
	   (setf (aref p i) tm)
	   (setq j (+ i 1))
	   (while (<= j n)
	     (setf (aref r j) (aref p j))
	     (incf j))
	   (setq j (+ i 1))
	   (setq m (+ n i 1))
	   (while (<= j n)
	     (setf (aref p j) (aref r (- m j)))
	     (incf j))
	   p))))

(defun $random_permutation (a)
  (if ($listp a)
    (setq a (copy-list (cdr a)))
    (setq a (copy-list (require-set a "$random_permutation"))))

  (let ((n (length a)))
    (dotimes (i n)
      (let
        ((j (+ i ($random (- n i))))
         (tmp (nth i a)))
        (setf (nth i a) (nth j a))
        (setf (nth j a) tmp))))

  `((mlist) ,@a))

    
#|
;;; Returns 3 values
;;; FOUND -- is X in L
;;; POSITION -- where is X in L; if not in L, position it is before
;;; REST -- everything after X in L

(defun old-b-search-expr (x l lo len)
  (declare (fixnum lo len))
  (if (= len 0) (values nil lo l)
    (let ((mid) (midl))
      (while (> len 1)
	(if ($orderlessp x (car (setq midl (nthcdr (setq mid (floor len 2)) l))))
            (setq len mid)
          (setq l midl
		lo (+ lo mid)
                len (- len mid))))
      (cond (($orderlessp x (nth 0 l)) (values nil lo l))
            ((like x (nth 0 l)) (values t lo (cdr l)))
            (t (values nil (1+ lo) (cdr l)))))))
|#

;;; Returns 3 values
;;; FOUND -- is X in L
;;; POSITION -- where is X in L; if not in L, position it is before
;;; REST -- everything after X in L

(defun b-search-expr (x l lo len)
  (declare (fixnum lo len))
  (if (= len 0) (values nil lo l)
    (progn
    ;; uses great directly instead of $orderlessp; only specrepcheck x once
      (setq x (specrepcheck x))
      (let ((mid) (midl) (midel))
	(while (> len 1)
	  (cond
	   ;; Previously, it could hit x and continue searching
	   ;; Since great doesn't guarantee inequality, we need the check for
	   ;; alike1 anyway (hidden inside $orderlessp), so we might as well
	   ;; take advantage of it
	 ((alike1
	   x
	   (setq midel
		 (specrepcheck (car (setq midl (nthcdr (setq mid (floor
								  len 2)) l))))))
	  (setq l midl
		lo (+ lo mid)
		len -1))
	 
	 ((great midel x)
	  (setq len mid))
	 (t (setq l midl
		  lo (+ lo mid)
		  len (- len mid)))))
	
	(cond ((= len -1) (values t lo (cdr l)))
	      ((alike1 x (specrepcheck (nth 0 l))) (values t lo (cdr l)))
	      ((great (specrepcheck (nth 0 l)) x) (values nil lo l))
	      (t (values nil (1+ lo) (cdr l))))))))
  
;; Flatten is somewhat difficult to define -- essentially it evaluates an 
;; expression as if its main operator had been declared nary; however, there 
;; is a difference.  We have

;; (C2) flatten(f(g(f(f(x)))));
;; (D2)         f(g(f(f(x))))
;; (C3) declare(f,nary);
;; (D3)         DONE
;; (C4) ev(d2);
;; (D4)         f(g(f(x)))

;; Unlike declaring the main operator of an expression to be nary, flatten 
;; doesn't recurse into other function arguments.  

;; To successfully flatten an expression, the main operator must be
;; defined for zero or more arguments;  if this isn't the case, 
;; Maxima can halt with an error. So be it.

(defun $flatten (e)
  (cond ((or (specrepp e) (mapatom e)) e)
	(t (mcons-op-args (mop e) (flattenl-op (margs e) (mop e))))))

(defun flattenl-op (e op)
  (mapcan #'(lambda (e)
	      (cond ((or (mapatom e) (not (alike1 (mop e) op)))
		     (list e))
		    (t (flattenl-op (margs e) op))))
	  e))

; doesn't work on f[1](f[1](x)).
;(defun $flatten (e)
;  (if (or (specrepp e) (mapatom e)) e
;    (cons `(,(mop e)) (total-nary e))))

(defun sorted-remove-duplicates (l)
  (prog1 l
    (while (cdr l)
      (while (and (cdr l) (like (car l) (cadr l))
		  (rplacd l (cddr l))))
      (setq l (cdr l)))))

(defun set-intersect (l1 l2)
  ;;  Only works for lists of sorted by $orderlessp.
  (with-collector collect
    (do-merge-symm
        l1 l2
        #'like
        #'$orderlessp
        #'collect
        nil)))

(defun set-union (l1 l2)
  ;; Only works for lists of sorted by $orderlessp.
  (with-collector collect
    (do-merge-symm
        l1 l2
        #'like
        #'$orderlessp
        #'collect
        #'collect)))

(defun sset-difference (l1 l2)
  ;; Only works for lists of sorted by $orderlessp.
  (with-collector collect
    (do-merge-asym
        l1 l2
        #'like
        #'$orderlessp
        nil
        #'collect
        nil)))

(defun set-subsetp (l1 l2)
  ;; Is l1 a subset of l2
  (catch 'subset
    (do-merge-asym
     l1 l2
     #'like
     #'$orderlessp
     nil
     #'(lambda (xx) (declare (ignore xx)) (throw 'subset nil))
     nil)
    t))

(defun set-symmetric-difference (l1 l2)	; i.e. xor
  (with-collector collect
    (do-merge-symm
        l1 l2
        #'like
        #'$orderlessp
        nil
        #'collect)))
	
(defun set-disjointp (l1 l2)
  (catch 'disjoint
    (do-merge-symm
     l1 l2
     #'like
     #'$orderlessp
     #'(lambda (xx) (declare (ignore xx)) (throw 'disjoint nil))
     nil)
    t))
   
;; When s = $max, return  { x in a | f(x) = maximum of f on a} and
;; when s = $min, return  { x in a | f(x) = minimum of f on a}.
;; Signal an error when s isn't $max or $min.

(defun $extremal_subset (a f s)
  (setq a (require-set a "$extremal_subset"))
  (cond ((null a) 
	 `(($set simp)))
	(t
	 (cond ((eq s '$min)
		(setq s -1))
	       ((eq s '$max)
		(setq s 1))
	       (t
		(merror (intl:gettext "extremal_subset: third argument must be 'max or 'min; found: ~:M") s)))
	 (let* ((max-subset (nth 0 a))
		(mx (mul s (mfuncall f max-subset)))
		(x))
	   (setq max-subset `(,max-subset))
	   (setq a (cdr a))
	   (dolist (ai a)
	     (setq x (mul s (mfuncall f ai)))
	     (cond ((is-boole-check (mgrp x mx))
		    (setq mx x
			  max-subset `(,ai)))
		   ((like x mx)
		    (setq max-subset (cons ai max-subset)))))
	   `(($set simp) ,@(nreverse max-subset))))))
   
(defun bool-checked-mfuncall (f x y)
;  (let ((bool (is-boole-check (mfuncall f x y))))
;    (if (not (or (eq bool 't) (eq bool nil))) 
;	(merror "~:M(~:M,~:M) doesn't evaluate to a boolean" f x y)
;      bool)))
  (let (($prederror nil) (b))
    (setq b (mevalp (mfuncall f x y)))
    (if (or (eq b t) (eq b nil)) b
      (merror (intl:gettext "equiv_classes: ~:M(~:M, ~:M) evaluates to a non-boolean.") f x y))))
  
    
;; Return the set of equivalence classes of f on the set l.  The
;; function f must be an boolean-valued function defined on the
;; cartesian product of l with l; additionally, f should be an
;; equivalence relation.

;; The lists acc and tail share structure.
           
(defun $equiv_classes (l f)
  (setq l (require-set l "$equiv_classes"))
  (do ((l l (cdr l))
       (acc)
       (tail)
       (x))
      ((null l) (simplify (cons '($set) (mapcar #'(lambda (x) (cons '($set) x)) acc))))
    (setq x (car l))
    (setq tail (member-if #'(lambda (z) (bool-checked-mfuncall f x (car z))) acc))
    (cond ((null tail)
	   (setq acc (cons `(,x) acc)))
	  (t
	   (setf (car tail) (cons x (car tail)))))))

;; cartesian_product(a,b1,b2,...,bn) returns the set with members
;; of the form [x0,x1, ..., xn], where x0 in a,  x1 in b1, ... , and 
;; xn in bn. With just one argument cartesian_product(a) returns the 
;; set with members [a1],[a2], ... [an], where a1, ..., an are the members of a.

;; Signal an error when a or any b isn't a list or a set.

;; After completing the dolist (bi b), the list a doesn't have duplicate 
;; members -- thus we can get by with  only sorting a.

(defun $cartesian_product (&rest b)
  (cond ((null b)
         `(($set) ((mlist simp))))
	(t
	 (let ((a) 
	       (acc (mapcar #'list (require-set (car b) "$cartesian_product"))))
	   (setq b (cdr b))
	   (dolist (bi b)
	     (setq a nil)
	     (setq bi (require-set bi "$cartesian_product"))
	     (dolist (bij bi (setq acc a))
	       (setq a (append a (mapcar #'(lambda (x) (cons bij x)) acc)))))
	   (cons '($set simp) 
		 (sort (mapcar #'(lambda (x) (cons '(mlist simp) (reverse x))) acc) 
		       '$orderlessp))))))

;; When n is defined, return a set of partitions of the set or list a
;; into n disjoint subsets.  When n isn't defined, return the set of
;; all partitions. 

;; Let S be a set. We say a set P is a partition of S provided
;;   (1) p in P implies p is a set,
;;   (2) p1, p2 in P and p1 # p2 implies p1 and p2 are disjoint,
;;   (3) union(x | x in P) = S.
;; Thus set() is a partition of set().

(defun $set_partitions (a &optional n-sub)
  (setq a (require-set a "$set_partitions"))
  (cond ((and (integerp n-sub) (> n-sub -1))
	 `(($set) ,@(set-partitions a n-sub)))
	((null n-sub)
	 (setq n-sub (length a))
	 (let ((acc (set-partitions a 0)) (k 1))
	   (while (<= k n-sub)
	     (setq acc (append acc (set-partitions a k)))
	     (incf k))
	   `(($set) ,@acc)))
	(t
	 (merror (intl:gettext "set_partitions: second argument must be a positive integer; found: ~:M") n-sub))))

(defun set-partitions (a n)
  (cond ((= n 0)
	 (cond ((null a)
		(list `(($set))))
	       (t
		nil)))
	((null a)
	 nil)
	(t
	 (let ((p) (x) (acc) (w) (s) (z))
	   (setq x (car a))
	   (setq p (set-partitions (cdr a) n))
	   (dolist (pj p)
	     (setq w nil)
	     (setq s (cdr pj))
	     (while (not (null s))
	       (setq z (pop s))
	       (setq acc (cons (simplifya `(($set) ,@w ,($adjoin x z) ,@s) t) acc))
	       (setq w (cons z w))))
	     	   
	   (setq x `(($set) ,x))
	   (setq p (set-partitions (cdr a) (- n 1)))
	   (dolist (pj p acc)
	     (setq acc (cons ($adjoin x pj) acc)))))))

;; Generate the integer partitions in dictionary order.  When the optional
;; argument len is defined, only generate the partitions with exactly len
;; members, including 0.

(defun $integer_partitions (n &optional len)
  (let ((acc))
    (cond ((and (integerp n) (>= n 0))
	   (setq acc (cond ((= n 0) nil)
			   ((integerp len) (fixed-length-partitions n n len))
	               (t (integer-partitions n))))
           (if (not acc)
               (setq acc `(((mlist simp))))
               (setq acc (mapcar #'(lambda (x) (simplify (cons '(mlist) x))) acc)))
	   `(($set simp) ,@acc))
	  (t
	   (if len `(($integer_partitions simp) ,n ,len) `(($integer_partitions simp) ,n))))))
	 
(defun integer-partitions (n)
  (let ((p `(,n)) (acc nil) (d) (k) (j) (r))
    (while (> (car (last p)) 1)
      (setq acc (cons (copy-list (reverse p)) acc))
      (setq p (member t p :key #'(lambda (x) (> x 1))))
      (setq k (- (nth 0 p) 1))
      (setf (nth 0 p) k)
      (setq d (- n (reduce #'+ p)))
      (setq j k)
      (while (and (> k 0) (> d 0))
      	(multiple-value-setq (d r) (floor d k))
	(setq p (append (make-list d :initial-element k) p))
	(setq d r)
	(decf k)))
    (setq acc (cons (copy-list (reverse p)) acc))
    acc))

(defun fixed-length-partitions (n b len)
  (let ((p t) (acc) (i))
    (cond ((> n (* b len)) nil)
	  ((= len 1) (if (<= n b) (setq acc `((,n))) nil))
	  (t
	   (setq len (- len 1))
	   (setq i (- n (min n b)))
	   (setq n (min n b))
	   (while (not (null p))
	     (setq p (mapcar #'(lambda (x) (cons n x)) (fixed-length-partitions i (min i n) len)))
	     (setq acc (append p acc))
	     (decf n)
	     (incf i))))
    acc))

;; When n is a nonnegative integer, return the number of partitions of n.
;; If the optional parameter lst has the value "list", return a list of
;; the number of partitions of 1,2,3, ... , n.  If n isn't a nonnegative
;; integer, return a noun form.

(defun $num_partitions (n &optional lst)
  (cond ((equal n 0) 1)
	((and (integerp n) (> n -1))
	 (let ((p (make-array (+ n 1)))
	       (s (make-array (+ n 1)))
	       (sum) (i) (j))
	   (setf (aref p 0) 1)
	   (setf (aref p 1) 1)
	   
	   (setq i 0)
	   (while (<= i n)
	     (setf (aref s i) (mfuncall '$divsum i 1))
	     (incf i))
	   
	  (setq i 2)
	  (while (<= i n)
	    (setq sum 0)
	    (setq j 1)
	    (while (<= j i)
	       (setq sum (+ sum (* (aref s j) (aref p (- i j)))))
	       (incf j))
	    (setf (aref p i) (/ sum i))
	    (incf i))
	  (cond ((eq lst '$list)
		 (let ((acc))
		   (incf n)
		   (dotimes (i n)
		     (push (aref p i) acc))
		   (setq acc (nreverse acc))
		   (push '(mlist simp) acc)))
		(t
		 (aref p n)))))
	(t (if lst `(($num_partitions simp) ,n ,lst) 
	     `(($num_partitions simp) ,n)))))

(defun $num_distinct_partitions (n &optional lst)
  (cond ((eq n 0) 1)
	((and (integerp n) (> n -1))
	 (let ((p (make-array (+ n 1)))
	       (s (make-array (+ n 1)))
	       (u (make-array (+ n 1)))
	       (sum) (i) (j))
	   (setf (aref p 0) 1)
	   (setf (aref p 1) 1)
	   
	   (setq i 0)
	   (while (<= i n)
	     (setf (aref s i) (mfuncall '$divsum i 1))
	     (incf i))
	   (setq i 0)
	   (while (<= i n)
	     (if (oddp i)
		 (setf (aref u i) (aref s i))
	       (setf (aref u i) (- (aref s i) (* 2 (aref s (/ i 2))))))
	     (incf i))
	   (setq i 2)
	   (while (<= i n)
	     (setq sum 0)
	     (setq j 1)
	     (while (<= j i)
	       (setq sum (+ sum (* (aref u j) (aref p (- i j)))))
	       (incf j))
	     (setf (aref p i) (/ sum i))
	     (incf i))

	   (cond ((eq lst '$list)
		  (let ((acc))
		    (incf n)
		    (dotimes (i n)
		      (push (aref p i) acc))
		    (setq acc (nreverse acc))
		    (push '(mlist simp) acc)))
		 (t
		  (aref p n)))))
	(t (if lst `(($num_distinct_partitions simp) ,n ,lst) 
	     `(($num_distinct_partitions simp) ,n)))))

;; A n-ary Kronecker delta function: kron_delta(n0,n1, ..., nk) simplifies to 1 if
;; (meqp ni nj) is true for *all* pairs ni, nj in (n0,n1, ..., nk); it simplifies to 0 if
;; (mnqp ni nj) is true for *some* pair ni, nj in (n0,n1, ..., nk). Further kron_delta() --> 1
;; and kron_delta(xxx) --> wrong number of arguments error. Thus
;;
;;    kron_delta(x0,...,xn) * kron_delta(y0,..., ym) = kron_delta(x0, ..., xn, y0, ..., ym)
;;
;; is an identity.

(defprop %kron_delta simp-kron-delta operators)
(setf (get '$kron_delta 'noun) '%kron_delta)
(setf (get '%kron_delta 'verb) '$kron_delta)
(setf (get '$kron_delta 'alias) '%kron_delta)
(setf (get '%kron_delta 'reversealias) '$kron_delta)
(defun $kron_delta (&rest x) (simplifya `((%kron_delta) ,@x) t))
(setf (get '%kron_delta 'real-valued) t) ;; conjugate(kron_delta(xxx)) --> kron_delta(xxx)
(setf (get '%kron_delta 'integer-valued) t) ;; featurep(kron_delta(xxx), integer) --> true

(putprop '%kron_delta #'(lambda (s) (setq sign '$pz)) 'sign-function)

(defun simp-kron-delta (l y z)
  (declare (ignore y))

  (setq l (cdr l)) ;; remove (($kron_delta simp)
  (if (and l (null (cdr l))) (wna-err '$kron_delta)) ;; wrong number of arguments error for exactly one argument

  ;; Checking both mnqp and meqp is convenient, but unnecessary. This code misses simplifications that
  ;; involve three or more arguments. Example: kron_delta(a,b,a+b+1,a-b+5) could (but doesn't) simplify 
  ;; to 0 (the solution set (a = b, a = a+b+1, a=a-b+5) is empty.

  (let ((acc nil) (return-zero nil))
    (setq return-zero (catch 'done
			(dolist (lk l) 
			  (setq lk (simpcheck lk z))
			  (cond ((some #'(lambda (s) (eq t (mnqp s lk))) acc) ;; lk # some member of acc, return zero.
				 (throw 'done t))
				((some #'(lambda (s) (eq t (meqp s lk))) acc)) ;; lk = some member of acc, do nothing
				(t (push lk acc))));; push lk onto acc
			nil)) ;; set return-zero to nil
    (cond (return-zero 0)
	  ((or (null acc) (null (cdr acc))) 1)
	  (t  ;; reflection: kron_delta(-a,-b,...) == kron_delta(a,b,...).
	   (let ((neg-acc (sort (mapcar #'neg acc) '$orderlessp)))
	     (setq acc (sort acc '$orderlessp))
	     `((%kron_delta simp) ,@(if (great (cons '(mlist) neg-acc) (cons '(mlist) acc)) neg-acc acc)))))))
			
(defprop %kron_delta tex-kron-delta tex)

(defun tex-kron-delta (x l r)
  (append l `("\\delta_{" ,@(tex-list (cdr x) nil (list "} ") ", ")) r))

;; Stirling numbers of the first kind.

(defprop $stirling1 simp-stirling1 operators)

;; Apply the simplifications (See Knuth Third Edition, Volume 1, 
;; Section 1.2.6, Equations 48, 49, and 50.  For a nonnegative
;; integer n, we have

;; (1) stirling1 (0, n) = kron_delta(0,n),
;; (2) stirling1 (n, n) = 1,
;; (3) stirling1 (n, n - 1) = binomial(n,2),
;; (4) stirling1 (n + 1, 0) = 0,
;; (5) stirling1 (n + 1, 1) = n!,
;; (6) stirling1 (n + 1, 2) = 2^n  - 1.

(defun simp-stirling1 (n yy z)
  (declare (ignore yy))
  (twoargcheck n)
  (setq n (mapcar #'(lambda (x) (simplifya x z)) (cdr n)))
  (let ((m (nth 1 n)))
    (setq n (nth 0 n))
 (cond ((and (integerp n) (integerp m) (> n -1) (> m -1))
	   (integer-stirling1 n m))
	  ((and (nonnegative-integerp n) ($featurep m '$integer))
	   (cond ((like n 0) (simplify `(($kron_delta) ,m 0)))
		 ((like n m) 1)
		 ((like (sub n m) 1) (simplify `((%binomial) ,n 2)))
		 ((and (like m 0) (nonnegative-integerp (sub n 1))) 0)
		 ((and (like m 1) (nonnegative-integerp (sub n 1))) 
		  (simplify `((mfactorial) ,(sub n 1))))
		 ((and (like m 2) (nonnegative-integerp (sub n 1)))
		   (sub (power 2 (sub n 1)) 1))
		 (t  `(($stirling1 simp) ,n ,m))))   
	  (t `(($stirling1 simp) ,n ,m)))))

;; This code is based on Algorithm 3.17 "Combinatorial Algorithms Generation,
;; Enumeration, and Search," CRC Press, 1999 by Donald Kreher and Douglas
;; Stinson. There is a typographical error in Algorithm 3.17; replace i - j 
;; with i - 1. See Theorem 3.14.
	 
(defun integer-stirling1 (m n)
  (cond ((>= m n)
	 (let ((s (make-array `(,(+ m 1) ,(+ m 1)) :initial-element 0))
	       (i) (j) (k) (im1))
	   (setf (aref s 0 0) 1)
	   (setq i 1)
	   (while (<= i m)
	     (setq k (min i n))
	     (setq j 1)
	     (setq im1 (- i 1))
	     (while (<= j k)
	       (setf (aref s i j) (- (aref s im1 (- j 1)) 
				     (* im1 (aref s im1 j))))
	       (incf j))
	     (incf i))
	   (aref s m n)))
	(t 0)))

;; Apply the simplifications (See Knuth Third Edition, Volume 1, 
;; Section 1.2.6, Equations 48, 49, and 50.  For a nonnegative
;; integer n, we have

;; (1) stirling2 (0, n) = kron_delta(0,n),
;; (2) stirling2 (n, n) = 1,
;; (3) stirling2 (n, n - 1) = binomial(n,2),
;; (4) stirling2 (n + 1, 0) = 0,
;; (5) stirling2 (n + 1, 1) = 1,
;; (6) stirling2 (n + 1, 2) = 2^n  - 1.

;; Additionally, we use (See Graham, Knuth, and Patashnik, 
;; "Concrete Mathematics," Table 264)

;; (7) stirling2 (n,0) = kron_delta(n,0),
;; (8) stirling2 (n,m) = 0 when m > n.

;; Instead of (4), we use (7). We do not extend Stirling2 off the integers
;; or into the left HP. The recursion relation 
;;   stirling2(n,m) = stirling2(n-1,m-1) + m * stirling2(n-1,m)
;; does extend Stirling2 into the lower HP.

(defun nonnegative-integerp (e)
  (and ($featurep e '$integer)
       (member ($sign (specrepcheck e)) `($pos $zero $pz) :test #'eq)))
      
(defprop $stirling2 simp-stirling2 operators)

(defun simp-stirling2 (n yy z)
  (declare (ignore yy))
  (twoargcheck n)
  (setq n (mapcar #'(lambda (x) (simplifya x z)) (cdr n)))
  (let ((m (nth 1 n)))
    (setq n (nth 0 n))
    (cond ((and (integerp n) (integerp m) (> n -1))
	   (integer-stirling2 n m))
	  ((and (nonnegative-integerp n) ($featurep m '$integer))
	   (cond ((like n 0) (simplify `(($kron_delta) ,m 0)))         ;; (1)
		 ((like m 0) (simplify `(($kron_delta) ,n 0)))         ;; (7)
		 ((like n m) 1)                                        ;; (2) 
		 ((like (sub n m) 1) (simplify `((%binomial) ,n 2)))   ;; (3)
		 ((and (like m 1) (nonnegative-integerp (sub n 1))) 1) ;; (5)
		 ((and (like m 2) (nonnegative-integerp (sub n 1)))
		   (sub (power 2 (sub n 1)) 1))                        ;; (6) 
		 ((or (eq (csign m) '$neg) (eq (csign (sub m n)) '$pos))
		  0)                                                   ;; (8)
		 (t  `(($stirling2 simp) ,n ,m))))
	  (t `(($stirling2 simp) ,n ,m)))))
	  
;; Stirling2(n,m) = sum((-1)^(m - k) binomial(m k) k^n,i,1,m) / m!.
;; See A & S 24.1.4, page 824.

(defun integer-stirling2 (n m)
  (let ((s (if (= n 0) 1 0)) (i 1) (z) (f 1) (b m))
    (while (<= i m)
      (setq z (* b (expt i n)))
	     (setq f (* f i))
	     (setq b (/ (* (- m i) b) (+ i 1)))
	     (if (oddp i) (setq z (- z)))
	     (setq s (+ s z))
	     (incf i))
    (setq s (/ s f))
    (if (oddp m) (- s) s)))

;; Return the Bell number of n; specifically,  belln(n) is the 
;; cardinality of the set of partitions of a set with n elements.

(defprop $belln simp-belln operators)

;; Simplify the Bell function.  Other than evaluation for nonnegative
;; integer arguments, there isn't much that can be done. I don't know
;; a reasonable extension of the Bell function to non-integers or of
;; any simplifications -- we do thread belln over lists, sets, matrices,
;; and equalities.

(defun simp-belln (n y z)
  (oneargcheck n)
  (setq y (caar n))
  (setq n (simpcheck (cadr n) z))
  (cond ((and (integerp n) (> n -1))
	 (integer-belln n))
	 ((or ($listp n) ($setp n) ($matrixp n) (mequalp n))
	  (thread y (cdr n) (caar n)))
	 (t `(($belln simp) ,n))))

(defun integer-belln (n)
  (let ((s (if (= n 0) 1 0)) (i 1))
    (while (<= i n)
      (setq s (+ s (integer-stirling2 n i)))
      (incf i))
    s))

;; The multinomial coefficient; explicitly multinomial_coeff(a1,a2, ... an) =
;; (a1 + a2 + ... + an)! / (a1! a2! ... an!). The multinomial coefficient
;; gives the number of ways of placing a1 + a2 + ... + an distinct objects
;; into n boxes with ak elements in the k-th box.

;; multinomial_coeff is symmetric; thus when at least one of its arguments
;; is symbolic, we sort them.  Additionally any zero element of the 
;; argument list can be removed without changing the value of 
;; multinomial_coeff; we make this simplification as well.  If
;; b is nil following (remove 0 b), something has gone wrong.

(defun $multinomial_coeff (&rest a)
  (let ((n 0) (d 1))
    (dolist (ai a)
      (setq n (add n ai))
      (setq d (mult d (simplify `((mfactorial) ,ai)))))
    (div (simplify `((mfactorial) ,n)) d)))
	  	 
;; Extend a function f : S x S -> S to n arguments using right associativity.
;; Thus rreduce(f,[0,1,2]) -> f(0,f(1,2)). The second argument must be a list.

(defun $rreduce (f s &optional (init 'no-init))
  (rl-reduce f s t init "$rreduce"))
  
;; Extend a function f : S x S -> S to n arguments using left associativity.
;; Thus lreduce(f,[0,1,2]) -> f(f(0,1),2). Rhe second argument must be a list.

(defun $lreduce (f s &optional (init 'no-init))
  (rl-reduce f s nil init "$lreduce"))

(defun rl-reduce (f s left init fn)
  (setq s (require-list s fn))
  (cond ((not (equal init 'no-init))
	 (reduce #'(lambda (x y) (mfuncall f x y)) s :from-end left 
		 :initial-value init))
	((null s)
	 (merror (intl:gettext "~a: either a nonempty set or initial value must be given.") fn))
	(t
	 (reduce #'(lambda (x y) (mfuncall f x y)) s :from-end left))))

;; Define an operator (signature S x S -> S, for some set S) to be nary and 
;; define a function for its n-argument reduction.  There isn't a user-level
;; interface to this mechanism.

(defmacro def-nary (fn arg f-body id)
  `(setf (get ,fn '$nary) (list #'(lambda ,arg ,f-body) ,id)))

(defun xappend (s)
  #+(or cmu scl)
  (cons '(mlist) (apply 'append (mapcar #'(lambda (x)
                        (require-list x "$append")) s)))
  #-(or cmu scl)
  (let ((acc))
    (dolist (si (reverse s) (cons '(mlist) acc))
      (setq acc (append (require-list si "$append") acc)))))

(def-nary 'mand (s) (mevalp (cons '(mand) s)) t)
(def-nary 'mor (s)  (mevalp (cons '(mor) s)) nil)
(def-nary 'mplus (s) (simplify (cons '(mplus) s)) 0)
(def-nary 'mtimes (s) (simplify (cons '(mtimes) s)) 1)
(def-nary '$max (s) (if (null s) '$minf (maximin s '$max)) '$minf)
(def-nary '$min (s) (if (null s) '$inf (maximin s '$min)) '$inf)
(def-nary '$append (s) (xappend s) '((mlist)))
(def-nary '$union (s) ($apply '$union (cons '(mlist) s)) '(($set)))

;; Extend a function f : S x S -> S to n arguments. When we 
;; recognize f as a nary function (associative), if possible we call a Maxima
;; function that does the work efficiently -- examples are "+", "min", and "max".
;; When there isn't a Maxima function we can call (actually when (get op '$nary) 
;; returns nil) we give up and use rl-reduce with left-associativity.


(defun $xreduce (f s &optional (init 'no-init))
  (let* ((op-props (get (if (atom f) ($verbify f) nil) '$nary))
	 (opfn  (if (consp op-props) (car op-props) nil)))
  
    (cond (opfn
	   (setq s (require-list-or-set s "$xreduce"))
	   (if (not (equal init 'no-init))
	       (setq s (cons init s)))
	  
	   (if (null s)
	       (cadr op-props)        ; is this clause really needed?
	     
	     (funcall opfn s)))

	  (op-props
	   ($apply f ($listify s)))
	  
	  (t
	   (rl-reduce f ($listify s) nil init "$xreduce")))))


;; Extend a function f : S x S -> S to n arguments using a minimum depth tree.
;; The function f should be nary (associative); otherwise, the result is somewhat 
;; difficult to describe -- for an odd number of arguments, we favor the left side of the tree.
	 
(defun $tree_reduce (f a &optional (init 'no-init))
  (setq a (require-list-or-set a "$tree_reduce"))
  (if (not (equal init 'no-init)) (push init a))
  (if (null a)
      (merror (intl:gettext "tree_reduce: either a nonempty set or initial value must be given.")))
  
  (let ((acc) (x) (doit nil))
    (while (consp a)
      (setq x (pop a))
      (while (consp a)
	(push (mfuncall f x (pop a)) acc)
	(if (setq doit (consp a)) (setq x (pop a))))
      (if doit (push x acc))
      (setq a (nreverse acc))
      (setq acc nil))
    x))



;; An identity function -- may see some use in things like
;;     every(identity, [true, true, false, ..]).

(defun $identity (x) x)

;; Maxima 'some' and 'every' functions.  The first argument should be
;; a predicate (a function that evaluates to true, false, or unknown).
;; The functions 'some' and 'every' locally bind $prederror to false.
;; Thus within 'some' or 'every,'  is(a < b) evaluates to unknown instead
;; of signaling an error (as it would when $prederror is true).
;;
;; Three cases:
;;
;;  (1) some(f, set(a1,...,an))  If any f(ai) evaluates to true,
;;  'some' returns true.  'Some' may or may not evaluate all the
;;  f(ai)'s.  Since sets are unordered, 'some' is free to evaluate
;;  f(ai) in any order.  To use 'some' on multiple set arguments,
;;  they should first be converted to an ordered sequence so that
;;  their relative alignment becomes well-defined.

;; (2) some(f,[a11,...,a1n],[a21,...],...) If any f(ai1,ai2,...)
;;  evaluates to true, 'some' returns true.  'Some' may or may not
;;  evaluate all the f(ai)'s.  Since sequences are ordered, 'some'
;;  evaluates in the order of increasing 'i'.

;; (3) some(f, matrix([a111,...],[a121,...],[a1n1...]), matrix(...)). 
;;  If any f(a1ij, a2ij, ...) evaluates to true, return true.  'Some' 
;;  may or may not evaluate all the predicates. Since there is no 
;;  natural order for the entries of a matrix, 'some' is free to 
;;  evaluate the predicates in any order.

;;   Notes:
;;   (a) 'some' and 'every' automatically apply 'maybe'; thus the following
;;   work correctly
;;  
;;   (C1) some("<",[a,b,5],[1,2,8]);
;;   (D1) TRUE
;;   (C2) some("=",[2,3],[2,7]);
;;   (D2) TRUE
;;   
;;  (b) Since 'some' is free to choose the order of evaluation, and
;;  possibly stop as soon as any one instance returns true, the
;;  predicate f should not normally have side-effects or signal
;;  errors. Similarly, 'every' may halt after one instance returns false;
;;  however, the function 'maybe' is wrapped inside 'errset' This allows 
;;  some things to work that would otherwise signal an error:

;;    (%i1) some("<",[i,1],[3,12]);
;;    (%o1) true
;;    (%i2) every("<",[i,1],[3,12]);
;;    (%o2) false
;;    (%i3) maybe(%i < 3);
;;    `sign' called on an imaginary argument:

;;   
;;  (c) The functions 'some' and 'every' effectively use the functions
;;  'map' and 'matrixmap' to map the predicate over the arguments. The
;;  option variable 'maperror' modifies the behavior of 'map' and 
;;  'matrixmap;' similarly, the value of 'maperror' modifies the behavior
;;  of 'some' and 'every.'
;; 
;;   (d) 'every' behaves similarly to 'some' except that 'every' returns
;;   true iff every f evaluates to true for all its inputs.
;;
;;   (e) If emptyp(e) is true, then some(f,e) --> false and every(f,e) --> true. 
;;   Thus (provided an error doesn't get signaled), we have the identities:
;;
;;       some(f,s1) or some(f,s2) == some(f, union(s1,s2)), 
;;       every(f,s1) and every(f,s2) == every(f, union(s1,s2)).
;;   Similarly, some(f) --> false and every(f) --> true.

(defun checked-and (x)
  (setq x (mfuncall '$maybe `((mand) ,@x)))
  (cond ((or (eq x t) (eq x nil) (not $prederror)) x)
	((eq x '$unknown) nil)
	(t
         ;; FOLLOWING MESSAGE IS UNREACHABLE FROM WHAT I CAN TELL
         ;; SINCE MAYBE RETURNS T, NIL, OR '$UNKNOWN
	 (merror "Predicate isn't true/false valued; maybe you want to set 'prederror' to false"))))
    
(defun checked-or (x)
  (setq x (mfuncall '$maybe `((mor) ,@x)))
  (cond ((or (eq x t) (eq x nil) (not $prederror)) x)
	((eq x '$unknown) nil)
	(t
         ;; FOLLOWING MESSAGE IS UNREACHABLE FROM WHAT I CAN TELL
         ;; SINCE MAYBE RETURNS T, NIL, OR '$UNKNOWN
	 (merror "Predicate isn't true/false valued; maybe you want to set 'prederror' to false"))))

;; Apply the Maxima function f to x. If an error is signaled, return nil; otherwise
;; return (list (mfuncall f x)).

(defun ignore-errors-mfuncall (f x)
  (let ((errcatch t))
    (declare (special errcatch))
    (errset (mfuncall f x) lisperrprint)))

(defun $every (f &rest x)
  (cond ((or (null x) (and (null (cdr x)) ($emptyp (first x)))) t)
   
 ((or ($listp (first x)) (and ($setp (first x)) (null (cdr x))))
  (setq x (margs (simplify (apply #'map1 (cons f x)))))
  (checked-and (mapcar #'car (mapcar #'(lambda (s) (ignore-errors-mfuncall '$maybe s)) x))))
   
 ((every '$matrixp x)
  (let ((fmaplvl 2))
    (setq x (margs (simplify (apply #'fmapl1 (cons f x)))))
    (checked-and (mapcar #'(lambda (s) ($every '$identity s)) x))))
 
 (t
   ;; NOT CLEAR FROM PRECEDING CODE WHAT IS "INVALID" HERE
   (merror (intl:gettext "every: invalid arguments.")))))

(defun $some (f &rest x)
  (cond ((or (null x) (and (null (cdr x)) ($emptyp (first x)))) nil)

 ((or ($listp (first x)) (and ($setp (first x)) (null (cdr x))))
  (setq x (margs (simplify (apply #'map1 (cons f x)))))
  (checked-or (mapcar #'car (mapcar #'(lambda (s) (ignore-errors-mfuncall '$maybe s)) x))))

 ((every '$matrixp x)
  (let ((fmaplvl 2))
    (setq x (margs (simplify (apply #'fmapl1 (cons f x)))))
    (checked-or (mapcar #'(lambda (s) ($some '$identity s)) x))))

 (t
   ;; NOT CLEAR FROM PRECEDING CODE WHAT IS "INVALID" HERE
   (merror (intl:gettext "some: invalid arguments.")))))

(defmspec $makeset (l)
  (let* ((fn (car (pop l)))
	 (f (if l (pop l) (wna-err fn)))
	 (v (if l (pop l) (wna-err fn)))
	 (s (if l (pop l) (wna-err fn))))
    (if l (wna-err fn))
    (if (or (not ($listp v)) (not (every #'(lambda (x) (or ($symbolp x) ($subvarp x))) (cdr v))))
   	(merror (intl:gettext "makeset: second argument must be a list of symbols; found: ~:M") v))
    (setq s (require-list-or-set (meval s) "$makeset"))
    (setq f (list (list 'lambda) v f))
    (setq v (margs v))
    (dolist (sk v) (setq f (subst (gensym) sk f :test #'alike1)))
    (simplifya (cons '($set) (mapcar #'(lambda (x) (mfuncall '$apply f x)) s)) t)))

;; Thread fn over l and apply op to the resulting list.

(defun thread (fn l op)
  (simplify (cons `(,op) (mapcar #'(lambda (x) (simplify `((,fn) ,x))) l))))
  
;; Return a set of the divisors of n. If n isn't a positive integer,
;; return a noun form.  We consider both 1 and n to be divisors of n.
;; The divisors of a negative number are the divisors of its absolute
;; value; divisors(0) simplifies to itself.  We thread divisors over
;; lists, sets, matrices, and equalities.

(defprop $divisors simp-divisors operators)

(defun simp-divisors (n y z)
  (oneargcheck n)
  (setq y (caar n))
  (setq n (simpcheck (cadr n) z))
  (cond ((or ($listp n) ($setp n) ($matrixp n) (mequalp n))
	 (thread y (cdr n) (caar n)))
	((and (integerp n) (not (= n 0)))
	 (let (($intfaclim))
	   (setq n (abs n))
	   `(($set simp) ,@(sort (mapcar #'(lambda (x) (car x)) 
					 (divisors (cfactorw n))) '$orderlessp))))
	(t `(($divisors simp) ,n))))

;; The Moebius function; it threads over lists, sets, matrices, and equalities.

(defprop $moebius simp-moebius operators)

(defun simp-moebius (n y z)
  (oneargcheck n)
  (setq y (caar n))
  (setq n (simpcheck (cadr n) z))
  (cond ((and (integerp n) (> n 0))
	 (cond ((= n 1) 1)
	       (t
		(let (($intfaclim))
		  (setq n (cfactorw n))
		  (if (every #'(lambda (x) (= 1 x)) (odds n 0))
		      (if (evenp (ash (length n) -1)) 1 -1)
		    0)))))
	((or ($listp n) ($setp n) ($matrixp n) (mequalp n))
	 (thread y (cdr n) (caar n)))
	(t `(($moebius simp) ,n))))

; Find indices of elements which satisfy a predicate.
; Thanks to Bill Wood (william.wood3@comcast.net) for his help.
; Released under terms of GNU GPL v2 with Bill's approval.

(defun $sublist_indices (items pred)
  (let ((items (require-list items "$sublist_indices")))
    (do ((i 0 (1+ i))
         (xs items (cdr xs))
         (acc '() (if (definitely-so (mfuncall pred (car xs))) (cons (1+ i) acc) acc)))
      ((endp xs) `((mlist) ,@(nreverse acc))))))
