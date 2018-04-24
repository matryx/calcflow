#|
Author: Barton Willis, June 2008

I, Barton Willis, hereby place this code into the public domain.

The dot-dot operator generates terms of an arithmetic sequence. The
two argument dot-dot operator is defined by (Z is the set of integers).

  a .. b = [a + k | k in Z, 0 <= k <= (b - a)].

Thus a .. b = [a, a + 1, a + 2, ..., a + n], where n = floor(b - a). The three
argument dot-dot operator is defined by 

  a .. h .. b = [a + h * k | k in 0 .. (b - a) / h].

a .. b expands to a list when either floor(b - a) is an integer (not a
declared integer) or sign(b - a) is negative or zero; otherwise, the dot-dot
operator returns a noun form.

a .. h .. b expands to a list when floor((b-a) / h) is an integer (not a
declared integer) or  sign(b - a) is negative or zero and h is nonzero.

|#

;; These binding powers make a .. b op c == a .. (b op c), where op = +, -, *, /, or ^.

($nary ".." 80)
(setf (get '$.. 'operators) 'simp-integer-sequence)

(defun simp-integer-sequence (e yy z)
  (declare (ignore yy))

  (let ((i) (j) (k) (lo) (hi) (h) (n) (sgn) (sgn-h) (acc nil))
    (pop e)
    (setq i (if e (simpcheck (pop e) z) (merror "The '..' operator needs 2 or 3 arguments, not 0")))
    (setq j (if e (simpcheck (pop e) z) (merror "The '..' operator needs 2 or 3 arguments, not 1")))
    (setq k (if e (simpcheck (pop e) z) nil))
    (if e (merror "The '..' operator needs 3 or fewer arguments"))
    (if k (setq lo i hi k h j) (setq lo i h 1 hi j))
    (if (zerop1 h) (merror "The step argument to '..' must be nonzero"))
    
    (setq sgn (if (like hi lo) '$zero (csign (sub hi lo))))
    (setq sgn-h (csign h))
    (setq n (if (eq sgn '$zero) 0 (take '($floor) (div (sub hi lo) h))))
    (cond ((and (integerp n) (memq sgn-h '($neg $pos $pn)))
	   (while (>= n 0)
	     (push (add lo (mul n h)) acc)
	     (decf n))
	   (simplify (cons '(mlist) acc)))
	  
	  ((or (and (eq '$neg sgn) (eq '$pos sgn-h))
	       (and (eq '$pos sgn) (eq '$neg sgn-h)))
	   (simplify `((mlist))))
	  
	  ((not k) `(($.. simp) ,i ,j))
	  ((eq 1 j) `(($.. simp) ,i ,k)) ; a .. 1 .. b == a .. b
	  (t `(($.. simp) ,i ,j ,k)))))

 