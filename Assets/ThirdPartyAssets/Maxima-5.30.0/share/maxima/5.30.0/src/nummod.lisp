;; Maxima functions for floor, ceiling, and friends
;; Copyright (C) 2004, 2005, 2007 Barton Willis

;; Barton Willis
;; Department of Mathematics
;; University of Nebraska at Kearney
;; Kearney NE 68847
;; willisb@unk.edu

;; This source code is licensed under the terms of the Lisp Lesser
;; GNU Public License (LLGPL). The LLGPL consists of a preamble, published
;; by Franz Inc. (http://opensource.franz.com/preamble.html), and the GNU
;; Library General Public License (LGPL), version 2, or (at your option)
;; any later version.  When the preamble conflicts with the LGPL,
;; the preamble takes precedence.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Library General Public License for details.

;; You should have received a copy of the GNU Library General Public
;; License along with this library; if not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301, USA.

(in-package :maxima)

(macsyma-module nummod)

;; Let's have version numbers 1,2,3,...

(eval-when (:load-toplevel :execute)
  (mfuncall '$declare '$integervalued '$feature)
  ($put '$nummod 3 '$version))

(defmacro opcons (op &rest args)
  `(simplify (list (list ,op) ,@args)))

;; charfun(pred) evaluates to 1 when the predicate 'pred' evaluates
;; to true; it evaluates to 0 when 'pred' evaluates to false; otherwise,
;; it evaluates to a noun form.

(defprop $charfun simp-charfun operators)
(defprop %charfun simp-charfun operators)

(defun simp-charfun (e yy z)
  (declare (ignore yy))
  (oneargcheck e)
  (setq e (take '($is) (simplifya (specrepcheck (second e)) z)))
  (let ((bool (mevalp e)))
    (cond ((eq t bool) 1)
	  ((eq nil bool) 0)
	  ((op-equalp e '$is) `(($charfun simp) ,(second e)))
	  (t `(($charfun simp) ,e)))))
  
(defun integer-part-of-sum (e)
  (let ((i-sum 0) (n-sum 0) (o-sum 0) (n))
    (setq e (margs e))
    (dolist (ei e)
      (cond ((maxima-integerp ei)
	     (setq i-sum (add ei i-sum)))
	    ((or (ratnump ei) (floatp ei) ($bfloatp ei))
	     (setq n-sum (add ei n-sum)))
	    (t
	     (setq o-sum (add ei o-sum)))))
    (setq n (opcons '$floor n-sum))
    (setq i-sum (add i-sum n))
    (setq o-sum (add o-sum (sub n-sum n)))
    (values i-sum o-sum)))

(defprop $floor simp-floor operators)

(defprop $floor tex-matchfix tex)
(defprop $floor (("\\left \\lfloor " ) " \\right \\rfloor") texsym)

;; $floor distributes over lists, matrices, and equations
(setf (get '$floor 'distribute_over) '(mlist $matrix mequal))

; These defprops for $entier are copied from orthopoly/orthopoly-init.lisp.

(defprop $entier tex-matchfix tex)
(defprop $entier (("\\lfloor ") " \\rfloor") texsym)

;; $entier and $fix distributes over lists, matrices, and equations
(setf (get '$entier 'distribute_over) '(mlist $matrix mequal))
(setf (get '$fix 'distribute_over) '(mlist $matrix mequal))

;; For an example, see pretty-good-floor-or-ceiling. Code courtesy of Stavros Macrakis.

(defmacro bind-fpprec (val &rest exprs)
  `(let (fpprec bigfloatzero bigfloatone bfhalf bfmhalf)
     (let (($fpprec (fpprec1 nil ,val)))
       ,@exprs)))

;; Return true if the expression can be formed using rational numbers, logs, mplus, mexpt, or mtimes.

(defun use-radcan-p (e)
  (or ($ratnump e) (and (op-equalp e '%log 'mexpt 'mplus 'mtimes) (every 'use-radcan-p (cdr e)))))

;; When constantp(x) is true, we use bfloat evaluation to try to determine
;; the ceiling or floor. If numerical evaluation of e is ill-conditioned, this function
;; can misbehave.  I'm somewhat uncomfortable with this, but it is no worse
;; than some other stuff. One safeguard -- the evaluation is done with three
;; values for fpprec.  When the floating point evaluations are
;; inconsistent, bailout and return nil.  I hope that this function is
;; much more likely to return nil than it is to return a bogus value.

(defun pretty-good-floor-or-ceiling (x fn &optional digits)
  (let (($float2bf t) ($algebraic t) (f1) (f2) (f3) (eps) (lb) (ub) (n))
    
    (setq digits (if (and (integerp digits) (> 0 digits)) digits 25))
    (catch 'done

      ;; To handle ceiling(%i^%i), we need to apply rectform. If bfloat
      ;; is improved, it might be possible to remove this call to rectform.

      (setq x ($rectform x))
      (if (not ($freeof '$%i '$minf '$inf '$und '$infinity x)) (throw 'done nil))

      ;; When x doesn't evaluate to a bigfloat, bailout and return nil.
      ;; This happens when, for example, x = asin(2). For now, bfloatp
      ;; evaluates to nil for a complex big float. If this ever changes,
      ;; this code might need to be repaired.
      
      (bind-fpprec digits 
		   (setq f1 ($bfloat x))
		   (if (not ($bfloatp f1)) (throw 'done nil)))
		   
      (incf digits 20)
      (setq f2 (bind-fpprec digits ($bfloat x)))
      (if (not ($bfloatp f2)) (throw 'done nil))

      (incf digits 20)
      (bind-fpprec digits 
		   (setq f3 ($bfloat x))
		   (if (not ($bfloatp f3)) (throw 'done nil))

		   ;; Let's say that the true value of x is in the interval
		   ;; [f3 - |f3| * eps, f3 + |f3| * eps], where eps = 10^(20 - digits).
		   ;; Define n to be the number of integers in this interval; we have
		   
		   (setq eps (power ($bfloat 10) (- 20 digits)))
		   (setq lb (sub f3 (mult (take '(mabs) f3) eps)))
		   (setq ub (add f3 (mult (take '(mabs) f3) eps)))
		   (setq n (sub (take '($ceiling) ub) (take '($ceiling) lb))))
      
      (setq f1 (take (list fn) f1))
      (setq f2 (take (list fn) f2))
      (setq f3 (take (list fn) f3))
      
      ;; Provided f1 = f2 = f3 and n = 0, return f1; if n = 1 and (use-radcan-p e) and ($radcan e)
      ;; is a $ratnump, return floor / ceiling of radcan(x),
      
      (cond ((and (= f1 f2 f3) (= n 0)) f1)
	    ((and (=  f1 f2 f3) (= n 1) (use-radcan-p x))
	     (setq x ($radcan x))
	     (if ($ratnump x) (take (list fn) x) nil))
	    (t nil)))))


;; (a) The function fpentier rounds a bigfloat towards zero--we need to
;;     check for that.

;; (b) Since limit(f(x))=(m)inf implies limit(floor(f(x)))=(m)inf,
;;     floor(inf/minf) = inf/minf.  Since minf<limit(f(x))<inf implies
;;     minf<limit(floor(f(x)))<inf, floor(ind)=ind

;;     I think floor(complex number) should be undefined too.  Some folks
;;     might like floor(a + %i b) --> floor(a) + %i floor(b). But
;;     this would violate the integer-valuedness of floor.
;;     So floor(infinity) remains unsimplified

(defun simp-floor (e e1 z)
  (oneargcheck e)
  (setq e (simplifya (specrepcheck (nth 1 e)) z))

  (cond ((numberp e) (floor e))

	((ratnump e) (floor (cadr e) (caddr e)))

	(($bfloatp e)
	 (setq e1 (fpentier e))
	 (if (and (minusp (cadr e)) (not (zerop1 (sub e1 e))))
	     (1- e1)
	     e1))

	((maxima-integerp e) e)

	(($orderlessp e (neg e))
	 (sub 0 (opcons '$ceiling (neg e))))

	((and (setq e1 (mget e '$numer)) (floor e1)))

	((or (member e infinities) (eq e '$und) (eq e '$ind)) '$und)
	;;((member e '($inf $minf $ind $und)) e) ; Proposed code
	;; leave floor(infinity) as is
	((or (eq e '$zerob)) -1)
	((or (eq e '$zeroa)) 0)

	((and ($constantp e) (pretty-good-floor-or-ceiling e '$floor)))

	((mplusp e)
	 (let ((i-sum) (o-sum))
	   (multiple-value-setq (i-sum o-sum) (integer-part-of-sum e))
	   (setq o-sum (if (equal i-sum 0) `(($floor simp) ,o-sum)
			 (opcons '$floor o-sum)))
	   (add i-sum o-sum)))

	;; handle 0 < e < 1 implies floor(e) = 0 and
	;; -1 < e < 0 implies floor(e) = -1.

	((and (equal ($compare 0 e) "<") (equal ($compare e 1) "<")) 0)
	((and (equal ($compare -1 e) "<") (equal ($compare e 0) "<")) -1)
	(t `(($floor simp) ,e))))

(defun floor-integral (x)
  (let ((f (take '($floor) x)))
    (mul f (div 1 2) (add (mul 2 x) -1 (neg f)))))

(defun ceiling-integral (x)
  (let ((f (take '($ceiling) x)))
    (mul f (div 1 2) (add 1 (mul 2 x) (neg f)))))

(putprop '$floor `((x) ,#'floor-integral) 'integral)
(putprop '$ceiling `((x) ,#'ceiling-integral) 'integral)

(defprop $ceiling simp-ceiling operators)

(defprop $ceiling tex-matchfix tex)
(defprop $ceiling (("\\left \\lceil " ) " \\right \\rceil") texsym)

;; $ceiling distributes over lists, matrices, and equations.
(setf (get '$ceiling 'distribute_over) '(mlist $matrix mequal))

(defun simp-ceiling (e e1 z)
  (oneargcheck e)
  (setq e (simplifya (specrepcheck (nth 1 e)) z))
  (cond ((numberp e) (ceiling e))

	((ratnump e) (ceiling (cadr e) (caddr e)))

	(($bfloatp e)
	 (sub 0 (opcons '$floor (sub 0 e))))

	((maxima-integerp e) e)

	(($orderlessp e (neg e))
	 (sub* 0 (opcons '$floor (neg e))))

	((and (setq e1 (mget e '$numer)) (ceiling e1)))

	((or (member e infinities) (eq e '$und) (eq e '$ind)) '$und)
	((or (eq e '$zerob)) 0)
	((or (eq e '$zeroa)) 1)

	((and ($constantp e) (pretty-good-floor-or-ceiling e '$ceiling)))

	((mplusp e)
	 (let ((i-sum) (o-sum))
	   (multiple-value-setq (i-sum o-sum) (integer-part-of-sum e))
	   (setq o-sum (if (equal i-sum 0) `(($ceiling simp) ,o-sum)
			 (opcons '$ceiling o-sum)))
	   (add i-sum o-sum)))

	;; handle 0 < e < 1 implies ceiling(e) = 1 and
	;; -1 < e < 0 implies ceiling(e) = 0.

	((and (equal ($compare 0 e) "<") (equal ($compare e 1) "<")) 1)
	((and (equal ($compare -1 e) "<") (equal ($compare e 0) "<")) 0)
	(t `(($ceiling simp) ,e))))

(defprop $mod simp-nummod operators)
(defprop $mod tex-infix tex)
(defprop $mod (" \\rm{mod} ") texsym)
(defprop $mod 180. tex-rbp)
(defprop $mod 180. tex-rbp)

;; $mod distributes over lists, matrices, and equations
(setf (get '$mod 'distribute_over) '(mlist $matrix mequal))

;; See "Concrete Mathematics," Section 3.21.
	     
(defun simp-nummod (e e1 z)
  (twoargcheck e)
  (let ((x (simplifya (specrepcheck (cadr e)) z))
	(y (simplifya (specrepcheck (caddr e)) z)))
    (cond ((or (equal 0 y) (equal 0 x)) x)
	  ((equal 1 y) (sub x (opcons '$floor x)))
	  ((and ($constantp x) ($constantp y))
	   (sub x (mul y (opcons '$floor (div x y)))))
	  ((not (equal 1 (setq e1 ($gcd x y))))
	   (mul e1 (opcons '$mod (div x e1) (div y e1))))
	  (t `(($mod simp) ,x ,y)))))

;; The function 'round' rounds a number to the nearest integer. For a tie, round to the 
;; nearest even integer. 

(defprop %round simp-round operators)
(setf (get '%round 'integer-valued) t)
(setf (get '%round 'reflection-rule) #'odd-function-reflect)
(setf (get '$round 'alias) '%round)
(setf (get '$round 'verb) '%round)
(setf (get '%round 'noun) '$round)

;; $round distributes over lists, matrices, and equations.
(setf (get '$round 'distribute_over) '(mlist $matrix mequal))

(defun simp-round (e yy z)
  (oneargcheck e)
  (setq yy (caar e)) ;; find a use for the otherwise unused YY.
  (setq e (simplifya (specrepcheck (second e)) z))
  (cond (($featurep e '$integer) e) ;; takes care of round(round(x)) --> round(x).
	((member e '($inf $minf $und $ind) :test #'eq) e)
	(t 
	 (let* ((lb (take '($floor) e))
		(ub (take '($ceiling) e))
		(sgn (csign (sub (sub ub e) (sub e lb)))))
	   (cond ((eq sgn t) `((,yy simp) ,e))
		 ((eq sgn '$neg) ub)
		 ((eq sgn '$pos) lb)
		 ((alike lb ub) lb) ;; For floats that are integers, this can happen. Maybe featurep should catch this.
		 ((and (eq sgn '$zero) ($featurep lb '$even)) lb)
		 ((and (eq sgn '$zero) ($featurep ub '$even)) ub)
		 ((apply-reflection-simp yy e t))
		 (t `((,yy simp) ,e)))))))
 
;; Round a number towards zero.

(defprop %truncate simp-truncate operators)
(setf (get '%truncate 'integer-valued) t)
(setf (get '%truncate 'reflection-rule) #'odd-function-reflect)
(setf (get '$truncate 'alias) '%truncate)
(setf (get '$truncate 'verb) '%truncate)
(setf (get '%truncate 'noun) '$truncate)

(defun simp-truncate (e yy z)
  (oneargcheck e)
  (setq yy (caar e)) ;; find a use for the otherwise unused YY.
  (setq e (simplifya (specrepcheck (second e)) z))
  (cond (($featurep e '$integer) e) ;; takes care of truncate(truncate(x)) --> truncate(x).
	((member e '($inf $minf $und $ind) :test #'eq) e)
	(t
	 (let ((sgn (csign e)))
	   (cond ((member sgn '($neg $nz) :test #'eq) (take '($ceiling) e))
		 ((member sgn '($zero $pz $pos) :test #'eq) (take '($floor) e))
		 ((apply-reflection-simp yy e t))
		 (t `((,yy simp) ,e)))))))
