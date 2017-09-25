;; Maxima functions for finding the maximum or minimum
;; Copyright (C) 2005, 2007 Barton Willis

;; Barton Willis
;; Department of Mathematics, 
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
(macsyma-module maxmin)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ($put '$trylevel 1 '$maxmin)  ;; Default: only use basic simplification rules
  ($put '$maxmin 1 '$version))  ;; Let's have version numbers 1,2,3,...

;; Return true if there is pi in the CL list p and qi in the CL lisp q such that
;; x is between pi and qi.  This means that either pi <= x <= qi or
;; qi <= x <= pi. For example, 2x is between x and 3x.

;; Strangely, sign((a-b)*(b-a)) --> pnz but sign(expand((a-b)*(b-a))) --> nz.
;; This is the reason for the $expand.

;; The betweenp simplification is done last; this has some interesting effects:
;; max(x^2,x^4,x^6,x^2+1) (standard simplification) --> max(x^4,x^6,x^2+1) 
;; (betweenp) --> max(x^4,x^6,x^2+1). If the betweenp simplification were done 
;; first, we'd have max(x^2,x^4,x^6,x^2+1) --> max(x^2,x^6,x^2+1) --> max(x^6,x^2+1).

(defun betweenp (x p q)
  (catch 'done
      (dolist (pk p)
	(dolist (qk q)
	  (if (member (csign ($expand (mul (sub x pk) (sub qk x)))) '($pos $pz) :test #'eq) (throw 'done t))))
      nil))
	  	       
;; Return true if y is the additive inverse of x. 

(defun add-inversep (x y)
  (eq t (meqp x (neg y))))

;; Define a simplim%function to handle a limit of $max.

(defprop $max simplim$max simplim%function)

(defun simplim$max (expr var val)
  (cons '($max) (mapcar #'(lambda (e) (limit e var val 'think)) (cdr expr))))

;; When get(trylevel,maxmin) is two or greater, max and min try additional 
;; O(n^2) and O(n^3) methods.
 
;; Undone:  max(1-x,1+x) - max(x,-x) --> 1.

(defprop $max simp-max operators)

(defun simp-max (l tmp z)
  (let ((acc nil) (sgn) (num-max nil) (issue-warning))
    (setq l (margs (specrepcheck l)))
    (dolist (li l)
      (if (op-equalp li '$max) (setq acc (append acc (mapcar #'(lambda (s) (simplifya s z)) (margs li))))
	(push (simplifya li z) acc)))
    
    ;; First, delete duplicate members of l.
    
    (setq l (sorted-remove-duplicates (sort acc '$orderlessp)))
    (setq acc nil)
    
    ;; Second, find the largest real number in l. Since (mnump '$%i) --> false, we don't 
    ;; have to worry that num-max is complex. 
    
    (dolist (li l)
      (if (mnump li) (setq num-max (if (or (null num-max) (mgrp li num-max)) li num-max)) (push li acc)))
    (setq l acc)
    (setq acc (if (null num-max) num-max (list num-max)))
    
    ;; Third, accumulate the maximum in the list acc. For each x in l, do:
    
    ;; (a) if x is > or >= every member of acc, set acc to (x),
    ;; (b) if x is < or <= to some member of acc, do nothing,
    ;; (c) if neither 'a' or 'b', push x into acc,
    ;; (d) if x cannot be compared to some member of acc, set issue-warning to true.
    
    (dolist (x l)
      (catch 'done
	(dolist (ai acc)
	  (setq sgn ($compare x ai))
	  (cond ((member sgn '(">" ">=") :test #'equal)
		 (setq acc (delete ai acc :test #'eq)))
		((eq sgn '$notcomparable) (setq issue-warning t))
		((member sgn '("<" "=" "<=") :test #'equal)
		 (throw 'done t))))
             (push x acc)))
    
    ;; Fourth, when when trylevel is 2 or higher e and -e are members of acc, replace e by |e|.
    
    (cond ((eq t (mgrp ($get '$trylevel '$maxmin) 1))
           (let ((flag nil))
             (setq sgn nil)
             (dolist (ai acc)
               (setq tmp (if (lenient-realp ai)
                             (member-if #'(lambda (s) (add-inversep ai s)) sgn)
                             nil))
               (cond (tmp
                      (setf (car tmp) (take '(mabs) ai))
                      (setq flag t))
                     (t (push ai sgn))))
             (if flag
                 ;; We have replaced -e and e with |e|. Call simp-max again.
                 (return-from simp-max (simplify (cons '($max) sgn)))
                 (setq acc sgn)))))
 
    ;; Fifth, when trylevel is 3 or higher and issue-warning is false, try the
    ;; betweenp simplification.

    (cond ((and (not issue-warning) (eq t (mgrp ($get '$trylevel '$maxmin) 2)))
	   (setq l nil)
	   (setq sgn (cdr acc))
	   (dolist (ai acc)
	     (if (not (betweenp ai sgn sgn)) (push ai l))
	     (setq sgn `(,@(cdr sgn) ,ai)))
	   (setq acc l)))

    ;; Finally, do a few clean ups:
    
    (setq acc (if (not issue-warning) (delete '$minf acc) acc))
    (cond ((null acc) '$minf)
          ((and (not issue-warning) (member '$inf acc :test #'eq)) '$inf)
          ((null (cdr acc)) (car acc))
          (t  `(($max simp) ,@(sort acc '$orderlessp))))))

(defun limitneg (x)
  (cond ((eq x '$minf) '$inf)
	((eq x '$inf) '$minf)
	((member x '($und $ind $infinity) :test #'eq) x)
	(t (neg x))))

;; Define a simplim%function to handle a limit of $min.

(defprop $min simplim$min simplim%function)

(defun simplim$min (expr var val)
  (cons '($min) (mapcar #'(lambda (e) (limit e var val 'think)) (cdr expr))))

(defprop $min simp-min operators)

(defun simp-min (l tmp z)
  (declare (ignore tmp))
  (let ((acc nil))
    (setq l (margs (specrepcheck l)))
    (dolist (li l)
      (if (op-equalp li '$min) (setq acc (append acc (mapcar #'(lambda (s) (simplifya s z)) (margs li))))
	(push (simplifya li z) acc)))
    (setq l acc)
    (setq l (mapcar #'limitneg acc))
    (setq l (simplify `(($max) ,@l)))
    (if (op-equalp l '$max)
	`(($min simp) ,@(mapcar #'limitneg (margs l))) (limitneg l))))

;; Several functions (derivdegree for example) use the maximin function. Here is 
;; a replacement that uses simp-min or simp-max.

(defun maximin (l op) (simplify `((,op) ,@l)))
 
(defmfun $lmax (e)
  (simplify `(($max) ,@(require-list-or-set e "$lmax")))) 

(defmfun $lmin (e)
  (simplify `(($min) ,@(require-list-or-set e "$lmin"))))

;; Return the narrowest comparison operator op (<, <=, =, >, >=) such that
;; a op b evaluates to true. Return 'unknown' when either there is no such 
;; operator or when  Maxima's sign function isn't powerful enough to determine
;; such an operator; when Maxima is able to show that either argument is not 
;; real valued, return 'notcomparable.'

;; The subtraction can be a problem--for example, compare(0.1, 1/10)
;; evaluates to "=". But for flonum floats, I believe 0.1 > 1/10. 
;; If you want to convert flonum and big floats to exact rational
;; numbers, use $rationalize.

;; I think compare(asin(x), asin(x) + 1) should evaluate to < without
;; being quizzed about the sign of x. Thus the call to lenient-extended-realp.

(defun $compare (a b)
  ;; Simplify expressions with infinities, indeterminates, or infinitesimals
  (when (amongl '($ind $und $inf $minf $infinity $zeroa $zerob) a)
    (setq a ($limit a)))
  (when (amongl '($ind $und $inf $minf $infinity $zeroa $zerob) b)
    (setq b ($limit b)))
  (cond ((or (amongl '($infinity $ind $und) a)
             (amongl '($infinity $ind $und) b))
         ;; Expressions with $infinity, $ind, or $und are not comparable
         '$notcomparable)
        ((eq t (meqp a b)) "=")
        ((or (not (lenient-extended-realp a))
             (not (lenient-extended-realp b)))
         '$notcomparable)
	(t
	 (let ((sgn (csign (specrepcheck (sub a b)))))
	   (cond ((eq sgn '$neg) "<")
		 ((eq sgn '$nz) "<=")
		 ((eq sgn '$zero) "=")
		 ((eq sgn '$pz) ">=")
		 ((eq sgn '$pos) ">")
		 ((eq sgn '$pn) "#")
		 ((eq sgn '$pnz) '$unknown)
		 (t '$unknown))))))

;; When it's fairly likely that the real domain of e is nonempty, return true; 
;; otherwise, return false. Even if z has been declared complex, the real domain
;; of z is nonempty; thus (lenient-extended-realp z) --> true.  When does this
;; function lie?  One example is acos(abs(x) + 2). The real domain of this 
;; expression is empty, yet lenient-extended-realp returns true for this input.

(defun lenient-extended-realp (e)
  (and ($freeof '$infinity '$%i '$und '$ind '$false '$true t nil e) ;; what else?
       (not (mbagp e))
       (not ($featurep e '$nonscalarp))
       (not (mrelationp e))
       (not ($member e $arrays))))

(defun lenient-realp (e)
  (and ($freeof '$inf '$minf e) (lenient-extended-realp e)))

;; Convert all floats and big floats in e to an exact rational representation. 

(defun $rationalize (e)
  (setq e (ratdisrep e))
  (cond ((floatp e) 
	 (let ((significand) (expon) (sign))
	   (multiple-value-setq (significand expon sign) (integer-decode-float e))
	   (cl-rat-to-maxima (* sign significand (expt 2 expon)))))
	(($bfloatp e) (cl-rat-to-maxima (* (cadr e)(expt 2 (- (caddr e) (third (car e)))))))
	(($mapatom e) e)
	(t (simplify (cons (list (mop e)) (mapcar #'$rationalize (margs e)))))))

