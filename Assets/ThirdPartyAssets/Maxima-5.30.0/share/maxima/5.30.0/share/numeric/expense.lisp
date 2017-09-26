;;; -*- Mode: Lisp; Package: Macsyma -*-                                 ;;;
;;;    (c) Copyright 1984 the Regents of the University of California.   ;;;
;;;        All Rights Reserved.                                          ;;;
;;;        This work was produced under the sponsorship of the           ;;;
;;;        U.S. Department of Energy.  The Government retains            ;;;
;;;        certain rights therein.                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macsyma-module expens)

(defmvar $cost_reciprocal 4
         "The expense of computing a floating point reciprocal in terms of
  scalar floating point additions on the CRAY-1(Note that this can be redefined
  for vector mode on the CRAY-1, another computer, or put in terms of absolute
  machine cycles.  However, all COST_-type variables would need to be
  consistently redefined.  Note further that EXPENSE would probably need to
  be completely rethought for a multiprocessor or data-flow machine)."
         fixnum
         modified-commands '$expense)

(defmvar $cost_divide 5
         "The expense of computing a floating point divide in terms of
  scalar floating point additions on the CRAY-1(For further discussion do:
  DESCRIBE(COST_RECIPROCAL) )."
         fixnum
         modified-commands '$expense)

(defmvar $cost_sqrt 29.
         "The expense of computing a floating point square root in terms of
  scalar floating point additions on the CRAY-1(For further discussion do:
  DESCRIBE(COST_RECIPROCAL) )."
         fixnum
         modified-commands '$expense)

(defmvar $cost_exp 30.
         "The expense of computing a floating point exponentiation in terms
  of scalar floating point additions on the CRAY-1(For further discussion do:
  DESCRIBE(COST_RECIPROCAL) )."
         fixnum
         modified-commands '$expense)

(defmvar $cost_sin_cos_log 32.
         "The expense of computing a floating point SIN, COS, or LOG in
  terms of scalar floating point additions on the CRAY-1.  Note that this
  is a mean of sorts for the three operations(For further discussion do:
  DESCRIBE(COST_RECIPROCAL) )."
         fixnum
         modified-commands '$expense)

(defmvar $cost_float_power (+ $cost_exp $cost_sin_cos_log)
         "The expense of computing a floating point power in terms of scalar
  floating point additions on the CRAY-1(For further discussion do:
  DESCRIBE(COST_RECIPROCAL) )."
         fixnum
         modified-commands '($expense $gather_exponents))

(defmvar $cost_hyper_trig 35.
         "The expense of computing a floating point ARCSIN, ARCCOS, ARCTAN,
  SINH, COSH, TANH, or TAN in terms of scalar floating point additions on the
  CRAY-1.  Note that this is a mean of sorts for these 7 different operations.
  (For further discussion do: DESCRIBE(COST_RECIPROCAL) )."
         fixnum
         modified-commands '$expense)

(defmvar $merge_ops '((mlist simp) $cvmgp $cvmgt)
         "A MACSYMA list of currently known CRAY-1 vector merge operations."
         modified-commands '($block_optimize $expense))

(defun multiplies-in-nth-power (nth)
   (cond ((< nth 2) 0)
         (t
          (let ((slow (bignump nth)))
            (do ((exin nth (cond (slow (- exin (* pw2 rem)))
                                 (t (- exin (* pw2 rem)))))
                 (rem 0)
                 (in-cut -2 (+ 1 in-cut rem))
                 (pw2 1 (cond (slow (+ pw2 pw2))
                              (t (+ pw2 pw2)))))
                ((or (zerop exin) (> in-cut $cost_float_power))
                 (cond ((< in-cut $cost_float_power) in-cut)
                       (t $cost_float_power)))
              (declare (fixnum exin rem in-cut pw2))
              (setq rem (cond (slow (rem (quotient exin pw2) 2))
                              (t (/ (truncate exin pw2) 2)))))))))

;;; the following macro is courtesy of gjc.

(defmacro eval&reduce (oper eval list
                            &aux (temp (gensym))
                            (val (gensym)))
          `(let ((,temp ,list))
             (do ((,val (funcall ,eval (pop ,temp))
			(funcall ,oper ,val (funcall ,eval (pop ,temp)))))
                 ((null ,temp) ,val))))

(defun $expense (x)
  (cond (($mapatom x) 0)
        (t (let ((opr (caar x)))
             (cond ((member opr '(mplus mtimes) :test #'eq)
                    (let ((cl (+ (- (length x) 2)
                                 (eval&reduce '+ '$expense (cdr x)))))
                      (cond ((and (eq opr 'mtimes) (equal -1 (cadr x))) (1- cl))
                            (t cl))))
                   ((eq opr 'mexpt)
                    (let ((expon (caddr x)))
                      (+ ($expense (cadr x))
                         (cond ((integerp expon)
                                (cond ((< expon 0)
                                       (+ $cost_reciprocal
                                          (multiplies-in-nth-power (- expon))))
                                      (t (multiplies-in-nth-power expon))))
                               (t (+ ($expense expon)
                                     $cost_exp
                                     (cond ((eq (cadr x) '$%e) 0)
                                           (t $cost_sin_cos_log))))))))
                   ((eq opr 'mminus) ($expense (cadr x)))
                   ((eq opr '%sqrt) (+ $cost_sqrt ($expense (cadr x))))
                   ((member opr $merge_ops :test #'eq) (+ 4
                                             ($expense (cadr x))
                                             ($expense (caddr x))
                                             ($expense (cadddr x))))
                   ((eq opr 'mquotient)
                    (cond ((member (cadr x) '(1 -1))
                           (+ $cost_reciprocal ($expense (caddr x))))
                          (t (+ (* $cost_divide (- (length x) 2))
                                (eval&reduce '+ '$expense (cdr x))))))
                   ((member opr '(%acos %asin %atan %cosh %sinh %tan %tanh) :test #'eq)
                    (+ $cost_hyper_trig ($expense (cadr x))))
                   ((member opr '(%cos %log %sin) :test #'eq)
                    (+ $cost_sin_cos_log ($expense (cadr x))))
                   ((eq opr '$atan2)
                    (+ $cost_hyper_trig ($expense (cadr x)) ($expense (caddr x))))
                   (t
                    (mformat nil "Beware: ~M is not in function base of EXPENSE~%" opr)
                    (eval&reduce '+ '$expense (cdr x))))))))
