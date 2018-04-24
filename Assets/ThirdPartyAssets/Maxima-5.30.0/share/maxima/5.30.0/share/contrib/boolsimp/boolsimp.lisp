; Simplification and evaluation of Boolean and conditional expressions.
; 
; Copyright 2006 by Robert Dodier.
; Released under the terms of the GNU General Public License.

; The functions in this file are an attempt to make Boolean (and, or, not)
; and conditional (if -- then -- else/elseif) expressions work more like
; arithmetic expressions in the treatment of predicates which are
; undecidable at the time the expression is evaluated,
; by allowing undecided predicates in simplified and evaluated
; expressions, instead of complaining or returning 'unknown.

; Ideas that haven't gone anywhere yet:
;  - given "if foo then bar else baz", simplify bar assuming foo,
;    and simplify baz assuming not foo
;  - flatten conditionals -- nested if --> if -- elseif -- elseif -- elseif -- else
;  - arithmetic on conditionals -- distribute arithmetic ops over if
;  - make up rules via tellsimp & friends for integrate / sum / diff applied to conditionals
;  - knock out redundant clauses in simplification (i.e. if x implies y then knock out y)

; Examples:
;
; assume (a > 1);
;
;                        Before      After
;
; a > 1 and b < 0   =>   error       b < 0
; c > 1 and b < 0   =>   error       c > 1 and b < 0
; not b < 0         =>   error       b >= 0
; if c then d       =>   error       if c then d
; plot2d (if x > 0 then x else -x, [x, -1, 1])
;                   =>   error       nice plot
; quad_qags (if x > 0 then x else -x, x, -1 ,1)
;                   =>   error       [1.0, 1.1107651257113993E-14, 63, 0]


; In and, or, and if, arguments are evaluated, or simplified as the case may be,
; from left to right, only as needed to establish whether the whole expression
; is true or false. Therefore arguments which potentially have side effects
; (print, kill, save, quit, etc) may or may not actually have those side effects.
;
; Simplification of "if" expressions:
;
; Let the expression be if P[1] then E[1] elseif P[2] then E[2] ... elseif P[n] then E[n]
; ("if P[1] then E[1] else E[2]" parses to the above with P[2] = true,
; and "if P[1] then E[1]" parses to the above with P[2] = true and E[2] = false.)
;
; (1) If any P[k] simplifies to false, do not simplify E[k],
;     and omit P[k] and E[k] from the result.
; (2) If any P[k] simplifies to true, simplify E[k],
;     but do not simplify any P[k + 1], E[k + 1], ..., and omit them from the result.
; (3) Otherwise, simplify E[k].
;
; If there are no P and E remaining, return false.
; Let P*[1], E*[1], ... be any P and E remaining after applying (1), (2), and (3).
; If P*[1] = true, return E*[1].
; Otherwise return "if P*[1] then E*[1] elseif P*[2] then E*[2] ..."
; with "if" being a noun iff the original "if" was a noun.
;
; Evaluation of "if" expressions:
;
; (1) If any P[k] evaluates to false, do not evaluate E[k],
;     and omit P[k] and E[k] from the result.
; (2) If any P[k] evaluates to true, evaluate E[k],
;     but do not evaluate any P[k + 1], E[k + 1], ..., and omit them from the result.
; (3) Otherwise, evaluate atoms (not function calls) in E[k].
;
; If there are no P and E remaining, return false.
; Let P*[1], E*[1], ... be any P and E remaining after applying (1), (2), and (3).
; If P*[1] = true, return E*[1].
; Otherwise return "if P*[1] then E*[1] elseif P*[2] then E*[2] ..."
; with "if" being a noun iff the original "if" was a noun.

(in-package :maxima)

; Kill off translation properties of conditionals and Boolean operators.
; Ideally we could avoid calling MEVAL when arguments are declared Boolean.
; We're not there yet.

(remprop 'mcond 'translate)

; It's OK for MEVALATOMS to evaluate the arguments of MCOND.
; %MCOND already has this property.
(defprop mcond t evok)

; X is an expression of the form ((OP) B1 G1 B2 G2 B3 G3 ...)
; where OP is MCOND or %MCOND,
; and B1, B2, B3, ... are boolean expressions,
; and G1, G2, G3, ... are general expressions.

; Evaluation of conditional expressions.

; If any B evaluates to T,
; then append B and G to the list of evaluated arguments, and ignore any remaining arguments.
; If any B evaluates to NIL,
; then ignore B and G, and continue processing arguments.
; Otherwise, append B and G to the list of evaluated arguments, and continue processing arguments.

; If the first element of the list of evaluated arguments is T,
; return the second element.
; Otherwise, construct a new conditional expression from the evaluated arguments.

(defmspec mcond (x)
  (let ((op (car x)) (args (cdr x)) (conditions) (consequences))
    (loop while (> (length args) 0) do
      (let* ((b (car args)) (g (cadr args)) (v (mevalp b)))
        (cond 
          ((eq v t)
           (setq conditions (append conditions (list v)))
           (setq consequences (append consequences (list g)))
           (setq args nil))
          ((eq v nil)
           nil)
          (t
            (setq conditions (append conditions (list v)))
            (setq consequences (append consequences (list g)))))
        (setq args (cddr args))))

    (cond
      ((eq (car conditions) t)
       (meval (car consequences)))
      (t
        ; RESIMPLIFY since MEVALATOMS might yield expressions which can be simplified
        (setq consequences (mapcar 'resimplify (mapcar 'mevalatoms consequences)))
        ; Burn off SIMP flag, if any, when constructing the new CAAR
        (cons `(,(car op))
              (apply 'append (mapcar #'(lambda (x y) `(,x ,y)) conditions consequences)))))))

; Simplification of conditional expressions.

; If any B simplifies to T or $TRUE,
; then append B and G to the list of simplified arguments, and ignore any remaining arguments.
; If any B simplifies to NIL or $FALSE, 
; then ignore B and G, and continue processing arguments.
; Otherwise, append B and G to the list of simplified arguments, and continue processing arguments.

; If there are no arguments remaining (i.e., expression is just ((MCOND)) or ((%MCOND))) return $FALSE.
; If the first element of the list of simplified arguments is T or $TRUE, return the second element.
; Otherwise, construct a new conditional expression from the simplified arguments.

(defun simp-mcond-shorten (x z)
  (let ((op (car x)) (args (cdr x)) (args-simplified))
    (loop while (> (length args) 0) do
      (let ((b (maybe-simplifya (car args) z)) (g (cadr args)))
        (cond
          ((or (eq b nil) (eq b '$false)) nil)
          ((or (eq b t) (eq b '$true))
           (setq args-simplified (append args-simplified (list b (maybe-simplifya g z)))
                 args nil))
          (t
            (setq args-simplified (append args-simplified (list b (maybe-simplifya g z)))))))
      (setq args (cddr args)))
    
    (cond
      ((null args-simplified) nil)
      ((or (eq (car args-simplified) t) (eq (car args-simplified) t))
       (cadr args-simplified))
      (t
        ; Indicate that the return value has been simplified.
        (cons `(,(car op) simp) args-simplified)))))

(defun simp-mcond (x y z)
  (declare (ignore y))
  (simp-mcond-shorten x z))

(putprop 'mcond 'simp-mcond 'operators)
(putprop '%mcond 'simp-mcond 'operators)

; WTF IS THIS ??
(let ((save-intext (symbol-function 'intext)))
  (defun intext (rel body)
    (let ((result (funcall save-intext rel body)))
      (if result result `((,rel) ,@body)))))


; $SOME / $EVERY REDEFINED FROM SRC/NSET.LISP

#|
(defun $every (f &rest x)
  (cond ((or (null x) (and (null (cdr x)) ($emptyp (first x)))) t)
   
 ((or ($listp (first x)) (and ($setp (first x)) (null (cdr x))))
  (setq x (margs (simplify (apply #'map1 (cons f x)))))
  ; ACTUALLY WE REALLY REALLY WANT TO POSTPONE EVALUATING THE PREDICATE HERE
  (setq x (mapcar #'car (mapcar #'(lambda (s) (ignore-errors-mfuncall '$maybe s)) x)))
  ; IF MAND RETURNS AN UNEVALUATED EXPRESSION HERE, RETURN AN UNEVALUATED EXPR WITH OP = $EVERY
  (let ((a (simplifya `((mand) ,@x) t)))
    ; FOR NOW ASSUME NIL IF ANYTHING BUT T OR $UNKNOWN (DON'T CHANGE $EVERY NOW)
    (if (or (eq a t) (eq a '$unknown)) a nil)))
   
 ((every '$matrixp x)
  (let ((fmaplvl 2))
    (setq x (margs (simplify (apply #'fmapl1 (cons f x)))))
    (setq x (mapcar #'(lambda (s) ($every '$identity s)) x))
    ; IF MAND RETURNS AN UNEVALUATED EXPRESSION HERE, RETURN AN UNEVALUATED EXPR WITH OP = $EVERY
    (let ((a (simplifya `((mand) ,@x) t)))
      ; FOR NOW ASSUME NIL IF ANYTHING BUT T OR $UNKNOWN (DON'T CHANGE $EVERY NOW)
      (if (or (eq a t) (eq a '$unknown)) a nil))))
 
 (t (merror "Improper arguments to function 'every'"))))

(defun $some (f &rest x)
  (cond ((or (null x) (and (null (cdr x)) ($emptyp (first x)))) nil)

 ((or ($listp (first x)) (and ($setp (first x)) (null (cdr x))))
  (setq x (margs (simplify (apply #'map1 (cons f x)))))
  ; ACTUALLY WE REALLY REALLY WANT TO POSTPONE EVALUATING THE PREDICATE HERE
  (setq x (mapcar #'car (mapcar #'(lambda (s) (ignore-errors-mfuncall '$maybe s)) x)))
  ; IF MOR RETURNS AN UNEVALUATED EXPRESSION HERE, RETURN AN UNEVALUATED EXPR WITH OP = $SOME
  (let ((a (simplifya `((mor) ,@x) t)))
    ; FOR NOW ASSUME NIL IF ANYTHING BUT T OR $UNKNOWN (DON'T CHANGE $SOME NOW)
    (if (or (eq a t) (eq a '$unknown)) a nil)))

 ((every '$matrixp x)
  (let ((fmaplvl 2))
    (setq x (margs (simplify (apply #'fmapl1 (cons f x)))))
    (setq x (mapcar #'(lambda (s) ($some '$identity s)) x))
    ; IF MOR RETURNS AN UNEVALUATED EXPRESSION HERE, RETURN AN UNEVALUATED EXPR WITH OP = $SOME
    (let ((a (simplifya `((mor) ,@x) t)))
      ; FOR NOW ASSUME NIL IF ANYTHING BUT T OR $UNKNOWN (DON'T CHANGE $SOME NOW)
      (if (or (eq a t) (eq a '$unknown)) a nil))))


 (t (merror "Improper arguments to function 'some'"))))
|#

; REDEFINE PARSE-CONDITION (IN SRC/NPARSE.LISP) TO APPEND NIL INSTEAD OF $FALSE
; WHEN INPUT IS LIKE "IF FOO THEN BAR" (WITHOUT ELSE)

(defun parse-condition (op)
  (list* (parse (rpos op) (rbp op))
	 (if (eq (first-c) '$then)
	     (parse '$any (rbp (pop-c)))
	     (mread-synerr "Missing `then'"))
	 (case (first-c)
	   (($else)   (list t (parse '$any (rbp (pop-c)))))
	   (($elseif) (parse-condition (pop-c)))
	   (t
	    (list t nil)))))
