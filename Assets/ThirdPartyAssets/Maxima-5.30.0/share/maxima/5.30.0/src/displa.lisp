;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module displa)

;; N.B. You must read the macro file before reading this file.

(load-macsyma-macros displm)

(declare-top (special $linel))

(defmvar $stardisp nil
  "Causes factors of products to be separated by * when displayed.")

(defmvar $leftjust nil
  "Causes equations to be drawn left justified rather than centered.
	 For slow consoles.")

(defmvar $display2d t
  "Causes equations to be drawn in two dimensions.  Otherwise, drawn
	 linearly.")

(defmvar $lispdisp nil
  "Causes symbols not having $ as the first character in their pnames
	 to be preceded with a ? when displayed.")

(defmvar $derivabbrev nil)

(defmvar $noundisp nil)

(defmvar $stringdisp nil
  "Causes strings to be bracketed in double quotes when displayed.
	 Normally this is off, but is turned on when a procedure definition is
	 being displayed.")

(defmvar displayp nil "Is T when inside of `displa'")

;; Parameters which control how boxes, absolute value signs,
;; evaluation-at-a-point bars, and matrices are drawn.

(defmvar $boxchar "\""  "Character used for drawing boxes.")
(defmvar $absboxchar "!" "Character used for drawing absolute value signs and 'evaluation at' signs.")
(defmvar $lmxchar "["  "Character used for drawing the left edge of a matrix.")
(defmvar $rmxchar "]"  "Character used for drawing the right edge of a matrix.")

;; These variables are bound within Macsyma Listeners since they are different
;; for each window.  Set them here, anyway, so that RETRIEVE can be called from
;; top level.  The size of TOP-WINDOW is wired in here.

(setq linel 79. $linel 79. ttyheight 24.)

(defvar linearray (make-array 80. :initial-element nil))

(defmfun maxima-display (form &key (stream *standard-output*) )
  (let ((*standard-output* stream))
    (displa form)))

(defvar *alt-display2d* nil)
(defvar *alt-display1d* nil)

(defmfun displa (form &aux #+kcl(form form))
  (if (not #.ttyoff)
      (cond ($display2d
	     (cond (*alt-display2d* (apply *alt-display2d* form ()))
		   (t
		    (let ((displayp t)
			  (linearray (if displayp (make-array 80.) linearray))
			  (mratp (checkrat form))
			  (maxht     1) (maxdp   0) (width   0)
			  (height    0) (depth   0) (level   0) (size   2)
			  (break     0) (right   0) (lines   1) bkpt
			  (bkptwd    0) (bkptht  1) (bkptdp  0) (bkptout 0)
			  (bkptlevel 0) in-p)
		      (unwind-protect
			   (progn
			     (setq form (dimension form nil 'mparen 'mparen 0 0))
			     (checkbreak form width)
			     (output form (if (and (not $leftjust) (= 2 lines))
					      (- linel (- width bkptout))
					      0)))
			;; make sure the linearray gets cleared out.
			(fill linearray nil))))))
	    (t
	     (if *alt-display1d*
		 (apply *alt-display1d* form ())
		 (linear-displa form))))))

;;; (LINEAR-DISPLA <thing-to-display>)
;;;
;;; Display text linearly. This function should be usable in any case
;;;  DISPLA is usable and will attempt to do something reasonable with
;;;  its input.

(defun linear-displa (form)
  (fresh-line *standard-output*)
  (mgrind form *standard-output*)
  (terpri))

(defmvar $display_format_internal nil
  "Setting this TRUE can help give the user a greater understanding
	 of the behavior of maxima on certain of his problems,
	 especially those involving roots and quotients")

(defun nformat-check (form)
  (if (and $display_format_internal
	   (not (or (atom form) (atom (car form)) (specrepp form))))
      form
      (nformat form)))

(defun dimension (form result lop rop w right)
  (let ((level (1+ level))
	(break (if (and w break) (+ w break))))
    (setq form (nformat-check form))
    (cond ((atom form)
	   (dimension-atom form result))
	  ((and (atom (car form)) (setq form (cons '(mprogn) form)) nil))
	  ((or (<= (lbp (caar form)) (rbp lop)) (> (lbp rop) (rbp (caar form))))
	   (dimension-paren form result))
	  ((member 'array (car form) :test #'eq) (dimension-array form result))
	  ((safe-get (caar form) 'dimension)
	   (funcall (get (caar form) 'dimension) form result))
	  (t (dimension-function form result)))))

(defvar atom-context 'dimension-list)
;; bound by DIMENSION-ARRAY and DIMENSION-FUNCTION.
;; This ATOM-CONTEXT put in by GJC so that MCW could have a clean
;; hook by which to write his extensions for vector-underbars.

;; Referenced externally by RAT;FLOAT.

(defmfun dimension-atom (form result)
  (cond ((and (symbolp form) (get form atom-context))
	 (funcall (get form atom-context) form result))
	((stringp form) (dimension-string (makestring form) result))
	((member (marray-type form) '(array hash-table $functional))
	 (dimension-array-object form result))
	(t (dimension-string (makestring form) result))))

;; Referenced externally by anyone who wants to display something as
;; a funny looking atom, e.g. Trace, Mformat.

(defmfun dimension-string (dummy result &aux crp)
  (setq width 0 height 1 depth 0)
  (do ((l dummy (cdr l)))
      ((null l))
    (incf width)
    (if (char= (car l) #\newline) (setq crp t)))
  (cond ((or (and (checkfit width) (not crp)) (not break))
	 (nreconc dummy result))
	(t (setq width 0)
	   (do ((l dummy) (w (- linel (- break bkptout))))
	       ((null l) (checkbreak result width) result)
	     (setq dummy l l (cdr l))
	     (cond ((char= (car dummy) #\newline)
		    (forcebreak result width)
		    (setq result nil w (+ linel width)))
		   (t (incf width)
		      (when (and (= w width) l)
			(forcebreak (cons #\\ result) width)
			(setq result nil w (+ linel width))
			(incf width))
		      (setq result (rplacd dummy result))))))))

(defmfun makestring (atom)
  (let (dummy)
    (cond ((numberp atom) (exploden atom))
      ((stringp atom)
       (setq dummy (coerce atom 'list))
       (if $stringdisp
         (cons #\" (nconc dummy (list #\")))
         dummy))
	  ((not (symbolp atom)) (exploden atom))
	  ((and (setq dummy (get atom 'reversealias))
		(not (and (member atom $aliases :test #'eq) (get atom 'noun))))
	   (exploden (stripdollar dummy)))
	  ((not (eq (getop atom) atom))
       (makestring (getop atom)))
	  (t (setq dummy (exploden atom))
	     (cond
           ((null dummy) nil)
           ((char= #\$ (car dummy)) (cdr dummy))
		   ((char= #\% (car dummy)) (cdr dummy))
		   ($lispdisp (cons #\? dummy))
		   (t dummy))))))

(defun dimension-paren (form result)
  (setq result (cons #\) (dimension form (cons #\( result) 'mparen 'mparen 1 (1+ right))))
  (incf width 2)
  result)

(defun dimension-array (x result)
  (prog (dummy bas (w 0) (h 0) (d 0) sub)
     (if (eq (caar x) 'mqapply)
	 (setq dummy (cadr x) x (cdr x))
	 (setq dummy (caar x)))
     (cond ((or (not $noundisp) (not (symbolp (caar x)))))
	   ((and (get (caar x) 'verb) (get (caar x) 'alias))
	    (push-string "''" result) (setq w 2))
	   ((and (get (caar x) 'noun) (not (member (caar x) (cdr $aliases) :test #'eq))
		 (not (get (caar x) 'reversealias)))
	    (setq result (cons #\' result) w 1)))
     (setq sub (let ((lop 'mparen)
		     (rop 'mparen)
		     (break nil) (size 1))
		 (dimension-list x nil))
	   w (+ w width)
	   h height
	   d depth)
     (setq bas (if (and (not (atom dummy)) (member 'array (car dummy) :test #'eq))
		   (let ((break nil) (right 0)) (dimension-paren dummy result))
		   (let ((atom-context 'dimension-array))
		     (dimension dummy result lop 'mfunction nil 0))))
     (cond ((not (checkfit (setq width (+ w width))))
	    (return (dimension-function (cons '(subscript) (cons dummy (cdr x))) result)))
	   ((and (atom (car bas)) (char= #\ (car bas))) ; Adding the test (atom (car bas))
	    (setq result (cons (cons 0 (cons (- h) sub)) bas) depth (max (+ h d) depth)))
	   (t (setq result (cons (cons 0 (cons (- (+ depth h)) sub)) bas)
		    depth (+ h d depth))))
     (update-heights height depth)
     (return result)))

(defun dimension-function (x result)
  (prog (fun (w 0) (h 0) (d 0))
     (cond ((or (not $noundisp) (not (symbolp (caar x)))))
	   ((and (get (caar x) 'verb) (get (caar x) 'alias))
	    (push-string "''" result) (setq w 2))
	   ((and (get (caar x) 'noun) (not (member (caar x) (cdr $aliases) :test #'eq))
		 (not (get (caar x) 'reversealias)))
	    (setq result (cons #\' result) w 1)))
     (if (eq (caar x) 'mqapply) (setq fun (cadr x) x (cdr x)) (setq fun (caar x)))
     (setq result (let ((atom-context 'dimension-function))
		    (dimension fun result lop 'mparen 0 1))
	   w (+ w width) h height d depth)
     (cond ((null (cdr x))
	    (setq result (list* #\) #\( result)
		  width (+ 2 w)))
	   (t (setq result (let ((lop 'mparen)
				 (rop 'mparen)
				 (break (if break (+ 1 w break))))
			     (cons #\) (dimension-list x (cons #\( result))))
		    width (+ 2 w width) height (max h height) depth (max d depth))))
     (return result)))

(defmfun dimension-prefix (form result)
  (prog (dissym (symlength 0))
     (setq dissym (safe-get (caar form) 'dissym)
	   symlength (length dissym))
     (setq result (dimension (cadr form) (revappend dissym result) (caar form) rop symlength right)
	   width (+ symlength width))
     (return result)))

(defun dimension-list (form result)
  (prog ((w 0) (h 0) (d 0))
     (setq result (dimension (cadr form) result lop 'mcomma 0 right)
	   w width h height d depth)
     (do ((l (cddr form) (cdr l)))
	 ((null l))
       (push-string ", " result)
       (incf w 2)
       (checkbreak result w)
       (setq result (dimension (car l) result 'mcomma 'mcomma w right)
	     w (+ w width) h (max h height) d (max d depth)))
     (setq width w height h depth d)
     (return result)))

(defmfun dimension-infix (form result)
  (unless (= (length (cdr form)) 2)
    (return-from dimension-infix (dimension-function form result)))
  (prog (dissym (symlength 0) (w 0) (h 0) (d 0))
     (setq dissym (safe-get (caar form) 'dissym)
	   symlength (length dissym)
	   result (dimension (cadr form) result lop (caar form) 0 symlength)
	   w width
	   h height
	   d depth)
     (setq result (revappend dissym result))
     (checkbreak result (+ symlength w))
     (setq result (dimension (caddr form) result (caar form) rop (+ symlength w) right)
	   width (+ w symlength width)
	   height (max h height)
	   depth (max d depth))
     (return result)))

(defmfun dimension-nary (form result)
  ;; If only 0 or 1 arguments, then print "*"() or "*"(A)
  (cond ((null (cddr form))
	 (dimension-function form result))
	(t
	 (prog (dissym (symlength 0) (w 0) (h 0) (d 0) helper)
	    (setq dissym (safe-get (caar form) 'dissym)
		  symlength (length dissym)

          ;; Look for a helper function. Fall back on default if none found.
          helper (or (safe-get (caar form) 'dimension-nary-helper) 'dimnary)

		  result (funcall helper (cadr form) result lop (caar form) (caar form) 0)
		  w width
		  h height
		  d depth
          )
	    (do ((l (cddr form) (cdr l)))
		(nil)
	      (checkbreak result w)
	      (setq result (revappend dissym result) w (+ symlength w))
	      (cond ((null (cdr l))
		     (setq result (funcall helper (car l) result (caar form) (caar form) rop w)
			   width (+ w width)
			   height (max h height)
			   depth (max d depth))
		     (return t))
		    (t
		     (setq result (funcall helper (car l) result (caar form) (caar form) (caar form) w)
			   w (+ w width)
			   h (max h height)
			   d (max d depth)))))
	    (return result)))))

;; Output for MTEXT, generated for example by print(...).
;; Suppress parentheses which would be generated by DIMNARY.

(defun dimnary-mtext (form result lop op rop w)
  (declare (ignore op))
  (dimension form result lop rop w right))

;; Output for general n-ary operators.
;; Heuristic: if some argument is displayed as an infix or n-ary operator,
;; then put parentheses around that argument (even if parentheses are not
;; necessary, according to operator precedence, to disambiguate the output).
;;
;; Examples:
;;
;; a * b * c;                   --> a b c
;; a . b . c;                   --> a . b . c
;; a * b . c;                   --> a (b . c)
;; (a * b) . c;                 --> (a b) . c
;; a . b * c;                   --> (a . b) c
;; a . (b * c);                 --> a . (b c)
;; a . b * c . d;               --> (a . b) (c . d)
;; a * b . c * d;               --> a (b . c) d
;; (a * b) . (c * d);           --> (a b) . (c d)
;; infix ("@@");
;; a @@ b * c @@ d;             --> (a @@ b) (c @@ d)
;; a @@ b . c @@ d;             --> (a @@ b) . (c @@ d)
;; a * b @@ c . d;              --> a ((b @@ c) . d)
;; (a * b) @@ (c . d);          --> (a b) @@ (c . d)

(defun dimnary (form result lop op rop w)
  (declare (ignore op))
  (if (and (consp form)
           (member (safe-get (caar form) 'dimension)
                   '(dimension-infix dimension-nary)))
      (progn
        (setq result
              (cons #\)
                    (dimension form
                               (cons #\( result)
                               'mparen 'mparen (if w (1+ w)) (1+ right))))
        (incf width 2)
        result)
      (dimension form result lop rop w right)))

;; Output for Boolean n-ary operators.
;; Heuristic: if some argument is displayed as an infix or n-ary operator,
;; or the operator of the argument is MNOT, then put parentheses around that argument
;; (even if parentheses are not necessary, according to operator precedence,
;; to disambiguate the output).
;;
;; Examples:
;;
;; a and b and c;               --> a and b and c
;; a or b or c;                 --> a or b or c
;; a or b and c;                --> a or (b and c)
;; (a or b) and c;              --> (a or b) and c
;; a and b or c;                --> (a and b) or c
;; a and (b or c);              --> a and (b or c)
;; a and b or c and d;          --> (a and b) or (c and d)
;; a or b and c or d;           --> a or (b and c) or d
;; (a or b) and (c or d);       --> (a or b) and (c or d)
;; a < 0 and b < 0 or c < 0;    --> ((a < 0) and (b < 0)) or (c < 0)
;; a < 0 and not foo;           --> (a < 0) and (not foo)
;; a < 0 or not foo;            --> (a < 0) or (not foo)
;; infix ("=>");
;; a => b and a => c;           --> (a => b) and (a => c)
;; a => b or not b => c;        --> (a => b) or (not b => c)
;; a and b => c and d;          --> a and (b => c) and d
;; a or b => c and d;           --> a or ((b => c) and d)
;; (a and b) => (c and d);      --> (a and b) => (c and d)
;; if a and b then 1 else 0;    --> if a and b then 1 else 0

(defun dimnary-boolean (form result lop op rop w)
  (declare (ignore op))
  (if (and (consp form)
	   (or (member (safe-get (caar form) 'dimension) '(dimension-infix dimension-nary))
	       (eq (caar form) 'mnot)))
      (dimension-paren form result)
      (dimension form result lop rop w right)))

(defmfun dimension-postfix (form result)
  (prog (dissym (symlength 0))
     (setq dissym (safe-get (caar form) 'dissym)
	   symlength (length dissym))
     (setq result (dimension (cadr form) result lop (caar form) 0 (+ symlength right))
	   width (+ symlength width))
     (return (revappend dissym result))))

(defmfun dimension-nofix (form result)
  (setq form (safe-get (caar form) 'dissym)
	width (length form))
  (revappend form result))

(defun dimension-match (form result)
  (prog (dissym (symlength 0))
     (setq dissym (safe-get (caar form) 'dissym)
	   symlength (length (car dissym)))
     (cond ((null (cdr form))
	    (setq width (+ symlength (length (cdr dissym)))
		  height 1
		  depth 0)
	    (return (revappend (cdr dissym) (revappend (car dissym) result))))
	   (t (setq result (let ((lop 'mparen)
				 (rop 'mparen)
				 (break (if break (+ symlength break)))
				 (right (+ symlength right)))
			     (dimension-list form (revappend (car dissym) result))))
	      (setq width (+ (length (cdr dissym)) symlength width))
	      (return (revappend (cdr dissym) result))))))

(defmfun dimension-superscript (form result)
  (prog (exp (w 0) (h 0) (d 0) bas)
     (setq exp (let ((size 1))
		 (dimension (caddr form) nil 'mparen 'mparen nil 0))
	   w width
	   h height
	   d depth)
     (cond ((and (not (atom (cadr form))) (member 'array (cdaadr form) :test #'eq))
	    (prog (sub (w2 0) (h2 0) (d2 0))
	       (setq sub (if (eq 'mqapply (caaadr form))
			     (cdadr form) (cadr form)))
	       (setq sub (let ((lop 'mparen) (break nil) (size 1))
			   (dimension-list sub nil))
		     w2 width h2 height d2 depth)
	       (setq bas (dimension (mop (cadr form)) result lop 'mexpt nil 0))
	       (when (not (checkfit (+ width (max w w2))))
		 (setq result (dimension-function (cons '($expt) (cdr form)) result))
		 (return result))
	       (setq result (cons (cons 0 (cons (+ height d) exp)) bas))
	       (setq result (cons (cons (- w) (cons (- (+ depth h2)) sub)) result))
	       (setq result (cons (list (- (max w w2) w2) 0) result)
		     width (+ width (max w w2)) height (+ h d height) depth (+ d2 h2 depth)))
	    (update-heights height depth)
	    (return result))
	   ((and (atom (caddr form))
		 (not (atom (cadr form)))
		 (not (safe-get (caaadr form) 'dimension))
		 (prog2 (setq bas (nformat-check (cadr form)))
		     (not (safe-get (caar bas) 'dimension))))
	    (return (dimension-function
		     (list* '(mqapply) (list '(mexpt) (mop bas) (caddr form)) (margs bas))
		     result)))
	   (t (setq bas (dimension (cadr form) result lop 'mexpt nil 0) width (+ w width))
	      (if (not (checkfit width))
		  (return (dimension-function (cons '($expt) (cdr form)) result)))
	      (if (eql #\) (car bas))
		  (setq result (cons (list* 0 (1+ d) exp) bas) height (max (+ 1 h d) height))
		  (setq result (cons (list* 0 (+ height d) exp) bas) height (+ h d height)))
	      (update-heights height depth)
	      (return result)))))

(defun dsumprod (form result d-form sw sh sd)
  (prog (dummy (w 0) (h 0) (d 0) dummy2 (lsum (eq (caar form) '%lsum)))
     (setq dummy2 (dimension (caddr form) nil 'mparen 'mequal nil 0)
	   w width
	   h height
	   d depth)
     (if lsum
	 (push-string " in "  dummy2)
	 (push-string " = " dummy2))
     (setq dummy2 (dimension (cadddr form) dummy2 'mequal 'mparen nil 0)
	   w (+ 3 w width)
	   h (max h height)
	   d (max d depth))
     (or lsum
	 (setq dummy (dimension (cadr (cdddr form)) nil 'mparen 'mparen nil 0)))
     (unless (checkfit (max w width))
       (return (dimension-function form result)))
     (setq dummy2 (cons (cons (- sw) (cons (- (+ sd h)) dummy2)) (cons d-form result)))
     (cond ((> width sw)
	    (setq sw 0))
	   (t
	    (setq sw (truncate (- sw width) 2)
		  width (+ sw width))))
     (setq dummy (cons (cons (- sw w) (cons (+ sh depth) dummy)) dummy2)
	   w (max w width)
	   d (+ sd h d)
	   h (+ sh height depth))
     (update-heights h d)
     (setq dummy (dimension (cadr form) (cons (list (1+ (- w width)) 0) dummy)
			    (caar form) rop w right)
	   width (+ 1 w width)
	   height (max h height)
	   depth (max d depth))
     (return dummy)))

(displa-def bigfloat  dim-bigfloat)
(displa-def mquote    dimension-prefix "'")
(displa-def msetq     dimension-infix  " : ")
(displa-def mset      dimension-infix  " :: ")
(displa-def mdefine   dim-mdefine      " := ")
(displa-def mdefmacro dim-mdefine      " ::= ")

(defun dim-mdefine (form result)
  (let (($noundisp t)
	($stringdisp t))
    (dimension-infix (if (cdddr form)
			 (list (car form) (cadr form) (cons '(mprogn) (cddr form)))
			 form)
		     result)))

(displa-def mfactorial dimension-postfix "!")
(displa-def mexpt      dimension-superscript)
(displa-def mncexpt    dim-mncexpt "^^")

(defun dim-mncexpt (form result)
  (dimension-superscript (list '(mncexpt) (cadr form) (cons '(mangle) (cddr form)))
			 result))

(displa-def mnctimes dimension-nary " . ")

(displa-def %product dim-%product 115.)

(defun dim-%product (form result)
  (dsumprod form result '(d-prodsign) 5 3 1))

(displa-def rat dim-rat "/")

(defun dim-rat (form result)
  (if $pfeformat
      (dimension-nary form result)
      (dim-mquotient form result)))

(displa-def mquotient dim-mquotient "/")

(defun dim-mquotient (form result)
  (unless (= (length (cdr form)) 2)
    (return-from dim-mquotient (dimension-function form result)))
  (prog (num (w 0) (h 0) (d 0) den)
     (when (and (= 1 size) (atom (cadr form)) (atom (caddr form)))
       (return (dimension-nary form result)))
     (setq num (dimension (cadr form) nil 'mparen 'mparen nil right)
	   w width
	   h height
	   d depth)
     (unless (checkfit w)
       (return (dimension-nary form result)))
     (setq den (dimension (caddr form) nil 'mparen 'mparen nil right))
     (unless (checkfit width)
       (return (dimension-nary form result)))
     (return (dratio result num w h d den width height depth))))

;;            <--     W1     -->
;;            ------------------
;;            | ^              |
;;   <- X1 -> | | H1           |
;;            | | D1           |
;;            | v              |
;;            ------------------
;;    ----------------------------------
;; (Likewise for X2, H2, D2, W2 in the denominator)

(defun dratio (result num w1 h1 d1 den w2 h2 d2)
  (setq width (max w1 w2)
	height (+ 1 h1 d1)
	depth (+ h2 d2))
  (setq h1 (truncate (- width w1) 2)
	d2 (truncate (- width w2) 2))
  (update-heights height depth)
  (push `(,h1 ,(1+ d1) . ,num) result)
  (push `(,(- d2 (+ h1 w1)) ,(- h2) . ,den) result)
  (push `(,(- 0 d2 w2) 0) result)
  (push `(d-hbar ,width) result)
  result)

(displa-def mtimes dimension-nary " ")

;; This code gets run when STARDISP is assigned a value.

(defprop $stardisp stardisp assign)

(defun stardisp (symbol val)
  (declare (ignore symbol))
  (putprop 'mtimes (if val '(#\*) '(#\space)) 'dissym))

(displa-def %integrate dim-%integrate 115.)

(defun dim-%integrate (form result)
  (prog (dummy (w 0)(h 0)(d 0) dummy2)
     (cond ((not (or (= (length (cdr form)) 2) (= (length (cdr form)) 4)))
	    (return-from dim-%integrate (dimension-function form result)))
	   ((null (cdddr form))
	    (setq dummy `(#\space (d-integralsign) . ,result) w 2 h 3 d 2))
	   (t (setq dummy (dimension (cadr (cdddr form)) nil 'mparen 'mparen nil 0)
		    w width h height d depth)
	      (setq dummy2 (dimension (cadddr form) nil 'mparen 'mparen nil 0))
	      (if (not (checkfit (+ 2 (max w width))))
		  (return (dimension-function form result)))
	      (setq dummy `((0 ,(+ 3 d) . ,dummy) (d-integralsign) . ,result))
	      (setq dummy (cons (cons (- w) (cons (- (+ 2 height)) dummy2)) dummy)
		    w (+ 2 (max w width)) h (+ 3 h d) d (+ 2 height depth)
		    dummy (cons (list (- w 1 width) 0) dummy))))
     (update-heights h d)
     (setq dummy (dimension (cadr form) dummy '%integrate 'mparen w 2)
	   w (+ w width) h (max h height) d (max d depth))
     (push-string " d" dummy)
     (setq dummy (dimension (caddr form) dummy 'mparen rop (+ 2 w) right)
	   width (+ 2 w width) height (max h height) depth (max d depth))
     (return dummy)))

(displa-def %derivative dim-%derivative 125.)

(defun dim-%derivative (form result)
  (prog ()
     (cond ((null (cddr form))
	    (return (dimension-function (cons '(%diff) (cdr form)) result))))
     (cond ((null (cdddr form)) (setq form (append form '(1)))))
     (cond ((and $derivabbrev
		 (do ((l (cddr form) (cddr l))) ((null l) t)
		   (cond ((and (atom (car l)) (integerp (cadr l)) (> (cadr l) 0)))
			 (t (return nil)))))
	    (return (dmderivabbrev form result)))
	   ((or (> (rbp lop) 130.) (> (lbp rop) 130.)
		(and (not (atom (cadr form))) (or (> (rbp lop) 110.) (> (lbp rop) 110.))))
	    (return (dimension-paren form result)))
	   (t (return (dmderivlong form result))))))

(defun dmderivabbrev (form result)
  (prog (dummy (w 0))
     (do ((l (cddr form) (cddr l)) (var))
	 ((null l) (setq dummy (cdr dummy) w (1- w)))
       (setq var (dimension (car l) nil 'mparen 'mparen nil 0))
       (do ((i (cadr l) (1- i)))
	   ((= 1 i))
	 (setq dummy (cons #\space (append var dummy))))
       (setq dummy (cons #\space (nconc var dummy)) w (+ w (cadr l) (* (cadr l) width))))
     (setq result (dimension (cadr form) result lop '%deriv 0 right))
     (setq result (cons (cons 0 (cons (- 0 depth 1) dummy)) result)
	   width (+ w width) depth (max 1 (1+ depth)))
     (update-heights height depth)
     (return result)))

(defun dmderivlong (form result)
  (prog (num (w1 0) (h1 0) (d1 0) den (w2 0)( h2 0)  (d2 0))
     (setq num (list (cadddr form))
	   den (cond ((equal 1 (cadddr form))
		      (dimension (caddr form)
				 (list #\d) 'mparen 'mparen nil 0))
		     (t (dimension-superscript
			 (cons '(diff)(cddr form)) (list #\d))))
	   w2 (1+ width) h2 height d2 depth)
     (do ((l (cddddr form) (cddr l))) ((null l))
       (setq num (cons (cadr l) num)
	     den (cond ((equal 1 (cadr l))
			(dimension (car l) (cons #\d (cons #\space den))
				   'mparen 'mparen nil 0))
		       (t (dimension-superscript
			   (cons '(diff) l) (cons #\d (cons #\space den)))))
	     w2 (+ 2 w2 width) h2 (max h2 height) d2 (+ d2 depth)))
     (setq num (nformat-check (addn num t)))
     (cond ((equal 1 num) (setq num (list #\d) w1 1 h1 1 d1 0))
	   (t (setq num (dimension-superscript (list '(diff) #\d num) nil)
		    w1 width h1 height d1 depth)))
     (cond ((atom (setq form (nformat-check (cadr form))))
	    (setq num (dimension form num '%deriv 'mparen nil 0) w1 (+ w1 width))
	    (return (dratio result num w1 h1 d1 den w2 h2 d2)))
	   (t (setq result (dratio result num w1 h1 d1 den w2 h2 d2) w1 width h1 height d1 depth)
	      (setq result (dimension form (cons #\space result) '%deriv rop w1 right)
		    width (+ 1 w1 width) height (max h1 height) depth (max d1 depth))
	      (update-heights height depth)
	      (return result)))))

(displa-def %at dim-%at 105. 105.)

(defun dim-%at (form result)
  (prog (exp  eqs (w 0) (h 0) (d 0))
     (unless (= (length (cdr form)) 2)
       (return-from dim-%at (dimension-function form result)))
     (setq exp (dimension (cadr form) result lop '%at nil 0)
	   w width
	   h height
	   d depth)
     (setq eqs (dimension (cond ((not (eq 'mlist (caar (caddr form)))) (caddr form))
				((null (cddr (caddr form))) (cadr (caddr form)))
				(t (cons '(mcomma) (cdaddr form))))
			  nil 'mparen 'mparen nil 0))
     (unless (checkfit (+ 1 w width))
       (return (dimension-function form result)))
     (setq result (cons (cons 0 (cons (- 0 1 d) eqs))
			(cons `(d-vbar ,(1+ h) ,(1+ d) ,(car (coerce $absboxchar 'list))) exp))
	   width (+ 1 w width)
	   height (1+ h)
	   depth (+ 1 d depth))
     (update-heights height depth)
     (return result)))

(displa-def mplus  dim-mplus)
(defprop munaryplus (#\+ #\space) dissym)

(defun dim-mplus (form result)
  ;; If only 0 or 1 arguments, then print "+"() or +A
  (cond ((and (null (cddr form))
	      (not (member (cadar form) '(trunc exact) :test #'eq)))
	 (if (null (cdr form))
	     (dimension-function form result)
	     (dimension-prefix (cons '(munaryplus) (cdr form)) result)))
	(t (setq result (dimension (cadr form) result lop 'mplus 0 0))
	   (checkbreak result width)
	   (do ((l (cddr form) (cdr l))
		(w width) (h height) (d depth)
		(trunc (member 'trunc (cdar form) :test #'eq)) (dissym))
	       ((null l) (cond (trunc
				(setq width (+ 8 w) height h depth d)
				(push-string " + . . ." result)))
		result)
	     (if (mmminusp (car l))
		 (setq dissym '(#\space #\- #\space) form (cadar l))
		 (setq dissym '(#\space #\+ #\space) form (car l)))
	     (cond ((and (not trunc) (null (cdr l)))
		    (setq result (dimension form (append dissym result)
					    'mplus rop (+ 3 w) right)
			  width (+ 3 w width)
			  height (max h height)
			  depth (max d depth))
		    (return result))
		   (t (setq result
			    (dimension form (append dissym result)
				       'mplus 'mplus (+ 3 w) 0)
			    w (+ 3 w width)
			    h (max h height)
			    d (max d depth))
		      (checkbreak result w)))))))

(displa-def mminus dim-mminus)
(defprop munaryminus (#\- #\space) dissym)
(def-rbp munaryminus 134)
(def-rbp munaryminus 100)

(defun dim-mminus (form result)
  (cond ((and (null (cddr form))
              (not (member (cadar form) '(trunc exact) :test #'eq)))
         (if (null (cdr form))
             (dimension-function form result)
             (dimension-prefix (cons '(munaryminus) (cdr form)) result)))
        (t
         (setq result (dimension (cadr form) result lop 'mminus 0 0))
         (checkbreak result width)
         (do ((l (cddr form) (cdr l))
              (w width)
              (h height)
              (d depth)
              (trunc (member 'trunc (cdar form) :test #'eq))
              (dissym))
             ((null l)
              (cond (trunc
                     (setq width (+ 8 w)
                           height h
                           depth d)
                     (push-string " + . . ." result)))
              result)
           (if (mmminusp (car l))
               (setq dissym '(#\space #\+ #\space) form (cadar l))
               (setq dissym '(#\space #\- #\space) form (car l)))
           (cond ((and (not trunc) (null (cdr l)))
                  (setq result (dimension form (append dissym result)
                                          'mminus rop (+ 3 w) right)
                        width (+ 3 w width)
                        height (max h height)
                        depth (max d depth))
                  (return result))
                 (t
                  (setq result
                        (dimension form (append dissym result)
                                   'mminus 'mminus (+ 3 w) 0)
                        w (+ 3 w width)
                        h (max h height)
                        d (max d depth))
                  (checkbreak result w)))))))

(displa-def %sum   dim-%sum 110.)
(displa-def %limit dim-%limit 110. 110.)
(displa-def %lsum   dim-%lsum 110.)

(defun dim-%lsum (form result)
  (dsumprod form result '(d-sumsign) 4 3 2))

(defun dim-%sum (form result)
  (dsumprod form result '(d-sumsign) 4 3 2))

(defun dim-%limit (form result)
  (prog ((w 0) (h 0) (d 0) dummy)
     (unless (or (= (length (cdr form)) 3) (= (length (cdr form)) 4))
       (return-from dim-%limit (dimension-function form result)))
     (setq dummy (dimension (third form) nil 'mparen 'mparen nil 0)
	   w width h height d depth)
     (push-string " -> " dummy)
     (setq dummy (dimension (fourth form) dummy 'mparen 'mparen nil 0)
	   w (+ 4 w width)
	   h (max h height)
	   d (max d depth))
     (cond ((null (cddddr form)))
	   ((eq '$plus (fifth form))
	    (push #\+ dummy)
	    (incf w))
	   (t (push #\- dummy)
	      (incf w)))
     (push-string "limit" result)
     (setq dummy (cons (list* -5 (- h) dummy) result)
	   d (+ h d))
     (update-heights 1 d)
     (setq dummy (dimension (cadr form) (cons '(1 0) dummy) '%limit rop (1+ w) right))
     (setq width (+ 1 w width)
	   depth (max d depth))
     (return dummy)))

;; Some scheme needs to be worked out to allow use of mathematical character
;; sets on consoles which have them.

(displa-def marrow    dimension-infix  " -> " 80. 80.)
(displa-def mgreaterp dimension-infix  " > ")
(displa-def mgeqp     dimension-infix  " >= ")
(displa-def mequal    dimension-infix  " = ")
(displa-def mnotequal dimension-infix  " # ")
(displa-def mleqp     dimension-infix  " <= ")
(displa-def mlessp    dimension-infix  " < ")
(displa-def mnot      dimension-prefix "not ")
(displa-def mand      dimension-nary   " and ")
(defprop mand dimnary-boolean dimension-nary-helper)
(displa-def mor	      dimension-nary   " or ")
(defprop mor dimnary-boolean dimension-nary-helper)
(displa-def mcond     dim-mcond)
(displa-def %mcond    dim-mcond)

;; MCOND or %MCOND always has an even number of arguments.
;; The first two arguments are the foo and bar in 'if foo then bar .
;; Of the remaining pairs of arguments,
;; the first is MAYBE-ELSEIF and the second is ELSE-OR-THEN in the code below.
;; MAYBE-ELSEIF is T if the construct is 'if foo then bar else quux ,
;; otherwise the construct is 'if foo then bar elseif baz then quux
;; where baz is the value of MAYBE-ELSEIF.
;; If ELSE-OR-THEN is NIL, just omit the final "else".

;; The parser appends (T NIL) to any if-then which lacks an else.
;; DIM-MCOND renders both '((%MCOND) $A $B) and '((%MCOND) $A $B T NIL) as "if a then b".

;; Examples. The "<==>" here means that the stuff on the right parses as the stuff on the left,
;; and the stuff on the left displays as the stuff on the right.

;; ((%mcond) $a $b t nil)                    <==> 'if a then b
;; ((%mcond) $a $b t $d)                     <==> 'if a then b else d
;; ((%mcond) $a $b $c nil t nil)             <==> 'if a then b elseif c then false
;; ((%mcond) $a $b $c $d t nil)              <==> 'if a then b elseif c then d
;; ((%mcond) $a $b $c $d t $f)               <==> 'if a then b elseif c then d else f
;; ((%mcond) $a $b $c $d $e nil t nil)       <==> 'if a then b elseif c then d elseif e then false
;; ((%mcond) $a $b $c $d $e $f t nil)        <==> 'if a then b elseif c then d elseif e then f
;; ((%mcond) $a $b $c $d $e $f t $h)         <==> 'if a then b elseif c then d elseif e then f else h
;; ((%mcond) $a $b $c $d $e $f $g nil t nil) <==> 'if a then b elseif c then d elseif e then f elseif g then false
;; ((%mcond) $a $b $c $d $e $f $g $h)        <==> 'if a then b elseif c then d elseif e then f elseif g then h

(defun dim-mcond (form result)
  (prog ((w 0) (h 0) (d 0))
     (push-string "if " result)
     (setq result (dimension (cadr form) result 'mcond 'mparen 3 0)
	   w (+ 3 width)
	   h height
	   d depth)
     (checkbreak result w)
     (push-string " then " result)
     (setq result (dimension (caddr form) result 'mcond 'mparen (+ 6 w) 0)
	   w (+ 6 w width)
	   h (max h height)
	   d (max d depth))
     (let ((args (cdddr form)))
       (loop while (>= (length args) 2) do
	    (let ((maybe-elseif (car args)) (else-or-then (cadr args)))
	      (cond
		((and (eq maybe-elseif t) (= (length args) 2))
		 (unless (or (eq '$false else-or-then) (eq nil else-or-then))
		   (checkbreak result w)
		   (push-string " else " result)
		   (setq result (dimension else-or-then result 'mcond rop (+ 6 w) right)
			 w (+ 6 w width)
			 h (max h height)
			 d (max d depth))))
		(t
		 (checkbreak result w)
		 (push-string " elseif " result)
		 (setq result (dimension maybe-elseif result 'mcond rop (+ 8 w) right)
		       w (+ 8 w width)
		       h (max h height)
		       d (max d depth))
		 (checkbreak result w)
		 (push-string " then " result)
		 (setq result (dimension else-or-then result 'mcond rop (+ 6 w) right)
		       w (+ 6 w width)
		       h (max h height)
		       d (max d depth)))))
	    (setq args (cddr args))))
     (setq width w height h depth d)
     (return result)))


(displa-def mdo dim-mdo)
(displa-def %mdo dim-mdo)

(defun dim-mdo (form result)
  (prog ((w 0) (h 0) (d 0) brkflag)
     (cond ((not (null (cadr form)))
	    (push-string "for " result)
	    (setq result (cons #\space (dimension (cadr form) result 'mdo 'mparen 4 right))
		  w (+ 4 width) h height d depth brkflag t)))
     (cond ((or (null (caddr form)) (equal 1 (caddr form))))
	   (t (push-string "from " result)
	      (setq result (cons #\space (dimension (caddr form) result 'mdo 'mparen (+ 6 w) 0))
		    w (+ 6 w width) h (max h height) d (max d depth))))
     (setq form (cdddr form))
     (cond ((equal 1 (car form)))
	   ((not (null (car form)))
	    (push-string "step " result)
	    (setq result (cons #\space (dimension (car form) result 'mdo 'mparen (+ 6 w) 0))
		  w (+ 6 w width) h (max h height) d (max d depth)))
	   ((not (null (cadr form)))
	    (push-string "next " result)
	    (setq result (cons #\space (dimension (cadr form) result 'mdo 'mparen (+ 6 w) 0))
		  w (+ 6 w width) h (max h height) d (max d depth))))
     (cond ((not (null (caddr form)))
	    (push-string "thru " result)
	    (setq result (cons #\space (dimension (caddr form) result 'mdo 'mparen (+ 6 w) 0))
		  w (+ 6 w width) h (max h height) d (max d depth) brkflag t)))
     (cond ((not (null (cadddr form)))
	    (cond ((and (not (atom (cadddr form))) (eq (caar (cadddr form)) 'mnot))
		   (push-string "while " result)
		   (setq result (cons #\space (dimension (cadr (cadddr form)) result 'mdo 'mparen (+ 7 w) 0))
			 w (+ 7 w width) h (max h height) d (max d depth)))
		  (t (push-string "unless " result)
		     (setq result
			   (cons #\space (dimension (cadddr form) result 'mdo 'mparen (+ 8 w) 0))
			   w (+ 8 w width) h (max h height) d (max d depth))))))
     (if brkflag (checkbreak result w))
     (push-string "do " result)
     (setq result (dimension (car (cddddr form)) result 'mdo rop (+ 4 w) right)
	   width (+ 4 w width) height (max h height) depth (max d depth))
     (return result)))


(displa-def mdoin dim-mdoin)
(displa-def %mdoin dim-mdoin)

(defun dim-mdoin (form result)
  (prog ((w 0) (h 0) ( d 0))
     (push-string "for " result)
     (setq result (dimension (cadr form) result 'mdo 'mparen 4 0)
	   w (+ 4 width) h height d depth)
     (push-string " in " result)
     (setq result (dimension (caddr form) result 'mdo 'mparen (+ 4 w) 0)
	   w (+ 4 w width) h (max h height) d (max d depth))
     (setq form (cdr (cddddr form)))
     (cond ((not (null (car form)))
	    (push-string " thru " result)
	    (setq result (dimension (car form) result 'mdo 'mparen (+ 6 w) 0)
		  w (+ 6 w width) h (max h height) d (max d depth))))
     (cond ((not (null (cadr form)))
	    (push-string " unless " result)
	    (setq result (dimension (cadr form) result 'mdo 'mparen (+ 8 w) 0)
		  w (+ 8 w width) h (max h height) d (max d depth))))
     (push-string " do " result)
     (setq result (dimension (caddr form) result 'mdo rop (+ 4 w) right)
	   width (+ 4 w width) height (max h height) depth (max d depth))
     (return result)))

(displa-def mprogn dimension-match "(" ")")
(displa-def mlist  dimension-match "[" "]")
(displa-def mangle dimension-match "<" ">")
(displa-def mcomma dimension-nary  ", " 10. 10.)
(displa-def mabs   dim-mabs)

(defun dim-mabs (form result &aux arg bar)
  (setq arg (dimension (cadr form) nil 'mparen 'mparen nil 0))
  (cond ((or (> (+ 2 width) linel) (and (= 1 height) (= 0 depth)))
	 (dimension-function form result))
	(t (setq width (+ 2 width))
	   (update-heights height depth)
	   (setq bar `(d-vbar ,height ,depth ,(car (coerce $absboxchar 'list))))
	   (cons bar (nconc arg (cons bar result))))))

(displa-def $matrix dim-$matrix)

(defun dim-$matrix (form result)
  (prog (dmstr rstr cstr consp cols)
     (setq cols (if ($listp (cadr form)) (length (cadr form)) 0))
     (if (or (null (cdr form))
             (memalike '((mlist simp)) (cdr form))
             ;; Check if the matrix has lists as rows with a equal number of
             ;; columns.
             (dolist (row (cdr form))
               (if (or (not ($listp row))
                       (not (eql cols (length row))))
                   (return t))))
         ;; The matrix is not well formed. Display the matrix in linear mode.
         (return (dimension-function form result)))
     (do ((l (cdadr form) (cdr l))) ((null l))
       (setq dmstr (cons nil dmstr) cstr (cons 0 cstr)))
     (do ((r (cdr form) (cdr r)) (h1 0) (d1 0))
	 ((or consp (null r))
	  (setq width 0)
	  (do ((cs cstr (cdr cs))) ((null cs)) (setq width (+ 2 (car cs) width)))
	  (setq h1 (1- (+ h1 d1)) depth (truncate h1 2) height (- h1 depth)))
       (do ((c (cdar r) (cdr c))
	    (nc dmstr (cdr nc))
	    (cs cstr (cdr cs)) (dummy) (h2 0) (d2 0))
	   ((null c) (setq d1 (+ d1 h1 h2) h1 (1+ d2)))
	 (setq dummy (dimension (car c) nil 'mparen 'mparen nil 0)
	       h2 (max h2 height) d2 (max d2 depth))
	 (cond ((not (checkfit (+ 14. width))) (setq consp t) (return nil))
	       (t (rplaca nc (cons (list* width height depth dummy) (car nc)))
		  (rplaca cs (max width (car cs))))))
       (setq rstr (cons d1 rstr)))
     (if (> (+ height depth) (length linearray))
	 (setq consp t))
     (return
       (cond ((and (not consp) (checkfit (+ 2 width)))
	      (matout dmstr cstr rstr result))
	     ((and (not consp) (<= level 2)) (colout dmstr cstr result))
	     (t (dimension-function form result))))))

(defun matout (dmstr cstr rstr result)
  (push `(d-matrix left ,height ,depth) result)
  (push #\space result)
  (do ((d dmstr (cdr d)) (c cstr (cdr c)) (w 0 0))
      ((null d))
    (do ((d (car d) (cdr d)) (r rstr (cdr r))) ((null d))
      (rplaca (cddar d) (- height (car r)))
      (rplaca (cdar d) (- (truncate (- (car c) (caar d)) 2) w))
      (setq w (truncate (+ (car c) (caar d)) 2))
      (rplaca d (cdar d)))
    (setq result (cons (list (+ 2 (- (car c) w)) 0) (nreconc (car d) result))))
  (setq width (+ 2 width))
  (update-heights height depth)
  (rplaca (car result) (1- (caar result)))
  (push `(d-matrix right ,height ,depth) result)
  result)

(defun colout (dmstr cstr result)
  (setq width 0 height 1 depth 0)
  (do ((r dmstr (cdr r)) (c cstr (cdr c)) (col 1 (1+ col)) (w 0 0) (h -1 -1) (d 0))
      ((null r))
    (push-string " Col " result)
    (setq result (nreconc (exploden col) result))
    (push-string " = " result)
    (setq width (+ 8 (flatc col) width))
    (do ((r (car r) (cdr r))) ((null r))
      (setq h (+ 1 h (cadar r) (caddar r)))
      (rplaca (cddar r) (- h (cadar r)))
      (rplaca (cdar r) (- (truncate (- (car c) (caar r)) 2) w))
      (setq w (truncate (+ (car c) (caar r)) 2))
      (rplaca r (cdar r)))
    (setq d (truncate h 2) h (- h d))
    (push `(d-matrix left ,h ,d) result)
    (push #\space result)
    (push `(0 ,(- d) . ,(nreverse (car r))) result)
    (push `(,(1+ (- (car c) w)) 0) result)
    (push `(d-matrix right ,h ,d) result)
    (setq width (+ 4 (car c) width) height (max h height) depth (max d depth))
    (update-heights h d)
    (checkbreak result width))
  result)

(displa-def mbox dim-mbox)

(defun dim-mbox (form result &aux dummy)
  (setq dummy (dimension (cadr form) nil 'mparen 'mparen nil 0))
  (cond ((not (checkfit (+ 2 width)))
	 (dimension-function (cons '($box) (cdr form)) result))
	(t (push `(d-box ,height ,depth ,width ,(nreverse dummy)) result)
	   (setq width (+ 2 width) height (1+ height) depth (1+ depth))
	   (update-heights height depth)
	   result)))

(displa-def mlabox dim-mlabox)

(defun dim-mlabox (form result)
  (prog (dummy ch)
     (setq dummy (dimension (cadr form) nil 'mparen 'mparen nil 0))
     (cond ((not (checkfit (+ 2 width)))
	    (return (dimension-function (cons '($box) (cdr form)) result))))
     (setq width (+ 2 width) height (1+ height) depth (1+ depth))
     (setq ch (car (coerce $boxchar 'list)))
     (setq result
	   (cons (do ((l (mapcar #'(lambda (l) (char (symbol-name l) 0))
				 (makstring (caddr form))) (cdr l))
		      (w 0) (nl))
		     ((or (null l) (= width w))
		      (cons 0 (cons (1- height)
				    (cond ((< w width)
					   (cons `(d-hbar ,(- width w) ,ch) nl))
					  (t nl)))))
		   (setq nl (cons (car l) nl) w (1+ w)))
		 result))
     (setq result (nconc dummy (list* `(d-vbar ,(1- height) ,(1- depth) ,ch)
				      (list (- width) 0) result)))
     (setq result (cons (list (- 1 width) (- depth) `(d-hbar ,width ,ch)) result))
     (setq result (list* `(d-vbar ,(1- height) ,(1- depth) ,ch) '(-1 0) result))
     (update-heights height depth)
     (return result)))

(displa-def mtext dim-mtext 1 1)
(defprop mtext dimnary-mtext dimension-nary-helper)

(defun dim-mtext (form result)
  (if (null (cddr form)) (dimension (cadr form) result lop rop 0 0)
      (dimension-nary form result)))

(displa-def mlabel dim-mlabel 0 0)
(setf (get 'mlabel 'wxxml) 'wxxml-mlable) ;; backwards-compatibility for wxMaxima

(defvar *display-labels-p* t)

(defun dim-mlabel (form result)
  (prog (dummy (w 0) (h 0) (d 0))
     (cond ((eq nil (cadr form)) (setq w 0 h 0 d 0))
	   (mratp (setq result (append mratp (if *display-labels-p*
						 (dimension-paren (cadr form) result)))
			w (+ 4 width) h height d depth))
	   (t (setq result (cons #\space (if *display-labels-p*
					     (dimension-paren (cadr form) result)))
		    w (1+ width) h height d depth)))
     (let ((level linel)) (checkbreak result w))
     (setq dummy (list 0 0))
     (setq result (dimension (caddr form) (cons dummy result) 'mlabel rop w right))
     (cond ((and (not $leftjust) (= 0 bkptout))
	    (rplaca dummy (max 0 (- (truncate (- linel width) 2) w)))
	    (setq width (+ (car dummy) width))))
     (setq width (+ w width) height (max h height) depth (max d depth))
     (return result)))

(defprop mparen -1. lbp)
(defprop mparen -1. rbp)

(defun checkrat (form)
  (cond ((atom form) nil)
	((and (not (atom (car form))) (eq (caar form) 'mrat))
	 (if (member 'trunc (cdar form) :test #'eq)
	     '(#\space #\/ #\T #\/)
	     '(#\space #\/ #\R #\/)))
	((and (not (atom (car form))) (eq (caar form) 'mpois))
	 '(#\space #\/ #\P #\/))
	(t
	 (do ((l (cdr form) (cdr l)))
	     ((null l))
	   (cond ((atom l)
		  (merror (intl:gettext "display: not a well-formed Maxima expression: ~S") form))
		 ((setq form (checkrat (car l)))
		  (return form)))))))

(defun checkfit (w)
  (or (not break) (<= (- (+ w break right 1) bkptwd) linel)))

(defun checkbreak (result w)
  (cond ((not break))
	((> (- (setq w (+ w break)) bkptout) linel)
	 (if (or (null bkpt) (eq result bkpt))
	     (merror (intl:gettext "display: failed to break up a long expression.~%display: change 'linel' slightly and try again.")))
	 (do ((l result (cdr l)))
	     ;; THE NEED FOR EQUAL HERE IS PROBABLY THE SYMPTOM OF A BUG IN ECL !!
	     ;; PROBABLY RELATED TO SIDE-EFFECTS OF NRECONC, RPLACD, ETC !!
	     ((#+ecl equal #-ecl eq bkpt (cdr l)) (rplacd l nil))
	   (if (null l)
         ;; MEANING OF FOLLOWING MESSAGE IS OBSCURE.
         (merror (intl:gettext "display: 'checkbreak' not found."))))
	 (output bkpt 0)
	 (setq lines (1+ lines)
	       bkpt result bkptout bkptwd bkptwd w
	       bkptht maxht bkptdp maxdp bkptlevel level maxht 1 maxdp 0))
	((or (null bkpt) (<= level bkptlevel) (> (truncate linel 2) (- bkptwd bkptout)))
	 (setq bkpt result bkptwd w bkptlevel level
	       bkptht (max maxht bkptht) bkptdp (max maxdp bkptdp) maxht 1 maxdp 0))))

(defun forcebreak (result w)
  (output result 0)
  (setq lines (+ 2 lines)
	bkpt nil
	bkptout (+ w break)
	maxht 1 maxdp 0))

(defun update-heights (ht* dp*)
  (if break
      (setq maxht (max maxht ht*)
	    maxdp (max maxdp dp*))))

;;; BKPT	dimension structure for last breakpoint saved
;;; BKPTWD	width at last bkpt
;;; BKPTHT	height of current line to last bkpt
;;; BKPTDP	depth of current line to last bkpt
;;; BKPTOUT	width of stuff already output

;;; MAXHT	height from last bkpt saved to current point
;;; MAXDP	depth from last bkpt saved to current point

;;; BREAK	width up to last call to DIMENSION
;;; RESULT	dimension structure to current point minus output
;;; W		width from last call to DIMENSION to current point

;; Code above this point deals with dimensioning and constructing
;; up dimension strings.  Code past this point deals with printing
;; them.

;; <dimension string> ::= () | (<string element> . <dimension string>)
;; <string element> ::= character |
;;		     (<column-relative> <row-relative> . <dimension string>) |
;;		     (<drawing function> . args)
;; <column-relative> ::= <fixnum>
;; <row-relative>    ::= <fixnum>
;; <drawing function> ::= D-HBAR | D-VBAR | D-INTEGRALSIGN | ...

;; When a character appears in a dimension string, it is printed and
;; the cursor moves forward a single position.  (The variable OLDCOL is
;; incremented)  When a form with a fixnum car is encountered, the
;; first two elements of the form are taken to be relative displacements
;; for OLDCOL and OLDROW.  *** NOTE *** After drawing the cddr of the form,
;; OLDROW is reset to its original value, but OLDCOL is left in the new
;; position.  Why this is done is beyond me.  It only appears to complicate
;; things.

;; When a <drawing function> is invoked, the first argument passed to it is a
;; flag which is T for linear output and NIL for 2D output.  A
;; <drawing function> is also expected to return the new column position.

(defun output (result w)
;; The following is a hack to attempt to determine if we're on an
;; interactive terminal, in which case the user's hitting of the ENTER
;; key will have caused a newline to be displayed already.
#+(or gcl clisp) (cond ((not (equal *query-io* *standard-input*)) (fresh-line)))
#-(or gcl clisp) (cond ((not (interactive-stream-p *standard-input*)) (fresh-line)))
  (cond
    ;; If console output is disabled, don't output anything.
    (#.ttyoff)
    ;; Constant 80. in this test appears to be the size of LINEARRAY.
    ((> (+ bkptht bkptdp) 80.)
     ;; I suppose we could reallocate LINEARRAY to some larger size and keep going here ...
     (merror (intl:gettext "display: expression is too tall to be displayed.")))
    (t
     (output-linear (nreverse result) w))))

;; Output function for terminals without cursor positioning capability.
;; Characters are drawn into LINEARRAY instead.  Each element of LINEARRAY is a
;; list -- the car is how many spaces to indent; the cdr is a list of
;; characters to draw.  After drawing into this array, lines are printed one at
;; a time.  This is used for printing terminals and when writing to files.
;; Block mode i/o isn't needed since PRINC is used instead of WRITE-CHAR and
;; CURSORPOS.

(defun output-linear (result w)
  (draw-linear result bkptdp w)
  (do ((i (1- (+ bkptht bkptdp)) (1- i)))
      ((< i 0))
    (cond ((null (aref linearray i)))
	  (t (output-linear-one-line i)))))

(defun output-linear-one-line (i)
  (prog (line (n 0))
     (setq line (aref linearray i)
	   line (nreverse (cdr line))
	   n (car line))
     (setf (aref linearray i) nil)
     (tyotbsp n)
     (loop for v in (cdr line) do (write-char v))
     (mterpri)))

;; Move the cursor over N spaces to the left by outputting spaces.
;; This function assumes that the cursor is in the left margin when
;; it is called.  This is only called from OUTPUT-LINEAR, so it is
;; used only for printing terminals or for file output.

(defun tyotbsp (n)
  (do ()
      ((< n 1))
    (write-char #\space)
    (decf n)))

(defun draw-linear (dmstr oldrow oldcol)
  "This puts the LINE lists into LINEARRAY ready to be drawn.
   Each LINE consists of first an initial number of columns to space
   and then the characters to be printed.
   oldrow and oldcol are the starting points for the the (dx,dy) offsets
   given in the dimension string DMSTR.  It does not check that oldrow
   is big enough for possible negative y offsets in DMSTR, but BKPTDP is the
   right global to use for  oldrow."
  (do ((line))
      ((null dmstr))
    (cond ((atom (car dmstr))
	   (setq line (aref linearray oldrow))
	   (cond ((null line) (setq line (list oldcol)))
		 (t (prog (n)
		       (setq n (car line) line (cdr line))
		       (do ()
			   ((<= oldcol n))
			 (push #\space line)
			 (incf n)))))
	   (do ()
	       ((or (null dmstr) (not (atom (car dmstr))))
		(setf (aref linearray oldrow) (cons oldcol line)))
	     (incf oldcol)
	     (push (car dmstr) line)
	     (pop dmstr)))
	  ((integerp (caar dmstr))
	   ;; Why update OLDCOL and not OLDROW?  Should either update both
	   ;; (requiring multiple value return) or neither (analagous to lambda
	   ;; binding).
	   (setq oldcol (draw-linear (reverse (cddar dmstr))
				     (+ oldrow (cadar dmstr))
				     (+ oldcol (caar dmstr))))
	   (pop dmstr))
	  (t (setq oldcol (apply (caar dmstr) t (cdar dmstr)))
	     (pop dmstr))))
  ;; Be sure to return this.
  oldcol)

;; Special symbol drawing functions -- lines, boxes, summation signs, etc.
;; Every drawing function must take at least one argument.  The first
;; argument is T if equations must be printed line-by-line.  Otherwise,
;; draw them using cursor movement, character graphics, or line graphics
;; if possible.

;; Most of these functions just invoke DRAW-XXX on some constant
;; list structure, so be careful about NREVERSEing.  In other cases,
;; stuff is consed only for the linear case, but direct calls are used
;; in the 2D case.  This should work for both cases.  (See end of
;; program.)

(defun d-hbar (linear? w &optional (char #\-) &aux nl)
  (declare (ignore linear?))
  (dotimes (i w)
    (push char nl))
  (draw-linear nl oldrow oldcol))

;; Notice that in all of the height computations, an offset of 2 is added or
;; subtracted to the y-dimension.  This is to get the lines to fit within the
;; character cell precisely and not get clipped when moving things around in
;; the equation editor.

(defun d-vbar (linear? h d &optional (char #\|))
  (declare (ignore linear?))
  (setq d (- d))
  (do ((i (- h 2) (1- i))
       (nl `((0 ,(1- h) ,char))))
      ((< i d) (draw-linear (nreverse nl) oldrow oldcol))
    (push `(-1 ,i ,char) nl)))

(defun d-integralsign (linear? &aux dmstr)
  (declare (ignore linear?))
  (setq dmstr `((0 2 #\/) (-1 1 #\[) (-1 0 #\I) (-1 -1 #\]) (-1 -2 #\/)))
  (draw-linear dmstr oldrow oldcol))

(defun d-prodsign (linear? &aux dmstr)
  (declare (ignore linear?))
  (setq dmstr '((0 2 #\\ (d-hbar 3 #\=) #\/) (-4 0) (d-vbar 2 1 #\!) #\space (d-vbar 2 1 #\!) (1 0)))
  (draw-linear dmstr oldrow oldcol))

(defun d-sumsign (linear? &aux dmstr)
  (declare (ignore linear?))
  (setq dmstr '((0 2 (d-hbar 4 #\=)) (-4 1 #\\) #\> (-2 -1 #\/)	(-1 -2 (d-hbar 4 #\=))))
  (draw-linear dmstr oldrow oldcol))

;; Notice how this calls D-VBAR in the non-graphic case.  The entire output
;; side should be structured this way, with no consing of intermediate
;; dimension strings.

(defun d-matrix (linear? direction h d)
  (d-vbar linear? h d (car (coerce (if (eq direction 'right)
					     $rmxchar
					     $lmxchar) 'list))))

;; There is wired knowledge of character offsets here.

(defun d-box (linear? h d w body &aux (char 0) dmstr) ;char a char?
  (declare (ignore linear?))
  (setq char (car (coerce $boxchar 'list)))
  (setq dmstr `((0 ,h (d-hbar ,(+ 2 w) ,char))
		(,(- (+ w 2)) 0)
		(d-vbar ,h ,d ,char)
		,@body
		(,(- (1+ w)) ,(- (1+ d)) (d-hbar ,(+ w 2) ,char))
		(-1 0)
		(d-vbar ,h ,d ,char)))
  (draw-linear dmstr oldrow oldcol))
