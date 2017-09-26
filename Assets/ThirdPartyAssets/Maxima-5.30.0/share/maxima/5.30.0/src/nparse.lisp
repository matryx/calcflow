;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module nparse)

(load-macsyma-macros defcal mopers)

(defmvar *alphabet* (list #\_ #\%))

(defmfun alphabetp (n)
  (and (characterp n)
       (or (alpha-char-p n)
	   (member n *alphabet*))))

(defmfun ascii-numberp (num)
  (and (characterp num) (char<= num #\9) (char>= num #\0)))

(defvar *parse-window* nil)
(defvar *parse-stream* ()     "input stream for Maxima parser")
(defvar *parse-stream-eof* -1 "EOF value for Maxima parser")
(defvar *parse-tyi* nil)

(defvar *mread-prompt* nil    "prompt used by `mread'")
(defvar *mread-eof-obj* ()    "Bound by `mread' for use by `mread-raw'")

(defun mread-synerr (format-string &rest l)
    (let (tem
	  errset
	  (file "stdin"))
      (errset
       (setq tem (file-position *parse-stream*))
       (setq file  (namestring *parse-stream*)))
      (when tem
	(format t "~%~a:~a:" file tem))
      (format t (intl:gettext "incorrect syntax: "))
      (apply 'format t format-string (mapcar #'(lambda (x)
						 (if (symbolp x)
						     (print-invert-case x)
						     x))
					     l))
      (cond ((eql *parse-stream* *standard-input*)
	     (let ((n (get '*parse-window* 'length))
		   some ch)
	       (loop for i from (1- n) downto (- n 20)
		      while (setq ch (nth i *parse-window*))
		      do
		      (cond ((eql ch #\newline)
			     (push #\n some)
			     (push #\\ some))
			    ((eql ch #\tab)
			     (push #\t some)
			     (push #\\ some))
			    (t (push ch some))))
	       (format t "~%~{~c~}~%~vt^" some (- (length some) 2))
           (read-line *parse-stream* nil nil))))
      (terpri)
      (throw-macsyma-top)))

(defun tyi-parse-int (stream eof)
  (or *parse-window*
      (progn (setq *parse-window* (make-list 25))
	     (setf (get '*parse-window* 'length) (length *parse-window*))
	     (nconc *parse-window* *parse-window*)))
  (let ((tem (tyi stream eof)))
    (setf (car *parse-window*) tem *parse-window*
	  (cdr *parse-window*))
    (if (eql tem #\newline)
	(newline stream))
    tem))

(defun *mread-prompt* (out-stream char)
  (declare (ignore char))
  (format out-stream "~&~A" *mread-prompt*))

(defun aliaslookup (op)
  (if (symbolp op)
      (or (get op 'alias) op)
      op))

;;;; Tokenizing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;                                                                    ;;;;;
;;;;;                      The Input Scanner                             ;;;;;
;;;;;                                                                    ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gobble-whitespace ()
  (do ((ch (parse-tyipeek) (parse-tyipeek)))
      ((not (member ch '(#\tab #\space #\linefeed #\return #\page #\newline))))
    (parse-tyi)))

(defun read-command-token (obj)
  (gobble-whitespace)
  (read-command-token-aux obj))

(defun safe-assoc (item lis)
  "maclisp would not complain about (car 3), it gives nil"
  (loop for v in lis
	when (and (consp v)
		  (equal (car v) item))
	do
	(return v)))

;; list contains an atom, only check
;; (parser-assoc 1 '(2 1 3)) ==>(1 3)
;; (parser-assoc 1 '(2 (1 4) 3)) ==>(1 4)

(defun parser-assoc (c lis )
  (loop for v on lis
	 do
	 (cond ((consp (car v))
		(if (eq (caar v) c)
		    (return (car v))))
	       ((eql (car v) c)
		(return v)))))

;; we need to be able to unparse-tyi an arbitrary number of
;; characters, since if you do
;; PREFIX("ABCDEFGH");
;; then ABCDEFGA should read as a symbol.
;; 99% of the time we dont have to unparse-tyi, and so there will
;; be no consing...

(defun parse-tyi ()
  (let ((tem  *parse-tyi*))
    (cond ((null tem)
	   (tyi-parse-int *parse-stream* *parse-stream-eof*))
	  ((atom tem)
	   (setq *parse-tyi* nil)
	   tem)
	  (t ;;consp
	   (setq *parse-tyi* (cdr tem))
	   (car tem)))))

;; read one character but leave it there. so next parse-tyi gets it
(defun parse-tyipeek ()
  (let ((tem  *parse-tyi*))
    (cond ((null tem)
	   (setq *parse-tyi* (tyi-parse-int *parse-stream* *parse-stream-eof*)))
	  ((atom tem) tem)
	  (t (car tem)))))

;; push characters back on the stream
(defun unparse-tyi (c)
  (let ((tem  *parse-tyi*))
    (if (null tem)
	(setq *parse-tyi* c)
	(setq *parse-tyi* (cons c tem)))))

;;I know that the tradition says there should be no comments
;;in tricky code in maxima.  However the operator parsing
;;gave me a bit of trouble.   It was incorrect because
;;it could not handle things produced by the extensions
;;the following was broken for prefixes 

(defun read-command-token-aux (obj)
  (let* (result
	 (ch (parse-tyipeek))
	 (lis (if (eql ch *parse-stream-eof*)
		  nil
	          (parser-assoc ch obj))))
    (cond ((null lis)
	   nil)
          (t
	   (parse-tyi)
	   (cond ((atom (cadr lis))
		  ;; INFIX("ABC"); puts into macsyma-operators
		  ;;something like: (#\A #\B #\C (ANS $abc))
		  ;; ordinary things are like:
		  ;; (#\< (ANS $<) (#\= (ANS $<=)))
		  ;; where if you fail at the #\< #\X
	          ;; stage, then the previous step was permitted.
		  (setq result (read-command-token-aux (list (cdr lis)))))
	         ((null (cddr lis))
		  ;; lis something like (#\= (ANS $<=))
		  ;; and this says there are no longer operators
		  ;; starting with this.
		  (setq result
		        (and (eql (car (cadr lis)) 'ans)
		             ;; When we have an operator, which starts with a
		             ;; literal, we check, if the operator is
		             ;; followed with a whitespace. With this code
		             ;; Maxima parses an expression grad x or grad(x)
		             ;; as (($grad) x) and gradef(x) as (($gradef) x),
		             ;; when grad is defined as a prefix operator.
		             ;; See bug report ID: 2970792.
		             (or (not (alphabetp (cadr (exploden (cadr (cadr lis))))))
		                 (member (parse-tyipeek)
		                         '(#\tab #\space #\linefeed #\return #\page #\newline)))
		             (cadr (cadr lis)))))
	         (t
		  (let ((res   (and (eql (car (cadr lis)) 'ans)
				    (cadr (cadr lis))))
			(com-token (read-command-token-aux (cddr lis) )))
		    (setq result (or com-token res
				     (read-command-token-aux (list (cadr lis))))))))
	     (or result (unparse-tyi ch))
	     result))))

(defun scan-macsyma-token ()
  ;; note that only $-ed tokens are GETALIASed.
  (getalias (implode (cons '#\$ (scan-token t)))))

(defun scan-lisp-token ()
  (let ((charlist (scan-token nil)))
    (if charlist
	(implode charlist)
	(mread-synerr "Lisp symbol expected."))))

;; Example: ?mismatch(x+y,x*z,?:from\-end,true); => 3
(defun scan-keyword-token ()
  (let ((charlist (cdr (scan-token nil))))
    (if charlist
	(let ((*package* (find-package :keyword)))
	  (implode charlist))
	(mread-synerr "Lisp keyword expected."))))

(defun scan-token (flag)
  (do ((c (parse-tyipeek) (parse-tyipeek))
       (l () (cons c l)))
      ((or (eql c *parse-stream-eof*)
           (and flag
                (not (or (digit-char-p c (max 10 *read-base*))
                         (alphabetp c)
                         (char= c #\\ )))))
       (nreverse (or l (list (parse-tyi))))) ; Read at least one char ...
    (when (char= (parse-tyi) #\\ )
      (setq c (parse-tyi)))
    (setq flag t)))

(defun scan-lisp-string () (scan-string))
(defun scan-macsyma-string () (scan-string))

(defun scan-string (&optional init)
  (let ((buf (make-array 50 :element-type ' #.(array-element-type "a")
			 :fill-pointer 0 :adjustable t)))
    (when init
      (vector-push-extend init buf))
    (do ((c (parse-tyipeek) (parse-tyipeek)))
	((cond ((eql c *parse-stream-eof*))
	       ((char= c #\")
		(parse-tyi) t))
	 (copy-seq buf))
      (if (char= (parse-tyi) #\\ )
	  (setq c (parse-tyi)))
      (vector-push-extend c  buf))))

(defun readlist (lis)
  (read-from-string (coerce lis 'string)))

;; These variables control how we convert bfloat inputs to the
;; internal bfloat representation.  These variables should probably go
;; away after some testing.
(defmvar $fast_bfloat_conversion t
  "Use fast, but possibly inaccurate conversion")
(defmvar $fast_bfloat_threshold 100000.
  "Exponents larger than this (in absolute value) will use the fast
  conversion instead of the accurate conversion")
(defvar *fast-bfloat-extra-bits* 0)

;; Here is a test routine to test the fast bfloat conversion
#+nil
(defun test-make-number (&optional (n 1000))
  (let ((failures 0))
    (dotimes (k n)
      (flet ((digit-list (n)
	       (coerce (format nil "~D" n) 'list)))
	(let ((numlist nil))
	  ;; Generate a random number with 30 fraction digits and an
	  ;; large exponent.
	  (push (digit-list (random 10)) numlist)
	  (push '(#\.) numlist)
	  (push (digit-list (random (expt 10 30))) numlist)
	  (push '(#\B) numlist)
	  (push (if (zerop (random 2)) '(#\+) '(#\-)) numlist)
	  (push (digit-list (+ $fast_bfloat_threshold
			       (random $fast_bfloat_threshold)))
		numlist)
	  ;; Convert using accurate and fast methods and compare the
	  ;; results.
	  (let ((true (let (($fast_bfloat_conversion nil))
			(make-number (copy-list numlist))))
		(fast (let (($fast_bfloat_conversion t))
			(make-number (copy-list numlist)))))
	    (format t "Test ~3A: " k)
	    (map nil #'(lambda (x)
			 (map nil #'princ x))
		 (reverse numlist))
	    (terpri)
	    (unless (equalp true fast)
	      (incf failures)
	      (format t "NUM:  ~A~%  TRUE: ~S~%  FAST: ~S~%"
		      (reverse numlist) true fast))))))
    (format t "~D failures in ~D tests (~F%)~%"
	    failures n (* 100 failures (/ (float n))))))


;; WARNING: MAKE-NUMBER destructively modifies it argument!  Should we
;; change that?
(defun make-number (data)
  (setq data (nreverse data))
  ;; Maxima really wants to read in any number as a flonum
  ;; (except when we have a bigfloat, of course!).  So convert exponent
  ;; markers to the flonum-exponent-marker.
  (let ((marker (car (nth 3 data))))
    (unless (eql marker flonum-exponent-marker)
      (when (member marker '(#\E #\F #\S #\D #\L #+cmu #\W))
        (setf (nth 3 data) (list flonum-exponent-marker)))))
  (if (not (equal (nth 3 data) '(#\B)))
      (readlist (apply #'append data))
      (let*
	   ((*read-base* 10.)
	    (int-part (readlist (or (first data) '(#\0))))
	    (frac-part (readlist (or (third data) '(#\0))))
	    (frac-len (length (third data)))
	    (exp-sign (first (fifth data)))
	    (exp (readlist (sixth data))))
	(if (and $fast_bfloat_conversion
		 (> (abs exp) $fast_bfloat_threshold))
	    ;; Exponent is large enough that we don't want to do exact
	    ;; rational arithmetic.  Instead we do bfloat arithmetic.
	    ;; For example, 1.234b1000 is converted by computing
	    ;; bfloat(1234)*10b0^(1000-3).  Extra precision is used
	    ;; during the bfloat computations.
	    (let* ((extra-prec (+ *fast-bfloat-extra-bits* (ceiling (log exp 2e0))))
		   (fpprec (+ fpprec extra-prec))
		   (mant (+ (* int-part (expt 10 frac-len)) frac-part))
		   (bf-mant (bcons (intofp mant)))
		   (p (power (bcons (intofp 10))
			     (- (if (char= exp-sign #\-)
				    (- exp)
				    exp)
				frac-len)))
		   ;; Compute the product using extra precision.  This
		   ;; helps to get the last bit correct (but not
		   ;; always).  If we didn't do this, then bf-mant and
		   ;; p would be rounded to the target precision and
		   ;; then the product is rounded again.  Doing it
		   ;; this way, we still have 3 roundings, but bf-mant
		   ;; and p aren't rounded too soon.
		   (result (mul bf-mant p)))
	      (let ((fpprec (- fpprec extra-prec)))
		;; Now round the product back to the desired precision.
		(bigfloatp result)))
	    ;; For bigfloats, turn them into rational numbers then
	    ;; convert to bigfloat.  Fix for the 0.25b0 # 2.5b-1 bug.
	    ;; Richard J. Fateman posted this fix to the Maxima list
	    ;; on 10 October 2005.  Without this fix, some tests in
	    ;; rtestrationalize will fail.  Used with permission.
	    (let ((ratio (* (+ int-part (* frac-part (expt 10 (- frac-len))))
			    (expt 10 (if (char= exp-sign #\-)
					 (- exp)
					 exp)))))
	    ($bfloat (cl-rat-to-maxima ratio)))))))

;; Richard J. Fateman wrote the big float to rational code and the function
;; cl-rat-to-maxmia.

(defun cl-rat-to-maxima (x)
  (if (integerp x)
      x
      (list '(rat simp) (numerator x) (denominator x))))

(defun scan-digits (data continuation? continuation &optional exponent-p)
  (do ((c (parse-tyipeek) (parse-tyipeek))
       (l () (cons c l)))
      ((not (and (characterp c) (digit-char-p c (max 10. *read-base*))))
       (cond ((member c continuation?)
	      (funcall continuation (list* (ncons (char-upcase
						   (parse-tyi)))
					   (nreverse l)
					   data)))
	     ((and (null l) exponent-p)
	      ;; We're trying to parse the exponent part of a number,
	      ;; and we didn't get a value after the exponent marker.
	      ;; That's an error.
	      (merror (intl:gettext "parser: incomplete number; missing exponent?")))
	     (t
	      (make-number (cons (nreverse l) data)))))
    (parse-tyi)))

(defun scan-number-after-dot (data)
  (scan-digits data '(#\E #\e #\F #\f #\B #\b #\D #\d #\S #\s) #'scan-number-exponent))

(defun scan-number-exponent (data)
  (push (ncons (if (or (char= (parse-tyipeek) #\+)
		       (char= (parse-tyipeek) #\-))
		   (parse-tyi)
		   #\+))
	data)
  (scan-digits data () () t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;                                                                    ;;;;;
;;;;;                    The Expression Parser                           ;;;;;
;;;;;                                                                    ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;	Based on a theory of parsing presented in:                       ;;;
;;;                                                                      ;;;
;;;	    Pratt, Vaughan R., ``Top Down Operator Precedence,''         ;;;
;;;	    ACM Symposium on Principles of Programming Languages         ;;;
;;;	    Boston, MA; October, 1973.                                   ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;	Implementation Notes ....
;;;
;;;	JPG	Chars like ^A, ^B, ... get left around after interrupts and
;;;		should be thrown away by the scanner if not used as editting
;;;		commands.
;;;
;;;	KMP	There is RBP stuff in DISPLA, too. Probably this sort of
;;;		data should all be in one place somewhere.
;;;
;;;	KMP	Maybe the parser and/or scanner could use their own GC scheme
;;;		to recycle conses used in scan/parse from line to line which
;;;		really ought not be getting dynamically discarded and reconsed.
;;;	        Alternatively, we could call RECLAIM explicitly on certain
;;;		pieces of structure which get used over and over. A
;;;		local-reclaim abstraction may want to be developed since this
;;;		stuff will always be needed, really. On small-address-space
;;;		machines, this could be overridden when the last DYNAMALLOC
;;;		GC barrier were passed (indicating that space was at a premium
;;;		-- in such case, real RECLAIM would be more economical -- or
;;;		would the code to control that be larger than the area locked
;;;		down ...?)
;;;
;;;	KMP	GJC has a MAKE-EVALUATOR type package which could probably
;;;		replace the CALL-IF-POSSIBLE stuff used here.
;;;             [So it was written, so it was done. -gjc]
;;;
;;;	KMP	DEFINE-SYMBOL and KILL-OPERATOR need to be redefined.
;;;		Probably these shouldn't be defined in this file anyway.
;;;
;;;	KMP	The relationship of thisfile to SYNEX needs to be thought
;;;		out more carefully.
;;;
;;;	GJC	Need macros for declaring INFIX, PREFIX, etc ops
;;;
;;;	GJC	You know, PARSE-NARY isn't really needed it seems, since
;;;		the SIMPLIFIER makes the conversion of
;;;			((MTIMES) ((MTIMES) A B) C) => ((MTIMES) A B C)
;;;		I bet you could get make "*" infix and nobody would
;;;		ever notice.

;;; The following terms may be useful in deciphering this code:
;;;
;;; NUD -- NUll left Denotation (op has nothing to its left (prefix))
;;; LED -- LEft Denotation	(op has something to left (postfix or infix))
;;;
;;; LBP -- Left Binding Power  (the stickiness to the left)
;;; RBP -- Right Binding Power (the stickiness to the right)
;;;

;;;; Macro Support

(defvar scan-buffered-token (list nil)
  "put-back buffer for scanner, a state-variable of the reader")

(defun peek-one-token ()
  (peek-one-token-g nil nil))

(defun peek-one-token-g (eof-ok? eof-obj)
  (cond
   ((car scan-buffered-token)
    (cdr scan-buffered-token))
   (t (rplacd scan-buffered-token (scan-one-token-g eof-ok? eof-obj))
      (cdr (rplaca scan-buffered-token t)))))

(defun scan-one-token ()
  (scan-one-token-g nil nil))

(defun scan-one-token-g (eof-ok? eof-obj)
  (declare (special macsyma-operators))
  (cond ((car scan-buffered-token)
	 (rplaca scan-buffered-token ())
	 (cdr scan-buffered-token))
	((read-command-token macsyma-operators))
	(t
	 (let ((test (parse-tyipeek)))
	   (cond  ((eql test *parse-stream-eof*)
		   (parse-tyi)
		   (if eof-ok? eof-obj
		       (maxima-error (intl:gettext "parser: end of file while scanning expression."))))
		  ((eql test #\/)
		   (parse-tyi)
		   (cond ((char= (parse-tyipeek) #\*)
                          (parse-tyi)
			  (gobble-comment)
			  (scan-one-token-g eof-ok? eof-obj))
			 (t '$/)))
		  ((eql test #\.) (parse-tyi)	; Read the dot
		   (if (digit-char-p (parse-tyipeek) 10.)
		       (scan-number-after-dot (list (ncons #\.) nil))
		       '|$.|))
		  ((eql test #\")
		   (parse-tyi)
		   (scan-macsyma-string))
		  ((eql test #\?)
		   (parse-tyi)
		   (cond ((char= (parse-tyipeek) #\")
			  (parse-tyi)
			  (scan-lisp-string))
			 ((char= (parse-tyipeek) #\:)
			  (scan-keyword-token))
			 (t
			  (scan-lisp-token))))
		  (t
		   (if (digit-char-p test 10.)
		       (scan-number-before-dot ())
		       (scan-macsyma-token))))))))

;; nested comments are permitted.
(defun gobble-comment ()
  (prog (c depth)
	(setq depth 1)
     read
	(setq c (parse-tyipeek))
	(parse-tyi)
	(cond ((= depth 0) (return t)))
	(cond ((and (numberp c) (< c 0))(error (intl:gettext "parser: end of file in comment.")))
	      ((char= c #\*)
	       (cond ((char= (parse-tyipeek) #\/)
		      (decf depth)
		      (parse-tyi)
		      (cond ((= depth 0) (return t)))
		      (go read))))
	      ((char= c #\/)
	       (cond ((char= (parse-tyipeek) #\*)
		      (incf depth) (parse-tyi)
		      (go read)))))
        (go read))
  )

(defun scan-number-rest (data)
  (let ((c (caar data)))
    (cond ((member c '(#\.))
	   ;; We found a dot
	   (scan-number-after-dot data))
	  ((member c '(#\E #\e #\F #\f #\B #\b #\D #\d #\S #\s))
	   ;; Dot missing but found exponent marker.  Fake it.
	   (setf data (push (ncons #\.) (rest data)))
	   (push (ncons #\0) data)
	   (push (ncons c) data)
	   (scan-number-exponent data)))))

(defun scan-number-before-dot (data)
  (scan-digits data '(#\. #\E #\e #\F #\f #\B #\b #\D #\d #\S #\s)
	       #'scan-number-rest))


;; "First character" and "Pop character"

(defmacro first-c () '(peek-one-token))
(defmacro pop-c   () '(scan-one-token))

(defun mstringp (x) (stringp x)) ;; OBSOLETE. PRESERVE FOR SAKE OF POSSIBLE CALLS FROM NON-MAXIMA CODE !!

(defun inherit-propl (op-to op-from getl)
  (let ((propl (getl op-from getl)))
    (if propl
	(progn (remprop op-to (car propl))
	       (putprop op-to (cadr propl) (car propl)))
	(inherit-propl op-to
		       (maxima-error "has no ~a properties. ~a ~a" getl op-from 'wrng-type-arg)
		       getl))))


;;; (NUD <op>)
;;; (LED <op> <left>)
;;;
;;;  <op>   is the name of the operator which was just popped.
;;;  <left> is the stuff to the left of the operator in the LED case.
;;;

(eval-when
  #+gcl (eval compile load)
  #-gcl (:execute :compile-toplevel :load-toplevel)
  (defmacro def-nud-equiv (op equiv)
    (list 'putprop (list 'quote op) (list 'function equiv)
          (list 'quote 'nud)))

  (defmacro nud-propl () ''(nud))

  (defmacro def-nud-fun (op-name op-l . body)
    (list* 'defun-prop (list* op-name 'nud 'nil) op-l body))

  (defmacro def-led-equiv (op equiv)
    (list 'putprop (list 'quote op) (list 'function equiv)
          (list 'quote 'led)))

  (defmacro led-propl () ''(led))

  (defmacro def-led-fun (op-name op-l . body)
    (list* 'defun-prop (list* op-name 'led 'nil) op-l body)))

(defun nud-call (op)
  (let ((tem (and (symbolp op) (getl op '(nud)))) res)
    (setq res
	  (if (null tem)
	      (if (operatorp op)
		  (mread-synerr "~A is not a prefix operator" (mopstrip op))
		  (cons '$any op))
	      (funcall (cadr tem) op)))
    res))

(defun led-call (op l)
  (let ((tem (and (symbolp op) (getl op '(led)))) res)
    (setq res
	  (if (null tem)
	      (mread-synerr "~A is not an infix operator" (mopstrip op))
	      (funcall (cadr tem) op l)))
    res))

;;; (DEF-NUD (op lbp rbp) bvl . body)
;;;
;;;  Defines a procedure for parsing OP as a prefix operator.
;;;
;;;  OP  should be the name of the symbol as a string or symbol.
;;;  LBP is an optional left  binding power for the operator.
;;;  RBP is an optional right binding power for the operator.
;;;  BVL must contain exactly one variable, which the compiler will not
;;;      complain about if unused, since it will rarely be of use anyway.
;;;      It will get bound to the operator being parsed.
;;;  lispm:Optional args not allowed in release 5 allowed, necessary afterwards..

(defmacro def-nud ((op . lbp-rbp) bvl . body)
  (let (( lbp (nth 0 lbp-rbp))
	( rbp (nth 1 lbp-rbp)))
    `(progn 'compile 	  ,(make-parser-fun-def op 'nud bvl body)
	    (set-lbp-and-rbp ',op ',lbp ',rbp))))

(defun set-lbp-and-rbp (op lbp rbp)
  (cond ((not (consp op))
	 (let ((existing-lbp (get op 'lbp))
	       (existing-rbp (get op 'rbp)))
	   (cond ((not lbp) ;; ignore ommitted arg
		  )
		 ((not existing-lbp)
		  (putprop op lbp 'lbp))
		 ((not (equal existing-lbp lbp))
		  (maxima-error "Incompatible LBP's defined for this operator ~a" op)))
	   (cond ((not rbp) ;; ignore ommitted arg
		  )
		 ((not existing-rbp)
		  (putprop op rbp 'rbp))
		 ((not (equal existing-rbp rbp))
		  (maxima-error "Incompatible RBP's defined for this operator ~a" op)))))
	(t
	 (mapcar #'(lambda (x) (set-lbp-and-rbp x lbp rbp))
		 op))))

;;; (DEF-LED (op lbp rbp) bvl . body)
;;;
;;;  Defines a procedure for parsing OP as an infix or postfix operator.
;;;
;;;  OP  should be the name of the symbol as a string or symbol.
;;;  LBP is an optional left  binding power for the operator.
;;;  RBP is an optional right binding power for the operator.
;;;  BVL must contain exactly two variables, the first of which the compiler
;;;       will not complain about if unused, since it will rarely be of use
;;;	  anyway. Arg1 will get bound to the operator being parsed. Arg2 will
;;;	  get bound to the parsed structure which was to the left of Arg1.


(defmacro def-led((op . lbp-rbp) bvl . body)
  (let (( lbp (nth 0 lbp-rbp))
	( rbp (nth 1 lbp-rbp)))
    `(progn 'compile
	    ,(make-parser-fun-def  op 'led bvl body)
	    (set-lbp-and-rbp ',op ',lbp ',rbp))))

(defmacro def-collisions (op &rest alist)
  (let ((keys (do ((i 1 (ash i 1))
		   (lis  alist (cdr lis))
		   (nl ()    (cons (cons (caar lis) i) nl)))
		  ((null lis) nl))))
    `(progn 'compile
       (defprop ,op ,(let nil
			  (copy-tree keys )) keys)
       ,@(mapcar #'(lambda (data)
		     `(defprop ,(car data)
			       ,(do ((i 0 (logior i  (cdr (assoc (car lis) keys :test #'eq))))
				     (lis (cdr data) (cdr lis)))
				    ((null lis) i))
			       ,op))
		 alist))))


(defun collision-lookup (op active-bitmask key-bitmask)
  (let ((result (logand active-bitmask key-bitmask)))
    (if (not (zerop result))
	(do ((l (get op 'keys) (cdr l)))
	    ((null l) (parse-bug-err 'collision-check))
	  (if (not (zerop (logand result (cdar l))))
	      (return (caar l)))))))

(defun collision-check (op active-bitmask key)
  (let ((key-bitmask (get key op)))
    (if (not key-bitmask)
	(mread-synerr "~A is an unknown keyword in a ~A statement."
		      (mopstrip key) (mopstrip op)))
    (let ((collision (collision-lookup op active-bitmask key-bitmask)))
      (if collision
	  (if (eq collision key)
	      (mread-synerr "This ~A's ~A slot is already filled."
			    (mopstrip op)
			    (mopstrip key))
	      (mread-synerr "A ~A cannot have a ~A with a ~A field."
			    (mopstrip op)
			    (mopstrip key)
			    (mopstrip collision))))
      (logior (cdr (assoc key (get op 'keys) :test #'eq)) active-bitmask))))

;;;; Data abstraction

;;; LBP = Left Binding Power
;;;
;;; (LBP <op>)		 - reads an operator's Left Binding Power
;;; (DEF-LBP <op> <val>) - defines an operator's Left Binding Power

(defmfun lbp (lex) (cond ((safe-get lex 'lbp)) (t 200.)))

(defmacro def-lbp (sym val) `(defprop ,sym ,val lbp))

;;; RBP = Right Binding Power
;;;
;;; (RBP <op>)		 - reads an operator's Right Binding Power
;;; (DEF-RBP <op> <val>) - defines an operator's Right Binding Power

(defmfun rbp (lex) (cond ((safe-get lex 'rbp)) (t 200.)))

(defmacro def-rbp (sym val) `(defprop ,sym ,val rbp))

(defmacro def-match (x m) `(defprop ,x ,m match))

;;; POS = Part of Speech!
;;;
;;; (LPOS <op>)
;;; (RPOS <op>)
;;; (POS  <op>)
;;;

(defun lpos (op) (cond ((safe-get op 'lpos)) (t '$any)))
(defun rpos (op) (cond ((safe-get op 'rpos)) (t '$any)))
(defun pos (op) (cond ((safe-get op 'pos)) (t '$any)))

(defmacro def-pos  (op pos) `(defprop ,op ,pos  pos))
(defmacro def-rpos (op pos) `(defprop ,op ,pos rpos))
(defmacro def-lpos (op pos) `(defprop ,op ,pos lpos))

;;; MHEADER

(defun mheader (op) (add-lineinfo (or (safe-get op 'mheader) (ncons op))))

(defmacro def-mheader (op header) `(defprop ,op ,header mheader))


(defmvar $parsewindow 10.
	 "The maximum number of 'lexical tokens' that are printed out on
each side of the error-point when a syntax (parsing) MAXIMA-ERROR occurs.  This
option is especially useful on slow terminals.  Setting it to -1 causes the
entire input string to be printed out when an MAXIMA-ERROR occurs."
	 fixnum)


;;;; Misplaced definitions

(defmacro def-operatorp ()
  `(defmfun operatorp (lex)
     (and (symbolp lex) (getl lex '(,@(nud-propl) ,@(led-propl))))))

(def-operatorp)

(defmacro def-operatorp1 ()
  ;Defmfun -- used by SYNEX if not others.
  `(defmfun operatorp1 (lex)
     ;; Referenced outside of package: OP-SETUP, DECLARE1
     ;; Use for truth value only, not for return-value.
     (and (symbolp lex) (getl lex '(lbp rbp ,@(nud-propl) ,@(led-propl))))))

(def-operatorp1)

;;;; The Macsyma Parser

;;; (MREAD) with arguments compatible with losing maclisp READ style.
;;;
;;; Returns a parsed form of tokens read from stream.
;;;
;;; If you want rubout processing, be sure to call some stream which knows
;;; about such things. Also, I'm figuring that the PROMPT will be
;;; an atribute of the stream which somebody can hack before calling
;;; MREAD if he wants to.


(defvar *current-line-info* nil)

;;Important for lispm rubout handler
(defun mread (&rest read-args)
  (progn
    (when *mread-prompt*
      (and *parse-window* (setf (car *parse-window*) nil
				*parse-window* (cdr *parse-window*)))
      (princ *mread-prompt*)
      (force-output))
    (apply 'mread-raw read-args)))

(defun mread-prompter (stream char)
  (declare (special *mread-prompt-internal*)
	   (ignore char))
  (fresh-line stream)
  (princ *mread-prompt-internal* stream))

;; input can look like:
;;aa && bb && jim:3;

(defun mread-raw (*parse-stream* &optional *mread-eof-obj*)
  (let ((scan-buffered-token (list nil))
	*parse-tyi*)
    (if (eq scan-buffered-token ;; a handly unique object for the EQ test.
	    (peek-one-token-g t scan-buffered-token))
	*mread-eof-obj*
	(do ((labels ())
	     (input (parse '$any 0.) (parse '$any 0.)))
	    (nil)
	  (case (first-c)
	    ((|$;| |$$|)
	      ;force a separate line info structure
	     (setf *current-line-info* nil)
	     (return (list (mheader (pop-c))
			   (if labels (cons (mheader '|$[|) (nreverse labels)))
			   input)))
	    ((|$&&|)
	     (pop-c)
	     (if (symbolp input)
		 (push input labels)
		 (mread-synerr "Invalid && tag. Tag must be a symbol")))
	    (t
	     (parse-bug-err 'mread-raw)))))))

;;; (PARSE <mode> <rbp>)
;;;
;;;  This will parse an expression containing operators which have a higher
;;;  left binding power than <rbp>, returning as soon as an operator of
;;;  lesser or equal binding power is seen. The result will be in the given
;;;  mode (which allows some control over the class of result expected). 
;;;  Modes used are as follows:
;;;	$ANY    = Match any type of expression
;;;	$CLAUSE = Match only boolean expressions (or $ANY)
;;;	$EXPR   = Match only mathematical expressions (or $ANY)
;;;  If a mismatched mode occurs, a syntax error will be flagged. Eg,
;;;  this is why "X^A*B" parses but "X^A and B" does not. X^A is a $EXPR
;;;  and not coercible to a $CLAUSE. See CONVERT.
;;;
;;;  <mode> is the required mode of the result.
;;;  <rbp>  is the right binding power to use for the parse. When an
;;;	     LED-type operator is seen with a lower left binding power
;;;	     than <rbp>, this parse returns what it's seen so far rather
;;;	     than calling that operator.
;;;

(defun parse (mode rbp)
  (do ((left (nud-call (pop-c))		; Envoke the null left denotation
	     (led-call (pop-c) left)))	;  and keep calling LED ops as needed
      ((>= rbp (lbp (first-c)))		; Until next op lbp too low
       (convert left mode))))		;  in which case, return stuff seen

;;; (PARSE-PREFIX <op>)
;;;
;;;  Parses prefix forms -- eg, -X or NOT FOO.
;;;
;;;  This should be the NUD property on an operator. It fires after <op>
;;;  has been seen. It parses forward looking for one more expression
;;;  according to its right binding power, returning
;;;  ( <mode> . ((<op>) <arg1>) )

(defun parse-prefix (op)
  (list (pos op)			; Operator mode
	(mheader op)			; Standard Macsyma expression header
	(parse (rpos op) (rbp op))))	; Convert single argument for use

;;; (PARSE-POSTFIX <op> <left>)
;;;
;;;  Parses postfix forms. eg, X!.
;;;
;;;  This should be the LED property of an operator. It fires after <left>
;;;  has been accumulated and <op> has been seen and gobbled up. It returns
;;;  ( <mode> . ((<op>) <arg1>) )

(defun parse-postfix (op l)
  (list (pos op)			; Operator's mode
	(mheader op)			; Standard Macsyma expression header
	(convert l (lpos op))))		; Convert single argument for use

;;; (PARSE-INFIX <op> <left>)
;;;
;;;  Parses infix (non-nary) forms. eg, 5 mod 3.
;;;
;;;  This should be the led property of an operator. It fires after <left>
;;;  has been accumulated and <op> has been seen and gobbled up. It returns
;;;  ( <mode> . ((<op>) <arg1> <arg2>) )

(defun parse-infix (op l)
  (list (pos op)			; Operator's mode
	(mheader op)			; Standard Macsyma expression header
	(convert l (lpos op))		; Convert arg1 for immediate use
	(parse (rpos op) (rbp op))))	; Look for an arg2 

;;; (PARSE-NARY <op> <left>)
;;;
;;;  Parses nary forms. Eg, form1*form2*... or form1+form2+...
;;;  This should be the LED property on an operator. It fires after <op>
;;;  has been seen, accumulating and returning
;;;  ( <mode> . ((<op>) <arg1> <arg2> ...) )
;;;
;;;  <op>   is the being parsed.
;;;  <left> is the stuff that has been seen to the left of <op> which
;;;         rightly belongs to <op> on the basis of parse precedence rules.

(defun parse-nary (op l)
  (list* (pos op)			    ; Operator's mode
	 (mheader op)			    ; Normal Macsyma operator header
	 (convert l (lpos op))		    ; Check type-match of arg1
	 (prsnary op (lpos op) (lbp op))))  ; Search for other args

;;; (PARSE-MATCHFIX <lop>)
;;;
;;;  Parses matchfix forms. eg, [form1,form2,...] or (form1,form2,...)
;;;
;;;  This should be the NUD property on an operator. It fires after <op>
;;;  has been seen. It parses <lop><form1>,<form2>,...<rop> returning
;;;  ( <mode> . ((<lop>) <form1> <form2> ...) ).

(defun parse-matchfix (op)
  (list* (pos op)			         ; Operator's mode
	 (mheader op)			         ; Normal Macsyma operator header
	 (prsmatch (safe-get op 'match) (lpos op))))  ; Search for matchfixed forms

;;; (PARSE-NOFIX <op>)
;;;
;;;  Parses an operator of no args. eg, @+X where @ designates a function
;;;  call (eg, @() is implicitly stated by the lone symbol @.)
;;;
;;;  This should be a NUD property on an operator which takes no args.
;;;  It immediately returns ( <mode> . ((<op>)) ).
;;;
;;;  <op> is the name of the operator.
;;;
;;;  Note: This is not used by default and probably shouldn't be used by
;;;   someone who doesn't know what he's doing. Example lossage. If @ is
;;;   a nofix op, then @(3,4) parses, but parses as "@"()(3,4) would -- ie,
;;;   to ((MQAPPLY) (($@)) 3 4) which is perhaps not what the user will expect.

(defun parse-nofix (op) (list (pos op) (mheader op)))

;;; (PRSNARY <op> <mode> <rbp>)
;;;
;;;  Parses an nary operator tail Eg, ...form2+form3+... or ...form2*form3*...
;;;
;;;  Expects to be entered after the leading form and the first call to an
;;;  nary operator has been seen and popped. Returns a list of parsed forms
;;;  which belong to that operator. Eg, for X+Y+Z; this should be called
;;;  after the first + is popped. Returns (Y Z) and leaves the ; token
;;;  in the parser scan buffer.
;;;
;;;  <op>   is the nary operator in question.
;;;  <rbp>  is (LBP <op>) and is provided for efficiency. It is for use in
;;;	     recursive parses as a binding power to parse for.
;;;  <mode> is the name of the mode that each form must be.

(defun prsnary (op mode rbp)
  (do ((nl (list (parse mode rbp))	   ; Get at least one form
	   (cons (parse mode rbp) nl)))	   ;  and keep getting forms
      ((not (eq op (first-c)))		   ; until a parse pops on a new op
       (nreverse nl))			   ;  at which time return forms
      (pop-c)))				   ; otherwise pop op

;;; (PRSMATCH <match> <mode>)
;;;
;;; Parses a matchfix sequence. Eg, [form1,form2,...] or (form1,form2,...)
;;; Expects to be entered after the leading token is the popped (ie, at the
;;;  point where the parse of form1 will begin). Returns (form1 form2 ...).
;;;
;;; <match> is the token to look for as a matchfix character.
;;; <mode>  is the name of the mode that each form must be.

(defun prsmatch (match mode)			  ; Parse for matchfix char
  (cond ((eq match (first-c)) (pop-c) nil)	  ; If immediate match, ()
	(t					  ; Else, ...
	 (do ((nl (list (parse mode 10.))	  ;  Get first element
		  (cons (parse mode 10.) nl)))	  ;   and Keep adding elements
	     ((eq match (first-c))		  ;  Until we hit the match.
	      (pop-c)				  ;   Throw away match.
	      (nreverse nl))			  ;   Put result back in order
	   (if (eq '|$,| (first-c))		  ;  If not end, look for ","
	       (pop-c)				  ;   and pop it if it's there
	       (mread-synerr "Missing ~A"	  ;   or give an error message.
			     (mopstrip match)))))))

;;; (CONVERT <exp> <mode>)
;;;
;;;  Parser coercion function.
;;;
;;;  <exp>  should have the form ( <expressionmode> . <expression> )
;;;  <mode> is the target mode.
;;;
;;;  If <expressionmode> and <mode> are compatible, returns <expression>.

(defun convert (item mode)
  (if (or (eq mode (car item))		; If modes match exactly
	  (eq '$any mode)		;    or target is $ANY
	  (eq '$any (car item)))	;    or input is $ANY
      (cdr item)			;  then return expression
      (mread-synerr "Found ~A expression where ~A expression expected"
		    (get (car item) 'english)
		    (get mode       'english))))

(defprop $any    "untyped"   english)
(defprop $clause "logical"   english)
(defprop $expr   "algebraic" english)

;;;; Parser Error Diagnostics

 ;; Call this for random user-generated parse errors

(defun parse-err () (mread-synerr "Syntax error"))

 ;; Call this for random internal parser lossage (eg, code that shouldn't
 ;;  be reachable.)

(defun parse-bug-err (op)
  (mread-synerr
    "Parser bug in ~A. Please report this to the Maxima maintainers,~
   ~%including the characters you just typed which caused the error. Thanks."
    (mopstrip op)))

;;; Random shared error messages

(defun delim-err (op)
  (mread-synerr "Illegal use of delimiter ~A" (mopstrip op)))

(defun erb-err (op l) l ;Ignored
  (mread-synerr "Too many ~A's" (mopstrip op)))

(defun premterm-err (op)
  (mread-synerr "Premature termination of input at ~A."
		(mopstrip op)))

;;;; Operator Specific Data

(def-nud-equiv |$]| delim-err)
(def-led-equiv |$]| erb-err)
(def-lbp     |$]| 5.)

(def-nud-equiv	|$[| parse-matchfix)
(def-match	|$[| |$]|)
(def-lbp	|$[| 200.)
;No RBP
(def-mheader	|$[| (mlist))
(def-pos	|$[| $any)
(def-lpos	|$[| $any)
;No RPOS

(def-led (|$[| 200.) (op left)
  (setq left (convert left '$any))
  (if (numberp left) (parse-err))			; number[...] invalid
  (let ((header (if (atom left)
		    (add-lineinfo (list (amperchk left) 'array))
		  (add-lineinfo '(mqapply array))))
	(right (prsmatch '|$]| '$any)))			; get sublist in RIGHT
    (cond ((null right)					; 1 subscript minimum
	   (mread-synerr "No subscripts given"))
	  ((atom left)					; atom[...]
	   (setq right (cons header
			     right))
	   (cons '$any (aliaslookup right)))
	  (t						; exp[...]
	   (cons '$any (cons header
			     (cons left right)))))))


(def-nud-equiv |$)| delim-err)
(def-led-equiv |$)| erb-err)
(def-lbp       |$)| 5.)

(def-mheader   |$(| (mprogn))

  ;; KMP: This function optimizes out (exp) into just exp.
  ;;  This is useful for mathy expressions, but obnoxious for non-mathy
  ;;  expressions. I think DISPLA should be made smart about such things,
  ;;  but probably the (...) should be carried around in the internal
  ;;  representation. This would make things like BUILDQ much easier to
  ;;  work with.
  ;; GJC: CGOL has the same behavior, so users tend to write extensions
  ;;  to the parser rather than write Macros per se. The transformation
  ;;  "(EXP)" ==> "EXP" is done by the evaluator anyway, the problem
  ;;  comes inside quoted expressions. There are many other problems with
  ;;  the "QUOTE" concept however.

(def-nud (|$(| 200.) (op)
  (let ((right)(hdr (mheader '|$(|)))        ; make mheader first for lineinfo
    (cond ((eq '|$)| (first-c)) (parse-err))		  ; () is illegal
	  ((or (null (setq right (prsmatch '|$)| '$any))) ; No args to MPROGN??
	       (cdr right))				  ;  More than one arg.
	   (cons '$any (cons hdr right)))	  ; Return an MPROGN
	  (t (cons '$any (car right))))))		  ; Optimize out MPROGN

(def-led (|$(| 200.) (op left)
  (setq left (convert left '$any))		        ;De-reference LEFT
  (if (numberp left) (parse-err))			;number(...) illegal
  (let ((hdr (and (atom left)(mheader (amperchk left))))
	(r (prsmatch '|$)| '$any))                       ;Get arglist in R
	)
    (cons '$any						;Result is type $ANY
	  (cond ((atom left)				;If atom(...) =>
		 (cons hdr r))    ;(($atom) exp . args)
		(t				        ;Else exp(...) =>
		 (cons '(mqapply) (cons left r)))))))	;((MQAPPLY) op . args)

(def-mheader |$'| (mquote))

(def-nud (|$'|) (op)
  (let (right)
    (cond ((eq '|$(| (first-c))
	   (list '$any (mheader '|$'|) (parse '$any 190.)))
	  ((or (atom (setq right (parse '$any 190.)))
	       (member (caar right) '(mquote mlist mprog mprogn lambda) :test #'eq))
	   (list '$any (mheader '|$'|) right))
	  ((eq 'mqapply (caar right))
	   (cond ((eq (caaadr right) 'lambda)
		  (list '$any (mheader '|$'|) right))
		 (t (rplaca (cdr right)
			    (cons (cons ($nounify (caaadr right))
					(cdaadr right))
				  (cdadr right)))
		    (cons '$any right))))
	  (t (cons '$any (cons (cons ($nounify (caar right)) (cdar right))
			       (cdr right)))))))

(def-nud (|$''|) (op)
  (let (right)
    (cons '$any
	  (cond ((eq '|$(| (first-c))  (meval (parse '$any 190.)))
		((atom (setq right (parse '$any 190.))) (meval1 right))
		((eq 'mqapply (caar right))
		 (rplaca (cdr right)
			 (cons (cons ($verbify (caaadr right)) (cdaadr right))
			       (cdadr right)))
		 right)
		(t (cons (cons ($verbify (caar right)) (cdar right))
			 (cdr right)))))))

(def-led-equiv |$:| parse-infix)
(def-lbp       |$:| 180.)
(def-rbp       |$:|  20.)
(def-pos       |$:| $any)
(def-rpos      |$:| $any)
(def-lpos      |$:| $any)
(def-mheader   |$:| (msetq))

(def-led-equiv |$::| parse-infix)
(def-lbp       |$::| 180.)
(def-rbp       |$::|  20.)
(def-pos       |$::| $any)
(def-rpos      |$::| $any)
(def-lpos      |$::| $any)
(def-mheader   |$::| (mset))

(def-led-equiv |$:=| parse-infix)
(def-lbp       |$:=| 180.)
(def-rbp       |$:=|  20.)
(def-pos       |$:=| $any)
(def-rpos      |$:=| $any)
(def-lpos      |$:=| $any)
(def-mheader   |$:=| (mdefine))

(def-led-equiv |$::=| parse-infix)
(def-lbp       |$::=| 180.)
(def-rbp       |$::=|  20.)
(def-pos       |$::=| $any)
(def-rpos      |$::=| $any)
(def-lpos      |$::=| $any)
(def-mheader   |$::=| (mdefmacro))

(def-led-equiv	|$!| parse-postfix)
(def-lbp	|$!| 160.)
;No RBP
(def-pos	|$!| $expr)
(def-lpos	|$!| $expr)
;No RPOS
(def-mheader	|$!| (mfactorial))

(def-mheader |$!!| ($genfact))

(def-led (|$!!| 160.) (op left)
  (list '$expr
	(mheader '$!!)
	(convert left '$expr)
	(list (mheader '$/) (convert left '$expr) 2)
	2))

(def-lbp     |$^| 140.)
(def-rbp     |$^| 139.)
(def-pos     |$^| $expr)
(def-lpos    |$^| $expr)
(def-rpos    |$^| $expr)
(def-mheader |$^| (mexpt))

(def-led ((|$^| |$^^|)) (op left)
  (cons '$expr 
	(aliaslookup (list (mheader op)
			   (convert left (lpos op))
			   (parse (rpos op) (rbp op))))))

(mapc #'(lambda (prop) ; Make $** like $^
	  (let ((propval (get '$^ prop)))
	    (if propval (putprop '$** propval prop))))
      '(lbp rbp pos rpos lpos mheader))

(inherit-propl  '$** '$^ (led-propl))

(def-lbp     |$^^| 140.)
(def-rbp     |$^^| 139.)
(def-pos     |$^^| $expr)
(def-lpos    |$^^| $expr)
(def-rpos    |$^^| $expr)
(def-mheader |$^^| (mncexpt))

;; note y^^4.z gives an error because it scans the number 4 together with
;; the trailing '.' as a decimal place.    I think the error is correct.
(def-led-equiv	|$.| parse-infix)
(def-lbp	|$.| 130.)
(def-rbp	|$.| 129.)
(def-pos	|$.| $expr)
(def-lpos	|$.| $expr)
(def-rpos	|$.| $expr)
(def-mheader	|$.| (mnctimes))

(def-led-equiv	|$*| parse-nary)
(def-lbp	|$*| 120.)
;RBP not needed
(def-pos	|$*| $expr)
;RPOS not needed
(def-lpos	|$*| $expr)
(def-mheader	|$*| (mtimes))

(def-led-equiv	$/  parse-infix)
(def-lbp	$/  120.)
(def-rbp	$/  120.)
(def-pos	$/  $expr)
(def-rpos	$/  $expr)
(def-lpos	$/  $expr)
(def-mheader	$/  (mquotient))

(def-nud-equiv	|$+| parse-prefix)
(def-lbp	|$+| 100.)
(def-rbp	|$+| 134.) ; Value increased from 100 to 134 (DK 02/2010).
(def-pos	|$+| $expr)
(def-rpos	|$+| $expr)
;LPOS not needed
(def-mheader	|$+| (mplus))

(def-led ((|$+| |$-|) 100.) (op left)
  (setq left (convert left '$expr))
  (do ((nl (list (if (eq op '$-)
		     (list (mheader '$-) (parse '$expr 100.))
		     (parse '$expr 100.))
		 left)
	   (cons (parse '$expr 100.) nl)))
      ((not (member (first-c) '($+ $-) :test #'eq))
       (list* '$expr (mheader '$+) (nreverse nl)))
    (if (eq (first-c) '$+) (pop-c))))

(def-nud-equiv	|$-| parse-prefix)
(def-lbp	|$-| 100.)
(def-rbp	|$-| 134.)
(def-pos	|$-| $expr)
(def-rpos	|$-| $expr)
;LPOS not needed
(def-mheader	|$-| (mminus))

(def-led-equiv	|$=| parse-infix)
(def-lbp	|$=| 80.)
(def-rbp	|$=| 80.)
(def-pos	|$=| $clause)
(def-rpos	|$=| $expr)
(def-lpos	|$=| $expr)
(def-mheader	|$=| (mequal))

(def-led-equiv	|$#| parse-infix)
(def-lbp	|$#| 80.)
(def-rbp	|$#| 80.)
(def-pos	|$#| $clause)
(def-rpos	|$#| $expr)
(def-lpos	|$#| $expr)
(def-mheader	|$#| (mnotequal))

(def-led-equiv	|$>| parse-infix)
(def-lbp	|$>| 80.)
(def-rbp	|$>| 80.)
(def-pos	|$>| $clause)
(def-rpos	|$>| $expr)
(def-lpos	|$>| $expr)
(def-mheader	|$>| (mgreaterp))

(def-led-equiv	|$>=| parse-infix)
(def-lbp	|$>=| 80.)
(def-rbp	|$>=| 80.)
(def-pos	|$>=| $clause)
(def-rpos	|$>=| $expr)
(def-lpos	|$>=| $expr)
(def-mheader	|$>=| (mgeqp))

(def-led-equiv	|$<| parse-infix)
(def-lbp	|$<| 80.)
(def-rbp	|$<| 80.)
(def-pos	|$<| $clause)
(def-rpos	|$<| $expr)
(def-lpos	|$<| $expr)
(def-mheader	|$<| (mlessp))

(def-led-equiv	|$<=| parse-infix)
(def-lbp	|$<=| 80.)
(def-rbp	|$<=| 80.)
(def-pos	|$<=| $clause)
(def-rpos	|$<=| $expr)
(def-lpos	|$<=| $expr)
(def-mheader	|$<=| (mleqp))

(def-nud-equiv	$not parse-prefix)
;LBP not needed
(def-rbp	$not 70.)
(def-pos	$not $clause)
(def-rpos	$not $clause)
(def-lpos	$not $clause)
(def-mheader	$not (mnot))

(def-led-equiv	$and parse-nary)
(def-lbp	$and 65.)
;RBP not needed
(def-pos	$and $clause)
;RPOS not needed
(def-lpos	$and $clause)
(def-mheader	$and (mand))

(def-led-equiv	$or parse-nary)
(def-lbp	$or 60.)
;RBP not needed
(def-pos	$or $clause)
;RPOS not needed
(def-lpos	$or $clause)
(def-mheader	$or (mor))

(def-led-equiv	|$,| parse-nary)
(def-lbp	|$,| 10.)
;RBP not needed
(def-pos	|$,| $any)
;RPOS not needed
(def-lpos	|$,| $any)
(def-mheader	|$,| ($ev))

(def-nud-equiv $then delim-err)
(def-lbp $then 5.)
(def-rbp $then 25.)

(def-nud-equiv $else delim-err)
(def-lbp $else 5.)
(def-rbp $else 25.)

(def-nud-equiv $elseif delim-err)
(def-lbp  $elseif 5.)
(def-rbp  $elseif 45.)
(def-pos  $elseif $any)
(def-rpos $elseif $clause)

;No LBP - Default as high as possible
(def-rbp     $if 45.)
(def-pos     $if $any)
(def-rpos    $if $clause)
;No LPOS
(def-mheader $if (mcond))

(def-nud ($if) (op)
  (list* (pos op)
	 (mheader op)
	 (parse-condition op)))

(defun parse-condition (op)
  (list* (parse (rpos op) (rbp op))
	 (if (eq (first-c) '$then)
	     (parse '$any (rbp (pop-c)))
	     (mread-synerr "Missing `then'"))
	 (case (first-c)
	   (($else)   (list t (parse '$any (rbp (pop-c)))))
	   (($elseif) (parse-condition (pop-c)))
	   (t ; Note: $false instead of () makes DISPLA suppress display!
	    (list t '$false)))))

(def-mheader $do (mdo))

(defun parse-$do (lex &aux (left (make-mdo)))
  (setf (car left) (mheader 'mdo))
  (do ((op lex (pop-c))  (active-bitmask 0))
      (nil)
    (if (eq op '|$:|) (setq op '$from))
    (setq active-bitmask (collision-check '$do active-bitmask op))
    (let ((data (parse (rpos op) (rbp op))))
      (case op
	($do		(setf (mdo-body left) data) (return (cons '$any left)))
	($for		(setf (mdo-for  left) data))
	($from		(setf (mdo-from left) data))
	($in		(setf (mdo-op   left) 'mdoin)
			(setf (mdo-from left) data))
	($step		(setf (mdo-step left) data))
	($next		(setf (mdo-next left) data))
	($thru		(setf (mdo-thru left) data))
	(($unless $while)
			(if (eq op '$while)
			    (setq data (list (mheader '$not) data)))
			(setf (mdo-unless left)
			   (if (null (mdo-unless left))
			       data
			       (list (mheader '$or) data (mdo-unless left)))))
	(t (parse-bug-err '$do))))))

(def-lbp $for    25.)
(def-lbp $from   25.)
(def-lbp $step   25.)
(def-lbp $next   25.)
(def-lbp $thru   25.)
(def-lbp $unless 25.)
(def-lbp $while  25.)
(def-lbp $do	 25.)

(def-nud-equiv $for    parse-$do)
(def-nud-equiv $from   parse-$do)
(def-nud-equiv $step   parse-$do)
(def-nud-equiv $next   parse-$do)
(def-nud-equiv $thru   parse-$do)
(def-nud-equiv $unless parse-$do)
(def-nud-equiv $while  parse-$do)
(def-nud-equiv $do     parse-$do)

(def-rbp $do      25.)
(def-rbp $for    200.)
(def-rbp $from    95.)
(def-rbp $in      95.)
(def-rbp $step    95.)
(def-rbp $next    45.)
(def-rbp $thru    95.)
(def-rbp $unless  45.)
(def-rbp $while	  45.)

(def-rpos $do     $any)
(def-rpos $for    $any)
(def-rpos $from   $any)
(def-rpos $step   $expr)
(def-rpos $next   $any)
(def-rpos $thru   $expr)
(def-rpos $unless $clause)
(def-rpos $while  $clause)


(def-collisions $do
  ($do	   . ())
  ($for    . ($for))
  ($from   . ($in $from))
  ($in     . ($in $from $step $next))
  ($step   . ($in       $step $next))
  ($next   . ($in	$step $next))
  ($thru   . ($in $thru)) ;$IN didn't used to get checked for
  ($unless . ())
  ($while  . ()))

(def-mheader   |$$| (nodisplayinput))
(def-nud-equiv |$$| premterm-err)
(def-lbp       |$$| -1)
;No RBP, POS, RPOS, RBP, or MHEADER

(def-mheader   |$;| (displayinput))
(def-nud-equiv |$;| premterm-err)
(def-lbp       |$;| -1)
;No RBP, POS, RPOS, RBP, or MHEADER

(def-nud-equiv  |$&&| delim-err)
(def-lbp	|$&&| -1)

(defun mopstrip (x)
  ;; kludge interface function to allow the use of lisp PRINC in places.
  (cond ((null x) 'false)
	((or (eq x t) (eq x 't)) 'true)
	((numberp x) x)
	((symbolp x)
	 (or (get x 'reversealias)
	     (let ((name (symbol-name x)))
	       (if (member (char name 0) '(#\$ #\%) :test #'char=)
		   (subseq name 1)
		   name))))
	(t x)))

(define-initial-symbols
    ;; * Note: /. is looked for explicitly rather than
    ;;     existing in this chart. The reason is that
    ;;     it serves a dual role (as a decimal point) and
    ;;     must be special-cased.
    ;;
    ;;     Same for // because of the /* ... */ handling
    ;;     by the tokenizer
    ;; Single character
    |+| |-| |*| |^| |<| |=| |>| |(| |)| |[| |]| |,|
    |:| |!| |#| |'| |;| |$| |&|
    ;;Two character
    |**| |^^| |:=| |::| |!!| |<=| |>=| |''| |&&|
    ;; Three character
    |::=|
    )

;; !! FOLLOWING MOVED HERE FROM MLISP.LISP (DEFSTRUCT STUFF)
;; !! SEE NOTE THERE
(define-symbol "@") 

;;; User extensibility:
(defmfun $prefix (operator &optional (rbp  180.)
			             (rpos '$any)
				     (pos  '$any))
  (def-operator operator pos ()  ()     rbp rpos () t
		'(nud . parse-prefix) 'msize-prefix 'dimension-prefix ()   )
  operator)

(defmfun $postfix (operator &optional (lbp  180.)
			             (lpos '$any)
				     (pos  '$any))
  (def-operator operator pos lbp lpos   ()  ()   t  ()
		'(led . parse-postfix) 'msize-postfix 'dimension-postfix  ()   )
  operator)

(defmfun $infix  (operator &optional (lbp  180.)
			             (rbp  180.)
				     (lpos '$any)
				     (rpos '$any)
				     (pos  '$any))
  (def-operator operator pos lbp lpos   rbp rpos t t
		'(led . parse-infix) 'msize-infix 'dimension-infix () )
  operator)

(defmfun $nary   (operator &optional (bp     180.)
			             (argpos '$any)
				     (pos    '$any))
  (def-operator operator pos bp  argpos bp  ()   t t
		'(led . parse-nary) 'msize-nary 'dimension-nary () )
  operator)

(defmfun $matchfix (operator
		    match  &optional (argpos '$any)
				     (pos    '$any))
  ;shouldn't MATCH be optional?
  (def-operator operator pos ()  argpos ()  ()  () ()
		'(nud . parse-matchfix) 'msize-matchfix 'dimension-match match)
  operator)

(defmfun $nofix  (operator &optional (pos '$any))
  (def-operator operator pos ()  ()     ()  () () ()
		'(nud . parse-nofix) 'msize-nofix 'dimension-nofix ()   )
  operator)

;;; (DEF-OPERATOR op pos lbp lpos rbp rpos sp1 sp2
;;;	parse-data grind-fn dim-fn match)
;;; OP        is the operator name.
;;; POS       is its ``part of speech.''
;;; LBP       is its ``left binding power.''
;;; LPOS      is the part of speech of the arguments to its left, or of all.
;;;            arguments for NARY and MATCHFIX.
;;; RBP       is its ``right binding power.''
;;; RPOS      is the part of speech of the argument to its right.
;;; SP1       says if the DISSYM property needs a space on the right.
;;; SP2       says if the DISSYM property needs a space on the left.
;;; PARSE-DATA is (prop . fn) -- parser prop name dotted with function name
;;; GRIND-FN  is the grinder function for the operator.
;;; DIM-FN    is the dimension function for the operator.
;;; PARSEPROP is the property name to use for parsing. One of LED or NUD.
;;; MATCH     if non-(), ignores SP1 and SP2. Should be the match symbol.
;;;	        sets OP up as matchfix with MATCH.
;;;
;;; For more complete descriptions of these naming conventions, see
;;; the comments in GRAM package, which describe them in reasonable detail.

(defun def-operator (op pos lbp lpos rbp rpos sp1 sp2
			parse-data grind-fn dim-fn match)
  (let ((x))
    (if (or (and rbp (not (integerp (setq x rbp))))
	    (and lbp (not (integerp (setq x lbp)))))
	(merror (intl:gettext "syntax extension: binding powers must be integers; found: ~M") x))
    (if (stringp op) (setq op (define-symbol op)))
    (op-setup op)
    (let ((noun   ($nounify op))
	  (dissym (cdr (exploden op))))
      (cond
       ((not match)
	(setq dissym (append (if sp1 '(#\space)) dissym (if sp2 '(#\space)))))
       (t (if (stringp match) (setq match (define-symbol match)))
	  (op-setup match)
	  (putprop op    match 'match)
	  (putprop match 5.    'lbp)
	  (setq dissym (cons dissym (cdr (exploden match))))))
      (putprop op pos 'pos)
      (putprop op (cdr parse-data) (car parse-data))
      (putprop op   grind-fn  'grind)
      (putprop op   dim-fn    'dimension)
      (putprop noun dim-fn    'dimension)
      (putprop op   dissym 'dissym)
      (putprop noun dissym 'dissym)
      (when rbp
	(putprop op   rbp  'rbp)
	(putprop noun rbp  'rbp))
      (when lbp
	(putprop op   lbp  'lbp)
	(putprop noun lbp  'lbp))
      (when lpos (putprop op   lpos 'lpos))
      (when rpos (putprop op   rpos 'rpos))
      (getopr op))))

(defun op-setup (op)
  (declare (special *mopl*))
  (let ((dummy (or (get op 'op)
                   (coerce (string* op) 'string))))
    (putprop op    dummy 'op )
    (putopr dummy op)
    (if (and (operatorp1 op) (not (member dummy (cdr $props) :test #'eq)))
	(push dummy *mopl*))
    (add2lnc dummy $props)))

(defun kill-operator (op)
  (let
    ((opr (get op 'op))
     (noun-form ($nounify op)))
    ;; Refuse to kill an operator which appears on *BUILTIN-$PROPS*.
    (unless (member opr *builtin-$props* :test #'equal)
      (undefine-symbol opr)
      (remopr opr)
      (rempropchk opr)
      (mapc #'(lambda (x) (remprop op x))
   	  '(nud nud-expr nud-subr			; NUD info
  		     led led-expr led-subr		; LED info
  		     lbp rbp			; Binding power info
  		     lpos rpos pos		; Part-Of-Speech info
  		     grind dimension dissym	; Display info
  		     op))			; Operator info
      (mapc #'(lambda (x) (remprop noun-form x))
   	  '(dimension dissym lbp rbp)))))



;; the functions get-instream etc.. are all defined in
;; gcl lsp/debug.lsp
;; they are all generic common lisp and could be used by
;; any Common lisp implementation.

#-gcl
(defvar *stream-alist* nil)

#-gcl
(defun stream-name (path)
  (let ((tem (errset (namestring (pathname path)))))
    (car tem)))

#-gcl
(defun instream-name (instr)
  (or (instream-stream-name instr)
      (stream-name (instream-stream instr))))

#-gcl
(defstruct instream
  stream
  (line 0 :type fixnum)
  stream-name)

;; (closedp stream) checks if a stream is closed.
;; how to do this in common lisp!!

#-gcl
(defun cleanup ()
  #+never-clean-up-dont-know-how-to-close
  (dolist (v *stream-alist*)
    (if (closedp (instream-stream v))
	(setq *stream-alist* (delete v *stream-alist*)))))

#-gcl
(defun get-instream (str)
  (or (dolist (v *stream-alist*)
	(cond ((eq str (instream-stream v))
	       (return v))))
      (let (name errset)
	(errset (setq name (namestring str)))
	(car (setq *stream-alist*
		   (cons  (make-instream :stream str :stream-name name)
			  *stream-alist*))))))

(defun newline (str)
  (incf (instream-line (get-instream str)))
  (values))

(defun find-stream (stream)
   (dolist (v *stream-alist*)
	(cond ((eq stream (instream-stream v))
	       (return v)))))


(defun add-lineinfo (lis)
  (if (or (atom lis) (and (eq *parse-window* *standard-input*)
			  (not (find-stream *parse-stream*))))
			  lis
    (let* ((st (get-instream *parse-stream*))
 	   (n (instream-line st))
	   (nam (instream-name st)))
      (or nam (return-from add-lineinfo lis))
      (setq *current-line-info*
	    (cond ((eq (cadr *current-line-info*) nam)
		   (cond ((eql (car *current-line-info*) n)
			  *current-line-info*)
			 (t  (cons n (cdr *current-line-info*)))))
		  (t (list n nam  'src))))
      (cond ((null (cdr lis))
	     (list (car lis) *current-line-info*))
	    (t (append lis (list *current-line-info*)))))))

;; Remove debugging stuff.
;; STRIP-LINEINFO does not modify EXPR.

(defun strip-lineinfo (expr)
  (if (atom expr) expr
    (cons (strip-lineinfo-op (car expr)) (mapcar #'strip-lineinfo (cdr expr)))))

;; If something in the operator looks like debugging stuff, remove it.
;; It is assumed here that debugging stuff is a list comprising an integer and a string
;; (and maybe other stuff, which is ignored).

(defun strip-lineinfo-op (maxima-op)
  (remove-if #'(lambda (x) (and (consp x) (integerp (first x)) (stringp (second x)))) maxima-op))
