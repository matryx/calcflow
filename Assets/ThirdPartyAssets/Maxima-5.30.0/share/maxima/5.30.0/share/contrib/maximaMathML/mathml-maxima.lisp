;;;;;;;;;;;;;;;;; File:  mathml-maxima.lsp  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Purpose:   Enabling maxima to receive mathml contentent-coded input
;;
;; Usage:     compile this file with UNIX command
;;                %mc maxima-mp.lsp
;;             which produces mathml-maxima.o
;;
;;            load into MAXIMA by MAXIMA top-level comamnd
;;                loadfile("mathml-maxima.lsp");
;;
;; Author: Paul S. Wang
;; Date: 3/06/2000
;
; Authors:  Paul S. Wang, Kent State University
; This work was supported by NSF/USA.
; Permission to use this work for any purpose is granted provided that
; the copyright notice, author and support credits above are retained.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(declaim (special *tag* *special-proc* *parse-stream* *in* parse-tyipeek))

(defvar *in* *parse-stream*  "input stream to read")

(setq parse-tyipeek nil) ;; look-ahead in nparse.lisp

(defvar *tag* nil "tag of element returned by ml2ma")


;;;;; mltoma is the top-level mathml input function

(defun $mathml()
  (meval (mltoma *parse-stream*)))

(defun mltoma(&optional *in*)
(prog(ans)
   (if (null *in*) (setq *in* t))
   (setq g (get-tag))
   (if (not (eq g 'math)) (return nil))
   (setq ans (ml2ma))
   (setq g (get-tag)) ;; this should be </math>
   (return ans)))

(defun ml2ma ()
  (multiple-value-bind (tag attributes) (get-tag)
    (setq *tag* tag)
    (cond
      ((eq tag 'ci) (ml-ci attributes))
      ((eq tag 'cn) (ml-cn attributes))
      ((eq tag 'apply) (ml-apply))
      ((member tag '(bvar lowlimit uplimit) :test #'eq)
       (setq ans (ml2ma)) (get-tag) ans)
      ((eq tag '/apply) nil)
      (t (merror "unknown or invalid mathml tag: ~A" tag)))))

(defun ml-apply()
  (prog(op *special-proc* ans)
     (setq op (get-op))
     (cond ((null op)
	    (if *special-proc* (return (apply *special-proc* nil))
		(merror "internal error: null mct-proc"))
	    ))
     (do ((operand (ml2ma) (ml2ma))) ;; returns nil after seeing </apply>
	 ((null operand) (setq ans (cons op (nreverse ans)))) ;; done
       (setq ans (cons operand ans))
       )
     (return ans)))

;; <apply>
;; <diff/> <apply> <fn>G</fn><ci>X</ci> </apply>
;; <bvar><ci>X</ci><degree><cn type="integer">2</cn></degree></bvar>
;; </apply>
;;(($DIFF) (($F) $X) $X 2)

(defun mctdiff()
  (let ((fn (ml2ma)) (var-deg (diff-bvar))) 
       (get-tag) ;; lose </apply>
       (cons (list '$diff) (cons fn var-deg))))

(defun mctintegrate()
  (prog(var nt ll up grand)
     (setq var (get-bvar))
     (setq nt (ml2ma)) 
     (cond ((eq *tag* 'lowlimit)
	    (setq ll nt)
	    (setq up (ml2ma))
	    (cond ((eq *tag* 'uplimit)
		   (setq grand (ml2ma))
		   (get-tag) ;; lose </apply> for <int\>  
		   (return (list '($integrate) grand var ll up)))
		  (t (merror "definite intergral error"))
		  ))
	   )
     ;; indefinte integral
     (setq grand nt)
     (get-tag) ;; lose </apply>
     (return (list '($integrate) grand var))))
	  
(defun get-bvar()
(prog(tag v)
     (setq tag (get-tag))
     (if (not (eq tag 'bvar)) (merror "Expecting bvar but got ~A" tag))
     (setq v (ml2ma))
     (get-tag) ;; lose </bvar>
     (return v)
))

(defun diff-bvar()
(prog(tag v d)
     (setq tag (get-tag))
     (if (not (eq tag 'bvar)) (merror "Expecting bvar but got ~A" tag))
     (setq v (ml2ma))
     (setq tag (get-tag))
     (if (not (eq tag 'degree)) (merror "Expecting degree but got ~A" tag))
     (setq d (ml2ma))
     (get-tag)(get-tag) ;; skip closing tags
     (return (list v d))
))
      
(defun ml-cn (attributes)
(prog(type number)
   (setq type (find-attribute "type" attributes))  ;; always has type
   (cond
     ((or (equal type "integer") (equal type "float"))
      (setq number (get-number)))
     ((equal type "constant")
      (setq number (coerce (get-token) 'string))
      (setq number (mathml-constant-to-maxima-constant number)))
     (t
       ;; Dunno what we could do here to handle an arbitrary type.
       ;; For now, just raise an error.
       (merror "Unrecognized type ``%a'' in <cn> tag.~%" type)))
   (get-token) ;; skip to >
   (return number)))

(defun ml-ci (attributes)
(prog(parse-tyipeek a *parse-stream* type)
   (setq type (find-attribute "type" attributes)) ;; may or may not have type
   (cond
     ((equal type "constant") ;; math constants
      (setq a (coerce (get-token) 'string))
      (setq a (mathml-constant-to-maxima-constant a)))
     (t
       ;; normal identifier
       (setq a (read-from-string (concatenate 'string "$" (get-token))))))
  (get-token) ;; skip to >
  (return a)))

(defun mathml-constant-to-maxima-constant (name-string)
  (cond
    ((equal name-string "&pi;") '$%pi)
    ((equal name-string "&ee;") '$%e)
    ((equal name-string "&ii;") '$%i)
    ((equal name-string "&gamma;") '$%gamma)
    (t
      (let
        ((bare-name (string-left-trim "&" (string-right-trim ";" name-string))))
        (read-from-string (concatenate 'string "$" bare-name))))))

(defun get-number()
  (let ((s (get-str #\<)))
    (if s (read-from-string s))))

;; returns next non-white non #\> char (or -1?)
(defun next-char ()
(do (c) (nil)      ; Gobble whitespace
    (if (member (setq c (tyi *in* -1))
           '(#\> #\tab #\space #\linefeed #\return #\page))
        nil
        (return c)
    )
))

(defun get-tag(&optional (endc #\>))
(prog(tag c)
   (setq c (next-char))
   (if (not (char= c #\<)) (return nil))
   (return (get-atag endc))
))

(defun get-atag(&optional endc)
(prog(str)
    (setq str (get-str endc))
    (if (null str) (return nil))
    (return
      (let*
        ((tag+attributes
           (split-string str '(#\tab #\space #\linefeed #\return #\page)))
         (tag (car tag+attributes))
         (attributes (mapcar #'split-attribute (cdr tag+attributes))))
        (values (read-from-string tag) attributes)))))

;; Split "FOO=\"BAR\"" into ("FOO" . "BAR")
(defun split-attribute (attr-string)
  (let ((i (position #\= attr-string)))
    (if (null i)
      (list attr-string)
      (let
        ((name (subseq attr-string 0 i))
         (value (dequotify (subseq attr-string (+ i 1)))))
        `(,name . ,value)))))

;; Remove first character and last characters if the first character
;; is a single or double quote and last character is the same as the first.
;; (Do not call STRING-TRIM, because might strip off multiple characters.)

(defun dequotify (s)
  (if (or (null s) (< (length s) 2))
    s
    (let
      ((first-char (aref s 0))
       (last-char (aref s (1- (length s)))))
      (if
        (and
          (or (eq first-char #\") (eq first-char #\'))
          (eq last-char first-char))
        (subseq (subseq s 1) 0 (- (length s) 2))
        s))))

(defun find-attribute (attr attrs-list)
  (cdr (assoc attr attrs-list :test #'equal)))

(defun get-str(&optional endc)
(prog(str)
   (setq str (get-token endc))
   (if str
      (return (coerce str 'string))
      (return nil)
   )
))

;; returns list of chars for next token
(defun get-token (&optional endc)
  ; Read at least one char ...
  (do ((c (tyi *in* -1) (tyi *in* -1))
       (l () (cons c l)))
    ((or
       (equal c -1)
       (and endc (char= c endc))
       (and (not endc) (member c '(#\< #\> #\tab #\space #\linefeed #\return #\page))))
     (nreverse (or l (ncons (tyi *in* -1)))))))

(defun get-op ()
(prog(op mop opa)
    (setq op (get-tag))
    (cond ((eq op 'fn) 
             (setq op (get-str))
	     (cond ((null op)
		(merror "get-op: invalid null function")
	     ))
	     (setq opa (read-from-string op))
             (setq opa (get opa 'mmfun))
             (get-token)
	     (if opa (return (list opa)))
             (return (list (read-from-string
		  (concatenate 'string "$" op))))
          )
	  ((setq proc (get op 'mct-proc)) 
	     (setq *special-proc* proc)
	     (return nil)
	  )
    )
    (setq mop (get op 'mmfun))
    (if mop (return (list mop))
	    (return (list op))   ;; should not reach here
    )
))

;;;(defmacro upcase (operator)
;;;`(setq operator (intern (string-upcase (string ,operator)))))

(defun set-table (arg)
(prog(a b)
  (cond ((equal (length arg) 2)
          (setq a (cadadr arg))
	  (setq b (caadr arg))
          (if (stringp a) (setq a (read-from-string a)))
          (setf (get a b) (car arg))
	)
        ((equal (length arg) 3)
	  (setq arg (cdr arg))
	  (setq b (car arg) a (cadr arg))
	  (setq a (cadr a))
          (if (stringp a) (setq a (read-from-string a)))
	  (setf (get a (car b)) (cadr b))
	)
  )
))

;;;;;;;;;;; tables ;;;;;;;;;;;;
;;(set-table '(%sin  (mmfun "sin/")))
;;(set-table '(%cos  (mmfun "cos/")))
;;(set-table '(%tan  (mmfun "tan/")))
;;(set-table '(%cot  (mmfun "cot/")))
;;(set-table '(%sec  (mmfun "sec/")))
;;(set-table '(%csc  (mmfun "csc/")))

;;(set-table '(%asin  (mmfun "arcsin/")))
;;(set-table '(%acos  (mmfun "arccos/")))
;;(set-table '(%atan  (mmfun "arctan/")))
;;(set-table '(%acot  (mmfun "acot/")))
;;(set-table '(%asec  (mmfun "asec/")))
;;(set-table '(%acsc  (mmfun "acsc/")))
;;(set-table '(%sinh  (mmfun "sinh/")))
;;(set-table '(%cosh  (mmfun "cosh/")))
;;(set-table '(%tanh  (mmfun "tanh/")))
;;(set-table '(%coth  (mmfun "coth/")))
;;(set-table '(%sech  (mmfun "sec/")))
;;(set-table '(%csch  (mmfun "csch/")))


;;(set-table '(%asinh  (mmfun "asinh/")))
;;(set-table '(%acosh  (mmfun "acosh/")))
;;(set-table '(%atanh  (mmfun "atanh/")))
;;(set-table '(%acoth  (mmfun "acoth/")))
;;(set-table '(%asech  (mmfun "asec/")))
;;(set-table '(%acsch  (mmfun "acsch/")))

(set-table '(%ln  (mmfun "ln/")))
(set-table '(%log  (mmfun "log/")))

(set-table '($sin  (mmfun "sin/")))
(set-table '($cos  (mmfun "cos/")))
(set-table '($tan  (mmfun "tan/")))
(set-table '($cot  (mmfun "cot/")))
(set-table '($sec  (mmfun "sec/")))
(set-table '($csc  (mmfun "csc/")))

(set-table '($asin  (mmfun "arcsin/")))
(set-table '($acos  (mmfun "arccos/")))
(set-table '($atan  (mmfun "arctan/")))
(set-table '($acot  (mmfun "acot/")))
(set-table '($asec  (mmfun "asec/")))
(set-table '($acsc  (mmfun "acsc/")))

(set-table '($sinh  (mmfun "sinh/")))
(set-table '($cosh  (mmfun "cosh/")))
(set-table '($tanh  (mmfun "tanh/")))
(set-table '($coth  (mmfun "coth/")))
(set-table '($sech  (mmfun "sec/")))
(set-table '($csch  (mmfun "csch/")))

(set-table '($asinh  (mmfun "asinh/")))
(set-table '($acosh  (mmfun "acosh/")))
(set-table '($atanh  (mmfun "atanh/")))
(set-table '($acoth  (mmfun "acoth/")))
(set-table '($asech  (mmfun "asec/")))
(set-table '($acsch  (mmfun "acsch/")))
(set-table '($ln  (mmfun "ln/")))
(set-table '($log  (mmfun "log/")))


;;;;; containers
;;(set-table '(mlist (mct-proc mctlist)))
;;(set-table '($matrix (mct-proc  mctmatrix)))
;;(set-table '($vector (mct-proc  mctvector)))

;;;;;;; Operators and functions
(set-table '(mand  (mmfun "and/")))
(set-table '(mor  (mmfun "or/")))
(set-table '(mnot  (mmfun "not/")))
(set-table '($xor  (mmfun "xor/")))

(set-table '(mplus  (mmfun "plus/")))
(set-table '(mminus  (mmfun "minus/")))
;;(set-table '($minus (mmfun "minus/")))
;;(set-table '(mdif  (mmfun "minus/")))
(set-table '($remainder  (mmfun "rem/")))
(set-table '($max  (mmfun "max/")))
(set-table '($min  (mmfun "min/")))
(set-table '(mfactorial  (mmfun "factorial/")))
(set-table '(mabs (mmfun "abs/")))
(set-table '(%abs (mct-proc abs)))
;;(set-table '(mnctimes  (mmfun "times/ type=\"noncommutative\"")))
(set-table '(mtimes  (mmfun "times/")))
(set-table '(mexpt (mmfun "power/")))
(set-table '(mquotient (mmfun "quotient/"))) 
(set-table '(%sqrt (mmfun "sqrt/")))
(set-table '(mquote  (mmfun "quote/")))

(set-table '(mgreaterp  (mct-proc relation) (mmfun "gt/")))
(set-table '(mgeqp (mct-proc relation)  (mmfun "geq/")))
(set-table '(mequal (mct-proc relation)  (mmfun "eq/")))
(set-table '(mnotequal (mct-proc relation)  (mmfun "neq/")))
(set-table '(mleqp (mct-proc relation)  (mmfun "leq/")))
(set-table '(mlessp (mct-proc relation)  (mmfun "lt/")))

(set-table '(mdefine (mct-proc def-fun)))

(set-table '(msetq  (mmfun "&Assign;")))
;;(set-table '(mset  (mmfun "&Assign;")))  ;;; This is not math
;;(set-table '(marrow  (mmfun "&RightArrow;")))
;;(set-table '(mrarrow  (mmfun "&RightArrow;")))
;;(set-table '(%at (mct-proc mPr-at)))
;;(set-table '($at (mct-proc mPr-at)))
;;(set-table '($det (mct-proc mPr-det)))
;;(set-table '(%determinant (mct-proc det)))
;;(set-table '($binomial (mct-proc binomial)))
;;(set-table '(%binomial (mct-proc binomial)))

(set-table '(%sum (mct-proc sumprod)(mmfun "sum/")))
;;(set-table '($sum (mct-proc sumprod)(mmfun "sum/")))
;;(set-table '($product (mct-proc sumprod)(mmfun "product/")))
(set-table '(%product (mct-proc sumprod)(mmfun "product/")))
;;(set-table '($integrate (mct-proc mctintegrate)(mmfun "int/")))
(set-table '(%integrate (mct-proc mctintegrate)(mmfun "int/")))
(set-table '($diff (mct-proc mctdiff)(mmfun "diff/")))
;;(set-table '(%derivative (mct-proc mctdiff)(mmfun "diff/")))
(set-table '($limit (mct-proc mctlimit)(mmfun "limit/")))
;;(set-table '(%limit (mct-proc mctlimit)(mmfun "limit/")))

;;(set-table '(mprog (mmfun "block")))
;;(set-table '($block (mmfun "block")))
;;(set-table '($$boldif (mmfun "if/")))
;;(set-table '($$boldthen (mmfun "then/")))
;;(set-table '($$boldelse (mmfun "else/")))

