;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;

(in-package :maxima)

;; TeX-printing
;; (c) copyright 1987, Richard J. Fateman
;; small corrections and additions: Andrey Grozin, 2001
;; additional additions: Judah Milgram (JM), September 2001
;; additional corrections: Barton Willis (BLW), October 2001

;; Usage: tex(d8,"/tmp/foo.tex"); tex(d10,"/tmp/foo.tex"); ..
;; to append lines d8 and d10 to the tex file.  If given only
;; one argument the result goes to standard output.

;; Extract from permission letter to wfs:
;; Date: Sat, 2 Apr 88 18:06:16 PST
;; From: fateman%vangogh.Berkeley.EDU@ucbvax.Berkeley.EDU (Richard Fateman)
;; To: wfs@rascal.ics.UTEXAS.EDU
;; Subject: about tex...
;; You have my permission to put it in NESC or give it to anyone
;; else who might be interested in it....

;; source language:
;; There are changes by wfs to allow use inside MAXIMA which runs
;; in COMMON LISP.  For original FRANZ LISP version contact rfw.

;; intended environment: vaxima (Vax or Sun). Parser should be
;; equivalent (in lbp/rbp data) to 1986 NESC Vaxima.
;;;(provide 'tex)
;;;(in-package 'tex)
;;;(export '($tex $texinit))
;;;;; we'd like to just
;;;(import '(user::$bothcases user::lbp user::rbp user::nformat))
;;;(use-package 'user)

;; March, 1987

;; Method:

;; Producing TeX from a macsyma internal expression is done by
;; a reversal of the parsing process.  Fundamentally, a
;; traversal of the expression tree is produced by the tex programs,
;; with appropriate substitutions and recognition of the
;; infix / prefix / postfix / matchfix relations on symbols. Various
;; changes are made to this so that TeX will like the results.
;; It is important to understand the binding powers of the operators
;; in Macsyma, in mathematics, and in TeX so that parentheses will
;; be inserted when necessary. Because TeX has different kinds of
;; groupings (e.g. in superscripts, within sqrts), not all
;; parentheses are explicitly need.

;;  Instructions:
;; in macsyma, type tex(<expression>);  or tex(<label>); or
;; tex(<expr-or-label>, <file-name>);  In the case of a label,
;; a left-equation-number will be produced.
;; in case a file-name is supplied, the output will be sent
;; (perhaps appended) to that file.

(declare-top (special lop rop $labels $inchar))

(defvar *tex-environment-default* '("$$" . "$$"))

(defun $set_tex_environment_default (env-open env-close)
  (setq env-open ($sconcat env-open))
  (setq env-close ($sconcat env-close))
  (setq *tex-environment-default* `(,env-open . ,env-close))
  ($get_tex_environment_default))

(defun $get_tex_environment_default ()
  `((mlist) ,(car *tex-environment-default*) ,(cdr *tex-environment-default*)))

(defun $set_tex_environment (x env-open env-close)
  (setq env-open ($sconcat env-open))
  (setq env-close ($sconcat env-close))
  (if (getopr x) (setq x (getopr x)))
  (setf (get x 'tex-environment) `(,env-open . ,env-close))
  ($get_tex_environment x))

(defun $get_tex_environment (x)
  (if (getopr x) (setq x (getopr x)))
  (let ((e (get-tex-environment x)))
    `((mlist) ,(car e) ,(cdr e))))

(defun get-tex-environment (x)
  (cond
    ((symbolp x)
     (or (get x 'tex-environment) *tex-environment-default*))
    ((atom x)
     *tex-environment-default*)
    (t
      (get-tex-environment (caar x)))))

(setf (get 'mdefine 'tex-environment)
      `(,(format nil "~%\\begin{verbatim}~%") . ,(format nil ";~%\\end{verbatim}~%")))

(setf (get 'mdefmacro 'tex-environment)
      `(,(format nil "~%\\begin{verbatim}~%") . ,(format nil ";~%\\end{verbatim}~%")))

(setf (get 'mlabel 'tex-environment)
      `(,(format nil "~%\\begin{verbatim}~%") . ,(format nil ";~%\\end{verbatim}~%")))

;; top level command the result of tex'ing the expression x.
;; Lots of messing around here to get C-labels verbatim printed
;; and function definitions verbatim "ground"

(defmspec $tex(l) ;; mexplabel, and optional filename or stream
  ;;if filename or stream supplied but 'nil' then return a string
  (let ((args (cdr l)))
    (cond ((and (cdr args) (null (cadr args)))
	   (let ((*standard-output* (make-string-output-stream)))
	     (apply 'tex1  args)
	     (get-output-stream-string *standard-output*)
	     )
	   )
	  (t (apply 'tex1  args)))))

(defun quote-% (sym)
  (let* ((strsym (string sym))
         (pos (position-if #'(lambda (c) (find c "$%&_")) strsym)))
    (if pos
      (concatenate 'string (subseq strsym 0 pos) "\\" (subseq strsym pos (1+ pos))
                           (quote-% (subseq strsym (1+ pos))))
      strsym)))

(defun tex1 (mexplabel &optional filename-or-stream) ;; mexplabel, and optional filename or stream
  (prog (mexp  texport x y itsalabel need-to-close-texport)
     (reset-ccol)
     (cond ((null mexplabel)
	    (displa " No eqn given to TeX")
	    (return nil)))
     ;; collect the file-name, if any, and open a port if needed
     (setq filename-or-stream (meval filename-or-stream))
     (setq texport
       (cond
         ((null filename-or-stream) *standard-output*)
         ((eq filename-or-stream t) *standard-output*)
         ((streamp filename-or-stream) filename-or-stream)
         (t
           (setq need-to-close-texport t)
           (open (namestring (maxima-string filename-or-stream))
                 :direction :output
                 :if-exists :append
                 :if-does-not-exist :create))))
     ;; go back and analyze the first arg more thoroughly now.
     ;; do a normal evaluation of the expression in macsyma
     (setq mexp (meval mexplabel))
     (cond ((member mexplabel $labels :test #'eq)	; leave it if it is a label
	    (setq mexplabel (concatenate 'string "(" (print-invert-case (stripdollar mexplabel))
					 ")"))
	    (setq itsalabel t))
	   (t (setq mexplabel nil)))	;flush it otherwise

     ;; maybe it is a function?
     (cond((symbolp (setq x mexp)) ;;exclude strings, numbers
	   (setq x ($verbify x))
	   (cond ((setq y (mget x 'mexpr))
		  (setq mexp (list '(mdefine) (cons (list x) (cdadr y)) (caddr y))))
		 ((setq y (mget x 'mmacro))
		  (setq mexp (list '(mdefmacro) (cons (list x) (cdadr y)) (caddr y))))
		 ((setq y (mget x 'aexpr))
		  (setq mexp (list '(mdefine) (cons (list x 'array) (cdadr y)) (caddr y)))))))
     (cond ((and (null(atom mexp))
		 (member (caar mexp) '(mdefine mdefmacro) :test #'eq))
	    (format texport (car (get-tex-environment (caar mexp))))
	    (cond (mexplabel (format texport "~a " mexplabel)))
	    (mgrind mexp texport)	;write expression as string
	    (format texport (cdr (get-tex-environment (caar mexp)))))
	   ((and
	     itsalabel ;; but is it a user-command-label?
         ;; THE FOLLOWING TESTS SEEM PRETTY STRANGE --
         ;; WHY CHECK INITIAL SUBSTRING IF SYMBOL IS ON THE $LABELS LIST ??
         ;; PROBABLY IT IS A HOLDOVER FROM THE DAYS WHEN LABELS WERE C AND D INSTEAD OF %I AND %O
	     (<= (length (string $inchar)) (length (string mexplabel)))
	     (string= (subseq (maybe-invert-string-case (string $inchar)) 1 (length (string $inchar)))
		      (subseq (string mexplabel) 1 (length (string $inchar))))
	     ;; Check to make sure it isn't an outchar in disguise
	     (not
	      (and
	       (<= (length (string $outchar)) (length (string mexplabel)))
	       (string= (subseq (maybe-invert-string-case (string $outchar)) 1 (length (string $outchar)))
			(subseq (string mexplabel) 1 (length (string $outchar)))))))
	    ;; aha, this is a C-line: do the grinding:
	    (format texport (car (get-tex-environment 'mlabel)))
        (format texport "~a" mexplabel)
	    (mgrind mexp texport)	;write expression as string
	    (format texport (cdr (get-tex-environment 'mlabel))))
	   (t 
	    (if mexplabel (setq mexplabel (quote-% mexplabel)))
					; display the expression for TeX now:
        (myprinc (car (get-tex-environment mexp)) texport)
	    (mapc #'(lambda (x) (myprinc x texport))
		  ;;initially the left and right contexts are
		  ;; empty lists, and there are implicit parens
		  ;; around the whole expression
		  (tex mexp nil nil 'mparen 'mparen))
	    (cond (mexplabel
		   (format texport "\\leqno{\\tt ~a}" mexplabel)))
	    (format texport (cdr (get-tex-environment mexp)))))
     (terpri texport)
     (if need-to-close-texport
	    (close texport))
     (return mexplabel)))

;;; myprinc is an intelligent low level printing routine.  it keeps track of
;;; the size of the output for purposes of allowing the TeX file to
;;; have a reasonable line-line. myprinc will break it at a space
;;; once it crosses a threshold.
;;; this has nothign to do with breaking the resulting equations.

;;-      arg:    chstr -  string or number to princ
;;-      scheme: This function keeps track of the current location
;;-              on the line of the cursor and makes sure
;;-              that a value is all printed on one line (and not divided
;;-              by the crazy top level os routines)

(let ((ccol 1))
  (defun reset-ccol () (setq ccol 1))

  (defun myprinc (chstr texport)
    (prog (chlst)
       (cond ((and (> (+ (length (setq chlst (exploden chstr))) ccol) 70.)
                   (or (stringp chstr) (equal chstr '| |)))
	      (terpri texport)      ;would have exceeded the line length
	      (setq ccol 1.)
	      (myprinc " " texport))) ; lead off with a space for safetyso we split it up.			
       (do ((ch chlst (cdr ch))
	    (colc ccol (1+ colc)))
	   ((null ch) (setq ccol colc))
         (write-char (car ch) texport)))))

(defun tex (x l r lop rop)
  ;; x is the expression of interest; l is the list of strings to its
  ;; left, r to its right. lop and rop are the operators on the left
  ;; and right of x in the tree, and will determine if parens must
  ;; be inserted
  (setq x (nformat x))
  (cond ((atom x) (tex-atom x l r))
	((or (<= (tex-lbp (caar x)) (tex-rbp lop)) (> (tex-lbp rop) (tex-rbp (caar x))))
	 (tex-paren x l r))
	;; special check needed because macsyma notates arrays peculiarly
	((member 'array (cdar x) :test #'eq) (tex-array x l r))
	;; dispatch for object-oriented tex-ifiying
	((get (caar x) 'tex) (funcall (get (caar x) 'tex) x l r))
	(t (tex-function x l r nil))))

(defun tex-atom (x l r)	;; atoms: note: can we lose by leaving out {}s ?
  (append l
	  (list (cond ((numberp x) (texnumformat x))
		      ((and (symbolp x) (or (get x 'texword) (get (get x 'reversealias) 'texword))))
                      ((stringp x)
                       (tex-string (quote-% (if $stringdisp (concatenate 'string "``" x "''") x))))
                      ((characterp x) (tex-char x))
		      (t (tex-stripdollar (or (get x 'reversealias) x)))))
	  r))

(defun tex-string (x)
  (cond ((equal x "") "")
	((eql (elt x 0) #\\) x)
	(t (concatenate 'string "\\mbox{{}" x "{}}"))))

(defun tex-char (x)
  (if (eql x #\|) "\\mbox{\\verb/|/}"
      (concatenate 'string "\\mbox{\\verb|" (string x) "|}")))

(defvar *tex-translations* nil)
;; '(("ab" . "a")("x" . "x")) would cause  AB12 and X3 C4 to print a_{12} and x_3 C_4

;; Read forms from file F1 and output them to F2
(defun tex-forms (f1 f2 &aux tem (eof *mread-eof-obj*))
  (with-open-file (st f1)
    (loop while (not (eq (setq tem (mread-raw st eof)) eof))
	   do (tex1 (third tem) f2))))

(defun tex-stripdollar (x)
  (let ((s (maybe-invert-string-case (symbol-name (tex-stripdollar0 x)))))
    (if (> (length s) 1)
      (concatenate 'string "{\\it " s "}")
      s)))

(defun tex-stripdollar0 (sym &aux )
  (or (symbolp sym) (return-from tex-stripdollar0  sym))
  (let* ((pname (quote-% (stripdollar sym)))
	 (l (length pname))
	 (begin-sub
	  (loop for i downfrom (1- l)
		 when (not (digit-char-p (aref pname i)))
		 do (return (1+ i))))
	 (tem  (make-array (+ l 4) :element-type ' #.(array-element-type "abc") :fill-pointer 0)))
    (loop for i below l
	   do
	   (cond ((eql i begin-sub)
		  (let ((a (assoc tem  *tex-translations* :test 'equal)))
		    (cond (a
			   (setq a (cdr a))
			   (setf (fill-pointer tem) 0)
			   (loop for i below (length a)
				  do
				  (vector-push (aref a i) tem)))))
		  (vector-push #\_ tem)
		  (unless (eql i (- l 1))
		    (vector-push #\{ tem)
		    (setq begin-sub t))))
		  (vector-push (aref pname i) tem)
	   finally
	   (cond ((eql begin-sub t)
		  (vector-push #\} tem))))
    (intern tem)))

(defun strcat (&rest args)
  (apply #'concatenate 'string (mapcar #'string args)))

;; 10/14/87 RJF  convert 1.2e20 to 1.2 \cdot 10^{20}
;; 03/30/01 RLT  make that 1.2 \times 10^{20}
(defun texnumformat(atom)
  (let (r firstpart exponent)
    (cond ((integerp atom)
	   (coerce (exploden atom) 'string))
	  (t
	   (setq r (exploden atom))
	   (setq exponent (member 'e r :test #'string-equal)) ;; is it ddd.ddde+EE
	   (cond
         ((null exponent)
		  (coerce r 'string))
		 (t
		  (setq firstpart
			(nreverse (cdr (member 'e (reverse r) :test #'string-equal))))
		  (strcat (apply #'strcat firstpart )
			  " \\times 10^{"
			  (apply #'strcat (cdr exponent))
			  "}")))))))

(defun tex-paren (x l r)
  (tex x (append l '("\\left(")) (cons "\\right)" r) 'mparen 'mparen))

(defun tex-array (x l r)
  (let ((f))
    (if (eq 'mqapply (caar x))
	(setq f (cadr x)
	      x (cdr x)
	      l (tex f (append l (list "\\left(")) (list "\\right)") 'mparen 'mparen))
	(setq f (caar x)
	      l (tex f l nil lop 'mfunction)))
    (setq
     r (nconc (tex-list (cdr x) nil (list "}") ",") r))
    (nconc l (list "_{") r  )))

;; we could patch this so sin x rather than sin(x), but instead we made sin a prefix
;; operator

(defun tex-function (x l r op) op
       (setq l (tex (caar x) l nil 'mparen 'mparen)
	     r (tex (cons '(mprogn) (cdr x)) nil r 'mparen 'mparen))
       (nconc l r))

;; set up a list , separated by symbols (, * ...)  and then tack on the
;; ending item (e.g. "]" or perhaps ")"

(defun tex-list (x l r sym)
  (if (null x) r
      (do ((nl))
	  ((null (cdr x))
	   (setq nl (nconc nl (tex (car x)  l r 'mparen 'mparen)))
	   nl)
	(setq nl (nconc nl (tex (car x)  l (list sym) 'mparen 'mparen))
	      x (cdr x)
	      l nil))))

(defun tex-prefix (x l r)
  (tex (cadr x) (append l (texsym (caar x))) r (caar x) rop))

(defun tex-infix (x l r)
  ;; check for 2 args
  (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))
  (setq l (tex (cadr x) l nil lop (caar x)))
  (tex (caddr x) (append l (texsym (caar x))) r (caar x) rop))

(defun tex-postfix (x l r)
  (tex (cadr x) l (append (texsym (caar x)) r) lop (caar x)))

(defun tex-nary (x l r)
  (let* ((op (caar x)) (sym (texsym op)) (y (cdr x)) (ext-lop lop) (ext-rop rop))
    (cond ((null y)       (tex-function x l r t)) ; this should not happen
          ((null (cdr y)) (tex-function x l r t)) ; this should not happen, too
          (t (do ((nl) (lop ext-lop op) (rop op (if (null (cdr y)) ext-rop op)))
                 ((null (cdr y)) (setq nl (append nl (tex (car y)  l r lop rop))) nl)
	       (setq nl (append nl (tex (car y) l sym lop rop))
		     y (cdr y)
		     l nil))))))

(defun tex-nofix (x l r) (tex (car (texsym (caar x))) l r (caar x) rop))

(defun tex-matchfix (x l r)
  (setq l (append l (car (texsym (caar x))))
    ;; car of texsym of a matchfix operator is the lead op
    r (append (list (nth 1 (texsym (caar x)))) r)
    ;; cdr is the trailing op
    x (tex-list (cdr x) nil r (or (nth 2 (texsym (caar x))) " , ")))
  (append l x))

(defun texsym (x)
  (or (get x 'texsym) (get x 'strsym)
      (get x 'dissym)
      (stripdollar x)))

(defun texword (x)
  (or (get x 'texword)
      (stripdollar x)))

(defprop bigfloat tex-bigfloat tex)

; For 1.2345b678, generate TeX output 1.2345_B \times 10^{678} .
; If the exponent is 0, then ... \times 10^{0} is generated
; (no attempt to strip off zero exponent).

(defun tex-bigfloat (x l r) 
  (let ((formatted (fpformat x)))
    ; There should always be a '|b| or '|B| in the FPFORMAT output.
    ; Play it safe -- check anyway.
    (if (or (find '|b| formatted) (find '|B| formatted))
      (let*
        ((spell-out-expt
           (append
             (apply #'append
                    (mapcar
                     #'(lambda (e) (if (or (eq e '|b|) (eq e '|B|))
                                       '("_B" | | "\\times" | | "10^{")
                                       (list e)))
                      formatted))
             '(|}|))))
        (append l spell-out-expt r))
      (append l formatted r))))

(defprop mprog "\\mathbf{block}\\;" texword)
(defprop %erf "\\mathrm{erf}" texword)
(defprop $erf "\\mathrm{erf}" texword) ;; etc for multicharacter names
(defprop $true  "\\mathbf{true}"  texword)
(defprop $false "\\mathbf{false}" texword)
(defprop $done "\\mathbf{done}" texword)

(defprop mprogn tex-matchfix tex) ;; mprogn is (<progstmnt>, ...)
(defprop mprogn (("\\left(") "\\right)") texsym)

(defprop mlist tex-matchfix tex)
(defprop mlist (("\\left[ ")" \\right] ") texsym)

;;absolute value
(defprop mabs tex-matchfix tex)
(defprop mabs (("\\left| ")"\\right| ") texsym)

(defprop mqapply tex-mqapply tex)

(defun tex-mqapply (x l r)
  (setq l (tex (cadr x) l (list "(" ) lop 'mfunction)
	r (tex-list (cddr x) nil (cons ")" r) ","))
  (append l r))	;; fixed 9/24/87 RJF

(defprop $%i "i" texword)
(defprop $%e "e" texword)
(defprop $inf "\\infty " texword)
(defprop $minf " -\\infty " texword)
(defprop %laplace "\\mathcal{L}" texword)

(defprop $alpha "\\alpha" texword)
(defprop $beta "\\beta" texword)
(defprop $gamma "\\gamma" texword)
(defprop %gamma "\\gamma" texword)

(defprop %gamma tex-gamma tex)
(defun tex-gamma (x l r)
 (tex (cadr x) (append l '("\\Gamma\\left(")) (append '("\\right)") r) 'mparen 'mparen))

(defprop $%gamma "\\gamma" texword)
(defprop $delta "\\delta" texword)
(defprop $epsilon "\\varepsilon" texword)
(defprop $zeta "\\zeta" texword)
(defprop $eta "\\eta" texword)
(defprop $theta "\\vartheta" texword)
(defprop $iota "\\iota" texword)
(defprop $kappa "\\kappa" texword)
(defprop lambda "\\lambda" texword)
(defprop $lambda "\\lambda" texword)
(defprop $mu "\\mu" texword)
(defprop $nu "\\nu" texword)
(defprop $xi "\\xi" texword)
(defprop $omicron " o" texword)
(defprop $%pi "\\pi" texword)
(defprop $pi "\\pi" texword)
(defprop $rho "\\rho" texword)
(defprop $sigma "\\sigma" texword)
(defprop $tau "\\tau" texword)
(defprop $upsilon "\\upsilon" texword)
(defprop $phi "\\varphi" texword)
(defprop $chi "\\chi" texword)
(defprop $psi "\\psi" texword)
(defprop $omega "\\omega" texword)

(defprop |$Alpha| "{\\rm A}" texword)
(defprop |$Beta| "{\\rm B}" texword)
(defprop |$Gamma| "\\Gamma" texword)
(defprop |$Delta| "\\Delta" texword)
(defprop |$Epsilon| "{\\rm E}" texword)
(defprop |$Zeta| "{\\rm Z}" texword)
(defprop |$Eta| "{\\rm H}" texword)
(defprop |$Theta| "\\Theta" texword)
(defprop |$Iota| "{\\rm I}" texword)
(defprop |$Kappa| "{\\rm K}" texword)
(defprop |$Lambda| "\\Lambda" texword)
(defprop |$Mu| "{\\rm M}" texword)
(defprop |$Nu| "{\\rm N}" texword)
(defprop |$Xi| "\\Xi" texword)
(defprop |$Omicron| "{\\rm O}" texword)
(defprop |$Pi| "\\Pi" texword)
(defprop |$Rho| "{\\rm P}" texword)
(defprop |$Sigma| "\\Sigma" texword)
(defprop |$Tau| "{\\rm T}" texword)
(defprop |$Upsilon| "\\Upsilon" texword)
(defprop |$Phi| "\\Phi" texword)
(defprop |$Chi| "{\\rm X}" texword)
(defprop |$Psi| "\\Psi" texword)
(defprop |$Omega| "\\Omega" texword)

(defprop mquote tex-prefix tex)
(defprop mquote ("\\mbox{{}'{}}") texsym)

(defprop msetq tex-infix tex)
(defprop msetq (":") texsym)

(defprop mset tex-infix tex)
(defprop mset ("::") texsym)

(defprop mdefine tex-infix tex)
(defprop mdefine (":=") texsym)

(defprop mdefmacro tex-infix tex)
(defprop mdefmacro ("::=") texsym)

(defprop marrow tex-infix tex)
(defprop marrow ("\\rightarrow ") texsym)

(defprop mfactorial tex-postfix tex)
(defprop mfactorial ("!") texsym)

(defprop mexpt tex-mexpt tex)

(defprop %sum 110. tex-rbp) ;; added by BLW, 1 Oct 2001
(defprop %product 115. tex-rbp)	;; added by BLW, 1 Oct 2001

;; If the number contains a exponent marker when printed, we need to
;; put parens around it.
(defun numneedsparen (number)
  (unless (integerp number)
    (let ((r (exploden number)))
      (member 'e r :test #'string-equal))))

;; insert left-angle-brackets for mncexpt. a^<n> is how a^^n looks.
(defun tex-mexpt (x l r)
  (let((nc (eq (caar x) 'mncexpt)))	; true if a^^b rather than a^b
    ;; here is where we have to check for f(x)^b to be displayed
    ;; as f^b(x), as is the case for sin(x)^2 .
    ;; which should be sin^2 x rather than (sin x)^2 or (sin(x))^2.
    ;; yet we must not display (a+b)^2 as +^2(a,b)...
    ;; or (sin(x))^(-1) as sin^(-1)x, which would be arcsine x
    (cond ;; this whole clause
      ;; should be deleted if this hack is unwanted and/or the
      ;; time it takes is of concern.
      ;; it shouldn't be too expensive.
      ((and (eq (caar x) 'mexpt)      ; don't do this hack for mncexpt
	    (let*
		((fx (cadr x))		; this is f(x)
		 (f (and (not (atom fx)) (atom (caar fx)) (caar fx))) ; this is f [or nil]
		 (bascdr (and f (cdr fx))) ; this is (x) [maybe (x,y..), or nil]
		 (expon (caddr x)) ;; this is the exponent
		 (doit (and
			f		; there is such a function
			(member (getcharn f 1) '(#\% #\$)) ;; insist it is a % or $ function
            (not (member 'array (cdar fx) :test #'eq))	; fix for x[i]^2
					; Jesper Harder <harder@ifa.au.dk>
			(not (member f '(%sum %product %derivative %integrate %at
					      %lsum %limit $pderivop) :test #'eq)) ;; what else? what a hack...
			(or (and (atom expon) (not (numberp expon))) ; f(x)^y is ok
			    (and (atom expon) (numberp expon) (> expon 0))))))
					; f(x)^3 is ok, but not f(x)^-1, which could
					; inverse of f, if written f^-1 x
					; what else? f(x)^(1/2) is sqrt(f(x)), ??
	      (cond (doit
		     (setq l (tex `((mexpt) ,f ,expon) l nil 'mparen 'mparen))
		     (if (and (null (cdr bascdr))
			      (eq (get f 'tex) 'tex-prefix))
			 (setq r (tex (car bascdr) nil r f 'mparen))
			 (setq r (tex (cons '(mprogn) bascdr) nil r 'mparen 'mparen))))
		    (t nil)))))		; won't doit. fall through
      (t (setq l (cond ((or ($bfloatp (cadr x)) (and (numberp (cadr x))
			     (numneedsparen (cadr x))))
            ; ACTUALLY THIS TREATMENT IS NEEDED WHENEVER (CAAR X) HAS GREATER BINDING POWER THAN MTIMES ...
			(tex (cadr x) (append l '("\\left(")) '("\\right)") lop
			     (caar x)))
		       (t (tex (cadr x) l nil lop (caar x))))
	       r (if (mmminusp (setq x (nformat (caddr x))))
		     ;; the change in base-line makes parens unnecessary
		     (if nc
			 (tex (cadr x) '("^ {-\\langle ")(cons "\\rangle }" r) 'mparen 'mparen)
             (tex (cadr x) '("^ {- ") (cons " }" r) 'mminus 'mparen))
		     (if nc
			 (tex x (list "^{\\langle ")(cons "\\rangle}" r) 'mparen 'mparen)
			 (if (and (integerp x) (< x 10))
			     (tex x (list "^")(cons "" r) 'mparen 'mparen)
			     (tex x (list "^{")(cons "}" r) 'mparen 'mparen))
			 )))))
    (append l r)))

(defprop mncexpt tex-mexpt tex)

(defprop mnctimes tex-nary tex)
(defprop mnctimes ("\\cdot ") texsym)

(defprop mtimes tex-nary tex)
(defprop mtimes ("\\,") texsym)

(defprop %sqrt tex-sqrt tex)

(defun tex-sqrt(x l r)
  ;; format as \\sqrt { } assuming implicit parens for sqr grouping
  (tex (cadr x) (append l  '("\\sqrt{")) (append '("}") r) 'mparen 'mparen))

;; macsyma doesn't know about cube (or nth) roots,
;; but if it did, this is what it would look like.
(defprop $cubrt tex-cubrt tex)

(defun tex-cubrt (x l r)
  (tex (cadr x) (append l  '("\\root 3 \\of{")) (append '("}") r) 'mparen 'mparen))

(defprop mquotient tex-mquotient tex)
(defprop mquotient ("\\over") texsym)

(defun tex-mquotient (x l r)
  (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))
  (setq l (tex (cadr x) (append l '("{{")) nil 'mparen 'mparen)
					;the divide bar groups things
	r (tex (caddr x) (list "}\\over{") (append '("}}")r) 'mparen 'mparen))
  (append l r))

(defprop $matrix tex-matrix tex)

(defun tex-matrix(x l r) ;;matrix looks like ((mmatrix)((mlist) a b) ...)
  (append l `("\\pmatrix{")
	  (mapcan #'(lambda(y)
		      (tex-list (cdr y) nil (list "\\cr ") "&"))
		  (cdr x))
	  '("}") r))

;; macsyma sum or prod is over integer range, not  low <= index <= high
;; TeX is lots more flexible .. but

(defprop %sum tex-sum tex)
(defprop %lsum tex-lsum tex)
(defprop %product tex-sum tex)

;; easily extended to union, intersect, otherops

(defun tex-lsum(x l r)
  (let ((op (cond ((eq (caar x) '%lsum) "\\sum_{")
		  ;; extend here
		  ))
	;; gotta be one of those above
	;; 4th arg of tex is changed from mparen to (caar x)
	;; to reflect the operator preceedance correctly.
	;; This change improves the how to put paren.
	(s1 (tex (cadr x) nil nil (caar x) rop))	;; summand
	(index ;; "index = lowerlimit"
	 (tex `((min simp) , (caddr x), (cadddr x))  nil nil 'mparen 'mparen)))
    (append l `( ,op ,@index "}}{" ,@s1 "}") r)))

(defun tex-sum(x l r)
  (let ((op (cond ((eq (caar x) '%sum) "\\sum_{")
		  ((eq (caar x) '%product) "\\prod_{")
		  ;; extend here
		  ))
	;; gotta be one of those above
	;; 4th arg of tex is changed from mparen to (caar x)
	;; to reflect the operator preceedance correctly.
	;; This change improves the how to put paren.
	(s1 (tex (cadr x) nil nil (caar x) rop))	;; summand
	(index ;; "index = lowerlimit"
	 (tex `((mequal simp) ,(caddr x),(cadddr x)) nil nil 'mparen 'mparen))
	(toplim (tex (car(cddddr x)) nil nil 'mparen 'mparen)))
    (append l `( ,op ,@index "}^{" ,@toplim "}{" ,@s1 "}") r)))

(defprop %integrate tex-int tex)
(defun tex-int (x l r)
  (let ((s1 (tex (cadr x) nil nil 'mparen 'mparen)) ;;integrand delims / & d
	(var (tex (caddr x) nil nil 'mparen rop))) ;; variable
    (cond((= (length x) 3)
	  (append l `("\\int {" ,@s1 "}{\\;d" ,@var "}") r))
	 (t ;; presumably length 5
	  (let ((low (tex (nth 3 x) nil nil 'mparen 'mparen))
		;; 1st item is 0
		(hi (tex (nth 4 x) nil nil 'mparen 'mparen)))
	    (append l `("\\int_{" ,@low "}^{" ,@hi "}{" ,@s1 "\\;d" ,@var "}") r))))))

(defprop %limit tex-limit tex)

(defun tex-limit (x l r)
  (let*
     ;; limit function
    ((s1 (tex (cadr x) nil nil 'mparen rop))
     (direction (fifth x))
     ;; the thing underneath "limit"
     (subfun
       (subst (or (and (eq direction '$plus) "\\downarrow ")
                  (and (eq direction '$minus) "\\uparrow ")
                  "\\rightarrow ")
              '=
              (tex `((mequal simp) ,(caddr x),(cadddr x))
                   nil nil 'mparen 'mparen))))
    (append l `("\\lim_{" ,@subfun "}{" ,@s1 "}") r)))

(defprop %at tex-at tex)

;; e.g.  at(diff(f(x)),x=a)
(defun tex-at (x l r)
  (let ((s1 (tex (cadr x) nil nil lop rop))
	(sub (tex (caddr x) nil nil 'mparen 'mparen)))
    (append l '("\\left.") s1  '("\\right|_{") sub '("}") r)))

(defprop mbox tex-mbox tex)

;; \boxed is defined in amsmath.sty,
;; \newcommand{\boxed}[1]{\fbox{\m@th$\displaystyle#1$}}

(defun tex-mbox (x l r)
  (append l '("\\boxed{") (tex (cadr x) nil nil 'mparen 'mparen) '("}") r))

(defprop mlabox tex-mlabox tex)

(defun tex-mlabox (x l r)
  (append l '("\\stackrel{") (tex (caddr x) nil nil 'mparen 'mparen)
	  '("}{\\boxed{") (tex (cadr x) nil nil 'mparen 'mparen) '("}}") r))

;;binomial coefficients

(defprop %binomial tex-choose tex)

(defun tex-choose (x l r)
  (append l
          '("{{")
          (tex (cadr x) nil nil 'mparen 'mparen)
          '("}\\choose{")
          (tex (caddr x) nil nil 'mparen 'mparen)
          '("}}")
          r))

(defprop rat tex-rat tex)
(defun tex-rat(x l r) (tex-mquotient x l r))

(defprop mplus tex-mplus tex)

(defun tex-mplus (x l r)
					;(declare (fixnum w))
  (cond ((member 'trunc (car x) :test #'eq) (setq r (cons "+\\cdots " r))))
  (cond ((null (cddr x))
	 (if (null (cdr x))
	     (tex-function x l r t)
	     (tex (cadr x) (cons "+" l) r 'mplus rop)))
	(t (setq l (tex (cadr x) l nil lop 'mplus)
		 x (cddr x))
	   (do ((nl l)  (dissym))
	       ((null (cdr x))
		(if (mmminusp (car x)) (setq l (cadar x) dissym (list "-"))
		    (setq l (car x) dissym (list "+")))
		(setq r (tex l dissym r 'mplus rop))
		(append nl r))
	     (if (mmminusp (car x)) (setq l (cadar x) dissym (list "-"))
		 (setq l (car x) dissym (list "+")))
	     (setq nl (append nl (tex l dissym nil 'mplus 'mplus))
		   x (cdr x))))))

(defprop mminus tex-prefix tex)
(defprop mminus ("-") texsym)

;; MIN = "Maxima in", apparently -- not to be confused with the least value of a set.
;; MIN is not known to the parser, although it seems stuff like "x in S" could make use of MIN.

(defprop min tex-infix tex)
(defprop min ("\\in{") texsym)
(defprop min 80. tex-lbp)
(defprop min 80. tex-rbp)

(defprop mequal tex-infix tex)
(defprop mequal (=) texsym)

(defprop mnotequal tex-infix tex)
(defprop mnotequal ("\\neq ") texsym)

(defprop mgreaterp tex-infix tex)
(defprop mgreaterp (>) texsym)

(defprop mgeqp tex-infix tex)
(defprop mgeqp ("\\geq ") texsym)

(defprop mlessp tex-infix tex)
(defprop mlessp (<) texsym)

(defprop mleqp tex-infix tex)
(defprop mleqp ("\\leq ") texsym)

(defprop mnot tex-prefix tex)
(defprop mnot ("\\neg ") texsym)

(defprop mand tex-nary tex)
(defprop mand ("\\land ") texsym)

(defprop mor tex-nary tex)
(defprop mor ("\\lor ") texsym)

;; make sin(x) display as sin x , but sin(x+y) as sin(x+y)
;; etc

(defun tex-setup (x)
  (let((a (car x))
       (b (cadr x)))
    (setf (get a 'tex) 'tex-prefix)
    (setf (get a 'texword) b)	;This means "sin" will always be roman
    (setf (get a 'texsym) (list b))
    (setf (get a 'tex-rbp) 130)))


;; I WONDER IF ALL BUILT-IN FUNCTIONS SHOULD BE SET IN ROMAN TYPE
(defprop $atan2 "{\\rm atan2}" texword)

;; JM 09/01 expand and re-order to follow table of "log-like" functions,
;; see table in Lamport, 2nd edition, 1994, p. 44, table 3.9.
;; I don't know if these are Latex-specific so you may have to define
;; them if you use plain Tex.

(mapc #'tex-setup
      '(
	(%acos "\\arccos ")
	(%asin "\\arcsin ")
	(%atan "\\arctan ")

					; Latex's arg(x) is ... ?
	(%cos "\\cos ")
	(%cosh "\\cosh ")
	(%cot "\\cot ")
	(%coth "\\coth ")
	(%csc "\\csc ")
					; Latex's "deg" is ... ?
	(%determinant "\\det ")
	(%dim "\\dim ")
	(%exp "\\exp ")
	(%gcd "\\gcd ")
					; Latex's "hom" is ... ?
	(%inf "\\inf ")		   ; many will prefer "\\infty". Hmmm.
					; Latex's "ker" is ... ?
					; Latex's "lg" is ... ?
					; lim is handled by tex-limit.
					; Latex's "liminf" ... ?
					; Latex's "limsup" ... ?
	(%ln "\\ln ")
	(%log "\\log ")
	(%max "\\max ")
	(%min "\\min ")
					; Latex's "Pr" ... ?
	(%sec "\\sec ")
	(%sin "\\sin ")
	(%sinh "\\sinh ")
					; Latex's "sup" ... ?
	(%tan "\\tan ")
	(%tanh "\\tanh ")
	;; (%erf "{\\rm erf}") this would tend to set erf(x) as erf x. Unusual
					;(%laplace "{\\cal L}")

    ; Maxima built-in functions which do not have corresponding TeX symbols.

    (%asec "{\\rm arcsec}\\; ")
    (%acsc "{\\rm arccsc}\\; ")
    (%acot "{\\rm arccot}\\; ")

    (%sech "{\\rm sech}\\; ")
    (%csch "{\\rm csch}\\; ")
    
    (%asinh "{\\rm asinh}\\; ")
    (%acosh "{\\rm acosh}\\; ")
    (%atanh "{\\rm atanh}\\; ")

    (%asech "{\\rm asech}\\; ")
    (%acsch "{\\rm acsch}\\; ")
    (%acoth "{\\rm acoth}\\; ")

	)) ;; etc

(defprop mcond tex-mcond tex)
(defprop %mcond tex-mcond tex)

(defprop %derivative tex-derivative tex)
(defun tex-derivative (x l r)
  (tex (if $derivabbrev
	   (tex-dabbrev x)
	   (tex-d x '$d)) l r lop rop ))

(defun tex-d(x dsym)		    ;dsym should be $d or "$\\partial"
  ;; format the macsyma derivative form so it looks
  ;; sort of like a quotient times the deriva-dand.
  (let*
      ((arg (cadr x)) ;; the function being differentiated
       (difflist (cddr x)) ;; list of derivs e.g. (x 1 y 2)
       (ords (odds difflist 0))	;; e.g. (1 2)
       (vars (odds difflist 1))	;; e.g. (x y)
       (numer `((mexpt) ,dsym ((mplus) ,@ords))) ; d^n numerator
       (denom (cons '(mtimes)
		    (mapcan #'(lambda(b e)
				`(,dsym ,(simplifya `((mexpt) ,b ,e) nil)))
			    vars ords))))
    `((mtimes)
      ((mquotient) ,(simplifya numer nil) ,denom)
      ,arg)))

(defun tex-dabbrev (x)
  ;; Format diff(f,x,1,y,1) so that it looks like
  ;; f
  ;;  x y
  (let*
      ((arg (cadr x)) ;; the function being differentiated
       (difflist (cddr x)) ;; list of derivs e.g. (x 1 y 2)
       (ords (odds difflist 0))	;; e.g. (1 2)
       (vars (odds difflist 1))) ;; e.g. (x y)
    (append
     (if (symbolp arg)
	 `((,arg array))
	 `((mqapply array) ,arg))
     (if (and (= (length vars) 1)
	      (= (car ords) 1))
	 vars
	 `(((mtimes) ,@(mapcan #'(lambda (var ord)
				   (make-list ord :initial-element var))
			       vars ords)))))))

(defun odds(n c)
  ;; if c=1, get the odd terms  (first, third...)
  (cond ((null n) nil)
	((= c 1)(cons (car n)(odds (cdr n) 0)))
	((= c 0)(odds (cdr n) 1))))

;;
;; The format of MCOND expressions is documented above the definition
;; of DIM-MCOND in displa.lisp.  Here are some examples:
;;
;;   ((%mcond) $a $b t nil)         <==>  'if a then b
;;   ((%mcond) $a $b t $d)          <==>  'if a then b else d
;;   ((%mcond) $a $b $c nil t nil)  <==>  'if a then b elseif c then false
;;   ((%mcond) $a $b $c $d t nil)   <==>  'if a then b elseif c then d
;;   ((%mcond) $a $b $c $d t $f)    <==>  'if a then b elseif c then d else f
;; 
;; Note that DIM-MCOND omits display of the final "else" in three
;; cases illustrated below, so we do the same here:
;; 
;;   ((%mcond) $a $b $c $d t $false)  <==>  '(if a then b elseif c then d)
;;   ((%mcond) $a $b $c $d t nil)     <==>   'if a then b elseif c then d
;;   ((%mcond) $a $b $c $d)            ==>   'if a then b elseif c then d
;;
;; The first two cases occur in practice, as can be seen by evaluating
;; ?print('(if a then b)) and ?print(if a then b).  The parser
;; produces the first case, which is transformed into the second case
;; during evaluation.  The third case is handled equivalently by the
;; evaluator and DIM-MCOND, and might plausibly be created by some
;; code, so we handle it here as well.
;;
;; The use of '$false (instead of nil) may be a hack that is no longer
;; needed.  For more information on this, search for $false in
;; PARSE-CONDITION of nparse.lisp and DIM-MCOND of displa.lisp.  Also
;; see the mailing list thread with subject "Bugs in tex-mcond" which
;; took place in January 2011.  -MHW
;;
(defun tex-mcond (x l r)
  (labels
      ((recurse (x l)
	 (append
	  (tex (car x) l '("\\;\\mathbf{then}\\;") 'mparen 'mparen)
	  (cond ((member (cddr x) '(() (t nil) (t $false)) :test #'equal)
		 (tex (second x) nil r 'mcond rop))
		((and (eq (third x) t) (null (nthcdr 4 x)))
		 (append
		  (tex (second x) nil nil 'mparen 'mparen)
		  (tex (fourth x) '("\\;\\mathbf{else}\\;") r 'mcond rop)))
		(t (append
		    (tex (second x) nil nil 'mparen 'mparen)
		    (recurse (cddr x) '("\\;\\mathbf{elseif}\\;"))))))))
  (append l (recurse (cdr x) '("\\mathbf{if}\\;")))))

(defprop mdo tex-mdo tex)
(defprop mdoin tex-mdoin tex)

(defprop %mdo tex-mdo tex)
(defprop %mdoin tex-mdoin tex)

(defun tex-lbp(x)(cond((get x 'tex-lbp))(t(lbp x))))
(defun tex-rbp(x)(cond((get x 'tex-rbp))(t(rbp x))))

;; these aren't quite right

(defun tex-mdo (x l r)
  (tex-list (texmdo x) l r "\\;"))

(defun tex-mdoin (x l r)
  (tex-list (texmdoin x) l r "\\;"))

(defun texmdo (x)
  (nconc (cond ((second x) `("\\mathbf{for}" ,(second x))))
	 (cond ((equal 1 (third x)) nil)
	       ((third x)  `("\\mathbf{from}" ,(third x))))
	 (cond ((equal 1 (fourth x)) nil)
	       ((fourth x) `("\\mathbf{step}" ,(fourth x)))
	       ((fifth x)  `("\\mathbf{next}" ,(fifth x))))
	 (cond ((sixth x)  `("\\mathbf{thru}" ,(sixth x))))
	 (cond ((null (seventh x)) nil)
	       ((eq 'mnot (caar (seventh x)))
		`("\\mathbf{while}" ,(cadr (seventh x))))
	       (t `("\\mathbf{unless}" ,(seventh x))))
	 `("\\mathbf{do}" ,(eighth x))))

(defun texmdoin (x)
  (nconc `("\\mathbf{for}" ,(second x) "\\mathbf{in}" ,(third x))
	 (cond ((sixth x) `("\\mathbf{thru}" ,(sixth x))))
	 (cond ((null (seventh x)) nil)
	       ((eq 'mnot (caar (seventh x)))
		`("\\mathbf{while}" ,(cadr (seventh x))))
	       (t `("\\mathbf{unless}" ,(seventh x))))
	 `("\\mathbf{do}" ,(eighth x))))

(defprop mtext tex-mtext tex)
(defprop text-string tex-mtext tex)
(defprop mlabel tex-mlabel tex)
(defprop spaceout tex-spaceout tex)

;; Additions by Marek Rychlik (rychlik@u.arizona.edu)
;; This stuff handles setting of LET rules

(defprop | --> | "\\longrightarrow " texsym)
(defprop #.(intern (format nil " ~A " 'where)) "\\;\\mathbf{where}\\;" texsym)

;; end of additions by Marek Rychlik

(defun tex-try-sym (x)
  (if (symbolp x)
      (let ((tx (get x 'texsym))) (if tx tx x))
      x))

(defun tex-mtext (x l r)
  (tex-list (map 'list #'tex-try-sym (cdr x)) l r ""))

(defun tex-mlabel (x l r)
  (tex (caddr x)
       (append l
	       (if (cadr x)
		   (list (format nil "\\mbox{\\tt\\red(~A) \\black}" (tex-stripdollar (cadr x))))
		   nil))
       r 'mparen 'mparen))

(defun tex-spaceout (x l r)
  (append l (cons (format nil "\\hspace{~dmm}" (* 3 (cadr x))) r)))

;; run some code initialize file before $tex is run
(defun $texinit(file)
(declare (ignore file))
  '$done)

;; this just prints a \\end on the file;  this is something a TeXnician would
;; probably have no trouble spotting, and will generally be unnecessary, since
;; we anticipate almost all use of tex would be involved in inserting this
;; stuff into larger files that would have their own \\end or equivalent.
(defun $texend(filename)
  (with-open-file (st (stripdollar filename)  :direction :output
		      :if-exists :append :if-does-not-exist :create)
    (format st "\\end~%"))
  '$done)

;; Construct a Lisp function and attach it to the TEX property of
;; operator OP. The constructed function calls a Maxima function F
;; to generate TeX output for OP.
;; F must take 1 argument (an expression which has operator OP)
;; and must return a string (the TeX output).

(defun make-maxima-tex-glue (op f)
  (let
    ((glue-f (gensym))
     (f-body `(append l
                      (list
                        (let ((f-x (mfuncall ',f x)))
                          (if (stringp f-x) f-x
                            (merror (intl:gettext "tex: function ~s did not return a string.~%") ($sconcat ',f)))))
                      r)))
    (setf (symbol-function glue-f) (coerce `(lambda (x l r) ,f-body) 'function))
    (setf (get op 'tex) glue-f))
  f)

;; Convenience function to allow user to process expression X
;; and get a string (TeX output for X) in return.

(defun $tex1 (x) (apply #'strcat (tex x nil nil 'mparen 'mparen)))

;; Undone and trickier:
;; handle reserved symbols stuff, just in case someone
;; has a macsyma variable named (yuck!!) \over  or has a name with
;; {} in it.
;; Maybe do some special hacking for standard notations for
;; hypergeometric fns, alternative summation notations  0<=n<=inf, etc.

;;Undone and really pretty hard: line breaking

;;  The texput function was written by Barton Willis.

(defun $texput (e s &optional tx)

  (cond
    ((stringp e)
     (setq e ($verbify e)))
    ((not (symbolp e))
     (merror (intl:gettext "texput: first argument must be a string or a symbol; found: ~M") e)))

  (setq s (if ($listp s) (margs s) (list s)))
  
  (cond
    ((null tx)
     ;; texput was called as texput(op, foo) where foo is a string
     ;; or a symbol; when foo is a string, assign TEXWORD property,
     ;; when foo is a symbol, construct glue function to call
     ;; the Maxima function named by foo.
     (let ((s0 (nth 0 s)))
       (if (stringp s0)
         (putprop e s0 'texword)
         (make-maxima-tex-glue e s0)))) ;; assigns TEX property
	((eq tx '$matchfix)
	 (putprop e 'tex-matchfix 'tex)
	 (cond ((< (length s) 2)
		(merror (intl:gettext "texput: expected a list of two items for matchfix operator.")))
	       ((= (length s) 2)
		(putprop e (list (list (first s)) (second s)) 'texsym))
	       (t
		(putprop e (list (list (first s)) (second s) (third s)) 'texsym)))
	 `((mlist) ,@s))

	((eq tx '$nofix)
	 (putprop e 'tex-nofix 'tex)
	 (putprop e s 'texsym)
	 (car s))

	((eq tx '$prefix)
	 (putprop e 'tex-prefix 'tex)
	 (when (null (get e 'grind))
	   (putprop e 180 'tex-rbp))
	 (putprop e s 'texsym)
	 (car s))
		
	((eq tx '$infix)
	 (putprop e 'tex-infix 'tex)
	 (when (null (get e 'grind))
	   (putprop e 180 'tex-lbp)
	   (putprop e 180 'tex-rbp))
	 (putprop e  s 'texsym)
	 (car s))

	((eq tx '$nary)
	 (putprop e 'tex-nary 'tex)
	 (when (null (get e 'grind))
	   (putprop e 180 'tex-lbp)
	   (putprop e 180 'tex-rbp))
	 (putprop e s 'texsym)
	 (car s))

	((eq tx '$postfix)
	 (putprop e 'tex-postfix 'tex)
	 (when (null (get e 'grind))
	   (putprop e 180 'tex-lbp))
	 (putprop e  s 'texsym)
	 (car s))))
