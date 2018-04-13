(in-package :maxima)
;; MathML-printing
;; Created by David Drysdale (DMD), December 2002/January 2003
;;
;; closely based on the original TeX conversion code in mactex.lisp, 
;; for which the following credits apply:
;;   (c) copyright 1987, Richard J. Fateman
;;   small corrections and additions: Andrey Grozin, 2001
;;   additional additions: Judah Milgram (JM), September 2001
;;   additional corrections: Barton Willis (BLW), October 2001

;; Usage: mathml(d8,"/tmp/foo.xml"); mathml(d10,"/tmp/foo.xml"); ..
;; to append lines d8 and d10 to the mathml file.  If given only
;; one argument the result goes to standard output.

;; Method:

;; Producing MathML from a macsyma internal expression is done by
;; a reversal of the parsing process.  Fundamentally, a
;; traversal of the expression tree is produced by the program,
;; with appropriate substitutions and recognition of the
;; infix / prefix / postfix / matchfix relations on symbols. Various
;; changes are made to this so that MathML will like the results.

;;  Instructions:
;; in macsyma, type mathml(<expression>);  or mathml(<label>); or
;; mathml(<expr-or-label>, <file-name>);  In the case of a label,
;; an equation-number will also be produced.
;; in case a file-name is supplied, the output will be sent
;; (perhaps appended) to that file.

(macsyma-module mathml)

(declare-top (special lop rop ccol $gcprint texport $labels $inchar vaxima-main-dir))

;; top level command the result of converting the expression x.

(defmspec $mathml(l) ;; mexplabel, and optional filename
  ;;if filename supplied but 'nil' then return a string
  (let ((args (cdr l)))
    (cond ((and (cdr args) (null (cadr args)))
	   (let ((*standard-output* (make-string-output-stream)))
	     (apply 'mathml1  args)
	     (get-output-stream-string *standard-output*)
	     )
	   )
	  (t (apply 'mathml1  args)))))

(defun mathml1 (mexplabel &optional filename ) ;; mexplabel, and optional filename
  (prog (mexp  texport $gcprint ccol x y itsalabel tmpport)
	;; $gcprint = nil turns gc messages off
	(setq ccol 1)
	(cond ((null mexplabel)
	       (displa " No eqn given to MathML")
	       (return nil)))
	;; collect the file-name, if any, and open a port if needed
	(setq texport (cond((null filename) *standard-output* ); t= output to terminal
			   (t
			     (open (string (stripdollar filename))
				   :direction :output
				   :if-exists :append
				   :if-does-not-exist :create))))
	;; go back and analyze the first arg more thoroughly now.
	;; do a normal evaluation of the expression in macsyma
	(setq mexp (meval mexplabel))
	(cond ((member mexplabel $labels :test #'eq); leave it if it is a label
	       (setq mexplabel (intern (format nil "(~a)" (stripdollar mexplabel))))
	       (setq itsalabel t))
	      (t (setq mexplabel nil)));flush it otherwise

	;; maybe it is a function?
	(cond((symbolp (setq x mexp)) ;;exclude strings, numbers
	      (setq x ($verbify x))
	      (cond ((setq y (mget x 'mexpr))
		     (setq mexp (list '(mdefine) (cons (list x) (cdadr y)) (caddr y))))
		    ((setq y (mget x 'mmacro))
		     (setq mexp (list '(mdefmacro) (cons (list x) (cdadr y)) (caddr y))))
		    ((setq y (mget x 'aexpr))
		     (setq mexp (list '(mdefine) (cons (list x 'array) (cdadr y)) (caddr y)))))))
	(cond ((and (null (atom mexp))
		    (member (caar mexp) '(mdefine mdefmacro) :test #'eq))
	       (format texport "<pre>~%" ) 
	       (cond (mexplabel (format texport "~a " mexplabel)))
               ;; need to get rid of "<" signs
               (setq tmpport (make-string-output-stream))
               (mgrind mexp tmpport)
               (close tmpport)
               (format texport "~a" 
                       (string-substitute "&lt;" #\< (get-output-stream-string tmpport)))
	       (format texport ";~%</pre>"))

	      ((and
		itsalabel ;; but is it a user-command-label?
		(eq (getcharn $inchar 2) (getcharn mexplabel 2)))
	       ;; aha, this is a C-line: do the grinding:
	       (format texport "<pre>~%~a " mexplabel) 
               ;; need to get rid of "<" signs
               (setq tmpport (make-string-output-stream))
               (mgrind mexp tmpport)
               (close tmpport)
               (format texport "~a" 
                       (string-substitute "&lt;" #\< (get-output-stream-string tmpport)))
	       (format texport ";~%</pre>"))

	      (t ; display the expression for MathML now:
		 (myprinc "<math xmlns=\"http://www.w3.org/1998/Math/MathML\"> " texport)
		 (mapc #'(lambda (x) (myprinc x texport))
		       ;;initially the left and right contexts are
		       ;; empty lists, and there are implicit parens
		       ;; around the whole expression
		       (mathml mexp nil nil 'mparen 'mparen))
		 (cond (mexplabel
			(format texport "<mspace width=\"verythickmathspace\"/> <mtext>~a</mtext> " mexplabel)))
		 (format texport "</math>")))
	(cond(filename(terpri texport); and drain port if not terminal
		      (close texport)))
	(return mexplabel)))

(defun mathml (x l r lop rop)
	;; x is the expression of interest; l is the list of strings to its
	;; left, r to its right. lop and rop are the operators on the left
	;; and right of x in the tree, and will determine if parens must
	;; be inserted
	(setq x (nformat x))
	(cond ((atom x) (mathml-atom x l r))
	      ((or (<= (mathml-lbp (caar x)) (mathml-rbp lop)) 
                   (> (mathml-lbp rop) (mathml-rbp (caar x))))
	       (mathml-paren x l r))
	      ;; special check needed because macsyma notates arrays peculiarly
	      ((member 'array (cdar x) :test #'eq) (mathml-array x l r))
	      ;; dispatch for object-oriented mathml-ifiying
	      ((get (caar x) 'mathml) (funcall (get (caar x) 'mathml) x l r))
	      (t (mathml-function x l r nil))))

(defun string-substitute (newstring oldchar x &aux matchpos)
  (setq matchpos (position oldchar x))
  (if (null matchpos) x
    (concatenate 'string 
                 (subseq x 0 matchpos)
                 newstring
                 (string-substitute newstring oldchar (subseq x (1+ matchpos))))))

;;; NOTE that we try to include spaces after closing tags (e.g. "</mwhatever> ")
;;; so that the line breaking algorithm in myprinc has some spaces where it
;;; can choose to line break.

;;; First we have the functions which are called directly by mathml and its
;;; descendents

(defun mathml-atom (x l r) 
  (append l
	  (list (cond ((numberp x) (mathmlnumformat x))
                      ((stringp x) (format nil "<mtext>~a</mtext>" x))
		      ((and (symbolp x) (get x 'mathmlword)))
		      (t (mathml-stripdollar x))))
	  r))

(defun mathmlnumformat(atom)
  (let (r firstpart exponent)
    (cond ((integerp atom)
           (strcat "<mn>" (format nil "~d" atom) "</mn> "))
	  (t
	   (setq r (explode atom))
	   (setq exponent (member 'e r :test #'string-equal));; is it ddd.ddde+EE
	   (cond ((null exponent)
                  (strcat "<mn>" (format nil "~a" (implode (exploden atom))) "</mn> "))
		 (t
		  (setq firstpart
			(nreverse (cdr (member 'e (reverse r) :test #'string-equal))))
		  (strcat 
                   "<mrow><mn>"
                   (apply #'strcat firstpart)
                   "</mn><mo>&times;</mo> <msup><mn>10</mn><mn>"
                   (apply #'strcat (cdr exponent))
                   "</mn></msup> </mrow> ")
                  ))))))

(defun mathml-stripdollar(sym)
  (or (symbolp sym) 
      (return-from mathml-stripdollar sym))
  (let* ((pname (maybe-invert-string-case (string-left-trim '(#\$) (symbol-name sym))))
	 (l (length pname))
	 (begin-sub
	  (loop for i downfrom (1- l)
		 when (not (digit-char-p (aref pname i)))
		 do (return (1+ i)))))
    (cond ((< begin-sub l) ;; need to do subscripting
           (strcat "<msub><mi>" 
                   (subseq pname 0 begin-sub)
                   "</mi> <mn>" 
                   (subseq pname begin-sub l)
                   "</mn></msub> "))
          (t ;; no subscripting needed
           (strcat "<mi>" pname "</mi> ")))))

(defun mathml-paren (x l r)
  (mathml x (append l '("<mfenced separators=\"\">")) (cons "</mfenced> " r) 'mparen 'mparen))

(defun mathml-array (x l r)
  (let ((f))
    (if (eq 'mqapply (caar x))
	(setq f (cadr x)
	      x (cdr x)
	      l (mathml f (append l (list "<mfenced separators=\",\">")) 
                        (list "</mfenced> ") 'mparen 'mparen))
      (setq f (caar x)
	    l (mathml (mathmlword f) (append l '("<msub><mrow>")) nil lop 'mfunction)))
    (setq
     r (nconc (mathml-list (cdr x) nil (list "</mrow></msub> ") "<mo>,</mo>") r))
    (nconc l (list "</mrow><mrow>") r  )))

;; set up a list , separated by symbols (, * ...)  and then tack on the
;; ending item (e.g. "]" or perhaps ")"
(defun mathml-list (x l r sym)
  (if (null x) r
      (do ((nl))
	  ((null (cdr x))
	   (setq nl (nconc nl (mathml (car x)  l r 'mparen 'mparen)))
	   nl)
	  (setq nl (nconc nl (mathml (car x)  l (list sym) 'mparen 'mparen))
		  x (cdr x)
		  l nil))))

;; we could patch this so sin x rather than sin(x), but instead we made sin a prefix
;; operator
(defun mathml-function (x l r op) op
  (setq l (mathml (mathmlword (caar x)) l nil 'mparen 'mparen)
        r (mathml (cons '(mprogn) (cdr x)) nil r 'mparen 'mparen))
  (nconc l r))

;;; Now we have functions which are called via property lists

(defun mathml-prefix (x l r)
  (mathml (cadr x) (append l (mathmlsym (caar x))) r (caar x) rop))

(defun mathml-infix (x l r)
  ;; check for 2 args
  (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))
  (setq l (mathml (cadr x) l nil lop (caar x)))
  (mathml (caddr x) (append l (mathmlsym (caar x))) r (caar x) rop))

(defun mathml-postfix (x l r)
  (mathml (cadr x) l (append (mathmlsym (caar x)) r) lop (caar x)))

(defun mathml-nary (x l r)
  (let* ((op (caar x)) (sym (mathmlsym op)) (y (cdr x)) (ext-lop lop) (ext-rop rop))
    (cond ((null y)       (mathml-function x l r t)) ; this should not happen
          ((null (cdr y)) (mathml-function x l r t)) ; this should not happen, too
          (t (do ((nl) (lop ext-lop op) (rop op (if (null (cdr y)) ext-rop op)))
                 ((null (cdr y)) (setq nl (nconc nl (mathml (car y)  l r lop rop))) nl)
	         (setq nl (nconc nl (mathml (car y)  l (list sym)   lop rop))
		       y (cdr y)
		       l nil))))))

(defun mathml-nofix (x l r) (mathml (caar x) l r (caar x) rop))

(defun mathml-matchfix (x l r)
  (setq l (append l (car (mathmlsym (caar x))))
	;; car of mathmlsym of a matchfix operator is the lead op
	r (append (cdr (mathmlsym (caar x))) r)
	;; cdr is the trailing op
	x (mathml-list (cdr x) nil r "<mo>,</mo>")) 
  (append l x))

(defun mathmlsym (x) (or (get x 'mathmlsym) (get x 'strsym)(get x 'dissym)
		      (stripdollar x)))

(defun mathmlword (x)(or (get x 'mathmlword) (stripdollar x)))

(defprop bigfloat mathml-bigfloat mathml)

(defun mathml-bigfloat (x l r) 
  (let ((formatted (fpformat x)))
    (if (or (find '|b| formatted) (find '|B| formatted))
      (let*
        ((spell-out-expt
           (append
             '("<mrow><msub><mn>")
             (apply #'append
                    (mapcar
                     #'(lambda (e) (if (or (eq e '|b|) (eq e '|B|))
                                       '("</mn><mi>B</mi></msub>"
                                         "<mo>&times;</mo>"
                                         "<msup><mn>10</mn><mn>")
                                       (list e)))
                      formatted))
             '("</mn></msup></mrow>"))))
        (append l spell-out-expt r))
      (append l formatted r))))

(defprop mprog "<mi>block</mi><mspace width=\"mediummathspace\"/> " mathmlword) 
(defprop %erf "<mi>erf</mi> " mathmlword)
(defprop $erf "<mi>erf</mi> " mathmlword) ;; etc for multicharacter names
(defprop $true  "<mi>true</mi> "  mathmlword)
(defprop $false "<mi>false</mi> " mathmlword)

(defprop mprogn mathml-matchfix mathml) ;; mprogn is (<progstmnt>, ...)
(defprop mprogn (("<mfenced separators=\"\">") "</mfenced> ") mathmlsym)

(defprop mlist mathml-matchfix mathml)
(defprop mlist (("<mfenced separators=\"\" open=\"[\" close=\"]\">")"</mfenced> ") mathmlsym)

;;absolute value
(defprop mabs mathml-matchfix mathml)
(defprop mabs (("<mfenced separators=\"\" open=\"|\" close=\"|\">")"</mfenced> ") mathmlsym) 

(defprop mqapply mathml-mqapply mathml)

(defun mathml-mqapply (x l r)
  (setq l (mathml (cadr x) l (list "(" ) lop 'mfunction)
	r (mathml-list (cddr x) nil (cons ")" r) "<mo>,</mo>"))
  (append l r));; fixed 9/24/87 RJF

(defprop $%i "<mi>&ImaginaryI;</mi> " mathmlword)
(defprop $%pi "<mi>&pi;</mi> " mathmlword)
(defprop $%e "<mi>&ExponentialE;</mi> " mathmlword)
(defprop $inf "<mi>&infin;</mi> " mathmlword)
(defprop $minf "<mi>-&infin;</mi> " mathmlword)
(defprop %laplace "<mo>&Laplacetrf;</mo>" mathmlword)
(defprop $alpha "<mi>&alpha;</mi> " mathmlword)
(defprop $beta "<mi>&beta;</mi> " mathmlword)
(defprop $gamma "<mi>&gamma;</mi> " mathmlword)
(defprop %gamma "<mi>&Gamma;</mi> " mathmlword)
(defprop $delta "<mi>&delta;</mi> " mathmlword)
(defprop $epsilon "<mi>&epsilon;</mi> " mathmlword)
(defprop $zeta "<mi>&zeta;</mi> " mathmlword)
(defprop $eta "<mi>&eta;</mi> " mathmlword)
(defprop $theta "<mi>&theta;</mi> " mathmlword)
(defprop $iota "<mi>&iota;</mi> " mathmlword)
(defprop $kappa "<mi>&kappa;</mi> " mathmlword)
;(defprop $lambda "<mi>&lambda;</mi> " mathmlword)
(defprop $mu "<mi>&mu;</mi> " mathmlword)
(defprop $nu "<mi>&nu;</mi> " mathmlword)
(defprop $xi "<mi>&xi;</mi> " mathmlword)
(defprop $pi "<mi>&pi;</mi> " mathmlword)
(defprop $rho "<mi>&rho;</mi> " mathmlword)
(defprop $sigma "<mi>&sigma;</mi> " mathmlword)
(defprop $tau "<mi>&tau;</mi> " mathmlword)
(defprop $upsilon "<mi>&upsilon;</mi> " mathmlword)
(defprop $phi "<mi>&phi;</mi> " mathmlword)
(defprop $chi "<mi>&chi;</mi> " mathmlword)
(defprop $psi "<mi>&psi;</mi> " mathmlword)
(defprop $omega "<mi>&omega;</mi> " mathmlword)

(defprop mquote mathml-prefix mathml)
(defprop mquote ("<mo>'</mo>") mathmlsym)
(defprop mquote 201. mathml-rbp)

(defprop msetq mathml-infix mathml)
(defprop msetq ("<mo>:</mo>") mathmlsym)
(defprop msetq 180. mathml-rbp)
(defprop msetq 20. mathml-rbp)

(defprop mset mathml-infix mathml)
(defprop mset ("<mo>::</mo>") mathmlsym)
(defprop mset 180. mathml-lbp)
(defprop mset 20. mathml-rbp)

(defprop mdefine mathml-infix mathml)
(defprop mdefine ("<mo>:=</mo>") mathmlsym)
(defprop mdefine 180. mathml-lbp)
(defprop mdefine 20. mathml-rbp)

(defprop mdefmacro mathml-infix mathml)
(defprop mdefmacro ("<mo>::=</mo>") mathmlsym)
(defprop mdefmacro 180. mathml-lbp)
(defprop mdefmacro 20. mathml-rbp)

(defprop marrow mathml-infix mathml)
(defprop marrow ("<mo>&rightarrow;</mo>") mathmlsym)
(defprop marrow 25 mathml-lbp)
(defprop marrow 25 mathml-rbp)

(defprop mfactorial mathml-postfix mathml)
(defprop mfactorial ("<mo>!</mo>") mathmlsym)
(defprop mfactorial 160. mathml-lbp)

(defprop mexpt mathml-mexpt mathml)
(defprop mexpt 140. mathml-lbp)
(defprop mexpt 139. mathml-rbp)

(defprop %sum 110. mathml-rbp)  ;; added by BLW, 1 Oct 2001
(defprop %product 115. mathml-rbp) ;; added by BLW, 1 Oct 2001

;; insert left-angle-brackets for mncexpt. a^<n> is how a^^n looks.
(defun mathml-mexpt (x l r)
  (let((nc (eq (caar x) 'mncexpt))); true if a^^b rather than a^b
     ;; here is where we have to check for f(x)^b to be displayed
     ;; as f^b(x), as is the case for sin(x)^2 .
     ;; which should be sin^2 x rather than (sin x)^2 or (sin(x))^2.
     ;; yet we must not display (a+b)^2 as +^2(a,b)...
     ;; or (sin(x))^(-1) as sin^(-1)x, which would be arcsine x
     (cond ;; this whole clause
	   ;; should be deleted if this hack is unwanted and/or the
	   ;; time it takes is of concern.
	   ;; it shouldn't be too expensive.
	   ((and (eq (caar x) 'mexpt) ; don't do this hack for mncexpt
		 (let*
		  ((fx (cadr x)); this is f(x)
		   (f (and (not (atom fx)) (atom (caar fx)) (caar fx))) ; this is f [or nil]
		   (bascdr (and f (cdr fx))) ; this is (x) [maybe (x,y..), or nil]
		   (expon (caddr x)) ;; this is the exponent
		   (doit (and
			  f ; there is such a function
			  (member (getcharn f 1) '(#\% #\$)) ;; insist it is a % or $ function
                          (not (member f '(%sum %product %derivative %integrate %at
					      %lsum %limit) :test #'eq)) ;; what else? what a hack...
			  (or (and (atom expon) (not (numberp expon))) ; f(x)^y is ok
			      (and (atom expon) (numberp expon) (> expon 0))))))
			      ; f(x)^3 is ok, but not f(x)^-1, which could
			      ; inverse of f, if written f^-1 x
			      ; what else? f(x)^(1/2) is sqrt(f(x)), ??
		  (cond (doit
			(setq l (mathml `((mexpt) ,f ,expon) l nil 'mparen 'mparen))
			(if (and (null (cdr bascdr))
				 (eq (get f 'mathml) 'mathml-prefix))
			    (setq r (mathml (car bascdr) nil r f 'mparen))
			  (setq r (mathml (cons '(mprogn) bascdr) nil r 'mparen 'mparen))))
		        (t nil))))) ; won't doit. fall through
      (t (setq l (mathml (cadr x) (append l '("<msup><mrow>")) nil lop (caar x))
	       r (if (mmminusp (setq x (nformat (caddr x))))
		    ;; the change in base-line makes parens unnecessary
		    (if nc
			(mathml (cadr x) '("</mrow> <mfenced separators=\"\" open=\"<\" close=\">\"> -")(cons "</mfenced></msup> " r) 'mparen 'mparen)
			(mathml (cadr x) '("</mrow> <mfenced separators=\"\"> -")(cons "</mfenced></msup> " r) 'mparen 'mparen))
		    (if nc
			(mathml x (list "</mrow> <mfenced separators=\"\" open=\"<\" close=\">\">")(cons "</mfenced></msup>" r) 'mparen 'mparen)
			(if (and (numberp x) (< x 10))
			    (mathml x (list "</mrow> ")(cons "</msup> " r) 'mparen 'mparen)
			    (mathml x (list "</mrow> <mrow>")(cons "</mrow></msup> " r) 'mparen 'mparen))
			)))))
      (append l r)))

(defprop mncexpt mathml-mexpt mathml)

(defprop mncexpt 135. mathml-lbp)
(defprop mncexpt 134. mathml-rbp)

(defprop mnctimes mathml-nary mathml)
(defprop mnctimes "<mi>&ctdot;</mi> " mathmlsym)
(defprop mnctimes 110. mathml-lbp)
(defprop mnctimes 109. mathml-rbp)

(defprop mtimes mathml-nary mathml)
(defprop mtimes "<mspace width=\"thinmathspace\"/>" mathmlsym)
(defprop mtimes 120. mathml-lbp)
(defprop mtimes 120. mathml-rbp)

(defprop %sqrt mathml-sqrt mathml)

(defun mathml-sqrt(x l r)
  ;; format as \\sqrt { } assuming implicit parens for sqr grouping
  (mathml (cadr x) (append l  '("<msqrt>")) (append '("</msqrt>") r) 'mparen 'mparen))

;; macsyma doesn't know about cube (or nth) roots,
;; but if it did, this is what it would look like.
(defprop $cubrt mathml-cubrt mathml)

(defun mathml-cubrt (x l r)
  (mathml (cadr x) (append l  '("<mroot><mrow>")) (append '("</mrow>3</mroot>") r) 'mparen 'mparen))

(defprop mquotient mathml-mquotient mathml)
(defprop mquotient ("<mo>/</mo>") mathmlsym)
(defprop mquotient 122. mathml-lbp) ;;dunno about this
(defprop mquotient 123. mathml-rbp)

(defun mathml-mquotient (x l r)
  (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))
  (setq l (mathml (cadr x) (append l '("<mfrac><mrow>")) nil 'mparen 'mparen)
	r (mathml (caddr x) (list "</mrow> <mrow>") (append '("</mrow></mfrac> ")r) 'mparen 'mparen))
  (append l r))

(defprop $matrix mathml-matrix mathml)

(defun mathml-matrix(x l r) ;;matrix looks like ((mmatrix)((mlist) a b) ...)
  (append l `("<mfenced separators=\"\" open=\"(\" close=\")\"><mtable>")
	 (mapcan #'(lambda(y)
			  (mathml-list (cdr y) (list "<mtr><mtd>") (list "</mtd></mtr> ") "</mtd><mtd>"))
		 (cdr x))
	 '("</mtable></mfenced> ") r))

;; macsyma sum or prod is over integer range, not  low <= index <= high
;; Mathml is lots more flexible .. but

(defprop %sum mathml-sum mathml)
(defprop %lsum mathml-lsum mathml)
(defprop %product mathml-sum mathml)

;; easily extended to union, intersect, otherops

(defun mathml-lsum(x l r)
  (let ((op (cond ((eq (caar x) '%lsum) "<mrow><munder><mo>&sum;</mo> <mrow>")
		  ;; extend here
		  ))
	;; gotta be one of those above 
	(s1 (mathml (cadr x) nil nil 'mparen rop));; summand
	(index ;; "index = lowerlimit"
	       (mathml `((min simp) , (caddr x), (cadddr x))  nil nil 'mparen 'mparen)))
       (append l `( ,op ,@index "</mrow></munder> <mrow>" ,@s1 "</mrow></mrow> ") r)))

(defun mathml-sum(x l r)
  (let ((op (cond ((eq (caar x) '%sum) "<mrow><munderover><mo>&sum;</mo><mrow>")
		  ((eq (caar x) '%product) "<mrow><munderover><mo>&prod;</mo><mrow>")
		  ;; extend here
		  ))
	;; gotta be one of those above
	(s1 (mathml (cadr x) nil nil 'mparen rop));; summand
	(index ;; "index = lowerlimit"
	       (mathml `((mequal simp) ,(caddr x),(cadddr x)) nil nil 'mparen 'mparen))
	(toplim (mathml (car(cddddr x)) nil nil 'mparen 'mparen)))
       (append l `( ,op ,@index "</mrow> <mrow>" ,@toplim "</mrow></munderover> <mrow>" ,@s1 "</mrow></mrow> ") r)))

(defprop %integrate mathml-int mathml)

(defun mathml-int (x l r)
  (let ((s1 (mathml (cadr x) nil nil 'mparen 'mparen));;integrand delims / & d
	(var (mathml (caddr x) nil nil 'mparen rop))) ;; variable
       (cond((= (length x) 3)
	     (append l `("<mrow><mo>&int;</mo><mrow>" ,@s1 "</mrow> <mspace width=\"mediummathspace\"/> <mrow><mo>&DifferentialD;</mo><mi>" ,@var "</mi></mrow></mrow> ") r))
	    (t ;; presumably length 5
	       (let ((low (mathml (nth 3 x) nil nil 'mparen 'mparen))
		     ;; 1st item is 0
		     (hi (mathml (nth 4 x) nil nil 'mparen 'mparen)))
		    (append l `("<mrow><munderover><mo>&int;</mo> <mrow>" ,@low "</mrow> <mrow>" ,@hi "</mrow> </munderover> <mrow>" ,@s1 "</mrow> <mspace width=\"mediummathspace\"/> <mrow><mo>&DifferentialD;</mo><mi>" ,@var "</mi> </mrow></mrow> ") r))))))

(defprop %limit mathml-limit mathml)

(defprop mrarr mathml-infix mathml)
(defprop mrarr ("<mo>&rarr;</mo> ") mathmlsym)
(defprop mrarr 80. mathml-lbp)
(defprop mrarr 80. mathml-rbp)

(defun mathml-limit(x l r) ;; ignoring direction, last optional arg to limit
  (let ((s1 (mathml (second x) nil nil 'mparen rop));; limitfunction
	(subfun ;; the thing underneath "limit"
         (mathml `((mrarr simp) ,(third x) ,(fourth x)) nil nil 'mparen 'mparen)))
       (append l `("<munder><mo>lim</mo><mrow>" ,@subfun "</mrow> </munder> <mrow>" ,@s1 "</mrow>") r)))

(defprop %at mathml-at mathml)

;; e.g.  at(diff(f(x)),x=a)
(defun mathml-at (x l r)
  (let ((s1 (mathml (cadr x) nil nil lop rop))
	(sub (mathml (caddr x) nil nil 'mparen 'mparen)))
       (append l '("<msub><mfenced separators=\"\" open=\"\" close=\"|\">") s1  '("</mfenced> <mrow>") sub '("</mrow> </msub> ") r)))

;;binomial coefficients

(defprop %binomial mathml-choose mathml)

(defun mathml-choose (x l r)
  `(,@l
    "<mfenced separators=\"\" open=\"(\" close=\")\"><mtable><mtr><mtd>"
    ,@(mathml (cadr x) nil nil 'mparen 'mparen)
    "</mtd></mtr> <mtr><mtd>"
    ,@(mathml (caddr x) nil nil 'mparen 'mparen)
    "</mtd></mtr> </mtable></mfenced> "
    ,@r))


(defprop rat mathml-rat mathml)
(defprop rat 120. mathml-lbp)
(defprop rat 121. mathml-rbp)
(defun mathml-rat(x l r) (mathml-mquotient x l r))

(defprop mplus mathml-mplus mathml)
(defprop mplus 100. mathml-lbp)
(defprop mplus 100. mathml-rbp)

(defun mathml-mplus (x l r)
 ;(declare (fixnum w))
 (cond ((member 'trunc (car x) :test #'eq)
	(setq r (cons "<mo>+</mo><mtext>&ctdot;</mtext> " r))))
 (cond ((null (cddr x))
	(if (null (cdr x))
	    (mathml-function x l r t)
	    (mathml (cadr x) (cons "<mo>+</mo>" l) r 'mplus rop)))
       (t (setq l (mathml (cadr x) l nil lop 'mplus)
		x (cddr x))
	  (do ((nl l)  (dissym))
	      ((null (cdr x))
	       (if (mmminusp (car x)) (setq l (cadar x) dissym (list "<mo>-</mo> "))
		   (setq l (car x) dissym (list "<mo>+</mo> ")))
	       (setq r (mathml l dissym r 'mplus rop))
	       (append nl r))
	      (if (mmminusp (car x)) (setq l (cadar x) dissym (list "<mo>-</mo> "))
		  (setq l (car x) dissym (list "<mo>+</mo> ")))
	      (setq nl (append nl (mathml l dissym nil 'mplus 'mplus))
		    x (cdr x))))))

(defprop mminus mathml-prefix mathml)
(defprop mminus ("<mo>-</mo>") mathmlsym)
(defprop mminus 100. mathml-rbp)
(defprop mminus 100. mathml-lbp)

(defprop min mathml-infix mathml)
(defprop min ("<mo>&isin;</mo> ") mathmlsym)
(defprop min 80. mathml-lbp)
(defprop min 80. mathml-rbp)

(defprop mequal mathml-infix mathml)
(defprop mequal ("<mo>=</mo> ") mathmlsym)
(defprop mequal 80. mathml-lbp)
(defprop mequal 80. mathml-rbp)

(defprop mnotequal mathml-infix mathml)
(defprop mnotequal 80. mathml-lbp)
(defprop mnotequal 80. mathml-rbp)

(defprop mgreaterp mathml-infix mathml)
(defprop mgreaterp ("<mo>&gt;</mo> ") mathmlsym)
(defprop mgreaterp 80. mathml-lbp)
(defprop mgreaterp 80. mathml-rbp)

(defprop mgeqp mathml-infix mathml)
(defprop mgeqp ("<mo>&ge;</mo> ") mathmlsym)
(defprop mgeqp 80. mathml-lbp)
(defprop mgeqp 80. mathml-rbp)

(defprop mlessp mathml-infix mathml)
(defprop mlessp ("<mo>&lt;</mo> ") mathmlsym)
(defprop mlessp 80. mathml-lbp)
(defprop mlessp 80. mathml-rbp)

(defprop mleqp mathml-infix mathml)
(defprop mleqp ("<mo>&le;</mo> ") mathmlsym)
(defprop mleqp 80. mathml-lbp)
(defprop mleqp 80. mathml-rbp)

(defprop mnot mathml-prefix mathml)
(defprop mnot ("<mo>&not;</mo> ") mathmlsym)
(defprop mnot 70. mathml-rbp)

(defprop mand mathml-nary mathml)
(defprop mand ("<mo>&and;</mo> ") mathmlsym)
(defprop mand 60. mathml-lbp)
(defprop mand 60. mathml-rbp)

(defprop mor mathml-nary mathml)
(defprop mor ("<mo>&or;</mo> ") mathmlsym)

;; make sin(x) display as sin x , but sin(x+y) as sin(x+y)
;; etc

(defun mathml-setup (x)
  (let((a (car x))
       (b (cadr x)))
      (setf (get a 'mathml) 'mathml-prefix)
      (setf (get a 'mathmlword) b)  ;This means "sin" will always be roman
      (setf (get a 'mathmlsym) (list b))
      (setf (get a 'mathml-rbp) 130)))

(mapc #'mathml-setup
  '(
     (%acos "<mi>arccos</mi> ")
     (%asin "<mi>arcsin</mi> ")
     (%atan "<mi>arctan</mi> ")
     (%arg "<mi>arg</mi> ")
     (%cos "<mi>cos</mi> ")
     (%cosh "<mi>cosh</mi> ")
     (%cot "<mi>cot</mi> ")
     (%coth "<mi>coth</mi> ")
     (%csc "<mi>cosec</mi> ")
     (%deg "<mi>deg</mi> ")
     (%determinant "<mi>det</mi> ")
     (%dim "<mi>dim</mi> ")
     (%exp "<mi>exp</mi> ")
     (%gcd "<mi>gcd</mi> ")
     (%hom "<mi>hom</mi> ")
     (%inf "<mi>&infin;</mi> ") 
     (%ker "<mi>ker</mi> ")
     (%lg "<mi>lg</mi> ")
     ;;(%limit "<mi>lim</mi> ")
     (%liminf "<mi>lim inf</mi> ")
     (%limsup "<mi>lim sup</mi> ")
     (%ln "<mi>ln</mi> ")
     (%log "<mi>log</mi> ")
     (%max "<mi>max</mi> ")
     (%min "<mi>min</mi> ")
     ; Latex's "Pr" ... ?
     (%sec "<mi>sec</mi> ")
     (%sech "<mi>sech</mi> ")
     (%sin "<mi>sin</mi> ")
     (%sinh "<mi>sinh</mi> ")
     (%sup "<mi>sup</mi> ")
     (%tan "<mi>tan</mi> ")
     (%tanh "<mi>tanh</mi> ")
    ;; (%erf "{\\rm erf}") this would tend to set erf(x) as erf x. Unusual
     ;(%laplace "{\\cal L}")
     )) ;; etc

(defprop mor mathml-nary mathml)
(defprop mor 50. mathml-lbp)
(defprop mor 50. mathml-rbp)

(defprop mcond mathml-mcond mathml)
(defprop mcond 25. mathml-lbp)
(defprop mcond 25. mathml-rbp)

(defprop %derivative mathml-derivative mathml)

(defun mathml-derivative (x l r)
  (mathml (mathml-d x "&DifferentialD;") l r lop rop ))

(defun mathml-d(x dsym) ;dsym should be "&DifferentialD;" or "&PartialD;"
  ;; format the macsyma derivative form so it looks
  ;; sort of like a quotient times the deriva-dand.
  (let*
   ((arg (cadr x)) ;; the function being differentiated
    (difflist (cddr x)) ;; list of derivs e.g. (x 1 y 2)
    (ords (odds difflist 0)) ;; e.g. (1 2)
    (vars (odds difflist 1)) ;; e.g. (x y)
    (numer `((mexpt) ,dsym ((mplus) ,@ords))) ; d^n numerator
    (denom (cons '(mtimes)
		 (mapcan #'(lambda(b e)
				  `(,dsym ,(simplifya `((mexpt) ,b ,e) nil)))
			 vars ords))))
   `((mtimes)
     ((mquotient) ,(simplifya numer nil) ,denom)
     ,arg)))

(defun mathml-mcond (x l r)
  (append l
    (mathml (cadr x) '("<mi>if</mi> <mspace width=\"mediummathspace\"/>")
      '("<mspace width=\"mediummathspace\"/> <mi>then</mi><mspace width=\"mediummathspace\"/> ") 'mparen 'mparen)
    (if (eql (fifth x) '$false)
      (mathml (caddr x) nil r 'mcond rop)
      (append (mathml (caddr x) nil nil 'mparen 'mparen)
        (mathml (fifth x) '("<mspace width=\"mediummathspace\"/> <mi>else</mi><mspace width=\"mediummathspace\"/> ") r 'mcond rop)))))

(defprop mdo mathml-mdo mathml)
(defprop mdo 30. mathml-lbp)
(defprop mdo 30. mathml-rbp)
(defprop mdoin mathml-mdoin mathml)
(defprop mdoin 30. mathml-rbp)

(defun mathml-lbp(x)(cond((get x 'mathml-lbp))(t(lbp x))))
(defun mathml-rbp(x)(cond((get x 'mathml-rbp))(t(lbp x))))

;; these aren't quite right

(defun mathml-mdo (x l r)
  (mathml-list (mathmlmdo x) l r "<mspace width=\"mediummathspace\"/> "))

(defun mathml-mdoin (x l r)
  (mathml-list (mathmlmdoin x) l r "<mspace width=\"mediummathspace\"/> "))

(defun mathmlmdo (x)
   (nconc (cond ((second x) `("<mi>for</mi> " ,(second x))))
	 (cond ((equal 1 (third x)) nil)
	       ((third x)  `("<mi>from</mi> " ,(third x))))
	 (cond ((equal 1 (fourth x)) nil)
	       ((fourth x) `("<mi>step</mi> " ,(fourth x)))
	       ((fifth x)  `("<mi>next</mi> " ,(fifth x))))
	 (cond ((sixth x)  `("<mi>thru</mi> " ,(sixth x))))
	 (cond ((null (seventh x)) nil)
	       ((eq 'mnot (caar (seventh x)))
		`("<mi>while</mi> " ,(cadr (seventh x))))
	       (t `("<mi>unless</mi> " ,(seventh x))))
	 `("<mi>do</mi> " ,(eighth x))))

(defun mathmlmdoin (x)
  (nconc `("<mi>for</mi>" ,(second x) "<mi>in</mi> " ,(third x))
	 (cond ((sixth x) `("<mi>thru</mi> " ,(sixth x))))
	 (cond ((null (seventh x)) nil)
	       ((eq 'mnot (caar (seventh x)))
		`("<mi>while</mi> " ,(cadr (seventh x))))
	       (t `("<mi>unless</mi> " ,(seventh x))))
	 `("<mi>do</mi> " ,(eighth x))))

;; Undone and trickier:
;; Maybe do some special hacking for standard notations for
;; hypergeometric fns, alternative summation notations  0<=n<=inf, etc.
