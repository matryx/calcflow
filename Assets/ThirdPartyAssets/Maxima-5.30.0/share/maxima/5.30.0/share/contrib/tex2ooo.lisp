; Modifications of tex properties and formatting functions to yield output suitable for OpenOffice formula writer.
; Modifications to src/mactex.lisp made by Dieter Schuster,
; extracted into this file by Robert Dodier.
; Lines beginning with ";-" are lines which have been modified.
; In addition, all of the defprops here have been modified.

; Usage: 
;  load (tex2ooo);
;  tex (expr);


(declare-top
 (special lop rop ccol $gcprint texport $labels $inchar))

(defun quote-% (sym)
  (let* ((strsym (string sym))
         (pos (position-if #'(lambda (c) (find c "%_")) strsym)))
    (if pos
;-      (concatenate 'string (subseq strsym 0 pos) "\\" (subseq strsym pos (1+ pos))
        (concatenate 'string (subseq strsym 0 pos) "" (subseq strsym pos (1+ pos))
                           (quote-% (subseq strsym (1+ pos))))
      strsym)))

(defun tex1 (mexplabel &optional filename ) ;; mexplabel, and optional filename
  (prog (mexp  texport $gcprint ccol x y itsalabel)
     ;; $gcprint = nil turns gc messages off
     (setq ccol 1)
     (cond ((null mexplabel)
	    (displa " No eqn given to TeX")
	    (return nil)))
     ;; collect the file-name, if any, and open a port if needed
     (setq texport (cond((null filename) *standard-output* ) ; t= output to terminal
			(t
			 (open (string (print-invert-case (stripdollar filename)))
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
	    (if mexplabel (setq mexplabel (quote-% mexplabel)))
	    (format texport "|~%" )	;delimit with |marks
	    (cond (mexplabel (format texport "~a " mexplabel)))
	    (mgrind mexp texport)	;write expression as string
	    (format texport ";|~%"))
	   ((and
	     itsalabel ;; but is it a user-command-label?
	     (<= (length (string $inchar)) (length (string mexplabel)))
	     (string= (subseq (string $inchar) 1 (length (string $inchar)))
		      (subseq (string mexplabel) 1 (length (string $inchar))))
	     ;; Check to make sure it isn't an outchar in disguise
	     (not
	      (and
	       (<= (length (string $outchar)) (length (string mexplabel)))
	       (string= (subseq (string $outchar) 1 (length (string $outchar)))
			(subseq (string mexplabel) 1 (length (string $outchar)))))))
	    ;; aha, this is a C-line: do the grinding:
	    (format texport "~%|~a " mexplabel) ;delimit with |marks
	    (mgrind mexp texport)	;write expression as string
	    (format texport ";|~%"))
	   (t 
	    (if mexplabel (setq mexplabel (quote-% mexplabel)))
					; display the expression for TeX now:
;-	    (myprinc "$$")
  	    (myprinc "" texport)
	    (mapc #'(lambda (x) (myprinc x texport))
		  ;;initially the left and right contexts are
		  ;; empty lists, and there are implicit parens
		  ;; around the whole expression
		  (tex mexp nil nil 'mparen 'mparen))
	    (cond (mexplabel
;-		   (format texport "\\leqno{\\tt ~a}" mexplabel)))
;-	    (format texport "$$")))
  		   (format texport "" mexplabel)))
  	    (format texport "")))
     (terpri texport)
     (cond (filename   ; and close port if not terminal
	    (close texport)))
     (return mexplabel)))

(defun tex-string (x)
  (cond ((equal x "") "")
	((eql (elt x 0) #\\) x)
;-	(t (concatenate 'string "\\mbox{{}" x "{}}"))))
  	(t (concatenate 'string "" x ""))))

(defun tex-char (x)
;-  (if (eql x #\|) "\\mbox{\\verb/|/}"
;-      (concatenate 'string "\\mbox{\\verb|" (string x) "|}")))
    (if (eql x #\|) ""
        (concatenate 'string "" (string x) "")))

(defun tex-stripdollar(sym &aux )
  (or (symbolp sym) (return-from tex-stripdollar sym))
  (let* ((pname (quote-% sym))
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
;-		  (vector-push #\_ tem)
  		  ;; (vector-push #\_ tem)
		  (unless (eql i (- l 1))
		    (vector-push #\{ tem)
		    (setq begin-sub t))))
	   (cond ((not (and (eql i 0) (eql (aref pname i) #\$)))
		  (vector-push (aref pname i) tem)))
	   finally
	   (cond ((eql begin-sub t)
		  (vector-push #\} tem))))
    (intern tem)))

(defun texnumformat(atom)
  (let (r firstpart exponent)
    (cond ((integerp atom)
	   atom)
	  (t
	   (setq r (explode atom))
	   (setq exponent (member 'e r :test #'string-equal)) ;; is it ddd.ddde+EE
	   (cond ((null exponent)
		  ;; it is not. go with it as given
		  atom)
		 (t
		  (setq firstpart
			(nreverse (cdr (member 'e (reverse r) :test #'string-equal))))
		  (strcat (apply #'strcat firstpart )
;-			  " \\times 10^{"
  			  " times 10^{"
			  (apply #'strcat (cdr exponent))
			  "}")))))))

(defun tex-paren (x l r)
;-  (tex x (append l '("\\left(")) (cons "\\right)" r) 'mparen 'mparen))
    (tex x (append l '(" left( ")) (cons " right)" r) 'mparen 'mparen))

(defun tex-array (x l r)
  (let ((f))
    (if (eq 'mqapply (caar x))
	(setq f (cadr x)
	      x (cdr x)
;-	      l (tex f (append l (list "\\left(")) (list "\\right)") 'mparen 'mparen))
  	      l (tex f (append l (list " left( ")) (list " right) ") 'mparen 'mparen))
	(setq f (caar x)
	      l (tex f l nil lop 'mfunction)))
    (setq
     r (nconc (tex-list (cdr x) nil (list "}") ",") r))
    (nconc l (list "_{") r  )))

(defprop mprog "" texword)
(defprop %erf " erf " texword)
(defprop $erf " erf " texword) ;; etc for multicharacter names
(defprop $true  " true "  texword)
(defprop $false " false " texword)
(defprop mprogn ((" left( ") " right) ") texsym)
(defprop mlist ((" left[ ")" right] ") texsym)
(defprop mabs ((" left lline ")" right rline ") texsym)
(defprop $%pi "%pi" texword)
(defprop $inf " infty " texword)
(defprop $minf " - infty " texword)
(defprop %laplace "%DELTA" texword)
(defprop $alpha "%alpha" texword)
(defprop $beta "%beta" texword)
(defprop $gamma "%gamma" texword)
(defprop %gamma "%GAMMA" texword)
(defprop $%gamma "%gamma" texword)
(defprop $delta "%delta" texword)
(defprop $epsilon "%varepsilon" texword)
(defprop $zeta "%zeta" texword)
(defprop $eta "%eta" texword)
(defprop $theta "%vartheta" texword)
(defprop $iota "%iota" texword)
(defprop $kappa "%varkappa" texword)
(defprop $mu "%my" texword)
(defprop $nu "%nu" texword)
(defprop $xi "%xi" texword)
(defprop $pi "%pi" texword)
(defprop $rho "%rho" texword)
(defprop $sigma "%sigma" texword)
(defprop $tau "%tau" texword)
(defprop $upsilon "%ypsilon" texword)
(defprop $phi "%varphi" texword)
(defprop $chi "%chi" texword)
(defprop $psi "%psi" texword)
(defprop $omega "%omega" texword)
(defprop |$Gamma| "%GAMMA" texword)
(defprop |$Delta| "%DELTA" texword)
(defprop |$Theta| "%ThETA" texword)
(defprop |$Lambda| "%LAMBDA" texword)
(defprop |$Xi| "%XI" texword)
(defprop |$Pi| "%PI" texword)
(defprop |$Sigma| "%SIGMA" texword)
(defprop |$Upsilon| "%YPSILON" texword)
(defprop |$Phi| "%PHI" texword)
(defprop |$Psi| "%PSI" texword)
(defprop |$Omega| "%OMEGA" texword)
(defprop marrow (" rightarrow ") texsym)

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
				       %lsum %limit) :test #'eq)) ;; what else? what a hack...
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
      (t (setq l (cond ((and (numberp (cadr x))
			     (numneedsparen (cadr x)))
;-			(tex (cadr x) (cons "\\left(" l) '("\\right)") lop
  			(tex (cadr x) (cons " left( " l) '(" right) ") lop
			     (caar x)))
		       (t (tex (cadr x) l nil lop (caar x))))
	       r (if (mmminusp (setq x (nformat (caddr x))))
		     ;; the change in base-line makes parens unnecessary
		     (if nc
;-			 (tex (cadr x) '("^ {-\\langle ")(cons "\\rangle }" r) 'mparen 'mparen)
  			 (tex (cadr x) '("^ {- langle ")(cons " rangle }" r) 'mparen 'mparen)
			 (tex (cadr x) '("^ {- ")(cons " }" r) 'mparen 'mparen))
		     (if nc
;-			 (tex x (list "^{\\langle ")(cons "\\rangle}" r) 'mparen 'mparen)
  			 (tex x (list "^{ langle ")(cons " rangle }" r) 'mparen 'mparen)
			 (if (and (integerp x) (< x 10))
;-			     (tex x (list "^")(cons "" r) 'mparen 'mparen)
  			     (tex x (list "^")(cons " " r) 'mparen 'mparen)
			     (tex x (list "^{")(cons "}" r) 'mparen 'mparen))
			 )))))
    (append l r)))

(defprop mnctimes (" cdot ") texsym)
(defprop mtimes (" cdot ") texsym)    ;; HMM, SEEMS INADVISABLE

(defun tex-sqrt(x l r)
  ;; format as \\sqrt { } assuming implicit parens for sqr grouping
;-  (tex (cadr x) (append l  '("\\sqrt{")) (append '("}") r) 'mparen 'mparen))
    (tex (cadr x) (append l  '(" sqrt {")) (append '("}") r) 'mparen 'mparen))

(defun tex-cubrt (x l r)
;-  (tex (cadr x) (append l  '("\\root 3 \\of{")) (append '("}") r) 'mparen 'mparen))
    (tex (cadr x) (append l  '(" nroot {3} {")) (append '("}") r) 'mparen 'mparen))

(defprop mquotient (" over ") texsym)

(defun tex-mquotient (x l r)
  (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))
;-  (setq l (tex (cadr x) (append l '("{{")) nil 'mparen 'mparen)
    (setq l (tex (cadr x) (append l '("{alignc {")) nil 'mparen 'mparen)
					;the divide bar groups things
;-	r (tex (caddr x) (list "}\\over{") (append '("}}")r) 'mparen 'mparen))
  	r (tex (caddr x) (list "} over {") (append '("}}")r) 'mparen 'mparen))
  (append l r))

(defun tex-matrix(x l r) ;;matrix looks like ((mmatrix)((mlist) a b) ...)
;-  (append l `("\\pmatrix{")
    (append l `(" left( matrix {")
	  (let ((foo (mapcan #'(lambda(y)
;-		      (tex-list (cdr y) nil (list "\\cr ") "&"))
  		      (tex-list (cdr y) nil (list " ## ") " # "))
		  (cdr x))))
            (setf (car (last foo)) " ")
            foo)
;-	  '("}") r))
  	  '("} right) ") r))

(defun tex-lsum(x l r)
;-  (let ((op (cond ((eq (caar x) '%lsum) "\\sum_{")
    (let ((op (cond ((eq (caar x) '%lsum) "sum from {")
		  ;; extend here
		  ))
	;; gotta be one of those above 
	(s1 (tex (cadr x) nil nil 'mparen rop))	;; summand
	(index ;; "index = lowerlimit"
	 (tex `((min simp) , (caddr x), (cadddr x))  nil nil 'mparen 'mparen)))
    (append l `( ,op ,@index "}}{" ,@s1 "}") r)))

(defun tex-sum(x l r)
;-  (let ((op (cond ((eq (caar x) '%sum) "\\sum_{")
;-		  ((eq (caar x) '%product) "\\prod_{")
    (let ((op (cond ((eq (caar x) '%sum) " sum from {")
  		  ((eq (caar x) '%product) " prod from {")
		  ;; extend here
		  ))
	;; gotta be one of those above
	(s1 (tex (cadr x) nil nil 'mparen rop))	;; summand
	(index ;; "index = lowerlimit"
	 (tex `((mequal simp) ,(caddr x),(cadddr x)) nil nil 'mparen 'mparen))
	(toplim (tex (car(cddddr x)) nil nil 'mparen 'mparen)))
;-    (append l `( ,op ,@index "}^{" ,@toplim "}{" ,@s1 "}") r)))
      (append l `( ,op ,@index "} to {" ,@toplim "}{" ,@s1 "}") r)))

(defun tex-int (x l r)
  (let ((s1 (tex (cadr x) nil nil 'mparen 'mparen)) ;;integrand delims / & d
	(var (tex (caddr x) nil nil 'mparen rop))) ;; variable
    (cond((= (length x) 3)
;-	  (append l `("\\int {" ,@s1 "}{\\;d" ,@var "}") r))
  	  (append l `(" int {" ,@s1 "}{`d" ,@var "}") r))
	 (t ;; presumably length 5
	  (let ((low (tex (nth 3 x) nil nil 'mparen 'mparen))
		;; 1st item is 0
		(hi (tex (nth 4 x) nil nil 'mparen 'mparen)))
;-	    (append l `("\\int_{" ,@low "}^{" ,@hi "}{" ,@s1 "\\;d" ,@var "}") r))))))
  	    (append l `(" int from {" ,@low "} to {" ,@hi "}{" ,@s1 " d" ,@var "}") r))))))

(defun tex-limit(x l r)	;; ignoring direction, last optional arg to limit
  (let ((s1 (tex (cadr x) nil nil 'mparen rop))	;; limitfunction
	(subfun	;; the thing underneath "limit"
;-	 (subst "\\rightarrow " '=
  	 (subst " rightarrow " '=
		(tex `((mequal simp) ,(caddr x),(cadddr x))
		     nil nil 'mparen 'mparen))))
;-    (append l `("\\lim_{" ,@subfun "}{" ,@s1 "}") r)))
      (append l `(" lim from {" ,@subfun "}{" ,@s1 "}") r)))

(defun tex-at (x l r)
  (let ((s1 (tex (cadr x) nil nil lop rop))
	(sub (tex (caddr x) nil nil 'mparen 'mparen)))
;-    (append l '("\\left.") s1  '("\\right|_{") sub '("}") r)))
      (append l '(" left .") s1  '(" right |_{") sub '("}") r)))

(defun tex-mbox (x l r)
;-  (append l '("\\boxed{") (tex (cadr x) nil nil 'mparen 'mparen) '("}") r))
    (append l '("{") (tex (cadr x) nil nil 'mparen 'mparen) '("}") r))

(defun tex-choose (x l r)
  `(,@l
;-    "\\pmatrix{"
      " matrix {"
    ,@(tex (cadr x) nil nil 'mparen 'mparen)
;-    "\\\\"
      " ## "
    ,@(tex (caddr x) nil nil 'mparen 'mparen)
    "}"
    ,@r))

(defun tex-mplus (x l r)
;-  (cond ((member 'trunc (car x) :test #'eq)(setq r (cons "+\\cdots " r))))
    (cond ((member 'trunc (car x) :test #'eq)(setq r (cons " + dotsaxis " r))))
  (cond ((null (cddr x))
	 (if (null (cdr x))
	     (tex-function x l r t)
;-	     (tex (cadr x) (cons "+" l) r 'mplus rop)))
  	     (tex (cadr x) (cons " + " l) r 'mplus rop)))
	(t (setq l (tex (cadr x) l nil lop 'mplus)
		 x (cddr x))
	   (do ((nl l)  (dissym))
	       ((null (cdr x))
;-		(if (mmminusp (car x)) (setq l (cadar x) dissym (list "-"))
;-		    (setq l (car x) dissym (list "+")))
  		(if (mmminusp (car x)) (setq l (cadar x) dissym (list " - "))
  		    (setq l (car x) dissym (list " + ")))
		(setq r (tex l dissym r 'mplus rop))
		(append nl r))
;-	     (if (mmminusp (car x)) (setq l (cadar x) dissym (list "-"))
;-		 (setq l (car x) dissym (list "+")))
  	     (if (mmminusp (car x)) (setq l (cadar x) dissym (list " - "))
  		 (setq l (car x) dissym (list " + ")))
	     (setq nl (append nl (tex l dissym nil 'mplus 'mplus))
		   x (cdr x))))))

(defprop mminus (" `-`") texsym)
(defprop min (" in ") texsym)
(defprop mgeqp (" geq ") texsym)
(defprop mleqp (" leq ") texsym)
(defprop mnot (" not ") texsym)
(defprop mand (" and ") texsym)
(defprop mor (" or ") texsym)
(defprop mnotequal (" neq ") texsym)

(mapc #'tex-setup
      '(
	(%acos " arccos ")
	(%asin " arcsin ")
	(%atan " arctan ")
	(%cos " cos ")
	(%cosh " cosh ")
	(%cot " cot ")
	(%coth " coth ")
	(%csc " csc ")
	(%determinant " det ")
	(%dim " dim ")
	(%exp " exp ")
	(%gcd " gcd ")
	(%inf " inf ")
	(%ln " ln ")
	(%log " log ")
	(%max " max ")
	(%min " min ")
	(%sec " sec ")
	(%sin " sin ")
	(%sinh " sinh ")
	(%tan " tan ")
	(%tanh " tanh ")
 	))

(defun tex-mcond (x l r)
  (append l
;-	  (tex (cadr x) '("\\mathbf{if}\\;")
;-	       '("\\;\\mathbf{then}\\;") 'mparen 'mparen)
  	  (tex (cadr x) '(" bold if")
  	       '(" bold then") 'mparen 'mparen)
     (if (eql (fifth x) '$false)
         (tex (caddr x) nil r 'mcond rop)
         (append (tex (caddr x) nil nil 'mparen 'mparen)
;-		      (tex (fifth x) '("\\;\\mathbf{else}\\;") r 'mcond rop)))))
  		      (tex (fifth x) '(" bold else") r 'mcond rop)))))
   
(defun tex-mdo (x l r)
;-  (tex-list (texmdo x) l r "\\;"))
    (tex-list (texmdo x) l r "`"))

(defun tex-mdoin (x l r)
;-  (tex-list (texmdoin x) l r "\\;"))
    (tex-list (texmdoin x) l r "`"))

(defun texmdo (x)
;-  (nconc (cond ((second x) `("\\mathbf{for}" ,(second x))))
    (nconc (cond ((second x) `(" bold for" ,(second x))))
	 (cond ((equal 1 (third x)) nil)
;-	       ((third x)  `("\\mathbf{from}" ,(third x))))
  	       ((third x)  `(" bold from" ,(third x))))
	 (cond ((equal 1 (fourth x)) nil)
;-	       ((fourth x) `("\\mathbf{step}" ,(fourth x)))
;-	       ((fifth x)  `("\\mathbf{next}" ,(fifth x))))
;-	 (cond ((sixth x)  `("\\mathbf{thru}" ,(sixth x))))
  	       ((fourth x) `(" bold step" ,(fourth x)))
  	       ((fifth x)  `(" bold next" ,(fifth x))))
  	 (cond ((sixth x)  `(" bold thru" ,(sixth x))))
	 (cond ((null (seventh x)) nil)
	       ((eq 'mnot (caar (seventh x)))
;-		`("\\mathbf{while}" ,(cadr (seventh x))))
;-	       (t `("\\mathbf{unless}" ,(seventh x))))
;-	 `("\\mathbf{do}" ,(eighth x))))
  		`(" bold while" ,(cadr (seventh x))))
  	       (t `(" bold unless" ,(seventh x))))
  	 `(" bold do" ,(eighth x))))

(defun texmdoin (x)
;-  (nconc `("\\mathbf{for}" ,(second x) "\\mathbf{in}" ,(third x))
;-	 (cond ((sixth x) `("\\mathbf{thru}" ,(sixth x))))
    (nconc `(" bold for" ,(second x) " bold in" ,(third x))
  	 (cond ((sixth x) `(" bold thru" ,(sixth x))))
	 (cond ((null (seventh x)) nil)
	       ((eq 'mnot (caar (seventh x)))
;-		`("\\mathbf{while}" ,(cadr (seventh x))))
;-	       (t `("\\mathbf{unless}" ,(seventh x))))
;-	 `("\\mathbf{do}" ,(eighth x))))
  		`(" bold while" ,(cadr (seventh x))))
  	       (t `(" bold unless" ,(seventh x))))
  	 `(" bold do" ,(eighth x))))
 
(defprop | --> | " rightarrow " texsym)
(defprop | WHERE | "` bold where`" texsym)
 
(defun tex-mlabel (x l r)
  (tex (caddr x)
       (append l
	       (if (cadr x)
;-		   (list (format nil "\\mbox{\\tt\\red(~A) \\black}" (tex-stripdollar (cadr x))))
  		   (list (format nil "" (tex-stripdollar (cadr x))))
		   nil))
       r 'mparen 'mparen))
 
(defun tex-spaceout (x l r)
;-  (append l (cons (format nil "\\hspace{~dmm}" (* 3 (cadr x))) r)))
    (append l (cons (format nil "~" (* 3 (cadr x))) r)))
