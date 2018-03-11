;*******************************************************************************
;*                                                                             *
;*  copyright (c) 1988 kent state univ.  kent, ohio 44242                      *
;*                                                                             *
;*******************************************************************************

(when (null (fboundp 'wrs)) (load "convmac.lisp"))

(declare-top (special *gentran-dir tempvartype* tempvarname* tempvarnum* genstmtno*
	genstmtincr* *symboltable* *instk* *stdin* *currin* *outstk*
	*stdout* *currout* *outchanl* *lispdefops* *lisparithexpops*
	*lisplogexpops* *lispstmtops* *lispstmtgpops*))

;;  -----------  ;;
;;  vaxlsp.l     ;;    lisp code generation module
;;  -----------  ;;

(defvar lefttype 'real)
(declare-top (special *float allnum expty oincr onextexp tvname))

;; *float is a flag set to t to cause all constants to be floated

;;                               ;;
;; 2. vaxima -> lisp translation ;;
;;                               ;;

(defun safe-car (x) (if (listp x) (car x) nil))
(defun safe-caar (x) (if (listp x) (car (safe-car x)) nil))

(defun franz (form)
  ; translate form from macsyma internal representation into franz lisp ;
  (foreach f in form collect
	   (cond ((member f '($begin_group $end_group))
		  f)
		 ((macexpp f)
		  ; Following line was (franzexp form 0 form))
		  ; This change corrected failure for gentran(x+y), however
		  ; I am not sure if it is the correct way of fixing this
		  ; problem. It seems unlikely that gentran(x+y) didn't work.
		  (franzexp f 0 f))

;; should have a function to make the choices of 0, 1, 2, 3 or 4 ;;

		 (t
		  (franzstmt f)) )))


(defun franzexp (exp ind context)

  (setq allnum t ) ;;set flag to check if all numbers in an expression
  (cond ((atom exp)

	 (cond ((numberp exp)
		(cond ((equal ind 0)

		       (setq expty (exptype  context ))
		       (cond(allnum (setq expty lefttype)))
				;;solve all numbers in an expression

		       (cond ((eq expty 'integer)
			       exp)
			     ((eq expty 'real)
			      (float exp))
			     ((eq expty 'double)       ;"double" & "complex"
			      (double exp))	       ;are for the time being
			     ((eq expty 'complex)
			      (gcomplex exp))
			     (t (float exp)) ))

		     ((equal ind 1)
		       exp)

		     ((equal ind 2)
		      (float exp))

		     ((equal ind 3)
		      (double exp))

		     ((equal ind 4)
		      (gcomplex exp))))

	       ((char= (char (string exp) 0) #\&)
		(format nil "\"~A\"" (stripdollar1 exp)))
	       ((eq exp t) (cond ((eq *gentranlang 'c) 1)
				 (t '| .true. |)))
	       (t
		(stripdollar1 exp))))

	((eq (caar exp) '$gquote) (cadr exp)) ;; gquote added by pwang 11/10/86
	((eq (caar exp) 'mtimes)
	 (simptimes1 (foreach term in (cdr exp) collect
			      (franzexp term 0 exp))
		      0 ))

	((eq (caar exp) 'mexpt)
	 ; ((mexpt) x -1) --> (quotient 1.0 x)                    ;
	 ; ((mexpt) x ((mminus) i)) --> (quotient 1.0 (expt x i)) ;
	 (let ((var (cadr exp)) (pow (caddr exp)))
	      (cond ((or (eq pow -1)
			 (and (listp pow)
			      (eq (caar pow) 'mminus)
			      (onep (cadr pow))))
		     (list 'quotient (franzexp 1 0 exp) (franzexp var 0 exp)))
		    ((and (numberp pow) (minusp pow))
		     (list 'quotient
			   (franzexp 1 0 exp)
			   (list 'expt (franzexp var 0 exp)
				 (franzexp (abs pow) 1 nil))))
		    ((and (listp pow) (eq (caar pow) 'mminus))
		     (list 'quotient
			   (franzexp 1 0 exp)
			   (list 'expt (franzexp var 0 exp)
				       (franzexp (cadr pow) 1 nil))))
		    (t
		     (list 'expt (franzexp (cadr exp) 0 exp)
				 (franzexp (caddr exp) 1 nil))))))

	((and (and (eq (caar exp) 'mminus) (numberp (cadr exp)))
		    (and (= 1 (length (car exp)))
			 (and (numberp (cadr exp)) (numberp (caddr exp))) ))

	 (cond ((get (caar exp) 'franznotn)
		(cons (get (caar exp) 'franznotn)
		(mapcar (function
			(lambda (elt) (franzexp elt ind context)))
		       (cdr exp))))
		(t
		 (cons (franzexp (caar exp) 1 nil)
		       (mapcar (function
				(lambda (elt) (franzexp elt 1 nil)))
			       (cdr exp)))) )  )
	;; added by Trevor 12/28/86

	((get (caar exp) 'franznotn)
	 (cons (get (caar exp) 'franznotn)
	       (mapcar (function
			(lambda (elt) (franzexp elt ind exp)))
		       (cdr exp))))
	(t
	 (cons (franzexp (caar exp) 1 nil)
	       (mapcar (function
			(lambda (elt) (franzexp elt 1 nil)))
		       (cdr exp))))))
;;	1 is always the right selection?????

;;	Following several functions were added by Trevor 12/86

( defun exptype ( exp )
    ( prog(ty1 ty2)
;;(terpri)
;;(print "enter exptype with")
;;(print exp)
	( cond ( ( null exp ) ( return 'integer ) ) )
	( cond ( ( atom exp ) ( return ( itemtype exp ) ) ) )

	(cond ((and (listp (car exp)) (eq 'array (cadar exp)))
	       (return (exptype (caar exp))) ))

	(cond ((member (car exp)
	       '((mplus) (mminus) (mtimes) (mquotient) (mexpt)) )
	       (setq ty1 'integer))
	      (t (setq ty1 (exptype (car exp)))))

	(setq ty2 (exptype (cdr exp)))
;;(terpri)
;;(print "ty1 -- ")
;;(print ty1)
;;(terpri)
;;(print "ty2 -- ")
;;(print ty2)
	(cond((or (eq ty1 'complex) (eq ty2 'complex))
	      (return 'complex)))

	(cond((or (eq ty1 'double) (eq ty2 'double))
	      (return 'double)))

	(cond((or (eq ty1 'real) (eq ty2 'real))
	      (return 'real)))

	(cond((and (eq ty1 'integer) (eq ty2 'integer))
	      (return 'integer))
	     (t (return 'nil)))  ))


;;	( cond ( ( and ( numberp ( cadr exp ) )
;;		       ( numberp ( caddr exp ) ) )
;;		 ( return expty ) ) )


( defun itemtype ( item )
    ( prog()
;;(print "enter itemtype with")
;;(print item)
	( cond ( ( numberp item )
		 ( cond ( ( floatp item ) ( return 'real ) )
			( t ( return 'integer ) )  ))
	       ( t
		 (setq allnum nil)
			;; set flag to to nil to show
			;; not all numbers in an expression
		 ( return ( getvartype (stripdollar1 item)) ) )  )))

(defun double (num)
    (prog (dnum)
	(cond ((floatp num)
	       (setq dnum (append (explode num) '(d 0)))
	       (return (apply 'concat dnum)))
	      (t (return (intern (format nil "~a.D0" num)))))))

(defun gcomplex (num)
    (prog (cnum)
	(cond ((floatp num)
	       (setq cnum (append (explode num) '( |,| 0 |.| 0 |)|)))
	       (setq cnum (cons '|(| cnum ))
	       (return (apply 'concat cnum)))
	      (t (return (intern (format nil "(~a.0,0.0)" num)))))))

(defun simptimes1 (terms fp)
  (let ((neg) (denoms))
       (setq terms
	     (foreach trm in (simptimes2 terms) conc
		      (cond ((atom trm)
			     (cond ((member trm '(1 1.0)) ())
				   ((member trm '(-1 -1.0)) (setq neg (not neg))
							    ())
				   (t (list trm))))
			    ((and (eq (car trm) 'minus)
				  (member (cadr trm) '(1 1.0)))
			     (setq neg (not neg)) ())
			    ((and (eq (car trm) 'quotient)
				  (member (cadr trm) '(1 1.0)))
			     (setq denoms (aconc denoms (caddr trm))) ())
			    (t (list trm)))))
       (setq terms (or terms (list (franzexp 1 0 terms))))

;; not sure the context here, should we set an ind for *float*?;;

       (cond (neg (setq terms (cons (list 'minus (car terms))
				    (cdr terms)))))
       (setq terms (cond ((onep (length terms)) (car terms))
			 (t (cons 'times terms))))
       (foreach d in denoms do
		(setq terms (list 'quotient terms d)))
       terms))

(defun simptimes2 (terms)
  (foreach trm in terms conc
	   (cond ((atom trm) (list trm))
		 ((eq (car trm) 'times) (simptimes2 (cdr trm)))
		 (t (list trm)))))

(defun franzstmt (stmt)
  ; return the franz lisp equivalent statement ;
  (cond ((member (safe-caar stmt) '( msetq mdo ))
	 (setq lefttype (exptype (cadr stmt))) ))
		;;added by Trevor 12/28/86

  (cond ((null stmt) nil)
	((maclabelp stmt) (franzlabel stmt))
	((macstmtgpp stmt) (franzstmtgp stmt))
	((macdefp stmt) (franzdef stmt))
	((macreadp stmt) (franzread stmt))
	((macmatassignp stmt) (franzmatassign stmt))
	((macnestassignp stmt) (franznestassign stmt))
	((macassignp stmt) (franzassign stmt))
	((macifp stmt) (franzif stmt))
	((macforp stmt) (franzfor stmt))
	((macforinp stmt) (franzforin stmt))
	((macgop stmt) (franzgo stmt))
	((macretp stmt) (franzret stmt))
	((macprintp stmt) (franzprint stmt))
	((macstopp stmt) (franzstop stmt))
	((macendp stmt) (franzend stmt))
	((mac$literalp stmt) (franzliteral (stripdollar1 (caar stmt)) stmt))
	((maccallp stmt) (franzcall stmt))))


(defun mac$literalp (stmt)
  ; is stmt a $literal function? ;
  (member (caar stmt) '($literal literal $data data) :test #'eq))

(defun franzliteral (fn stmt)
  (cons fn
	(foreach exp in (cdr stmt) collect
		 (cond ((member exp '($tab $cr) :test #'eq) exp)
		       ((listp exp) (franzexp exp 0 stmt))
		       (t (stripdollar1 exp))))))

(defun franzlabel (label)
  ; return the franz lisp representation for a label ;
  (stripdollar1 label))

(defun franzstmtgp (stmtgp)
  ; return the franz lisp representation for a statement group ;
  (append '(prog ()) (mapcar 'franzstmt (cdr stmtgp))))

(defun franzdef (def)
  ; return the franz lisp representation for a function definition ;
  ; case 1: ((msetq) id ((lambda) ((mlist) id ... id) exp))        ;
  ;           -->  (defun id (id ... id) (prog () (return exp)))   ;
  ; case 2: ((mdefine) ((id) id ... id) exp)                       ;
  ;           -->  (defun id (id ... id) (prog () (return exp)))   ;
  ; case 3: ((mdefine) ((id) id ... id) stmtgp)                    ;
  ;           -->  (defun id  (id ... id) (prog () stmt ... stmt)) ;
  (cond ((equal (caar def) 'msetq)
	 `(defun ,(franzexp (cadr def) 0 ( cadr def ) )

;; not sure how to change here and below a lot. mainly the ind;;


		 ,(mapcar (function (lambda (elt)
					    (franzexp elt 0 elt)))
			  (cdadaddr def))
		 (prog () (return ,(franzexp (caddaddr def) 0 ( caddaddr def ))))))
	((macexpp (caddr def))
	 `(defun ,(franzexp (caaadr def) 0 ( caaadr def ))
		 ,(mapcar (function (lambda (elt)
					    (franzexp elt 0 elt )))
			  (cdadr def))
		 (prog () (return ,(franzexp (caddr def) 0 ( caddr def ) )))))
	(t
	 `(defun ,(franzexp (caaadr def) 0 ( caaadr def ) )
		 ,(mapcar (function (lambda (elt)
					    (franzexp elt 0 elt )))
			  (cdadr def))
		 ,(franzstmt (caddr def))))))

(defun franzread (stmt)
  ; return the franz lisp representation for a read statement ;
  (let (varlist outlist fr)
       (setq varlist nil)
       (do ((s stmt (caddr s)))
	   ((or (null s) (atom s) (not (macstmtp s))))
	   (cond ((equal (caar s) 'msetq)
		  (setq varlist (cons (franzexp (cadr s) 0 ( cadr s ) )
				      varlist)))
		 (t
		  (setq outlist
			(mapcar (function (lambda (elt)
						  (franzexp elt 0 elt )))
				(cdr s))))))
       (setq fr nil)
       (cond (outlist (setq fr (append1 fr (cons 'princ outlist)))))
       (cond (varlist (setq fr (append1 fr `(setq ,(car varlist) (read))))))
       (do ((v varlist (cdr v)))
	   ((null (cdr v)))
	   (setq fr (append1 fr `(setq ,(cadr v) ,(car v)))))
       (cond ((> (length fr) 1) (cons 'progn fr))
	     (t (car fr)))))

(defun franzmatassign (stmt)
  ; return the franz lisp representation for a matrix assignment statement ;
  (do ((rows (cdaddr stmt) (cdr rows)) (r 1 (1+ r)) (fr (list 'progn)))
      ((null rows) fr)
      (do ((cols (cdar rows) (cdr cols)) (c 1 (1+ c)))
	  ((null cols))
	  (setq fr (append1 fr (list 'setq
				     (franzexp (list (list (cadr stmt)) r c)
					     0  (list (list (cadr stmt)) r c))
				     (franzexp (car cols)
					     0  (car cols) )))))))

(defun franznestassign (stmt)
  ; return the franz lisp representation for a nested assignment statement ;
  (let (varlist exp fr)
       (do ((s stmt (caddr s)))
	   ((or (atom s) (not (macstmtp s)))
	    (setq exp (franzexp s 0 s )))
	   (setq varlist (cons (franzexp (cadr s) 0 ( cadr s )) varlist)))
       (setq fr `(progn (setq ,(car varlist) ,exp)))
       (do ((v varlist (cdr v)))
	   ((null (cdr v)))
	   (setq fr (append1 fr `(setq ,(cadr v) ,(car v)))))
       fr))

(defun franzassign (stmt)
  ; return the franz lisp representation for an assignment statement ;
  `(setq ,(franzexp (cadr stmt) 0 ( cadr stmt ))
	 ,(franzexp (caddr stmt) 0 ( caddr stmt ) )))

(defun franzif (stmt)
  ; return the franz lisp representation for an if statement ;
  (destructuring-bind (x exp stmt1 y stmt2) stmt
   (let ((fr '(cond)))
        (setq fr (append1 fr (list (franzexp exp 0 exp)
				   (franzstmt stmt1))))
        (cond ((not (equal stmt2 '$false))
	       (append1 fr (list 't (franzstmt stmt2))))
	      (t
	       fr)))))

(defun franzfor (stmt)
  ; return the franz lisp representation for a for statement      ;
  ; ((mdo) var lo incr nextexp hi exitcond dobody)                ;
  ;   -->  (do ((var lo (+ var incr))  =or=  (var lo nextexp)) ;
  ;            ((or (> var hi) exitcond))                  ;
  ;            dobody)                                            ;
  (destructuring-bind (var lo incr nextexp hi exitcond dobody) (cdr stmt)
   (let (dovars doexit posincr)
       (setq oincr    incr
	     onextexp nextexp)
       (setq var      (franzexp var 0 var )
	     lo       (franzexp lo 0 lo )
	     incr     (franzexp incr 0 incr )
	     nextexp  (franzexp nextexp 0 nextexp )
	     hi       (franzexp hi 0 hi )
	     exitcond (franzexp exitcond 0 exitcond )
	     dobody   (franzstmt dobody))
       (cond ((and (not var) (or lo incr nextexp hi))
	      (setq tvname tempvarname*)
	      (setq tempvarname* 'i)
	      (setq var ($tempvar nil))
	      (setq tempvarname* tvname)))
       (cond ((and (not lo) (or var incr hi))
	      (setq lo 1)))
       (cond ((and (not incr) (not nextexp) (or var lo hi))
	      (setq incr 1)))
       (cond (incr
	      (cond ((or (null (getvartype var))
			 (inttypep (getvartype var)))
		     (cond ((numberp lo) (setq lo (floor lo))))
		     (cond ((numberp hi) (setq hi (floor hi))))
		     (cond ((numberp incr) (setq incr (floor incr))))))
	      (setq dovars `((,var ,lo (plus ,var ,incr))))))
       (cond (nextexp
	      (setq dovars `((,var ,lo ,nextexp)))))
       (cond (hi
	      (cond (nextexp
		     (setq posincr (noerrmevalp '((mgeqp) onextexp 0))))
		    (t
		     (setq posincr (noerrmevalp '((mgeqp) oincr 0)))))
	      (cond (posincr
		     (setq doexit `((greaterp ,var ,hi))))
		    (t
		     (setq doexit `((lessp ,var ,hi)))))))
       (cond (exitcond (setq doexit (append1 doexit exitcond))))
       (cond ((> (length doexit) 1)
	      (setq doexit (list (cons 'or doexit)))))
       `(do ,dovars ,doexit ,dobody))))

(defun franzforin (stmt)
  ; return the franz lisp representation for a for-in statement             ;
  ; ((mdoin) dovar dolist nil nil nil doexitcond dobody)                    ;
  ;   -->  (do ((genvar 1 (+ genvar 1)))                                 ;
  ;            ((> genvar listlength))                               ;
  ;            (cond ((equal genvar 1) (setq dovar list(1)))                ;
  ;                  ((equal genvar 2) (setq dovar list(2)))                ;
  ;                    .                                                    ;
  ;                    .                                                    ;
  ;                  ((equal genvar listlength) (setq dovar list(length)))) ;
  ;            (cond ((doexitcond) (break)))                                ;
  ;            dobody)                                                      ;
  (let ((gvar) condbody)
    (destructuring-bind (dovar (x . dolist) x x x doexitcond dobody) (cdr stmt)
       (setq tvname tempvarname*)
       (setq tempvarname* 'i)
       (setq gvar ($tempvar nil))
       (setq tempvarname* tvname)
       (setq dovar (franzexp dovar 0 dovar ))
       (do ((i 1 (1+ i)))
	   ((> i (length dolist)))
	   (setq condbody
		 (append condbody
			 `(((equal ,gvar ,i)
			    (setq ,dovar ,(franzexp (nthelem i dolist)
						  0  (nthelem i dolist))))))))
       (cond (doexitcond
	      `(do ((,gvar 1 (+ ,gvar 1)))
		   ((> ,gvar ,(length dolist)))
		   (progn
		    ,(cons 'cond condbody)
		    (cond (,(franzexp doexitcond 0 doexitcond ) (break)))
		    ,(franz dobody))))
	     (t
	      `(do ((,gvar 1 (+ ,gvar 1)))
		   ((> ,gvar ,(length dolist)))
		   (progn
		    ,(cons 'cond condbody)
		    ,(franz dobody))))))))

(defun franzgo (stmt)
  ; return the franz lisp representation for a go statement ;
  `(go ,(franzlabel (cadr stmt))))

(defun franzret (stmt)
  ; return the franz lisp representation for a return statement ;
  (cond ((cdr stmt) `(return ,(franzexp (cadr stmt) 0 ( cadr stmt ) )))
	(t '(return))))

(defun franzprint (stmt)
  ; return the franz lisp representation for a print statement ;
  (cons 'princ
	(mapcar (function (lambda (elt)
				  (franzexp elt 0 elt )))
		(cdr stmt))))

(defun franzstop (stmt)
  ; return the franz lisp representation for a stop statement ;
  '(stop))

(defun franzend (stmt)
  ; return the franz lisp representation for an end statement ;
  '(end))

(defun franzcall (exp)
  ; return the franz lisp representation for a call statement ;
  (cond ((cdr exp) (cons (franzexp (caar exp) 0 ( caar exp ))
			 (mapcar (function (lambda (elt)
						   (franzexp elt
							    0 elt )))
				 (cdr exp))))
	(t (list (franzexp (caar exp) 0 ( caar exp ) )))))




(defun macexpp (exp)
  ; is exp an arithmetic or logical macsyma expression? ;
  (cond ((null exp) nil)
	((atom exp))
	((atom (car exp)) nil)
	((not (member (caar exp) '(mcond mdefine mdo mdoin mgo mprog mprogn
				 mreturn msetq $end $ev $literal $print
				 $readonly $stop $data) :test #'eq)))))

(defun maclogexpp (exp)
  ; is exp a macsyma logical expression? ;
  (cond ((atom exp)
	 (not (numberp exp)))
	((listp (car exp))
	 (not (member (caar exp)
		    '(mcond mdefine mdo mdoin mgo mexpt mminus mplus mprog
		      mprogn mquotient mreturn msetq mtimes rat $end $ev
		      $print $readonly $stop) :test #'eq)))))

(defun macstmtp (stmt)
  ; is stmt a macsyma statement? ;
  (cond ((null stmt) nil)
	((atom stmt))
	((atom (car stmt)) nil)
	((member (caar stmt) '(mcond mdo mdoin mgo mreturn msetq $end $print
			       $readonly $stop))
	 t)))

(defun macstmtgpp (stmt)
  ; is stmt a macsyma statement group? ;
  (cond ((or (null stmt) (atom stmt) (atom (car stmt))) nil)
	((member (caar stmt) '(mprog mprogn $ev)) t)))

(defun macdefp (stmt)
  ; is stmt a macsyma function or procedure definition? ;
  (cond ((or (null stmt) (atom stmt) (atom (car stmt))) nil)
	((or (equal (caar stmt) 'mdefine)
	     (and (equal (caar stmt) 'msetq)
		  (listp (caddr stmt))
		  (listp (caaddr stmt))
		  (equal (caaaddr stmt) 'lambda))))))


(defun macassignp (stmt)
  ; is stmt a macsyma assignment statement? ;
  (equal (safe-caar stmt) 'msetq))

(defun macnestassignp (stmt)
  ; is stmt a macsyma nested assignment statement? ;
  (and (macassignp stmt)
       (listp (caddr stmt))
       (listp (caaddr stmt))
       (macassignp (caddr stmt))))

(defun macmatassignp (stmt)
  ; is stmt a macsyma matrix assignment statement? ;
  (cond ((or (null stmt) (atom stmt) (atom (car stmt))) nil)
	((equal (caar stmt) '$matrix))
	((equal (caar stmt) 'msetq)
	 (macmatassignp (caddr stmt)))))

(defun macifp (stmt)
  ; is stmt a macsyma if-then or if-then-else statement? ;
  (equal (safe-caar stmt) 'mcond))

(defun macforp (stmt)
  ; is stmt a macsyma for-loop? ;
  (equal (safe-caar stmt) 'mdo))

(defun macforinp (stmt)
  ; is stmt a macsyma for-in-loop? ;
  (equal (safe-caar stmt) 'mdoin))

(defun macgop (stmt)
  ; is stmt a macsyma go statement? ;
  (equal (safe-caar stmt) 'mgo))

(defun maclabelp (stmt)
  ; is stmt a macsyma statement label? ;
  (atom stmt))


(defun maccallp (stmt)
  ; is stmt a macsyma call statement? ;
  t)

(defun macretp (stmt)
  ; is stmt a macsyma return statement? ;
  (equal (safe-caar stmt) 'mreturn))

(defun macreadp (stmt)
  ; is stmt a macsyma read statement? ;
  (cond ((or (null stmt) (atom stmt) (atom (car stmt))) nil)
	((equal (safe-caar stmt) '$readonly))
	((equal (safe-caar stmt) 'msetq)
	 (macreadp (caddr stmt)))))

(defun macprintp (stmt)
  ; is stmt a macsyma print statement? ;
  (equal (safe-caar stmt) '$print))

(defun macstopp (stmt)
  ; is stmt a macsyma stop statement? ;
  (equal (safe-caar stmt) '$stop))

(defun macendp (stmt)
  ; is stmt a macsyma end statement? ;
  (equal (safe-caar stmt) '$end))
