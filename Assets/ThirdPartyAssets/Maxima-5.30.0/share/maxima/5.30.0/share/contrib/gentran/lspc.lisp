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
;;  ---------  ;;
;;  lspc.l     ;;    lisp-to-c translation module
;;  ---------  ;;

(put nil '*cname* 0)

(put 'or       '*cprecedence* 1)
(put 'and      '*cprecedence* 2)
(put 'equal    '*cprecedence* 3)
(put 'notequal '*cprecedence* 3)
(put 'greaterp '*cprecedence* 4)
(put 'geqp     '*cprecedence* 4)
(put 'lessp    '*cprecedence* 4)
(put 'leqp     '*cprecedence* 4)
(put 'plus     '*cprecedence* 5)
(put 'times    '*cprecedence* 6)
(put 'quotient '*cprecedence* 6)
(put 'not      '*cprecedence* 7)
(put 'minus    '*cprecedence* 7)
(put 'and      '*cop* '|&&|)
(put 'not      '*cop* '|!|)
(put 'equal    '*cop* '|==|)
(put 'notequal '*cop* '|!=|)
(put 'greaterp '*cop* '|>|)
(put 'geqp     '*cop* '|>=|)
(put 'lessp    '*cop* '|<|)
(put 'leqp     '*cop* '|<=|)
(put 'plus     '*cop* '|+|)
(put 'times    '*cop* '|*|)
(put 'quotient '*cop* '|//|)
(put 'minus    '*cop* '|-|)
(put 'or       '*cop* "||")
;;                                  ;;
;;  lisp-to-c transltion functions  ;;
;;                                  ;;


;;  control function  ;;

(defun ccode (forms)
  (foreach f in forms conc
	   (cond ((atom f)
		  (cond ((equal f '$begin_group) (mkfcbegingp))
			((equal f '$end_group) (mkfcendgp))
			(t (cexp f))))
		 ((or (lispstmtp f) (lispstmtgpp f))
		  (cond (*gendecs (prog (r)
					(setq r
					      (append
					       (cdecs (symtabget '*main*
								 '*decs*))
					       (cstmt f)))
					(symtabrem '*main* '*decs*)
					(return r)))
			(t (cstmt f))))
		 ((lispdefp f) (cproc f))
		 (t (cexp f)))))

;;  procedure translation  ;;

(defun cproc (def)
  (prog (type name params paramtypes vartypes body r)
	(setq name (cadr def))
	(setq body (cdddr def))
	(cond ((and body (equal body '(nil))) (setq body ())))
	(cond ((and (onep (length body))
		    (lispstmtgpp (car body)))
	       (progn
		(setq body (cdar body))
		(cond ((null (car body))
		       (setq body (cdr body)))))))
	(cond ((setq type (symtabget name name))
	       (progn
		(setq type (cadr type))
		(symtabrem name name))))
	(setq params (or (symtabget name '*params*) (caddr def)))
	(symtabrem name '*params*)
	(foreach dec in (symtabget name '*decs*) do
		 (cond ((member (car dec) params)
			(setq paramtypes (aconc paramtypes dec)))
		       (t
			(setq vartypes (aconc vartypes dec)))))
	(setq r (append (mkfcprocdec type name params)
			(cdecs paramtypes)))
	(cond (body
	       (setq r (append r (mkfcbegingp)))
	       (indentclevel (+ 1))
	       (cond (*gendecs (setq r (append r (cdecs vartypes)))))
	       (setq r (append r (foreach s in body conc (cstmt s))))
	       (indentclevel (- 1))
	       (setq r (append r (mkfcendgp)))))
	(cond (*gendecs
	       (progn
		(symtabrem name nil)
		(symtabrem name '*decs*))))
	(return r)))

;;  generation of declarations  ;;

(defun cdecs (decs)
  (foreach tl in (formtypelists decs) conc (mkfcdec (car tl) (cdr tl))))

;;  expression translation  ;;

(defun cexp (exp)
  (cexp1 exp 0))

(defun cexp1 (exp wtin)
  (cond ((atom exp) (list (cname exp)))
	((eq (car exp) 'literal) (cliteral exp))
	((member (car exp) '(minus not) :test #'eq)
	 (let* ((wt (cprecedence (car exp)))
		(res (cons (cop (car exp)) (cexp1 (cadr exp) wt))))
	       (cond ((< wt wtin) (aconc (cons '|(| res) '|)| ))
		     (t res))))
	((eq (car exp) 'expt)
	 (append (cons 'power (cons '|(| (cexp1 (cadr exp) 0)))
                 (aconc (cons '|,| (cexp1 (caddr exp) 0)) '|)| )))
	((or (member (car exp) *lisparithexpops* :test #'eq)
	     (member (car exp) *lisplogexpops* :test #'eq))
	 (let* ((wt (cprecedence (car exp)))
		(op (cop (car exp)))
		(res (cexp1 (cadr exp) wt))
		(res1))
	       (setq exp (cdr exp))
	       (cond ((eq op '+)
		      (while (setq exp (cdr exp))
                         (progn
			  (setq res1 (cexp1 (car exp) wt))
			  (cond ((or (eq (car res1) '-)
				     (and (numberp (car res1))
					  (minusp (car res1))))
				 (setq res (append res res1)))
				(t
				 (setq res (append res (cons op res1))))))))
		     (t
		      (while (setq exp (cdr exp))
                         (setq res (append res
					   (cons op
						 (cexp1 (car exp) wt)))))))
	       (cond ((< wt wtin) (aconc (cons '|(| res) '|)| ))
		     (t res))))
	((arrayeltp exp)
	 (let ((res (list (car exp))))
	      (while (setq exp (cdr exp))
                 (setq res (append res
				   (aconc (cons '|[| (cexp1 (car exp) 0)) '|]|))))
	      res))
	((onep (length exp)) (aconc (aconc exp '|(|) '|)| ))
	(t
	 (let ((res (cons (car exp) (cons '|(| (cexp1 (cadr exp) 0)))))
              (setq exp (cdr exp))
	      (while (setq exp (cdr exp))
                 (setq res (append res (cons '|,| (cexp1 (car exp) 0)))))
              (aconc res '|)|)))))

(defun cname (name)
  (or (get name '*cname*) name))

(defun cop (op)
  (or (get op '*cop*) op))

(defun cprecedence (op)
  (or (get op '*cprecedence*) 8))

;;  statement translation  ;;

(defun cstmt (stmt)
  (cond ((null stmt) nil)
	((equal stmt '$begin_group) (mkfcbegingp))
	((equal stmt '$end_group) (mkfcendgp))
	((lisplabelp stmt) (clabel stmt))
	((equal (car stmt) 'literal) (cliteral stmt))
	((lispassignp stmt) (cassign stmt))
	((lispcondp stmt) (cif stmt))
	((lispbreakp stmt) (cbreak stmt))
	((lispgop stmt) (cgoto stmt))
	((lispreturnp stmt) (creturn stmt))
	((lispstopp stmt) (cexit stmt))
	((lispdop stmt) (cloop stmt))
	((lispstmtgpp stmt) (cstmtgp stmt))
	((lispdefp stmt) (cproc stmt))
	(t (cexpstmt stmt))))

(defun cassign (stmt)
  (mkfcassign (cadr stmt) (caddr stmt)))

(defun cbreak (stmt)
  (mkfcbreak))

(defun cexit (stmt)
  (mkfcexit))

(defun cexpstmt (exp)
  (append (cons (mkctab) (cexp exp))
	  (list '|;| (mkterpri))))

(defun cfor (var lo nextexp cond body)
  (prog (r)
	(cond (cond (setq cond (list 'not cond))))
	(cond ((equal nextexp '(nil))
	       (setq r (mkfcfor var lo cond var nil)))
	      (nextexp
	       (setq r (mkfcfor var lo cond var nextexp)))
	      (t
	       (setq r (mkfcfor var lo cond nil nil))))
	(indentclevel (+ 1))
	(setq r (append r (cstmt body)))
	(indentclevel (- 1))
	(return r)))

(defun cgoto (stmt)
  (mkfcgo (cadr stmt)))

(defun cif (stmt)
  (prog (r st)
	(setq r (mkfcif (caadr stmt)))
	(indentclevel (+ 1))
	(setq st (seqtogp (cdadr stmt)))
	(cond ((and (listp st)
		    (equal (car st) 'cond)
		    (equal (length st) 2))
	       (setq st (mkstmtgp 0 (list st)))))
	(setq r (append r (cstmt st)))
	(indentclevel (- 1))
	(setq stmt (cdr stmt))
	(while (and (setq stmt (cdr stmt))
		    (not (eq (caar stmt) t)))
	       (progn
		(setq r (append r (mkfcelseif (caar stmt))))
		(indentclevel (+ 1))
		(setq st (seqtogp (cdar stmt)))
		(cond ((and (listp st)
			    (equal (car st) 'cond)
			    (equal (length st) 2))
		       (setq st (mkstmtgp 0 (list st)))))
		(setq r (append r (cstmt st)))
		(indentclevel (- 1))))
	(cond (stmt (progn
		     (setq r (append r (mkfcelse)))
		     (indentclevel (+ 1))
		     (setq st (seqtogp (cdar stmt)))
		     (cond ((and (listp st)
				 (equal (car st) 'cond)
				 (equal (length st) 2))
			    (setq st (mkstmtgp 0 (list st)))))
		     (setq r (append r (cstmt st)))
		     (indentclevel (- 1)))))
	(return r)))

(defun clabel (label)
  (mkfclabel label))

(defun cliteral (stmt)
  (mkfcliteral (cdr stmt)))

(defun cloop (stmt)
  (prog (var lo nextexp exitcond body)
	(cond ((complexdop stmt)
	       (return (cstmt (seqtogp (simplifydo stmt))))))
	(cond ((setq var (cadr stmt))
	       (progn
		(setq lo (cadar var))
		(cond ((equal (length (car var)) 3)
		       (setq nextexp (or (caddar var) (list 'nil)))))
		(setq var (caar var)))))
	(cond ((setq exitcond (caddr stmt))
	       (setq exitcond (car exitcond))))
	(setq body (seqtogp (cdddr stmt)))
	(cond ((and exitcond (not var))
	       (return (cwhile exitcond body)))
	      ((and var
		    (not lo)
		    (lisplogexpp nextexp)
		    (equal exitcond var))
	       (return (crepeat body nextexp)))
	      (t
	       (return (cfor var lo nextexp exitcond body))))))

(defun crepeat (body logexp)
  (prog (r)
	(setq r (mkfcdo))
	(indentclevel (+ 1))
	(setq r (append r (cstmt body)))
	(indentclevel (- 1))
	(return (append r (mkfcdowhile (list 'not logexp))))))

(defun creturn (stmt)
  (mkfcreturn (cadr stmt)))

(defun cstmtgp (stmtgp)
  (prog (r)
	(cond ((equal (car stmtgp) 'progn)
	       (setq stmtgp (cdr stmtgp)))
	      (t
	       (setq stmtgp (cddr stmtgp))))
	(setq r (mkfcbegingp))
	(indentclevel (+ 1))
	(setq r (append r (foreach stmt in stmtgp conc (cstmt stmt))))
	(indentclevel (- 1))
	(return (append r (mkfcendgp)))))

(defun cwhile (cond body)
  (prog (r)
	(cond (cond (setq cond (list 'not cond))))
	(setq r (mkfcwhile cond))
	(indentclevel (+ 1))
	(setq r (append r (cstmt body)))
	(indentclevel (- 1))
	(return r)))


;;                               ;;
;;  c code formatting functions  ;;
;;                               ;;


;;  statement formatting  ;;

(defun mkfcassign (lhs rhs)
  (append (append (cons (mkctab) (cexp lhs))
		  (cons '= (cexp rhs)))
	  (list '|;| (mkterpri))))

(defun mkfcbegingp ()
  (list (mkctab) '{ (mkterpri)))

(defun mkfcbreak ()
  (list (mkctab) 'break '|;| (mkterpri)))

(defun mkfcdec (type varlist)
  (progn
   (setq varlist
	 (foreach v in varlist collect
		  (cond ((atom v) v)
			(t (cons (car v)
				 (foreach dim in (cdr v) collect
					  (1+ dim)))))))
   (append (cons (mkctab)
		 (cons type
		       (cons '| |  (foreach v in (insertcommas varlist) conc
					  (cexp v)))))
	   (list '|;| (mkterpri)))))

(defun mkfcdo ()
  (list (mkctab) 'do (mkterpri)))

(defun mkfcdowhile (exp)
  (append (append (list (mkctab) 'while '| | '|(|)
		  (cexp exp))
	  (list '|)| '|;| (mkterpri))))

(defun mkfcelse ()
  (list (mkctab) 'else (mkterpri)))

(defun mkfcelseif (exp)
  (append (append (list (mkctab) 'else '| |  'if '| |  '|(|) (cexp exp))
		  (list '|)| (mkterpri))))

(defun mkfcendgp ()
  (list (mkctab) '} (mkterpri)))

(defun mkfcexit ()
  (list (mkctab) 'exit '|(| 0 '|)| '|;| (mkterpri)))

(defun mkfcfor (var1 lo cond var2 nextexp)
  (progn
   (cond (var1 (setq var1 (append (cexp var1) (cons '= (cexp lo))))))
   (cond (cond (setq cond (cexp cond))))
   (cond (var2 (setq var2 (append (cexp var2) (cons '= (cexp nextexp))))))
   (append (append (append (list (mkctab) 'for '| |  '|(|) var1)
		   (cons '|;| cond))
	   (append (cons '|;| var2)
		   (list '|)| (mkterpri))))))

(defun mkfcgo (label)
  (list (mkctab) 'goto '| |  label '|;| (mkterpri)))

(defun mkfcif (exp)
  (append (append (list (mkctab) 'if '| |  '|(|)
		  (cexp exp))
	  (list '|)| (mkterpri))))

(defun mkfclabel (label)
  (list label '|:| (mkterpri)))

(defun mkfcliteral (args)
  (foreach a in args conc
	   (cond ((equal a '$tab) (list (mkctab)))
		 ((equal a '$cr) (list (mkterpri)))
		 ((listp a) (cexp a))
		 (t (list a)))))

(defun mkfcprocdec (type name params)
  (progn
   (setq params
	 (aconc (cons '|(| (foreach p in (insertcommas params) conc
				   (cexp p)))
		'|)|))
   (cond (type (append (cons (mkctab) (cons type (cons '| |  (cexp name))))
		       (aconc params (mkterpri))))
	 (t (append (cons (mkctab) (cexp name))
		    (aconc params (mkterpri)))))))

(defun mkfcreturn (exp)
  (cond (exp
	 (append (append (list (mkctab) 'return '|(|) (cexp exp))
		 (list '|)| '|;| (mkterpri))))
	(t
	 (list (mkctab) 'return '|;| (mkterpri)))))

(defun mkfcwhile (exp)
  (append (append (list (mkctab) 'while '| |  '|(|)
		  (cexp exp))
	  (list '|)| (mkterpri))))

;;  indentation control  ;;

(defun mkctab ()
  (list 'ctab ccurrind*))

(defun indentclevel (n)
  (setq ccurrind* (+ ccurrind* (* n tablen*))))
