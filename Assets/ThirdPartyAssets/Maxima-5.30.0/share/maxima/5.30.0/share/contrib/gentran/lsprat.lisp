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
;;  lsprat.l     ;;    lisp-to-ratfor translation module
;;  -----------  ;;

(put nil '*ratforname* ".false.")
(put 'or       '*ratforprecedence* 1)
(put 'and      '*ratforprecedence* 2)
(put 'not      '*ratforprecedence* 3)
(put 'equal    '*ratforprecedence* 4)
(put 'notequal '*ratforprecedence* 4)
(put 'greaterp '*ratforprecedence* 4)
(put 'geqp     '*ratforprecedence* 4)
(put 'lessp    '*ratforprecedence* 4)
(put 'leqp     '*ratforprecedence* 4)
(put 'plus     '*ratforprecedence* 5)
(put 'times    '*ratforprecedence* 6)
(put 'quotient '*ratforprecedence* 6)
(put 'minus    '*ratforprecedence* 7)
(put 'expt     '*ratforprecedence* 8)
(put 'or       '*ratforop* "||")
(put 'and      '*ratforop* '|&|)
(put 'not      '*ratforop* '|!|)
(put 'equal    '*ratforop* '|==|)
(put 'notequal '*ratforop* '|!=|)
(put 'greaterp '*ratforop* '|>|)
(put 'geqp     '*ratforop* '|>=|)
(put 'lessp    '*ratforop* '|<|)
(put 'leqp     '*ratforop* '|<=|)
(put 'plus     '*ratforop* '|+|)
(put 'times    '*ratforop* '|*|)
(put 'quotient '*ratforop* '|//|)
(put 'expt     '*ratforop* '|**|)
(put 'minus    '*ratforop* '|-|)

;;                                        ;;
;;  lisp-to-ratfor translation functions  ;;
;;                                        ;;


;;  control function  ;;

(defun ratcode (forms)
  (foreach f in forms conc
	   (cond ((atom f)
		  (cond ((equal f '$begin_group)
			 (mkfratbegingp))
                        ((equal f '$end_group)
			 (mkfratendgp))
                        (t
			 (ratexpgen f))))
		 ((or (lispstmtp f) (lispstmtgpp f))
		  (cond (*gendecs
			 (prog (r)
			       (setq r (append (ratdecs (symtabget '*main*
								   '*decs*))
					       (ratstmt f)))
			       (symtabrem '*main* '*decs*)
			       (return r)))
			(t
			 (ratstmt f))))
		 ((lispdefp f)
		  (ratsubprog f))
		 (t
		  (ratexpgen f)))))

;;  subprogram translation  ;;

(defun ratsubprog (def)
  (prog (type stype name params body lastst r)
	(setq name (cadr def))
	(setq body (cdddr def))
	(cond ((and body (equal body '(nil))) (setq body ())))
	(cond ((and (onep (length body))
		    (lispstmtgpp (car body)))
	       (progn
		(setq body (cdar body))
		(cond ((null (car body))
		       (setq body (cdr body)))))))
	(cond (body
	       (cond ((lispreturnp (setq lastst (car (reverse body))))
		      (setq body (aconc body '(end))))
		     ((not (lispendp lastst))
		      (setq body (append body (list '(return) '(end))))))))
	(cond ((setq type (symtabget name name))
	       (progn
		(setq type (cadr type))
		(symtabrem name name))))
	(setq stype (or (symtabget name '*type*)
			(cond ((or type (gfunctionp body name))
			       'function)
			      (t
			       'subroutine))))
	(symtabrem name '*type*)
	(setq params (or (symtabget name '*params*) (caddr def)))
	(symtabrem name '*params*)
	(setq r (mkfratsubprogdec type stype name params))
	(cond (*gendecs
	       (setq r (append r (ratdecs (symtabget name '*decs*))))))
	(setq r (append r (foreach s in body conc (ratstmt s))))
	(cond (*gendecs
	       (progn
		(symtabrem name nil)
		(symtabrem name '*decs*))))
	(return r)))

;;  generation of declarations  ;;

(defun ratdecs (decs)
  (foreach tl in (formtypelists decs) conc (mkfratdec (car tl) (cdr tl))))

;;  expression translation  ;;

(defun ratexpgen (exp)
  (ratexpgen1 exp 0))

(defun ratexpgen1 (exp wtin)
  (cond ((atom exp) (list (ratforname exp)))
	((eq (car exp) 'literal) (ratliteral exp))
	((onep (length exp)) exp)
	((member (car exp) '(minus not) :test #'eq)
	 (let* ((wt (ratforprecedence (car exp)))
		(res (cons (ratforop (car exp)) (ratexpgen1 (cadr exp) wt))))
	       (cond ((< wt wtin) (aconc (cons '|(| res) '|)|))
		     (t res))))
	((or (member (car exp) *lisparithexpops* :test #'eq)
	     (member (car exp) *lisplogexpops* :test #'eq))
	 (let* ((wt (ratforprecedence (car exp)))
		(op (ratforop (car exp)))
		(res (ratexpgen1 (cadr exp) wt))
		(res1))
	       (setq exp (cdr exp))
	       (cond ((eq op '+)
		      (while (setq exp (cdr exp))
                         (progn
			  (setq res1 (ratexpgen1 (car exp) wt))
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
						 (ratexpgen1 (car exp) wt)))))))
	       (cond ((< wt wtin) (aconc (cons '|(| res) '|)|))
		     (t res))))
	(t
	 (let ((res (cons (car exp) (cons '|(| (ratexpgen1 (cadr exp) 0)))))
              (setq exp (cdr exp))
	      (while (setq exp (cdr exp))
                 (setq res (append res (cons '|,| (ratexpgen1 (car exp) 0)))))
              (aconc res '|)| )))))

(defun ratforname (name)
  (or (get name '*ratforname*) name))

(defun ratforop (op)
  (or (get op '*ratforop*) op))

(defun ratforprecedence (op)
  (or (get op '*ratforprecedence*) 9))

;;  statement translation  ;;

(defun ratstmt (stmt)
  (cond ((null stmt) nil)
	((equal stmt '$begin_group) (mkfratbegingp))
	((equal stmt '$end_group) (mkfratendgp))
	((lisplabelp stmt) (ratstmtno stmt))
	((equal (car stmt) 'literal) (ratliteral stmt))
	((lispreadp stmt) (ratread stmt))
	((lispassignp stmt) (ratassign stmt))
	((lispprintp stmt) (ratwrite stmt))
	((lispcondp stmt) (ratif stmt))
	((lispbreakp stmt) (ratbreak stmt))
	((lispgop stmt) (ratgoto stmt))
	((lispreturnp stmt) (ratreturn stmt))
	((lispstopp stmt) (ratstop stmt))
	((lispendp stmt) (ratend stmt))
	((lispdop stmt) (ratloop stmt))
	((lispstmtgpp stmt) (ratstmtgp stmt))
	((lispdefp stmt) (ratsubprog stmt))
	((lispcallp stmt) (ratcall stmt))))

(defun ratassign (stmt)
  (mkfratassign (cadr stmt) (caddr stmt)))

(defun ratbreak (stmt)
  (mkfratbreak))

(defun ratcall (stmt)
  (mkfratcall (car stmt) (cdr stmt)))

(defun ratdo (var lo nextexp exitcond body)
  (prog (r hi incr)
	(setq hi
	      (car (delete1 'greaterp (delete1 'lessp (delete1 var exitcond)))))
	(setq incr (car (delete1 'plus (delete1 var nextexp))))
	(setq r (mkfratdo var lo hi incr))
	(indentratlevel (+ 1))
	(setq r (append r (ratstmt body)))
	(indentratlevel (- 1))
	(return r)))

(defun ratend (stmt)
  (mkfratend))

(defun ratforfor (var lo nextexp cond body)
  (prog (r)
	(cond (cond
	       (setq cond (list 'not cond))))
	(cond ((equal nextexp '(nil))
	       (setq r (mkfratfor var lo cond var nil)))
	      (nextexp
	       (setq r (mkfratfor var lo cond var nextexp)))
	      (t
	       (setq r (mkfratfor var lo cond nil nil))))
	(indentratlevel (+ 1))
	(setq r (append r (ratstmt body)))
	(indentratlevel (- 1))
	(return r)))

(defun ratgoto (stmt)
  (prog (stmtno)
	(setq stmtno (or (get (cadr stmt) '*stmtno*)
			 (put (cadr stmt) '*stmtno* (genstmtno))))
	(return (mkfratgo stmtno))))

(defun ratif (stmt)
  (prog (r st)
	(setq r (mkfratif (caadr stmt)))
	(indentratlevel (+ 1))
	(setq st (seqtogp (cdadr stmt)))
	(cond ((and (listp st)
		    (equal (car st) 'cond)
		    (equal (length st) 2))
	       (setq st (mkstmtgp 0 (list st)))))
	(setq r (append r (ratstmt st)))
	(indentratlevel (- 1))
	(setq stmt (cdr stmt))
	(while (and (setq stmt (cdr stmt))
		    (not (eq (caar stmt) t)))
	       (progn
		(setq r (append r (mkfratelseif (caar stmt))))
		(indentratlevel (+ 1))
		(setq st (seqtogp (cdar stmt)))
		(cond ((and (listp st)
			    (equal (car st) 'cond)
			    (equal (length st) 2))
		       (setq st (mkstmtgp 0 (list st)))))
		(setq r (append r (ratstmt st)))
		(indentratlevel (- 1))))
	(cond (stmt
	       (progn
		(setq r (append r (mkfratelse)))
		(indentratlevel (+ 1))
		(setq st (seqtogp (cdar stmt)))
		(cond ((and (listp st)
			    (equal (car st) 'cond)
			    (equal (length st) 2))
		       (setq st (mkstmtgp 0 (list st)))))
		(setq r (append r (ratstmt st)))
		(indentratlevel (- 1)))))
	(return r)))

(defun ratliteral (stmt)
  (mkfratliteral (cdr stmt)))

(defun ratloop (stmt)
  (prog (var lo nextexp exitcond body r)
	(cond ((complexdop stmt)
	       (return (ratstmt (seqtogp (simplifydo stmt))))))
	(cond ((setq var (cadr stmt))
	       (progn
		(setq lo (cadar var))
		(cond ((equal (length (car var)) 3)
		       (setq nextexp (or (caddar var) (list 'nil)))))
		(setq var (caar var)))))
	(cond ((setq exitcond (caddr stmt))
	       (setq exitcond (car exitcond))))
	(setq body (seqtogp (cdddr stmt)))
	(cond ((and var
		    lo
		    (equal (car nextexp) 'plus)
		    (member var nextexp)
		    (member (car exitcond) '(greaterp lessp))
		    (member var exitcond))
	       (return (ratdo var lo nextexp exitcond body)))
	      ((and exitcond
		    (not var))
	       (return (ratwhile exitcond body)))
	      ((and var
		    (not lo)
		    (lisplogexpp nextexp)
		    (equal exitcond var))
	       (return (ratrepeat body nextexp)))
	      (t
	       (return (ratforfor var lo nextexp exitcond body))))))

(defun ratread (stmt)
  (mkfratread (cadr stmt)))

(defun ratrepeat (body exitcond)
  (prog (r)
	(setq r (mkfratrepeat))
	(indentratlevel (+ 1))
	(setq r (append r (ratstmt body)))
	(indentratlevel (- 1))
	(return (append r (mkfratuntil exitcond)))))

(defun ratreturn (stmt)
  (mkfratreturn (cadr stmt)))

(defun ratstmtgp (stmtgp)
  (prog (r)
	(cond ((equal (car stmtgp) 'progn)
	       (setq stmtgp (cdr stmtgp)))
	      (t
	       (setq stmtgp (cddr stmtgp))))
	(setq r (mkfratbegingp))
	(indentratlevel (+ 1))
	(setq r (append r (foreach stmt in stmtgp conc (ratstmt stmt))))
	(indentratlevel (- 1))
	(return (append r (mkfratendgp)))))

(defun ratstmtno (label)
  (prog (stmtno)
	(setq stmtno (or (get label '*stmtno*)
			 (put label '*stmtno* (genstmtno))))
	(return (mkfratcontinue stmtno))))

(defun ratstop (stmt)
  (mkfratstop))

(defun ratwhile (cond body)
  (prog (r)
	(cond (cond
	       (setq cond (list 'not cond))))
	(setq r (mkfratwhile cond))
	(indentratlevel (+ 1))
	(setq r (append r (ratstmt body)))
	(indentratlevel (- 1))
	(return r)))

(defun ratwrite (stmt)
  (mkfratwrite (cdr stmt)))


;;                                    ;;
;;  ratfor code formatting functions  ;;
;;                                    ;;


;;  statement formatting  ;;

(defun mkfratassign (lhs rhs)
  (append (append (cons (mkrattab) (ratexpgen lhs))
		  (cons '= (ratexpgen rhs)))
	  (list (mkterpri))))

(defun mkfratbegingp ()
  (list (mkrattab) '{ (mkterpri)))

(defun mkfratbreak ()
  (list (mkrattab) 'break (mkterpri)))

(defun mkfratcall (fname params)
  (progn
   (cond (params
	  (setq params (append (append (list '|(|)
				       (foreach p in (insertcommas params)
						conc (ratexpgen p)))
			       (list '|)|)))))
   (append (append (list (mkrattab) 'call '| |)
		   (ratexpgen fname))
	   (append params
		   (list (mkterpri))))))

(defun mkfratcontinue (stmtno)
  (list stmtno '| | (mkrattab) 'continue (mkterpri)))

(defun mkfratdec (type varlist)
  (progn
   (setq type (or type 'dimension))
   (setq varlist (foreach v in (insertcommas varlist) conc (ratexpgen v)))
   (cond ((implicitp type)
	  (append (list (mkrattab) type '| |  '|(|)
                  (append varlist (list '|)| (mkterpri)))))
	 (t
	  (append (list (mkrattab) type '| | )
		  (aconc varlist (mkterpri)))))))

(defun mkfratdo (var lo hi incr)
  (progn
   (cond ((onep incr)
	  (setq incr nil))
	 (incr
	  (setq incr (cons '|,| (ratexpgen incr)))))
   (append (append (append (list (mkrattab) 'do '| |)
			   (ratexpgen var))
		   (append (cons '|=| (ratexpgen lo))
			   (cons '|,| (ratexpgen hi))))
	   (append incr
		   (list (mkterpri))))))

(defun mkfratelse ()
  (list (mkrattab) 'else (mkterpri)))

(defun mkfratelseif (exp)
  (append (append (list (mkrattab) 'else '| | 'if '| | '|(|)
		  (ratexpgen exp))
	  (list '|)| (mkterpri))))

(defun mkfratend ()
  (list (mkrattab) 'end (mkterpri)))

(defun mkfratendgp ()
  (list (mkrattab) '} (mkterpri)))

(defun mkfratfor (var1 lo cond var2 nextexp)
  (progn
   (cond (var1
	  (setq var1 (append (ratexpgen var1) (cons '= (ratexpgen lo))))))
   (cond (cond
	  (setq cond (ratexpgen cond))))
   (cond (var2
	  (setq var2 (append (ratexpgen var2) (cons '= (ratexpgen nextexp))))))
   (append (append (append (list (mkrattab) 'for '| |  '|(|)
			   var1)
		   (cons '|;| cond))
	   (append (cons '|;| var2)
		   (list '|)| (mkterpri))))))

(defun mkfratgo (stmtno)
  (list (mkrattab) 'goto '| | stmtno (mkterpri)))

(defun mkfratif (exp)
  (append (append (list (mkrattab) 'if '| |  '|(|)
		  (ratexpgen exp))
	  (list '|)| (mkterpri))))

(defun mkfratliteral (args)
  (foreach a in args conc
	   (cond ((equal a '$tab) (list (mkrattab)))
		 ((equal a '$cr) (list (mkterpri)))
		 ((listp a) (ratexpgen a))
		 (t (list a)))))

(defun mkfratread (var)
  (append (list (mkrattab) 'read '|(*,*)| '| | )
	  (append (ratexpgen var) (list (mkterpri)))))

(defun mkfratrepeat ()
  (list (mkrattab) 'repeat (mkterpri)))

(defun mkfratreturn (exp)
  (cond (exp
	 (append (append (list (mkrattab) 'return '|(|) (ratexpgen exp))
		 (list '|)| (mkterpri))))
	(t
	 (list (mkrattab) 'return (mkterpri)))))

(defun mkfratstop ()
  (list (mkrattab) 'stop (mkterpri)))

(defun mkfratsubprogdec (type stype name params)
  (progn
   (cond (params
	  (setq params (aconc (cons '|(|
				    (foreach p in (insertcommas params)
					     conc (ratexpgen p)))
			      '|)|))))
   (cond (type
	  (setq type (list (mkrattab) type '| |  stype '| | )))
	 (t
	  (setq type (list (mkrattab) stype '| | ))))
   (append (append type (ratexpgen name))
	   (aconc params (mkterpri)))))

(defun mkfratuntil (logexp)
  (append (list (mkrattab) 'until '| |  '|(|)
	  (append (ratexpgen logexp) (list '|)| (mkterpri)))))

(defun mkfratwhile (exp)
  (append (append (list (mkrattab) 'while '| |  '|(|)
		  (ratexpgen exp))
	  (list '|)| (mkterpri))))

(defun mkfratwrite (arglist)
  (append (append (list (mkrattab) 'write '|(*,*)| '| | )
		  (foreach arg in (insertcommas arglist) conc (ratexpgen arg)))
	  (list (mkterpri))))

;;  indentation control  ;;

(defun mkrattab ()
  (list 'rattab ratcurrind*))

(defun indentratlevel (n)
  (setq ratcurrind* (+ ratcurrind* (* n tablen*))))
