;*******************************************************************************
;*                                                                             *
;*  copyright (c) 1988 kent state univ.  kent, ohio 44242                      *
;*                                                                             *
;*******************************************************************************

(when (null (fboundp 'wrs)) (load "convmac.lisp"))

(declare-top (special *gentran-dir tempvartype* tempvarname* tempvarnum* genstmtno*
	genstmtincr* *symboltable* *instk* *stdin* *currin* *outstk* COMMA*
	*stdout* *currout* *outchanl* *lispdefops* *lisparithexpops*
	*lisplogexpops* *lispstmtops* *lispstmtgpops*))

(declare-top (special *gendecs *endofloopstack* fortcurrind* tablen*))

;;  -----------  ;;
;;  lspfor.l     ;;    lisp-to-fortran translation module
;;  -----------  ;;

(put 'or       '*fortranprecedence* 1)
(put 'and      '*fortranprecedence* 2)
(put 'not      '*fortranprecedence* 3)
(put 'equal    '*fortranprecedence* 4)
(put 'notequal '*fortranprecedence* 4)
(put '>        '*fortranprecedence* 4)
(put 'greaterp '*fortranprecedence* 4)
(put 'geqp     '*fortranprecedence* 4)
(put '<        '*fortranprecedence* 4)
(put 'lessp    '*fortranprecedence* 4)
(put 'leqp     '*fortranprecedence* 4)
(put '+        '*fortranprecedence* 5)
(put 'plus     '*fortranprecedence* 5)
(put '*        '*fortranprecedence* 6)
(put 'times    '*fortranprecedence* 6)
(put 'quotient '*fortranprecedence* 6)
(put '-        '*fortranprecedence* 7)
(put 'minus    '*fortranprecedence* 7)
(put 'expt     '*fortranprecedence* 8)
(put 'or       '*fortranop* '| .or. |)
(put 'and      '*fortranop* '| .and. |)
(put 'not      '*fortranop* '| .not. |)
(put 'equal    '*fortranop* '| .eq. |)
(put 'notequal '*fortranop* '| .ne. |)
(put '>        '*fortranop* '| .gt. |)
(put 'greaterp '*fortranop* '| .gt. |)
(put 'geqp     '*fortranop* '| .ge. |)
(put '<        '*fortranop* '| .lt. |)
(put 'lessp    '*fortranop* '| .lt. |)
(put 'leqp     '*fortranop* '| .le. |)
(put '+        '*fortranop* '|+|)
(put 'plus     '*fortranop* '|+|)
(put '*        '*fortranop* '|*|)
(put 'times    '*fortranop* '|*|)
(put 'quotient '*fortranop* '|//|)
(put 'expt     '*fortranop* '|**|)
(put '-        '*fortranop* '|-|)
(put 'minus    '*fortranop* '|-|)
(put nil '*fortranname* ".false.")

;;                                         ;;
;;  lisp-to-fortran translation functions  ;;
;;                                         ;;


;;  control function  ;;


(defun fortcode (forms)
  (foreach f in forms conc
	   (cond ((atom f)
		  (cond ((member f '($begin_group $end_group)) ())
		        (t (fortexp f))))
		 ((or (lispstmtp f)
		      (lispstmtgpp f))
		  (cond (*gendecs
			 (prog (r)
			       (setq r
				     (append (fortdecs (symtabget '*main*
								  '*decs*))
					     (fortstmt f)))
			       (symtabrem '*main* '*decs*)
			       (return r)))
			(t
			 (fortstmt f))))
		 ((lispdefp f)
		  (fortsubprog f))
		 (t
		  (fortexp f)))))


;;  subprogram translation  ;;


(defun fortsubprog (def)
  (prog (type stype name params body lastst r)
	(setq name (cadr def))
	(setq body (cdddr def))
	(cond ((and body (equal body '(nil))) (setq body ())))
	(cond ((and (onep (length body))
		    (lispstmtgpp (car body)))
	       (progn (setq body (cdar body))
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
			(cond ((or type
				   (gfunctionp body name))
			       'function)
			      (t
			       'subroutine))))
	(symtabrem name '*type*)
	(setq params (or (symtabget name '*params*) (caddr def)))
	(symtabrem name '*params*)
	(setq r (mkffortsubprogdec type stype name params))
	(cond (*gendecs
	       (setq r (append r (fortdecs (symtabget name '*decs*))))))
	(setq r (append r (foreach s in body conc (fortstmt s))))
	(cond (*gendecs
	       (progn
		(symtabrem name nil)
		(symtabrem name '*decs*))))
	(return r)))

;;  generation of declarations  ;;

(defun fortdecs (decs)
  (foreach tl in (formtypelists decs) conc
	   (mkffortdec (car tl) (cdr tl))))

;;  expression translation  ;;

(defun fortexp (exp)
  (fortexp1 exp 0))

(defun fortexp1 (exp wtin)
  (cond ((atom exp) (list (fortranname exp)))
	((eq (car exp) 'data) (fortdata exp))
	((eq (car exp) 'literal) (fortliteral exp))
	((null (cdr exp)) exp)
	((member (car exp) '(minus not) :test #'eq)
	 (let* ((wt (fortranprecedence (car exp)))
		(res (cons (fortranop (car exp)) (fortexp1 (cadr exp) wt))))
	       (cond ((< wt wtin) (aconc (cons '|(| res) '|)|))
		     (t res))))
	((or (member (car exp) *lisparithexpops* :test #'eq)
	     (member (car exp) *lisplogexpops* :test #'eq))
	 (let* ((wt (fortranprecedence (car exp)))
		(op (fortranop (car exp)))
		(res (fortexp1 (cadr exp) wt))
		(res1))
	       (setq exp (cdr exp))
	       (cond ((eq op '+)
		      (while (setq exp (cdr exp))
                         (progn
			  (setq res1 (fortexp1 (car exp) wt))
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
						 (fortexp1 (car exp) wt)))))))
	       (cond ((< wt wtin) (aconc (cons '|(| res) '|)|))
		     (t res))))
	(t
	 (let ((res (cons (car exp) (cons '|(| (fortexp1 (cadr exp) 0)))))
              (setq exp (cdr exp))
	      (while (setq exp (cdr exp))
                 (setq res (append res (cons '|,| (fortexp1 (car exp) 0)))))
              (aconc res '|)|)))))

(defun fortranname (name)   
  (if (symbolp name) (or (get name '*fortranname*) name) name))

(defun fortranop (op)
  (or (get op '*fortranop*) op))

(defun fortranprecedence (op)
  (or (get op '*fortranprecedence*) 9))

;;  statement translation  ;;

(defun fortstmt (stmt)
  (cond ((null stmt) nil)
	((member stmt '($begin_group $end_group)) nil)
	((lisplabelp stmt) (fortstmtno stmt))
	((eq (car stmt) 'data) (fortdata stmt))
	((eq (car stmt) 'literal) (fortliteral stmt))
	((lispreadp stmt) (fortread stmt))
	((lispassignp stmt) (fortassign stmt))
	((lispprintp stmt) (fortwrite stmt))
	((lispcondp stmt) (fortif stmt))
	((lispbreakp stmt) (fortbreak stmt))
	((lispgop stmt) (fortgoto stmt))
	((lispreturnp stmt) (fortreturn stmt))
	((lispstopp stmt) (fortstop stmt))
	((lispendp stmt) (fortend stmt))
	((lispdop stmt) (fortloop stmt))
	((lispstmtgpp stmt) (fortstmtgp stmt))
	((lispdefp stmt) (fortsubprog stmt))
	((lispcallp stmt) (fortcall stmt))))

(defun fortassign (stmt)
  (mkffortassign (cadr stmt) (caddr stmt)))

(defun fortbreak (stmt)
  (cond ((null *endofloopstack*)
	 (gentranerr 'e nil "break not inside loop - cannot be translated" nil))
	((atom (car *endofloopstack*))
	 (prog (n1)
	       (setq n1 (genstmtno))
	       (rplaca *endofloopstack* (list (car *endofloopstack*) n1))
	       (return (mkffortgo n1))))
	(t
	 (mkffortgo (cadar *endofloopstack*)))))

(defun fortcall (stmt)
  (mkffortcall (car stmt) (cdr stmt)))

(defun fortdo (var lo nextexp exitcond body)
  (prog (n1 hi incr result)
	(setq n1 (genstmtno))
	(setq *endofloopstack* (cons n1 *endofloopstack*))
	(setq hi (car (delete1 '> (delete1 '< (delete1 var exitcond)))))
	(setq incr (car (delete1 'plus (delete1 var nextexp))))
	(setq result (mkffortdo n1 var lo hi incr))
	(indentfortlevel (+ 1))
	(setq result (append result (foreach st in body conc (fortstmt st))))
	(indentfortlevel (- 1))
	(setq result (append result (mkffortcontinue n1)))
	(cond ((listp (car *endofloopstack*))
	       (setq result
		     (append result
			     (mkffortcontinue (cadar *endofloopstack*))))))
	(setq *endofloopstack* (cdr *endofloopstack*))
	(return result)))

(defun fortend (stmt)
  (mkffortend))

(defun fortfor (var lo nextexp exitcond body)
  (prog (n1 n2 result)
	(setq n1 (genstmtno))
	(setq n2 (genstmtno))
	(setq *endofloopstack* (cons n2 *endofloopstack*))
	(cond (var (setq result (mkffortassign var lo))))
	(cond (exitcond
	       (setq result (append result
				    (append (list n1 '| | )
					    (mkffortifgo exitcond n2)))))
	      (t
	       (setq result (append result (mkffortcontinue n1)))))
	(indentfortlevel (+ 1))
	(setq result (append result (foreach st in body conc (fortstmt st))))
	(cond (nextexp
	       (progn
		(cond ((equal nextexp '(nil)) (setq nextexp nil)))
		(setq result (append result (mkffortassign var nextexp))))))
	(setq result (append result (mkffortgo n1)))
	(indentfortlevel (- 1))
	(setq result (append result (mkffortcontinue n2)))
	(cond ((listp (car *endofloopstack*))
	       (setq result
		     (append result
			     (mkffortcontinue (cadar *endofloopstack*))))))
	(setq *endofloopstack* (cdr *endofloopstack*))
	(return result)))

(defun fortgoto (stmt)
  (prog (stmtno)
	(cond ((not (setq stmtno (get (cadr stmt) '*stmtno*)))
	       (setq stmtno (put (cadr stmt) '*stmtno* (genstmtno)))))
	(return (mkffortgo stmtno))))

(defun fortif (stmt)
  (prog (n1 n2 res)
	(setq stmt (cdr stmt))
	(cond ((onep (length stmt))
	       (cond ((equal (caar stmt) 't)
		      (return (foreach st in (cdar stmt) conc
				       (fortstmt st))))
		     (t
		      (return
		       (progn
			(setq n1 (genstmtno))
			(setq res (mkffortifgo (list 'not (caar stmt)) n1))
			(indentfortlevel (+ 1))
			(setq res (append res (foreach st in (cdar stmt) conc
						       (fortstmt st))))
			(indentfortlevel (- 1))
			(append res (mkffortcontinue n1)))))))
	      (t
	       (return
		(progn
		 (setq n1 (genstmtno))
		 (setq n2 (genstmtno))
		 (setq res (mkffortifgo (list 'not (caar stmt)) n1))
		 (indentfortlevel (+ 1))
		 (setq res (append res (foreach st in (cdar stmt) conc
						(fortstmt st))))
		 (setq res (append res (mkffortgo n2)))
		 (indentfortlevel (- 1))
		 (setq res (append res (mkffortcontinue n1)))
		 (indentfortlevel (+ 1))
		 (setq res (append res (fortif (cons 'cond (cdr stmt)))))
		 (indentfortlevel (- 1))
		 (append res (mkffortcontinue n2))))))))

(defun fortliteral (stmt)
  (foreach a in (cdr stmt) conc
	   (cond ((equal a '$tab) (list (mkforttab)))
		 ((equal a '$cr) (list (mkterpri)))
		 ((listp a) (fortexp a))
		 (t (list a)))))

;; fortdata added by pwang 12/12/88
(defun fortdata (stmt)
  (append (list (mkforttab) "data " (cadr stmt) '|//|)
	  (addcom (cddr stmt))
	  (list '|//|))
)

(setq COMMA* ",")

(defun addcom(nl)
(cond ((null nl) nil)
      ((null (cdr nl)) nl)
      (t (cons (car nl) (cons COMMA* (addcom (cdr nl)))))
      )
)

(defun fortloop (stmt)
  (prog (var lo nextexp exitcond body r)
	(cond ((complexdop stmt)
	       (return (fortstmt (seqtogp (simplifydo stmt))))))
	(cond ((setq var (cadr stmt))
	       (progn
		(setq lo (cadar var))
		(cond ((equal (length (car var)) 3)
		       (setq nextexp (or (caddar var) (list 'nil)))))
		(setq var (caar var)))))
	(cond ((setq exitcond (caddr stmt))
	       (setq exitcond (car exitcond))))
	(setq body (cdddr stmt))
	(cond ((and var
		    lo
		    (equal (car nextexp) 'plus)
		    (member var nextexp)
		    (member (car exitcond) '(> <))
		    (member var exitcond))
	       (return (fortdo var lo nextexp exitcond body)))
	      ((and exitcond
		    (not var))
	       (return (fortwhile exitcond body)))
	      ((and var
		    (not lo)
		    (lisplogexpp nextexp)
		    (equal exitcond var))
	       (return (fortrepeat body nextexp)))
	      (t
	       (return (fortfor var lo nextexp exitcond body))))))

(defun fortread (stmt)
  (mkffortread (cadr stmt)))

(defun fortrepeat (body exitcond)
  (prog (n result)
	(setq n (genstmtno))
	(setq *endofloopstack* (cons 'dummy *endofloopstack*))
	(setq result (mkffortcontinue n))
	(indentfortlevel (+ 1))
	(setq result (append result (foreach st in body conc (fortstmt st))))
	(indentfortlevel (- 1))
	(setq result (append result (mkffortifgo (list 'not exitcond) n)))
	(cond ((listp (car *endofloopstack*))
	       (setq result
		     (append result
			     (mkffortcontinue (cadar *endofloopstack*))))))
	(setq *endofloopstack* (cdr *endofloopstack*))
	(return result)))

(defun fortreturn (stmt)
  (cond ((onep (length stmt))
	 (mkffortreturn))
	((not (eq (car *symboltable*) '*main*))
	 (append (mkffortassign (car *symboltable*) (cadr stmt))
		 (mkffortreturn)))
	(t
	 (gentranerr 'e
		     nil
		     "return not inside function - cannot be translated"
		     nil))))

(defun fortstmtgp (stmtgp)
  (progn
   (cond ((equal (car stmtgp) 'progn)
	  (setq stmtgp (cdr stmtgp)))
	 (t
	  (setq stmtgp (cddr stmtgp))))
   (foreach stmt in stmtgp conc (fortstmt stmt))))

(defun fortstmtno (label)
  (prog (stmtno)
	(cond ((not (setq stmtno (get label '*stmtno*)))
	       (setq stmtno (put label '*stmtno* (genstmtno)))))
	(return (mkffortcontinue stmtno))))

(defun fortstop (stmt)
  (mkffortstop))

(defun fortwhile (exitcond body)
  (prog (n1 n2 result)
	(setq n1 (genstmtno))
	(setq n2 (genstmtno))
	(setq *endofloopstack* (cons n2 *endofloopstack*))
	(setq result (append (list n1 '| |) (mkffortifgo exitcond n2)))
	(indentfortlevel (+ 1))
	(setq result (append result (foreach st in body conc (fortstmt st))))
	(setq result (append result (mkffortgo n1)))
	(indentfortlevel (- 1))
	(setq result (append result (mkffortcontinue n2)))
	(cond ((listp (car *endofloopstack*))
	       (setq result
		     (append result
			     (mkffortcontinue (cadar *endofloopstack*))))))
	(setq *endofloopstack* (cdr *endofloopstack*))
	(return result)))

(defun fortwrite (stmt)
  (mkffortwrite (cdr stmt)))


;;                                     ;;
;;  fortran code formatting functions  ;;
;;                                     ;;


;;  statement formatting  ;;

(defun mkffortassign (lhs rhs)
  (append (append (cons (mkforttab) (fortexp lhs))
		  (cons '= (fortexp rhs)))
	  (list (mkterpri))))

(defun mkffortcall (fname params)
  (progn
   (cond (params
	  (setq params (append (append (list '|(|)
				        (foreach p in (insertcommas params)
						 conc (fortexp p)))
				(list '|)|)))))
   (append (append (list (mkforttab) 'call '| |)
		   (fortexp fname))
	   (append params (list (mkterpri))))))

(defun mkffortcontinue (stmtno)
  (list stmtno '| | (mkforttab) 'continue (mkterpri)))

(defun mkffortdec (type varlist)
  (progn
   (setq type (or type 'dimension))
   (setq varlist (foreach v in (insertcommas varlist)
		          conc (fortexp v)))
   (cond ((implicitp type)
          (append (list (mkforttab) type '| | '|(|)
		  (append varlist
		          (list '|)| (mkterpri)))))
	 (t
	  (append (list (mkforttab) type '| |)
	          (aconc varlist (mkterpri)))))))

(defun mkffortdo (stmtno var lo hi incr)
  (progn
   (cond ((onep incr)
          (setq incr nil))
	 (incr
	  (setq incr (cons '|,| (fortexp incr)))))
   (append (append (append (list (mkforttab) 'do '| | stmtno '| | )
		           (fortexp var))
		   (append (cons '= (fortexp lo))
		           (cons '|,| (fortexp hi))))
	   (append incr
	           (list (mkterpri))))))

(defun mkffortend ()
  (list (mkforttab) 'end (mkterpri)))

(defun mkffortgo (stmtno)
  (list (mkforttab) 'goto '| | stmtno (mkterpri)))

(defun mkffortifgo (exp stmtno)
  (append (append (list (mkforttab) 'if '| | '|(|)
		  (fortexp exp))
	  (list '|)| '| |  'goto '| |  stmtno (mkterpri))))


(defun mkffortread (var)
  (append (list (mkforttab) 'read '|(*,*)| '| | )
	  (append (fortexp var)
		  (list (mkterpri)))))

(defun mkffortreturn ()
  (list (mkforttab) 'return (mkterpri)))

(defun mkffortstop ()
  (list (mkforttab) 'stop (mkterpri)))

(defun mkffortsubprogdec (type stype name params)
  (progn
   (cond (params
	  (setq params
		(aconc (cons '|(|
			     (foreach p in (insertcommas params) conc
				      (fortexp p)))
		       '|)|))))
   (cond (type
	  (setq type (list (mkforttab) type '| |  stype '| | )))
	 (t
	  (setq type (list (mkforttab) stype '| | ))))
   (append (append type (fortexp name))
	   (aconc params (mkterpri)))))

(defun mkffortwrite (arglist)
  (append (append (list (mkforttab) 'write '|(*,*)| '| | )
		  (foreach arg in (insertcommas arglist) conc (fortexp arg)))
	  (list (mkterpri))))

;;  indentation control  ;;

(defun mkforttab ()
  (list 'forttab (- fortcurrind* 6)))

(defun indentfortlevel (n)
  (setq fortcurrind* (+ fortcurrind* (* n tablen*))))
