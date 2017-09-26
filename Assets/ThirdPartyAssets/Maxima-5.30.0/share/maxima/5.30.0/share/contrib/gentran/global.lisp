;*******************************************************************************
;*                                                                             *
;*  copyright (c) 1988 kent state univ.  kent, ohio 44242                      *
;*                                                                             *
;*******************************************************************************

(when (null (fboundp 'wrs)) (load "convmac.lisp"))

(declare-top (special *gentranlang *gentran-dir tempvartype* tempvarname* tempvarnum* genstmtno*
	genstmtincr* *symboltable* *instk* *stdin* *currin* *outstk*
	*stdout* *currout* *outchanl* *lispdefops* *lisparithexpops*
	*lisplogexpops* *lispstmtops* *lispstmtgpops*))
;;  -----------  ;;
;;  global.l     ;;    general functions
;;  -----------  ;;


;;                                                                   ;;
;;  1. temporary variable generation, marking & unmarking functions  ;;
;;                                                                   ;;


(defun tempvar (type)
  ;                                                           ;
  ; if type member '(nil 0) then type <- tempvartype*         ;
  ;                                                           ;
  ; if type neq 'nil and type neq 'unknown then               ;
  ;    var <- 1st unmarked tvar of vtype type or of vtype nil ;
  ;           which isn't in the symbol table                 ;
  ;    put type on var's vtype property list                  ;
  ;    put declaration in symbol table                        ;
  ; else if type = nil then                                   ;
  ;    var <- 1st unmarked tvar of type nil                   ;
  ;           which isn't in the symbol table                 ;
  ; else type = 'unknown                                      ;
  ;    var <- 1st unmarked tvar of type nil                   ;
  ;           which isn't in the symbol table                 ;
  ;    put 'unknown on var's vtype property list              ;
  ;    print warning - "undeclared"                           ;
  ;                                                           ;
  ; return var                                                ;
  ;                                                           ;
  (prog (tvar num)
(cond (tempvartype* (setq tempvartype* (stripdollar1 tempvartype*))))
	(cond ((member type '(nil 0)) (setq type tempvartype*)))
(cond (type (setq type (stripdollar1 type))))
(setq tempvarname* (stripdollar1 tempvarname*))
	(setq num tempvarnum*)
	(cond ((member type '(nil unknown))
	       (setq tvar (gentemp (string tempvarname*))))
	      (t
	       (setq tvar (gentemp (string tempvarname*)))))
	(put tvar '*vtype* type)
	(cond ((equal type 'unknown)
	       (gentranerr 'w tvar "undeclared variable" nil))
	      (type
	       (symtabput nil tvar (list type))))
	(return tvar)))

(defun markvar (var)
  (cond ((numberp var) var)
	((atom var) (progn (flag (list var) '*marked*) var))
	(t (progn (foreach v in var do (markvar v)) var))))

(defun markedvarp (var)
  (flagp var '*marked*))

(defun unmarkvar (var)
  (cond ((numberp var) var)
	(t (remflag (list var) '*marked*))))

(defun recurunmark (exp)
  (cond ((atom exp) (unmarkvar exp))
	(t (foreach elt in exp do (recurunmark elt)))))


;;                                           ;;
;;  2. statement number generation function  ;;
;;                                           ;;

(defun genstmtno ()
  (incf genstmtno* genstmtincr*))

;;                                                             ;;
;;  3. symbol table insertion, retrieval & deletion functions  ;;
;;                                                             ;;


(defun symtabput (name type value)
  ;                                                                       ;
  ; call                                                inserts           ;
  ; (symtabput subprogname nil       nil              ) subprog name      ;
  ; (symtabput subprogname '*type*   subprogtype      ) subprogram type   ;
  ; (symtabput subprogname '*params* paramlist        ) parameter list    ;
  ; (symtabput subprogname vname     '(type d1 d2 ...)) type & dimensions ;
  ;                                                     for variable,     ;
  ;                                                     variable range,   ;
  ;     if subprogname=nil                              parameter, or     ;
  ;        then subprogname <- car symboltable          function name     ;
  ;                                                                       ;
  (progn
   (setq name (or name (car *symboltable*)))
   (setq *symboltable* (cons name (delete1 name *symboltable*)))
   (cond ((member type '(*type* *params*))
	  (put name type value))
	 (type
	  (prog (v vtype vdims dec decs)
		(setq v type)
		(setq vtype (car value))
		(setq vdims (cdr value))
		(setq decs (get name '*decs*))
		(setq dec (assoc v decs))
		(setq decs (delete1 dec decs))
		(setq vtype (or vtype (cond ((> (length dec) 1)
					     (cadr dec)))))
		(setq vdims (or vdims (cond ((> (length dec) 2)
					     (cddr dec)))))
		(setq dec (cons v (cons vtype vdims)))
		(put name '*decs* (aconc decs dec)))))))

(defun symtabget (name type)
  ;                                                                   ;
  ; call                              retrieves                       ;
  ; (symtabget nil         nil      ) all subprogram names            ;
  ; (symtabget subprogname '*type*  ) subprogram type                 ;
  ; (symtabget subprogname '*params*) parameter list                  ;
  ; (symtabget subprogname vname    ) type & dimensions for variable, ;
  ;                                   variable range, parameter, or   ;
  ;                                   function name                   ;
  ; (symtabget subprogname '*decs*  ) all types & dimensions          ;
  ;                                                                   ;
  ;     if subprogname=nil & 2nd arg is non-nil                       ;
  ;        then subprogname <- car symboltable                        ;
  ;                                                                   ;
  (progn
   (cond (type (setq name (or name (car *symboltable*)))))
   (cond ((null name) *symboltable*)
	 ((member type '(*type* *params* *decs*)) (get name type))
	 ((assoc type (get name '*decs*))))))

(defun symtabrem (name type)
  ;                                                                   ;
  ; call                              deletes                         ;
  ; (symtabrem subprogname nil      ) subprogram name                 ;
  ; (symtabrem subprogname '*type*  ) subprogram type                 ;
  ; (symtabrem subprogname '*params*) parameter list                  ;
  ; (symtabrem subprogname vname    ) type & dimensions for variable, ;
  ;                                   variable range, parameter, or   ;
  ;                                   function name                   ;
  ; (symtabrem subprogname '*decs*  ) all types & dimensions          ;
  ;                                                                   ;
  ;     if subprogname=nil                                            ;
  ;        then subprogname <- car symboltable                        ;
  ;                                                                   ;
  (progn
   (setq name (or name (car *symboltable*)))
   (cond ((null type)
	  (setq *symboltable* (or (delete1 name *symboltable*) '(*main*))))
	 ((member type '(*type* *params* *decs*))
	  (remprop name type))
	 (t (prog (v dec decs)
		  (setq v type)
		  (setq decs (get name '*decs*))
		  (setq dec (assoc v decs))
		  (setq decs (delete1 dec decs))
		  (put name '*decs* decs))))))

(defun getvartype (var)
  (prog (type)
	(cond ((listp var) (setq var (car var))))
	(setq type (symtabget nil var))
	(cond ((and type (> (length type) 1))
	       (setq type (cadr type)))
	      ((setq type nil)))
	(cond ((and (not (eq *gentranlang 'c)) (atom var) (null type))
	       (setq type (imptype var))))
	(return type)))

(defun $gentranlang(a)
	(setq a (stripdollar1 a))
	(cond ((not (member a '(fortran ratfor c) :test #'eq))
		(merror "arg must be one of fortran c or ratfor")))
	(setq *gentranlang a))

(defun imptype(var)
   (cond ((member (car (explode var)) '(|i| |j| |k| |l| |m| |n|) :test #'eq) 'integer)
	 (t 'real)))

(defun arrayeltp (exp)
  (> (length (symtabget nil (car exp))) 2))


;;                                                       ;;
;;  4. input & output file stack manipulation functions  ;;
;;                                                       ;;


(defun delinstk (pr)
  (progn
   (setq *instk* (or (delete1 pr *instk*) (list *stdin*)))
   (setq *currin* (car *instk*))))

(defun delstk (pr)
  ; remove all occurrences of filepair from output file stack ;
  (while (member pr (cdr (reverse *outstk*)))
	 (popstk pr)))

(defun flisteqp (flist1 flist2)
  (progn
   (setq flist1 (foreach f in flist1 collect (mkfil f)))
   (foreach f in flist2 do (setq flist1 (delete1 (mkfil f) flist1)))
   (null flist1)))

(defun filpr (fname stk)
  ; retrieve fname's filepair from stack stk ;
  (cond ((null stk) nil)
	((and (caar stk) (equal (mkfil fname) (mkfil (caar stk))))
	 (car stk))
	((filpr fname (cdr stk)))))

(defun mkfilpr (fname)
  ; open output channel & return filepair (fname . chan#) ;
  (cons fname (open fname :direction :output :if-exists :append :if-does-not-exist :create)))

(defun pfilpr (flist stk)
  ; retrieve flist's "parallel" filepair from stack stk ;
  (cond ((null stk) nil)
	((and (null (caar stk)) (flisteqp flist (cdar stk)))
	 (car stk))
	((pfilpr flist (cdr stk)))))

(defun popinstk ()
  (delinstk *currin*))

(defun popstk (pr)
  ; remove top-most occurrence of filepair from output file stack ;
  (cond ((car pr)
	 (resetstk (delete1 pr *outstk*)))
	(t
	 (prog (stk1 stk2)
	       (setq stk1 *outstk*)
	       (while (not (equal (car stk1) pr))
		      (progn (setq stk2 (aconc stk2 (car stk1)))
			     (setq stk1 (cdr stk1))))
	       (while (not (equal (car stk1) '(nil)))
		      (setq stk1 (cdr stk1)))
	       (resetstk (append stk2 (cdr stk1)))))))

(defun pushinstk (pr)
  (progn (setq *instk* (cons pr *instk*))
	 (setq *currin* (car *instk*))))

(defun pushstk (pr)
  ; push filepair onto output file stack ;
  (progn (setq *outstk* (cons pr *outstk*))
	 (resetstkvars)))

(defun resetstk (stk)
  (prog (s)
	(cond (stk
	       (repeat (cond ((or (caar stk) (equal (car stk) '(nil)))
			      (setq s (aconc s (car stk))))
			     (t (progn
				 (foreach f in (cdar stk) do
					  (cond ((not (filpr f *outstk*))
						 (setq stk
						       (cons
							(delete1 f (car stk))
							(cdr stk))))))
				 (cond ((equal (car stk) '(nil))
					(setq stk (cdr stk)))
				       (t
					(setq s (aconc s (car stk))))))))
		       (null (setq stk (cdr stk))))))
	(setq *outstk* (or s (list *stdout*)))
	(resetstkvars)))

(defun resetstkvars ()
  ; reset current-output to filepair on top of output file stack, ;
  ; reset output channel list to channel #'s corresponding to     ;
  ;  name(s) in current-output                                    ;
  (progn
   (setq *currout* (car *outstk*))
   (setq *outchanl* (cond ((car *currout*) (list (cdr *currout*)))
			  (t (foreach f in (cdr *currout*) collect
				      (cdr (filpr f *outstk*))))))))


;;                                      ;;
;;  5. functions for making lisp forms  ;;
;;                                      ;;


(defun mkassign (var exp)
  (list 'setq var exp))

(defun mkcond (pairs)
  (cons 'cond pairs))

(defun mkdef (name params body)
  (append (list 'defun name params) body))

(defun mkdo (var exitcond body)
  (append (list 'do var exitcond) body))

(defun mkreturn (exp)
  (list 'return exp))

(defun mkstmtgp (vars stmts)
   (cond ((numberp vars) (cons 'progn stmts))
	 ((cons 'prog (cons vars stmts)))))

(defun mkterpri ()
  '(terpri))


;;                           ;;
;;  6. lisp form predicates  ;;
;;                           ;;


(defun lispassignp (stmt)
  (and (listp stmt)
       (equal (car stmt) 'setq)))

(defun lispbreakp (form)
  (equal (car form) 'break))

(defun lispcallp (form)
  (listp form))

(defun lispcondp (stmt)
  (and (listp stmt)
       (equal (car stmt) 'cond)))

(defun lispdefp (form)
  (and (listp form)
       (member (car form) *lispdefops*)))

(defun lispdop (stmt)
  (and (listp stmt)
       (equal (car stmt) 'do)))

(defun lispexpp (form)
  (or (atom form)
      (member (car form) (append *lisparithexpops* *lisplogexpops*))
      (not (member (car form) (append (append *lispstmtops* *lispstmtgpops*)
				      *lispdefops*)))))

(defun lispendp (form)
  (and (listp form)
       (equal (car form) 'end)))

(defun lispgop (form)
  (equal (car form) 'go))

(defun lisplabelp (form)
  (atom form))

(defun lisplogexpp (form)
  (or (atom form)
      (member (car form) *lisplogexpops*)
      (not (member (car form) (append (append *lisparithexpops* *lispstmtops*)
				      (append *lispstmtgpops* *lispdefops*))))))

(defun lispprintp (form)
  (equal (car form) 'princ))

(defun lispreadp (form)
  (and (equal (car form) 'setq)
       (listp (caddr form))
       (equal (caaddr form) 'read)))

(defun lispreturnp (stmt)
  (and (listp stmt)
       (equal (car stmt) 'return)))

(defun lispstmtp (form)
  (or (atom form)
      (member (car form) *lispstmtops*)
      (and (atom (car form))
	   (not (member (car form) (append
				    (append *lisparithexpops* *lisplogexpops*)
				    (append *lispstmtgpops* *lispdefops*)))))))

(defun lispstmtgpp (form)
  (and (listp form)
       (member (car form) *lispstmtgpops*)))

(defun lispstopp (form)
  (equal (car form) 'stop))


;;                      ;;
;;  7. type predicates  ;;
;;                      ;;


(defun gfunctionp (stmt name)
  ; does stmt contain an assignment which assigns a value to name? ;
  ; does it contain a (return exp) stmt?                           ;
  ;  i.e., (setq name exp) -or- (return exp)                       ;
  (cond ((or (null stmt) (atom stmt)) nil)
	((and (equal (car stmt) 'setq) (equal (cadr stmt) name)) t)
	((and (equal (car stmt) 'return) (cdr stmt)) t)
	((eval (cons 'or
		     (foreach st in stmt collect (gfunctionp st name)))))))

(defun implicitp (type)
  (prog (xtype ximp r)
	(setq xtype (explode2 type))
	(setq ximp (explode2 'implicit))
	(setq r t)
	(repeat (setq r (and r (equal (car xtype) (car ximp))))
		(or (null (setq xtype (cdr xtype)))
		    (null (setq ximp (cdr ximp)))))
	(return r)))

(defun inttypep (type)
  (cond ((member type '(integer int long short)))
	((prog (xtype xint r)
	       (setq xtype (explode2 type))
	       (setq xint (explode2 'integer))
	       (setq r t)
	       (repeat (setq r (and r (equal (car xtype) (car xint))))
		       (or (null (setq xtype (cdr xtype)))
			   (null (setq xint (cdr xint)))))
	       (return r)))))


;;                      ;;
;;  8. misc. functions  ;;
;;                      ;;


(defun complexdop (dostmt)
  (and (lispdop dostmt)
       (or (> (length (cadr dostmt)) 1)
	   (> (length (caddr dostmt)) 1))))

(defun formtypelists (varlists)
  ; ( (var type d1 d2 ..)         ( (type (var d1 d2 ..) ..)   ;
  ;    .                     -->     .                         ;
  ;    .                             .                         ;
  ;   (var type d1 d2 ..) )         (type (var d1 d2 ..) ..) ) ;
  (prog (type typelists tl)
	(foreach vl in varlists do
		 (progn
		  (setq type (cadr vl))
		  (cond ((onep (length (setq vl (delete1 type vl))))
			 (setq vl (car vl))))
		  (cond ((setq tl (assoc type typelists))
			 (setq typelists (delete1 tl typelists)))
			(t
			 (setq tl (list type))))
		  (setq typelists (aconc typelists (aconc tl vl)))))
	(return typelists)))

(defun insertcommas (lst)
  (prog (result)
	(cond ((null lst) (return nil)))
	(setq result (list (car lst)))
	(while (setq lst (cdr lst))
	       (setq result (cons (car lst)
				  (cons '|,| result))))
	(return (reverse result))))


(defun noerrmevalp (pred)
  ;mevalp without call to merror
  (let ((ans (mevalp1 pred)))
       (cond ((member ans '(t nil)) ans)
	     (t '$unknown))))

(defun simplifydo (dostmt)
  (prog (varlst exitlst stmtlst result tmp1 tmp2)
	(cond ((not (lispdop dostmt)) (return dostmt)))
	(setq varlst (reverse (cadr dostmt)))
	(setq exitlst (caddr dostmt))
	(setq stmtlst
	      (foreach st in (cdddr dostmt) collect (simplifydo st)))
	(setq result
	      (foreach st in (cdr exitlst) collect (simplifydo st)))
	(setq exitlst (list (car exitlst)))
	(foreach var in (cdr varlst) do
		 (progn
		  (setq tmp1 (cons (mkassign (car var) (cadr var)) tmp1))
		  (cond ((cddr var)
			 (setq tmp2
			       (cons (mkassign (car var) (caddr var)) tmp2))))))
	(setq varlst (list (car varlst)))
	(setq result (cons (mkdo varlst exitlst (append stmtlst tmp2)) result))
	(setq result (append tmp1 result))
	(return result)))

(defun seqtogp (lst)
  (cond ((or (null lst) (atom lst) (lispstmtp lst) (lispstmtgpp lst))
	 lst)
	((and (onep (length lst)) (listp (car lst)))
	 (seqtogp (car lst)))
	((mkstmtgp 0 (foreach st in lst collect (seqtogp st))))))


(defun stripdollar1 (x)
  (cond ((not (atom x))
	 (cond ((and (eq (caar x) 'bigfloat)
		     (not (minusp (cadr x))))
		(implode (fpformat x)))
	       (t
		(merror "atomic arg required" x))))
	((numberp x)
	 x)
	((member (char (string x) 0) '(#\$ #\% #\&))
	 (intern (subseq (string x) 1)))
	(t
	 x)))
