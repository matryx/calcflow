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
;;  --------  ;;
;;  pre.l     ;;    preprocessing module
;;  --------  ;;

(declare-top (special *gentranopt gentranopt* *gentranlang))

(defun preproc (exp)
  (prog (r)
	(setq r (preproc1 exp))
	(cond (r (return (car r)))
	      (t (return r)))))


(defun preproc1 (exp)
  (cond ((atom exp)
	 (list exp))
	((or (atom (car exp))
	     (listp (caar exp)))
	 (list (foreach e in exp conc (preproc1 e))))
	((and *gentranopt
	      (member gentranopt* '(vaxima macsyma $vaxima $macsyma))
              (macassignp exp)
	      (not (macdefp exp))
	      (not (macnestassignp exp))
	      (not (macmatassignp exp)))
	 (setq exp (foreach e in exp conc (preproc1 e)))
	 (prog (lhs rhs tvarlist tvartype tassigns)
	       (setq lhs (cadr exp))
	       (setq rhs ($optimize (caddr exp)))
	       (cond ((macexpp rhs)
		      (return (list (list '(msetq) lhs rhs)))))
	       (setq rhs (cdr rhs))
	       (cond ((and (listp (car rhs))
			   (listp (caar rhs))
			   (equal (caaar rhs) 'mlist))
		      (setq tvarlist (cdar rhs))
		      (setq rhs (cdr rhs))
		      (setq tvartype (getvartype (cond ((atom lhs) lhs)
						       (t (car lhs)))))
		      (foreach tv in tvarlist do
			       (prog (v)
				     (setq v (tempvar tvartype))
				     (markvar v)
				     (putprop tv v '*varname*)
				     (setq rhs (subst v tv rhs))))
		      (foreach tv in tvarlist do
			       (progn
				(unmarkvar (get tv '*varname*))
				(putprop tv nil '*varname*)))
		      (setq rhs (reverse rhs))
		      (setq tassigns (reverse (cdr rhs)))
		      (setq rhs (car rhs))))
	       (cond (tassigns
		      (return (list (append1 (cons '(mprogn) tassigns)
					     (list '(msetq) lhs rhs)))))
		     (t
		      (return (list (list '(msetq) lhs rhs)))))))
	((member (stripdollar1 (caar exp)) '(lsetq rsetq lrsetq))
	 ; (($lsetq  ~) (name d1 d2 ... dn) exp)                               ;
	 ;   -->  ((msetq) ((name) (($eval) d1) (($eval) d2) ... (($eval) dn)) ;
         ;	           exp)                                                ;
	 ; (($rsetq  ~) var exp)                                               ;
	 ;   -->  ((msetq) var (($eval) exp))                                  ;
	 ; (($lrsetq ~) ((name) d1 d2 ... dn) exp)                             ;
	 ;   -->  ((msetq) (name (($eval) d1) (($eval) d2) ... (($eval) dn))   ;
         ;                 (($eval) exp))                                      ;
	 (prog (op lhs rhs)
	       (setq op (stripdollar1 (caar exp)))
	       (setq lhs (cadr exp))
	       (setq rhs (caddr exp))
	       (cond ((and (member op '(lsetq lrsetq))
			   (listp lhs))
		      (setq lhs
			    (cons (list (caar lhs))
				  (foreach s in (cdr lhs) collect
					   (list '($eval) s))))))
	       (cond ((member op '(rsetq lrsetq))
		      (setq rhs (list '($eval) rhs))))
	       (return (preproc1 (list '(msetq) lhs rhs)))))
	((equal (stripdollar1 (caar exp)) 'eval)
	 (preproc1 (meval (cadr exp))))
	((and (equal (caar exp) 'msetq)
	      (listp (caddr exp))
	      (listp (caaddr exp))
	      (equal (caaaddr exp) 'lambda))
	 ; store subprogram name & parameters in symbol table ;
	 (symtabput (stripdollar1 (cadr exp))
		    '*params*
		    (foreach p in (cdadaddr exp) collect (stripdollar1 p)))
	 (list (foreach e in exp conc (preproc1 e))))
	((equal (caar exp) 'mdefine)
	 ; store subprogram name & parameters in symbol table ;
	 (symtabput (stripdollar1 (caaadr exp))
		    '*params*
		    (foreach p in (cdadr exp) collect (stripdollar1 p)))
	 (list (foreach e in exp conc (preproc1 e))))
	((equal (stripdollar1 (caar exp)) 'type)
	  ; store type declarations in symbol table ;
	  (setq exp (car (preproc1 (cdr exp))))
	  (setq exp (preprocdec exp))
	  (foreach var in (cdr exp) do
		   (cond ((member (car exp) '(subroutine function))
			  (symtabput var '*type* (car exp)))
			 (t
			  (symtabput nil
				     (cond ((atom var) var)
					   (t (caar var)))
				     (cond ((atom var) (list (car exp)))
					   (t (cons (car exp) (cdr var))))))))
	 nil)
	((equal (stripdollar1 (caar exp)) 'body)
	 ; (($body) stmt1 stmt2 ... stmtn)                                  ;
	 ; -->                                                              ;
	 ; if a main (fortran or ratfor) program is being generated then    ;
	 ;    ((mprogn) stmt1 stmt2 ... stmtn (($stop)) (($end)))           ;
	 ; else if fortran or ratfor then                                   ;
	 ;    ((mprogn) stmt1 stmt2 ... stmtn ((mreturn)) (($end)))         ;
	 ; else c                                                           ;
	 ;    ((mprog) stmt1 stmt2 ... stmtn)                               ;
	 (cond ((eq *gentranlang 'c)
		(preproc1 (cons '(mprog) (cdr exp))))
	       (t
		(progn
		 (setq exp (reverse (cons '(mprogn) (cdr exp))))
		 (cond ((equal (car *symboltable*) '*main*)
			(setq exp (cons '(($end)) (cons '(($stop)) exp))))
		       (t
			(setq exp (cons '(($end)) (cons '((mreturn)) exp)))))
		 (preproc1 (reverse exp))))))
	((member (stripdollar1 (caar exp)) '(subroutine function cprocedure))
	 ; store subprogram name, (subprogram type), (return value type),   ;
	 ; parameter list in symbol table                                   ;
	 ; (($subroutine/$function/$cprocedure) {&type} (($name) $p1..$pn)) ;
	 ;  --> ((mdefine) (($name) $p1..$pn))                              ;
	 (prog (decs)
	       (cond ((member *gentranlang '(fortran ratfor) :test #'eq)
		      (setq decs (list `(($type) ,(caar exp)
						 ,(caaar (last exp)))))))
	       (cond ((equal (length exp) 3)
		      (setq decs
			    (aconc decs
				   `(($type) ,(cadr exp)
					     ,(caaar (last exp)))))))
	       (setq decs
		     (cons '(mdefine)
			   (cons (car (last exp))
				 decs)))
	       (return (preproc1 decs))))
	(t
	 (list (foreach e in exp conc (preproc1 e))))))

(defun preprocdec (arg)
  ; $var  -->  var                           ;
  ; |$implicit type|  -->  implicit\ type    ;
  ; ((mtimes) $type int)  -->  type*int      ;
  ; ((mplus) $v1 ((mminus) $v2))  -->  v1-v2 ;
  (cond ((atom arg)
	 (stripdollar1 arg))
	((atom (car arg))
	 (foreach a in arg collect (preprocdec a)))
	((equal (caar arg) 'mtimes)
	 (intern (compress (append (append (explode (stripdollar1 (cadr arg)))
					   (explode '*))
				   (explode (caddr arg))))))
	((equal (caar arg) 'mplus)
	 (intern (compress (append (append (explode (stripdollar1 (cadr arg)))
					   (explode '-))
				   (explode (stripdollar1 (cadaddr arg)))))))
	(t
	 (foreach a in arg collect (preprocdec a)))))
