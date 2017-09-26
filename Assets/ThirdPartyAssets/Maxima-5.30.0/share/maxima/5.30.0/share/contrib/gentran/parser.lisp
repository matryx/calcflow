


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
;;  parser.l     ;;    gentran parser module
;;  -----------  ;;
(declare-top (special *reswds*))

;;                                            ;;
;;  2. vaxima internal representation parser  ;;
;;                                            ;;


(defun gentranparse (forms)
  (foreach f in forms do
	   (cond ((not (or (pmstmt f)
			   (pmexp f)
			   (pmlogexp f)))
		  (gentranerr 'e f "cannot be translated" nil)))))

(defun pmexp (s)
  ; exp  ::=  const | var | funcall | ((mminus ~) exp) |       ;
  ;           ((mquotient ~) exp exp) | ((rat ~) exp exp) |    ;
  ;           ((mexpt ~) exp exp) | ((mplus ~) exp exp exp') | ;
  ;           ((mtimes ~) exp exp exp')                        ;
  ; funcall  ::=  ((id ~) exp')                                ;
  (cond ((atom s)
	 (or (pmconst s)
	     (pmid s)))
	((and (listp s)
	      (listp (car s)))
	 (cond ((pmidop (car s))
		(pmexp1 (cdr s)))
	       ((pmmminusop (car s))
		(and (equal (length s) 2)
		     (pmexp (cadr s))))
	       ((or (pmmquotientop (car s))
		    (pmratop (car s))
		    (pmmexptop (car s)))
		(and (equal (length s) 3)
		     (pmexp (cadr s))
		     (pmexp (caddr s))))
	       ((or (pmmplusop (car s))
		    (pmmtimesop (car s)))
		(and (> (length s) 2)
		     (pmexp (cadr s))
		     (pmexp (caddr s))
		     (pmexp1 (cdddr s))))))))

(defun pmexp1 (s)
  ; exp'  ::=  exp exp' | epsilon ;
  (or (null s)
      (and (pmexp (car s))
	   (pmexp1 (cdr s)))))

(defun pmlogexp (s)
  ; logexp  ::=  t | nil | var | funcall | relexp | ((mnot ~) logexp) | ;
  ;              ((mand ~) logexp logexp logexp') |                     ;
  ;              ((mor ~) logexp logexp logexp')                        ;
  ; relexp  ::=  ((mgreaterp ~) exp exp) | ((mequal ~) exp exp) |       ;
  ;              ((mnotequal ~) exp exp) | ((mlessp ~) exp exp) |       ;
  ;              ((mgeqp ~) exp exp) | ((mleqp ~) exp exp)              ;
  ; funcall  ::=  (id exp')                                             ;
  (cond ((atom s)
	 (or (pmid s)
	     (null s)
	     (equal s t)))
	((and (listp s)
	      (listp (car s)))
	 (cond ((pmidop (car s))
		(pmexp1 (cdr s)))
	       ((or (pmmgreaterpop (car s))
		    (pmmequalop (car s))
		    (pmmnotequalop (car s))
		    (pmmlesspop (car s))
		    (pmmgeqpop (car s))
		    (pmmleqpop (car s)))
		(and (equal (length s) 3)
		     (pmexp (cadr s))
		     (pmexp (caddr s))))
	       ((pmmnotop (car s))
		(and (equal (length s) 2)
		     (pmlogexp (cadr s))))
	       ((or (pmmandop (car s))
		    (pmmorop (car s)))
		(and (> (length s) 2)
		     (pmlogexp (cadr s))
		     (pmlogexp (caddr s))
		     (pmlogexp1 (cdddr s))))))))

(defun pmlogexp1 (s)
  ; logexp'  ::=  logexp logexp' | epsilon ;
  (or (null s)
      (and (pmlogexp (car s))
	   (pmlogexp1 (cdr s)))))

(defun pmstmt (s)
  ; stmt  ::=  assign | nestassign | matassign | cond | for | forin | go |  ;
  ;            label | call | return | stop | end | read | print | stmtgp | ;
  ;            defn                                                         ;
  ; assign  ::=  ((msetq ~) var exp) | ((msetq ~) var logexp)               ;
  ; nestassign  ::=  ((msetq ~) var ((msetq ~) var nestassign'))            ;
  ; nestassign'  ::=  ((msetq ~) var nestassign') | exp | logexp            ;
  ; matassign  ::=  ((msetq ~) var (($matrix ~) list list'))                ;
  ; cond  ::=  ((mcond ~) logexp stmt t $false) |                           ;
  ;            ((mcond ~) logexp stmt t stmt)                               ;
  ; for  ::=  ((mdo ~) varnil exp exp exp exp logexp stmt)                  ;
  ; forin  ::=  ((mdoin ~) var list nil nil nil logexp stmt)                ;
  ; go  ::=  ((mgo ~) label)                                                ;
  ; label  ::=  id                                                          ;
  ; call  ::=  ((id ~) params)                                              ;
  ; return  ::=  ((mreturn ~) retexp)                                       ;
  ; stop  ::=  (($stop ~))                                                  ;
  ; end  ::=  (($end ~))                                                    ;
  ; read  ::=  ((msetq ~) var read')                                        ;
  ; read'  ::=  ((msetq ~) var read') | (($readonly ~) var)                 ;
  ; print  ::=  (($print ~) params)                                         ;
  ; stmtgp  ::=  ((mprog ~) stmt stmt') | ((mprogn ~) stmt stmt') |         ;
  ;              (($ev ~) stmt stmt')                                       ;
  ; defn  ::=  ((msetq ~) id ((lambda ~) list retexp)) |                    ;
  ;            ((mdefine ~) ((id ~) id') retexp) |                          ;
  ;            ((mdefine ~) ((id ~) id') stmt) |                            ;
  ;            ((mdefine ~) ((id ~) id'))                                   ;
  (cond ((atom s)
	 (pmid s))
	((and (listp s)
	      (listp (car s)))
	 (cond ((pmmsetqop (car s))
		(pmmsetq1 (cdr s)))
	       ((pmmcondop (car s))
		(and (> (length s) 4)
		     (pmlogexp (cadr s))
		     (pmstmt (caddr s))
		     (equal (cadddr s) 't)
		     (pmmcond1 (cddddr s))))
	       ((pmmdoop (car s))
		(and (equal (length s) 8)
		     (pmvarnil (cadr s))
		     (pmexp (caddr s))
		     (pmexp (cadddr s))
		     (pmexp (caddddr s))
		     (pmexp (cadddddr s))
		     (pmlogexp (caddddddr s))
		     (pmstmt (cadddddddr s))))
	       ((pmmdoinop (car s))
		(and (equal (length s) 8)
		     (pmvar (cadr s))
		     (pmlist (caddr s))
		     (null (cadddr s))
		     (null (caddddr s))
		     (null (cadddddr s))
		     (pmlogexp (caddddddr s))
		     (pmstmt (cadddddddr s))))
	       ((pmmgoop (car s))
		(and (equal (length s) 2)
		     (pmid (cadr s))))
	       ((pmmreturnop (car s))
		(or (equal (length s) 1)
		    (and (equal (length s) 2)
			 (pmretexp (cadr s)))))
	       ((pm$stopop (car s))
		(equal (length s) 1))
	       ((pm$endop (car s))
		(equal (length s) 1))
	       ((pm$printop (car s))
		(pmparams1 (cdr s)))
	       ((pm$declare_typeop (car s)))
	       ((or (pmmprogop (car s))
		    (pmmprognop (car s))
		    (pm$evop (car s)))
		(and (> (length s) 1)
		     (pmstmt (cadr s))
		     (pmstmt1 (cddr s))))
	       ((pmmdefineop (car s))
		(and (> (length s) 1)
		     (pmidparamop (cadr s))
		     (or (null (cddr s))
			 (pmmdefine1 (cddr s)))))
	       ((pmidop (car s))
		(pmparams1 (cdr s)))))))

(defun pmstmt1 (s)
  ; stmt'  ::=  stmt stmt' | epsilon  ;
  (or (null s)
      (and (pmstmt (car s))
	   (pmstmt1 (cdr s)))))

(defun pmmsetq1 (s)
  (cond ((and (listp s)
	      (atom (car s)))
	 (and (pmid (car s))
	      (pmmsetq2 (cdr s))))
	((and (listp s)
	      (listp (car s)))
	 (and (> (length (car s)) 1)
	      (pmidop (caar s))
	      (pmexp (cadar s))
	      (pmexp1 (cddar s))
	      (pmmsetq3 (cdr s))))))

(defun pmmsetq2 (s)
  (cond ((and (listp s)
	      (listp (car s))
	      (equal (length s) 1)
	      (equal (length (car s)) 3)
	      (pmlambdaop (caar s)))
	 (and (pmlist (cadar s))
	      (pmretexp (caddar s))))
	((pmmsetq3 s))))

(defun pmmsetq3 (s)
  (cond ((and (listp s)
	      (listp (car s))
	      (equal (length s) 1)
	      (> (length (car s)) 1)
	      (pm$matrixop (caar s)))
	 (and (pmlist (cadar s))
	      (pmlist1 (cddar s))))
	((pmmsetq4 s))))

(defun pmmsetq4 (s)
  (cond ((listp s)
	 (cond ((pmexp (car s))
		(null (cdr s)))
	       ((pmlogexp (car s))
		(null (cdr s)))
	       ((and (listp (car s))
		     (pmmsetqop (caar s)))
		(and (equal (length s) 1)
		     (> (length (car s)) 1)
		     (pmvar (cadar s))
		     (pmmsetq4 (cddar s))))
	       ((and (listp (car s))
		     (pm$readonlyop (caar s)))
		(and (equal (length s) 1)
		     (or (equal (length (car s)) 1)
			 (and (equal (length (car s)) 2)
			      (pmvar (cadar s))))))))))

(defun pmmcond1 (s)
  (cond ((equal s '($false)))
	((pmstmt (car s))
	 (null (cdr s)))))

(defun pmidop (s)
  (and (listp s)
       (not (member (car s) *reswds*))))

(defun pmmminusop (s)
  (and (listp s)
       (equal (car s) 'mminus)))

(defun pmmquotientop (s)
  (and (listp s)
       (equal (car s) 'mquotient)))

(defun pmratop (s)
  (and (listp s)
       (equal (car s) 'rat)))

(defun pmmexptop (s)
  (and (listp s)
       (equal (car s) 'mexpt)))

(defun pmmplusop (s)
  (and (listp s)
       (equal (car s) 'mplus)))

(defun pmmtimesop (s)
  (and (listp s)
       (equal (car s) 'mtimes)))

(defun pmmgreaterpop (s)
  (and (listp s)
       (equal (car s) 'mgreaterp)))

(defun pmmequalop (s)
  (and (listp s)
       (equal (car s) 'mequal)))

(defun pmmnotequalop (s)
  (and (listp s)
       (equal (car s) 'mnotequal)))

(defun pmmlesspop (s)
  (and (listp s)
       (equal (car s) 'mlessp)))

(defun pmmgeqpop (s)
  (and (listp s)
       (equal (car s) 'mgeqp)))

(defun pmmleqpop (s)
  (and (listp s)
       (equal (car s) 'mleqp)))

(defun pmmnotop (s)
  (and (listp s)
       (equal (car s) 'mnot)))

(defun pmmandop (s)
  (and (listp s)
       (equal (car s) 'mand)))

(defun pmmorop (s)
  (and (listp s)
       (equal (car s) 'mor)))

(defun pmmsetqop (s)
  (and (listp s)
       (equal (car s) 'msetq)))

(defun pmmcondop (s)
  (and (listp s)
       (equal (car s) 'mcond)))

(defun pmmdoop (s)
  (and (listp s)
       (equal (car s) 'mdo)))

(defun pmmdoinop (s)
  (and (listp s)
       (equal (car s) 'mdoin)))

(defun pmmgoop (s)
  (and (listp s)
       (equal (car s) 'mgo)))

(defun pmmreturnop (s)
  (and (listp s)
       (equal (car s) 'mreturn)))

(defun pm$stopop (s)
  (and (listp s)
       (equal (car s) '$stop)))

(defun pm$endop (s)
  (and (listp s)
       (equal (car s) '$end)))

(defun pm$printop (s)
  (and (listp s)
       (equal (car s) '$print)))

(defun pm$declare_typeop (s)
  (and (listp s)
       (equal (car s) '$declare_type)))

(defun pmmprogop (s)
  (and (listp s)
       (equal (car s) 'mprog)))

(defun pmmprognop (s)
  (and (listp s)
       (equal (car s) 'mprogn)))

(defun pm$evop (s)
  (and (listp s)
       (equal (car s) '$ev)))

(defun pmmdefineop (s)
  (and (listp s)
       (equal (car s) 'mdefine)))

(defun pm$readonlyop (s)
  (and (listp s)
       (equal (car s) '$readonly)))

(defun pmlambdaop (s)
  (and (listp s)
       (equal (car s) 'lambda)))

(defun pm$matrixop (s)
  (and (listp s)
       (equal (car s) '$matrix)))

(defun pmmlistop (s)
  (and (listp s)
       (equal (car s) 'mlist)))

(defun pmidparamop (s)
  (and (listp s)
       (pmidop (car s))
       (pmid1 (cdr s))))

(defun pmmdefine1 (s)
  (and (listp s)
       (equal (length s) 1)
       (or (pmretexp (car s))
	   (pmstmt (car s)))))

(defun pmid1 (s)
  ; id'  ::=  id id' | epsilon ;
  (or (null s)
      (and (pmid (car s))
	   (pmid1 (cdr s)))))

(defun pmvar (s)
  ; var  ::=  id | arrelt          ;
  ; arrelt  ::=  ((id ~) exp exp') ;
  (cond ((atom s)
	 (pmid s))
	((listp s)
	 (and (> (length s) 1)
	      (pmidop (car s))
	      (pmexp (cadar s))
	      (pmexp1 (cddar s))))))

(defun pmvarnil (s)
  ; varnil  ::=  var | nil ;
  (or (null s)
      (pmvar s)))

(defun pmretexp (s)
  ; retexp  ::=  exp | logexp | string | epsilon ;
  (or (null s)
      (pmexp s)
      (pmlogexp s)
      (pmstring s)))

(defun pmparams1 (s)
  ; params  ::=  exp params | logexp params | string params | epsilon ;
  (or (null s)
      (and (pmexp (car s))
	   (pmparams1 (cdr s)))
      (and (pmlogexp (car s))
	   (pmparams1 (cdr s)))
      (and (pmstring (car s))
	   (pmparams1 (cdr s)))))

(defun pmlist (s)
  ; list  ::=  ((mlist ~) exp exp') | ((mlist ~) logexp logexp') ;
  (and (listp s)
       (pmmlistop (car s))
       (pmlist2 (cdr s))))

(defun pmlist2 (s)
  (or (and (pmexp (car s))
	   (pmexp1 (cdr s)))
      (and (pmlogexp (car s))
	   (pmlogexp1 (cdr s)))))

(defun pmlist1 (s)
  ; list'  ::=  list list' | epsilon ;
  (or (null s)
      (and (pmlist (car s))
	   (pmlist1 (cdr s)))))

(defun pmconst (s)
  (or (numberp s)
      (null s)
      (equal s t)))

(defun pmstring (s)
  (and (atom s)
       (equal (car (explodec s)) '&)))

(defun pmid (s)
  (and (atom s)
       (not (member s '(t nil)))))
