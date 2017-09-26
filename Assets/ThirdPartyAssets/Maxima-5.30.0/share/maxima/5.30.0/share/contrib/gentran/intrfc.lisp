


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
;;  intrfc.l     ;;    command parsing routines & control functions
;;  -----------  ;;

(declare-top (special *gentranlang *gentranparser *gentranopt gentranopt*
	*gentranseg piport))

;;                               ;;
;;  1. command parsing routines  ;;
;;                               ;;


;;  command parsers  ;;


(defmspec $gentran (forms)
  ;                                                     ;
  ;  gentran(stmt1,stmt2,...,stmtn {,[f1,f2,...,fm]});  ;
  ;      -->                                            ;
  ;  (gentran (stmt1 stmt2 ... stmtn)                   ;
  ;           (f1 f2 ... fm))                           ;
  ;                                                     ;
  (prog (flist)
	(setq forms (reverse forms))
	(cond ((and (listp (car forms))
		    (listp (caar forms))
		    (equal (caaar forms) 'mlist))
	       (setq flist (cdar forms))
	       (setq forms (cdr forms))))
	(setq forms (cdr (reverse forms)))
	(return (gentran forms flist))))


(defmfun $gentranout (flist)
  ;                                                                     ;
  ;  gentranout(f1,f2,...,fn);  -->  (gentranoutpush (f1 f2 ... fn) t)  ;
  ;                                                                     ;
  (gentranoutpush flist t))

(defun gentranout (flist)
  ;                                                                       ;
  ;  (gentranout (f1 f2 ... fn))  -->  (gentranoutpush (f1 f2 ... fn) t)  ;
  ;                                                                       ;
  (gentranoutpush flist t))


(defmfun $gentranshut (flist)
  ;                                                                 ;
  ;  gentranshut(f1,f2,...,fn);  -->  (gentranshut (f1 f2 ... fn))  ;
  ;                                                                 ;
  (gentranshut flist))


(defmfun $gentranpush (flist)
  ;                                                                        ;
  ;  gentranpush(f1,f2,...,fn);  -->  (gentranoutpush (f1 f2 ... fn) nil)  ;
  ;                                                                        ;
  (gentranoutpush flist nil))

(defun gentranpush (flist)
  ;                                                                          ;
  ;  (gentranpush (f1 f2 ... fn))  -->  (gentranoutpush (f1 f2 ... fn) nil)  ;
  ;                                                                          ;
  (gentranoutpush flist nil))


(defmfun $gentranpop (flist)
  ;                                                               ;
  ;  gentranpop(f1,f2,...,fn);  -->  (gentranpop (f1 f2 ... fn))  ;
  ;                                                               ;
  (gentranpop flist))


(defmfun $gentranin (forms)
  ;                                              ;
  ;  gentranin(f1,f2,...,fn {,[f1,f2,...,fm]});  ;
  ;      -->                                     ;
  ;  (gentranin (f1 f2 ... fn) (f1 f2 ... fm))   ;
  ;                                              ;
  (prog (outflist)
	(setq forms (reverse forms))
	(cond ((and (listp (car forms))
		    (listp (caar forms))
		    (equal (caaar forms) 'mlist))
	       (setq outflist (cdar forms))
	       (setq forms (cdr forms))))
	(setq forms (reverse forms))
	(return (gentranin forms outflist))))


(defmfun $on (flaglist)
  ;                              ;
  ;  on(flag1,flag2,...,flagn);  ;
  ;    -->                       ;
  ;  (onoff flaglist t)          ;
  ;                              ;
  (onoff flaglist t))

(defun on (flaglist)
  ;                                          ;
  ;  (on flaglist)  -->  (onoff flaglist t)  ;
  ;                                          ;
  (onoff flaglist t))


(defmfun $off (flaglist)
  ;                               ;
  ;  off(flag1,flag2,...,flagn);  ;
  ;    -->                        ;
  ;  (onoff flaglist nil)         ;
  ;                               ;
  (onoff flaglist nil))

(defun off (flaglist)
  ;                                             ;
  ;  (off flaglist)  -->  (onoff flaglist nil)  ;
  ;                                             ;
  (onoff flaglist nil))



;;                        ;;
;;  2. control functions  ;;
;;                        ;;


;;  command control functions  ;;


(defun gentran (forms flist)
  (prog ()
	(cond ((setq flist (preproc flist))
	       (eval (list 'gentranoutpush (list 'quote flist) nil))))
	(setq forms (preproc forms))
	(cond (*gentranparser (gentranparse forms)))
	(setq forms (franz forms))
	(cond ((and *gentranopt
		    (equal (setq gentranopt* (stripdollar1 gentranopt*))
				   'reduce)
		    )
	       (setq forms (opt forms))))
	(cond (*gentranseg (setq forms (seg forms))))
	(cond ((eq *gentranlang 'ratfor)
	       (formatrat (ratcode forms)))
	      ((eq *gentranlang 'c)
	       (formatc (ccode forms)))
	      ((formatfort (fortcode forms))))
	(return (cond (flist
		       (progn
			(setq flist (or (car *currout*)
					(cons '(mlist) (cdr *currout*))))
			(eval '(gentranpop '(nil)))
			flist))
		      (t
		       (or (car *currout*)
			   (cons '(mlist)
				 (cdr *currout*))))))))


(defun gentranoutpush (flist outp)
  ;  open, [delete,] push  ;
  (prog (fp)
	(setq flist (fargstonames (preproc flist) t))
	(cond ((onep (length flist))
	       (progn
		(setq fp (or (filpr (car flist) *outstk*)
			     (mkfilpr (car flist))))
		(cond (outp (delstk fp)))
		(pushstk fp)))
	      (t
	       (progn
		(setq fp (foreach f in flist collect
				  (or (filpr f *outstk*) (mkfilpr f))))
		(cond (outp
		       (progn
			(foreach p in fp do (delstk p))
			(delstk (pfilpr flist *outstk*)))))
		(pushstk '(nil))
		(cond (outp
		       (foreach p in fp do (pushstk p)))
		      ((foreach p in fp do
				(cond ((not (member p *outstk*))
				       (pushstk p))))))
		(pushstk (cons nil flist)))))
	(resetstk *outstk*)
	(return (or (car *currout*)
		    (cons '(mlist) (cdr *currout*))))))


(defun gentranshut (flist)
  ;  close, delete, [output to t]  ;
  (prog (trm fp)
	(setq flist (fargstonames (preproc flist) nil))
	(cond ((onep (length flist))
	       (progn
		(setq trm (equal (car *currout*) (car flist)))
		(setq fp (filpr (car flist) *outstk*))
		(close (cdr fp))
		(delstk fp)
		(cond (trm (pushstk *stdout*)))))
	      (t
	       (progn
		(cond ((car *currout*)
		       (setq trm (member (car *currout*) flist)))
		      (t
		       (setq trm
			     (eval (cons 'and
					 (foreach f in (cdr *currout*) collect
						  (cond ((member f flist)
							 t))))))))
		(setq fp (foreach f in flist collect (filpr f *outstk*)))
		(foreach p in fp do (close (cdr p)))
		(foreach p in fp do (delstk p))
		(delstk (pfilpr flist *outstk*))
		(cond (trm (pushstk *stdout*))))))
	(resetstk *outstk*)
	(return (or (car *currout*)
		    (cons '(mlist) (cdr *currout*))))))


(defun gentranpop (flist)
  ;  [close,] delete  ;
  (prog (fp)
	(setq flist (preproc flist))
	(cond ((member '$all flist)
	       (while (> (length *outstk*) 1)
		      (gentranpop '(nil)))
	       (return (car *currout*))))
	(setq flist (fargstonames flist nil))
	(cond ((onep (length flist))
	       (progn
		(setq fp (filpr (car flist) *outstk*))
		(popstk fp)
		(cond ((not (member fp *outstk*)) (close (cdr fp))))))
	      (t
	       (progn
		(setq fp (foreach f in flist collect (filpr f *outstk*)))
		(popstk (pfilpr flist *outstk*))
		(foreach p in fp do
			 (cond ((not (member p *outstk*))
				(close (cdr p))))))))
	(return (or (car *currout*)
		    (cons '(mlist)
			  (cdr *currout*))))))


(defun gentranin (inlist outlist)
  (prog (holdich)
	(foreach inf in (setq inlist (preproc inlist)) do
		 (cond ((listp inf)
			(gentranerr 'e inf "wrong type of arg" nil))
		       ((and (not (filep (mkfil inf))) (not (eq inf 't)))
			(gentranerr 'e inf "nonexistent input file" nil))))
	(cond (outlist
	       (eval (list 'gentranoutpush (list 'quote outlist) nil))))
	(setq holdich (rds nil))
	(foreach inf in inlist do
		 (progn
		  (cond ((equal inf (car *stdin*))
			 (pushinstk *stdin*))
			((filpr inf *instk*)
			 (gentranerr 'e
				     inf
				     "template file already open for input"
				     nil))
			(t
			 (pushinstk (cons inf (infile (mkfil inf))))))
		  (rds (cdr *currin*))
		  (cond ((eq *gentranlang 'ratfor) (procrattem))
			((eq *gentranlang 'c) (procctem))
			(t (procforttem)))
		  (rds holdich)
		  (cond ((cdr *currin*) (close (cdr *currin*))))
		  (popinstk)))
	(return (cond (outlist
		       (progn
			(setq outlist (or (car *currout*)
					  (cons '(mlist) (cdr *currout*))))
			(eval '(gentranpop '(nil)))
			outlist))
		      (t
		       (or (car *currout*)
			   (cons '(mlist) (cdr *currout*))))))))


;;  misc. control functions  ;;



(defun onoff (flags onp)
  (foreach f in flags do
	   (prog (flag funlist)
		 (setq flag (setq f (stripdollar1 f)))
		 (setq f (implode (cons '* (explode f))))
		 (set f onp)
		 (cond ((setq funlist (assoc onp (get flag 'simpfg)))
			(foreach form in (cdr funlist) do (eval form))))))
  '$done)



(defun $tempvar (type)
  (tempvar (stripdollar1 type)))


(defun $markvar (var)
  (markvar (stripdollar1 var))
  var)


(defun $markedvarp (var)
  (markedvarp (stripdollar1 var)))


(defun $unmarkvar (var)
  (unmarkvar (stripdollar1 var))
  '$done)


(defun $recurunmark (exp)
  (cond ((atom exp) (unmarkvar (stripdollar1 exp)))
	(t (foreach elt in exp do ($recurunmark elt))))
  '$done)


(defun $genstmtno ()
  (genstmtno))


(defun $gendecs (name)
  (gendecs name))


;;  file arg conversion function  ;;


(defun fargstonames (args openp)
  (prog (names)
	(setq args
	      (foreach a in (if (listp args) args (list args)) conc
		       (cond ((member a '(nil 0))
			      (cond ((car *currout*)
				     (list (car *currout*)))
				    (t
				     (cdr *currout*))))
			     ((eq a 't)
			      (list (car *stdout*)))
			     ((eq a '$all)
			      (foreach fp in *outstk* conc
				       (cond ((and (car fp)
						   (not (equal fp *stdout*)))
					      (list (car fp))))))
			     ((atom a)
			      (cond (openp
				     (progn (list a)))
				    ((filpr a *outstk*)
				     (list a))
				    (t
				     (gentranerr 'w
						 a
						 "file not open for output"
						 nil))))
			     (t
			      (gentranerr 'e a "wrong type of arg" nil)))))
	(repeat
	 (cond ((not (member (car args) names))
		(setq names (aconc names (car args)))))
	 (null (setq args (cdr args))))
	(return names)))


;;  mode switch control functions  ;;


(defun gentranswitch (lang)
  ;                   ;
  ;  on/off fortran;  ;
  ;  on/off ratfor;   ;
  ;  on/off c;        ;
  ;                   ;
  (prog (hlang flag exp)
	(setq hlang *gentranlang)
	(setq *gentranlang lang)
	(setq flag (intern (compress (append (explode '*)
					     (explode lang)))))
	(while (eval flag)
	       (progn
		(setq exp (gentranswitch1 (list ($readvexp (cons nil *currin*))
						                   )))
		(eval (list 'gentran (list 'quote exp) 'nil))))
	(setq *gentranlang hlang)))

(defun gentranswitch1 (exp)
  (prog (r)
	(setq r (gentranswitch2 exp))
	(cond (r (return (car r)))
	      (t (return r)))))

(defun gentranswitch2 (exp)
  (cond ((atom exp)
	 (list exp))
((and (listp (car exp))
	      (member (caar exp) '(off $off)))
	 (foreach f in (cdr exp) do
		  (onoff (list f) nil)))
	(t
	 (list (foreach e in exp conc (gentranswitch2 e))))))


(defun gendecs (name)
  ;                        ;
  ;  on/off gendecs;       ;
  ;                        ;
  ;  gendecs subprogname;  ;
  ;                        ;
  (progn
   (cond ((equal name 0)
	  (setq name nil)))
   (cond ((eq *gentranlang 'ratfor)
	  (formatrat (ratdecs (symtabget name '*decs*))))
	 ((eq *gentranlang 'c)
	  (formatc (cdecs (symtabget name '*decs*))))
	 ((formatfort (fortdecs (symtabget name '*decs*)))))
   (symtabrem name nil)
   (symtabrem name '*decs*)
   '$done))


;;  misc. control functions  ;;


(defun gentranpairs (prs)
  ;                                ;
  ;  gentranpairs dottedpairlist;  ;
  ;                                ;
  (progn
   (cond ((eq *gentranlang 'ratfor)
	  (foreach pr in prs do
		   (formatrat (mkfratassign (car pr) (cdr pr)))))
	 ((eq *gentranlang 'c)
	  (foreach pr in prs do
		   (formatc (mkfcassign (car pr) (cdr pr)))))
	 ((foreach pr in prs do
		   (formatfort (mkffortassign (car pr) (cdr pr))))))))
