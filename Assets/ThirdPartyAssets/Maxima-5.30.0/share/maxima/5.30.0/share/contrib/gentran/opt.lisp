


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
;;  opt.l     ;;    interface to the code optimizer
;;  --------  ;;       for local optimizations

(declare-top (special *cr*))

(defun opt (code)
  (opt1 code))

(defun opt1 (code)
  (cond ((atom code) code)
	((atom (car code))
	 (cond ((eq (car code) 'setq) (opt2 (list code)))
	       (t (cons (opt1 (car code)) (opt1 (cdr code))))))
	((eq (caar code) 'setq)
	 (prog (setqseq)
	       (setq setqseq (list (car code)))
	       (while (and (setq code (cdr code))
			   (listp (car code))
			   (eq (caar code) 'setq))
		      (setq setqseq (aconc setqseq (car code))))
	       (setq setqseq (opt2 setqseq))
	       (setq code (opt1 code))
	       (return (cons setqseq code))))
	(t (cons (opt1 (car code)) (opt1 (cdr code))))))


(defun opt2 (setqseq)
  (prog (p)
	; 1. take a list of setq's and write each one in reduce infix form ;
	;    to the file ##tmp1                                            ;
	(gentranoutpush '("##tmp1") nil)
	(foreach s in setqseq do
		 (progn
		  (formatrat (ratcode (list (cadr s))))
		  (pprin2 ":=")
		  (formatrat (ratcode (list (caddr s))))
		  (pprin2 "$")
		  (pprin2 *cr*)))
	(pprin2 "end$")
        (gentranpop '(nil))
	; 2. start a reduce job and load the optimizer.                       ;
	;    optimize contents of ##tmp1 & write (infix) results to ##tmp2.   ;
	;    read in ##tmp2 and write lisp prefix equivalent forms to ##tmp3. ;
	(exec "reduce < ~barbg//gentran//lisp//opt.red")
	; 3. read ##tmp3 back into vaxima. ;
	(setq p (open "##tmp3" :direction :input))
	(setq setqseq (read p))
	(cond ((> (length setqseq) 1)
	       (setq setqseq (cons 'progn setqseq))))
	(close p)
	; 4. (remove temporary files.) ;
	(exec "rm ##tmp[1-3]")
	(return setqseq)))
