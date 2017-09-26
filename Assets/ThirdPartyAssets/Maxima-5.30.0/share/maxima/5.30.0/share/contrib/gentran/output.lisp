


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
;;  output.l     ;;    code formatting & printing
;;  -----------  ;;        and error handler


(declare-top (special poport fortlinelen* fortcurrind* ratlinelen*
	ratcurrind* clinelen* ccurrind* *cr* *slash*))
;;                                        ;;
;;  code formatting & printing functions  ;;
;;                                        ;;


;;  fortran code formatting & printing functions  ;;

(defun formatfort (lst)
  (foreach c in *outchanl* do
     (let ((*standard-output* c))
       (formatfort1 lst))))

(defun formatfort1 (lst)
  (foreach elt in lst do
	   (cond ((listp elt)
		  (eval elt))
		 (t
		  (progn
		   (cond ((> (+ (posn) (length (explode2 elt))) fortlinelen*)
			  (fortcontline)))
		   (princ elt))))))

(defun fortcontline ()
  (progn
   (terpri)
   (princ "     .")
   (forttab (- fortcurrind* 6))
   (spaces 1)))

(defun forttab (n)
  (progn
   (setq fortcurrind* (min (+ n 6) (- fortlinelen* 40)))
   (spaces (- fortcurrind* (posn)))))

;;  ratfor code formatting & printing functions  ;;

(defun formatrat (lst)
  (foreach c in *outchanl* do
     (let ((*standard-output* c))
       (formatrat1 lst))))

(defun formatrat1 (lst)
  (foreach elt in lst do
	   (cond ((listp elt)
		  (eval elt))
		 (t
		  (progn
		   (cond ((> (+ (posn) (length (explode2 elt)))
				    ratlinelen*)
			  (ratcontline)))
		   (princ elt))))))

(defun ratcontline ()
  (progn
   (terpri)
   (rattab ratcurrind*)
   (spaces 1)))

(defun rattab (n)
  (progn
   (setq ratcurrind* (min n (- ratlinelen* 40)))
   (spaces (- ratcurrind* (posn)))))

;;  c code formatting & printing functions  ;;

(defun formatc (lst)
  (foreach c in *outchanl* do
     (let ((*standard-output* c))
       (formatc1 lst))))

(defun formatc1 (lst)
  (foreach elt in lst do
	   (cond ((listp elt)
		  (eval elt))
		 (t
		  (progn
		   (cond ((> (+ (posn) (length (explode2 elt)))
				    clinelen*)
			  (ccontline)))
		   (princ elt))))))

(defun ccontline ()
  (progn
   (terpri)
   (ctab ccurrind*)
   (spaces 1)))

(defun ctab (n)
  (progn
   (setq ccurrind* (min n (- clinelen* 40)))
   (spaces (- ccurrind* (posn)))))


;;                             ;;
;;  general printing function  ;;
;;                             ;;


(defun pprin2 (arg)
  (if (eq arg *cr*)
      (foreach c in *outchanl* do (terpri c))
      (foreach c in *outchanl* do (princ arg c))))


;;                 ;;
;;  error handler  ;;
;;                 ;;


;;  error & warning message printing routine  ;;
(defun gentranerr( msgtype exp msg1 msg2)
  (if (eq msgtype 'e) ($error exp msg1 msg2) (mtell exp msg1 msg2)))

#+nil(defun gentranerr (msgtype exp msg1 msg2)
  (prog (holdich holdoch c)
	(setq holdich (rds *standard-input*))
	(setq holdoch (wrs *error-output*))
	(terpri)
	(cond (exp
	       (prettyprint exp)))
	(cond ((equal msgtype 'e)
	       (progn
		(rds (cdr *stdin*))
		(wrs (cdr *stdout*))
		(rederr msg1))))
	(princ "*** ")
	(princ msg1)
	(terpri)
	(cond (msg2
	       (repeat
		(unless (member c (list '| |  *cr*))
		  (format t  "~%    ~a" msg2)
		  (format t " (y~an)  " *slash*))
		(member (setq c (read-char t nil nil)) '(y y n n)))))
	(wrs holdoch)
	(rds holdich)
	(cond ((member c '(n n))
	       (error "Unknown error")))))
