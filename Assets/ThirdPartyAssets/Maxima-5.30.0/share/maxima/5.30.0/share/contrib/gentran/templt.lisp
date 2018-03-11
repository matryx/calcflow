


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
;;  templt.l     ;;    template processing routines
;;  -----------  ;;

(declare-top (special *cr* *slash* *vexptrm))

;;                               ;;
;;  1. text processing routines  ;;
;;                               ;;


;;  fortran  ;;


(defun procforttem ()
  (prog (c)
	(setq c (procfortcomm))
	(while (not (eq c '$eof$))
	       (cond ((eq c *cr*)
		      (progn (pprin2 *cr*)
			     (setq c (procfortcomm))))
                     ((eq c '<)
		      (setq c (read-char nil nil '$eof$))
		      (cond ((eq c '<)
			     (setq c (procactive)))
			    (t
			     (pprin2 '<)
			     (pprin2 c)
			     (setq c (read-char nil nil '$eof$)))))
		     (t
		      (progn (pprin2 c)
			     (setq c (read-char nil nil '$eof$))))))))

(defun procfortcomm ()
  ; <col 1>c ... <cr> ;
  ; <col 1>c ... <cr> ;
  ; <col 1>* ... <cr> ;
  (prog (c)
	(while (member (setq c (read-char nil nil '$eof$)) '(c c *))
	       (progn (pprin2 c)
		      (repeat (pprin2 (setq c (read-char nil nil '$eof$)))
			      (eq c *cr*))))
	(return c)))


;;  ratfor  ;;


(defun procrattem ()
  (prog (c)
	(setq c (read-char nil nil '$eof$))
	(while (not (eq c '$eof$))
	       (cond ((eq c '|#|)
		      (setq c (procratcomm)))
                     ((eq c '<)
		      (setq c (read-char nil nil '$eof$))
		      (cond ((eq c '<)
			     (setq c (procactive)))
			    (t
			     (pprin2 '<)
			     (pprin2 c)
			     (setq c (read-char nil nil '$eof$)))))
		     (t
		      (progn (pprin2 c)
			     (setq c (read-char nil nil '$eof$))))))))

(defun procratcomm ()
  ; # ... <cr> ;
  (prog (c)
	(pprin2 '|#|)
	(while (not (eq (setq c (read-char nil nil '$eof$)) *cr*))
	       (pprin2 c))
	(pprin2 *cr*)
	(return (read-char nil nil '$eof$))))


;;  c  ;;


(defun procctem ()
  (prog (c)
	(setq c (read-char nil nil '$eof$))
	(while (not (eq c '$eof$))
	       (cond ((eq c *slash*)
		      (setq c (procccomm)))
                     ((eq c '<)
		      (setq c (read-char nil nil '$eof$))
		      (cond ((eq c '<)
			     (setq c (procactive)))
			    (t
			     (pprin2 '<)
			     (pprin2 c)
			     (setq c (read-char nil nil '$eof$)))))
		     (t
		      (progn (pprin2 c)
			     (setq c (read-char nil nil '$eof$))))))))

(defun procccomm ()
  ; /* ... */ ;
  (prog (c)
	(pprin2 *slash*)
	(setq c (read-char nil nil '$eof$))
	(cond ((eq c '*)
	       (progn (pprin2 c)
		      (setq c (read-char nil nil '$eof$))
		      (repeat (progn (while (not (eq c '*))
					    (progn (pprin2 c)
						   (setq c (read-char nil nil '$eof$))))
				     (pprin2 c)
				     (setq c (read-char nil nil '$eof$)))
			      (eq c *slash*))
		      (pprin2 c)
		      (setq c (read-char nil nil '$eof$)))))
	(return c)))


;;                                        ;;
;;  2. template file active part handler  ;;
;;                                        ;;


(defun procactive ()
  ;  procactive reads vaxima expressions and statements inside "<<" and ">>"  ;
  ;  and mevals each.                                                         ;
  (prog (vexp vexptrm c)
   loop (setq vexp ($readvexp *currin*))
	(setq vexptrm *vexptrm)
	(meval vexp)
	(cond ((member vexptrm '(#\NULL #\>))
	       (return (cond ((equal (setq c (read-char nil nil '$eof$)) *cr*)
			      (read-char nil nil '$eof$))
			     (c)))))
	(go loop)))

(defun $readvexp (in)
  ;  $readvexp is the parser for expressions and statements        ;
  ;  inside "<<" and ">>" within the template file.                ;
  ;                                                                ;
  ;  "/*  ...  */" can be used for comments inside "<<" and ">>".  ;
  (prog (test oldst st iport)
      (setq iport (cdr in))
   loop (setq test (tyi iport))
   c    (cond ((and (equal test #\*)
		    st
		    (equal (car st) #\/))
	        (do ((ch1 (tyi iport) ch2) (ch2))
		   ((or (and (equal ch1 #\*) (equal ch2 #\/))
			(equal ch2 #\NULL))
		    (setq st (cdr st)))
		   (setq ch2 (tyi iport)))
	        (go loop))
	      ((member test '(#\space #\tab #\linefeed))
	       (cond ((null st) (go loop))))
	      ((and (equal test #\>)
		    st
		    (equal (car st) #\>))
	       (setq *vexptrm test)
	       (go d))
	      ((member test '(#\; #\$ #\NULL))
	       (setq *vexptrm test)
	       (go d))
	      ((equal test #\\)
	       (setq st (cons test st)
		     test (tyi iport))))
	(setq st (cons test st))
	(go loop)
   d    (cond ((and (null st)
		    (not (member *vexptrm '(#\linefeed #\NULL #\>))))
	       (go loop)))
	(setq oldst st)
	(cond ((null st) (return nil))
	      ((setq test (let (*prompt-on-read-hang*) (mread iport nil))) (return (third test))))
	(setq test (tyi iport))
	(go c)))
