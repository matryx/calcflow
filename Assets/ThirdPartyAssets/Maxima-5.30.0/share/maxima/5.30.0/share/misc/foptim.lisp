;;; -*- LISP -*-
;;; Auxiliary routines for OPTIMIZE'ing
;;;
;;; Created by KMP 8:23pm  Friday, 23 February 1979

;;; Syntax is:
;;;
;;;  FOPTIMIZE(A,B,[C,D],[Q]...);
;;;
;;; Elements of the arg list with different forms do different things:
;;;
;;; [1] If an arg is an ATOM, then its function will be redefined with the 
;;;	optimized form.
;;;
;;; [2] If the arg is a 1-length List, then the function named in the list
;;;     will be optimized, and the optimized LAMBDA will be returned.
;;;
;;; [3] If the arg is a 2-length list, then the function named by its
;;;	first arg will be optimized and given the name of the second 
;;;	element of the list.
;;;

;;; $FOPTIMIZE is the name of the driver that gets called from Macsyma

(DEFUN $FOPTIMIZE FEXPR (X) (CONS (NCONS 'MLIST) (MAPCAR 'FOPTIMIZE X)))

;;; FOPTIMIZE is the function that does the work.
;;;  It does type checking but will only do interesting things with
;;;  ATOMS or MLIST's one or two long. 

(DEFUN FOPTIMIZE (X)
       (COND ((SYMBOLP X)
	      (*CATCH 'FOPTIMIZE-NO-MEXPR-DEFINITION
		      (PROGN (MPUTPROP X
				       (FOPTIMIZE-AUX (MGET X 'MEXPR) X)
				       'MEXPR)
			     X)))
	     ((OR (ATOM X)
		  (< (LENGTH X) 2.)
		  (> (LENGTH X) 3.)
		  (NOT (EQ (CAAR X) 'MLIST))
		  (NOT (SYMBOLP (CADR X)))
		  (NOT (SYMBOLP (CADDR X))))
	      (fresh-line)
	      (PRINC '|;FOPTIMIZE called on an illegal form.| TYO)
	      (ERR))
	     (T
	      (LET (((IN OUT) (CDR X)) (DEF ()))
		   (*CATCH 'FOPTIMIZE-NO-MEXPR-DEFINITION
			   (PROGN
			    (SETQ DEF (FOPTIMIZE-AUX (MGET IN 'MEXPR) IN))
			    (COND (OUT (MPUTPROP OUT DEF 'MEXPR) OUT)
				  (T   DEF))))))))

;;; FOPTIMIZE-AUX
;;;  This function is where the LAMBDA is actually optimized.

(DEFUN FOPTIMIZE-AUX (DEF NAME)
       (COND ((NOT DEF)
	      (fresh-line)
	      (PRINC '|;No function definition for | TYO)
	      (PRINC (STRIPDOLLAR NAME) TYO)
	      (PRINC '|. It will be ignored.| TYO)
	      (*THROW 'FOPTIMIZE-NO-MEXPR-DEFINITION '$FAILED)))
       (LIST (NCONS 'LAMBDA)
	     (CADR DEF)
	     ($OPTIMIZE (LIST* (NCONS 'MPROG)
			       (NCONS (NCONS 'MLIST))
			       (CDDR DEF)))))
