;;; -*- Lisp -*-
;;; DIFFOP: A Library for making ' a more useful character in Macsyma
;;;
;;; Loading this file sets things up so that you can do
;;;
;;;	DEPENDS(F,X); => [F(X)]
;;;
;;;	F';	      => dF/dX
;;;
;;;                        3
;;;	F'3; or F'''; =>  d F
;;;			 -----
;;;			    3
;;;			  dX
;;;
;;;  If a variable has more than one DEPENDS property, the variable
;;;   which will be used is undefined.
;;;  If a variable has no DEPENDS property, UND will be used as the
;;;   variable to differentiate by.

(DEFUN INFER-DEPENDENCY (X)
  (OR (CAR (MGET (CADR ($LISTOFVARS X)) 'DEPENDS)) '$UND))

(DECLARE (SPECIAL STRING)) 

 ;Makes awfully big assumptions about the internals of GRAM

(DEFUN PARSE-PRIME (OP LEFT) 
  (SETQ LEFT (CDR LEFT))
  (CONS '$ANY 
	(LIST '($DIFF)
	      LEFT
	      (INFER-DEPENDENCY LEFT)
	      (+ -1
		 (FLATC OP)
		 (COND ((AND STRING (NUMBERP (CAR STRING)))
			(POP STRING))
		       (T 0))))))

(DEFPROP $/' 195. LBP)

(DEFPROP $/' PARSE-PRIME LED)

(DEFPROP $/'/' 195. LBP)

(DEFPROP $/'/' PARSE-PRIME LED)
