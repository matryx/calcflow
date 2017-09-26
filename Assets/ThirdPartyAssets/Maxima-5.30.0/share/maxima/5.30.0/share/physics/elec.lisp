;;; -*- LISP -*-				5 October 1979
;;;
;;; Electrical

;;; Setup for the Macsyma world

;; Make PARALLEL(arg1,arg2,...,argN) mean arg1 | arg2 | arg3 | ... | argN

(meval '(($ALIAS) $PARALLEL &/| ))

 ;; Make "|" be an N-ary operator

($NARY '&/|)

 ;; Redefine label chars...
 ;;  (Cn) => (INn)
 ;;  (Dn) => (OUTn)
 ;;  (En) => (AUXn)

(SETQ $INCHAR '$IN $OUTCHAR '$OUT $LINECHAR '$AUX)

 ;; Set up variables

;;; Supporting Macros

 ;; (MAKE-CIRCULAR-LIST <list>) returns a pointer to a circular list with
 ;;  elements of <list>

(DEFUN MAKE-CIRCULAR-LIST MACRO (FORM)
       `((LAMBDA (X) (NCONC X X)) (APPEND ,(CADR FORM) NIL)))

;;; Define a "|" function which computes parallel resistances.

(DEFUN $/| N
       (LET ((COMPONENTS (LISTIFY N)))
	    `((MQUOTIENT)
	      ((MTIMES) ,@COMPONENTS)
	      ((MPLUS)  ,@(PERMUTATIONS-OF-<N-1> N COMPONENTS)))))

(DEFUN PERMUTATIONS-OF-<N-1> (N-COMPONENTS COMPONENTS-LIST)
  (DO ((RETURN-VALUE ())
       (I 1. (1+ I))
       (CYCLE (MAKE-CIRCULAR-LIST COMPONENTS-LIST)
	      (CDR CYCLE)))
      ((> I N-COMPONENTS) RETURN-VALUE)
    (PUSH (DO ((I 2. (1+ I))
	       (RETURN-VALUE () (CONS (CAR L) RETURN-VALUE))
	       (L CYCLE (CDR L)))
	      ((> I N-COMPONENTS)
	       (CONS '(MTIMES) RETURN-VALUE)))
	  RETURN-VALUE)))
