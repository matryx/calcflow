;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module troper)

;;; The basic OPERATORS properties translators.

(def%tr mminus (form)
  (setq form (translate (cadr form)))
  (cond ((numberp (cdr form))
	 `(,(car form) . ,(- (cdr form))))
	((eq '$fixnum (car form)) `($fixnum - ,(cdr form)))
	((eq '$float (car form)) `($float - ,(cdr form)))
	((eq '$number (car form)) `($number - ,(cdr form)))
	((eq '$rational (car form))
	 (cond ((and (not (atom (caddr form))) (eq 'rat (caar (caddr form))))
		(setq form (cdaddr form))
		`($rational quote ((rat) ,(- (car form)) ,(cadr form))))
	       (t `($rational rtimes -1 ,(cdr form)))))
	(t `($any . (*mminus ,(cdr form))))))

(def%tr mplus (form)
  (let   (args mode)
    (do ((l (cdr form) (cdr l))) ((null l))
      (setq args (cons (translate (car l)) args)
	    mode (*union-mode (car (car args)) mode)))
    (setq args (nreverse args))
    (cond ((eq '$fixnum mode) `($fixnum + . ,(mapcar #'cdr args)))
	  ((eq '$float mode) `($float + . ,(mapcar #'dconv-$float args)))
	  ((eq '$rational mode) `($rational rplus . ,(mapcar #'cdr args)))
	  ((eq '$number mode) `($number + . ,(mapcar #'cdr args)))
	  (t `($any add* . ,(mapcar #'dconvx args))))))

(def%tr mtimes (form)
  (let (args mode)
    (cond ((equal -1 (cadr form))
	   (translate `((mminus) ((mtimes) . ,(cddr form)))))
	  (t
	   (do ((l (cdr form) (cdr l)))
	       ((null l))
	     (setq args (cons (translate (car l)) args)
		   mode (*union-mode (car (car args)) mode)))
	   (setq args (nreverse args))
	   (cond ((eq '$fixnum mode) `($fixnum * . ,(mapcar #'cdr args)))
		 ((eq '$float mode) `($float * . ,(mapcar #'dconv-$float args)))
		 ((eq '$rational mode) `($rational rtimes . ,(mapcar #'cdr args)))
		 ((eq '$number mode) `($number * . ,(mapcar #'cdr args)))
		 (t `($any mul* . ,(mapcar #'dconvx args))))))))


(def%tr mquotient (form)
  (let (arg1 arg2 mode)
    (setq arg1 (translate (cadr form))
	  arg2 (translate (caddr form))
	  mode (*union-mode (car arg1) (car arg2))
	  arg1 (dconv arg1 mode)
	  arg2 (dconv arg2 mode))
    (cond ((eq '$float mode)
	   (setq arg1 (if (member arg1 '(1 1.0) :test #'equal)
			  (list arg2)
			  (list arg1 arg2)))
	   `($float / . ,arg1))
	  ((and (eq mode '$fixnum) $tr_numer)
	   `($float . (/ (float ,arg1) (float ,arg2))))
	  ((member mode '($fixnum $rational) :test #'eq)
	   `($rational rremainder ,arg1 ,arg2))
	  (t `($any div ,arg1 ,arg2)))))

(defvar $tr_exponent nil
  "If True it allows translation of x^n to generate (expt $x $n) if $n is fixnum and $x is fixnum, or number")

(def%tr mexpt (form)
  (if (eq '$%e (cadr form)) (translate `(($exp) ,(caddr form)))
      (let   (bas exp)
	(setq bas (translate (cadr form)) exp (translate (caddr form)))
	(cond ((eq '$fixnum (car exp))
	       (setq exp (cdr exp))
	       (cond ((eq '$float (car bas))
		      `($float expt ,(cdr bas) ,exp))
		     ((and (eq (car bas) '$fixnum)
			   $tr_numer)
		      ;; when NUMER:TRUE we have 1/2 evaluating to 0.5
		      ;; therefore we have a TR_NUMER switch to control
		      ;; this form numerical hackers at translate time
		      ;; where it does the most good. -gjc
		      `($float . (expt (float ,(cdr bas)) ,exp)))
		     ;;It seems to me we can do this,
		     ;; although 2^-3 would result in a "cl rat'l number"
		     ((and $tr_exponent (member (car bas) '($fixnum $number) :test #'eq))
		      `($number expt ,(cdr bas) ,exp))
		     (t `($any power ,(cdr bas) ,exp))))
	      ((and (eq '$float (car bas))
		    (eq '$rational (car exp))
		    (not (atom (caddr exp)))
		    (cond ((equal 2 (caddr (caddr exp)))
			   (setq exp (cadr (caddr exp)))
			   (cond ((= 1 exp) `($float sqrt ,(cdr bas)))
				 ((= -1 exp) `($float / (sqrt ,(cdr bas))))
				 (t `($float expt (sqrt ,(cdr bas)) ,exp))))
			  ((eq 'rat (caar (caddr exp)))
			   `($float expt ,(cdr bas) ,($float (caddr exp)))))))
	      (t `($any power ,(cdr bas) ,(cdr exp)))))))

(def%tr rat (form)
  `($rational . ',form))

(def%tr bigfloat (form)
  `($any . ',form))

(def%tr %sqrt (form)
  (setq form (translate (cadr form)))
  (if (eq '$float (car form)) `($float sqrt ,(cdr form))
      `($any simplify (list '(%sqrt) ,(cdr form)))))

(def%tr mabs (form) 
  (setq form (translate (cadr form)))
  (if (covers '$number (car form)) (list (car form) 'abs (cdr form))
      `($any simplify (list '(mabs) ,(dconvx form)))))

(def%tr %signum (form)
  (destructuring-let (( (mode . arg) (translate (cadr form))))
    (cond ((member mode '($fixnum $float) :test #'eq)
	   (let ((temp (tr-gensym)))
	     `($fixnum . ((lambda (,temp)
			    (declare (,(if (eq mode '$float)	
					   'flonum
					   'fixnum)
				       ,temp))
			    (cond ((minusp ,temp) -1)
				  ((plusp ,temp) 1)
				  (t 0)))
			  ,arg))))
	  (t
	   ;; even in this unknown case we can do a hell
	   ;; of a lot better than consing up a form to
	   ;; call the macsyma simplifier. I mean, shoot
	   ;; have a little SUBR called SIG-NUM or something.
	   `($any simplify (list '(%signum) ,arg))))))

;; The optimization of using -1.0, +1.0 and 0.0 cannot be made unless we
;; know the TARGET MODE. The action of the simplifier is that
;; SIGNUM(3.3) => 1 , SIGNUM(3.3) does not give 0.0
;; Maybe this is a bug in the simplifier, maybe not. -gjc

;; There are many possible non-trivial optimizations possible involving
;; SIGNUM. MODE TARGETTING must be built in to get these easily of course,
;; examples are: SIGNUM(X*Y); No need to multiple X and Y, just multiply
;; there SIGN's, which is a conditional and comparisons. However, these
;; are only optimizations if X and Y are numeric. What if
;; X:'a,Y:'B, ASSUME(A*B>0), SIGNUM(X*Y). Well, here
;; SIGNUM(X)*SIGNUM(Y) won't be the same as SIGNUM(X*Y). -gjc

;; just to show the kind of brain damage...
;;(DEF%TR %SIGNUM (FORM)
;;   (SETQ FORM (TRANSLATE (CADR FORM)))
;;   (COND ((MEMber (CAR FORM) 
;;	  (LET   ((X (CDR FORM)) (MODE (CAR FORM))
;;		    (ONE 1) (MINUS1 -1) (ZERO 0) (VAR '%%N)
;;		    (DECLARE-TYPE 'FIXNUM) COND-CLAUSE)
;;	     (IF (EQ '$FLOAT MODE) (SETQ ONE 1.0 MINUS1 -1.0 ZERO 0.0 VAR '$$X
;;					 DECLARE-TYPE 'FLONUM))
;;	     (SETQ COND-CLAUSE `(COND ((MINUSP ,X) ,MINUS1)
;;				      ((PLUSP ,X)  ,ONE)
;;				      (T ,ZERO)))
;;	     (IF (ATOM (CDR FORM)) `(,MODE . ,COND-CLAUSE)
;;		 (PUSHNEW `(,DECLARE-TYPE ,VAR) DECLARES)
;;		 `(,MODE (LAMBDA (,VAR) ,COND-CLAUSE) ,X))))
;;	 (T `($ANY SIMPLIFY (LIST '(%SIGNUM) ,(CDR FORM))))))


(def%tr $entier (form) 
  (setq form (translate (cadr form)))
  (cond ((eq '$fixnum (car form)) form)
        ((member (car form) '($float $number) :test #'eq)
	 (if (eq 'sqrt (cadr form))
	     `($fixnum $isqrt ,(caddr form))
	     `($fixnum floor ,(cdr form))))
        (t `(,(if (eq (car form) '$rational) '$fixnum '$any)
	     $entier ,(cdr form)))))

(def%tr $float (form)
  (setq form (translate (cadr form)))
  (if (covers '$float (car form))
      (cons '$float (dconv-$float form))
      `($any $float ,(cdr form))))

(def%tr %exp (form)
  (setq form (translate (cadr form)))
  (if (eq '$float (car form))
      `($float exp ,(cdr form))
      `($any simplify (list '(%exp) ,(cdr form)))))

(def%tr $atan2 (form)
  (setq form (cdr form))
  (let ((x (translate (car form))) (y (translate (cadr form))))
    (if (eq '$float (*union-mode (car x) (car y)))
	`($float atan ,(cdr x) ,(cdr y))
	`($any simplify (list '($atan2) ,(cdr x) ,(cdr y))))))

(def%tr %atan (form)
  (setq form (cdr form))
  (let ((x (translate (car form))))
    (if (eq '$float (car x))
	`($float atan ,(cdr x))
	`($any simplify (list '(%atan) ,(cdr x))))))
