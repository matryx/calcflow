;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module db)

(load-macsyma-macros mrgmac)

;; This file uses its own special syntax which is set up here.  The function
;; which does it is defined in LIBMAX;MRGMAC.  It sets up <, >, and : for
;; structure manipulation.  A major bug with this package is that the code is
;; almost completely uncommented.  Someone with nothing better to do should go
;; through it, figure out how it works, and write it down.

;; External specials

(defvar context 'global)
(defvar contexts nil)
(defvar current 'global)
(defvar dbtrace nil)
(defvar dobjects nil)

;; Internal specials

(defvar *nobjects* nil)
(defvar *dbcheck* nil)
(defvar +l)
(defvar -l)

(defvar *conindex* 0)
(defvar *connumber* 50)

(defconstant +lab-high-bit+ most-negative-fixnum)

;; One less than the number of bits in a fixnum.
(defconstant +labnumber+ (1- (integer-length +lab-high-bit+)))

;; A cell with the high bit turned on.
(defvar *lab-high-lab* (list +lab-high-bit+))

;; Variables that are set by (clear)
(defvar +s)
(defvar +sm)
(defvar +sl)
(defvar -s)
(defvar -sm)
(defvar -sl)
(defvar *labs*)
(defvar *lprs*)
(defvar *labindex*)
(defvar *lprindex*)
(defvar *marks* 0)
(defvar +labs nil)
(defvar -labs nil)
(defvar ulabs nil)


(defvar *world*)
(defvar *db*)

;; Macro for indirecting through the contents of a cell.

(defmacro unlab (cell)
  `(car ,cell))

(defmacro copyn (n)
  `(list ,n))

(defmacro iorm (cell n)
  `(rplaca ,cell (logior (car ,cell) (car ,n))))

(defmacro xorm (cell n)
  `(rplaca ,cell (logxor (car ,cell) (car ,n))))

(defprop global 1 cmark)

(defvar conunmrk (make-array (1+ *connumber*) :initial-element nil))
(defvar conmark  (make-array (1+ *connumber*) :initial-element nil))

(defmfun mark (x)
  (putprop x t 'mark))

(defmfun markp (x)
  (and (symbolp x) (zl-get x 'mark)))

(defun zl-remprop (sym indicator)
  (if (symbolp sym)
      (remprop sym indicator)
      (unless (atom sym)
        (remf (cdr sym) indicator))))

(defmfun unmrk (x)
  (zl-remprop x 'mark))

(defun marks (x)
  (cond ((numberp x))
	((atom x) (mark x))
	(t (mapc #'marks x))))

(defun unmrks (x)
  (cond ((numberp x))
	((or (atom x) (numberp (car x))) (unmrk x))
	(t (mapc #'unmrks x))))

(defmode type ()
  (atom (selector +labs) (selector -labs) (selector data))
  selector)

(defmode indv ()
  (atom (selector =labs) (selector nlabs) (selector data) (selector in))
  selector)

(defmode univ ()
  (atom (selector =labs) (selector nlabs) (selector data) (selector un))
  selector)

(defmode datum ()
  (atom (selector ulabs) (selector con) (selector wn))
  selector)

(defmode context ()
  (atom (selector cmark fixnum 0) (selector subc) (selector data)))

(defmacro +labz (x)
  `(cond ((+labs ,x))
    (t '(0))))

(defmacro -labz (x)
  `(cond ((-labs ,x))
    (t '(0))))

(defmacro =labz (x)
  `(cond ((=labs ,x))
    (t '(0))))

(defmacro nlabz (x)
  `(cond ((nlabs ,x))
    (t '(0))))

(defmacro ulabz (x)
  `(cond ((ulabs ,x))
    (t '(0))))

(defmacro subp (&rest x)
  (setq x (mapcar #'(lambda (form) `(unlab ,form)) x))
  `(= ,(car x) (logand ,@x)))

(defun dbnode (x)
  (if (symbolp x) x (list x)))

(defun nodep (x)
  (or (atom x) (mnump (car x))))

(defun dbvarp (x)
  (getl x '(un ex)))

(defun lab (n)
  (ash 1 (1- n)))

(defun lpr (m n)
  (cond ((do ((l *lprs* (cdr l)))
	     ((null l))
	   (if (and (labeq m (caaar l)) (labeq n (cdaar l)))
	       (return (cdar l)))))
	((= (decf *lprindex*) *labindex*)
	 (break))
	(t
	 (push (cons (cons m n) (ash 1 *lprindex*)) *lprs*)
	 (cdar *lprs*))))

(defun labeq (x y)
  (= (logior x +lab-high-bit+) (logior y +lab-high-bit+)))

(defun marknd (nd)
  (cond ((+labs nd))
	((= *lprindex* (incf *labindex*))
	 (break))
	(t (push (cons nd (lab *labindex*)) *labs*)
	   (beg nd (lab *labindex*))
	   (cdar *labs*))))

(defun dbv (x r)
  (do ((l *lprs* (cdr l))
       (y 0))
      ((null l) y)
    (unless (or (zerop (logand r (cdar l))) (zerop (logand x (caaar l))))
      (setq y (logior (cdaar l) y)))))

(defun dba (r y)
  (do ((l *lprs* (cdr l))
       (x 0))
      ((null l) x)
    (unless (or (zerop (logand r (cdar l))) (zerop (logand (cdaar l) y)))
      (setq x (logior x (caaar l))))))

(defun prlab (x)
  (setq x (unlab x))
  (when x
    (format t " ~,,' ,3:B" (logandc1 +lab-high-bit+ x))))

(defun onp (cl lab)
  (subp lab (+labz cl)))

(defun offp (cl lab)
  (subp lab (-labz cl)))

(defun onpu (lab fact)
  (subp lab (ulabz fact)))

(defmfun visiblep (dat)
  (and (not (ulabs dat)) (cntp dat)))

(defun cancel (lab dat)
  (cond ((setq *db* (ulabs dat))
	 (iorm *db* lab))
	(t
	 (push dat ulabs)
	 (setq lab (unlab lab))
	 (putprop dat (copyn lab) 'ulabs))))

(defun queue+p (nd lab)
  (cond ((null (setq *db* (+labs nd)))
	 (push nd +labs)
	 (setq lab (unlab lab))
	 (putprop nd (copyn (logior +lab-high-bit+ lab)) '+labs))
	((subp lab *db*)
	 nil)
	((subp *lab-high-lab* *db*)
	 (iorm *db* lab)
	 nil)
	(t
	 (iorm *db* (copyn (logior +lab-high-bit+ (unlab lab)))))))

(defun beg (nd lab)
  (setq lab (copyn lab))
  (if (queue+p nd lab)
      (if (null +s)
	  (setq +s (ncons nd)
		+sm +s
		+sl +s)
	  (push nd +s))))

(defun queue-p (nd lab)
  (cond ((null (setq *db* (-labs nd)))
	 (push nd -labs)
	 (setq lab (unlab lab))
	 (putprop nd (copyn (logior +lab-high-bit+ lab)) '-labs))
	((subp lab *db*)
	 nil)
	((subp *lab-high-lab* *db*)
	 (iorm *db* lab)
	 nil)
	(t
	 (iorm *db* (copyn (logior +lab-high-bit+ (unlab lab)))))))

(defun beg- (nd lab)
  (setq lab (copyn lab))
  (if (queue-p nd lab)
      (if (null -s)
	  (setq -s (ncons nd)
		-sm -s
		-sl -s)
	  (setq -s (cons nd -s)))))

(defun mid (nd lab)
  (if (queue+p nd lab)
      (cond ((null +sm)
	     (setq +s (ncons nd)
		   +sm +s
		   +sl +s))
	    (t
	     (rplacd +sm (cons nd (cdr +sm)))
	     (if (eq +sm +sl)
		 (setq +sl (cdr +sl)))
	     (setq +sm (cdr +sm))))))

(defun mid- (nd lab)
  (if (queue-p nd lab)
      (cond ((null -sm)
	     (setq -s (ncons nd)
		   -sm -s
		   -sl -s))
	    (t
	     (rplacd -sm (cons nd (cdr -sm)))
	     (when (eq -sm -sl)
	       (setq -sl (cdr -sl)))
	     (setq -sm (cdr -sm))))))

(defun end (nd lab)
  (if (queue+p nd lab)
      (cond ((null +sl)
	     (setq +s (ncons nd)
		   +sm +s
		   +sl +s))
	    (t
	     (rplacd +sl (ncons nd))
	     (setq +sl (cdr +sl))))))

(defun end- (nd lab)
  (if (queue-p nd lab)
      (cond ((null -sl)
	     (setq -s (ncons nd)
		   -sm -s
		   -sl -s))
	    (t
	     (rplacd -sl (ncons nd))
	     (setq -sl (cdr -sl))))))

(defun dq+ ()
  (if +s
      (prog2
	  (xorm (zl-get (car +s) '+labs) *lab-high-lab*)
	  (car +s)
	(cond ((not (eq +s +sm))
	       (setq +s (cdr +s)))
	      ((not (eq +s +sl))
	       (setq +s (cdr +s)
		     +sm +s))
	      (t
	       (setq +s nil
		     +sm nil
		     +sl nil))))))

(defun dq- ()
  (if -s
      (prog2
	  (xorm (-labs (car -s)) *lab-high-lab*)
	  (car -s)
	(cond ((not (eq -s -sm))
	       (setq -s (cdr -s)))
	      ((not (eq -s -sl))
	       (setq -s (cdr -s)
		     -sm -s))
	      (t
	       (setq -s nil
		     -sm nil
		     -sl nil))))))

(defmfun clear ()
  (when dbtrace
    (format *trace-output* "~%CLEAR: clearing ~A" *marks*))
  (mapc #'(lambda (sym) (push+sto (sel sym +labs) nil)) +labs)
  (mapc #'(lambda (sym) (push+sto (sel sym -labs) nil)) -labs)
  (mapc #'(lambda (sym) (zl-remprop sym 'ulabs)) ulabs)
  (setq +s nil
	+sm nil
	+sl nil
	-s nil
	-sm nil
	-sl nil
	*labs* nil
	*lprs* nil
	*labindex* 0
	*lprindex* +labnumber+
	*marks* 0
	+labs nil
	-labs nil
	ulabs nil)
  (contextmark))

(defmfun truep (pat)
  (clear)
  (cond ((atom pat) pat)
	((prog2 (setq pat (mapcar #'semant pat)) nil))
	((eq (car pat) 'kind)
	 (beg (cadr pat) 1)
	 (beg- (caddr pat) 1)
	 (propg))
	(t
	 (beg (cadr pat) 1)
	 (beg- (caddr pat) 2)
	 (beg (car pat) (lpr 1 2))
	 (propg))))

(defmfun falsep (pat)
  (clear)
  (cond ((eq (car pat) 'kind)
	 (beg (cadr pat) 1)
	 (beg (caddr pat) 1)
	 (propg))))

(defmfun isp (pat)
  (let ((isp 'unknown))
    (ignore-errors
      (setq isp
	    (cond ((truep pat))
		  ((falsep pat) nil)
		  (t 'unknown))))
    isp))

(defmfun kindp (x y)
  (unless (symbolp x)
    (merror (intl:gettext "declare: argument must be a symbol; found ~M") x))
  (clear)
  (beg x 1)
  (do ((p (dq+) (dq+)))
      ((null p))
    (if (eq y p)
	(return t)
	(mark+ p (+labs p)))))

(defmfun true* (pat)
  (let ((dum (semant pat)))
    (if dum
	(cntxt (ind (ncons dum)) context))))

(defmfun fact (fun arg val)
  (cntxt (ind (datum (list fun arg val))) context))

(defmfun kind (x y &aux #+kcl (y y))
  (setq y (datum (list 'kind x y)))
  (cntxt y context)
  (addf y x))

(defmfun par (s y)
  (setq y (datum (list 'par s y)))
  (cntxt y context)
  (mapc #'(lambda (lis) (addf y lis)) s))

(defmfun datum (pat)
  (ncons pat))

(defun ind (dat)
  (mapc #'(lambda (lis) (ind1 dat lis)) (cdar dat))
  (mapc #'ind2 (cdar dat))
  dat)

(defun ind1 (dat pat)
  (cond ((not (nodep pat))
	 (mapc #'(lambda (lis) (ind1 dat lis)) pat))
	((or (markp pat) (eq 'unknown pat)))
	(t
	 (addf dat pat) (mark pat))))

(defun ind2 (nd)
  (if (nodep nd)
      (unmrk nd)
      (mapc #'ind2 nd)))

(defmfun addf (dat nd)
  (push+sto (sel nd data) (cons dat (sel nd data))))

(defmfun maxima-remf (dat nd)
  (push+sto (sel nd data) (fdel dat (sel nd data))))

(defun fdel (fact data)
  (cond ((and (eq (car fact) (caaar data))
	      (eq (cadr fact) (cadaar data))
	      (eq (caddr fact) (caddar (car data))))
	 (cdr data))
	(t
	 (do ((ds data (cdr ds))
	      (d))
	     ((null (cdr ds)))
	   (setq d (caadr ds))
	   (cond ((and (eq (car fact) (car d))
		       (eq (cadr fact) (cadr d))
		       (eq (caddr fact) (caddr d)))
		  (push+sto (sel d con data) (delete d (sel d con data) :test #'eq))
		  (rplacd ds (cddr ds)) (return t))))
	 data)))

(defun semantics (pat)
  (if (atom pat)
      pat
      (list (semant pat))))

(defun db-mnump (x)
  (or (numberp x)
      (and (not (atom x))
	   (not (atom (car x)))
	   (member (caar x) '(rat bigfloat) :test #'eq))))

(defun semant (pat)
  (cond ((symbolp pat) (or (zl-get pat 'var) pat))
	((db-mnump pat) (dintnum pat))
	(t (mapcar #'semant pat))))

(defmfun dinternp (x)
  (cond ((mnump x) (dintnum x))
	((atom x) x)
	((assol x dobjects))))

(defmfun dintern (x)
  (cond ((mnump x) (dintnum x))
	((atom x) x)
	((assol x dobjects))
	(t (setq dobjects (cons (dbnode x) dobjects))
	   (car dobjects))))

(defun dintnum (x &aux foo)
  (cond ((assol x *nobjects*))
	((progn (setq x (dbnode x)) nil))
	((null *nobjects*)
	 (setq *nobjects* (list x))
	 x)
	((eq '$zero (setq foo (rgrp (car x) (caar *nobjects*))))
	 (let ((context 'global))
	   (fact 'meqp x (car *nobjects*)))
	 (push x *nobjects*)
	 x)
	((eq '$pos foo)
	 (let ((context 'global))
	   (fact 'mgrp x (car *nobjects*)))
	 (push x *nobjects*)
	 x)
	(t
	 (do ((lis *nobjects* (cdr lis))
	      (context '$global))
	     ((null (cdr lis))
	      (let ((context 'global))
		(fact 'mgrp (car lis) x))
	      (rplacd lis (list x)) x)
	   (cond ((eq '$zero (setq foo (rgrp (car x) (caadr lis))))
              (let ((context 'global))
                (fact 'meqp (cadr lis) x))
              (rplacd lis (cons x (cdr lis)))
              (return x))
         ((eq '$pos foo)
		  (let ((context 'global))
		    (fact 'mgrp (car lis) x)
		    (fact 'mgrp x (cadr lis)))
		  (rplacd lis (cons x (cdr lis)))
		  (return x)))))))

(defmfun doutern (x)
  (if (atom x) x (car x)))

(defmfun untrue (pat)
  (kill (car pat) (semant (cadr pat)) (semant (caddr pat))))

(defmfun kill (fun arg val)
  (kill2 fun arg val arg)
  (kill2 fun arg val val))

(defun kill2 (fun arg val cl)
  (cond ((not (atom cl)) (mapc #'(lambda (lis) (kill2 fun arg val lis)) cl))
	((numberp cl))
	(t (push+sto (sel cl data) (kill3 fun arg val (sel cl data))))))

(defun kill3 (fun arg val data)
  (cond ((and (eq fun (caaar data))
	      (eq arg (cadaar data))
	      (eq val (caddar (car data))))
	 (cdr data))
	(t
	 (do ((ds data (cdr ds))
	      (d))
	     ((null (cdr ds)))
	 (setq d (caadr ds))
	 (cond ((not (and (eq fun (car d))
			  (eq arg (cadr d))
			  (eq val (caddr d))))
		t)
	       (t (push+sto (sel d con data) (delete d (sel d con data) :test #'eq))
		  (rplacd ds (cddr ds)) (return t))))
	 data)))

(defmfun unkind (x y)
  (setq y (car (datum (list 'kind x y))))
  (kcntxt y context)
  (maxima-remf y x))

(defmfun remov (fact)
  (remov4 fact (cadar fact))
  (remov4 fact (caddar fact)))

(defun remov4 (fact cl)
  (cond ((or (symbolp cl)		;if CL is a symbol or
	     (and (consp cl)            ;an interned number, then we want to REMOV4 FACT
		  (mnump (car cl))))	;from its property list.
	 (push+sto (sel cl data) (delete fact (sel cl data) :test #'eq)))
	((or (atom cl) (atom (car cl)))) ;if CL is an atom (not a symbol)
					;or its CAR is an atom then we don't want to do
					;anything to it.
	(t
	 (mapc #'(lambda (lis) (remov4 fact lis))
	       (cond ((atom (caar cl)) (cdr cl)) ;if CL's CAAR is
					;an atom, then CL is an expression, and
					;we want to REMOV4 FACT from the parts
					;of the expression.
		     ((atom (caaar cl)) (cdar cl)))))))
					;if CL's CAAAR is an atom, then CL is a
					;fact, and we want to REMOV4 FACT from
					;the parts of the fact.

(defmfun killframe (cl)
  (mapc #'remov (sel cl data))
  (zl-remprop cl '+labs)
  (zl-remprop cl '-labs)
  (zl-remprop cl 'obj)
  (zl-remprop cl 'var)
  (zl-remprop cl 'fact)
  (zl-remprop cl 'wn))

(defmfun activate (&rest l)
  (dolist (e l)
    (cond ((member e contexts :test #'eq) nil)
	  (t (push e contexts)
	     (cmark e)))))

(defmfun deactivate (&rest l)
  (dolist (e l)
    (cond ((not (member e contexts :test #'eq))
	   nil)
	  (t
	   (cunmrk e)
	   (setq contexts (delete e contexts :test #'eq))))))

(defun gccon ()
  (gccon1)
  (when (> *conindex* *connumber*)
    #+gc (gc)
    (gccon1)
    (when (> *conindex* *connumber*)
      (merror (intl:gettext "context: too many contexts.")))))

(defun gccon1 ()
  (setq *conindex* 0)
  (do ((i 0 (1+ i)))
      ((> i *connumber*))
    (cond ((not (eq (aref conmark i) (cdr (aref conunmrk i))))
	   (killc (aref conmark i)))
	  (t
	   (setf (aref conunmrk *conindex*) (aref conunmrk i))
	   (setf (aref conmark *conindex*) (aref conmark i))
	   (incf *conindex*)))))

(defmfun cntxt (dat con)
  (unless (atom con)
    (setq con (cdr con)))
  (putprop con (cons dat (zl-get con 'data)) 'data)
  (unless (eq 'global con)
    (putprop dat con 'con))
  dat)

(defmfun kcntxt (fact con)
  (unless (atom con)
    (setq con (cdr con)))
  (putprop con (fdel fact (zl-get con 'data)) 'data)
  (unless (eq 'global con)
    (zl-remprop fact 'con))
  fact)

(defun cntp (f)
  (cond ((not (setq f (sel f con))))
	((setq f (zl-get f 'cmark))
	 (> f 0))))

(defmfun contextmark ()
  (let ((con context))
    (unless (eq current con)
      (cunmrk current)
      (setq current con)
      (cmark con))))

(defun cmark (con)
  (unless (atom con)
    (setq con (cdr con)))
  (let ((cm (zl-get con 'cmark)))
    (putprop con (if cm (1+ cm) 1) 'cmark)
    (mapc #'cmark (zl-get con 'subc))))

(defun cunmrk (con)
  (if (not (atom con))
      (setq con (cdr con)))
  (let ((cm (zl-get con 'cmark)))
    (cond (cm (putprop con (1- cm) 'cmark)))
    (mapc #'cunmrk (zl-get con 'subc))))

(defmfun killc (con)
  (contextmark)
  (unless (null con)
    (mapc #'remov (zl-get con 'data))
    (zl-remprop con 'data)
    (zl-remprop con 'cmark)
    (zl-remprop con 'subc))
  t)

(defun propg ()
  (do ((x)
       (lab))
      (nil)
    (cond ((setq x (dq+))
	   (setq lab (+labs x))
	   (if (zerop (logand (unlab lab) (unlab (-labz x))))
	       (mark+ x lab)
	       (return t)))
	  ((setq x (dq-))
	   (setq lab (-labs x))
	   (if (zerop (logand (unlab lab) (unlab (+labz x))))
	       (mark- x lab)
	       (return t)))
	  (t (return nil)))))

(defun mark+ (cl lab)
  (when dbtrace
    (incf *marks*)
    (format *trace-output* "~%MARK+: marking ~A +" cl)
    (prlab lab))
  (mapc #'(lambda (lis) (mark+0 cl lab lis)) (sel cl data)))

(defun mark+3 (dat)
  (if (/= 0 (logand (unlab (+labz (caddar dat)))
		    (unlab (dbv (+labz (cadar dat)) (-labz (caar dat))))))
      (beg- (sel dat wn) *world*)))

(defun mark+0 (cl lab fact)
  (when *dbcheck*
    (format *trace-output* "~%MARK+0: checking ~a from ~A+" (car fact) cl)
    (prlab lab))
  (cond ((onpu lab fact))
	((not (cntp fact)))
	((null (sel fact wn)) (mark+1 cl lab fact))
	((onp (sel fact wn) *world*) (mark+1 cl lab fact))
	((offp (sel fact wn) *world*) nil)
	(t (mark+3 fact))))

(defun mark+1 (cl lab dat)
  (cond ((eq (caar dat) 'kind)
	 (if (eq (cadar dat) cl) (mid (caddar dat) lab))) ; E1
	((eq (caar dat) 'par)
	 (if (not (eq (caddar dat) cl))
	     (progn
	       (cancel lab dat)		; PR1
	       (mid (caddar dat) lab)
	       (do ((lis (cadar dat) (cdr lis)))
		   ((null lis))
		 (if (not (eq (car lis) cl))
		     (mid- (car lis) lab))))))
	((eq (cadar dat) cl)
	 (if (+labs (caar dat))		; V1
	     (end (caddar dat) (dbv lab (+labs (caar dat)))))
	 (if (-labs (caddar dat))	; F4
	     (end- (caar dat) (lpr lab (-labs (caddar dat))))))))

(defun mark- (cl lab)
  (when dbtrace
    (incf *marks*)
    (format *trace-output* "~%MARK-: marking ~A -" cl)
    (prlab lab))
  (mapc #'(lambda (lis) (mark-0 cl lab lis)) (sel cl data)))

(defun mark-0 (cl lab fact)
  (when *dbcheck*
    (format *trace-output* "~%MARK-0: checking ~A from ~A-" (car fact) cl)
    (prlab lab))
  (cond ((onpu lab fact))
	((not (cntp fact)))
	((null (sel fact wn)) (mark-1 cl lab fact))
	((onp (sel fact wn) *world*) (mark-1 cl lab fact))
	((offp (sel fact wn) *world*) nil)))

(defun mark-1 (cl lab dat)
  (cond ((eq (caar dat) 'kind)
	 (if (not (eq (cadar dat) cl)) (mid- (cadar dat) lab)))	; E4
	((eq (caar dat) 'par)
	 (if (eq (caddar dat) cl)
	     (prog2
		 (cancel lab dat)	; S4
		 (do ((lis (cadar dat) (cdr lis)))
		     ((null lis))
		   (mid- (car lis) lab)))
	     (progn
	       (setq lab (unlab lab))	; ALL4
	       (do ((lis (cadar dat) (cdr lis)))
		   ((null lis))
		 (setq lab (logand (unlab (-labz (car lis))) lab)))
	       (setq lab (copyn lab))
	       (cancel lab dat)
	       (mid- (caddar dat) lab))))
	((eq (caddar dat) cl)
	 (if (+labs (caar dat))		; A2
	     (end- (cadar dat) (dba (+labs (caar dat)) lab)))
	 (if (+labs (cadar dat))	; F6
	     (end- (caar dat) (lpr (+labs (cadar dat)) lab))))))

;;	     in out                    in out                  ins  in out
;;	-----------		-------------             ----------------
;;	E1 |     +		INV1 |     +              AB1 |(+)  +   +
;;	E2 |     -		INV2 |     -              AB2 |(+)  -   +
;;	E3 | +			INV3 | +                  AB3 |(+)  +   -
;;	E4 | -			INV4 | -                  AB4 |(+)  -   -
;;                                                         AB5 |(-)  +   +
;;            in out                    in out             AB6 |(-)  -   +
;;       -----------             -------------             AB7 |(-)  +   -
;;       S1 |    (+)             ALL1 |(+)  +              AB8 |(-)  -   -
;;       S2 |    (-)             ALL2 |(+)  -
;;       S3 |(+)                 ALL3 |(-)  +
;;       S4 |(-)                 ALL4 |(-)  -



;;	     in rel out	         in rel out	     in rel out
;;	---------------	    ---------------	---------------
;;	V1 |    (+)  +	    A1 | +  (+)		F1 |     +  (+)
;;	V2 |    (+)  -	    A2 | -  (+)		F2 |     +  (-)
;;	V3 |    (-)  +	    A3 | +  (-)		F3 |     -  (+)
;;	V4 |    (-)  -	    A4 | -  (-)		F4 |     -  (-)
;;						F5 |(+)  +
;;						F6 |(+)  -
;;						F7 |(-)  +
;;						F8 |(-)  -

(defun uni (p1 p2 al)
  (cond ((dbvarp p1) (dbunivar p1 p2 al))
	((nodep p1)
	 (cond ((dbvarp p2) (dbunivar p2 p1 al))
	       ((nodep p2) (if (eq p1 p2) al))))
	((dbvarp p2) (dbunivar p2 p1 al))
	((nodep p2) nil)
	((setq al (uni (car p1) (car p2) al))
	 (uni (cdr p1) (cdr p2) al))))

(defun dbunivar (p v al)
  (let ((dum (assoc p al :test #'eq)))
    (if (null dum)
	(cons (cons p v) al)
	(uni (cdr dum) v al))))
