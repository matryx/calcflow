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

(macsyma-module residu)

(load-macsyma-macros rzmac)

(declare-top (special $breakup $noprincipal varlist
		      leadcoef var *roots *failures wflag nn*
		      sn* sd* $tellratlist genvar dn* zn))


;; Compute the poles (roots) of the polynomial D and return them.
;; Divide these roots into several parts: Those in REGION, REGION1,
;; and everywhere else.  These are returned in a list.  (In a more
;; modern style, we'd probably return them in 4 different values.)
;;
;; The regions are determined by functions REGION and REGION1, which
;; should return non-NIL if the root is in the given region.
;;
;; The description below applies if *SEMIRAT* is NIL.  If *SEMIRAT* is
;; non-NIL, somewhat different results are returned.  I (rtoy) am not
;; exactly sure what *SEMIRAT* is intended to mean.
;;
;; The first part of the list of the form ((r1 (x - r1)^d1) (r2 (x -
;; r2)^d2) ...) where r1, r2 are the roots, d1, d2 are the
;; multiplicity of each root, and x is the variable.
;;
;; The second part is a list of the repeated roots in REGION.  Each
;; element of the list is of the form (r d) where r is the root and d
;; is the multiplicity.
;;
;; The third part is a list of the simple roots in REGION.
;;
;; Finally, the fourth part is NIL, unless *semirat* is T.
(defun polelist (d region region1)
  (prog (roots $breakup r rr ss r1 s pole wflag cf)
     (setq wflag t)
     (setq leadcoef (polyinx d var 'leadcoef))
     (setq roots (solvecase d))
     (if (eq roots 'failure) (return ()))
     ;; Loop over all the roots.  SOLVECASE returns the roots in the form
     ;; ((x = r1) mult1
     ;;  (x = r2) mult2
     ;;  ...)

   loop1
     (cond ((null roots)
	    (cond ((and *semirat*
			(> (+ (length s) (length r))
			   (+ (length ss) (length rr))))
		   ;; Return CF, repeated roots (*semirat*), simple
		   ;; roots (*semirat*), roots in region 1.
		   (return (list cf rr ss r1)))
		  (t
		   ;; Return CF, repeated roots, simple roots, roots in region 1.
		   (return (list cf r s r1)))))
	   (t
	    ;; Set POLE to the actual root and set D to the
	    ;; multiplicity of the root.
	    (setq pole (caddar roots))
	    (setq d (cadr roots))
	    (cond (leadcoef
		   ;; Is it possible for LEADCOEF to be NIL ever?
		   ;;
		   ;; Push (pole (x - pole)^d) onto the list CF.
		   (setq cf (cons pole
				  (cons
				   (m^ (m+ var (m* -1 pole))
				       d)
				   cf)))))))
     ;; Don't change the order of clauses here.  We want to call REGION and then REGION1.
     (cond ((funcall region pole)
	    ;; The pole is in REGION
	    (cond ((equal d 1)
		   ;; A simple pole, so just push the pole onto the list S.
		   (push pole s))
		  (t
		   ;; A multiple pole, so push (pole d) onto the list R.
		   (push (list pole d) r))))
	   ((funcall region1 pole)
	    ;; The pole is in REGION1
	    (cond ((not $noprincipal)
		   ;; Put the pole onto the R1 list.  (Don't know what
		   ;; $NOPRINCIPAL is.)
		   (push pole r1))
		  (t
		   ;; Return NIL if we get here.
		   (return nil))))
	   (*semirat*
	    ;; (What does *SEMIRAT* mean?)  Anyway if we're here, the
	    ;; pole is not in REGION or REGION1, so push the pole onto
	    ;; SS or RR depending if the pole is repeated or not.
	    (cond ((equal d 1)
		   (push pole ss))
		  (t (push (list pole d) rr)))))
     ;; Pop this root and multiplicity and move on.
     (setq roots (cddr roots))
     (go loop1)))

(defun solvecase (e)
  (cond ((not (among var e)) nil)
	(t (let (*failures *roots)
	     (solve e var 1)
	     (cond (*failures 'failure)
		   ((null *roots) ())
		   (t *roots))))))

;; Compute the sum of the residues of n/d.
(defun res (n d region region1)
  (let ((pl (polelist d region region1))
	dp a b c factors leadcoef)
    (cond
      ((null pl) nil)
      (t
       (setq factors (car pl))
       (setq pl (cdr pl))
       ;; PL now contains the list of the roots in region, roots in
       ;; region1, and everything else.
       (cond ((or (cadr pl)
		  (caddr pl))
	      (setq dp (sdiff d var))))
       (cond ((car pl)
	      ;; Compute the sum of the residues of n/d for the
	      ;; multiple roots in REGION.
	      (setq a (m+l (residue n (cond (leadcoef factors)
					    (t d))
				    (car pl)))))
	     (t (setq a 0)))
       (cond ((cadr pl)
	      ;; Compute the sum of the residues of n/d for the simple
	      ;; roots in REGION1.  Since the roots are simple, we can
	      ;; use RES1 to compute the residues instead of the more
	      ;; complicated $RESIDUE.  (This works around a bug
	      ;; described in bug 1073338.)
	      #+nil
	      (setq b (m+l (mapcar #'(lambda (pole)
				       ($residue (m// n d) var pole))
				   (cadr pl))))
	      (setq b (m+l (res1 n dp (cadr pl)))))
	     (t (setq b 0)))
       (cond ((caddr pl)
	      ;; Compute the sum of the residues of n/d for the roots
	      ;; not in REGION nor REGION1.
	      (setq c (m+l (mapcar #'(lambda (pole)
				       ($residue (m// n d) var pole))
				   (caddr pl)))))
	     (t (setq c ())))
       ;; Return the sum of the residues in the two regions and the
       ;; sum of the residues outside the two regions.
       (list (m+ a b) c)))))

(defun residue (zn factors pl)
  (cond (leadcoef
	 (mapcar #'(lambda (j)
		     (destructuring-let (((factor1 factor2) (remfactor factors (car j) zn)))
		       (resm0 factor1 factor2 (car j) (cadr j))))
		 pl))
	(t (mapcar #'(lambda (j)
		       (resm1 (div* zn factors) (car j)))
		   pl))))

;; Compute the residues of zn/d for the simple poles in the list PL1.
;; The poles must be simple because ZD must be the derivative of
;; denominator.  For simple poles the residue can be computed as
;; limit(n(z)/d(z)*(z-a),z,a).  Since the pole is simple we have the
;; indeterminate form (z-a)/d(z).  Use L'Hospital's rule to get
;; 1/d'(z).  Hence, the residue is easily computed as n(a)/d'(a).
(defun res1 (zn zd pl1)
  (setq zd (div* zn zd))
  (mapcar #'(lambda (j)
	      ;; In case the pole is messy, call $RECTFORM.  This
	      ;; works around some issues with gcd bugs in certain
	      ;; cases.  (See bug 1073338.)
	      ($rectform ($expand (subin ($rectform j) zd))))
	  pl1))

(defun resprog0 (f g n n2)
  (prog (a b c r)
     (setq a (resprog f g))
     (setq b (cadr a) c (ptimes (cddr a) n2) a (caar a))
     (setq a (ptimes n a) b (ptimes n b))
     (setq r (pdivide a g))
     (setq a (cadr r) r (car r))
     (setq b (cons (pplus (ptimes (car r) f) (ptimes (cdr r) b))
		   (cdr r)))
     (return (cons (cons (car a) (ptimes (cdr a) c))
		   (cons (car b) (ptimes (cdr b) c))))))


(defun resm0 (e n pole m)
  (setq e (div* n e))
  (setq e ($diff e var (1- m)))
  (setq e ($rectform ($expand (subin pole e))))
  (div* e (simplify `((mfactorial) ,(1- m)))))

(defun remfactor (l p n)
  (prog (f g)
   loop (cond ((null l)
	       (return (list (m*l (cons leadcoef g)) n)))
	      ((equal p (car l)) (setq f (cadr l)))
	      (t (setq g (cons (cadr l) g))))
   (setq l (cddr l))
   (go loop)))

(defun resprog (p1b p2b)
  (prog (temp coef1r coef2r fac coef1s coef2s zeropolb f1 f2)
     (setq coef2r (setq coef1s 0))
     (setq coef2s (setq coef1r 1))
     b1   (cond ((not (< (pdegree p1b var) (pdegree p2b var))) (go b2)))
     (setq temp p2b)
     (setq p2b p1b)
     (setq p1b temp)
     (setq temp coef2r)
     (setq coef2r coef1r)
     (setq coef1r temp)
     (setq temp coef2s)
     (setq coef2s coef1s)
     (setq coef1s temp)
     b2   (cond ((zerop (pdegree p2b var))
		 (return (cons (cons coef2r p2b) (cons coef2s p2b)))))
     (setq zeropolb (psimp var
			   (list (- (pdegree p1b var) (pdegree p2b var))
				 1)))
     (setq fac (pgcd (caddr p1b) (caddr p2b)))
     (setq f1 (pquotient (caddr p1b) fac))
     (setq f2 (pquotient (caddr p2b) fac))
     (setq p1b (pdifference (ptimes f2 (psimp (car p1b) (cdddr p1b)))
			    (ptimes f1
				    (ptimes zeropolb
					    (psimp (car p2b)
						   (cdddr p2b))))))
     (setq coef1r (pdifference (ptimes f2 coef1r)
			       (ptimes (ptimes f1 coef2r) zeropolb)))
     (setq coef1s (pdifference (ptimes f2 coef1s)
			       (ptimes (ptimes f1 coef2s) zeropolb)))
     (go b1)))

;;;Looks for polynomials. puts polys^(pos-num) in sn* polys^(neg-num) in sd*.
(defun snumden (e)
  (cond ((or (atom e)
	     (mnump e))
	 (setq sn* (cons e sn*)))
	((and (mexptp e)
	      (integerp (caddr e)))
	 (cond ((polyinx (cadr e) var nil)
		(cond ((minusp (caddr e))
		       (setq sd* (cons (cond ((equal (caddr e) -1) (cadr e))
					     (t (m^ (cadr e)
						    (- (caddr e)))))
				       sd*)))
		      (t (setq sn* (cons e sn*)))))))
	((polyinx e var nil)
	 (setq sn* (cons e sn*)))))

(setq sn* nil sd* nil)

(defmfun $residue (e var p)
  (cond (($unknown e)
	 ($nounify '$residue)
	 (list '(%residue) e var p))
	(t
	 (let (sn* sd*)
	   (if (and (mtimesp e) (andmapcar #'snumden (cdr e)))
	       (setq nn* (m*l sn*) dn* (m*l sd*))
	       (numden e)))
	 (resm1 (div* nn* dn*) p))))

(defun resm1 (e pole)
  ;; Call $ratsimp to simplify pole.  Not sure if this is really
  ;; necessary or desired, but it fixes bug 1504505.  It would be
  ;; better to fix $taylor, but that seems much harder.
  (let ((pole ($ratsimp ($rectform pole))))
    ;; Call taylor with silent-taylor-flag t and catch an error.
    (if (setq e (catch 'taylor-catch
                  (let ((silent-taylor-flag t))
                    (declare (special silent-taylor-flag))
                    ;; Things like residue(s/(s^2-a^2),s,a) fails if use -1.
                    ($taylor e var pole 1))))
        (coeff (ratdisrep e) (m^ (m+ (m* -1 pole) var) -1) 1)
        (merror (intl:gettext "residue: taylor failed.")))))
