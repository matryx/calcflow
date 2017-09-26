;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   *****************************************************************
;;;   ***** SPGCD ******* Sparse polynomial routines for GCD **********
;;;   *****************************************************************
;;;   ** (C) COPYRIGHT 1982 MASSACHUSETTS INSTITUTE OF TECHNOLOGY *****
;;;   ****** THIS IS A READ-ONLY FILE! (ALL WRITES RESERVED) **********
;;;   *****************************************************************

(in-package :maxima)

(macsyma-module spgcd)

(declare-top (special modulus temp genvar *alpha *which-factor*
		      $algebraic algfac* $gcd))

(load-macsyma-macros ratmac)

(defmvar $pointbound *alpha)

(defmacro len (lobj) `(cadr ,lobj))

(defmacro skel (lobj) `(caddr ,lobj))

(defmacro matrix (lobj) `(cadddr ,lobj))

(defmacro currow (lobj) `(cadr (cdddr ,lobj)))

(defmacro badrows (lobj) `(cadr (cddddr ,lobj)))

(defun pinterp (x pts vals)
  (do ((u (car vals)
	  (pplus u (ptimes
		    (pctimes (crecip (pcsubstz (car xk) qk))
			     qk)
		    (pdifference (car uk)
				 (pcsubstz (car xk) u)))))
       (uk (cdr vals) (cdr uk))
       (qk (list x 1 1 0 (cminus (car pts)))
	   (ptimes qk (list x 1 1 0 (cminus (car xk)))))
       (xk (cdr pts) (cdr xk)))
      ((null xk) u)))

(defun pcsubstz (val p)
  (if (pcoefp p) p
      (do ((l (p-terms p) (pt-red l))
	   (ans 0
		(ctimes
		 (cplus ans (pt-lc l))
		 (cexpt val (- (pt-le l) (pt-le (pt-red l)))))))
	  ((null (pt-red l))
	   (ctimes
	    (cplus ans (pt-lc l))
	    (if (zerop (pt-le l))
		1
		(cexpt val (pt-le l))))))))

;;skeletons consist of list of exponent vectors
;;lifting structures are as follows:

;;(matrix <n>            ;length of <skel>
;;        <skel>		;skeleton of poly being lifted
;;        <matrix>       ; partially diagonalized matrix used to obtain sol.
;;        <row>          ;we have diagonalized this far
;;        <bad rows>)    ; after we get 

(defun eval-mon (mon pt)
  (do ((l mon (cdr l))
       (ll pt (cdr ll))
       (ans 1 (ctimes
	       (if (zerop (car l)) 1
		   (cexpt (car ll) (car l)))
	       ans)))
      ((null l) ans)))

;; ONE-STEP causes the  (row,col) element of mat to be made to be zero.  It is
;; assumed that row > col, and that the matrix is diagonal above the row.  
;; n indicates how wide the rows are suppoded to be.

(defun one-step (mat row col n)
  (do ((i col (1+ i))
       (c (aref mat row col)))	  ;Got to save this away before it is zeroed
      ((> i n) mat)
    (setf (aref mat row i)
	   (cdifference (aref mat row i)
			(ctimes c (aref mat col i))))))

;; MONICIZE-ROW assumes the (row,row) element is non-zero and that this element
;; is to be made equal to one.  It merely divides the entire row by the 
;; (row,row) element.  It is assumed that the (row,n) elements with n < row are
;; already zero.  Again n is the width of the rows.

(defun monicize-row (mat row n)
  (do ((i n (1- i))
       (c (crecip (aref mat row row))))
      ((= i row)
       (setf (aref mat row row) 1)) ;Don't bother doing the division on the diagonal
    (setf (aref mat row i) (ctimes (aref mat row i) c))))

;; FILL-ROW is given a point and the value of the skeleton at the point and
;; fills the apropriate row in the matrix. with the values of the monomials
;; n is the length of the skeleton

(defun fill-row (skel mat n row pt val)
  (do ((i 0 (1+ i))
       (l skel (cdr l)))
      ((= i n)				;l should be nil now,
       (if (not (null l))	     ;but lets check just to make sure
	   (merror "FILL-ROW: skeleton too long: ~S"
		   (list n '= skel)))
       (setf (aref mat row n) val)) ;The value is stored in the last column
    (setf (aref mat row i)
	   (eval-mon (car l) pt))))	;Should think about a better
					;way to do this evaluation.

#-allegro
(defun swap-rows (mat m n)		;Interchange row m and n
  (do ((k 0 (1+ k))
       (l (array-dimension mat 0)))
      ((> k l) mat)
    (setf (aref mat m k)
	   (prog1
	       (aref mat n k)
	     (setf (aref mat n k) (aref mat m k))))))

;; PARTIAL-DIAG fills in one more row of the matrix and tries to diagonalize
;; what it has so far.  If the row which has just been introduced has a 
;; zero element on the diagonal then it is scanned for its first non-zero
;; element and it is placed in the matrix so that the non-zero element is 
;; on the diagonal.  The rows which are missing are added to the list in 
;; badrows.  The current row pointer may skip a couple of numbers here, so
;; when it is equal to n, the only empty rows to add thing to are on the
;; bad rows list.

(defun partial-diag (lobj pt val) ; Does one step of diagonalization of
  (let ((n (len lobj))			;the matrix in lobj
	(skel (skel lobj))		
	(mat (matrix lobj))		;The matrix, obviously
	(row (currow lobj))	      ;This is the row which is to be 
					;introduced
	(badrows (badrows lobj))    ;Rows which could not be used when
					;their time came, and will be used later
	crow)
    (cond ((= row n)		    ;All rows already done, must start
					;using the rows in badrows.
	   (fill-row skel mat n (setq crow (car badrows)) pt val))
	  ((fill-row skel mat n row pt val) ;Fill up the data
	   (setq crow row)))

    ;; This loop kills all the elements in the row up to the diagonal.

    (do ((i 0 (1+ i))			;runs over the columns of mat
	 (l (setq badrows (cons nil badrows)))
	 (flag))
	((= i crow)
	 (setq badrows (cdr badrows)))	;update badrows
      (if (cdr l)			;Any bad rows around?
	  (if (= (cadr l) i)		;No one for this row,
	      (if (and (null flag)	;So if this guy can fill in
		       (not (zerop (aref mat crow i))))
		  (progn
		    (swap-rows mat crow i) ;Put this guy in the right spot
		    (rplacd l (cddr l))
		    (setq flag t crow i)) ; and make him a winner
		  (setq l (cdr l)))	;At any rate this guy isn't 
					;used any more.
	      (one-step mat crow i n))
	  (one-step mat crow i n)))

    (if (zerop (aref mat crow crow))	;diagonal is zero?
	(setq badrows (cons crow badrows))
	(progn
	  (monicize-row mat crow n) ;Monicize the diagonal element on this
					;row
	  (do ((j 0 (1+ j)))	  ;For each element in the rows above 
					;this one zero the the crow column
	      ((= j crow))	      ;Don't zero the diagonal element
	    (one-step mat j crow n))))
    (cond ((and (= (1+ row) n)
		(progn (setq row (1- row)) t) ;Decrement it just in case
		(null (cdr badrows)))
	   (do ((l nil (cons (aref mat i n) l))
		(i (1- n) (1- i)))
	       ((< i 0)
		(list 'solution n skel mat l))))
	  (t (list 'matrix n skel mat (1+ row) badrows)))))

(defun gen-point (vars)
  (do ((l vars (cdr l))
       (ans nil (cons (cmod (random $pointbound)) ans)))
      ((null l) ans)))

;; PDIAG-ALL introduces a new row in each matrix in the list of l-lobjs.
;; The RHS of the equations is stripped off of poly.

(defun pdiag-all (l-lobjs poly pt)
  (do ((l l-lobjs (cdr l))
       (lp (p-terms poly))
       (solved t) (c))
      ((null l)
       (if solved (cons 'solved l-lobjs)
	   l-lobjs))
    (if (and lp (= (caar l) (pt-le lp))) ;This term corresponds to the
					;current lobj, set c to the coefficient
	(setq c (pt-lc lp) lp (pt-red lp))
	(setq c 0))
    ;;FIXTHIS				;Should put it some check for extra 
					;terms in the polynomial
    (if (not (eq (cadar l) 'solution))
	(progn (rplacd (car l)
		       (partial-diag (cdar l) pt c))
	       (and solved (null (eq (cadar l) 'solution)) 
		    (setq solved nil))))))

;; not currently called
;; (defun CREATE-INTVECT (h)
;;      (do ((l (cdr h) (cddr l))
;; 	    (ans nil (cons (list (car l) (cadr l))
;; 			   ans)))
;; 	   ((null l)
;; 	    (nreverse ans))))

;; (defun MERGE-INTVECT (iv h)
;;      (do ((l iv (cdr l))
;; 	    (h (cdr h)))
;; 	   ((null l) iv)
;; 	   (cond ((or (null h) (> (caar l) (car h)))
;; 		  (rplacd (car l) (cons 0 (cdar l))))
;; 		 ((= (caar l) (car h))
;; 		  (rplacd (car l) (cons (cadr h) (cdar l)))
;; 		  (setq h (cddr h)))
;; 		 (t (error '|Bad Evaluation point - MERGE-INTVECT|)))))


(defun merge-skel (mon poly)
  (cond ((pcoefp poly)
	 (list (cons 0 mon)))
	((do ((l (cdr poly) (cddr l))
	      (ans nil
		   (cons (cons (car l) mon) ans)))
	     ((null l) ans)))))

(defun new-skel (skel polys)
  (list
   (mapcan #'(lambda (mon poly) (merge-skel mon poly)) skel polys)
   (mapcan #'(lambda (q)
	       (cond ((pcoefp q) (list q))
		     ((do ((l (cdr q) (cddr l))
			   (ans nil (cons (cadr l) ans)))
			  ((null l) ans)))))
	   polys)))

(defun create-lobjs (prev-lift)
  (mapcar #'(lambda (q)
	      (let ((n (length (cadr q))))
		(cons (car q)
		      (list 'matrix n (cadr q)
			    (make-array (list n (1+ n)) :initial-element 0 :element-type 'fixnum)
			    0 nil))))
	  prev-lift))

(defun clear-lobjs (lobjs)
  (mapcar #'(lambda (q)
	      (cons (car q)
		    (list 'matrix (caddr q) (cadddr q)
			  (caddr (cddr q)) 0 nil)))
	  lobjs))

(defun sparse-lift (c f g l-lobjs vars)
  (do ((pt (gen-point vars) (gen-point vars))
       (gcd))
      ((eq (car l-lobjs) 'solved)
       (cdr l-lobjs))
    (setq gcd (lifting-factors-image
	       (pcsub c pt vars) (pcsub f pt vars) (pcsub g pt vars)))
    (if (or (pcoefp gcd)
	    (not (= (pt-le (p-terms gcd)) (caar l-lobjs))))
	(throw 'bad-point nil)
	(setq l-lobjs (pdiag-all l-lobjs gcd pt)))))

(defun lifting-factors-image (c f g)
  (let ((gcd (pgcdu f g)))
    (case *which-factor*
      (1 (pctimes c gcd))
      (2 (pquotient f gcd))
      (3 (pquotient g gcd)))))

(defun zgcd-lift* (c f g vars degb)
  (do ((vals (gen-point vars) (gen-point vars))
       (ans))
      ((not (null ans))
       ans)
    (setq ans
	  (catch 'bad-point
	    (zgcd-lift c f g vars vals degb)))))

;; ZGCD-LIFT returns objects called lifts.  These have the the following 
;; structure
;;      ((n <skel> <poly>) ...  )
;; n corresponds to the degree in the main variable to which this guy 
;; corresponds.

(defun zgcd-lift (c f g vars vals degb)
  (cond ((null vars)		 ;No variables left, just the main one
	 (let ((p (lifting-factors-image c f g))) ;Compute factor and 
					;enforce leading coefficient
	   (if (pcoefp p) (throw 'relprime 1) ;if the GCD is 1 quit
	       (do ((l (p-terms p) (pt-red l)) ;otherwise march
					;though the polynomial
		    (ans nil	   ;constructing a lift for each term.
			 (cons (list (pt-le l) '(nil) (list (pt-lc l)))
			       ans)))
		   ((null l)
		    (nreverse ans))))))
	((let ((prev-lift		;Recurse if possible
		(zgcd-lift (pcsubsty (car vals) (car vars) c)
			   (pcsubsty (car vals) (car vars) f)
			   (pcsubsty (car vals) (car vars) g)
			   (cdr vars) (cdr vals) (cdr degb))))
	   (do ((i 0 (1+ i))		;counts to the degree bound
		(lobjs (create-lobjs prev-lift)	;need to create
					;the appropriate matrices
		       (clear-lobjs lobjs)) ;but reuse them at each
					;step
		(pts (add-point (list (car vals))) ;List of random
		     (add-point pts))	;points
		(linsols (mapcar 'make-linsols prev-lift)
			 (merge-sol-lin
			  linsols
			  (sparse-lift
			   (pcsubsty (car pts) (car vars) c)
			   (pcsubsty (car pts) (car vars) f)
			   (pcsubsty (car pts) (car vars) g)
			   lobjs (cdr vars)))))
	       ((= i (car degb))
		(interp-polys linsols (cdr pts) (car vars))))))))

(defun make-linsols (prev-lift)
  (list (car prev-lift)
	(cadr prev-lift)
	(mapcan #'(lambda (q)
		    (cond ((pcoefp q) (list (list q)))
			  (t (do ((l (p-terms q) (pt-red l))
				  (ans nil (cons (list (pt-lc l)) ans)))
				 ((null l) ans)))))
		(caddr prev-lift))))

(defun add-point (l)
  (do ((try (cmod (random $pointbound))
	    (cmod (random $pointbound))))
      ((null (member try l :test #'equal))
       (cons try l))))

(defun merge-sol-lin (l1 l2)
  (do ((l l1 (cdr l))
       (l2 l2 (cdr l2)))
      ((null l) l1)
    (cond ((= (caar l) (caar l2))
	   (rplaca (cddar l)
		   (mapcar 'cons (cadddr (cddar l2)) (caddar l)))))))

(defun interp-polys (l pts var)
  (mapcar #'(lambda (q)
	      (cons (car q)
		    (new-skel
		     (cadr q)
		     (mapcar #'(lambda (r) (pinterp var pts r))
			     (caddr q)))))
	  l))

(defun zgcd (f g &aux $algebraic algfac*)
  (let ((f (oldcontent f))		;This is a good spot to
	(g (oldcontent g))		;initialize random
	(gcd) (mon) 
	(*which-factor*))
    ;; *WHICH-FACTOR* is interpreted as follows.  It is set fairly deep in the
    ;; algorithm, inside ZGCD1.
    ;; 1 -> Lift the GCD
    ;; 2 -> Lift the cofactor of F
    ;; 3 -> Lift the cofactor of G


    (setq mon (pgcd (car f) (car g))	;compute GCD of content
	  f (cadr f) g (cadr g))	;f and g are now primitive
    (if (or (pcoefp f) (pcoefp g)
	    (not (eq (car f) (car g))))
	(merror "ZGCD: incorrect arguments."))
    (ptimes mon
	    (do ((test))
		(nil)
	      (setq gcd (catch 'relprime (zgcd1 f g)))
	      (setq test
		    (case *which-factor*
		      (1 (testdivide f gcd))
		      (2 (testdivide f gcd))
		      (3 (testdivide g gcd))))
	      (cond ((not (null test))
		     (return
		       (cond ((equal *which-factor* 1)
			      gcd)
			     (t test))))
		    ((not (null modulus))
		     (return (let (($gcd '$red))
			       (pgcd f g)))))))))

(defun zgcd1 (f g)
  (let* ((modulus modulus)
	 first-lift
	 h degb c
	 (vars (sort (union1 (listovars f) (listovars g))
		     #'pointergp))
	 (genvar (reverse vars))

	 ;; the elements of the following degree vectors all correspond to the 
	 ;; contents of var.  Thus there may be variables missing that are in 
	 ;;GENVAR.
	 ;; (f-degv (zpdegreevector f vars))  ;;WHY NOT JUST PUT GENVAR THE REVERSE
	 ;; (g-degv (zpdegreevector g vars))  ;;THE REVERSE OF VARS AND JUST USE PDEGREEVECTOR--wfs
	 (f-degv (pdegreevector f))
	 (g-degv (pdegreevector g))
	 (gcd-degv (gcd-degree-vector f g vars)))

    ;; First we try to decide which of the gcd and the cofactors of f and g 
    ;; is smallest.  The result of this decision is indicated by *which-factor*.
    ;; Then the leading coefficient that is to be enforced is changed if a 
    ;; cofactor has been chosen.
    (case (setq *which-factor*
		(determine-lifting-factor f-degv g-degv gcd-degv))
      (1 (setq c (pgcd (p-lc f) (p-lc g))))
      (2 (setq c (p-lc f)))
      (3 (setq c (p-lc g))))

    ;; Next a degree bound must be chosen.
    (setq degb
	  (reverse
	   (mapcar #'+
		   (case *which-factor*
		     (1 gcd-degv)
		     (2 (mapcar #'- f-degv gcd-degv))
		     (3 (mapcar #'- g-degv gcd-degv)))
		   (zpdegreevector c vars))))

    (cond ((not (null modulus))
	   (lobj->poly (car vars) (cdr vars)
		       (zgcd-lift* c f g (cdr vars) (cdr degb))))
	  (t
	   (setq h (* (maxcoefficient f)
			  (maxcoefficient g)))
	   (setq modulus *alpha)	;Really should randomize
	   (setq first-lift
		 (zgcd-lift* (pmod c) (pmod f) (pmod g)
			     (cdr vars) (cdr degb)))
	   (do ((linsols (mapcar #'(lambda (q)
				     (cons (car q)
					   (new-skel (cadr q) (caddr q))))
				 first-lift)
			 (merge-sol-lin-z linsols
					  (sparse-lift cm fm gm lobjs (cdr vars))
					  (* coef-bound
						 (crecip (cmod coef-bound)))
					  (* modulus coef-bound)))
		(lobjs (create-lobjs first-lift)
		       (clear-lobjs lobjs))
		(coef-bound *alpha (* modulus coef-bound))
		(cm) (fm) (gm))
	       ((> coef-bound h)
		(setq modulus nil)
		(lobj->poly (car vars) (cdr vars) linsols))
	     (setq modulus (newprime modulus))
	     (setq cm (pmod c)
		   fm (pmod f)
		   gm (pmod g)))))))

(defun lobj->poly (var vars lobj)
  (primpart
   (cons var
	 (mapcan
	  #'(lambda (q) 
	      (list (car q)
		    (do ((x (cadr q) (cdr x))
			 (y (caddr q) (cdr y))
			 (ans 0
			      (pplus ans
				     (disrep-monom (cdar x) (car y)
						   vars))))
			((null x) ans))))
	  lobj))))

(defun disrep-monom (monom c vars)
  (cond ((null monom) c)
	((equal 0 (car monom))
	 (disrep-monom (cdr monom) c (cdr vars)))
	((list (car vars) (car monom)
	       (disrep-monom (cdr monom) c (cdr vars))))))

(defun merge-sol-lin-z (l1 l2 c new-coef-bound)
  (do ((l l1 (cdr l))
       (l2 l2 (cdr l2))
       (modulus new-coef-bound)
       (n))
      ((null l) l1)
    (cond ((= (caar l) (caar l2))
	   (rplaca (cddar l)
		   (mapcar
		    #'(lambda (a b)
			(cond ((> (abs (setq n (cplus b (ctimes c (cdifference a b)))))
				  new-coef-bound)
			       (throw 'relprime 1))
			      (n)))
		    (cadddr (cddar l2)) (caddar l)))))))

;; The following function tries to determine the degree of gcd(f, g) in each
;; variable.  This is done in the following manner:  All but one of the
;; variables in f and g are replaced by randomly chosen integers.  The
;; resulting polynomials are called f* and g*.   The degree of gcd(f*, g*) is
;; used as the degree of gcd(f, g) in that variable.
;;
;; The univariate gcd's are computed with modulus=*alpha.

(defun gcd-degree-vector (f g vars)
  (let ((modulus *alpha))
    (setq f (pmod f) g (pmod g))
    (do ((vn (cdr vars) (cdr vn))
	 (vs (delete (car vars) (copy-list vars) :test #'equal)
	     (delete (car vn) (copy-list vars) :test #'equal))
	 (l) (f*) (g*) (gcd*) (rand))
	(nil)
      (setq rand (gen-point vs))
      (setq f* (pcsub f rand vs)
	    g* (pcsub g rand vs))
      (cond ((or (pcoefp f*) (pcoefp g*)
		 (pcoefp (setq gcd* (pgcdu f* g*))))
	     (push 0 l))
	    (t (push (pt-le (p-terms gcd*)) l)))
      (cond ((null vn)
	     (return l))))))		;No reverse needed here

;; DETERMINE-LIFTING-FACTOR returns a number indicating which factor of f or g
;; to which to lift 

(defun dlf-mumblify (a b)
  (loop for x in a for y in b sum (- x y)))

(defun determine-lifting-factor (f-degv g-degv gcd-degv)
  (let* ((fv (dlf-mumblify f-degv gcd-degv))
	 (gv (dlf-mumblify g-degv gcd-degv))
	 (gcdv (apply '+ gcd-degv)))
    (if (< fv gcdv)
	(if (< fv gv) 2 3)
	(if (< gv gcdv) 3 1))))

(defun excise-extra-variables (degv vars)
  (do ((l (reverse degv) (cdr l))
       (lv (reverse genvar) (cdr lv))
       (ndegv))
      ((null l)
       ndegv)
    (cond ((eq (car lv) (car vars))
	   (push (car l) ndegv)
	   (setq vars (cdr vars))))))

(defun zpdegreevector (p vars)
  (excise-extra-variables (pdegreevector p) vars))

;; Local Modes:
;; Mode:LISP
;; Fill Column:76
;; Auto Fill Mode:1
;; Comment Column:40
;; END:
