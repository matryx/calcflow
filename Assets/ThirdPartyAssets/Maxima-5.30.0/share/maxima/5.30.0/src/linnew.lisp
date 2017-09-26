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
(macsyma-module linnew)

;; This is a matrix package which uses minors, basically.
;; TMLINSOLVE(LIST-OF-EQUAIONS,LIST-OF-VARIABLES,LIST-OF-VARIABLES-TO-BE-OBTAINED)
;; solves the linear equation. LIST-OF-VARIABLES-TO-BE-OBTAINED can be omitted,
;; in which case all variables are obtained. TMNEWDET(MATRIX,DIMENSION)
;; computes the determinant.  DIMENSION can be omitted.  The default is
;; DIMENSION=(declared dimension of MATRIX). TMINVERSE(MATRIX) computes the
;; inverse of matrix.

;; The program uses hash arrays to remember the minors if N > threshold.  If
;; $WISE is set to T, the program knocks out unnecessary elements.  But also it
;; kills necessary ones in the case of zero elements! The $WISE flag should
;; not be set to T for inverse.  The default of $WISE is NIL.

;; There seem to have been a number of bugs in this code.  I changed
;; the array referencing to value cell, and some of the stuff about
;; cre form.  It now seems tminverse  and tmlinsolve, now seem to work. --wfs.

;;these are arrays
(declare-top (special *tmarrays* *a2* *b* *aa* *row* *col* *rowinv* *colinv* *indx*))

(declare-top (special n nx ix))

(declare-top (special $linenum $dispflag $linechar $wise $fool))

(defvar *tmarrays* nil)

;; If N < threshold declared array is used, otherwise hashed array.

(defparameter *threshold* 10)

(defun tminitialflag nil
  (unless (boundp '$wise) (setq $wise nil))
  (unless (boundp '$fool) (setq $fool nil)))

;; TMDET returns the determinant of N*N matrix A2 which is in an globally
;; declared array A2.

(defun tmdet (a4 n)
  (prog (index ix)
     (tminitialflag)
     (setq ix 0 nx 0)
     (do ((i 1 (1+ i)))
	 ((> i n))
       (push i index))
     (setq index (nreverse index))
     (tminor a4 n 1 index 0)))

;; TMLIN SOLVES M SETS OF LINEAR EQUATIONS WHITH N UNKNOWN VARIABLES. IT SOLVES
;; ONLY FOR THE FIRST NX UNKNOWNS OUT OF N. THE EQUATIONS ARE EXPRESSED IN
;; MATRIX FORM WHICH IS IN N*(N+M) ARRAY A2. AS USUAL , THE LEFT HAND SIDE N*N
;; OF A2 REPRESENTS THE COEFFICIENT MATRIX, AND NEXT N*M OF A2 IS THE RIGHT
;; HAND SIDE OF THE M SETS OF EQUATIONS.  SUPPOSE N=3, M=2, AND THE UNKKNOWNS
;; ARE (X1 Y1 Z1) FOR THE FIRST SET AND (X2 Y2 Z2) FOR THE SECOND. THEN THE
;; RESULT OF TMLIN IS ((DET) (U1 U2) (V1 V2) (W1 W2)) WHERE DET IS THE
;; DETERMINANT OF THE COEFFICIENT MATRIX AND X1=U1/DET, X2=U2/DET, Y1=V1/DET,
;; Y2=V2/DET ETC.

(defun tmlin (a4 n m nx)
  (prog (index r)
     (tmdefarray n)
     (tminitialflag)
     (do ((i 1 (1+ i)))
	 ((> i n))
       (push i index))
     (setq index (reverse index))
     (setq r
	   (do ((ix 0 (1+ ix))
		(result))
	       ((> ix nx) (reverse result))
	     (push (do ((i 1 (1+ i)) (res))
		       ((> i (if (= ix 0) 1 m))
			(reverse res))
		     (unless $wise (tmkillarray ix))
		     (push (tminor a4 n 1 index i) res))
		   result)
	     (when (and (= ix 0) (equal (car result) '(0 . 1)))
	       (merror (intl:gettext "tmlin: coefficient matrix is singular.")))))
     (tmrearray n)
     (return r)))

;; TMINOR ACTUALLY COMPUTES THE MINOR DETERMINANT OF A SUBMATRIX OF A2, WHICH
;; IS CONSTRUCTED BY EXTRACTING ROWS (K,K+1,K+2,...,N) AND COLUMNS SPECIFIED BY
;; INDEX. N IS THE DIMENSION OF THE ORIGINAL MATRIX A2.  WHEN TMINOR IS USED
;; FOR LINEAR EQUATION PROGRAM, JRIGHT SPECIFIES A COLUMN OF THE CONSTANT
;; MATRIX WHICH IS PLUGED INTO AN IX-TH COLUMN OF THE COEFFICIENT MATRIX FOR
;; ABTAINING IX-TH UNKNOWN. IN OTHER WORDS, JRIGHT SPECIFIES JRIGHT-TH
;; EQUATION.

(defun tminor (a4 n k index jright)
  (prog (subindx l result name aorb)
     (setq a4 (get-array-pointer a4))
     (cond ((= k n)
	    (setq result
		  (if (= k ix)
		      (aref a4 (car index) (+ jright n))
		      (aref a4 (car index) k))))
	   (t
	    (do
	     ((j 1 (1+ j))
	      (sum '(0 . 1)))
	     ((> j (1+ (- n k))) (setq result sum))
	      (setq l (extract index j))
	      (setq subindx (cadr l))
	      (setq l (car l))
	      (setq aorb (if (= k ix)
			     (aref a4 l (+ jright n))
			     (aref a4 l k)))
	      (unless (equal aorb '(0 . 1))
		(setq name (tmaccess subindx))
		(setq sum
		      (funcall (if (oddp j) #'ratplus #'ratdifference)
			       sum
			       (rattimes
				aorb
				(if $fool
				    (tminor a4 n (1+ k) subindx jright)
				    (cond ((not (null (tmeval name)))
					   (tmeval name))
					  ((tmnomoreuse j l k)
					   (tmstore name nil)
					   (tminor a4 n (1+ k) subindx jright))
					  (t
					   (tmstore name (tminor a4 n (1+ k) subindx jright)))))
				t))))
	      (when $wise
		(when (tmnomoreuse j l k)
		  (tmkill subindx k))))))
     (return result)))

(defun extract (index j)
  (do ((ind index (cdr ind))
       (count 1 (1+ count))
       (subindx))
      ((null ind))
    (if (= count j)
	(return (list (car ind) (nconc subindx (cdr ind))))
	(setq subindx (nconc subindx (list (car ind)))))))

(declare-top (special vlist varlist genvar))

(defun tmratconv (bbb n m)
  (prog (ccc)
     (declare (special ccc))   ;Tell me this worked in Maclisp.  --gsb
					;Actually, i suspect it didn't, at least ever since
					; (sstatus punt).
     (setf (symbol-value 'ccc) bbb)
     (do ((k 1 (1+ k)))
	 ((> k n))
       (do ((j 1 (1+ j)))
	   ((> j m))
	 (newvar1 (setf (aref *a2* k j)
			 (maref ccc k j)
			 ;; (nth j (nth k *a2*))
			 ;; (MEVAL (LIST (LIST 'CCC 'array) K J))  ;;just the
			 ))))

     (newvar (cons '(mtimes) vlist))
     (do ((k 1 (1+ k)))
	 ((> k n))
       (do ((j 1 (1+ j)))
	   ((> j m))
	 (setf (aref *a2* k j) (cdr (ratrep* (aref *a2* k j))))))))

(defmfun $tmnewdet (mat &optional (dim nil dim?))
  (prog (*aa* r vlist n)
     (cond (dim?
	    (unless (integerp dim)
	      (merror (intl:gettext "tmnewdet: second argument must be an integer; found: ~M") dim))
	    (setq n dim))
	   (($matrixp mat)
	    (setq n (length (cdr mat))))
	   (t
	    (merror (intl:gettext "tmnewdet: first argument must be a matrix; found: ~M") mat)))
     (setq *aa* mat)
     (setq *a2* (make-array (list (1+ n) (1+ n)) :initial-element nil))
     (tmdefarray n)
     (tmratconv *aa* n n)
     (setq r (cons (list 'mrat 'simp varlist genvar) (tmdet '*a2* n)))
     (tmrearray n)
     (return r)))

(defun $tmlinsolve (&rest arglist)
  (prog (equations vars outvars result *aa*)
     (setq equations (cdar arglist)
	   vars (cdadr arglist)
	   outvars (cond ((null (cddr arglist)) vars)
			 (t (cdaddr arglist))))
     (setq vars (tmerge vars outvars))
     (setq nx (length outvars))
     (setq n (length vars))
     (unless (= n (length equations))
       (return (print 'too-few-or-much-equations)))
     (setq *aa*
	   (cons '($matrix simp)
		 (mapcar #'(lambda (exp)
			     (append
			      '((mlist))
			      (mapcar #'(lambda (v)
					  (prog (r)
					     (setq exp ($bothcoef exp v)
						   r (cadr exp)
						   exp (meval (caddr exp)))
					     (return r)))
				      vars)
			      (list (list '(mminus) exp))))
			 (mapcar #'(lambda (e)
				     (meval (list '(mplus)
						  ($lhs e)
						  (list '(mminus) ($rhs e)))))
				 equations))))
     (setq result (cdr ($tmlin *aa* n 1 nx)))
     (return
       (do ((vars (cons nil outvars) (cdr vars))
	    (labels)
	    (dlabel)
	    (name))
	   ((null vars)
	    (cons '(mlist) (cdr (reverse labels))))
	 (setq name (makelabel $linechar))
	 (incf $linenum)
	 (setf (symbol-value name)
	       (cond ((null (car vars))
		      (setq dlabel name)
		      (cadar result))
		     (t (list '(mequal)
			      (car vars)
			      (list '(mtimes simp)
				    (cadar result)
				    (list '(mexpt simp) dlabel -1))))))
	 (push name labels)
	 (setq result (cdr result))
	 (when $dispflag
	   (mtell-open "~M" (nconc (ncons '(mlabel))
				   (ncons name)
				   (ncons (eval name)))))))))

(defun tmerge (vars outvars)
  (append outvars
	  (prog (l)
	     (mapcar #'(lambda (v)
			 (if (member v outvars) nil (push v l)))
		     vars)
	     (return (reverse l)))))

(defmfun $tmlin (*aa* n m nx)
  (prog (r vlist)
     (setq *a2* (make-array (list (1+ n) (+ 1 m n)) :initial-element nil))
     (show *a2*)
     (tmratconv *aa* n (+ m n))
     (setq r
	   (cons '(mlist)
		 (mapcar
		  #'(lambda (res)
		      (cons '(mlist)
			    (mapcar #'(lambda (result)
					(cons (list 'mrat 'simp varlist genvar) result))
				    res)))
		  (tmlin '*a2* n m nx))))
     (show *a2*)
     (return r)))

(defun tmkill (*indx* k)
  (prog (name subindx j l)
     (when (null *indx*) (return nil))
     (setq name (tmaccess *indx*))
     (cond ((not (null (tmeval name)))
	    (tmstore name nil))
	   (t
	    (do ((ind *indx* (cdr ind))
		 (count 1 (1+ count)))
		((null ind))
	      (setq l (extract *indx* count)
		    j (car l)
		    subindx (cadr l))
	      (when (= j count)
		(tmkill subindx (1+ k))))))))

(defun tmnomoreuse (j l k)
  (if (and (= j l) (or (> k nx) (< k (1+ ix))))
      t
      nil))

(defun tmdefarray (n)
  (prog (name)
     (cond ((setq *tmarrays* (get-array-pointer *tmarrays*))
	    (tmrearray (1- (cond ((cadr (arraydims *tmarrays*)))
				 (t 1))))))
     (setq *tmarrays* (make-array (1+ n) :initial-element nil))
     (do ((i 1 (1+ i)))
	 ((> i n))
       (setq name (if (= i 1) (make-symbol "M") (gensym)))
       (cond ((< n *threshold*)
	      (setf (symbol-value name) (make-array (1+ (tmcombi n i)) :initial-element nil))
	      (setf (aref *tmarrays* i) (get-array-pointer name)))
	     (t
	      (setf (aref *tmarrays* i) (list name 'simp 'array)))))
     (gensym "G")))

;; TMREARRAY kills the TMARRAYS which holds pointers to minors. If (TMARRAYS I)
;; is an atom, it is declared array.  Otherwise it is hashed array.

(defun tmrearray (n)
  (prog nil
     (do ((i 1 (1+ i)))
	 ((> i n))
       (unless (atom (aref *tmarrays* i))
	 (tm$kill (car (aref *tmarrays* i)))))))

(defun tmaccess (index)
  (prog (l)
     (cond ($fool (return nil)))
     (setq l (length index))
     (return
       (cond ((< n *threshold*)
	      (list 'aref (aref *tmarrays* l)
		    (do ((i 1 (1+ i))
			 (x 0 (car y))
			 (y index (cdr y))
			 (sum 0))
			((> i l) (1+ sum))
		      (do ((j (1+ x) (1+ j)))
			  ((= j (car y)))
			(incf sum (tmcombi (- n j) (- l i)))))))
	     (t (cons 'aref (cons (aref *tmarrays* l) index)))))) )

(defun tmcombi (n i)
  (if (> (- n i) i)
      (/ (tmfactorial n (- n i)) (tmfactorial i 0))
      (/ (tmfactorial n i) (tmfactorial (- n i) 0))))

(defun tmfactorial (i j)
  (if (= i j)
      1
      (* i (tmfactorial (1- i) j))))

(defun tmstore (name x)
  (cond ((< n *threshold*)
	 (eval `(setf ,name ',x)))
	(t
	 (mset name (list '(mquote simp) x))
	 x)))

;; TMKILLARRAY kills all (N-IX+1)*(N-IX+1) minors which are not necessary for
;; the computation of IX-TH variable in the linear equation.  Otherwise, they
;; will do harm.

(defun tmkillarray (ix)
  (do ((i (1+ (- n ix)) (1+ i)))
      ((> i n))
    (if (< n *threshold*)
	(fillarray (aref *tmarrays* i) '(nil))
	(tm$kill (car (aref *tmarrays* i))))))

(defun tmeval (e)
  (prog (result)
     (return (cond ((< n *threshold*)
		    (eval e))
		   (t
		    (setq result (meval e))
		    (if (equal result e) nil (cadr result)))))))

(defun tm$kill (e)
  (kill1 e))

(defmfun $tminverse (*aa*)
  (prog (r vlist n m nx)
     (setq n (length (cdr *aa*)) m n nx n)
     (setq *a2* (make-array (list (1+ n) (+ 1 m n)) :initial-element nil))
     (tmratconv *aa* n n)
     (do ((i 1 (1+ i)))
	 ((> i n))
       (do ((j 1 (1+ j)))
	   ((> j m))
	 (setf (aref *a2* i (+ n j))
	       (if (= i j) '(1 . 1) '(0 . 1)))))
     (setq r (mapcar #'(lambda (res)
			 (cons '(mlist)
			       (mapcar #'(lambda (result)
					   ($ratdisrep (cons (list 'mrat 'simp varlist genvar) result)))
				       res)))
		     (tmlin '*a2* n m nx)))
     (setq r (list '(mtimes simp)
		   (list '(mexpt simp) (cadar r) -1)
		   (cons '($matrix simp) (cdr r))))
     (return r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;THIS IS A UTILITY PACKAGE FOR SPARSE
;;MATRIX INVERSION. A3 IS A N*N MATRIX.
;;IT RETURNS A LIST OF LISTS, SUCH AS
;;((I1 I2 ...) (J1 J2...) ...) WHERE (I1
;;I2 ..) SHOWS THE ROWS WHICH BELONGS TO
;;THE FIRST BLOCK, AND SO ON.  THE ROWS
;;SHOUD BE REORDERED IN THIS ORDER. THE
;;COLUMNS ARE NOT CHANGED. IT RETURNS NIL
;;IF A3 IS "OBVIOUSLY" SINGULAR.

;; (DEFUN TMISOLATE (A3 N)
;;        (PROG (NODELIST)
;; 	     (SETQ A3 (GET A3 'ARRAY))
;; 	     (setq  B (*ARRAY nil 'T (1+ N) (1+ N)))
;; 	     (setq  ROW (*ARRAY nil 'T (1+ N)))
;; 	     (setq  COL (*ARRAY nil 'T (1+ N)))
;; 	     (DO ((I 1 (1+ I)))
;; 		 ((> I N))
;; 		 (STORE (ROW I) I)
;; 		 (STORE (COL I) I))
;; 	     (DO ((I 1 (1+ I)))
;; 		 ((> I N))
;; 		 (DO ((J 1 (1+ J)))
;; 		     ((> J N))
;; 		     (STORE (B I J)
;; 			    (NOT (EQUAL (AREF A3 I J)
;; 					'(0 . 1))))))
;; 	     (COND ((NULL (TMPIVOT-ISOLATE 1))
;; 		    (SETQ NODELIST NIL)
;; 		    (GO EXIT)))
;; 	     (DO ((I 1 (1+ I)))
;; 		 ((> I N))
;; 		 (DO ((J 1 (1+ J)))
;; 		     ((> J I))
;; 		     (STORE (B (ROW J) (COL I))
;; 			    (OR (B (ROW I) (COL J))
;; 				(B (ROW J) (COL I))))
;; 		     (STORE (B (ROW I) (COL J)) (B (ROW J) (COL I))))
;; 		 (STORE (B (ROW I) (COL I)) T))
;; 	     (DO ((I 1 (1+ I)))
;; 		 ((> I N))
;; 		 (COND ((EQ (B (ROW I) (COL I)) T)
;; 			(SETQ NODELIST
;; 			      (CONS (TMPULL-OVER I N) NODELIST)))))
;; 	     EXIT
;; 	     (*TMREARRAY 'B)
;; 	     (*TMREARRAY 'ROW)
;; 	     (*TMREARRAY 'COL)
;; 	     (RETURN (REVERSE NODELIST)))))

;; (DEFUN TMPULL-OVER (P N)
;;        (PROG (Q)
;; 	     (STORE (B (ROW P) (COL P)) NIL)
;; 	     (DO ((J 1 (1+ J)))
;; 		 ((> J N) (SETQ Q NIL))
;; 		 (COND ((EQ (B (ROW P) (COL J)) T)
;; 			(RETURN (SETQ Q J)))))
;; 	     (COND ((NULL Q) (RETURN (LIST (ROW P))))
;; 		   (T (DO ((J 1 (1+ J)))
;; 			  ((> J N))
;; 			  (STORE (B (ROW Q) (COL J))
;; 				 (OR (B (ROW Q) (COL J))
;; 				     (B (ROW P) (COL J))))
;; 			  (STORE (B (ROW J) (COL Q))
;; 				 (B (ROW Q) (COL J))))
;; 		      (TMCRIP P)
;; 		      (RETURN (CONS (ROW P) (TMPULL-OVER Q N)))))))

;; (DEFUN TMCRIP (P)
;;        (DO ((I 1 (1+ I)))
;; 	   ((> I N))
;; 	   (STORE (B (ROW P) (COL I)) NIL)
;; 	   (STORE (B (ROW I) (COL P)) NIL)))

;;TMPIVOT-ISOLATE CARRIES OUT PIVOTTING
;;SO THAT THE ALL DIAGONAL ELEMENTS ARE
;;NONZERO. THIS GARANTIES WE HAVE MAXIMUM
;;NUMBER OF BLOCKS ISOLATED.

(defun tmpivot-isolate (k)
  (cond ((> k n) t)
	(t (do ((i k (1+ i)))
	       ((> i n) nil)
	     (when (aref *b* (aref *row* i) (aref *col* k))
	       (tmexchange '*row* k i)
	       (if (tmpivot-isolate (1+ k))
		   (return t)
		   (tmexchange '*row* k i)))))))

(defun tmexchange (rowcol i j)
  (prog (dummy)
     (setq rowcol (get-array-pointer rowcol))
     (setq dummy (aref rowcol i))
     (setf (aref rowcol i) (aref rowcol j))
     (setf (aref rowcol j) dummy)))


;; PROGRAM TO PREDICT ZERO ELEMENTS IN
;; THE SOLUTION OF INVERSE OR LINEAR
;; EQUATION. A IS THE COEFFICIENT MATRIX.
;; B IS THE RIGHT HAND SIDE MATRIX FOR
;; LINEAR EQUATIONS. A3 IS N*N AND B IS
;; M*M. X IS AN N*M MATRIX WHERE T -NIL
;; PATTERN SHOWING THE ZERO ELEMENTS IN
;; THE RESULT IS RETURND. T CORRESPONDS TO
;; NON-ZERO ELEMENT. IN THE CASE OF
;; INVERSE, YOU CAN PUT ANYTHING (SAY,NIL)
;; FOR B AND 0 FOR M.  NORMALLY IT RETURNS
;; T, BUT IN CASE OF SINGULAR MATRIX, IT
;; RETURNS NIL.

;; (DEFUN TMPREDICT (A3 B X N M)
;;   (PROG (FLAGINV FLAG-NONSINGULAR)
;; 	(SETQ A3 (GET A3 'ARRAY) B (GET B 'ARRAY) X (GET X 'ARRAY))
;; 	(setq  AA (*ARRAY nil 'T (1+ N) (1+ N)))
;; 	(setq  ROW (*ARRAY nil 'T (1+ N)))
;; 	(SETQ FLAGINV (= M 0))
;; 	(COND (FLAGINV (SETQ M N)))
;; 	(DO ((I 1 (1+ I)))
;; 	    ((> I N))
;; 	    (DO ((J 1 (1+ J)))
;; 		((> J N))
;; 		(STORE (AA I J)
;; 		       (NOT (EQUAL (AREF A3 I J) '(0 . 1))))))
;; 	(DO ((I 1 (1+ I)))
;; 	    ((> I N))
;; 	    (DO ((J 1 (1+ J)))
;; 		((> J M))
;; 		(STORE (AREF X I J)
;; 		       (COND (FLAGINV (EQ I J))
;; 			     (T (EQUAL (AREF B I J)
;; 				       '(0 . 1)))))))
;; 	(DO ((I 1 (1+ I))) ((> I N)) (STORE (ROW I) I))
;; 		;FORWARD ELIMINATION.
;; 	(DO ((I 1 (1+ I)))
;; 	    ((> I N))
;; 	    (SETQ FLAG-NONSINGULAR
;; 		  (DO ((II I (1+ II)))
;; 		      ((> II N) NIL)
;; 		      (COND ((AA (ROW II) I)
;; 			     (TMEXCHANGE 'ROW II I)
;; 			     (RETURN T)))))
;; 	    (COND ((NULL FLAG-NONSINGULAR) (RETURN NIL)))
;; 	    (DO ((II (1+ I) (1+ II)))
;; 		((> II N))
;; 		(COND ((AA (ROW II) I)
;; 		       (DO ((JJ (1+ I) (1+ JJ)))
;; 			   ((> JJ N))
;; 			   (STORE (AA (ROW II) JJ)
;; 				  (OR (AA (ROW I) JJ)
;; 				      (AA (ROW II) JJ))))
;; 		       (DO ((JJ 1 (1+ JJ)))
;; 			   ((> JJ M))
;; 			   (STORE (AREF X (ROW II) JJ)
;; 				  (OR (AREF X (ROW I) JJ)
;; 				      (AREF X (ROW II) JJ))))))))
;; 	(COND ((NULL FLAG-NONSINGULAR) (GO EXIT)))       ;GET OUT  BACKWARD SUBSTITUTION
;; 	(DO ((I (1- N) (1- I)))
;; 	    ((< I 1))
;; 	    (DO ((L 1 (1+ L)))
;; 		((> L M))
;; 		(STORE (AREF X (ROW I) L)
;; 		       (OR (AREF X (ROW I) L)
;; 			   (DO ((J (1+ I) (1+ J)) (SUM))
;; 			       ((> J N) SUM)
;; 			       (SETQ SUM
;; 				     (OR SUM
;; 					 (AND (AA (ROW I) J)
;; 					      (AREF
;; 							 X
;; 							 (ROW J)
;; 							 L)))))))))
;; 	       ;RECOVER THE ORDER.
;; 	(TMPERMUTE 'X N M 0 0 'ROW N 'ROW)
;;    EXIT (*TMREARRAY 'ROW) (*TMREARRAY 'AA) (RETURN FLAG-NONSINGULAR)))

;;TMPERMUTE PERMUTES THE ROWS OR COLUMNS
;;OF THE N*M MATRIX AX ACCORDING TO THE
;;SPECIFICATION OF INDEXLIST. THE FLAG
;;MUST BE SET 'ROW IF ROW PERMUTATION IS
;;DESIRED , OR 'COL OTHERWISE. THE RESULT
;;IS IN AX. NM IS THE DIMENSION OF
;;INDEXLIST.

(defun tmpermute (ax n m rbias cbias indexlist nm flag)
  (prog (k l)
     ;;	     (SETQ AX (GET AX 'array)
     ;;		   INDEXLIST (GET INDEXLIST 'array))
     (setq ax (get-array-pointer ax))
     (setq indexlist (get-array-pointer indexlist))
     (setf (symbol-array *indx*) (make-array (1+ nm) :initial-element nil))
     (do ((i 1 (1+ i)))
	 ((> i nm))
       (setf (aref *indx* i) (aref indexlist i)))
     (do ((i 1 (1+ i)))
	 ((> i nm))
       (cond ((not (= (aref *indx* i) i))
	      (prog nil
		 (tmmove ax n m rbias cbias i 0 flag)
		 (setq l i)
		 loop (setq k (aref *indx* l))
		 (setf (aref *indx* l) l)
		 (cond ((= k i)
			(tmmove ax n m rbias cbias 0 l flag))
		       (t (tmmove ax n m rbias cbias k l flag)
			  (setq l k)
			  (go loop)))))))))

(defun tmmove (ax n m rbias cbias i j flag)
  (prog (ll)
     (setq ax (get-array-pointer ax))
     (setq ll (if (eq flag '*row*)
		  (- m cbias)
		  (- n rbias)))
     (do ((k 1 (1+ k)))
	 ((> k ll))
       (cond ((eq flag '*row*)
	      (setf (aref ax (+ rbias j) (+ cbias k))
		    (aref ax (+ rbias i) (+ cbias k))))
	     (t (setf (aref ax (+ rbias k) (+ cbias j))
		      (aref ax	(+ rbias k) (+ cbias i))))))))

;;TMSYMETRICP CHECKS THE SYMETRY OF THE MATRIX.

(defun tmsymetricp (a3 n)
  (setq a3 (get-array-pointer a3))
  (do ((i 1 (1+ i)))
      ((> i n) t)
    (cond ((null (do ((j (1+ i) (1+ j)))
		     ((> j n) t)
		   (unless (equal (aref a3 i j) (aref a3 j i))
		     (return nil))))
	   (return nil)))))

;;TMLATTICE CHECKS THE "LATTICE"
;;STRUCTURE OF THE MATRIX A. IT RETURNS
;;NIL IF THE MATRIX IS "OBVIOUSLY"
;;SINGULAR. OTHERWISE IT RETURNS A LIST
;;(L1 L2 ... LM) WHERE M IS THE NUMBER OF
;;BLOCKS (STRONGLY CONNECTED SUBGRAPHS),
;;AND L1 L2 ... ARE LIST OF ROW AND
;;COLUMN NUBERS WHICH BELONG TO EACH
;;BLOCKS. THE LIST LOOKS LIKE ((R1 C1)
;;(R2 C2) ...) WHERE R R'S ARE ROWS AND
;;C'S ARE COLUMMS.

(defun tmlattice (a3 xrow xcol n)
  (setq a3 (get-array-pointer a3))
  (setq xrow (get-array-pointer xrow))
  (setq xcol (get-array-pointer xcol))
  (setq *b* (make-array (list (1+ n) (1+ n)) :initial-element nil))
  (setq *row* (make-array (1+ n) :initial-element nil))
  (setq *col* (make-array (1+ n) :initial-element nil))
  (do ((i 1 (1+ i)))
      ((> i n))
    (do ((j 1 (1+ j)))
	((> j n))
      (setf (aref *b* i j)
	    (not (equal (aref a3 i j) '(0 . 1))))))
  (do ((i 0 (1+ i)))
      ((> i n))
    (setf (aref *row* i) i)
    (setf (aref *col* i) i))
  (when (null (tmpivot-isolate 1))
    (return-from tmlattice nil))
  (do ((i 1 (1+ i)))
      ((> i n))
    (setf (aref *b* (aref *row* i) (aref *col* i)) i)
    (setf (aref *b* (aref *row* i) (aref *col* 0)) t))
  (tmlattice1 1)
  (tmsort-lattice xrow xcol))

(defun tmlattice1 (k)
  (cond ((= k n)
	 nil)
	(t
	 (tmlattice1 (1+ k))
	 (do ((looppath))
	     (nil)
	   (if (setq looppath (tmpathp k k))
	       (tmunify-loop k (cdr looppath))
	       (return nil))))))

(defun tmpathp (j k)
  (cond ((equal (aref *b* (aref *row* j) (aref *col* k)) t)
	 (list j k))
	(t
	 (do ((jj k (1+ jj)) (path))
	     ((> jj n))
	   (when (and (equal (aref *b* (aref *row* j) (aref *col* jj)) t)
		      (setq path (tmpathp jj k)))
	     (return (cons j path)))))))

(defun tmunify-loop (k chain)
  (prog (l dummyk dummyl)
     (setq l (car chain))
     (cond ((= l k) (return nil)))
     (setq dummyk (aref *b* (aref *row* k) (aref *col* k)))
     (setq dummyl (aref *b* (aref *row* l) (aref *col* l)))
     (setf (aref *b* (aref *row* k) (aref *col* k)) nil)
     (setf (aref *b* (aref *row* l) (aref *col* l)) nil)
     (do ((i 1 (1+ i)))
	 ((> i n))
       (setf (aref *b* (aref *row* k) (aref *col* i))
	     (or (aref *b* (aref *row* k) (aref *col* i)) (aref *b* (aref *row* l) (aref *col* i))))
       (setf (aref *b* (aref *row* i) (aref *col* k))
	     (or (aref *b* (aref *row* i) (aref *col* k)) (aref *b* (aref *row* i) (aref *col* l))))
       (setf (aref *b* (aref *row* l) (aref *col* i)) nil)
       (setf (aref *b* (aref *row* i) (aref *col* l)) nil))
     (setf (aref *b* (aref *row* k) (aref *col* k)) dummyl)
     (setf (aref *b* (aref *row* l) (aref *col* l)) dummyk)
     (setf (aref *b* (aref *row* k) (aref *col* 0)) t)
     (setf (aref *b* (aref *row* l) (aref *col* 0)) nil)
     (tmunify-loop k (cdr chain))))

(defun tmsort-lattice (xrow xcol)
  (prog (nodelist result)
     (setq nodelist (tmsort1))
     (setq result
	   (do ((x nodelist (cdr x)) (result))
	       ((null x) result)
	     (setq result
		   (cons (do ((next (aref *b* (aref *row* (car x)) (aref *col* (car x)))
				    (aref *b* (aref *row* next) (aref *col* next)))
			      (res))
			     ((= next (car x))
			      (cons (list (aref *row* next) (aref *col* next)) res))
			   (push (list (aref *row* next) (aref *col* next)) res))
			 result))))
     (do ((list1 result (cdr list1))
	  (i 1))
	 ((null list1))
       (do ((list2 (car list1) (cdr list2)))
	   ((null list2))
	 (setf (aref xrow i) (caar list2))
	 (setf (aref xcol i) (cadar list2))
	 (incf i)))
     (return result)))

;; (DEFUN TMLESS (I J) (B (ROW I) (COL J)))

(defun tmsort1 nil
  (do ((i 1 (1+ i))
       (result))
      ((> i n) result)
    (cond ((and (aref *b* (aref *row* i) (aref *col* 0)) (tmmaxp i))
	   (do ((j 1 (1+ j)))
	       ((> j n))
	     (unless (= j i)
	       (setf (aref *b* (aref *row* i) (aref *col* j)) nil)))
	   (setf (aref *b* (aref *row* i) (aref *col* 0)) nil)
	   (push i result)
	   (setq i 0)))))

(defun tmmaxp (i)
  (do ((j 1 (1+ j)))
      ((> j n) t)
    (when (and (not (= i j)) (aref *b* (aref *row* j) (aref *col* i)))
      (return nil))))

;;UNPIVOT IS USED IN PAUL WANG'S PROGRAM
;;TO RECOVER THE PIVOTTING. TO GET THE
;;INVERSE OF A, PAUL'S PROGRAM COMPUTES
;;THE INVERSE OF U*A*V BECAUSE OF
;;BLOCKING. LET THE INVERSE Y. THEN
;;A^^-1=V*Y*U. WHERE U AND V ARE
;;FUNDAMENTAL TRANSFORMATION
;;(PERMUTATION). UNPIVOT DOES THIS,
;;NAMELY, GIVEN A MATRIX A3, INDEX ROW
;;AND COL ,WHICH CORRESPONDS TO THE Y , U
;; AND V, RESPECTIVELY, IT COMPUTES V*Y*U
;;AND RETURNS IT TO THE SAME ARGUMENT A.

(defun tmunpivot (a3 *row* *col* n m)
  (prog nil
     (setq *col* (get-array-pointer *col*))
     (setq *row* (get-array-pointer *row*))
     (setq *rowinv* (make-array (1+ n) :initial-element nil))
     (setq *colinv* (make-array (1+ n) :initial-element nil))
     (do ((i 1 (1+ i)))
	 ((> i n))
       (setf (aref *rowinv* (aref *row* i)) i))
     (do ((i 1 (1+ i)))
	 ((> i n))
       (setf (aref *colinv* (aref *col* i)) i))
     (tmpermute a3 n m 0 n '*colinv* n '*row*)
     (tmpermute a3 n m 0 n '*rowinv* n '*col*)))

(declare-top (unspecial n vlist nx ix))
