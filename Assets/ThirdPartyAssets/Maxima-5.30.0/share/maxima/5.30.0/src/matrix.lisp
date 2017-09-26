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

(macsyma-module matrix)

(declare-top (special errrjfflag *ech* *tri* *inv*
		      mdl $detout vlist mul* top* *det* genvar $ratfac
		      varlist header $scalarmatrixp $sparse
		      $algebraic *rank* *mat*))

(defmvar $detout nil)
(defmvar top* nil)
(defmvar $ratmx nil)
(defmvar $matrix_element_mult "*")  ;;; Else, most useful when "."
(defmvar $matrix_element_add "+")
(defmvar $matrix_element_transpose nil)

(defvar *mat*)

;;I believe that all the code now stores arrays in the value cell 
(defun get-array-pointer (symbol)
  "There may be nesting of functions and we may well need to apply
   this twice in a row"
  (if (arrayp symbol) symbol (symbol-value symbol)))

(defun mxc (x)
  (mapcar #'(lambda (y) (cons '(mlist) y)) x)) ; Matrix to MACSYMA conversion
	
(defun mcx (x)
  (mapcar #'cdr x))			; MACSYMA to Matrix conversion

;; Transpose a list of lists ll. Example: ((1 2) (3 4)) --> ((1 3) (2 4)).

(defun transpose (ll)
  (let ((acc nil))
    (while (car ll)
      (push (mapcar #'car ll) acc)
      (setq ll (mapcar #'cdr ll)))
    (nreverse acc)))

(defun nthcol (x nn)
  (if (or (null x) (> nn (length (car x))))
      nil
      (nthcol1 x nn)))

(defun nthcol1 (x nn)
  (if (or (null x) (= nn 0))
      nil
      (cons (ith (car x) nn) (nthcol1 (cdr x) nn))))

;; MAYBE THIS FUNCTION SHOULD HAVE AN ARGUMENT TO INDICATE WHO CALLED IT (TO SMASH INTO ERROR MESSAGES)
(defun check (x)
  (cond ((atom x) (merror (intl:gettext "not a matrix: ~M") x))
	((eq (caar x) '$matrix) x)
	((eq (caar x) 'mlist) (list '($matrix) x))
	(t (merror (intl:gettext "not a matrix: ~M") x)))) 

(defun check1 (x)
  (cond ((atom x) nil)
	((eq (caar x) '$matrix) x)
	((eq (caar x) 'mlist) (list '($matrix) x)))) 

(defmfun $matrixp (x)
  (and (not (atom x)) (eq (caar x) '$matrix)))

(defmfun $charpoly (mat var) 
  (setq mat (check mat))
  (unless (= (length mat) (length (cadr mat)))
    (merror (intl:gettext "charpoly: matrix must be square; found ~M rows, ~M columns.") (length mat) (length (cadr mat))))
  (cond ((not $ratmx)
	 (det1 (addmatrix1
		(setq mat (mcx (cdr mat))) 
		(diagmatrix (length mat) (list '(mtimes) -1 var) '$charpoly))))
	(t (newvar var) (newvarmat1 mat)
	   (setq mat (mcx (cdr mat)))
	   (determinant1 (addmatrix mat (diagmatrix (length mat) 
						    (list '(mtimes) -1 var)
						    '$charpoly))))))

(defun disreplist1 (a)
  (setq header (list 'mrat 'simp varlist genvar))
  (mapcar #'disreplist a))

(defun disreplist (a)
  (mapcar #'(lambda (e) (cons header e)) a))
 
(defun replist1 (a)
  (mapcar #'replist a)) 

(defun replist (a)
  (mapcar #'(lambda (e) (cdr (ratrep* e))) a))

(defun timex (mat1 mat2)
  (cond ((equal mat1 1) mat2)
	((and ($matrixp mat1) ($matrixp mat2) (null (cdr mat1)))
	 (ncons '($matrix simp)))
	(t (newvarmat mat1 mat2)
	   (let (($scalarmatrixp
		  (if (and ($listp mat1) ($listp mat2)) t $scalarmatrixp)))
	     (simplifya (timex0 mat1 mat2) nil)))))

(defun lnewvar (a)
  (let ((vlist nil))
    (lnewvar1 a)
    (setq varlist (nconc (sortgreat vlist) varlist))))

(defun lnewvar1 (a)
  (cond ((atom a) (newvar1 a))
	((member (caar a) '(mlist mequal $matrix) :test #'eq) (mapc #'lnewvar1 (cdr a)))
	(t (newvar1 a))))

(defun newvarmat (mat1 mat2)
  (when $ratmx
    (let ((vlist nil))
      (lnewvar1 mat1) (lnewvar1 mat2)
      (setq varlist (nconc (sortgreat vlist) varlist)))))

(defun newvarmat1 (a)
  (cond ($ratmx (lnewvar a))))

(defun addmatrix (x y)
  (setq x (replist1 x) y (replist1 y))
  (disreplist1 (addmatrix1 x y)))
 
(defun addmatrix1 (b c)
  (unless (and (= (length b) (length c))
	       (= (length (car b)) (length (car c))))
    (merror (intl:gettext "ADDMATRIX1: attempt to add nonconformable matrices.")))
  (mapcar #'addrows b c))
 
(defun addrows (a b)
  (if (not $ratmx)
      (mapcar #'(lambda (i j) (simplus (list '(mplus) i j) 1 nil)) a b)
      (mapcar #'ratplus a b))) 

(defmfun $determinant (mat)
  (cond ((not (or (mbagp mat) ($matrixp mat))) 
         (if ($scalarp mat) mat (list '(%determinant) mat)))
	(t (setq mat (check mat))
	   (unless  (= (length mat) (length (cadr mat)))
             (merror 
               (intl:gettext
                 "determinant: matrix must be square; found ~M rows, ~M columns.")
               (length (cdr mat))
               (length (cdadr mat))))
           (cond ((not $ratmx) (det1 (mcx (cdr mat))))
	         (t (newvarmat1 mat) (determinant1 (mcx (cdr mat))))))))

(defun det (m)
  (if (= (length m) 1)
      (caar m)
      (let (*det* mul*)
	(mtoa '*mat* (setq *det* (length m)) *det* m)
	(setq *det* (tfgeli0 '*mat* *det* *det*))
	(ratreduce *det* mul*)))) 
 
(defun determinant1 (x)
  (catch 'dz (rdis (det (replist1 x))))) 

(defun treedet (mat)
  (prog (row mdl lindex tuplel n id md lt)
     (setq mat (reverse mat))
     (setq n (length mat) md (car mat))
     (setq mat (cdr mat)) (setq lindex (nreverse (index* n)) tuplel (mapcar #'list lindex))
     loop1 (when (null mat) (return (car md)))
     (setq mdl nil)
     (mapcar #'(lambda(a b) (setq mdl (nconc mdl (list a b)))) tuplel md)
     (setq md nil)
     (setq row (car mat)
	   mat (cdr mat))
     (setq lt (setq tuplel (nextlevel tuplel lindex)))
     loop2 (when (null lt)
	     (setq md (nreverse md))
	     (go loop1))
     (setq id (car lt) lt (cdr lt))
     (setq md (cons (compumd id row) md))
     (go loop2)))

(defun assoo (e l)
  (prog ()
   loop (cond ((null l) (return nil))
	      ((equal e (car l)) (return (cadr l))))
   (setq l (cddr l))
   (go loop)))

(defun compumd (id row)
  (prog (e minor i d sign ans)
     (setq ans 0 sign -1 i id)
     loop (when (null i) (return ans)) 
     (setq d (car i) i (cdr i) sign (* -1 sign))
     (cond ((equal (setq e (ith row d)) 0)
	    (go loop))
	   ((equal (setq minor (assoo (delete d (copy-tree id) :test #'equal) mdl)) 0)
	    (go loop)))
     (setq ans
	   (if (and (equal $matrix_element_mult "*")
		    (equal $matrix_element_add "+"))
	       (add ans (mul sign e minor)) ;fast common case
	     (mapply $matrix_element_add
		     (list ans
			   (mapply $matrix_element_mult
				   (list sign e minor)
				   $matrix_element_mult))
		     $matrix_element_add)))
     (go loop)))

(defun apdl (l1 l2)
  (mapcar #'(lambda (j) (append l1 (list j))) l2))

(defun nextlevel (tuplel lindex)
  (prog (ans l li)
   loop (when (null tuplel) (return ans))
   (setq l (car tuplel)
	 tuplel (cdr tuplel)
	 li (cdr (nthcdr (1- (car (last l))) lindex)))
   (when (null li) (go loop))
   (setq ans (nconc ans (apdl l li)))
   (go loop)))

(defun det1 (x)
  (cond ($sparse (mtoa '*mat* (length x) (length x) 
		       (mapcar #'(lambda (x) (mapcar #'(lambda (y) (ncons y)) x))x))
		 (sprdet '*mat* (length x)))
	(t (treedet x))))

(defmfun $ident (n)
  (cons '($matrix) (mxc (diagmatrix n 1 '$ident))))
 
(defmfun $diagmatrix (n var)
  (cons '($matrix) (mxc (diagmatrix n var '$diagmatrix))))

(defun diagmatrix (n var fn)
  (prog (i ans)
     (if (or (not (eq (ml-typep n) 'fixnum)) (minusp n))
	 (improper-arg-err n fn))
     (setq i n)
     loop (if (zerop i) (return ans))
     (setq ans (cons (onen i n var 0) ans) i (1- i))
     (go loop)))

;; ATOMAT GENERATES A MATRIX FROM A MXN ARRAY BY TAKING COLUMNS S TO N

(defun atomat (name m n s)
  (setq name (get-array-pointer name))
  (prog (j d row mat)
     (incf m)
     (incf n)
     loop1 (when (= m 1) (return mat))
     (decf m)
     (setq j n)
     loop2 (when (= j s)
	     (push row mat)
	     (setq row nil)
	     (go loop1))
     (decf j)
     (setq d (if top*
		 (meval (list (list name 'array) m j))
		 (aref name m j)))
     (push (or d '(0 . 1)) row)
     (go loop2)))

(defun diaginv (ax m)
  (setq ax (get-array-pointer ax))
  (cond ($detout (setq *det* 1)
		 (do ((i 1 (1+ i)))
		     ((> i m))
		   (setq *det* (plcm *det* (car (aref ax i i)))))
		 (setq *det* (cons *det* 1))))
  (do ((i 1 (1+ i))
       (elm))
      ((> i m))
    (setq elm (aref ax i i))
    (setf (aref ax i (+ m i))
	   (cond ($detout (cons (ptimes (cdr elm)
					(pquotient (car *det*) (car elm))) 1))
		 (t (ratinvert elm))))))

(defun diagp (ax m)
  (declare (fixnum m))
  (prog ((i 0) (j 0))
     (declare (fixnum i j))
     (setq ax (get-array-pointer ax))
     loop1 (setq i (1+ i) j 0)
     (cond ((> i m) (return t)))
     loop2 (incf j)
     (cond ((> j m) (go loop1))
	   ((and (not (= i j)) (equal (aref ax i j) '(0 . 1))) nil)
	   ((and (= i j) (not (equal (aref ax i j) '(0 . 1)))) nil)
	   (t (return nil)))
     (go loop2)))

(defun tfgeli0 (x m n)
  (cond ((or $sparse *det*) (tfgeli x m n))
	(t (tfgeli x m n) (diaglize1 x m n))))

;;  TWO-STEP FRACTION-FREE GAUSSIAN ELIMINATION ROUTINE

(defun ritediv (x m n a)
  (declare (fixnum m n))
  (setq x (get-array-pointer x))
  (prog ((j 0) (i 0) d errrjfflag)
     (declare (fixnum i j))
     (setq errrjfflag t)
     (setq i m)
     loop1 (when (zerop i) (return nil))
     (setf (aref x i i) nil)
     (setq j m)
     loop (cond ((= j n) (decf i) (go loop1)))
     (incf j)
     (cond ((equal a 1)
	    (setf (aref x i j) (cons (aref x i j) 1))
	    (go loop)))
     (setq d (catch 'raterr (pquotient (aref x i j) a)))
     (setq d (cond (d (cons d 1))
		   (t (ratreduce (aref x i j) a))))
     (setf (aref x i j) d)
     (go loop)))

(defun diaglize1 (x m n)
  (setq x (get-array-pointer x))
  (prog nil
     (cond (*det* (return (ptimes *det* (aref x m m)))))
     (setq *det* (cons (aref x m m) 1))
     (cond ((not $detout) (return (ritediv x m n (aref x m m))))
	   (t (return (ritediv x m n 1))))))

;; Takes an M by N matrix and creates an array containing the elements
;; of the matrix.  The array is associated "functionally" with the
;; symbol NAME.
;; For CL we have put it in the value cell-WFS.  Things still work.

(defun mtoa (name m n mat)
  (declare (fixnum m n))
  (proclaim (list 'special name))
  (setf (symbol-value name) (make-array (list (1+ m) (1+ n))))
  (setq name (get-array-pointer name))
  (do ((i 1 (1+ i))
       (mat mat (cdr mat)))
      ((> i m) nil)
    (declare (fixnum i))
    (do ((j 1 (1+ j))
	 (row (car mat) (cdr row)))
	((> j n))
      (declare (fixnum j))
      (setf (aref name i j) (car row)))))


(defmfun $echelon (x)
  (let (($ratmx t))
    (newvarmat1 (setq x (check x))))
  (let ((*ech* t) ($algebraic $algebraic))
    (and (not $algebraic) (some #'algp varlist) (setq $algebraic t))
    (setq x (cons '($matrix) (mxc (disreplist1 (echelon1 (replist1 (mcx (cdr x)))))))))
  (if $ratmx x ($totaldisrep x)))

(defun echelon1 (x)
  (let ((m (length x))
	(n (length (car x))))
    (mtoa '*mat* m n x)
    (setq x (catch 'rank (tfgeli '*mat* m n)))
    (cond ((and *rank* x)
	   (throw 'rnk x))
	  (t (echelon2 '*mat* m n)))))

(defun echelon2 (name m n)
  (declare (fixnum m n))
  (setq name (symbol-value name))
  (prog ((j 0) row mat a)
     (declare (fixnum j))
     (incf m)
     loop1 (when (= m 1) (return mat))
     (setq m (1- m) j 0 a nil)
     loop2 (when (= j n)
	     (setq mat (cons row mat) row nil)
	     (go loop1))
     (incf j)
     (setq row (nconc row (ncons (cond ((or (> m j) (equal (aref name m j) 0))
					'(0 . 1))
				       (a (ratreduce (aref name m j)a))
				       (t (setq a (aref name m j)) '(1 . 1))))))
     (go loop2)))

(defun triang (x)
  (let ((m (length x))
	(n (length (car x)))
	(*tri* t))
    (mtoa '*mat* m n x) 
    (tfgeli '*mat* m n)
    (triang2 '*mat* m n)))

(defun triang2 (nam m n)
  (declare (fixnum m n))
  (setq nam (get-array-pointer nam))
  (prog ((j 0) row mat)
     (declare (fixnum j))
     (setf (aref nam 0 0) 1)
     (incf m)
     loop1 (when (= m 1) (return mat))
     (decf m)
     (setq j 0)
     loop2 (when (= j n)
	     (setq mat (cons row mat) row nil)
	     (go loop1))
     (incf j)
     (setq row (nconc row (ncons (if (> m j) '(0 . 1) (cons (aref nam m j) 1)))))
     (go loop2)))

(defmfun onen (n i var fill)
  (prog (g)
   loop (cond ((= i n) (setq g (cons var g)))
	      ((zerop i) (return g)) 
	      (t (setq g (cons fill g))))
   (decf i)
   (go loop)))

(defun timex0 (x y)
  (let ((u (check1 x))
	(v (check1 y)))
    (cond ((and (null u) (null v)) (list '(mtimes) x y))
	  ((null u) (timex1 x (cons '($matrix) (mcx (cdr v)))))
	  ((null v) (timex1 y (cons '($matrix) (mcx (cdr u)))))
	  (t (cons '($matrix mult) (mxc (multiplymatrices (mcx (cdr u)) (mcx (cdr v)))))))))

(defun timex1 (x y)
  (setq y (check y))
  (cond ((not $ratmx) (setq y (cdr y)))
	(t (setq x (cdr (ratf x)) y (replist1 (cdr y)))))
  (ctimesx x y))

(defun ctimesx (x y)
  (prog (c)
   loop (cond ((null y) 
	       (return (cons '($matrix mult)
			     (mxc (cond ((not $ratmx) c) (t (disreplist1 c)))))))) 
   (setq c (nconc c (list (timesrow x (car y)))) y (cdr y))
   (go loop)))
 
(defun multiplymatrices (x y) 
  (cond ((and (null (cdr y)) (null (cdr x)))
	 (and (cdar x) (setq y (transpose y))))
	((and (null (cdar x)) (null (cdar y)))
	 (and (cdr y) (setq x (transpose x)))))
  (cond ((not (= (length (car x)) (length y)))
	 (cond ((and (null (cdr y)) (= (length (car x)) (length (car y))))
		(setq y (transpose y)))
	       (t (merror (intl:gettext "MULTIPLYMATRICES: attempt to multiply nonconformable matrices."))))))
  (cond ((not $ratmx) (multmat x y))
	(t (setq x (replist1 x) y (replist1 y)) 
	   (disreplist1 (multmat x y))))) 

(defun multmat (x y)
  (prog (mat row yt rowx)
     (setq yt (transpose y))
     loop1 (when (null x) (return mat))
     (setq rowx (car x) y yt)
     loop2 (when (null y)
	     (setq mat (nconc mat (ncons row)) x (cdr x) row nil)
	     (go loop1))
     (setq row (nconc row (ncons (multl rowx (car y)))) y (cdr y))
     (go loop2)))

;;; This actually takes the inner product of the two vectors.
;;; I check for the most common cases for speed. "*" is a slight
;;; violation of data abstraction here. The parser should turn "*" into
;;; MTIMES, well, it may someday, which will break this code. Don't
;;; hold your breath.

(defun multl (a b)
  (cond ((equal $matrix_element_add "+")
	 (do ((ans (if (not $ratmx) 0 '(0 . 1))
		   (cond ((not $ratmx)
			  (cond ((equal $matrix_element_mult "*")
				 (add ans (mul (car a) (car b))))
				((equal $matrix_element_mult ".")
				 (add ans (ncmul (car a) (car b))))
				(t
				 (add ans
				      (meval `((,(getopr $matrix_element_mult))
					       ((mquote simp) ,(car a))
					       ((mquote simp) ,(car b))))))))
			 (t
			  (ratplus ans (rattimes (car a) (car b) t)))))
	      (a a (cdr a))
	      (b b (cdr b)))
	     ((null a) ans)))
	(t
	 (mapply (getopr $matrix_element_add)
		 (mapcar #'(lambda (u v)
			     (meval `((,(getopr $matrix_element_mult))
				      ((mquote simp) ,u)
				      ((mquote simp) ,v))))
			 a b)
		 (getopr $matrix_element_add)))))

(defmfun bbsort (l fn)
  (nreverse (sort (copy-list l) fn)))

(defmfun powerx (mat x) 
  (prog (n y) 
     (cond ((not (fixnump x))
	    (return (list '(mncexpt simp) mat x)))
	   ((= x 1) (return mat))
	   ((minusp x)
	    (setq x (- x) mat ($invert mat))
	    (cond ($detout
		   (return (let ((*inv* '$detout))
			     (mul2* (power* (cadr mat) x)
				    (fmapl1 #'(lambda (x) x)
					    (powerx (caddr mat) x)))))))))
     (newvarmat1 (setq mat (check mat)))
     (setq n 1 mat (mcx (cdr mat)) y mat) 
     loop (if (= n x)
	      (let (($scalarmatrixp (if (eq $scalarmatrixp '$all) '$all)))
		(return (simplify (cons '($matrix mult) (mxc y))))))
     (setq y (multiplymatrices y mat) n (1+ n)) 
     (go loop))) 

;; The following $ALGEBRAIC code is so that 
;; RANK(MATRIX([1-SQRT(5),2],[-2,1+SQRT(5)])); will give 1.
;; - JPG and BMT
 
(defmfun $rank (x)
  (let ((*rank* t) ($ratmx t) ($algebraic $algebraic))
    (newvarmat1 (setq x (check x)))
    (and (not $algebraic) (some #'algp varlist) (setq $algebraic t))
    (setq x (replist1 (mcx (cdr x))))
    (mtoa '*mat* (length x) (length (car x)) x)
    (tfgeli '*mat* (length x) (length (car x)))))

(defun replacerow (i y x)
  (if (= i 1)
      (cons y (cdr x))
      (cons (car x) (replacerow (1- i) y (cdr x)))))
 
(defun timesrow (y row)
  (prog (ans)
     (when (and $ratmx (atom y) y)
       (setq y (cdr (ratf y))))
     loop (when (null row) (return ans))
     (setq ans (nconc ans (list (if (not $ratmx)
				    (simptimes (list '(mtimes) y (car row)) 1 nil)
				    (rattimes y (car row) t)))))
     (setq row (cdr row))
     (go loop)))
 
(defmfun $triangularize (x) 
  (let (($ratmx t))
    (newvarmat1 (setq x (check x))))
  (setq x (cons '($matrix) (mxc (disreplist1 (triang (replist1 (mcx (cdr x)))))))) 
  (if $ratmx x ($totaldisrep x)))

(defmfun $col (mat n)
  (cons '($matrix) (mxc (transpose (list (nthcol (mcx (cdr (check mat))) n)))))) 

(defun deletecol (n x)
  (prog (m g)
     (setq m x)
     loop (when (null m) (return g))
     (setq g (nconc g (ncons (deleterow n (car m)))) m (cdr m))
     (go loop)))
 
(defun deleterow (i m) 
  (cond ((or (null m) (< i 0)) (merror (intl:gettext "DELETEROW: matrix is null, or index is negative.")))
	((= i 1) (cdr m)) 
	(t (cons (car m) (deleterow (1- i) (cdr m)))))) 
 
(defmfun $minor (mat m n)
  (cons '($matrix) (mxc (minor m n (mcx (cdr (check mat)))))))
 
(defun minor (i j m)
  (deletecol j (deleterow i m))) 

(defmfun $row (mat m)
  (cons '($matrix) (mxc (list (ith (mcx (cdr (check mat))) m)))))

(defmfun $setelmx (elm m n mat) 
  (cond
    ((not (and (integerp m) (integerp n)))
	 (merror (intl:gettext "setelmx: indices must be integers; found: ~M, ~M") m n))
    ((not ($matrixp mat))
     (merror (intl:gettext "setelmx: last argument must be a matrix; found: ~M") mat))
	((not (and (> m 0) (> n 0) (> (length mat) m) (> (length (cadr mat)) n)))
	 (merror (intl:gettext "setelmx: no such element [~M, ~M]") m n)))
  (rplaca (nthcdr n (car (nthcdr m mat))) elm) mat) 
 
;;; Here the function transpose can actually do simplification of
;;; its argument. TRANSPOSE(TRANSPOSE(FOO)) => FOO.
;;; If you think this is a hack, well, realize that the hack is
;;; actually the fact that TRANSPOSE can return a noun form.



(defmfun $transpose (mat)
  (cond ((not (mxorlistp mat))
	 (cond ((and (not (atom mat)) (member (mop mat) '($transpose %transpose) :test #'eq))
		(cadr mat))
	       (($scalarp mat) mat)
	       ((mplusp mat)
		`((mplus) .,(mapcar #'$transpose (cdr mat))))
	       ((mtimesp mat)
		`((mtimes) .,(mapcar #'$transpose (cdr mat))))
	       ((mnctimesp mat)
		`((mnctimes) .,(nreverse (mapcar #'$transpose (cdr mat)))))
	       ((mncexptp mat)
		(destructuring-let (((mat pow) (cdr mat)))
		  `((mncexpt) ,($transpose mat) ,pow)))
	       (t ($nounify '$transpose) (list '(%transpose) mat))))
	(t
	 (let ((ans (transpose (mcx (cdr (check mat))))))
	   (cond ($matrix_element_transpose
		  (setq ans (mapcar #'(lambda (u) (mapcar #'transpose-els u))
				    ans))))
	   `(($matrix) . ,(mxc ans))))))

;;; THIS IS FOR TRANSPOSING THE ELEMENTS OF A MATRIX
;;; A hack for Block matricies and tensors.

(defun transpose-els (elem)
  (cond ((eq $matrix_element_transpose '$transpose)
	 ($transpose elem))
	((eq $matrix_element_transpose '$nonscalars)
	 (if ($nonscalarp elem)
	     ($transpose elem)
	     elem))
	(t
	 (meval `((,(getopr $matrix_element_transpose)) ((mquote simp) ,elem))))))


(defmfun $submatrix (&rest x)
  (prog (r c)
     l1   (when (numberp (car x))
	    (push (car x) r)
	    (setq x (cdr x))
	    (go l1))
     (setq c (nreverse (bbsort (cdr x) #'>))
	   r (nreverse (bbsort r #'>)))
     (setq x (mcx (cdar x)))
     l2   (if (null r)
	      (go b)
	      (setq x (deleterow (car r) x)))
     (setq r (cdr r))
     (go l2)
     b    (when (null c) (return (cons '($matrix) (mxc x))))
     (setq x (deletecol (car c) x) c (cdr c))
     (go b)))


(defun $list_matrix_entries (m)
  (unless ($matrixp m)
    (merror (intl:gettext "list_matrix_entries: argument must be a matrix; found: ~M") m))
  (cons (if (null (cdr m)) '(mlist) (caadr m))
	(loop for row in (cdr m) append (cdr row))))
