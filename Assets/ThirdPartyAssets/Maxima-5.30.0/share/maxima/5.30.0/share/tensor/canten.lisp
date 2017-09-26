;;; -*- Mode:LISP; Package:MACSYMA -*-
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; Comments: Canonical simplification for itensor.lisp
;;

;	** (c) Copyright 1979 Massachusetts Institute of Technology **

(in-package :maxima)

(declare-top (special frei bouni $canterm breaklist smlist $idummyx))

(setq nodown '($ichr2 $ichr1 %ichr2 %ichr1 $kdelta %kdelta))

(defun ndown (x) (putprop x t 'nodown))

(defun ndownl (l) (mapcar 'ndown l))

(ndownl nodown)

(setq breaklist nil $canterm nil)

(defun brek (i)
  (cond  ((member i breaklist :test #'equal) t) ))

; This package blindly rearranges the indices of RPOBJs, even those for
; which such index mangling is forbidden, like our special tensors. To
; avoid this, we use a private version of RPOBJ that excludes special
; tensors like the Levi-Civita or Christoffel symbols

(defun specialrpobj (e)
  (cond ((or (atom e) (atom (car e))) nil)
	(t (or (member (caar e) christoffels)
	       (eq (caar e) '$kdelta) (eq (caar e) '%kdelta)
	       (eq (caar e) '$levi_civita) (eq (caar e) '%levi_civita)
	   )
	)
  )
)

(defun reallyrpobj (e) (cond ((specialrpobj e) nil) (t (rpobj e))))

;L IS A LIST OF FACTORS WHICH RPOBS SEPARATES INTO A LIST OF TWO LISTS.  THE
;FIRST IS A LIST OF THE RPOBECTS IN L.  THE SECOND IS A LIST OF NON-RP OBJECTS

(defun rpobs (l)
 (do ( (x l (cdr x))
       (y nil (cond ((reallyrpobj (car x)) (append (list (car x)) y) )
		    (t  y) )    )
       (z nil (cond ((reallyrpobj (car x)) z)
		    (t (append (list (car x)) z)) ) )    )

     ( (null x) (cons  y (list z)))  ))

;(defun name (rp) (cond ((rpobj rp) (caar rp) )
;                        (t (merror "not rpobject"))))
;(defun conti (rp) (cond ((rpobj rp) (cdaddr rp))
;                       (t (merror "not rpobject"))))
;
;(defun covi (rp) (cond ((rpobj rp) (cdadr rp))
;                       (t (merror "not rpobject"))))
;
;(defun deri (rp) (cond ((rpobj rp) (cdddr rp))
;                       (t (merror "not rpobject"))))
;
;(defmfun $name (rp) (cond (($tenpr rp) (caar rp) ) ;test the name of tensor
;                        (t (merror "not tenprect"))))
;(defmfun $conti (rp) (cond (($tenpr rp) (cons smlist (cdaddr rp)))
;                       (t (merror "not rpobject")))) ;test the contravariant indices
;
;(defmfun $covi (rp) (cond (($tenpr rp) (cons smlist (cdadr rp)))
;                       (t (merror "not rpobject")))) ;test the contravariant indices
;
;(defun $deri (rp) (cond (($tenpr rp) (cons smlist (cdddr rp)))
;                       (t (merror "not rpobject"))))

(defun fre (l) (intersect l frei))

(defun boun (l) (intersect l bouni))


(defun grade (rp)
  (+ (length (covi rp))
     (length (conti rp))
     (length (deri rp))))

;MON IS A MONOMIAL WHOSE "APPARENT" RANK IS ARANK

(defun arank (mon)
  (do ( (i 0 (+ (length (allind (car l))) i))
	(l (cdr mon) (cdr l) ) )
	((null l)  i)  ))

(defun eqarank (m1 m2) (= (arank m1) (arank m2)))

;RP1 AND RP2 ARE RPOBJECTS WITH THE SAME GRADE

(defun alphadi (rp1 rp2)
  (alphalessp
   (catenate (indlis rp1))
   (catenate (indlis rp2))))


(defun catenate (lis)  (implode (exploden lis)))

(defun indlis (rp)
  (append (sort (append (fre (covi rp))
			(fre (conti rp))
			(fre (deri rp))) 'alphalessp)
	  (list (caar rp))
	  (sort (append (boun (covi rp))
			(boun (conti rp))
			(boun (deri rp))) 'alphalessp)))

;HOW TO USE ARRAY NAME AS PROG VARIABLE?

(defun asort (l p)
  (cond ((< (length l) 2) l)
	(t
	 (prog (i j k az)
	    (setq i 0 j 0 k (length l) az (make-array k))
	    (fillarray az l)
	    a  (cond ((equal j (1- k))
		      (return (listarray az)))
		     ((equal i (- k 1)) (setq i 0) (go a))
		     ((apply p (list (aref az i) (aref az (1+ i))))
		      (setq i (1+ i) j (1+ j)) (go a))
		     ((apply p (list (aref az (1+ i)) (aref az i)))
		      (permute az i (1+ i))
		      (setq i (1+ i) j 0)  (go a) ))   ))))

(defun permute (arra i j)
  (prog (x) (setq x (aref arra i))
	(setf (aref arra i) (aref arra j))
	(setf (aref arra j)  x)  ))

(defun prank (rp1 rp2) (cond
    ((> (grade rp1) (grade rp2)) t)
    ((equal (grade rp1) (grade rp2)) (alphadi rp1 rp2))  ))


(defun sa (x)
  (sort (copy-list x) 'alphalessp))

(defun top (rp) (cdaddr rp))
(defun bot (rp) (append (cdadr rp) (cdddr rp)))
(defun allind (rp) (cond ((not (reallyrpobj rp)) nil)
	    (t (append (cdadr rp) (cdaddr rp) (cdddr rp)))))

;MON IS A MONOMIAL WHOSE FACTORS ARE ANYBODY
;$IRPMON AND IRPMON RETURN LIST IS FREE AND DUMMY INDICES

(defun $irpmon (mon) (prog (l cf dum free cl ci)
 (setq l    (cdr mon)
       cf   (car l)
       dum  nil
       free nil
       cl   (allind cf)
       ci   nil )
 a  (cond ((null l) (when (eq t (brek 19)) (break "19"))
	    (return (append (list smlist)
			    (la (list smlist) free)
			    (la (list smlist) dum) ) ))
	  ((null cl) (when (eq t (brek 18)) (break "18"))
	    (setq l (cdr l) cf (car l) cl (allind cf))
		     (go a) )
	  (t (setq ci (car cl))
	     (cond ((not (member ci free :test #'eq)) (when (eq t (brek 17)) (break "17"))
		     (setq free (endcons ci free)
			   cl (cdr cl))  (go a) )
		   (t  (when (eq t (brek 16)) (break "16"))
		    (setq free (comp free (list ci))
			   dum (endcons ci dum)
			    cl (cdr cl))  (go a) ) ) ))))

(defun irpmon (mon) (prog (l cf dum free unio cl ci)
 (setq l    (cdr mon)
       cf   (car l)
       dum  nil
       free nil
       unio nil
       cl   (allind cf)
       ci   nil )
 a  (cond ((null l) (when (eq t (brek 15)) (break "15"))
	     (setq free (comp unio dum)
		   dum (comp unio free))
	    (return (append (list free) (list dum)) ))

	  ((null cl) (when (eq t (brek 14)) (break "14"))
	    (setq l (cdr l) cf (car l) cl (allind cf))
		     (go a) )
	  (t (setq ci (car cl))
	     (cond ((not (member ci unio :test #'eq)) (when (eq t (brek 13)) (break "13"))
		     (setq unio (endcons ci unio)
			   cl (cdr cl))  (go a) )
		   (t  (when (eq t (brek 12)) (break "12"))
		    (setq dum (endcons ci dum)
			    cl (cdr cl))  (go a) ) ) ))))

;THE ARGUMENT E IS A PRODUCT OF FACTORS SOME OF WHICH ARE NOT RPOBJECTS. THE
;FUNCTION RPOBS SEPARATES THESE AND PUTS THEM INTO NRPFACT.  THE RPFACTORS ARE
;SORTED AND PUT IN A

(defun redcan (e)
  (prog (a b c d l nrpfact cci coi ct cil ocil)  (when (eq t (brek 6)) (break "6"))
    (setq nrpfact  (cadr (rpobs (cdr e)))
	  d (irpmon e)
	  frei (car d) bouni (cadr d)
	  a (sort (append (car (rpobs (cdr e))) nil) 'prank)
	  l (length a)
	  b (make-array l)
	  c (make-array (list l 4)))
    (fillarray b a) (when (eq t (brek 7)) (break "7"))
    (do  ((i 0 (1+ i)))
	 ((equal i l))
	 (setf (aref c i 0) (name (aref b i)))
	 (setf (aref c i 1) (conti (aref b i)))
	 (setf (aref c i 2) (covi (aref b i)))
	 (setf (aref c i 3) (deri (aref b i))))


     (setq ocil (do  ((i 0 (1+ i))
		      (m nil (append (aref c i 3) m) ) )
		     ((equal i l) m))
	   ocil (append ocil (aref c 0 2) (car d))
	   ct  0
	   cil  (aref c ct 1)
	   cci  (car cil)  )
     (setf (aref c ct 1) nil)

 a (cond
    ((equal ct (1- l))
     (when (eq t (brek 1)) (break "1"))
     (setf (aref c ct 1) cil)
     (return
      (append nrpfact
	      (do ((i 0 (1+ i))
		   (lis (apdval c 0) (append lis (apdval c (1+ i))) ) )
		  ((equal i (1- l)) lis ) ))) )

    ((zl-get (aref c ct 0) 'nodown)
     (setf (aref c ct 1) cil)
     (incf ct)
     (setq cil (aref c ct 1)
	   ocil (append (aref c ct 2) ocil))
     (setf (aref c ct 1) nil) (go a))

    ((null cil)
     (when (eq t (brek 2)) (break "2"))
     (incf ct)
     (setq cil (aref c ct 1)
	   ocil (append (aref c ct 2) ocil) )
     (setf (aref c ct 1) nil) (go a) )

    (t (setq cci (car cil))  (when (eq t (brek 5)) (break "5"))
       (cond ((not (member cci ocil :test #'eq))
	     (when (eq t (brek 3)) (break "3"))
	      (setq coi (do  ((i (1+ ct) (1+ i) ) )
			     ((member cci (aref c i 2) :test #'eq) i)))

	      (setf (aref c ct 2)
		    (cons cci (aref c ct 2)))
	      (setf (aref c coi 1)
		    (cons cci (aref c coi 1)))
	      (setf (aref c coi 2)
		    (comp (aref c coi 2) (list cci)))
	      (setq ocil (cons cci ocil)
		     cil (cdr cil)       )  (go a)   )
	 (t  (when (eq t (brek 4)) (break "4"))
	     (setf (aref c ct 1) (cons cci (aref c ct 1)) )
	     (setq cil (cdr cil)  ) (go a) ) )) )   ) )

(defun la (x y) (list (append x y)))

(defun apdval (c i) (list (append (list (cons (aref c i 0)
					  (list 'simp)))
			     (la (list smlist)
				 (sa (aref c i 2)))
			     (la (list smlist)
				 (sa (aref c i 1)))
			     (sa (aref c i 3) ))))
(defun canform (e)
   (cond ((atom e) e)
	 ((rpobj e)   e)
	 ((and (eq (caar e) 'mtimes)
	       (= 0 (length (car (rpobs (cdr e))))) )  e)
	 ((eq (caar e) 'mtimes)
	  (cons '(mtimes) (redcan e)) )
	 ((eq (caar e) 'mplus)
	    (mysubst0
	     (simplus (cons '(mplus)
	      (mapcar #'(lambda (v) (cons '(mplus) (canarank v)))
		      (strata (cdr e) 'eqarank) ))
		1. nil)              e)  )
	  (t e) ))


(defun endcons (x l) (reverse (cons x (reverse l))))

(defun comp (x y)
 (do  ((z  (cond ((atom y) (ncons y)) (y)));patch for case when Y is not a list
       (l  x  (cdr l))
       (a  nil (cond ((member (car l) z :test #'equal)  a)
		     (t  (endcons (car l) a)) )))
      ((null l) a)  )  )

(defun apdunion (x y)
(do  ((l  y  (cdr l))
      (a  x  (cond ((member (car l) a :test #'equal)  a)
		   (t (endcons (car l) a))  )))
     ((null l)  a) ))

;LIST IS A LIST OF CANFORMED MONOMIALS OF THE SAME ARANK
;CANARANK FINDS THE EQUIVALENT ONES

(defun canarank (lis) (prog (a b c d ct cnt i)
     (setq  a  lis
	    b  nil
	    c  (cdr a)
	    d  (make-array (length a))
	   ct  (canform (car a))
	  cnt  (canform (car c))
	    i  1)
     (fillarray d a)

a   (cond ((null a)
	       (return b))

	  ((and (null (cdr a)) (null c))
	       (setq b (cons ct b))
	       (return b) )


	  ((null c) (when (eq t (brek 9)) (break "9"))
	     (setq b (cons ct b))
	     (setf (aref d 0) nil)
	     (setq a (comp (listarray d) (list nil))
		   c (cdr a)
		   i 1
		  ct (canform (car a))
		 cnt (canform (car c)) )
	    (cond ((null a) (return b))
		  (t (setq d (make-array (length a)))
		     (fillarray d a)))
	    (go a))

	  ((samestruc ct cnt) (when (eq t (brek 10)) (break "10"))
	     (setq b (cons (canform (transform cnt ct)) b))
	     (setf (aref d i) nil)
	     (setq c (cdr c)
		 cnt (canform (car c))
		   i (1+ i) )  (go a) )

	   (t (when (eq t (brek 11)) (break "11"))
	     (setq c (cdr c)
		 cnt (canform (car c))
		   i (1+ i)) (go a)) )))

;M1,M2 ARE (CANFORMED) MONOMIALS WHOSE INDEX STRUCTURE MAY BE THE SAME

(defun samestruc (m1 m2) (equal (indstruc m1) (indstruc m2)))

;MON IS (MTIMES) A LIST OF RP AND NON-RP FACTORS IN A MONOMIAL.  THE NEXT
;FUNCTION RETURNS A LIST WHOSE ELEMENTS ARE 4-ELEMENT LISTS GIVING THE NAME
;(MON) AND THE LENGTHS OF THE COVARIANT,CONTRAVARIANT,DERIVATIVE INDICES.

(defun indstruc (mon)
 (do  ( (l (cdr mon) (cdr l))
	(b nil (cond ((reallyrpobj (car l))
		       (append b  (list (findstruc (car l))) ))
		      (t  b) ))  )
      ( (null l)  b)  )  )



;FACT IS AN RP  FACTOR IN MON. HERE WE FIND ITS INDEX STRUCTURE

(defun findstruc (fact)
       (append (list (caar fact) )
	       (list (length (cdadr fact)))
	       (list (length (cdaddr fact)))
	       (list (length (cdddr fact))) ))

;M1,M2 ARE MONOMIALS WITH THE SAMESTRUC TURE. THE NEXT FUNCTION TRANSFORMS
;(PERMUTES) THE FORM OF M1 INTO M2.

(defun transform  (m1 m2)
  (sublis  (findperm m1 m2) m1))

(defun strata (lis p)
 (cond ((or (null lis) (null (cdr lis))) (list lis))
       (t

  (prog  (l bl cs)   (setq l lis cs nil bl nil)

  a  (cond ((null l) (when (eq t (brek 1)) (break "1"))
		     (return (cond ((null cs) bl)
				   (t (endcons cs bl)))))

	   ((and (null (cdr l)) (null cs)) (when (eq t (brek 2)) (break "2"))
		      (setq bl (endcons (list (car l)) bl))
		      (return bl) )
	((and (null (cdr l)) (not (null cs))) (when (eq t (brek 3)) (break "3"))
	     (return (cond ((funcall p (car l) (car cs))
		     (setq cs (cons (car l) cs)
			   bl (endcons cs bl)))
		   (t (setq bl (endcons (list (car l)) (endcons cs bl)))))))

	   ((null cs) (when (eq t (brek 4)) (break "4"))
 (setq cs (list (car l)) l (cdr l)) (go a))
	   ((funcall p (car l) (car cs)) (when (eq t (brek 5)) (break "5"))
	     (setq cs (cons (car l) cs)
		   l (cdr l)) (go a)   )

	   (t   (when (eq t (brek 6)) (break "6"))
 (setq bl (endcons  cs bl)
		    cs (list (car l))
		    l  (cdr l) )  (go a) ) ) ))))



(defun tindstruc (mon)
 (do ( (l (cdr mon) (cdr l))
       (b nil (cond ((reallyrpobj (car l))
		     (append b  (tfindstruc (car l)) ))
		    (t b) )))
     ((null l) b)))

(defun tfindstruc (fact)
     (append (cdadr fact) (cdaddr fact) (cdddr fact) ))

(defun dumm (x)  (equal (cadr (explodec x)) $idummyx))


(defun findpermut (i1 i2)
  (comp (mapcar 'pij i1 i2) (list nil)))

(defun pij (x y)
  (cond ((and (dumm x) (dumm y) (not (eq x y))) (cons x y))))


;(SAMESTRUC M1 M2) IS  T  FOR THE ARGUMENTS BELOW
;THE RESULTING PERMUTATION IS GIVEN TO TRANSFORM

(defun findperm (m1 m2)
  (do  ((d1 (cadr (irpmon m1))    )
	(d2 (cadr (irpmon m2))    )
	(i1 (tindstruc m1) (cdr i1) )
	(i2 (tindstruc m2) (cdr i2) )
	(l nil (cond ((and (member (car i1) d1 :test #'eq) (member (car i2) d2 :test #'eq)
			   (not (eq (car i1) (car i2)))
			   (not (member (car i1) (car l) :test #'eq))
			   (not (member (car i2) (cadr l) :test #'eq)) )
		      (cons (endcons (car i1) (car l))
		       (list (endcons (car i2) (cadr l))) ) )
		     (t l))))

  ((null i1) (mapcar 'cons
		      (append (car l) (comp d1 (car l)))
		      (append (cadr l) (comp d2 (cadr l)))))))

(defun $canten (x)
  (cond ((not $allsym)
	 (merror "canten works only if allsym:true has been set"))
	(t
	 (do ((i ($nterms x) ($nterms l))
	      (l (canform x) (canform l)))
	     ((= i ($nterms l))  l)
	   (cond ((eq $canterm t) (print i)))))))

(defun $concan (x)
  (cond ((not $allsym)
	 (merror "concan works only if allsym:true has been set"))
	(t
	 (do ((i ($nterms x) ($nterms l))
	      (l (canform x) ($contract (canform l))))
	     ((= i ($nterms l)) l)
	   (cond ((eq $canterm t) (print i)))))))
